//Written in the D programming language
/**
 * Fast Regular expressions for D
 *
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * Authors: Dmitry Olshansky
 *
 */
module regex;

import std.stdio;
import std.array, std.algorithm, std.range, std.conv, std.exception, std.ctype, std.format, std.typecons;

enum:uint {
    IRchar              =  0, // a
    IRany               =  1, // .
    IRcharset           =  2, // [...]
    IRstartoption       =  3, // size prefix
    IRoption            =  4, // x | y
    IRendoption         =  5, // option separator
    IRstartinfinite     =  6, // size prefix
    IRstartrepeat       =  7, // ditto
    IRrepeat            =  8, // x{n,m}
    IRrepeatq           =  9, // x{n,m}?
    IRinfinite          = 10, // x*
    IRinfiniteq         = 11, // x*?
    IRdigit             = 12, 
    IRnotdigit          = 13,
    IRspace             = 14,
    IRnotspace          = 15,
    IRword              = 16,
    IRnotword           = 17,    
    IRstartgroup        = 18, // (
    IRendgroup          = 19, // )
    IRbol               = 20, // ^
    IReol               = 21, // $ 
    
    IRbackref           = 22,
    IRwordboundary      = 23,
    IRnotwordboundary   = 24,
    IRlookahead         = 25,
    IRneglookahead      = 26,
    IRlookbehind        = 27,
    IRneglookbehind     = 28,
    IRret               = 29, //end of lookaround sub
    //TODO: ...
    IRlambda            = 128
};
//IR bit twiddling helpers
uint opcode(uint ir){ return ir >>24; }
uint opdata(uint ir){ return ir & 0x00ff_ffff; }
uint opgen(uint code, uint data=0){ return (code<<24) | data; }
//multiply-add, throws exception on overflow
uint checkedMulAdd(uint f1, uint f2, uint add)
{
    ulong r = f1 * cast(ulong)f2 + add;
    if(r < (1<<32UL))
        throw new RegexException("Regex internal errror - integer overflow");
    return cast(uint)r;
}

struct RecursiveParser(R)
if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;       //keep full pattern for pretty printing error messages
    uint[][] ir;      //resulting bytecode separated by lookaround levels
    uint level = 0;      //current lookaround level
    uint[] index;        //user group number -> internal number
    struct NamedGroup
    { 
        string name; 
        uint group;
    }
    NamedGroup[] dict; //maps name -> user group number
    //current num of group, current nesting, and peak number of group
    uint nsub = 0, nesting = 0, top = 0; 
    
    this(R pattern)
    {
        pat = origin = pattern;     
        ir = new uint[][1];
        ir[0].reserve(pat.length);
        next();
        parseRegex();
    }
    @property dchar current(){ return _current; }
    void enterLevel()
    { 
        if(++level >= ir.length)
            ir.length += 1;
    }
    void leaveLevel(){  --level;   }
    bool next()
    {
        if(pat.empty)
        {
            empty =  true;
            return false;
        }
        _current = pat.front;
        pat.popFront();
        return true;
    }
    void skipSpace()
    {
        while(isspace(current) && next()){ }
    }
    void restart(R newpat)
    { 
        pat = newpat;
        empty = false;
        next();
    }
    void put(uint code){  ir[level] ~= code; }    
    uint parseDecimal()
    {
        uint r=0;
        while(isdigit(current))
        {
            if(r >= (uint.max/10)) 
                error("Overflow in decimal number");
            r = 10*r + cast(uint)(current-'0'); 
            next();
        }
        return r;
    }
    /*
        Parse and store IR for sub regex, returns effective length of IR 
    */
    uint parseRegex()
    {
        uint effectiveLength = 0;
        uint start = cast(uint)ir[level].length;
        auto subSave = nsub;
        while(!empty && current != '|' && current != ')')
            effectiveLength += parseRepetition();
        if(!empty)
            switch(current)
            {
            case ')':
                nesting || error("Unmatched ')'");
                nesting--;
                return effectiveLength;
            case '|':
                uint[2] piece = [opgen(IRstartoption), opgen(IRoption, ir[level].length - start)];
                insertInPlace(ir[level], start, piece[]); // + 2 
                put(opgen(IRendoption)); // + 1 
                effectiveLength += 3;
                uint anchor = cast(uint)(ir[level].length); //points to first option
                uint maxSub = 0; //maximum number of captures out of each code path
                do
                {//TODO: check overflows
                    writeln(current);
                    next();
                    uint offset = cast(uint)(ir[level].length);
                    put(0); //reserve space
                    while(!empty && current != '|' && current != ')')
                        effectiveLength += parseRepetition();
                    if(current == '|')      //another option?
                    {
                        put(opgen(IRendoption));   //we can turn this into jump later
                        effectiveLength++;
                    }
                    uint len = cast(uint)(ir[level].length - offset - 1);
                    assert(len < (1<<24));
                    ir[level][offset] = opgen(IRoption,  len);
                    maxSub = max(nsub,maxSub);
                    nsub = subSave; //reuse groups across alternations
                }while(current == '|'); //process all options of alternation
                nsub = maxSub;
                if(current == ')') 
                    goto case ')';
            }
        return effectiveLength;   
    }
    /*
        Parse and store IR for atom-quantifier pair, returns effective length of IR
    */
    uint parseRepetition()
    {
        uint offset = cast(uint)ir[level].length;
        uint effectiveLength = parseAtom();
        uint len = cast(uint)ir[level].length - offset;
        if(empty)
            return effectiveLength;
        uint min, max;
        switch(current)
        {
        case '*':
            min = 0;
            max = infinite;
            break;
        case '?':
            min = 0;
            max = 1;
            break;
        case '+':
            min = 1;
            max = infinite;
            break;
        case '{':
            next() || error("Unexpected end of regex pattern");
            isdigit(current) || error("First number required in repetition"); 
            min = parseDecimal();    
            skipSpace();
            if(current == '}')
                max = min;
            else if(current == ',')
            {
                next();
                if(isdigit(current))
                    max = parseDecimal();
                else if(current == '}')
                    max = infinite;
                else
                    error("Unexpected symbol in regex pattern"); 
                skipSpace();
                if(current != '}')
                    error("Unmatched '{' in regex pattern");
            }
            else
                error("Unexpected symbol in regex pattern");
                       
            break;
        default:
            return effectiveLength;
        }
        next(); 
        bool greedy = true;
        if(current == '?')
        {
            greedy = false;
            next();
        }
        if(max != infinite)
        {
            if(min != 1 || max != 1)
            {
                insertInPlace(ir[level], offset, opgen(IRstartrepeat, len)); // + 1 word
                put(opgen(greedy ? IRrepeat : IRrepeatq, len));
                put(effectiveLength); //step of RIN counter
                put(min);
                put(max);
                effectiveLength = checkedMulAdd(max,effectiveLength,5);
            }
        }
        else if(min) // && max is infinite
        {
            if(min != 1)
            {
                insertInPlace(ir[level], offset, opgen(IRstartrepeat, len));// + 1 word
                offset += 1;//so it still points to the repeated block
                put(opgen(greedy ? IRrepeat : IRrepeatq, len));//TODO: include step
                put(effectiveLength); //step of RIN counter
                put(min);
                put(min);
            }
            put(opgen(IRstartinfinite, len));
            ir[level] ~= ir[level][offset .. offset+len];// + another effectiveLength
            put(opgen(greedy ? IRinfinite : IRinfiniteq, len));
            effectiveLength = checkedMulAdd((min+1),effectiveLength,7);
        }
        else//vanila {0,inf}
        {
            insertInPlace(ir[level], offset, opgen(IRstartinfinite, len));// + 1 word
            put(opgen(greedy ? IRinfinite : IRinfiniteq, len));
            effectiveLength = checkedMulAdd(2,effectiveLength,2);
        }
        return effectiveLength;
    }
    /*
        Parse and store IR for atom, returns effective length of IR
    */
    uint parseAtom()
    {
        if(empty)
            return 0;
        switch(current)
        {
        case '*', '?', '+', '|', '{', '}':
            error("'*', '+', '?', '{', '}' not allowed in atom");
            break;
        case '.':
            put(IRany);
            next();
            break;
        case '(':
            R save = pat;
            next();
            uint op = 0, nglob = void, effectiveLength = 0;
            bool lookaround  = false;
            nesting++;
            if(current == '?')
            {
                
                next();
                switch(current)
                {
                case ':':
                    next();
                    break;
                case '=':
                    op = IRlookahead;
                    next();
                    lookaround = true;
                    break;
                case '!':
                    op = IRneglookahead;
                    next();
                    lookaround = true;
                    break;
                case 'P':
                    next();
                    if(current != '<')
                        error("Expected '<' in named group");
                    string name;
                    while(next() && isalpha(current))
                    {
                        name ~= current;
                    }
                    if(current != '>')
                        error("Expected '>' closing named group");
                    next();
                    nglob = cast(uint)index.length;
                    index ~= nsub++;
                    auto t = NamedGroup(name,nglob);
                    auto d = assumeSorted!"a.name < b.name"(dict);
                    auto ind = d.lowerBound(t).length;
                    insertInPlace(dict, ind, t);
                    op = opgen(IRstartgroup, nglob);
                    break;
                case '<':
                    next();
                    if(current == '=')
                        op = IRlookbehind;
                    else if(current == '!')
                        op = IRneglookbehind;
                    else
                        error("'!' or '=' expected after '<'");
                    next();
                    lookaround = true;
                    break;
                default:
                    error(" ':', '=', '<', 'P' or '!' expected after '(?' ");
                }
            }
            else
            {
                nglob = cast(uint)index.length;
                index ~= nsub++; //put local index
                op = opgen(IRstartgroup, nglob);
            }            
            if(lookaround)
            {
                enterLevel();
                uint offset = cast(uint)ir[level].length;
                parseRegex();//lookarounds are isolated
                put(opgen(IRret));
                leaveLevel();
                put(opgen(op, offset));//aims to the next level
                effectiveLength = 1; // only one word 
            }
            else
            {
                if(op) //currently only groups
                {
                    put(op);
                    effectiveLength++;
                }
                effectiveLength += parseRegex();
            }
            if(opcode(op) == IRstartgroup)
            {
                put(opgen(IRendgroup, nglob));
                effectiveLength++;
            }
            if(current != ')')
            {
                pat = save;
                error("Unmatched '(' in regex pattern");
            }
            next();
            return effectiveLength;
        case '[':
            //range
            assert(0);
            break;
        case '\\':
            next() || error("Unfinished escape sequence");
            put(escape());
            break;
        case '^':
            put(opgen(IRbol));
            next();
            break;
        case '$':
            put(opgen(IReol));
            next();
            break;
        default:
            put(current);
            next();
        }
        return 1;
    }
    
    uint escape()
    {
        switch(current)
        {
        case 'f':   next(); return '\f';
        case 'n':   next(); return '\n';
        case 'r':   next(); return '\r';
        case 't':   next(); return '\t';
        case 'v':   next(); return '\v';
            
        case 'd':   next(); return opgen(IRdigit); 
        case 'D':   next(); return opgen(IRnotdigit); 
        case 'b':   next(); return opgen(IRwordboundary);
        case 'B':   next(); return opgen(IRnotwordboundary);
        case 's':   next(); return opgen(IRspace);
        case 'S':   next(); return opgen(IRnotspace);
        case 'w':   next(); return opgen(IRword);
        case 'W':   next(); return opgen(IRnotword);
        case 'x':            
            auto save = pat;
            uint code = 0;
            for(int i=0;i<2;i++)
            {
                if(!next())
                {
                    restart(save);
                    return 'x';
                }
                if('0' <= current && current <= '9')
                    code = code * 16 + current - '0';
                else if('a' <= current && current <= 'f')
                    code = code * 16 + current -'a' + 10;
                else if('A' <= current && current <= 'Z')
                    code = code * 16 + current - 'A' + 10;
                else//wrong unicode escape treat \x like 'x' 
                {
                    restart(save);
                    return 'x';
                }
            }
            next();
            return opgen(IRchar,code);
        case 'u':
            auto save = pat;
            uint code = 0;
            for(int i=0; i<4; i++)
            {
                if(!next())
                {
                    restart(save);
                    return 'u';
                }
                if('0' <= current && current <= '9')
                    code = code * 16 + current - '0';
                else if('a' <= current && current <= 'f')
                    code = code * 16 + current -'a' + 10;
                else if('A' <= current && current <= 'Z')
                    code = code * 16 + current - 'A' + 10;
                else //wrong unicode escape treat \u like 'u' 
                {
                    restart(save);
                    return 'u';
                }
            }
            next();
            return opgen(IRchar, code);
        case 'c': //control codes                      
            next() || error("Unfinished escape sequence");
            ('a' <= current && current <= 'z') || ('A' <= current && current <= 'Z')
                || error("Only letters are allowed after \\c");
            uint code = opgen(IRchar, current &  0x1f);
            next();
            return code;
        case '0':
            next();
            return 0;//NUL character
        case '1': .. case '9':
            uint nref = cast(uint)current - '0'; 
            //groups counted from zero, so nref comes greater by 1 hence '<='
            nref <=  index.length || error("Backref to unseen group");
            //perl's disambiguation rule i.e.
            //get next digit only if there is such group number
            while(nref <= index.length && next() && isdigit(current))
            {
                nref = nref * 10 + current - '0';
            }
            if(nref > index.length)
                nref /= 10;
            nref--;
            return opgen(IRbackref, nref);
        }
    }
    //
    void error(string msg)
    {
        auto app = appender!string;
        formattedWrite(app,"%s\nPattern with error: `%s <--HERE-- %s`",
                       msg, origin[0..$-pat.length], pat);
        throw new RegexException(app.data);
    }
    //
    void print()
    {
        foreach(lvl, irb; ir)
        {
            writefln("%sPC\tINST", lvl ? "Lookaround level #"~to!string(lvl)~"\n" : "");
            for(size_t i=0;i<irb.length;i++)
            {
                writef("%d\t",i);
                switch(opcode(irb[i]))
                {
                case IRchar:
                    write("char ",cast(dchar)irb[i]);
                    break;
                case IRany:
                    write("any char");
                    break;
                case IRword:
                    write("word");
                    break;
                case IRnotword:
                    write("not word");
                    break;
                case IRdigit:
                    write("digit");
                    break;
                case IRnotdigit:
                    write("not digit");
                    break;
                case IRspace:
                    write("space");
                    break;
                case IRnotspace:
                    write("not space");
                    break;
                case IRwordboundary:
                    write("word-boundary");
                    break;
                case IRnotwordboundary:
                    write("not word-boundary");
                    break;
                case IRbol:
                    write("begining-of-line");
                    break;
                case IReol:
                    write("end-of-line");
                    break;
                case IRstartrepeat:
                    uint len = opdata(irb[i]);
                    writef("start repeat pc=>%u", i+len+1);
                    break;
                case IRstartinfinite:
                    uint len = opdata(irb[i]);
                    writef("start infinite pc=>%u", i+len+1);
                    break;
                case IRrepeat:
                case IRrepeatq:
                    uint len = opdata(irb[i]);
                    writef("repeat%s pc=>%u min=%u max=%u (dRIN=%u)", 
                           opcode(irb[i]) == IRrepeatq ? "q" : "",
                           i-len, irb[i+2], irb[i+3],irb[i+1]);
                    i += 3;//3 extra operands
                    break;
                case IRinfinite:
                case IRinfiniteq:
                    uint len = opdata(irb[i]);
                    writef("infinite%s pc=>%u ", 
                           opcode(irb[i]) == IRinfiniteq ? "q" : "", i-len);
                    break;
                case IRstartoption:
                    writef("start option");
                    break;
                case IRoption:
                    uint len = opdata(irb[i]);
                    writef("option pc=>%u", i+len+1);
                    break;
                case IRendoption:
                    uint len = opdata(irb[i]);
                    writef("end option pc=>%u", i+len+1);
                    break;
                case IRstartgroup: 
                case IRendgroup:
                    uint n = opdata(irb[i]);
                    // Ouch: '!vthis->csym' on line 713 in file 'glue.c'
                    //auto ng = find!((x){ return x.group == n; })(dict); 
                    string name;
                    foreach(v;dict)
                        if(v.group == n)
                        {
                            name = "'"~v.name~"'";
                            break;   
                        }
                    if(opcode(irb[i]) == IRstartgroup)
                    {
                        writef("start group %s #%u (internal %u)",
                           name, n,  index[n]);    
                    }
                    else
                    {
                        writef("end group '%s' #%u (internal %u)",
                           name, n, index[n]);
                    }
                    break;
                case IRlookahead:
                    uint dest = opdata(irb[i]);
                    writef("lookahead dest=%u",  dest);
                    break;
                case IRneglookahead: 
                    uint dest = opdata(irb[i]);
                    writef("neglookahead dest=%u",  dest);
                    break;
                case IRlookbehind:
                    uint dest = opdata(irb[i]);
                    writef("lookbehind dest=%u",  dest);
                    break;
                case IRneglookbehind:
                    uint dest = opdata(irb[i]);
                    writef("neglookbehind dest=%u",  dest);
                    break;
                case IRbackref:
                    uint n = opdata(irb[i]);
                    writef("backref %u",  n);
                    break;
                case IRret:
                    writeln("return");
                    break;
                }
                writeln();
            }
        }
    }
}

//Actual instructions for VM 
enum InstType:uint {
    Banychar,
    Bchar,
    Bnotchar,
    Brange,
    Bnotrange,
    Bstring,
    Bsplit,
    Bsave
};

struct GenericInst{
    InstType type;
    union{
        dchar ch;
        uint nsave;
        struct{
            dchar low,hi;
        }
        struct{
            uint x,y;
        }
    }
}


struct Inst(InstType type){
    GenericInst inst;
    enum length = instSize!type;
    alias inst this;
}


auto _instSize(InstType type){
    switch(type){
        case InstType.Banychar:
            return InstType.sizeof;
        case InstType.Bchar: case InstType.Bnotchar:
            return InstType.sizeof+dchar.sizeof;
        case InstType.Brange: case InstType.Bnotrange:
            return InstType.sizeof+dchar.sizeof+dchar.sizeof;
        case InstType.Bsplit:
            return InstType.sizeof+uint.sizeof+uint.sizeof;
    }
}

template instSize(InstType t){
    enum opcodeSize = _instSize(t);
}


class RegexException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}