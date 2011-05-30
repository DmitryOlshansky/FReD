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
    IRchar              =  0,
    IRany               =  1,
    IRcharset           =  2,
    IRstartoption       =  3,
    IRoption            =  4,
    IRendoption         =  5,
    IRstartinfinite     =  6, 
    IRstartrepeat       =  7,
    IRrepeat            =  8,
    IRrepeatq           =  9,
    IRinfinite          = 10, 
    IRinfiniteq         = 11, 
    IRdigit             = 12, 
    IRnotdigit          = 13,
    IRspace             = 14,
    IRnotspace          = 15,
    IRword              = 16,
    IRnotword           = 17,    
    IRstartgroup        = 18,
    IRendgroup          = 19,
    IRgoto              = 20,
    
    IRbackref           = 30,
    IRwordboundary      = 31,
    IRnotwordboundary   = 32,
    IRlookahead         = 33,
    IRneglookahead      = 34,
    IRlookbehind        = 35,
    IRneglookbehind     = 36,
    
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
    R pat, origin;  //keep full pattern for pretty printing error messages
    uint[] ir;      //resulting bytecode
    uint[] index;   //user group number -> internal number
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
        ir.reserve(pat.length);
        next();
        parseRegex();
    }
    @property dchar current(){ return _current; }
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
    void put(uint code){  ir ~= code; }    
    
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
        while(!empty)
            switch(current)
            {
                case '|'://alternation
                    put(opgen(IRstartoption));// +1 word
                    effectiveLength++;
                    uint anchor = cast(uint)(ir.length); //points to first option
                    do
                    {//TODO: check overflows
                        nsub = nesting;//reuse groups across alternations
                        next();
                        uint offset = cast(uint)(ir.length);
                        put(0); //reserve space
                        effectiveLength += parseRepetition()+1;
                        if(current == '|')      //another option?
                        {
                            put(opgen(IRendoption));   //we can turn this into jump later
                            effectiveLength++;
                        }
                        uint len = cast(uint)(ir.length - offset - 1);
                        assert(len < (1<<24));
                        ir[offset] = opgen(IRoption,  len);
                    }while(current == '|'); //process all options of alternation
                    //TODO: account empty alternation?  (a|b|) -> (a|b|*lambda*)
                    break;
                case ')':
                    return effectiveLength;
                default:
                    effectiveLength += parseRepetition();
            }
        return effectiveLength;
    }
    /*
        Parse and store IR for atom-quantifier pair, returns effective length of IR
    */
    uint parseRepetition()
    {
        uint offset = cast(uint)ir.length;
        uint effectiveLength = parseAtom();
        uint len = cast(uint)ir.length - offset;
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
            insertInPlace(ir, offset, opgen(IRstartrepeat, len)); // + 1 word
            put(opgen(greedy ? IRrepeat : IRrepeatq, len));
            put(effectiveLength); //step of RIN counter
            put(min);
            put(max);
            effectiveLength = checkedMulAdd(max,effectiveLength,5);
        }
        else if(min) // && max is infinite
        {
            insertInPlace(ir, offset, opgen(IRstartrepeat, len));// + 1 word
            offset += 1;//so it still points to the repeated block
            put(opgen(greedy ? IRrepeat : IRrepeatq, len));//TODO: include step
            put(effectiveLength); //step of RIN counter
            put(min);
            put(min);
            put(opgen(IRstartinfinite, len));
            ir ~= ir[offset .. offset+len];// + another effectiveLength
            put(opgen(greedy ? IRinfinite : IRinfiniteq, len));
            effectiveLength = checkedMulAdd((min+1),effectiveLength,7);
        }
        else//vanila {0,inf}
        {
            insertInPlace(ir, offset, opgen(IRstartinfinite, len));// + 1 word
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
                    break;
                case '!':
                    op = IRneglookahead;
                    next();
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
                    auto old = nsub++;
                    nesting++;
                    top = max(nsub,top);//count max capture stack usage
                    nglob = cast(uint)index.length;
                    index ~= old;
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
                    break;
                default:
                    error(" ':', '=', '<', 'P' or '!' expected after '(?' ");
                }
            }
            else
            {
                auto old = nsub++;
                nesting++;
                top = max(nsub,top);//count max capture stack usage
                nglob = cast(uint)index.length;
                index ~= old;
                op = opgen(IRstartgroup, nglob);
                
            }
            if(op) 
            {
                effectiveLength++;
                put(op); //needs fixup for lookarounds
            }
            uint offset = cast(uint)ir.length;
            effectiveLength += parseRegex();
            uint len = cast(uint)ir.length - offset;
            if(current != ')')
            {
                pat = save;
                error("Unmatched '(' in regex pattern");
            }
            assert(index.length < (1<<24));
            if(opcode(op) == IRstartgroup)
            {
                assert(nesting);
                --nesting;
                put(opgen(IRendgroup, nglob));
                effectiveLength++;
            }
            else if(op)
            {
                ir[offset-1] = opgen(ir[offset-1],len);    
            }
            next();
            return effectiveLength;
        case '[':
            //range
            assert(0);
            break;
        case '\\':
            //escape
            next() || error("Unfinished escape sequence");
            put(escape());
            break;
        case ')':
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
    
    void error(string msg)
    {
        auto app = appender!string;
        formattedWrite(app,"%s\nPattern with error: `%s <--HERE-- %s`",
                       msg, origin[0..$-pat.length], pat);
        throw new RegexException(app.data);
    }
    
    void print()
    {
        uint nsub;
        uint[] nsub_save;//used to simulate normal execution of alternation
        uint[] group;
        writeln("PC\tINST");
        for(size_t i=0;i<ir.length;i++)
        {
            writef("%d\t",i);
            switch(opcode(ir[i]))
            {
            case IRchar:
                write("char ",cast(dchar)ir[i]);
                break;
            case IRany:
                write("any char");
                break;
            case IRstartrepeat:
                uint len = opdata(ir[i]);
                writef("start repeat pc=>%u", i+len+1);
                break;
            case IRstartinfinite:
                uint len = opdata(ir[i]);
                writef("start infinite pc=>%u", i+len+1);
                break;
            case IRrepeat:
            case IRrepeatq:
                uint len = opdata(ir[i]);
                writef("repeat%s pc=>%u min=%u max=%u (dRIN=%u)", 
                       opcode(ir[i]) == IRrepeatq ? "q" : "",
                       i-len, ir[i+2], ir[i+3],ir[i+1]);
                i += 3;//3 extra operands
                break;
            case IRinfinite:
            case IRinfiniteq:
                uint len = opdata(ir[i]);
                writef("infinite%s pc=>%u ", 
                       opcode(ir[i]) == IRinfiniteq ? "q" : "", i-len);
                break;
            case IRstartoption:
                writef("start option");
                break;
            case IRoption:
                uint len = opdata(ir[i]);
                nsub_save ~= nsub;
                writef("option pc=>%u", i+len+1);
                break;

            case IRendoption:
                uint len = opdata(ir[i]);
                assert(nsub_save.length);
                nsub = nsub_save.back;
                nsub_save.length -= 1;
                writef("end option pc=>%u", i+len+1);
                break;
            case IRstartgroup: 
            case IRendgroup:
                uint n = opdata(ir[i]);
                // Ouch: '!vthis->csym' on line 713 in file 'glue.c'
                //auto ng = find!((x){ return x.group == n; })(dict); 
                string name;
                foreach(v;dict)
                    if(v.group == n)
                    {
                        name = "<"~v.name~">";
                        break;   
                    }
                if(opcode(ir[i]) == IRstartgroup)
                {
                    group ~= nsub;
                    nsub++;
                    writef("start group %s #%u (internal %u)",
                       name, n,  group.back);    
                }
                else
                {
                    assert(group.length);
                    auto x = group.back;
                    group.length -= 1;
                    writef("end group %s #%u (internal %u)",
                       name, n, x);
                }
                break;
            case IRlookahead:
                uint len = opdata(ir[i]);
                writef("lookahead pc=>%u",  i+len+1);
                break;
            case IRneglookahead: 
                uint len = opdata(ir[i]);
                writef("neglookahead pc=>%u",  i+len+1);
                break;
            case IRlookbehind:
                uint len = opdata(ir[i]);
                writef("lookbehind pc=>%u",  i+len+1);
                break;
            case IRneglookbehind:
                uint len = opdata(ir[i]);
                writef("neglookbehind pc=>%u",  i+len+1);
                break;
            case IRbackref:
                uint n = opdata(ir[i]);
                writef("backref %u",  n);
                break;
            }
            writeln();
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