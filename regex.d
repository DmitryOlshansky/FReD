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

import std.stdio, core.stdc.stdlib, std.array, std.algorithm, std.range,
       std.conv, std.exception, std.ctype, std.traits, std.typetuple,
       std.uni, std.utf, std.format, std.typecons;

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
};

//single IR instruction
struct Bytecode
{
    uint raw;
    this(uint code, uint data, bool hotspot=false)
    { 
        
        assert(data < (1<<24) && code < 128);
        raw = code<<25 | data | (hotspot ? 1<<24  : 0);
    }    
    static Bytecode fromRaw(uint data)
    { 
        Bytecode t;
        t.raw = data;
        return t;
    }    
    //bit twiddling helpers
    @property uint data(){ return raw & 0x00ff_ffff; }
    @property uint code(){ return raw>>25; }
    @property bool hotspot(){ return (raw & (1<<24)) != 0; }
    //
    @property string mnemonic()
    {
        switch(code)
        {
        case IRchar:            return "char";
        case IRany:             return "any char";
        case IRword:            return "word";
        case IRnotword:         return "not word";
        case IRdigit:           return "digit";
        case IRnotdigit:        return "not digit";
        case IRspace:           return "space";
        case IRnotspace:        return "not space";
        case IRwordboundary:    return "word-boundary";
        case IRnotwordboundary: return "not word-boundary";
        case IRbol:             return "begining-of-line";
        case IReol:             return "end-of-line";
        case IRstartrepeat:     return "start repeat";
        case IRstartinfinite:   return "start infinite";
        case IRrepeat:          return "repeat";
        case IRrepeatq:         return "repeatq";
        case IRinfinite:        return "infinite";
        case IRinfiniteq:       return "infiniteq";
        case IRstartoption:     return "start option";
        case IRoption:          return "option";
        case IRendoption:       return "end option";
        case IRstartgroup:      return "start group";
        case IRendgroup:        return "end group";
        case IRlookahead:       return "lookahead";
        case IRneglookahead:    return "neglookahead";
        case IRlookbehind:      return "lookbehind";
        case IRneglookbehind:   return "neglookbehind";
        case IRbackref:         return "backref";
        case IRret:             return "return";
        default:    
            assert(0,"Illegal instruction");
        }
    }
    @property uint length()
    {
        switch(code)
        {
        case IRrepeat, IRrepeatq:
             //[opcode | len], step, min, max
            return 4 + (hotspot ? 1 : 0);
        default:
            return 1 + (hotspot ? 1 : 0);
        }
    }
}

static assert(Bytecode.sizeof == 4);


//debug tool
string disassemble(Bytecode[] irb, uint pc, uint[] index, NamedGroup[] dict=[])
{
    auto output = appender!string();
    formattedWrite(output,"%s", irb[pc].mnemonic);
    switch(irb[pc].code)
    {
    case IRchar:
        formattedWrite(output, " %s",cast(dchar)irb[pc].data);
        break;
    case IRstartrepeat, IRstartinfinite, IRoption, IRendoption, IRstartoption:
        //forward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc+len+1);
        break;
    case IRrepeat, IRrepeatq: //backward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u min=%u max=%u step=%u", 
                pc-len, irb[pc+2].raw, irb[pc+3].raw, irb[pc+1].raw);
        break;
    case IRinfinite, IRinfiniteq: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u ", pc-len);
        break;
    case IRstartgroup, IRendgroup:
        uint n = irb[pc].data;
        // Ouch: '!vthis->csym' on line 713 in file 'glue.c'
        //auto ng = find!((x){ return x.group == n; })(dict); 
        string name;
        foreach(v;dict)
            if(v.group == n)
            {
                name = "'"~v.name~"'";
                break;   
            }
        formattedWrite(output, " %s #%u (internal %u)",
                name, n,  index[n]);    
        break;
    case IRlookahead, IRneglookahead, IRlookbehind, IRneglookbehind:    
        uint len = irb[pc].data;
        formattedWrite(output, " next=%u", pc + len + 1);
        break;
    case IRbackref:
        uint n = irb[pc].data;
        formattedWrite(output, " %u",  n);
        break;
        break;
    default://all data-free instructions
    }
    if(irb[pc].hotspot)
        formattedWrite(output," HOTSPOT %u", irb[pc+irb[pc].length-1].raw);
    return output.data;
}

enum RegexOption: uint { global = 0x1, caseinsensitive = 0x2, freeform = 0x4};

//multiply-add, throws exception on overflow
uint checkedMulAdd(uint f1, uint f2, uint add)
{
    ulong r = f1 * cast(ulong)f2 + add;
    if(r < (1<<32UL))
        throw new RegexException("Regex internal errror - integer overflow");
    return cast(uint)r;
}

struct NamedGroup
{ 
    string name; 
    uint group;
}

struct Group
{ 
    size_t begin, end;
}

struct RecursiveParser(R,bool markHotspots)
if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;       //keep full pattern for pretty printing error messages
    Bytecode[] ir;       //resulting bytecode
    uint level = 0;      //current lookaround level
    uint[] index;        //user group number -> internal number
    uint re_flags = 0;   //global flags e.g. multiline + internal ones
    NamedGroup[] dict;   //maps name -> user group number
    //current num of group, group nesting level and repetitions step
    uint ngroup = 1, nesting = 0;
    uint counterStep = 1, counterDepth = 0; 
    static if (markHotspots)
    {
        bool nextHotspot;
        uint hotspotIndex;
    }
    this(R pattern)
    {
        pat = origin = pattern;    
        index = [ 0 ]; //map first to start-end of the whole match
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
    void put(Bytecode code){  ir ~= code; }   
    void putRaw(uint number){ ir ~= Bytecode.fromRaw(number); }
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
        Parse and store IR for sub regex
    */
    void parseRegex()
    {
        uint start = cast(uint)ir.length;
        auto subSave = ngroup;
        auto maxStep = counterStep;
        auto maxCounterDepth = counterDepth;
        while(!empty && current != '|' && current != ')')
        {
            auto saveStep = counterStep;
            auto saveCounterDepth = counterDepth;
            parseRepetition();
            maxStep = max(counterStep, maxStep);
            maxCounterDepth = max(counterDepth, maxCounterDepth);
            counterStep = saveStep;
            counterDepth = saveCounterDepth;
        } 
        if(!empty)
            switch(current)
            {
            case ')':
                nesting || error("Unmatched ')'");
                nesting--;
                break;
            case '|':
                static if(markHotspots)
                {
                    Bytecode[3] piece = nextHotspot ? 
                    [Bytecode.init, Bytecode.init, Bytecode(IRoption, ir.length - start + 1)] 
                    : [Bytecode.init, Bytecode(IRoption, ir.length - start + 1), Bytecode.init];
                    insertInPlace(ir, start, piece[0..$-nextHotspot]); 
                }
                else
                {
                    Bytecode[2] piece = [Bytecode.init, Bytecode(IRoption, ir.length - start + 1)];
                    insertInPlace(ir, start, piece[]);
                }
                put(Bytecode(IRendoption, 0)); 
                uint anchor = cast(uint)(ir.length); //points to first option
                uint maxSub = 0; //maximum number of captures out of each code path
                do
                {
                    next();
                    uint offset = cast(uint)(ir.length);
                    put(Bytecode.init); //reserve space
                    
                    while(!empty && current != '|' && current != ')')
                    {
                        auto saveStep = counterStep;
                        auto saveCounterDepth = counterDepth;
                        parseRepetition();
                        maxStep = max(counterStep,maxStep);
                        maxCounterDepth = max(counterDepth, maxCounterDepth);
                        counterStep = saveStep;
                        counterDepth = saveCounterDepth;
                    }
                    if(current == '|')      //another option?
                    {
                        put(Bytecode(IRendoption, 0));//mark now, fixup later   
                    }
                    uint len = cast(uint)(ir.length - offset - 1);
                    len < (1<<24) || error("Internal error - overflow");
                    ir[offset] = Bytecode(IRoption,  len);
                    maxSub = max(ngroup,maxSub);
                    ngroup = subSave; //reuse groups across alternations
                }while(current == '|');
                static if(markHotspots)
                {
                    ir[start] = Bytecode(
                        IRstartoption, 
                        ir.length - start - 1 + (nextHotspot ? 1 : 0),nextHotspot);   
                    uint pc = start + 1 + nextHotspot;
                    if(nextHotspot)
                    {
                        ir[start+1] = Bytecode.fromRaw(hotspotIndex);
                        hotspotIndex += counterStep;
                    }
                    nextHotspot = true; //hotspot comes after option
                }
                else
                {
                    ir[start] = Bytecode(IRstartoption, ir.length - start - 1);                
                    uint pc = start + 1;
                }
                //fixup
                while(pc < ir.length)
                {
                    pc = pc + ir[pc].data;
                    if(ir[pc].code != IRendoption)
                        break;
                    ir[pc] = Bytecode(IRendoption,cast(uint)(ir.length - pc - 1));
                    pc++;
                }
                ngroup = maxSub;
                if(current == ')') 
                    goto case ')';
                break;
            default:
            }
        counterStep = maxStep;
        counterDepth = maxCounterDepth;
    }
    /*
        Parse and store IR for atom-quantifier pair
    */
    void parseRepetition()
    {
        uint offset = cast(uint)ir.length;
        parseAtom();
        uint len = cast(uint)ir.length - offset;
        if(empty)
            return;
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
            //skipSpace();
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
            return;
        }
        bool greedy = true;
        //check only if we managed to get new symbol
        if(next() && current == '?')
        {
            greedy = false;
            next();
        }
        if(max != infinite)
        {
            if(min != 1 || max != 1)
            {
                insertInPlace(ir, offset, Bytecode(IRstartrepeat, len));
                put(Bytecode(greedy ? IRrepeat : IRrepeatq, len));
                putRaw(counterStep); 
                putRaw(min*counterStep);
                putRaw(max*counterStep);
                counterStep = (max+1)*counterStep;
                counterDepth++;
            }
        }
        else if(min) // && max is infinite
        {
            if(min != 1)
            {
                insertInPlace(ir, offset, Bytecode(IRstartrepeat, len));
                offset += 1;//so it still points to the repeated block
                put(Bytecode(greedy ? IRrepeat : IRrepeatq, len));
                putRaw(counterStep);
                putRaw(min*counterStep);
                putRaw(min*counterStep);
                counterDepth++;
            }
            put(Bytecode(IRstartinfinite, len));
            ir ~= ir[offset .. offset+len];
            //IRinfinteX is always a hotspot
            static if(markHotspots)
            {
                put(Bytecode(greedy ? IRinfinite : IRinfiniteq, len, true));
                putRaw(0);
            }
            else
                put(Bytecode(greedy ? IRinfinite : IRinfiniteq, len));
            if(min != 1) 
                counterStep = (min+1)*counterStep;
        }
        else//vanila {0,inf}
        {
            insertInPlace(ir, offset, Bytecode(IRstartinfinite, len));
            //IRinfinteX is always a hotspot
            static if(markHotspots)
            {
                put(Bytecode(greedy ? IRinfinite : IRinfiniteq, len, true));
                putRaw(0);
            }
            else
                put(Bytecode(greedy ? IRinfinite : IRinfiniteq, len));
            
        }
    }
    /*
        Parse and store IR for atom
    */
    void parseAtom()
    {
        if(empty)
            return;
        switch(current)
        {
        case '*', '?', '+', '|', '{', '}':
            error("'*', '+', '?', '{', '}' not allowed in atom");
            break;
        case '.':
            put(Bytecode(IRany, 0));
            next();
            break;
        case '(':
            R save = pat;
            next();
            Bytecode op;
            uint nglob;
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
                    op = Bytecode(IRlookahead, 0);
                    next();
                    lookaround = true;
                    break;
                case '!':
                    op = Bytecode(IRneglookahead, 0);
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
                    nglob = cast(uint)index.length-1;//not counting whole match
                    index ~= ngroup++;
                    auto t = NamedGroup(name,nglob);
                    auto d = assumeSorted!"a.name < b.name"(dict);
                    auto ind = d.lowerBound(t).length;
                    insertInPlace(dict, ind, t);
                    op = Bytecode(IRstartgroup, nglob);
                    break;
                case '<':
                    next();
                    if(current == '=')
                        op = Bytecode(IRlookbehind, 0);
                    else if(current == '!')
                        op = Bytecode(IRneglookbehind, 0);
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
                nglob = cast(uint)index.length-1;//not counting whole match
                index ~= ngroup++; //put local index
                op = Bytecode(IRstartgroup, nglob);
            }            
            if(lookaround)
            {
                uint offset = cast(uint)ir.length;
                parseRegex();
                put(Bytecode(IRret, 0));
                put(Bytecode(op.code, cast(uint)(ir.length - offset)));
            }
            else
            {
                if(op != Bytecode.init) //currently only groups
                {
                    put(op);
                }
                //auto saveStep = counterStep;
                parseRegex();
                //counterStep = saveStep;
            }
            if(op.code == IRstartgroup)
            {
                put(Bytecode(IRendgroup, nglob));
            }
            if(current != ')')
            {
                pat = save;
                error("Unmatched '(' in regex pattern");
            }
            next();
            break;
        case '[':
            //range
            assert(0, "Codepoint set not implemented");
            break;
        case '\\':
            next() || error("Unfinished escape sequence");
            put(escape());
            break;
        case '^':
            put(Bytecode(IRbol, 0));
            next();
            break;
        case '$':
            put(Bytecode(IReol, 0));
            next();
            break;
        default:
            put(Bytecode(IRchar, current));
            next();
        }
    }
    
    Bytecode escape()
    {
        switch(current)
        {
        case 'f':   next(); return Bytecode(IRchar,'\f');
        case 'n':   next(); return Bytecode(IRchar, '\n');
        case 'r':   next(); return Bytecode(IRchar, '\r');
        case 't':   next(); return Bytecode(IRchar, '\t');
        case 'v':   next(); return Bytecode(IRchar, '\v');
            
        case 'd':   next(); return Bytecode(IRdigit, 0); 
        case 'D':   next(); return Bytecode(IRnotdigit, 0); 
        case 'b':   next(); return Bytecode(IRwordboundary, 0);
        case 'B':   next(); return Bytecode(IRnotwordboundary, 0);
        case 's':   next(); return Bytecode(IRspace, 0);
        case 'S':   next(); return Bytecode(IRnotspace, 0);
        case 'w':   next(); return Bytecode(IRword, 0);
        case 'W':   next(); return Bytecode(IRnotword, 0);
        case 'x':            
            auto save = pat;
            uint code = 0;
            for(int i=0;i<2;i++)
            {
                if(!next())
                {
                    restart(save);
                    return Bytecode(IRchar, 'x');
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
                    return Bytecode(IRchar, 'x');
                }
            }
            next();
            return Bytecode(IRchar,code);
        case 'u':
            auto save = pat;
            uint code = 0;
            for(int i=0; i<4; i++)
            {
                if(!next())
                {
                    restart(save);
                    return Bytecode(IRchar, 'u');
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
                    return Bytecode(IRchar, 'u');
                }
            }
            next();
            return Bytecode(IRchar, code);
        case 'c': //control codes                      
            next() || error("Unfinished escape sequence");
            ('a' <= current && current <= 'z') || ('A' <= current && current <= 'Z')
                || error("Only letters are allowed after \\c");
            Bytecode code = Bytecode(IRchar, current &  0x1f);
            next();
            return code;
        case '0':
            next();
            return Bytecode(IRchar, 0);//NUL character
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
            return Bytecode(IRbackref, nref);
        default:
            auto op = Bytecode(IRchar, current);
            next();
            return op;
        }
    }
    //
    void error(string msg)
    {
        auto app = appender!string;
        ir = null;
        formattedWrite(app,"%s\nPattern with error: `%s <--HERE-- %s`",
                       msg, origin[0..$-pat.length], pat);
        throw new RegexException(app.data);
    }
    /*
    */
    @property Program program()
    { 
        return Program(this); 
    }
}
//for backwards comaptibility
struct Regex(Char)
    if(is(Char : char) || is(Char : wchar) || is(Char : dchar))
{
    Program storage;
    this(Program rs){ storage = rs; }
    alias storage this;

}
//holds all persistent data about compiled regex
struct Program
{
    Bytecode[] ir;
    uint[] index;       //user group number -> internal number
    NamedGroup[] dict;  //maps name -> user group number
    uint ngroup;        //number of internal groups
    uint maxCounterDepth; //max depth of nested {n,m} repetitions
    uint flags;         //global regex flags   
    //
    this(Parser)(Parser p)
    {
        ir = p.ir;
        index = p.index;
        dict = p.dict;
        ngroup = p.ngroup;
        maxCounterDepth = p.counterDepth;
        flags = p.re_flags;
    }
    //
    void processHotspots()
    {
        uint[] counterRange = new uint[maxCounterDepth+1];
        uint hotspotIndex = 0;
        uint top = 0;
        counterRange[0] = 1;
        
        for(size_t i=0; i<ir.length; i+=ir[i].length)
        {
            
            if(ir[i].code == IRstartrepeat)
            {
                uint len = ir[i].data;
                assert(ir[i+len+1].code == IRrepeat);
                counterRange[++top] = ir[i+len+4].raw;
            }
            else if(ir[i].code == IRrepeat)
            {
                top--;
            }
            if(ir[i].hotspot)
            {
                ir[i+1].raw = hotspotIndex;
                hotspotIndex += counterRange[top];
            }
        }
    }
    //
    void print()
    {
        writefln("PC\tINST\n");
        for(size_t i=0; i<ir.length; i+=ir[i].length)
        {
            writefln("%d\t%s", i, disassemble(ir, i, index, dict));
        }
        writefln("Max counter nesting depth %u ",maxCounterDepth);
    }
}

/*
    BacktrackingMatcher implements backtracking scheme of matching
    regular expressions. 
    low level construct, doesn't 'own' any memory
*/
struct BacktrackingMatcher(Char)
if( is(Char : dchar) )
{
    alias immutable(Char)[] String;
    Program re;           //regex program
    enum initialStack = 2048;
    String origin, s;
    bool exhausted;
    uint[] mainStack;
    this(Program program, String input)
    {
        re = program;
        origin = s = input;
        exhausted = false;
        mainStack = (cast(uint*)enforce(malloc(initialStack*uint.sizeof)))
                [0..initialStack];
    }
    ~this()
    {
        free(mainStack.ptr);
    }
    @property dchar previous()
    {
        return origin[0..$-s.length].back;
    }
    @property size_t inputIndex(){ return origin.length - s.length; }
    //lookup next match fill matches with indices into input
    bool match(Group matches[])
    {
        debug
        {
            writeln("------------------------------------------");
            re.print();
        }
        if(exhausted) //all matches collected
            return false;
        for(;;)
        {
            size_t start = origin.length - s.length;
            if(matchImpl(matches[1..$], mainStack))
            {//s updated
                matches[0].begin = start;
                matches[0].end = origin.length - s.length;
                //empty match advances the input
                if(matches[0].begin == matches[0].end && !s.empty)
                    s.popFront();
                if(!(re.flags & RegexOption.global))
                    exhausted = true;
                return true;
            }
            if(s.empty)
                break;
            s.popFront();
        }
        exhausted = true;
        return false;
    }
    /*
        same as three argument version, but creates temporary buffer
    */
    bool matchImpl(uint level, Group[] matches)
    {
        bool stackOnHeap;
        uint* mem = cast(uint*)alloca(initialStack*uint.sizeof);
        if(!mem)
        {
            stackOnHeap = true;
            mem = cast(uint*)enforce(malloc(initialStack*uint.sizeof));
        }
        uint[] stack = mem[0..initialStack];
        //stack can be reallocated in matchImpl
        scope(exit) if(stackOnHeap) free(stack.ptr);
        return matchImpl(matches, mem[0..initialStack]);
    }
    /*
        match subexpression against input, being on lookaround level 'level'
        storing results in matches
    */
    bool matchImpl(Group[] matches, ref uint[] stack)
    {  
        enum headWords = size_t.sizeof/uint.sizeof + 3;//size of a thread state head
        enum groupSize = Group.sizeof/uint.sizeof;
        uint pc, counter;
        uint last;          //top of stack
        Bytecode[] prog = re.ir;
        //TODO: it's smaller, make parser count nested infinite loops
        size_t[] trackers = new uint[matches.length+1];
        uint infiniteNesting = -1;// intentional
        /*
            helper function saves engine state
        */
        void pushState(uint pc, uint counter)
        {//TODO: more options on out of memory
            if(last + headWords + matches.length*groupSize >= stack.length)
            {
                stack = (cast(uint*)realloc(stack.ptr,stack.length*2*uint.sizeof))
                        [0..stack.length*2];
            }
            stack[last++] = pc;
            stack[last++] = counter;
            stack[last++] = infiniteNesting;
            static if(size_t.sizeof == uint.sizeof)
            {
                stack[last++] = inputIndex;
            }
            else static if(size_t.sizeof == 2*uint.sizeof)
            {
                *cast(size_t*)&stack[last] = inputIndex; 
                last += 2;
            }
            else
                pragma(error,"32 & 64 bits only");
            stack[last..last+matches.length*groupSize] = cast(uint[])matches[];
            last += matches.length*groupSize;
        }
        //helper function restores engine state
        bool popState()
        {
            if(!last)
                return false;
            last -= matches.length*groupSize;
            matches[] = cast(Group[])stack[last..last+matches.length*groupSize];
            last -= headWords;
            pc = stack[last];
            counter = stack[last+1];
            infiniteNesting = stack[last+2];
            static if(size_t.sizeof == uint.sizeof)
            {
                s = origin[stack[last+3] .. $];
            }
            else static if(size_t.sizeof == 2*uint.sizeof)
            {
                s = origin[*cast(size_t*)&stack[last+3] .. $];
            }
            else
                pragma(error,"32 & 64 bits only");
            return true;
        }   
        auto start = origin.length - s.length;
        debug writeln("Try match starting at ",origin[inputIndex..$]);
        while(pc<prog.length)
        {
            debug writefln("%d\t%s", pc, disassemble(prog, pc, re.index, re.dict));
            switch(prog[pc].code)
            {
            case IRchar:
                if(s.empty || s.front != prog[pc].data)
                   goto L_backtrack;
                pc++;
                s.popFront();
            break;
            case IRany:
                if(s.empty)
                    goto L_backtrack;
                pc++;
                s.popFront();
                break;
            case IRword:
                if(s.empty || !isUniAlpha(s.front))
                    goto L_backtrack;
                s.popFront();
                pc++;
                break;
            case IRnotword:
                if(s.empty || isUniAlpha(s.front))
                    goto L_backtrack;
                s.popFront();
                pc++;
                break;
            case IRdigit:
                if(s.empty || !isdigit(s.front))
                    goto L_backtrack;
                s.popFront();
                pc++;
                break;
            case IRnotdigit:
                if(s.empty || isdigit(s.front))
                    goto L_backtrack;
                s.popFront();
                pc++;
                break;
            case IRspace:
                if(s.empty || !isspace(s.front))
                    goto L_backtrack;
                s.popFront();
                pc++;
                break;
            case IRnotspace:
                if(s.empty || isspace(s.front))
                    goto L_backtrack;
                s.popFront();
                pc++;
                break;
            case IRwordboundary:
                //at start & end of input
                if((s.empty && isUniAlpha(s.front))
                   || (s.length == origin.length && isUniAlpha(s.front)) )
                    pc++;
                else if( (isUniAlpha(s.front) && !isUniAlpha(previous))
                      || (!isUniAlpha(s.front) && isUniAlpha(previous)) )
                    pc++;
                else
                    goto L_backtrack;
                break;
            case IRnotwordboundary:
                if((s.empty && isUniAlpha(s.front))
                   || (s.length == origin.length && isUniAlpha(previous)) )
                    goto L_backtrack;
                else if( (isUniAlpha(s.front) && !isUniAlpha(previous))
                      || (!isUniAlpha(s.front) && isUniAlpha(previous)) )
                    goto L_backtrack;
                else
                    pc++;
                break;
            case IRbol:
                //TODO: multiline & attributes, unicode line terminators
                if(s.length == origin.length || previous == '\n')
                    pc++;
                else
                    goto L_backtrack;
                break;
            case IReol:
                //TODO: ditto for the begining of line
                if(s.empty || s.front == '\n')
                    pc++;
                else
                    goto L_backtrack;
                break;
            case IRstartinfinite: 
                trackers[infiniteNesting+1] = inputIndex;
                pc += prog[pc].data + 1;
                uint len = prog[pc].data;
                if(prog[pc].code == IRinfinite)
                {
                    pushState(pc+1, counter);
                    infiniteNesting++;
                    pc -= len;
                }
                else
                {
                    pushState(pc-len, counter);
                    pc++;
                }
                break;
            case IRstartrepeat:
                pc += prog[pc].data + 1;
                break;
            case IRrepeat:
            case IRrepeatq:
                // len, step, min, max
                uint len = prog[pc].data;
                uint step =  prog[pc+1].raw;
                uint min = prog[pc+2].raw;
                uint max = prog[pc+3].raw;
                //debug writefln("repeat pc=%u, counter=%u",pc,counter);
                uint cnt = counter % (max+1);
                if(cnt < min)
                {
                    counter += step;
                    pc -= len;
                }
                else if(cnt < max)
                {
                    if(prog[pc].code == IRrepeat)
                    {    
                        pushState(pc + 4, counter - counter%(max+1));
                        counter += step;
                        pc -= len;
                    }
                    else
                    {
                        pushState(pc - len, counter + step);   
                        counter -= counter%(max+1);
                        pc += 4; 
                    }
                }
                else
                {
                    counter -= counter%(max+1);
                    pc += 4;
                }                       
                break;
            case IRinfinite:
            case IRinfiniteq:
                uint len = prog[pc].data;
                assert(infiniteNesting < trackers.length);
                if(trackers[infiniteNesting] == inputIndex)
                {//source not consumed
                    pc++;
                    infiniteNesting--;
                    break;
                }
                else
                    trackers[infiniteNesting] = inputIndex;
                
                if(prog[pc].code == IRinfinite)
                {
                    infiniteNesting--;
                    pushState(pc+1, counter);
                    infiniteNesting++;
                    pc -= len;
                    writeln("CHECK ",disassemble(prog, pc, re.index, re.dict));
                }
                else
                {
                    writeln("CHECK2 ",disassemble(prog, pc - len, re.index, re.dict));
                    pushState(pc-len, counter);
                    pc++;    
                    infiniteNesting--;
                }
                break;
            case IRstartoption:
                pc++;
                goto case;
            case IRoption:
                uint len = prog[pc].data;
                if(prog[pc+len].code == IRendoption)//not a last one
                {
                   pushState(pc + len + 1, counter); //remember 2nd branch
                }
                pc++;
                break;
            case IRendoption:
                pc = pc + prog[pc].data + 1;
                break;
            case IRstartgroup: //TODO: mark which global matched and do the other alternatives
                uint n = prog[pc].data;
                matches[re.index[n]].begin = inputIndex;
                debug  writefln("IR group #%u starts at %u", n, inputIndex);
                pc++;
                break;
            case IRendgroup:   //TODO: ditto
                uint n = prog[pc].data;
                matches[re.index[n]].end = inputIndex;
                debug writefln("IR group #%u ends at %u", n, inputIndex);
                pc++;
                break;
            case IRlookahead:
                assert(0, "No impl!");
                break;
            case IRneglookahead: 
                assert(0, "No impl!");
                break;
            case IRlookbehind:
                assert(0, "No impl!");
                break;
            case IRneglookbehind:
                assert(0, "No impl!");
                break;
            case IRbackref:
                uint n = re.index[prog[pc].data];
                auto referenced = origin[matches[n].begin .. matches[n].end];
                if(startsWith(s, referenced))
                {
                    s = s[referenced.length..$];
                    pc++;
                }
                else
                    goto L_backtrack;
                break;
            case IRret:
                assert(0, "No impl!");
                break;
            default:
                assert(0);
            L_backtrack:
                if(!popState()) 
                {
                    s = origin[start..$];
                    return false;
                }
            }
        }
        return true;
    }   
}
/*
    
*/
struct ThompsonMatcher(R)
if(isForwardRange!R && !is(ElementType!R : dchar))
{
    Program re;           //regex program
    
}

/**
*/
struct RegexMatch(R, alias Engine = BacktrackingMatcher)
{
private: 
    R input;
    Group[] matches;
    bool _empty;
    uint[] index;
    uint flags;
    NamedGroup[] named;
    alias Engine!(typeof(R.init[0])) EngineType;
    EngineType engine;
    struct Captures
    {
        RegexMatch m;
        Group[] groups;
        uint[] index;
        this(RegexMatch rmatch)
        {
            m = rmatch;
            index = m.index;
            groups = rmatch.matches;
        }
        @property R front()
        {
            assert(!index.empty);
            return m.input[groups[index[0]].begin .. groups[index[0]].end];  
        }
        @property R back()
        { 
            assert(!index.empty);
            return m.input[groups[index[$-1]].begin .. groups[index[$-1]].end];
        }
        void popFront()
        {
            index = index[1..$];
        }
        void popBack()
        {
            index = index[0..$-1];
        }
        @property bool empty(){ return index.empty; }
        R opIndex(size_t i)
        { 
            return m.input[groups[index[i]].begin..groups[index[i]].end];
        }
        @property size_t length() const { return index.length; }
    }
    
public:
    //
    this(Program prog, R _input)
    {
        input = _input;
        index = prog.index;
        matches = new Group[prog.ngroup];
        engine = EngineType(prog, input);
        popFront();
    }
    @property R pre()
    {
        assert(!empty);
        return input[matches[0].begin .. matches[0].end]; 
    }
    @property R post()
    {
        assert(!empty);
        return input[matches[0].end..$]; 
    }
    @property R hit()
    { 
        assert(!empty);
        return input[matches[0].begin .. matches[0].end]; 
    }
    void popFront()
    { //previous one can have escaped references from Capture object
        matches = new Group[matches.length];
        _empty = !engine.match(matches);
    }
    @property bool empty(){ return _empty; }
    @property captures(){ return Captures(this); }
}

auto regex(S)(S pattern, S flags=[])
{
    auto parser = RecursiveParser!(typeof(pattern),false)(pattern);
    Regex!(Unqual!(typeof(S.init[0]))) r = parser.program;
    return r;
}

auto match(R,C)(R input, Regex!C re)
{
    return RegexMatch!(Unqual!(typeof(input)))(re, input);
}

unittest
{//sanity checks
    regex("abc|edf|ighrg");
    auto r = regex("abc");
    assert(match("abcdef",r).hit == "abc");
    assert(match("wida",regex("(gylba)")).empty);
}

/* The test vectors in this file are altered from Henry Spencer's regexp
   test code. His copyright notice is:

        Copyright (c) 1986 by University of Toronto.
        Written by Henry Spencer.  Not derived from licensed software.

        Permission is granted to anyone to use this software for any
        purpose on any computer system, and to redistribute it freely,
        subject to the following restrictions:

        1. The author is not responsible for the consequences of use of
                this software, no matter how awful, even if they arise
                from defects in it.

        2. The origin of this software must not be misrepresented, either
                by explicit claim or by omission.

        3. Altered versions must be plainly marked as such, and must not
                be misrepresented as being the original software.


 */

unittest
{
    struct TestVectors
    {
        string pattern;
        string input;
        string result;
        string format;
        string replace;
    };

    static TestVectors tv[] = [
        {  "(a)\\1",    "abaab","y",    "&",    "aa" },
        {  "abc",       "abc",  "y",    "&",    "abc" },
        {  "abc",       "xbc",  "n",    "-",    "-" },
        {  "abc",       "axc",  "n",    "-",    "-" },
        {  "abc",       "abx",  "n",    "-",    "-" },
        {  "abc",       "xabcy","y",    "&",    "abc" },
        {  "abc",       "ababc","y",    "&",    "abc" },
        {  "ab*c",      "abc",  "y",    "&",    "abc" },
        {  "ab*bc",     "abc",  "y",    "&",    "abc" },
        {  "ab*bc",     "abbc", "y",    "&",    "abbc" },
        {  "ab*bc",     "abbbbc","y",   "&",    "abbbbc" },
        {  "ab+bc",     "abbc", "y",    "&",    "abbc" },
        {  "ab+bc",     "abc",  "n",    "-",    "-" },
        {  "ab+bc",     "abq",  "n",    "-",    "-" },
        {  "ab+bc",     "abbbbc","y",   "&",    "abbbbc" },
        {  "ab?bc",     "abbc", "y",    "&",    "abbc" },
        {  "ab?bc",     "abc",  "y",    "&",    "abc" },
        {  "ab?bc",     "abbbbc","n",   "-",    "-" },
        {  "ab?c",      "abc",  "y",    "&",    "abc" },
        {  "^abc$",     "abc",  "y",    "&",    "abc" },
        {  "^abc$",     "abcc", "n",    "-",    "-" },
        {  "^abc",      "abcc", "y",    "&",    "abc" },
        {  "^abc$",     "aabc", "n",    "-",    "-" },
        {  "abc$",      "aabc", "y",    "&",    "abc" },
        {  "^",         "abc",  "y",    "&",    "" },
        {  "$",         "abc",  "y",    "&",    "" },
        {  "a.c",       "abc",  "y",    "&",    "abc" },
        {  "a.c",       "axc",  "y",    "&",    "axc" },
        {  "a.*c",      "axyzc","y",    "&",    "axyzc" },
        {  "a.*c",      "axyzd","n",    "-",    "-" },
      //no codepoint sets yet
     /* {  "a[bc]d",    "abc",  "n",    "-",    "-" },
        {  "a[bc]d",    "abd",  "y",    "&",    "abd" },
        {  "a[b-d]e",   "abd",  "n",    "-",    "-" },
        {  "a[b-d]e",   "ace",  "y",    "&",    "ace" },
        {  "a[b-d]",    "aac",  "y",    "&",    "ac" },
        {  "a[-b]",     "a-",   "y",    "&",    "a-" },
        {  "a[b-]",     "a-",   "y",    "&",    "a-" },
        {  "a[b-a]",    "-",    "c",    "-",    "-" },
        {  "a[]b",      "-",    "c",    "-",    "-" },
        {  "a[",        "-",    "c",    "-",    "-" },
        {  "a]",        "a]",   "y",    "&",    "a]" },
        {  "a[]]b",     "a]b",  "y",    "&",    "a]b" },
        {  "a[^bc]d",   "aed",  "y",    "&",    "aed" },
        {  "a[^bc]d",   "abd",  "n",    "-",    "-" },
        {  "a[^-b]c",   "adc",  "y",    "&",    "adc" },
        {  "a[^-b]c",   "a-c",  "n",    "-",    "-" },
        {  "a[^]b]c",   "a]c",  "n",    "-",    "-" },
        {  "a[^]b]c",   "adc",  "y",    "&",    "adc" }, */
        {  "ab|cd",     "abc",  "y",    "&",    "ab" },
        {  "ab|cd",     "abcd", "y",    "&",    "ab" },
        {  "()ef",      "def",  "y",    "&-\\1",        "ef-" },
        {  "()*",       "-",    "y",    "-",    "-" },
        {  "*a",        "-",    "c",    "-",    "-" },
        {  "^*",        "-",    "y",    "-",    "-" },
        {  "$*",        "-",    "y",    "-",    "-" },
        {  "(*)b",      "-",    "c",    "-",    "-" },
        {  "$b",        "b",    "n",    "-",    "-" },
        {  "a\\",       "-",    "c",    "-",    "-" }, 
        {  "a\\(b",     "a(b",  "y",    "&-\\1",        "a(b-" },
        {  "a\\(*b",    "ab",   "y",    "&",    "ab" },
        {  "a\\(*b",    "a((b", "y",    "&",    "a((b" },
        {  "a\\\\b",    "a\\b", "y",    "&",    "a\\b" },
        {  "abc)",      "-",    "c",    "-",    "-" },
        {  "(abc",      "-",    "c",    "-",    "-" },
        {  "((a))",     "abc",  "y",    "&-\\1-\\2",    "a-a-a" },
        {  "(a)b(c)",   "abc",  "y",    "&-\\1-\\2",    "abc-a-c" },
        {  "a+b+c",     "aabbabc","y",  "&",    "abc" },
        {  "a**",       "-",    "c",    "-",    "-" },
        {  "a*?a",      "aa",   "y",    "&",    "a" },
        {  "(a*)*",     "aaa",  "y",    "-",    "-" },
        {  "(a*)+",     "aaa",  "y",    "-",    "-" },
        {  "(a|)*",     "-",    "y",    "-",    "-" },
        {  "(a*|b)*",   "aabb", "y",    "-",    "-" },
        {  "(a|b)*",    "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)*",   "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)+",   "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)?",   "ab",   "y",    "&-\\1",        "a-a" },
   //     {  "[^ab]*",    "cde",  "y",    "&",    "cde" },
        {  "(^)*",      "-",    "y",    "-",    "-" },
        {  "(ab|)*",    "-",    "y",    "-",    "-" },
        {  ")(",        "-",    "c",    "-",    "-" },
        {  "",  "abc",  "y",    "&",    "" },
        {  "abc",       "",     "n",    "-",    "-" },
        {  "a*",        "",     "y",    "&",    "" },
    //    {  "([abc])*d", "abbbcd",       "y",    "&-\\1",        "abbbcd-c" },
    //    {  "([abc])*bcd", "abcd",       "y",    "&-\\1",        "abcd-a" },
        {  "a|b|c|d|e", "e",    "y",    "&",    "e" },
        {  "(a|b|c|d|e)f", "ef",        "y",    "&-\\1",        "ef-e" },
        {  "((a*|b))*", "aabb", "y",    "-",    "-" },
        {  "abcd*efg",  "abcdefg",      "y",    "&",    "abcdefg" },
        {  "ab*",       "xabyabbbz",    "y",    "&",    "ab" },
        {  "ab*",       "xayabbbz",     "y",    "&",    "a" },
        {  "(ab|cd)e",  "abcde",        "y",    "&-\\1",        "cde-cd" },
      //  {  "[abhgefdc]ij",      "hij",  "y",    "&",    "hij" },
        {  "^(ab|cd)e", "abcde",        "n",    "x\\1y",        "xy" },
        {  "(abc|)ef",  "abcdef",       "y",    "&-\\1",        "ef-" },
        {  "(a|b)c*d",  "abcd", "y",    "&-\\1",        "bcd-b" },
        {  "(ab|ab*)bc",        "abc",  "y",    "&-\\1",        "abc-a" },
    /*    {  "a([bc]*)c*",        "abc",  "y",    "&-\\1",        "abc-bc" },
        {  "a([bc]*)(c*d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-bc-d" },
        {  "a([bc]+)(c*d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-bc-d" },
        {  "a([bc]*)(c+d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-b-cd" },
        {  "a[bcd]*dcdcde",     "adcdcde",      "y",    "&",    "adcdcde" },
        {  "a[bcd]+dcdcde",     "adcdcde",      "n",    "-",    "-" },
    */    {  "(ab|a)b*c", "abc",  "y",    "&-\\1",        "abc-ab" },
        {  "((a)(b)c)(d)",      "abcd", "y",    "\\1-\\2-\\3-\\4",      "abc-a-b-d" },
    //    {  "[a-zA-Z_][a-zA-Z0-9_]*",    "alpha",        "y",    "&",    "alpha" },
    //    {  "^a(bc+|b[eh])g|.h$",        "abh",  "y",    "&-\\1",        "bh-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "effgz",        "y",    "&-\\1-\\2",    "effgz-effgz-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "ij",   "y",    "&-\\1-\\2",    "ij-ij-j" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "effg", "n",    "-",    "-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "bcdd", "n",    "-",    "-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "reffgz",       "y",    "&-\\1-\\2",    "effgz-effgz-" },
        {  "(((((((((a)))))))))",       "a",    "y",    "&",    "a" },
        {  "multiple words of text",    "uh-uh",        "n",    "-",    "-" },
        {  "multiple words",    "multiple words, yeah", "y",    "&",    "multiple words" },
        {  "(.*)c(.*)", "abcde",        "y",    "&-\\1-\\2",    "abcde-ab-de" },
        {  "\\((.*), (.*)\\)",  "(a, b)",       "y",    "(\\2, \\1)",   "(b, a)" },
        {  "abcd",      "abcd", "y",    "&-\\&-\\\\&",  "abcd-&-\\abcd" },
        {  "a(bc)d",    "abcd", "y",    "\\1-\\\\1-\\\\\\1",    "bc-\\1-\\bc" },
     /*   {  "[k]",                       "ab",   "n",    "-",    "-" },
        {  "[ -~]*",                    "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~]*",         "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~]*",              "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~]*",           "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~]*",        "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~ -~]*",     "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~ -~ -~]*",  "abc",  "y",    "&",    "abc" },
     */   {  "a{2}",      "candy",                "n",    "",     "" },
        {  "a{2}",      "caandy",               "y",    "&",    "aa" },
        {  "a{2}",      "caaandy",              "y",    "&",    "aa" },
        {  "a{2,}",     "candy",                "n",    "",     "" },
        {  "a{2,}",     "caandy",               "y",    "&",    "aa" },
        {  "a{2,}",     "caaaaaandy",           "y",    "&",    "aaaaaa" },
        {  "a{1,3}",    "cndy",                 "n",    "",     "" },
        {  "a{1,3}",    "candy",                "y",    "&",    "a" },
        {  "a{1,3}",    "caandy",               "y",    "&",    "aa" },
        {  "a{1,3}",    "caaaaaandy",           "y",    "&",    "aaa" },
        {  "e?le?",     "angel",                "y",    "&",    "el" },
        {  "e?le?",     "angle",                "y",    "&",    "le" },
        {  "\\bn\\w",   "noonday",              "y",    "&",    "no" },
        {  "\\wy\\b",   "possibly yesterday",   "y",    "&",    "ly" },
        {  "\\w\\Bn",   "noonday",              "y",    "&",    "on" },
        {  "y\\B\\w",   "possibly yesterday",   "y",    "&",    "ye" },
        {  "\\cJ",      "abc\ndef",             "y",    "&",    "\n" },
        {  "\\d",       "B2 is",                "y",    "&",    "2" },
        {  "\\D",       "B2 is",                "y",    "&",    "B" },
        {  "\\s\\w*",   "foo bar",              "y",    "&",    " bar" },
        {  "\\S\\w*",   "foo bar",              "y",    "&",    "foo" },
        {  "abc",       "ababc",                "y",    "&",    "abc" },
        {  "apple(,)\\sorange\\1",      "apple, orange, cherry, peach", "y", "&", "apple, orange," },
        {  "(\\w+)\\s(\\w+)",           "John Smith", "y", "\\2, \\1", "Smith, John" },
        {  "\\n\\f\\r\\t\\v",           "abc\n\f\r\t\vdef", "y", "&", "\n\f\r\t\v" },
        {  ".*c",       "abcde",                "y",    "&",    "abc" },
        {  "^\\w+((;|=)\\w+)+$", "some=host=tld", "y", "&-\\1-\\2", "some=host=tld-=tld-=" },
        {  "^\\w+((\\.|-)\\w+)+$", "some.host.tld", "y", "&-\\1-\\2", "some.host.tld-.tld-." },
        {  "q(a|b)*q",  "xxqababqyy",           "y",    "&-\\1",        "qababq-b" },
        {  "^(a)(b){0,1}(c*)",   "abcc", "y", "\\1 \\2 \\3", "a b cc" },
        {  "^(a)((b){0,1})(c*)", "abcc", "y", "\\1 \\2 \\3", "a b b" },
        {  "^(a)(b)?(c*)",       "abcc", "y", "\\1 \\2 \\3", "a b cc" },
        {  "^(a)((b)?)(c*)",     "abcc", "y", "\\1 \\2 \\3", "a b b" },
        {  "^(a)(b){0,1}(c*)",   "acc",  "y", "\\1 \\2 \\3", "a  cc" },
        {  "^(a)((b){0,1})(c*)", "acc",  "y", "\\1 \\2 \\3", "a  " },
        {  "^(a)(b)?(c*)",       "acc",  "y", "\\1 \\2 \\3", "a  cc" },
        {  "^(a)((b)?)(c*)",     "acc",  "y", "\\1 \\2 \\3", "a  " },
        {"(?:ab){3}",       "_abababc",  "y","&-\\1","ababab-" },
        {"(?:a(?:x)?)+",    "aaxaxx",     "y","&-\\1-\\2","aaxax--" },
        //no lookahead yet
      /*  {"foo.(?=bar)",     "foobar foodbar", "y","&-\\1", "food-" },
        {"(?:(.)(?!\\1))+",  "12345678990", "y", "&-\\1", "12345678-8" },*/

        ];

    int i;
    sizediff_t a;
    uint c;
    sizediff_t start;
    sizediff_t end;
    TestVectors tvd;

    foreach (Char; TypeTuple!(char, wchar, dchar))
    {
        alias immutable(Char)[] String;
        String produceExpected(Range)(RegexMatch!(Range) m, String fmt)
        {
            String result;
            while (!fmt.empty)
                switch (fmt.front)
                {
                    case '\\':
                        fmt.popFront();
                        if (!isdigit(fmt.front) )
                        {
                            result ~= fmt.front;
                            fmt.popFront();
                            break;
                        }
                        auto nmatch = parse!uint(fmt);
                        if (nmatch < m.captures.length)
                            result ~= m.captures[nmatch];
                    break;
                    case '&':
                        result ~= m.hit;
                        fmt.popFront();
                    break;
                    default:
                        result ~= fmt.front;
                        fmt.popFront();
                }
            return result;
        }
        Regex!(Char) r;
        start = 0;
        end = tv.length;

        for (a = start; a < end; a++)
        {
//             writef("width: %d tv[%d]: pattern='%s' input='%s' result=%s"
//                     " format='%s' replace='%s'\n",
//                     Char.sizeof, a,
//                     tv[a].pattern,
//                     tv[a].input,
//                     tv[a].result,
//                     tv[a].format,
//                     tv[a].replace);

            tvd = tv[a];

            c = tvd.result[0];

            try
            {
                i = 1;
                r = regex(to!(String)(tvd.pattern));
            }
            catch (RegexException e)
            {
                i = 0;
            }

            assert((c == 'c') ? !i : i);

            if (c != 'c')
            {
                auto m = match(to!(String)(tvd.input), r);
                i = !m.empty;
                assert((c == 'y') ? i : !i, text("Match failed pattern: ", tvd.pattern));
                if (c == 'y')
                {
                    auto result = produceExpected(m, to!(String)(tvd.format));
                    assert(result == to!String(tvd.replace),
                           text("Mismatch pattern: ", tvd.pattern," expected:",
                                tvd.replace, " vs ", result));
                }

            }
        }
    }
}

class RegexException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}