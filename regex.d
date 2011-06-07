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
    IRlambda            = 128
};
//IR bit twiddling helpers
uint opcode(uint ir){ return ir >>24; }
uint opdata(uint ir){ return ir & 0x00ff_ffff; }
uint opgen(uint code, uint data=0){ return (code<<24) | data; }

uint instSize(uint inst)
{
    switch(inst)
    {
    case IRrepeat, IRrepeatq:
         //[opcode | len], step, min, max
        return 4;
    default:
        return 1;
    }    
}
//debug tool
string disassemble(uint[] irb, uint pc, uint[] index, NamedGroup[] dict=[])
{
    auto output = appender!string();
    switch(opcode(irb[pc]))
    {
    case IRchar:
        formattedWrite(output, "char %s",cast(dchar)irb[pc]);
        break;
    case IRany:
        formattedWrite(output, "any char");
        break;
    case IRword:
        formattedWrite(output, "word");
        break;
    case IRnotword:
        formattedWrite(output, "not word");
        break;
    case IRdigit:
        formattedWrite(output, "digit");
        break;
    case IRnotdigit:
        formattedWrite(output, "not digit");
        break;
    case IRspace:
        formattedWrite(output, "space");
        break;
    case IRnotspace:
        formattedWrite(output, "not space");
        break;
    case IRwordboundary:
        formattedWrite(output, "word-boundary");
        break;
    case IRnotwordboundary:
        formattedWrite(output, "not word-boundary");
        break;
    case IRbol:
        formattedWrite(output, "begining-of-line");
        break;
    case IReol:
        formattedWrite(output, "end-of-line");
        break;
    case IRstartrepeat:
        uint len = opdata(irb[pc]);
        formattedWrite(output, "start repeat pc=>%u", pc+len+1);
        break;
    case IRstartinfinite:
        uint len = opdata(irb[pc]);
        formattedWrite(output, "start infinite pc=>%u", pc+len+1);
        break;
    case IRrepeat:
    case IRrepeatq:
        uint len = opdata(irb[pc]);
        formattedWrite(output, "repeat%s pc=>%u min=%u max=%u (dRIN=%u)", 
                opcode(irb[pc]) == IRrepeatq ? "q" : "",
                pc-len, irb[pc+2], irb[pc+3],irb[pc+1]);
        pc += 3;//3 extra operands
        break;
    case IRinfinite:
    case IRinfiniteq:
        uint len = opdata(irb[pc]);
        formattedWrite(output, "infinite%s pc=>%u ", 
                opcode(irb[pc]) == IRinfiniteq ? "q" : "", pc-len);
        break;
    case IRstartoption:
        formattedWrite(output, "start option");
        break;
    case IRoption:
        uint len = opdata(irb[pc]);
        formattedWrite(output, "option pc=>%u", pc+len+1);
        break;
    case IRendoption:
        uint len = opdata(irb[pc]);
        formattedWrite(output, "end option pc=>%u", pc+len+1);
        break;
    case IRstartgroup: 
    case IRendgroup:
        uint n = opdata(irb[pc]);
        // Ouch: '!vthis->csym' on line 713 in file 'glue.c'
        //auto ng = find!((x){ return x.group == n; })(dict); 
        string name;
        foreach(v;dict)
            if(v.group == n)
            {
                name = "'"~v.name~"'";
                break;   
            }
        if(opcode(irb[pc]) == IRstartgroup)
        {
            formattedWrite(output, "start group %s #%u (internal %u)",
                name, n,  index[n]);    
        }
        else
        {
            formattedWrite(output, "end group '%s' #%u (internal %u)",
                name, n, index[n]);
        }
        break;
    case IRlookahead:
        uint dest = opdata(irb[pc]);
        formattedWrite(output, "lookahead dest=%u",  dest);
        break;
    case IRneglookahead: 
        uint dest = opdata(irb[pc]);
        formattedWrite(output, "neglookahead dest=%u",  dest);
        break;
    case IRlookbehind:
        uint dest = opdata(irb[pc]);
        formattedWrite(output, "lookbehind dest=%u",  dest);
        break;
    case IRneglookbehind:
        uint dest = opdata(irb[pc]);
        formattedWrite(output, "neglookbehind dest=%u",  dest);
        break;
    case IRbackref:
        uint n = opdata(irb[pc]);
        formattedWrite(output, "backref %u",  n);
        break;
    case IRret:
        formattedWrite(output, "return");
        break;
    default:
        assert(0,"Illegal instruction");
    }
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

struct RecursiveParser(R)
if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;       //keep full pattern for pretty printing error messages
    uint[][] ir;         //resulting bytecode separated by lookaround levels
    uint level = 0;      //current lookaround level
    uint[] index;        //user group number -> internal number
    uint re_flags = 0;   //global flags e.g. multiline + internal ones
    NamedGroup[] dict;   //maps name -> user group number
    //current num of group, group nesting level and repetitions step
    uint ngroup = 1, nesting = 0, counterStep = 1; 
    
    this(R pattern)
    {
        pat = origin = pattern;    
        index = [ 0 ]; //map first to start-end of the whole match
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
        Parse and store IR for sub regex
    */
    void parseRegex()
    {
        uint start = cast(uint)ir[level].length;
        auto subSave = ngroup;
        while(!empty && current != '|' && current != ')')
            parseRepetition();
        if(!empty)
            switch(current)
            {
            case ')':
                nesting || error("Unmatched ')'");
                nesting--;
                return;
            case '|':
                uint[2] piece = [opgen(IRstartoption), opgen(IRoption, ir[level].length - start + 1)];
                insertInPlace(ir[level], start, piece[]); // + 2 
                put(opgen(IRendoption)); // + 1 
                uint anchor = cast(uint)(ir[level].length); //points to first option
                uint maxSub = 0; //maximum number of captures out of each code path
                do
                {
                    next();
                    uint offset = cast(uint)(ir[level].length);
                    put(0); //reserve space
                    while(!empty && current != '|' && current != ')')
                        parseRepetition();
                    if(current == '|')      //another option?
                    {
                        put(opgen(IRendoption));//mark now, fixup later   
                    }
                    uint len = cast(uint)(ir[level].length - offset - 1);
                    len < (1<<24) || error("Internal error - overflow");
                    ir[level][offset] = opgen(IRoption,  len);
                    maxSub = max(ngroup,maxSub);
                    ngroup = subSave; //reuse groups across alternations
                }while(current == '|');
                //fixup
                uint pc = start + 1;
                while(pc < ir[level].length)
                {
                    pc = pc + opdata(ir[level][pc]);
                    if(opcode(ir[level][pc]) != IRendoption)
                        break;
                    ir[level][pc] = opgen(IRendoption,cast(uint)(ir[level].length - pc));
                    pc++;
                }
                ngroup = maxSub;
                if(current == ')') 
                    goto case ')';
                break;
            default:
            }
    }
    /*
        Parse and store IR for atom-quantifier pair
    */
    void parseRepetition()
    {
        uint offset = cast(uint)ir[level].length;
        parseAtom();
        uint len = cast(uint)ir[level].length - offset;
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
            return;
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
                insertInPlace(ir[level], offset, opgen(IRstartrepeat, len));
                put(opgen(greedy ? IRrepeat : IRrepeatq, len));
                put(counterStep); 
                put(min*counterStep);
                put(max*counterStep);
                counterStep = (max+1)*counterStep;
            }
        }
        else if(min) // && max is infinite
        {
            if(min != 1)
            {
                insertInPlace(ir[level], offset, opgen(IRstartrepeat, len));
                offset += 1;//so it still points to the repeated block
                put(opgen(greedy ? IRrepeat : IRrepeatq, len));
                put(counterStep);
                put(min*counterStep);
                put(min*counterStep);
                counterStep = (min+1)*counterStep;
            }
            put(opgen(IRstartinfinite, len));
            ir[level] ~= ir[level][offset .. offset+len];
            put(opgen(greedy ? IRinfinite : IRinfiniteq, len));
        }
        else//vanila {0,inf}
        {
            insertInPlace(ir[level], offset, opgen(IRstartinfinite, len));
            put(opgen(greedy ? IRinfinite : IRinfiniteq, len));
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
            put(opgen(IRany));
            next();
            break;
        case '(':
            R save = pat;
            next();
            uint op = 0, nglob = void;
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
                    nglob = cast(uint)index.length-1;//not counting whole match
                    index ~= ngroup++;
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
                nglob = cast(uint)index.length-1;//not counting whole match
                index ~= ngroup++; //put local index
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
            }
            else
            {
                if(op) //currently only groups
                {
                    put(op);
                }
                parseRegex();
            }
            if(opcode(op) == IRstartgroup)
            {
                put(opgen(IRendgroup, nglob));
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
        case '*', '(', ')', '[', ']', '+', '|', '\\', '{', '}':
            auto op = opgen(IRchar,current);
            next();
            return op;
        default:
            error("Unrecognaized escape sequence");
            assert(0);
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
    //
    void print()
    {
        foreach(lvl, irb; ir)
        {
            writefln("%sPC\tINST", lvl ? "Lookaround level #"~to!string(lvl)~"\n" : "");
            for(size_t i=0; i<irb.length; i+=instSize(irb[i]))
            {
                writefln("%d\t%s", i, disassemble(irb, i, index, dict));
            }
        }
    }
    /*
    */
    @property Program program()
    { 
        assert(!ir.empty);
        writeln("Index ",index);
        debug print();
        return Program(ir, index, dict, ngroup, re_flags); 
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
//holds all persistent information about regex
struct Program
{
    uint[][] ir;
    uint[] index;       //user group number -> internal number
    NamedGroup[] dict;  //maps name -> user group number
    uint ngroup;          //number of local groups
    uint flags;         //global regex flags    
}

//low level construct, doesn't 'own' any memory
struct BacktrackingMatcher(String)
if(isForwardRange!String && !is(String.init[0] : dchar))
{
    Program re;           //regex program
    enum headWords = size_t.sizeof/uint.sizeof + 2;//size of a thread state head
    enum groupSize = Group.sizeof/uint.sizeof;
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
       // free(mainStack.ptr);
    }
    @property dchar previous()
    {
        return origin[0..$-s.length].back;
    }
    @property size_t index(){ return origin.length - s.length; }
    //lookup next match fill matches with indices into input
    bool match(Group matches[])
    {
        if(exhausted) //all matches collected
            return false;
        for(;;)
        {
            size_t start = origin.length - s.length;
            if(matchImpl(0, matches[1..$], mainStack))
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
        return matchImpl(level, matches, mem[0..initialStack]);
    }
    /*
        match subexpression against input, being on lookaround level 'level'
        storing results in matches
    */
    bool matchImpl(uint level, Group[] matches, ref uint[] stack)
    {  
        uint pc, counter;
        uint last;          //top of stack
        uint[] prog = re.ir[level];
        //helper function saves engine state
        void pushState(uint pc, uint counter, size_t saved)
        {//TODO: more options on out of memory
            if(last + headWords + matches.length*groupSize >= stack.length)
            {
                stack = (cast(uint*)realloc(stack.ptr,stack.length*2*uint.sizeof))
                        [0..stack.length*2];
            }
            stack[last++] = pc;
            stack[last++] = counter;
            static if(size_t.sizeof == uint.sizeof)
            {
                stack[last++] = saved;
            }
            else static if(size_t.sizeof == 2*uint.sizeof)
            {
                *cast(size_t*)&stack[last] = saved; 
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
            static if(size_t.sizeof == uint.sizeof)
            {
                s = origin[stack[last+2] .. $];
            }
            else static if(size_t.sizeof == 2*uint.sizeof)
            {
                s = origin[*cast(size_t*)&stack[last] .. $];
            }
            else
                pragma(error,"32 & 64 bits only");
            return true;
        }   
        auto start = origin.length - s.length;
        while(pc<prog.length)
            switch(opcode(prog[pc]))
            {
            case IRchar:
                if(s.empty || s.front != opdata(prog[pc]))
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
                   || (s.length == origin.length && isUniAlpha(previous)) )
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
            case IRstartrepeat:
            case IRstartinfinite: 
                pc += opdata(prog[pc]) + 1;
                break;
            case IRrepeat:
            case IRrepeatq:
                // len, step, min, max
                uint len = opdata(prog[pc]);
                uint step =  prog[pc+1];
                uint min = prog[pc+2];
                uint max = prog[pc+3];
                debug writefln("repeat pc=%u, counter=%u",pc,counter);
                uint cnt = counter % (max+1);
                if(cnt < min)
                {
                    counter += step;
                    pc -= len;
                }
                else if(cnt < max)
                {
                    if(opcode(prog[pc]) == IRrepeat)
                    {    
                        pushState(pc + 4, counter - counter%(max+1), origin.length - s.length);
                        counter += step;
                        pc -= len;
                    }
                    else
                    {
                        pushState(pc - len, counter + step, origin.length - s.length);
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
                bool greedy = opcode(prog[pc]) == IRinfinite;
                uint len = opdata(prog[pc]);
                if(greedy)
                {
                    pushState(pc+1, counter, index);
                    pc -= len;
                }
                else
                {
                    pushState(pc-len, counter, index);
                    pc++;    
                }
                break;
            case IRstartoption:
                pc++;
                //fallthrough
            case IRoption:
                uint len = opdata(prog[pc]);
                if(opcode(prog[pc+len]) == IRendoption)//not a last one
                {
                   pushState(pc + len + 1, counter, index); //remember 2nd branch
                }
                pc++;
                break;
            case IRendoption:
                pc = pc + opdata(prog[pc]) + 1;
                break;
            case IRstartgroup: //TODO: mark which global matched and do the other alternatives
                uint n = opdata(prog[pc]);
                matches[re.index[n]].begin = index;
                debug  writefln("IR group #%u starts at %u", n, index);
                pc++;
                break;
            case IRendgroup:   //TODO: ditto
                uint n = opdata(prog[pc]);
                matches[re.index[n]].end = index;
                debug writefln("IR group #%u ends at %u", n, index);
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
                uint n = re.index[opdata(prog[pc])];
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
        return true;
    }
    
}

struct RegexMatch(R, Engine = BacktrackingMatcher!R)
{
private: 
    R input;
    Group[] matches;
    bool _empty;
    uint[] index;
    uint flags;
    NamedGroup[] named;
    Engine engine;
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
        engine = BacktrackingMatcher!(R)(prog, input);
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
    { //previous one can have escaped references from Capture
        matches = new Group[matches.length];
        _empty = !engine.match(matches);
    }
    @property bool empty(){ return _empty; }
    @property captures(){ return Captures(this); }
}

auto regex(S)(S pattern, S flags=[])
{
    auto parser = RecursiveParser!(typeof(pattern))(pattern);
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
        //infinite zero-width loops not supported yet
      //  {  "()*",       "-",    "y",    "-",    "-" },
        {  "*a",        "-",    "c",    "-",    "-" },
      //  {  "^*",        "-",    "y",    "-",    "-" },
      //  {  "$*",        "-",    "y",    "-",    "-" },
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
      //  {  "(a*)*",     "aaa",  "y",    "-",    "-" },
     //   {  "(a*)+",     "aaa",  "y",    "-",    "-" },
     //   {  "(a|)*",     "-",    "y",    "-",    "-" },
     //   {  "(a*|b)*",   "aabb", "y",    "-",    "-" },
        {  "(a|b)*",    "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)*",   "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)+",   "ab",   "y",    "&-\\1",        "ab-b" },
    //    {  "(a+|b)?",   "ab",   "y",    "&-\\1",        "a-a" },
    //    {  "[^ab]*",    "cde",  "y",    "&",    "cde" },
   //     {  "(^)*",      "-",    "y",    "-",    "-" },
   //     {  "(ab|)*",    "-",    "y",    "-",    "-" },
        {  ")(",        "-",    "c",    "-",    "-" },
        {  "",  "abc",  "y",    "&",    "" },
        {  "abc",       "",     "n",    "-",    "-" },
        {  "a*",        "",     "y",    "&",    "" },
    /*    {  "([abc])*d", "abbbcd",       "y",    "&-\\1",        "abbbcd-c" },
        {  "([abc])*bcd", "abcd",       "y",    "&-\\1",        "abcd-a" },
        {  "a|b|c|d|e", "e",    "y",    "&",    "e" },
        {  "(a|b|c|d|e)f", "ef",        "y",    "&-\\1",        "ef-e" },
        {  "((a*|b))*", "aabb", "y",    "-",    "-" },
        {  "abcd*efg",  "abcdefg",      "y",    "&",    "abcdefg" },
        {  "ab*",       "xabyabbbz",    "y",    "&",    "ab" },
        {  "ab*",       "xayabbbz",     "y",    "&",    "a" },
        {  "(ab|cd)e",  "abcde",        "y",    "&-\\1",        "cde-cd" },
        {  "[abhgefdc]ij",      "hij",  "y",    "&",    "hij" },
        {  "^(ab|cd)e", "abcde",        "n",    "x\\1y",        "xy" },
        {  "(abc|)ef",  "abcdef",       "y",    "&-\\1",        "ef-" },
        {  "(a|b)c*d",  "abcd", "y",    "&-\\1",        "bcd-b" },
        {  "(ab|ab*)bc",        "abc",  "y",    "&-\\1",        "abc-a" },
        {  "a([bc]*)c*",        "abc",  "y",    "&-\\1",        "abc-bc" },
        {  "a([bc]*)(c*d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-bc-d" },
        {  "a([bc]+)(c*d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-bc-d" },
        {  "a([bc]*)(c+d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-b-cd" },
        {  "a[bcd]*dcdcde",     "adcdcde",      "y",    "&",    "adcdcde" },
        {  "a[bcd]+dcdcde",     "adcdcde",      "n",    "-",    "-" },
        {  "(ab|a)b*c", "abc",  "y",    "&-\\1",        "abc-ab" },
        {  "((a)(b)c)(d)",      "abcd", "y",    "\\1-\\2-\\3-\\4",      "abc-a-b-d" },
        {  "[a-zA-Z_][a-zA-Z0-9_]*",    "alpha",        "y",    "&",    "alpha" },
        {  "^a(bc+|b[eh])g|.h$",        "abh",  "y",    "&-\\1",        "bh-" },
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
        {  "[k]",                       "ab",   "n",    "-",    "-" },
        {  "[ -~]*",                    "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~]*",         "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~]*",              "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~]*",           "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~]*",        "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~ -~]*",     "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~ -~ -~]*",  "abc",  "y",    "&",    "abc" },*/
        {  "a{2}",      "candy",                "n",    "",     "" },
        {  "a{2}",      "caandy",               "y",    "&",    "aa" },
        {  "a{2}",      "caaandy",              "y",    "&",    "aa" },
        {  "a{2,}",     "candy",                "n",    "",     "" },
        {  "a{2,}",     "caandy",               "y",    "&",    "aa" },
        {  "a{2,}",     "caaaaaandy",           "y",    "&",    "aaaaaa" },
        {  "a{1,3}",    "cndy",                 "n",    "",     "" },
        {  "a{1,3}",    "candy",                "y",    "&",    "a" },
        {  "a{1,3}",    "caandy",               "y",    "&",    "aa" },
        {  "a{1,3}",    "caaaaaandy",           "y",    "&",    "aaa" },
      /*  {  "e?le?",     "angel",                "y",    "&",    "el" },
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
        {"foo.(?=bar)",     "foobar foodbar", "y","&-\\1", "food-" },
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
            catch (Exception e)
            {
                i = 0;
            }

            //writefln("\tcompile() = %d", i);
            assert((c == 'c') ? !i : i);

            if (c != 'c')
            {
                auto m = match(to!(String)(tvd.input), r);
                i = !m.empty;
                //writefln("\ttest() = %d", i);
                //fflush(stdout);
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