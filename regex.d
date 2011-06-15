//Written in the D programming language
/**
 * Fast Regular expressions for D
 *
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * Authors: Dmitry Olshansky
 *
 */
//TODO: kill GC allocations when possible (everywhere)
module fred;

import std.stdio, core.stdc.stdlib, std.array, std.algorithm, std.range,
       std.conv, std.exception, std.ctype, std.traits, std.typetuple,
       std.uni, std.utf, std.format, std.typecons;

/// starting from the low bits [to do: format for doc]
///  bits 0-1
///      00: atom, a normal instruction
///      01: open, opening of a group, has length of contained IR in the low bits 
///      10: close, closing of a group, has length of contained IR in the low bits 
///      11 unused
///
//  Loops with Q (non-greedy, with ? mark) must have the same size / other properties as non Q version
/// open questions:
/// * encode non eagerness (*q) and groups with content (B) differently?
/// * merge equivalent ends?
/// * reorganize groups to make n args easier to find, or simplify the check for groups of similar ops
///   (like lookaround), or make it easier to identify hotspots.
/// * there is still an unused bit that might be used for something
enum IR:uint {
    
    Char              = 0b1_00000_00, /// a character
    Any               = 0b1_00001_00, /// any character
    Digit             = 0b1_00010_00, /// a digit
    Notdigit          = 0b1_00011_00, /// not a digit
    Space             = 0b1_00100_00, /// a space
    Notspace          = 0b1_00101_00, /// not a space
    Word              = 0b1_00110_00, /// a word
    Notword           = 0b1_00111_00, /// not a word
    Bol               = 0b1_01000_00, /// beginning of a string ^
    Eol               = 0b1_01001_00, /// end of a string $ 
    Wordboundary      = 0b1_01010_00, /// boundary of a word
    Notwordboundary   = 0b1_01011_00, /// not a word boundary
    Backref           = 0b1_01100_00, /// backreference to a group (that has to be pinned, i.e. locally unique) (group index)
    GroupStart        = 0b1_01101_00, /// start of a group (x) (groupIndex+groupPinning(1bit))
    GroupEnd          = 0b1_01110_00, /// end of a group (x) (groupIndex+groupPinning(1bit))
    OptionStart       = 0b1_01111_00, /// start of an option within an alternation x | y (length)
    OptionEnd         = 0b1_10000_00, /// end of an option (length of the rest)
    Charset           = 0b1_10001_00, /// a most generic charset [...]

    OrStart           = 0b1_00000_01, /// start of alternation group  (length)
    OrEnd             = 0b1_00000_10, /// end of the or group (length,mergeIndex)
    InfiniteStart     = 0b1_00001_01, /// start of an infinite repetition x* (length)
    InfiniteEnd       = 0b1_00001_10, /// end of infinite repetition x* (length,mergeIndex)
    InfiniteQStart    = 0b1_00010_01, /// start of a non eager infinite repetition x*? (length)
    InfiniteQEnd      = 0b1_00010_10, /// end of non eager infinite repetition x*? (length,mergeIndex)
    RepeatStart       = 0b1_00011_01, /// start of a {n,m} repetition (length)
    RepeatEnd         = 0b1_00011_10, /// end of x{n,m} repetition (length,step,minRep,maxRep)
    RepeatQStart      = 0b1_00100_01, /// start of a non eager x{n,m}? repetition (length)
    RepeatQEnd        = 0b1_00100_10, /// end of non eager x{n,m}? repetition (length,step,minRep,maxRep)
    
   
    
    LookaheadStart    = 0b1_00110_01, /// begin of the lookahead group (length)
    LookaheadEnd      = 0b1_00110_10, /// end of a lookahead group (length)
    NeglookaheadStart = 0b1_00111_01, /// start of a negative lookahead (length)
    NeglookaheadEnd   = 0b1_00111_10, /// end of a negative lookahead (length)
    LookbehindStart   = 0b1_01000_01, /// start of a lookbehind (length)
    LookbehindEnd     = 0b1_01000_10, /// end of a lookbehind (length)
    NeglookbehindStart= 0b1_01001_01, /// start of a negative lookbehind (length)
    NeglookbehindEnd  = 0b1_01001_10, /// end of negative lookbehind (length)
    //TODO: ...
}
/// a shorthand for IR length - full length of specific opcode evaluated at compile time
template IRL(IR code)
{
    enum IRL =  lengthOfIR(code);
}

/// how many parameters follow the IR, should be optimized fixing some IR bits
int immediateParamsIR(IR i){
    switch (i){
    case IR.OrEnd,IR.InfiniteEnd,IR.InfiniteQEnd:
        return 1;
    case IR.RepeatEnd,IR.RepeatQEnd:
        return 3;
    default:
        return 0;
    }
}
/// full length of IR instruction inlcuding all parameters that might follow it
int lengthOfIR(IR i)
{
    return 1 + immediateParamsIR(i);
}
/// full length of the paired IR instruction inlcuding all parameters that might follow it
int lengthOfPairedIR(IR i)
{
    return 1 + immediateParamsIR(pairedIR(i));
}
/// if the operation has a merge point (this relies on the order of the ops)
bool hasMerge(IR i)
{
    return (i&0b11)==0b10 && i<=IR.InfiniteQEnd;
}
/// is an IR that opens a "group"
bool isStartIR(IR i)
{
    return (i&0b11)==0b01;
}
/// is an IR that ends a "group"
bool isEndIR(IR i)
{
    return (i&0b11)==0b10;
}
/// is a standalone IR
bool isAtomIR(IR i)
{
    return (i&0b11)==0b00;
}
/// makes respective pair out of IR i, swapping start/end bits of instruction
IR pairedIR(IR i)
{
    assert(isStartIR(i) || isEndIR(i));
    return cast(IR)(i ^ 0b11);
}

/// encoded IR instruction
struct Bytecode
{
    uint raw;
    this(IR code, uint data)
    { 
        assert(data < (1<<24) && code < 256);
        raw = code<<24 | data;
    }
    static Bytecode fromRaw(uint data)
    { 
        Bytecode t;
        t.raw = data;
        return t;
    }
    ///bit twiddling helpers
    @property uint data(){ return raw & 0x00ff_ffff; }
    ///ditto
    @property IR code(){ return cast(IR)(raw>>24); }
    ///ditto
    @property bool hotspot(){ return hasMerge(code); }
    ///ditto
    @property bool isStart(){ return isStartIR(code); }
    ///ditto
    @property bool isEnd(){ return isStartIR(code); }    
    /// number of arguments
    @property int args(){ return immediateParamsIR(code); }
    /// human readable name of instruction
    @property string mnemonic()
    {
        return to!string(code);
    }
    @property uint length()
    {
        return lengthOfIR(code);
    }
    @property uint pairedLength()
    {
        return lengthOfPairedIR(code);
    }
}

static assert(Bytecode.sizeof == 4);

/// debuging tool, prints out instruction along with opcodes
string disassemble(Bytecode[] irb, uint pc, uint[] index, NamedGroup[] dict=[])
{
    auto output = appender!string();
    formattedWrite(output,"%s", irb[pc].mnemonic);
    switch(irb[pc].code)
    {
    case IR.Char:
        formattedWrite(output, " %s",cast(dchar)irb[pc].data);
        break;
    case IR.RepeatStart, IR.InfiniteStart, IR.OptionStart, IR.OptionEnd, IR.OrStart:
        //forward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc+len+1);
        break;
    case IR.RepeatEnd, IR.RepeatQEnd: //backward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u min=%u max=%u step=%u", 
                pc-len, irb[pc+2].raw, irb[pc+3].raw, irb[pc+1].raw);
        break;
    case IR.InfiniteEnd, IR.InfiniteQEnd: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u %u", pc-len, irb[pc+1].raw);
        break;
    case  IR.LookaheadEnd, IR.NeglookaheadEnd: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc-len);
        break;
    case IR.GroupStart, IR.GroupEnd:
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
    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:    
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc + len + 1);
        break;
    case IR.Backref:
        uint n = irb[pc].data;
        formattedWrite(output, " %u",  n);
        break;
    default://all data-free instructions
    }
    if(irb[pc].hotspot)
        formattedWrite(output, " Hotspot %u", irb[pc+1].raw);
    return output.data;
}

/// another pretty printer, writes out the bytecode of a regex and where the pc is
void prettyPrint(Sink,Char=const(char))(Sink sink,Bytecode[] irb, uint pc=uint.max,int indent=3,size_t index=0)
    if (isOutputRange!(Sink,Char))
{
    while(irb.length>0){
        formattedWrite(sink,"%3d",index);
        if (pc==0 && irb[0].code!=IR.Char){
            for (int i=0;i<indent-2;++i)
                put(sink,"=");
            put(sink,"> ");
        } else {
            if (isEndIR(irb[0].code)){
                indent-=2;
            }
            if (indent>0){
                string spaces="             ";
                put(sink,spaces[0..(indent%spaces.length)]);
                for (size_t i=indent/spaces.length;i>0;--i)
                    put(sink,spaces);
            }
        }
        if (irb[0].code==IR.Char)
        {
            put(sink,`"`);
            int i=0;
            do{
                put(sink,cast(char[])([cast(dchar)irb[i].data]));
                ++i;
            } while(i<irb.length && irb[i].code==IR.Char);
            put(sink,"\"");
            if (pc<i){
                put(sink,"\n");
                for (int ii=indent+pc+1;ii>0;++ii)
                    put(sink,"=");
                put(sink,"^");
            }
            index+=i;
            irb=irb[i..$];
        } else {
            put(sink,irb[0].mnemonic);
            put(sink,"(");
            formattedWrite(sink,"%x",irb[0].data);
            int nArgs= irb[0].args;
            for (int iarg=0;iarg<nArgs;++iarg){
                if (iarg+1<irb.length){
                    formattedWrite(sink,",%x",irb[iarg+1].data);
                } else {
                    put(sink,"*error* incomplete irb stream");
                }
            }
            put(sink,")");
            if (isStartIR(irb[0].code)){
                indent+=2;
            }
            index+=lengthOfIR(irb[0].code);
            irb=irb[lengthOfIR(irb[0].code)..$];
        }
        put(sink,"\n");
    }
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
    string toString()
    {
        auto a = appender!string();
        formattedWrite(a, "%s..%s", begin, end);
        return a.data;
    }
}

struct RecursiveParser(R)
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
    uint counterDepth = 0; 
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
        auto maxCounterDepth = counterDepth;
        while(!empty && current != '|' && current != ')')
        {
            auto saveCounterDepth = counterDepth;
            parseRepetition();
            maxCounterDepth = max(counterDepth, maxCounterDepth);
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
                Bytecode[2] piece = [Bytecode.init, Bytecode(IR.OptionStart, ir.length - start + 1)];
                insertInPlace(ir, start, piece[]);
                put(Bytecode(IR.OptionEnd, 0)); 
                uint anchor = cast(uint)(ir.length); //points to first option
                uint maxSub = 0; //maximum number of captures out of each code path
                do
                {
                    next();
                    uint offset = cast(uint)(ir.length);
                    put(Bytecode.init); //reserve space
                    
                    while(!empty && current != '|' && current != ')')
                    {
                        auto saveCounterDepth = counterDepth;
                        parseRepetition();
                        maxCounterDepth = max(counterDepth, maxCounterDepth);
                        counterDepth = saveCounterDepth;
                    }
                    if(current == '|')      //another option?
                    {
                        put(Bytecode(IR.OptionEnd, 0));//mark now, fixup later   
                    }
                    uint len = cast(uint)(ir.length - offset - 1);
                    len < (1<<24) || error("Internal error - overflow");
                    ir[offset] = Bytecode(IR.OptionStart,  len);
                    maxSub = max(ngroup,maxSub);
                    ngroup = subSave; //reuse groups across alternations
                }while(current == '|');
                ir[start] = Bytecode(IR.OrStart, ir.length - start - 1);
                uint pc = start + 1;
                //fixup OptionEnds so that they point to the last OrEnd
                while(pc < ir.length)
                {
                    pc = pc + ir[pc].data;
                    if(ir[pc].code != IR.OptionEnd)
                        break;
                    ir[pc] = Bytecode(IR.OptionEnd,cast(uint)(ir.length - pc - 1));
                    pc++;
                }
                //end and start use the same length
                put(Bytecode(IR.OrEnd, ir[start].data));
                put(Bytecode.init); //merge point
                ngroup = maxSub;
                if(current == ')') 
                    goto case ')';
                break;
            default:
            }
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
                insertInPlace(ir, offset, Bytecode(IR.RepeatStart, len));
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                putRaw(1); 
                putRaw(min);
                putRaw(max);
                counterDepth++;
            }
        }
        else if(min) // && max is infinite
        {
            if(min != 1)
            {
                insertInPlace(ir, offset, Bytecode(IR.RepeatStart, len));
                offset += 1;//so it still points to the repeated block
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                putRaw(1);
                putRaw(min);
                putRaw(min);
                counterDepth++;
            }
            put(Bytecode(IR.InfiniteStart, len));
            ir ~= ir[offset .. offset+len];
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index
        }
        else//vanila {0,inf}
        {
            insertInPlace(ir, offset, Bytecode(IR.InfiniteStart, len));
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index
            
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
            put(Bytecode(IR.Any, 0));
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
                    op = Bytecode(IR.LookaheadStart, 0);
                    next();
                    lookaround = true;
                    break;
                case '!':
                    op = Bytecode(IR.NeglookaheadStart, 0);
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
                    op = Bytecode(IR.GroupStart, nglob);
                    break;
                case '<':
                    next();
                    if(current == '=')
                        op = Bytecode(IR.LookbehindStart, 0);
                    else if(current == '!')
                        op = Bytecode(IR.NeglookbehindStart, 0);
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
                op = Bytecode(IR.GroupStart, nglob);
            }            
            if(lookaround)
            {
                uint offset = cast(uint)ir.length;
                put(Bytecode.init);
                parseRegex();
                uint sz = cast(uint)(ir.length - offset - 1);
                put(Bytecode(pairedIR(op.code), sz));
                ir[offset] = Bytecode(op.code, sz);
            }
            else
            {
                if(op != Bytecode.init) //currently only groups
                {
                    put(op);
                }
                parseRegex();
            }
            
            if(op.code == IR.GroupStart)
            {
                put(Bytecode(IR.GroupEnd, nglob));
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
            put(Bytecode(IR.Bol, 0));
            next();
            break;
        case '$':
            put(Bytecode(IR.Eol, 0));
            next();
            break;
        default:
            put(Bytecode(IR.Char, current));
            next();
        }
    }

    Bytecode escape()
    {
        switch(current)
        {
        case 'f':   next(); return Bytecode(IR.Char,'\f');
        case 'n':   next(); return Bytecode(IR.Char, '\n');
        case 'r':   next(); return Bytecode(IR.Char, '\r');
        case 't':   next(); return Bytecode(IR.Char, '\t');
        case 'v':   next(); return Bytecode(IR.Char, '\v');
            
        case 'd':   next(); return Bytecode(IR.Digit, 0); 
        case 'D':   next(); return Bytecode(IR.Notdigit, 0); 
        case 'b':   next(); return Bytecode(IR.Wordboundary, 0);
        case 'B':   next(); return Bytecode(IR.Notwordboundary, 0);
        case 's':   next(); return Bytecode(IR.Space, 0);
        case 'S':   next(); return Bytecode(IR.Notspace, 0);
        case 'w':   next(); return Bytecode(IR.Word, 0);
        case 'W':   next(); return Bytecode(IR.Notword, 0);
        case 'x':            
            auto save = pat;
            uint code = 0;
            for(int i=0;i<2;i++)
            {
                if(!next())
                {
                    restart(save);
                    return Bytecode(IR.Char, 'x');
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
                    return Bytecode(IR.Char, 'x');
                }
            }
            next();
            return Bytecode(IR.Char,code);
        case 'u':
            auto save = pat;
            uint code = 0;
            for(int i=0; i<4; i++)
            {
                if(!next())
                {
                    restart(save);
                    return Bytecode(IR.Char, 'u');
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
                    return Bytecode(IR.Char, 'u');
                }
            }
            next();
            return Bytecode(IR.Char, code);
        case 'c': //control codes                      
            next() || error("Unfinished escape sequence");
            ('a' <= current && current <= 'z') || ('A' <= current && current <= 'Z')
                || error("Only letters are allowed after \\c");
            Bytecode code = Bytecode(IR.Char, current &  0x1f);
            next();
            return code;
        case '0':
            next();
            return Bytecode(IR.Char, 0);//NUL character
        case '1': .. case '9':
            uint nref = cast(uint)current - '0'; 
            nref <  index.length || error("Backref to unseen group");
            //perl's disambiguation rule i.e.
            //get next digit only if there is such group number
            while(nref <= index.length && next() && isdigit(current))
            {
                nref = nref * 10 + current - '0';
            }
            if(nref > index.length)
                nref /= 10;
            nref--;
            return Bytecode(IR.Backref, nref);
        default:
            auto op = Bytecode(IR.Char, current);
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
///std.regex-like Regex object wrapper, provided for backwards compatibility
struct Regex(Char)
    if(is(Char : char) || is(Char : wchar) || is(Char : dchar))
{
    Program storage;
    this(Program rs){ storage = rs; }
    alias storage this;

}
///Object that holds all persistent data about compiled regex
struct Program
{
    Bytecode[] ir;
    uint[] index;       //user group number -> internal number
    NamedGroup[] dict;  //maps name -> user group number
    uint ngroup;        //number of internal groups
    uint maxCounterDepth; //max depth of nested {n,m} repetitions
    uint hotspotTableSize; // number of entries in merge table
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
        processHotspots();
    }
    ///lightweight post process step - no GC allocations (TODO!),
    ///only essentials
    void processHotspots()
    {
        uint[] counterRange = new uint[maxCounterDepth+1];
        uint hotspotIndex = 0;
        uint top = 0;
        counterRange[0] = 1;
        
        for(size_t i=0; i<ir.length; i+=ir[i].length)
        {
            if(ir[i].code == IR.RepeatStart)
            {
                uint repEnd = i + ir[i].data + IRL!(IR.RepeatStart);
                assert(ir[repEnd].code == IR.RepeatEnd);
                uint max = ir[repEnd + 3].raw;
                ir[repEnd+1].raw = counterRange[top];
                ir[repEnd+2].raw *= counterRange[top];
                ir[repEnd+3].raw *= counterRange[top];
                counterRange[top+1] = (max+1) * counterRange[top];
                top++;
            }
            else if(ir[i].code == IR.RepeatEnd)
            {
                top--;
            }
            if(ir[i].hotspot)
            {
                ir[i+1].raw = hotspotIndex;
                hotspotIndex += counterRange[top];
            }
        }
        hotspotTableSize = hotspotIndex;
        debug writeln("---\nHotspots & counters fixed, total merge table size: ",hotspotTableSize);
    }
    /// print out disassembly a program's IR
    void print()
    {
        writefln("PC\tINST\n");
        prettyPrint(delegate void(const(char)[] s){ writef(s); },ir);
        writefln("\n");
        for(size_t i=0; i<ir.length; i+=ir[i].length)
        {
            writefln("%d\t%s ", i, disassemble(ir, i, index, dict));
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
    alias const(Char)[] String;
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
    ///lookup next match, fills matches with indices into input
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
            if(matchImpl(re.ir, matches[1..$], mainStack))
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
    /++
        same as three argument version, but creates temporary buffer
    ++/
    bool matchImpl(Bytecode[] prog, Group[] matches)
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
        return matchImpl(prog, matches, mem[0..initialStack]);
    }
    /++
        match subexpression against input, using provided malloca'ed array as stack
        results are stored in matches
    ++/
    bool matchImpl(Bytecode[] prog, Group[] matches, ref uint[] stack)
    {  
        enum headWords = size_t.sizeof/uint.sizeof + 3;//size of a thread state head
        enum groupSize = Group.sizeof/uint.sizeof;
        uint pc, counter;
        uint last;          //top of stack
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
            debug writeln("Backtracked");
            return true;
        }   
        auto start = origin.length - s.length;
        debug writeln("Try match starting at ",origin[inputIndex..$]);
        while(pc<prog.length)
        {
            debug writefln("PC: %s\tCNT: %s\t%s", pc, counter, disassemble(prog, pc, re.index, re.dict));
            switch(prog[pc].code)
            {
            case IR.Char:
                if(s.empty || s.front != prog[pc].data)
                   goto L_backtrack;
                pc += IRL!(IR.Char);
                s.popFront();
            break;
            case IR.Any:
                if(s.empty)
                    goto L_backtrack;
                pc += IRL!(IR.Any);
                s.popFront();
                break;
            case IR.Word:
                if(s.empty || !isUniAlpha(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Word);
                break;
            case IR.Notword:
                if(s.empty || isUniAlpha(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Word);
                break;
            case IR.Digit:
                if(s.empty || !isdigit(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Word);
                break;
            case IR.Notdigit:
                if(s.empty || isdigit(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Notdigit);
                break;
            case IR.Space:
                if(s.empty || !isspace(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Space);
                break;
            case IR.Notspace:
                if(s.empty || isspace(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Notspace);
                break;
            case IR.Wordboundary:
                //at start & end of input
                if((s.empty && isUniAlpha(s.front))
                   || (s.length == origin.length && isUniAlpha(s.front)) )
                    pc += IRL!(IR.Wordboundary);
                else if( (isUniAlpha(s.front) && !isUniAlpha(previous))
                      || (!isUniAlpha(s.front) && isUniAlpha(previous)) )
                    pc += IRL!(IR.Wordboundary);
                else
                    goto L_backtrack;
                break;
            case IR.Notwordboundary:
                if((s.empty && isUniAlpha(s.front))
                   || (s.length == origin.length && isUniAlpha(previous)) )
                    goto L_backtrack;
                else if( (isUniAlpha(s.front) && !isUniAlpha(previous))
                      || (!isUniAlpha(s.front) && isUniAlpha(previous)) )
                    goto L_backtrack;
                else
                    pc += IRL!(IR.Notwordboundary);
                break;
            case IR.Bol:
                //TODO: multiline & attributes, unicode line terminators
                if(s.length == origin.length || previous == '\n')
                    pc += IRL!(IR.Bol);
                else
                    goto L_backtrack;
                break;
            case IR.Eol:
                //TODO: ditto for the end of line
                if(s.empty || s.front == '\n')
                    pc += IRL!(IR.Eol);
                else
                    goto L_backtrack;
                break;
            case IR.InfiniteStart: 
                trackers[infiniteNesting+1] = inputIndex;
                pc += prog[pc].data + IRL!(IR.InfiniteStart);
                //now pc is at end IR.Infininite(Q)End
                uint len = prog[pc].data;
                if(prog[pc].code == IR.InfiniteEnd)
                {
                    pushState(pc + IRL!(IR.InfiniteEnd), counter);
                    infiniteNesting++;
                    pc -= len;
                }
                else
                {
                    pushState(pc - len, counter);
                    pc += IRL!(IR.InfiniteEnd);
                }
                break;
            case IR.RepeatStart:
                pc += prog[pc].data + IRL!(IR.RepeatStart);
                break;
            case IR.RepeatEnd:
            case IR.RepeatQEnd:
                // len, step, min, max
                uint len = prog[pc].data;
                uint step =  prog[pc+1].raw;
                uint min = prog[pc+2].raw;
                uint max = prog[pc+3].raw;
                //debug writefln("repeat pc=%u, counter=%u",pc,counter);

                if(counter < min)
                {
                    counter += step;
                    pc -= len;
                }
                else if(counter < max)
                {
                    if(prog[pc].code == IR.RepeatEnd)
                    {
                        pushState(pc + IRL!(IR.RepeatEnd), counter%step);
                        counter += step;
                        pc -= len;
                    }
                    else
                    {
                        pushState(pc - len, counter + step);   
                        counter = counter%step;
                        pc += IRL!(IR.RepeatEnd);
                    }
                }
                else
                {
                    counter = counter%step;
                    pc += IRL!(IR.RepeatEnd);
                }                       
                break;
            case IR.InfiniteEnd:
            case IR.InfiniteQEnd:
                uint len = prog[pc].data;
                assert(infiniteNesting < trackers.length);
                if(trackers[infiniteNesting] == inputIndex)
                {//source not consumed
                    pc += IRL!(IR.InfiniteEnd);
                    infiniteNesting--;
                    break;
                }
                else
                    trackers[infiniteNesting] = inputIndex;
                
                if(prog[pc].code == IR.InfiniteEnd)
                {
                    infiniteNesting--;
                    pushState(pc + IRL!(IR.InfiniteEnd), counter);
                    infiniteNesting++;
                    pc -= len;
                }
                else
                {
                    pushState(pc-len, counter);
                    pc += IRL!(IR.InfiniteEnd);    
                    infiniteNesting--;
                }
                break;
            case IR.OrEnd:
                pc += IRL!(IR.OrEnd);
                break;
            case IR.OrStart:
                pc += IRL!(IR.OrStart);
                goto case;
            case IR.OptionStart:
                uint len = prog[pc].data;
                if(prog[pc+len].code == IR.OptionEnd)//not a last one
                {
                   pushState(pc + len + IRL!(IR.OptionStart), counter); //remember 2nd branch
                }
                pc += IRL!(IR.OptionStart);
                break;
            case IR.OptionEnd:
                pc = pc + prog[pc].data + IRL!(IR.OptionEnd);
                break;
            case IR.GroupStart: //TODO: mark which global matched and do the other alternatives
                uint n = prog[pc].data;
                matches[re.index[n]].begin = inputIndex;
                debug  writefln("IR group #%u starts at %u", n, inputIndex);
                pc += IRL!(IR.GroupStart);
                break;
            case IR.GroupEnd:   //TODO: ditto
                uint n = prog[pc].data;
                matches[re.index[n]].end = inputIndex;
                debug writefln("IR group #%u ends at %u", n, inputIndex);
                pc += IRL!(IR.GroupEnd);
                break;
            case IR.LookaheadStart:
            case IR.NeglookaheadStart: 
                uint len = prog[pc].data;
                auto save = inputIndex;
                uint matched = matchImpl(prog[pc+1 .. pc+1+len], matches);
                s = origin[save .. $];
                if(matched ^ (prog[pc].code == IR.LookaheadStart))
                    goto L_backtrack;
                pc += IRL!(IR.LookaheadStart) + IRL!(IR.LookaheadEnd) + len;
                break;
            case IR.LookbehindStart:
                assert(0, "No impl!");
            case IR.NeglookbehindEnd:
                assert(0, "No impl!");
            case IR.Backref:
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
            case IR.LookaheadEnd:
            case IR.NeglookaheadEnd:
                return true;
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
/++
   Thomspon matcher does all matching in lockstep, never looking at the same char twice 
+/
struct ThompsonMatcher(Char)
    if(is(Char : dchar))
{    
    alias const(Char)[] String;
    ///State of VM thread
    struct Thread
    {
        Group[] matches;    
        Thread* next;    //intrusive linked list
        uint pc;         
        uint counter;    // loop counter
        uint uopCounter; // counts micro operations inside one macro instruction (e.g. BackRef)
    }
    ///head-tail singly-linked list (optionally with cached length)
    struct ThreadList
    {
        Thread* tip=null, toe=null;
        uint length;
        //add new thread to the end of list
        void insert(Thread* t)
        {
            if(toe)
            {
                toe.next = t;
                toe = t;
            }
            else
                tip = toe = t;
            toe.next = null;
            length++;
        }
        //null on end
        Thread* fetch()
        {
            auto t = tip;
            if(tip == toe)
                tip = toe = null;
            else
                tip = tip.next;
            return t;
        }
        /+ dropped for now
        /// input range troika
        Thread* front()
        {
            return tip;
        }
        ///ditto
        void popFront()
        {
            assert(!empty);
            if(tip == toe)
                tip = toe = null;
            else
                tip = tip.next;
            length --;
        }
        ///ditto
        @property bool empty()
        {
            return tip == null;
        }+/
    }
    enum threadAllocSize = 16;
    Thread* freelist;
    ThreadList clist, nlist;
    uint[] merge;
    Program re;           //regex program
    String origin;
    size_t genCounter;    //merge trace counter, goes up on every dchar
    this(Program prog, String input)
    {
        re = prog;
        origin = input;
        if(re.hotspotTableSize)
        {
            merge = new uint[re.hotspotTableSize];
        }
        genCounter = 1;
    }
    /++
        run threads to exhaustion at the end of input  
    +/
    bool matchEnd(Group[] matches)
    {
        debug writeln("TRY match the END");
        Bytecode[] prog = re.ir;
        for(Thread* t = clist.fetch(); t; t = clist.fetch())
        {
            if(t.pc == prog.length)
            {
                matches[] = t.matches[];
                matches[0].end = origin.length;//end of the whole match
                debug writefln("FOUND AT END: %s..%s", matches[0].begin, matches[0].end);
                return true;
            }
            switch(prog[t.pc].code)
            {
            case IR.Wordboundary:
                assert(0, "No impl yet");
                break;
            case IR.Notwordboundary:
                assert(0, "No impl yet");
                break;
            case IR.Bol:
                //TODO: multiline & attributes, unicode line terminators
                if(origin.empty)
                {
                    t.pc += IRL!(IR.Bol);
                    clist.insert(t);
                }
                break;
            case IR.Eol:
                //TODO: ditto for the end of line
                t.pc += IRL!(IR.Eol);
                clist.insert(t);
                break;
            case IR.InfiniteStart: 
                t.pc += prog[t.pc].data + IRL!(IR.InfiniteStart);
                goto case IR.InfiniteEnd; // both Q and non-Q
                break;
            case IR.RepeatStart:
                t.pc += prog[t.pc].data + IRL!(IR.RepeatStart);
                goto case IR.RepeatEnd; // both Q and non-Q
            case IR.RepeatEnd:
            case IR.RepeatQEnd:
                // len, step, min, max
                uint len = prog[t.pc].data;
                uint step =  prog[t.pc+1].raw;
                uint min = prog[t.pc+2].raw;
                if(t.counter < min)
                {
                    t.counter += step;
                    t.pc -= len;
                    clist.insert(t);
                    break;
                }
                uint max = prog[t.pc+3].raw;
                if(t.counter < max)
                {
                        
                    if(prog[t.pc].code == IR.RepeatEnd)
                    {
                        clist.insert(fork(t, t.pc  - len,  t.counter + step));
                        t.counter %= step;
                        t.pc += IRL!(IR.RepeatEnd);
                        //inserted later == lesser priority
                        clist.insert(t);
                    }
                    else
                    {
                        clist.insert(fork(t, t.pc + IRL!(IR.RepeatQEnd),  t.counter % step));
                        t.counter += step;
                        t.pc -= len;
                        //inserted later == lesser priority
                        clist.insert(t);
                    }
                    break;
                }
                t.counter %= step;
                t.pc += IRL!(IR.RepeatEnd);                       
                clist.insert(t);
                break;
            case IR.InfiniteEnd:
            case IR.InfiniteQEnd:
                // merge point
                uint idx = prog[t.pc + 1].raw + t.counter;
                if(merge[idx] < genCounter)
                    merge[idx] = genCounter;
                else
                {
                    recycle(t);
                    break;
                }
                uint len = prog[t.pc].data;
                if(prog[t.pc].code == IR.InfiniteEnd)
                {
                    clist.insert(fork(t, t.pc - len, t.counter));
                    t.pc += IRL!(IR.InfiniteEnd);
                    //inserted later == lesser priority
                    clist.insert(t);
                }
                else
                {
                    clist.insert(fork(t, t.pc + IRL!(IR.InfiniteQEnd), t.counter));
                    t.pc -= len;
                    //inserted later == lesser priority
                    clist.insert(t);
                }
                break;
            case IR.OrEnd:
                // merge point
                uint idx = prog[t.pc + 1].raw + t.counter;
                if(merge[idx] < genCounter)
                    merge[idx] = genCounter;
                else
                {
                    recycle(t);
                    break;
                }
                t.pc += IRL!(IR.OrEnd);
                clist.insert(t);
                break;
            case IR.OrStart:
                // add a thread for each option in turn
                uint pc = t.pc + IRL!(IR.OrStart);
                while(prog[pc].code == IR.OptionStart)
                {
                    uint len = prog[pc].data;
                    clist.insert(fork(t, pc + IRL!(IR.OptionStart), t.counter));
                    pc += len + IRL!(IR.OptionStart);
                }
                recycle(t);
                break;
            case IR.OptionStart:
                assert(0, "Faulty OrStart in Thompson VM");
            case IR.OptionEnd:
                t.pc = t.pc + prog[t.pc].data + IRL!(IR.OptionEnd);
                clist.insert(t);
                break;
            case IR.GroupStart: //TODO: mark which global matched and do the other alternatives
                uint n = prog[t.pc].data;
                t.matches[re.index[n]+1].begin = origin.length;
                t.pc += IRL!(IR.GroupStart);
                clist.insert(t);
                break;
            case IR.GroupEnd:   //TODO: ditto
                uint n = prog[t.pc].data;
                t.matches[re.index[n]+1].end = origin.length;
                t.pc += IRL!(IR.GroupEnd);
                clist.insert(t);
                break;
            case IR.LookaheadStart:
            case IR.NeglookaheadStart: 
            case IR.LookbehindStart:
            case IR.NeglookbehindStart:
            case IR.LookaheadEnd:
            case IR.NeglookaheadEnd:
            case IR.LookbehindEnd:
            case IR.NeglookbehindEnd:
                assert(0, "No lookaround for ThompsonVM yet!");
            default:
                recycle(t);
            }
        }
        return false;
    }
    /++
        the usual match the input and fill matches
    +/
    bool match(Group[] matches)
    {
        bool matched = false;
        debug
        {
            writeln("------------------------------------------");
            re.print();
        }
        Bytecode[] prog = re.ir;
        debug writeln("Threaded matching started");
        if(origin.empty)
        {
            clist.insert(createStart(0));
            return matchEnd(matches);
        }
        foreach(i, dchar ch; origin)
        {
            if(!matched)//if we already have match no need to gouge the engine
                clist.insert(createStart(i));//initiate a new thread staring a this position
            writefln("Threaded matching index %s",i);
            Thread* t = clist.tip;
            while(t)
            {
                assert(t);
                writef("pc=%s ",t.pc);
                write(t.matches);
                writeln();
                t = t.next;
            }
        L_threadLoop:
            for(t = clist.fetch(); t; t = clist.fetch())
            {
                if(t.pc == prog.length)
                {
                    writeln(t.matches);
                    matches[] = t.matches[];
                    matches[0].end = i;//end of the whole match
                    debug writefln("FOUND pc=%s prog_len=%s: %s..%s", 
                                   t.pc, prog.length,matches[0].begin, matches[0].end);
                    matched = true;
                    //TODO: recylce the whole clist 
                    clist = ThreadList.init;//cut off low priority threads
                    break;
                }
                switch(prog[t.pc].code)
                {
                case IR.Char:
                    if(ch == prog[t.pc].data)
                    {   
                        t.pc += IRL!(IR.Char);
                        nlist.insert(t);
                    }
                    else
                        recycle(t);
                    break;
                case IR.Any:
                    t.pc += IRL!(IR.Any);
                    nlist.insert(t);
                    break;
                case IR.Word:
                    if(isUniAlpha(ch))
                    {
                        t.pc += IRL!(IR.Word);
                        nlist.insert(t);
                    }
                    else
                        recycle(t);
                    break;
                case IR.Notword:
                    if(!isUniAlpha(ch))
                    {
                        t.pc += IRL!(IR.Notword);
                        nlist.insert(t);
                    }
                    else
                        recycle(t);
                    break;
                case IR.Digit:
                    if(isdigit(ch))
                    {
                        t.pc += IRL!(IR.Digit);
                        nlist.insert(t);
                    }
                    else
                        recycle(t);
                    break;
                case IR.Notdigit:
                    if(!isdigit(ch))
                    {
                        t.pc += IRL!(IR.Notdigit);
                        nlist.insert(t);
                    }
                    else
                        recycle(t);
                    break;
                case IR.Space:
                    if(isspace(ch))
                    {
                        t.pc += IRL!(IR.Space);
                        nlist.insert(t);
                    }
                    else
                        recycle(t);
                    break;
                case IR.Notspace:
                    if(!isspace(ch))
                    {
                        t.pc += IRL!(IR.Space);
                        nlist.insert(t);
                    }
                    else
                        recycle(t);
                    break;
                case IR.Wordboundary:
                    assert(0, "No impl yet");
                    break;
                case IR.Notwordboundary:
                    assert(0, "No impl yet");
                    break;
                case IR.Bol:
                    //TODO: multiline & attributes, unicode line terminators
                    if(i == 0 || origin[0..i].back == '\n')
                    {
                        t.pc += IRL!(IR.Bol);
                        clist.insert(t);
                    }
                    break;
                case IR.Eol:
                    //TODO: ditto for the end of line
                    writeln("EOL", std.utf.stride(origin,i) + i ," vs ", origin.length);
                    if(ch == '\n')
                    {
                        t.pc += IRL!(IR.Eol);
                        clist.insert(t);
                    }
                    break;
                case IR.InfiniteStart: 
                    t.pc += prog[t.pc].data + IRL!(IR.InfiniteStart);
                    goto case IR.InfiniteEnd; // both Q and non-Q
                    break;
                case IR.RepeatStart:
                    t.pc += prog[t.pc].data + IRL!(IR.RepeatStart);
                    goto case IR.RepeatEnd; // both Q and non-Q
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    // len, step, min, max
                    uint len = prog[t.pc].data;
                    uint step =  prog[t.pc+1].raw;
                    uint min = prog[t.pc+2].raw;
                    if(t.counter < min)
                    {
                        t.counter += step;
                        t.pc -= len;
                        clist.insert(t);
                        break;
                    }
                    uint max = prog[t.pc+3].raw;
                    if(t.counter < max)
                    {
                        
                        if(prog[t.pc].code == IR.RepeatEnd)
                        {
                            clist.insert(fork(t, t.pc  - len,  t.counter + step));
                            t.counter %= step;
                            t.pc += IRL!(IR.RepeatEnd);
                            //inserted later == lesser priority
                            clist.insert(t);
                        }
                        else
                        {
                            clist.insert(fork(t, t.pc + IRL!(IR.RepeatQEnd),  t.counter % step));
                            t.counter += step;
                            t.pc -= len;
                            //inserted later == lesser priority
                            clist.insert(t);
                        }
                        break;
                    }
                    t.counter %= step;
                    t.pc += IRL!(IR.RepeatEnd);                       
                    clist.insert(t);
                    break;
                case IR.InfiniteEnd:
                case IR.InfiniteQEnd:
                    // merge point
                    if(merge[prog[t.pc + 1].raw] < genCounter)
                        merge[prog[t.pc + 1].raw] = genCounter;
                    else
                    {
                        debug writeln("A thread(pc=%u) got merged there: ", origin[i..$]);
                        recycle(t);
                        continue L_threadLoop;
                    }
                    uint len = prog[t.pc].data;
                    if(prog[t.pc].code == IR.InfiniteEnd)
                    {
                        clist.insert(fork(t, t.pc - len, t.counter));
                        t.pc += IRL!(IR.InfiniteEnd);
                        //inserted later == lesser priority
                        clist.insert(t);
                    }
                    else
                    {
                        clist.insert(fork(t, t.pc + IRL!(IR.InfiniteQEnd), t.counter));
                        t.pc -= len;
                        //inserted later == lesser priority
                        clist.insert(t);
                    }
                    break;
                case IR.OrEnd:
                    // merge point
                    if(merge[prog[t.pc + 1].raw] < genCounter)
                        merge[prog[t.pc + 1].raw] = genCounter;
                    else
                    {
                        debug writeln("A thread(pc=%u) got merged there: ", origin[i..$]);
                        recycle(t);
                        continue L_threadLoop;
                    }
                    t.pc += IRL!(IR.OrEnd);
                    clist.insert(t);
                    break;
                case IR.OrStart:
                    // add a thread for each option in turn
                    uint pc = t.pc + IRL!(IR.OrStart);
                    while(prog[pc].code == IR.OptionStart)
                    {
                       uint len = prog[pc].data;
                       clist.insert(fork(t, pc + IRL!(IR.OptionStart), t.counter));
                       pc += len + IRL!(IR.OptionStart);
                    }
                    recycle(t);//TODO: can omit this step and fork less
                    break;
                case IR.OptionStart:
                    assert(0, "Faulty OrStart in Thompson VM");
                case IR.OptionEnd:
                    t.pc = t.pc + prog[t.pc].data + IRL!(IR.OptionEnd);
                    clist.insert(t);
                    break;
                case IR.GroupStart: //TODO: mark which global matched and do the other alternatives
                    uint n = prog[t.pc].data;
                    t.matches[re.index[n]+1].begin = i;
                    t.pc += IRL!(IR.GroupStart);
                    //debug  writefln("IR group #%u starts at %u", n, i);
                    clist.insert(t);
                    break;
                case IR.GroupEnd:   //TODO: ditto
                    uint n = prog[t.pc].data;
                    t.matches[re.index[n]+1].end = i;
                    t.pc += IRL!(IR.GroupEnd);
                    //debug writefln("IR group #%u ends at %u", n, i);
                    clist.insert(t);
                    break;
                case IR.LookaheadStart:
                case IR.NeglookaheadStart: 
                case IR.LookbehindStart:
                case IR.NeglookbehindStart:
                case IR.LookaheadEnd:
                case IR.NeglookaheadEnd:
                case IR.LookbehindEnd:
                case IR.NeglookbehindEnd:
                    assert(0, "No lookaround for ThompsonVM yet!");
                case IR.Backref:
                    assert(0, "No backref for ThompsonVM yet!");
                default:
                    assert(0);   
                }
            }            
            clist =  nlist;
            nlist = ThreadList.init;
            genCounter++;
            writefln("Now list has %d threads", clist.length);
        }
        return matchEnd(matches) || matched;//TODO: here happens partial match
    }

    ///get a dirty recycled Thread 
    Thread* allocate()
    {
        if(freelist)
        {
            Thread* t = freelist;
            freelist = freelist.next;
            return t;
        }
        else
        { 
            Thread[] block = new Thread[threadAllocSize];
            freelist = &block[0];
            for(size_t i=1; i<block.length; i++)
                block[i-1].next = &block[i];
            block[$-1].next = null;
            debug writefln("Allocated space for another %d threads", threadAllocSize);
            return allocate();
        }
    }
    ///dispose a thread
    void recycle(Thread* t)
    {
        t.next = freelist;
        freelist = t;
    }
    ///creates a copy of master thread with given pc
    Thread* fork(Thread* master, uint pc, size_t counter)
    {
        auto t = allocate();
        t.matches = master.matches.dup; //TODO: Small array optimization and/or COW
        t.pc = pc;
        t.counter = counter;
        t.uopCounter = 0;
        return t;
    }
    ///creates a start thread 
    Thread*  createStart(size_t index)
    {
        auto t = allocate();
        t.matches = new Group[re.ngroup]; //TODO: ditto
        t.matches[0].begin = index;
        t.pc = 0;
        t.counter = 0;
        t.uopCounter = 0;
        return t;
    }
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
    auto parser = RecursiveParser!(typeof(pattern))(pattern);
    Regex!(Unqual!(typeof(S.init[0]))) r = parser.program;
    return r;
}

auto match(R,C)(R input, Regex!C re)
{
    return RegexMatch!(Unqual!(typeof(input)))(re, input);
}

auto tmatch(R,C)(R input, Regex!C re)
{
    return RegexMatch!(Unqual!(typeof(input)),ThompsonMatcher)(re, input);
}
/// Exception object thrown in case of any errors during regex compilation
class RegexException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}
