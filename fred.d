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


import fred_uni;//unicode property tables
import std.stdio, core.stdc.stdlib, std.array, std.algorithm, std.range,
       std.conv, std.exception, std.ctype, std.traits, std.typetuple,
       std.uni, std.utf, std.format, std.typecons, std.bitmanip, std.functional, std.exception;

//uncomment to get a barrage of debug info
//debug = fred_parser;
//debug = fred_matching;

/// [TODO: format for doc]
///  IR bit pattern: 0b1_xxxxx_yy
///  where yy indicates class of instruction, xxxxx for actual operation code
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
    Option            = 0b1_01111_00, /// start of an option within an alternation x | y (length)
    GotoEndOr         = 0b1_10000_00, /// end of an option (length of the rest)
    Charset           = 0b1_10001_00, /// a most generic charset [...]
    Nop               = 0b1_10010_00, /// no operation (padding)
    OrChar            = 0b1_10011_00, /// match with any of a consecutive OrChar's inthis sequence (used for case insensitive match)
    //OrChar holds in upper two bits of data total number of OrChars in this _sequence_

    OrStart           = 0b1_00000_01, /// start of alternation group  (length)
    OrEnd             = 0b1_00000_10, /// end of the or group (length,mergeIndex)
    //with this instruction order
    //bit mask 0b1_00001_00 could be used to test/set greediness
    InfiniteStart     = 0b1_00001_01, /// start of an infinite repetition x* (length)
    InfiniteEnd       = 0b1_00001_10, /// end of infinite repetition x* (length,mergeIndex)
    InfiniteQStart    = 0b1_00010_01, /// start of a non eager infinite repetition x*? (length)
    InfiniteQEnd      = 0b1_00010_10, /// end of non eager infinite repetition x*? (length,mergeIndex)
    RepeatStart       = 0b1_00011_01, /// start of a {n,m} repetition (length)
    RepeatEnd         = 0b1_00011_10, /// end of x{n,m} repetition (length,step,minRep,maxRep)
    RepeatQStart      = 0b1_00100_01, /// start of a non eager x{n,m}? repetition (length)
    RepeatQEnd        = 0b1_00100_10, /// end of non eager x{n,m}? repetition (length,step,minRep,maxRep)
    //
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
    this(IR code, uint data, uint seq)
    {
        assert(data < (1<<22) && code < 256 );
        assert(seq < 4 && seq >= 1);
        raw = code<<24 | ((seq-1)<<22) | data;
    }
    static Bytecode fromRaw(uint data)
    {
        Bytecode t;
        t.raw = data;
        return t;
    }
    ///bit twiddling helpers
    @property uint data(){ return raw & 0x003f_ffff; }
    ///ditto
    @property uint sequence(){ return 1 + (raw >>22) & 0x3; }
    ///ditto
    @property IR code(){ return cast(IR)(raw>>24); }
    ///ditto
    @property bool hotspot(){ return hasMerge(code); }
    ///test the class of this instruction
    @property bool isAtom(){ return isAtomIR(code); }
    ///ditto
    @property bool isStart(){ return isStartIR(code); }
    ///ditto
    @property bool isEnd(){ return isEndIR(code); }
    /// number of arguments
    @property int args(){ return immediateParamsIR(code); }
    /// human readable name of instruction
    @property string mnemonic()
    {
        return to!string(code);
    }
    /// full length of instruction
    @property uint length()
    {
        return lengthOfIR(code);
    }
    /// full length of respective start/end of this instruction
    @property uint pairedLength()
    {
        return lengthOfPairedIR(code);
    }
    ///returns bytecode of paired instruction (assuming this one is start or end)
    @property Bytecode paired()
    {//depends on bit and struct layout order
        assert(isStart || isEnd);
        return Bytecode.fromRaw(raw ^ (0b11<<24));
    }
    /// gets an index into IR block of the respective pair
    uint indexOfPair(uint pc)
    {
        assert(isStart || isEnd);
        return isStart ? pc + data + length  : pc - data - lengthOfPairedIR(code);
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
    case IR.RepeatStart, IR.InfiniteStart, IR.Option, IR.GotoEndOr, IR.OrStart:
        //forward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc+len+1);
        break;
    case IR.RepeatEnd, IR.RepeatQEnd: //backward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u min=%u max=%u step=%u",
                pc-len, irb[pc+2].raw, irb[pc+3].raw, irb[pc+1].raw);
        break;
    case IR.InfiniteEnd, IR.InfiniteQEnd, IR.OrEnd: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc-len);
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
    case IR.Backref: case IR.Charset:
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
            formattedWrite(sink,"%d",irb[0].data);
            int nArgs= irb[0].args;
            for (int iarg=0;iarg<nArgs;++iarg){
                if (iarg+1<irb.length){
                    formattedWrite(sink,",%d",irb[iarg+1].data);
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
//do not reorder this list
///Regular expression engine/parser options:
/// global - search  nonoverlapping matches in input
/// casefold - case insensitive matching, do casefolding on match in unicode mode
/// freeform - ignore whitespace in pattern, to match space use [ ] or \s
enum RegexOption: uint { global = 0x1, casefold = 0x2, freeform = 0x4, nonunicode = 0x8,  };

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
    string toString() const
    {
        auto a = appender!string();
        formattedWrite(a, "%s..%s", begin, end);
        return a.data;
    }
}

/// structure representing interval: [a,b]
struct Interval
{
    ///
    uint begin, end;
    ///
    this(uint x)
    {
        begin = x;
        end = x;
    }
    ///
    this(uint x, uint y)
    {
        assert(x <= y);
        begin = x;
        end = y;
    }
    ///
    string toString()const
    {
        auto s = appender!string;
        formattedWrite(s,"%s(%s)..%s(%s)",
                       begin, isgraph(begin) ? to!string(cast(dchar)begin) : "",
                       end, isgraph(end) ? to!string(cast(dchar)end) : "");
        return s.data;
    }
}
/// basic internal data structure for [...] sets
struct Charset
{
    Interval[] intervals;
    ///
    void add(Interval inter)
    {
        //TODO: This all could use an improvment
        /*
        auto beg = assumeSorted(map!"a.end"(intervals)).lowerBound(inter.begin).length;
        auto end = assumeSorted(map!"a.begin"(intervals)).lowerBound(inter.end).length;
        */
        uint beg, end;
        for(beg=0; beg<intervals.length;beg++)
            if(intervals[beg].end >= inter.begin)
                break;
        for(end=0; end<intervals.length;end++)
            if(intervals[end].begin >= inter.end)
                break;
        //debug(fred_parser) writeln("Found ",inter," beg:", beg, "  end:", end);
        if(beg == intervals.length)
        {
            intervals ~= inter;
        }
        else
        {
            if(inter.begin > intervals[beg].begin)
                inter.begin = intervals[beg].begin;
            if(end && inter.end < intervals[end-1].end)
                inter.end = intervals[end-1].end;
            replaceInPlace(intervals, beg, end, [inter]);
        }

        if(beg > 0 && intervals[beg].begin == intervals[beg-1].end+1)
        {
            intervals[beg-1].end = intervals[beg].end;
            replaceInPlace(intervals, beg, beg+1, cast(Interval[])[]);
        }
        if(end > 0 && end < intervals.length && intervals[end-1].end+1 == intervals[end].begin)
        {
            intervals[end-1].end = intervals[end].begin;
            replaceInPlace(intervals, end, end+1, cast(Interval[])[]);
        }
        //debug(fred_parser) writeln("Charset after add: ", intervals);
    }
    ///
    void add(dchar ch){ add(Interval(cast(uint)ch)); }
    /// this = this || set
    void add(in Charset set)
    {
        debug(fred_charset) writef ("%s || %s --> ", intervals, set.intervals);
        foreach(inter; set.intervals)
            add(inter);
        debug(fred_charset) writeln(intervals);
    }
    /// this = this || set
    void sub(in Charset set)
    {
        Interval[] result;
        auto a = intervals;
        const(Interval)[] b = set.intervals;
        for(;;)
        {
            if(a.front.end < b.front.begin)
            {
                result ~= a.front;
                a.popFront();
                if(a.empty)
                    break;
            }
            else if(a.front.begin > b.front.end)
            {
                b.popFront();
                if(b.empty)
                    break;
            }
            else //there is an intersection
            {
                if(a.front.begin < b.front.begin)
                {
                    result ~= Interval(a.front.begin, b.front.begin-1);
                    if(a.front.end < b.front.end)
                    {
                        a.popFront();
                        if(a.empty)
                            break;
                    }
                    else if(a.front.end > b.front.end)
                    {
                        //adjust a in place
                        a.front.begin = b.front.end+1;
                        b.popFront();
                        if(b.empty)
                            break;
                    }
                    else //==
                    {
                        a.popFront();
                        b.popFront();
                        if(a.empty || b.empty)
                            break;
                    }
                }
                else //a.front.begin > b.front.begin
                {//adjust in place
                    if(a.front.end < b.front.end)
                    {
                        a.popFront();
                        if(a.empty)
                            break;
                    }
                    else
                    {
                        a.front.begin = b.front.end+1;
                        b.popFront();
                        if(b.empty)
                            break;
                    }
                }

            }
        }
        intervals = result ~ a;//+ leftover of original (if any)
    }
    /// this = this ~~ set (i.e. (this || set) -- (this && set))
    void symmetricSub(in Charset set)
    {
        auto a = Charset(intervals.dup);
        a.intersect(set);
        this.add(set);
        this.sub(a);
    }
    /// this = this && set
    void intersect(in Charset set)
    {
        Interval[] intersection;
        auto a = intervals;
        const(Interval)[] b = set.intervals;
        for(;;)
        {
            if(a.front.end < b.front.begin)
            {
                a.popFront();
                if(a.empty)
                    break;
            }
            else if(a.front.begin > b.front.end)
            {
                b.popFront();
                if(b.empty)
                    break;
            }
            else //there is an intersection
            {
                if(a.front.end < b.front.end)
                {
                    intersection ~= Interval(max(a.front.begin, b.front.begin), a.front.end);
                    a.popFront();
                    if(a.empty)
                        break;
                }
                else if(a.front.end > b.front.end)
                {
                    intersection ~= Interval(max(a.front.begin, b.front.begin), b.front.end);
                    b.popFront();
                    if(b.empty)
                        break;
                }
                else //==
                {
                    intersection ~= Interval(max(a.front.begin, b.front.begin), a.front.end);
                    a.popFront();
                    b.popFront();
                    if(a.empty || b.empty)
                        break;
                }
            }
        }
        intervals = intersection;
    }
    /// this = !this (i.e. [^...] in regex syntax)
    void negate()
    {
        if(intervals.empty)
        {
            intervals ~= Interval(0, 0x10FFFF);
            return;
        }
        Interval[] negated;
        if(intervals[0].begin != 0)
            negated ~= Interval(0, intervals[0].begin-1);
        for(size_t i=0; i<intervals.length-1; i++)
            negated ~= Interval(intervals[i].end+1, intervals[i+1].begin-1);
        if(intervals[$-1].end != 0x10FFFF)
            negated ~= Interval(intervals[$-1].end+1, 0x10FFFF);
        intervals = negated;
    }
    /// test if ch is present in this set
    bool contains(dchar ch) const
    {
        //debug(fred_charset) writeln(intervals);
        for(uint i=0; i<intervals.length; i++)
            if(ch >= intervals[i].begin && ch <= intervals[i].end)
                return true;
        return false;
        //debug(fred_charset) writeln("Test at ", fnd);
    }
    /// true if set is empty
    @property bool empty() const {   return intervals.empty; }
}

/// basic stack, just in case it gets used anywhere else then Parser
struct Stack(T)//TODO: redo with TmpAlloc
{
    Appender!(T[]) stack;
    @property bool empty(){ return stack.data.empty; }
    void push(T item)
    {
        stack.put(item);
    }
    @property ref T top()
    {
        assert(!empty);
        return stack.data[$-1];
    }
    @property void top(T val)
    {
        assert(!empty);
        stack.data[$-1] = val;
    }
    @property size_t length(){  return stack.data.length; }
    T pop()
    {
        assert(!empty);
        auto t = stack.data[$-1];
        stack.shrinkTo(stack.data.length-1);
        return t;
    }
}
struct Parser(R)
if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;       //keep full pattern for pretty printing error messages
    Bytecode[] ir;       //resulting bytecode
    uint[] index;        //user group number -> internal number
    uint re_flags = 0;   //global flags e.g. multiline + internal ones
    Stack!uint fixupStack;  //stack of opened start instructions
    NamedGroup[] dict;   //maps name -> user group number
    //current num of group, group nesting level and repetitions step
    uint ngroup = 1, nesting = 0;
    uint counterDepth = 0;
    immutable(Charset)[] charsets;
    this(R pattern, R flags)
    {
        pat = origin = pattern;
        index = [ 0 ]; //map first to start-end of the whole match
        ir.reserve(pat.length);
        next();
        parseFlags(flags);
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
    /**

    */
    void parseFlags(R flags)
    {
        foreach(dchar ch; flags)
            switch(ch)
            {
                alias TypeTuple!('g', 'i', 'x', 'U') switches;
                foreach(i, op; __traits(allMembers, RegexOption))
                {
                    case switches[i]:
                            if(re_flags & mixin("RegexOption."~op))
                                throw new RegexException(text("redundant flag specified: ",ch));
                            re_flags |= mixin("RegexOption."~op);
                            break;
                }
                default:
                    new RegexException(text("unknown regex flag '",ch,"'"));
            }
    }
    /**
        Parse and store IR for regex pattern
    */
    void parseRegex()
    {
        fixupStack.push(0);
        auto subSave = ngroup;
        auto maxCounterDepth = counterDepth;
        uint fix;//fixup pointer
        void finishAlternation()
        {
            ir[fix].code == IR.Option || error("LR syntax error");
            ir[fix] = Bytecode(ir[fix].code, ir.length - fix - IRL!(IR.OrStart));
            fix = fixupStack.pop();
            ir[fix].code == IR.OrStart || error("LR syntax error");
            ir[fix] = Bytecode(IR.OrStart, ir.length - fix - IRL!(IR.OrStart));
            put(Bytecode(IR.OrEnd, ir.length - fix - IRL!(IR.OrStart)));
            uint pc = fix + IRL!(IR.OrStart);
            while(ir[pc].code == IR.Option)
            {
                pc = pc + ir[pc].data;
                if(ir[pc].code != IR.GotoEndOr)
                    break;
                ir[pc] = Bytecode(IR.GotoEndOr,cast(uint)(ir.length - pc - IRL!(IR.OrEnd)));
                pc += IRL!(IR.GotoEndOr);
            }
            put(Bytecode.fromRaw(0));
        }
        while(!empty)
        {
            debug(fred_parser) writeln("*LR*\nSource: ", pat, "\nStack: ",fixupStack.stack.data);

            switch(current)
            {
            case '(':
                next();
                nesting++;
                uint nglob;
                fixupStack.push(cast(uint)ir.length);
                if(current == '?')
                {
                    next();
                    switch(current)
                    {
                    case ':':
                        next();
                        break;
                    case '=':
                        put(Bytecode(IR.LookaheadStart, 0));
                        next();
                        break;
                    case '!':
                        put(Bytecode(IR.NeglookaheadStart, 0));
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
                        nglob = cast(uint)index.length-1;//not counting whole match
                        index ~= ngroup++;
                        auto t = NamedGroup(name,nglob+1);
                        auto d = assumeSorted!"a.name < b.name"(dict);
                        auto ind = d.lowerBound(t).length;
                        insertInPlace(dict, ind, t);
                        put(Bytecode(IR.GroupStart, nglob));
                        break;
                    case '<':
                        next();
                        if(current == '=')
                            put(Bytecode(IR.LookbehindStart, 0));
                        else if(current == '!')
                            put(Bytecode(IR.NeglookbehindStart, 0));
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
                    nglob = cast(uint)index.length-1;//not counting whole match
                    index ~= ngroup++; //put local index
                    put(Bytecode(IR.GroupStart, nglob));
                }
                break;
            case ')':
                nesting || error("Unmatched ')'");
                nesting--;
                next();
                fix = fixupStack.pop();
                switch(ir[fix].code)
                {
                case IR.GroupStart:
                    put(Bytecode(IR.GroupEnd,ir[fix].data));
                    parseQuantifier(fix);
                    break;
                case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                    ir[fix] = Bytecode(ir[fix].code, ir.length - fix - 1);
                    put(ir[fix].paired);
                    break;
                case IR.Option: // | xxx )
                    // two fixups: last option + full OR
                    finishAlternation();
                    fix = fixupStack.top;
                    if(ir[fix].code == IR.GroupStart)//was also a group
                    {
                        fixupStack.pop();
                        put(Bytecode(IR.GroupEnd,ir[fix].data));
                        parseQuantifier(fix);
                    }
                    break;
                default://(?:xxx)
                    parseQuantifier(fix);
                }
                break;
            case '|':
                next();
                fix = fixupStack.top;
                switch(ir[fix].code)
                {
                case IR.Option:
                    ir[fix] = Bytecode(ir[fix].code, ir.length - fix);
                    put(Bytecode(IR.GotoEndOr, 0));
                    fixupStack.top = ir.length; // replace latest fixup for Option
                    put(Bytecode(IR.Option, 0));
                    break;
                default:    //start a new option
                    if(fixupStack.length == 1)//only one entry
                        fix = -1;
                    uint len = ir.length - fix;
                    Bytecode[2] piece = [Bytecode(IR.OrStart, 0), Bytecode(IR.Option, len)];
                    insertInPlace(ir, fix+1, piece[]);
                    assert(ir[fix+1].code == IR.OrStart);
                    put(Bytecode(IR.GotoEndOr, 0));
                    fixupStack.push(fix+1); // fixup for StartOR
                    fixupStack.push(ir.length); //for Option
                    put(Bytecode(IR.Option, 0));
                }

                /*uint maxSub = 0; //maximum number of captures out of each code path

                ngroup = maxSub;*/
                break;
            default://no groups or whatever
                uint start = cast(uint)ir.length;
                parseAtom();
                parseQuantifier(start);
            }
        }
        //unwind fixup stack, check for errors
        if(fixupStack.length != 1)
        {
            fix = fixupStack.pop();
            if(ir[fix].code == IR.Option)
            {
                finishAlternation();
                fixupStack.length == 1 || error(" LR syntax error");
            }
            else
                error(" LR syntax error");
        }
    }
    /*
        Parse and store IR for atom-quantifier pair
    */
    void parseQuantifier(uint offset)
    {
        if(empty)
            return;
        uint len = cast(uint)ir.length - offset;
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
                insertInPlace(ir, offset, Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len));
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                putRaw(1);
                putRaw(min);
                putRaw(max);
                counterDepth = std.algorithm.max(counterDepth, nesting+1);
            }
        }
        else if(min) // && max is infinite
        {
            if(min != 1)
            {
                insertInPlace(ir, offset, Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len));
                offset += 1;//so it still points to the repeated block
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                putRaw(1);
                putRaw(min);
                putRaw(min);
                counterDepth = std.algorithm.max(counterDepth, nesting+1);
            }
            put(Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len));
            ir ~= ir[offset .. offset+len];
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index
        }
        else//vanila {0,inf}
        {
            insertInPlace(ir, offset, Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len));
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index

        }
    }
    /**
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
        case '[':
            parseCharset();
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
            if(re_flags & RegexOption.casefold)
            {
                dchar[5] data;
                auto range = getCommonCasing(current, data);
                foreach(v; range)
                        put(Bytecode(IR.OrChar, v, range.length));
            }
            else
                put(Bytecode(IR.Char, current));
            next();
        }
    }
    //Charset operations relatively in order of priority
    enum Operator:uint { Open=0, Negate,  Difference, SymDifference, Intersection, Union, None };
    // parse unit of charset spec, most notably escape sequences and char ranges
    // also fetches next set operation
    Tuple!(Charset,Operator) parseCharTerm()
    {
        enum State{ Start, Char, Escape, Dash };
        Operator op = Operator.None;;
        dchar last;
        Charset set;
        State state = State.Start;
        static void addWithFlags(ref Charset set, uint ch, uint re_flags)
        {
            if(re_flags & RegexOption.casefold)
            {
                dchar[5] chars;
                auto range = getCommonCasing(ch, chars);
                foreach(v; range)
                    set.add(v);
            }
            else
                set.add(ch);
        }
        L_CharTermLoop:
        for(;;)
        {
            final switch(state)
            {
            case State.Start:
                switch(current)
                {
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    break L_CharTermLoop;
                case '\\':
                    state = State.Escape;
                    break;
                default:
                    state = State.Char;
                    last = current;
                }
                break;
            case State.Char:
                switch(current)
                {
                case '-':
                    if(last == '-')
                    {
                        op = Operator.Difference;
                        next();
                        break L_CharTermLoop;
                    }
                    state = State.Dash;
                    break;
                case '~':
                    if(last == '~')
                    {
                        op = Operator.SymDifference;
                        next();
                        break L_CharTermLoop;
                    }
                    goto default;
                case '&':
                    if(last == '&')
                    {
                        op = Operator.Intersection;
                        next();
                        break L_CharTermLoop;
                    }
                    goto default;
                case '\\':
                    set.add(last);
                    state = State.Escape;
                    break;
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    set.add(last);
                    break L_CharTermLoop;
                default:
                    addWithFlags(set, last, re_flags);
                    last = current;
                }
                break;
            case State.Escape:
                switch(current)
                {
                case 'f':
                    set.add('\f');
                    state = State.Char;
                    break;
                case 'n':
                    set.add('\n');
                    state = State.Char;
                    break;
                case 'r':
                    set.add('\r');
                    state = State.Char;
                    break;
                case 't':
                    set.add('\t');
                    state = State.Char;
                    break;
                case 'v':
                    set.add('\v');
                    state = State.Char;
                    break;
                case '\\':
                    set.add('\\');
                    state = State.Char;
                    break;
                case 'p':
                    set.add(parseUnicodePropertySpec(false));
                    state = State.Start;
                    continue L_CharTermLoop; //next char already fetched
                case 'P':
                    set.add(parseUnicodePropertySpec(true));
                    state = State.Start;
                    continue L_CharTermLoop; //next char already fetched
                //TODO: charsets for commmon properties
                /+
                    case 'd':

                    case 'D':

                    case 's':

                    case 'S':

                    case 'w':

                    case 'W':
                +/
                default:
					assert(0);
                }
                break;
            case State.Dash:
                switch(current)
                {
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    //error("unexpected end of charset");
                    //means dash is a single char not an interval specifier
                    addWithFlags(set, last, re_flags);
                    set.add('-');
                    break L_CharTermLoop;
                 case '-'://set Difference again
                    addWithFlags(set, last, re_flags);
                    op = Operator.Difference;
                    next();//skip '-'
                    break L_CharTermLoop;
                default:
                    last <= current || error("reversed order in interval");
                    if(re_flags & RegexOption.casefold)
                    {
                        for(uint ch = last; ch < current; ch++)
                            addWithFlags(set, ch, re_flags);
                    }
                    else
                        set.add(Interval(last, current));
                    state = State.Start;
                }
                break;
            }
            next() || error("unexpected end of charset");
        }
        return tuple(set, op);
    }
    /**
        Parse and store IR for charset
    */
    void parseCharset()
    {
        Stack!Charset vstack;
        Stack!Operator opstack;
        //
        static bool apply(Operator op, ref Stack!Charset stack)
        {
            switch(op)
            {
            case Operator.Negate:
                stack.top.negate;
                break;
            case Operator.Union:
                auto s = stack.pop();//2nd operand
                stack.top.add(s);
                break;
            case Operator.Difference:
                auto s = stack.pop();//2nd operand
                stack.top.sub(s);
                break;
            case Operator.SymDifference:
                auto s = stack.pop();//2nd operand
                stack.top.symmetricSub(s);
                break;
            case Operator.Intersection:
                auto s = stack.pop();//2nd operand
                stack.top.intersect(s);
                break;
            default:
                return false;
            }
            return true;
        }
        static bool unrollWhile(alias cond)(ref Stack!Charset vstack, ref Stack!Operator opstack)
        {
            while(cond(opstack.top))
            {
                debug(fred_charset)
                {
                    writeln(opstack.stack.data);
                    writeln(map!"a.intervals"(vstack.stack.data));
                }
                if(!apply(opstack.pop(),vstack))
                    return false;//syntax error
                if(opstack.empty)
                    return false;
            }
            return true;
        }


        L_CharsetLoop:
        do
        {
            switch(current)
            {
            case '[':
                opstack.push(Operator.Open);
                next() || error("unexpected end of charset");
                if(current == '^')
                {
                    opstack.push(Operator.Negate);
                    next() || error("unexpected end of charset");
                }
                //[] is prohibited
                current != ']' || error("wrong charset");
                goto default;
            case ']':
                unrollWhile!(unaryFun!"a != a.Open")(vstack, opstack) || error("charset syntax error");
                !opstack.empty || error("unmatched ']'");
                opstack.pop();
                next();
              /*  writeln("After ] ", current, pat);
                writeln(opstack.stack.data);
                writeln(map!"a.intervals"(vstack.stack.data));
                writeln("---");*/
                if(opstack.empty)
                    break L_CharsetLoop;
                auto pair  = parseCharTerm();
                if(!pair[0].empty)//not only operator e.g. -- or ~~
                {
                    vstack.top.add(pair[0]);//apply union
                }
                if(pair[1] != Operator.None)
                {
                    if(opstack.top == Operator.Union)
                        unrollWhile!(unaryFun!"a == a.Union")(vstack, opstack);
                    opstack.push(pair[1]);
                }
                break;
            //
            default://yet another pair of term(op)?
                auto pair = parseCharTerm();
                if(pair[1] != Operator.None)
                {
                    if(opstack.top == Operator.Union)
                        unrollWhile!(unaryFun!"a == a.Union")(vstack, opstack);
                    opstack.push(pair[1]);
                }
                vstack.push(pair[0]);
            }

        }while(!empty || !opstack.empty);
        while(!opstack.empty)
            apply(opstack.pop(),vstack);
        assert(vstack.length == 1);
        put(Bytecode(IR.Charset, charsets.length));
        charsets ~= cast(immutable(Charset))(vstack.top);//unique
    }
    ///parse and return IR
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

        case 'p': case 'P':
            charsets ~= parseUnicodePropertySpec(current == 'P');
            return Bytecode(IR.Charset, charsets.length-1);
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
        case 'U':
            assert(0, "8 hexdigit unicode!");
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
        case '1': .. case '9': //TODO: use $ instead of \ for backreference
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
	/// parse and return a Charset for \p{...Property...} and \P{...Property..},
	// \ - assumed to be processed, p - is current
	immutable(Charset) parseUnicodePropertySpec(bool negated)
	{
		next() && current == '{' || error("{ expected ");
		static bool sep(dchar x){ return x ==':' || x== '}'; }
		auto end = find!(sep)(pat);
		string name = to!string(pat[0..$-end.length]);
		pat = end;
		Charset s;
		if(name.length > 2 && tolower(name[0]) == 'i' && tolower(name[1]) == 'n')
		{//unicode block
			name = name[2..$];//"In" is ascii only, so 2 codepoint == 2 codeunits
			//auto fnd = assumeSorted!(propertyNameLess)(map!"a.name"(unicodeBlocks)).lowerBound(name).length;
            uint fnd;
            for(fnd=0;fnd<unicodeBlocks.length;fnd++)
                if(comparePropertyName(unicodeBlocks[fnd].name,name) == 0)
                    break;
			fnd < unicodeBlocks.length || error("unrecognized unicode block name");
			debug(fred_charset) writefln("For %s using unicode block: %s", name, unicodeBlocks[fnd].extent);
			s = Charset([unicodeBlocks[fnd].extent]);
			if(negated)
				s.negate();
		}
		else
		{//unicode property
			assert(0);
		}
		next() && current == '}' || error("} expected ");
		next();
		return cast(immutable Charset)(s);
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
    ///packages parsing results into a Program object
    @property Program program()
    {
        return Program(this);
    }
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
    immutable(Charset)[] charsets; //
    /++
        lightweight post process step - no GC allocations (TODO!),
        only essentials
    +/
    void lightPostprocess()
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
                assert(i + 1 < ir.length, "unexpected end of IR while looking for hotspot");
                ir[i+1].raw = hotspotIndex;
                hotspotIndex += counterRange[top];
            }
        }
        hotspotTableSize = hotspotIndex;
    }
    /// IR code validator - proper nesting, illegal instructions, etc.
    void validate()
    {
        for(size_t pc=0; pc<ir.length; pc+=ir[pc].length)
        {
            if(ir[pc].isStart || ir[pc].isEnd)
            {
                uint dest =  ir[pc].indexOfPair(pc);
                assert(dest < ir.length, text("Wrong length in opcode at pc=",pc));
                assert(ir[dest].paired ==  ir[pc],
                        text("Wrong pairing of opcodes at pc=", pc, "and pc=", dest));
            }
            else if(ir[pc].isAtom)
            {

            }
            else
               assert(0, text("Unknown type of instruction at pc=", pc));
        }
    }
    /// print out disassembly a program's IR
    void print()
    {
        writefln("PC\tINST\n");
        prettyPrint(delegate void(const(char)[] s){ write(s); },ir);
        writefln("\n");
        for(size_t i=0; i<ir.length; i+=ir[i].length)
        {
            writefln("%d\t%s ", i, disassemble(ir, i, index, dict));
        }
        writeln("Total merge table size: ", hotspotTableSize);
        writeln("Max counter nesting depth: ", maxCounterDepth);
    }
	///
	uint lookupNamedGroup(string name)
	{
		//auto fnd = assumeSorted(map!"a.name"(dict)).lowerBound(name).length;
        uint fnd;
        for(fnd = 0; fnd<dict.length; fnd++)
            if(dict[fnd].name == name)
                break;
        if(fnd == dict.length)
               throw new Exception("out of range");
		/*if(dict[fnd].name != name)
			throw new Exception("out of range");*/
		return dict[fnd].group;
	}
	///
    this(Parser)(Parser p)
    {
        ir = p.ir;
        index = p.index;
        dict = p.dict;
        ngroup = p.ngroup;
        maxCounterDepth = p.counterDepth;
        flags = p.re_flags;
        charsets = p.charsets;
        lightPostprocess();
        debug(fred_parser)
        {
            print();
            validate();
        }
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

/// Simple UTF-string stream abstraction with caching
struct Input(Char)//TODO: simillar thing with UTF normalization
    if(is(Char == char) || is(Char == wchar))
{
    alias const(Char)[] String;
    String _origin;
    size_t _index;
    uint _cached;
    enum { cachedBack = 0x1, cachedFront = 0x2, cachedNext = 0x4 };
    dchar _back, _cur, _next;
    /// constructs Input object out of plain string
    this(String input)
    {
        _origin = input;
        _index = 0;
        _cached = 0;
    }
    /// previous codepoint
    @property dchar back()
    {
        if((_cached & cachedBack))
        {
            _cached |= cachedBack;
            _back = _origin[0.._index].back;
        }
        return _back;
    }
    /// peek at the next codepoint
    @property dchar next()
    {
        if((_cached & cachedNext))
        {
            _cached |= cachedNext;
            _next = _origin[_index+std.utf.stride(_origin, _index)..$].front;
        }
        return _next;
    }
    /// codepoint at current stream position
    @property dchar front()
    {
        if(!(_cached & cachedFront))
        {
            _cached |= cachedFront;
            _cur = _origin[_index..$].front;
        }
        return _cur;
    }
    /// reset position to i
    void seek(size_t i)
    {
        _index = i;
        _cached = 0;
    }
    // returns position in input
    @property size_t index(){ return _index; }
    /// advance input to the next codepoint
    void popFront()
    {
        _index += std.utf.stride(_origin, _index);
        _cached >>= 1;
        _back = _cur;
        _cur = _next;
    }
    /// true if it's end of input
    @property empty(){  return _index == _origin.length; }
    /// true if it's start of input
    @property atStart(){ return _index == 0; }
    String opSlice(size_t start, size_t end){   return _origin[start..end]; }
    @property size_t length(){  return _origin.length;  }
}

///ditto
struct Input(Char)
    if(is(Char == dchar))
{
    alias const(Char)[] String;
    String _origin;
    size_t _index;
    /// constructs Input object out of plain string
    this(String input)
    {
        _origin = input;
        _index = 0;
    }
    /// previous codepoint
    @property dchar back()
    {
        return _origin[_index-1];
    }
    /// peek at the next codepoint
    @property dchar next()
    {
        return _origin[_index+1];
    }
    /// codepoint at current stream position
    @property dchar front()
    {
        return _origin[_index];
    }
    /// reset position to i
    void seek(size_t i)
    {
        _index = i;
    }
    // returns position in input
    @property size_t index(){ return _index; }
    /// advance input to the next codepoint
    void popFront()
    {
        _index++;
    }
    /// true if it's end of input
    @property empty(){  return _index == _origin.length; }
    /// true if it's start of input
    @property atStart(){ return _index == 0; }
    String opSlice(size_t start, size_t end){   return _origin[start..end]; }
    @property size_t length(){  return _origin.length;  }
}


/++
    BacktrackingMatcher implements backtracking scheme of matching
    regular expressions.
    low level construct, doesn't 'own' any memory
+/
struct BacktrackingMatcher(Char)
if( is(Char : dchar) )
{
    alias const(Char)[] String;
    Program re;           //regex program
    enum initialStack = 2^^16;
    String origin, s;
    bool exhausted;
    this(Program program, String input)
    {
        re = program;
        origin = s = input;
        exhausted = false;
    }
    @property dchar previous()
    {
        return origin[0..$-s.length].back;
    }
    @property size_t inputIndex(){ return origin.length - s.length; }
    ///lookup next match, fills matches with indices into input
    bool match(Group matches[])
    {
        debug(fred_parser)
        {
            writeln("------------------------------------------");
        }
        if(exhausted) //all matches collected
            return false;
        for(;;)
        {
            size_t start = origin.length - s.length;
            auto mainStack = (cast(uint*)enforce(malloc(initialStack*uint.sizeof)))
                [0..initialStack];
            scope(exit) free(mainStack.ptr);
            if(matchImpl(re.ir, matches[1..$], mainStack))
            {//s updated
                matches[0].begin = start;
                matches[0].end = origin.length - s.length;
                //empty match advances the input
                if(matches[0].begin == matches[0].end)
                    if(s.empty)
                        exhausted = true;
                    else
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
            stack[last..last+matches.length*groupSize] = (cast(uint*)matches.ptr)[0..groupSize*matches.length];
            last += matches.length*groupSize;
        }
        //helper function restores engine state
        bool popState()
        {
            if(!last)
                return false;
            last -= matches.length*groupSize;
            matches[] = (cast(Group*)&stack[last])[0..matches.length];
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
            debug(fred_matching) writeln("Backtracked");
            return true;
        }
        auto start = origin.length - s.length;
        debug(fred_matching) writeln("Try match starting at ",origin[inputIndex..$]);
        while(pc<prog.length)
        {
            debug(fred_matching) writefln("PC: %s\tCNT: %s\t%s", pc, counter, disassemble(prog, pc, re.index, re.dict));
            switch(prog[pc].code)
            {
            case IR.OrChar://assumes IRL!(OrChar) == 1
                if(s.empty)
                    goto L_backtrack;
                uint len = prog[pc].sequence;
                uint end = pc + len;
                for(; pc<end; pc++)
                    if(prog[pc].data == s.front)
                        break;
                if(pc == end)
                    goto L_backtrack;
                pc = end;
                s.popFront();
                break;
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
            case IR.Charset:
                if(s.empty || !re.charsets[prog[pc].data].contains(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Charset);
                break;
            case IR.Wordboundary:
                //at start & end of input
                if((s.empty && isUniAlpha(previous))
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
            case IR.InfiniteStart, IR.InfiniteQStart:
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
                    infiniteNesting++;
                    pushState(pc - len, counter);
                    infiniteNesting--;
                    pc += IRL!(IR.InfiniteEnd);
                }
                break;
            case IR.RepeatStart, IR.RepeatQStart:
                pc += prog[pc].data + IRL!(IR.RepeatStart);
                break;
            case IR.RepeatEnd:
            case IR.RepeatQEnd:
                // len, step, min, max
                uint len = prog[pc].data;
                uint step =  prog[pc+1].raw;
                uint min = prog[pc+2].raw;
                uint max = prog[pc+3].raw;
                //debug(fred_matching) writefln("repeat pc=%u, counter=%u",pc,counter);

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
                debug(fred_matching) writeln("Infinited nesting:", infiniteNesting);
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
            case IR.Option:
                uint len = prog[pc].data;
                if(prog[pc+len].code == IR.GotoEndOr)//not a last one
                {
                   pushState(pc + len + IRL!(IR.Option), counter); //remember 2nd branch
                }
                pc += IRL!(IR.Option);
                break;
            case IR.GotoEndOr:
                pc = pc + prog[pc].data + IRL!(IR.GotoEndOr);
                break;
            case IR.GroupStart: //TODO: mark which global matched and do the other alternatives
                uint n = prog[pc].data;
                matches[re.index[n]].begin = inputIndex;
                debug(fred_matching)  writefln("IR group #%u starts at %u", n, inputIndex);
                pc += IRL!(IR.GroupStart);
                break;
            case IR.GroupEnd:   //TODO: ditto
                uint n = prog[pc].data;
                matches[re.index[n]].end = inputIndex;
                debug(fred_matching) writefln("IR group #%u ends at %u", n, inputIndex);
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
        /// add new thread to the start of list
        void insertFront(Thread* t)
        {
            if(tip)
            {
                t.next = tip;
                tip = t;
            }
            else
            {
                t.next = null;
                tip = toe = t;
            }
            length++;
        }
        //add new thread to the end of list
        void insertBack(Thread* t)
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
        ///move head element out of list
        Thread* fetch()
        {
            auto t = tip;
            if(tip == toe)
                tip = toe = null;
            else
                tip = tip.next;
            if(t) length--;
            return t;
        }
        ///non-destructive iteration of ThreadList
        struct ThreadRange
        {
            const(Thread)* ct;
            this(ThreadList tlist){ ct = tlist.tip; }
            @property bool empty(){ return ct == null; }
            @property const(Thread)* front(){ return ct; }
            @property popFront()
            {
                assert(ct);
                ct = ct.next;
            }
        }
        @property bool empty()
        {
            return tip == null;
        }
        ThreadRange opSlice()
        {
            return ThreadRange(this);
        }
    }
    enum threadAllocSize = 16;
    Thread* freelist;
    ThreadList clist, nlist;
    uint[] merge;
    Program re;           //regex program
    Input!Char s;
    size_t genCounter;    //merge trace counter, goes up on every dchar
    bool matched;

    this(Program program, String input)
    {
        re = program;
        s = Input!Char(input);
        if(re.hotspotTableSize)
        {
            merge = new uint[re.hotspotTableSize];
        }
        genCounter = 0;
    }
    /++
        the usual match the input and fill matches
    +/
    bool match(Group[] matches)
    {
        debug(fred_matching)
        {
            writeln("------------------------------------------");
        }
        matched = false; //reset match flag
        Bytecode[] prog = re.ir;

        assert(clist == ThreadList.init);
        assert(nlist == ThreadList.init);
        while(!s.empty)
        {
            genCounter++;
            debug(fred_matching)
            {
                writefln("Threaded matching %d threads at index %s", clist.length, s.index);
                foreach(t; clist[])
                {
                    assert(t);
                    writef("pc=%s ",t.pc);
                    write(t.matches);
                    writeln();
                }
            }
            for(Thread* t = clist.fetch(); t; t = clist.fetch())
            {
                eval!true(t, matches);
            }
            if(!matched)//if we already have match no need to push the engine
                eval!true(createStart(s.index), matches);// new thread staring at this position
            else if(nlist.empty)
            {
                debug(fred_matching) writeln("Stopped  matching before consuming full input");
                break;//not a partial match for sure
            }
            clist = nlist;
            nlist = ThreadList.init;
            //to next codepoint
            s.popFront();
        }
        genCounter++; //increment also on each end
        debug(fred_matching) writefln("Threaded matching %d threads at end", clist.length);
        //try out all zero-width posibilities
        if(!matched)
            eval!false(createStart(s.index), matches);// new thread starting at end of input
        for(Thread* t = clist.fetch(); t; t = clist.fetch())
        {
            eval!false(t, matches);
        }
        //writeln("CLIST :", clist[]);
        //TODO: partial matching
        return matched;
    }
    /++
        handle succesful threads
    +/
    void finish(const(Thread)* t, Group[] matches)
    {
        //debug(fred_matching) writeln(t.matches);
        matches[] = t.matches[];
        //end of the whole match happens after current symbol
        matches[0].end = s.index;
        debug(fred_matching) writefln("FOUND pc=%s prog_len=%s: %s..%s",
                    t.pc, re.ir.length,matches[0].begin, matches[0].end);
        matched = true;
    }

    /++
        match thread against codepoint, cutting trough all 0-width instructions
        and taking care of control flow, then add it to nlist
    +/
    void eval(bool withInput)(Thread* t, Group[] matches)
    {
        Bytecode[] prog = re.ir;
        ThreadList worklist;
        debug(fred_matching) writeln("Evaluating thread");
        do
        {
            debug(fred_matching)
            {
                writef("\tpc=%s [", t.pc);
                foreach(x; worklist[])
                    writef(" %s ", x.pc);
                writeln("]");
            }
            if(t.pc == prog.length)
            {
                finish(t, matches);
                recycle(t);
                //cut off low priority threads
                recycle(clist);
                recycle(worklist);
                return;
            }
            else
            {
                switch(prog[t.pc].code)
                {
                case IR.Wordboundary:
                    //at start & end of input
                    if((s.empty && isUniAlpha(s.back))
                       || (s.atStart && isUniAlpha(s.front)) )
                        t.pc += IRL!(IR.Wordboundary);
                    else if( (isUniAlpha(s.front) && !isUniAlpha(s.back))
                          || (!isUniAlpha(s.front) && isUniAlpha(s.back)) )
                        t.pc += IRL!(IR.Wordboundary);
                    else
                    {
                        recycle(t);
                        t = worklist.fetch();
                    }
                    break;
                case IR.Notwordboundary:
                    if((s.empty && isUniAlpha(s.back))
                       || (s.atStart && isUniAlpha(s.front)) )
                    {
                        recycle(t);
                        t = worklist.fetch();
                    }
                    else if( (isUniAlpha(s.front) && !isUniAlpha(s.back))
                          || (!isUniAlpha(s.front) && isUniAlpha(s.back)) )
                    {
                        recycle(t);
                        t = worklist.fetch();
                    }
                    else
                        t.pc += IRL!(IR.Wordboundary);
                    break;
                case IR.Bol:
                    //TODO: multiline & attributes, unicode line terminators
                    if(s.atStart || (s.back == '\n'))
                    {
                        t.pc += IRL!(IR.Bol);
                    }
                    else
                        t = worklist.fetch();
                    break;
                case IR.Eol:
                    //TODO: ditto for the end of line
                    debug(fred_matching) writeln("EOL ", s);
                    if(s.empty || s.front == '\n')
                    {
                        t.pc += IRL!(IR.Eol);
                    }
                    else
                        t = worklist.fetch();
                    break;
                case IR.InfiniteStart, IR.InfiniteQStart:
                    t.pc += prog[t.pc].data + IRL!(IR.InfiniteStart);
                    goto case IR.InfiniteEnd; // both Q and non-Q
                    break;
                case IR.RepeatStart, IR.RepeatQStart:
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
                        break;
                    }
                    uint max = prog[t.pc+3].raw;
                    if(t.counter < max)
                    {
                        if(prog[t.pc].code == IR.RepeatEnd)
                        {
                            //queue out-of-loop thread
                            worklist.insertFront(fork(t, t.pc + IRL!(IR.RepeatEnd),  t.counter % step));
                            t.counter += step;
                            t.pc -= len;
                        }
                        else
                        {
                            //queue into-loop thread
                            worklist.insertFront(fork(t, t.pc - len,  t.counter + step));
                            t.counter %= step;
                            t.pc += IRL!(IR.RepeatEnd);
                        }
                    }
                    else
                    {
                        t.counter %= step;
                        t.pc += IRL!(IR.RepeatEnd);
                    }
                    break;
                case IR.InfiniteEnd:
                case IR.InfiniteQEnd:
                    if(merge[prog[t.pc + 1].raw+t.counter] < genCounter)
                    {
                        debug(fred_matching) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                        t.pc, s[s.index..s.length], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
                        merge[prog[t.pc + 1].raw+t.counter] = genCounter;
                    }
                    else
                    {
                        debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                        t.pc, s[s.index..s.length], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
                        t = worklist.fetch();
                        break;
                    }
                    uint len = prog[t.pc].data;
                    if(prog[t.pc].code == IR.InfiniteEnd)
                    {
                         //queue out-of-loop thread
                        worklist.insertFront(fork(t, t.pc +IRL!(IR.InfiniteEnd), t.counter));
                        t.pc -= len;
                    }
                    else
                    {
                        //queue into-loop thread
                        worklist.insertFront(fork(t, t.pc - len, t.counter));
                        t.pc += IRL!(IR.InfiniteQEnd);
                    }
                    break;
                case IR.OrEnd:
                    if(merge[prog[t.pc + 1].raw+t.counter] < genCounter)
                    {
                        debug(fred_matching) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                        t.pc, s[s.index..s.length], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
                        merge[prog[t.pc + 1].raw+t.counter] = genCounter;
                        t.pc += IRL!(IR.OrEnd);
                    }
                    else
                    {
                        debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                        t.pc, s[s.index..s.length], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
                        t = worklist.fetch();
                    }
                    break;
                case IR.OrStart:
                    t.pc += IRL!(IR.OrStart);
                    goto case;
                case IR.Option:
                    uint next = t.pc + prog[t.pc].data + IRL!(IR.Option);
                    //queue next Option
                    if(prog[next].code == IR.Option)
                    {
                        worklist.insertFront(fork(t, next, t.counter));
                    }
                    t.pc += IRL!(IR.Option);
                    break;
                case IR.GotoEndOr:
                    t.pc = t.pc + prog[t.pc].data + IRL!(IR.GotoEndOr);
                    break;
                case IR.GroupStart: //TODO: mark which global matched and do the other alternatives
                    uint n = prog[t.pc].data;
                    t.matches[re.index[n]+1].begin = s.index;
                    t.pc += IRL!(IR.GroupStart);
                    //debug(fred_matching)  writefln("IR group #%u starts at %u", n, i);
                    break;
                case IR.GroupEnd:   //TODO: ditto
                    uint n = prog[t.pc].data;
                    t.matches[re.index[n]+1].end = s.index;
                    t.pc += IRL!(IR.GroupEnd);
                    //debug(fred_matching) writefln("IR group #%u ends at %u", n, i);
                    break;
                case IR.Backref:
                    uint n = prog[t.pc].data;
                    if(t.matches[n+1].begin == t.matches[n+1].end)//zero-width Backref!
                    {
                        t.pc += IRL!(IR.Backref);
                    }
                    else static if(withInput)
                    {
                        uint idx = t.matches[n+1].begin + t.uopCounter;
                        if(s[idx..s.length].front == s.front)
                        {
                           t.uopCounter += std.utf.stride(s[idx..s.length], 0);
                           if(t.uopCounter + t.matches[n+1].begin == t.matches[n+1].end)
                           {//last codepoint
                                t.pc += IRL!(IR.Backref);
                                t.uopCounter = 0;
                           }
                           nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                    }
                    else
                    {
                        recycle(t);
                        t = worklist.fetch();
                    }
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
                static if(withInput)
                {
                    case IR.OrChar://assumes IRL!(OrChar) == 1
                        uint len = prog[t.pc].sequence;
                        uint end = t.pc + len;
                        for(; t.pc<end; t.pc++)
                            if(prog[t.pc].data == s.front)
                                break;
                        if(t.pc != end)
                        {
                            t.pc = end;
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Char:
                        if(s.front == prog[t.pc].data)
                        {
                            // debug(fred_matching) writefln("IR.Char %s vs %s ", s.front, cast(dchar)prog[t.pc].data);
                            t.pc += IRL!(IR.Char);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Any:
                        t.pc += IRL!(IR.Any);
                        nlist.insertBack(t);
                        t = worklist.fetch();
                        break;
                    case IR.Word:
                        if(isUniAlpha(s.front))
                        {
                            t.pc += IRL!(IR.Word);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Notword:
                        if(!isUniAlpha(s.front))
                        {
                            t.pc += IRL!(IR.Notword);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Digit:
                        if(isdigit(s.front))
                        {
                            t.pc += IRL!(IR.Digit);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Notdigit:
                        if(!isdigit(s.front))
                        {
                            t.pc += IRL!(IR.Notdigit);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Space:
                        if(isspace(s.front))
                        {
                            t.pc += IRL!(IR.Space);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Notspace:
                        if(!isspace(s.front))
                        {
                            t.pc += IRL!(IR.Space);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Charset:
                        if(re.charsets[prog[t.pc].data].contains(s.front))
                        {
                            debug(fred_matching) writeln("Charset passed");
                            t.pc += IRL!(IR.Charset);
                            nlist.insertBack(t);
                        }
                        else
                        {
                            debug(fred_matching) writeln("Charset notpassed");
                            recycle(t);
                        }
                        t = worklist.fetch();
                        break;
                    default:
                        assert(0, "Unrecognized instruction " ~ prog[t.pc].mnemonic);
                }
                else
                {
                    default:
                        recycle(t);
                        t = worklist.fetch();
                }

                }
            }
        }while(t);
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
            debug(fred_matching) writefln("Allocated space for another %d threads", threadAllocSize);
            return allocate();
        }
    }
    ///dispose a thread
    void recycle(Thread* t)
    {
        t.next = freelist;
        freelist = t;
    }
    //dispose list of threads
    void recycle(ref ThreadList list)
    {
        auto t = list.tip;
        while(t)
        {
            auto next = t.next;
            recycle(t);
            t = next;
        }
        list = list.init;
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
    alias Engine!(Unqual!(typeof(R.init[0]))) EngineType;
    EngineType engine;
    struct Captures
    {
        R input;
        Group[] groups;
        uint[] index;
        Program re;
        this(ref RegexMatch rmatch)
        {
            input = rmatch.input;
            index = rmatch.index;
            groups = rmatch.matches;
            re = rmatch.engine.re;
        }
        @property R front()
        {
            assert(!index.empty);
            return input[groups[index[0]].begin .. groups[index[0]].end];
        }
        @property R back()
        {
            assert(!index.empty);
            return input[groups[index[$-1]].begin .. groups[index[$-1]].end];
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
            return input[groups[index[i]].begin..groups[index[i]].end];
        }
        R opIndex(string i)
        {
            size_t index = re.lookupNamedGroup(i);
            return opIndex(index);
        }
        @property size_t length() const { return index.length; }
    }

public:
    ///
    this(Program prog, R _input)
    {
        input = _input;
        index = prog.index;
        matches = new Group[prog.ngroup];
        engine = EngineType(prog, input);
        popFront();
    }
    ///
    @property R pre()
    {
        assert(!empty);
        return input[0 .. matches[0].begin];
    }
    ///
    @property R post()
    {
        assert(!empty);
        return input[matches[0].end..$];
    }
    ///
    @property R hit()
    {
        assert(!empty);
        return input[matches[0].begin .. matches[0].end];
    }
    ///
    @property auto front()
    {
        return this;
    }
    ///
    void popFront()
    { //previous one can have escaped references from Capture object
        matches = new Group[matches.length];
        _empty = !engine.match(matches);
    }
    ///
    @property bool empty(){ return _empty; }
    ///
    @property auto captures(){ return Captures(this); }
}

auto regex(S)(S pattern, S flags=[])
{
    auto parser = Parser!(typeof(pattern))(pattern, flags);
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
