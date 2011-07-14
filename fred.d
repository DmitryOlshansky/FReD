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
       std.conv, std.exception, std.traits, std.typetuple,
       std.uni, std.utf, std.format, std.typecons, std.bitmanip, std.functional, std.exception;
import ascii = std.ascii;

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
/// * merge group, option, infinite/repeat start (to never copy during parsing of (a|b){1,2}) ?
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
    /// match with any of a consecutive OrChar's in this sequence (used for case insensitive match)
    /// OrChar holds in upper two bits of data total number of OrChars in this _sequence_
    /// the drawback of this representation is that it is difficult to detect a jump in the middle of it
    OrChar            = 0b1_10011_00,

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
        assert(seq >= 2 && seq < 2+4);
        raw = code<<24 | ((seq-2)<<22) | data;
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
    @property uint sequence(){ return 2+(raw >>22) & 0x3; }
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

/// debugging tool, prints out instruction along with opcodes
string disassemble(Bytecode[] irb, uint pc, uint[] index, NamedGroup[] dict=[])
{
    auto output = appender!string();
    formattedWrite(output,"%s", irb[pc].mnemonic);
    switch(irb[pc].code)
    {
    case IR.Char:
        formattedWrite(output, " %s (0x%x)",cast(dchar)irb[pc].data, irb[pc].data);
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
private enum NEL = '\u0085', LS = '\u2028', PS = '\u2029'; 
//multiply-add, throws exception on overflow
uint checkedMulAdd(uint f1, uint f2, uint add)
{
    ulong r = f1 * cast(ulong)f2 + add;
    if(r < (1<<32UL))
        throw new RegexException("Regex internal errror - integer overflow");
    return cast(uint)r;
}

/// test if a given string starts with hex number of maxDigit that's a valid codepoint
/// returns it's value and skips these maxDigit chars on success, throws on failure
dchar parseUniHex(Char)(ref immutable(Char)[] str, uint maxDigit)
{
    enforce(str.length >= maxDigit,"incomplete escape sequence");        
    uint val;
    for(int k=0;k<maxDigit;k++)
    {
        auto current = str[k];//accepts ascii only, so it's OK to index directly
        if('0' <= current && current <= '9')
            val = val * 16 + current - '0';
        else if('a' <= current && current <= 'f')
            val = val * 16 + current -'a' + 10;
        else if('A' <= current && current <= 'Z')
            val = val * 16 + current - 'A' + 10;
        else
            throw new Exception("invalid escape sequence");
    }
    enforce(val <= 0x10FFFF, "invalid codepoint");
    str = str[maxDigit..$];
    return val;
}
///index entry structure for name --> number of submatch
struct NamedGroup
{
    string name;
    uint group;
}
///holds pir of start-end markers for a submatch
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
                       begin, ascii.isGraphical(begin) ? to!string(cast(dchar)begin) : "",
                       end, ascii.isGraphical(end) ? to!string(cast(dchar)end) : "");
        return s.data;
    }
}
/// basic internal data structure for [...] sets
struct Charset
{
    Interval[] intervals;
    ///
    ref add(Interval inter)
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
        return this;
    }
    ///
    ref add(dchar ch){ add(Interval(cast(uint)ch)); return this; }
    /// this = this || set
    ref add(in Charset set)
    {
        debug(fred_charset) writef ("%s || %s --> ", intervals, set.intervals);
        foreach(inter; set.intervals)
            add(inter);
        debug(fred_charset) writeln(intervals);
        return this;
    }
    /// this = this -- set
    ref sub(in Charset set)
    {
        if(empty)
        {
            intervals = [];
            return this;
        }
        if(set.empty)
            return this;
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
        return this;
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
    ref intersect(in Charset set)
    {
        if(empty || set.empty)
        {
            intervals = [];
            return this;
        }
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
        return this;
    }
    /// this = !this (i.e. [^...] in regex syntax)
    auto negate()
    {
        if(intervals.empty)
        {
            intervals ~= Interval(0, 0x10FFFF);
            return this;
        }
        Interval[] negated;
        if(intervals[0].begin != 0)
            negated ~= Interval(0, intervals[0].begin-1);
        for(size_t i=0; i<intervals.length-1; i++)
            negated ~= Interval(intervals[i].end+1, intervals[i+1].begin-1);
        if(intervals[$-1].end != 0x10FFFF)
            negated ~= Interval(intervals[$-1].end+1, 0x10FFFF);
        intervals = negated;
        return this;
    }
    /// test if ch is present in this set
    bool contains(dchar ch) const
    {
        //debug(fred_charset) writeln(intervals);
        for(uint i=0; i<intervals.length; i++) // could use binary search (lower bound) on (cast(uint*)intervals.ptr)[0..2*intervals.length], and then check if it is even or odd...
            if(ch >= intervals[i].begin && ch <= intervals[i].end)
                return true;
        return false;
        //debug(fred_charset) writeln("Test at ", fnd);
    }
    /// true if set is empty
    @property bool empty() const {   return intervals.empty; }
    /// print out in [\uxxxx-\uyyyy...] style
    void printUnicodeSet(void delegate(const(char)[])sink) const
    {
        sink("[");
        foreach(i; intervals)
            if(i.begin == i.end)
                formattedWrite(sink, "\\U%08x", i.begin);
            else
                formattedWrite(sink, "\\U%08x-\\U%08x", i.begin, i.end);
        sink("]");
    }
    
    /// deep copy this Charset
    @property Charset dup()const
    {
        return Charset(intervals.dup);
    }
    
}
/// fussy compare for unicode property names as per UTS-18
int comparePropertyName(Char)(const(Char)[] a, const(Char)[] b)
{
    for(;;)
    {
        while(!a.empty && (isWhite(a.front) || a.front == '-' || a.front =='_'))
        {
            a.popFront();
        }
        while(!b.empty && (isWhite(b.front) || b.front == '-' || b.front =='_'))
        {
            b.popFront();
        }
        if(a.empty)
            return b.empty ? 0 : -1;
        if(b.empty)
            return 1;
        auto ca = toLower(a.front), cb = toLower(b.front);
        if(ca > cb)
            return 1;
        else if( ca < cb)
            return -1;
        a.popFront();
        b.popFront();
    }
}
///ditto
bool propertyNameLess(Char)(const(Char)[] a, const(Char)[] b)
{
	return comparePropertyName(a, b) < 0;
}

unittest
{
    assert(comparePropertyName("test","test") == 0);
    assert(comparePropertyName("Al chemical Symbols", "Alphabetic Presentation Forms") == -1);
    assert(comparePropertyName("Basic Latin","basic-LaTin") == 0);
}

///Gets array of all of common case eqivalents of given codepoint (fills provided array & returns a slice of it)
dchar[] getCommonCasing(dchar ch, dchar[] range)
{
    assert(range.length >= 5);
    range[0] = ch;
    if(evenUpper.contains(ch))//simple version
    {
        range[1] = ch ^ 1;
        return range[0..2];
    }
    uint s = 0, n = 1;
    for(s=0;s < n; s++)
    {
        foreach(i, v; commonCaseTable)
            if(v.set.contains(range[s]) && !canFind(range[0..n], range[s]+cast(int)v.delta))
            {

                range[n++] = range[s]+v.delta;
            }
        auto f = countUntil(casePairs, range[s]);
        if(f >=0)
            while(1)
            {
                if(!canFind(range[0..n], casePairs[f^1]))
                {
                   range[n++] = casePairs[f^1];
                }
                f++;
                auto next =  countUntil(casePairs[f..$], range[s]);
                if(next < 0)
                    break;
                f += next;
            }
    }
    return range[0..n];
}

unittest
{
    dchar[6] data;
    //these values give 100% code coverage for getCommonCasing
    assert(getCommonCasing(0x01BC, data) == [0x01bc, 0x01bd]);
    assert(getCommonCasing(0x03B9, data) == [0x03b9, 0x0399, 0x0345, 0x1fbe]);
    assert(getCommonCasing(0x10402, data) == [0x10402, 0x1042a]);
}
//property for \w character class
Charset wordCharacter;

static this()
{
    wordCharacter.add(unicodeAlphabetic).add(unicodeMn).add(unicodeMc)
        .add(unicodeMe).add(unicodeNd).add(unicodePc); 
}

/++
    fetch codepoint set corrsponding to a name (InBlock or binary property)
+/
immutable(Charset) getUnicodeSet(in char[] name, bool negated)
{
    alias comparePropertyName ucmp;
    Charset s;
    
    //unicode property
    //helper: direct access with a sanity check
    static void addTest(ref Charset set, int delta, uint index)
    {
        assert(commonCaseTable[index].delta == delta, text(commonCaseTable[index].delta," vs ", delta));
        set.add(commonCaseTable[index].set);
    }  
    if(ucmp(name, "L") == 0 || ucmp(name, "Letter") == 0)
    {
        s.add(evenUpper);
        foreach(v; commonCaseTable)
            s.add(v.set);
        foreach(v; casePairs)
            s.add(v);
        s.add(unicodeLt).add(unicodeLo).add(unicodeLm);
    }
    else if(ucmp(name,"LC") == 0 || ucmp(name,"Cased Letter")==0)
    {
        s.add(evenUpper);
        foreach(v; commonCaseTable)
            s.add(v.set);
        foreach(v; casePairs)
            s.add(v);
        s.add(unicodeLt);//Title case
    }
    else if(ucmp(name,"Ll") == 0 || ucmp(name,"Lowercase Letter")==0)
    {
        foreach(ival; evenUpper.intervals)
            for(uint ch=ival.begin; ch<=ival.end; ch++)
                if(ch & 1)
                    s.add(ch);   
        addTest(s,   8, 0);
        addTest(s, -32, 7);
        addTest(s, -37, 9);
        addTest(s, -40, 11);
        addTest(s, -48, 13);
        addTest(s, -63, 15);
        addTest(s,  74, 16);
        addTest(s, -80, 19);
        addTest(s,  86, 20);
        addTest(s, 100, 22);
        addTest(s, 112, 24);
        addTest(s, 126, 26);
        addTest(s, 128, 28);
        addTest(s, 130, 30);
        addTest(s,-205, 33);
        addTest(s,-217, 35);
        addTest(s,-7264, 37);
        addTest(s,10815, 38);   
    }
    else if(ucmp(name,"Lu") == 0 || ucmp(name,"Uppercase Letter")==0)
    {
        foreach(ival; evenUpper.intervals)
            for(uint ch=ival.begin; ch<=ival.end; ch++)
                if(!(ch & 1))
                    s.add(ch);
        addTest(s,  -8, 1);
        addTest(s,  32, 6);
        addTest(s,  37, 8);
        addTest(s,  40, 10);
        addTest(s,  48, 12);
        addTest(s,  63, 14);
        addTest(s, -74, 17);
        addTest(s,  80, 18);
        addTest(s, -86, 21);
        addTest(s,-100, 23);
        addTest(s,-112, 25);
        addTest(s,-126, 27);
        addTest(s,-128, 29);
        addTest(s,-130, 31);
        addTest(s, 205, 32);
        addTest(s, 217, 34);
        addTest(s, 7264, 36);
        addTest(s,-10815, 39); 
    }
    else if(ucmp(name, "M") == 0 || ucmp(name, "Mark") == 0)
    {
        s.add(unicodeMn).add(unicodeMc).add(unicodeMe);
    }
    else if(ucmp(name, "P") == 0 || ucmp(name, "Punctuation") == 0)
    {
        s.add(unicodePc).add(unicodePd).add(unicodePs).add(unicodePe)
            .add(unicodePi).add(unicodePf).add(unicodePo);
    }
    else if(ucmp(name, "S") == 0 || ucmp(name, "Symbol") == 0)
    {
        s.add(unicodeSm).add(unicodeSc).add(unicodeSk).add(unicodeSo);
    }
    else if(ucmp(name, "Z") == 0 || ucmp(name, "Separator") == 0)
    {
        s.add(unicodeZs).add(unicodeZl).add(unicodeZp);
    }
    else if(ucmp(name, "C") == 0 || ucmp(name, "Other") == 0)
    {
        s.add(unicodeCo).add(unicodeLo).add(unicodeNo)
            .add(unicodeSo).add(unicodePo);
    }
    else if(ucmp(name, "any") == 0)
        s.add(Interval(0,0x10FFFF));
    else if(ucmp(name, "ascii") == 0)
        s.add(Interval(0,0x7f));
    else
    {
        version(fred_perfect_hashing)
        {
            uint key = phash(name);
            if(key >= PHASHNKEYS || ucmp(name,unicodeProperties[key].name) != 0)
                enforce(0, "invalid property name");
            s = cast(Charset)unicodeProperties[key].set;
        }
        else
        {
            auto range = assumeSorted!((x,y){ return ucmp(x.name, y.name) < 0; })(unicodeProperties); 
            auto eq = range.equalRange(UnicodeProperty(cast(string)name,Charset.init));//TODO: hackish
            enforce(!eq.empty,"invalid property name");
            s = cast(Charset)eq.front.set;
        }
    }
    if(negated)
    {
		s = s.dup;//tables are immutable
        s.negate();
    }
    return cast(immutable(Charset))s;
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
    uint counterDepth = 0; //current depth of nested counted repetitions
    immutable(Charset)[] charsets;
    this(R pattern, R flags)
    {
        pat = origin = pattern;
        index = [ 0 ]; //map first to start-end of the whole match
        ir.reserve(pat.length);
        next();
        parseFlags(flags);
        try
        {    
            parseRegex();
        }
        catch(Exception e)
        {
            error(e.msg);//also adds pattern location
        }
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
        while(isWhite(current) && next()){ }
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
        while(ascii.isDigit(current))
        {
            if(r >= (uint.max/10))
                error("Overflow in decimal number");
            r = 10*r + cast(uint)(current-'0');
            next();
        }
        return r;
    }
    // parse control code of form \cXXX, c assumed to be the current symbol
    dchar parseControlCode()
    {
        next() || error("Unfinished escape sequence");
        ('a' <= current && current <= 'z') || ('A' <= current && current <= 'Z')
            || error("Only letters are allowed after \\c");
        return current & 0x1f;
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
                        put(Bytecode(IR.Nop, 0));
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
                        while(next() && isAlpha(current))
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
                    switch(ir[fix].code)
                    {
                    case IR.GroupStart:
                        fixupStack.pop();
                        put(Bytecode(IR.GroupEnd,ir[fix].data));
                        parseQuantifier(fix);
                        break;
                    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                        fixupStack.pop();
                        ir[fix] = Bytecode(ir[fix].code, ir.length - fix - 1);
                        put(ir[fix].paired);
                        break;
                    default://(?:xxx)
                        fixupStack.pop();
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
                    if(fixupStack.length == 1)//only root entry
                        fix = -1;
                    uint len = ir.length - fix;
                    insertInPlace(ir, fix+1, Bytecode(IR.OrStart, 0), Bytecode(IR.Option, len));
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
        bool replace = ir[offset].code == IR.Nop;
        if(empty && !replace)
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
            ascii.isDigit(current) || error("First number required in repetition");
            min = parseDecimal();
            //skipSpace();
            if(current == '}')
                max = min;
            else if(current == ',')
            {
                next();
                if(ascii.isDigit(current))
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
            if(replace)
            {
                moveAll(ir[offset+1..$],ir[offset..$-1]);
                ir.length -= 1;
            }
            return;
        }
        uint len = cast(uint)ir.length - offset - cast(uint)replace;
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
                Bytecode op = Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len);
                if(replace)
                    ir[offset] = op;
                else
                    insertInPlace(ir, offset, op);
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
                Bytecode op = Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len);
                if(replace)
                    ir[offset] = op;
                else
                    insertInPlace(ir, offset, op);
                offset += 1;//so it still points to the repeated block
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                putRaw(1);
                putRaw(min);
                putRaw(min);
                counterDepth = std.algorithm.max(counterDepth, nesting+1);
            }
            else if(replace)
            {
                moveAll(ir[offset+1..$],ir[offset..$-1]);
                ir.length -= 1;
            }
            put(Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len));
            ir ~= ir[offset .. offset+len];
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index
        }
        else//vanila {0,inf}
        {
            Bytecode op = Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len);
            if(replace)
                ir[offset] = op;
            else
                insertInPlace(ir, offset, op);
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
                if(range.length == 1)
                    put(Bytecode(IR.Char, range[0]));
                else
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
        enum State{ Start, Char, Escape, Dash, DashEscape };
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
                case '|':
                    if(last == '|')
                    {
                        op = Operator.Union;
                        next();
                        break L_CharTermLoop;
                    }
                    goto default;   
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
                    last = '\f';
                    state = State.Char;
                    break;
                case 'n':
                    last = '\n';
                    state = State.Char;
                    break;
                case 'r':
                    last = '\r';
                    state = State.Char;
                    break;
                case 't':
                    last = '\t';
                    state = State.Char;
                    break;
                case 'v':
                    last = '\v';
                    state = State.Char;
                    break;
                case 'c':
                    last = parseControlCode();
                    state = State.Char;
                    break;
                case '\\', '[', ']':
                    last = current;
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
                case 'x':
                    last = parseUniHex(pat, 2);
                    state = State.Char;
                    break;
                case 'u':
                    last = parseUniHex(pat, 4);
                    state = State.Char;
                    break;
                case 'U':
                    last = parseUniHex(pat, 8);
                    state = State.Char;
                    break;                
                case 'd':
                    set.add(unicodeNd);
                    state = State.Start;
                    break;
                case 'D':
                    set.add(unicodeNd.dup.negate);
                    state = State.Start;
                    break;
                case 's':
                    set.add(unicodeWhite_Space);
                    state = State.Start;
                    break;
                case 'S':
                    set.add(unicodeWhite_Space.dup.negate);
                    state = State.Start;
                    break;
                case 'w':
                    set.add(wordCharacter);
                    state = State.Start;
                    break;
                case 'W':
                    set.add(wordCharacter.dup.negate);
                    state = State.Start;
                    break;
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
                    //means dash is a single char not an interval specifier
                    addWithFlags(set, last, re_flags);
                    set.add('-');
                    break L_CharTermLoop;
                 case '-'://set Difference again
                    addWithFlags(set, last, re_flags);
                    op = Operator.Difference;
                    next();//skip '-'
                    break L_CharTermLoop;
                case '\\':
                    state = State.DashEscape;
                    break;
                default:
                    enforce(last <= current, "inverted range");
                    if(re_flags & RegexOption.casefold)
                    {
                        for(uint ch = last; ch <= current; ch++)
                            addWithFlags(set, ch, re_flags);
                    }
                    else
                        set.add(Interval(last, current));
                    state = State.Start;
                }
                break;            
            case State.DashEscape:  // xxxx-\yyyy
                uint end;
                switch(current)
                {
                case 'f':
                    end = '\f';
                    break;
                case 'n':
                    end = '\n';
                    break;
                case 'r':
                    end = '\r';
                    break;
                case 't':
                    end = '\t';
                    break;
                case 'v':
                    end = '\v';
                    break;
                case '\\', '[', ']': 
                    end = current;
                    break;
                case 'c':
                    end = parseControlCode();
                    break;
                case 'x':
                    end = parseUniHex(pat, 2);
                    break;
                case 'u': 
                    end = parseUniHex(pat, 4);
                    break;
                case 'U':
                    end = parseUniHex(pat, 8);
                    break;
                default:
                    error("invalid escape sequence");
                }
                enforce(last <= end,"inverted range");
                set.add(Interval(last,end)); 
                state = State.Start;
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
    ///parse and return IR for escape stand alone escape sequence
    Bytecode escape()
    {

        switch(current)
        {
        case 'f':   next(); return Bytecode(IR.Char, '\f');
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
            uint code = parseUniHex(pat, 2);
            next();
            return Bytecode(IR.Char,code);
        case 'u': case 'U':
            uint code = parseUniHex(pat, current == 'u' ? 4 : 8);
            next();
            return Bytecode(IR.Char, code);
        case 'c': //control codes
            Bytecode code = Bytecode(IR.Char, parseControlCode());
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
            while(nref <= index.length && next() && ascii.isDigit(current))
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
        alias comparePropertyName ucmp;
        enum MAX_PROPERTY = 128;
		next() && current == '{' || error("{ expected ");
        char[MAX_PROPERTY] result;
        uint k=0;
        while(k<MAX_PROPERTY && next() && current !='}' && current !=':')
            if(current != '-' && current != ' ' && current != '_')
                result[k++] = cast(char)ascii.toLower(current);
        enforce(k != MAX_PROPERTY, "invalid property name");
		auto s = getUnicodeSet(result[0..k], negated);
		if(s.empty)
            error("unrecognized unicode property spec");
		current == '}' || error("} expected ");
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
    Bytecode[] ir;      // compiled bytecode of pattern
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

/// Simple UTF-string stream abstraction (w/o normalization and such)
struct Input(Char)
    if(is(Char :dchar))
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
    /// codepoint at current stream position
    bool nextChar(ref dchar res,ref size_t pos)
    {
        if(_index == _origin.length)
            return false;
        pos = _index;
        res = std.utf.decode(_origin, _index);
        return true;
    }
    @property size_t lastIndex(){   return _origin.length; }
    
    String opSlice(size_t start, size_t end){   return _origin[start..end]; }
    struct BackLooper
    {
        String _origin;
        size_t _index;
        this(Input input)
        {
            _origin = input._origin;
            _index = input._index;
        }
        bool nextChar(ref dchar res,ref size_t pos)
        {
            if(_index == 0)
                return false;
            _index -= std.utf.strideBack(_origin, _index);
            if(_index == 0)
                return false;
            res = _origin[0.._index].back;
            return true;
        }
    }
    @property auto loopBack(){   return BackLooper(this); }
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
        auto mainStack = (cast(uint*)enforce(malloc(initialStack*uint.sizeof)))
                [0..initialStack];
        scope(exit) free(mainStack.ptr);
        for(;;)
        {
            size_t start = origin.length - s.length;
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
                dchar c = s.front;
                uint len = prog[pc].sequence;
                uint end = pc + len;
                if(prog[pc].data != c && prog[pc+1].data != c)
                {
                    for(pc = pc+2; pc<end; pc++)
                        if(prog[pc].data == c)
                            break;
                    if(pc == end)
                        goto L_backtrack;
                }
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
                if(s.empty || !wordCharacter.contains(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Word);
                break;
            case IR.Notword:
                if(s.empty || wordCharacter.contains(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Word);
                break;
            case IR.Digit:
                if(s.empty || !unicodeNd.contains(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Word);
                break;
            case IR.Notdigit:
                if(s.empty || unicodeNd.contains(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Notdigit);
                break;
            case IR.Space:
                if(s.empty || !unicodeWhite_Space.contains(s.front))
                    goto L_backtrack;
                s.popFront();
                pc += IRL!(IR.Space);
                break;
            case IR.Notspace:
                if(s.empty || unicodeWhite_Space.contains(s.front))
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
             case IR.Nop:
                pc += IRL!(IR.Nop);
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
    dchar front;
    size_t index;
    size_t genCounter;    //merge trace counter, goes up on every dchar
    bool matched;
    bool seenCr;    //true if CR was processed
    /// true if it's start of input
    @property bool atStart(){   return index == 0; }
    /// true if it's end of input
    @property bool atEnd(){  return index == s.lastIndex; }
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
        for(;;)
        {
            seenCr = front == '\r';
            if(!s.nextChar(front, index))
            {
                index =  s.lastIndex;
                break;
            }
            genCounter++;
            debug(fred_matching)
            {
                writefln("Threaded matching %d threads at index %s", clist.length, index);
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
                eval!true(createStart(index), matches);// new thread staring at this position
            else if(nlist.empty)
            {
                debug(fred_matching) writeln("Stopped  matching before consuming full input");
                break;//not a partial match for sure
            }
            clist = nlist;
            nlist = ThreadList.init;
        }
        genCounter++; //increment also on each end
        debug(fred_matching) writefln("Threaded matching %d threads at end", clist.length);
        //try out all zero-width posibilities
        if(!matched)
            eval!false(createStart(index), matches);// new thread starting at end of input
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
        matches[0].end = index;
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
                    dchar back;
                    size_t bi;
                    //at start & end of input
                    if(atStart && wordCharacter.contains(front))
                    {
                        t.pc += IRL!(IR.Wordboundary);
                        break;
                    }
                    else if(atEnd && s.loopBack.nextChar(back, bi)
                            && wordCharacter.contains(back))
                    {
                        t.pc += IRL!(IR.Wordboundary);
                        break;
                    }
                    else if(s.loopBack.nextChar(back, index))
                    {
                        bool af = wordCharacter.contains(front) != 0;
                        bool ab = wordCharacter.contains(back) != 0;
                        if(af ^ ab)
                        {
                            t.pc += IRL!(IR.Wordboundary);
                            break;
                        }
                    }
                    recycle(t);
                    t = worklist.fetch();
                    break;
                case IR.Notwordboundary:
                    dchar back;
                    size_t bi;
                    //at start & end of input
                    if(atStart && !wordCharacter.contains(front))
                    {
                        recycle(t);
                        t = worklist.fetch();
                        break;
                    }
                    else if(atEnd && s.loopBack.nextChar(back, bi)
                            && !wordCharacter.contains(back))
                    {
                        recycle(t);
                        t = worklist.fetch();
                        break;
                    }
                    else if(s.loopBack.nextChar(back, index))
                    {
                        bool af = wordCharacter.contains(front) != 0;
                        bool ab = wordCharacter.contains(back)  != 0;
                        if(af ^ ab)
                        {
                            recycle(t);
                            t = worklist.fetch();
                            break;
                        }    
                    }
                    t.pc += IRL!(IR.Wordboundary);
                    break;
                case IR.Bol:
                    dchar back;
                    size_t bi;
                    //TODO: multiline & attributes, unicode line terminators
                    if(atStart)
                        t.pc += IRL!(IR.Bol);
                    else if(s.loopBack.nextChar(back,bi) && back == '\n') 
                        t.pc += IRL!(IR.Bol);
                    else
                    {
                        recycle(t);
                        t = worklist.fetch();
                    }
                    break;
                case IR.Eol:
                    debug(fred_matching) writefln("EOL (seen CR: %s, front 0x%x) %s", seenCr, front, s[index..s.lastIndex]);
                    //no matching inside \r\n
                    if(atEnd || ((front == '\n') ^ seenCr) || front == LS 
                       || front == PS || front == NEL)
                    {
                        t.pc += IRL!(IR.Eol);
                    }
                    else
                    {
                        recycle(t);
                        t = worklist.fetch();
                    }
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
                                        t.pc, s[index..s.lastIndex], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
                        merge[prog[t.pc + 1].raw+t.counter] = genCounter;
                    }
                    else
                    {
                        debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                        t.pc, s[index..s.lastIndex], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
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
                                        t.pc, s[index..s.lastIndex], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
                        merge[prog[t.pc + 1].raw+t.counter] = genCounter;
                        t.pc += IRL!(IR.OrEnd);
                    }
                    else
                    {
                        debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                        t.pc, s[index..s.lastIndex], genCounter, merge[prog[t.pc + 1].raw+t.counter] );
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
                    t.matches[re.index[n]+1].begin = cast(size_t)index;
                    t.pc += IRL!(IR.GroupStart);
                    //debug(fred_matching)  writefln("IR group #%u starts at %u", n, i);
                    break;
                case IR.GroupEnd:   //TODO: ditto
                    uint n = prog[t.pc].data;
                    t.matches[re.index[n]+1].end = cast(size_t)index;
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
                        uint end = t.matches[n+1].end;
                        if(s[idx..end].front == front)
                        {
                           t.uopCounter += std.utf.stride(s[idx..end], 0);
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
                case IR.Nop:
                    t.pc += IRL!(IR.Nop);
                    break;
                static if(withInput)
                {
                    case IR.OrChar://assumes IRL!(OrChar) == 1
                        uint len = prog[t.pc].sequence;
                        uint end = t.pc + len;
                        for(; t.pc<end; t.pc++)
                            if(prog[t.pc].data == front)
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
                        if(front == prog[t.pc].data)
                        {
                            // debug(fred_matching) writefln("IR.Char %s vs %s ", front, cast(dchar)prog[t.pc].data);
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
                        if(wordCharacter.contains(front))
                        {
                            t.pc += IRL!(IR.Word);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Notword:
                        if(!wordCharacter.contains(front))
                        {
                            t.pc += IRL!(IR.Notword);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Digit:
                        if(unicodeNd.contains(front))
                        {
                            t.pc += IRL!(IR.Digit);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Notdigit:
                        if(!unicodeNd.contains(front))
                        {
                            t.pc += IRL!(IR.Notdigit);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Space:
                        if(unicodeWhite_Space.contains(front))
                        {
                            t.pc += IRL!(IR.Space);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Notspace:
                        if(!unicodeWhite_Space.contains(front))
                        {
                            t.pc += IRL!(IR.Space);
                            nlist.insertBack(t);
                        }
                        else
                            recycle(t);
                        t = worklist.fetch();
                        break;
                    case IR.Charset:
                        if(re.charsets[prog[t.pc].data].contains(front))
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
        _empty = !engine.match(matches);
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
