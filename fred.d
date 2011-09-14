//Written in the D programming language
/++
  $(D FReD) is an acronym for Fast Regular Expressions for D, a proposed replacement for $(D std.regex).

  $(LUCKY Regular expressions) are commonly used method of pattern matching
  on strings, with $(I regex) being a catchy word for a pattern in this domain
  specific language. Typical problems usually solved by regular expressions
  include validation of user input and ubiquitous find & replace in text processing utilities.

  The general usage guideline is keeping regex complexity on the side of simplicity, as its capabilities
  reside in purely character-level manipulation,  and as such are ill suited for tasks
  involving higher level invariants like matching an integer number $(U bounded) in [a,b] interval.
  Checks of this sort of are better addressed by additional postprocessing.

  Synposis:
  ---
  import fred;
  import std.stdio;

  //print out all possible dd/mm/yy(yy) dates found in user input
  //g - global, find all matches
  auto r = regex(r"\b[0-9][0-9]?/[0-9][0-9]?/[0-9][0-9](?:[0-9][0-9])?\b", "g");
  foreach(line; stdin.byLine)
  {
    //match returns a range that can be iterated
    //to get all of subsequent matches
    foreach(c; match(line, r))
        writeln(c.hit);
  }

  ...

  //create static regex at compile-time, contains fast native code
  enum ctr = ctRegex!(`^.*/([^/]+)/?$`);

  //works just like normal regex:
  auto m2 = match("foo/bar", ctr);   //first match found here if any
  assert(m2);   // be sure to check if there is a match, before examining contents!
  assert(m2.captures[1] == "bar");//captures is a range of submatches, 0 - full match

  ...

  //result of match is directly testable with if/assert/while
  //e.g. test if a string consists of letters:
  assert(match("Letter", `^\p{L}+$`));


  ---

  The basic syntax shouldn't surprize experienced users of regular expressions.
  Thankfully, nowdays the web is bustling with resources to help newcomers, and a good reference with tutorial
  could be found on $(WEB www.regular-expressions.info, regular-expressions).
  For specifics search up ECMAScript flavor.

 $(BOOKTABLE Flags controlling the behavior of pattern matching,
    $(TR $(TH Flag) $(TH Effect))
    $(TR $(TD $(B g)) $(TD global; repeat over the whole input ))
    $(TR $(TD $(B i)) $(TD case insensitive))
    $(TR $(TD $(B m)) $(TD multiline, match $ on line separators as well as end of input))
    $(TR $(TD $(B x)) $(TD free-form syntax, ignores whitespace in pattern,
        useful for formating complex regular expressions))
  )

  Advanced Syntax

  Aside from providing a well known features of regular expressions found in e.g. JavaScript,
  FReD does support the following extensions.
  $(UL
    $(LI Named groups, with Python style syntax (?P&lt;name&gt;re),
        with names working like aliases in addition to direct numbers.)
    $(LI Arbitrary length and complexity lookbehind with common syntax
        (?<=re) and (?<!re), including lookahead in lookbehind and vise-versa. )
    $(LI Unicode properities such as Scripts, Blocks and
        common binary properties e.g Alphabetic, White_Space, Hex_Digit etc.)
  )

  Unicode support

  This library provides full Level 1 support* according to
    $(WEB http://unicode.org/reports/tr18/, UTS 18). Specifically:
  $(UL
    $(LI 1.1 Hex notation via any of \uxxxx, \u00YYYYYY, \xZZ.)
    $(LI 1.2 Unicode properties.)
    $(LI 1.3 Charactar classes with set operations.)
    $(LI 1.4 Word boundaries use full set of "word" characters.)
    $(LI 1.5 Using simple casefolding to match case
        insensitevely across full range of codepoints.)
    $(LI 1.6 Respecting line breaks as any of
        \u000A | \u000B | \u000C | \u000D | \u0085 | \u2028 | \u2029 | \u000D\u000A.)
    $(LI 1.7 Operating on codepoint level.)
  )
  *With exception being point 1.1.1, as of yet, normalization of input
    is expected to be enforced by user.

 $(BOOKTABLE Syntax mostly follows technical standard,
   $(TR $(TD \p{PropertyName}, \P{PropertyName}) $(TD unicode property sets,
syntax for unicode blocks is InBlockName))
   $(TR $(TD [a||b], [a--b], [a~~b], [a&&b]) $(TD where a, b
    are arbitrary [...] sets; means union, set difference,
    symmetric set difference, and intersection respectively))
  )

  All matches returned by pattern matching functionality in this library
  are slices of original input. Notable exception being replace family of functions
  that generate new string from input.

  License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).

  Authors: Dmitry Olshansky,

  API and utility constructs are based on original $(D std.regex) by Walter Bright and Andrei Alexandrescu

  Copyright: Copyright Dmitry Olshansky, 2011
 +/

module fred;

import fred_uni;//unicode property tables
import std.array, std.algorithm, std.range,
       std.conv, std.exception, std.traits, std.typetuple,
       std.uni, std.utf, std.format, std.typecons, std.bitmanip,
       std.functional, std.exception, std.regionallocator;
import core.bitop, core.stdc.string;
import ascii = std.ascii;
import std.string : representation;

debug import std.stdio;

private:
@safe:

//uncomment to get a barrage of debug info
//debug = fred_parser;
//debug = fred_matching;
//debug = fred_charset;

// IR bit pattern: 0b1_xxxxx_yy
// where yy indicates class of instruction, xxxxx for actual operation code
//     00: atom, a normal instruction
//     01: open, opening of a group, has length of contained IR in the low bits
//     10: close, closing of a group, has length of contained IR in the low bits
//     11 unused
//
// Loops with Q (non-greedy, with ? mark) must have the same size / other properties as non Q version
// Possible changes:
//* merge group, option, infinite/repeat start (to never copy during parsing of (a|b){1,2})
//* reorganize groups to make n args easier to find, or simplify the check for groups of similar ops
//  (like lookaround), or make it easier to identify hotspots.

enum IR:uint {
    Char              = 0b1_00000_00, //a character
    Any               = 0b1_00001_00, //any character
    CodepointSet           = 0b1_00010_00, //a most generic CodepointSet [...]
    Trie              = 0b1_00011_00, //CodepointSet implemented as Trie
    //match with any of a consecutive OrChar's in this sequence (used for case insensitive match)
    //OrChar holds in upper two bits of data total number of OrChars in this _sequence_
    //the drawback of this representation is that it is difficult to detect a jump in the middle of it
    OrChar            = 0b1_00100_00,
    Nop               = 0b1_00101_00, //no operation (padding)
    End               = 0b1_00110_00, //end of program
    Bol               = 0b1_00111_00, //beginning of a string ^
    Eol               = 0b1_01000_00, //end of a string $
    Wordboundary      = 0b1_01001_00, //boundary of a word
    Notwordboundary   = 0b1_01010_00, //not a word boundary
    Backref           = 0b1_01011_00, //backreference to a group (that has to be pinned, i.e. locally unique) (group index)
    GroupStart        = 0b1_01100_00, //start of a group (x) (groupIndex+groupPinning(1bit))
    GroupEnd          = 0b1_01101_00, //end of a group (x) (groupIndex+groupPinning(1bit))
    Option            = 0b1_01110_00, //start of an option within an alternation x | y (length)
    GotoEndOr         = 0b1_01111_00, //end of an option (length of the rest)
    //... any additional atoms here

    OrStart           = 0b1_00000_01, //start of alternation group  (length)
    OrEnd             = 0b1_00000_10, //end of the or group (length,mergeIndex)
    //with this instruction order
    //bit mask 0b1_00001_00 could be used to test/set greediness
    InfiniteStart     = 0b1_00001_01, //start of an infinite repetition x* (length)
    InfiniteEnd       = 0b1_00001_10, //end of infinite repetition x* (length,mergeIndex)
    InfiniteQStart    = 0b1_00010_01, //start of a non eager infinite repetition x*? (length)
    InfiniteQEnd      = 0b1_00010_10, //end of non eager infinite repetition x*? (length,mergeIndex)
    RepeatStart       = 0b1_00011_01, //start of a {n,m} repetition (length)
    RepeatEnd         = 0b1_00011_10, //end of x{n,m} repetition (length,step,minRep,maxRep)
    RepeatQStart      = 0b1_00100_01, //start of a non eager x{n,m}? repetition (length)
    RepeatQEnd        = 0b1_00100_10, //end of non eager x{n,m}? repetition (length,step,minRep,maxRep)
    //
    LookaheadStart    = 0b1_00101_01, //begin of the lookahead group (length)
    LookaheadEnd      = 0b1_00101_10, //end of a lookahead group (length)
    NeglookaheadStart = 0b1_00110_01, //start of a negative lookahead (length)
    NeglookaheadEnd   = 0b1_00110_10, //end of a negative lookahead (length)
    LookbehindStart   = 0b1_00111_01, //start of a lookbehind (length)
    LookbehindEnd     = 0b1_00111_10, //end of a lookbehind (length)
    NeglookbehindStart= 0b1_01000_01, //start of a negative lookbehind (length)
    NeglookbehindEnd  = 0b1_01000_10, //end of negative lookbehind (length)
}

//a shorthand for IR length - full length of specific opcode evaluated at compile time
template IRL(IR code)
{
    enum uint IRL =  lengthOfIR(code);
}

static assert (IRL!(IR.LookaheadStart) == 3);

//how many parameters follow the IR, should be optimized fixing some IR bits
int immediateParamsIR(IR i){
    switch (i){
    case IR.OrEnd,IR.InfiniteEnd,IR.InfiniteQEnd:
        return 1;
    case IR.RepeatEnd, IR.RepeatQEnd:
        return 4;
    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
        return 2;
    default:
        return 0;
    }
}

//full length of IR instruction inlcuding all parameters that might follow it
int lengthOfIR(IR i)
{
    return 1 + immediateParamsIR(i);
}

//full length of the paired IR instruction inlcuding all parameters that might follow it
int lengthOfPairedIR(IR i)
{
    return 1 + immediateParamsIR(pairedIR(i));
}

//if the operation has a merge point (this relies on the order of the ops)
bool hasMerge(IR i)
{
    return (i&0b11)==0b10 && i<=IR.RepeatQEnd;
}

//is an IR that opens a "group"
bool isStartIR(IR i)
{
    return (i&0b11)==0b01;
}

//is an IR that ends a "group"
bool isEndIR(IR i)
{
    return (i&0b11)==0b10;
}

//is a standalone IR
bool isAtomIR(IR i)
{
    return (i&0b11)==0b00;
}

//makes respective pair out of IR i, swapping start/end bits of instruction
IR pairedIR(IR i)
{
    assert(isStartIR(i) || isEndIR(i));
    return cast(IR)(i ^ 0b11);
}

//encoded IR instruction
struct Bytecode
{
    uint raw;
    //natural constraints
    enum maxSequence = 2+4;
    enum maxData = 1<<22;
    enum maxRaw = 1<<31;

    this(IR code, uint data)
    {
        assert(data < (1<<22) && code < 256);
        raw = code<<24 | data;
    }

    this(IR code, uint data, uint seq)
    {
        assert(data < (1<<22) && code < 256 );
        assert(seq >= 2 && seq < maxSequence);
        raw = code<<24 | ((seq-2)<<22) | data;
    }

    //store raw data
    static Bytecode fromRaw(uint data)
    {
        Bytecode t;
        t.raw = data;
        return t;
    }

    //bit twiddling helpers
    @property uint data() const { return raw & 0x003f_ffff; }

    //ditto
    @property uint sequence() const { return 2+((raw >>22) & 0x3); }

    //ditto
    @property IR code() const { return cast(IR)(raw>>24); }

    //ditto
    @property bool hotspot() const { return hasMerge(code); }

    //test the class of this instruction
    @property bool isAtom() const { return isAtomIR(code); }

    //ditto
    @property bool isStart() const { return isStartIR(code); }

    //ditto
    @property bool isEnd() const { return isEndIR(code); }

    //number of arguments for this instruction
    @property int args() const { return immediateParamsIR(code); }

    //mark this GroupStart or GroupEnd as referenced in backreference
    void setBackrefence()
    {
        assert(code == IR.GroupStart || code == IR.GroupEnd);
        raw = raw | (1<<23);
    }

    //is referenced
    @property bool backreference() const
    {
        assert(code == IR.GroupStart || code == IR.GroupEnd);
        return cast(bool)(raw & (1<<23));
    }

    //mark as local reference (for backrefs in lookarounds)
    void setLocalRef()
    {
        assert(code == IR.Backref);
        raw = raw | (1<<23);
    }

    //is a local ref
    @property bool localRef() const
    {
        assert(code == IR.Backref);
        return cast(bool)(raw & (1<<23));
    }

    //human readable name of instruction
    @trusted @property string mnemonic() const
    {//@@@BUG@@@ to is @system
        return to!string(code);
    }

    //full length of instruction
    @property uint length() const
    {
        return lengthOfIR(code);
    }

    //full length of respective start/end of this instruction
    @property uint pairedLength() const
    {
        return lengthOfPairedIR(code);
    }

    //returns bytecode of paired instruction (assuming this one is start or end)
    @property Bytecode paired() const
    {//depends on bit and struct layout order
        assert(isStart || isEnd);
        return Bytecode.fromRaw(raw ^ (0b11<<24));
    }

    //gets an index into IR block of the respective pair
    uint indexOfPair(uint pc) const
    {
        assert(isStart || isEnd);
        return isStart ? pc + data + length  : pc - data - lengthOfPairedIR(code);
    }
}

static assert(Bytecode.sizeof == 4);

//debugging tool, prints out instruction along with opcodes
@trusted string disassemble(in Bytecode[] irb, uint pc, in NamedGroup[] dict=[])
{
    auto output = appender!string();
    formattedWrite(output,"%s", irb[pc].mnemonic);
    switch(irb[pc].code)
    {
    case IR.Char:
        formattedWrite(output, " %s (0x%x)",cast(dchar)irb[pc].data, irb[pc].data);
        break;
    case IR.OrChar:
        formattedWrite(output, " %s (0x%x) seq=%d", cast(dchar)irb[pc].data, irb[pc].data, irb[pc].sequence);
        break;
    case IR.RepeatStart, IR.InfiniteStart, IR.Option, IR.GotoEndOr, IR.OrStart:
        //forward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc+len+IRL!(IR.RepeatStart));
        break;
    case IR.RepeatEnd, IR.RepeatQEnd: //backward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u min=%u max=%u step=%u"
                , pc-len, irb[pc+3].raw, irb[pc+4].raw, irb[pc+2].raw);
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
        string name;
        foreach(v;dict)
            if(v.group == n)
            {
                name = "'"~v.name~"'";
                break;
            }
        formattedWrite(output, " %s #%u " ~ (irb[pc].backreference ? "referenced" : ""),
                name, n);
        break;
    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
        uint len = irb[pc].data;
        uint start = irb[pc+1].raw, end = irb[pc+2].raw;
        formattedWrite(output, " pc=>%u [%u..%u]", pc + len + IRL!(IR.LookaheadStart), start, end);
        break;
    case IR.Backref: case IR.CodepointSet: case IR.Trie:
        uint n = irb[pc].data;
        formattedWrite(output, " %u",  n);
        if(irb[pc].code == IR.Backref)
            formattedWrite(output, " %s", irb[pc].localRef ? "local" : "global");
        break;
    default://all data-free instructions
    }
    if(irb[pc].hotspot)
        formattedWrite(output, " Hotspot %u", irb[pc+1].raw);
    return output.data;
}

//another pretty printer, writes out the bytecode of a regex and where the pc is
@trusted void prettyPrint(Sink,Char=const(char))(Sink sink,const(Bytecode)[] irb, uint pc=uint.max,int indent=3,size_t index=0)
    if (isOutputRange!(Sink,Char))
{//formattedWrite is @system
    while(irb.length>0)
    {
        formattedWrite(sink,"%3d",index);
        if(pc==0 && irb[0].code!=IR.Char)
        {
            for (int i=0;i<indent-2;++i)
                put(sink,"=");
            put(sink,"> ");
        }
        else
        {
            if(isEndIR(irb[0].code))
            {
                indent-=2;
            }
            if(indent>0)
            {
                string spaces="             ";
                put(sink,spaces[0..(indent%spaces.length)]);
                for (size_t i=indent/spaces.length;i>0;--i)
                    put(sink,spaces);
            }
        }
        if(irb[0].code==IR.Char)
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
        }
        else
        {
            put(sink,irb[0].mnemonic);
            put(sink,"(");
            formattedWrite(sink,"%d",irb[0].data);
            int nArgs= irb[0].args;
            for(int iarg=0;iarg<nArgs;++iarg)
            {
                if(iarg+1<irb.length)
                    formattedWrite(sink,",%d",irb[iarg+1].data);
                else
                    put(sink,"*error* incomplete irb stream");
            }
            put(sink,")");
            if(isStartIR(irb[0].code))
            {
                indent+=2;
            }
            index+=lengthOfIR(irb[0].code);
            irb=irb[lengthOfIR(irb[0].code)..$];
        }
        put(sink,"\n");
    }
}

//wrappers for CTFE
@trusted void insertInPlaceAlt(T)(ref T[] arr, size_t idx, T[] items...)
{
    if(__ctfe)
        arr = arr[0..idx] ~ items ~ arr[idx..$];
    else
        insertInPlace(arr, idx, items);
}

//ditto
@trusted void replaceInPlaceAlt(T)(ref T[] arr, size_t from, size_t to, T[] items...)
{
    //if(__ctfe)
        arr = arr[0..from]~items~arr[to..$];
    /*else //@@@BUG@@@ in replaceInPlace? symptoms being sudden ZEROs in array
        replaceInPlace(arr, from, to, items);*/
}

//ditto
@trusted void moveAllAlt(T)(T[] src, T[] dest)
{//moveAll is @system
    if(__ctfe)
        foreach(i,v; src)
            dest[i] = v;
    else
        moveAll(src, dest);
}

//default allocator to use
alias RegionAllocator Allocator;

//Regular expression engine/parser options:
// global - search  all nonoverlapping matches in input
// casefold - case insensitive matching, do casefolding on match in unicode mode
// freeform - ignore whitespace in pattern, to match space use [ ] or \s
// multiline - switch  ^, $ detect start and end of linesinstead of just start and end of input
enum RegexOption: uint { global = 0x1, casefold = 0x2, freeform = 0x4, nonunicode = 0x8, multiline = 0x10 };
alias TypeTuple!('g', 'i', 'x', 'U', 'm') RegexOptionNames;//do not reorder this list
static assert( RegexOption.max < 0x80);
enum RegexInfo : uint { oneShot = 0x80 };

private enum NEL = '\u0085', LS = '\u2028', PS = '\u2029';

//test if a given string starts with hex number of maxDigit that's a valid codepoint
//returns it's value and skips these maxDigit chars on success, throws on failure
dchar parseUniHex(Char)(ref Char[] str, uint maxDigit)
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

//index entry structure for name --> number of submatch
struct NamedGroup
{
    string name;
    uint group;
}

//holds pair of start-end markers for a submatch
struct Group(DataIndex)
{
    DataIndex begin, end;
    @trusted string toString() const
    {
        auto a = appender!string();
        formattedWrite(a, "%s..%s", begin, end);
        return a.data;
    }
}

//$(D Interval)  represents an interval of codepoints: [a,b).
public struct Interval
{
    uint begin, end;

    ///Create interval containig a single character $(D ch).
    this(dchar ch)
    {
        begin = ch;
        end = ch+1;
    }

    /++
        Create Interval from inclusive range [$(D a),$(D b)]. Contrary to internal structure, inclusive is chosen for interface.
        The reason for this is usability e.g. it's would force user to type the unwieldy Interval('a','z'+1) all over the place.
    +/
    this(dchar a, dchar b)
    {
        assert(a <= b);
        begin = a;
        end = b+1;
    }

    ///
    @trusted string toString()const
    {
        auto s = appender!string;
        formattedWrite(s,"%s(%s)..%s(%s)",
                       begin, ascii.isGraphical(begin) ? to!string(cast(dchar)begin) : "",
                       end, ascii.isGraphical(end) ? to!string(cast(dchar)end) : "");
        return s.data;
    }

}

/++
    $(D CodepointSet) is a data structure for manipulating sets
    of Unicode codepoints in an efficient manner.
    Instances of CodepointSet have half-reference semantics akin to dynamic arrays,
    to obtain a unique copy use $(D dup).
+/
public struct CodepointSet
{
private:
    enum uint endOfRange = 0x110000;
    uint[] ivals;

public:
    ///Add an $(D interval) of codepoints to this set.
    @trusted ref CodepointSet add(Interval inter)
    {
        debug(fred_charset) writeln("Inserting ",inter);
        if(ivals.empty)
        {
            insertInPlaceAlt(ivals, 0, inter.begin, inter.end);
            return this;
        }//assumeSorted is @system
        auto svals = assumeSorted(ivals);
        auto s = svals.lowerBound(inter.begin).length;
        auto e = s+svals[s..svals.length].lowerBound(inter.end).length;
        debug(fred_charset)  writeln("Indexes: ", s,"  ", e);
        if(s & 1)
        {
            inter.begin = ivals[s-1];
            s ^= 1;
        }
        if(e & 1)
        {
            inter.end = ivals[e];
            e += 1;
        }
        else //e % 2 == 0
        {
            if(e < ivals.length && inter.end == ivals[e])
            {
                    inter.end = ivals[e+1];
                    e+=2;
            }
        }
        debug(fred_charset)
            for(size_t i=1;i<ivals.length; i++)
                assert(ivals[i-1] < ivals[i]);
        replaceInPlaceAlt(ivals, s, e, inter.begin ,inter.end);
        return this;
    }

    ///Add a codepoint $(D ch) to this set.
    ref CodepointSet add(dchar ch){ add(Interval(cast(uint)ch)); return this; }

    ///Add $(D set) in this set.
    ///Algebra: this = this | set.
    ref CodepointSet add(in CodepointSet set)
    {
        debug(fred_charset) writef ("%s || %s --> ", ivals, set.ivals);
        for(size_t i=0; i<set.ivals.length; i+=2)
            add(Interval(set.ivals[i], set.ivals[i+1]-1));
        debug(fred_charset) writeln(ivals);
        return this;
    }
    ///Exclude $(D set) from this set.
    ///Algebra: this = this - set.
    @trusted ref CodepointSet sub(in CodepointSet set)
    {
        if(empty)
        {
            ivals = [];
            return this;
        }
        if(set.empty)
            return this;
        auto a = cast(Interval[])ivals;
        auto b = cast(const(Interval)[])set.ivals;
        Interval[] result;
        while(!a.empty && !b.empty)
        {
            if(a.front.end < b.front.begin)
            {
                result ~= a.front;
                a.popFront();
            }
            else if(a.front.begin > b.front.end)
            {
                b.popFront();
            }
            else //there is an intersection
            {
                if(a.front.begin < b.front.begin)
                {
                    result ~= Interval(a.front.begin, b.front.begin-1);
                    if(a.front.end < b.front.end)
                    {
                        a.popFront();
                    }
                    else if(a.front.end > b.front.end)
                    {
                        //adjust a in place
                        a.front.begin = b.front.end;
                        if(a.front.begin >= a.front.end)
                            a.popFront();
                        b.popFront();
                    }
                    else //==
                    {
                        a.popFront();
                        b.popFront();
                    }
                }
                else //a.front.begin > b.front.begin
                {//adjust in place
                    if(a.front.end < b.front.end)
                    {
                        a.popFront();
                    }
                    else
                    {
                        a.front.begin = b.front.end;
                        if(a.front.begin >= a.front.end)
                            a.popFront();
                        b.popFront();
                    }
                }
            }
        }
        result ~= a;//+ leftover of original
        ivals = cast(uint[])result;
        return this;
    }
    ///Make this set a symmetric difference with $(D set).
    ///Algebra: this = this ~ set (i.e. (this || set) -- (this && set)).
    @trusted ref symmetricSub(in CodepointSet set)
    {
        auto a = CodepointSet(ivals.dup);
        a.intersect(set);
        this.add(set);
        this.sub(a);
        return this;
    }
    ///Intersect this set with $(D set).
    ///Algebra: this = this & set
    @trusted ref CodepointSet intersect(in CodepointSet set)
    {
        if(empty || set.empty)
        {
            ivals = [];
            return this;
        }
        Interval[] intersection;
        auto a = cast(const(Interval)[])ivals;
        auto b = cast(const(Interval)[])set.ivals;
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
        ivals = cast(uint[])intersection;
        return this;
    }

    //this = !this (i.e. [^...] in regex syntax)
    @trusted ref CodepointSet negate()
    {
        if(empty)
        {
            insertInPlaceAlt(ivals, 0, 0u, endOfRange);
            return this;
        }
        if(ivals[0] != 0)
            insertInPlaceAlt(ivals, 0, 0u);
        else
        {
            for(size_t i=1; i<ivals.length; i++)
                ivals[i-1] = ivals[i];//moveAll(ivals[1..$], ivals[0..$-1]);
            ivals = ivals[0..$-1];
            if(!__ctfe)
                assumeSafeAppend(ivals);
        }
        if(ivals[$-1] != endOfRange)
            insertInPlaceAlt(ivals, ivals.length, endOfRange);
        else
        {
            ivals = ivals[0..$-1] ;
            if(!__ctfe)
                assumeSafeAppend(ivals);
        }
        assert(!(ivals.length & 1));
        return this;
    }

    /**
        Test if ch is present in this set, linear search done in $(BIGOH N) operations
        on number of $(U intervals) in this set.
        In practice linear search outperforms binary search until a certain threshold.
        Unless number of elements is known to be small in advance it's recommended
        to use overloaded indexing operator.
    */
    bool scanFor(dchar ch) const
    {
        //linear search is in fact faster (given that length is fixed under threshold)
        for(size_t i=1; i<ivals.length; i+=2)
            if(ch < ivals[i])
                return ch >= ivals[i-1];
        return false;
    }

    /**
        Test if ch is present in this set, in $(BIGOH LogN) operations on number
        of $(U intervals) in this set.
    */
    @trusted bool opIndex(dchar ch)const
    {
        auto svals = assumeSorted!"a <= b"(ivals);
        auto s = svals.lowerBound(cast(uint)ch).length;
        return s & 1;
    }

    ///Test if this set is empty.
    @property bool empty() const {   return ivals.empty; }

    ///Write out in regular expression style [\uxxxx-\uyyyy...].
    @trusted void printUnicodeSet(R)(R sink) const
        if(isOutputRange!(R, const(char)[]))
    {
        sink("[");
        for(uint i=0;i<ivals.length; i+=2)
            if(ivals[i] + 1 == ivals[i+1])
                formattedWrite(sink, "\\U%08x", ivals[i]);
            else
                formattedWrite(sink, "\\U%08x-\\U%08x", ivals[i], ivals[i+1]-1);
        sink("]");
    }

    ///Deep copy this set.
    @property CodepointSet dup() const
    {
        return CodepointSet(ivals.dup);
    }

    ///Full covered length from first codepoint to the last one.
    @property uint extent() const
    {
        return ivals.empty ? 0 : ivals[$-1] - ivals[0];
    }

    ///Number of codepoints stored in this set.
    @property uint chars() const
    {
        //CTFE workaround
        uint ret;
        for(uint i=0; i<ivals.length; i+=2)
            ret += ivals[i+1] - ivals[i];
        return ret;
    }

    ///Troika for built-in hash maps.
    bool opEquals(ref const CodepointSet set) const
    {
        return ivals == set.ivals;
    }

    ///ditto
    int opCmp(ref const CodepointSet set) const
    {
        return cmp(cast(const(uint)[])ivals, cast(const(uint)[])set.ivals);
    }

    ///ditto
    hash_t toHash() const
    {
        hash_t hash = 5381+7*ivals.length;
        if(!empty)
            hash = 31*ivals[0] + 17*ivals[$-1];
        return hash;
    }

    struct ByCodepoint
    {
        const(uint)[] ivals;
        uint j;
        this(in CodepointSet set)
        {
            ivals = set.ivals;
            if(!empty)
                j = ivals[0];
        }
        @property bool empty() const { return ivals.empty; }
        @property uint front() const
        {
            assert(!empty);
            return j;
        }
        void popFront()
        {
            assert(!empty);
            if(++j >= ivals[1])
            {
                ivals = ivals[2..$];
                if(!empty)
                    j = ivals[0];
            }
        }
        auto ref save() const { return this; }
    }
    static assert(isForwardRange!ByCodepoint);

    ///Forward range of all codepoints in this set.
    auto opSlice() const
    {
        return ByCodepoint(this);
    }

    ///Random access range of intervals in this set.
    @trusted @property auto byInterval() const
    {
        return cast(const(Interval)[])ivals;
    }
    //eaten alive by @@@BUG@@@s
    /+invariant()
    {
        assert(ivals.length % 2 == 0);
        for(size_t i=1; i<ivals.length; i++)
            assert(ivals[i-1] < ivals[i]);
    }+/
}

/**
    $(D CodepointTrie) is 1-level  $(LUCKY Trie) of codepoints.
    Primary use case is to convert a previously obtained CodepointSet
    in order to speed up subsequent element lookup.

    ---
        auto input = ...;
        Charset set;
        set.add(unicodeAlphabetic).add('$').add('#');
        auto lookup = CodepointTrie!8(set);
        int count;
        foreach(dchar ch; input)
            if(lookup[ch])
                count++;
    ---
    $(D prefixBits) parameter controls number of bits used to index last level
    and provided for tuning to a specific applications.
    A default parameter of 8 works best in common cases though.
*/
struct CodepointTrie(uint prefixBits)
    if(prefixBits > 4)
{
	static if(size_t.sizeof == 4)
		enum unitBits = 2;
	else static if(size_t.sizeof == 8)
		enum unitBits = 3;
	else
		static assert(0);
    enum prefixWordBits = prefixBits-unitBits, prefixSize=1<<prefixBits,
        prefixWordSize = 1<<(prefixWordBits),
        bitTestShift = prefixBits+3, prefixMask = (1<<prefixBits)-1;
    size_t[] data;
    ushort[] indexes;
    bool negative;

    //debugging tool
    @trusted debug static void printBlock(in size_t[] block)
    {//@@@BUG@@@ write is @system
        for(uint k=0; k<prefixSize; k++)
        {
            if((k & 15) == 0)
                write(" ");
            if((k & 63) == 0)
                writeln();
            writef("%d", bt(block.ptr, k) != 0);
        }
        writeln();
    }

    //ditto
    @trusted debug void desc() const
    {//@@@BUG@@@ writeln is @system
        writeln(indexes);
        writeln("***Blocks***");
        for(uint i=0; i<data.length; i+=prefixWordSize)
        {
            printBlock(data[i .. i+prefixWordSize]);
            writeln("---");
        }
    }

public:
    ///Create a trie from CodepointSet $(D set).
    @trusted this(in CodepointSet s)
    {
        if(s.empty)
            return;
        const(CodepointSet) set = s.chars > 500_000 ? (negative=true, s.dup.negate) : s;
        uint bound = 0;//set up on first iteration
        ushort emptyBlock = ushort.max;
        auto ivals  = set.ivals;
        size_t[prefixWordSize] page;
        for(uint i=0; i<CodepointSet.endOfRange; i+= prefixSize)
        {
            if(i+prefixSize > ivals[bound] || emptyBlock == ushort.max)//avoid empty blocks if we have one already
            {
                bool flag = true;
            L_Prefix_Loop:
                for(uint j=0; j<prefixSize; j++)
                {
                    while(i+j >= ivals[bound+1])
                    {
                        bound += 2;
                        if(bound == ivals.length)
                        {
                            bound = uint.max;
                            if(flag)//not a single one set so far
                                return;
                            //no more bits in the whole set, but need to add the last bucket
                            break L_Prefix_Loop;
                        }
                    }
                    if(i+j >= ivals[bound])
                    {
						enum mask = (1<<(3+unitBits))-1;
                        page[j>>(3+unitBits)]
							|=  cast(size_t)1<<(j & mask);
                        flag = false;
                    }
                }

                debug(fred_trie)
                {
                   printBlock(page);
                }
                uint npos;
                for(npos=0;npos<data.length;npos+=prefixWordSize)
                    if(equal(page[], data[npos .. npos+prefixWordSize]))
                    {
                        indexes ~= cast(ushort)(npos>>prefixWordBits);
                        break;
                    }
                if(npos == data.length)
                {
                    indexes ~= cast(ushort)(data.length>>prefixWordBits);
                    data ~= page;
                    if(flag)
                        emptyBlock = indexes[$-1];
                }
                if(bound == uint.max)
                    break;
                page[] = 0;
            }
            else//fast reroute whole blocks to an empty one
            {
                indexes ~= emptyBlock;
            }
        }
    }

    ///Test if contains $(D ch).
    @trusted bool opIndex(dchar ch) const
    {
        assert(ch < 0x110000);
        uint ind = ch>>prefixBits;
        if(ind >= indexes.length)
            return negative;
        return cast(bool)bt(data.ptr, (indexes[ind]<<bitTestShift)+(ch&prefixMask)) ^ negative;
    }

    //invert trie (trick internal for regular expressions, has aliasing problem)
    @trusted private auto negated() const
    {
        CodepointTrie t = cast(CodepointTrie)this;//shallow copy, need to subvert type system?
        t.negative = !negative;
        return t;
    }
}
//heuristic value determines maximum CodepointSet length suitable for linear search
enum maxCharsetUsed = 6;

alias CodepointTrie!8 Trie;

Trie[const(CodepointSet)] trieCache;

//accessor with caching
Trie getTrie(in CodepointSet set)
{
    if(__ctfe)
        return Trie(set);
    else
    {
        auto p = set in trieCache;
        if(p)
            return *p;
        return (trieCache[set] = Trie(set));
    }
}

version(fred_trie_test)
unittest//a very sloow test
{
    uint max_char, max_data;
    Trie t;
    t = wordTrie;
    assert(t['a']);
    assert(!t[' ']);
    CodepointSet set;
    set.add(unicodeAlphabetic);
    for(size_t i=1;i<set.ivals.length; i++)
        assert(set.ivals[i-1] < set.ivals[i],text(set.ivals[i-1], "  ",set.ivals[i]));
    t = wordTrie;
    assert(t['a']);
    assert(!t[' ']);
    foreach(up; unicodeProperties)
    {
        t = Trie(up.set);
        foreach(uint ch; up.set[])
            assert(t[ch], text("on ch ==", ch));
        auto s = up.set.dup.negate.negate;
        assert(equal(cast(immutable(Interval)[])s.ivals, cast(immutable(Interval)[])up.set.ivals));
        foreach(ch; up.set.dup.negate[])
        {
            assert(!t[ch], text("negative on ch ==", ch));
        }
    }
}

//fussy compare for unicode property names as per UTS-18
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

//ditto (workaround for internal tools)
public bool propertyNameLess(Char)(const(Char)[] a, const(Char)[] b)
{
	return comparePropertyName(a, b) < 0;
}

unittest
{
    assert(comparePropertyName("test","test") == 0);
    assert(comparePropertyName("Al chemical Symbols", "Alphabetic Presentation Forms") == -1);
    assert(comparePropertyName("Basic Latin","basic-LaTin") == 0);
}

//Gets array of all of common case eqivalents of given codepoint (fills provided array & returns a slice of it)
@trusted dchar[] getCommonCasing(dchar ch, dchar[] range)
{
    CommonCaseEntry cs;
    size_t i=1, j=0;
    range[0] = ch;
    while(j < i)
    {
        ch = range[j++];
        cs.start = ch;
        cs.end = ch;
        auto idx = assumeSorted!"a.end <= b.end"(commonCaseTable)
            .lowerBound(cs).length;
        immutable(CommonCaseEntry)[] slice = commonCaseTable[idx..$];
        idx = assumeSorted!"a.start <= b.start"(slice).lowerBound(cs).length;
        slice = slice[0..idx];
        foreach(v; slice)
            if(ch < v.end)
            {
                if(v.xor)
                {
                    auto t = ch ^ v.delta;
                    if(countUntil(range[0..i], t) < 0)
                        range[i++] = t;
                }
                else
                {
                    auto t =  v.neg ? ch - v.delta : ch + v.delta;
                    if(countUntil(range[0..i], t) < 0)
                        range[i++] = t;
                }
            }
    }
    return range[0..i];
}

unittest
{
    dchar[6] data;
    //these values give 100% code coverage for getCommonCasing
    assert(getCommonCasing(0x01BC, data) == [0x01bc, 0x01bd]);
    assert(getCommonCasing(0x03B9, data) == [0x03b9, 0x0399, 0x1fbe, 0x0345]);
    assert(getCommonCasing(0x10402, data) == [0x10402, 0x1042a]);
}

//
@trusted CodepointSet caseEnclose(in CodepointSet set)
{
    CodepointSet n;
    for(size_t i=0;i<set.ivals.length; i+=2)
    {
        CommonCaseEntry cs;
        cs.start = set.ivals[i+1]-1;
        cs.end = set.ivals[i];
        auto idx = assumeSorted!"a.end <= b.end"(commonCaseTable)
            .lowerBound(cs).length;
        immutable(CommonCaseEntry)[] slice = commonCaseTable[idx..$];
        idx = assumeSorted!"a.start <= b.start"(slice)
            .lowerBound(cs).length;
        slice = slice[0..idx];
        if(!slice.empty)
        {
            dchar[6] r;
            for(uint ch = set.ivals[i]; ch <set.ivals[i+1]; ch++)
            {
                auto rng = getCommonCasing(ch, r[]);
                foreach(v; rng)
                    n.add(v);
            }
        }
        else
            n.add(Interval(cs.end,cs.start));
    }
    return n;
}

//property for \w character class
@property CodepointSet wordCharacter()
{
    return memoizeExpr!("CodepointSet.init.add(unicodeAlphabetic).add(unicodeMn).add(unicodeMc)
        .add(unicodeMe).add(unicodeNd).add(unicodePc)")();
}

@property Trie wordTrie()
{
    return memoizeExpr!("Trie(wordCharacter)")();
}

auto memoizeExpr(string expr)()
{
    if(__ctfe)
        return mixin(expr);
    alias typeof(mixin(expr)) T;
    static T slot;
    if(slot == T.init)
        slot =  mixin(expr);
    return slot;
}

/+
    fetch codepoint set corresponding to a name (InBlock or binary property)
+/
@trusted const(CodepointSet) getUnicodeSet(in char[] name, bool negated,  bool casefold)
{
    alias comparePropertyName ucmp;
    CodepointSet s;

    //unicode property
    //helper: direct access with a sanity check
    if(ucmp(name, "L") == 0 || ucmp(name, "Letter") == 0)
    {
        s.add(unicodeLu).add(unicodeLl).add(unicodeLt)
            .add(unicodeLo).add(unicodeLm);
    }
    else if(ucmp(name,"LC") == 0 || ucmp(name,"Cased Letter")==0)
    {
        s.add(unicodeLl).add(unicodeLu).add(unicodeLt);//Title case
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
            s = cast(CodepointSet)unicodeProperties[key].set;
        }
        else
        {
            auto range = assumeSorted!((x,y){ return ucmp(x.name, y.name) < 0; })(unicodeProperties);
            auto eq = range.lowerBound(UnicodeProperty(cast(string)name,CodepointSet.init)).length;//TODO: hackish
            enforce(eq!=range.length && ucmp(name,range[eq].name)==0,"invalid property name");
            s = range[eq].set.dup;
        }
    }

    if(casefold)
        s = caseEnclose(s);
    if(negated)
        s.negate();
    return cast(const CodepointSet)s;
}

//basic stack, just in case it gets used anywhere else then Parser
@trusted struct Stack(T, bool CTFE=false)
{
    static if(!CTFE)
        Appender!(T[]) stack;//compiles but bogus at CTFE
    else
    {
        struct Proxy
        {
            T[] data;
            void put(T val)
            {
                data ~= val;
            }
            void shrinkTo(size_t sz){   data = data[0..sz]; }
        }
        Proxy stack;
    }
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
    @property size_t length() {  return stack.data.length; }
    T pop()
    {
        assert(!empty);
        auto t = stack.data[$-1];
        stack.shrinkTo(stack.data.length-1);
        return t;
    }
}

//safety limits
enum maxGroupNumber = 2^^19;
enum maxLookaroundDepth = 16;
// *Bytecode.sizeof, i.e. 1Mb of bytecode alone
enum maxCompiledLength = 2^^18;
//amounts to up to 4 Mb of auxilary table for matching
enum maxCumulativeRepetitionLength = 2^^20;

template BasicElementOf(Range)
{
    alias Unqual!(ElementEncodingType!Range) BasicElementOf;
}

struct Parser(R, bool CTFE=false)
    if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;       //keep full pattern for pretty printing error messages
    Bytecode[] ir;       //resulting bytecode
    uint re_flags = 0;   //global flags e.g. multiline + internal ones
    Stack!(uint, CTFE) fixupStack;  //stack of opened start instructions
    NamedGroup[] dict;   //maps name -> user group number
    //current num of group, group nesting level and repetitions step
    Stack!(uint,CTFE) groupStack;
    uint nesting = 0;
    uint lookaroundNest = 0;
    uint counterDepth = 0; //current depth of nested counted repetitions
    const(CodepointSet)[] charsets;  //
    const(Trie)[] tries; //
    uint[] backrefed; //bitarray for groups

    @trusted this(S)(R pattern, S flags)
        if(isSomeString!S)
    {
        pat = origin = pattern;
        if(!__ctfe)
            ir.reserve(pat.length);
        parseFlags(flags);
        _current = ' ';//a safe default for freeform parsing
        next();
        if(__ctfe)
            parseRegex();
        else
        {
            try
            {
                parseRegex();
            }
            catch(Exception e)
            {
                error(e.msg);//also adds pattern location
            }
        }
        put(Bytecode(IR.End, 0));

    }

    //mark referenced groups for latter processing
    void markBackref(uint n)
    {
        if(n/32 >= backrefed.length)
            backrefed.length = n/32 + 1;
        backrefed[n/32] |= 1<<(n & 31);
    }

    @property dchar current(){ return _current; }

    bool _next()
    {
        if(pat.empty)
        {
            empty =  true;
            return false;
        }
        //for CTFEability
        size_t idx=0;
        _current = decode(pat, idx);
        pat = pat[idx..$];
        return true;
    }

    void skipSpace()
    {
        while(isWhite(current) && _next()){ }
    }

    bool next()
    {
        if(re_flags & RegexOption.freeform)
        {
            bool r = _next();
            skipSpace();
            return r;
        }
        else
            return _next();
    }

    void put(Bytecode code)
    {
        enforce(ir.length < maxCompiledLength, "maximum compiled pattern length is exceeded");
        if(__ctfe)
        {
            ir = ir ~ code;
        }
        else
            ir ~= code;
    }

    void putRaw(uint number)
    {
        enforce(ir.length < maxCompiledLength, "maximum compiled pattern length is exceeded");
        ir ~= Bytecode.fromRaw(number);
    }

    //parsing number with basic overflow check
    uint parseDecimal()
    {
        uint r=0;
        while(ascii.isDigit(current))
        {
            if(r >= (uint.max/10))
                error("Overflow in decimal number");
            r = 10*r + cast(uint)(current-'0');
            if(!next())
                break;
        }
        return r;
    }

    //parse control code of form \cXXX, c assumed to be the current symbol
    dchar parseControlCode()
    {
        enforce(next(), "Unfinished escape sequence");
        enforce(('a' <= current && current <= 'z') || ('A' <= current && current <= 'Z'),
            "Only letters are allowed after \\c");
        return current & 0x1f;
    }

    //
    @trusted void parseFlags(S)(S flags)
    {//@@@BUG@@@ text is @system
        foreach(ch; flags)//flags are ASCII anyway
        {
        L_FlagSwitch:
            switch(ch)
            {

                foreach(i, op; __traits(allMembers, RegexOption))
                {
                    case RegexOptionNames[i]:
                            if(re_flags & mixin("RegexOption."~op))
                                throw new RegexException(text("redundant flag specified: ",ch));
                            re_flags |= mixin("RegexOption."~op);
                            break L_FlagSwitch;
                }
                default:
                    if(__ctfe)
                       assert(text("unknown regex flag '",ch,"'"));
                    else
                        new RegexException(text("unknown regex flag '",ch,"'"));
            }
        }
    }

    //parse and store IR for regex pattern
    @trusted void parseRegex()
    {
        fixupStack.push(0);
        groupStack.push(1);//0 - whole match
        auto maxCounterDepth = counterDepth;
        uint fix;//fixup pointer

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
                        genLookaround(IR.LookaheadStart);
                        next();
                        break;
                    case '!':
                        genLookaround(IR.NeglookaheadStart);
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
                        nglob = groupStack.top++;
                        enforce(groupStack.top <= maxGroupNumber, "limit on submatches is exceeded");
                        auto t = NamedGroup(name, nglob);

                        if(__ctfe)
                        {
                            size_t ind;
                            for(ind=0; ind <dict.length; ind++)
                                if(t.name >= dict[ind].name)
                                    break;
                            insertInPlaceAlt(dict, ind, t);
                        }
                        else
                        {
                            auto d = assumeSorted!"a.name < b.name"(dict);
                            auto ind = d.lowerBound(t).length;
                            insertInPlaceAlt(dict, ind, t);
                        }
                        put(Bytecode(IR.GroupStart, nglob));
                        break;
                    case '<':
                        next();
                        if(current == '=')
                            genLookaround(IR.LookbehindStart);
                        else if(current == '!')
                            genLookaround(IR.NeglookbehindStart);
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
                    nglob = groupStack.top++;
                    enforce(groupStack.top <= maxGroupNumber, "limit on number of submatches is exceeded");
                    put(Bytecode(IR.GroupStart, nglob));
                }
                break;
            case ')':
                enforce(nesting, "Unmatched ')'");
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
                    assert(lookaroundNest);
                    fixLookaround(fix);
                    lookaroundNest--;
                    put(ir[fix].paired);
                    break;
                case IR.Option: //| xxx )
                    //two fixups: last option + full OR
                    finishAlternation(fix);
                    fix = fixupStack.top;
                    switch(ir[fix].code)
                    {
                    case IR.GroupStart:
                        fixupStack.pop();
                        put(Bytecode(IR.GroupEnd,ir[fix].data));
                        parseQuantifier(fix);
                        break;
                    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                        assert(lookaroundNest);
                        lookaroundNest--;
                        fix = fixupStack.pop();
                        fixLookaround(fix);
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
                if(ir.length > fix && ir[fix].code == IR.Option)
                {
                    ir[fix] = Bytecode(ir[fix].code, cast(uint)ir.length - fix);
                    put(Bytecode(IR.GotoEndOr, 0));
                    fixupStack.top = cast(uint)ir.length; //replace latest fixup for Option
                    put(Bytecode(IR.Option, 0));
                    break;
                }
                //start a new option
                if(fixupStack.length == 1)//only root entry
                    fix = -1;
                uint len = cast(uint)ir.length - fix;
                insertInPlaceAlt(ir, fix+1, Bytecode(IR.OrStart, 0), Bytecode(IR.Option, len));
                assert(ir[fix+1].code == IR.OrStart);
                put(Bytecode(IR.GotoEndOr, 0));
                fixupStack.push(fix+1); //fixup for StartOR
                fixupStack.push(cast(uint)ir.length); //for Option
                put(Bytecode(IR.Option, 0));
                break;
            default://no groups or whatever
                uint start = cast(uint)ir.length;
                parseAtom();
                parseQuantifier(start);
            }
        }

        if(fixupStack.length != 1)
        {
            fix = fixupStack.pop();
            enforce(ir[fix].code == IR.Option,"no matching ')'");
            finishAlternation(fix);
            enforce(fixupStack.length == 1, "no matching ')'");
        }
    }

    //helper function, finalizes IR.Option, fix points to the first option of sequence
    void finishAlternation(uint fix)
    {
        enforce(ir[fix].code == IR.Option, "no matching ')'");
        ir[fix] = Bytecode(ir[fix].code, cast(uint)ir.length - fix - IRL!(IR.OrStart));
        fix = fixupStack.pop();
        enforce(ir[fix].code == IR.OrStart, "no matching ')'");
        ir[fix] = Bytecode(IR.OrStart, cast(uint)ir.length - fix - IRL!(IR.OrStart));
        put(Bytecode(IR.OrEnd, cast(uint)ir.length - fix - IRL!(IR.OrStart)));
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

    //parse and store IR for atom-quantifier pair
    @trusted void parseQuantifier(uint offset)
    {//moveAll is @system
        uint replace = ir[offset].code == IR.Nop;
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
            enforce(next(), "Unexpected end of regex pattern");
            enforce(ascii.isDigit(current), "First number required in repetition");
            min = parseDecimal();
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
                moveAllAlt(ir[offset+1..$],ir[offset..$-1]);
                ir.length -= 1;
            }
            return;
        }
        uint len = cast(uint)ir.length - offset - replace;
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
                    insertInPlaceAlt(ir, offset, op);
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                put(Bytecode.init); //hotspot
                putRaw(1);
                putRaw(min);
                putRaw(max);
                counterDepth = std.algorithm.max(counterDepth, nesting+1);
            }
        }
        else if(min) //&& max is infinite
        {
            if(min != 1)
            {
                Bytecode op = Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len);
                if(replace)
                    ir[offset] = op;
                else
                    insertInPlaceAlt(ir, offset, op);
                offset += 1;//so it still points to the repeated block
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                put(Bytecode.init); //hotspot
                putRaw(1);
                putRaw(min);
                putRaw(min);
                counterDepth = std.algorithm.max(counterDepth, nesting+1);
            }
            else if(replace)
            {
                if(__ctfe)//CTFE workaround: no moveAll and length -= x;
                {
                    ir = ir[0..offset] ~ ir[offset+1..$];
                }
                else
                {
                    moveAll(ir[offset+1 .. $],ir[offset .. $-1]);
                    ir.length -= 1;
                }
            }
            put(Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len));
            enforce(ir.length + len < maxCompiledLength,  "maximum compiled pattern length is exceeded");
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
                insertInPlaceAlt(ir, offset, op);
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index

        }
    }

    //parse and store IR for atom
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
            enforce(_next(), "Unfinished escape sequence");
            parseEscape();
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
                assert(range.length <= 5);
                if(range.length == 1)
                    put(Bytecode(IR.Char, range[0]));
                else
                    foreach(v; range)
                        put(Bytecode(IR.OrChar, v, cast(uint)range.length));
            }
            else
                put(Bytecode(IR.Char, current));
            next();
        }
    }

    //generate code for start of lookaround: (?= (?! (?<= (?<!
    void genLookaround(IR opcode)
    {
        put(Bytecode(opcode, 0));
        put(Bytecode.fromRaw(0));
        put(Bytecode.fromRaw(0));
        groupStack.push(0);
        lookaroundNest++;
        enforce(lookaroundNest <= maxLookaroundDepth, "maximum lookaround depth is exceeded");
    }

    //fixup lookaround with start at offset fix
    void fixLookaround(uint fix)
    {
        ir[fix] = Bytecode(ir[fix].code, cast(uint)ir.length - fix - IRL!(IR.LookaheadStart));
        auto g = groupStack.pop();
        assert(!groupStack.empty);
        ir[fix+1] = Bytecode.fromRaw(groupStack.top);
        //groups are cumulative across lookarounds
        ir[fix+2] = Bytecode.fromRaw(groupStack.top+g);
        groupStack.top += g;
    }

    //CodepointSet operations relatively in order of priority
    enum Operator:uint { Open=0, Negate,  Difference, SymDifference, Intersection, Union, None };

    //parse unit of CodepointSet spec, most notably escape sequences and char ranges
    //also fetches next set operation
    Tuple!(CodepointSet,Operator) parseCharTerm()
    {
        enum State{ Start, Char, Escape, Dash, DashEscape };
        Operator op = Operator.None;;
        dchar last;
        CodepointSet set;
        State state = State.Start;
        static void addWithFlags(ref CodepointSet set, uint ch, uint re_flags)
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
            case State.DashEscape:  //xxxx-\yyyy
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
            enforce(next(), "unexpected end of CodepointSet");
        }
        return tuple(set, op);
    }

    alias Stack!(CodepointSet,CTFE) ValStack;
    alias Stack!(Operator,CTFE) OpStack;

    //parse and store IR for CodepointSet
    void parseCharset()
    {
        ValStack vstack;
        OpStack opstack;
        //
        static bool apply(Operator op, ref ValStack stack)
        {
            switch(op)
            {
            case Operator.Negate:
                stack.top.negate;
                break;
            case Operator.Union:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '||'");
                stack.top.add(s);
                break;
            case Operator.Difference:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '--'");
                stack.top.sub(s);
                break;
            case Operator.SymDifference:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '~~'");
                stack.top.symmetricSub(s);
                break;
            case Operator.Intersection:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '&&'");
                stack.top.intersect(s);
                break;
            default:
                return false;
            }
            return true;
        }
        static bool unrollWhile(alias cond)(ref ValStack vstack, ref OpStack opstack)
        {
            while(cond(opstack.top))
            {
                debug(fred_charset)
                {
                    writeln(opstack.stack.data);
                    //writeln(map!"a.intervals"(vstack.stack.data));
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
                enforce(next(), "unexpected end of CodepointSet");
                if(current == '^')
                {
                    opstack.push(Operator.Negate);
                    enforce(next(), "unexpected end of CodepointSet");
                }
                //[] is prohibited
                enforce(current != ']', "wrong CodepointSet");
                goto default;
            case ']':
                enforce(unrollWhile!(unaryFun!"a != a.Open")(vstack, opstack),
                        "CodepointSet syntax error");
                enforce(!opstack.empty, "unmatched ']'");
                opstack.pop();
                next();
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
        charsetToIr(vstack.top);
    }
    //try to generate optimal IR code for this CodepointSet
    @trusted void charsetToIr(in CodepointSet set)
    {//@@@BUG@@@ writeln is @system
        uint chars = set.chars();
        if(chars < Bytecode.maxSequence)
        {
            switch(chars)
            {
                case 1:
                    put(Bytecode(IR.Char, set.ivals[0]));
                    break;
                case 0:
                    error("empty CodepointSet not allowed");
                    break;
                default:
                    foreach(ch; set[])
                        put(Bytecode(IR.OrChar, ch, chars));
            }
        }
        else
        {
            if(set.ivals.length > maxCharsetUsed)
            {
                auto t  = getTrie(set);
                put(Bytecode(IR.Trie, cast(uint)tries.length));
                tries ~= t;
                debug(fred_allocation) writeln("Trie generated");
            }
            else
            {
                put(Bytecode(IR.CodepointSet, cast(uint)charsets.length));
                tries ~= Trie.init;
            }
            charsets ~= set;
            assert(charsets.length == tries.length);
        }
    }

    //parse and generate IR for escape stand alone escape sequence
    @trusted void parseEscape()
    {//accesses array of appender

        switch(current)
        {
        case 'f':   next(); put(Bytecode(IR.Char, '\f')); break;
        case 'n':   next(); put(Bytecode(IR.Char, '\n')); break;
        case 'r':   next(); put(Bytecode(IR.Char, '\r')); break;
        case 't':   next(); put(Bytecode(IR.Char, '\t')); break;
        case 'v':   next(); put(Bytecode(IR.Char, '\v')); break;

        case 'd':
            next();
            charsetToIr(unicodeNd);
            break;
        case 'D':
            next();
            charsetToIr(unicodeNd.dup.negate);
            break;
        case 'b':   next(); put(Bytecode(IR.Wordboundary, 0)); break;
        case 'B':   next(); put(Bytecode(IR.Notwordboundary, 0)); break;
        case 's':
            next();
            charsetToIr(unicodeWhite_Space);
            break;
        case 'S':
            next();
            charsetToIr(unicodeWhite_Space.dup.negate);
            break;
        case 'w':
            next();
            charsetToIr(wordCharacter);
            break;
        case 'W':
            next();
            charsetToIr(wordCharacter.dup.negate);
            break;
        case 'p': case 'P':
            auto CodepointSet = parseUnicodePropertySpec(current == 'P');
            charsetToIr(CodepointSet);
            break;
        case 'x':
            uint code = parseUniHex(pat, 2);
            next();
            put(Bytecode(IR.Char,code));
            break;
        case 'u': case 'U':
            uint code = parseUniHex(pat, current == 'u' ? 4 : 8);
            next();
            put(Bytecode(IR.Char, code));
            break;
        case 'c': //control codes
            Bytecode code = Bytecode(IR.Char, parseControlCode());
            next();
            put(code);
            break;
        case '0':
            next();
            put(Bytecode(IR.Char, 0));//NUL character
            break;
        case '1': .. case '9':
            uint nref = cast(uint)current - '0';
            uint maxBackref;
            foreach(v; groupStack.stack.data)
                maxBackref += v;
            uint localLimit = maxBackref - groupStack.top;
            enforce(nref < maxBackref, "Backref to unseen group");
            //perl's disambiguation rule i.e.
            //get next digit only if there is such group number
            while(nref < maxBackref && next() && ascii.isDigit(current))
            {
                nref = nref * 10 + current - '0';
            }
            if(nref >= maxBackref)
                nref /= 10;

            if(nref >= localLimit)
            {
                put(Bytecode(IR.Backref, nref-localLimit));
                ir[$-1].setLocalRef();
            }
            else
                put(Bytecode(IR.Backref, nref));
            markBackref(nref);
            break;
        default:
            auto op = Bytecode(IR.Char, current);
            next();
            put(op);
        }
    }

	//parse and return a CodepointSet for \p{...Property...} and \P{...Property..},
	//\ - assumed to be processed, p - is current
	const(CodepointSet) parseUnicodePropertySpec(bool negated)
	{
        alias comparePropertyName ucmp;
        enum MAX_PROPERTY = 128;
        char[MAX_PROPERTY] result;
        uint k=0;
        enforce(next());
		if(current == '{')
        {
            while(k<MAX_PROPERTY && next() && current !='}' && current !=':')
                if(current != '-' && current != ' ' && current != '_')
                    result[k++] = cast(char)ascii.toLower(current);
            enforce(k != MAX_PROPERTY, "invalid property name");
            enforce(current == '}', "} expected ");
        }
        else
        {//single char properties e.g.: \pL, \pN ...
            enforce(current < 0x80, "invalid property name");
            result[k++] = cast(char)current;
        }
		auto s = getUnicodeSet(result[0..k], negated, cast(bool)(re_flags & RegexOption.casefold));
		enforce(!s.empty, "unrecognized unicode property spec");
		next();
		return s;
	}

    //
    @trusted void error(string msg)
    {
        auto app = appender!string();
        ir = null;
        formattedWrite(app, "%s\nPattern with error: `%s` <--HERE-- `%s`",
                       msg, origin[0..$-pat.length], pat);
        throw new RegexException(app.data);
    }
    alias BasicElementOf!R Char;
    //packages parsing results into a RegEx object
    @property Regex!Char program()
    {
        return Regex!Char(this);
    }
}


/**
    $(D RegEx) object holds regular expression pattern in compiled form.
    Instances of this object are constructed via calls to $(D regex).
    It's an intended form for caching and storage of frequently used regular expressions.
*/
public struct Regex(Char)
{
    //temporary workaround for identifier lookup
    const(CodepointSet)[] charsets; //
    Bytecode[] ir;      //compiled bytecode of pattern

private:
    NamedGroup[] dict;  //maps name -> user group number
    uint ngroup;        //number of internal groups
    uint maxCounterDepth; //max depth of nested {n,m} repetitions
    uint hotspotTableSize; //number of entries in merge table
    uint threadCount;
    uint flags;         //global regex flags
    const(Trie)[]  tries; //
    uint[] backrefed; //bit array of backreferenced submatches
    Kickstart!Char kickstart;

    //bit access helper
    uint isBackref(uint n)
    {
        if(n/32 >= backrefed.length)
            return 0;
        return backrefed[n/32] & (1<<(n&31));
    }

    //check if searching is not needed
    void checkIfOneShot()
    {
        if(flags & RegexOption.multiline)
            return;
    L_CheckLoop:
        for(uint i=0; i<ir.length; i+=ir[i].length)
        {
            switch(ir[i].code)
            {
                case IR.Bol:
                    flags |= RegexInfo.oneShot;
                    break L_CheckLoop;
                case IR.GroupStart, IR.GroupEnd, IR.Eol, IR.Wordboundary, IR.Notwordboundary:
                    break;
                default:
                    break L_CheckLoop;
            }
        }
    }

    /+
        lightweight post process step,
        only essentials
    +/
    @trusted void lightPostprocess()
    {//@@@BUG@@@ write is @system
        struct FixedStack(T)
        {
            T[] arr;
            uint _top;
            //this(T[] storage){   arr = storage; _top = -1; }
            @property ref T top(){  assert(!empty); return arr[_top]; }
            void push(T x){  arr[++_top] = x; }
            T pop() { assert(!empty);   return arr[_top--]; }
            @property bool empty(){   return _top == -1; }
        }
        auto counterRange = FixedStack!uint(new uint[maxCounterDepth+1], -1);
        counterRange.push(1);
        ulong cumRange = 0;
        for(uint i=0; i<ir.length; i+=ir[i].length)
        {
            if(ir[i].hotspot)
            {
                assert(i + 1 < ir.length, "unexpected end of IR while looking for hotspot");
                ir[i+1] = Bytecode.fromRaw(hotspotTableSize);
                hotspotTableSize += counterRange.top;
            }
            switch(ir[i].code)
            {
            case IR.RepeatStart, IR.RepeatQStart:
                uint repEnd = cast(uint)(i + ir[i].data + IRL!(IR.RepeatStart));
                assert(ir[repEnd].code == ir[i].paired.code);
                uint max = ir[repEnd + 4].raw;
                ir[repEnd+2].raw = counterRange.top;
                ir[repEnd+3].raw *= counterRange.top;
                ir[repEnd+4].raw *= counterRange.top;
                ulong cntRange = cast(ulong)(max)*counterRange.top;
                cumRange += cntRange;
                enforce(cumRange < maxCumulativeRepetitionLength, "repetition length limit is exceeded");
                counterRange.push(cast(uint)cntRange + counterRange.top);
                threadCount += counterRange.top;
                break;
            case IR.RepeatEnd, IR.RepeatQEnd:
                threadCount += counterRange.top;
                counterRange.pop();
                break;
            case IR.GroupStart:
                if(isBackref(ir[i].data))
                    ir[i].setBackrefence();
                threadCount += counterRange.top;
                break;
            case IR.GroupEnd:
                if(isBackref(ir[i].data))
                    ir[i].setBackrefence();
                threadCount += counterRange.top;
                break;
            default:
                threadCount += counterRange.top;
            }
        }
        checkIfOneShot();
        if(!(flags & RegexInfo.oneShot))
            kickstart = Kickstart!Char(this, new uint[](256));
        debug(fred_allocation) writefln("IR processed, max threads: %d", threadCount);
    }

    //IR code validator - proper nesting, illegal instructions, etc.
    @trusted void validate()
    {//@@@BUG@@@ text is @system
        uint[] groupBits = new uint[ngroup/32+1];
        for(uint pc=0; pc<ir.length; pc+=ir[pc].length)
        {
            if(ir[pc].isStart || ir[pc].isEnd)
            {
                uint dest = ir[pc].indexOfPair(pc);
                assert(dest < ir.length, text("Wrong length in opcode at pc=",pc, " ", dest, " vs ", ir.length));
                assert(ir[dest].paired ==  ir[pc],
                        text("Wrong pairing of opcodes at pc=", pc, "and pc=", dest));
            }
            else if(ir[pc].isAtom)
            {

            }
            else
               assert(0, text("Unknown type of instruction at pc=", pc));
        }
        foreach(i,v; groupBits)
            assert(v == 0, text("unclosed group, bogus # is in range ", i*32, " - ",i*32+32));
    }

    //print out disassembly a program's IR
    @trusted debug public void print() const
    {//@@@BUG@@@ write is system
        import std.stdio;
        writefln("PC\tINST\n");
        prettyPrint(delegate void(const(char)[] s){ write(s); },ir);
        writefln("\n");
        for(uint i=0; i<ir.length; i+=ir[i].length)
        {
            writefln("%d\t%s ", i, disassemble(ir, i, dict));
        }
        writeln("Total merge table size: ", hotspotTableSize);
        writeln("Max counter nesting depth: ", maxCounterDepth);
    }

	//
    this(S,bool x)(Parser!(S,x) p)
    {
        if(__ctfe)//CTFE something funky going on with array
            ir = p.ir.dup;
        else
            ir = p.ir;
        dict = p.dict;
        ngroup = p.groupStack.top;
        maxCounterDepth = p.counterDepth;
        flags = p.re_flags;
        charsets = p.charsets;
        tries = p.tries;
        backrefed = p.backrefed;
        lightPostprocess();
        debug(fred_parser)
        {
            print();
        }
        debug validate();
    }
}

//
@trusted uint lookupNamedGroup(String)(NamedGroup[] dict,String name)
{//equal is @system?
    //@@@BUG@@@ kills "-inline"
	//auto fnd = assumeSorted(map!"a.name"(dict)).lowerBound(name).length;
    uint fnd;
    for(fnd = 0; fnd<dict.length; fnd++)
        if(equal(dict[fnd].name,name))
            break;
    enforce(fnd < dict.length, text("no submatch named ", name));
	return dict[fnd].group;
}

//whether ch is one of unicode newline sequences
bool endOfLine(dchar front, bool seenCr)
{
    return ((front == '\n') ^ seenCr) || front == '\r' || front == NEL || front == LS || front == PS;
}

//
bool startOfLine(dchar back, bool seenNl)
{
    return ((back == '\r') ^ seenNl) || back == '\n' || back == NEL || back == LS || back == PS;
}

//Test if bytecode starting at pc in program 're' can match given codepoint
//Returns: length of matched atom if test is positive, 0 - can't tell, -1 if doesn't match
int quickTestFwd(RegEx)(uint pc, dchar front, const ref RegEx re)
{
    static assert(IRL!(IR.OrChar) == 1);//used in code processing IR.OrChar
    for(;;)
        switch(re.ir[pc].code)
        {
        case IR.OrChar:
            uint len = re.ir[pc].sequence;
            uint end = pc + len;
            if(re.ir[pc].data != front && re.ir[pc+1].data != front)
            {
                for(pc = pc+2; pc<end; pc++)
                    if(re.ir[pc].data == front)
                        break;
                if(pc == end)
                    return -1;
            }
            return 0;
        case IR.Char:
            if(front == re.ir[pc].data)
                return 0;
            else
                return -1;
        case IR.Any:
            return 0;
        case IR.CodepointSet:
            if(re.charsets[re.ir[pc].data].scanFor(front))
                return 0;
            else
                return -1;
        case IR.GroupStart, IR.GroupEnd:
            pc += IRL!(IR.GroupStart);
            break;
        case IR.Trie:
            if(re.tries[re.ir[pc].data][front])
                return IRL!(IR.Trie);
            else
                return -1;
        default:
            return 0;
        }
}

/*
    Useful utility for self-testing, an infinite range of string samples
    that _have_ to match given compiled regex.
    Caveats: supports only a simple subset of bytecode.
*/
@trusted public struct SampleGenerator(Char)
{
    import std.random;
    const(Regex!Char) re;
    Appender!(char[]) app;
    uint limit, seed;
    Xorshift gen;
    //generator for pattern r, with soft maximum of threshold elements
    //and a given random seed
    this(in Regex!Char r, uint threshold, uint randomSeed)
    {
        re = r;
        limit = threshold;
        seed = randomSeed;
        app = appender!(Char[])();
        compose();
    }

    uint rand(uint x)
    {
        uint r = gen.front % x;
        gen.popFront();
        return r;
    }

    void compose()
    {
        uint pc = 0, counter = 0, dataLenOld = uint.max;
        for(;;)
        {
            switch(re.ir[pc].code)
            {
            case IR.Char:
                    formattedWrite(app,"%s", cast(dchar)re.ir[pc].data);
                    pc += IRL!(IR.Char);
                    break;
                case IR.OrChar:
                    uint len = re.ir[pc].sequence;
                    formattedWrite(app, "%s", cast(dchar)re.ir[pc + rand(len)].data);
                    pc += len;
                    break;
                case IR.CodepointSet:
                case IR.Trie:
                    auto set = re.charsets[re.ir[pc].data];
                    auto x = rand(set.ivals.length/2);
                    auto y = rand(set.ivals[x*2+1] - set.ivals[2*x]);
                    formattedWrite(app, "%s", cast(dchar)(set.ivals[2*x]+y));
                    pc += IRL!(IR.CodepointSet);
                    break;
                case IR.Any:
                    uint x;
                    do
                    {
                        x = rand(0x11_000);
                    }while(!isValidDchar(x));
                    formattedWrite(app, "%s", cast(dchar)x);
                    pc += IRL!(IR.Any);
                    break;
                case IR.GotoEndOr:
                    pc += IRL!(IR.GotoEndOr)+re.ir[pc].data;
                    assert(re.ir[pc].code == IR.OrEnd);
                    goto case;
                case IR.OrEnd:
                    pc += IRL!(IR.OrEnd);
                    break;
                case IR.OrStart:
                    pc += IRL!(IR.OrStart);
                    goto case;
                case IR.Option:
                    uint next = pc + re.ir[pc].data + IRL!(IR.Option);
                    uint nOpt = 0;
                    //queue next Option
                    while(re.ir[next].code == IR.Option)
                    {
                        nOpt++;
                        next += re.ir[next].data + IRL!(IR.Option);
                    }
                    nOpt++;
                    nOpt = rand(nOpt);
                    for(;nOpt; nOpt--)
                    {
                        pc += re.ir[pc].data + IRL!(IR.Option);
                    }
                    assert(re.ir[pc].code == IR.Option);
                    pc += IRL!(IR.Option);
                    break;
                case IR.RepeatStart:case IR.RepeatQStart:
                    pc += IRL!(IR.RepeatStart)+re.ir[pc].data;
                    goto case IR.RepeatEnd;
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    uint len = re.ir[pc].data;
                    uint step = re.ir[pc+2].raw;
                    uint min = re.ir[pc+3].raw;
                    if(counter < min)
                    {
                        counter += step;
                        pc -= len;
                        break;
                    }
                    uint max = re.ir[pc+4].raw;
                    if(counter < max)
                    {
                        if(app.data.length < limit && rand(3) > 0)
                        {
                            pc -= len;
                            counter += step;
                        }
                        else
                        {
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
                case IR.InfiniteStart, IR.InfiniteQStart:
                    pc += re.ir[pc].data + IRL!(IR.InfiniteStart);
                    goto case IR.InfiniteEnd; //both Q and non-Q
                case IR.InfiniteEnd:
                case IR.InfiniteQEnd:
                    uint len = re.ir[pc].data;
                    if(app.data.length == dataLenOld)
                    {
                        pc += IRL!(IR.InfiniteEnd);
                        break;
                    }
                    dataLenOld = app.data.length;
                    if(app.data.length < limit && rand(3) > 0)
                        pc = pc - len;
                    else
                        pc = pc + IRL!(IR.InfiniteEnd);
                    break;
                case IR.GroupStart, IR.GroupEnd:
                    pc += IRL!(IR.GroupStart);
                    break;
                case IR.Bol, IR.Wordboundary, IR.Notwordboundary:
                case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                default:
                    return;
            }
        }
    }

    @property Char[] front()
    {
        return app.data;
    }

    @property empty(){  return false; }

    void popFront()
    {
        app.shrinkTo(0);
        compose();
    }
}

/**
    A $(D StaticRegex) is $(D Regex) object that contains specially
    generated machine code to speed up matching.
    Implicitly convertible to normal $(D Regex),
    however doing so will result in loosing this additional capability.
*/
public struct StaticRegex(Char)
{
private:
    alias BacktrackingMatcher!(true) Matcher;
    alias bool function(ref Matcher!Char) MatchFn;
    MatchFn nativeFn;
public:
    Regex!Char _regex;
    alias _regex this;
    this(Regex!Char re, MatchFn fn)
    {
        _regex = re;
        nativeFn = fn;

    }

}

//utility for shiftOr, returns a minimum number of bytes to test in a Char
uint effectiveSize(Char)()
{
    static if(is(Char == char))
        return 1;
    else static if(is(Char == wchar))
        return 2;
    else static if(is(Char == dchar))
        return 3;
    else
        static assert(0);
}

/*
    Kickstart engine using ShiftOr algorithm,
    a bit parallel technique for inexact string searching.
*/
struct ShiftOr(Char)
{
private:
    uint[] table;
    uint fChar;
    uint n_length;
    enum charSize =  effectiveSize!Char();
    //maximum number of chars in CodepointSet to process
    enum uint charsetThreshold = 32_000;
    static struct ShiftThread
    {
        uint[] tab;
        uint mask;
        uint idx;
        uint pc, counter, hops;
        this(uint newPc, uint newCounter, uint[] table)
        {
            pc = newPc;
            counter = newCounter;
            mask = 1;
            idx = 0;
            hops = 0;
            tab = table;
        }

        void setMask(uint idx, uint mask)
        {
            tab[idx] |= mask;
        }

        void setInvMask(uint idx, uint mask)
        {
            tab[idx] &= ~mask;
        }

        void set(alias setBits=setInvMask)(dchar ch)
        {
            static if(charSize == 3)
            {
                uint val = ch, tmask = mask;
                setBits(val&0xFF, tmask);
                tmask <<= 1;
                val >>= 8;
                setBits(val&0xFF, tmask);
                tmask <<= 1;
                val >>= 8;
                assert(val <= 0x10);
                setBits(val, tmask);
                tmask <<= 1;
            }
            else
            {
                Char[dchar.sizeof/Char.sizeof] buf;
                uint tmask = mask;
                size_t total = encode(buf, ch);
                for(size_t i=0; i<total; i++, tmask<<=1)
                {
                    static if(charSize == 1)
                        setBits(buf[i], tmask);
                    else static if(charSize == 2)
                    {
                        setBits(buf[i]&0xFF, tmask);
                        tmask <<= 1;
                        setBits(buf[i]>>8, tmask);
                    }
                }
            }
        }
        void add(dchar ch){ return set!setInvMask(ch); }
        void advance(uint s)
        {
            mask <<= s;
            idx += s;
        }
        @property bool full(){    return !mask; }
    }

	static ShiftThread fork(ShiftThread t, uint newPc, uint newCounter)
	{
		ShiftThread nt = t;
		nt.pc = newPc;
		nt.counter = newCounter;
		return nt;
	}

    @trusted static ShiftThread fetch(ref ShiftThread[] worklist)
	{
		auto t = worklist[$-1];
		worklist.length -= 1;
		if(!__ctfe)
			worklist.assumeSafeAppend();
		return t;
	}

    static uint charLen(uint ch)
	{
		assert(ch <= 0x10FFFF);
		return codeLength!Char(cast(dchar)ch)*charSize;
	}

public:
    @trusted this(const ref Regex!Char re, uint[] memory)
    {
        assert(memory.length == 256);
        fChar = uint.max;
	L_FindChar:
        for(size_t i = 0;;)
        {
			switch(re.ir[i].code)
			{
				case IR.Char:
					fChar = re.ir[i].data;
					static if(charSize != 3)
					{
						Char buf[dchar.sizeof/Char.sizeof];
						encode(buf, fChar);
						fChar = buf[0];
					}
					fChar = fChar & 0xFF;
					break L_FindChar;
				case IR.GroupStart, IR.GroupEnd:
                    i += IRL!(IR.GroupStart);
                    break;
                case IR.Bol, IR.Wordboundary, IR.Notwordboundary:
                    i += IRL!(IR.Bol);
                    break;
				default:
					break L_FindChar;
			}
		}
		debug(fred_search) writefln("ShiftOr: %x ", fChar);
		table = memory;
        table[] =  uint.max;
        ShiftThread[] trs;
        ShiftThread t = ShiftThread(0, 0, table);
        //locate first fixed char if any
        n_length = 32;
        for(;;)
        {
        L_Eval_Thread:
            for(;;)
            {
                switch(re.ir[t.pc].code)
                {
                case IR.Char:
                    uint s = charLen(re.ir[t.pc].data);
                    if(t.idx+s > n_length)
                        goto L_StopThread;
                    t.add(re.ir[t.pc].data);
                    t.advance(s);
                    t.pc += IRL!(IR.Char);
                    break;
                case IR.OrChar://assumes IRL!(OrChar) == 1
                    uint len = re.ir[t.pc].sequence;
                    uint end = t.pc + len;
                    uint[Bytecode.maxSequence] s;
                    uint numS;
                    for(uint i = 0; i<len; i++)
                    {
                        auto x = charLen(re.ir[t.pc+i].data);
                        if(countUntil(s[0..numS], x) < 0)
                           s[numS++] = x;
                    }
                    for(uint i = t.pc; i < end; i++)
                    {
                        t.add(re.ir[i].data);
                    }
                    for(uint i=0; i<numS; i++)
                    {
                        auto tx = fork(t, t.pc + len, t.counter);
                        if(tx.idx + s[i] <= n_length)
                        {
                            tx.advance(s[i]);
                            trs ~= tx;
                        }
                    }
                    if(!trs.empty)
                        t = fetch(trs);
                    else
                        goto L_StopThread;
                    break;
                case IR.CodepointSet:
                case IR.Trie:
                    auto set = re.charsets[re.ir[t.pc].data];
                    uint[4] s;
                    uint numS;
                    static if(charSize == 3)
                    {
                        s[0] = charSize;
                        numS = 1;
                    }
                    else
                    {
                        static if(charSize == 1)
                            static immutable codeBounds = [0x0, 0x7F, 0x80, 0x7FF, 0x800, 0xFFFF, 0x10000, 0x10FFFF];
                        else //== 2
                            static immutable codeBounds = [0x0, 0xFFFF, 0x10000, 0x10FFFF];
                        auto srange = assumeSorted!"a<=b"(set.ivals);
                        for(uint i = 0; i<codeBounds.length/2; i++)
                        {
                            auto start = srange.lowerBound(codeBounds[2*i]).length;
                            auto end = srange.lowerBound(codeBounds[2*i+1]).length;
                            if(end > start || (end == start && (end & 1)))
                               s[numS++] = (i+1)*charSize;
                        }
                    }
                    if(numS == 0 || t.idx + s[numS-1] > n_length)
                        goto L_StopThread;
                    auto  chars = set.chars;
                    if(chars > charsetThreshold)
                        goto L_StopThread;
                    foreach(ch; set[])
                    {
                        //avoid surrogate pairs
                        if(0xD800 <= ch && ch <= 0xDFFF)
                            continue;
                        t.add(ch);
                    }
                    for(uint i=0; i<numS; i++)
                    {
                        auto tx =  fork(t, t.pc + IRL!(IR.CodepointSet), t.counter);
                        tx.advance(s[i]);
                        trs ~= tx;
                    }
                    if(!trs.empty)
                        t = fetch(trs);
                    else
                        goto L_StopThread;
                    break;
                case IR.Any:
                    goto L_StopThread;

                case IR.GotoEndOr:
                    t.pc += IRL!(IR.GotoEndOr)+re.ir[t.pc].data;
                    assert(re.ir[t.pc].code == IR.OrEnd);
                    goto case;
                case IR.OrEnd:
                    t.pc += IRL!(IR.OrEnd);
                    break;
                case IR.OrStart:
                    t.pc += IRL!(IR.OrStart);
                    goto case;
                case IR.Option:
                    uint next = t.pc + re.ir[t.pc].data + IRL!(IR.Option);
                    //queue next Option
                    if(re.ir[next].code == IR.Option)
                    {
                        trs ~= fork(t, next, t.counter);
                    }
                    t.pc += IRL!(IR.Option);
                    break;
                case IR.RepeatStart:case IR.RepeatQStart:
                    t.pc += IRL!(IR.RepeatStart)+re.ir[t.pc].data;
                    goto case IR.RepeatEnd;
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    uint len = re.ir[t.pc].data;
                    uint step = re.ir[t.pc+2].raw;
                    uint min = re.ir[t.pc+3].raw;
                    if(t.counter < min)
                    {
                        t.counter += step;
                        t.pc -= len;
                        break;
                    }
                    uint max = re.ir[t.pc+4].raw;
                    if(t.counter < max)
                    {
                        trs ~= fork(t, t.pc - len, t.counter + step);
                        t.counter = t.counter%step;
                        t.pc += IRL!(IR.RepeatEnd);
                    }
                    else
                    {
                        t.counter = t.counter%step;
                        t.pc += IRL!(IR.RepeatEnd);
                    }
                    break;
                case IR.InfiniteStart, IR.InfiniteQStart:
                    t.pc += re.ir[t.pc].data + IRL!(IR.InfiniteStart);
                    goto case IR.InfiniteEnd; //both Q and non-Q
                case IR.InfiniteEnd:
                case IR.InfiniteQEnd:
                    uint len = re.ir[t.pc].data;
                    uint pc1, pc2; //branches to take in priority order
                    if(++t.hops == 32)
                        goto L_StopThread;
                    pc1 = t.pc + IRL!(IR.InfiniteEnd);
                    pc2 = t.pc - len;
                    trs ~= fork(t, pc2, t.counter);
                    t.pc = pc1;
                    break;
                case IR.GroupStart, IR.GroupEnd:
                    t.pc += IRL!(IR.GroupStart);
                    break;
                case IR.Bol, IR.Wordboundary, IR.Notwordboundary:
                    t.pc += IRL!(IR.Bol);
                    break;
                case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                    t.pc += IRL!(IR.LookaheadStart) + IRL!(IR.LookaheadEnd) + re.ir[t.pc].data;
                    break;
                default:
                L_StopThread:
                    assert(re.ir[t.pc].code >= 0x80);
                    debug (fred_search) writeln("ShiftOr stumbled on ",re.ir[t.pc].mnemonic);
                    n_length = min(t.idx, n_length);
                    break L_Eval_Thread;
                }
            }
            if(trs.empty)
                break;
            t = fetch(trs);
        }
        debug(fred_search)
        {
            writeln("Min length: ", n_length);
        }
    }

    @property bool empty() const {  return n_length == 0; }

    @property uint length() const{ return n_length/charSize; }

    // lookup compatible bit pattern in haystack, return starting index
    // has a useful trait: if supplied with valid UTF indexes,
    // returns only valid UTF indexes
    // (that given the haystack in question is valid UTF string)
    @trusted size_t search(const(Char)[] haystack, size_t idx)
    {
        assert(!empty);
        auto p = cast(const(ubyte)*)(haystack.ptr+idx);
		uint state = uint.max;
		uint limit = 1u<<(n_length - 1u);
		debug(fred_search) writefln("Limit: %32b",limit);
        if(fChar != uint.max)
        {
			const(ubyte)* end = cast(ubyte*)(haystack.ptr + haystack.length);
			while(p != end)
			{
				if(!~state)
				{
					for(;;)
					{
						p = cast(ubyte*)memchr(p, fChar, end - p);
						if(!p)
							return haystack.length;
						if(!(cast(size_t)p & (Char.sizeof-1)))
							break;
						if(++p == end)
							return haystack.length;
					}
					state = ~1u;
					assert((cast(size_t)p & (Char.sizeof-1)) == 0);
					static if(charSize == 3)
					{
						state = (state<<1) | table[p[1]];
						state = (state<<1) | table[p[2]];
						p += 3;
					}
				}
				//first char is already tested, see if that's all
				if(!(state & limit))//division rounds down for dchar
					return (p-cast(ubyte*)haystack.ptr)/Char.sizeof
						-length+1;
				static if(charSize == 3)
				{
					state = (state<<1) | table[p[1]];
					state = (state<<1) | table[p[2]];
					state = (state<<1) | table[p[3]];
					p+=4;
				}
				else
				{
					state = (state<<1) | table[p[1]];
					p++;
				}
				debug(fred_search) writefln("State: %32b", state);
			}
		}
		else
		{
			//in this path we have to shift first
			static if(charSize == 3)
			{
				const(ubyte)* end = cast(ubyte*)(haystack.ptr + haystack.length);
				while(p != end)
				{
					state = (state<<1) | table[p[0]];
					state = (state<<1) | table[p[1]];
					state = (state<<1) | table[p[2]];
					p += 4;
					if(!(state & limit))//division rounds down for dchar
						return (p-cast(ubyte*)haystack.ptr)/Char.sizeof
						-length;
				}
			}
			else
			{
				auto len = cast(ubyte*)(haystack.ptr + haystack.length) - p;
				size_t i  = 0;
				if(len & 1)
				{
					state = (state<<1) | table[p[i++]];
					if(!(state & limit))
						return idx+i/Char.sizeof-length;
				}
				while(i<len)
				{
					state = (state<<1) | table[p[i++]];
					if(!(state & limit))
						return idx+i/Char.sizeof
							-length;
					state = (state<<1) | table[p[i++]];
					if(!(state & limit))
						return idx+i/Char.sizeof
							-length;
					debug(fred_search) writefln("State: %32b", state);
				}
			}
        }
        return haystack.length;
    }

    @system debug static void dump(uint[] table)
    {//@@@BUG@@@ writef(ln) is @system
        for(size_t i=0; i<table.length; i+=4)
        {
            writefln("%32b %32b %32b %32b",table[i], table[i+1], table[i+2], table[i+3]);
        }
    }
}

unittest
{

    @trusted void test_fixed(alias Kick)()
    {
        foreach(i, v; TypeTuple!(char, wchar, dchar))
        {
            alias v Char;
            alias immutable(v)[] String;
            auto r = regex(to!String(`abc$`));
            auto kick = Kick!Char(r, new uint[256]);
            assert(kick.length == 3, text(Kick.stringof," ",v.stringof, " == ", kick.length));
            auto r2 = regex(to!String(`(abc){2}a+`));
            kick = Kick!Char(r2, new uint[256]);
            assert(kick.length == 7, text(Kick.stringof,v.stringof," == ", kick.length));
            auto r3 = regex(to!String(`\b(a{2}b{3}){2,4}`));
            kick = Kick!Char(r3, new uint[256]);
            assert(kick.length == 10, text(Kick.stringof,v.stringof," == ", kick.length));
            auto r4 = regex(to!String(`\ba{2}c\bxyz`));
            kick = Kick!Char(r4, new uint[256]);
            assert(kick.length == 6, text(Kick.stringof,v.stringof, " == ", kick.length));
            auto r5 = regex(to!String(`\ba{2}c\b`));
            kick = Kick!Char(r5, new uint[256]);
            size_t x = kick.search("aabaacaa", 0);
            assert(x == 3, text(Kick.stringof,v.stringof," == ", kick.length));
            x = kick.search("aabaacaa", x+1);
            assert(x == 8, text(Kick.stringof,v.stringof," == ", kick.length));
        }
    }
    @trusted void test_flex(alias Kick)()
    {
        foreach(i, v;TypeTuple!(char, wchar, dchar))
        {
            alias v Char;
            alias immutable(v)[] String;
            auto r = regex(to!String(`abc[a-z]`));
            auto kick = Kick!Char(r, new uint[256]);
            auto x = kick.search(to!String("abbabca"), 0);
            assert(x == 3, text("real x is ", x, " ",v.stringof));

            auto r2 = regex(to!String(`(ax|bd|cdy)`));
            String s2 = to!String("abdcdyabax");
            kick = Kick!Char(r2, new uint[256]);
            x = kick.search(s2, 0);
            assert(x == 1, text("real x is ", x));
            x = kick.search(s2, x+1);
            assert(x == 3, text("real x is ", x));
            x = kick.search(s2, x+1);
            assert(x == 8, text("real x is ", x));
            auto rdot = regex(to!String(`...`));
            kick = Kick!Char(rdot, new uint[256]);
            assert(kick.length == 0);
            auto rN = regex(to!String(`a(b+|c+)x`));
            kick = Kick!Char(rN, new uint[256]);
            assert(kick.length == 3);
            assert(kick.search("ababx",0) == 2);
            assert(kick.search("abaacba",0) == 3);//expected inexact

        }
    }
    test_fixed!(ShiftOr)();
    test_flex!(ShiftOr)();
}

alias ShiftOr Kickstart;

//Simple UTF-string abstraction compatible with stream interface
struct Input(Char)
    if(is(Char :dchar))
{
	alias size_t DataIndex;
    alias const(Char)[] String;
    String _origin;
    size_t _index;

    //constructs Input object out of plain string
    this(String input, size_t idx=0)
    {
        _origin = input;
        _index = idx;
    }

    //codepoint at current stream position
    bool nextChar(ref dchar res, ref size_t pos)
    {
        if(_index == _origin.length)
            return false;
        pos = _index;
        res = std.utf.decode(_origin, _index);
        return true;
    }
    @property bool atEnd(){
        return _index==_origin.length;
    }
    bool search(Kickstart)(ref Kickstart kick, ref dchar res, ref size_t pos)
    {
        size_t idx = kick.search(_origin, _index);
        _index = idx;
        return nextChar(res, pos);
    }

    //index of at End position
    @property size_t lastIndex(){   return _origin.length; }

    //support for backtracker engine, might not be present
    void reset(size_t index){   _index = index;  }

    String opSlice(size_t start, size_t end){   return _origin[start..end]; }

    struct BackLooper
    {
        alias size_t DataIndex;
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
            pos = _index;
            res = _origin[0.._index].back;
            return true;
        }
        @property atEnd(){ return _index==0 || _index==std.utf.strideBack(_origin, _index); }
        @property auto loopBack(){   return Input(_origin, _index); }

        //support for backtracker engine, might not be present
        void reset(size_t index){   _index = index+std.utf.stride(_origin, index);  }

        String opSlice(size_t start, size_t end){   return _origin[end..start]; }
        //index of at End position
        @property size_t lastIndex(){   return 0; }
    }
    @property auto loopBack(){   return BackLooper(this); }
}

// Test stream against simple UTF-string stream abstraction (w/o normalization and such)
struct StreamTester(Char)
    if (is(Char:dchar))
{
    alias ulong DataIndex;
    alias const(Char)[] String;
    Input!(Char) refStream;
    String allStr;
    StreamCBuf!(Char) stream;
    size_t[] splits;
    size_t pos;

    //adds the next chunk to the stream
    bool addNextChunk()
    {
        if(splits.length<pos)
        {
            ++pos;
            if(pos<splits.length)
            {
                assert(splits[pos-1]<=splits[pos],"splits is not ordered");
                stream.addChunk(allStr[splits[pos-1]..splits[pos]]);
            }
            else
            {
                stream.addChunk(allStr[splits[pos-1]..$]);
                stream.hasEnd=true;
            }
            return true;
        }
        else
            return false;
    }

    //constructs Input object out of plain string
    this(String input, size_t[] splits)
    {
        allStr=input;
        refStream=Input!(Char)(input,splits);
        stream=new StreamCBuf!(Char)();
        pos=0;
        if (splits.length) {
            stream.addChunk(allStr);
            stream.hasEnd=true;
        }
        else
            stream.addChunk(allStr[0..splits[0]]);
    }

    //codepoint at current stream position
    bool nextChar(ref dchar res, ref size_t pos)
    {
        bool ret=stream.nextChar(res,pos);
        dchar refRes;
        size_t refPos;
        if(!res)
        {
            if (stream.hasEnd && refStream.nextChar(refRes,refPos))
            {
                throw new Exception("stream eneded too early");
            }
            return false;
        }
        else
        {
            bool refRet=refStream.nextChar(refRes,refPos);
            enforce(refRet==ret,"stream contiinued past end");
            enforce(refRes==res,"incorrect char "~res~" vs "~refRes);
            enforce(refPos==(pos &~(255UL<<48)),"incorrect pos, string wans't normalized???");
            return true;
        }
    }

    @property bool atEnd()
    {
        enforce(!stream.atEnd || refStream.atEnd,"stream ended too early");
        return stream.atEnd;
    }

    bool search(Kickstart)(ref Kickstart kick, ref dchar res, ref ulong pos)
    {
        bool ret=stream.search(kick,res,pos);
        dchar refRes;
        size_t refPos;
        if(ret)
        {
            bool refRet=refStream.search(kick,refRes,refPos);
            enforce(refRet,"stream found spurious kickstart match");
            enforce(refRes==res,"stream found different kickstart match "~res~" vs "~refRes);
            enforce(refPos==(pos &~(255UL<<48)),"stream found different pos for kickstart match, non normalized input?: "~to!string(pos)~" vs "~to!string(refPos));
        }
        else if(hasEnd)
        {
            enforce(!refStream.search(kick,refRes,refPos),"stream missed kickstart match");
        }
        return ret;
    }

    //index of at End position
    @property size_t lastIndex(){   return _origin.length; }

    String opSlice(size_t start, size_t end)
    {
        return _origin[start..end];
    }

    struct BackLooper
    {
        alias ulong DataIndex;
        Input!(Char).BackLooper refBacklooper;
        StreamCBuf!(Char).BackLooper backlooper;
        ulong startPos;

        this(Input!(Char).BackLooper refBacklooper,StreamCBuf!(Char).BackLooper backlooper)
        {
            this.refBacklooper=refBacklooper;
            this.backlooper=backlooper;
        }
        bool nextChar(ref dchar res,ref ulong pos)
        {
            bool ret=backlooper.nextChar(res,pos);
            if(ret)
            {
                dchar refRes;
                size_t refPos;
                bool refRet=refBacklooper.nextChar(refRes,refPos);
                enforce(refRet,"stream backlooper goes back beyond start");
                enforce(refRes==res,"stream backlooper has different char "~res~" vs "~refRes);
                enforce(refPos==(pos &~(255UL<<48)),"stream backlooper has different pos: "~to!string(pos)~" vs "~to!string(refPos));
            }
            else if (refBacklooper.nextChar(refPos,refPos))
            {
                enforce(refPos+historySize<=(startPos &~(255UL<<48)),"stream backlooper stopped before historyWindow");
            }
            return ret;
        }
        @property atEnd(){
            if(backlooper.atEnd)
            {
                dchar res;
                size_t pos;
                if (refBacklooper.nextChar(res,pos))
                {
                    // this should be mostly true for already normalized/decoded stuff
                    enforce(pos+backlooper.streamBuf.historySize<=(startPos &~(255UL<<48)),"backlooper stream ended too early");
                }
            }
            else
            {
                enforce(backlooper.atEnd,"backlooper stream did not end");
            }
            return backlooper.atEnd;
        }
        @property auto loopBack(){   return Input(_origin, _index); }

        //support for backtracker engine, might not be present
        void reset(size_t index){   _index = index+std.utf.stride(_origin, index);  }

        String opSlice(size_t start, size_t end){   return _origin[end..start]; }
        //index of at End position
        @property size_t lastIndex(){   return 0; }
    }
    @property auto loopBack(){   return BackLooper(this); }
}

/+
    BacktrackingMatcher implements backtracking scheme of matching
    regular expressions.
+/
template BacktrackingMatcher(bool CTregex)
{
    @trusted struct BacktrackingMatcher(Char, Stream=Input!Char)
        if(is(Char : dchar))
    {
        alias Stream.DataIndex DataIndex;
        struct State
        {//top bit in pc is set if saved along with matches
            DataIndex index;
            uint pc, counter, infiniteNesting;
        }
        static assert(State.sizeof % size_t.sizeof == 0);
        enum stateSize = State.sizeof / size_t.sizeof;
        enum initialStack = 1<<16;
        alias const(Char)[] String;
        static if(CTregex)
            StaticRegex!Char re;
        else
            Regex!Char re;           //regex program
        //Stream state
        Stream s;
        DataIndex index;
        dchar front;
        bool exhausted;
        //backtracking machine state
        uint pc, counter;
        DataIndex lastState = 0;       //top of state stack
        DataIndex[] trackers;
        uint infiniteNesting;
        size_t[] memory;
        Group!DataIndex[] matches, backrefed; //local slice of matches, global for backref
        Allocator* alloc;
        static if(__traits(hasMember,Stream, "search"))
        {
            enum kicked = true;
        }
        else
            enum kicked = false;

        @property bool atStart(){ return index == 0; }

        @property bool atEnd(){ return index == s.lastIndex && s.atEnd; }

        void next()
        {
            if(!s.nextChar(front, index))
                index = s.lastIndex;
        }

        void search()
        {
            static if(kicked)
            {
                if(!s.search(re.kickstart, front, index))
                {
                    index = s.lastIndex;
                }
            }
            else
                next();
        }

        //
        void newStack()
        {
            auto chunk = alloc.newArray!(size_t[])(initialStack*(stateSize + re.ngroup*(Group!DataIndex).sizeof/size_t.sizeof)+1);
            chunk[0] = cast(size_t)(memory.ptr);
            memory = chunk[1..$];
        }

        //
        this(Regex!Char program, Stream stream, Allocator* allocator)
        {
            re = program;
            s = stream;
            alloc = allocator;
            next();
            exhausted = false;
            trackers = alloc.newArray!(DataIndex[])(re.ngroup+1);
            newStack();
            backrefed = null;
        }

        //
        bool matchFinalize()
        {
            size_t start = index;
            if(matchImpl())
            {//stream is updated here
                matches[0].begin = start;
                matches[0].end = index;
                if(!(re.flags & RegexOption.global) || atEnd)
                    exhausted = true;
                if(start == index)//empty match advances input
                    next();
                return true;
            }
            else
                return false;
        }

        //lookup next match, fill matches with indices into input
        bool match(Group!DataIndex matches[])
        {
            debug(fred_matching)
            {
                writeln("------------------------------------------");
            }
            if(exhausted) //all matches collected
                return false;
            this.matches = matches;
            if(re.flags & RegexInfo.oneShot)
            {
                exhausted = true;
                DataIndex start = index;
                auto m = matchImpl();
                if(m)
                {
                    matches[0].begin = start;
                    matches[0].end = index;
                }
                return m;
            }
            static if(kicked)
                auto searchFn = re.kickstart.empty ? &this.next :&this.search;
            else
                auto searchFn = &this.next;
            for(;;)
            {

                if(matchFinalize())
                    return true;
                else
                {
                    if(atEnd)
                        break;
                    searchFn();
                    if(atEnd)
                    {
                        exhausted = true;
                        return matchFinalize();
                    }
                }

            }
            exhausted = true;
            return false;
        }

        /+
            match subexpression against input,
            results are stored in matches
        +/
        bool matchImpl()
        {
            static if(CTregex && is(typeof(re.nativeFn(this))))
            {
                if(re.nativeFn)
                {
                    version(fred_ct) debug writeln("using C-T matcher");
                    return re.nativeFn(this);
                }
            }
            pc = 0;
            counter = 0;
            lastState = 0;
            infiniteNesting = -1;//intentional
            auto start = s._index;
            debug(fred_matching) writeln("Try match starting at ",s[index..s.lastIndex]);
            for(;;)
            {
                debug(fred_matching) writefln("PC: %s\tCNT: %s\t%s \tfront: %s src: %s", pc, counter, disassemble(re.ir, pc, re.dict), front, s._index);
                switch(re.ir[pc].code)
                {
                case IR.OrChar://assumes IRL!(OrChar) == 1
                    if(atEnd)
                        goto L_backtrack;
                    uint len = re.ir[pc].sequence;
                    uint end = pc + len;
                    if(re.ir[pc].data != front && re.ir[pc+1].data != front)
                    {
                        for(pc = pc+2; pc<end; pc++)
                            if(re.ir[pc].data == front)
                                break;
                        if(pc == end)
                            goto L_backtrack;
                    }
                    pc = end;
                    next();
                    break;
                case IR.Char:
                    if(atEnd || front != re.ir[pc].data)
                        goto L_backtrack;
                    pc += IRL!(IR.Char);
                    next();
                break;
                case IR.Any:
                    if(atEnd)
                        goto L_backtrack;
                    pc += IRL!(IR.Any);
                    next();
                    break;
                case IR.CodepointSet:
                    if(atEnd || !re.charsets[re.ir[pc].data].scanFor(front))
                        goto L_backtrack;
                    next();
                    pc += IRL!(IR.CodepointSet);
                    break;
                case IR.Trie:
                    if(atEnd || !re.tries[re.ir[pc].data][front])
                        goto L_backtrack;
                    next();
                    pc += IRL!(IR.Trie);
                    break;
                case IR.Wordboundary:
                    dchar back;
                    DataIndex bi;
                    //at start & end of input
                    if(atStart && wordTrie[front])
                    {
                        pc += IRL!(IR.Wordboundary);
                        break;
                    }
                    else if(atEnd && s.loopBack.nextChar(back, bi)
                            && wordTrie[back])
                    {
                        pc += IRL!(IR.Wordboundary);
                        break;
                    }
                    else if(s.loopBack.nextChar(back, index))
                    {
                        bool af = wordTrie[front];
                        bool ab = wordTrie[back];
                        if(af ^ ab)
                        {
                            pc += IRL!(IR.Wordboundary);
                            break;
                        }
                    }
                    goto L_backtrack;
                    break;
                case IR.Notwordboundary:
                    dchar back;
                    DataIndex bi;
                    //at start & end of input
                    if(atStart && wordTrie[front])
                        goto L_backtrack;
                    else if(atEnd && s.loopBack.nextChar(back, bi)
                            && wordTrie[back])
                        goto L_backtrack;
                    else if(s.loopBack.nextChar(back, index))
                    {
                        bool af = wordTrie[front];
                        bool ab = wordTrie[back];
                        if(af ^ ab)
                            goto L_backtrack;
                    }
                    pc += IRL!(IR.Wordboundary);
                    break;
                case IR.Bol:
                    dchar back;
                    DataIndex bi;
                    if(atStart)
                        pc += IRL!(IR.Bol);
                    else if((re.flags & RegexOption.multiline)
                        && s.loopBack.nextChar(back,bi)
                        && endOfLine(back, front == '\n'))
                    {
                        pc += IRL!(IR.Bol);
                    }
                    else
                        goto L_backtrack;
                    break;
                case IR.Eol:
                    dchar back;
                    DataIndex bi;
                    debug(fred_matching) writefln("EOL (front 0x%x) %s", front, s[index..s.lastIndex]);
                    //no matching inside \r\n
                    if(atEnd || ((re.flags & RegexOption.multiline)
                        && s.loopBack.nextChar(back,bi)
                        && endOfLine(front, back == '\r')))
                    {
                        pc += IRL!(IR.Eol);
                    }
                    else
                        goto L_backtrack;
                    break;
                case IR.InfiniteStart, IR.InfiniteQStart:
                    trackers[infiniteNesting+1] = index;
                    pc += re.ir[pc].data + IRL!(IR.InfiniteStart);
                    //now pc is at end IR.Infininite(Q)End
                    uint len = re.ir[pc].data;
                    int test;
                    if(re.ir[pc].code == IR.InfiniteEnd)
                    {
                        test = quickTestFwd(pc+IRL!(IR.InfiniteEnd), front, re);
                        if(test >= 0)
                            pushState(pc+IRL!(IR.InfiniteEnd), counter);
                        infiniteNesting++;
                        pc -= len;
                    }
                    else
                    {
                        test = quickTestFwd(pc - len, front, re);
                        if(test >= 0)
                        {
                            infiniteNesting++;
                            pushState(pc - len, counter);
                            infiniteNesting--;
                        }
                        pc += IRL!(IR.InfiniteEnd);
                    }
                    break;
                case IR.RepeatStart, IR.RepeatQStart:
                    pc += re.ir[pc].data + IRL!(IR.RepeatStart);
                    break;
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    //len, step, min, max
                    uint len = re.ir[pc].data;
                    uint step =  re.ir[pc+2].raw;
                    uint min = re.ir[pc+3].raw;
                    uint max = re.ir[pc+4].raw;
                    if(counter < min)
                    {
                        counter += step;
                        pc -= len;
                    }
                    else if(counter < max)
                    {
                        if(re.ir[pc].code == IR.RepeatEnd)
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
                    uint len = re.ir[pc].data;
                    debug(fred_matching) writeln("Infinited nesting:", infiniteNesting);
                    assert(infiniteNesting < trackers.length);

                    if(trackers[infiniteNesting] == index)
                    {//source not consumed
                        pc += IRL!(IR.InfiniteEnd);
                        infiniteNesting--;
                        break;
                    }
                    else
                        trackers[infiniteNesting] = index;
                    int test;
                    if(re.ir[pc].code == IR.InfiniteEnd)
                    {
                        test = quickTestFwd(pc+IRL!(IR.InfiniteEnd), front, re);
                        if(test >= 0)
                        {
                            infiniteNesting--;
                            pushState(pc + IRL!(IR.InfiniteEnd), counter);
                            infiniteNesting++;
                        }
                        pc -= len;
                    }
                    else
                    {
                        test = quickTestFwd(pc-len, front, re);
                        if(test >= 0)
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
                    uint len = re.ir[pc].data;
                    if(re.ir[pc+len].code == IR.GotoEndOr)//not a last one
                    {
                        pushState(pc + len + IRL!(IR.Option), counter); //remember 2nd branch
                    }
                    pc += IRL!(IR.Option);
                    break;
                case IR.GotoEndOr:
                    pc = pc + re.ir[pc].data + IRL!(IR.GotoEndOr);
                    break;
                case IR.GroupStart:
                    uint n = re.ir[pc].data;
                    matches[n].begin = index;
                    debug(fred_matching)  writefln("IR group #%u starts at %u", n, index);
                    pc += IRL!(IR.GroupStart);
                    break;
                case IR.GroupEnd:
                    uint n = re.ir[pc].data;
                    matches[n].end = index;
                    debug(fred_matching) writefln("IR group #%u ends at %u", n, index);
                    pc += IRL!(IR.GroupEnd);
                    break;
                case IR.LookaheadStart:
                case IR.NeglookaheadStart:
                    uint len = re.ir[pc].data;
                    auto save = index;
                    uint ms = re.ir[pc+1].raw, me = re.ir[pc+2].raw;
                    auto matcher = this;
                    auto a = newRegionAllocator();
                    matcher.alloc = &a;
                    matcher.re.ngroup =  me - ms;
                    matcher.memory = null;
                    matcher.newStack();
                    matcher.matches = matches[ms .. me];
                    matcher.backrefed = backrefed.empty ? matches : backrefed;
                    matcher.re.ir = re.ir[pc+IRL!(IR.LookaheadStart) .. pc+IRL!(IR.LookaheadStart)+len+IRL!(IR.LookaheadEnd)];
                    bool match = matcher.matchImpl() ^ (re.ir[pc].code == IR.NeglookaheadStart);
                    s.reset(save);
                    next();
                    if(!match)
                        goto L_backtrack;
                    else
                    {
                        pc += IRL!(IR.LookaheadStart)+len+IRL!(IR.LookaheadEnd);
                    }
                    break;
                case IR.LookbehindStart:
                case IR.NeglookbehindStart:
                    uint len = re.ir[pc].data;
                    auto prog = re;
                    uint ms = re.ir[pc+1].raw, me = re.ir[pc+2].raw;
                    prog.ir = re.ir[pc .. pc+IRL!(IR.LookbehindStart)+len];
                    prog.ngroup = me - ms;
                    auto a = newRegionAllocator();
                    auto backMatcher = BacktrackingMatcher!(Char, typeof(s.loopBack))(prog, s.loopBack, &a);
                    backMatcher.matches = matches[ms .. me];
                    backMatcher.backrefed  = backrefed.empty ? matches : backrefed;
                    bool match = backMatcher.matchBackImpl() ^ (re.ir[pc].code == IR.NeglookbehindStart);
                    if(!match)
                        goto L_backtrack;
                    else
                    {
                        pc += IRL!(IR.LookbehindStart)+len+IRL!(IR.LookbehindEnd);
                    }
                    break;
                case IR.Backref:
                    uint n = re.ir[pc].data;
                    auto referenced = re.ir[pc].localRef
                            ? s[matches[n].begin .. matches[n].end]
                            : s[backrefed[n].begin .. backrefed[n].end];
                    while(!atEnd && !referenced.empty && front == referenced.front)
                    {
                        next();
                        referenced.popFront();
                    }
                    if(referenced.empty)
                        pc++;
                    else
                        goto L_backtrack;
                    break;
                    case IR.Nop:
                    pc += IRL!(IR.Nop);
                    break;
                case IR.LookaheadEnd:
                case IR.NeglookaheadEnd:
                case IR.End:
                    return true;
                default:
                    assert(0);
                L_backtrack:
                    if(!popState())
                    {
                        s.reset(start);
                        return false;
                    }
                }
            }
            assert(0);
        }

        //helper function, saves engine state
        void pushState(uint pc, uint counter)
        {
            if(lastState + stateSize + matches.length >= memory.length)
            {
                newStack();
                lastState = 0;
            }
            *cast(State*)&memory[lastState] = State(index, pc, counter, infiniteNesting);
            lastState += stateSize;
            memory[lastState..lastState+2*matches.length] = cast(size_t[])matches[];
            lastState += 2*matches.length;
            debug(fred_matching)
                writefln("Saved(pc=%s) front: %s src: %s", pc, front, s[index..s.lastIndex]);
        }

        //helper function, restores engine state
        bool popState()
        {
            if(!lastState)
            {
                size_t* prev = memory.ptr-1;
                prev = cast(size_t*)*prev;//take out hidden pointer
                if(!prev)
                    return false;
                immutable size = initialStack*(stateSize + 2*re.ngroup);
                memory = prev[0..size];
                lastState = size;
            }
            lastState -= 2*matches.length;
            auto pm = cast(size_t[])matches;
            pm[] = memory[lastState .. lastState+2*matches.length];
            lastState -= stateSize;
            State* state = cast(State*)&memory[lastState];
            index = state.index;
            pc = state.pc;
            counter = state.counter;
            infiniteNesting = state.infiniteNesting;
            debug(fred_matching)
            {
                writefln("Restored matches", front, s[index .. s.lastIndex]);
                foreach(i, m; matches)
                    writefln("Sub(%d) : %s..%s", i, m.begin, m.end);
            }
            s.reset(index);
            next();
            debug(fred_matching)
                writefln("Backtracked (pc=%s) front: %s src: %s", pc, front, s[index..s.lastIndex]);
            return true;
        }

        /+
            Match subexpression against input, executing re.ir backwards.
            Results are stored in matches
        +/
        bool matchBackImpl()
        {
            pc = cast(uint)re.ir.length-1;
            counter = 0;
            lastState = 0;
            infiniteNesting = -1;//intentional
            auto start = index;
            debug(fred_matching) writeln("Try matchBack at ",retro(s[index..s.lastIndex]));
            for(;;)
            {
                debug(fred_matching) writefln("PC: %s\tCNT: %s\t%s \tfront: %s src: %s", pc, counter, disassemble(re.ir, pc, re.dict), front, retro(s[index..s.lastIndex]));
                switch(re.ir[pc].code)
                {
                case IR.OrChar://assumes IRL!(OrChar) == 1
                    if(atEnd)
                        goto L_backtrack;
                    uint len = re.ir[pc].sequence;
                    uint end = pc - len;
                    if(re.ir[pc].data != front && re.ir[pc-1].data != front)
                    {
                        for(pc = pc-2; pc>end; pc--)
                            if(re.ir[pc].data == front)
                                break;
                        if(pc == end)
                            goto L_backtrack;
                    }
                    pc = end;
                    next();
                    break;
                case IR.Char:
                    if(atEnd || front != re.ir[pc].data)
                        goto L_backtrack;
                    pc--;
                    next();
                break;
                case IR.Any:
                    if(atEnd)
                        goto L_backtrack;
                    pc--;
                    next();
                    break;
                case IR.CodepointSet:
                    if(atEnd || !re.charsets[re.ir[pc].data].scanFor(front))
                        goto L_backtrack;
                    next();
                    pc--;
                    break;
                case IR.Trie:
                    if(atEnd || !re.tries[re.ir[pc].data][front])
                        goto L_backtrack;
                    next();
                    pc--;
                    break;
                case IR.Wordboundary:
                    dchar back;
                    DataIndex bi;
                    //at start & end of input
                    if(atStart && wordTrie[front])
                    {
                        pc--;
                        break;
                    }
                    else if(atEnd && s.loopBack.nextChar(back, bi)
                            && wordTrie[back])
                    {
                        pc--;
                        break;
                    }
                    else if(s.loopBack.nextChar(back, index))
                    {
                        bool af = wordTrie[front];
                        bool ab = wordTrie[back];
                        if(af ^ ab)
                        {
                            pc--;
                            break;
                        }
                    }
                    goto L_backtrack;
                    break;
                case IR.Notwordboundary:
                    dchar back;
                    DataIndex bi;
                    //at start & end of input
                    if(atStart && wordTrie[front])
                        goto L_backtrack;
                    else if(atEnd && s.loopBack.nextChar(back, bi)
                            && wordTrie[back])
                        goto L_backtrack;
                    else if(s.loopBack.nextChar(back, index))
                    {
                        bool af = wordTrie[front];
                        bool ab = wordTrie[back];
                        if(af ^ ab)
                            goto L_backtrack;
                    }
                    pc--;
                    break;
                case IR.Bol:
                    dchar back;
                    DataIndex bi;
                    if(atStart)
                        pc--;
                    else if((re.flags & RegexOption.multiline)
                        && s.loopBack.nextChar(back,bi)
                        && endOfLine(back, front == '\n'))
                    {
                        pc--;
                    }
                    else
                        goto L_backtrack;
                    break;
                case IR.Eol:
                    dchar back;
                    DataIndex bi;
                    debug(fred_matching) writefln("EOL (front 0x%x) %s", front, s[index..s.lastIndex]);
                    //no matching inside \r\n
                    if((re.flags & RegexOption.multiline)
                            && s.loopBack.nextChar(back,bi)
                        && endOfLine(front, back == '\r'))
                    {
                        pc -= IRL!(IR.Eol);
                    }
                    else
                        goto L_backtrack;
                    break;
                case IR.InfiniteStart, IR.InfiniteQStart:
                    uint len = re.ir[pc].data;
                    assert(infiniteNesting < trackers.length);
                    if(trackers[infiniteNesting] == index)
                    {//source not consumed
                        pc--; //out of loop
                        infiniteNesting--;
                        break;
                    }
                    else
                        trackers[infiniteNesting] = index;
                    if(re.ir[pc].code == IR.InfiniteStart)//greedy
                    {
                        infiniteNesting--;
                        pushState(pc-1, counter);//out of loop
                        infiniteNesting++;
                        pc += len;
                    }
                    else
                    {
                        pushState(pc+len, counter);
                        pc--;
                        infiniteNesting--;
                    }
                    break;
                case IR.InfiniteEnd:
                case IR.InfiniteQEnd://now it's a start
                    uint len = re.ir[pc].data;
                    trackers[infiniteNesting+1] = index;
                    pc -= len+IRL!(IR.InfiniteStart);
                    assert(re.ir[pc].code == IR.InfiniteStart || re.ir[pc].code == IR.InfiniteQStart);
                    debug(fred_matching) writeln("(backmatch) Infinite nesting:", infiniteNesting);
                    if(re.ir[pc].code == IR.InfiniteStart)//greedy
                    {
                        pushState(pc-1, counter);
                        infiniteNesting++;
                        pc += len;
                    }
                    else
                    {
                        infiniteNesting++;
                        pushState(pc + len, counter);
                        infiniteNesting--;
                        pc--;
                    }
                    break;
                case IR.RepeatStart, IR.RepeatQStart:
                    uint len = re.ir[pc].data;
                    uint tail = pc + len + 1;
                    uint step =  re.ir[tail+2].raw;
                    uint min = re.ir[tail+3].raw;
                    uint max = re.ir[tail+4].raw;
                    if(counter < min)
                    {
                        counter += step;
                        pc += len;
                    }
                    else if(counter < max)
                    {
                        if(re.ir[pc].code == IR.RepeatStart)//greedy
                        {
                            pushState(pc-1, counter%step);
                            counter += step;
                            pc += len;
                        }
                        else
                        {
                            pushState(pc + len, counter + step);
                            counter = counter%step;
                            pc--;
                        }
                    }
                    else
                    {
                        counter = counter%step;
                        pc--;
                    }
                    break;
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    pc -= re.ir[pc].data+IRL!(IR.RepeatStart);
                    assert(re.ir[pc].code == IR.RepeatStart || re.ir[pc].code == IR.RepeatQStart);
                    goto case IR.RepeatStart;
                case IR.OrEnd:
                    uint len = re.ir[pc].data;
                    pc -= len;
                    assert(re.ir[pc].code == IR.Option);
                    len = re.ir[pc].data;
                    auto pc_save = pc+len-1;
                    pc = pc + len + IRL!(IR.Option);
                    while(re.ir[pc].code == IR.Option)
                    {
                        pushState(pc-IRL!(IR.GotoEndOr)-1, counter);
                        len = re.ir[pc].data;
                        pc += len + IRL!(IR.Option);
                    }
                    assert(re.ir[pc].code == IR.OrEnd);
                    pc--;
                    if(pc != pc_save)
                    {
                        pushState(pc, counter);
                        pc = pc_save;
                    }
                    break;
                case IR.OrStart:
                    assert(0);
                case IR.Option:
                    assert(re.ir[pc].code == IR.Option);
                    pc += re.ir[pc].data + IRL!(IR.Option);
                    if(re.ir[pc].code == IR.Option)
                    {
                        pc--;//hackish, assumes size of IR.Option == 1
                        if(re.ir[pc].code == IR.GotoEndOr)
                        {
                            pc += re.ir[pc].data + IRL!(IR.GotoEndOr);
                        }

                    }
                    assert(re.ir[pc].code == IR.OrEnd);
                    pc -= re.ir[pc].data + IRL!(IR.OrStart)+1;
                    break;
                case IR.GotoEndOr:
                    assert(0);
                case IR.GroupStart:
                    uint n = re.ir[pc].data;
                    matches[n].begin = index;
                    debug(fred_matching)  writefln("IR group #%u starts at %u", n, index);
                    pc --;
                    break;
                case IR.GroupEnd:
                    uint n = re.ir[pc].data;
                    matches[n].end = index;
                    debug(fred_matching) writefln("IR group #%u ends at %u", n, index);
                    pc --;
                    break;
                case IR.LookaheadStart:
                case IR.NeglookaheadStart:
                    assert(0);
                case IR.LookaheadEnd:
                case IR.NeglookaheadEnd:
                    uint len = re.ir[pc].data;
                    pc -= len + IRL!(IR.LookaheadStart);
                    auto prog = re;
                    uint ms = re.ir[pc+1].raw, me = re.ir[pc+2].raw;
                    prog.ir = re.ir[pc+IRL!(IR.LookaheadStart) .. pc+IRL!(IR.LookaheadStart)+len+IRL!(IR.LookaheadEnd)];
                    prog.ngroup = me - ms;
                    auto a = newRegionAllocator();
                    auto matcher = BacktrackingMatcher!(Char, typeof(s.loopBack))(prog, s.loopBack, &a);
                    matcher.matches = matches[ms .. me];
                    matcher.backrefed  = backrefed.empty ? matches : backrefed;
                    bool match = matcher.matchImpl() ^ (re.ir[pc].code == IR.NeglookaheadStart);
                    if(!match)
                        goto L_backtrack;
                    else
                    {
                        pc --;
                    }
                    break;
                case IR.LookbehindEnd:
                case IR.NeglookbehindEnd:
                    uint len = re.ir[pc].data;
                    pc -= len + IRL!(IR.LookbehindStart);
                    auto save = index;
                    uint ms = re.ir[pc+1].raw, me = re.ir[pc+2].raw;
                    auto matcher = this;
                    auto a = newRegionAllocator();
                    matcher.alloc = &a;
                    matcher.re.ngroup =  me - ms;
                    matcher.memory = null;
                    matcher.newStack();
                    matcher.matches = matches[ms .. me];
                    matcher.backrefed = backrefed.empty ? matches : backrefed;
                    matcher.re.ir = re.ir[pc .. pc+IRL!(IR.LookbehindStart)+len];
                    bool match = matcher.matchBackImpl() ^ (re.ir[pc].code == IR.NeglookbehindStart);
                    s.reset(save);
                    next();
                    if(!match)
                        goto L_backtrack;
                    else
                    {
                        pc --;
                    }
                    break;
                case IR.Backref:
                    uint n = re.ir[pc].data;
                    auto referenced = re.ir[pc].localRef
                            ? s[matches[n].begin .. matches[n].end]
                            : s[backrefed[n].begin .. backrefed[n].end];
                    while(!atEnd && !referenced.empty && front == referenced.front)
                    {
                        next();
                        referenced.popFront();
                    }
                    if(referenced.empty)
                        pc--;
                    else
                        goto L_backtrack;
                    break;
                    case IR.Nop:
                    pc --;
                    break;
                case IR.LookbehindStart:
                case IR.NeglookbehindStart:
                    return true;
                default:
                    assert(re.ir[pc].code < 0x80);
                    pc --; //data
                    break;
                L_backtrack:
                    if(!popState())
                    {
                        s.reset(start);
                        return false;
                    }
                }
            }
            return true;
        }
    }
}

//state of codegenerator
struct CtState
{
    string code;
    int addr;
}

//very shitty string formatter, $$ replaced with next argument converted to string
@trusted string ctSub( U...)(string format, U args)
{
    bool seenDollar;
    foreach(i, ch; format)
    {
        if(ch == '$')
        {
            if(seenDollar)
            {
                static if(args.length > 0)
                {
                    return  format[0..i-1] ~ to!string(args[0]) ~ ctSub(format[i+1..$], args[1..$]);
                }
                else
                    assert(0);
            }
            else
                seenDollar = true;
        }
        else
            seenDollar = false;

    }
    return format;
}

//
CtState ctGenBlock(Bytecode[] ir, int addr)
{
    CtState result;
    result.addr = addr;
    while(!ir.empty)
    {
        auto n = ctGenGroup(ir, result.addr);
        result.code ~= n.code;
        result.addr = n.addr;
    }
    return result;
}

//
CtState ctGenGroup(ref Bytecode[] ir, int addr)
{
    CtState r;
    assert(!ir.empty);
    switch(ir[0].code)
    {
    case IR.InfiniteStart, IR.InfiniteQStart, IR.RepeatStart, IR.RepeatQStart:
        uint len = ir[0].data;
        auto nir = ir[ir[0].length .. ir[0].length+len];
        r = ctGenBlock(nir, addr+1);
        //start/end codegen
        //r.addr is at last test+ jump of loop, addr+1 is body of loop
        nir = ir[ir[0].length+len..$];
        r.code = ctGenFixupCode(ir[0..ir[0].length], addr, r.addr) ~ r.code;
        r.code ~= ctGenFixupCode(nir, r.addr, addr+1);
        r.addr += 1;   //account end instruction
        ir = nir;
        break;
    case IR.OrStart:
        uint len = ir[0].data;
        auto nir = ir[ir[0].length .. ir[0].length+len];
        r = ctGenAlternation(nir, addr);
        ir = ir[ir[0].length+len..$];
        assert(ir[0].code == IR.OrEnd);
        ir = ir[ir[0].length..$];
        break;
    default:
        assert(ir[0].isAtom,  text(ir[0].mnemonic));
        r = ctGenAtom(ir, addr);
    }
    return r;
}

//generate source for bytecode contained  in OrStart ... OrEnd
CtState ctGenAlternation(Bytecode[] ir, int addr)
{
    CtState[] pieces;
    CtState r;
    enum optL = IRL!(IR.Option);
    for(;;)
    {
        assert(ir[0].code == IR.Option);
        auto len = ir[0].data;
        auto nir = ir[optL .. optL+len-IRL!(IR.GotoEndOr)];
        if(optL+len < ir.length  && ir[optL+len].code == IR.Option)//not a last option
        {
            r = ctGenBlock(nir, addr+1);
            //r.addr+1 to account GotoEndOr  at end of branch
            r.code = ctGenFixupCode(ir[0 .. ir[0].length], addr, r.addr+1) ~ r.code;
            addr = r.addr+1;//leave space for GotoEndOr
            pieces ~= r;
            ir = ir[optL+len..$];
        }
        else
        {
            pieces ~= ctGenBlock(ir[optL..$], addr);
            addr = pieces[$-1].addr;
            break;
        }

    }
    r = pieces[0];
    for(uint i=1; i<pieces.length; i++)
    {
        r.code ~= ctSub(`
            case $$:
                goto case $$; `, pieces[i-1].addr, addr);
        r.code ~= pieces[i].code;
    }
    r.addr = addr;
    return r;
}

// genereate fixup code for instruction in ir,
// fixup means it has an altrenative way for control flow
string ctGenFixupCode(ref Bytecode[] ir, int addr, int fixup)
{
    string r;
    string testCode;
    switch(ir[0].code)
    {
    case IR.InfiniteStart, IR.InfiniteQStart:
        r = ctSub( `
            case $$:
                trackers[++infiniteNesting] = DataIndex.max;
                goto case $$;`, addr, fixup);
        ir = ir[ir[0].length..$];
        break;
    case IR.InfiniteEnd:
        testCode = ctQuickTest(ir[IRL!(IR.InfiniteEnd)..$],addr+1);
        r = ctSub( `
            case $$:
                debug(fred_matching) writeln("Infinited nesting:", infiniteNesting);
                assert(infiniteNesting < trackers.length);
                if(trackers[infiniteNesting] == index)
                {//source not consumed
                    infiniteNesting--;
                    goto case $$;
                }
                trackers[infiniteNesting] = index;

                infiniteNesting--;
                $$
                    pushState($$, counter);
                infiniteNesting++;
                goto case $$;`, addr, addr+1, testCode, addr+1, fixup);
        ir = ir[ir[0].length..$];
        break;
    case IR.InfiniteQEnd:
        testCode = ctQuickTest(ir[IRL!(IR.InfiniteEnd)..$],addr+1);
        r = ctSub( `
            case $$:
                debug(fred_matching) writeln("Infinited nesting:", infiniteNesting);
                assert(infiniteNesting < trackers.length);
                if(trackers[infiniteNesting] == index)
                {//source not consumed
                    infiniteNesting--;
                    goto case $$;
                }
                trackers[infiniteNesting] = index;

                $$
                {
                    pushState($$, counter);
                    infiniteNesting--;
                    goto case $$;
                }
                else
                    goto case $$;`, addr, addr+1, testCode, fixup, addr+1, fixup);
        ir = ir[ir[0].length..$];
        break;
    case IR.RepeatStart, IR.RepeatQStart:
        r = ctSub( `
            case $$:
                goto case $$;`, addr, fixup);
        ir = ir[ir[0].length..$];
        break;
     case IR.RepeatEnd, IR.RepeatQEnd:
        //len, step, min, max
        uint len = ir[0].data;
        uint step = ir[2].raw;
        uint min = ir[3].raw;
        uint max = ir[4].raw;
        r = ctSub(`
            case $$:
                if(counter < $$)
                {
                    debug(fred_matching) writeln("RepeatEnd min case pc=", $$);
                    counter += $$;
                    goto case $$;
                }`,  addr, min, addr, step, fixup);
        if(ir[0].code == IR.RepeatEnd)
        {
            r ~= ctSub(`
                else if(counter < $$)
                {
                        pushState($$, counter % $$);
                        counter += $$;
                        goto case $$;
                }`, max, addr+1, step, step, fixup);
        }
        else
        {
            r ~= ctSub(`
                else if(counter < $$)
                {
                    pushState($$, counter + $$);
                    counter = counter % $$;
                    goto case $$;
                }`, max, fixup, step, step, addr+1);
        }
        r ~= ctSub(`
                else
                {
                    counter = counter % $$;
                    goto case $$;
                }`, step, addr+1);
        ir = ir[ir[0].length..$];
        break;
    case IR.Option:
            r ~= ctSub( `
            case $$:
                pushState($$, counter);
                goto case;`, addr, fixup);
            ir = ir[ir[0].length..$];
            break;
    default:
        assert(0, text(ir[0].mnemonic));
    }
    return r;
}


string ctQuickTest(Bytecode[] ir, int id)
{
    uint pc=0;
    while(pc < ir.length && ir[pc].isAtom)
    {
        if(ir[pc].code == IR.GroupStart || ir[pc].code == IR.GroupEnd)
        {
            pc++;
        }
        else
        {
            auto code = ctAtomCode(ir[pc..$], -1);
            return ctSub(`
                int test_$$()
                {
                    $$
                }
                if(test_$$() >= 0)`, id, code ? code : "return -1;", id);
        }
    }
    return "";
}

//process & generate source for the simple bytecodes at front of ir using address addr
CtState ctGenAtom(ref Bytecode[] ir, int addr)
{
    CtState result;
    result.code = ctAtomCode(ir, addr);
    ir.popFrontN(ir[0].code == IR.OrChar ? ir[0].sequence : ir[0].length);
    result.addr = addr + 1;
    return result;
}

//D code for atom at ir using address addr, addr < 0 means quickTest
string ctAtomCode(Bytecode[] ir, int addr)
{
    string code;
    string bailOut, nextInstr;
    if(addr < 0)
    {
        bailOut = "return -1;";
        nextInstr = "return 0;";
    }
    else
    {
        bailOut = "goto L_backtrack;";
        nextInstr = ctSub("goto case $$;", addr+1);
        code ~=  ctSub( `
             case $$:
                `, addr);
    }
    switch(ir[0].code)
    {
    case IR.OrChar://assumes IRL!(OrChar) == 1
        code ~=  ctSub(`
                if(atEnd)
                    $$`, bailOut);
        uint len = ir[0].sequence;
        for(uint i = 0; i<len; i++)
        {
            code ~= ctSub( `
                if(front == $$)
                {
                    $$
                    $$
                }`,   ir[i].data, addr >= 0 ? "next();" :"", nextInstr);
        }
        code ~= ctSub( `
            $$`, bailOut);
        break;
    case IR.Char:
        code ~= ctSub( `
                if(atEnd || front != $$)
                    $$
                $$
                $$`, ir[0].data, bailOut, addr >= 0 ? "next();" :"", nextInstr);
        break;
    case IR.Any:
        code ~= ctSub( `
                if(atEnd)
                    $$
                $$
                $$`, bailOut, addr >= 0 ? "next();" :"",nextInstr);
        break;
    case IR.CodepointSet:
        code ~= ctSub( `
                if(atEnd || !re.charsets[$$].scanFor(front))
                    $$
                $$
            $$`, ir[0].data, bailOut, addr >= 0 ? "next();" :"", nextInstr);
        break;
    case IR.Trie:
        code ~= ctSub( `
                if(atEnd || !re.tries[$$][front])
                    $$
                $$
            $$`, ir[0].data, bailOut, addr >= 0 ? "next();" :"", nextInstr);
        break;
    case IR.Wordboundary:
        code ~= ctSub( `
                dchar back;
                DataIndex bi;
                if(atStart && wordTrie[front])
                {
                    $$
                }
                else if(atEnd && s.loopBack.nextChar(back, bi)
                        && wordTrie[back])
                {
                    $$
                }
                else if(s.loopBack.nextChar(back, bi))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back];
                    if(af ^ ab)
                    {
                        $$
                    }
                }
                $$`
            , nextInstr, nextInstr, nextInstr, bailOut);
        break;
    case IR.Notwordboundary:
        code ~= ctSub( `
                dchar back;
                DataIndex bi;
                //at start & end of input
                if(atStart && wordTrie[front])
                    $$
                else if(atEnd && s.loopBack.nextChar(back, bi)
                        && wordTrie[back])
                    $$
                else if(s.loopBack.nextChar(back, index))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back];
                    if(af ^ ab)
                        $$
                }
                $$`
            , bailOut, bailOut, bailOut, nextInstr);

        break;
    case IR.Bol:
        code ~= ctSub(`
                dchar back;
                DataIndex bi;
                if(atStart || ((re.flags & RegexOption.multiline)
                    && s.loopBack.nextChar(back,bi)
                    && endOfLine(back, front == '\n')))
                {
                    $$
                }
                else
                    $$
        `, nextInstr, nextInstr, bailOut);

        break;
    case IR.Eol:
        code ~= ctSub(`
                dchar back;
                DataIndex bi;
                debug(fred_matching) writefln("EOL (front 0x%x) %s", front, s[index..s.lastIndex]);
                //no matching inside \r\n
                if(atEnd || ((re.flags & RegexOption.multiline)
                         && s.loopBack.nextChar(back,bi)
                        && endOfLine(front, back == '\r')))
                {
                    $$
                }
                else
                    $$`, nextInstr, bailOut);

        break;
    case IR.GroupStart:
        code ~= ctSub(`
                matches[$$].begin = index;
                $$`, ir[0].data, nextInstr);

        break;
    case IR.GroupEnd:
        code ~= ctSub(`
                matches[$$].end = index;
                $$`, ir[0].data, nextInstr);
        break;
    case IR.Backref:
        string mStr = ir[0].localRef
            ? ctSub("matches[$$].begin .. matches[$$].end];", ir[0].data, ir[0].data)
            : ctSub("s[backrefed[$$].begin .. backrefed[$$].end];",ir[0].data, ir[0].data);
        code ~= ctSub( `
                $$
                while(!atEnd && !referenced.empty && front == referenced.front)
                {
                    next();;
                    referenced.popFront();
                }
                if(referenced.empty)
                    $$
                else
                    $$`, mStr, nextInstr, bailOut);
        break;
    case IR.Nop:
    case IR.End:
        break;
    default:
        assert(0, text(ir[0].mnemonic, "is not supported yet"));
    }
    return code;
}

//generate D code for the whole regex
string ctGenRegEx(Bytecode[] ir)
{
    auto r = `
        with(matcher)
        {
        pc = 0;
        counter = 0;
        lastState = 0;
        infiniteNesting = -1;//intentional
        auto start = s._index;
        debug(fred_matching) writeln("Try CT matching  starting at ",s[index..s.lastIndex]);
    StartLoop:
        switch(pc)
        {
    `;
    auto bdy = ctGenBlock(ir, 0);
    r ~= bdy.code;
    r ~= ctSub(`
            case $$: break;`,bdy.addr);
    r ~= `
        default:
            assert(0);
        L_backtrack:
            if(!popState())
            {
                s.reset(start);
                return false;
            }
            else
                goto StartLoop;
        }
        return true;
        }
    `;
    return r;
}

//State of VM thread
struct Thread(DataIndex)
{
    Thread* next;    //intrusive linked list
    uint pc;
    uint counter;    //loop counter
    uint uopCounter; //counts micro operations inside one macro instruction (e.g. BackRef)
    Group!DataIndex[1] matches;
}

//head-tail singly-linked list
struct ThreadList(DataIndex)
{
    Thread!DataIndex* tip=null, toe=null;
    //add new thread to the start of list
    void insertFront(Thread!DataIndex* t)
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
    }
    //add new thread to the end of list
    void insertBack(Thread!DataIndex* t)
    {
        if(toe)
        {
            toe.next = t;
            toe = t;
        }
        else
            tip = toe = t;
        toe.next = null;
    }
    //move head element out of list
    Thread!DataIndex* fetch()
    {
        auto t = tip;
        if(tip == toe)
            tip = toe = null;
        else
            tip = tip.next;
        return t;
    }
    //non-destructive iteration of ThreadList
    struct ThreadRange
    {
        const(Thread!DataIndex)* ct;
        this(ThreadList tlist){ ct = tlist.tip; }
        @property bool empty(){ return ct is null; }
        @property const(Thread!DataIndex)* front(){ return ct; }
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

//direction parameter for thompson one-shot match evaluator
enum OneShot { Fwd, Bwd };

/+
   Thomspon matcher does all matching in lockstep,
   never looking at the same char twice
+/
@trusted struct ThompsonMatcher(Char, Stream=Input!Char)
    if(is(Char : dchar))
{
    alias Stream.DataIndex DataIndex;
    alias const(Char)[] String;
    enum threadAllocSize = 16;
    Thread!DataIndex* freelist;
    ThreadList!DataIndex clist, nlist;
    DataIndex[] merge;
    Group!DataIndex[] backrefed;
    Regex!Char re;           //regex program
    Stream s;
    dchar front;
    DataIndex index;
    DataIndex genCounter;    //merge trace counter, goes up on every dchar
    size_t threadSize;
    bool matched;
    bool exhausted;
    Allocator* alloc;
    static if(__traits(hasMember,Stream, "search"))
    {
        enum kicked = true;
    }
    else
        enum kicked = false;

    //true if it's start of input
    @property bool atStart(){   return index == 0; }

    //true if it's end of input
    @property bool atEnd(){  return index == s.lastIndex && s.atEnd; }

    bool next()
    {
        if(!s.nextChar(front, index))
        {
            index =  s.lastIndex;
            return false;
        }
        return true;
    }

    bool search()
    {
        static if(kicked)
        {
            if(!s.search(re.kickstart, front, index))
            {
                index = s.lastIndex;
                return false;
            }
            return true;
        }
        assert(0);
    }

    this()(Regex!Char program, Stream stream, Allocator* allocator)
    {
        re = program;
        s = stream;
        alloc = allocator;
        threadSize = re.ngroup ? (Thread!DataIndex).sizeof+(re.ngroup-1)*(Group!DataIndex).sizeof : (Thread!DataIndex).sizeof - (Group!DataIndex).sizeof;
        reserve(re.threadCount);
        if(re.hotspotTableSize)
            merge = alloc.newArray!(DataIndex[])(re.hotspotTableSize);
        genCounter = 0;
    }

    this(S)(ref ThompsonMatcher!(Char,S) matcher, Bytecode[] piece, Stream stream)
    {
        s = stream;
        re = matcher.re;
        re.ir = piece;
        alloc = matcher.alloc;
        threadSize = matcher.threadSize;
        merge = matcher.merge;
        genCounter = matcher.genCounter;
        freelist = matcher.freelist;
    }

    this(this)
    {
        merge = merge.dup;
        debug(fred_allocation) writeln("ThompsonVM postblit!");
        //free list is  efectively shared ATM
    }

    enum MatchResult{
        NoMatch,
        PartialMatch,
        Match,
    }

    //match the input and fill matches
    bool match(Group!DataIndex[] matches)
    {
        debug(fred_matching)
        {
            writeln("------------------------------------------");
        }
        if(exhausted)
           return false;
        if(re.flags & RegexInfo.oneShot)
        {
            next(); // why this is not
            exhausted = true;
            return matchOneShot!(OneShot.Fwd)(matches)==MatchResult.Match;
        }
        static if(kicked)
        {
            auto searchFn = re.kickstart.empty ? &this.next : &this.search;
        }
        else
            auto searchFn = &this.next;
        if((!matched) && clist.empty)
        {
           searchFn();
        }
        else//char in question is  fetched in prev call to match
        {
            matched = false;
        }
        //assert(clist == (ThreadList!DataIndex).init);
        //assert(nlist == (ThreadList!DataIndex).init);
        if(!atEnd)//if no char
            for(;;)
            {
                genCounter++;
                debug(fred_matching)
                {
                    writefln("Threaded matching threads at  %s", s[index..s.lastIndex]);
                    foreach(t; clist[])
                    {
                        assert(t);
                        writef("pc=%s ",t.pc);
                        write(t.matches);
                        writeln();
                    }
                }
                for(Thread!DataIndex* t = clist.fetch(); t; t = clist.fetch())
                {
                    eval!true(t, matches);
                }
                if(!matched)//if we already have match no need to push the engine
                    eval!true(createStart(index), matches);//new thread staring at this position
                else if(nlist.empty)
                {
                    debug(fred_matching) writeln("Stopped  matching before consuming full input");
                    break;//not a partial match for sure
                }
                clist = nlist;
                nlist = (ThreadList!DataIndex).init;
                if(clist.tip is null)
                {
                    if(!searchFn())
                        break;
                }
                else if(!next()){
                    if (!atEnd) return false;
                    break;
                }
            }
        else
            exhausted = true;
        genCounter++; //increment also on each end
        debug(fred_matching) writefln("Threaded matching threads at end");
        //try out all zero-width posibilities
        if(!matched)
            eval!false(createStart(index), matches);//new thread starting at end of input
        for(Thread!DataIndex* t = clist.fetch(); t; t = clist.fetch())
        {
            eval!false(t, matches);
        }
        if(matched && !(re.flags & RegexOption.global))
           exhausted = true;
        return matched;
    }

    /+
        handle succesful threads
    +/
    void finish(const(Thread!DataIndex)* t, Group!DataIndex[] matches)
    {
        matches.ptr[0..re.ngroup] = t.matches.ptr[0..re.ngroup];
        debug(fred_matching)
        {
            writef("FOUND pc=%s prog_len=%s",
                    t.pc, re.ir.length);
            if(!matches.empty)
                writefln(": %s..%s", matches[0].begin, matches[0].end);
            foreach(v; matches)
                writefln("%d .. %d", v.begin, v.end);
        }
        matched = true;
    }

    /+
        match thread against codepoint, cutting trough all 0-width instructions
        and taking care of control flow, then add it to nlist
    +/
    void eval(bool withInput)(Thread!DataIndex* t, Group!DataIndex[] matches)
    {
        ThreadList!DataIndex worklist;
        debug(fred_matching) writeln("Evaluating thread");
        for(;;)
        {
            debug(fred_matching)
            {
                writef("\tpc=%s [", t.pc);
                foreach(x; worklist[])
                    writef(" %s ", x.pc);
                writeln("]");
            }
            switch(re.ir[t.pc].code)
            {
            case IR.End:
                finish(t, matches);
                matches[0].end = index; //fix endpoint of the whole match
                recycle(t);
                //cut off low priority threads
                recycle(clist);
                recycle(worklist);
                debug(fred_matching) writeln("Finished thread ", matches);
                return;
            case IR.Wordboundary:
                dchar back;
                DataIndex bi;
                //at start & end of input
                if(atStart && wordTrie[front])
                {
                    t.pc += IRL!(IR.Wordboundary);
                    break;
                }
                else if(atEnd && s.loopBack.nextChar(back, bi)
                        && wordTrie[back])
                {
                    t.pc += IRL!(IR.Wordboundary);
                    break;
                }
                else if(s.loopBack.nextChar(back, index))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back];
                    if(af ^ ab)
                    {
                        t.pc += IRL!(IR.Wordboundary);
                        break;
                    }
                }
                recycle(t);
                t = worklist.fetch();
                if(!t)
                    return;
                break;
            case IR.Notwordboundary:
                dchar back;
                DataIndex bi;
                //at start & end of input
                if(atStart && wordTrie[front])
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else if(atEnd && s.loopBack.nextChar(back, bi)
                        && wordTrie[back])
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else if(s.loopBack.nextChar(back, index))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back]  != 0;
                    if(af ^ ab)
                    {
                        recycle(t);
                        t = worklist.fetch();
                        if(!t)
                            return;
                        break;
                    }
                }
                t.pc += IRL!(IR.Wordboundary);
                break;
            case IR.Bol:
                dchar back;
                DataIndex bi;
                if(atStart
                    ||( (re.flags & RegexOption.multiline)
                    && s.loopBack.nextChar(back,bi)
                    && startOfLine(back, front == '\n')))
                {
                    t.pc += IRL!(IR.Bol);
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                }
                break;
            case IR.Eol:
                debug(fred_matching) writefln("EOL (front 0x%x) %s",  front, s[index..s.lastIndex]);
                dchar back;
                DataIndex bi;
                //no matching inside \r\n
                if(atEnd || ((re.flags & RegexOption.multiline)
                    && endOfLine(front, s.loopBack.nextChar(back, bi)
                        && back == '\r')))
                {
                    t.pc += IRL!(IR.Eol);
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                }
                break;
            case IR.InfiniteStart, IR.InfiniteQStart:
                t.pc += re.ir[t.pc].data + IRL!(IR.InfiniteStart);
                goto case IR.InfiniteEnd; //both Q and non-Q
            case IR.RepeatStart, IR.RepeatQStart:
                t.pc += re.ir[t.pc].data + IRL!(IR.RepeatStart);
                goto case IR.RepeatEnd; //both Q and non-Q
            case IR.RepeatEnd:
            case IR.RepeatQEnd:
                //len, step, min, max
                uint len = re.ir[t.pc].data;
                uint step =  re.ir[t.pc+2].raw;
                uint min = re.ir[t.pc+3].raw;
                if(t.counter < min)
                {
                    t.counter += step;
                    t.pc -= len;
                    break;
                }
                if(merge[re.ir[t.pc + 1].raw+t.counter] < genCounter)
                {
                    debug(fred_matching) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    merge[re.ir[t.pc + 1].raw+t.counter] = genCounter;
                }
                else
                {
                    debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                uint max = re.ir[t.pc+4].raw;
                if(t.counter < max)
                {
                    if(re.ir[t.pc].code == IR.RepeatEnd)
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
                if(merge[re.ir[t.pc + 1].raw+t.counter] < genCounter)
                {
                    debug(fred_matching) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    merge[re.ir[t.pc + 1].raw+t.counter] = genCounter;
                }
                else
                {
                    debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                uint len = re.ir[t.pc].data;
                uint pc1, pc2; //branches to take in priority order
                if(re.ir[t.pc].code == IR.InfiniteEnd)
                {
                    pc1 = t.pc - len;
                    pc2 = t.pc + IRL!(IR.InfiniteEnd);
                }
                else
                {
                    pc1 = t.pc + IRL!(IR.InfiniteEnd);
                    pc2 = t.pc - len;
                }
                static if(withInput)
                {
                    int test = quickTestFwd(pc1, front, re);
                    if(test > 0)
                    {
                        nlist.insertBack(fork(t, pc1 + test, t.counter));
                        t.pc = pc2;
                    }
                    else if(test == 0)
                    {
                        worklist.insertFront(fork(t, pc2, t.counter));
                        t.pc = pc1;
                    }
                    else
                        t.pc = pc2;
                }
                else
                {
                    worklist.insertFront(fork(t, pc2, t.counter));
                    t.pc = pc1;
                }
                break;
            case IR.OrEnd:
                if(merge[re.ir[t.pc + 1].raw+t.counter] < genCounter)
                {
                    debug(fred_matching) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, s[index..s.lastIndex], genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    merge[re.ir[t.pc + 1].raw+t.counter] = genCounter;
                    t.pc += IRL!(IR.OrEnd);
                }
                else
                {
                    debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, s[index..s.lastIndex], genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                }
                break;
            case IR.OrStart:
                t.pc += IRL!(IR.OrStart);
                goto case;
            case IR.Option:
                uint next = t.pc + re.ir[t.pc].data + IRL!(IR.Option);
                //queue next Option
                if(re.ir[next].code == IR.Option)
                {
                    worklist.insertFront(fork(t, next, t.counter));
                }
                t.pc += IRL!(IR.Option);
                break;
            case IR.GotoEndOr:
                t.pc = t.pc + re.ir[t.pc].data + IRL!(IR.GotoEndOr);
                goto case IR.OrEnd;
            case IR.GroupStart:
                uint n = re.ir[t.pc].data;
                t.matches.ptr[n].begin = index;
                t.pc += IRL!(IR.GroupStart);
                break;
            case IR.GroupEnd:
                uint n = re.ir[t.pc].data;
                t.matches.ptr[n].end = index;
                t.pc += IRL!(IR.GroupEnd);
                break;
            case IR.Backref:
                uint n = re.ir[t.pc].data;
                Group!DataIndex* source = re.ir[t.pc].localRef ? t.matches.ptr : backrefed.ptr;
                assert(source);
                if(source[n].begin == source[n].end)//zero-width Backref!
                {
                    t.pc += IRL!(IR.Backref);
                }
                else static if(withInput)
                {
                    size_t idx = source[n].begin + t.uopCounter;
                    size_t end = source[n].end;
                    if(s[idx..end].front == front)
                    {
                        t.uopCounter += std.utf.stride(s[idx..end], 0);
                        if(t.uopCounter + source[n].begin == source[n].end)
                        {//last codepoint
                            t.pc += IRL!(IR.Backref);
                            t.uopCounter = 0;
                        }
                        nlist.insertBack(t);
                    }
                    else
                        recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                break;
            case IR.LookbehindStart:
            case IR.NeglookbehindStart:
                auto matcher =
                    ThompsonMatcher!(Char, typeof(s.loopBack))
                    (this, re.ir[t.pc..t.pc+re.ir[t.pc].data+IRL!(IR.LookbehindStart)], s.loopBack);
                matcher.re.ngroup = re.ir[t.pc+2].raw - re.ir[t.pc+1].raw;
                matcher.backrefed = backrefed.empty ? t.matches : backrefed;
                //backMatch
                matcher.next(); //load first character from behind
                bool match = (matcher.matchOneShot!(OneShot.Bwd)(t.matches)==MatchResult.Match) ^ (re.ir[t.pc].code == IR.LookbehindStart);
                freelist = matcher.freelist;
                genCounter = matcher.genCounter;
                if(match)
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else
                    t.pc += re.ir[t.pc].data + IRL!(IR.LookbehindStart) + IRL!(IR.LookbehindEnd);
                break;
            case IR.LookaheadEnd:
            case IR.NeglookaheadEnd:
                t.pc = re.ir[t.pc].indexOfPair(t.pc);
                assert(re.ir[t.pc].code == IR.LookaheadStart || re.ir[t.pc].code == IR.NeglookaheadStart);
                uint ms = re.ir[t.pc+1].raw, me = re.ir[t.pc+2].raw;
                finish(t, matches.ptr[ms..me]);
                recycle(t);
                //cut off low priority threads
                recycle(clist);
                recycle(worklist);
                return;
            case IR.LookaheadStart:
            case IR.NeglookaheadStart:
                auto save = index;
                uint len = re.ir[t.pc].data;
                uint ms = re.ir[t.pc+1].raw, me = re.ir[t.pc+2].raw;
                bool positive = re.ir[t.pc].code == IR.LookaheadStart;
                auto matcher = ThompsonMatcher(this, re.ir[t.pc .. t.pc+len+IRL!(IR.LookaheadEnd)+IRL!(IR.LookaheadStart)], s);
                matcher.front = front;
                matcher.index = index;
                matcher.re.ngroup = me - ms;
                matcher.backrefed = backrefed.empty ? t.matches : backrefed;
                bool nomatch = (matcher.matchOneShot!(OneShot.Fwd)(t.matches, IRL!(IR.LookaheadStart)) == MatchResult.Match) ^ positive;
                freelist = matcher.freelist;
                genCounter = matcher.genCounter;
                s.reset(index);
                next();
                if(nomatch)
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else
                    t.pc += len + IRL!(IR.LookaheadEnd) + IRL!(IR.LookaheadStart);
                break;
            case IR.LookbehindEnd:
            case IR.NeglookbehindEnd:
                assert(0);
            case IR.Nop:
                t.pc += IRL!(IR.Nop);
                break;
            static if(withInput)
            {
                case IR.OrChar:
                    uint len = re.ir[t.pc].sequence;
                    uint end = t.pc + len;
                    static assert(IRL!(IR.OrChar) == 1);
                    for(; t.pc<end; t.pc++)
                        if(re.ir[t.pc].data == front)
                            break;
                    if(t.pc != end)
                    {
                       t.pc = end;
                        nlist.insertBack(t);
                    }
                    else
                        recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                case IR.Char:
                    if(front == re.ir[t.pc].data)
                    {
                        t.pc += IRL!(IR.Char);
                        nlist.insertBack(t);
                    }
                    else
                        recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                case IR.Any:
                    t.pc += IRL!(IR.Any);
                    nlist.insertBack(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                case IR.CodepointSet:
                    if(re.charsets[re.ir[t.pc].data].scanFor(front))
                    {
                        debug(fred_matching) writeln("CodepointSet passed");
                        t.pc += IRL!(IR.CodepointSet);
                        nlist.insertBack(t);
                    }
                    else
                    {
                        debug(fred_matching) writeln("CodepointSet notpassed");
                        recycle(t);
                    }
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                case IR.Trie:
                    if(re.tries[re.ir[t.pc].data][front])
                    {
                        debug(fred_matching) writeln("Trie passed");
                        t.pc += IRL!(IR.Trie);
                        nlist.insertBack(t);
                    }
                    else
                    {
                        debug(fred_matching) writeln("Trie notpassed");
                        recycle(t);
                    }
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                default:
                    assert(0, "Unrecognized instruction " ~ re.ir[t.pc].mnemonic);
            }
            else
            {
                default:
                    recycle(t);
                    t = worklist.fetch();
                    if(t)
                       break;
                    else
                        return;
            }
            }
        }

    }
    enum uint RestartPc=uint.max;
    //match the input, evaluating IR without searching
    MatchResult matchOneShot(OneShot direction)(Group!DataIndex[] matches, uint startPc=0)
    {
        debug(fred_matching)
        {
            writefln("---------------single shot match %s----------------- ",
                     direction == OneShot.Fwd ? "forward" : "backward");
        }
        static if(direction == OneShot.Fwd)
            alias eval evalFn;
        else
            alias evalBack evalFn;
        assert(clist == (ThreadList!DataIndex).init || startPc==RestartPc); // incorrect after a partial match
        assert(nlist == (ThreadList!DataIndex).init || startPc==RestartPc);
        if(!atEnd)//if no char
        {
            if (startPc!=RestartPc){
                auto startT = createStart(index);
                static if(direction == OneShot.Fwd)
                    startT.pc = startPc;
                else
                    startT.pc = cast(uint)re.ir.length-IRL!(IR.LookbehindEnd);
                genCounter++;
                evalFn!true(startT, matches);
            }
            for(;;)
            {
                genCounter++;
                debug(fred_matching)
                {
                    static if(direction == OneShot.Fwd)
                        writefln("Threaded matching (forward) threads at  %s",  s[index..s.lastIndex]);
                    else
                        writefln("Threaded matching (backward) threads at  %s", retro(s[index..s.lastIndex]));
                    foreach(t; clist[])
                    {
                        assert(t);
                        writef("pc=%s ",t.pc);
                        write(t.matches);
                        writeln();
                    }
                }
                for(Thread!DataIndex* t = clist.fetch(); t; t = clist.fetch())
                {
                    evalFn!true(t, matches);
                }
                if(nlist.empty)
                {
                    debug(fred_matching) writeln("Stopped  matching before consuming full input");
                    break;//not a partial match for sure
                }
                clist = nlist;
                nlist = (ThreadList!DataIndex).init;
                if(!next()){
                    if (!atEnd) return MatchResult.PartialMatch;
                    break;
                }
            }
        }
        genCounter++; //increment also on each end
        debug(fred_matching) writefln("Threaded matching (%s) threads at end",
                                      direction == OneShot.Fwd ? "forward" : "backward");
        //try out all zero-width posibilities
        for(Thread!DataIndex* t = clist.fetch(); t; t = clist.fetch())
        {
            evalFn!false(t, matches);
        }
        return (matched?MatchResult.Match:MatchResult.NoMatch);
    }

    /+
        a version of eval that executes IR backwards
    +/
    void evalBack(bool withInput)(Thread!DataIndex* t, Group!DataIndex[] matches)
    {
        ThreadList!DataIndex worklist;
        debug(fred_matching) writeln("Evaluating thread backwards");
        do
        {
            debug(fred_matching)
            {
                writef("\tpc=%s [", t.pc);
                foreach(x; worklist[])
                    writef(" %s ", x.pc);
                writeln("]");
            }
            debug(fred_matching) writeln(disassemble(re.ir, t.pc));
            switch(re.ir[t.pc].code)
            {
            case IR.Wordboundary:
                dchar back;
                DataIndex bi;
                //at start & end of input
                if(atStart && wordTrie[front])
                {
                    t.pc--;
                    break;
                }
                else if(atEnd && s.loopBack.nextChar(back, bi)
                        && wordTrie[back])
                {
                    t.pc--;
                    break;
                }
                else if(s.loopBack.nextChar(back, index))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back];
                    if(af ^ ab)
                    {
                        t.pc--;
                        break;
                    }
                }
                recycle(t);
                t = worklist.fetch();
                break;
            case IR.Notwordboundary:
                dchar back;
                DataIndex bi;
                //at start & end of input
                if(atStart && wordTrie[front])
                {
                    recycle(t);
                    t = worklist.fetch();
                    break;
                }
                else if(atEnd && s.loopBack.nextChar(back, bi)
                        && wordTrie[back])
                {
                    recycle(t);
                    t = worklist.fetch();
                    break;
                }
                else if(s.loopBack.nextChar(back, index))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back];
                    if(af ^ ab)
                    {
                        recycle(t);
                        t = worklist.fetch();
                        break;
                    }
                }
                t.pc--;
                break;
            case IR.Bol:
                dchar back;
                DataIndex bi;
                if(atStart
                    ||((re.flags & RegexOption.multiline)
                    && s.loopBack.nextChar(back,bi)
                    && startOfLine(back, front == '\n')))
                {
                    t.pc--;
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                }
                break;
            case IR.Eol:
                debug(fred_matching) writefln("EOL (front 0x%x) %s", front, s[index..s.lastIndex]);
                dchar back;
                DataIndex bi;
                //no matching inside \r\n
                if((re.flags & RegexOption.multiline)
                    && endOfLine(front, s.loopBack.nextChar(back, bi)
                        && back == '\r'))
                {
                    t.pc--;
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                }
                break;
            case IR.InfiniteStart, IR.InfiniteQStart:
                uint len = re.ir[t.pc].data;
                uint mIdx = t.pc + len + IRL!(IR.InfiniteEnd); //we're always pointed at the tail of instruction
                if(merge[re.ir[mIdx].raw+t.counter] < genCounter)
                {
                    debug(fred_matching) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[mIdx].raw+t.counter] );
                    merge[re.ir[mIdx].raw+t.counter] = genCounter;
                }
                else
                {
                    debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[mIdx].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    break;
                }
                if(re.ir[t.pc].code == IR.InfiniteStart)//greedy
                {
                    worklist.insertFront(fork(t, t.pc-1, t.counter));
                    t.pc += len;
                }
                else
                {
                    worklist.insertFront(fork(t, t.pc+len, t.counter));
                    t.pc--;
                }
                break;
            case IR.InfiniteEnd:
            case IR.InfiniteQEnd://now it's a start
                uint len = re.ir[t.pc].data;
                t.pc -= len+IRL!(IR.InfiniteStart);
                assert(re.ir[t.pc].code == IR.InfiniteStart || re.ir[t.pc].code == IR.InfiniteQStart);
                goto case IR.InfiniteStart;
            case IR.RepeatStart, IR.RepeatQStart:
                uint len = re.ir[t.pc].data;
                uint tail = t.pc + len + IRL!(IR.RepeatStart);
                uint step =  re.ir[tail+2].raw;
                uint min = re.ir[tail+3].raw;

                if(t.counter < min)
                {
                    t.counter += step;
                    t.pc += len;
                    break;
                }
                uint max = re.ir[tail+4].raw;
                if(merge[re.ir[tail+1].raw+t.counter] < genCounter)
                {
                    debug(fred_matching) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[tail+1].raw+t.counter] );
                    merge[re.ir[tail+1].raw+t.counter] = genCounter;
                }
                else
                {
                    debug(fred_matching) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[tail+1].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    break;
                }
                if(t.counter < max)
                {
                    if(re.ir[t.pc].code == IR.RepeatStart)//greedy
                    {
                        worklist.insertFront(fork(t, t.pc-1, t.counter%step));
                        t.counter += step;
                        t.pc += len;
                    }
                    else
                    {
                        worklist.insertFront(fork(t, t.pc + len, t.counter + step));
                        t.counter = t.counter%step;
                        t.pc--;
                    }
                }
                else
                {
                    t.counter = t.counter%step;
                    t.pc--;
                }
                break;
            case IR.RepeatEnd:
            case IR.RepeatQEnd:
                t.pc -= re.ir[t.pc].data+IRL!(IR.RepeatStart);
                assert(re.ir[t.pc].code == IR.RepeatStart || re.ir[t.pc].code == IR.RepeatQStart);
                goto case IR.RepeatStart;
            case IR.OrEnd:
                uint len = re.ir[t.pc].data;
                t.pc -= len;
                assert(re.ir[t.pc].code == IR.Option);
                len = re.ir[t.pc].data;
                t.pc = t.pc + len; //to IR.GotoEndOr or just before IR.OrEnd
                break;
            case IR.OrStart:
                uint len = re.ir[t.pc].data;
                uint mIdx = t.pc + len + IRL!(IR.OrEnd); //should point to the end of OrEnd
                if(merge[re.ir[mIdx].raw+t.counter] < genCounter)
                {
                    debug(fred_matching) writefln("A thread(t.pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[mIdx].raw+t.counter] );
                    merge[re.ir[mIdx].raw+t.counter] = genCounter;
                }
                else
                {
                    debug(fred_matching) writefln("A thread(t.pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[mIdx].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    break;
                }
                t.pc--;
                break;
            case IR.Option:
                assert(re.ir[t.pc].code == IR.Option);
                t.pc += re.ir[t.pc].data + IRL!(IR.Option);
                if(re.ir[t.pc].code == IR.Option)
                {
                    t.pc--;//hackish, assumes size of IR.Option == 1
                    if(re.ir[t.pc].code == IR.GotoEndOr)
                    {
                        t.pc += re.ir[t.pc].data + IRL!(IR.GotoEndOr);
                    }
                }
                assert(re.ir[t.pc].code == IR.OrEnd);
                t.pc -= re.ir[t.pc].data + 1;
                break;
            case IR.GotoEndOr:
                assert(re.ir[t.pc].code == IR.GotoEndOr);
                uint npc = t.pc+IRL!(IR.GotoEndOr);
                assert(re.ir[npc].code == IR.Option);
                worklist.insertFront(fork(t, npc + re.ir[npc].data, t.counter));//queue next branch
                t.pc--;
                break;
            case IR.GroupStart:
                uint n = re.ir[t.pc].data;
                t.matches.ptr[n].begin = index;
                t.pc--;
                break;
            case IR.GroupEnd:
                uint n = re.ir[t.pc].data;
                t.matches.ptr[n].end = index;
                t.pc--;
                break;
            case IR.Backref:
                uint n = re.ir[t.pc].data;
                auto source = re.ir[t.pc].localRef ?  t.matches.ptr : backrefed.ptr;
                assert(source);
                if(source[n].begin == source[n].end)//zero-width Backref!
                {
                    t.pc--;
                }
                else static if(withInput)
                {
                    size_t idx = source[n].begin + t.uopCounter;
                    size_t end = source[n].end;
                    if(s[idx..end].front == front)//could be a BUG in backward matching
                    {
                        t.uopCounter += std.utf.stride(s[idx..end], 0);
                        if(t.uopCounter + source[n].begin == source[n].end)
                        {//last codepoint
                            t.pc--;
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

            case IR.LookbehindStart:
            case IR.NeglookbehindStart:
                uint ms = re.ir[t.pc+1].raw, me = re.ir[t.pc+2].raw;
                finish(t, matches.ptr[ms .. me]);
                recycle(t);
                //cut off low priority threads
                recycle(clist);
                recycle(worklist);
                return;
            case IR.LookaheadStart:
            case IR.NeglookaheadStart:
                assert(0);
            case IR.LookaheadEnd:
            case IR.NeglookaheadEnd:
                uint len = re.ir[t.pc].data;
                t.pc -= len + IRL!(IR.LookaheadStart);
                bool positive = re.ir[t.pc].code == IR.LookaheadStart;
                auto matcher = ThompsonMatcher!(Char, typeof(s.loopBack))
                    (this
                    , re.ir[t.pc .. t.pc+len+IRL!(IR.LookbehindStart)+IRL!(IR.LookbehindEnd)]
                    , s.loopBack);
                matcher.re.ngroup = re.ir[t.pc+2].raw - re.ir[t.pc+1].raw;
                matcher.backrefed = backrefed.empty ? t.matches : backrefed;
                matcher.next(); //fetch a char, since direction was reversed
                bool match = (matcher.matchOneShot!(OneShot.Fwd)(t.matches, IRL!(IR.LookaheadStart)) == MatchResult.Match) ^ positive;
                freelist = matcher.freelist;
                if(match)
                {
                    recycle(t);
                    t = worklist.fetch();
                }
                else
                    t.pc--;
                break;
            case IR.LookbehindEnd:
            case IR.NeglookbehindEnd:
                auto save = index;
                uint len = re.ir[t.pc].data;
                t.pc -= len + IRL!(IR.LookbehindStart);
                uint ms = re.ir[t.pc+1].raw, me = re.ir[t.pc+2].raw;
                bool positive = re.ir[t.pc].code == IR.LookbehindStart;
                auto matcher = ThompsonMatcher(this, re.ir[t.pc .. t.pc+len+IRL!(IR.LookbehindStart)], s);
                matcher.front = front;
                matcher.index = index;
                matcher.re.ngroup = me - ms;
                matcher.backrefed = backrefed.empty ? t.matches : backrefed;
                bool nomatch = (matcher.matchOneShot!(OneShot.Bwd)(t.matches) == MatchResult.Match) ^ positive;
                freelist = matcher.freelist;
                s.reset(index);
                next();
                if(nomatch)
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    //
                }
                else
                    t.pc--;
                break;
            case IR.Nop:
                t.pc--;
                break;
            static if(withInput)
            {
                case IR.OrChar://assumes IRL!(OrChar) == 1
                    uint len = re.ir[t.pc].sequence;
                    uint end = t.pc - len;
                    for(; t.pc>end; t.pc--)
                        if(re.ir[t.pc].data == front)
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
                    if(front == re.ir[t.pc].data)
                    {
                        t.pc--;
                        nlist.insertBack(t);
                    }
                    else
                        recycle(t);
                    t = worklist.fetch();
                    break;
                case IR.Any:
                    t.pc--;
                    nlist.insertBack(t);
                    t = worklist.fetch();
                    break;
                case IR.CodepointSet:
                    if(re.charsets[re.ir[t.pc].data].scanFor(front))
                    {
                        debug(fred_matching) writeln("CodepointSet passed");
                        t.pc--;
                        nlist.insertBack(t);
                    }
                    else
                    {
                        debug(fred_matching) writeln("CodepointSet notpassed");
                        recycle(t);
                    }
                    t = worklist.fetch();
                    break;
                case IR.Trie:
                    if(re.tries[re.ir[t.pc].data][front])
                    {
                        debug(fred_matching) writeln("Trie passed");
                        t.pc--;
                        nlist.insertBack(t);
                    }
                    else
                    {
                        debug(fred_matching) writeln("Trie notpassed");
                        recycle(t);
                    }
                    t = worklist.fetch();
                    break;
                default:
                    assert(re.ir[t.pc].code < 0x80, "Unrecognized instruction " ~ re.ir[t.pc].mnemonic);
                    t.pc--;
            }
            else
            {
                default:
                    if(re.ir[t.pc].code < 0x80)
                        t.pc--;
                    else
                    {
                        recycle(t);
                        t = worklist.fetch();
                    }
            }
            }
        }while(t);
    }

    //get a dirty recycled Thread
    Thread!DataIndex* allocate()
    {
        assert(freelist, "not enough preallocated memory");
        Thread!DataIndex* t = freelist;
        freelist = freelist.next;
        return t;
    }

    //reserve memory for Threads
    void reserve(size_t size)
    {
        void[] mem = alloc.allocate(threadSize*size)[0 .. threadSize*size];
        freelist = cast(Thread!DataIndex*)&mem[0];
        size_t i;
        for(i=threadSize; i<threadSize*size; i+=threadSize)
            (cast(Thread!DataIndex*)&mem[i-threadSize]).next = cast(Thread!DataIndex*)&mem[i];
        (cast(Thread!DataIndex*)&mem[i-threadSize]).next = null;
    }

    //dispose a thread
    void recycle(Thread!DataIndex* t)
    {
        t.next = freelist;
        freelist = t;
    }

    //dispose list of threads
    void recycle(ref ThreadList!DataIndex list)
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

    //creates a copy of master thread with given pc
    Thread!DataIndex* fork(Thread!DataIndex* master, uint pc, uint counter)
    {
        auto t = allocate();
        t.matches.ptr[0..re.ngroup] = master.matches.ptr[0..re.ngroup];
        t.pc = pc;
        t.counter = counter;
        t.uopCounter = 0;
        return t;
    }

    //creates a start thread
    Thread!DataIndex*  createStart(DataIndex index)
    {
        auto t = allocate();
        t.matches.ptr[0..re.ngroup] = (Group!DataIndex).init;
        t.matches[0].begin = index;
        t.pc = 0;
        t.counter = 0;
        t.uopCounter = 0;
        return t;
    }
}

/**
    $(D Captures) object contains submatches captured during a call to $(D match) or iteration over $(D RegexMatch) range.
    First element of range is the whole match.
    Example, showing basic operations on $(D Captures):

    ----
    import fred;
    import std.range;

    auto m = match("@abc#", regex(`(\w)(\w)(\w)`));
    auto c = m.captures;
    assert(c.pre == "@");// part of input preceeding match
    assert(c.post == "#"); // immediately after match
    assert(c.hit == c[0] && c.hit == "abc");// the whole match
    assert(c[2] =="b");
    assert(c.front == "abc");
    c.popFront();
    assert(c.front == "a");
    assert(c.back == "c");
    c.popBack();
    assert(c.back == "b");
    popFrontN(c, 2);
    assert(c.empty);
    ----
*/
@trusted struct Captures(R,DIndex)
    if(isSomeString!R)
{//@trusted because of union inside
    alias DIndex DataIndex;
    alias R String;
private:
    R _input;
    bool _empty;
    enum smallString = 3;
    union
    {
        Group!DataIndex[] big_matches;
        Group!DataIndex[smallString] small_matches;
    }
    uint f, b;
    uint ngroup;
    NamedGroup[] names;

    this(alias Engine)(ref RegexMatch!(R,Engine) rmatch)
    {
        _input = rmatch._input;
        ngroup = rmatch._engine.re.ngroup;
        names = rmatch._engine.re.dict;
        newMatches();
        b = ngroup;
        f = 0;
    }

    @property Group!DataIndex[] matches()
    {
       return ngroup > smallString ? big_matches : small_matches[0..ngroup];
    }

    void newMatches()
    {
        if(ngroup > smallString)
            big_matches = new Group!DataIndex[ngroup];
    }

public:
    ///Slice of input prior to the match.
    @property R pre()
    {
        return _empty ? _input[] : _input[0 .. matches[0].begin];
    }

    ///Slice of input immediately after the match.
    @property R post()
    {
        return _empty ? _input[] : _input[matches[0].end .. $];
    }

    ///Slice of matched portion of input.
    @property R hit()
    {
        assert(!_empty);
        return _input[matches[0].begin .. matches[0].end];
    }

    ///Range interface.
    @property R front()
    {
        assert(!empty);
        return _input[matches[f].begin .. matches[f].end];
    }

    ///ditto
    @property R back()
    {
        assert(!empty);
        return _input[matches[b-1].begin .. matches[b-1].end];
    }

    ///ditto
    void popFront()
    {
        assert(!empty);
        ++f;
    }

    ///ditto
    void popBack()
    {
        assert(!empty);
        --b;
    }

    ///ditto
    @property bool empty() const { return _empty || f >= b; }

    ///ditto
    R opIndex()(size_t i) /*const*/ //@@@BUG@@@
    {
        assert(f+i < b,"requested submatch number is out of range");
        assert(matches[f+i].begin <= matches[f+i].end, text("wrong match: ", matches[f+i].begin, "..", matches[f+i].end));
        return _input[matches[f+i].begin..matches[f+i].end];
    }

    /**
        Lookup named submatch.

        ---
        import fred;
        import std.range;

        auto m = match("a = 42;", regex(`(?P<var>\w+)\s*=\s*(?P<value>\d+);`));
        auto c = m.captures;
        assert(c["var"] == "a");
        assert(c["value"] == "42");
        popFrontN(c, 2);
        //named groups are unaffected by range primitives
        assert(c["var"] =="a"); 
        assert(c.front == "42");
        ----
    */
    R opIndex(String)(String i) /*const*/ //@@@BUG@@@
        if(isSomeString!String)
    {
        size_t index = lookupNamedGroup(names, i);
        return opIndex(index - f);
    }

    ///Number of matches in this object.
    @property size_t length() const { return b-f;  }

    ///A hook for compatibility with std.regex.
    @property ref captures(){ return this; }
}

/**
    A regex engine state, as returned by $(D match) family of functions.
    Effectively it's a forward range of Captures!R, produced 
    by lazily searching for matches in a given input.
    alias Engine specifies an engine type to use during matching,
    and is automatically deduced in a call to $(D match)/$(D bmatch).
*/
@trusted public struct RegexMatch(R, alias Engine=ThompsonMatcher)
    if(isSomeString!R)
{
private:
    alias BasicElementOf!R Char;
    alias Engine!Char EngineType;
    EngineType _engine;
    Allocator _alloc;
    R _input;
    Captures!(R,EngineType.DataIndex) _captures;

    this(Regex!Char prog, R input)
    {
        _input = input;
        auto stack = RegionAllocatorStack(1<<20, GCScan.no);
        _alloc = stack.newRegionAllocator();
        _engine = EngineType(prog, Input!Char(input), &_alloc);
        _captures = Captures!(R,EngineType.DataIndex)(this);
        _captures._empty = !_engine.match(_captures.matches);
    }

    ~this(){}
public:

    ///Shorthands for front.pre, front.post, front.hit.
    @property R pre()
    {
        return _captures.pre;
    }

    ///ditto
    @property R post()
    {
        return _captures.post;
    }

    ///ditto
    @property R hit()
    {
        return _captures.hit;
    }

    /**
        Functionality for processing subsequent matches of global regexes via range interface:
        ---
        import fred;
        auto m = match("Hello, world!", regex(`\w+`, "g"));
        assert(m.front.hit == "Hello");
        m.popFront();
        assert(m.front.hit == "world");
        m.popFront();
        assert(m.empty);
        ---
    */
    @property auto front()
    {
        return _captures;
    }

    ///ditto
    void popFront()
    { //previous one can have escaped references from Capture object
        _engine.alloc = &_alloc;
        _captures.newMatches();
        _captures._empty = !_engine.match(_captures.matches);
    }

    ///ditto
    auto save(){ return this; }

    ///Test if this match object is empty.
    @property bool empty(){ return _captures._empty; }

    ///Same as !(x.empty), provided for its convenience  in conditional statements.
    T opCast(T:bool)(){ return !empty; }

    /// Same as .front, provided for compatibility with original std.regex.
    @property auto captures(){ return _captures; }

}

/**
    Compile regular expression pattern for the later execution.
    Resulting $D(Regex) object works on inputs having same character width as $(D pattern).
    Params:
    pattern = Regular expression
    flags = The _attributes (g, i, m and x accepted)

    Throws: $(D RegexException) if there are any compilation errors.
*/
public auto regex(S, S2=string)(S pattern, S2 flags="")
    if(isSomeString!S && isSomeString!S2)
{
    if(!__ctfe)
    {
        auto parser = Parser!(Unqual!(typeof(pattern)))(pattern, flags);
        Regex!(BasicElementOf!S) r = parser.program;
        return r;
    }
    else
    {
        auto parser = Parser!(Unqual!(typeof(pattern)), true)(pattern, flags);
        Regex!(BasicElementOf!S) r = parser.program;
        return r;
    }
}

template ctRegexImpl(alias pattern, string flags=[])
{
    enum r = regex(pattern, flags);
    alias BasicElementOf!(typeof(pattern)) Char;
    enum source = ctGenRegEx(r.ir);
    alias BacktrackingMatcher!(true) Matcher;
    bool func(ref Matcher!Char matcher)
    {
        version(fred_ct) debug pragma(msg, source);
        mixin(source);
    }
    enum nr = StaticRegex!Char(r, &func);
}

/**
    Experimental feature. Compile regular expression using CTFE
    and generate optimized native machine code for matching it.
    Returns: StaticRegex object for faster matching.
*/
public template ctRegex(alias pattern, alias flags=[])
{
    enum ctRegex = ctRegexImpl!(pattern, flags).nr;
}

/**
    Start matching $(D input) to regex pattern $(D re),
    using Thompson NFA matching scheme.
    Returns a $(D RegexMatch) object holding engine state after first match.

    This is the $(U recommended) method for matching regular expression.
*/

public auto match(R, RegEx)(R input, RegEx re)
    if(is(RegEx == Regex!(BasicElementOf!R)))
{
    return RegexMatch!(Unqual!(typeof(input)),ThompsonMatcher)(re, input);
}

///ditto
public auto match(R, String)(R input, String re)
    if(isSomeString!String)
{
    return RegexMatch!(Unqual!(typeof(input)),ThompsonMatcher)(regex(re), input);
}

///Match regular expression using statically generated $(D StaticRegex).
public auto match(R, RegEx)(R input, RegEx re)
    if(is(RegEx == StaticRegex!(BasicElementOf!R)))
{
    return RegexMatch!(Unqual!(typeof(input)),BacktrackingMatcher!true)(re, input);
}

/**
    Start matching $(D input) to regex pattern $(D re),
    using traditional $(LUCKY backtracking) matching scheme.
    Returns a $(D RegexMatch) object holding engine
    state after first match.
*/
public auto bmatch(R, RegEx)(R input, RegEx re)
    if(is(RegEx == Regex!(BasicElementOf!R)))
{
    return RegexMatch!(Unqual!(typeof(input)), BacktrackingMatcher!false)(re, input);
}

///ditto
public auto bmatch(R, String)(R input, String re)
    if(isSomeString!String)
{
    return RegexMatch!(Unqual!(typeof(input)), BacktrackingMatcher!false)(regex(re), input);
}

///Match regular expression using statically generated $(D StaticRegex).
public auto bmatch(R, RegEx)(R input, RegEx re)
    if(is(RegEx == StaticRegex!(BasicElementOf!R)))
{
    return RegexMatch!(Unqual!(typeof(input)),BacktrackingMatcher!true)(re, input);
}

/**
    Construct a new string from $(D input) by replacing each match with
    string generated form match accroding to $(D format) specifier.
    To replace all occurances use regex with "g" flag, otherwise first occurence get replaced.

    Params:
    input = string to search
    re = compiled regular expression to use
    format = format string to generate replacements from

    Example:
    ---
    //Comify a number
    auto com = regex(r"(?<=\d)(?=(\d\d\d)+\b)","g");
    assert(replace("12000 + 42100 = 56000", com, ",") == "12,000 + 42,100 = 56,100");
    ---

    The replacement format can reference the matches using the $&amp;, $$,
    $', $`, $0 .. $99 notation:
    ---
    assert(replace("noon", regex("^n"), "[$&]") == "[n]oon");
    ---
*/
public @trusted R replace(R, alias scheme=match, RegEx)(R input, RegEx re, R format)
  if(isSomeString!R && is(RegEx == Regex!(BasicElementOf!R))
     || is(RegEx == StaticRegex!(BasicElementOf!R)))
{
    auto app = appender!(R)();
    auto matches = scheme(input, re);
    size_t offset = 0;
    foreach(ref m; matches)
    {
        app.put(m.pre[offset .. $]);
        replaceFmt(format, m.captures, app);
        offset = m.pre.length + m.hit.length;
    }
    app.put(input[offset .. $]);
    return app.data;
}

/*
    Search string for matches with regular expression pattern with
    attributes.  Pass captures for each match to function $(D fun).  Replace each match
    with the return value from dg.

    Params:
    s = string to search
    re = compiled regular expression
    dg = delegate to use

    Returns: the resulting string.
    Example:
    Capitalize the letters 'a' and 'r':
    ---
    string baz(Captures!(string) m)
    {
        return std.string.toUpper(m.hit);
    }
    auto s = replace!(baz)("Strap a rocket engine on a chicken.",
            regex("[ar]", "g"));
    assert(s == "StRAp A Rocket engine on A chicken.");
    ---
*/
public @trusted R replace(alias fun, R, RegEx, alias scheme=match)(R input, RegEx re)
    if(isSomeString!R && is(RegEx == Regex!(BasicElementOf!R)))
{
    auto app = appender!(R)();
    auto matches = scheme(input, re);
    size_t offset = 0;
    foreach(m; matches)
    {
        app.put(m.pre[offset .. $]);
        app.put(fun(m));
        offset = m.pre.length + m.hit.length;
    }
    app.put(input[offset .. $]);
    return app.data;
}

//produce replacement string from format using captures for substitue
public @trusted void replaceFmt(R, Capt, OutR)
    (R format, Capt captures, OutR sink, bool ignoreBadSubs=false)
    if(isOutputRange!(OutR, ElementEncodingType!R[]) &&
        isOutputRange!(OutR, ElementEncodingType!(Capt.String)[]))
{
    enum State { Normal, Escape, Dollar };
    auto state = State.Normal;
    size_t offset;
L_Replace_Loop:
    while(!format.empty)
        final switch(state)
        {
        case State.Normal:
            for(offset = 0; offset < format.length; offset++)//no decoding
            {
                switch(format[offset])
                {
                case '\\':
                    state = State.Escape;
                    sink.put(format[0 .. offset]);
                    format = format[offset+1 .. $];//safe since special chars are ascii only
                    continue L_Replace_Loop;
                case '$':
                    state = State.Dollar;
                    sink.put(format[0 .. offset]);
                    format = format[offset+1 .. $];//ditto
                    continue L_Replace_Loop;
                default:
                }
            }
            sink.put(format[0 .. offset]);
            format = format[offset .. $];
            break;
        case State.Escape:
            offset = std.utf.stride(format, 0);
            sink.put(format[0 .. offset]);
            format = format[offset .. $];
            state = State.Normal;
            break;
        case State.Dollar:
            if(ascii.isDigit(format[0]))
            {
                uint digit = parse!uint(format);
                enforce(ignoreBadSubs || digit < captures.length, text("invalid submatch number ", digit));
                if(digit < captures.length)
                    sink.put(captures[digit]);
            }
            else if(format[0] == '{')
            {
                auto x = find!"!std.ascii.isAlpha(a)"(format[1..$]);
                enforce(!x.empty && x[0] == '}', "no matching '}' in replacement format");
                auto name = format[1 .. $ - x.length];
                format = x[1..$];
                enforce(!name.empty, "invalid name in ${...} replacement format");
                sink.put(captures[name]);
            }
            else if(format[0] == '&')
            {
                sink.put(captures[0]);
                format = format[1 .. $];
            }
            else if(format[0] == '`')
            {
                sink.put(captures.pre);
                format = format[1 .. $];
            }
            else if(format[0] == '\'')
            {
                sink.put(captures.post);
                format = format[1 .. $];
            }
            else if(format[0] == '$')
            {
                sink.put(format[0 .. 1]);
                format = format[1 .. $];
            }
            state = State.Normal;
            break;
        }
    enforce(state == State.Normal, "invalid format string in regex replace");
}

/**
Range that splits a string using a regular expression as a
separator.

Example:
----
auto s1 = ", abc, de,  fg, hi, ";
assert(equal(splitter(s1, regex(", *")),
    ["", "abc", "de", "fg", "hi", ""]));
----
 */
public struct Splitter(Range, alias Engine=ThompsonMatcher)
    if(isSomeString!Range)
{
private:
    Range _input;
    size_t _offset;
    alias RegexMatch!(Range, Engine) Rx;
    Rx _match;

    @trusted this(Range input, Regex!(BasicElementOf!Range) separator)
    {//@@@BUG@@@ generated opAssign of RegexMatch is not @trusted
        _input = input;
        separator.flags |= RegexOption.global;
        if (_input.empty)
        {
            //there is nothing to match at all, make _offset > 0
            _offset = 1;
        }
        else
        {
            _match = Rx(separator, _input);
        }
    }

public:
    auto ref opSlice()
    {
        return this.save();
    }

    ///Forward range primitives.
    @property Range front()
    {
        assert(!empty && _offset <= _match.pre.length
                && _match.pre.length <= _input.length);
        return _input[_offset .. min($, _match.pre.length)];
    }

    ///ditto
    @property bool empty()
    {
        return _offset > _input.length;
    }

    ///ditto
    void popFront()
    {
        assert(!empty);
        if (_match.empty)
        {
            //No more separators, work is done here
            _offset = _input.length + 1;
        }
        else
        {
            //skip past the separator
            _offset = _match.pre.length + _match.hit.length;
            _match.popFront;
        }
    }

    ///ditto
    @property auto save()
    {
        return this;
    }
}

///A helper function, creates a $(D Spliiter) on range $(D r) separated by regex $(D pat).
public Splitter!(Range) splitter(Range, RegEx)(Range r, RegEx pat)
    if( is(BasicElementOf!Range : dchar) && is(RegEx == Regex!(BasicElementOf!Range)))
{
    return Splitter!(Range)(r, pat);
}

///An eager version of $(D splitter) that creates an array with splitted slices of $(D input).
public @trusted String[] split(String, RegEx)(String input, RegEx rx)
    if(isSomeString!String && is(RegEx == Regex!(BasicElementOf!String)))
{
    auto a = appender!(String[])();
    foreach(e; splitter(input, rx))
        a.put(e);
    return a.data;
}

///Exception object thrown in case of errors during regex compilation.
public class RegexException : Exception
{
    ///
    @trusted this(string msg, string file = __FILE__, size_t line = __LINE__)
    {//@@@BUG@@@ Exception constructor is not @safe
        super(msg, file, line);
    }
}
