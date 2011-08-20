//Written in the D programming language
/**
    gen_uni is a quick & dirty source code generator for unicode datastructures
*/
import fred;
import std.ascii, std.stdio, std.conv, std.algorithm, 
       std.array, std.format, std.string, std.bitmanip,
       std.range;
import core.bitop;
CodepointSet[int] casefold;//entries by delta
CodepointSet[string] props;
string[string] aliases;
CodepointSet[string] normalization;
string[] blacklist = [  ];
enum mixedCCEntry = q{
struct CommonCaseEntry
{
    dchar start, end;
    uint op; 
    @property uint delta() const { return op & 0xFF_FFFF; }
    @property uint xor()const {   return op & doXor; }
    @property uint neg()const {   return op & doMinus; }
    enum doXor = 1<<31, doMinus = 1<<30;
}
};
mixin(mixedCCEntry);
void scanUniData(alias Fn)(string name, Regex!char r)
{
    foreach(line; File(name).byLine)
	{
        auto m = match(line, r);
        if(!m.empty)
            Fn(m);
    }
}

void main(string[] argv)
{
    writeln("//Written in the D programming language
/**
 * Fast Regular expressions for D
 *
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * Authors: Dmitry Olshansky
 *
 */
//Automatically generated from Unicode Character Database files
import fred;");
    loadCaseFolding("Casefolding.txt");
    loadBlocks("Blocks.txt");
    loadProperties("PropList.txt");
    loadProperties("DerivedGeneralCategory.txt");
    loadProperties("DerivedCoreProperties.txt");
    loadProperties("Scripts.txt");
    loadNormalization("DerivedNormalizationProps.txt");
    writeCaseFolding();
    writeProperties();
    writeNormalization();
}

void loadCaseFolding(string name)
{
	uint[uint] hash;
	auto r = regex("([^;]*); [CS];\\s*([^;]*);");
    auto f = File(name);
	foreach(line; f.byLine)
	{
		auto m = match(line, r);
		if(!m.empty)
		{
			auto s1 = m.captures[1];
			auto s2 = m.captures[2];
			auto a1 = parse!uint(s1, 16);
			auto a2 = parse!uint(s2, 16);
			hash[a1] = a2;
		}
	}
	foreach(k, v; hash)
	{
		auto d = cast(int)k - cast(int)v;
		uint val;
		if(d > 0 )
            val = v;
        else
        {
             val = k;
             d = -d;
        }
        //store as lesser codepoint interval + positive delta
		if(d !in casefold)
			casefold[d] = CodepointSet.init;
		casefold[d].add(val);
	}
	auto copy =  CodepointSet(casefold[1].ivals.dup);
    //merge for first bit flipped intervals
	foreach(ch; copy[])
        {
            casefold[1].add(1 + ch);
        }
}

void loadBlocks(string f)
{
	auto r = regex(`^([0-9A-F]+)\.\.([0-9A-F]+);\s*(.*)\s*$`);
	scanUniData!((m){
			auto s1 = m.captures[1];
			auto s2 = m.captures[2];
			auto a1 = parse!uint(s1, 16);
			auto a2 = parse!uint(s2, 16);
			props["In"~to!string(m.captures[3])] = CodepointSet([a1, a2+1]);
	})(f, r);
}

void loadProperties(string inp)
{
    auto r = regex(`^(?:(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*([a-zA-Z_0-9]*)\s*#|# [a-zA-Z_0-9]+=([a-zA-Z_0-9]+))`);
    string aliasStr;
	scanUniData!((m){
        auto name = to!string(m.captures[4]);
        if(!m.captures[5].empty)
            aliasStr = to!string(m.captures[5]);
		else if(!m.captures[1].empty)
        {
            auto sa = m.captures[1];
            auto sb = m.captures[2];
            uint a = parse!uint(sa, 16);
            uint b = parse!uint(sb, 16);
            if(name !in props)
                props[name] = CodepointSet.init;
            props[name].add(Interval(a,b));
            if(!aliasStr.empty)
            {
                aliases[name] = aliasStr;
                aliasStr = "";
            }
        }
        else if(!m.captures[3].empty)
        {
            auto sx = m.captures[3];
            uint x = parse!uint(sx, 16);
            if(name !in props)
                props[name] = CodepointSet.init;
            props[name].add(x);
            if(!aliasStr.empty)
            {
                aliases[name] = aliasStr;
                aliasStr = "";
            }
        }
	})(inp, r);
}

void loadNormalization(string inp)
{
    auto r = regex(`^(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*(NFC_QC)\s*;\s*([NM])|#\s*[a-zA-Z_0-9]+=([a-zA-Z_0-9]+)`);
    string aliasStr;
	scanUniData!((m){
        auto name = to!string(m.captures[4]) ~ to!string(m.captures[5]);
        /*if(!m.captures[6].empty)
            aliasStr = to!string(m.captures[6]);
		else*/ if(!m.captures[1].empty)
        {
            auto sa = m.captures[1];
            auto sb = m.captures[2];
            uint a = parse!uint(sa, 16);
            uint b = parse!uint(sb, 16);
            if(name !in normalization)
                normalization[name] = CodepointSet.init;
            normalization[name].add(Interval(a,b));
        }
        else if(!m.captures[3].empty)
        {
            auto sx = m.captures[3];
            uint x = parse!uint(sx, 16);
            if(name !in normalization)
                normalization[name] = CodepointSet.init;
            normalization[name].add(x);
        }
        //stderr.writeln(m.hit);
    })(inp, r);
}

string charsetString(in CodepointSet set, string sep=";\n")
{
    auto app = appender!(char[])();
	formattedWrite(app,"CodepointSet([\n");
	for(size_t i=0; i<set.ivals.length; i+=2)
		formattedWrite(app, "    0x%05x, 0x%05x,\n", set.ivals[i], set.ivals[i+1]);
	formattedWrite(app, "])%s\n",sep);
    return cast(string)app.data;
}

void writeCaseFolding()
{
	writeln(mixedCCEntry, "
//sorted by .start, however they do intersect
immutable commonCaseTable = [");
    CommonCaseEntry[] table;
    
    foreach(k, v; casefold)
    {
        CommonCaseEntry e;
        for(size_t i=0; i< v.ivals.length; i+= 2)
        {
            auto xored = map!((x){ return x ^ k; })(iota(v.ivals[i],v.ivals[i+1]));
            auto plused = map!((x){ return x + k; })(iota(v.ivals[i],v.ivals[i+1]));
            e.start = v.ivals[i];
            e.end = v.ivals[i+1];
            e.op = k;
            if(equal(xored, plused) || k == 1)
            {
                e.op |= CommonCaseEntry.doXor;
                table ~= e;
                if(k != 1)
                    table ~= CommonCaseEntry(e.start+k, e.end+k, e.op);
            }
            else
            {
                table ~= e;
                table ~= CommonCaseEntry(e.start+k, e.end+k, CommonCaseEntry.doMinus | e.op);
            }
        }
    }
    alias uint[4] ebh;
    ebh[] mapper = new ebh[0x11_0000];
    foreach(v; table)
    {
        int i;
        foreach(ch; iota(cast(uint)v.start, cast(uint)v.end))
        {
            for(i=0; i<4;i++)
                if(!mapper[ch][i])
                {
                    mapper[ch][i] = v.op;
                    break;
                }
            assert(i != 4);
        }
    }
    table.length = 0;
    uint cur = 0;
    for(uint i=1; i<mapper.length;i++)
    {
        if(mapper[cur] != mapper[i])
        {
            foreach(v; mapper[cur][])
                if(v)
                    table ~= CommonCaseEntry(cur, i,  v);
            cur = i;
        }
    }
    for(size_t i=0; i<table.length; i++)
        for(size_t j=i+1;j<table.length; j++)
        {
            assert(table[i].start <= table[j].start);
            assert(table[i].end <= table[j].end);
        }
	foreach(e; table)
    {
        writefln("CommonCaseEntry(0x%05x, 0x%05x, %s),", e.start, e.end,  e.op);
    }
	writeln("];\n");
}

string identName(string s)
{
    auto app = appender!(char[])();
    foreach(c; s)
        if(c == '-' || c == ' ')
            app.put('_');
        else
            app.put(c);
    return cast(string)app.data;
}

string uniformName(string s)
{
    auto app = appender!(char[])();
    foreach(c; s)
        if(c != '-' && c != ' ' && c != '_')
            app.put(toLower(c));
    return cast(string)app.data;
}


void writeProperties()
{
    File hashf = File("HashKeys.txt", "wb");
    writeln("struct UnicodeProperty
{
    string name;
    immutable CodepointSet set;
}");
    foreach(k, v; props)
    {
        if(countUntil(blacklist, k) < 0)
        {
            writef("immutable(CodepointSet) unicode%s = ", identName(k));
            write(charsetString(v));
        }
    }
    write("immutable(UnicodeProperty)[] unicodeProperties = [\n");
    string[] lines;
    string[] hashLines;
    auto app = appender!(char[])();
    auto keys = props.keys;
    
    foreach(k; keys)
    {
        if(countUntil(blacklist, k) < 0)
        {
            formattedWrite(app, "UnicodeProperty(\"%s\", unicode%s),\n", k, identName(k));
            lines ~= app.data.idup;
            hashLines ~= uniformName(k);
            app.shrinkTo(0);
            if(k in aliases)
            {
                stderr.writeln(aliases[k]);
                formattedWrite(app, "UnicodeProperty(\"%s\", unicode%s),\n",aliases[k], identName(k));
                lines ~= app.data.idup;
                hashLines ~= uniformName(aliases[k]);
                app.shrinkTo(0);
            }
            
        }
    }
    assert(hashLines.length ==lines.length);
    int[] index = new int[lines.length];
    makeIndex!propertyNameLess(lines, index);
    foreach(i, v; lines)
    {
        //auto j = countUntil(globalIndex, i);
        //write(lines[j]);
        write(lines[index[i]]);
        hashf.writeln(hashLines[index[i]]);
    }
        
    writeln("];");
}


void writeNormalization()
{
    foreach(key, value; normalization)
    {
        writef("immutable %s = %s",key, charsetString(value));
    }
}