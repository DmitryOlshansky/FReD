//Written in the D programming language
/**
    gen_uni is a quick & dirty source code generator for unicode datastructures
*/
import fred, std.stdio, std.conv, std.algorithm, std.array, std.format;


struct UniBlock
{
    string name;
    Interval ival;
}
Charset[int] casefold;//entries by delta
UniBlock[] blocks;
string[] gcNames = [
	"Cc", "Cf", "Co", "Cs",       
	"L", "LC", "Ll", "Lm", "Lo", "Lt", "Lu", 
	"Mc", "Me", "Mn",  "Nd", "Nl", "No", 
	 "Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps", 
	 "Sc", "Sk", "Sm", "So", "Z", "Zl", "Zp", "Zs"  
];
string[] directGcNames = [//to be printed as is
	"Cc", "Cf", "Co", "Cs",       
	"L", "Lm", "Lo", "Lt", 
	"Mc", "Me", "Mn",  "Nd", "Nl", "No", 
	 "Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps", 
	 "Sc", "Sk", "Sm", "So", "Z", "Zl", "Zp", "Zs"  
];
uint[] lowIrreg;//that low/high doesn't mean they are all _letters_!
uint[] highIrreg; 
Charset[] gcSets;//General category
static this()
{
    gcSets = new Charset[gcNames.length];
}

void main(string[] argv)
{
    File casef = File("Casefolding.txt");
    File scriptf = File("Scripts.txt");
    File blockf  = File("Blocks.txt");
    File propf= File("UnicodeData.txt");
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
    loadCaseFolding(casef);
    loadBlocks(blockf);
    loadProperties(propf);
    testCasingIrregular();
    writeCaseFolding();
    writeBlocks();
    writeProperties();
    
}

void testCasingIrregular()
{
    auto low = countUntil(gcNames,"Ll");
    auto lowSet = gcSets[low].dup;
    Charset irreg;
    foreach(ch; lowIrreg)
            irreg.add(ch);
    foreach(ch; highIrreg)
            irreg.add(ch);
    /*lowSet.printUnicodeSet((const(char)[] s){stderr.write(s); });
    irreg.printUnicodeSet((const(char)[] s){stderr.write(s); });*/
    lowSet.intersect(irreg);
    uint lowArray[];
    foreach(ival; lowSet.intervals)
    {
        for(uint ch=ival.begin; ch<=ival.end; ch++)
            lowArray ~= ch;
    }
    assert(equal(sort(lowArray),sort(lowIrreg.dup)));

    //auto highSet = gcSets[high].dup;
    //lowSet.printUnicodeSet((const(char)[] s){stderr.write(s); });
    //stderr.writeln();
}

void loadCaseFolding(File f)
{
	uint[uint] hash;
	auto r = regex("([^;]*); C;\\s*([^;]*);");
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
			casefold[d] = Charset.init;
		casefold[d].add(val);
	}
	auto copy =  casefold[1].intervals.dup;
	foreach(ival;  copy)
        for(uint ch = ival.begin; ch<=ival.end; ch++)
        {
            casefold[1].add(1 + ch);
        }
}

void loadBlocks(File f)
{
	auto r = regex(`([0-9A-F]+)..([0-9A-F]+);\s*(.*)\s*$`);
	foreach(line; f.byLine)
	{
		auto m = match(line, r);
		if(!m.empty)
		{
			auto s1 = m.captures[1];
			auto s2 = m.captures[2];
			auto a1 = parse!uint(s1, 16);
			auto a2 = parse!uint(s2, 16);
			blocks ~= UniBlock(m.captures[3].idup, Interval(a1, a2));
		}
	}
}

void loadProperties(File inp)
{
	auto r = regex("([0-9A-F]*);[^;]*;([^;]*);[^;]*;([^;]*);");
	foreach(char[] s; inp.byLine)
	{
		static int cnt;
		auto m = tmatch(s, r);
		if(!m.empty)
		{
			auto i = countUntil(gcNames, m.captures[2]);
            uint val;
			if(i >= 0)
            {
                auto x = m.captures[1];
                val = parse!uint(x,16);
				gcSets[i].add(val);
            }
			i = countUntil(gcNames, m.captures[3]);
			if(i >= 0)
            {
				auto x = m.captures[1];
                val = parse!uint(x,16);
				gcSets[i].add(val);
            }
		}
	}
}


void writeCharset(in Charset set)
{
	writeln("Charset([");
	foreach(i; set.intervals)
		writef("Interval(0x%05x,0x%05x),\n", i.begin, i.end);
	writeln("]);");
}

void writeCaseFolding()
{
    write("//any codepoint in these intervals is trivally uppercased/lowercased"
" (lowest bit set -> lower)\nimmutable evenUpper = ");
	writeCharset(casefold[1]);
	writeln("struct CommonCaseEntry
{
    short delta;
    Charset set;
}
//these are a bit harder to lowercase/uppercase lower: +- delta
immutable commonCaseTable = [");
    auto keys =casefold.keys();
    sort!"abs(a) < abs(b)"(keys);
	foreach(k; keys)
	{
		if(k > 1)
		{
			auto app = appender!(char[])();
			bool notEmpty = false;
			formattedWrite(app, "CommonCaseEntry(%d, Charset([", k);
			foreach(i; casefold[k].intervals)
			{
				if(i.begin == i.end)
				{
					lowIrreg ~= i.begin;
					highIrreg ~= i.begin+k;
				}
				else
				{
					formattedWrite(app, "Interval(0x%05x, 0x%05x),",i.begin, i.end);
					notEmpty = true;
				}

			}
			formattedWrite(app, "])),");
			if(notEmpty)
				writeln(app.data);
			app.clear();
			notEmpty  = false;
			formattedWrite(app,"CommonCaseEntry(%d, Charset([", -k);
			foreach(i; casefold[k].intervals)
			{
				if(i.begin != i.end)
				{
					formattedWrite(app,"Interval(0x%05x, 0x%05x),",i.begin+k, i.end+k);
					notEmpty = true;
				}

			}
			formattedWrite(app, "])),");
			if(notEmpty)
				writeln(app.data);
		}
	}
	writeln("];\n");
	auto index = new uint[lowIrreg.length];
	assert(lowIrreg.length == highIrreg.length);
	makeIndex(lowIrreg, index);
	writeln("//horrible irregularities are stockpiled here as equivalent pairs"
            " (note  it's not a 1:1 mapping, more like n:m)\n"
            "immutable(uint)[] casePairs = [");
	foreach(i; index)
		writefln("0x%05x, 0x%05x,",lowIrreg[i], highIrreg[i]);
	writeln("];");
}

void writeBlocks()
{
    write("struct UnicodeBlock\n"
"{\n"
"    string name;\n"
"    Interval extent;\n"
"}\nimmutable unicodeBlocks = [");
    static bool less(UniBlock a,UniBlock b)
    {  
        return propertyNameLess(a.name, b.name);
    }
    sort!(less)(blocks);
    foreach(b; blocks)
    {
        writefln(`UnicodeBlock("%s", Interval(0x%05X, 0x%05X)),`,
                 b.name, b.ival.begin, b.ival.end);
    }
    writeln("];");
}

void writeProperties()
{
    
    foreach(i, cs; gcSets)
	{
        if(canFind(directGcNames,gcNames[i]))
        {
		    writef("immutable unicode%s = ", gcNames[i]);
		    writeCharset(cs);
        }
	}
    
}