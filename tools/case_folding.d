//Written in the D programming language
/**
    case_folding is quick & dirty source code generator for casefolding datastructures.
    Based on Unicode common case folding datafile
*/
import fred, std.stdio, std.conv, std.algorithm, std.array, std.format;

struct Entry
{
	Charset set;
	int delta;
}
void writeCharset(in Charset set)
{
	writeln("Charset([");
	foreach(i; set.intervals)
		writefln("Interval(0x%05x,0x%05x),", i.begin, i.end);
	writeln("]);");

}
void main()
{
	uint[uint] hash;
	auto r = regex("([^;]*); C;\\s*([^;]*);");
	foreach(line; stdin.byLine)
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
	Entry[int] entries;     //entries by delta
	foreach(k, v; hash)
	{
		auto d = cast(int)k - cast(int)v;
		if(d !in entries)
			entries[d] = Entry.init;
		if(-d !in entries)
			entries[-d] = Entry.init;
		entries[d].set.add(hash[k]);
		entries[-d].set.add(k);
	}
   	entries[1].set.add(entries[-1].set);
	write("immutable evenUpper = ");
	writeCharset(entries[1].set);
	uint[] low;
	uint[] high;
	writeln("immutable commonCaseTable = [");
	foreach(k, v; entries)
	{
		if(k > 1)
		{
			auto app = appender!(char[])();
			bool notEmpty = false;
			formattedWrite(app, "CommonCaseEntry(%d, Charset([", k);
			foreach(i; entries[k].set.intervals)
			{
				if(i.begin == i.end)
				{
					low ~= i.begin;
					high ~= i.begin+k;
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
			foreach(i; entries[k].set.intervals)
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
	writeln("]\n");
	auto index = new uint[low.length];
	assert(low.length == high.length);
	makeIndex(low, index);
	writeln("immutable casePairs = [");
	foreach(i; index)
		writefln("0x%05x, 0x%05x,",low[i], high[i]);
	writeln("]");

}
