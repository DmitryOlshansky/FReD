import std.regex, std.random, std.stdio, std.conv, std.range;

int main(string[] argv)
{
    if(argv.length != 3)
    {
        writeln("Usage: gen_sample <pattern> <iterations>");
	return 1;
    }
    auto t = SampleGenerator!char(regex(argv[1]), 10, unpredictableSeed);
    int n = to!int(argv[2]);
    foreach(sample; take(t, n))
    {
          writeln(sample);
    }
    return 0;
}