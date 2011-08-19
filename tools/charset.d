//Written in the D programming language
/**
    CodepointSet  utility for analyzing unicode charsets of a given regex
*/
import fred, fred_uni;

import std.stdio, std.string;

void main(string argv[])
{
    if(argv.length < 1)
    {
        writeln("Dump charsets of regex object\nUsage CodepointSet regex1 ... regexN");
    }
    foreach(s; argv[1..$])
    {
        auto r = regex(s);
        foreach(cset; r.charsets)
        {
            cset.printUnicodeSet( (const(char)[] s){ write(s); }  );
            writeln();
        }
    }
}
