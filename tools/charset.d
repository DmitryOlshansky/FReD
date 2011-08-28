//Written in the D programming language
/**
    Charset -  utility for analyzing codepoint sets of a given regex
*/
import fred, fred_uni;

import std.stdio, std.string;

int main(string argv[])
{
    if(argv.length < 2)
    {
        writeln("Dump charsets of regex object\nUsage charset regex1 [flags]");
        return 1;
    }
    auto r = regex(argv[1], argv.length > 2 ? argv[2] : "");
    foreach(cset; r.charsets)
    {
        cset.printUnicodeSet( (const(char)[] s){ write(s); }  );
        writeln();
    }
    return 0;
}
