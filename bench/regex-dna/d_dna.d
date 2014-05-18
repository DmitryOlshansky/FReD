// Benchmark adapted from some c++ version from
// The Computer Language Benchmarks Game
// http://shootout.alioth.debian.org/


import std.regex;
import std.file, std.stdio, std.array, std.algorithm, std.range, std.typetuple;

version(backtracking)
	alias bmatch matchFn;
else version(thompson)	
	alias match matchFn;
else version(ct_regex)
	alias match matchFn;
else	
	static assert(0, "Use -version=backtracking or -version=thompson or -version=ct_regex");

alias TypeTuple!(
    "agggtaaa|tttaccct",
    "[cgt]gggtaaa|tttaccc[acg]",
    "a[act]ggtaaa|tttacc[agt]t",
    "ag[act]gtaaa|tttac[agt]ct",
    "agg[act]taaa|ttta[agt]cct",
    "aggg[acg]aaa|ttt[cgt]ccct",
    "agggt[cgt]aa|tt[acg]accct",
    "agggta[cgt]a|t[acg]taccct",
    "agggtaa[cgt]|[acg]ttaccct",
) patterns;

alias TypeTuple!(
    "B", "(c|g|t)", "D", "(a|g|t)", "H", "(a|c|t)", "K", "(g|t)",
    "M", "(a|c)", "N", "(a|c|g|t)", "R", "(a|g)", "S", "(c|g)",
    "V", "(a|c|g)", "W", "(a|t)", "Y", "(c|t)"
) patterns2;

int main(string[] args)
{
    if(args.length < 2)
    {
        writeln("Usage regex-dna <FASTA-file>");
        return 1;
    }

    auto stripper = regex(`>.*?\n|\n`, "g");
    auto data = cast(string)std.file.read(args[1]);
    auto stripped = replace!matchFn(data, stripper, "");
    foreach(p; patterns)
    {
        version(ct_regex)
            alias ctRegex!(p, "ig") reg;
        else//auto since parsing takes miniscule amount of time
            auto reg = regex(p, "ig");
        int count = 0;
        foreach(m; matchFn(stripped, reg))
        {
            count++;
        }
        writeln(p, " ", count);
    }

    auto replaced = stripped;
    foreach(i, p; patterns2)
    {
        static if (!(i & 1))
        {
            version(ct_regex)
                alias ctRegex!(p, "g") reg;
            else//auto since parsing takes miniscule amount of time
                auto reg = regex(p, "g");
            replaced = replace!matchFn(replaced, reg, patterns2[i+1]);
        }
    }

    writeln(data.length);
    writeln(stripped.length);
    writeln(replaced.length);
    return 0;
}
