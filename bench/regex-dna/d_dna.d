// Benchmark adapted from some c++ version from
// The Computer Language Benchmarks Game
// http://shootout.alioth.debian.org/


import fred;
import std.file, std.stdio, std.array, std.algorithm, std.range;

int main(string[] args)
{
    if(args.length < 2)
    {
        writeln("Usage regex-dna <FASTA-file>");
        return 1;
    }

    string[] patterns =  [
        "agggtaaa|tttaccct",
        "cgt]gggtaaa|tttaccc[acg]",
        "a[act]ggtaaa|tttacc[agt]t",
        "ag[act]gtaaa|tttac[agt]ct",
        "agg[act]taaa|ttta[agt]cct",
        "aggg[acg]aaa|ttt[cgt]ccct",
        "agggt[cgt]aa|tt[acg]accct",
        "agggta[cgt]a|t[acg]taccct",
        "agggtaa[cgt]|[acg]ttaccct",
    ];
    string[] patterns2 = [
        "B", "(c|g|t)", "D", "(a|g|t)", "H", "(a|c|t)", "K", "(g|t)",
        "M", "(a|c)", "N", "(a|c|g|t)", "R", "(a|g)", "S", "(c|g)",
        "V", "(a|c|g)", "W", "(a|t)", "Y", "(c|t)"
    ];
    auto regs = array(map!((s){ return regex(s,"ig"); })(patterns));
    auto regs2 = array(map!((s){ return regex(s,"ig"); })(stride(patterns2, 2)));
    auto stripper = regex(`>.*?\n|\n`, "g");
    auto data = cast(string)std.file.read(args[1]);
    auto stripped = replace(data, stripper, "");
    foreach(i, vr; regs)
    {
        int count = 0;
        foreach(m; match(stripped, vr))
        {
            count++;
        }
        writeln(patterns[i], " ", count);
    }
    auto replaced = stripped;
    foreach(i, vr; regs2)
    {
        replaced = replace(replaced, vr, patterns2[2*i+1]);
    }
    writeln(data.length);
    writeln(stripped.length);
    writeln(replaced.length);
    return 0;
}
