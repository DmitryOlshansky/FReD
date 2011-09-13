//Written in the D Programming Language
/**
    stress.d a tool to do automated stress tests of FReD
*/
module stress;

import fred;
import std.stdio, std.range, std.string, std.array, std.exception, std.conv, std.random;

version(backtracking)
	alias bmatch matchFn;
else version(thompson)
	alias match matchFn;
else
	static assert(0, "Use -version=backtracking or -version=thompson");

void testRegex(Char)(in Char[] pat, Regex!Char r, int iterations, int length, uint seed)
{
    auto gen = SampleGenerator!(Char)(r, length, seed);
    foreach(s; take(gen, iterations))
    {
        auto m = matchFn(s, r);
        enforce(!m.empty,
                text("having seed=", seed, " failed to match sample ", s
                     , " for pattern ", pat));
    }
}

int main(string[] args)
{
    static void usage()
    {
        writeln(
            "FReD stress test tool\nUsage: \n\tstress command <args> ",
            "[<exec-test iterations> <exec test sample length>] [seed]\n",
            "Commands:\n",
            "permutation: tests all permutations of terms up to max_pattern_length \n",
            "\targs are: \"term1 term2 ... termN\" max_pattern_length\n",
            "random: tests given number of random combination of terms of specified length",
            "\targs are: \"term1 term2 ... termN\" length iterations ",
        );
    }

    if(args.length < 3)
    {
        usage();
        return 1;
    }
    bool exec_test;
    int exec_iter, exec_len;
    uint exec_seed;
    int numExceptions = 0;
    void test_body(Char)(Char[] data)
    {
        try
        {
            auto r = regex(data);
            if(exec_test)
                testRegex(data, r, exec_iter, exec_len, exec_seed);
            r = regex(data, "i");
            if(exec_test)
                testRegex(data, r, exec_iter, exec_len, exec_seed);
        }
        catch(RegexException ex)
        {
            numExceptions++;
        }
        catch(Throwable t)
        {
            writeln("@@@BUG@@@: ", data, t.msg);
        }
    }
    if(args[1] == "permutation")
    {
        auto alphabet = std.string.split(args[2], " ");
        int maxLen = to!int(args[3]);
        enforce(maxLen > 0);
        int[] index = new int[maxLen];

        exec_test = args.length > 4;
        exec_seed = unpredictableSeed;
        if(exec_test)
        {
            enforce(args.length == 7 || args.length == 6
                    , "Wrong num of args for optional exe-test");
            exec_iter = to!int(args[4]);
            exec_len = to!int(args[5]);
            if(args.length == 7)
                exec_seed = to!uint(args[6]);
        }
        for(int pat=1;pat<=maxLen;pat++)
        {
            auto app = appender!(char[])();
            //generate up to a maxLen
            index[] = 0;
    Gen_Loop:
            for(;;)
            {
                if(index[pat-1] == alphabet.length)
                    break;
                if(index[0] == alphabet.length)
                {
                    index[0] = 0;
                    for(int j=1;j<pat;j++)
                    {
                        index[j]++;
                        if(index[j] < alphabet.length )
                            break;
                        else if(j != pat-1)
                            index[j] = 0;
                        else if(index[j] == alphabet.length)
                            break Gen_Loop;
                    }
                }
                for(int i=0;i<pat;i++)
                {
                    app.put(alphabet[index[i]]);
                }
                test_body(app.data);
                //writeln(app.data);
                app.shrinkTo(0);
                //writeln(index[0..pat]);
                index[0]++;
            }
        }
        writefln("Complete with %d exceptions caught", numExceptions);
    }
    else if(args[1] == "random")
    {
        auto alphabet = std.string.split(args[2], " ");
        int len = to!int(args[3]);
        int iterations = to!int(args[4]);
        auto rnd = Xorshift(0);
        uint seed;
        exec_test = args.length > 6;
        if(exec_test)
        {
            enforce(args.length == 8 || args.length == 7
                    , "Wrong num of args for optional exe-test");
            exec_iter = to!int(args[5]);
            exec_len = to!int(args[6]);
            if(args.length == 8)
                exec_seed = seed = to!uint(args[7]);
            else
                exec_seed = seed = unpredictableSeed();
        }
        else
            seed = args.length < 6 ? unpredictableSeed() : to!uint(args[5]);
        rnd.seed(seed);
        writeln("Random test using seed ", seed);
        enforce(len > 0);
        auto app = appender!(char[])();
        for(int i=0; i<iterations; i++)
        {
            for(int j=0;j<len;j++)
            {
                app.put(alphabet[rnd.front()%alphabet.length]);
                rnd.popFront();
            }
            test_body(app.data);
            app.shrinkTo(0);
            //writeln(app.data);
        }
        writefln("Complete with %d exceptions caught", numExceptions);
    }
    else
    {
        usage();
        return 1;
    }
    return 0;
}
