//Written in the D Programming Language
/**
    stress.d a tool to do automated stress tests of FReD's parser
*/
module stress;

import fred;
import std.stdio, std.string, std.array, std.exception, std.conv, std.random;


int main(string[] args)
{
    if(args.length < 3)
    {
        writeln("FReD stress test tool\nUsage: \n\tstress command <args>\n",
                "Commands:\n",
                "permutation: tests all permutations of terms up to max_pattern_length\n",
                "\targs are: \"term1 term2 ... termN\" max_pattern_length\n",
                "random: tests given number of random combination of terms of specified length",
                "\targs are: \"term1 term2 ... termN\" length iterations");
        return 1;
    }
    if(args[1] == "permutation")
    {
        auto alphabet = std.string.split(args[2], " ");
        int maxLen = to!int(args[3]);
        enforce(maxLen > 0);
        int[] index = new int[maxLen];
        auto numExceptions = 0;
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
                try{
                    auto r = regex(app.data);
                    r = regex(app.data, "i");
                }
                catch(RegexException ex)
                {
                    numExceptions++;
                    writeln(app.data);
                }
                catch(Throwable t)
                {
                    writeln("@@@BUG@@@: ", app.data);
                    throw t;
                }
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
        auto seed = unpredictableSeed();
        rnd.seed(seed);
        writeln("Random test using seed ", seed);
        enforce(len > 0);
        auto numExceptions = 0;
        
        auto app = appender!(char[])();
        for(int i=0; i<iterations; i++)
        {
            for(int j=0;j<len;j++)
            {
                app.put(alphabet[rnd.front()%alphabet.length]);
                rnd.popFront();
            }
            try
            {
                auto r = regex(app.data);
                r = regex(app.data, "i");
            }
            catch(RegexException)
            {
                writeln(app.data);
            }
            catch(Throwable t)
            {
                writeln("@@@BUG@@@: ", app.data);
                throw t;
            }
            app.shrinkTo(0);
            //writeln(app.data);
        }
        writefln("Complete with %d exceptions caught", numExceptions);
    }
    return 0;
}