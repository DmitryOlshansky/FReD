module main;

import std.file;
import std.stdio;
import std.algorithm, std.range;
import std.datetime;

int main(string[] argv)
{
    if(argv.length < 3){
        writefln("Usage %s <re> file [print]",argv[0]);
        return 1;
    }
    auto data = cast(char[])std.file.read(argv[2]);
    bool print = argv.length == 4 && argv[3] =="print";
    size_t count=0;
    StopWatch sw;
    sw.start();
        for(;;){
            auto x = findSplitAfter(data, argv[1]);
            if(x[0].empty)
                break;
            if(print)
                writeln(x[0][$-argv[1].length..$]);
            count++;
            data = x[1];
        }
    sw.stop();
    auto dt = sw.peek().msecs;
    writefln("Total matches %s.\nTime elapsed %s sec",count,dt/1000.0);
    return 0;
}
