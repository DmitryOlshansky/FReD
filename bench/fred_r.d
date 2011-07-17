module main;

import std.file;
import std.stdio;
import std.datetime;

import fred;
version(backtracking)
	alias match matchFn;
else version(thompson)	
	alias tmatch matchFn;
else
	static assert(0, "Choose version: backtracking or thompson");
int main(string[] argv)
{
    if(argv.length < 3){
        writefln("Usage %s <re> file [print]",argv[0]);
        return 1;
    }
    auto engine = regex(argv[1],"g");
    auto data = cast(char[])std.file.read(argv[2]);
    size_t count=0;
    StopWatch sw;
    sw.start();
    if(argv.length == 4 && argv[3] =="print")
        foreach(m; matchFn(data,engine)){
            writeln(m.hit);
            count++;
        }
    else
        foreach(m; matchFn(data,engine)){
            count++;
        }
    sw.stop();
    auto dt = sw.peek().msecs;
    writefln("Total matches %s.\nTime elapsed %s sec",count,dt/1000.0);
    return 0;
}
