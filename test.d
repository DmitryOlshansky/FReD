module test;
import std.stdio, std.conv, std.string, std.range, std.exception;
import std.typetuple;
import regex;

int main(string[] argv)
{    
    if(argv.length < 2)
    {
        writefln("regex test\nUsage %s <pattern file>\n"
                 "or: %s <pattern> <test-string>\n",argv[0],argv[0]);
        return 1;
    }      
    return 0;
}
