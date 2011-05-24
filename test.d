module test;
import std.stdio, std.conv, std.string, std.range, std.exception;
import std.typetuple;
import regex;

int main(string[] argv)
{
    //RecursiveParser!string("a{1}");
    if(argv.length < 2)
    {
        writeln("regex parser test\nUsage %s <pattern file>");
        return 1;
    }
    auto f = File(argv[1]);
    string s;
    for(;;)
    { 
        s = strip(f.readln());
        if(s.empty)
            break;
        write(s.idup," ");
        try
        {
            auto p = RecursiveParser!string(s);
            write(" OK \n");
            p.printPostfix();
        }
        catch(Exception ex)
        {
            write(" FAIL\n",ex.msg);
        }    
        writeln();
    }
    return 0;
}
