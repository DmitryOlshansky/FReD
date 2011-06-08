module test;
import std.stdio, std.conv, std.string, std.range, std.exception;
import std.typetuple;
import regex;

int main(string[] argv)
{
    if(argv.length < 2)
    {
        writefln("regex test\nUsage %s <compile | exec> [file1] [file2] \n"
                 "Patterns to test are read by line till empty one\n",argv[0],argv[0]);
        return 0;
    }      
    switch(argv[1])
    {
    case "compile":
        string s;
        auto f = argv.length == 3 ? File(argv[2]) : stdin;
        for(;;)
        {
            s = strip(f.readln());
            if(s.empty)
                break;
            write(s);
            try
            {
                auto p = RecursiveParser!(string,true)(s);
                write(" OK \n");
                auto re = p.program;
                re.processHotspots();
                re.print();
            }
            catch(Exception ex)
            {
                write(" FAIL\n",ex.msg);
            }
            writeln();
        }
    break;
    case "exec":
        
    break;
    default:
        writeln("Unknown command ", argv[1]);
        return 1;
    }
    return 0;
}
