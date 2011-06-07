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
    else if(argv.length == 2)
    {
        auto f = File(argv[1]);
        string s;
        for(;;)
        { 
            s = strip(f.readln());
            if(s.empty)
                break;
            write(s," ");
            try
            {
                auto p = RecursiveParser!string(s);
                write(" OK \n");
                p.print();
            }
            catch(Exception ex)
            {
                write(" FAIL\n",ex.msg);
            }    
            writeln();
        }
    }
        
    
    return 0;
}
