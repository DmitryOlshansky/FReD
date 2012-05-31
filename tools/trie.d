//Written in the D programming language
/**
    Small tool for analyzing tries with w.r.t. memory footprint for Unicode propertes
*/
import std.internal.uni_tab, std.internal.uni;
import std.stdio, std.algorithm, std.typetuple;

void main()
{
    void test_nbits(uint bits)(File f)
    {
        uint max_char, max_data;
        CodepointTrie!bits t;
        foreach(up; unicodeProperties)
        {
            t = CodepointTrie!bits(up.set);
            uint num = 0;
            foreach(ival; up.set.byInterval)
            {
                num += ival.end - ival.begin + 1;
                for(uint ch=ival.begin;ch<=ival.end;ch++)
                    assert(cast(dchar)t[ch]);
            }
            f.writefln("\"%s\";%s;%s;%s;", up.name, num, t.data.length*4, t.indexes.length*2);
        }
    }
    test_nbits!6(File("6bits.csv", "wb"));
    test_nbits!7(File("7bits.csv", "wb"));
    test_nbits!8(File("8bits.csv", "wb"));
    test_nbits!9(File("9bits.csv", "wb"));
    test_nbits!10(File("10bits.csv", "wb"));
}