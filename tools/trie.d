//Written in the D programming language
/**
    Small tool for analyzing tries with w.r.t. memory footprint for Unicode propertes
*/
import fred, fred_uni;
import std.stdio, std.algorithm, std.typetuple;

void main()
{
    void test_nbits(uint bits)(File f)
    {
        uint max_char, max_data;
        Trie!bits t;
        foreach(up; unicodeProperties)
        {
            t = Trie!bits(up.set);
            uint num = 0;
            foreach(ival; up.set.intervals)
            {
                num += ival.end - ival.begin + 1;
                for(uint ch=ival.begin;ch<=ival.end;ch++)
                    assert(cast(dchar)t[ch]);
            }
            //t.desc();
            //writefln("That was %s set with %s codepoints encoded as %d bit-tables", up.name, num, t.data.length);
            f.writefln("\"%s\";%s;%s;%s;", up.name, num, t.data.length*4, t.indexes.length*2);
        }
    }
    test_nbits!6(File("6bits.csv", "wb"));
    test_nbits!7(File("7bits.csv", "wb"));
    test_nbits!8(File("8bits.csv", "wb"));
    test_nbits!9(File("9bits.csv", "wb"));
    test_nbits!10(File("10bits.csv", "wb"));
}