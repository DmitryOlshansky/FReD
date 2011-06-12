module test;
import std.stdio, std.conv, std.string, std.range, std.exception;
import std.typetuple, std.ctype;
import fred;


unittest
{//sanity checks
    regex("abc|edf|ighrg");
    auto r = regex("abc");
    assert(match("abcdef",r).hit == "abc");
    assert(match("wida",regex("(gylba)")).empty);
}

/* The test vectors in this file are altered from Henry Spencer's regexp
   test code. His copyright notice is:

        Copyright (c) 1986 by University of Toronto.
        Written by Henry Spencer.  Not derived from licensed software.

        Permission is granted to anyone to use this software for any
        purpose on any computer system, and to redistribute it freely,
        subject to the following restrictions:

        1. The author is not responsible for the consequences of use of
                this software, no matter how awful, even if they arise
                from defects in it.

        2. The origin of this software must not be misrepresented, either
                by explicit claim or by omission.

        3. Altered versions must be plainly marked as such, and must not
                be misrepresented as being the original software.


 */

unittest
{
    struct TestVectors
    {
        string pattern;
        string input;
        string result;
        string format;
        string replace;
    };

    static TestVectors tv[] = [
        {  "(a)\\1",    "abaab","y",    "&",    "aa" },
        {  "abc",       "abc",  "y",    "&",    "abc" },
        {  "abc",       "xbc",  "n",    "-",    "-" },
        {  "abc",       "axc",  "n",    "-",    "-" },
        {  "abc",       "abx",  "n",    "-",    "-" },
        {  "abc",       "xabcy","y",    "&",    "abc" },
        {  "abc",       "ababc","y",    "&",    "abc" },
        {  "ab*c",      "abc",  "y",    "&",    "abc" },
        {  "ab*bc",     "abc",  "y",    "&",    "abc" },
        {  "ab*bc",     "abbc", "y",    "&",    "abbc" },
        {  "ab*bc",     "abbbbc","y",   "&",    "abbbbc" },
        {  "ab+bc",     "abbc", "y",    "&",    "abbc" },
        {  "ab+bc",     "abc",  "n",    "-",    "-" },
        {  "ab+bc",     "abq",  "n",    "-",    "-" },
        {  "ab+bc",     "abbbbc","y",   "&",    "abbbbc" },
        {  "ab?bc",     "abbc", "y",    "&",    "abbc" },
        {  "ab?bc",     "abc",  "y",    "&",    "abc" },
        {  "ab?bc",     "abbbbc","n",   "-",    "-" },
        {  "ab?c",      "abc",  "y",    "&",    "abc" },
        {  "^abc$",     "abc",  "y",    "&",    "abc" },
        {  "^abc$",     "abcc", "n",    "-",    "-" },
        {  "^abc",      "abcc", "y",    "&",    "abc" },
        {  "^abc$",     "aabc", "n",    "-",    "-" },
        {  "abc$",      "aabc", "y",    "&",    "abc" },
        {  "^",         "abc",  "y",    "&",    "" },
        {  "$",         "abc",  "y",    "&",    "" },
        {  "a.c",       "abc",  "y",    "&",    "abc" },
        {  "a.c",       "axc",  "y",    "&",    "axc" },
        {  "a.*c",      "axyzc","y",    "&",    "axyzc" },
        {  "a.*c",      "axyzd","n",    "-",    "-" },
      //no codepoint sets yet
     /* {  "a[bc]d",    "abc",  "n",    "-",    "-" },
        {  "a[bc]d",    "abd",  "y",    "&",    "abd" },
        {  "a[b-d]e",   "abd",  "n",    "-",    "-" },
        {  "a[b-d]e",   "ace",  "y",    "&",    "ace" },
        {  "a[b-d]",    "aac",  "y",    "&",    "ac" },
        {  "a[-b]",     "a-",   "y",    "&",    "a-" },
        {  "a[b-]",     "a-",   "y",    "&",    "a-" },
        {  "a[b-a]",    "-",    "c",    "-",    "-" },
        {  "a[]b",      "-",    "c",    "-",    "-" },
        {  "a[",        "-",    "c",    "-",    "-" },
        {  "a]",        "a]",   "y",    "&",    "a]" },
        {  "a[]]b",     "a]b",  "y",    "&",    "a]b" },
        {  "a[^bc]d",   "aed",  "y",    "&",    "aed" },
        {  "a[^bc]d",   "abd",  "n",    "-",    "-" },
        {  "a[^-b]c",   "adc",  "y",    "&",    "adc" },
        {  "a[^-b]c",   "a-c",  "n",    "-",    "-" },
        {  "a[^]b]c",   "a]c",  "n",    "-",    "-" },
        {  "a[^]b]c",   "adc",  "y",    "&",    "adc" }, */
        {  "ab|cd",     "abc",  "y",    "&",    "ab" },
        {  "ab|cd",     "abcd", "y",    "&",    "ab" },
        {  "()ef",      "def",  "y",    "&-\\1",        "ef-" },
        {  "()*",       "-",    "y",    "-",    "-" },
        {  "*a",        "-",    "c",    "-",    "-" },
        {  "^*",        "-",    "y",    "-",    "-" },
        {  "$*",        "-",    "y",    "-",    "-" },
        {  "(*)b",      "-",    "c",    "-",    "-" },
        {  "$b",        "b",    "n",    "-",    "-" },
        {  "a\\",       "-",    "c",    "-",    "-" }, 
        {  "a\\(b",     "a(b",  "y",    "&-\\1",        "a(b-" },
        {  "a\\(*b",    "ab",   "y",    "&",    "ab" },
        {  "a\\(*b",    "a((b", "y",    "&",    "a((b" },
        {  "a\\\\b",    "a\\b", "y",    "&",    "a\\b" },
        {  "abc)",      "-",    "c",    "-",    "-" },
        {  "(abc",      "-",    "c",    "-",    "-" },
        {  "((a))",     "abc",  "y",    "&-\\1-\\2",    "a-a-a" },
        {  "(a)b(c)",   "abc",  "y",    "&-\\1-\\2",    "abc-a-c" },
        {  "a+b+c",     "aabbabc","y",  "&",    "abc" },
        {  "a**",       "-",    "c",    "-",    "-" },
        {  "a*?a",      "aa",   "y",    "&",    "a" },
        {  "(a*)*",     "aaa",  "y",    "-",    "-" },
        {  "(a*)+",     "aaa",  "y",    "-",    "-" },
        {  "(a|)*",     "-",    "y",    "-",    "-" },
        {  "(a*|b)*",   "aabb", "y",    "-",    "-" },
        {  "(a|b)*",    "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)*",   "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)+",   "ab",   "y",    "&-\\1",        "ab-b" },
        {  "(a+|b)?",   "ab",   "y",    "&-\\1",        "a-a" },
   //     {  "[^ab]*",    "cde",  "y",    "&",    "cde" },
        {  "(^)*",      "-",    "y",    "-",    "-" },
        {  "(ab|)*",    "-",    "y",    "-",    "-" },
        {  ")(",        "-",    "c",    "-",    "-" },
        {  "",  "abc",  "y",    "&",    "" },
        {  "abc",       "",     "n",    "-",    "-" },
        {  "a*",        "",     "y",    "&",    "" },
    //    {  "([abc])*d", "abbbcd",       "y",    "&-\\1",        "abbbcd-c" },
    //    {  "([abc])*bcd", "abcd",       "y",    "&-\\1",        "abcd-a" },
        {  "a|b|c|d|e", "e",    "y",    "&",    "e" },
        {  "(a|b|c|d|e)f", "ef",        "y",    "&-\\1",        "ef-e" },
        {  "((a*|b))*", "aabb", "y",    "-",    "-" },
        {  "abcd*efg",  "abcdefg",      "y",    "&",    "abcdefg" },
        {  "ab*",       "xabyabbbz",    "y",    "&",    "ab" },
        {  "ab*",       "xayabbbz",     "y",    "&",    "a" },
        {  "(ab|cd)e",  "abcde",        "y",    "&-\\1",        "cde-cd" },
      //  {  "[abhgefdc]ij",      "hij",  "y",    "&",    "hij" },
        {  "^(ab|cd)e", "abcde",        "n",    "x\\1y",        "xy" },
        {  "(abc|)ef",  "abcdef",       "y",    "&-\\1",        "ef-" },
        {  "(a|b)c*d",  "abcd", "y",    "&-\\1",        "bcd-b" },
        {  "(ab|ab*)bc",        "abc",  "y",    "&-\\1",        "abc-a" },
    /*    {  "a([bc]*)c*",        "abc",  "y",    "&-\\1",        "abc-bc" },
        {  "a([bc]*)(c*d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-bc-d" },
        {  "a([bc]+)(c*d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-bc-d" },
        {  "a([bc]*)(c+d)",     "abcd", "y",    "&-\\1-\\2",    "abcd-b-cd" },
        {  "a[bcd]*dcdcde",     "adcdcde",      "y",    "&",    "adcdcde" },
        {  "a[bcd]+dcdcde",     "adcdcde",      "n",    "-",    "-" },
    */    {  "(ab|a)b*c", "abc",  "y",    "&-\\1",        "abc-ab" },
        {  "((a)(b)c)(d)",      "abcd", "y",    "\\1-\\2-\\3-\\4",      "abc-a-b-d" },
    //    {  "[a-zA-Z_][a-zA-Z0-9_]*",    "alpha",        "y",    "&",    "alpha" },
    //    {  "^a(bc+|b[eh])g|.h$",        "abh",  "y",    "&-\\1",        "bh-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "effgz",        "y",    "&-\\1-\\2",    "effgz-effgz-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "ij",   "y",    "&-\\1-\\2",    "ij-ij-j" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "effg", "n",    "-",    "-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "bcdd", "n",    "-",    "-" },
        {  "(bc+d$|ef*g.|h?i(j|k))",    "reffgz",       "y",    "&-\\1-\\2",    "effgz-effgz-" },
        {  "(((((((((a)))))))))",       "a",    "y",    "&",    "a" },
        {  "multiple words of text",    "uh-uh",        "n",    "-",    "-" },
        {  "multiple words",    "multiple words, yeah", "y",    "&",    "multiple words" },
        {  "(.*)c(.*)", "abcde",        "y",    "&-\\1-\\2",    "abcde-ab-de" },
        {  "\\((.*), (.*)\\)",  "(a, b)",       "y",    "(\\2, \\1)",   "(b, a)" },
        {  "abcd",      "abcd", "y",    "&-\\&-\\\\&",  "abcd-&-\\abcd" },
        {  "a(bc)d",    "abcd", "y",    "\\1-\\\\1-\\\\\\1",    "bc-\\1-\\bc" },
     /*   {  "[k]",                       "ab",   "n",    "-",    "-" },
        {  "[ -~]*",                    "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~]*",         "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~]*",              "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~]*",           "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~]*",        "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~ -~]*",     "abc",  "y",    "&",    "abc" },
        {  "[ -~ -~ -~ -~ -~ -~ -~]*",  "abc",  "y",    "&",    "abc" },
     */   {  "a{2}",      "candy",                "n",    "",     "" },
        {  "a{2}",      "caandy",               "y",    "&",    "aa" },
        {  "a{2}",      "caaandy",              "y",    "&",    "aa" },
        {  "a{2,}",     "candy",                "n",    "",     "" },
        {  "a{2,}",     "caandy",               "y",    "&",    "aa" },
        {  "a{2,}",     "caaaaaandy",           "y",    "&",    "aaaaaa" },
        {  "a{1,3}",    "cndy",                 "n",    "",     "" },
        {  "a{1,3}",    "candy",                "y",    "&",    "a" },
        {  "a{1,3}",    "caandy",               "y",    "&",    "aa" },
        {  "a{1,3}",    "caaaaaandy",           "y",    "&",    "aaa" },
        {  "e?le?",     "angel",                "y",    "&",    "el" },
        {  "e?le?",     "angle",                "y",    "&",    "le" },
        {  "\\bn\\w",   "noonday",              "y",    "&",    "no" },
        {  "\\wy\\b",   "possibly yesterday",   "y",    "&",    "ly" },
        {  "\\w\\Bn",   "noonday",              "y",    "&",    "on" },
        {  "y\\B\\w",   "possibly yesterday",   "y",    "&",    "ye" },
        {  "\\cJ",      "abc\ndef",             "y",    "&",    "\n" },
        {  "\\d",       "B2 is",                "y",    "&",    "2" },
        {  "\\D",       "B2 is",                "y",    "&",    "B" },
        {  "\\s\\w*",   "foo bar",              "y",    "&",    " bar" },
        {  "\\S\\w*",   "foo bar",              "y",    "&",    "foo" },
        {  "abc",       "ababc",                "y",    "&",    "abc" },
        {  "apple(,)\\sorange\\1",      "apple, orange, cherry, peach", "y", "&", "apple, orange," },
        {  "(\\w+)\\s(\\w+)",           "John Smith", "y", "\\2, \\1", "Smith, John" },
        {  "\\n\\f\\r\\t\\v",           "abc\n\f\r\t\vdef", "y", "&", "\n\f\r\t\v" },
        {  ".*c",       "abcde",                "y",    "&",    "abc" },
        {  "^\\w+((;|=)\\w+)+$", "some=host=tld", "y", "&-\\1-\\2", "some=host=tld-=tld-=" },
        {  "^\\w+((\\.|-)\\w+)+$", "some.host.tld", "y", "&-\\1-\\2", "some.host.tld-.tld-." },
        {  "q(a|b)*q",  "xxqababqyy",           "y",    "&-\\1",        "qababq-b" },
        {  "^(a)(b){0,1}(c*)",   "abcc", "y", "\\1 \\2 \\3", "a b cc" },
        {  "^(a)((b){0,1})(c*)", "abcc", "y", "\\1 \\2 \\3", "a b b" },
        {  "^(a)(b)?(c*)",       "abcc", "y", "\\1 \\2 \\3", "a b cc" },
        {  "^(a)((b)?)(c*)",     "abcc", "y", "\\1 \\2 \\3", "a b b" },
        {  "^(a)(b){0,1}(c*)",   "acc",  "y", "\\1 \\2 \\3", "a  cc" },
        {  "^(a)((b){0,1})(c*)", "acc",  "y", "\\1 \\2 \\3", "a  " },
        {  "^(a)(b)?(c*)",       "acc",  "y", "\\1 \\2 \\3", "a  cc" },
        {  "^(a)((b)?)(c*)",     "acc",  "y", "\\1 \\2 \\3", "a  " },
        {"(?:ab){3}",       "_abababc",  "y","&-\\1","ababab-" },
        {"(?:a(?:x)?)+",    "aaxaxx",     "y","&-\\1-\\2","aaxax--" },
        {"foo.(?=bar)",     "foobar foodbar", "y","&-\\1", "food-" },
        {"(?:(.)(?!\\1))+",  "12345678990", "y", "&-\\1", "12345678-8" },

        ];

    int i;
    sizediff_t a;
    uint c;
    sizediff_t start;
    sizediff_t end;
    TestVectors tvd;

    foreach (Char; TypeTuple!(char, wchar, dchar))
    {
        alias immutable(Char)[] String;
        String produceExpected(Range)(RegexMatch!(Range) m, String fmt)
        {
            String result;
            while (!fmt.empty)
                switch (fmt.front)
                {
                    case '\\':
                        fmt.popFront();
                        if (!isdigit(fmt.front) )
                        {
                            result ~= fmt.front;
                            fmt.popFront();
                            break;
                        }
                        auto nmatch = parse!uint(fmt);
                        if (nmatch < m.captures.length)
                            result ~= m.captures[nmatch];
                    break;
                    case '&':
                        result ~= m.hit;
                        fmt.popFront();
                    break;
                    default:
                        result ~= fmt.front;
                        fmt.popFront();
                }
            return result;
        }
        Regex!(Char) r;
        start = 0;
        end = tv.length;

        for (a = start; a < end; a++)
        {
//             writef("width: %d tv[%d]: pattern='%s' input='%s' result=%s"
//                     " format='%s' replace='%s'\n",
//                     Char.sizeof, a,
//                     tv[a].pattern,
//                     tv[a].input,
//                     tv[a].result,
//                     tv[a].format,
//                     tv[a].replace);

            tvd = tv[a];

            c = tvd.result[0];

            try
            {
                i = 1;
                r = regex(to!(String)(tvd.pattern));
            }
            catch (RegexException e)
            {
                i = 0;
            }

            assert((c == 'c') ? !i : i);

            if (c != 'c')
            {
                auto m = match(to!(String)(tvd.input), r);
                i = !m.empty;
                assert((c == 'y') ? i : !i, text("Match failed pattern: ", tvd.pattern));
                if (c == 'y')
                {
                    auto result = produceExpected(m, to!(String)(tvd.format));
                    assert(result == to!String(tvd.replace),
                           text("Mismatch pattern: ", tvd.pattern," expected:",
                                tvd.replace, " vs ", result));
                }

            }
        }
        writeln("! bulk test  done !");
    }
}


int main(string[] argv)
{
    if(argv.length < 2)
    {
        writefln("regex test\nUsage %s <compile | exec> \n"
                 "Patterns to test and input are read by line till empty one (always from STDIN!)\n",argv[0],argv[0]);
        return 0;
    }      
    switch(argv[1])
    {
    case "compile":
        string s;
        for(;;)
        {
            s = strip(readln());
            if(s.empty)
                break;
            write(s);
            try
            {
                auto p = RecursiveParser!(string)(s);
                write(" OK \n");
                auto re = p.program;
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
        for(;;)
        {
            try{
                write("Test pattern:");
                stdout.flush();
                auto pat = strip(readln());
                if(pat.empty)
                    return 0;
                write("Test input:");
                stdout.flush();
                auto inp = strip(readln());
                auto m = match(inp, regex(pat));
                writefln("Match status: %s\nResult: %s",m.empty ? "NO" : "YES", m.captures);
            }
            catch(Exception ex)
            {
                writeln("FAIL\n",ex.msg);
            }
        }
    break;
    default:
        writeln("Unknown command ", argv[1]);
        return 1;
    }
    return 0;
}

