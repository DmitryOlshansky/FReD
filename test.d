module test;
import std.algorithm, std.stdio, std.conv, std.range, std.exception;
import std.string: strip;
import std.typetuple, std.ascii;
import fred;
import core.memory;


unittest
{//sanity checks
    regex("(a|b)*");
    regex(`(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*(.*)\s*#`);
    regex("abc|edf|ighrg");
    auto r1 = regex("abc");
    auto r2 = regex("(gylba)");
    assert(match("abcdef", r1).hit == "abc");
    assert(match("wida",r2).empty);
    assert(tmatch("abcdef", r1).hit == "abc");
    assert(tmatch("wida", r2).empty);
    assert(!match("abc", "abc".dup).empty);
    assert(!tmatch("abc", "abc".dup).empty);
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
        string flags;
    };

    enum TestVectors tv[] = [
        TestVectors(  "(a)b\\1",   "abaab","y",    "$&",    "aba" ),
        TestVectors(  "()b\\1",     "aaab", "y",    "$&",    "b" ),
        TestVectors(  "abc",       "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "abc",       "xbc",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "axc",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "abx",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "xabcy","y",    "$&",    "abc" ),
        TestVectors(  "abc",       "ababc","y",    "$&",    "abc" ),
        TestVectors(  "ab*c",      "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab*bc",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab*bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab*bc",     "abbbbc","y",   "$&",    "abbbbc" ),
        TestVectors(  "ab+bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab+bc",     "abc",  "n",    "-",    "-" ),
        TestVectors(  "ab+bc",     "abq",  "n",    "-",    "-" ),
        TestVectors(  "ab+bc",     "abbbbc","y",   "$&",    "abbbbc" ),
        TestVectors(  "ab?bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab?bc",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab?bc",     "abbbbc","n",   "-",    "-" ),
        TestVectors(  "ab?c",      "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "abcc", "n",    "-",    "-" ),
        TestVectors(  "^abc",      "abcc", "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "aabc", "n",    "-",    "-" ),
        TestVectors(  "abc$",      "aabc", "y",    "$&",    "abc" ),
        TestVectors(  "^",         "abc",  "y",    "$&",    "" ),
        TestVectors(  "$",         "abc",  "y",    "$&",    "" ),
        TestVectors(  "a.c",       "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "a.c",       "axc",  "y",    "$&",    "axc" ),
        TestVectors(  "a.*c",      "axyzc","y",    "$&",    "axyzc" ),
        TestVectors(  "a.*c",      "axyzd","n",    "-",    "-" ),
        TestVectors(  "a[bc]d",    "abc",  "n",    "-",    "-" ),
        TestVectors(  "a[bc]d",    "abd",  "y",    "$&",    "abd" ),
        TestVectors(  "a[b-d]e",   "abd",  "n",    "-",    "-" ),
        TestVectors(  "a[b-d]e",   "ace",  "y",    "$&",    "ace" ),
        TestVectors(  "a[b-d]",    "aac",  "y",    "$&",    "ac" ),
        TestVectors(  "a[-b]",     "a-",   "y",    "$&",    "a-" ),
        TestVectors(  "a[b-]",     "a-",   "y",    "$&",    "a-" ),
        TestVectors(  "a[b-a]",    "-",    "c",    "-",    "-" ),
        TestVectors(  "a[]b",      "-",    "c",    "-",    "-" ),
        TestVectors(  "a[",        "-",    "c",    "-",    "-" ),
        TestVectors(  "a]",        "a]",   "y",    "$&",    "a]" ),
        TestVectors(  "a[\\]]b",     "a]b",  "y",  "$&",    "a]b" ),
        TestVectors(  "a[^bc]d",   "aed",  "y",    "$&",    "aed" ),
        TestVectors(  "a[^bc]d",   "abd",  "n",    "-",    "-" ),
        TestVectors(  "a[^-b]c",   "adc",  "y",    "$&",    "adc" ),
        TestVectors(  "a[^-b]c",   "a-c",  "n",    "-",    "-" ),
        TestVectors(  "a[^\\]b]c",   "adc",  "y",  "$&",    "adc" ),
        TestVectors(  "ab|cd",     "abc",  "y",    "$&",    "ab" ),
        TestVectors(  "ab|cd",     "abcd", "y",    "$&",    "ab" ),
        TestVectors(  "()ef",      "def",  "y",    "$&-$1",        "ef-" ),
        TestVectors(  "()*",       "-",    "y",    "-",    "-" ),
        TestVectors(  "*a",        "-",    "c",    "-",    "-" ),
        TestVectors(  "^*",        "-",    "y",    "-",    "-" ),
        TestVectors(  "$*",        "-",    "y",    "-",    "-" ),
        TestVectors(  "(*)b",      "-",    "c",    "-",    "-" ),
        TestVectors(  "$b",        "b",    "n",    "-",    "-" ),
        TestVectors(  "a\\",       "-",    "c",    "-",    "-" ),
        TestVectors(  "a\\(b",     "a(b",  "y",    "$&-$1",        "a(b-" ),
        TestVectors(  "a\\(*b",    "ab",   "y",    "$&",    "ab" ),
        TestVectors(  "a\\(*b",    "a((b", "y",    "$&",    "a((b" ),
        TestVectors(  "a\\\\b",    "a\\b", "y",    "$&",    "a\\b" ),
        TestVectors(  "abc)",      "-",    "c",    "-",    "-" ),
        TestVectors(  "(abc",      "-",    "c",    "-",    "-" ),
        TestVectors(  "((a))",     "abc",  "y",    "$&-$1-$2",    "a-a-a" ),
        TestVectors(  "(a)b(c)",   "abc",  "y",    "$&-$1-$2",    "abc-a-c" ),
        TestVectors(  "a+b+c",     "aabbabc","y",  "$&",    "abc" ),
        TestVectors(  "a**",       "-",    "c",    "-",    "-" ),
        TestVectors(  "a*?a",      "aa",   "y",    "$&",    "a" ),
        TestVectors(  "(a*)*",     "aaa",  "y",    "-",    "-" ),
        TestVectors(  "(a*)+",     "aaa",  "y",    "-",    "-" ),
        TestVectors(  "(a|)*",     "-",    "y",    "-",    "-" ),
        TestVectors(  "(a*|b)*",   "aabb", "y",    "-",    "-" ),
        TestVectors(  "(a|b)*",    "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)*",   "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)+",   "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)?",   "ab",   "y",    "$&-$1",        "a-a" ),
        TestVectors(  "[^ab]*",    "cde",  "y",    "$&",    "cde" ),
        TestVectors(  "(^)*",      "-",    "y",    "-",    "-" ),
        TestVectors(  "(ab|)*",    "-",    "y",    "-",    "-" ),
        TestVectors(  ")(",        "-",    "c",    "-",    "-" ),
        TestVectors(  "",  "abc",  "y",    "$&",    "" ),
        TestVectors(  "abc",       "",     "n",    "-",    "-" ),
        TestVectors(  "a*",        "",     "y",    "$&",    "" ),
        TestVectors(  "([abc])*d", "abbbcd",       "y",    "$&-$1",        "abbbcd-c" ),
        TestVectors(  "([abc])*bcd", "abcd",       "y",    "$&-$1",        "abcd-a" ),
        TestVectors(  "a|b|c|d|e", "e",    "y",    "$&",    "e" ),
        TestVectors(  "(a|b|c|d|e)f", "ef",        "y",    "$&-$1",        "ef-e" ),
        TestVectors(  "((a*|b))*", "aabb", "y",    "-",    "-" ),
        TestVectors(  "abcd*efg",  "abcdefg",      "y",    "$&",    "abcdefg" ),
        TestVectors(  "ab*",       "xabyabbbz",    "y",    "$&",    "ab" ),
        TestVectors(  "ab*",       "xayabbbz",     "y",    "$&",    "a" ),
        TestVectors(  "(ab|cd)e",  "abcde",        "y",    "$&-$1",        "cde-cd" ),
        TestVectors(  "[abhgefdc]ij",      "hij",  "y",    "$&",    "hij" ),
        TestVectors(  "^(ab|cd)e", "abcde",        "n",    "x$1y",        "xy" ),
        TestVectors(  "(abc|)ef",  "abcdef",       "y",    "$&-$1",        "ef-" ),
        TestVectors(  "(a|b)c*d",  "abcd",         "y",    "$&-$1",        "bcd-b" ),
        TestVectors(  "(ab|ab*)bc",        "abc",  "y",    "$&-$1",        "abc-a" ),
        TestVectors(  "a([bc]*)c*",        "abc",  "y",    "$&-$1",        "abc-bc" ),
        TestVectors(  "a([bc]*)(c*d)",     "abcd", "y",    "$&-$1-$2",    "abcd-bc-d" ),
        TestVectors(  "a([bc]+)(c*d)",     "abcd", "y",    "$&-$1-$2",    "abcd-bc-d" ),
        TestVectors(  "a([bc]*)(c+d)",     "abcd", "y",    "$&-$1-$2",    "abcd-b-cd" ),
        TestVectors(  "a[bcd]*dcdcde",     "adcdcde",      "y",    "$&",    "adcdcde" ),
        TestVectors(  "a[bcd]+dcdcde",     "adcdcde",      "n",    "-",    "-" ),
        TestVectors(  "(ab|a)b*c", "abc",           "y",    "$&-$1",        "abc-ab" ),
        TestVectors(  "((a)(b)c)(d)",      "abcd",  "y",    "$1-$2-$3-$4",      "abc-a-b-d" ),
        TestVectors(  "[a-zA-Z_][a-zA-Z0-9_]*",    "alpha",        "y",    "$&",    "alpha" ),
        TestVectors(  "^a(bc+|b[eh])g|.h$",        "abh",  "y",    "$&-$1",        "bh-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "effgz",        "y",    "$&-$1-$2",    "effgz-effgz-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "ij",   "y",    "$&-$1-$2",    "ij-ij-j" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "effg", "n",    "-",    "-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "bcdd", "n",    "-",    "-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "reffgz",       "y",    "$&-$1-$2",    "effgz-effgz-" ),
        TestVectors(  "(((((((((a)))))))))",       "a",    "y",    "$&",    "a" ),
        TestVectors(  "multiple words of text",    "uh-uh",        "n",    "-",    "-" ),
        TestVectors(  "multiple words",    "multiple words, yeah", "y",    "$&",    "multiple words" ),
        TestVectors(  "(.*)c(.*)", "abcde",                "y",    "$&-$1-$2",    "abcde-ab-de" ),
        TestVectors(  "\\((.*), (.*)\\)",  "(a, b)",       "y",    "($2, $1)",   "(b, a)" ),
        TestVectors(  "abcd",      "abcd",                   "y",    "$&-&-$$$&",  "abcd-&-$abcd" ),
        TestVectors(  "a(bc)d",    "abcd",                 "y",    "$1-$$1-$$$1",    "bc-$1-$bc" ),
        TestVectors(  "[k]",                       "ab",   "n",    "-",    "-" ),
        TestVectors(  "[ -~]*",                    "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~]*",                 "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~]*",              "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~]*",           "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~]*",        "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~ -~]*",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~ -~ -~]*",  "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "a{2}",      "candy",                "n",    "",     "" ),
        TestVectors(  "a{2}",      "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{2}",      "caaandy",              "y",    "$&",    "aa" ),
        TestVectors(  "a{2,}",     "candy",                "n",    "",     "" ),
        TestVectors(  "a{2,}",     "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{2,}",     "caaaaaandy",           "y",    "$&",    "aaaaaa" ),
        TestVectors(  "a{1,3}",    "cndy",                 "n",    "",     "" ),
        TestVectors(  "a{1,3}",    "candy",                "y",    "$&",    "a" ),
        TestVectors(  "a{1,3}",    "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{1,3}",    "caaaaaandy",           "y",    "$&",    "aaa" ),
        TestVectors(  "e?le?",     "angel",                "y",    "$&",    "el" ),
        TestVectors(  "e?le?",     "angle",                "y",    "$&",    "le" ),
        TestVectors(  "\\bn\\w",   "noonday",              "y",    "$&",    "no" ),
        TestVectors(  "\\wy\\b",   "possibly yesterday",   "y",    "$&",    "ly" ),
        TestVectors(  "\\w\\Bn",   "noonday",              "y",    "$&",    "on" ),
        TestVectors(  "y\\B\\w",   "possibly yesterday",   "y",    "$&",    "ye" ),
        TestVectors(  "\\cJ",      "abc\ndef",             "y",    "$&",    "\n" ),
        TestVectors(  "\\d",       "B2 is",                "y",    "$&",    "2" ),
        TestVectors(  "\\D",       "B2 is",                "y",    "$&",    "B" ),
        TestVectors(  "\\s\\w*",   "foo bar",              "y",    "$&",    " bar" ),
        TestVectors(  "\\S\\w*",   "foo bar",              "y",    "$&",    "foo" ),
        TestVectors(  "abc",       "ababc",                "y",    "$&",    "abc" ),
        TestVectors(  "apple(,)\\sorange\\1",      "apple, orange, cherry, peach", "y", "$&", "apple, orange," ),
        TestVectors(  "(\\w+)\\s(\\w+)",           "John Smith", "y", "$2, $1", "Smith, John" ),
        TestVectors(  "\\n\\f\\r\\t\\v",           "abc\n\f\r\t\vdef", "y", "$&", "\n\f\r\t\v" ),
        TestVectors(  ".*c",       "abcde",                        "y",    "$&",    "abc" ),
        TestVectors(  "^\\w+((;|=)\\w+)+$", "some=host=tld",    "y", "$&-$1-$2", "some=host=tld-=tld-=" ),
        TestVectors(  "^\\w+((\\.|-)\\w+)+$", "some.host.tld",    "y", "$&-$1-$2", "some.host.tld-.tld-." ),
        TestVectors(  "q(a|b)*q",  "xxqababqyy",                "y",    "$&-$1",        "qababq-b" ),
        TestVectors(  "^(a)(b){0,1}(c*)",   "abcc", "y", "$1 $2 $3", "a b cc" ),
        TestVectors(  "^(a)((b){0,1})(c*)", "abcc", "y", "$1 $2 $3", "a b b" ),
        TestVectors(  "^(a)(b)?(c*)",       "abcc", "y", "$1 $2 $3", "a b cc" ),
        TestVectors(  "^(a)((b)?)(c*)",     "abcc", "y", "$1 $2 $3", "a b b" ),
        TestVectors(  "^(a)(b){0,1}(c*)",   "acc",  "y", "$1 $2 $3", "a  cc" ),
        TestVectors(  "^(a)((b){0,1})(c*)", "acc",  "y", "$1 $2 $3", "a  " ),
        TestVectors(  "^(a)(b)?(c*)",       "acc",  "y", "$1 $2 $3", "a  cc" ),
        TestVectors(  "^(a)((b)?)(c*)",     "acc",  "y", "$1 $2 $3", "a  " ),
        TestVectors(  "(?:ab){3}",       "_abababc","y", "$&-$1",    "ababab-" ),
        TestVectors(  "(?:a(?:x)?)+",    "aaxaxx",  "y", "$&-$1-$2", "aaxax--" ),
        TestVectors(  `\W\w\W`,         "aa b!ca",  "y", "$&",       " b!"),
        //no lookaround for Thompson VM
     /+   TestVectors("foo.(?=bar)",     "foobar foodbar", "y","$&-$1", "food-" ),
        TestVectors("(?:(.)(?!\\1))+",  "12345678990", "y", "$&-$1", "12345678-8" ), +/
//more repetitions:
        TestVectors(  "(?:a{2,4}b{1,3}){1,2}",  "aaabaaaabbb", "y", "$&", "aaabaaaabbb" ),
        TestVectors(  "(?:a{2,4}b{1,3}){1,2}?", "aaabaaaabbb", "y", "$&", "aaab" ),
//groups:
        TestVectors(  "(abc)|(edf)|(xyz)",     "xyz",             "y",   "$1-$2-$3","--xyz"),
        TestVectors(  "(?P<q>\\d+)/(?P<d>\\d+)",     "2/3",       "y",     "${d}/${q}",    "3/2"),
//set operations:
        TestVectors(  "[a-z--d-f]",                  " dfa",      "y",   "$&",     "a"),
        TestVectors(  "[abc[pq--acq]]{2}",           "bqpaca",    "y",   "$&",     "pa"),
        TestVectors(  "[a-z9&&abc0-9]{3}",           "z90a0abc",  "y",   "$&",     "abc"),
        TestVectors(  "[0-9a-f~~0-5a-z]{2}",         "g0a58x",    "y",   "$&",     "8x"),
        TestVectors(  "[abc[pq]xyz[rs]]{4}",         "cqxr",      "y",   "$&",     "cqxr"),
        TestVectors(  "[abcdf--[ab&&[bcd]][acd]]",   "abcdefgh",  "y",   "$&",     "f"),
//unicode blocks & properties:
        TestVectors(  `\p{InLatin-1 Supplement}\p{in-mathematical-operators}\P{Inlatin1suppl ement}`, "\u00c2\u2200\u00c3\u2203.", "y", "$&", "\u00c3\u2203."),
        TestVectors(  `[-+*/\p{in-mathematical-operators}]{2}`,    "a+\u2212",    "y",    "$&",    "+\u2212"),
        TestVectors(  `\p{Ll}+`,                      "XabcD",    "y",  "$&",      "abc"),
        TestVectors(  `\p{Lu}+`,                      "абвГДЕ",   "y",  "$&",      "ГДЕ"),
        TestVectors(  `^\p{Currency Symbol}\p{Sc}`    "$₤",       "y",  "$&",      "$₤"),  
        TestVectors(  `\p{Common}\p{Thai}`            "!ฆ",       "y",  "$&",      "!ฆ"), 
        TestVectors(  `[\d\s]*\D`,     "12 \t3\U00001680\u0F20_2",        "y",  "$&", "12 \t3\U00001680\u0F20_"),
//case insensitive:
        TestVectors(   `^abcdEf$`,           "AbCdEF"               "y",   "$&", "AbCdEF",      "i"),
        TestVectors(   `Русский язык`,    "рУсскИй ЯзЫк",           "y",   "$&", "рУсскИй ЯзЫк",     "i"),
        TestVectors(   `ⒶⒷⓒ` ,        "ⓐⓑⒸ",                   "y",   "$&", "ⓐⓑⒸ",      "i"),
        TestVectors(   "\U00010400{2}",  "\U00010428\U00010400 ",   "y",   "$&", "\U00010428\U00010400", "i"),
        TestVectors(   `[adzУ-Я]{4}`,    "DzюА"                     "y",   "$&", "DzЮа", "i"),
        TestVectors(   `\p{L}\p{Letter}{10}`, "абвгдеЖЗИКЛ",        "y",   "$&", "абвгдеЖЗИКЛ", "i"),
        TestVectors(   `(?:Dåb){3}`,  "DåbDÅBdÅb",                  "y",   "$&", "DåbDÅBdÅb", "i"),
//escapes:
        TestVectors(    `\u0041\u005a\U00000065\u0001`,         "AZe\u0001",       "y",   "$&", "AZe\u0001"),  
        TestVectors(    `\u`,               "",   "c",   "-",  "-"),
        TestVectors(    `\U`,               "",   "c",   "-",  "-"),
        TestVectors(    `\u003`,            "",   "c",   "-",  "-"),
        TestVectors(    `[\x00-\x7f]{4}`,        "\x00\x09ab",   "y", "$&", "\x00\x09ab"),
        TestVectors(    `[\cJ\cK\cA-\cD]{3}\cQ`, "\x01\x0B\x0A\x11", "y", "$&", "\x01\x0B\x0A\x11"),
        TestVectors(    `\r\n\v\t\f\\`,     "\r\n\v\t\f\\",   "y",   "$&", "\r\n\v\t\f\\"),
        TestVectors(    `[\u0003\u0001]{2}`,  "\u0001\u0003",         "y",   "$&", "\u0001\u0003"),
        TestVectors(    `^[\u0020-\u0080\u0001\n-\r]{8}`,  "abc\u0001\v\f\r\n",  "y",   "$&", "abc\u0001\v\f\r\n"),
        TestVectors(    `\w+\S\w+`, "ab7!44c",  "y", "$&", "ab7!44c"),
        TestVectors(    `\b\w+\b`,  " abde4 ",  "y", "$&", "abde4"),
        TestVectors(    `\b\w+\b`,  " abde4",   "y", "$&", "abde4"),
        TestVectors(    `\b\w+\b`,  "abde4 ",   "y", "$&", "abde4"),
// no newline inside \r\n :
        TestVectors(    `\r.*?$`,    "abc\r\nxy", "y",    "$&", "\r\nxy"),
// luckily obtained regression on incremental matching in backtracker
        TestVectors(  `^(?:(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*([^ ]*)\s*#|# (?:\w|_)+=((?:\w|_)+))`,
            "0020  ; White_Space # ", "y", "$1-$2-$3", "--0020"),
//lookback
        TestVectors(   `(?<=(ab))\d`,    "12ba3ab4",    "y",   "$&-$1", "4-ab",  "i"),
        TestVectors(   `\w(?<!\d)\w`,   "123ab24",  "y",   "$&", "ab"),
        TestVectors(   `(?<=Dåb)x\w`,  "DåbDÅBxdÅb",  "y",   "$&", "xd", "i"),
        TestVectors(   `(?<=(ab*c))x`,   "abbbbcxac",  "y",   "$&-$1", "x-abbbbc"),
        TestVectors(   `(?<=(ab*?c))x`,   "abbbbcxac",  "y",   "$&-$1", "x-abbbbc"),
        TestVectors(   `(?<=(a.*?c))x`,   "ababbcxac",  "y",   "$&-$1", "x-abbc"),
        TestVectors(   `(?<=(a{2,4}b{1,3}))x`,   "yyaaaabx",  "y",   "$&-$1", "x-aaaab"),
        TestVectors(   `(?<=((?:a{2,4}b{1,3}){1,2}))x`,   "aabbbaaaabx",  "y",   "$&-$1", "x-aabbbaaaab"),
        TestVectors(   `(?<=((?:a{2,4}b{1,3}){1,2}?))x`,   "aabbbaaaabx",  "y",   "$&-$1", "x-aaaab"),
        TestVectors(   `(?<=(abc|def|aef))x`,    "abcx", "y",        "$&-$1",  "x-abc"),
        TestVectors(   `(?<=(abc|def|aef))x`,    "aefx", "y",        "$&-$1",  "x-aef"),
        TestVectors(   `(?<=(abc|dabc))x`,    "dabcx", "y",        "$&-$1",  "x-abc"),
        TestVectors(   `(?<=(|abc))x`,        "dabcx", "y",        "$&-$1",  "x-"),
        TestVectors(   `(?<=((ab|da)*))x`,    "abdaabx", "y",        "$&-$2-$1",  "x-ab-abdaab"),
        ];
    string produceExpected(M,String)(M m, String fmt)
    {
        auto app = appender!(String)();
        replaceFmt(fmt, m.captures, app, true);
        return app.data;
    }
    void run_tests(alias matchFn)()
    {
        int i;
        foreach(Char; TypeTuple!(char, wchar, dchar))
        {
            alias immutable(Char)[] String;
            String produceExpected(M,Range)(M m, Range fmt)
            {
                auto app = appender!(String)();
                replaceFmt(fmt, m.captures, app, true);
                return app.data;
            }
            Regex!(Char) r;
            foreach(a, tvd; tv)
            {
                uint c = tvd.result[0];
                try
                {
                    i = 1;
                    r = regex(to!(String)(tvd.pattern), to!String(tvd.flags));
                }
                catch (RegexException e)
                {
                    i = 0;
                    //debug writeln(e.msg);
                }

                assert((c == 'c') ? !i : i, "failed to compile pattern "~tvd.pattern);

                if(c != 'c')
                {
                    auto m = matchFn(to!(String)(tvd.input), r);
                    i = !m.empty;
                    assert((c == 'y') ? i : !i, text(matchFn.stringof ~": failed to match pattern #", a ,": ", tvd.pattern));
                    if(c == 'y')
                    {
                        debug writeln(" Test #", a, " pattern: ", tvd.pattern);
                        auto result = produceExpected(m, to!(String)(tvd.format));
                        assert(result == to!String(tvd.replace), text(matchFn.stringof ~": mismatch pattern #", a, ": ", tvd.pattern," expected: ",
                                    tvd.replace, " vs ", result));
                    }
                }
            }
        }
        debug writeln("!!! FReD bulk test done "~matchFn.stringof~" !!!");
    }
    static string generate(uint n,uint[] black_list...)
    {
        string s = "TypeTuple!(";
        for(uint i=0; i<n; i++)
        {
            uint j;
            for(j =0; j<black_list.length; j++)
                if(i == black_list[j])
                    break;
            if(j == black_list.length)
            {
                s ~= to!string(i);
                s ~= ",";
            }
        }
        s ~= ")";
        return s;
    }
    //CTFE parsing
    version(fred_ct)
    void ct_tests()
    {
        foreach(a, v; mixin(generate(140,38,39,40,52,55,57,62,63,67,80,190,191,192)))
        {
            enum tvd = tv[v];
            enum r = regex(tvd.pattern, tvd.flags);
            auto nr = regex(tvd.pattern, tvd.flags);
            debug
            {
                writeln(" Test #", a, " pattern: ", tvd.pattern);
                if(!equal(r.ir, nr.ir))
                {
                    writeln("C-T version :");
                    r.print();
                    writeln("R-T version :");
                    nr.print();
                    assert(0, text("!C-T regex! failed to compile pattern #", a ,": ", tvd.pattern));
                }
            }
            else
                assert(equal(r.ir, nr.ir), text("!C-T regex! failed to compile pattern #", a ,": ", tvd.pattern));
            
        }
        //enum x = regex(tv[141].pattern);
        debug writeln("!!! FReD C-T test done !!!");
    }
    
    version(fred_ct)
        ct_tests();
    else
    {
        run_tests!match(); //backtracker
        run_tests!tmatch(); //thompson VM
    }
}
 version(fred_ct)
 {
    unittest
    {
        auto cr = ctRegex!("abc");
        assert(match("abc",cr).hit == "abc");
        auto cr2 = ctRegex!("ab*c"); 
        assert(match("abbbbc",cr2).hit == "abbbbc");
        auto cr3 = ctRegex!("^abc$"); 
        assert(match("abc",cr3).hit == "abc");
        auto cr4 = ctRegex!(`\b(a\B[a-z]b)\b`); 
        assert(array(match("azb",cr4).captures) == ["azb", "azb"]);
        auto cr5 = ctRegex!("(?:a{2,4}b{1,3}){1,2}");
        assert(match("aaabaaaabbb", cr5).hit == "aaabaaaabbb");
        auto cr6 = ctRegex!("(?:a{2,4}b{1,3}){1,2}?");
        assert(match("aaabaaaabbb",  cr6).hit == "aaab");
        
        //CTFE parser BUG is triggered by group 
        //in the middle (at least not first and not last) in regex
        enum testCT = regex(`abc|(edf)|xyz`);
        auto testRT = regex(`abc|(edf)|xyz`);
        debug
        {
            writeln("C-T version :");
            testCT.print();
            writeln("R-T version :");
            testRT.print();
        }
        
        assert(testCT.ir == testRT.ir);
        
    }
}
else
{
    unittest
    {
    //global matching
        void test_body(alias matchFn)()
        {
            string s = "a quick brown fox jumps over a lazy dog";
            auto r1 = regex("\\b[a-z]+\\b","g");
            string[] test;
            foreach(m; matchFn(s, r1))
                test ~= m.hit;
            assert(equal(test, [ "a", "quick", "brown", "fox", "jumps", "over", "a", "lazy", "dog"]));
            debug writeln("!!! FReD FLAGS test done "~matchFn.stringof~" !!!");
        }
        test_body!match();
        test_body!tmatch();
    }

    //tests for accomulated std.regex issues
    unittest
    {
        void test_body(alias matchFn)()
        {
            //issue 5857
            //matching goes out of control if ... in (...){x} has .*/.+
            auto c = match("axxxzayyyyyzd",regex("(a.*z){2}d")).captures;
            assert(c[0] == "axxxzayyyyyzd");
            assert(c[1] == "ayyyyyz");
            auto c2 = match("axxxayyyyyd",regex("(a.*){2}d")).captures;
            assert(c2[0] == "axxxayyyyyd");
            assert(c2[1] == "ayyyyy");
            //issue 2108
            //greedy vs non-greedy
            auto nogreed = regex("<packet.*?/packet>");
            assert(match("<packet>text</packet><packet>text</packet>", nogreed).hit
                   == "<packet>text</packet>");
            auto greed =  regex("<packet.*/packet>");
            assert(match("<packet>text</packet><packet>text</packet>", greed).hit
                   == "<packet>text</packet><packet>text</packet>");
            //issue 4574
            //empty successful match still advances the input
            string[] pres, posts, hits;
            foreach(m; match("abcabc", regex("","g"))) {
                pres ~= m.pre;
                posts ~= m.post;
                assert(m.hit.empty);

            }
            auto heads = [
                "abcabc",
                "abcab",
                "abca",
                "abc",
                "ab",
                "a",
                ""
            ];
            auto tails = [
                "abcabc",
                 "bcabc",
                  "cabc",
                   "abc",
                    "bc",
                     "c",
                      ""
            ];
            assert(pres == array(retro(heads)));
            assert(posts == tails);
            //issue 6076
            //regression on .*
            auto re = regex("c.*|d");
            auto m = match("mm", re);
            assert(m.empty);
            debug writeln("!!! FReD REGRESSION test done "~matchFn.stringof~" !!!");
        }
        test_body!match();
        test_body!tmatch();
    }
    //@@@BUG@@@ template function doesn't work inside unittest block
    version(unittest)
    String baz(String)(RegexMatch!(String) m)
    {
        return std.string.toUpper(m.hit);
    }

    // tests for replace 
    unittest
    {
        void test(alias matchFn)()
        {
            foreach(i, v; TypeTuple!(string, wstring, dstring))
            {
                alias v String;
                assert(fred.replace!(String, matchFn)(to!String("ark rapacity"), regex("r"), to!String("c"))
                       == to!String("ack rapacity"));
                assert(fred.replace!(String, matchFn)(to!String("ark rapacity"), regex("r", "g"), to!String("c"))
                       == to!String("ack capacity"));
                assert(fred.replace!(String, matchFn)(to!String("noon"), regex("^n"), to!String("[$&]"))
                       == to!String("[n]oon"));
                assert(fred.replace!(String, matchFn)(to!String("test1 test2"), regex(`\w+`,"g"), to!String("$`:$'"))
                       == to!String(": test2 test1 :"));
                auto s = fred.replace!(baz)(to!String("Strap a rocket engine on a chicken."),
                        regex("[ar]", "g"));
                assert(s == "StRAp A Rocket engine on A chicken.");
            }
            debug writeln("!!! Replace test done "~matchFn.stringof~"  !!!");
        }
        test!(match)();
        test!(tmatch)();
    }

    // tests for splitter
    unittest
    {
        auto s1 = ", abc, de,     fg, hi, ";
        auto sp1 = splitter(s1, regex(", *"));
        auto w1 = ["", "abc", "de", "fg", "hi", ""];
        assert(equal(sp1, w1));
    
        auto s2 = ", abc, de,  fg, hi";
        auto sp2 = splitter(s2, regex(", *"));
        auto w2 = ["", "abc", "de", "fg", "hi"];

        uint cnt;
        foreach(e; sp2) {
            assert(w2[cnt++] == e);
        }
        assert(equal(sp2, w2));
    }

    unittest
    {
        char[] s1 = ", abc, de,  fg, hi, ".dup;
        auto sp2 = splitter(s1, regex(", *"));
    }

    unittest
    {
        auto s1 = ", abc, de,  fg, hi, ";
        auto w1 = ["", "abc", "de", "fg", "hi", ""];
        assert(equal(split(s1, regex(", *")), w1[]));
    }

}


version(unittest){
void main(){}
}
else{

int main(string[] argv)
{
    if(argv.length < 2)
    {
        writefln("regex test\nUsage %s <compile | exec> \n"
                 "Patterns to test and input are read from STDIN by line till empty one\n",argv[0]);
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
                auto re = regex(s);
                write(" OK \n");
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


}
