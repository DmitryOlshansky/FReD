//Written in the D programming language
/**
 * Fast Regular expressions for D
 *
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * Authors: Dmitry Olshansky
 *
 */
module regex;

import std.stdio;
import std.algorithm, std.range, std.conv, std.exception, std.ctype, std.format, std.typecons;

enum:uint {
    IRchar              =      0,
    IRstring            =  1<<24,
    IRany               =  2<<24,
    IRcharset           =  3<<24,
    IRalter             =  4<<24,
    IRnm                =  5<<24,
    IRstar              =  6<<24,
    IRcross             =  7<<24,
    IRquest             =  8<<24,
    IRnmq               =  9<<24,
    IRstarq             = 10<<24,
    IRcrossq            = 11<<24,
    IRconcat            = 12<<24,
    IRdigit             = 13<<24,
    IRnotdigit          = 14<<24,
    IRspace             = 15<<24,
    IRnotspace          = 16<<24,
    IRword              = 17<<24,
    IRnotword           = 18<<24,    
    IRgroup             = 19<<24,
    IRbackref           = 20<<24,
    
    IRwordboundary      = 31<<24,
    IRnotwordboundary   = 32<<24,
    IRlookahead         = 33<<24,
    IRneglookahead      = 34<<24,
    IRlookbehind        = 35<<24,
    IRneglookbehind     = 36<<24,
    
    //TODO: ...
    IRlambda            = 128<<24
};

struct RecursiveParser(R)
if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;//keep full pattern for pretty printing error messages
    uint[] ir;
    uint[] index; //user group number -> internal number
    struct NamedGroup
    { 
        string name; 
        uint group;
    }
    NamedGroup[] dict; //for named ones
    //current num of group, current nesting, and peak number of group
    uint nsub = 0, nesting = 0, top = 0; 
    
    this(R pattern)
    {
        pat = origin = pattern;     
        ir.reserve(pat.length);
        next();
        parseRegex();
    }
    @property dchar current(){ return _current; }
    bool next()
    {
        if(pat.empty)
        {
            empty =  true;
            return false;
        }
        _current = pat.front;
        pat.popFront();
        return true;
    }
    void skipSpace()
    {
        while(isspace(current) && next()){ }
    }
    void restart(R newpat)
    { 
        pat = newpat;
        empty = false;
        next();
    }
    void put(uint code){  ir ~= code; }    
    uint parseDecimal()
    {
        uint r=0;
        while(isdigit(current))
        {
            if(r >= (uint.max/10)) 
                error("Overflow in decimal number");
            r = 10*r + cast(uint)(current-'0'); 
            next();
        }
        return r;
    }
    void parseRegex()
    {
        while(!empty)
            switch(current)
            {
                case '|':
                    nsub = nesting;
                    next();
                    //alternation
                    parseConcat();
                    put(IRalter);
                    //TODO: account empty alternation  (a|) -> (a|*lambda*)
                    break;
                case ')':
                    return;
                default:
                    parseConcat();
            }
    }
    void parseConcat()
    {
        parseRepetition();
        while(!empty)
            switch(current)
            {
            case '|': case ')':
                return;
            default:
                parseRepetition();
                put(IRconcat);
            }
    }
    void parseRepetition()
    {
        parseAtom();
        if(empty)
            return;
        uint min, max;
        switch(current)
        {
        case '*':
            if(next())
                if(current == '?')
                {
                    put(IRstarq); 
                    next();
                }
            else
                put(IRstar);
            break;
        case '?':
            next();
            put(IRquest);
            break;
        case '+':
            if(next())
                if(current == '?')
                {
                    put(IRcrossq); 
                    next();
                }
            else
                put(IRcross);
            break;
        case '{':
            next() || error("Unexpected end of regex pattern");
            isdigit(current) || error("First number required in repetition"); 
            min = parseDecimal();    
            skipSpace();
            if(current == '}')
                max = min;
            else if(current == ',')
            {
                next();
                if(isdigit(current))
                    max = parseDecimal();
                else if(current == '}')
                    max = infinite;
                else
                    error("Unexpected symbol in regex pattern"); 
                skipSpace();
                if(current != '}')
                    error("Unmatched '{' in regex pattern");
            }
            else
                error("Unexpected symbol in regex pattern");
            next();       
            if(current == '?')
            {
                put(IRnmq);
                next();
            }
            else
                put(IRnm);
            put(min);
            put(max);
                    
            break;
        default:
            break;
        }
    }
    void parseAtom()
    {
        if(empty)
            return;
        switch(current)
        {
        case '*', '?', '+', '|', '{', '}':
            error("'*', '+', '?', '{', '}' not allowed in atom");
            break;
        case '.':
            put(IRany);
            next();
            break;
        case '(':
            R save = pat;
            next();
            uint op = 0, nglob;
            if(current == '?')
            {
                next();
                switch(current)
                {
                case '=':
                    op = IRlookahead;
                    next();
                    break;
                case '!':
                    op = IRneglookahead;
                    next();
                    break;
                case 'P':
                    next();
                    if(current != '<')
                        error("Expected '<' in named group");
                    string name;
                    while(next() && isalpha(current))
                    {
                        name ~= current;
                    }
                    if(current != '>')
                        error("Expected '>' closing named group");
                    next();
                    auto old = nsub++;
                    nesting++;
                    top = max(nsub,top);
                    nglob = cast(uint)index.length;
                    index ~= old;
                    auto t = NamedGroup(name,old);
                    auto d = assumeSorted!"a.name < b.name"(dict);
                    auto ind = d.lowerBound(t).length;
                    insertInPlace(dict, ind, t);
                    op = IRgroup | old;
                    break;
                case '<':
                    next();
                    if(current == '=')
                        op = IRlookbehind;
                    else if(current == '!')
                        op = IRneglookbehind;
                    else
                        error("'!' or '=' expected after '<'");
                    next();
                    break;
                default:
                    //nothing
                }
            }
            else
            {
                auto old = nsub++;
                nesting++;
                top = max(nsub,top);
                nglob = cast(uint)index.length;
                index ~= old;
                op = IRgroup | old;
            }
            parseRegex();
            if(current != ')')
            {
                pat = save;
                error("Unmatched '(' in regex pattern");
            }
            assert(nsub < (1<<24));
            if((op & 0xff00_0000) == IRgroup)
            {
                assert(nesting);
                --nesting;
                put(op);
                put(nglob);
            }
            else if(op)
                put(op);
            next();
            break;
        case '[':
            //range
            assert(0);
            break;
        case '\\':
            //escape
            next() || error("Unfinished escape sequence");
            put(escape());
            break;
        case ')':
            break;
        default:
            put(current);
            next();
        }
    }
    uint escape()
    {
        switch(current)
        {
        case 'f':   next(); return '\f';
        case 'n':   next(); return '\n';
        case 'r':   next(); return '\r';
        case 't':   next(); return '\t';
        case 'v':   next(); return '\v';
            
        case 'd':   next(); return IRdigit; 
        case 'D':   next(); return IRnotdigit; 
        case 'b':   next(); return IRwordboundary;
        case 'B':   next(); return IRnotwordboundary;
        case 's':   next(); return IRspace;
        case 'S':   next(); return IRnotspace;
        case 'w':   next(); return IRword;
        case 'W':   next(); return IRnotword;
        case 'x':            
            auto save = pat;
            uint code = 0;
            for(int i=0;i<2;i++)
            {
                if(!next())
                {
                    restart(save);
                    return 'x';
                }
                if('0' <= current && current <= '9')
                    code = code * 16 + current - '0';
                else if('a' <= current && current <= 'f')
                    code = code * 16 + current -'a' + 10;
                else if('A' <= current && current <= 'Z')
                    code = code * 16 + current - 'A' + 10;
                else//wrong unicode escape treat \x like 'x' 
                {
                    restart(save);
                    return 'x';
                }
            }
            next();
            return code;
        case 'u':
            auto save = pat;
            uint code = 0;
            for(int i=0; i<4; i++)
            {
                if(!next())
                {
                    restart(save);
                    return 'u';
                }
                if('0' <= current && current <= '9')
                    code = code * 16 + current - '0';
                else if('a' <= current && current <= 'f')
                    code = code * 16 + current -'a' + 10;
                else if('A' <= current && current <= 'Z')
                    code = code * 16 + current - 'A' + 10;
                else //wrong unicode escape treat \u like 'u' 
                {
                    restart(save);
                    return 'u';
                }
            }
            next();
            return code;
        case 'c': //control codes                      
            next() || error("Unfinished escape sequence");
            (('a' <= current && current <= 'z') || ('A' <= current && current <= 'Z'))
                || error("Only letters are allowed after \\c");
            return current &  0x1f;
        case '0':
            next();
            return 0;//NUL character
        case '1': .. case '9':
            uint nref = cast(uint)current - '0'; 
            //groups counted from zero, so nref comes greater by 1 hence '<='
            nref <=  index.length || error("Backref to unseen group");
            while(nref <= index.length && next() && isdigit(current))
            {
                nref = nref * 10 + current - '0';
            }
            if(nref > index.length)
                nref /= 10;
            nref--;
            return IRbackref | nref;
        }
    }
    void error(string msg)
    {
        auto app = appender!string;
        formattedWrite(app,"%s\nPattern with error: `%s <--HERE-- %s`",
                       msg, origin[0..$-pat.length], pat);
        throw new RegexException(app.data);
    }
    void printPostfix()
    {
        for(size_t i=0;i<ir.length;i++)
        {
            switch(ir[i] & 0xff00_0000)
            {
            case IRchar:
                write(cast(dchar)ir[i]);
                break;
            case IRany:
                write("(.)");
                break;
            case IRconcat:
                write('.');
                break;
            case IRquest:
                write('?');
                break;
            case IRstar:
                write('*');
                break;
            case IRstarq:
                write("*?");
                break;
            case IRcross:
                write('+');
                break;
            case IRcrossq:
                write("+?");
                break;
            case IRnm:
                writef("{%u,%u}",ir[i+1],ir[i+2]);
                i += 2;//2 extra words
                break;
            case IRnmq:
                writef("{%u,%u}?",ir[i+1],ir[i+2]);
                i += 2;//ditto
                break;
            case IRalter:
                write('|');
                break;
            case IRgroup:
                uint n = ir[i] & 0x00ff_ffff;
                // Ouch: '!vthis->csym' on line 713 in file 'glue.c'
                //auto ng = find!((x){ return x.group == n; })(dict); 
                string name;
                foreach(v;dict)
                    if(v.group == n)
                    {
                        name = "<"~v.name~">";
                        break;   
                    }
                writef("(%s%u->%u)", name, n, ir[i+1]);
                i++;//1 extra word
                break;
            case IRlookahead:
                uint n = ir[i] & 0x00ff_ffff;
                writef("(?=%u)",  n);
                break;
            case IRneglookahead: 
                uint n = ir[i] & 0x00ff_ffff;
                writef("(?!%u)",  n);
                break;
            case IRlookbehind:
                uint n = ir[i] & 0x00ff_ffff;
                writef("(?<=%u)",  n);
                break;
            case IRneglookbehind:
                uint n = ir[i] & 0x00ff_ffff;
                writef("(?<!%u)",  n);
                break;
            case IRbackref:
                uint n = ir[i] & 0x00ff_ffff;
                writef("\\%u",  n);
                break;
            }
        }
    }
}

//Actual instructions for VM 
enum InstType:uint {
    Banychar,
    Bchar,
    Bnotchar,
    Brange,
    Bnotrange,
    Bstring,
    Bsplit,
    Bsave
};

struct GenericInst{
    InstType type;
    union{
        dchar ch;
        uint nsave;
        struct{
            dchar low,hi;
        }
        struct{
            uint x,y;
        }
    }
}


struct Inst(InstType type){
    GenericInst inst;
    enum length = instSize!type;
    alias inst this;
}


auto _instSize(InstType type){
    switch(type){
        case InstType.Banychar:
            return InstType.sizeof;
        case InstType.Bchar: case InstType.Bnotchar:
            return InstType.sizeof+dchar.sizeof;
        case InstType.Brange: case InstType.Bnotrange:
            return InstType.sizeof+dchar.sizeof+dchar.sizeof;
        case InstType.Bsplit:
            return InstType.sizeof+uint.sizeof+uint.sizeof;
    }
}

template instSize(InstType t){
    enum opcodeSize = _instSize(t);
}


class RegexException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}