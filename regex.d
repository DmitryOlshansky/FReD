module regex;

import std.stdio;
import std.range, std.conv, std.exception, std.ctype, std.format;

enum:uint {
    IRchar        = 0,
    IRstring      = 1<<24,
    IRany         = 2<<24,
    IRcharset     = 3<<24,
    IRalter       = 4<<24,
    IRnm          = 5<<24,
    IRstar        = 6<<24,
    IRcross       = 7<<24,
    IRquest       = 8<<24,
    IRnmq         = 9<<24,
    IRstarq       = 10<<24,
    IRcrossq      = 11<<24,
    IRconcat      = 12<<24,

    IRdigit       = 13<<24,
    IRnotdigit    = 14<<24,
    //TODO: ...
    IRlambda      =  128<<24
};


struct RecursiveParser(R)
if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;
    uint[] ir;
    this(R pattern)
    {
        origin = pat = pattern;
        ir.reserve(pat.length);
        next();
        parseRegex();
    }
    void skipSpace()
    {
        while(next() && isspace(current)){ }
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
    void put(uint code){  ir ~= code; }
    void parseRegex()
    {
        while(!empty)
            switch(current)
            {
                case '|':
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
                    current == '?' ?  put(IRstarq) : put(IRstar);
                else
                    put(IRstar);
                break;
            case '?':
                next();
                put(IRquest);
                break;
            case '+':
                if(next())
                    current == '?' ? put(IRcrossq) : put(IRcross);
                else
                    put(IRcross);
                break;
            case '{':
                if(!next())
                    error("Unexpected end of regex pattern");
                try
                {
                    if(isdigit(current))
                        min = parse!uint(pat);
                    else 
                        min = 0;
                    skipSpace();
                    if(current == '}')
                        max = infinite;
                    else if(current == ',')
                    {
                        next();
                        max = parse!uint(pat);
                        skipSpace();
                        if(current == '}')
                            error("Unmatched '{' in regex pattern");
                    }
                    else
                        error("Unexpected symbol in regex pattern");
                    
                    if(next() && current == '?')
                        put(IRnmq);
                    else
                        put(IRnm);
                    put(min);
                    put(max);
                    
                }
                catch(ConvException ex)
                {
                    error("Failed to parse number in regex pattern");                       
                }
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
        case '*', '?', '+', '|':
            error("'*', '+', '?' not allowed in atom");
            break;
        case '.':
            put(IRany);
            next();
            break;
        case '(':
            R save = pat;
            next();
            parseRegex();
            if(current != ')')
            {
                pat = save;
                error("Unmatched '(' in regex pattern");
            }
            next();
        break;
        case '[':
            //range
            assert(0);
        break;
        case '\\':
            //escape
            assert(0);
        break;
        case ')':
        break;
        default:
            put(current);
            next();
        }
    }
    void error(string msg)
    {
        auto app = appender!string;
        formattedWrite(app,"%s\nPattern with error: `%s <--HERE-- s`",
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
                write("{%u,%u}",ir[i+1],ir[i+2]);
                i += 2;//2 extra words
                break;
            case IRnmq:
                write("{%u,%u}?",ir[i+1],ir[i+2]);
                i += 2;//ditto
                break;
            case IRalter:
                write('|');
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