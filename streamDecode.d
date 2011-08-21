/// Simple UTF-string stream abstraction with caching
/// able to decode and normalize on the fly, and trying to have as little
/// overhead (copying) as possible
module streamDecode;
import std.range:put;
import std.stdio, std.format;
import std.range;
import std.conv:to;
import std.math;
import std.exception:Exception,enforce;

/// core of the streaming/decoding/caching engine
enum QC { Yes, No, Maybe, Invalid };

/// quick check if ch is allowed in NFC
QC quickCheck(dchar ch)
{
    return QC.Yes;
}

immutable(string) decodeNextChar=`
        // decodes next char in newC and newPos, might exit if more chuncks are requested
        dchar newC=chunkAtt[chunkPos];
        ulong newPos=chunkPos+chunkStart;
        ++chunkPos;
        static if (is(Char==wchar)){
            if ((newC & ~0x7F)!=0){
                writefln("nextChar wchar decoding");
                if ((newC >= 0xD800 && newC <= 0xDBFF)){
                    if (chunkPos<chunkAtt.length){
                        dchar c2 = chunkAtt[chunkPos];
                        assert(c2 >= 0xDC00 && c2 <= 0xDFFF,"invalid surrogate low value");
                        newC = ((newC - 0xD7C0) << 10) + (c2 - 0xDC00); // could fuse constants...
                        ++chunkPos;
                    } else {
                        maybeCompleteBuf(decodedPos);
                        bufPushNonNormal(decodedChar,decodedPos);
                        decodedChar=newC;
                        decodedPos=newPos;
                        status=Status.BufCharPartial;
                        enforce(!hasEnd,"coding error");
                        return false;
                    }
                } else {
                    assert(newC < 0xDC00 || newC > 0xDFFF,"unexpected low surrogate");
                }
            }
        } else static if (is(Char==char)){
            if (newC & 0x80){ // needs more decoding
                writefln("nextChar char decoding");
                switch(newC>>4){ // here we could assert((newC&0x40)!=0), and use 0b11&(newC>>4) in the switch which is more compact, possibly genearting better switch table
                case 0b1101:
                    if (chunkPos<chunkAtt.length){
                        dchar c2=chunkAtt[chunkPos];
                        ++chunkPos;
                        assert((c2&0b1100_0000)==0x80,"encoding error");
                        newC=(newC<<7)+c2-0b0110_0000_1000_0000; // fused consts
                    } else {
                        maybeCompleteBuf(decodedPos);
                        bufPushNonNormal(decodedChar,decodedPos);
                        decodedChar=newC-0b1100_0000;
                        decodedPos=newPos;
                        status=Status.BufCharPartial;
                        enforce(!hasEnd,"encoding error");
                        return false;
                    }
                    break;
                case 0b1110:
                    assert((newC & 0b1_1000)!=0b1000,"encoding error");
                    if (chunkPos+1<chunkAtt.length){
                        dchar c2=chunkAtt[chunkPos];
                        assert((c2&0b1100_0000)==0x80,"encoding error");
                        dchar c3=chunkAtt[chunkPos+1];
                        assert((c3&0b1100_0000)==0x80,"encoding error");
                        chunkPos+=2;
                        newC=(newC<<14)+(c2<<7)+c3-0b0011_1000_0100_0000_1000_0000; // fused consts
                    } else {
                        maybeCompleteBuf(decodedPos);
                        bufPushNonNormal(decodedChar,decodedPos);
                        if (chunkPos<chunkAtt.length){
                            dchar c2=chunkAtt[chunkPos];
                            assert((c2&0b1100_0000)==0x80,"encoding error");
                            ++chunkPos;
                            decodedChar=(newC<<7)+c2-0b0111_0000_1000_0000; // fused consts
                        } else {
                            decodedChar=newC-0b1110_0000;
                        }
                        decodedPos=newPos;
                        status=Status.BufCharPartial;
                        enforce(!hasEnd,"encoding error");
                        return false;
                    }
                    break;
                case 0b1111:
                    assert((newC & 0b1100)!=0b0100,"encoding error");
                    if (chunkPos+2<chunkAtt.length){
                        dchar c2=chunkAtt[chunkPos];
                        assert((c2&0b1100_0000)==0x80,"encoding error");
                        dchar c3=chunkAtt[chunkPos+1];
                        assert((c3&0b1100_0000)==0x80,"encoding error");
                        dchar c4=chunkAtt[chunkPos+2];
                        assert((c4&0b1100_0000)==0x80,"encoding error");
                        chunkPos+=3;
                        newC=(newC<<21)+(c2<<14)+(c3<<7)+c4-0b0001_1110_0010_0000_0100_0000_1000_0000; // fused consts
                    } else {
                        maybeCompleteBuf(decodedPos);
                        bufPushNonNormal(decodedChar,decodedPos);
                        if (chunkPos+1<chunkAtt.length){
                            dchar c2=chunkAtt[chunkPos];
                            assert((c2&0b1100_0000)==0x80,"encoding error");
                            dchar c3=chunkAtt[chunkPos+1];
                            assert((c3&0b1100_0000)==0x80,"encoding error");
                            chunkPos+=2;
                            decodedChar=(newC<<14)+(c2<<7)+c3-0b0011_1100_0100_0000_1000_0000; // fused consts
                        } else if (chunkPos<chunkAtt.length){
                            dchar c2=chunkAtt[chunkPos];
                            assert((c2&0b1100_0000)==0x80,"encoding error");
                            ++chunkPos;
                            decodedChar=(newC<<7)+c2-0b0111_1000_1000_0000; // fused consts
                        } else {
                            decodedChar=newC-0b1111_0000;
                        }
                        decodedPos=newPos;
                        status=Status.BufCharPartial;
                        enforce(!hasEnd,"encoding error");
                        return false;
                    }
                    break;
                default:
                    assert(0,"encoding error");
                }
            }
        }
`;
/// Simple UTF-string stream abstraction with caching
struct StreamCBuf(Char)
    if(is(Char : dchar))//any char
{
    alias ulong DataIndex;
    alias const(Char)[] String;
    /// current chunk of the string
    String chunkAtt;
    /// position that will be read next in the chunk
    size_t chunkPos;
    /// circular buffer with history/decoded chars, its use is avoided as much as possible
    dchar[] bufAtt;
    /// circular buffer connected with bufAtt, giving the position of the char wrt. the original stream
    ulong[] indexes;
    /// maxium number of codepoints that should be read before normalizing, it was choosen so that
    /// from maxRead-1 (we fix the last char) the longest normalized sequence that we can generate
    /// is still smaller than 255 and we can safely index it
    enum int maxRead=86;
    /// amount of histor to keep (in non normalized codepoints)
    int historyWindow;
    /// position that will be read next in the buffer
    int bufPos;
    /// amount of stored history (going back from bufPos)
    int bufHistorySize;
    /// amount of buffer already normalized and still to use from bufPos on (but the last char is not necessarily normalized)
    int bufSize;
    /// size already read in the buffer, but not yed decoded, range start after bufPos+bufSize
    int bufReadSize;
    /// index of the first char of the current chunk
    ulong chunkStart;
    /// if the end of the current chunk is the end of the stream
    public bool hasEnd;
    /// * if status is DirectCharOne cached quickcheck=yes char that will be returned 
    ///   if the next char is also quickcheck=yes (the normal case).
    /// * if status is BufCharPartial it contains the partially decoded char
    /// * otherwise it is invalid
    dchar decodedChar; 
    ulong decodedPos; /// position of decodedChar if it is valid
    
    enum Status{
        /// initial status
        DirectCharNone,
        /// one char was quickcheck==Yes, and is stored in decodedChar (normal case)
        DirectCharOne,
        /// we should use the buffer eiter extracting already normalized chars, or filling it and normalizing chars
        BufChar,
        /// we should use the buffer and the last char is partially decoded and is stored in decodedChar
        BufCharPartial,
        /// we have no further chars left
        End,
    }
    
    Status status;
    
    /// constructor, charBuf and indexBuf need to have the same size, which must be a power of two
    this(dchar[]charBuf,ulong[]indexBuf,size_t historyWindow=2){// maybe pack them in single array of (dcahr,ulong) pairs ?? removes check, could improve cache locality
        indexes=indexBuf;
        bufAtt=charBuf;
        this.historyWindow=historyWindow;   
        assert(charBuf.length>=historyWindow+256,"historyWindow can be at most charBuf.length-256");
        assert(charBuf.length>255,"length has to be larger than 255");
        assert(charBuf.length==indexBuf.length,"length of buffers have to be equal");
        assert(((charBuf.length-1)&charBuf.length)==0,"the length has to be a power of two");
        indexes[0]=ulong.max;
    }
    
    /// prints a description of the internal state of the StreamCBuf
    void desc(Sink,Char=const(char))(Sink sink,bool detailed=false){
        formattedWrite(sink,"StreamCBuf{\n");
        size_t mPos=chunkPos;
        if (mPos>chunkAtt.length) {
            formattedWrite(sink," chunkPos=%d (>chunkAtt.length=%d),\n",chunkPos,chunkAtt.length);
            mPos=chunkAtt.length;
        }
        formattedWrite(sink," chunkAtt: %x '",chunkStart);
        put(sink,to!(Char[])(chunkAtt[0..mPos])); // this should be made utf safe
        formattedWrite(sink,"'^^^@%x+%d=%x^^^'",chunkStart,chunkPos,chunkStart+chunkPos);
        put(sink,to!(Char[])(chunkAtt[mPos..$])); // this should be made utf safe
        formattedWrite(sink,"' %x,\n",chunkStart+chunkAtt.length);
        formattedWrite(sink," historyWindow:%d,",historyWindow);
        mPos=bufPos;
        if (mPos>bufAtt.length){
            formattedWrite(sink," bufPos=%d (>bufAtt.length=%d),\n",bufPos,bufAtt.length);
            mPos=bufAtt.length;
        }
        if (detailed){
            formattedWrite(sink," bufAtt: ");
            foreach(i,c;bufAtt[0..mPos]){
                formattedWrite(sink,"%x<%s>",indexes[i],to!(Char[])([c]));
            }
            formattedWrite(sink,"^^^@%d^^^",bufPos);
            foreach(i,c;bufAtt[mPos..$]){
                formattedWrite(sink,"%x<%s>",indexes[i],to!(Char[])([c]));
            }
        }
        formattedWrite(sink," bufferedHistory:");
        for (size_t i=bufHistorySize;i!=0;--i){
            size_t ii=(bufPos-i+bufAtt.length)%bufAtt.length;
            formattedWrite(sink,"%x<%s>",indexes[ii],to!(Char[])(bufAtt[ii..ii+1]));
        }
        formattedWrite(sink,",\n normalizedBuf:");
        for (size_t i=0;i<bufSize;++i){
            size_t ii=(bufPos+i)%bufAtt.length;
            formattedWrite(sink,"%x<%s>",indexes[ii],to!(Char[])(bufAtt[ii..ii+1]));
        }
        formattedWrite(sink,",\n readBuf:");
        for (size_t i=0;i<bufReadSize;++i){
            size_t ii=(bufPos+bufSize+i)%bufAtt.length;
            formattedWrite(sink,"%x<%s>",indexes[ii],to!(Char[])(bufAtt[ii..ii+1]));
        }
        formattedWrite(sink,",\n");
        formattedWrite(sink," bufStart:%d,\n",bufStart());
        formattedWrite(sink," bufEnd:%d\n",bufEnd());
        formattedWrite(sink," nextPos:%d\n",nextPos());
        formattedWrite(sink," hasEnd:%d,\n",hasEnd);
        formattedWrite(sink," decodedChar:%s,\n",to!(Char[])([decodedChar]));
        formattedWrite(sink," decodedPos:%d,\n",decodedPos);
        formattedWrite(sink," status:%s,\n",to!string(status));
        formattedWrite(sink,"}\n");
    }
    /// ditto
    string toString(){
        char[] res;
        desc(delegate void(const(char[])s){ res~=s; });
        return res.idup;
    }
    
    /// adds a new chunck to evaluate
    void addChunk(String chunk, bool lastOne=false){
        assert(chunkPos==chunkAtt.length);
        chunkStart += chunkAtt.length;
        maybeCompleteBuf(chunkStart); // should have been done before. Actually assert that it was done to make sure that updating the chunk directly is ok???
        chunkAtt = chunk;
        chunkPos = 0;
        hasEnd = lastOne;
    }
    
    /// returns the next codepoint and position (start) if possible
    bool nextChar(ref dchar res,ref ulong pos){
        if (status==Status.DirectCharOne){ // normal case
            if (chunkAtt.length>chunkPos)
            {
                mixin(decodeNextChar); // expand if you need to debug...
                switch (quickCheck(newC)){
                case QC.Yes: // normal case
                    res=decodedChar;
                    pos=decodedPos;
                    decodedChar=newC;
                    decodedPos=newPos;
                    return true;
                case QC.No, QC.Maybe: // could try harder with maybe to quickly recover
                    maybeCompleteBuf(decodedPos);
                    bufPushNonNormal(decodedChar,decodedPos);
                    bufPushNonNormal(newC,newPos);
                    bufPushPosNext(chunkPos+chunkStart); // avoidable?
                    status=Status.BufChar;
                    break;
                default: 
                    assert(0);
                }
            }
            else
            {
                if (hasEnd) {
                    res=decodedChar;
                    pos=decodedPos;
                    status=Status.End;
                    return true;
                }
                maybeCompleteBuf(decodedPos);
                bufPushNonNormal(decodedChar,decodedPos);
                bufPushPosNext(chunkStart+chunkPos);
                status = Status.BufChar;
                return false; // we could automatically try to load more
            }
        }
        if ((status==Status.BufChar || status==Status.BufCharPartial)&&
            bufSize>0)
        {
            if(chunkPos == chunkAtt.length && !hasEnd)//empty chunk or trottling at end
                return false;
            res=bufAtt[bufPos];
            pos=indexes[bufPos];
            ++bufPos;
            if (bufPos>bufAtt.length){
                bufPos-=cast(int)bufAtt.length;
            }
            --bufSize;
            if (bufHistorySize<historyWindow) ++bufHistorySize;
            if (bufSize!=0){
                return true;
            } else if (quickCheck(res)==QC.Yes){ // could avoid if we cache the qc value (below in decoding)
                status=Status.DirectCharOne;
                decodedChar=res;
                decodedPos=pos;
                return nextChar(res,pos);
            }
            // we need to normalize this char (this happens only when we overflow)
            ++bufReadSize;
            --bufPos;
        }
        if (status==Status.BufCharPartial){
            static if (is(Char==wchar)){
                if (chunkPos<chunkAtt.length){
                    dchar c2 = chunkAtt[chunkPos];
                    assert(c2 >= 0xDC00 && c2 <= 0xDFFF,"invalid surrogate low value");
                    decodedChar = ((decodedChar - 0xD7C0) << 10) + (c2 - 0xDC00); // could fuse constants...
                    ++chunkPos;
                    bufPushNonNormal(decodedChar,decodedPos);
                    status=Status.BufChar;
                    return nextChar(res,pos);
                } else {
                    enforce(!hasEnd,"encoding error");
                    return false; // request more chuncks
                }
            } else static if (is(Char==char)){
                while (chunkPos<chunkAtt.length){ // we don't really check the length of the encoding, might generate invalid chars with invalid encodings
                    dchar c2=chunkAtt[chunkPos];
                    if ((c2&0b1100_0000)!=0b1000_0000) break;
                    decodedChar=(decodedChar<<7)+c2-0x80;
                    ++chunkPos;
                }
                if (chunkPos>=chunkAtt.length){
                    if (!hasEnd) return false; // request more chuncks
                }
                bufPushNonNormal(decodedChar,decodedPos);
                status=Status.BufChar;
                return nextChar(res,pos);
            } else {
                assert(0);
            }
        }
        if (status==Status.End) return false;
        assert (status==Status.BufChar || status==Status.DirectCharNone);
        // decode until quickCheck==yes
        QC qc=QC.Invalid;
        while (1){
            if (chunkAtt.length>chunkPos){
                mixin(decodeNextChar); // expand if you need to debug...
                qc=quickCheck(newC);
                if (Status.DirectCharNone || bufReadSize==0){
                    if (qc==QC.Yes){
                        decodedChar=newC;
                        decodedPos=newPos;
                        status=Status.DirectCharOne;
                        return nextChar(res,pos);
                    } else {
                        status=Status.BufChar;
                    }
                }
                bufPushNonNormal(newC,newPos);
                if (qc==QC.Yes || bufReadSize==maxRead) break;
            } else {
                if (hasEnd){
                    break;
                } else {
                    return false;
                }
            }
        }
        bufPushPosNext(chunkPos+chunkStart);
        // normalize the range bufPos..bufPos+bufReadSize-1 (bufSize==0)
        // and declare it as normalized updating bufSize
        //
        // we artificially *never* normalize the last char when the grapheme is longer than maxRead,
        // inserad we normalize maxRead-1 chunks keeping the maxRead fixed.
        // Note that the maxRead characterwill be normalized with the subsequent ones in his batch.
        // another choice would have made the references in the following groups not correctly handled
        // this still gives the correct results if the sequence was normalized, otherwise, well incorrect
        // normalization of graphemes larger than maxRead unnormalized codepoints is an error that we might accept...
        //
        // we could optimize if we detect that the normalization didn't change anything
        
        // fix indexes (if the normalization did change something)
        ulong idxBase=indexes[bufPos];
        assert((idxBase&(255UL<<48))==0);
        int mPos=bufPos;
        assert(bufSize==0);
        assert(bufReadSize>0);
        for (int i=0;i<bufReadSize-1;++i){
            indexes[mPos]=idxBase+1+((cast(ulong)i)<<48);
            ++mPos;
            if (mPos>indexes.length) mPos-=cast(int)indexes.length;
        }
        /+if (bufReadSize>1){
            // we "fix" its index to be the same of the following character (so that already from the index
            // on sees that normalization is required, and hasn't to check with quickcheck)
            indexes[bufPos]+=(1UL<<48)+1;
        }+/
        // even if qc!=Yes we keep the last char index as it is for the "next" decoding run
        return true;
    }
    /// return the start of the grapheme that contains this the char at this index (if decodeing is needed)
    ulong decodeStart(ulong i){
        return (i&~(255UL<<48))-(i>>48);
    }
    /// if we are at the end of the stream
    @property bool atEnd(){ return status==Status.End; }
    /// adds the position of the next char (i.e. buffer end)
    void bufPushPosNext(ulong pos){
        int mp=(bufPos+bufReadSize+bufSize)%bufAtt.length;
        indexes[mp]=pos;
    }
    /// adds a not yet normalized character c
    void bufPushNonNormal(dchar c,ulong pos){
        assert(bufReadSize<maxRead);
        assert(bufReadSize+bufSize+bufHistorySize<bufAtt.length);
        int mp=(bufPos+bufReadSize+bufSize)%bufAtt.length;
        bufAtt[mp]=c;
        indexes[mp]=pos;
        bufSize++;
    }
    /// the next position (mostly given for informative purposes, normally you should not use this)
    ulong nextPos(){
        switch (status){
        case Status.DirectCharOne:
            return decodedPos;
        case Status.DirectCharNone:
            return chunkStart+chunkPos;
        case Status.BufCharPartial:
            if (bufSize==0 && bufReadSize==0){
                return decodedPos;
            }
        case Status.BufChar:
            assert(bufSize!=0 || bufReadSize!=0);
            return indexes[bufPos];
        case Status.End:
            return chunkStart+chunkAtt.length;
        default:
            assert(0);
        }
    }
    /// first char in the history kept in the buffer
    ulong bufStart(){
        if (bufHistorySize==0){
            if (bufSize>0)
                return indexes[bufPos];
            else
                return chunkStart+chunkPos;
        }
        return indexes[(bufPos-bufHistorySize+indexes.length)%indexes.length];
    }
    /// last char read in the buffer (not necessarily decoded)
    /// returns a normal index (no high bits), or ulong.max if nothing was read
    ///
    /// assumes indexes is initialized at the beginning, so that this will return ulong.max
    ulong bufEnd(){
        ulong res=indexes[(bufPos+bufSize+bufReadSize)%indexes.length];
        /// this value should *never* have high bits set
        assert((res & (255UL<<48))==0 || res == ulong.max);
        return indexes[(bufPos+bufSize+bufReadSize)%indexes.length];
    }
    /// possibly completes the buffer checking the history window
    /// always moves the current position to the end of the read region
    void maybeCompleteBuf(ulong upTo){
        assert((upTo  & (255UL<<48))==0); // check what happens if we start with a 0 length chunk...
        assert(bufReadSize==0);
        // calculate where we must start keeping history for the switch
        ulong finalHistoryWindowStart=chunkStart+chunkAtt.length;
        if (!hasEnd){
            if (historyWindow>finalHistoryWindowStart)
                finalHistoryWindowStart=0;
            else
                finalHistoryWindowStart-=historyWindow+1;
        }
        ulong bEnd=bufEnd();
        assert(((bufAtt.length-1)&bufAtt.length)==0, "bufAtt.length must be a power of two");
        if (bEnd==ulong.max){ // start up
            if (finalHistoryWindowStart<upTo){
                assert(chunkStart<=finalHistoryWindowStart);
                size_t start=cast(size_t)(finalHistoryWindowStart-chunkStart);
                // with char/wchar we might need to adjust start
                size_t end=cast(size_t)(upTo-chunkStart);
                assert(end<=chunkAtt.length);
                size_t i=start;
                while (i<end){
                    dchar newC=cast(dchar)chunkAtt[i];
                    indexes[bufPos]=chunkStart+i;
                    ++i;
                    // with char/wchar we should maybe decode more chars into newC...
                    bufAtt[bufPos]=newC;
                    ++bufPos;
                    if (bufHistorySize<historyWindow) ++bufHistorySize;
                    if (bufPos>=bufAtt.length) bufPos-=bufAtt.length;
                }
                bufPushPosNext(chunkStart+upTo);
            }
        } else {
            if (bEnd+historyWindow>upTo) { // avoid holes
                finalHistoryWindowStart=bEnd;
            }
            if (finalHistoryWindowStart<upTo){
                assert(chunkStart<=finalHistoryWindowStart);
                ulong start= finalHistoryWindowStart-chunkStart;
                // with char/wchar we might need to adjust start
                size_t end=cast(size_t)(upTo-chunkStart);
                assert(end<=chunkAtt.length);
                size_t i = cast(size_t)start;
                while (i<end){
                    dchar newC=cast(dchar)chunkAtt[i];
                    indexes[bufPos]=chunkStart+i;
                    ++i;
                    // with char/wchar we should maybe decode more chars into newC...
                    bufAtt[bufPos]=newC;
                    ++bufPos;
                    if (bufHistorySize<historyWindow) ++bufHistorySize;
                    if (bufPos>=bufAtt.length) bufPos-=bufAtt.length;
                }
                bufPushPosNext(chunkStart+upTo);
            } else if (bEnd+historyWindow<upTo){
                // clear buf
                bufHistorySize=0;
            }
        }
    }
    /// an iterator that goes back in history.
    /// is invalidated by nextPos or addChunk
    static struct BackLooper{
        alias ulong DataIndex;
        StreamCBuf *streamBuf;
        ulong bound;
        ulong pos;
        enum Access{
            PreBuf,
            InBuf,
            PostBuf,
        }
        Access status;
        
        /// returns the next char going back from the current position
        bool nextChar(ref dchar res,ref ulong rpos)
        {
            // decoding as function, not mixin as less critical then forward step
            // returns false if decoding fails
            bool decodeNextChar(ref dchar newC){
                --pos;
                newC=streamBuf.chunkAtt[cast(size_t)(pos - streamBuf.chunkStart)];
                static if (is(Char==wchar)){
                    if ((newC & ~0x7F)!=0){
                        if (newC >= 0xDC00 && newC <= 0xDFFF){
                            if (pos<=bound){
                                // ignoring partially encoded char
                                return false;
                            }
                            --pos;
                            dchar c1=streamBuf.chunkAtt[cast(size_t)(pos - streamBuf.chunkStart)];
                            assert(c1 >= 0xD800 && c1 <= 0xDBFF,"invalid encoding");
                            newC = ((c1 - 0xD7C0) << 10) + (newC - 0xDC00); // could fuse constants...
                        } else {
                            assert(newC < 0xD800 || newC > 0xDBFF,"encoding error (high surrogate)");
                        }
                    }
                } else static if (is(Char==char)){
                    if ((newC & 0x80)!=0){
                        assert((newC & 0b1100_0000)==0b1000_0000,"encoding error");
                        int shift=0;
                        newC &= ~0b1000_0000;
                        for(;;){
                            if (pos<=bound) {
                                // ignoring partially encoded char
                                return false;
                            }
                            --pos;
                            dchar c2=streamBuf.chunkAtt[cast(size_t)(pos - streamBuf.chunkStart)];
                            if ((c2 & 0b1100_0000)!=0b1000_0000){
                                switch(c2>>4){
                                case 0b1101:
                                    assert(shift==7);
                                    newC|=(c2&~0b1100_000)<<7;
                                    break;
                                case 0b1110:
                                    assert((c2&0b1000)!=0,"encoding error");
                                    assert(shift==14);
                                    newC|=(c2&~0b1100_000)<<14;
                                    break;
                                case 0b1111:
                                    assert((c2&0b0100)!=0,"encoding error");
                                    assert(shift==21);
                                    newC|=(c2&~0b1100_000)<<21;
                                    break;
                                default:
                                    assert(0,"encoding error");
                                }
                            } else {
                                newC|=(c2 & ~0b1000_0000)<<shift;
                                shift+=7;
                            }
                        }
                    }
                }
                return true;
            }
            dchar newC;
            switch (status)
            {
            case Access.InBuf:
                if(pos <= bound)
                {
                    pos = streamBuf.bufStart;
                    bound = streamBuf.chunkStart;
                    if (streamBuf.nextPos>streamBuf.historyWindow+bound) {
                        bound=streamBuf.nextPos-streamBuf.historyWindow;
                    }
                    status = Access.PostBuf;
                    if(pos < bound)
                        return false;
                    else
                        return nextChar(res, rpos);
                }
                --pos;
                rpos=streamBuf.indexes[cast(size_t)pos];
                res=streamBuf.bufAtt[cast(size_t)pos];
                return true;
            case Access.PreBuf: 
                if(pos <= bound)
                {
                    if (bound == streamBuf.bufEnd){
                        status = Access.InBuf;
                        pos = streamBuf.bufPos;
                        bound = streamBuf.bufPos - streamBuf.bufHistorySize;
                        return nextChar(res, rpos);
                    } else {
                        return false;
                    }
                }
                if (!decodeNextChar(newC)) return false;
                res=newC;
                rpos=pos;
                return true;
            case Access.PostBuf:
                if(pos <= bound)
                    return false;
                if (!decodeNextChar(newC)) return false;
                res=newC;
                rpos=pos;
                return true;
            default:
                assert(0);
            }
        }
        
        this(ref StreamCBuf streamBuf)
        {
            this.streamBuf = &streamBuf;
            ulong bStart = streamBuf.bufStart();
            ulong bEnd = streamBuf.bufEnd();
            //get absolute position in stream
            pos = streamBuf.atEnd ?  streamBuf.chunkStart+streamBuf.chunkAtt.length : streamBuf.decodedPos;
            //now set proper status, adjust pos if in buffer
            if(bEnd == ulong.max || streamBuf.decodedPos < bStart){
                status = Access.PostBuf;
                bound = streamBuf.chunkStart;
            }
            else if(streamBuf.decodedPos > bEnd)
            {
                status = Access.PreBuf;
                bound = streamBuf.indexes[cast(size_t)(bEnd)];
            }
            else if(streamBuf.decodedPos >= bStart)
            {
                pos = streamBuf.bufPos;
                status = Access.InBuf;
                bound = streamBuf.bufPos - streamBuf.bufHistorySize;
            }
            else
                assert(0);
        }
        
    }
    
    BackLooper loopBack(){
        return BackLooper(this);
    }
    
    const(Char)[] opIndex()(ulong from,ulong to,char[] buf=null){
        
    }
    const(Char)[] opIndex(G)(G g,char[] buf=null){
        return opIndex(g.start,g.end,buf);
    }
}


unittest
{
    import std.stdio;
    alias char Char;
    dchar[] charBuf = new dchar[1024];
    ulong[] indexes = new ulong[1024];
    auto stream = StreamCBuf!Char(charBuf, indexes, 8);
    dchar ch,ch2;
    ulong index,index2;
    alias immutable(Char)[] String;
    String fullStr="Hello,another chunk";
    String hello = fullStr[0..6];
    String another = fullStr[6..$];
    String emptyStr="";
    size_t ii=0;
    auto chunks = [emptyStr,hello,emptyStr,another,emptyStr];
    foreach(ichunk,chunk;chunks){
        debug(StreamTest){
            writefln("pippo pre addChunk(%s)",chunk);
            stream.desc(delegate void(const(char[])s){ writef(s); });
        }
        stream.addChunk(chunk);
        debug(StreamTest){
            writefln("pippo addChunk(%s)",chunk);
            stream.desc(delegate void(const(char[])s){ writef(s); });
        }
        size_t i=ii;
        while(stream.nextChar(ch, index))
        {
            debug(StreamTest){
                writefln("stream.nextChar('%s',%s)",ch,index);
            }
            assert(ch == fullStr[i]);
            assert(index == i);
            i++;
            auto j=i;
            debug(StreamTest){
                writefln("pre Loopback");
                stream.desc(delegate void(const(char[])s){ writef(s); });
            }
            auto firstPass = stream.loopBack();
            while(firstPass.nextChar(ch2, index2))
            {
                debug(StreamTest){
                    writefln("loopBack, char:%s, index:%s",ch2,index2);
                }
                j--;
                assert(ch2 == fullStr[j]);
                assert(j == index2);
            }
            assert(j<=((i>stream.historyWindow)?(i-stream.historyWindow):0));
        }
        debug(StreamTest){
            writefln("--- post stream.nextChar()");
            stream.desc(delegate void(const(char[])s){ writef(s); });
            writefln("i:%s ii:%s chunk.length:%s",i,ii,chunk.length);
        }
        assert(((ichunk==1)?(i == ii+chunk.length-1):(i ==  ii+chunk.length)));// OK, last one waits possible normalization
        ii=i;
    }
    stream.addChunk(emptyStr, true);
    assert(index == fullStr.length - 2); //one char awaits normalization
    assert(ii == fullStr.length - 1); //note the difference
    {
        size_t i=ii;
        while(stream.nextChar(ch, index))
        {
            //writeln(ch, " at ", index);
            assert(ch == fullStr[i]);
            assert(index == i);
            i++;
            auto j=i;
            auto firstPass = stream.loopBack();
            while(firstPass.nextChar(ch2, index2))
            {
                debug(StreamTest){
                    writefln("--- inside last loopback");
                    stream.desc(delegate void(const(char[])s){ writef(s); });
                    writefln("last loopBack, char:%s, index:%s",ch2,index2);
                }
                j--;
                assert(ch2 == fullStr[j]);
                assert(j == index2);
            }
            assert(j<=((i>stream.historyWindow)?(i-stream.historyWindow):0));
        }
        ii=i;
    }
    assert(index == fullStr.length-1); 
    assert(ii == fullStr.length); 
}

dstring fromStream(S)(S stream)
{
    auto a = appender!(dstring)();
    dchar ch;
    ulong i;
    while(stream.nextChar(ch, i))
        a.put(ch);
    return a.data;
}


unittest// a very simple loopBack use (by one char)
{
    import std.stdio;
    dchar[] charBuf = new dchar[512];
    ulong[] indexes = new ulong[512];
    dchar ch;
    ulong index;
    auto s2 = StreamCBuf!dchar(charBuf, indexes);
    dstring chunk = "test history";
    s2.addChunk(chunk);
    s2.nextChar(ch, index);
    //writeln(ch, " ", index);
    auto back2 = s2.loopBack();
    dchar ch2;
    ulong index2;
    back2.nextChar(ch2, index2);
    //writeln(ch2, " ", index2);
    assert(ch == ch2);
    assert(index == index2);
}


version(StreamTest)
{
    void main(){}
}