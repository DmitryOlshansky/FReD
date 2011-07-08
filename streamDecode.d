/// core of the streaming/decoding/caching engine


/// Simple UTF-string stream abstraction with caching
struct StreamCBuf(Char)
    if(is(Char == char) || is(Char == wchar))
{
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
    bool hasEnd;
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
    
    /// adds a new chunck to evaluate
    void addChunk(String chunk){
        assert(chunkPos==chunkAtt.length);
        chunkStart+=chunkAtt.length;
        maybeCompleteBuf(chunkStart); // should have been done before. Actually assert that it was done to make sure that updating the chunk directly is ok???
        chunkAtt=chunk;
    }
    
    /// returns the next codepoint and position (start) if possible
    bool nextChar(ref dchar res,ref long pos){
        if (status==Status.DirectCharOne){ // normal case
            if (chunkAtt.length>chunkPos){
                dchar newC=chunkAtt[chunkPos];
                ulong newPos=chunkPos+chunkStart;
                ++chunkPos;
                static if (!is(Char==dchar)){
                    // maybe decode more, or create a partial state, guarantee history and return false
                }
                switch (quickCheck(newC)){
                case QC.Yes: // normal case
                    res=decodedChar;
                    pos=decodedPos;
                    decodedChar=newC;
                    decodedPos=newPos;
                    return true;
                case QC.No,QC.Maybe: // could try harder with maybe to quickly recover
                    maybeCompleteBuf(decodedPos);
                    bufPushNonNormal(decodedChar,decodedPos);
                    bufPushNonNormal(newChar,newPos);
                    status=Status.BufChar;
                    break;
                default: assert(0);
                }
            } else {
                if (hasEnd) {
                    res=decodedChar;
                    pos=decodedPos;
                    status=Status.End;
                    return true;
                }
                maybeCompleteBuf(decodedChar);
                bufPushNonNormal(decodedChar,decodedPos);
                status=BufChar;
                return false; // we could automatically try to load more
            }
        }
        bool didOverflow=false; // if we had to normalize more than maxRead
        if ((status==Status.BufChar || status==Status.BufCharPartial)&&
            bufSize>0)
        {
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
            } else if (quickCheck(res)==QuickCheck.Yes){ // could avoid if we cache the qc value (below in decoding)
                status=Status.DirectCharOne;
                decodedChar=res;
                decodedPos=pos;
                return nextChar(res,pos);
            }
            // we need to normalize this char (this happens only when we overflow)
            ++bufReadSize;
            --bufPos;
            didOverflow=true;
        }
        if (status==Status.BufCharPartial){
            static if (!is(Char==dchar)){
                while (chunkAtt.length>chunkPos){
                    dchar newC=chunkAtt[chunkPos];
                    ++chunkPos;
                    // combine with decodedChar
                    // if not combinable --chunkPos;
                    // if did combine
                    bufPushNonNormal(decodedChar,decodedPos);
                    status=Status.BufChar;
                    return nextChar(res,pos);
                }
                if (hasEnd){
                    res=decodedChar;
                    pos=decodedPos;
                    status=Status.End;
                    return true;
                } else {
                    return false;
                }
            } else {
                assert(0);
            }
        }
        if (status==Status.End) return false;
        assert (status==Status.BufChar || status==Status.CharNone);
        // decode until quickCheck==yes
        QuickCheck qc=QuickCheck.Invalid;
        while (1){
            if (chunkAtt.length>chunkPos){
                dchar newC=chunkAtt[chunkPos];
                ulong newPos=chunkPos+chunkStart; // store???
                ++chunkPos;
                static if (!is(Char==dchar)){
                    // maybe decode more, or create a partial state, guarantee history and return false
                }
                qc=quickCheck(newC);
                if (Status.DirectCharNone || bufReadSize==0){
                    if (qc==QuickCheck.Yes){
                        decodedChar=newC;
                        decodedPos=newPos;
                        status=Status.DirectCharOne;
                        return nextChar(res,pos);
                    } else {
                        status=Status.BufChar;
                    }
                }
                bufPushNonNormal(newC,newPos);
                if (qc==QuickCheck.Yes || bufReadSize==maxRead) break;
            } else {
                if (hasEnd){
                    break;
                } else {
                    return false;
                }
            }
        }
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
        assert(idxBase&(255UL<<48))==0);
        int mPos=bufPos;
        assert(bufSize>0)
        for (i=0;i<bufSize-1;++i){
            indexes[mPos]=idxBase+1+((cast(ulong)i)<<48);
            ++mPos;
            if (mPos>indexes.length) mPos-=cast(int)indexes.length;
        }
        if (didOverflow){
            // we "fix" its index to be the same of the following character (so that already from the index
            // on sees that normalization is required, and hasn't to check with quickcheck)
            indexes[bufPos]+=(1UL<<48)+1;
        }
        // even if qc!=Yes we keep the last char index as it is for the "next" decoding run
    }
    /// return the start of the grapheme that contains this the char at this index (if decodeing is needed)
    ulong decodeStart(ulong i){
        return (i&~(255UL<<48))-(i>>48);
    }
    /// if we are at the end of the stream
    bool atEnd(){
        return status==Status.End;
    }
    /// adds a not yet normalized character c
    void bufPushNonNormal(dchar c,ulong pos){
        assert(bufReadSize<maxRead);
        assert(bufReadSize+bufSize+bufHistorySize<bufAtt.length)
        int mp=(bufPos+bufReadSize+bufSize)%bufAtt.length;
        bufAtt[mp]=c;
        indexes[mp]=pos;
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
            return ulong.max;
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
        return indexes[(bufPos-bufHistorySize)%indexes.length];
    }
    /// last char read in the buffer (not necessarily decoded)
    /// returns a normal index (no high bits), or ulong.max if nothing was read
    ///
    /// assumes indexes is initialized at the beginning, so that this will return ulong.max
    ulong bufEnd(){
        ulong res=indexes[(bufPos+bufSize+bufReadSize)%indexes.length];
        /// this value should *never* have high bits set
        assert((res & (255UL<<48))==0 || res=ulong.max);
        return indexes[(bufPos+bufSize+bufReadSize)%indexes.length];
    }
    /// possibly completes the buffer checking the history window
    /// always moves the current position to the end of the read region
    bool maybeCompleteBuf(ulong upTo){
        assert((upTo  & (255UL<<48))==0); // check what happens if we start with a 0 length chunk...
        assert(bufReadSize==0);
        ulong finalHistoryWindowStart=chunkStart+chunk.length;
        if (!hasEnd){
            if (historyWindow>finalHistoryWindowStart)
                finalHistoryWindowStart=0;
            else
                finalHistoryWindowStart-=historyWindow;
        }
        ulong bEnd=bufEnd();
        assert(((bufAtt.lenth-1)&bufAtt.lenth)==0,"bufAtt.lenth must be a power of two");
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
            }
        } else {
            if (bEnd+historyWindow>upTo) { // avoid holes
                finalHistoryWindowStart=bEnd;
            }
            if (finalHistoryWindowStart<upTo){
                assert(chunkStart<=finalHistoryWindowStart);
                size_t start=cast(size_t)(finalHistoryWindowStart-chunkStart);
                bool skipFirst=false;
                if (start<bEnd) {
                    start=bEnd;
                    skipFirst=true;
                }
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
                    if (skipFirst) {
                        skipFirst=false;
                        continue;
                    }
                    ++bufPos;
                    if (bufHistorySize<historyWindow) ++bufHistorySize;
                    if (bufPos>=bufAtt.length) bufPos-=bufAtt.length;
                }
            }
        }
    }
    /// an iterator that goes back in history.
    /// is invalidated by nextPos or addChunk
    static struct BackLooper{
        StreamCBuf streamBuf;
        ulong nextPos;
        ulong bufEnd;
        ulong posAtt;
        int iPos;
        Status 
        
        /// returns the next char going back from 
        bool nextChar(ref dchar,ref ulong pos){
            if (i>historyWindow || i>=) return false;
            // to do
        }
        /// looks back at the previous character (inefficient, probably we should give back an iterator that does it)
        
    }
    
    BackLooper loopBack(){
        BackLooper res;
        res.streamBuf=this;
        res.nextP=nextPos();
        res.bufEnd=bufEnd();
        res.posAtt=0
        return res;
    }
}
