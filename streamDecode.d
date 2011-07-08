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
    /// maxium number of codepoints that should be read before normalizing (fix to 255?)
    int maxRead;
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
        maybeCompleteBuf(chunkStart);
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
            // we need to normalize this char (the normalized will have to decide what to do when the first char is not stable like this, this can happen only if )
            ++bufReadSize;
            --bufPos;
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
        // normalize the range bufPos..bufPos+bufReadSize
        // and declare it as normalized updating bufSize
        
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
        if (qc!=QuickCheck.Yes){
            int i=bufSize;
            indexes[mPos]=idxBase+1+((cast(ulong)i)<<48);
            ++mPos;
            if (mPos>indexes.length) mPos-=cast(int)indexes.length;
        }
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
    }
    ulong bufStart(){
        if (bufHistorySize==0){
            if (bufSize>0)
                return indexes[bufPos];
            else
                return chunkStart+chunkPos;
        }
        return indexes[(bufPos-bufHistorySize)%indexes.length];
    }
    ulong bufEnd(){
        // assumes indexes is initialized at the beginning, add an explicit end
        return indexes[(bufPos+bufSize+bufReadSize)%indexes.length];
    }
    /// possibly completes the buffer checking the history window
    bool maybeCompleteBuf(ulong upTo){
        minHistoryWindow=chunkStart+chunk.length-historyWindow;
        
        // to finish
    }
    
    bool lookback(int i,ref dchar,ref ulong pos){
        // to do
    }
}
