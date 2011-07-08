/// core of the streaming/decoding/caching engine


/// Simple UTF-string stream abstraction with caching
struct StreamCBuf(Char)
    if(is(Char == char) || is(Char == wchar))
{
    alias const(Char)[] String;
    String chunkAtt;
    size_t chunkPos;
    dchar[] bufAtt;
    ulong[] indexes;
    int maxRead; // maxium number of codepoints that should be read before normalizing
    int historyWindow; // amount of histor to keep (in non normalized codepoints)
    int bufHistorySize;
    int bufPos;
    int bufSize;
    int bufReadSize;
    ulong chunkStart;
    bool hasEnd; // if the end of the current chunk is the end of the file
    dchar decodedChar;
    ulong decodedPos;
    
    enum Status{
        DirectCharNone,
        DirectCharOne,
        BufChar,
        BufCharPartial,
        End, // not really needed
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
        long idxBase=indexes[bufPos];
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
    
    ulong nextPos(){
        switch (status){
        case Status.decodedPos
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
