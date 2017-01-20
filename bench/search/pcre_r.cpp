#include <stdio.h>
#include <stdlib.h>
#include <pcre.h>

#ifdef WIN32
  #include <windows.h>
#else
  #include <sys/time.h>
#endif

#ifdef JIT
  #define USE_JIT PCRE_STUDY_JIT_COMPILE
#else
  #define USE_JIT 0
#endif


int main(int argc,char* argv[]){
    if(argc < 3){
      printf("Usage: test_pcre <re> <file> [print]>\n");
      return 1;
    }
    pcre *re;
    pcre_extra *re_ex;
    const char *re_e;
    int re_eo;
    if (!(re = pcre_compile(argv[1], 0/*PCRE_UTF*/, &re_e, &re_eo, NULL)))
        exit(1);
    re_ex = pcre_study(re, USE_JIT, &re_e);
    if(re_e)
        printf("%s\n", re_e);
    FILE* f = fopen(argv[2],"rb");
    if(!f){
      printf("Cannot read file %s\n",argv[2]);
      return 2;
    }
    fseek(f,0,SEEK_END);
    size_t size = ftell(f);
    fseek(f,0,SEEK_SET);
    char* data = new char[size+1];
    data[size] = 0;
    fread(data,1,size,f);
    fclose(f);
    size_t count=0;
#ifdef WIN32
    LARGE_INTEGER start, end, resolution;
    QueryPerformanceCounter(&start);
#else
    timeval start, end;
    gettimeofday(&start,NULL);
#endif
    int m[24];
    int pos = 0;
    while (pcre_exec(re, re_ex, data, size, pos, 0, m, 24) >= 0){
        count++;
        pos = m[1];
    }
    #ifdef WIN32
      QueryPerformanceCounter(&end);
      QueryPerformanceFrequency(&resolution);
      double time = ((end.QuadPart - start.QuadPart)/(double)resolution.QuadPart);
    #else
      gettimeofday(&end, NULL);
      double time = (end.tv_sec - start.tv_sec) + (1e-6*end.tv_usec - 1e-6*start.tv_usec);
    #endif
    delete[] data;
    printf("\n\nTotal matches %d\nTime elapsed %.2lf sec\n", (int)count, time);
    return 0;
}
