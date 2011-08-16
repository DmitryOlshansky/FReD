#include <stdio.h>
#include <stdlib.h>
#define PCRE_STATIC 1
#include <pcrecpp.h>
#ifdef WIN32
	#include <windows.h>
#else
	#include <sys/time.h>
#endif

using namespace std;

int main(int argc,char* argv[]){
	if(argc < 3){
		printf("Usage: test_pcre <re> <file> [print]>\n");
		return 1;
	}
	pcrecpp::RE engine(argv[1], pcrecpp::UTF8());
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
	pcrecpp::StringPiece input(data);
	string str;
	size_t count=0;
	#ifdef WIN32
		LARGE_INTEGER start, end, resolution;
		QueryPerformanceCounter(&start);
	#else
		timeval start, end;
		gettimeofday(&start,NULL);
	#endif
	if(argc == 4 && strcmp(argv[3],"print") == 0)
		while(engine.FindAndConsume(&input,&str)){
			printf("%s\n",str.c_str());
			count++;
		}	
	else
		while(engine.FindAndConsume(&input)){
			count++;
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
	printf("\n\nTotal matches %d\nTime elapsed %.2lf sec\n",count,time);
	return 0;
}

	
