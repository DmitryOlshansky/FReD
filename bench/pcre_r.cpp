#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#define PCRE_STATIC 1
#include "pcrecpp.h"
using namespace std;

int main(int argc,char* argv[]){
	if(argc < 3){
		printf("Usage: test_pcre <re> <file> [print]>\n");
		return 1;
	}
	pcrecpp::RE engine(argv[1]);
	FILE* f = fopen(argv[2],"rb");
	if(!f){
		printf("Cannot read file %s\n",argv[2]);
		return 2;
	}
	fseek(f,0,SEEK_END);
	auto size = ftell(f);
	fseek(f,0,SEEK_SET);
	char* data = new char[size+1];
	data[size] = 0;
	fread(data,1,size,f);
	fclose(f);
	pcrecpp::StringPiece input(data);
	string str;
	size_t count=0;
	LARGE_INTEGER start, end, resolution;
	QueryPerformanceCounter(&start);
	if(argc == 4 && strcmp(argv[3],"print") == 0)
		while(engine.FindAndConsume(&input,&str)){
			printf("%s\n",str.c_str());
			count++;
		}	
	else
		while(engine.FindAndConsume(&input)){
			count++;
		}	
	QueryPerformanceCounter(&end);
	QueryPerformanceFrequency(&resolution);
	double time = ((end.QuadPart - start.QuadPart)/(double)resolution.QuadPart);
	delete[] data;
	printf("\n\nTotal matches %d\nTime elapsed %.2lf sec\n",count,time);
	return 0;
}