#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <regex>
using namespace std;
int main(int argc,char* argv[]){
	if(argc < 3){
		printf("Usage: test_pcre <re> <file> [print]>\n");
		return 1;
	}
	regex engine(argv[1]);
	cmatch match;
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
	
	
	size_t count=0;
	LARGE_INTEGER start, end, resolution;
	QueryPerformanceCounter(&start);
	const char* ptr = data;
	cregex_iterator m1(ptr,ptr+size,engine), m2;
	if(argc == 4 && strcmp(argv[3],"print") == 0)
		for(;m1!=m2;++m1){
			printf("%s\n",m1->str().c_str());
			count++;
		}	
	else
		for(;m1!=m2;++m1){
			count++;
		}	
	QueryPerformanceCounter(&end);
	QueryPerformanceFrequency(&resolution);
	double time = ((end.QuadPart - start.QuadPart)/(double)resolution.QuadPart);
	delete[] data;
	printf("\n\nTotal matches %d\nTime elapsed %.2lf sec\n",count,time);
	return 0;
}