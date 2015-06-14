#include <stdio.h>
#include <stdlib.h>
#ifdef WIN32
	#include <windows.h>
#else
	#include <sys/time.h>
#endif
#include <boost/regex.hpp>

using namespace boost;

int main(int argc,char* argv[]){
	if(argc < 3){
		printf("Usage: test_boost <re> <file> [print]>\n");
		return 1;
	}
	regex engine = boost::regex(argv[1]);
	cmatch match;
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
