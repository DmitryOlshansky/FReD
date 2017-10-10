#include <stdio.h>
#include <stdlib.h>
#ifdef WIN32
	#include <windows.h>
#else
	#include <sys/time.h>
#endif
#include <re2/re2.h>
#include <string>


int main(int argc,char* argv[]){
	if(argc < 3){
		printf("Usage: test_re2 <re> <file> [print]>\n");
		return 1;
	}
	RE2 engine(argv[1]);
	if(!engine.ok())
	{
		printf("Failed to compile\n");
		return 2;
	}
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
	re2::StringPiece input(data);
	std::string str;
	if(argc == 4 && strcmp(argv[3],"print") == 0)
		while(engine.FindAndConsume(&input, engine, &str)){
			printf("%s\n",str.c_str());
			count++;
		}	
	else
		while(engine.FindAndConsume(&input, engine, &str)){
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
	printf("\n\nTotal matches %d\nTime elapsed %.2lf sec\n", (int)count, time);
	return 0;
}
