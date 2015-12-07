#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <openssl/md5.h>

/*
 * Do this one in C for speed...
 *
 * compile with:
 * gcc day04.c -o day04 -lcrypto -lssl
 */
#define BUFSZ 100

int starts_with_five_zeroes(unsigned char* string) 
{
  return (string[0] == 0 &&
	  string[1] == 0 &&
	  (string[2] & 0xF0) == 0);
}

int starts_with_six_zeroes(unsigned char* string) 
{
  return (string[0] == 0 &&
	  string[1] == 0 &&
	  string[2] == 0);
}

void print_md5(unsigned char* input, unsigned char* output) 
{
  int i;
  printf("MD5(%s) = ", input);
  for(i=0; i<MD5_DIGEST_LENGTH; i++) {
    printf("%02x", output[i]);
  }
  printf("\n");  
}

    
int main(int argc, char **argv) 
{
  if(argc != 2) {
    printf("Computes the md5-hash of the prefix followed by a number formatted as\n");
    printf("an ascii-string. Will stop when it finds a hash starting with 00000.\n");
    printf("Usage: %s <prefix>\n", argv[0]);
    return 1;
  }

  char* prefix = argv[1];
  int prefixlen = strlen(prefix);
  
  if(prefixlen > (BUFSZ-21)) {
    printf("Prefix too long\n");
    return 2;
  }
  
  long i=0;
  unsigned char result[MD5_DIGEST_LENGTH];
  unsigned char string[BUFSZ];
  strcpy(string, prefix);
  unsigned char* write_num_at = string+prefixlen;
  int inputlen = 0;
  int done5 = 0;
  int done6 = 0;
  
  struct timespec start;
  struct timespec stop;

  clock_gettime(CLOCK_MONOTONIC, &start);
  
  do {
    i++;
    inputlen = sprintf(write_num_at, "%ld", i);
    inputlen += prefixlen;
    MD5(string, inputlen, result);
    if((done5 == 0) && starts_with_five_zeroes(result)) {
      printf("part 1: ");
      print_md5(string, result);
      done5 = 1;
    } else if((done6 == 0) && starts_with_six_zeroes(result)) {
      printf("part 2: ");
      print_md5(string, result);
      done6 = 1;
    }
  } while((done5 == 0) || (done6 == 0));

  clock_gettime(CLOCK_MONOTONIC, &stop);

  long seconds  = stop.tv_sec - start.tv_sec;
  long millisec = (stop.tv_nsec- start.tv_nsec)/1000000;
  if(millisec < 0) {
    millisec += 1000;
    seconds -= 1;
  }

  
  printf("time used: %ld seconds, %ld milliseconds\n", seconds, millisec);
  return 0;
}
