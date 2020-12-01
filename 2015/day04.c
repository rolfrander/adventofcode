#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>
#include <openssl/md5.h>

/*
 * Do this one in C for speed...
 *
 * compile with:
 * gcc day04.c -o day04 -lcrypto -lssl
 */
const int BUFSZ = 100;

int starts_with_zeroes(unsigned char* string, int zcnt)
{
  int i;
  for(i=0; i<(zcnt >> 1); i++) {
    if(string[i] != 0) {
      return false;
    }
  }
  
  if((zcnt & 1) == 1) {
    /* is odd: look at the first nibble of the next byte */
    return (string[(zcnt >> 1)] & 0xf0) == 0;
  }
  return true;
  
}

int number_of_leading_zeroes(unsigned char* data) 
{
  int i=0;
  while(data[i] == 0) {
    i++;
  }
  
  if((data[i] & 0xF0) != 0) {
    return i*2;
  }
  if((data[i] & 0x0F) != 0) {
    return i*2+1;
  }
}

void print_md5(float time, unsigned char* input, unsigned char* output, int zeroes) 
{
  int i;
  printf("%7.3f MD5(%-25.25s) = ", time, input);
  for(i=0; i<MD5_DIGEST_LENGTH; i++) {
    printf("%02x", output[i]);
  }
  printf(" (%d zeroes)\n", zeroes);
}

void usage(char* program_name) 
{
    printf("Computes the md5-hash of the prefix followed by a number formatted as\n");
    printf("an ascii-string. \n");
    printf("Usage: %s <prefix>\n", program_name);
}

void clock_start(struct timespec *pstart)
{
  clock_gettime(CLOCK_MONOTONIC, pstart);
}

float clock_stop(struct timespec *pstart)
{
  struct timespec stop;
  clock_gettime(CLOCK_MONOTONIC, &stop);
  long seconds  =  stop.tv_sec  - pstart->tv_sec;
  long nanosec = (stop.tv_nsec - pstart->tv_nsec);
  if(nanosec < 0) {
    nanosec += 1000000000;
    seconds -= 1;
  }

  return ((float)seconds) + ((float)nanosec / 1000000000.0);
}

    
int main(int argc, char **argv) 
{
  if(argc != 2) {
    usage(argv[0]);
    return 1;
  }

  char* prefix = argv[1];
  int prefixlen = strlen(prefix);
  
  if(prefixlen > (BUFSZ-21)) {
    printf("Prefix too long\n");
    usage(argv[0]);
    return 2;
  }

  int zeroes = 0;
  int maxzeroes = 0;
  long i=0;
  unsigned char result[MD5_DIGEST_LENGTH];
  unsigned char string[BUFSZ];
  strcpy(string, prefix);
  unsigned char* write_num_at = string+prefixlen;
  int inputlen = 0;
  
  struct timespec start;
  clock_start(&start);
  
  do {
    i++;
    inputlen = sprintf(write_num_at, "%ld", i);
    inputlen += prefixlen;
    MD5(string, inputlen, result);
    zeroes = number_of_leading_zeroes(result);
    if(zeroes > maxzeroes) {
      print_md5(clock_stop(&start), string, result, zeroes);
      maxzeroes = zeroes;
    }
  } while(maxzeroes < 10);

  return 0;
}
