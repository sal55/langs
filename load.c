/*
 * Load: Converts Intel Format Paper Tape into memory image
 * 1/2/90
 *
 * Copyright 1990 Erik Mueller
 */

#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>

char* gets(char*);
int readhex(char* s, short len);
int hextodec(char c);


char		buf[512];
unsigned char	mem[32767];

int main(void)
{
	long	minaddr, maxaddr, i, line, len, addr, t, b, checksum, cnt;
	unsigned char sum;
	FILE	*stream;

	stream = fopen("empl.bin", "w+b");
	minaddr = maxaddr = -1;
	line = 0;
	cnt = 0;
	while (gets(buf)) {
		line++;
		if (buf[0] == '*') {
			continue;
		}
		if (buf[0] != ':') {
			fprintf(stderr, "Line %d: does not begin with colon\n", line);
		}
		sum = 0;
		if ((len = readhex(&buf[1], 2)) < 0) {
			fprintf(stderr, "Line %d: trouble reading record length (1-2)\n",
				line);
			continue;
		}
		sum += len;
		if (len == 0) {
		/* End of file indication. */
			if (gets(buf))
				fprintf(stderr, "Line %d: characters past end of file\n");
			break;
		}
		if ((addr = readhex(&buf[3], 4)) < 0) {
			fprintf(stderr, "Line %d: trouble reading load address (3-6)\n",
				line);
			continue;
		}
		sum += readhex(&buf[3], 2);
		sum += readhex(&buf[5], 2);
		if ((minaddr == -1) || (addr < minaddr)) minaddr = addr;
		if ((t = readhex(&buf[7], 2)) < 0) {
			fprintf(stderr, "Line %d: trouble reading record type (7-8)\n",
				line);
			t = 0;
		}
		sum += t;
		if (t != 0) {
			fprintf(stderr, "Line %d: unsupported record type (7-8) %d\n",
				line, t);
		}
		for (i = 9; i < 9+2*len; i += 2) {
			if ((b = readhex(&buf[i], 2)) < 0) {
				fprintf(stderr, "Line %d: trouble reading data (9+)\n",
					line);
				b = 0;
			}
			mem[addr] = b;
			cnt++;
			sum += b;
			addr++;
		}
		addr--;
		sum = -sum;
		if ((checksum = readhex(&buf[i], 2)) < 0) {
			fprintf(stderr, "Line %d: trouble reading checksum\n", line);
			checksum = 0;
		}
		if (checksum != sum) {
			fprintf(stderr, "Line %d: checksum error: %xH %xH\n", line,
				checksum, sum);
		}
		if ((maxaddr == -1) || (addr > maxaddr)) maxaddr = addr;
	}
	for (i = minaddr; i <= maxaddr; i++) putc(mem[i], stream);
	fprintf(stderr, "%d bytes (range of %d) from %xH to %xH\n",
		cnt, (short)(1+maxaddr-minaddr), minaddr, maxaddr);
}

int readhex(char* s, short len)
{
	int	i, d;
	int	result;

	result = 0;
	for (i = 0; i < len; i++) {
		result = result * 16;
		if ((d = hextodec(s[i])) < 0) return(-1);
		result += d;
	}
	return(result);
}

//int isdigit(char c)
//{
//	return((c >= '0') && (c <= '9'));
//}
//
//int isxdigit(char c)
//{
//	return(((c >= '0') && (c <= '9')) ||
//               ((c >= 'a') && (c <= 'f')));
//}

int hextodec(char c)
{
	if (!isxdigit(c)) return(-1);
	if (isdigit(c)) return(c - '0');
	if (c >= 'a') return(10 + c - 'a');
	else return(10 + c - 'A');
}

/* End of file. */
