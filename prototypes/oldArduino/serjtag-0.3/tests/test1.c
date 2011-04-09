#include <errno.h>
#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>

#define uint8_t unsigned char
#define int16_t short
#define uint16_t unsigned short

main(int argc,char *argv[]) {
	int fd,c,i,r;
	int speed;
	struct termios term;
	uint8_t buf[1024];

	fd = open("/dev/ttyUSB0", O_RDWR);
	if (fd < 0) {
		perror("open device");
		exit(1);
	}
	tcgetattr(fd, &term);
	speed = B57600;
	cfsetispeed(&term,speed);
	cfsetospeed(&term,speed);
	cfmakeraw(&term);
	usleep(100000); /* 100 ms */
	tcsetattr(fd, TCSANOW, &term);
	/* setup done */

	strcpy(buf, "SERJTAG");
	printf("SER  %02x %02x %02x %02x %02x %02x %02x\n"
			,buf[0],buf[1],buf[2]
			,buf[3],buf[4],buf[5],buf[6]);
	write(fd, "S",	1);
	r = read(fd, buf , 7);
	printf("S repl r = %d  %02x %02x %02x %02x %02x %02x %02x\n"
			,r,buf[0],buf[1],buf[2]
			,buf[3],buf[4],buf[5],buf[6]);
}
