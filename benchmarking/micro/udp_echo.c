#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#define PORT 7

static char buf[16*1024];

static void die(const char *msg)
{
    perror(msg);
    exit(1);
}


int main(int argc, char *argv[])
{
    struct sockaddr_in si, si_other;
    int s;
    socklen_t slen;
    ssize_t len;

    if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
        die("socket() failed");


    memset(&si, 0, sizeof(si));
    si.sin_family = AF_INET;
    si.sin_port = htons(PORT);
    if (bind(s, (struct sockaddr *) &si, sizeof(si))==-1)
        die("bind() failed");

    while (1) {
        slen = sizeof(si_other);
        if ((len = recvfrom(s, buf, sizeof(buf), 0,
                        (struct sockaddr *) &si_other, &slen)) < 0)
            die("recvfrom() failed");
        if (sendto(s, buf, len, 0, (struct sockaddr *) &si_other, slen) == -1)
            die("sendto() failed");
    }

    return 0;
}

