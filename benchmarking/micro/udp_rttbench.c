#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <time.h>

int main(int argc, char *argv[])
{
    struct sockaddr_in server, client;
    socklen_t s_len, c_len;
    char buf[1024], rxbuf[1024];
    struct hostent *host;
    int s, port;
    struct timespec start, end;
    size_t i, n, size;

    if (argc != 4) {
        fprintf(stderr, "Usage: %s <host> <port> <n>\n", argv[0]);
        return 1;
    }

    host = gethostbyname(argv[1]);
    if (host == NULL) {
        perror("gethostbyname");
        return 1;
    }

    port = atoi(argv[2]);
    n = atol(argv[3]);

    if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
        perror("socket");
        return 1;
    }

    memset(&server, 0, sizeof(struct sockaddr_in));
    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr = *((struct in_addr*) host->h_addr);

    end = start;
    s_len = sizeof(struct sockaddr_in);
    for (size = 64; size <= 1024; size <<= 1) {
        for (i = 0; i < n; i++) {
            clock_gettime(CLOCK_MONOTONIC, &start);
            memset(buf, 'a', size);
            if (sendto(s, buf, size, 0,
                       (struct sockaddr *) &server, s_len) != size)
            {
                perror("sendto()");
                return 1;
            }
            c_len = sizeof(client);
            if (recvfrom(s, rxbuf, sizeof(rxbuf), 0,
                         (struct sockaddr *) &client, &c_len) != size)
            {
                perror("recvfrom()");
                return 1;
            }
            if (memcmp(rxbuf, buf, size)) {
                fprintf(stderr, "Unexpected payload\n");
            }
            clock_gettime(CLOCK_MONOTONIC, &end);
            long double rtt = (end.tv_nsec - start.tv_nsec) / 1000.;
            rtt += (end.tv_sec - start.tv_sec) * 1000000.L;
            printf("%zu %Lf\n", size, rtt);
        }
    }
    close(s);
    return 0;
}
