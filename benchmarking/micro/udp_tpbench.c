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
    char buf[1024];
    struct hostent *host;
    int s, port;
    size_t total, win, received;
    struct timespec start, end;
    uint64_t micros, total_micros;

    if (argc != 5) {
        fprintf(stderr, "Usage: %s <host> <port> <window> <time>\n", argv[0]);
        return 1;
    }

    host = gethostbyname(argv[1]);
    if (host == NULL) {
        perror("gethostbyname");
        return 1;
    }

    port = atoi(argv[2]);
    win = atoi(argv[3]);
    total_micros = atoi(argv[4]);

    memset(buf, 0, sizeof(buf));
    total_micros *= 1000000;

    if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
        perror("socket");
        return 1;
    }

    memset(&server, 0, sizeof(struct sockaddr_in));
    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr = *((struct in_addr*) host->h_addr);

    clock_gettime(CLOCK_MONOTONIC, &start);
    end = start;
    micros = 0;
    received = 0;
    s_len = sizeof(struct sockaddr_in);
    // Fill send window
    for (total = 0; total < win; total++) {
        if (sendto(s, buf, sizeof(buf), 0,
                   (struct sockaddr *) &server, s_len) != sizeof(buf))
        {
            perror("sendto()");
            return 1;
        }
    }

    do {
        c_len = sizeof(client);
        if (recvfrom(s, buf, sizeof(buf), 0, (struct sockaddr *) &client,
                     &c_len) != sizeof(buf))
        {
            perror("recvfrom()");
            return 1;
        }
        received++;
        if (sendto(s, buf, sizeof(buf), 0,
                   (struct sockaddr *) &client, c_len) != sizeof(buf))
        {
            perror("sendto()");
            return 1;
        }
        total++;

        if (total % 1000 == 0) {
            clock_gettime(CLOCK_MONOTONIC, &end);
            micros = (end.tv_nsec - start.tv_nsec) / 1000;
            micros += (end.tv_sec - start.tv_sec) * 1000000;
        }
    } while (micros < total_micros);

    long double through = received * sizeof(buf);
    through /= micros / 1000000.L;

    puts("       Time [μs],"
         "    Data [bytes],"
         "Throughput [MB/s]");
    printf("%16"PRId64",%16"PRId64",%17Lf\n",
           micros, received * sizeof(buf), through / 1024 / 1024);

    close(s);
    return 0;
}
