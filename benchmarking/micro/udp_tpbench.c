#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <time.h>

struct config {
    uint64_t bench_micros;
    uint64_t warmup_micros;
    uint64_t cooldown_micros;
    uint64_t check_freq;

    size_t msg_size;
    size_t win_size;
    uint32_t remote_addr;
    uint32_t local_addr;
    uint16_t remote_port;
    uint16_t local_port;
};

static void default_config(struct config *cfg)
{
    memset(cfg, 0, sizeof(*cfg));

    cfg->bench_micros = 10 * 1000 * 1000;
    cfg->check_freq = 1000;
    cfg->msg_size = 1024;
    cfg->win_size = 10;
}

static void print_usage(void)
{
    puts("Usage: udp_tpbench [options] dst-ip:dst-port");
    puts("Timing:");
    puts("  -t <secs>   Time measuring throughput        [default      10]");
    puts("  -u <secs>   Warm-up time before measuring    [default       0]");
    puts("  -d <secs>   Cool-down time after measuring   [default       0]");
    puts("  -f <freq>   Check time every <freq> receives [default    1000]");
    puts("\nData:");
    puts("  -m <bytes>  Message size in bytes            [default    1024]");
    puts("  -w <count>  Number of Messages in flight     [default      10]");
    puts("\nNetwork:");
    puts("  -l <ip>     Local IP to use                  [default 0.0.0.0]");
    puts("  -p <port>   Local port to use                [default       0]");
}

static void parse_params(int argc, char *argv[], struct config *cfg)
{
    char *r_ip, *col, *r_p;
    struct hostent *host;
    struct in_addr ina;
    int res;

    while ((res = getopt(argc, argv, "t:u:d:f:m:w:l:p:")) != -1) {
        switch (res) {
            // Timing
            case 't':
                cfg->bench_micros = atoll(optarg) * 1000 * 1000;
                break;
            case 'u':
                cfg->warmup_micros = atoll(optarg) * 1000 * 1000;
                break;
            case 'd':
                cfg->cooldown_micros = atoll(optarg) * 1000 * 1000;
                break;
            case 'f':
                cfg->check_freq = atoll(optarg);
                break;

            // Data
            case 'm':
                cfg->msg_size = atol(optarg);
                break;
            case 'w':
                cfg->win_size = atol(optarg);
                break;

            // Network
            case 'l':
                if (inet_pton(AF_INET, optarg, &ina) != 1) {
                    goto out_err;
                }
                cfg->local_addr = ina.s_addr;
                break;
            case 'p':
                cfg->local_port = atoi(optarg);
                break;

            // Error handling
            default:
                goto out_err;
        }
    }

    if (optind >= argc || argc - optind != 1) {
        goto out_err;
    }

    // parse ip:port
    r_ip = argv[optind];
    col = strchr(r_ip, ':');
    if (col == NULL) {
        goto out_err;
    }
    *col = 0;
    r_p = col + 1;

    cfg->remote_port = atoi(r_p);
    host = gethostbyname(r_ip);
    if (host == NULL) {
        perror("gethostbyname");
        exit(-1);
    }
    cfg->remote_addr = ((struct in_addr*) host->h_addr)->s_addr;
    return;

out_err:
    print_usage();
    exit(-1);
}

static uint64_t get_micros(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 * 1000 + ts.tv_nsec / 1000;
}


int main(int argc, char *argv[])
{
    struct sockaddr_in server, client;
    socklen_t s_len, c_len;
    char *buf;
    int s;
    size_t total, received;
    uint64_t micros, total_micros, measure_micros;
    uint64_t t_start, t_mstart, t_mend, t_end;
    bool warmup_done, measuring_done;

    // Get configuration
    struct config rwcfg;
    default_config(&rwcfg);
    parse_params(argc, argv, &rwcfg);
    const struct config cfg = rwcfg;

    // Prepare data buffer
    buf = malloc(cfg.msg_size);
    memset(buf, 0, cfg.msg_size);

    // Prepare socket parameters
    memset(&client, 0, sizeof(client));
    client.sin_family = AF_INET;
    client.sin_port = htons(cfg.local_port);
    ((struct in_addr *) &client.sin_addr)->s_addr = cfg.local_addr;

    s_len = sizeof(server);
    memset(&server, 0, s_len);
    server.sin_family = AF_INET;
    server.sin_port = htons(cfg.remote_port);
    ((struct in_addr *) &server.sin_addr)->s_addr = cfg.remote_addr;

    // Initialize socket
    if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
        perror("socket");
        return 1;
    }
    if (bind(s, (struct sockaddr *) &client, sizeof(client)) != 0) {
        perror("bind");
        return 1;
    }

    received = total = 0;
    micros = 0;
    t_mstart = t_mend = 0;
    total_micros = cfg.warmup_micros + cfg.bench_micros + cfg.cooldown_micros;
    warmup_done = (cfg.warmup_micros == 0);
    measuring_done = false;

    t_start = get_micros();
    if (warmup_done) {
        t_mstart = t_start;
    }
    // Fill send window
    for (total = 0; total < cfg.win_size; total++) {
        if (sendto(s, buf, cfg.msg_size, 0,
                   (struct sockaddr *) &server, s_len) != cfg.msg_size)
        {
            perror("sendto()");
            return 1;
        }
    }
    do {
        c_len = sizeof(client);
        if (recvfrom(s, buf, cfg.msg_size, 0, (struct sockaddr *) &client,
                     &c_len) != cfg.msg_size)
        {
            perror("recvfrom()");
            return 1;
        }
        if (warmup_done && !measuring_done) {
            received++;
        }
        if (sendto(s, buf, cfg.msg_size, 0,
                   (struct sockaddr *) &client, c_len) != cfg.msg_size)
        {
            perror("sendto()");
            return 1;
        }
        total++;

        if (total % cfg.check_freq == 0) {
            t_end = get_micros();
            if (!warmup_done && (t_end - t_start) >= cfg.warmup_micros) {
                warmup_done = true;
                t_mstart = t_end;
            }
            if (warmup_done && !measuring_done &&
                (t_end - t_mstart) >= cfg.bench_micros)
            {
                measuring_done = true;
                t_mend = t_end;
            }
            micros = t_end - t_start;
        }
    } while (micros < total_micros);

    measure_micros = t_mend - t_mstart;
    long double through = received * cfg.msg_size;
    through /= measure_micros / 1000000.L;

    puts("       Time [μs],"
         "    Data [bytes],"
         "Throughput [MB/s]");
    printf("%16"PRId64",%16"PRId64",%17Lf\n",
           measure_micros, received * cfg.msg_size, through / 1024 / 1024);

    close(s);
    return 0;
}
