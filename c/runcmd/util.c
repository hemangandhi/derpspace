#include <sys/socket.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <netinet/in.h>
#include <unistd.h>

int populateSockAddr(struct sockaddr_in * dest, int port, const char * host){
    bzero((char *) dest, sizeof(struct sockaddr_in));
    dest->sin_family = AF_INET;
    dest->sin_port = htons(port);

    if(host == NULL){
        dest->sin_addr.s_addr = htonl(INADDR_ANY);
    }else{
        struct hostent * hinfo = gethostbyname(host);
        if(hinfo == NULL) return 0;
        dest->sin_addr = * (struct in_addr *) hinfo->h_addr;
    }
    return 1;
}

int makeSocket(int port, const char * hostname){
    int sock;
    struct sockaddr_in name;

    sock = socket(PF_INET, SOCK_STREAM, 0);
    if(sock < 0){
        return -1;
    }

    if(!populateSockAddr(&name, port, hostname)){
        return -1;
    }

    int (* conFn) (int, const struct sockaddr*, socklen_t);
    if(hostname == NULL){
        conFn = &bind;
    }else{
        conFn = &connect;
    }
    if(conFn(sock, (struct sockaddr *) &name, sizeof(name)) < 0){
        return -1;
    }
    return sock;
}

typedef struct {
    char ** argv;
    char * stdoutNm;
    char * stderrNm;
} ExecInfo;

int execExecInfo(ExecInfo * info){
    int pid = fork();
    if(pid < 0){
        return 0;
    }else if(pid == 0){
        int stdoutFd = open(info->stdoutNm, O_WRONLY | O_TRUNC | O_CREAT, S_IRWXU);
        int stderrFd = open(info->stderrNm, O_WRONLY | O_TRUNC | O_CREAT, S_IRWXU);
        if(stdoutFd < 0 || stderrFd < 0){
            exit(EXIT_FAILURE);
        }

        int d1 = dup2(stdoutFd, STDOUT_FILENO);
        int d2 = dup2(stderrFd, STDERR_FILENO);
        if(d1 < 0 || d2 < 0){
            exit(EXIT_FAILURE);
        }

        execv(info->argv[0], info->argv + 1);
        exit(EXIT_FAILURE);
    }else{
        int status;
        waitpid(pid, &status, 0);
        if(WIFEXITED(status)){
            return (WEXITSTATUS(status) != EXIT_FAILURE);
        }
        return 0;
    }
}

int writeBuffer(const char * buf, int len, int outFd){
    ssize_t p, e;
    for(p = 0, e = 0; p < len && e >= 0;
        e = write(outFd, buf + p, len - p), p += (e > 0)? e: 0);
    return e >= 0;
}

int writeExecInfo(ExecInfo * info, int fd){
    char space = ' ';
    for(int i = 0; info->argv[i] != NULL; i++){
        int r = 1;
        if(i > 0)
            r = writeBuffer(&space, 1, fd);
        r = r && writeBuffer(info->argv[i], strlen(info->argv[1]), fd);
        if(!r) return 0;
    }

    return 1;
}

ExecInfo * readExecInfo(int fd){
    
}
