#include "err.h"
#include <arpa/inet.h> // inet_ntop
#include <fstream>
#include <iostream>
#include <sstream>
#include <unistd.h> // close
#include <zim/archive.h>
#include <zim/entry.h>
#include <zim/item.h>
using namespace std;
const int one = 1;
const char*pre1 = "    <summary class=\"section-heading\"><h2 id=\"English";
const char*pre2 = "    <summary class=\"section-heading\"><h2 id=\"German";
const char*pre3 = "    <summary class=\"section-heading\"><h2 id=\"Polish";
const char*pre4 = "    <summary class=\"section-heading\"><h2 id=\"Spanish";
const char*pre  = "    <summary class=\"section-heading\"><h2 id=\"";
inline u8 h2i(u8 h) {return h - (h < 65 ? 48 : 55);}
inline void percentDecode(char *s) {char*t = s;
doNextChar:
  if (!s[0]) {t[0] = 0; return;}
  if (s[0] == '%') {t[0] = char(16 * h2i((u8)s[1]) + h2i((u8)s[2]));
    s += 3; t++; goto doNextChar;}
  t[0] = s[0]; s++; t++; goto doNextChar;
}
int main(int argc, char **argv) {
  string r, l, ls; stringstream ss; char*c;
  zim::Archive arc("/home/d/data/wik/t/en.zim");
  int cN, cN1, conn, s = socket(PF_INET, SOCK_STREAM, 0);
  nie(setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int)));
  struct sockaddr_in ca, sa; bzero(&sa, sizeof(sa)); sa.sin_family = AF_INET;
  sa.sin_addr.s_addr = htonl(INADDR_ANY); sa.sin_port = htons(atoi(argv[1]));
  socklen_t al = sizeof(ca); char cliIp[32], m[1024];
  nie(bind(s, (struct sockaddr*)&sa, sizeof(sa))); listen(s, 1024);
awaitClient:
  conn = nie(accept(s, (struct sockaddr*)&ca, &al));
  inet_ntop(AF_INET, (struct in_addr*)&ca.sin_addr, cliIp, sizeof(cliIp));
  memset(m, 0, sizeof(m));
  if (recvfrom(conn, m, sizeof(m), 0, (struct sockaddr*)&ca, &al) > 0) {
    char *wd = m + 5; while (wd[0] == '/') wd++;
    u8 haveData, full = 0;
    if (wd[0] == 'f' && wd[1] == '/') {wd += 2; full = 1;}
    char *wdEnd = wd; while (wdEnd[0] != ' ') wdEnd++; wdEnd[0] = '\0';
    percentDecode(wd);
    //printf("wd[%s]\n", wd);
    printf("wd[%s] %d %d %d\n", wd, wd[0], wd[1], wd[2]);
    zim::Blob data;
    try {data = arc.getEntryByTitle(wd).getItem().getData(); haveData = 1;}
      catch (const std::exception& e) {haveData = 0;}
    if (haveData) {
      if (!full) {
        u8 copy = 1; cN1 = data.size(), cN = cN1 + 1; c = (char*)malloc(cN);
        memcpy(c, data.data(), cN1); c[cN1] = 0; ss<<c;
        //printf("%s\n", c);
        free(c);
        while (getline(ss, l, '\n')) {
          if (!l.rfind(pre1, 0)) copy = 1; else if (!l.rfind(pre2, 0)) copy = 1;
          else if (!l.rfind(pre3, 0)) copy = 1;
          else if (!l.rfind(pre4, 0)) copy = 1;
          else if (!l.rfind(pre, 0)) copy = 0;
          if (copy) {ls += l; ls += "\n";}}
        //cout<<"LOLLOLLOL "<<ls.substr(1500, 100)<<"\n";
        r = "HTTP/1.1 200 OK\r\n Content-type:text/html\r\n Content-length: ";
        r += to_string(ls.length());
        r += "\r\n\r\n";
        r += ls;
        //cout<<"LOLLOLLOL "<<r.substr(200, 100)<<"\n";
        sendto(conn, r.c_str(), r.length(), 0, (struct sockaddr*)&sa, al);
      } else {
        r = "HTTP/1.1 200 OK\r\n Content-type:text/html\r\n Content-length: ";
        r += to_string(data.size());
        r += "\r\n\r\n";
        r += data.data();
        sendto(conn, r.c_str(), r.length(), 0, (struct sockaddr*)&sa, al);
      }
    }
  } close(conn); goto awaitClient;}
