#include "err.h"
#include "util.h"
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
const u8*pre  = (u8*)"    <summary class=\"section-heading\"><h2 id=\"";
const u8*http = (u8*)
  "HTTP/1.1 200 OK\r\n Content-type:text/html\r\n Content-length: ";
inline u8 h2i(u8 h) {return h - (h < 65 ? 48 : 55);}
inline void percentDecode(u8 *s) {u8*t = s;
doNextChar:
  if (!s[0]) {t[0] = 0; return;}
  if (s[0] == '%') {t[0] = 16 * h2i(s[1]) + h2i(s[2]);
    s += 3; t++; goto doNextChar;}
  t[0] = s[0]; s++; t++; goto doNextChar;
}
int main(int argc, char **argv) {
  string r, l, ls; stringstream ss; u8 *c, *c1, *c2, *c3, *d, *d1;
  zim::Archive arc("/home/d/data/wik/t/en.zim");
  int cN, conn, s = socket(PF_INET, SOCK_STREAM, 0);
  u8 copy; ptrdiff_t diff;
  nie(setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int)));
  struct sockaddr_in ca, sa; bzero(&sa, sizeof(sa)); sa.sin_family = AF_INET;
  sa.sin_addr.s_addr = htonl(INADDR_ANY); sa.sin_port = htons(atoi(argv[1]));
  socklen_t al = sizeof(ca); u8 cliIp[32], m[1024];
  nie(bind(s, (struct sockaddr*)&sa, sizeof(sa))); listen(s, 1024);
awaitClient:
  conn = nie(accept(s, (struct sockaddr*)&ca, &al));
  inet_ntop(AF_INET, (struct in_addr*)&ca.sin_addr, (char*)cliIp,
    sizeof(cliIp));
  memset(m, 0, sizeof(m));
  if (recvfrom(conn, m, sizeof(m), 0, (struct sockaddr*)&ca, &al) > 0) {
    u8 *wd = m + 5; while (wd[0] == '/') wd++;
    u8 haveData, full = 0;
    if (wd[0] == 'f' && wd[1] == '/') {wd += 2; full = 1;}
    u8 *wdEnd = wd; while (wdEnd[0] != ' ') wdEnd++; wdEnd[0] = '\0';
    percentDecode(wd);
    zim::Blob data;
    try {data = arc.getEntryByTitle((char*)wd).getItem().getData();
      haveData = 1;} catch (const std::exception& e) {haveData = 0;}
    if (haveData) {
      if (!full) {
        cN = data.size(); c = (u8*)malloc(2*cN); d = (u8*)malloc(cN);
        memcpy(c, data.data(), cN); c1 = c; copy = 1;
doLine:
        //c2 = c + 1600; c2[200] = 0; printf("%s\n", c2);
        if (startsW(c1, pre)) {
          c2 = c1 + strlen((char*)pre);
          if (startsW(c2, (u8*)"Eng") || startsW(c2, (u8*)"Ger") ||
            startsW(c2, (u8*)"Pol") || startsW(c2, (u8*)"Spa")) copy = 1;
          else copy = 0;
        }
        c2 = strchr((char*)c1, '\n');
        if (c2) {
          diff = c2 - c1;
          if (copy) memcpy(d1, c1, diff); c1 = c2; d1 += diff; goto doLine;
        }
        strcpy((char*)d1, (char*)c1);
        printf("strlen d: %d\n", strlen(d));
        sprintf(c, "%s%d\r\n\r\n%s", http, strlen(d), d); free(c); free(d);
        sendto(conn, c, strlen((char*)c), 0, (struct sockaddr*)&sa, al);
      } else {
        r = "HTTP/1.1 200 OK\r\n Content-type:text/html\r\n Content-length: ";
        r += to_string(data.size());
        r += "\r\n\r\n";
        r += data.data();
        sendto(conn, r.c_str(), r.length(), 0, (struct sockaddr*)&sa, al);
      }
    }
  } close(conn); goto awaitClient;}
