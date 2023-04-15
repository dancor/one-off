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
const char *pre  = "    <summary class=\"section-heading\"><h2 id=\"",
  *http = "HTTP/1.1 200 OK\r\n Content-type:text/html\r\n Content-length: ",
  *s1 = "<html><body>", *s2 = ": no entry</body></html>";
inline char h2i(char h) {return h - (h < 65 ? 48 : 55);}
inline void percentDecode(char *s) {char *t = s;
doNextChar:
  if (!s[0]) {t[0] = 0; return;}
  if (s[0] == '%') {t[0] = 16 * h2i(s[1]) + h2i(s[2]);
    s += 3; t++; goto doNextChar;}
  t[0] = s[0]; s++; t++; goto doNextChar;
}
int main(int argc, char **argv) {
  string r;
  char *c, *c1, *c2, *c3, *d, *d1, cliIp[32], m[1024]; ptrdiff_t diff;
  zim::Archive arc("/home/d/data/wik/t/en.zim"); zim::Blob data;
  int cN, conn, cLen, dLen, s = socket(PF_INET, SOCK_STREAM, 0), wdLen; u8 copy;
  nie(setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int)));
  struct sockaddr_in ca, sa; bzero(&sa, sizeof(sa)); sa.sin_family = AF_INET;
  sa.sin_addr.s_addr = htonl(INADDR_ANY); sa.sin_port = htons(atoi(argv[1]));
  socklen_t al = sizeof(ca);
  nie(bind(s, (struct sockaddr*)&sa, sizeof(sa))); listen(s, 1024);
awaitClient:
  conn = nie(accept(s, (struct sockaddr*)&ca, &al));
  inet_ntop(AF_INET, (struct in_addr*)&ca.sin_addr, cliIp, sizeof(cliIp));
  memset(m, 0, sizeof(m));
  if (recvfrom(conn, m, sizeof(m), 0, (struct sockaddr*)&ca, &al) > 0) {
    char *wd = m + 5; percentDecode(wd); while (wd[0] == '/') wd++;
    u8 haveData, full = 0;
    if (wd[0] == 'f' && wd[1] == '/') {wd += 2; full = 1;}
    char *wdEnd = wd; while (wdEnd[0] != ' ') wdEnd++; wdEnd[0] = '\0';
    try {data = arc.getEntryByTitle((char*)wd).getItem().getData();
      haveData = 1;} catch (const std::exception& e) {haveData = 0;}
    if (haveData) {
      if (!full) {
        cN = data.size(); c = (char*)malloc(cN + 999);
        d = (char*)malloc(cN + 1); d1 = d;
        memcpy(c, data.data(), cN); c[cN] = 0; c1 = c; copy = 1;
doLine:
        //c2 = c + 1600; c2[200] = 0; printf("%s\n", c2);
        if (startsW(c1, pre)) {
          c2 = c1 + strlen((char*)pre);
          if (startsW(c2, "Eng") || startsW(c2, "Ger") || startsW(c2, "Pol") ||
              startsW(c2, "Spa")) copy = 1; else copy = 0;
        }
        c2 = strchr(c1, '\n');
        if (c2) {
          diff = c2 + 1 - c1;
          if (copy) {memcpy(d1, c1, diff); d1 += diff;}
          c1 += diff;
          goto doLine;
        }
        strcpy(d1, c1); dLen = (int)strlen(d);
        //printf("dLen[%d]\n", dLen);
        snprintf(c, 2 * cN, "%s%d\r\n\r\n%.*s", http, dLen, dLen, d);
        sendto(conn, c, strlen((char*)c), 0, (struct sockaddr*)&sa, al);
        free(c); free(d);
      } else {
        r = http;
        r += to_string(data.size());
        r += "\r\n\r\n";
        r += data.data();
        sendto(conn, r.c_str(), r.length(), 0, (struct sockaddr*)&sa, al);
      }
    } else {
      wdLen = strlen(wd); dLen = strlen(s1) + wdLen + strlen(s2);
      d = (char*)malloc(dLen + 1); snprintf(d, dLen + 1, "%s%s%s", s1, wd, s2);
      cN = strlen(http) + 999 + dLen; c = (char*)malloc(cN);
      snprintf(c, cN, "%s%d\r\n\r\n%s", http, dLen, d); cLen = strlen(c);
      sendto(conn, c, cLen, 0, (struct sockaddr*)&sa, al);
      //printf("dLen[%d] cLen[%d]\n", dLen, cLen);
      //char*e=escStr(c);printf("c[%.*s]\n",199,e);free(e);
      free(c); free(d);
    }
  } close(conn); goto awaitClient;}
