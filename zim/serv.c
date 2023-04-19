#include "err.h"
#include "util.h"
#include <arpa/inet.h> // inet_ntop
#include <unistd.h> // close
#include <zim/archive.h>
#include <zim/item.h>
using namespace std;
const int one = 1;
const char
  *pre  = "    <summary class=\"section-heading\"><h2 id=\"",
  *hHtml = 
    "HTTP/1.1 200 OK\r\n Content-length: ",
  *hJs = 
"HTTP/1.1 200 OK\r\n Content-type: application/javascript\r\n Content-length: ",
  *postWd =
  " HTTP/1.1", *s1 = 
  "<!doctype html><html><head><meta charset=\"utf-8\"></head><body>",
  //"<html><head><meta charset=\"utf-8\"></head><body>",
  *s2 = ": no entry</body></html>";
inline char h2i(char h) {return h - (h < 65 ? 48 : 55);}
inline void urlDecode(char *s) {char *t = s; while (*s) {*t = (*s != '%') ?
  ((*s != '_') ? *s : ' ') : ((h2i(*++s)<<4) + h2i(*++s)); s++; t++;} *t = 0;}
inline int readWikChoice(char *s, char **t, u8 *full) {
  int l = (int)strlen(s); if (l >= 7 && s[0] == 'e' && s[1] == 'n' &&
      s[2] == 'w' && s[3] == 'i' && s[4] == 'k') {
    *t = s + 7;
    switch (s[5]) {
    case 'i': if (s[6] == '/') return 0; else return -1; break;
    case 't': 
      if (l >= 9 && s[6] == 'p' && s[7] == 'l' && s[8] == '/') {
        *full = 0; *t = s + 9; return 1;}
      if (s[6] == '/') return 1; else return -1; break;
    default: return -1;}
  } else return -1;}
int main(int argc, char **argv) {
  char *c, *c1, *c2, *c3, *d, *d1, *wd, *wdEnd, cliIp[32], m[1024];
  ptrdiff_t diff;
  u8 copy; int arcI;
  zim::Archive arc0("/home/d/data/wik/i/en.zim"),
    arc1("/home/d/data/wik/t/en.zim"), arc[2]={arc0,arc1}; zim::Blob data;
  int cN, conn, cLen, dLen, s = socket(PF_INET, SOCK_STREAM, 0), wdLen;
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
    u8 haveData = 0, full = 1; wdEnd = 0;
    wd = m + 5;
    urlDecode(wd);
    wdEnd = strchr(wd, '\n');
    arcI = -1;
    if (wdEnd) {
      wdEnd[-strlen(postWd) - 1] = 0;
      //char*e;
      //e=escStr(wd);printf("wd[%.*s]\n",199,e);free(e);
      arcI = readWikChoice(wd, &wd, &full);}
      while (*wd == '/') wd++;
    if (arcI != -1) {
      if (arcI == 0) *wd = toupper(*wd);
      try {data = arc[arcI].getEntryByTitle((char*)wd).getItem().getData();
        haveData = 1;} catch (const std::exception& e) {}}
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
        snprintf(c, 2 * cN, "%s%d\r\n\r\n%.*s", hHtml, dLen, dLen, d);
        sendto(conn, c, strlen((char*)c), 0, (struct sockaddr*)&sa, al);
        free(c); free(d);
      } else {
        char *r; int rL =
          asprintf(&r, "%s%zd\r\n\r\n%s", hHtml, data.size(), data.data());
        if (rL != -1) {
          sendto(conn, r, rL, 0, (struct sockaddr*)&sa, al); free(r);}
      }
    } else {
      wdLen = strlen(wd);
      u8 isJs = wd[wdLen - 1] == 's';
      if (isJs)
      dLen = strlen(s1) + wdLen + strlen(s2);
      d = (char*)malloc(dLen + 1); snprintf(d, dLen + 1, "%s%s%s", s1, wd, s2);
      cN = strlen(hHtml) + 999 + dLen; c = (char*)malloc(cN);
      snprintf(c, cN, "%s%d\r\n\r\n%s", isJs ? hHtml : hJs, isJs ? 0 : dLen,
        isJs ? "" : d);
      cLen = strlen(c); sendto(conn, c, cLen, 0, (struct sockaddr*)&sa, al);
      free(c); free(d);
    }
  } close(conn); goto awaitClient;}
//printf("arcI[%d]\n", arcI);
//e=escStr(wd);printf("wd[%.*s]\n",199,e);free(e);
