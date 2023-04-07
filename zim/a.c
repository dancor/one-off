#include <fstream>
#include <sstream>
#include <iostream>
#include <unistd.h>
#include <zim/archive.h>
#include <zim/entry.h>
#include <zim/item.h>
using namespace std;
const char*preGood = "    <summary class=\"section-heading\"><h2 id=\"Polish";
const char*pre = "    <summary class=\"section-heading\"><h2 id=\"";
int main(int argc, char**argv) {try {
  zim::Archive a("/home/d/data/wik/t/en.zim");
  ofstream f; f.open("/home/d/tmp/zim.htm");
  auto data = a.getEntryByTitle(argv[1]).getItem().getData();
  int cN = data.size() + 1; char*c = (char*)malloc(cN);
  memcpy(c, data.data(), cN - 1); c[cN - 1] = 0;
  stringstream ss(c);
  string l;
  bool copy = true;
  while (getline(ss, l, '\n')) {
    if (!l.rfind(preGood, 0)) copy = true;
    else {if (!l.rfind(pre, 0)) copy = false;}
    if (copy) f << l << "\n";
  }
  /*int lineBeg = 0;
  // go thru and print lines until get to first line
  
  //f << int(c[cN - 1]) << "\n";
  */
  f.close();
  } catch (const std::exception& e) {cerr << e.what() << ". LOL\n";}
  const char*cmd = "/home/danl/bin/ff";
  execl(cmd, cmd, "/home/d/tmp/zim.htm", NULL);
}
