#include <fstream>
#include <iostream>
#include <unistd.h>
#include <zim/archive.h>
#include <zim/entry.h>
#include <zim/item.h>
using namespace std;
int main(int argc, char**argv) {try {
  zim::Archive a("/home/d/data/wik/t/en.zim");
  ofstream f; f.open("/home/d/tmp/zim.htm");
  f << a.getEntryByTitle(argv[1]).getItem().getData() << "\n";
  f.close();
  } catch (const std::exception& e) {cerr << e.what() << ". LOL\n";}
  const char*cmd = "/home/danl/bin/ff";
  execl(cmd, cmd, "/home/d/tmp/zim.htm", NULL);
}
