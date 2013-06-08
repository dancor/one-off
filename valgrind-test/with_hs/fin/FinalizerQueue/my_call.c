#include "my_call.h"
void my_call_2(my_thunk thunk1, my_thunk thunk2) {
  thunk1();
  thunk2();
}
