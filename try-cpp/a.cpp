#include <iostream>
using namespace std;
int main() {
    // 64-bit linux:   4 8 8
    // 64-bit windows: 4 4 8
    cout << sizeof(int) << endl;
    cout << sizeof(long) << endl;
    cout << sizeof(int*) << endl;
    cout << sizeof(size_t) << endl;
}
