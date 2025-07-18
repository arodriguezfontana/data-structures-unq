#include <iostream>
using namespace std;

struct SetSt;
typedef SetSt* Set;

Set emptyS();
bool isEmptyS(Set s);
bool belongsS(int x, Set s);
void AddS(int x, Set s);
void RemoveS(int x, Set s);
int sizeS(Set s);
LinkedList setToList(Set s);
void DestroyS(Set s);