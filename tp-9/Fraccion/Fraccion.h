#include <iostream>
using namespace std;

struct Fraccion;

Fraccion consFraccion(int numerador, int denominador);
int numerador(Fraccion f);
int denominador(Fraccion f);
float division(Fraccion f);
Fraccion multF(Fraccion f1, Fraccion f2);
Fraccion simplificada(Fraccion p);
Fraccion sumF(Fraccion f1, Fraccion f2);
void ShowFraccion(Fraccion f);