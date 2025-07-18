#include <iostream>
using namespace std;

struct PersonaSt;
typedef PersonaSt* Persona;

Persona consPersona(string nombre, int edad);
string nombre(Persona p);
int edad(Persona p);
void crecer(Persona p);
void cambioDeNombre(string nombre, Persona p);
bool esMayorQueLaOtra(Persona p1, Persona p2);
Persona laQueEsMayor(Persona p1, Persona p2);
void liberarP(Persona p);
void asignarMascota(Persona p);
Mascota mascotaP(Persona p);