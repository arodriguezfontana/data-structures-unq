#include <iostream>
#include "Persona.h"
using namespace std;

struct PersonaSt
{
    string nombre;
    int edad;
    Mascota mascota;
    int cantMascotas; 
};
typedef PersonaSt* Persona;

Persona consPersona(string nombre, int edad)
{
    PersonaSt* p = new PersonaSt;
    p->nombre = nombre;
    p->edad = edad;
    return p;
}

string nombre(Persona p)
{
    return p->nombre;
}

int edad(Persona p)
{
    return p->edad;
}

void crecer(Persona p)
{
    p->edad++;
}

void cambioDeNombre(string nombre, Persona p)
{
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2)
{
    return p1->edad >= p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2)
{
    if (p1->edad >= p2->edad)
    {
        return p1;
    }
    else
    {
        return p2;
    }
}

void liberarP(Persona p) {
    delete p;
}

void asignarMascota(Persona p, Mascota mascota) {
    p->mascota = mascota;
}

void arrayAsignarMascota(Persona p, Mascota mascota) { 
    p->mascotas[p->cantMascotas] = m;
    p->cantMascotas++;
    if (p->cantMascotas == p->tamanioMascotas)
    {
        agrandarArray(p)
    }
}

void agrandarArray(Persona p) { 
    Mascota* nuevoArray = new Mascota[p->tamanioMascotas * 2]
    for (int i = 0; i < p->tamanioMascotas; i++)
    {
        nuevoArray[i] = p->mascotas[i];
    }
    delete[] p->mascotas;
    p->mascotas = nuevoArray;
    p->tamanioMascotas = p->tamanioMascotas * 2;
}

Mascota mascotaP(Persona p) {
    return p->mascota;
}