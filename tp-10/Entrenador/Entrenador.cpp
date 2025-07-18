#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"
using namespace std;

struct EntrenadorSt
{
    string nombre;
    Pokemon* pokemons;
    int cantPokemons;
};

typedef EntrenadorSt* Entrenador;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon *pokemons)
{
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemons = cantidad;
    e->pokemons = pokemons;
    return e;
}

string nombreDeEntrenador(Entrenador e)
{
    return e->nombre;
}

int cantidadDePokemon(Entrenador e)
{
    return e->cantPokemons;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e)
{
    int cantidad = 0;
    for (int i = 0; i < e->cantPokemons; i++)
    {
        if (tipoDePokemon(e->pokemons[i]) == tipo)
        {
            cantidad++;
        }
    }
    return cantidad;
}

Pokemon pokemonNro(int i, Entrenador e)
{
    return e->pokemons[i];
}

bool esteLeGanaATodos(Pokemon p, Entrenador e)
{
    int cantLeGana = 0;
    for (int i = 0; i < e->cantPokemons; i++)
    {
        if (superaA(p, e->pokemons[i]))
        {
            cantLeGana++;
        }
    }
    return cantLeGana == e->cantPokemons;
}

bool leGanaATodos(Entrenador e1, Entrenador e2)
{
    int i = 0;
    while (i < e1->cantPokemons)
    {
        if (esteLeGanaATodos(e1->pokemons[i], e2)) {
            return true;
        }
      i++  
    }
    return false;
}