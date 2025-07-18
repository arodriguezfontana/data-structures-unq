#include <iostream>
#include "Pokemon.h"
using namespace std;

typedef string TipoDePokemon;
struct PokemonSt
{
    TipoDePokemon tipo;
    int vida;
};
typedef PokemonSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo)
{
    PokemonSt* p = new PokemonSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p)
{
    return p->tipo;
}

int energia(Pokemon p)
{
    return p->vida;
}

void perderEnergia(int energia, Pokemon p)
{
    p->vida = p->vida - energia;
}

bool superaA(Pokemon p1, Pokemon p2)
{
    return (p1->tipo == 'agua' && p2->tipo == 'fuego' ||
            p1->tipo == 'fuego' && p2->tipo == 'planta' ||
            p1->tipo == 'planta' && p2->tipo == 'agua');
}