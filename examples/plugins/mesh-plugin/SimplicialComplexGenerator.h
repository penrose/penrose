#ifndef SIMPLICIALCOMPLEXGENERATOR_H
#define

#include <map>
#include <string>

#include "SimplicialComplex.h"

// Wrapper around mesh class that generates Penrose
// code (for visualization)

typedef std::string str

class SimplicialComplexGenerator
{
   public:
      str generateSimplicialComplex( const str& K );
      str generateVertexOf( const str& K );
      str generateVertexStar( const str& K, const str& S, const str& v );
      str generateClosure( const str& K, const str& S, const str& A );

   protected:
      map<str,SimplicialComplex> complexes;
};

#endif // SIMPLICIALCOMPLEXGENERATOR_H
