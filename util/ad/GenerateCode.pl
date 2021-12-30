#!/usr/bin/perl

open( IN, "<derivatives.txt" ) or die "Need to run GenerateDerivatives.nb first!\n";

while( <IN> )
{
   # grab the function name
   s/([^:]+):\s*//;
   print $_

   # TODO perform substitutions
}

close( IN );
