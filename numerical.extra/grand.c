#include <stdlib.h>
#include <limits.h>
#include <math.h>

/*
     grand() gerarates a random number which has gaussian (regular) 
     distribution.  p(x) = sqrt(2*pi) exp(-x^2/2).


     DESCRIPTION:

     Consider a function which is defined on a two-dimentional
     circle of r < 1.

         f(x1, x2) = sqrt(-2 * log(r^2)) r/|r|,

     where r is a position vector r = (x1, x2).
     The Jacobian of this function is gaussian type like this

         dx1dx2 = A exp(-(y1^2 + y2^2)/2) dy1dy2.
                => A exp(-(y1^2)/2) dy1 * exp(-(y2^2)/2) dy2

    We get two independent series of numbers both of which 
    have gaussian distribution from one uniform random vector 
    of |r| < 1.

 */

double grand()
{
  double x1, x2, y1, y2, w, p;
  double retval;
  static int saved = 0;
  static double save;

  if (saved) {
    saved = 0;
    return save;
  } else {
    do {
      /*      
       x1 = (double)random()/LONG_MAX;
       x2 = (double)random()/LONG_MAX;
      */
      x1 = drand48();
      x2 = drand48();
      x1 = 2*x1 - 1.0;
      x2 = 2*x2 - 1.0;
      w = x1*x1 + x2*x2;
    } while (w <= 0.0 || w >= 1.0);

    p = sqrt(-2*log(w)/w);
    y1 = p * x1;
    y2 = p * x2;

    save  = y2;
    saved = 1;
    return y1;
  }
}

