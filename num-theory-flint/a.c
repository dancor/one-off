#include <stdio.h>
#include "flint.h"
#include "fmpz_poly.h"
//#include "ulong_extras.h"

int main()
{
    int i;
    fmpz_poly_t f, g;
    fmpz_poly_init(f);
    fmpz_poly_init(g);

    for (i = 0; i < 5; i++)
        fmpz_poly_set_coeff_ui(f, i, n_nth_prime(i + 1));
    fmpz_poly_mul(g, f, f);
    fmpz_poly_print_pretty(g, "x");

    fmpz_poly_clear(f);
    fmpz_poly_clear(g);
}
