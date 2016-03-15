/* Factoring with Pollard's rho method.

Copyright 1995, 1997-2003, 2005, 2009, 2012 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see https://www.gnu.org/licenses/.  */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>
#include <sys/time.h>

#include "gmp.h"

static unsigned char primes_diff[] = {
#define P(a,b,c) a,
#include "primes.h"
#undef P
};
#define PRIMES_PTAB_ENTRIES (sizeof(primes_diff) / sizeof(primes_diff[0]))

typedef uint64_t n_t;

struct mpfactors {
    mpz_t         *p;
    unsigned long *e;
    long nmpfactors;
};

struct factors {
    n_t     *p;
    uint8_t *e;
    uint8_t nfactors;
};

void factor_clear (struct factors *factors) {
    free (factors->p);
    free (factors->e);
}

void mpfactor (mpz_t, struct mpfactors *);

void mpfactor_init (struct mpfactors *mpfactors) {
    mpfactors->p = malloc (1);
    mpfactors->e = malloc (1);
    mpfactors->nmpfactors = 0;
}

void mpfactor_clear (struct mpfactors *mpfactors) {
    int i;

    for (i = 0; i < mpfactors->nmpfactors; i++) {
        mpz_clear (mpfactors->p[i]);
    }

    free (mpfactors->p);
    free (mpfactors->e);
}

void mpfactor_insert (struct mpfactors *mpfactors, mpz_t prime) {
    long    nmpfactors  = mpfactors->nmpfactors;
    mpz_t         *p  = mpfactors->p;
    unsigned long *e  = mpfactors->e;
    long i, j;

    /* Locate position for insert new or increment e.  */
    for (i = nmpfactors - 1; i >= 0; i--) {
        if (mpz_cmp (p[i], prime) <= 0) {
            break;
        }
    }

    if (i < 0 || mpz_cmp (p[i], prime) != 0) {
        p = realloc (p, (nmpfactors + 1) * sizeof p[0]);
        e = realloc (e, (nmpfactors + 1) * sizeof e[0]);

        mpz_init (p[nmpfactors]);
        for (j = nmpfactors - 1; j > i; j--) {
            mpz_set (p[j + 1], p[j]);
            e[j + 1] = e[j];
        }
        mpz_set (p[i + 1], prime);
        e[i + 1] = 1;

        mpfactors->p = p;
        mpfactors->e = e;
        mpfactors->nmpfactors = nmpfactors + 1;
    } else {
        e[i] += 1;
    }
}

void mpfactor_insert_ui (struct mpfactors *mpfactors, unsigned long prime) {
    mpz_t pz;

    mpz_init_set_ui (pz, prime);
    mpfactor_insert (mpfactors, pz);
    mpz_clear (pz);
}

void mpfactor_using_division (mpz_t t, struct mpfactors *mpfactors) {
    mpz_t q;
    unsigned long int p;
    int i;

    mpz_init (q);

    p = mpz_scan1 (t, 0);
    mpz_div_2exp (t, t, p);
    while (p) {
        mpfactor_insert_ui (mpfactors, 2);
        --p;
    }

    p = 3;
    for (i = 1; i <= PRIMES_PTAB_ENTRIES;) {
        if (! mpz_divisible_ui_p (t, p)) {
            p += primes_diff[i++];
            if (mpz_cmp_ui (t, p * p) < 0) {
                break;
            }
        } else {
            mpz_tdiv_q_ui (t, t, p);
            mpfactor_insert_ui (mpfactors, p);
        }
    }

    mpz_clear (q);
}

static int mp_millerrabin (mpz_srcptr n, mpz_srcptr nm1, mpz_ptr x, mpz_ptr y,
        mpz_srcptr q, unsigned long int k) {
    unsigned long int i;

    mpz_powm (y, x, q, n);

    if (mpz_cmp_ui (y, 1) == 0 || mpz_cmp (y, nm1) == 0) {
        return 1;
    }

    for (i = 1; i < k; i++) {
        mpz_powm_ui (y, y, 2, n);
        if (mpz_cmp (y, nm1) == 0) {
            return 1;
        }
        if (mpz_cmp_ui (y, 1) == 0) {
            return 0;
        }
    }
    return 0;
}

int mp_prime_p (mpz_t n) {
    int k, r, is_prime;
    mpz_t q, a, nm1, tmp;
    struct mpfactors mpfactors;

    if (mpz_cmp_ui (n, 1) <= 0) {
        return 0;
    }

    /* We have already casted out small primes. */
    if (mpz_cmp_ui (n, (long) FIRST_OMITTED_PRIME * FIRST_OMITTED_PRIME) < 0) {
        return 1;
    }

    mpz_inits (q, a, nm1, tmp, NULL);

    /* Precomputation for Miller-Rabin.  */
    mpz_sub_ui (nm1, n, 1);

    /* Find q and k, where q is odd and n = 1 + 2**k * q.  */
    k = mpz_scan1 (nm1, 0);
    mpz_tdiv_q_2exp (q, nm1, k);

    mpz_set_ui (a, 2);

    /* Perform a Miller-Rabin test, finds most composites quickly.  */
    if (!mp_millerrabin (n, nm1, a, tmp, q, k)) {
        is_prime = 0;
        goto ret2;
    }

    /* Factor n-1 for Lucas.  */
    mpz_set (tmp, nm1);
    mpfactor (tmp, &mpfactors);

    /* Loop until Lucas proves our number prime, or Miller-Rabin proves our
       number composite.  */
    for (r = 0; r < PRIMES_PTAB_ENTRIES; r++) {
        int i;
  
        is_prime = 1;
        for (i = 0; i < mpfactors.nmpfactors && is_prime; i++) {
            mpz_divexact (tmp, nm1, mpfactors.p[i]);
            mpz_powm (tmp, a, tmp, n);
            is_prime = mpz_cmp_ui (tmp, 1) != 0;
        }
  
        if (is_prime) {
            goto ret1;
        }
  
        mpz_add_ui (a, a, primes_diff[r]);        /* Establish new base.  */
  
        if (!mp_millerrabin (n, nm1, a, tmp, q, k)) {
            is_prime = 0;
            goto ret1;
        }
    }

    fprintf (stderr, "Lucas prime test failure.  This should not happen\n");
    abort ();

  ret1:
    mpfactor_clear (&mpfactors);
  ret2:
    mpz_clears (q, a, nm1, tmp, NULL);

    return is_prime;
}

void mpfactor_using_pollard_rho (mpz_t n, unsigned long a,
        struct mpfactors *mpfactors) {
    mpz_t x, z, y, P;
    mpz_t t, t2;
    unsigned long long k, l, i;
  
    mpz_inits (t, t2, NULL);
    mpz_init_set_si (y, 2);
    mpz_init_set_si (x, 2);
    mpz_init_set_si (z, 2);
    mpz_init_set_ui (P, 1);
    k = 1;
    l = 1;
  
    while (mpz_cmp_ui (n, 1) != 0) {
        for (;;) {
            do {
                mpz_mul (t, x, x);
                mpz_mod (x, t, n);
                mpz_add_ui (x, x, a);
  
                mpz_sub (t, z, x);
                mpz_mul (t2, P, t);
                mpz_mod (P, t2, n);
  
                if (k % 32 == 1) {
                    mpz_gcd (t, P, n);
                    if (mpz_cmp_ui (t, 1) != 0) {
                        goto mpfactor_found;
                    }
                    mpz_set (y, x);
                }
            } while (--k != 0);
  
            mpz_set (z, x);
            k = l;
            l = 2 * l;
            for (i = 0; i < k; i++) {
                mpz_mul (t, x, x);
                mpz_mod (x, t, n);
                mpz_add_ui (x, x, a);
            }
            mpz_set (y, x);
        }
  
  mpfactor_found:
        do {
            mpz_mul (t, y, y);
            mpz_mod (y, t, n);
            mpz_add_ui (y, y, a);
  
            mpz_sub (t, z, y);
            mpz_gcd (t, t, n);
        } while (mpz_cmp_ui (t, 1) == 0);
  
        mpz_divexact (n, n, t); /* divide by t, before t is overwritten */
  
        if (!mp_prime_p (t)) {
            mpfactor_using_pollard_rho (t, a + 1, mpfactors);
        } else {
            mpfactor_insert (mpfactors, t);
        }
  
        if (mp_prime_p (n)) {
            mpfactor_insert (mpfactors, n);
            break;
        }
  
        mpz_mod (x, x, n);
        mpz_mod (z, z, n);
        mpz_mod (y, y, n);
    }
  
    mpz_clears (P, t2, t, z, x, y, NULL);
}

void mpfactor (mpz_t t, struct mpfactors *mpfactors) {
    mpfactor_init (mpfactors);

    if (mpz_sgn (t) != 0) {
        mpfactor_using_division (t, mpfactors);

        if (mpz_cmp_ui (t, 1) != 0) {
            if (mp_prime_p (t)) {
                mpfactor_insert (mpfactors, t);
            } else {
                mpfactor_using_pollard_rho (t, 1, mpfactors);
            }
        }
    }
}

void factor(n_t n, struct factors *factors) {
    mpz_t t;
    struct mpfactors mpfactors;

    mpz_init_set_ui(t, n);
    mpfactor(t, &mpfactors);

    factors->nfactors = mpfactors.nmpfactors;
    factors->p = malloc(factors->nfactors * sizeof(factors->p[0]));
    factors->e = malloc(factors->nfactors * sizeof(factors->e[0]));
    for (uint8_t i = 0; i < factors->nfactors; i++) {
        factors->p[i] = mpz_get_ui(mpfactors.p[i]);
        factors->e[i] = mpfactors.e[i];
    }
}


int n_pow(n_t n, n_t e) {
    int ret = 1;
    while (e) {
        if ((e & 1) == 1) {
            ret *= n;
        }
        n *= n;
        e >>= 1;
    }
    return ret;
}

void with_ab(n_t a, n_t a3, n_t b) {
    struct factors factors;
    n_t a3b3 = a3 + b * b * b;
    n_t div = 1;
    n_t rest = 4 * a3b3;

    factor(rest, &factors);

    int e_counter[factors.nfactors]; 
    for (int i = 0; i < factors.nfactors; i++) {
        e_counter[i] = 0;
    }

    for (;;) {
        for (int i = 0; i < factors.nfactors; i++) {
            if (e_counter[i] == factors.e[i]) {
                if (i == factors.nfactors - 1) {
                    goto e_counter_done;
                }
                n_t change = n_pow(factors.p[i], factors.e[i]);
                div /= change;
                rest *= change;
                e_counter[i] = 0;
            } else {
                div *= factors.p[i];
                rest /= factors.p[i];
                e_counter[i]++;
                break;
            }
        }

        if (div >= rest) {
            continue;
        }
        n_t div2 = div * div;
        if (rest < div2) {
            continue;
        }
        n_t trip_d2 = rest - div2;
        if (trip_d2 % 3 != 0) {
            continue;
        }
        n_t d2 = trip_d2 / 3;
        n_t d = round(sqrt((double)d2));
        if (d >= div) {
            continue;
        }
        if (d * d != d2) {
            continue;
        }
        n_t dub_c = div + d;
        if (dub_c % 2 == 1) continue;
        n_t c = dub_c / 2;
        if (c >= a) {
            continue;
        }
        n_t real_d = (div - d) / 2;
        printf("%lu,%lu,%lu,%lu\n", a, b, c, real_d);
    }
  e_counter_done:

    factor_clear(&factors);
}

void with_a(n_t a) {
    n_t a3 = a * a * a;
    for (n_t b = 1; b < a; b++) {
        with_ab(a, a3, b);
    }
}

int main (int argc, char *argv[]) {
    struct timeval tv1, tv2;

    gettimeofday(&tv1, NULL);
    for (int i = 1; i < argc; i++) {
        with_a(atoll(argv[i]));
    }
    gettimeofday(&tv2, NULL);
    fprintf(stderr, "%s: %fs\n", argv[1],
        (double)(tv2.tv_sec - tv1.tv_sec) +
        (double)(tv2.tv_usec - tv1.tv_usec) / 1000000);
}
