#include <h>

-- The true time of a full moon may differ from this
-- approximation by up to about 14.5 hours as a result of
-- the non-circularity of the moon's orbit.
--
-- The first moon of the year 2000 corresponds to n = 0.
--
-- This gives the number of days in UT.
daysToMoonN :: Double -> Double
daysToMoonN n =
    20.362955 + 29.530588861 * n + 102.026e-12 * n^2 +
    (-0.000739) - 235e-12 * n^2
