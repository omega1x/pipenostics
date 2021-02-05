
# b31gpf   [PSI] d [inch], wth [inch], smys [PSI], depth [inch], l [inch]
# b31modpf [PSI] d [inch], wth [inch], smys [PSI], depth [inch], l [inch]
# dnvpf    [MPa] d [mm]  , wth [mm]  , uts [MPa], depth [mm]  , l [mm]
# prorrcpf [MPa] d [mm]  , wth [mm]  , uts  [MPa], depth [mm]  , l [mm]
# shell92pf[MPa] d [mm]  , wth [mm]  , uts  [MPa], depth [mm]  , l [mm]

# args: depth, l, d, wth, strength, p, t, k = 0..1


lsprob <- function(depth, l, d, wth, strength, p, t, k, method) {
  # no argument check for speed:
  # k is a single number
  # method is a single character
  strength <- strderate(strength, t)
  pf <- switch(method,
               b31g    = mpa_psi(b31gpf(inch_mm(d), inch_mm(wth),
                                        psi_mpa(strength), inch_mm(depth),
                                        inch_mm(l))),
               b31gmod = mpa_psi(b31gmodpf(inch_mm(d), inch_mm(wth),
                                           psi_mpa(strength), inch_mm(depth),
                                           inch_mm(l))),
               dnv     = dnvpf(d, wth, strength, depth, l),
               pcorrc  = pcorrcpf(d, wth, strength, depth, l),
               shell92 = shell92pf(d, wth, strength, depth, l)
  )
  # limit state function
  lsf <- 1 - pmax(depth/(wth*k), p/pf)  # 2021 © Possokhov

  # limit state probability
  sum(lsf[lsf > 0])/length(lsf)
}