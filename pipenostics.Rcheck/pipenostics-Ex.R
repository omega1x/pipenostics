pkgname <- "pipenostics"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "pipenostics-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('pipenostics')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("b31crvl")
### * b31crvl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31crvl
### Title: ASME B31G. Basic computer program CRVL.BAS
### Aliases: b31crvl

### ** Examples


## Further examples are inspired by those used in Appendix A of
## ASME B31G-1991 to verify correct entry of CRVL.BAS source code

## Example 1
b31crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)
#
# -- Calculated data --
# Intermediate factor (A) = 1.847
# Design pressure = 1093 PSI; Safe pressure = 1093 PSI
# Pipe may be operated safely at MAOP, 910 PSI
# With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
# With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000


## Example 2
b31crvl(maop = 400, d = 20, wth = .25, smys = 35000, def  = 0.5, depth = 0.18, l = 10)
#
# -- Calculated data --
# Intermediate factor (A) = 3.993
# Design pressure = 438 PSI; Safe pressure = 284 PSI
# Reduce operating pressure so it will not exceed 284 PSI, and so operate legally and safely
# With corrosion length 10.000 inch, maximum allowed corrosion depth is 0.0790 inch; A = 3.993
# With corrosion depth 0.180 inch, maximum allowed corrosion length is 2.0180 inch; A = 0.806


## Example 3
b31crvl(maop = 910, d = 24, wth = .432, smys = 52000, def  = .72, depth = 0.13, l = 30)
#
# -- Calculated data --
# Intermediate factor (A) = 8.320
# Design pressure = 1348 PSI; Safe pressure = 1037 PSI
# Pipe may be operated safely at MAOP, 910 PSI
# With corrosion length 30.000 inch, maximum allowed corrosion depth is 0.1670 inch; A = 8.320
# With corrosion depth 0.130 inch, maximum allowed corrosion length is Inf inch; A = 5.000


## Example 4
b31crvl(maop = 910, d = 24, wth = .432, smys = 52000, def  = .72, depth = .3, l = 30)
#
# -- Calculated data --
# Intermediate factor (A) = 8.320
# Design pressure = 1348 PSI; Safe pressure = 453 PSI
# Reduce operating pressure so it will not exceed 453 PSI, and so operate legally and safely
# With corrosion length 30.000 inch, maximum allowed corrosion depth is 0.1670 inch; A = 8.320
# With corrosion depth 0.300 inch, maximum allowed corrosion length is 12.8670 inch; A = 3.568


## Example 5
b31crvl(maop = 731, d = 24, wth = .281, smys = 52000, def  = 0.72, depth = 0.08, l = 15)
#
# -- Calculated data --
# Intermediate factor (A) = 5.158
# Design pressure = 877 PSI; Safe pressure = 690 PSI
# Reduce operating pressure so it will not exceed 690 PSI, and so operate legally and safely
# With corrosion length 15.000 inch, maximum allowed corrosion depth is 0.0680 inch; A = 5.158
# With corrosion depth 0.080 inch, maximum allowed corrosion length is 11.6340 inch; A = 4.000


## Example 6
b31crvl(maop = 1e3, d = 36, wth = .5, smys = 52000, def  = 0.72, depth = 0.41, l = 100)
# Alert! Corrosion depth exceeds 80 % of pipe wall! Pipe must be replaced!
# -- Calculated data --
# Intermediate factor (A) = 21.048
# Design pressure = 1040 PSI; Safe pressure = 206 PSI
# Repair or replace pipe because corrosion depth exceeds 80 % of pipe wall!
# Reduce operating pressure so it will not exceed 206 PSI, and so operate legally and safely
# With corrosion length 100.000 inch, maximum allowed corrosion depth is 0.0630 inch; A = 21.048
# With corrosion depth 0.410 inch, maximum allowed corrosion length is 2.5560 inch; A = 0.538
# But 0.410 inch exceeds allowable corrosion depth!!!


## Example 7
b31crvl(maop = 877, d = 12.625, wth = .5, smys = 35000, def  = .4, depth = .035, l = 3)
# Corrosion depth is less than 10 % of pipe wall. No resrictions on operation
# -- Calculated data --
# Intermediate factor (A) = 1.066
# Design pressure = 1109 PSI; Safe pressure = 1109 PSI
# Pipe may be operated safely at MAOP, 877 PSI
# With corrosion length 3.000 inch, maximum allowed corrosion depth is 0.4000 inch; A = 1.066
# With corrosion depth 0.035 inch, maximum allowed corrosion length is Inf inch; A = 5.000


## Example 8
b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .125, l = 12)
#
# -- Calculated data --
# Intermediate factor (A) = 3.093
# Design pressure = 875 PSI; Safe pressure = 845 PSI
# Pipe may be operated safely at MAOP, 790 PSI
# With corrosion length 12.000 inch, maximum allowed corrosion depth is 0.1790 inch; A = 3.093
# With corrosion depth 0.125 inch, maximum allowed corrosion length is 15.5190 inch; A = 4.000


## TEST #1
b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179, l = 12)
#
#-- Calculated data --
# Intermediate factor (A) = 3.093
# Design pressure = 875 PSI; Safe pressure = 791 PSI
# Pipe may be operated safely at MAOP, 790 PSI
# With corrosion length 12.000 inch, maximum allowed corrosion depth is 0.1790 inch; A = 3.093
# With corrosion depth 0.179 inch, maximum allowed corrosion length is 12.1820 inch; A = 3.140


## TEST #1A
b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179, l = 12.182)
#
# -- Calculated data --
# Intermediate factor (A) = 3.140
# Design pressure = 875 PSI; Safe pressure = 790 PSI
# Pipe may be operated safely at MAOP, 790 PSI
# With corrosion length 12.182 inch, maximum allowed corrosion depth is 0.1780 inch; A = 3.140
# With corrosion depth 0.179 inch, maximum allowed corrosion length is 12.1820 inch; A = 3.140


## TEST #1B
b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .180, l = 12.182)
#
# -- Calculated data --
# Intermediate factor (A) = 3.140
# Design pressure = 875 PSI; Safe pressure = 789 PSI
# Reduce operating pressure so it will not exceed 789 PSI, and so operate legally and safely
# With corrosion length 12.182 inch, maximum allowed corrosion depth is 0.1780 inch; A = 3.140
# With corrosion depth 0.180 inch, maximum allowed corrosion length is 11.9610 inch; A = 3.083


## TEST #2
b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179, l = 12.297)
#
# -- Calculated data --
# Intermediate factor (A) = 3.170
# Design pressure = 875 PSI; Safe pressure = 789 PSI
# Reduce operating pressure so it will not exceed 789 PSI, and so operate legally and safely
# With corrosion length 12.297 inch, maximum allowed corrosion depth is 0.1780 inch; A = 3.170
# With corrosion depth 0.179 inch, maximum allowed corrosion length is 12.1820 inch; A = 3.140


## All examples at once:
data(b31gdata)
examples <- with(b31gdata, b31crvl(maop, d, wth, smys, def, depth, l))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31crvl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gacd")
### * b31gacd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gacd
### Title: ASME B31G. Allowable corrosion depth in pipe
### Aliases: b31gacd

### ** Examples

 b31gacd(1093, 910, 30, .438, 7.5)
 # [1] 0.249  # [inch]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gacd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gacl")
### * b31gacl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gacl
### Title: ASME B31G. Allowable corrosion length in pipe
### Aliases: b31gacl

### ** Examples

 b31gacl(1093, 910, 30, .438, .1, 7.5)
 # [1] Inf  # [inch] - corrosion is low, no limit for the corroded area length

 b31gacl(438, 400, 20, .25, .18, 10)
 # [1] 2.018  # [inch] - finite allowed length of the corroded area





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gacl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gafr")
### * b31gafr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gafr
### Title: ASME B31G. A-factor
### Aliases: b31gafr

### ** Examples

 b31gafr(30, .438, 7.5)
 # [1] 1.847  # A-factor is less than 5, so the corrosion is not critical




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gafr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gdep")
### * b31gdep

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gdep
### Title: ASME B31G. Design pressure of pipe
### Aliases: b31gdep

### ** Examples

 b31gdep(30, .438, 52e3, .72)
 # [1] 1093.748  # [PSI]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gdep", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gmodpf")
### * b31gmodpf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gmodpf
### Title: ASME B31G. Failure pressure of the corroded pipe (modified)
### Aliases: b31gmodpf

### ** Examples

 ## Example: maximum percentage disparity of original B31G
 ## algorithm and modified B31G showed on CRVL.BAS data
 with(b31gdata, {
   original  <- b31gpf(d, wth, smys, depth, l)
   modified  <- b31gmodpf(d, wth, smys, depth, l)
   round(max(100*abs(1 - original/modified), na.rm = TRUE), 4)
 })
 ## Output:
 #[1] 32.6666

 ## Example: plot disparity of original B31G algorithm and
 ## modified B31G showed on CRVL data
 with(b31gdata[-(6:7),], {
   b31g  <- b31gpf(depth, wth, smys, depth, l)
   b31gmod  <- b31gmodpf(depth, wth, smys, depth, l)
   axe_range <- range(c(b31g, b31gmod))
   plot(b31g, b31g, type = 'b', pch = 16,
        xlab = 'Pressure, [PSI]',
        ylab = 'Pressure, [PSI]',
        main = 'Failure pressure method comparison',
        xlim = axe_range, ylim = axe_range)
   inc <- order(b31g)
   lines(b31g[inc], b31gmod[inc], type = 'b', col = 'red')
   legend('topleft',
          legend = c('B31G Original',
                     'B31G Modified'),
          col = c('black', 'red'),
          lty = 'solid')
 })




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gmodpf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gops")
### * b31gops

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gops
### Title: ASME B31G. Operational status of pipe
### Aliases: b31gops

### ** Examples

 b31gops(.438, .1)
 # [1] 2  # typical status for the most of pipes

 b31gops(.5, .41)
 # [1] 3  # alert! Corrosion depth is too high! Replace the pipe!




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gops", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gpf")
### * b31gpf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gpf
### Title: ASME B31G. Failure pressure of the corroded pipe (original)
### Aliases: b31gpf

### ** Examples

 ## Example: maximum percentage disparity of original B31G
 ## algorithm and modified B31G showed on CRVL.BAS data
 with(b31gdata, {
   original  <- b31gpf(d, wth, smys, depth, l)
   modified  <- b31gmodpf(d, wth, smys, depth, l)
   round(max(100*abs(1 - original/modified), na.rm = TRUE), 4)
 })
 ## Output:
 #[1] 32.6666

 ## Example: plot disparity of original B31G algorithm and
 ## modified B31G showed on CRVL data
 with(b31gdata[-(6:7),], {
   b31g  <- b31gpf(depth, wth, smys, depth, l)
   b31gmod  <- b31gmodpf(depth, wth, smys, depth, l)
   axe_range <- range(c(b31g, b31gmod))
   plot(b31g, b31g, type = 'b', pch = 16,
        xlab = 'Pressure, [PSI]',
        ylab = 'Pressure, [PSI]',
        main = 'Failure pressure method comparison',
        xlim = axe_range, ylim = axe_range)
   inc <- order(b31g)
   lines(b31g[inc], b31gmod[inc], type = 'b', col = 'red')
   legend('topleft',
          legend = c('B31G Original',
                     'B31G Modified'),
          col = c('black', 'red'),
          lty = 'solid')
 })





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gpf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("b31gsap")
### * b31gsap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: b31gsap
### Title: ASME B31G. Safe maximum pressure for the corroded area of pipe
### Aliases: b31gsap

### ** Examples

 b31gsap(1093, 30, .438, .1, 7.5)
 # [1] 1093  # [PSI], safe pressure is equal to design pressure

 b31gsap(877, 24, .281, .08, 15)
 # [1] 690   # [PSI], safe pressure is lower than design pressure due corrosion




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("b31gsap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dnvpf")
### * dnvpf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dnvpf
### Title: DNV-RP-F101. Failure pressure of the corroded pipe
### Aliases: dnvpf

### ** Examples


d     <- c(812.8, 219.0)  # [mm]
wth   <- c( 19.1,  14.5)  # [mm]
uts   <- c(530.9, 455.1)  # [N/mm^2]
l     <- c(203.2, 200.0)  # [mm]
depth <- c( 13.4,   9.0)  # [mm]

dnvpf(d, wth, uts, depth, l)
# [1] 15.86626 34.01183




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dnvpf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dropg")
### * dropg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dropg
### Title: Consumption drop in pipe
### Aliases: dropg

### ** Examples

# Let consider pipes according to network segment scheme depicted in figure
# in ?dropg help-page.

# Typical large diameters of pipes under consideration, [mm]:
d <- as.double(unique(subset(pipenostics::m325nhldata, diameter > 700)$diameter))

# Let sensor-measured consumption in the inlet of the pipe
# under consideration be proportional to d, [ton/hour]:
consumption <- .125*d

# Let consider total diameter case when total diameters of adjacent pipes are no
# more than d, [mm]:
adj <- c(450, -400, 950, -255, 1152)

# As at may be seen for the second and fourth cases they predominantly have
# recharges from network.
# Let calculate consumption on the outlet of the pipe under consideration,
# [ton/hour]

result <- consumption - dropg(adj, d, consumption)
print(result)

# [1]  75.96439 134.72222  65.70302 180.80580  78.05995

# For more clarity they may perform calculations in data.table:
## Not run: 
##D dataset <- data.table::data.table(adj, d, consumption)
##D print(dataset)
##D 
##D #     adj    d consumption
##D # 1:  450  800       100.0
##D # 2: -400  900       112.5
##D # 3:  950 1000       125.0
##D # 4: -255 1400       175.0
##D # 5: 1152 1200       150.0
##D 
##D dataset[, drop := dropg(adj, d, consumption)]  # calculate drop and recovery
##D dataset[, result := consumption - drop]
##D print(dataset)
##D 
##D #     adj    d consumption       drop    result
##D # 1:  450  800       100.0  24.035608  75.96439
##D # 2: -400  900       112.5 -22.222222 134.72222
##D # 3:  950 1000       125.0  59.296978  65.70302
##D # 4: -255 1400       175.0  -5.805804 180.80580
##D # 5: 1152 1200       150.0  71.940050  78.05995
## End(Not run)

# Now let consider particular diameter case with the same total diameters of
# adjacent pipes, [mm]:

adjp <- list(
  c(100, 175, 175, -65, 125, -60),  # diameters of 4 discharge pipes and 2 recharge pipes, [mm]
  c(-300, -100, -65, 125, -60),  # diameter of 1 discharge pipe and 4 recharge pipes, [mm]
  c(950),  # diameter of 1 discharge pipe, [mm]
  c(-255), # diameter of 1 recharge pipe, [mm]
  c(50, 70, 1000, 32)  # diameter of 4 discharge pipes, [mm]
)

stopifnot(
  all(sapply(adjp, sum) == adj)
)

# Recalculate the result:
result2 <- consumption - dropg(adjp, d, consumption)
stopifnot(
  all(result == result2)
)

# They may do it in data.table:
## Not run: 
##D dataset <- data.table::data.table(adjp, d, consumption)
##D print(dataset)
##D 
##D #                        adjp    d consumption
##D # 1:  100,175,175,-65,125,-60  800       100.0
##D # 2: -300,-100, -65, 125, -60  900       112.5
##D # 3:                      950 1000       125.0
##D # 4:                     -255 1400       175.0
##D # 5:        50,  70,1000,  32 1200       150.0
##D 
##D dataset[, drop := dropg(adj, d, consumption)]  # calculate drop and recovery
##D dataset[, result := consumption - drop]
##D print(dataset)
##D 
##D #                         adjp    d consumption       drop    result
##D # 1:  100,175,175,-65,125,-60  800       100.0  24.035608  75.96439
##D # 2: -300,-100, -65, 125, -60  900       112.5 -22.222222 134.72222
##D # 3:                      950 1000       125.0  59.296978  65.70302
##D # 4:                     -255 1400       175.0  -5.805804 180.80580
##D # 5:        50,  70,1000,  32 1200       150.0  71.940050  78.05995
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dropg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dropp")
### * dropp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dropp
### Title: Pressure drop in pipe
### Aliases: dropp

### ** Examples

# Typical pressure drop for horizontal pipeline segments
# in high-way heating network in Novosibirsk
dropp(len = c(200, 300))

#[1] 0.0007000666 0.0010500999




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dropp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dropt")
### * dropt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dropt
### Title: Temperature drop in pipe due heat losses
### Aliases: dropt

### ** Examples

 # Calculate normative temperature drop based on Minenergo-325 for pipe segment
 pipeline <- list(
   year = 1968,
   laying = "channel",
   d = 700,
   l = 1000
 )
 operation_temperature <- c(130, 150)  # [°C]

 foo <- dropt(
   temperature = operation_temperature,
   flux = do.call(
     m325nhl,
     c(pipeline, temperature = list(operation_temperature))
   )
 )

 foo
 # [1] 1.366806 1.433840

 # This is the same as using m325dropt:
 bar <- m325dropt(temperature = operation_temperature,
   year = 1968, laying = "channel", d = 700, len = 1000
 )

 bar
 # [1] 1.366806 1.433840




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dropt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flowls")
### * flowls

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flowls
### Title: List all possible flow paths in district heating network
### Aliases: flowls

### ** Examples

## No test: 
# Find path from A to B in trivial line topology:
flowls("A", "B")

# $B
# [1] 1

# More complex example with two terminal nodes D and E:
flowls(c("A", "B", "B"), c("B", "D", "E"))

#$D
#[1] 1 2
#
#$E
#[1] 1 3

# All possible flow paths in test bench illustrated in `?m325testbench`:
all_paths <- list(
  c(12, 13, 11, 8, 4, 1),  # hereinafter indexes of acceptor nodes
  c(12, 13, 11, 8, 4, 2),
  c(12, 13, 11, 8, 6, 5,  3),
  c(12, 13, 11, 8, 6, 7),
  c(12, 13, 11, 8, 6, 9),
  c(12, 13, 11, 10),
  c(12, 13, 14, 15),
  c(12, 13, 16, 17),
  c(12, 13, 16, 18, 20, 19),
  c(12, 13, 16, 18, 20, 21),
  c(12, 13, 16, 18, 22, 24),
  c(12, 13, 16, 18, 22, 25),
  c(12, 13, 16, 18, 20, 23, 26)
)

# find those paths:
path <- with(pipenostics::m325testbench, {
  flowls(sender, acceptor)
})

path[[4]]
# [1] 12 13 11  8  6  7

## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flowls", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("inch_mm")
### * inch_mm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: inch_mm
### Title: Millimeters to inches
### Aliases: inch_mm

### ** Examples

 inch_mm(c(25.4, 1))
 # [1] 1.00000000 0.03937008  # [inch]





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("inch_mm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("kgf_mpa")
### * kgf_mpa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: kgf_mpa
### Title: Megapascals to kilogram-force per square
### Aliases: kgf_mpa

### ** Examples

 kgf_mpa(c(0.0980665, 1))
 # [1]  1.00000 10.19716





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("kgf_mpa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m278hlair")
### * m278hlair

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m278hlair
### Title: Minenergo-278. Heat losses of overhead pipeline segment
### Aliases: m278hlair

### ** Examples

 m278hlair()
 # [1] 138.7736




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m278hlair", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m278hlcha")
### * m278hlcha

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m278hlcha
### Title: Minenergo-278. Heat losses of pipeline segment in channel
### Aliases: m278hlcha

### ** Examples

 m278hlcha()
 #

 ## Naive way to find out technical state (factors k1 and k2) for pipe
 ## segments constructed in 1980:
   optim(
     par = c(1.5, 1.5),
     fn = function(x) {
     # functional to optimize
       abs(
           m278hlcha(k1 = x[1], k2 = x[2]) -
           m325nhl(year = 1980, laying = "channel", d = 250, temperature = 110)
       )
     },
     method = "L-BFGS-B",
     lower = 1.01, upper = 4.4
   )$par
   # [1] 4.285442 4.323628




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m278hlcha", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m278hlund")
### * m278hlund

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m278hlund
### Title: Minenergo-278. Heat losses of underground pipeline segment
### Aliases: m278hlund

### ** Examples

 m278hlund()
 # [1] 102.6226




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m278hlund", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m278inshcm")
### * m278inshcm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m278inshcm
### Title: Minenergo-278. Thermal conductivity of pipe insulation materials
### Aliases: m278inshcm

### ** Examples


# Averaged thermal conductivity of pipe insulation at 110 °C
print(m278insdata)
head(m278inshcm(110, m278insdata[["material"]]))
# [1] 0.09600 0.07525 0.14950 0.14325 0.14950 0.10800

# Terms for linear connection between thermal conductivity of unknown
# (averaged) pipe insulator vs temperature:
temperature <- as.double(1:450)
lambda_ins <- with(m278insdata, {
  vapply(temperature, function(x) mean(m278inshcm(x, material)), .1)
})
C <- coef(lsfit(temperature, lambda_ins))  # c(Intercept, X)
stopifnot(
  all(abs(C - c(7.963590e-02, 9.730769e-05)) < 1e-8)
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m278inshcm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m325beta")
### * m325beta

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m325beta
### Title: Minenergo-325. Local heat loss coefficient
### Aliases: m325beta

### ** Examples

norms <- within(m325nhldata, {
  beta <- m325beta(laying, as.double(diameter))
})
unique(norms$beta)
# [1] 1.15 1.20




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m325beta", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m325dropt")
### * m325dropt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m325dropt
### Title: Minenergo-325. Temperature drop in pipe due heat losses
### Aliases: m325dropt

### ** Examples

 stopifnot(
   round(
     m325dropt(
       temperature = 130, year = 1968, laying = "channel", d = 700, l = 1000
     ), 2) == 1.37
 )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m325dropt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m325nhl")
### * m325nhl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m325nhl
### Title: Minenergo-325. Normative heat losses of pipe
### Aliases: m325nhl

### ** Examples


 with(m325nhldata, {

 ## Linear extrapolation adopted in Minenergo's Order 325 using last two points:
 temperature <- seq(0, 270, 10)  # [°C]
 flux <- m325nhl(1980, "underground", TRUE, 0, 73, temperature)  # [kcal/m/h]
 plot(temperature, flux, type = "b")

 ## Consider heat losses of fittings:
 stopifnot(
   ## when beta becomes 1.15
   all(
     round(
       m325nhl(1980, "underground", d = 73, temperature = 65,
               beta = c(FALSE, TRUE)),
       3
     ) == c(65.500, 75.325)
   ),

   ## when beta becomes 1.2
   all(
     round(
       m325nhl(2000, "channel", d = 73, temperature = 65,
               beta = c(FALSE, TRUE)),
       3
     ) == c(17.533, 21.040)
   )
 )
})





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m325nhl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m325testbench")
### * m325testbench

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m325testbench
### Title: Minenergo-325. Test bench of district heating network
### Aliases: m325testbench
### Keywords: datasets

### ** Examples

## Not run: 
##D # Do not hesitate to use data.table for larger chunks of network:
##D library(data.table)
##D setDT(m325testbench)
##D 
##D # Check for declared topology isomorphism:
##D stopifnot(
##D   all(!duplicated(m325testbench$acceptor))
##D )
##D 
##D # Do all terminal nodes have sensor-measured regime parameters?:
##D terminal_nodes <- m325testbench[!(acceptor %in% sender)]
##D stopifnot(
##D   all(!is.na(terminal_nodes[, .(temperature, pressure, consumption)]))
##D )
##D 
##D # Welcome to use igraph for topology investigations:
##D library(igraph)
##D g <- graph_from_data_frame(
##D # provide edge list with edge attributes:
##D   d = m325testbench[,
##D                     .SD,
##D                     .SDcols = !c("temperature", "pressure", "consumption", "inlet", "outlet")
##D   ],
##D 
##D   # provide node attributes:
##D   v = rbind(
##D     # attributes for zero sender
##D     list(
##D       acceptor = 0, elevation = NA_real_, temperature = NA_real_,
##D       pressure = NA_real_, consumption= NA_real_
##D     ),
##D     m325testbench[,
##D                   .(
##D                     # Since every row describes node with its incoming edge:
##D                     acceptor, elevation = outlet,
##D                     # sensor readings (if any):
##D                     temperature, pressure, consumption
##D                   )
##D     ]
##D   )
##D )
##D 
##D plot(g, layout = layout_as_tree(g, root = 1))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m325testbench", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m325tracebw")
### * m325tracebw

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m325tracebw
### Title: Minenergo-325. Trace backwards thermal-hydraulic regime for
###   district heating network
### Aliases: m325tracebw

### ** Examples

# It is possible to run without specification of argument values:
m325tracebw()

# Get isomorphic representation of district heating network graph:
nx <- pipenostics::m325testbench
nx$d <- 1e3*nx$d  # convert [m] to [mm]

# When tracing large network graphs put screen log to file
output <- do.call("m325tracebw", c(as.list(nx), verbose = TRUE))

# Distinct options for opinion aggregation lead to distinct traced
# temperature and pressure:
output <- list(
  mean   = do.call("m325tracebw",
                   c(as.list(nx), verbose = FALSE, opinion = "mean")),
  median = do.call("m325tracebw",
                   c(as.list(nx), verbose = FALSE, opinion = "median"))
)

stopifnot(
  round(
    subset(
      output$mean,
      node == 13 & aggregation == "median",
      c("temperature", "pressure", "consumption")
    ) - subset(
      output$median,
      node == 13 & aggregation == "median",
      c("temperature", "pressure", "consumption")
    ),
    5
    # difference between aggregation options
  ) == c(dt = 0.03732, dp = 0.00139, dg = 0)
)

# Do not hesitate to use along with data.table...
## Not run: 
##D data.table::setDT(nx)
##D result <-
##D   nx[, m325tracebw(sender, acceptor, temperature, pressure, consumption, d, len,
##D                    year, insulation, laying, beta, exp5k, roughness, inlet,
##D                    outlet, method = "romeo", verbose = TRUE, opinion = "median",
##D                    csv = FALSE, file = "m325tracebw.csv")]
##D 
##D # ...but mind that the obtained result should go to independent data.frame:
##D stopifnot(is.data.frame(result) & !is.data.table(result))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m325tracebw", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m325tracefw")
### * m325tracefw

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m325tracefw
### Title: Minenergo-325. Trace forwards thermal-hydraulic regime for
###   district heating network
### Aliases: m325tracefw

### ** Examples

## No test: 
# Minimum two nodes should be in district heating network graph:
m325tracefw(verbose = FALSE)

#   node  trace backward aggregation temperature  pressure consumption job
# 1    1 sensor    FALSE    identity    70.00000 0.5883990          20   0
# 2    2      1    FALSE    identity    69.71603 0.5813153          20   1

# Example with the test bench:
nx <- pipenostics::m325testbench

# avoid using numeric identifiers for nodes:
nx$sender <- paste0("N", nx$sender)
nx$acceptor <- paste0("N", nx$acceptor)

# Alter units:
nx$d <- 1e3 * nx$d  # convert [m] to [mm]

# Perform backward tracing to get regime on root node:
bw_report <- do.call("m325tracebw", c(as.list(nx), verbose = FALSE))

# Put the traced values to the root node of the test bench:
root_node_idx <- 12
root_node <- paste0("N", root_node_idx)
regime_param  <- c("temperature", "pressure", "consumption")
nx[root_node_idx, regime_param] <-
  subset(bw_report,
         node == root_node & aggregation == "median",
         regime_param)
rm(root_node, root_node_idx)

# Trace the test bench forward for the first time:
fw_report <- do.call("m325tracefw",
                     c(as.list(nx), verbose = FALSE, elev_tol = .5))

# Let's compare traced regime at terminal nodes back to test bench:
report <- subset(
  rbind(bw_report, fw_report),
  node %in% subset(nx, !(acceptor %in% sender))$acceptor &
    aggregation == "identity"
)

regime_delta <- colMeans(
  subset(report, backward, regime_param) -
    subset(report, !backward, regime_param)
)
print(regime_delta)
# temperature      pressure   consumption
# -4.640201e-01 -5.208802e-03 -5.465713e-16

stopifnot(sqrt(regime_delta %*% regime_delta) < 0.5)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m325tracefw", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("m325traceline")
### * m325traceline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: m325traceline
### Title: Minenergo-325. Trace thermal-hydraulic regime for linear segment
### Aliases: m325traceline

### ** Examples

# Consider 4-segment tracing path depicted in ?m325regtrace help page.
# First, let sensor readings for forward tracing:
t_fw <- 130  # [°C]
p_fw <- .588399*all.equal(.588399, mpa_kgf(6))  # [MPa]
g_fw <- 250  # [ton/hour]

# Let discharges to network for each pipeline segment are somehow determined as
discharges <- seq(0, 30, 10)  # [ton/hour]

## No test: 
# Then the calculated regime (red squares) for forward tracing is
regime_fw <- m325traceline(t_fw, p_fw, g_fw, discharges, forward = TRUE)
print(regime_fw)

# $temperature
# [1] 129.1799 128.4269 127.9628 127.3367
#
# $pressure
# [1] 0.5878607 0.5874226 0.5872143 0.5870330
#
# $consumption
# [1] 250 240 220 190
## End(No test)
# Next consider values of traced regime as sensor readings for backward tracing:
t_bw <- 127.3367  # [°C]
p_bw <- .5870330  # [MPa]
g_bw <- 190  # [ton/hour]

# Then the calculated regime (red squares) for backward tracing is
## No test: 
regime_bw <- m325traceline(t_bw, p_bw, g_bw, discharges, forward = FALSE)
print(regime_bw)

# $temperature
# [1] 129.9953 129.1769 128.4254 127.9619
#
# $pressure
# [1] 0.5883998 0.5878611 0.5874228 0.5872144
#
# $consumption
# [1] 250 250 240 220

# Let compare sensor readings with backward tracing results:
tracing <- with(regime_bw, {
  lambda <- function(val, constraint)
    c(val, constraint, constraint - val,
      abs(constraint - val)*100/constraint)
  first <- 1
  structure(
    rbind(
      lambda(temperature[first], t_fw),
      lambda(pressure[first],    p_fw),
      lambda(consumption[first], g_fw)
    ),
    dimnames = list(
      c("temperature", "pressure", "consumption"),
      c("sensor.value", "traced.value", "abs.discr", "rel.discr")
    )
  )
})
print(tracing)

# sensor.value traced.value     abs.discr    rel.discr
# temperature   130.000000  129.9952943  4.705723e-03 0.0036197868
# pressure        0.588399    0.5883998 -8.290938e-07 0.0001409067
# consumption   250.000000  250.0000000  0.000000e+00 0.0000000000
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("m325traceline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mepof")
### * mepof

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mepof
### Title: Probability of failure of the corroded pipe within maximum
###   entropy
### Aliases: mepof

### ** Examples

## No test: 
# Let's consider a pipe in district heating network with
diameter           <- 762         # [mm]
wall_thickness     <-  10         # [mm]
UTS                <- 434.3697    # [MPa]

# which transfers heat-carrier (water) at
operating_pressure <-   0.588399  # [MPa].
temperature        <-  95         # [°C]

# During inline inspection four corroded areas (defects) are detected with:
depth  <- c(2.45,  7.86,   7.93,   8.15)  # [mm]

# whereas the length of all defects is not greater 200 mm:
length <- rep(200, 4)  # [mm]

# Corrosion rates in radial and in longitudinal directions are not well-known and
# may vary in range .01 - .30 mm/year:
rar = function(n) stats::runif(n, .01, .30) / 365
ral = function(n) stats::runif(n, .01, .30) / 365

# Then POFs related to each corroded area are near:
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv")
print(pof)
# 0.000000 0.252510 0.368275 0.771595

# So, the POF of the pipe is near
print(max(pof))
# 0.771595

# The value of POF changes in time. So, in a year after inline inspection of
# the pipe we can get something near
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = 365)
print(pof)
# 0.000000 0.525539 0.648359 0.929099

# for entire pipe we get something near:
print(max(pof))
# 0.929099

# Two years ago before inline inspection the pipe state was rather good:
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = -2 * 365)

print(pof)
# 0.000000 0.040780 0.072923 0.271751

# for entire pipe we get something near:
print(max(pof))
# 0.271751


## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mepof", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mm_inch")
### * mm_inch

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mm_inch
### Title: Inches to mm
### Aliases: mm_inch

### ** Examples

 mm_inch(c(0.03937008, 1))
 # [1]  1.0 25.4  # [mm]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mm_inch", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mpa_kgf")
### * mpa_kgf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mpa_kgf
### Title: Kilogram-force per square cm to megapascals
### Aliases: mpa_kgf

### ** Examples


 mpa_kgf(c(10.1971619998, 1))
 # [1] 1.0000000 0.0980665#'




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mpa_kgf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mpa_psi")
### * mpa_psi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mpa_psi
### Title: Pounds per square inch to megapascals
### Aliases: mpa_psi

### ** Examples

 mpa_psi(c(145.03773800721814, 1))
 # [1] 1.000000000 0.006894757 # [MPa]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mpa_psi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pcorrcpf")
### * pcorrcpf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pcorrcpf
### Title: PCORRC. Failure pressure of the corroded pipe
### Aliases: pcorrcpf

### ** Examples


d     <- c(812.8, 219.0)  # [mm]
wth   <- c( 19.1,  14.5)  # [mm]
uts   <- c(530.9, 455.1)   # [N/mm^2]
l     <- c(203.2, 200.0)  # [mm]
depth <- c( 13.4,   9.0)   # [mm]

pcorrcpf(d, wth, uts, depth, l)
# [1] 16.35449 33.01288




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pcorrcpf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("psi_mpa")
### * psi_mpa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: psi_mpa
### Title: Megapascals to pounds per square inch
### Aliases: psi_mpa

### ** Examples

 psi_mpa(c(6.89475728e-3, 1))
 # [1] 1.0000 145.0377 # [PSI]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("psi_mpa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("shell92pf")
### * shell92pf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: shell92pf
### Title: Shell92. Failure pressure of the corroded pipe
### Aliases: shell92pf

### ** Examples


d     = c(812.8, 219.0)  # [mm]
wth   = c( 19.1,  14.5)  # [mm]
uts  = c(530.9, 455.1)   # [N/mm^2]
l     = c(203.2, 200.0)  # [mm]
depth = c( 13.4,   9.0)  # [mm]

shell92pf(d, wth, uts, depth, l)
# [1] 11.09262 25.27286




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("shell92pf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("strderate")
### * strderate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: strderate
### Title: DNV-RP-F101. De-rate yield stress and tensile strength of pipe
###   due to temperature
### Aliases: strderate

### ** Examples

with(api5l3t, {
print(strderate(mpa_psi(smys), 53))
print(
  strderate(mpa_psi(uts),seq(0, 250, length.out = length(smys)))
)
})
# [1] 170.5689 205.0427 239.5165 287.7798 315.3588 356.7274 384.3064 411.8854 446.3592 480.8330
# [11] 549.7806
# [1] 310.2641 330.9483 413.6854 398.6854 404.3697 415.0540 439.5278 457.1068 460.8963 485.3701
# [11] 530.5282





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("strderate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
