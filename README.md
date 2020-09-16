# _pipenostics_ - _R_-package for diagnostics, reliability and predictive maintenance of pipeline systems
[![Build Status](https://travis-ci.com/omega1x/pipenostics.svg?branch=master)](https://travis-ci.com/omega1x/pipenostics)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Introduction
----
The motivation for the package was to aggregate to some extent the separate knowledge about reliability, diagnostics
and predictive maintenance of pipeline systems suitable for pipe holders. Distributing such knowledge with _R_-package
seemed the most attractive option for white-collar engineers, having utilized spreadsheet software as a mainstream.

Aiming to avoid portability and accessibility problems made us search ways to restrict source code development by 
functionality of _base R_ only, eluding as long as possible any external packages. 

The next values describing technological conditions, material properties of pipe and defect parameters are used as arguments
throughout the most functions concerning corrosion diagnostics:

- `maop` - maximum allowable operating pressure - [MAOP](https://en.wikipedia.org/wiki/Maximum_allowable_operating_pressure),
  [[PSI](https://en.wikipedia.org/wiki/Pounds_per_square_inch)]
- `d` - nominal outside diameter of the pipe, [[inch](https://en.wikipedia.org/wiki/Inch)]
- `wth` - nominal wall thickness of the pipe, [[inch](https://en.wikipedia.org/wiki/Inch)]
- `smys` - specified minimum yield of stress ([SMYS](https://en.wikipedia.org/wiki/Specified_minimum_yield_strength))
   as a characteristics of steel strength, [[PSI](https://en.wikipedia.org/wiki/Pounds_per_square_inch)]
- `depth` - measured maximum depth of the corroded area, [[inch](https://en.wikipedia.org/wiki/Inch)]
- `l` - measured maximum longitudial length of the corroded area, [[inch](https://en.wikipedia.org/wiki/Inch)]


ASME B31G-1991
----
It is recognized by pipeline companies that some sections of high pressure pipelines particularly those installed
a number of years ago, have experienced some corrosion. Where corrosion is found, pipeline operators have been deeply
concerned about the need for a method of determining the remaining strength of these corroded areas. If the corrosion
does not penetrate the pipe wall, what is the pressure containing capability of the remaining pipe metal in terms of its
ability to continue to operate safely at the maximum allowable operating pressure (_MAOP_) of the pipeline system?

Thus, one of the needs of the pipeline industry has been a procedure that will help operators, particularly field personnel,
make decisions on existing pipelines, when exposed for any purpose, as to whether any corroded region may be left in
service or whether it needs to be repaired or replaced. Such detenninations must be based upon sound research and extensive
testing in order to provide safe and conservative guidelines on which to base field decisions.

The [ASME B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf) provides procedures to assist
in this determination.

_Appendix A_ to [ASME B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf) shows the source code
for determining the allowable length and maximum allowable working pressure. 
The _b31g*_ and _crvl_ functions reproduce the idea of _CRVL.BAS_. They are natively vectorized, but 
remain simple without argument checks. Thus, use only physically
conditioned values as input to prevent erroneous and meaningless output.

The next usage of _crvl_ function imitates the output of _CRVL.BAS_
```
## Example 1
crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)

-- Calculated data --
Intermediate factor (A) = 1.847
Design pressure = 1093 PSI; Safe pressure = 1093 PSI
Pipe may be operated safely at MAOP, 910 PSI
With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000
```
For further details and usage examples see package documentation.


ASME B31G-2012
----
An effort was undertaken to update the [ASME B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf) up to
[ASME B31G-2012](https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines)
document to recognize certain other corrosion evaluation methods that have proven sound and that have seen successful
use in the pipeline industry. Incorporation of these other methods provides us with a formalized framework within
which to use such methodologies.

Nevertheless, to preserve simplicity of traditional inline measurements during inspections we only consider _Analysis Level 1_.
As noted in [ASME B31G-2012](https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines), _Level 1_ valuation is quite suitable for use in prioritizing corrosion defects identified by inline inspection.

TODO
-----

- program pf-function for Shell92
- program pf-function for PCORRC
- pf-functions that operate with [N/mm^2] pressure units 
  should operate with [MPa]
- describe api5l3t - table:
  https://law.resource.org/pub/us/cfr/ibr/002/api.5l.2004.pdf
- findout functions for critical corrosion depth
- program first dignostic function using JCGM, MC and Timashev


  


