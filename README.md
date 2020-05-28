# _pipenostics_ - _R_-package for diagnostics, reliability and predictive maintenance of pipeline systems


Introduction
----
The motivation for the package was to aggregate to some extent the separate knowledge about reliability, diagnostics
and predictive maintenance of pipeline systems suitable for pipe holders. Distributing such knowledge with _R_-package
seemed the most attractive option for white-collar engineers, having utilized spreadsheet software as a mainstream.

Aiming to avoid portability and accessibility problems made us search ways to restrict source code development by 
functionality of _base R_ only, eluding as long as possible any external packages. The package also appeares to 
promote [R-language](https://www.r-project.org/about.html) to those who want to survive in the belligerent environment of
_green snake lovers_.

ASME B31G
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
for determining the allowable length and maximum allowable working pressure. The _b31g*_ package functions reproduce
the idea of _CRVL.BAS_. They are natively vectorized, but remain simple without argument checks. Thus, use only physically
conditioned values as input to prevent erroneous and meaningless output.

The next usage of _b31gcrvl_ function imitates the output of _CRVL.BAS_
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



