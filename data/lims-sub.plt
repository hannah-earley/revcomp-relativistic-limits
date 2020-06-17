preview=0

set logscale xy
unset xtics
unset ytics
set xr [4e-6:2.5e5]
set yr [5e-15:1e8]
unset key

limquant(l)=quantcoeff * ((l < thresh) ? l**3. : thresh**2*l)
limirrev(l)=irrevcoeff * l**2
limrev(l)=revcoeff * ((l < thresh) ? l**2.5 : thresh*l**1.5)

# max, ex arch, faster, more efficient, cooler, denser
array    threshes[6] = [1,1,1,1,1,1e-2]
array quantcoeffs[6] = [1e4,1e-1,1e2,1e-1,1e-1,1e-1]
array irrevcoeffs[6] = [1e-1,1e-8,1e-8,1e-4,1e-8,1e-8]
array   revcoeffs[6] = [1e2,1e-5,1e-5,1e-2,1e-2,1e-5]

if (preview) {
    set multiplot layout 3,3    
} else {
    set terminal tikz size 4cm,4cm
    set lmargin 0
    set rmargin 0
    set tmargin 0
    set bmargin 0
}

do for [i=1:6] {
    thresh=threshes[i]
    quantcoeff=quantcoeffs[i]
    irrevcoeff=irrevcoeffs[i]
    revcoeff=revcoeffs[i]

    if (!preview) {set output sprintf("lims-sub-%d.tex", i)}
    set arrow from thresh,graph 0 to thresh,graph 1 nohead dt 3
    plot limquant(x) lt 1 dt 1,\
         limirrev(x) lt 2 dt 1,\
         limrev(x)   lt 3 dt 1
    unset arrow
}


if (!preview) {
    set terminal tikz size 10cm,2cm
    set output "lims-sub-key.tex"
}
unset border
unset tics
set key inside right bottom Right nobox
plot NaN lt 1 dt 1 title "Quantum",\
     NaN lt 2 dt 1 title "Thermo (Irrev.)",\
     NaN lt 3 dt 1 title "Thermo (Rev.)"

if (preview) {pause -1}