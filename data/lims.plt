set terminal tikz size 10cm,8cm
set output "lims.tex"
# set terminal qt

# set title "Limits"
set xlabel "Linear Dimension"
set ylabel "Rate" offset -1,0
set logscale xy
unset xtics
unset ytics
set key left top Left reverse #box

thresh=sqrt(8./9.)
ratio=100.
powlim(l)=(l < thresh) ? l**2.5 : thresh*l**1.5
masslim(l)=ratio * ((l < thresh) ? l**3. : thresh**2*l)

set xr [4e-6:2.5e5]
set yr [5e-15:1e8]

plot 'lims2.dat' using 1:3 lt 1           title "Quantum" w lines,\
              masslim(x)   lt 1 dt 2      title "Quantum$^\\dagger$",\
              '' using 1:5 lt 2           title "Thermo (Irrev.)" w lines,\
              '' using 1:2 lt 3           title "Thermo (Rev.)" w lines,\
              powlim(x)    lt 3 dt 2      title "Thermo (Rev.)$^\\dagger$",\
              '' using 1:4 lt 6 dt 3      title "Thermo (Rev.)$^\\ddagger$" w lines,\
              '' using 1:6 lt 4 lw 3 dt 4 title "Net Bound" w lines
# plot powlim(x) #, masslim(x)
# pause -1