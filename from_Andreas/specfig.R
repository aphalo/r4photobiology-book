fig_sun <- ggplot(data=sun.data, aes(x=w.length, y=s.e.irrad)) + geom_line()
fig_sun
uvb <- energy_irradiance(sun.data$w.length,sun.data$s.e.irrad,UVB())
uva <- energy_irradiance(sun.data$w.length,sun.data$s.e.irrad,UVA())
par <- photon_irradiance(sun.data$w.length,sun.data$s.e.irrad,PAR()) * 1e6
fig_sun2 <- fig_sun + 
  annotate_waveband(UVB(), "rect", ymax=0.82) +
  annotate_waveband(UVB(), "text", label=paste("UVB ",round(uvb,digits=1)," W m⁻²"), y=0.7, size=5, angle=90, colour="black") +
  annotate_waveband(UVA(), "rect", ymax=0.82) +
  annotate_waveband(UVA(), "text", label=paste("UVA ",round(uva,digits=1)," W m⁻²"), y=0.86, size=5, colour="black") +
  annotate_waveband(PAR(), "rect", ymax=0.82) +
  annotate_waveband(PAR(), "text", label=paste("PAR ",round(par,digits=0)," µmol m⁻² s⁻¹"), y=0.86, size=5, colour="black")
fig_sun2
