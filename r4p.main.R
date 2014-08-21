
## ----setup, include=FALSE, cache=FALSE-----------------------------------
opts_knit$set(child.command = 'include')
opts_knit$set(self.contained=FALSE)
opts_knit$set(concordance=TRUE)
opts_chunk$set(fig.path='figure/pos-', fig.align='center', fig.show='hold', size="footnotesize", dev='cairo_pdf', cache=TRUE)
opts_chunk$set(tidy=FALSE)
options(replace.assign=TRUE,width=50)


## ----fig-setup, include=FALSE, cache=FALSE-------------------------------
opts_fig_wide <- list(fig.width=8, fig.height=4, out.width='.95\\textwidth')
opts_fig_wide_square <- list(fig.width=6, fig.height=6, out.width='.95\\textwidth')
opts_fig_narrow <- list(fig.width=4, fig.height=4, out.width='.47\\textwidth')
opts_fig_very_narrow <- list(fig.width=3, fig.height=3, out.width='.32\\textwidth')
opts_fig_medium <- list(fig.width=6, fig.height=4, out.width='.64\\textwidth')
opts_chunk$set(opts_fig_narrow)


## ----eval=FALSE, include=FALSE-------------------------------------------
## opts_knit$get()
## search()


## ----own-set-up, echo=FALSE, include=FALSE, cache=FALSE------------------
incl_chaps <- TRUE
incl_ckbk <- FALSE
incl_apdx <- FALSE


## ----child-intro, child='introduction.Rnw', eval=incl_chaps--------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-optics, child='optics.Rnw', eval=incl_chaps-------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-photochemistry, child='photochemistry.Rnw', eval=incl_chaps----

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-software, child='software.Rnw', eval=incl_chaps---------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-suite, child='suite.Rnw', eval=incl_chaps---------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## setRepositories(graphics = getOption("menu.graphics"),
##                 ind = NULL,
##                 addURLs = c(r4photo =
##                       "http://www.mv.helsinki.fi/aphalo/R"))


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## setRepositories(graphics = getOption("menu.graphics"),
##                 ind = c(1, 6),
##                 addURLs = c(r4photo =
##                       "http://www.mv.helsinki.fi/aphalo/R"))


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## utils::setRepositories(graphics = getOption("menu.graphics"),
##                 ind = c(1, 6),
##                 addURLs = c(r4photo =
##                       "http://www.mv.helsinki.fi/aphalo/R"))


## ----eval=FALSE----------------------------------------------------------
## setRepositories()


## ----eval=FALSE----------------------------------------------------------
## install.packages(c("photobiologyAll", "photobiologygg"))


## ----eval=FALSE----------------------------------------------------------
## update.packages()


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## install.packages("photobiology",
##                  repos = "http://www.mv.helsinki.fi/aphalo/R")


## ----eval=FALSE----------------------------------------------------------
## update.packages(repos = "http://www.mv.helsinki.fi/aphalo/R")


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## install.packages(c("photobiologyAll", "photobiologygg"),
##          repos = c(r4photo =
##                      "http://www.mv.helsinki.fi/aphalo/R",
##                    CRAN =
##                      "http://cran.rstudio.com"))


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## photobiology_packages <- c("photobiology",
##     "photobiologyWavebands",
##     "photobiologyCry", "photobiologyPhy",
##     "photobiologyLamps", "photobiologyLEDs",
##     "photobiologySun", "photobiologygg",
##     "photobiologyFilters",  "photobiologySensors")
## 
## install.packages(photobiology_packages,
##          repos = c(r4photo =
##                      "http://www.mv.helsinki.fi/aphalo/R",
##                    CRAN =
##                      "http://cran.rstudio.com"))


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## update.packages(repos =
##                   c(r4photo =
##                       "http://www.mv.helsinki.fi/aphalo/R",
##                     CRAN =
##                       "http://cran.rstudio.com"))


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## install.packages("photobiology",
##                  repos = "http://www.mv.helsinki.fi/aphalo/R",
##                  type="source")




## ----child-uwirrad, child='physics.Rnw', eval=incl_ckbk------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(ggplot2)
library(photobiologygg)
library(photobiology)
library(photobiologyFilters)


## ----eval=TRUE, cache=FALSE, include=FALSE-------------------------------
opts_chunk$set(opts_fig_medium)


## ----tidy=FALSE----------------------------------------------------------
h <- 6.626e-34 # J s-1
c <- 2.998e8 # m s-1
kB <- 1.381e-23 # J K-1
black_body_spectrum <- function(w.length, Tabs) {
  w.length <- w.length * 1e-9 # nm -> m
  ((2 * h * c^2) / w.length^5) *
    1 / (exp((h * c / (kB * Tabs * w.length))) - 1)
}


## ------------------------------------------------------------------------
black_body_spectrum(500, 5000)


## ------------------------------------------------------------------------
black_body_spectrum(c(300,400,500), 5000)


## ------------------------------------------------------------------------
black_body_spectrum(500, c(4500,5000))


## ------------------------------------------------------------------------
black_body_spectrum(c(500, 500, 600, 600), c(4500,5000)) # tricky!


## ----tidy=FALSE----------------------------------------------------------
ggplot(data=data.frame(x=c(50,1500)), aes(x)) +
  stat_function(fun=black_body_spectrum,
                args = list(Tabs=5600),
                colour="blue") +
  stat_function(fun=black_body_spectrum,
                args = list(Tabs=4500),
                colour="orange") +
  stat_function(fun=black_body_spectrum,
                args = list(Tabs=3700),
                colour="red") +
  labs(y=expression(Spectral~~radiance~~(W~sr^-1~m^-3)),
       x="Wavelength (nm)")


## ----tidy=FALSE----------------------------------------------------------
k.wein <- 2.8977721e6 # nm K
black_body_peak_wl <- function(Tabs) {
  k.wein / Tabs
}


## ----tidy=FALSE----------------------------------------------------------
ggplot(data=data.frame(Tabs=c(2000,7000)), aes(x=Tabs)) +
  stat_function(fun=black_body_peak_wl) +
  labs(x="Temperature (K)",
       y="Wavelength at peak of emission (nm)")


## ------------------------------------------------------------------------
try(detach(package:photobiologyFilters))
try(detach(package:photobiologygg))
try(detach(package:photobiology))
try(detach(package:ggplot2))



## ----child-uwirrad, child='astronomy.Rnw', eval=incl_ckbk----------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(photobiology)
library(lubridate)
library(ggplot2)
library(ggmap)


## ------------------------------------------------------------------------
ymd("20140320")
ymd("2014-03-20")
ymd("14-03-20")
ymd("2014-3-20")
ymd("2014/3/20")
dmy("20032014")
mdy("03202014")


## ----astro-geocode-1, message=FALSE, cache=TRUE--------------------------
geocode("Helsinki")
geocode("Viikinkaari 1, 00790 Helsinki, Finland")


## ----astro-geocode-2, message=FALSE, cache=TRUE--------------------------
geo_code_BA <- geocode("Buenos Aires")
geo_code_BA
day_night(ymd("2013-12-21"),
          lon = geo_code_BA[["lon"]],
          lat = geo_code_BA[["lat"]],
          tz="America/Argentina/Buenos_Aires")
day_night(ymd("2013-06-21"),
          lon = geo_code_BA[["lon"]],
          lat = geo_code_BA[["lat"]],
          tz="America/Argentina/Buenos_Aires")


## ----astro-geocode-3, message=FALSE, cache=TRUE--------------------------
geo_code_Mu <- geocode("Munich")
geo_code_Mu
day_night(ymd("2013-12-21"),
          lon = geo_code_Mu[["lon"]],
          lat = geo_code_Mu[["lat"]],
          tz="Europe/Berlin")
day_night(ymd("2013-06-21"),
          lon = geo_code_Mu[["lon"]],
          lat = geo_code_Mu[["lat"]],
          tz="Europe/Berlin")


## ----astro-geocode-4, message=FALSE, cache=TRUE--------------------------
geo_code_He <- geocode("Helsinki")
geo_code_He
day_night(ymd("2013-09-21"),
          lon = geo_code_He[["lon"]], lat = geo_code_He[["lat"]])
day_night(ymd("2013-09-21"),
          lon = geo_code_He[["lon"]], lat = geo_code_He[["lat"]],
          twilight="civil")
day_night(ymd("2013-09-21"),
          lon = geo_code_He[["lon"]], lat = geo_code_He[["lat"]],
          twilight="nautical")
day_night(ymd("2013-09-21"),
          lon = geo_code_He[["lon"]], lat = geo_code_He[["lat"]],
          twilight="astronomical")


## ----astro-geocode-5, message=FALSE, cache=TRUE--------------------------
geo_code_Jo <- geocode("Joensuu")
geo_code_Jo
my_time <- ymd_hms("2014-05-29 18:00:00", tz="EET")
sun_angles(my_time,
         lon = geo_code_Jo[["lon"]], lat = geo_code_Jo[["lat"]])


## ------------------------------------------------------------------------
sun_angles(now(),
         lon = geo_code_Jo[["lon"]], lat = geo_code_Jo[["lat"]])


## ------------------------------------------------------------------------
opts_chunk$set(opts_fig_wide)


## ----astro-6, cache=TRUE-------------------------------------------------
hours <- seq(from=ymd("2014-06-23", tz="EET"),
             by="10 min",
             length=24 * 6)
elevations <- sun_angles(hours,
          lon = geo_code_He[["lon"]],
          lat = geo_code_He[["lat"]])$elevation
sun_elev_hel <- data.frame(time_eet = hours,
                            elevation = elevations,
                            location = "Helsinki",
                            lon = geo_code_He[["lon"]],
                           lat = geo_code_He[["lat"]])


## ----astro-7, cache=TRUE-------------------------------------------------
twilight <-
  data.frame(angle = c(0, -6, -12, -18),
             label = c("Horizon", "Civil twilight",
                       "Nautical twilight",
                       "Astronomical twilight"),
             time = rep(ymd_hms("2014-06-23 12:00:00",
                                tz="EET"),
                        4) )


## ----astro-8-------------------------------------------------------------
ggplot(sun_elev_hel,
       aes(x = time_eet, y = elevation)) +
  geom_line() +
  geom_hline(data=twilight,
             aes(yintercept = angle, linetype=factor(label))) +
  annotate(geom="text",
           x=twilight$time, y=twilight$angle,
           label=twilight$label, vjust=-0.4, size=4) +
  labs(y = "Solar elevation at Helsinki (degrees)",
       x = "Time EEST")


## ----astro-9, cache=TRUE-------------------------------------------------
days <- seq(from=ymd("2014-01-01"), to=ymd("2014-12-31"),
            by="3 day")


## ----astro-geocode-10, message=FALSE, cache=TRUE-------------------------
len_days <- length(days)
photoperiods <- numeric(len_days)
geo_code_He <- geocode("Helsinki")
for (i in 1:len_days) {
  day_night.ls <- day_night(days[i],
                            lon = geo_code_He[["lon"]],
                            lat = geo_code_He[["lat"]],
                            tz="EET")
  photoperiods[i] <-
    as.numeric(day_night.ls[["daylength"]],
               units="hours")
}
daylengths_hel <-
  data.frame(day = days,
             daylength = photoperiods,
             location="Helsinki",
             lon = geo_code_He[["lon"]],
             lat = geo_code_He[["lat"]])
geo_code_Iv <- geocode("Ivalo")
for (i in 1:len_days) {
  day_night.ls <- day_night(days[i],
                            lon = geo_code_Iv[["lon"]],
                            lat = geo_code_Iv[["lat"]],
                            tz="EET")
  photoperiods[i] <-
    as.numeric(day_night.ls[["daylength"]],
               units="hours")
}
daylengths_ivalo <-
  data.frame(day = days,
             daylength = photoperiods,
             location="Ivalo",
             lon = geo_code_Iv[["lon"]],
             lat = geo_code_Iv[["lat"]])
geo_code_At <- geocode("Athens, Greece")
for (i in 1:len_days) {
  day_night.ls <- day_night(days[i],
                            lon = geo_code_At[["lon"]],
                            lat = geo_code_At[["lat"]],
                            tz="EET")
  photoperiods[i] <-
    as.numeric(day_night.ls[["daylength"]],
               units="hours")
}
daylengths_athens <-
  data.frame(day = days,
             daylength = photoperiods,
             location="Athens",
             lon = geo_code_At[["lon"]],
             lat = geo_code_At[["lat"]])

daylengths <- rbind(daylengths_hel,
                    daylengths_ivalo,
                    daylengths_athens)


## ----astro-11------------------------------------------------------------
ggplot(daylengths,
       aes(x = day, y = daylength, colour=factor(location))) +
  geom_line() +
  scale_y_continuous(breaks=c(0,6,12,18,24), limits=c(0,24)) +
  labs(x = "Date", y = "Daylength (h)", colour="Location")


## ------------------------------------------------------------------------
try(detach(package:photobiology))
try(detach(package:lubridate))
try(detach(package:ggmap))
try(detach(package:ggplot2))



## ----child-uwirrad, child='baseoper.Rnw', eval=incl_ckbk-----------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(photobiologyFilters)
library(photobiologyLEDs)


## ------------------------------------------------------------------------
my_sun.spct <- sun.data
setSourceSpct(my_sun.spct)


## ------------------------------------------------------------------------
class(my_sun.spct)
is(my_sun.spct, "source.spct")


## ------------------------------------------------------------------------
filtered_sun.spct <- sun.spct * gg400.spct
filtered_sun.spct


## ------------------------------------------------------------------------
filtered_uncoated_sun.spct <- sun.spct * gg400.spct * (100 - 9) / 100
filtered_uncoated_sun.spct


## ------------------------------------------------------------------------
sun.spct
sun.spct * 2


## ------------------------------------------------------------------------
filtered_sun.spct <- ug1.spct * sun.spct
filtered_sun.spct


## ----eval=FALSE----------------------------------------------------------
## # not run
## with(sun.data, s.e.irrad^2 / w.length)


## ----eval=FALSE----------------------------------------------------------
## # not run
## sun.dt[ , s.e.irrad^2 / w.length]


## ------------------------------------------------------------------------
# run
my_sun.dt <- copy(sun.dt)
my_sun.dt[ , result := s.e.irrad^2 / w.length]


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## out1.dt <- sum_spectra(spc1$w.length, spc2$w.length,
##                        spc1$s.e.irrad, spc2$s.e.irrad)


## ------------------------------------------------------------------------
out2.spct <- sun.spct + sun.spct
out3.spct <- e2q(sun.spct + sun.spct)
out3.spct


## ----eval=FALSE, tidy=FALSE----------------------------------------------
## out.data <- oper_spectra(spc1$w.length, spc2$w.length,
##                          spc1$s.e.irrad, spc2$s.e.irrad,
##                          bin.oper=`^`)


## ------------------------------------------------------------------------
trim_spct(my_sun.spct, UV())
trim_spct(my_sun.spct, UV(), fill=0)
trim_spct(my_sun.spct, low.limit=400)
trim_spct(my_sun.spct, low.limit=250, fill=0.0)


## ----eval=FALSE----------------------------------------------------------
## # not run
## with(sun.data,
##      trim_tails(w.length, s.e.irrad,
##                 low.limit=300))
## with(sun.data,
##      trim_tails(w.length, s.e.irrad,
##                 low.limit=300, fill=NULL))
## with(sun.data,
##      trim_tails(w.length, s.e.irrad,
##                 low.limit=300, fill=NA))
## with(sun.data,
##      trim_tails(w.length, s.e.irrad,
##                 low.limit=300, fill=0.0))


## ----eval=FALSE----------------------------------------------------------
## # not run
## with(sun.data,
##      trim_tails(w.length, s.e.irrad,
##                 low.limit=300, high.limit=1000))
## with(sun.data,
##      trim_tails(w.length, s.e.irrad,
##                 low.limit=300, high.limit=1000, fill=NA))
## with(sun.data,
##      trim_tails(w.length, s.e.irrad,
##                 low.limit=300, high.limit=1000, fill=0.0))


## ------------------------------------------------------------------------
as_quantum_mol(550, 200) * 1e6


## ------------------------------------------------------------------------
head(sun.data$s.e.irrad, 10)
s.q.irrad <- with(sun.data,
                  as_quantum_mol(w.length, s.e.irrad))
head(s.q.irrad, 10)


## ------------------------------------------------------------------------
sun.spct
my_sun.spct <- copy(sun.spct)
e2q(my_sun.spct)


## ------------------------------------------------------------------------
sun.spct
my_sun.spct <- copy(sun.spct)
e2q(my_sun.spct, "replace")
my_sun.spct


## ------------------------------------------------------------------------
as_energy(600, 1) * 1e-3
as_energy(300, 1) * 1e-3


## ------------------------------------------------------------------------
s.e.irrad <- with(sun.data, as_energy(w.length, s.q.irrad))


## ------------------------------------------------------------------------
sun.spct
my_sun.spct <- copy(sun.spct)
q2e(my_sun.spct, "replace")


## ------------------------------------------------------------------------
sun.spct
my_sun.spct <- copy(sun.spct)
my_sun.spct[ , s.q.irrad := NULL]


## ------------------------------------------------------------------------
sun.spct
my_sun.spct <- copy(sun.spct)
my_sun.spct[ , s.e.irrad := NULL]


## ------------------------------------------------------------------------
q2e(my_sun.spct, byref=TRUE)
my_sun.spct


## ------------------------------------------------------------------------
interpolate_spct(sun.spct, seq(290, 300, by=0.1))
interpolate_spct(sun.spct, seq(290, 300, by=0.1), fill=0.0)


## ------------------------------------------------------------------------
with(sun.dt,
     interpolate_spectrum(w.length, s.e.irrad, 290:300))
with(sun.dt,
     interpolate_spectrum(w.length, s.e.irrad, 290:300, fill=0.0))


## ------------------------------------------------------------------------
try(detach(package:photobiologyFilters))
try(detach(package:photobiologyLEDs))
try(detach(package:photobiologyWavebands))
try(detach(package:photobiology))



## ----child-uwirrad, child='uwirrad.Rnw', eval=incl_ckbk------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     energy_irradiance(w.length, s.e.irrad,
                       new_waveband(400, 700)))


## ------------------------------------------------------------------------
with(sun.data,
     energy_irradiance(w.length, s.e.irrad, PAR()))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     energy_irradiance(w.length, s.e.irrad))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     energy_irradiance(w.length, s.e.irrad,
                       new_waveband(700,1000)))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     energy_irradiance(w.length, s.e.irrad,
                       new_waveband(100,200)))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     photon_irradiance(w.length, s.e.irrad, PAR()))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     photon_irradiance(w.length, s.e.irrad, PAR())) * 1e6


## ------------------------------------------------------------------------
with(sun.data,
     energy_irradiance(w.length, s.q.irrad,
                       PAR()), unit.in="photon")


## ------------------------------------------------------------------------
with(sun.data,
     photon_irradiance(w.length, s.q.irrad,
                       PAR()), unit.in="photon")


## ------------------------------------------------------------------------
with(sun.data,
     photon_irradiance(w.length, s.e.irrad,
                       list(Red(), Green(), Blue()))) * 1e6
Q.RGB <- with(sun.data,
     photon_irradiance(w.length, s.e.irrad,
                       list(Red(), Green(), Blue()))) * 1e6
signif(Q.RGB, 3)
Q.RGB[1]
Q.RGB["Green.ISO"]


## ------------------------------------------------------------------------
with(sun.data,
     photon_irradiance(w.length, s.e.irrad,
                       list(R=Red(), G=Green(), B=Blue()))) * 1e6


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     photon_irradiance(w.length, s.e.irrad,
                       list(UVB=UVB()))) * 1e6


## ------------------------------------------------------------------------
uvb <- UVB()
uvb


## ------------------------------------------------------------------------
red <- Red()
red
min(red)
max(red)
range(red)
midpoint(red)
labels(red)
color(red)


## ------------------------------------------------------------------------
UVB()
UVB("ISO")
UVB("CIE")
UVB("medical")
UVB("none")


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     photon_irradiance(w.length, s.e.irrad, UVB("ISO"))) * 1e6
with(sun.data,
     photon_irradiance(w.length, s.e.irrad, UVB("none"))) * 1e6


## ----tidy=FALSE----------------------------------------------------------
wb1 <- new_waveband(500,600)
wb1
with(sun.data,
     photon_irradiance(w.length, s.e.irrad, wb1)) * 1e6
wb2 <- new_waveband(500,600, wb.name="my.colour")
wb2
with(sun.data,
     photon_irradiance(w.length, s.e.irrad, wb2)) * 1e6


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     photon_ratio(w.length, s.e.irrad,
                  Red("Smith"), Far_red("Smith")))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     R_FR_ratio(w.length, s.e.irrad))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     energy_ratio(w.length, s.e.irrad, UVB(), PAR()))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     energy_ratio(w.length, s.e.irrad,
                  Red("Smith"), Far_red("Smith")))


## ----tidy=FALSE----------------------------------------------------------
with(sun.data,
     photons_energy_ratio(w.length, s.e.irrad, PAR()))


## ------------------------------------------------------------------------
with(sun.data,
     photons_energy_ratio(w.length, s.e.irrad, PAR())) * 1e6


## ------------------------------------------------------------------------
with(sun.data,
     split_energy_irradiance(w.length, s.e.irrad,
                             c(400, 500, 600, 700)))


## ------------------------------------------------------------------------
with(sun.data,
     sum(split_energy_irradiance(w.length, s.e.irrad,
                                 c(400, 500, 600, 700))))
with(sun.data,
     energy_irradiance(w.length, s.e.irrad, PAR()))


## ------------------------------------------------------------------------
with(sun.data,
     split_energy_irradiance(w.length, s.e.irrad,
                             c(400, 500, 600, 700),
                             scale="relative"))


## ------------------------------------------------------------------------
with(sun.data,
     split_energy_irradiance(w.length, s.e.irrad,
                             c(400, 500, 600, 700),
                             scale="percent"))


## ------------------------------------------------------------------------
with(sun.data,
     split_energy_irradiance(w.length, s.e.irrad,
                             c(400,500,600,700),
                             scale="percent"))


## ------------------------------------------------------------------------
with(sun.data,
     split_energy_irradiance(w.length, s.e.irrad,
                             c(400,500,600),
                             scale="percent"))


## ------------------------------------------------------------------------
with(sun.data,
     split_energy_irradiance(w.length, s.e.irrad,
                             c(400, 700),
                             scale="percent"))


## ------------------------------------------------------------------------
with(sun.data,
     split_energy_irradiance(w.length, s.e.irrad,
                             c(400, 700)))


## ------------------------------------------------------------------------
with(sun.data,
     split_photon_irradiance(w.length, s.e.irrad,
                             c(400, 500, 600, 700),
                             scale="percent"))


## ------------------------------------------------------------------------
detach(package:photobiologyWavebands)
detach(package:photobiology)



## ----child-wtirrad, child='wtirrad.Rnw', eval=incl_ckbk------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)


## ------------------------------------------------------------------------
with(sun.data,
     energy_irradiance(w.length, s.e.irrad, GEN.G()))


## ------------------------------------------------------------------------
range(GEN.G())
with(sun.data,
     energy_irradiance(w.length, s.e.irrad, GEN.G(280)))


## ------------------------------------------------------------------------
GEN.G()
GEN.G(300)
GEN.G(280)


## ------------------------------------------------------------------------
cie <- CIE()
cie


## ------------------------------------------------------------------------
min(cie)
max(cie)
range(cie)
midpoint(cie)
normalization(cie)
labels(cie)
color(cie)


## ----tidy=FALSE----------------------------------------------------------
toy.wb <- new_waveband(400, 700, "SWF",
                       SWF.e.fun=function(wl){(wl - 400)^2},
                       norm=550, SWF.norm=550,
                       wb.name="TOY")
toy.wb
with(sun.data,
     energy_irradiance(w.length, s.e.irrad, toy.wb))
with(sun.data,
     photon_irradiance(w.length, s.e.irrad, toy.wb))


## ------------------------------------------------------------------------
detach(package:photobiologyWavebands)
detach(package:photobiology)




## ----child-wtirrad, child='transmittance.Rnw', eval=incl_ckbk------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(photobiologyFilters)
library(photobiologyLEDs)


## ----eval=FALSE----------------------------------------------------------
## tau <- spc_above / spc_below


## ----eval=FALSE----------------------------------------------------------
## my_T2A <- function(x) {-log10(x)}


## ----eval=FALSE----------------------------------------------------------
## my_A2T <- function(x) {10^-x}


## ------------------------------------------------------------------------
my_gg400.spct <- copy(gg400.spct)
my_gg400.spct[ , A := T2A(Tfr)]
my_gg400.spct


## ------------------------------------------------------------------------
T2A(gg400.spct)
a.gg400.spct <- T2A(gg400.spct, action="replace")


## ------------------------------------------------------------------------
A2T(a.gg400.spct)
A2T(a.gg400.spct, action="replace")


## ------------------------------------------------------------------------
class(sun.spct)
class(gg400.spct)


## ----cache=FALSE---------------------------------------------------------
filtered_sun.spct <- sun.spct * gg400.spct
class(filtered_sun.spct)
filtered_sun.spct


## ----cache=FALSE---------------------------------------------------------
q_irrad_spct(sun.spct, UV()) * 1e6
q_irrad_spct(sun.spct * gg400.spct, UV()) * 1e6
q_irrad_spct(sun.spct * ug1.spct, UV()) * 1e6


## ----cache=FALSE---------------------------------------------------------
q_irrad_spct(sun.spct * gg400.spct) * 1e6
q_irrad_spct(sun.spct * gg400.spct, new_waveband(min(sun.spct), max(sun.spct))) * 1e6


## ------------------------------------------------------------------------
# not working
my_luminaire <- (0.5 * Norlux_B.spct + Norlux_R.spct) * PLX0A000_XT.spct
my_luminaire
# works fine
my_luminaire <- (Norlux_B.spct * 0.5 + Norlux_R.spct) * PLX0A000_XT.spct
my_luminaire

q_ratio_spct(my_luminaire, list(Red(), Blue(), Green()), PAR())
q_irrad_spct(my_luminaire, list(PAR(), Red(), Blue(), Green())) * 1e6


## ------------------------------------------------------------------------
try(detach(package:photobiologyFilters))
try(detach(package:photobiologyLEDs))
try(detach(package:photobiologyWavebands))
try(detach(package:photobiology))




## ----child-colour, child='colour.Rnw', eval=incl_ckbk--------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(photobiology)


## ------------------------------------------------------------------------
w_length2rgb(550) # green
w_length2rgb(630) # red
w_length2rgb(380) # UVA
w_length2rgb(750) # far red
w_length2rgb(c(550, 630, 380, 750)) # vectorized


## ------------------------------------------------------------------------
w_length_range2rgb(c(400,700))
w_length_range2rgb(400:700)
w_length_range2rgb(sun.data$w.length)
w_length_range2rgb(550)


## ------------------------------------------------------------------------
with(sun.data,
     s_e_irrad2rgb(w.length, s.e.irrad))
with(sun.data,
     s_e_irrad2rgb(w.length, s.e.irrad, sens=ciexyzCMF2.spct))
with(sun.data,
     s_e_irrad2rgb(w.length, s.e.irrad, sens=ciexyzCMF10.spct))
with(sun.data,
     s_e_irrad2rgb(w.length, s.e.irrad, sens=ciexyzCC2.spct))
with(sun.data,
     s_e_irrad2rgb(w.length, s.e.irrad, sens=ciexyzCC10.spct))


## ------------------------------------------------------------------------
wl <- c(390, 829)

my.colors <- w_length2rgb(wl[1]:wl[2])

colCount <- 40 # number per row
rowCount <- trunc(length(my.colors) / colCount)

plot( c(1,colCount), c(0,rowCount), type="n",
      ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0))
title(paste("RGB colours for",
            as.character(wl[1]), "to",
            as.character(wl[2]), "nm"))

for (j in 0:(rowCount-1))
{
  base <- j*colCount
  remaining <- length(my.colors) - base
  RowSize <-
    ifelse(remaining < colCount, remaining, colCount)
  rect((1:RowSize)-0.5, j-0.5, (1:RowSize)+0.5, j+0.5,
       border="black",
       col=my.colors[base + (1:RowSize)])
}



## ------------------------------------------------------------------------
try(detach(package:photobiology))



## ----child-photoreceptors, child='photoreceptors.Rnw', eval=incl_ckbk----

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-sources, child='sources.Rnw', eval=incl_ckbk------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ----eval=FALSE----------------------------------------------------------
## library(photobiology)
## library(photobiologySun)
## library(photobiologyLamps)
## library(photobiologyLEDs)
## library(photobiologyWavebands)
## library(ggplot2)
## library(ggtern)
## library(photobiologygg)




## ----child-filters, child='filters.Rnw', eval=incl_ckbk------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ----eval=FALSE----------------------------------------------------------
## library(photobiology)
## library(photobiologyFilters)
## library(photobiologySun)
## library(photobiologyLamps)
## library(photobiologyLEDs)
## library(photobiologyWavebands)
## library(ggplot2)
## library(ggtern)
## library(photobiologygg)




## ----child-plots, child='plots.Rnw', eval=incl_ckbk----------------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(ggplot2)
library(ggtern)
library(gridExtra)
library(photobiology)
library(photobiologyFilters)
library(photobiologyWavebands)
library(photobiologygg)


## ----echo=FALSE, include=FALSE, cache=FALSE------------------------------
opts_chunk$set(opts_fig_wide)
opts_chunk$set(cache=TRUE)


## ----sun-e-0-------------------------------------------------------------
fig_sun.e <-
  ggplot(data=sun.spct, aes(x=w.length, y=s.e.irrad)) +
  geom_line()  +
  labs(
    y = expression(Spectral~~energy~~irradiance~~(W~m^{-2}~nm^{-1})),
    x = "Wavelength (nm)")
fig_sun.e


## ----axis-labs-----------------------------------------------------------
ylab_watt <-
  expression(Spectral~~energy~~irradiance~~(W~m^{-2}~nm^{-1}))
ylab_watt_atop <-
  expression(atop(Spectral~~energy~~irradiance,
                  (W~m^{-2}~nm^{-1})))
ylab_umol <-
  expression(Spectral~~photon~~irradiance~~(mu*mol~m^{-2}~s^{-1}~nm^{-1}))
ylab_umol_atop <-
  expression(atop(Spectral~~photon~~irradiance,
                  (mu*mol~m^{-2}~s^{-1}~nm^{-1})))


## ----plot-fig-sun-e-1----------------------------------------------------
fig_sun.e + scale_y_log10(limits=c(1e-3, 1e0))


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide_square)


## ----theme-stack---------------------------------------------------------
theme_stack_opts <-
  list(theme(axis.text.x = element_blank(),
             axis.ticks = element_blank(),
             axis.title.x = element_blank()))
num_one_dec <- function(x, ...)
  format(x, nsmall=1, trim=FALSE, width=4, ...)
fig_sun.q <-
  ggplot(data=sun.spct, aes(x=w.length, y=s.q.irrad * 1e6)) +
  geom_line() +
  scale_y_continuous(labels = num_one_dec) +
  labs(y = ylab_umol_atop) +
  theme_stack_opts
fig_sun.e1 <- fig_sun.e +
  labs(y = ylab_watt_atop,
       x = "Wavelength (nm)")
grid.arrange(fig_sun.q, fig_sun.e1, nrow=2)


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide)


## ----peaks-1-------------------------------------------------------------
head(with(sun.spct,
          get_peaks(w.length, s.e.irrad, span=31)))
head(with(sun.spct,
          get_peaks(w.length, s.e.irrad, span=31,
                    ignore_threshold=0.75)))


## ----peaks-2-------------------------------------------------------------
head(with(sun.spct,
          get_peaks(w.length, s.e.irrad, span=21)))
head(with(sun.spct,
          get_peaks(w.length, s.e.irrad, span=51)))


## ----valleys-1-----------------------------------------------------------
head(with(sun.spct,
          get_valleys(w.length, s.e.irrad, span=51)))
head(with(sun.spct,
          get_valleys(w.length, s.e.irrad, span=51,
                      ignore_threshold=0.5)))


## ----fig-sun-e-2---------------------------------------------------------
fig_sun.e + stat_peaks(span=31)


## ----fig-sun-e-3---------------------------------------------------------
fig_sun.e + stat_peaks(colour="red", span=31) +
            stat_valleys(colour="blue", span=51)


## ----fig-sun-e-4---------------------------------------------------------
fig_sun.e +
  stat_peaks(colour="red", geom="point",
             shape="|", size=6, span=31)


## ----fig-sun-e-5---------------------------------------------------------
fig_sun.e +
  stat_peaks(colour="red", geom="point", shape=23,
             fill="white", size=3, span=31) +
  stat_peaks(colour="red", vjust=-1, span=31) +
  expand_limits(y=0.9)


## ----fig-sun-e-6---------------------------------------------------------
fig_sun.e +
  stat_peaks(angle=90, hjust=-0.5, colour="red", span=31) +
  stat_valleys(angle=90, hjust=1, color="blue", span=51) +
  expand_limits(y=1.0)


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide)


## ----figvl-1-------------------------------------------------------------
figvl <- fig_sun.e + annotate_waveband(PAR(), "rect", ymax=0.82) +
                     annotate_waveband(PAR(), "text", y=0.86)
figvl + theme_bw()


## ----figvl-2-------------------------------------------------------------
figvl <- fig_sun.e + annotate_waveband(Yellow(), "rect", ymax=0.82) +
                     annotate_waveband(Yellow(), "text", y=0.86)
figvl + theme_bw()


## ----figv2---------------------------------------------------------------
figv2 <- fig_sun.e +
  annotate_waveband(UVC(), "rect",
                    ymax=0.82) +
  annotate_waveband(UVC(), "text",
                    y=0.86) +
  annotate_waveband(UVB(), "rect",
                    ymax=0.82) +
  annotate_waveband(UVB(), "text",
                    y=0.80, angle=90, hjust=1) +
  annotate_waveband(UVA(), "rect",
                    ymax=0.82) +
  annotate_waveband(UVA(), "text",
                    y=0.86) +
  annotate_waveband(Blue("Sellaro"), "rect",
                    ymax=0.82) +
  annotate_waveband(Blue("Sellaro"), "text",
                    y=0.5, angle=90, hjust=1) +
  annotate_waveband(Green("Sellaro"), "rect",
                    ymax=0.82) +
  annotate_waveband(Green("Sellaro"), "text",
                    y=0.50, angle=90, hjust=1) +
  annotate_waveband(Red(), "rect",
                    ymax=0.82) +
  annotate_waveband(Red(), "text",
                    y=0.86) +
  annotate_waveband(Red("Smith"), "rect",
                    ymax=0.82) +
  annotate_waveband(Red("Smith"), "text",
                    y=0.80, angle=90, hjust=1) +
  annotate_waveband(Far_red("Smith"), "rect",
                    ymax=0.82) +
  annotate_waveband(Far_red("Smith"), "text",
                    y=0.80, angle=90, hjust=1)
figv2 + theme_bw()


## ----figvl-3-------------------------------------------------------------
figvl3 <- fig_sun.q +
  geom_vline(xintercept=range(PAR()))
figvl3


## ----figvl-4-------------------------------------------------------------
figvl4 <- fig_sun.q +
  geom_vline(xintercept=range(PAR()), linetype="dashed") +
  annotate_waveband(PAR(), "text", y=1.4, size=10, colour="black")
figvl4


## ----sun-0---------------------------------------------------------------
fig_sun <- ggplot(data=sun.spct,
                  aes(x=w.length, y=s.e.irrad)) +
  geom_line() +
  labs(y = ylab_watt,
       x = "Wavelength (nm)")
uvb <- e_irrad_spct(sun.spct, UVB())
uva <- e_irrad_spct(sun.spct, UVA())
par <- q_irrad_spct(sun.spct, PAR()) * 1e6
fig_sun2 <- fig_sun +
  annotate_waveband(UVB(), "rect", ymax=0.82) +
  annotate_waveband(UVB(), "text",
                    label=paste("UVB:~", signif(uvb, digits=2),
                                "*~W~m^{-2}~nm^{-1}", sep=""),
                    y=0.2, hjust=0, angle=90, colour="black",
                    parse=TRUE) +
  annotate_waveband(UVA(), "rect", ymax=0.82) +
  annotate_waveband(UVA(), "text",
                    label=paste("UVA:~", signif(uva, digits=2),
                                "*~W~m^{-2}~nm^{-1}", sep=""),
                    y=0.86, colour="black", parse=TRUE) +
  annotate_waveband(PAR(), "rect", ymax=0.82) +
  annotate_waveband(PAR(), "text",
                    label=paste("PAR:~", signif(par,digits=2),
                                "*~mu*mol~m^{-2}~s^{-1}", sep=""),
                    y=0.86, colour="black", parse=TRUE)
fig_sun2 + theme_bw()


## ----fig-dsun, warning=FALSE---------------------------------------------
fig_dsun <- ggplot(data=sun.daily.spct * polythene.new.spct,
                   aes(x=w.length, y=s.e.irrad * 1e-3)) + geom_line() +
  geom_line(data=sun.daily.spct * polyester.new.spct, colour="red") +
  geom_line(data=sun.daily.spct * PC.spct, colour="blue") +
  labs(y = expression(Spectral~~energy~~exposure~~(kJ~m^{-2}~d^{-1}~nm^{-1})),
       x = "Wavelength (nm)") + xlim(290, 425) + ylim(0, 25)
cie.pe <- e_irrad_spct(sun.daily.spct * polythene.new.spct, CIE()) * 1e-3
cie.ps <- e_irrad_spct(sun.daily.spct * polyester.new.spct, CIE()) * 1e-3
cie.pc <- e_irrad_spct(sun.daily.spct * PC.spct, CIE()) * 1e-3
y.pos = 22.5
fig_dsun2 <- fig_dsun +
  annotate("text",
           label=paste("Polythene~~filter~~CIE:~",
                       signif(cie.pe, digits=3),
                       "*~kJ~m^{-2}~d^{-1}", sep=""),
                    y=y.pos+2, x=300, hjust=0, colour="black",
           parse=TRUE) +
  annotate("text", label=paste("Polyester~~filter~~CIE:~",
                               signif(cie.ps, digits=3),
                               "*~kJ~m^{-2}~d^{-1}", sep=""),
                    y=y.pos, x=300,  hjust=0,  colour="red",
           parse=TRUE) +
  annotate("text", label=paste("Polycarbonate~~filter~~CIE:~",
                               signif(cie.pc, digits=3),
                               "*~kJ~m^{-2}~d^{-1}", sep=""),
                    y=y.pos-2, x=300, hjust=0,  colour="blue",
           parse=TRUE)
fig_dsun2 + theme_bw()


## ----scales-1------------------------------------------------------------
scale_colour_tgspct <-
  function(...,
           tg.spct,
           labels = NULL,
           guide = NULL,
           na.value=NA) {
    spct.tags <- attr(tg.spct, "spct.tags", exact=TRUE)
    if (is.null(guide)){
      if (spct.tags$wb.num > 12) {
        guide = "none"
      } else {
        guide = guide_legend(title=NULL)
      }
    }
    values <- as.character(spct.tags$wb.colors)
    if (is.null(labels)) {
      labels <- spct.tags$wb.names
    }
    ggplot2:::manual_scale("colour",
                           values = values,
                           labels = labels,
                           guide = guide,
                           na.value = na.value,
                           ...)
}


## ----scales-2------------------------------------------------------------
scale_fill_tgspct <-
  function(...,
           tg.spct,
           labels = NULL,
           guide = NULL,
           na.value=NA) {
    spct.tags <- attr(tg.spct, "spct.tags", exact=TRUE)
    if (is.null(guide)){
      if (spct.tags$wb.num > 12) {
        guide = "none"
      } else {
        guide = guide_legend(title=NULL)
      }
    }
    values <- as.character(spct.tags$wb.colors)
    if (is.null(labels)) {
      labels <- spct.tags$wb.names
    }

    ggplot2:::manual_scale("fill",
                           values = values,
                           labels = labels,
                           guide = guide,
                           na.value = na.value,
                           ...)
}


## ----tag-par-1-----------------------------------------------------------
par.sun.spct <- copy(sun.spct)
tag(par.sun.spct, PAR())


## ----tag-par-2-----------------------------------------------------------
fig_sun.z <-
  ggplot(data=par.sun.spct,
         aes(x=w.length, y=s.e.irrad)) +
  geom_line() +
  geom_point(aes(color=wb.f)) +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.z


## ----tag-vis-1-----------------------------------------------------------
tg.sun.spct <- copy(sun.spct)
tag(tg.sun.spct, VIS_bands())


## ----tag-vis-2-----------------------------------------------------------
fig_sun.z <-
  ggplot(data=tg.sun.spct,
         aes(x=w.length, y=s.e.irrad)) +
  geom_line() +
  scale_fill_tgspct(tg.spct=tg.sun.spct) +
  geom_point(aes(fill=wb.f), shape=21)  +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.z


## ----tag-vis-3-----------------------------------------------------------
fig_sun.zz <-
ggplot(tg.sun.spct,
       aes(x=w.length, y=s.e.irrad)) +
  scale_fill_tgspct(tg.spct=tg.sun.spct) +
  geom_line() +
  geom_area(aes(fill=wb.f), alpha=0.5) +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.zz + theme_bw()


## ----tag-plants-1--------------------------------------------------------
pl.sun.spct <- copy(sun.spct)
tag(pl.sun.spct, Plant_bands())


## ----tag-plants-2--------------------------------------------------------
fig_sun.pl <-
ggplot(pl.sun.spct,
       aes(x=w.length, y=s.e.irrad)) +
  scale_fill_tgspct(tg.spct=pl.sun.spct) +
  geom_line() +
  geom_area(aes(fill=wb.f)) +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.pl + theme_bw()


## ----tag-plants-3--------------------------------------------------------
fig_sun.pl <-
ggplot(pl.sun.spct,
       aes(x=w.length, y=s.e.irrad)) +
  geom_area(aes(fill=wb.f)) +
  scale_fill_grey(na.value=NA, name="",
                  labels=c("UVB", "UVA", "Blue", "Green", "Red", "Far red")) +
  geom_line() +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.pl + theme_bw()


## ----interpolate, eval=FALSE---------------------------------------------
## interpolate_spct(sun.spct, length.out=800)


## ----split-VIS-1---------------------------------------------------------
splt.sun.spct <- copy(sun.spct)
tag(splt.sun.spct, split_bands(VIS(), length.out=150))


## ----split-VIS-2---------------------------------------------------------
fig_sun.splt <-
ggplot(splt.sun.spct,
       aes(x=w.length, y=s.e.irrad)) +
  scale_fill_tgspct(tg.spct=splt.sun.spct) +
  geom_area(aes(fill=wb.f), alpha=0.67) +
  geom_line() +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.splt + theme_bw()


## ----split-all-1---------------------------------------------------------
splt1.sun.spct <- copy(sun.spct)
# splt1.sun.spct <- interpolate_spct(splt1.sun.spct, length.out=1000)
tag(splt1.sun.spct, split_bands(sun.spct, length.out=200))


## ----split-all-2---------------------------------------------------------
fig_sun.splt1 <-
ggplot(splt1.sun.spct,
       aes(x=w.length, y=s.e.irrad)) +
  scale_fill_tgspct(tg.spct=splt1.sun.spct) +
  geom_area(aes(fill=wb.f)) +
  geom_line() +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.splt1 + theme_bw()


## ----split-all-3---------------------------------------------------------
fig_sun.splt1 <-
ggplot(splt1.sun.spct,
       aes(x=w.length, y=s.e.irrad)) +
  scale_fill_tgspct(tg.spct=splt1.sun.spct) +
  geom_area(aes(fill=wb.f), alpha=0.5) +
  geom_line() +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.splt1 + theme_bw()


## ----split-all-4---------------------------------------------------------
fig_sun.splt1 <-
ggplot(splt1.sun.spct,
       aes(x=w.length, y=s.e.irrad)) +
  scale_colour_tgspct(tg.spct=splt1.sun.spct) +
  geom_line() +
  geom_point(aes(colour=wb.f)) +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.splt1 + theme_bw()


## ----split-all-5---------------------------------------------------------
fig_sun.y <-
  ggplot(data=tg.sun.spct,
         aes(x=w.length, y=s.e.irrad)) +
  geom_line() +
  scale_color_identity() +
  geom_point(aes(color=wl.color))  +
  labs(
    y = ylab_watt,
    x = "Wavelength (nm)")
fig_sun.y  + theme_bw()


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide_square)


## ----tern-1--------------------------------------------------------------
colours <- c("red", "green", "yellow", "white",
             "orange", "purple", "seagreen", "pink")
rgb.values <- col2rgb(colours)
test.data <- data.frame(colour=colours,
                        R=rgb.values[1, ],
                        G=rgb.values[2, ],
                        B=rgb.values[3, ])
maxwell.tern <- ggtern(data=test.data,
                       aes(x=R, y=G, z=B, label=colour, fill=colour)) +
                       geom_point(shape=23, size=3) +
  geom_text(hjust=-0.2) +
  labs(x = "R", y="G", z="B") + scale_fill_identity()
maxwell.tern


## ------------------------------------------------------------------------
try(detach(package:photobiologygg))
try(detach(package:ggtern))
try(detach(package:ggplot2))
try(detach(package:gridExtra))
try(detach(package:photobiologyFilters))
try(detach(package:photobiologyWavebands))
try(detach(package:photobiology))




## ----child-calibration, child='calibration.Rnw', eval=incl_ckbk----------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-simulation, child='simulation.Rnw', eval=incl_ckbk------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-measurement, child='measurement.Rnw', eval=incl_ckbk----------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)




## ----child-performance, child='performance.Rnw', eval=incl_ckbk----------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(microbenchmark)


## ------------------------------------------------------------------------
attach(sun.data)


## ------------------------------------------------------------------------
library(microbenchmark)


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, PAR(), use.cache=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  irradiance(w.length, s.e.irrad, PAR(), unit.out="photon",
             use.cache=TRUE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, PAR()),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, PAR(), use.cache=TRUE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  energy_irradiance(w.length, s.e.irrad, CIE()),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  energy_irradiance(w.length, s.e.irrad, CIE(), use.cache=TRUE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, PAR(), use.cache=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, PAR(), use.cache=TRUE,
                    check.spectrum=FALSE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
myPAR <- PAR()
res1 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, PAR(), use.cache=TRUE,
                    check.spectrum=FALSE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, myPAR, use.cache=TRUE,
                    check.spectrum=FALSE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
myCIE <- CIE()
res1 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, CIE(), use.cache=TRUE,
                    check.spectrum=FALSE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, myCIE, use.cache=TRUE,
                    check.spectrum=FALSE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  energy_irradiance(w.length, s.e.irrad, PAR(), use.cache=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  energy_irradiance(w.length, s.e.irrad, PAR(), use.cache=TRUE,
                    use.hinges=TRUE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  energy_irradiance(w.length, s.e.irrad, CIE(), use.cache=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  energy_irradiance(w.length, s.e.irrad, CIE(), use.cache=TRUE,
                    use.hinges=TRUE),
  times=100L, control=list(warmup = 10L))


## ----tidy=FALSE----------------------------------------------------------
# slowest
res1 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, GEN.G(),
                    use.cache=FALSE,
                    use.hinges=TRUE,
                    check.spectrum=TRUE),
                      times=100L, control=list(warmup = 10L))
# default
res2 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, GEN.G()),
                       times=100L, control=list(warmup = 10L))

# fastest
gen.g <- GEN.G()
res3 <- microbenchmark(
  irradiance(w.length, s.e.irrad, gen.g,
             use.cache=TRUE,
             use.hinges=FALSE,
             check.spectrum=FALSE,
             unit.out="photon"),
                       times=100L, control=list(warmup = 10L))


## ----tidy=FALSE----------------------------------------------------------
# slowest
photon_irradiance(w.length, s.e.irrad, GEN.G(),
                  use.cache=FALSE,
                  use.hinges=TRUE,
                  check.spectrum=TRUE)

# default
photon_irradiance(w.length, s.e.irrad, GEN.G())

# fastest
gen.g <- GEN.G()
irradiance(w.length, s.e.irrad, gen.g,
           use.cache=TRUE,
           use.hinges=FALSE,
           check.spectrum=FALSE,
           unit.out="photon")


## ----tidy=FALSE----------------------------------------------------------
# slowest
res1 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, CIE(),
                    use.cache=FALSE,
                    use.hinges=TRUE,
                    check.spectrum=TRUE),
                      times=100L, control=list(warmup = 10L))
# default
res2 <- microbenchmark(
  photon_irradiance(w.length, s.e.irrad, CIE()),
                       times=100L, control=list(warmup = 10L))

# fastest
cie <- CIE()
res3 <- microbenchmark(
  irradiance(w.length, s.e.irrad, cie,
             use.cache=TRUE,
             use.hinges=FALSE,
             check.spectrum=FALSE,
             unit.out="photon"),
                       times=100L, control=list(warmup = 10L))


## ----tidy=FALSE----------------------------------------------------------
# slowest
photon_irradiance(w.length, s.e.irrad, CIE(),
                  use.cache=FALSE,
                  use.hinges=TRUE,
                  check.spectrum=TRUE)

# default
photon_irradiance(w.length, s.e.irrad, CIE())

# fastest
CIE <- CIE()
irradiance(w.length, s.e.irrad, CIE,
           use.cache=TRUE,
           use.hinges=FALSE,
           check.spectrum=FALSE,
           unit.out="photon")


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  split_photon_irradiance(w.length, s.e.irrad,
                          c(400, 500, 600, 700)),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  split_photon_irradiance(w.length, s.e.irrad,
                          c(400, 500, 600, 700),
                          use.cache=TRUE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  split_photon_irradiance(w.length, s.e.irrad,
                          c(400, 500, 600, 700),
                          use.cache=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  split_photon_irradiance(w.length, s.e.irrad,
                          c(400, 500, 600, 700),
                          use.cache=TRUE,
                          use.hinges=TRUE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
res1 <- microbenchmark(
  split_photon_irradiance(w.length, s.e.irrad,
                          c(400, 500, 600, 700),
                          use.cache=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  split_photon_irradiance(w.length, s.e.irrad,
                          c(400, 500, 600, 700),
                          use.cache=TRUE,
                          check.spectrum=FALSE),
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
detach(sun.data)


## ----warning=FALSE-------------------------------------------------------
res1 <- microbenchmark(
  q_irrad_spct(sun.spct, PAR(), use.cached.mult=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  q_irrad_spct(sun.spct, list(UVC(), UVB(), UVA(), PAR()), use.cached.mult=TRUE),
  times=100L, control=list(warmup = 10L))


## ----warning=FALSE-------------------------------------------------------
res1 <- microbenchmark(
  q_irrad_spct(sun.spct),
  times=100L, control=list(warmup = 10L))


## ----warning=FALSE-------------------------------------------------------
res1 <- microbenchmark(
  q_irrad_spct(sun.spct, PAR(), use.cached.mult=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  q_irrad_spct(sun.spct, PAR(), use.cached.mult=FALSE),
  times=100L, control=list(warmup = 10L))


## ----warning=FALSE-------------------------------------------------------
res1 <- microbenchmark(
  q_irrad_spct(sun.spct, CIE(), use.cached.mult=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  q_irrad_spct(sun.spct, CIE(), use.cached.mult=FALSE),
  times=100L, control=list(warmup = 10L))


## ----warning=FALSE-------------------------------------------------------
res1 <- microbenchmark(
  q_irrad_spct(sun.spct, CIE(), use.cached.mult=TRUE),
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  q_irrad_spct(sun.spct, CIE(), use.cached.mult=TRUE, use.hinges=TRUE),
  times=100L, control=list(warmup = 10L))


## ----warning=FALSE-------------------------------------------------------
q_irrad_spct(sun.spct, CIE(), use.cached.mult=TRUE)
q_irrad_spct(sun.spct, CIE(), use.cached.mult=FALSE)
q_irrad_spct(sun.spct, CIE(), use.cached.mult=TRUE, use.hinges=TRUE)


## ----warning=FALSE-------------------------------------------------------
cp_sun.spct <- copy(sun.spct)
res1 <- microbenchmark(
  sun_out.spct <- cp_sun.spct * 2 + cp_sun.spct,
  times=100L, control=list(warmup = 10L))
res2 <- microbenchmark(
  sun_out.spct <- with(sun.data, s.e.irrad * 2 + s.e.irrad),
  times=100L, control=list(warmup = 10L))
res3 <- microbenchmark(
  sun_out.spct <- with(sun.dt, s.e.irrad * 2 + s.e.irrad),
  times=100L, control=list(warmup = 10L))
res4 <- microbenchmark(
  sun_out.spct <- with(cp_sun.spct, s.e.irrad * 2 + s.e.irrad),
  times=100L, control=list(warmup = 10L))
res5 <- microbenchmark(
  sun_out.spct <- cp_sun.spct[ , s.e.irrad := s.e.irrad * 2 + s.e.irrad],
  times=100L, control=list(warmup = 10L))


## ------------------------------------------------------------------------
library(ggplot2)
library(profr)


## ----eval=FALSE----------------------------------------------------------
## profr.df <- profr({q_irrad_spct(sun.spct)},
##                   interval = 0.0005, quiet = TRUE)
## head(profr.df)
## ggplot(profr.df)


## ----eval=FALSE----------------------------------------------------------
## profr.df <- profr({q_irrad_spct(sun.spct, my_PAR, use.hinges=TRUE)},
##                   interval = 0.0001, quiet = TRUE)
## head(profr.df)
## ggplot(profr.df)


## ----eval=FALSE----------------------------------------------------------
## profr.df <- profr({q_irrad_spct(sun.spct, my_PAR, use.hinges=FALSE)},
##                   interval = 0.0001, quiet = TRUE)
## head(profr.df)
## ggplot(profr.df)


## ------------------------------------------------------------------------
profr.df <- profr({tag(my_sun.spct)},
                  interval = 0.005, quiet = TRUE)
head(profr.df)
ggplot(profr.df)


## ------------------------------------------------------------------------
my_sun.spct <- copy(sun.spct)
Rprof("profile1.out", line.profiling=TRUE, interval = 0.002)
tag(my_sun.spct)
Rprof(NULL)
summaryRprof("profile1.out", lines = "show")[["by.line"]]
# profr.df <- parse_rprof("profile1.out")
# head(profr.df)
# ggplot(profr.df)


## ------------------------------------------------------------------------
try(detach(package:profr))
try(detach(package:photobiologyWavebands))
try(detach(package:photobiology))
try(detach(package:microbenchmark))
try(detach(package:ggplot2))



## ----child-r-calc, child='R.as.calculator.Rnw', eval=incl_apdx-----------

## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide)
opts_knit$set(concordance=TRUE)


## ----numbers-1-----------------------------------------------------------
1 + 1
2 * 2
2 + 10 / 5
(2 + 10) / 5
10^2 + 1
sqrt(9)
pi # whole precision not shown when printing
print(pi, digits=22)
sin(pi) # oops! Read on for explanation.
log(100)
log10(100)
log2(8)
exp(1)


## ----numbers-2-----------------------------------------------------------
a <- 1
a + 1
a
b <- 10
b <- a + b
b
3e-2 * 2.0


## ----numbers-3, tidy=FALSE-----------------------------------------------
a <- b <- c <- 0.0
a
b
c
1 -> a
a
a = 3
a


## ----numbers-4-----------------------------------------------------------
a <- c(3,1,2)
a
b <- c(4,5,0)
b
c <- c(a, b)
c
d <- c(b, a)
d


## ----numbers-5-----------------------------------------------------------
a <- -1:5
a
b <- 5:-1
b
c <- seq(from = -1, to = 1, by = 0.1)
c
d <- rep(-5, 4)
d


## ----numbers-6-----------------------------------------------------------
a + 1 # we add one to vector a defined above
(a + 1) * 2
a + b
a - a


## ----numbers-6a----------------------------------------------------------
a <- rep(1, 6)
a
a + 1:2
a + 1:3
a + 1:4


## ----numbers-7-----------------------------------------------------------
z <- numeric(0)
z
ls(pattern="^z$")
rm(z)
try(z)
ls(pattern="^z$")


## ----numbers-8-----------------------------------------------------------
a <- NA
a
-1 / 0
1 / 0
Inf / Inf
Inf + 4


## ----numbers-9-----------------------------------------------------------
1 - 1e-20


## ----numbers-10, eval=FALSE----------------------------------------------
## abs(x) < eps
## abs(x) < 1e-100


## ----integers-1----------------------------------------------------------
1L + 3L
1L * 3L
1L %/% 3L
1L / 3L


## ----logical-1-----------------------------------------------------------
a <- TRUE
b <- FALSE
a
!a # negation
a && b # logical AND
a || b # logical OR


## ----logical-2-----------------------------------------------------------
a <- c(TRUE,FALSE)
b <- c(TRUE,TRUE)
a
b
a & b # vectorized AND
a | b # vectorized OR
a && b # not vectorized
a || b # not vectorized


## ------------------------------------------------------------------------
any(a)
all(a)
any(a & b)
all(a & b)


## ----logical-3-----------------------------------------------------------
TRUE || NA
FALSE || NA
TRUE && NA
FALSE && NA
TRUE && FALSE && NA
TRUE && TRUE && NA


## ----logical-4-----------------------------------------------------------
a & b & NA
a & b & c(NA, NA)
a | b | c(NA, NA)


## ----comparison-1--------------------------------------------------------
1.2 > 1.0
1.2 >= 1.0
1.2 == 1.0 # be aware that here we use two = symbols
1.2 != 1.0
1.2 <= 1.0
1.2 < 1.0
a <- 20
a < 100 && a > 10


## ----comparison-2--------------------------------------------------------
a <- 1:10
a > 5
a < 5
a == 5
all(a > 5)
any(a > 5)
b <- a > 5
b
any(b)
all(b)


## ----comparison-3--------------------------------------------------------
c <- c(a, NA)
c > 5
all(c > 5)
any(c > 5)
all(c < 20)
any(c > 20)
is.na(a)
is.na(c)
any(is.na(c))
all(is.na(c))


## ----comparison-4--------------------------------------------------------
all(c < 20)
any(c > 20)
all(c < 20, na.rm=TRUE)
any(c > 20, na.rm=TRUE)


## ----comparison-5--------------------------------------------------------
1e20 == 1 + 1e20
1 == 1 + 1e-20
0 == 1e-20


## ----comparion-6---------------------------------------------------------
a == 0.0 # may not always work
abs(a) < 1e-15 # is safer
sin(pi) == 0.0 # angle in radians, not degrees!
sin(2 * pi) == 0.0
abs(sin(pi)) < 1e-15
abs(sin(2 * pi)) < 1e-15
sin(pi)
sin(2 * pi)
.Machine$double.eps # see help for .Machine for explanation
.Machine$double.neg.eps


## ----char-1--------------------------------------------------------------
a <- "A"
b <- letters[2]
c <- letters[1]
a
b
c
d <- c(a, b, c)
d
e <- c(a, b, "c")
e
h <- "1"
try(h + 2)


## ----char-2--------------------------------------------------------------
f <- c("1", "2", "3")
g <- "123"
f == g
f
g


## ----char-3--------------------------------------------------------------
a <- "He said 'hello' when he came in"
a
b <- 'He said "hello" when he came in'
b


## ----char-4--------------------------------------------------------------
c <- "abc\ndef\txyz"
print(c)
cat(c)


## ----convert-1-----------------------------------------------------------
as.character(1)
as.character(3.0e10)
as.numeric("1")
as.numeric("5E+5")
as.numeric("A")
as.numeric(TRUE)
as.numeric(FALSE)
TRUE + TRUE
TRUE + FALSE
TRUE * 2
FALSE * 2
as.logical("T")
as.logical("t")
as.logical("TRUE")
as.logical("true")
as.logical(100)
as.logical(0)
as.logical(-1)


## ----convert-2-----------------------------------------------------------
f <- c("1", "2", "3")
g <- "123"
as.numeric(f)
as.numeric(g)


## ----convert-3, tidy=FALSE-----------------------------------------------
round(0.0124567, 3)
round(0.0124567, 1)
round(0.0124567, 5)
signif(0.0124567, 3)
round(1789.1234, 3)
signif(1789.1234, 3)
a <- 0.12345
b <- round(a, 2)
a == b
a - b
b


## ----vectors-1-----------------------------------------------------------
a <- letters[1:10]
a
a[2]
a[c(3,2)]
a[10:1]


## ----vectors-2-----------------------------------------------------------
a[c(3,3,3,3)]
a[c(10:1, 1:10)]


## ----vectors-3-----------------------------------------------------------
a[-2]
a[-c(3,2)]


## ----vectors-4-----------------------------------------------------------
a[11]
a[1:11]


## ----vectors-5-----------------------------------------------------------
a[ ]
a[numeric(0)]
a[NA]
a[c(1, NA)]
a[NULL]
a[c(1, NULL)]


## ----vectors-6-----------------------------------------------------------
a[TRUE]
a[FALSE]
a[c(TRUE, FALSE)]
a[c(FALSE, TRUE)]
a > "c"
a[a > "c"]
selector <- a > "c"
a[selector]
which(a > "c")
indexes <- which(a > "c")
a[indexes]
b <- 1:10
b[selector]
b[indexes]


## ----stat-fun-1----------------------------------------------------------
x <- 1:20
mean(x)
var(x)
median(x)
mad(x)
sd(x)
range(x)
max(x)
min(x)
length(x)




## ----child-r-scripts, child='R.scripts.Rnw', eval=incl_apdx--------------

## ----echo=FALSE, cache=FALSE---------------------------------------------
set_parent('r4p.main.Rnw')
opts_knit$set(concordance=TRUE)


## ----setup-scripts, include=FALSE, cache=FALSE---------------------------
show.results <- FALSE


## ----evaluate=FALSE------------------------------------------------------
source("my.first.script.r")


## ----fun-00--------------------------------------------------------------
my.prod <- function(x, y){x * y}
my.prod(4, 3)


## ----fun-01--------------------------------------------------------------
my.change <- function(x){x <- NA}
a <- 1
my.change(a)
a


## ----fun-02--------------------------------------------------------------
print.x.1 <- function(x){print(x)}
print.x.1("test")
print.x.2 <- function(x){print(x); return(x)}
print.x.2("test")
print.x.3 <- function(x){return(x); print(x)}
print.x.3("test")
print.x.4 <- function(x){return(); print(x)}
print.x.4("test")


## ----fun-1---------------------------------------------------------------
SEM <- function(x){sqrt(var(x)/length(x))}
a <- c(1, 2, 3, -5)
a.na <- c(a, NA)
SEM(x=a)
SEM(a)
SEM(a.na)


## ----fun-1-safe----------------------------------------------------------
SEM <- function(x) sqrt(var(x, na.rm=TRUE)/length(na.omit(x)))
a <- c(1, 2, 3, -5)
a.na <- c(a, NA)
SEM(x=a)
SEM(a)
SEM(a.na)


## ----fun-2---------------------------------------------------------------
SEM <- function(x, na.rm=FALSE){sqrt(var(x, na.rm=na.rm)/length(na.omit(x)))}
SEM(a)
SEM(a.na)
SEM(a.na, TRUE)
SEM(x=a.na, na.rm=TRUE)
SEM(TRUE, a.na)
SEM(na.rm=TRUE, x=a.na)


## ----plot-1--------------------------------------------------------------
data(cars)
names(cars)
head(cars)
tail(cars)


## ----plot-2--------------------------------------------------------------
plot(dist ~ speed, data=cars)


## ----models-1------------------------------------------------------------
fm1 <- lm(dist ~ speed, data=cars) # we fit a model, and then save the result
plot(fm1) # we produce diagnosis plots
summary(fm1) # we inspect the results from the fit
anova(fm1) # we calculate an ANOVA


## ----models-2------------------------------------------------------------
fm2 <- lm(dist ~ speed - 1, data=cars) # we fit a model, and then save the result
plot(fm2) # we produce diagnosis plots
summary(fm2) # we inspect the results from the fit
anova(fm2) # we calculate an ANOVA


## ----models-3------------------------------------------------------------
fm3 <- lm(dist ~ speed + I(speed^2), data=cars) # we fit a model, and then save the result
plot(fm3) # we produce diagnosis plots
summary(fm3) # we inspect the results from the fit
anova(fm3) # we calculate an ANOVA


## ----models-4------------------------------------------------------------
anova(fm2, fm1)


## ----models-5------------------------------------------------------------
anova(fm2, fm1, fm3)


## ----if-1----------------------------------------------------------------
print("A")
{
  print("B")
  print("C")
}


## ----if-2----------------------------------------------------------------
printing <- TRUE
if (printing) {
  print("A")
  print("B")
}


## ----if-3----------------------------------------------------------------
a <- 10.0
if (a < 0.0) print("'a' is negative") else print("'a' is not negative")
print("This is always printed")


## ----auxiliary, echo=FALSE, eval=TRUE------------------------------------
if (show.results) eval.if.4 <- c(1:4) else eval.if.4 <- FALSE


## ----if-4, eval=eval.if.4------------------------------------------------
# 1
if (a < 0.0)
  print("'a' is negative") else
    print("'a' is not negative")
# 2 (not evaluated here)
if (a < 0.0) print("'a' is negative")
else print("'a' is not negative")


## ----ifelse-1------------------------------------------------------------
a <- 1:10
ifelse(a > 5, 1, -1)
ifelse(a > 5, a + 1, a - 1)
ifelse(any(a>5), a + 1, a - 1) # tricky
ifelse(logical(0), a + 1, a - 1) # even more tricky
ifelse(NA, a + 1, a - 1) # as expected


## ----ifelse-2------------------------------------------------------------
a <- rep(-1, 10)
b <- rep(+1, 10)
c <- c(rep("a", 5), rep("b", 5))
# your code


## ----for-0---------------------------------------------------------------
b <- 0
for (a in 1:5) b <- b + a
b
b <- sum(1:5) # built-in function
b


## ----for-3---------------------------------------------------------------
test.for <- function(x) {
  for (i in x) {print(i)}
}
test.for(numeric(0))
test.for(1:3)
test.for(NA)
test.for(c("A", "B"))
test.for(c("A", NA))
test.for(list("A", 1))
test.for(c("z", letters[1:4]))


## ----for-1---------------------------------------------------------------
a <- c(1, 4, 3, 6, 8)
for(x in a) x*2 # result is lost
for(x in a) print(x*2) # print is needed!
b <- for(x in a) x*2 # doesn't work as expected, but triggers no error
b
for(x in a) b <- x*2 # a bit of a surprise, as b is not a vector!
b
for(i in seq(along=a)) {
  b[i] <- a[i]^2
  print(b)
}
b # is a vector!
# a bit faster if we first allocate a vector of the required length
b <- numeric(length(a))
for(i in seq(along=a)) {
  b[i] <- a[i]^2
  print(b)
}
b # is a vector!
# vectorization is simplest and fastest
b <- a^2
b


## ----for-2---------------------------------------------------------------
b <- numeric(length(a)-1)
for(i in seq(along=b)) {
  b[i] <- a[i+1] - a[i]
  print(b)
}
# although in this case there were alternatives, there
# are other cases when we need to use indexes explicitly
b <- a[2:length(a)] - a[1:length(a)-1]
b
# or even better
b <- diff(a)
b


## ----while-1-------------------------------------------------------------
a <- c(1, 4, 3, 6, 8)
i <- 1
while (i < length(a)) {
  b[i] <- a[i]^2
  print(b)
  i <- i + 1
}
b


## ----while-2-------------------------------------------------------------
a <- 2
while (a < 50) {print(a); a <- a^2}
print(a)


## ----repeat-1------------------------------------------------------------
a <- 2
repeat{
  print(a)
  a <- a^2
  if (a > 50) {print(a); break()}
}
# or more elegantly
a <- 2
repeat{
  print(a)
  if (a > 50) break()
  a <- a^2
}


## ----nested-1------------------------------------------------------------
A <- matrix(1:50, 10)
A
A <- matrix(1:50, 10, 5)
A
# argument names used for clarity
A <- matrix(1:50, nrow = 10)
A
A <- matrix(1:50, ncol = 5)
A
A <- matrix(1:50, nrow = 10, ncol = 5)
A


## ----nested-21-----------------------------------------------------------
row.sum <- numeric() # slower as size needs to be expanded
for (i in 1:nrow(A)) {
  row.sum[i] <- 0
  for (j in 1:ncol(A))
    row.sum[i] <- row.sum[i] + A[i, j]
}
print(row.sum)


## ----nested-22-----------------------------------------------------------
row.sum <- numeric(nrow(A)) # faster
for (i in 1:nrow(A)) {
  row.sum[i] <- 0
  for (j in 1:ncol(A))
    row.sum[i] <- row.sum[i] + A[i, j]
}
print(row.sum)


## ----nested-3------------------------------------------------------------
row.sum <- numeric(nrow(A)) # faster
for (i in 1:nrow(A)) {
  row.sum[i] <- sum(A[i, ])
}
print(row.sum)


## ----nested-4------------------------------------------------------------
row.sum <- apply(A, MARGIN = 1, sum) # MARGIN=1 inidcates rows
print(row.sum)


## ----packages-1----------------------------------------------------------
library(graphics)


## ----packages-2----------------------------------------------------------
length  # a function defined in C within R itself
SEM # the function we defined earlier




## ----child-r-plotting, child='R.plotting.Rnw', eval=incl_apdx------------

## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide)
opts_knit$set(concordance=TRUE)


## ------------------------------------------------------------------------
library(plyr)
library(grid)
library(Hmisc)
library(ggplot2)
library(scales)
# library(rgdal)
# library(ggtern)
# library(ggmap)
# library(GGally)


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=mpg)) +
  geom_point()


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=mpg, colour=cyl)) +
  geom_point()


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=mpg, colour=factor(cyl))) +
  geom_point()


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=factor(cyl), y=mpg / disp)) +
  geom_point()


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=hp, fill=factor(cyl))) +
  geom_point(shape=21, colour="grey10")


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=hp, colour=factor(cyl))) +
  geom_point() +
  geom_smooth(colour="black")


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=hp, colour=factor(cyl), shape=factor(cyl))) +
  geom_point()


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=hp, colour=factor(cyl), shape=factor(cyl))) +
  geom_point() +
    labs(x="Engine displacement)",
       y="Gross horsepower",
       colour="Number of\ncylinders",
       shape="Number of\ncylinders",
       title="Motor Trend Car Road Tests (1973-74 models)")


## ------------------------------------------------------------------------
myplot <- ggplot(mtcars, aes(x=disp, y=mpg, colour=factor(cyl))) +
  geom_point()
mylabs <- labs(x="Engine displacement)",
       y="Gross horsepower",
       colour="Number of\ncylinders",
       shape="Number of\ncylinders",
       title="Motor Trend Car Road Tests (1973-74 models)")
myplot
myplot + mylabs


## ------------------------------------------------------------------------
myplot + mylabs + theme_bw()
myplot + mylabs + theme_bw() + ylim(0, NA)


## ------------------------------------------------------------------------
mylogplot <- myplot + scale_y_log10(breaks=c(10,20,40), limits=c(8,45))
mylogplot + mylabs + theme_bw()


## ------------------------------------------------------------------------
myplot + mylabs + theme_grey(10)
myplot + mylabs + theme_grey(16)


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=mpg, colour=factor(cyl))) +
  geom_point() + geom_smooth(colour="black", alpha=0.7)


## ------------------------------------------------------------------------
ggplot(mtcars, aes(x=disp, y=mpg, colour=factor(cyl))) +
   geom_smooth(colour="black", alpha=0.7) + geom_point()


## ------------------------------------------------------------------------
myplot + stat_smooth(colour="black")


## ------------------------------------------------------------------------
myplot + stat_smooth(method="lm", colour="black")


## ------------------------------------------------------------------------
myplot + stat_smooth(method="lm", formula=y~poly(x,2), colour="black")


## ------------------------------------------------------------------------
myplot + stat_smooth(method="lm")


## ------------------------------------------------------------------------
fake.data <- data.frame(
  y = c(rnorm(20, mean=2, sd=0.5), rnorm(20, mean=4, sd=0.7)),
  group = factor(c(rep("A", 20), rep("B", 20)))
  )


## ------------------------------------------------------------------------
fig2 <- ggplot(data=fake.data, aes(y=y, x=group)) + geom_point()
fig2


## ------------------------------------------------------------------------
fig2 + stat_summary(fun.y = "mean", geom="point",
                    colour="red", shape="-", size=20)


## ------------------------------------------------------------------------
fig2 + stat_summary(fun.y = "median", geom="point",
                    colour="red", shape="-", size=20)


## ------------------------------------------------------------------------
fig2 + stat_summary(fun.data = "mean_cl_boot",
                    colour="red", size=1, alpha=0.7)


## ------------------------------------------------------------------------
fig2 + stat_summary(fun.data = "mean_cl_boot", conf.int=0.90,
                    colour="red", size=1, alpha=0.7)


## ------------------------------------------------------------------------
fig2 + stat_summary(fun.data = "mean_cl_normal",
                    colour="red", size=1, alpha=0.7)


## ------------------------------------------------------------------------
fig2 + stat_summary(fun.data = "mean_cl_normal", mult=1,
                    colour="red", size=1, alpha=0.7)


## ------------------------------------------------------------------------
fig2 + stat_summary(fun.data = "mean_sdl",
                    colour="red", size=1, alpha=0.7)


## ------------------------------------------------------------------------
ggplot(data=fake.data, aes(y=y, x=group)) +
  stat_summary(fun.y = "mean", geom = "bar",
               fill="yellow", colour="black") +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width=0.1, size=1, colour="red") +
  geom_point(size=3, alpha=0.3)


## ------------------------------------------------------------------------
ggplot(data.frame(x=-3:3), aes(x=x)) +
  stat_function(fun=dnorm)


## ------------------------------------------------------------------------
ggplot(data.frame(x=-3:3), aes(x=x)) +
  stat_function(fun = dnorm, args = list(mean = 1, sd = .5))


## ------------------------------------------------------------------------
ggplot(data.frame(x=0:1), aes(x=x)) +
  stat_function(fun = function(x, a, b){a + b * x^2},
                args = list(a = 1, b = 1.4))


## ------------------------------------------------------------------------
ggplot(data.frame(x=c(0, 2 * pi)), aes(x=x)) +
  stat_function(fun=sin)


## ------------------------------------------------------------------------
ggplot(data.frame(x=c(0, 2 * pi)), aes(x=x)) +
  stat_function(fun=sin) +
  scale_x_continuous(
    breaks=c(0, 0.5, 1, 1.5, 2) * pi,
    labels=c("0", expression(0.5~pi), expression(pi),
             expression(1.5~pi), expression(2~pi))) +
  labs(y="sin(x)")


## ------------------------------------------------------------------------
my.data <-
  data.frame(x=1:5, y=rep(2, 5), label=paste(letters[1:5], " "))
ggplot(my.data, aes(x,y,label=label)) +
  geom_text(angle=45, hjust=1) + geom_point()


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_medium)


## ------------------------------------------------------------------------
fake2.data <- data.frame(
  y = c(rnorm(20, mean=20, sd=5), rnorm(20, mean=40, sd=10)),
  group = factor(c(rep("A", 20), rep("B", 20))),
  z = rnorm(40, mean=12, sd=6)
  )


## ------------------------------------------------------------------------
fig2 <-
  ggplot(data=fake2.data,
         aes(y=y, x=group, shape=group, colour=group, size=z)) +
  geom_point(alpha=0.3) + ylim(0, NA)
fig2


## ------------------------------------------------------------------------
fig2 +
  scale_y_log10(breaks=c(10,20,30,40,50,60)) +
       stat_summary(fun.data = "mean_cl_normal",
                    colour="black", size=1, alpha=1)


## ------------------------------------------------------------------------
ggplot(data.frame(x=c(0, 2 * pi)), aes(x=x)) +
  stat_function(fun=sin) +
  scale_x_continuous(
    breaks=c(0, 0.5, 1, 1.5, 2) * pi,
    labels=c("0", expression(0.5~pi), expression(pi),
             expression(1.5~pi), expression(2~pi))) +
  labs(y="sin(x)") +
  annotate(geom="text",
           label=c("+", "-"),
           x=c(0.5, 1.5) * pi, y=c(0.5, -0.5),
           size=20) +
  annotate(geom="point",
           colour="red",
           shape=21,
           fill="white",
           x=c(0, 1, 2) * pi, y=0,
           size=6)


## ------------------------------------------------------------------------
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
# With one variable
p + facet_grid(. ~ cyl)


## ------------------------------------------------------------------------
p + facet_grid(cyl ~ .)


## ------------------------------------------------------------------------
p + facet_grid(. ~ cyl, scales = "free")


## ------------------------------------------------------------------------
p + facet_grid(. ~ cyl, scales = "free", space = "free")


## ------------------------------------------------------------------------
p + facet_grid(vs ~ am)


## ------------------------------------------------------------------------
p + facet_grid(vs ~ am, margins=TRUE)


## ------------------------------------------------------------------------
p + facet_grid(. ~ vs + am)


## ------------------------------------------------------------------------
p + facet_grid(. ~ vs + am, labeller = label_both)


## ------------------------------------------------------------------------
p + facet_grid(. ~ vs + am, margins=TRUE)


## ------------------------------------------------------------------------
p + facet_grid(cyl ~ vs, labeller = label_both)


## ------------------------------------------------------------------------
mtcars$cyl12 <- factor(mtcars$cyl, labels = c("alpha", "beta", "sqrt(x, y)"))
p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p1 + facet_grid(. ~ cyl12, labeller = label_parsed)


## ------------------------------------------------------------------------
p + facet_grid(. ~ vs, labeller = label_bquote(alpha ^ .(x)))


## ------------------------------------------------------------------------
p + facet_wrap(~ cyl)


## ------------------------------------------------------------------------
p + facet_wrap(~ vs + am, ncol=2)


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide_square)


## ------------------------------------------------------------------------
library(GGally)


## ------------------------------------------------------------------------
# Use sample of the diamonds data
data(diamonds, package="ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],200),]
# Custom Example
pm <- ggpairs(
diamonds.samp[,1:3],
upper = list(continuous = "density", combo = "box"),
lower = list(continuous = "points", combo = "dot"),
color = "cut",
title = "Diamonds"
)
pm


## ------------------------------------------------------------------------
try(detach(package:GGally))


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide)


## ------------------------------------------------------------------------
# Hadley's favourite pie chart
df <- data.frame(
  variable = c("resembles", "does not resemble"),
  value = c(80, 20)
)
ggplot(df, aes(x = "", y = value, fill = variable)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = c("red", "yellow")) +
  coord_polar("y", start = pi / 3) +
  labs(title = "Pac man")


## ------------------------------------------------------------------------
# A pie chart = stacked bar chart + polar coordinates
pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
 geom_bar(width = 1)
pie + coord_polar(theta = "y")


## ----eval=FALSE, echo=FALSE----------------------------------------------
## a <- matrix(c(17, 18, 20, 22, 23,
##                 20, 20, 19, 21, 20,
##                 23, 22, 20, 18, 17), nrow=3, byrow=T)
##   titles <- c("A", "B", "C")
##   cols <- c("red", "blue", "green", "yellow", "black")
## 
##   postscript("piecharts.eps", paper="special", width=8, height=6)
## 
##   defaultmar <- par()$mar
##   layout(matrix(c(1,3,5,
##                   2,4,6), nrow=2, byrow=T), height=c(1,1))
##   par(cex=1)
##   par(font=1)
##   par(las=1)
##   par(font.axis=1)
##   par(mgp=c(1,1,0))
## 
##   for (i in 1:nrow(a)) {
##     par(mar=c(0,0,2,0))
##     pie(a[i,], init=90, clockwise=T, col=cols, radius=0.8)
##     title(main=titles[i], line=0)
##     par(mar=defaultmar+c(-2,-1.5,-4,-0.5))
##     par(mgp=c(0,0.5,0))
##     barplot(a[i,], horiz=F, xlim=c(0,10), ylim=c(0,25), col=cols, border=0,
##             names.arg=1:5, space=0.8, axes=F)
##     par(mgp=c(0,1,0))
## 
##     abline(h=5*1:5, col="white")
##     axis(2)
##   }
## 
##   dev.off()


## ------------------------------------------------------------------------
example.data <-
  data.frame(values = c(17, 18, 20, 22, 23,
                        20, 20, 19, 21, 20,
                        23, 22, 20, 18, 17),
             examples= rep(c("A", "B", "C"), c(5,5,5)),
             cols = rep(c("red", "blue", "green", "yellow", "black"), 3)
  )

ggplot(example.data, aes(x=cols, y=values, fill=cols)) +
  geom_bar(width = 1, stat="identity") +
  facet_grid(.~examples) +
  scale_fill_identity()
ggplot(example.data, aes(x=factor(1), y=values, fill=cols)) +
  geom_bar(width = 1, stat="identity") +
  facet_grid(.~examples) +
  scale_fill_identity() +
  coord_polar(theta="y")


## ----eval=FALSE----------------------------------------------------------
##  svg("anscombe.svg", width=10.5, height=7)
##  par(las=1)
## 
##  ##-- some "magic" to do the 4 regressions in a loop:
##  ff <- y ~ x
##  for(i in 1:4) {
##    ff[2:3] <- lapply(paste(c("y","x"), i, sep=""), as.name)
##    ## or   ff2 <- as.name(paste("y", i, sep=""))
##    ##      ff3 <- as.name(paste("x", i, sep=""))
##    assign(paste("lm.",i,sep=""), lmi <- lm(ff, data= anscombe))
##  }
## 
##  ## Now, do what you should have done in the first place: PLOTS
##  op <- par(mfrow=c(2,2), mar=1.5+c(4,3.5,0,1), oma=c(0,0,0,0),
##            lab=c(6,6,7), cex.lab=1.5, cex.axis=1.3, mgp=c(3,1,0))
##  for(i in 1:4) {
##    ff[2:3] <- lapply(paste(c("y","x"), i, sep=""), as.name)
##    plot(ff, data =anscombe, col="red", pch=21, bg = "orange", cex = 2.5,
##         xlim=c(3,19), ylim=c(3,13),
##         xlab=eval(substitute(expression(x[i]), list(i=i))),
##         ylab=eval(substitute(expression(y[i]), list(i=i))))
##    abline(get(paste("lm.",i,sep="")), col="blue")
##  }
## 
##  dev.off()


## ------------------------------------------------------------------------
# we rearrange the data
my.mat <- matrix(as.matrix(anscombe), ncol=2)
my.anscombe <- data.frame(x = my.mat[ , 1], y = my.mat[ , 2], case=factor(rep(1:4, rep(11,4))))
# we draw the figure
ggplot(my.anscombe, aes(x,y)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~case, ncol=2)


## ------------------------------------------------------------------------
ggplot(my.anscombe, aes(x,y)) +
  geom_point(shape=21, fill="orange", size=3) +
  geom_smooth(method="lm", se=FALSE) +
  facet_wrap(~case, ncol=2) +
  theme_bw()


## ------------------------------------------------------------------------
ggplot(my.anscombe, aes(x,y)) +
  geom_point(shape=21, fill="orange", size=3) +
  geom_smooth(method="lm") +
  facet_wrap(~case, ncol=2) +
  theme_bw()


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_narrow)
library(ggtern)


## ------------------------------------------------------------------------
# create some artificial data
my.trn1.data <- data.frame(x=runif(50), y=runif(50), z=runif(50))


## ------------------------------------------------------------------------
fig.trn <- ggplot(my.trn1.data, aes(x,y,z)) +
  coord_tern(L="x",T="y",R="z")


## ----eval=FALSE----------------------------------------------------------
## fig.trn <- ggtern(my.trn1.data, aes(x,y,z))


## ------------------------------------------------------------------------
fig.trn +
  geom_point()
fig.trn +
  geom_point() +
  theme_bw()


## ------------------------------------------------------------------------
fig.trn +
  geom_point() +
  geom_confidence()


## ------------------------------------------------------------------------
fig.trn +
  stat_density2d(fullrange=T,n=200,
                       geom="polygon", fill="grey10",
                       aes(alpha =..level..)) +
  geom_point(shape=21, fill="orange", size=4) +
  labs(x="x (%)", y="y (%)", z="z (%)", alpha="Density") +
  theme_rgbw()


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide_square)


## ------------------------------------------------------------------------
# Load the Data. (Available in ggtern 1.0.3.0 next version)
data(USDA)

# Put tile labels at the midpoint of each tile.
USDA.LAB = ddply(USDA, 'Label', function(df) {
    apply(df[, 1:3], 2, mean)
})

# Tweak
USDA.LAB$Angle = 0
USDA.LAB$Angle[which(USDA.LAB$Label == 'Loamy Sand')] = -35


## ------------------------------------------------------------------------
# Construct the plot.
ggplot(data = USDA, aes(y=Clay, x=Sand, z=Silt,
                        color = Label,
                        fill = Label)) +
  coord_tern(L="x",T="y",R="z") +
  geom_polygon(alpha = 0.75, size = 0.5, color = 'black') +
  geom_text(data = USDA.LAB,
            aes(label = Label, angle = Angle),
            color = 'black',
            size = 3.5) +
  theme_rgbw() +
  theme_showsecondary() +
  theme_showarrows() +
  custom_percent("Percent") +
  theme(legend.justification = c(0, 1),
        legend.position      = c(0, 1),
        axis.tern.padding    = unit(0.15, 'npc')) +
  labs(title = 'USDA Textural Classification Chart',
       fill  = 'Textural Class',
       color = 'Textural Class')


## ------------------------------------------------------------------------
try(detach(package:ggtern))


## ------------------------------------------------------------------------
library(ggmap)
library(rgdal)


## ----plot-maps-1, echo=FALSE, include=FALSE------------------------------
opts_chunk$set(opts_fig_very_narrow)


## ----plot-maps-2, message=FALSE, cache=TRUE------------------------------
Europe1 <- get_map("Europe", zoom=3, maptype="satellite")
ggmap(Europe1)

ggmap(Europe1, extent = "device")

ggmap(Europe1, extent = "normal")



## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_narrow)


## ----plot-maps-3, message=FALSE, cache=TRUE------------------------------
Europe2 <- get_map("Europe", zoom=3,
                  maptype="terrain")
ggmap(Europe2)

Europe3 <- get_map("Europe", zoom=3,
                  maptype="terrain",
                  color="bw")
ggmap(Europe3)


## ----plot-maps-4, message=FALSE, cache=TRUE------------------------------
Finland1 <- get_map("Oulu", zoom=5, maptype="terrain")
ggmap(Finland1)

Finland2 <- get_map("Oulu", zoom=5, maptype="roadmap")
ggmap(Finland2)


## ----plot-maps-5, message=FALSE, cache=TRUE------------------------------
BIO3 <- get_map("Viikinkaari 1, 00790 Helsinki",
                zoom=18,
                maptype="satellite")
ggmap(BIO3)


## ----plot-maps-6, message=FALSE, cache=TRUE------------------------------
viikki <- get_map("Viikki",
                  zoom=15,
                  maptype="satellite")

our_location <- data.frame(lat=c(60.225, 60.227),
                           lon=c(25.017, 25.018),
                           label=c("BIO3", "field"))
ggmap(viikki, extent = "normal") +
  geom_point(data=our_location, aes(y=lat, x=lon),
             size=4, colour="yellow") +
  geom_text(data=our_location, aes(y=lat, x=lon, label=label),
            hjust=-0.3, colour="yellow")

our_geocode <- geocode("Viikinkaari 1, 00790 Helsinki")
ggmap(viikki, extent = "normal") +
  annotate(geom="point",
           y=our_geocode[ 1, "lat"], x=our_geocode[ 1, "lon"],
           size=4, colour="yellow") +
  annotate(geom="text",
           y=our_geocode[ 1, "lat"], x=our_geocode[ 1, "lon"],
           label="BIO3", hjust=-0.3, colour="yellow")


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_wide)


## ----plot-maps-7, cache=TRUE---------------------------------------------
oldwd <- setwd("./maps")

url_path <-
#  "http://www.naturalearthdata.com/download/110m/"
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/"

download.file(paste(url_path,
                    "physical/ne_110m_land.zip",
                    sep=""), "ne_110m_land.zip")
unzip("ne_110m_land.zip")

download.file(paste(url_path,
                    "cultural/ne_110m_admin_0_countries.zip",
                    sep=""), "ne_110m_admin_0_countries.zip")
unzip("ne_110m_admin_0_countries.zip")

download.file(paste(url_path,
                    "physical/ne_110m_graticules_all.zip",
                    sep=""), "ne_110m_graticules_all.zip")
unzip("ne_110m_graticules_all.zip")

setwd(oldwd)


## ------------------------------------------------------------------------
ogrListLayers(dsn="./maps")


## ------------------------------------------------------------------------
wmap <- readOGR(dsn="./maps", layer="ne_110m_land")
wmap.data <- fortify(wmap)
wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_robin.data <- fortify(wmap_robin)


## ------------------------------------------------------------------------
countries <- readOGR("./maps", layer="ne_110m_admin_0_countries")
countries.data <- fortify(countries)
countries_robin <- spTransform(countries, CRS("+init=ESRI:54030"))
countries_robin.data <- fortify(countries_robin)


## ------------------------------------------------------------------------
grat <- readOGR("./maps", layer="ne_110m_graticules_15")
grat.data <- fortify(grat)
grat_robin <- spTransform(grat, CRS("+proj=robin"))
grat_robin.data <- fortify(grat_robin)

bbox <- readOGR("./maps", layer="ne_110m_wgs84_bounding_box")
bbox.data <- fortify(bbox)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))
bbox_robin.data <- fortify(bbox_robin)


## ------------------------------------------------------------------------
ggplot(wmap.data, aes(long,lat, group=group)) +
  geom_polygon() +
  labs(title="World map (longlat)") +
  coord_equal()


## ------------------------------------------------------------------------
ggplot(wmap.data, aes(long,lat, group=group, fill=hole)) +
  geom_polygon() +
  labs(title="World map (longlat)") +
  scale_fill_manual(values=c("#262626", "#e6e8ed"),
                    guide="none") +
  coord_equal()


## ----echo=FALSE, eval=FALSE----------------------------------------------
## # does not work as expected
## ggplot(wmap.data, aes(long,lat, group=group)) +
##   geom_polygon() +
##   labs(title="World map (longlat)") +
##   scale_fill_manual(values=c("#262626", "#e6e8ed"),
##                     guide="none") +
##   coord_map()


## ------------------------------------------------------------------------
theme_map_opts <-
  list(theme(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             panel.background = element_blank(),
             plot.background = element_rect(fill="#e6e8ed"),
             panel.border = element_blank(),
             axis.line = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank()))


## ------------------------------------------------------------------------
ggplot(bbox_robin.data, aes(long,lat, group=group)) +
  geom_polygon(fill="white") +
  geom_polygon(data=countries_robin.data,
               aes(long,lat, group=group,
                   fill=hole)) +
  geom_path(data=countries_robin.data,
            aes(long,lat, group=group, fill=hole),
            color="white",
            size=0.3) +
  geom_path(data=grat_robin.data,
            aes(long, lat, group=group, fill=NULL),
            linetype="dashed",
            color="grey50") +
  labs(title="World map (Robinson)") +
  coord_equal() +
  theme_map_opts +
  scale_fill_manual(values=c("black", "white"),
                    guide="none")


## ------------------------------------------------------------------------
try(detach(package:ggmap))
try(detach(package:rgdal))


## ------------------------------------------------------------------------
my.data$greek.label <- paste("alpha[", my.data$x, "]", sep="")
(fig <- ggplot(my.data, aes(x,y,label=greek.label)) +
   geom_text(angle=45, hjust=1.2, parse=TRUE) + geom_point())


## ------------------------------------------------------------------------
fig + labs(x=expression(alpha), y=expression(Speed~~(m~s^{-1})))


## ------------------------------------------------------------------------
my.title <- expression(sqrt(alpha[1] + frac(beta, gamma)))
fig + labs(title=my.title)


## ------------------------------------------------------------------------
fig + ylim(1,3) +
  annotate("text", label="sqrt(alpha[1] + frac(beta, gamma))",
           y=2.5, x=3, size=8, colour="red", parse=TRUE)


## ----cache=TRUE----------------------------------------------------------
d = data.frame(x = sort(rlnorm(300)),
               y = sort(rlnorm(300)),
               grp = 1)

main <- ggplot(d, aes(x, y)) +
  geom_point() + theme_bw()

sub <- main +
  geom_rect(data=d[1,],
            xmin=0, ymin=0, xmax=5, ymax=5,
            fill="grey50", alpha=0.3)
sub$layers <- rev(sub$layers) # draw rect below

main +
  annotation_custom(ggplotGrob(sub),
                    xmin=2.5, xmax=5,
                    ymin=0, ymax=2.5) +
  scale_x_continuous(limits=c(0, 5)) +
  scale_y_continuous(limits=c(0, 4))


## ----echo=FALSE, include=FALSE-------------------------------------------
opts_chunk$set(opts_fig_medium)


## ------------------------------------------------------------------------
print(qplot(1,1), vp=viewport(height=0.8))
grid.text(0.5, unit(1,"npc") - unit(1,"line"),
          label="I'm (slightly) out of here!")


## ----eval=FALSE----------------------------------------------------------
## fig1 <- ggplot(data.frame(x=-3:3), aes(x=x)) +
##   stat_function(fun=dnorm)
## pdf(file="fig1.pdf", width=8, height=6)
## print(fig1)
## dev.off()


## ----eval=FALSE----------------------------------------------------------
## postscript(file="fig1.eps", width=8, height=6)
## print(fig1)
## dev.off()


## ----eval=FALSE----------------------------------------------------------
## tiff(file="fig1.tiff", width=1000, height=800)
## print(fig1)
## dev.off()


## ------------------------------------------------------------------------
try(detach(package:scales))
try(detach(package:plyr))
try(detach(package:Hmisc))
try(detach(package:ggplot2))
try(detach(package:grid))




## ------------------------------------------------------------------------
Sys.info()


## ----eval=FALSE, echo=FALSE----------------------------------------------
## R.Version()


## ------------------------------------------------------------------------
sessionInfo()


