trim <- function(x) {
  bb <- attr(x, "bb")
  raster_dim <- dim(x)
  pix <- data.frame(ll.lat = 0, ll.lon = 0, ur.lat = raster_dim[1], ur.lon = raster_dim[2])
  if (bb$ll.lat < -90 || bb$ur.lat > 90) {
    range.lat <- bb$ur.lat - bb$ll.lat

  }
  if (bb$ll.lon <= -180 || bb$ur.lon >= 180) {
    range.lon <- bb$ur.lon - bb$ll.lon
    pix.to.trim <- (range.lon - 358) * pix$ur.lon / range.lon
    # we assume centred map in raster
    pix.tail <- pix.to.trim / 2
    x.out <- x[ , (pix.tail+1):(pix$ur.lon-pix.tail)]
    bb.out <- data.frame(ll.lat = bb$ll.lat, ll.lon = -179, ur.lat=bb$ur.lat, ur.lon=179)
    attr(x.out, "bb") <- bb.out
#    class(x.out) <- c("ggmap", "raster")
  }
  return(x.out)
}

