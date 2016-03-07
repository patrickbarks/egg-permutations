

# adjust transparency of raster array such that alpha increases
#  with distance from raster centroid based on logistic equation with
#  parameters a, b (band is alpha channel; usually 4)

RasterAlpha <- function(ras, band = 4, a, b) {
  require(raster)
  
  ras_alpha <- raster(as.matrix(ras[,,band]))
  l <- nrow(ras)
  w <- ncol(ras)
  
  base <- raster(matrix(NA, l, w))
  base[round(l/2), round(w/2)] <- 1
  projection(base) = "+proj=tmerc +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=m"
  
  dx <- distance(base)
  fx <- calc(dx, function(x) x / max(x))
  fx <- calc(fx, function(x) 1 / (1 + exp(a * (x - b))))
  fx <- calc(fx, function(x) x / max(x))
  
  ras_alpha_new <- overlay(ras_alpha, fx, fun = function(x,y) x * y)
  ras_alpha_new <- calc(ras_alpha_new, function(x) x / max(x))
  
  ras[,,band] <- as.matrix(ras_alpha_new)
  
  return(ras)
}
