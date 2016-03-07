
# load libraries
library(png)
library(raster)
library(animation)
library(ggplot2)
library(grid)
library(gridBase)
library(gridExtra)
library(dplyr)

# setwd
setwd('~/Desktop/data-projects/egg-permutations/R')

# source fn to adjust raster transparency
source('fn-alpha.R')

# combinatorics
2^12                                                     # 12-egg carton, non-unique eggs
2^24                                                     # 24-egg carton, non-unique eggs
sum(choose(12, 0:12) * (factorial(12)/factorial(0:12)))  # 12-egg carton, unique eggs
sum(choose(24, 0:24) * (factorial(24)/factorial(0:24)))  # 24-egg carton, unique eggs

# generate all 2^12 possible arrangements of 0:12 eggs in a 12-egg carton
#  for each of the 12 spots, there may (1) or may not (0) be an egg
arr <- expand.grid(1:0, 1:0, 1:0, 1:0, 1:0, 1:0,
                   1:0, 1:0, 1:0, 1:0, 1:0, 1:0,
                   stringsAsFactors = F,
                   KEEP.OUT.ATTRS = F)

# re-order rows of arr by number of eggs
arr <- as.matrix(arr[order(rowSums(arr)), 12:1])

# load images
bg <- readPNG("../img/carton_empty.png")
e01 <- readPNG("../img/e01.png")
e02 <- readPNG("../img/e02.png")
e03 <- readPNG("../img/e03.png")
e04 <- readPNG("../img/e04.png")
e05 <- readPNG("../img/e05.png")
e06 <- readPNG("../img/e06.png")
e07 <- readPNG("../img/e07.png")
e08 <- readPNG("../img/e08.png")
e09 <- readPNG("../img/e09.png")
e10 <- readPNG("../img/e10.png")
e11 <- readPNG("../img/e11.png")
e12 <- readPNG("../img/e12.png")

# adjust transparency
egg01 <- RasterAlpha(e01, a = 40, b = 0.65)
egg02 <- RasterAlpha(e02, a = 40, b = 0.66)
egg03 <- RasterAlpha(e03, a = 40, b = 0.66)
egg04 <- RasterAlpha(e04, a = 40, b = 0.65)
egg05 <- RasterAlpha(e05, a = 40, b = 0.65)
egg06 <- RasterAlpha(e06, a = 40, b = 0.65)
egg07 <- RasterAlpha(e07, a = 40, b = 0.65)
egg08 <- RasterAlpha(e08, a = 40, b = 0.65)
egg09 <- RasterAlpha(e09, a = 40, b = 0.66)
egg10 <- RasterAlpha(e10, a = 40, b = 0.64)
egg11 <- RasterAlpha(e11, a = 40, b = 0.66)
egg12 <- RasterAlpha(e12, a = 40, b = 0.65)

# create list of egg images
eggs <- list(egg01, egg02, egg03, egg04, egg05, egg06,
             egg07, egg08, egg09, egg10, egg11, egg12)

# original egg positions and dimensions (in cm)
posdim_orig <- read.csv("../dat/posdim_orig.csv")

# original carton dimensions
carton_height <- 6.11
carton_width <- 17.27

# carton relative position within plot (with dims 1,1)
posdim <- posdim_orig %>% 
  transmute(x = pos_horiz / carton_width,
            y = 1 - (pos_vert / carton_height),
            w = width / carton_width,
            h = height / carton_height)

# wrapper function to draw eggs at specified positions (indices) within carton
RasterWrapper <- function(plot, indices) {
  for (i in indices) {
    eggGrob <- rasterGrob(eggs[[i]],
                          x = posdim[i,1],
                          y = posdim[i,2],
                          width = posdim[i,3],
                          height = posdim[i,4],
                          just = c('left', 'top'))
    plot <- plot + annotation_custom(eggGrob)
  }
  return(plot)
}

# empty carton grob
bg_grob <- rasterGrob(bg)

# base plot
pl <- qplot(0:100, 0:100, geom = "blank") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(ratio = carton_height/carton_width) +
  annotation_custom(bg_grob) +
  theme_void()

# variables for table tracking permutations
ns <- rowSums(arr)
tot_by_n <- unlist(sapply(0:12, function (x) seq(1, choose(12, x))))

# table to track permutations
perm_tab <- matrix(nrow = 2, ncol = 13)
perm_tab[1,] <- 0:12
perm_tab <- as.data.frame(perm_tab)
perm_tab[2,] <- ''
names(perm_tab) <- NULL
rownames(perm_tab) <- c("eggs", "permutations")

# tableGrob theme
t1 <- ttheme_default(core = list(fg_params = list(fontsize = 24)),
                     rowhead = list(fg_params = list(hjust = 1,
                                                     x = 0.95,
                                                     fontsize = 26)))

# save animation
saveVideo({
  
  # first frame
  tbl <- tableGrob(perm_tab,
                   theme = t1,
                   widths = unit(rep(24, 13), 'mm'),
                   heights = unit(rep(15, 2), 'mm'))
  
  grid.arrange(pl, tbl, nrow = 2, heights = c(0.8, 0.2))

  # subsequent plot frames 1:n
  for (i in 1:nrow(arr)) {
  
    perm_tab[2, ns[i]+1] <- tot_by_n[i]

    tbl_panel <- tableGrob(perm_tab,
                           theme = t1,
                           widths = unit(rep(24, 13), 'mm'),
                           heights = unit(rep(15, 2), 'mm'))
    
    eggs_to_plot <- which(arr[i,] == 1)
    egg_panel <- RasterWrapper(pl, eggs_to_plot)
    
    grid.arrange(egg_panel, tbl_panel, nrow = 2, heights = c(0.8, 0.2))
 
  }
}, ani.dev = "png", ani.height = 520, ani.width = 1080, interval = 0.15,
   video.name = "../vid/vid-init.mp4")




