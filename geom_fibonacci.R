library(ggplot2)
library(grid)

#--------------------------------------------
# Helper function to generate Fibonacci spiral
#--------------------------------------------
fibonacci_spiral_df <- function(turns = 3, n = 2000, c = 0.5) {
  phi <- (1 + sqrt(5)) / 2
  k   <- log(phi) / (pi / 2)   # growth rate
  theta_max <- turns * 2 * pi

  theta <- seq(0, theta_max, length.out = n)
  r <- c * exp(k * theta)

  data.frame(
    x = r * cos(theta),
    y = r * sin(theta)
  )
}

#--------------------------------------------
#   Define new geom: GeomFibonacci
#--------------------------------------------
GeomFibonacci <- ggproto(
  "GeomFibonacci", Geom,

  default_aes = aes(
    colour = "black",
    linewidth = 1,
    alpha = 1
  ),

  required_aes = character(0),

  draw_panel = function(data, panel_scales, coord,
                        turns = 3, n = 2000, c = 0.5) {

    spiral <- fibonacci_spiral_df(turns = turns, n = n, c = c)

    coords <- coord$transform(
      data.frame(
        x = spiral$x,
        y = spiral$y,
        colour = data$colour[1],
        linewidth = data$linewidth[1],
        alpha = data$alpha[1]
      ),
      panel_scales
    )

    grid::linesGrob(
      x = coords$x,
      y = coords$y,
      gp = grid::gpar(
        col = coords$colour[1],
        lwd = coords$linewidth[1],
        alpha = coords$alpha[1]
      )
    )
  }
)

#--------------------------------------------
# User-facing function
#--------------------------------------------
geom_fibonacci <- function(mapping = NULL, data = NULL,
                           turns = 3, n = 2000, c = 0.5,
                           ..., inherit.aes = TRUE) {

  layer(
    geom = GeomFibonacci, mapping = mapping, data = data,
    stat = "identity", position = "identity",
    params = list(
      turns = turns, n = n, c = c,
      ...
    ),
    inherit.aes = inherit.aes
  )
}
