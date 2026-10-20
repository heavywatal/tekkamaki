library(conflicted)
library(tidyverse)

radius = 54
ylim = c(-radius, radius)
xlim = ylim * sqrt(3) / 2

p = ggplot() +
  wtl::annotate_regpolygon(6L, radius, fill = "#303030", linewidth = 0) +
  wtl::annotate_regpolygon(6L, radius - 6, fill = "#ffffff", linewidth = 0) +
  wtl::annotate_regpolygon(6L, radius * 2 / 5, fill = "#C41A41", linewidth = 0) +
  coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void()
p

height = 2
width = sqrt(3)
ggsave(
  "tekkamaki.png",
  p,
  height = height,
  width = width,
  dpi = 278 * 2 / height,
  bg = "transparent"
) |>
  wtl::oxipng()

ggsave("tekkamaki.svg", p, height = height, width = width, bg = "transparent") |>
  wtl::logo_svg_optimize()

use_logo("tekkamaki.svg")
