# Split violin geom, adapted from introdataviz::geom_split_violin
# (PsyTeachR, https://github.com/PsyTeachR/introdataviz, MIT licence).
# Vendored rather than installed: the package drags ~100 dependencies (lme4, papaja,
# nloptr needing CMake, ...) into the image for this single function.
# Each pair of groups at an x position is drawn as two half-violins: odd groups on the
# left, even groups on the right - so the fill variable must have exactly 2 levels.
# Quantile lines are not supported (overlay a boxplot instead).

GeomSplitViolin <- ggplot2::ggproto("GeomSplitViolin", ggplot2::GeomViolin,
  draw_group = function(self, data, panel_params, coord, ...) {
    data <- transform(data,
                      xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x))
    left <- data[1, "group"] %% 2 == 1
    data$x <- if (left) data$xminv else data$xmaxv
    data <- data[order(if (left) data$y else -data$y), ]
    # close the half-violin along the centre line
    data <- rbind(data[1, ], data, data[nrow(data), ], data[1, ])
    data[c(1, nrow(data) - 1, nrow(data)), "x"] <- round(data[1, "x"])
    ggplot2::GeomPolygon$draw_panel(data, panel_params, coord)
  }
)

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                              position = "identity", ..., trim = TRUE, scale = "area",
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(trim = trim, scale = scale, na.rm = na.rm, ...))
}
