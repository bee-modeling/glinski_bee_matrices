# have to create a custom theme class to drop x-axis tick makrs and labels
# https://stackoverflow.com/questions/70189692/ggplot2-geom-boxplot-cannot-remove-x-axis-tics-and-text
theme_classic_custom <- function(base_size = 11, base_family = "", base_line_size = base_size / 22,
                                 base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) %+replace%
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line = element_line(
        colour = "black",
        size = rel(1)
      ), legend.key = element_blank(),
      strip.background = element_rect(
        fill = "white", colour = "black",
        size = rel(2)
      ), complete = TRUE
    )
}
