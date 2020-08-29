
# build custom theme
theme_custom <- function()
  theme_minimal() +
  theme(
    strip.background = element_rect(
      fill = "gray95",
      color = 'white'),
    strip.text = element_text(
      color = "gray30",
      size = 11,
      face = "bold"
    )
  )

# set custom theme
theme_set(theme_custom())

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

# set default discrete colors
scale_colour_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}
