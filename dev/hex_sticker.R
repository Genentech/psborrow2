# Color palette reference: https://coolors.co/04e762-f5b700-00a1e4-dc0073-89fc00

# Make a ggplot2 image of 3 distributions
make_ggplot_of_dists <- function() {
  # Dependancies
  require(ggplot2)
  require(showtext)
  require(dplyr)
  require(ggthemes)

  # Three normal distributions
  log_x_vals <- seq(-15, 15, .01)
  x_vals <- exp(log_x_vals)
  norm_1 <- norm_2 <- norm_3 <- vector("numeric", length(log_x_vals))
  norm_1 <- dnorm(x = log_x_vals, mean = 3, sd = 3)
  norm_2 <- dnorm(x = log_x_vals, mean = .25, sd = 1.5)
  norm_3 <- dnorm(x = log_x_vals, mean = 2, sd = .9)

  # Combine
  df <- data.frame(
    density = c(norm_1, norm_2, norm_3),
    x = rep(log_x_vals, 3),
    analysis = factor(rep(1:3, each = length(x_vals)))
  )

  # Plot
  ggplot(df) +
    geom_density(
      aes(
        x = x,
        y = density,
        fill = analysis,
        color = analysis
      ),
      stat = "identity",
      alpha = 0.8,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("#F5B700", "#00A1E4", "#DC0073")) +
    scale_color_manual(values = c("#F5B700", "#00A1E4", "#DC0073")) +
    theme_void()
}

make_hexplot <- function(out_path = "./man/figures/hex.png") {
  # Dependancies
  require(hexSticker)
  require(showtext)

  # HexSticker
  hexSticker::sticker(
    subplot = make_ggplot_of_dists(),
    package = "psborrow2",
    p_size = 140,
    p_color = "#094F26",
    p_y = 1.4,
    s_y = .83,
    s_x = .7,
    s_width = 4.5,
    s_height = .7,
    h_fill = "#b7fed4",
    h_color = "#094F26",
    h_size = 2,
    url = "github.com/Genentech/psborrow2",
    u_size = 24,
    filename = out_path,
    p_family = "mono",
    dpi = 2000
  )
}

make_hexplot()
