# Make a ggplot2 image of 3 distributions
make_ggplot_of_dists <- function() {
    
    # Dependancies
    require(hexSticker)
    require(ggplot2)
    require(showtext)
    require(dplyr)
    require(ggthemes)

    # Three normal distributions
    log_x_vals <- seq(-10, 10, .01)
    x_vals <- exp(log_x_vals)
    norm_1 <- norm_2 <- norm_3 <- vector("numeric", length(log_x_vals))
    norm_1 <- dnorm(x = log_x_vals, mean = 3, sd = 3)
    norm_2 <- dnorm(x = log_x_vals, mean = .25, sd = 1.5)
    norm_3 <- dnorm(x = log_x_vals, mean = 2.8, sd = .9)

    # Combine
    df <- data.frame( 
        density = c(norm_1, norm_2, norm_3),
        x = rep(log_x_vals, 3),
        analysis = factor(rep(1:3, each = length(x_vals)))
    )

    # Plot
    ggplot(df) + 
        geom_density(aes(x = x, y = density, fill = analysis),
                     stat = "identity",
                     alpha = 0.5,
                     show_guide = FALSE) + 
        scale_fill_manual(values = c("#F5B700", "#00A1E4", "#DC0073"))
        theme_void()
}

make_ggplot_of_dists()