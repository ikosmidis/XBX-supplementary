base_path <- "~/Repositories/XBX-supplementary/"
fig_path <- file.path(base_path, "figures/")
results_path <- file.path(base_path, "results/")

library("betareg")
library("distributions3")
library("ggplot2")

save_plot <- FALSE

parameters <- expand.grid(
    mu = 0:100 / 100,
    phi = c(0.5, 2, 10, 50, 100, 200),
    nu = 2^seq(0, 10, 2) / 100)
pars <- rbind(
    parameters |>
    transform(mean = mean(XBetaX(mu, phi, nu)),
              variance = variance(XBetaX(mu, phi, nu)),
              distribution = "XBX"),
    parameters |>
    transform(mean = mean(BetaR(mu, phi)),
              variance = variance(BetaR(mu, phi)),
              distribution = "Beta")
)
pars <- pars |>
    transform(
        mu_labs = factor(paste("mu ==", mu), levels = paste("mu ==", sort(unique(mu))), ordered = TRUE),
        phi_labs = factor(paste("phi ==", phi), levels = paste("phi ==", sort(unique(phi))), ordered = TRUE),
        nu_labs = factor(paste("nu ==", nu), levels = paste("nu ==", sort(unique(nu))), ordered = TRUE))


e_xbx <- ggplot(pars) +
    geom_line(aes(mu, mean, col = distribution)) +
    labs(x = expression(mu), y = "Expectation") +
    facet_grid(phi_labs ~ nu_labs, labeller = label_parsed) +
    scale_color_manual(values = c("gray", "black")) +
    theme_minimal() +
    theme(legend.position = "top")

v_xbx <- ggplot(pars) +
    geom_line(aes(mu, variance, col = distribution)) +
    labs(x = expression(mu), y = "Variance") +
    facet_grid(phi_labs ~ nu_labs, labeller = label_parsed) +
    scale_color_manual(values = c("gray", "black")) +
    theme_minimal() +
    theme(legend.position = "top")

if (save_plot) {
    grDevices::pdf(file.path(fig_path, paste0("xbx-e.pdf")),
                   width = 10, height = 8)
    print(e_xbx)
    dev.off()
    grDevices::pdf(file.path(fig_path, paste0("xbx-v.pdf")),
                   width = 10, height = 8)
    print(v_xbx)
    dev.off()
} else {
    print(e_xbx)
    print(v_xbx)
}
