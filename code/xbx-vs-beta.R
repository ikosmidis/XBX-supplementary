base_path <- "~/Repositories/XBX-supplementary/"
fig_path <- file.path(base_path, "figures/")

library("ggplot2")
library("betareg")
library("distributions3")
save_plot <- FALSE

## Parameter settings to consider
settings <- expand.grid(mu = c(0.5, 0.7, 0.9),
                        phi = c(0.5, 2, 20),
                        nu = c(0.01, 0.1, 1))
settings_id <- 1:nrow(settings)
settings$xbx <- with(settings, XBetaX(mu, phi, nu))
settings$beta <- with(settings, BetaR(mu, phi))
settings <- settings |>  transform(mu_lab = factor(settings_id,
                                                   labels = paste0("mu == ", mu),
                                                   ordered = TRUE),
                                   nu_lab = factor(settings_id,
                                                   labels = paste0("nu == ", nu),
                                                   ordered = TRUE),
                                   phi_lab = factor(settings_id,
                                                    labels = paste0("phi == ", phi),
                                                    ordered = TRUE))

z <- seq(0, 1, length = 1000)
dd <- lapply(settings_id, function(j) {
    with(settings[j, ], {
        d_xbx <- pdf(xbx, z, quad = 50)
        d_beta <- pdf(beta, z)
        data.frame(
            z = c(z, z),
            density = c(d_beta, d_xbx),
            mu = mu,
            phi = phi,
            nu = nu,
            phi_lab = phi_lab,
            mu_lab = mu_lab,
            nu_lab = nu_lab,
            distribution = factor(rep(c("beta", "XBX"), each = length(z)),
                                  levels = c("beta", "XBX"),
                                  ordered = TRUE))
    })
})
dd <- do.call("rbind", dd)
gr <- gray.colors(3, start = 0.45, end = 0.85)

fig <- ggplot(dd) +
    geom_hline(aes(yintercept = 1), lty = 3, linewidth = 0.4) +
    geom_ribbon(data = subset(dd, z > 0 & z < 1), aes(ymin = 0, ymax = density, x = z,
                                                      fill = distribution), alpha = 0.8) +
    geom_segment(data = dd |> subset(z %in% c(0, 1) & distribution == "XBX"),
                 aes(x = z, xend = z, y = rep(0, length(z)), yend = density)) +
    geom_point(data = dd |> subset(z %in% c(0, 1) & distribution == "XBX"),
               aes(x = z, y = density), size = 0.4) +
    facet_grid(nu_lab ~ phi_lab + mu_lab,
               labeller = labeller(mu_lab = label_parsed,
                                   phi_lab = label_parsed,
                                   nu_lab = label_parsed)) +
    labs(x = "y", y = "density") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "top") +
    scale_fill_manual(values = gr[c(2, 1)]) +
    scale_x_continuous(breaks = c(0, 0.5, 1)) +
    coord_cartesian(x = c(-0.05, 1.05), y = c(0, 5))

if (save_plot) {
    grDevices::pdf(file.path(fig_path, "xbx-vs-beta.pdf"), width = 9, height = 4)
    print(fig)
    dev.off()
} else {
    print(fig)
}
