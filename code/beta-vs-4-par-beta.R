base_path <- "~/Repositories/XBX-supplementary/"
fig_path <- file.path(base_path, "figures/")

library("betareg")
library("ggplot2")
save_plot <- FALSE

## Beta density versus four parameter Beta density

settings <- expand.grid(mu = c(0.5, 0.7, 0.9),
                        phi = c(0.5, 2, 20),
                        u = c(0.01, 0.1, 0.5))
settings_id <- 1:nrow(settings)

settings <- settings |>
    transform(mu_lab = factor(settings_id,
                              labels = paste0("mu == ", mu),
                              ordered = TRUE),
              u_lab = factor(settings_id,
                              labels = paste0("u == ", u),
                              ordered = TRUE),
              phi_lab = factor(settings_id,
                               labels = paste0("phi == ", phi),
                               ordered = TRUE))


## Values to consider (dense close to the bound, sparse in the middle)
z <- sort(c(1:99/2000, 1:999/1000, 1 - 1:99/2000))

## Density computatioon
dd <- lapply(settings_id, function(j) {
    with(settings[j, ], {
        density_beta <- dbetar(z, mu, phi)
        znew <- -u + z * (1 + 2 * u)
        density_beta4 <- dbeta4(znew, mu, phi, -u, u + 1)
        data.frame(
            z = c(z, znew),
            density = c(density_beta, density_beta4),
            phi_lab = phi_lab,
            mu_lab = mu_lab,
            u_lab = u_lab,
            distribution = factor(rep(c("beta", "4-parameter beta"), each = length(z)),
                                  levels = c("beta", "4-parameter beta"),
                                  ordered = TRUE))
    })
})
dd <- do.call("rbind", dd)

## Plotting
gr <- gray.colors(2, start = 0.45, end = 0.85)

fig <- ggplot(dd) +
    geom_hline(aes(yintercept = 1), lty = 3, lwd = 0.4) +
    geom_ribbon(aes(ymin = 0, ymax = density, x = z,
                    fill = distribution), alpha = 0.8) +
    geom_vline(aes(xintercept = rep(0, nrow(dd))), color = gr[1], lty = 2) +
    geom_vline(aes(xintercept = rep(1, nrow(dd))), color = gr[1], lty = 2) +
    facet_grid(u_lab ~ phi_lab + mu_lab,
               labeller = labeller(mu_lab = label_parsed,
                                   phi_lab = label_parsed,
                                   u_lab = label_parsed)) +
    labs(x = "y", y = "density") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "top") +
    scale_fill_manual(values = gr[c(2, 1)]) +
    scale_x_continuous(breaks = c(-0.5, 0.5, 1.5)) +
    coord_cartesian(x = c(-0.55, 1.55), y = c(0, 5))



if (save_plot) {
    pdf(file.path(fig_path, "beta-vs-4-par-beta.pdf"), width = 9, height = 4)
    print(fig)
    dev.off()
} else {
    print(fig)
}

