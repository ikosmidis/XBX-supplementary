base_path <- "~/Repositories/XBX-supplementary/"
fig_path <- file.path(base_path, "figures/")

library("ggplot2")
library("distributions3")
library("betareg")
library("crch")

save_plot <- FALSE

match_moments <- function(xbx_mom) {
    l2 <- function(pars) {
        cn <- CensoredNormal(pars[1], exp(pars[2]), left = 0, right = 1)
        (xbx_mom[1] - mean(cn))^2 + (xbx_mom[2] - variance(cn)^0.5)^2
    }
    res <- nlminb(c(0, 1), l2)
    rval <- c("mean" = res$par[1], "sd" = exp(res$par[2]))
    attr(rval, "l2") <- res$objective
    rval
}

## Parameter settings to consider

settings <- expand.grid(mu = c(0.5, 0.7, 0.9),
                        phi = c(0.5, 2, 5, 20),
                        nu = c(0.01, 0.1, 1))
settings_id <- 1:nrow(settings)

settings$xbx <- with(settings, XBetaX(mu, phi, nu))
settings <- settings |> transform(expectation = mean(xbx), stdev = variance(xbx)^0.5)
moms <- apply(settings[c("expectation", "stdev")], 1, match_moments)
settings <- settings |> transform(cn_mu = moms[1, ], cn_sigma = moms[2, ])
settings$cn <- with(settings, CensoredNormal(cn_mu, cn_sigma, 0, 1))
settings <- settings |>  transform(mu_lab = factor(settings_id,
                                                   labels = paste0("mu == ", mu),
                                                   ordered = TRUE),
                                   nu_lab = factor(settings_id,
                                                   labels = paste0("nu == ", nu),
                                                   ordered = TRUE),
                                   phi_lab = factor(settings_id,
                                                    labels = paste0("phi == ", phi),
                                                    ordered = TRUE))

## Test things out
## max(abs(crch:::sdcnorm(settings$cn_mu, settings$cn_sigma, 0, 1) - settings$stdev))
## max(abs(crch:::ecnorm(settings$cn_mu, settings$cn_sigma, 0, 1) - settings$expectation))

set.seed(123)
z <- seq(0, 1, length = 1000)
dd <- lapply(settings_id, function(j) {
    with(settings[j, ], {
        d_xbx <- pdf(xbx, z, quad = 50)
        d_cn <- pdf(cn, z)
        data.frame(
            s = c(random(cn, length(z)), random(xbx, length(z))),
            z = c(z, z),
            density = c(d_cn, d_xbx),
            mu = mu,
            phi = phi,
            nu = nu,
            expectation = expectation,
            stdev = stdev,
            cn_mu = cn_mu,
            cn_sigma = cn_sigma,
            phi_lab = phi_lab,
            mu_lab = mu_lab,
            nu_lab = nu_lab,
            distribution = factor(rep(c("CN", "XBX"), each = length(z)),
                                  levels = c("CN", "XBX"),
                                  ordered = TRUE))
    })
})
dd <- do.call("rbind", dd)

gr <- gray.colors(3, start = 0.45, end = 0.85)

fig1 <- ggplot(dd) +
    geom_hline(aes(yintercept = 1), lty = 3, linewidth = 0.4) +
    geom_ribbon(data = subset(dd, z > 0 & z < 1), aes(ymin = 0, ymax = density, x = z,
                                                      fill = distribution)) +
    geom_segment(data = dd |> subset(z %in% c(0, 1)),
                 aes(x = z, xend = z, y = rep(0, length(z)), yend = density)) +
    geom_point(data = dd |> subset(z %in% c(0, 1)),
               aes(x = z, y = density), size = 0.4) +
    facet_grid(nu_lab + distribution ~ phi_lab + mu_lab,
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
    grDevices::pdf(file.path(fig_path, "xbx-vs-cn-pdf.pdf"), width = 9, height = 8)
    print(fig1)
    dev.off()
} else {
    print(fig1)
}

fig2 <- ggplot(dd) +
    geom_histogram(aes(x = s, y = after_stat(count),
                       fill = distribution), bins = 12) +
    facet_grid(nu_lab + distribution ~ phi_lab + mu_lab,
               labeller = labeller(mu_lab = label_parsed,
                                   phi_lab = label_parsed,
                                   nu_lab = label_parsed)) +
    labs(x = "y", y = "count") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "top") +
    scale_fill_manual(values = gr[c(2, 1)]) +
    scale_x_continuous(breaks = c(0, 0.5, 1))


if (save_plot) {
    grDevices::pdf(file.path(fig_path, "xbx-vs-cn-hist.pdf"), width = 9, height = 8)
    print(fig2)
    dev.off()
} else {
    print(fig2)
}

