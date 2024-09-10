base_path <- "~/Repositories/XBX-supplementary/"
fig_path <- file.path(base_path, "figures/")
results_path <- file.path(base_path, "results/")

library("betareg")
library("crch")
## install.packages("topmodels", repos = "https://zeileis.r-universe.dev")
library("topmodels")
library("parallel")
library("ggplot2")
library("dplyr")

save_plot <- FALSE

## Number of processes to use for the simulation study
n_cores <- 9
## Number of samples per parameter setting
nrep <- 100
## Number of observations in each sample
nobs <- 500
## Average boundary probability threshold
prob_thres <- 0.95


## DGP: XBeta(mu = plogis(beta0 + beta1 * x), phi = exp(gamma0 + gamma1 * x), nu)
dgp <- function(nobs = 500, beta0 = -1, beta1 = 1, gamma0 = 1.5, gamma1 = 0, nu = 0.3) {
    d <- data.frame(x = seq(from = -1, to = 1, length.out = nobs))
    d$mu <- plogis(beta0 + beta1 * d$x)
    d$phi <- exp(gamma0 + gamma1 * d$x)
    d$nu <- nu
    d$y <- rxbeta(n = nobs, mu = d$mu, phi = d$phi, nu = d$nu)
    return(d)
}

## replace response with a newly simulated response on the same grid
newdgp <- function(data) {
    data$y <- rxbeta(n = nrow(data), mu = data$mu, phi = data$phi, nu = data$nu)
    return(data)
}

avg_boundary_prob <-  function(nobs = 500, beta0 = -1, beta1 = 1, gamma0 = 1.5, gamma1 = 0, nu = 0.3) {
    x <- seq(from = -1, to = 1, length.out = nobs)
    mu <- plogis(beta0 + beta1 * x)
    phi <- exp(gamma0 + gamma1 * x)
    mean(pxbeta(0, mu = mu, phi = phi, nu = nu) + pxbeta(1, mu = mu, phi = phi, nu = nu, lower.tail = FALSE))
}

get_beta <- function(mu_interval) {
    etas <- qlogis(mu_interval)
    c(mean(etas), diff(etas)/2)
}

get_gamma <- function(phi_interval) {
    zetas <- log(phi_interval)
    c(mean(zetas), diff(zetas)/2)
}

trapezoidalRule <- function(x, y) {
    N <- length(x)
    if (N != length(y)) stop("x and y must have the same length")
    sum(diff(x) * (y[-1] + y[-N]))/2
}

## u values to consider
nus <- 2^(-6:1)

## Compute beta0 and beta1 for supplied mu ranges
mu_ranges <- data.frame(mu_low = c(0.05, 0.05, 0.05, 0.05,
                                   0.25, 0.25, 0.25,
                                   0.50, 0.50,
                                   0.75),
                        mu_upp = c(0.25, 0.50, 0.75, 0.95,
                                   0.50, 0.75, 0.95,
                                   0.75, 0.95,
                                   0.95))
mu_ranges[c("beta0", "beta1")] <- t(apply(mu_ranges, 1, get_beta))

## Compute gamma0 and gamma1 for supplied phi ranges
phi_ranges <- data.frame(phi_low = c(0.5, 0.5, 0.5, 5, 10, 20, 50),
                      phi_upp = c(10.0, 20.0, 50.0, 100, 20, 50, 100))
phi_ranges[c("gamma0", "gamma1")] <- t(apply(phi_ranges, 1, get_gamma))

## Construct all possible combinations of mu and phi ranges
exp_settings <- cbind(mu_ranges[rep(1:nrow(mu_ranges), each = nrow(phi_ranges)), ], phi_ranges)

## Construct all possible combinations of mu and phi ranges  and nu values
exp_settings <- cbind(exp_settings[rep(1:nrow(exp_settings), each = length(nus)), ], nu = nus)

## Compute average boundary probability
exp_settings <- exp_settings |>
    rowwise() |>
    mutate(avg_bp = avg_boundary_prob(nobs, beta0, beta1, gamma0, gamma1, nu))

## Contruct factor with the mu and phi ranges
exp_settings <- exp_settings |>
    mutate(fmu = paste0("'(", round(mu_low, 3), ",", round(mu_upp, 3), ")'"),
           fphi = paste0("'(", round(phi_low, 3), ",", round(phi_upp, 3), ")'"))


## Settings with average boundary probability less than prob_thres
exp_settings <- exp_settings |>
    mutate(include = factor(ifelse(avg_bp < prob_thres, "yes", "no"),
                            levels = c("no", "yes"), ordered = TRUE))

## Example data sets
set.seed(0)
ds <- lapply(1:nrow(exp_settings), function(i) {
    current_setting <- exp_settings[i, ]
    dat <- with(current_setting,
                dgp(500, beta0 = beta0, beta1 = beta1,
                    gamma0 = gamma0, gamma1 = gamma1, nu = nu))
    within(dat, {
        beta0 <- current_setting$beta0
        beta1 <- current_setting$beta1
        gamma0 <- current_setting$gamma0
        gamma1 <- current_setting$gamma1
        nu <- current_setting$nu
        fmu <- current_setting$fmu
        fphi <- current_setting$fphi
        avg_bp <- current_setting$avg_bp
        color <- if (current_setting$include == "yes") gray(0.3) else gray(0.7)
    })
})
ds <- do.call("rbind", ds)

for (w in seq_along(nus)) {
    c_nu <- nus[w]
    fig <- ggplot(ds |> subset(nu == c_nu)) +
        geom_point(aes(x, y, color = I(color)), alpha = 0.2) +
        geom_rug(aes(x, y), sides = "l", alpha = 0.1) +
        facet_grid(fphi ~ fmu, labeller = label_parsed) +
        theme_bw() +
        theme(legend.position = "bottom") +
        labs(title = bquote(u == 2^.(log(nus[w], 2)))) +
        scale_x_continuous(breaks = c(-1, 0, 1))
    if (save_plot) {
        grDevices::pdf(file.path(fig_path, paste0("xbeta-data-nu-", w, ".pdf")),
                       width = 8, height = 5.5)
        print(fig)
        dev.off()
    } else {
        print(fig)
    }
}

## simulation
if (file.exists(file.path(results_path, "xbetax.rds"))) {

    results <- readRDS(file.path(results_path, "xbetax.rds"))

} else {

    ## Add repetitions and number of observations
    simu_settings <- cbind(
        exp_settings[rep(1:nrow(exp_settings), each = nrep), ],
        repetition = 1:nrep,
        nobs = nobs
    )
    simu_settings <- subset(simu_settings, include == "yes")
    rownames(simu_settings) <- NULL


    RNGkind("L'Ecuyer-CMRG")
    set.seed(0)
    results <- mclapply(1:nrow(simu_settings), function(i) {
        current_setting <- simu_settings[i, ]
        current_setting$xbetax <- current_setting$htobit <- NA
        d <- with(current_setting, dgp(nobs   = nobs,
                                       beta0  = beta0,
                                       beta1  = beta1,
                                       gamma0 = gamma0,
                                       gamma1 = gamma1,
                                       nu     = nu))

        ## Fit XBX and 2-limit tobit
        start_xbx <- with(current_setting, c(beta0, beta1, gamma0, gamma1, log(nu)))
        m_xbx <- try(betareg(y ~ x | x, data = d, dist = "xbetax", start = start_xbx))
        m_ht <- try(crch(y ~ x | x, data = d, left = 0, right = 1))

        ## Compute CRPS on new data
        nd <- newdgp(d)
        if (!inherits(m_xbx, "try-error"))
            current_setting$xbetax <- proscore(m_xbx, newdata = nd, type = "crps",
                                               aggregate = mean, drop = TRUE)
        if (!inherits(m_ht, "try-error"))
            current_setting$htobit <- proscore(m_ht, newdata = nd, type = "crps",
                                               aggregate = mean, drop = TRUE)
        ## Report
        if (isTRUE(i %% nrep == 0)) {
            with(current_setting, {
                cat("setting:", i, "out of", nrow(simu_settings), "\t",
                    "mu:", mu_low, "-", mu_upp, "\t",
                    "phi:", phi_low, "-", phi_upp, "\t",
                    "nu:", nu, "\n")
            })
        }
        current_setting
    }, mc.cores = n_cores)
    RNGkind(kind = "default")

    ## rbind and compute relative change in CRPS when moving from xbetax to htobit
    results <- do.call("rbind", results) |>
        mutate(rel_crps = (htobit/xbetax - 1))

    saveRDS(results, file = file.path(results_path, "xbetax.rds"))
}



a_results <-  aggregate(rel_crps ~ nu + fmu + fphi,
                        data = results,
                        FUN = mean, na.rm = TRUE)
## Linear interpolation for polygons
nus_x <- 2^(seq(-6, 1, length.out = 100))
a_results_smooth <- tapply(a_results, ~ fmu + fphi, function(x) {
    data.frame(nu = nus_x, rel_crps = approx(log(x$nu), x$rel_crps, xout = log(nus_x))$y, fmu = unique(x$fmu), fphi = unique(x$fphi))
}, simplify = FALSE)
a_results_smooth <- do.call("rbind", c(a_results_smooth)) |>
    transform(sign = factor(ifelse(rel_crps < 0, "-", "+"), levels = c("+", "-")))

## Find smallest u such that avg boundary probability is larger than prob_thres per setting
## and keep those that are less than max(nus)
bp <- unique(exp_settings |> select(beta0, beta1, gamma0, gamma1, fmu, fphi)) |>
    group_by(fmu, fphi) |>
    rowwise() |>
    summarize(nu = uniroot(function(u) avg_boundary_prob(nobs, beta0, beta1, gamma0, gamma1, u) - prob_thres, c(0, 100))$root) |>
    subset(nu < max(nus))

fig_all <- ggplot(data = a_results,
                  aes(x = log(nu), y = rel_crps)) +
    geom_vline(data = bp, aes(xintercept = log(nu)), linetype = 3, col = gray(0.5)) +
    geom_ribbon(data = na.omit(a_results_smooth),
                aes(ymin = 0, ymax = rel_crps, x = log(nu), fill = sign)) +
    facet_grid(fphi ~ fmu, labeller = label_parsed) +
    labs(x = expression(u), y = expression(S[HT] / S[XBX] - 1)) +
    scale_fill_grey(start = 0.8, end = 0.5) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.3, 0.3)) +
    scale_x_continuous(breaks = log(2^seq(-6, 0, length.out = 4)),
                       labels = label_parsed(paste0("2^", seq(-6, 0, length.out = 4)))) +
    theme_bw() +
    theme(legend.position = "top")


if (save_plot) {
    grDevices::pdf(file.path(fig_path, paste0("xbeta-rel-crps.pdf")),
                   width = 8, height = 10)
    print(fig_all)
    dev.off()
} else {
    print(fig_all)
}



## Subset of mu/phi intervals for main text
fmu_sub <- c("'(0.25,0.75)'", "'(0.05,0.95)'", "'(0.05,0.25)'", "'(0.05,0.5)'", "'(0.05,0.75)'")
fphi_sub <- c("'(0.5,20)'", "'(0.5,50)'", "'(20,50)'", "'(50,100)'")


fig_sub <- ggplot(data = a_results |> subset(fmu %in% fmu_sub & fphi %in% fphi_sub),
                  aes(x = log(nu), y = rel_crps)) +
    geom_vline(data = bp |> subset(fmu %in% fmu_sub & fphi %in% fphi_sub),
               aes(xintercept = log(nu)), linetype = 3, col = gray(0.5)) +
    geom_ribbon(data = na.omit(a_results_smooth) |> subset(fmu %in% fmu_sub & fphi %in% fphi_sub),
                aes(ymin = 0, ymax = rel_crps, x = log(nu), fill = sign)) +
    facet_grid(fphi ~ fmu, labeller = label_parsed) +
    labs(x = expression(u), y = expression(S[HT] / S[XBX] - 1)) +
    scale_fill_grey(start = 0.8, end = 0.5) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.3, 0.3)) +
    scale_x_continuous(breaks = log(2^seq(-6, 0, length.out = 4)),
                       labels = label_parsed(paste0("2^", seq(-6, 0, length.out = 4)))) +
    theme_bw() +
    theme(legend.position = "top")



if (save_plot) {
    grDevices::pdf(file.path(fig_path, paste0("xbeta-rel-crps-subset.pdf")),
                   width = 8, height = 6)
    print(fig_sub)
    dev.off()
} else {
    print(fig_sub)
}








