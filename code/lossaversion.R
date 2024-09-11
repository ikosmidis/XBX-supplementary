## paths ----------------------------------------------------------------------------
base_path <- "~/Repositories/XBX-supplementary/"
fig_path <- file.path(base_path, "figures/")
code_path <- file.path(base_path, "code/")
save_plot <- FALSE


## packages -------------------------------------------------------------------------
library("betareg")
library("VGAM")
library("crch")
## install.packages("topmodels", repos = "https://zeileis.r-universe.dev")
library("topmodels")
library("ggplot2")
library("lmtest")


## support functions ----------------------------------------------------------------
source(file.path(code_path, "beta01.R"))


## data -----------------------------------------------------------------------------
## averaged responses for myopic loss aversion data from nine rounds, see
## Glätzle-Rützler, Sutter, Zeileis (2015, Journal of Economic Behavior & Organization)
data("LossAversion", package = "betareg")

## ad-hoc scaled response
LossAversion$invests <- ((LossAversion$invest * (nrow(LossAversion) - 1) + 0.5)/nrow(LossAversion))

## hurdle components
LossAversion$investh <- factor((LossAversion$invest <= 0) + 2 * (LossAversion$invest >= 1),
  levels = c(0, 1, 2), labels = c("(0,1)", "0", "1"))


## regression models ----------------------------------------------------------------
## N
la_ols <- glm(invest ~ grade * (arrangement + age) + male, data = LossAversion)
summary(la_ols)

## CN
la_htobit <- crch(invest ~ grade * (arrangement + age) + male | arrangement + male + grade, data = LossAversion, left = 0, right = 1)
summary(la_htobit)

## B (after scaling)
la_beta <- betareg(invests ~ grade * (arrangement + age) + male | arrangement + male + grade, data = LossAversion)
summary(la_beta)

## XBX
la_xbx <- betareg(invest ~ grade * (arrangement + age) + male | arrangement + male + grade, data = LossAversion)
summary(la_xbx)


## goodness of fit  -----------------------------------------------------------------
r_ols <- rootogram(la_ols, confint_type = "tukey",
                   breaks = -6:16 / 10,
                   xlab = "Proportion of tokens invested",  main = "N",
                   axes = FALSE, plot = FALSE)
r_htobit <- rootogram(la_htobit, confint_type = "tukey",
                      xlab = "Proportion of tokens invested", main = "CN",
                      ylim = ylims, xlim = xlims,
                      axes = FALSE, plot = FALSE)
r_beta <- rootogram(la_beta, confint_type = "tukey",
                    xlab = "Proportion of tokens invested", main = "B",
                    ylim = ylims, xlim = xlims,
                    axes = FALSE, plot = FALSE)
r_xbx <- rootogram(la_xbx, confint_type = "tukey",
                   xlab = "Proportion of tokens invested", main = "XBX",
                   ylim = ylims, xlim = xlims,
                   axes = FALSE, plot = FALSE)
fig_rootogram <- autoplot(c(r_ols, r_htobit, r_beta, r_xbx)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.6, 1.6)) +
    facet_wrap(. ~ group)

if (save_plot) {
    grDevices::pdf(file.path(fig_path, "lossaversion-rootogram.pdf"), width = 9, height = 8)
    print(fig_rootogram)
    dev.off()
} else {
    print(fig_rootogram)
}

lrtest(update(la_xbx, formula = . ~ grade * (arrangement + age) + male | ~ 1), la_xbx)
waldtest(update(la_xbx, formula = . ~ grade * (arrangement + age) + male | ~ 1), la_xbx)

lrtest(update(la_htobit, formula = . ~ grade * (arrangement + age) + male | ~ 1), la_htobit)
waldtest(update(la_htobit, formula = . ~ grade * (arrangement + age) + male | ~ 1), la_htobit)


## Mean and probability effects -----------------------------------------------------

la <- subset(LossAversion, male == "yes" & grade == "10-12" & age >= 15 &  age <= 17)
la_nd <- data.frame(arrangement = c("single", "team"), male = "yes", age = 16, grade = "10-12")

la_mod <- c("Empirical", "N", "CN", "B", "XBX")
la_col <- unname(palette.colors())[c(1, 2, 2, 4, 4)]
la_lty <- c(1, 5, 1, 5, 1)

la_mean <- data.frame(
    Prediction = "E(Y)",
    Model = rep(la_mod, each = 2),
    Arrangement = rep(c("Individual", "Team"), 5),
    Investment = c(
        aggregate(invest ~ arrangement, data = la, FUN = mean)$invest,
        procast(la_ols,    newdata = la_nd, type = "mean", drop = TRUE),
        procast(la_htobit, newdata = la_nd, type = "mean", drop = TRUE),
        procast(la_beta,   newdata = la_nd, type = "mean", drop = TRUE),
        procast(la_xbx,    newdata = la_nd, type = "mean", drop = TRUE)
    )
)

la_prob0 <- data.frame(
  Prediction = "P(Y < 0.05)",
  Model = rep(la_mod, each = 2),
  Arrangement = rep(c("Individual", "Team"), 5),
  Investment = c(
    aggregate(invest <= 0.05 ~ arrangement, data = la, FUN = mean)$invest,
    procast(la_ols,    newdata = la_nd, type = "cdf", at = 0.05, lower.tail = TRUE, drop = TRUE),
    procast(la_htobit, newdata = la_nd, type = "cdf", at = 0.05, lower.tail = TRUE, drop = TRUE),
    procast(la_beta,   newdata = la_nd, type = "cdf", at = 0.05, lower.tail = TRUE, drop = TRUE),
    procast(la_xbx,    newdata = la_nd, type = "cdf", at = 0.05, lower.tail = TRUE, drop = TRUE)
  )
)

la_prob1 <- data.frame(
  Prediction = "P(Y > 0.95)",
  Model = rep(la_mod, each = 2),
  Arrangement = rep(c("Individual", "Team"), 5),
  Investment = c(
    aggregate(invest >= 0.95 ~ arrangement, data = la, FUN = mean)$invest,
    procast(la_ols,    newdata = la_nd, type = "cdf", at = 0.95, lower.tail = FALSE, drop = TRUE),
    procast(la_htobit, newdata = la_nd, type = "cdf", at = 0.95, lower.tail = FALSE, drop = TRUE),
    procast(la_beta,   newdata = la_nd, type = "cdf", at = 0.95, lower.tail = FALSE, drop = TRUE),
    procast(la_xbx,    newdata = la_nd, type = "cdf", at = 0.95, lower.tail = FALSE, drop = TRUE)
  )
)

la_dist <- la_prob0[-(1:2), ]
la_dist$Prediction <- "Distribution"
la_dist$Investment = c(
    lapply(1:2, function(i) procast(la_ols,    newdata = la_nd[i,], type = "distribution", drop = TRUE)),
    lapply(1:2, function(i) procast(la_htobit, newdata = la_nd[i,], type = "distribution", drop = TRUE)),
    lapply(1:2, function(i) procast(la_beta,   newdata = la_nd[i,], type = "distribution", drop = TRUE)),
    lapply(1:2, function(i) procast(la_xbx,    newdata = la_nd[i,], type = "distribution", drop = TRUE))
)

vapply(la_dist$Investment, format, "", digits = 3, nsmall = 3)

la_meanprob <- rbind(la_mean, la_prob0, la_prob1) |>
  transform(Model = factor(Model, levels = la_mod))
la_meanprob |>
  xtabs(formula = Investment ~ Prediction + Arrangement + Model) |>
  ftable()

fig_meanprob <- ggplot(la_meanprob |> subset(Prediction != "P(Y < 0.05)"),
                       aes(x = Arrangement, y = Investment,
                           group = Model, colour = Model, linetype = Model)) +
    geom_point(size = 2) +
    geom_line(lwd = 1) +
    scale_color_manual(values = la_col) + scale_linetype_manual(values = la_lty) +
    facet_wrap(. ~ Prediction, scales = "free_y") +
    labs(y = "Expected proportion of tokens invested") +
    theme_bw() +
    theme(legend.key.width = unit(2.1, "line"), legend.position = "top")

if (save_plot) {
    grDevices::pdf(file.path(fig_path, "lossaversion-meanprob.pdf"), width = 10, height = 4.5)
    print(fig_meanprob)
    dev.off()
} else {
    print(fig_meanprob)
}

## CDF effects ----------------------------------------------------------------------
iv <- -50:1050/1000
la_cdf <- data.frame(
    Model = factor(rep(la_mod, each = 2 * length(iv)), levels = la_mod),
    Arrangement = rep(rep(c("Individual", "Team"), each = length(iv)), 5),
    Investment = rep(iv, 2 * 5),
    Probability = c(
        ecdf(subset(la, arrangement == "single")$invest)(iv),
        ecdf(subset(la, arrangement == "team"  )$invest)(iv),
        c(t(procast(la_ols,    newdata = la_nd, type = "cdf", at = iv))),
        c(t(procast(la_htobit, newdata = la_nd, type = "cdf", at = iv))),
        c(t(procast(la_beta,   newdata = la_nd, type = "cdf", at = iv))),
        c(t(procast(la_xbx,    newdata = la_nd, type = "cdf", at = iv)))
    )
)

fig_cdf <- ggplot(la_cdf, aes(x = Investment, y = Probability,
                              group = Model, colour = Model, linetype = Model)) +
    geom_line(lwd = 1) +
    scale_color_manual(values = la_col) +
    scale_linetype_manual(values = la_lty) +
    facet_grid(. ~ Arrangement) +
    geom_hline(yintercept = 0:1, colour = "darkgray") +
    labs(x = "Proportion of tokens invested") +
    theme_bw() +
    theme(legend.key.width = unit(2.1, "line"), legend.position = "top")

if (save_plot) {
    grDevices::pdf(file.path(fig_path, "lossaversion-cdf.pdf"), width = 10, height = 4.5)
    print(fig_cdf)
    dev.off()
} else {
    print(fig_cdf)
}

## three-part hurdle ----------------------------------------------------------------
la_mnl1 <- vglm(investh ~ grade + arrangement + male, data = LossAversion,
                   family = multinomial(refLevel = "(0,1)"))
la_mnl2 <- vglm(investh ~ grade * (arrangement + age) + male, data = LossAversion,
                   family = multinomial(refLevel = "(0,1)"))
la_h2_beta <- betareg(invest ~ grade * (arrangement + age) + male | arrangement + male + grade,
                      data = LossAversion, weights = as.numeric(investh == "(0,1)"))

la_hmnl1 <- beta01(la_mnl1, la_h2_beta)
la_hmnl2 <- beta01(la_mnl2, la_h2_beta)

coef(summary(la_mnl2))
coef(summary(la_h2_beta))
logLik(la_hmnl2)
AIC(la_hmnl2)
BIC(la_hmnl2)

coef(summary(la_mnl1))
coef(summary(la_h2_beta))
logLik(la_hmnl1)
AIC(la_hmnl1)
BIC(la_hmnl1)


posterior_u <- function(obj) {
    pars <- predict(obj, type = "parameters")
    cquad <- betareg:::quadtable(obj$control$quad)
    mu <- pars$mu
    phi <- pars$phi
    nu <- pars$nu
    y <- obj$y
    dens <- apply(cquad, 1, function(rule) {
        e <- rule[1] * nu
        rule[2] * dxbeta(y, mu, phi, nu = e, log = FALSE)
    })
    ex <- apply(cquad, 1, function(rule) {
        e <- rule[1] * nu
        e * rule[2] * dxbeta(y, mu, phi, nu = e, log = FALSE)
    })
    rowSums(ex) / rowSums(dens)
}

df <- LossAversion[c("male", "arrangement", "grade", "age", "invest")]
df$u <- posterior_u(la_xbx)

ggplot(df) +
    geom_point(aes(invest, u, col = male:grade:arrangement)) +
    facet_grid(male ~ grade + arrangement) +
    lims(y = c(0, 0.4))

