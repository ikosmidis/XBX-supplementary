base_path <- "~/Repositories/XBX-supplementary/"
fig_path <- file.path(base_path, "figures/")

library("betareg")
library("lmtest")
library("ggplot2")
save_plot <- FALSE

data("ReadingSkills", package = "betareg")

## Constants to adjust the data a la Smithson and Verkuilen (2006)
u <- 10^seq(-6, -1, length = 100)
N <- nrow(ReadingSkills)
u_sv <- 1 / (2 * (N - 1))
sv_transform <- function(y, a) {
    (y + a) / (1 + 2 * a)
}

results <- sapply(u, function(const) {
    rs_beta <- betareg(accuracy1 ~ dyslexia * iq | dyslexia + iq,
                       data = within(ReadingSkills, accuracy1 <- sv_transform(accuracy1, const)),
                       hessian = TRUE)
    z_statistic <- coeftest(rs_beta)["dyslexia:iq", "z value"]
    lr_statistic <- lrtest(update(rs_beta, . ~ . - dyslexia:iq), rs_beta)$Chisq[2]
    data.frame(u = const,
               statistic = c(z_statistic, lr_statistic),
               test = c("Wald", "Likelihood ratio"))
}, simplify = FALSE)
results <- do.call("rbind", results)

## Plot the statistcs
alpha <- c(0.1, 0.05, 0.025, 0.01)
critical_points <- data.frame(critical = c(-qnorm(1 - alpha/2), qchisq(1 - alpha, 1)),
                              alpha = factor(paste0(format(alpha * 100),  "%")),
                              test = rep(c("Wald", "Likelihood ratio"), each = length(alpha)))
fig <- ggplot(results) +
    geom_hline(data = critical_points, aes(yintercept = critical, colour = alpha), lty = 1) +
    geom_vline(data = critical_points, aes(xintercept = log10(u_sv)), lty = 3) +
    geom_line(aes(x = log10(u), y = statistic)) +
    facet_wrap(~ test, scales = "free_y", ncol = 2) +
    scale_color_manual(values = rev(gray.colors(4, 0.55, 0.85))) +
    labs(x = expression(log[10]*u), color = expression(alpha)) +
    theme_bw() +
    theme(legend.position = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

if (save_plot) {
    pdf(file.path(fig_path, "interaction-stats.pdf"), width = 6, height = 3)
    print(fig)
    dev.off()
} else {
    print(fig)
}

