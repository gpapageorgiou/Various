#################
# ggplot model assumptions for lme objects

fit_vs_res <- function(lmeObj) {
  require(ggplot2)
  ggplot() + geom_point(aes(x = fitted(lmeObj, level = 0), 
                            y = resid(lmeObj, type = 'pearson')), 
                        size = 3, alpha = 0.6) + 
    geom_hline(aes(yintercept = 0), color = '#FF1971') + 
    ylab('Standardized Residuals') + xlab('Fitted Values') + theme_bw()
}

qq_plot <- function(lmeObj) {
  mod.res <- resid(lmeObj, type = 'pearson')[order(resid(lmeObj, type = 'pearson'))]
  snorm.res <- qnorm(seq(0, 1, length.out = length(resid(lmeObj, type = 'pearson'))))
  probs <- c(0.25, 0.75)
  yline <- quantile(mod.res, probs, names = FALSE, type = 7, na.rm = TRUE)
  xline <- qnorm(probs)
  slope <- diff(yline) / diff(xline)
  intrcpt <- yline[1L] - slope * xline[1L]
  ggplot() + geom_point(aes(x = mod.res, 
                            y = snorm.res), 
                        size = 3, alpha = 0.6) + 
    geom_abline(aes(intercept = intrcpt, slope = slope), color = '#FF1971') + 
    xlab('Standardized Residuals') + ylab('Quantiles of Standard Normal') + theme_bw()
}
