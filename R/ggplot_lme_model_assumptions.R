#################
# ggplot model assumptions for lme objects

fit_vs_res <- function(lmeObj) {
  require(ggplot2)
  ggplot() + geom_point(aes(x = fitted(lmeObj, level = 0), y = resid(lmeObj, type = 'pearson')))
}

qq_plot <- function(lmeObj) {
  ggplot() + geom_point(aes(x = resid(lmeObj, type = 'pearson')[order(resid(lmeObj, type = 'pearson'))], 
                            y = qnorm(seq(0, 1, length.out = length(resid(lmeObj, type = 'pearson'))))))
}