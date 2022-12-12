## ----knitr-setup, include = FALSE-----------------------------------------------------------------
library(knitr)
library("ggplot2")

knitr::opts_chunk$set(
  comment = "#",
  prompt = F,
  tidy = FALSE,
  cache = FALSE,
  collapse = T,
  echo = FALSE,
  dpi = 300,
  fig.width = 5, fig.height = 5
)

okabe_palette <- c('orange' = "#E69F00",
                   'sky blue' = "#56B4E9",
                   'bluish green' = "#009E73",
                   'yellow' = "#F0E442",
                   'blue' = "#0072B2",
                   'vermillion' = "#D55E00",
                   'reddish purple' = "#CC79A7")

old <- options(width = 100L, digits = 10)

## -------------------------------------------------------------------------------------------------
fill_colors = c(X1 = okabe_palette[["bluish green"]],
                X2 = okabe_palette[["vermillion"]],
                "X1+X2" = "#6B7E3A")

data  = data.frame(x = c(1:10, 1:10, 1:20),
                   y = c(rep(2, 10), rep(1.5, 10), rep(1.5, 20)),
                   Sample = c(rep("X1", 10), rep("X2", 10), rep("X1+X2", 20))
)

## ---- out.width = "35%", message = FALSE, fig.height = 1, fig.width = 2.5, fig.align = "center", fig.cap = "Schematic view of a study sample - each circle represents some measurement."----
dat = data[1:10, ]
ggplot(dat, aes(x = x, y = y)) +
    geom_point(size = 10, shape = 21, aes(fill = Sample)) +
    scale_fill_manual(values = fill_colors[1]) +
    lims(x = c(0.5, 10.5), y = c(2, 2)) +
    theme_void() +
    theme(legend.position = "bottom")


## ---- echo = FALSE, out.width = "65%", fig.cap = fig.cap, fig.height=3----------------------------
fig.cap = "Standard Normal Density with critial bounds marking 5% significance level."

blue = okabe_palette[["sky blue"]]
yellow = okabe_palette[["yellow"]]


crit = qnorm(1 - 0.025)
probLabel = paste0(pnorm(-crit)*100, "%")
x = seq(-3, 3, by = 0.01)

normalDensPlot =
    ggplot(data.frame(x = x), aes(x = x)) +
    stat_function(fun = dnorm, geom = "area", fill = blue, xlim = c(-4, -crit)) +
    stat_function(fun = dnorm, geom = "area", fill = yellow, xlim = c(-crit, crit)) +
    stat_function(fun = dnorm, geom = "area", fill = blue, xlim = c(crit, 4)) +
    stat_function(fun = dnorm, color = okabe_palette[["bluish green"]], size = 1.5) +
    geom_segment(x = 2.2, xend = 2.7, y = 0.02, yend = 0.07) +
    geom_segment(x = -2.2, xend = -2.7, y = 0.02, yend = 0.07) +
    annotate("text", label = probLabel, x = 2.9, y = 0.09, size = 3.5) +
    annotate("text", label = probLabel, x = -2.9, y = 0.09, size = 3.5) +
    lims(x = range(x)) +
    labs(x = expression(x[1]),
         y = expression(f(x[1]))) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line())

normalDensPlot

## ---- echo = TRUE---------------------------------------------------------------------------------
qnorm(0.025)

## ---- out.width = "35%", message = FALSE, fig.height = 1.3, fig.width = 2.5, fig.align = "center"----
dat = data[1:20, ]
ggplot(dat, aes(x = x, y = y)) +
    geom_point(size = 10, shape = 21, aes(fill = Sample)) +
    scale_fill_manual(values = fill_colors[1:2]) +
    lims(x = c(0.5, 10.5), y = c(1.2, 2.2)) +
    theme_void() +
    theme(legend.position = "bottom")


## ---- fig.width = 8, fig.height = 3, out.width = "85%"--------------------------------------------
normalDensPlotX2 = normalDensPlot +
    labs(x = expression(x[2]), y = expression(f(x[2]))) +
    stat_function(fun = dnorm, color = okabe_palette[["vermillion"]], size = 1.5)
gridExtra::grid.arrange(normalDensPlot, normalDensPlotX2, nrow = 1)

## ---- echo = TRUE---------------------------------------------------------------------------------
qnorm(0.025/2)

## ---- echo = FALSE, out.width = "65%", fig.cap = fig.cap, fig.height=3----------------------------
fig.cap = "Standard Normal Density with critial bounds marking 5% significance level."

blue = okabe_palette[["sky blue"]]
yellow = okabe_palette[["yellow"]]

crit = qnorm(1 - 0.025/2)
probLabel = paste0(pnorm(-crit)*100, "%")
x = seq(-3, 3, by = 0.01)

normalDensPlotAdj =
    ggplot(data.frame(x = x), aes(x = x)) +
    stat_function(fun = dnorm, geom = "area", fill = blue, xlim = c(-4, -crit)) +
    stat_function(fun = dnorm, geom = "area", fill = yellow, xlim = c(-crit, crit)) +
    stat_function(fun = dnorm, geom = "area", fill = blue, xlim = c(crit, 4)) +
    stat_function(fun = dnorm, color = okabe_palette[["bluish green"]], size = 1.2) +
    geom_segment(x = 2.2+.2, xend = 2.7+.2, y = 0.02, yend = 0.07) +
    geom_segment(x = -2.2-.2, xend = -2.7-.2, y = 0.02, yend = 0.07) +
    annotate("text", label = probLabel, x = 2.9, y = 0.09, size = 3.5) +
    annotate("text", label = probLabel, x = -2.9, y = 0.09, size = 3.5) +
    lims(x = range(x)) +
    labs(x = expression(x[1]),
         y = expression(f(x[1]))) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line())

## ---- fig.width = 8, fig.height = 3, out.width = "85%"--------------------------------------------
normalDensPlotX2Adj = normalDensPlotAdj +
    labs(x = expression(x[2]), y = expression(f(x[2]))) +
    stat_function(fun = dnorm, color = okabe_palette[["vermillion"]], size = 1.5)
gridExtra::grid.arrange(normalDensPlotAdj, normalDensPlotX2Adj, nrow = 1)

## -------------------------------------------------------------------------------------------------
alpha = 0.05
alpha.Bonf = alpha / 2
alpha.Sidak = 1 - (1-alpha)^(1/2)

crit.Bonf = -rep(qnorm(alpha.Bonf / 2), 2)
crit.Sidak = -rep(qnorm(alpha.Sidak / 2), 2)

## ----prepare-2D-normal-density-plot, echo = FALSE, include = FALSE--------------------------------
mu = 0
Mean = c(mu, mu)
covariance = 0
Sigma0 = matrix(c(1, rep(covariance, 2), 1), 2)


width = 6.6
binwidth = 0.2
x1 = x2 = seq(from = mu - width/2, to = mu + width/2, by = binwidth)
grid = expand.grid(x1, x2)

z = mvtnorm::dmvnorm(grid, mean = Mean, sigma = Sigma0)
zmat = matrix(z, ncol = length(x1))

grid.col = expand.grid(x1[-1] - binwidth/2, x2[-1] - binwidth/2)
gc1 = grid.col[, 1]
gc2 = grid.col[, 2]

crit = crit.Sidak
get_color = Vectorize(function(x1, x2) {
    if (x1 < -crit[1] || x1 > crit[1] || x2 < -crit[2] || x2 > crit[2])
        blue else yellow
})

plot_normal_dens_2d = function(zmat, ...) {
    persp(x1, x2, zmat,
          expand = 0.5,
          lphi = 0,
          xlab = "x1",
          ylab = "x2\n",
          zlab = "f(x1, x2)",
          shade = 0.2,
          ticktype = "detailed",
          ...)
}

## ---- out.width = "55%"---------------------------------------------------------------------------
op = par(mar = c(0, 2, 0, 0))
plot_normal_dens_2d(zmat, phi = 45, col = get_color(gc1, gc2))
par(op)

## ---- include = FALSE-----------------------------------------------------------------------------
crit.Bonf = -rep(qnorm(alpha.Bonf / 2), 2)
crit.Sidak = -rep(qnorm(alpha.Sidak / 2), 2)

crit = crit.Bonf
crit = crit.Sidak

calc2Dprob = function(crit, sigma) {
    calc_prob = function(lower, upper) {
        as.numeric(mvtnorm::pmvnorm(lower = lower, upper = upper, sigma = sigma))
    }

    left_stripe = calc_prob(lower = c(-Inf, -Inf), upper = c(-crit[1], Inf))
    lower_rectangle = calc_prob(lower = c(-crit[1], -Inf), upper = c(crit[1], -crit[2]))
    2 * left_stripe + 2 * lower_rectangle # return two-sided prob
}

p.edge = as.numeric(mvtnorm::pmvnorm(lower = c(-Inf, -Inf),
                                     upper = c(-crit[1], -crit[2]),
                                     sigma = Sigma0))


p2D.Bonf = calc2Dprob(crit.Bonf, sigma = Sigma0)
p2D.Sidak = calc2Dprob(crit.Sidak, sigma = Sigma0)

## ---- out.width = "55%"---------------------------------------------------------------------------
orange = okabe_palette[["orange"]]
crit = crit.Sidak
get_color_edges = Vectorize(function(x1, x2) {
    # Make edges grey
    if (x1 < -crit[1] && x2 < -crit[2] ||
        x1 < -crit[1] && x2 >  crit[2] ||
        x1 >  crit[1] && x2 < -crit[2] ||
        x1 >  crit[1] && x2 >  crit[2])
        return("grey")

    if (x1 < -crit[1] || x1 > crit[1] || x2 < -crit[2] || x2 > crit[2])
        blue else yellow
})

op = par(mar = c(0, 2, 0, 0))
plot_normal_dens_2d(zmat, phi = 45, col = get_color_edges(gc1, gc2))
par(op)

## ---- out.width = "35%", message = FALSE, fig.height = 1.3, fig.width = 2.5, fig.align = "center"----
dat = data[1:20, ]
dat[11:20, "x"] = 11:20
dat = rbind(data.frame(x = 1:10, y = 1.5, Sample = "X1"), dat)
ggplot(dat, aes(x = x, y = y)) +
    geom_point(size = 10, shape = 21, aes(fill = Sample)) +
    scale_fill_manual(values = fill_colors[1:2]) +
    lims(x = c(0.5, 20.5), y = c(1.2, 2.2)) +
    theme_void() +
    theme(legend.position = "bottom")


## ---- out.width = "35%", message = FALSE, fig.height = 1.3, fig.width = 2.5, fig.align = "center", fig.cap = "Schematic view of group sequential two-stage study."----
dat = data[c(1:10, 21:40), ]

ggplot(dat, aes(x = x, y = y)) +
    geom_point(size = 10, shape = 21, aes(fill = Sample)) +
    scale_fill_manual(values = fill_colors[c(1, 3)]) +
    lims(x = c(0.5, 20.5), y = c(1.2, 2.2)) +
    theme_void() +
    theme(legend.position = "bottom")


## ---- fig.width = 8, fig.height = 3, out.width = "85%", fig.cap = "Normal densities for stage 1 and 2 with Bonferroni bounds."----
normalDensPlotX2Adj = normalDensPlotAdj +
    labs(x = expression(x[2]), y = expression(f(x[2]))) +
    stat_function(fun = dnorm, color = fill_colors[3], size = 1.5)
gridExtra::grid.arrange(normalDensPlotAdj, normalDensPlotX2Adj, nrow = 1)

## -------------------------------------------------------------------------------------------------
mu = 0
Mean = c(mu, mu)
covariance = sqrt(0.5)
Sigma = matrix(c(1, rep(covariance, 2), 1), 2)

z = mvtnorm::dmvnorm(grid, mean = Mean, sigma = Sigma)
zmat = matrix(z, ncol = length(x1))

## ---- fig.show="hold", out.width = "30%", fig.cap = "Bivariate normal density of two-stage design viewed from different angles."----
op = par(mar = rep(.1, 4))
plot_normal_dens_2d(zmat, phi = 40, col = get_color(gc1, gc2), box = FALSE)
plot_normal_dens_2d(zmat, phi = 65, col = get_color(gc1, gc2), box = FALSE)
plot_normal_dens_2d(zmat, phi = 90, col = get_color(gc1, gc2), box = FALSE)
par(op)

## -------------------------------------------------------------------------------------------------
p2D.Bonf = calc2Dprob(crit.Bonf, sigma = Sigma)

## ---- out.width = "40%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/two-stage-Pocock-bounds.png")

## ---- out.width = "70%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/two-stage-Pocock-bounds-result.png")

## -------------------------------------------------------------------------------------------------
crit.Pocock = rep(2.1783, 2)
p2D.Pocock = calc2Dprob(crit.Pocock, sigma = Sigma)

## ---- echo = FALSE, out.width = "50%", fig.cap = "Group sequential 2-stage Pocock design with one interim look at half of the total samples."----
n = 100
tt = c(.5, 1)
crit = crit.Pocock
x = seq(0.2, 1.0, length = n)

upperLine = data.frame(x = x, y = crit[1])
lowerLine = data.frame(x = x, y = -crit[1])

upperArea = data.frame(x = x, ymin = upperLine$y, ymax = 3, y = 3)
innerArea = data.frame(x = x, ymin = lowerLine$y, ymax = upperLine$y, y = 0)
lowerArea = data.frame(x = x, ymin = -3, ymax = lowerLine$y, y = -3)

p = ggplot(mapping = aes(x = x, y = y)) +

    geom_ribbon(data = upperArea, aes(x = x, ymin = ymin, ymax = ymax, y = y), fill = blue) +
    geom_ribbon(data = innerArea, aes(x = x, ymin = ymin, ymax = ymax, y = y), fill = yellow) +
    geom_ribbon(data = lowerArea, aes(x = x, ymin = ymin, ymax = ymax, y = y), fill = blue) +

    geom_line(data = upperLine) +
    geom_line(data = lowerLine) +

    geom_point(data = data.frame(x = tt, y =  crit), size = 5, shape = 21, fill = fill_colors[c(1, 3)]) +
    geom_point(data = data.frame(x = tt, y = -crit), size = 5, shape = 21, fill = fill_colors[c(1, 3)]) +

    theme_minimal() +
    labs(x = "Information Rate", y = "Critical Value") +
    lims(y = c(-3, 3)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line())

p

## ---- echo = FALSE, out.width = "50%", fig.cap = "Group sequential 2-stage O'Brien-Fleming design with one interim look at 0.5."----

asOBF <- function(t, alpha = 0.05, side = 2) {
  2 * (1 - stats::pnorm((stats::qnorm(1 - (alpha / side)/2)) / sqrt(t)))
}

n = 100
tt = c(.5, 1)

crit.OBF = -qnorm(asOBF(tt))
crit = crit.OBF
x = seq(0.2, 1.0, length = n)

upperLine = data.frame(x = x, y = -qnorm(asOBF(x)))
lowerLine = data.frame(x = x, y = qnorm(asOBF(x)))

upperArea = data.frame(x = x, ymin = upperLine$y, ymax = 6, y = 3)
innerArea = data.frame(x = x, ymin = lowerLine$y, ymax = upperLine$y, y = 0)
lowerArea = data.frame(x = x, ymin = -6, ymax = lowerLine$y, y = -3)

p = ggplot(mapping = aes(x = x, y = y)) +

    geom_ribbon(data = upperArea, aes(x = x, ymin = ymin, ymax = ymax, y = y), fill = blue) +
    geom_ribbon(data = innerArea, aes(x = x, ymin = ymin, ymax = ymax, y = y), fill = yellow) +
    geom_ribbon(data = lowerArea, aes(x = x, ymin = ymin, ymax = ymax, y = y), fill = blue) +

    geom_line(data = upperLine) +
    geom_line(data = lowerLine) +

    geom_point(data = data.frame(x = tt, y =  crit), size = 5, shape = 21, fill = fill_colors[c(1, 3)]) +
    geom_point(data = data.frame(x = tt, y = -crit), size = 5, shape = 21, fill = fill_colors[c(1, 3)]) +

    theme_minimal() +
    labs(x = "Information Rate", y = "Critical Value") +
    lims(y = c(-6, 6)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line())

p


## ---- out.width = "70%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/two-stage-OBF-bounds-result.png")

## ---- fig.show="hold", out.width = "45%", fig.cap = "Bivariate normal density with critical bounds of Pocock (left) and O'Brien-Fleming design (right) both with interim analysis after 50% of the sample."----
op = par(mar = rep(1.5, 4))
crit = crit.Pocock
plot_normal_dens_2d(zmat, phi = 80, col = get_color(gc1, gc2))
crit = crit.OBF
plot_normal_dens_2d(zmat, phi = 80, col = get_color(gc1, gc2))
par(op)

## ---- fig.show="hold", out.width = "30%", fig.cap = "Bivariate normal density of two-stage O'Brien-Fleming design with first stage after 50% (left), 70% (middle) and 90% (right) of all samples."----

crit = crit.OBF
mu = 0
Mean = c(mu, mu)

Sigma = matrix(c(1, rep(sqrt(0.5), 2), 1), 2)
z = mvtnorm::dmvnorm(grid, mean = Mean, sigma = Sigma)
zmat0.5 = matrix(z, ncol = length(x1))

Sigma = matrix(c(1, rep(sqrt(0.7), 2), 1), 2)
z = mvtnorm::dmvnorm(grid, mean = Mean, sigma = Sigma)
zmat0.7 = matrix(z, ncol = length(x1))

Sigma = matrix(c(1, rep(sqrt(0.9), 2), 1), 2)
z = mvtnorm::dmvnorm(grid, mean = Mean, sigma = Sigma)
zmat0.9 = matrix(z, ncol = length(x1))


op = par(mar = rep(.1, 4))
plot_normal_dens_2d(zmat0.5, phi = 90, col = get_color(gc1, gc2), box = FALSE)
plot_normal_dens_2d(zmat0.7, phi = 90, col = get_color(gc1, gc2), box = FALSE)
plot_normal_dens_2d(zmat0.9, phi = 90, col = get_color(gc1, gc2), box = FALSE)
par(op)

## ---- include = FALSE---------------------------------------------------------
options(old)

