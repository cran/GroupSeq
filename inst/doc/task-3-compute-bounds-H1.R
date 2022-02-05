## ----knitr-setup, include = FALSE-----------------------------------------------------------------
library(knitr)
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

options(width = 100L)

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task3-example-manual-bounds.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task3-example-manual-bounds-results.png")

## ---- out.width = "35%"---------------------------------------------------------------------------
include_graphics("figures/task3-example-manual-bounds-zero-drift.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task3-example-manual-bounds-zero-drift-results.png")

## ---- out.width = c("40%", "55%"), echo = FALSE, fig.show = "hold"--------------------------------
include_graphics("figures/task3-example-manual-bounds-zero-drift-adjusted.png")
include_graphics("figures/task3-example-manual-bounds-zero-drift-adjusted-results.png")

## ---- out.width = c("40%", "55%"), echo = FALSE, fig.show = "hold"--------------------------------
include_graphics("figures/task3-manual-Pocock.png")
include_graphics("figures/task3-manual-Pocock-results.png")

## ---- out.width = c("40%", "55%"), echo = FALSE, fig.show = "hold"--------------------------------
include_graphics("figures/task3-exact-Pocock.png")
include_graphics("figures/task3-exact-Pocock-results.png")

## ---- out.width = c("40%", "55%"), echo = FALSE, fig.show = "hold"--------------------------------
include_graphics("figures/task3-exact-Pocock-drift2.png")
include_graphics("figures/task3-exact-Pocock-drift2-results.png")

