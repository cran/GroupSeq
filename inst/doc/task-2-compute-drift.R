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

old <- options(width = 100L, digits = 10)

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task2-menu.png")

## ---- out.width = c("55%", "40%"), echo = FALSE,fig.show="hold"-----------------------------------
include_graphics("figures/task2-3stage-OBF-result.png")
include_graphics("figures/task2-3stage-OBF-graph.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task2-3stage-Pocock-result.png")

## ---- out.width = "35%"---------------------------------------------------------------------------
include_graphics("figures/task2-set-power.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task2-3stage-Pocock-result-power90.png")

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task2-set-manual-bounds.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task2-3stage-Pocock-result-manual-bounds.png")

## ---- include = FALSE---------------------------------------------------------
options(old)

