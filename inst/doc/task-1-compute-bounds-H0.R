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
include_graphics("figures/task1-menu.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task1-1stage-result.png")

## ---- echo = TRUE---------------------------------------------------------------------------------
alpha = 0.05
qnorm(1 - alpha)

## ---- out.width = "25%"---------------------------------------------------------------------------
include_graphics("figures/task1-select-K3.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task1-3stage-result.png")

## ---- out.width = "50%"---------------------------------------------------------------------------
include_graphics("figures/task1-3stage-result-graph.png")

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task1-3stage-pocock.png")

## ---- out.width = c("55%", "40%"), echo = FALSE,fig.show="hold"-----------------------------------
include_graphics("figures/task1-3stage-pocock-result.png")
include_graphics("figures/task1-3stage-pocock-result-graph.png")

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task1-3stage-pocock-custom-times.png")

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task1-3stage-pocock-custom-times-entered.png")

## ---- out.width = "50%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task1-3stage-pocock-custom-times-result.png")

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task1-3stage-pocock-two-side.png")

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/task1-3stage-pocock-two-side-asym.png")

## ---- out.width = c("55%", "40%"), echo = FALSE,fig.show="hold"-----------------------------------
include_graphics("figures/task1-3stage-pocock-two-side-asym-result.png")
include_graphics("figures/task1-3stage-pocock-two-side-asym-graph.png")

