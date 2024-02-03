## ----knitr-setup, include = FALSE-----------------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  comment = "#",
  prompt = F,
  tidy = FALSE,
  cache = FALSE,
  collapse = T
)

old <- options(width = 100L, digits = 10)

## ----out.width = "35%", echo = FALSE--------------------------------------------------------------
include_graphics("figures/task4-two-sided-3-stage-Pocock-setup.png")

## ----out.width = c("55%", "38%"), echo = FALSE,fig.show="hold"------------------------------------
include_graphics("figures/task4-two-sided-3-stage-Pocock.png")
include_graphics("figures/task4-two-sided-3-stage-Pocock-graph.png")

## ----out.width = "45%", echo = FALSE--------------------------------------------------------------
include_graphics("figures/task4-CI-setup.png")

## ----out.width = "50%", echo = FALSE--------------------------------------------------------------
include_graphics("figures/task4-CI-result.png")

## ----include = FALSE----------------------------------------------------------
options(old)

