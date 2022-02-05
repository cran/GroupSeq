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
  dpi = 300
)

okabe_palette <- c('orange' = "#E69F00",
                   'sky blue' = "#56B4E9",
                   'bluish green' = "#009E73",
                   'yellow' = "#F0E442",
                   'blue' = "#0072B2",
                   'vermillion' = "#D55E00",
                   'reddish purple' = "#CC79A7")

options(width = 100L, digits = 10)

## ---- eval = FALSE, echo = TRUE-------------------------------------------------------------------
#  library("GroupSeq")

## ---- out.width = "35%", echo = FALSE-------------------------------------------------------------
include_graphics("figures/menu-after-load.png")

