---
layout: page
title: Test
permalink: /test/
---

```{r setup, include = FALSE, cache = FALSE}
knitr::opts_chunk$set(cache = TRUE, dpi = 300, fig.width = 8, fig.height = 5)
```


```{r gen_data}
library(data.table)
## Convenience function for generating our experimental panel data. Takes a 
## single argument: `sims` (i.e. how many simulation runs to do we want; defaults 
## to 1).
gen_data = function(sims=1) {
  
  ## Total time periods in the the panel = 500
  tt = 500
  
  sim = rep(rep(1:sims, each = 10), times = 2) ## Repeat twice b/c we have two countries
  
  ## x1 covariates
  x1_A = 1 + rnorm(tt*sims, 0, 1)
  x1_B = 1/4 + rnorm(tt*sims, 0, 1)
  
  ## Add second, nested x2 covariates for each country
  x2_A = 1 + x1_A + rnorm(tt*sims, 0, 1)
  x2_B = 1 + x1_B + rnorm(tt*sims, 0, 1)
  
  ## Outcomes (notice different slope coefs for x2_A and x2_B)
  y_A = x1_A + 1*x2_A + rnorm(tt*sims, 0, 1)
  y_B = x1_B + 2*x2_B + rnorm(tt*sims, 0, 1)
  
  ## Combine in a data table (basically just an enhanced data frame)
  dat = 
    data.table(
      sim,
      id = as.factor(c(rep('A', length(x1_A)), rep('B', length(x1_B)))),
      x1 = c(x1_A, x1_B),
      x2 = c(x2_A, x2_B),
      y = c(y_A, y_B)
      )
  
  ## Demeaned covariates (grouped by country and simulation)
  dat[, 
      `:=` (x1_dmean = x1 - mean(x1),
            x2_dmean = x2 - mean(x2)),
      by = .(sim, id)][]
  
  ## Optional set order i.t.o sims
  setorder(dat, sim)
  
  return(dat)
}
## Generate an instance of the data (using the default arguments)
set.seed(123)
d = gen_data()
d
```

Let's run some regressions on one simulated draw of our dataset. Since this is a panel model, I'll use the (incredible)  **fixest** package to control for country ("id") fixed-effects.

```{r reg1, warning=FALSE, message=FALSE}
library(fixest)
mod_level = feols(y ~ x1 * x2 | id, d)
mod_dmean = feols(y ~ x1_dmean * x2_dmean | id, d)
etable(mod_level, mod_dmean, se  = 'standard')
```
