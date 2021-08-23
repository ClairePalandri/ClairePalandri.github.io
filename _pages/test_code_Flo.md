test\_code\_Flo
================
8/23/2021

Here’s the first code:

``` r
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

    ##       sim id            x1         x2          y    x1_dmean    x2_dmean
    ##    1:   1  A  0.4395243534  0.4437256  0.3716463 -0.59506609 -1.61706557
    ##    2:   1  A  0.7698225105  0.7298675  1.7366279 -0.26476794 -1.33092373
    ##    3:   1  A  2.5587083141  3.5407281  5.5578472  1.52411787  1.47993688
    ##    4:   1  A  1.0705083914  1.9383333  4.2280693  0.03591794 -0.12245794
    ##    5:   1  A  1.1292877352 -0.4200550  0.8833686  0.09469729 -2.48084624
    ##   ---                                                                   
    ##  996:   1  B  0.1600248030  1.2366685  3.6943280 -0.08764048 -0.06972657
    ##  997:   1  B  1.3205160368  2.5756808  6.0263720  1.07285075  1.26928576
    ##  998:   1  B -1.1011003857  0.1763464 -1.1775877 -1.34876567 -1.13004861
    ##  999:   1  B -0.2726166972  1.2642393  3.4448738 -0.52028198 -0.04215572
    ## 1000:   1  B  0.0008093222  0.5403238  1.9157509 -0.24685596 -0.76607128

Let’s run some regressions

    ##                               mod_level          mod_dmean
    ## Dependent Var.:                       y                  y
    ##                                                           
    ## x1                    1.195*** (0.0650)                   
    ## x2                    1.638*** (0.0394)                   
    ## x1 x x2             -0.1373*** (0.0187)                   
    ## x1_dmean                                0.9544*** (0.0577)
    ## x2_dmean                                 1.556*** (0.0388)
    ## x1_dmean x x2_dmean                        0.0199 (0.0213)
    ## Fixed-Effects:      ------------------- ------------------
    ## id                                  Yes                Yes
    ## ___________________ ___________________ __________________
    ## S.E. type                      Standard           Standard
    ## Observations                      1,000              1,000
    ## R2                              0.86768            0.86062
    ## Within R2                       0.86761            0.86055

Well, there you have it
