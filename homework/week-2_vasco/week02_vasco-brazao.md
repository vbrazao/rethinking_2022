---
title: "Homework - Week 2"
author: "Vasco Brazão"
output: 
 html_document:
   keep_md: yes
editor_options: 
  markdown: 
    wrap: 72
---




```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: StanHeaders
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 4.0.5
```

```
## rstan (Version 2.21.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## Do not specify '-march=native' in 'LOCAL_CPPFLAGS' or a Makevars file
```

```
## Loading required package: parallel
```

```
## rethinking (Version 2.13)
```

```
## 
## Attaching package: 'rethinking'
```

```
## The following object is masked from 'package:stats':
## 
##     rstudent
```

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v tibble  3.0.6     v dplyr   1.0.4
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.1
## v purrr   0.3.4
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x tidyr::extract() masks rstan::extract()
## x dplyr::filter()  masks stats::filter()
## x dplyr::lag()     masks stats::lag()
## x purrr::map()     masks rethinking::map()
```

```r
data(Howell1)

d <- Howell1
```

## 1


```r
d1 <- d %>% 
  dplyr::filter(
    age >= 18
  )

mean_height <- mean(d1$height)
sd_height <- sd(d1$height)

mean_weight <- mean(d1$weight)
sd_weight <- sd(d1$weight)

# standardize height and weight
d1_z <- d1 %>% 
  dplyr::mutate(
    across(
      .cols = c("height", "weight"),
      .fns = ~ rethinking::standardize(.x)
    )
  )
```

We will run the following model with `quap`:

$$ \text{weight}_i \sim \operatorname{Normal}(\mu_i, \sigma) $$
$$ \mu_i = \alpha + \beta \text{height}_i$$
$$ \alpha \sim \operatorname{Normal}(0, 1) $$
$$ \beta \sim \operatorname{Log-Normal}(0, 1) $$
$$ \sigma \sim \operatorname{Exponential}(\frac{1}{2}) $$ Let's do some
prior predictive simulation to check that this makes *some* sense.


```r
one_prior_lines <- tibble::tibble(
  n = 1:1e2,
  intercept = rnorm(n = 1e2, mean = 0, sd = 1),
  slope = rlnorm(n = 1e2, mean = 0, sd = 1)
) %>% 
  tidyr::expand(tidyr::nesting(n, intercept, slope), height = range(d1_z$height)) %>% 
  dplyr::mutate(
    weight = intercept + slope * height
  )

one_prior_lines %>% 
  ggplot2::ggplot(ggplot2::aes(x = height, y = weight, group = n)) +
  ggplot2::geom_line(alpha = .2)
```

![](week02_vasco-brazao_files/figure-html/one.ppsim.lines-1.png)<!-- -->

This captures the idea that negative relationships are not possible
while still allowing for some absurdly large slopes. Let's also simulate
some data.


```r
one_prior_data <- tibble::tibble(
  height = seq(-3, 3, len = 1e2),
  intercept = rnorm(n = 1e2, mean = 0, sd = 1),
  slope = rlnorm(n = 1e2, mean = 0, sd = 1),
  sigma = rexp(n = 1e2, rate = 1/2)
) %>%
  tidyr::expand(height, tidyr::nesting(intercept, slope, sigma)) %>%
  dplyr::mutate(
    mu = intercept + slope * height,
    weight = rnorm(n = 1e4, mean = mu, sd = sigma)
  )

one_prior_data %>% 
  ggplot2::ggplot(ggplot2::aes(x = height, y = weight)) +
  ggplot2::geom_point(alpha = .3)
```

![](week02_vasco-brazao_files/figure-html/one.ppsim.data-1.png)<!-- -->

Finally, we are ready to run the model.


```r
m1 <- rethinking::quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b*height,
    a ~ dnorm(0, 1),
    b ~ dlnorm(0, 1),
    sigma ~ dexp(1/2)
  ), data = d1_z
)

rethinking::precis(m1)
```

```
##                mean         sd        5.5%      94.5%
## a     -9.414733e-08 0.03487860 -0.05574283 0.05574264
## b      7.535857e-01 0.03493899  0.69774646 0.80942498
## sigma  6.547789e-01 0.02466052  0.61536665 0.69419121
```

To make our predictions we can then use the `sim()` function.


```r
heights <- tibble::tibble(
  height = c(140, 160, 175)
)

heights_z <- heights %>% 
  dplyr::mutate(
    height = (height-mean_height)/sd_height
  )

sim.weight <- rethinking::sim(m1, data = heights_z)

weight.PI <- purrr::map_df(
  .x = as.data.frame(sim.weight),
  .f = ~ rethinking::PI(.x),
  prob = .89
)

weight.mean <- purrr::map_dbl(
  .x = as.data.frame(sim.weight),
  .f = ~ mean(.x)
)

dplyr::bind_cols(weight.mean, weight.PI) %>% 
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = function(x) x*sd_weight + mean_weight
    )
  ) %>% 
  dplyr::transmute(
    Height = heights$height,
    `Predicted weight` = `...1`,
    `89% Interval` = paste0("[", round(`5%`, 2), ", ", round(`94%`, 2), "]")
  ) %>% knitr::kable()
```

```
## New names:
## * NA -> ...1
```



| Height| Predicted weight|89% Interval   |
|------:|----------------:|:--------------|
|    140|         35.93127|[28.86, 42.74] |
|    160|         48.37105|[41.45, 55.23] |
|    175|         57.87701|[51.09, 64.58] |

## 2


```r
d2 <- d %>% 
  dplyr::filter(
    age < 13
  )
```

For the total causal effect, we simply need to regress weight on age.

Two things are important for the priors: 1. The intercept should be
positive and somewhere around average weight at birth. 2. The slope
should be positive.

I think we can use the Log-Normal trick here again.

For the intercept: [English
Wikipedia](https://en.wikipedia.org/wiki/Birth_weight) tells us that normal birthweight ranges from 2.5 to 4.5kg. 
The mean of that is 3.5, so that should be the mean of our prior for the intercept. Tinkering with [this online calculator](https://www.omnicalculator.com/statistics/lognormal-distribution) tells me that $\operatorname{Log-Normal}(0.75, 1)$ has a mean of 3.49 and a standard deviation of 4.575. I'll stick with that unless simulation shows it to be absurd.

For the slope, I'll try the same prior as before, $\operatorname{Log-Norma}(0, 1)$.

And so, our model: 

$$ \text{weight}_i \sim \operatorname{Normal}(\mu_i, \sigma) $$
$$ \mu_i = \alpha + \beta \text{age}_i$$
$$ \alpha \sim \operatorname{Log-Normal}(0.75, 1) $$
$$ \beta \sim \operatorname{Log-Normal}(0, 1) $$
$$ \sigma \sim \operatorname{Exponential}(\frac{1}{2}) $$

Let's again simulate many lines to see if this makes sense. Adapting the code from the previous slide to reflect the new priors and swapping height for age gives:


```r
two_prior_lines <- tibble::tibble(
  n = 1:1e2,
  intercept = rlnorm(n = 1e2, mean = 0.75, sd = 1),
  slope = rlnorm(n = 1e2, mean = 0, sd = 1)
) %>% 
  tidyr::expand(tidyr::nesting(n, intercept, slope), age = range(d2$age)) %>% 
  dplyr::mutate(
    weight = intercept + slope * age
  )

two_prior_lines %>% 
  ggplot2::ggplot(ggplot2::aes(x = age, y = weight, group = n)) +
  ggplot2::geom_line(alpha = .2)
```

![](week02_vasco-brazao_files/figure-html/two.ppsim.lines-1.png)<!-- -->

Ok, this seems a little excessive... Let's try to tighten the prior on the intercept and slope a little bit. $\operatorname{Log-Normal}(1.1, 0.5)$ has a mean of 3.40 and a standard deviation of 1.81, which seems a bit more reasonable. 
For the slope, the mean should be around 2: these WHO charts (for [girls](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/cht-wfa-girls-p-0-5.pdf?sfvrsn=d4a8e3bc_12) and [boys](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/cht-wfa-boys-p-0-5.pdf?sfvrsn=7a899731_12)) make me think that is a reasonable number. $\operatorname{Log-Normal}(0.5, 0.7)$ has a mean of 2.11 and a standard deviation of 1.68, so let's try that.

The new model:

$$ \text{weight}_i \sim \operatorname{Normal}(\mu_i, \sigma) $$
$$ \mu_i = \alpha + \beta \text{age}_i$$
$$ \alpha \sim \operatorname{Log-Normal}(1.1, 0.5) $$
$$ \beta \sim \operatorname{Log-Normal}(0.5, 0.7) $$
$$ \sigma \sim \operatorname{Exponential}(\frac{1}{2}) $$

Let's again simulate many lines to see if this makes sense. Adapting the code from the previous slide to reflect the new priors and swapping height for age gives:


```r
two_prior_lines2 <- tibble::tibble(
  n = 1:1e2,
  intercept = rlnorm(n = 1e2, mean = 1.1, sd = 0.5),
  slope = rlnorm(n = 1e2, mean = 0.5, sd = 0.7)
) %>% 
  tidyr::expand(tidyr::nesting(n, intercept, slope), age = range(d2$age)) %>% 
  dplyr::mutate(
    weight = intercept + slope * age
  )

two_prior_lines2 %>% 
  ggplot2::ggplot(ggplot2::aes(x = age, y = weight, group = n)) +
  ggplot2::geom_line(alpha = .2)
```

![](week02_vasco-brazao_files/figure-html/two.ppsim.lines.2-1.png)<!-- -->

This seems a lot more reasonable! Let's estimate with `quap`.


```r
m2 <- rethinking::quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b*age,
    a ~ dlnorm(1.1, 0.5),
    b ~ dlnorm(0.5, 0.7),
    sigma ~ dexp(1/2)
  ), data = d2
)

rethinking::precis(m2)
```

```
##           mean         sd     5.5%    94.5%
## a     7.371900 0.36091499 6.795088 7.948712
## b     1.350492 0.05455439 1.263303 1.437680
## sigma 2.513848 0.14622531 2.280151 2.747544
```

Thus, the marginal intercept is higher than I thought, and the marginal slope is lower than I thought.

Maybe we should plot the data now:


```r
d2 %>% 
  ggplot2::ggplot(ggplot2::aes(x = age, y = weight)) +
  ggplot2::geom_point(alpha = 0.5)
```

![](week02_vasco-brazao_files/figure-html/two.data.plot-1.png)<!-- -->

And now for the full plot:


```r
age.seq <- tibble::tibble(
  age = seq(0, 12, by = 0.2)
)

mu2 <- rethinking::link(m2, data = age.seq)

mu2.mean <- apply(mu2, 2, mean)
mu2.pi <- apply(mu2, 2, rethinking::PI, prob = 0.89)

mu2.tibble <- age.seq %>% 
  dplyr::mutate(
    mu2.mean = mu2.mean,
    mu2.lower = mu2.pi[1,],
    mu2.higher = mu2.pi[2,]
  )

d2 %>% 
  ggplot2::ggplot(ggplot2::aes(x = age, y = weight)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(aes(x = age, y = mu2.mean, ymin = mu2.lower, ymax = mu2.higher), data = mu2.tibble, stat = "identity")
```

![](week02_vasco-brazao_files/figure-html/two.full.plot-1.png)<!-- -->

