---
title: "Rethinking Distributions Cheat Sheet"
author: "VB & VAAN"
date: "Last updated: Jan 5 2022"
output: 
  html_document:
    toc: true
    toc_float: true
    keep_md: true
  pdf_document: default
---




```r
set.seed(1159)
```

## Intention

The intention for this doc is to update it with useful notes about the statistical distributions that show up in the Statistical Rethinking textbook, including probability density / mass functions, some moments when relevant, and, especially, comments on why one might choose specific distributions for different uses.

# Distributions

## Binomial

Rethinking notation:

$$ X \sim \operatorname{Binomial}(N, p) $$

Probability mass function:

$$ P(X=x|N,p) = \binom{N}{x} p^x(1-p)^{N-x} $$

Where $N\in\{1,2,...\}$ is the number of trials, $x\in\{1,2,...,N\}$ is the number of successes, $p\in[0,1]$ is the probability of success, and $\binom{N}{x}$ is "$N$ choose $x$", aka the binomial coefficient (see the relevant section for more: [The binomial coefficient]).

### Usefulness

This distribution is very useful as a **likelihood** when we want to model the probability of success, i.e., of one of the options of a binary response.

### R commands

#### Random draw

`rbinom(n, size, prob)` generates a vector with the number of successes (we called that $x$) in each of `n` draws from a binomial distribution where $N$ equals `size` and $p$ equals `prob`.


```r
example.rbinom <- rbinom(
  n = 5,
  size = 10,
  prob = .8
)

example.rbinom
```

```
## [1] 7 9 8 8 9
```

In this example, there were 7 successes out of 10 attempts in the first draw, 9 successes out of 10 attempts in the second draw, and so on.

#### Density

`dbinom(x, size, prob)` computes the probability mass function $P(X=x|N,p)$, where $x$ equals `x`, $N$ equals `size`, and $p$ equals `prob`. So, for example, the probability of observing 4 successes out of 10 attempts, if each has a probability of 0.7 of occurring is:


```r
dbinom(
  x = 4,
  size = 10,
  prob = .7
)
```

```
## [1] 0.03675691
```


## Gaussian (normal)

Rethinking notation:

$$ y_i \sim \operatorname{Normal}(\mu, \sigma) $$

This is what is meant elsewhere by

$$ y_i \sim \mathcal{N}(\mu, \sigma) $$

or even

$$ y_i \stackrel{iid}{\sim} \mathcal{N}(\mu, \sigma) $$
(iid = independent and identically distributed, meaning each value $h_i$ is independent of the others, and all come from the same distribution function)

Probability density function:

$$ p(y|\mu,\sigma) = \frac{1}{\sqrt{2\pi\sigma^2}}
                                 \text{exp}\left(-\frac{(y-\mu)^2}{2\sigma^2}\right) $$

The PDF can also be parameterized with $\tau = 1/\sigma^2$, as such:

$$ p(y|\mu,\tau) = \sqrt{\frac{\tau}{2\pi}}\text{exp}\left(-\frac{1}{2}\tau(y-\mu)^2\right) $$

# Other useful things

## Combinatorics

### The binomial coefficient

The binomial coefficient, also known as "$N$ choose $x$" allows us to calculate the number of *combinations* of size $x$ of $N$ different elements that are not repeated. A combination does not take order into account. Thus, for example, there are only three ways to combine two elements out of the set $\{A, B, C\}$, namely $\{A, B\}$, $\{A, C\}$, and $\{B, C\}$.
 
 $$ C_{N}^x = {}^{N}C_{x} = \binom{N}{x} = \frac{N!}{x!(N - x)!} $$
 
Online, you mostly find $n$ and $k$ instead of $N$ and $x$, respectively.

You can use `choose(n, k)` to compute them.


```r
# how many ways to pick exactly 7 diferent elements out of 10 total elements?
choose(10, 7)
```

```
## [1] 120
```

[Note: could be good to include here a good explainer for the formula?]
