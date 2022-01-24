library(rethinking)

data(Howell1)

d <- Howell1[Howell1$age >= 18, ]

xbar <- mean(d$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d
)


# how link works ----------------------------------------------------------

post <- extract.samples(m4.3)

mu.link <- function(weight) {
  post$a + post$b*(weight-xbar)
}

weight.seq <- seq(from = 25, to = 79, by = 1) 

mu <- sapply(weight.seq, mu.link)

## extra processing

mu.mean <- apply(mu, 2, mean)

mu.CI <- apply(mu, 2, rethinking::PI, prob = .89)


# actual link function ----------------------------------------------------
weight.seq.2 <- seq(from = 20, to = 81, by = 1)

link.m4.3 <- link(fit = m4.3, data = list(weight = weight.seq.2))

mu.mean <- apply(link.m4.3, 2, mean)

mu.CI <- apply(link.m4.3, 2, rethinking::PI, prob = .89)
