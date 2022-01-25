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

# how sim works -----------------------------------------------------------

post <- extract.samples(m4.3)

weight.seq <- 25:70

simulate.weight <- function(weight){
  rnorm(
    n = nrow(post),
    mean = post$a + post$b*(weight-xbar),
    sd = post$sigma
  )
}


sim.height <- sapply(weight.seq, simulate.weight)

height.PI <- apply(sim.height, 2, PI, prob = .89)
