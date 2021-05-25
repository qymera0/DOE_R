library(acebayes)

# 2 APROXIMATE COORDINATE EXCHANGE ----------------------------------------

## 2.2 acebebayes implementation ------------------------------------------

# Simple poisson response 

# Fisher utility
utility_fisher <- function(d, B){
        
        theta <- rnorm(B)
        
        ui <- matrix(rep(d[ ,1]^2, B), ncol = B) * exp(outer(d[ ,1], theta))
        
        apply(ui, 2, sum)
        
}

# Application of ace

set.seed(1)

# Number of runs
n <- 12

# Initial design (zero matrix)
startD <- matrix(0, nrow = n, ncol = 1)

ex22a <-
        ace(
                utility = utility_fisher,
                start.d = startD
        )

ex22a

# Randomly generated designs

c <- 10

startD <- list()

for(i in 1:c){
        
        startD[[i]] <- matrix(runif(n = n, min = -1, max = 1), ncol = 1)
        
}

ex22b <- pace(utility = utility_fisher, start.d = startD)

ex22b

# Compare design with a zeros

assess(d1 = ex22a, d2 = matrix(rep(0, n), ncol = 1), n.assess = 100)

# Compare a and b examples

assess22 <- assess(d1 = ex22a, d2 = ex22b, n.assess = 100)

plot(assess22)


# 4 EXAMPLES --------------------------------------------------------------


## 4.1 Compartmental non-linear model -------------------------------------

# Setting up the initial designs

set.seed(1)

# number of runs

n <- 18

# number of variables

k <- 1

# number of parameters

p <- 3

# Results for 24 hours (original lhs holds only between 0 and 1)

startD <- randomLHS(n = n, k = k) * 24

colnames(startD) <- c('t')

# Define constant priors

a1 <- c(0.01884, 0.298)

a2 <- c(0.09884, 8.298)

prior <-
        list(
                support = 
                        cbind(
                                rbind(
                                        a1, a2
                                ),
                        c(21.8, 21.8)
                )
        )

colnames(prior[[1]]) <- c('theta1', 'theta2', 'theta3')

# Run the design

ex441 <-
        acenlm(
                formula = ~ theta3 * (exp(-theta1 * t) - (exp(-theta2 * t))),
                start.d = startD,
                prior = prior,
                lower = 0,
                upper = 24
        )

assess(d1 = ex441, d2 = ex441$phase1.d)

length(unique(ex441$phase1.d))

length(unique(ex441$phase2.d))

# Using constrain between measurements

limits <- function(d, i, j){
        
        grid <- seq(from = 0, to = 24, length.out = 10000)
        
        for(s in as.vector(d)[-i]){
                
                grid <- grid[(grid < (s - 0.25)) | (grid > (s + 0.25))]
        }
        
        grid
        
}

ex442 <-
        acenlm(
                formula = ~ theta3 * (exp(-theta1 * t) - (exp(-theta2 * t))),
                start.d = startD,
                prior = prior,
                lower = 0,
                upper = 24,
                limits = limits,
                N2 = 0
        )

# Repeating initial designs

c <- 10

startD <- list()

for(i in 1:c){
        
        startD[[i]] <- randomLHS(n = n, k = k) * 24
        
        colnames(startD[[i]]) <- c('t')
        
}

ex442 <-
        pacenlm(
                formula = ~ theta3 * (exp(-theta1 * t) - (exp(-theta2 * t))),
                start.d = startD,
                prior = prior,
                lower = 0,
                upper = 24,
                limits = limits,
                N2 = 0
        )


## 4.2 Logistic regression ------------------------------------------------

set.seed(1)

n <- 6

p <- 5

k <- 4

c <- 10

startD <- list()

for(i in 1:c){
        
        startD[[i]] <- randomLHS(n = n, k = k) * 2 - 1
        
        colnames(startD[[i]]) <- c('x1', 'x2', 'x3', 'x4')
        
}

# Pseudo-bayesian A-optimal design

a1 <- c(-3, 4, 5, -6, -2.5)

a2 <- c(3, 10, 11, 0, 3.5)

prior <- list(support = rbind(a1, a2))

ex411 <-
        paceglm(
                formula = ~ x1 + x2 + x3 + x4,
                family = binomial,
                start.d = startD,
                prior = prior,
                criterion = 'A'
        )

assess(d1 = ex411, d2 = ex411$final.d[[1]])

