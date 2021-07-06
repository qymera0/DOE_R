
# 0 LOAD PACKAGES ---------------------------------------------------------

library(FrF2)
library(DoE.base)
library(daewr)
library(AlgDesign)


# 6.2 HALF FRACTION 2K DESIGNS --------------------------------------------

# Generate a 2^(5-1) res. V

design <-
        FrF2(
                nruns = 16,
                nfactors = 5,
                generators = 'ABCD',
                randomize = F
        )

design.info(design)

y <- runif(16, 0, 1)

aliases(lm(y ~ (.)^4, data = design))

# Example

soup <-
        FrF2(
                16, 5, generators = 'ABCD',
                factor.names = list(
                        Ports = c(1, 3),
                        Temp = c('Cool', 'Ambient'),
                        MixTime = c(60, 80),
                        BatchWt = c(1500, 2000),
                        delay = c(7, 1)
                ),
                randomize = F
        )
y <- 
        c(1.13, 1.25, .97, 1.70, 1.47, 1.28, 1.18, .98, .78, 1.36, 1.85, .62, 1.09, 1.10, .76, 2.10 )

soup <- add.response(soup, y)

mod1 <- lm(y ~ (.)^2, data = soup)

summary(mod1)

# Create the design with coded factors

soup <-
        FrF2(
                16, 5, generators = 'ABCD',
                randomize = F
        )

soup <- add.response(soup, y)

modc <- lm(y ~ (.)^2, data = soup)

summary(modc)

LGB(coef(modc)[-1], rpt = FALSE)

IAPlot(modc)

with(soup, interaction.plot(E, B, y))

with(soup, interaction.plot(E, D, y))

# 6.3 QUARTER AND HIGHER FRACTIONS OF 2K DESIGNS --------------------------

frac <-
        FrF2(8, 6, generators = c('AB', 'AC', 'BC'))

frac

# Maximum of clean effects

FrF2(32, 9, MaxC2 = T)

# Example

culture <-
        FrF2(16, 
             generators = c("BCD", "ACD", "ABC", "ABD"),
             randomize = F)

y1 <- c(5.75, 6.7, 11.12, 10.67, 4.92, 5.35, 2.81, 10.83, 6.08, 7.27, 9.68, 4.2, 3.9, 3.78, 11.57, 7.39 )

culture <- add.response(culture, y1)

culture

modf <-
        lm(
                y1 ~ (.)^2, data = culture
        )

summary(modf)

halfnorm(coef(modf)[2:16], alpha = 0.2)

# 6.5 AUGMENTING FRACTIONAL FACTORIALS ------------------------------------


## 6.5.1 Foldover ---------------------------------------------------------

# Same experiment with desired inverse coded factors (all other still the same)

des <-
        FrF2(
                8, 6, generators = c('AB', 'AC', 'BC'),
                randomize = F
        )

# Fold only B and D (columns 2 and 4)

desa <- fold.design(des, columns = c(2, 4))

# Fold all

desb <- fold.design(des, columns = 'full')



## 6.5.2 Optimal design --------------------------------------------------

des2 <-
        FrF2(
                8, 7, generators = c('AB', 'AC', 'BC', 'ABC'),
                randomize = F
        )

augm <- fold.design(des2)

A <- (as.numeric(augm$A) - 1.5 ) / .5
B <- (as.numeric(augm$B) - 1.5 ) / .5
C <- (as.numeric(augm$C) - 1.5 ) / .5
D <- (as.numeric(augm$D) - 1.5 ) / .5
E <- (as.numeric(augm$E) - 1.5 ) / .5
f <- (as.numeric(augm$F) - 1.5 ) / .5
G <- (as.numeric(augm$G) - 1.5 ) / .5

Block <- augm$fold

augm <-
        data.frame(
                A, B, C, D, E, f, G,
                Block
        )

# Generate a full factorial design as a candidate

cand <-
        gen.factorial(
                levels = 2,
                nVars = 7,
                varNames = c('A', 'B', 'C', 'D', 'E', 'f', 'G')
        )

Block <- rep('cand', '128')

cand <- 
        data.frame( 
                A = cand$A, 
                B = cand$B, 
                C = cand$C, 
                D = cand$D,
                E = cand$E, 
                f = cand$f, 
                G = cand$G, 
                Block)
all <-
        rbind(augm, cand)

# Select runs to "maintain"

fr <- 1:16

optim <-
        optFederov(
                ~ A + B + f + I(A*D) + I(C*F),
                data = all,
                nTrials = 24, # Add 8 additional runs
                criterion = 'D',
                nRepeats = 10,
                augment = T,
                rows = fr
        ) 

newRuns <- optim$design[17:24, ]

newRuns

# 6.6 PLACKET-BURMAN ------------------------------------------------------

pb(nruns = 12, randomize = F)
