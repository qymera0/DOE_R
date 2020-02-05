
# 2.2 GENERATE DATA COLLECTION --------------------------------------------

set.seed(7638)

f <- factor(rep(c(35, 40, 45), each = 4))

fac <- sample( f, 12 )

eu <- 1:12

plan <- data.frame(loaf = eu, time = fac)

write.csv( plan, file = "Plan.csv", row.names = FALSE)


# 2.4 LINEAR MODEL FOR CRD ------------------------------------------------

library(daewr)

mod0 <- lm(height ~ time, data = bread)

summary(mod0)

library(gmodels)

fit.contrast(mod0, "time", c(1, -1,0))

mod1 <- aov( height ~ time, data = bread )

summary(mod1)


# 2.5 CHECK ADEQUACY ------------------------------------------------------

par(mfrow = c(2,2))

plot(mod1, which=5)

plot(mod1, which=1)

plot(mod1, which=2)

plot(residuals(mod1) ~ loaf, main ="Residuals vs Exp. Unit", font.main = 1, data = bread)

abline(h = 0, lty = 2)


# 2.6 ASSUMPTIONS VIOLATED ------------------------------------------------

# 2.6.1 Box Cox

library(MASS)

par(mfrow = c(1,1))

bc <- boxcox(mod1)

lambda <- bc$x[which.max(bc$y)]

lambda

# Modeling with Box Cox

tbread <- transform(bread, theight = height^(lambda))

mod2 <- aov( theight~time, data = tbread )

summary(mod2)

par(mfrow = c(2,2))

plot(mod2, which=5)

plot(mod2, which=1)

plot(mod2, which=2)

plot(residuals(mod2) ~ loaf, main ="Residuals vs Exp. Unit", font.main = 1, data = bread)

abline(h = 0, lty = 2)


# 2.6.3 ALTERNATIVES TO LS ------------------------------------------------

# 2.6.3.1 Weighted

with(bread, { 
                std <- tapply(height, time, sd)
                weights <- rep( 1/std, each = 4 )
                mod3 <- lm( height ~ time, weights = weights, data = bread )
                anova( mod3 )
 })


# 2.6.3.2 Comparing Models

modf <- polr( score ~ method, weight = count, data=teach)

modr <- polr( score ~ 1, weight = count, data = teach)

anova(modf,modr)


# 2.7 NUMBER OF REPLICATES ------------------------------------------------

rmin <- 2 #smallest number of replicates considered

rmax <- 6 # largest number of replicates considered

alpha <- rep(0.05, rmax - rmin +1)

sigma <- sqrt(2.1)

nlev <- 3

nreps <- rmin:rmax

Delta <- 3

power <- Fpower1(alpha,nlev,nreps,Delta,sigma)

power
