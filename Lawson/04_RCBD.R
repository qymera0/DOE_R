
# 0 LOAD PACKAGES ---------------------------------------------------------

library(tidyverse)
library(agricolae)
library(daewr)

# 4.2 CREATING RCBD IN R --------------------------------------------------

# Using base R

f <- factor(c(1, 2, 3, 4))

b1t <- sample(f, 4)

b2t <- sample(f, 4)

b3t <- sample(f, 4)

b4t <- sample(f, 4)

treat <- t(cbind(t(b1t), t(b2t), t(b3t), t(b4t)))

block <-
        factor(
                rep(c('carnation', 'daisy', 'rose', 'tulip'), each = 4)
        )

flnum <- rep(f, 4)

plan <-
        data.frame(
                type.Flower = block,
                flower.Number = flnum,
                treatment = treat
        )

# Using packages

treat <- c(1, 2, 3, 4)

outdesign <-
        design.rcbd(treat, 4, seed = 11)

rcb <- outdesign$book

levels(rcb$block) <- c('carnation', 'daisy', 'rose', 'tulip')

# 4.4 EXAMPLE -------------------------------------------------------------

mod <- aov(rate ~ rat + dose, data = drug)

summary(mod)

# Test constratst

contrasts(drug$dose) <- contr.poly(5)

mod2 <- aov(rate ~ rat + dose, data = drug)

summary.aov(
        mod2,
        split = list(
                dose = list(
                        'Linear' = 1,
                        'Quadratic' = 2,
                        'Cubic' = 3,
                        'Quartic' = 4
                )
        )
)

# Plot the relationship

R <- do.call('cbind', split(drug$rate, drug$rat))

y <- apply(R, 1, mean)

x <- as.double(levels(drug$dose))

plot(x, y, xlab = "dose", ylab = "average lever press rate" )

xx <- seq(0.0, 2.0, .1)

rateQuad <- lm(y ~ poly(x, 2))

lines(xx, predict(rateQuad, data.frame(x = xx)))

# 4.5 NUMBER OF BLOCKS ----------------------------------------------------

# 4.6 FACTORIAL DESIGNS IN BLOCKS -----------------------------------------

mod3 <- aov(y ~ block + strain * treat, data = bha)

summary(mod3)

# 4.7 GENERALIZED COMPLETE BLOCK DESIGN -----------------------------------

# Right way

mod <- aov(cdistance ~ teehgt + Error(id/teehgt), data = rcb)

summary(mod)

# Wrong way

md2 <- aov(cdistance ~ id*teehgt, data = rcb)

summary(md2)

# Another right way

cellMeans <-
        tapply(rcb$cdistance, list(rcb$id, rcb$teehgt), mean)

dim(cellMeans) <- NULL

teehgt <-
        factor(
                rep(
                        c(1:3),
                        each = 9
                        
                )
        )

id <-
        factor(
                rep(
                        c(1:9),
                        3
                        
                )
        )

mod5 <- aov(cellMeans ~ id + teehgt)

summary(mod5)

model.tables(mod5, type = 'means')$tables$teehgt

# TukeyHSD

TukeyHSD(mod5, 'teehgt')
