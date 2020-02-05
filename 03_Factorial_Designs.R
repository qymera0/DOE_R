
library(daewr)

# 3 CREATING A 2 FACTOR FACTORIAL -----------------------------------------

D <- expand.grid(BW = c(3.25, 3.75, 4.25), WL = c(4, 5, 6) )

# Replicate runs

D <- rbind(D, D)

# Randomize

set.seed(2591)

D <- D[order(sample(1:18)), ]

CopterDes <- D[ c( "BW", "WL" )]


# 3.5 ANALYSIS ------------------------------------------------------------

par(mfrow = c(1, 1))

mod1 <- aov( CO ~ Eth * Ratio, data = COdata )

summary(mod1)

model.tables( mod1, type = "means", se = T )

# Plot Effects

with(COdata, (interaction.plot(Eth, Ratio, CO, type = "b",
                               pch = c(18,24,22), leg.bty = "o",
                               main = "Interaction Plot of Ethanol and air/fuel ratio",
                               xlab = "Ethanol",ylab = "CO emissions")))


Ethanol <- COdata$Eth

with(COdata, (interaction.plot(Ratio, Ethanol, CO, type = "b",
                               pch = c(18,24,22), leg.bty = "o",
                               main="Interaction Plot of Ethanol and air/fuel ratio", 
                               xlab = "Ratio", ylab = "CO emissions")))

# Power Analysis

rmin <- 2 # smallest number of replicates

rmax <- 8 # largest number of replicates

sigma <- .32

alpha <- .05

Delta <- 1

nlev <- 16

nreps <- c(rmin:rmax)

power <- Fpower1(alpha, nlev, nreps, Delta, sigma)

options(digits = 5)

power


# 3.6 FACTORIAL DESIGNS MULTIPLE FACTORS ----------------------------------

data(web)

modb <- glm( cbind(signup, visitors-signup) ~ A * B * C * D, data = web, family = binomial )

anova(update(modb, .~ A+B + A:B + C + A:C + B:C + A:B:C + D + A:D+B:D + A:B:D + C:D + A:C:D + B:C:D + A:B:C:D ),
      test = "Chisq")


prop <- web$signup / web$visitors

webp <- data.frame(web,prop)

par ( mfrow = c(1,3) )

webp1 <- subset(webp, A == 1)

interaction.plot(webp1$C, webp1$D, webp1$prop, type = "l", legend=FALSE, ylim = c(.015,.0275), main = "Background = 1", 
                 xlab = "Text Color", ylab = "Proportion Signing-up")

webp2 <- subset(webp, A == 2 )

interaction.plot( webp2$C, webp2$D, webp2$prop, type = "l", legend = FALSE, ylim = c(.015,.0275), 
                  main = "Background = 2", xlab = "Text Color", ylab = " ")

lines( c(1.7,1.85), c(.016,.016), lty = 2)

lines( c(1.7,1.85), c(.017,.017) ,lty = 1)

text(1.3, .017, "Sign-up link ")

text(1.3, .016, "Sign-up Button" )

text(1.4, .018, "LEGEND" )
 
webp3 <- subset(webp, A == 3)
 
interaction.plot(webp3$C, webp3$D, webp3$prop, type = "l", 
                 legend=FALSE, ylim = c(.015,.0275), main="Background = 3",
                 xlab = "Text Color", ylab = " ")


# 3.7 TWO LEVEL FACTORIAS -------------------------------------------------

library(DoE.base)

data(volt)

modv <- lm( y ~ A*B*C, data=volt, contrast = list(A=contr.FrF2, B=contr.FrF2, C=contr.FrF2))

summary(modv)

par(mfrow = c(1,1))

C_Warmup=volt$C

with(volt, (interaction.plot(A, C_Warmup, y, type = "b", 
                             pch = c(24,22), leg.bty = "o",
                             xlab = "Temperature",ylab = "Voltage")))


modf <- lm( y ~ A*B*C*D, data = chem)

summary(modf)

fullnormal(coef(modf)[-1],alpha=.2)

LGB(coef(modf)[-1], rpt = FALSE)

with(chem, (interaction.plot( A, B, y, type = "b", pch = c(18,24), 
                              main = "Interaction Plot of Catalyst by Excess A",
                              xlab = "Excess Reactant A", ylab = "Percent Conversion")))


par( mfrow = c(2,1))

library(BsMD)

LenthPlot(modf, main = "Lenth Plot of Effects")

X <- model.matrix(modf)[ , 2:16]

y <- chem$y

Chem.BsProb <- BsProb( X = X, y = y, blk = 0, mFac = 15, mInt = 1, p = 0.2, g = 2.49, ng = 1, nMod = 10)

plot( Chem.BsProb, main = "Bayes Plot of Effects" )


# 3.8 MODEL ASSUMPTIONS ---------------------------------------------------

data(BoxM)

Gaptest(BoxM)

