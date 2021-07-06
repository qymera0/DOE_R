
# 0 LOAD LIBRARIES --------------------------------------------------------

library(daewr)
library(lme4)
library(lsmeans)


# 5.4 ESTIMATING THE VARIANCE COMPONENTS ----------------------------------

## 5.4.1 Moments method --------------------------------------------------

# Mean squares are equaled to expected values and simultaneous equations are solved.

# In this example:

# E(MSELab) = VAR(error) + c*VAR(lab)

# Var(Error) = MSE(Error)

# Var(Lab) = (E(MSELab) - Var(Error)) / c

mod1 <- aov(conc ~ lab, data = Apo)

sm1 <- summary(mod1)

sm1

# Extract indicators

X <- as.matrix(model.matrix(mod1))

labB <- X[ ,2]

labC <- X[ ,3]

labD <- X[ ,4]

labA <- X[ ,1] - (labB + labC + labD)

# Compute ANOVA with the indicators variables to get the mean squares

s1 <- summary(aov(labA ~ Apo$lab))

x1 <- as.matrix(s1[[1]][1,3])

s2 <- summary(aov(labB ~ Apo$lab))

x2 <- as.matrix(s2[[1]][1,3])

s3 <- summary(aov(labC ~ Apo$lab))

x3 <- as.matrix(s3[[1]][1,3])

s4 <- summary(aov(labD ~ Apo$lab))

x4 <- as.matrix(s4[[1]][1,3])

c <- x1 + x2 + x3 + x4

# Retrieve model error

sigma2 <- as.matrix(sm1[[1]][2, 3])

mslab <- as.matrix(sm1[[1]][1, 3])

cat(" Mean Square for Lab = ",mslab,"\n"," Mean Square for Error = ", sigma2,"\n"," Expected Mean Square for Lab","\n", "Var(error)+",c,"Var(Lab)","\n")

# Solve the equation

sigma2t <- (mslab - sigma2) / c

cat("Method of Moments Variance Component Estimates","\n", "Var(error)=",sigma2, "\n","Var(Lab)=",sigma2t,"\n")

## 5.4.2 Interval estimates ----------------------------------------------


## 5.4.3 MLE and RMLE -----------------------------------------------------

rmod2 <-
        lmer(
                weight ~ 1 + (1|batch),
                data = soupmx
        )

summary(rmod2)

# Confidence interval

pr1 <-
        profile(
                fm1M <- lmer(yield ~ 1 + (1|sample),
                             data = Naph,
                             REML = F)
        )

confint(pr1)


# 5.5 TWO FACTORS SAMPLING DESIGNS ----------------------------------------

# FRSE


## 5.5.1 - Estimating the COV  --------------------------------------------

# Moments Methods

modr1 <- aov(y ~ part + oper + part:oper, data = gagerr)

summary(modr1)

# REML

modr2 <- lmer(y ~ 1 + (1|part) + (1|oper) + (1|part:oper), data = gagerr)

summary(modr2)

# Confidence intervals

vci(
        confl = .9,
        c1 = .05,
        ms1 = .01485,
        nu1 = 2,
        c2 = .05,
        ms2 = .02689,
        nu2 = 18
        
)


# 5.6 NESTED SAMPLING EXPERIMENTS -----------------------------------------

mod2 <- aov(elasticity ~ supplier + supplier:batch + supplier:batch:sample, data = rubber)

modr3 <-
        lmer(
                elasticity ~ 1 + (1|supplier) + (1|supplier:batch) + (1|supplier:batch:sample), data = rubber)

summary(modr3)


# 5.7 STAGGERED NESTED DESIGNS --------------------------------------------

mod2 <- 
        aov(strength ~ lot + lot:box + lot:box:prep,
            data = polymer)

summary(mod2)

modr3 <-
        lmer(
                strength ~ 1 + (1|lot) + (1|lot:box) + (1|lot:box:prep), data = polymer
        )

summary(modr3)

# 5.8 MIXED DESIGNS -------------------------------------------------------

c1 <- c(-.5, .5)

mod5 <-
        lmer(
                residue ~ 1 + form + tech + form:tech + (1|plot:form:tech),
                contrasts = list(
                        form = c1,
                        tech = c1
                ),
                data = pesticide
        )

summary(mod5)

lsmeans(mod5, pairwise ~ tech, adjust = c('tukey'))

anova(mod5)


# 5.9 CHECK MODEL ASSUMPTIONS ---------------------------------------------

# 5.9.2 Gamma and Half Normal plot to constant variance assumption --------

data("Naph")

s2 <- tapply(Naph$yield, Naph$sample, var)

os2 <- sort(s2)

r <- c(1:length(s2))

gscore <-
        qgamma(
                (r - .5) / length(s2),
                2
        )

plot(gscore, os2, main = "Gamma plot of within sample variances", xlab = "Gamma score", ylab = "Sample Variance")
