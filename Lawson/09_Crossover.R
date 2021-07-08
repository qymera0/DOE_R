
# 0 LOAD PACKAGES ---------------------------------------------------------

library(daewr)
library(car)
library(lsmeans)
library(lme4)
library(GAD)


# 9.3 SIMPLE AB BA --------------------------------------------------------

mod1 <-
        lm(
                pl ~ Subject + Period + Treat,
                data = antifungal,
                contrasts = list(
                        Subject = contr.sum,
                        Period = contr.sum,
                        Treat = contr.sum
                )
        )

Anova(mod1, type = 'III')

lsmeans(mod1, pairwise ~ Treat)

# Carryover effects

c1 <- c(.5, -.5)

mod2 <-
        lmer(
                pl ~ 1 + Group + (1|Subject:Group) + Period + Treat,
                contrast = list(
                        Group = c1,
                        Period = c1,
                        Treat = c1
                ),
                data = antifungal
        )

summary(mod2)

# Modified designs to accomodate carryover

data("bioequiv")

mod3 <-
        lm(
                y ~ Subject + Period + Treat + Carry,
                data = bioequiv,
                contrast = list(
                        Subject = contr.sum,
                        Period = contr.sum,
                        Treat = contr.sum,
                        Carry = contr.sum
                )
        )

Anova(mod3, type = 'III', singular.ok = T)

lsmeans(mod3, ~ Treat)

# 9.4 CROSSOVER DESIGNS FOR MULTIPLE TREATMENTS ---------------------------


# 9.6 REPEATED MEASURE DESIGNS --------------------------------------------

data("strung")

# Using GAD package

D <- as.fixed(strung$Diet)

W <- as.fixed(strung$week)

C <- as.random(strung$Cow)

mod4 <-
        lm(
                protein ~ D + C%in%D + W + D*W,
                data = strung
        )

summary(mod4)

gad(mod4)

mod5 <-
        lmer(
                protein ~ 1 + Diet*week + (1|Cow:Diet),
                data = strung
        )

anova(mod5)

# Test of Huynh-Feldt condition

# Could use 'cast function'
pr1 <- strung$protein[1:30]
pr2 <- strung$protein[31:60]
pr3 <- strung$protein[61:90]
pr4 <- strung$protein[91:120]

dairy <-
        data.frame(
                Diet = as.factor(strung$Diet[1:30]),
                pr1, pr2, pr3, pr4
        )

mod6 <-
        lm(cbind(pr1, pr2, pr3, pr4) ~ Diet,
           data = dairy)

summary(mod6)

time <- factor(c(1:4))

idata <- data.frame(time)

(mod7 <- Anova(mod6, idata = idata, idesign = ~ time))

summary(mod7, multivariate = F)
