
# 0 LOAD PACKAGEZ ---------------------------------------------------------

library(daewr)
library(leaps)
library(car)
library(lsmeans)
library(nlme)

# 10.4 ANALYSIS OF PRODUCT ARRAY EXPERIMENTS ------------------------------

## 12.4.1 Loss model or Location-Dispersion modeling ---------------------

### Example 01 ----------------------------------------------------------

data("tile")

# Mean model

modyb <-
        lm(
                ybar ~ A + B + C + D + E + F + G,
                data = tile
        )

summary(modyb)

cfsMean <- coef(modyb)[-1]

halfnorm(cfsMean, names(cfsMean), alpha = .2)

# Variance model

modlv <-
        lm(
                lns2 ~ A + B + C + D + E + F + G,
                data = tile
        )

summary(modlv)

cfsVar <- coef(modlv)[-1]

halfnorm(cfsVar, names(cfsVar), alpha = .5)

### Example 02 -----------------------------------------------------------

data(cont)

mod <-
        transform(
                cont,
                XA = (A - 4)/2, 
                XB = (B - 2), 
                XC = (C - 8)/8, 
                XD = (D - 10)/2, 
                XF = (F - 6)/1.2 
        )

xvar <-
        transform(
                mod,
                XA2 = XA*XA, XB2 = XB*XB, XC2 = XC*XC, XD2 = XD*XD, XF2 = XF*XF,
                XAB = XA*XB, XAC = XA*XC, XAD = XA*XD, XAF = XA*XF, XBC = XB*XC,
                XBD = XB*XD, XBF = XB*XF, XCD = XC*XD, XCF = XC*XF, XDF = XD*XF
        )

modc <-
        regsubsets(
                lns2 ~ XA + XB + XC + XD + XF + XA2 + XB2 + XC2 + XD2 + XF2 + XAB 
                + XAC + XAD + XAF + XBC + XBD + XBF + XCD + XCF + XDF,
                data = xvar,
                nvmax = 8,
                nbest = 4
        )

rs <- summary(modc)

plot(c(rep(1:8,each = 4)), 
     rs$adjr2, 
     xlab = "No. of Parameters", 
     ylab = "Adjusted R-square")

# Modelo reduzido

pmod <-
        lm(
                lns2 ~ XB + XC + XD + XF + XAF + XBD,
                data = xvar
        )

summary(pmod)


# # 12.4.2 Response modeling ----------------------------------------------

# Example 01

# Consider noise and design on different plots

smod <-
        lm(
                y ~ A + B + C + D + E + F + G + H + AH + BH + CH + DH + EH + FH
                + GH,
                data = strungtile
        )

summary(smod)

effects <- coef(smod)[2:16]

Wpeffects <- effects[1:7]

Speffects <- effects[8:15]

halfnorm(Wpeffects, names(Wpeffects), alpha = .10)

halfnorm(Speffects, names(Speffects), alpha = .25)


# 12.5 SINGLE ARRAY PARAMETER DESIGN --------------------------------------

# Exmple 01

rmod <-
        lm(
                torque ~ A + B + C + D + E + A:B + A:C + A:D + A:E,
                data = Smotor,
                contrasts = list(
                        A = contr.sum, 
                        B = contr.sum,
                        C = contr.sum, 
                        D = contr.sum,
                        E = contr.sum
                )
        )

Anova(rmod, type = 'III')

lsmeans(rmod, ~ C)

lsmeans(rmod, ~ A:B)


# 12.6 JOINT MODELING -----------------------------------------------------

eptaxg <-
        groupedData(y ~ 1|A, data = eptaxr)

cnt <-
        lmeControl(opt = 'optim')

modj <-
        lme(
                y ~ D + H,
                data = eptaxg,
                weights = varIdent(form = ~ 1 | A),
                control = cnt
        )

summary(modj)


## 12.6.2 Designing a Noise Array ----------------------------------------


