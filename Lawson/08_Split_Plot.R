
# 0 LOAD LIBRARIES --------------------------------------------------------

library(tidyverse)
library(AlgDesign)
library(FrF2)
library(daewr)
library(GAD)
library(lme4)
library(lsmeans)
library(pbkrtest)

# 1 GENERATE DESIGNS ------------------------------------------------------


## 1.1 Bake time with 3 levels --------------------------------------------

sp <-
        expand.grid(
                trayT = factor(c('RoomT', 'Hot')),
                bakeT = factor(c('low', 'mid', 'high'))
        )

wp <-
        data.frame(
                short = factor(c('100%', '80%'))
        )

# Double wp size

wp <- rbind(wp, wp)

splitP <-
        optBlock(
                ~ short * (trayT + bakeT + trayT:bakeT),
                withinData = sp,
                blocksizes = rep(6, 4),
                wholeBlockData = wp
        )

## 1.2 Bake time with 2 levels --------------------------------------------

Sp <-
        FrF2(
                nruns = 16,
                nfactors = 3,
                WPs = 4,
                nfac.WP = 1,
                factor.names = list(
                        short = c('100%', '80%'),
                        bakeT = c('low', 'high'),
                        trayT = c('RoomT', 'Hot')
                )
        )


# 2 DATA ANALYSIS ---------------------------------------------------------

load("~/R/doe/Lawson/dataSets/splitPdes.rda")

splitFinal <-
        splitPdes %>% 
        mutate(
                short = as.fixed(short), 
                batch = as.random(batch),
                bakeT = as.fixed(bakeT),
                trayT = as.fixed(trayT)
        )


# On the simple anova mode, the F-test for the whole is created using the general MSe

model <-
        aov(
                formula =  y ~ short + short%in%batch + bakeT + trayT + 
                        short*bakeT + short*trayT + bakeT*trayT + short*bakeT*trayT,
                data = splitFinal
        )

# On gad, the F-test for the whole is created using the WP error 

gad(model)

reml <-
        lmer(
                y ~ 1 + short + (1|short:batch) + bakeT + trayT + short:bakeT + short:trayT + 
                        bakeT:trayT + short:bakeT:trayT,
                data = splitFinal
        )

anova(reml)

summary(reml)


# 3 RCB in WP/SP RBSP -----------------------------------------------------

## 3.1 Create design ----------------------------------------------------

sp <-
        expand.grid(
                poleStiff = factor(c('light', 'medLight')),
                lureW = factor(c('light', 'heavy'))
        )

wp <-
        data.frame(
                lineW = factor(c('6lb', '10lb', '20lb'))
        )

wp <- 
        rbind(wp, wp)

splitP <- 
        optBlock(
                ~ lineW * (poleStiff + lureW + poleStiff:lureW),
                withinData = sp,
                blocksizes = rep(4, 6),
                wholeBlockData = wp
        )

fisherman <-
        factor(
                c(rep(1:2, each = 12))
        )

splitP$design <- cbind(fisherman, splitP$design)

## 3.2 Analysis example --------------------------------------------------

load("~/R/doe/Lawson/dataSets/sausage.rda")

rmle2 <-
        lmer(
                ys ~ A + B + A:B + (1|Block) + (1|A:B:Block) + C + D + C:D + A:C + A:D + B:C + B:D + A:B:C + A:B:D + A:C:D + B:C:D + A:B:C:D,
                data = sausage
                
        )

summary(rmle2)

anova(rmle2)

# Significância A

1 - pf(28.6517, 1, 3)

# Significância para os sp

# df = 32 - 1 - 1 - 3 - 15 = 12

1 - pf(16.048, 1, 12)

## 3.3 Least square means ------------------------------------------------

lsmeans(rmle2, ~ A)

lsmeans(rmle2, ~ C)
