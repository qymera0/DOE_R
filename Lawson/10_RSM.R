
# 0 LOAD PACKAGES ---------------------------------------------------------

library(rsm)
library(Vdgraph)
library(daewr)
library(AlgDesign)
library(lme4)

# 10.3 STANDARD DESIGNS ---------------------------------------------------

# CCD

rotd <-
        ccd(
                3,
                n0 = c(4, 2),
                alpha = 'rotatable'
        )

rotdm <- rotd[ ,3:5]

# Variance of a predicted value

Vdgraph(rotdm)

# When the design is not rotatable, there are three distinct lines from Max, Min and Avg

# It can also be seen that the value of the scaled variance of a predicted value at the origin is nearly equal to the variance at radius 1.0, indicating the design has uniform precision.

# Box-Behnken design

# 10.4 CREATING STANDARD RSM DESIGNS --------------------------------------

# Avaliable designs

ccd.pick(3)

ccdUp <-
        ccd(
                y ~ x1 + x2 + x3,
                n0 = c(4, 2),
                alpha = 'rotatable',
                randomize = F
        )

Vdgraph(ccdUp[ ,3:5])

# Change the coding secheme

ccdUp <-
        ccd(
                y ~ x1 + x2 + x3,
                n0 = c(4, 2),
                alpha = 'rotatable',
                coding = list(
                        x1~(Temp-150)/10,
                        x2~(Press-50)/5, 
                        x3~(Rate-4/1)     
                ),
                randomize = F
        )

head(ccdUp)

# Box-Behnken

treb <-
        bbd(
                y ~ x1 + x2 + x3,
                randomize = F,
                n0 = 3,
                coding = list(
                        x1~(A-6)/2, 
                        x2~(B-15)/5, 
                        x3~(C-2.5)/.5)
        )
        

data(D310)

D310


# 10.5 NON-STANDARD RSM DESIGNS -------------------------------------------

data(qsar)

design1 <-
        optFederov(
                ~quad(.),
                data = qsar,
                nTrials = 15,
                center = T,
                criterion = 'D',
                nRepeats = 40
        )

design2 <-
        optFederov(
                ~quad(.),
                data = qsar,
                nTrials = 15,
                center = T,
                criterion = 'I',
                nRepeats = 40
        )

Compare2FDS(design1$design, design2$design, 'D-optimal', 'I-optimal', mod = 2)


# 10.7 DETERMINING OPTIMUM OPERATING CONDITIONS ---------------------------

# Graphical optimization

data("Treb")

trebQuad <- rsm(y ~ SO(x1, x2, x3), data = Treb)

par(mfrow = c(2, 2))

contour(trebQuad, ~ x1 + x2 + x3)

par(mfrow = c(2, 2))

persp(
        trebQuad,
        ~ x1 + x2 + x3,
        zlab = 'Distance',
        contours = list(
                z = 'bottom'
        )
)

# Canonical analysis

# Ridge analysis

ridge <-
        steepest(
                trebQuad,
                dist = seq(0, 1.412, by = .1),
                descent = F
        )

ridge


# # Non linear optimization -----------------------------------------------

start <- c(12.5, 400)

# Equations to be optimized

prod <- function(x){
        
        time <- x[1]
        
        Temp <- x[2]
        
        k1 <- .523*exp(-9847*((1/Temp - 1/400)))
        
        k2 <- .2*exp(-12327*((1/Temp - 1/400)))
        
        f <- 132*(exp(-k1*time) - exp(-k2*time))*k1/(k1 - k2)
        
}

# Constrains for time and Temperature

ui <- matrix(c(1, -1, 0, 0, 0, 0, 1, -1), 4, 2)

ci <- c(0, -25, 375, -425)

# Run the optimization

constrOptim(
        start,
        prod,
        NULL,
        ui,
        ci
)


# 10.8 BLOCKED RESPONSE SURFACE -------------------------------------------

# Box Behen

bbd(4, n0 = 1, randomize = F)

bbd(5, n0 = 3, randomize = F)

# 10.9 RSM SPLIT PLOT -----------------------------------------------------

data(cake)

mmod <-
        lmer(
                y ~ x1 + x2 + x1:x2 + x1sq + x2sq + (1|Ovenrun),
                data = cake
        )

summary(mmod)
