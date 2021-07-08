
# 0 LOAD PACKAGES ---------------------------------------------------------

library(desirability)

# 4 AN EXAMPLE ------------------------------------------------------------


conversionPred <- function(x){
                
                81.09 + 1.0284 * x[1] + 4.043 * x[2] + 6.2037 * x[3] - 1.8366 * x[1]^2 + 2.9382 * x[2]^2 - 5.1915 * x[3]^2 + 2.2150 * x[1] * x[2] + 11.375 * x[1] * x[3] - 3.875 * x[2] * x[3]  
                
        } 


activityPred <- function(x){
        
        59.85 + 3.583 * x[1] + 0.2546 * x[2] + 2.2298 * x[3] + 0.83479 * x[1]^2 + 0.07484 * x[2]^2 + 0.05716 * x[3]^2 - 0.3875 * x[1] * x[2] - 0.375 * x[1] * x[3] + 0.3125 * x[2] * x[3]  
        
} 

# Set individual desirabilities

conversionD <- dMax(80, 97)

activityD <- dTarget(55, 57.5, 60)

# Creates a center point of the desing (for pedagogical porpous)

redOutcomes <-
        c(
                conversionPred(c(0, 0, 0)),
                activityPred(c(0, 0, 0))
        )

predict(conversionD, predOutcomes[1])

predict(activityD, predOutcomes[2])

# Overall desirabily

overallD <- dOverall(conversionD, activityD)

print(overallD)

predict(overallD, predOutcomes)


# 5 MAXIMIZING DESIRABILITY -----------------------------------------------

rsmOpt <- function(x, dObject, space = 'square'){
        
        # Predict answers for x values
        
        conv <- conversionPred(x)
        
        acty <- activityPred(x)
        
        # Predict overall desirability
        
        out <-
                predict(
                        dObject,
                        data.frame(
                                conv = conv,
                                acty = acty
                        )
                )
        
        if (space == 'circular') {
                
                if (sqrt(sum(x^2)) > 1.682) out <- 0
                
        } else if (space == 'square') {
                
                if (any(abs(x) > 1.682)) out <- 0
                
        }
        
        out
        
}

# Nelder-Mean simplex

searchGrid <-
        expand.grid(
                time = seq(-1.5, 1.5, length = 5),
                temperature = seq(-1.5, 1.5, length = 5),
                catalyst = seq(-1.5, 1.5, length = 5)
        )

for (i in 1:dim(searchGrid)[1]) {
        
        tmp <- 
                optim(
                       as.vector(searchGrid[i, ]),
                       rsmOpt,
                       dObject = overallD,
                       space = 'square',
                       # Set method to maximize
                       control = list(fnscale = -1)
                )
        
        if (i == 1) {
                
                best <- tmp
                
        } else {
                
                if (tmp$value > best$value) best <- tmp
                
        }
}

print(best)

