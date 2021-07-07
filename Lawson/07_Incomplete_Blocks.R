
# 0 LOAD PACKAGES ---------------------------------------------------------

library(daewr)
library(AlgDesign)
library(lsmeans)


# 7.2 BIBD ----------------------------------------------------------------

BIBsize(6, 3)

bib <- optBlock(
        ~ .,
        withinData = factor(1:6),
        blocksizes = rep(3, 10)
)

des <- bib$rows

dim(des) <- NULL

des <-
        matrix(
                des,
                nrow = 10,
                ncol = 3,
                byrow = T,
                dimnames = list(c("Block1", "Block2", "Block3", "Block4", "Block5", "Block6", "Block7", "Block8", "Block9", "Block10"), c("unit1", "unit2", "unit3"))
        )

des

# 7.3 ANALYSIS OF BIBD ----------------------------------------------------

mod1 <-
        aov(
                score ~ panelist + recipe,
                data = taste
        )

summary(mod1)

lsmeans(mod1, pairwise ~ recipe, adjust = ('tukey'))


# 7.4 BTIB AND PBIB DESIGNS -----------------------------------------------

mod2 <-
        aov(
                pressure ~ Block + Treatment,
                data = BPmonitor
        )

lsmeans(mod2, pairwise ~ Treatment, adjust = ('tukey'))
