library(car)
library(tidyverse)


#read in data
chem <- read_csv("data/R/Initial_chemistry.csv")
str(chem)
chem$C.L <- as.factor(chem$C.L)  


# Check for normality with RAW data H0=Initial Chem
tapply(chem$CT, chem$C.L, shapiro.test)
tapply(chem$C.CHN, chem$C.L, shapiro.test)
tapply(chem$N.CHN, chem$C.L, shapiro.test)
tapply(chem$C.N.CHN, chem$C.L, shapiro.test)
# All n.s

# Check for equal variances with RAW H0=Initial Chem 
leveneTest(CT ~ C.L, data=chem)
leveneTest(C.CHN ~ C.L, data=chem)
leveneTest(N.CHN ~ C.L, data=chem)#sig
leveneTest(LN.N ~ C.L, data = chem)#sig, use welch's
leveneTest(C.N.CHN ~ C.L, data=chem)

# Two Sample t-test
t.test(CT ~ C.L, data=chem, var.equal=TRUE, conf.level=0.95)  # condensed tannins
t.test(C.CHN ~ C.L, data=chem, var.equal=TRUE, conf.level=0.95) # %C
oneway.test(N.CHN ~ C.L, data=chem) # welch, %N
t.test(C.N.CHN ~ C.L, data=chem, var.equal=TRUE, conf.level=0.95) # C:N ratio




