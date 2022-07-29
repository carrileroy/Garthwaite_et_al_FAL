library(dplyr)
library(ggplot2)
library(ggpubr)


# Decomposition Collection 0-3 --------------------------------------------

# Read in data
AFDM_dat <- read_csv("data/R/Decomposition.csv")
AFDM_dat$LitterType <- as.factor(AFDM_dat$LitterType)  
str(AFDM_dat)

# Subset
AFDM_cat <- subset(AFDM_dat,LitterType =="Catkin", select = c(Ln.P.AFDM.mg, Days, LitterType))
AFDM_leaf <- subset(AFDM_dat, LitterType =="Leaf", select = c(Ln.P.AFDM.mg, Days, LitterType))

# Run separate linear models for each factor   
AFDM_cat_lm <- lm(Ln.P.AFDM.mg~Days, data=AFDM_cat)  
summary(AFDM_cat_lm)
AFDM_leaf_lm <- lm(Ln.P.AFDM.mg~Days, data=AFDM_leaf)  
summary(AFDM_leaf_lm)

# Model with Ln.P.AFDM.mg=ln(%AFDM+1) regressed against days (methods in stream ecology)
# Diagnostics
AFDM_lm <- lm(Ln.P.AFDM.mg ~ Days * LitterType, data=AFDM_dat )
lindia::gg_diagnose(AFDM_lm)  # All the plots...

# model output
ancova <- anova(AFDM_lm)
ancova

# visualize
ggscatter(
    AFDM_dat, x = "Days", y = "Ln.P.AFDM.mg",
    color = "LitterType", add = "reg.line"
)+
    stat_regline_equation(
        aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = LitterType)
    )


# Leach only --------------------------------------------------------------

# Filter
AFDM_dat_leach <- AFDM_dat %>% filter(Harvest < 2 )

# Visualize
ggscatter(
    AFDM_dat_leach, x = "Days", y = "Ln.P.AFDM.mg",
    color = "LitterType", add = "reg.line"
)+
    stat_regline_equation(
        aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = LitterType)
    )

# ANCOVA
AFDM_leach <- lm(formula = Ln.P.AFDM.mg ~ Days * LitterType, data=AFDM_dat_leach )
ancova_leach <- anova(AFDM_leach)
ancova_leach

# Separate models
# catkin
AFDM_cat_leach <- subset(AFDM_dat_leach,LitterType =="Catkin", select = c(Ln.P.AFDM.mg, Days, LitterType))
Rsquared_cat <- lm(formula = Ln.P.AFDM.mg ~Days, data = AFDM_cat_leach)
summary(Rsquared_cat)


# leaf
AFDM_leaf_leach <- subset(AFDM_dat_leach, LitterType =="Leaf", select = c(Ln.P.AFDM.mg, Days, LitterType))
Rsquared_leaf <- lm(formula = Ln.P.AFDM.mg ~Days, data = AFDM_leaf_leach)
summary(Rsquared_leaf)



# Plots -------------------------------------------------------------------

D <- ggplot(AFDM_dat, aes(Days, Ln.P.AFDM.mg, shape=LitterType, colour=LitterType)) +
    geom_smooth(method="lm", aes(linetype=LitterType)) + 
    geom_point(size=2)+
    scale_shape_manual(values = c(2,15))+
    scale_color_manual(values=c("grey0", "black"))+
    theme_classic(base_size = 12) + 
    scale_y_continuous(limits= c(3.9,4.7), labels= scales::number_format(accuracy = .1))+
    scale_x_continuous(limits= c(0,40), labels= scales::number_format(accuracy = 1))+
    theme(legend.title=element_blank())+
    theme(legend.position=c(.80,.93))+
    theme(legend.key.width = unit(3, "line"))+
    labs(y= "ln (% AFDM remaining)", x= "Days")
D

# 
# png(file = "Figure_2.png", width = 80, height = 80, units= 'mm', res = 600)                     
# D
# dev.off()


