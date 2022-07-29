library(car)
library(tidyverse)
library(ggpubr)
library(rstatix)

# Read in data
AqInv <- read_csv("data/R/MainTaxa.csv") 
str(AqInv)
AqInv$Litter <- as.factor(AqInv$Litter)  
AqInv$LitterHarvest <- as.factor(AqInv$LitterHarvest)
AqInv$Rebar <- as.factor(AqInv$Rebar)
AqInv$Harvest <- as.factor(AqInv$Harvest)
View(AqInv)
str(AqInv)

# Testing Assumptions for Aq Inv data 
# normality assumption
AqInv %>%
  group_by(Litter, Harvest) %>%
  shapiro_test(Abundance)
AqInv %>%
  group_by(Litter, Harvest) %>%
  shapiro_test(Richness)
AqInv %>%
  group_by(Litter, Harvest) %>%
  shapiro_test(Evenness)
AqInv %>%
  group_by(Litter, Harvest) %>%
  shapiro_test(Shannons.H)

# equal variance assumption
leveneTest(Abundance ~ Litter*Harvest, data=AqInv)
leveneTest(Richness ~ Litter*Harvest, data=AqInv)
leveneTest(Evenness ~ Litter*Harvest, data=AqInv)
leveneTest(Shannons.H ~ Litter*Harvest, data=AqInv)
#all p > 0.05

# two-wat anovas
A<- aov(Abundance ~ Litter * Harvest, data = AqInv)
summary(A)
TukeyHSD(A, "Harvest", ordered = T, conf.level = 0.95)

R<- aov(Richness ~ Litter * Harvest, data = AqInv)
summary(R)
TukeyHSD(R, "Harvest", ordered = T, conf.level = 0.95)
std <- function(x) sd(x)/sqrt(length(x))
std(AqInv$Richness)
summary(AqInv$Richness)

E<- aov(Evenness ~ Litter * Harvest, data = AqInv)
summary(E)
TukeyHSD(E, "Harvest", ordered = T, conf.level = 0.95)

D<- aov(Shannons.H ~ Litter * Harvest, data = AqInv)
summary(D)
TukeyHSD(D, "Harvest", ordered = T, conf.level = 0.95)

# visualize
global_size = 12
Abundance <- ggplot(AqInv, aes(x=Litter,y=Abundance))+
  geom_boxplot(aes(fill = factor(Litter))) +
  scale_fill_manual(values=c("white", "grey10"))+
  guides(fill=FALSE)+
  geom_signif(comparisons = list(c("Catkins", "Leaves")), 
              map_signif_level=TRUE, test = 't.test', y_position = 215)+
  labs(y= "Abundance", x= "", tag ="(a)")
Abundance
A.bp <- Abundance + theme_classic(base_size = global_size) + theme(axis.title.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank())+scale_y_continuous(limits= c(0,220),labels= scales::number_format(accuracy = 1))
A.bp

Richness<- ggplot(AqInv, aes(x=Litter,y=Richness))+
  theme_classic(base_size = 12)+
  geom_boxplot(aes(fill = factor(Litter))) +
  scale_fill_manual(values=c("white", "grey10"))+
  guides(fill=FALSE)+
  stat_compare_means(comparisons = list(c("Catkins", "Leaves")), label = "p.signif", method = 't.test', vjust = 0, label.y = 14, bracket.size =.5)+
  labs(y= "Richness", x= "", tag ="(d)")+
  theme(text=element_text(size=12))
Richness
R.bp <- Richness  + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank())+scale_y_continuous(limits= c(0,20), labels= scales::number_format(accuracy = 1))#+ theme_classic(base_size = 12))
R.bp

Evenness<- ggplot(AqInv, aes(x=Litter,y=Evenness))+
  theme_classic(base_size = 12)+
  geom_boxplot(aes(fill = factor(Litter))) +
  scale_fill_manual(values=c("white", "grey10"))+
  guides(fill=FALSE)+
  geom_signif(comparisons = list(c("Catkins", "Leaves")), 
              map_signif_level=TRUE, test = 't.test', y_position = .8)+
  labs(y= "Evenness", x= "", tag ="(b)")
Evenness
E.bp <- Evenness + theme(axis.title.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank())+scale_y_continuous(limits= c(0,.9),labels= scales::number_format(accuracy = 0.1))
E.bp

Diversity<- ggplot(AqInv, aes(x=Litter,y=Shannons.H))+
  geom_boxplot(aes(fill = factor(Litter))) +
  scale_fill_manual(values=c("white", "grey10"))+
  guides(fill=FALSE)+
  geom_signif(comparisons = list(c("Catkins", "Leaves")), 
              map_signif_level=TRUE, test = 't.test', y_position = 1.8)+
  labs(y= "Diversity", x= "", tag ="(c)")
Diversity
D.bp <- Diversity + theme_classic(base_size = 12) + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank())+scale_y_continuous(limits= c(0,2),labels= scales::number_format(accuracy = 0.1))
D.bp


allplots <- ggarrange(A.bp, E.bp, D.bp, R.bp,
                      ncol = 2, nrow = 2, align="v")
allplots

# png(file = "Figure_X.png", width = 160, height = 160,units= 'mm', res = 600) 
# allplots
# dev.off()


