################################################################################
# Simulation for ICE
################################################################################
library("tidyverse")
library("dplyr")
library("ggplot2")
library("lsmeans")
library("fitdistrplus")
library("MASS")
library("car")
library("tibble")
library("glmmADMB")

# Loading the real data and organizing it
Tobacco = read.csv("/media/alejandro/42D2-7BC2/ICE_Simulation/Data2012_Total.csv", 
                   colClasses = c("Date", "factor", "factor", "factor", "integer", 
                                  "factor", "factor", "factor", "integer"))
Tobacco$Block = c(rep(1:4, each = 8))

Tobacco = tbl_df(Tobacco)

by_treatment_wat = Tobacco %>% 
        group_by(Treatment, WAT) %>%
        summarise(Count = n(),
                  Mean = mean(TBW),
                  MeanProp = mean(TBW/Plants),
                  Sum = sum(TBW),
                  AIC.TBW.norm = fitdist(TBW, "norm")$aic,
                  AIC.TBW.pois = fitdist(TBW, "pois")$aic,
                  AIC.TBW.nb = fitdist(TBW, "nbinom")$aic)

by_wat_treament = Tobacco %>% 
        group_by(WAT, Treatment) %>%
        summarise(Count = n(),
                  Mean = mean(TBW),
                  MeanProp = mean(TBW/Plants),
                  Sum = sum(TBW),
                  AIC.TBW.norm = fitdist(TBW, "norm")$aic,
                  AIC.TBW.pois = fitdist(TBW, "pois")$aic,
                  AIC.TBW.nb = fitdist(TBW, "nbinom")$aic)

TBW.norm = fitdist(Tobacco$TBW, "norm")
qqp(Tobacco$TBW, "norm")

TBW.pois = fitdist(Tobacco$TBW, "pois")
qqp(Tobacco$TBW, "pois", TBW.pois$estimate)

TBW.nb = fitdist(Tobacco$TBW, "nbinom")
qqp(Tobacco$TBW, "nbinom", size = TBW.nb$estimate[[1]], mu = TBW.nb$estimate[[2]])

cdfcomp(list(TBW.nb, TBW.pois, TBW.norm))
gofstat(list(TBW.nb, TBW.pois, TBW.norm))
#################################################################################
TBWS = c()
for(i in 1:32){
  TBW = sort(rpois(16,
                   lambda = by_treatment_wat$Mean[i]))
  TBWS = c(TBWS, TBW)
}

# test for fit

TBWS.norm = fitdist(TBWS, "norm")
qqp(TBWS, "norm")

TBWS.pois = fitdist(TBWS, "pois")
qqp(TBWS, "pois", TBWS.pois$estimate)

TBWS.nb = fitdist(TBWS, "nbinom")
qqp(TBWS, "nbinom", size = TBWS.nb$estimate[[1]], mu = TBWS.nb$estimate[[2]])

plot(TBWS.norm)
plot(TBWS.pois)
plot(TBWS.nb)

cdfcomp(list(TBWS.nb, TBWS.pois, TBWS.norm))
gofstat(list(TBWS.nb, TBWS.pois, TBWS.norm))
### 
Treatment = rep(1:8, each = 64)
WAT = rep(3:6, each = 16, times = 8)
Location = rep(c("K", "RM"), each = 8, times = 32)
Block = rep(1:4, each = 2, times = 64)
Row = rep(2:3, 256)


Simulated_df = data_frame(Treatment, WAT, Location, Block, TBWS)
Simulated_df$Treatment = as.factor(Simulated_df$Treatment)
Simulated_df$WAT = as.factor(Simulated_df$WAT)
Simulated_df$Block = as.factor(Simulated_df$Block)
Simulated_df$Location = as.factor(Simulated_df$Location)
Simulated_df

by_treatment_wat_sim = Simulated_df %>% 
        group_by(Treatment, WAT) %>%
        summarise(Count = n(),
                  Mean = mean(TBWS),
                  Sum = sum(TBWS))
######################################################
plot_tobacco = ggplot(aes(x = WAT, y = TBW, color = Treatment), 
                      data = Tobacco)
plot_tobacco + geom_boxplot() +
  theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(vjust=0.5, size=16)) +
        geom_hline(yintercept = 6) +
        annotate("text", label = "10% threshold", x = 1.3, y = 7, size = 8, colour = "red")

plot_tobacco1 = ggplot(aes(x = Treatment, y = TBW, color = WAT), 
                      data = Tobacco)
plot_tobacco1 + geom_boxplot() +
        theme(axis.title.x = element_text(face="bold", size=20),
              axis.text.x  = element_text(vjust=0.5, size=16),
              axis.title.y = element_text(face="bold", size=20),
              axis.text.y  = element_text(vjust=0.5, size=16)) +
        ylim(0, 22)
##############################################################################
plot_tobacco + geom_boxplot() +
        theme(axis.title.x = element_text(face="bold", size=20),
              axis.text.x  = element_text(vjust=0.5, size=16),
              axis.title.y = element_text(face="bold", size=20),
              axis.text.y  = element_text(vjust=0.5, size=16)) +
        ylim(0, 22)

plot_simulated = ggplot(aes(x = WAT, y = TBWS, color = Treatment), 
                        data = Simulated_df)
plot_simulated + geom_boxplot() +
  theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(vjust=0.5, size=16))
################################################################################
model_NB1 = glmmadmb(TBW ~ Treatment * WAT, 
                     random = ~ Block | Location,
                     family = "nbinom", 
                     data = Tobacco)

fullNB1.rg1 = ref.grid(model_NB1, data = Tobacco)
fullNB1.lsmeans = lsmeans(fullNB1.rg1, ~ Treatment * WAT)

fullNB1.sum = summary(fullNB1.rg1, infer = c(TRUE, TRUE), 
                      adjust = "bon", by = "Treatment",
                      type = "response")

NBResultTreatment = cld(fullNB1.lsmeans, by = "Treatment", sort = FALSE, type = "response")
cld(fullNB1.lsmeans, by = "WAT", sort = FALSE, type = "response")

model_Pois = glmmadmb(TBW ~ Treatment * WAT, 
                      random = ~ Block | Location,
                      family = "poisson", 
                      data = Tobacco)

fullPois.rg1 = ref.grid(model_Pois, data = Tobacco)
fullPois.lsmeans = lsmeans(fullPois.rg1, ~ Treatment * WAT)

fullPois.sum = summary(fullPois.rg1, infer = c(TRUE, TRUE), 
                       adjust = "bon", by = "Treatment",
                       type = "response")

cld(fullPois.lsmeans, by = "Treatment", sort = FALSE, type = "response")
cld(fullPois.lsmeans, by = "WAT", sort = FALSE, type = "response")

model_Norm = glmmadmb(TBW ~ Treatment * WAT,
                      random = ~ Block | Location,
                      family = "gaussian",
                      data = Tobacco)

fullNorm.rg1 = ref.grid(model_Norm, data = Tobacco)
fullNorm.lsmeans = lsmeans(fullNorm.rg1, ~ Treatment * WAT)

fullNorm.sum = summary(fullNorm.rg1, infer = c(TRUE, TRUE), 
                       adjust = "bon", by = "Treatment",
                       type = "response")

cld(fullNorm.lsmeans, by = "Treatment", sort = FALSE, type = "response")
cld(fullNorm.lsmeans, by = "WAT", sort = FALSE, type = "response")
##########################################################################################
model_Arcsine = glmmadmb(asin(sqrt(TBW/Plants)) ~ Treatment * WAT,
                      random = ~ Block | Location,
                      family = "gaussian",
                      data = Tobacco)

fullArcsine.rg1 = ref.grid(model_Arcsine, data = Tobacco)
fullArcsine.lsmeans = lsmeans(fullArcsine.rg1, ~ Treatment * WAT)

fullArcsine.sum = summary(fullArcsine.rg1, infer = c(TRUE, TRUE), 
                       adjust = "bon", by = "Treatment",
                       type = "response")

cld(fullArcsine.lsmeans, by = "Treatment", sort = FALSE, type = "response")
cld(fullArcsine.lsmeans, by = "WAT", sort = FALSE, type = "response")

model_Binom = glmmadmb(cbind(TBW, (Plants-TBW)) ~ Treatment * WAT,
                       random = ~ Block | Location,
                       family = "binomial",
                       data = Tobacco)

fullBinom.rg1 = ref.grid(model_Binom, data = Tobacco)
fullBinom.lsmeans = lsmeans(fullBinom.rg1, ~ Treatment * WAT)

fullBinom.sum = summary(fullBinom.rg1, infer = c(TRUE, TRUE), 
                        adjust = "bon", by = "Treatment",
                        type = "response")

cld(fullBinom.lsmeans, by = "Treatment", sort = FALSE, type = "response")
cld(fullBinom.lsmeans, by = "WAT", sort = FALSE, type = "response")

##########################################################################################
library("coefplot")
