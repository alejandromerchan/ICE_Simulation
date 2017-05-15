# Put model estimates into temporary data.frames:
lev = factor(c(rownames(summary(model_NB1)$coef)), 
             levels = c(rownames(summary(model_NB1)$coef)))
model1Frame = data.frame(Variable = lev,
                          Coefficient = summary(model_NB1)$coef[, 1],
                          SE = summary(model_NB1)$coef[, 2],
                          modelName = "Negative Binomial")

model2Frame = data.frame(Variable = lev,
                          Coefficient = summary(model_Pois)$coef[, 1],
                          SE = summary(model_Pois)$coef[, 2],
                          modelName = "Poisson")
levels(model2Frame$Variable) = c(rownames(summary(model_Pois)$coef))

model3Frame = data.frame(Variable = lev,
                          Coefficient = summary(model_Norm)$coef[, 1],
                          SE = summary(model_Norm)$coef[, 2],
                          modelName = "Normal")
levels(model3Frame$Variable) = c(rownames(summary(model_Norm)$coef))

# Combine these data.frames
allModelFrame = data.frame(rbind(model1Frame, model2Frame, model3Frame))  # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
#zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
#                                ymax = Coefficient + SE*interval1),
#                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw() 
zp1 <- zp1 + ggtitle("Comparing several models") +
        theme(axis.title.x = element_text(face="bold", size=20),
              axis.text.x  = element_text(vjust=0.5, size=16),
              axis.title.y = element_text(face="bold", size=20))
print(zp1) # The trick to these is position_dodge().
###########################################################################################
# Put model estimates into temporary data.frames:
lev = factor(c(rownames(summary(model_Binom)$coef)), 
             levels = c(rownames(summary(model_Binom)$coef)))
model4Frame = data.frame(Variable = lev,
                         Coefficient = summary(model_Binom)$coef[, 1],
                         SE = summary(model_Binom)$coef[, 2],
                         modelName = "Binomial")

model5Frame = data.frame(Variable = lev,
                         Coefficient = summary(model_Arcsine)$coef[, 1],
                         SE = summary(model_Arcsine)$coef[, 2],
                         modelName = "Arcsine")
levels(model5Frame$Variable) = c(rownames(summary(model_Arcsine)$coef))


# Combine these data.frames
allModelFrame = data.frame(rbind(model4Frame, model5Frame))  # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
zp2 <- ggplot(allModelFrame, aes(colour = modelName))
zp2 <- zp2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
#zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
#                                ymax = Coefficient + SE*interval1),
#                            lwd = 1, position = position_dodge(width = 1/2))
zp2 <- zp2 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp2 <- zp2 + coord_flip() + theme_bw() 
zp2 <- zp2 + ggtitle("Comparing several models") +
        theme(axis.title.x = element_text(face="bold", size=20),
              axis.text.x  = element_text(vjust=0.5, size=16),
              axis.title.y = element_text(face="bold", size=20))
print(zp2) # The trick to these is position_dodge().
