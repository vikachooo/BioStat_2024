rm (list = ls())

library(dplyr)
library(ggplot2)

##### USEFUL FUNCTIONS #####

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(mean(x) - 3*sd(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), mean(x) + 3*sd(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

##### LECTURE ####

# 2

soccer_general <- read.csv("soccer.csv", sep=";")[, 2:6] %>%
  mutate(Position = as.factor(Position),
         Nationality = as.factor(Nationality),
         Age = as.numeric(Age),
         Height = as.numeric(Height)) %>%
  filter(Nationality %in% c("Spanish", "Italian", "German", "English", "Argentinian"))

set.seed(1)

soccer_wrk <- soccer_general[sample(1:nrow(soccer_general), 150), ] %>%
  mutate(Nationality = factor(Nationality))

# 3 

soccer_wrk %>%
  with(
    boxplot(Height ~ Position, col = "cadetblue3", pch = 20,
            ylab = "Height (cm)")
  )

# 10 

ggplot(aes(y = Height, x = Position), data = soccer_wrk) +
  ylim(155, 210) +
  geom_jitter(colour = "cornsilk4", position=position_jitter(width=.2)) + 
  ggtitle("Boxplot: mean + sd") + 
  xlab("Position") + 
  ylab("Height (cm)") +
  theme_classic()

# 11

ggplot(aes(y = Height, x = Position), data = soccer_wrk) +
  ylim(155, 210) + 
  ggtitle("Boxplot: mean + sd") + 
  xlab("Position") + 
  ylab("Height (cm)") +
  theme_classic() +
  geom_hline(yintercept = mean(soccer_wrk$Height),
             lwd = 1,
             colour = "salmon2") +
  geom_jitter(colour = "cornsilk4", position=position_jitter(width=.2))

# 12

ggplot(aes(y = Height, x = Position), data = soccer_wrk) +
  ylim(155, 210) + 
  ggtitle("Boxplot: mean + sd") + 
  xlab("Position") + 
  ylab("Height (cm)") +
  theme_classic() +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",
               colour = "cadetblue3") +
  geom_hline(yintercept = mean(soccer_wrk$Height),
             lwd = 1,
             colour = "salmon2") +
  geom_jitter(colour = "cornsilk4", position=position_jitter(width=.2))

# 18

soccer_wrk %>%
  with(
    boxplot(Height ~ Position, col = "cadetblue3", pch = 20,
            ylab = "Height (cm)")
  )

# 19

soccer_wrk %>% pull(Height) %>% var*(nrow(soccer_wrk)-1)

SST <- with(soccer_wrk, (sum(
  (Height - mean(Height))^2
)))

SST

# 20

SSe <- soccer_wrk %>% 
  group_by(Position) %>% 
  mutate(residuals = Height - mean(Height)) %>%
  with(sum(residuals^2))

SSe

# 21

SST
SSe
SST-SSe

F <- (SST - SSe)/3 / ((SSe)/(150-3))
F

# 22

lm(Height ~ Position, data = soccer_wrk) %>% anova

# 24, 27

aov(Height ~ Position, data = soccer_wrk) %>% summary
oneway.test(Height ~ Position, data = soccer_wrk)

# 28

library(multcomp)

m0 <- lm(Height ~ Position, data = soccer_wrk)
HBW.comp <- m0 %>%  glht(linfct = mcp(Position = "Tukey"))

# 29

HBW.comp %>% summary()

# 30

HBW.comp %>% confint()

# 31

par(mar = c(5, 10, 4, 2)+0.1)
HBW.comp %>% plot(xlab = "Height difference (cm)")
par(mar = c(5, 10, 4, 2)+0.1)

# 33

library(sandwich)

HBW.comp.hetero <- m0 %>%  glht(linfct = mcp(Position = "Tukey"), 
                                vcov = vcovHC(m0, type = "HC4"))
HBW.comp %>% summary
HBW.comp.hetero %>% summary

# 34

HBW.comp %>% confint
HBW.comp.hetero %>% confint

# 35

par(mar = c(5, 10, 4, 2)+0.1)
HBW.comp %>% plot(xlab = "Height difference (cm)")
HBW.comp.hetero %>% plot(xlab = "Height difference (cm)")
par(mar = c(5, 10, 4, 2)+0.1)

