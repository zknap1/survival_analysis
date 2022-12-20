rm(list = ls())

#Nacitanie balíkov
library(tidyr)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(SurvRegCensCov)
library(survMisc)
library(muhaz)
library(kernhaz)
library(timereg)
library(cowplot)

#setwd("C:/Users/zknap/Desktop/diplomka/AP")
data2 = read.table("peacedata.csv", head = T, sep = ",")
summary(data2)


# Uprava -------------------------------------------------

data2$Pohlavie <- as.factor(data2$gender)
data2$Liecba <- as.factor(data2$tx)
data2$Vek <- (data2$age)
data2$Diabetes <- as.factor(data2$hidiabet)
data2$Hypertenzia <- as.factor(data2$hihypert)

set.seed(1234)
data2$time <- data2$t2death
data2$time2 <- ifelse(data2$t2death == 0, runif(1), data2$time)



# jadrovy odhad rizikovej funkcie -----------------------------------------

#vyhladeny odhad pomocou Epanecnikovho jadra

# jadrovy odhad pomocou baliku kernhaz a muhaz
hazard.smooth <- muhaz(data2$time2, data2$death)
hazard.rate.df <-
  data.frame(hazard.smooth$est.grid, hazard.smooth$haz.est)

ggplot(
  data = hazard.rate.df,
  mapping = aes(x = hazard.smooth.est.grid, y = hazard.smooth.haz.est)
) +
  geom_line() +
  xlab("Èas") +
  ylab ("Odhad rizikovej funkcie") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )






# NZCH pre zavisle premenne -----------------------------------------------

## pre TX - typ liecby
# porovnam, ktora skupina ma vyssiu nzsiu funkciu prezivania, ktora vyssie kumulativne riziko


fit.tx <- survfit(Surv(time2, death) ~ tx, data = data2)

# Funkcia prezivania
g41 <- ggsurvplot(
  fit.tx,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  palette = c("#0000ff", "#ff0000"),
  legend.title = " ",
  legend.labs = c("Placebo", "Liek"),
  legend = "none",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
  
) + xlab("Cas") + ylab("Funkcia prežívania")

# Kumulativne riziko
g42 <- ggsurvplot(
  fit.tx,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  fun = "cumhaz",
  pval = FALSE,
  palette = c("#0000ff", "#ff0000"),
  legend.title = " ",
  legend.labs = c("Placebo", "Liek"),
  legend = "none",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")

ggsurvplot(
  fit.tx,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  fun = "cumhaz",
  pval = FALSE,
  palette = c("#0000ff", "#ff0000"),
  legend.title = " ",
  legend.labs = c("Placebo", "Liek"),
  legend = "none",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")


g41.plot <- g41$plot
g42.plot <- g42$plot

prow <- plot_grid(
  g41.plot + theme(legend.position = "none"),
  g42.plot + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b <- get_legend(g41.plot + theme(legend.position = "bottom"))
p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .2))
p



## pre pohlavie ##

fit.sex <- survfit(Surv(time2, death) ~ gender, data = data2)

# Funkcia prezivania
g31 <- ggsurvplot(
  fit.sex,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  palette = c("#00c8ff", "#ff0066"),
  legend.title = " ",
  legend.labs = c("Muži", "Ženy"),
  legend = "none",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
  
) + xlab("Cas") + ylab("Funkcia prežívania")

# Kumulativne riziko

g32 <- ggsurvplot(
  fit.sex,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  fun = "cumhaz",
  pval = FALSE,
  ggtheme = theme_classic(),
  palette = c("#00c8ff", "#ff0066"),
  legend.title = " ",
  legend.labs = c("Muži", "Ženy"),
  legend = "none",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")


ggsurvplot(
  fit.sex,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  fun = "cumhaz",
  pval = FALSE,
  ggtheme = theme_classic(),
  palette = c("#00c8ff", "#ff0066"),
  legend.title = " ",
  legend.labs = c("Muži", "Ženy"),
  legend = "none",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")

g31.plot <- g31$plot
g32.plot <- g32$plot

prow2 <- plot_grid(
  g31.plot + theme(legend.position = "none"),
  g32.plot + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b <- get_legend(g31.plot + theme(legend.position = "bottom"))
p2 <- plot_grid(prow2,
                legend_b,
                ncol = 1,
                rel_heights = c(1, .2))
p31 <- plot_grid(p, p2, nrow = 2, rel_heights = c(1, 1))
p31



## history of diabetes ##

fit.hidiabet <-
  survfit(Surv(time2, death) ~ hidiabet, data = data2)


# Funkcia prezivania
g11 <- ggsurvplot(
  fit.hidiabet,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  palette = c("#3CB371", "#ff0000"),
  legend.title = " ",
  legend.labs = c("Bez diabetu", "S diabetom"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
  
) + xlab("Cas") + ylab("Funkcia prežívania")

# Kumulativne riziko

g12 <- ggsurvplot(
  fit.hidiabet,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  fun = "cumhaz",
  ggtheme = theme_classic(),
  pval = FALSE,
  palette = c("#3CB371", "#ff0000"),
  legend.title = " ",
  legend.labs = c("Bez diabetu", "S diabetom"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")

ggsurvplot(
  fit.hidiabet,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  fun = "cumhaz",
  ggtheme = theme_classic(),
  pval = TRUE,
  palette = c("#3CB371", "#ff0000"),
  legend.title = " ",
  legend.labs = c("Bez diabetu", "S diabetom"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")


g11.plot <- g11$plot
g12.plot <- g12$plot

prow2 <- plot_grid(
  g11.plot + theme(legend.position = "none"),
  g12.plot + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b <- get_legend(g11.plot + theme(legend.position = "bottom"))
p11 <- plot_grid(prow2,
                 legend_b,
                 ncol = 1,
                 rel_heights = c(1, .2))


## history of hypertension ##

fit.hihypert <-
  survfit(Surv(time2, death) ~ hihypert, data = data2)


# Funkcia prezivania
g21 <- ggsurvplot(
  fit.hihypert,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  palette = c("#194C30", "#4C1935"),
  legend.title = " ",
  legend.labs = c("Bez hypertenzie", "S hypertenziou"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
  
) + xlab("Cas") + ylab("Funkcia prežívania")

# Kumulativne riziko

g22 <- ggsurvplot(
  fit.hihypert,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  pval = FALSE,
  fun = "cumhaz",
  ggtheme = theme_classic(),
  palette = c("#194C30", "#4C1935"),
  legend.title = " ",
  legend.labs = c("Bez hypertenzie", "S hypertenziou"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")

ggsurvplot(
  fit.hihypert,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  pval = FALSE,
  fun = "cumhaz",
  ggtheme = theme_classic(),
  palette = c("#194C30", "#4C1935"),
  legend.title = " ",
  legend.labs = c("Bez hypertenzie", "S hypertenziou"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")


g21.plot <- g21$plot
g22.plot <- g22$plot

prow2 <- plot_grid(
  g21.plot + theme(legend.position = "none"),
  g22.plot + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b <- get_legend(g21.plot + theme(legend.position = "bottom"))
p2 <- plot_grid(prow2,
                legend_b,
                ncol = 1,
                rel_heights = c(1, .2))
p32 <- plot_grid(p11, p2, nrow = 2, rel_heights = c(1, 1))
p32





## grafy pre vek a SYSBP ##

fit.age <- survfit(Surv(time2, death) ~ age, data = data2)

# urèenie cut point podla Klein, 8.6
cut.point.age <- as.numeric(cutp(fit.age)$age[1, 1])

# kodovanie: 0 pre hodnoty menšie ako cut point, a 1 inak
age.cat2 <- ifelse(data2$age < cut.point.age, 0, 1)

data2$age.cat2 <- as.factor(age.cat2)

fit.age.cat2 <-
  survfit(Surv(time2, death) ~ age.cat2, data = data2)


# Funkcia prezivania
graf.age1 <- ggsurvplot(
  fit.age.cat2,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  palette = c("#F2904F", "#3B4270"),
  legend.title = " ",
  legend.labs = c("vek < 67", "vek >= 67"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16),
  
) + xlab("Cas") + ylab("Funkcia prežívania")

# Kumulativne riziko

graf.age2 <- ggsurvplot(
  fit.age.cat2,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  pval = FALSE,
  fun = "cumhaz",
  ggtheme = theme_classic(),
  palette = c("#F2904F", "#3B4270"),
  legend.title = " ",
  legend.labs = c("Vek < 67", "Vek >= 67"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")

# pre systolicky tlak

fit.sysbp <- survfit(Surv(time2, death) ~ sysbp, data = data2)

# urèenie cut point podla Klein, 8.6
cut.point.sysbp <- as.numeric(cutp(fit.sysbp)$sysbp[1, 1])

# kodovanie: 0 pre hodnoty menšie ako cut point, a 1 inak
sysbp.cat2 <- ifelse(data2$sysbp < cut.point.sysbp, 0, 1)


fit.sysbp.cat2 <-
  survfit(Surv(time2, death) ~ sysbp.cat2, data = data2)



# Funkcia prezivania
graf.sysbp1 <- ggsurvplot(
  fit.sysbp.cat2,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  ggtheme = theme_classic(),
  #palette = c("#DP9057", "#1E87D2"),
  legend.title = " ",
  legend.labs = c("systolický tlak  < 124", "systolický tlak >= 124"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16),
) + xlab("Cas") + ylab("Funkcia prežívania")

# Kumulativne riziko

graf.sysbp2 <- ggsurvplot(
  fit.sysbp.cat2,
  size = 0.5,
  censor = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.1,
  pval = FALSE,
  fun = "cumhaz",
  ggtheme = theme_classic(),
  #palette = c("#DP9057", "#1E87D2"),
  legend.title = " ",
  legend.labs = c("Systolický tlak  < 124", "Systolický tlak >= 124"),
  legend = "bottom",
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16)
) + xlab("Cas") + ylab("Kumulatívne riziko")



graf.age1.plot <- graf.age1$plot
graf.age2.plot <- graf.age2$plot

prow2 <- plot_grid(
  graf.age1.plot + theme(legend.position = "none"),
  graf.age2.plot + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b <-
  get_legend(graf.age1.plot + theme(legend.position = "bottom"))
p1 <- plot_grid(prow2,
                legend_b,
                ncol = 1,
                rel_heights = c(1, .2))


graf.sysbp1.plot <- graf.sysbp1$plot
graf.sysbp2.plot <- graf.sysbp2$plot

prow3 <- plot_grid(
  graf.sysbp1.plot + theme(legend.position = "none"),
  graf.sysbp2.plot + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b2 <-
  get_legend(graf.sysbp1.plot + theme(legend.position = "bottom"))
p2 <- plot_grid(prow3,
                legend_b2,
                ncol = 1,
                rel_heights = c(1, .2))


p33 <- plot_grid(p1, p2, nrow = 2, rel_heights = c(1, 1))
p33



# Coxov model -------------------------------------------------------------



cox.model.full <-
  coxph(Surv(time2, death) ~ Diabetes + Vek + Hypertenzia + sysbp + Pohlavie +
          Liecba,
        data = data2)
summary(cox.model.full)

#vyhodenie SYSBP
cox.testL <-
  coxph(Surv(time2, death) ~ Diabetes + Hypertenzia + Vek + Liecba + Pohlavie,
        data = data2)
anova(cox.testL, cox.model.full)

#vyhodenie Liecby
cox.testP <-
  coxph(Surv(time2, death) ~ Diabetes + Hypertenzia + Vek + Pohlavie, data = data2)
anova(cox.testP, cox.model.full)

cox.final <-
  coxph(Surv(time2, death) ~ Diabetes + Hypertenzia + Vek + Pohlavie, data = data2)
summary(cox.final)


# Overenie funkcionalnej formy pre vek ------------------------------------



#ggcoxfunctional(Surv(time2, death) ~ Vek, data = data2)

cox.model5 <-
  coxph(Surv(time2, death) ~ 1,
        data = data2)

cox.model5

rr <- resid(cox.model5, type = "martingale") #martingale residuals

y <- lowess(data2$Vek, rr, iter = 0)
df <- data.frame(data2$Vek, rr)
df2 <- data.frame(data2$Vek, y)

ggplot(data = df, mapping = aes(x = data2.Vek, y = rr)) + geom_abline(slope =
                                                                        0,
                                                                      intercept = 0,
                                                                      lty = 2) +
  geom_point() +
  geom_line(
    data = df2,
    aes(x = x, y = y),
    linetype = "solid",
    color = "red",
    size = 0.75
  ) +
  xlab("Vek") +
  ylab ("martingalové rezíduá") +
  #ggtitle("Graf martingalových rezíduí - lineárna závislos")+
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )





# Predpoklad prop. rizik --------------------------------------------------


test.ph <-
  cox.zph(cox.final)
test.ph$table

scaled_sch <- data.frame(test.ph$time, test.ph$y)


g1 <-
  ggplot(scaled_sch, aes(x = test.ph.time, y = Diabetes)) + geom_point(alpha = 0.5) + geom_smooth(col = 2) +
  geom_abline(slope = 0,
              intercept = 0,
              lty = 2) +
  ylab("Šk. Schoen.rezduá pre diabetes") + xlab("Èas") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )
g2 <-
  ggplot(scaled_sch, aes(x = test.ph.time, y = Hypertenzia)) + geom_point(alpha = 0.5) + geom_smooth(col = 2) +
  geom_abline(slope = 0,
              intercept = 0,
              lty = 2) +
  ylab("Šk. Schoen.rezduá pre hypertenziu") + xlab("Èas") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

g3 <-
  ggplot(scaled_sch, aes(x = test.ph.time, y = Pohlavie)) + geom_point(alpha = 0.5) + geom_smooth(col = 2) +
  geom_abline(slope = 0,
              intercept = 0,
              lty = 2) +
  ylab("Šk. Schoen.rezduá pre pohlavie") + xlab("Èas") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

g4 <-
  ggplot(scaled_sch, aes(x = test.ph.time, y = Vek)) + geom_point(alpha = 0.5) + geom_smooth(col = 2) +
  geom_abline(slope = 0,
              intercept = 0,
              lty = 2) +
  ylab("Šk. Schoen.rezduá pre vek") + xlab("Èas") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )


sc.sch.plot <- plot_grid(g1,
                         g2,
                         g3,
                         g4,
                         align = 'h',
                         nrow = 2,
                         ncol = 2)
sc.sch.plot






# log-kumulatívne riziko
cox.model.strata <-
  coxph(
    Surv(time2, death) ~ strata(Diabetes) + strata(Hypertenzia) + Vek + strata(Pohlavie),
    data = data2
  )
base <- basehaz(cox.model.strata)

ggplot(base, aes(x = log(time))) +
  geom_step(aes(y = log(hazard), group = strata)) +
  ylab(expression(hat(H)(t))) + xlab("t") + aes(col = strata) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

data.p1 <- data2[data2$Diabetes == "1", ]
cox.model.strata <-
  coxph(Surv(time2, death) ~ strata(Pohlavie) + strata(Hypertenzia) + Vek,
        data = data.p1)
base1 <- basehaz(cox.model.strata)

log1 <- ggplot(base1, aes(x = log(time))) +
  geom_step(aes(y = log(hazard), group = strata)) +
  ylab("Log-kumulativne riziko") + xlab("Logaritmus casu") + aes(col = strata) +
  theme_classic() + ggtitle("S diabetom") +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) + scale_colour_manual(
    "",
    values = c("blue", "red", "green", "orange"),
    breaks = c("0, 0", "0, 1", "1, 0", "1, 1"),
    labels = c(
      "Pohl.=0, Hypert.=0",
      "Pohl.=0, Hypert.=1",
      "Pohl.=1, Hypert.=0",
      "Pohl.=1, Hypert.=1"
    )
  )
table(data2[, c(9, 12, 13)])


data.p0 <- data2[data2$Diabetes == "0", ]
cox.model.strata <-
  coxph(Surv(time2, death) ~ strata(Pohlavie) + strata(Hypertenzia) + Vek,
        data = data.p0)
base0 <- basehaz(cox.model.strata)

log2 <- ggplot(base0, aes(x = log(time))) +
  geom_step(aes(y = log(hazard), group = strata)) +
  ylab("Log-kumulativne riziko") + xlab("Logaritmus casu") + aes(col = strata) +
  theme_classic() + ggtitle("S diabetom") +
  theme_classic() + ggtitle("Bez diabetu") +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) + scale_colour_manual(
    "",
    values = c("blue", "red", "green", "orange"),
    breaks = c("0, 0", "0, 1", "1, 0", "1, 1"),
    labels = c(
      "Pohl.=0, Hypert.=0",
      "Pohl.=0, Hypert.=1",
      "Pohl.=1, Hypert.=0",
      "Pohl.=1, Hypert.=1"
    )
  )


prow <- plot_grid(
  log1 + theme(legend.position = "none"),
  log2 + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b <- get_legend(log1 + theme(legend.position = "bottom"))
p1 <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .2))
p1






# Pridanie casovo zavisleho kovariatu - logaritmus casu -------------------------------------


cox.model.tc1 <-
  coxph(
    Surv(time2, death) ~ Diabetes,
    data = data2
  )



cox.model.tc2 <-
  coxph(
    Surv(time2, death) ~  hidiabet + tt( hidiabet),
    data = data2,
    tt = function(x, time2, ...) {
      x * log(time2)
    }
  )


cox.model.tc3 <-
  coxph(
    Surv(time2, death) ~  hidiabet + tt( hidiabet)+Vek+Pohlavie+Hypertenzia,
    data = data2,
    tt = function(x, time2, ...) {
      x * log(time2)
    }
  )



# Pridanie casovo zavisleho kovariatu - cas -------------------------------------


cox.model.tc4 <-
  coxph(
    Surv(time2, death) ~  hidiabet + tt( hidiabet),
    data = data2,
    tt = function(x, time2, ...) {
      x * time2
    }
  )


cox.model.tc5 <-
  coxph(
    Surv(time2, death) ~  hidiabet + tt( hidiabet)+Vek+Pohlavie+Hypertenzia,
    data = data2,
    tt = function(x, time2, ...) {
      x * time2
    }
  )



# Arjasov graf ------------------------------------------------------------

cox.model <- coxph(Surv(time2, death) ~ Diabetes,
                   data = data2)
cox.model


base.surv1 <--log(
  survfit(cox.model, newdata = data.frame(Diabetes=data2$Diabetes))$surv)


base.surv2 <-
  survfit(cox.model, newdata = data.frame(Diabetes=data2$Diabetes))
cum.haz <- base.surv2$cumhaz
cum.time <- base.surv2$time

summary(cum.haz)



 
# Rezíduá -----------------------------------------------------------------


# vykreslenie rezíduí - Cox Snellove rezidua

mart.res <- resid(cox.final, type = "martingale")
cs.res <- data2$death - mart.res
r.surv <-
  survfit(Surv(cs.res, data2$death) ~ 1, type = "fleming-harrington")

df <- data.frame(r.surv$time, r.surv$surv)


cs <- ggplot() +
  geom_step(data = df, mapping = aes(x = r.surv.time, y = -log(r.surv.surv))) +
  geom_abline(intercept = 0,
              slope = 1,
              lty = 2) +
  xlab("Cox-Snellove rezíduá") +
  ylab ("Odhad kum. rizika na zákl. rezíduí") +
  #ggtitle("Nevhodne zvoleného modelu")+
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


# vykreslenie rezíduí - martingalove rezidua

df <- data.frame(x = seq(1:length(mart.res)), mart.res)

mart <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = mart.res)) + geom_abline(
    slope = 0,
    intercept = 0,
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index") +
  ylab ("Martingalové rezíduá") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


# deviaèné rezíduá

dev.res <- resid(cox.final, type = "deviance")
df <- data.frame(x = seq(1:length(dev.res)), dev.res)

dev <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = dev.res)) + geom_abline(
    slope = 0,
    intercept = 0,
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index") +
  ylab ("Deviaèné rezíduá") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


prow <- plot_grid(mart,
                  dev,
                  align = 'h',
                  nrow = 1,
                  ncol = 2)

prow2 <- plot_grid(prow, cs,
                   align = 'h',
                   nrow = 2)
prow2


# Štandardizované delta beta rezíduá

newdata <- data2[order(data2$time2), ]

cox.model.ord <-
  coxph(Surv(time2, death) ~ Diabetes + Pohlavie + Hypertenzia + Vek,
        data = newdata)


# overenie vplyvných pozorovaní

D <- residuals(cox.model.ord, type = 'dfbetas')
#desc_time<-data2$time2[order(D[,1], decreasing = TRUE)]
df <- data.frame(x = seq(1:8283), D)

g1 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X1)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Diabetes") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

g2 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X2)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Hypertenziu") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



g3 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X3)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Vek") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



g4 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X4)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Pohlavie") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


prow <- plot_grid(g1,
                  g2,
                  g3,
                  g4,
                  align = 'h',
                  nrow = 2,
                  ncol = 2)





# Modely zrýchleného èasu -------------------------------------------------


wei.model <-
  survreg(
    Surv(time2, death) ~ Diabetes + Pohlavie + Hypertenzia + Liecba + Vek +
      sysbp,
    data = data2,
    dist = 'weibull'
  )


wei.model2 <-
  survreg(
    Surv(time2, death) ~ Diabetes + Pohlavie + Hypertenzia + Vek + Liecba,
    data = data2,
    dist = 'weibull'
  )

wei.model3 <-
  survreg(
    Surv(time2, death) ~ Diabetes +  Hypertenzia + Vek + Pohlavie,
    data = data2,
    dist = 'weibull'
  )

anova(wei.model, wei.model2)
anova(wei.model2, wei.model3)

m2 <-
  WeibullReg(Surv(time2, death) ~ Diabetes + Pohlavie + Hypertenzia + Vek,
             data = data2)

## Scale parameter
sigma <- wei.model3$scale
## Shape parameter
alpha <- 1 / wei.model3$scale

# graf log-kumulativneho rizika

data2$vek_fix <- 64

data1 <- data2[data2$Diabetes == "1", ]
diab1 <-
  WeibullDiag(
    Surv(time2, death) ~ Diabetes + Pohlavie + Hypertenzia + vek_fix,
    labels = c(
      "Diab=1, Pohl.=0, Hypert.=0",
      "Diab=1, Pohl.=0, Hypert.=1",
      "Diab=1, Pohl.=1, Hypert.=0",
      "Diab=1, Pohl.=1, Hypert.=1"
    ),
    data = data1
  )

df1 <- data.frame(diab1)

data0 <- data2[data2$Diabetes == "0", ]
diab2 <-
  WeibullDiag(
    Surv(time2, death) ~ Diabetes + Pohlavie + Hypertenzia + vek_fix,
    labels = c(
      "Diab=0, Pohl.=0, Hypert.=0",
      "Diab=0, Pohl.=0, Hypert.=1",
      "Diab=0, Pohl.=1, Hypert.=0",
      "Diab=0, Pohl.=1, Hypert.=1"
    ),
    data = data0
  )
df0 <- data.frame(diab2)


log1 <- ggplot(df1, aes(x = x)) +
  geom_step(aes(y = y, group = strata)) +
  ylab("Log-kumulativne riziko") + xlab("Logaritmus casu") + aes(col = strata) +
  theme_classic() + ggtitle("S diabetom") +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) + scale_colour_manual(
    "",
    values = c("blue", "red", "green", "orange"),
    breaks = c(
      "Diabetes=1, Pohlavie=0, Hypertenzia=0, vek_fix=64",
      "Diabetes=1, Pohlavie=0, Hypertenzia=1, vek_fix=64",
      "Diabetes=1, Pohlavie=1, Hypertenzia=0, vek_fix=64",
      "Diabetes=1, Pohlavie=1, Hypertenzia=1, vek_fix=64"
    ),
    labels = c(
      "Pohl.=0, Hypert.=0",
      "Pohl.=0, Hypert.=1",
      "Pohl.=1, Hypert.=0",
      "Pohl.=1, Hypert.=1"
    )
  )


log2 <- ggplot(df0, aes(x = x)) +
  geom_step(aes(y = y, group = strata)) +
  ylab("Log-kumulativne riziko") + xlab("Logaritmus casu") + aes(col = strata) +
  theme_classic() + ggtitle("Bez diabetu") +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) + scale_colour_manual(
    "",
    values = c("blue", "red", "green", "orange"),
    breaks = c(
      "Diabetes=0, Pohlavie=0, Hypertenzia=0, vek_fix=64",
      "Diabetes=0, Pohlavie=0, Hypertenzia=1, vek_fix=64",
      "Diabetes=0, Pohlavie=1, Hypertenzia=0, vek_fix=64",
      "Diabetes=0, Pohlavie=1, Hypertenzia=1, vek_fix=64"
    ),
    labels = c(
      "Pohl.=0, Hypert.=0",
      "Pohl.=0, Hypert.=1",
      "Pohl.=1, Hypert.=0",
      "Pohl.=1, Hypert.=1"
    )
  )


prow <- plot_grid(
  log1 + theme(legend.position = "none"),
  log2 + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)

legend_b <- get_legend(log1 + theme(legend.position = "bottom"))
p1 <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .2))
p1



diab3 <-
  WeibullDiag(Surv(time2, death) ~ Diabetes + Pohlavie + Hypertenzia + vek_fix,
              data = data2)

df3 <- data.frame(diab3)

ggplot(df3, aes(x = x)) +
  geom_step(aes(y = y, group = strata)) +
  ylab("Log-kumulativne riziko") + xlab("Logaritmus casu") + aes(col = strata) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) + scale_colour_manual(
    "",
    values = c(
      "blue",
      "red",
      "green",
      "orange",
      "purple",
      "yellow",
      "pink",
      "lightblue"
    ),
    breaks = c(
      "Diabetes=0, Pohlavie=0, Hypertenzia=0, vek_fix=64",
      "Diabetes=0, Pohlavie=0, Hypertenzia=1, vek_fix=64",
      "Diabetes=0, Pohlavie=1, Hypertenzia=0, vek_fix=64",
      "Diabetes=0, Pohlavie=1, Hypertenzia=1, vek_fix=64",
      "Diabetes=1, Pohlavie=0, Hypertenzia=0, vek_fix=64",
      "Diabetes=1, Pohlavie=0, Hypertenzia=1, vek_fix=64",
      "Diabetes=1, Pohlavie=1, Hypertenzia=0, vek_fix=64",
      "Diabetes=1, Pohlavie=1, Hypertenzia=1, vek_fix=64"
    ),
    labels = c(
      "Diab.= 0, Pohl.=0, Hypert.=0",
      "Diab.= 0, Pohl.=0, Hypert.=1",
      "Diab.= 0, Pohl.=1, Hypert.=0",
      "Diab.= 0, Pohl.=1, Hypert.=1",
      "Diab.= 1, Pohl.=0, Hypert.=0",
      "Diab.= 1, Pohl.=0, Hypert.=1",
      "Diab.= 1, Pohl.=1, Hypert.=0",
      "Diab.= 1, Pohl.=1, Hypert.=1"
    )
  )



# standard. rezidua
stand.res <- residuals(wei.model3, type = "response")

# dev. rezidua
dev.res <- residuals(wei.model3, type = "deviance")


df <- data.frame(x = seq(1,  8283), stand.res, dev.res)

cs1 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = stand.res)) +
  geom_abline(intercept = 0,
              slope = 0,
              lty = 2) +
  xlab("Index") +
  ylab ("Štandardné rezíduá") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

cs2 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = dev.res)) +
  geom_abline(intercept = 0,
              slope = 0,
              lty = 2) +
  xlab("Index") +
  ylab ("Deviaèné rezíduá") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



prow <- plot_grid(
  cs1 + theme(legend.position = "none"),
  cs2 + theme(legend.position = "none"),
  align = 'h',
  nrow = 1,
  ncol = 2
)
prow


# Štandardizované delta beta rezíduá

newdata <- data2[order(data2$time2), ]

wei.model3 <-
  survreg(
    Surv(time2, death) ~ Diabetes +  Hypertenzia + Vek + Pohlavie,
    data = newdata,
    dist = 'weibull'
  )

df.res <- residuals(wei.model3, type = "dfbetas")



D <- residuals(cox.model.ord, type = 'dfbetas')

df <- data.frame(x = seq(1:8283), df.res)

g1 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X1)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Diabetes") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

g2 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X2)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Hypertenziu") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



g3 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X3)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Vek") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )



g4 <- ggplot() +
  geom_point(data = df, mapping = aes(x = x, y = X4)) + geom_abline(
    slope = 0,
    intercept = (2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  geom_abline(
    slope = 0,
    intercept = (-2 / sqrt(length(df$X1))),
    col = "red",
    lty = 2,
    size = 0.75
  ) +
  xlab("Index pacienta (zor. podåa èasu prež.)") +
  ylab ("Šk. delta-beta rez. pre Pohlavie") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


prow <- plot_grid(g1,
                  g2,
                  g3,
                  g4,
                  align = 'h',
                  nrow = 2,
                  ncol = 2)
prow
