install.packages("readxl")
library("readxl")
{
dataset1 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                      sheet = "R1")
dataset2 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R2")
dataset3 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R3")
dataset4 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R4")
dataset5 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R5")
dataset6 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R6")
dataset7 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R7")
dataset8 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R8")
dataset9 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R9")
dataset10 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R10")
dataset11 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R11")
dataset12 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                       sheet = "R12")
dataset13 <- read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                        sheet = "R13")

#Remove missing values
dataset1 <- na.omit(dataset1)
dataset2 <- na.omit(dataset2)
dataset3 <- na.omit(dataset3)
dataset4 <- na.omit(dataset4)
dataset5 <- na.omit(dataset5)
dataset6 <- na.omit(dataset6)
dataset7 <- na.omit(dataset7)
dataset8 <- na.omit(dataset8)
dataset9 <- na.omit(dataset9)
dataset10 <- na.omit(dataset10)
dataset11 <- na.omit(dataset11)
dataset12 <- na.omit(dataset12)
dataset13 <- na.omit(dataset13)
}

#Remove non-numeric entries
#non_numeric_columns <- sapply(data, is.numeric)
#numeric_columns <- which(non_numeric_columns)
#complete_cases <- complete.cases(dataset[, numeric_columns])
#complete_data <- dataset[complete_cases, numeric_columns]
#View(complete_data)

library(lme4)
library(glmnet)
{
mg1<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
              t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
              t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
              (1|year),
           data = dataset1, family = gaussian)
mg2<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset2, family = gaussian)

mg3<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset3, family = gaussian)
mg4<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset4, family = gaussian)
mg5<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset5, family = gaussian)
mg6<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset6, family = gaussian)
mg7<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset7, family = gaussian)
mg8<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset8, family = gaussian)
mg9<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset9, family = gaussian)
mg10<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset10, family = gaussian)
mg11<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset11, family = gaussian)
mg12<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset12, family = gaussian)
mg13<-glmer(VOLUMEN_TOTAL ~t_cm_dom+	t_cms_do+	t_sm_dom+	t_cm_ind+	t_cms_in+
             t_sm_ind+	t_cm_com+	t_cms_co+	t_sm_com+	t_cm_pub+	t_cms_pu+	t_sm_pub+
             t_cm_tot+	t_cms_to+	t_sm_tot+	num_toma+	mont_fac+	mont_rec+	num_tom+
             (1|year),
           data = dataset13, family = gaussian)

}
{p1<-predict(mg1,type="response")
p2<-predict(mg2,type="response")
p3<-predict(mg3,type="response")
p4<-predict(mg4,type="response")
p5<-predict(mg5,type="response")
p6<-predict(mg6,type="response")
p7<-predict(mg7,type="response")
p8<-predict(mg8,type="response")
p9<-predict(mg9,type="response")
p10<-predict(mg10,type="response")
p11<-predict(mg11,type="response")
p12<-predict(mg12,type="response")
p13<-predict(mg13,type="response")
RESPONSE<-data.frame(HR1=p1[2:11],HR2=p2[2:11],HR3=p3[2:11],HR4=p4[2:11],HR5=p5[2:11],
                     HR6=p6[2:11],HR7=p7[2:11],HR8=p8[2:11],HR9=p9[2:11],HR10=p10[2:11],
                     HR11=p11[2:11],HR12=p12[2:11],HR13=p13[2:11])
}


##QQ PLOT
qqnorm(resid(mg1))
qqnorm(resid(mg2))
qqnorm(resid(mg3))
qqnorm(resid(mg4))
qqnorm(resid(mg5))
qqnorm(resid(mg6))
qqnorm(resid(mg7))
qqnorm(resid(mg8))
qqnorm(resid(mg9))
qqnorm(resid(mg10))
qqnorm(resid(mg11))
qqnorm(resid(mg12))
qqnorm(resid(mg13))



library(boot.pval)
summary(mg1, correlation = FALSE)
qqnorm(mg1)

mixed_mod <- coef(mg1)$item
mixed_mod$item <- row.names(mixed_mod)
ggplot(mixed_mod, aes(`(Intercept)`, item)) +
  geom_point() +
  xlab("Random intercept")
library(ggeffects) 


summary(m4) # depends on previous result, there is no p-value to select the sig variables....

fixef(mg1)
ranef(mg1)

install.packages("broom.mixed")
library(broom.mixed)
library(lmerTest)
tidy(mg1)


#residual plots:
library(ggplot2)
fm4 <- fortify.merMod(mg1)

# Plot residuals:
ggplot(fm4, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") + ylab("Residuals")

# Compare the residuals of different subjects:
ggplot(fm4, aes(year, .resid)) +
  geom_boxplot() +
  coord_flip() +
  ylab("Residuals")

# Observed values versus fitted values:
ggplot(fm4, aes(.fitted, Reaction)) +
  geom_point(colour = "blue") +
  facet_wrap(~ Subject, nrow = 3) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Fitted values") + ylab("Observed values")

## Q-Q plot of residuals:
ggplot(mg1, aes(sample = .resid)) +
  geom_qq() + geom_qq_line()

## Q-Q plot of random effects:
ggplot(ranef(m4)$Subject, aes(sample = `(Intercept)`)) +
  geom_qq() + geom_qq_line()
ggplot(ranef(m4)$Subject, aes(sample = `year`)) +
  geom_qq() + geom_qq_line()
