library(faraway)
library(tidyverse)
library(skimr)
#devtools::install_github("boxuancui/DataExplorer")
library(DataExplorer)
library(scales)
library(corrr)
library(readxl)
# Modelado
# ==============================================================================
library(glmnet)
library(pls)
#Datos 
datx<-read_excel("LAST SEMESTER/Proyecto final/MUNICIPIO 2019.xlsx",sheet = "R1")
View(datx)
skim(datx)
datax<-datx[1:11,6:186]
skim(datax)
View(datax)

set.seed(1235)
id_train <- sample(1:nrow(datax), size = 0.7*nrow(datax), replace = FALSE)

datos_train <- datax[id_train, ]
datos_test  <- datax[-id_train, ]
modelo <- lm(A187 ~ ., data = datos_train)
summary(modelo)

df_coeficientes <- modelo$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))
predicciones_train <- predict(modelo, newdata = datos_train)


########REGION IV
dataVI<-read_excel("LAST SEMESTER/Proyecto final/BASE DE DATOS.xlsx",sheet = "R6")
datavi<-dataVI[2:145,6:188]
View(datavi)
#CORRELATION ANALYSIS
rom<-read_excel("C:/Users/pacor/OneDrive/Escritorio/DATA.xlsx", 
                sheet = "R6")
rom<-na.omit(rom)

numeric_datavi <- lapply(rom, as.numeric)
pairs <- lapply(numeric_datavi, function(x) sapply(numeric_datavi, FUN = function(y) cor(x, y, method = "pearson")))
df_correlaciones <- as.data.frame(pairs)
write.xlsx(df_correlaciones,"C:/Users/pacor/OneDrive/Escritorio/corel.xlsx")
read_excel("C:/Users/pacor/OneDrive/Escritorio/corel.xlsx")
##LASSO RIDGE ELASTIC 
tc<-trainControl(method = "cv",number=10,
                 savePredictions = "all")
#VECTOR OF POTENTIAL LAMBDA VALUES
lmbda_vector<-10^seq(5,-5,length=100)
#ELIMMINSATION FO VARIABLES###
library(caret)
datax <- subset(datax, select = -c(A181))
m1 <-train(VOLUMEN_TOTAL ~ .,
            na.action=na.omit,
            data = rom,
            tuneGrid = expand.grid(alpha = 1,lambda = lmbda_vector),
            method = "glmnet",
            metric="RMSE",
            trControl = tc)


m2 <- train(VOLUMEN_TOTAL~ .,
            na.action=na.omit,
            data = rom,
            tuneGrid = expand.grid(alpha = seq(-1, 1, 0.1),lambda = lmbda_vector),
            method = "glmnet",
            metric="RMSE",
            trControl = tc)
m3 <- train(VOLUMEN_TOTAL ~ .,
            na.action=na.omit,
            data =  rom,
            tuneGrid = expand.grid(alpha = 0,lambda = lmbda_vector),
            method = "glmnet",
            metric="RMSE",
            trControl = tc)
coef(m1$finalModel,m1$finalModel$lambdaOpt)
varImp(m1)
plot(varImp(m1))
coef(m2$finalModel,m2$finalModel$lambdaOpt)
varImp(m2)
plot(varImp(m2))
coef(m3$finalModel,m3$finalModel$lambdaOpt)
varImp(m3)
plot(varImp(m3))



##########################################33
data<-read_excel("LAST SEMESTER/Proyecto final/BASE DE DATOS.xlsx", 
                 sheet = "RR6")

set.seed(1235)
id_train <- sample(1:nrow(data), size = 0.7*nrow(data), replace = FALSE)

datos_train <- data[id_train, ]
datos_test  <- data[-id_train, ]


x_train <- model.matrix(R~., data = datos_train)[, -1]
y_train <- datos_train$R

x_test <- model.matrix(R~., data = datos_test)[, -1]
y_test <- datos_test$R



# PROVE REGULARIZATION REGRESSION
##EEK FOLD CROSS VALIDATION
ctrspec<-trainControl(method = "cv",number=10,
                      savePredictions = "all")
#VECTOR OF POTENTIAL LAMBDA VALUES
lmbda_vector<-10^seq(5,-5,length=100)
library(glmnet)
library(caret)
# Fit lasso regression:
mlasso <- train(R~.,
                data=datos_train,
                method = "glmnet", 
                tuneGrid = expand.grid(alpha = 1,lambda = lmbda_vector),
                trControl = ctrspec,na.action=na.omit) 
melastic<-train(R~ .,
                data = datos_train,
                method = "glmnet", 
                tuneGrid = expand.grid(alpha = seq(0.1, 1, 0.1),lambda = lmbda_vector),
                trControl = ctrspec,
                na.action=na.omit) 
mridge<-train(R~ .,
              data = datos_train,
              method = "glmnet", 
              tuneGrid = expand.grid(alpha = 0,lambda = lmbda_vector),
              trControl = ctrspec,
              na.action=na.omit) 
coef(mlasso$finalModel,mlasso$finalModel$lambdaOpt)
varImp(mlasso)
plot(varImp(mlasso))

coef(mridge$finalModel,mridge$finalModel$lambdaOpt)
varImp(mridge)
plot(varImp(mridge))

coef(melastic$finalModel,melastic$finalModel$lambdaOpt)
varImp(melastic)
plot(varImp(melastic))

datos_train <- subset(datos_train, select = -c(A174))



#Regularised mixed model REGION VI

library(lme4)
install.packages("glmmLasso")
library(glmmLasso)
# Load your data
datar6 <- read_excel("LAST SEMESTER/Proyecto final/DATA_Agua_potable_y_saneamiento_2021_FINAL.xlsx", 
                      sheet = "R6")

# Define the fixed effects, 
fixed <- as.formula(R ~A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+A21+
                      A22+A23+A24+A25+A26+A27+A28+A29+A30+A31+A32+A33+A34+A35+A36+A37+
                      A38+A39+A40+A41+A42+A43+A44+A45+A46+A47+A48+A49+A50+A51+A52+A53
                    +A54+A55+A56+A57+A58+A59+A60+A61+A62+A63+A64+A65+A66+A67+A68+A69+A70+
                      A71+A128+A129+A130+A131+A132+A148+A175+A176+A177+A178+A179+A180+A181+A182+
                      A183+A184+A185+A186+A187+A188+A189)

# Define the random effect
random <- as.formula(1 | datar6$A4)

# Fit the regularised mixed model using the lmer function
model <- lmer(fixed + (1 | datar6$A4), data = datar6, control = lmerControl(optimizer = "boptimizer", calc.derivs = TRUE))
model <- glmmLasso(fixed + (1 | datar6$A4), data = datar6, lambda = lambda_value, control = glmmLassoControl(maxIter = 10000))

# Number of effects used in model:
params <- 10
# Set parameters for optimisation:
lambda <- seq(500, 0, by = -5)
BIC_vec <- rep(Inf, length(lambda))
m_list <- list()
Delta_start <- as.matrix(t(rep(0, params + 23)))
Q_start <- 0.1
# Search for optimal lambda:
pbar <- txtProgressBar(min = 0, max = length(lambda), style = 3)
for(j in 1:length(lambda)){
  setTxtProgressBar(pbar, j)
  m <- glmmLasso(R ~A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+A21+
                   A22+A23+A24+A25+A26+A27+A28+A29+A30+A31+A32+A33+A34+A35+A36+A37+
                   A38+A39+A40+A41+A42+A43+A44+A45+A46+A47+A48+A49+A50+A51+A52+A53
                 +A54+A55+A56+A57+A58+A59+A60+A61+A62+A63+A64+A65+A66+A67+A68+A69+A70+
                   A71+A128+A129+A130+A131+A132+A148+A175+A176+A177+A178+A179+A180+A181+A182+
                   A183+A184+A185+A186+A187+A188+A189,
                 rnd = datar6$A4,
                 family =gaussian(link = log),
                 data = datar6,
                 lambda = lambda[j],
                 switch.NR = FALSE,
                 final.re = TRUE)
  BIC_vec[j] <- m$bic
  m_list[[j]] <- m
}
close(pbar)
# Print the optimal model:
opt_m <- m_list[[which.min(BIC_vec)]]
summary(opt_m)



# Get the regularisation parameter
lambda <- coef(model, "lambda")

# Get the summary of the model
summary(model)
m <- glmmLasso(R ~A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+A21+
                 A22+A23+A24+A25+A26+A27+A28+A29+A30+A31+A32+A33+A34+A35+A36+A37+
                 A38+A39+A40+A41+A42+A43+A44+A45+A46+A47+A48+A49+A50+A51+A52+A53
               +A54+A55+A56+A57+A58+A59+A60+A61+A62+A63+A64+A65+A66+A67+A68+A69+A70+
                 A71+A128+A129+A130+A131+A132+A148+A175+A176+A177+A178+A179+A180+A181+A182+
                 A183+A184+A185+A186+A187+A188+A189,
               rnd =list(A4=~1),
               family =gaussian(link = log),
               data = datar6,
               lambda = lambda[j],
               switch.NR = FALSE,
               final.re = TRUE)
