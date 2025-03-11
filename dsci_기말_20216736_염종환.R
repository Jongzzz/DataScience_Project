# 모듈 다운
install.packages("tidyverse")
install.packages('dplyr')
install.packages("ggplot2")
install.packages("corrr")
install.packages("corrplot")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrr)
library(corrplot)

############# 데이터 가공 ########################################

dat <- read.csv("dat.csv", encoding="EUC-KR", header=TRUE, sep = ",")
head(dat)

## 필요한 데이터만 select로 가공 즉, 불러옴
dat <- dat %>% select(
  pulse, respiration,
  systolic.blood.pressure,
  diastolic.blood.pressure,
  map, weight, height, BMI,
  creatinine.enzymatic.method, 
  cystatin, white.blood.cell,
  monocyte.count, red.blood.cell,
  hematocrit, lymphocyte.count, 
  basophil.count, eosinophil.count,
  hemoglobin, platelet, neutrophil.count, 
  D.dimer, fibrinogen, myoglobin, calcium,
  potassium, chloride, sodium, creatine.kinase,
  lactate.dehydrogenase, nucleotidase,
  fucosidase, albumin, globulin, cholesterol, 
  triglyceride, pH, methemoglobin, 
  hematocrit.blood.gas, potassium.ion, chloride.ion,
  sodium.ion, glucose.blood.gas, carboxyhemoglobin, 
  oxygen.saturation, partial.oxygen.pressure, oxyhemoglobin
)

# 이상값 처리 이전에 상관관계가 있는 특성을 알아내기 위해 corrplot을 사용한다. 
cor(dat) %>% round(1)
corrplot(cor(dat), method = "circle")
dat %>% correlate() %>% network_plot(min_cor = 0, colours = c("red", "black", "blue"))
dat[, 1:10] %>% plot()

######################## 최종 사용 데이터 추출 ################################
dat <- dat %>% select(
  creatinine.enzymatic.method, cystatin,
  hematocrit, hemoglobin,
  D.dimer, myoglobin,
  chloride, sodium
)

names(dat)
summary(dat)
boxplot(dat)

# 최종 데이터 특성 중 이상값 처리 필요성 유무 확인

# 연관성을 확인한 후, 
# 2개의 특성이 같은 질환의 진단 및 예측을 위한 수치임이 확인되면 이상값을 처리한다.

# 신장 기능 평가
# creatinine.enzymatic.method (크레아티닌) & cystatin (시스타닌)

# 빈혈 등의 혈액 질환
# hematocrit (적혈구 용적율) & hemoglobin (적혈구 산소 운반 단백질)

# 혈전성 질환, 근육 손상
# D-dimer (혈액 응고 분해 산물) & myoglobin (근육 산소 운반 단백질)

# 체내 전해질 균형 파악
# chloride (클로라이드) & sodium (나트륨)


############# 전처리 과정 ########################################

# 이상값 존재 유무 확인
table(is.na(dat))
dat <- na.omit(dat, na.rm=TRUE)
table(is.na(dat))

summary(dat)
boxplot(dat)
corrplot(cor(dat), method = "circle")

cor(dat) %>% round(2)

###################  creatinine.enzymatic.method 이상값 처리##############################
iqr1 <- summary(dat$creatinine.enzymatic.method)[5] - summary(dat$creatinine.enzymatic.method)[2]
dat_upiqr1 <- summary(dat$creatinine.enzymatic.method)[5] + iqr1 * 1.5 
dat_lowiqr1 <- summary(dat$creatinine.enzymatic.method)[2] - iqr1 * 1.5

dat$creatinine.enzymatic.method <- ifelse((dat$creatinine.enzymatic.method > dat_upiqr1 | dat$creatinine.enzymatic.method < dat_lowiqr1), NA, dat$creatinine.enzymatic.method)
dat <- na.omit(dat)
table(is.na(dat$creatinine.enzymatic.method))
boxplot(dat$creatinine.enzymatic.method)

###################  cystatin 이상값 처리##############################
iqr2 <- summary(dat$cystatin)[5] - summary(dat$cystatin)[2]
dat_upiqr2 <- summary(dat$cystatin)[5] + iqr2 * 1.5 
dat_lowiqr2 <- summary(dat$cystatin)[2] - iqr2 * 1.5

dat$cystatin <- ifelse((dat$cystatin > dat_upiqr2 | dat$cystatin < dat_lowiqr2), NA, dat$cystatin)
dat <- na.omit(dat)
table(is.na(dat$cystatin))
boxplot(dat$cystatin)

###################  hematocrit 이상값 처리##############################
iqr3 <- summary(dat$hematocrit)[5] - summary(dat$hematocrit)[2]
dat_upiqr3 <- summary(dat$hematocrit)[5] + iqr3 * 1.5 
dat_lowiqr3 <- summary(dat$hematocrit)[2] - iqr3 * 1.5

dat$hematocrit <- ifelse((dat$hematocrit > dat_upiqr3 | dat$hematocrit < dat_lowiqr3), NA, dat$hematocrit)
dat <- na.omit(dat)
table(is.na(dat$hematocrit))
boxplot(dat$hematocrit)

###################  hemoglobin 이상값 처리##############################
iqr4 <- summary(dat$hemoglobin)[5] - summary(dat$hemoglobin)[2]
dat_upiqr4 <- summary(dat$hemoglobin)[5] + iqr4 * 1.5 
dat_lowiqr4 <- summary(dat$hemoglobin)[2] - iqr4 * 1.5

dat$hemoglobin <- ifelse((dat$hemoglobin > dat_upiqr4 | dat$hemoglobin < dat_lowiqr4), NA, dat$hemoglobin)
dat <- na.omit(dat)
table(is.na(dat$hemoglobin))
boxplot(dat$hemoglobin)

###################  D.dimer 이상값 처리##############################
iqr5 <- summary(dat$D.dimer)[5] - summary(dat$D.dimer)[2]
dat_upiqr5 <- summary(dat$D.dimer)[5] + iqr5 * 1.5 
dat_lowiqr5 <- summary(dat$D.dimer)[2] - iqr5 * 1.5

dat$D.dimer <- ifelse((dat$D.dimer > dat_upiqr5 | dat$D.dimer < dat_lowiqr5), NA, dat$D.dimer)
dat <- na.omit(dat)
table(is.na(dat$D.dimer))
boxplot(dat$D.dimer)

###################  myoglobin 이상값 처리##############################
iqr6 <- summary(dat$myoglobin)[5] - summary(dat$myoglobin)[2]
dat_upiqr6 <- summary(dat$myoglobin)[5] + iqr6 * 1.5 
dat_lowiqr6 <- summary(dat$myoglobin)[2] - iqr6 * 1.5

dat$myoglobin <- ifelse((dat$myoglobin > dat_upiqr6 | dat$myoglobin < dat_lowiqr6), NA, dat$myoglobin)
dat <- na.omit(dat)
table(is.na(dat$myoglobin))
boxplot(dat$myoglobin)

###################  chloride 이상값 처리##############################
iqr7 <- summary(dat$chloride)[5] - summary(dat$chloride)[2]
dat_upiqr7 <- summary(dat$chloride)[5] + iqr7 * 1.5 
dat_lowiqr7 <- summary(dat$chloride)[2] - iqr7 * 1.5

dat$chloride <- ifelse((dat$chloride > dat_upiqr7 | dat$chloride < dat_lowiqr7), NA, dat$chloride)
dat <- na.omit(dat)
table(is.na(dat$chloride))
boxplot(dat$chloride)

###################  sodium 이상값 처리##############################
iqr8 <- summary(dat$sodium)[5] - summary(dat$sodium)[2]
dat_upiqr8 <- summary(dat$sodium)[5] + iqr8 * 1.5 
dat_lowiqr8 <- summary(dat$sodium)[2] - iqr8 * 1.5

dat$sodium <- ifelse((dat$sodium > dat_upiqr8 | dat$sodium < dat_lowiqr8), NA, dat$sodium)
dat <- na.omit(dat)
table(is.na(dat$sodium))
boxplot(dat$sodium)

# 최종 이상값 처리 확인
summary(dat)
boxplot(dat)

#########################상관관계 확인#################################

# creatinine.enzymatic.method 와 cystatin 상관관계 값
with(dat, cor(x=creatinine.enzymatic.method, y=cystatin))

# hematocrit 과 hemoglobin 상관관계 값
with(dat, cor(x=hematocrit, y=hemoglobin))

# D.dimer 와 myoglobin 상관관계 값
with(dat, cor(x=D.dimer, y=myoglobin))

# chloride 와 sodium 상관관계 값
with(dat, cor(x=chloride, y=sodium))

# 표준편차가 0인 열 제거
dat <- dat[, sapply(dat, sd, na.rm = TRUE) != 0]
dat %>%
  correlate() %>%
  network_plot(min_cor = 0, colours = c("red", "black", "blue"))

################ 시각화(geom_point 사용) #################################

# creatinine.enzymatic.method 와 cystatin 시각화
ggplot(data=dat, aes(x=creatinine.enzymatic.method, y=cystatin)) + geom_point()

# hematocrit 과 hemoglobin 시각화
ggplot(data=dat, aes(x=hematocrit, y=hemoglobin)) + geom_point()

# D.dimer 와 myoglobin 시각화
ggplot(data=dat, aes(x=D.dimer, y=sodium)) + geom_point()

# chloride 와  sodium 시각화
ggplot(data=dat, aes(x=chloride, y=sodium)) + geom_point()


# D.dimer 와 myoglobin 상관관계 값
with(dat, cor(x=creatinine.enzymatic.method, y=myoglobin))


################ 귀무가설, 대립가설, 유의수준 ########################

## 가설1: 심부전 환자의 creatinine (umol/L), cystatin(mg/L) 의 농도의 상관관계를 통해 신장 기능을 진단할 수 있다.

# 귀무가설: 심부전 환자의 혈중 크레아티닌 (umol/L) 농도와 시스타틴 (mg/L)  농도 사이에 상관관계가 없다.
# 대립가설: 심부전 환자의 혈중 크레아티닌 (umol/L) 농도와 시스타틴 (mg/L)  농도 사이에 상관관계가 존재한다.
# 유의수준(α): 0.05

## 가설2: 심부전 환자의 hematocrit (%), hemoglobin (g/L) 의 상관관계를 통해 빈혈을 진단할 수 있다.


# 귀무가설: 심부전 환자의 혈중 hematocrit (%)와 hemoglobin(g/L) 농도 사이에 상관관계가 없다.
# 대립가설: 심부전 환자의 혈중 hematocrit (%)와 hemoglobin(g/L) 농도 사이에 상관관계가 존재한다.
# 유의수준(α): 0.05


## 가설3: 심부전 환자의 D.dimer (mg/L), Myoglobin (ng/mL) 의 농도의 상관관계를 통해 혈전성 질환을 진단할 수 있다.

# 귀무가설: 심부전 환자의 혈중 D.dimer (mg/L) 와 Myoglobin (ng/mL) 농도 사이에 상관관계가 없다.
# 대립가설: 심부전 환자의 혈중 D.dimer (mg/L) 와 Myoglobin (ng/mL) 농도 사이에 상관관계가 존재한다.
# 유의수준(α): 0.05


## 가설4: 심부전 환자의 chloride (mmol/L), sodium (mmol/L) 의 상관관계를 통해 체내 전해질 균형 여부를 진단할 수 있다.

#귀무가설: 심부전 환자의 혈중 chloride (mmol/L) 농도와 sodium (mmol/L) 농도 사이에 상관관계가 없다.
#대립가설: 심부전 환자의 혈중 chloride (mmol/L) 농도와 sodium (mmol/L) 농도 사이에 상관관계가 존재한다.
# 유의수준(α): 0.05



######################## 회귀식 모델 산출 및 시각화 ########################

## creatinine.enzymatic.method 와 cystatin 모델
dat_model1 = lm(creatinine.enzymatic.method ~ cystatin, data=dat)
#회귀식 모델 및 검정통계량 확인
summary(dat_model1)

## hematocrit 과 hemoglobin 모델
dat_model2 = lm(hematocrit ~ hemoglobin, data=dat)
#회귀식 모델 및 검정통계량 확인
summary(dat_model2)

## D.dimer 와 myoglobin 모델
dat_model3 = lm(D.dimer ~ myoglobin, data=dat)
#회귀식 모델 및 검정통계량 확인
summary(dat_model3)

## chloride 과 sodium 모델
dat_model4 = lm(chloride ~ sodium, data=dat)
#회귀식 모델 및 검정통계량 확인
summary(dat_model4)



################# 대립가설 채택 ######################
#  model1 : creatinine.enzymatic.method = cystatin * 34.113 + 28.387    
#  Adjusted R-squared: 0.2591

#  model2 : hematocrit = hemoglobin * 2.662e-03 + 4.654e-02      
#  Adjusted R-squared: 0.9632  

#  model3 : D.dimer = myoglobin * 0.001364 + 0.876659      
#  Adjusted R-squared: -0.002145  

#  model4 : chloride = sodium * 1.02956 - 40.78823       
#  Adjusted R-squared: 0.5587  




############## 결과, 결론 ############################

## creatinine.enzymatic.method = cystatin * 34.113 + 28.387 함수로 표현될 수 있다. 
## hematocrit = hemoglobin * 2.662e-03 + 4.654e-02 함수로 표현될 수 있다. 
## D.dimer = myoglobin * 0.001364 + 0.876659 함수로 표현될 수 있다.
## chloride = sodium * 1.02956 - 40.78823 함수로 표현될 수 있다. 


############# 추정(예측) #############################

#### cystatin, creatinine.enzymatic.method 예측 ###############

# 입력값 설정을 위한 summary
summary(dat$cystatin)
summary(dat$creatinine.enzymatic.method)

# cystatin 이 2(mg/L)(비정상 수치) 일 때 creatinine.enzymatic.method 값은 96.6(umol/L)(정상 수치)
dat1 = data.frame(cystatin=c(2)) 
pred1 = predict(dat_model1, dat1)

# cystatin, creatinine.enzymatic.method 예측값 시각화
p1 <- ggplot(data = dat, aes(x = creatinine.enzymatic.method, y = cystatin)) +
  geom_point() + geom_point(data = dat1, aes(x = pred1, y = cystatin), color = "red", size = 5)
p1


 
#### hemoglobin, hematocrit 예측 ###############

# 입력값 설정을 위한 summary
summary(dat$hemoglobin)
summary(dat$hematocrit)

# hemoglobin 이 180(g/L)(비정상 수치)일때 hematocrit 값은 0.526(%)(비정상 수치)
dat2 = data.frame(hemoglobin=c(180))
pred2 = predict(dat_model2, dat2)

# hemoglobin, hematocrit 예측값 시각화
p2 <- ggplot(data = dat, aes(x = hematocrit, y = hemoglobin)) +
  geom_point() + geom_point(data = dat2, aes(x = pred2, y = hemoglobin), color = "red", size = 5)
p2



#### myoglobin, D.dimer 예측 ###############

# 입력값 설정을 위한 summary
summary(dat$myoglobin)
summary(dat$D.dimer)

# myoglobin 이 80(ng/mL)(비정상 수치)일때 D.dimer 값은 0.986(mg/L)(비정상 수치)
dat3 = data.frame(myoglobin=c(80))
pred3 = predict(dat_model3, dat3)

# myoglobin, D.dimer 예측값 시각화
p3 <- ggplot(data = dat, aes(x = D.dimer, y = myoglobin)) +
  geom_point() + geom_point(data = dat3, aes(x = pred3, y = myoglobin), color = "red", size = 5)
p3



#### sodium, chloride 예측 ###############

# 입력값 설정을 위한 summary
summary(dat$sodium)
summary(dat$chloride)

# sodium 160(mmol/L)(비정상 수치)일때 chloride 은 124(mmol/L)(비정상 수치)
dat4 = data.frame(sodium=c(160))
pred4 = predict(dat_model4, dat4)

# sodium, chloride 예측값 시각화
p4 <- ggplot(data = dat, aes(x = chloride, y = sodium)) +
  geom_point() + geom_point(data = dat4, aes(x = pred4, y = sodium), color = "red", size = 5)
p4

