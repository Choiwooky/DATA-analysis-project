### Japan Analysis ###

library(rvest)
library(dplyr)
library(stringr)

# 출저 : https://tochidai.info/tokyo/setagaya/
url <- "https://tochidai.info/tokyo/setagaya/"

tokyo <- read_html(url, encoding = 'UTF-8')

temp <- tokyo %>% html_node("#past-land-price") %>% html_nodes(".year") %>% html_text()
temp1 <- tokyo %>% html_node("#past-land-price") %>% html_nodes(".land-price") %>% html_text()
temp2 <- tokyo %>% html_node("#past-land-price") %>% html_nodes(".tsubo-price") %>% html_text()
temp3 <- tokyo %>% html_node("#past-land-price") %>% html_nodes(".change") %>% html_text()

raw.data <- bind_cols(list(temp,temp1,temp2,temp3))
View(raw.data)

# t1 : 전체 평균 (공시지가 기준 땅값의 총 평균)
t1 <- raw.data[2:37,]
# t2 : 공시지가 평균
t2 <- raw.data[39:75,]
# t3 : 기준 땅값 평균
t3 <- raw.data[77:112,]

names(t1) <- c("year", "m^2_pr", "3.3m^2_pr", "yony")
names(t2) <- c("year", "m^2_pr", "3.3m^2_pr", "yony")
names(t3) <- c("year", "m^2_pr", "3.3m^2_pr", "yony")

#####################################################################
## 분석 대상 : t3

for(i in 1:nrow(t3)) {
  t3[i,2] <- str_replace_all(t3[i,2],'万','')
}
for(i in 1:nrow(t3)) {
  t3[i,2] <- str_replace_all(t3[i,2],'円/m2','')
}
for(i in 1:nrow(t3)) {
  t3[i,3] <- str_replace_all(t3[i,3],'万','')
}
for(i in 1:nrow(t3)) {
  t3[i,3] <- str_replace_all(t3[i,3],'円/坪','')
}
for (i in 1:nrow(t3)){
  t3[i,1] <- str_sub(t3[i,1],1,4)
}

t3$year <- as.numeric(t3$year)
t3$`m^2_pr` <- as.numeric(t3$`m^2_pr`)
t3$`3.3m^2_pr` <- as.numeric(t3$`3.3m^2_pr`)
View(t3)

###########################################################

# 1. 시계열 데이터 호출 및 시계열 그림을 통한 개형 파악
t3.ts <- t3[, 2]
t3.ts <- ts(t3.ts)

plot(t3.ts)
abline(h = mean(t3.ts), col = "blue")

# 2. 단위근 검정
# (1) ADF test
library(tseries)
adf.test(t3.ts)

# 결과 해석
# H0 : 단위근이 있는 비정상 시계열 vs H1 : 단위근이 없는 정상 시계열
# p-value = 0.04992이므로 0.05로 볼 수 있다.
# 이에 따라, H0를 기각할 수 있게 된다.
# 이에, 단위근이 없는 정상 시계열이라고 볼 수 있다.
# KPSS test를 돌려 조금 더 확인해보자.

# (2) KPSS test
kpss.test(t3.ts)

# 결과 해석
# H0 : level stationarity vs H1 : nonstationarity
# p-value = 0.07619이므로 H0를 기각할 수 있다.
# 이에, 비정상 시계열이라고 볼 수 있다.

# 3. ACF, PACF 판단
op <- par(mfrow = c(1, 2))
acf(t3.ts)
pacf(t3.ts)

# 결과 해석
# ACF는 지수적으로 감소하는 형태이다.
# PACF는 시차 2 이후부터 절단된 형태임을 보인다.
# 이에, 모형은 AR(2)로 설정할 수 있을 것으로 보인다.

# 4. 모형 식별
ar(t3.ts, order.max = 10, method = "yule-walker")
ar(t3.ts, order.max = 10, method = "yule-walker")$aic

# 결과 해석
# ar() 함수에서 aic = TRUE로 설정하였으므로 AIC가 가장 작은 모수까지 추정을 해준다.
# AR(10) model까지 고려하였으나, 2까지만 모수를 추정하였으므로 AR(2) 모형이라고 식별할 수 있다.

# 5. 모형 적합
fit <- arima(t3.ts, order = c(2, 0, 0)) # method : CSS-ML
fit

# 결과 해석
# Model : (Z(t) - 810418.2) = 1.2995(Z(t-1) - 810418.2) - (-0.4542)(Z(t-2) - 810418.2) + a(t)

# 6. 모형 진단
par(op)
plot(fit$residuals)
abline(h = mean(fit$residuals), col = "blue")

# 결과 해석
# 평균 = 0이며, 분산이 일정한 것을 보아 White Noise라고 판단할 수 있다.
# tsdiag()를 이용하여 조금 더 자세히 확인해보자.

tsdiag(fit)

### 최종 모형 : AR(2)