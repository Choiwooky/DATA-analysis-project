subset(usa,  )
a <- as.character(usa$DATE)
a
usa$DATE <- as.character(usa$DATE)
y <-substr(usa$DATE,1,4)
uy <- unique(y)

m <- NULL
for(i in 1:length(uy)){
  a <- mean(subset(usa, substr(DATE, 1, 4) == uy[i])$FEDFUNDS)
  m <- c(m, a)
}

m

usa <- data.frame(uy, m)
View(usa)

#######################
# 1. 시계열 데이터 호출 및 시계열 그림을 통한 개형 파악
tusa <- ts(usa$m)

plot(tusa)
abline(h = mean(tusa), col = "blue")

# 2. 단위근 검정
# (1) ADF test
library(tseries)
adf.test(tusa)

# 결과 해석
# H0 : 단위근이 있는 비정상 시계열 vs H1 : 단위근이 없는 정상 시계열
# p-value = 0.5721이므로 0.05 보다 훨씬 큼을 확인 할 수 있다.
# 이에 따라, H0를 기각할 수 없으므로 H0를 채택한다.
# 따라서, 단위근이 있는 비정상 시계열이라고 판단할 수 있다.
# KPSS test를 돌려 조금 더 확인해보자.

# (2) KPSS test
kpss.test(tusa)

# 결과 해석
# H0 : level stationarity vs H1 : nonstationarity
# p-value = 0.04138이므로 H0를 기각할 수 있다.
# 이에, 비정상 시계열이라고 판단할 수 있다.

# 결론
# 단위근이 존재하므로 비정상 시계열이다.
# 그림을 보아 Random Walk와 유사하게 보인다.
# 이에 따라, 차분을 1번 진행해주자.

# 3. 분산 안정화 변환
x <- 1:length(tusa)
bc.tusa <- boxcox(tusa ~ x)

lam <- bc.tusa$x[which.max(bc.tusa$y)]
lam

# lambda = 0.4242424가 출력되었다.
# 이제, 분산 안정화 변환을 취해주자.
z1 <- ((tusa)^lam - 1)/lam

plot(z1)
abline(h = mean(z1), col = "blue")

# 여전히, 비정상 시계열임을 볼 수 있다.
# 계절성이 살아있는 것으로 보이며, 차분을 1번 진행해준다.

# 4. 차분
z2 <- diff(z1)

plot(z2)
abline(h = mean(z2), col = "blue")

# 다시 단위근 검정을 진행해보자.
adf.test(z2)

# 결과 해석
# p-value = 0.01보다도 작으므로 H0를 기각할 수 있다.
# 따라서, 단위근이 없는 정상 시계열이라고 볼 수 있다.
# KPSS test를 통하여 조금 더 자세히 확인해보자.

kpss.test(z2)

# 결과 해석
# p-value = 0.1보다도 크므로 H0를 기각할 수 없다.
# 따라서, Level Stationarity라고 판단할 수 있다.

# 5. ACF, PACF 판단
op <- par(mfrow = c(1, 2))
acf(z2)
pacf(z2)

# 결과 해석
# ACF는 sin형태로 감소하는 형태이다.
# PACF는 시차 2 이후부터 절단된 형태임을 보인다.
# 이에, 모형은 AR(2)로 설정할 수 있을 것으로 보인다.

# 6. 모형 식별
ar(z2, order.max = 10, method = "yule-walker")
ar(z2, order.max = 10, method = "yule-walker")$aic

# 결과 해석
# AR(2)가 실제로 적합하다는 것을 확인할 수 있다.

# 7. 모수 추정
fit <- arima(z2, order = c(2, 0, 0))
fit

# 8. 모형 진단
par(op)
plot(fit$residuals)
abline(h = mean(fit$residuals), col = "blue")

# 평균 = 0, 분산은 일정하므로 White NOise라고 볼 수 있다.
# tsdiag()를 통하여 자세히 확인해보자.

tsdiag(fit)