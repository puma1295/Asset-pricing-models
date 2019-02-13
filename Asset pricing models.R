# загружаем библиотеки
library(PerformanceAnalytics)
library(readr)

# выгружаем данные
load("cur.cap.final.RData")  #капитализация
load("cur.pb.final.RData")   # бук ту маркет
load("cur.rets.final.RData") # доходность
load("rf_mr2.RData")         # доходность рынка и риск фри (MSCI)
load("novie_fondi.RData")        # доходность ПИФов (только пифы акций)
load("cur_volume_final.RData") #дневные объемы торгов
load("cur_pxlast_final.RData") #дневные доходности 

 rf_mr1 <- as.data.frame(risk_free_monthly)
 
 novie_fondi <- novie_fondi[,-369]  # убираем дичь
 novie_fondi <- as.xts(novie_fondi[,-97])
 
 novie_fondi_sorted <- sort(apply(novie_fondi, 2, mean)) # отсортируем ср.ар. доходностей по возр.
 ramka <- match(novie_fondi_sorted, apply(novie_fondi, 2, mean)) # посмотрим на их места, сделаем рамку
 
 novie_fondi_names <- t(colnames(novie_fondi)[ramka])
 
 novie_fondi <- novie_fondi[,novie_fondi_names] # отсортируем по средним доходностям
 
 
 # считаем доходность
 returns_newnew <- Return.calculate(cur.rets.final) 
 returns_newnew <- returns_newnew[-c(1:83),]

# выравниваем остальные таблицы
cur.pb.final <- cur.pb.final[-c(1:83),]
cur.cap.final <- cur.cap.final[-c(1:83),]
rf_mr1 <- rf_mr1[-c(1:47),]

SL<-SN<-SH<-BL<-BN<-BH<-NULL # это наши будущие группы small high, small neutral и тд

for (y in 1:120) {
  cur.cap <- cur.cap.final[y,]  # берем строчку капитализации
  cur.cap.sorted <- sort(as.numeric(cur.cap))  # сортируем ее по возрастанию
  cur.cap.sorted.tickers <- colnames(cur.cap)[match(cur.cap.sorted, cur.cap)] # вытаскиваем отсортированные тикеры
  
  
  cur.pb <- cur.pb.final[y,]  # берем строчку бук ту маркет
  cur.pb.sorted <- cur.pb[,cur.cap.sorted.tickers]  # сортируем ее по тикерам капитализации
  # делим на две части и каждую соритруем по возрастанию
  cur.pb.sorted[,1:141]  <- sort(as.numeric(cur.pb.sorted[,1:141]))
  cur.pb.sorted[,142:281] <- sort(as.numeric(cur.pb.sorted[,142:281]))
  cur.pb.sorted.tickers <- colnames(cur.pb)[match(cur.pb.sorted, cur.pb)] # вытаскиваем отсортированные тикеры
  
  
  curr.rets <- returns_newnew[y,] # берем строчку доходностей
  curr.rets.sorted <- curr.rets[, cur.pb.sorted.tickers]  # сортируем ее по тикерам бук ту маркет
  
  
  cur.cap.sorted.final <- cur.cap[,cur.pb.sorted.tickers] # сортируем капитализацию по итоговой маске (для средневзвешанного)
  
  # делим строчку на 6 групп и по каждой находим средневзвешанное
  cur.ret.p1 <- weighted.mean(apply(curr.rets.sorted[,1:46], 2, mean),cur.cap.sorted.final[,1:46])
  SH <- rbind(SH, cur.ret.p1)
  cur.ret.p2 <- weighted.mean(apply(curr.rets.sorted[,47:93], 2, mean),cur.cap.sorted.final[,47:93])
  SN <- rbind(SN, cur.ret.p2)
  cur.ret.p3 <- weighted.mean(apply(curr.rets.sorted[,94:140], 2, mean),cur.cap.sorted.final[,94:140])
  SL <- rbind(SL, cur.ret.p3)
  cur.ret.p4 <- weighted.mean(apply(curr.rets.sorted[,141:187], 2, mean),cur.cap.sorted.final[,141:187])
  BH <- rbind(BH, cur.ret.p4)
  cur.ret.p5 <- weighted.mean(apply(curr.rets.sorted[,188:234], 2, mean),cur.cap.sorted.final[,188:234])
  BN <- rbind(BN, cur.ret.p5)
  cur.ret.p6 <- weighted.mean(apply(curr.rets.sorted[,235:281], 2, mean),cur.cap.sorted.final[,235:281])
  BL <- rbind(BL, cur.ret.p6)
}

kuvshinSMB <- kuvshinHML <- NULL
factors <- cbind(SH,SN,SL,BH,BN,BL)

# считаем фактор SMB как: (SH + SL)/2 - (BH + BL)/2
for (y in 1:nrow(factors))  {
  SMB<-mean(as.numeric(factors[y,1:3]))-mean(as.numeric(factors[y,4:6]))
  kuvshinSMB<-rbind(kuvshinSMB,SMB)
}

# считаем фактор HML как: (SH + BH)/2 - (SL + BL)/2
for (y in 1:nrow(factors))  {
  HML <- mean(as.numeric(factors[y,c(1,4)]))-mean(as.numeric(factors[y,c(3,6)]))
  kuvshinHML <- rbind(kuvshinHML,HML)
}

# посмотрим на графики факторов SMB,HML 
plot(kuvshinSMB)
plot(kuvshinHML)


# выгружаем риск маркет риск фри 
risk.market_and_risk.free <- as.data.frame(rf_mr1)


# находим доходность рынка минус безриск
RMRF.NEW <- t(risk.market_and_risk.free[,1]-risk.market_and_risk.free[,2])

kuvshin.FF <- NULL -> kuvshin.FFR -> kuvshin.stars.FF
# строим регрессию для трехфакторного Фамы-Френча
rmrf <- t(RMRF.NEW)
smb <- as.matrix(kuvshinSMB)
hml <- as.matrix(kuvshinHML) 
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free[,2] 
  kosti <- lm(fund.xcess ~ rmrf + smb + hml)
  kuvshin.FF <- rbind(kuvshin.FF,summary(kosti)$coefficients[,1])
  kuvshin.FFR <- rbind(kuvshin.FFR,summary(kosti)$r.squared)
  kuvshin.stars.FF <- rbind(kuvshin.stars.FF,summary(kosti)$coefficients[,4])
}
# Посмотрим на результаты
#R^2
plot(kuvshin.FFR) 
#alpha
plot(kuvshin.FF[,1])
plot(kuvshin.stars.FF[,1])

#rmrf
plot(kuvshin.FF[,2])
plot(kuvshin.stars.FF[,2])
#SMB
plot(kuvshin.FF[,3])
plot(kuvshin.stars.FF[,3])
#HML
plot(kuvshin.FF[,4])
plot(kuvshin.stars.FF[,4])


### ТЕПЕРЬ ЗАЙМЕМСЯ КАРХАРТОМ ###

# найдем фактор моментум

zzz<-NULL
# считаем доходность (еще раз да, ну так удобнее)
returns_newnew <- Return.calculate(cur.rets.final)
returns_newnew <- returns_newnew[-1,]

for (i in 1:192) {
  momentumrets <- apply(returns_newnew[i:(i+10),], 2, mean)
  zzz<-rbind(zzz,momentumrets) 
} # это мы посчитали среднее арифметическое с лагом в 1 месяц

kuvshinmomentum <- NULL
momentumretssorted <- NULL

# а тут мы каждую строчку сортируем по возрастанию и вычетаем из 
# акций победителей акции проигравших
for (i in 1:nrow(zzz)){
  momentumretssorted <- sort(zzz[i,])
  momentum <- mean (momentumretssorted[141:281]) - mean (momentumretssorted[1:140])
  kuvshinmomentum <- rbind(kuvshinmomentum,momentum)
} 
kuvshinmomentum <- as.matrix(kuvshinmomentum[-c(1:72),])

#посмотрим на график фактора momentum
plot(kuvshinmomentum)

# построим регрессию Кархарта
kuvshin.MOM <-NULL -> kuvshin.MOMR -> kuvshin.stars.MOM

rmrf <- t(as.matrix(RMRF.NEW))
smb <- as.matrix(kuvshinSMB)
hml <- as.matrix(kuvshinHML)
mom <- as.matrix(kuvshinmomentum)


for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + smb + hml + mom)
  kuvshin.MOM<-rbind(kuvshin.MOM,summary(kosti)$coefficients[,1])
  kuvshin.MOMR<-rbind(kuvshin.MOMR,summary(kosti)$r.squared)
  kuvshin.stars.MOM <- rbind(kuvshin.stars.MOM, summary(kosti)$coefficients[,4])
}

# Посмотрим на результаты
#R^2
plot(kuvshin.MOMR) 
#alpha
plot(kuvshin.MOM[,1])
plot(kuvshin.stars.MOM[,1])
#rmrf
plot(kuvshin.MOM[,2])
plot(kuvshin.stars.MOM[,2])
#SMB
plot(kuvshin.MOM[,3])
plot(kuvshin.stars.MOM[,3])
#HML
plot(kuvshin.MOM[,4])
plot(kuvshin.stars.MOM[,4])
#MOM
plot(kuvshin.MOM[,5])
plot(kuvshin.stars.MOM[,5])

### ТЕПЕРЬ ЗАЙМЕМСЯ ПАСТОРОМ-СТАМБАУ ###

#рассчитаем факторы регрессии для ликвидити
rets_daily <- Return.calculate(cur_pxlast_final) # считаем дневные доходности
rets_daily <- rets_daily[-c(1:2893),]
volume <- cur_volume_final[-c(1:2893),]
factorLiq <- sign(rets_daily) * volume

rets_daily<-as.xts(rets_daily)
factorLiq <- as.xts(factorLiq)

#составим вектор дат
datevector <- NULL
for (years in 2008:2017) {
  months <- 1:12
  t <- paste(years, '-', months, sep='')
  datevector <- cbind(datevector,t)
}

kuvshin_notbad <- notbad_tickers <- liq.factor <- temp.liq.factor <- NULL

for (i in 1:120) { # c единицы, потому что данные в капитализации начинаются с января

  t <- datevector[i]  
  temp1 <- rets_daily[t]
  temp2 <- factorLiq[t]
  temp3 <- cur.cap.final[i,] # потому что данные в капитализации начинаются с января
  
  # объединим в одну таблицу доходности, лаггированные доходности и флоус
  for (b in 1:281) {
    df.temp <- cbind(temp1[,b],  lag(temp1[,b]), lag(temp2[,b]))
    
    df.temp[is.infinite(df.temp)] <- 0 # убираем бесконечности
    df.temp[is.na(df.temp)] <- 0  # убираем NA
    
    test.vector <- apply(df.temp, 2, sum) != 0 # сумма по строчке не равна нулю?
    
    colnames(df.temp) <- c('Rets', 'LaggedRets', 'Flows')
    
    
    if(test.vector[1] && test.vector[2] && test.vector[3])  # если не равна, то добавляем в регрессию 
    { 
      model.tmp <- lm(Rets ~ LaggedRets + Flows, data = df.temp)
      temp.liq.factor <- rbind(temp.liq.factor, summary(model.tmp)$coefficients[,4])
      notbad_tickers <- cbind(notbad_tickers,temp3[,b])
    }
    else { 
      temp.liq.factor <- rbind(temp.liq.factor, rep(NA, 3))
      notbad_tickers <- cbind(notbad_tickers,rep(NA, 1))
    } # а если равна, то добавляем строчку NA
  }
  kuvshin_notbad <- rbind(kuvshin_notbad, notbad_tickers)
  # делаем тут что-то важное и убираем NA-шки
  mean.monthly.liq.factor <- weighted.mean(t(temp.liq.factor[,3]), kuvshin_notbad, na.rm = T)
  liq.factor <- rbind(liq.factor, mean.monthly.liq.factor)
  temp.liq.factor <- NULL #важное исправление
  notbad_tickers <- NULL
  kuvshin_notbad <- NULL
} # я не понимаю, что значат ошибки


              # посмотрим на график фактора LIQ
plot(liq.factor)

# строим регрессию для Пастора -Стамбау
kuvshin.LIQUID <- NULL -> kuvshin.LIQUIDR -> kuvshin.stars.LIQUID

liq  <- liq.factor

for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free[,2] 
  kosti <- lm(fund.xcess ~ rmrf + smb + hml + liq)
  kuvshin.LIQUID<-rbind(kuvshin.LIQUID,summary(kosti)$coefficients[,1])
  kuvshin.LIQUIDR<-rbind(kuvshin.LIQUIDR,summary(kosti)$r.squared)
  kuvshin.stars.LIQUID <- rbind(kuvshin.stars.LIQUID,summary(kosti)$coefficients[,4])
}
# посмотрим на результаты
# R^2
plot(kuvshin.LIQUIDR) 
# alpha
plot(kuvshin.LIQUID[,1])
plot(kuvshin.stars.LIQUID[,1])
# rmrf
plot(kuvshin.LIQUID[,2])
plot(kuvshin.stars.LIQUID[,2])
# SMB
plot(kuvshin.LIQUID[,3])
plot(kuvshin.stars.LIQUID[,3])
# HML
plot(kuvshin.LIQUID[,4])
plot(kuvshin.stars.LIQUID[,4])
# liq
plot(kuvshin.LIQUID[,5])
plot(kuvshin.stars.LIQUID[,5])

#ИТОГОВАЯ СМЕШАННАЯ МОДЕЛЬ

kuvshin.MIXED <- NULL -> kuvshin.MIXEDR -> kuvshin.stars.MIXED
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free[,2] 
  kosti <- lm(fund.xcess ~ rmrf + smb + hml + liq + mom)
  kuvshin.MIXED<-rbind(kuvshin.MIXED,summary(kosti)$coefficients[,1])
  kuvshin.MIXEDR<-rbind(kuvshin.MIXEDR,summary(kosti)$r.squared)
  kuvshin.stars.MIXED <- rbind(kuvshin.stars.MIXED,summary(kosti)$coefficients[,4])
}
#R^2
plot(kuvshin.MIXEDR) 
#alpha
plot(kuvshin.MIXED[,1])
plot(kuvshin.stars.MIXED[,1])
#rmrf
plot(kuvshin.MIXED[,2])
plot(kuvshin.stars.MIXED[,2])
#SMB
plot(kuvshin.MIXED[,3])
plot(kuvshin.stars.MIXED[,3])
#HML
plot(kuvshin.MIXED[,4])
plot(kuvshin.stars.MIXED[,4])
#liq
plot(kuvshin.MIXED[,5])
plot(kuvshin.stars.MIXED[,5])
#mom
plot(kuvshin.MIXED[,6])
plot(kuvshin.stars.MIXED[,6])


### А ТЕПЕРЬ СТРОИМ МОДЕЛИ С ХЕНРИКСОНОМ-МЕРТОНОМ ###

# сначала считаем новые факторы

# новый rmrf
HM.rmrf <- rmrf
for (i in 1:nrow(rmrf)) {
  if (HM.rmrf[i,]>0)
  {HM.rmrf[i,] <- HM.rmrf[i,]} 
  else
  {HM.rmrf[i,] <- 0}
}

# новый smb
HM.smb <- smb
for (i in 1:nrow(smb)) {
  if (HM.smb[i,]>0)
  {HM.smb[i,] <- HM.smb[i,]} 
  else
  {HM.smb[i,] <- 0}
}

# новый hml
HM.hml <- hml
for (i in 1:nrow(hml)) {
  if (HM.hml[i,]>0)
  {HM.hml[i,] <- HM.hml[i,]} 
  else
  {HM.hml[i,] <- 0}
}

# новый mom
HM.mom <- mom
for (i in 1:nrow(mom)) {
  if (HM.mom[i,]>0)
  {HM.mom[i,] <- HM.mom[i,]} 
  else
  {HM.mom[i,] <- 0}
}

# новый liq
HM.liq <- liq
for (i in 1:nrow(liq)) {
  if (HM.liq[i,]>0)
  {HM.liq[i,] <- HM.liq[i,]} 
  else
  {HM.liq[i,] <- 0}
}

# теперь строим регрессии

# трехфакторный фама-френч
kuvshin.HMFF <- NULL -> kuvshin.HMFFR -> kuvshin.stars.HMFF
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + HM.rmrf + smb + HM.smb + hml + HM.hml)
  kuvshin.HMFF <- rbind(kuvshin.HMFF,summary(kosti)$coefficients[,1])
  kuvshin.HMFFR <- rbind(kuvshin.HMFFR,summary(kosti)$r.squared)
  kuvshin.stars.HMFF <- rbind(kuvshin.stars.HMFF,summary(kosti)$coefficients[,4])
}
# Посмотрим на результаты
# R^2
summary(kuvshin.HMFFR) 

# звезды
#alpha
plot(kuvshin.stars.HMFF[,2])
#rmrf
plot(kuvshin.stars.HMFF[,4])
#SMB
plot(kuvshin.stars.HMFF[,6])
#HML
plot(kuvshin.stars.HMFF[,8])


# кархарт
kuvshin.HMMOM <-NULL -> kuvshin.HMMOMR -> kuvshin.stars.HMMOM
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + HM.rmrf + smb + HM.smb + hml + HM.hml + mom + HM.mom)
  kuvshin.HMMOM <- rbind(kuvshin.HMMOM,summary(kosti)$coefficients[,1])
  kuvshin.HMMOMR <- rbind(kuvshin.HMMOMR,summary(kosti)$r.squared)
  kuvshin.stars.HMMOM <- rbind(kuvshin.stars.HMMOM,summary(kosti)$coefficients[,4])
}

# Посмотрим на результаты
#R^2
summary(kuvshin.HMMOMR) 

# звезды
#alpha
plot(kuvshin.stars.HMMOM[,2])
#rmrf
plot(kuvshin.stars.HMMOM[,4])
#SMB
plot(kuvshin.stars.HMMOM[,6])
#HML
plot(kuvshin.stars.HMMOM[,8])
#mom
plot(kuvshin.stars.HMMOM[,10])


# пастор-стамбау
kuvshin.HMLIQUID <- NULL -> kuvshin.HMLIQUIDR -> kuvshin.stars.HMLIQUID
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + HM.rmrf + smb + HM.smb + hml + HM.hml + liq + HM.liq)
  kuvshin.HMLIQUID <- rbind(kuvshin.HMLIQUID,summary(kosti)$coefficients[,1])
  kuvshin.HMLIQUIDR <- rbind(kuvshin.HMLIQUIDR,summary(kosti)$r.squared)
  kuvshin.stars.HMLIQUID  <- rbind(kuvshin.stars.HMLIQUID ,summary(kosti)$coefficients[,4])
}

# Посмотрим на результаты
#R^2
summary(kuvshin.HMLIQUIDR) 

# звезды
#alpha
plot(kuvshin.stars.HMLIQUID[,2])
#rmrf
plot(kuvshin.stars.HMLIQUID[,4])
#SMB
plot(kuvshin.stars.HMLIQUID[,6])
#HML
plot(kuvshin.stars.HMLIQUID[,8])
#liq
plot(kuvshin.stars.HMLIQUID[,10])


# смешанная пятифакторная модель
kuvshin.HMMIXED <- NULL -> kuvshin.HMMIXEDR -> kuvshin.stars.HMMIXED
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + HM.rmrf + smb + HM.smb + hml + HM.hml + mom + HM.mom + liq + HM.liq)
  kuvshin.HMMIXED <- rbind(kuvshin.HMMIXED,summary(kosti)$coefficients[,1])
  kuvshin.HMMIXEDR <- rbind(kuvshin.HMMIXEDR,summary(kosti)$r.squared)
  kuvshin.stars.HMMIXED  <- rbind(kuvshin.stars.HMMIXED ,summary(kosti)$coefficients[,4])
}

# Посмотрим на результаты
#R^2
summary(kuvshin.HMMIXEDR)  

# звезды
#alpha
plot(kuvshin.stars.HMMIXED[,2])
#rmrf
plot(kuvshin.stars.HMMIXED[,4])
#SMB
plot(kuvshin.stars.HMMIXED[,6])
#HML
plot(kuvshin.stars.HMMIXED[,8])
#mom
plot(kuvshin.stars.HMMIXED[,10])
#liq
plot(kuvshin.stars.HMMIXED[,12])



### А ТЕПЕРЬ СТРОИМ МОДЕЛИ С ТРЕЙНОРОМ-МАЗУИ ###

# сначала считаем новые факторы

# новый rmrf
TM.rmrf <- rmrf^2

# новый smb
TM.smb <- smb^2

# новый hml
TM.hml <- hml^2

# новый mom
TM.mom <- mom^2

# новый liq
TM.liq <- liq^2

# теперь строим регрессии

# трехфакторный фама-френч
kuvshin.TMFF <- NULL -> kuvshin.TMFFR -> kuvshin.stars.TMFF
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + TM.rmrf + smb + TM.smb + hml + TM.hml)
  kuvshin.TMFF <- rbind(kuvshin.TMFF,summary(kosti)$coefficients[,1])
  kuvshin.TMFFR <- rbind(kuvshin.TMFFR,summary(kosti)$r.squared)
  kuvshin.stars.TMFF <- rbind(kuvshin.stars.TMFF ,summary(kosti)$coefficients[,4])
  
}
# Посмотрим на результаты
#R^2
summary(kuvshin.TMFFR) 

# звезды
#alpha
plot(kuvshin.stars.TMFF[,2])
#rmrf
plot(kuvshin.stars.TMFF[,4])
#SMB
plot(kuvshin.stars.TMFF[,6])
#HML
plot(kuvshin.stars.TMFF[,8])


# кархарт
kuvshin.TMMOM <-NULL -> kuvshin.TMMOMR -> kuvshin.stars.TMMOM
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + TM.rmrf + smb + TM.smb + hml + TM.hml + mom + TM.mom)
  kuvshin.TMMOM <- rbind(kuvshin.TMMOM,summary(kosti)$coefficients[,1])
  kuvshin.TMMOMR <- rbind(kuvshin.TMMOMR,summary(kosti)$r.squared)
  kuvshin.stars.TMMOM <- rbind(kuvshin.stars.TMMOM ,summary(kosti)$coefficients[,4])
}

# Посмотрим на результаты
#R^2
summary(kuvshin.TMMOMR) 

# звезды
#alpha
plot(kuvshin.stars.TMMOM[,2])
#rmrf
plot(kuvshin.stars.TMMOM[,4])
#SMB
plot(kuvshin.stars.TMMOM[,6])
#HML
plot(kuvshin.stars.TMMOM[,8])
#mom
plot(kuvshin.stars.TMMOM[,10])

# пастор-стамбау
kuvshin.TMLIQUID <- NULL -> kuvshin.TMLIQUIDR -> kuvshin.stars.TMLIQUID
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + TM.rmrf + smb + TM.smb + hml + TM.hml + liq + TM.liq)
  kuvshin.TMLIQUID <- rbind(kuvshin.TMLIQUID,summary(kosti)$coefficients[,1])
  kuvshin.TMLIQUIDR <- rbind(kuvshin.TMLIQUIDR,summary(kosti)$r.squared)
  kuvshin.stars.TMLIQUID  <- rbind(kuvshin.stars.TMLIQUID  ,summary(kosti)$coefficients[,4])
}

# Посмотрим на результаты
#R^2
summary(kuvshin.TMLIQUIDR) 

# звезды
#alpha
plot(kuvshin.stars.TMLIQUID[,2])
#rmrf
plot(kuvshin.stars.TMLIQUID[,4])
#SMB
plot(kuvshin.stars.TMLIQUID[,6])
#HML
plot(kuvshin.stars.TMLIQUID[,8])
#liq
plot(kuvshin.stars.TMLIQUID[,10])


# смешанная пятифакторная модель
kuvshin.TMMIXED <- NULL -> kuvshin.TMMIXEDR -> kuvshin.stars.TMMIXED
for (i in 1:ncol(novie_fondi)){
  fund.xcess <- novie_fondi[,i]-risk.market_and_risk.free [,2] 
  kosti <- lm(fund.xcess ~ rmrf + TM.rmrf + smb + TM.smb + hml + TM.hml + mom + TM.mom + liq + TM.liq)
  kuvshin.TMMIXED <- rbind(kuvshin.TMMIXED,summary(kosti)$coefficients[,1])
  kuvshin.TMMIXEDR <- rbind(kuvshin.TMMIXEDR,summary(kosti)$r.squared)
  kuvshin.stars.TMMIXED  <- rbind(kuvshin.stars.TMMIXED  ,summary(kosti)$coefficients[,4])
}

# Посмотрим на результаты
#R^2
summary(kuvshin.TMMIXEDR) 

# звезды
#alpha
plot(kuvshin.stars.TMMIXED[,2])
#rmrf
plot(kuvshin.stars.TMMIXED[,4])
#SMB
plot(kuvshin.stars.TMMIXED[,6])
#HML
plot(kuvshin.stars.TMMIXED[,8])
#mom
plot(kuvshin.stars.TMMIXED[,10])
#liq
plot(kuvshin.stars.TMMIXED[,12])


# График стандартног отклонения со средним
Mean <- apply(novie_fondi, 2, mean)
Standart_deviation <- apply(novie_fondi, 2, sd)
plot(Standart_deviation, Mean)

# не знаю как это сделать
index(novie_fondi) <- index(managers[1:120,]) 
Benchmark <- xts(rmrf, order.by = index(novie_fondi))
index(Benchmark) <- index(managers[1:120,]) 
chart.CaptureRatios(Ra = novie_fondi, Rb = Benchmark, xlim = c(-1, 2))
?chart.CaptureRatios
data(managers)
chart.CaptureRatios(managers[,1:6], managers[,7,drop=FALSE])

nrow(managers)
nrow(novie_fondi)

# график ковариаций (каков синатиксиз блин diag panel)
chart.Correlation(cbind(mom, rmrf, smb, hml, liq), labels = c('mom', 'rmrf', 'smb', 'hml', 'liq'))
                
plot(cumsum(kuvshinSMB))

# ЗАЙМЕМСЯ БУТСТРАПОМ
# склеем доходности фондов и все факторы в одну таблицу
bootstrap_data <- NULL
bootstrap_data <- cbind(bootstrap_data,novie_fondi)
bootstrap_data <- cbind(bootstrap_data,rmrf)
bootstrap_data <- cbind(bootstrap_data,smb)
bootstrap_data <- cbind(bootstrap_data,hml)
bootstrap_data <- cbind(bootstrap_data,mom)
bootstrap_data <- cbind(bootstrap_data,liq)
kuvshin.BS.MEAN <- kuvshin.BS.SD <- kuvshin.BS.MIN <- kuvshin.BS.MAX <- kuvshin.BS.STAR <- NULL

# возьмем 100 раз по 40 случайных строк
for (i in 1:100) {
temp.data <- bootstrap_data[sample(1:120,40),]
kuvshin.BOOTSTRAP <- NULL
kuvshin.BOOTSTRAP.STAR <- NULL
for (b in 1:386){  # сделаем регрессию
  fund.xcess <- temp.data[,b]-temp.data[,387] 
  kosti <- lm(fund.xcess ~ temp.data[,387] + temp.data[,388] + temp.data[,389]  + temp.data[,390] + temp.data[,391])
kuvshin.BOOTSTRAP <- rbind(kuvshin.BOOTSTRAP, summary(kosti)$coefficients[1,1]) # вынем альфы
kuvshin.BOOTSTRAP.STAR <- rbind (kuvshin.BOOTSTRAP.STAR,summary(kosti)$coefficients[1,4] ) # вынем значительность альф (звезды)
}
kuvshin.BS.MEAN <- rbind(kuvshin.BS.MEAN, mean(kuvshin.BOOTSTRAP))
kuvshin.BS.SD <- rbind(kuvshin.BS.SD, sd(kuvshin.BOOTSTRAP))
kuvshin.BS.MIN <- rbind(kuvshin.BS.MIN, min(kuvshin.BOOTSTRAP))
kuvshin.BS.MAX <- rbind(kuvshin.BS.MAX, max(kuvshin.BOOTSTRAP))
kuvshin.BS.STAR <- rbind(kuvshin.BS.STAR, mean(kuvshin.BOOTSTRAP.STAR)) # от звезд берем среднее по каждой 40-ке
}
bootstrap_result <- NULL
bootstrap_result <- cbind(bootstrap_result, kuvshin.BS.MEAN)
bootstrap_result <- cbind(bootstrap_result, kuvshin.BS.SD)
bootstrap_result <- cbind(bootstrap_result, kuvshin.BS.MIN)
bootstrap_result <- cbind(bootstrap_result, kuvshin.BS.MAX)
bootstrap_result <- cbind(bootstrap_result, kuvshin.BS.STAR)
colnames(bootstrap_result) <- c("MEAN", "SD", "MIN", "MAX", "STAR")


# доказательство того, что тут нету плохих фондов
kuvschinCCC <- NULL
for (i in 1: ncol(novie_fondi)) {
  ccc <- max(novie_fondi[,i])
  kuvschinCCC <- rbind(kuvschinCCC,ccc)
}
plot(kuvschinCCC)

# посмотрим на графики факторов
#SMB
plot(cumsum(smb))
#HML
plot(cumsum(hml))
#MOM
plot(cumsum(mom))
#LIQ
plot(cumsum(liq))
 

