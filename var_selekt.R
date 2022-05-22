library(readxl)
library(xts)

library(readxl)
total <- read_excel("~/Documents/2.Semester/Advanced Quantitative Methods/Semesterarbeit/total.xlsx")
total <- xts(total[,-1], total$Date)
total

######################
#Apple (AIC)
fit <- lm(total$`Apple Inc` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Apple Inc Lag` + total$`USUNR=ECI` + total$`USCPI=ECI` + total$`USRSL=ECI` + total$`USIP=ECI` + total$`USIMP=ECI` + total$`USEXP=ECI` + total$`USGDPF=ECI` + total$PCUAWHLTRAWHLTR, lower = ~ 1))

#Nvidia
fit <- lm(total$`NVIDIA Corp` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`NVIDIA Corp Lag` + total$`USUNR=ECI` + total$`USCPI=ECI` + total$`USRSL=ECI` + total$`USIP=ECI` + total$`USIMP=ECI` + total$`USEXP=ECI` + total$`USGDPF=ECI` + total$PCUAWHLTRAWHLTR, lower = ~ 1))

#Alphabet
fit <- lm(total$`Alphabet Inc` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Alphabet Inc Lag` + total$`USUNR=ECI` + total$`USCPI=ECI` + total$`USRSL=ECI` + total$`USIP=ECI` + total$`USIMP=ECI` + total$`USEXP=ECI` + total$`USGDPF=ECI` + total$PCUAWHLTRAWHLTR, lower = ~ 1))

#Cisco (AIC)
fit <- lm(total$`Cisco Systems Inc` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Cisco Systems Inc Lag` + total$`USUNR=ECI` + total$`USCPI=ECI` + total$`USRSL=ECI` + total$`USIP=ECI` + total$`USIMP=ECI` + total$`USEXP=ECI` + total$`USGDPF=ECI` + total$PCUAWHLTRAWHLTR, lower = ~ 1))

#Cisco (F-Test) --> Fehler
fit <- lm(total$`Cisco Systems Inc` ~ total$`Cisco Systems Inc Lag` + total$`Cisco Systems Inc Lag` + total$`USUNR=ECI` + total$`USCPI=ECI` + total$`USRSL=ECI` + total$`USIP=ECI` + total$`USIMP=ECI` + total$`USEXP=ECI` + total$`USGDPF=ECI` + total$PCUAWHLTRAWHLTR, data=total) 
drop1(fit, test="F")

fit <- update(fit, .~. -total$`USCPI=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`USUNR=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`USGDPF=ECI` )
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`USEXP=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$PCUAWHLTRAWHLTR)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`USIMP=ECI`)
drop1(fit, test = "F")

#ASLM
fit <- lm(total$`ASML Holding NV` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`ASML Holding NV Lag` + total$`NLUNRS=ECI` + total$`NLCPIY=ECI` + total$`NLRSLY=ECI` + total$`NLGDP=ECI` + total$NLDPROINDMISMEI + total$XTIMVA01NLM667S, lower = ~ 1))

#SAP
fit <- lm(total$`SAP SE` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`SAP SE Lag` + total$`DEUNR=ECI` + total$`DECPI=ECI` + total$`DERSL=ECI` + total$`DEIP=ECI` + total$`DEIMY=ECI` + total$`DEEXPY=ECI` + total$`DEGDP=ECI`, lower = ~ 1))

#Logitech (AIC) --> Fehler
fit <- lm(total$`Logitech International SA` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Logitech International SA Lag` + total$`CHJOB=ECI` + total$`CHCPI=ECI` + total$`CHRS=ECI` + total$`CHGDP=ECI` + total$CHEPROINDQISMEI + total$XTEXVA01CHM667S, lower = ~ 1))

#Logitech (F-Test)
fit <- lm(total$`Logitech International SA` ~ total$`Logitech International SA Lag` + total$`CHJOB=ECI` + total$`CHCPI=ECI` + total$`CHRS=ECI` + total$`CHGDP=ECI` + total$CHEPROINDQISMEI + total$XTEXVA01CHM667S, data=total)
drop1(fit, test="F")

fit <- update(fit, .~. -total$`CHRS=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$XTEXVA01CHM667S)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`CHGDP=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`CHCPI=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$CHEPROINDQISMEI)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`CHJOB=ECI`)
drop1(fit, test = "F")

#Tencent (AIC)
fit <- lm(total$`Tencent Holdings Ltd` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Tencent Holdings Ltd Lag` + total$`CNCPI=ECI` + total$`CNRSL=ECI` + total$`CNIMP=ECI` + total$`CNGDPA=ECI`, lower = ~ 1))

#Samsung
fit <- lm(total$`Samsung Electronics Co Ltd` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Samsung Electronics Co Ltd Lag` + total$`JPUNR=ECI` + total$`JPCPI=ECI` + total$`JPRSLS=ECI` + total$`JPIMPY=ECI` + total$`JPEXPY=ECI`, lower = ~ 1))

#Sony
fit <- lm(total$`Sony Group Corp` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Sony Group Corp Lag` + total$`KRCPI=ECI` + total$`KRIO=ECI` + total$`KREXP=ECI` + total$`KRGDQA=ECI` + total$KORPROINDMISMEI, lower = ~ 1))

#ASML Eu
fit <- lm(total$`ASML Holding NV` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`ASML Holding NV Lag` + total$`EUUNR=ECI` + total$`EUPPI=ECI` + total$`EUIP=ECI` + total$`EUGDP=ECI` + total$CPHPTT01EZM659N + total$EA19PRINTO01GPSAM + total$XTIMVA01EZM667S + total$XTEXVA01EZM667S, lower = ~ 1))

#SAP Eu
fit <- lm(total$`SAP SE` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`SAP SE Lag` + total$`EUUNR=ECI` + total$`EUPPI=ECI` + total$`EUIP=ECI` + total$`EUGDP=ECI` + total$CPHPTT01EZM659N + total$EA19PRINTO01GPSAM + total$XTIMVA01EZM667S + total$XTEXVA01EZM667S, lower = ~ 1))

#Logitech Eu (AIC) --> Fehler
fit <- lm(total$`Logitech International SA` ~ 1, data=total)
step(fit, scope=list(direction="both" ,upper = ~ total$`Logitech International SA Lag` + total$`EUUNR=ECI` + total$`EUPPI=ECI` + total$`EUIP=ECI` + total$`EUGDP=ECI` + total$CPHPTT01EZM659N + total$EA19PRINTO01GPSAM + total$XTIMVA01EZM667S + total$XTEXVA01EZM667S, lower = ~ 1))

#Logitech Eu (F-Test)
fit <- lm(total$`Logitech International SA` ~ total$`Logitech International SA Lag` + total$`EUUNR=ECI` + total$`EUPPI=ECI` + total$`EUIP=ECI` + total$`EUGDP=ECI` + total$CPHPTT01EZM659N + total$EA19PRINTO01GPSAM + total$XTIMVA01EZM667S + total$XTEXVA01EZM667S, data=total)
drop1(fit, test="F")

fit <- update(fit, .~. -total$`EUUNR=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`EUPPI=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`EUIP=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$`EUGDP=ECI`)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$XTIMVA01EZM667S)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$XTEXVA01EZM667S)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$EA19PRINTO01GPSAM)
drop1(fit, test = "F")

fit <- update(fit, .~. -total$CPHPTT01EZM659N )
drop1(fit, test = "F")
