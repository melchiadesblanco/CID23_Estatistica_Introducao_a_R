temperatura <- c(22, 24, 21, 22, 25, 26, 25, 24, 23, 25, 25, 26, 27, 25, 26,
								 25, 26, 27, 27, 28, 29, 29, 29, 28, 30, 29, 30, 31, 30, 28, 29)
#
mean(temperatura)

temp_agrupado <- 
	hist(temperatura, breaks = c(15, 20, 25, 30, 35, 40), right = F)
weighted.mean(temp_agrupado$mids, temp_agrupado$counts)

com_outliers <- c(1, -1, 1, -1, 1, -1, 1, -1, 1000, 1, -1, 1, -1)
mean(com_outliers)

median(com_outliers)

sort(temperatura)
median(temperatura)

quantile(temperatura)

table(temperatura) # encontrar a moda (maior ocorrência)

prod(temperatura)^(1/length(temperatura))

timao_investimento <- c(1.01, 1.015, 1.025, 1.03, 1.03, 1.03, 1.03, 1.03, 1.03)
prod(timao_investimento)^(1/length(timao_investimento))

gols_coritiba <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
gols_athletico <- c(1, -1, 1, -1, 1, -1, 1, -1, 1)
gols_timao <- c(5, 2, 3, 4, 1, 5, 2, 1, 10)

range(gols_coritiba)
range(gols_athletico)
range(gols_timao)

gols <- data.frame(gols_coritiba,
									 gols_athletico,
									 gols_timao)
summary(gols)

# desvio absoluto (com média e com mediana)
mad(temperatura, center = mean(temperatura), constant = 1)
mad(temperatura, constant = 1)

# variancia e std com viés
n = length(temperatura)
var(temperatura)*(n-1)/n
sd(temperatura)*sqrt((n-1)/n)

mad(gols_coritiba,  center = mean(gols_coritiba),  constant = 1);
mad(gols_athletico, center = mean(gols_athletico), constant = 1);
mad(gols_timao,     center = mean(gols_timao),     constant = 1);

n = length(gols_coritiba) # mesmo n para outros gols_*
var(gols_coritiba)*(n-1)/n
var(gols_athletico)*(n-1)/n
var(gols_timao)*(n-1)/n

sd(gols_coritiba)*sqrt((n-1)/n)
sd(gols_athletico)*sqrt((n-1)/n)
sd(gols_timao)*sqrt((n-1)/n)

scale(gols_coritiba)
scale(gols_athletico)
scale(gols_timao)
scale(gols)
sd(scale(gols_coritiba))
sd(scale(gols_athletico))
sd(scale(gols_timao))

(sd(gols_coritiba)*(n-1)/n)/mean(gols_coritiba)
(sd(gols_athletico)*(n-1)/n)/mean(gols_athletico)
(sd(gols_timao)*(n-1)/n)/mean(gols_timao)

# install.packages('moments')
library(moments)
all.moments(temperatura, order.max=4)
all.moments(temperatura, order.max=4, central = T)
mean(temperatura)
var(temperatura)*(length(temperatura)-1)/length(temperatura)
skewness(temperatura)
kurtosis(temperatura)

skewness(gols)
kurtosis(gols)
