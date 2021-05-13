# distribuicao discreta uniforme

set.seed(123789)
x <- sample(x = c(1,2,3,4,5,6), size=10000, replace=T, prob = c(1/6,1/6,1/6,1/6,1/6,1/6))
barplot(table(x)/10000, ylim=c(0,0.2))

x <- -1:8
barplot(dunif(x, min=1, max=6), names = x, main = 'PDF uniforme')
barplot(punif(x, min=1, max=6), names = x, main = 'CDF uniforme')

# distribuicao discreta binomial
choose(10,7)*(0.9^7)*(0.1^3)
choose(10,9)*(0.9^9)*(0.1^1)
dbinom(x=7, size=10, prob=0.9)
dbinom(x=9, size=10, prob=0.9)

x <- 0:10
barplot(dbinom(x, size=10, prob=0.9), names = x, main = 'PDF binomial')
barplot(pbinom(x, size=10, prob=0.9), names = x, main = 'CDF binomial')

# distribuicao discreta de Poisson
dpois(x=3, lambda=3)
ppois(q=3, lambda=3)
x <- 0:10
barplot(dpois(x, lambda=3), names = x, main = 'PDF de Poisson')
barplot(ppois(x, lambda=3), names = x, main = 'CDF de Poisson')

# distribuicao multinomial
dmultinom(x=c(5,2,3), prob=c(0.7,0.1,0.2))

# distribuicao continua uniforme
x <- seq(-3,4,length.out=1000)
plot(x, dunif(x, min=-2, max=3), main='PDF uniforme', type='l', lwd=2, ylab='')
plot(x, punif(x, min=-2, max=3), main='CDF uniforme', type='l', lwd=2, ylab='')

# distribuicao normal
x <- seq(-3,3,length.out=100)
plot(x, dnorm(x, mean=0, sd=sqrt(0.5)), main='PDFs normais', type='l', lwd=2, ylab='')
lines(x, dnorm(x, mean=0, sd=1), lty=2, lwd=2)
lines(x, dnorm(x, mean=0, sd=sqrt(2)), lty=3, lwd=2)
legend('topright', legend=c('N(0,0.5)', 'N(0,1)', 'N(0.2)'), lty=c(1, 2, 3), lwd=2, bty = 'n')

plot(x, pnorm(x, mean=0, sd=sqrt(0.5)), main='CDFs normais', type='l', lwd=2, ylab='')
lines(x, pnorm(x, mean=0, sd=1), lty=2, lwd=2)
lines(x, pnorm(x, mean=0, sd=sqrt(2)), lty=3, lwd=2)
legend('bottomright', legend=c('N(0,0.5)', 'N(0,1)', 'N(0.2)'), lty=c(1, 2, 3), lwd=2, bty = 'n')

# distribuicao exponencial
x <- seq(-1,4,length.out=1000)
plot(x, dexp(x, rate=1.2), main='PDF exponencial', type='l', lwd=2, ylab='')
plot(x, pexp(x, rate=1.2), main='CDF exponencial', type='l', lwd=2, ylab='')

# distribuicao qui-quadrado
x <- seq(0,4,length.out=100)
plot(x, dchisq(x, df=1), main=expression(paste('PDFs ', chi^2)), type='l', lwd=2, ylab='')
lines(x, dchisq(x, df=2), lty=2, lwd=2)
lines(x, dchisq(x, df=5), lty=3, lwd=2)
legend('topright', legend=c('df = 1', 'df = 2', 'df = 5'), lty=c(1, 2, 3), lwd=2, bty = 'n')

plot(x, pchisq(x, df=1), main=expression(paste('CDFs ', chi^2)), type='l', lwd=2, ylab='')
lines(x, pchisq(x, df=2), lty=2, lwd=2)
lines(x, pchisq(x, df=5), lty=3, lwd=2)
legend('bottomright', legend=c('df = 1', 'df = 2', 'df = 5'), lty=c(1, 2, 3), lwd=2, bty = 'n')

# distribuicao t
x <- seq(-3,3,length.out=100)
plot(x, dt(x, df=30), main='PDFs t', type='l', lwd=2, ylab='')
lines(x, dt(x, df=5), lty=2, lwd=2)
lines(x, dt(x, df=1), lty=3, lwd=2)
legend('topright', legend=c('df = 30', 'df = 5', 'df = 1'), lty=c(1, 2, 3), lwd=2, bty = 'n')

plot(x, pt(x, df=30), main='CDFs t', type='l', lwd=2, ylab='')
lines(x, pt(x, df=5), lty=2, lwd=2)
lines(x, pt(x, df=1), lty=3, lwd=2)
legend('bottomright', legend=c('df = 30', 'df = 5', 'df = 1'), lty=c(1, 2, 3), lwd=2, bty = 'n')

# distribuicao F
x <- seq(-1,3,length.out=100)
plot(x, df(x, df1=5, df2=30), main='PDFs F', type='l', lwd=2, ylab='')
lines(x, df(x, df1=5, df2=10), lty=2, lwd=2)
lines(x, df(x, df1=5, df2=5), lty=3, lwd=2)
legend('topright', legend=c('df = 30', 'df = 5', 'df = 1'), lty=c(1, 2, 3), lwd=2, bty = 'n')

plot(x, pf(x, df1=5, df2=30), main='CDFs F', type='l', lwd=2, ylab='')
lines(x, pf(x, df1=5, df2=10), lty=2, lwd=2)
lines(x, pf(x, df1=5, df2=5), lty=3, lwd=2)
legend('bottomright', legend=c('df = 30', 'df = 5', 'df = 1'), lty=c(1, 2, 3), lwd=2, bty = 'n')
