# --- intervalo de confianca da media, conhecendo a variancia populacional

temp <- c(22, 24, 21, 22, 25, 26, 25, 24, 23, 25, 25, 26, 27, 25, 26, 25, 26, 27, 27, 28, 29, 29, 29, 28, 30, 29, 30, 31, 30, 28, 29)
n <- length(temp)
media <- mean(temp)
varia <- var(temp) # usa n-1
varia_0 <- 3

alfa <- 0.01

lim_inf <- media - qnorm(1-alfa/2)*sqrt(varia_0/n)
lim_sup <- media + qnorm(1-alfa/2)*sqrt(varia_0/n)

cat('alfa = ', alfa, 'interv = [', lim_inf, ', ', lim_sup, ']\n')

alfa <- 0.05

lim_inf <- media - qnorm(1-alfa/2)*sqrt(varia_0/n)
lim_sup <- media + qnorm(1-alfa/2)*sqrt(varia_0/n)

cat('alfa = ', alfa, 'interv = [', lim_inf, ', ', lim_sup, ']\n')

sao_iguais <- qnorm(1-alfa/2) - (-qnorm(alfa/2))
cat(sao_iguais, '\n')


# --- intervalo de confianca da media, desconhecendo a variancia populacional

alfa <- 0.05

lim_inf <- media - qt(1-alfa/2, n-1)*sqrt(varia/n)
lim_sup <- media + qt(1-alfa/2, n-1)*sqrt(varia/n)

cat('alfa = ', alfa, 'interv = [', lim_inf, ', ', lim_sup, ']\n')

cat(t.test(temp, conf.level = 0.95)$conf.int, '\n')


# --- avaliacao de intervalo de confianca de trechos

varia_0 <- 1
n <- 10000
dados   <- rnorm(n, 0, varia_0)
for(i in c(10, 10, 10, 10, 100, 1000, 10000)) {
  subdados <- sample(dados, i)
	media   <- mean(subdados)
	varia   <- var(subdados)
	lim_inf <- media - qnorm(1-alfa/2)*sqrt(varia_0/i)
	lim_sup <- media + qnorm(1-alfa/2)*sqrt(varia_0/i)
	cat('n = ', i, 'alfa = ', alfa, 'interv = [', lim_inf, ', ', lim_sup, ']\n')

	lim_inf <- media - qt(1-alfa/2, i-1)*sqrt(varia/i)
	lim_sup <- media + qt(1-alfa/2, i-1)*sqrt(varia/i)
	cat('n = ', i, 'alfa = ', alfa, 'interv = [', lim_inf, ', ', lim_sup, ']\n')
}