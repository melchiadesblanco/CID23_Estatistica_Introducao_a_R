n = 200

# geracao de valores para scatter : associacao positiva linear forte
X = rnorm(n, mean = 50, sd = 50)
Y = 2 + 5 * X + rnorm(n, mean = 50, sd = 100)

r_pearson  <- cor(X, Y, method="pearson")
r_spearman <- cor(X, Y, method="spearman")
plot(X, Y, pch = 19, main = sprintf('Associação forte positiva linear\nr_pearson = %f, r_spearman = %f', r_pearson, r_spearman), col.axis = 'white')

# geracao de valores para scatter : associacao negativa linear forte
X = rnorm(n, mean = 50, sd = 50)
Y = 2 - 5 * X + rnorm(n, mean = 50, sd = 100)

r_pearson  <- cor(X, Y, method="pearson")
r_spearman <- cor(X, Y, method="spearman")
plot(X, Y, pch = 19, main = sprintf('Associação forte negativa linear\nr_pearson = %f, r_spearman = %f', r_pearson, r_spearman), col.axis = 'white')



# geracao de valores para scatter : associacao positiva linear fraca
X = rnorm(n, mean = 50, sd = 50)
Y = 2 + 0.5 * X + rnorm(n, mean = 50, sd = 80)

r_pearson  <- cor(X, Y, method="pearson")
r_spearman <- cor(X, Y, method="spearman")
plot(X, Y, pch = 19, main = sprintf('Associação fraca positiva linear\nr_pearson = %f, r_spearman = %f', r_pearson, r_spearman), col.axis = 'white')

# geracao de valores para scatter : associacao negativa linear fraca
X = rnorm(n, mean = 50, sd = 50)
Y = 2 - 0.5 * X + rnorm(n, mean = 50, sd = 80)

r_pearson  <- cor(X, Y, method="pearson")
r_spearman <- cor(X, Y, method="spearman")
plot(X, Y, pch = 19, main = sprintf('Associação fraca negativa linear\nr_pearson = %f, r_spearman = %f', r_pearson, r_spearman), col.axis = 'white')



# geracao de valores para scatter : sem associacao
X = rnorm(n, mean = 50, sd = 50)
Y = rnorm(n, mean = 50, sd = 100)

r_pearson  <- cor(X, Y, method="pearson")
r_spearman <- cor(X, Y, method="spearman")
plot(X, Y, pch = 19, main = sprintf('Sem associação linear\nr_pearson = %f, r_spearman = %f', r_pearson, r_spearman), col.axis = 'white')



# geracao de valores para scatter : sem associacao
X = rnorm(n, mean = 50, sd = 18)
Y = - (X-50)^2 + rnorm(n, mean = 50, sd = 200)

r_pearson  <- cor(X, Y, method="pearson")
r_spearman <- cor(X, Y, method="spearman")
plot(X, Y, pch = 19, main = sprintf('Associação não-linear\nr_pearson = %f, r_spearman = %f', r_pearson, r_spearman), col.axis = 'white')
