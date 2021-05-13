x <- c(24, 35, 64, 20, 33, 27, 42, 41, 22, 50, 36, 31)
y <- c(90, 65, 30, 60, 60, 80, 45, 45, 80, 35, 50, 45)

plot(x, y, col = 'black', pch = 16,
		 main = 'Performance do elenco do Timão',
		 xlim = c(20, 70),
		 ylim = c(20, 100))

mod_linear <- lm(y~x)
mod_poli_2 <- lm(y~x+I(x^2))
mod_poli_3 <- lm(y~x+I(x^2)+I(x^3))

abline(mod_linear$coefficients[1], mod_linear$coefficients['x'],
			 lty = 3, lwd = 2, col = 'blue')

x_plot <- seq(10, 70, length.out = 100)
lines(x_plot, predict.lm(mod_poli_2, data.frame(x = x_plot)),
			lty = 1, lwd = 2)
lines(x_plot, predict.lm(mod_poli_3, data.frame(x = x_plot)),
			lty = 2, lwd = 2)
legend('topright', c('linear', 'polinômio quadrático', 'polinômio cúbico'),
			 lty = c(2, 1, 3),
			 lwd = 2,
			 col = c('blue', 'black', 'black'))
