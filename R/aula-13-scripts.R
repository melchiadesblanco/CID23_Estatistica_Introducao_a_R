#control <- c(11, 15, 9, 4, 34, 17, 18, 14, 12, 13, 26, 31)
#drug    <- c(34, 31, 35, 29, 29, 12, 18, 30, 14, 22, 10)

#control <- c(1, 2, 3, 4, 5)
#drug    <- c(6, 7, 8, 9, 10)

control <- c(3, 4, 2, 6, 2, 5)
drug    <- c(9, 7, 5, 10, 6, 8)


res_t <- wilcox.test(control, drug, alternative = 't', exact = T)
cat('H0:control == drug ',
		res_t$p.value, res_t$statistic, '-',
		qwilcox(res_t$p.value/2,   length(control), length(drug)),
		2*pwilcox(res_t$statistic, length(control), length(drug)),
		'aceita se',
		res_t$statistic, '>',
		qwilcox(0.05/2,            length(control), length(drug)),
		'\n')

res_l <- wilcox.test(control, drug, alternative = 'l', exact = T)
cat('H0:control >= drug ',
	  res_l$p.value, res_l$statistic, '-',
		qwilcox(res_l$p.value,   length(control), length(drug)),
		pwilcox(res_t$statistic, length(control), length(drug)),
		'aceita se',
		res_l$statistic, '>',
		qwilcox(0.05,            length(control), length(drug)),
		'\n')

res_g <- wilcox.test(control, drug, alternative = 'g', exact = T)
cat('H0:control <= drug ',
	  res_g$p.value, res_g$statistic, '-',
		qwilcox(1-res_g$p.value,   length(control), length(drug)),
		1-pwilcox(res_g$statistic, length(control), length(drug)),
		'aceita se',
		res_g$statistic, '<',
		qwilcox(1-0.05,              length(control), length(drug)),
		'\n')
