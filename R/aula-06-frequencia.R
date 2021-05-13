torcedores <- c(sample(1:20, 30, replace=T), sample(21:40, 65, replace=T), sample(41:60, 50, replace=T), sample(61:70, 100, replace=T))
torcedores_agrup <- cut(torcedores, breaks = c(-Inf, 20, 40, 60, +Inf), include.lowest = T, right = T)
table(torcedores_agrup)
table(torcedores_agrup)/length(torcedores)
