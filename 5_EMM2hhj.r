set.seed(123)
delta = 0.0001 #шаг дискретизации
sigma = 0.01 #средне-квадратическое отклонение
N = 1000 #количество наблюдений

#2. Замоделировать процесс (8) для ∆ = 0.0001 
#( т.е. средне-квадратическое отклонение с.в. 𝜀𝑘∆ равно √∆ = 0.01) 𝑘 = 0,1,...,10^3

t = seq(from = 0, to = N*delta, by = delta)# вектор моментов наблюдений
length(t)
B = numeric(length(t))# вектор для процесса B(t)
B[1] = 0
#моделируем процесс B(t)
for (k in 2:length(t)) {B[k] = B[k-1] + rnorm(1, mean=0, sd=sigma)}
plot(t,B,type='l', main='Процесс B(t)')  
plot(t,B,type='l', main='Процесс B(t)',ylim = c(-1,1))

#3. Построить ансамбль реализаций процесса 𝐵(𝑡):
Ansamble = matrix(0, nrow=N+1, ncol=200)
for (i in 1:200) {for (k in 2:length(t)) {B[k] = B[k-1] + rnorm(1, mean = 0, sd = sigma)}
                  Ansamble[,i] = B #сохраняем реализацию в матрицу ансамбля
                  lines(t,B,type='l')
} 
#4.Ограничить ансамбль реализаций по правилу трех сигм:
#вычисление границ по правилу трех сигм
plot(t, Ansamble[,1], type='l', col='red', xlab='t', ylab='B(t)', main='Ансамбль реализаций процесса B(t)',ylim = c(-1,1))
for (i in 1:200) {lines(t, Ansamble[,i], col='green')}

lower = c(mean(Ansamble[1,]) - 3*sd(Ansamble[1,]), apply(Ansamble[-1,], 1, function(x) mean(x) - 3*sd(x)))
upper = c(mean(Ansamble[1,]) + 3*sd(Ansamble[1,]), apply(Ansamble[-1,], 1, function(x) mean(x) + 3*sd(x)))

for (i in 1:200) {lines(t, Ansamble[,i], col='green')}
lines(t, lower, col='red', lwd=2, lty=2)
lines(t, upper, col='red', lwd=2, lty=2)

#5. Реализовать процесс (9) при следующих входных значениях параметров:
#𝑆0 = 1, 𝑎 = 0.5, 𝜎 = 0.9, √∆ = 0.01, 𝑘 = 0, 1, . . . , 10^3
#Реализовать процесс 𝑆(𝑡):
S_0 = 1
a = 0.5
sigma = 0.9

t = seq(0, N*delta, delta)#вектор моментов наблюдений
  
S = numeric(length(t))#вектор для процесса S(t)
S[1] = S_0
  
#моделируем процесс S(t)
for (k in 2:length(t)) {S[k] = S[k-1] * exp((a - sigma^2/2) * delta + sigma * sqrt(delta) * rnorm(1))}

Ansamble <- matrix(0, nrow=N+1, ncol=200)
for (i in 1:200) 
  {S = numeric(length(t))
   S[1] = S_0
   for (k in 2:length(t)) {S[k] <- S[k-1] * exp((a - sigma^2/2) * delta + sigma * sqrt(delta) * rnorm(1))}
   Ansamble[,i] = S
  }
  
plot(t, Ansamble[,1], type='l', col='gray', xlab='t', ylab='S(t)', main='Ансамбль реализаций процесса S(t)',ylim = c(0.5,2))
for (i in 1:200) {lines(t, Ansamble[,i], col='green')}
