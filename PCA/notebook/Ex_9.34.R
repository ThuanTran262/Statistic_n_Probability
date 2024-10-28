data = read.table('./data/T7-7.dat')
data = rename(data, BL=V1, EM=V2, SF=V3, BS=V4, AFL=V5, LFF=V6, FFF=V7, ZST=V8)
View(data)
sub_data = data[, c('BL', 'EM', 'SF', 'BS')]
View(sub_data)
######################## use R matrix ##########################################
(mR = cor(sub_data))
########################### Principal Component Method #########################
# eigenvalues of mR
ev = eigen(mR)
(lambdas = ev$values)
# cummulative proportion
cumsum(lambdas)/sum(lambdas)
# factor loading ( m = 1 factor)
(L = cbind(
  sqrt(lambdas[1])*ev$vectors[,1]))
# communality
(h2=diag(L %*% t(L)))
# specific variances
(Psi=diag(mR) - h2)
# verify R = LL' + Psi
L %*% t(L) + diag(Psi)
(error = mR - (L %*% t(L) + diag(Psi)))
norm(error, type="F")
########################### Maximum Likelihood Method ##########################
diag(t(L) %*% L)
diag(t(L) %*% L)/sum(diag(mR))
# communality
(h2 = diag(L %*% t(L)))
# specific variances
(Psi = diag(mR) - h2)
# residual matrix (R - LL' - Psi)
(error = mR - (L %*% t(L) + diag(Psi)))
norm(error, type="F")

########################## test mô hình với m = 1 ##############################
(n=dim(sub_data)[1])
(p=dim(sub_data)[2])
m=1
anpha = 5/100
(lambda = det(L %*% t(L) + diag(Psi))/det(mR))
(test_stat = (n-1-(2*p + 4*m +5)/6)*log(lambda))
qchisq(1-anpha, df=2)
# p-value
1-pchisq(test_stat, df=2)

######################## use S matrix ##########################################
(mS = cov(sub_data))
########################### Principal Component Method #########################
# eigenvalues of mS
ev = eigen(mS)
(lambdas = ev$values)
# cummulative proportion
cumsum(lambdas)/sum(lambdas)
# factor loading ( m = 1 factor)
(L = cbind(
  sqrt(lambdas[1])*ev$vectors[,1]))
# communality
(h2=diag(L %*% t(L)))
# specific variances
(Psi=diag(mS) - h2)
# verify R = LL' + Psi
L %*% t(L) + diag(Psi)
(error = mS - (L %*% t(L) + diag(Psi)))
norm(error, type="F")
########################### Maximum Likelihood Method ##########################
diag(t(L) %*% L)
diag(t(L) %*% L)/sum(diag(mS))
# communality
(h2 = diag(L %*% t(L)))
# specific variances
(Psi = diag(mS) - h2)
# residual matrix (S - LL' - Psi)
(error = mS - (L %*% t(L) + diag(Psi)))
norm(error, type="F")

########################## test mô hình với m = 1 ##############################
(n=dim(sub_data)[1])
(p=dim(sub_data)[2])
m=1
anpha = 5/100
(lambda = det(L %*% t(L) + diag(Psi))/det(mS))
(test_stat = (n-1-(2*p + 4*m +5)/6)*log(lambda))
qchisq(1-anpha, df=2)
# p-value
1-pchisq(test_stat, df=2)