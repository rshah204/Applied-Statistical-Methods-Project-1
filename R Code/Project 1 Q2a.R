
N=200 # random samples
n=20 # size of random samples
df=6 # degrees of freedom

# Generating random samples of chi-sq dist
samples = matrix(rchisq(N * n, df), nrow = N)

# Calculating the first quantile for each sample
quantile_samples = matrix(0, nrow = N) # Initialize
for(i in 1:N){
  quantile_samples[i] = quantile(samples[i,1:n], probs = 0.25) 
}

# Reporting first 10 sample quantiles
quantile_10 = quantile_samples[1:10]
print(quantile_10)

mean_value = mean(quantile_samples)
var_value = var(quantile_samples)
sd_value = sd(quantile_samples)

# par(mfrow=c(1,1))
# Plotting histogram of sample means (y-axis: density)
hist(quantile_samples, freq = FALSE, main = "Histogram of Sample Q1's") 
# The option freq=FALSE plots probability densities instead of frequencies
# Adding a dashed line for the sample mean
abline(v=mean(quantile_samples), lwd=3, col='darkslategray4')
theoretical_mean = df
theoretical_sd = sqrt(2*df/n)
# Adding a dashed line for the theoretical mean
abline(v=theoretical_mean, lty=2, lwd=3, col='firebrick')
legend(c("Sample", "Theoretical"),x='topright', lty=c(1,2), lwd=c(3,3), col=c('darkslategray4', 'firebrick'))

# Fitting a density estimate (default estimator)
d = density(quantile_samples)
plot(d, main = "Kernel Density Estimate of Sample Means", bty = 'n')
polygon(d, col = "#FFCCCC", border = 'blue', lwd = 2)

# # Adding a dashed line for the sample mean
# abline(v=mean(mean_samples), lty = 2, lwd=2, col='red')
# Adding a dashed line for the sample mean
abline(v=mean(quantile_samples), lwd=3, col='darkslategray4')
# Adding a dashed line for the theoretical mean
abline(v=theoretical_mean, lty=2, lwd=3, col='firebrick')
legend(c("Sample", "Theoretical"),x='topright', lty=c(1,2), lwd=c(3,3), col=c('darkslategray4', 'firebrick'))

# # Plotting histogram of sample Q1's (y-axis: density)
# hist(quantile_samples, freq = FALSE, main = "Histogram of Sample Q1's") 
# # The option freq=FALSE plots probability densities instead of frequencies
# # Adding a dashed line for the sample mean
# abline(v=mean(quantile_samples), lwd=3, col='darkslategray4')
# 
# # Fitting a density estimate for sample Q1's (default estimator)
# d = density(quantile_samples)
# plot(d, main = "Kernel Density Estimate of Sample Q1's", bty = 'n')
# polygon(d, col = "#FFCCCC", border = 'blue', lwd = 2)
# # Adding a dashed line for the sample mean
# abline(v=mean(quantile_samples), lwd=3, col='darkslategray4')

qqnorm(quantile_samples)
abline(a = 0, b = 1)
quantile_samples_norm<-(quantile_samples-mean(quantile_samples))/sd(quantile_samples) ## standardized data
qqnorm(quantile_samples_norm) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line

ks.test(quantile_samples, "pnorm", mean = 0, sd = 1)
ks.test(quantile_samples_norm, "pnorm", mean = 0, sd = 1)

## Shapiro-Wilk Normality Test
## H_0: population is normal
## H_1: population is not normal
shapiro.test(quantile_samples_norm)

error = qnorm(0.975)*sd(quantile_samples)/sqrt(n)
left = mean(quantile_samples)-error
right = mean(quantile_samples)+error
left
right
