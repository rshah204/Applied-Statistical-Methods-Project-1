# Some useful R functions:
# rpois
# matrix; rowMeans
# for
# hist
# lines
# density
# mean
# sd
# var
# sqrt
# rchisq
# quantile

N=200 # random samples
n=10 # size of random samples
lambda = 12 # Poisson distribution parameter

# Generating random samples
samples = matrix(rpois(N * n, lambda), nrow = N)

# Calculating sample mean for each sample
mean_samples = matrix(0, nrow = N) # Initialize
for(i in 1:N){
  mean_samples[i] = mean(samples[i,1:n]) 
  # print(mean_samples[i])
}

# Reporting first 10 sample means
mean_10 = mean_samples[1:10]
print(mean_10)


mean_value = mean(mean_samples)
var_value = var(mean_samples)
sd_value = sd(mean_samples)

# par(mfrow=c(1,1))

# Plotting histogram of sample means (y-axis: density)
hist(mean_samples, freq = FALSE, main = 'Histogram of Sample Means') 
# The option freq=FALSE plots probability densities instead of frequencies
# Adding a dashed line for the mean of sample means
abline(v=mean(mean_samples), lwd=3, col='darkslategray4')
# Theoretical mean and standard deviation 
theoretical_mean = lambda
theoretical_sd = sqrt(lambda/n)
# Adding a dashed line for the theoretical mean
abline(v=theoretical_mean, lty=2, lwd=3, col='firebrick')
legend(c("Sample", "Theoretical"),x='topright', lty=c(1,2), 
       lwd=c(3,3), col=c('darkslategray4', 'firebrick'))

# Fitting a density estimate (default estimator)
d = density(mean_samples)
plot(d, main = "Kernel Density Estimate of Sample Means", bty = 'n')
polygon(d, col = "#FFCCCC", border = 'blue', lwd = 2)
# Adding a dashed line for the sample mean
abline(v=mean(mean_samples), lwd=3, col='darkslategray4')
# Adding a dashed line for the theoretical mean
abline(v=theoretical_mean, lty=2, lwd=3, col='firebrick')
legend(c("Sample", "Theoretical"),x='topright', lty=c(1,2), 
       lwd=c(3,3), col=c('darkslategray4', 'firebrick'))




qqnorm(mean_samples)
abline(a = 0, b = 1)

# Standardizing the sample means to draw QQ-plot
mean_samples_norm<-(mean_samples-mean(mean_samples))/sd(mean_samples) 
qqnorm(mean_samples_norm) # drawing the QQplot
abline(0,1) # drawing a 45-degree reference line

ks.test(mean_samples, "pnorm", mean = 0, sd = 1)
ks.test(mean_samples_norm, "pnorm", mean = 0, sd = 1)
shapiro.test(mean_samples_norm)

error = qnorm(0.975)*sd(mean_samples)/sqrt(n)
left = mean(mean_samples)-error
right = mean(mean_samples)+error
left
right
