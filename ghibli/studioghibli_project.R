### load libraries
library(ggplot2)
library(dplyr)
library(knitr)


### little bit of data cleaning
studioghibli <- read.csv("C:/Users/19132/Downloads/Studio Ghibli.csv", header=FALSE)

colnames(studioghibli) <- studioghibli[1, ]

studioghibli <- studioghibli[-1, ]

### pom poko revenue listed in yen instead of usd
studioghibli$Revenue[studioghibli$Name == "Pom Poko"] <- 29669178

### changing class
studioghibli$Revenue <- as.numeric(gsub("[$,]", "", studioghibli$Revenue))
studioghibli$Budget <- as.numeric(gsub("[$,]", "", studioghibli$Budget))
studioghibli$Year <- as.integer(studioghibli$Year)
studioghibli$Director <- as.factor(studioghibli$Director)
studioghibli$Screenplay <- as.factor(studioghibli$Screenplay)
studioghibli$`Genre 1` <- as.factor(studioghibli$`Genre 1`)
studioghibli$`Genre 2` <- as.factor(studioghibli$`Genre 2`)
studioghibli$`Genre 3` <- as.factor(studioghibli$`Genre 3`)

studioghibli$Revenue <- as.numeric(gsub("[$,]", "", studioghibli$Revenue)) / 1e6
studioghibli$Revenue <- round(studioghibli$Revenue, 2)

### analyze data


summary(studioghibli_subset)

studioghibli_subset <- studioghibli |> 
  select(Name, Revenue) 
  
Revenue <- studioghibli$Revenue
hist(Revenue, col = "pink")


ggplot(studioghibli, aes(x = Revenue, y = reorder(Name, Revenue))) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Revenue of Studio Ghibli Movies", x = "Revenue (USD)", y = "Movie") +
  theme_minimal()



# revenue is right skewed, this a gamma distribution would be a good fit for this analysis.
# revenue is all positive and the gamma distribution is naturally defined on (0, infinity),
# which makes it a good fit. Further, the gamma distribution captures right-skewed 
# distributions well as alpha controls the skewness and beta controls the spread. 


### Posterior inference 


# Y_miyazaki ~ Gamma(αM, βM)
# Y_Notmiyazaki ~ Gamma(αN, βN)



# Define prior parameters
a <- 2  # Weakly informative prior
b <- .02

# mean around 100 m

# Separate data into Miyazaki and Non-Miyazaki
miyazaki_films <- studioghibli |> 
  filter(Director == "Hayao Miyazaki")

not_miyazaki_films <- studioghibli |> 
  filter(Director != "Hayao Miyazaki")

# Calculate posterior parameters for Miyazaki
a_star_miyazaki <- a + sum(miyazaki_films$Revenue, na.rm = TRUE)
b_star_miyazaki <- b + nrow(miyazaki_films)

# Calculate posterior parameters for Non-Miyazaki
a_star_notMiyazaki <- a+ sum(not_miyazaki_films$Revenue, na.rm = TRUE)
b_star_notMiyazaki <- b + nrow(not_miyazaki_films)

# Compute expected values (mean of Gamma distribution)
ev_Miyazaki <- a_star_miyazaki / b_star_miyazaki
ev_NonMiyazaki <- a_star_notMiyazaki / b_star_notMiyazaki

# Compute 95% credible intervals
ci_Miyazaki <- qgamma(c(0.025, 0.975), shape = a_star_miyazaki, rate = b_star_miyazaki)
ci_NonMiyazaki <- qgamma(c(0.025, 0.975), shape = a_star_notMiyazaki, rate = b_star_notMiyazaki)

# Print results
## Miyazaki
cat("Posterior for Miyazaki-directed films: Gamma(", a_star_miyazaki, ",", b_star_miyazaki, ")\n")
cat("  Expected Revenue: ", ev_Miyazaki, "\n")
cat("  95% Credible Interval: (", ci_Miyazaki[1], ",", ci_Miyazaki[2], ")\n\n")

## Not Miyazaki
cat("Posterior for Non-Miyazaki-directed films: Gamma(", a_star_notMiyazaki, ",", b_star_notMiyazaki, ")\n")
cat("  Expected Revenue: ", ev_NonMiyazaki, "\n")
cat("  95% Credible Interval: (", ci_NonMiyazaki[1], ",", ci_NonMiyazaki[2], ")\n\n")



### answering the question (finding the differences)

# Compute prior & posterior means for Miyazaki and Non-Miyazaki
mean_prior_M <- a/b
mean_prior_N <- a/b

# Compute variance of the difference
var_prior_M <- a/b^2
var_prior_N <- a/b^2

var_post_M <- a_star_miyazaki / (b_star_miyazaki^2)
var_post_N <- a_star_notMiyazaki / (b_star_notMiyazaki^2)

# Difference in means (prior & posterior)
mean_prior_diff <- mean_prior_M - mean_prior_N
mean_post_diff <- ev_Miyazaki - ev_NonMiyazaki

# Standard deviation of the difference
sd_prior_diff <- sqrt(var_prior_M + var_prior_N)
sd_post_diff <- sqrt(var_post_M + var_post_N)

# Compute 95% Credible Intervals
ci_prior <- c(mean_prior_diff - 1.96 * sd_prior_diff, mean_prior_diff + 1.96 * sd_prior_diff)
ci_post <- c(mean_post_diff - 1.96 * sd_post_diff, mean_post_diff + 1.96 * sd_post_diff)

# Print results
cat("Prior Difference in Means:\n")
cat("  Mean:", mean_prior_diff, "\n")
cat("  95% CI:", ci_prior, "\n\n")
# before looking at data prior belief assumes no difference 
# CI includes zero supporting prior belief of no difference as well


cat("Posterior Difference in Means:\n")
cat("  Mean:", mean_post_diff, "\n")
cat("  95% CI:", ci_post, "\n\n")
# posterior mean difference is huge ($56.54 million)
# interval does not include zero, suggesting films directed by
# Miyazaki do significantly better (make more revenue) than 
# non-miyazaki films


#### Attempting Plots:

### Prior and Posterior Distributions 

### Prior and Post Distribution 

# Set up plotting area for two plots side by side
par(mfrow = c(1, 2))

# Define range for x-axis based on posterior distributions
x_range_miyazaki <- seq(0, 150, length.out = 1001)
x_range_notmiyazaki <- seq(0,150, length.out = 1001)

# Plot for Miyazaki-directed films
plot(x_range_miyazaki, dgamma(x_range_miyazaki, shape = a, rate = b), 
     type = "l", col = "lightpink", lwd = 2,
     ylim = c(0, max(dgamma(x_range_notmiyazaki, shape = a_star_notMiyazaki, rate = b_star_notMiyazaki))),
     xlab = expression(theta[Miyazaki]), ylab = "Density", main = "Prior & Posterior: Miyazaki")
lines(x_range_miyazaki, dgamma(x_range_miyazaki, shape = a_star_miyazaki, rate = b_star_miyazaki), 
      col = "orchid", lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = c("lightpink", "orchid"), lwd = 2, cex = 0.75)


# Plot for Non-Miyazaki films
plot(x_range_notmiyazaki, dgamma(x_range_notmiyazaki, shape = a, rate = b), 
     type = "l", col = "lightpink", lwd = 2, 
     ylim = c(0, max(dgamma(x_range_notmiyazaki, shape = a_star_notMiyazaki, rate = b_star_notMiyazaki))),
     xlab = expression(theta[NotMiyazaki]), ylab = "Density", main = "Prior & Posterior: Non-Miyazaki")
lines(x_range_notmiyazaki, dgamma(x_range_notmiyazaki, shape = a_star_notMiyazaki, rate = b_star_notMiyazaki), 
      col = "orchid", lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = c("lightpink", "orchid"), lwd = 2, cex = 0.75)


# Reset plotting layout
par(mfrow = c(1, 1))





### Posterior Distribution of θ_M - θ_N

# Set the number of samples for Monte Carlo simulation
n_samples <- 10000

# Sample from posterior distributions
theta_M_samples <- rgamma(n_samples, shape = a_star_miyazaki, rate = b_star_miyazaki)
theta_N_samples <- rgamma(n_samples, shape = a_star_notMiyazaki, rate = b_star_notMiyazaki)

# Compute differences
theta_diff_samples <- theta_M_samples - theta_N_samples

ci_diff <- quantile(theta_diff_samples, probs = c(0.025, 0.975))
cat("95% Credible Interval for θ_M - θ_N:\n")
cat("  (", ci_diff[1], ",", ci_diff[2], ")\n")

# Plot the posterior distribution of θ_M - θ_N
hist(theta_diff_samples, breaks = 50, probability = TRUE, col = "pink", border = "white",
     main = expression("Posterior Distribution of " ~ theta[M] - theta[N]),
     xlab = expression(theta[M] - theta[N]), ylab = "Density")


plot(theta_diff_samples, type = "l", col = "hotpink", 
     main = expression("Trace Plot of " ~ theta[M] - theta[N]),
     xlab = "Sample Index", ylab = expression(theta[M] - theta[N]))


## test



library(flextable)

# Prepare the table
ft <- studioghibli |> 
  select(Name, Year, Director, Revenue) |> 
  arrange(Year) |> 
  flextable() |> 
  set_header_labels(Name = "Movie Name", Year = "Year", Director = "Director", Revenue = "Revenue (In Million USD)") |> 
  colformat_num(j = "Revenue", digits = 2) |> 
  autofit()

save_as_image(ft, path = "studio_ghibli_table.png")
