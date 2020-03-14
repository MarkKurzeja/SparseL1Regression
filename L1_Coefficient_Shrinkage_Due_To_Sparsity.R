# L1 regularization script
rm(list = ls())

library(L1pack)
library(plyr)
library(dplyr)
library(glmnet)
library(ggplot2)
library(magrittr)


# Plot the true coefficients
coef_vector = c(10, 2, 1, 0.5, 0)

data.frame(Coefficient = seq_along(coef_vector), Value = coef_vector) %>% 
  ggplot() + 
  labs(y = "") + 
  geom_bar(aes(x = Coefficient, Value), stat = "identity") +
  geom_hline(yintercept = 3, color = "green") +
  geom_hline(yintercept = 1, color = "blue") +
  ggtitle("True Coefficients to Be Estimated")

# Run the simulation
missing_percentages = c(0.01, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975, 0.99,0.995, 0.999)
controls = data.frame(expand.grid(lambda = c(0, 0.5, 2), missing = missing_percentages))

results <- mdply(.data = controls, function (lambdain, missingin) {
  result = rdply(500, {
    rownum = 2000
    colnum = length(coef_vector)
    rowdata = rnorm(rownum * colnum)
    
    data = data.frame(matrix(
      rep(rowdata, rownum), byrow = T, nrow = rownum, ncol = colnum))
    
    # Make all of the other values missing
    missing = missingin
    index = sample(seq_len(rownum), missing * rownum, replace = F)
    data[index, 1] = 0
    
    values_to_guess = as.matrix(coef_vector, ncol = 1)
    y = as.matrix(data) %*% as.matrix(values_to_guess) + matrix(rnorm(rownum, mean = 0, sd = 1), ncol = 1)
    
    result = glmnet(as.matrix(data), y, alpha = 1, lambda = lambdain)
    true_value = coef_vector[1]
    estimate = coef(result)[2]
    return(data.frame(truth = true_value, estimate = estimate))
  })
  return(result)
}, .progress = plyr::progress_tk(title = "Estimation Progress"))


results %>% 
  group_by(lambda, missing) %>% 
  summarize(
    estimate_m = mean(estimate), 
    low = quantile(estimate, probs = 0.05),
    high = quantile(estimate, probs = 0.95),
    ) %>%
  ungroup() %>%
  mutate(lambda = as.factor(lambda)) %>%
  ggplot() +
  geom_line(aes(x = missing, y = estimate_m, group = lambda, color = lambda)) +
  geom_ribbon(aes(x = missing, ymin = low, ymax = high, group = lambda, fill = lambda), alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") 

results %>% 
  group_by(lambda, missing) %>% 
  summarize(
    estimate_m = mean(estimate), 
    low = quantile(estimate, probs = 0.05),
    high = quantile(estimate, probs = 0.95),
  ) %>%
  ungroup() %>%
  mutate(lambda = as.factor(lambda)) %>%
  ggplot() +
  geom_line(aes(x = missing, y = estimate_m, group = lambda, color = lambda)) +
  geom_ribbon(aes(x = missing, ymin = low, ymax = high, group = lambda, fill = lambda), alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(trans="atanh", breaks = c(0.01, 0.5, 0.8, 0.9, 0.95, 0.99, 0.999))
  # scale_x_continuous(breaks = missing_percentages, )
  # scale_y_log10() + 


max_range = 12

results %>% 
  filter(lambda == 0) %>% 
  filter(missing == 0.01) %>%
  ggplot() + 
  geom_histogram(aes(x = estimate, y = stat(count) / sum(count)), binwidth = 0.1) +
  scale_x_continuous(limits = c(-1,max_range)) +
  geom_vline(xintercept = 10, color = "red") +
  ggtitle("No Shrinkage, No Missing Data")

results %>% 
  filter(lambda == 0) %>% 
  filter(missing == 0.999) %>%
  ggplot() + 
  geom_histogram(aes(x = estimate, y = stat(count) / sum(count)), binwidth = 0.1) +
  scale_x_continuous(limits = c(-1,max_range)) +
  geom_vline(xintercept = 10, color = "red") +
  ggtitle("No Shrinkage, 1/1000 Sparse")

results %>% 
  filter(lambda == 0.5) %>% 
  filter(missing == 0.999) %>%
  ggplot() + 
  geom_histogram(aes(x = estimate, y = stat(count) / sum(count)), binwidth = 0.1) +
  scale_x_continuous(limits = c(-1,max_range)) +
  geom_vline(xintercept = 10, color = "red") +
  ggtitle("Minimal Shrinkage, 1/1000 Sparse")

results %>% 
  filter(lambda == 0.5) %>% 
  filter(missing == 0.01) %>%
  ggplot() + 
  geom_histogram(aes(x = estimate, y = stat(count) / sum(count)), binwidth = 0.1) +
  scale_x_continuous(limits = c(-1,max_range)) +
  geom_vline(xintercept = 10, color = "red") +
  ggtitle("Minimal Shrinkage, No Missing Data")

results %>% 
  filter(lambda == 0.5) %>% 
  filter(missing == 0.99) %>%
  ggplot() + 
  geom_histogram(aes(x = estimate, y = stat(count) / sum(count)), binwidth = 0.1) +
  scale_x_continuous(limits = c(-1,max_range)) +
  geom_vline(xintercept = 10, color = "red") +
  ggtitle("Effective Shrinkage, 99/100 sparse")

# summary(results$diff)

# plot(coef_vector, result[2:(colnum + 1)])
# abline(a = 0, b = 1)
# points(coef_vector[1], result[2], col = "red")

