library(tidyverse)
library(reshape2)
library(purrr)
library(viridis)
library(ggthemes)
library(xkcd)
library(assertthat)

my_fun <- function(n){
  x <- runif(n, min = -1, max = 1)
  noise <- rnorm(n, mean = 0, sd = 0.1)
  target <- sin(2 * pi * x) + noise
  return(data.frame(x, target))
}

#####################################
# Generate data and iteration steps #
#####################################

data_gen <- function(n){
  x <- runif(n, min = -1, max = 1)
  noise <- rnorm(n, mean = 0, sd = 0.1)
  target <- sin(2 * pi * x) + noise
  return(data.frame(x, target))
}

gauss_basis <- function(x, mu, s_sq){
  return(
    exp(
      (-(x - mu) ^ 2) / (2 * s_sq)
    )
  )
}

gauss_basis_gen <- function(data_frame, s_sq = 0.2) {
  #  x <- data_frame$x
  result <- data_frame
  counter <- 1
  for (i in seq(-1, 1, 0.25)) {
    result <- result %>% 
      mutate(gauss_basis(x, mu = i, s_sq = s_sq))
    colnames(result)[dim(result)[2]] <- paste('gauss', counter, sep = "_")
    counter <- counter + 1
  }
  return(result)
}

my_values <- gauss_basis_gen(data_gen(100), s_sq = 0.02)

my_values %>% 
  melt(id.vars = c("x", "target"), variable.name = "gauss_fun") %>% 
  ggplot(aes(x = x, y = value)) + 
  geom_line(aes(colour = gauss_fun), size = 1) + 
  geom_point(aes(y = target), alpha = 0.2, color = "indianred") + 
  scale_color_viridis(discrete = TRUE, option = "inferno") + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        panel.grid.major = element_line(colour = "gray94"), 
        panel.grid.minor = element_line(linetype = "blank"), 
        panel.background = element_rect(fill = NA))






#####################
## Regularized OLS ##
#####################


RLS <- function(m, target, lambda) {
  assert_that(is.matrix(m))
  assert_that(is.matrix(target))
  
  w_RLS <- (solve(lambda * diag(dim(m)[2]) + t(m) %*% m) %*% t(m)) %*% target
  return(w_RLS)
}

get_w_RLS <- function(my_values, target, lambda){
  phi_mat <- my_values %>% 
    select(3:11) %>% 
    as.matrix()
  t_mat <- as.matrix(target)
  return(RLS(m = phi_mat, target = t_mat, lambda = lambda))
}

w_RLS_result <- matrix(ncol = 5, nrow = 9)
colnames(w_RLS_result) <- c(paste("n", (1:5) * 20, sep = "_"))

for (i in 1:5) {
  values_gb <- data_gen(i * 20) %>% 
    gauss_basis_gen()
  
  phi_mat <- values_gb %>% 
    select(starts_with("gauss")) %>% 
    as.matrix()
  
  t_mat <- as.matrix(values_gb$target)
  
  w_RLS_result[, i] <- RLS(m = phi_mat, 
                                   target = t_mat, 
                                   lambda = 0.2)
}



w_RLS_result

#########################
## Bayesian Linear reg ##
#########################

BLR <- function(m, target, alpha, beta){
  assert_that(is.matrix(m))
  assert_that(is.matrix(target))
  
  m_0 <- rep(0, 9)
  S_0 <- alpha * diag(1, 9)
  
  
  S_N <- solve(solve(S_0) + beta * t(m) %*% m)
  m_N <- S_N %*% (solve(S_0) %*% m_0 + beta * t(m) %*% target)
  
  
  return(m_N)
}


BLR_updating <- function(m, target, alpha, beta) {
  assert_that(is.matrix(m))
  assert_that(is.matrix(target))
  
  m_0 <- as.matrix(rep(0, 9))
  S_0 <- alpha * diag(1, 9)
  m_N <- m_0
  S_N <- S_0
  
  steps <- list()
  
  for (i in 1:dim(m)[1]) {
    m_0 <- m_N
    S_0 <- S_N
    phi <- t(as.matrix(m[i, ]))
    S_N <- solve(solve(S_0) + (beta * t(phi) %*% phi))
    m_N <- S_N %*% (solve(S_0) %*% m_0 + (beta * t(phi) %*% target[i]))
    steps[[i]] <- m_N 
  }
  
  return(m_N)
  
}

w_BLR_result <- matrix(ncol = 5, nrow = 9)
colnames(w_BLR_result) <- c(paste("n", (1:5) * 20, sep = "_"))


for (i in 1:5) {
  values_gb <- data_gen(i * 20) %>% 
    gauss_basis_gen()
  
  phi_mat <- values_gb %>% 
    select(starts_with("gauss")) %>% 
    as.matrix()
  
  t_mat <- as.matrix(values_gb$target)
  
  w_BLR_result[, i] <- BLR_updating(m = phi_mat, 
                           target = t_mat, 
                           alpha = 0.1, beta = 25)
}

w_BLR_result

##########################
# s_sq interactive graph #
##########################

values_gb <- data_gen(100) %>% 
  gauss_basis_gen()

phi_mat <- values_gb %>% 
  select(starts_with("gauss")) %>% 
  as.matrix()

predicted_vals <- phi_mat %*% as.matrix(w_BLR_result[,5])
values_gb$pred_BLR <- predicted_vals[,1]

values_gb %>% 
  ggplot() + 
  geom_line(aes(x = x, y = pred_BLR), color = "red") + 
  geom_line(aes(x = x, y = sin(2*pi*x))) + 
  geom_jitter(aes(x = x, y = target))



# generate data beforehand
values <- data_gen(100)
values_gaussbases <- values %>% gauss_basis_gen(s_sq = 0) %>% mutate(s_sq = 0)

# create dataset of values for different S_sq
for (i in 1:25) {
  val_temp <- values %>% 
    gauss_basis_gen(s_sq = 0.01 * i) %>% 
    mutate(s_sq = 0.01 * i)
  values_gaussbases <- rbind(values_gaussbases, val_temp)
}

# finding coefficients and then predictions for every set of data.
values_predictions <- values %>% mutate(preds = 0) %>% mutate(s_sq = 0)

for (i in unique(values_gaussbases$s_sq)) {
  val_temp <- values_gaussbases %>% filter(s_sq == i)
  phi_mat <- val_temp %>% 
    select(starts_with("gauss")) %>% 
    as.matrix()
  t_mat <- as.matrix(val_temp$target)
  coefs <- RLS(m = phi_mat, 
              target = t_mat, 
              lambda = 0.2)
  val_temp_2 <- cbind(values, phi_mat %*% as.matrix(coefs))
  val_temp_2 <- cbind(val_temp_2, rep(i, 100))
  colnames(val_temp_2) <- names(values_predictions)
  values_predictions <- rbind(values_predictions, val_temp_2)
}
  
  

values_predictions <- values_predictions %>% 
  filter(s_sq != 0)

# plotting !
plot_s_sq <- ggplot(values_predictions, aes(x = x, y = preds)) +
  geom_point(aes(frame = s_sq)) + 
  geom_line(aes(x = x, y = sin(2*pi*x)))
plot_s_sq <- ggplotly(plot_s_sq)                
plot_s_sq


####################
# Exercise 4 - RLS #
####################

# generate data beforehand
values <- data_gen(10)
values_gaussbases <- values %>% gauss_basis_gen(s_sq = 0.02) %>% mutate(n = 10)

for (i in 5:25) {
  val_temp <- data_gen(i * 4) %>% 
    gauss_basis_gen(s_sq = 0.02) %>% 
    mutate(n = 4 * i)
  values_gaussbases <- rbind(values_gaussbases, val_temp)
}

# finding coefficients and then predictions for every set of data.
values_predictions <- values %>% 
  mutate(preds = 0) %>% 
  mutate(n = 0)

for (i in unique(values_gaussbases$n)) {
  val_temp <- values_gaussbases %>% filter(n == i) %>% arrange(x)
  phi_mat <- val_temp %>% 
    select(starts_with("gauss")) %>% 
    as.matrix()
  t_mat <- as.matrix(val_temp$target)
  coefs <- RLS(m = phi_mat, 
               target = t_mat, 
               lambda = 0.1)
  
  val_temp_2 <- cbind(val_temp$x, val_temp$target, phi_mat %*% as.matrix(coefs))
  val_temp_2 <- cbind(val_temp_2, rep(i, i))
  print(dim(val_temp_2))
  colnames(val_temp_2) <- names(values_predictions)
  values_predictions <- rbind(values_predictions, val_temp_2)
}


# 
# values_predictions <- values_predictions %>% 
#   filter(s_sq != 0)

# plotting !
p <- ggplot(values_predictions, aes(x = x, y = preds)) +
  geom_line(aes(frame = n), color = "red") + 
  geom_point(aes(x = x, y = target, frame = n), alpha = 0.2) + 
  geom_line(aes(x = x, y = sin(2 * pi * x)), alpha = 0.2)
plot_s_sq <- ggplotly(p) %>%
  animation_opts(easing = "linear",redraw = TRUE)
plot_s_sq


####################
# Exercise 4 - BLS #
####################

# generate data beforehand
values <- data_gen(10)
values_gaussbases <- values %>% gauss_basis_gen(s_sq = 0.02) %>% mutate(n = 10)

for (i in 5:25) {
  val_temp <- data_gen(i * 4) %>% 
    gauss_basis_gen(s_sq = 0.02) %>% 
    mutate(n = 4 * i)
  values_gaussbases <- rbind(values_gaussbases, val_temp)
}

# finding coefficients and then predictions for every set of data.
values_predictions <- values %>% 
  mutate(preds = 0) %>% 
  mutate(n = 0)

for (i in unique(values_gaussbases$n)) {
  val_temp <- values_gaussbases %>% filter(n == i)
  phi_mat <- val_temp %>% 
    select(starts_with("gauss")) %>% 
    as.matrix()
  t_mat <- as.matrix(val_temp$target)
  coefs <- RLS(m = phi_mat, 
               target = t_mat, 
               lambda = 25)
  
  val_temp_2 <- cbind(val_temp$x, val_temp$target, phi_mat %*% as.matrix(coefs))
  val_temp_2 <- cbind(val_temp_2, rep(i, i))
  print(dim(val_temp_2))
  colnames(val_temp_2) <- names(values_predictions)
  values_predictions <- rbind(values_predictions, val_temp_2)
}


# 
# values_predictions <- values_predictions %>% 
#   filter(s_sq != 0)

# plotting !
plot_s_sq <- ggplot(values_predictions, aes(x = x, y = preds)) +
  geom_line(aes(frame = n), color = "red") + 
  geom_line(aes(x = x, y = sin(2*pi*x)))
plot_s_sq <- ggplotly(plot_s_sq)                
plot_s_sq

