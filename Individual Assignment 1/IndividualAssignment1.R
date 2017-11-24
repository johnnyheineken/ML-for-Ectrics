library(tidyverse)
library(reshape2)
library(purrr)
library(viridis)
library(ggthemes)
library(assertthat)
library(plotly)
library(ggthemes)

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

gauss_basis_gen <- function(data_frame, s_sq = 0.02) {
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

# this function  quickly creates test data. It will be handy throughout the analysis.
# variables are stored in global env. And because it is annoying 
# in the editor, I also create them once here.
values_gb <- data_gen(100) %>% 
  gauss_basis_gen()
phi_mat <- values_gb %>% 
  select(starts_with("gauss")) %>% 
  as.matrix()
t_mat <- as.matrix(values_gb$target) 


create_test_data <- function(n = 100, s_sq = 0.02) {
  values_gb <<- data_gen(n) %>% 
    gauss_basis_gen(s_sq)
  phi_mat <<- values_gb %>% 
    select(starts_with("gauss")) %>% 
    as.matrix()
  t_mat <<- as.matrix(values_gb$target)  
}



values_gb %>% 
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


RLS <- function(phi_mat, target, lambda) {
  assert_that(is.matrix(phi_mat))
  assert_that(is.matrix(target))
  
  w_RLS <- (solve(lambda * diag(dim(phi_mat)[2]) + t(phi_mat) %*% phi_mat) %*% t(phi_mat)) %*% target
  return(w_RLS)
}

get_w_RLS <- function(my_values, target, lambda){
  phi_mat <- my_values %>% 
    select(3:11) %>% 
    as.matrix()
  t_mat <- as.matrix(target)
  return(RLS(phi_mat = phi_mat, target = t_mat, lambda = lambda))
}

w_RLS_result <- matrix(ncol = 5, nrow = 9)
colnames(w_RLS_result) <- c(paste("n", (1:5) * 20, sep = "_"))

for (i in 1:5) {
  create_test_data(n = (20 * i))
  
  w_RLS_result[, i] <- RLS(phi_mat = phi_mat, 
                                   target = t_mat, 
                                   lambda = 0.2)
}



w_RLS_result

#########################
## Bayesian Linear reg ##
#########################

BLR <- function(phi_mat, target, alpha, beta){
  assert_that(is.matrix(phi_mat))
  assert_that(is.matrix(target))
  
  m_0 <- rep(0, 9)
  S_0 <- alpha * diag(1, 9)
  
  
  S_N <- solve(solve(S_0) + beta * t(phi_mat) %*% phi_mat)
  m_N <- S_N %*% (solve(S_0) %*% m_0 + beta * t(phi_mat) %*% target)
  
  
  return(m_N)
}


BLR_updating <- function(phi_mat, target, alpha, beta) {
  assert_that(is.matrix(phi_mat))
  assert_that(is.matrix(target))
  
  m_0 <- as.matrix(rep(0, 9))
  S_0 <- alpha * diag(1, 9)
  m_N <- m_0
  S_N <- S_0
  
  steps <- list()
  
  for (i in 1:dim(phi_mat)[1]) {
    m_0 <- m_N
    S_0 <- S_N
    phi <- t(as.matrix(phi_mat[i, ]))
    S_N <- solve(solve(S_0) + (beta * t(phi) %*% phi))
    m_N <- S_N %*% (solve(S_0) %*% m_0 + (beta * t(phi) %*% target[i]))
    steps[[i]] <- m_N 
  }
  
  return(m_N)
  
}

w_BLR_result <- matrix(ncol = 5, nrow = 9)
colnames(w_BLR_result) <- c(paste("n", (1:5) * 20, sep = "_"))


for (i in 1:5) {
  create_test_data(n = (20 * i))
  w_BLR_result[, i] <- BLR_updating(phi_mat = phi_mat, 
                           target = t_mat, 
                           alpha = 0.5, beta = 25)
}

w_BLR_result

##########################
# s_sq interactive graph #
##########################
# 
# create_test_data()
# 
# predicted_vals <- phi_mat %*% as.matrix(w_BLR_result[,5])
# values_gb$pred_BLR <- predicted_vals[,1]
# 
# values_gb %>% 
#   ggplot() + 
#   geom_line(aes(x = x, y = pred_BLR), color = "red") + 
#   geom_line(aes(x = x, y = sin(2*pi*x))) + 
#   geom_jitter(aes(x = x, y = target))
# 
# 
# 
# # generate data beforehand
# values <- data_gen(100)
# values_gaussbases <- values %>% gauss_basis_gen(s_sq = 0) %>% mutate(s_sq = 0)
# 
# # create dataset of values for different S_sq
# for (i in 1:25) {
#   val_temp <- values %>% 
#     gauss_basis_gen(s_sq = 0.01 * i) %>% 
#     mutate(s_sq = 0.01 * i)
#   values_gaussbases <- rbind(values_gaussbases, val_temp)
# }
# 
# # finding coefficients and then predictions for every set of data.
# values_predictions <- values %>% mutate(preds = 0) %>% mutate(s_sq = 0)
# 
# for (i in unique(values_gaussbases$s_sq)) {
#   val_temp <- values_gaussbases %>% filter(s_sq == i)
#   phi_mat <- val_temp %>% 
#     select(starts_with("gauss")) %>% 
#     as.matrix()
#   t_mat <- as.matrix(val_temp$target)
#   coefs <- RLS(phi_mat = phi_mat, 
#               target = t_mat, 
#               lambda = 0.2)
#   val_temp_2 <- cbind(values, phi_mat %*% as.matrix(coefs))
#   val_temp_2 <- cbind(val_temp_2, rep(i, 100))
#   colnames(val_temp_2) <- names(values_predictions)
#   values_predictions <- rbind(values_predictions, val_temp_2)
# }
#   
#   
# 
# values_predictions <- values_predictions %>% 
#   filter(s_sq != 0)
# 
# # plotting !
# plot_s_sq <- ggplot(values_predictions, aes(x = x, y = preds)) +
#   geom_point(aes(frame = s_sq)) + 
#   geom_line(aes(x = x, y = sin(2*pi*x)))
# plot_s_sq <- ggplotly(plot_s_sq)                
# plot_s_sq


####################
# Exercise 4 - RLS #
####################

# generate data beforehand
values <- data_gen(10)
values_gaussbases <- values %>% 
  gauss_basis_gen(s_sq = 0.02) %>% 
  mutate(n = 10)

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
  coefs <- RLS(phi_mat = phi_mat, 
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
  coefs <- RLS(phi_mat = phi_mat, 
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


#########
# Ex 5  #
#########

w_BLR_result <- matrix(ncol = 5, nrow = 9)
colnames(w_BLR_result) <- c(paste("n", (1:5) * 20, sep = "_"))
beta <- 10
alpha <- 0.2


BLR_iter <- function(phi_mat, target, alpha, beta){
  assert_that(is.matrix(phi_mat))
  assert_that(is.matrix(target))
  
  S_N <- solve(alpha * diag(1, 9) + beta * t(phi_mat) %*% phi_mat)
  m_N <- beta * S_N %*%  t(phi_mat) %*% target
  
  
  return(list(m_N, S_N))
}

BLR_optimal <- function(phi_mat, t_mat, 
                        max_n_iter = 1000, 
                        min_d_alpha = 0.001, 
                        min_d_beta = 0.001, 
                        echo = FALSE) {
  alpha <- 0.5
  beta <- 5
  iteration <- 1 
  d_alpha <- 10
  d_beta <- 10
  
  while ((iteration < max_n_iter) & 
         ((abs(d_alpha) > min_d_alpha) | (abs(d_beta) > min_d_beta))) {
    lambdas <- eigen(beta * t(phi_mat) %*% phi_mat)$values
    gamma <- 0
    for (lambda in lambdas) {
      gamma <- gamma + (lambda/(alpha + lambda))
    }
    
    iter_mN_SN <- BLR_iter(phi_mat = phi_mat, 
                           target = t_mat, 
                           alpha = alpha, 
                           beta = beta)
    m_N <- iter_mN_SN[[1]]
    S_N <- iter_mN_SN[[2]]
    SSR <- 0
    for (i in 1:nrow(phi_mat)) {
      SSR <- SSR + (t_mat[i, ] - (t(m_N) %*% phi_mat[i, ]))^2
    }
    
    d_alpha <- (gamma / (t(m_N) %*% m_N)) - alpha
    alpha <- as.numeric(alpha + d_alpha)
    
    d_beta <- (1/((1 / (nrow(phi_mat) - gamma)) * SSR)) - beta
    beta <- as.numeric(beta + d_beta)
    iteration <- iteration + 1
  }
  if (echo) {
    print(iteration)
    print(as.numeric(d_alpha))
    print(as.numeric(d_beta))
    print(alpha)
    print(beta)
  }
  return(m_N)
}


create_test_data(n = 10, s_sq = 0.02)
BLR_optimal(phi_mat, t_mat, echo = TRUE)





plot_interactive_graph_BLS_RLS <- function(s_squared, 
                                           lambda_RLS = 0.01, 
                                           alpha_BLR = 1, 
                                           beta_BLR = 25, n_obs = 101) {
  values <- data_gen(10)
  values_gaussbases <- values %>% 
    gauss_basis_gen(s_sq = s_squared) %>% 
    mutate(n = 10)
  
  for (i in 5:25) {
    val_temp <- data_gen(i * 4) %>% 
      gauss_basis_gen(s_sq = s_squared) %>% 
      mutate(n = 4 * i)
    values_gaussbases <- rbind(values_gaussbases, val_temp)
  }
  
  # finding coefficients and then predictions for every set of data.
  values_predictions <- values %>% 
    mutate(preds_BLR = 0)  %>%  
    mutate(preds_RLS = 0) %>%
    mutate(n = 0)
  
  
  
  # values_preds_temp <- data_gen(n_obs)  %>% # OCD unfriendly
  #   gauss_basis_gen(s_sq = s_squared) %>% 
  #   mutate(n = 0)
  # phi_mat_temp <- values_preds_temp %>% 
  #     select(starts_with("gauss")) %>% 
  #     as.matrix()
  
  for (i in unique(values_gaussbases$n)) {
    val_temp <- values_gaussbases %>% filter(n == i)
    
    phi_mat <- val_temp %>% 
      select(starts_with("gauss")) %>% 
      as.matrix()
    t_mat <- as.matrix(val_temp$target)
    
    coefs_RLS    <- RLS(phi_mat, t_mat, lambda = lambda_RLS)
    coefs_BLR    <- BLR(phi_mat, t_mat, alpha = alpha_BLR, beta = beta_BLR)
    
    val_temp_2 <- cbind(val_temp$x, 
                        val_temp$target, 
                        phi_mat %*% as.matrix(coefs_BLR), 
                        phi_mat %*% as.matrix(coefs_RLS),
                        rep(i, n_obs))
    
    # print(dim(val_temp_2))
    colnames(val_temp_2) <- names(values_predictions)
    values_predictions <- rbind(values_predictions, val_temp_2)
  }
  values_predictions <- values_predictions %>% 
    filter(n != 0)
  
  # plotting !
  p <- ggplot(values_predictions, aes(x = x))  +
    geom_line(aes(y = preds_BLR, frame = n, colour = sprintf("BLR, alpha = %s, beta = %s", alpha_BLR, beta_BLR))) +
    geom_line(aes(y = preds_RLS, frame = n, colour = sprintf("RLS, lambda = %s", lambda_RLS))) +
    geom_point(aes(x = x, y = target, frame = n, colour = "target"), alpha = 0.3) + 
    geom_line(aes(x = x, y = sin(2 * pi * x), colour = "sin(2*pi*x)"), alpha = 0.3)  +
    ggtitle(sprintf("Comparision of performance of RLS and BLR when s_squared = %s", s_squared)) + 
    labs(y = "predictions and targets", colour = "colours") + my_theme
  
  plot <- ggplotly(p,
                   hoverinfo = 'text',
                   text = ~paste('Species: ', Species,
                                 '</br> Petal Lenght: ', rmse_BLR,
                                 '</br> Petal Width: ', rmse_RLS)) %>%
    animation_opts(easing = "linear",redraw = TRUE)
  rmse <- values_predictions %>% 
    group_by(n) %>% 
    select(n, rmse_BLR, rmse_RLS) %>% 
    summarise(rmse_BLR = mean(rmse_BLR), rmse_RLS = mean(rmse_RLS)) %>% filter(n%%20 == 0)
  print(rmse)
  plot
  
}




#```{r plot_interactive_graph_BLS_RLS, message=FALSE, warning=FALSE, echo=FALSE}
####################
# Exercise 4 - RLS  - graph#
####################

plot_interactive_graph_BLS_RLS <- function(s_squared, 
                                           lambda_RLS = 0.01, 
                                           alpha_BLR = 1, 
                                           beta_BLR = 25, n_obs = 101) {
  values <- data_gen(10)
  values_gaussbases <- values %>% 
    gauss_basis_gen(s_sq = s_squared) %>% 
    mutate(n = 10)
  
  for (i in 5:25) {
    val_temp <- data_gen(i * 4) %>% 
      gauss_basis_gen(s_sq = s_squared) %>% 
      mutate(n = 4 * i)
    values_gaussbases <- rbind(values_gaussbases, val_temp)
  }
  
  # finding coefficients and then predictions for every set of data.
  values_predictions <- values %>% 
    mutate(preds_BLR = 0)  %>%  
    mutate(preds_RLS = 0) %>%
    mutate(n = 0)
  
  
  
  values_preds_temp <- data_gen(n_obs)  %>% # OCD unfriendly
    gauss_basis_gen(s_sq = s_squared) %>% 
    mutate(n = 0)
  phi_mat_temp <- values_preds_temp %>% 
    select(starts_with("gauss")) %>% 
    as.matrix()
  
  for (i in unique(values_gaussbases$n)) {
    val_temp <- values_gaussbases %>% filter(n == i)
    
    phi_mat <- val_temp %>% 
      select(starts_with("gauss")) %>% 
      as.matrix()
    t_mat <- as.matrix(val_temp$target)
    
    coefs_RLS    <- RLS(phi_mat, t_mat, lambda = lambda_RLS)
    coefs_BLR    <- BLR(phi_mat, t_mat, alpha = alpha_BLR, beta = beta_BLR)
    
    val_temp_2 <- cbind(values_preds_temp$x, 
                        values_preds_temp$target, 
                        phi_mat_temp %*% as.matrix(coefs_BLR), 
                        phi_mat_temp %*% as.matrix(coefs_RLS),
                        rep(i, n_obs))
    
    # print(dim(val_temp_2))
    colnames(val_temp_2) <- names(values_predictions)
    values_predictions <- rbind(values_predictions, val_temp_2)
  }
  
  
  # 
  values_predictions <- values_predictions %>% 
    filter(n != 0)
  
  # plotting !
  p <- ggplot(values_predictions, aes(x = x))  +
    geom_line(aes(y = preds_BLR, frame = n, colour = sprintf("BLR, alpha = %s, beta = %s", alpha_BLR, beta_BLR))) +
    geom_line(aes(y = preds_RLS, frame = n, colour = sprintf("RLS, lambda = %s", lambda_RLS))) + 
    geom_point(aes(x = x, y = target, frame = n, colour = "target"), alpha = 0.3) + 
    geom_line(aes(x = x, y = sin(2 * pi * x), colour = "sin(2*pi*x)"), alpha = 0.3)  +
    ggtitle(sprintf("Comparision of performance of RLS and BLR when s_squared = %s", s_squared)) + 
    labs(y = "predictions and targets", colour = "colours") + my_theme
  plot <- ggplotly(p) %>%
    animation_opts(easing = "linear",redraw = FALSE)
  plot
}





