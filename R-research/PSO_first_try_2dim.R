#https://rtichoke.netlify.app/post/pso_optimisation/

pacman::p_load(dplyr, gganimate, metR)

obj_func <- function(x, y){
  # Modifying for a different global minimum
  -20 * exp(-0.2 * sqrt(0.5 *((x-1)^2 + (y-1)^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20
}

# Set of x and y values (search domain)
x <- seq(-10, 10, length.out = 100)
y <- seq(-10, 10, length.out = 100)

# Create a data frame that stores every permutation of
# x and y coordinates
grid <- expand.grid(x, y, stringsAsFactors = F)

# Set of x and y values (search domain)
x <- seq(-10, 10, length.out = 100)
y <- seq(-10, 10, length.out = 100)

# Create a data frame that stores every permutation of
# x and y coordinates
grid <- expand.grid(x, y, stringsAsFactors = F)

# Evaluate the objective function at each x, y value
grid$z <- obj_func(grid[,1], grid[,2])

# create a contour plot
contour_plot <- ggplot(grid, aes(x = Var1, y = Var2)) +
  geom_contour_filled(aes(z = z), color = "black", alpha = 0.5) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  labs(x = "x", y = "y", title = "Ackley's Function", subtitle = "Contour plot")

contour_plot








# Say we start with 20 particles
n_particles <- 20

# Set some initial values for constants
w <- 0.5
c1 <- 0.05
c2 <- 0.1

# Search domain in x and y coordinates
x <- seq(-10, 10, length.out = 20)
y <- seq(-10, 10, length.out = 20)

# Combine into a matrix
X <- data.frame(x = sample(x, n_particles, replace = F),
                y = sample(y, n_particles, replace = F))


# Chart starting locations
contour_plot +
  geom_point(data = X, aes(x, y), color = "red", size = 2.5) +
  labs(title = "PSO", subtitle = "Iter 0")


# Uniformly distributed (positive) perturbations
dX <- matrix(runif(n_particles * 2), ncol = 2)

# Scale down the perturbations by a factor (w in the equation above)
dX <- dX * w



# Set the location of the local best (optimal value) to starting positions
pbest <- X

# Evaluate the function at each point and store
pbest_obj <- obj_func(X[,1], X[,2])

# Find a global best and its position
gbest <- pbest[which.min(pbest_obj),]
gbest_obj <- min(pbest_obj)


X_dir <- X %>%
  mutate(g_x = gbest[1,1],
         g_y = gbest[1,2],
         angle = atan((g_y - y)/(g_x - x))*180/pi,
         angle = ifelse(g_x < x, 180 + angle, angle))

contour_plot +
  geom_point(data = X, aes(x, y), color = "red", size = 2.5) +
  geom_arrow(data = X_dir, aes(x, y, mag = 1, angle = angle), direction = "ccw", pivot = 0, show.legend = F) +
  labs(title = "PSO", subtitle = "Iter 0")


# Update dx based on the equation shown previously
dX <- w * dX + c1*runif(1)*(pbest - X) + c2*runif(1)*(as.matrix(gbest) - X)

# Add dx to current locations
X <- X + dX

# Evaluate objective function at new positions
# Note that X[,1] is the first column i.e. x coordinates
obj <- obj_func(X[,1], X[,2])

# Find those points where the objective function is lower
# than previous iteration
idx <- which(obj >= pbest_obj)

# Update locations of local best and store local best values
pbest[idx,] <- X[idx,]
pbest_obj[idx] <- obj[idx]

# Identify the minimum value of the of the objective function
# amongst all points
idx <- which.min(pbest_obj)

# Store as global best
gbest <- pbest[idx,]
gbest_obj <- min(pbest_obj)

X_dir <- X %>%
  mutate(g_x = gbest[1,1],
         g_y = gbest[1,2],
         angle = atan((g_y - y)/(g_x - x))*180/pi, # Need angles to show direction
         angle = ifelse(g_x < x, 180 + angle, angle))

contour_plot +
  geom_point(data = X, aes(x, y), color = "red", size = 2.5) +
  geom_arrow(data = X_dir, aes(x, y, mag = 1, angle = angle), direction = "ccw", pivot = 0, show.legend = F) +
  labs(title = "PSO", subtitle = "Iter 1")














# Final function
pso_optim <- function(obj_func,  #Accept a function directly
                      c1 = 0.05,
                      c2 = 0.05,
                      w = 0.8,
                      n_particles = 20,
                      init_fact = 0.1,
                      n_iter = 50,
                      ...   # This ensures we can pass any additional
                      # parameters to the objective function
){

  x <- seq(min(x), max(x), length.out = 100)
  y <- seq(min(y), max(y), length.out = 100)

  X <- cbind(sample(x, n_particles, replace = F),
             sample(y, n_particles, replace = F))

  dX <- matrix(runif(n_particles * 2) * init_fact, ncol = 2)

  pbest <- X
  pbest_obj <- obj_func(x = X[,1], y = X[,2])

  gbest <- pbest[which.min(pbest_obj),]
  gbest_obj <- min(pbest_obj)

  loc_df <- data.frame(X, iter = 0)
  iter <- 1

  while(iter < n_iter){

    dX <- w * dX + c1*runif(1)*(pbest - X) + c2*runif(1)*t(gbest - t(X))
    X <- X + dX

    obj <- obj_func(x = X[,1], y = X[,2])

    idx <- which(obj <= pbest_obj)
    pbest[idx,] <- X[idx,]
    pbest_obj[idx] <- obj[idx]

    idx <- which.min(pbest_obj)
    gbest <- pbest[idx,]
    gbest_obj <- min(pbest_obj)

    # Update iteration
    iter <- iter + 1
    loc_df <- rbind(loc_df, data.frame(X, iter = iter))
  }

  lst <- list(X = loc_df, obj = gbest_obj, obj_loc = paste0(gbest, collapse = ","))
  return(lst)
}


# Test optimiser
out <- pso_optim(obj_func,
                 x = x,
                 y = y,
                 c1 = 0.01,
                 c2 = 0.05,
                 w = 0.5,
                 n_particles = 50,
                 init_fact = 0.1,
                 n_iter = 200)


out$obj_loc





ggplot(out$X) +
  geom_contour(data = grid, aes(x = Var1, y = Var2, z = z), color = "black") +
  geom_point(aes(X1, X2)) +
  labs(x = "X", y = "Y") +
  transition_time(iter) +
  ease_aes("linear")




















# Portfolio optimization

pacman::p_load(pso, ggplot2, dplyr, quantmod, tidyr, plotly)


# Read csv file directly into R
ticker_list <- read.csv("https://www1.nseindia.com/content/indices/ind_nifty50list.csv")
class(ticker_list)
## [1] "data.frame"

# Check if data was red in correctly
head(ticker_list[,1:3], 5)



# Note that for India, tickers need to be appended with ".NS"
tickers <- paste0(ticker_list$Symbol, ".NS")

# Pull data using quantmod::getSymbols
# Since getsymbols assigns data for each ticker to a
# separate variable, we'll use a loop to pull data for one
# ticker at a time and append to a data.frame
ticker_df <- data.frame()

pb <- txtProgressBar(min = 1, max = length(tickers), style = 3)

for(nms in tickers){
  df <- getSymbols(Symbols = nms, verbose = F, src = "yahoo", auto.assign = F)

  colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
  df <- data.frame(df)
  df$ticker <- nms
  df$date <- rownames(df)
  ticker_df <- rbind(ticker_df, df)

  setTxtProgressBar(pb, which(tickers == nms))
}

# We'll need to do some data cleaning
# Using only closing prices
prices_df <- pivot_wider(data = ticker_df, id_cols = "date", names_from = "ticker", values_from = "close")

# For simplicity, we'll remove all NAs
prices_df <- na.omit(prices_df)

# Check date range for which data is available
range(prices_df$date)
## [1] "2017-11-17" "2021-10-29"

# Check dimensions
dim(prices_df)
## [1] 973  51

# Chart to check if data has been downloaded correctly
prices_df %>%

  # Convert to long form for easy plotting with ggplot
  gather(key = "ticker", value = "price", -date) %>%

  # Attach industry
  left_join(ticker_list %>%
              mutate(ticker = paste0(Symbol, ".NS")) %>%
              select(ticker, industry = Industry),
            by = "ticker") %>%
  mutate(date = as.Date(date)) %>%

  # Showing only metals
  filter(industry == "METALS") %>%

  # Plot with ggplot
  ggplot(aes(x = date, y = price, color = ticker)) +
  geom_line(size = 0.8) +
  theme_minimal() +
  scale_color_brewer(palette = "RdBu") +
  labs(title = "Closing Prices",
       subtitle = "Nifty 50 metal stocks",
       x = "Date",
       y = "Closing Price") +
  theme(legend.position = "top",
        legend.title = element_text(colour = "transparent"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))




# Calculate daily returns
returns_df <- apply(prices_df[,-1], 2, function(vec){
  ret <- vec/lag(vec) - 1
  return(ret)
})

returns_df <- as.data.frame(returns_df)
returns_df <- returns_df[-1,]  ## Remove first row since that's NA




# Pre computing average returns and the covariance matrix
mean_returns <- sapply(returns_df, mean)
cov_mat <- cov(returns_df)

obj_func <- function(wts,
                     risk_av = 10,
                     lambda1 = 10,
                     lambda2 = 1e2,
                     ret_vec, cov_mat){

  # Some matrix multiplication
  port_returns <- ret_vec %*% wts
  port_risk <- t(wts) %*% cov_mat %*% wts

  # Objective function
  # Note that alpha is the risk aversion parameter
  # Higher the value of alpha the more conservative the portfolio
  obj <- port_returns - risk_av * port_risk

  # Full investment penalisation
  obj <- obj - lambda1 * (sum(wts) - 1)^2

  # Returning negative since the optimiser does minimisation by default
  # We need maximisation
  return(-obj)
}



# Calculate average returns and covariance matrix for 2 assets
mean_returns_small <- apply(returns_df[,1:2], 2, mean)
cov_mat_small <- cov(returns_df[,1:2])




pso_optim <- function(obj_func,
                      c1 = 0.05,
                      c2 = 0.05,
                      w = 0.8,
                      init_fact = 0.1,
                      n_particles = 20,
                      n_dim = 2,
                      n_iter = 50,
                      upper = 1,
                      lower = 0,
                      n_avg = 10,
                      ...){

  # Initialise positions
  X <- matrix(runif(n_particles * n_dim), nrow = n_particles)

  # Ensure upper and lower bounds are respected
  X <- X * (upper - lower) + lower

  # Initialise velocities
  dX <- matrix(runif(n_particles * n_dim) * init_fact, ncol = n_dim)
  dX <- dX * (upper - lower) + lower

  # Get first personal and global bests
  pbest <- X
  pbest_obj <- apply(X, 1, obj_func, ...)

  gbest <- pbest[which.min(pbest_obj),]
  gbest_obj <- min(pbest_obj)

  # Initialise an empty data frame to store results
  loc_df <- data.frame(X, iter = 0, obj = pbest_obj)
  iter <- 1

  while(iter < n_iter){

    # Find updated velocities
    dX <- w * dX + c1*runif(1)*(pbest - X) + c2*runif(1)*t(gbest - t(X))

    # Update positions
    X <- X + dX

    # Calculate objective function
    obj <- apply(X, 1, obj_func, ...)

    # Update local and global bests
    idx <- which(obj <= pbest_obj)
    pbest[idx,] <- X[idx,]
    pbest_obj[idx] <- obj[idx]

    idx <- which.min(pbest_obj)
    gbest <- pbest[idx,]
    gbest_obj <- min(pbest_obj)

    # Update iteration and store locations
    iter <- iter + 1
    loc_df <- rbind(loc_df, data.frame(X, iter = iter, obj = pbest_obj))
  }

  # Create list containing relevant items to be returned
  lst <- list(X = loc_df, obj = gbest_obj, obj_loc = gbest)
  return(lst)
}

out <- pso_optim(obj_func,
                 ret_vec = mean_returns_small,
                 cov_mat = cov_mat_small,
                 lambda1 = 10, risk_av = 100,
                 n_particles = 100,
                 n_dim = 2,
                 n_iter = 200,
                 upper = 1, lower = 0,
                 c1 = 0.02, c2 = 0.02, w = 0.05, init_fact = 0.01)

# Check if weights add to one
sum(out$obj_loc)
## [1] 1.00063


grid <- expand.grid(x = seq(0, 1, by = 0.01),
                    y = seq(0, 1, by = 0.01))

grid$obj <- apply(grid, 1, obj_func, ret_vec = mean_returns_small, cov_mat = cov_mat_small,
                  lambda1 = 10, risk_av = 100)

# Interactive 3D scatter plot with mesh
p <- plot_ly() %>%
  add_mesh(data = grid, x = ~x, y = ~y, z = ~obj, inherit = F, color = "red") %>%
  add_markers(data = out$X, x = ~X1, y = ~X2, z = ~obj, color = ~ iter, inherit = F,
              marker = list(size = 2))

htmlwidgets::saveWidget(p, "plotly.html")

# Interactive 3D scatter plot
plot_ly(out$X, x = ~X1, y = ~X2, z = ~obj) %>%
  add_markers(size = 1) %>%
  add_mesh(data = grid, x = ~x, y = ~y, z = ~obj, inherit = F)








# MULTI ASSETS

n_stocks <- ncol(returns_df)
opt <- psoptim(par = rep(0, n_stocks),
               fn = obj_func,
               ret_vec = mean_returns,
               cov_mat = cov_mat,
               lambda1 = 10, risk_av = 1000,
               lower = rep(0, n_stocks),
               upper = rep(1, n_stocks),
               control = list(maxit = 200, s = 100, maxit.stagnate = 500))

paste("Portfolio returns:", round(opt$par %*% mean_returns, 5))
## [1] "Portfolio returns: 0.00075"
paste("Portfolio Std dev:", round(sqrt(opt$par %*% cov_mat %*% opt$par), 5))
## [1] "Portfolio Std dev: 0.00986"

# Check if weights add up to one
sum(opt$par)
## [1] 0.9902846


# Benchmark portfolio
# For now let's use an equally weighted portfolio
bench_wts <- rep(1/n_stocks, n_stocks)
bench_returns <- as.matrix(returns_df) %*% t(t(bench_wts))

# Update the objective function
obj_func_TE <- function(wts,
                        risk_av = 10,
                        lambda1 = 10,
                        lambda2 = 50,
                        ret_vec, cov_mat){

  # Some matrix multiplication
  port_returns <- ret_vec %*% wts
  port_risk <- t(wts) %*% cov_mat %*% wts
  port_returns_ts <- as.matrix(returns_df) %*% t(t(wts))

  obj <- port_returns - risk_av * port_risk
  obj <- obj - lambda1 * (sum(wts) - 1)^2

  # Tracking error
  obj <- obj - lambda2 * sd(port_returns_ts - bench_returns)

  return(-obj)
}

opt <- psoptim(par = rep(0, n_stocks),
               fn = obj_func_TE,
               ret_vec = mean_returns,
               cov_mat = cov_mat,
               lambda1 = 10, risk_av = 1000,
               lower = rep(0, n_stocks),
               upper = rep(1, n_stocks),
               control = list(maxit = 200, s = 100, maxit.stagnate = 500))

paste("Portfolio returns:", round(opt$par %*% mean_returns, 5))
## [1] "Portfolio returns: 0.00074"
paste("Portfolio Std dev:", round(sqrt(opt$par %*% cov_mat %*% opt$par), 5))
## [1] "Portfolio Std dev: 0.01212"

# Check if weights add up to one
sum(opt$par)
## [1] 0.9876818





###################################
#### REDUCES EXAMPLE
pacman::p_load(pso, ggplot2, dplyr, quantmod, tidyr, plotly)

ticker_list <- read.csv("https://www1.nseindia.com/content/indices/ind_nifty50list.csv")


tickers <- paste0(ticker_list$Symbol, ".NS")
ticker_df <- data.frame()
pb <- txtProgressBar(min = 1, max = length(tickers), style = 3)
for(nms in tickers){
  df <- getSymbols(Symbols = nms, verbose = F, src = "yahoo", auto.assign = F)

  colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
  df <- data.frame(df)
  df$ticker <- nms
  df$date <- rownames(df)
  ticker_df <- rbind(ticker_df, df)

  setTxtProgressBar(pb, which(tickers == nms))
}

prices_df <- pivot_wider(data = ticker_df, id_cols = "date", names_from = "ticker", values_from = "close")
prices_df <- na.omit(prices_df)

# prices_df %>%
#   gather(key = "ticker", value = "price", -date) %>%
#   left_join(ticker_list %>%
#               mutate(ticker = paste0(Symbol, ".NS")) %>%
#               select(ticker, industry = Industry),
#             by = "ticker") %>%
#   mutate(date = as.Date(date)) %>%
#   filter(industry == "METALS") %>%
#   ggplot(aes(x = date, y = price, color = ticker)) +
#   geom_line(size = 0.8) +
#   theme_minimal() +
#   scale_color_brewer(palette = "RdBu") +
#   labs(title = "Closing Prices",
#        subtitle = "Nifty 50 metal stocks",
#        x = "Date",
#        y = "Closing Price") +
#   theme(legend.position = "top",
#         legend.title = element_text(colour = "transparent"),
#         axis.title.x = element_text(face = "bold"),
#         axis.title.y = element_text(face = "bold"))


# Calculate daily returns
returns_df <- apply(prices_df[,-1], 2, function(vec){
  ret <- vec/lag(vec) - 1
  return(ret)
})

returns_df <- as.data.frame(returns_df)
returns_df <- returns_df[-1,]

# Pre computing average returns and the covariance matrix
mean_returns <- sapply(returns_df, mean)
cov_mat <- cov(returns_df)

n_stocks <- ncol(returns_df)

# Benchmark portfolio
# For now let's use an equally weighted portfolio
bench_wts <- rep(1/n_stocks, n_stocks)
bench_returns <- as.matrix(returns_df) %*% t(t(bench_wts))

# Update the objective function
obj_func_TE <- function(wts,
                        risk_av = 10,
                        lambda1 = 10,
                        lambda2 = 50,
                        ret_vec, cov_mat){

  # Some matrix multiplication
  port_returns <- ret_vec %*% wts
  port_risk <- t(wts) %*% cov_mat %*% wts
  port_returns_ts <- as.matrix(returns_df) %*% t(t(wts))

  obj <- port_returns - risk_av * port_risk - lambda1 * (sum(wts) - 1)^2 - lambda2 * sd(port_returns_ts - bench_returns)

  return(-obj)
}

opt <- psoptim(par = rep(0, n_stocks),
               fn = obj_func_TE,
               ret_vec = mean_returns,
               cov_mat = cov_mat,
               lambda1 = 10, risk_av = 1000,
               lower = rep(0, n_stocks),
               upper = rep(1, n_stocks),
               control = list(maxit = 200, s = 100, maxit.stagnate = 500, trace=1))#trace.stats=TRUE

paste("Portfolio returns:", round(opt$par %*% mean_returns, 5))

paste("Portfolio Std dev:", round(sqrt(opt$par %*% cov_mat %*% opt$par), 5))


sum(opt$par)




