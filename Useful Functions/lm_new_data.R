#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### New Data | Linear Model Function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

lm_new_data <- function(
    model, # input model
    df, # input data
    h, # number of forecasts
    type = c( # calculation of values
      "random", "window", "arima"
    ),
    iterations = 10, # number of iterations
    window_width = 4, # moving average width (window only)
    seed = 1234 # set seed from random data 
)
{
  
  # Obtain times
  times <- tsibble::new_data(df, h)
  
  # Obtain outcome variable
  outcome <- as.character(model$tslm[[1]]$response[[1]])
  
  # Obtain coefficient variables
  coefficients <- row.names(fit$tslm[[1]]$fit$coefficients)[-1]
  
  # Coefficient matrix
  coefficient_matrix <- as.matrix(df[,coefficients])
  
  # Initialize new data
  new_matrix <- matrix(
    NA, nrow = h,
    ncol = ncol(coefficient_matrix)
  )
  colnames(new_matrix) <- colnames(coefficient_matrix)
  row.names(new_matrix) <- 1:nrow(new_matrix)
  
  # Set seed
  set.seed(seed)
  
  # Produce values
  if(type == "random"){
    
    # Random values based on frequencies
    for(i in 1:h){
      
      # Populate new data
      new_matrix[i,] <- rowMeans(
        replicate(
          n = iterations,
          {
            apply(coefficient_matrix, 2, function(x){
              
              # Obtain frequencies
              frequency <- table(x)
              
              # Sample from options
              sample(
                as.numeric(names(frequency)),
                1, prob = frequency / length(x)
              )
            })
          }
        ), na.rm = TRUE
      )
      
    }
    
  }else if(type == "window"){
    
    # Values based on last four values
    for(i in 1:h){
      
      # Obtain last values in window
      last_values <- coefficient_matrix[
        (nrow(coefficient_matrix) - h - window_width + 1):(nrow(coefficient_matrix) - h),
      ]
      
      # Populate new data
      new_matrix[i,] <- colMeans(last_values, na.rm = TRUE)
      
      # Update range of coefficients
      coefficient_matrix <- rbind(coefficient_matrix, new_matrix[i,])
      
    }
    
  }else if(type == "arima"){
    
    # Values based on ARIMA
    for(i in 1:ncol(coefficient_matrix)){
      
      # Select data
      select_df <- df[,c(
        "time",
        colnames(coefficient_matrix)[i]
      )]
      
      # Obtain model
      arima_fit <- select_df %>%
        fabletools::model(
          fable::ARIMA()
        )
      
      # Obtain forecasts
      arima_forecast <- arima_fit %>% 
        fabletools::forecast(h = h)
      
      # Populate new data
      new_matrix[,i] <- arima_forecast$.mean
      
    }
    
  }
  
  # Remove random seed
  set.seed(NULL)
  
  # Convert new data to data frame
  new_df <- cbind.data.frame(
    times, new_matrix
  )
  
  # Convert to `tsibble`
  return(tsibble::as_tsibble(
    new_df,
    index = names(new_df)[1]
  ))
  
}
