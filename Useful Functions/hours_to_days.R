#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### tsibble | Hours to Days Function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

hours_to_days <- function(df, ymd_name)
{
  # Obtain year-month-day
  ymd <- df[,ymd_name]
  
  # Remove ymd variable from df
  df <- df[,-which(colnames(df) == ymd_name)]
  
  # Split string
  splits <- strsplit(
    ymd, split = "-"
  )
  
  # Split spaces as well
  splits <- lapply(splits, function(x){
    unlist(strsplit(x, split = " "))
  })
  
  # Split spaces as well
  splits <-  t(simplify2array(splits))
  
  # Obtain days
  days <- unique(splits[,3])
  
  # Obtain months
  months <- unique(splits[,2])
  
  # Obtain years
  years <- unique(splits[,1])
  
  # Obtain new values
  new_values <- matrix(
    NA,
    nrow = length(days) *
      length(months) * 
      length(years),
    ncol = ncol(df) + 3
  )
  
  # Initialize count
  count <- 0
  
  # Loop through years and months
  for(i in 1:length(years))
    for(j in 1:length(months))
      for(k in 1:length(days)){
      
      # Increase count
      count <- count + 1
      
      new_values[count,] <- c(
        years[i],
        months[j],
        days[k],
        colMeans(
          df[
            which(
              splits[,1] == years[i] & 
                splits[,2] == months[j] &
                splits[,3] == days[k]
            ),
          ], na.rm = TRUE
        )
      )
      
      
    }
  
  # Remove missing variables
  new_values <- ifelse(new_values == "NaN", NA, new_values)
  new_values <- na.omit(new_values)
  
  # Reorder by year and month
  ordered_values <- new_values[order(
    new_values[,1],
    new_values[,2],
    new_values[,3]
  ),]
  
  # Combine year and month
  ordered_values[,1] <- apply(ordered_values[,c(1,2,3)], 1, function(x){
    paste(
      x[1], x[2], x[3], sep = "-", collapse = ""
    )
  })
  
  # Remove second column
  ordered_values <- ordered_values[,-c(2, 3)]
  
  # Make data frame
  final_df <- as.data.frame(ordered_values)
  
  # Make numeric
  final_df[,-1] <- apply(final_df[,-1], 2, as.numeric)
  
  # Make year-month
  final_df[,1] <- lubridate::ymd(final_df[,1])
  
  # Add column names
  colnames(final_df) <- c(
    "Day", colnames(df)
  )
  
  # Convert to tsibble
  final_ts <- as_tsibble(final_df)
  
  # Return
  return(final_ts)
  
}
