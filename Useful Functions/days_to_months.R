#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### tsibble | Days to Months Function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

days_to_months <- function(df, ymd_name)
{
  # Obtain year-month-day
  ymd <- df[,ymd_name]
  
  # Remove ymd variable from df
  df <- df[,-which(colnames(df) == ymd_name)]
  
  # Check for conversion need
  if(is(ymd, "Date")){
    ymd <- as.character(ymd)
  }else if(is(ymd, "tbl_ts")){
    ymd <- as.character(
      as.matrix(
        as.data.frame(ymd)
      )
    )
  }
  
  # Split string
  splits <- t(simplify2array(strsplit(
    ymd, split = "-"
  )))
  
  # Obtain months
  months <- unique(splits[,2])
  
  # Obtain years
  years <- unique(splits[,1])
  
  # Obtain new values
  new_values <- matrix(
    NA,
    nrow = length(months) * length(years),
    ncol = ncol(df) + 2
  )
  
  # Initialize count
  count <- 0
  
  # Loop through years and months
  for(i in 1:length(years))
    for(j in 1:length(months)){
      
      # Increase count
      count <- count + 1
      
      new_values[count,] <- c(
        years[i],
        months[j],
        colMeans(
          df[
            which(
              splits[,1] == years[i] & 
                splits[,2] == months[j]
            ),
          ]
        )
      )
      
      
    }
  
  # Remove missing variables
  new_values <- ifelse(new_values == "NaN", NA, new_values)
  new_values <- na.omit(new_values)
  
  # Reorder by year and month
  ordered_values <- new_values[order(
    new_values[,1],
    new_values[,2]
  ),]
  
  # Ensure matrix
  if(is(ordered_values, "matrix")){
    # Combine year and month
    ordered_values[,1] <- apply(ordered_values[,c(1,2)], 1, function(x){
      paste(
        x[1], x[2], sep = "-", collapse = ""
      )
    })
    
    # Remove second column
    ordered_values <- ordered_values[,-2]
    
  }else{
    
    # Combine year and month
    ordered_values[1] <- paste(
      ordered_values[1], ordered_values[2],
      sep = "-", collapse = ""
    )
    
    # Remove second column
    ordered_values <- ordered_values[-2]
    
    # Ensure matrix
    if(!is(ordered_values, "matrix")){
      ordered_values <- matrix(
        ordered_values, nrow = 1, byrow = TRUE
      )
    }
  }
  
  # Make data frame
  final_df <- as.data.frame(ordered_values)
  
  # Make numeric
  final_df[,-1] <- apply(final_df[,-1], 2, as.numeric)
  
  # Make year-month
  final_df[,1] <- tsibble::yearmonth(final_df[,1])
  
  # Add column names
  colnames(final_df) <- c(
    "Month", colnames(df)
  )
  
  # Convert to tsibble
  final_ts <- as_tsibble(final_df)
  
  # Return
  return(final_ts)
  
}
