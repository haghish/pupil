
# Load the required resources
# ============================================================
library(h2o)        # machine learning framework
library(splitTools) # for splitting the data
library(dplyr)      # for adding lag in the data

# Load the data
# ============================================================
df <- read.csv("//kant/sv-psi-u1/haghish/pc/Downloads/frame_values.csv")
df <- na.omit(df)

# initiate the Java server
# ============================================================
h2o.init(ignore_config =TRUE)


# defining the loop variables and results
# ============================================================
lag <- 0:60
R2 <- NULL
RMSE <- NULL


for (i in lag) {
  
  # clean the server
  h2o.removeAll()
  
  # Add lag in the data (from 0 frame to 60 lagged frames)
  # ------------------------------------------------------------
  print(paste("lag =", i))
  #data <- df %>%                            # Add lagged column
  #  dplyr::mutate(pupilg = dplyr::lag(raw_pupil, n = -i, default = NA)) %>% 
  #  na.omit(as.data.frame())
  
  data <- df %>%                            # Add lagged column
    dplyr::mutate(pupil = dplyr::lead(raw_pupil, n = i, default = NA)) %>% 
    na.omit(as.data.frame())
  
  # Partition the data, giving 90% for testing
  # ------------------------------------------------------------
  data$raw_pupil <- NULL
  inds <- partition(data[, "pupil"], p = c(train = .9, test = .1), seed = 2022)
  test <- data[inds$test, ]
  train<- data[inds$train, ]
  
  # upload the data to the Java server
  train.hex <- as.h2o(train)
  test.hex <- as.h2o(test)
  
  # develop the model
  # ------------------------------------------------------------
  y <- "pupil"
  x <- setdiff(colnames(data), y)
  fit <- h2o.automl(training_frame = train.hex,
                    x = x, y = y,
                    include_algos = c("GLM"))
  
  # test the model on the test dataset
  # ------------------------------------------------------------
  performance <- h2o.performance(model = fit@leader, newdata = train.hex)
  R2 <- c(R2, performance@metrics$r2)
  RMSE <- c(RMSE, performance@metrics$RMSE)
  
  plotdata <<- as.data.frame(cbind(lag[1:i], R2, RMSE))
  plot(x = plotdata$V1, y = plotdata$RMSE)
  Sys.sleep(2)
}

# Plot the data
# ============================================================
# plotdata <- as.data.frame(cbind(lag[1:44], R2, RMSE))
# plot(x = plotdata$V1, y = plotdata$R2)


