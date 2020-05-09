# STDM Coursework - Artificial Neural Networks
# Rebecca Shannon

rm(list=ls())

library(rgdal)
library(nnet)

# Advantage of ANN is the ability to model non-linear data. No need to use differencing to make
# the time series stationary.

# Temporal prediction - predict TT values for the final 7 days based on the first 23 days.

# Data is arranged into two groups: training and testing. 
# - 77% data will train the ANN
# - 23% data will evaulate the ANN performance

tt.data <- avg_travel_time_date_L
rownames(tt.data) <- tt.data$Date
tt.data <- tt.data[, -1]
tt.data <- as.matrix(tt.data)

# Construct a multi-input multi-output ANN to predict the coming day's ATT value (d + 1) from the ATT value of a given day (d)
# Use the previous day's ATT for all links to predict the following day's ATT for all links.

tt.data_y <- as.matrix(tt.data[-1,])    # Matrix tt.data missing the first day

# decay parameter = weight decay
# linout parameter switches to linear output units (as opposed to logistic)
# size parameter = number of neurons in the hidden layer
# maxit = maximum number of iterations. 100 is sufficient.
tt.data.nnet <- nnet(tt.data[1:22, 1:25], tt.data_y[1:22, 1:25], decay = 5e-6, linout = TRUE, size = 6)

# Make a prediction using the trained model for all links for the last 7 days
tt.data.pred <- predict(tt.data.nnet, tt.data_y[23:29, 1:25])

# Plot modelled vs predicted for first link
matplot(cbind(tt.data_y[23:29,1], tt.data.pred[,3]), type="l")

# Few nodes in the hidden layer. Also using a single time-step to predict the next time
# step. This means the model has limited info on the spatio-temporal evolution of the
# series. This has a smoothing effect on the model, eg. some peaks/troughts underestimated.


