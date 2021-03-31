# remotes::install_github("mlverse/torch")
# torch::install_torch()
library(torch)
library(dplyr)
library(RcppCNPy)
library(caret)

# hyperparameters ----
batch_size    <- 50
time_steps    <- 259
channels      <- 1
classes       <- 3
learning_rate <- 0.005
epochs        <- 50

# loading data ----
input_dir <- "notebooks/INPUT/EMOTION_DATASET/"
X <- npyLoad(file.path(input_dir, "X_3_emotion_all_augmented_1.npy"))
X <- as.data.frame(X)
y <- read.csv(file.path(input_dir, "y_3_emotion_all_augmented_1.csv"), stringsAsFactors = FALSE, header = FALSE)$V1
print(c(dim(X), "|", length(y)))

# preprocess inputs ----
pre_proc_X <- preProcess(X, method = c("center", "scale"))
X          <- predict(pre_proc_X, X)

yX              <- cbind(y, X)
yX$y            <- as.numeric(as.factor(yX$y))
yX[,2:ncol(yX)] <- apply(yX[,2:ncol(yX)], 2, as.numeric)
yX              <- as.matrix(yX)

# data loader ----
emotion_dataset <- dataset(
  name = "emotion_dataset",
  
  initialize = function(indices) {
    self$data <- self$prepare_emotion_data(indices)
  },
  
  .getitem = function(index) {
    x <- self$data[index, 2:-1]
    y <- self$data[index, 1]$to(torch_long())
    
    list(x, y)
  },
  
  .length = function() {
    self$data$size()[[1]]
  },
  
  prepare_emotion_data = function(indices) {
    torch_tensor(yX[indices,])
  }
)

# emotion_dataset()$.getitem(1)

# train test val test split ----
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
test_index  <- setdiff(1:length(y), train_index)
train_ds    <- emotion_dataset(train_index)
train_dl    <- dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)
test_ds     <- emotion_dataset(test_index)
test_dl     <- dataloader(test_ds, batch_size = batch_size, shuffle = TRUE)

# dataloader_make_iter(train_dl) %>% dataloader_next()
# enumerate(train_dl)[[1]][[1]]

# define model ----
ann <- nn_module(
  "emotion_net",
  
  initialize = function(fc1_dim, fc2_dim){
    # fc stands for "fully connected" layers
    self$fc_1 <- nn_linear(fc1_dim, 32)
    self$fc_2 <- nn_linear(32, fc2_dim)
  },
  
  forward = function(x){
    x %>% self$fc_1() %>%
      nnf_relu() %>%
      self$fc_2()
  }
)

# build model ----
model <- ann(
  time_steps,
  classes
)

device <- if (cuda_is_available()) torch_device("cuda:0") else "cpu"
model  <- model$to(device = device)
optimizer <- optim_adam(model$parameters, lr = learning_rate)
criterion <- nn_cross_entropy_loss()

# train model ----
for (epoch in 1:epochs) {
  model$train()
  train_losses <- c()  
  
  coro::loop(for (b in train_dl) {
    optimizer$zero_grad()
    output <- model(b[[1]])
    loss <- criterion(output, b[[2]])
    loss$backward()
    optimizer$step()
    train_losses <- c(train_losses, loss$item())
  })
  
  model$eval()
  test_losses <- c()
  
  coro::loop(for (b in test_dl) {
    output <- model(b[[1]])
    loss <- criterion(output, b[[2]])
    test_losses <- c(test_losses, loss$item())
  })
  
  cat(sprintf("Loss at epoch %d: training: %3f, test: %3f\n", epoch, mean(train_losses), mean(test_losses)))
}

plot(train_losses)
plot(test_losses)
