# Daniel Eubanks
# May 2019
# WorksOfR.wordpress.com

# Activation function ---------------------------------------------------------

f <- function(x, name = "tanh") {
    if (name == "tanh") {
        (exp(x) - exp(-x))/(exp(x) + exp(-x))
    } else if (name == "logistic") {
        1/(1 + exp(-x))
    } else if (name == "ReLU") {
        pmax(x, 0)
    }
    else {
        stop("Activation function not recognized.")
    }
}

# Derivative of activation function -------------------------------------------

fprime <- function(x, name = "tanh") {
    if (name == "tanh") {
        1 - f(x, name = "tanh")^2
    } else if (name == "logistic") {
        f(x, name = "logistic") * (1 - f(x, name = "logistic"))
    } else if (name == "ReLU") {
        ifelse(x < 0, 0, 1)
    }
    else {
        stop("Activation function not recognized.")
    } 
}

# Function to fit neural network ----------------------------------------------

fit_nn <- function(x, y, arch, act_fun = "tanh", alpha = 0.1, tol = 0.001, lambda = 0.01) {
    
    # Get architecture and number of hidden layers L
    N <- nrow(X)
    L <- length(arch)
    nd_0 <- ncol(X)
    arch <- c(nd_0, arch, 1)
    
    # List to store W matrices; initialize with random numbers
    W <- list()
    for (l in 1:(L+1)) {
        W[[l]] <- matrix(rnorm((arch[l] + 1) * arch[l + 1]), nrow = arch[l] + 1)
    }
    
    # Intialize lists to store components
    A <- list()
    Z <- list()
    d <- list()
    dJ_dW <- list()
    
    # Set initial eps and E_0
    eps <- Inf
    E_0 <- 10e+10
    max_iter <- 1000
    iter <- 0
    
    while (E_0 > tol & iter < max_iter) {
      
        # Forward propagation -------------------------------------------------
        
        # Input layer; add bias
        A[[1]] <- cbind(1, X)
        
        # Feed forward; add bias at each layer
        for (l in 2:(L+1)) {
            Z[[l]] <- A[[l-1]] %*% W[[l-1]]
            A[[l]] <- f(Z[[l]], name = act_fun)
            A[[l]] <- cbind(1, A[[l]])
        }
        
        # Output layer; don't add bias
        Z[[L+2]] <- A[[L+1]] %*% W[[L+1]]
        A[[L+2]] <- f(Z[[L+2]], name = act_fun)
        y_out <- A[[L+2]]
        
        # Backward propagation ------------------------------------------------
        
        # Error after forward propagation
        E_1 <- 1/length(y) * sum(1/2 * (y - y_out)^2)
        
        # Calculate deltas
        
        # Output layer and layer just before output layer
        d[[L+2]] <- -(y - y_out) * fprime(Z[[L+2]], name = act_fun)
        d[[L+1]] <- (d[[L+2]] %*% t(W[[L+1]])) * fprime(cbind(1, Z[[L+1]]), name = act_fun)
        
        # Hidden layers; don't propagate bias backwards
        if ( L > 1) {
            for (l in L:2) {
                d[[l]] <- (d[[l+1]][,-1] %*% t(W[[l]])) * fprime(cbind(1, Z[[l]]), name = act_fun)
            }
        }
        
        # Calculate partial derivatives
        
        # Output layer
        dJ_dW[[L+1]] <- t(A[[L+1]]) %*% d[[L+2]] - lambda * W[[L+1]]
        
        # Hidden layers; don't propagate bias backwards
        for(l in L:1) {
            dJ_dW[[l]] <- t(A[[l]]) %*% d[[l+1]][,-1] - lambda*W[[l]]
        }
        
        
        # If error decreases, update weights and increase alpha; otherwise, reduce alpha
        W_new <- list()
        for (l in 1:(L+1)) {
            W_new[[l]] <- W[[l]] - alpha * 1/N * dJ_dW[[l]]
        }
        
        if(E_1 < E_0) {
            W <- W_new
            alpha <- 1.02 * alpha
            E_0 <- E_1
        } else {
            alpha <- 0.8 * alpha
        }
        
        print(E_0)
        
        iter <- iter + 1
        
        # If alpha is very close to zero, terminate
        if (alpha < 1e-10) {
            break
        }
          
    }
    
    output <- list(pred = y_out,
                   W = W)
    return(output)
    
}
