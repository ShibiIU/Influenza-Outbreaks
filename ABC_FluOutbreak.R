### Approximate Bayesian Computation for disearse outbreaks
### Shibi He, 10/2019


### Load the data:

A1 = matrix(c(66,87,25,22,4,13,14,15,9,4,0,4,4,9,1,0,0,4,3,1,0,0,0,1,1,0,0,0,0,0), nrow=6, ncol=5, byrow=TRUE)

A2 = matrix(c(44,62,47,38,9,10,13,8,11,5,0,9,2,7,3,0,0,3,5,1,0,0,0,1,0,0,0,0,0,1), nrow=6, ncol=5, byrow=TRUE)

B1 = matrix(c(9,12,18,9,4,1,6,6,4,3,0,2,3,4,0,0,0,1,3,2,0,0,0,0,0,0,0,0,0,0), nrow=6, ncol=5, byrow=TRUE)

B2 = matrix(c(15,12,4,11,17,4,0,21,4,0,0,5), nrow=4, ncol=3, byrow=TRUE)

# Compute the probability of infection
A1 = sweep(A1, 2, colSums(A1), "/")
A2 = sweep(A2, 2, colSums(A2), "/")
B1 = sweep(B1, 2, colSums(B1), "/")
B2 = sweep(B2, 2, colSums(B2), "/")


### ABC SMC Step 1: sample parameters from a prior distribution (population 1)

set.seed(610)


generate_abc_sample = function(observed_data,
                               prior_distribution,
                               data_generating_function,
                               distance,
                               epsilon) {
    while(TRUE) {
        theta = prior_distribution()
        y = data_generating_function(observed_data, theta[1], theta[2])
        if(distance(y, observed_data) < epsilon) {
            return(theta)
        }
    }
}


prior_distribution = function() runif(2)


# data generating function
data_generating_function = function(observed_data, qc, qh){
    Nrow = nrow(observed_data)
    Ncol = ncol(observed_data)
    w = matrix(rep(0, Nrow*Ncol), nrow = Nrow, ncol=Ncol)
    for (s in 1:Ncol){
        for (i in 1:Nrow){
            j = i-1
            if (j > s){
                w[i,s] = 0
            } else if (i == 1){
                w[i,s] = qc^s
            } else if (j < s) {
                w[i,s] = choose(s,j) * w[i,j] * (qc * qh^j)^(s-j)
            } else {
                w[i,s] = 1- (colSums(w)[s]-w[i,s])
            } 
        }
        
    }
    return(w)
}




distance = function(matrix, data){
    dist = sqrt(sum((matrix - data)^2))
    return(dist)
}



### ABC SMC Step 2: write function to generate intermediate distributions
# (population 2 to T-1)

generate_abc_sample2 = function(observed_data, 
                                population,
                                data_generating_function,
                                distance,
                                epsilon) {
    while(TRUE) {
        theta = population[, sample(ncol(population), 1, replace=TRUE)]
        theta[1] = rnorm(1, mean=theta[1], sd=0.01)  # add perturbation
        theta[2] = rnorm(1, mean=theta[2], sd=0.01)
        y = data_generating_function(observed_data, theta[1], theta[2])
        if(distance(y, observed_data) < epsilon) {
            return(theta)
        }
    }
}



### ABC SMC Step 3: generate the final population T

ABC_SMC_sample = function(observed_data){
    epsilon = seq(1, 0.32, -0.04)
    N = 5000
    for (i in 1: length(epsilon)){
        if (i == 1){
            posterior_samples = replicate(n=N, generate_abc_sample(
                observed_data, 
                prior_distribution, 
                data_generating_function, 
                distance, epsilon[i]))
            
        } else{
            posterior_samples = replicate(n=N, generate_abc_sample2(
                observed_data, 
                population = posterior_samples,
                data_generating_function, 
                distance, epsilon[i]))
        }
        
    }
    return(posterior_samples)
}



q_A1 =ABC_SMC_sample(A1)
q_A2 =ABC_SMC_sample(A2)
q_B1 =ABC_SMC_sample(B1)
q_B2 =ABC_SMC_sample(B2)



### Plot the distribution

plot(x = q_A1[2, ], y = q_A1[1, ], type = "p", col = "red", xlim = c(0,1), ylim = c(0,1), xlab = "qh", ylab = "qc")
points(x = q_A2[2, ], y = q_A2[1, ], type = "p", col = "blue", xlim = c(0,1), ylim = c(0,1))

plot(x = q_B1[2, ], y = q_B1[1, ], type = "p", col = "red", xlim = c(0,1), ylim = c(0,1), xlab = "qh", ylab = "qc")
points(x = q_B2[2, ], y = q_B2[1, ], type = "p", col = "blue", xlim = c(0,1), ylim = c(0,1))








