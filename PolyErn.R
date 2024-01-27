# ---
#   title: "PolyErn"
# author: "Tushar Chitnavis"
# date: "2023-09-23"
# output: html_document
# ---
#   
#   ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```
# 
# -----------------------------------------------------------------------------------------------------
#   
#   Generalized urn simulation with R: The initial composition of colours in the urn is 20
# black, 10 white, 7 red, and 7 green. When a ball of a certain colour is picked, we remove
# another ball of the same colour from the urn. For example, when a black ball is picked
# for the first draw, we remove another black ball before we start the second draw, so that
# there are 2 black balls removed in total.
# (a) Simulate the composition of colours in each draw for 20 trials. R coding only
# (b) Repeat this experiment 3 more times (with different set.seed). R coding only
# (c) Visualize the results of the above 4 experiments in a 2 by 2 plot. In each pot,
# visualize the time series of the number of balls of each colour.
# 
# ```{r}

#This function simulates the Polya urn process for a given initial composition (init), a decrement factor (lambda), and a specified number of trials (n_trials).

polya_urn <- function(init, lambda, n_trials){
  
  res <- matrix(NA, nrow = n_trials, ncol = length(init))
  res[1, ] <- init #first composition we give it 
  
  for(i in 2:n_trials){
    
    #for things after the first composition
    
    interval <- cumsum(res[i-1, ])/sum(res[i-1, ]) #Calculate the cumulative probabilities of drawing each color. This represents the proportion of each item in the urn.runif(1): Generate a random number from a uniform distribution between 0 and 1.
    
    draw <- runif(1)
    bin_asign <- c(interval > draw) * 1  #Create a binary vector indicating which bin is selected based on whether the cumulative probability is greater than the random draw. The multiplication by 1 converts TRUE/FALSE to 1/0.
    
    bin_select <- c() #empty 
    
    for(j in 1:length(bin_asign)){
      
      if(bin_asign[j] == 1){
        #that means interval > draw 
        bin_select <- j #this was chosen 
        break #Exit the loop as soon as a bin is selected
      }
    }
    
    res[i, ] <- res[i-1, ] #update results matrix
    
    if(res[i, bin_select] - lambda == -1 ){
      res[i, bin_select] <- 0 #added restriction to draw only one ball, if only last ball available
    }
    else {
      res[i, bin_select] <- res[i, bin_select] - lambda
    }
  }
  return(res)
}

#The function is called with different initial compositions and random seeds, resulting in four different simulations (polya_result_1, polya_result_2, polya_result_3, 

polya_result_1 <- polya_urn(c(20,10,7,7), 2, 20)
print(polya_result_1)

set.seed(200)
polya_result_2 <- polya_urn(c(20,10,7,7), 2, 20)
print(polya_result_2)

set.seed(300)
polya_result_3 <- polya_urn(c(20,10,7,7), 2, 20)
print(polya_result_3)

set.seed(400)
polya_result_4 <- polya_urn(c(20,10,7,7), 2, 20)
print(polya_result_4)

#The results of the four simulations are stored in a list for further analysis.

list_res <- list(polya_result_1, polya_result_2, polya_result_3, polya_result_4)

color_lists <- c('black','gray','red','green')

#This loop generates plots for each simulation, showing the evolution of the urn's composition over time. Different colors are used for different types of items. Used Gary to denote White ball.

for(i in 1:length(list_res)){
  
  y_min <- min(list_res[[i]])
  y_max <- max(list_res[[i]])
  
  
  plot(1, type = "n", ylim = c(y_min, y_max), xlim = c(1, 20), xlab = "Time", ylab = "# of occurances")
  
  for(j in 1:4){
    lines(1:20, list_res[[i]][,j], col = color_lists[j], lwd = 1.5)
  }
}
# ```
# 
# -----------------------------------------------------------------------------------------------------
# 
#   
  