
#set.seed(1)

library(dplyr)

iter <- 5000

sol <- matrix(sample(0:10, size = 1000, prob = 11:1/sum(11:1), replace=TRUE), ncol = 10)

target <- c(1,5,8,1,3,2,5,8,6,2)

max_not_null <- 6

fitness_f <- function(sol, target){
  apply(sweep(sol,2,target), 1, function(x){sum(x^2)})
}

make_child <- function(sol, sol_fitness){
  split_point <- round(length(sol_fitness)/2)
  dad <- sol[which(sol_fitness[1:split_point] == min(sol_fitness[1:split_point]))[1],]
  mum <- sol[split_point + which(sol_fitness[(split_point+1):length(sol_fitness)] == min(sol_fitness[(split_point+1):length(sol_fitness)]))[1],]
  rand <- sample(c(TRUE,FALSE), replace=TRUE, size=10, prob=c(0.5,0.5))
  child <- dad
  child[rand] <- mum[rand]
  child <- child + sample(c(-1,0,1), replace=TRUE, size=10, prob=c(0.02,0.96,0.02))
  if(sum(child!=0)-max_not_null > 0){
    child[sample(1:length(child), size = sum(child!=0)-max_not_null, prob = abs(child)/sum(abs(child)))] <- 0
  }
  return(child)
}

save <- NULL
for(i in 1:iter){

  if(sum(!duplicated(sol))!=1){
    sol_fitness <- fitness_f(sol, target)

    #print(paste0("sol ",mean(sol_fitness)))
    child <- make_child(sol, sol_fitness)

    #print(paste0("child ",sum((child-target)^2)))

    if(sum((child-target)^2) <= max(sol_fitness)){
      sol[which(sol_fitness == max(sol_fitness))[1],] <- child
    }

    save <- rbind(save, data.frame("i"=i, "child"=sum((child-target)^2), "pool"=mean(sol_fitness)))

    sol <- sol[sample(1:nrow(sol), size = nrow(sol)),]
  }
}



sol_fitness <- fitness_f(sol, target)
sol[which(sol_fitness == min(sol_fitness))[1],]
target

sum((sol[which(sol_fitness == min(sol_fitness))[1],]-target)^2)
mean(sol_fitness)


library(plotly)

plot_ly(data = save, x=~i) %>%
  add_trace(y=~child, name="child", mode="lines") %>%
  add_trace(y=~pool, name="pool", mode="lines")



