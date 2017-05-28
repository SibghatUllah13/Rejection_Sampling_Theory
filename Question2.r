


# Functions ---------------------------------------------------------------

q=function(x){
  dbeta(x,3,2)
}
draw_from_q=function(n){
  rbeta(n,3,2)
}
f=function(x){
  dbeta(x,4,6)
}
# Acceptance Rejection ----------------------------------------------------
AR=function(dtarget,dauxiliary,rauxiliary,k){
  
  count=0
  E=0
  
  while(E==0){
    candidate = rauxiliary(1)
    acc_prob=dtarget(candidate)/(k*dauxiliary(candidate))
    E = sample(c(1,0),prob=c(acc_prob, 1-acc_prob),size=1)
    count=count+1
  }
  
  return(list(draw=candidate,computational_effort=count))
  
}
# Perform Simulation ------------------------------------------------------
mcsize=1000
draw_vec=rep(NA,mcsize)
effort_vec=rep(NA,mcsize)

for(i in 1:mcsize){
  
  DD=AR(dtarget=f,dauxiliary=q,rauxiliary=draw_from_q,k=4)
  draw_vec[i] = DD$draw
  effort_vec[i] = DD$computational_effort
  
}

# Results -----------------------------------------------------------------
hist(draw_vec,freq=FALSE,col='orchid')
curve(f(x),add=TRUE,lwd=4, col='blue')
