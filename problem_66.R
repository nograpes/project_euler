# Diophantine equation
# Specifically, Pell's equation.
# Langrange showed that as long as n is not a square, there exist infinite solutions.
# Brahmagupta studied in 628 CE.
# But Bhaskara offered the first complete solution, the 
# chakravala method (चक्रवाल विध)
chakravala<- function(n) {
  stopifnot(sqrt(n)%%1!=0)
  current_p = current_x = optimal_p = round(sqrt(n))
  current_k = (current_p^2) - n
  current_y<-1
  
  if (current_k == 1 ) return (c(current_x, current_y))  # simplest answer was right!
  while(TRUE){
    current_k_abs   = abs(current_k)
    diff            = (current_p + optimal_p) %% current_k_abs
    next_p_low      = optimal_p - diff
    next_p_high     = next_p_low + current_k_abs

    if (next_p_low < 1) {
      next_p = next_p_high
    } else {
      next_p = ifelse(abs((next_p_low^2) - n) < abs((next_p_high^2) - n),
                      next_p_low,
                      next_p_high)
    }
    next_k = ((next_p^2) - n) / current_k
    next_x = ((next_p * current_x) + (n * current_y)) / current_k_abs
    next_y = ((next_p * current_y) + current_x) / current_k_abs
    
    if (next_k==1) return (c(next_x,next_y))

    current_p = next_p
    current_k = next_k
    current_x = next_x
    current_y = next_y
  }
}

ints<-1:1000
non.squares<-ints[!(ints %in% ints^2)]
non.squares[which.max(sapply(non.squares,chakravala)[1,])]

data.frame(matrix(1:9,ncol=3))
