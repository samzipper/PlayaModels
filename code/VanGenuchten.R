## VanGenuchten.R
#' Functions for processing Van Genuchten data

VG_ThetaFromHead <- function(head, ThetaR, ThetaS, alpha, n){
  # This is intended to take ThetaS [m3 m-3], ThetaR [m3 m-3], alpha [m-1], and
  # n [-] values and produce theta estimates for a vector of head [m].

  theta <- ThetaR + (ThetaS - ThetaR)/((1+(alpha*abs(head))^n)^(1-1/n))
  theta[head >= 0] <- ThetaS
  
  return(theta)
}

VG_HeadFromTheta <- function(theta, ThetaR, ThetaS, alpha, n){
  # This is intended to take ThetaS [m3 m-3], ThetaR [m3 m-3], alpha [m-1], and
  # n [-] values and produce head [m] estimates for a vector of theta [m3 m-3].
  -((((ThetaS-ThetaR)/(theta-ThetaR))^(1/(1-1/n))-1)^(1/n))/alpha
  
}

#rm(list=ls())
#alpha <- 1.56
#n <- 3.60
#ThetaR <- 0.078
#ThetaS <- 0.43
#head <- c(0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 1, 3, 5, 10, 50, 100, 500, 1000, 5000)
#head <- c(40.2497599989674, 36.6573406183674, 33.4022904555746, 30.4230990166042, 
#  27.7106781404251, 25.2507904635931, 22.9998628565528, 20.9470517819242, 
#  19.0847605495341, 17.386505399122, 15.8370096395701, 14.4264372756156, 
#  13.142778775173, 11.971969065725, 10.9048387967942, 9.93453288879857, 
#  9.04997414254467, 8.24367844320889, 7.50928358189691, 6.84054384754715, 
#  6.23157764343821, 5.67657569973449, 5.17083247674299, 4.71032884715827)



# Make a plot
#require(ggplot2)
#p.theta <-
#  ggplot(data.frame(head=head, theta=theta), aes(x=head, y=theta)) +
#  geom_point() +
#  scale_x_log10()