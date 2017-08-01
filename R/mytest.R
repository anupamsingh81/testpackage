
myfirstpack <- function(x,y)
  {
g = t.test(x,y)
a = sd(x)
b = sd(y)
h = c("The means  are " ,g$estimate,"The standard deviation of group X is" ,a, "The standard deviation of Y is",b ,"The upper and lower bound of confidence intervals are ", g$conf.int,"The p value is ",g$p.value)
return(h)
}

Bfkay<-function(sd, obtained, uniform, lower=0, upper=1, meanoftheory=0, sdtheory=1, tail=2)
{
#Authors Danny Kaye & Thom Baguley
#Version 1.0
#19/10/2009
#test data can be found starting at p100
#
	area <- 0
	if(identical(uniform, 1)){
		theta <- lower
		range <- upper - lower
		incr <- range / 2000
		for (A in -1000:1000){
			theta <- theta + incr
			dist_theta <- 1 / range
			height <- dist_theta * dnorm(obtained, theta, sd)
			area <- area + height * incr
		}
	}else{
		theta <- meanoftheory - 5 * sdtheory
		incr <- sdtheory / 200
		for (A in -1000:1000){
			theta <- theta + incr
			dist_theta <- dnorm(theta, meanoftheory, sdtheory)
			if(identical(tail, 1)){
				if (theta <= 0){
					dist_theta <- 0
				} else {
					dist_theta <- dist_theta * 2
				}
			}
			height <- dist_theta * dnorm(obtained, theta, sd)
			area <- area + height * incr
		}
	}
	LikelihoodTheory <- area
	Likelihoodnull <- dnorm(obtained, 0, sd)
	BayesFactor <- LikelihoodTheory / Likelihoodnull
	ret <- list("LikelihoodTheory" = LikelihoodTheory, "Likelihoodnull" = Likelihoodnull, "BayesFactor" = BayesFactor)
	ret
}


retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000){
z <- qt(1-alpha/2, df) 
p.hi <- 1 - pt(z-A/s, df) 
p.lo <- pt(-z-A/s, df)  
power <- p.hi + p.lo
typeS <- p.lo/power
estimate <- A + s*rt(n.sims,df)
significant <- abs(estimate) > s*z
exaggeration <- mean(abs(estimate)[significant])/A
return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}

retropower<- function(A, s)
  {
g = retrodesign(A, s, alpha=.05, df=Inf, n.sims=10000)

h = c("The type S error is " ,g$typeS,"The power is" ,g$power, "The exaggeration factor(M error) is",g$exaggeration )
return(h)
}
randomplot <- function(n, dist=c("normal", "uniform")){
  #input validation
  dist <- match.arg(dist)
  stopifnot(n < 1e6)
  
  if(dist == "normal"){
    hist(rnorm(n))
  }
  
  if(dist == "uniform"){
    hist(runif(n))
  }
  
  #return nothing
  invisible();  
}
