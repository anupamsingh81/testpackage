
myfirstpack <- function(x,y)
  {
g = t.test(x,y)
a = sd(x)
b = sd(y)
h = c("The means  are " ,g$estimate,"The standard deviation of group X is" ,a, "The standard deviation of Y is",b ,"The upper and lower bound of confidence intervals are ", g$conf.int,"The p value is ",g$p.value)
return(h)
}

