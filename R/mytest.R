
myfirstpack <- function(x,y)
  {
g = t.test(x,y)
h = c("The means  are " ,g$estimate,"The standard deviation of group X is" ,sd(x), "The standard deviation of Y is",sd(y)  ,"The upper and lower bound of confidence intervals are ", g$conf.int,"The p value is ",g$p.value)
return(h)
}

