
myfirstpack <- function(x,y)
  {
g = t.test(x,y)
h = list("The means are " ,g$estimate,"The upper and lower bound of confidence intervals are ", g$conf.int,"The p value is ",g$p.value)
h
}

