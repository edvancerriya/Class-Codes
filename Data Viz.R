## ----For Detailed Comments refer to reading material-------------------------------------------------------
library(ggplot2)



p=ggplot(mtcars, aes(x=mpg,y=wt))




## ------------------------------------------------------------------------
p + geom_point()

## --------------------------------------------------------------
p+geom_line()

p+geom_rect()



#geom_rect makes rectangles.

p+geom_boxplot()


library(vcd)
data("Arthritis")

d=Arthritis
str(d)

ggplot(d,aes(x=Treatment,y=Age))+geom_boxplot()



## ------------------------------------------------------------------------
p = ggplot(mtcars,aes(x=wt,y=mpg))

p+geom_point()+geom_line()+geom_smooth()

p+geom_smooth()


## ------------------------------------------------------------------------
p = ggplot(mtcars,aes(x=wt,y=mpg,color=vs))

p+geom_point()






str(mtcars)

## ------------------------------------------------------------------------

mtcars$vs=as.factor(mtcars$vs)

p=ggplot(mtcars,aes(x=wt,y=mpg,color=vs))

p+geom_point()


## ------------------------------------------------------------------------
mtcars$am=as.factor(mtcars$am)

p=ggplot(mtcars,aes(x=wt,y=mpg,color=vs,shape=am,size=cyl))


p+geom_point()





##shape can not be mapped to a continuous numeric variable.

## -----------------------------------------------------------



p=ggplot(mtcars,aes(x=wt,y=mpg,color=vs))

p+geom_point()+geom_line()+geom_smooth()


p+geom_point() + coord_polar(theta = "x")

## -----------------------------------------------------------
p=ggplot(mtcars,aes(x=wt,y=mpg))


p+geom_point(aes(color=vs))+geom_smooth()+geom_line()

###
ggplot(mtcars,aes(x=wt,y=mpg,color=am,size=cyl))+geom_point()


###



## ----------------------------------
library(vcd)
head(Arthritis)

## ------------------------------------------------------------------------
ggplot(Arthritis, aes(x= Improved)) + geom_bar()

## ------------------------------------------------------------------------
ggplot(Arthritis, aes(x=Improved)) + geom_bar(aes(color=Treatment))

ggplot(Arthritis, aes(x=Improved)) + geom_bar(aes(fill=Treatment))

ggplot(Arthritis, aes(x=Improved)) + geom_bar(aes(fill=Improved))

## in context of bar plots , we can have two aes mappings : color and fill

## ------------------------------------------------------------------------
ggplot(mtcars,aes(y=mpg,x=am)) + geom_boxplot()



ggplot(mtcars,aes(y=mpg,x=am)) + geom_violin()

## ------------------------------------------------------------------------
ggplot(mtcars,aes(y=mpg,x=am)) + geom_point() +geom_jitter()



ggplot(Arthritis,aes(x=Improved)) + geom_bar() + coord_polar(theta="y")




ggplot(Arthritis,aes(x="",fill=Improved)) +geom_bar()+
  coord_polar(theta="y")





## ------------------------------------------------------------------------
?diamonds

ggplot(diamonds, aes(x = "", fill = clarity)) + 
  geom_bar() + 
  
  coord_polar(theta = "y")

## ------------------------------------------------------------------------
ggplot(diamonds, aes(x = "", fill = clarity)) +
  geom_bar() + 
  coord_polar(theta ="y")+
  scale_fill_brewer(palette = "Accent")

## ------------------------------------------------------------------------
ggplot(diamonds, aes(x =clarity, fill = clarity)) + 
  geom_bar() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Accent")


## ------------------------------------------------------------------------

ggplot(diamonds, aes(x = clarity, fill = cut)) + 
  geom_bar(width=0.2)+ 
  coord_polar(theta = "x") + 
  scale_fill_brewer(palette = "Accent")

## ------------------------------------------------------------------------

ggplot(diamonds, aes(x = clarity, fill = clarity)) + 
  geom_bar(width =0.9) +
  coord_polar(theta = "y") + 
  scale_fill_brewer(palette = "Accent")

## ------------------------------------------------------------------------

ggplot(diamonds, aes(x = clarity, fill = cut)) + 
  geom_bar(width = 0.9) + 
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Accent")

### how to visualise numeric data and different mappings of the data to
## aesthetics, visualizing categorical data, polar coordinates.

### distributions and density 

## ------------------------------------------------------------------------
mydata=data.frame(v1=c(rnorm(2000),runif(1000)),
                  dist=c(rep("Normal",2000),rep("Uniform",1000))
                  )


ggplot(mydata,aes(x=v1)) + geom_histogram()



ggplot(mydata,aes(x=v1,fill=dist,alpha=0.5))+geom_histogram()

## ------------------------------------------------------------------------
ggplot(mydata,aes(x=v1,fill=dist)) + geom_density()

ggplot(mydata,aes(x=v1)) + geom_density()



## ------------------------------------------------------------------------
ggplot(mydata,aes(x=v1,fill=dist,alpha=0.30))+geom_density()

### how exactly alpha controls tranparency of the fill : for lalit



## ------------------------------------------------------------------------
ggplot(mydata,aes(x=v1,fill=dist,alpha=1/5))+geom_density()+geom_histogram()





## ------------------------------------------------------------------------
ggplot(mydata,aes(x=v1,fill=dist,alpha=1/5))+geom_density()+
  geom_histogram(aes(y=..density..))

## ------------------------------------------------------------------------
ggplot(mydata,aes(x=v1))+geom_density(color="red")+
  
  geom_histogram(aes(y=..density..,alpha=0.5))+
  
  stat_function(fun=dnorm,aes(x=v1),color="green")

mydata1=mydata[mydata$dist=="Normal",]

ggplot(mydata1,aes(x=v1))+geom_density(color="red")+
  
  geom_histogram(aes(y=..density..,alpha=0.5))+
  
  stat_function(fun=dnorm,aes(x=v1),color="green")


ggplot(mtcars, aes(x=mpg))+ geom_histogram()
