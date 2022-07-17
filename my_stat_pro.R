data("trees")
trees
summary(trees$Girth)

me<-mean(trees$Girth)
print(me)

mo<-getmode(trees$Girth)
print(mo)

mi<-median(trees$Girth)
print(mi)

quantile(trees$Girth,0.25)
quantile(trees$Girth,0.75)


maximum<-max(trees$Girth)
print(maximum)
minimum<-min(trees$Girth)
print(minimum)


std<-sd(trees$Girth)
print(std)

v<-var(trees$Girth)
print(v)

hist(trees$Girth,xlab="Girth column",col="Red",main="Trees GrithHistogram",breaks = 10)
boxplot(trees$Girth,col=rainbow(6),ylab="box plot of Girth ")

z<-c(me,mi,std,v,maximum,minimum)
xlap<-c("mean","median","std","var","maximum vaue","minmum value")
xlap<-paste(xlap,"=","[",z,"]")
xlap<-paste(xlap,sep=" ")
pie(z,labels = xlap)

barplot(trees$Girth,col=rainbow(6),main="bar chart of Girth column")
#cor.test(trees$Girth,trees$Height,method = "pearson")
#cor(trees$Girth,trees$Height,method = "pearson")
cor(trees[,1:3])

#trees$Girth (y-value)
#trees$Height(x-value)
#y is the response variable:whose value is derived from the predictor variable..
#x is the predictor variable:whose value is gathered through experiments.
#This function creates the relationship model between the predictor and the response variable.

plot(trees$Girth,trees$Height,col="red")

var<-lm(trees$Girth ~ trees$Height,data=trees)
var
# Residuals: the distance from data to the fited line
# height = -6.18839+ 0.25575  * Girth
#y       =    B(0) +    b(1)  *    x
va<-predict(var,newdata = trees)
va
vart<-t.test(trees$Girth ~ trees$Height)
vart
#data("rock")
#rock[rock$perm==84.2&rock$peri>4000&rock$area<11111,]

plot(density(rock$area),cex=0.1,bty="n",yaxt="n",ylab="Y-axis",xlab="X-axis",main="area")
plot(density(rock$shape),cex=0.1,bty="n",yaxt="n",ylab="Y-axis",xlab="X-axis",main="shape")
