setwd("/Users/Joshua/Desktop/Internet/Internet")

library(maps)
map('state', region="florida")

orlando = c(28.37,-81.5568)
miami = c(25.6132,-80.3474)

points(orlando[2], orlando[1], col="red", cex=1, pch=16)
text(orlando[2]+.2, orlando[1]+.2, col="black", "Orlando")

points(miami[2], miami[1], col="blue", cex=1, pch=16)
text(miami[2]-.5, miami[1]+.2, col="black", "Miami")

x = orlando[1]
y = orlando[2]
m = (orlando[2] - miami[2])/((orlando[1] - miami[1]))
b =  y - m*x 

y.new = seq(miami[1],orlando[1], by=.01)
x.new = y.new*m + b

lines(x.new, y.new, lty=2, col="brown")

zipcode = na.omit(read.csv("zipcode.csv"))

x.top = x.new+.2
x.bottom = x.new-.2
lines(x.top, y.new, lty=2, col="red") 
lines(x.bottom, y.new, lty=2, col="red") 


answer = c()
for(i in 1:length(x.bottom)){
answer= c(answer, which(zipcode$latitude < max(y.new) & zipcode$latitude > min(y.new)))
}

new.zipcode = zipcode[unique(answer),]

answer1 = c()

for(i in 1:length(x.bottom)){
  answer1 = c(answer1, which(  (x.top[i] > new.zipcode$longitude &  new.zipcode$longitude > x.bottom[i] 
                                & new.zipcode$latitude < y.new[i]+.2 & new.zipcode$latitude > y.new[i]-.2 ) ) )
}


nzc = new.zipcode[unique(answer1),]

points(nzc$longitude, nzc$latitude, col="green")


final.answer = nzc

head(final.answer)
