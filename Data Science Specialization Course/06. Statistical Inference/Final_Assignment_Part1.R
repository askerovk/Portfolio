if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}

set.seed(234112)

expn<-rexp(1000, 0.2)

expn.means<- c(1:1000)

for (i in 1:1000) {
    
    x<-mean(rexp(40, 0.2)) 

    expn.means[i]<-x
}

u1<-mean(expn)

u2<-mean(expn.means)

var1<-var(expn)

var2<-var(expn.means)

plot1<-as.data.frame(expn)

ggplot(data = plot1, aes(x=plot1)) + geom_histogram(col= "black", fill="white",aes(y=..density..), binwidth = 2) + 
    
    scale_x_continuous(name = "exponentials", breaks = seq(0, 32, 2), minor_breaks = NULL) + 
    
    scale_y_continuous(breaks = seq(0, 0.15, 0.05), minor_breaks = FALSE) + geom_vline(xintercept = u1, col="red", lwd=1) + 
    
    ggtitle("Historgram of 1000 exponentials") + stat_function(fun=dnorm, color="blue", args=list(mean=u1, sd=sqrt(var1)))

plot2<-as.data.frame(expn.means)

ggplot(data = plot2, aes(x=plot2)) + geom_histogram(col= "black", fill="white", aes(y=..density..), binwidth = 0.25) + 
    
    scale_x_continuous(name = "means" , breaks = seq(1, 10, 0.5)) + 
    
    scale_y_continuous(breaks = seq(0, 0.6, 0.1), minor_breaks = FALSE) + geom_vline(xintercept = u2, col="red", lwd=1) + 
    
    ggtitle("Historgram of 1000 means of 40 exponentials") + stat_function(fun=dnorm, color="blue", args=list(mean=u2, sd=sqrt(var2)))
