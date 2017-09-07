if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}

head(ToothGrowth)

toof<-ToothGrowth

names(toof)<- c("cell_length", "supplement", "dose")

toof$supplement<- gsub(pattern = "OJ", replacement = "Orange juice", x = toof$supplement)

toof$supplement<- gsub(pattern = "VC", replacement = "Ascorbic acid", x = toof$supplement)

list<-rep("mg/day", 60)

toof$group<- paste(toof$dose, list, sep = " ")

toof$group<- paste(toof$group, toof$supplement, sep = " ")

toof$group<-factor(toof$group, ordered = TRUE, levels = unique(toof$group))

ggplot(toof, aes(toof$group, toof$cell_length)) + geom_violin(aes(fill=toof$group)) + scale_fill_brewer(name= "Daily Dose", palette = "Dark2") + 
    
    geom_boxplot(width=0.05) +
    
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
    
    scale_y_continuous(name = "Length of ondoblasts (microns)", breaks = seq(0, 35, 5), minor_breaks = FALSE) + 
    
    ggtitle("Effects of Vitamin C supplements on the length of ondoblast cells in Guinea Pigs")

list1<-toof[toof$group == "0.5 mg/day Ascorbic acid", 1]

list3<-toof[toof$group == "1 mg/day Ascorbic acid", 1]

list5<-toof[toof$group == "2 mg/day Ascorbic acid", 1]

list2<-toof[toof$group == "0.5 mg/day Orange juice", 1]

list4<-toof[toof$group == "1 mg/day Orange juice", 1]

list6<-toof[toof$group == "2 mg/day Orange juice", 1]

t.test(list1, list2, paired = FALSE)

t.test(list3, list4, paired = FALSE)

t.test(list5, list6, paired = FALSE)
