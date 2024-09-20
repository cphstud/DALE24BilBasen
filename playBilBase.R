library(skimr)
options(scipen = 999)
# 1) Research Goal: show numeric variables part of variance in carprices

# 2) Retrieve data
# 2.1) External from bilbasen.dk
bildfraw <-  read.csv("data/bilbasen.csv")

# 3) Data preparation
# 3.0) Data Description
skim(bildfraw)

# 3.0.1) Numeric data subset
bildfnumv <- sapply(bildfraw,is.numeric)
bildfnum <- bildfraw[,bildfnumv]
bildfnum <- bildfnum[,-c(1,4,7)]
bildfoutl <- bildfnum[is.na(bildfnum$mpg),]
bildfcl <- bildfnum[!is.na(bildfnum$mpg),]
sum(is.na(bildfcl$mpg))


# 3.1) Data cleansing 
# remove rows with no "," in maketype
lv=grepl(",",bildfraw$maketype)
bildfraw=bildfraw[lv,]

# 3.1.1) Outliers
cnn=colnames(bildfcl3)
par(mfrow=c(3,2))
for (name in cnn) {
  boxplot(bildfcl3[[name]], xlab=name)
}
boxplot(bildfcl4$displ)

# remove mpg-outlier
bildfcl2 = bildfcl[bildfcl$mpg < 35,]
# remove price-outlier
bildfcl3 = bildfcl2[bildfcl2$price <400000,]
# remove milage-outlier
bildfcl3 = bildfcl3[bildfcl3$milage <300000,]
# remove year-outlier
bildfcl3 = bildfcl3[bildfcl3$age < 20,]
# remove displ-outlier
bildfcl4 = bildfcl4[bildfcl4$displ < 3,]



# 3.2) Data transformation
# 3.2.1) Variable extrapolation
# én case
st="Toyota Yaris 1,5 VVT-iE T3 Smart 5d"
cpt="[0-9]+,[0-9]+"
m=regexpr(cpt,st)
displ=regmatches(st,m)

# flere obs
testdf = bildfraw[sample(1:nrow(bildfraw),10),]
m=regexpr(cpt,testdf$maketype)
displ=regmatches(testdf$maketype,m)
testdf$displ=as.numeric(gsub(",",".",displ))

# alle obs
m=regexpr(cpt,bildfraw$maketype)
displ=regmatches(bildfraw$maketype,m)
bildfraw$displ=as.numeric(gsub(",",".",displ))
hist(bildfraw$displ)

# merge this onto numeric-only-dataframe
oidx=rownames(bildfcl3)
bildfcl3$X=oidx
bildfrawM=bildfraw[,c(1,15)]
bildfcl4=merge(bildfcl3,bildfrawM, by="X", all.x = T)


# 3.2.2) Variable derivation
# age
bildfcl3$age=2024-bildfcl3$year
# 3.3) Data combinations 


# 4) Data preparation
# 4.1) Simple graphs 
# 4.1.1) Simple graphs Categorial
# 4.1.2) Simple graphs Numeric
par(mfrow=c(2,2))
boxplot(bildfcl$mpg, xlab="mpg")
boxplot(bildfcl$milage, xlab="milage")
boxplot(bildfcl$year, xlab="year")
boxplot(bildfcl$price, xlab="price")
par(mfrow=c(1,1))
boxplot(bildfcl4$displ, xlab="displ")

# 4.1.3) More variables

par(mfrow=c(1,1))
plot(bildfcl3$mpg,bildfcl3$price)
plot(bildfcl4$mpg,bildfcl4$displ)

plot(bildfcl4$price,bildfcl4$age)
plot(bildfcl4$price,bildfcl4$mpg)
plot(bildfcl4$price,bildfcl4$dipl)
plot(bildfcl4$price,bildfcl4$milage)

# 5) Data modelling
# lav et loop for hver numerisk variabel og læg lm-modellen i en liste
v=colnames(bildfcl4)
lmv=v[c(2,3,6,7)]
collist=list()

for (i in (1:length(lmv))) {
  tmpform=paste0("price ~ ",lmv[i])
  tmplm=lm(data=bildfcl4, formula = tmpform)
  #collist[[i]]=tmplm
  collist[[lmv[i]]]=tmplm
}

