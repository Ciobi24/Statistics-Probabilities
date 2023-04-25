#ex1.1
sample1=scan("sample1.txt")
stem(sample1)

#ex1.2
tablou = read.csv('unemploy2012.csv', header = T, sep = ';')
rate = tablou[['rate']]
interval=c(0,4,6,8,10,12,14,30)
hist(rate,breaks=interval,right=T,freq=T)

#ex1.3
tablou=read.csv('life_expect.csv',sep=',')
femei=tablou[['female']]
barbati=tablou[['male']]
hist(femei, breaks=7, freq=T, main="Histograma femei",col='pink')
hist(barbati, breaks=7, freq=T, main="Histograma barbati",col='blue')

#ex2.1
mean(sample1)
median(sample1)

#ex2.2
tablou2=read.csv('life_expect.csv',header=T,sep=',')
female=tablou2[['female']]
male=tablou2[['male']]
country=tablou2[['country']]
mean(female)
median(female)
mean(male)
median(male)

#ex2.3


#ex3
sample3 = c(9, 8, 12, 3, 17, 41, 29, 35, 32, 40, 19, 8)
quantile(sample3)
as.vector(quantile(sample3))
summary(sample3)

#ex3.1
sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)

outliers_mean=function(sample){
m = mean(sample)
s = sd(sample)
outliers = vector()
j = 0
for(i in 1:length(sample))
  if(sample[i] <m - 2*s | sample[i] > m + 2*s) {
    j = j + 1
    outliers[j] = sample[i]
    }
return(outliers)
}
outliers_mean(sample)
#ex3.2
outliers_iqr=function(sample){
  q1=as.vector(quantile(sample))[2]
  q3=as.vector(quantile(sample))[4]
 iqr=q3-q1
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] <q1-1.5*iqr | sample[i] >q3+1.5*iqr) {
      j = j + 1
      outliers[j] = sample[i]
    } 
  return(outliers)
}
outliers_iqr(sample)

#ex3.3

