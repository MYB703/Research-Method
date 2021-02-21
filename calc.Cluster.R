rm(list=ls())

#opening the file
egs=read.table('Droughts.txt',skip=22,header=T)


# Removing Year and Month
xx=egs[,3:18]
# count=0
# for (i in xx){
#   if (xx==-999){
#     count=count+1
#   }
# }
# Replacing the missing values (-999) with -2.33
# xx= egs[,-c(1,7,10)]
 xx[xx==-999]=-2.33


# Creating a sequence  of january from all the months
jan=seq(1,612,12)

# Creating an array to keep all the 51 januaries
# for the 16 variables

yy=array(1,c(51,16))

# selecting the month of january 
for ( i in 1 :16){

yy[,i]=xx[,i][jan]
}

# scaling the array

yy.scaled=scale(yy)
colnames(yy.scaled)=names(xx)

####### performing the clustering############
yy.clus=hclust(dist(t(yy.scaled)),method='ward')

#####plotting the tree#################
plot(yy.clus,hang=-2)

#cutree(yy.clus,k=4)


