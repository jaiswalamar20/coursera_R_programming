library(stringr) #library for str_pad function
directory = "/datadrive/amar.jaiswal/R/x86_64-pc-linux-gnu-library/Coursera/specdata"
############################ Part 1 ##########################################
pollutantmean <- function(directory,pollutant,id=1:332)
        {
        v <- vector()
        for (i in id){
             #a <- read.csv(sprintf("%s/00%s.csv",directory,i),as.is=T)#sprintf eliminated the use of str_pad
             #alternate code to write the below code is above with one problem that when we go above 009.csv,it should be 010.csv but our code gives 0010.csv:
             a <- read.csv(paste0(directory,"/",str_pad(i,3,pad="0"),".csv"),as.is=T)
             v <- c(v,a[,pollutant])
             }
        b <- mean(v,na.rm=TRUE)
        return(b)
}

############################ Part 2 ##########################################
# commented lines are of the default function available for the count of complete cases
complete <- function(directory,id=1:332)
        {
        nobs=vector()
        for (i in id)
                {
                a <- read.csv(paste0(directory,"/",str_pad(i,3,pad="0"),".csv"),as.is=T)
#                 d= c(d,sum(complete.cases(a)))
#                 }
#         result <- data.frame(id,d)
#                 print(result)}
                z = rowSums(a[,2:4])
                nobs = c(nobs,sum(!is.na(z)))
                }
        return(data.frame(id,nobs))
}

############################ Part 3 ##########################################
corr <- function(directory,threshold=0){
        p=vector()
        for (i in 1:332){
                a <- read.csv(paste0(directory,"/",str_pad(i,3,pad="0"),".csv"),as.is=T) 
                z = rowSums(a[,2:3])
                b = sum(!is.na(z))
                if(b >= threshold){
                        q= cor(a$sulfate,a$nitrate, use = "complete.obs")
                        p = c(p,q)                      
                        }
        }
        return(p)
}
