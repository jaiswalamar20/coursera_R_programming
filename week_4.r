#df_all = df_all[,-c(which(colnames(df_all) %in% c('id')))]
#train = train[-grep('country_destination', colnames(train))]
setwd("/datadrive/amar.jaiswal/R/x86_64-pc-linux-gnu-library/Coursera")
getwd()
main_data <- read.csv("outcome-of-care-measures.csv",as.is=TRUE,check.names=FALSE)
hospital_name <- read.csv("hospital-data.csv",as.is=TRUE,check.names=FALSE)
# str(main_data)
# main_data[,11]= as.numeric(data[,11]) 
# hist(data[,11])

#few codes for info extraction
# length(unique(main_data$Provider.Number))
# length(unique(hospital_name$Provider.Number))

#main_data[main_data == "Not Available"]<- NA #converting to NAs for convinience

x <- merge(hospital_name,main_data,all.x = T) #all.x includes all the ibservations irrespective of the matches
n <- c(15,17,18,19,21,23,24,25,27,29,30,31,33,35,36,37,39,41,42,43,45,47,48,49)
for (i in n){
        x[,i] <- as.numeric(x[,i])
}
table(sapply(x,class))
na_count <- data.frame(sapply(x,function(y) length(which(is.na(y)))))
#names(x) <- gsub("\\."," ",names(x))
#names(mydf) <- gsub("\\.", "", names(mydf))
#To search for coloumn names that contain a particular string
# library(dplyr)
# outcome = "Heart Attack"
# names(x %>% select(contains(sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome))))
# grep(sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome), names(x), value = TRUE,fixed = T,useBytes = TRUE)
########################### function 1 #################################################
best <- function(state,outcome){
        if(!state %in% x$State) stop("invalid state")
        if(!outcome %in% c("Heart Attack", "Heart Failure","Pneumonia")) stop("invalid outcome")
        #state = 'AL'
        y <- subset(x[(colnames(x)) %in% c("Hospital Name","State",sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome))],x$State == state)
        z <- y[complete.cases(y),]
        #name <- z$`Hospital Name`[z[,sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome)] == min(y[,sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome)],na.rm=T)] 
        xyz <- sort(z$`Hospital Name`[which.min(z[,sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome)])])
        return(xyz[1])
}

best("TX", "Heart Attack")
best("TX", "Heart Failure")
best("MD", "Heart Attack")

######################### function2 ##################################################

rankhospital <- function(state,outcome,num){
        if(!state %in% x$State) stop("invalid state")
        if(!outcome %in% c("Heart Attack", "Heart Failure","Pneumonia")) stop("invalid outcome")
        #state = 'AL'
        y2 <- subset(x[(colnames(x)) %in% c("Hospital Name","State",sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome))],x$State == state)
        z2 <- y2[complete.cases(y2),]
        #name <- z$`Hospital Name`[z[,sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome)] == min(y[,sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome)],na.rm=T)] 
        #xyz <- sort(z[,3],decreasing =  TRUE)
        xyz2 = z2[order(z2[,3],z2[,1],decreasing = F),]
        #print(head(xyz2,10))
        if (num=="best") return(xyz2$`Hospital Name`[1])
        if (num=="worst") return(xyz2$`Hospital Name`[length(xyz2$`Hospital Name`)])
        if (num > length(z2$`Hospital Name`)) return(NA)
        return(xyz2$`Hospital Name`[num])
}
rankhospital("TX", "Heart Failure", 300)
rankhospital("MD", "Heart Attack", "worst")
rankhospital("TX", "Heart Failure", 4)

###################### function 3 ############################################

rankall <- function(outcome,num){
        #if(!state %in% x$State) stop("invalid state")
        if(!outcome %in% c("Heart Attack", "Heart Failure","Pneumonia")) stop("invalid outcome")
        #state = 'AL'
        #outcome = "Heart Attack"
        y3 <- subset(x[(colnames(x)) %in% c("Hospital Name","State",sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome))])
        z3 <- y3[complete.cases(y3),]
        #name <- z$`Hospital Name`[z[,sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome)] == min(y[,sprintf("Hospital 30-Day Death (Mortality) Rates from %s",outcome)],na.rm=T)] 
        #xyz <- sort(z[,3],decreasing =  TRUE)
        z3$State <- as.factor(z3$State)
        names(z3)[c(1,2)] <- c('Hospital','state')
#         xyz3 = data.frame(tapply(z3[,3],z3$State,function(q) q[order(q)]))
#         names(xyz3)[c(1,2)] <- c('Hospital','state')
        mnb <- split(z3,z3$state)
        xyz3 <- lapply(mnb,function(q) q[order(q[,3],rev(q[,1]),decreasing = F),])
        #names(xyz3)[c(1,2)] <- c('Hospital','state')
        if (num=="best") return(lapply(xyz3,function(w) w[1,c(1,3)]))
        if (num=="worst") return(lapply(xyz3,function(w) w[nrow(w),c(1,3)]))
        if (num > length(z$`Hospital Name`)) return(NA)
        return(lapply(xyz3,function(w) w[num,c(1,3)]))
}

r <- rankall("Heart Attack", 4)
as.character(subset(r, state == "HI")$hospital)
r$HI[1,]

r <- rankall("Pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r$NJ[nrow(r$NJ),]

r <- rankall("Heart Failure", 10)
r$NV[1,]
