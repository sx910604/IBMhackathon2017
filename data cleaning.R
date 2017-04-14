gr2014 <- read_csv("~/Desktop/gr2014.csv")
gr2013 <- read_csv("~/Desktop/gr2013.csv")
gr2012 <- read_csv("~/Desktop/gr2012.csv")
gr2011 <- read_csv("~/Desktop/gr2011.csv")
gr2010 <- read_csv("~/Desktop/gr2010.csv")

n1 <- names(gr2010)
n2 <- names(gr2011)
n3 <- names(gr2012)
n4 <- names(gr2013)
n5 <- names(gr2014)

#delete useless variable in 2013
gr2013 <- gr2013[,-27]

n3 <- names(gr2012)

rbind(n1,n2,n3,n4,n5)


gr2010$year <- 2010
gr2011$year <- 2011
gr2012$year <- 2012
gr2013$year <- 2013
gr2014$year <- 2014


name_modify <- function(char){
    #eliminate things like 1011 to help merge the data
    char[7] <- "ALL_T"
    char[8] <- "ALL_R"
    char[9] <- "IndianAlaska_T"
    char[10] <- "IndianAlaska_R"
    char[11] <- "AsianPasific_T"
    char[12] <- "AsianPasific_R"
    char[13] <- "Black_T"
    char[14] <- "Black_R"
    char[15] <- "Hispanic_T"
    char[16] <- "Hispanic_R"
    char[17] <- "MoreR_T"
    char[18] <- "MoreR_R"
    char[19] <- "White_T"
    char[20] <- "White_R"
    char[21] <- "Disa_T"
    char[22] <- "Disa_R"
    char[23] <- "Econ_T"
    char[24] <- "Econ_R"
    char[25] <- "LimitE_T"
    char[26] <- "LimitE_R"
    return(char)
}


names(gr2010) <- name_modify(names(gr2010))
names(gr2011) <- name_modify(names(gr2011))
names(gr2012) <- name_modify(names(gr2012))
names(gr2013) <- name_modify(names(gr2013))
names(gr2014) <- name_modify(names(gr2014))

gr <- rbind(gr2010,gr2011,gr2012,gr2013,gr2014)
gr <- gr[,-27]

#delete ps in total
id <- which(gr$ALL_R=="PS")
gr <- gr[-id,]



#decode the protection
decode <- function(char){
    char1 <- rep(NA,length(char))
    char2 <- rep(NA,length(char))
    for (i in 1:length(char)){
        if (char[i]=="GE99") {char1[i]="99";char2[i] <- "100"}
        else if (char[i]=="GE95") {char1[i]="95";char2[i] <- "100"}
        else if (char[i]=="GE90") {char1[i]="90";char2[i] <- "100"}
        else if (char[i]=="GE80") {char1[i]="80";char2[i] <- "100"}
        else if (char[i]=="GE50") {char1[i]="50";char2[i] <- "100"}
        else if (char[i]=="LE1") {char1[i]="0";char2[i] <- "1"}
        else if (char[i]=="LE10") {char1[i]="0";char2[i] <- "10"}
        else if (char[i]=="LE20") {char1[i]="0";char2[i] <- "20"}
        else if (char[i]=="LE5") {char1[i]="0";char2[i] <- "5"}
        else if (char[i]=="LT50") {char1[i]="0";char2[i] <- "49"}
        else if (substr(char[i],3,3)=="-") {char1[i]=substr(char[i],1,2);char2[i] <- substr(char[i],4,5)}
        else if (substr(char[i],2,2)=="-") {char1[i]=substr(char[i],1,1);char2[i] <- substr(char[i],3,3)}
        else {char1[i] <- char[i];char2[i] <- char[i]}
    }
    return(cbind(char1,char2))
}

d1 <- decode(gr$ALL_R)
gr$ALL_RL <- as.numeric(d1[,1])
gr$ALL_RU <- as.numeric(d1[,2])

rm(d1)

#A simpler decoding method
decode2 <- function(char){
    charnew <- rep(NA,length(char))
    for (i in 1:length(char)){
        if (is.na(char[i])) next
        else if (char[i]=="GE99") {charnew[i]=99}
        else if (char[i]=="GE95") {charnew[i]=97}
        else if (char[i]=="GE90") {charnew[i]=95}
        else if (char[i]=="GE80") {charnew[i]=90}
        else if (char[i]=="GE50") {charnew[i]=75}
        else if (char[i]=="LE1") {charnew[i]=1}
        else if (char[i]=="LE5") {charnew[i]=3}
        else if (char[i]=="LE10") {charnew[i]=5}
        else if (char[i]=="LE20") {charnew[i]=10}
        else if (char[i]=="LT50") {charnew[i]=25}
        else if (char[i]=="PS") {charnew[i]=NA}
        else if (char[i]==".") {charnew[i]="."}
        else if (substr(char[i],3,3)=="-") {charnew[i]=round((as.numeric(substr(char[i],1,2))+as.numeric(substr(char[i],4,5)))/2)}
        else if (substr(char[i],2,2)=="-") {charnew[i]=round((as.numeric(substr(char[i],1,1))+as.numeric(substr(char[i],3,3)))/2)}
        else charnew[i] <- char[i]
    }
    return(charnew)
}

grnew <- gr

grnew$IndianAlaska_R <- decode2(grnew$IndianAlaska_R)
grnew$AsianPasific_R <- decode2(grnew$AsianPasific_R)
grnew$Black_R <- decode2(grnew$Black_R)
grnew$Hispanic_R <- decode2(grnew$Hispanic_R)
grnew$MoreR_R <- decode2(grnew$MoreR_R)
grnew$White_R <- decode2(grnew$White_R)
grnew$Disa_R <- decode2(grnew$Disa_R)
grnew$Econ_R <- decode2(grnew$Econ_R)
grnew$LimitE_R <- decode2(grnew$LimitE_R)
grnew$ALL_R <- (grnew$ALL_RL+grnew$ALL_RU)/2
grnew <- grnew[,-c(28,29)]
grnew$ALL_G <- grnew$ALL_T*grnew$ALL_R/100



names(grnew)[1] <- "State"

save(grnew,file="gr.Rdata")

