# megnézem, hogy a küldött fájl mit ad eredményül.

source("tr_tst.R")
library(data.table)

orig <- fread("../../sample_submission.csv", na.strings=c("null", "NA", ""))
res1 <- fread("../../lead_1.csv", sep=";", encoding="UTF-8")
res2 <- fread("../../dbvesz2.csv", sep=",")
train <- fread("../../Train.csv", na.strings=c("null", "NA", ""))
test <- fread("../../Test.csv", na.strings=c("null", "NA", ""))

# negatív értékek kiszedése
nullazott <- copy(res2)
for(j in seq(ncol(nullazott))){
  for(i in seq(nrow(nullazott))){
    if(nullazott[[j]][i]<0){
      set(nullazott,i,j,0)
    }
  }
}

nullazott <- cbind(orig[[1]], nullazott)
setnames(nullazott, names(orig))

options(scipen=999)
write.csv(nullazott,
          file="../../NN_nullazott.csv",
          row.names=FALSE,
          quote=FALSE)

intersect(nullazott$objectId, test$objectId)
orig[, sorszam := order(objectId)]
nullazott2 <- merge(nullazott,
                    orig[, .(objectId, sorszam)],
                    all=FALSE,
                    by="objectId")


# csináljuk a cetlik
# 801 sor, nincs header, 260, a megfelelő sorrendben
szeles_train <- dcast.data.table(cetliId.objectId ~ product,
                                 data=train,
                                 value.var="objectId",
                                 fun.aggregate=function(x){as.numeric(length(x)>0)})
szukseges_cetlik <- szeles_train[cetliId.objectId %in% test$cetliId.objectId]
szukseges_cetlik <- merge(szukseges_cetlik, test[, .(cetliId.objectId, objectId)],
                          all=FALSE,
                          by="cetliId.objectId")[order(objectId),][,cetliId.objectId:=NULL]

write.csv(szukseges_cetlik,
          file="../../teszt_cetlik.csv",
          row.names=FALSE,
          quote=FALSE)


