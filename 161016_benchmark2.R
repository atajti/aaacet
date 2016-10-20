## load
library(data.table)

train_file <- "/home/munka/Desktop/cetli/Train.csv"
dat <- fread(train_file,
             na.strings=c("", "null", "NULL", "\\N"))

unique(dat[ownerId.objectId=="Z1AFjm45Bd",.(objectId, product)])

szeles_cetli <- dcast.data.table(dat,
    cetliId.objectId ~ product,
    value.var="quantity",
    fun.aggregate=sum)



# ugyanez felhasználóra



# basic benchmark:
# full_1

test_file <- 
test_full <- fread(test_file, na.strings=c("", "null", "NULL"))
teszt <- copy(test_full)
test_sub_1 <- data.table(objectId=teszt$objectId)[, (names(szeles_cetli)[2:ncol(szeles_cetli)]):=1]
write.csv(test_sub_1,
  file=,
  row.names=FALSE)


# basic bencmark:
# bármi amit vett a felhasználó és nincs a listán
tr_full <- fread(train_file,
             na.strings=c("", "null", "NULL", "\\N", "NA"))
# felhasználó milyen item-eket vett?
felh_prod <- dcast.data.table(tr_full,
                              owner.objectId ~ product,
                              value.var="quantity",
                              fun.aggregate=length)
setkey(test_full, "objectId")
setkey(tr_full, "cetliId.objectId")
object_poss <- lapply(test_full$objectId,
                      function(x){
                        felh_item <- unlist(felh_prod[test_full[x, owner.objectId], ])
                        felh_item <- felh_item[felh_item>0]
                        lista_item <- unlist(tr_full[test_full[x, cetliId.objectId], product])
                        lehet <- felh_item[setdiff(names(felh_item), lista_item)]
                        return(lehet[-which(names(lehet)=="owner.objectId")])
                        })
names(object_poss) <- test_full$objectId

test_sub_2 <- copy(test_sub_1)
for(col in 2:ncol(test_sub_2)){
  set(test_sub_2, j=col, value=0)
}

for(obj in test_full$objectId){
  for(prod in names(object_poss[[obj]])){
    if(is.na(as.integer(object_poss[[obj]][prod]))){
      str(list(obj, prod, object_poss[[obj]][prod]))
    }
        test_sub_2[objectId==obj,
          (prod):=as.integer(object_poss[[obj]][prod])]
  }
}
test_sub_2[, (names(test_sub_2)[ncol(test_sub_2)]) := NULL]
write.csv(test_sub_2,
  file=,
  row.names=FALSE)
