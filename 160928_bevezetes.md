## load
library(data.table)

dat <- fread(train_file)

unique(dat[ownerId.objectId=="Z1AFjm45Bd",.(objectId, product)])

szeles_cetli <- dcast.data.table(dat,
    cetliId.objectId ~ product,
    value.var="quantity",
    fun.aggregate=sum)



# ugyanez felhasználóra



# basic benchmark:
# full_1


test_sub_1 <- data.table(objectId=teszt$objectId)[, (names(szeles_cetli)[2:ncol(szeles_cetli)]):=1]
write.csv(test_sub_1,
  file=,
  row.names=FALSE)
