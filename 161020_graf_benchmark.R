# basic benchmark3:
# hálózat, először élsúlyok nélkül

library(data.table)
library(igraph)
train_file="../../Train.csv"
tr_full <- fread(train_file,
             na.strings=c("", "null", "NULL", "\\N", "NA"))

# éllista:
tr_el <- merge(tr_full[, .(product1=product, cetliId.objectId)],
               tr_full[, .(product2=product, cetliId.objectId)],
               by="cetliId.objectId",
               all=TRUE,
               allow.cartesian=TRUE)[, .(weight=.N),
                                      by=.(product1, product2)][
         product1 != product2, ][product1 < product2,]

compno <- 0
limit <- 0
while(compno < 2){
  prod_g <- graph_from_data_frame(tr_el[weight>limit], directed=FALSE)
  comps <- components(prod_g)
  compno <- comps$no
  print(paste0(limit, ": ", compno))
  limit <- limit + 1
  rm(prod_g, comps);gc()
}

# nem lesz ez jó


prod_g <- graph_from_data_frame(tr_el[weight > 3], directed=FALSE)
tkplot(prod_g)
wlaktrap_coms <- cluster_walktrap(prod_g, steps=2)
plot(wlaktrap_coms, prod_g)

optimal_coms <- cluster_optimal(prod_g)
plot(optimal_coms, prod_g)

