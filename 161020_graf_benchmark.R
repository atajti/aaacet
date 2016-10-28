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

# optimal_coms <- cluster_optimal(prod_g)
# plot(optimal_coms, prod_g)

#--------------------------------------------------------------------#
### a listán szereplő itemek közvetlen szomszédai fokszám alapján ####
#--------------------------------------------------------------------#

szomszedok <- list()
for(cetli in tr_full$cetliId.objectId){
  szomszedok[[cetli]] <- neighbors(prod_g,
                                   V(prod_g)[V(prod_g)$name %in%
                                             tr_full[cetliId.objectId == cetli,
                                                     product]])
}

lista_jeloltek <- sapply(szomszedok, function(V){V$name})

test_file="../../Test.csv"
test_full <- fread(test_file,
             na.strings=c("", "null", "NULL", "\\N", "NA"))

lista_jeloltek <- lista_jeloltek[names(lista_jeloltek) %in% test_full$cetliId.objectId]

for(i in seq(length(lista_jeloltek))){
  lista_jeloltek[[i]] <- data.table(prod = lista_jeloltek[[i]],
                                    cetliId.objectId=names(lista_jeloltek)[i])
}

graf_benchmark_szomszedok_sulytalan <- dcast.data.table(cetliId.objectId ~ prod,
                                                        data=rbindlist(lista_jeloltek),
                                                        fun.aggregate=length)

sample_header <- fread("../../sample_submission.csv")
graf_benchmark_szomszedok_sulytalan2 <- merge(graf_benchmark_szomszedok_sulytalan,
                                             test_full[, .(objectId, cetliId.objectId)],
                                             by="cetliId.objectId",
                                             all=TRUE)[, cetliId.objectId := NULL]
graf_benchmark_szomszedok_sulytalan2[, (setdiff(names(sample_header),
                                                names(graf_benchmark_szomszedok_sulytalan2))) := 0]


objid_colnum <- which("objectId" == names(graf_benchmark_szomszedok_sulytalan2))

setcolorder(graf_benchmark_szomszedok_sulytalan2,
            c(objid_colnum, seq(ncol(graf_benchmark_szomszedok_sulytalan2))[-objid_colnum]))

setdiff(names(graf_benchmark_szomszedok_sulytalan2), names(sample_header))

write.csv(graf_benchmark_szomszedok_sulytalan2,
          file="../../graf_benchmark_1.csv",
          row.names=FALSE,
          quote=FALSE)


# kiszedem a benne lévőket
