# paraméterek: él gráfbakerülésének limitje,
#              élsúly figyelembevétele

source("tr_tst.R")
prcl2 <- function(graph, infection){
  dm <- (1/distances(graph))
  diag(dm) <- 0
  # dm[!is.finite(dm)] <- 0
  weight_matrix <- infection %*% t(1/(sum(infection)-infection))
  #weight_matrix[!is.finite(weight_matrix)] <- 0
  dm <- dm * weight_matrix

  res <- colSums(dm)
  return(res)
}


library(data.table)
library(igraph)


cetli_train <- fread("../../Train.csv",
                     na.strings=c("", "null", "NULL", "\\N", "NA"))

train_test <- tr_tst(cetli_train)

#--------------------------------------------------------------------#
### Eljárás 1. #######################################################
#--------------------------------------------------------------------#

basic_prcl_bmrk <- function(train_test){

  tr_wide <- train_test$train
  tst_wide <- train_test$test

  # éllista
  long_form <- melt.data.table(data=tr_wide,
                               id.vars="cetliId.objectId",
                               variable.name="product")[
    value>0,][, value := NULL] 
  edgelist <- merge(long_form, long_form,
                    by="cetliId.objectId",
                    allow.cartesian=TRUE,
                    all=FALSE)[
    , cetliId.objectId := NULL][
    product.x != product.y, ][
    as.character(product.x) > as.character(product.y), ]

  # gráf
  product_graph <- graph_from_data_frame(edgelist, directed=FALSE)

  # minden listánál kiszámolom a prcl-t
  prcl_list <- lapply(seq(nrow(tr_wide)),
        function(r){
          res <- prcl2(product_graph,
                unlist(tr_wide[r,
                               V(product_graph)$name,
                               with=FALSE]))
          if(!r%%100){
            cat(paste0("\n", r, " : ", Sys.time()))
          }
          return(res)
        })

  # data-table-lé teszem, majd kiszedem belőle a teszt adatokat
  res_wide <- rbindlist(lapply(prcl_list, as.list))[,
    cetliId.objectId := tr_wide$cetliId.objectId]

  res_wide <- res_wide[cetliId.objectId %in% tst_wide$cetliId.objectId,]

  res_obj <- tst_wide[, .(cetliId.objectId, objectId)][
                      res_wide,  on="cetliId.objectId"]

  return(res_obj)
}

model_res <- basic_prcl_bmrk(train_test)

cetli_eval(train_test$test, model_res)

# 2 óra futás, 5 per 1 menet, 24 próba. akkor legyen 4 óra futás. (4.5 lett)

prcl_benchmark <- numeric(50)
for(i in 1:50){
  train_test <- tr_tst(cetli_train)
  model_res <- basic_prcl_bmrk(train_test)
  prcl_benchmark[i] <- cetli_eval(train_test$test, model_res)
  gc()
}


#--------------------------------------------------------------------#
### Eljárás 2.: közös listák száma -> élsúly, ezen szűrök alulról ####
#--------------------------------------------------------------------#
#~4ó10perc egy 10-es kör


limit_prcl_bmrk <- function(train_test, limit){

  tr_wide <- train_test$train
  tst_wide <- train_test$test

  # éllista
  long_form <- melt.data.table(data=tr_wide,
                               id.vars="cetliId.objectId",
                               variable.name="product",
                               value.name="weight")[
    weight > 0, ]
  edgelist <- merge(long_form, long_form,
                    by="cetliId.objectId",
                    allow.cartesian=TRUE,
                    all=FALSE)[
    , cetliId.objectId := NULL][
    product.x != product.y, ][
    as.character(product.x) > as.character(product.y), ][,
    weight := weight.x][, `:=`(weight.x=NULL,
                               weight.y=NULL)]

  # gráf
  product_graph <- graph_from_data_frame(edgelist, directed=FALSE)
  product_graph <- delete.edges(product_graph,
                                which(E(product_graph)$weight > limit))
  # minden listánál kiszámolom a prcl-t
  prcl_list <- lapply(seq(nrow(tr_wide)),
        function(r){
          res <- prcl2(product_graph,
                unlist(tr_wide[r,
                               V(product_graph)$name,
                               with=FALSE]))
          if(!r%%100){
            cat(paste0("\nlimit:", limit, ", ", r, " : ", Sys.time()))
          }
          return(res)
        })

  # data-table-lé teszem, majd kiszedem belőle a teszt adatokat
  res_wide <- rbindlist(lapply(prcl_list, as.list))[,
    cetliId.objectId := tr_wide$cetliId.objectId]

  res_wide <- res_wide[cetliId.objectId %in% tst_wide$cetliId.objectId,]

  res_obj <- tst_wide[, .(cetliId.objectId, objectId)][
                      res_wide,  on="cetliId.objectId"]

  return(res_obj)
}

limits <- c(1,2,3,5,7,10,15,20,30,50)
prcl_benchmark <- rep(list(numeric(10)), 10)
names(prcl_benchmark) <- limits
for(lim in limits){
  for(experiment in 1:10){
    train_test <- tr_tst(cetli_train)
    model_res <- limit_prcl_bmrk(train_test, lim)
    prcl_benchmark[experiment] <- cetli_eval(train_test$test, model_res)
    gc()
  }
}


