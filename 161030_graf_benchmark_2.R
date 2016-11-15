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


#--------------------------------------------------------------------#
### Hasonló userek #############################
#--------------------------------------------------------------------#

train_test <- tr_tst(cetli_train,
                     formula="owner.objectId + cetliId.objectId ~ product ")
train_test2 <- copy(train_test)
user_based_benchmark <- function(train_test, quant=0.75){
 
  user_similarity_table <- melt.data.table(data=train_test$train,
                                           id.vars="owner.objectId",
                                           measure.vars=setdiff(names(train_test$train),
                                                                c("owner.objectId",
                                                                  "cetliId.objectId")),
                                           variable.factor=FALSE)[!value==0] 

  jaccard_index <- function(set1, set2){
    return(length(intersect(set1, set2))/length(union(set1, set2)))
  }

  user_similarity_results <- as.data.table(t(combn(unique(train_test$train$owner.objectId),
                                                        2)))[, sim:=NA_real_]

  all_users <- unique(user_similarity_table$owner.objectId)

  for(r in seq(nrow(user_similarity_results))){
    set(user_similarity_results,
        i=r,
        j=3,
        value=jaccard_index(user_similarity_table[
                              owner.objectId==user_similarity_results[r, V1],
                              variable],
                            user_similarity_table[
                              owner.objectId==user_similarity_results[r, V2],
                              variable]))
  }
  
  # plot(density(user_similarity_results$sim[user_similarity_results$sim<.2 & user_similarity_results$sim>.015]))
  # 75. percentilistől
  similarity_limit <- quantile(user_similarity_results$sim, quant)
  user_pairs <- user_similarity_results[sim > similarity_limit,
                                        .(V1, V2)]

result_list <- NULL
for(u in all_users){ 

  users_to_graph <- c(u,
                      user_pairs[V1==u, V2],
                      user_pairs[V2==u, V1])

  user_data <- train_test$train[owner.objectId %in% users_to_graph,]



  long_form <- melt.data.table(data=copy(user_data)[, owner.objectId := NULL],
                               id.vars="cetliId.objectId",
                               variable.name="product",
                               value.name="weight",
                               variable.factor=FALSE)[
    weight > 0, ]
  edgelist <- merge(long_form, long_form,
                    by="cetliId.objectId",
                    allow.cartesian=TRUE,
                    all=FALSE)[
    , cetliId.objectId := NULL][
    product.x != product.y, ][
    as.character(product.x) > as.character(product.y), ][,
    weight := weight.x][, `:=`(weight.x=NULL,
                               weight.y=NULL)][, weight := weight/max(weight)]

  # gráf
  limit <- -1
  product_graph <- graph_from_data_frame(edgelist, directed=FALSE)
  # product_graph <- delete.edges(product_graph,
  #                               which(E(product_graph)$weight > limit))
  prcl_list <- lapply(which(user_data$owner.objectId==u),
                      function(r){
                        res <- prcl2(product_graph,
                              unlist(user_data[r,
                                             V(product_graph)$name,
                                             with=FALSE]))
                        return(res)
                        })
  user_cetli_prcl <- cbind(user_data[owner.objectId==u,
        .(owner.objectId, cetliId.objectId)],
    rbindlist(lapply(prcl_list,
           function(x){
            as.data.table(t(x))
           }),
      use.names=TRUE,
      fill=TRUE))
  result_list <- c(result_list, list(user_cetli_prcl))
  }

  res_for_all_cetli <- rbindlist(result_list,
                                 use.names=TRUE,
                                 fill=TRUE)

  for(i in seq(ncol(res_for_all_cetli))){
    set(res_for_all_cetli,
        i=which(is.na(res_for_all_cetli[[i]])),
        j=as.integer(i),
        value=0)
  }

  mego <- merge(res_for_all_cetli,
                train_test$test[, .(owner.objectId, cetliId.objectId, objectId)],
                all.x=FALSE,
                all.y=TRUE,
                by=c("owner.objectId", "cetliId.objectId"))[, `:=`(owner.objectId=NULL,
                                                                   cetliId.objectId=NULL)]
  setcolorder(mego,
              c("objectId",
                sort(names(mego)[!names(mego)=="objectId"])))
  return(mego)

}

mego <- user_based_benchmark(train_test, .75)
cetli_eval(copy(train_test$test), mego)

mego2 <- copy(mego)
for(i in 2:(ncol(mego2))){
  set(mego2, j=as.integer(i), value=mego2[[i]]^10)
}

cetli_eval(copy(train_test$test), mego2)


mego <- user_based_benchmark(train_test, .5)
cetli_eval(copy(train_test$test), mego)

mego2 <- copy(mego)
for(i in 2:(ncol(mego2))){
  set(mego2, j=as.integer(i), value=mego2[[i]]^10)
}

cetli_eval(copy(train_test$test), mego2)


mego <- user_based_benchmark(train_test, .25)
cetli_eval(copy(train_test$test), mego)

mego2 <- copy(mego)
for(i in 2:(ncol(mego2))){
  set(mego2, j=as.integer(i), value=mego2[[i]]^10)
}

cetli_eval(copy(train_test$test), mego2)


quants <- c(.5, .6, .7, .75, .8, .85, .90, .95, .975, .99)
quants <- quants[length(quants):1]
num_of_experiments <- 10
all_results <- matrix(NA_real_,
                      ncol=length(quants),
                      nrow=num_of_experiments)
for(quant in quants){
  print(paste0(quant, ": ", Sys.time()))

  for(experiment in seq(num_of_experiments)){
    print(paste0("experiment ", experiment, ": ", Sys.time()))
    traintest <- tr_tst(cetli_train,
                        formula="owner.objectId + cetliId.objectId ~ product ")
    all_results[experiment, as.character(quant)] <- cetli_eval(traintest$test,
                                                     user_based_benchmark(
                                                      traintest,
                                                      quant)) 
  }
}



#--------------------------------------------------------------------#
### Minden terméket figyelembe véve, de használva a súlyokat #########
#--------------------------------------------------------------------#

basic_prcl_bmrk <- function(train_test){

  tr_wide <- train_test$train
  tst_wide <- train_test$test

  # éllista
  long_form <- melt.data.table(data=tr_wide,
                               id.vars="cetliId.objectId",
                               variable.name="product",
                               variable.factor=FALSE,
                               value.name="weight")[
    weight>0,]#[, value := NULL] 
  edgelist <- merge(long_form, long_form,
                    by="cetliId.objectId",
                    allow.cartesian=TRUE,
                    all=FALSE)[
    , cetliId.objectId := NULL][
    product.x != product.y, ][
    as.character(product.x) > as.character(product.y), ]
  weighted_edgelist <- edgelist[, .(weight=.N), by=.(product.x, product.y)]
  weighted_edgelist[, weight := (weight/max(weight))*100]

  # gráf
  product_graph <- graph_from_data_frame(weighted_edgelist, directed=FALSE)

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
mego2 <- copy(res_obj)
for(i in 3:(ncol(mego2))){
  set(mego2, j=as.integer(i), value=mego2[[i]]^10)
}

cetli_eval(copy(train_test$test), mego2)

eval_data <- fread("../../Test.csv")

res_wide <- res_wide[cetliId.objectId %in% eval_data$cetliId.objectId,]
res_obj <- eval_data[, .(cetliId.objectId, objectId),
                     res_wide,
                     on="cetliId.objectId"][, cetliId.objectId:= NULL][,cetliId.objectId:= NULL]
setcolorder(res_obj,
            c("objectId",
              sort(names(res_obj)[which(names(res_obj)!="objectId")])))

mego2 <- copy(res_obj)
for(i in 2:(ncol(mego2))){
  set(mego2, j=as.integer(i), value=mego2[[i]]^10)
}

write.csv(res_obj,
          file="../../weighted_graph_benchmark.csv",
          quote=FALSE,
          row.names=FALSE)
write.csv(mego2,
          file="../../weighted_graph_benchmark_powered.csv",
          quote=FALSE,
          row.names=FALSE)
