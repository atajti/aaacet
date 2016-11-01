#--------------------------------------------------------------------#
### Train és teszt-szet készítése ####################################
### Hosszúból széles formátummal #####################################
#--------------------------------------------------------------------#

#--------------------------------------------------------------------#
### train-test felosztás: ############################################
#--------------------------------------------------------------------#

# n=801 listából kiveszek 1-1 elemet,
# azok a listaelemek a tesztben lesznek
# a felosztófüggvény kételemű listát ad:
# train: széles DT: usrID, listaID, products
# test: széles DT, de csak a teszt sorok vannak benne

# library(data.table)
# train_file="../../Train.csv"
# cetli_train <- fread(train_file,
#              na.strings=c("", "null", "NULL", "\\N", "NA"))

# tr_wide <- dcast.data.table(cetliId.objectId ~ product,
#                             data=cetli_train,
#                             value.var="quantity",
#                             fun.aggregate=length)

tr_tst <- function(train, n=801, ...){
  tr_full <- copy(train)
  # kiválasztok n cetlit, minden cetliről egy elemet
  tst_cetli <- sample(unique(tr_full$cetliId.objectId),
                      n)
  tst_item <- tr_full[cetliId.objectId %in% tst_cetli,
                      .(tst_item=sample(objectId, 1)),
                      by=cetliId.objectId]

  # különszedem a kiválasztott itemeket
  tr_sub <- tr_full[!(objectId %in% tst_item$tst_item), ]
  tst_sub <- tr_full[(objectId %in% tst_item$tst_item), ]

  # megcsinálom a train set szálesítését:
  # a ... az elvégzendő dcasnak a paraméterei value.var-ra és fun.aggregate-re
  # a szűrést mg a tr_full beadása előtt kell elvégezni

  dcast_args <- list(formula=as.formula("cetliId.objectId ~ product"),
                     value.var="product",
                     fun.aggregate=length,
                     data=tr_full)
  user_dcast_args <- list(...)
  dcast_args[names(user_dcast_args)] <- user_dcast_args

  tr_wide <- do.call(dcast.data.table, dcast_args, quote=TRUE)

  
  # a teszt eredménytáblája fix:
  tst_wide <- dcast.data.table(
    owner.objectId + cetliId.objectId + objectId ~ product,
    data=tst_sub,
    value.var="product",
    fun.aggregate=length)
  # a kimaradó oszlopokat is fel kell tölteni
  tst_wide[, (setdiff(names(tr_wide),
                      names(tst_wide))) := 0]

  # if(any(unlist(suppressWarnings(
  #    lapply(names(tst_wide),
  #     function(x){
  #       as.integer(tst_wide[[x]])
  #       })) > 1))){
  #   lapply(tst_wide, summary)
  #   stop("a széles teszt táblában 1 vagy 0 lehet!")
  # }


  return(list(train=tr_wide,
              test=tst_wide))  
}

# innentől minden eljárást olyan függvényként igyekszek megírni,
# ami egy szélessé alakított táblát vesz be inputnak,
# eredménye meg egy ugyanannyi sorból,
# és az összes elem számá megfelelő oszlopból álló DT lesz, 
# amihez ha meg van adva, hozzácsapja az objectId-kat:
# eljaras1(train_data, test_ids)



#--------------------------------------------------------------------#
### Kiértékelés ######################################################
#--------------------------------------------------------------------#

# a tr_tst()$test és az eljárás ereményét kérem be,
# mindkettőt sortolom ObjectId szertin majd setnames(eredmény, tr_tst()$test)
# nomeg sorszázalékot számítok
# aztán a képlet.

cetli_eval <- function(tst_res, model_res){
  model_res <- model_res[order(tst_res$objectId),]
  model_res[, (c("owner.objectId", "cetliId.objectId",
                 "objectId")) := NULL]
  tst_res[, (c("owner.objectId", "cetliId.objectId",
               "objectId")) := NULL]

  setcolorder(model_res, names(tst_res))

  probs_what_count <- (as.matrix(model_res)/
                       rowSums(as.matrix(model_res)))[as.matrix(tst_res)]

  result <- -sum(log(probs_what_count))/nrow(tst_res)

  return(result)
}
