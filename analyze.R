# togroup - группируемый столбец или вектор столбцов
# togrouptype - типы группируемых столбцов
# groupby - группирующий столбец или вектор столбцов
# groupbytype - типы группирующих столбцов
# datasource - источник данных
# datarowstart - номер строки с которой начинаются данные
# типы для колонок: кач - качественный, кол- количественный, пор- порядковый
analyze <-function ( togroup, togrouptype,groupby, groupbytype, datasource, datarowstart){
  tgl <-length(togrouptype)
  gbl <-length(groupbytype)
  tg <- paste0("X",togroup)
  gb <- paste0("X", groupby)
  tgv <-datasource[tg] #matrix(datasource[tg], ncol=tgl, byrow=F)
  gbv <-datasource[gb] #matrix(groupby, ncol=gbl, byrow=F)
  row_names <- c(gbv[datarowstart-1,])
  col_names <- c(tgv[datarowstart-1,])
  # res матрица с результатами
  res <- matrix(ncol=tgl, nrow=gbl)
  rownames(res) <- row_names
  colnames(res) <- col_names
  result_list<-list()
  for (i in 1:tgl){
    for (j in 1:gbl){
      # Удаление NA по строкам из togroup
      cur_tg <- as.numeric(tgv[datarowstart:(nrow(tgv)),i][!is.na(tgv[datarowstart:(nrow(tgv)),i])])
      cur_gb <- gbv[datarowstart:nrow(gbv),j][!is.na(tgv[datarowstart:(nrow(tgv)),i])]
      # Удаление NA по строкам из groupby
      cur_tg <- cur_tg[!is.na(cur_gb)]
      cur_gb <- cur_gb[!is.na(cur_gb)]
      v<-factor(cur_gb)
      if (length(levels(v))==2){
        if (togrouptype[i]=="кач" || togrouptype[i]=="пор"){
          #Mann - Whitney
          if (check(cur_tg)){
            res[j,i] <- 'Все значения одинаковые'
            next
          } else {
            result<-wilcox.test(cur_tg ~ cur_gb, data=datasource)
          }
        }
        if (togrouptype[i]=="кол"){
          if (nrow(tgv)<=1000){
            #Fisher
            result<-fisher.test(cur_tg, cur_gb)
          } else {
            #Xi2 Pirsona
            result<-chisq.test(cur_tg, cur_gb)
          }  
        }
      } else {
        if (length(levels(v))<2){
          res[j,i] <- 'После удаления NA осталась 1 группа'
          next
        } else{
          if (togrouptype[i]=="кол" || togrouptype[i]=="пор"){
            #Kruskal — Wallis
            if (check(cur_tg)){
              res[j,i] <- 'Все значения одинаковые'
              next
            } else {
              result<-kruskal.test(cur_tg ~ as.factor(cur_gb))
              p <- posthoc.kruskal.nemenyi.test(cur_tg, as.factor(cur_gb), method="Tukey")
              result_list <- c(result_list,list(table_name=colnames(res)[i], table = format(p$p.value,"p")))
            }
          }
        }
      }
      res[j,i] <- format(result$p.value,"p")
    }
  }
  result_list<- c(result_list,list(table_name="Сравнение групп", table =  res))
  #print(res)
  #print(result_list)
  return (result_list)
}
