# funções (Gráficos e tabelas descritivas): ================================== #

## Gráfico de boxplot -------------------------------------------------------- #
graf_box<-function(var_num,var_cat,legx="x",legy="y",title=""){
  a<-data.frame(Var1=var_num,Var2=var_cat)
  ggplot(a, aes(x = Var2, y = Var1,fill=Var2))+
    geom_boxplot(show.legend = F,size=0.35,outlier.size = 0.9) +
    scale_fill_brewer(palette = "Paired", direction = 1) +
    labs(title = title,
         x = legx,
         y = legy) +
    theme_bw(base_size = 10) +
    theme(panel.grid = ggplot2::element_blank(),
          plot.title = ggplot2::element_blank())
}

## Gráfico de barras --------------------------------------------------------- #
graf_bar<-function(V1,V2,legx="x",legy = NULL,legl="",title="",tipo=1, position = "dodge"){
  # 1 Normal
  if(tipo==1){a<-as.data.frame(table({{V1}},{{V2}}))
  if(is.null(legy)){legy="Frequência absoluta"}}
  # 2 Percentual coluna (Marginal-coluna)
  if(tipo==2){a<-as.data.frame(round(prop.table(table({{V1}},{{V2}}),2)*100,2))
  if(is.null(legy)){legy="Percentual coluna"}}
  # 3 Percentual linha (Marginal-linha)
  if(tipo==3){a<-as.data.frame(round(prop.table(table({{V1}},{{V2}}),1)*100,2))
  if(is.null(legy)){legy="Percentual linha"}}
  ggplot(a, aes(x = Var2, y = Freq, fill = Var1))+
    geom_bar(stat = "identity", position = position) +
    scale_fill_brewer(palette = "Paired", direction = 1) +
    labs(title = title,
         x = legx,
         fill = legl,
         y = legy) +
    theme_bw(base_size = 10) +
    theme(panel.grid = ggplot2::element_blank(),
          legend.position = "top",
          plot.title = ggplot2::element_blank(),
          legend.margin = margin(b = -7),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          legend.key.size = unit(0.5, "lines")
    )
}


## Tabela descritiva quantitativa -------------------------------------------- #
descr_num_cat<-function(var_num,var_cat){
  a<-tapply(var_num, as.factor(var_cat), function(x) c(n=sum(!is.na(x)), S = round(summary(x)[c(1,3,4,6)],3),
                                                       SD = round(sd(x,na.rm = T),3), p.Shapiro = round(shapiro.test(x)$p.value,3), falta= sum(is.na(x))))
  b<-NULL
  for (i in 1:length(levels(factor(var_cat)))) {
    b<-rbind(b,c(names(a[i]),a[[i]]))
  }
  b<-as.data.frame(b)
  colnames(b)<-c("","n","Mín.","Mediana","Média","Máx.","DP","p.Shapiro","NA's")
  return(b)
}

## Tabela e teste qualitativo ------------------------------------------------ #

analise_assoc_cat<-function(V1,V2,tipo=1,bar_legx="x",bar_legy = NULL,bar_legl="",bar_title=""){
  lista<-c()
  tab1<-table(V1,V2)
  if(tipo==1){tipe<-NULL;lista$Tipo<-"Percentual total (Graf. Freq. absoluta)"}
  if(tipo==2){tipe<-2;lista$Tipo<-"Percentual coluna"}
  if(tipo==3){tipe<-1;lista$Tipo<-"Percentual linha"}
  lista$n<-c(n_total=length(V1),`V1(NA)`=sum(is.na(V1)),`V2(NA)`=sum(is.na(V2)),
             n=nrow(na.omit(cbind(V1,V2))))
  m1<-matrix(paste0(tab1,"(",round(prop.table(tab1,tipe)*100,2),")"),
             nrow = nrow(tab1),ncol = ncol(tab1))
  m1<-cbind(c(paste0(table(V1),"(",round(prop.table(table(V1))*100,2),")")),m1)
  m2<-rbind(c(rep("n(%)",ncol(tab1)+1)),m1)
  colnames(m2)<-c("-",colnames(tab1))
  row.names(m2)<-c("",row.names(tab1))
  lista$Tab<-tibble::rownames_to_column(as.data.frame(m2),var = "--")
  cq<-suppressWarnings(chisq.test(V1,V2))
  if(any(cq$expected < 5)){
    lista$Fisher_test<-fisher.test(V1,V2)
  }else{
    lista$Chi_squared<-cq
  }
  lista$g_bar<-graf_bar(V1,V2,legl = bar_legl, legy = bar_legy,
                        tipo = tipo,legx = bar_legx,title = bar_title)
  return(lista)
}

## Tabela e teste qualitativo x quantitativo --------------------------------- #
analise_2grupo<- function(var_valor,var_cat,legx_box="x",legy_box="y",
                          title_box="",pareado=FALSE,id=NULL,cond_norm=TRUE){
  lista<-c()
  g1<-graf_box(var_num = var_valor,var_cat = var_cat,legx = legx_box,
               legy = legy_box,title = title_box)
  lista$Tab_descr<-descr_num_cat(var_num = var_valor,var_cat = var_cat)
  if(any(lista$Tab_descr$p.Shapiro<0.05) | cond_norm == FALSE){
    lista$Tipo<-"Teste Não Paramétrico:"
    if(pareado==FALSE){
      lista$Mann_Whitney<-wilcox.test(var_valor ~ var_cat, exact = FALSE)
    }else{
      lista$Wilcoxon<-wilcox.test(var_valor ~ var_cat, exact = TRUE)
    }
  }else{
    if(pareado==FALSE){
      lista$Supos<-data.frame(Teste=c("Bartlett homogeneity"),
                              p_value=round(c(bartlett.test(var_valor ~ var_cat)$p.value),4))
      if(lista$Supos$p_value<0.05){
        lista$Teste_t_var_dif<-t.test(var_valor ~ var_cat,var.equal = FALSE)
      }else{
        lista$Teste_t_var_igual<-t.test(var_valor ~ var_cat,var.equal = TRUE)
      }
    }else{
      if(is.null(id)){stop("Precisa do identificador 'id'")}
      d1<-data.frame(id=id,V1=as.factor(var_cat),V2=var_valor) |>
        tidyr::pivot_wider(names_from = V1,values_from = V2)
      V1 <- as.vector(unlist(d1[,2])); V2 <- as.vector(unlist(d1[,3]))
      lista$Teste_t_pareado<-t.test(x = V1,y = V2, paired = TRUE)
    }
  }
  lista$g_box<-g1
  return(lista)
}
