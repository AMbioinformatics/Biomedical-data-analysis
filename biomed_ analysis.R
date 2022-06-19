library(Hmisc)
library(dplyr)
library(ggpubr)
library(dunn.test)
library(FSA)
library(car)
#library(ggstastplot)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Nalezy podac co najmniej jeden argument wejsciowy")
}
All_data <- read.csv2(file=args[1], header=TRUE)

#All_data <- read.csv2("przykladoweDane-Projekt.csv",sep=";")
#All_data <- read.csv2("2_grupy.csv",sep=";")

#Zliczanie grup
group <- All_data[1,1]
counter <- 1 #licznik grup, najpierw mamy jedn¹ grupê

for(i in 1: (length(All_data[,1])))
{
  if (All_data[i,1]!= group){
    counter <- counter + 1
    group <- All_data[i,1]
  }
}
cat(paste("How many groups: ", counter,"\n"))

#Uzupelnienie brakow danych
vector_mean<-vector()
mean_value <- vector()
column <- vector()
row <- vector()
parameter <- vector()
group <- vector()
count <- 0

for (i in 1:(length(All_data[,1]))){
  for (j in 1:(length(All_data[1,]))){
    if(is.numeric(All_data[,j])){ #lub [1,j]
    if(is.na(All_data[i,j])){
      count <- count +1
      for(k in 1:(length(All_data[,1]))){
        if (All_data[k,1] == All_data[i,1]){
          vector_mean<-append(vector_mean,All_data[k,j])
        }
      }
     #cat(paste0("Not Available Data in row: ",i,", column: ",j, ", average for ",colnames(All_data)[j]," ",All_data[i,1],":\n"))
     mean_value <- append(mean_value,mean(vector_mean,na.rm=TRUE))
     group <- append(group,All_data[i,1])
     column <- append(column,j)
     row <- append(row,i)
     parameter <- append(parameter,colnames(All_data)[j])
     #print(mean(vector_mean,na.rm=TRUE))
     All_data[i,j]<-mean(vector_mean,na.rm=TRUE)
     #all<-impute(All_data[i,j],mean(vector_mean,na.rm=TRUE))
     vector_mean<-vector()
    }
  }
  }
}
cat(paste("How many Not Available data:", count,"\n"))
not_available_data_frame <- data.frame(group, parameter, column, row,mean_value)
names(not_available_data_frame) <- c('Group','Parameter', 'Column','Row','Mean')
print(not_available_data_frame)
write.csv2(not_available_data_frame, file="Not_available_data.csv",row.names = FALSE)
All_data

#Informacje o wartosciach odstajacych dla danych parametrow
pdf("Outliers.pdf")
par(mfrow = c(2,2)) 
group <- vector()
outliers <- vector()
rows <- vector()
parameter <- vector()
column <- vector()
for ( i in 1:(length(All_data[1,]))){
  column_name<-colnames(All_data)[i]
  if(is.numeric(All_data[1,i])){
    if(length(boxplot.stats(All_data[,i])$out)!=0){
      boxplot(All_data[,i],ylab="",outcol="palevioletred4",pch = 19,cex=1,col="plum3")
      title(ylab = column_name, line = 2.5)
      mtext(line=1,cex=0.7,paste("Outlier: ", paste(boxplot.stats(All_data[,i])$out, collapse = ", ")))
      mtext(line=1,cex=0.7,side=1,paste("Row: ",paste(which(All_data[,i] %in% c(boxplot.stats(All_data[,i])$out)),collapse = ", ")))
      outliers <- append(outliers,boxplot.stats(All_data[,i])$out)
      rows <- append(rows,which(All_data[,i] %in% c(boxplot.stats(All_data[,i])$out)))
      for (value in which(All_data[,i] %in% c(boxplot.stats(All_data[,i])$out))){
        group <- append(group,All_data[value,1]) 
        parameter <- append(parameter,column_name)
        column <- append(column,i)
      }
    } else {cat(paste("For",column_name,"there are no outliers\n"))
      boxplot(All_data[,i],ylab="",outcol="palevioletred4",pch = 19,cex=1,col="plum3")
      title(ylab = column_name, line = 2.5)
      mtext(line=1,cex=0.7,"No outliers")
    }
  }
}
outliers_frame <- data.frame(group,parameter,outliers,rows,column)
names(outliers_frame) <- c('Group','Parameter', 'Outlier', 'Row','Column')
outliers_frame
dev.off()
write.csv2(outliers_frame, file="Outliers_frame.csv",row.names = FALSE)

#Charakterystyki dla badanych grup
table_for_groups <- data.frame()
#qualitative_data <- data.frame()
pdf("Characteristic.pdf")
par(mfrow = c(2,1))

for(i in 1:(length(All_data[1,]))){
  if(is.numeric(All_data[,i])){
   summation <- group_by(All_data,group = All_data[,1]) %>%
     summarise(
       parameter = colnames(All_data)[i],
       amount_of_value = n(),
       mean = format (round(mean(.data[[colnames(All_data)[i]]]),3),nsmall = 3),
       sd = format(round(sd(.data[[colnames(All_data)[i]]]), 3), nsmall = 3),
       median = format(round(median(.data[[colnames(All_data)[i]]]), 3), nsmall = 3),
       IQR = format(round(IQR(.data[[colnames(All_data)[i]]]), 3), nsmall = 3),
       var = format(round(var(.data[[colnames(All_data)[i]]]), 3), nsmall = 3),
       min = format(round(min(.data[[colnames(All_data)[i]]]), 3), nsmall = 3),
       max = format(round(max(.data[[colnames(All_data)[i]]]), 3), nsmall = 3),
       Qu._1st. = format(round(quantile(.data[[colnames(All_data)[i]]],probs = 0.25), 3), nsmall = 3),
       Qu._3rd. = format(round(quantile(.data[[colnames(All_data)[i]]],probs = 0.75), 3), nsmall = 3),
      )
    table_for_groups <- rbind(table_for_groups,summation)
    boxplot(All_data[,i] ~ All_data[,1], outcol="palevioletred4",pch = 19, cex = 0.7,
            col =c('plum1','plum','plum4','plum3','purple','rosybrown'),
            xlab = 'group',
            ylab = colnames(All_data)[i])
    #print(summation)
  }
  if(!is.numeric(All_data[,i])& i!=1){
    count <- 1
    #cat(paste("For", colnames(All_data)[i], ":"))
    #print(table(group = All_data[,1],All_data[,i]))
    qualitative_data <- as.data.frame.matrix(table(group=All_data[,1],All_data[,i]))
    print(qualitative_data)
    }
}


#write.csv2(ok, file="hallo.csv",row.names = TRUE)
print(table_for_groups)
sorted_table_for_groups <- table_for_groups %>% 
  group_by(table_for_groups[,1]) %>% 
  arrange(table_for_groups[,1])
print(sorted_table_for_groups)
write.csv2(sorted_table_for_groups, file="Characteristic_for_groups.csv",row.names = FALSE)
dev.off()

#Dodatkowe wykresy - zaznaczenie outliers
plot_outliers_characteristic <- list()
count_plot <- 1
for(i in 1:(length(All_data[1,]))){
  if(is.numeric(All_data[,i])){
      plot <- print(ggstatsplot::ggbetweenstats(
      data = All_data,
      x = !!colnames(All_data)[1],
      y = !!colnames(All_data)[i],
      results.subtitle = FALSE,
      outlier.tagging=TRUE,
      pairwise.comparisons = FALSE,
      centrality.point.args = list(size = 3, color = "darkorchid4")
      ))+ ggplot2::scale_color_manual(values = c("darkorchid1","hotpink4","orchid4","plum1","plum","plum4"))   
       plot_outliers_characteristic[[count_plot]] <- plot
    count_plot <- count_plot +1
  }
}
ggexport( 
  plotlist = plot_outliers_characteristic, filename = "Characteristic_plots.pdf",
  ncol = 1, nrow = 2
)
dev.off

#Porownywanie grup - analiza statystyczna
if (counter < 2){
  cat("Statistical comparsion is not possible. Too few groups.\n")
} 
if (counter >= 2){
  count <- 1
  plot_list = list()
  wektor_shapiro <- vector()
  wektor_levene <- vector()
  count_shapiro <- 0
  count_levene <-0
  temporary_vector<- vector()
  values_list <- list()
  
  table_for_shapiro <- data.frame()
  vector_consistent_with_normal <- vector()
  
  vector_parameter_levene <- vector()
  vector_homogeneity_of_variance <- vector()
  vector_p_value_levene <- vector()
  
  for(i in 1:(length(All_data[1,]))){
    if(is.numeric(All_data[,i])){
      #Shapiro - test
      shapiro_test <- group_by(All_data,group = All_data[,1]) %>%
        summarise(
          parameter = colnames(All_data)[i],
          p.value = round(shapiro.test(.data[[colnames(All_data)[i]]])$p.value,5)
        )
      print(shapiro_test)
      table_for_shapiro <- rbind(table_for_shapiro,shapiro_test)
      for (j in 1:(length(shapiro_test$p.value))){
        if(shapiro_test$p.value[j] < 0.05){
          cat(paste(shapiro_test$p.value[j], "< 0.05 - inconsistent with the normal distribution\n"))
          temporary_vector <- append(temporary_vector,0)
          vector_consistent_with_normal <- append(vector_consistent_with_normal,'NO')
        }else{
          cat(paste(shapiro_test$p.value[j], "> 0.05 - consistent with the normal distribution\n"))
          count_shapiro <- count_shapiro +1
          temporary_vector <- append(temporary_vector,1)
          vector_consistent_with_normal <- append(vector_consistent_with_normal,'YES')
        }
      }
      values_list[[count]] <-temporary_vector   
      #values_data_frame <- cbind(values_data_frame, new_col = temporary_vector) 
      wektor_shapiro <- append(wektor_shapiro, count_shapiro)
      count_shapiro <- 0
      temporary_vector <- vector()
      
      #Wykresy gestosci
      plot <- print(ggdensity(All_data,x = colnames(All_data)[i],
               color = colnames(All_data)[1],
               fill = colnames(All_data)[1],
               palette = c("mediumorchid4", "orchid3","plum","bisque","thistle","'rosybrown'"),
               ylab = "density",
               xlab = colnames(All_data)[i],
      ) 
      + facet_wrap(~All_data[,1],scales="free")
      + font("x.text", size = 8)
      + font("y.text",size = 9))
      plot_list[[count]] <- plot
      count <- count+1

  
      #Levene - test
      p_value <- round(leveneTest(All_data[,i]~All_data[,1],data=All_data)$"Pr(>F)"[1],6)
      cat(paste("Test Levene'a - for" ,colnames(All_data[i]), "p.value is", p_value,"\n"))
      vector_parameter_levene <- append(vector_parameter_levene,colnames(All_data[i]))
      vector_p_value_levene <- append (vector_p_value_levene,p_value)
      if(p_value < 0.05){
        cat(paste(p_value, "< 0.05 - inhomogeneity of variance\n"))
        vector_homogeneity_of_variance <- append(vector_homogeneity_of_variance,'NO')
      }else{
        cat(paste(p_value, "> 0.05 - homogeneity of variance\n"))
        count_levene <- count_levene +1
        vector_homogeneity_of_variance <- append(vector_homogeneity_of_variance,'YES')
      }
      wektor_levene <- append(wektor_levene,count_levene)
      count_levene <- 0
    }
  }
  
}
values_matrix <-  do.call("rbind",values_list)
#values_matrix


#Wykresy gestosci zapisane do pdf
ggexport( 
  plotlist = plot_list, filename = "Normal_distribution.pdf",
  ncol = 1, nrow = 2
)
dev.off()

#wektor_shapiro
#wektor_levene
table_for_shapiro <- cbind(table_for_shapiro, new_col = vector_consistent_with_normal)
colnames(table_for_shapiro)[which(names(table_for_shapiro) == "new_col")] <- "consistent with the normal distribution"
table_for_shapiro
table_for_levene <- data.frame(vector_parameter_levene,vector_p_value_levene,vector_homogeneity_of_variance)
names(table_for_levene) <- c('parameter','p.value','homogeneity of variance')
table_for_levene
write.csv2(table_for_shapiro, file="Table_shapiro.csv",row.names = FALSE)
write.csv2(table_for_levene, file="Table_levene.csv",row.names = FALSE)


#TESTY
if (counter>2){
  vector_parameter <- vector()
  vector_test <- vector()
  vector_pvalue <- vector()
  vector_difference <- vector()
  vector_cons_diff <- vector()
  vector_parameter_cons_diff <- vector()
  vector_groups_cons_diff<- vector()
  vector_which_post_hoc <- vector()
  count_2 <- 1
  plot_more_than_2_list <- list()
  
  count <- 1
  for(i in 1:(length(All_data[1,]))){
    if(is.numeric(All_data[,i])){
      if (wektor_shapiro[count]==counter & wektor_levene[count]==1){ #zgodne z rozkladen normalnym i jednorodnosc wariancji - test ANOVA
        cat(paste("\nParameter :",colnames(All_data)[i],"- consistent with the normal distribution,homogeneity of variance - ANOVA test\n"))
        #print(aov(All_data[,i]~All_data[,1]))
        #print(summary(aov(All_data[,i]~All_data[,1],data=All_data)))
        pvalue_Anova_test <- summary(aov(All_data[,i]~All_data[,1],data=All_data))[[1]][["Pr(>F)"]][[1]]
        #print(pvalue_Anova_test)
        if (pvalue_Anova_test<0.05){
          cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Anova_test, "< 0.05 - there are differences among groups\n\n" ))
          vector_difference <- append (vector_difference, 'YES')
          groups <- All_data[,1]
          print(TukeyHSD(aov(All_data[,i]~groups)))
          tukey_test <- TukeyHSD(aov(All_data[,i]~groups))
          tukey_test_data_frame <- data.frame(Reduce(rbind, tukey_test))
          for (j in 1:length( tukey_test_data_frame[,4])){
            if(tukey_test_data_frame[,4][j]<0.05){
              vector_cons_diff <- append(vector_cons_diff,tukey_test_data_frame[,4][j])
              vector_parameter_cons_diff <- append(vector_parameter_cons_diff,colnames(All_data)[i])
              vector_groups_cons_diff <- append(vector_groups_cons_diff,row.names(tukey_test_data_frame)[j])
              vector_which_post_hoc <- append(vector_which_post_hoc,'Tukeya')
            }
          }
        }else{
          cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Anova_test, "> 0.05 - there are no differences among groups\n\n" ))
          vector_difference <- append (vector_difference, 'NO')
        }
        vector_parameter <- append(vector_parameter,colnames(All_data)[i])
        vector_test <- append (vector_test, "ANOVA")
        vector_pvalue <- append(vector_pvalue, pvalue_Anova_test )
      }else{
        cat(paste("\nParameter:",colnames(All_data)[i],"-inconsistent with the normal distribution,inhomogeneity of variance - Kruskal test\n"))
        #print(kruskal.test(All_data[,i]~All_data[,1]))
        pvalue_Kruskal_test <- kruskal.test(All_data[,i]~All_data[,1])$p.value
        if (pvalue_Kruskal_test<0.05){
          cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Kruskal_test, "< 0.05 - there are differences among groups\n\n" ))
          vector_difference <- append (vector_difference, 'YES')
          print(dunnTest(All_data[,i],All_data[,1]))
          dunn_test <- dunnTest(All_data[,i],All_data[,1])
          plot <- print(ggstatsplot::ggbetweenstats(
                  data = All_data,
                  x = !!colnames(All_data)[1],
                  y = !!colnames(All_data)[i],
                  results.subtitle = TRUE,
                  outlier.tagging=TRUE,
                  type = 'np',
                   centrality.point.args = list(size = 3, color = "darkorchid4")
                 ))+ ggplot2::scale_color_manual(values = c("darkorchid1","hotpink4","orchid4","plum1","plum","plum4"))   
          plot_more_than_2_list[[count_2]] <- plot
          count_2 <- count_2+1
          dunn_test_data_frame <- do.call(rbind.data.frame,dunn_test)
          for (j in 1:length(dunn_test_data_frame$P.adj)){
            if(dunn_test_data_frame$P.adj[j]<0.05){
              vector_cons_diff <- append(vector_cons_diff,dunn_test_data_frame$P.adj[j])
              vector_parameter_cons_diff <- append(vector_parameter_cons_diff,colnames(All_data)[i])
              vector_groups_cons_diff <- append(vector_groups_cons_diff,dunn_test_data_frame[j,1])
              vector_which_post_hoc <- append(vector_which_post_hoc,'Dunna')
            }
          }
        }else{
          cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Kruskal_test, "> 0.05 - there are no differences among groups\n\n" ))
          vector_difference <- append (vector_difference, 'NO')
        }
        vector_parameter <- append(vector_parameter,colnames(All_data)[i])
        vector_test <- append (vector_test, "Kruskala")
        vector_pvalue <- append(vector_pvalue, pvalue_Kruskal_test )
      }
      count <- count + 1
    }
  }
  tests_frame <- data.frame(vector_parameter, vector_test, vector_pvalue,vector_difference)
  names(tests_frame) <- c('Parameter','Test', 'P-value','Significant differences')
  print(tests_frame)
  cat("\n\n")
  which_group_differ_frame <- data.frame(vector_groups_cons_diff,vector_parameter_cons_diff,vector_which_post_hoc, vector_cons_diff)
  names(which_group_differ_frame) <- c('Groups','Parameter','Post hoc test', 'P-value')
  which_group_differ_frame
  if(length(plot_more_than_2_list)){
  ggexport( 
    plotlist = plot_more_than_2_list, filename = "More_than_2_groups_plots.pdf",
    ncol = 1, nrow = 1
  )
  dev.off()
  }
  write.csv2(tests_frame, file="Tests_table.csv",row.names = FALSE)
  write.csv2(which_group_differ_frame, file="Which_groups_differ_table.csv",row.names = FALSE)
}



#Wykresy dla danych jakosciowych
pdf("Qualitative_data.pdf")
par(mfrow = c(2,1))
for(i in 1:(length(All_data[1,]))){
  if(!is.numeric(All_data[,i])& i!=1){
    chisq.test(All_data[,1], All_data[,i])
    pvalue <- chisq.test(All_data[,1],All_data[,i])$p.value
    
    barplot(table(All_data[,i], All_data[,1]),
            beside = TRUE,
            col = c("plum2", "plum4"),
            xlab = 'group',
            ylab = colnames(All_data)[i],
            legend.text = TRUE,
            args.legend = list(x = "topleft", bty = "n", inset=c(0.85, -0.4))
    )
    mtext(line=0.5,cex=1,paste("P-value", round(pvalue, digits = 3)))
  }
}
dev.off()


if (counter==2){
  vector_parameter <- vector()
  vector_test <- vector()
  vector_pvalue <- vector()
  vector_difference <- vector()
  count <- 1
  count_2 <- 1
  plot_2_test_list <- list()
  
  for(i in 1:(length(All_data[1,]))){
   if(is.numeric(All_data[,i])){
    if (wektor_shapiro[count]==counter & wektor_levene[count]==1){
      cat(paste("\nParameter :",colnames(All_data)[i],"- consistent with the normal distribution,homogeneity of variance -t-Student test\n"))
      #print(t.test(All_data[,i]~All_data[,1],data=All_data,var.equal = TRUE))
      pvalue_t_test <- t.test(All_data[,i]~All_data[,1],var.equal = TRUE)$p.value
      if (pvalue_t_test<0.05){
        cat(paste("P-value for", colnames(All_data)[i],":", pvalue_t_test, "< 0.05 - there are differences between groups\n\n" ))
        vector_difference <- append (vector_difference, 'YES')
        plot <- print(ggstatsplot::ggbetweenstats(
                 data = All_data,
                 x = !!colnames(All_data)[1],
                 y = !!colnames(All_data)[i],
                 results.subtitle = TRUE,
                 var.equal = TRUE,
                 outlier.tagging=TRUE,
                 type = 'p',
                 centrality.point.args = list(size = 3, color = "darkorchid4")
               ))+ ggplot2::scale_color_manual(values = c("darkorchid1","hotpink4","orchid4","plum1","plum","plum4")) 
        plot_2_test_list[[count_2]] <- plot
        count_2 <- count_2+1
      }else{
        cat(paste("P-value for", colnames(All_data)[i],":", pvalue_t_test, "> 0.05 - there are no differences between groups\n\n" ))
        vector_difference <- append (vector_difference, 'NO')
      }
      vector_parameter <- append(vector_parameter,colnames(All_data)[i])
      vector_test <- append (vector_test, "t-Studenta")
      vector_pvalue <- append(vector_pvalue, pvalue_t_test )
    }
     if(wektor_shapiro[count]==counter & wektor_levene[count]==0){
       cat(paste("\nParameter :",colnames(All_data)[i],"- consistent with the normal distribution,inhomogeneity of variance - Welch test\n"))
       #print(pvalue_Welcha_test <- t.test(All_data[,i]~All_data[,1],data=All_data,var.equal = FALSE))
       pvalue_Welcha_test <- t.test(All_data[,i]~All_data[,1],var.equal = FALSE)$p.value
       if (pvalue_Welcha_test<0.05){
         cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Welcha_test, "< 0.05 - there are differences between groups\n\n" ))
         vector_difference <- append (vector_difference, 'YES')
         plot <- print(ggstatsplot::ggbetweenstats(
                        data = All_data,
                        x = !!colnames(All_data)[1],
                        y = !!colnames(All_data)[i],
                        results.subtitle = TRUE,
                        type = 'p',
                        outlier.tagging=TRUE,
                       centrality.point.args = list(size = 3, color = "darkorchid4")
                 ))+ ggplot2::scale_color_manual(values = c("darkorchid1","hotpink4","orchid4","plum1","plum","plum4"))   
           plot_2_test_list[[count_2]] <- plot
           count_2 <- count_2+1
      }else{
         cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Welcha_test, "> 0.05 - there are no differences between groups\n\n" ))
         vector_difference <- append (vector_difference, 'NO')
       }
       vector_parameter <- append(vector_parameter,colnames(All_data)[i])
       vector_test <- append (vector_test, "Welcha")
       vector_pvalue <- append(vector_pvalue, pvalue_Welcha_test )
     }
     if(wektor_shapiro[count]!=counter){
       cat(paste("\nParameter :",colnames(All_data)[i],"- inconsistent with the normal distribution- Wilcoxon test\n"))
       #print(pvalue_Wilcoxona_test <- wilcox.test(All_data[,i]~All_data[,1]))
       pvalue_Wilcoxona_test <- wilcox.test(All_data[,i]~All_data[,1])$p.value
       if (pvalue_Wilcoxona_test<0.05){
         cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Wilcoxona_test, "< 0.05 - there are differences between groups\n\n" ))
         vector_difference <- append (vector_difference, 'YES')
         plot <- print(ggstatsplot::ggbetweenstats(
                      data = All_data,
                      x = !!colnames(All_data)[1],
                      y = !!colnames(All_data)[i],
                      results.subtitle = TRUE,
                      outlier.tagging=TRUE,
                      type = 'np',
                      centrality.point.args = list(size = 3, color = "darkorchid4")
              ))+ ggplot2::scale_color_manual(values = c("darkorchid1","hotpink4","orchid4","plum1","plum","plum4")) 
          plot_2_test_list[[count_2]] <- plot
          count_2 <- count_2+1
       }else{
         cat(paste("P-value for", colnames(All_data)[i],":", pvalue_Wilcoxona_test, "> 0.05 - there are no differences between groups\n\n" ))
         vector_difference <- append (vector_difference, 'NO')
       }
       vector_parameter <- append(vector_parameter,colnames(All_data)[i])
       vector_test <- append (vector_test, "Wilcoxona")
       vector_pvalue <- append(vector_pvalue, pvalue_Wilcoxona_test )
       }
     count <- count +1
   }
  }
  tests_frame <- data.frame(vector_parameter, vector_test, vector_pvalue,vector_difference)
  names(tests_frame) <- c('Parameter','Test', 'P-value','Significant differences')
  print(tests_frame)
  if(length(plot_2_test_list)){
    ggexport( 
    plotlist = plot_2_test_list, filename = "2_groups_plots.pdf",
    ncol = 1, nrow = 1
  )
  dev.off()
  write.csv2(tests_frame, file="Tests_table.csv",row.names = FALSE)
  }
}



#Testy korelacji
vector_group <- vector()
vector_first_parameter <- vector()
vector_second_parameter <- vector()
vector_estimate <- vector()
vector_p_value <- vector()
vector_test <- vector()
correlation <- vector()


which_group <- All_data[length(All_data[,1]),1]
l<-1

for(k in 1: (length(All_data[,1])))
{
  if (All_data[k,1]!= which_group){
    which_group <- All_data[k,1]
    group <- All_data %>% filter(All_data[,1] == All_data[k,1])
    count <- 1
    count_2 <- 2
    for(i in 1:(length(All_data[1,])-1)){
      if(is.numeric(All_data[,i])){
        for (j in (i+1): length(All_data[1,])){
          if( is.numeric(All_data[,j])){
            vector_group <- append(vector_group,All_data[k,1])
            vector_first_parameter <- append(vector_first_parameter,colnames(All_data)[i])
            vector_second_parameter <- append(vector_second_parameter,colnames(All_data)[j])
            if (values_matrix[count,l]==1 & values_matrix[count_2,l]==1){
              #cat(paste("\nParamters: ",colnames(All_data)[i],"and", colnames(All_data)[j],"\n"))
              #cat(paste("Consistent with the normal distribution - Pearosn test\n"))
              Pearson_test <- cor.test(group[,i], group[,j], method = "pearson")
              #cat(paste("Estimate: ",Pearson_test$estimate),"\n")
              #cat(paste("P-value: ",Pearson_test$p.value),"\n")
              vector_estimate <- append(vector_estimate,round(Pearson_test$estimate,6))
              vector_p_value <- append(vector_p_value,round(Pearson_test$p.value,6))
              vector_test <- append(vector_test,"Pearsona")
              if(Pearson_test$p.value < 0.05){
                correlation <- append(correlation, 'YES')
              }else{
                correlation <- append(correlation, 'NO')
              }
            }else{
              #cat(paste("\nParamters: ",colnames(All_data)[i],"and", colnames(All_data)[j],"\n"))
              #cat(paste("Consistent with the normal distribution - Spearman test\n"))
              Spearman_test <- cor.test(group[,i], group[,j], method = "spearman")
              #cat(paste("Estimate: ",Spearman_test$estimate),"\n")
              #cat(paste("P-value: ",Spearman_test$p.value),"\n")
              vector_estimate <- append(vector_estimate,round(Spearman_test$estimate,6))
              vector_p_value <- append(vector_p_value,round(Spearman_test$p.value,6))
              vector_test <- append(vector_test,"Spearmana")
              if(Spearman_test$p.value< 0.05){
                correlation <- append(correlation, 'YES')
              }else{
                correlation <- append(correlation, 'NO')
              }
            }
            count_2 <- count_2 +1
          }
        }
        count <- count + 1
        count_2 <- count +1
      }
    }
    l <- l+1
  }
}
correlation_frame <- data.frame(vector_group, vector_first_parameter, vector_second_parameter,vector_test, vector_estimate,vector_p_value,correlation)
names(correlation_frame) <- c('Group','First parameter', 'Second parameter','Test','Estimate','P-value','Significant correlation')
correlation_frame
write.csv2(correlation_frame, file="Correlation_table.csv",row.names = FALSE)

#Sila korelacji, kierunek i wykresy
vector_2_group <- vector()
vector_2_first_parameter <- vector()
vector_2_second_parameter <- vector()
vector_2_estimate <- vector()
vector_2_correlation <- vector()
vector_2_strength <- vector()
count <- 1
plot_2_list <- list()


for (i in 1:length(correlation_frame[,7])){
  if (correlation_frame[i,7] == 'YES'){
    vector_2_group <- append(vector_2_group,correlation_frame[i,1])
    vector_2_first_parameter <- append(vector_2_first_parameter,correlation_frame[i,2])
    vector_2_second_parameter <- append(vector_2_second_parameter,correlation_frame[i,3])
    vector_2_estimate <- append(vector_2_estimate,correlation_frame[i,5])
    if(correlation_frame$Estimate[i]>0){
      vector_2_correlation <- append(vector_2_correlation,'positive correlation')
    }
    if(correlation_frame$Estimate[i]==0){
      vector_2_correlation <- append(vector_2_correlation,'no correlation')
    }
    if (correlation_frame$Estimate[i]<0){
      vector_2_correlation <- append(vector_2_correlation,'negative correlation')
    }
    if(correlation_frame$Estimate[i]>-1 & correlation_frame$Estimate[i]< (-0.7)){
      vector_2_strength <- append(vector_2_strength,'very strong negative correlation')
    }
    if(correlation_frame$Estimate[i]>-0.7 & correlation_frame$Estimate[i]<(-0.5)){
      vector_2_strength <- append(vector_2_strength,'strong negative correlation')
    }
    if(correlation_frame$Estimate[i]>-0.5 & correlation_frame$Estimate[i]<(-0.3)){
      vector_2_strength <- append(vector_2_strength,'average negative correlation')
    }
    if(correlation_frame$Estimate[i]>-0.3 & correlation_frame$Estimate[i]<(-0.2)){
      vector_2_strength <- append(vector_2_strength,'weak negative correlation')
    }
    if(correlation_frame$Estimate[i]>-0.2 & correlation_frame$Estimate[i]<0.2){
      vector_2_strength <- append(vector_2_strength,'no correlation')
    }
    if(correlation_frame$Estimate[i]>0.3 & correlation_frame$Estimate[i]<0.5){
      vector_2_strength <- append(vector_2_strength,'average positive correlation')
    }
    if(correlation_frame$Estimate[i]>0.5 & correlation_frame$Estimate[i]<0.7){
      vector_2_strength <- append(vector_2_strength,'strong positive correlation')
    }
    if(correlation_frame$Estimate[i]>0.7 & correlation_frame$Estimate[i]<1){
      vector_2_strength <- append(vector_2_strength,'very strong positive correlation')
    }
    if (correlation_frame[i,4] == 'Pearsona'){
      plot <- print(ggscatter(All_data %>% filter(All_data[,1] == correlation_frame[i,1]), x = correlation_frame[i,2], y = correlation_frame[i,3], 
                              add = "reg.line", conf.int = TRUE, 
                              #cor.coef = TRUE,cor.coef.size = 3,
                              cor.method = "pearson",
                              color = colnames(All_data)[1], fill = colnames(All_data)[1],
                              palette = c("darkorchid4"),
                              ylab = correlation_frame[i,2], 
                              xlab = correlation_frame[i,3]
      )) + ggtitle(paste("cor = ",round(correlation_frame[i,5],3),", p-value =",round(correlation_frame[i,6],3))) + theme(plot.title = element_text(size=10,face = "bold"))
      plot_2_list[[count]] <- plot
      count <- count+1
    }
    if (correlation_frame[i,4] == 'Spearmana'){
      plot <- print(ggscatter(All_data %>% filter(All_data[,1] == correlation_frame[i,1]), x = correlation_frame[i,2], y = correlation_frame[i,3], 
                              conf.int = TRUE, #cor.coef = TRUE,  cor.coef.size = 3,
                              cor.method = "spearman",
                              color = colnames(All_data)[1], fill = colnames(All_data)[1],
                              palette = c("darkorchid4"),
                              ylab = correlation_frame[i,2], 
                              xlab = correlation_frame[i,3]
      )) + ggtitle(paste("rho = ",round(correlation_frame[i,5],3),", p-value =",round(correlation_frame[i,6],3))) + theme(plot.title = element_text(size=10,face = "bold"))
      plot_2_list[[count]] <- plot
      count <- count+1
    }
  }
}
correlation_2_frame <- data.frame(vector_2_group, vector_2_first_parameter, vector_2_second_parameter,vector_2_estimate,vector_2_correlation,vector_2_strength)
names(correlation_2_frame) <- c('Group','First parameter', 'Second parameter','Estimate','Correlation','Strength')
correlation_2_frame
write.csv2(correlation_2_frame, file="Correlation_2_table.csv",row.names = FALSE)


ggexport( 
  plotlist = plot_2_list, filename = "Correlation_plots.pdf",
  ncol = 2, nrow = 2
)
dev.off()

# Wykresy testy jakosciowe dla grup
group <- All_data[1,1]
list_data_f <- list()
data_f <- data.frame()
c<-1
for ( i in 1:(length(All_data[,1]))){
  if (All_data[i,1] == group){
    data_f <- rbind(data_f,All_data[i,])
  } else {
    group <- All_data[i,1]
    list_data_f[[c]]<-data_f
    data_f <- data.frame()
    data_f <- rbind(data_f,All_data[i,])
    c<-c+1
  }
}
list_data_f[[c]]<-data_f



plot_3_list <- list()
count <- 1
for (k in 1:length(list_data_f)){
for(i in 1:(length(list_data_f[[k]][1,]))){
  if(!is.numeric(list_data_f[[k]][,i])& i!=1){
    for (j in 1:(length(list_data_f[[k]][1,]))){
      if(is.numeric(list_data_f[[k]][,j])){
        plot <- print(ggstatsplot::ggbetweenstats(
          data = list_data_f[[k]],
          title = list_data_f[[k]][1,1],
          x = !!colnames(list_data_f[[k]])[i],
          y = !!colnames(list_data_f[[k]])[j],
          results.subtitle = FALSE,
          outlier.tagging=TRUE,
          #centrality.plotting = FALSE,
          centrality.point.args = list(size = 3, color = "darkorchid4")
        ))+ ggplot2::scale_color_manual(values = c("darkorchid1","hotpink4","orchid4","plum1","plum","plum4"))   
        plot_3_list[[count]] <- plot
        count <- count+1
      }
    }
  }
}
}
ggexport( 
  plotlist = plot_3_list, filename = "Qualitative_data_additional_2.pdf",
  ncol = 1, nrow = 2
)
dev.off()


#Wykresy - testy jakosciowe dla prob
plot_3_list <- list()
count <- 1
for(i in 1:(length(All_data[1,]))){
  if(!is.numeric(All_data[,i])& i!=1){
    for (j in 1:(length(All_data[1,]))){
      if(is.numeric(All_data[,j])){
         plot <- print(ggstatsplot::ggbetweenstats(
         data = All_data,
         x = !!colnames(All_data)[i],
         y = !!colnames(All_data)[j],
         results.subtitle = FALSE,
         outlier.tagging=TRUE,
         #centrality.plotting = FALSE,
         centrality.point.args = list(size = 3, color = "darkorchid4")
       ))+ ggplot2::scale_color_manual(values = c("darkorchid1","hotpink4","orchid4","plum1","plum","plum4"))   
         plot_3_list[[count]] <- plot
         count <- count+1
      }
    }
  }
}

ggexport( 
  plotlist = plot_3_list, filename = "Qualitative_data_additional.pdf",
  ncol = 1, nrow = 2
)
dev.off()
