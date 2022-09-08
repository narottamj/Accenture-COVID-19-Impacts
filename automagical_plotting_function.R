#Hey team, I figured I would make your graphing lives a little easier this week. 
#Let me know if you have any questions but it should be fairly straightforward.


#Packages needed
library(ggplot2) #Make graph
library(ggthemes) #Make graph look good

#This code creates some mock data
set.seed(1)
row_num = 1000
df <- data.frame(Variable1= rnorm(row_num, mean=50, sd=10))
df$Variable2 <- rnorm(row_num, mean=150, sd=20)
df$Variable3 <- rnorm(row_num, mean=70, sd=5)
df$Variable4 <- rnorm(row_num, mean=1, sd=2)
a <- c(0,1)
df$actual <- sample(a, row_num, replace=TRUE)
df$predicted <- sample(a, row_num, replace=TRUE)
df$Variable1 <- ifelse(df$actual != df$predicted, df$Variable1 + 50, df$Variable1 )
df$Variable2 <- ifelse(df$actual != df$predicted, df$Variable2 -30, df$Variable2 )
df$Variable3 <- ifelse(df$actual != df$predicted, df$Variable3 +20, df$Variable3 )
df$Variable4 <- ifelse(df$actual != df$predicted, df$Variable4 -5, df$Variable4 )

#This is a superfulous function I made that just adds a little copyright logo, feel free to remove
add_copyright <- function() {
  a=format(Sys.Date(), "%Y")
  ggplot2::labs(caption= sprintf("Copyright \u00A9 %s Accenture. All rights reserved", a))
}


#This looks unwieldy, but it rocks, trust me
#This function assumes the entire df should be plotted
#df should only have plottable columns,a predicted column, and an actual/truth column
amazing_auto_magical_function_thingy <- function(df, predicted, actual, graph_type= "boxplot"){
  #Create a status of our prediction column
  df$Pred_Status <- ifelse(df$actual == 1 & df$predicted == 1, 'True Positive', NA) 
  df$Pred_Status <- ifelse(df$actual == 0 & df$predicted == 0, 'True Negative', df$Pred_Status)
  df$Pred_Status <- ifelse(df$actual == 0 & df$predicted == 1, 'False Positive', df$Pred_Status)
  df$Pred_Status <- ifelse(df$actual == 1 & df$predicted == 0, 'False Negative', df$Pred_Status)
  
  #create a list of columns we want to loop through and plot
  cols_remove <- c(predicted, actual, 'Pred_Status')
  cols_loop <- df[, !(colnames(df) %in% cols_remove)]
  cols_loop <- colnames(cols_loop)
  
  #For each column we want to plot, print that plot
  for (col_loop in cols_loop){ 
    plot_df <- data.frame(var_col = df[, c(col_loop)])
    plot_df$Pred_Status <- as.factor(df$Pred_Status)
    
    if (graph_type == "histogram"){
      plot_df$Pred_Status <- factor(df$Pred_Status, levels = c("True Positive", "True Negative", 
                                                               "False Positive", "False Negative"))
      
      #I have nothing to say about this chunk except it makes graph look gud
      print(ggplot(data= plot_df) + aes(x= var_col ) + geom_histogram(size=1.25, fill= "#A100FF", color="black", bins=30) +
              theme_fivethirtyeight() +  facet_wrap("Pred_Status") + 
              ggtitle(sprintf("%s Boxplot Distribution\n", col_loop)) + 
              theme(axis.title=element_text(size=16, face="bold"),
                    axis.text.x=element_text(size=10),
                    plot.title= element_text(hjust=.5, size=20, face="bold"),
                    strip.text.x = element_text(size=14))+ 
              xlab("\nX") + ylab('Count\n') + add_copyright())
    } 
    else if (graph_type == "facet_boxplot") {
      plot_df$Pred_Status <- factor(df$Pred_Status, levels = c("True Positive", "True Negative", 
                                                               "False Positive", "False Negative"))
      
      #I have nothing to say about this chunk except it makes graph look gud
      print(ggplot(data= plot_df) + aes(x= var_col) + geom_boxplot(size=1.25, color= "#A100FF") +
              theme_fivethirtyeight() + facet_wrap("Pred_Status") + 
              ggtitle(sprintf("%s Boxplot Distribution\n", col_loop)) + 
              theme(axis.title=element_text(size=16, face="bold"), 
                    plot.title= element_text(hjust=.5, size=20, face="bold"),
                    axis.text.y = element_blank(), 
                    strip.text.x = element_text(size=14)) +
              xlab("X") + add_copyright())
    }
    else if (graph_type == "boxplot"){
      plot_df$Pred_Status <- factor(df$Pred_Status, levels = c("False Negative","False Positive",
                                                               "True Positive", "True Negative"))
      
      #I have nothing to say about this chunk except it makes graph look gud
      print(ggplot(data= plot_df) + aes(x= Pred_Status, y= var_col ) + geom_boxplot(size=1.25, color= "#A100FF") +
              coord_flip() + theme_fivethirtyeight() +  
              ggtitle(sprintf("%s Boxplot Distribution\n", col_loop)) + 
              theme(axis.title=element_text(size=16, face="bold"),
                    axis.text.y=element_text(size=12, face="bold"),
                    plot.title= element_text(hjust=.5, size=20, face="bold"),
                    axis.title.y = element_blank())+
              ylab("\nX") + add_copyright())
      
    }
    else{print("Nuh uh uh, you didn't provide a valid graph type. Try again :)")}
  }
}

#You can select from 3 graph types, though histogram is my favorite
amazing_auto_magical_function_thingy(df, "predicted", "actual", graph_type= "histogram")
amazing_auto_magical_function_thingy(df, "predicted", "actual", graph_type= "boxplot")
amazing_auto_magical_function_thingy(df, "predicted", "actual", graph_type= "facet_boxplot")

#If you're wondering why it only outputted one variable 4 graph it's because
#the function prints all plots at once, so just use the back arrow on the plotting
#window in R studio

#Happy plotting!

