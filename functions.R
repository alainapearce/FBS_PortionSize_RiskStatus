# This script was written by Alaina Pearce in January 2022
# to set up functions needed for the Flux 2022 abstract examining 
# the portion size effect and TESQ-E
#
#     Copyright (C) 2022 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

## gtsummary table functions ####

my_ttest <- function(data, variable, by, ...) {
  round(t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value, 3)
}

my_chifisher <- function(data, variable, by, ...) {
  tab <- xtabs(~data[[variable]] + data[[by]])
  if (min(tab) <= 5){
    round(fisher.test(tab)$p.value, 3)
  } else {
    round(chisq.test(tab)$p.value, 3)
  }
}


##extracts standard deviation table for DV (either 1 variable or a vector of variables)--for more information on how tapply works, use the R help or RStudio help menu
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Block)
sd.function = function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd))
  return(sd)
}

sd.function.na = function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  return(sd)
}


##extracts standard error table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to er
##  eg. er=se.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
se.function=function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd))
  length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
  er=sd/sqrt(length)
  return(er)
}

se.function.na=function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
  er=sd/sqrt(length)
  return(er)
}

##extracts mean table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to means
##  eg. means=means.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
means.function = function(data, DV, IV){
  means=with(data, tapply(DV, IV, mean))
  return(means)
}

means.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, mean, na.rm=T))
  return(means)
}

#### Graphing ####

##make bar graph with standard error bars and is designed to be used in congunction with the means and se functions above.  In this case it will only work if your DV vector has 2 or less variables.  If graphing a 3-way interaction see other function for splitting data sets by factors

##--group=0 if only have 1 DV, if DV is multiple variables, group is the variable name for the grouping one
##if group =! 0, it means you have two DV's/a DV and a covariate. The first variable listed in your DV vector will be represtened by different colors in the legend. This will be the "group" variable and will create side by side bars. The second variable will have levels represented on x-axis. note: xpd=False restrains bars to graphic pane (can truncate lower part of graph)
bar_graph.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }

  else {
    #palette(c("steelblue4", "lightsteelblue2", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    palette(c("blue", "cadetblue1", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)

  }
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix=function(var_vector, var_names, method, alpha){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      if (hasArg(alpha)){
        p_res=round(cor.test(x,y, na.rm=TRUE, method = method)$p.value, 3)
        c_res=round(cor.test(x,y, na.rm=TRUE, method = method)$estimate, 2)
      } else {
        p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
        c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      }
      
      if (hasArg(alpha)){
        if (p_res>alpha){
          c_res="NA"
        }
        
        res_matrix[icount,jcount]=c_res
        
      } else {
        if (p_res <= 0.05){
          res_matrix[icount,jcount]=paste0(c_res, '*')
        } else{
          res_matrix[icount,jcount]=c_res
        }
      }
    }
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}
