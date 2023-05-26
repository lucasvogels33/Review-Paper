###read metrics ####

setwd("C:/Users/lvogels/OneDrive - UvA/Documents/PhD-Project-Lucas/0. Projects/0. Review paper/Code/Results/results/p10")
source("read_metrics_functions.R")

#########################################################################################
## create files (for p=10, all n, all graphs, that contain p_plus and p_minus vectors)###
#########################################################################################

p_list = c(10)
n_list = c(10,20,100)
graph_list = c("random","cluster","scale-free")
rep_list = 1:50

#already done edge_inclusion_prob(p_list=p_list,n_list=n_list,graph_list=graph_list,rep_list=rep_list)

p_list = c(10)
n_list = c(10,20,100)
graph_list = c("random","cluster","scale-free")
file_p_plus = "p_plus_p10.txt"
file_p_min = "p_min_p10.txt"
round_nr = 2
main="mean"
brackets="median"
na.rm=TRUE

edge_inclusion_prob_table(p_list=p_list,n_list=n_list,graph_list=graph_list,
                                     file_p_plus=file_p_plus,file_p_min=file_p_min,
                                     round_nr=round_nr,main=main,brackets=brackets,na.rm=na.rm)

p_list = c(10)
n_list = c(10,20,100)
graph_list = c("random","cluster","scale-free")
rep_list = 1:50
AUCname = "AUC_Marit_p10.txt"
convname = "Time_Marit_p10.txt"
round_nr = 2
main="mean"
brackets="median"
na.rm=TRUE
epsilon = 0.01

AUC_table_Marit(p_list=p_list,n_list=n_list,graph_list=graph_list,rep_list=rep_list,
                convname=convname,AUCname=AUCname,round_nr=round_nr,
                main=main,brackets=brackets,na.rm=na.rm,
                epsilon=epsilon)




#################
## make tables###
#################

#set which metrics we want to see
AUC_ROC = TRUE
AUC_PR = FALSE
AUC_ROC_calibrated = FALSE
CE = FALSE
CE_weighed = FALSE
MSE = FALSE
MSE_weighed = TRUE
TP = TRUE
FP = TRUE
TN = FALSE
FN = FALSE
time = FALSE

#set file names
AUC_ROCname = "AUC_ROC_table_p10.txt"
AUC_PRname = "AUC_PR_table_p10.txt"
AUC_ROC_calibratedname = "AUC_ROC_calibrated_table_p10.txt"
CEname = "CE_table_p10.txt" 
CE_weighedname = "CE_weighed_table_p10.txt" 
MSEname = "MSE_table_p10.txt" 
MSE_weighedname = "MSE_weighed_table_p10.txt" 
TPname = "TP_table_p10.txt" 
FPname = "FP_table_p10.txt" 
TNname = "TN_table_p10.txt" 
FNname = "FN_table_p10.txt" 
timename = "time_table_p10.txt" 

#set over which parameters we want to average
rep_list = 1:50
n_list = c(10,20,100)
p_list = 10
graph_list = c("random","cluster","scale-free")

#set other parameters
round_nr = 4

latex_tables(p_list=p_list,n_list=n_list,graph_list=graph_list,rep_list=rep_list,
             AUC_ROC=AUC_ROC,AUC_PR=AUC_PR,AUC_ROC_calibrated=AUC_ROC_calibrated,
             CE=CE,CE_weighed=CE_weighed,
             MSE=MSE,MSE_weighed=MSE_weighed,
             TP=TP,FP=FP,TN=TN,FN=FN,
             time = time,
             AUC_ROCname = AUC_ROCname,
             AUC_PRname = AUC_PRname,
             AUC_ROC_calibratedname = AUC_ROC_calibratedname,
             CEname = CEname,
             CE_weighedname = CE_weighedname,
             MSEname = MSEname,
             MSE_weighedname = MSE_weighedname,
             TPname = TPname,
             FPname = FPname,
             TNname = TNname,
             FNname = FNname,
             timename = timename,
              round_nr = 2)

#############################
## make convergence plots ###
#############################
p_list=c(10)
n_list=c(100)
graph_list=c("random")
rep_list=c(3)
AUC_ROC=TRUE
AUC_PR=FALSE
AUC_ROC_calibrated=FALSE
CE=FALSE
CE_weighed=FALSE
MSE=FALSE
MSE_weighed=FALSE
TP=FALSE
FP=FALSE
TN=FALSE
FN=FALSE
plinks_max_diff=TRUE
x_time = FALSE  #if TRUE, then the x-axis is wallclocktime, be careful, because it average the time per iteration over all lists. 
                #So only set to TRUE if the time per iteration is similar accross the p,n,graphs and rep mentioned in the lists
x_log = FALSE
xlim=c(0,20000)
ylim=c(0,1)
legend=TRUE
xlim_log=c(0.01,10)


convergence_plots_one(p_list=p_list,n_list=n_list,graph_list=graph_list,rep_list=rep_list,
                  AUC_ROC=AUC_ROC,AUC_PR=AUC_PR,AUC_ROC_calibrated=AUC_ROC_calibrated,
                  CE=CE,CE_weighed=CE_weighed,
                  MSE=MSE,MSE_weighed=MSE_weighed,
                  TP=TP,FP=FP,TN=TN,FN=FN,plinks_max_diff=plinks_max_diff,
                  x_time = x_time,x_log = x_log,xlim=xlim,ylim=ylim,legend=legend,xlim_log=xlim_log)

##########################################################
## make convergence plots, one method per plot ###
##########################################################
p=10
n_list=c(10,20,100)
graph="random"
method_list=c("mpl_bd","mpl_rj","bd","rj","ss")
rep_list=1:5
AUC_ROC=TRUE
AUC_PR=FALSE
AUC_ROC_calibrated=FALSE
CE=FALSE
CE_weighed=FALSE
MSE=FALSE
MSE_weighed=FALSE
TP=FALSE
FP=FALSE
TN=FALSE
FN=FALSE
plinks_max_diff=FALSE
x_time = FALSE
x_log = FALSE
xlim=c(0,100000)
ylim=c(0,1)
legend=FALSE
xlim_log=NULL

for (n in n_list){
  for (method in method_list){
    convergence_plots_two(p=p,n=n,graph=graph,method=method,rep_list=rep_list,
                  AUC_ROC=AUC_ROC,AUC_PR=AUC_PR,AUC_ROC_calibrated=AUC_ROC_calibrated,
                  CE=CE,CE_weighed=CE_weighed,
                  MSE=MSE,MSE_weighed=MSE_weighed,
                  TP=TP,FP=FP,TN=TN,FN=FN,plinks_max_diff=plinks_max_diff,                             
                  x_time = x_time,x_log = x_log,xlim=xlim,ylim=ylim,legend=legend,xlim_log=xlim_log)
  }
}

##########################################################
## make scatterplots of AUC and MSE#######################
##########################################################
p_list=c(10)
n_list=c(10)
graph_list=c("random")
rep_list= 1:50
xlim = c(0,1)
ylim = c(0,1)
legend = TRUE 



metric_scatter(p_list=p_list,n_list=n_list,graph_list=graph_list,rep_list=rep_list,xlim=xlim,ylim=ylim,legend=legend)

##########################################################
## convergence of AUC and MSE#######################
##########################################################
p_list=c(10)
n_list=c(100)
graph_list=c("cluster")
rep_list=c(1:50)
epsilon = 0.01
AUC_ROC = TRUE
MSE_weighed = FALSE
convergence(p_list=p_list,n_list=n_list,graph_list=graph_list,rep_list=rep_list,epsilon=epsilon,AUC_ROC=AUC_ROC,MSE_weighed=MSE_weighed)

##########################################################
## convergence of AUC and MSE#######################
##########################################################
p_list=c(10)
n_list=c(100)
graph_list=c("cluster")
rep_list=1:20
epsilon = 0.01
AUC_ROC = TRUE
MSE_weighed = FALSE
time_unit = FALSE
AUC_ROC_time_name = "AUC_ROC_time_table_p10.txt"
MSE_weighed_time_name = "MSE_weighed_time_table_p10.txt" 
AUC_ROC_iter_name = "AUC_ROC_iter_table_p10.txt"
MSE_weighed_iter_name = "MSE_weighed_iter_table_p10.txt" 
stat = "max"

convergence_tables(p_list=p_list,n_list=n_list,graph_list=graph_list,rep_list=rep_list,
                   epsilon=epsilon,AUC_ROC=AUC_ROC,MSE_weighed=MSE_weighed,time_unit=time_unit,
                   AUC_ROC_time_name = AUC_ROC_time_name,
                   MSE_weighed_time_name = MSE_weighed_time_name,
                   AUC_ROC_iter_name = AUC_ROC_iter_name,
                   MSE_weighed_iter_name = MSE_weighed_iter_name,
                   stat = stat,
                   round_nr = 5)






