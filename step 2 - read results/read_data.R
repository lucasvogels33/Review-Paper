library(BDgraph)
library(PRROC)
source("read_data_functions.R")

#--set parameters
graph_list = c("cluster","scale-free")
n_list = c(10,20,100)
p_list = c(10)
rep_list = 1:50
thin = 100
plinks_diff = 1000
cut_AUC_calibrated = 200
cutoff=0.5


##---------------------------------------------------------------------------|

for (graph in graph_list){
  for (n in n_list){
    for (p in p_list){
      for (rep in rep_list){
        output_list = read_data(n=n,p=p,graph=graph,rep=rep,thin=thin,
                                plinks_diff=plinks_diff,cut_AUC_calibrated=cut_AUC_calibrated,cutoff=cutoff)
        filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
        save(output_list, file = filename )
      }
    }
  }
}

