
#calculate p_ij(+) and p_ij(-)
func_edge_inclusion = function(predictor=c(),response=c()){
  
  if (length(response) != length(predictor)) {
    stop("response and predictor vector must be of same length")
  }
  
  #determine length of vectors
  qp = length(response)
  
  #determine amount of zeroes and ones
  ones = sum(response)
  zeroes = length(response)-ones
  
  #calculate the weighed CE and weighed MSE
  ones_index = which(response==1)
  zeroes_index = which(response==0)
  predictor_ones =  predictor[ones_index]
  predictor_zeroes = predictor[zeroes_index]
  p_plus = mean(predictor_ones)
  p_min = mean(predictor_zeroes)
  
  #return the metrics
  return(list(p_plus=p_plus,p_min=p_min))
}

#function to calculate the statistics of a vector
stat_func=function(vec=c(1,2,3),stat="mean",round_nr=2,na.rm=FALSE){
  if (stat =="mean"){
    return(round(mean(vec,na.rm=na.rm),round_nr))
  }
  if (stat =="median"){
    return(round(median(vec,na.rm=na.rm),round_nr))
  }
  if (stat =="max"){
    return(round(max(vec,na.rm=na.rm),round_nr))
  }
  if (stat =="sd"){
    return(round(sd(vec,na.rm=na.rm),round_nr))
  }
  
}

#calculate p_ij(+) and p_ij(-) for all p,n,graphs and replications
edge_inclusion_prob = function(p_list=c(10),n_list=c(10),graph_list=c("cluster"),rep_list=1:50){
  for (p in p_list){
    cat("working on p=",p)
    for (n in n_list){
      cat("working on n=",n)
      for (graph in graph_list){
        cat("working on graph=",graph)
        
        p_plus_mpl_bd=c()
        p_plus_mpl_rj=c()
        p_plus_bd=c()
        p_plus_rj=c()
        p_plus_ss=c()
        
        p_min_mpl_bd=c()
        p_min_mpl_rj=c()
        p_min_bd=c()
        p_min_rj=c()
        p_min_ss=c()
        
        count = 0
        for (rep in rep_list){
          cat("working on rep=",rep)
          count = count + 1
          
          filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
          load(file = filename)
          response = output_list$obj_true$response
          
          ##mpl_bd##
          predictor =  output_list$obj_mpl_bd$plinks
          obj_edge_inclusion = func_edge_inclusion(predictor=predictor,response=response)
          p_plus_mpl_bd=c(p_plus_mpl_bd,obj_edge_inclusion$p_plus)
          p_min_mpl_bd=c(p_min_mpl_bd,obj_edge_inclusion$p_min)
          
          ##mpl_rj##
          predictor =  output_list$obj_mpl_rj$plinks
          obj_edge_inclusion = func_edge_inclusion(predictor=predictor,response=response)
          p_plus_mpl_rj=c(p_plus_mpl_rj,obj_edge_inclusion$p_plus)
          p_min_mpl_rj=c(p_min_mpl_rj,obj_edge_inclusion$p_min)
          
          ##bd##
          predictor =  output_list$obj_bd$plinks
          obj_edge_inclusion = func_edge_inclusion(predictor=predictor,response=response)
          p_plus_bd=c(p_plus_bd,obj_edge_inclusion$p_plus)
          p_min_bd=c(p_min_bd,obj_edge_inclusion$p_min)
          
          ##rj##
          predictor =  output_list$obj_rj$plinks
          obj_edge_inclusion = func_edge_inclusion(predictor=predictor,response=response)
          p_plus_rj=c(p_plus_rj,obj_edge_inclusion$p_plus)
          p_min_rj=c(p_min_rj,obj_edge_inclusion$p_min)
          
          ##ss##
          predictor =  output_list$obj_ss$plinks
          obj_edge_inclusion = func_edge_inclusion(predictor=predictor,response=response)
          p_plus_ss=c(p_plus_ss,obj_edge_inclusion$p_plus)
          p_min_ss=c(p_min_ss,obj_edge_inclusion$p_min)
          
        }
        
        output = list()
        output$p_plus_mpl_bd = p_plus_mpl_bd
        output$p_min_mpl_bd = p_min_mpl_bd
        output$p_plus_mpl_rj = p_plus_mpl_rj
        output$p_min_mpl_rj = p_min_mpl_rj
        output$p_plus_bd = p_plus_bd
        output$p_min_bd = p_min_bd
        output$p_plus_rj = p_plus_rj
        output$p_min_rj = p_min_rj
        output$p_plus_ss = p_plus_ss
        output$p_min_ss = p_min_ss
        
        filename = paste0("p_plus_min_p",p,"_n",n,"_",graph,".Rdata")
        save(output, file = filename )  
      
      }
      
    }
      
  }
  
}

#output matrices for p_ij(+) and p_ij(-)
edge_inclusion_prob_table = function(p_list=c(10),n_list=c(10),graph_list=c("cluster"),file_p_plus="test.txt",file_p_min="test.txt",round_nr=2,main="mean",brackets="median",na.rm=FALSE){
  
  cat("graph & p & n & MPL-RJ & MPL-BD & BD & SS \\\\",file=file_p_plus,"\n", append = TRUE )
  cat("\\hline",file=file_p_plus,"\n", append = TRUE )
  
  cat("graph & p & n & MPL-RJ & MPL-BD & BD & SS \\\\",file=file_p_min,"\n", append = TRUE )
  cat("\\hline",file=file_p_min,"\n", append = TRUE )
  
  for (graph in graph_list){
    
    cat(graph," &",  file=file_p_plus, append = TRUE )
    cat(graph," &",  file=file_p_min, append = TRUE )
    
    for (p in p_list){
      cat(p," &",  file=file_p_plus, append = TRUE )
      cat(p," &",  file=file_p_min, append = TRUE )
      for (n in n_list){
      
        filename = paste0("p_plus_min_p",p,"_n",n,"_",graph,".Rdata")
        load(file=filename)
        
        p_plus_main_mpl_rj = stat_func(vec=output$p_plus_mpl_rj,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_plus_brackets_mpl_rj = stat_func(vec=output$p_plus_mpl_rj,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        p_min_main_mpl_rj = stat_func(vec=output$p_min_mpl_rj,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_min_brackets_mpl_rj = stat_func(vec=output$p_min_mpl_rj,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        p_plus_main_mpl_bd = stat_func(vec=output$p_plus_mpl_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_plus_brackets_mpl_bd = stat_func(vec=output$p_plus_mpl_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        p_min_main_mpl_bd = stat_func(vec=output$p_min_mpl_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_min_brackets_mpl_bd = stat_func(vec=output$p_min_mpl_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        p_plus_main_bd = stat_func(vec=output$p_plus_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_plus_brackets_bd = stat_func(vec=output$p_plus_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        p_min_main_bd = stat_func(vec=output$p_min_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_min_brackets_bd = stat_func(vec=output$p_min_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        p_plus_main_ss = stat_func(vec=output$p_plus_ss,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_plus_brackets_ss = stat_func(vec=output$p_plus_ss,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        p_min_main_ss = stat_func(vec=output$p_min_ss,stat=main,round_nr=round_nr,na.rm=na.rm)
        p_min_brackets_ss = stat_func(vec=output$p_min_ss,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        prestring = ""
        if (n > p){
          prestring = "& &"
        }
        filename = file_p_plus
        cat(prestring,n," & ",
            p_plus_main_mpl_rj," (",p_plus_brackets_mpl_rj,") & ",
            p_plus_main_mpl_bd," (",p_plus_brackets_mpl_bd,") & ",
            p_plus_main_bd," (",p_plus_brackets_bd,") & ",
            p_plus_main_ss," (",p_plus_brackets_ss,") \\\\",
            file = filename,"\n", append = TRUE )
        
        filename = file_p_min
        cat(prestring,n," & ",
            p_min_main_mpl_rj," (",p_min_brackets_mpl_rj,") & ",
            p_min_main_mpl_bd," (",p_min_brackets_mpl_bd,") & ",
            p_min_main_bd," (",p_min_brackets_bd,") & ",
            p_min_main_ss," (",p_min_brackets_ss,") \\\\",
            file = filename,"\n", append = TRUE )
        
      }
    }  
  }
  
}

#output matrices for AUC (for Marit)
AUC_table_Marit = function(p_list=c(10),n_list=c(10),graph_list=c("cluster"),rep_list=1:10,convname="test.txt",AUCname="test.txt",round_nr=2,main="mean",brackets="median",na.rm=FALSE,epsilon=0.01){
  
  filename = AUCname
  cat("graph & p & n & MPL-RJ & MPL-BD & BD & SS \\\\",file=filename,"\n", append = TRUE )
  cat("\\hline",file=filename,"\n", append = TRUE )
  
  filename = convname
  cat("graph & p & n & MPL-RJ & MPL-BD & BD & SS \\\\",file=filename,"\n", append = TRUE )
  cat("\\hline",file=filename,"\n", append = TRUE )
  
  for (graph in graph_list){
    cat("working on graph=",graph)
    cat(graph," &",  file=AUCname, append = TRUE )
    cat(graph," &",  file=convname, append = TRUE )
    
    for (p in p_list){
      cat("working on p=",p)
      cat(p," &",  file=AUCname, append = TRUE )
      cat(p," &",  file=convname, append = TRUE )
  
      for (n in n_list){
        
        #initiate vectors
        len = length(rep_list)
        AUC_ROC_vec_mpl_rj= rep(0,len)
        AUC_ROC_vec_mpl_bd = rep(0,len)
        AUC_ROC_vec_bd= rep(0,len)
        AUC_ROC_vec_ss= rep(0,len)
        
        AUC_ROC_conv_vec_mpl_rj= rep(0,len)
        AUC_ROC_conv_vec_mpl_bd = rep(0,len)
        AUC_ROC_conv_vec_bd= rep(0,len)
        AUC_ROC_conv_vec_ss= rep(0,len)
          
        #load data into the vectors
        i = 0
        for (rep in rep_list){
          #print progress
          print(paste0("working on rep=",rep))
          
          #counter
          i = i + 1
          
          #load file
          filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
          load(file = filename)
          
          
          time_per_iter_mpl_rj = output_list$obj_mpl_rj$time/output_list$obj_mpl_rj$iter
          time_per_iter_mpl_bd = output_list$obj_mpl_bd$time/output_list$obj_mpl_bd$iter
          time_per_iter_bd = output_list$obj_bd$time/output_list$obj_bd$iter
          time_per_iter_ss = output_list$obj_ss$time/output_list$obj_ss$iter
          
          #mpl_rj
          AUC_ROC_final = as.numeric(output_list$obj_mpl_rj$AUC_ROC) 
          AUC_ROC_vec = output_list$obj_mpl_rj$AUC_ROC_vec 
          
          diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec 
          conv_index = min(which(diff_AUC_ROC_vec < epsilon))
          iter_vec_thin = output_list$obj_mpl_rj$iter_vec_thin
          conv_iteration = iter_vec_thin[conv_index]
          
          AUC_ROC_vec_mpl_rj[i] = AUC_ROC_final
          AUC_ROC_conv_vec_mpl_rj[i] = conv_iteration*time_per_iter_mpl_rj
            
          #mpl_bd
          AUC_ROC_final = as.numeric(output_list$obj_mpl_bd$AUC_ROC) 
          AUC_ROC_vec = output_list$obj_mpl_bd$AUC_ROC_vec 
          
          diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec 
          conv_index = min(which(diff_AUC_ROC_vec < epsilon))
          iter_vec_thin = output_list$obj_mpl_bd$iter_vec_thin
          conv_iteration = iter_vec_thin[conv_index]
          
          AUC_ROC_vec_mpl_bd[i] = AUC_ROC_final
          AUC_ROC_conv_vec_mpl_bd[i] = conv_iteration*time_per_iter_mpl_bd
          
          #bd
          AUC_ROC_final = as.numeric(output_list$obj_bd$AUC_ROC) 
          AUC_ROC_vec = output_list$obj_bd$AUC_ROC_vec 
          
          diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec 
          conv_index = min(which(diff_AUC_ROC_vec < epsilon))
          iter_vec_thin = output_list$obj_bd$iter_vec_thin
          conv_iteration = iter_vec_thin[conv_index]
          
          AUC_ROC_vec_bd[i] = AUC_ROC_final
          AUC_ROC_conv_vec_bd[i] = conv_iteration*time_per_iter_bd
          
          #ss
          AUC_ROC_final = as.numeric(output_list$obj_ss$AUC_ROC) 
          AUC_ROC_vec = output_list$obj_ss$AUC_ROC_vec 
          
          diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec 
          conv_index = min(which(diff_AUC_ROC_vec < epsilon))
          iter_vec_thin = output_list$obj_ss$iter_vec_thin
          conv_iteration = iter_vec_thin[conv_index]
          
          AUC_ROC_vec_ss[i] = AUC_ROC_final
          AUC_ROC_conv_vec_ss[i] = conv_iteration*time_per_iter_ss
          
        } #end rep
        
        AUC_main_mpl_rj = stat_func(vec=AUC_ROC_vec_mpl_rj,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_brackets_mpl_rj = stat_func(vec=AUC_ROC_vec_mpl_rj,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_main_mpl_rj = stat_func(vec=AUC_ROC_conv_vec_mpl_rj,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_brackets_mpl_rj = stat_func(vec=AUC_ROC_conv_vec_mpl_rj,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        AUC_main_mpl_bd = stat_func(vec=AUC_ROC_vec_mpl_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_brackets_mpl_bd = stat_func(vec=AUC_ROC_vec_mpl_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_main_mpl_bd = stat_func(vec=AUC_ROC_conv_vec_mpl_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_brackets_mpl_bd = stat_func(vec=AUC_ROC_conv_vec_mpl_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        AUC_main_bd = stat_func(vec=AUC_ROC_vec_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_brackets_bd = stat_func(vec=AUC_ROC_vec_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_main_bd = stat_func(vec=AUC_ROC_conv_vec_bd,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_brackets_bd = stat_func(vec=AUC_ROC_conv_vec_bd,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        AUC_main_ss = stat_func(vec=AUC_ROC_vec_ss,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_brackets_ss = stat_func(vec=AUC_ROC_vec_ss,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_main_ss = stat_func(vec=AUC_ROC_conv_vec_ss,stat=main,round_nr=round_nr,na.rm=na.rm)
        AUC_conv_brackets_ss = stat_func(vec=AUC_ROC_conv_vec_ss,stat=brackets,round_nr=round_nr,na.rm=na.rm)
        
        prestring = ""
        if (n > p){
          prestring = "& &"
        }
        filename = AUCname
        cat(prestring,n," & ",
            AUC_main_mpl_rj," (",AUC_brackets_mpl_rj,") & ",
            AUC_main_mpl_bd," (",AUC_brackets_mpl_bd,") & ",
            AUC_main_bd," (",AUC_brackets_bd,") & ",
            AUC_main_ss," (",AUC_brackets_ss,") \\\\",
            file = filename,"\n", append = TRUE )
        
        filename = convname
        cat(prestring,n," & ",
            AUC_conv_main_mpl_rj," (",AUC_conv_brackets_mpl_rj,") & ",
            AUC_conv_main_mpl_bd," (",AUC_conv_brackets_mpl_bd,") & ",
            AUC_conv_main_bd," (",AUC_conv_brackets_bd,") & ",
            AUC_conv_main_ss," (",AUC_conv_brackets_ss,") \\\\",
            file = filename,"\n", append = TRUE )
        
      }
    }  
  }
  
}

#the function below produces the latex tables for all metrics
latex_tables = function(p_list=c(10),n_list=c(10),graph_list=c("random"),rep_list=1:10,
             AUC_ROC=TRUE,AUC_PR=FALSE,AUC_ROC_calibrated=FALSE,
             CE=FALSE,CE_weighed=FALSE,
             MSE=FALSE,MSE_weighed=FALSE,
             TP=FALSE,FP=FALSE,TN=FALSE,FN=FALSE,
             time = FALSE,
             AUC_ROCname = "AUC_ROC_table_p10.txt",
             AUC_PRname = "AUC_PR_table_p10.txt",
             AUC_ROC_calibratedname = "AUC_ROC_calibrated_table_p10.txt",
             CEname = "CE_table_p10.txt" ,
             CE_weighedname = "CE_weighed_table_p10.txt" ,
             MSEname = "MSE_table_p10.txt" ,
             MSE_weighedname = "MSE_weighed_table_p10.txt" ,
             TPname = "TP_table_p10.txt" ,
             FPname = "FP_table_p10.txt" ,
             TNname = "TN_table_p10.txt" ,
             FNname = "FN_table_p10.txt" ,
             timename = "time_table_p10.txt" ,
             round_nr = 2)
  {
    for (p in p_list)
    
    print(paste0("working on p=",p))
  
    {
      for( n in n_list ){
      print(paste0("working on n=",n))
      
        if (AUC_ROC){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = AUC_ROCname,"\n", append = TRUE )
        }
        if (AUC_PR){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = AUC_PRname,"\n", append = TRUE )
        }
        if (AUC_ROC_calibrated){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = AUC_ROC_calibratedname,"\n", append = TRUE )
        }
        if (CE){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = CEname,"\n", append = TRUE )
        }
        if (CE_weighed){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = CE_weighedname,"\n", append = TRUE )
        }
        if (MSE){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = MSEname,"\n", append = TRUE )
        }
        if (MSE_weighed){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = MSE_weighedname,"\n", append = TRUE )
        }
        if (TP){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = TPname,"\n", append = TRUE )
        }
        if (FP){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = FPname,"\n", append = TRUE )
        }
        if (TN){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = TNname,"\n", append = TRUE )
        }
        if (FN){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = FNname,"\n", append = TRUE )
        }
        if (time){
          cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = timename,"\n", append = TRUE )
        }
        
        for( graph in graph_list ){
          
          print(paste0("working on graph=",graph))
          
          #initiate vectors
          len = length(rep_list)
          if (AUC_ROC){
            AUC_ROC_vec_mpl_bd = rep(0,len)  
            AUC_ROC_vec_mpl_rj = rep(0,len)  
            AUC_ROC_vec_bd = rep(0,len)  
            AUC_ROC_vec_rj = rep(0,len)  
            AUC_ROC_vec_ss = rep(0,len)  
          }
          if (AUC_PR){
            AUC_PR_vec_mpl_bd = rep(0,len)  
            AUC_PR_vec_mpl_rj = rep(0,len)  
            AUC_PR_vec_bd = rep(0,len)  
            AUC_PR_vec_rj = rep(0,len)  
            AUC_PR_vec_ss = rep(0,len)  
          }
          if (AUC_ROC_calibrated){
            AUC_ROC_calibrated_vec_mpl_bd = rep(0,len)  
            AUC_ROC_calibrated_vec_mpl_rj = rep(0,len)  
            AUC_ROC_calibrated_vec_bd = rep(0,len)  
            AUC_ROC_calibrated_vec_rj = rep(0,len)  
            AUC_ROC_calibrated_vec_ss = rep(0,len)  
          }
          if (CE){
            CE_vec_mpl_bd = rep(0,len)  
            CE_vec_mpl_rj = rep(0,len)  
            CE_vec_bd = rep(0,len)  
            CE_vec_rj = rep(0,len)  
            CE_vec_ss = rep(0,len)  
          }
          if (CE_weighed){
            CE_weighed_vec_mpl_bd = rep(0,len)  
            CE_weighed_vec_mpl_rj = rep(0,len)  
            CE_weighed_vec_bd = rep(0,len)  
            CE_weighed_vec_rj = rep(0,len)  
            CE_weighed_vec_ss = rep(0,len)  
          }
          if (MSE){
            MSE_vec_mpl_bd = rep(0,len)  
            MSE_vec_mpl_rj = rep(0,len)  
            MSE_vec_bd = rep(0,len)  
            MSE_vec_rj = rep(0,len)  
            MSE_vec_ss = rep(0,len)  
          }
          if (MSE_weighed){
            MSE_weighed_vec_mpl_bd = rep(0,len)  
            MSE_weighed_vec_mpl_rj = rep(0,len)  
            MSE_weighed_vec_bd = rep(0,len)  
            MSE_weighed_vec_rj = rep(0,len)  
            MSE_weighed_vec_ss = rep(0,len)  
          }
          if (TP){
            TP_vec_mpl_bd = rep(0,len)  
            TP_vec_mpl_rj = rep(0,len)  
            TP_vec_bd = rep(0,len)  
            TP_vec_rj = rep(0,len)  
            TP_vec_ss = rep(0,len)  
          }
          if (FP){
            FP_vec_mpl_bd = rep(0,len)  
            FP_vec_mpl_rj = rep(0,len)  
            FP_vec_bd = rep(0,len)  
            FP_vec_rj = rep(0,len)  
            FP_vec_ss = rep(0,len)  
          }
          if (TN){
            TN_vec_mpl_bd = rep(0,len)  
            TN_vec_mpl_rj = rep(0,len)  
            TN_vec_bd = rep(0,len)  
            TN_vec_rj = rep(0,len)  
            TN_vec_ss = rep(0,len)  
          }
          if (FN){
            FN_vec_mpl_bd = rep(0,len)  
            FN_vec_mpl_rj = rep(0,len)  
            FN_vec_bd = rep(0,len)  
            FN_vec_rj = rep(0,len)  
            FN_vec_ss = rep(0,len)  
          }
          if (time){
            time_vec_mpl_bd = rep(0,len)  
            time_vec_mpl_rj = rep(0,len)  
            time_vec_bd = rep(0,len)  
            time_vec_rj = rep(0,len)  
            time_vec_ss = rep(0,len)
          }
          
          #load data into the vectors
          count = 0
          for (rep in rep_list){
            
            print(paste0("working on rep=",rep))
            
            #counter
            count = count + 1
            
            #load file
            filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
            load(file = filename)
            
            #calculate number of ones and zeroes
            response = output_list$obj_true$response
            ones = sum(response)
            zeroes = length(response)-ones
            if (ones==0){cat(graph,n,rep)}
            
            #for each metric, save the data in the corresponding vector
            if (AUC_ROC){
              AUC_ROC_vec_mpl_bd[count] = output_list$obj_mpl_bd$AUC_ROC
              AUC_ROC_vec_mpl_rj[count] = output_list$obj_mpl_rj$AUC_ROC
              AUC_ROC_vec_bd[count] = output_list$obj_bd$AUC_ROC
              AUC_ROC_vec_rj[count] = output_list$obj_rj$AUC_ROC
              AUC_ROC_vec_ss[count] = output_list$obj_ss$AUC_ROC
            }
            if (AUC_PR){
              AUC_PR_vec_mpl_bd[count] = output_list$obj_mpl_bd$AUC_PR
              AUC_PR_vec_mpl_rj[count] = output_list$obj_mpl_rj$AUC_PR
              AUC_PR_vec_bd[count] = output_list$obj_bd$AUC_PR
              AUC_PR_vec_rj[count] = output_list$obj_rj$AUC_PR
              AUC_PR_vec_ss[count] = output_list$obj_ss$AUC_PR
            }
            if (AUC_ROC_calibrated){
              AUC_ROC_calibrated_vec_mpl_bd[count] = output_list$obj_mpl_bd$AUC_ROC_calibrated
              AUC_ROC_calibrated_vec_mpl_rj[count] = output_list$obj_mpl_rj$AUC_ROC_calibrated
              AUC_ROC_calibrated_vec_bd[count] = output_list$obj_bd$AUC_ROC_calibrated
              AUC_ROC_calibrated_vec_rj[count] = output_list$obj_rj$AUC_ROC_calibrated
              AUC_ROC_calibrated_vec_ss[count] = output_list$obj_ss$AUC_ROC_calibrated
            }
            if (CE){
              CE_vec_mpl_bd[count] = output_list$obj_mpl_bd$CE
              CE_vec_mpl_rj[count] = output_list$obj_mpl_rj$CE
              CE_vec_bd[count] = output_list$obj_bd$CE
              CE_vec_rj[count] = output_list$obj_rj$CE
              CE_vec_ss[count] = output_list$obj_ss$CE
            }
            if (CE_weighed){
              CE_weighed_vec_mpl_bd[count] = output_list$obj_mpl_bd$CE_weighed
              CE_weighed_vec_mpl_rj[count] = output_list$obj_mpl_rj$CE_weighed
              CE_weighed_vec_bd[count] = output_list$obj_bd$CE_weighed
              CE_weighed_vec_rj[count] = output_list$obj_rj$CE_weighed
              CE_weighed_vec_ss[count] = output_list$obj_ss$CE_weighed
            }
            if (MSE){
              MSE_vec_mpl_bd[count] = output_list$obj_mpl_bd$MSE
              MSE_vec_mpl_rj[count] = output_list$obj_mpl_rj$MSE
              MSE_vec_bd[count] = output_list$obj_bd$MSE
              MSE_vec_rj[count] = output_list$obj_rj$MSE
              MSE_vec_ss[count] = output_list$obj_ss$MSE
            }
            if (MSE_weighed){
              MSE_weighed_vec_mpl_bd[count] = output_list$obj_mpl_bd$MSE_weighed
              MSE_weighed_vec_mpl_rj[count] = output_list$obj_mpl_rj$MSE_weighed
              MSE_weighed_vec_bd[count] = output_list$obj_bd$MSE_weighed
              MSE_weighed_vec_rj[count] = output_list$obj_rj$MSE_weighed
              MSE_weighed_vec_ss[count] = output_list$obj_ss$MSE_weighed
            }
            if (TP){
              TP_vec_mpl_bd[count] = output_list$obj_mpl_bd$TP/ones
              TP_vec_mpl_rj[count] = output_list$obj_mpl_rj$TP/ones
              TP_vec_bd[count] = output_list$obj_bd$TP/ones
              TP_vec_rj[count] = output_list$obj_rj$TP/ones
              TP_vec_ss[count] = output_list$obj_ss$TP/ones
            }
            if (FP){
              FP_vec_mpl_bd[count] = output_list$obj_mpl_bd$FP/zeroes
              FP_vec_mpl_rj[count] = output_list$obj_mpl_rj$FP/zeroes
              FP_vec_bd[count] = output_list$obj_bd$FP/zeroes
              FP_vec_rj[count] = output_list$obj_rj$FP/zeroes
              FP_vec_ss[count] = output_list$obj_ss$FP/zeroes
            }
            if (TN){
              TN_vec_mpl_bd[count] = output_list$obj_mpl_bd$TN/zeroes
              TN_vec_mpl_rj[count] = output_list$obj_mpl_rj$TN/zeroes
              TN_vec_bd[count] = output_list$obj_bd$TN/zeroes
              TN_vec_rj[count] = output_list$obj_rj$TN/zeroes
              TN_vec_ss[count] = output_list$obj_ss$TN/zeroes
            }
            if (FN){
              FN_vec_mpl_bd[count] = output_list$obj_mpl_bd$FN/ones
              FN_vec_mpl_rj[count] = output_list$obj_mpl_rj$FN/ones
              FN_vec_bd[count] = output_list$obj_bd$FN/ones
              FN_vec_rj[count] = output_list$obj_rj$FN/ones
              FN_vec_ss[count] = output_list$obj_ss$FN/ones
            }
            if (time){
              time_vec_mpl_bd[count] = output_list$obj_mpl_bd$time/output_list$obj_mpl_bd$iter
              time_vec_mpl_rj[count] = output_list$obj_mpl_rj$time/output_list$obj_mpl_rj$iter
              time_vec_bd[count] = output_list$obj_bd$time/output_list$obj_bd$iter
              time_vec_rj[count] = output_list$obj_rj$time/output_list$obj_rj$iter
              time_vec_ss[count] = output_list$obj_ss$time/output_list$obj_ss$iter
            }
          }
          
          #print mean and standard deviation of the vectors in the tables
          if (AUC_ROC){
            filename = AUC_ROCname
            cat(graph," & ",
                round(mean(AUC_ROC_vec_mpl_bd,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_vec_mpl_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_vec_mpl_rj,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_vec_mpl_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_vec_bd,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_vec_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_vec_rj,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_vec_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_vec_ss,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_vec_ss,na.rm=TRUE),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (AUC_PR){
            filename = AUC_PRname
            cat(graph," & ",
                round(mean(AUC_PR_vec_mpl_bd,na.rm=TRUE),round_nr)," (",round(sd(AUC_PR_vec_mpl_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_PR_vec_mpl_rj,na.rm=TRUE),round_nr)," (",round(sd(AUC_PR_vec_mpl_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_PR_vec_bd,na.rm=TRUE),round_nr)," (",round(sd(AUC_PR_vec_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_PR_vec_rj,na.rm=TRUE),round_nr)," (",round(sd(AUC_PR_vec_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_PR_vec_ss,na.rm=TRUE),round_nr)," (",round(sd(AUC_PR_vec_ss,na.rm=TRUE),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (AUC_ROC_calibrated){
            filename = AUC_ROC_calibratedname
            cat(graph," & ",
                round(mean(AUC_ROC_calibrated_vec_mpl_bd,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_calibrated_vec_mpl_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_calibrated_vec_mpl_rj,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_calibrated_vec_mpl_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_calibrated_vec_bd,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_calibrated_vec_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_calibrated_vec_rj,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_calibrated_vec_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(AUC_ROC_calibrated_vec_ss,na.rm=TRUE),round_nr)," (",round(sd(AUC_ROC_calibrated_vec_ss,na.rm=TRUE),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (CE){
            filename = CEname
            cat(graph," & ",
                round(mean(CE_vec_mpl_bd),round_nr)," (",round(sd(CE_vec_mpl_bd),round_nr),") & ",
                round(mean(CE_vec_mpl_rj),round_nr)," (",round(sd(CE_vec_mpl_rj),round_nr),") & ",
                round(mean(CE_vec_bd),round_nr)," (",round(sd(CE_vec_bd),round_nr),") & ",
                round(mean(CE_vec_rj),round_nr)," (",round(sd(CE_vec_rj),round_nr),") & ",
                round(mean(CE_vec_ss),round_nr)," (",round(sd(CE_vec_ss),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (CE_weighed){
            filename = CE_weighedname
            cat(graph," & ",
                round(mean(CE_weighed_vec_mpl_bd,na.rm=TRUE),round_nr)," (",round(sd(CE_weighed_vec_mpl_bd),round_nr),") & ",
                round(mean(CE_weighed_vec_mpl_rj,na.rm=TRUE),round_nr)," (",round(sd(CE_weighed_vec_mpl_rj),round_nr),") & ",
                round(mean(CE_weighed_vec_bd,na.rm=TRUE),round_nr)," (",round(sd(CE_weighed_vec_bd),round_nr),") & ",
                round(mean(CE_weighed_vec_rj,na.rm=TRUE),round_nr)," (",round(sd(CE_weighed_vec_rj),round_nr),") & ",
                round(mean(CE_weighed_vec_ss,na.rm=TRUE),round_nr)," (",round(sd(CE_weighed_vec_ss),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (MSE){
            filename = MSEname
            cat(graph," & ",
                round(mean(MSE_vec_mpl_bd),round_nr)," (",round(sd(MSE_vec_mpl_bd),round_nr),") & ",
                round(mean(MSE_vec_mpl_rj),round_nr)," (",round(sd(MSE_vec_mpl_rj),round_nr),") & ",
                round(mean(MSE_vec_bd),round_nr)," (",round(sd(MSE_vec_bd),round_nr),") & ",
                round(mean(MSE_vec_rj),round_nr)," (",round(sd(MSE_vec_rj),round_nr),") & ",
                round(mean(MSE_vec_ss),round_nr)," (",round(sd(MSE_vec_ss),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (MSE_weighed){
            filename = MSE_weighedname
            cat(graph," & ",
                round(mean(MSE_weighed_vec_mpl_bd,na.rm=TRUE),round_nr)," (",round(sd(MSE_weighed_vec_mpl_bd),round_nr),") & ",
                round(mean(MSE_weighed_vec_mpl_rj,na.rm=TRUE),round_nr)," (",round(sd(MSE_weighed_vec_mpl_rj),round_nr),") & ",
                round(mean(MSE_weighed_vec_bd,na.rm=TRUE),round_nr)," (",round(sd(MSE_weighed_vec_bd),round_nr),") & ",
                round(mean(MSE_weighed_vec_rj,na.rm=TRUE),round_nr)," (",round(sd(MSE_weighed_vec_rj),round_nr),") & ",
                round(mean(MSE_weighed_vec_ss,na.rm=TRUE),round_nr)," (",round(sd(MSE_weighed_vec_ss),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (TP){
            filename = TPname
            cat(graph," & ",
                round(mean(TP_vec_mpl_bd,na.rm=TRUE),round_nr)," (",round(sd(TP_vec_mpl_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(TP_vec_mpl_rj,na.rm=TRUE),round_nr)," (",round(sd(TP_vec_mpl_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(TP_vec_bd,na.rm=TRUE),round_nr)," (",round(sd(TP_vec_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(TP_vec_rj,na.rm=TRUE),round_nr)," (",round(sd(TP_vec_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(TP_vec_ss,na.rm=TRUE),round_nr)," (",round(sd(TP_vec_ss,na.rm=TRUE),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (FP){
            filename = FPname
            cat(graph," & ",
                round(mean(FP_vec_mpl_bd),round_nr)," (",round(sd(FP_vec_mpl_bd),round_nr),") & ",
                round(mean(FP_vec_mpl_rj),round_nr)," (",round(sd(FP_vec_mpl_rj),round_nr),") & ",
                round(mean(FP_vec_bd),round_nr)," (",round(sd(FP_vec_bd),round_nr),") & ",
                round(mean(FP_vec_rj),round_nr)," (",round(sd(FP_vec_rj),round_nr),") & ",
                round(mean(FP_vec_ss),round_nr)," (",round(sd(FP_vec_ss),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (TN){
            filename = TNname
            cat(graph," & ",
                round(mean(TN_vec_mpl_bd),round_nr)," (",round(sd(TN_vec_mpl_bd),round_nr),") & ",
                round(mean(TN_vec_mpl_rj),round_nr)," (",round(sd(TN_vec_mpl_rj),round_nr),") & ",
                round(mean(TN_vec_bd),round_nr)," (",round(sd(TN_vec_bd),round_nr),") & ",
                round(mean(TN_vec_rj),round_nr)," (",round(sd(TN_vec_rj),round_nr),") & ",
                round(mean(TN_vec_ss),round_nr)," (",round(sd(TN_vec_ss),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (FN){
            filename = FNname
            cat(graph," & ",
                round(mean(FN_vec_mpl_bd,na.rm=TRUE),round_nr)," (",round(sd(FN_vec_mpl_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(FN_vec_mpl_rj,na.rm=TRUE),round_nr)," (",round(sd(FN_vec_mpl_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(FN_vec_bd,na.rm=TRUE),round_nr)," (",round(sd(FN_vec_bd,na.rm=TRUE),round_nr),") & ",
                round(mean(FN_vec_rj,na.rm=TRUE),round_nr)," (",round(sd(FN_vec_rj,na.rm=TRUE),round_nr),") & ",
                round(mean(FN_vec_ss,na.rm=TRUE),round_nr)," (",round(sd(FN_vec_ss,na.rm=TRUE),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          if (time){
            filename = timename
            cat(graph," & ",
                round(mean(time_vec_mpl_bd),round_nr)," (",round(sd(time_vec_mpl_bd),round_nr),") & ",
                round(mean(time_vec_mpl_rj),round_nr)," (",round(sd(time_vec_mpl_rj),round_nr),") & ",
                round(mean(time_vec_bd),round_nr)," (",round(sd(time_vec_bd),round_nr),") & ",
                round(mean(time_vec_rj),round_nr)," (",round(sd(time_vec_rj),round_nr),") & ",
                round(mean(time_vec_ss),round_nr)," (",round(sd(time_vec_ss),round_nr),")"," \\","\\",sep="",
                file = filename,"\n", append = TRUE )
          }
          
          
        }    
      }    
    }    
  }

#this function produce convergence plots (all methods in one figure)
convergence_plots_one = function(p_list=c(10),n_list=c(10),graph_list=c("random"),rep_list=1:10,
                           AUC_ROC=TRUE,AUC_PR=FALSE,AUC_ROC_calibrated=FALSE,
                           CE=FALSE,CE_weighed=FALSE,
                           MSE=FALSE,MSE_weighed=FALSE,
                           TP=FALSE,FP=FALSE,TN=FALSE,FN=FALSE,
                           plinks_max_diff=FALSE,
                           x_time = FALSE,
                           x_log = FALSE,
                           xlim=c(0,100000),ylim=c(0,1),legend=FALSE,xlim_log=NULL)
  {
  
  length = length(n_list)*length(p_list)*length(graph_list)*length(rep_list)
  
  
  #determine length of the iter_vec_thin vector for each method
  filename = paste0("metrics_p",p_list[1],"_n",n_list[1],"_",graph_list[1],"_rep",rep_list[1],".Rdata")
  load(file = filename)
  
  iter_vec_thin_mpl_bd = output_list$obj_mpl_bd$iter_vec_thin
  thin_len_mpl_bd = length(iter_vec_thin_mpl_bd)
    
  iter_vec_thin_mpl_rj = output_list$obj_mpl_rj$iter_vec_thin
  thin_len_mpl_rj = length(iter_vec_thin_mpl_rj)
  
  iter_vec_thin_bd = output_list$obj_bd$iter_vec_thin
  thin_len_bd = length(iter_vec_thin_bd)
  
  iter_vec_thin_rj = output_list$obj_rj$iter_vec_thin
  thin_len_rj = length(iter_vec_thin_rj)
  
  iter_vec_thin_ss = output_list$obj_ss$iter_vec_thin
  thin_len_ss = length(iter_vec_thin_ss)
  
  #initiate time per iteration
  time_per_iter_mpl_bd = 0
  time_per_iter_mpl_rj = 0
  time_per_iter_bd = 0
  time_per_iter_rj = 0
  time_per_iter_ss = 0
  
  #initiate vectors
  if (AUC_ROC){
    AUC_ROC_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    AUC_ROC_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    AUC_ROC_vec_bd = rep(0,thin_len_bd)  
    AUC_ROC_vec_rj = rep(0,thin_len_rj)  
    AUC_ROC_vec_ss = rep(0,thin_len_ss)  
  }
  if (AUC_PR){
    AUC_PR_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    AUC_PR_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    AUC_PR_vec_bd = rep(0,thin_len_bd)  
    AUC_PR_vec_rj = rep(0,thin_len_rj)  
    AUC_PR_vec_ss = rep(0,thin_len_ss)  
  }
  if (AUC_ROC_calibrated){
    AUC_ROC_calibrated_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    AUC_ROC_calibrated_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    AUC_ROC_calibrated_vec_bd = rep(0,thin_len_bd)  
    AUC_ROC_calibrated_vec_rj = rep(0,thin_len_rj)  
    AUC_ROC_calibrated_vec_ss = rep(0,thin_len_ss)  
  }
  if (CE){
    CE_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    CE_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    CE_vec_bd = rep(0,thin_len_bd)  
    CE_vec_rj = rep(0,thin_len_rj)  
    CE_vec_ss = rep(0,thin_len_ss)  
  }
  if (CE_weighed){
    CE_weighed_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    CE_weighed_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    CE_weighed_vec_bd = rep(0,thin_len_bd)  
    CE_weighed_vec_rj = rep(0,thin_len_rj)  
    CE_weighed_vec_ss = rep(0,thin_len_ss)  
  }
  if (MSE){
    MSE_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    MSE_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    MSE_vec_bd = rep(0,thin_len_bd)  
    MSE_vec_rj = rep(0,thin_len_rj)  
    MSE_vec_ss = rep(0,thin_len_ss)  
  }
  if (MSE_weighed){
    MSE_weighed_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    MSE_weighed_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    MSE_weighed_vec_bd = rep(0,thin_len_bd)  
    MSE_weighed_vec_rj = rep(0,thin_len_rj)  
    MSE_weighed_vec_ss = rep(0,thin_len_ss)  
  }
  if (TP){
    TP_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    TP_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    TP_vec_bd = rep(0,thin_len_bd)  
    TP_vec_rj = rep(0,thin_len_rj)  
    TP_vec_ss = rep(0,thin_len_ss)  
  }
  if (FP){
    FP_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    FP_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    FP_vec_bd = rep(0,thin_len_bd)  
    FP_vec_rj = rep(0,thin_len_rj)  
    FP_vec_ss = rep(0,thin_len_ss)  
  }
  if (TN){
    TN_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    TN_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    TN_vec_bd = rep(0,thin_len_bd)  
    TN_vec_rj = rep(0,thin_len_rj)  
    TN_vec_ss = rep(0,thin_len_ss)  
  }
  if (FN){
    FN_vec_mpl_bd = rep(0,thin_len_mpl_bd)  
    FN_vec_mpl_rj = rep(0,thin_len_mpl_rj)  
    FN_vec_bd = rep(0,thin_len_bd)  
    FN_vec_rj = rep(0,thin_len_rj)  
    FN_vec_ss = rep(0,thin_len_ss)  
  }
  if (plinks_max_diff){
    plinks_max_diff_mpl_bd = rep(0,thin_len_mpl_bd)  
    plinks_max_diff_mpl_rj = rep(0,thin_len_mpl_rj)  
    plinks_max_diff_bd = rep(0,thin_len_bd)  
    plinks_max_diff_rj = rep(0,thin_len_rj)  
    plinks_max_diff_ss = rep(0,thin_len_ss)  
  }
  
  #fill vectors
  for (p in p_list)
  {
    for( n in n_list )
    {
      for( graph in graph_list )
      {
        for (rep in rep_list){
          
          #load data
          filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
          load(file = filename)
          
          #calculate number of ones and zeroes
          response = output_list$obj_true$response
          ones = sum(response)
          zeroes = length(response)-ones
          
          #calculate time per iteration for all the methods
          time_per_iter_mpl_bd = time_per_iter_mpl_bd + (output_list$obj_mpl_bd$time)/(output_list$obj_mpl_bd$iter)
          time_per_iter_mpl_rj = time_per_iter_mpl_rj + (output_list$obj_mpl_rj$time)/(output_list$obj_mpl_rj$iter)
          time_per_iter_bd = time_per_iter_bd + (output_list$obj_bd$time)/(output_list$obj_bd$iter)
          time_per_iter_rj = time_per_iter_rj + (output_list$obj_rj$time)/(output_list$obj_rj$iter)
          time_per_iter_ss = time_per_iter_ss + (output_list$obj_ss$time)/(output_list$obj_ss$iter)
          
          # vector = vector + output_list$vector
          if (AUC_ROC){
            AUC_ROC_vec_mpl_bd = AUC_ROC_vec_mpl_bd + output_list$obj_mpl_bd$AUC_ROC_vec
            AUC_ROC_vec_mpl_rj = AUC_ROC_vec_mpl_rj + output_list$obj_mpl_rj$AUC_ROC_vec
            AUC_ROC_vec_bd = AUC_ROC_vec_bd + output_list$obj_bd$AUC_ROC_vec
            AUC_ROC_vec_rj = AUC_ROC_vec_rj + output_list$obj_rj$AUC_ROC_vec
            AUC_ROC_vec_ss = AUC_ROC_vec_ss + output_list$obj_ss$AUC_ROC_vec
          }
          
          # vector = vector + output_list$vector
          if (AUC_PR){
            AUC_PR_vec_mpl_bd = AUC_PR_vec_mpl_bd + output_list$obj_mpl_bd$AUC_PR_vec
            AUC_PR_vec_mpl_rj = AUC_PR_vec_mpl_rj + output_list$obj_mpl_rj$AUC_PR_vec
            AUC_PR_vec_bd = AUC_PR_vec_bd + output_list$obj_bd$AUC_PR_vec
            AUC_PR_vec_rj = AUC_PR_vec_rj + output_list$obj_rj$AUC_PR_vec
            AUC_PR_vec_ss = AUC_PR_vec_ss + output_list$obj_ss$AUC_PR_vec
          }
          
          # vector = vector + output_list$vector
          if (AUC_ROC_calibrated){
            AUC_ROC_calibrated_vec_mpl_bd = AUC_ROC_calibrated_vec_mpl_bd + output_list$obj_mpl_bd$AUC_ROC_calibrated_vec
            AUC_ROC_calibrated_vec_mpl_rj = AUC_ROC_calibrated_vec_mpl_rj + output_list$obj_mpl_rj$AUC_ROC_calibrated_vec
            AUC_ROC_calibrated_vec_bd = AUC_ROC_calibrated_vec_bd + output_list$obj_bd$AUC_ROC_calibrated_vec
            AUC_ROC_calibrated_vec_rj = AUC_ROC_calibrated_vec_rj + output_list$obj_rj$AUC_ROC_calibrated_vec
            AUC_ROC_calibrated_vec_ss = AUC_ROC_calibrated_vec_ss + output_list$obj_ss$AUC_ROC_calibrated_vec
          }
          
          # vector = vector + output_list$vector
          if (CE){
            CE_vec_mpl_bd = CE_vec_mpl_bd + output_list$obj_mpl_bd$CE_vec
            CE_vec_mpl_rj = CE_vec_mpl_rj + output_list$obj_mpl_rj$CE_vec
            CE_vec_bd = CE_vec_bd + output_list$obj_bd$CE_vec
            CE_vec_rj = CE_vec_rj + output_list$obj_rj$CE_vec
            CE_vec_ss = CE_vec_ss + output_list$obj_ss$CE_vec
          }
          
          # vector = vector + output_list$vector
          if (CE_weighed){
            CE_weighed_vec_mpl_bd = CE_weighed_vec_mpl_bd + output_list$obj_mpl_bd$CE_weighed_vec
            CE_weighed_vec_mpl_rj = CE_weighed_vec_mpl_rj + output_list$obj_mpl_rj$CE_weighed_vec
            CE_weighed_vec_bd = CE_weighed_vec_bd + output_list$obj_bd$CE_weighed_vec
            CE_weighed_vec_rj = CE_weighed_vec_rj + output_list$obj_rj$CE_weighed_vec
            CE_weighed_vec_ss = CE_weighed_vec_ss + output_list$obj_ss$CE_weighed_vec
          }
          
          # vector = vector + output_list$vector
          if (MSE){
            MSE_vec_mpl_bd = MSE_vec_mpl_bd + output_list$obj_mpl_bd$MSE_vec
            MSE_vec_mpl_rj = MSE_vec_mpl_rj + output_list$obj_mpl_rj$MSE_vec
            MSE_vec_bd = MSE_vec_bd + output_list$obj_bd$MSE_vec
            MSE_vec_rj = MSE_vec_rj + output_list$obj_rj$MSE_vec
            MSE_vec_ss = MSE_vec_ss + output_list$obj_ss$MSE_vec
          }
          
          # vector = vector + output_list$vector
          if (MSE_weighed){
            MSE_weighed_vec_mpl_bd = MSE_weighed_vec_mpl_bd + output_list$obj_mpl_bd$MSE_weighed_vec
            MSE_weighed_vec_mpl_rj = MSE_weighed_vec_mpl_rj + output_list$obj_mpl_rj$MSE_weighed_vec
            MSE_weighed_vec_bd = MSE_weighed_vec_bd + output_list$obj_bd$MSE_weighed_vec
            MSE_weighed_vec_rj = MSE_weighed_vec_rj + output_list$obj_rj$MSE_weighed_vec
            MSE_weighed_vec_ss = MSE_weighed_vec_ss + output_list$obj_ss$MSE_weighed_vec
          }
          
          # vector = vector + output_list$vector
          if (TP){
            TP_vec_mpl_bd = TP_vec_mpl_bd + output_list$obj_mpl_bd$TP_vec*(1/ones)
            TP_vec_mpl_rj = TP_vec_mpl_rj + output_list$obj_mpl_rj$TP_vec*(1/ones)
            TP_vec_bd = TP_vec_bd + output_list$obj_bd$TP_vec*(1/ones)
            TP_vec_rj = TP_vec_rj + output_list$obj_rj$TP_vec*(1/ones)
            TP_vec_ss = TP_vec_ss + output_list$obj_ss$TP_vec*(1/ones)
          }
          
          # vector = vector + output_list$vector
          if (FP){
            FP_vec_mpl_bd = FP_vec_mpl_bd + output_list$obj_mpl_bd$FP_vec*(1/zeroes)
            FP_vec_mpl_rj = FP_vec_mpl_rj + output_list$obj_mpl_rj$FP_vec*(1/zeroes)
            FP_vec_bd = FP_vec_bd + output_list$obj_bd$FP_vec*(1/zeroes)
            FP_vec_rj = FP_vec_rj + output_list$obj_rj$FP_vec*(1/zeroes)
            FP_vec_ss = FP_vec_ss + output_list$obj_ss$FP_vec*(1/zeroes)
          }
          
          # vector = vector + output_list$vector
          if (TN){
            TN_vec_mpl_bd = TN_vec_mpl_bd + output_list$obj_mpl_bd$TN_vec*(1/zeroes)
            TN_vec_mpl_rj = TN_vec_mpl_rj + output_list$obj_mpl_rj$TN_vec*(1/zeroes)
            TN_vec_bd = TN_vec_bd + output_list$obj_bd$TN_vec*(1/zeroes)
            TN_vec_rj = TN_vec_rj + output_list$obj_rj$TN_vec*(1/zeroes)
            TN_vec_ss = TN_vec_ss + output_list$obj_ss$TN_vec*(1/zeroes)
          }
          
          # vector = vector + output_list$vector
          if (FN){
            FN_vec_mpl_bd = FN_vec_mpl_bd + output_list$obj_mpl_bd$FN_vec*(1/ones)
            FN_vec_mpl_rj = FN_vec_mpl_rj + output_list$obj_mpl_rj$FN_vec*(1/ones)
            FN_vec_bd = FN_vec_bd + output_list$obj_bd$FN_vec*(1/ones)
            FN_vec_rj = FN_vec_rj + output_list$obj_rj$FN_vec*(1/ones)
            FN_vec_ss = FN_vec_ss + output_list$obj_ss$FN_vec*(1/ones)
          }
          
          #p-links convergence
          if (plinks_max_diff){
            plinks_max_diff_mpl_bd = plinks_max_diff_mpl_bd + output_list$obj_mpl_bd$plinks_max_diff
            plinks_max_diff_mpl_rj = plinks_max_diff_mpl_rj + output_list$obj_mpl_rj$plinks_max_diff
            plinks_max_diff_bd = plinks_max_diff_bd + output_list$obj_bd$plinks_max_diff
            plinks_max_diff_rj = plinks_max_diff_rj + output_list$obj_rj$plinks_max_diff
            plinks_max_diff_ss = plinks_max_diff_ss + output_list$obj_ss$plinks_max_diff
          }
          
          
        }
      }
    }
  }
  
  #calculate average time per iteration
  time_per_iter_mpl_bd = time_per_iter_mpl_bd/length
  time_per_iter_mpl_rj = time_per_iter_mpl_rj/length
  time_per_iter_bd = time_per_iter_bd/length
  time_per_iter_rj = time_per_iter_rj/length
  time_per_iter_ss = time_per_iter_ss/length
  
  #calculate average of vectors
  if (AUC_ROC){
    AUC_ROC_vec_mpl_bd = AUC_ROC_vec_mpl_bd/length
    AUC_ROC_vec_mpl_rj = AUC_ROC_vec_mpl_rj/length
    AUC_ROC_vec_bd = AUC_ROC_vec_bd/length
    AUC_ROC_vec_rj = AUC_ROC_vec_rj/length
    AUC_ROC_vec_ss = AUC_ROC_vec_ss/length
  }
  if (AUC_PR){
    AUC_PR_vec_mpl_bd = AUC_PR_vec_mpl_bd/length
    AUC_PR_vec_mpl_rj = AUC_PR_vec_mpl_rj/length
    AUC_PR_vec_bd = AUC_PR_vec_bd/length
    AUC_PR_vec_rj = AUC_PR_vec_rj/length
    AUC_PR_vec_ss = AUC_PR_vec_ss/length
  }
  if (AUC_ROC_calibrated){
    AUC_ROC_calibrated_vec_mpl_bd = AUC_ROC_calibrated_vec_mpl_bd/length
    AUC_ROC_calibrated_vec_mpl_rj = AUC_ROC_calibrated_vec_mpl_rj/length
    AUC_ROC_calibrated_vec_bd = AUC_ROC_calibrated_vec_bd/length
    AUC_ROC_calibrated_vec_rj = AUC_ROC_calibrated_vec_rj/length
    AUC_ROC_calibrated_vec_ss = AUC_ROC_calibrated_vec_ss/length
  }
  if (CE){
    CE_vec_mpl_bd = CE_vec_mpl_bd/length
    CE_vec_mpl_rj = CE_vec_mpl_rj/length
    CE_vec_bd = CE_vec_bd/length
    CE_vec_rj = CE_vec_rj/length
    CE_vec_ss = CE_vec_ss/length
  }
  if (CE_weighed){
    CE_weighed_vec_mpl_bd = CE_weighed_vec_mpl_bd/length
    CE_weighed_vec_mpl_rj = CE_weighed_vec_mpl_rj/length
    CE_weighed_vec_bd = CE_weighed_vec_bd/length
    CE_weighed_vec_rj = CE_weighed_vec_rj/length
    CE_weighed_vec_ss = CE_weighed_vec_ss/length
  }
  if (MSE){
    MSE_vec_mpl_bd = MSE_vec_mpl_bd/length
    MSE_vec_mpl_rj = MSE_vec_mpl_rj/length
    MSE_vec_bd = MSE_vec_bd/length
    MSE_vec_rj = MSE_vec_rj/length
    MSE_vec_ss = MSE_vec_ss/length
  }
  if (MSE_weighed){
    MSE_weighed_vec_mpl_bd = MSE_weighed_vec_mpl_bd/length
    MSE_weighed_vec_mpl_rj = MSE_weighed_vec_mpl_rj/length
    MSE_weighed_vec_bd = MSE_weighed_vec_bd/length
    MSE_weighed_vec_rj = MSE_weighed_vec_rj/length
    MSE_weighed_vec_ss = MSE_weighed_vec_ss/length
  }
  if (TP){
    TP_vec_mpl_bd = TP_vec_mpl_bd/length
    TP_vec_mpl_rj = TP_vec_mpl_rj/length
    TP_vec_bd = TP_vec_bd/length
    TP_vec_rj = TP_vec_rj/length
    TP_vec_ss = TP_vec_ss/length
  }
  if (FP){
    FP_vec_mpl_bd = FP_vec_mpl_bd/length
    FP_vec_mpl_rj = FP_vec_mpl_rj/length
    FP_vec_bd = FP_vec_bd/length
    FP_vec_rj = FP_vec_rj/length
    FP_vec_ss = FP_vec_ss/length
  }
  if (TN){
    TN_vec_mpl_bd = TN_vec_mpl_bd/length
    TN_vec_mpl_rj = TN_vec_mpl_rj/length
    TN_vec_bd = TN_vec_bd/length
    TN_vec_rj = TN_vec_rj/length
    TN_vec_ss = TN_vec_ss/length
  }
  if (FN){
    FN_vec_mpl_bd = FN_vec_mpl_bd/length
    FN_vec_mpl_rj = FN_vec_mpl_rj/length
    FN_vec_bd = FN_vec_bd/length
    FN_vec_rj = FN_vec_rj/length
    FN_vec_ss = FN_vec_ss/length
  }
  if (plinks_max_diff){
    plinks_max_diff_mpl_bd = plinks_max_diff_mpl_bd/length
    plinks_max_diff_mpl_rj = plinks_max_diff_mpl_rj/length
    plinks_max_diff_bd = plinks_max_diff_bd/length
    plinks_max_diff_rj = plinks_max_diff_rj/length
    plinks_max_diff_ss = plinks_max_diff_ss/length
  }
  
  ##set parameters for plotting
  #title
  pstring = paste(p_list,collapse=" ")
  nstring = paste(n_list,collapse=" ")
  graphstring = paste(graph_list,collapse=" ")
  repstring = paste(rep_list,collapse=" ")
  
  #x-axis label
  xlab = "Iterations"
  if (x_time){
    iter_vec_thin_mpl_bd = iter_vec_thin_mpl_bd*time_per_iter_mpl_bd
    iter_vec_thin_mpl_rj = iter_vec_thin_mpl_rj*time_per_iter_mpl_rj
    iter_vec_thin_bd = iter_vec_thin_bd*time_per_iter_bd
    iter_vec_thin_rj = iter_vec_thin_rj*time_per_iter_rj
    iter_vec_thin_ss = iter_vec_thin_ss*time_per_iter_ss
    xlab = "time (seconds)"
  }
  
  #set x-axis to log scale (if log =TRUE)
  logvalue =''
  if (x_log){
    logvalue = 'x'
    xlim = xlim_log
  }
  
  #make plots
  if (AUC_ROC){
    title = paste0("AUC_ROC, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="AUC_ROC",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=AUC_ROC_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=AUC_ROC_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=AUC_ROC_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=AUC_ROC_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=AUC_ROC_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (AUC_PR){
    title = paste0("AUC_PR, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="AUC_PR",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=AUC_PR_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=AUC_PR_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=AUC_PR_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=AUC_PR_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=AUC_PR_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (AUC_ROC_calibrated){
    title = paste0("AUC_ROC_calibrated, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="AUC_ROC_calibrated",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=AUC_ROC_calibrated_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=AUC_ROC_calibrated_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=AUC_ROC_calibrated_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=AUC_ROC_calibrated_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=AUC_ROC_calibrated_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (CE){
    title = paste0("CE, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="CE",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=CE_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=CE_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=CE_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=CE_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=CE_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (CE_weighed){
    title = paste0("CE_weighed, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="CE_weighed",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=CE_weighed_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=CE_weighed_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=CE_weighed_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=CE_weighed_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=CE_weighed_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (MSE){
    title = paste0("MSE, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="MSE",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=MSE_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=MSE_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=MSE_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=MSE_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=MSE_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (MSE_weighed){
    title = paste0("MSE_weighed, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="MSE_weighed",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=MSE_weighed_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=MSE_weighed_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=MSE_weighed_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=MSE_weighed_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=MSE_weighed_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (TP){
    title = paste0("TP, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="TP",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=TP_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=TP_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=TP_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=TP_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=TP_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (FP){
    title = paste0("FP, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="FP",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=FP_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=FP_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=FP_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=FP_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=FP_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (TN){
    title = paste0("TN, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="TN",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=TN_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=TN_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=TN_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=TN_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=TN_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (FN){
    title = paste0("FN, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="FN",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=FN_vec_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=FN_vec_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=FN_vec_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=FN_vec_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=FN_vec_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  if (plinks_max_diff){
    title = paste0("plinks_max_diff, p =",pstring,", n=",nstring," ",graphstring," ",repstring)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="plinks_max_diff",log=logvalue,main=title)
    points(x=iter_vec_thin_mpl_bd,y=plinks_max_diff_mpl_bd,type="l",col=1,lty=1,lw=1.5)  
    points(x=iter_vec_thin_mpl_rj,y=plinks_max_diff_mpl_rj,type="l",col=2,lty=2,lw=1.5)
    points(x=iter_vec_thin_bd,y=plinks_max_diff_bd,type="l",col=3,lty=3,lw=1.5)
    points(x=iter_vec_thin_rj,y=plinks_max_diff_rj,type="l",col=4,lty=4,lw=1.5)
    points(x=iter_vec_thin_ss,y=plinks_max_diff_ss,type="l",col=5,lty=5,lw=1.5)
    if (legend){
      legend( "bottomright", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), lty = 1:5, col = 1:5, lw = 1, cex = 1, box.lty = 0 )
    }
  }
  
  
  
  
  
}

#this function produce convergence plots (several replications in one figure, one figure per method)
convergence_plots_two = function(p=10,n=100,graph="random",method="mpl_rj",rep_list=1:10,
                             AUC_ROC=TRUE,AUC_PR=FALSE,AUC_ROC_calibrated=FALSE,
                             CE=FALSE,CE_weighed=FALSE,
                             MSE=FALSE,MSE_weighed=FALSE,
                             TP=FALSE,FP=FALSE,TN=FALSE,FN=FALSE,
                             plinks_max_diff=FALSE,
                             x_time = FALSE,
                             x_log = FALSE,
                             xlim=c(0,100000),ylim=c(0,1),legend=FALSE,xlim_log=NULL)
  
{
  ###make plot###
  
  #set x-axis to log scale (if log =TRUE)
  logvalue =''
  if (x_log){
    logvalue = 'x'
    xlim = xlim_log
  }
  #only one value can be set to TRUE
  if (AUC_ROC+AUC_PR+AUC_ROC_calibrated+CE+CE_weighed+MSE+MSE_weighed+TP+FP+TN+FN+plinks_max_diff !=1){
    stop("only one metric can be set to TRUE")
  }
  
  #create title and plot
  xlab = "Iterations"
  if (AUC_ROC){
    title = paste0("AUC_ROC, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="AUC_ROC",log=logvalue,main=title)
  }
  if (AUC_PR){
    title = paste0("AUC_PR, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="AUC_PR",log=logvalue,main=title)
  }
  if (AUC_ROC_calibrated){
    title = paste0("AUC_ROC_calibrated, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="AUC_ROC_calibrated",log=logvalue,main=title)
  }
  if (CE){
    title = paste0("CE, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="CE",log=logvalue,main=title)
  }
  if (CE_weighed){
    title = paste0("CE_weighed, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="CE_weighed",log=logvalue,main=title)
  }
  if (MSE){
    title = paste0("MSE, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="MSE",log=logvalue,main=title)
  }
  if (MSE_weighed){
    title = paste0("MSE_weighed, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="MSE_weighed",log=logvalue,main=title)
  }
  if (TP){
    title = paste0("TP, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="TP",log=logvalue,main=title)
  }
  if (FP){
    title = paste0("FP, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="FP",log=logvalue,main=title)
  }
  if (TN){
    title = paste0("TN, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="TN",log=logvalue,main=title)
  }
  if (FN){
    title = paste0("FN, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="FN",log=logvalue,main=title)
  }
  if (plinks_max_diff){
    title = paste0("plinks_max_diff, p =",p,", n=",n," ",graph," ",method)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="plinks_max_diff",log=logvalue,main=title)
  }
  
  col = 0
  
  for (rep in rep_list){
    filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
    load(file = filename)
    
    #calculate number of ones and zeroes
    response = output_list$obj_true$response
    ones = sum(response)
    zeroes = length(response)-ones
    
    if (method=="mpl_bd"){
      iter_vec_thin = output_list$obj_mpl_bd$iter_vec_thin
      if (AUC_ROC){y = output_list$obj_mpl_bd$AUC_ROC_vec}
      if (AUC_PR){y = output_list$obj_mpl_bd$AUC_PR_vec}
      if (AUC_ROC_calibrated){y = output_list$obj_mpl_bd$AUC_ROC_calibrated_vec}
      if (CE){y = output_list$obj_mpl_bd$CE_vec}
      if (CE_weighed){y = output_list$obj_mpl_bd$CE_weighed_vec}
      if (MSE){y = output_list$obj_mpl_bd$MSE_vec}
      if (MSE_weighed){y = output_list$obj_mpl_bd$MSE_weighed_vec}
      if (TP){y = output_list$obj_mpl_bd$TP_vec*(1/ones)}
      if (FP){y = output_list$obj_mpl_bd$FP_vec*(1/zeroes)}
      if (TN){y = output_list$obj_mpl_bd$TN_vec*(1/zeroes)}
      if (FN){y = output_list$obj_mpl_bd$FN_vec*(1/ones)}
      if (plinks_max_diff){y = output_list$obj_mpl_bd$plinks_max_diff}
    }
    
    if (method=="mpl_rj"){
      iter_vec_thin = output_list$obj_mpl_rj$iter_vec_thin
      if (AUC_ROC){y = output_list$obj_mpl_rj$AUC_ROC_vec}
      if (AUC_PR){y = output_list$obj_mpl_rj$AUC_PR_vec}
      if (AUC_ROC_calibrated){y = output_list$obj_mpl_rj$AUC_ROC_calibrated_vec}
      if (CE){y = output_list$obj_mpl_rj$CE_vec}
      if (CE_weighed){y = output_list$obj_mpl_rj$CE_weighed_vec}
      if (MSE){y = output_list$obj_mpl_rj$MSE_vec}
      if (MSE_weighed){y = output_list$obj_mpl_rj$MSE_weighed_vec}
      if (TP){y = output_list$obj_mpl_rj$TP_vec*(1/ones)}
      if (FP){y = output_list$obj_mpl_rj$FP_vec*(1/zeroes)}
      if (TN){y = output_list$obj_mpl_rj$TN_vec*(1/zeroes)}
      if (FN){y = output_list$obj_mpl_rj$FN_vec*(1/ones)}
      if (plinks_max_diff){y = output_list$obj_mpl_rj$plinks_max_diff}
    }
    
    if (method=="bd"){
      iter_vec_thin = output_list$obj_bd$iter_vec_thin
      if (AUC_ROC){y = output_list$obj_bd$AUC_ROC_vec}
      if (AUC_PR){y = output_list$obj_bd$AUC_PR_vec}
      if (AUC_ROC_calibrated){y = output_list$obj_bd$AUC_ROC_calibrated_vec}
      if (CE){y = output_list$obj_bd$CE_vec}
      if (CE_weighed){y = output_list$obj_bd$CE_weighed_vec}
      if (MSE){y = output_list$obj_bd$MSE_vec}
      if (MSE_weighed){y = output_list$obj_bd$MSE_weighed_vec}
      if (TP){y = output_list$obj_bd$TP_vec*(1/ones)}
      if (FP){y = output_list$obj_bd$FP_vec*(1/zeroes)}
      if (TN){y = output_list$obj_bd$TN_vec*(1/zeroes)}
      if (FN){y = output_list$obj_bd$FN_vec*(1/ones)}
      if (plinks_max_diff){y = output_list$obj_bd$plinks_max_diff}
    }
    
    if (method=="rj"){
      iter_vec_thin = output_list$obj_rj$iter_vec_thin
      if (AUC_ROC){y = output_list$obj_rj$AUC_ROC_vec}
      if (AUC_PR){y = output_list$obj_rj$AUC_PR_vec}
      if (AUC_ROC_calibrated){y = output_list$obj_rj$AUC_ROC_calibrated_vec}
      if (CE){y = output_list$obj_rj$CE_vec}
      if (CE_weighed){y = output_list$obj_rj$CE_weighed_vec}
      if (MSE){y = output_list$obj_rj$MSE_vec}
      if (MSE_weighed){y = output_list$obj_rj$MSE_weighed_vec}
      if (TP){y = output_list$obj_rj$TP_vec*(1/ones)}
      if (FP){y = output_list$obj_rj$FP_vec*(1/zeroes)}
      if (TN){y = output_list$obj_rj$TN_vec*(1/zeroes)}
      if (FN){y = output_list$obj_rj$FN_vec*(1/ones)}
      if (plinks_max_diff){y = output_list$obj_rj$plinks_max_diff}
    }
    
    if (method=="ss"){
      iter_vec_thin = output_list$obj_ss$iter_vec_thin
      if (AUC_ROC){y = output_list$obj_ss$AUC_ROC_vec}
      if (AUC_PR){y = output_list$obj_ss$AUC_PR_vec}
      if (AUC_ROC_calibrated){y = output_list$obj_ss$AUC_ROC_calibrated_vec}
      if (CE){y = output_list$obj_ss$CE_vec}
      if (CE_weighed){y = output_list$obj_ss$CE_weighed_vec}
      if (MSE){y = output_list$obj_ss$MSE_vec}
      if (MSE_weighed){y = output_list$obj_ss$MSE_weighed_vec}
      if (TP){y = output_list$obj_ss$TP_vec*(1/ones)}
      if (FP){y = output_list$obj_ss$FP_vec*(1/zeroes)}
      if (TN){y = output_list$obj_ss$TN_vec*(1/zeroes)}
      if (FN){y = output_list$obj_ss$FN_vec*(1/ones)}
      if (plinks_max_diff){y = output_list$obj_ss$plinks_max_diff}
    }
    
    col = col + 1
    points(x=iter_vec_thin,y=y,type="l",col=col,lty=1,lw=1.5)  
    
    
  }
}

#this function makes scatter plots
metric_scatter = function(p_list=c(10),n_list=c(10),graph_list=c("random"),xlim=c(0,1),ylim=c(0,1),legend=TRUE,rep_list=1:10){
  
  pstring = paste(p_list,collapse=" ")
  nstring = paste(n_list,collapse=" ")
  graphstring = paste(graph_list,collapse=" ")
  repstring = paste(rep_list,collapse=" ")
  
  title = paste0("p =",pstring,", n=",nstring,", graph=",graphstring,"rep=",repstring)
  plot(NA,xlim=xlim,ylim=ylim,xlab="MSE (weighed)",ylab="",main=title,cex.lab=1.5,font.lab=2)
  title(ylab="AUC",line=1.9,cex.lab=1.5,font.lab=2)
  
  AUC_mpl_bd =  c()
  MSE_weighed_mpl_bd =  c()
  AUC_mpl_rj =  c()
  MSE_weighed_mpl_rj =  c()
  AUC_bd =  c()
  MSE_weighed_bd =  c()
  AUC_rj =  c()
  MSE_weighed_rj =  c()
  AUC_ss =  c()
  MSE_weighed_ss =  c()
  
  len = length(p_list)*length(n_list)*length(graph_list)*length(rep_list)
  
  
  for (p in p_list)
  {
    for( n in n_list )
    {
      for( graph in graph_list )
      {
        for (rep in rep_list){
            filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
            load(file = filename)
            
            MSE_weighed_mpl_bd = c(MSE_weighed_mpl_bd,output_list$obj_mpl_bd$MSE_weighed)
            AUC_mpl_bd = c(AUC_mpl_bd,output_list$obj_mpl_bd$AUC_ROC)
            
            MSE_weighed_mpl_rj = c(MSE_weighed_mpl_rj,output_list$obj_mpl_rj$MSE_weighed)
            AUC_mpl_rj = c(AUC_mpl_rj,output_list$obj_mpl_rj$AUC_ROC)
            
            MSE_weighed_bd = c(MSE_weighed_bd,output_list$obj_bd$MSE_weighed)
            AUC_bd = c(AUC_bd,output_list$obj_bd$AUC_ROC)
            
            MSE_weighed_rj = c(MSE_weighed_rj,output_list$obj_rj$MSE_weighed)
            AUC_rj = c(AUC_rj,output_list$obj_rj$AUC_ROC)
            
            MSE_weighed_ss = c(MSE_weighed_ss,output_list$obj_ss$MSE_weighed)
            AUC_ss = c(AUC_ss,output_list$obj_ss$AUC_ROC)
            
            
        }
      }
    }
  }
  
  points(x=MSE_weighed_mpl_bd,y=AUC_mpl_bd,type="p",col=1,lty=1,pch=1,lwd=2)
  points(x=MSE_weighed_mpl_rj,y=AUC_mpl_rj,type="p",col=2,lty=1,pch=1,lwd=2)
  points(x=MSE_weighed_bd,y=AUC_bd,type="p",col=3,lty=1,pch=1,lwd=2)
  points(x=MSE_weighed_rj,y=AUC_rj,type="p",col=4,lty=1,pch=1,lwd=2)
  points(x=MSE_weighed_ss,y=AUC_ss,type="p",col=5,lty=1,pch=1,lwd=2)
  
  points(x=mean(MSE_weighed_mpl_bd),y=mean(AUC_mpl_bd),type="p",col=1,cex=2,lwd=2,pch=15)
  points(x=mean(MSE_weighed_mpl_rj),y=mean(AUC_mpl_rj),type="p",col=2,cex=2,lwd=2,pch=15)
  points(x=mean(MSE_weighed_bd),y=mean(AUC_bd),type="p",col=3,cex=2,lwd=2,pch=15)
  points(x=mean(MSE_weighed_rj),y=mean(AUC_rj),type="p",col=4,cex=2,lwd=2,pch=15)
  points(x=mean(MSE_weighed_ss),y=mean(AUC_ss),type="p",col=5,cex=2,lwd=2,pch=15)
  
  if (legend){
    legend( "bottomleft", inset=.03, c( "MPL-BD","MPL-RJ", "BD", "RJ", "SS" ), col = 1:5, pch=1, box.lty = 0,text.font=2) 
  }
}

#this function determines the amount of iterations until AUC or MSE convergence
convergence = function(p_list=c(10),n_list=c(10),graph_list=c("random"),rep_list=1:10,epsilon=0.005,AUC_ROC=TRUE,MSE_weighed=FALSE){
  
  for (p in p_list)
  {
    for( n in n_list )
    {
      for( graph in graph_list )
      {
        AUC_ROC_conv_iteration = rep(0,length(rep_list))
        i = 0
        for (rep in rep_list){
          i = i+1
          filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
          load(file = filename)
          
          AUC_ROC = output_list$obj_mpl_bd$AUC_ROC
          AUC_ROC_vec = output_list$obj_mpl_bd$AUC_ROC_vec
          diff_AUC_ROC_vec = AUC_ROC - AUC_ROC_vec
          conv_index = min(which(diff_AUC_ROC_vec < epsilon))
          iter_vec_thin = output_list$obj_mpl_bd$iter_vec_thin
          conv_iteration = iter_vec_thin[conv_index]
          AUC_ROC_conv_iteration[i] = conv_iteration
        }
      }
    }
  }
  return(AUC_ROC_conv_iteration)
          
}

convergence_tables = function(p_list=c(10),n_list=c(10),graph_list=c("random"),rep_list=1:10,
                              epsilon=0.01,time_unit=FALSE,
                              AUC_ROC=TRUE,MSE_weighed=FALSE,
                              AUC_ROC_time_name = "no file name given",
                              MSE_weighed_time_name = "no file name given",
                              AUC_ROC_iter_name = "no file name given",
                              MSE_weighed_iter_name = "no file name given",
                              stat = "mean",
                              round_nr = 2)
{
  if (time_unit==TRUE){
    AUC_ROCname =  AUC_ROC_time_name
    MSE_weighedname = MSE_weighed_time_name
  }
  if (time_unit==FALSE){
    AUC_ROCname = AUC_ROC_iter_name
    MSE_weighedname = MSE_weighed_iter_name
  }
  
  for (p in p_list)
    
    print(paste0("working on p=",p))
  
  {
    for( n in n_list ){
      print(paste0("working on n=",n))
      
      if (AUC_ROC){
        cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = AUC_ROCname,"\n", append = TRUE )
      }
      
      if (MSE_weighed){
        cat("p = ",p,", n = ",n," \\","\\"," \\","\\",sep="",file = MSE_weighedname,"\n", append = TRUE )
      }
      
      for( graph in graph_list ){
        
        print(paste0("working on graph=",graph))
        
        #initiate vectors
        len = length(rep_list)
        AUC_ROC_conv_vec_mpl_bd = rep(0,len)
        AUC_ROC_conv_vec_mpl_rj= rep(0,len)
        AUC_ROC_conv_vec_bd= rep(0,len)
        AUC_ROC_conv_vec_rj= rep(0,len)
        AUC_ROC_conv_vec_ss= rep(0,len)
        MSE_weighed_conv_vec_mpl_bd = rep(0,len)
        MSE_weighed_conv_vec_mpl_rj= rep(0,len)
        MSE_weighed_conv_vec_bd= rep(0,len)
        MSE_weighed_conv_vec_rj= rep(0,len)
        MSE_weighed_conv_vec_ss= rep(0,len)
        
        #load data into the vectors
        i = 0
        for (rep in rep_list){
          #print progress
          print(paste0("working on rep=",rep))
          
          #counter
          i = i + 1
          
          #load file
          filename = paste0("metrics_p",p,"_n",n,"_",graph,"_rep",rep,".Rdata")
          load(file = filename)
          
          #set time per iter values
          time_per_iter_mpl_bd = 1
          time_per_iter_mpl_rj = 1
          time_per_iter_bd = 1
          time_per_iter_rj = 1
          time_per_iter_ss = 1
          if (time_unit){
            time_per_iter_mpl_bd = output_list$obj_mpl_bd$time/output_list$obj_mpl_bd$iter
            time_per_iter_mpl_rj = output_list$obj_mpl_rj$time/output_list$obj_mpl_rj$iter
            time_per_iter_bd = output_list$obj_bd$time/output_list$obj_bd$iter
            time_per_iter_rj = output_list$obj_rj$time/output_list$obj_rj$iter
            time_per_iter_ss = output_list$obj_ss$time/output_list$obj_ss$iter
          }
          
          if (AUC_ROC){
            
            #mpl_bd
            AUC_ROC_final = as.numeric(output_list$obj_mpl_bd$AUC_ROC)
            AUC_ROC_vec = output_list$obj_mpl_bd$AUC_ROC_vec
            diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec
            conv_index = min(which(diff_AUC_ROC_vec < epsilon))
            iter_vec_thin = output_list$obj_mpl_bd$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            AUC_ROC_conv_vec_mpl_bd[i] = conv_iteration*time_per_iter_mpl_bd
            
            #mpl_rj
            AUC_ROC_final = as.numeric(output_list$obj_mpl_rj$AUC_ROC)
            AUC_ROC_vec = output_list$obj_mpl_rj$AUC_ROC_vec
            diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec
            conv_index = min(which(diff_AUC_ROC_vec < epsilon))
            iter_vec_thin = output_list$obj_mpl_rj$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            AUC_ROC_conv_vec_mpl_rj[i] = conv_iteration*time_per_iter_mpl_rj
            
            #bd
            AUC_ROC_final = as.numeric(output_list$obj_bd$AUC_ROC)
            AUC_ROC_vec = output_list$obj_bd$AUC_ROC_vec
            diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec
            conv_index = min(which(diff_AUC_ROC_vec < epsilon))
            iter_vec_thin = output_list$obj_bd$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            AUC_ROC_conv_vec_bd[i] = conv_iteration*time_per_iter_bd
            
            #rj
            AUC_ROC_final = as.numeric(output_list$obj_rj$AUC_ROC)
            AUC_ROC_vec = output_list$obj_rj$AUC_ROC_vec
            diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec
            conv_index = min(which(diff_AUC_ROC_vec < epsilon))
            iter_vec_thin = output_list$obj_rj$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            AUC_ROC_conv_vec_rj[i] = conv_iteration*time_per_iter_rj
            
            #ss
            AUC_ROC_final = as.numeric(output_list$obj_ss$AUC_ROC)
            AUC_ROC_vec = output_list$obj_ss$AUC_ROC_vec
            diff_AUC_ROC_vec = AUC_ROC_final - AUC_ROC_vec
            conv_index = min(which(diff_AUC_ROC_vec < epsilon))
            iter_vec_thin = output_list$obj_ss$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            AUC_ROC_conv_vec_ss[i] = conv_iteration*time_per_iter_ss
          }
          if (MSE_weighed){
            
            #mpl_bd
            MSE_weighed_final = as.numeric(output_list$obj_mpl_bd$MSE_weighed)
            MSE_weighed_vec = output_list$obj_mpl_bd$MSE_weighed_vec
            diff_MSE_weighed_vec = MSE_weighed_vec - MSE_weighed_final
            conv_index = min(which(diff_MSE_weighed_vec < epsilon))
            iter_vec_thin = output_list$obj_mpl_bd$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            MSE_weighed_conv_vec_mpl_bd[i] = conv_iteration*time_per_iter_mpl_bd
            
            #mpl_rj
            MSE_weighed_final = as.numeric(output_list$obj_mpl_rj$MSE_weighed)
            MSE_weighed_vec = output_list$obj_mpl_rj$MSE_weighed_vec
            diff_MSE_weighed_vec =  MSE_weighed_vec - MSE_weighed_final
            conv_index = min(which(diff_MSE_weighed_vec < epsilon))
            iter_vec_thin = output_list$obj_mpl_rj$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            MSE_weighed_conv_vec_mpl_rj[i] = conv_iteration*time_per_iter_mpl_rj
            
            #bd
            MSE_weighed_final = as.numeric(output_list$obj_bd$MSE_weighed)
            MSE_weighed_vec = output_list$obj_bd$MSE_weighed_vec
            diff_MSE_weighed_vec =  MSE_weighed_vec - MSE_weighed_final
            conv_index = min(which(diff_MSE_weighed_vec < epsilon))
            iter_vec_thin = output_list$obj_bd$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            MSE_weighed_conv_vec_bd[i] = conv_iteration*time_per_iter_bd
            
            #rj
            MSE_weighed_final = as.numeric(output_list$obj_rj$MSE_weighed)
            MSE_weighed_vec = output_list$obj_rj$MSE_weighed_vec
            diff_MSE_weighed_vec =  MSE_weighed_vec - MSE_weighed_final
            conv_index = min(which(diff_MSE_weighed_vec < epsilon))
            iter_vec_thin = output_list$obj_rj$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            MSE_weighed_conv_vec_rj[i] = conv_iteration*time_per_iter_rj
            
            #ss
            MSE_weighed_final = as.numeric(output_list$obj_ss$MSE_weighed)
            MSE_weighed_vec = output_list$obj_ss$MSE_weighed_vec
            diff_MSE_weighed_vec =  MSE_weighed_vec - MSE_weighed_final
            conv_index = min(which(diff_MSE_weighed_vec < epsilon))
            iter_vec_thin = output_list$obj_ss$iter_vec_thin
            conv_iteration = iter_vec_thin[conv_index]
            MSE_weighed_conv_vec_ss[i] = conv_iteration*time_per_iter_ss
          }
        }  
        
        #print mean and standard deviation of the vectors in the tables
        if (AUC_ROC){
          filename = AUC_ROCname
          cat(graph," & ",
              round(stat_func(vec=AUC_ROC_conv_vec_mpl_bd,stat=stat),round_nr)," (",round(sd(AUC_ROC_conv_vec_mpl_bd),round_nr),") & ",
              round(stat_func(vec=AUC_ROC_conv_vec_mpl_rj,stat=stat),round_nr)," (",round(sd(AUC_ROC_conv_vec_mpl_rj),round_nr),") & ",
              round(stat_func(vec=AUC_ROC_conv_vec_bd,stat=stat),round_nr)," (",round(sd(AUC_ROC_conv_vec_bd),round_nr),") & ",
              round(stat_func(vec=AUC_ROC_conv_vec_rj,stat=stat),round_nr)," (",round(sd(AUC_ROC_conv_vec_rj),round_nr),") & ",
              round(stat_func(vec=AUC_ROC_conv_vec_ss,stat=stat),round_nr)," (",round(sd(AUC_ROC_conv_vec_ss),round_nr),")"," \\","\\",sep="",
              file = filename,"\n", append = TRUE )
        }
        
        if (MSE_weighed){
          filename = MSE_weighedname
          cat(graph," & ",
              round(stat_func(vec=MSE_weighed_conv_vec_mpl_bd,stat=stat),round_nr)," (",round(sd(MSE_weighed_conv_vec_mpl_bd),round_nr),") & ",
              round(stat_func(vec=MSE_weighed_conv_vec_mpl_rj,stat=stat),round_nr)," (",round(sd(MSE_weighed_conv_vec_mpl_rj),round_nr),") & ",
              round(stat_func(vec=MSE_weighed_conv_vec_bd,stat=stat),round_nr)," (",round(sd(MSE_weighed_conv_vec_bd),round_nr),") & ",
              round(stat_func(vec=MSE_weighed_conv_vec_rj,stat=stat),round_nr)," (",round(sd(MSE_weighed_conv_vec_rj),round_nr),") & ",
              round(stat_func(vec=MSE_weighed_conv_vec_ss,stat=stat),round_nr)," (",round(sd(MSE_weighed_conv_vec_ss),round_nr),")"," \\","\\",sep="",
              file = filename,"\n", append = TRUE )
        }
        
        
        
      }    
    }    
    }    
}







