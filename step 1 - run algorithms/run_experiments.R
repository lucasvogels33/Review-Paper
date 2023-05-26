run_experiments = function( p = 10, n = 100, graph = "random", type = "Gaussian", 
                        vis = FALSE, jump = 1, iter = 10000, burnin = 7000, 
                        save = TRUE, cores = 1, verbose = TRUE,g.start="empty",var1=var1,var2=var2,lambda=lambda,g.prior=0.2,seed=1)
{

##-----simulate data-------------##
seed = 10*seed

set.seed(seed)
data.sim = bdgraph.sim( p = p, n = n, graph = graph, prob = prob, size = size, type = type, vis = vis )

#----solve data using mpl_bd method --##
set.seed(seed+1)
t1_mpl_bd      = proc.time()	#start time
sample_mpl_bd  = bdgraph.mpl( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_mpl_bd    = as.numeric( ( proc.time() - t1_mpl_bd)[ 3 ] ) #end and save time

all_weights_mpl_bd = round(sample_mpl_bd$all_weights,10) #obtain weights for AUC vs iteration graph
all_graphs_mpl_bd = sample_mpl_bd$all_graphs #obtain graphs for AUC vs iteration graph
sample_graphs_mpl_bd = sample_mpl_bd$sample_graphs #obtain graphs for AUC vs iteration graph

#----solve data using mpl_rj method --##
set.seed(seed+2)
t1_mpl_rj      = proc.time()	#start time
sample_mpl_rj  = bdgraph.mpl( data = data.sim, algorithm = "rjmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_mpl_rj    = as.numeric( ( proc.time() - t1_mpl_rj)[ 3 ] ) #end and save time

all_weights_mpl_rj = round(sample_mpl_rj$all_weights,10) #obtain weights for AUC vs iteration graph
all_graphs_mpl_rj = sample_mpl_rj$all_graphs #obtain graphs for AUC vs iteration graph
sample_graphs_mpl_rj = sample_mpl_rj$sample_graphs #obtain graphs for AUC vs iteration graph

#----solve data using bdmcmc method with app ratio of norm constants---#
set.seed(seed+3)
t1_bd      = proc.time()	
sample_bd  = bdgraph( data = data.sim, algorithm = "bdmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_bd   = as.numeric( ( proc.time() - t1_bd )[ 3 ] )

all_weights_bd = round(sample_bd$all_weights,10) #obtain weights for AUC vs iteration graphh
all_graphs_bd = sample_bd$all_graphs #obtain graphs for AUC vs iteration graphh
sample_graphs_bd = sample_bd$sample_graphs #obtain graphs for AUC vs iteration graphh

#----solve data using rj method with app ratio of norm constants with first jump value--------#
set.seed(seed+4)
t1_rj     = proc.time()	
sample_rj  = bdgraph( data = data.sim, algorithm = "rjmcmc", iter = iter, burnin = burnin, jump = jump, save = save,cores=cores,g.start=g.start,g.prior=g.prior)
time_rj    = as.numeric( ( proc.time() - t1_rj )[ 3 ] )
	
all_weights_rj = round(sample_rj$all_weights,10) #obtain weights for AUC vs iteration graphh
all_graphs_rj = sample_rj$all_graphs #obtain graphs for AUC vs iteration graphh
sample_graphs_rj = sample_rj$sample_graphs #obtain graphs for AUC vs iteration graphh

#----solve data using ss method --------#
set.seed(seed+5)
t1_ss      = proc.time()	
sample_ss  = ssgraph( data = data.sim, iter = iter, burnin = burnin, var1 = var1, var2=var2,lambda=lambda,g.start=g.start,save=save,cores=cores,g.prior=g.prior)
time_ss    = as.numeric( ( proc.time() - t1_ss )[ 3 ] )
	
all_weights_ss = round(sample_ss$all_weights,10) #obtain weights for AUC vs iteration graphh
all_graphs_ss = sample_ss$all_graphs #obtain graphs for AUC vs iteration graphh
sample_graphs_ss = sample_ss$sample_graphs #obtain graphs for AUC vs iteration graphh


# save solutions results in a list
return(list( 
            time_mpl_bd = time_mpl_bd, all_weights_mpl_bd = all_weights_mpl_bd, all_graphs_mpl_bd = all_graphs_mpl_bd, sample_graphs_mpl_bd = sample_graphs_mpl_bd,
            time_mpl_rj = time_mpl_rj, all_weights_mpl_rj = all_weights_mpl_rj, all_graphs_mpl_rj = all_graphs_mpl_rj, sample_graphs_mpl_rj = sample_graphs_mpl_rj,
            time_bd = time_bd, all_weights_bd = all_weights_bd, all_graphs_bd = all_graphs_bd, sample_graphs_bd = sample_graphs_bd,
            time_rj = time_rj, all_weights_rj = all_weights_rj, all_graphs_rj = all_graphs_rj, sample_graphs_rj = sample_graphs_rj,
            time_ss = time_ss, all_weights_ss = all_weights_ss, all_graphs_ss = all_graphs_ss, sample_graphs_ss = sample_graphs_ss,
            true_g = as.matrix( data.sim $ G ),
            true_K =  as.matrix( data.sim $ K ),
            true_sigma =  as.matrix( data.sim $ sigma ),
            true_data = as.matrix( data.sim $ data ) ) )
}