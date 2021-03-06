#'
#'@title Function to run TCSAM2015 multiple times using jittered initial parameter values.
#'
#'@description This functions runs a TCSAM2015 model multiple times, jittering the
#'initial staarting values to assess model convergence.
#'
#'@details
#'For each model run, this function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run the ADMB version of the TCSAM2015 model.
#'Initial model parameters are jittered based on the system clock time as a seed
#'to the random number generator. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'When all the models requested have been run,
#'the function determines the seed associated with the 1st model run that yielded
#'the smallest value for the objective function and re-runs the modelusing this seed
#'to re-create the model run resulting in the minimum objectve function to recreate
#'the model output files. The final model run is done estimating the hessian, so
#'standard deviations for estimated model parameters are available in the .std file.
#'
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path for model output
#'@param model      - TCSAM2015 model executable name
#'@param path2model - path to model executable
#'@param configFile - path to model configuration file
#'@param numRuns    - number of jitter runs to make
#'@param onlyEvalJitter - flag (T/F) to only evaluate a (previous) set of jitter runs, not make new runs
#'@param in.csv - filename for jitter info (seed, obj fun value) from ADMB model run
#'@param out.csv - filename for jittered results
#'@param mcmc - flag (T/F) to run mcmc on "best" model
#'@param mc.N - number of mcmc iterations to make
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param plotResults - T/F to plot final results using \code{???}
#'
#'@return - list w/ 4 elements:
#'  imx  - index of (1st) smallest value for the objective function
#'  seed - seed resulting in the smallest objective function
#'  par  - dataframe with par results from run w/ smallest objective function
#'  objFuns - vector of objective function values from all model runs
#'  parList - list of par dataframes for each model run
#'
#'@export
#'
runJitter<-function(os='osx',
                    path='.',
                    model='tcsam2015',
                    path2model='',
                    configFile='',
                    numRuns=3,
                    onlyEvalJitter=FALSE,
                    in.csv='jitterInfo.csv',
                    out.csv='jitterResults.csv',
                    mcmc=FALSE,
                    mc.N=1000000,
                    mc.save=1000,
                    mc.scale=1000,
                    plotResults=FALSE){
    #start timing
    stm<-Sys.time();

    #set up output
    out.csv<-file.path(path,out.csv)

    #run models
    if (!onlyEvalJitter){
        rc<-0;
        parList<-list();
        for (r in 1:numRuns){
            cat("\n\n---running ADMB program for",r,"out of",numRuns,"---\n\n");
            fldr<-paste('run',wtsUtilities::formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
            p2f<-file.path(path,fldr);
            par<-runTCSAM2015(path=p2f,
                              os=os,
                              model=model,
                              path2model=path2model,
                              configFile=configFile,
                              pin=FALSE,
                              hess=FALSE,
                              mcmc=FALSE,
                              jitter=TRUE,
                              jit.seed=NULL,
                              plotResults=FALSE);
            if (!is.null(par)){
                rc<-rc+1;
                objFun  <-par$value[par$name=='objective function'];
                seed    <-par$value[par$name=='seed'];
                maxgrad <-par$value[par$name=='max gradient'];
                tbl<-data.frame(idx=r,objFun=objFun,maxGrad=maxgrad,seed=seed);
                if (r==1) write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
                if (r>1)  write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
            }
            parList[[fldr]]<-par;
        }
    }

    #determine row index associated w/ minimum obj fun value
    #read jitter results from file
    tbl<-read.csv(out.csv);
    idx<-order(tbl$objFun,abs(tbl$maxGrad));
    best<-tbl$idx[idx[1]];
    seed<-tbl$seed[idx[1]];
    if (onlyEvalJitter){parList<-NULL;}

    #re-run case associated with mininum objective function value, save in "best.runxx"
    cat("\n\n---Re-running ADMB program for",idx[1],"out of",numRuns,"as best run---\n");
    fldr<-paste('best.run',wtsUtilities::formatZeros(best,width=max(2,ceiling(log10(numRuns)))),sep='');
    p2f<-file.path(path,fldr);
    cat("---Output folder is '",p2f,"'\n\n",sep='');
    par<-runTCSAM2015(path=p2f,
                      os=os,
                      model=model,
                      path2model=path2model,
                      configFile=configFile,
                      pin=FALSE,
                      hess=TRUE,
                      mcmc=mcmc,
                      mc.N=mc.N,
                      mc.save=mc.save,
                      mc.scale=mc.scale,
                      jitter=TRUE,
                      jit.seed=seed,
                      plotResults=plotResults);

    #print timing-related info
    etm<-Sys.time();
    elt<-etm-stm;
    cat("start time: ")
    print(stm);
    cat("end time: ")
    print(etm);
    cat("elapsed time: ")
    print(elt);

    #return output
    return(list(imn=best,seed=seed,par=par,objFuns=tbl,parList=parList));
}
#res<-jitterTCSAM2015(200);
