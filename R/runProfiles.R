#'
#'@title Function to run profiles on a TCSAM2015 model.
#'
#'@description This functions runs profiles on a TCSAM2015 model.
#'
#'@details
#'This function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run a version of the TCSAM2015 model.
#'Initial model parameters can be jittered based on the system clock time as a seed
#'to the random number generator. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path to parent folder for model output
#'@param model      - TCSAM2015 model executable name
#'@param path2model - path to model executable
#'@param configFile  - path to model configuration file template
#'@param controlFile - path to model control file template
#'@param profiles    - dataframe with variables to profile on in each row
#'@param runSeqs     - TRUE to run sequences, FALSE to jitter
#'@param numRuns     - number of sequences or jitters to run
#'@param mcmc - flag (T/F) to run mcmc on "best" model
#'@param mc.N - number of mcmc iterations to make
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param plotResults - T/F to plot final results within each profile using \code{???}
#'
#'@return - par file dataframe
#'
#'@export
#'
runProfiles<-function(os='osx',
                      path='.',
                      model='tcsam2015',
                      path2model='',
                      configFileTemplate=NULL,
                      controlFileTemplate=NULL,
                      profiles=NULL,
                      runSeqs=TRUE,
                      numRuns=4,
                      mcmc=FALSE,
                      mc.N=1000000,
                      mc.save=1000,
                      mc.scale=1000,
                      plotResults=TRUE){
    #read template files
    cfgt<-readLines(con=file.path(path,configFileTemplate));
    ctlt<-readLines(con=file.path(path,controlFileTemplate));
    
    #create string representation of configuration file
    cfgFile<-gsub("&&ControlFile","ModelControl.txt",cfgt,fixed=TRUE)

    #create profiles to run
    nprf<-nrow(profiles);
    prfList<-list();
    for (iprf in 1:nprf){
        #create one profile
        vals<-profiles[iprf,,drop=FALSE];#need drop=FALSE to keep column names for 1-column dataframes
        prfDir<-'prof.';
        prfCtl<-ctlt;
        for (nm in names(vals)){
            prfDir<-paste(prfDir,"_",nm,"=",vals[1,nm],sep='');
            prfCtl<-gsub(paste('&&',nm,sep=''),vals[1,nm],prfCtl,fixed=TRUE)
        }
        prfDir<-file.path(path,prfDir);
        if (!file.exists(prfDir)) dir.create(prfDir,recursive=TRUE); #create folder for profile, if necessary
        writeLines(cfgFile,file.path(prfDir,'ModelConfig.txt'))
        writeLines(prfCtl,file.path(prfDir,'ModelControl.txt'))
        #run profile
        xprfDir<-normalizePath(prfDir);
        cfgFN<-file.path(xprfDir,'ModelConfig.txt');##best to specify absolute path here
        cat('##Running profile ',xprfDir,'\n')
        if (runSeqs){
            par<-runSequence(path=xprfDir,
                             os=os,
                             model=model,
                             path2model=path2model,
                             configFile=cfgFN,
                             numRuns=numRuns,
                             mcmc=mcmc,
                             mc.N=mc.N,
                             mc.save=mc.save,
                             mc.scale=mc.scale,
                             plotResults=plotResults);
        } else {
            par<-runJitter(path=xprfDir,
                           os=os,
                           model=model,
                           path2model=path2model,
                           configFile=cfgFN,
                           numRuns=numRuns,
                           mcmc=mcmc,
                           mc.N=mc.N,
                           mc.save=mc.save,
                           mc.scale=mc.scale,
                           plotResults=plotResults);
        }
        prfList[[prfDir]]<-par;
    }
    
    return(prfList);
}