#!/bin/bash

## args: jobid

### this script waits for sge job to be done

## TODO Process jobid_qsub in case it contains a message with job id
jobid="$1"
## remove leading spaces
jobid=${jobid##+([[:space:]])}
## remove trailing spaces
jobid=${jobid%%}

### if jobid is not an integer, process string to find it
if [[ ! $jobid =~ ^[0-9]+$ ]]; then
    ### Example:
    ## Unable to run job: warning: philipde's job is not allowed to run in any queue
    ## Your job 10430 ("xgxr032_datalist_01_105.mod") has been submitted
    ## Exiting.
    
    jobid=$(echo "$jobid" | grep -oP 'Your job \K\d+')
fi

###### for some reason I am getting error: return: can only `return' from a function or sourced script

## If we still don't have an integer, report that we can't wait for it and paste $jobid_qsub
if [[ ! $jobid =~ ^[0-9]+$ ]]; then 
    echo "Cannot identify queue job ID"
 #   return 1;
#fi

else 
    
### We have an integer. We wait for the job to be done
###

### qsub -hold_jid "$jobid" -b y sleep 1

    while true; do
	qstat -j "$jobid" &> /dev/null
	if [[ $? -ne 0 ]]; then
	    ## job completed
	    break
	fi
	sleep 5
    done
    

#return 0;

fi
