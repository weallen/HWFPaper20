# look at what's waiting in the queue
showq -o -p ncf
showq -o -p ncf_vdi

# start an interactive job
srun --pty -p ncf_vdi -t 0-04:00 --mem 16000 /bin/bash
srun --pty -p ncf -t 0-04:00 --mem 16000 /bin/bash

# load modules for R
module load gcc/8.2.0-fasrc01 openmpi/3.1.1-fasrc01 R/3.5.1-fasrc01
export R_LIBS_USER=$HOME/apps/R-3.5.1:$R_LIBS_USER

# load modules and run MKL version of R
module load gcc/8.2.0-fasrc01 openmpi/3.1.1-fasrc01 intel-mkl/2017.2.174-fasrc01 R/3.6.3-fasrc01
export R_LIBS_USER=$HOME/apps/R-3.6.3-MKL:$HOME/apps/R-3.5.1:$R_LIBS_USER
cd ~/HWF_analysis
~/software/bin/R
