### Install R 3.6.1 with MKL
module purge
module load gcc/8.2.0-fasrc01 openmpi/3.1.1-fasrc01 intel-mkl/2017.2.174-fasrc01 R/3.6.3-fasrc01
module list


# Assumes you've downloaded the R sources from e.g. https://cran.r-project.org/
tar -xzvf R-3.6.3.tar.gz

cd R-3.6.3/
source /n/sw/intel-cluster-studio-2017/compilers_and_libraries_2017.2.174/linux/mkl/bin/mklvars.sh intel64
MKL="-Wl,--no-as-needed -lmkl_gf_lp64 -Wl,--start-group -lmkl_gnu_thread -lmkl_core -Wl,--end-group -fopenmp -ldl -lpthread -lm"

# R will be installed under $HOME/software
mkdir $HOME/software
./configure --with-blas="$MKL" --with-lapack --with-x=no --prefix=$HOME/software

make
make install

# directory for your R packages
mkdir ~/apps/R-3.6.3-MKL
export R_LIBS_USER=$HOME/apps/R-3.6.3-MKL:$R_LIBS_USER
echo $R_LIBS_USER
