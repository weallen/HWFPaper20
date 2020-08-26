# R Code / Notebooks

## Utility Files

+ `install-r-mkl.sh`: On the FAS cluster, the default linear algebra
  libraries used by R are very slow. By following the commands in this
  file we can compile R from source and link it with Intel MKL, which is
  an extremely fast linear algebra library. Might not be super necessary
  but could speed up things.

+ `scratch.sh`: Bunch of bash snippets to run various things on the
  cluster.

## Code / Analysis Files

1. `analysis0_cleandata.R`: Glen's data cleaning script.
2. `subset_testwindow.R`: Subset the data to within a window around the
   test date. For individuals who were tested, we restrict their
   responses to the time two weeks before and after their first test
   date. For individuals who ever tested positive, the first test date
   is the first date positive test. Otherwise, the first test date is
   the earliest test date. For individuals who were not tested, we use
   the date one week before their last response as the pseudo test date.
   We take data two weeks before the pseudo test date and one week after
   the pseudo test date.
3. `household-transmission.R`: Study household transmission.
4. `testing-behavior.Rmd`: Study behavior in response to testing.
