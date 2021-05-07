# r2ogs5

`r2ogs5` is an R-API to the opens-ource multiphysics simulator [OpenGeoSys 5](https://www.opengeosys.org/ogs-5/).
`r2ogs5` allows to

* reading and preparing input files
* generate simple structured meshes
* executing single and ensemble simulation of serial and parallelized OGS models
* retrieving, post-processing and visualizing OGS output data
* calibrate OGS simulation models with Bayesian Optimization


## Installation

Package installation was tested for Linux (Ubuntu 20.4) and Windows, package functionalites are only tested for Linux operating systems so far. 

### Set-Up your Environment

Before you install and load `r2ogs5` you need to set-up your python environment. 
This is required to read in the `.vtu` and `.pvd` files produced by OpenGeoSys 5.
You need a Python installation including the libraries `numpy` and `vtk`. 
If you have an [Anaconda](https://www.anaconda.com/products/individual) installation on your system, setting up a Python environment with `numpy` and `vtk` will work. `r2ogs5` was tested with the following environment:
```
$ conda create -n r2ogs5 python=3.8
$ conda install -n r2ogs5 -c conda-forge vtk numpy
$ conda activate r2ogs5
``` 
The configuration of avialable python environments on the system can be found via

```
reticulate::conda_list()
```
In R, the path for the python environment needs to be indicated to the package `reticulate`. This can be achieved e.g. via:
```
Sys.setenv(RETICULATE_PYTHON = "C:/Users/admin/anaconda3/envs/r2ogs5/python.exe")
```


If you don't have any Python installation on your system, you can download the latest release of Python from [here](https://www.python.org/downloads/). Instructions on how to install `numpy` and `vtk` can be found [here](https://numpy.org/install/) and  [here](https://pypi.org/project/vtk/).





### Install r2ogs5

The first option for installation is directly from the online repository via
```
devtools::install_git("https://gitlab.opengeosys.org/ogs5/r2ogs5")
```
or alternatively 
```
remotes::install_git("https://gitlab.opengeosys.org/ogs5/r2ogs5")
```
in your R console.  

The second option is first cloning the repository by typing in a terminal 
```
git clone https://gitlab.opengeosys.org/ogs5/r2ogs5.git
cd r2ogs5
R --slave -e 'devtools::install(".")'
```

Note that it is necessary to have git https://www.git-scm.com/
and/or the R-package `devtools` installed on your system. 


### Get OpenGeoSys 5  

Finally, the actual simulation program, OpenGeoSys 5 needs to be available on the system.
*r2ogs5* does not come with OGS5 executables, as they usually
need to be compiled specifically for the machine run on. For this tutorial, the default compiled
executables should be sufficient which can be downloaded from [here](https://www.opengeosys.org/ogs-5/) for Linux, macOS, and Windows.  
The name (here *ogs_fem*), and location of the OGS5 executable have to be set accordingly via `options`.

```
options(r2ogs5.default_ogs5_bin = "path/to/ogs_fem")
```
Note: To install r2ogs5 including vignettes, the packages `knitr`, `rmarkdown` with `pandoc` need to be installed on the system. In addition, an ogs executable named "ogs_fem" needs to be provided in the search path of the OS e.g. in */usr/local/sbin*, */usr/local/bin* */usr/sbin*, */usr/bin*. This is currently only possible for Linux operating systems. 

## Usage

For using the package please consider the vignettes:

* [base package functionality](vignettes/r2ogs5.Rmd)
* [define ensemble runs](Ensembles/cal_bayesOpt.Rmd)
* [model calibration with Bayesian Optimization](vignettes/cal_bayesOpt.Rmd)


## Get Help

* please report bugs via opening an issue
* or ask question on our [OGS Q&A page](https://discourse.opengeosys.org/)


## Contribute

If you want to contribute to *r2ogs5* look [here](CONTRIBUTING.md), please.
