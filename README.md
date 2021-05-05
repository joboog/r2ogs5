# r2ogs5

`r2ogs5` is an R-API to the opens-ource multiphysics simulator [OpenGeoSys 5](https://www.opengeosys.org/ogs-5/).
`r2ogs5` allows to

* reading and preparing input files
* generate simple structured meshes
* executing single and ensemble simulation of serial and parallelized OGS models
* retrieving, post-processing and visualizing OGS output data
* calibrate OGS simulation models with Bayesian Optimization


## Installation

### Set-Up your Environment

Before you install and load `r2ogs5` you need to set-up your python environment. 
This is required to read in the `.vtu` and `.pvd` files produced by OpenGeoSys 5.
You need a Python installation including the libraries `numpy` and `vtk`. 
If you have an Anaconda installation on your system, setting up a Python environment with `numpy` and `vtk` will work. `r2ogs5` was tested with the following environment:
```
$ conda create -n r2ogs5 python=3.7
$ conda install -c anaconda numpy, vtk==8.2.0
```

If you don't have any Python installation on your system, you can download the latest release of Python from [here](https://www.python.org/downloads/). Instructions on how to install `numpy` and `vtk` can be found [here](https://numpy.org/install/) and  [here](https://pypi.org/project/vtk/).


### Install r2ogs5

The first option for installation is directly from the online repository via
```
devtools::install_gitlab("online/repo/path")
```
in your R console.  

The second option is first cloning the repository by typing in a terminal 
```
git clone online/repo/path
```
and then installing from the local repository via
```
devtools::install("local/repo/path")
```
in your R console.

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


## Usage

For using the package please consider the vignettes:

* [base package functionality](vignettes/r2ogs5.Rmd)
* [define ensemble runs](Ensembles/cal_bayesOpt.Rmd)
* [model calibration with Bayesian Optimization](vignettes/cal_bayesOpt.Rmd)


## Get Help

* please report bugs via opening an issue
* or ask question on our [OGS Q&A page](https://discourse.opengeosys.org/)
