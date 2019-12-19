author: Johannes Boog    
date: 27.08.2019    

# Input functions for pqc_bloc

1. ~~function to read in complete *.pqc file and/or phreeqc-stand-alone input file~~
2. generic function to add a keyword specific bloc
  * ~~e.g. pqc_add_bloc(skey=c("SURFACE", "RATES",...), textbloc = character(NULL))~~
3. function to parse input blocs or complete pqc bloc
4. keyword specific functions
  * pqc_add_solution(x=list(),temp = NULL, master_species = character(NULL), density = 1, water = 1)
  * pqc_add_kineticreactant(x=list(), name = character(), formula = NULL, m = numeric(), parameter = NULL, tol = 1e-8, rate = character(NULL))
  * pqc_add_kineticreactants(x = list(), data = tibble(NULL)) 
  * pqc_add_kinetics_options(runge_kutta = 3, step_devide = 1, bad_steps = 500, cvode = FALSE, cvode_order = 5, cvode_steps = 100 )
  * pqc_add_equilibrium_phase = (x = list(), name, si = numeric(NULL), moles = numeric(NULL))
  * pqc_add_surfacesite = (x = list(), name, site_or_density = numeric(NULL), unit = c("absolute", "density"), spec_area = numeric(NULL), mass = numeric(NULL))