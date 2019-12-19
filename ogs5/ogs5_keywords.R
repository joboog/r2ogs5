
# ogs5 keyword list -------------------------------------------------------
ogs5_keywordlist <- list()

# bc ----------------------------------------------------------------------
ogs5_keywordlist$bc <- list()
ogs5_keywordlist$bc$mkey <- "#BOUNDARY_CONDITION"
ogs5_keywordlist$bc$skey = "$COMP_NAME 
                $CONSTRAINED 
                $COPY_VALUE 
                $DIS_TYPE 
                $DIS_TYPE_CONDITION 
                $EPSILON 
                $EXCAVATION 
                $FCT_TYPE 
                $GEO_TYPE 
                $MSH_TYPE 
                $NO_DISP_INCREMENT 
                $PCS_TYPE 
                $PRESSURE_AS_HEAD 
                $PRIMARY_VARIABLE 
                $TIME_CONTROLLED_ACTIVE 
                $TIM_TYPE"

# cct ---------------------------------------------------------------------
ogs5_keywordlist$cct$mkey <- "#COMMUNICATION_TABLE"
ogs5_keywordlist$cct$skey <- "$MYRANK
                        $NEIGHBOR
                        $NNEIGHBORS "

# fct ---------------------------------------------------------------------
ogs5_keywordlist$fct$mkey <- "#FUNCTION"
ogs5_keywordlist$fct$skey <- "$DATA
                        $DIMENSION
                        $DIS_TYPE
                        $GEO_TYPE
                        $MATRIX
                        $TYPE
                        $VARIABLES "

# gem ---------------------------------------------------------------------
ogs5_keywordlist$gem$mkey <- "#GEM_PROPERTIES"
ogs5_keywordlist$gem$skey <- "$CALCULATE_BOUNDARY_NODES
                        $DISABLE_GEMS
                        $FLAG_COUPLING_HYDROLOGY
                        $FLAG_DISABLE_GEM
                        $FLAG_POROSITY_CHANGE
                        $GEM_CALCULATE_BOUNDARY_NODES
                        $GEM_INIT_FILE
                        $GEM_THREADS
                        $ITERATIVE_SCHEME
                        $KINETIC_GEM
                        $MAX_FAILED_NODES
                        $MAX_POROSITY
                        $MIN_POROSITY
                        $MY_SMART_GEMS
                        $PRESSURE_GEM
                        $TEMPERATURE_GEM
                        $TRANSPORT_B "

# gli ---------------------------------------------------------------------
ogs5_keywordlist$gli$mkey <- "#POINTS
                        #POLYLINE
                        #STOP
                        #SURFACE
                        #VOLUME "
ogs5_keywordlist$gli$skey <- "$EPSILON
                        $ID
                        $LAYER
                        $MAT_GROUP
                        $MD
                        $NAME
                        $POINTS
                        $POINT_VECTOR
                        $POLYLINES
                        $SURFACES
                        $TIN
                        $TYPE "

# ic ----------------------------------------------------------------------
ogs5_keywordlist$ic$mkey <- "#INITIAL_CONDITION"
ogs5_keywordlist$ic$skey <- "$COMP_NAME
                        $DIS_TYPE
                        $GEO_TYPE
                        $PCS_TYPE
                        $PRIMARY_VARIABLE
                        $STORE_VALUES "

# krc ---------------------------------------------------------------------
ogs5_keywordlist$krc$mkey <- "#BLOB_PROPERTIES
                        #KINREACTIONDATA
                        #MICROBE_PROPERTIES
                        #REACTION "
ogs5_keywordlist$krc$skey <- "$ACTIVITY_MODEL
                        $ALLOW_REACTIONS
                        $BACTERIACAPACITY
                        $BACTERIAL_YIELD
                        $BACTERIANAME
                        $BACTERIA_SPECIFIC_CAPACITY
                        $BASETERM
                        $CALC_SHERWOOD
                        $CALC_SHERWOOD_MODIFIED
                        $CHEMAPPNAME
                        $COPY_CONCENTRATIONS
                        $D50
                        $DEBUG_OUTPUT
                        $DM
                        $DS
                        $EQUATION
                        $EQUILIBRIUM_CONSTANT
                        $EXCHANGE_PARAMETERS
                        $GAS_DISSOLUTION
                        $GEOMETRY
                        $GRAIN_SPHERE_RATIO
                        $GROWTH
                        $INHIBITIONTERMS
                        $INITIAL_TIMESTEP
                        $INTERFACIAL_AREA
                        $ISOTOPE_FRACTIONATION
                        $LAGNEAU_BENCHMARK
                        $LENGTH
                        $MECHANISMTERM
                        $MICROBENAME
                        $MINERALNAME
                        $MIN_BACTERIACONC
                        $MIN_CONCENTRATION_REPLACE
                        $MIN_TIMESTEP
                        $MONODTERMS
                        $MONOD_REACTION_NAME
                        $NAME
                        $NAPL_CONTENT_INI
                        $NAPL_CONTENT_RES
                        $NAPL_PROPERTIES
                        $NO_REACTIONS
                        $OMEGA_THRESHOLD
                        $PRECIPITATION_BY_BASETERM_ONLY
                        $PRECIPITATION_EXPONENT
                        $PRECIPITATION_FACTOR
                        $PRODUCTIONSTOCH
                        $PRODUCTIONTERMS
                        $RATECONSTANT
                        $RATE_EXPONENTS
                        $REACTION_DEACTIVATION
                        $REACTION_ORDER
                        $REACTIVE_SURFACE_AREA
                        $RELATIVE_ERROR
                        $SCALE_DCDT
                        $SHERWOOD_MODEL
                        $SOLVER_TYPE
                        $SORPTION_TYPE
                        $SORT_NODES
                        $STANDARD_GIBBS_ENERGY
                        $SURFACES
                        $SWITCH_OFF_GEOMETRY
                        $TEMPERATURE_DEPENDENCE
                        $THRESHHOLDTERMS
                        $TORTUOSITY
                        $TYPE
                        $UI
                        $drmc_
                        $drmc__PARAMETERS "

# mcp ---------------------------------------------------------------------
ogs5_keywordlist$mcp$mkey <- "#COMPONENT_PROPERTIES"
ogs5_keywordlist$mcp$skey <- "$ACENTRIC_FACTOR
                        $A_ZERO
                        $BUBBLE_VELOCITY
                        $CRITICAL_PRESSURE
                        $CRITICAL_TEMPERATURE
                        $DECAY
                        $DIFFUSION
                        $FLUID_ID
                        $FLUID_PHASE
                        $FORMULA
                        $ISOTHERM
                        $MAXIMUM_AQUEOUS_SOLUBILITY
                        $MINERAL_DENSITY
                        $MOBILE
                        $MOLAR_DENSITY
                        $MOLAR_VOLUME
                        $MOLAR_WEIGHT
                        $MOL_MASS
                        $NAME
                        $OutputMassOfComponentInModel
                        $TRANSPORT_PHASE
                        $VALENCE
                        $VOLUME_DIFFUSION "

# mfp ---------------------------------------------------------------------
ogs5_keywordlist$mfp$mkey <- "#FLUID_PROPERTIES"
ogs5_keywordlist$mfp$skey <- "$COMPONENTS
                        $COMPRESSIBILITY
                        $DAT_TYPE
                        $DECAY
                        $DENSITY
                        $DIFFUSION
                        $DRHO_DT_UNSATURATED
                        $EOS_TYPE
                        $FLUID_NAME
                        $FLUID_TYPE
                        $GRAVITY
                        $HEAT_CONDUCTIVITY
                        $ISOTHERM
                        $JTC
                        $NON_GRAVITY
                        $PHASE_DIFFUSION
                        $SPECIFIC_HEAT_CAPACITY
                        $SPECIFIC_HEAT_SOURCE
                        $TEMPERATURE
                        $VISCOSITY "

# mmp ---------------------------------------------------------------------
ogs5_keywordlist$mmp$mkey <- "#MEDIUM_PROPERTIES" #MEDIUM_PROPERTIES_DISTRIBUTED "
ogs5_keywordlist$mmp$skey <- "$CAPILLARY_PRESSURE
                        $CHANNEL
                        $COMPOUND_DEPENDENT_DT
                        $CONDUCTIVITY_MODEL
                        $CONVERSION_FACTOR
                        $DATA
                        $DIFFUSION
                        $DIS_TYPE
                        $ELEMENT_VOLUME_MULTIPLYER
                        $EVAPORATION
                        $FLOWLINEARITY
                        $GEOMETRY_AREA
                        $GEOMETRY_DIMENSION
                        $GEOMETRY_INCLINATION
                        $GEO_TYPE
                        $HEAT_DISPERSION
                        $HEAT_TRANSFER
                        $INTERPHASE_FRICTION
                        $MASS_DISPERSION
                        $MMP_TYPE
                        $MSH_TYPE
                        $NAME
                        $ORGANIC_CARBON
                        $PARTICLE_DIAMETER
                        $PCS_TYPE
                        $PERMEABILITY_DISTRIBUTION
                        $PERMEABILITY_FUNCTION_DEFORMATION
                        $PERMEABILITY_FUNCTION_EFFSTRESS
                        $PERMEABILITY_FUNCTION_POROSITY
                        $PERMEABILITY_FUNCTION_PRESSURE
                        $PERMEABILITY_FUNCTION_STRAIN
                        $PERMEABILITY_FUNCTION_STRESS
                        $PERMEABILITY_FUNCTION_VELOCITY
                        $PERMEABILITY_SATURATION
                        $PERMEABILITY_TENSOR
                        $POROSITY
                        $POROSITY_DISTRIBUTION
                        $RILL
                        $SPECIFIC_STORAGE
                        $STORAGE
                        $STORAGE_FUNCTION_EFFSTRESS
                        $SURFACE_FRICTION
                        $TORTUOSITY
                        $TRANSFER_COEFFICIENT
                        $UNCONFINED
                        $VOL_BIO
                        $VOL_MAT
                        $WIDTH "

# msh ---------------------------------------------------------------------
ogs5_keywordlist$msh$mkey <- "#FEM_MSH"
ogs5_keywordlist$msh$skey <- "$AREA
                        $AXISYMMETRY
                        $CROSS_SECTION
                        $ELEMENTS
                        $GEO_NAME
                        $GEO_TYPE
                        $LAYER
                        $NODES
                        $PCS_TYPE "

# msp ---------------------------------------------------------------------
ogs5_keywordlist$msp$mkey <- "SOLID_PROPERTIES"
ogs5_keywordlist$msp$skey <- "$BIOT_CONSTANT
                        $CREEP
                        $DENSITY
                        $ELASTICITY
                        $EXCAVATION
                        $E_Function
                        $GRAVITY_CONSTANT
                        $MICRO_STRUCTURE_PLAS
                        $NAME
                        $NON_REACTIVE_FRACTION
                        $PLASTICITY
                        $REACTIVE_SYSTEM
                        $SOLID_BULK_MODULUS
                        $SPECIFIC_HEAT_SOURCE
                        $STRESS_INTEGRATION_TOLERANCE
                        $STRESS_UNIT
                        $SWELLING_PRESSURE_TYPE
                        $THERMAL
                        $THRESHOLD_DEV_STR
                        $TIME_DEPENDENT_YOUNGS_POISSON "

# num ---------------------------------------------------------------------
ogs5_keywordlist$num$mkey <- "#NUMERICS"
ogs5_keywordlist$num$skey <- "$COUPLED_PROCESS
                        $COUPLING_CONTROL
                        $COUPLING_ITERATIONS
                        $DYNAMIC_DAMPING
                        $ELE_GAUSS_POINTS
                        $ELE_MASS_LUMPING
                        $ELE_SUPG
                        $ELE_UPWINDING
                        $EXTERNAL_SOLVER_OPTION
                        $FEM_FCT
                        $GRAVITY_PROFILE
                        $LINEAR_SOLVER
                        $LOCAL_PICARD1
                        $NON_LINEAR_ITERATION
                        $NON_LINEAR_SOLVER
                        $NON_LINEAR_UPDATE_VELOCITY
                        $OVERALL_COUPLING
                        $PCS_TYPE
                        $PLASTICITY_TOLERANCE
                        $RENUMBER
                        $TIME_STEPS "

# out ---------------------------------------------------------------------
ogs5_keywordlist$out$mkey <- "#OUTPUT"
ogs5_keywordlist$out$skey <- "$AMPLIFIER
                        $DAT_TYPE
                        $DIS_TYPE
                        $ELE_VALUES
                        $GEO_TYPE
                        $MFP_VALUES
                        $MMP_VALUES
                        $MSH_TYPE
                        $NOD_VALUES
                        $PCON_VALUES
                        $PCS_TYPE
                        $RWPT_VALUES
                        $TECPLOT_ZONE_SHARE
                        $TIM_TYPE
                        $VARIABLESHARING "

# pcs ---------------------------------------------------------------------
ogs5_keywordlist$pcs$mkey <- "#PROCESS"
ogs5_keywordlist$pcs$skey <- "$APP_TYPE
                        $BOUNDARY_CONDITION_OUTPUT
                        $COUNT
                        $CPL_TYPE
                        $DEACTIVATED_SUBDOMAIN
                        $DISSOLVED_CO2_INGAS_PCS_NAME
                        $DISSOLVED_CO2_PCS_NAME
                        $ELEMENT_MATRIX_OUTPUT
                        $GEO_TYPE
                        $MEDIUM_TYPE
                        $MEMORY_TYPE
                        $MSH_TYPE
                        $NEGLECT_H_INI_EFFECT
                        $NUM_TYPE
                        $OutputMassOfGasInModel
                        $PCS_TYPE
                        $PHASE_TRANSITION
                        $PRIMARY_VARIABLE
                        $PROCESSED_BC
                        $RELOAD
                        $SATURATION_SWITCH
                        $SAVE_ECLIPSE_DATA_FILES
                        $SIMULATOR
                        $SIMULATOR_MODEL_PATH
                        $SIMULATOR_PATH
                        $SIMULATOR_WELL_PATH
                        $ST_RHS
                        $TIME_CONTROLLED_EXCAVATION
                        $TIM_TYPE
                        $UPDATE_INI_STATE
                        $USE_PRECALCULATED_FILES
                        $USE_VELOCITIES_FOR_TRANSPORT "

# pqc ---------------------------------------------------------------------
ogs5_keywordlist$pqc$mkey <- "#ende 
                        #libprint"

# rei ---------------------------------------------------------------------
ogs5_keywordlist$rei$mkey <- "#REACTION_INTERFACE"
ogs5_keywordlist$rei$skey <- "$ALL_PCS_DUMP
                        $DISSOLVED_NEUTRAL_CO2_SPECIES_NAME
                        $HEATPUMP_2DH_TO_2DV
                        $INITIAL_CONDITION_OUTPUT
                        $MOL_PER
                        $PCS_RENAME_INIT
                        $PCS_RENAME_POST
                        $PCS_RENAME_PRE
                        $POROSITY_RESTART
                        $PRESSURE
                        $P_VLE
                        $RESIDUAL
                        $SODIUM_SPECIES_NAME
                        $SOLID_SPECIES_DUMP_MOLES
                        $TEMPERATURE
                        $UPDATE_INITIAL_SOLID_COMPOSITION
                        $VLE
                        $WATER_CONCENTRATION
                        $WATER_SATURATION_LIMIT
                        $WATER_SPECIES_NAME "

# tim ---------------------------------------------------------------------
ogs5_keywordlist$tim$mkey <- "#TIME_STEPPING"
ogs5_keywordlist$tim$skey <- "$CRITICAL_TIME
                        $INDEPENDENT
                        $PCS_TYPE
                        $SUBSTEPS
                        $TIME_CONTROL
                        $TIME_END
                        $TIME_FIXED_POINTS
                        $TIME_SPLITS
                        $TIME_START
                        $TIME_STEPS
                        $TIME_UNIT "

# st ----------------------------------------------------------------------
ogs5_keywordlist$st$mkey <- "#SOURCE_TERM"
ogs5_keywordlist$st$skey <- "$CRITICAL_TIME
                        $INDEPENDENT
                        $PCS_TYPE
                        $SUBSTEPS
                        $TIME_CONTROL
                        $TIME_END
                        $TIME_FIXED_POINTS
                        $TIME_SPLITS
                        $TIME_START
                        $TIME_STEPS
                        $TIME_UNIT "


# function to extract and list ogs5_keywordlist -----------------------------------
ogs5_extract_ogs5_keywordlist <- 
  function(x) {
    
    for (i in seq_len(length(x))){
      x[[i]]$mkey <- x[[i]]$mkey %>% 
        str_squish() %>% 
        str_split(pattern = " ") %>% 
        .[[1]] %>% as.character %>% 
        str_remove(pattern = "\\#")
      
      if (!is.null(x[[i]]$skey)){
        x[[i]]$skey <- x[[i]]$skey %>% 
          str_squish() %>% 
          str_split(pattern = " ") %>% 
          .[[1]] %>% as.character %>% 
          str_remove(pattern = "\\$")
      }
    }
    return(x)
  }

ogs5_keywordlist <- ogs5_extract_ogs5_keywordlist(ogs5_keywordlist)