# show CorrelatedCovariates works for narrow matrices

    Code
      show(with_age)
    Output
      Baseline Data Simulation Object
        N internal treated:  100 
        N internal control:  50 
        N external control:  100 
      
      Covariates: 
      [[1]]
       covariate means_internal means_external
             age             55             55
      
      Covariance Matrices
      Internal   External
          age        age 
      age   5    age   5 
      

# show CorrelatedCovariates works for wide matrices

    Code
      show(with_age)
    Output
      Baseline Data Simulation Object
        N internal treated:  100 
        N internal control:  50 
        N external control:  100 
      
      Covariates: 
      [[1]]
             covariate means_internal means_external
       age_at_baseline             55             55
      
      Covariance Matrices
      Internal                          External
                      age_at_baseline                   age_at_baseline 
      age_at_baseline               5   age_at_baseline               5 
      

# show CorrelatedCovariates works for very wide matrices

    Code
      show(with_age)
    Output
      Baseline Data Simulation Object
        N internal treated:  100 
        N internal control:  50 
        N external control:  100 
      
      Covariates: 
      [[1]]
                                                      covariate means_internal
       number_of_years_since_the_date_of_birth_at_baseline_date             55
       means_external
                   55
      
      Covariance Matrices
      Internal
                                                               number_of_years_since_the_date_of_birth_at_baseline_date 
      number_of_years_since_the_date_of_birth_at_baseline_date                                                        5 
      
      External
                                                               number_of_years_since_the_date_of_birth_at_baseline_date 
      number_of_years_since_the_date_of_birth_at_baseline_date                                                        5 
      

