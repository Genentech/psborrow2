# show works for Analysis

    Code
      show(object)
    Output
      Analysis Object
      
      Outcome model: ExponentialSurvDist 
      Outcome variables: time cnsr 
      
      Borrowing method: Full borrowing 
      External flag: ext 
      
      Treatment variable: trt 
      
      Covariates: cov1 cov2 
      
      Data: Matrix with 724 observations 
          -  120  internal controls
          -  344  external controls 
          -  260  internal experimental
      
      Not ready to sample yet.

# show works without covariates

    Code
      show(object)
    Output
      Analysis Object
      
      Outcome model: ExponentialSurvDist 
      Outcome variables: time cnsr 
      
      Borrowing method: Full borrowing 
      External flag: ext 
      
      Treatment variable: trt 
      
      Data: Matrix with 724 observations 
          -  120  internal controls
          -  344  external controls 
          -  260  internal experimental
      
      Not ready to sample yet.

# show works with no borrowing

    Code
      show(object)
    Output
      Analysis Object
      
      Outcome model: ExponentialSurvDist 
      Outcome variables: time cnsr 
      
      Borrowing method: No borrowing 
      External flag: ext 
      
      Treatment variable: trt 
      
      Data: Matrix with 724 observations 
          -  120  internal controls
          -  344  external controls  (ignored in this analysis)
          -  260  internal experimental
      
      Not ready to sample yet.

