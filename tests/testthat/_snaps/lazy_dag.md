# S3 print method general

    A lazy DAG object with:
      -  4  nodes in total
      -  2  of which are root nodes
      -  2  of which are child nodes
      -  0  of which are time-varying nodes
      -  0  imposed network structure(s)
      -  1  unspecified parameter(s)

# S3 print method with node that is both time-dependent and not

    Code
      print(dag)
    Output
      A lazy DAG object with:
        -  1  nodes in total
        -  0  of which are root nodes
        -  0  of which are child nodes
        -  1  of which are time-varying nodes
        -  0  imposed network structure(s)
        -  1  unspecified parameter(s)

# S3 summary method general

    A lazy DAG object using the following structural equations:
    
    A ~ Bernoulli(0.1)
    B ~ Bernoulli(0.2)
    C ~ N(-10 + 0.1*A + 0.2*B, Inf)
    D ~ Bernoulli(logit(-5 + 7*B + 1*C))
    
    Unspecified parameters are denoted as Inf.

# S3 summary method with dag filled with formulas

    A lazy DAG object using the following structural equations:
    
       A ~ Bernoulli(0.1)
       B ~ Bernoulli(0.2)
       C ~ N(-10 + A*0.1 + B*0.2, 10)
       D ~ Bernoulli(logit(-5 + B*7 + C*1))
       E ~ Poisson(exp(-2 + A*4 + C*3 + A:B*2))
       F ~ NegBinomial(Inf + A*4 + C*3 + A:B*2 + log(Inf))
    G[T] ~ (-(log(Unif(0, 1))/(10*exp(A*4 + C*3 +
             A:B*2))))^(1/0.23)
    G[C] ~ Inf
    
    Unspecified parameters are denoted as Inf.

# S3 plot method general

    

