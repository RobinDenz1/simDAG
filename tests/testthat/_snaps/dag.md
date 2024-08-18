# S3 print method with empty dag

    An empty DAG object without any nodes.

# S3 print method with filled dag

    A DAG object with:
      -  4  nodes in total
      -  2  of which are root nodes
      -  2  of which are child nodes
      -  0  of which are time-varying nodes

# S3 summary method with empty dag

    An empty DAG object without any nodes.

# S3 summary method with filled dag

    A DAG object using the following structural equations:
    
    A ~ Bernoulli(0.1)
    B ~ Bernoulli(0.2)
    C ~ N(-10 + 0.1*A + 0.2*B, 10)
    D ~ Bernoulli(logit(-5 + 7*B + 1*C))

# S3 summary method with dag filled with formulas

    A DAG object using the following structural equations:
    
       A ~ Bernoulli(0.1)
       B ~ Bernoulli(0.2)
       C ~ N(-10 + A*0.1 + B*0.2, 10)
       D ~ Bernoulli(logit(-5 + B*7 + C*1))
       E ~ Poisson(-2 + A*4 + C*3 + A:B*2)
       F ~ NegBinomial(-2 + A*4 + C*3 + A:B*2 + log(0.34))
    G[T] ~ (-(log(Unif(0, 1))/(10*exp(A*4 + C*3 + A:B*2))))^(1/0.23)
    G[C] ~ Inf

