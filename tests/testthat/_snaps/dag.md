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

