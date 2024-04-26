# S3 print list of nodes

    A list of DAG.node objects.

# S3 print root node, no params

    A DAG.node object specifying a single root node with:
      - name: 'A'
      - type: 'rbernoulli'
      - no additional parameters

# S3 print root node, with params

    A DAG.node object specifying a single root node with:
      - name: 'A'
      - type: 'rbernoulli'
      - with parameters: p=0.45

# S3 print child node, no params

    A DAG.node object specifying a single child node with:
      - name: 'A'
      - type: 'gaussian'
      - parents: 'C', 'D'
      - no additional parameters

# S3 print child node, with params

    A DAG.node object specifying a single child node with:
      - name: 'A'
      - type: 'gaussian'
      - parents: 'C', 'D'
      - betas: 1, 2
      - intercept: 15
      - with additional parameters: error

# S3 summary list of nodes

    A DAG.node object using the following structural equation(s):
    
    A ~ Bernoulli()
    B ~ Bernoulli()

# S3 summary root node, no params

    A DAG.node object using the following structural equation(s):
    
    A ~ Bernoulli()

# S3 summary root node, with params

    A DAG.node object using the following structural equation(s):
    
    A ~ Bernoulli(0.45)

# S3 summary child node, no params

    A DAG.node object using the following structural equation(s):
    
    A ~ N()

# S3 summary child node, with params

    A DAG.node object using the following structural equation(s):
    
    A ~ N(15 + 1*C + 2*D, 4)

