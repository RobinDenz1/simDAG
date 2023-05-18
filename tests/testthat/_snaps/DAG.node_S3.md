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
      - with additional parameters: 'error'

# S3 summary

    A DAG.node object specifying a single root node with:
      - name: 'A'
      - type: 'rbernoulli'
      - no additional parameters

