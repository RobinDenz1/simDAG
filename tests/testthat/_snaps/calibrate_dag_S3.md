# standard calibrated DAG output

    Code
      print(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha     0.25   0.0    -2     2        2
            beta    -0.10   0.5  -Inf   Inf        3
      
      Targets (2)
           target desired achieved  error scaled_error
       prevalence     0.4    0.401  0.001         0.01
           mean_y     1.0    0.980 -0.020        -0.02
      
      Optimization
        Method:       L-BFGS-B
        Convergence:  successful
        Objective:    0.123
        Evaluations:  function 12, gradient 4
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────
      
      Calibrated DAG object
      ----------------------------------------
        Parameters: 2
        Targets:    2
        Method:     L-BFGS-B (converged)
        Max |scaled target error|: 0.02
        Simulations: 10,000 (optimization); 1e+05 (final evaluation)
      
      Use summary(x) for parameter estimates, target results, and optimization details.
    Code
      summary(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha     0.25   0.0    -2     2        2
            beta    -0.10   0.5  -Inf   Inf        3
      
      Targets (2)
           target desired achieved  error scaled_error
       prevalence     0.4    0.401  0.001         0.01
           mean_y     1.0    0.980 -0.020        -0.02
      
      Optimization
        Method:       L-BFGS-B
        Convergence:  successful
        Objective:    0.123
        Evaluations:  function 12, gradient 4
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────

# unsuccessful optimization output

    Code
      print(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha     0.25   0.0    -2     2        2
            beta    -0.10   0.5  -Inf   Inf        3
      
      Targets (2)
           target desired achieved  error scaled_error
       prevalence     0.4    0.401  0.001         0.01
           mean_y     1.0    0.980 -0.020        -0.02
      
      Optimization
        Method:       L-BFGS-B
        Convergence:  unsuccessful (code 1)
        Objective:    0.123
        Message:      iteration limit reached
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────
      
      Calibrated DAG object
      ----------------------------------------
        Parameters: 2
        Targets:    2
        Method:     L-BFGS-B (not converged (code 1))
        Max |scaled target error|: 0.02
        Simulations: 10,000 (optimization); 1e+05 (final evaluation)
      
      Use summary(x) for parameter estimates, target results, and optimization details.
    Code
      summary(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha     0.25   0.0    -2     2        2
            beta    -0.10   0.5  -Inf   Inf        3
      
      Targets (2)
           target desired achieved  error scaled_error
       prevalence     0.4    0.401  0.001         0.01
           mean_y     1.0    0.980 -0.020        -0.02
      
      Optimization
        Method:       L-BFGS-B
        Convergence:  unsuccessful (code 1)
        Objective:    0.123
        Message:      iteration limit reached
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────

# root-finding output

    Code
      print(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (1)
       parameter estimate start lower upper parscale
              mu     0.25     0    -2     2       NA
      
      Targets (1)
       target desired achieved error scaled_error
       mean_x    0.25    0.251 0.001         0.01
      
      Optimization
        Method:       uniroot
        Convergence:  successful
        Root residual: 0.001
        Iterations:   7
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────
      
      Calibrated DAG object
      ----------------------------------------
        Parameters: 1
        Targets:    1
        Method:     uniroot (converged)
        Max |scaled target error|: 0.01
        Simulations: 10,000 (optimization); 1e+05 (final evaluation)
      
      Use summary(x) for parameter estimates, target results, and optimization details.
    Code
      summary(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (1)
       parameter estimate start lower upper parscale
              mu     0.25     0    -2     2       NA
      
      Targets (1)
       target desired achieved error scaled_error
       mean_x    0.25    0.251 0.001         0.01
      
      Optimization
        Method:       uniroot
        Convergence:  successful
        Root residual: 0.001
        Iterations:   7
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────

# fallback and non-finite output

    Code
      print(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha    -0.10   0.0    -2     2       NA
            beta     0.25   0.5  -Inf   Inf       NA
      
      Targets (2)
         target desired achieved error scaled_error
       Target 1     0.4       NA    NA           NA
       Target 2     1.0      Inf   Inf          Inf
      
      Optimization
        Method:       L-BFGS-B
        Convergence:  successful
        Objective:    0.123
        Evaluations:  function 12, gradient 4
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────
      
      Calibrated DAG object
      ----------------------------------------
        Parameters: 2
        Targets:    2
        Method:     L-BFGS-B (converged)
        Max |scaled target error|: unavailable
        Simulations: 10,000 (optimization); 1e+05 (final evaluation)
      
      Use summary(x) for parameter estimates, target results, and optimization details.
    Code
      summary(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha    -0.10   0.0    -2     2       NA
            beta     0.25   0.5  -Inf   Inf       NA
      
      Targets (2)
         target desired achieved error scaled_error
       Target 1     0.4       NA    NA           NA
       Target 2     1.0      Inf   Inf          Inf
      
      Optimization
        Method:       L-BFGS-B
        Convergence:  successful
        Objective:    0.123
        Evaluations:  function 12, gradient 4
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────

# explicit optimization method and scalar evaluation count

    Code
      print(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha     0.25   0.0    -2     2       NA
            beta    -0.10   0.5  -Inf   Inf       NA
      
      Targets (2)
           target desired achieved  error scaled_error
       prevalence     0.4    0.401  0.001         0.01
           mean_y     1.0    0.980 -0.020        -0.02
      
      Optimization
        Method:       BFGS
        Convergence:  successful
        Objective:    0.123
        Iterations:   5
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────
      
      Calibrated DAG object
      ----------------------------------------
        Parameters: 2
        Targets:    2
        Method:     BFGS (converged)
        Max |scaled target error|: 0.02
        Simulations: 10,000 (optimization); 1e+05 (final evaluation)
      
      Use summary(x) for parameter estimates, target results, and optimization details.
    Code
      summary(x)
    Output
      ────────────────────────────────────────────────────────────
      Calibrated DAG
      ────────────────────────────────────────────────────────────
      Parameters (2)
       parameter estimate start lower upper parscale
           alpha     0.25   0.0    -2     2       NA
            beta    -0.10   0.5  -Inf   Inf       NA
      
      Targets (2)
           target desired achieved  error scaled_error
       prevalence     0.4    0.401  0.001         0.01
           mean_y     1.0    0.980 -0.020        -0.02
      
      Optimization
        Method:       BFGS
        Convergence:  successful
        Objective:    0.123
        Iterations:   5
        Simulations:  10,000 optimization; 1e+05 final evaluation
      ────────────────────────────────────────────────────────────

