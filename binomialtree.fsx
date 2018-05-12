// s: stock price
// k: strike price of option
// t: time to expiration in years
// r: risk free interest rate
// v: volatility
// q: divident yield
// n: height of binomial tree

let binomialtree s k t r v q n =
    let deltaT = t/float n
    let up = exp(v*sqrt(deltaT))
    let p0 = (up*exp(-r*deltaT)-exp(-q*deltaT))*up/((up*up)-1.0)
    let p1 = exp(-r*deltaT)-p0
    let value = Array.zeroCreate(n+1)

    for i = 0 to n do
      value.[i] <- (k-s) * (up**(float(2*i-n)))
      if (value.[i] >= 0.0) then value.[i] <- 0.0
    
    for j = n - 1 downto 0 do
      for i = 0 to j do
        value.[i] <- p0*value.[i]+p1*value.[i+1] // binomial value
        let exercise = k-s*(up**float(2*i-j))  // exercise value
        if value.[i] < exercise then value.[i] <- exercise
    
    value.[0]

// Example usage:
binomialtree 60.0 65.0 0.25 0.08 0.3 0.0 100;;