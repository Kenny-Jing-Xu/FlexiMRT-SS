getTrueBeta=function(q1, q2, q3, days, occ_per_day, dayaa, beta_shape) 
{
  if (beta_shape == "quadratic") 
  {bet = matrix( 0, 3 )
  a = sum( c( ( ( dayaa - 1 )*occ_per_day ) : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day )
  b = sum( ( c( ( ( dayaa - 1 )*occ_per_day ) : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day )^2 )
  mat = rbind( c( days*occ_per_day - ( dayaa - 1 )*occ_per_day, a, b ), 
               c( 0, 1/occ_per_day, ( 2/occ_per_day )*( ( ( q3 - 1 )*occ_per_day + occ_per_day - 1 )/occ_per_day ) ), 
               c( 1, dayaa-1, (dayaa-1)^2 ) )
  bet = solve( mat ) %*% matrix( c( q2*( days*occ_per_day - ( dayaa - 1 )*occ_per_day ), 0, q1 ) )
  }
  else if (beta_shape == "constant") 
  { bet = matrix( 0, 1 )
  mat = rbind( c( 1 ) )
  bet = solve( mat ) %*% matrix( c( q2 ) ) 
  }
  else if (beta_shape == "linear") 
  { bet = matrix( 0, 2 )
  a = sum( c( ( ( dayaa - 1 )*occ_per_day ) : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day )
  mat = rbind( c( days*occ_per_day - ( dayaa - 1 )*occ_per_day, a ), c( 1, dayaa-1 ) )
  bet = solve( mat ) %*% matrix( c( q2*( days*occ_per_day - ( dayaa - 1 )*occ_per_day ), q1 ) )
  }
  else if (beta_shape == "linear and constant")
  {
    bet = matrix( 0, 2 )
    d = rep( c( dayaa:days ), each = occ_per_day )
    dt = c( ( ( dayaa - 1 ) * occ_per_day ) : ( ( days - 1 ) * occ_per_day + occ_per_day - 1 ) )
    dt[ d > q3 ] = ( ( q3 - 1 ) * occ_per_day + occ_per_day - 1 )
    a = sum( dt / occ_per_day )
    mat = rbind( c( days * occ_per_day - ( dayaa - 1 )*occ_per_day, a ), c( 1, dayaa - 1 ) )
    bet = solve( mat ) %*% matrix( c( q2 * ( days * occ_per_day - ( dayaa - 1 ) * occ_per_day ), q1 ) )
  }
  return(bet)
}

generateBeta=function(days, occ_per_day, dayaa, beta_shape, beta_mean, beta_initial, beta_quadratic_max) 
{
  s = c( 0 : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day
  beta_input = rep( 0, days*occ_per_day )
  
  if (beta_shape == "quadratic") 
  {
    beta1=getTrueBeta(q1=beta_initial, q2=beta_mean, q3=beta_quadratic_max, days=days, occ_per_day = occ_per_day, dayaa=dayaa, beta_shape="quadratic")
    bet1 = beta1[1]
    bet2 <- beta1[2]
    bet3 <- beta1[3]
    beta_input[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days*occ_per_day ) ] = bet1 + bet2 * s[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days*occ_per_day ) ] + 
      bet3 * s[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days*occ_per_day ) ]^2
  }
  else if (beta_shape == "constant") 
  { 
    beta1=getTrueBeta(q1=beta_initial, q2=beta_mean, q3=beta_quadratic_max, days=days, occ_per_day = occ_per_day, dayaa=dayaa, beta_shape="constant")
    beta_input[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days*occ_per_day ) ] <- beta1 
  }
  else if (beta_shape == "linear") 
  {
    beta1=getTrueBeta(q1=beta_initial, q2=beta_mean, q3=beta_quadratic_max, days=days, occ_per_day = occ_per_day, dayaa=dayaa, beta_shape="linear")
    bet1 = beta1[1]
    bet2 = beta1[2]
    beta_input[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days*occ_per_day ) ] = bet1 + bet2 * s[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days*occ_per_day ) ] 
  }
  else if (beta_shape == "linear and constant")
  {
    beta1=getTrueBeta(q1=beta_initial, q2=beta_mean, q3=beta_quadratic_max, days=days, occ_per_day = occ_per_day, dayaa=dayaa, beta_shape="linear and constant")
    bet1 = beta1[1]
    bet2 = beta1[2]
    d = rep( c( 1 : days ), each = occ_per_day )
    s[d > beta_quadratic_max] = ( ( beta_quadratic_max - 1 ) * occ_per_day + occ_per_day - 1 ) / occ_per_day
    beta_input[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days * occ_per_day ) ] = bet1 + bet2*s[ ( ( dayaa - 1 )*occ_per_day + 1 ) : ( days * occ_per_day ) ] 
  }
  return(list(beta_input = beta_input, beta1 = beta1))
}

generateTau=function(days, occ_per_day, tau_shape, tau_mean, tau_initial, tau_quadratic_max) 
{
  if (tau_shape == "quadratic") 
  {
    beta1 <- getTrueBeta(q1 = tau_initial, q2 = tau_mean, q3 = tau_quadratic_max, days = days, occ_per_day = occ_per_day, dayaa = 1, beta_shape="quadratic")
    ta1 = beta1[1]
    ta2 <- beta1[2]
    ta3 <- beta1[3]
    s = c( 0 : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day
    tau_input <- ta1 + ta2 * s + ta3 * s^2
  }
  else if (tau_shape == "constant") 
  {
    tau_input <- replicate(days*occ_per_day, tau_mean)
  }
  else if (tau_shape == "linear") 
  {
    beta1=getTrueBeta(q1=tau_initial, q2=tau_mean, q3=tau_quadratic_max, days=days, occ_per_day = occ_per_day, dayaa=1, beta_shape="linear")
    ta1 = beta1[1]
    ta2 = beta1[2]
    s = c( 0 : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day
    tau_input <- ta1 + ta2 * s 
  }
  return(tau_input)
}

para=function(ord, Total, days, occ_per_day) 
{
  if (ord == 0) 
  {
    return( matrix( 1, Total, 1 ) )
  }
  else 
  {
    s = c( 0 : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day
    Z = matrix( 0, Total, ord + 1 )
    Z[  , 1 ] = 1 
    for ( i in 1:ord ) 
    {
      Z[ , i + 1 ] = s^i
      #( rep( c( 0:( days - 1 ) ), each = occ_per_day ) )^i 
    }
  }
  return(Z)
}

PowerCal=function( pb, pa, N, d, bet, alpha, test ) 
{
  M=length(d)/pb
  df1= M*pb
  if(test=="chi")
  {
    C = N * t( d ) %*% bet %*% d
    adj_c = qchisq( 1 - alpha, df = df1 )
    pow = 1 - pchisq( q = adj_c, df = df1, ncp = C )
    adj_c2 <- ( N )*t( d )%*%bet%*%d
    cp <- pchisq( q = adj_c2, df = df1, ncp = 0 )
  }
  else if (test=="hotelling N")
  {
    C = N * t( d ) %*% bet %*% d
    df2 = ( N - 0 - 0 ) - M*pb + 1 
    adj_c = qf( 1 - alpha, df1 = df1, df2 = df2 )
    pow = 1 - pf( q = adj_c, df1 = df1, df2 = df2, ncp = C )
    adj_c2 <- ( ( N - M*pb + 1 )/( M*pb*( N ) ) )*( N )*t( d )%*%bet%*%d
    cp <- pf( q = adj_c2, df1 = df1, df2 = df2, ncp = 0 )
  }
  else if(test=="hotelling N-1")
  {
    C = ( N ) * t( d ) %*% bet %*% d
    df2 = ( N - 0 - 1 ) - M*pb + 1 
    adj_c = qf( 1 - alpha, df1 = df1, df2 = df2 )
    pow = 1 - pf( q = adj_c, df1 = df1, df2 = df2, ncp = C )
    adj_c2 <- ( ( N - 1 - M*pb + 1 )/( M*pb*( N - 1 ) ) )*( N )*t( d )%*%bet%*%d
    cp <- pf( q = adj_c2, df1 = df1, df2 = df2, ncp = 0 )
  }
  else if(test=="hotelling N-q-1")
  {
    C = ( N ) * t( d ) %*% bet %*% d
    df2 = ( N - pa - 1 ) - M*pb + 1 
    adj_c = qf( 1 - alpha, df1 = df1, df2 = df2 )
    pow = 1 - pf( q = adj_c, df1 = df1, df2 = df2, ncp = C )
    adj_c2 <- ( ( N - pa - 1 - M*pb + 1 )/( M*pb*( N - pa - 1 ) ) )*( N )*t( d )%*%bet%*%d
    cp <- pf( q = adj_c2, df1 = df1, df2 = df2, ncp = 0 )
  }
  return( list( pow = pow, cp = cp, adj_c = adj_c, adj_c2 = adj_c2 ) )
}

SampleSizeAA=function(days, occ_per_day, 
                      beta_shape, beta_quadratic_max, 
                      beta1, beta_t, 
                      tau, 
                      prob_t, 
                      alpha0, beta0, 
                      pb, pa, 
                      sigma, 
                      Nmax = 1000, 
                      method, 
                      test, 
                      result, SS) 
{
  Total <- days * occ_per_day
  d = rep( c( 1:days ), each = occ_per_day )
  #s = c( 0 : ( ( days - 1 )*occ_per_day+occ_per_day-1 ) )/occ_per_day
  Zt = para( ord = pb - 1, Total, days, occ_per_day )
  EXiXj=matrix( 0, pb*( dim( prob_t )[2] - 1 ), pb*( dim( prob_t )[2] - 1 ) )
  for( i in 1:( dim( prob_t )[2] - 1 ) )
  {
    if ( beta_shape == "linear and constant" ) 
    { 
      Zti = Zt
      Zti[ d > beta_quadratic_max[i], 2 ] = ( ( beta_quadratic_max[i] - 1 ) * occ_per_day + occ_per_day - 1 )/occ_per_day
    } 
    else{ 
      Zti = Zt 
    }
    for( j in 1:( dim( prob_t )[2] - 1 ) )
    {
      if ( beta_shape == "linear and constant" )
      { 
        Ztj = Zt
        Ztj[ d > beta_quadratic_max[j], 2 ] = ( ( beta_quadratic_max[j] - 1 ) * occ_per_day + occ_per_day - 1 )/occ_per_day
      } 
      else
      { 
        Ztj = Zt 
      }
      if(i==j)
      { 
        EXiXj[ ( ( i - 1 )*pb + 1 ):( pb*i ), ( ( j - 1 )*pb + 1 ):( pb*j ) ]=( t( ( tau*prob_t[ , i + 1 ]*( 1 - prob_t[ , j + 1 ] ) )%*%matrix( 1, 1, pb ) )*t( Zti ) )%*%( Ztj )  
      }
      else
      { 
        EXiXj[ ( ( i - 1 )*pb + 1 ):( pb*i ), ( ( j - 1 )*pb + 1 ):( pb*j ) ]=( t( ( tau*( -prob_t[ , i + 1 ] )*( prob_t[ , j + 1 ] ) )%*%matrix( 1, 1, pb ) )*t( Zti ) )%*%( Ztj ) 
      }
    }
  }
  
  EXiXjbeta=matrix( 0, pb*( dim( prob_t )[2] - 1), 1*( dim( prob_t )[2] - 1 ) )
  for( i in 1:( dim( prob_t )[2] - 1 ) )
  {
    if ( beta_shape == "linear and constant" )
    { 
      Zti = Zt
      Zti[ d > beta_quadratic_max[i], 2 ] = ( ( beta_quadratic_max[i] - 1 ) * occ_per_day + occ_per_day - 1 )/occ_per_day
    } 
    else 
    { 
      Zti = Zt 
    }
    for( j in 1:( dim( prob_t )[2] - 1 ) )
    {
      if(i==j)
      { 
        EXiXjbeta[ ( ( i - 1 )*pb + 1 ):( pb*i ), ( ( j - 1 )*1 + 1 ):( 1*j ) ] = t( Zti )%*%( tau* prob_t[ , i + 1 ]*(1 - prob_t[ , j + 1 ] )*beta_t[ , j ] ) 
      }
      else
      { 
        EXiXjbeta[ ( ( i - 1 )*pb + 1 ):( pb*i ), ( ( j - 1 )*1 + 1 ):( 1*j ) ] = t( Zti )%*%( tau*( -prob_t[ , i + 1 ] )*( prob_t[ , j + 1 ] )*beta_t[ , j ] )
      }
    }
  }
  
  Sig_bet_inv=( sigma^2 )^(-1)*( EXiXj )
  sumEXiXjbeta = rowSums(EXiXjbeta)
  d=solve( EXiXj ) %*% rowSums(EXiXjbeta)
  beta1=matrix(beta1, pb*( dim( prob_t )[2] - 1 ),1)
  
  if(method=="power")
  {
    if( result == "choice_sample_size" )
    {
      M=dim( prob_t )[2] - 1
      
      if(test=="chi")
      {
        Nmin = 1 + pb*M
      }
      else{
        Nmin = 1 + pa + pb*M
      }
      
      N.all <- c( ( Nmin ):Nmax)
      for (k in N.all) {
        pow <- PowerCal(pb, pa, k, beta1, Sig_bet_inv, alpha0, test)$pow
        if (pow >= beta0) {
          N <- k
          break
        }
      }
      if (N == Nmax) {
        stop(paste("Cannot attain", beta0, "power when sample size is below", 
                   Nmax))
      }
      P <- beta0  
      CP <- 1 - alpha0
    }
    else if(result == "choice_power")
    {
      N <- SS
      P <- PowerCal( pb=pb, pa=pb, N=N, d=beta1, bet=Sig_bet_inv, alpha=alpha0, test = test )$pow 
      CP <- 1 - alpha0
    }
    else{ stop( paste( "Define the correct result, i.e. sample size calculation or power calculation" ) ) }
  }
  else if(method=="confidence interval")
  {
    if( result == "choice_sample_size" )
    {
      M=dim( prob_t )[2] - 1
      B=t(beta1) %*% Sig_bet_inv %*% beta1  
      
      if(test=="chi")
      {
        Nmin = 1 + pb*M
      }
      else{
        Nmin = 1 + pa + pb*M
      }
      
      N.all <- c( ( Nmin ):Nmax )
      for (k in N.all) {
        adj_c <- PowerCal(pb, pa, k, beta1, Sig_bet_inv, alpha0, test)$adj_c
        
        if(test=="chi")
        {
          B_crit = ( 1 )/( k )*adj_c
        }
        else if(test=="hotelling N")
        {
          B_crit = ( ( pb*M*( k - 0 - 0 ) )/( k*(  ( k - 0 - 0 ) - pb*M  + 1 ) ) )*adj_c
        }
        else if(test=="hotelling N-1")
        {
          B_crit = ( ( pb*M*( k - 0 - 1 ) )/( k*(  ( k - 0 - 1 ) - pb*M  + 1 ) ) )*adj_c
        }
        else if(test=="hotelling N-q-1")
        {
          B_crit = ( ( pb*M*( k - pa - 1 ) )/( k*(  ( k - pa - 1 ) - pb*M  + 1 ) ) )*adj_c
        }
        
        if ( B >= B_crit ) {
          N <- k
          break
        }
      }
      if (N == Nmax) {
        stop(paste("Cannot attain", (1-alpha0), "confidence interval when sample size is below", Nmax))
      }
      P <- beta0
      CP <- 1 - alpha0
    }
    else if( result == "choice_coverage_probability" )
    {
      N <- SS
      P <- beta0
      CP <- PowerCal( pb=pb, pa=pb, N=N, d=beta1, bet=Sig_bet_inv, alpha=alpha0, test = test )$cp 
    }
    else{ stop( paste( "Define the correct result, i.e. sample size calculation or coverage probability calculation" ) ) }
  }    
  else{stop(paste("Define the method, power or confidence interval"))}
  return( 
    list( 
      N = N, P = P, CP = CP, 
      d = d, 
      Sig_bet_inv = Sig_bet_inv, 
      sumEXiXjbeta = sumEXiXjbeta 
      ) 
    )
}

SampleSize_MLMRT=function(days, occ_per_day, 
                          aa.day.aa, 
                          prob, 
                          beta_shape, beta_mean, beta_initial, beta_quadratic_max, 
                          tau_shape, tau_mean, tau_initial, tau_quadratic_max, 
                          sigma, 
                          pow, sigLev, 
                          method, test, 
                          result, SS) 
{
  if(pow < 0){stop("Error: Please specify the power greater than or equal to 0")}
  if(pow > 1){stop("Error: Error: Please specify the power less than or equal to 1")}
  beta_shape <- tolower(beta_shape)
  tau_shape <- tolower(tau_shape)
  
  if( beta_shape == "constant" ){ p_input = 1 }
  else if( beta_shape == "linear" ){ p_input = 2 }
  else if( beta_shape == "linear and constant" ){ p_input = 2 }
  else if( beta_shape == "quadratic" ){ p_input = 3 }
  
  beta_input=matrix(0, days*occ_per_day, dim(prob)[2]-1)
  beta1=matrix(0, p_input, dim(prob)[2]-1)
  
  for(j in 1:( dim( prob )[2] - 1 ) )
  {
    genbeta=generateBeta( days=days, occ_per_day = occ_per_day, dayaa=aa.day.aa[j], beta_shape, beta_mean[j], beta_initial[j], ( beta_quadratic_max )[j] )
    beta_input[,j]=genbeta$beta_input
    beta1[,j]=genbeta$beta1
  }
  
  tau_input = generateTau( days = days, occ_per_day = occ_per_day, tau_shape = tau_shape, tau_mean = tau_mean, tau_initial = tau_initial, tau_quadratic_max = tau_quadratic_max )
  
  total = days * occ_per_day
  input_avail = tau_input #matrix( 0, total, 1 )
  input_effect = beta_input #matrix( 0, total, dim( prob )[2] - 1 ) 
  prob_t = matrix( 0, total, dim( prob )[2] - 0 )
  for( j in 1:( dim( prob )[2] - 1 ) )
  {
    for( k in 1:days )
    {
      prob_t[ , 1 ] = replicate( occ_per_day, prob[ k, 1 ] )
      prob_t[ ( occ_per_day * ( k - 1 ) + 1 ):( occ_per_day * k ), ( j + 1) ] = replicate( occ_per_day, prob[ k, ( j + 1) ] ) 
    }
  }
  
  MRTN <- SampleSizeAA(days = days, occ_per_day = occ_per_day, 
                       beta_shape = beta_shape, beta_quadratic_max = beta_quadratic_max,
                       beta1 = beta1, beta_t = input_effect, tau = input_avail, prob_t = prob_t, alpha0 = sigLev, beta0 = pow, pb = p_input, pa = p_input, sigma = sigma, Nmax = 10000, 
                       method = method, test = test, 
                       result = result, SS = SS) 
  N <- MRTN$N
  P <- MRTN$P
  CP <- MRTN$CP
  d <- MRTN$d
  Sig_bet_inv <- MRTN$Sig_bet_inv
  #sumEXiXjbeta=MRTN$sumEXiXjbeta
  
  return(list(N=N, P = P, CP = CP,  
              d=d, Sig_bet_inv=Sig_bet_inv, 
              #sumEXiXjbeta=sumEXiXjbeta, 
              beta1=beta1))
}

