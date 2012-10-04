FUNCTION ionomermodelphi, x
  
  RETURN, 3*(sin(x) - x*cos(x))/x^3
  
END

PRO ionomermodel, q, a, f

  scale = a[0]
  d1 = a[1]
  d2 = a[2]
  R1 = a[3]
  Rca = a[4]
  vp = a[5]
  
  V1 = 4*!dpi*R1^3/3d  
  Vca = 4*!dpi*Rca^3/3d

  f = scale*(V1*(d1-d2)*ionomermodelphi(q*R1)+Vca*d2*ionomermodelphi(q*Rca))^2/(1+(8*Vca/vp)*ionomermodelphi(2*q*Rca))

END