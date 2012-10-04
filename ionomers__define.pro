FUNCTION ionomers::init

  self.name  = 'Ionomer Model'
  self.description = 'Plotting Ionomer model from Kutsumizu et al.'
  self.scale = 1d
  self.Vp    = 11300d
  self.R1    = 5.9d
  self.Rca   = 7.4d
  self.V1    = 4*!dpi*self.R1^3/3d
  self.Vca   = 4*!dpi*self.Rca^3/3d
  self.d1    = 0.054d
  self.d2    = -0.022d
  self.qmin  = 0.0001d
  self.qmax  = 0.7d
  self.points = 100
  
  result = self.IDLitComponent::INIT()

  self->RegisterProperty, 'Scale', 3
  self->RegisterProperty, 'Vp'   , 3, VALID_RANGE = [0d,50000d]
  self->RegisterProperty, 'R1'   , 3, VALID_RANGE = [0d,50d,0.1d]
  self->RegisterProperty, 'V1'   , 3, SENSITIVE = 0
  self->RegisterProperty, 'Rca'  , 3, VALID_RANGE = [0d,50d,0.1d]
  self->RegisterProperty, 'Vca'  , 3, SENSITIVE = 0
  self->RegisterProperty, 'd1'   , 3, VALID_RANGE = [-1d,1d,0.0001d]
  self->RegisterProperty, 'd2'   , 3, VALID_RANGE = [-1d,1d,0.0001d]
  self->RegisterProperty, 'qmin' , 3, VALID_RANGE = [0.0001d,2d]
  self->RegisterProperty, 'qmax' , 3, VALID_RANGE = [0.0001d,2d]
  self->RegisterProperty, 'XLog', 1
  self->RegisterProperty, 'YLog', 1
  self->RegisterProperty, 'Points', 2, VALID_RANGE=[2,1000]
  
  RETURN, 1

END

PRO ionomers::Cleanup

  IF Obj_Valid(self.plot) THEN self.plot.close
  self.IDLitComponent::Cleanup

END

FUNCTION ionomers::phi, x
  
  RETURN, 3*(sin(x) - x*cos(x))/x^3

END

FUNCTION ionomers::GetData

  self.calculate, DATA=data, /NOPLOT
  RETURN, data

END

PRO ionomers::SetExpData, expData

  IF Ptr_Valid(self.expData) THEN *self.expData = expData ELSE self.expData = Ptr_New(expData)
  If Obj_Valid(self.expPlot) THEN self.expPlot.Delete
  self.expPlot = plot(expData[0,*],expData[1,*],/OVERPLOT, COLOR = 'Red')

END

PRO ionomers::GetProperty, $
  SCALE = scale, $
  Vp    = Vp, $
  R1    = R1, $
  V1    = V1, $
  Rca   = Rca, $
  Vca   = Vca, $
  d1    = d1, $
  d2    = d2, $
  QMIN  = qmin, $
  QMAX  = qmax, $
  XLOG  = XLog, $
  YLOG  = YLog, $
  POINTS = points, $
  _Ref_Extra = extra

  IF Arg_Present(Scale) GE 1 THEN scale = self.scale
  IF Arg_Present(Vp)    GE 1 THEN Vp = self.Vp
  IF Arg_Present(R1)    GE 1 THEN R1 = self.R1
  IF Arg_Present(V1)    GE 1 THEN V1 = self.V1
  IF Arg_Present(Rca)   GE 1 THEN Rca = self.Rca
  IF Arg_Present(Vca)   GE 1 THEN Vca = self.Vca
  IF Arg_Present(d1)    GE 1 THEN d1 = self.d1
  IF Arg_Present(d2)    GE 1 THEN d2 = self.d2
  IF Arg_Present(qmin)  GE 1 THEN qmin = self.qmin
  IF Arg_Present(qmax)  GE 1 THEN qmax = self.qmax
  IF Arg_Present(XLog)  GE 1 THEN BEGIN
    IF Obj_Valid(self.plot) THEN self.plot.GetProperty, XLOG = XLog ELSE XLog = 0
  ENDIF
  IF Arg_Present(YLog)  GE 1 THEN BEGIN
    IF Obj_Valid(self.plot) THEN self.plot.GetProperty, YLOG = YLog ELSE YLog = 0
  ENDIF
  IF Arg_Present(points) GE 1 THEN points = self.points

  self.IDLitComponent::GetProperty, _EXTRA = extra

END

PRO ionomers::SetProperty, $
  SCALE = scale, $
  Vp    = Vp, $
  R1    = R1, $
  V1    = V1, $
  Rca   = Rca, $
  Vca   = Vca, $
  d1    = d1, $
  d2    = d2, $
  QMIN  = qmin, $
  QMAX  = qmax, $
  XLOG  = XLog, $
  YLOG  = YLog, $
  POINTS = points, $ 
  NOPLOT = noPlot, $
  _Ref_Extra = extra

  IF N_Elements(Scale) GE 1 THEN self.scale = scale
  IF N_Elements(Vp)    GE 1 THEN self.Vp = Vp
  IF N_Elements(R1)    GE 1 THEN BEGIN
    self.R1 = R1
    self.V1 = 4*!dpi*self.R1^3/3d
  ENDIF
  IF N_Elements(V1)    GE 1 THEN self.V1 = V1
  IF N_Elements(Rca)   GE 1 THEN BEGIN
    self.Rca = Rca
    self.Vca = 4*!dpi*self.Rca^3/3d
  ENDIF
  IF N_Elements(Vca)   GE 1 THEN self.Vca = Vca
  IF N_Elements(d1)    GE 1 THEN self.d1 = d1
  IF N_Elements(d2)    GE 1 THEN self.d2 = d2
  IF N_Elements(qmin)  GE 1 THEN self.qmin = qmin
  IF N_Elements(qmax)  GE 1 THEN self.qmax = qmax
  IF N_Elements(XLog)  GE 1 THEN self.xlog = xlog
  IF N_Elements(YLog)  GE 1 THEN self.ylog = yLog
  IF N_Elements(points)GE 1 THEN self.points = points
  
  self.IDLitComponent::SetProperty, _EXTRA = extra

  IF ~KeyWord_Set(noPlot) THEN self.calculate

END

PRO ionomers::Fit

  expQ = (*self.expData)[0,*]
  expY = (*self.expData)[1,*]
  expRange = Where(expQ GE self.qmin AND expQ LE self.qmax)

  q = expQ[expRange]
  y = expY[expRange]

  a = [self.scale,self.d1,self.d2,self.R1,self.Rca,self.vp]
  a_copy = a

  blah = curvefit(q,y,1/y,a, ITMAX = 100, FUNCTION_NAME='ionomermodel', /NODERIVATIVE, STATUS=status)
  
  print, status
  
  IF status NE 1 THEN self.SetProperty, scale = a[0], d1 = a[1], d2 = a[2], R1 = a[3], Rca = a[4], vp = a[5]
  
END

PRO ionomers::Calculate, DATA=data, NOPLOT=noPlot

  q = Double(self.qmin) + (DIndgen(self.points)/Double(self.points - 1)) * Double(self.qmax-self.qmin)
  ;I = self.scale*(self.V1*(self.d1-self.d2)*self.phi(q*self.R1)+self.Vca*self.d2*self.phi(q*self.Rca))^2/(1+(8*self.Vca/self.vp)*self.phi(2*q*self.Rca))

  ionomermodel, q, [self.scale,self.d1,self.d2,self.R1,self.Rca,self.vp], I

  IF Arg_Present(data) THEN data = Transpose([[q],[I]])

  IF ~KeyWord_Set(noPlot) THEN BEGIN
    IF ~Obj_Valid(self.plot) THEN BEGIN
      self.plot = Plot(q,I, XLOG = self.xlog, YLOG=self.ylog, COLOR = 'Blue')
    ENDIF ELSE BEGIN
      self.plot.Refresh, /DISABLE
      self.plot.SetProperty, XLOG = 0, YLOG = 0
      self.plot.PutData, Transpose([[q],[I]])
      self.plot.SetProperty, XLOG = self.XLog, YLOG = self.YLog
      self.plot.Refresh, DISABLE = 0
    ENDELSE
  ENDIF

END

PRO ionomers__define

  void = { ionomers, $
           INHERITS IDLitComponent, $
           plot   : Obj_New(), $
           scale  : 0d, $
           Vp     : 0d, $
           R1     : 0d, $
           V1     : 0d, $
           Rca    : 0d, $
           Vca    : 0d, $
           d1     : 0d, $
           d2     : 0d, $
           qmin   : 0d, $
           qmax   : 0d, $
           XLog   : 0,  $
           YLog   : 0,  $
           points : 0,  $
           expData: Ptr_New(), $
           expPlot: Obj_New() }
  
END