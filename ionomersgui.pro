PRO ionomersGui_event, event
  
  widgetName = Widget_Info(event.id, /UNAME)
  
  CASE widgetName OF 
  
  'PropSheet'     : BEGIN
                       IF (event.type EQ 0) THEN BEGIN ; Value changed. 
                       
                        ; Get the value of the property identified by 
                        ; event.identifier. 
                        value = Widget_Info(event.ID, COMPONENT = event.component, PROPERTY_VALUE = event.identifier) 
                       
                        ; Set the component's property value. 
                        event.component->SetPropertyByIdentifier, event.identifier, value 
                       
                       ENDIF ELSE BEGIN ; Selection changed. 
                       
                       ENDELSE
                       
                       Widget_Control, event.id, PROPERTYSHEET_SETSELECTED=event.identifier
                       Widget_Control, event.id, /REFRESH_PROPERTY
                     END
  'BASE'           : BEGIN
                       Widget_Control, Widget_Info(event.top, FIND_BY_UNAME = 'PropSheet'), SCR_XSIZE = event.x, SCR_YSIZE = event.y
                     END
  'SAVE'           : BEGIN
                       Widget_Control, event.top, GET_UVALUE = ionomerModel
                       data = ionomerModel.GetData()
                       file = Dialog_Pickfile(/OVERWRITE_PROMPT)
                       IF file EQ '' THEN RETURN
                       OpenW, fileLUN, file, /GET_LUN
                         PrintF, fileLUN, data
                       Free_LUN, fileLUN
                     END
  'LOAD'           : BEGIN
                        Widget_Control, event.top, GET_UVALUE = ionomerModel
                        file = Dialog_Pickfile(GET_PATH=path)
                        CD, path
                        lines = File_Lines(file)
                        expData = DblArr(3,lines)
                        temp = ''
                        i = 0
                        OpenR, fileLUN, file, /GET_LUN
                          While NOT EOF(fileLUN) DO BEGIN
                            ReadF, fileLUN, temp
                            expData[*,i] = StrSplit(temp,' ',/EXTRACT)
                            i++
                          ENDWHILE
                        Free_LUN, fileLUN
                        ionomerModel.SetExpData, expData[0:1,*]
                     END
   'FIT' : BEGIN
             Widget_Control, event.top, GET_UVALUE = ionomerModel
             ionomerModel.fit
             Widget_Control, Widget_Info(event.top, FIND_BY_UNAME = 'PropSheet'), /REFRESH_PROPERTY
           END
    'BATCH' : BEGIN
                Widget_Control, event.top, GET_UVALUE = ionomerModel
                files = Dialog_Pickfile(GET_PATH=PATH,/MULTIPLE)
                CD, path
                
                IF files[0] EQ '' THEN RETURN
                ionomerModel.GetProperty, SCALE=startscale, Vp=startVp, R1=startR1, Rca=startRca, d1=startd1, d2=startd2, Vca=startVca, V1=startV1, QMIN=startQmin, QMAX=startQmax
                data = List()
                data.add, ['Filename', 'Scale', 'Vp', 'R1', 'Rca', 'd1', 'd2', 'Vca', 'V1', 'QMIN', 'QMAX']
                FOREACH file, files DO BEGIN
                  ionomerModel.SetProperty, SCALE=startscale, Vp=startVp, R1=startR1, Rca=startRca, d1=startd1, d2=startd2, Vca=startVca, V1=startV1, QMIN=startQmin, QMAX=startQmax, /NOPLOT
                  lines = File_Lines(file)
                  expData = DblArr(3,lines)
                  temp = ''
                  i = 0
                  OpenR, fileLUN, file, /GET_LUN
                    While NOT EOF(fileLUN) DO BEGIN
                      ReadF, fileLUN, temp
                      expData[*,i] = StrSplit(temp,' ',/EXTRACT)
                      i++
                    ENDWHILE
                  Free_LUN, fileLUN
                  ionomerModel.SetExpData, expData[0:1,*]
                  ionomerModel.fit
                  Widget_Control, Widget_Info(event.top, FIND_BY_UNAME = 'PropSheet'), /REFRESH_PROPERTY
                  ionomerModel.GetProperty, SCALE=scale, Vp=Vp, R1=R1, Rca=Rca, d1=d1, d2=d2, Vca=Vca, V1=V1, QMIN=Qmin, QMAX=Qmax 
                  data.add, [file_basename(file),String([Scale, Vp, R1, Rca, d1, d2, Vca, V1, QMIN, QMAX])]
                ENDFOREACH
                saveFile = Dialog_Pickfile(TITLE = 'Output file', /WRITE)
        
                IF saveFile EQ '' THEN RETURN
                  OpenW, fileLUN, saveFile, /GET_LUN
                    FOREACH datum, data DO BEGIN
                      PrintF, fileLUN, StrJoin(datum,',')
                    ENDFOREACH
                  Free_LUN, fileLUN
                END
  ENDCASE

END

PRO ionomersGui_Cleanup, base

  Widget_Control, base, GET_UVALUE = ionomersModel
  ionomersModel.Cleanup

END

PRO ionomersGui

  ionomersModel = ionomers()
  ionomersModel.calculate
  
  IF Obj_Valid(ionomersModel) THEN BEGIN
  
    wBase = Widget_Base(/COLUMN, UNAME = 'BASE', /TLB_SIZE_EVENTS)
    wProp = Widget_PropertySheet(wBase, GROUP_LEADER = wBase, XSIZE = 50, YSIZE = 15, VALUE = ionomersModel, UNAME = 'PropSheet') 
    wFit = Widget_Button(wBase, VALUE = 'Fit', UNAME = 'FIT')
    wFit = Widget_Button(wBase, VALUE = 'Batch Fit', UNAME = 'BATCH')
    wOpen = Widget_Button(wBase, VALUE = 'Load Experimental Data', UNAME = 'LOAD')
    wSave = Widget_Button(wBase, VALUE = 'Save Data', UNAME = 'SAVE')
    Widget_Control, wBase, /REALIZE
    Widget_Control, wBase, SET_UVALUE = ionomersModel
    XManager, 'ionomersGui', wBase, CLEANUP = 'ionomersGui_Cleanup', /NO_BLOCK
    
  ENDIF

END