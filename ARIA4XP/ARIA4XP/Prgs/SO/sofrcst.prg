*:************************************************************************
*: Program file  : SOFRCST.PRG
*: Program desc. : Style Forecast.   (128616)
*: For screen    : SOFRCST.SCX
*: System        : ARIA4
*: Module        : Sales Order Module
*: Developer     : Hend Ghanem (HBG)
*:*************************************************************
#INCLUDE R:\ARIA4XP\SCREENS\SO\SOFRCST.H

DO FORM (oAriaApplication.ScreenHome+"\SO\SOFRCST.SCX")
RETURN


DEFINE CLASS PRG AS Custom
  
  *-- Declear all prperties of business class
  loFormSet = .F.
  loForm = .F.
  llGetData = .F.
  lnWeek = 0
  lnYear = 0
  lnOldYear = ""
  lnOldWeek = ""
  lnOldTotQt = 0         &&  Define lnOldTotQt  
  lcForCstDtl =""
  lcForCstHdr =""
  
  *!*************************************************************
  *! Name      : lpInit
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : To be called from the Init Event of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : True / Flase
  *!*************************************************************
  FUNCTION lfInit
  LPARAMETERS loFrm

  SET MULTILOCKS ON
  This.lcForCstDtl = gfTempName()
  This.lcForCstHdr = gfTempName()

  This.loForm    = loFrm
  This.loFormSet = loFrm.Parent
  This.lfopenfile('FORCASTH','FORCASTH')
  This.lfopenfile('FORCAST','FORCASTW')
  This.lfopenfile('STYLE','STYLE')
  This.lfopenfile('SCALE','SCALE')  
  
  This.lfCrtFile()
  
  WITH This.loFormSet
    .DataEnvironment.InitialSelectedAlias = 'FORCASTH'
    .cbrowsetabledbengine   = 'SQL'
    .cBrowseFileName        = "FORCASTH"
    .cBrowseIndexExpression = "STR(nyear,4)+STR(nweek,2)"
    .cBrowseIndexFields     = "nyear,nweek"
    .cBrowseIndexName       = "FORCASTH"
    .cBrowseAliasName       = "FORCASTH"
    .cBrowseTableName       = "FORCASTH"
  ENDWITH

  *!*************************************************************
  *! Name      : lfChangeMode
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : To be called from the ChangeMode method of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : True / Flase
  *!*************************************************************
  FUNCTION lfChangeMode


	DO CASE
	  CASE This.loFormSet.activemode = 'S'
		SELECT (This.lcForCstHdr)
		ZAP
		SELECT (This.lcForCstDtl)
		ZAP   	    

        ldDate      = CTOD('01/01/'+STR(YEAR(oAriaApplication.SystemDate),4))
        ldEndDate   = GOMONTH(LDdATE,12) - 1
        No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
        St_Day      = ldDate-DOW(ldDate)+1 
        last_day    = ldEndDate-dow(ldEndDate)
        lnWeek      = (MAX(CEILING((oAriaApplication.SystemDate-St_Day)/7),1))
        *-- Display Week Start Date.
        DO CASE
          CASE BETWEEN(oAriaApplication.SystemDate,St_Day,last_day)
           lnYear = YEAR(oAriaApplication.SystemDate) 
           ldDispDate = ((lnWeek-1)*7) + St_Day
         CASE oAriaApplication.SystemDate <  St_Day
           lnYear = YEAR(oAriaApplication.SystemDate) - 1
           ldDispDate = ((lnWeek-1)*7) + St_Day
         CASE oAriaApplication.SystemDate > last_day
           lnYear = YEAR(oAriaApplication.SystemDate) + 1
           lnWeek = 1
           ldDispDate = last_day + 1     
        ENDCASE
	    
  	    IF (This.FORCASTH.SEEK(STR(lnYear,4)+STR(lnWeek,2)) OR ;
	  		RECCOUNT(This.FORCASTH.lcCursorView) <> 0) AND This.FORCAST.SEEK(STR(lnYear,4)+STR(lnWeek,2)) 
          lnWeek = lnWeek + 1
          IF lnWeek > No_Of_weeks
            lnWeek = 1
            lnYear = lnYear + 1
          ENDIF
          DO WHILE (This.FORCASTH.SEEK(STR(lnYear,4)+STR(lnWeek,2)) OR ;
	  		RECCOUNT(This.FORCASTH.lcCursorView) <> 0) AND This.FORCAST.SEEK(STR(lnYear,4)+STR(lnWeek,2)) 
            lnWeek = lnWeek + 1
            IF lnWeek > No_Of_weeks
              lnWeek = 1
              lnYear = lnYear + 1
            ENDIF
	      ENDDO		
 	      ldDate    = CTOD('01/01/'+STR(lnYear,4))    
  	      St_Day    = ldDate-DOW(ldDate)+1 
	      IF lnWeek  = 1
	        ldDispDate  = St_Day
	      ELSE
	        ldDispDate  = ((lnWeek -1)*7) + St_Day
	      ENDIF
	    ENDIF
        This.loForm.txtWeek.value = lnWeek
	    This.loForm.txtYear.value = lnYear 
	    This.loFormSet.Ariaform1.dtForcasting.text1.value = ldDispDate 	    

	  CASE This.loFormSet.activemode = 'V'
	    SELECT (This.lcForCstDtl)
	    lnWeek = IIF(EMPTY(This.loForm.txtWeek.value),FORCASTH.nWeek,This.loForm.txtWeek.value)
	    lnYear = IIF(EMPTY(This.loForm.txtYear.value),FORCASTH.nYear,This.loForm.txtYear.value)
        lnWeek = IIF(TYPE('lnWeek') = 'N' ,lnWeek,IIF(EMPTY(lnWeek),0,EVALUATE(lnWeek)))	    
        lnYear = IIF(TYPE('lnYear') = 'N' ,lnYear,IIF(EMPTY(lnYear),0,EVALUATE(lnYear)))
	    This.loForm.txtWeek.value = lnWeek
	    This.loForm.txtYear.value = lnYear

	    *- Function to Display the sizes of the style+color from the scale file.
	    This.lfDispSize()
	    ldDate    = CTOD('01/01/'+STR(This.loForm.txtYear.value,4))    
	    St_Day    = ldDate-DOW(ldDate)+1 
	    IF This.loForm.txtWeek.value = 1
	      This.loFormSet.Ariaform1.dtForcasting.text1.value = St_Day
	    ELSE
	      This.loFormSet.Ariaform1.dtForcasting.text1.value = ((This.loForm.txtWeek.value-1)*7) + St_Day
	    ENDIF
	    
        This.lfGetData()
        
      CASE This.loFormSet.activemode = 'E'  
        SELECT (This.lcForCstDtl)
        LOCATE
        This.Style.SEEK(EVALUATE(This.lcForCstDtl +'.Style'))
        This.Scale.SEEK('S'+STYLE.Scale)
        This.loForm.sbFCstQty.scalecnt = Scale.Cnt

      CASE This.loFormSet.activemode = 'A'
		SELECT (This.lcForCstHdr)
		ZAP
		SELECT (This.lcForCstDtl)
		ZAP   	    
		
        lnWeek = IIF(TYPE('This.loForm.txtWeek.value') = 'N' ,This.loForm.txtWeek.value,;
        			 IIF(EMPTY(This.loForm.txtWeek.value),0,EVALUATE(This.loForm.txtWeek.value)))	    
        lnYear = IIF(TYPE('This.loForm.txtYear.value') = 'N' ,This.loForm.txtYear.value,;
        			 IIF(EMPTY(This.loForm.txtYear.value),0,EVALUATE(This.loForm.txtYear.value)))
        
   	    ldDate    = CTOD('01/01/'+STR(lnYear,4))    
	    St_Day    = ldDate-DOW(ldDate)+1 
	    IF lnWeek = 1
	      This.loFormSet.Ariaform1.dtForcasting.text1.value = St_Day
	    ELSE
	      This.loFormSet.Ariaform1.dtForcasting.text1.value = ((lnWeek-1)*7) + St_Day
	    ENDIF
	    
	    m.nWeek = lnWeek 
        m.nYear = lnYear 
        INSERT INTO (This.lcForCstHdr) FROM MEMVAR	    
	
	ENDCASE 
    This.lfBoundGrid()
    
  *!*************************************************************
  *! Name      : lfOpenFile
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : To Open a Table Remotely
  *!*************************************************************
  *! Parameters:  lcFile, lcTag
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION lfOpenFile
  PARAMETERS lcFile, lcTag
  LOCAL lcProp

  lcFile = JUSTSTEM(lcFile)
  lcTag = JUSTSTEM(lcTag)
  lcProp = lcFile
  IF !PEMSTATUS(This,lcProp,5)
    This.addproperty(lcProp)
  ENDIF
  lcProp = 'This.'+lcProp

  IF TYPE(lcProp)<>'O'
    &lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,This.loFormSet.DataSessionID)
  ELSE
    &lcProp..SetOrder(lcTag)
  ENDIF

  ENDFUNC
  
  *!*************************************************************
  *! Name      : lfDispSize
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : Creat Temp Forecasting file
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfCrtFile
  
  *-- Creat temp header file
  SELECT FORCASTH
  =AFIELDS(laFileStru)
  DIMENSION laImdex[1,2]
  laImdex[1,1] = 'STR(nyear,4)+STR(nweek,2)'
  laImdex[1,2] = 'Forcasth'
  *--  Create temp file for the english Charges
  =gfCrtTmp(This.lcForCsthdr,@laFileStru,@laImdex)

  *-- Creat temp detail file
  SELECT FORCAST
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,18]
  laFileStru[lnFileStru+1,1] = 'Flag'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'Desc'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 20
  laFileStru[lnFileStru+2,4] = 0

  FOR lnCount = 1 TO 2
    STORE '' TO laFileStru[lnFileStru+lnCount,7] ,laFileStru[lnFileStru+lnCount,8] ,laFileStru[lnFileStru+lnCount,9],;
                laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
                laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
                laFileStru[lnFileStru+lnCount,16]
    STORE 0 TO  laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
  ENDFOR
  DIMENSION laImdex[2,2]
  laImdex[1,1] = 'style+STR(nyear,4)+STR(nweek,2)'
  laImdex[1,2] = 'Forcast'
  laImdex[2,1] = 'STR(nyear,4)+STR(nweek,2)+style'
  laImdex[2,2] = 'Forcastw'
  *--  Create temp file for the english Charges
  =gfCrtTmp(This.lcForCstDtl,@laFileStru,@laImdex)
  SELECT (This.lcForCstDtl)
  SET ORDER TO Forcastw
  
  *!*************************************************************
  *! Name      : lfDispSize
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : Display the sizes of the style+color from the scale file
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfDispSize

	This.Style.SEEK(This.loFormSet.Ariaform1.kbStyle.value)
	This.Scale.SEEK('S'+STYLE.Scale)
	This.loForm.sbFCstQty.scalecnt = Scale.Cnt
    FOR lnI =1 TO Scale.Cnt
      lcI = STR(lnI,1)
	  This.loForm.sbFCstQty.txtsize&lcI..Value =  PADR(scale.sz&lcI,5) 
    ENDFOR		
	FOR lnI =Scale.Cnt+1 TO 8
      lcI = STR(lnI,1)
	  This.loForm.sbFCstQty.txtsize&lcI..Value =  ""
    ENDFOR		

  *!*************************************************************
  *! Name      : lfBoundGrid
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : Bound Grid's coulmn to the temp file
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfBoundGrid
  
  lcForCstDtl = This.lcForCstDtl

  lcStyleTtl = gfItemMask("HI", '', '0001')

  WITH This.loForm.grdDetail 
    .RecordSource = ''
    .RecordSource = lcForCstDtl
    *-- Build browse columns
    .Style.Header1.Caption  = lcStyleTtl
    IF This.loFormSet.activemode = 'S'
      .Style.ControlSource = ""
      .Description.ControlSource = ""
      .Qty1.ControlSource = ""
      .Qty2.ControlSource = ""
      .Qty3.ControlSource = ""
      .Qty4.ControlSource = ""
      .Qty5.ControlSource = ""
      .Qty6.ControlSource = ""
      .Qty7.ControlSource = ""
      .Qty8.ControlSource = ""
      .Total.ControlSource = ""
    ELSE
      .Style.ControlSource = lcForCstDtl +'.Style'
      .Description.ControlSource = lcForCstDtl +'.Desc'
      .Qty1.ControlSource = lcForCstDtl +'.nforqty1'
      .Qty2.ControlSource = lcForCstDtl +'.nforqty2'
      .Qty3.ControlSource = lcForCstDtl +'.nforqty3'
      .Qty4.ControlSource = lcForCstDtl +'.nforqty4'
      .Qty5.ControlSource = lcForCstDtl +'.nforqty5'
      .Qty6.ControlSource = lcForCstDtl +'.nforqty6'
      .Qty7.ControlSource = lcForCstDtl +'.nforqty7'
      .Qty8.ControlSource = lcForCstDtl +'.nforqty8'                  
      .Total.ControlSource = lcForCstDtl +'.nfortotqty' 
    ENDIF 
    .SetAll('ReadOnly',.T.)
    .refresh()
  ENDWITH
  
  *!*************************************************************
  *! Name      : lfGetData
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : Update temp files with forcasting information
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfGetData
  

  lnWeek = IIF(TYPE('This.loForm.txtWeek.value') = 'N' ,This.loForm.txtWeek.value,;
       			 IIF(EMPTY(This.loForm.txtWeek.value),0,EVALUATE(This.loForm.txtWeek.value)))	    
  lnYear = IIF(TYPE('This.loForm.txtYear.value') = 'N' ,This.loForm.txtYear.value,;
       			 IIF(EMPTY(This.loForm.txtYear.value),0,EVALUATE(This.loForm.txtYear.value)))
  
  *--Update header file
  This.FORCASTH.SetOrder('FORCASTH')
  This.FORCASTH.SEEK(STR(lnYear,4)+STR(lnWeek,2))
  SELECT FORCASTH
  IF RECCOUNT(This.FORCASTH.lcCursorView) <> 0
    LOCATE
  ENDIF
  SCATTER MEMVAR MEMO
  SET ORDER TO FORCASTH IN (This.lcForCstHdr)
  IF !SEEK(STR(m.nYear,4)+STR(m.nWeek,2),This.lcForCstHdr)
    INSERT INTO (This.lcForCstHdr) FROM MEMVAR
  ENDIF    
  SET ORDER TO Forcast IN (This.lcForCstDtl)
  *-- Update detail file
  This.FORCAST.SetOrder('FORCASTW')
  This.FORCAST.SEEK(STR(FORCASTH.nYear,4)+STR(FORCASTH.nWeek,2))
  SELECT FORCAST
  SCAN REST WHILE STR(nyear,4)+STR(nweek,2)+style = STR(FORCASTH.nYear,4)+STR(FORCASTH.nWeek,2)
    SCATTER MEMVAR MEMO
    SET ORDER TO FORCASTW IN (This.lcForCstDtl)
    IF !SEEK(STR(m.nYear,4)+STR(m.nWeek,2)+m.Style,This.lcForCstDtl)
      This.Style.SEEK(m.Style)
      m.Desc = Style.Desc
      INSERT INTO (This.lcForCstDtl) FROM MEMVAR
    ENDIF
  ENDSCAN
  SELECT (This.lcForCstDtl)
  LOCATE
  This.loForm.grdDetail.Refresh
  	 
  *!*************************************************************
  *! Name      : lfvWeek
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Validate for the week number
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfvWeek
  LPARAMETERS lnValue,lnOldValue

  lnValue    = IIF(TYPE('lnValue') = 'N' ,lnValue,IIF(EMPTY(lnValue),0,EVALUATE(lnValue)))
  lnOldValue = IIF(TYPE('lnOldValue') = 'N',lnOldValue,IIF(EMPTY(lnOldValue),0,EVALUATE(lnOldValue)))
  
  This.llGetData = .T.
  lnYear = This.loForm.txtYear.value
  lnYear = IIF(TYPE('lnYear ')='N',lnYear,IIF(EMPTY(lnYear ),0,EVALUATE(lnYear )))

  IF lnValue =< 0
    *- Text Message  : Week number cannot be less or equal to zero!.
    *- Message No    : 36014.
    *- Buttom Message: OK
    *- Buttom No.    : 00000
    =gfModalGen('TRM36014B00000','ALERT',LANG_StyForCst_WeekNo)
    This.loForm.txtWeek.value  = lnOldValue
    RETURN .F.
  ENDIF
  IF lnYear = 0
    lnYear = 1900
  ENDIF
  ldDate      = CTOD('01/01/'+STR(lnYear,4))
  ldEndDate   = GOMONTH(LDdATE,12) - 1
  No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
  St_Day      = ldDate-DOW(ldDate)+1 

  IF lnValue  > ROUND(No_Of_weeks,2)
    *- Text Message  : Weeks number cannot exceeds XX Weeks!.
    *- Message No    : 40171.
    *- Buttom Message: OK
    *- Buttom No.    : 00000
    =gfModalGen('TRM40171B00000','ALERT',LANG_StyForCst_WeekNo+"|"+STR(No_Of_weeks,2)+LANG_StyForCst_Weeks)  
    This.loForm.txtWeek.value  = lnOldValue
    RETURN .F.
  ENDIF
  IF lnYear = 0
    This.loFormSet.Ariaform1.dtForcasting.text1.value = {}
  ENDIF  
  *- End oflfvWeek
  
  *!*************************************************************
  *! Name      : lfvYear
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Check if the entered week belongs to the year or not
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfvYear
  LPARAMETERS lnValue,lnOldValue
  lnValue    = IIF(TYPE('lnValue') = 'N' ,lnValue,IIF(EMPTY(lnValue),0,EVALUATE(lnValue)))
  lnOldValue = IIF(TYPE('lnOldValue') = 'N' ,lnOldValue,IIF(EMPTY(lnOldValue),0,EVALUATE(lnOldValue)))
   

  lnWeek = This.loForm.txtWeek.value
  lnWeek = IIF(TYPE('lnWeek')='N',lnWeek,IIF(EMPTY(lnWeek),0,EVALUATE(lnWeek)))

  IF LEN(ALLTRIM(STR(lnValue))) < 4
    *- Text Message  : Invalid year.
    *- Message No    : 42138.
    *- Buttom Message: OK
    *- Buttom No.    : 00000
    =gfModalGen('TRM42138B00000','ALERT',LANG_StyForCst_Year)
    This.loForm.txtYear.value  = lnOldValue
    RETURN 0
  ENDIF
  
  IF lnWeek = 0
    = gfModalGen('TRM32135B00000','ALERT')
    RETURN -1
  ENDIF
  
  ldDate      = CTOD('01/01/'+STR(lnValue,4))
  ldEndDate   = GOMONTH(LDdATE,12) - 1
  No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
  IF lnWeek  > ROUND(No_Of_weeks,2)
    *- Text Message  : Invalid year.
    *- Message No    : 42138.
    *- Buttom Message: OK
    *- Buttom No.    : 00000
    = gfModalGen('TRM32136B00000','ALERT',STR(No_Of_weeks,2))
    RETURN 0
  ENDIF

  St_Day    = ldDate-DOW(ldDate)+1 
  IF lnWeek = 1
    This.loForm.dtForcasting.text1.value = St_Day
  ELSE
    This.loForm.dtForcasting.text1.value = ((lnWeek -1)*7) + St_Day
  ENDIF

  *- Enable and disable sizes fileds.
 	  This.FORCASTH.SetOrder('FORCASTH')
  This.FORCAST.SetOrder('FORCASTW')
  IF (This.FORCASTH.SEEK(STR(lnValue,4)+STR(lnWeek,2)) OR ;
  		RECCOUNT(This.FORCASTH.lcCursorView) <> 0) AND This.FORCAST.SEEK(STR(lnValue,4)+STR(lnWeek,2)) 
    This.loFormSet.ActiveMode = 'V'
    This.loFormSet.ChangeMode('V')
  ELSE
    lnChoice = gfModalGen('TRM32137B00001','ALERT',STR(lnWeek,2)+"|"+STR(lnValue,4))
    DO CASE
      CASE lnChoice = 1
        oAriaApplication.oToolBar.cmdFind.Click()
      CASE lnChoice = 2
   	        This.loFormSet.ActiveMode = 'A'
        This.loFormSet.ChangeMode('A')
      CASE lnChoice = 3
        This.loForm.txtWeek.value = ""
        This.loForm.txtYear.value = ""	
        This.loForm.dtForcasting.text1.value = {}     
        RETURN -1   
    ENDCASE  	      	      
  ENDIF

  *!*************************************************************
  *! Name      : lfwDtlBrow
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : When function for the grid
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfwDtlBrow
  
  This.loForm.kBSTYLE.Value = EVALUATE(This.lcForCstDtl +'.Style')
  This.loForm.txtdESCRIPTION.Value = EVALUATE(This.lcForCstDtl +'.Desc')

  This.Style.SEEK(EVALUATE(This.lcForCstDtl +'.Style'))
  This.Scale.SEEK('S'+STYLE.Scale)
  This.loForm.sbFCstQty.scalecnt = Scale.Cnt
  FOR lnI =1 TO Scale.Cnt
    lcI = STR(lnI,1)
    This.loForm.sbFCSTQTY.txtQty&lcI..Value  = EVALUATE(This.lcForCstDtl +'.nforqty'+lcI)
    This.loForm.sbFCstQty.txtsize&lcI..Value =  PADR(scale.sz&lcI,5) 
  ENDFOR
  This.loForm.sbFCSTQTY.txttotalQty.Value  = EVALUATE(This.lcForCstDtl +'.nfortotqty')
  
  FOR lnI = Scale.Cnt + 1 TO 8
    lcI = STR(lnI,1)
    This.loForm.sbFCstQty.txtsize&lcI..Value =  ""
  ENDFOR  		  
 
  *!*************************************************************
  *! Name      : lfvStyle 
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Valid Function for style field
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfvStyle 
  LPARAMETERS lcValue , lcOldValue
  
  IF lcValue <> lcOldValue
    lnWeek = IIF(TYPE('This.loForm.txtWeek.value') = 'N' ,This.loForm.txtWeek.value,;
       			 IIF(EMPTY(This.loForm.txtWeek.value),0,EVALUATE(This.loForm.txtWeek.value)))	    
    lnYear = IIF(TYPE('This.loForm.txtYear.value') = 'N' ,This.loForm.txtYear.value,;
       			 IIF(EMPTY(This.loForm.txtYear.value),0,EVALUATE(This.loForm.txtYear.value)))
    IF This.FORCAST.SEEK(STR(lnYear,4)+STR(lnWeek,2)+lcValue)
      =gfModalGen('TRM32138B00000','ALERT',lcValue)
      lcValue = ""
      RETURN .F.
    ENDIF
    m.nweek = lnWeek 
    m.nyear = lnYear
    m.style = lcValue
    This.Style.SEEK(m.Style)
    m.Desc = STYLE.Desc
    m.flag = 'N'
    INSERT INTO (This.lcForCstDtl) FROM MEMVAR
    SELECT (This.lcForCstDtl)
    LOCATE
    =SEEK(STR(m.nYear,4)+STR(m.nWeek,2)+m.Style,This.lcForCstDtl,'FORCASTW')
    This.loForm.grdDetail.AFTERROWCOLCHANGE()
  ENDIF
  
  *!*************************************************************
  *! Name      : lfvQty
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Valid Function for Qty fields
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfvQty
  LPARAMETERS lnValue , lnOldValue , lcQty
 
  lnValue    = IIF(TYPE('lnValue') = 'N' ,lnValue,IIF(EMPTY(lnValue),0,EVALUATE(lnValue)))
  lnOldValue = IIF(TYPE('lnOldValue') = 'N' ,lnOldValue,IIF(EMPTY(lnOldValue),0,EVALUATE(lnOldValue)))
  
  IF lnValue <> lnOldValue
    IF lnValue < 0
	  *- Text Message  : Quantity cannot be less or equal to zero!.
	  *- Message No    : 36014.
	  *- Buttom Message: OK
	  *- Buttom No.    : 00000
	  =gfModalGen('TRM321414B00000','ALERT',LANG_StyForCst_Quantity)
	  lnValue = lnOldValue
	  RETURN .F.
	ENDIF
    SELECT (This.lcForCstDtl)
    REPLACE nforqty&lcQty WITH lnValue,;
    		Flag		  WITH IIF(Flag = 'N',FLAG,'M')

    lnTotal = 0
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      lnTotal = lnTotal + nforqty&lcI
    ENDFOR    		
    REPLACE nfortotqty WITH lnTotal
     		
  ENDIF 

  *!*************************************************************
  *! Name      : lfRecChange
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : navigation function
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfvRemLine
  
  SELECT (This.lcForCstDtl)
  REPLACE FLAG WITH IIF(This.loFormSet.ActiveMode = 'A',"",'D')
  DELETE
  SELECT (This.lcForCstDtl)
  LOCATE
  This.loForm.grdDetail.AFTERROWCOLCHANGE()
  
  *!*************************************************************
  *! Name      : lfRecChange
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : navigation function
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfRecChange
 
  SELECT (This.lcForCstHdr)
  ZAP
  SELECT (This.lcForCstDtl)
  ZAP   

  This.loForm.txtWeek.value = FORCASTH.nWeek
  This.loForm.txtYear.value = FORCASTH.nYear

  ldDate    = CTOD('01/01/'+STR(This.loForm.txtYear.value,4))    
  St_Day    = ldDate-DOW(ldDate)+1 
  IF This.loForm.txtWeek.value = 1
    This.loFormSet.Ariaform1.dtForcasting.text1.value = St_Day
  ELSE
    This.loFormSet.Ariaform1.dtForcasting.text1.value = ((This.loForm.txtWeek.value-1)*7) + St_Day
  ENDIF  	    
  This.lfGetData()
  SELECT (This.lcForCstDtl)
  LOCATE
  This.lfBoundGrid()
  This.loForm.grdDetail.Refresh()
  This.loForm.grdDetail.AFTERROWCOLCHANGE()

    
  *!*************************************************************
  *! Name      : lfUpdPlan
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Update plan function
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfUpdPlan

  lnYear = IIF(TYPE('This.loForm.txtYear.value') = 'N' ,This.loForm.txtYear.value,;
        			 IIF(EMPTY(This.loForm.txtYear.value),0,EVALUATE(This.loForm.txtYear.value)))

  llUpdate = .F.
  This.FORCAST.SetOrder('FORCAST')
  SELECT (This.lcForCstDtl)
  SCAN
    lcStyle = Style
    lnYear  = nYear
    IF This.FORCAST.SEEK(lcStyle +STR(lnYear,4))
      SELECT FORCAST
      STORE 0 TO m.Plan1,m.Plan2,m.Plan3,m.Plan4,m.Plan5,m.Plan6,m.Plan7,m.Plan8,m.TOTPLAN
      SCAN REST WHILE style+STR(nyear,4)+STR(nweek,2) = lcStyle+STR(lnYear,4) 
        m.Plan1 = m.Plan1 + FORCAST.NFORQTY1  
        m.Plan2 = m.Plan2 + FORCAST.NFORQTY2  
        m.Plan3 = m.Plan3 + FORCAST.NFORQTY3  
        m.Plan4 = m.Plan4 + FORCAST.NFORQTY4  
        m.Plan5 = m.Plan5 + FORCAST.NFORQTY5  
        m.Plan6 = m.Plan6 + FORCAST.NFORQTY6  
        m.Plan7 = m.Plan7 + FORCAST.NFORQTY7  
        m.Plan8 = m.Plan8 + FORCAST.NFORQTY8  
        m.TOTPLAN = m.TOTPLAN + FORCAST.NFORTOTQTY      
      ENDSCAN
      IF m.TOTPLAN > 0 AND THIS.Style.SEEK(lcStyle)
        llUpdate = .T.
        SELECT Style
        THIS.Style.REPLACE('PLAN1 WITH m.Plan1,;
                PLAN2 WITH m.Plan2,;
                PLAN3 WITH m.Plan3,;
                PLAN4 WITH m.Plan4')
        THIS.Style.REPLACE('PLAN5 WITH m.Plan5,;
                PLAN6 WITH m.Plan6,;
                PLAN7 WITH m.Plan7,;
                PLAN8 WITH m.Plan8,;
                TOTPLAN WITH m.TOTPLAN')
      ENDIF      
    ENDIF  
  ENDSCAN

    IF llUpdate 
      DIMENSION laTableUpdate[1]
      laTableUpdate[1] = This.Style
       =This.lfTableUpdate()
       =gfModalGen("TRM32139B00000","DIALOG")
    ENDIF

  *!*************************************************************
  *! Name      : lfBeforeSave 
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Validate saving procces
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfBeforeSave 

  SELECT (This.lcForCstDtl)

  LOCATE
  IF EOF()
     = gfModalGen('TRM32140B00000','ALERT')
    RETURN .F.
  ENDIF
  
  *!*************************************************************
  *! Name      : lfSaveFiles
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Saving procces
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfSaveFiles

  lcDelStat = SET("Deleted")
  SET DELETED OFF 
  SELECT (This.lcForCstDtl)

  RECALL ALL
  SET DELETED &lcDelStat   
  
  lcShortVersion = 'A40'
  lcUser_ID = oAriaApplication.User_ID 
    
  This.lnWeek = IIF(TYPE('This.loForm.txtWeek.value') = 'N' ,This.loForm.txtWeek.value,;
       			 IIF(EMPTY(This.loForm.txtWeek.value),0,EVALUATE(This.loForm.txtWeek.value)))	    
  This.lnYear = IIF(TYPE('This.loForm.txtYear.value') = 'N' ,This.loForm.txtYear.value,;
       			 IIF(EMPTY(This.loForm.txtYear.value),0,EVALUATE(This.loForm.txtYear.value)))
  
  SET ORDER TO FORCASTW
  SELECT (This.lcForCstHdr)
  SCATTER MEMVAR MEMO
  IF SEEK(STR(m.nyear,4)+STR(m.nweek,2),This.lcForCstDtl) AND EVALUATE(This.lcForCstDtl+'.nfortotqty') <> 0
    IF !This.FORCASTH.SEEK(STR(m.nyear,4)+STR(m.nweek,2)) AND RECCOUNT(This.FORCASTH.lcCursorView) = 0
      m.cAdd_User  = lcUser_ID 
      m.dAdd_Date  = DATE()    
      m.cAdd_Time  = gfGetTime()
      m.cAdd_Ver   = lcShortVersion
      THIS.FORCASTH.INSERT('FROM MEMVAR')
    ENDIF
    SELECT (This.lcForCstDtl)
    SCAN REST WHILE STR(nyear,4)+STR(nweek,2)+style = STR(m.nyear,4)+STR(m.nweek,2)
      SCATTER MEMVAR MEMO
      DO CASE
        CASE m.FLAG = 'D'   && Delete line
          IF This.FORCAST.SEEK(STR(m.nyear,4)+STR(m.nweek,2)+m.Style) 
            SELECT FORCAST
            BLANK
            THIS.FORCAST.DELETE()
          ENDIF
        CASE m.FLAG = 'M'  && Modifiy Line 
          IF This.FORCAST.SEEK(STR(m.nyear,4)+STR(m.nweek,2)+m.Style) 
            SELECT FORCAST
            THIS.FORCAST.REPLACE('nweek    WITH m.nWeek,;
            		nYear    WITH m.nYear,;
            		NFORQTY1 WITH m.NFORQTY1,;
            		NFORQTY2 WITH m.NFORQTY2,;
            		NFORQTY3 WITH m.NFORQTY3')
            THIS.FORCAST.REPLACE('NFORQTY4 WITH M.NFORQTY4,;
            		NFORQTY5 WITH m.NFORQTY5,;
            		NFORQTY6 WITH m.NFORQTY6,;
            		NFORQTY7 WITH m.NFORQTY7')
            THIS.FORCAST.REPLACE('NFORQTY8 WITH M.NFORQTY8,;
            		NFORTOTQTY WITH M.NFORTOTQTY')
	        =This.lfAdd_Info(This.FORCAST)  
          ENDIF
        CASE m.FLAG = 'N'  && New line
          IF !This.FORCAST.SEEK(STR(m.nyear,4)+STR(m.nweek,2)+m.Style)
		    m.cAdd_User  = lcUser_ID 
	        m.dAdd_Date  = DATE()    
	        m.cAdd_Time  = gfGetTime()
	        m.cAdd_Ver   = lcShortVersion
          
            THIS.FORCAST.INSERT('FROM MEMVAR')
	        =This.lfAdd_Info(This.FORCAST)
          ENDIF
      ENDCASE
    ENDSCAN
  ENDIF
  SELECT (This.FORCASTH.lcCursorUpdate)
  DELETE FOR STR(nyear,4)+STR(nweek,2) = " "
  This.FORCASTH.SEEK(STR(This.lnyear,4)+STR(This.lnweek,2))
  SELECT FORCASTH
  IF RECCOUNT(This.FORCASTH.lcCursorView) <> 0  
    LOCATE
  ENDIF
  SELECT FORCAST
  DELETE FOR STR(nyear,4)+STR(nweek,2)+Style = " "
  SELECT (This.lcForCstHdr)
  ZAP
  SELECT (This.lcForCstDtl)
  ZAP   	    

 *!*************************************************************
  *! Name      : lfSaveFiles
  *: Developer : Adel Mohammed El Gazzar (ADEL)
  *: DATE      : 02/20/2002
  *! Purpose   : Saving procces
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  FUNCTION lfDelFiles

  lnWeek = IIF(TYPE('This.loForm.txtWeek.value') = 'N' ,This.loForm.txtWeek.value,;
        			 IIF(EMPTY(This.loForm.txtWeek.value),0,EVALUATE(This.loForm.txtWeek.value)))	    
  lnYear = IIF(TYPE('This.loForm.txtYear.value') = 'N' ,This.loForm.txtYear.value,;
        			 IIF(EMPTY(This.loForm.txtYear.value),0,EVALUATE(This.loForm.txtYear.value)))
  This.FORCASTH.SetOrder('FORCASTH')
  This.FORCASTH.SEEK(STR(lnYear,4)+STR(lnWeek,2))  
  SELECT FORCASTH
  IF RECCOUNT(This.FORCASTH.lcCursorView) <> 0  
    LOCATE
  ENDIF
  This.FORCAST.SetOrder('FORCASTW')
  This.FORCAST.SEEK(STR(FORCASTH.nYear,4)+STR(FORCASTH.nWeek,2))
  lcKeyExpr = STR(FORCASTH.nYear,4)+STR(FORCASTH.nWeek,2)
  SELECT FORCAST
  DELETE REST FOR STR(nyear,4)+STR(nweek,2)+style = lcKeyExpr 
  This.FORCASTH.Delete()
  
  *!*************************************************************
  *! Name      : lfUpdate
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : function to Update Sql Tables.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  FUNCTION lfUpdate
  

  DIMENSION laTableUpdate[2]
  laTableUpdate[1] = This.FORCASTH
  laTableUpdate[2] = This.FORCAST

  IF !This.lfTableUpdate()
    RETURN .F.
  ENDIF
  This.FORCASTH.SEEK(STR(This.lnyear,4)+STR(This.lnweek,2))
  SELECT FORCASTH
  IF RECCOUNT(This.FORCASTH.lcCursorView) <> 0  
    LOCATE
  ENDIF
  


  *!*************************************************************
  *! Name      : lfTableUpdate
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : function to Update Sql Tables.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  FUNCTION lfTableUpdate

  *--Open Dictionary files.
  LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
  lnAlias = SELECT(0)

  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
  IF TYPE('lcTranCode') = 'N'
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  
  FOR lnI = 1 TO ALEN(laTableUpdate,1)
    llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
    IF !llUpdate
      =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
      SELECT (lnAlias)
      RETURN .F.
    ENDIF
  ENDFOR

  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
  IF lnConnectionHandlar # 1
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT(lnAlias)
    RETURN .F.
  ENDIF

  SELECT(lnAlias)
  *--end of lfTableUpdate.

  *!*************************************************************
  *! Name      : lfTableUpdate
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 06/26/2005
  *! Purpose   : function to Update Sql Tables.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  FUNCTION lfAdd_Info
  LPARAMETERS loFileName 

  lcShortVersion = 'A40'
  lcUser_ID = oAriaApplication.User_ID 
  *** stamp the record for this user with date and time
  loFileName.REPLACE('cAdd_User  WITH lcUser_ID ,;
            dAdd_Date  WITH DATE()     ,;
            cAdd_Time  WITH gfGetTime(),;
            cAdd_Ver   WITH lcShortVersion')

  
ENDDEFINE
