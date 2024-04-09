*!**************************************************************************
*! Name      : FREMAIN.PRG
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/17/2000
*! Purpose   : ARIA'S payment schedule Custom Process Program.
*! Reference : #C101898,1
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!***************************************************************
*! Name      : lfDELORD
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/17/2000
*! Purpose   : 
*!***************************************************************
*!
FUNCTION lfDELORD

PRIVATE llChoice , lcActvAlis

*-- Message : 32082
*--         : "This order is canceled. What do you want to do?"
*-- Button  : 00039
*--         : <Uncancel><Delete><Nothing>


llChoice = gfModalGen('QRM32082B00039','DIALOG')
IF llChoice = 3 
  RETURN .F.
ENDIF
IF llChoice = 2
  lcActvAlis = SELECT(0)
  SELECT ORDHDR
  SET RELATION TO cordtype+order INTO Ordline 
  SELECT ORDLINE
  
  *SCAN WHILE Order = ORDHDR.Order AND cordtype = ORDHDR.cordtype
  *  BLANK
  *  DELETE
  *  IF SEEK(ORDHDR.cordtype+ORDHDR.order,"OrdLine")
  *    SKIP -1
  *  ENDIF
  *ENDSCAN

  DO WHILE !EOF() AND Order = ORDHDR.Order AND cordtype = ORDHDR.cordtype 
    BLANK
    DELETE
    =SEEK(ORDHDR.cordtype+ORDHDR.order,"OrdLine")
  ENDDO
    
  SELECT ORDHDR 
  BLANK 
  DELETE
  LOCATE FOR !EMPTY(cOrdType)
  IF EOF() 
    APPEND BLANK	
  ENDIF
  
  STORE .T. TO laScrMode[1]
  SHOW GETS
  SELECT (lcActvAlis) 
  RETURN .F.
ENDIF
RETURN .T.
*-- End of lfDELORD