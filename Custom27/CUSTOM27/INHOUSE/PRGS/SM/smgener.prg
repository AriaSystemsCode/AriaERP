*:*************************************************************************
*: Program file  : SMGENERR.prg
*: Program desc. : Program To Generate Error
*: For Screen    : 
*:        System : Aria 27
*:        Module : SM
*:     Developer : Hesham El-Sheltawi 
*:*************************************************************************
*: Calls       :
*:            FUNCTION : 
*:*************************************************************************
*E000000,1 New program to GENERETE ERROR so we can test the error handler
*E000000,1 in different cases
EXTERNAL ARRAY laData,laDefProc
DECLARE laScrError[1,2]
STORE '' TO lcScrMode,laScrError
STORE 0 TO lserrorPos,lsGenerror,lsError
DECLARE laObjStat[9]  && Array to hold the display status of each seg.
llNoContrl = .T.
IF !gfSetup()
  RETURN
ENDIF

DECLARE laErrors[1]
STORE '' TO laErrors
SELECT ALLT(CERRNO)+' '+ALLT(CERRMSG) FROM (gcSysHome+'SYERRORS') ;
  INTO ARRAY laErrors
IF USED('SYERRORS')  
  USE IN SYERRORS
ENDIF
IF !WEXIST(gcBaseWind)
  lcScrMode = 'E'
  IF FILE(gcSysHome+'ERROR.MEM')
    RESTORE FROM (gcSysHome+'ERROR.MEM') ADDI
    IF !EMPTY(laScrError)
      lsGenerror = 1
      lserrorPos = laScrError[1,2]
    ENDIF
  ENDIF 
ENDIF

DO (gcScrDir+"\SMGENER.SPX")

*!*************************************************************************
*! Name      : LPSHOW
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lpShow

DO CASE
  CASE lcScrMode = 'N'
    SHOW GETS DISABLE ONLY
    SHOW GET lsError ENABLE
    SHOW GET PBCANCEL ENABLE    
  Otherwise
    SHOW GETS ENABLE ONLY
    lsError = 0    
    SHOW GET lsError DISABLE
ENDCASE
IF EMPTY(laScrError)
  SHOW GET PBREMOVE DISABLE
  SHOW GET lserrorPos DISABLE
ENDIF

*!*************************************************************************
*! Name      : lfvOk
*! Developer : Hesham El-Sheltawi 
*! Date      : 11/18/98
*! Purpose   : Valid Function for Ok push button
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvOk
SAVE TO (gcSysHome+'ERROR.MEM') ALL LIKE laScrError
glQuitting = .T.
CLEAR READ

*!*************************************************************************
*! Name      : lfvCancel
*! Developer : Hesham El-Sheltawi 
*! Date      : 11/18/98
*! Purpose   : Valid Function for Cancel push button
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvCancel
glQuitting = .T.
CLEAR READ

*!*************************************************************************
*! Name      : lfvNew
*! Developer : Hesham El-Sheltawi 
*! Date      : 11/18/98
*! Purpose   : Valid Function for New push button
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvNew
lsError = 0
lcScrMode = 'N'
SHOW GETS
KEYBOARD "{SPACEBAR}"
RETURN -1

*!*************************************************************************
*! Name      : lfvRemove
*! Developer : Hesham El-Sheltawi 
*! Date      : 11/18/98
*! Purpose   : Valid Function for Remove push button
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvRemove
IF ALEN(laScrError,1) = 1
  STORE '' TO laScrError
  lserrorPos = 0
ELSE
  =gfADel(@laScrError,lsGenerror)
  lsGenerror = 1
  =lfvLine()
ENDIF
lcScrMode = 'E'
SHOW GETS


*!*************************************************************************
*! Name      : lfvSelect
*! Developer : Hesham El-Sheltawi 
*! Date      : 11/18/98
*! Purpose   : Valid Function for  Errors List push button
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvSelect
IF lsError>0 
  IF !EMPTY(laScrError)
    DIMENSION laScrError[ALEN(laScrError,1)+1,ALEN(laScrError,2)]
  ENDIF
  laScrError[ALEN(laScrError,1),1] = laErrors[lsError]
  laScrError[ALEN(laScrError,1),1] = STRTRAN(laScrError[ALEN(laScrError,1),1],'["<')
  laScrError[ALEN(laScrError,1),1] = STRTRAN(laScrError[ALEN(laScrError,1),1],'>"]')  
  laScrError[ALEN(laScrError,1),2] = 1
  lsGenerror = ALEN(laScrError,1)
  lserrorPos = 1
ENDIF
lcScrMode = 'E'
SHOW GETS

*!*************************************************************************
*! Name      : lfvProces
*! Developer : Hesham El-Sheltawi 
*! Date      : 11/18/98
*! Purpose   : Valid Function for Error position List push button
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvProces
IF lsGenerror<>0 AND lserrorPos<>0
  laScrError[lsGenerror,2] = lserrorPos
ENDIF  

*!*************************************************************************
*! Name      : lfvLine
*! Developer : Hesham El-Sheltawi 
*! Date      : 11/18/98
*! Purpose   : Valid Function for  User selected errors push button
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvLine
IF lsGenerror>0
  lserrorPos = laScrError[lsGenerror,2]
ENDIF  
SHOW GET lserrorPos
