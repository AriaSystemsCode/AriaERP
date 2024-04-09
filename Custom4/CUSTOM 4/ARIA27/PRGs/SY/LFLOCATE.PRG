*:************************************************************************
*: Program file  : LFLOCATE.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
PARAMETERS lcCurField

lnCount  = 1
lnCurRec = RECNO()  
lcLocExp =""

DO lwLocate.SPR

*!**************************************************************************
*!
*!      Function : lfvShowLoc
*!
*!**************************************************************************
*
FUNCTION lfShowLoc
SHOW GET pbActFind,1 DISABLE

*!**************************************************************************
*!
*!      Function : lfvActLoc
*!
*!**************************************************************************
*
*FUNCTION lfvActLoc
*lcLocExp=" "
*DO CASE
*  CASE pbActLoc = 1
*    lcLocExp=lcLocExp+lfMakeExp(TYPE(lcCurField),lcCurField) 
*    lcFindExp=ALLTRIM(IIF(cbIgnore,UPPER(lcFindExp),lcFindExp))
*    IF !EMPTY(lcLocExp)
*      IF rbScope= 3
*        GO Top
*        LOCATE FOR &lcLocExp NEXT lnCurRec
*      ELSE
*        IF rbScope = 1
*          GO TOP
*          LOCATE FOR &lcLocExp
*        ELSE
*          LOCATE FOR &lcLocExp NEXT RECCOUNT()-RECNO()
*        ENDIF
*      ENDIF  
*      IF !FOUND()
*        WAIT WINDOW "Not found" NOWAIT
*        GO lnCurRec
*      ENDIF  
*    ENDIF  
*  CASE pbActLoc = 2
*    CLEAR READ
*ENDCASE

*!**************************************************************************
*!
*!      FUNCTION :  lfMakeExp
*!
*!**************************************************************************
*
FUNCTION lfMakeExp
PARAMETERS CTYPE,CFIELD
lcRet=""
** Making the Expression **
DO CASE
  CASE CTYPE="N"
  lcRet="lcFindExp"+IIF(cbMatch," = "," $ ")+IIF(cbIgnore,;
                  "UPPER(ALLTRIM(STR("+CFIELD+")))",";
                  ALLTRIM(STR("+CFIELD+"))")
  CASE CTYPE="D"
  lcRet="lcFindExp"+IIF(cbMatch," = "," $ ")+IIF(cbIgnore,;
                  "UPPER(ALLTRIM((DTOC("+CFIELD+")))",";
                  ALLTRIM(DTOC("+CFIELD+"))")
  CASE CTYPE="C" 
  lcRet="lcFindExp"+IIF(cbMatch," = "," $ ")+IIF(cbIgnore,;
                  "UPPER(ALLTRIM("+CFIELD+"))",";
                  ALLTRIM("+CFIELD+")")
ENDCASE
RETURN lcRet
*!**************************************************************************
*!
*!      Function : lfvLocate
*!
*!**************************************************************************
*
FUNCTION lfvLocate
lcLocExp=" "
lcLocExp=lcLocExp+lfMakeExp(TYPE(lcCurField),lcCurField) 
IF USED('CODES')
  lcBefCode=SELECT(0) 
  SELECT CODES 
  SET ORDER TO 1
  lnCodRec=IIF(RECNO()>RECCOUNT(),0,RECNO())
  IF SEEK(UPPER(lcCurField))
    GO TOP
    LOCATE FOR CFLD_NAME=UPPER(lcCurField) AND;
               cdiscrep =ALLTRIM(IIF(cbIgnore,UPPER(lcFindExp),lcFindExp))
               
     IF FOUND()
       lcFindExp=cCode_No
     ENDIF
  ENDIF   
  IF lnCodRec<>0
      GO lnCodRec
  ENDIF
  SELECT (lcBefCode)
ENDIF
  lcFindExp=ALLTRIM(IIF(cbIgnore,UPPER(lcFindExp),lcFindExp))

IF !EMPTY(lcLocExp)
  IF rbScope= 3
    GO Top
    LOCATE FOR &lcLocExp NEXT lnCurRec
  ELSE
    IF rbScope = 1
      GO TOP
      LOCATE FOR &lcLocExp
    ELSE
      LOCATE FOR &lcLocExp NEXT RECCOUNT()-RECNO()
    ENDIF
  ENDIF  
  IF !FOUND()
    WAIT WINDOW "Not found" NOWAIT
    GO lnCurRec
  ENDIF  
ENDIF  
*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel
lcLocExp=" "
CLEAR READ
