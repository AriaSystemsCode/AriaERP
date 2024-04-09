*:************************************************************************
*: Program file  : LFFILTER.PRG
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
EXTERNAL ARRAY laData,laKeyField,laField_H,laField_N
PRIVATE lcCurFile
lnCount  = 1
lcFiltExp= ""
=lfDefFieldArr()


IF TYPE('lcOldExpr') $ "UL"
  PUBLIC lcOldExpr
  lcOldExpr = ""
ENDIF  

DO lwFilter.SPR


*!**************************************************************************
*!
*!      Function : lfShowExp
*!
*!**************************************************************************
*
FUNCTION lfShowExp
IF !EMPTY(lcFiltExp)
  SHOW GET pbClear ENABLE
  SHOW GET pbSet   ENABLE
ELSE
  SHOW GET pbClear DISABLE
  SHOW GET pbSet   DISABLE
ENDIF  

*!**************************************************************************
*!
*!      Function : lfvExp
*!
*!**************************************************************************
*
FUNCTION lfvExp
IF llEmpty
  lcFiltExp=GetExpr() 
ELSE
  lcFiltExp=GetExpr(lcFiltExp,ALIAS()," ","laField_H",@laField_N)
ENDIF
IF !EMPTY(lcFiltExp)
  SHOW GET lcFiltExp
  SHOW GET pbSet ENABLE
ELSE
  SHOW GET lcFiltExp
  SHOW GET pbSet DISABLE
ENDIF
SHOW GETS
   
*!**************************************************************************
*!
*!      Function : lfvFiltExp
*!
*!**************************************************************************
*
FUNCTION lfvFiltExp
IF !EMPTY(lcFiltExp)
  IF(TYPE(lcFiltExp)="U")
    WAIT WINDOW "Not a valid expression"  NOWAIT
  ENDIF  
  SHOW GET pbSet ENABLE
ELSE
  SHOW GET pbSet DISABLE 
ENDIF           
SHOW GETS

*!**************************************************************************
*!
*!      Function : lfvAction
*!
*!**************************************************************************
*
*FUNCTION lfvAction
*DO CASE
*  CASE pbClear
*    SET FILTER TO &gcFiltExp
*  CASE pbSet
*    IF TYPE(lcFiltExp)<>"L" .OR. EMPTY(lcFiltExp)
*      WAIT WINDOW "No expression" NOWAIT
*    ELSE
*      IF EMPTY(gcFiltExp) 
*        lcFiltExp= UPPER(ALLTRIM(lcFiltExp))
*      ELSE   
*        lcFiltExp= ALLTRIM(gcFiltExp)+" .AND. "+ALLTRIM(lcFiltExp)
*      ENDIF
*      SET FILTER TO &lcFiltExp
*      GO TOP
*      IF EOF()
*        WAIT WINDOW "No Records Selected" NOWAIT
*        SET FILTER TO &gcFiltExp
*      ENDIF 
*    ENDIF
*  CASE pbCancel
*    lcOldExpr = lcFiltExp
*ENDCASE

*!**************************************************************************
*!
*!      Function : lfvClear
*!
*!**************************************************************************
*
FUNCTION lfvClear

SET FILTER TO &gcFiltExp

*!**************************************************************************
*!
*!      Function : lfvSet
*!
*!**************************************************************************
*
FUNCTION lfvSet
IF TYPE(lcFiltExp)<>"L" .OR. EMPTY(lcFiltExp)
  WAIT WINDOW "No expression" NOWAIT
ELSE
  IF EMPTY(gcFiltExp) 
    lcFiltExp= UPPER(ALLTRIM(lcFiltExp))
  ELSE   
    lcFiltExp= ALLTRIM(gcFiltExp)+" .AND. "+ALLTRIM(lcFiltExp)
  ENDIF
  SET FILTER TO &lcFiltExp
  GO TOP
  IF EOF()
    WAIT WINDOW "No Records Selected" NOWAIT
    SET FILTER TO &gcFiltExp
  ENDIF 
ENDIF

*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel

lcOldExpr = lcFiltExp
