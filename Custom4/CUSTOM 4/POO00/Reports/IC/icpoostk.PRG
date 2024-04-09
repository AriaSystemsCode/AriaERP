*:***********************************************************************************************************************
*: Procedure file: ICPOOSTK.PRG
*: C200642
*:         System: ARIA4XP
*:         Module: Inventory Control
*:         Author: Ahmed Salah
*:***********************************************************************************************************************
*! Modifications :
*! B608112,1 MMT 06/04/07 Fix bug of error while select style range 
*! while there is a Custom filter on style browse[T20070315.0008]
*! C200824,1 SSH Add three new option and some changes in layout
*:***********************************************************************************************************************

STORE '' TO lcWareName,lcNonMajPi,lcNonMajTl
lnNonMajSt=0
lnColorLen=0
=lfEvalSegs()
lnMajor  = LEN(gfItemMask('PM'))
llWAREHOUS = !EMPTY('WAREHOUS') AND USED('WAREHOUS') AND (RECCOUNT('WAREHOUS')>0)
IF llWAREHOUS 
  SELECT WAREHOUS
  GO TOP
  lcWareName=WAREHOUS.CDESC
ENDIF

SELECT *,999999999 as nCurrWip,999999999 as nWip1,999999999 as nWip2,999999999 as nWip3;
  FROM Style WHERE Style="************" INTO CURSOR TStyle  READWRITE

PRIVATE llReportByStyle,lcReportFile
llReportByStyle = .F.
STORE '' TO lcTempStyFile 
lnStyPos = ASCAN(loOgScroll.laOgfxFlt,"STYLE.CSTYMAJOR")
IF lnStyPos > 0 
  lnStyPos = ASUBSCRIPT(loOgScroll.laOgfxFlt,lnStyPos,1)
  lcTempStyFile = IIF(!EMPTY(loOgScroll.laOgFXFlt[lnStyPos,6]),loOgScroll.laOgFXFlt[lnStyPos,6],'')
  llReportByStyle = !EMPTY(lcTempStyFile) AND USED(lcTempStyFile) AND (RECCOUNT(lcTempStyFile)>0)
ENDIF 
*IF EMPTY(lcTempStyFile) 
*  lcTempStyFile = "Style"
*ENDIF
loDBFPOH = CreateObject("RemoteTable","POSHDR","POSHDR" ,"POSHDR",SET("DATASESSION"),,.T.)
loDBFPOL = CreateObject("RemoteTable","POSLN","POSLN" ,"POSLN",SET("DATASESSION"),,.T.)
loDBFPOH.SqlRun("SELECT PO,Status FROM POSHDR Where cbusdocu='P' AND cstytype='P' AND (Status='O' OR Status='H')","TPOSHDR",.T.)
loDBFPOL.SqlRun("SELECT PO,Style,TotQty,trancd,Complete FROM POSLN Where cbusdocu='P' AND cstytype='P'","TPOSLN",.T.)
IF llReportByStyle
  SELECT &lcTempStyFile.
  SCAN
    lcTempSty = substr(cStyMajor ,1,lnMajor)
    SELECT Style 
    SEEK lcTempSty
    SCAN REST WHILE cstymajor = lcTempSty
      SCATTER MEMVAR MEMO
      INSERT INTO TStyle FROM MEMVAR
      STORE 0 TO lnCurrentWip,lnWip1,lnWip2,lnWip3
      =lfGetWip(m.Style)
      SELECT tStyle
      REPLACE nWip1    WITH lnWip1,;
              nWip2    WITH lnWip2,;
              nWip3    WITH lnWip3,;
              nCurrWip WITH lnCurrentWip
    ENDSCAN
  ENDSCAN
ELSE
  SELECT Style
  SCAN
    SCATTER MEMVAR MEMO
    INSERT INTO TStyle FROM MEMVAR
    STORE 0 TO lnCurrentWip,lnWip1,lnWip2,lnWip3
    =lfGetWip(m.Style)
    SELECT tStyle
    REPLACE nWip1    WITH lnWip1,;
            nWip2    WITH lnWip2,;
            nWip3    WITH lnWip3,;
            nCurrWip WITH lnCurrentWip
  ENDSCAN
ENDIF
lcTempStyFile = "TStyle"
SELECT &lcTempStyFile.
loogScroll.cCROrientation = 'P'
INDEX ON Style TAG (lcTempStyFile)

*! C200824,1 SSH If print Zero WIP & Stk.
IF lcRpStkWip="Y"
*! C200824,1 SSH If print Zero WIP & Stk.
ELSE
  DELETE FOR (totstk=0 .AND. totwip=0)
*! C200824,1 SSH If print Zero WIP & Stk.
  *! C200824,1 SSH If print Zero WIP & Stk.
  *DELETE FOR ncurrWIP=0 AND totstk<=0 .AND. totwip<=0 .AND. totalo=0 .AND.  totord=0 .AND.  nwip1=0 .AND. nwip2=0 .AND. nwip3=0
  *! C200824,1 SSH If print Zero WIP & Stk.  
ENDIF
GO TOP
IF RECCOUNT() = 0 .OR. EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
*! C200824,1 SSH If print Zero WIP & Stk.

  DO gfDispRe WITH EVAL('lcRPFormNa')    

*! C200824,1 SSH If print Zero WIP & Stk.
ENDIF
*! C200824,1 SSH If print Zero WIP & Stk.



FUNCTION lfwRepWhen

*


FUNCTION lfStySum

PARAMETERS lcSty,lccomp,lnAddToVar

*! B608112,1 MMT 06/04/07 Fix bug of error while select style range while there is a Custom filter on style browse[Start]
*!*	PRIVATE lnStyRec
*!*	lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnoldAlias = SELECT(0)
*! B608112,1 MMT 06/04/07 Fix bug of error while select style range while there is a Custom filter on style browse[End]


STORE 0 TO lnTotcomp,lnO_T_S
SELECT Style_X
SET ORDER TO Style

IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty 
ENDIF 



*! B608112,1 MMT 06/04/07 Fix bug of error while select style range while there is a Custom filter on style browse[Start]
*SELECT Style
*GO lnStyRec
*! B608112,1 MMT 06/04/07 Fix bug of error while select style range while there is a Custom filter on style browse[End]

DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE

*! B608112,1 MMT 06/04/07 Fix bug of error while select style range while there is a Custom filter on style browse[Start]
SELECT(lnoldAlias)
*! B608112,1 MMT 06/04/07 Fix bug of error while select style range while there is a Custom filter on style browse[End]

RETURN INT(lnTotcomp)


FUNCTION lfSRSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
*    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.


FUNCTION lfGetWip
LPARAMETERS lcSty,lcMonth
PRIVATE lnOthers0,lnOthers1,lnOthers2,lnOthers3
STORE 0 TO lnOthers0,lnOthers1,lnOthers2,lnOthers3
*SSH
PRIVATE ldCompDate
*SSH
lnCurrMonth = MONTH(oAriaApplication.SystemDate)

SELECT TPOSLN
SCAN FOR Style=m.Style
  SELECT TPOSHDR 
  LOCATE FOR PO=TPOSLN.Po
  IF FOUND()
    SELECT TPOSLN
*SSH
    ldCompDate = IIF(TYPE('TPOSLN.Complete')='T',TTOD(TPOSLN.Complete),TPOSLN.Complete) + 45
*SSH
    DO CASE
      CASE BETWEEN(ldCompDate ,oAriaApplication.SystemDate,oAriaApplication.SystemDate+15)
        IF trancd='1'
          lnCurrentWip = lnCurrentWip + TotQty
        ELSE
          lnOthers0 = lnOthers0 + TotQty
        ENDIF
      CASE BETWEEN(ldCompDate ,oAriaApplication.SystemDate+16,oAriaApplication.SystemDate+30)
        IF trancd='1'
          lnWip1 = lnWip1 + TotQty
        ELSE
          lnOthers1 = lnOthers1 + TotQty
        ENDIF
      
      CASE BETWEEN(ldCompDate ,oAriaApplication.SystemDate+31,oAriaApplication.SystemDate+60)
        IF trancd='1'
          lnWip2 = lnWip2 + TotQty
        ELSE
          lnOthers2 = lnOthers2 + TotQty
        ENDIF
        
      CASE ldCompDate>oAriaApplication.SystemDate+60
        IF trancd='1'
          lnWip3 = lnWip3 + TotQty
        ELSE
          lnOthers3 = lnOthers3 + TotQty
        ENDIF
        
    ENDCASE
  ENDIF
ENDSCAN
lnWip1 = lnWip1 - lnOthers1
lnWip2 = lnWip2 - lnOthers2
lnWip3 = lnWip3 - lnOthers3
lnCurrentWip = lnCurrentWip - lnOthers0

FUNCTION lfGetClrDs
LPARAMETERS lcStyle
PRIVATE lnOldAls,lcClrDsc
lnOldAls = SELECT(0)
lcClrDsc=gfCodDes(SUBSTR(lcStyle,lnNonMajSt,lnColorLen), 'COLOR')
SELECT(lnOldAls)
RETURN(lcClrDsc)



*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Ahmed Salah
*! Date      : 11/30/1998
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
 *-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,3],lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                   PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                   lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF                     

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

  ENDFOR    && end Loop Around Non Major elements.

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
RETURN ''
*-- end of lfEvalSegs.
