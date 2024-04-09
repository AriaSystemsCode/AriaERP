****************************************************************************
*: Program file      : ICFPCTSD.PRG
*: Program desc.     : Custom Cut & Sold report                  
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : TAK
*: Date              : 10/25/2000
*:**************************************************************************

*--Set releations
SELECT ORDLINE
SET RELATION TO cOrdType+Order INTO ORDHDR
SELECT INVLINE
SET RELATION TO Invoice INTO INVHDR
SELECT CUTTKTL
SET RELATION TO Cuttkt INTO CUTTKTH


*--If optional Grid elements was changed.
IF llOGFltCh

  *-- Check for style major was selected.
  llStySel  = .F.
  lcStyFile = ''
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
  IF lnPosition > 0
    lcStyFile = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcStyFile) AND USED(lcStyFile)
      SELECT (lcStyFile)
      GOTO TOP
      llStySel = !EOF()
    ENDIF  
  ENDIF


  *--Style file filter.
  lcStyFiltr = ".T."
  
  *--Check for groups
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
  IF lnPosition > 0 AND !EMPTY(laOGFxFlt[lnPosition,6])
    lcGroups = laOGFxFlt[lnPosition,6]
    lcStyFiltr = lcStyFiltr + " AND STYLE.CSTYGROUP $ '"+lcGroups+"'"
  ENDIF
  *--Check for Season
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.SEASON'),1)
  IF lnPosition > 0 AND !EMPTY(laOGFxFlt[lnPosition,6])
    lcSeasons = laOGFxFlt[lnPosition,6]
    lcStyFiltr = lcStyFiltr + " AND STYLE.SEASON $ '"+lcSeasons+"'"
  ENDIF

  *-- Check for primary fabrics was selected.
  llFabSel  = .F.
  lcFabFile = ''
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.FABRIC'),1)
  IF lnPosition > 0
    lcFabFile = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcFabFile) AND USED(lcFabFile)
      SELECT (lcFabFile)
      GOTO TOP
      llFabSel = !EOF()
    ENDIF  
  ENDIF
  IF llFabSel
    lcStyFiltr = lcStyFiltr + " AND SEEK(STYLE.FABRIC,lcFabFile)"
  ENDIF

  *--Order file filter.
  lcOrdFiltr = ".T."
  lcInvFiltr = ".T."
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)
  IF lnPosition > 0 AND !EMPTY(SUBSTR(laOGFxFlt[lnPosition,6],1,2))
    =ACOPY(laOGFxFlt,laOrdFlt,1,7)
    DIME laOrdFlt[1,7]
    lcOrdFiltr = gfGenFlt('laOrdFlt',.T.)
    lcOrdFiltr = IIF(EMPTY(lcOrdFiltr),".T.",lcOrdFiltr) 
    lcInvFiltr = IIF(EMPTY(lcOrdFiltr),".T.",STRTRAN(lcOrdFiltr,'ORDLINE.START','INVLINE.INVDATE')) 
  ENDIF


  *--Cut file filter.
  lcCutFiltr = ".T."
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.ENTERED'),1)
  IF lnPosition > 0 AND !EMPTY(SUBSTR(laOGFxFlt[lnPosition,6],1,2))
    =ACOPY(laOGFxFlt,laCutFlt,8,14)
    DIME laCutFlt[1,7]
    lcCutFiltr = gfGenFlt('laCutFlt',.T.)
    lcCutFiltr = IIF(EMPTY(lcCutFiltr),".T.",lcCutFiltr)
  ENDIF

  *--Create a temp file.
  IF USED(lcTmpStyle)
    USE IN (lcTmpStyle)
  ENDIF  
  SELECT STYLE
  SET ORDER TO TAG Style
  COPY STRU TO (gcWorkDir+lcTmpStyle)
  =gfOpenFile(gcWorkDir+lcTmpStyle,'','EX')
  INDEX ON Style TAG (lcTmpStyle)


  *--Start collecting records to print.
  IF llStySel
    SELECT (lcStyFile)
    SCAN
      llAllDone = .F.
      =SEEK(PADR(cStyMajor,12),'STYLE')
      SELECT STYLE
      SCAN WHILE Style = PADR(&lcStyFile..cStyMajor,12) FOR &lcStyFiltr
        WAIT WINDOW 'Collecting Style:'+ALLT(STYLE.Style)+' Information...' NOWAIT
        =lfCollect()
      ENDSCAN
    ENDSCAN
  ELSE
    lcOldSty=" * "
    SELECT STYLE
    SCAN FOR &lcStyFiltr
      llAllDone = (PADR(Style,12) = lcOldSty)
      WAIT WINDOW 'Collecting Style:'+ALLT(STYLE.Style)+' Information...' NOWAIT
      =lfCollect()
      lcOldSty = PADR(Style,12)
    ENDSCAN
  ENDIF  
  WAIT CLEAR

ENDIF  && optional grid changed.


*--Read the date range.
lcDateRng = ""
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)
IF lnPosition > 0 AND !EMPTY(SUBSTR(laOGFxFlt[lnPosition,6],1,2))
   lcDateRng = 'Sold Dates:'+STRTRAN(laOGFxFlt[lnPosition,6],'|',' To ')
ENDIF
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.ENTERED'),1)
IF lnPosition > 0 AND !EMPTY(SUBSTR(laOGFxFlt[lnPosition,6],1,2))
   lcDateRng = IIF(EMPTY(lcDateRng),"",lcDateRng+',   ') + 'Cut Dates:'+STRTRAN(laOGFxFlt[lnPosition,6],'|',' To ')
ENDIF


SELECT (lcTmpStyle)
GO TOP  
IF !EOF()
  IF llOutExl  
    *-- To generate the excel file
    IF !EMPTY(lcRpExlFil)
      =lfGenExlFl()
    ENDIF  
  ELSE
    = gfDispRe(lcRpForm)
  ENDIF
ELSE
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF


SELECT STYLE
SET ORDER TO TAG Cstyle

SELECT ORDLINE
SET RELATION TO
SELECT INVLINE
SET RELATION TO
SELECT CUTTKTL
SET RELATION TO

RETURN


*!*************************************************************
*! Name      : lfCollect
*! Developer : TAK
*! Date      : 10/25/2000 
*! Purpose   : Collect data.
*!*************************************************************
FUNCTION lfCollect

lnAlias = SELECT()
*--Collect Ordered.
SELECT ORDLINE
=SEEK(STYLE.Style)
SCAN REST WHILE Style = STYLE.Style FOR ORDHDR.Status $ 'OH' AND &lcOrdFiltr
  WAIT WINDOW 'Collecting Style:'+ALLT(STYLE.Style)+' Ordered...' NOWAIT
  =lfUpdSty(PADR(Style,12),'ORD',TotQty)
ENDSCAN

*--Collect Invoiced.
SELECT INVLINE
=SEEK(STYLE.Style)
SCAN REST WHILE Style = STYLE.Style FOR INVHDR.Status <> 'V' AND &lcInvFiltr
  WAIT WINDOW 'Collecting Style:'+ALLT(STYLE.Style)+' Invoiced...' NOWAIT
  =lfUpdSty(PADR(Style,12),'INV',TotQty)
ENDSCAN

*--Collect Cut for style component.
lcStyComp = IIF(SEEK(STYLE.cStyMajor+'4','BOM'),BOM.Item,"")
IF !EMPTY(lcStyComp) AND ! llAllDone
  SELECT CUTTKTL
  =SEEK(lcStyComp)
  SCAN REST WHILE Style = lcStyComp FOR CUTTKTH.Status <> 'X' AND TranCd='1' AND &lcCutFiltr
    WAIT WINDOW 'Collecting Style:'+ALLT(lcStyComp)+' Cut...' NOWAIT
    =lfUpdSty(PADR(STYLE.cStyMajor,12),'CUT',TotQty)
  ENDSCAN
  llAllDone = .T.
ENDIF
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name      : lfUpdSty
*! Developer : TAK
*! Date      : 10/25/2000 
*! Purpose   : Update temp file.
*!*************************************************************
FUNCTION lfUpdSty
PARA lcStMaj, lcType , lnTotQty

SELECT (lcTmpStyle)
IF ! SEEK(lcStMaj,lcTmpStyle)
  APPEND BLANK
  REPLACE Style WITH lcStMaj,;
          Desc  WITH STYLE.Desc
ENDIF
DO CASE
  CASE lcType = 'ORD'
    REPLACE TotOrd WITH TotOrd + lnTotQty
  CASE lcType = 'INV'
    REPLACE TotShp WITH TotShp + lnTotQty
  CASE lcType = 'CUT'
    REPLACE TotWip WITH TotWip + lnTotQty
ENDCASE
RETURN


*!*************************************************************
*! Name      : lfvOutPut
*! Developer : TAK
*! Date      : 10/25/2000 
*! Purpose   : Validate output to excel OG element.
*!*************************************************************
FUNCTION lfvOutPut

IF llOutExl
  lcRpExlFil =PUTFILE('Excel File:',lcRpExlFil, 'XLS')
ENDIF  


*!*************************************************************
*! Name      : lfGenExlFl
*! Developer : TAK
*! Date      : 10/25/2000 
*! Purpose   : To generate a generic Excel file.
*!*************************************************************
*! Example     : = lfGenExlFl()
*!*************************************************************
FUNCTION lfGenExlFl

lcTmpCur = gfTempName()
SELECT (lcTmpStyle)
SELECT Style,Desc,TotOrd AS 'Ordered',TotShp AS 'Invoiced', TotOrd+TotShp AS 'Tot_Sold',;
       TotWip AS 'Cut',(totwip*0.9)-(totord+totshp) AS 'Diff' FROM (lcTmpStyle) INTO CURSOR (lcTmpCur)
SELECT (lcTmpCur)
GO TOP
EXPORT TO (lcRpExlFil) XLS   
=gfModalGen("TRM000000B00000","DIALOG",'','','Export to Excel has been completed successfully.')
USE IN (lcTmpCur)
RETURN




*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
FUNCTION lfClearRep

IF USED(lcTmpStyle)
  SELECT (lcTmpStyle)
  USE
  ERASE(gcWorkDir+lcTmpStyle+'.DBF')
  ERASE(gcWorkDir+lcTmpStyle+'.CDX')
ENDIF

*-- End of lfClearRep.
