*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLUBATRN.PRG
*:  Module      : General Ledger
*:  Desc.       :  Unposted batches\transactions
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303249,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*E610601,1 TMI 11/26/2013 Disable updating the field DBATELDAT [T20131118.0005] 
*:************************************************************************
*** Report Setup
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#include r:\aria4xp\reports\gl\glubatrn.h
*N000682,1 MMT 02/10/2013 Globalization changes[End]
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
lcAdd2Exp = "DTOS(glbatch.dbateldat)+ glbatch.cbateltim > DTOS(glbatch.dadd_date)+glbatch.cadd_time"
IF !UPPER(CHRTRAN(lcAdd2Exp,' ','')) $ UPPER(CHRTRAN(lcRpExp,' ',''))
  lcRpExp = lcRpExp + ' AND '+lcAdd2Exp + '=.F.'
ENDIF

SELECT GLBATCH
DO CASE
  CASE lcRpGroup == 'cBatType'
    SET ORDER TO TAG BATTYPE
  CASE lcRpGroup == 'cBatStat'
    SET ORDER TO TAG BATSTAT
  OTHERWISE
    SET ORDER TO TAG BATCHNO
ENDCASE
SET ORDER TO TAG BATCHTRN IN GLTRNSHD
SET RELATION TO cbatchno INTO GLTRNSHD ADDITIVE
IF lcRpForm = "GLUBATRS"
  SET SKIP TO GLTRNSHD
ELSE
  SELECT GLTRNSHD
  SET ORDER TO TAG BATCHTRN IN GLTRNSDT
  SET ORDER TO TAG ACCTCODE IN GLACCHAR
  SET RELATION TO cbatchno + ctranno INTO GLTRNSDT ADDITIVE
  SELECT GLTRNSDT
  SET RELATION TO cacctcode INTO GLACCHAR ADDITIVE
  SELECT GLBATCH
  SET SKIP TO GLTRNSDT, GLTRNSHD
ENDIF

lfCollect()
*- data collection
SELECT (lcTmpFile)
SET RELATION TO cbatchno INTO glbatch
DO gfDispRe WITH EVAL('lcRpForm')

SET RELATION TO
SET SKIP TO
IF lcRpForm = "GLUBATRD"
  SELECT GLTRNSHD
  SET RELATION TO
  SELECT GLTRNSDT
  SET RELATION TO
ENDIF

SELECT GlBatch
*E610601,1 TMI 11/26/2013 21:08 [Start] disable the update of the field DBATELDAT
*!*	IF !EMPTY(ALLTRIM(lcRpExp)) AND loogscroll.ll2printer
*!*	  SCAN FOR &lcRpExp
*!*	    =RLOCK()
*!*	    REPLACE cbatElUsr WITH gcUser_ID ,;
*!*	            dBatElDat WITH DATE()    ,;
*!*	            cBatElTim WITH gfGetTime()
*!*	    UNLOCK
*!*	  ENDSCAN
*!*	ENDIF
*E610601,1 TMI 11/26/2013 21:09 [End  ] 

RETURN

************************************************************
*! Name      : lfCollect
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/03/2012
*! Purpose   : Collect data
************************************************************
FUNCTION lfCollect
PARAMETERS lcFile
lcFile = IIF(EMPTY(lcFile),ALIAS(),lcFile)

LOCAL lnSlct
lnSlct = SELECT(0)

IF USED(lcTmpFile)
  SELECT (lcTmpFile)
  ZAP
ELSE
  LOCAL laStru[20,4],lnLen,i

  lnI = 0
  lnI = lnI + 1
  laStru[lnI,1] = "cbatchno"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 6
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "cbatstat"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 1
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "cSrcModul"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 1
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "cBatType"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 1
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CTRANNO"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CACCTCODE"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 24
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CACCNSDES"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 65
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CTRDTEXP"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 40
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CDRORCR"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 1
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "NAMOUNT"
  laStru[lnI,2] = "N"
  laStru[lnI,3] = 15
  laStru[lnI,4] = 2

  lnI = lnI + 1
  laStru[lnI,1] = "CTRNDESC"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 40
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CTRNREFER"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 15
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "DTRNPDATE"
  laStru[lnI,2] = "D"
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CTRNTYPE"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 1
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "DTRNREVDT"
  laStru[lnI,2] = "D"
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "NTRNTOTDR"
  laStru[lnI,2] = "N"
  laStru[lnI,3] = 15
  laStru[lnI,4] = 2

  lnI = lnI + 1
  laStru[lnI,1] = "NTRNTOTCR"
  laStru[lnI,2] = "N"
  laStru[lnI,3] = 15
  laStru[lnI,4] = 2

  lnI = lnI + 1
  laStru[lnI,1] = "CSRCJRNL"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 2
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "AUTNOR"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 15
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CRPNAME"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) FROM ARRAY laStru

ENDIF

SELECT &lcFile
lcRel = SET("Relation")
SELECT (lcTmpFile)

SELECT &lcFile
lcFor = IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
SCAN &lcFor
  m.cbatchno = cbatchno
  m.cbatstat = cbatstat
  m.cSrcModul = cSrcModul
  m.cBatType = cBatType

  m.cacctcode = IIF(EOF('GLTRNSDT'),' ',GLACCHAR.cacctcode)
  m.caccnsdes = IIF(EOF('GLTRNSDT'),' ',GLACCHAR.caccnsdes)
  m.cdrorcr = GLTRNSDT.cdrorcr
  m.namount = GLTRNSDT.namount
  m.ctrdtexp = IIF(EOF('GLTRNSDT'),' ',GLTRNSDT.ctrdtexp)
  m.ctranno = IIF(cbatstat='E',' ',GLTRNSHD.ctranno)
  m.ctrntype = GLTRNSHD.ctrntype
  m.AUTNOR = IIF(EOF('GLTRNSHD'),' ',IIF(cbatstat='E','',IIF(GLTRNSHD.ctrntype='A','Automatic','Normal')))
  m.csrcjrnl = IIF(EOF('GLTRNSHD'),' ',IIF(cbatstat='E',' ',GLTRNSHD.csrcjrnl))
  m.CRPNAME = IIF(EOF('GLTRNSHD'),' ',lfRpName(cbatstat))
  m.ctrnrefer = IIF(EOF('GLTRNSHD'),' ',IIF(cbatstat='E',' ',GLTRNSHD.ctrnrefer))
  M.dtrnpdate = IIF(EOF('GLTRNSHD'),{},IIF(cbatstat='E',{},GLTRNSHD.dtrnpdate))
  m.ctrndesc = IIF(EOF('GLTRNSHD'),' ',IIF(cbatstat='E',' ',GLTRNSHD.ctrndesc))
  m.dtrnrevdt = IIF(EOF('GLTRNSHD'),{},IIF(cbatstat='E',{},GLTRNSHD.dtrnrevdt))
  m.ntrntotdr = IIF(EOF('GLTRNSHD'),0,IIF(cbatstat='E',0,GLTRNSHD.ntrntotdr))
  m.ntrntotcr = IIF(EOF('GLTRNSHD'),0,IIF(cbatstat='E',0,GLTRNSHD.ntrntotcr))
  INSERT INTO (lcTmpFile) FROM MEMVAR
ENDSCAN

SELECT (lnSlct)
*- End of lfCollect.


*!************************************************************************
*!
*!      FUNCTION lfChngCond
*!
*!************************************************************************
***  Function to switch between two FRX reports
FUNCTION lfChngCond
DO CASE
  CASE lcRpForm =   "GLUBATRD"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start]
*      =lfChangeGrid('GLUBATR2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ]
  CASE lcRpForm =   "GLUBATRS"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start]
*      =lfChangeGrid('GLUBATRN')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ]
ENDCASE
ClearRead()

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep



*!************************************************************************
*!
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue

RETURN IIF(EMPTY(lcRpValue) OR !(lcRpValue $ 'NSBLZEOUAVPYH'),'',SUBSTR(lcRpVldEnt,;
                  ATC('|',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))+1,;
                 (ATC('|',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')+1)-1)-;
                 (ATC('|',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')))))

*!************************************************************************
*!
*!      FUNCTION lfvBatCode
*!
*!************************************************************************
*
FUNCTION lfvBatCode

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF
lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*lcBrFields    = 'CBATCHNO:H="Code",CBatDesc:H="Description"'
lcBrFields    = 'CBATCHNO:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CODE,oAriaApplication.GetHeaderText("LANG_CODE",AHEADERFILE))+;
'",CBatDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESC,oAriaApplication.GetHeaderText("LANG_DESC",AHEADERFILE))+'"'
*N000682,1 MMT 02/10/2013 Globalization changes[End]
laRpRetFld[1] = ''

IF !EMPTY(EVAL(lcRpCurFld))
  &lcRpCurFld=trans(VAL(EVAL(lcRpCurFld)),'@l 999999')
ENDIF

SELECT GLBATCH
SET ORDER TO TAG BATCHNO
IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  SET ORDER TO TAG BATSTAT
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  *=gfBrows([FOR ALLTRIM(CBATSTAT) $ 'EOUHA'],'CBATCHNO',"laRpRetFld",'Batches Codes ',.F.)
  =gfBrows([FOR ALLTRIM(CBATSTAT) $ 'EOUHA'],'CBATCHNO',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHCODETTL,oAriaApplication.GetHeaderText("LANG_BATCHCODETTL",AHEADERFILE)),.F.)
  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  if Empty(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  endif
  loOgScroll.&lcRpCurFld = laRpRetFld[1]
ENDIF
SET ORDER TO

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE

*- End of lfRepWhen.
