*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLTRABAL.PRG
*:  Module      : General Ledger
*:  Desc.       : Trial Balance 
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 08/30/2012
*:  Reference   : E303224
*:************************************************************************

lcRpAddExp = " ABS(GLACBALS.NACBOPBAL) + ABS(GLACBALS.NACBCLBAL) +"+;
             " GLACBALS.NACBPTDDR + GLACBALS.NACBPTDCR > 0"
lcPrExp = lcRpExp
IF !EMPTY(laOGFxFlt[3,6])             
  lcPrExp    = STRTRAN(lcRpExp,[ GLGRPDT.CGRPCODE = "]+laOGFxFlt[3,6]+'"',[ !EOF('GLGRPDT')]) 
ENDIF  
  lcPrExp    = IIF(lcRpZerAct='N',lcPrExp + ' AND '+ lcRpAddExp,;
                   lcPrExp) 
                                 
* if the user entered account code mask then change the expression
* to use the like function or if the user entered a complete 
* account code then leave  the expression using the cacctcode= value
lcMaskExp = lfSegExp()
IF !EMPTY(STRTRAN(laOGFxFlt[4,6],'-'))
  IF ATC(' ',laOGFxFlt[4,6]) > 0
    lcPrExp    = STRTRAN(lcPrExp,[ GLACBALS.CACCTCODE = "]+laOGFxFlt[4,6]+'"',lcMaskExp)
  ENDIF  
ELSE  
  lcPrExp    = STRTRAN(lcPrExp,[AND  GLACBALS.CACCTCODE = "]+laOGFxFlt[4,6]+'"','') 
ENDIF  

lnOldAlias = SELECT()
SELECT GLACBALS
=lfSetRel()

lnRpColWid=18

lnRPTempCol=lnRpColWid
lnRpColWid=lnRpColWid -LEN(SET('CURR',1))

lnRpColDis=VAL(lcRpDecPla)
lcRPTemPic=REPL('?',lnRpColWid)
lcRPTemPic=IIF(lnRpColDis> 0,STRTRAN(lcRpTemPic,'?','.',lnRpColWid-lnRpColDis,1),lcRPTemPic)
lcRpColPic=''
lnNoDecPoint = IIF(lnRpColDis>0,lnRpColWid-lnRpColDis-1,lnRpColWid)
FOR lnColCount = 1 TO (lnNoDecPoint)-MOD(lnNoDecPoint,4)
  lcRpColPic=lcRpColPic+IIF(MOD(lnColCount,4)=1,',','9')
ENDFOR
lcRpColPic=PADL(lcRpColPic,lnNoDecPoint,'9')
lcRpColPic=STUFF(lcRPTemPic,1,LEN(lcRpColPic),lcRpColPic)
lcRpColPic='@$ '+STRTRAN(lcRpColPic,'?','9')

lnRpColWid = lnRPTempCol


SELECT FSPRD

= SEEK(ALLTRIM(laOGFxFlt[1,6])+RIGHT(laOGFxFlt[2,6],2)))
ldYearPrd = DFSPPENDT

=lfDataCollect()

lnPrdCnt = val(right(laOgFxFlt[2,6],2))-val(left(laOgFxFlt[2,6],2))+1
DO gfDispRe WITH EVAL('lcRpForm')

=lfClrRel()
SELECT (lnOldAlias)  
glEscPrs = .F.    



************************************************************
*! Name      : lfDataCollect
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/30/2012
*! Purpose   : Collect data for the report 
************************************************************
FUNCTION lfDataCollect
LOCAL lcKey
=lfCrtTmp()

SELECT GLACBALS
SCAN FOR &lcPrExp
  SCATTER MEMVAR MEMO
  INSERT INTO &lcTempFile FROM MEMV
  SELECT &lcTempFile 
  REPLACE BBal WITH lfGetCur(nacbopbal/VAL(lcRpRound),lnRpColWid,VAL(lcRpDecPla)) ;
          DBr  WITH lfGetCur(nacbptddr/VAL(lcRpRound),lnRpColWid,VAL(lcRpDecPla)) ;
          CDr  WITH lfGetCur(nacbptdcr/VAL(lcRpRound),lnRpColWid,VAL(lcRpDecPla)) ;
          CBal WITH lfGetCur(nacbclbal+lfCloseAmnt()/VAL(lcRpRound),lnRpColWid,VAL(lcRpDecPla))
ENDSCAN 

SELECT GLACBALS
lcKey = KEY()

SELECT (lcTempFile)
SET RELATION TO &lcKey INTO GLACBALS


*- End of lfDataCollect.
************************************************************
*! Name      : lfCrtTmp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/30/2012
*! Purpose   : Create temp file for data collection 
************************************************************
FUNCTION lfCrtTmp
LOCAL laStru
IF USED(lcTempFile)
  SELECT (lcTempFile)
  ZAP
ELSE  
  SELECT GLACBALS
  DIMENSION laStru[1,18] 
  =AFIELDS(laStru)
  lnLen = ALEN(laStru,1)
  DIMENSION laStru[lnLen+4,18]
  laStru[lnLen+1,1] = 'BBal'
  laStru[lnLen+1,2] = 'C'
  laStru[lnLen+1,3] = FSIZE('nacbopbal')
  laStru[lnLen+1,4] = 0
  
  laStru[lnLen+2,1] = 'DBr'
  laStru[lnLen+2,2] = 'C'
  laStru[lnLen+2,3] = FSIZE('nacbptddr')
  laStru[lnLen+2,4] = 0
  
  laStru[lnLen+3,1] = 'Cdr'
  laStru[lnLen+3,2] = 'C'
  laStru[lnLen+3,3] = FSIZE('nacbptdcr')
  laStru[lnLen+3,4] = 0
  
  laStru[lnLen+4,1] = 'CBal'
  laStru[lnLen+4,2] = 'C'
  laStru[lnLen+4,3] = FSIZE('nacbclbal')
  laStru[lnLen+4,4] = 0 
  
  FOR lnI = lnLen+1 TO ALEN(laStru,1)
    FOR lnJ = 7 TO 16
      laStru[lnI,lnJ] = ''
    ENDFOR
    STORE 0 TO laStru[lnI,17],laStru[lnI,18]
  ENDFOR
  CREATE TABLE (oAriaApplication.WorkDir+lcTempFile) FROM ARRAY laStru
  INDEX on CACCTCODE+CFISFYEAR+CFSPPRDID TAG ACCYRPRD
  INDEX on CFISFYEAR+CFSPPRDID TAG FISFYEAR   
  SET ORDER TO TAG (ORDER('glacbals'))
ENDIF 
*- End of lfCrtTmp.

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION lfSetRel
*!
*!************************************************************************
*
FUNCTION lfSetRel
SELECT GLACBALS
SET RELATION TO CACCTCODE INTO GLACCHAR ADDITIVE

SELECT glacchar
SET RELATION TO glacchar.ctypecode INTO GLTYPES ADDITIVE

IF !EMPTY(loOgScroll.laOGFxFlt[3,6])
  SET RELATION TO laOGFxFlt[3,6]+ glacbals.cacctcode INTO GLGRPDT ADDITIVE
ENDIF  

SELECT GLACBALS

*!************************************************************************
*!
*!      FUNCTION lfClrRel
*!
*!************************************************************************
*
FUNCTION lfClrRel

SELECT GLACCHAR
SET RELATION TO 
SELECT (lcTempFile)
SET RELATION TO


*!************************************************************************
*!
*!      Function lfvFisYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvFisYer
loFld = _Screen.ActiveForm.ActiveControl
DECLARE laRpRetFld(1)
*lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

lcRpFicsYr = loFld.Value

lcRpCurFld = VARREAD()
IF .NOT.EMPTY(lcRpFicsYr)  
  lcOldAlias = SELECT()    && Save the current alias  
  SELECT FISHD
  
  IF ('?' $ lcRpFicsYr .OR. !SEEK(lcRpFicsYr)) 
    =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
    loFld.Value = laRpRetFld[1]

    loOgScroll.laOgFxFlt[loOgScroll.lnFisYrPos,6] = laRpRetFld[1]
  ENDIF
  
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvFisPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvFisPrd
loFld = _Screen.ActiveForm.ActiveControl

DECLARE laRpRetFld(1)
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''

lcRpFicsYr = loOgScroll.laOgFxFlt[loOgScroll.lnFisYrPos,6]
IF .NOT. EMPTY(lcRpFicsYr) 
  IF .NOT.EMPTY(ALLTRIM(loFld.Value))
    lcOldAlias = SELECT()    && Save the current alias
    
    SELECT FSPRD    
    IF ('?' $ loFld.Value .OR.;  
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(loFld.Value)))
      =gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)

      loFld.Value = laRpRetFld[1]
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ELSE
  *ð
  *00274
  gfModalGen('INM00274B00000','DIALOG','Fiscal year should be entered')
  loFld.Value = ''    
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvGrpCode
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
Function lfvGrpCode
loFld = _Screen.ActiveForm.ActiveControl
DECLARE laRpRetFld(1)

lcBrFields    = 'CGrpCode:H="Code",CGrplnHed:H="Description"'
laRpRetFld[1] = ''
&& Check If year field is empty
IF .NOT. EMPTY(loFld.Value) 
  lcOldAlias = ALIAS()    && Save the current alias
  SELECT GLGRPHD
  *** Search for the current Group code
  IF ('?' $ loFld.Value .OR.;  
      !SEEK(ALLTRIM(loFld.Value)))
        =gfBrows([],'CGrpCode',"laRpRetFld",'Group Codes ',.F.)
      loFld.Value = laRpRetFld[1]
      
      loOgScroll.laOgFxFlt[loOgScroll.lnGrpCodPos,6] = laRpRetFld[1]
    ENDIF
    IF NOT EMPTY(lcOldAlias)
      SELECT (lcOldAlias)
    ENDIF    
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfSegExp
*!
*!************************************************************************
FUNCTION lfSegExp

IF !EMPTY(STRTRAN(laOGFxFlt[4,6],'-'))
lcRpSegMas = IIF(EMPTY(ALLTRIM(laOGFxFlt[4,6])),;
              STRTRAN(STRTRAN(lcRpPic, 'X', '?'),'9','?'),;
              STRTRAN(PADR(laOGFxFlt[4,6],LEN(GLACBALS.cacctCode)), ' ', '?'))
ELSE 
  RETURN ''              
ENDIF              
RETURN [ LIKE("]+lcRpSegMas+[",GLACBALS.cacctCode)]

*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias

SELECT ACCOD
GO TOP

IF EOF()
  lcRpSegMak = " "
  
ELSE
  lcRpSegMak = ALLTRIM(ACCOD.cacsmask)
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  
  lcRpSegMak = STRTRAN(lcRpSegMak, '#', '9',2) 
  lcRpSegMak = STRTRAN(lcRpSegMak, '#', 'X',1,1) 

ENDIF

IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMak

*!************************************************************************
*!
*!      FUNCTION lfWGLTraial
*!
*!************************************************************************
*
FUNCTION lfWGLTraia
IF EMPTY(laOGFxFlt[1,6])
  loOgScroll.laOGFxFlt[1,6] = lfGetCurYr()
ENDIF

IF EMPTY(laOGFxFlt[2,6])
  lcElmSep = '|'
  laOGFxFlt[2,6] = lfGetCurPD()
  laOGFxFlt[2,6] = laOGFxFlt[2,6]+lcElmSep+laOGFxFlt[2,6]
  loOgScroll.laOGFxFlt[2,6] = laOGFxFlt[2,6]
ENDIF

loOgScroll.AddProperty('lnFisYrPos',lfGetPos('LAOGFXFLT','GLACBALS.CFISFYEAR'))
loOgScroll.AddProperty('lnFisPrdPos',lfGetPos('LAOGFXFLT','GLACBALS.CFSPPRDID'))
loOgScroll.AddProperty('lnAcCodPos',lfGetPos('LAOGFXFLT','GLACBALS.CACCTCODE'))
loOgScroll.AddProperty('lnGrpCodPos',lfGetPos('LAOGFXFLT','GLGRPDT.CGRPCODE'))


*!************************************************************************
*!
*!      FUNCTION lfChkValue
*!
*!************************************************************************
*
FUNCTION lfChkValue


IF (EMPTY(ALLTRIM(laOGFxFlt[1,6]))  .OR.  lfEmptyPrd())
  ** NO recoeds hove been collected
  =gfModalGen("INM02219B00000","DIALOG")
  RETURN .F.
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfGetCurYr
*!
*!************************************************************************
*
FUNCTION lfGetCurYr

llCompUsd=.f.
lcRetVal = '    '
gcSyshome = oAriaApplication.SysPath
IF !USED('SYCCOMP')
  gfOpenFile(oAriaApplication.SysPath+"SYCCOMP","CCOMP_ID","SH")
  llCompUsd=.T.
ENDIF
SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER
ENDIF
IF llCompUsd
  USE IN SYCCOMP
ENDIF
RETURN lcRetVal

*!************************************************************************
*!
*!      FUNCTION lfGetCurYr
*!
*!************************************************************************
*
FUNCTION lfGetCurPD

llCompUsd=.f.
lcRetVal='  '
gcSyshome = oAriaApplication.SysPath
IF !USED('SYCCOMP')
  USE &gcSyshome.SYCCOMP IN SELECT(1)
  llCompUsd=.T.
ENDIF
SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_PRD
ENDIF
IF llCompUsd
  USE IN SYCCOMP
ENDIF
RETURN lcRetVal

*!************************************************************************
*!
*!      FUNCTION lfdum
*!
*!************************************************************************
*
FUNCTION lfdum
PARAMETERS lnPrdToDisp

lnStrtPrd = VAL(SUBSTR(laOGFxFlt[2,6],1,2))
lnEndPrd  = VAL(RIGHT(laOGFxFlt[2,6],2))
lcRetVal=''
FOR lnCount = 1 TO lnEndPrd-lnStrtPrd+1 
  lnPrdToDisp=lnCount 
  lcStrtPrd = SUBSTR(laOGFxFlt[2,6],1,2)
  lcEndPrd  = RIGHT(laOGFxFlt[2,6],2)
  lcTotVar  = 'lnTotPr'+ALLTRIM(STR(lnStrtPrd+lnPrdToDisp-1,2))
  lcRetVal = lcRetVal+'Pr '+STR(lnStrtPrd+lnPrdToDisp-1,2)+':'+STR(&lcTotVar)+CHR(13)+CHR(10)
ENDFOR
WAIT lcRetVal WINDOW
RETURN lcRetVal

*!*********************************************************************************
*!
*!             FUNCTION : lfTotPriod
*!
*!*********************************************************************************
*
FUNCTION lfTotPriod
PARAMETERS lnPrdToDisp

lnStrtPrd = VAL(SUBSTR(laOGFxFlt[2,6],1,2))
lcRetVal = 'Period '+STRTRAN(STR(lnStrtPrd+lnPrdToDisp-1,2),' ','0')+': '
RETURN lcRetVal

*!*********************************************************************************
*!
*!             FUNCTION : lfTotBBal
*!
*!*********************************************************************************
* 
FUNCTION lfTotBBal
PARAMETERS lnPrdToDisp,lcRepVar

lnStrtPrd = VAL(SUBSTR(laOGFxFlt[2,6],1,2))
lcRetVal =  EVAL(lcRepVar+ALLTRIM(STR(lnStrtPrd+lnPrdToDisp-1,2)))
RETURN lcRetVal

*!*********************************************************************************
*!
*!        FUNCTION : lfRpAlltrim
*!
*!*********************************************************************************
*
FUNCTION lfRpAlltrim
PARAMETERS lcVarName

&lcVarName=ALLTRIM(&lcVarName)
 
*!************************************************************************
*!
*!      FUNCTION lfvActCod
*!
*!************************************************************************
*
FUNCTION lfvActCod
loFld = _Screen.ActiveForm.ActiveControl

DECLARE laRpRetFld(1)
*lcOldBrFld    = lcBrFields
lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
laRpRetFld[1] = ''
IF !EMPTY(loFld.Value)
  SELECT GLACCHAR
  IF !EMPTY(STRTRAN(loFld.Value,'-'))
    lcRpSegMas = STRTRAN(PADR(loFld.Value,LEN(cacctCode)), ' ', '?')
    lcRpSegMas= [ LIKE("]+lcRpSegMas+[",cacctCode)]
    LOCATE FOR &lcRpSegMas
    IF !FOUND() .AND. gfModalGen("INM02245B00000",'DIALOG')=1
      loFld.Value = loFld.OldValue
      loOgScroll.LAOGFXFLT[loOgScroll.lnAcCodPos,6] = loFld.OldValue
    ENDIF
  ENDIF    
ENDIF

******************************************************************
*
* lfGetCur
*
******************************************************************
FUNCTION lfGetCur
PARAMETERS lnAmm,lnWid,lnDec
RETURN PADL(ALLTR(TRAN(lnAmm,lcRpColPic)),lnRpColWid)


*!*************************************************************
*! Name      : lfEmptyPrd
*! Developer : Wael 
*! Date      : 04/16/2000
*! Purpose   : Check whether the period is empty or not.
*!*************************************************************
*! Called from : lfChkValue()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      :  True, means the period range is empty 
*! 			   :  False, means the period range is not empty  	
*!*************************************************************
FUNCTION lfEmptyPrd
lnFltPos=ASCAN(laOgFxflt,'GLACBALS.CFSPPRDID')
IF lnFltPos > 0
  lnFltRow = ASUBSCRIPT(laOgFxflt,lnFltPos,1)
  lnPID    = ASUBSCRIPT(laOgFxflt,ASCAN(laOgFxflt,'GLACBALS.CFSPPRDID'),1)
  lcFrom   = ALLTRIM(SUBSTR(LAOGFXFLT[lnPID,6],1 , 2))
  lcTo     = ALLTRIM(SUBSTR(LAOGFXFLT[lnPID,6],4 , 5))
  IF EMPTY(lcFrom) .OR. EMPTY(lcTo) 
    RETURN .T.
  ELSE
	RETURN .F.  
  ENDIF
ENDIF  
RETURN .T.

*!*************************************************************
*! Name      : lfCloseAmnt
*! Developer : Wael 
*! Date      : 07/13/2003
*! Purpose   : Summation the closing amount for sertine account
*!*************************************************************
*! Called from : lfCloseAmnt()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      :  Sum The closing amount
*!*************************************************************
FUNCTION lfCloseAmnt

PRIVATE lnCloseAmnt
STORE 0 TO lnCloseAmnt,lnSubTot

IF llRpClose
  lcOldAlias = ALIAS()
  SCATTER MEMVAR MEMO
  IF !USED('GLPTRNHD')
    =gfOpenFile(gcDataDir+'GLPTRNHD','TRANSTAT','SH')
  ENDIF
  SELECT GLPTRNHD
  SET ORDER TO TAG Transtat
    
  IF !USED('GLPTRNDT')
    =gfOpenFile(gcDataDir+'GLPTRNDT','BATCHTRN','SH')
    SET ORDER TO TAG Batchtrn
  ENDIF
  SELECT GLPTRNDT
  SET ORDER TO TAG Batchtrn
  
  IF !USED('GLBATCH')
    =gfOpenFile(gcDataDir+'GLBATCH','BATTYPE','SH')
  ENDIF
  SELECT GLBATCH
  SET ORDER TO TAG BATTYPE
  SEEK("CP")
  SCAN REST WHILE cbattype+cbatstat = "CP" FOR CBATPYR = m.CFISFYEAR
    SELECT Glptrnhd
    SEEK('P')
    SCAN REST WHILE ctrnstat+cbatchno+ctranno = 'P' FOR cbatchno = GLBATCH.cbatchno
      SELECT Glptrndt
      SEEK(Glptrnhd.CBATCHNO + Glptrnhd.ctranno)
      SUM REST IIF(CDRORCR = 'C',namount,namount * -1) ;
          WHILE CBATCHNO+ctranno = Glptrnhd.CBATCHNO + Glptrnhd.ctranno FOR (cacctcode  = ALLTRIM(m.cacctcode);
          AND ctrnpyr+ctrnpprd= m.CFISFYEAR + m.CFSPPRDID )  TO lnSubTot
      lnCloseAmnt = lnCloseAmnt + lnSubTot 
    ENDSCAN
  ENDSCAN
  SELECT (lcOldAlias)
ENDIF

RETURN lnCloseAmnt

************************************************************
*! Name      : lfGetPos
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/02/2012
*! Purpose   : Get position in the laOgFxFlt/laOgVrFlt array
************************************************************
FUNCTION lfGetPos
PARAMETERS lcArr,lcFld
LOCAL lnPos
lnPos = ASUBSCRIPT(loOgScroll.&lcArr , ASCAN(loOgScroll.&lcArr,lcFld) , 1)
RETURN lnPos
*- End of lfGetPos.
