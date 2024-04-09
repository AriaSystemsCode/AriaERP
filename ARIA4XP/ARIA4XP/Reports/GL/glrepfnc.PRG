*E303241,1 This program contains some functions uses so much in the GL reports instead of writing them in each program separetly
*!************************************************************************
*Modifications
*!************************************************************************
*B610176,1 TMI 12/16/2012 [T20121209.0006] if this is the chart of accounts and the account structure has nore than one segment , exit if it is empty
*!************************************************************************




*!************************************************************************
*!
*!      FUNCTION lfvRatGrp
*!
*!************************************************************************
*
FUNCTION lfvRatGrp

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

lcRpCurFld = loOgScroll.ActiveControl.Value
SELECT GLRACOD
LOCATE FOR GLRACOD.csegratio == lcRpCurFld
IF !FOUND()
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CSEGRATIO:H="Ratio group code",CRATDESC:H="Ratio group description"'
  laRpRetFld[1] = ''
  
  lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
  
  *-  Get user position in the laOg??Flt
  LOCAL i,lnPos
  FOR i=1 TO ALEN(&lcRpCurFld,1)
    IF '.CSEGRATIO' $ UPPER(PADR(&lcRpCurFld.[i,1],100))
      lnPos = i
      EXIT 
    ENDIF 
  ENDFOR

  IF !EMPTY(&lcRpCurFld.[lnPos,6])
    SELECT GLRACOD
    LOCATE FOR csegratio = LEFT(&lcRpCurFld.[lnPos,6],1)
    IF !FOUND()
      LOCATE
    ENDIF 
    =gfBrows([],'CSEGRATIO',"laRpRetFld",'Ratio Group File',.F.)
    IF EMPTY(laRpRetFld[1])
      laRpRetFld[1] = loFld.OldValue
    ENDIF 
    loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
  ENDIF
ENDIF

SELECT GLACCHAR


************************************************************
*! Name      : lfvUserID
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get user ID 
************************************************************
FUNCTION lfvUserID
*GFVLFLD('SYUUSER','CUser_ID',VARREAD(),'',.F.,.F.,[CUser_ID:H="User ID",CUsr_name:H="User name"],'1',.F.,.t.)

lcBrFields    = [CUser_ID:H="User ID",CUsr_name:H="User name"]
lfGetBrow('CADD_USER','SYUUSER','CUSER_ID','Users','CUSER_ID')

*- End of FUNCTION lfvUserID

************************************************************
*! Name      : lfvCashFl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get Cash flow id
************************************************************
FUNCTION lfvCashFl
*GFVLFLD('GLCFITEM','CSEGCAFLO',VARREAD(),IIF(EMPTY(EVAL(VARREAD())),'',lcRpOld),.F.,.F.,[CSEGCAFLO:H="Statement of cash flow Id",CCFIDESC:H="Cash flow discription"],'1',.F.,.t.)

lcBrFields    = [CSEGCAFLO:H="Statement of cash flow Id",CCFIDESC:H="Cash flow Description"]
lfGetBrow('CSEGCAFLO','GLCFITEM','CFICODE','Cash Flow Items')

*- End of FUNCTION lfvUserID



************************************************************
*! Name      : lfGetBrow
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get browse
************************************************************
FUNCTION lfGetBrow
PARAMETERS lcFld,lcBrwFl,lcBrwFlIdx,lcTitle,lcRetFlt,lcKey
lcKey = IIF(EMPTY(lcKey),'',ALLTRIM(lcKey))
LOCAL lnSlct,lcPath
lnSlct = SELECT(0)

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

*- Get user position in the laOg??Flt
IF UPPER(LEFT(lcRpCurFld,4)) == 'LAOG'
  LOCAL i,lnPos
  FOR i=1 TO ALEN(&lcRpCurFld,1)
    IF '.'+lcFld $ UPPER(PADR(&lcRpCurFld.[i,1],100))
      lnPos = i
      EXIT 
    ENDIF 
  ENDFOR
  lcRpCurFld = lcRpCurFld + '['+ALLTRIM(STR(lnPos))+',6]'
ENDIF   
lcRetFlt = IIF(EMPTY(lcRetFlt),lcFld,lcRetFlt)
DECLARE laRpRetFld(1)
laRpRetFld[1] = ''

llUsed = .F.
IF !USED(lcBrwFl)
  lcPath = IIF(LEFT(lcBrwFl,2)='SY',oAriaApplication.SysPath,oAriaApplication.DataDir)
  =gfOpenFile(lcPath+lcBrwFl,lcBrwFlIdx,'SH')
  llUsed = .T.  
ENDIF 
SELECT (lcBrwFl)
SET ORDER TO (lcBrwFlIdx)
*IF '?' $ &lcRpCurFld.[lnPos,6] .OR. !SEEK(&lcRpCurFld.[lnPos,6])

*B610176,1 TMI 12/16/2012 [Start] if this is the chart of accounts and the account structure has nore than one segment , exit if it is empty
IF lcBrwFl = 'GLACCHAR'
  IF EMPTY(CHRTRAN(&lcRpCurFld.,'0-',''))
    IF llUsed
      USE IN (lcBrwFl)
    ENDIF
    RETURN 
  ENDIF 
ENDIF 
*B610176,1 TMI 12/16/2012 [End  ] 
IF '?' $ &lcRpCurFld. .OR. !SEEK(lcKey+&lcRpCurFld.)
  IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
    GOTO RECNO(0)
  ENDIF 
  =gfBrows(["&lcKey"],lcRetFlt,"laRpRetFld",lcTitle,.F.)
  *loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue 
  ENDIF 
  loOgScroll.&lcRpCurFld. = laRpRetFld[1]
  &lcRpCurFld. = laRpRetFld[1]
  *loFld.Value = laRpRetFld[1]
ENDIF

IF llUsed
  USE IN (lcBrwFl)
ENDIF 
SELECT (lnSlct)
*- End of lfvUserID.


*!************************************************************************
*!
*!      FUNCTION lfvActCod
*!
*!************************************************************************
FUNCTION lfvActCod
PRIVATE lcOldOrder
lcOldOrder = ''

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

DECLARE laRpRetFld(1)
lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
lnPos = lfNav2GetPos(lcRpCurFld,'CACCTCODE')
IF !EMPTY(&lcRpCurFld.[lnPos,6])
  SELECT GLACCHAR
  lcOldOrder = ORDER()
  SET ORDER TO ACCTCODE
  IF "?" $ &lcRpCurFld.[lnPos,6] .OR. !SEEK(&lcRpCurFld.[lnPos,6])
    =gfBrows([],'CACCTCODE',"laRpRetFld",'Accounts File',.F.)
    loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
  ENDIF
  SET ORDER TO &lcOldOrder  
ENDIF


************************************************************
*! Name      : lfNav2GetPos
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : navigate the system array to get position
************************************************************
FUNCTION lfNav2GetPos
PARAMETERS lcArr,lcFld
  *-  Get user position in the laOg??Flt
  LOCAL i,lnPos
  FOR i=1 TO ALEN(&lcArr,1)
    IF '.&lcFld' $ UPPER(PADR(&lcArr.[i,1],100))
      lnPos = i
      EXIT 
    ENDIF 
  ENDFOR
RETURN lnPos
*- End of lfNav2GetPos.


*!************************************************************************
*!
*!      FUNCTION lfvTypeCode
*!
*!************************************************************************
*
FUNCTION lfvTypeCode

lcBrFields    = [CTypeCode:H="Code",CTypeDesc:H="Description"]
lfGetBrow('CTYPECODE','GLTypes','TYPECODE','Types File')

*- End of lfvTypeCode.


*!************************************************************************
*!
*!      FUNCTION lfvSorMod
*!
*!************************************************************************
*
FUNCTION lfvSorMod

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

PRIVATE llApplFile
llApplFile = .F.

DECLARE laRpRetFld(1)

lcBrFields    = 'Capp_id:H="Modul code",Capp_name:H="Modul name"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
lnPos = lfNav2GetPos(lcRpCurFld,'CSRCMODUL')
IF !USED('SYDAPPL')
  llApplFile = .T.
  SELECT 0
  USE (oAriaApplication.SysPath+'SYDAPPL')
ENDIF

SELECT sydAPPL
SET ORDER TO TAG CAPP_ID

IF !SEEK(&lcRpCurFld.[lnPos,6]) 
  =gfBrows([FOR !INLIST(CAPP_ID,'SY','SM')],'CAPP_ID',"laRpRetFld",'Moduls',.F.)
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  ENDIF 
  loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
ELSE
  IF !SEEK(&lcRpCurFld.[lnPos,6])
    loOgScroll.&lcRpCurFld.[lnPos,6]=""
  ENDIF
ENDIF
SET ORDER TO

IF llApplFile
  USE IN SYDAPPL
ENDIF


*!************************************************************************
*!
*!      Function lfvFisPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvFisPrd
loFld = _Screen.ActiveForm.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   
loFld.Value = PADL(ALLTRIM(loFld.Value),2,'0')

DECLARE laRpRetFld(1)
lcBrFields    = [CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"]
laRpRetFld[1] = ''

IF TYPE('loOgScroll.ActiveControl.Parent.cOgArray')='U'
  lcRpCurFld = 'loFld.Value'
ELSE
  lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
ENDIF   

lcRpFicsYr      = laOGFxFlt[1,6]
&& Check If year field is empty
IF .NOT. EMPTY(lcRpFicsYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = SELECT()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    
    IF NOT USED("FSPRD") 
      SELECT 0
    gcDataDir = oAriaApplication.DataDir
      USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
      llUesdBefo = .T.
    ENDIF
    SELECT FSPRD
    
    *** Search for the current company+year+Prd
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
        =gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld","Fiscal period",.F.)
        IF EMPTY(laRpRetFld[1])
          laRpRetFld[1] = loFld.OldValue
        ENDIF 
        IF TYPE('loOgScroll.ActiveControl.Parent.cOgArray')='U'
          &lcRpCurFld = laRpRetFld[1]      
        ELSE
          loOgScroll.&lcRpCurFld = laRpRetFld[1]      
        ENDIF 

    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      
      USE IN FSPRD
      
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ELSE
  IF TYPE('loOgScroll.ActiveControl.Parent.cOgArray')='U'
    &lcRpCurFld = "  "
  ELSE
    loOgScroll.&lcRpCurFld = "  "
  ENDIF     
ENDIF
RETURN 

************************************************************
*! Name      : lfGLSUBJOR
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : GLSUBJOR
************************************************************
FUNCTION lfGLSUBJOR
*GFVLFLD('GLSUBJOR','CSRCJRNL',VARREAD(),'',.F.,.F.,[CSRCJRNL:H="Source journal entry",CJORLNDES:H="Source journal description"],'1',.F.,.t.)
lcBrFields    = [CSRCJRNL:H="Source journal entry",CJORLNDES:H="Source journal description"]
lfGetBrow('CSRCJRNL','GLSUBJOR','SRCJRNL','Source Journal Entry')


*- End of lfGLSUBJOR.
************************************************************
*! Name      : lfBudgCode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : Budget Code browse
************************************************************
FUNCTION lfBudgCode
*GFVLFLD('GLBUDHD','CBUDCODE',VARREAD(),'',.F.,.F.,[CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"],'1',.F.,.t.)                                                                      
lcBrFields    = [CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"]
lfGetBrow('CBUDCODE','GLBUDHD','BDCODYR','Budget Codes')

*- End of lfBudgCode.

*!************************************************************************
*!
*!      Function lfvBudYear
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvBudYear

*GFVLFLD('GLBUDHD','CBUDCODE',VARREAD(),'',.F.,.F.,[CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"],'1',.F.,.t.)                                                                      
lcBrFields    = [CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"]
lfGetBrow('CBUDYEAR','GLBUDHD','BDYRCOD','Budget Codes')
*- End of FUNCTION lfvBudYear
************************************************************
*! Name      : lfvPosYer
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : Posting Year
************************************************************
FUNCTION lfvPosYer
LPARAMETERS lcFld
lcBrFields    = 'CFISFYEAR:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
lfGetBrow(lcFld,'fishd','COMPFYEAR','Transaction Codes','CFISFYEAR')

*- if the posting year is empitied then also empty the posting period 
LOCAL lcArr,lnPos
lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
IF EMPTY(&lcRpCurFld.)
  lcArr = loOgScroll.ActiveControl.Parent.cOgArray
  lnPos = lfNav2GetPos(lcArr,'CTRNPPRD')
  IF lnPos>0
    lcRpPrdFld = lcArr + '['+ALLTRIM(STR(lnPos))+',6]'
    loOgScroll.&lcRpPrdFld. = ''
  ENDIF 
ENDIF   

*- End of lfvPosYer.
************************************************************
*! Name      : lfvPosPrd
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : Posting Prd
************************************************************
FUNCTION lfvPosPrd
LPARAMETERS lcYearFld
LOCAL lnPos,lcArr,lcKey
lcArr = loOgScroll.ActiveControl.Parent.cOgArray
lnPos = ASUBSCRIPT(&lcArr,ASCAN(&lcArr,lcYearFld),1)
lcKey = ALLTRIM(loOgScroll.&lcArr.[lnPos,6])
IF .NOT. EMPTY(lcKey)
  lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'  
  =lfGetBrow('CTRNPPRD','FSPRD','COMFYRPRDI','Transaction Codes','CFsppRdid',lcKey)
ELSE 
  lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
  loOgScroll.&lcRpCurFld. = ''
ENDIF   

*- End of lfvPosPrd.

*!************************************************************************
*!
*!      FUNCTION lfWaitMsg
*!
*!************************************************************************
FUNCTION lfWaitMsg

WAIT "Collecting data... " WINDOW NOWAIT


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
  SELECT &lcFile
  AFIELDS(laStru)
  CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) FROM ARRAY laStru
  
ENDIF

SELECT &lcFile
lcRel = SET("Relation")
SELECT (lcTmpFile)
SET RELATION TO &lcRel


SELECT &lcFile
lcFor = IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
SCAN &lcFor
  SCATTER MEMVAR 
  INSERT INTO (lcTmpFile) FROM MEMVAR 
ENDSCAN 
  
SELECT (lnSlct)
*- End of lfCollect.
