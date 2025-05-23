*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLUNBATS.PRG
*:  Module      : General Ledger
*:  Desc.       : Unposted batches
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303248,1
*:************************************************************************
*E610601,1 TMI 11/26/2013 Disable updating the field DBATELDAT [T20131118.0005] 
*:************************************************************************
*:
*** Report Setup
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include r:\aria4xp\reports\gl\glunbats.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

IF !'lfEditList()'$lcRpExp
  lcRpExp = lcRpExp + ' AND lfEditList() = .F.'
ENDIF

SET ORDER TO TAG COMPFYEAR IN FISHD

SELECT GLBATCH
SET RELATION TO glbatch.cbatpyr INTO FISHD ADDITIVE

GO TOP

*- data collection
=lfCollect()

SELECT (lcTmpFile)
DO gfDispRe WITH EVAL('lcRpForm')

SELECT GLBATCH
SET RELATION TO

*E610601,1 TMI 11/26/2013 21:07 [Start] disable the update of the field DBATELDAT
*!*	IF !EMPTY(ALLTRIM(lcRpExp))
*!*	  SCAN FOR &lcRpExp
*!*	    =RLOCK()
*!*	    REPLACE cbatElUsr WITH gcUser_ID ,;
*!*	            dBatElDat WITH DATE()    ,;
*!*	            cBatElTim WITH gfGetTime()
*!*	    UNLOCK
*!*	  ENDSCAN
*!*	ENDIF
*E610601,1 TMI 11/26/2013 21:07 [End  ] 

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

  lnLen = ALEN(laStru,1)
  lnMore = 3
  DIMENSION laStru[lnlen+lnMore ,18]

  lnInsAt = 8
  FOR i=1 TO lnMore
    AINS(laStru,lnInsAt)
  ENDFOR

  lnI = lnInsAt-1
  lnI = lnI + 1
  laStru[lnI,1] = 'TYPE'
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 24
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = 'STATUS'
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 24
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = 'ELPRN'
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 3
  laStru[lnI,4] = 0

  FOR i= lnInsAt TO lnInsAt +lnMore -1
    STORE .F. TO laStru[i,5],laStru[i,6]
    STORE '' TO laStru[i,7],laStru[i,8],laStru[i,9],laStru[i,10],laStru[i,11],laStru[i,12],laStru[i,13],laStru[i,14],laStru[i,15],laStru[i,16]
    STORE 0 TO laStru[i,17],laStru[i,18]
  ENDFOR

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
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	  m.TYPE = IIF(cbattype='N','Normal',IIF(cbattype='S','Statistical',IIF(cbattype='B','Beginning Balances',IIF(cbattype='L','Subledger',IIF(cbattype='Z','Summerized',' ')))))
*!*	  m.STATUS = IIF(cBatstat='E','EMPTY',IIF(cBatstat='O','Out of balance',IIF(cBatstat='U','Unposted',IIF(cBatstat='A','Approved',IIF(cBatstat='V','Void','Hold')))))
*!*	  m.ELPRN = IIF(lfEditList(),'Yes','No')
  m.TYPE = IIF(cbattype='N',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NORMAL,oAriaApplication.GetHeaderText("LANG_NORMAL",AHEADERFILE)),;
           IIF(cbattype='S',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATIS,oAriaApplication.GetHeaderText("LANG_STATIS",AHEADERFILE)),;
           IIF(cbattype='B',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BEGBAL,oAriaApplication.GetHeaderText("LANG_BEGBAL",AHEADERFILE)),;
           IIF(cbattype='L',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SUBLED,oAriaApplication.GetHeaderText("LANG_SUBLED",AHEADERFILE)),;
			IIF(cbattype='Z',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SUMMER,oAriaApplication.GetHeaderText("LANG_SUMMER",AHEADERFILE)),' ')))))
  m.STATUS = IIF(cBatstat='E',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_EMPTY,oAriaApplication.GetHeaderText("LANG_EMPTY",AHEADERFILE)),;
             IIF(cBatstat='O',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OUTOFBAL,oAriaApplication.GetHeaderText("LANG_OUTOFBAL",AHEADERFILE)),;
             IIF(cBatstat='U',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UNPOSTED,oAriaApplication.GetHeaderText("LANG_UNPOSTED",AHEADERFILE)),;
             IIF(cBatstat='A',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APPOVED,oAriaApplication.GetHeaderText("LANG_APPOVED",AHEADERFILE)),;
             IIF(cBatstat='V',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOID,oAriaApplication.GetHeaderText("LANG_VOID",AHEADERFILE)),;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE)))))))
  m.ELPRN = IIF(lfEditList(),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_YES,oAriaApplication.GetHeaderText("LANG_YES",AHEADERFILE)),;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NO,oAriaApplication.GetHeaderText("LANG_NO",AHEADERFILE)))
  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  INSERT INTO (lcTmpFile) FROM MEMVAR
ENDSCAN

SELECT (lnSlct)
*- End of lfCollect.

*!************************************************************************
*!
*!      Function lfEditList
*!
*!************************************************************************
*
FUNCTION lfEditList

RETURN  ( DTOS(dbaTelDat)+cbAtelTim >;
                                  DTOS(daDd_Date)+caDd_Time )

*!************************************************************************
*!
*!      Function lfvRpPost
*!
*!************************************************************************
*
FUNCTION lfvRpPost

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_YEAR,oAriaApplication.GetHeaderText("LANG_YEAR",AHEADERFILE))+;
                '",DFisBgDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BEGINDATE,oAriaApplication.GetHeaderText("LANG_BEGINDATE",AHEADERFILE))+;
'",DFisEnDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENDDATE,oAriaApplication.GetHeaderText("LANG_ENDDATE",AHEADERFILE))+'"'
*N000682,1 MMT 02/11/2013 Globalization changes[End]
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
IF .NOT.EMPTY(&lcRpCurFld.)
  lcOldAlias = ALIAS()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time

  IF NOT USED("FISHD")
    SELECT 0
    USE (oAriaApplication.DataDir+'FISHD') ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD


  SET ORDER TO TAG COMPFYEAR
  *** Search for the current company+year

    IF ('?' $ &lcRpCurFld. .OR.;
      !SEEK(ALLTRIM(&lcRpCurFld.))) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
         *N000682,1 MMT 02/11/2013 Globalization changes[Start]
         *=gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
		=gfBrows('','CFisFyear',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANCODE,oAriaApplication.GetHeaderText("LANG_TRANCODE",AHEADERFILE)),.F.)
         *N000682,1 MMT 02/11/2013 Globalization changes[Start]
      &lcRpCurFld = laRpRetFld[1]
    ENDIF

  IF llUesdBefo       && .F.- this file used by the system

    USE IN FISHD

  ENDIF
  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF
ENDIF
RETURN

*!************************************************************************
*!
*!      Function lfRpNum
*!
*!************************************************************************
*
FUNCTION lfRpNum

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF


EXTERNAL ARRAY laOgFxFlt
DECLARE laRetInfo[1]
lcRpBatNo = loOgScroll.ActiveControl.Parent.oItem.cAssociate
IF !EMPTY(EVAL(lcRpBatNo))
  &lcRpBatNo=trans(VAL(EVAL(lcRpBatNo)),'@l 999999')
ENDIF
SELECT GLBATCH
lcOldTag=SYS(22)
SET ORDER TO TAG BATCHNO
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*lcBrFields       = 'cbatchno:H="Batch number",cbatdesc:H="Batch description"'
lcBrFields       = 'cbatchno:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATNO,oAriaApplication.GetHeaderText("LANG_BATNO",AHEADERFILE)) +;
'",cbatdesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATDESC,oAriaApplication.GetHeaderText("LANG_BATDESC",AHEADERFILE))+'"'
*N000682,1 MMT 02/11/2013 Globalization changes[End]

laRetInfo[1]     = laOgFxFlt[1,6]
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*lcFile_Ttl       = 'Batches'
lcFile_Ttl       = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHES,oAriaApplication.GetHeaderText("LANG_BATCHES",AHEADERFILE))
*N000682,1 MMT 02/11/2013 Globalization changes[End]
*** first  parameter : index expression
*** second parameter : key field
*** third  parameter : array to return in
*** forth  parameter : flag to set user filter in brows on/off

IF !EMPTY(ALLTRIM(&lcRpBatNo)) AND ! SEEK(EVAL(lcRpBatNo))
  IF !EMPTY(laOgFxFlt[1,6])
    =gfBrows('"'+laOgFxFlt[1,6]+laOgFxFlt[2,6]+'"',"cbatchno",'laRetInfo',.F.)
  ELSE
    SET ORDER TO TAG BATSTAT
    =gfBrows('"'+laOgFxFlt[2,6]+'"',"cbatchno",'laRetInfo',.F.)
  ENDIF
  IF EMPTY(laRetInfo[1])
    laRetInfo[1] = loFld.OldValue
  ENDIF
  loOgScroll.&lcRpBatNo.=laRetInfo[1]
ENDIF

SET ORDER TO TAG &lcOldTag

*!************************************************************************
*!
*!      Function lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE

*- End of lfRepWhen.
