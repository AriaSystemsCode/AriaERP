*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLACMEN.PRG
*:  Module      : General Ledger
*:  Desc.       : Multiple Accounts
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 07/15/2012
*:  Reference   : *E303197,1
*:************************************************************************
*B610137,1 TMI 10/31/2012 [] update the GLACBALS file
*:************************************************************************
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\gl\GLACMEN.H
*N000682,1 MMT 11/22/2012 Globalization changes[End]
*- Get the screen , call it
lcRunScx = lfGetScx("GL\GLACMEN.scx")
DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/15/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName','GLACMEN')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES(loFormSet.lcProgName)

IF !lfGL(loFormset)
  RETURN .F.
ENDIF

*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

SELECT GLTYPES
GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.
  RETURN .F.
ENDIF

SELECT GLSEGVAL

FOR lnCounter = 1 TO loFormset.lnAcsNoSeg
  SEEK(STR(lnCounter,1))
  IF ! FOUND()
    =gfModalGen("TRM02199B00000","DIALOG")
    glQuitting  = .T.
    RETURN .F.
  ENDIF
ENDFOR

*- Define needed variables.
=lfDefineVars(loFormSet)


*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/15/2012
*! Purpose   : Defining the variables used in the screen , add them as properties
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormset

lcScFields = 'CACSSEGNO ,CSEGVALUE ,CSEGLNDES ,CSEGSHDES ,CSEGACTIV ,CSEGTERM  ,CSEGACCAT ,CTYPECODE ,CSTANDARD ,NSEGSEQN  ,CSEGRATIO ,CSEGCAFLO ,CSEGALPOS ,CSEGAUTDS ,CSEGCOCAC ,NSEGCONPR '
loFormset.Addproperty('lcScFields',lcScFields)
loFormset.Addproperty('laSegSiz[1,3]','')
loFormset.Addproperty('laBalRecs[1]','')
loFormset.Addproperty('laFileStru[1]','')

loFormset.Addproperty('lnOldRecs',0)

loFormset.Addproperty('lnOldRow',0)
loFormset.Addproperty('lcCurObj','DISABLE')
loFormset.Addproperty('lcObjUpDat','DISABLE')
loFormset.Addproperty('lcObjGener','DISABLE')
loFormset.Addproperty('laCtrStat','DISABLE')

loFormset.Addproperty('rbAction',1)
loFormSet.Ariaform1.rbAction.ControlSource = 'Thisformset.rbAction'

loFormset.Addproperty('lnAction'     ,  1)

loFormset.Addproperty('llProcessing' , .T.)

gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir

SELECT DISTINCT FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
       FROM &gcDataDir.FSPRD;
       WHERE VAL(CFISFYEAR) >= loFormSet.lnCurr_yer - 1;
       ORDER BY FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
       INTO ARRAY loFormSet.laBalRecs

 SELECT DISTINCT ;
       NACSSIZE,CACSSHDES,CACSSHDES,nacssegno;
  FROM  &gcDataDir.ACCOD;
  WHERE !EMPTY(ACCOD.nacssize) ;
 ORDER BY nacssegno ;
 INTO  ARRAY loFormSet.laSegSiz

loFormset.Addproperty('lnSegNo', ALEN(loFormSet.laSegSiz,1) )

FOR lnCount = 1 TO loFormSet.lnSegNo

  loFormset.laSegSiz[lnCount,3] = "Sg"+ALLTRIM(STR(lnCount))
  lcExclAry = "laExcAry"+STR(lnCount,1)

  loFormset.Addproperty('&lcExclAry[1]',' ')

  loFormset.laSegSiz[lnCount,2] =ALLTRIM(loFormset.laSegSiz[lnCount,2])

  IF !USED(loFormset.laSegSiz[lnCount,3])
	SELECT 0
	USE &gcDataDir.GLSEGVAL	AGAIN ALIAS (loFormset.laSegSiz[lnCount,3]) ORDER 1
    lcFilter = "CACSSEGNO='"+STR(lnCount,1)+"'"
	SET FILTER TO &lcFilter
  ENDIF

  lcList1 = "lcSegRng"+STR(lnCount,1)+"1"
  lcList2 = "lcSegRng"+STR(lnCount,1)+"2"
  GO TOP
  loFormset.Addproperty('&lcList1', ALLTRIM(csegvalue))
  GO BOTTOM
  loFormset.Addproperty('&lcList2', ALLTRIM(csegvalue))

  SELECT GLSEGVAL
  lcArrayName = "laSegment"+STR(lnCount,1)
  loFormset.Addproperty('&lcArrayName[1]', ' ')

  SELECT ALLTRIM(cSegValue)+' '+cSeglnDes FROM (loFormset.laSegSiz[lnCount,3]);
     WHERE CACSSEGNO = STR(lnCount,1);
     ORDER BY  cSegValue ;
     INTO ARRAY loFormSet.&lcArrayName

ENDFOR

loFormset.Addproperty('lc_BlankDBF', gfTempName()    )

loFormset.Addproperty('lc_TempFile', gfTempName())

SELECT GLACCHAR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,18]

laFileStru[lnFileStru+1,1] = 'Include'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

FOR lnI = lnFileStru+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[lnI,5],laFileStru[lnI,6]
  FOR lnJ = 7 TO 16
    laFileStru[lnI,lnJ] = ""
  ENDFOR
  STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]
ENDFOR

lc_TempFile = loFormSet.lc_TempFile
CREATE TABLE &gcWorkDir.&lc_TempFile FROM ARRAY laFileStru

SELECT (lc_TempFile)
INDEX ON  cAcctCode TAG cAcctCode
SET ORDER TO TAG cAcctCode

*- Set grid field sources
=lfGrdSrc(loFormSet)

*- End of lfDefineVars.

************************************************************
*! Name      : lfGrdSrc
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/16/2012
*! Purpose   : Set grid field sources
************************************************************
FUNCTION lfGrdSrc
PARAMETERS loFormSet

WITH loFormSet.Ariaform1.grdCreatedAccounts
  .Column1.header1.Caption = ''
  .Column1.ControlSource = '&lc_TempFile..Include'

  .Column2.header1.Caption = loFormSet.LCACSEGDES
  .Column2.ControlSource = '&lc_TempFile..cAcctcode'

  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  .Column3.header1.Caption = 'Account Description'
  .Column3.header1.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_ACCOUNT_DESCRIPTION,loFormSet.GetHeaderText("LANG_GLACMEN_ACCOUNT_DESCRIPTION",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]
 
  .Column3.ControlSource = '&lc_TempFile..cAccnldes'

ENDWITH

*- End of lfGrdSrc.

************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/15/2012
*! Purpose   : Destroy method
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormset

FOR lnCount = 1 TO loFormSet.lnSegNo
  IF USED(loFormset.laSegSiz[lnCount,3])
	USE IN ALIAS(loFormset.laSegSiz[lnCount,3])
  ENDIF
ENDFOR

lc_TempFile = loFormSet.lc_TempFile
IF USED(lc_TempFile)
  USE IN ALIAS(lc_TempFile)
ENDIF
gcWorkDir = oAriaApplication.WorkDir
ERASE &gcWorkDir.&lc_TempFile..DBF
ERASE &gcWorkDir.&lc_TempFile..FPT
ERASE &gcWorkDir.&lc_TempFile..CDX

*- End of lfFormDestroy.


************************************************************
*! Name      : lfvSegFr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/15/2012
*! Purpose   : The From Segment validation function
************************************************************
FUNCTION lfvSegFr
PARAMETERS loRngFormset,loFld
RETURN lfSegFrTo(loRngFormset,loFld,'F')

*- End of lfvSegFr.
************************************************************
*! Name      : lfvSegTo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/15/2012
*! Purpose   : The TO Segment validation function
************************************************************
FUNCTION lfvSegTo
PARAMETERS loRngFormset,loFld
RETURN lfSegFrTo(loRngFormset,loFld,'T')

*- End of lfvSegTo.
************************************************************
*! Name      : lfSegFrTo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/16/2012
*! Purpose   : Validate segments in the criteria screen
************************************************************
FUNCTION lfSegFrTo
PARAMETERS loRngFormset,loFld,lcFrTo
IF loFld.Value = loFld.OldValue AND !EMPTY(loFld.Value)
  RETURN 1
ENDIF

LOCAL lnSlct,lcI,lnCount,loFr,loTo,loFormset
lnSlct = SELECT(0)

lcI = RIGHT(loFld.Name,1)
lnSg = INT(VAL(lcI))

loFormset = loRngFormset.loFormset

lcScFields = loFormSet.lcScFields

loFld.Value = ALLTRIM(loFld.Value)
IF EMPTY(loFld.Value) OR '?'$loFld.Value OR !SEEK(lcI + loFld.Value,loFormset.laSegSiz[lnSg,3])
  *- Call the appropriate segment file browse
  SELECT (loFormset.laSegSiz[lnSg,3])
  *- Get the neareset record
  IF EMPTY(loFld.Value) OR '?'$loFld.Value
    LOCATE
  ELSE
    IF RECNO(0)>0
      GOTO RECNO(0)
    ELSE
      LOCATE
    ENDIF
  ENDIF

  DIMENSION laSeg[2]
  laSeg = ' '
  lcFile_Ttl = 'Segment #: '+lcI
  =gfBrows("'"+lcI+"'",lcScFields,'laSeg',lcFile_Ttl)
  IF Type('laSeg[2]')<>'U'
    loFld.Value = laSeg[2]
  ELSE
    loFld.Value = loFld.OldValue
    SELECT ( lnSlct )
    RETURN 0
  ENDIF
ENDIF

*- Define From, To controls
loFr = loFld.Parent.txtSegFr&lcI
loTo = loFld.Parent.txtSegTo&lcI

*- The From can not exceed the To segment value
IF loFr.Value > loTo.Value
  *- if come from 'FROM', apply to 'TO' alos, and vise versa
  IF lcFrTo = 'T'
    loFr.Value = loTo.Value
  ELSE
    loTo.Value = loFr.Value
  ENDIF
ENDIF
loRngFormset.Ariaform1.Refresh()

SELECT ( lnSlct )

*- End of lfSegFrTo.
************************************************************
*! Name      : lfwSeg
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/16/2012
*! Purpose   : to display the description
************************************************************
FUNCTION lfwSeg
PARAMETERS loFld
LOCAL lnSlct,lcSg
lnSlct = SELECT(0)

lcSg = RIGHT(loFld.Name,1)

SELECT(loFormset.laSegSiz[&lcSg,3])
loFld.parent.txtDesc.Value = IIF(SEEK(lcSg+loFld.Value,loFormset.laSegSiz[&lcSg,3]),CSEGLNDES,'')

SELECT (lnSlct)
*- End of lfwSeg.

                            *- the original code *-

*!**************************************************************************
*!
*!      Function: lfvRange
*!
*!**************************************************************************
*
FUNCTION lfvRange
PARAMETERS loFormSet

llSegChang = .F.

SELECT GLSEGVAL

*- Get the screen , call it
lcRunScx = lfGetScx("GL\GLSEGRAN.scx")
DO FORM (lcRunScx) WITH loFormset TO llSegChang

loFormset.Ariaform1.pbGenerat.Enabled = llSegChang


*- End of lfvRange.

*!**************************************************************************
*!
*!      Function: lfReadWhen
*!
*!**************************************************************************
*
FUNCTION lfReadWhen
PARAMETERS loFormSet
=lfCheckBut(loFormSet)

*!**************************************************************************
*!
*!      Function: lfvGenerat
*!
*!**************************************************************************
FUNCTION lfvGenerat
PARAMETERS loFormset

DECLARE laFilter[6,2]

lc_TempFile = loFormSet.lc_TempFile
lc_BlankDBF = loFormSet.lc_BlankDBF

IF RECCOUNT(lc_TempFile) > 0
  IF gfModalGen("TRM02208B00012","DIALOG") = 1
    SELECT (lc_TempFile)
    ZAP
    =lfReadWhen(loFormSet)
  ELSE
    RETURN
  ENDIF
ENDIF

lnTotAcont = 1
lnTotRec   = 0
lnSavTime = SECONDS()

gcWorkDir = oAriaApplication.WorkDir

SELECT GLACCHAR
COPY STRUCTURE TO &gcWorkDir.&lc_BlankDBF
USE &gcWorkDir.&lc_BlankDBF IN 0 EXCLUSIVE
SELECT (lc_BlankDBF)

FOR lnCounter = 1 TO 40
  APPEND BLANK
ENDFOR

SELECT GLACCHAR
SET ORDER TO TAG ACCTCODE

oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
=lfThermo(100,1,'Generating Account...',' ')
laFilter = .F.

*- make a copy of laSegSiz array to not add the loFormSet in each place below
DIMENSION laSegSiz[ALEN(loFormset.laSegSiz,1),ALEN(loFormset.laSegSiz,2)]
ACOPY(loFormset.laSegSiz,laSegSiz)

FOR lnCount = 1 TO loFormSet.lnSegNo

  SELECT (laSegSiz[lnCount,3])
  lcFromSeg  = "loFormSet.lcSegRng"+STR(lnCount,1)+"1"
  lcTOSeg    = "loFormSet.lcSegRng"+STR(lnCount,1)+"2"
  lcExcAry   = "laExcAry"+STR(lnCount,1)
  lnSegTotal = 0

  GO TOP
  COUNT FOR cSegValue >= EVALUATE(lcFromSeg)  .AND. ;
            cSegValue <= EVALUATE(lcTOSeg)    .AND. ;
            ASCAN(loFormSet.&lcExcAry,RTRIM(cSegValue)) = 0 ;
            TO lnSegTotal

  lnTotAcont = lnTotAcont * IIF(lnSegTotal=0,1,lnSegTotal)
  GO TOP
  lcSegFilt =  [CACSSEGNO =']+STR(lnCount,1)+;
    [' AND BETWEEN(cSegValue,']+EVAL(lcFromSeg)+[',']+EVAL(lcToSeg)+[')]

  SET FILTER TO &lcSegFilt

ENDFOR

lcEscStat = SET('ESCAPE')
lcOnEsc   = ON('ESCAPE')
SET ESCAPE ON
ON ESCAPE llProcessing = .F.
llProcessing = .T.

SELECT (laSegSiz[1,3])

SCAN ALL FOR ASCAN(loFormset.laExcAry1,RTRIM(cSegValue)) = 0 AND llProcessing

IF lfTerminate()
  EXIT
ENDIF

  SCATTER MEMVAR MEMO
  IF TYPE('laSegSiz[2,3]') = 'C'
    SELECT (laSegSiz[2,3])
    SCAN ALL FOR ASCAN(loFormset.laExcAry2,RTRIM(cSegValue)) = 0 AND  llProcessing
      IF TYPE('laSegSiz[3,3]') = 'C'
        SELECT (laSegSiz[3,3])
        SCAN ALL FOR ASCAN(loFormset.laExcAry3,RTRIM(cSegValue)) = 0 AND  llProcessing
          IF TYPE('laSegSiz[4,3]') = 'C'
            SELECT (laSegSiz[4,3])
            SCAN ALL FOR  ASCAN(loFormset.laExcAry4,RTRIM(cSegValue)) = 0 AND llProcessing
              IF TYPE('laSegSiz[5,3]') = 'C'
                SELECT (laSegSiz[5,3])
                SCAN ALL FOR ASCAN(loFormset.laExcAry5,RTRIM(cSegValue)) = 0 AND  llProcessing
                  IF TYPE('laSegSiz[6,3]') = 'C'
                    SELECT (laSegSiz[6,3])
                    SCAN ALL FOR ASCAN(loFormset.laExcAry6,RTRIM(cSegValue)) = 0 AND  llProcessing
                      =lfUpdTemp()
                      =lfTerminate()
                    ENDSCAN && 6
                  ELSE   && 5
                    =lfUpdTemp()
                    =lfTerminate()
                  ENDIF  && 5
                ENDSCAN  && 5
              ELSE   && 4
                =lfUpdTemp()
                =lfTerminate()
              ENDIF  && 4
            ENDSCAN  && 4
          ELSE   && 3
            =lfUpdTemp()
            =lfTerminate()
          ENDIF  && 3
        ENDSCAN  && 3
      ELSE  && 2
        =lfUpdTemp()
        =lfTerminate()
      ENDIF && 2
    ENDSCAN && 2
  ELSE  && 1
    =lfUpdTemp()
    =lfTerminate()
  ENDIF && 1
ENDSCAN && 1

IF USED(lc_BlankDBF)
  USE IN ALIAS(lc_BlankDBF)
ENDIF
ERASE &gcWorkDir.&lc_BlankDBF..DBF

FOR lnCount = 1 TO loFormSet.lnSegNo
  SELECT (laSegSiz[lnCount,3])
  lcFilter = "'"+STR(lnCount,1)+"'"
  SET FILTER TO CACSSEGNO=&lcFilter
ENDFOR


ON  ESCAPE &lcOnEsc
SET ESCAPE &lcEscStat
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*=lfThermo(100,100,'Generating Account...')
=lfThermo(100,100,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_GENERATEACCOUNT,loFormSet.GetHeaderText("LANG_GLACMEN_GENERATEACCOUNT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oProgress = NULL

lcTotTime = STR(SECONDS()-lnSavTime,8,3)

*WAIT ALLTRIM(STR(lnTotRec))+" Accounts Generated in "+lcTotTime+" seconds " WINDOW NOWAIT
lcTmpStr = ALLTRIM(STR(lnTotRec)) + "|" + lcTotTime
SET STEP ON
=gfWait("02232","NOWAIT",lcTmpStr)

lcObjGener = 'DISABLE'
loFormset.Ariaform1.pbGenerat.Enabled = .F.
SELECT (lc_TempFile)
DELETE ALL FOR EMPTY(&lc_TempFile..cAcctcode)
GO TOP

loFormset.Ariaform1.grdCreatedAccounts.RecordSource = lc_TempFile
*- Set grid field sources
=lfGrdSrc(loFormSet)

loFormset.Ariaform1.Refresh()

=lfReadWhen(loFormSet)


*!**************************************************************************
*!
*!      Function: lfUpdTemp
*!
*!**************************************************************************
*
FUNCTION lfUpdTemp
lcOldAlias= ALIAS()

lc_TempFile = loFormSet.lc_TempFile
lcAccount = ""
lcLongDes = ""
lcSortDes = ""

FOR lnCount = 1 TO loFormSet.lnSegNo
**** error
  lcAccount = lcAccount + IIF(EMPTY(lcAccount),"","-") +;
              ALLTRIM(EVALUATE[laSegSiz[lnCount,3]+".cSegValue"])

  lcLongDes = lcLongDes + IIF(EMPTY(lcLongDes),"",IIF(EMPTY(EVALUATE[laSegSiz[lnCount,3]+'.cSegShDes']),"","-")) +;
              ALLTRIM(EVALUATE[laSegSiz[lnCount,3]+".cSeglnDes"])

  lcSortDes = lcLongDes + IIF(EMPTY(lcSortDes),"",IIF(EMPTY(EVALUATE[laSegSiz[lnCount,3]+'.cSegShDes']),"","-")) +;
              ALLTRIM(EVALUATE[laSegSiz[lnCount,3]+".cSegShDes"])

ENDFOR

IF loFormset.rbAction = 1
  IF  !SEEK(lcAccount,'GLACCHAR')
    SELECT (lc_TempFile)
    GO TOP
    IF  RECCOUNT() = 0 .OR. !EMPTY(&lc_TempFile..cAcctcode)
      APPEND FROM &gcWorkDir.&lc_BlankDBF
      GO TOP
    ENDIF

    REPLACE  &lc_TempFile..Include   WITH .T.      ;
             &lc_TempFile..cAcctcode  WITH lcAccount ;
             &lc_TempFile..cAccnsdes  WITH lcSortDes ;
             &lc_TempFile..cAccnldes  WITH lcLongDes ;
             &lc_TempFile..lLok_Stat  WITH .F.       ;
             &lc_TempFile..cLok_User  WITH ''        ;
             &lc_TempFile..dLok_Date  WITH {}        ;
             &lc_TempFile..cLok_Time  WITH ''

    GATHER MEMVAR MEMO
    lnTotRec   = lnTotRec   + 1
    lnCurrunt  = RECNO(lc_TempFile)
    =lfThermo(lnTotAcont,lnCurrunt,'Generating Account:',lcAccount)
  ELSE
    lnTotAcont = lnTotAcont - 1
  ENDIF
ELSE
  IF  SEEK(lcAccount,'GLACCHAR')
    SELECT (lc_TempFile)
    GO TOP
    IF  RECCOUNT() = 0 .OR. !EMPTY(&lc_TempFile..cAcctcode)
      APPEND FROM &gcWorkDir.&lc_BlankDBF
      GO TOP
    ENDIF

    REPLACE  &lc_TempFile..Include   WITH .T.      ;
             &lc_TempFile..cAcctcode  WITH lcAccount ;
             &lc_TempFile..cAccnldes  WITH lcLongDes
    lnTotRec   = lnTotRec   + 1
    lnCurrunt  = RECNO(lc_TempFile)
    =lfThermo(lnTotAcont,lnCurrunt,'Generating Account...',lcAccount)
  ELSE
    lnTotAcont = lnTotAcont - 1
  ENDIF
ENDIF

SELECT &lcOldAlias

*!**************************************************************************
*!
*!      Function: lfvMUpdate
*!
*!**************************************************************************
*
FUNCTION lfvMUpdate
PARAMETERS loFormset

lc_TempFile = loFormSet.lc_TempFile
gcWorkDir = oAriaApplication.WorkDir

lc_Balance =  gfTempName()

lnCurrunt = 0
SELECT (lc_TempFile)

COUNT FOR !EMPTY(Include) TO lnTotAcont

IF lnTotAcont = 0
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen("TRM02103B00000","DIALOG",IIF(loFormset.rbAction = 1,'add','delete'))  
  =gfModalGen("TRM02103B00000","DIALOG",IIF(loFormset.rbAction = 1,;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_MSG_ADD,loFormSet.GetHeaderText("LANG_GLACMEN_MSG_ADD",loFormSet.HeaderAlias)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_MSG_DELETE,loFormSet.GetHeaderText("LANG_GLACMEN_MSG_DELETE",loFormSet.HeaderAlias))))
  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  loFormset.Ariaform1.pbSelOne.Setfocus()
  RETURN
ENDIF

lcEscStat = SET('ESCAPE')
lcOnEsc   = ON('ESCAPE')
SET ESCAPE ON
ON ESCAPE llProcessing = .F.

oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
llProcessing = .T.

*** Adding Accounts
IF loFormset.rbAction = 1
  lcTime = gfGetTime()
  ldDate = DATE()
  SELECT (lc_TempFile)
  REPLACE ALL &lc_TempFile..cAdd_User WITH oAriaApplication.User_Id  ;
              &lc_TempFile..dAdd_Date WITH ldDate     ;
              &lc_TempFile..cAdd_Time WITH lcTime

  SELECT GLACBALS
  COPY STRUCTURE TO &gcWorkDir.&lc_Balance
  SELECT GLACBALS

  FOR lnCount = 1 TO ALEN(loFormSet.laBalRecs,1)
    INSERT INTO &gcWorkDir.&lc_Balance ;
       (cAcctCode,cFisfYear,cFspPrdid,nacbptddr,nacbptdcr,;
        nacbytddr,nacbytdcr,nacbopbal,nacbclbal,cAdd_User,;
        dAdd_Date,cAdd_Time);
        VALUES ('00',loFormSet.laBalRecs[lnCount,1],loFormSet.laBalRecs[lnCount,2],;
        0,0,0,0,0,0,oAriaApplication.User_Id,ldDate,lcTime)
  ENDFOR

  SELECT (lc_TempFile)

  SCAN FOR !EMPTY(Include)
    IF lfTerminate()
      EXIT
    ENDIF

    lnCurrunt = lnCurrunt + 1
    lcAccount = cAcctCode
    =lfThermo(lnTotAcont,lnCurrunt,'Updating Account...',lcAccount)

    IF !SEEK(lcAccount,'GLACCHAR')
      SCATTER MEMVAR MEMO
      SELECT GLACCHAR
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT (lc_Balance)
      REPLACE ALL &lc_Balance..cAcctCode With &lc_TempFile..cAcctCode
      SELECT GLACBALS
      APPEND FROM &gcWorkDir.&lc_Balance
      SELECT (lc_TempFile)
      DELETE
    ENDIF
    SELECT (lc_TempFile)
  ENDSCAN

  IF USED(lc_Balance)
    USE IN ALIAS(lc_Balance)
  ENDIF
  ERASE &gcWorkdir.&lc_Balance..DBF


ELSE    && *** Deleting Accounts

  SCAN FOR !EMPTY(Include)

    IF lfTerminate()
      EXIT
    ENDIF
    lnCurrunt = lnCurrunt + 1
    lcAccount = cAcctCode
    *N000682,1 12/21/12 TMI Globlization changes[Start] 
    *=lfThermo(lnTotAcont,lnCurrunt,'Deleting Account...',lcAccount)
    lcMsg =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Deleting_Account,loFormSet.GetHeaderText("LANG_Deleting_Account",loFormSet.HeaderAlias)) &&'Deleting Account...'
    =lfThermo(lnTotAcont,lnCurrunt,lcMsg,lcAccount)
    *N000682,1 12/21/12 TMI Globlization changes[End  ] 

    SELECT GLACCHAR
    SET ORDER TO TAG ACCTCODE
    SEEK(lcAccount)

    IF gfObj_Lock(.T.)
      SELECT (lc_TempFile)
      IF lfDelAcnt(lcAccount)
        DELETE
      ELSE
        SELECT GLACCHAR
        gfObj_Lock(.F.)
      ENDIF
    ENDIF
    SELECT (lc_TempFile)
  ENDSCAN
ENDIF

ON  ESCAPE &lcOnEsc
SET ESCAPE &lcEscStat

IF lnCurrunt <> lnTotAcont
  =lfThermo(100,100,' ',' ')
ENDIF
oProgress = NULL

loFormset.Ariaform1.grdCreatedAccounts.RecordSource =  ' '

SELECT GLACCHAR
gfTableUpdate()

*B610137,1 TMI 10/31/2012 [Start] update the GLACBALS file
SELECT GLACBALS
gfTableUpdate()
*B610137,1 TMI 10/31/2012 [End  ]


SELECT (lc_TempFile)
PACK
GO TOP

*- Re Set grid field sources
loFormset.Ariaform1.grdCreatedAccounts.RecordSource =  lc_TempFile
=lfGrdSrc(loFormSet)

=lfReadWhen(loFormSet)

loFormSet.Ariaform1.pbGenerat.Enabled = .F.



*!**************************************************************************
*!
*!      Function: lfvAction
*!
*!**************************************************************************
*
FUNCTION lfvAction
PARAMETERS loFormset,loFld

IF loFormset.rbAction = loFormSet.lnAction
  RETURN
ELSE
  loFormset.rbAction = loFormSet.lnAction
  loFormSet.Ariaform1.rbAction.Refresh()
ENDIF

IF RECCOUNT(loFormSet.lc_TempFile) > 0
  IF gfModalGen("TRM02207B00012","DIALOG") = 1
    SELECT (loFormSet.lc_TempFile)
    ZAP
    =lfReadWhen(loFormSet)
  ELSE
    RETURN
  ENDIF
ENDIF


loFormset.rbAction = IIF(loFormset.rbAction=1,2,1)
loFormSet.lnAction = loFormset.rbAction
loFormSet.Ariaform1.rbAction.Refresh()

IF RECCOUNT(loFormSet.lc_TempFile) > 0
  lcObjUpDat = 'ENABLE'
ELSE
  lcObjUpDat = 'DISABLE'
ENDIF

IF loFormset.rbAction = 1
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.Ariaform1.pbUpdate.Caption = 'Add to \<Master File'
  loFormSet.Ariaform1.pbUpdate.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_ADD_TO_MASTER_FILE,loFormSet.GetHeaderText("LANG_GLACMEN_ADD_TO_MASTER_FILE",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]
ELSE
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  loFormSet.Ariaform1.pbUpdate.Caption = 'D\<elete from Master File'
  loFormSet.Ariaform1.pbUpdate.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_DELETE_FROM_MASTER_FILE,loFormSet.GetHeaderText("LANG_GLACMEN_DELETE_FROM_MASTER_FILE",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]
ENDIF
loFormSet.Ariaform1.pbUpdate.Enabled = lcObjUpDat = 'ENABLE'

loFormSet.Ariaform1.Refresh()
*- End of lfvAction.

*!**************************************************************************
*!
*!      Function:  lfCheckBut
*!
*!**************************************************************************
*
FUNCTION  lfCheckBut
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT (loFormSet.lc_TempFile)

WITH loFormSet.Ariaform1
  IF RECCOUNT() > 0
    IF !EMPTY(Include)
      *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
      *      .pbSelOne.Caption = '\<Unselect'
      .pbSelOne.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_UNSELECT,loFormSet.GetHeaderText("LANG_GLACMEN_UNSELECT",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]
    ELSE
      *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
      *      .pbSelOne.Caption = '\<Select'
      .pbSelOne.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_SELECT,loFormSet.GetHeaderText("LANG_GLACMEN_SELECT",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]
    ENDIF
    lcObjUpDat = 'ENABLE'
  ELSE
    lcObjUpDat = 'DISABLE'
  ENDIF

  .pbSelOne.Enabled = lcObjUpDat = 'ENABLE'
  .pbSelAll.Enabled = lcObjUpDat = 'ENABLE'
  .pbSelNon.Enabled = lcObjUpDat = 'ENABLE'
  .pbInvert.Enabled = lcObjUpDat = 'ENABLE'
  .pbUpdate.Enabled = lcObjUpDat = 'ENABLE'
ENDWITH


SELECT (lnSlct)
*- End of lfCheckBut.

*!**************************************************************************
*!					
*!      Function:  lfvSelOne
*!
*!**************************************************************************
*
FUNCTION  lfvSelOne
PARAMETERS loFormset

SELECT (loFormSet.lc_TempFile)
REPLACE Include WITH !Include
=lfCheckBut(loFormSet)


*!**************************************************************************
*!
*!      Function:  lfvSelAll
*!
*!**************************************************************************
*

FUNCTION  lfvSelAll
PARAMETERS loFormset

SELECT (loFormSet.lc_TempFile)
lnRecNo = RECNO()
REPLACE ALL Include WITH .T.

IF lnRecNo > RECCOUNT()
  GO BOTTOM
ELSE
  GOTO lnRecNo
ENDIF

=lfCheckBut(loFormSet)


*!**************************************************************************
*!
*!      Function:  lfvSelNon
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon
PARAMETERS loFormset

SELECT (loFormSet.lc_TempFile)
lnRecNo = RECNO()
REPLACE ALL Include WITH .F.

IF lnRecNo > RECCOUNT()
  GO BOTTOM
ELSE
  GOTO lnRecNo
ENDIF

=lfCheckBut(loFormSet)

*!**************************************************************************
*!
*!      Function:  lfvInvert
*!
*!**************************************************************************
*
FUNCTION  lfvInvert
PARAMETERS loFormset

SELECT (loFormSet.lc_TempFile)
lnRecNo = RECNO()
REPLACE ALL Include WITH !Include

IF lnRecNo > RECCOUNT()
  GO BOTTOM
ELSE
  GOTO lnRecNo
ENDIF

=lfCheckBut(loFormSet)


*!**************************************************************************
*!
*!      Function: lfvCancRan
*!
*!**************************************************************************
*
FUNCTION lfvCancRan
llSegChang = .F.

*!**************************************************************************
*!
*!      Function: lfvOkRange
*!
*!**************************************************************************
*
FUNCTION lfvOkRange

llSegChange = .T.
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvExclud
*!
*!**************************************************************************
*
FUNCTION lfvExclud
PARAMETERS loRngFormSet,loBtn
LOCAL loFormSet
loFormSet = loRngFormSet.loFormSet

DECLARE laSource[1]

lnAliasNum = INT(VAL(RIGHT(loBtn.Name,1)))

lcFromSeg  = "loFormSet.lcSegRng"+STR(lnAliasNum,1)+"1"
lcTOSeg    = "loFormSet.lcSegRng"+STR(lnAliasNum,1)+"2"
lcExcAry   = "laExcAry"+STR(lnAliasNum,1)

SELECT ALLTRIM(cSegValue)+' '+cSeglnDes ;
       FROM (loFormSet.laSegsiz[lnAliasNum,3]);
       WHERE cAcsSegNo = STR(lnAliasNum,1)   .AND.;
             cSegValue>= EVALUATE(lcFromSeg) .AND.;
             cSegValue<= EVALUATE(lcTOSeg);
       ORDER BY  cSegValue ;
       INTO ARRAY laSource

FOR lnI = 1 TO ALEN(laSource)
  lnElement = ASCAN(loFormSet.&lcExcAry,laSource[lnI])
  IF lnElement > 0
    laSource[lnI] = '\'+laSource[lnI]
  ENDIF
ENDFOR

DIMENSION &lcExcAry[ALEN(loFormSet.&lcExcAry)]
ACOPY(loFormSet.&lcExcAry,&lcExcAry)
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*=gfMover(@laSource,@&lcExcAry,"Exclude segment values",.T.)
=gfMover(@laSource,@&lcExcAry,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLACMEN_MOVER_CAPTION,oAriaApplication.GetHeaderText("LANG_GLACMEN_MOVER_CAPTION",loRngFormSet.HeaderAlias,loRngFormSet.DataSessionID)),.T.)
*N000682,1 MMT 11/22/2012 Globalization changes[End]
DIMENSION loFormSet.&lcExcAry[ALEN(&lcExcAry)]
ACOPY(&lcExcAry,loFormSet.&lcExcAry)

RELEASE laSource

*!**************************************************************************
*!
*!      Function: lfTerminate
*!
*!**************************************************************************
*
FUNCTION lfTerminate

IF ! llProcessing
  *Do you want to terminate the processing?
  * \!\<Terminate;\?\<Resume
  IF gfModalGen("TRM02209B02019",'ALART') = 1
    RETURN .T.
  ENDIF
ENDIF

llProcessing = .T.
RETURN .F.



************************************************************
*! Name      : lfCritFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/16/2012
*! Purpose   : Segment selectore form
************************************************************
FUNCTION lfCritFormInit
PARAMETERS loCritForm

LOCAL lnI,lcI,lnH
WITH loCritForm.ariaform1
  FOR lnI = 1 TO loFormset.lnAcsNoSeg

    lcI = STR(lnI,1)
    .lblSeg&lcI..Visible = .T.
    .lblSeg&lcI..Caption = loFormSet.laSegSiz[lnI,2]
    .lblColon&lcI..Visible = .T.
    .txtSegFr&lcI..Visible = .T.
    .txtSegTo&lcI..Visible = .T.
    .cmdExclude&lcI..Visible = .T.

    .txtSegFr&lcI..ControlSource = 'Thisformset.loFormset.lcSegRng&lcI.1'
    .txtSegTo&lcI..ControlSource = 'Thisformset.loFormset.lcSegRng&lcI.2'

    .txtSegFr&lcI..InputMask = REPLICATE('9',loFormset.laSegSiz[lnI,1])
    .txtSegTo&lcI..InputMask = REPLICATE('9',loFormset.laSegSiz[lnI,1])

  ENDFOR
  lnH = .txtSegFr&lcI..Height + 2
  .lblDesc.Top = .txtSegFr&lcI..Top + lnH
  .lblColonDesc.Top = .txtSegFr&lcI..Top + lnH
  .txtDesc.Top = .txtSegFr&lcI..Top + lnH
  .cmdOk.Top   = .txtDesc.Top + lnH
  .cmdCancel.Top = .txtDesc.Top + lnH
  .Height = .cmdOk.Top + lnH
ENDWITH
*- End of lfCritFormInit.

************************************************************
*! Name      : lfThermo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/06/2012
*! Purpose   : lfThermo
************************************************************
FUNCTION lfThermo
PARAMETERS lnTotRecs, lnThermRec, lcPstTB,lcTranNo
lcPstTB = IIF(EMPTY(lcPstTB),'',lcPstTB)
lcTranNo = IIF(EMPTY(lcTranNo),'',lcTranNo)
WITH oProgress
  .TotalProgress = lnTotRecs
  .lblFirstLabel.CAPTION = lcPstTB
  .lblSecondLabel.CAPTION = lcTranNo
  .CurrentProgress(lnThermRec)
  .SHOW()
ENDWITH

