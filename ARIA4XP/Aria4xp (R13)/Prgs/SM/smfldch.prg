*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMFLDCH.Prg
*:  Module      : System Manager
*:  Desc.       : Global Changes screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/21/2013
*:  Reference   :
*E303425,1 TMI 10/21/2013 [T20130716.0014 ]
*:************************************************************************
*B610613,1 TMI 12/05/2013 09:55 [Start] fix problem of not saving or loading filters
*B610613,3 TMI 12/11/2013 remove the MAKE field [T20130716.0014]
*                         Resolve a problem that an old value can be added more than one time
*B610654,1 TMI 01/06/2014 fix problems arose while testing the build [T20130716.0014]
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019]
*B611660,1 SAH 09/09/2018 modify the program as the Key Change program does not update the SQL tables when style Non major is changed [T20180629.0006]
*E611964,1 Es 10/21/2019  Change size scales from one size scale to another [P20190901.0001]
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002]
*B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003]
*B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002]
*B612139,1 MMT 05/10/2020 Key change does not update all selected styles scale while changing scale sizes[T20200202.0002]
*B612447,1 MMT 08/12/2021 Key Change Corrupts store IDs if it starts with same characters[T20210806.0001]
*B612706,1 MMT 02/28/2024 The key Change program should read scale width from setups in all cases not in case of extended size scale case only [T-ERP-20240209.0008]
*E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002]
*:************************************************************************
PARAMETERS lcPassedType

#INCLUDE R:\Aria4xp\Screens\SM\SMFLDCH.H
*E611964,1 Es 10/21/2019  Change size scales from one size scale to another [Start]
PUBLIC  laScopIMP ,laScopExpr,laScopeSty,laScopeMajor,laScopeLoc,laScopeFabric
*E611964,1 Es 10/21/2019  Change size scales from one size scale to another [End]

LOCAL oFrm
lcRunScx = lfGetScx("SM\SMFLDCH.scx")
DO FORM (lcRunScx) NAME oFrm NOSHOW WITH lcPassedType
IF TYPE('oFrm') = 'O' AND !ISNULL(oFrm)
  oFrm.Show(1)
ENDIF

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
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

************************************************************
*! Name      : lfSMFLDCHFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/21/2013
*! Purpose   : init form function
************************************************************
FUNCTION lfSMFLDCHFormInit
PARAMETERS loFormSet
LOCAL lnSlct,llLock
lnSlct = SELECT(0)

*B610613,1 TMI 12/05/2013 09:29 [Start] test code
IF .F. AND SYS(0) = 'SDE_TAREK # tarek'  && remove this when tested
*B610613,1 TMI 12/05/2013 09:29 [End  ]
  IF !lfChkUsrScr(loFormSet)
    RETURN .F.
  ENDIF
*B610613,1 TMI 12/05/2013 09:29 [Start]
ENDIF
*B610613,1 TMI 12/05/2013 09:29 [End  ]

*- Define several variables needed in the form
=lfDefineVars(loFormSet)

=lfvComp(loFormSet)

=lfDefControlSource(loFormSet)

loFormSet.ChangeMode('V')

SELECT (lnSlct)
*- End of lfSMFLDCHFormInit.

************************************************************
*! Name      : lfChkUsrScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/03/2013
*! Purpose   : check if there are more than one user logged, check if there are more than one screen open
************************************************************
FUNCTION lfChkUsrScr
PARAMETERS loFormSet

*- if a user logged in the system then do not open the screen
IF  gfUserList(.T.)>1
  gfModalGen('INM00025B00000','DIALOG')
  RETURN .F.
ENDIF

*- if the current user opens other screen then don't open this screen
LOCAL lnDataSession,lnWinCnt
lnDataSession = SET("Datasession")
SET DATASESSION TO 1
SELECT syustatc
COUNT TO lnWinCnt;
  FOR oAriaApplication.Station = cStation AND COBJ_TYP = 'WIN'
IF lnWinCnt > 1
  gfModalGen('INM00160B00000','DIALOG')
  SET DATASESSION TO lnDataSession
  RETURN .F.
ENDIF
SET DATASESSION TO lnDataSession

*- if you opened the screen then lock the system
loFormSet.Addproperty('llSYCINST_Opn',.F.)
IF !USED('SYCINST')
  loFormSet.llSYCINST_Opn = gfOpenFile(oAriaApplication.SysPath+'SYCINST','','SH')
ENDIF
llLock = gfLOCKSYS(.T.)
IF !llLock
  RETURN .F.
ENDIF

*- End of lfChkUsrScr.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/21/2013
*! Purpose   : Defining the screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

IF TYPE('loFormset.lFirstRun') = 'U'
  loFormSet.AddProperty('lFirstRun',.F.)

  lcPath = oAriaApplication.ApplicationHome
  SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE

  loFormSet.AddProperty('lcKeyType' , '' )
  loFormSet.AddProperty('lnFiles' , 0 )
  loFormSet.AddProperty('lnfields' , 0 )

*-- Get the current company information
  loFormSet.AddProperty('laComp[1,3]' , '' )
  loFormSet.laComp[1,1] = oAriaApplication.ActiveCompanyName
  loFormSet.laComp[1,2] = oAriaApplication.ActiveCompanyID
  loFormSet.laComp[1,3] = oAriaApplication.DataDir
  loFormSet.AddProperty('lnComp' , 1 )

  loFormSet.AddProperty('lnClrPo',0)
  loFormSet.AddProperty('lnColorLen',0)
  loFormSet.AddProperty('llColorExt',.F.)
  loFormSet.AddProperty('lcTmpStyle', '')
  loFormSet.AddProperty('lcStyMajor',gfItemMask('HM'))
  loFormSet.AddProperty('llNonMjExt',!EMPTY(gfItemMask('HN')))

  loFormSet.AddProperty('lcKeyVal','' )        && Holds the value of primary key
  loFormSet.AddProperty('lcPKey','' )          && Holds the value of primary key that stored in loFormSet.lcKeyVal
  loFormSet.AddProperty('lcOldKey','' )        && Holds the old value of key that will be changed
  loFormSet.AddProperty('lcNewKey','' )        && Holds the new value of key that will be changed
  loFormSet.AddProperty('lcKeyTemp','' )       && Holds the Temporary value of loFormSet.lcKeyVal
  loFormSet.AddProperty('lnkeys',1  )          && Holds the numbers of keys that stored in SYDFDVLD.mFull_exp

  loFormSet.AddProperty('laMajSeg[1,1]',.F.)

  loFormSet.AddProperty('laFiles[1,2]','')
  loFormSet.AddProperty('laFields[1,7]','')
  loFormSet.AddProperty('lcMaj','')
  loFormSet.AddProperty('lcNMjrPt','')
  loFormSet.AddProperty('lcValues',gfTempName())
  loFormSet.AddProperty('lcHistory',gfTempName())
  loFormSet.AddProperty('lcNValues',gfTempName())
  loFormSet.AddProperty('laCodInfo[1,10]','')
  loFormSet.AddProperty('laCodInf[1,10]','')
  loFormSet.AddProperty('lcOldfil','')
  loFormSet.AddProperty('lcOldOrd','')
  loFormSet.AddProperty('lcCust','')
  loFormSet.AddProperty('lcCusdp','')
  loFormSet.AddProperty('lcRep','')
  loFormSet.AddProperty('lcOld','')
  loFormSet.AddProperty('lcScalePic','')
  loFormSet.AddProperty('laFullExp[loFormSet.lnkeys,4]','')

  loFormSet.AddProperty('llBrowse',.F.)
  loFormSet.AddProperty('llNoShow',.F.)
  loFormSet.AddProperty('llMScale',.F.)
  loFormSet.AddProperty('llSclExist',.F.)

  loFormSet.AddProperty('llNew',.T.)
  loFormSet.AddProperty('llFstSr',.T.)

  loFormSet.AddProperty('lnMajSize',0)
  loFormSet.AddProperty('lnMajSeg',0)
  loFormSet.AddProperty('lnOldRecNo',0)
  loFormSet.AddProperty('lnActFolder',1)
  loFormSet.AddProperty('lnLastFold',1)
  loFormSet.AddProperty('lnLenght',0)
  loFormSet.AddProperty('lcOldVal','')
  loFormSet.AddProperty('lcNewVal','')

  loFormSet.AddProperty('lnMajSegmt','')
  loFormSet.AddProperty('lcfdTit', 'Field')

*-- loFormSet.lcMaj      && Style major picture
*-- loFormSet.lcNMjrPt   && Style Non Major picture
*-- loFormSet.lnMajSize  && Style Major length
*-- loFormSet.lnMajSeg   && Number of segments in the style code structure
*-- Array to hold information of style segments

*-- Title name of the browse of open keys and values
  loFormSet.AddProperty('lcbrwOpen', 'Open '+IIF(loFormSet.lcKeyType='F','Fields','Keys'))

*-- Title name of the browse of History keys and values
  loFormSet.AddProperty('lcbrwHst', 'History')

  loFormSet.AddProperty('lcDummyCom')
  loFormSet.lcDummyCom = REPLICATE("~",2)

  loFormSet.AddProperty('lcBrTtl', 'Values')
  loFormSet.AddProperty('laFixFltr[1,7]','')

*E303425,3 TMI 11/24/2013 18:04 [Start]
  loFormSet.AddProperty('lcFabMaj')
  loFormSet.AddProperty('lnFabMajSize')
*E303425,3 TMI 11/24/2013 18:04 [End  ]

*B610654,1 TMI 01/08/2014 16:19 [Start] width of scale when the company is extended
  loFormSet.AddProperty('M_EXTWIDTH',gfGetMemVar('M_EXTWIDTH'))
*B610654,1 TMI 01/08/2014 16:19 [End  ]

ENDIF

*- open system tables
=gfOpenFile(oAriaApplication.SysPath+'SYDFIELD', 'cFld_Name','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYDFILES', 'cFile_Nam','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYDFLFLD', 'CFile_Nam','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYDFDCHG', 'SYDFDCHG','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYDFDVLD', 'SYDFDVLD','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYDINDEX', 'CFILE','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','','SH')

SELECT SYCCOMP
GO TOP
IF EOF()
*** There is no companies available.  You have ***
*** to enter companies first before you enter ***
*** their codes...
*** <  Ok  > ***
  =gfModalGen("TRM00223B00000","DIALOG")
  glQuitting = .T.
  RETURN
ENDIF

*-- Create the temporary file Based on the KEY_CHG file , to work in it.

*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
*!*  CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcValues) (cKeyType C(1), cKey_Chg C(oAriaApplication.FieldW),Old_Key C(30),;
*!*            New_Key C(30) , cFld_Head C(30), cStatus C(1) , ENTERED D(10), ;
*!*            lKey L(1) , mSaveFilt M(10))mmsa
*!*  INDEX ON cKeyType+cKey_Chg+Old_Key TAG (loFormSet.lcValues)
CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcValues) (cKeyType C(1), cKey_Chg C(oAriaApplication.FieldW),Old_Key C(30),;
  New_Key C(30) , cFld_Head C(30), cStatus C(1) , ENTERED D(10), ;
  lKey L(1) , mSaveFilt M(10),Note C(30) ,  MSIZES M(10))
INDEX ON cKeyType+cKey_Chg+Old_Key TAG (loFormSet.lcValues)
*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]

*-- Create a temproray file to browse the history data from it.

*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
*!*  CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcHistory) (cKeyType C(1), cKey_Chg C(oAriaApplication.FieldW),Old_Key C(30),;
*!*            New_Key C(30) , cFld_Head C(30), ENTERED D(10), Date_Procd D(10),;
*!*             lKey L(1) )
*!*  INDEX ON cKeyType+cKey_Chg+Old_Key TAG (loFormSet.lcHistory)

CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcHistory) (cKeyType C(1), cKey_Chg C(oAriaApplication.FieldW),Old_Key C(30),;
  New_Key C(30) , cFld_Head C(30), ENTERED D(10), Date_Procd D(10),;
  lKey L(1) ,Note C(30) ,  MSIZES M(10))
INDEX ON cKeyType+cKey_Chg+Old_Key TAG (loFormSet.lcHistory)
*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]

*-- Use it again with another alias to make two browses based on it.
USE (oAriaApplication.WorkDir+loFormSet.lcValues) AGAIN ALIAS (loFormSet.lcNValues) ORDER TAG (loFormSet.lcValues) IN 0


loFormSet.lcKeyType = loFormSet.lcPassedType

loFormSet.AriaForm1.lnFiles.Visible = loFormSet.lcKeyType = 'F'
loFormSet.AriaForm1.lblFiles.Visible = loFormSet.lcKeyType = 'F'
IF !loFormSet.AriaForm1.lnFiles.Visible
  WITH loFormSet.AriaForm1.pgfChange
    .Top = 3
    .Height = .Height + 45
    .Ariapage1.Ariagrid1.Height = .Ariapage1.Ariagrid1.Height + 45
    .Ariapage2.Ariagrid1.Height = .Ariapage2.Ariagrid1.Height + 45
    loFormSet.AriaForm1.Ariashape1.Visible = .F.
  ENDWITH
ENDIF

*- End of lfDefineVars.

************************************************************
*! Name      : lfDefControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/08/2012
*! Purpose   : Define Control Source
************************************************************
FUNCTION lfDefControlSource
PARAMETERS loFormSet

WITH loFormSet.AriaForm1
  .lnFiles.ControlSource = 'Thisformset.lnFiles'
  .lnFiles.Rowsource = 'Thisformset.laFiles'

  .pgfChange.Ariapage1.lnfields.ControlSource = 'Thisformset.lnFields'
  .pgfChange.Ariapage1.lnfields.RowSource = 'Thisformset.laFields'
ENDWITH

loGrdKey = loFormSet.AriaForm1.pgfChange.Ariapage1.Ariagrid1

*E611964,1 Es 10/21/2019  Change size scales from one size scale to another [Start]
*loGrdKey.ColumnCount = 4
loGrdKey.ColumnCount = 5
*E611964,1 Es 10/21/2019  Change size scales from one size scale to another [End]

loGrdKey.RecordSource = ''
loGrdKey.RecordSource = loFormSet.lcNValues
lcNValues = loFormSet.lcNValues
lcHeader1 = IIF(loFormSet.lcKeyType='K',LANG_KEY,LANG_FIELD)
lfSetColumnsProp(loGrdKey,'1',"&lcNValues..cFld_Head" ,lcHeader1,100)
lfSetColumnsProp(loGrdKey,'2',"IIF(&lcNValues..lKey,LOOKUP(CODES.cDiscrep, 'N'+ALLTRIM(&lcNValues..Old_Key)+'N'+ALLTRIM(&lcNValues..cKey_Chg), CODES.cCode_No,'CODES'),&lcNValues..Old_Key)" ,LANG_OLD_VALUE ,160)
lfSetColumnsProp(loGrdKey,'3',"IIF(&lcNValues..lKey,LOOKUP(CODES.cDiscrep, 'N'+ALLTRIM(&lcNValues..New_Key)+'N'+ALLTRIM(&lcNValues..cKey_Chg), CODES.cCode_No,'CODES'),&lcNValues..New_Key)" ,LANG_NEW_VALUE ,160)
lfSetColumnsProp(loGrdKey,'4',"&lcNValues..ENTERED" ,LANG_DATE ,100)

*E611964,1 Es 10/21/2019  Change size scales from one size scale to another [Start]
lfSetColumnsProp(loGrdKey,'5',"&lcNValues..Note" ,LANG_Comment ,100)
*E611964,1 Es 10/21/2019  Change size scales from one size scale to another [End]
loGrdKey.Readonly = .T.
loGrdKey.Refresh()

loGrdHst = loFormSet.AriaForm1.pgfChange.Ariapage2.Ariagrid1

*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
*loGrdKey.ColumnCount = 4
loGrdHst.ColumnCount = 6
*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]
loGrdHst.RecordSource = ''
loGrdHst.RecordSource = loFormSet.lcHistory
lcHistory = loFormSet.lcHistory
lfSetColumnsProp(loGrdHst,'1',"&lcHistory..cFld_Head" ,lcHeader1,100)
lfSetColumnsProp(loGrdHst,'2',"IIF(&lcHistory..lKey,LOOKUP(CODES.cDiscrep, 'N'+ALLTRIM(&lcHistory..Old_Key)+'N'+ALLTRIM(&lcHistory..cKey_Chg), CODES.cCode_No,'CODES'),&lcHistory..Old_Key)" ,LANG_OLD_VALUE ,160)
lfSetColumnsProp(loGrdHst,'3',"IIF(&lcHistory..lKey,LOOKUP(CODES.cDiscrep, 'N'+ALLTRIM(&lcHistory..New_Key)+'N'+ALLTRIM(&lcHistory..cKey_Chg), CODES.cCode_No,'CODES'),&lcHistory..New_Key)" ,LANG_NEW_VALUE ,160)
lfSetColumnsProp(loGrdHst,'4',"&lcHistory..ENTERED"   ,LANG_ENTERED   ,100)
lfSetColumnsProp(loGrdHst,'5',"&lcHistory..Date_Procd",LANG_Date_Procd,100)
*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
*!*  oGrd,lcCol,lcSrc,lcHeader,lnWidth
lfSetColumnsProp(loGrdHst,'6',"&lcHistory..Note",LANG_Comment,100)
*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]
loGrdHst.Readonly = .T.
loGrdHst.Refresh()

*- End of lfDefControlSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS oGrd,lcCol,lcSrc,lcHeader,lnWidth
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH oGrd
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth

*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
  IF (INT(VAL(ALLTRIM(lcCol))))==6
    .Column&lcCol..forecolor= rgb(255,0,0)
  ENDIF
*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]
ENDWITH
*- End of lfSetColumnsProp.

*!*************************************************************
*! Name      : lfvComp
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the company POPUP
*!*************************************************************
*! Calls       : FUNCTIONS : lfCrtFiles(), gfItemMask()
*!               PROCEDURES: None.
*!*************************************************************
FUNCTION lfvComp
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

IF loFormSet.laComp[loFormSet.lnComp,2] = loFormSet.lcDummyCom
  DIMENSION loFormSet.laFiles[1,2], loFormSet.laFields[1,7]
  STORE SPACE(0) TO loFormSet.laFiles
  RETURN
ELSE
*-- the file of the required company.
  =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'Codes','Codes','SH')
  *E611964,1 Es 10/21/2019  Change size scales from one size scale to another [P20190901.0001][Start]
  loFormSet.llMScale   = gfGetMemVar('M_USEEXSSC',loFormSet.laComp[loFormSet.lnComp,2])
  *E611964,1 Es 10/21/2019  Change size scales from one size scale to another [P20190901.0001][End]
*-- Fill the pop up of files or  keys
  IF !lfCrtFiles(loFormSet)
    RETURN
  ENDIF

  IF FILE(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICISTRU.DBF')
    llICISTRU = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICISTRU','Segno','SH')
    SELECT ICISTRU
    IF SEEK('U1')
*-- Get the major Picture of the style
      loFormSet.lcMaj     = gfItemMask('PM',ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3]))
*-- Length of the major
      loFormSet.lnMajSize = LEN(loFormSet.lcMaj)
*-- Picture of non major
      loFormSet.lcNMjrPt  = gfItemMask('PN',ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3]))
*-- Number of major segments
      loFormSet.lnMajSeg  = gfItemMask('SM',ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3]))    && No. of major segments.

*E303425,3 TMI 11/24/2013 18:03 [Start]
*-- Get the major Picture of the FABRIC
      loFormSet.lcFabMaj     = gfItemMask('PM',ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3]),'0002')
*-- Length of the major
      loFormSet.lnFabMajSize = LEN(loFormSet.lcFabMaj)
*E303425,3 TMI 11/24/2013 18:03 [End  ]

      DIMENSION laMajSeg[ALEN(loFormSet.laMajSeg,1),ALEN(loFormSet.laMajSeg,2)]
      ACOPY(loFormSet.laMajSeg,laMajSeg)
      =gfItemMask(@laMajSeg,ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3]))
      DIMENSION loFormSet.laMajSeg[ALEN(laMajSeg,1),ALEN(laMajSeg,2)]
      ACOPY(laMajSeg,loFormSet.laMajSeg)
    ENDIF
    IF llICISTRU AND USED('ICISTRU')
      gfCloseTable('ICISTRU')
    ENDIF
  ENDIF

*-- Check for company Using Extended Size Scale.
  *E611964,1 Es 10/21/2019  Change size scales from one size scale to another [P20190901.0001][Start]
  *loFormSet.llMScale   = gfGetMemVar('M_USEEXSSC',loFormSet.laComp[loFormSet.lnComp,2])
  *E611964,1 Es 10/21/2019  Change size scales from one size scale to another [P20190901.0001][End]
  loFormSet.lnLenght =  IIF(SEEK('SCALE','SYDFIELD'),SYDFIELD.nFld_Wdth,0)
  lcScalePic = REPLICATE('X',loFormSet.lnLenght)  && Extended size scale ID picture.
ENDIF

SELECT (lnSlct)
*- End of lfvComp.

************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/21/2013
*! Purpose   : Destroy method
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

*-- Close data and erase the temporary tables
IF USED(loFormSet.lcNValues)
  USE IN (loFormSet.lcNValues)
ENDIF
IF USED(loFormSet.lcValues)
  USE IN (loFormSet.lcValues)
  ERASE (oAriaApplication.WorkDir+loFormSet.lcValues+'.DBF')
  ERASE (oAriaApplication.WorkDir+loFormSet.lcValues+'.CDX')
ENDIF
IF USED('KEY_CHG')
  =gfCloseTable('KEY_CHG')
ENDIF
IF USED(loFormSet.lcHistory)
  USE IN (loFormSet.lcHistory)
  ERASE (oAriaApplication.WorkDir+loFormSet.lcHistory+'.DBF')
  ERASE (oAriaApplication.WorkDir+loFormSet.lcHistory+'.CDX')
ENDIF

**** release the system lock
*B610613,1 TMI 12/05/2013 09:28 [Start] test code
IF .F.
*B610613,1 TMI 12/05/2013 09:29 [End  ]
  =gfLOCKSYS(.T.)

  IF loFormSet.llSYCINST_Opn
    SELECT SYCINST
    LOCATE
    REPLACE LLOCKSYS WITH .F.
    =gfTableUpdate(.T.)
    USE IN SYCINST
  ENDIF

*B610613,1 TMI 12/05/2013 09:29 [Start]
ENDIF
*B610613,1 TMI 12/05/2013 09:29 [End  ]

*- Close tables
IF USED('SYDFDCHG')
  =gfCloseTable('SYDFDCHG')
ENDIF
IF USED('SYDFDVLD')
  =gfCloseTable('SYDFDVLD')
ENDIF
IF USED('SYDFIELD')
  =gfCloseTable('SYDFIELD')
ENDIF
IF USED('SYDFILES')
  =gfCloseTable('SYDFILES')
ENDIF
IF USED('SYDFLFLD')
  =gfCloseTable('SYDFLFLD')
ENDIF
IF USED('SYDINDEX')
  =gfCloseTable('SYDINDEX')
ENDIF
IF USED('SYCCOMP')
  =gfCloseTable('SYCCOMP')
ENDIF
*- End of lfFormDestroy.

************************************************************
*! Name      : lfvChangType
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/21/2013
*! Purpose   : determine the change type
*:           : 'K' Key change program
*:           : 'F' Field change program
************************************************************
FUNCTION lfvChangType
PARAMETERS loFormSet,loFld
IF loFld.Value <> 0
  loFormSet.lcKeyType = IIF(loFld.Value = 1 , 'K', 'F' )
  loFormSet.AriaForm1.pgfChange.Enabled = .T.
  loFormSet.AriaForm1.lnFiles.Visible = loFormSet.lcKeyType = 'F'
  loFormSet.AriaForm1.lblFiles.Visible = loFormSet.lcKeyType = 'F'
ENDIF
*- End of lfvChangType.

*!*************************************************************
*! Name      : lfCrtFiles
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To fill the POPUP of files or keys.
*!*************************************************************
*! Calls       : FUNCTIONS  : gfOpenTable(), lfBldTmp(loFormSet), lfbrwOpn(loFormSet)
*!               Procedures : None
*!*************************************************************
FUNCTION lfCrtFiles
PARAMETERS loFormSet

= gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'KEY_CHG', 'KEY_CHG','SH')
IF loFormSet.lcKeyType = 'F'
  DIMENSION loFormSet.laFiles[1,2]
  STORE SPACE(0) TO loFormSet.laFiles
*-- Fill the files array
  SELECT DISTINCT SYDFILES.cFile_ttl, SYDFDVLD.cFile_nam  ;
    FROM SYDFDVLD, SYDFILES ;
    WHERE SYDFDVLD.cKeyType+SYDFDVLD.cFile_nam = 'F'+SYDFILES.cFile_nam ;
    .AND. FILE(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+ALLTRIM(SYDFDVLD.cFile_nam)+'.DBF');
    INTO ARRAY loFormSet.laFiles
  IF _TALLY = 0
    WAIT WINDOW 'No files available in this company.'
    RETURN .F.
  ENDIF

  DIMENSION loFormSet.laFiles[ALEN(loFormSet.laFiles,1)+1,2]
  = AINS(loFormSet.laFiles,1)
  loFormSet.laFiles[1] = "Select a File"
  loFormSet.laFiles[2] = loFormSet.lcDummyCom
  loFormSet.lnFiles    = 1

  DIMENSION loFormSet.laFields[ALEN(loFormSet.laFields,1)+1,7]
  = AINS(loFormSet.laFields,1)
  loFormSet.laFields[1] = "All"
  loFormSet.laFields[2] = loFormSet.lcDummyCom
  loFormSet.lnfields    = 1

ELSE
*-- Fill the field keys array
  SET ENGINEBEHAVIOR 70


*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
*SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
SYDFIELD.NFld_wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry,SYDFDVLD.mFull_exp ;
FROM SYDFDVLD,SYDFIELD ;
WHERE SYDFDVLD.cKeyType+SYDFDVLD.cFld_name = 'K'+SYDFIELD.cFld_Name ;
INTO ARRAY loFormSet.laFields
  
  IF loFormSet.llMScale
    SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
      SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
      SYDFIELD.nFld_Wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry,SYDFDVLD.mFull_exp ;
      FROM SYDFDVLD,SYDFIELD ;
      WHERE SYDFDVLD.cKeyType+SYDFDVLD.CFld_name = 'K'+SYDFIELD.CFld_name AND  UPPER(ALLTRIM(SYDFDVLD.cFld_Head))<>UPPER(ALLTRIM('Scale Sizes'));
      INTO ARRAY loFormSet.laFields

  ELSE
    SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
      SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
      SYDFIELD.nFld_Wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry,SYDFDVLD.mFull_exp ;
      FROM SYDFDVLD,SYDFIELD ;
      WHERE SYDFDVLD.cKeyType+SYDFDVLD.CFld_name = 'K'+SYDFIELD.CFld_name ;
      INTO ARRAY loFormSet.laFields
  ENDIF
   *E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]


  SET ENGINEBEHAVIOR 90
  DIMENSION loFormSet.laFields[ALEN(loFormSet.laFields,1)+1,7]

  = AINS(loFormSet.laFields,1)
  loFormSet.laFields[1] = "All"
  loFormSet.laFields[2] = loFormSet.lcDummyCom
  loFormSet.lnfields    = 1
ENDIF
*- End of lfCrtFiles

*!*************************************************************
*! Name      : lfvFiles
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the files POPUP .
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
FUNCTION lfvFiles
PARAMETERS loFormSet,loFld
IF loFld.ListIndex = 1
  RETURN
ENDIF

loFormSet.lnFiles = loFld.ListIndex
PRIVATE llKey
llKey = .F.
lnAlias = SELECT(0)
IF loFormSet.laComp[loFormSet.lnComp,2] = loFormSet.lcDummyCom
*-- 'You have to select a company.'
*--        <OK>
  =gfModalGen("INM00331B00000","DIALOG")
  RETURN
ENDIF
IF !EMPTY(loFormSet.laFiles[loFormSet.lnFiles,2]) .AND. loFormSet.laFiles[loFormSet.lnFiles,2] # loFormSet.lcDummyCom
*-- Open the chosen file
  STORE SPACE(0) TO loFormSet.lcOldfil, loFormSet.lcOldOrd
  =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+loFormSet.laFiles[loFormSet.lnFiles,2],'','SH')

*-- Fill the pop up of the fields from the system file SYDFDVLD
  SET ENGINEBEHAVIOR 70
  SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
    SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
    SYDFIELD.nFld_Wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry,SYDFDVLD.mFull_exp;
    FROM SYDFDVLD,SYDFIELD ;
    WHERE SYDFDVLD.cKeyType+SYDFDVLD.CFld_name = 'F'+SYDFIELD.CFld_name ;
    .AND. SYDFDVLD.cFile_nam = loFormSet.laFiles[loFormSet.lnFiles,2];
    INTO ARRAY loFormSet.laFields
  SET ENGINEBEHAVIOR 90

  DIMENSION loFormSet.laFields[ALEN(loFormSet.laFields,1)+1,7]

  = AINS(loFormSet.laFields,1)
  loFormSet.laFields[1] = "All"
  loFormSet.laFields[2] = loFormSet.lcDummyCom
  loFormSet.lnfields    = 1
*-- Build the temporary files based on the KEY_CHG file.
  =lfBldTmp(loFormSet)
*loFormSet.ChangeMode('V')
  loFormSet.AriaForm1.lnFiles.Enabled = .F.

*B610613,1 TMI 12/05/2013 09:33 [Start] fox does no recognize alias with spaces
*SELECT (loFormSet.laFiles[loFormSet.lnFiles,2])
  SELECT (ALLTRIM(loFormSet.laFiles[loFormSet.lnFiles,2]))
*B610613,1 TMI 12/05/2013 09:33 [End  ]
ELSE
  DIMENSION laFileds[1,5]
  STORE SPACE(0) TO loFormSet.laFields
  loFormSet.AriaForm1.pgfChange.Ariapage1.lnfields.Enabled = .F.
  loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Enabled = .F.
ENDIF
=lfbrwOpn(loFormSet)
SELECT (lnAlias)
*- End of lfvFiles

*!*************************************************************
*! Name      : lfvFields
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of Fields POPUP
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : lfbrwOpn(loFormSet)
*!               PROCEDURES : None
*!*************************************************************
FUNCTION lfvFields
PARAMETERS loFormSet,loFld

loFormSet.lnfields = loFld.ListIndex

IF loFormSet.laFields[loFormSet.lnfields,2]=loFormSet.lcDummyCom
  loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Enabled = .F.
ELSE
  DO CASE
  CASE loFormSet.laFields[loFormSet.lnfields,2] = 'CSTYMAJOR'
    loFormSet.laFields[loFormSet.lnfields,4] = loFormSet.lnMajSize
    loFormSet.laFields[loFormSet.lnfields,5] = loFormSet.lcMaj

  CASE loFormSet.laFields[loFormSet.lnfields,2] = 'STYLE'
    loFormSet.laFields[loFormSet.lnfields,4] = LEN(loFormSet.lcNMjrPt)
    loFormSet.laFields[loFormSet.lnfields,5] = loFormSet.lcNMjrPt

  CASE loFormSet.laFields[loFormSet.lnfields,2] = 'SCALE'
    loFormSet.laFields[loFormSet.lnfields,4] = loFormSet.lnLenght
    loFormSet.laFields[loFormSet.lnfields,5] = loFormSet.lcScalePic

*E303425,3 TMI 11/24/2013 17:59 [Start]
  CASE loFormSet.laFields[loFormSet.lnfields,2] = 'ITEM'
    loFormSet.laFields[loFormSet.lnfields,4] = loFormSet.lnFabMajSize
    loFormSet.laFields[loFormSet.lnfields,5] = loFormSet.lcFabMaj
*E303425,3 TMI 11/24/2013 17:59 [End  ]
  ENDCASE

  loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Enabled = .T.
ENDIF
=lfbrwOpn(loFormSet)

*!*************************************************************
*! Name      : lfvValues
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the Values check box to open the
*!             values screen
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : gfOpenTable(),gfwCodePop(),SMFLDVL.scx,
*!                            lfbrwOpn(loFormSet)
*!               PROCEDURES : None
*!*************************************************************
FUNCTION lfvValues
PARAMETERS loFormSet,loFld
lnAlias = SELECT(0)
PRIVATE llFile , lnCnt
SET STEP ON 
llFile = .F.
STORE SPACE(0) TO lcPct, lcCode
IF loFormSet.laFields[loFormSet.lnfields,2] = loFormSet.lcDummyCom .OR. loFormSet.laFields[loFormSet.lnfields,4] = 0
  loFld.Value = .F.
  RETURN
ENDIF
*-- Open the master file of the field to browse from it.
IF SEEK(loFormSet.lcKeyType+loFormSet.laFields[loFormSet.lnfields,2],'SYDFDVLD')
  llFile = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+ALLTRIM(SYDFDVLD.cMainFile) ,SYDFDVLD.cFile_Tag,'SH')
  IF !llFile .AND. !USED(SYDFDVLD.cMainFile)
    RETURN
  ENDIF
ELSE
  RETURN
ENDIF

lcCode   = loFormSet.laFields[loFormSet.lnfields,1]

IF UPPER(lcCode) = "STYLE NON MAJOR"
  IF TYPE('lnCnt') = "N"
    PRIVATE lnOldCount
    lnOldCount = lnCnt
  ENDIF
  =lpSelecSty(loFormSet)
  IF TYPE('lnCnt') = "N"
    lnCnt = lnOldCount
  ENDIF
ENDIF

STORE SPACE(loFormSet.laFields[loFormSet.lnfields,4]) TO loFormSet.lcOldVal,loFormSet.lcNewVal

IF !EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
  IF !USED('SYDFIELD')
    =gfOpenFile(oAriaApplication.SysPath+'SYDFIELD','SYDFIELD','SH')
  ENDIF
  loFormSet.lnkeys=OCCURS('+',SYDFDVLD.mFull_exp)+1
  DIMENSION loFormSet.laFullExp[loFormSet.lnkeys,4]
  SELECT SYDFDVLD
  lcKeyValues = SYDFDVLD.mFull_exp
  FOR lnCnt=1 TO loFormSet.lnkeys
    lcPlus=OCCURS('+',lcKeyValues)
    IF lcPlus > 0
      loFormSet.laFullExp[lnCnt,1] = SUBSTR(lcKeyValues,1,AT('+',lcKeyValues)-1)
      lcKeyValues = SUBSTR(lcKeyValues,LEN(loFormSet.laFullExp[lnCnt,1])+2)
    ELSE
      loFormSet.laFullExp[lnCnt,1] = lcKeyValues
    ENDIF

    SELECT SYDFIELD
    SET ORDER TO TAG CFld_name
    IF SEEK(loFormSet.laFullExp[lnCnt,1],'SYDFIELD')
      loFormSet.laFullExp[lnCnt,2] = cFld_Head
      loFormSet.laFullExp[lnCnt,3] = nFld_Wdth
      loFormSet.laFullExp[lnCnt,4] = cPict_str
    ENDIF
  ENDFOR
ENDIF

loFormSet.lcKeyTemp = ''
loFormSet.lcKeyVal  = ''
*-- Call the values screen
lcScx = lfGetScx('SM\SMFLDVL.scx')
DO FORM (lcScx ) WITH loFormSet

*-- Refresh the Open keys browse
=lfbrwOpn(loFormSet)

loFld.Value = .F.

IF llFile .AND. SEEK(loFormSet.lcKeyType+loFormSet.laFields[loFormSet.lnfields,2],'SYDFDVLD')
  gfCloseTable(ALLTRIM(SYDFDVLD.cMainFile))
ENDIF

SELECT (lnAlias)

************************************************************
*! Name      : lfSMFLDVLFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/29/2013
*! Purpose   : SMFLDVL Form Init method call
************************************************************
FUNCTION lfSMFLDVLFormInit
PARAMETERS loBranchFormSet
LOCAL loFormSet,lnSlct
loFormSet = loBranchFormSet.loFormSet
lnSlct = SELECT(0)

WITH loBranchFormSet.AriaForm1
  .lcCode.Caption = lcCode
  .pbFilter.Visible = loFormSet.lcKeyType = 'F'
  .lcOldVal.Visible = !loFormSet.laFields[loFormset.lnFields,6]
  .lcNewVal.Visible = !loFormSet.laFields[loFormset.lnFields,6]

  .lcOldVal.KeyTextbox.ControlSource = "Thisformset.loFormSet.lcOldVal"
  .lcNewVal.KeyTextbox.ControlSource = "Thisformset.loFormSet.lcNewVal"

  .lcOldVal.KeyTextbox.MaxLength = loFormSet.laFields[loFormSet.lnfields,4]
  .lcNewVal.KeyTextbox.MaxLength = loFormSet.laFields[loFormSet.lnfields,4]
  IF loFormSet.laFields[loFormSet.lnfields,2] = 'SCALE'
    LOCAL lnSclLen
    *B612706,1 MMT 02/28/2024 The key Change program should read scale width from setups in all cases not in case of extended size scale case only [T-ERP-20240209.0008][Start]
    *lnSclLen = IIF(!gfGetMemVar('M_USEEXSSC'),1,gfGetMemVar('M_EXTWIDTH'))
    lnSclLen = gfGetMemVar('M_EXTWIDTH')
    *B612706,1 MMT 02/28/2024 The key Change program should read scale width from setups in all cases not in case of extended size scale case only [T-ERP-20240209.0008][End]
    .lcOldVal.KeyTextbox.MaxLength = lnSclLen
    .lcNewVal.KeyTextbox.MaxLength = lnSclLen
  ENDIF

  .lnOldPop.Visible = loFormSet.laFields[loFormset.lnFields,6]
  .lnNewPop.Visible = loFormSet.laFields[loFormset.lnFields,6]
  IF loFormSet.laFields[loFormset.lnFields,6]

    .lnOldPop.CodesField = ALLTRIM(loFormSet.laFields[loFormset.lnFields,2])
    .lnOldPop.Init(.T.)
*.lnOldPop.ALLvalue = .lnOldPop.NOTapplicablevalue

    .lnNewPop.CodesField = ALLTRIM(loFormSet.laFields[loFormset.lnFields,2])
    .lnNewPop.Init(.T.)
*.lnNewPop.ALLvalue = .lnNewPop.NOTapplicablevalue

    .lnOldPop.ControlSource = "Thisformset.loFormSet.lcOldVal"
    .lnNewPop.ControlSource = "Thisformset.loFormSet.lcNewVal"

  ENDIF

ENDWITH

*- Define grid columns
loGrd = loBranchFormSet.AriaForm1.Ariagrid1
loGrd.ColumnCount = 3
loGrd.RecordSource = ''
loGrd.RecordSource = loFormSet.lcValues
lcValues = loFormSet.lcValues
lcHeader1 = IIF(loFormSet.lcKeyType='K',LANG_KEY,LANG_FIELD)

LOCAL lcFld_name
SELECT (lcValues)
lcCond = "cKey_Chg = '" + loFormSet.laFields[loFormSet.lnfields,2]+"'"

SET FILTER TO &lcCond
lcFld_name = ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2])
lcOldValSrc = IIF(loFormSet.laFields[loFormSet.lnfields,6],"gfCodDes(ALLTRIM(&lcVALUES..Old_Key),'&lcFld_name')","&lcVALUES..Old_Key")
lcNewValSrc = IIF(loFormSet.laFields[loFormSet.lnfields,6],"gfCodDes(ALLTRIM(&lcVALUES..New_Key),'&lcFld_name')","&lcVALUES..New_Key")
lfSetColumnsProp(loGrd,'1',lcOldValSrc ,LANG_OLD_VALUE ,160)
lfSetColumnsProp(loGrd,'2',lcNewValSrc ,LANG_NEW_VALUE ,160)
lfSetColumnsProp(loGrd,'3',"&lcVALUES..ENTERED" ,LANG_DATE ,100)

*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
IF  UPPER(ALLTRIM(lcCode))==UPPER(ALLTRIM('Scale Sizes'))
  loBranchFormSet.AriaForm1.sizeMap.Visible=.T.
  loBranchFormSet.AriaForm1.SElectStyles.Visible=.T.
  loBranchFormSet.AriaForm1.lcNewVal.KeyCmd.Visible = .T.
  IF  lfHasData()
  loBranchFormSet.AriaForm1.sizeMap.Enabled =.T.
  loBranchFormSet.AriaForm1.SElectStyles.Enabled =.T.
  ELSE
  loBranchFormSet.AriaForm1.sizeMap.Enabled =.F.
  loBranchFormSet.AriaForm1.SElectStyles.Enabled =.F.
  ENDIF
ELSE
  loBranchFormSet.AriaForm1.sizeMap.Visible=.F.
  loBranchFormSet.AriaForm1.SElectStyles.Visible=.F.
  loBranchFormSet.AriaForm1.lcNewVal.KeyCmd.Visible = .F.
ENDIF
*E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]

SELECT (loFormSet.lcValues)
LOCATE
loGrd.Readonly = .T.
loGrd.Refresh()



*E611964,1 Es 10/27/2019 Change size scales from one size scale to another [Start]
loGrd.AfterRowColChange()
*E611964,1 Es 10/27/2019 Change size scales from one size scale to another [End]


SELECT (lnSlct)
*- End of lfSMFLDVLFormInit.

************************************************************
*! Name      : lfSMFLDVL_AfterRowColChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/05/2013
*! Purpose   : AfterRowColChange method for SMFLDVL screen
************************************************************
FUNCTION lfSMFLDVL_AfterRowColChange
PARAMETERS loBranchFormSet
LOCAL loFormSet,lnSlct,lcValues
loFormSet = loBranchFormSet.loFormSet
lnSlct = SELECT(0)
lcValues = loFormSet.lcValues
SELECT (lcValues)
IF !EOF(lcValues)
  WITH loBranchFormSet.AriaForm1
    IF loFormSet.lcKeyType='K'
      .lcOldVal.KeyTextbox.Value = &lcValues..Old_Key
      .lcOldVal.Enabled = .F.
      .lcOldVal.KeyTextbox.Refresh()
      .lcNewVal.KeyTextbox.Value = &lcValues..New_Key
      .lcNewVal.Enabled = .F.
      .lcNewVal.KeyTextbox.Refresh()
    ELSE
      .lnOldPop.Value = ALLTRIM(&lcValues..Old_Key)
      .lnOldPop.Enabled = .F.
      .lnOldPop.Refresh()
      .lnNewPop.Value = ALLTRIM(&lcValues..New_Key)
      .lnNewPop.Enabled = .F.
      .lnNewPop.Refresh()
    ENDIF
    .pbRemove.Enabled = .T.
  ENDWITH
ENDIF
*E611964,1 Es 12/31/2019  Change size scales from one size scale to another [Start]
SET STEP ON 
IF !EMPTY(loFormSet.lcNewVal) AND !EMPTY(loFormSet.lcOldVal)
 loBranchFormSet.AriaForm1.sizeMap.Enabled =.T.
  loBranchFormSet.AriaForm1.SElectStyles.Enabled =.T.
ENDIF 
*E611964,1 Es 12/31/2019  Change size scales from one size scale to another [End]


SELECT (lnSlct)
*- End of lfSMFLDVL_AfterRowColChange.

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To enable or disable screen objects due to screen mode
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : gfOpenTable()
*!               PROCEDURES : None
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

=lfFormActivate(loFormSet)
DO CASE
*-- select Mode , redirect to the View mode
CASE loFormSet.ActiveMode = 'S'
  IF TYPE('loFormSet.lFirstRun') = 'L'
    loFormSet.ChangeMode('V')
  ENDIF
  loFormSet.RefreshAll()

*-- View Mode
CASE loFormSet.ActiveMode = 'V'

*-- Build the temporary files based on the key_chg file
  =lfBldTmp(loFormSet)
*-- Update the browse of open fields
  =lfbrwOpn(loFormSet)

  loFormSet.AriaForm1.lnFiles.Enabled = .F.
  loFormSet.AriaForm1.pgfChange.Ariapage1.lnfields.Enabled = .F.

  loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Enabled = .F.
  loFormSet.AriaForm1.pgfChange.Ariapage2.Enabled = .T.

    *E611964,1 Es 10/21/2019 Change size scales from one size scale to another [Start]
    * loFormset.AriaForm1.pgfChange.Ariapage1.pbPrcd.Enabled = .T.
  SELECT (loFormSet.lcValues)
  IF RECCOUNT(loFormSet.lcValues) > 0
    loFormSet.AriaForm1.pgfChange.Ariapage1.pbPrcd.Enabled = .T.
  ELSE
    loFormSet.AriaForm1.pgfChange.Ariapage1.pbPrcd.Enabled =.F.
  ENDIF
    *E611964,1 Es 10/21/2019 Change size scales from one size scale to another [End]
*-- Edit Mode
CASE loFormSet.ActiveMode = 'E'
  loFormSet.AriaForm1.lnFiles.Enabled = .T.
  loFormSet.AriaForm1.pgfChange.Ariapage1.lnfields.Enabled = .T.

  IF loFormSet.laFields[loFormSet.lnfields,2] = loFormSet.lcDummyCom
    loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Value = .F.
    loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Enabled = .F.
  ELSE
    loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Enabled = .T.
  ENDIF
  loFormSet.AriaForm1.pgfChange.Ariapage2.Enabled = .T.
  loFormSet.AriaForm1.pgfChange.Ariapage1.pbPrcd.Enabled = .F.
ENDCASE

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/28/2013
*! Purpose   : activate the form method
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

WITH loFormSet.oToolbar
  .cmdDelete.Enabled = .F.
  .cmdFind.Enabled = .F.
  .cmdSelect.Enabled = .F.
  .cmdEdit.Enabled = .ActiveMode = 'V'
  .cmdAdd.Enabled = .ActiveMode = 'E'
ENDWITH
*- End of lfFormActivate.

************************************************************
*! Name      : lfRefreshAll
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/28/2013
*! Purpose   : RefreshAll method
************************************************************
FUNCTION lfRefreshAll
PARAMETERS loFormSet
WITH loFormSet
  .oToolbar.cmdEdit.Enabled = .ActiveMode = 'V'
  .oToolbar.cmdAdd.Enabled = .ActiveMode = 'E'
  .oToolbar.cmdSelect.Enabled = .F.
ENDWITH
*- End of lfRefreshAll.

*!*************************************************************
*! Name      : lfvFilter
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To make a filter on the chosen table
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : gfDbfField(), =gfBFiltr()
*!               PROCEDURES :
*!*************************************************************
FUNCTION lfvFilter
PARAMETERS loBranchFormSet
LOCAL loFormSet

loFormSet = loBranchFormSet.loFormSet

lcBaseFile = ALLTRIM(loFormSet.laFiles[loFormSet.lnFiles,2])
lcScFields = 'KEY_CHG.cKEY_CHG'
SELECT (loFormSet.lcValues)
*B610613,1 TMI 12/05/2013 14:26 [Start] get filter
IF !EMPTY(mSaveFilt)

  RESTORE FROM memo mSaveFilt ADDITIVE
ENDIF
*B610613,1 TMI 12/05/2013 14:26 [End  ]

lcBrFields = gfDbfField(lcBaseFile)

DIMENSION laFileFields[1,4]
SET ENGINEBEHAVIOR 70
SELECT DIST SYDFIELD.cFld_Head, SYDFIELD.CFld_name, SYDFIELD.CDATA_TYP,'F' ;
  FROM SYDFLFLD, SYDFIELD;
  WHERE SYDFIELD.CFld_name = SYDFLFLD.CFld_name  .AND. ;
  SYDFLFLD.cFile_nam == PADR(lcBaseFile,oAriaApplication.FileW) .AND. ;
  ALLTRIM(SYDFIELD.CFld_name) $  lcBrFields ;
  ORDER BY SYDFLFLD.NFLD_POS;
  INTO ARRAY laFileFields
SET ENGINEBEHAVIOR 90

SELECT (lcBaseFile)
*B610613,1 TMI 12/05/2013 14:29 [Start] comment this
*IF loFormSet.llNew
*  STORE SPACE(0) TO loFormSet.laFixFltr
*ENDIF
*DIMENSION laFixFltr[ALEN(loFormSet.laFixFltr,1),ALEN(loFormSet.laFixFltr,2)]
*ACOPY(loFormSet.laFixFltr,laFixFltr)
IF TYPE('laFixFltr[1]')='U'
  DIMENSION laFixFltr[1,7]
  laFixFltr = ''
ENDIF
*B610613,1 TMI 12/05/2013 14:29 [End  ]

lcCurFilter = ''
*lcFltBefor  = SET('FILTER')
lcFltBefor  = ALLTRIM(gfGenFlt('laFixFltr', .T.))

*=gfBFiltr('laFixFltr',@laField_H,@laField_N,'F')

PRIVATE loFilter
lcType = ''
*lcType = 'BROWSE_NEW_FILTER'  &&, using this option shows the 'Save' button on the filter screen, but while saving it called a method in the browse screen
loFilter = CREATEOBJECT('AriaSearchForm', 'laFixFltr', .F., .F., .T., lcType)
loFilter.AriaSearchContainer1.InitializeFields(@laFileFields, @laFixFltr, ALEN(laFileFields,1), ALLTRIM(lcBaseFile))
loFilter.Show()

lcFltAftr = ALLTRIM(gfGenFlt('laFixFltr', .T.))

SELECT (loFormSet.lcValues)
IF lcFltBefor # lcFltAftr
  SAVE TO MEMO mSaveFilt;
    ALL LIKE laFixFltr
  REPLACE cStatus   WITH IIF(cStatus='S','M',cStatus)
  DIMENSION loFormSet.laFixFltr[ALEN(laFixFltr,1),ALEN(laFixFltr,2)]
  ACOPY(laFixFltr,loFormSet.laFixFltr)
ENDIF
*- End of lfvFilter

*!*************************************************************
*! Name      : lpClsScr
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To Get the data from the original files and reject
*!             the modifications of the user
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : lfBldTmp(loFormSet), lfbrwOpn(loFormSet)
*!               PROCEDURES :
*!*************************************************************
FUNCTION lpClsScr

IF loFormSet.ActiveMode = 'E'
*-- Build the temporary files again
  =lfBldTmp(loFormSet)
  IF loFormSet.lnActFolder = 1
*-- Refresh the browse
    =lfbrwOpn(loFormSet)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Saving the screens (local save)
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : None
*!               PROCEDURES : None
*!*************************************************************
FUNCTION lpSavScr
PARAMETERS loFormSet
*E303425,3 TMI 11/24/2013 19:12 [Start]
LOCAL lnSlct
lnSlct = SELECT(0)
*E303425,3 TMI 11/24/2013 19:12 [End  ]
SET STEP ON 
*B610613,1 TMI 12/05/2013 13:08 [Start] get cKEY_CHG field width in KEY_CHG table
LOCAL lnKeyChgW
lnKeyChgW = FSIZE('CKEY_CHG','KEY_CHG')
*B610613,1 TMI 12/05/2013 13:08 [End  ]

lcValuesTbl = loFormSet.lcValues

PRIVATE lcFldtmp, lcDel
lcFldtmp = ''
lcDel = SET('DELETE')
SET DELETED OFF
*B610613,1 TMI 12/05/2013 14:15 [Start]
SELECT KEY_CHG
SET FILTER TO KEYSTATUS<>'C'
LOCATE
*B610613,1 TMI 12/05/2013 14:15 [End  ]

SELECT (loFormSet.lcValues)
*E303425,3 TMI 11/24/2013 19:10 [Start]
LOCAL lcFltr
lcFltr = FILTER()
SET FILTER TO
LOCATE
*E303425,3 TMI 11/24/2013 19:10 [End  ]
SCAN FOR cStatus # 'S'
*-- If old value equals new value, loop and don't do any thing
  IF Old_Key == New_Key
    LOOP
  ENDIF
  DO CASE
  CASE cStatus = 'A'
*-- If added record
    INSERT INTO KEY_CHG (cKeyType,cKey_Chg,Old_Key,New_Key,KEYSTATUS,ENTERED) ;
      VALUES (&lcValuesTbl..cKeyType,&lcValuesTbl..cKey_Chg,&lcValuesTbl..Old_Key,;
      &lcValuesTbl..New_Key,'O',DATE())
*B610613,1 TMI 12/05/2013 14:03 [Start] Update the MSAVEFILT memo field for filters
    SELECT KEY_CHG
    REPLACE mSaveFilt WITH &lcValuesTbl..mSaveFilt

      *E611964,1 Es 10/31/2019 Change size scales from one size scale to another [Start]
    REPLACE msizes WITH &lcValuesTbl..msizes,;
            Note   WITH &lcValuesTbl..Note
        *E611964,1 Es 10/31/2019 Change size scales from one size scale to another [End]

*B610613,1 TMI 12/05/2013 14:05 [End  ]
  CASE cStatus = 'M'
*-- If modified record
    SET DELETED ON
*B610613,1 TMI 12/05/2013 13:09 [Start] use the lnKeyChgW value as the field width
*IF SEEK(&lcValuesTbl..cKeyType+PADR(EVAL(loFormSet.lcValues+'.cKey_Chg'),oAriaApplication.FieldW) + ALLTRIM(&lcValuesTbl..Old_Key),'KEY_CHG')
    IF SEEK(&lcValuesTbl..cKeyType+PADR(EVAL(loFormSet.lcValues+'.cKey_Chg'),lnKeyChgW) + ALLTRIM(&lcValuesTbl..Old_Key),'KEY_CHG')
*B610613,1 TMI 12/05/2013 13:09 [End  ]
      SELECT KEY_CHG
      REPLACE New_Key  WITH &lcValuesTbl..New_Key,;
        ENTERED  WITH &lcValuesTbl..ENTERED
*B610613,1 TMI 12/05/2013 14:03 [Start] Update the MSAVEFILT memo field for filters
      REPLACE mSaveFilt WITH &lcValuesTbl..mSaveFilt
*B610613,1 TMI 12/05/2013 14:03 [End  ]

*E611964,1 Es 10/31/2019 Change size scales from one size scale to another [Start]
      REPLACE msizes WITH &lcValuesTbl..msizes,;
              Note  WITH &lcValuesTbl..Note
*E611964,1 Es 10/31/2019 Change size scales from one size scale to another [End]
      ELSE
      LOOP
    ENDIF
    SET DELETED OFF
  CASE cStatus = 'D'
*-- If deleted record
    SET DELETED ON
*B610613,1 TMI 12/05/2013 13:10 [Start] use the lnKeyChgW value as the field width
*IF SEEK(&lcValuesTbl..cKeyType+PADR(EVAL(loFormSet.lcValues+'.cKey_Chg'),10) + ALLTRIM(&lcValuesTbl..Old_Key),'KEY_CHG')
    IF SEEK(&lcValuesTbl..cKeyType+PADR(EVAL(loFormSet.lcValues+'.cKey_Chg'),lnKeyChgW) + ALLTRIM(&lcValuesTbl..Old_Key),'KEY_CHG')
*B610613,1 TMI 12/05/2013 13:10 [End  ]
      SELECT KEY_CHG
      DELETE
    ENDIF
    SET DELETED OFF
  ENDCASE

ENDSCAN
SET DELETE &lcDel

*B610613,1 TMI 12/05/2013 14:17 [Start] releae filter
SELECT KEY_CHG
SET FILTER TO
LOCATE
*B610613,1 TMI 12/05/2013 14:17 [End  ]

*E303425,3 TMI 11/24/2013 19:11 [Start]
SELECT (loFormSet.lcValues)
SET FILTER TO
loFormSet.AriaForm1.pgfChange.Ariapage1.lnfields.ListIndex = 1
loFormSet.AriaForm1.Refresh()
SELECT (lnSlct)
*E303425,3 TMI 11/24/2013 19:11 [End  ]

*- End of lpSavScr

*!*************************************************************
*! Name      : lfvAdd
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the add button
*!*************************************************************
*! Called from : SMFLDVL.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : gfwCodePop()
*!               PROCEDURES : None
*!*************************************************************
FUNCTION lfvAdd
PARAMETERS loBranchFormSet
LOCAL loFormSet
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [Start]
DECLARE laScopExpr[1]
STORE "" TO laScopIMP,laScopExpr,laScopeSty,laScopeMajor,laScopeLoc,laScopeFabric
loBranchFormSet.AriaForm1.sizeMap.Enabled =.F.
loBranchFormSet.AriaForm1.SElectStyles.Enabled =.F.
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [End]

loFormSet = loBranchFormSet.loFormSet

IF EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
  STORE SPACE(loFormSet.laFields[loFormSet.lnfields,4]) TO loFormSet.lcOldVal, loFormSet.lcNewVal
  loBranchFormSet.AriaForm1.pbRemove.Enabled = .F.
  loFormSet.llNew    = .T.
  IF loFormSet.laFields[loFormSet.lnfields,6]
*E303425,3 TMI 11/24/2013 19:15 [Start]
*DIMENSION laCodInfo[ALEN(loFormSet.laCodInfo,1),ALEN(loFormSet.laCodInfo,2)]
*ACOPY(loFormSet.laCodInfo,laCodInfo)
*DIMENSION laCodInf[ALEN(loFormSet.laCodInf,1),ALEN(loFormSet.laCodInf,2)]
*ACOPY(loFormSet.laCodInf,laCodInf)
*E303425,3 TMI 11/24/2013 19:15 [End  ]
*=gfwCodePop(@laCodInfo, ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]) , "N",loFormSet.laComp[loFormSet.lnComp,2])
*=gfwCodePop(@laCodInf , ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]) , "N",loFormSet.laComp[loFormSet.lnComp,2])
*E303425,3 TMI 11/24/2013 19:15 [Start]
*DIMENSION loFormSet.laCodInfo[ALEN(laCodInfo,1),ALEN(laCodInfo,2)]
*ACOPY(laCodInfo,loFormSet.laCodInfo)
*DIMENSION loFormSet.laCodInf[ALEN(laCodInf,1),ALEN(laCodInf,2)]
*ACOPY(laCodInf,loFormSet.laCodInf)
*E303425,3 TMI 11/24/2013 19:15 [End  ]
    WITH loBranchFormSet.AriaForm1
      .lnOldPop.Enabled = .T.
      .lnNewPop.Enabled = .T.
      .lnOldPop.Setfocus()
    ENDWITH
  ELSE
    WITH loBranchFormSet.AriaForm1
      .lcOldVal.Enabled = .T.
      .lcNewVal.Enabled = .T.
      .lcOldVal.Setfocus()
    ENDWITH
  ENDIF
ELSE    && !EMPTY(SYDFDVLD.MFulL_EXP)
  STORE SPACE(loFormSet.laFields[loFormSet.lnfields,4]) TO loFormSet.lcOldVal, loFormSet.lcNewVal
  FOR lnCnt=1 TO loFormSet.lnkeys
    lcKeyPic = loFormSet.laFullExp[loFormSet.lnkeys,4]
    lcKeyTitl= loFormSet.laFullExp[lnCnt,2]
    loFormSet.lcKeyVal=''
    lcScx = lfGetScx('SM\smkeynam.scx')
    DO FORM (lcScx ) WITH loFormSet
    IF EMPTY(loFormSet.lcKeyTemp)
      EXIT
    ENDIF
  ENDFOR
  IF !EMPTY(loFormSet.lcKeyTemp)
    STORE SPACE(loFormSet.laFields[loFormSet.lnfields,4]) TO loFormSet.lcOldVal, loFormSet.lcNewVal
    loBranchFormSet.AriaForm1.pbRemove.Enabled = .F.
    loFormSet.llNew   = .T.
    IF loFormSet.laFields[loFormSet.lnfields,6]
*E303425,3 TMI 11/24/2013 19:15 [Start]
*DIMENSION laCodInfo[ALEN(loFormSet.laCodInfo,1),ALEN(loFormSet.laCodInfo,2)]
*ACOPY(loFormSet.laCodInfo,laCodInfo)
*DIMENSION laCodInf[ALEN(loFormSet.laCodInf,1),ALEN(loFormSet.laCodInf,2)]
*ACOPY(loFormSet.laCodInf,laCodInf)
*E303425,3 TMI 11/24/2013 19:15 [End  ]
*= gfwCodePop(@laCodInfo, ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]) , "N",loFormSet.laComp[loFormSet.lnComp,2])
*= gfwCodePop(@laCodInf , ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]) , "N",loFormSet.laComp[loFormSet.lnComp,2])
*E303425,3 TMI 11/24/2013 19:15 [Start]
*DIMENSION loFormSet.laCodInfo[ALEN(laCodInfo,1),ALEN(laCodInfo,2)]
*ACOPY(laCodInfo,loFormSet.laCodInfo)
*DIMENSION loFormSet.laCodInf[ALEN(laCodInf,1),ALEN(laCodInf,2)]
*ACOPY(laCodInf,loFormSet.laCodInf)
*E303425,3 TMI 11/24/2013 19:15 [End  ]
      loBranchFormSet.AriaForm1.lnOldPop.Enabled = .T.
      loBranchFormSet.AriaForm1.lnNewPop.Enabled = .T.
      loBranchFormSet.AriaForm1.lnOldPop.Setfocus()
    ELSE
      loBranchFormSet.AriaForm1.lcOldVal.Enabled = .T.
      loBranchFormSet.AriaForm1.lcNewVal.Enabled = .T.
      loBranchFormSet.AriaForm1.lcOldVal.Setfocus()
    ENDIF
  ENDIF
  IF EMPTY(loFormSet.lcKeyVal)
    IF !EOF(loFormSet.lcValues)
      =lfwBrWhen()
    ENDIF
  ENDIF
ENDIF

WITH loBranchFormSet.AriaForm1
  .lcOldVal.KeyTextbox.Value = ''
  .lcNewVal.KeyTextbox.Value = ''
ENDWITH

RETURN
*- End of lfvAdd.

*!*************************************************************
*! Name      : lfvRemove
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the remove button
*!*************************************************************
*! Called from : SMFLDVL.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : lfBrows()
*!               PROCEDURES : None
*!*************************************************************
FUNCTION lfvRemove
PARAMETERS loBranchFormSet
LOCAL loFormSet
loFormSet = loBranchFormSet.loFormSet

*-- Message : Are you sure you want to delete this record ?
*-- Button     <Remove>    <Cancel>
IF gfModalGen("QRM00002B00007","DIALOG","remove") = 1
  SELECT (loFormSet.lcValues)
  REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
  DELETE
  WITH loBranchFormSet.AriaForm1
    .lcOldVal.KeyTextbox.Value = ''
    .lcOldVal.KeyTextbox.OldValue = ''
    .lcNewVal.KeyTextbox.Value = ''
    .lcNewVal.KeyTextbox.OldValue = ''
    .pbRemove.Enabled = .F.
    .Ariagrid1.Refresh()
  ENDWITH
ENDIF
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [Start]
SELECT (loFormSet.lcValues)
IF  !EOF(loFormSet.lcValues) OR !BOF(loFormSet.lcValues)
  IF !lfHasData()
  loBranchFormSet.AriaForm1.sizeMap.Enabled =.F.
  loBranchFormSet.AriaForm1.SElectStyles.Enabled =.F.
  ELSE
  SELECT (loFormSet.lcValues)
  LOCATE
  loBranchFormSet.AriaForm1.Ariagrid1.Refresh()
  loBranchFormSet.AriaForm1.Ariagrid1.AfterRowColChange()
  ENDIF
ENDIF
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [End]



*!*************************************************************
*! Name      : lfvNewVal
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To get some codes descriptions
*!*************************************************************
*! Called from : SMFLDVL.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : lfVldBrw(),
*!                          &lcSpecFn Special function of the field if any.
*!               PROCEDURES :
*!*************************************************************
FUNCTION lfvNewVal
PARAMETERS loBranchFormSet,loFld
LOCAL loFormSet,lcF

loFormSet = loBranchFormSet.loFormSet
IF TYPE('loFld.KeyTextbox')='O'
  loFormSet.lcNewVal = loFld.KeyTextbox.Value
  loFormSet.llBrowse = loFld.selectedfrombrowse
  loFormSet.lcOld = loFld.KeyTextbox.OldValue
ELSE
  loFormSet.lcNewVal = loFld.Value
  loFormSet.llBrowse = .F.
  loFormSet.lcOld = loFld.OldValue
ENDIF

PRIVATE  llRet, lnAlias

llRet = .T.
lnAlias = SELECT(0)
SELECT (loFormSet.lcValues)
IF loFormSet.laFields[loFormSet.lnfields,6]
  loFormSet.lcNewVal = loFld.Value
ENDIF
llChgValue = (UPPER(loFormSet.laFields[loFormSet.lnfields,1]) ="SALES REPRESENTATIVE")
IF EMPTY(loFormSet.lcNewVal) .AND. !loFormSet.llBrowse
  IF llChgValue
  ELSE
    RETURN
  ENDIF

ELSE
  IF loFormSet.lcNewVal == loFormSet.lcOldVal .AND. !loFormSet.llBrowse
*-- "Old and new value Cannot be the same. Cannot proceed. !!!"
*--                       <OK>
    =gfModalGen("INM00332B00000","DIALOG")
    loFormSet.lcNewVal = loFormSet.lcOld
  ELSE

    lnRecNo = RECNO(loFormSet.lcValues)
    lcExpr = cKeyType+cKey_Chg+Old_Key
*B610613,3 TMI 12/15/2013 10:38 [Start]
*IF SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+loFormSet.lcNewVal,loFormSet.lcValues) AND lnRecNo <> RECNO(loFormSet.lcValues)
    IF SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+PADR(loFormSet.lcNewVal,30),loFormSet.lcValues)
*B610613,3 TMI 12/15/2013 10:38 [End  ]
*-- "This value has been selected before as "old value". Cannot accept."
      =gfModalGen("INM00335B00000","DIALOG","old value")
      llRet = .F.
    ENDIF
    =SEEK(lcExpr,loFormSet.lcValues)
    IF llRet
      IF EMPTY(loFormSet.laFields[loFormSet.lnfields,3]) OR ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]) == 'ITEM'
*-- If there isn't special function , go to the browse function of all.
*-- Browse field by using SYDFDCLD file.
        IF llChgValue
          loFormSet.lcNewVal =lfVldBrw(loBranchFormSet,loFormSet.lcNewVal)
        ELSE
          loFormSet.lcNewVal =lfGetVal(loBranchFormSet,loFormSet.lcNewVal)
        ENDIF

      ELSE
*-- Get the special function of the field and call it with the
*-- parameter "N"
        lcSpecFn = STRTRAN(loFormSet.laFields[loFormSet.lnfields,3] , '()','("N")'  )
        llRet    = &lcSpecFn
      ENDIF
      IF EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
        llRet = IIF(EMPTY(loFormSet.lcNewVal) OR (ALLTRIM(loFormSet.lcNewVal) = "?" AND loFormSet.llSclExist) OR !llRet,.F.,.T.)

        IF !EMPTY(loFormSet.lcNewVal ) .AND. loFormSet.lcOldVal = loFormSet.lcNewVal
*-- "Old and new value Cannot be the same. Cannot proceed. !!!"
*--                  < OK >
          =gfModalGen("INM00332B00000","DIALOG")
          loFormSet.lcNewVal = loFormSet.lcOld
          llRet = .F.
        ENDIF
        SELECT(loFormSet.lcValues)
        lcExpr = cKeyType+cKey_Chg+Old_Key
        IF !EMPTY(loFormSet.lcNewVal) .AND. SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+loFormSet.lcNewVal,loFormSet.lcValues)
*-- "This value has been selected before as "old value". Cannot accept."
          =gfModalGen("INM00335B00000","DIALOG","old value")
          loFormSet.lcNewVal = loFormSet.lcOld
          llRet = .F.
        ENDIF
        =SEEK(lcExpr,loFormSet.lcValues)
      ENDIF
    ENDIF
*-- If the new valid entered is not valid.!!
    IF !llRet
    ELSE
*-- Update the temporary cursor
      SELECT (loFormSet.lcValues)
      IF !EMPTY(loFormSet.lcOldVal) .AND. EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
        IF loFormSet.llNew
          APPEND BLANK
          REPLACE cStatus   WITH 'A',;
            cFld_Head WITH  loFormSet.laFields[loFormSet.lnfields,1]
          loFormSet.llNew = .F.
        ELSE
          REPLACE cStatus WITH IIF(cStatus='S','M',cStatus)
        ENDIF
        REPLACE cKeyType   WITH loFormSet.lcKeyType,;
          Old_Key    WITH loFormSet.lcOldVal,;
          New_Key    WITH loFormSet.lcNewVal,;
          cKey_Chg   WITH loFormSet.laFields[loFormSet.lnfields,2],;
          ENTERED    WITH DATE()

*B610654,1 TMI 01/08/2014 13:17 [Start] update the oldkey value in the table according to the MaxLength
        IF loFormSet.laFields[loFormSet.lnfields,2] = 'SCALE'
          lnSclLen = loBranchFormSet.AriaForm1.lcOldVal.KeyTextbox.MaxLength
          replace Old_Key    WITH LEFT(Old_Key,lnSclLen)
        ENDIF
*B610654,1 TMI 01/08/2014 13:17 [End  ]

*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [Start]

*!*          DIMENSION laFixFltr[ALEN(loFormSet.laFixFltr,1),ALEN(loFormSet.laFixFltr,2)]
*!*          ACOPY(loFormSet.laFixFltr,laFixFltr)

*!*          SAVE TO MEMO mSaveFilt ALL LIKE laFixFltr
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [End]


*-- Variable to indicate that data has modified
        llcUpDate = .T.
      ENDIF
    ENDIF
  ENDIF
ENDIF

*- empty fields when a line is added
WITH loBranchFormSet.AriaForm1
  .lcOldVal.KeyTextbox.Value = ''
  .lcNewVal.KeyTextbox.Value = ''
ENDWITH

loFormSet.llBrowse = .F.
IF TYPE('loFld.selectedfrombrowse')<>'U'
  loFld.selectedfrombrowse = .F.
  loFld.KeyTextbox.Refresh()
ENDIF

loBranchFormSet.AriaForm1.Ariagrid1.Refresh()
SELECT (lnAlias)
loBranchFormSet.AriaForm1.pbAdd.Enabled = .T.
IF !llRet
  IF TYPE('loFld.Keytextbox') = 'O'
    loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
  ELSE
    loFld.Value = loFld.OldValue
  ENDIF
ENDIF
*B610613,3 TMI 12/15/2013 10:53 [Start] refresh
IF llRet
  =lfSMFLDVL_AfterRowColChange(loBranchFormSet)
  *E611964,1 Es 11/05/2019  Change size scales from one size scale to another [Start]
  SELECT (loFormSet.lcValues)
  IF ( !EOF(loFormSet.lcValues) OR !BOF(loFormSet.lcValues) )
    loBranchFormSet.AriaForm1.sizeMap.Enabled =.T.
    loBranchFormSet.AriaForm1.SElectStyles.Enabled =.T.
  ENDIF
  *E611964,1 Es 11/05/2019  Change size scales from one size scale to another [End]
ENDIF
*B610613,3 TMI 12/15/2013 10:53 [End  ]
RETURN llRet
*- End of lfvNewVal

*!*************************************************************
*! Name      : lfvOldVal
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the old value.
*!*************************************************************
*! Called from : SMFLDVL.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : lfVldBrw(),
*!                          &lcSpecFn Special function of the field if any.
*!               PROCEDURES :
*!*************************************************************
FUNCTION lfvOldVal
PARAMETERS loBranchFormSet,loFld
LOCAL loFormSet
loFormSet = loBranchFormSet.loFormSet
IF TYPE('loFld.KeyTextbox')='O'
  loFormSet.lcOldVal = loFld.KeyTextbox.Value
  loFormSet.llBrowse = loFld.selectedfrombrowse
  loFormSet.lcOld = loFld.KeyTextbox.OldValue
ELSE
  loFormSet.lcOldVal = loFld.Value
  loFormSet.llBrowse = .F.
  loFormSet.lcOld = loFld.OldValue
ENDIF

PRIVATE lnAlias,llRet
lnAlias = SELECT(0)

SELECT(loFormSet.lcValues)
llRet    = .T.
IF loFormSet.laFields[loFormSet.lnfields,6]
  loFormSet.lcOldVal = loFld.Value
ENDIF

IF EMPTY(loFormSet.lcOldVal) .AND. !loFormSet.llBrowse
ELSE

  IF loFormSet.lcOldVal == loFormSet.lcNewVal .AND. !loFormSet.llBrowse
*-- "Old and new value Cannot be the same. Cannot proceed. !!!"
*--                  < OK >
    =gfModalGen("INM00332B00000","DIALOG")
    loFormSet.lcOldVal = loFormSet.lcOld
  ELSE
    lnRecNo = RECNO(loFormSet.lcValues)
*B610613,3 TMI 12/15/2013 10:39 [Start]
*IF SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+loFormSet.lcOldVal,loFormSet.lcValues) AND lnRecNo <> RECNO(loFormSet.lcValues)
    IF SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+loFormSet.lcOldVal,loFormSet.lcValues)
*B610613,3 TMI 12/15/2013 10:39 [End  ]
*-- "This value has been selected before as an "old value". Cannot accept."
      =gfModalGen("INM00335B00000","DIALOG","old value")
      llRet = .F.
    ELSE
      SELECT (loFormSet.lcValues)
      LOCATE FOR cKeyType+cKey_Chg+Old_Key+New_Key = ;
        loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+Old_Key+loFormSet.lcOldVal
*IF FOUND() AND lnRecNo <> RECNO(loFormSet.lcValues)
      IF FOUND()
*-- "This value has been selected before as a "new value". Cannot accept."
        =gfModalGen("INM00335B00000","DIALOG","new value")
        llRet = .F.
      ENDIF
    ENDIF
    IF BETWEEN(lnRecNo,1,RECCOUNT(loFormSet.lcValues))
      GO lnRecNo IN (loFormSet.lcValues)
    ENDIF
    IF llRet
      IF EMPTY(loFormSet.laFields[loFormSet.lnfields,3]) AND !(ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]) == 'SHIPVIA')

*-- If there isn't special function, Call the browse of all.
        loFormSet.lcOldVal = lfVldBrw(loBranchFormSet,loFormSet.lcOldVal)
      ELSE

        IF (ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]) == 'SHIPVIA')
          loFormSet.lcOldVal = loFld.Value
        ELSE

*-- Call the special function with parameter "O"
          lcSpecFn = STRTRAN(loFormSet.laFields[loFormSet.lnfields,3] , '()','("O")'  )
          llRet    = &lcSpecFn

        ENDIF

      ENDIF
      IF EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
        IF !EMPTY(loFormSet.lcOldVal) .AND. loFormSet.lcNewVal == loFormSet.lcOldVal
*-- "Old and new value Cannot be the same. Cannot proceed. !!!"
*--                  < OK >
          =gfModalGen("INM00332B00000","DIALOG")
          loFormSet.lcOldVal = loFormSet.lcOld
          llRet = .F.
        ENDIF
*B610613,3 TMI 12/15/2013 10:40 [Start]
*IF SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+loFormSet.lcOldVal,loFormSet.lcValues) AND lnRecNo <> RECNO(loFormSet.lcValues)
        IF SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+loFormSet.lcOldVal,loFormSet.lcValues)
*B610613,3 TMI 12/15/2013 10:40 [End  ]
*-- "This value has been selected before as an "old value". Cannot accept."
          =gfModalGen("INM00335B00000","DIALOG","old value")
          llRet = .F.
        ELSE
          SELECT (loFormSet.lcValues)
          LOCATE FOR cKeyType+cKey_Chg+Old_Key+New_Key = ;
            loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+Old_Key+loFormSet.lcOldVal
*IF FOUND() AND lnRecNo <> RECNO(loFormSet.lcValues)
          IF FOUND()
*-- "This value has been selected before as a "new value". Cannot accept."
            =gfModalGen("INM00335B00000","DIALOG","new value")
            llRet = .F.
          ENDIF
        ENDIF
        IF BETWEEN(lnRecNo,1,RECCOUNT(loFormSet.lcValues))
          GO lnRecNo IN (loFormSet.lcValues)
        ENDIF
      ENDIF
    ENDIF

    IF !llRet
    ELSE
      IF EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
        IF !loFormSet.llNew
          SELECT (loFormSet.lcValues)
          REPLACE Old_Key   WITH loFormSet.lcOldVal,;
            cStatus   WITH IIF(cStatus='S','M',cStatus),;
            ENTERED   WITH IIF(cStatus='S',ENTERED,DATE())

          DIMENSION laFixFltr[ALEN(loFormSet.laFixFltr,1),ALEN(loFormSet.laFixFltr,2)]
          ACOPY(loFormSet.laFixFltr,laFixFltr)

          SAVE TO MEMO mSaveFilt ALL LIKE laFixFltr
        ENDIF
      ENDIF
*-- Variable to indicate that data has modified
      llcUpDate = .T.

    ENDIF
  ENDIF
ENDIF

loFormSet.llBrowse = .F.
IF TYPE('loFld.selectedfrombrowse')<>'U'
  loFld.selectedfrombrowse = .F.
  loFld.KeyTextbox.Refresh()
ENDIF
SELECT (lnAlias)
IF !llRet
  IF TYPE('loFld.Keytextbox') = 'O'
    loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
  ELSE
    loFld.Value = loFld.OldValue
  ENDIF
ENDIF
RETURN llRet
*- End of lfvOldVal.

*!***********************************************************************
*! Function   : lfwBrWhen
*! Developer  : AHMED MOHAMMED IBRAHIM
*! Date       : 10/13/1998
*! Purpose    : When function of the browse to refresh the edit region when
*!              the record pointer changes.
*!***********************************************************************
*! Calls      : FUNCTIONS  : gfwCodePop()
*!              PROCEDURES : None
*!***********************************************************************
FUNCTION lfwBrWhen
LOCAL lnSlct
lnSlct = SELECT(0)

lcValuesTbl = loFormSet.lcValues

loFormSet.lnOldRecNo = RECNO()
IF !EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
  loFormSet.lcNewVal   = PADR(SUBSTR(&lcValuesTbl..New_Key , LEN(ALLTRIM(SUBSTR(&lcValuesTbl..New_Key,1,AT('-',&lcValuesTbl..New_Key)-1))) + 2),8)
  loFormSet.lcOldVal   = PADR(SUBSTR(&lcValuesTbl..Old_Key , LEN(ALLTRIM(SUBSTR(&lcValuesTbl..Old_Key,1,AT('-',&lcValuesTbl..Old_Key)-1))) + 2),8)
  loFormSet.lcKeyTemp  = ALLTRIM(SUBSTR(&lcValuesTbl..Old_Key,1,AT('-',&lcValuesTbl..Old_Key)-1))
ELSE
  loFormSet.lcNewVal   = ALLTRIM(&lcValuesTbl..New_Key)
  loFormSet.lcOldVal   = ALLTRIM(&lcValuesTbl..Old_Key)
ENDIF
SET STEP ON 
SELECT (loFormSet.lcValues)
IF EOF(loFormSet.lcValues) .OR.  EMPTY(mSaveFilt)
  STORE SPACE(0) TO loFormSet.laFixFltr
  DIMENSION laFixFltr[ALEN(loFormSet.laFixFltr,1),ALEN(loFormSet.laFixFltr,2)]
  ACOPY(loFormSet.laFixFltr,laFixFltr)
  
  *B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
  STORE "" TO  laScopIMP ,laScopExpr,laScopeSty,laScopeMajor,laScopeLoc,laScopeFabric
  *B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]

ELSE
  RESTORE FROM MEMO mSaveFilt ADDITIVE
ENDIF
IF loFormSet.laFields[loFormSet.lnfields,6]
*E303425,3 TMI 11/24/2013 19:15 [Start]
*DIMENSION laCodInfo[ALEN(loFormSet.laCodInfo,1),ALEN(loFormSet.laCodInfo,2)]
*ACOPY(loFormSet.laCodInfo,laCodInfo)
*DIMENSION laCodInf[ALEN(loFormSet.laCodInf,1),ALEN(loFormSet.laCodInf,2)]
*ACOPY(loFormSet.laCodInf,laCodInf)
*E303425,3 TMI 11/24/2013 19:15 [End  ]
  lnOldPop = loFormSet.lcOldVal
*= gfwCodePop(@laCodInfo, ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]), "V,"+loFormSet.lcOldVal,loFormSet.laComp[loFormSet.lnComp,2])
  lnNewPop = loFormSet.lcNewVal
*= gfwCodePop(@laCodInf , ALLTRIM(loFormSet.laFields[loFormSet.lnfields,2]), "V,"+loFormSet.lcNewVal, loFormSet.laComp[loFormSet.lnComp,2])
*E303425,3 TMI 11/24/2013 19:16 [Start]
*DIMENSION loFormSet.laCodInfo[ALEN(laCodInfo,1),ALEN(laCodInfo,2)]
*ACOPY(laCodInfo,loFormSet.laCodInfo)
*DIMENSION loFormSet.laCodInf[ALEN(laCodInf,1),ALEN(laCodInf,2)]
*ACOPY(laCodInf,loFormSet.laCodInf)
*E303425,3 TMI 11/24/2013 19:16 [End  ]
ENDIF

*-- Variable to enable or disable the remove button.
llNoItem = EOF(loFormSet.lcValues)
IF llNoItem
  loBranchFormSet.AriaForm1.pbRemove.Enabled = .F.
  IF loFormSet.laFields[loFormSet.lnfields,6]
    loBranchFormSet.AriaForm1.lnNewPop.Enabled = .F.
    loBranchFormSet.AriaForm1.lnOldPop.Enabled = .F.
  ELSE
    loBranchFormSet.AriaForm1.lcNewVal.Enabled = .F.
    loBranchFormSet.AriaForm1.lcOldVal.Enabled = .F.
  ENDIF
ELSE
  loBranchFormSet.AriaForm1.pbRemove.Enabled = .T.
  IF loFormSet.laFields[loFormSet.lnfields,6]
    loBranchFormSet.AriaForm1.lnNewPop.Enabled = .T.
    IF cStatus = 'A'
      loBranchFormSet.AriaForm1.lnOldPop.Enabled = .T.
    ELSE
      loBranchFormSet.AriaForm1.lnOldPop.Enabled = .F.
    ENDIF
  ELSE
    loBranchFormSet.AriaForm1.lcNewVal.Enabled = .T.
    IF cStatus = 'A'
      loBranchFormSet.AriaForm1.lcOldVal.Enabled = .T.
    ELSE
      loBranchFormSet.AriaForm1.lcOldVal.Enabled = .F.
    ENDIF
  ENDIF
ENDIF
loFormSet.llNew = .F.
loBranchFormSet.AriaForm1.pbAdd.Enabled = .T.
SELECT (lnSlct)
*- End of lfwBrWhen.

*!*************************************************************
*! Name      : lfvPrcd
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the proceed button
*!*************************************************************
*! Called from : SMFLDCH.scx
*!*************************************************************
*! Calls       : FUNCTIONS  : gfModalGen(), lfOpClFile(), lfBldTmp(loFormSet),
*!                            lfbrwOpn(loFormSet),
*!                          &lcSpecFn Special function of the field processed
*!               PROCEDURES : None
*!*************************************************************
FUNCTION lfvPrcd
PARAMETERS loFormSet

*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
SELECT KEY_CHG
=SEEK(loFormSet.lcKeyType+"CSTYMAJOR")
LOCATE REST WHILE cKeyType+cKey_Chg+Old_Key= loFormSet.lcKeyType+"CSTYMAJOR" FOR KEYSTATUS = 'O' .AND. !EMPTY(Old_Key) .AND. !EMPTY(New_Key)
IF FOUND()
  CREATE CURSOR 'StyleList' (OldStyle C(19),NewStyle C(19),Status C(1))
  SELECT 'StyleList'
  INDEX On OldStyle+NewStyle TAG 'StyleList'
  CREATE CURSOR 'IssueList' (OldStyle C(19),NewStyle C(19),FieldDesc C(30),OldValue C(30),NewValue C(30))
  SELECT 'IssueList'
  INDEX On OldStyle+NewStyle  TAG 'StyleList'
  IF !USED('STYLEAG')
    =gfOpenTable('STYLE','STYLE','SH','STYLEAG')
  ENDIF
  IF !USED('SCALEAG')
    =gfOpenTable('SCALE','SCALE','SH','SCALEAG')
  ENDIF
  IF !USED('BOMHEADR_AG')
    =gfOpenTable('BOMHEADR','BOMHEADR','SH','BOMHEADR_AG')
  ENDIF
  IF !USED('BOM_AG')
    =gfOpenTable('BOM','MULTIBOM','SH','BOM_AG')
  ENDIF
  IF !USED('BOM_AG2')
    =gfOpenTable('BOM','MULTIBOM','SH','BOM_AG2')
  ENDIF


  SELECT KEY_CHG
  =SEEK( loFormSet.lcKeyType+"CSTYMAJOR")
  SCAN WHILE cKeyType+cKey_Chg+Old_Key= loFormSet.lcKeyType+"CSTYMAJOR" FOR KEYSTATUS = 'O' .AND. !EMPTY(Old_Key) .AND. !EMPTY(New_Key)

    IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
      INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
    ENDIF

    IF !gfSEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize),'STYLEAG')
      LOOP
    ENDIF
    IF !gfSEEK(PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize),'STYLEAG')
      LOOP
    ENDIF

    =gfSEEK('S'+STYLEAG.Scale,'SCALEAG')
    lnNewScalCnt = SCALEAG.CNT
    lcNewDivison = STYLEAG.CDivision
    lcNewScale = STYLEAG.Scale
    lcNewSeason = STYLEAG.Season
    lcNewcstygroup = STYLEAG.cstygroup
    lcNewcpurcode= STYLEAG.cpurcode
    lcNewcdisccode = STYLEAG.cdisccode
    lcNewcRotalty = STYLEAG.royalty
    lcNewcFabric = STYLEAG.cprifabric
    lnNewPriceA = STYLEAG.PriceA
    lnNewPriceB = STYLEAG.PriceB
    lnNewPriceC = STYLEAG.PriceC
    lnNewnsugretpri = STYLEAG.nsugretpri
    llNewInvSty = STYLEAG.linvsty

    =gfSEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize),'STYLEAG')
    =gfSEEK('S'+STYLEAG.Scale,'SCALEAG')
    IF lcNewScale <> STYLEAG.Scale
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_SCALE_CONFLICT, STYLEAG.Scale,lcNewScale)
      IF lnNewScalCnt <> SCALEAG.CNT
        IF SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
          REPLACE Status WITH 'N' IN 'StyleList'
        ENDIF
        INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
          (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_SCALE_BUCKET, STR(SCALEAG.CNT,1),STR(lnNewScalCnt,1))
      ENDIF
    ENDIF
    IF lcNewDivison <> STYLEAG.CDivision
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_DIVISION,gfCodDes(STYLEAG.CDivision,'CDIVISION'),gfCodDes(lcNewDivison ,'CDIVISION'))
    ENDIF

    IF lcNewSeason <> STYLEAG.Season
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_SEASON,gfCodDes(STYLEAG.Season,'SEASON'),gfCodDes(lcNewSeason ,'SEASON'))

    ENDIF

    IF lcNewcstygroup <> STYLEAG.cstygroup
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_STYGROUP,gfCodDes(STYLEAG.cstygroup,'CSTYGROUP'),gfCodDes(lcNewcstygroup ,'CSTYGROUP'))

    ENDIF

    IF lcNewcpurcode <> STYLEAG.cpurcode
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_PURGROUP,gfCodDes(STYLEAG.cpurcode,'CPURCODE'),gfCodDes(lcNewcpurcode ,'CPURCODE'))

    ENDIF

    IF lcNewcdisccode <> STYLEAG.cdisccode
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_DISCCODE,gfCodDes(STYLEAG.cdisccode,'CDISCCODE'),gfCodDes(lcNewcdisccode ,'CDISCCODE'))

    ENDIF

    IF lcNewcRotalty <> STYLEAG.royalty
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_ROYAL,gfCodDes(STYLEAG.royalty ,'ROYALTY'),gfCodDes(lcNewcRotalty ,'ROYALTY'))

    ENDIF

    IF lcNewcFabric <> STYLEAG.cprifabric
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_PRIMEFABRIC,STYLEAG.cprifabric,lcNewcFabric)
    ENDIF

    IF lnNewPriceA <> STYLEAG.PriceA
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_PRICEA,STR(STYLEAG.PriceA,12,2),STR(lnNewPriceA ,12,2))

    ENDIF

    IF lnNewPriceB <> STYLEAG.PriceB
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_PRICEB,STR(STYLEAG.PriceB,12,2),STR(lnNewPriceB ,12,2))

    ENDIF

    IF lnNewPriceC <> STYLEAG.PriceC
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_PRICEC,STR(STYLEAG.PriceC,12,2),STR(lnNewPriceC ,12,2))

    ENDIF

    IF lnNewnsugretpri <> STYLEAG.nsugretpri
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_PRICERETAIL,STR(STYLEAG.nsugretpri,12,2),STR(lnNewnsugretpri,12,2))

    ENDIF

    IF llNewInvSty <> STYLEAG.linvsty
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_INVSTY,IIF(STYLEAG.linvsty,'Yes',"No"),IIF(llNewInvSty ,'Yes',"No"))

    ENDIF


    lcNewCostSheetID = ""
    lcOldCostSheetID = ""
    lcOldType = ''
    lcNewType = ''
    IF gfSEEK('0001'+PADR(ALLTRIM(KEY_CHG.New_Key),19),'BOMHEADR_AG','BOMHEADR')
      SELECT BOMHEADR_AG
      LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(ALLTRIM(KEY_CHG.New_Key),19) FOR LDEFCSTSHT
      IF !FOUND()
        lcNewCostSheetID = ""
        lcNewType = ''
      ELSE
        lcNewCostSheetID = BOMHEADR_AG.CCSTSHT_ID
        lcNewType =  BOMHEADR_AG.CCSTSHTTYP
      ENDIF
    ENDIF
    IF gfSEEK('0001'+PADR(ALLTRIM(KEY_CHG.Old_Key),19),'BOMHEADR_AG','BOMHEADR')
      SELECT BOMHEADR_AG
      LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(ALLTRIM(KEY_CHG.Old_Key),19) FOR LDEFCSTSHT
      IF !FOUND()
        lcOldCostSheetID = ""
        lcOldType = ''
      ELSE
        lcOldCostSheetID = BOMHEADR_AG.CCSTSHT_ID
        lcOldType =  BOMHEADR_AG.CCSTSHTTYP
      ENDIF
    ENDIF
    IF EMPTY(lcOldCostSheetID) OR  EMPTY(lcNewCostSheetID)
      LOOP
    ENDIF

    IF lcNewCostSheetID ==lcOldCostSheetID
      IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
        INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'N')
      ELSE
        REPLACE Status WITH 'N' IN 'StyleList'
      ENDIF
      INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
        (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_COSTSHEET,lcOldCostSheetID ,lcNewCostSheetID )
    ENDIF


    SELECT BOM_AG
    =gfSEEK("0001"+PADR(ALLTRIM(KEY_CHG.Old_Key),19)+lcOldType+lcOldCostSheetID )

    SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)="0001"+PADR(ALLTRIM(KEY_CHG.Old_Key),19)+lcOldType+lcOldCostSheetID
      IF !gfSEEK("0001"+PADR(ALLTRIM(KEY_CHG.New_Key),19)+lcNewType+lcNewCostSheetID+BOM_AG.TYP+;
          STRTRAN(BOM_AG.CITMMASK,ALLTRIM(KEY_CHG.Old_Key),ALLTRIM(KEY_CHG.New_Key))+BOM_AG.MFGCODE+BOM_AG.CINVTYPC+BOM_AG.ITEM,'BOM_AG2')
        IF !SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList')
          INSERT INTO 'StyleList' VALUES (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),'A')
        ENDIF
        INSERT INTO  'IssueList' (OldStyle ,NewStyle ,FieldDesc ,OldValue ,NewValue ) VALUES ;
          (PADR(ALLTRIM(KEY_CHG.Old_Key),19),PADR(ALLTRIM(KEY_CHG.New_Key),19),LANG_COSTSHEET,BOM_AG.MFGCODE+BOM_AG.Item,"Does not Exist")
      ENDIF
    ENDSCAN



    SELECT KEY_CHG
  ENDSCAN
  SELECT 'StyleList'
  LOCATE FOR Status = 'N'
  IF !EOF()
    llResume = .F.
    DO FORM (oAriaApplication.screenhome + "SM\SMCONFLOG.SCX")
    IF !llResume
      RETURN
    ENDIF
  ENDIF
  SELECT 'StyleList'
  LOCATE
  IF !EOF()
    LOCATE FOR Status <> 'N'
    IF !FOUND()
      RETURN
    ENDIF
  ENDIF
ENDIF
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]


SET STEP ON 

PRIVATE laOpnFl,lnCnt,lnC,lcSkip, lnAlias
lnAlias = SELECT(0)
*-- CHECK THAT ALL USERS IS OUT THE SYSTEM
IF !USED('SYUSTATC')
  =gfOpenFile(oAriaApplication.SysPath+'SYUSTATC','CUSER_ID','SH')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
ENDIF
SELECT syustatc
LOCATE FOR cObj_name ='OLDVARS' .AND. syustatc.cStation <> oAriaApplication.Station ;
  .AND. syustatc.cComp_ID = ALLTRIM(loFormSet.laComp[loFormSet.lnComp,2])

*** If at least one user have a record in the static files
*** keep locating for all users
DO WHILE FOUND()
  lcUser    = syustatc.cUser_ID
  lcStion   = syustatc.cStation
  IF (lcUser = oAriaApplication.User_Id .AND. lcStion = oAriaApplication.Station)
    CONTINUE
  ELSE
    lcOldRep = SET('REPROCESS')
    SET REPROCESS TO 1
    IF RLOCK('SYUSTATC')
      UNLOCK IN ALIAS('SYUSTATC')
      SET REPROCESS TO lcOldRep
      CONTINUE
    ELSE
      SET REPROCESS TO lcOldRep
      =gfModalGen ('INM00025B00000','Alert')
*glQuitting = .T.
      RETURN .T.
    ENDIF
  ENDIF
ENDDO
DIMENSION laOpnFl[1,4],laRelF[1,2], laAltFlds[1,2]
laOpnFl[1,1] = ''
lnCnt = 1
STORE SPACE(0) TO laRelF,lcMainF,lcSkip
lnRelCnt = 1
SET ORDER TO TAG CFld_name IN SYDFLFLD
lnProc = SET('REPROCESS')
SET REPROCESS TO 20

*
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [Start]
IF loFormSet.lcKeyType = 'K'
  SELECT Key_CHG
  IF SEEK('KSZ1')
    *Exclude Matrial Tables[Start]
    DIMENSION laMatTables[6]
    laMatTables[1]='ITEM'
    laMatTables[2]='ITEMJRNL'
    laMatTables[3]='ITEMLOC'
    laMatTables[4]='ITEMADJ'
    laMatTables[5]='ROLLS'
    laMatTables[6]='SCALE'
    *Exclude Matrial Tables[End]

    IF !USED('SYDFIELD')
      =gfOpenTable('SYDFIELD','cFld_Name','SH','SYDFIELD')
    ENDIF

    IF !USED('SYDFLFLD_FILE')
      =gfOpenTable('SYDFLFLD','CFLFLD','SH','SYDFLFLD_FILE')
    ENDIF

    IF !USED('SYDFLFLD_FLFLD')
      =gfOpenTable('SYDFLFLD','CFLD_NAME','SH','SYDFLFLD_FLFLD')
    ENDIF

    IF !USED('SYDFILES')
      =gfOpenTable('SYDFILES','CFILE_NAM','SH','SYDFILES')
    ENDIF

    IF !USED('STYLEAG')
      =gfOpenTable('STYLE','STYLE','SH','STYLEAG')
    ENDIF

    IF !USED('STYLEUPC1')
      =gfOpenTable('STYLEUPC','STYLEUPC','SH','STYLEUPC1')
    ENDIF

    IF !USED('STYLEUPC2')
      =gfOpenTable('STYLEUPC','STYUPCN','SH','STYLEUPC2')
    ENDIF

    IF !USED('BOM_x')
      =gfOpenTable('BOM','MULTIBOM','SH','BOM_x')
    ENDIF

    IF !USED('BOM_y')
      =gfOpenTable('BOM','MITEMTYP','SH','BOM_y')
    ENDIF

    *Get all fields that have 8 in its name from SYDFLFLD [Start]
    SELECT DISTINCT cfld_name FROM SYDFIELD WHERE '8' $ cfld_name AND cdata_typ='N' AND;
      !ISDIGIT(SUBSTR(ALLTRIM(cfld_name),LEN(ALLTRIM(cfld_name))-1,1)) INTO ARRAY laListFields
    *Get all fields that have 8 in its name from SYDFLFLD  [End]
    SELECT Key_CHG
    SCAN REST WHILE CKEYTYPE+CKEY_CHG+OLD_KEY='KSZ1' FOR KEYSTATUS = 'O' .AND. !EMPTY(OLD_KEY) .AND. !EMPTY(New_Key) .AND. !EMPTY(MSIZES)
      IF !EMPTY(mSaveFilt)
        DECLARE laScopeSty[1]
        STORE "" TO laScopeSty
        RESTORE FROM MEMO mSaveFilt ADDITIVE
      ENDIF
      lcMsz=IIF(EMPTY(MSIZES),'',MSIZES)
      IF !EMPTY(lcMsz)
        *!*  @–1|2|3|4|5|6|7|8~B–2|3|1|6|5|4|7|0
        *!*  @
        *!*  1|2|3|4|5|6|7|8~B
        *!*  2|3|1|6|5|4|7|0
        ALINES(aFieldList1,lcMsz, "–")
        lcNewScal = aFieldList1[1]
        *!*  1|2|3|4|5|6|7|8
        *!*  B
        ALINES(aFieldList2,aFieldList1[2], "~")
        lcOldScal = aFieldList2[2]
        *!*  2|3|1|6|5|4|7|0
        ALINES(aOldSzValue,aFieldList1[3], "|")

        *Get System tables that have Scale field and Replace the old scale value with the new scale value[Start]
        *Get System tables that have Scale field [Strat]
        SELECT SYDFLFLD_FLFLD
        =SEEK('SCALE','SYDFLFLD_FLFLD')
        *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][Start]
        lnScalTableCnt = 0
        COUNT TO lnScalTableCnt REST WHILE CFLD_NAME+STR(NFLD_POS)  = 'SCALE'
        oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
        oProgress.TotalProgress = lnScalTableCnt 
        lcCaptionVal = LANG_WaitWindow
        oProgress.lblFirstLabel.CAPTION = lcCaptionVal 
        oProgress.SHOW()
        lnCntRecProg  = 1
        =SEEK('SCALE','SYDFLFLD_FLFLD')
        *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][End]
        SCAN REST WHILE cfld_name+STR(NFLD_POS) = 'SCALE'
          *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][Start]
          oProgress.CurrentProgress(lnCntRecProg)
          lnCntRecProg  = lnCntRecProg + 1
          *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][End]
          IF !EMPTY(ALLTRIM(SYDFLFLD_FLFLD.Cfile_nam))  AND ASCAN(laMatTables,ALLTRIM(UPPER(SYDFLFLD_FLFLD.Cfile_nam)))==0 AND;
              SEEK(PADR(SYDFLFLD_FLFLD.Cfile_nam,30)+PADR('STYLE',30),'SYDFLFLD_FILE')
            =SEEK(SYDFLFLD_FLFLD.Cfile_nam,'SYDFILES')
            *Get System tables that have Scale field [End]
            *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][Start]
            lcCaptionVal = LANG_WaitWindow
            oProgress.lblFirstLabel.CAPTION = lcCaptionVal +ALLTRIM(SYDFILES.cfile_ttl)
            *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][End]
            *Replace the old scale value with the new scale value [Start]
            lcTable=ALLTRIM(SYDFLFLD_FLFLD.Cfile_nam)
            lcTablesver=ALLTRIM(SYDFILES.cver)
            lcTablesTag=ALLTRIM(SYDFILES.cfile_tag)
            
            IF !USED(lcTable)
              *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [Start]
              llFileOpened = .T.
              TRY 
              *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [End]
                =gfOpenTable(lcTable,lcTablesTag,'SH',lcTable)
                llFileOpened = .T.
              *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [Start]
              CATCH  
                llFileOpened = .F.
              ENDTRY 
              IF !llFileOpened
                LOOP
              ENDIF
              *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [End]
            ENDIF
            
            

            SELECT &lcTable
            IF lcTablesver=='A40'
              =gfSEEK('')
            ENDIF
            IF RECCOUNT(lcTable)>0
              IF !EMPTY(laScopeSty) && if user selected certain styles to change its scale and sizes
                FOR j=1 TO ALEN(laScopeSty)
                  IF !EMPTY(ALLTRIM(laScopeSty[j]))
                    SCAN FOR STYLE = PADR(laScopeSty[j],19)
                      WAIT WINDOW LANG_WaitWindow+ALLTRIM(SYDFILES.cfile_ttl) NOWAIT
                      REPLACE SCALE  WITH  lcNewScal
                      =gfReplace('')
                    ENDSCAN
                    =gfTableUpdate(.T.)
                    *Update AUDTRAIL [Strat]
                    IF ALLTRIM(UPPER(lcTable))=='STYLE'
                      =lfAddAudit('STYLE')
                    ENDIF
                    SELECT &lcTable
                    *Update AUDTRAIL [End]
                  ENDIF
                ENDFOR
              ELSE && if user did not select certain style (i.e. select updating all styles
                SELECT STYLEAG
                SCAN FOR SCALE=lcOldScal
                  lcstyle=STYLEAG.STYLE
                  SELECT &lcTable
                  SCAN FOR STYLE ==  lcstyle
                    WAIT WINDOW LANG_WaitWindow+ALLTRIM(SYDFILES.cfile_ttl) NOWAIT
                    REPLACE  SCALE  WITH  lcNewScal
                    =gfReplace('')
                    *Update AUDTRL [Strat]
                    IF ALLTRIM(UPPER(lcTable))=='STYLE'
                      =lfAddAudit('STYLE')
                    ENDIF
                    *Update AUDTRL [End]
                    SELECT &lcTable
                  ENDSCAN
                  =gfTableUpdate(.T.)
                  SELECT STYLEAG
                ENDSCAN
              ENDIF
            ENDIF
            *Replace the old scale value with the new scale value [Start]
          ELSE
            LOOP
          ENDIF
        ENDSCAN
        *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][Start]
        oProgress = Null
        *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][End]
        *Get System tables that have Scale field and Replace the old scale value with the new scale value [End]
        llMap=.F.
        FOR i=1 TO ALEN(aOldSzValue)
          lnoldval=INT(VAL(aOldSzValue[i]))
          IF lnoldval=0
            LOOP
          ENDIF
          IF lnoldval!=i
            llMap=.T.
            EXIT
          ENDIF
        ENDFOR
        IF llMap
          *E611964,1 Es 12/02/2019  Change size scales from one size scale to another [End]
          *Get related tables of the ListFields [Start]
          *B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][Start]
          SET STEP ON 
          oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
          oProgress.TotalProgress = ALEN(laListFields)
          lcCaptionVal = LANG_WaitWindow
          oProgress.lblFirstLabel.CAPTION = lcCaptionVal 
          oProgress.SHOW()
          lnCntRecProg  = 1
          *B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][End]
          FOR N=1 TO ALEN(laListFields)
            *B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][Start]
            oProgress.CurrentProgress(lnCntRecProg)
            lnCntRecProg  = lnCntRecProg + 1
            *B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][End]
            SELECT SYDFLFLD_FLFLD
            =SEEK(laListFields[n],'SYDFLFLD_FLFLD')
            SCAN REST WHILE cfld_name+STR(NFLD_POS)= laListFields[n]
              IF !EMPTY(ALLTRIM(SYDFLFLD_FLFLD.Cfile_nam))  AND ASCAN(laMatTables,ALLTRIM(UPPER(SYDFLFLD_FLFLD.Cfile_nam)))==0 AND;
                  SEEK(PADR(SYDFLFLD_FLFLD.Cfile_nam,30)+PADR('STYLE',30),'SYDFLFLD_FILE')
                =SEEK(SYDFLFLD_FLFLD.Cfile_nam,'SYDFILES')
                *B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][Start]
                lcCaptionVal = LANG_WaitWindow
                oProgress.lblFirstLabel.CAPTION = lcCaptionVal +ALLTRIM(SYDFILES.cfile_ttl)
                *B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][End]
                lcListTable=ALLTRIM(SYDFLFLD_FLFLD.Cfile_nam)
                lcListVer=ALLTRIM(SYDFILES.cver)
                lcListTag=ALLTRIM(SYDFILES.cfile_tag)
                IF !USED(lcListTable)
                  *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [Start]
                  llFileOpened = .T.
                  TRY 
                    *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [End]
                    =gfOpenTable(lcListTable,lcListTag,'SH',lcListTable)
                    llFileOpened = .T.
                    *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [Start]
                  CATCH  
                    llFileOpened = .F.
                  ENDTRY 
                  IF !llFileOpened
                    LOOP
                  ENDIF
                  *E611964,2 MMT 12/17/2019  Change size scales from one size scale to another [End]    
                ENDIF

                SELECT &lcListTable
                IF lcListVer=='A40'
                  =gfSEEK('')
                ENDIF
                IF RECCOUNT(lcListTable)>0
                  IF !EMPTY(laScopeSty)
                    FOR j=1 TO ALEN(laScopeSty)
                      IF !EMPTY(ALLTRIM(laScopeSty[j]))
                        *Update StyleUPC Table AND bom TABLE.[Start]

                        IF N=1
                          =lfUpdateStyleUPC(laScopeSty[j])
                          =lfUpdateBom(laScopeSty[j])
                        ENDIF
                        SELECT &lcListTable
                        *Update StyleUPC Table.[End]
                        SCAN FOR STYLE=   PADR(laScopeSty[j],19)
                          *Get the old filed Value and the Mapping Fields Name.[Start]
                          WAIT WINDOW LANG_WaitWindow+ALLTRIM(SYDFILES.cfile_ttl)NOWAIT
                          DIMENSION laTmpQty[8]
                          FOR q=1 TO 8
                            *Get the old filed Value.[Start]
                            lnq=ALLTRIM(STR(q))
                            lcq=ALLTRIM(laListFields[n])
                            lcq=SUBSTR(lcq,1,LEN(lcq)-1)
                            lcq=lcq+lnq
                            laTmpQty[q]=&lcListTable..&lcq
                          ENDFOR
                          *Get the old filed Value.[End]

                          *Get the Mapping Fields Name.[Start]
                          lcstr=''
                          FOR u=1 TO 8
                            lnoldval=INT(VAL(aOldSzValue[u]))
                            lnq1=ALLTRIM(STR(u))
                            lcfld=ALLTRIM(laListFields[n])
                            lcfield=SUBSTR(lcq,1,LEN(lcfld)-1)
                            lcq1=lcfield+lnq1
                            *Updating tables by looping on related styles from point a based on Sizes mapping. [Start]
                            IF lnoldval!=0
                              lnindex2=ALLTRIM(STR(lnoldval))
                              lcq2=lcfield+lnindex2
                              REPLACE &lcListTable..&lcq1 WITH  laTmpQty[lnoldval]
                              IF VAL(lnindex2) > u
                                REPLACE &lcListTable..&lcq2 WITH  0
                              ENDIF
                            ELSE
                              REPLACE &lcListTable..&lcq1 WITH  0
                            ENDIF
                            =gfReplace('')
                          ENDFOR
                          *Updating tables by looping on related styles from point a based on Sizes mapping. [End]
                          *Get the Mapping Fields Name.[End]
                          *Get the old filed Value and the Mapping Fields Name.[End]
                        ENDSCAN
                        =gfTableUpdate(.T.)
                      ENDIF
                    ENDFOR
                  ELSE
                    SELECT STYLEAG
                    SCAN FOR SCALE=lcOldScal
                      lcstyle=STYLEAG.STYLE
                      *Update StyleUPC Table.[Start]
                      IF N=1
                        =lfUpdateStyleUPC(lcstyle)
                        =lfUpdateBom(lcstyle)
                      ENDIF
                      *Update StyleUPC Table.[End]
                      SELECT &lcListTable
                      SCAN FOR STYLE=   PADR(lcstyle,19)
                        *Get the old filed Value and the Mapping Fields Name.[Start]
                        WAIT WINDOW LANG_WaitWindow+ALLTRIM(SYDFILES.cfile_ttl) NOWAIT
                        DIMENSION laTmpQty[8]
                        FOR q=1 TO 8
                          *Get the old filed Value.[Start]
                          lnq=ALLTRIM(STR(q))
                          lcq=ALLTRIM(laListFields[n])
                          lcq=SUBSTR(lcq,1,LEN(lcq)-1)
                          lcq=lcq+lnq
                          laTmpQty[q]=&lcListTable..&lcq
                        ENDFOR
                        *Get the old filed Value.[End]

                        *Get the Mapping Fields Name.[Start]
                        FOR u=1 TO 8
                          lnoldval=INT(VAL(aOldSzValue[u]))
                          lnq1=ALLTRIM(STR(u))
                          lcfld=ALLTRIM(laListFields[n])
                          lcfield=SUBSTR(lcq,1,LEN(lcfld)-1)
                          lcq1=lcfield+lnq1
                          *Updating tables by looping on related styles from point a based on Sizes mapping. [Start]
                          IF lnoldval!=0
                            lnindex2=ALLTRIM(STR(lnoldval))
                            lcq2=lcfield+lnindex2
                            REPLACE &lcListTable..&lcq1 WITH  laTmpQty[lnoldval]
                            IF VAL(lnindex2) > u
                              &lcListTable..&lcq2 WITH  0
                            ENDIF
                          ELSE
                            REPLACE &lcListTable..&lcq1 WITH  0
                          ENDIF
                          =gfReplace('')
                        ENDFOR
*B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][Start]
            oProgress = Null
*B612047,1 Es  02/19/2020 Key Change screen does not show progress bar while updating in case of Scale Sizes change [T20200202.0003][End]
                        *Get the Mapping Fields Name.[End]
                        *Get the old filed Value and the Mapping Fields Name.[End]
                      ENDSCAN
                      =gfTableUpdate(.T.)
                      SELECT STYLEAG
                    ENDSCAN
                  ENDIF
                ENDIF
              ELSE
                LOOP
              ENDIF
            ENDSCAN
          ENDFOR
          *Get related tables of the ListFields [End]
        ENDIF
        SELECT Key_CHG
        REPLACE KEYSTATUS  WITH 'C',;
          Date_Procd WITH DATE()
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [End]

SELECT KEY_CHG
=SEEK(loFormSet.lcKeyType)

SCAN WHILE cKeyType=loFormSet.lcKeyType  FOR KEYSTATUS = 'O' .AND. !EMPTY(Old_Key) .AND. !EMPTY(New_Key)
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
  IF USED('StyleList') AND SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),19)+PADR(ALLTRIM(KEY_CHG.New_Key),19),'StyleList','StyleList') AND StyleList.Status = 'N'
    LOOP
  ENDIF
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
  IF SEEK(cKeyType+cKey_Chg,'SYDFDVLD') .AND. !EMPTY(SYDFDVLD.mSpcFunc)
    lcSpecFn = STRTRAN( SYDFDVLD.mSpcFunc, '()','("R")'  )
    llRet    = &lcSpecFn
    =lfOpClFile(loFormSet,'C')
    IF llRet
      SELECT KEY_CHG
      REPLACE KEYSTATUS  WITH 'C',;
        Date_Procd WITH DATE()
    ENDIF
    LOOP
  ENDIF
  IF SEEK(cKeyType+cKey_Chg,'SYDFDCHG')
    SELECT SYDFDCHG
    IF KEY_CHG.cKeyType = 'K'
      SCAN WHILE cKeyType+CFld_name = KEY_CHG.cKeyType + KEY_CHG.cKey_Chg
*-- If main field record , Get all files that has that field then replace old value by new value
        IF EMPTY(SYDFDCHG.cAltField)
          SELECT SYDFLFLD
          =SEEK(SYDFDCHG.CFld_name)
          SCAN WHILE CFld_name = SYDFDCHG.CFld_name
*-- Activate the file by openning or selecting it.
*-- Don't replace the field value in the main file
            IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFLFLD.cFile_nam),'')
              LOOP
            ENDIF

            lcFl = ALLTRIM(SYDFLFLD.cFile_nam)
            WAIT WINDOW 'Updating ' + lcFl +' file.' NOWAIT
*-- Replace Old value by new value
            lcFldtmp = ALLTRIM(SYDFDCHG.CFld_name)
            IF SYDFILES.CVER = 'A27'
              *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][Start]
              IF (lcFl ="CUSTOMER") AND (lcFldtmp = 'ACCOUNT') AND SEEK('M'+ALLTRIM(KEY_CHG.New_Key),lcFl,lcFl)
                REPLACE ALL &lcFldtmp WITH KEY_CHG.New_Key ;
                  FOR ALLTRIM(&lcFldtmp) == ALLTRIM(KEY_CHG.Old_Key) AND TYPE!='M'
              ELSE
              *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][End]
                SELECT (lcFl)
                REPLACE ALL &lcFldtmp WITH KEY_CHG.New_Key ;
                  FOR ALLTRIM(&lcFldtmp) == ALLTRIM(KEY_CHG.Old_Key)
              *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][Start]
              ENDIF
              *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][End]  
            ELSE
*- update sql
* there is no oportunity to fail as we alwyas creae new values in key fields
              lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
                "SET &lcFldtmp = '"+ALLTRIM(KEY_CHG.New_Key)+"' " +;
                "WHERE &lcFldtmp = '"+ALLTRIM(KEY_CHG.Old_Key)+"'"
              lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
              =lfSetstep(lnSQLRunResult)
            ENDIF
            WAIT CLEAR
          ENDSCAN
          IF lcFldtmp = 'ACCOUNT'
            =lfOpClFile(loFormSet,'O',ALLTRIM('NOTEPAD'),'')
            WAIT WINDOW 'Updating NOTEPAD file.' NOWAIT
            REPLACE ALL KEY WITH KEY_CHG.New_Key ;
              FOR ALLTRIM(KEY) == ALLTRIM(KEY_CHG.Old_Key) AND TYPE == 'A'
            WAIT CLEAR
          ENDIF

        ELSE   && i.e. !EMPTY(SYDFDVLD.cAltField)
*-- Open necessary files
          IF !SEEK(SYDFDCHG.cFile_nam,'SYDFILES')
            LOOP
          ENDIF
          IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag) )
            LOOP
          ENDIF

          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_nam) + ' file.' NOWAIT

*-- Replace Old value by new value
          lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)

          lcFltExp = lfGetExp(loFormSet)
*xUx*
          IF SYDFILES.CVER = 'A27'
            *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][Start]
            IF (SYDFDCHG.cFile_nam ="CUSTOMER") AND (lcFldtmp = 'ACCOUNT') AND SEEK('M'+ALLTRIM(KEY_CHG.New_Key),ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cFile_nam))
              REPLACE ALL &lcFldtmp WITH KEY_CHG.New_Key ;
                FOR ALLTRIM(&lcFldtmp) == ALLTRIM(KEY_CHG.Old_Key) ;
                .AND. &lcFltExp AND TYPE !='M'
            ELSE
            *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][End]
              REPLACE ALL &lcFldtmp WITH KEY_CHG.New_Key ;
                FOR ALLTRIM(&lcFldtmp) == ALLTRIM(KEY_CHG.Old_Key) ;
               .AND. &lcFltExp
           *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][Start]
           ENDIF
           *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][End]
          ELSE
            lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
              "SET &lcFldtmp = '"+KEY_CHG.New_Key +"' "+;
              "WHERE &lcFldtmp = '"+PADR(ALLTRIM(KEY_CHG.Old_Key),lfGetFSIZE(lcFldtmp))+"' "+;
              "AND &lcFltExp"
            lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
            =lfSetstep(lnSQLRunResult)
          ENDIF
          WAIT CLEAR
        ENDIF
      ENDSCAN
    ELSE  && i.e. cKeyType = 'F'
      DIMENSION laRelF[1,2]
      lnRelCnt = 1
      STORE SPACE(0) TO laRelF
      SELECT SYDFDCHG
      STORE SPACE(0) TO laAltFlds
      lnNum = 0
*-- Scan loop to Open necessary files and prepare relation between
*-- main file and its cheldren.
*-- laAltFlds array hold each field with its file and filter
*-- expression if any to update all fields on one loop on the main
*-- file.
      SCAN WHILE cKeyType+CFld_name = 'F'+KEY_CHG.cKey_Chg
        IF EMPTY(cAltField)
          lcMainF = SYDFDCHG.cFile_nam
        ENDIF
        lnNum = lnNum + 1
        DIMENSION laAltFlds[lnNum,3]
        laAltFlds[lnNum,1] = SYDFDCHG.cFile_nam
        laAltFlds[lnNum,2] = IIF(EMPTY(SYDFDCHG.cAltField),SYDFDCHG.CFld_name,SYDFDCHG.cAltField)
        laAltFlds[lnNum,3] = IIF(EMPTY(SYDFDCHG.mFltExpr),".T.",ALLTRIM(SYDFDCHG.mFltExpr))
        IF ASCAN(laRelF,SYDFDCHG.cFile_nam) = 0 .AND. lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag) )
          IF !EMPTY(SYDFDCHG.mRelation)
*-- laRelF array hold each child file with its relation.
            laRelF[lnRelCnt,1] = SYDFDCHG.cFile_nam
            laRelF[lnRelCnt,2] = ALLTRIM(SYDFDCHG.mRelation)
            lnRelCnt = lnRelCnt+1
            DIMENSION laRelF[lnRelCnt,2]
            STORE SPACE(0) TO laRelF[lnRelCnt,1]
          ENDIF
        ELSE
          LOOP
        ENDIF
      ENDSCAN

*-- Set necessary relations to the main file
*B610613,1 TMI 12/05/2013 09:50 [Start] fox does no recognize alias with spaces, use & instead of ()
*SELECT (lcMainF)
      SELECT &lcMainF
*B610613,1 TMI 12/05/2013 09:50 [End  ]
      FOR lnC = 1 TO ALEN(laRelF,1)
        IF !EMPTY(laRelF[lnC,1])
          SET RELATION TO &laRelF[lnC,2] INTO &laRelF[lnC,1] ADDITIVE
*-- Collect files have relation with the main file to set the skip sequence
          lcSkip = lcSkip+IIF(EMPTY(lcSkip),'',',')+laRelF[lnC,1]
        ENDIF
      ENDFOR
      SET SKIP TO &lcSkip
*-- Get the saved filter

      RELEASE laFixFltr
      IF !EMPTY(KEY_CHG.mSaveFilt)
        RESTORE FROM MEMO KEY_CHG.mSaveFilt ADDITIVE
      ELSE
*B610613,1 TMI 12/05/2013 14:37 [Start] no need for these lines
*STORE SPACE(0) TO loFormSet.laFixFltr
*DIMENSION laFixFltr[ALEN(loFormSet.laFixFltr,1),ALEN(loFormSet.laFixFltr,2)]
*ACOPY(loFormSet.laFixFltr,laFixFltr)
*B610613,1 TMI 12/05/2013 14:37 [End  ]
      ENDIF
      IF TYPE('laFixFltr')='U'
        DIMENSION laFixFltr[1,7]
        laFixFltr = ''
      ENDIF

      lcFilt = ALLTRIM(gfGenFlt('laFixFltr', .T.))
      lcFilt=IIF(EMPTY(lcFilt),".T.",lcFilt)

*-- Scan on the main file and replace values in its children
      WAIT WINDOW 'Updating ' + ALLTRIM(lcMainF) +' file.' NOWAIT

      SCAN FOR &lcFilt
        FOR lnC = 1 TO ALEN(laAltFlds,1)
          SELECT &laAltFlds[lnC,1]
          lcFldtmp = laAltFlds[lnC,2]
          IF &lcFldtmp = ALLTRIM(KEY_CHG.Old_Key) .AND. &laAltFlds[lnC,3]
            REPLACE &lcFldtmp WITH KEY_CHG.New_Key
          ENDIF
        ENDFOR
      ENDSCAN

      WAIT CLEAR
    ENDIF
  ENDIF

  IF UPPER(ALLTRIM(loFormSet.laFields[loFormSet.lnfields,1])) $ "CUSTOMER|VENDOR"
    lnAlias = SELECT(0)
    IF !USED('SYSCHDUL')
      =gfOpenTable(oAriaApplication.SysPath + 'SYSCHDUL' , '' , 'SH')
      SELECT SYSCHDUL
      SET ORDER TO SCHACCT ASCENDING
    ELSE
      SELECT SYSCHDUL
      SET ORDER TO
      SET ORDER TO SCHACCT ASCENDING
    ENDIF

    WAIT WINDOW 'Updating ' + 'SYSCHDUL' +' file.' NOWAIT
    REPLACE ALL cCont_Id with PADR(KEY_CHG.New_Key,8) FOR cCont_Id = PADR(KEY_CHG.Old_Key,8)
    =gfTableUpdate(.T.)
    =gfCloseTable('SYSCHDUL')

    IF !USED('CONTACT')
      = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3]) + 'CONTACT' , 'CONTACT' , 'SH')
      SELECT CONTACT
    ELSE
      SELECT CONTACT
    ENDIF
    WAIT WINDOW 'Updating ' + 'CONTACT' +' file.' NOWAIT

    REPLACE ALL cCont_Id with PADR(KEY_CHG.New_Key,8) FOR ALLTRIM(cCont_Id) == ALLTRIM(PADR(KEY_CHG.Old_Key,8))
    =gfTableUpdate(.T.)
    =gfCloseTable('CONTACT')
    WAIT CLEAR

  ENDIF
*-- Close opened files (Files opened to change values in them)
  =lfOpClFile(loFormSet,'C')
  SELECT KEY_CHG
  REPLACE KEYSTATUS  WITH 'C',;
    Date_Procd WITH DATE()
*
ENDSCAN



SELECT KEY_CHG
LOCATE
=gfTableUpdate(.T.)

*-- Build the temporary files again
=lfBldTmp(loFormSet)
*-- Refresh the browse
=lfbrwOpn(loFormSet)

SET REPROCESS TO (lnProc)
*- End of lfvPrcd.

************************************************************
*! Name      : lfGetFSIZE
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/10/2013
*! Purpose   : Get field size
************************************************************
FUNCTION lfGetFSIZE
PARAMETERS lcFld
LOCAL lnRet
=SEEK(PADR(lcFld,oAriaApplication.FileW),'SYDFIELD')
lnRet = SYDFIELD.nFld_Wdth
RETURN lnRet

*- End of lfGetFSIZE.

*!*************************************************************
*! Name      : lfOpClFile
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To Open files then close them after processing
*!*************************************************************
*! Calls       : FUNCTIONS  : gfOpenTable()
*!               PROCEDURES :
*!*************************************************************
*! Passed Parameters : lcStat,lcfil,lcTag
*!                     lcStat = "O" for Open, "C" for close
*!                     lcFile File to be opened
*!                     lcTag  Tag name of the file
*!*************************************************************
FUNCTION lfOpClFile
PARAMETERS loFormSet,lcStat,lcfil,lcTag

IF lcStat='O'
  IF !SEEK(PADR(lcfil,oAriaApplication.FileW),'SYDFILES')
    RETURN .F.
  ENDIF
  IF SYDFILES.CVER <> 'A27'
    RETURN
  ENDIF
  IF !FILE(oAriaApplication.DataDir+ALLTRIM(lcfil)+'.DBF')
    RETURN .F.
  ENDIF

*IF !USED(lcFil)
  IF .T.
    laOpnFl[lnCnt,1] = lcfil
    laOpnFl[lnCnt,2] = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+ALLTRIM(lcfil),lcTag,'SH')

    SELECT (ALLTRIM(lcfil))

    llLock = FLOCK()
    IF !llLock
*-- "File " + &lcFil + " could not be locked. This file will be ignored."
*--                        < OK >
      =gfModalGen("INM00333B00000","DIALOG",lcfil)
      RETURN .F.
    ENDIF
    laOpnFl[lnCnt,3] = ''
    laOpnFl[lnCnt,4] = ''
    lnCnt = lnCnt+1
    DIMENSION laOpnFl[lnCnt,4]
    laOpnFl[lnCnt,1] = SPACE(0)
  ELSE
    =gfCloseTable(lcfil)
    laOpnFl[lnCnt,1] = lcfil
    laOpnFl[lnCnt,2] = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+ALLTRIM(lcfil), lcTag,'SH')
    SELECT(ALLTRIM(lcfil))
    llLock = FLOCK()
    IF !llLock
*-- "File " + &lcFil + " could not be locked. This file will be ignored."
*--                        < OK >
      =gfModalGen("INM00333B00000","DIALOG",lcfil)
      RETURN .F.
    ENDIF
    IF SYDFILES.CVER = 'A27'
      laOpnFl[lnCnt,3] = DBF(lcfil)
      laOpnFl[lnCnt,4] = ORDER(lcfil)
      lnCnt = lnCnt+1
      DIMENSION laOpnFl[lnCnt,4]
      laOpnFl[lnCnt,1] = SPACE(0)
    ENDIF
*ENDIF
  ENDIF
  IF SYDFILES.CVER = 'A27'
    SELECT (lcfil)
  ENDIF
ELSE
*-- Close files
  FOR lnC = 1 TO ALEN(laOpnFl,1)
*-- If the file opened by me, close it
    IF !EMPTY(laOpnFl[lnC,1]) .AND. USED(laOpnFl[lnC,1]) .AND. laOpnFl[lnC,2]
*USE IN (laOpnFl[lnC,1])
      SELECT (laOpnFl[lnC,1])
      =SEEK(laOpnFl[lnC,1],'SYDFILES')
      IF SYDFILES.CVER = 'A27'
        WAIT WINDOW NOWAIT 'Updating '+laOpnFl[lnC,1] + ' ... '
        =gfTableUpdate(.T.)
      ENDIF
      =gfCloseTable(laOpnFl[lnC,1])
      WAIT CLEAR

*-- If opened instead of another file, open the other file
      IF !EMPTY(laOpnFl[lnC,3])
        =gfOpenTable(laOpnFl[lnC,3],laOpnFl[lnC,4],'SH')
      ENDIF
    ENDIF
  ENDFOR
  DIMENSION laOpnFl[1,4]
  laOpnFl[1,1] = SPACE(0)
  lnCnt = 1
ENDIF
RETURN
*- End of lfOpClFile.

*!*************************************************************
*! Name      : lfVldBrw
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To browse the old or new value field based on the data
*!             in SYDFDVLD file
*!*************************************************************
*! Called from : lfvNewVal(), lfvOldVal()
*!*************************************************************
FUNCTION lfVldBrw
PARAMETERS loBranchFormSet,lcVal
LOCAL loFormSet
loFormSet = loBranchFormSet.loFormSet
*-- Go to the field record in the SYDFDVLD table to get the browse information
IF SEEK(loFormSet.lcKeyType+loFormSet.laFields[loFormSet.lnfields,2],'SYDFDVLD') .AND. ;
    (!gfSEEK( ALLTRIM(SYDFDVLD.mKeyExpr)+PADR(ALLTRIM(lcVal),loFormSet.laFields[loFormSet.lnfields,4]),ALLTRIM(SYDFDVLD.cMainFile)) .OR. loFormSet.llBrowse)

  SELECT (ALLTRIM(SYDFDVLD.cMainFile))

  gfSEEK('')

*-- Get the browse fields

  DO CASE
  CASE "Customer" $ loFormSet.laFields[loFormSet.lnfields,1]

    lcBrFields = "ACCOUNT    :R :H= 'Account Code'       ," +;
      "STNAME     :R :H= 'STNAME '            ," +;
      "PHONE1     :R :H= 'Phone  '            ," +;
      "BUYER      :R :H= 'BUYER  '            ," +;
      "SALESREP   :R :H= 'Sales Representive' ," +;
      "CADDRESS1  :R :H= 'CADDRESS1'          ," +;
      "CADDRESS2  :R :H= 'CADDRESS2'          ," +;
      "CADDRESS3  :R :H= 'CADDRESS3'          ," +;
      "CADDRESS4  :R :H= 'CADDRESS4'          ," +;
      "CADDRESS5  :R :H= 'CADDRESS5'          ," +;
      "CADDRESS12 :R :H= 'CADDRESS12'         ," +;
      "CADDRESS22 :R :H= 'CADDRESS22'         ," +;
      "CADDRESS32 :R :H= 'CADDRESS32'         ," +;
      "CADDRESS42 :R :H= 'CADDRESS42'         ," +;
      "CADDRESS52 :R :H= 'CADDRESS52'         ," +;
      "NETBAL     :R :H= 'NETBAL'             "
  CASE "Fabric" $ loFormSet.laFields[loFormSet.lnfields,1]
*B610613,3 TMI 12/11/2013 20:10 [Start] remove the MAKE field
* for this field in the SYDFIELD.MVENTRIES it contains the value 'Yes|No~Y|N' while the field type is LOGICAL,
* in the runnig  an expression is created as follows
* lcControlSource = iif(make='Y', ...
* which give type mismatch error,
* I thought that updating sydfield by removing this Y/N requires update file structure at all customers, to avoide this I decided to remove this field
* from the temp browse list
*lcBrFields = "CSTYMAJOR :H='Fabric',"+;
"DESC :H='Description',"+;
"MAKE :H='Manufactured/Purchased',"+;
"TOTCOST :H='Cost'"
    lcBrFields = "CSTYMAJOR :H='Fabric',"+;
      "DESC :H='Description',"+;
      "TOTCOST :H='Cost'"
*B610613,3 TMI 12/11/2013 20:10 [End  ]

*E303425,3 TMI 11/24/2013 18:19 [Start]
    lcTmpItem = gfTempName()
    SELECT CINVTYPE,CSTYMAJOR,DESC,MAKE,TOTCOST ;
      FROM ITEM INTO CURSOR &lcTmpItem
    SELECT (lcTmpItem)
*E303425,3 TMI 11/26/2013 13:18 [Start]
*INDEX ON CSTYMAJOR TAG CSTYMAJOR UNIQUE
    INDEX ON CINVTYPE+CSTYMAJOR TAG CSTYMAJOR UNIQUE
*E303425,3 TMI 11/26/2013 13:18 [End  ]
*E303425,3 TMI 11/24/2013 18:19 [End  ]

  OTHERWISE
    lcBrFields=gfDbfField(ALLTRIM(SYDFDVLD.cMainFile))
  ENDCASE

  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Select Value"
*-- Get the browse title
  IF SEEK(SYDFDVLD.cMainFile,'SYDFILES')
    lcFile_ttl = SYDFILES.cFile_ttl
  ENDIF
  lcFilt = IIF(!EMPTY(SYDFDVLD.mFltExpr),'FOR ' +ALLTRIM(SYDFDVLD.mFltExpr),;
    IIF(!EMPTY(SYDFDVLD.mKeyExpr), ;
    ALLTRIM(SYDFDVLD.mKeyExpr),''))

  IF !EMPTY(SYDFDVLD.mFltExpr)
    =gfBrows(lcFilt,ALLTRIM(SYDFDVLD.cOrg_Fld),"laTemp")
  ELSE
    =gfBrows("lcFilt",ALLTRIM(SYDFDVLD.cOrg_Fld),"laTemp")
  ENDIF
  lcVal = laTemp[1]

*E303425,3 TMI 11/24/2013 18:20 [Start]
  IF TYPE('lcTmpItem') = 'C' AND USED(lcTmpItem)
    USE IN (lcTmpItem)
  ENDIF
*E303425,3 TMI 11/24/2013 18:20 [End  ]
ENDIF
IF EMPTY(lcVal)
  lcVal = loFormSet.lcOld
  llRet = .F.
ENDIF

RETURN lcVal
*- End of lfVldBrw

*!*************************************************************
*! Name      : lfbrwOpn
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To Browse open keys or fields.
*!*************************************************************
*! Called from : SMFLDCH.scx, lfCrtFiles(), lfvFiles(), lfvFields(),
*!               lfvValues(), lfvPrcd()
*!*************************************************************
*! Calls       : FUNCTIONS  : gfCodDes()
*!               FUNCTION  : None
*!*************************************************************
FUNCTION lfbrwOpn
PARAMETERS loFormSet

PRIVATE lnAlias,lcDum,lcDumm,lcCond
STORE SPACE(0) TO lcDum,lcDumm
lnAlias = SELECT(0)

SELECT (loFormSet.lcNValues)
GO TOP

lcKeyType = loFormSet.lcKeyType
SET KEY TO "&lcKeyType."
lcCond = IIF(loFormSet.laFields[loFormSet.lnfields,2]=loFormSet.lcDummyCom,'.T.',;
  "cKey_Chg = '"+loFormSet.laFields[loFormSet.lnfields,2]+"'" )
SET FILTER TO &lcCond
LOCATE
loFormSet.AriaForm1.pgfChange.Ariapage1.Ariagrid1.Refresh()


SELECT(loFormSet.lcHistory)
lcKeyType = loFormSet.lcKeyType
SET KEY TO "&lcKeyType."
lcCond = IIF(loFormSet.laFields[loFormSet.lnfields,2]=loFormSet.lcDummyCom,".T.","cKey_Chg = '"+loFormSet.laFields[loFormSet.lnfields,2]+"'" )
SET FILTER TO &lcCond
LOCATE

*- refresh the grid
lfDefControlSource(loFormSet)

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfMajor
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Special function of the major key for Old , New Values
*!             and replacing process
*!*************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!*************************************************************
*! Calls       : FUNCTIONS  : gfOpenTable(), gfStyBrw(), lfOpClFile(loFormSet,)
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : lcStatus
*!                     lcStatus = "O"  Validation of old value object
*!                     lcStatus = "N"  Validation of New value object
*!                     lcStatus = "R"  Replacing process
*!*************************************************************
*! Notes       : Name of this function must be in mSpcFunc field
*!               in the SYDFDVLD file.
*!*************************************************************
FUNCTION lfMajor
PARAMETERS lcStatus

PRIVATE lcSrchCod, lcCod, lnLen, lcOrd, llopSGVL, llopsty, llopscl
STORE .F. TO llopSGVL, llopsty, llopscl
llVld = .T.
STORE SPACE(0) to lcSrchCod, lcCod, lcOrd

IF FILE(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICISTRU.DBF')
  llICISTRU = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICISTRU','Segno','SH')
  SELECT ICISTRU
  IF !SEEK('U1')
    IF llICISTRU
      =gfCloseTable('ICISTRU')
    ENDIF
    RETURN .F.
  ENDIF
  IF llICISTRU
    =gfCloseTable('ICISTRU')
  ENDIF
ELSE
  RETURN .F.
ENDIF

llopsty = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'STYLE','CSTYLE','SH')
llopscl = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'SCALE','SCALE','SH')
llopSGVL =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICSEGVAL','SEGVAL','SH')

DO CASE
***********************************************************************
*-- Validation of the old value
CASE lcStatus = 'O'
*loFormSet.lcOldVal = lfVldBrw(loFormSet.lcOldVal)
  IF !SEEK(loFormSet.lcOldVal, 'STYLE')
    loFormSet.lcOldVal = gfStyBrw('M','','',.F.)
  ENDIF
***********************************************************************
*-- Validations of the new value
CASE lcStatus = 'N'
*-- If not existing major, validate its structure
  IF !SEEK(loFormSet.lcNewVal,'STYLE')
*loFormSet.lcMajor = gfStyBrw('M',loFormSet.lcMajor,lcNonMjr,.T.)
*-- 'Style '+ ALLTRIM(loFormSet.lcNewVal) +'does not exist in the style file.
*--                     Continue anyway?'
*--              < YES >             < NO >

    lnLen = 1
    SELECT CODES
    lcOrd = ORDER()
    SET ORDER TO TAG cCode_No
*-- Loop to get segment by segment
    FOR lnC=1 TO loFormSet.lnMajSeg
      lcCod = SUBSTR(loFormSet.lcNewVal,lnLen,LEN(loFormSet.laMajSeg[lnC,3]) )
      lnLen = lnLen + LEN(loFormSet.laMajSeg[lnC,3])
*!   SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size

      DO CASE
      CASE loFormSet.laMajSeg[lnC,1] $ 'OTQ'
        llopSGVL =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICSEGVAL','SEGVAL','SH')
        IF !SEEK(STR(lnC,1)+lcCod,'ICSEGVAL')
          llVld  = .F.
        ENDIF
      CASE loFormSet.laMajSeg[lnC,1] $ 'ZCDG'
*-- check if these segments is a valid code
        lcSrchCod = IIF(loFormSet.laMajSeg[lnC,1]='Z','SEASON', ;
          IIF(loFormSet.laMajSeg[lnC,1]='C','COLOR', ;
          IIF(loFormSet.laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
        lcSrchCod = PADR(lcSrchCod,10)
        IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
          llVld  = .F.
        ENDIF
*-- If the segment is of type scale, check if it is a valid scale
      CASE loFormSet.laMajSeg[lnC,1] = 'S'
        IF !SEEK('S'+lcCod,'SCALE' )
          llVld  = .F.
        ENDIF
      ENDCASE
      IF !llVld
*-- 'Invalid Major code segments. Cannot add to the file.'
*--        < OK >
        =gfModalGen("INM00334B00000","DIALOG","Major")

        =gfStyBrw('M',loFormSet.lcNewVal,'',.F.)
        EXIT
      ENDIF
    ENDFOR
    SELECT CODES
    SET ORDER TO (lcOrd)
  ELSE
*-- Message : Style XXXX already exists in the style file, cannot proceed.
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
*!*        =gfModalgen('TRM00372B00000',"DIALOG",ALLTRIM(loFormSet.lcNewVal))
*!*        loFormSet.lcNewVal = loFormSet.lcOld
*!*        RETURN .F.
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
  ENDIF
  IF ('?' $ loFormSet.lcNewVal)
    =gfModalGen("TRM00385B00000","DIALOG")
    llVld = .F.
  ENDIF
************************************************************************
*-- Replacing process
CASE lcStatus = 'R'
*-- Validate the new value agian before updating
  lnLen = 1
  SELECT CODES
  lcOrd = ORDER()
  SET ORDER TO TAG cCode_No
  FOR lnC=1 TO loFormSet.lnMajSeg
    lcCod = SUBSTR(KEY_CHG.New_Key,lnLen,LEN(loFormSet.laMajSeg[lnC,3]) )
    lnLen = lnLen + LEN(loFormSet.laMajSeg[lnC,3])
*!   SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size
    DO CASE
    CASE loFormSet.laMajSeg[lnC,1] $ 'OTQ'
      IF !SEEK(STR(lnC,1)+lcCod,'ICSEGVAL')
        llVld  = .F.
      ENDIF
    CASE loFormSet.laMajSeg[lnC,1] $ 'ZCDG'
      lcSrchCod = IIF(loFormSet.laMajSeg[lnC,1]='Z','SEASON', ;
        IIF(loFormSet.laMajSeg[lnC,1]='C','COLOR', ;
        IIF(loFormSet.laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
      lcSrchCod = PADR(lcSrchCod,10)
      IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
        llVld  = .F.
      ENDIF
    CASE loFormSet.laMajSeg[lnC,1] = 'S'
      IF !SEEK('S'+lcCod,'SCALE' )
        llVld  = .F.
      ENDIF
    ENDCASE
*-- If not a valid major, don't replace or do any thing.
    IF !llVld
      RETURN .F.
    ENDIF
  ENDFOR

  SELECT SYDFDCHG
  =SEEK('K'+'STYLE')
  SCAN WHILE cKeyType+CFld_name = 'K'+'STYLE'
*-- If main field record , Get all files that has that field then replace old value by new value
    IF EMPTY(SYDFDCHG.cAltField)
*E303425,3 TMI 11/25/2013 16:22 [Start] open another alias of SYDFLFLD to check if CINVTYPE is included in the table structure, if so then skip this table
**                                      from this if condition as it will be dealt with separately
      SELECT SYDFLFLD
      USE (DBF()) AGAIN IN 0 ORDER CFLFLD ALIAS FLFLD
*E303425,3 TMI 11/25/2013 16:22 [End  ]

      SELECT SYDFLFLD
      =SEEK('STYLE')
      SCAN WHILE CFld_name = 'STYLE'

*E303425,3 TMI 11/25/2013 16:21 [Start]
        IF SEEK(SYDFLFLD.cFile_nam+'CINVTYPE','FLFLD')
          LOOP
        ENDIF
*E303425,3 TMI 11/25/2013 16:21 [End  ]

*-- Open necessary file.
        IF  !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFLFLD.cFile_nam),'')
          LOOP
        ENDIF

        WAIT WINDOW 'Updating style file.' NOWAIT

        IF ALLTRIM(SYDFLFLD.cFile_nam) == 'STYLE'
          SELECT STYLE
          SET ORDER TO
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
          IF !USED("STYLE_CH")
            =gfOpenTable('STYLE','STYLE','SH',"STYLE_CH")
          ENDIF
          lcDelSet = SET("Deleted")
          SET DELETED OFF
          SET ORDER TO STYLE
          =SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize),'STYLE')
          SCAN REST WHILE STYLE = PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize) FOR !DELETED()
            lcCurrentStyle = STYLE.STYLE
            IF SEEK(PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize) +  SUBSTR(STYLE,loFormSet.lnMajSize+1),"STYLE_CH",'STYLE')
              SELECT STYLE
              DELETE
            ENDIF
          ENDSCAN
          SET DELETED &lcDelSet.
          REPLACE ALL STYLE  WITH PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize) + ;
            SUBSTR(STYLE,loFormSet.lnMajSize+1), ;
            CSTYMAJOR WITH ALLTRIM(KEY_CHG.New_Key) ;
            FOR ALLTRIM(CSTYMAJOR) == ALLTRIM(KEY_CHG.Old_Key)
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
          SET ORDER TO CSTYLE
          =SEEK(ALLTRIM(KEY_CHG.New_Key))
*-- Change necessary fields of the style record due to the new segment values.
          SCAN WHILE CSTYMAJOR = ALLTRIM(KEY_CHG.New_Key)
            lnLen = 1
            FOR lnC=1 TO loFormSet.lnMajSeg
              lcCod = SUBSTR(KEY_CHG.New_Key,lnLen,LEN(loFormSet.laMajSeg[lnC,3]) )
              lnLen = lnLen + LEN(loFormSet.laMajSeg[lnC,3])
*!   SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size
              DO CASE
              CASE loFormSet.laMajSeg[lnC,1] = 'Z'
                REPLACE Season WITH lcCod
              CASE loFormSet.laMajSeg[lnC,1] = 'D'
                REPLACE CDivision WITH lcCod
              CASE loFormSet.laMajSeg[lnC,1] = 'G'
                REPLACE cstygroup WITH lcCod
              CASE loFormSet.laMajSeg[lnC,1] = 'S'
                REPLACE SCALE WITH lcCod
              CASE loFormSet.laMajSeg[lnC,1] = 'F'
                IF !SEEK(STR(lnC,1)+lcCod,'ICSEGVAL')
*-- add a record for the free part in icsegval file
                  INSERT INTO ICSEGVAL (cISegNo,cISegVal) VALUES (STR(lnC,1),lcCod)
                ENDIF
              ENDCASE
            ENDFOR
          ENDSCAN
        ELSE
          IF SYDFILES.CVER = 'A27'
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
            IF ALLTRIM(SYDFLFLD.cFile_nam) == 'STYDYE'
              IF !USED("STYDYE_CH")
                =gfOpenTable('STYDYE','STYDYE','SH',"STYDYE_CH")
              ENDIF
              lcDelSet = SET("Deleted")
              SET DELETED OFF
              SET ORDER TO STYDYE
              =SEEK(PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize),'STYDYE')
              SCAN REST WHILE STYLE+CWARECODE+DYELOT = PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize) FOR !DELETED()
                lcCurrentStyle = STYDYE.STYLE
                IF SEEK(PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize) +  SUBSTR(STYLE,loFormSet.lnMajSize+1)+STYDYE.CWARECODE+STYDYE.DYELOT,"STYDYE_CH",'STYDYE')
                  SELECT STYDYE
                  DELETE
                ENDIF
              ENDSCAN
              SET DELETED &lcDelSet.
            ENDIF
            IF INLIST(ALLTRIM(SYDFLFLD.cFile_nam) ,'ORDLINE','INVLINE')
              lcFld = IIF(ALLTRIM(SYDFLFLD.cFile_nam)=='ORDLINE','CORDTYPE+ORDER' ,'INVOICE')
              SELECT DISTINCT &lcFld. as 'cKey' FROM (ALLTRIM(SYDFLFLD.cFile_nam)) WHERE ALLTRIM(SUBSTR(STYLE,1,loFormSet.lnMajSize)) == ALLTRIM(KEY_CHG.Old_Key) INTO CURSOR 'TranTmp'
              lfAddAuditInfo('TranTmp',ALLTRIM(SYDFLFLD.cFile_nam))
              SELECT (ALLTRIM(SYDFLFLD.cFile_nam))
            ENDIF
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
            REPLACE ALL STYLE WITH PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize)+;
              SUBSTR(STYLE,loFormSet.lnMajSize+1) ;
              FOR ALLTRIM(SUBSTR(STYLE,1,loFormSet.lnMajSize)) == ALLTRIM(KEY_CHG.Old_Key)

          ELSE
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
            IF INLIST(ALLTRIM(SYDFLFLD.cFile_nam) ,'RETLINE','RALINE','POSLN')
              lcSqlSle =  "SELECT distinct "+IIF(ALLTRIM(SYDFLFLD.cFile_nam) ='RETLINE','CRMEMO',IIF(ALLTRIM(SYDFLFLD.cFile_nam) ='RALINE','RANO',;
                'CSTYTYPE+CBUSDOCU+PO'))+" AS 'CKEY' FROM "+ALLTRIM(SYDFLFLD.cFile_nam)+" WHERE  SUBSTRING(STYLE,1,"+STR(loFormSet.lnMajSize,2)+") = '"+PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize)+"'"
              lnSQLResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlSle ,'TranTmp','TranTmp', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
              lfAddAuditInfo('TranTmp',ALLTRIM(SYDFLFLD.cFile_nam))
*SELECT (ALLTRIM(SYDFLFLD.cFile_Nam))
            ENDIF
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
            lcFs = ALLTRIM(STR(loFormSet.lnMajSize+1))
*E303425,3 TMI 11/25/2013 10:26 [Start]
*lcLn = ALLTRIM(STR(19 - (loFormSet.lnMajSize + 1)))
            lcLn = ALLTRIM(STR(19 - (loFormSet.lnMajSize)))
*E303425,3 TMI 11/25/2013 10:26 [End  ]
            lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
              "SET STYLE = '"+PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize)+"'+SUBSTRING(STYLE,&lcFs,&lcLn) "+;
              "WHERE SUBSTRING(STYLE,1,"+STR(loFormSet.lnMajSize,2)+") = '"+PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize)+"'"
            lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
            =lfSetstep(lnSQLRunResult)

          ENDIF
        ENDIF
        WAIT CLEAR
      ENDSCAN

*E303425,3 TMI 11/25/2013 16:27 [Start]
      USE IN FLFLD
*E303425,3 TMI 11/25/2013 16:27 [End  ]

    ELSE   && i.e. !EMPTY(SYDFDCHG.cAltField)
*-- Open necessary files
      IF !SEEK(SYDFDCHG.cFile_nam,'SYDFILES')
        LOOP
      ENDIF
      IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag) )
        LOOP
      ENDIF
      WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_nam) + ' file.' NOWAIT
*-- Replace Old value by new value
      lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)

      IF SYDFILES.CVER = 'A27'
        lcFltExp = lfGetExp(loFormSet)
        REPLACE ALL &lcFldtmp WITH PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize)+;
          SUBSTR(&lcFldtmp,loFormSet.lnMajSize+1) ;
          FOR ALLTRIM(SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize)) == ALLTRIM(KEY_CHG.Old_Key) ;
          .AND. &lcFltExp
      ELSE
        lcFs = ALLTRIM(STR(loFormSet.lnMajSize+1))
*E303425,3 TMI 11/25/2013 10:26 [Start]
*lcLn = ALLTRIM(STR(19 - (loFormSet.lnMajSize + 1)))
        lcLn = ALLTRIM(STR(19 - (loFormSet.lnMajSize)))
*E303425,3 TMI 11/25/2013 10:26 [End  ]
        lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
          "SET &lcFldtmp = '"+PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnMajSize)+"' + SUBSTRING(&lcFldtmp,&lcFs,&lcLn) "+;
          "WHERE SUBSTRING(&lcFldtmp,1,"+STR(loFormSet.lnMajSize,2)+") = '"+PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnMajSize)+"'" +;
          "AND &lcFltExp"
        lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
        =lfSetstep(lnSQLRunResult)
      ENDIF
      WAIT CLEAR
    ENDIF
  ENDSCAN
  SELECT CODES
  SET ORDER TO (lcOrd)
  =lfOpClFile(loFormSet,'C')
ENDCASE

IF llopSGVL AND USED('ICSEGVAL')
  gfCloseTable('ICSEGVAL')
ENDIF
IF llopsty AND USED('STYLE')
  gfCloseTable('STYLE')
ENDIF
IF llopscl AND USED('SCALE')
  gfCloseTable('SCALE')
ENDIF

IF !llVld
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Special function of the non major key for Old , New Values
*!             and replacing process
*!*************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!*************************************************************
*! Calls       : FUNCTIONS  : gfOpenTable(), gfStyBrw(), lfOpClFile(loFormSet,)
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : lcStatus
*!                     lcStatus = "O"  Validation of old value object
*!                     lcStatus = "N"  Validation of New value object
*!                     lcStatus = "R"  Replacing process
*!*************************************************************
FUNCTION lfNonMaj
PARAMETERS lcStatus



PRIVATE lcSrchCod, lcCod, lnLen, lcOrd, llopSGVL, llopsty, llopscl
STORE .F. TO llopSGVL, llopsty, llopscl
llVld = .T.
STORE SPACE(0) to lcSrchCod, lcCod, lcOrd
IF FILE(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICISTRU.DBF')
  llICISTRU = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICISTRU','Segno','SH')
  SELECT ICISTRU
  IF !SEEK('U1')
    IF llICISTRU
      =gfCloseTable('ICISTRU')
    ENDIF
    RETURN .F.
  ENDIF
  IF llICISTRU
    =gfCloseTable('ICISTRU')
  ENDIF
ELSE
  RETURN .F.
ENDIF

llopsty = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'STYLE','CSTYLE','SH')
llopscl = gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'SCALE','SCALE','SH')
llopSGVL =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICSEGVAL','SEGVAL','SH')

DO CASE
****************************************************
*-- Validation of old or new values
CASE lcStatus $ 'ON'
  lnLen = 1
  SELECT CODES
  lcOrd = ORDER()
  SET ORDER TO TAG cCode_No
  FOR lnC = loFormSet.lnMajSeg + 1 TO ALEN(loFormSet.laMajSeg,1)
    lcCod = SUBSTR(IIF(lcStatus = 'O',loFormSet.lcOldVal,loFormSet.lcNewVal) ,lnLen,LEN(loFormSet.laMajSeg[lnC,3]) )
    lnLen = lnLen + LEN(loFormSet.laMajSeg[lnC,3])
*!   SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size

    DO CASE
    CASE loFormSet.laMajSeg[lnC,1] $ 'OTQ'
      llopSGVL =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICSEGVAL','SEGVAL','SH')
      IF !SEEK(STR(lnC,1)+lcCod,'ICSEGVAL')
        llVld  = .F.
      ENDIF
    CASE loFormSet.laMajSeg[lnC,1] $ 'ZCDG'
      lcSrchCod = IIF(loFormSet.laMajSeg[lnC,1]='Z','SEASON', ;
        IIF(loFormSet.laMajSeg[lnC,1]='C','COLOR', ;
        IIF(loFormSet.laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
      lcSrchCod = PADR(lcSrchCod,10)
      IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
        llVld  = .F.
      ENDIF
    CASE loFormSet.laMajSeg[lnC,1] = 'S'
      IF !SEEK('S'+lcCod,'SCALE' )
        llVld  = .F.
      ENDIF
    ENDCASE

    IF !llVld
      IF ('?' $ loFormSet.lcNewVal)
        =gfModalGen("TRM00385B00000","DIALOG")
        EXIT
      ENDIF
*-- 'Invalid non Major code segments. Cannot add to the file.'
      =gfModalGen("INM00334B00000","DIALOG","non Major")
      loFormSet.lcNewVal = SPACE(LEN(loFormSet.lcNewVal))

*E303425,3 TMI 11/26/2013 23:02 [Start]
*        EXIT
      RETURN .F.
*E303425,3 TMI 11/26/2013 23:02 [End  ]

    ENDIF
  ENDFOR
  SELECT CODES
  SET ORDER TO (lcOrd)
**********************************************************************
*-- Replacing process
CASE lcStatus = 'R'

  IF FILE(oAriaApplication.WorkDir+loFormSet.lcTmpStyle+'.DBF') AND !USED(loFormSet.lcTmpStyle)
    USE (oAriaApplication.WorkDir+loFormSet.lcTmpStyle) IN 0 order TAG (loFormSet.lcTmpStyle)
  ENDIF

  IF !USED(loFormSet.lcTmpStyle)
    IF TYPE('lnCnt') = "N"
      PRIVATE lnOldCount
      lnOldCount = lnCnt
    ENDIF
    =lpSelecSty(loFormSet)
    IF TYPE('lnCnt') = "N"
      lnCnt = lnOldCount
    ENDIF
  ENDIF

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]
  IF FILE(oAriaApplication.WorkDir+loFormSet.lcTmpStyle+'.DBF') AND !USED(loFormSet.lcTmpStyle)
    USE (oAriaApplication.WorkDir+loFormSet.lcTmpStyle) IN 0 order TAG (loFormSet.lcTmpStyle)
  ENDIF
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]
  IF EMPTY(loFormSet.lcTmpStyle) OR (USED(loFormSet.lcTmpStyle) AND RECCOUNT(loFormSet.lcTmpStyle)=0)
*-- Message < No style(s) selected.>
*-- Buttons <           OK         >
    =gfModalGen("TRM32038B00000","DIALOG",loFormSet.lcStyMajor+"(s)")
    loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Value = .F.

    IF llopSGVL
      gfCloseTable('ICSEGVAL')
    ENDIF
    IF llopsty
      gfCloseTable('STYLE')
    ENDIF
    IF llopscl
      gfCloseTable('SCALE')
    ENDIF
    RETURN .F.
  ENDIF
  PRIVATE lcStyleTag , lcTmpExist
  lcTmpExist = gfTempName()
  CREATE CURSOR (lcTmpExist) (Style C(19))
  INDEX ON Style TAG (lcTmpExist) OF (oAriaApplication.WorkDir+lcTmpExist+'.CDX')
  IF !loFormSet.llColorExt

    loFormSet.lnMajSegmt  = gfItemMask('SM')  && No. of major segments.
    DIMENSION laMajSeg[1,1]
    = gfItemMask(@laMajSeg)
    DIMENSION loFormSet.laMajSeg[ALEN(laMajSeg,1),ALEN(laMajSeg,2)]
    ACOPY(laMajSeg,loFormSet.laMajSeg)

*-- Loop Around Non Major elements.
    FOR lnI = loFormSet.lnMajSegmt + 1 TO ALEN(loFormSet.laMajSeg,1)
*-- If you Find Color Type
      IF loFormSet.laMajSeg[lnI,1] = 'C'
        loFormSet.llColorExt = .T.
        loFormSet.lnClrPo    = loFormSet.laMajSeg[lnI,4]
        loFormSet.lnColorLen = loFormSet.laMajSeg[lnI,3]
        EXIT
      ENDI
    ENDFOR
  ENDIF

*-- Validate the nonMajor before update
  lnLen = 1
  SELECT CODES
  lcOrd = ORDER()
  SET ORDER TO TAG cCode_No
  FOR lnC = loFormSet.lnMajSeg + 1 TO ALEN(loFormSet.laMajSeg,1)
    lcCod = SUBSTR( KEY_CHG.New_Key ,lnLen,LEN(loFormSet.laMajSeg[lnC,3]) )
    lnLen = lnLen + LEN(loFormSet.laMajSeg[lnC,3])
*!   SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size

    DO CASE
    CASE loFormSet.laMajSeg[lnC,1] $ 'OTQ'
      llopSGVL =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'ICSEGVAL','SEGVAL','SH')
      IF !SEEK(STR(lnC,1)+lcCod,'ICSEGVAL')
        llVld  = .F.
      ENDIF
    CASE loFormSet.laMajSeg[lnC,1] $ 'ZCDG'
      lcSrchCod = IIF(loFormSet.laMajSeg[lnC,1]='Z','SEASON', ;
        IIF(loFormSet.laMajSeg[lnC,1]='C','COLOR', ;
        IIF(loFormSet.laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
      lcSrchCod = PADR(lcSrchCod,10)
      IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
        llVld  = .F.
      ENDIF

    CASE loFormSet.laMajSeg[lnC,1] = 'S'
      =gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'SCALE','SCALE','SH')
      IF !SEEK('S'+lcCod,'SCALE' )
        llVld  = .F.
      ENDIF
    ENDCASE
    IF !llVld
      RETURN .F.
    ENDIF
  ENDFOR

  SELECT SYDFDCHG
  =SEEK('K'+'STYLE')
  SCAN WHILE cKeyType+CFld_name = 'K'+'STYLE'
*-- If main field record , Get all files that has that field then replace old value by new value
    IF EMPTY(SYDFDCHG.cAltField)

      USE (oAriaApplication.DataDir+'Style') SHARE AGAIN IN 0 ALIAS StyAgain ORDER TAG Style

      SELECT SYDFLFLD
      =SEEK('STYLE')
      SCAN WHILE CFld_name = 'STYLE'
*-- Open necessary file.
        IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFLFLD.cFile_nam),'')
          LOOP
        ENDIF
        WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_nam) + ' file.' NOWAIT
        IF SYDFLFLD.cFile_nam = 'STYLE'
* nothing
        ELSE
*-- Replace Old value by new value
          lcStyleTag = ORDER('Style')
          lcTmpStyle = loFormSet.lcTmpStyle
*xUx*
          SET ORDER TO TAG Style IN Style
          IF SYDFILES.CVER = 'A27'
            REPLACE ALL Style WITH SUBSTR(Style,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key);
              FOR IIF(SEEK(Style,loFormSet.lcTmpStyle),;
              IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1)+ALLTRIM(KEY_CHG.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
              ALLTRIM(SUBSTR(Style,loFormSet.lnMajSize+2)) == ALLTRIM(KEY_CHG.Old_Key)
          ELSE
*xx*lcSQL =
            lcTag = lfGetTag(SYDFILES.cFile_nam)
            SCAN FOR IIF(SEEK(Style,loFormSet.lcTmpStyle),;
                IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1)+ALLTRIM(KEY_CHG.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
                ALLTRIM(SUBSTR(Style,loFormSet.lnMajSize+2)) == ALLTRIM(KEY_CHG.Old_Key)
              gfReplace([Style WITH SUBSTR(Style,1,loFormSet.lnMajSize+1) + ALLTRIM(Key_Chg.New_Key)])
            ENDSCAN
            =gfTableUpdate(.T.)
            =gfCloseTable(ALLTRIM(SYDFILES.cFile_nam))
          ENDIF
          SET ORDER TO TAG (lcStyleTag) IN Style

        ENDIF
        WAIT CLEAR
      ENDSCAN

    ELSE   && i.e. !EMPTY(SYDFDCHG.cAltField)
*-- Open necessary files
      IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag))
        LOOP
      ENDIF
      WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_nam) + ' file.' NOWAIT

*-- Replace Old value by new value
      lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)

      lcStyleTag = ORDER('Style')
      lcTmpStyle = loFormSet.lcTmpStyle
      SET ORDER TO TAG Style IN Style
      IF ALLTRIM(SYDFDCHG.cFile_nam) = "BOM" AND lcFldtmp = "CITMMASK"
*xUx*  convert this to just a sql statement
        =SEEK(PADR("BOM",oAriaApplication.FileW),'SYDFILES')
        lcTag = lfGetTag("BOM")

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]

*!*            SCAN FOR IIF(SEEK(SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize+1),loFormSet.lcTmpStyle) AND SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1),'Style'),;
*!*                           IIF(SUBSTR(&lcFldtmp,loFormSet.lnMajSize+2) = '******' AND EVALUATE(SydFdChg.cFile_Nam+'.IClr') = '******',lfAddFab(),.T.),.F.) AND ;
*!*                   IIF(SEEK(&lcFldtmp,loFormSet.lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'Style'),;
*!*                      lfStyExist(&lcFldtmp),.T.),.F.) AND ;
*!*                   ALLTRIM(SUBSTR(&lcFldtmp,loFormSet.lnMajSize+2)) == ALLTRIM(Key_Chg.Old_Key)
*!*              gfReplace([&lcFldtmp WITH SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key)])

        SCAN FOR IIF(SEEK(SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize+1),loFormSet.lcTmpStyle) AND SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1),'Style'),;
            IIF(SUBSTR(&lcFldtmp,loFormSet.lnMajSize+2) = '******' AND '******' $ EVALUATE(SYDFDCHG.cFile_nam+'.Item'),lfAddFab(loFormSet),.T.),.F.) AND ;
            IIF(SEEK(&lcFldtmp,loFormSet.lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key),'Style'),;
            lfStyExist(&lcFldtmp),.T.),.F.) AND ;
            ALLTRIM(SUBSTR(&lcFldtmp,loFormSet.lnMajSize+2)) == ALLTRIM(KEY_CHG.Old_Key)
          SCATTER MEMO memVA
          =gfDelete()
          APPEND BLANK
          m.&lcFldtmp = SUBSTR(m.&lcFldtmp,1,loFormSet.lnMajSize+1)  + ALLTRIM(KEY_CHG.New_Key)
          =gfAppend(ALLTRIM(SYDFILES.cFile_nam),.T.)

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]

        ENDSCAN
        =gfTableUpdate(.T.)
        =gfCloseTable("BOM")

      ELSE
        lcTmpStyle = loFormSet.lcTmpStyle
        lcFltExp = lfGetExp(loFormSet)
*xUx*
        IF SYDFILES.CVER = 'A27'
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
*!*              REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key) ;
*!*                     FOR IIF(SEEK(&lcFldtmp,loFormSet.lcTmpStyle),;
*!*                             IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
*!*                     ALLTRIM(SUBSTR(&lcFldtmp,loFormSet.lnMajSize+2)) == ALLTRIM(Key_Chg.Old_Key)
*!*                     AND &lcFltExp
          REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize+1)  + ALLTRIM(KEY_CHG.New_Key) ;
            FOR IIF(SEEK(&lcFldtmp,loFormSet.lcTmpStyle),;
            IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
            ALLTRIM(SUBSTR(&lcFldtmp,loFormSet.lnMajSize+2)) == ALLTRIM(KEY_CHG.Old_Key) ;
            AND &lcFltExp
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
        ELSE
*xx*lcSql =
          lcTag = lfGetTag(SYDFILES.cFile_nam)
          SCAN FOR IIF(SEEK(&lcFldtmp,loFormSet.lcTmpStyle),;
              IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
              ALLTRIM(SUBSTR(&lcFldtmp,loFormSet.lnMajSize+2)) == ALLTRIM(KEY_CHG.Old_Key) AND &lcFltExp


*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]

* gfReplace([&lcFldtmp WITH SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key)])
            SCATTER MEMO memVA
            =gfDelete()
            APPEND BLANK
            m.&lcFldtmp = SUBSTR(m.&lcFldtmp,1,loFormSet.lnMajSize+1)  + ALLTRIM(KEY_CHG.New_Key)
            =gfAppend(ALLTRIM(SYDFILES.cFile_nam),.T.)

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]


          ENDSCAN
          =gfTableUpdate(.T.)
          =gfCloseTable(ALLTRIM(SYDFILES.cFile_nam))
        ENDIF

      ENDIF
      WAIT CLEAR
      SET ORDER TO TAG (lcStyleTag) IN Style

    ENDIF
  ENDSCAN


  SELECT STYLE
  SET ORDER TO

  lcStyleTag = ORDER('Style')
  SET ORDER TO TAG Style IN Style

  lcTmpStyle = loFormSet.lcTmpStyle
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
  SELECT (lcTmpStyle)
  LOCATE
  SCAN
    SELECT STYLE
    =SEEK(&lcTmpStyle..Style)
    SCAN REST WHILE STYLE = &lcTmpStyle..Style FOR ;
        SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key),'StyAgain')
      =lfStyExist(StyAgain.Style)
      DELETE
    ENDSCAN
  ENDSCAN
  SELECT style
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
  REPLACE ALL Style WITH SUBSTR(Style,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key) ;
    FOR IIF(SEEK(Style,loFormSet.lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
    ALLTRIM(SUBSTR(Style, loFormSet.lnMajSize+2)) == ALLTRIM(KEY_CHG.Old_Key)
  SET ORDER TO TAG (lcStyleTag) IN Style

  SET ORDER TO CSTYLE
  SCAN FOR SUBSTR(STYLE, loFormSet.lnMajSize+2 ) = ALLTRIM(KEY_CHG.New_Key)
    lnLen = 1
    FOR lnC = loFormSet.lnMajSeg + 1 TO ALEN(loFormSet.laMajSeg,1)
      lcCod = SUBSTR(KEY_CHG.New_Key,lnLen,LEN(loFormSet.laMajSeg[lnC,3]) )
      lnLen = lnLen + LEN(loFormSet.laMajSeg[lnC,3])
*!   SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size
      DO CASE
      CASE loFormSet.laMajSeg[lnC,1] = 'Z'
        REPLACE Season WITH lcCod
      CASE loFormSet.laMajSeg[lnC,1] = 'D'
        REPLACE CDivision WITH lcCod
      CASE loFormSet.laMajSeg[lnC,1] = 'G'
        REPLACE cstygroup WITH lcCod
      CASE loFormSet.laMajSeg[lnC,1] = 'S'
        REPLACE SCALE WITH lcCod
      ENDCASE
    ENDFOR
  ENDSCAN

  IF USED('StyAgain')
    USE IN StyAgain
  ENDIF

  SELECT CODES
  SET ORDER TO (lcOrd)
  =lfOpClFile(loFormSet,'C')

ENDCASE
IF llopSGVL AND USED('ICSEGVAL')
  gfCloseTable('ICSEGVAL')
ENDIF
IF llopsty AND USED('STYLE')
  gfCloseTable('STYLE')
ENDIF
IF llopscl AND USED('SCALE')
  gfCloseTable('SCALE')
ENDIF
*- End of lfNonMaj.
************************************************************
*! Name      : lfGetTag
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/08/2013
*! Purpose   : Get the tag name
************************************************************
FUNCTION lfGetTag
PARAMETERS lcFile

*- open the temp file that contrains the selected styles
IF FILE(oAriaApplication.WorkDir+loFormSet.lcTmpStyle+'.DBF') AND !USED(loFormSet.lcTmpStyle)
  USE (oAriaApplication.WorkDir+loFormSet.lcTmpStyle) IN 0 order TAG (loFormSet.lcTmpStyle)
ENDIF

*- get the tag of the sql table to seek with
LOCAL lcTag
lcTag = ALLTRIM(SYDFILES.cFile_Tag)
IF EMPTY(lcTag)
  =SEEK(SYDFILES.cFile_nam,'SYDINDEX')
  lcTag = SYDINDEX.cFile_Tag
  SELECT SYDINDEX
  LOCATE REST WHILE cFile_nam = SYDFILES.cFile_nam AND LUNIQUE
  IF FOUND()
    lcTag = SYDINDEX.cFile_Tag
  ENDIF
ENDIF

SELECT 0
=gfOpenTable(ALLTRIM(SYDFILES.cFile_nam),lcTag,'SH')
SELECT (ALLTRIM(SYDFILES.cFile_nam))
gfSEEK('')
LOCATE
*- End of lfGetTag.

*!*************************************************************
*! Name      : lfBldTmp
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To build the temporary files
*!*************************************************************
*! Called from : lfCrtFiles(), lfvPrcd(),
*!*************************************************************
FUNCTION lfBldTmp
PARAMETERS loFormSet

PRIVATE lnAlias
lnAlias = SELECT(0)

IF USED(loFormSet.lcNValues)
  USE IN (loFormSet.lcNValues)
ENDIF

SELECT (loFormSet.lcValues)
GO TOP
IF !EOF()
  ZAP
ENDIF

SELECT (loFormSet.lcHistory)
GO TOP
IF !EOF()
  ZAP
ENDIF

SELECT KEY_CHG
SET RELATION TO KEY_CHG.cKeyType+ KEY_CHG.cKey_Chg INTO SYDFDVLD ADDITIVE
=SEEK(loFormSet.lcKeyType)

SCAN WHILE cKeyType = loFormSet.lcKeyType
  IF ASCAN(loFormSet.laFields,ALLTRIM(cKey_Chg)) = 0
    LOOP
  ENDIF
  llKey = loFormSet.laFields[ASUBSCRIPT(loFormSet.laFields,ASCAN(loFormSet.laFields,ALLTRIM(cKey_Chg)),1),6]
  IF KEYSTATUS = 'O'
    *E611964,1 Es 11/04/2019  Change size scales from one size scale to another [Start]
    *INSERT INTO (loFormSet.lcValues) (cKeyType,cKey_Chg, Old_Key, New_Key,cFld_Head, ENTERED,;
    cStatus, lKey, mSaveFilt ) ;
    VALUES (KEY_CHG.cKeyType,KEY_CHG.cKey_Chg, KEY_CHG.Old_Key,      ;
    KEY_CHG.New_Key, SYDFDVLD.cFld_Head, KEY_CHG.ENTERED,'S',llKey,;
    KEY_CHG.mSaveFilt)
    INSERT INTO (loFormSet.lcValues) (cKeyType,cKey_Chg, Old_Key, New_Key,cFld_Head, ENTERED,;
      cStatus, lKey, mSaveFilt,Note,MSIZES) ;
      VALUES (KEY_CHG.cKeyType,KEY_CHG.cKey_Chg, KEY_CHG.Old_Key,      ;
      KEY_CHG.New_Key, SYDFDVLD.cFld_Head, KEY_CHG.ENTERED,'S',llKey,;
      KEY_CHG.mSaveFilt,KEY_CHG.Note,KEY_CHG.MSIZES)
    *E611964,1 Es 10/21/2019  Change size scales from one size scale to another [End]
  ELSE
    *E611964,1 Es 11/04/2019  Change size scales from one size scale to another [Start]
    *!*      INSERT INTO (loFormSet.lcHistory) (cKeyType,cKey_Chg, Old_Key, New_Key,  ;
    *!*        cFld_Head,ENTERED, Date_Procd, lKey)  ;
    *!*        VALUES (KEY_CHG.cKeyType,KEY_CHG.cKey_Chg, KEY_CHG.Old_Key,  ;
    *!*        KEY_CHG.New_Key,SYDFDVLD.cFld_Head, KEY_CHG.ENTERED, ;
    *!*        KEY_CHG.Date_Procd, llKey)
    INSERT INTO (loFormSet.lcHistory) (cKeyType,cKey_Chg, Old_Key, New_Key,  ;
      cFld_Head,ENTERED, Date_Procd, lKey,Note)  ;
      VALUES (KEY_CHG.cKeyType,KEY_CHG.cKey_Chg, KEY_CHG.Old_Key,  ;
      KEY_CHG.New_Key,SYDFDVLD.cFld_Head, KEY_CHG.ENTERED, ;
      KEY_CHG.Date_Procd, llKey,KEY_CHG.Note)
    *E611964,1 Es 11/04/2019  Change size scales from one size scale to another [End]
  ENDIF
ENDSCAN
FLUSH
USE (oAriaApplication.WorkDir+loFormSet.lcValues) AGAIN ALIAS (loFormSet.lcNValues) ORDER TAG (loFormSet.lcValues) IN 0

SET RELATION TO
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfvScale
*! Developer : Sameh Saiid Ezzat
*! Date      : 04/04/2000
*! Purpose   : Special function of the Scale key for Old , New Values
*!             and replacing process
*!**************************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!**************************************************************************
*! Calls       : FUNCTIONS  : gfOpenTable(), gfStyBrw(), lfOpClFile(loFormSet,)
*!**************************************************************************
*! Passed Parameters : lcStatus = "O"  Validation of old value object
*!                     lcStatus = "N"  Validation of New value object
*!                     lcStatus = "R"  Replacing process
*!**************************************************************************
*! Notes       : Name of this function must be in mSpcFunc field
*!               in the SYDFDVLD file.
*!**************************************************************************
FUNCTION lfvScale
PARAMETERS lcStatus
PRIVATE llScaleHD , lnNumber
lnNumber = 0
*-- open scalehd if company is ext. size scale
llScaleHD = IIF(loFormSet.llMScale,gfOpenTable(ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])+'SCALEHD','EXTSCALE','SH'),.F.)

DO CASE
*-- Validation of the Old Value
CASE lcStatus = 'O'
  *E611964,1 Es 10/28/2019 Change size scales from one size scale to another [Start]
  *IF !SEEK('S'+loFormSet.lcOldVal,'SCALE')
  IF !SEEK('S'+PADR(ALLTRIM(loFormSet.lcOldVal),3),'SCALE')
  *E611964,1 Es 10/28/2019 Change size scales from one size scale to another [End]

    lcOldVal = loFormSet.lcOldVal
    = lfScalBrow(@lcOldVal)
    loFormSet.lcOldVal = lcOldVal
  ENDIF

 *E611964,1 Es 10/23/2019 Change size scales from one size scale to another [Start]
CASE lcStatus = 'N' AND ALLTRIM(loFormSet.laFields[loFormSet.lnFields,2]) = 'SZ1'
  IF !SEEK('S'+PADR(ALLTRIM(loFormSet.lcNewVal),3),'SCALE') OR (SEEK('S'+PADR(ALLTRIM(loFormSet.lcNewVal),3),'SCALE') AND loFormSet.lcNewVal = loFormSet.lcOldVal)
    lcNewVal= loFormSet.lcNewVal
    = lfScalBrow(@lcNewVal, "Scale <> '"+loFormSet.lcOldVal+"'")
    loFormSet.lcNewVal= lcNewVal
  ENDIF
  lcNewScaleCnt = Scale.cnt
  =SEEK('S'+PADR(ALLTRIM(loFormSet.lcOldVal),3),'SCALE')
  IF lcNewScaleCnt < Scale.cnt
    =gfModalGen("INM44009B00000",'ALERT',ALLTRIM(STR(lcNewScaleCnt))+"|"+loFormSet.lcNewVal+"|"+ALLTRIM(STR(Scale.cnt))+"|"+loFormSet.lcOldVal)
    loFormSet.lcNewVal = ""
    RETURN .F.
  ENDIF
*E611964,1 Es 10/23/2019 Change size scales from one size scale to another [End]

*-- Validations of the new value
CASE .F. AND lcStatus = 'N'

  IF !SEEK('S'+PADR(ALLTRIM(loFormSet.lcNewVal),3),'SCALE')
    loFormSet.llSclExist = .F.

*-- 'Scale ' + ALLTRIM(loFormSet.lcNewVal) + 'does not exist in '
*--         the scale file Continue anyway?           '
*--              < YES >       < NO >                 '

    IF gfModalGen("QRM00274B00006","DIALOG",'Scale ' + ;
        ALLTRIM(loFormSet.lcNewVal) + ' does not exist in the Scale ' + ;
        'file continue anyway ?') = 2
      loFormSet.lcNewVal = loFormSet.lcOld
      RETURN .F.
    ENDIF
  ELSE
    loFormSet.llSclExist = .T.
  ENDIF

*-- Replacing process
CASE lcStatus = 'R'

*-- For Loop Around the Major/Non Major Array [Begin]
*-- To Get Scale in Style file
  FOR lnC = loFormSet.lnMajSeg + 1 TO ALEN(loFormSet.laMajSeg,1)
*-- If Type is Scale
    IF loFormSet.laMajSeg[lnC,1] = 'S'
      lnNumber = lnC
      EXIT
    ENDIF
  ENDFOR
*-- End For Around the Major/Non Major Array [End]
*-- First change Scale in Non Major part in all files that have Style field
*-- If there is a Scale in Style Non Major [Begin]
  IF lnNumber > 0
    SELECT SYDFDCHG
    =SEEK('K'+'STYLE')
*-- Scan around the changed style Fields in all system [Begin]
    SCAN WHILE cKeyType + CFld_name = 'K' + 'STYLE'
*-- If main field record , Get all files that has
*-- that field then replace old value by new value [Begin]
      IF EMPTY(SYDFDCHG.cAltField)
*B610654,1 TMI 01/09/2014 20:47 [Start]
        lcFldtmp = 'STYLE'
*B610654,1 TMI 01/09/2014 20:47 [End  ]
        SELECT SYDFLFLD
        =SEEK('STYLE')
        SCAN WHILE CFld_name = 'STYLE'
*-- Open necessary file.
          IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFLFLD.cFile_nam),'')
            LOOP
          ENDIF
          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_nam) + ' file.' NOWAIT
*B610654,1 TMI 01/08/2014 16:54 [Start]
          IF SYDFILES.CVER = 'A27'
*B610654,1 TMI 01/08/2014 16:54 [End  ]

*B610654,1 TMI 01/08/2014 17:51 [Start] add the lcFltExp filter
*REPLACE ALL STYLE WITH SUBSTR(STYLE,1,loFormSet.laMajSeg[lnNumber,4]-1) + ;
ALLTRIM(Key_Chg.New_Key) + SUBSTR(STYLE,loFormSet.laMajSeg[lnNumber,4]+loFormSet.lnLenght) ;
FOR ALLTRIM(SUBSTR(STYLE,loFormSet.laMajSeg[lnNumber,4],loFormSet.lnLenght)) == ALLTRIM(Key_Chg.Old_Key)
            REPLACE ALL STYLE WITH SUBSTR(STYLE,1,loFormSet.laMajSeg[lnNumber,4]-1) + ;
              ALLTRIM(KEY_CHG.New_Key) + SUBSTR(STYLE,loFormSet.laMajSeg[lnNumber,4]+loFormSet.lnLenght) ;
              FOR ALLTRIM(SUBSTR(STYLE,loFormSet.laMajSeg[lnNumber,4],loFormSet.M_EXTWIDTH)) == ALLTRIM(KEY_CHG.Old_Key)
*B610654,1 TMI 01/08/2014 17:52 [End  ]

*B610654,1 TMI 01/08/2014 16:53 [Start]                     include the ELSE part
          ELSE
            lcMj = ALLTRIM(STR(loFormSet.laMajSeg[lnNumber,4]))
            lcLn = ALLTRIM(STR(loFormSet.M_EXTWIDTH))
            lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
              "SET &lcFldtmp = SUBSTRING(&lcFldtmp,1,&lcMj) +'" + ALLTRIM(KEY_CHG.New_Key) + "' +" +;
              "SUBSTRING(&lcFldtmp,&lcMj+&lcLn,3-&lcLn) "+ ;
              "WHERE RTRIM(LTRIM(SUBSTRING(&lcFldtmp,&lcMj,&lcLn))) = '"+ALLTRIM(KEY_CHG.Old_Key) +"' "
            lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
            =lfSetstep(lnSQLRunResult)
          ENDIF
*B610654,1 TMI 01/08/2014 16:53 [End  ]                     x

          WAIT CLEAR
        ENDSCAN
      ELSE   && Else not empty of Alternative Field

*B610654,1 TMI 01/09/2014 11:02 [Start] exclude some fields
        ln = LEN(SYDFDCHG.cAltField)
        IF SYDFDCHG.cAltField $ PADR('CSTYMAJOR',ln)+'|'+;
            PADR('CITMMAJOR',ln)+'|'
          LOOP
        ENDIF
        IF SYDFDCHG.cFile_nam = 'ITEM' OR SYDFDCHG.cFile_nam = 'PWBUNDL'
          LOOP
        ENDIF
*B610654,1 TMI 01/09/2014 11:02 [End  ]

*-- If has alternative field change
*-- Open necessary files
        IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag) )
          LOOP
        ENDIF
        WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_nam) + ' file.' NOWAIT

*-- Replace Old value by new value
        lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)

        lcFltExp = lfGetExp(loFormSet)

*B610654,1 TMI 01/08/2014 18:00 [Start]
        lcMj = ALLTRIM(STR(loFormSet.laMajSeg[lnNumber,4]))
        lcLn = ALLTRIM(STR(loFormSet.M_EXTWIDTH))
*B610654,1 TMI 01/08/2014 18:00 [End  ]
        IF SYDFILES.CVER = 'A27'
*B610654,1 TMI 01/08/2014 18:06 [Start]
*REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,loFormSet.laMajSeg[lnNumber,4]-1) + ;
ALLTRIM(Key_Chg.New_Key) + SUBSTR(&lcFldtmp,loFormSet.laMajSeg[lnNumber,4]+loFormSet.lnLenght) ;
FOR ALLTRIM(SUBSTR(&lcFldtmp,loFormSet.laMajSeg[lnNumber,4],loFormSet.lnLenght)) == ALLTRIM(Key_Chg.Old_Key) ;
AND &lcFltExp
          REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,loFormSet.laMajSeg[lnNumber,4]-1) + ;
            ALLTRIM(KEY_CHG.New_Key) + SUBSTR(&lcFldtmp,loFormSet.laMajSeg[lnNumber,4]+loFormSet.lnLenght) ;
            FOR ALLTRIM(SUBSTR(&lcFldtmp,loFormSet.laMajSeg[lnNumber,4],&lcLn)) == ALLTRIM(KEY_CHG.Old_Key) ;
            AND &lcFltExp
*B610654,1 TMI 01/08/2014 18:06 [End  ]
        ELSE
          lcMj = ALLTRIM(STR(loFormSet.laMajSeg[lnNumber,4]))
*B610654,1 TMI 01/08/2014 15:54 [Start] correct the expression
*lcLn = ALLTRIM(STR(loFormSet.lnLenght))
*lcFs = ALLTRIM(STR(FSIZE(lcFltExp,ALLTRIM(sydfiles.cFILE_NAM))))
*lcSql = "UPDATE "+ALLTRIM(SYDFILES.CFILE_NAM)+" "+;
"SET &lcFldtmp = SUBSTRING(&lcFldtmp,1,&lcMj-1) +'" + ALLTRIM(Key_Chg.New_Key) + "' +" ;
"SUBSTRING(&lcFldtmp,lcMj+lcLn,lcFs-(lcMj+lcLn)+1) "+ ;
"WHERE RTRIM(LTRIM(SUBSTRING(&lcFldtmp,lcMj,lcLn))) = '"+ALLTRIM(Key_Chg.Old_Key) +"' " + ;
"AND &lcFltExp "
          lcLn = ALLTRIM(STR(loFormSet.M_EXTWIDTH))
          lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
            "SET &lcFldtmp = SUBSTRING(&lcFldtmp,1,&lcMj-1) +'" + ALLTRIM(KEY_CHG.New_Key) + "' +" +;
            "SUBSTRING(&lcFldtmp,&lcMj+&lcLn,3-&lcLn) "+ ;
            "WHERE RTRIM(LTRIM(SUBSTRING(&lcFldtmp,&lcMj,&lcLn))) = '"+ALLTRIM(KEY_CHG.Old_Key) +"' " + ;
            "AND &lcFltExp "
*B610654,1 TMI 01/08/2014 15:54 [End  ]
          lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
          =lfSetstep(lnSQLRunResult)
        ENDIF

        WAIT CLEAR
      ENDIF
    ENDSCAN
*-- EndScan Around the changed style Fields in all system [End]
  ENDIF
*-- EndIf of Scale in Style Non Major [End]

  SELECT SYDFDCHG
  =SEEK('K'+'SCALE')

*-- Scan around the changed scale Fields in all system [Begin]
  SCAN WHILE cKeyType + CFld_name = 'K' + 'SCALE'
*-- If main field record , Get all files that has that field
*-- then replace old value by new value
    IF EMPTY(SYDFDCHG.cAltField)
*B610654,1 TMI 01/09/2014 20:56 [Start]
      lcFldtmp = 'SCALE'
*B610654,1 TMI 01/09/2014 20:56 [End  ]
      SELECT SYDFLFLD
      =SEEK('SCALE')
      SCAN WHILE CFld_name = 'SCALE'
*-- Open necessary file.
        IF  !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFLFLD.cFile_nam),'')
          LOOP
        ENDIF

        WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_nam) +' file.' NOWAIT

*-- Replace Old value by new value
        lcFldtmp = ALLTRIM(SYDFDCHG.CFld_name)
        IF SYDFLFLD.cFile_nam = 'SCALE' AND loFormSet.llSclExist
          LOOP
        ENDIF

        IF SYDFILES.CVER = 'A27'
*B610654,1 TMI 01/08/2014 18:16 [Start]
*REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
FOR ALLTRIM(&lcFldtmp) = ALLTRIM(Key_Chg.Old_Key)
          REPLACE ALL &lcFldtmp WITH ALLTRIM(KEY_CHG.New_Key)+SUBSTR(&lcFldtmp,loFormSet.M_EXTWIDTH+1) ;
            FOR ALLTRIM(&lcFldtmp) = ALLTRIM(KEY_CHG.Old_Key)
*B610654,1 TMI 01/08/2014 18:16 [End  ]
        ELSE
*B610654,1 TMI 01/08/2014 16:58 [Start]
*lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_Nam)+" "+;
"SET &lcFldtmp = '"+Key_Chg.New_Key +"' "+;
"WHERE LTRIM(RTRIM(&lcFldtmp)) = '"+ALLTRIM(Key_Chg.Old_Key)+"'"
          lcLn = ALLTRIM(STR(loFormSet.M_EXTWIDTH))
          lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
            "SET &lcFldtmp = '"+ALLTRIM(KEY_CHG.New_Key) +"'+SUBSTRING(&lcFldtmp,&lcLn+1,3-&lcLn) "+;
            "WHERE LTRIM(RTRIM(SUBSTRING(&lcFldtmp,1,&lcLn))) = '"+ALLTRIM(KEY_CHG.Old_Key)+"'"
*B610654,1 TMI 01/08/2014 16:58 [End  ]
          lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
          =lfSetstep(lnSQLRunResult)
        ENDIF

        WAIT CLEAR
      ENDSCAN
    ELSE   && Else not empty of Alternative Field
*-- If has alternative field change
*-- Open necessary files
      IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag) )
        LOOP
      ENDIF

      WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_nam) + ' file.' NOWAIT

*-- Replace Old value by new value
      lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
      lcFltExp = lfGetExp(loFormSet)
*xUx*
      IF SYDFILES.CVER = 'A27'
*B610654,1 TMI 01/08/2014 18:22 [Start]
*REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
FOR ALLTrIM(&lcFldtmp) = ALLTRIM(Key_Chg.Old_Key) ;
AND &lcFltExp
        REPLACE ALL &lcFldtmp WITH ALLTRIM(KEY_CHG.New_Key)+SUBSTR(&lcFldtmp,loFormSet.M_EXTWIDTH+1) ;
          FOR ALLTrIM(&lcFldtmp) = ALLTRIM(KEY_CHG.Old_Key) ;
          AND &lcFltExp
*B610654,1 TMI 01/08/2014 18:23 [End  ]
      ELSE
*B610654,1 TMI 01/08/2014 17:06 [Start] correct the expression
*lcSql = "UPDATE "+ ALLTRIM(SYDFILES.CFILE_NAM) + " " +;
"SET &lcFldTmp = '"+Key_Chg.New_Key+"' "+;
"WHERE LTRIM(RTRIM(&lcFldtmp)) = '"+ALLTRIM(Key_Chg.Old_Key) +"' "+ ;
"AND &lcFltExp "
        lcLn = ALLTRIM(STR(loFormSet.M_EXTWIDTH))
        lcSQL = "UPDATE "+ ALLTRIM(SYDFILES.cFile_nam) + " " +;
          "SET &lcFldTmp = '"+ALLTRIM(KEY_CHG.New_Key)+"'+SUBSTRING(&lcFldtmp,&lcLn+1,3-&lcLn) "+;
          "WHERE LTRIM(RTRIM(SUBSTRING(&lcFldtmp,1,&lcLn))) = '"+ALLTRIM(KEY_CHG.Old_Key)+"' "+;
          "AND &lcFltExp "
*B610654,1 TMI 01/08/2014 17:06 [End  ]
        lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
        =lfSetstep(lnSQLRunResult)
      ENDIF
      WAIT CLEAR
    ENDIF
  ENDSCAN
*-- Scan around the changed scale Fields in all system [End]

  IF llScaleHD
    SELECT ScaleHD
    IF SEEK(ALLTRIM(SUBSTR(KEY_CHG.Old_Key,1,gfGetMemVar('M_EXTWIDTH',loFormSet.laComp[loFormSet.lnComp,2]))))
      REPLACE cExtScale WITH ALLTRIM(SUBSTR(KEY_CHG.Old_Key,1,gfGetMemVar('M_EXTWIDTH',loFormSet.laComp[loFormSet.lnComp,2])))
    ENDIF
  ENDIF
ENDCASE

IF llScaleHD AND USED('SCALEHD')
  =gfCloseTable('SCALEHD')
ENDIF
*-- End of lfvScale.

*!**************************************************************************
*! Name      : lfScalBrow
*! Developer : Sameh Saiid Ezzat
*! Date      : 04/05/2000
*! Purpose   : Function to validate entered scale size value.
*!**************************************************************************
*! Calls     : ARIABROW()
*!**************************************************************************
*! Passed Parameters  :  Entered Scale
*!                       Multi scale
*!**************************************************************************
*! Returns            :  .T. --> Valid scale
*!                       .F. --> Invalid scale
*!**************************************************************************
FUNCTION lfScalBrow
*E611964,1 Es 10/23/2019 Change size scales from one size scale to another [Start]
*PARAMETERS lcParaScale
PARAMETERS lcParaScale,lcForCond
*E611964,1 Es 10/23/2019 Change size scales from one size scale to another [End]
PRIVATE lcBrFields

lnOldAlias = SELECT()
lcFile_ttl = 'Scales'

SELECT SCALE
*E611964,1 MMT 12/22/2019  Change size scales from one size scale to another [P20190901.0001][Start]
*IF loFormSet.llBrowse OR EMPTY(lcParaScale) OR !SEEK('S'+lcParaScale)
IF loFormSet.llBrowse OR EMPTY(lcParaScale) OR '?' $ lcParaScale OR !SEEK('S'+lcParaScale)
*E611964,1 MMT 12/22/2019  Change size scales from one size scale to another [P20190901.0001][End]
  DECLARE laValues[1]  && array to get values from browse
  lcBrFields = "Scale:4:H='Scale' ,"+;
    "cScl_DESC :14:H='Description',"+;
    "CNT  :H='Cnt'  ,"+;
    "SZ1  :8:H='Size 1',"+;
    "SZ2  :8:H='Size 2',"+;
    "SZ3  :8:H='Size 3',"+;
    "SZ4  :8:H='Size 4',"+;
    "SZ5  :8:H='Size 5',"+;
    "SZ6  :8:H='Size 6',"+;
    "SZ7  :8:H='Size 7',"+;
    "SZ8  :8:H='Size 8'"


  =ARIABROW(['S']+IIF(TYPE('lcForCond')='C' And !EMPTY(lcForCond)," FOR &lcForCond.",''),lcFile_ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')
  lcParaScale = IIF(EMPTY(laValues[1]),SPACE(1),laValues[1])
ENDIF

SELECT (lnOldAlias)
RETURN !EMPTY(lcParaScale)
*-- End of lfScalBrow.

*!**************************************************************************
*! Name      : lfGetVal
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/25/01
*! Purpose   : Check new values.
*!**************************************************************************
FUNCTION lfGetVal
PARAMETERS loBranchFormSet,lcVal
LOCAL loFormSet
loFormSet = loBranchFormSet.loFormSet

*IF ALLTRIM(lcVal) = "?"
IF ('?' $ lcVal)
  =gfModalGen("TRM00385B00000","DIALOG")
  llRet = .F.
  RETURN lcVal
ENDIF
*-- Go to the field record in the SYDFDVLD table to get the browse information
IF SEEK(loFormSet.lcKeyType+loFormSet.laFields[loFormSet.lnfields,2],'SYDFDVLD')
  DO CASE
  CASE "Customer" $ loFormSet.laFields[loFormSet.lnfields,1]
*--Check if the account already exists.
    IF SEEK(ALLTRIM(SYDFDVLD.mKeyExpr)+lcVal,ALLTRIM(SYDFDVLD.cMainFile))
      *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][Start]
*!*	      =gfModalGen("TRM00383B00000","DIALOG","Account "+ALLTRIM(lcVal))
*!*	      llRet = .F.
      llRet = .T.
      *E612708,1 MMT 03/31/2024 Allow Key change program to Merge 2 existing accounts data[T-ERP-20240318.0002][End]
    ELSE
      IF LEN(ALLTRIM(lcVal)) < 5 .AND. !('?' $ lcVal)
*-- Account code should not be less than 5 characters.
        = gfModalGen('TRM00384B00000','ALERT') = 2
        llRet = .F.
      ENDIF
    ENDIF

  CASE   "SHIPVIA" $ loFormSet.laFields[loFormSet.lnfields,2]
    lnOldAls =SELECT(0)
    lcFile = SYDFDVLD.cFile_nam
    SELECT &lcFile
    LOCATE FOR &lcFile..SHIPVIA = lcVal
    IF FOUND()
      =gfModalGen("TRM00383B00000","DIALOG",PROPER(ALLTRIM(loFormSet.laFields[loFormSet.lnfields,1]))+" "+ALLTRIM(lcVal))
      llRet = .F.
    ENDIF
    SELECT(lnOldAls)

  OTHERWISE

*--Check if the account already exists.
*B610613,3 TMI 12/15/2013 11:26 [Start]
*IF SEEK(ALLTRIM(SYDFDVLD.mKeyExpr)+lcVal,ALLTRIM(SYDFDvld.cMainFile))
    IF gfSEEK(ALLTRIM(SYDFDVLD.mKeyExpr)+lcVal,ALLTRIM(SYDFDVLD.cMainFile))
*B610613,3 TMI 12/15/2013 11:26 [End  ]
      =gfModalGen("TRM00383B00000","DIALOG",PROPER(ALLTRIM(loFormSet.laFields[loFormSet.lnfields,1]))+" "+ALLTRIM(lcVal))
      llRet = .F.
    ENDIF
  ENDCASE
ENDIF
RETURN lcVal
*- End of lfGetVal

*!**************************************************************************
*! Name      : lfvKeyVal
*! Developer : Rania Abdel Razik ElSayed
*! Date      : 12/24/2001
*! Purpose   : Valid function of primary key object
*!**************************************************************************
*! Called from       : SMKEYNAM.scx
*!**************************************************************************
FUNCTION lfvKeyVal
PARAMETERS loSMKEYNAM_FormSet,loFld
loFormSet = loSMKEYNAM_FormSet.loFormSet

IF !EMPTY(loFormSet.laFields[loFormSet.lnfields,3]) AND  !EMPTY(loFormSet.laFields[loFormSet.lnfields,7])
* Calling the sepcial function with a specific k according to the numbers of keys in SYDFDVLD.mFull_Exp
  lcSpecFn = STRTRAN(loFormSet.laFields[loFormSet.lnfields,3] , '()','("K')+ALLTRIM(STR(lnCnt))+'")'
  llRet    = &lcSpecFn
ENDIF
*- End of lfvKeyVal

************************************************************
*! Name      : lfSMKEYNAM_FormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/03/2013
*! Purpose   : init method for the screen SMKEYNAM
************************************************************
FUNCTION lfSMKEYNAM_FormInit
PARAMETERS loSMKEYNAM_FormSet
loFormSet = loSMKEYNAM_FormSet.loFormSet

WITH loSMKEYNAM_FormSet.AriaForm1
  .laFullExp.Value = loFormSet.laFullExp[lnCnt,2]
  .lcKeyVal.KeyTextbox.MaxLength = loFormSet.laFullExp[lnCnt,3]
  .lcKeyVal.KeyTextbox.ControlSource = 'Thisformset.loFormSet.lcKeyVal'
ENDWITH
*- End of lfSMKEYNAM_FormInit.


*!**************************************************************************
*! Name      : lfvCustm
*! Developer : Rania Abdel Razik ElSayed
*! Date      : 12/24/2001
*! Purpose   : Special function of the Customer Store key for Primary Key,Old,
*!             New Values and replacing process
*!**************************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!**************************************************************************
*! Calls       : FUNCTION  : lfOpClFile(loFormSet,)
*!**************************************************************************
*! Passed Parameters : lcStatus = "K1"  Validation of Primary key object
*!                     lcStatus = "O"   Validation of old value object
*!                     lcStatus = "N"   Validation of New value object
*!                     lcStatus = "R"   Replacing process
*!**************************************************************************
*! Notes       : Name of this function must be in mSpcFunc field
*!               in the SYDFDVLD file.
*!               IF we have more than one key in the FullExp field we have
*!               the validation for each field as the follwings:
*!                      File # 1  valdition will be  CASE lcStatus = 'K1'
*!                      File # 2  valdition will be  CASE lcStatus = 'K2'
*!**************************************************************************
FUNCTION lfvCustm
PARAMETERS lcStatus

Do Case
CASE lcStatus = 'K1'
*--Validation of Primary key object
  lcBrFields = "ACCOUNT    :R :H= 'Account Code'       ," +;
    "STNAME     :R :H= 'STNAME '            ," +;
    "PHONE1     :R :H= 'Phone  '            ," +;
    "BUYER      :R :H= 'BUYER  '            ," +;
    "SALESREP   :R :H= 'Sales Representive' ," +;
    "CADDRESS1  :R :H= 'CADDRESS1'          ," +;
    "CADDRESS2  :R :H= 'CADDRESS2'          ," +;
    "CADDRESS3  :R :H= 'CADDRESS3'          ," +;
    "CADDRESS4  :R :H= 'CADDRESS4'          ," +;
    "CADDRESS5  :R :H= 'CADDRESS5'          ," +;
    "CADDRESS12 :R :H= 'CADDRESS12'         ," +;
    "CADDRESS22 :R :H= 'CADDRESS22'         ," +;
    "CADDRESS32 :R :H= 'CADDRESS32'         ," +;
    "CADDRESS42 :R :H= 'CADDRESS42'         ," +;
    "CADDRESS52 :R :H= 'CADDRESS52'         ," +;
    "NETBAL     :R :H= 'NETBAL'             "

  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Select Value"
*-- Get the browse title
  IF SEEK(SYDFDVLD.cMainFile,'SYDFILES')
    lcFile_ttl = SYDFILES.cFile_ttl
  ENDIF

  SELECT CUSTOMER
  lcFilt = "FOR TYPE = 'M' .AND. STATUS # 'X' "
  IF loFormSet.llBrowse OR '?' $ loFormSet.lcKeyVal .OR. !SEEK('M'+loFormSet.lcKeyVal,'CUSTOMER')
    xAccount = loFormSet.lcKeyVal
    DO CUSBROWM WITH xAccount
    loFormSet.lcKeyVal = xAccount
*=gfBrows(lcFilt,'ACCOUNT',"laTemp")
*loFormSet.lcKeyVal = laTemp[1]
*loFormSet.lcKeyTemp = loFormSet.lcKeyVal

*-- loFormSet.lcKeyVal must contain a value
*=gfModalGen("TRM00386B00000","DIALOG",loFormSet.laFullExp[loFormSet.lnkeys,1])
  ENDIF
  loFormSet.lcKeyTemp = loFormSet.lcKeyVal

  IF EMPTY(loFormSet.lcKeyTemp)
    loFormSet.lcKeyTemp= loFormSet.lcOld
    llRet = .F.
  ENDIF
  loFormSet.llBrowse = .F.

CASE lcStatus = 'O'
*--Validation of old value object
  lcBrFields = "ACCOUNT    :R :H= 'Account Code'       ," +;
    "STORE     :R :H= 'Store Code'         ," +;
    "STNAME     :R :H= 'STNAME '            ," +;
    "PHONE1     :R :H= 'Phone  '            ," +;
    "BUYER      :R :H= 'BUYER  '            ," +;
    "SALESREP   :R :H= 'Sales Representive' ," +;
    "CADDRESS1  :R :H= 'CADDRESS1'          ," +;
    "CADDRESS2  :R :H= 'CADDRESS2'          ," +;
    "CADDRESS3  :R :H= 'CADDRESS3'          ," +;
    "CADDRESS4  :R :H= 'CADDRESS4'          ," +;
    "CADDRESS5  :R :H= 'CADDRESS5'          ," +;
    "CADDRESS12 :R :H= 'CADDRESS12'         ," +;
    "CADDRESS22 :R :H= 'CADDRESS22'         ," +;
    "CADDRESS32 :R :H= 'CADDRESS32'         ," +;
    "CADDRESS42 :R :H= 'CADDRESS42'         ," +;
    "CADDRESS52 :R :H= 'CADDRESS52'         ," +;
    "NETBAL     :R :H= 'NETBAL'             "

  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Select Value"
*-- Get the browse title
  IF SEEK(SYDFDVLD.cMainFile,'SYDFILES')
    lcFile_ttl = SYDFILES.cFile_ttl
  ENDIF
  SELECT CUSTOMER

  lcFilt = "FOR TYPE = 'S' .AND. STATUS # 'X'.AND. ACCOUNT=loFormSet.lcKeyTemp "
  xAccount = loFormSet.lcKeyTemp
  xStore   = PADR(loFormSet.lcOldVal,FSIZE('STORE','CUSTOMER'))
  IF !SEEK('S'+xAccount+xStore) AND !CUSBROWS(loFormSet.lcKeyTemp,.T.)
    STORE SPACE(8) TO xStore
  ENDIF
  loFormSet.lcOldVal = xStore
  IF loFormSet.lcOldVal == loFormSet.lcOld
    llRet = .T.
    RETURN
  ENDIF

  lnRecNo = RECNO(loFormSet.lcValues)
  IF SEEK(loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW)+loFormSet.lcKeyTemp+'-'+loFormSet.lcOldVal,loFormSet.lcValues) AND (ALLTRIM(loFormSet.lcOldVal)<>ALLTRIM(loFormSet.lcOld) OR EMPTY(loFormSet.lcOld))
*-- "This value has been selected before as an "old value". Cannot accept."
    =gfModalGen("INM00335B00000","DIALOG","old value")
    loFormSet.lcOldVal = ''
  ENDIF
  IF BETWEEN(lnRecNo,1,RECCOUNT(loFormSet.lcValues))
    GO lnRecNo IN (loFormSet.lcValues)
  ENDIF

  llRet = .T.
  IF EMPTY(loFormSet.lcOldVal)
    loFormSet.lcOldVal= loFormSet.lcOld
    llRet = .F.
  ELSE

    IF !loFormSet.llNew
      lnAlias = SELECT()
      SELECT (loFormSet.lcValues)
      REPLACE Old_Key    WITH loFormSet.lcKeyTemp+'-'+loFormSet.lcOldVal
      SELECT (lnAlias)
      llRet = .T.
    ENDIF
  ENDIF

CASE lcStatus = 'N'

*-- Validation of New value object
  SELECT (loFormSet.lcValues)
  IF loFormSet.lcNewVal == loFormSet.lcOld
    RETURN
  ENDIF
  IF ('?' $ loFormSet.lcNewVal)
    =gfModalGen("TRM00385B00000","DIALOG")
    llRet = .F.
    RETURN
  ELSE
    lnRecNo = RECNO(loFormSet.lcValues)
    LOCATE FOR cKeyType+cKey_Chg+Old_Key+New_Key = ;
      loFormSet.lcKeyType+PADR(loFormSet.laFields[loFormSet.lnfields,2],oAriaApplication.FieldW) AND New_Key = loFormSet.lcKeyTemp+'-'+loFormSet.lcNewVal
    IF FOUND()
*-- "This value has been selected before as a "new value". Cannot accept."
      =gfModalGen("INM00335B00000","DIALOG","new value")
      loFormSet.lcNewVal = loFormSet.lcOld
      llRet = .F.
      RETURN .F.
    ENDIF
    IF BETWEEN(lnRecNo,1,RECCOUNT(loFormSet.lcValues))
      GO lnRecNo IN (loFormSet.lcValues)
    ENDIF
    IF !EMPTY(loFormSet.lcOldVal).AND.!EMPTY(loFormSet.lcNewVal)
      IF loFormSet.llNew
        APPEND BLANK
        REPLACE cStatus   WITH 'A',;
          cFld_Head WITH  loFormSet.laFields[loFormSet.lnfields,1]
        loFormSet.llNew = .F.
      ELSE
        REPLACE cStatus WITH IIF(cStatus='S','M',cStatus)
      ENDIF
      REPLACE cKeyType   WITH loFormSet.lcKeyType,;
        Old_Key    WITH loFormSet.lcKeyTemp+'-'+loFormSet.lcOldVal,;
        New_Key    WITH loFormSet.lcKeyTemp+'-'+loFormSet.lcNewVal,;
        cKey_Chg   WITH loFormSet.laFields[loFormSet.lnfields,2],;
        ENTERED    WITH DATE()

      DIMENSION laFixFltr[ALEN(loFormSet.laFixFltr,1),ALEN(loFormSet.laFixFltr,2)]
      ACOPY(loFormSet.laFixFltr,laFixFltr)

      SAVE TO MEMO mSaveFilt ALL LIKE laFixFltr
*-- Variable to indicate that data has modified
      llcUpDate = .T.
    ENDIF
  ENDIF

CASE lcStatus = 'R'
*-- Replacing process
*-- loFormSet.lcPKey variable holds the Primary Key ( The same foe new and old value )
  loFormSet.lcPKey=ALLTRIM(SUBSTR(KEY_CHG.Old_Key,1,AT('-',KEY_CHG.Old_Key)-1))

*-- loFormSet.lcOldKey variable holds the old value of secondary key
  loFormSet.lcOldKey=ALLTRIM(SUBSTR(KEY_CHG.Old_Key,LEN(loFormSet.lcPKey)+2))

*-- loFormSet.lcNewKey variable holds the new value of secondary key
  loFormSet.lcNewKey=ALLTRIM(SUBSTR(KEY_CHG.New_Key,LEN(loFormSet.lcPKey)+2))

  IF SEEK(cKeyType+cKey_Chg,'SYDFDCHG')
    SELECT SYDFDCHG
    IF KEY_CHG.cKeyType = 'K'
      SCAN WHILE cKeyType+CFld_name = KEY_CHG.cKeyType + KEY_CHG.cKey_Chg
*-- If main field record , Get all files that has that field then replace old value by new value
        IF EMPTY(SYDFDCHG.cAltField)
          SELECT SYDFLFLD
          =SEEK(SYDFDCHG.CFld_name)
          SCAN WHILE CFld_name = SYDFDCHG.CFld_name
*-- Open necessary file.
            IF ALLTRIM(SYDFLFLD.cFile_nam) = 'ASN_SHIP'
              LOOP
            ENDIF
            lcTagFile = IIF(ALLTRIM(SYDFLFLD.cFile_nam) = 'INVHDR','INVHDR','')
            IF  !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFLFLD.cFile_nam),lcTagFile)
              LOOP
            ENDIF

            WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_nam) +' file.' NOWAIT
*-- Replace Old value by new value
            lcFldtmp=ALLTRIM(SYDFDCHG.CFld_name)
            IF SYDFLFLD.cFile_nam = 'PROFILE' OR SYDFLFLD.cFile_nam = 'CONTACT'
              REPLACE ALL &lcFldtmp WITH loFormSet.lcNewKey ;
                FOR ALLTRIM(&lcFldtmp) == ALLTRIM(loFormSet.lcOldKey) .AND. cCont_Id=loFormSet.lcPKey

            ELSE
              IF SYDFILES.CVER = 'A27'
                *B612447,1 MMT 08/12/2021 Key Change Corrupts store IDs if it starts with same characters[T20210806.0001][Start]
*!*	                REPLACE ALL &lcFldtmp WITH loFormSet.lcNewKey ;
*!*	                  FOR ALLTRIM(&lcFldtmp) = ALLTRIM(loFormSet.lcOldKey) .AND. ACCOUNT=loFormSet.lcPKey
                REPLACE ALL &lcFldtmp WITH loFormSet.lcNewKey ;
                  FOR ALLTRIM(&lcFldtmp) == ALLTRIM(loFormSet.lcOldKey) .AND. ACCOUNT=loFormSet.lcPKey
                *B612447,1 MMT 08/12/2021 Key Change Corrupts store IDs if it starts with same characters[T20210806.0001][End]                  
              ELSE
                lcSQL=  "UPDATE "+ALLTRIM(SYDFLFLD.cFile_nam)+" "+;
                  "SET &lcFldtmp = '"+loFormSet.lcNewKey+"' "+;
                  "WHERE "+ALLTRIM(lcFldtmp)+" = '"+PADR(ALLTRIM(loFormSet.lcOldKey),lfGetFSIZE(lcFldtmp)) +"' AND ACCOUNT= '"+loFormSet.lcPKey+"'"
                lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'', ;
                  '', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
                =lfSetstep(lnSQLRunResult )

              ENDIF
            ENDIF
            WAIT CLEAR
          ENDSCAN

        ELSE   && i.e. !EMPTY(SYDFDVLD.cAltField)
*-- Open necessary files
          IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag) )
            LOOP
          ENDIF
          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_nam) + ' file.' NOWAIT

*-- Replace Old value by new value
          lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
          IF SYDFDCHG.cFile_nam = 'INVCHRG'
            lnAlias = ALIAS()
            IF !lfOpClFile(loFormSet,'O','INVHDR','INVHDR')
              LOOP
            ENDIF
            SELECT (lnAlias)
            SET RELATION TO
            SET RELATION TO Invchrg.invoice INTO Invhdr ADDITIVE

            lcFltExp = lfGetExp(loFormSet)
            *B612447,1 MMT 08/12/2021 Key Change Corrupts store IDs if it starts with same characters[T20210806.0001][Start]
*!*	            REPLACE ALL &lcFldtmp WITH loFormSet.lcNewKey ;
*!*	              FOR ALLTRIM(&lcFldtmp) = ALLTRIM(loFormSet.lcOldKey) .AND. Invhdr.ACCOUNT=loFormSet.lcPKey ;
*!*	              .AND. &lcFltExp
            REPLACE ALL &lcFldtmp WITH loFormSet.lcNewKey ;
              FOR ALLTRIM(&lcFldtmp) == ALLTRIM(loFormSet.lcOldKey) .AND. Invhdr.ACCOUNT=loFormSet.lcPKey ;
              .AND. &lcFltExp
            *B612447,1 MMT 08/12/2021 Key Change Corrupts store IDs if it starts with same characters[T20210806.0001][End]
          ELSE
            lcFltExp = lfGetExp(loFormSet)
*xUx*
            IF SYDFILES.CVER = 'A27'
              *B612447,1 MMT 08/12/2021 Key Change Corrupts store IDs if it starts with same characters[T20210806.0001][Start]
*!*	              REPLACE ALL &lcFldtmp WITH loFormSet.lcNewKey ;
*!*	                FOR ALLTRIM(&lcFldtmp) = ALLTRIM(loFormSet.lcOldKey).AND.ACCOUNT=loFormSet.lcPKey ;
*!*	                .AND. &lcFltExp
              REPLACE ALL &lcFldtmp WITH loFormSet.lcNewKey ;
                FOR ALLTRIM(&lcFldtmp) == ALLTRIM(loFormSet.lcOldKey).AND.ACCOUNT=loFormSet.lcPKey ;
                .AND. &lcFltExp
              *B612447,1 MMT 08/12/2021 Key Change Corrupts store IDs if it starts with same characters[T20210806.0001][End]                
            ELSE
              lcSQL = "UPDATE "+ ALLTRIM(SYDFILES.cFile_nam) + " " +;
                "SET &lcFldTmp = '"+loFormSet.lcNewKey + "' "+;
                "WHERE LTRIM(RTRIM(&lcFldTmp)) = '"+ALLTRIM(loFormSet.lcOldKey)+"' "+;
                "AND ACCOUNT = '"+loFormSet.lcPKey +"' "+;
                "AND &lcFltExp "
              lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
              =lfSetstep(lnSQLRunResult)
            ENDIF

          ENDIF
          WAIT CLEAR
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
ENDCASE
*- End of lfvCustm.

*!************************************************************************
*! Name      : lfGetSegmt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : To get the styles segments.
*!************************************************************************
*! Called from : Custom Option Grid.
*!************************************************************************
FUNCTION lfGetSegmt
PARAMETERS loFormSet

*-- Compute Free/Color Items in Style Structure. [Begin]
loFormSet.lnMajSegmt  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
DIMENSION loFormSet.laMajSeg[ALEN(laMajSeg,1),ALEN(laMajSeg,2)]
ACOPY(laMajSeg,loFormSet.laMajSeg)


llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = loFormSet.lnMajSegmt + 1 TO ALEN(loFormSet.laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,loFormSet.laMajSeg[lnI,4],lnNonMajPo)

  IF loFormSet.laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),loFormSet.laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),loFormSet.laMajSeg[lnI,3],lcNonMajPi + loFormSet.laMajSeg[lnI-1,6] + loFormSet.laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(loFormSet.laMajSeg[lnI,2],LEN(loFormSet.laMajSeg[lnI,3])),;
      lcNonMajT + loFormSet.laMajSeg[lnI-1,6] + PADR(loFormSet.laMajSeg[lnI,2],LEN(loFormSet.laMajSeg[lnI,3])))
  ENDIF

*-- If you Find Color Type or Find Free Type and current type not Free.
  IF loFormSet.laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND loFormSet.laMajSeg[lnI,1] != 'F')
    IF loFormSet.laMajSeg[lnI,1] = 'C'
      loFormSet.llColorExt = .T.
      loFormSet.lnClrPo    = loFormSet.laMajSeg[lnI,4]
      lcFreeClr  = loFormSet.laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = loFormSet.laMajSeg[lnI,3]
      lcNonMajT  = PADR(loFormSet.laMajSeg[lnI,2],LEN(loFormSet.laMajSeg[lnI,3]))
      EXIT
    ELSE
*-- this means that another type is found rather than color or free
*-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , loFormSet.lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*-- End of lfGetSegmt.

*!************************************************************************
*! Name      : lfsrSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Set and Rest functions for style filter.
*!************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!************************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
*-- open this file in another alias to set order to Style Major
*-- unique index.
  USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG CSTYLE
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- End of lfsrvSty.

*!************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : sum a specific field for the current style in style file
*!************************************************************************
*! Returns   : Calculated field value.
*!************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT STYLE_X
  SUM &lccomp TO lnTotcomp WHILE ALLTRIM(CSTYMAJOR) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF
  DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF
RETURN INT(lnTotcomp)
*-- End of lfStySum.

*!************************************************************************
*! Name      : lfvWareHo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Validate warehouse
*!************************************************************************
*! Called from : Option Grid
*!************************************************************************
FUNCTION lfvWareHo
*PARAMETERS loFormSet
PRIVATE lcWareHo , lcTag
*lcWareHo = VARREAD()
loFld = _Screen.ActiveForm.ActiveControl
lcTag = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS
IF loFld.Value <> loFld.OldValue
  IF SEEK(loFld.Value,'WAREHOUS')
    loFld.Value = WAREHOUS.CWARECODE
  ELSE
    loFld.Value = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  ENDIF
*!*  ELSE
*!*    &lcWareHo = ''
ENDIF
SET ORDER TO WAREHOUS IN WAREHOUS
*-- End of lfvWareHo.

*!************************************************************************
*! Name      : lfvFabric
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Validate fabric
*!************************************************************************
*! Called from : Option Grid
*!************************************************************************
FUNCTION lfvFabric
*PARAMETERS loFormSet
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (oAriaApplication.DataDir+'FABRIC') IN 0 SHARE
ENDIF

lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC')
    &lcFabObj = FABRIC.FABRIC
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF

SET ORDER TO TAG (lcTag) IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF
*-- End of lfvFabric.

*!************************************************************************
*! Name      : lpStyle
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Brow the style inrange and modify the filter.
*!************************************************************************
FUNCTION lpStyle
PRIVATE lcAlias , lcExpSty
IF TYPE('lcStyleExp') = "L"
  RETURN
ENDIF
*E611964,1 Es 10/23/2019 Change size scales from one size scale to another [Start]
DECLARE laScopExpr[1],laScopeSty[1]
STORE "" TO laScopExpr,laScopeSty
llSaveCriteria = .F.
=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)
laScopIMP = lcRpDomImp
*E611964,1 Es 10/23/2019 Change size scales from one size scale to another [End]



lcAlias = ALIAS()
loFormSet.lcTmpStyle = gfTempName()
STORE '' TO lcExpSty

*B610654,1 TMI 01/08/2014 11:41 [Start] add the field KEYEXP
*CREATE TABLE  (oAriaApplication.WorkDir+loFormSet.lcTmpStyle) (Style C(19))
CREATE TABLE  (oAriaApplication.WorkDir+loFormSet.lcTmpStyle) (Style C(19),KEYEXP C(19))
*B610654,1 TMI 01/08/2014 11:41 [End  ]
ZAP
INDEX ON Style TAG (loFormSet.lcTmpStyle) OF (oAriaApplication.WorkDir+loFormSet.lcTmpStyle+'.CDX')
lcBrowFlds = [STYLE     :H = lcStyTtl      :25 ,]    + ;
  [DESC      :H = 'Description' :25 ,]    + ;
  [SEASON    :H = 'Season'      :15 ,]    + ;
  [CDIVISION :H = 'Division'    :15 ,]    + ;
  [PRICEA    :H = 'Price'       :10 ]

LOCAL lcFlt1,lcFlt2,lcFlt3


lcFlt1 = gfGenFlt('laOgFxFlt',.T.)
lcFlt1 = IIF(EMPTY(lcFlt1),'.T.',lcFlt1)
lcFlt2 = gfGenFlt('laOgVrFlt',.T.)
lcFlt2 = IIF(EMPTY(lcFlt2),'.T.',lcFlt2)
lcFlt3 = gfGenFlt('laOgHdFlt',.T.)
lcFlt3 = IIF(EMPTY(lcFlt3),'.T.',lcFlt3)
lcStyleExp = lcFlt1+' .AND. '+lcFlt2+' .AND. '+lcFlt3

*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
*Season
llUseSeason  = .F.
lnSeaPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.SEASON")
IF lnSeaPos > 0
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnSeaPos,6]),loOgScroll.laOgFxFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel)
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
    IF llUseSeason
      lnSEAStart = AT('INLIST(STYLE.SEASON',lcStyleExp)
      IF lnSEAStart > 0
         lnEndPos = AT(")",SUBSTR(lcStyleExp,lnSEAStart))+lnSEAStart-1
         lnNumChar = lnEndPos -lnSEAStart+1
         lcStyleExp = STUFF(lcStyleExp ,lnSEAStart,lnNumChar,"Seek(STYLE.SEASON,'&lcSeaFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF

*Division
llUseDiv  = .F.
lnDivPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.CDIVISION")
IF lnDivPos > 0
  lnDivPos = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnDivPos,1)
  lcDivSel =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnDivPos,6]),loOgScroll.laOgFxFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel)
    lcDivFile = loOGScroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
    IF llUseDiv
      lnDivStart = AT('INLIST(STYLE.CDIVISION',lcStyleExp)
      IF lnDivStart > 0
         lnEndPos = AT(")",SUBSTR(lcStyleExp,lnDivStart))+lnDivStart-1
         lnNumChar = lnEndPos -lnDivStart+1
         lcStyleExp = STUFF(lcStyleExp ,lnDivStart,lnNumChar,"Seek(STYLE.CDIVISION,'&lcDivFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF

*Style Group
llUseGrp  = .F.
lnGrpPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.CSTYGROUP")
IF lnGrpPos  > 0
  lnGrpPos  = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnGrpPos ,1)
  lcGrpSel =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnGrpPos ,6]),loOgScroll.laOgFxFlt[lnGrpPos ,6],'')
  IF !EMPTY(lcGrpSel)
    lcGrpFile = loOGScroll.gfTempName()
    llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
    IF llUseGrp
      lnGrpStart = AT('INLIST(STYLE.CSTYGROUP',lcStyleExp)
      IF lnGrpStart > 0
         lnEndPos = AT(")",SUBSTR(lcStyleExp,lnGrpStart))+lnGrpStart-1
         lnNumChar = lnEndPos -lnGrpStart+1
         lcStyleExp = STUFF(lcStyleExp ,lnGrpStart,lnNumChar,"Seek(STYLE.CSTYGROUP,'&lcGrpFile')")
      ENDIF
    ENDIF
  ENDIF
ENDIF



*Color
llUseClr1  = .F.
lnClr1Pos = ASCAN(loOgScroll.laOgFxFlt,"SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)")
IF lnClr1Pos > 0
  lnClr1Pos  = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnClr1Pos,1)
  lcClr1Sel =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnClr1Pos ,6]),loOgScroll.laOgFxFlt[lnClr1Pos,6],'')
  IF !EMPTY(lcClr1Sel )
    lcClr1File = loOGScroll.gfTempName()
    llUseClr1= IIF(LEN(lcClr1Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr1Sel,'CSTYCLR',lcClr1File )
    IF llUseClr1
      lnClr1Start = AT('INLIST(SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)',lcStyleExp)
      IF lnClr1Start> 0
         lnEndPos = AT(")",SUBSTR(lcStyleExp,lnClr1Start),2) +lnClr1Start -1
         lnNumChar = lnEndPos - lnClr1Start+1
         lcStyleExp = STUFF(lcStyleExp,lnClr1Start,lnNumChar,"Seek(SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen),'&lcClr1File')")
      ENDIF
    ENDIF
  ENDIF
ENDIF

*Color2
llUseClr2  = .F.
lnClr2Pos = ASCAN(loOgScroll.laOgFxFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
IF lnClr2Pos > 0
  lnClr2Pos  = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnClr2Pos,1)
  lcClr2Sel =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnClr2Pos ,6]),loOgScroll.laOgFxFlt[lnClr2Pos,6],'')
  IF !EMPTY(lcClr2Sel)
    lcClr2File = loOGScroll.gfTempName()
    llUseClr2= IIF(LEN(lcClr2Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr2Sel,'CSTYCLR2',lcClr2File )
    IF llUseClr2
      lnClr2Start = AT('INLIST(SUBSTR(STYLE.Style,lnClrPo,lnColorLen)',lcStyleExp)
      IF lnClr2Start> 0
         lnEndPos = AT(")",SUBSTR(lcStyleExp,lnClr2Start),2)+lnClr2Start-1
         lnNumChar = lnEndPos -lnClr2Start+1
         lcStyleExp = STUFF(lcStyleExp,lnClr2Start,lnNumChar,"Seek(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),'&lcClr2File')")
      ENDIF
    ENDIF
  ENDIF
ENDIF
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]




*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [Start]
IF !EMPTY(lcStyleExp)
  lcStyleExp=lcStyleExp+" .AND. ALLTRIM(Scale)='"+ALLTRIM(loFormSet.lcOldVal)+"'"
ELSE
  lcStyleExp="ALLTRIM(Scale)='"+ALLTRIM(loFormSet.lcOldVal)+"'"
ENDIF
*E611964,1 Es 11/05/2019  Change size scales from one size scale to another [End]
lcExpSty = 'FOR ' + IIF(EMPTY(lcStyleExp),".T.",lcStyleExp)

SELECT Style
SCAN FOR &lcStyleExp
  WAIT WINDOW 'Selecting ' + lcStyTtl + " : " + Style NOWAIT
  m.Style = Style
  INSERT INTO (loFormSet.lcTmpStyle) FROM MEMVAR
ENDSCAN
WAIT CLEAR
SELECT (loFormSet.lcTmpStyle)
LOCATE
SELECT Style
llLastExp = gfrange(lcBrowFlds,loFormSet.lcTmpStyle,"Style",(lcExpSty),"","","@! XXXXXXXXXXXXXXXXXXX")
SET STEP ON
IF !llLastExp
  SELECT (loFormSet.lcTmpStyle)
  ZAP

*E611964,1 Es 12/01/2019 Change size scales from one size scale to another [Start]
  RETURN .F.
ELSE
  SELECT   (loFormSet.lcTmpStyle)
  *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][Start]
  *DIMENSION laScopeSty[RECCOUNT(),2]
  DIMENSION laScopeSty[1,2]
  *B612139,1 MMT 05/10/2020 Key change does not update all selected styles scale while changing scale sizes[T20200202.0002][Start]
  lnSelStyCnt = 0
  COUNT TO lnSelStyCnt  FOR !EMPTY(ALLTRIM(EVALUATE(loFormSet.lcTmpStyle+'.Style'))) AND !DELETED()
  DIMENSION laScopeSty[lnSelStyCnt,2]
  *B612139,1 MMT 05/10/2020 Key change does not update all selected styles scale while changing scale sizes[T20200202.0002][End]
  *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][Start]
  STORE '' TO  laScopeSty
  *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][Start]
  *COPY  to Array laScopeSty
  COPY  to ARRAY laScopeSty FOR !EMPTY(ALLTRIM(EVALUATE(loFormSet.lcTmpStyle+'.Style'))) AND !DELETED()
  *B612046,2 MMT 03/09/2020 Error in Key change screen while filtering style and filter crtteria give many styles[T20200202.0002][End]
ENDIF
lfSaveCriteria('STYLE.CSTYMAJOR')
lfSaveCriteria('STYLE.CDEFWARE')
lfSaveCriteria('STYLE.FABRIC')
*E611964,1 Es 10/23/2019 Change size scales from one size scale to another [End]


SELECT (lcAlias)
*-- End of lpStyle.


*E611964,1 Es 10/24/2019 Change size scales from one size scale to another [Start]
*!************************************************************************
*! Name      : lfSaveCriteria
*! Developer : Esraa Ahmed (Es)
*! Date      : 10/24/2019
*! Purpose   : Save OptionGrid Criteria
*!************************************************************************
FUNCTION lfSaveCriteria
PARAMETERS lcExpr
lnPos    = ASCAN(loOGScroll.laOGFxFlt, lcExpr)
lnPos    = IIF(lnPos > 0, ASUBSCRIPT(loOGScroll.laOGFxFlt, lnPos, 1), 0)
lcCursor = ""
SET DELETED OFF

IF lnPos > 0
  lcValue = loOGScroll.laOGFxFlt[lnPos,6]
  IF  USED(lcValue) AND RECCOUNT(lcValue) > 0
    SELECT (lcValue)
    DO CASE
    CASE UPPER(ALLTRIM(lcExpr))='STYLE.CSTYMAJOR'
      DIMENSION laScopeMajor[RECCOUNT(),FCOUNT()]
      STORE '' TO  laScopeMajor
      COPY  to Array laScopeMajor
    CASE UPPER(ALLTRIM(lcExpr))='STYLE.CDEFWARE'
      DIMENSION laScopeLoc[RECCOUNT(),FCOUNT()]
      STORE '' TO  laScopeLoc
      COPY  to Array laScopeLoc
    CASE UPPER(ALLTRIM(lcExpr))='STYLE.FABRIC'
      DIMENSION laScopeFabric[RECCOUNT(),FCOUNT()]
      STORE '' TO  laScopeFabric
      COPY  to Array laScopeFabric
    ENDCASE
  ENDIF
ENDIF

*E611964,1 Es 10/24/2019 Change size scales from one size scale to another [End]
*!************************************************************************
*! Name      : lfAddFab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Add Fabric in Fabric file.
*!************************************************************************
FUNCTION lfAddFab
PARAMETERS loFormSet

*-- If entering style cost sheet & the style structure has no
*-- non major, No need for cross refrencing the colors.
IF !loFormSet.llNonMjExt .OR. !loFormSet.llColorExt
  RETURN .T.
ENDIF

PRIVATE lcAlias , laItemRec , llUseFabrc , lcFabrcTag , lcOnlyColr
lcAlias = ALIAS()
lcOnlyColr = SUBSTR(SUBSTR(&lcFldtmp,1,loFormSet.lnMajSize+1)  + ALLTRIM(KEY_CHG.New_Key),loFormSet.lnClrPo,loFormSet.lnColorLen)

llUseFabrc = .F.
IF !USED('Fabric')
  llUseFabrc = .T.

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]

* USE (oAriaApplication.DataDir+'Fabric') IN 0 SHARE
  =gfOpenTable('ITEM','STYLE','SH','Fabric')
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]

ENDIF

lcFabrcTag = ORDER('Fabric')

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]
*SET ORDER TO Fabric IN Fabric
SELECT 'Fabric'
=gfSetOrder('STYLE')
lcMajFab = gfItemMask('PM',.F.,'0002')     && Get the major pict. of the style
lnMajSizeFab = LEN(lcMajFab)
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]


*-- If "same as color" , cross refrencing the colors or the majors.
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]
*IF Bom.cCatgTyp = 'F' OR (Bom.cCatgTyp = 'T' AND SEEK(ALLTRIM(Bom.Item),'Fabric'))
IF Bom.cCatgTyp = 'F' OR (Bom.cCatgTyp = 'T' AND gfSEEK('0002'+ALLTRIM(Bom.Item),'Fabric'))
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]


  WAIT IIF(Bom.cCatgTyp='F','Cross referencing Fabric colors','Cross referencing Trim colors') WINDOW NOWAIT
  SELECT FABRIC

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]
*=SEEK(PADR(ALLTRIM(Bom.Item),7))
  =gfSEEK("0002"+PADR(ALLTRIM(Bom.Item),lnMajSizeFab))
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]
  SCATTER TO laItemRec

*-- If the cost item was fabric or trim.

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]

*!*    IF !SEEK(SUBSTR(ALLTRIM(Bom.Item),1,7)+lcOnlyColr , 'Fabric')
*!*      =gfModalGen("INM00393B00000" , "DIALOG" , gfItemMask("HN")+"|"+lcOnlyColr+"|"+IIF(Bom.cCatgTyp= 'F','Fabric','Trim')+"|"+SUBSTR(ALLTRIM(Bom.Item),1,7))

  IF !gfSEEK(SUBSTR(ALLTRIM(Bom.Item),1,lnMajSizeFab)+lcOnlyColr , 'Fabric')
    =gfModalGen("INM00393B00000" , "DIALOG" , gfItemMask("HN")+"|"+lcOnlyColr+"|"+IIF(Bom.cCatgTyp= 'F','Fabric','Trim')+"|"+SUBSTR(ALLTRIM(Bom.Item),1,lnMajSizeFab))

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]

*-- If the fabric + color was not found, add it in the temp. fabric file.
    SELECT FABRIC
    APPEND BLANK
    GATHER FROM laItemRec

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]

*!*      REPLACE Color   WITH lcOnlyColr ;
*!*              OnHand  WITH 0 ;
*!*              OnOrder WITH 0 ;
*!*              Usage   WITH 0 ;
*!*              nStkVal WITH 0 ;
*!*              nMatWip WITH 0

    REPLACE STYLE   WITH SUBSTR(STYLE,1,lnMajSizeFab+1)+lcOnlyColr ;
      TOTSTK WITH 0 ;
      TOTWIP WITH 0;
      NTOTHUSAGE WITH 0;
      nStkVal WITH 0 ;
      NTOTCUSA WITH 0
    =gfReplace('')
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]

*-- Call global function to add audit fields info.
    =gfAdd_Info('Fabric')
  ENDIF

ENDIF
WAIT CLEAR

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]
*SET ORDER TO TAG (lcFabrcTag) IN Fabric
SELECT FABRIC
=gfTableUpdate()
=gfSetOrder(lcFabrcTag)
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]

IF llUseFabrc
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]
* USE IN Fabric
  =gfCloseTable('Fabric')
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]
ENDIF

SELECT (lcAlias)
*-- End of lfAddFab.

*!************************************************************************
*! Name      : lfStyExist
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Display a message of existing style.
*!************************************************************************
FUNCTION lfStyExist
PARAMETERS lcCurStyle

IF !SEEK(SUBSTR(lcCurStyle,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key),lcTmpExist)
*-- Message < Style : ~ exist in style file. Cannot proceed >
*-- Buttons <                       OK                      >
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
*=gfModalGen("INM00372B00000","DIALOG",SUBSTR(lcCurStyle,1,loFormSet.lnMajSize+1) + ALLTRIM(Key_Chg.New_Key))
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
  m.Style = SUBSTR(lcCurStyle,1,loFormSet.lnMajSize+1) + ALLTRIM(KEY_CHG.New_Key)
  INSERT INTO (lcTmpExist) FROM MEMVAR
ENDIF
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
*RETURN .F.
RETURN .t.
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]
*-- End of lfStyExist,

*!************************************************************************
*! Name      : lpSelecSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Display an OG to select styles to apply changes in its Non Major.
*!************************************************************************
FUNCTION lpSelecSty
PARAMETERS loFormSet

PRIVATE lcStyleExp , lcColorTt , lcNonMajT , lcFreeClr , lcNonMajPi , ;
  lnNonMajPo , lnFreeLen , llLastExp , lcStyTtl , lcRpDomImp
STORE '' TO lcColorTt , lcNonMajT , lcFreeClr , lcNonMajPi , ;
  lcStyTtl , lcRpDomImp

*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [begin]
*STORE 0 TO lnNonMajPo , lnFreeLen
STORE 0 TO lnNonMajPo , lnFreeLen,lnColorLen,lnClrPo
*B611660,1 SAH modify the program as the Key Change program does not update the SQL tables when style Non major is changed [end]

=lfGetSegmt(loFormSet)

PUSH KEY CLEAR
IF !EMPTY(loFormSet.lcTmpStyle) AND USED(loFormSet.lcTmpStyle) AND RECCOUNT(loFormSet.lcTmpStyle) > 0
  SELECT (loFormSet.lcTmpStyle)
  ZAP
ENDIF


lcStyleExp = gfOpGrid('SMFLDVL', .T.)


  *B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
*IF TYPE('lcStyleExp') = "L"
IF TYPE('lcStyleExp') = "L" OR lcStyleExp =".F."
  *B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]

  loFormSet.AriaForm1.pgfChange.Ariapage1.cbValues.Value = .F.
  RETURN
ELSE
*DO lpStyle
ENDIF
POP KEY
*-- End of lpSelecSty
*E611964,1 Es 10/22/2019 Change size scales from one size scale to another [Start]
*!************************************************************************
*! Name      : lfSelsty
*! Developer : Esraa Ahmed (ES)
*! Date      : 10/22/2019
*! Purpose   : Display an OG to select styles.
*!************************************************************************
FUNCTION lfSelsty

SELECT (loFormSet.lcValues)
IF !EMPTY(ALLTRIM(mSaveFilt))
  RESTORE FROM memo mSaveFilt ADDITIVE
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
ELSE 
    STORE "" TO   laScopIMP ,laScopExpr,laScopeSty,laScopeMajor,laScopeLoc,laScopeFabric
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]

ENDIF


PRIVATE lcStyleExp , lcColorTt , lcNonMajT , lcFreeClr , lcNonMajPi , ;
  lnNonMajPo , lnFreeLen , llLastExp , lcStyTtl , lcRpDomImp
STORE '' TO lcColorTt , lcNonMajT , lcFreeClr , lcNonMajPi , ;
  lcStyTtl , lcRpDomImp

STORE 0 TO lnNonMajPo , lnFreeLen,lnColorLen,lnClrPo


=lfGetSegmt(loFormSet)

lnColorLen=loFormSet.lnColorLen      
lnClrPo=loFormSet.lnClrPo    

PUSH KEY CLEAR
IF !EMPTY(loFormSet.lcTmpStyle) AND USED(loFormSet.lcTmpStyle) AND RECCOUNT(loFormSet.lcTmpStyle) > 0
  SELECT (loFormSet.lcTmpStyle)
  ZAP
ENDIF
 
lcStyleExp = gfOpGrid('SMFLDVL', .T.)

  *B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
*IF TYPE('lcStyleExp') = "L"
IF TYPE('lcStyleExp') = "L" OR lcStyleExp =".F."
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]
  RETURN
ELSE
  SELECT (loFormSet.lcValues)
  lcstatus=IIF(ALLTRIM(UPPER(cStatus)) ='A','A','M')
  REPLACE  cStatus WITH  lcstatus
  SAVE TO memo mSaveFilt ALL LIKE lascop*
ENDIF
*-- End of lfSelsty
*E611964,1 Es 10/22/2019 Change size scales from one size scale to another [End]



*!************************************************************************
*! Name      : lfwRepWhen
*! Developer : Esraa Ahmed (ES)
*! Date      : 10/24/2019
*!************************************************************************
FUNCTION lfwRepWhen
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
IF !USED('ITEM')
  =gfOpenTable('ITEM','STYLE','SH')
ENDIF
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]

IF TYPE('laScopIMP') ='C' AND !EMPTY(laScopIMP)
  lcRpDomImp =laScopIMP
ENDIF
IF TYPE('laScopExpr[1,1]') = "C" AND !EMPTY(laScopExpr[1,1])
  =ACOPY(laScopExpr , loOGScroll.laOGFxFlt)
  =ACOPY(laScopExpr , laOGFxFlt)


*!*  CSTYMAJOR
  lnPos    = ASCAN(loOGScroll.laOGFxFlt, 'STYLE.CSTYMAJOR')
  lnPos    = IIF(lnPos > 0, ASUBSCRIPT(loOGScroll.laOGFxFlt, lnPos, 1), 0)
  IF lnPos > 0
    lcValue = loOGScroll.laOGFxFlt[lnPos,6]
    IF !EMPTY(lcValue)
      CREATE CURSOR (lcValue) (Keyexp c(19),Cstymajor c(19))
      Index On KEYEXP TAG (lcValue)
      IF  USED(lcValue)
        SELECT (lcValue)
        
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
IF  TYPE('laScopeMajor') != 'U' AND !EMPTY(laScopeMajor)
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]
      
        FOR i=1 TO ALEN(laScopeMajor,1)
          IF !SEEK(ALLTRIM(laScopeMajor[i,1]),lcValue)
            INSERT INTO (lcValue) (KEYEXP,CSTYMAJOR) VALUES (laScopeMajor[i,1],laScopeMajor[i,1])
          ENDIF
        ENDFOR
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
ENDIF 
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]
   
      ENDIF
    ENDIF
  ENDIF

*!*  CDEFWARE
  lnPos    = ASCAN(loOGScroll.laOGFxFlt, 'STYLE.CDEFWARE')
  lnPos    = IIF(lnPos > 0, ASUBSCRIPT(loOGScroll.laOGFxFlt, lnPos, 1), 0)
  IF lnPos > 0
    lcValue = loOGScroll.laOGFxFlt[lnPos,6]
    IF !EMPTY(lcValue)
      CREATE CURSOR (lcValue) (Keyexp c(19),CDEFWARE c(19))
      Index On KEYEXP TAG (lcValue)
      IF  USED(lcValue)
        SELECT (lcValue)
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
IF  TYPE('laScopeLoc') != 'U' AND !EMPTY(laScopeLoc)
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]
   
        FOR i=1 TO ALEN(laScopeLoc,1)
          IF !SEEK(ALLTRIM(laScopeLoc[i,1]),lcValue)
            INSERT INTO (lcValue) (KEYEXP,CDEFWARE) VALUES (laScopeLoc[i,1],laScopeLoc[i,1])
          ENDIF
        ENDFOR
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
ENDIF 
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]
   
      ENDIF
    ENDIF
  ENDIF

*!*  FABRIC
  lnPos    = ASCAN(loOGScroll.laOGFxFlt, 'STYLE.FABRIC')
  lnPos    = IIF(lnPos > 0, ASUBSCRIPT(loOGScroll.laOGFxFlt, lnPos, 1), 0)
  IF lnPos > 0
    lcValue = loOGScroll.laOGFxFlt[lnPos,6]
    IF !EMPTY(lcValue)
      CREATE CURSOR (lcValue) (Keyexp c(19),FABRIC c(19))
      Index On KEYEXP TAG (lcValue)
      IF  USED(lcValue)
        SELECT (lcValue)
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
IF  TYPE('laScopeFabric') != 'U' AND !EMPTY(laScopeFabric)
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]

        FOR i=1 TO ALEN(laScopeFabric,1)
          IF !SEEK(ALLTRIM(laScopeFabric[i,1]),lcValue)
            INSERT INTO (lcValue) (KEYEXP,FABRIC) VALUES (laScopeFabric[i,1],laScopeFabric[i,1])
          ENDIF
        ENDFOR
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
ENDIF 
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]
   
      ENDIF
    ENDIF
  ENDIF


ENDIF

*E611964,1 Es 10/24/2019 Change size scales from one size scale to another [End]
*!************************************************************************
*! Name      : lfGetExp
*! Developer : Walid A. Wahab
*! Date      : 03/11/2002
*! Purpose   : create a filter expersion from SYDFDCHG.mFltExpr
*!************************************************************************
FUNCTION lfGetExp
PARAMETERS loFormSet
LOCAL lnLineNo,lnCnt
lnLineNo  = MEMLINES(SYDFDCHG.mFltExpr)
lcFltExp  = ''
FOR lnCnt = 1 TO lnLineNo
  lcFltExp = lcFltExp + MLINE(SYDFDCHG.mFltExpr,lnCnt)
ENDFOR
*E303425,3 TMI 11/24/2013 19:23 [Start] in case of empty add a condition that evalues to true to be value for use in fox and sql
*RETURN IIF(!EMPTY(lcFltExp),ALLTRIM(lcFltExp),".T.")
IF SYDFILES.CVER = 'A27' AND !EMPTY(lcFltExp) AND ' IN '$UPPER(lcFltExp)
  lcFltExp = STRTRAN(lcFltExp,' IN ',' $ ')
  lcFltExp = STRTRAN(lcFltExp,"','","")
ENDIF
RETURN IIF(!EMPTY(lcFltExp),ALLTRIM(lcFltExp),"1=1")
*E303425,3 TMI 11/24/2013 19:23 [End  ]
*- End of lfGetExp.
************************************************************
*! Name      : lfSetstep
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/05/2013
*! Purpose   : issue the SET STEP ON based on return value from sqlrun
************************************************************
FUNCTION lfSetstep
PARAMETERS lnResult
lnResult = IIF(TYPE('lnResult')='L',0,lnResult)
IF lnResult < 0
*IF '.EXE' $ SYS(16,1)
*  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Error running the statement '+CHR(13)+lcSql)
*ELSE
*ENDIF
ENDIF
*- End of lfSetstep.

************************************************************
*! Name      : lfFabric
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/24/2013
*! Purpose   : update function for the fabric case
*              the function always replaces the major part of the fabric field whether the field contains major or major+color
************************************************************
*E303425,3 TMI 11/24/2013 20:32 [Start]
FUNCTION lfFabric
PARAMETERS lcStatus

DO CASE
CASE lcStatus = 'O'
  loFormSet.lcOldVal = lfVldBrw(loBranchFormSet,loFormSet.lcOldVal)

CASE lcStatus = 'N'
  loFormSet.lcNewVal =lfVldBrw(loBranchFormSet,loFormSet.lcNewVal)

CASE lcStatus = 'R'

  =SEEK(KEY_CHG.cKeyType+KEY_CHG.cKey_Chg,'SYDFDCHG')
  SELECT SYDFDCHG
  SCAN WHILE cKeyType+CFld_name = KEY_CHG.cKeyType + KEY_CHG.cKey_Chg
*-- If main field record , Get all files that has that field then replace old value by new value
    IF EMPTY(SYDFDCHG.cAltField)
      SELECT SYDFLFLD
      =SEEK(SYDFDCHG.CFld_name)
      SCAN WHILE CFld_name = SYDFDCHG.CFld_name
*-- Activate the file by openning or selecting it.
*-- Don't replace the field value in the main file
        IF  !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFLFLD.cFile_nam),'')
          LOOP
        ENDIF

        lcFl = ALLTRIM(SYDFLFLD.cFile_nam)
        WAIT WINDOW 'Updating ' + lcFl +' file.' NOWAIT
*-- Replace Old value by new value
        lcFldtmp = ALLTRIM(SYDFDCHG.CFld_name)
        lcFltExp = lfGetExp(loFormSet)

        IF SYDFILES.CVER = 'A27'
          SELECT (lcFl)
          REPLACE ALL &lcFldtmp WITH PADR(KEY_CHG.New_Key,loFormSet.lnFabMajSize) + SUBSTR(&lcFldtmp ,loFormSet.lnFabMajSize+1) ;
            FOR ALLTRIM(SUBSTR(&lcFldtmp,1,loFormSet.lnFabMajSize)) == ALLTRIM(KEY_CHG.Old_Key) .AND. &lcFltExp
        ELSE
*- update sql
          lcFs = ALLTRIM(STR(loFormSet.lnFabMajSize+1))
*E303425,3 TMI 11/25/2013 10:26 [Start]
*lcLn = ALLTRIM(STR(19 - (loFormSet.lnFabMajSize + 1)))
          lcLn = ALLTRIM(STR(19 - (loFormSet.lnFabMajSize)))
*E303425,3 TMI 11/25/2013 10:26 [End  ]
          lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
            "SET &lcFldtmp = '"+PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnFabMajSize)+"'+SUBSTRING(&lcFldtmp,&lcFs,&lcLn) "+;
            "WHERE SUBSTRING(&lcFldtmp,1,"+STR(loFormSet.lnFabMajSize,2)+") = '"+PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnFabMajSize)+"' "+;
            "AND &lcFltExp"
          lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
          =lfSetstep(lnSQLRunResult)
        ENDIF
        WAIT CLEAR
      ENDSCAN

    ELSE   && i.e. !EMPTY(SYDFDVLD.cAltField)
*-- Open necessary files
      IF !SEEK(SYDFDCHG.cFile_nam,'SYDFILES')
        LOOP
      ENDIF
      IF !lfOpClFile(loFormSet,'O',ALLTRIM(SYDFDCHG.cFile_nam),ALLTRIM(SYDFDCHG.cBaseTag) )
        LOOP
      ENDIF

      WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_nam) + ' file.' NOWAIT
*-- Replace Old value by new value
      lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)

      lcFltExp = lfGetExp(loFormSet)
*xUx*
      IF SYDFILES.CVER = 'A27'
        REPLACE ALL &lcFldtmp WITH PADR(KEY_CHG.New_Key,loFormSet.lnFabMajSize) + SUBSTR(&lcFldtmp ,loFormSet.lnFabMajSize+1) ;
          FOR ALLTRIM(SUBSTR(&lcFldtmp,1,loFormSet.lnFabMajSize)) == ALLTRIM(KEY_CHG.Old_Key) .AND. &lcFltExp
      ELSE
*- update sql
        lcFs = ALLTRIM(STR(loFormSet.lnFabMajSize+1))
*E303425,3 TMI 11/25/2013 10:26 [Start]
*lcLn = ALLTRIM(STR(19 - (loFormSet.lnFabMajSize + 1)))
        lcLn = ALLTRIM(STR(19 - (loFormSet.lnFabMajSize)))
*E303425,3 TMI 11/25/2013 10:26 [End  ]
        lcSQL = "UPDATE "+ALLTRIM(SYDFILES.cFile_nam)+" "+;
          "SET &lcFldtmp = '"+PADR(ALLTRIM(KEY_CHG.New_Key),loFormSet.lnFabMajSize)+"'+SUBSTRING(&lcFldtmp,&lcFs,&lcLn) "+;
          "WHERE SUBSTRING(&lcFldtmp,1,"+STR(loFormSet.lnFabMajSize,2)+") = '"+PADR(ALLTRIM(KEY_CHG.Old_Key),loFormSet.lnFabMajSize)+"' "+;
          "AND &lcFltExp"
        lnSQLRunResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSQL,'','', oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))
        =lfSetstep(lnSQLRunResult)
      ENDIF
      WAIT CLEAR
    ENDIF
  ENDSCAN

ENDCASE
*- End of lfFabric.
*E303425,3 TMI 11/24/2013 20:32 [End  ]
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][Start]
************************************************************
*! Name      : lfInitConf
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/04/2016
*! Purpose   : Init Conflict Form
************************************************************
FUNCTION lfInitConf
PARAMETERS loBrForm
loBrForm.AriaForm1.grdHead.RecordSource =''
loBrForm.AriaForm1.grdHead.RecordSource ='StyleList'
loBrForm.AriaForm1.grdHead.Column1.ControlSource = 'StyleList.OldStyle'
loBrForm.AriaForm1.grdHead.Column2.ControlSource = 'StyleList.NewStyle'
loBrForm.AriaForm1.grdHead.Column3.ControlSource = 'IIF(StyleList.Status="A","Allowed","Not Allowed")'
loBrForm.AriaForm1.grdHead.Column3.DynamicBackColor ='IIF(StyleList.Status="A",RGB(0,255,0),RGB(255,0,0))'
loBrForm.AriaForm1.grdDet.RecordSource =''
loBrForm.AriaForm1.grdDet.RecordSource = 'IssueList'
loBrForm.AriaForm1.grdDet.Column1.ControlSource = 'IssueList.FieldDesc'
loBrForm.AriaForm1.grdDet.Column2.ControlSource = 'IssueList.OldValue'
loBrForm.AriaForm1.grdDet.Column3.ControlSource = 'IssueList.NewValue'
loBrForm.AriaForm1.grdDet.Column3.DynamicBackColor ='IIF(IssueList.FieldDesc="Scale Bucket Count Conflict" OR IssueList.FieldDesc="Cost Sheet Conflict",RGB(255,0,0),RGB(255,255,255))'
loBrForm.AriaForm1.grdDet.Column2.DynamicBackColor ='IIF(IssueList.FieldDesc="Scale Bucket Count Conflict" OR IssueList.FieldDesc="Cost Sheet Conflict",RGB(255,0,0),RGB(255,255,255))'
loBrForm.AriaForm1.grdDet.Column1.DynamicBackColor ='IIF(IssueList.FieldDesc="Scale Bucket Count Conflict" OR IssueList.FieldDesc="Cost Sheet Conflict",RGB(255,0,0),RGB(255,255,255))'
loBrForm.AriaForm1.grdDet.Readonly =.t.
loBrForm.AriaForm1.grdHead.Readonly =.t.
SELECT IssueList
SET KEY TO StyleList.OldStyle+StyleList.NewStyle
LOCATE
************************************************************
*! Name      : lfAfterRowColHed
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/04/2016
*! Purpose   : Grid After row Col. change
************************************************************
FUNCTION lfAfterRowColHed
PARAMETERS loBrForm
SELECT IssueList
SET KEY TO StyleList.OldStyle+StyleList.NewStyle
LOCATE
loBrForm.AriaForm1.grdDet.Refresh()
************************************************************
*! Name      : lfAddAudit
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/04/2016
*! Purpose   : Call Audit trail function
************************************************************
FUNCTION lfAddAudit
PARAMETERS lcTable

lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydFiles where cfile_nam='"+PADR(lcTable,30)+"'",'',"sydFilestmp","",oAriaApplication.SystemConnectionString,3,;
  "",SET("Datasession" ))
IF lnRemResult>=1
  LOCATE
ENDIF
lcSydKey = ""
IF lnRemResult>=1 AND FOUND()
  IF !EMPTY(SYDFILESTMP.cNoteKey)
    lcSydKey = SYDFILESTMP.cNoteKey
  ENDIF
ENDIF
SELECT(lcTable)
lcSydKey = lfGetKey(lcSydKey)
lnFldResult = oAriaApplication.remotesystemdata.execute( ;
  "SELECT SYDFIELD.* FROM SYDFIELD,SYDFLFLD WHERE SYDFIELD.cfld_name == SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam == '" + ;
  ALLTRIM(UPPER(lcTable)) + "'", ;
  '', ;
  "sydfieldtmp", ;
  "", ;
  oAriaApplication.cAria4Sysfiles, ;
  3, ;
  "", ;
  SET("Datasession" ))
lcDbfName = ALLTRIM(UPPER(lcTable))


lcInform = ""
IF lnFldResult = 1
  SELECT SYDFIELDTMP
  LOCATE
*-- scan through the fields to get the values
  SCAN REST
    IF !(ALLTRIM(SYDFIELDTMP.CFld_name) $ "CADD_USER,CADD_TIME,DADD_DATE,LLOK_STAT"+;
        ",CLOK_USER,DLOK_DATE,CLOK_TIME")
      lValue = EVALUATE(lcDbfName+"."+ALLTRIM(SYDFIELDTMP.CFld_name))
      lcValue = ""
*-- convert the values of the fields to a character expression
      DO CASE
      CASE TYPE('lValue') = "C"
        lcValue = ALLTRIM(lValue)
      CASE TYPE('lValue') = "N"
        lcValue = ALLTRIM(STR(lValue))
      CASE TYPE('lValue') = "L"
        lcValue = IIF(lValue,"True","False")
      CASE TYPE('lValue') = "Y"
      CASE TYPE('lValue') $ "DT"
        lcValue = ALLTRIM(DTOC(lValue))
      ENDCASE
      lcInform = lcInform + ALLTRIM(SYDFIELDTMP.cFld_Head)+" = " +lcValue+CHR(13)+CHR(10)
    ENDIF
  ENDSCAN

ENDIF
*-- call the global function to add the audit trail record
*E611964,1 MMT 12/22/2019  Change size scales from one size scale to another [P20190901.0001][Start]
*=GFAUDTRL(.F. ,  "SMFLDCH" , lcSydKey , "SMFLDCH" , "UPDATE", lcInform)
=GFAUDTRL(.F. ,  IIF(ALLTRIM(UPPER(lcTable)) =="STYLE",'ICSTYLE', "SMFLDCH") , lcSydKey , "SMFLDCH" , "UPDATE", lcInform)
*E611964,1 MMT 12/22/2019  Change size scales from one size scale to another [P20190901.0001][End]
************************************************************
*! Name      : lfGetKey
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/04/2016
*! Purpose   : Get Key of Table
************************************************************
FUNCTION lfGetKey
PARAMETERS lcSydKey
lnStarPos = IIF(ATC('*',lcSydKey) <> 0 , ATC('*',lcSydKey) + 1 , 2)
lcKeyExp  = ALLTRIM(SUBSTR(lcSydKey , lnStarPos))
lnLastPos = ATC('|' , lcKeyExp)
IF lnLastPos <> 0
  lcKeyExp = SUBSTR(lcKeyExp , 1 , lnLastPos - 1)
ENDIF    && End of IF lnLastPos <> 0

*-- Get the Audit Trail Key Value
*E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL
*lcKey     = PADR(&lcKeyExp , 20)
lcKey     = PADR(&lcKeyExp , 40)
RETURN lcKey
************************************************************
*! Name      : lfAddAuditInfo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/04/2016
*! Purpose   : Add Audit trail
************************************************************
FUNCTION lfAddAuditInfo
PARAMETERS lcTranTmp,lcFile_Nam
lcFileToOpen = ''
DO case

CASE (lcFile_Nam ==  'ORDLINE')
  =gfOpenTable('ORDHDR','ORDHDR')
  lcFileToOpen = 'ORDHDR'
CASE (lcFile_Nam ==  'INVLINE')
  =gfOpenTable('INVHDR','INVHDR')
  lcFileToOpen = 'INVHDR'
CASE (lcFile_Nam ==  'RETLINE')
  =gfOpenTable('RETHDR','RETHDR')
  lcFileToOpen = 'RETHDR'
CASE (lcFile_Nam ==  'POSLN')
  =gfOpenTable('POSHDR','POSHDR')
  lcFileToOpen = 'RETHDR'
CASE (lcFile_Nam ==  'RALINE')
  =gfOpenTable('RETAUTH','RETAUTH')
  lcFileToOpen = 'RETAUTH'
ENDCASE
SELECT (lcTranTmp)
LOCATE
SCAN
  =gfSEEK(&lcTranTmp..cKey,lcFileToOpen)
  lfAddAudit(lcFileToOpen)
ENDSCAN
*E303688,1 MMT 06/28/2016 Modify Key change program to merge styles and colors[T20160626.0019][End]


*E611964,1 Es 10/30/2019 Change size scales from one size scale to another [Start]
*!************************************************************************
*! Name      : lfSaveMapping
*! Developer : Esraa Ahmed (ES)
*! Date      : 10/22/2019
*! Purpose   : Saving Mapping Sizes.
*!************************************************************************
FUNCTION lfSaveMapping
PARAMETERS laMappedSz, lanewscales

lcMappedSz=''
SET STEP ON
IF ALEN(laMappedSz,1)!=0
  lcMappedSz=ALLTRIM(loFormSet.lcNewVal)+"–"

  FOR i=1 TO ALEN(laMappedSz,1)
    IF i!=8
      lcMappedSz=ALLTRIM(lcMappedSz)+ALLTRIM(STR(i))+"|"
    ELSE
      lcMappedSz=ALLTRIM(lcMappedSz)+ALLTRIM(STR(i))
    ENDIF
  ENDFOR

  lcMappedSz=ALLTRIM(lcMappedSz)+'~'+ALLTRIM(loFormSet.lcOldVal)+'–'

  FOR i=1 TO ALEN(laMappedSz,1)
    IF i!=8
      lcMappedSz=ALLTRIM(lcMappedSz)+ALLTRIM(STR(laMappedSz[i,2]))+"|"
    ELSE
      lcMappedSz=ALLTRIM(lcMappedSz)+ALLTRIM(STR(laMappedSz[i,2]))
    ENDIF
  ENDFOR
ENDIF

IF !EMPTY(ALLTRIM(lcMappedSz))
  SELECT (loFormSet.lcValues)
  lcstatus=IIF(ALLTRIM(UPPER(cStatus)) ='A','A','M')
  REPLACE  cStatus WITH  lcstatus
  REPLACE MSIZES  WITH lcMappedSz
ENDIF

*!************************************************************************
*! Name      : lfOpMapScreen
*! Developer : Esraa Ahmed (ES)
*! Date      : 10/22/2019
*! Purpose   : Open Mapping Sizes Screen.
*!************************************************************************
FUNCTION lfOpMapScreen
SELECT (loFormSet.lcValues)
lcData=IIF(EMPTY(MSIZES),'',MSIZES)
lcRunScx = lfGetScx("SM\SMSZMAP.scx")
DO FORM (lcRunScx)  WITH lcData,ALLTRIM(loFormSet.lcNewVal),ALLTRIM(loFormSet.lcOldVal)

*!************************************************************************
*! Name      : lfCheckMapping
*! Developer : Esraa Ahmed (ES)
*! Date      : 11/05/2019
*! Purpose   : Check if all scales in grid has Mapping or not
*!************************************************************************
FUNCTION lfCheckMapping

IF RECCOUNT(loFormSet.lcValues) >0
  SELECT (loFormSet.lcValues)
  SCAN
    llMap=IIF(EMPTY(MSIZES),.F.,.T.)
    IF !llMap
      EXIT
    ENDIF
  ENDSCAN

  IF !llMap
    =gfModalGen("INM54059B00000" , "DIALOG")
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*!************************************************************************
*! Name      : lfUpdateNote
*! Developer : Esraa Ahmed (ES)
*! Date      : 11/05/2019
*! Purpose   : Update Note filed
*!************************************************************************
FUNCTION lfUpdateNote

IF RECCOUNT(loFormSet.lcValues) >0
  SELECT (loFormSet.lcValues)
  SCAN
  llselcritria=.F.
    IF !EMPTY(mSaveFilt)
      RESTORE FROM memo mSaveFilt ADDITIVE
    IF !EMPTY(ALLTRIM(laScopExpr[1]))
      FOR i=1 TO ALEN(laScopExpr,1)
        lcValue = laScopExpr[i,6]
        IF !EMPTY(lcValue)
          llselcritria=.T.
          EXIT
        ENDIF
      ENDFOR
      ELSE 
        llselcritria=.F.
    ENDIF
    ENDIF
    IF llselcritria
      SELECT (loFormSet.lcValues)
      lcstatus=IIF(ALLTRIM(UPPER(cStatus)) ='A','A','M')
      REPLACE  cStatus WITH  lcstatus
      REPLACE Note  WITH LANG_Note1
    ELSE
      REPLACE Note  WITH LANG_Note2
    ENDIF
  ENDSCAN
ENDIF

*!************************************************************************
*! Name      : lfHasData
*! Developer : Esraa Ahmed (ES)
*! Date      : 11/20/2019
*! Purpose   : Grid Has data or not
*!************************************************************************
FUNCTION lfHasData
SELECT (loFormSet.lcValues)
LOCATE FOR !Deleted()
IF  FOUND()
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!************************************************************************
*! Name      : lfUpdateStyleUPC
*! Developer : Esraa Ahmed (ES)
*! Date      : 11/20/2019
*! Purpose   : Update StyleUPC Table
*!************************************************************************
FUNCTION lfUpdateStyleUPC
PARAMETERS lcStyle
SET STEP ON
*Update StyleUPC Table.[Start]
SELECT STYLEUPC1
IF gfSEEK(PADR(lcStyle,19))
  CREATE CURSOR 'STYLEUPC_TMP' (STYLE C(19),SIZE C(2),UPC C(13))
  SELECT STYLEUPC1
  SCAN REST WHILE  STYLE+SIZE=PADR(lcStyle,19)
  lnindex=ASCAN(aOldSzValue,  ALLTRIM(STYLEUPC1.SIZE))
  IF lnindex !=0
    INSERT INTO 'STYLEUPC_TMP' VALUES (STYLEUPC1.STYLE,ALLTRIM(STR(lnindex)),STYLEUPC1.CUPCNUM1+STYLEUPC1.CUPCNUM2+STYLEUPC1.CUPCNUM3)
  ENDIF
  ENDSCAN
  SELECT 'STYLEUPC_TMP'
  LOCATE
  SCAN
  WAIT WINDOW LANG_WaitWindow+'STYLEUPC' NOWAIT
  SELECT STYLEUPC2
  IF gfSEEK(STYLEUPC_TMP.UPC)
      Replace STYLEUPC2.SIZE WITH STYLEUPC_TMP.SIZE
    =gfReplace('')
  ENDIF
  ENDSCAN
  IF USED('STYLEUPC_TMP')
    USE IN 'STYLEUPC_TMP'
  ENDIF
  SELECT STYLEUPC2
  =gfTableUpdate(.T.)
ENDIF
*!************************************************************************
*! Name      : lfUpdateBom
*! Developer : Esraa Ahmed (ES)
*! Date      : 11/20/2019
*! Purpose   : Update Bom Table
*!************************************************************************
FUNCTION lfUpdateBom
PARAMETERS lcStyle
SET STEP ON
*Update Bom Table.[Start]
lnMaj =LEN(gfItemMask('PM',ALLTRIM(loFormSet.laComp[loFormSet.lnComp,3])))
lcMaj = SUBSTR(lcStyle,1,lnMaj)
llUpdate=.F.
SELECT BOM_x

IF gfSEEK("0001"+lcMaj)
  SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)="0001"+lcMaj
  IF !EMPTY(msizes)
    WAIT WINDOW LANG_WaitWindow+'Bom' NOWAIT
    lcMaj1 = SUBSTR(citmmask,1,lnMaj)
    lcMaj2 = SUBSTR(citmmask,lnMaj+1,LEN(citmmask))
    IF '*'  $ lcMaj2
    llUpdate=.T.
    ELSE
    IF citmmask==lcStyle
      llUpdate=.T.
    ENDIF
    ENDIF

    IF llUpdate
    *Update msizes [Start]
    *@  ~2,3,5,6,8
    DIMENSION laMsizes[1]
    DIMENSION lasizes[1]
    lcMsizes=""

    =gfsubstr(msizes,@laMsizes,'~')
    laMsizes[1]=STRTRAN(laMsizes[1],ALLTRIM(lcOldScal),ALLTRIM(lcNewScal))
    lcMsizes=lcMsizes+laMsizes[1]+"~"

    =gfsubstr(laMsizes[2],@lasizes,',')
    FOR i=1 TO ALEN(lasizes)
      lnindex=ASCAN(aOldSzValue,  ALLTRIM(STR(VAL(lasizes[i]),1)))
      IF lnindex !=0
        lasizes[i]=STR(lnindex,1)
      ENDIF
    ENDFOR


    FOR i=1 TO ALEN(lasizes)
      IF i!=ALEN(lasizes)
       lcMsizes=lcMsizes+lasizes[i]+","
      ELSE
        lcMsizes=lcMsizes+lasizes[i]
      ENDIF
    ENDFOR

    =gfReplace("msizes WITH lcMsizes")
        *Update msizes [End]


    *Update mszcrosref [Start]
    
    IF !EMPTY(mszcrosref)
        DIMENSION   lamszcrosref[1]
      =gfSubStr(mszcrosref,@lamszcrosref,CHR(13))
*@  ,2~3  ,2
*@  ,3~3  ,1
*@  ,5~3  ,3
*@  ,6~3  ,3
*@  ,8~3  ,3
      DIMENSION lamsz[1]
      DIMENSION lamsize[1]

      lcmszcrosref=""
      FOR i=1 TO ALEN(lamszcrosref)
      =gfSubStr(lamszcrosref[i],@lamsz,',')
      lamsz[1]=STRTRAN(lamsz[1],ALLTRIM(lcOldScal),ALLTRIM(lcNewScal))
      lcmszcrosref=lcmszcrosref+lamsz[1]+","

      =gfSubStr(lamsz[2],@lamsize,'~')
      lnindex=ASCAN(aOldSzValue, ALLTRIM(lamsize[1]))
      IF lnindex !=0
        lamsize[1]=STR(lnindex,1)
        ENDIF
            lcmszcrosref=lcmszcrosref+lamsize[1]+"~"+lamsize[2]+","+lamsz[3]+CHR(13)
      ENDFOR
        =gfReplace("mszcrosref WITH lcmszcrosref")
      ENDIF
        *Update mszcrosref [End]
      ENDIF
    ENDIF
  ENDSCAN
  =gfTableUpdate(.T.)
  llUpdate=.F.
ENDIF 

SELECT BOM_y
IF gfSEEK("0001"+lcMaj)
  SCAN REST WHILE CINVTYPC+ITEM+TYP+CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+CITMMASK="0001"+lcMaj AND !EMPTY(mszcrosref) &&AND !EMPTY(msizes)
  lcMaj2 = SUBSTR(item,lnMaj+1,LEN(item))
  IF '*'  $ lcMaj2 
    llUpdate=.T.
  ELSE
    IF citmmask==lcStyle
    llUpdate=.T.
    ENDIF
  ENDIF
  IF  llUpdate
      *Update mszcrosref [Start]
    DIMENSION lamszcrosref[1]
    = gfSubStr(mszcrosref,@lamszcrosref,CHR(13))
*(  ,1~(  ,2 
*(  ,2~(  ,6  
*(  ,3~(  ,6  
*(  ,4~(  ,8  
*(  ,8~(  ,8  
*(  ,6~(  ,4  
*(  ,7~(  ,4 
    DIMENSION lamsz[1]
    DIMENSION lamsize[1]
    lcmszcrosref=""
      FOR i=1 TO ALEN(lamszcrosref)
*(  ,1~(  ,2 
    =gfSubStr(lamszcrosref[i],@lamsz,',')
    =gfSubStr(lamsz[2],@lamsize,'~')
*1~(  
    lamsize[2]=STRTRAN(lamsize[2],ALLTRIM(lcOldScal),ALLTRIM(lcNewScal))
    lnindex=ASCAN(aOldSzValue,ALLTRIM( lamsz[3]))
    IF lnindex !=0
      lamsz[3]=STR(lnindex,1)
    ENDIF
    lcmszcrosref=lcmszcrosref+lamsz[1]+","+lamsize[1]+"~"+lamsize[2]+","+lamsz[3]+CHR(13)
    ENDFOR
*Update mszcrosref [End]

      =gfReplace("mszcrosref WITH lcmszcrosref")
  ENDIF
  ENDSCAN
  =gfTableUpdate(.T.)
*Update Bom Table.[End]

ENDIF
*E611964,1 Es 11/24/2019  Change size scales from one size scale to another [End]



*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][Start]
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!B608130
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'ROYALTY'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYCLR2'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0



ENDCASE
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
*B612046,1 Es 02/19/2020 Error in Key change screen while filtering style by Style group if the number of selected groups is more than 24 and the Fabric and locations options not working [T20200202.0002][End]

