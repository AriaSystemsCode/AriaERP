*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMACCOD.Prg
*:  Module      : System Manager
*:  Desc.       : Account code Structure
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 11/20/2012
*:  Reference   : *E303296
*:************************************************************************
Note  : I created the screen as Branch ( Modal )  screen , there is no meaning to make it anymode else
*:************************************************************************
*!B610258,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031]
*!B610270,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031][Start]
*:************************************************************************
#INCLUDE R:\ARIA4XP\PRGS\SM\smaccod.H
PARAMETERS pcComp_ID

*- Get the screen , call it
lcRunScx = lfGetScx("SM\SMACCOD.scx")
DO FORM (lcRunScx) WITH pcComp_ID

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

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName','SMACCOD')

*!*	*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*!*	*- Open tables
*!*	=lfOpenPRGFILES(loFormSet.lcProgName)

*!B610258,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031][Start]
*=gfOpenFile(oAriaApplication.SysPath+'SYCACCOD','ACCSEGNO','SH')
*!B610258,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031][End]

=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYDFIELD','CFLD_NAME','SH')

IF EMPTY(loFormSet.pcComp_ID)
  *** See if there is any companies available in the system. ***
  SELECT SYCCOMP
  GO TOP
  IF EOF()
    *** No companies available. ***
    *** <  Ok  > ***
    *N000682,1 04/16/2013 THB Globlization changes[Start]
*    =gfModalGen("TRM00189B00000","DIALOG","companies")
     *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00189B00000","DIALOG",LANG_companies)
=gfModalGen("TRM00189B00000","DIALOG",LANG_companies)
*N000682,1 11/20/2012 MMT Globlization changes[End]

     *N000682,1 04/16/2013 THB Globlization changes[END]
    glQuitting = .T.
    RETURN .F.
  ENDIF
ENDIF

*- Define needed variables.
=lfDefineVars(loFormSet)

=lfSetControlSource(loFormSet)

*- Initialize array
WITH loFormSet
  .laData[1] = 0
  .laData[2] = ''
  .laData[3] = ''
  .laData[4] = 0
ENDWITH


=lfwData_4(loFormSet)
loFormSet.AriaForm1.Refresh()
*- End of lfFormInit.

************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/20/2012
*! Purpose   : Destroy Form method
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet
lcAccExct  = loFormSet.lcAccExct
SET EXACT &lcAccExct

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
IF USED('SYDFIELD')
  USE IN SYDFIELD
ENDIF
IF USED('SYCACCOD')
  USE IN SYCACCOD
ENDIF

*- End of lfFormDestroy.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/23/2012
*! Purpose   : Define screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

*Old: DECLARE laObjStat[6]  && Array to hold the display status of each seg.
*Old: DECLARE laObjdisp[6]  && Array to control the display of seg. Des. in Child.

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	loFormSet.Ariaform1.Caption = 'Account Code Structure'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.Caption = LANG_ACC_CODE_STR
loFormSet.Ariaform1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACC_CODE_STR,loFormSet.GetHeaderText("LANG_ACC_CODE_STR",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[END  ]

lcScFields = 'NACSSEGSZ,CACSMASK,CACSEGDES,NACSNOSEG'
loFormSet.AddProperty('lcScFields',lcScFields)

lcCnt = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcCnt.]',' ')

loFormSet.AddProperty('laAcDes[6,3]','')  && Array to hold the description of each seg.
*Old: loFormSet.laAcDes    = ' '        && Default blank to seg description array

*** Default value of all variables holding the seg. width. ***
lcVars = 'lnSeg1,lnSeg2,lnSeg3,lnSeg4,lnSeg5,lnSeg6'
=lfAddProp(loFormSet,lcVars,0)

*** Default value of all variables holding the old seg. width. ***
lcVars = 'lnOldSeg1,lnOldSeg2,lnOldSeg3,lnOldSeg4,lnOldSeg5,lnOldSeg6,lnOldSNo'
=lfAddProp(loFormSet,lcVars,0)

loFormSet.AddProperty('lnBoxh'     , 16  )       && Max size of box in Child screen is 16
loFormSet.AddProperty('lnSetCostC' , 0   )

loFormSet.AddProperty('lcOldVal'   , ''  )       && Hold the old value of any object in the screen
loFormSet.AddProperty('lcGlSetup'  , gfTempName()  )      && Variable hold the temp alias for "GLSETUP"

loFormSet.AddProperty('llAcsUsed'  , .F. )
loFormSet.AddProperty('llCopyFlg'  , .F. )       && Flag to know copy or not.
loFormSet.AddProperty('laObjdisp[6]',.T. )      && Default Display all lines in child screen
loFormSet.AddProperty('llCancel'   , .F. )       && Flag to indicate if there is a canslation in
                        && the child screen
loFormSet.AddProperty('llChange'   , .F. )       && Check if any change in the data
                        && the platform is DOS
*loFormSet.AddProperty('ibCompany'  , 1   )
loFormSet.AddProperty('puComp'     , " " )
*loFormSet.AddProperty('puComp_ID'  , " " )
loFormSet.AddProperty('lcCompany'  , " " )
loFormSet.AddProperty('lcComp_Id'  , " " )
*loFormSet.AddProperty('lcOldComp'  , " " )
loFormSet.AddProperty('lcDataDir'  , " " )        && Hold the company setup dir.
loFormSet.AddProperty('llCompPrnt' , .T. )

*loFormSet.AddProperty('llNoContrl' , .T. )
loFormSet.AddProperty('lnOldSize' , ''  )        && Old total size
loFormSet.AddProperty('lcOldMask' , ''  )        && Old Account Mask
loFormSet.AddProperty('lcOldDes'  , ''  )        && Old Account segments desciption
loFormSet.AddProperty('lcOldNoSeg', ''  )        && Old No of segments

loFormSet.AddProperty('lsCompany', 1  )        && Select a company to copy from
loFormSet.AddProperty('llComIdFlg', .F.  )


loFormSet.AddProperty('lcAccExct'  , SET("EXACT") )
SET EXACT OFF

*Old: IF !gfSetup()
*Old:   RETURN
*Old: ENDIF

loFormSet.AddProperty('laCompany[1,2]'  )        && List of Companies
SELECT SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME,SYCCOMP.CCOMP_ID ;
FROM SYCCOMP ORDER BY SYCCOMP.CCOMP_ID INTO ARRAY loFormSet.laCompany

loFormSet.AddProperty('laCopyCom[1,2]'  )        && List of Companies
loFormSet.AddProperty('laCompInfo[1,3]'  )        && List of Companies



*** To know if the program called from the menu ***
*** or called from another program
IF !EMPTY(loFormSet.pcComp_ID)
  loFormSet.lcComp_ID  = loFormSet.pcComp_ID
  *loFormSet.lcCompany  = loFormSet.lcComp_ID+" "+LOOKUP(SYCCOMP.cCom_name,loFormSet.lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
  loFormSet.llComIdFlg = .T.
  *loFormSet.puComp     = loFormSet.lcComp_Id+" "+LOOKUP(SYCCOMP.cCom_name,loFormSet.lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
  lcCompStat = "DISABLE"
  lcData_4   = 'ENABLE'

  =lfvComp(loFormSet,loFormSet.Ariaform1.puComp)

ELSE
  loFormSet.lcComp_Id  = " "
  loFormSet.lcCompany  = " "
  loFormSet.llComIdFlg = .F.
  lcCompStat = "ENABLE"
  lcData_4   = 'DISABLE'
ENDIF

WITH loFormSet.Ariaform1
  .puComp.Enabled = lcCompStat = "ENABLE"
  .laData4.Enabled = lcData_4   = 'ENABLE'
ENDWITH


************************************************************
*! Name      : lfSetControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/07/2012
*! Purpose   : Set ControlSource for the screen controls
************************************************************
FUNCTION lfSetControlSource
PARAMETERS loFormset
*- fields are ordered according to their appearence in the screen
WITH loFormSet.Ariaform1
  .puComp.RowSource = 'Thisformset.laCompany'
  .puComp.ControlSource = 'Thisformset.lcComp_Id'

  .lnSeg1.txtSeg.ControlSource = 'Thisformset.lnSeg1'
  .lnSeg2.txtSeg.ControlSource = 'Thisformset.lnSeg2'
  .lnSeg3.txtSeg.ControlSource = 'Thisformset.lnSeg3'
  .lnSeg4.txtSeg.ControlSource = 'Thisformset.lnSeg4'
  .lnSeg5.txtSeg.ControlSource = 'Thisformset.lnSeg5'
  .lnSeg6.txtSeg.ControlSource = 'Thisformset.lnSeg6'

  .lnSeg1.txtSeg.InputMask = '99'
  .lnSeg2.txtSeg.InputMask = '99'
  .lnSeg3.txtSeg.InputMask = '99'
  .lnSeg4.txtSeg.InputMask = '99'
  .lnSeg5.txtSeg.InputMask = '99'
  .lnSeg6.txtSeg.InputMask = '99'

  .laData1.ControlSource = 'Thisformset.laData[1]'
  .laData2.ControlSource = 'Thisformset.laData[2]'
  .laData3.ControlSource = 'Thisformset.laData[3]'
  .laData4.ControlSource = 'Thisformset.laData[4]'

  .lcOldDes.ControlSource = 'Thisformset.lcOldDes'
  .lcOldMask.ControlSource = 'Thisformset.lcOldMask'
  .lnOldSize.ControlSource = 'Thisformset.lnOldSize'

ENDWITH
*- End of lfSetControlSource


*!**************************************************************************
*!
*!      Function: lfvComp
*!
*!**************************************************************************
FUNCTION lfvComp
PARAMETERS loFormSet,loFld

=LOOKUP(SYCCOMP.cCom_name,loFormSet.lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')

*** If any change in the data
IF loFormSet.llChange
  *** Do you want to save the change. ***
  *** < Save > - < NO > - < Cancel > ****
  lnOption = gfModalGen("QRM00153B00025","DIALOG")
  DO CASE
    CASE lnOption = 1
      *** If no segments description while saving
      IF !lfSaveData(loFormSet)
        loFormSet.lcComp_Id = loFld.OldValue
        *loFormSet.puComp    = loFormSet.lcComp_Id+" "+LOOKUP(SYCCOMP.cCom_name,loFormSet.lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
      ELSE
        *loFormSet.puComp     = SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME
        *loFormSet.lcComp_Id  = SYCCOMP.CCOMP_ID
        loFormSet.lcDataDir  = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
        loFormSet.llCompPrnt = IIF(EMPTY(SYCCOMP.CCOMPPRNT),.T.,.F.)
        loFormSet.llChange   = .F.
      ENDIF
    CASE lnOption = 2
      *loFormSet.puComp      = SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME
      *loFormSet.lcComp_Id   = SYCCOMP.CCOMP_ID
      loFormSet.lcDataDir   = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
      loFormSet.llCompPrnt  = IIF(EMPTY(SYCCOMP.CCOMPPRNT),.T.,.F.)
      loFormSet.llChange    = .F.
    CASE lnOption = 2
      loFormSet.lcComp_Id   = loFld.OldValue
      *loFormSet.puComp      = loFormSet.lcComp_Id+" "+LOOKUP(SYCCOMP.cCom_name,loFormSet.lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
  ENDCASE
ELSE
  *loFormSet.puComp      = SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME
  *loFormSet.lcComp_Id   = SYCCOMP.CCOMP_ID
  loFormSet.lcDataDir   = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
  loFormSet.llCompPrnt  = IIF(EMPTY(SYCCOMP.CCOMPPRNT),.T.,.F.)
ENDIF

IF FILE(loFormSet.lcDataDir+"GLSETUP.DBF")
  lcGlSetup = loFormSet.lcGlSetup
  SELECT 0
  USE (loFormSet.lcDataDir+'GLSETUP') AGAIN ALIAS &lcGlSetup
  SELECT (lcGlSetup)
  GO TOP
  loFormSet.lnSetCostC = IIF(EMPTY(&lcGlSetup..nSetcostc),0,&lcGlSetup..nSetcostc)
  USE IN &lcGlSetup.
ELSE
  loFormSet.lnSetCostC = 0
ENDIF
IF !EMPTY(loFld.OldValue) AND USED('SYCACCOD')
  SELECT SYCACCOD
  IF SEEK("")

    =gfObj_Lock(.F.)
  ENDIF
ENDIF

IF !lfOpenCmpData(loFormSet)
  RETURN
ENDIF

*E303296 TMI 11/20/2012 [Start]
lcScFields = loFormSet.lcScFields
lcData_4 = ' '
*E303296 TMI 11/20/2012 [End  ]

IF SEEK("")

  IF gfObj_Lock(.T.)
    SCATTER FIELDS &lcScFields TO loFormSet.laData
    loFormSet.llAcsUsed = lAcsUsed

    *** Get Short and long description for all segment in array
    FOR lnCount = 1 TO loFormSet.laData[4]
      loFormSet.laAcDes [lnCount,1] = gfSubStr(loFormSet.laData[3],lnCount,'-')
      IF !EOF()
        SKIP
        loFormSet.laAcDes [lnCount,2] = cAcsLgDes
      ENDIF
    ENDFOR

    *** Store old valus in variables
    loFormSet.lnOldSize = loFormSet.laData[1]          && Old total size
    loFormSet.lcOldMask = loFormSet.laData[2]          && Old Account Mask
    loFormSet.lcOldDes  = loFormSet.laData[3]          && Old Account segments desciption
    loFormSet.lcOldNoSeg= loFormSet.laData[4]          && Old No of segments

    *** Collect the no. and size of segments from the old mask
    lnNext = 0
    FOR lnCount  = 1 TO 6
      *lcObjName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
      lnCrurnt   = lnNext
      lnNext     = IIF(AT('-',loFormSet.laData[2],lnCount)=0,LEN(ALLTRIM(loFormSet.laData[2]))+1,;
                       AT('-',loFormSet.laData[2],lnCount))
      lnWidth    = MAX(0,lnNext-lnCrurnt-1)
      *&lcObjName = lnwidth
      lcI = STR(lnCount,1)
      loFormSet.lnSeg&lcI = lnwidth
    ENDFOR

    *** Check if this file is empty or not.  If it's empty, ***
    *** the user can copy from another company ***
    IF loFormSet.llAcsUsed
      loFormSet.llCopyFlg = .F.
      lcData_4  = 'DISABLE'
    ELSE
      loFormSet.llCopyFlg = .T.
      lcData_4  = 'ENABLE'
    ENDIF
  ELSE
    SCATTER FIELDS &lcScFields TO loFormSet.laData BLANK
    lcData_4     = 'ENABLE'
    loFormSet.llCopyFlg    = .T.
    loFormSet.llAcsUsed    = .F.
    *** Store old valus in variables
    loFormSet.lnOldSize    = loFormSet.laData[1]          && Old total size
    loFormSet.lcOldMask    = loFormSet.laData[2]          && Old Account Mask
    loFormSet.lcOldDes     = loFormSet.laData[3]          && Old Account segments desciption
    loFormSet.lcOldNoSeg   = loFormSet.laData[4]          && Old No of segments
    FOR lnCount  = 1 TO 6
      *lcObjName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
      *&lcObjName = 0
      lcI = STR(lnCount,1)
      loFormSet.lnSeg&lcI. = 0
    ENDFOR
  ENDIF
ELSE
  SCATTER FIELDS &lcScFields TO loFormSet.laData BLANK
  lcData_4 = 'ENABLE'
  loFormSet.llCopyFlg = .T.
  loFormSet.llAcsUsed = .F.
  *** Store old valus in variables
  loFormSet.lnOldSize = loFormSet.laData[1]          && Old total size
  loFormSet.lcOldMask = loFormSet.laData[2]          && Old Account Mask
  loFormSet.lcOldDes  = loFormSet.laData[3]          && Old Account segments desciption
  loFormSet.lcOldNoSeg= loFormSet.laData[4]          && Old No of segments
  FOR lnCount  = 1 TO 6
    *lcObjName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
    *&lcObjName = 0
    lcI = STR(lnCount,1)
    loFormSet.lnSeg&lcI. = 0
  ENDFOR
ENDIF

=lfvData_4(loFormset)
=lfComShow(loFormset)


loFormSet.AriaForm1.Refresh()

*Old: IF _WINDOWS
*  DEACTIVATE POPUP puComp_Id
*Old: ENDIF
*-End of lfvComp

*!**************************************************************************
*!
*!      Function: lfComShow
*!
*!**************************************************************************
*
FUNCTION lfComShow
PARAMETERS loFormSet

IF loFormSet.llCompPrnt .OR. loFormSet.llAcsUsed
  loFormSet.Ariaform1.laData4.Enabled = lcData_4  = 'ENABLE'

  *IF loFormSet.llCopyFlg
  *ELSE
  *ENDIF
  loFormSet.Ariaform1.pbCopy.Enabled = loFormSet.llCopyFlg

  loFormSet.Ariaform1.pbDiscrip.Enabled = .T.

  FOR lnCount  = 1 TO 6
    *lcObjName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
    lcI = STR(lnCount,1)
    loFormSet.Ariaform1.lnSeg&lcI..txtSeg.Enabled = lcData_4 = 'ENABLE'
  ENDFOR

  *IF !loFormSet.llChange
  *ELSE
  *ENDIF
  loFormSet.Ariaform1.pbSave.Enabled = loFormSet.llChange
ELSE
  WITH loFormSet.Ariaform1
    .puComp.Enabled = .T.
    .pbClose.Enabled = .T.
    .pbDiscrip.Enabled = .T.
  ENDWITH

ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_4
*!
*!**************************************************************************
* Valid of all segments :Calculat the new mask and description and show
* the objects enable or disable
*
FUNCTION lfwData_4
PARAMETERS loFormSet

loFormSet.lnOldSNo = loFormSet.laData[4]

FOR lnCount = 1 TO 6
  *lcObjName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
  *&lcObjName = IIF(lnCount<=loFormSet.laData[4],IIF(EMPTY(&lcObjName),3,&lcObjName),0)
  *lcObjStat  = "DISABLE"
  loFormSet.laObjdisp [lnCount] =IIF(lnCount<=loFormSet.laData[4],.T.,.F.)
  lcI = STR(lnCount,1)
  *lnSegVal = loFormSet.Ariaform1.lnSeg&lcI..txtSeg.Value
  lnSegVal = loFormSet.lnSeg&lcI
  loFormSet.Ariaform1.lnSeg&lcI..txtSeg.Value = IIF(lnCount<=loFormSet.laData[4],IIF(EMPTY(lnSegVal),3,lnSegVal),0)
  loFormSet.Ariaform1.lnSeg&lcI..txtSeg.Enabled = .F.
ENDFOR

*!**************************************************************************
*!
*!      Function: lfvData_4
*!
*!**************************************************************************
* Valid of all segments :Calculat the new mask and description and show
* the objects enable or disable
*
FUNCTION lfvData_4
PARAMETERS loFormSet

IF loFormSet.llComIdFlg
  loFormSet.llCopyFlg    = .T.
  loFormSet.llAcsUsed    = .F.
  WITH loFormSet.Ariaform1
    .pbDiscrip.Enabled = .T.
    .pbCopy.Enabled = .T.
    .pbSave.Enabled = .T.
  ENDWITH
ENDIF

IF !EMPTY(loFormSet.laData[4]) &&.AND. LASTKEY() = 13
  IF loFormSet.laData[4] < loFormSet.lnSetCostC .AND. loFormSet.lnSetCostC > 0
    *** You have been setup your cost ***
    *** center on segment no. ð.
    *** To decrease the no. of segment, ***
    *** you  have to adjust  your  cost ***
    *** center first. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00173B00000","DIALOG",ALLTRIM(STR(loFormSet.lnOldSNo)))
    loFormSet.laData[4] = loFormSet.lnOldSNo
    RETURN
  ENDIF
ENDIF

FOR lnCount = 1 TO 6
  *lcObjName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
  *&lcObjName = IIF(lnCount<=loFormSet.laData[4],IIF(EMPTY(&lcObjName),3,&lcObjName),0)
  lcObjStat  = IIF(!loFormSet.llAcsUsed,IIF(lnCount<=loFormSet.laData[4],"ENABLE","DISABLE"),;
               IIF(lnCount>loFormSet.lcOldNoSeg .AND. lnCount<=loFormSet.laData[4] ,"ENABLE",;
               "DISABLE"))
  loFormSet.laObjdisp [lnCount] =IIF(lnCount<=loFormSet.laData[4],.T.,.F.)
  lcI = STR(lnCount,1)
  *lnSegVal = loFormset.AriaForm1.lnSeg&lcI..txtSeg.Value
  lnSegVal = loFormSet.lnSeg&lcI
  loFormset.AriaForm1.lnSeg&lcI..txtSeg.Value = IIF(lnCount<=loFormSet.laData[4],IIF(EMPTY(lnSegVal),3,lnSegVal),0)
  loFormset.AriaForm1.lnSeg&lcI..txtSeg.Enabled = lcObjStat = 'ENABLE'
ENDFOR

*** Redifine the variable used to control the display of the child screen
loFormSet.lnBoxh = (loFormSet.laData[4] * 2) + 5
=lfTotalSiz(loFormSet)


*!**************************************************************************
*!
*!      Function: lfTotalSiz
*!
*!**************************************************************************
*
FUNCTION lfTotalSiz
PARAMETERS loFormSet

loFormSet.laData[1]   = MAX(loFormSet.laData[4]-1,0)
loFormSet.laData[2]   = ''
loFormSet.laData[3]   = ''
FOR lnCount = 1 TO loFormSet.laData[4]
  lnVarName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
  loFormSet.laData[1]  = loFormSet.laData[1] + EVALUATE(lnVarName)
  loFormSet.laData[2]  = loFormSet.laData[2] +IIF(EMPTY(loFormSet.laData[2]),'','-')+;
               REPLICATE('#',EVALUATE(lnVarName))
  loFormSet.laData[3]  = loFormSet.laData[3] +IIF(EMPTY(loFormSet.laData[3]),'','-')+;
               IIF(LEN(ALLTRIM(loFormSet.laAcDes[lnCount,1]))>EVALUATE(lnVarName),;
               LEFT(ALLTRIM(loFormSet.laAcDes[lnCount,1]),EVALUATE(lnVarName)),;
               ALLTRIM(loFormSet.laAcDes[lnCount,1])+;
               SPACE(EVALUATE(lnVarName)-LEN(ALLTRIM(loFormSet.laAcDes[lnCount,1]))))
ENDFOR
loFormSet.Ariaform1.Refresh()
*!*	IF WREAD(lcBaseWind)
*!*	ENDIF

*!**************************************************************************
*!
*!      Function: lfvDiscrip
*!
*!**************************************************************************
* Branch to segments descriptions screen
*
FUNCTION lfvDiscrip
PARAMETERS loFormSet

loFormSet.llCancel = .F.
LOCAL laDesc[ALEN(loFormset.laAcDes,1),ALEN(loFormset.laAcDes,2)]
=ACOPY(loFormset.laAcDes,laDesc)

DO FORM (oAriaApplication.ScreenHome + oAriaApplication.ActiveModuleID + '\SMACDES.SCX') WITH loFormSet
loFormSet.llChange = loFormSet.llChange OR !loFormSet.llCancel

IF loFormSet.llCancel
  =ACOPY(laDesc,loFormset.laAcDes)
ENDIF
IF loFormSet.llChange
  loFormSet.Ariaform1.pbSave.Enabled = .T.
ENDIF



*!**************************************************************************
*!
*!      Function: lfvSave
*!
*!**************************************************************************
* On termination of the home screen save data to master file
*
FUNCTION lfvSave
PARAMETERS loFormSet

IF loFormSet.llChange
 =lfSaveData(loFormSet)
ENDIF

*!**************************************************************************
*!
*!      Function: lfvClose
*!
*!**************************************************************************
* On termination of the home screen save data to master file
*
FUNCTION lfvClose
PARAMETERS loFormSet

IF loFormSet.llChange
  lnOption = gfModalGen("QRM00153B00025","DIALOG")
  DO CASE
    CASE lnOption = 1
      IF lfSaveData(loFormSet)
        loFormSet.llChange = .F.
      ENDIF
    CASE lnOption = 2
      loFormSet.llChange = .F.
  ENDCASE
ENDIF

IF !EMPTY(loFormSet.lcComp_id) AND USED("SYCACCOD")

  IF SEEK("","SYCACCOD") .AND. !EMPTY(SYCACCOD.nacssegsz)

    SELECT SYCACCOD
    =gfObj_Lock(.F.)
  ENDIF
ENDIF

IF !loFormSet.llChange
  *CLEAR READ
  loFormSet.Release
ENDIF

*!**************************************************************************
*!
*!      Function: lfSegDes
*!
*!**************************************************************************
*
* Function used in the picture of all short descriptions to define how manay
* Characters to be accepted in each field
*
FUNCTION lfSegDes
PARAMETERS lnWidth

RETURN REPLICATE("N",lnWidth)

*!**************************************************************************
*!
*!      Function: lfvCopyACD
*!
*!**************************************************************************
*
FUNCTION lfvCopyACD
PARAMETERS loFormSet

_TALLY = 0
SELECT cComp_ID,cCom_Name,cCom_DDir," " ;
       FROM (oAriaApplication.SysPath+"SYCCOMP") ;
       WHERE SYCCOMP.cComp_ID <> loFormSet.lcComp_Id .AND. ;
             IIF(EMPTY(CCOM_DDIR),.F.,lfCheckDir(CCOM_DDIR,.F.)) .AND. ;
             IIF(EMPTY(CCOMPPRNT),.T.,.F.) ;
       ORDER BY cCom_Name ;
       INTO ARRAY loFormSet.laCopyCom

IF _TALLY > 0
  *** Valid function of the push button ***
  *** <copy from another company> ...
  DECLARE loFormSet.laCompInfo[ALEN(loFormSet.laCopyCom,1),3]
  loFormSet.laCompInfo = " "
  SELECT SYCACCOD
  FOR lnCount = 1 TO ALEN(loFormSet.laCopyCom,1)

    =lfCheckDir(loFormSet.laCopyCom[lncount,3],.T.,lnCount)

  ENDFOR

  *** Select company to copy its account code struc. ***
  DO FORM (oAriaApplication.ScreenHome + oAriaApplication.ActiveModuleID + '\SMSELCM.scx') WITH loFormSet

  *** Refresh all the data has been collected from another company. ***
  =lfvData_4(loFormset)
  IF loFormSet.llChange
    loFormSet.Ariaform1.pbSave.Enabled = .T.
  ENDIF
ELSE
  *** No company available. ***
  *** <  Ok  > ***
  *N000682,1 04/16/2013 THB Globlization changes[Start]
  *=gfModalGen("TRM00189B00000","DIALOG","companies")
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00189B00000","DIALOG",LANG_companies)
=gfModalGen("TRM00189B00000","DIALOG",LANG_companies)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 THB Globlization changes[END]
ENDIF

*!**************************************************************************
*!
*!      Function: lfCheckDir
*!
*!**************************************************************************
*
FUNCTION lfCheckDir
PARAMETERS lcComp_path,llExcloded,lnArrRow

PRIVATE lcAlias,llFound
llFound = .F.
lcAlias = gfTempName()
lcComp_path = ALLT(lcComp_path)
lcSetfull = SET('FULL')
SET FULLPATH ON
IF FILE(lcComp_path+'ACCOD.DBF')
  SELECT 0
  USE (lcComp_path+'ACCOD.DBF') AGAIN ALIAS &lcAlias ORDER TAG Accsegno
  LOCATE
  llFound = Found()
  IF llExcloded
    *- select the correct value to seek with
    IF SEEK('0')
      loFormSet.laCopyCom [lnArrRow,4] = CACSEGDES
      loFormSet.laCompInfo[lnArrRow,1] = loFormSet.laCopyCom[lnArrRow,1]
      loFormSet.laCompInfo[lnArrRow,2] = loFormSet.laCopyCom[lnArrRow,2]
      loFormSet.laCompInfo[lnArrRow,3] = loFormSet.laCopyCom[lnArrRow,4]
    ENDIF

  ENDIF
  USE IN (lcAlias)
ENDIF
SET FULL &lcSetfull
RETURN (llFound .OR. llExcloded)

*!**************************************************************************
*!
*!      Function: lfvCompany
*!
*!**************************************************************************
*
FUNCTION lfvCompany
PARAMETERS loSelFormSet
loFormSet = loSelFormSet.loFormSet

*** Valid function of the list & select push button. ***

lcScFields = loFormSet.lcScFields

lcAccFields = STRTRAN(UPPER(lcScFields),'SYCACCOD.')
lcAlias = gfTempName()
SELECT 0
USE (ALLT(loFormSet.laCopyCom[loFormSet.lsCompany,3])+'ACCOD.DBF') AGAIN ALIAS &lcAlias ORDER TAG Accsegno

SELECT (lcAlias)

=SEEK("")

SCATTER FIELDS &lcAccFields TO loFormSet.laData
loFormSet.llAcsUsed = lAcsUsed

*** Get Short and long description for all segment in array
FOR lnCount = 1 TO loFormSet.laData[4]
  loFormSet.laAcDes [lnCount,1] = gfSubStr(loFormSet.laData[3],lnCount,'-')
  IF !EOF()
    SKIP
    loFormSet.laAcDes [lnCount,2] = cAcsLgDes
  ENDIF
ENDFOR

*** Collect the no and size of segments from the old mask
lnNext =0
FOR lnCount  = 1 TO 6
  lcObjName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
  lnCrurnt   = lnNext
  lnNext     = IIF(AT('-',loFormSet.laData[2],lnCount)=0,LEN(ALLTRIM(loFormSet.laData[2]))+1,;
                 AT('-',loFormSet.laData[2],lnCount))
  lnwidth    = MAX(0,lnNext-lnCrurnt-1)
  *&lcObjName = lnwidth
  lcI = STR(lnCount,1)
  loFormSet.lnSeg&lcI = lnWidth
ENDFOR
USE IN (lcAlias)
SELECT SYCACCOD
loFormSet.llChange = .T.
loFormSet.AriaForm1.Refresh()
loSelFormSet.Release

*!**************************************************************************
*!
*!      Function: lfSaveData
*!
*!**************************************************************************
*
FUNCTION lfSaveData
PARAMETERS loFormSet

SELECT SYCACCOD

lcScFields = loFormSet.lcScFields

IF loFormSet.laData[4] > 0
  *** This validate the  segments short  descriptions if it's ***
  *** empty or not, If it's empty, we force the user to enter ***
  *** the descriptions or accept default...
  llClrRead = .T.
  FOR lnCount = 1 TO loFormSet.laData[4]
    IF EMPTY(loFormSet.laAcDes[lnCount,1]) .OR. EMPTY(loFormSet.laData[3])
      *** You have to add segment short  descriptions ***
      *** or the system will add default descriptions ***
      *** <Enter descriptions> - <Add defaults> ***
      lnOption = gfModalGen("TRM00174B00029","DIALOG")
      DO CASE
        CASE lnOption = 1
          llClrRead = .F.
          FOR lnCont = 1 TO ALEN(loFormSet.laAcDes,1)
            loFormSet.laAcDes [lnCont,1] = " "
            loFormSet.laAcDes [lnCont,2] = " "
          ENDFOR
          *_CUROBJ = OBJNUM(loFormSet.laAcDes[1,1])
          EXIT
        CASE lnOptioN = 2
          llClrRead = .F.
          FOR lnCont = 1 TO ALEN(loFormSet.laAcDes,1)
            loFormSet.laAcDes [lnCont,1] = "Sg" + ALLTRIM(STR(lnCont))
            loFormSet.laAcDes [lnCont,2] = "Default"
          ENDFOR
          EXIT
      ENDCASE
    ENDIF
  ENDFOR

  IF !llClrRead

    DO FORM (oAriaApplication.ScreenHome + oAriaApplication.ActiveModuleID + '\SMACDES.scx') WITH loFormSet

    IF loFormSet.llCancel
      FOR lnCont = 1 TO ALEN(loFormSet.laAcDes,1)
        loFormSet.laAcDes [lnCont,1] = " "
        loFormSet.laAcDes [lnCont,2] = " "
      ENDFOR
      loFormSet.llCancel = .F.
    ENDIF
    RETURN .F.
  ENDIF

  *** Save mask and no of segments to the first record
  DELETE ALL
  IF !SEEK("  ")
    APPEND BLANK
  ENDIF
  GATHER FROM loFormSet.laData FIELDS &lcScFields
    *!B610270,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031][Start]
  TRY 
    Replace lacsused WITH loFormSet.llAcsUsed
  CATCH
  ENDTRY 
  *!B610270,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031][End]
  =gfAdd_Info(ALIAS())

  *** Save seg. size, short and long description to records as no of seg.
  FOR lnCount = 1 TO loFormSet.laData[4]
    lnVarName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
    IF !SEEK("  ")
      APPEND BLANK
    ENDIF

    REPLACE  nAcsSegNo WITH lnCount    ;
             nAcsSize  WITH EVALUATE(lnVarName) ;
             cAcsShDes WITH IIF(EMPTY(loFormSet.laAcDes[lnCount,1]),;
                                "Sg"+ALLTRIM(STR(lnCont)),;
                                loFormSet.laAcDes[lnCount,1]);
             cAcsLgDes WITH IIF(EMPTY(loFormSet.laAcDes[lnCount,2]),;
                                "Default",loFormSet.laAcDes[lnCount,2])
  *!B610270,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031][Start]
  TRY 
    Replace lacsused WITH loFormSet.llAcsUsed
  CATCH
  ENDTRY 
  *!B610270,1 HIA 02/25/2013 Aria XP - Account Code Structure error [T20130221.0031][End]
    =gfAdd_Info(ALIAS())
  ENDFOR

  SELECT SYCCOMP
  DECLARE laChldComp[1]
  laChldComp = " "
  _TALLY     = 0
  SELECT cComp_Id,gfGetDataDir(ALLT(CCOM_DDIR)) ;
         FROM  (oAriaApplication.SysPath+"SYCCOMP") ;
         WHERE cCompprnt = loFormSet.lcComp_Id ;
         INTO  ARRAY laChldComp

  SELECT SYCACCOD
  IF _TALLY > 0
    FOR lnCont1 = 1 TO ALEN(laChldComp,1)

      *** Prepare array hold the current company account code struc. ***
      DECLARE laStruc[1]
      laStruc = " "
      USE (laChldComp[lnCont1,2]+'ACCOD') IN 0 AGAIN ALIAS CHLDACCOD ORDER TAG Accsegno
      SELECT CHLDACCOD
      DELETE ALL

      IF !SEEK("  ")
        APPEND BLANK
      ENDIF
      GATHER FROM loFormSet.laData FIELDS &lcScFields
      =gfAdd_Info(ALIAS())

      *** Save seg. size, short and long description to records as no of seg.
      FOR lnCount = 1 TO loFormSet.laData[4]
        lnVarName  = "loFormSet.lnSeg"+ALLTRIM(STR(lnCount))
        IF !SEEK("  ")
          APPEND BLANK
        ENDIF

        REPLACE  nAcsSegNo WITH lnCount    ;
                 nAcsSize  WITH EVALUATE(lnVarName) ;
                 cAcsShDes WITH IIF(EMPTY(loFormSet.laAcDes[lnCount,1]),;
                                    "Sg"+ALLTRIM(STR(lnCont)),;
                                    loFormSet.laAcDes[lnCount,1]);
                 cAcsLgDes WITH IIF(EMPTY(loFormSet.laAcDes[lnCount,2]),;
                                    "Default",loFormSet.laAcDes[lnCount,2])

        =gfAdd_Info(ALIAS())
      ENDFOR
      USE IN CHLDACCOD
    ENDFOR
  ENDIF
  IF SEEK('',"SYCACCOD")
    =gfObj_Lock(.F.)
  ENDIF

  IF loFormSet.llComIdFlg
    loFormSet.llChange = .F.
    llSavAcc = .T.
  ENDIF
ENDIF

*** Store old valus in variables
loFormSet.lnOldSize = loFormSet.laData[1]          && Old total size
loFormSet.lcOldMask = loFormSet.laData[2]          && Old Account Mask
loFormSet.lcOldDes  = loFormSet.laData[3]          && Old Account segments desciption
loFormSet.lcOldNoSeg= loFormSet.laData[4]          && Old No of segments

loFormSet.llChange = .F.
loFormSet.Ariaform1.pbSave.Enabled = .F.


*!**************************************************************************
*!
*!      Function: lfvNewVal
*!
*!**************************************************************************
*
FUNCTION lfvNewVal
PARAMETERS loFormSet,loFld
*IF lcOldVal <> EVAL(VARREAD())
IF loFld.Value <> loFld.OldValue
  loFormSet.llChange = loFormSet.llChange OR .T.
  loFormSet.Ariaform1.pbSave.Enabled = .T.
ENDIF

*!**************************************************************************
*!
*!      Function: lfOpenCmpData
*!
*!**************************************************************************
* function to open the account code struc. file from the company data dir.
FUNCTION lfOpenCmpData
PARAMETERS loFormSet
IF USED('SYCACCOD')
  USE IN SYCACCOD
ENDIF
IF !EMPTY(loFormSet.lcComp_Id)
  USE (loFormSet.lcDataDir+'ACCOD') IN 0 AGAIN ALIAS SYCACCOD ORDER TAG Accsegno
  SELECT SYCACCOD
ELSE
  RETURN .F.
ENDIF

************************************************************
*! Name      : lfDescFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/20/2012
*! Purpose   : Descriptions FormInit method
************************************************************
FUNCTION lfDescFormInit
PARAMETERS loDesFormSet
LOCAL lnI,lcI
loFormSet = loDesFormSet.loFormSet
WITH loDesFormSet.Ariaform1

  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  .Caption = 'Account cdoe description'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = LANG_ACC_CODE_DESC
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACC_CODE_DESC,loFormSet.GetHeaderText("LANG_ACC_CODE_DESC",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[Start]

  FOR lnI = 1 TO loFormSet.laData[4]
    lcI = STR(lnI,1)
    .txtShrt&lcI..ControlSource = 'Thisformset.loFormset.laAcDes[&lcI,1]'
    .txtShrt&lcI..MaxLength = loFormset.lnSeg&lcI

    .txtLong&lcI..ControlSource = 'Thisformset.loFormset.laAcDes[&lcI,2]'
    .txtLong&lcI..MaxLength  = Len(loFormset.laAcDes[&lcI,2])

    .txtShrt&lcI..Visible = .T.
    .txtLong&lcI..Visible = .T.
    .lblSeg&lcI..Visible = .T.

  ENDFOR
  .Height = .txtShrt&lcI..Top + 70
  lcI = STR(loFormSet.laData[4],1)
  .shp1.Height = .txtShrt&lcI..Top + 25
  .shp2.Height = .txtShrt&lcI..Top + 25
  .shp3.Height = .txtShrt&lcI..Top + 25
  .pbOk.Top = .txtShrt&lcI..Top + 40
  .pbCancel.Top = .txtShrt&lcI..Top + 40
ENDWITH

*- End of lfDescFormInit.
************************************************************
*! Name      : lfSlctFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/20/2012
*! Purpose   : Select a company FormInit method function
************************************************************
FUNCTION lfSlctFormInit
PARAMETERS loSlctFormSet
loFormSet = loSlctFormSet.loFormSet
WITH loSlctFormSet.AriaForm1
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*  .Caption = 'Companies account code structure'
*N000682,1 MMT 11/20/2012 Globalization Changes[Start]
*  .Caption = 'Companies account code structure'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption =LANG_SMACCOD_COMPANIES_ACCOUNT_CODE_STRUCTURE
.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMACCOD_COMPANIES_ACCOUNT_CODE_STRUCTURE,loFormSet.GetHeaderText("LANG_SMACCOD_COMPANIES_ACCOUNT_CODE_STRUCTURE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization Changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  .laCompInfo.ControlSource = 'Thisformset.loformset.lsCompany'
  .laCompInfo.RowSource = 'Thisformset.loformset.laCompInfo'
ENDWITH

*- End of lfSlctFormInit.

************************************************************
*! Name      : lfRange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/21/2012
*! Purpose   : Check range
************************************************************
FUNCTION lfRange
PARAMETERS loFld,lnFrm,lnTo
IF !BETWEEN(loFld.Value,lnFrm,lnTo)
  lcMsg = 'Range : '+PADL(lnFrm,2)+' to '+PADL(lnTo,2)
  gfModalGen('INM00274B00000','DIALOG',lcMsg)
  loFld.Value = loFld.OldValue
  RETURN .F.
ENDIF

*- End of lfRange.

************************************************************
*! Name      : MSG
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/05/2012
*! Purpose   : MSG, can run only on my PC
************************************************************
FUNCTION msg
PARAMETERS llDoNotUseStep
IF SYS(0)='DEV4 # tarek'
  ON ERROR
  _SCREEN.Visible=.T.
  IF !llDoNotUseStep
    IF FILE('C:\TEMP\X.X')
      SET STEP ON
    ENDIF
  ENDIF
ENDIF
*- End of MSG.

