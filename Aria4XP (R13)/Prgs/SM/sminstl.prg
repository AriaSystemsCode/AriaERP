*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMINSTL.Prg
*:  Module      : System Manager
*:                Installation informtion screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/18/2013
*:  Reference   : *E303356,1
*:************************************************************************
*B610288,1 TMI 04/02/2013 [Start] remove this control as a media issu
*:************************************************************************

#INCLUDE R:\ARIA4XP\PRGS\SM\SMINSTL.H

lcRunScx = lfGetScx("SM\SMINSTL.scx")
DO FORM (lcRunScx)

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

loFormSet.AddProperty('lcProgName','SMINSTL')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES(loFormSet.lcProgName)

*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  *.cBrowseIndexExpression = "COMPFYEAR"
  *.cBrowseIndexFields     = "COMPFYEAR"
  *.cBrowseIndexName       = "CFISFYEAR"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
ENDWITH

lfDefineVars(loFormSet)
*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/18/2013
*! Purpose   : Define screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

lcScFields = 'LINSMUSER,LINSLOGRQ,LINSPASSW,LINSAUDLG,LINSWSREQ,CINSCLTYP,CINSDFCLM,CINSDFCLC,LINSUSDOS,LINSUSWIN,'+;
             'LINSUSUNX,LINSUSMAC,CINSYSFDR,CINSDOSPD,CINSDOSRD,CINSDOSWD,CINSWINPD,CINSWINRD,CINSWINWD,CINSUNXPD,'+;
             'CINSUNXRD,CINSUNXWD,CINSMACPD,CINSMACRD,CINSMACWD,CINSDFCOM,CINSALLCMP,CINSWINBM,CINSRSRDR,CDEF_BMP,CCONT_CODE'
loFormSet.AddProperty('lcScFields',lcScFields)
lcCnt = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcCnt.]')

loFormSet.AddProperty('llOld_3'    , .F.)
loFormSet.AddProperty('lcOld_ID'   , "")
loFormSet.AddProperty('lcOldColor' , "")
loFormSet.AddProperty('lcOldFil'   , "")
loFormSet.AddProperty('lcDefStat'  , "ENABLE")

loFormSet.AddProperty('lcSave1',' ')
loFormSet.AddProperty('lcSave2',' ')
loFormSet.AddProperty('lcSave3',' ')
loFormSet.AddProperty('lcSave4',' ')
loFormSet.AddProperty('lcSave5',' ')

SCATTER  FIELDS &lcScFields TO loFormSet.laData
loFormSet.AddProperty('lcComNam' , LOOKUP(sycComp.cCom_name,loFormSet.laData[26],sycComp.cComp_id) )

loFormSet.Ariaform1.laData3.Enabled = !EMPTY(loFormSet.laData[2])
loFormSet.Ariaform1.txtCountryDesc.Value = LOOKUP(SYCINT.cCont_Desc,loFormSet.LADATA[31],SYCINT.CCONT_CODE,'CCONTCODE')

*- Define the ControlSource of the screen controls
WITH loFormSet.Ariaform1
  DO msg
  .lcComNam.ControlSource = 'Thisformset.lcComNam'
  .laData2.ControlSource = 'Thisformset.laData[2]'
  .laData3.ControlSource = 'Thisformset.laData[3]'
  *B610288,1 TMI 04/02/2013 [Start] remove this control as a media issu
  *.laData9.ControlSource = 'Thisformset.laData[9]'
  *.laData10.ControlSource = 'Thisformset.laData[10]'
  *.laData11.ControlSource = 'Thisformset.laData[11]'
  *.laData12.ControlSource = 'Thisformset.laData[12]'
  *B610288,1 TMI 04/02/2013 [End  ]
  .laData26.ControlSource = 'Thisformset.laData[26]'
  .laData31.KeyTextbox.ControlSource = 'Thisformset.laData[31]'
  *E303356,1 TMI 04/02/2013 [Start] remove this control as a media issu
  *.laData7.ControlSource = 'Thisformset.laData[7]'
  *.laData8.ControlSource = 'Thisformset.laData[8]'
  *E303356,1 TMI 04/02/2013 [End  ]
  .laData13.ControlSource = 'Thisformset.laData[13]'
  .laData27.ControlSource = 'Thisformset.laData[27]'
  .laData29.ControlSource = 'Thisformset.laData[29]'
  .Refresh()
ENDWITH

*E303356,1 TMI 04/02/2013 [Start] initialize loFormSet.llOld_3
loFormSet.llOld_3 = loFormSet.laData[3]
*E303356,1 TMI 04/02/2013 [End  ]
loFormSet.Ariaform1.gnMaxUsers.Value = str(gnMaxUsers,3)
=lfShow(loFormset)

*- End of lfDefineVars.

*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
*
FUNCTION lfShow
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN
ENDIF

rbHours = 1
DO CASE
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	  CASE loFormSet.laData[6] = "none"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE loFormSet.laData[6] = LANG_none
CASE loFormSet.laData[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_none,loFormSet.GetHeaderText("LANG_none",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
    rbHours = 1
    *N000682,1 04/17/2013 RAS Globalization[START]
*!*	  CASE loFormSet.laData[6] = "24hr"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE loFormSet.laData[6] = LANG_24hr
CASE loFormSet.laData[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_24hr,loFormSet.GetHeaderText("LANG_24hr",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/17/2013 RAS Globalization[End  ]
    rbHours = 2
    *N000682,1 04/17/2013 RAS Globalization[START]
*!*	  CASE loFormSet.laData[6] = "12hr"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE loFormSet.laData[6] = LANG_12hr
CASE loFormSet.laData[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_12hr,loFormSet.GetHeaderText("LANG_12hr",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/17/2013 RAS Globalization[End  ]
    rbHours = 3
ENDCASE
loFormSet.Ariaform1.rbHours.Value = rbHours

lfvData_2(loFormSet,loFormSet.Ariaform1.laData2)

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2
PARAMETERS loFormSet,loFld

IF loFormSet.laData[2]
  loFormSet.laData[3] = loFormSet.llOld_3
  loFormSet.Ariaform1.laData3.Enabled = .T.
ELSE
  loFormSet.llOld_3   = loFormSet.laData[3]
  loFormSet.laData[3] = .F.
  loFormSet.Ariaform1.laData3.Enabled = .F.
ENDIF
loFormSet.Ariaform1.laData3.Refresh()


*!**************************************************************************
*!
*!      Function: lfvOpsDir()
*!
*!**************************************************************************
*
FUNCTION lfvOpsDir
PARAMETERS loFormSet,loFld

lnParOfs = 17
llOk = .F.
lcSave1 = loFormSet.laData[lnParOfs]
lcSave2 = loFormSet.laData[lnParOfs+1]
lcSave3 = loFormSet.laData[lnParOfs+2]
lcSave4 = loFormSet.laData[28]
lcSave5 = loFormSet.laData[30]
lcRunScx = lfGetScx("SM\SMwinop.scx")
DO FORM (lcRunScx)
IF llOk
  loFormSet.laData[lnParOfs] = lcSave1
  loFormSet.laData[lnParOfs+1] = lcSave2
  loFormSet.laData[lnParOfs+2] = lcSave3
  loFormSet.laData[28] = lcSave4
  loFormSet.laData[30] = lcSave5
ENDIF

loFld.Value = .T.


*!**************************************************************************
*!
*!      Function: lfvHours
*!
*!**************************************************************************
*
FUNCTION lfvHours
PARAMETERS loFormSet,loFld

rbHours = loFormSet.Ariaform1.rbHours.Value
DO CASE
  CASE rbHours = 1
  *N000682,1 04/17/2013 RAS Globalization[START]
*!*	    loFormSet.laData[6] = "none"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laData[6] = LANG_none
loFormSet.laData[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_none,loFormSet.GetHeaderText("LANG_none",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 RAS Globalization[End  ]
  CASE rbHours = 2
  *N000682,1 04/17/2013 RAS Globalization[START]
*!*	    loFormSet.laData[6] = "24hr"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laData[6] = LANG_24hr
loFormSet.laData[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_24hr,loFormSet.GetHeaderText("LANG_24hr",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 RAS Globalization[End  ]
  CASE rbHours = 3
  *N000682,1 04/17/2013 RAS Globalization[START]
*!*	    loFormSet.laData[6] = "12hr"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laData[6] = LANG_12hr
loFormSet.laData[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_12hr,loFormSet.GetHeaderText("LANG_12hr",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 RAS Globalization[End  ]
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvExit()
*!
*!**************************************************************************
*
FUNCTION lfvExit
PARAMETERS lnAryOfs

loFormSet.laData[lnAryOfs]   = lcSave1
loFormSet.laData[lnAryOfs+1] = lcSave2
loFormSet.laData[lnAryOfs+2] = lcSave3

IF lnAryOfs = 17
  loFormSet.laData[28] = lcSave4
  loFormSet.laData[30] = lcSave5
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDirPath
*!
*!**************************************************************************
*
FUNCTION lfvDirPath
PARAMETERS 	lcVarNam

lcNewPath = GETDIR()

IF !EMPTY(lcNewPath)
  &lcVarNam =lcNewPath + IIF(60 - LEN(lcNewPath) > 0 ,;
                             SPACE(60 - LEN(lcNewPath)) , '')
  =gfUpdate()
ENDIF


*!**************************************************************************
*!
*!      Function: lfvPExit
*!
*!**************************************************************************
*
Function lfvPExit
PARAMETERS loFormSet

lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO
=gfAdd_Info(ALIAS())

gcAllCmp  = ALLTRIM(sycinst.cInsAllCmp)
gcIntCont = loFormSet.laData[31]

IF FILE (oariaapplication.defaultpath+"OLDPATH.DAT")
  lcSavSAf = SET ('SAFETY')
  SET TALK    OFF
  SET SAFETY  OFF
  SET CONSOLE OFF
  SET TEXTMERGE ON
  SET TEXTMERGE TO (oariaapplication.defaultpath+"OLDPATH.DAT")
  \\<<ALLTRIM(sycinst.cinsysfdr)>>
  \<<ALLTRIM(sycinst.cinsallcmp)>>
  \<<ALLTRIM(sycinst.cinsdospd)>>
  \<<ALLTRIM(sycinst.cinsdosrd)>>
  \<<ALLTRIM(sycinst.cinsdoswd)>>
  \<<ALLTRIM(sycinst.cinswinpd)>>
  \<<ALLTRIM(sycinst.cinswinrd)>>
  \<<ALLTRIM(sycinst.cinswinwd)>>
  \<<ALLTRIM(sycinst.cinswinbm)>>
  \<<ALLTRIM(sycinst.cinsrsrdr)>>
  SET TEXTMERGE TO
  SET TEXTMERGE OFF
  SET CONSOLE ON
  SET SAFETY &lcSavSAf
ENDIF

loFormSet.Release()


*!**************************************************************************
*!
*!      Function lfvCompId
*!
*!**************************************************************************
*
Function lfvCompId
PARAMETERS loFormSet,loFld

IF EMPTY(loFld.Value) OR loFld.Value == loFld.OldValue
  IF EMPTY(loFld.Value)
    loFormSet.Ariaform1.lcComNam.Value = ' '
  ENDIF
  RETURN
ENDIF

loFld.Refresh()

IF (!EMPTY(loFormSet.laData[26]))
  IF SEEK(loFormSet.laData[26],"SYCCOMP")
    loFormSet.Ariaform1.lcComNam.Value = sycComp.cCom_name
  ELSE
    lcSavAlias    = SELECT(0)
    DIMENSION laCompInfo[2]
    laCompInfo[1] = loFormSet.laData[26]
    laCompInfo[2] = loFormSet.Ariaform1.lcComNam.Value
    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    lcBrFields    = "cComp_id :H='Company ID',cCom_name :H='Company Name'"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields    = "cComp_id :H='"+LANG_Company_ID+"',cCom_name :H='"+LANG_Company_Name+"'"
lcBrFields    = "cComp_id :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_ID,loFormSet.GetHeaderText("LANG_Company_ID",loFormSet.HeaderAlias))+"',cCom_name :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Name,loFormSet.GetHeaderText("LANG_Company_Name",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 HES Globlization changes[End  ]

    SELECT SYCCOMP

    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    =gfBrows(.F.,"cComp_id,cCom_name","laCompInfo",'Company List')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfBrows(.F.,"cComp_id,cCom_name","laCompInfo",LANG_Company_List)
=gfBrows(.F.,"cComp_id,cCom_name","laCompInfo",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_List,loFormSet.GetHeaderText("LANG_Company_List",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 HES Globlization changes[End  ]

    SELECT(lcSavAlias)
    IF loFormSet.laData[26] = laCompInfo[1]
      loFormSet.laData[26] = loFld.OldValue
    ELSE
      loFormSet.laData[26] = laCompInfo[1]
      loFormSet.Ariaform1.lcComNam.Value   = laCompInfo[2]
    ENDIF
  ENDIF
ENDIF
loFormSet.Ariaform1.Refresh()

*!**************************************************************************
*!
*!      Function lfvDefBmp
*!
*!**************************************************************************
*
FUNCTION lfvDefBmp

lcOldFil   = loFormSet.laData[30]

loFormSet.laData[30] = gfChngWall(.T.)

loFormSet.laData[30] = IIF(EMPTY(loFormSet.laData[30]),lcOldFil,loFormSet.laData[30])


*!**************************************************************************
*!
*!      Function: lfvcountry
*!
*!**************************************************************************
*
FUNCTION lfvcountry
PARAMETERS loFormSet,loFld
IF loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
  RETURN
ENDIF

llBrowse = loFld.Selectedfrombrowse
IF (!EMPTY(loFormSet.laData[31]) AND !SEEK(loFormSet.laData[31],'SYCINT')) OR llBrowse
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCINT
  lcBrFields=gfDbfField('SYCINT')
  PRIVATE laData1
  DIMENSION laData1[1]
  STORE '' TO laData1
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  lcFile_ttl    = "Internationals"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_ttl    = LANG_Internationals
lcFile_ttl    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Internationals,loFormSet.GetHeaderText("LANG_Internationals",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  =gfBrows(.F.,"cCont_Code","laData1",lcFile_ttl)
  loFormSet.laData[31]=laData1[1]
  SELECT (lcSelect)
ENDIF
IF EMPTY(loFormSet.laData[31])
  loFormSet.laData[31] = loFld.KeyTextbox.OldValue
ENDIF
loFormSet.Ariaform1.Refresh()
loFormSet.Ariaform1.txtCountryDesc.Value = LOOKUP(SYCINT.cCont_Desc,loFormSet.LADATA[31],SYCINT.CCONT_CODE,'CCONTCODE')


