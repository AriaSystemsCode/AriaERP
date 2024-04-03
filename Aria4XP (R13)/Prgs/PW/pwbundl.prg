*:************************************************************************
*:  Program File: ARIA4XP\PRGS\PW\PWBUNDL.FXP
*:  Module      : Bundle
*:  Desc.       : Bundle  Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim, HES -  Hesham Gomma
*:  Date        : 04/24/2012
*:  Reference   : E303136,1
*:************************************************************************
*: Modifications:
*E303164,1 Use global message [PW Project]
*E303303,1 HES Seek Full data [PW Project]
*B609914,1 MMT 05/13/2012 PW bugs fixes for demo 05/13/2012
*B610415,1 TMI 09/15/2013 changing the PROMPT to LPROMPT [T20130910.0010(TASK)] 
*B610572,1 TMI 10/31/2013 19:16 [Start] &lcpo is evaluated to numetic, just remove the '&' [T20131031.0009 Task] 
*:************************************************************************
PARAMETERS lcCutTktNo       && If calling from the Cutting Ticket Screen
lcProg = JUSTSTEM(SYS(16))
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\Screens\PW\PWBUNDL.h
*N000682,1 MMT 11/22/2012 Globalization changes[END]

*- Call the screen
lcRunScx = lfGetScx("PW\&lcProg..scx")
IF !EMPTY(lcCutTktNo)
  PRIVATE oScr
  DO FORM (lcRunScx) WITH lcCutTktNo NAME oScr NOSHOW
  IF TYPE('oScr')='O' AND !ISNULL(oScr)
    oScr.SHOW(1)
  ENDIF
ELSE
  DO FORM (lcRunScx)
ENDIF

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
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
  *! Date      : 04/24/2012
  *! Purpose   : called from the Screen init Method
  *!*************************************************************
FUNCTION lfFormInit
  PARAMETERS loFormSet

  lcProg = JUSTSTEM(SYS(16))
  loFormSet.ADDPROPERTY('lcProgName',lcProg)
  loFormSet.ADDPROPERTY('lcOrgFile',gfTempName())

  ** if the PW is not installed and the Use bundle in the setups is set to NO then do not run
  IF !'PW' $ oAriaApplication.CompanyInstalledModules
    * N000682,1 HES Handle globalization issues [Start]
*!*	    =gfModalGen('INM42083B00000','DIALOG','Piece Work')
    =gfModalGen('INM42083B00000','DIALOG',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Piece_Work,loformset.GetHeaderText("LANG_PWBUNDL_Piece_Work",loformset.HeaderAlias)))
    * N000682,1 HES Handle globalization issues [End  ]
    RETURN .F.
  ENDIF

  WITH loFormSet
    .ADDPROPERTY('LUSEBUNDLE',gfGetMemVar('LUSEBUNDLE'))
    .ADDPROPERTY('IDEALQTY',gfGetMemVar('IDEALQTY'))
    *B610415,1 TMI 09/15/2013 [Start] changing the PROMPT to LPROMPT [T20130910.0010(TASK)] 
    *.ADDPROPERTY('PROMPT',gfGetMemVar('PROMPT'))
    .ADDPROPERTY('PROMPT',gfGetMemVar('LPROMPT'))
    *B610415,1 TMI 09/15/2013 [End  ] 
    .ADDPROPERTY('TOLERANCE',gfGetMemVar('TOLERANCE'))
  ENDWITH
  IF EMPTY(loFormSet.LUSEBUNDLE)
    * N000682,1 HES Handle globalization issues [Start]
*!*	    =gfModalGen('INM52032B00000','DIALOG','Use Bundles')
    =gfModalGen('INM52032B00000','DIALOG',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Use_Bundles,loformset.GetHeaderText("LANG_PWBUNDL_Use_Bundles",loformset.HeaderAlias)))    
    * N000682,1 HES Handle globalization issues [End  ]
    RETURN .F.
  ENDIF

  loFormSet.ADDPROPERTY('Apply',.F.)  && to indicate the a generation has been done

  lcPath = oAriaApplication.ApplicationHome
  SET PROCEDURE TO (lcPath+'PW\PWGLB.FXP') ADDITIVE &&  all these functions will be copied to the ARIAGLB later

  *- Open tables
  =lfOpenPRGFILES(loFormSet.lcProgName)

  *** Load program base file
  loFormSet.ADDPROPERTY('lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

  *- initializations
  WITH loFormSet
    .cbrowsetabledbengine   = "SQL"
    .nWorkArea                            = .lcBaseFile
    .otoolbar.nWorkArea                   = .lcBaseFile
    .DATAENVIRONMENT.INITIALSELECTEDALIAS = .lcBaseFile
    .cBrowseFileName        = .lcBaseFile
    .cBrowseIndexExpression = "CBUSDOCU+CSTYTYPE+PO"
    .cBrowseIndexFields     = "CBUSDOCU,CSTYTYPE,PO"
    .cBrowseIndexName       = "POSHDR"
    .cBrowseAliasName       = .lcBaseFile
    .cBrowseTableName       = .lcBaseFile
    .cBrowseFilter          = ""
    .BrowseTitle 		  	  = 'Cutting Ticket'
    * N000682,1 HES Handle globalization issues [Start]
*!*	    .AriaBrFields.edtBrowseFields.VALUE = ;
*!*	      "PO        :H='"+"Cutkt#"+"' ,"+;
*!*	      "Status    :H='"+"Status"+"' ,"+;
*!*	      "Style     :H='"+"Style ',"+;
*!*	      "cDivision :H='"+"Division"+"' ,"+;
*!*	      IIF(gfGetMemVar('M_WAREHOUS')='Y' ,"cWarecode :H='"+"Location"+"' ,",'')+;
*!*	      "Entered   :H='"+"Entered"+"' ,"+;
*!*	      "Complete  :H='"+"Complete"+"' ,"+;
*!*	      "nStyOrder :H='"+"Total Qty."+"' ,"+;
*!*	      "POTotal   :H='"+"Amount"+"' ,"+;
*!*	      "Receive   :H='"+"Received"+"' ,"+;
*!*	      "Open      :H='"+"Open"+"' "
    .AriaBrFields.edtBrowseFields.VALUE = ;
      "PO        :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Cutkt,loformset.GetHeaderText("LANG_PWBUNDL_Cutkt",loformset.HeaderAlias))+"',"+;
      "Status    :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Status,loformset.GetHeaderText("LANG_PWBUNDL_Status",loformset.HeaderAlias))+"',"+;
      "Style     :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Style,loformset.GetHeaderText("LANG_PWBUNDL_Style",loformset.HeaderAlias))+"',"+;
      "cDivision :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Division,loformset.GetHeaderText("LANG_PWBUNDL_Division",loformset.HeaderAlias))+"',"+;
      IIF(gfGetMemVar('M_WAREHOUS')='Y' ,"cWarecode :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Location,loformset.GetHeaderText("LANG_PWBUNDL_Location",loformset.HeaderAlias))+"',",'')+;
      "Entered   :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Entered,loformset.GetHeaderText("LANG_PWBUNDL_Entered",loformset.HeaderAlias))+"',"+;
      "Complete  :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Complete,loformset.GetHeaderText("LANG_PWBUNDL_Complete",loformset.HeaderAlias))+"',"+;
      "nStyOrder :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Total_Qty,loformset.GetHeaderText("LANG_PWBUNDL_Total_Qty",loformset.HeaderAlias))+"',"+;
      "POTotal   :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Amount,loformset.GetHeaderText("LANG_PWBUNDL_Amount",loformset.HeaderAlias))+"',"+;
      "Receive   :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Received,loformset.GetHeaderText("LANG_PWBUNDL_Received",loformset.HeaderAlias))+"',"+;
      "Open      :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Open,loformset.GetHeaderText("LANG_PWBUNDL_Open",loformset.HeaderAlias))+"'"
    * N000682,1 HES Handle globalization issues [End  ]
    .cBrowseKey             = 'PU'
  ENDWITH

  WITH loFormSet.Ariaform1.kbWorkOrder
    .cBrowseFields = loFormSet.AriaBrFields.edtBrowseFields.VALUE
    .cBrowseTitle  = loFormSet.BrowseTitle
    .obrowsecursor = loFormSet.lcBaseFile
    .cbusinessdocumenttype = 'P'
    .cworkordertype = 'U'
  ENDWITH


  *- set data sources
  loFormSet.ADDPROPERTY('laLot[1]','')
  loFormSet.Ariaform1.cboLot.ROWSOURCE = 'Thisformset.laLot'
  lfColumnWidthes(loFormSet.Ariaform1.cboLot)

  loFormSet.ADDPROPERTY('laItem[1]','')
  loFormSet.Ariaform1.cboItem.ROWSOURCE = 'Thisformset.laItem'
  lfColumnWidthes(loFormSet.Ariaform1.cboItem)

  loFormSet.ADDPROPERTY('laSize[1]','')
  loFormSet.Ariaform1.cboSize.ROWSOURCE = 'Thisformset.laSize'
  lfColumnWidthes(loFormSet.Ariaform1.cboSize)

  =lfCrtGridSource(loFormSet)
  =lfSetGridSource(loFormSet)

  *E303136,1 TMI 13/05/2012 [Start] Add a property to hold operation code
  *-add operation array
  loFormSet.ADDPROPERTY('lamfgcode[1]','')
  *E303136,1 TMI 13/05/2012 [End  ]

  *-Set grid column's titles
  WITH loFormSet.Ariaform1.grdBundle
    *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
    *.Column1.Header1.CAPTION = 'Lot#'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.Header1.CAPTION =LANG_PWBUNDL_LOT
.Column1.Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PWBUNDL_LOT,loFormSet.GetHeaderText("LANG_PWBUNDL_LOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/20/2012 Globalization Changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *.Column2.Header1.CAPTION = 'Item'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column2.Header1.CAPTION =LANG_PWBUNDL_ITEM
.Column2.Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PWBUNDL_ITEM,loFormSet.GetHeaderText("LANG_PWBUNDL_ITEM",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/20/2012 Globalization Changes[End]

    *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
    *.Column3.Header1.CAPTION = 'Size'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column3.Header1.CAPTION =LANG_PWBUNDL_SIZE
.Column3.Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PWBUNDL_SIZE,loFormSet.GetHeaderText("LANG_PWBUNDL_SIZE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/20/2012 Globalization Changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *.Column4.Header1.CAPTION = 'Bundle'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column4.Header1.CAPTION =LANG_PWBUNDL_BUNDLE
.Column4.Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PWBUNDL_BUNDLE,loFormSet.GetHeaderText("LANG_PWBUNDL_BUNDLE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
    *.Column5.Header1.CAPTION = 'Quantity'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column5.Header1.CAPTION =LANG_PWBUNDL_QUANTITY
.Column5.Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PWBUNDL_QUANTITY,loFormSet.GetHeaderText("LANG_PWBUNDL_QUANTITY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/20/2012 Globalization Changes[End]

    *E303136,1 TMI 13/05/2012 [Start] allow the back color be different for original lines
    *--Set dyenamic Bacl color.
    .SETALL("Dynamicbackcolor", "", "Column")
    .SETALL("Dynamicbackcolor","IIF("+loFormSet.lcGridFile+".CBUNDLE=' ',2487103,16777215)", "Column")

    *--Set dyenamic Font Bold.
    .SETALL("DynamicFontBold", "", "Column")
    .Column1.DYNAMICFONTBOLD="IIF("+loFormSet.lcGridFile+".CBUNDLE=' ',,.T.,.F.)"
    *E303136,1 TMI 13/05/2012 [End  ]
  ENDWITH

  IF EMPTY(loFormSet.lcCutTktNo)   && from file menu
    loFormSet.ChangeMode('S')
  ELSE
    loFormSet.ChangeMode('E')
    RETURN lfvCuttkt(loFormSet,loFormSet.Ariaform1.kbWorkOrder)
  ENDIF
  *- End of lfFormInit.
  ************************************************************
  *! Name      : lfOpenPRGFILES
  *! Developer : HES - Hesham Elmasry
  *! Date      : 04/14/2012
  *! Purpose   : Open Tables
  ************************************************************
FUNCTION lfOpenPRGFILES
  LPARAMETERS lcProg

  LOCAL lcPath,i
  lcPath = ''
  lcProg = PADR(UPPER(lcProg),10)
  SET MULTILOCKS ON
  lnRemResult = oAriaApplication.RemoteSystemData.Execute;
    ("Select * from SYDOBJCT WHERE CAPOBJNAM= '&lcProg'",'',"SYDOBJCT","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

  =gfOpenTable(oAriaApplication.SysPath+'sydFiles','CFILE_NAM','SH')   && CFILE_NAM
  =gfOpenTable(oAriaApplication.SysPath+'sydField','CFLD_NAME','SH')   && CFLD_NAME

  lcFiles = sydObjct.MPRGFILES
  DIME laFiles[2],laTbl[1],laIndx[1]
  STORE '' TO laFiles,laTbl,laIndx
  =gfSubStr(lcFiles,@laFiles,'|')
  =gfSubStr(laFiles[1],@laTbl,',')
  =gfSubStr(laFiles[2],@laIndx,',')
  FOR i = 1 TO ALEN(laTbl)
    =gfOpenTable(laTbl[i],laIndx[i],'SH')
    SELECT(laTbl[i])
  ENDFOR

  *** Load program base file
  lcBaseFile = ALLTRIM(sydObjct.cBaseFile)

  *- End of lfOpen.

  ************************************************************
  *! Name      : lfFormActivate
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 04/14/2012
  *! Purpose   : Form Activate
  ************************************************************
FUNCTION lfFormActivate
  PARAMETERS loFormSet

  *- Hide the toolbar in the MODAL mode
  IF TYPE('loFormSet.lcCutTktNo') = 'C' AND !EMPTY(loFormSet.lcCutTktNo)
    *loFormSet.oToolBar.Visible =.F.
    loFormSet.otoolbar.cmdSelect.VISIBLE =.F.  && in the dialog mode do not select new cutticket
  ENDIF

  *- End of lfFormActivate.

  ************************************************************
  *! Name      : lfColumnWidthes
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 02/23/2012
  *! Purpose   : Adjust column widthes
  ************************************************************
FUNCTION lfColumnWidthes
  PARAMETERS loPop
  LOCAL lcW
  lcW = ALLTRIM(STR(loPop.WIDTH - 25 ))
  loPop.COLUMNWIDTHS = '&lcW'
  *- End of lfColumnWidthes.

  ************************************************************
  *! Name      : lfChangeMode
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 04/24/2012
  *! Purpose   : Change Mode
  ************************************************************
FUNCTION lfChangeMode
  PARAMETERS loFormSet

  LOCAL lnSlct
  lnSlct = SELECT(0)

  IF TYPE('loFormSet.lcBaseFile')='U'
    RETURN
  ENDIF

  loFormSet.Ariaform1.kbWorkOrder.ENABLED = .F.

  DO CASE
    CASE loFormSet.ActiveMode = 'S'
      WITH loFormSet.Ariaform1.kbWorkOrder
        .ENABLED = .T.
        .KeyTextbox.VALUE = ''
        .KeyTextbox.SETFOCUS()
      ENDWITH

      SELECT (loFormSet.lcGridFile)
      ZAP
      SET FILTER TO
      LOCATE
      loFormSet.Ariaform1.grdBundle.RECORDSOURCE = ''
      loFormSet.Ariaform1.grdBundle.REFRESH()

    CASE loFormSet.ActiveMode = 'V'
      SELECT (loFormSet.lcGridFile)
      SET FILTER TO
      LOCATE
      WITH loFormSet.Ariaform1
        .grdBundle.REFRESH()

        .cboLot.ENABLED = .T.
        .cboItem.ENABLED = .T.
        .cboSize.ENABLED = .T.

        .cboLot.VALUE = loFormSet.laLot[1]
        .cboItem.VALUE = loFormSet.laItem[1]
        .cboSize.VALUE = loFormSet.laSize[1]
        =lfvGridFileFilter(loFormSet)
      ENDWITH

      IF TYPE('loFormSet.lcCutTktNo') = 'C' AND !EMPTY(loFormSet.lcCutTktNo)
        loFormSet.otoolbar.cmdSelect.VISIBLE =.F.  && in the dialog mode do not select new cutticket
        loFormSet.otoolbar.cmdEdit.ENABLED =.T.  && in the dialog mode do not select new cutticket
      ENDIF

    CASE loFormSet.ActiveMode = 'E'

    CASE loFormSet.ActiveMode = 'A'

  ENDCASE

  SELECT (lnSlct)
  *- End of lfChangeMode.

  ************************************************************
  *! Name      : lfFormSavefiles
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 04/24/2012
  *! Purpose   : Save process
  ************************************************************
FUNCTION lfFormSavefiles
  PARAMETERS loFormSet
  LOCAL lnSlct
  lnSlct = SELECT(0)

  lcDel = SET("Deleted")
  SET DELETED OFF
  SELECT pwbundl
  SCAN
    IF DELETED()
      gfDelete()
    ELSE
      gfReplace('')
    ENDIF
  ENDSCAN
  =gfTableUpdate()

  SELECT pwctkbom
  SCAN
    IF DELETED()
      gfDelete()
    ELSE
      gfReplace('')
    ENDIF
  ENDSCAN
  =gfTableUpdate()

  SELECT poshdr
  SCAN
    gfReplace('')
  ENDSCAN
  =gfTableUpdate()

  SET DELETED &lcDel
  SELECT (lnSlct)
  *- End of lfFormSavefiles.

  ************************************************************
  *! Name      : lfFormDelete
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 04/24/2012
  *! Purpose   : lfFormDelete
  ************************************************************
FUNCTION lfFormDelete
  PARAMETERS loFormSet
  LOCAL lnSlct,llDel

  SELECT(lnSlct)
  *- End of lfFormDelete.

  ************************************************************
  *! Name      : lfFormBeforeSave
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 04/24/2012
  *! Purpose   : Before Save
  ************************************************************
FUNCTION lfFormBeforeSave
  PARAMETERS loFormSet
  LOCAL lnSlct,llRet
  lnSlct = SELECT(0)

  IF !loFormSet.APPLY AND !loFormSet.lRemove
    lccuttkt = loFormSet.Ariaform1.kbWorkOrder.KeyTextbox.VALUE
    *No ð found for ð.
    * N000682,1 HES Handle globalization issues [Start]
*!*	    =gfModalGen('INM42041B00000','DIALOG','new generated/Removed bundle lines|the cutting ticket &lccuttkt, cannot save')
    =gfModalGen('INM42041B00000','DIALOG',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Line_Validate1,loformset.GetHeaderText("LANG_PWBUNDL_Line_Validate1",loformset.HeaderAlias))+ &lccuttkt+ IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Line_Validate2,loformset.GetHeaderText("LANG_PWBUNDL_Line_Validate2",loformset.HeaderAlias)))    
    * N000682,1 HES Handle globalization issues [End  ]
  ENDIF


  SELECT (lnSlct)
  RETURN loFormSet.APPLY OR loFormSet.lRemove
  *- End of lfFormBeforeSave.

  ************************************************************
  *! Name      : lfCrtGridSource
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 09/05/2012
  *! Purpose   : Create Grid Source
  ************************************************************
FUNCTION lfCrtGridSource
  PARAMETERS loFormSet
  LOCAL i

  loFormSet.ADDPROPERTY('lcGridFile',gfTempName())
  SELECT MFGOPRDT
  AFIELDS(laStru)
  lnArrLen = ALEN(laStru,1)
  DIMENSION laStru[lnArrLen+26,18]

  i = lnArrLen
  i = i + 1
  laStru[i,1] = 'MFGCODE'   && IMPORTANT : WHEN COLLECTING LINES FROM MFGOPRDT
  &&==> MFGOPRDT.COPRCODE to be as MFGOPRDT.MFGCODE
  && SAVE the field  COPRCODE  in MFGCODE AND empty the COPRCODE
  laStru[i,2] = 'C'
  laStru[i,3] = 6
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'CBUNDLE'
  laStru[i,2] = 'C'
  laStru[i,3] = 6
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'LHASBUNDLE'
  laStru[i,2] = 'L'
  laStru[i,3] = 1
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'NBQTY'
  laStru[i,2] = 'N'
  laStru[i,3] = 6
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'LOTSIZE'
  laStru[i,2] = 'C'
  laStru[i,3] = 6
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ1'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ2'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ3'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ4'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ5'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ6'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ7'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SZ8'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'CSIZES'
  laStru[i,2] = 'C'
  laStru[i,3] = 8
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SIZENO'
  laStru[i,2] = 'C'
  laStru[i,3] = 1
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'SIZE'
  laStru[i,2] = 'C'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'TRANTYPE'
  laStru[i,2] = 'C'
  laStru[i,3] = 1
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'REC_DATE'
  laStru[i,2] = 'D'
  laStru[i,3] = 8
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'NLINENO'
  laStru[i,2] = 'N'
  laStru[i,3] = 4
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'coperseq'
  laStru[i,2] = 'C'
  laStru[i,3] = 2
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'coprattype'
  laStru[i,2] = 'C'
  laStru[i,3] = 6
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'noperatper'
  laStru[i,2] = 'N'
  laStru[i,3] = 3
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'nopercost'
  laStru[i,2] = 'N'
  laStru[i,3] = 9
  laStru[i,4] = 2

  i = i + 1
  laStru[i,1] = 'NREPNO'
  laStru[i,2] = 'N'
  laStru[i,3] = 5
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'covertype'
  laStru[i,2] = 'C'
  laStru[i,3] = 1
  laStru[i,4] = 0

  i = i + 1
  laStru[i,1] = 'noverunit'
  laStru[i,2] = 'N'
  laStru[i,3] = 7
  laStru[i,4] = 0

  *LOOP to fill rest of the array
  *- update other array fields
  FOR lnI = lnArrLen+1 TO ALEN(laStru,1)
    STORE .F. TO laStru[lnI,5],laStru[lnI,6]
    FOR lnJ = 7 TO 16
      laStru[lnI,lnJ] = ""
    ENDFOR
    STORE 0 TO laStru[lnI,17],laStru[lnI,18]
  ENDFOR

  CREATE CURSOR (loFormSet.lcGridFile) FROM ARRAY laStru
  INDEX ON CLOTNO+ITEM+SIZENO+CBUNDLE TAG (loFormSet.lcGridFile)
  *- End of lfCrtGridSource.
  ************************************************************
  *! Name      : lfSetGridSource
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 09/05/2012
  *! Purpose   : Set grid Data Sources
  ************************************************************
FUNCTION lfSetGridSource
  PARAMETERS loFormSet
  WITH loFormSet.Ariaform1.grdBundle
    .RECORDSOURCE = ''
    *  .RecordSource = IIF(loFormSet.ActiveMode='S','',loFormSet.lcGridFile)
    .RECORDSOURCE = loFormSet.lcGridFile

    .Column1.CONTROLSOURCE = loFormSet.lcGridFile+'.CLOTNO'
    .Column2.CONTROLSOURCE = loFormSet.lcGridFile+'.ITEM'
    .Column3.CONTROLSOURCE = loFormSet.lcGridFile+'.SIZE'
    .Column4.CONTROLSOURCE = loFormSet.lcGridFile+'.CBUNDLE'
    .Column5.CONTROLSOURCE = loFormSet.lcGridFile+'.NBQTY'

  ENDWITH
  *- End of lfSetGridSource.
  ************************************************************
  *! Name      : lfRemoveBundle
  *! Developer : HES - Hesham Elmasry
  *! Date      : 09/05/2012
  *! Purpose   : Remove Bundle
  ************************************************************
FUNCTION lfRemoveBundle
  PARAMETERS loFormSet

  SELECT(loFormSet.lcGridFile)
  lcBundle = CBUNDLE
  lcLotSize = lotSize
  lcLot = CLOTNO
  lccuttkt = ctktno
  lcItem = ITEM
  lcSize = SUBSTR(lcLotSize,2)
  lcMFGCode = MFGCode
  lcOprCode = cOprCode

  SELECT pwtrkdt
  LOCATE FOR CBUNDLE = lcBundle AND INLIST(TYPE,'C','P')
  IF FOUND()
    *!* E303164,1 Use global message [Start]
*!*	    MESSAGEBOX("Bundle# "+ALLTRIM(lcBundle)+" in process, you can't delete it.",16+512,_SCREEN.CAPTION)
    *-- Message: Bundle# ð in process, you can't delete it.
    *-- Buttons: OK
    =gfModalGen('INM54017B00000','DIALOG',ALLTRIM(lcBundle))
    *!* E303164,1 Use global message [End  ]
    RETURN
  ENDIF

  *!* E303164,1 Use global message [Start]
*!*	  IF MESSAGEBOX("Are you sure you want to erase Bundle# "+ALLTRIM(lcBundle)+"?" ,4+32+512,_SCREEN.CAPTION) = 6
  *-- Message: Are you sure you want to erase Bundle# ð?
  *-- Buttons: Yes\No
  IF gfModalGen('INM54018B00006','DIALOG',ALLTRIM(lcBundle)) = 1
  *!* E303164,1 Use global message [End  ]

    *!*	    SELECT pwtrkdt
    *!*	    lcSz = SUBSTR(lcLotSize,1,1)
    *!*	    lnQty = nQTy&lcSz
    *!*	    DELETE

    *!*	    SELECT pwtrkhd
    *!*	    IF SEEK(lccuttkt+lcMFGCode+lcOprCode)
    *!*	      REPLACE nQty_Issue WITH nQty_Issue - lnQty
    *!*	    ENDIF

    SELECT pwbundl
    IF SEEK(lccuttkt+lcBundle)
      DELETE
    ENDIF

    SELECT pwctkbom
    SCAN FOR CBUNDLE = lcBundle
      DELETE
    ENDSCAN

    SELECT (loFormSet.lcGridFile)
    DELETE

    *!* E303164,1 Use global message [Start]
*!*	    MESSAGEBOX("Bundle# "+ALLTRIM(lcBundle)+" removed successfully.",64+512,_SCREEN.CAPTION)
    *-- Message: Bundle# ð removed successfully.
    *-- Buttons: OK
    =gfModalGen('INM54019B00000','DIALOG',ALLTRIM(lcBundle))
    *!* E303164,1 Use global message [End  ]

    loFormSet.Ariaform1.grdBundle.REFRESH()
  ENDIF

  *- End of lfRemoveBundle.
  ************************************************************
  *! Name      : lfUndo
  *! Developer : HES - Hesham Elmasry
  *! Date      : 09/05/2012
  *! Purpose   : Undo
  ************************************************************
FUNCTION lfUndo
  PARAMETERS loFormSet


  *- End of lfUndo.
  ************************************************************
  *! Name      : lfvGenerate
  *! Developer : HES - Hesham Elmasry
  *! Date      : 09/05/2012
  *! Purpose   : Generate & Remove
  ************************************************************
FUNCTION lfvGenerate
  PARAMETERS loFormSet

  lGenearet = loFormSet.lGenerate
  lRemove   = loFormSet.lRemove

  lccuttkt = loFormSet.Ariaform1.kbWorkOrder.KeyTextbox.VALUE

  SELECT pwctkbom
  IF RECCOUNT() = 0
    =gfseek('')
  ENDIF

  SELECT pwbundl
  IF RECCOUNT() = 0
    =gfseek('')
  ENDIF

  SELECT pwtrkhd
  =gfseek(lccuttkt)

  SELECT pwtrkdt
  =gfseek(lccuttkt)

  SELECT (loFormSet.lcGridFile)
  IF lRemove
    lfRemoveBundle(loFormSet)
  ELSE

    LOCATE FOR !EMPTY(CBUNDLE)

    lHasCompBundles = .F.
    lcOrgKey = ""
    lcKey = ""
    lnStopQty = 0
    llStopQty = .F.
    lnSeq = 1
    DIMENSION laStopBundles[100]
    IF FOUND()
      *!* E303164,1 Use global message [Start]
*!*		      IF MESSAGEBOX("This will erase the existing Bundle(s), Continue?",4+32+512,_SCREEN.CAPTION) = 1
      *-- Message: This will erase the existing Bundle(s), Continue?
      *-- Buttons: Yes\No
      IF gfModalGen('INM54020B00006','DIALOG') = 1
      *!* E303164,1 Use global message [End  ]
        SCAN FOR !EMPTY(CBUNDLE)
          lcBundle = CBUNDLE
          lcLotSize = lotSize
          lcLot = CLOTNO
          lcItem = ITEM
          lcKey = CLOTNO+ITEM+SUBSTR(ALLTRIM(lcLotSize),2)

          SELECT pwtrkdt
          LOCATE FOR CBUNDLE = lcBundle AND INLIST(TYPE,'C','P')
          IF FOUND()
            laStopBundles[lnSeq] = lcBundle
            lnSeq = lnSeq + 1
            llStopQty = .T.

            SELECT pwbundl
            IF SEEK(lccuttkt+lcBundle)
              lnStopQty = lnStopQty + nbQty
            ENDIF

            SELECT pwtrkdt
            IF !(lcOrgKey == lcKey)
              *!* E303164,1 Use global message [Start]
*!*	              MESSAGEBOX("The following size has bundles in process, can't delete processed bundles, LOT# " +ALLTRIM(CLOTNO)+ ", Item# " +ALLTRIM(lcItem)+ ", Size: " + SUBSTR(ALLTRIM(lcLotSize),2)
               *-- Message: The following size has bundles in process, can't delete processed bundles, LOT# ð, Item# ð, Size: ð, this bundle skipped from generation.
               *-- Buttons: OK
               =gfModalGen('INM54021B00000','DIALOG',ALLTRIM(CLOTNO)+"|"+ALLTRIM(lcItem)+"|"+SUBSTR(ALLTRIM(lcLotSize),2))
              *!* E303164,1 Use global message [End  ]
            ENDIF
            LOOP
          ENDIF

          SELECT pwbundl
          IF SEEK(lccuttkt+lcBundle) AND ASCAN(laStopBundles,lcBundle) = 0
            DELETE
          ENDIF

          SELECT pwctkbom
          SCAN FOR CBUNDLE = lcBundle
            DELETE
          ENDSCAN

          SELECT (loFormSet.lcGridFile)
          DELETE
        ENDSCAN

      ELSE
        RETURN .F.
      ENDIF
    ENDIF

    IF lGenearet
      lnmajlen = LEN(gfitemmask('PM'))  && No. of major segments.

      lcRunScx = lfGetScx("PW\PWGEN.scx")
      DO FORM (lcRunScx) WITH loFormSet

      IF loFormSet.APPLY

        lntoler = loFormSet.tolerance/100
        lnideal = loFormSet.IdealQty
        lnmax = lntoler*lnideal+lnideal
        lnmin = lntoler*lnideal-lnideal

        SELECT poshdr
        =gfseek('PU'+lccuttkt)
        lnlastbundle = VAL(ALLTRIM(CBUNDLE))

        SELECT pwctkbom
        SELECT MAX(VAL(ngenno)) AS maxgenno FROM pwctkbom INTO ARRAY maxgenno
        lnmaxgenno = IIF(ISNULL(maxgenno),0,maxgenno)

        SELECT MAX(nlineno) AS maxlineno FROM pwctkbom INTO ARRAY maxlineno
        lnmaxlineno = IIF(ISNULL(maxlineno),0,maxlineno)
        m.nlineno = lnmaxlineno

        SELECT pwbundl
        SELECT MAX(VAL(cbarcode)) AS maxbarcode FROM pwbundl INTO ARRAY maxbarcode
        lnmaxbarcode = IIF(ISNULL(maxbarcode),0,maxbarcode)

        SELECT (loFormSet.lcGridFile)
        SCAN FOR !DELETED() AND EMPTY(CBUNDLE)
          m.cimtyp = cimtyp
          m.ctktno = ctktno
          m.cuttkt = ctktno
          m.CLOTNO = CLOTNO
          m.MFGCode = MFGCode
          m.cdyelot = cdyelot
          m.item = ITEM
          m.csizes = csizes
          m.trantype = 'B'
          lnorgqty = nbQty
          m.nbQty = lnideal
          m.lotSize = SIZENO+ALLTRIM(SIZE)
          m.SIZENO = SIZENO
          m.Size = SIZE
          lnidealbundls = INT((lnorgqty-lnStopQty)/lnideal)
          lnremain = (lnorgqty-lnStopQty)%lnideal
          lnbundcnt = lnidealbundls+IIF(lnremain>0,1,0)

          SELECT pwbom
          *!* E303303,1 HES Seek Full data [Start]
          =gfseek('')
          *!*	          =gfseek(PADR(SUBSTR(m.item,1,lnmajlen),19)+m.MFGCode)
          *!* E303303,1 HES Seek Full data [End  ]

          SELECT (loFormSet.lcGridFile)

          FOR lnx = 1 TO lnbundcnt

            IF lnremain > 0 AND lnx = lnbundcnt
              m.nbQty = lnremain
            ENDIF

            m.CBUNDLE = PADL(ALLTRIM(STR(lnlastbundle+1)),6,'0')
            m.cbarcode = PADL(ALLTRIM(STR(lnmaxbarcode+1)),6,'0')
            lnlastbundle = lnlastbundle + 1
            lnmaxbarcode = lnmaxbarcode + 1
            lnrecno = RECNO()

            lcMFGCode = IIF(TYPE('m.MFGCode') <> 'U',m.MFGCode,'')
            lcOprCode = IIF(TYPE('m.cOprCode') <> 'U',m.cOprCode,'')

            APPEND BLANK
            GATHER MEMVAR MEMO
            GOTO lnrecno

            m.MFGCode = ''
            m.cOprCode = ''
            SELECT pwbundl
            APPEND BLANK
            =gfAdd_Info('pwbundl')
            GATHER MEMVAR MEMO

            m.MFGCode = lcMFGCode
            m.cOprCode = lcOprCode

            SELECT (loFormSet.lcGridFile)
            REPLACE lhasbundle WITH .T.

            lcOrgBund = ""

            FOR lnc = 1 TO ALEN(loFormSet.lamfgcode)

              m.MFGCode = loFormSet.lamfgcode[lnc]

              SELECT pwbom
              SCAN FOR cItmMajor+MFGCode = PADR(SUBSTR(m.item,1,lnmajlen),19)+m.MFGCode
                m.cOprCode = cOprCode
                m.ngenno = PADL(ALLTRIM(STR(lnmaxgenno+1)),10,'0')
                lnmaxgenno = lnmaxgenno + 1
                m.style = m.item
                m.coperseq = coperseq
                m.coprattype = coprattype
                m.noperatper = noperatper
                m.nopercost = nopercost
                m.covertype = 'Q'
                m.noverunit = 0
                IF !(ALLTRIM(m.CBUNDLE) == lcOrgBund)
                  m.nlineno = m.nlineno + 1
                  lcOrgBund = ALLTRIM(m.CBUNDLE)
                ENDIF

                SELECT pwctkbom
                APPEND BLANK
                =gfAdd_Info('pwctkbom')
                GATHER MEMVAR MEMO

              ENDSCAN
            ENDFOR
            SELECT (loFormSet.lcGridFile)
          ENDFOR
        ENDSCAN

        SELECT poshdr
        REPLACE CBUNDLE WITH PADL(ALLTRIM(STR(lnlastbundle)),6,'0')
        loFormSet.Ariaform1.grdBundle.REFRESH()
      ENDIF
    ENDIF
  ENDIF
  * N000682,1 HES

  *- End of lfvGenerate.

  ************************************************************
  *! Name      : lfvRemove
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 09/05/2012
  *! Purpose   : Remove
  ************************************************************
FUNCTION lfvRemove
  PARAMETERS loFormSet

  *- End of lfvRemove.
  ************************************************************
  *! Name      : lfvCuttkt
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 09/05/2012
  *! Purpose   : Cuttkt validation
  ************************************************************
FUNCTION lfvCuttkt
  PARAMETERS loFormSet,loThis,llNoChngMode

  IF !EMPTY(loFormSet.lcCutTktNo)
    loFormSet.Ariaform1.kbWorkOrder.KeyTextbox.VALUE = loFormSet.lcCutTktNo
  ENDIF

  * Get only from mfgoprdt lines of CIMTYP  = 'M'

  SELECT (loThis.obrowsecursor)

  *!*  IF (!EMPTY(THIS.KEYTEXTBOX.VALUE) AND !SEEK(This.cbusinessdocumenttype+This.cworkordertype+THIS.KEYTEXTBOX.VALUE)) OR This.SelectedFromBrowse
  *!*
  *!*	LOCAL lcPO,lcBrowChr
  lcPO      = RTRIM(loThis.KeyTextbox.VALUE)
  lcBrowChr = RIGHT(lcPO,1)
  lcPO      = IIF(lcBrowChr=='?',SUBSTR(lcPO,1,LEN(lcPO)-1),lcPO)
  loThis.KeyTextbox.VALUE = lcPO
  LOCAL llNoRecordFound, llServerError
  loFormSet.oRemoteCursor.mGetCursor(loThis.obrowsecursor,loThis.obrowsecursor,;
    loFormSet.DATASESSIONID,'FIRST',.F.,;
    loFormSet.cBrowseTableName,'*','',;
    loFormSet.cBrowseIndexName,;
    loFormSet.cBrowseIndexExpression,.F.,;
    loFormSet.cBrowseIndexFields,1,;
    "cBusDocu = '"+loThis.cbusinessdocumenttype+;
    "' AND cStyType ='"+;
    loThis.cworkordertype+"' AND PO ='"+;
    ALLTRIM(loThis.KeyTextbox.VALUE)+"'",loFormSet.cBrowseKey,;
    @llNoRecordFound,@llServerError)
  IF llNoRecordFound OR llServerError
    PRIVATE llSelected, lcToDo
    llSelected = .F.
    DO FORM BROWSE ;
      WITH loThis.cBrowseFields,loThis.cBrowseTitle,;
      "PU","status<>'H'",;
      .F., .T., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.,;
      'POSHDR', ;
      'SQL', ;
      'POSHDR', .F., 'PU'+ALLTRIM(loThis.KeyTextbox.VALUE ) ;
      TO llSelected

    IF llSelected
      loThis.KeyTextbox.VALUE = EVALUATE(loThis.obrowsecursor+'.PO')
    ELSE
      loThis.KeyTextbox.VALUE = ""
    ENDIF
  ELSE
    IF poshdr.STATUS = 'H'
      *No cost sheet details found for ð.
      * N000682,1 HES Handle globalization issues [Start]
*!*	      =gfModalGen('INM34088B00000','DIALOG','the cutting ticket &lcPO, cannot proceed.')
      *B610572,1 [T20131031.0009 Task] TMI 10/31/2013 19:16 [Start] &lcpo is evaluated to numetic, just remove the '&'
      *=gfModalGen('INM34088B00000','DIALOG',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Cut_Validate1,loformset.GetHeaderText("LANG_PWBUNDL_Cut_Validate1",loformset.HeaderAlias))+ &lcPO +IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Cut_Validate2,loformset.GetHeaderText("LANG_PWBUNDL_Cut_Validate2",loformset.HeaderAlias)))      
      lcMsg = ;
      IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Cut_Validate1,loformset.GetHeaderText("LANG_PWBUNDL_Cut_Validate1",loformset.HeaderAlias))+ ;
      +" "+lcPO +" "+;
      IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Cut_Validate2,loformset.GetHeaderText("LANG_PWBUNDL_Cut_Validate2",loformset.HeaderAlias))
      =gfModalGen('INM34088B00000','DIALOG',lcMsg)      
      * N000682,1 HES Handle globalization issues [End  ]
      loThis.KeyTextbox.VALUE = ''
      RETURN .F.
    ENDIF
  ENDIF

  IF EMPTY(loThis.KeyTextbox.VALUE)
    RETURN .F.
  ENDIF

  *- A Value selected
  lcTktNo = loFormSet.Ariaform1.kbWorkOrder.KeyTextbox.VALUE
  SELECT MFGOPRDT
  =gfseek('M'+lcTktNo)
  LOCATE
  IF EOF()
    ** No ð found for ð.
    * N000682,1 HES Handle globalization issues [Start]
*!*	    =gfModalGen('INM42041B00000','DIALOG','Manufactring operation lines|the cutting ticket # &lcTktNo')
    =gfModalGen('INM42041B00000','DIALOG',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_Manuf_Validate,loformset.GetHeaderText("LANG_PWBUNDL_Manuf_Validate",loformset.HeaderAlias))+ lcTktNo)    
    * N000682,1 HES Handle globalization issues [End  ]
    loFormSet.Ariaform1.kbWorkOrder.KeyTextbox.VALUE = ''
    RETURN .F.
  ENDIF



  LOCATE FOR LINHOUSE
  IF EOF()
    ** No ð found for ð.
    * N000682,1 HES Handle globalization issues [Start]
*!*	    =gfModalGen('INM42041B00000','DIALOG','Inhouse manufactring operation lines|the cutting ticket # &lcTktNo')
    =gfModalGen('INM42041B00000','DIALOG',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_InhMan_Validate,loformset.GetHeaderText("LANG_PWBUNDL_InhMan_Validate",loformset.HeaderAlias))+ lcTktNo)    
    * N000682,1 HES Handle globalization issues [End  ]
    loFormSet.Ariaform1.kbWorkOrder.KeyTextbox.VALUE = ''
    RETURN .F.
  ENDIF

  *- Update the filter controls
  **
  DIMENSION loFormSet.laLot[1],loFormSet.laItem[1],loFormSet.laSize[1]
  STORE '' TO loFormSet.laLot[1],loFormSet.laItem[1],loFormSet.laSize[1]
  SELECT DISTINCT CLOTNO FROM MFGOPRDT INTO ARRAY loFormSet.laLot ORDER BY CLOTNO
  SELECT DISTINCT ITEM FROM MFGOPRDT INTO ARRAY loFormSet.laItem ORDER BY ITEM
  SELECT MFGOPRDT
  LOCATE
  =gfseek(ITEM,'STYLE') AND gfseek('S'+STYLE.SCALE,'SCALE')
  DIMENSION loFormSet.laSize[SCALE.CNT]
  FOR i = 1 TO SCALE.CNT
    lcI = STR(i,1)
    loFormSet.laSize[i] = SCALE.SZ&lcI
  ENDFOR

  *- Insert the 'ALL'
  *lfInsertElement(loFormSet,'laLot','ALL',1) && HES, The filter supposed no to work at Cut ticket level.
  lfInsertElement(loFormSet,'laItem','ALL',1)
  lfInsertElement(loFormSet,'laSize','ALL',1)

  WITH loFormSet.Ariaform1
    .cboLot.ROWSOURCE = 'Thisformset.laLot'
    .cboLot.VALUE = loFormSet.laLot[1]
    .cboLot.REFRESH()

    .cboItem.ROWSOURCE = 'Thisformset.laItem'
    .cboItem.VALUE = loFormSet.laItem[1]
    .cboItem.REFRESH()

    .cboSize.ROWSOURCE = 'Thisformset.laSize'
    .cboSize.VALUE = loFormSet.laSize[1]
    .cboSize.REFRESH()
  ENDWITH

  *E303136,1 TMI 13/05/2012 [Start] fill in the operation codes
  *-fill in the operation array
  DIMENSION loFormSet.lamfgcode[1]
  loFormSet.lamfgcode[1] = ''
  SELECT DISTINCT cOprCode FROM MFGOPRDT INTO ARRAY loFormSet.lamfgcode
  *E303136,1 TMI 13/05/2012 [End  ]

  *- Start filling the grid
  SELECT (loFormSet.lcGridFile)
  ZAP
  SET FILTER TO
  LOCATE

  IF !USED('POSLN')
    =gfOpenTable('POSLN','POSLN')
  ENDIF
  lnmajlen = LEN(gfitemmask('PM'))  && No. of major segments.

  SELECT MFGOPRDT
  *B609914,1 MMT 05/13/2012 PW bugs fixes for demo 05/13/2012[Start]
  lcCurrentMfgCode = ''
  *B609914,1 MMT 05/13/2012 PW bugs fixes for demo 05/13/2012[End]
  SCAN FOR LINHOUSE AND TRANCD = '1'
    *B609914,1 MMT 05/13/2012 PW bugs fixes for demo 05/13/2012[Start]
    lcCurrentMfgCode = MFGOPRDT.cOprCode
    =gfseek('PU'+MFGOPRDT.ctktno+MFGOPRDT.CINVTYPE+MFGOPRDT.ITEM,'POSLN')
    IF !gfseek(MFGOPRDT.CINVTYPE+PADR(SUBSTR(MFGOPRDT.ITEM,1,lnmajlen),19)+'M'+POSLN.CCSTSHT_ID+MFGOPRDT.cOprCode,'PWBOM','PWBOM')
      LOOP
    ENDIF
    *B609914,1 MMT 05/13/2012 PW bugs fixes for demo 05/13/2012[End]    
    SCATTER MEMVAR MEMO
    m.MFGCode = m.cOprCode
    m.cOprCode = ''
    SELECT (loFormSet.lcGridFile)
    =gfseek(m.item,'STYLE') AND gfseek('S'+STYLE.SCALE,'SCALE')
    m.csizes = LEFT('12345678',SCALE.CNT)
    SELECT pwbundl
    =gfseek(m.ctktno)
    SELECT (loFormSet.lcGridFile)
    FOR i = 1 TO SCALE.CNT
      lcI = STR(i,1)
      m.SIZENO = lcI
      m.Size = SCALE.SZ&lcI
      m.nbQty = m.NLOTQTY&lcI
      m.lotSize = lcI+m.Size
      m.trantype = 'B'
      lcKey = m.CLOTNO+m.item+lcI+m.Size
      m.lhasbundle = lfCheckBundl(lcKey)
      IF m.nbQty>0 AND !SEEK(m.CLOTNO+m.item+m.SIZENO,loFormSet.lcGridFile)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =IIF(m.lhasbundle,lfAddBundle(loFormSet,lcKey),'')
      ENDIF
    ENDFOR
  ENDSCAN

  SELECT (loFormSet.lcGridFile)
  LOCATE

  IF EOF()
    * N000682,1 HES Handle globalization issues [Start]
*!*	    =gfModalGen('INM42041B00000','DIALOG',"detail operation(s)"+"|"+ALLT(lcCurrentMfgCode)+" operation")
    =gfModalGen('INM42041B00000','DIALOG',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_detail_operation1,loformset.GetHeaderText("LANG_PWBUNDL_detail_operation1",loformset.HeaderAlias))+"|"+ALLT(lcCurrentMfgCode)+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PWBUNDL_detail_operation2,loformset.GetHeaderText("LANG_PWBUNDL_detail_operation2",loformset.HeaderAlias)))    
    * N000682,1 HES Handle globalization issues [End  ]
    *E303136,4 TMI 10/06/2012 [Start]
    RETURN .F.
    *E303136,4 TMI 10/06/2012 [End  ]
  ENDIF

  =lfSetGridSource(loFormSet)

  IF llNoChngMode
    RETURN
  ENDIF

  loFormSet.ChangeMode('E')

  *- End of lfvCuttkt.
  ************************************************************
  *! Name      : lfCheckBundl
  *! Developer : HES  - Hesham Elmasry
  *! Date      : 09/05/2012
  *! Purpose   : Check Bundle
  ************************************************************
FUNCTION lfCheckBundl
  PARAMETERS lcKey

  LOCAL lnSel
  lnSel = SELECT(0)
  SELECT pwbundl
  LOCATE FOR CLOTNO+ITEM+lotSize = lcKey AND !EMPTY(CBUNDLE)
  IF FOUND()
    SELECT(lnSel)
    RETURN .T.
  ELSE
    SELECT(lnSel)
    RETURN .F.
  ENDIF
  *- End of lfCheckBundl.
  ************************************************************
  *! Name      : lfAddBundle
  *! Developer : HES - Hesham Elmasry
  *! Date      : 09/05/2012
  *! Purpose   : Add Bundle lines
  ************************************************************
FUNCTION lfAddBundle
  PARAMETERS loFormSet,lcKey

  LOCAL lnSel
  lnSel = SELECT(0)

  SELECT pwbundl
  SCAN FOR CLOTNO+ITEM+lotSize = lcKey AND !EMPTY(CBUNDLE)
    m.CBUNDLE = CBUNDLE
    m.nbQty = nbQty

    SELECT (loFormSet.lcGridFile)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
  SELECT(lnSel)
  *- End of lfAddBundle.
  ************************************************************
  *! Name      : lfvGridFileFilter
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 09/05/2012
  *! Purpose   : Grid File Filter
  ************************************************************
FUNCTION lfvGridFileFilter
  PARAMETERS loFormSet
  LOCAL lnSlct,lcFilt,lcFl,i,lcI
  lnSlct = SELECT(0)

  DIMENSION laFl[3]
  SELECT (loFormSet.lcGridFile)
  SET FILTER TO
  LOCATE
  WITH loFormSet.Ariaform1
    laFl[1] = IIF(.cboLot.VALUE  = 'ALL','',"CLOTNO = '"+.cboLot.VALUE +"'")  &&as per Hesham the filter is not on the cut ticket level
    laFl[2] = IIF(.cboItem.VALUE = 'ALL','',"ITEM   = '"+.cboItem.VALUE+"'")
    laFl[3] = IIF(.cboSize.VALUE = 'ALL','',"SIZE   = '"+.cboSize.VALUE+"'")
  ENDWITH

  lcFilt = '.T.'
  FOR i = 1 TO ALEN(laFl,1)
    lcFilt = lcFilt + '.AND.' + IIF(!EMPTY(laFl[i]),laFl[i],'.T.')
  ENDFOR
  lcFilt = STRTRAN(lcFilt,'.AND..T.','')
  lcFilt = IIF(lcFilt=='.T.','',lcFilt)

  SET FILTER TO &lcFilt
  LOCATE
  loFormSet.Ariaform1.grdBundle.REFRESH()

  SELECT (lnSlct)
  *- End of lfvGridFileFilter.
  ************************************************************
  *! Name      : lfInsertElement
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 09/05/2012
  *! Purpose   : Insert Element TO one dim array array in a specified position
  ************************************************************
FUNCTION lfInsertElement
  PARAMETERS loFormSet,lcArr,lcVal,lnElm
  DIMENSION loFormSet.&lcArr[ALEN(loFormSet.&lcArr)+1]
  AINS(loFormSet.&lcArr,lnElm)
  loFormSet.&lcArr[lnElm] = lcVal

  *- End of lfInsertElement.
  ************************************************************
  *! Name      : lfAfterRowColChange
  *! Developer : HES - Hesham Elmasry
  *! Date      : 09/05/2012
  *! Purpose   : After row column change for Bundle grid
  ************************************************************
FUNCTION lfAfterRowColChange
  PARAMETERS loFormSet

  SELECT (loFormSet.lcGridFile)
  IF !EMPTY(ALLTRIM(CBUNDLE))
    loFormSet.Ariaform1.cmdRemove.ENABLED = .T. AND loFormSet.ActiveMode = 'E'
  ELSE
    loFormSet.Ariaform1.cmdRemove.ENABLED = .F.
  ENDIF

  *- End of MSG.




************************************************************
*! Name      : lfFormRefreshAll
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/06/2012
*! Purpose   : call the Form method RefreshAll
************************************************************
FUNCTION lfFormRefreshAll
PARAMETERS loFormSet
IF TYPE('loFormSet.lcCutTktNo') = 'C' AND !EMPTY(loFormSet.lcCutTktNo) AND loFormSet.activemode = 'V'
    loFormSet.oToolBar.cmdSelect.Visible =.F.  && in the dialog mode do not select new cutticket
    loFormSet.oToolBar.cmdEdit.Enabled =.T.
ENDIF
*- End of lfFormRefreshAll.

  ************************************************************
  *! Name      : MSG
  *! Developer : TMI - Tarek Mohamed Ibrahim
  *! Date      : 09/05/2012
  *! Purpose   : MSG
  ************************************************************
FUNCTION MSG
  PARAMETERS llsetp
  IF SYS(0)='DEV4 # tarek'
    ON ERROR
    _SCREEN.VISIBLE=.T.
    IF llsetp

    ENDIF
  ENDIF
  *- End of MSG.




