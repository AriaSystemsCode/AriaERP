*!*****************************************************************************************
*! Name      : MASCNRL.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/08/2007
*! Purpose   : Scan Material Rolls
*! Reference : N000591 - T20061226.0013
*!*****************************************************************************************
*! Modifications:
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*!*****************************************************************************************
#INCLUDE R:\ARIA4xp\SCREENS\MASCNRL.H

DO FORM (oAriaApplication.ScreenHome+"\MASCNRL.SCX")

*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : The Init Function
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet

WITH loFormSet
  DECLARE .laSetups[8,2]
  .laSetups[1,1]  = 'M_LINK_GL'
  .laSetups[2,1]  = 'M_DYELOT'
  .laSetups[3,1]  = 'M_CMTYPE1'
  .laSetups[4,1]  = 'M_CMTYPE2'
  .laSetups[5,1]  = 'M_CMTYPE3'
  .laSetups[6,1]  = 'M_CMTYPE4'
  .laSetups[7,1]  = 'M_CMTYPE5'
  .laSetups[8,1]  = 'M_WAREHOUS'
  .lenclrlen  = LEN(gfitemmask("PN", "", "0002"))
  .lnmajlength = LEN(gfItemMask('PM','', "0002"))
  .lcitmpic = gfItemMask("HI",'', "0002")
ENDWITH



DIMENSION laCopArr[1,1]
=ACOPY(loFormSet.laSetups,laCopArr)
=gfGetMemVar(@laCopArr, oAriaApplication.ActiveCompanyID)
=ACOPY(laCopArr,loFormSet.laSetups)
FOR ln=1 TO ALEN(loFormSet.laSEtups,1)
  IF loFormSet.laSetups[ln,2]=='F'
    loFormSet.lcTranType = SUBSTR(loFormSet.laSetups[ln,1],LEN(loFormSet.laSetups[ln,1]),1)
    EXIT
  ENDIF
ENDFOR


SET MULTILOCKS ON
DECLARE loFormSet.laMatWare[1,2]
STORE ' ' TO loFormSet.laMatWare
=gfOpenTable(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS INTO ARRAY loFormSet.laMatWare

=gfOpenTable(oAriaApplication.DataDir+'ITEMLOC',oAriaApplication.DataDir+'STYDYE','SH','ITEMLOC1')
=gfOpenTable(oAriaApplication.DataDir+'POSHDR',oAriaApplication.DataDir+'POSHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'ROLLS',oAriaApplication.DataDir+'ROLLS','SH')
=gfOpenTable(oAriaApplication.DataDir+'BOMCOST',oAriaApplication.DataDir+'BOMCOST','SH')
=gfOpenTable(oAriaApplication.DataDir+'ITEM',oAriaApplication.DataDir+'Style','SH','ITEM1')
=gfOpenTable(oAriaApplication.DataDir+'CTKTBOM',oAriaApplication.DataDir+'CTKTYP','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
=gfOpenTable(oAriaApplication.DataDir+'BOMLINE',oAriaApplication.DataDir+'BOMLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'ITEMJRNL',oAriaApplication.DataDir+'styinvjl','SH','ITEMJRNL1')
=gfOpenTable(oAriaApplication.DataDir+'UOM',oAriaApplication.DataDir+'UOMCODE','SH')


IF loFormSet.laSetups[1,2]='Y'
  loFormSet.lcGlDTemp  = gfTempName()
  IF !USED('GLDIST')
    =gfOpenTable(oAriaApplication.DataDir+'GLDIST','','SH')
  ENDIF
  SELECT GLDIST
  COPY STRUCTURE TO (oAriaApplication.WorkDir + loFormSet.lcGlDTemp)
  USE (oAriaApplication.WorkDir + loFormSet.lcGlDTemp) IN 0 EXCLUSIVE
ENDIF


loFormSet.lcBusDocu = 'P'
loFormSet.lcStyType = "U"
loFormSet.lcPosHdr  = gfTempName()
loFormSet.lcTempROLLS = gfTempName()
loFormSet.lcTmpCurs= gfTempName()

DIMENSION laTempStruct[2,4]
laTempStruct[1,1]='ROLL'
laTempStruct[1,2]='C'
laTempStruct[1,3]=20
laTempStruct[1,4]=''

laTempStruct[2,1]='lFound'
laTempStruct[2,2]='L'
laTempStruct[2,3]=1
laTempStruct[2,4]=''


=gfCrtTmp(loFormSet.lcTmpCurs ,@laTempStruct)

SELECT ROLLS
DIMENSION laRollStruct[1,18]
AFIELDS(laRollStruct)
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=gfCrtTmp(loFormSet.lcTempROLLS ,@laRollStruct,'CROLLID+CROLLITEM+COLOR+TRANCD',loFormSet.lcTempROLLS )
=gfCrtTmp(loFormSet.lcTempROLLS ,@laRollStruct,'CROLLID+Style+TRANCD',loFormSet.lcTempROLLS )
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

loFormSet.lcTmpRecv= gfTempName()
loFormSet.lcTmpRecv1=gftempname()

*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*DIMENSION laFileStruct[15,4]
DIMENSION laFileStruct[16,4]
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

laFileStruct[1,1]='CUTTKT'
laFileStruct[1,2]='C'
laFileStruct[1,3]=6
laFileStruct[1,4]=''

laFileStruct[2,1]='BYROLL'
laFileStruct[2,2]='L'
laFileStruct[2,3]=1
laFileStruct[2,4]=''

laFileStruct[3,1]='cRollid'
laFileStruct[3,2]='C'
laFileStruct[3,3]=20
laFileStruct[3,4]=''

laFileStruct[4,1]='Dyelot'
laFileStruct[4,2]='C'
laFileStruct[4,3]=10
laFileStruct[4,4]=''

laFileStruct[5,1]='FABRIC'
laFileStruct[5,2]='C'
laFileStruct[5,3]=7
laFileStruct[5,4]=''

laFileStruct[6,1]='cFabClr'
laFileStruct[6,2]='C'
laFileStruct[6,3]=6
laFileStruct[6,4]=''

laFileStruct[7,1]='cWareCode'
laFileStruct[7,2]='C'
laFileStruct[7,3]=6
laFileStruct[7,4]=''

laFileStruct[8,1]='cDesc'
laFileStruct[8,2]='C'
laFileStruct[8,3]=20
laFileStruct[8,4]=''

laFileStruct[9,1]='Req_Qty'
laFileStruct[9,2]='N'
laFileStruct[9,3]=10
laFileStruct[9,4]=3

laFileStruct[10,1]='Issue_Qty'
laFileStruct[10,2]='N'
laFileStruct[10,3]=10
laFileStruct[10,4]=3

laFileStruct[11,1]='REFERENCE'
laFileStruct[11,2]='C'
laFileStruct[11,3]=30
laFileStruct[11,4]=''

laFileStruct[12,1]='lIssue'
laFileStruct[12,2]='L'
laFileStruct[12,3]=1
laFileStruct[12,4]=''

laFileStruct[13,1]='nReturn'
laFileStruct[13,2]='N'
laFileStruct[13,3]=1
laFileStruct[13,4]=0

laFileStruct[14,1]='Aval'
laFileStruct[14,2]='N'
laFileStruct[14,3]=10
laFileStruct[14,4]=3

laFileStruct[15,1]='RecTyp'
laFileStruct[15,2]='C'
laFileStruct[15,3]=10
laFileStruct[15,4]=''

*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=gfCrtTmp(loFormSet.lcTmpRecv ,@laFileStruct,'cRollid + FABRIC + cFabClr + CUTTKT',loFormSet.lcTmpRecv )
laFileStruct[16,1]='STYLE'
laFileStruct[16,2]='C'
laFileStruct[16,3]=19
laFileStruct[16,4]=''
=gfCrtTmp(loFormSet.lcTmpRecv ,@laFileStruct,'cRollid + STyle + CUTTKT',loFormSet.lcTmpRecv )
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

SELECT (loFormSet.lcTmpRecv)
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*INDEX ON FABRIC  + cFabClr + CUTTKT TAG (loFormSet.lcTmpRecv1)
INDEX ON Style+ CUTTKT TAG (loFormSet.lcTmpRecv1)
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

WITH loFormSet
  .cBrowseTableDBEngine ='SQL'
  .nWorkArea        = 'POSHDR'
  .cBrowseAliasName = loFormSet.lcPosHdr
  .cBrowseKey       = loFormSet.lcBusDocu+loFormSet.lcStyType
  .cBrowseIndexExpression = "CBUSDOCU+CSTYTYPE+PO"
  .cBrowseIndexFields = "CBUSDOCU,CSTYTYPE,PO"
  .cBrowseTableName = 'POSHDR'
ENDWITH

SELECT POSHDR
=gfSqlRun("Select * from poshdr where cstytype = 'U' and cbusdocu ='P'",'PosHdr',.F.,loFormSet.lcPosHdr)


lcMjrHdr   = gfitemmask("HM", "", "0002")
WITH loFormSet.ariaFORM1.woDCUTNO
  .cbusinessdocumenttype =loFormSet.lcBusDocu
  .cworkordertype  = loFormSet.lcStyType
  .obrowsecursor   = loFormSet.lcPosHdr
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.cbrowsetitle    = LANG_CUT_TITLE
  .cbrowsetitle    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUT_TITLE,loFormSet.GetHeaderText("LANG_CUT_TITLE",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .cbrowsefields   = "PO        :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CTHEADER,loFormSet.GetHeaderText("LANG_CTHEADER",loFormSet.HeaderAlias))+"' ,"+;
                                                     "STYLE     :H='"+lcMjrHdr            +"' ,"+;
                                                     "STATUS    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATUS,loFormSet.GetHeaderText("LANG_STATUS",loFormSet.HeaderAlias))  +"' ,"+;
                                                     "ENTERED   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENTERED,loFormSet.GetHeaderText("LANG_ENTERED",loFormSet.HeaderAlias)) +"' ,"+;
                                                     "COMPLETE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPDATE,loFormSet.GetHeaderText("LANG_COMPDATE",loFormSet.HeaderAlias))+"' ,"+;
                                                     "SEASON    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SEASON,loFormSet.GetHeaderText("LANG_SEASON",loFormSet.HeaderAlias))  +"' ,"+;
                                                     "CDIVISION :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIVISION,loFormSet.GetHeaderText("LANG_DIVISION",loFormSet.HeaderAlias))+"' ,"+;
                                                     "NSTYORDER :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDGET,loFormSet.GetHeaderText("LANG_BUDGET",loFormSet.HeaderAlias))  +"' ,"+;
                                                     "RECEIVE   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECEIVE,loFormSet.GetHeaderText("LANG_RECEIVE",loFormSet.HeaderAlias)) +"' ,"+;
                                                     "DAMAGE    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DAMAGED,loFormSet.GetHeaderText("LANG_DAMAGED",loFormSet.HeaderAlias)) +"' ,"+;
													 "OPEN      :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,loFormSet.GetHeaderText("LANG_OPEN",loFormSet.HeaderAlias))    +"'"



ENDWITH
lfAddControlSource(loFormSet)
*!*************************************************************
*! Name      : lfvGetFil
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : The Get File button valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvGetFil()
*!*************************************************************
FUNCTION lfvGetFil
PARAMETERS loFormSet
*-- Get the text file hold the Rolls.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcImpFile = GETFILE('TXT', LANG_SEl_File, LANG_SELECT,1)
loFormSet.lcImpFile = GETFILE('TXT', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SEl_File,loFormSet.GetHeaderText("LANG_SEl_File",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT,loFormSet.GetHeaderText("LANG_SELECT",loFormSet.HeaderAlias)),1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

IF !FILE(loFormSet.lcImpFile)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_File_Not_Exist)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File_Not_Exist,loFormSet.GetHeaderText("LANG_File_Not_Exist",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.lcImpFile = ''
ENDIF
loFormSet.ariaform1.txtFilePath.Value = loFormSet.lcImpFile
RETURN


*!*************************************************************
*! Name      : lfChangeMode
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : The change Mode Function
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

DO CASE
  CASE loFormSet.ActiveMode = 'S'
    WITH loFormSet.AriaForm1
      .ogAddoption.enabled = .T.
      .ogAddoption.Ariaoptionbutton1.Enabled = .T.
      .ogAddoption.Ariaoptionbutton2.Enabled = .T.
      .obtnMat.enabled = .T.
      .optnRollID.enabled = .T.
      .WodCutNo.enabled = .T.

      IF .ogAddoption.Ariaoptionbutton1.Value = 0
        .ogAddoption.Ariaoptionbutton2.Value = 1
        .ogAddoption.Ariaoptionbutton1.Value = 0
        .cmdGetfile.enabled = .F.
        .txtFilePath.Value =""
      ELSE
        .ogAddoption.Ariaoptionbutton2.Value = 0
        .ogAddoption.Ariaoptionbutton1.Value = 1
        .cmdGetfile.Enabled = .T.
        .txtFilePath.Value = IIF(!EMPTY(loFormSet.lcImpFile),loFormSet.lcImpFile,"")
      ENDIF
      .optnRollID.value =1
      .obtnMat.value = 0
      .cboLocation.Enabled = .F.
      .cboLocation.value =loFormSet.laMatWare[1,2]
      .dtpDate.value = oAriaApplication.SystemDate
      .dtpDate.Enabled = .F.
      .WodCutNo.Enabled = .T.
      .cmdCollect.Enabled = .T.
      .CmdRemove.enabled=.F.
      .CmdNew.enabled=.F.
      .kbRollID.enabled=.F.
      .txtQty.enabled=.F.
      .txtQty.Value = 0
      IF USED(loFormSet.lcTmpRecv)
        SELECT(loFormSet.lcTmpRecv)
        ZAP
      ENDIF
      .grdRolls.readonly = .T.
    ENDWITH

  CASE loFormSet.ActiveMode = 'V'
    IF lfChkCtkt(loFormSet)
     lcSelectedFile =""
     IF loFormSet.ariaFORM1.ogAddoption.Ariaoptionbutton1.Value = 1
       lcSelectedFile=loFormSet.ariaform1.txtFilePath.Value
     ENDIF

     loFormSet.ChangeMode('S')
     loFormSet.ariaform1.WodCutNo.keyTextBox.value =EVALUATE(loFormSet.lcPoshdr+'.po')

     IF loFormSet.ariaFORM1.ogAddoption.Ariaoptionbutton1.Value = 1 AND !EMPTY(lcSelectedFile)
       loFormSet.ariaform1.txtFilePath.Value = lcSelectedFile
     ENDIF
    ELSE
      loFormSet.ChangeMode('S')
      loFormSet.ariaform1.WodCutNo.keyTextBox.value =''
    ENDIF

  CASE loFormSet.ActiveMode = 'A'
    WITH  loFormSet.ariaFORM1
      .ogAddoption.Enabled = .F.
      .optnRollID.Enabled = .F.
      .obtnMat.Enabled = .F.
      .WodCutNo.Enabled = .T.
      .WodCutNo.keyTextBox.value =EVALUATE(loFormSet.lcPoshdr+'.po')
      .cboLocation.Enabled = .T.
      .dtpDate.Enabled = .T.
      .CmdRemove.enabled=.F.
      .CmdNew.enabled=.F.
      .kbRollID.enabled=.F.
      .txtQty.enabled=.F.
      .txtQty.Value = 0
      .grdRolls.readonly = .T.
      .cmdGetfile.enabled = .F.
    ENDWITH

ENDCASE


*!*************************************************************
*! Name      : lfvCollect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : The collect data
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTkt()
*!*************************************************************
FUNCTION lfvCollect
PARAMETERS loFormset


lcCuttkt= loFormSet.ariaFORM1.woDCUTNO.KeyTextBox.value
IF EMPTY(lcCuttkt)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpTxt = LANG_CutTkt +"|" + IIF(loFormSet.ariaFORM1.ogAddoption.Ariaoptionbutton1.Value = 1 , LANG_Collect , LANG_StartScan)
lcTmpTxt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CutTkt,loFormSet.GetHeaderText("LANG_CutTkt",loFormSet.HeaderAlias)) +"|" + IIF(loFormSet.ariaFORM1.ogAddoption.Ariaoptionbutton1.Value = 1 , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias)) , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_StartScan,loFormSet.GetHeaderText("LANG_StartScan",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *** You have to enter carton/CT before collecting items. ***
  =gfModalGen("INM34142B00000" , "DIALOG" , lcTmpTxt)
  RETURN
ELSE
  IF !gfSEEK('PU'+lcCuttkt,'POSHDR')  .OR. !(POSHDR.Status $ 'AO')
    RETURN
  ENDIF
ENDIF

lcImpFile =loFormSet.ariaform1.txtFilePath.Value
*-- If import data from text file, & there was no text file selected.
IF loFormSet.ariaFORM1.ogAddoption.Ariaoptionbutton1.Value = 1 .AND. EMPTY(lcImpFile)
  *** You have to select the file you will use to import your items. ***
  =gfModalGen("INM34143B00000" , "DIALOG")
  RETURN
ENDIF


IF loFormSet.ariaFORM1.ogAddoption.Ariaoptionbutton1.Value = 1
  SELECT (loFormSet.lcTmpCurs)
  APPEND FROM (lcImpFile) TYPE DELIMITED
  GO TOP
  IF !EOF()
    llCollect = .T.
    SELECT (loFormSet.lcTmpCurs)
    lcTmpCurs=loFormSet.lcTmpCurs
    SCAN
      llIgnore = .F.
      =lfvRoll(loFormSet,&lcTmpCurs..Roll)
      *-- Open files
    ENDSCAN
    IF llIgnore
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B00000',.F.,.F.,.F.,LANG_Roll_Ignore)
=gfModalGen('TRM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Roll_Ignore,loFormSet.GetHeaderText("LANG_Roll_Ignore",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      lcRolls = ''
      loFormSet.ChangeMode('A')

      WITH loFormSet.ariaform1
        .WodCutNo.Enabled = .F.
        .cboLocation.Enabled = .T.
        .dtpDate.Enabled = .T.
        .CmdRemove.Enabled=.F.
        .CmdNew.Enabled=.T.
        .kbRollID.Enabled =.F.
        .txtQty.Enabled =.F.
        .txtQty.Value = 0
      ENDWITH

      RETURN
    ENDIF
  ENDIF
  SELECT (loFormSet.lcTmpCurs)
  ZAP
  loFormSet.ChangeMode('A')
  WITH loFormSet.ariaform1
    .WodCutNo.Enabled = .F.
    .cboLocation.Enabled = .T.
    .dtpDate.Enabled = .T.
    .CmdRemove.Enabled=.F.
    .CmdNew.Enabled=.T.
    .kbRollID.Enabled =.F.
    .txtQty.Enabled =.F.
    .txtQty.Value = 0
  ENDWITH

ELSE
  lnLineCnt  = 0
  llCollect  = .T.
  loFormSet.ChangeMode('A')
  WITH loFormSet.ariaform1
    .WodCutNo.Enabled = .F.
    .cboLocation.Enabled = .T.
    .dtpDate.Enabled = .T.
    .CmdRemove.enabled=.F.
    .CmdNew.enabled=.T.
    .kbRollID.enabled=.F.
    .txtQty.enabled=.F.
    .txtQty.Value = 0
  ENDWITH
ENDIF



*!*************************************************************
*! Name      : lfvRoll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : validate Roll
*!*************************************************************
FUNCTION lfvRoll
PARAMETER loFormSet,lcImpRoll



*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
loFormSet.ariaform1.cboLocation.BoundColumn = 2
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

lcRolls    = IIF(EMPTY(loFormSet.ariaFORM1.kbRollID.keytextbox.Value),SPACE(20),loFormSet.ariaFORM1.kbRollID.keytextbox.Value)
lcRolls   = IIF(TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll),lcImpRoll,lcRolls)

llFound = .F.
llEnaQty   = .T.
*-- Open files

lenClrlen = loFormset.lenclrlen
lnMajLength =loFormset.lnmajlength
lcStyPic  = loFormset.lcitmpic
lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)

llUseDye = IIF(loFormset.laSetups[2,2] = 'N' , .F. , .T.)

lcWareCode = loFormSet.ariaform1.cboLocation.VALUE
lcCuttkt   = loFormSet.ariaFORM1.woDCUTNO.KeyTextBox.value

IF !gfSEEK(lcRolls , 'ROLLS') .OR. (gfSEEK(lcRolls , 'ROLLS')  AND ROLLS.nQtyBal=0)

  IF TYPE('lcImpRoll')='C'
    llIgnore = .T.
    RETURN
  ENDIF

  SELECT ROLLS
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *lcTitle= 'Select Roll '
  lcTitle= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MASCNRL_SELROLL,loFormSet.GetHeaderText("LANG_MASCNRL_SELROLL",loFormSet.HeaderAlias))
  *N000682,1 MMT 12/09/2012 Globalization changes[end]
  DIMENSION  laTempData[4]
  STORE '' TO laTempData

  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
  *lcBrFields =  [cRollId :H= 'Roll ID',] +;
  IIF(llUseDye  AND FABRIC.cDye_Flg = 'Y', [Dyelot :H='Dyelot',]  ,[])  +;
  [NQtyBal :H= 'On Hand' , ] + ;
  [cRollItem :H= 'Fabric' , ] +;
  [Color :H= 'Color']
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	  lcBrFields =  [cRollId :H= 'Roll ID',] +;
*!*	  IIF(llUseDye  AND FABRIC.cDye_Flg = 'Y', [Dyelot :H='Dyelot',]  ,[])  +;
*!*	  [NQtyBal :H= 'On Hand' , ] + ;
*!*	  [Style  :H= 'Fabric'  ]
  lcBrFields =  [cRollId :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MASCNRL_ROLLID,loFormSet.GetHeaderText("LANG_MASCNRL_ROLLID",loFormSet.HeaderAlias))+[',] +;
  IIF(llUseDye  AND FABRIC.cDye_Flg = 'Y', [Dyelot :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MASCNRL_DYELOT,loFormSet.GetHeaderText("LANG_MASCNRL_DYELOT",loFormSet.HeaderAlias))+[',]  ,[])  +;
  [NQtyBal :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MASCNRL_ONHAND,loFormSet.GetHeaderText("LANG_MASCNRL_ONHAND",loFormSet.HeaderAlias))+[' , ] + ;
  [Style  :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MASCNRL_FABRIC,loFormSet.GetHeaderText("LANG_MASCNRL_FABRIC",loFormSet.HeaderAlias))+['  ]
  *N000682,1 MMT 12/09/2012 Globalization changes[end]
  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

  IF USED(loFormSet.lcTempROLLS)
    SELECT(loFormSet.lcTempROLLS)
    ZAP
  ENDIF

  IF gfSeek('M' + lcCutTkt , 'CTKTBOM')
    SELECT CTKTBOM
    SCAN
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *IF gfSEEK(PADR(SUBSTR(CTKTBOM.Item,1,lnmajlength),7)+PADR(RIGHT(CTKTBOM.Item,lenclrlen),6),'rolls','ROLLITEM')
      IF gfSEEK(CTKTBOM.Item,'rolls','ROLLITEM')
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
        SELECT Rolls

        *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        *SCAN REST WHILE ;
           CROLLITEM+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD =PADR(SUBSTR(CTKTBOM.Item,1,lnmajlength),7)+PADR(RIGHT(CTKTBOM.Item,lenclrlen),6);
           FOR cWareCode = lcWareCode AND TRANCD = "1" .AND. ROLLS.nQtyBal<>0
        SCAN REST WHILE ;
           Style+CWARECODE+DYELOT+CROLLID+TRANCD =CTKTBOM.Item;
           FOR cWareCode = lcWareCode AND TRANCD = "1" .AND. ROLLS.nQtyBal<>0
		*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
		
          SCATTER MEMO MEMVAR
          SELECT(loFormSet.lcTempROLLS)
          APPEND BLANK
          GATHER MEMO MEMVAR
        ENDSCAN
      ENDIF
    ENDSCAN
    SELECT(loFormSet.lcTempROLLS)
    GO TOP
    IF !EOF()
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	      = ARIABROW([],;
*!*	      lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,.F.,'' , 'cRollId,cRollItem,Color,Dyelot' , 'laTempData')
      = ARIABROW([],;
      lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,.F.,'' , 'cRollId,cRollItem,Color,Dyelot,Style' , 'laTempData')
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    ELSE
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_No_Rec_Brow)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Rec_Brow,loFormSet.GetHeaderText("LANG_No_Rec_Brow",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.ariaFORM1.kbRollID.keytextbox.Value = ''
      RETURN .f.
    ENDIF
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_No_Rec_Brow)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Rec_Brow,loFormSet.GetHeaderText("LANG_No_Rec_Brow",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.ariaFORM1.kbRollID.keytextbox.Value = ''
    RETURN .F.
  ENDIF


  lcTempRoll =loFormSet.lcTempROLLS
  IF !EMPTY(laTempData)
    lcFabric = &lcTempRoll..cRollItem
    lcColor  = &lcTempRoll..Color
    lcDyeObj = &lcTempRoll..Dyelot

    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    lcSTyle  = &lcTempRoll..Style
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

    llCollect  = .F.
    llFound = .F.
    IF gfSEEK('M' + lcCutTkt  , 'BOMLINE','mfgopr')
    *+ PADR(Rolls.cRollItem , 19)
      SELECT bomline
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	      SCAN REST WHILE cimtyp+ ctktno+ coprcode+ ctype+ cinvtype+ style+ cinvtypc+ mfgcode+item ='M' + lcCutTkt;
*!*	        FOR ITEM = Rolls.cRollItem
      SCAN REST WHILE cimtyp+ ctktno+ coprcode+ ctype+ cinvtype+ style+ cinvtypc+ mfgcode+item ='M' + lcCutTkt;
        FOR ITEM = Rolls.Style
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
        lcFabric = &lcTempRoll..cRollItem
        lcColor  = &lcTempRoll..Color
        lcDyeObj = &lcTempRoll..Dyelot

        *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        lcSTyle  = &lcTempRoll..Style
        *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

        llFound = .T.
      ENDSCAN
    ENDIF
    lcRolls  = laTempData[1]
    lcFabric = laTempData[2]
    lcColor  = laTempData[3]
    lcDyeObj = laTempData[4]
	
	*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    lcSTyle  = laTempData[5]
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

    llFound = .T.
    loFormSet.ariaFORM1.kbRollID.keytextbox.Value =lcRolls
  ELSE
    loFormSet.ariaFORM1.kbRollID.keytextbox.Value =''
    lcRolls  = ""
    lcFabric = ""
    lcColor  = ""
    lcDyeObj = ""
    loFormSet.ariaFORM1.kbRollID.keytextbox.Value =lcRolls
    RETURN
  ENDIF
ELSE
  SELECT ROLLS
  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *LOCATE REST WHILE cROllID + cRollItem + cTktNo=;
                    lcRolls;
              FOR cWareCode = lcWareCode
  LOCATE REST WHILE CROLLID+STYLE+TRANCD=;
                    lcRolls;
              FOR cWareCode = lcWareCode
  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

  IF FOUND()
    lcFabric = Rolls.cRollItem
    lcColor  = Rolls.Color
    lcDyeObj = Rolls.Dyelot

    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    lcSTyle  = Rolls.Style
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

    llCollect  = .F.
    llFound = .F.
    *IF gfSEEK('M' + lcCutTkt + PADR(Rolls.cRollItem , 19) , 'BOMLINE')
    IF gfSEEK('M' + lcCutTkt  , 'BOMLINE','mfgopr')
      SELECT bomline
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	      SCAN REST WHILE cimtyp+ ctktno+ coprcode+ ctype+ cinvtype+ style+ cinvtypc+ mfgcode+item ='M' + lcCutTkt;
*!*	        FOR ITEM = Rolls.cRollItem
      SCAN REST WHILE cimtyp+ ctktno+ coprcode+ ctype+ cinvtype+ style+ cinvtypc+ mfgcode+item ='M' + lcCutTkt;
        FOR ITEM = Rolls.Style
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
        lcFabric = Rolls.cRollItem
        lcColor  = Rolls.Color
        lcDyeObj = Rolls.Dyelot

        *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        lcSTyle  = &lcTempRoll..Style
        *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

        llFound = .T.
      ENDSCAN
    ELSE
      llFound = .F.
    ENDIF
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Roll_Assign_Loc)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Roll_Assign_Loc,loFormSet.GetHeaderText("LANG_Roll_Assign_Loc",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcRolls   = ''
    loFormSet.ariaFORM1.kbRollID.keytextbox.Value =''
    RETURN .F.
  ENDIF
ENDIF

IF !llFound
  IF (TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll))
    llIgnore = .T.
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Roll_Assign_CtTkt)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Roll_Assign_CtTkt,loFormSet.GetHeaderText("LANG_Roll_Assign_CtTkt",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.ariaFORM1.kbRollID.keytextbox.Value = ''
    RETURN .F.
  ENDIF
  loFormSet.ariaFORM1.kbRollID.keytextbox.Value =''
  lcRolls = ""
  RETURN .F.
ENDIF
lcTmpRecv =loFormSet.lcTmpRecv
SELECT(loFormSet.lcTmpRecv)
SET ORDER TO (loFormSet.lcTmpRecv)
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*IF SEEK(PADR(lcRolls , 20) + lcFabric +lcColor +lcCuttkt, loFormSet.lcTmpRecv)
IF SEEK(PADR(lcRolls , 20) + lcStyle+lcCuttkt, loFormSet.lcTmpRecv)
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
    llIgnore = .T.
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Roll_Scanned_Before)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Roll_Scanned_Before,loFormSet.GetHeaderText("LANG_Roll_Scanned_Before",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

     loFormSet.ariaFORM1.kbRollID.keytextbox.Value = ''
    RETURN .F.
  ENDIF
  lcRolls  = ''
  loFormSet.ariaFORM1.kbRollID.keytextbox.Value =''
  lnReturn = 0
  loFormSet.ariaFORM1.cmdNew.enabled = .T.
  loFormSet.ariaFORM1.kbRollID.Enabled = .F.
  RETURN
ENDIF
*--- Start Get the Available.
SELECT ROLLS
lnToIss = 0
lnCAvail= 0
*SET ORDER TO lcRolIndx
*--- cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode
lenClrlen = loFormset.lenclrlen
lnMajLength =loFormset.lnmajlength
lcStyPic  = loFormset.lcitmpic
lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=gfSEEK("***** N/A *****     " + lcFabric,'rolls','ROLLS')
=gfSEEK("***** N/A *****     " + lcSTYLE,'rolls','ROLLS')
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
SELECT Rolls

*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*LOCATE REST WHILE CROLLID+CROLLITEM+COLOR+TRANCD ="***** N/A *****     " + lcFabric+lcColor;
  FOR cTktNo = lcCutTkt AND Dyelot = lcDyeObj AND cWareCode = lcWareCode AND (lfCurrAppl() < Rolls.nQtyBal)
LOCATE REST WHILE CROLLID+STyle+TRANCD ="***** N/A *****     " + lcStyle;
  FOR cTktNo = lcCutTkt AND Dyelot = lcDyeObj AND cWareCode = lcWareCode AND (lfCurrAppl() < Rolls.nQtyBal)
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

IF FOUND()
*IF SEEK( "***** N/A *****     " + lcFabric+ lcCutTkt +lcColor+lcDyeObj +lcWareCode) .AND.  (lfCurrAppl() < Rolls.nQtyBal)
  lcFullItem = PADR(lcFabric ,lnMajLength )+lcSepar   +PADR(lcColor ,lenClrlen)

  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *  IF gfSEEK('M' + lcCutTkt + '0002'+lcFullItem , 'CTKTBOM')
 IF gfSEEK('M' + lcCutTkt + '0002'+lcStyle, 'CTKTBOM')
  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    SELECT(lcTmpRecv)
    IF Rolls.nQty = 0
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Roll_Zero_Qty)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Roll_Zero_Qty,loFormSet.GetHeaderText("LANG_Roll_Zero_Qty",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.ariaFORM1.kbRollID.keytextbox.Value =''
      RETURN .F.
    ELSE
      lnToApply = Rolls.nQtyBal
      lnCurrApply = lfCurrAppl()
      SELECT ROLLS

      *=SEEK(lcRolls + lcFabric+ SPACE(06) +lcColor+lcDyeObj +lcWareCode)

      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *=gfSEEK(lcRolls+ lcFabric,'rolls','ROLLS')
      =gfSEEK(lcRolls+ lcStyle,'rolls','ROLLS')
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

      SELECT Rolls

      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      LOCATE REST WHILE CROLLID+Style+TRANCD =lcRolls+ lcStyle;
          FOR cTktNo = SPACE(06)  AND Dyelot = lcDyeObj AND cWareCode = lcWareCode
	  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
	
      lnAvailable = Rolls.nQtyBal

      *=SEEK( "***** N/A *****     " + lcFabric+ lcCutTkt +lcColor+lcDyeObj +lcWareCode)

      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *=gfSEEK("***** N/A *****     " + lcFabric,'rolls','ROLLS')
      =gfSEEK("***** N/A *****     " + lcStyle,'rolls','ROLLS')
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

      SELECT Rolls

      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	      LOCATE REST WHILE CROLLID+CROLLITEM+COLOR+TRANCD ="***** N/A *****     " + lcFabric+lcColor;
*!*	        FOR cTktNo = lcCutTkt AND Dyelot = lcDyeObj AND cWareCode = lcWareCode
      LOCATE REST WHILE CROLLID+Style+TRANCD ="***** N/A *****     " + lcStyle;
        FOR cTktNo = lcCutTkt AND Dyelot = lcDyeObj AND cWareCode = lcWareCode
      *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

      lnDfQty2Ap  = MIN(lnToApply - lnCurrApply, lnAvailable)
      IF lnDfQty2Ap > 0
        SELECT(lcTmpRecv)
        APPEND BLANK
        REPLACE CUTTKT    WITH lcCutTkt,;
                cDesc     WITH "" ,;
                Req_Qty   WITH CTKTBOM.Req_Qty,;
                Issue_Qty WITH lnDfQty2Ap,;
                Fabric    WITH lcFabric,;
                cFabClr   WITH lcColor,;
                cWareCode WITH lcWareCode,;
                Aval      WITH lnAvailable ,;
                lIssue    WITH .F.,;
                Dyelot    WITH lcDyeObj,;
                cRollid   WITH lcRolls,;
                RecTyp    WITH "Apply"

        *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        REPLACE Style WITH lcStyle
        *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

      ELSE
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Avail_Qty)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Avail_Qty,loFormSet.GetHeaderText("LANG_Avail_Qty",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
ELSE
  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *IF gfSEEK(lcRolls + lcFabric , 'ROLLS')
  IF gfSEEK(lcRolls + lcStyle, 'ROLLS')
  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    lnCAvail = lfGetAval('R')
    lnCAvail = IIF(lnCAvail <0,0,lnCAvail)
    SELECT CTKTBOM
    gfSetorder('CTKTYP')
    lcFullItem = PADR(lcFabric ,lnMajLength )+lcSepar+PADR(lcColor ,lenClrlen)
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *IF gfSEEK('M' + lcCutTkt + '0002'+lcFullItem , 'CTKTBOM')
    IF gfSEEK('M' + lcCutTkt + '0002'+lcStyle , 'CTKTBOM')
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
      SELECT CTKTBOM
      IF CTKTBOM.Req_Qty > CTKTBOM.Used_Qty
        lnToIss = MIN((CTKTBOM.Req_Qty - CTKTBOM.Used_Qty) ,lnCAvail)
      ELSE
        lnToIss = MAX(0 ,lnCAvail)
      ENDIF
      SELECT(lcTmpRecv)
      IF lnToIss = 0
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Issue_Zero_Qty)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Issue_Zero_Qty,loFormSet.GetHeaderText("LANG_Issue_Zero_Qty",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        loFormSet.ariaFORM1.kbRollID.keytextbox.Value =''
        RETURN .F.
      ELSE
        APPEND BLANK
        REPLACE CUTTKT    WITH lcCutTkt,;
                cDesc     WITH "" ,;
                Req_Qty   WITH CTKTBOM.Req_Qty,;
                Issue_Qty WITH lnToIss,;
                Fabric    WITH lcFabric,;
                cFabClr   WITH lcColor,;
                cWareCode WITH lcWareCode,;
                Aval      WITH lnCAvail,;
                lIssue    WITH .F.,;
                Dyelot    WITH lcDyeObj,;
                cRollid   WITH lcRolls,;
                RecTyp    WITH "Issue_Apply"

         *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
         REPLACE Style WITH lcStyle
		 *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

      ENDIF
    ENDIF
  ENDIF
ENDIF
SELECT(lcTmpRecv)
loFormSet.ariaform1.txtQty.Value = &lcTmpRecv..Issue_Qty
loFormSet.ariaform1.txtQty.enabled = .T.
lnReco = RECNO()
STORE 0 TO lnNonDele
COUNT FOR !DELETED() TO lnNonDele
IF lnNonDele > 0
  loFormset.ariaForm1.cmdRemove.Enabled = .T.
ELSE
  loFormset.ariaForm1.cmdRemove.Enabled = .F.
ENDIF
IF BETWEEN(lnReco,1,RECCOUNT())
  GO lnReco
ENDIF
loFormSet.ariaform1.grdRolls.refresh()

*!*************************************************************
*! Name      : lfCurrAppl
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : Current app.
*!*************************************************************
FUNCTION lfCurrAppl

PRIVATE lnOldAls , lnToRet
lnOldAls = SELECT(0)
SELECT (loFormSet.lcTmpRecv)
SET ORDER TO (loFormSet.lcTmpRecv1)
*--- cRollid + FABRIC + cFabClr + CUTTKT
lnToRet = 0
*--- lcFabric+ lcCutTkt +lcColor+lcDyeObj +lcWareCode
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	=SEEK(lcFabric + lcColor)
*!*	SCAN REST WHILE FABRIC + cFabClr= lcFabric + lcColor;
*!*	          FOR RecTyp = "Apply" .AND. Dyelot=lcDyeObj
=SEEK(lcStyle)
SCAN REST WHILE Style+ CUTTKT  = lcStyle;
          FOR RecTyp = "Apply" .AND. Dyelot=lcDyeObj
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  lnToRet = lnToRet + Issue_Qty
ENDSCAN
SELECT(lnOldAls)
RETURN (lnToRet)

*!*************************************************************
*! Name      : lfGetCost
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : get Cost
*!*************************************************************
FUNCTION lfGetCost

*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*PARAMETER lcItmRol , lcItem , lcClr , lcWare , lcDye
PARAMETER lcItmRol , lcItem , lcWare , lcDye
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

PRIVATE lnOldAls , lnToRet
lnOldAls = SELECT(0)
SELECT ROLLS
gfSetorder('ROLLS')
lnToRet = 0
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*IF gfSEEK(lcItmRol + lcItem + lcClr)
IF gfSEEK(lcItmRol + lcItem )
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  lcTempResSes = cRSession
  SELECT ITEMJRNL1
  *--- cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
  lenClrlen = loFormset.lenclrlen
  lnMajLength =loFormset.lnmajlength
  lcStyPic  = loFormset.lcitmpic
  lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)

  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *lcFullItem = PADR(lcItem ,lnMajLength )+lcSepar   +PADR(lcClr,lenClrlen)
  *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

  SELECT ITEMJRNL1

*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  IF gfSEEK('0002'+lcFullItem + lcWare + lcTempResSes,'ITEMJRNL1','STYINVJL')
*!*	    SCAN REST WHILE cinvtype+ style+ cwarecode+csession+DTOC(dtrdate)+ctrcode+STR(lineno,6)='0002'+lcFullItem + lcWare + lcTempResSes;
*!*	      FOR  ITEMJRNL1.CDYELOT =lcDye
IF gfSEEK('0002'+lcItem + lcWare + lcTempResSes,'ITEMJRNL1','STYINVJL')
    SCAN REST WHILE cinvtype+ style+ cwarecode+csession+DTOC(dtrdate)+ctrcode+STR(lineno,6)='0002'+lcItem + lcWare + lcTempResSes;
      FOR  ITEMJRNL1.CDYELOT =lcDye
*!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

      lnToRet = ITEMJRNL1.nUntCstBuy
    ENDSCAN
  ENDIF
ENDIF
SELECT(lnOldAls)
RETURN (lnToRet)

*!*************************************************************
*! Name      : lfGetAval
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : Add control source
*!*************************************************************
FUNCTION lfGetAval
PARAMETER lcTyp
PRIVATE lnOldAls , lnAvail
lnOldAls = SELECT(0)

IF lcTyp='F'
  lnAvail = 0
  SELECT ITEMLOC1
  gfsetorder('STYDYE')
  *--- fabric+color+cwarecode+dyelot
  lcFullItem = PADR(lcFabric ,lnMajLength )+lcSepar   +PADR(lcColor ,lenClrlen)

  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
  *IF gfSEEK('0002'+lcFullItem + lcWareCode + lcDyeObj)
  IF gfSEEK('0002'+lcStyle + lcWareCode + lcDyeObj)
  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]

    lnAvail = ITEMloc1.TOTSTK
  ENDIF
  SELECT (lcTmpRecv)
  SET ORDER TO TAG (LOFORMSET.lcTmpRecv1)

  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
*!*	  IF SEEK(lcFabric +lcColor +lcCuttkt)
*!*	    SCAN REST WHILE FABRIC  + cFabClr + CUTTKT=;
*!*	                    lcFabric +lcColor +lcCuttkt;
*!*	              FOR Dyelot = lcDyeObj
  IF SEEK(lcStyle+lcCuttkt)
    SCAN REST WHILE Style + CUTTKT=;
                    lcStyle+lcCuttkt;
              FOR Dyelot = lcDyeObj
  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]

      lnAvail = MIN(lnAvail-Issue_Qty,0)
    ENDSCAN
  ENDIF
ELSE
  SELECT ROLLS
  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
  *LOCATE REST WHILE CROLLID+CROLLITEM+COLOR+TRANCD =  lcRolls + lcFabric+lcColor AND TranCd = '1';
                    FOR Dyelot = lcDyeObj
  LOCATE REST WHILE CROLLID+Style+TRANCD =  lcRolls + lcStyle AND TranCd = '1';
                    FOR Dyelot = lcDyeObj
  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [eND]
  lnAvail = nQtyBal
  SELECT (lcTmpRecv)
  SET ORDER TO TAG (LOFORMSET.lcTmpRecv1)
  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
*!*	  IF SEEK(lcFabric +lcColor +lcCuttkt)
*!*	    SUM REST Issue_Qty WHILE FABRIC  + cFabClr + CUTTKT=;
*!*	                    lcFabric +lcColor +lcCuttkt FOR cRollID = lcRolls TO lnTotRollIs
  IF SEEK(lcStyle+lcCuttkt)
    SUM REST Issue_Qty WHILE Style+ CUTTKT=;
                    lcStyle +lcCuttkt FOR cRollID = lcRolls TO lnTotRollIs
  * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [eND]

    lnAvail = lnAvail - lnTotRollIs
  ENDIF
ENDIF
SELECT(lnOldAls)
RETURN (lnAvail)

*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/11/2007
*! Purpose   : Add control source
*!*************************************************************
FUNCTION lfAddControlSource
PARAMETERS loFormSet
WITH loFormSet.ariaform1.grdRolls
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpRecv
  .column9.ControlSource =loFormSet.lcTmpRecv+'.CUTTKT'
  .column1.ControlSource =loFormSet.lcTmpRecv+'.cRollid'
  .column2.ControlSource =loFormSet.lcTmpRecv+'.Fabric'
  .column3.ControlSource =loFormSet.lcTmpRecv+'.cFabClr'
  .column4.ControlSource =loFormSet.lcTmpRecv+'.Dyelot'
  .column5.ControlSource =loFormSet.lcTmpRecv+'.Aval'
  .column6.ControlSource =loFormSet.lcTmpRecv+'.RecTyp'
  .column7.ControlSource =loFormSet.lcTmpRecv+'.Req_Qty'
  .column8.ControlSource =loFormSet.lcTmpRecv+'.Issue_Qty'
  .readonly = .T.
ENDWITH
*!*************************************************************
*! Name      : lfChkCtkt
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/08/2007
*! Purpose   : To check the cut ticket status
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkCtkt()
*!*************************************************************
FUNCTION lfChkCtkt
PARAMETERS loFormSet,lcTrCode
lcPoshdr = loFormSet.lcPoshdr
PRIVATE lcTrCode
DO CASE
CASE &lcPoshdr..Status = 'H'
  *** Cutting ticket status is Hold , No transaction can be done***
  *** <  Ok > ***
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen('INM34055B00000','DIALOG', LANG_CutTkt + '|' + LANG_Status_Hold + '|' + LANG_Trans)
= gfModalGen('INM34055B00000','DIALOG', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CutTkt,loFormSet.GetHeaderText("LANG_CutTkt",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Status_Hold,loFormSet.GetHeaderText("LANG_Status_Hold",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Trans,loFormSet.GetHeaderText("LANG_Trans",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.ariaFORM1.woDCUTNO.KeyTextBox.value = ''
  RETURN .F.
CASE &lcPoshdr..Status  = 'X'
  *** Cutting ticket status is caneled , No transaction can be done***
  *** <  Ok > ***
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen('INM34055B00000','DIALOG', LANG_CutTkt + '|' + LANG_Status_Cancel + '|' + LANG_Trans)
= gfModalGen('INM34055B00000','DIALOG', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CutTkt,loFormSet.GetHeaderText("LANG_CutTkt",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Status_Cancel,loFormSet.GetHeaderText("LANG_Status_Cancel",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Trans,loFormSet.GetHeaderText("LANG_Trans",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

   loFormSet.ariaFORM1.woDCUTNO.KeyTextBox.value = ''
  RETURN .F.
CASE &lcPoshdr..Status  = 'C'
  *** Cutting ticket status is received , No transaction can be done***
  *** <  Ok > ***
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen('INM34055B00000','DIALOG', LANG_CutTkt + '|' + LANG_Status_Receive+ '|' + LANG_Trans)
= gfModalGen('INM34055B00000','DIALOG', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CutTkt,loFormSet.GetHeaderText("LANG_CutTkt",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Status_Receive,loFormSet.GetHeaderText("LANG_Status_Receive",loFormSet.HeaderAlias))+ '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Trans,loFormSet.GetHeaderText("LANG_Trans",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

   loFormSet.ariaFORM1.woDCUTNO.KeyTextBox.value = ''
   RETURN .F.

CASE &lcPoshdr..Status  = 'S'
  *** Cutting ticket status is closed , No transaction can be done***
  *** <  Ok > ***
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen('INM34055B00000','DIALOG', LANG_CutTkt + '|' + LANG_Status_Close + '|' + LANG_Trans)
= gfModalGen('INM34055B00000','DIALOG', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CutTkt,loFormSet.GetHeaderText("LANG_CutTkt",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Status_Close,loFormSet.GetHeaderText("LANG_Status_Close",loFormSet.HeaderAlias)) + '|' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Trans,loFormSet.GetHeaderText("LANG_Trans",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

   loFormSet.ariaFORM1.woDCUTNO.KeyTextBox.value = ''
    RETURN .F.
OTHERWISE
  loFormSet.ariaFORM1.dtpdate.Enabled = .T.
  loFormSet.ariaFORM1.cmdCollect.Enabled = .T.
ENDCASE
*!*************************************************************
*! Name      : lfSaveFiles
*! Developer : Mariam Mazhar
*! Date      : 01/14/2007
*! Purpose   : To Save change
*!*************************************************************

FUNCTION lfSaveFiles
PARAMETERS loFormSet


lcOldValue = ""
lenClrlen = loFormset.lenclrlen
lnMajLength =loFormset.lnmajlength
lcStyPic  = loFormset.lcitmpic
lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)
lcCuttkt= loFormSet.ariaFORM1.woDCUTNO.KeyTextBox.value

STORE 0 TO lnLineNO
STORE " " TO  lcGlYear   , lcGlPeriod
STORE " " TO lcDyelot


SELECT (loFormSet.lcTmpRecv)
GO TOP
lcTmpRecv =loFormSet.lcTmpRecv
IF !EOF()
  ldIssDate  = oAriaApplication.SystemDate
  =CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
  lcISession = gfsequence('GLSession')
  lcRSession = gfsequence('GLSession')
  IF loFormSet.Activemode='A'
    SELECT (loFormSet.lcTmpRecv)
    SCAN
      IF RecTyp = "Apply"
          SELECT ROLLS
          *--- cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode
          lcRollID = &lcTmpRecv..cRollid

          *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
*!*	          =gfSEEK('***** N/A *****     '+&lcTmpRecv..Fabric +&lcTmpRecv..cFabClr)
*!*	          LOCATE REST WHILE CROLLID+CROLLITEM+COLOR+TRANCD='***** N/A *****     '+&lcTmpRecv..Fabric +&lcTmpRecv..cFabClr FOR ;
*!*	            cTktNo = &lcTmpRecv..cuttkt AND dyelot =&lcTmpRecv..Dyelot AND cwarecode =&lcTmpRecv..cWareCode
		  =gfSEEK('***** N/A *****     '+&lcTmpRecv..Style)
          LOCATE REST WHILE CROLLID+Style+TRANCD='***** N/A *****     '+&lcTmpRecv..Style FOR ;
            cTktNo = &lcTmpRecv..cuttkt AND dyelot =&lcTmpRecv..Dyelot AND cwarecode =&lcTmpRecv..cWareCode
          * B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]

          IF FOUND()
            gfAppend()
            gfREPLACE([cRollItem   WITH &lcTmpRecv..Fabric,]+ ;
                    [COLOR       WITH &lcTmpRecv..cFabClr  , ]+     ;
                    [Cwarecode   WITH &lcTmpRecv..cWareCode,]+;
                    [Dyelot      WITH &lcTmpRecv..Dyelot      ,   ]+;
                    [Crollid     WITH lcRollID                 ,  ]+;
                    [Nqty        WITH Nqty    + &lcTmpRecv..Issue_Qty ,]+;
                    [Nqtybal     WITH (Nqtybal + &lcTmpRecv..Issue_Qty),]+ ;
                    [Trancd      WITH '2'                 , ]+      ;
                    [Ctktno      WITH &lcTmpRecv..cuttkt ,]+        ;
                    [Csession    WITH lcISession ,]+                ;
                    [Crsession   WITH lcRSession ,]+                ;
                    [Cisession   WITH lcISession])

            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
*!*	            =gfSEEK('***** N/A *****     '+&lcTmpRecv..Fabric +&lcTmpRecv..cFabClr)
*!*	            LOCATE REST WHILE CROLLID+CROLLITEM+COLOR+TRANCD='***** N/A *****     '+&lcTmpRecv..Fabric +&lcTmpRecv..cFabClr FOR ;
*!*	               cTktNo = &lcTmpRecv..cuttkt AND dyelot =&lcTmpRecv..Dyelot AND cwarecode =&lcTmpRecv..cWareCode
             gfREPLACE([Style with  &lcTmpRecv..Style])
             =gfSEEK('***** N/A *****     '+&lcTmpRecv..Style)
             LOCATE REST WHILE CROLLID+Style+TRANCD='***** N/A *****     '+&lcTmpRecv..Style FOR ;
               cTktNo = &lcTmpRecv..cuttkt AND dyelot =&lcTmpRecv..Dyelot AND cwarecode =&lcTmpRecv..cWareCode

             *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

            IF nQtyBal-&lcTmpRecv..Issue_Qty = 0 .OR. nQtyBal-&lcTmpRecv..Issue_Qty < 0
              gfDELETE()
            ELSE
              gfREPLACE([nQtyBal WITH MAX(nQtyBal-&lcTmpRecv..Issue_Qty,0)])
            ENDIF
            gfsetorder('Rolls')
            *--- rollid+crollitem+color+trancd

            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
            *=gfSEEK(PADR(lcRollID,20) + &lcTmpRecv..Fabric + &lcTmpRecv..cFabClr + '1')
            =gfSEEK(PADR(lcRollID,20) + &lcTmpRecv..Style + '1')
            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

            =gfREPLACE([Nqtybal WITH MAX(Nqtybal - &lcTmpRecv..Issue_Qty,0)])
          ENDIF
      ELSE
        =gfSeek('PU'+&lcTmpRecv..CUTTKT,'POSHDR','POSHDR')

        lcFullItem = PADR(&lcTmpRecv..Fabric  ,lnMajLength )+lcSepar   +PADR(&lcTmpRecv..cFabClr,lenClrlen)

        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
        *IF gfSEEK('0002'+lcFullItem , 'ITEM1','Style')
        IF gfSEEK('0002'+&lcTmpRecv..Style, 'ITEM1','Style')
        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]

          lcFabLinkCode   = IIF(EMPTY(ITEM1.Link_Code),'DEFDEF',ITEM1.Link_Code)
        ENDIF
        IF  loFormSet.laSetups[1,2]<>'Y'
          DIME laGLDistAr[1,1]
          laGLDistAr = ''
        ELSE
          DECLARE laGLDistAr[2,13]
          laGLDistAr[1,1] = lcFabLinkCode
          laGLDistAr[2,1] = POSHDR.LINK_CODE
          laGLDistAr[1,2] = '015'
          laGLDistAr[2,2] = '013'
          laGLDistAr[1,3] = 1
          laGLDistAr[2,3] = -1
          STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
          STORE lcCutTkt   TO laGLDistAr[1,5],laGLDistAr[2,5]
          STORE ldIssDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
          STORE lcGlYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
          STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
          STORE loFormset.lcGlDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
          STORE ''         TO laGLDistAr[1,10],laGLDistAr[2,10]
        ENDIF

        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
        *lnTmpCst=lfGetCost( &lcTmpRecv..cRollid , &lcTmpRecv..Fabric , &lcTmpRecv..cFabClr,&lcTmpRecv..cWareCode , &lcTmpRecv..Dyelot)
        lnTmpCst=lfGetCost( &lcTmpRecv..cRollid , &lcTmpRecv..Style,&lcTmpRecv..cWareCode , &lcTmpRecv..Dyelot)
		*B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
		
        lenClrlen = loFormset.lenclrlen
        lnMajLength =loFormset.lnmajlength
        lcStyPic  = loFormset.lcitmpic
        lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)

        lcFullItem = PADR(&lcTmpRecv..Fabric,lnMajLength )+lcSepar   +PADR(&lcTmpRecv..cFabClr,lenClrlen)

        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
        *=gfSEEK('0002'+lcFullItem , 'ITEM1','Style')
        =gfSEEK('0002'+&lcTmpRecv..Style, 'ITEM1','Style')
        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

        LOCAL ARRAY laIssue[9]
        STORE 0 TO laIssue
        laIssue[1] =-1*&lcTmpRecv..Issue_Qty
        laIssue[9] =-1*&lcTmpRecv..Issue_Qty

        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
        *=gfItemCrl('9','0002',lcFullItem,&lcTmpRecv..cWareCode, &lcTmpRecv..Dyelot,ldIssDate,ldIssDate,;
            &lcTmpRecv..CutTkt,@laIssue,lnTmpCst/IIF(gfSeek(ITEM1.CCONVBUY,'UOM') and UOM.NCONF<> 0,Uom.NCONF,1),;
            '','','',@laGLDistAr,'',loFormSet.lcBusDocu,loFormSet.lcStyType,lcRSession,lcISession,;
            '','',.F.,.F.,lnLineNo,.F.)

		=gfItemCrl('9','0002',&lcTmpRecv..Style,&lcTmpRecv..cWareCode, &lcTmpRecv..Dyelot,ldIssDate,ldIssDate,;
            &lcTmpRecv..CutTkt,@laIssue,lnTmpCst/IIF(gfSeek(ITEM1.CCONVBUY,'UOM') and UOM.NCONF<> 0,Uom.NCONF,1),;
            '','','',@laGLDistAr,'',loFormSet.lcBusDocu,loFormSet.lcStyType,lcRSession,lcISession,;
            '','',.F.,.F.,lnLineNo,.F.)
        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

        lnLineNo = lnLineNo + 1
        IF !lIssue
          SELECT ROLLS
          lcOldInd = ORDER()
          gfSetorder('Rolapl')
          lcRollID = &lcTmpRecv..cRollid
          IF !EMPTY(lcRollID)

            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
            *IF !gfSEEK(lcRSession+lcISession+&lcTmpRecv..Fabric+&lcTmpRecv..cFabClr+&lcTmpRecv..cWareCode+lcDyelot+lcRollID)
            IF !gfSEEK(lcRSession+lcISession+&lcTmpRecv..Style+&lcTmpRecv..cWareCode+lcDyelot+lcRollID)
            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

              gfAppend()
            ENDIF
            gfREPLACE([cRollItem   WITH &lcTmpRecv..Fabric       ,]+  ;
                      [COLOR         WITH &lcTmpRecv..cFabClr    ,    ]+;
                      [Cwarecode     WITH &lcTmpRecv..cWareCode,]+;
                      [Dyelot        WITH &lcTmpRecv..Dyelot   , ]+     ;
                      [Crollid       WITH lcRollID                ])

             gfREPLACE([Nqty          WITH Nqty    + &lcTmpRecv..Issue_Qty, ]+;
                       [Nqtybal       WITH (Nqtybal + &lcTmpRecv..Issue_Qty), ]+;
                       [Trancd        WITH '2'                       , ]+;
                       [Ctktno        WITH &lcTmpRecv..cuttkt        , ]+;
                       [Csession      WITH lcISession                , ]+;
                       [Crsession     WITH lcRSession                 ,]+;
                       [Cisession     WITH lcISession])

            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
             gfREPLACE([Style with &lcTmpRecv..Style])
            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

            gfSetorder('Rolls')
            *--- crollid+crollitem+color+trancd

            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
            *=gfSEEK(PADR(lcRollID,20) + &lcTmpRecv..Fabric + &lcTmpRecv..cFabClr + '1')
            =gfSEEK(PADR(lcRollID,20) + &lcTmpRecv..Style + '1')
            *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

            gfREPLACE([Nqtybal WITH MAX(Nqtybal - &lcTmpRecv..Issue_Qty,0)])
            gfSetorder(lcOldInd)
          ELSE
          ENDIF
        ENDIF
        IF !USED('CTktBom')
          =gfOpenTable(oAriaApplication.DataDir+'CTKTBOM',oAriaApplication.DataDir+'CTKTBOM','SH')
        ELSE
          SELECT CTktBom
          gfSetorder('CTKTBOM')
        ENDIF
        lcFullItem = PADR(&lcTmpRecv..Fabric,lnMajLength )+lcSepar   +PADR(&lcTmpRecv..cFabClr,lenClrlen)

        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        *=gfSEEK('0002'+lcFullItem , 'ITEM1','Style')
        *IF gfSEEK('M' + lcCutTkt + loFormSet.lcTranType +'0002' +lcFullItem )
        =gfSEEK('0002'+&lcTmpRecv..Style, 'ITEM1','Style')
        IF gfSEEK('M' + lcCutTkt + loFormSet.lcTranType +'0002' +&lcTmpRecv..Style)
		*B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  *-  -- Issue_Qty Instead of Req_Qty
          gfREPLACE([Used_Qty  WITH Used_Qty  + &lcTmpRecv..Issue_Qty,] +  ;
                  [Issue_Qty WITH Issue_Qty + MAX(&lcTmpRecv..Issue_Qty,0),]+ ;
                  [Dyelot    WITH lcDyelot])
        ENDIF

        IF !USED('BomCost')
          =gfOpenTable(oAriaApplication.DataDir+'BOMCOST',oAriaApplication.DataDir+'Bomcstkt','SH')
        ELSE
          SELECT BomCost
          gfSetorder('Bomcstkt')
        ENDIF
        SELECT BomCost
        SET DELETE OFF
        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        *IF !gfSEEK( loFormSet.lcTranType + 'M' + lcCutTkt +'0002'+ lcFullItem)
        IF !gfSEEK( loFormSet.lcTranType + 'M' + lcCutTkt +'0002'+ &lcTmpRecv..Style)
        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
           gfAppend()
        ELSE
          IF DELETED()
            BLANK
            RECALL
          ENDIF
        ENDIF
        SET DELETE ON
        *B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        *gfREPLACE([cTktNo    WITH lcCutTkt,]+   ;
                [cWareCode WITH &lcTmpRecv..cWareCode,]+ ;
                [cDyelot   WITH &lcTmpRecv..Dyelot   ,]+;
                [Item      WITH lcFullItem    ,]+;
                [cBomType  WITH loFormSet.lcTranType, ]+;
                [cIMTyp    WITH 'M'        ,]+;
                [cInvType  WITH '0002'        ,]+;
                [MfgCode   WITH SPACE(6)   ,]+;
                [nTotQty   WITH nTotQty + &lcTmpRecv..Issue_Qty ,]+;
                [nTotCst   WITH nTotCst + &lcTmpRecv..Issue_Qty * lnTmpCst / IIF(gfSeek(ITEM1.CCONVBUY,'UOM') and UOM.NCONF<> 0 , UOM.NCONF,1) ,]+;
                [dTranDate WITH ldIssDate ,]+ ;
                [cRSession WITH lcRSession ,]+;
                [cISession WITH lcISession ,]+;
                [cCostType WITH ''         ,]+;
                [nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty),]+ ;
                [nUnitACst WITH nUnitCst  ,]+;
                [nTotACst  WITH nTotCst   ,]+;
                [cOprCode  WITH ''        ,]+;
                [cLotNo    WITH ''        ,]+;
                [Actualize WITH 'Y'])
        gfREPLACE([cTktNo    WITH lcCutTkt,]+   ;
                [cWareCode WITH &lcTmpRecv..cWareCode,]+ ;
                [cDyelot   WITH &lcTmpRecv..Dyelot   ,]+;
                [Item      WITH &lcTmpRecv..Style   ,]+;
                [cBomType  WITH loFormSet.lcTranType, ]+;
                [cIMTyp    WITH 'M'        ,]+;
                [cInvType  WITH '0002'        ,]+;
                [MfgCode   WITH SPACE(6)   ,]+;
                [nTotQty   WITH nTotQty + &lcTmpRecv..Issue_Qty ,]+;
                [nTotCst   WITH nTotCst + &lcTmpRecv..Issue_Qty * lnTmpCst / IIF(gfSeek(ITEM1.CCONVBUY,'UOM') and UOM.NCONF<> 0 , UOM.NCONF,1) ,]+;
                [dTranDate WITH ldIssDate ,]+ ;
                [cRSession WITH lcRSession ,]+;
                [cISession WITH lcISession ,]+;
                [cCostType WITH ''         ,]+;
                [nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty),]+ ;
                [nUnitACst WITH nUnitCst  ,]+;
                [nTotACst  WITH nTotCst   ,]+;
                [cOprCode  WITH ''        ,]+;
                [cLotNo    WITH ''        ,]+;
                [Actualize WITH 'Y'])
		*B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
        IF nTOtQty = 0
          gfDELETE()
        ENDIF
        IF !EMPTY(loFormSet.lcTranType)
          SELECT POSHDR
          =gfSEEK('PU'+&lcTmpRecv..CutTkt)
          =RLOCK()
          lcTranType =loFormSet.lcTranType
          gfREPLACE([nAct_Cost&lcTranType WITH nAct_Cost&lcTranType + &lcTmpRecv..Issue_Qty * lnTmpCst / IIF(gfSeek(ITEM1.CCONVBUY,'UOM') and UOM.NCONF<> 0 , UOM.NCONF,1)])
          UNLOCK
        ENDIF
        SELECT(lcTmpRecv)
        DO CASE
          CASE  nReturn = 0

          CASE  nReturn = 1

          CASE  nReturn = 2
        ENDCASE
      ENDIF
    lfSaveTables()
    ENDSCAN
    *MT33
    SELECT Rolls
    =gfTableUpdate()
    *MT33
  ENDIF
  SELECT (lcTmpRecv)
  ZAP
  RETURN .T.
ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_No_Rec_Save)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Rec_Save,loFormSet.GetHeaderText("LANG_No_Rec_Save",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  STORE .F. TO llCSave
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvQty
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/08/2007
*! Purpose   : The Qty object valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvQty()
*!*************************************************************
FUNCTION lfvQty
PARAMETERS loFormset

lcTmpRecv = loFormset.lcTmpRecv
lnQty     = loFormSet.ariaform1.txtQty.value
IF lnQty > &lcTmpRecv..Aval
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Issue_Qty_Great_Avl)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Issue_Qty_Great_Avl,loFormSet.GetHeaderText("LANG_Issue_Qty_Great_Avl",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.ariaform1.txtQty.value = loFormSet.ariaform1.txtQty.Oldvalue
  RETURN .F.
ENDIF

IF lnQty =< 0
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B36000',.F.,.F.,.F.,LANG_Zero_Neg)
=gfModalGen('TRM00000B36000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Zero_Neg,loFormSet.GetHeaderText("LANG_Zero_Neg",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.ariaform1.txtQty.value = loFormSet.ariaform1.txtQty.Oldvalue
  RETURN .F.
ENDIF
SELECT (lcTmpRecv)
REPLACE Issue_Qty WITH lnQty
*!*************************************************************
*! Name      : lfSaveTables
*! Developer : Mariam Mazhar
*! Date      : 02/12/2007
*! Purpose   : function to save files
*!*************************************************************
FUNCTION lfSaveTables

*-- Call default save
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == loFormSet.DataSessionId
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF


*!*************************************************************
*! Name      : lfvRemLn
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/26/2007
*! Purpose   : Push button "Remove Line" valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemLn()
*!*************************************************************
FUNCTION lfvRemLn
PARAMETERS loFormset

*** Are you sure you want to "Remove" this record? ***
*** < Yes > - < No > ***
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("QRM00002B00006","ALERT" , LANG_Delete) = 1
IF gfModalGen("QRM00002B00006","ALERT" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Delete,loFormSet.GetHeaderText("LANG_Delete",loFormSet.HeaderAlias))) = 1
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *-- Delete current record in the browse.
  SELECT (loFormset.lcTmpRecv)
  DELETE
  GO TOP
ENDIF
