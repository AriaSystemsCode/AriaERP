*:************************************************************
*: Program file  : icInvLo
*: Program desc. : Style Inventory Location
*: For screen    : icInvLo
*:        System : Aria Advantage Series(Aria4xp)
*:        Module : Inventory Control (IC).
*:     Developer : Mariam Mazhar Tawfik(MMT)
*:     Issue     : 037550
*:************************************************************
*: Modifications
*:B608057,1 SSH Some bugs in extended size scale case
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001] 
*:************************************************************
#INCLUDE R:\ARIA4XP\SCREENS\IC\ICINVLO.H

DO FORM (oAriaApplication.ScreenHome+"\IC\ICINVLO.SCX")

RETURN

DEFINE CLASS Inv_Bin as Custom
  loFormSet = .F.
  loForm = .F.
  llLink_GL = ''
  llMultiWH =''
  llMultiLoc = ''
  llExtSize = ''
  llBinsFile = .F.
  lcWareFil = ""
  lcDefWH = ""
  lcLocFil = ""
  lnItmWid = 0
  lnClrWid = 0
  lcStyleSel = ""
  lcAddTag = ""
  lcWhsLoc = ""
  loGl_link = ""
  lcWhsloc = ""
  *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
  lcLocPerSize = ""
  *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
  *:B608057,1 SSH size scale array
  DECLARE laExtend[1]
  *:B608057,1 SSH [END]
*!*****************************************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 11/29/2005
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfInit
LPARAMETERS loFrm

This.loFormSet = loFrm.Parent
This.loForm = loFrm
SET MULTILOCKS ON
This.lfopenfile('Codes','Codes')
This.lfopenfile('OrdHdr','OrdHdr')
This.lfopenfile('WAREHOUS','WAREHOUS')
This.lfopenfile('STYDYE','STYDYE')
=gfOpenTable('STYLE','STYLE')
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
=gfOpenTable('Scale','Scale')
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
This.addproperty("Style")
This.Style = oAriaApplication.laRemoteTable[gfGetRemoteTable(SET("Datasession"), 'Style')]

DECLARE laSetups[4,2]
laSetups[1,1]  = 'M_LINK_GL'
laSetups[2,1]  = 'M_WAREHOUS'
laSetups[3,1]  = 'M_WARELOC'
laSetups[4,1]  = 'M_USEEXSSC'
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyId)

This.llLink_GL  = UPPER(ALLTRIM(laSetups[1,2]))  = 'Y'
This.llMultiWH  = UPPER(ALLTRIM(laSetups[2,2]))  = 'Y'
This.llMultiLoc = UPPER(ALLTRIM(laSetups[3,2]))  = 'Y'
This.llExtSize  = laSetups[4,2]

IF !This.llMultiLoc AND !This.llMultiWH
  *-- System has not been setup to keep track of Warehouses or Locations.
  *-- < OK >
  = gfModalGen("TRM42088B42000","DIALOG")
  This.LoFormSet.llReturn = .F.
  RETURN
ENDIF

STORE .F. TO This.llBinsFile
*-- if system support multi bins.
IF This.llMultiLoc
  IF USED('WHSLOC')
    lcLocOrd = ORDER('WHSLOC')
    lcLocRec = WHSLOC.cwarecode+WHSLOC.clocation+WHSLOC.style+WHSLOC.color
    SET ORDER TO WHSLOC IN WHSLOC
  ELSE
    This.lfopenfile('WHSLOC','WHSLOC')
    This.llBinsFile = .T.
*      llBinsFile = gfOpenFile(gcDataDir+'WHSLOC',gcDataDir+'WHSLOC','SH')
  ENDIF
ENDIF

*-- If this company use GL open GL_Link File
IF This.llLink_GL
  *-- The llGlopen flag is true when current user is first one open the GL_Link file
  *-- and in clearup I use it to close the file if it is true
  This.loGl_link  = CREATEOBJECT("RemoteTable",'GL_LINK','Gl_Link1','Gl_Link_Temp',This.loFormSet.DataSessionID)
ENDIF
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
This.lcLocPerSize = gfTempName()
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
This.lcLocFil = gfTempName()
This.lcWhsloc = gfTempName()
*-- gfItemMask is a global Function that returend header and picture
*-- of Key Field according to the passed parameters
lcItmTtl   = gfItemMask("HI")
lcItmPct   = gfItemMask("PI")
lcClrPct   = gfItemMask("PN")
IF EMPTY(lcItmTtl) OR EMPTY(lcItmPct)
  *-- Item structure not found , Can't proceed .
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('QRM42080B42001','DIALOG',LANG_Item_Struct)
=gfModalGen('QRM42080B42001','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Item_Struct,THIS.loFormSet.GetHeaderText("LANG_Item_Struct",THIS.loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN
ENDIF

IF This.llExtSize
  This.lfopenfile('ICISTRU','SEGNO')
  This.ICISTRU.GOBOTTOM()
  SELECT ICISTRU
  SKIP -1
  lnCut = IIF(EMPTY(cISegSepr),3,4)
  *This.ICISTRU = Null
  lcItmPct   = SUBSTR(lcItmPct,1,LEN(lcItmPct)-lnCut)
  *-- Adjust lcBrfields to affect the global browsing BitMap
  lcBrfields = "ExStyle = SUBSTR(Style,1,LEN(lcItmPct))" &&+ SUBSTR(lcBrFields,6)
ENDIF
This.lnItmWid   = LEN(lcItmPct)
This.lnClrWid   = LEN(lcClrPct)
lcScFields = "Style"

*-- Create temp file name to hold wharehouses.
This.lcWareFil = gfTempName()
This.lcAddTag  = gfTempName()
This.lcWhsLoc  = gfTempName()

SELECT StyDye
= AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*-- Redimension the array to create a new field to hold the status of
*-- the record (S, M, A or D)
DIMENSION laFileStru[lnFileStru+2, 18]
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cStatus'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0

STORE ' ' TO  laFileStru[lnFileStru ,7],laFileStru[lnFileStru,8],;
              laFileStru[lnFileStru,9],laFileStru[lnFileStru,10],;
              laFileStru[lnFileStru,11],laFileStru[lnFileStru,12],;
              laFileStru[lnFileStru,13],laFileStru[lnFileStru,14],;
              laFileStru[lnFileStru,15],laFileStru[lnFileStru,16]
STORE 0   TO  laFileStru[lnFileStru,17] ,laFileStru[lnFileStru,18]

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Description'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 35
laFileStru[lnFileStru ,4] = 0

STORE ' ' TO  laFileStru[lnFileStru ,7],laFileStru[lnFileStru,8],;
              laFileStru[lnFileStru,9],laFileStru[lnFileStru,10],;
              laFileStru[lnFileStru,11],laFileStru[lnFileStru,12],;
              laFileStru[lnFileStru,13],laFileStru[lnFileStru,14],;
              laFileStru[lnFileStru,15],laFileStru[lnFileStru,16]
STORE 0   TO  laFileStru[lnFileStru,17] ,laFileStru[lnFileStru,18]

=gfCrtTmp(This.lcWareFil,@laFileStru,"STYLE+CWARECODE+DYELOT",This.lcWareFil,.T.)

SELECT(This.lcWareFil)
INDEX ON cWareCode+Style+DyeLot TAG (This.lcAddTag)

This.lfAddControlSource()
IF This.llMultiLoc
  SELECT WhsLoc
  = AFIELDS(laFileStruLoc)
  lnFileStruLoc = ALEN(laFileStruLoc,1)
  *-- Redimension the array to create a new field to hold the status of
  *-- the record (S, M, A or D)
  DIMENSION laFileStruLoc[lnFileStruLoc+1, 18]
  lnFileStruLoc = lnFileStruLoc + 1
  laFileStruLoc[lnFileStruLoc ,1] = 'cStatus'
  laFileStruLoc[lnFileStruLoc,2] = 'C'
  laFileStruLoc[lnFileStruLoc,3] = 1
  laFileStruLoc[lnFileStruLoc,4] = 0
  STORE ' ' TO  laFileStruLoc[lnFileStruLoc,7],laFileStruLoc[lnFileStruLoc,8],;
                laFileStruLoc[lnFileStruLoc,9],laFileStruLoc[lnFileStruLoc,10],;
                laFileStruLoc[lnFileStruLoc,11],laFileStruLoc[lnFileStruLoc,12],;
                laFileStruLoc[lnFileStruLoc,13],laFileStruLoc[lnFileStruLoc,14],;
                laFileStruLoc[lnFileStruLoc,15],laFileStruLoc[lnFileStruLoc,16]
  STORE 0   TO  laFileStruLoc[lnFileStruLoc,17] ,laFileStruLoc[lnFileStruLoc,18]

  *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
  *  =gfCrtTmp(This.lcLocFil,@laFileStruLoc,"STYLE+COLOR+CWARECODE+CLOCATION",This.lcLocFil,.T.)
  =gfCrtTmp(This.lcLocFil,@laFileStruLoc,"STYLE+COLOR+CWARECODE+CLOCATION+SIZE",This.lcLocFil,.T.)
  =gfCrtTmp(This.lcLocPerSize ,@laFileStruLoc,"STYLE+COLOR+CWARECODE+CLOCATION+SIZE",This.lcLocPerSize ,.T.)  
  SELECT (This.lcLocPerSize)
  INDEX ON CWARECODE+CLOCATION+STYLE+COLOR+SIZE TAG (This.lcWhsLoc)
  *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
  SELECT(This.lcLocFil)
  INDEX ON CWARECODE+CLOCATION+STYLE+COLOR TAG (This.lcWhsLoc)
ENDIF
lcBrowFile = This.lcWareFil

WITH This.loFormSet
  IF !This.STYLE.llNative
    .cbrowsetabledbengine   = "SQL"
  ELSE
    .cbrowsetabledbengine   = 'NATIVE'
  ENDIF
*:B608057,1 SSH Commented out
*!*	  .nWorkArea        = "Style"
*!*	  .DataEnvironment.InitialSelectedAlias = 'STYLE'
*!*	  .cBrowseFileName        = "STYLE"
*!*	  .cBrowseIndexExpression = "STYLE"
*!*	  .cBrowseIndexFields     = "STYLE"
*!*	  .cBrowseIndexName       = "STYLE"
*!*	  .cBrowseAliasName       = "STYLE"
*!*	  .cBrowseTableName       = "STYLE"
*!*	  .BrowseTitle            = "Styles"
*:B608057,1 SSH [END]
*MEDIA
  .nWorkArea        = "Style"
  .DataEnvironment.InitialSelectedAlias = 'STYLE'
  .cBrowseFileName        = "STYLE"
  .cBrowseIndexExpression = "STYLE"
  .cBrowseIndexFields     = "STYLE"
  .cBrowseIndexName       = "STYLE"
  .cBrowseAliasName       = "STYLE"
  .cBrowseTableName       = "STYLE"
  .BrowseTitle            = "Styles"
*MEDIA
ENDWITH
This.LoFormSet.llReturn = .T.

*!*****************************************************************************************
*! Name      : lfOpenFile
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 11/29/2005
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfOpenFile
PARAMETERS lcFile, lcTag
LOCAL lcProp

lcFile = JUSTSTEM(lcFile)
lcTag = JUSTSTEM(lcTag)
lcProp = lcFile
IF !PEMSTATUS(This,lcProp,5)
  This.addproperty(lcProp)
ENDIF
lcProp = 'This.'+lcProp
IF TYPE(lcProp)<>'O'
  &lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,this.loFormSet.DataSessionID)
ELSE
  &lcProp..SetOrder(lcTag)
ENDIF


*!*************************************************************
*! Name      : lfvStyle
*: Developer : Mariam Mazhar(MMT)
*: Date      : 12/06/2005
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Style field]
*!*************************************************************
*! Calls       : lfGScalCnt() , gfStyBrw() , gfModalGen() , gpAdStyWar()
*!               lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvStyle

lcStyle    = This.loFormSet.AriaForm1.kbStyle.value
lcOldVal   = This.loFormSet.AriaForm1.kbStyle.oldvalue
lcOldAlias = ALIAS()               && Variable to save the old Alis
*IF The user want to Browse or if the Style he entered is not in the file
SELECT (lcOldAlias)
This.Style.Seek(lcStyle)
this.loFormSet.AriaForm1.kbStyle.value = lcStyle

*!*************************************************************
*! Name      : lfGetData
*: Developer : Mariam Mazhar(MMT)
*: Date      : 11/30/2005
*! Purpose   : Get the selected style data
*!*************************************************************
*
FUNCTION lfGetData
PARAMETERS lcStyle

This.lcStyleSel =SUBSTR(Style.Style,1,This.lnItmWid)
lcCurrAlias = ALIAS()
This.loFormSet.AriaForm1.kbStyle.value = lcStyle
This.Style.Seek(lcStyle)
This.lcDefWH = Style.cDefWare
IF USED(This.lcWareFil) AND RECCOUNT(This.lcWareFil)> 0
  SELECT(This.lcWareFil)
  ZAP
ENDIF
IF USED(This.lcLocFil) AND RECCOUNT(This.lcLocFil) != 0
   SELECT (This.lcLocFil)
   ZAP
ENDIF
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
IF USED(This.lcLocPerSize) AND RECCOUNT(This.lcLocPerSize) != 0
   SELECT (This.lcLocPerSize)
   ZAP
ENDIF
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
this.StyDye.Seek(lcStyle)
INSERT INTO (This.lcWareFil) SELECT *,'S' as cStatus,"" as Description FROM stydye WHERE Style = lcStyle AND EMPTY(Dyelot)

SELECT (This.lcWareFil)
INDEX ON cwarecode TAG (This.lcWareFil) Unique
GO TOP

IF This.llMultiLoc
  *-- llFill is a flag when it is true fill temp. location file.
  llFill = !USED(This.lcLocFil) OR (USED(This.lcLocFil) AND (RECCOUNT(This.lcLocFil) = 0))

  *-- This confirmation avoid overriding the temp. loc file with another
  *-- data from WhsLoc file if it is already have data
  IF llFill
    *-- Create temp file holding the locations.
    lcOldOrd = ORDER('WhsLoc')
    This.WhsLoc.SetOrder('WHSLOCST')
    This.WhsLoc.Seek(This.lcStyleSel)
    INSERT INTO (This.lcLocFil) SELECT *, "S" AS cStatus FROM WhsLoc ORDER BY cWareCode, cLocation
    *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
    INSERT INTO (This.lcLocPerSize) SELECT *, "S" AS cStatus FROM WhsLoc ORDER BY cWareCode, cLocation
    *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
    This.WhsLoc.SetOrder(lcOldOrd)
  ENDIF       && Ending of filling the file IF.
ENDIF         && Ending of ask obout multilocation IF.

SELECT(This.lcWareFil)
SCAN
  REPLACE Description WITH This.lfGetDesc()
ENDSCAN
*!*************************************************************
*! Name      : lfRefreshModes
*: Developer : Mariam Mazhar(MMT)
*: Date      : 11/30/2005
*! Purpose   : used in change mode fn.
*!*************************************************************
*
FUNCTION lfRefreshModes
PARAMETERS lcActMode

DO CASE
  CASE lcActMode = 'S' && Select mode
    IF USED(This.lcWareFil) AND RECCOUNT(This.lcWareFil) != 0
      SELECT (This.lcWareFil)
      ZAP
    ENDIF
    IF USED(This.lcLocFil) AND RECCOUNT(This.lcLocFil) != 0
      SELECT (This.lcLocFil)
      ZAP
    ENDIF
    *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
    IF USED(This.lcLocPerSize) AND RECCOUNT(This.lcLocPerSize) != 0
      SELECT (This.lcLocPerSize)
      ZAP
    ENDIF
    *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
  CASE lcActMode = 'E' && Edit mode
    This.lcStyleSel =SUBSTR(This.loFormset.AriaForm1.kbStyle.value,1,This.lnItmWid)
    *-- Load the style's size scales in the size scale array if the
    *-- system is setup to use the extended size scales.
    IF This.llExtSize
      This.Style.Seek(This.lcStyleSel)
      SELECT DISTINCT SUBSTR(Style,This.lnItmWid+1) ;
      FROM STYLE                               ;
      WHERE Style = This.lcStyleSel             ;
      INTO ARRAY This.laExtend
    ENDIF
    IF RECCOUNT(This.lcWareFil) = 0 AND This.llMultiLoc
      lcOldOrd = ORDER('WhsLoc')
      This.WhsLoc.SetOrder('WHSLOCST')
      This.Stydye.Seek(This.lcStyleSel)
      INSERT INTO (This.lcWareFil)  SELECT *, "S" AS cStatus,"" as Description FROM StyDye WHERE EMPTY(DyeLot) ORDER BY cWareCode
      This.WhsLoc.SetOrder(lcOldOrd)
    ENDIF         && Ending confirmation for data in temp. ware file
    *-- If no multilocation there is no reason to create the temp file,
    *-- the button will be disabled.
    IF This.llMultiLoc
      *-- llFill is a flag when it is true fill temp. location file.
      llFill = !USED(This.lcLocFil) OR (USED(This.lcLocFil) AND (RECCOUNT(This.lcLocFil) = 0))
      *-- This confirmation avoid overriding the temp. loc file with another
      *-- data from WhsLoc file if it is already have data
      IF llFill
        *-- Create temp file holding the locations.
        This.WhsLoc.Seek(This.lcStyleSel)
        INSERT INTO (This.lcLocFil) SELECT *, "S" AS cStatus FROM WhsLoc ORDER BY cWareCode, cLocation
        *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
        INSERT INTO (This.lcLocPerSize) SELECT *, "S" AS cStatus FROM WhsLoc ORDER BY cWareCode, cLocation
        *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
      ENDIF       && Ending of filling the file IF.
    ENDIF         && Ending of ask obout multilocation IF.
    *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
    CASE lcActMode = 'V' && Viewe mode
 
      IF This.llMultiLoc
        
        IF USED(This.lcLocFil) AND RECCOUNT(This.lcLocFil) != 0
           SELECT (This.lcLocFil)
           ZAP
        ENDIF
        INSERT INTO (This.lcLocFil) SELECT *, "S" AS cStatus FROM WhsLoc ORDER BY cWareCode, cLocation

        IF USED(This.lcLocPerSize) AND RECCOUNT(This.lcLocPerSize) != 0
           SELECT (This.lcLocPerSize)
           ZAP
        ENDIF
        INSERT INTO (This.lcLocPerSize) SELECT *, "S" AS cStatus FROM WhsLoc ORDER BY cWareCode, cLocation
      ENDIF 
    *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
ENDCASE
SELECT(This.lcWareFil)
SCAN
  REPLACE Description WITH This.lfGetDesc()
ENDSCAN
LOCATE

*!*************************************************************
*! Name      : lfAddControlSource
*: Developer : Mariam Mazhar(MMT)
*: Date      : 12/01/2005
*! Purpose   : make controlsource for the grid
*!*************************************************************
*
FUNCTION lfAddControlSource

WITH This.loFormSet.ariaForm1.grdLocations
  .RecordSource = ""
  .RecordSource = This.lcWareFil
  .Column1.ControlSource = 'Cwarecode'
*  .Column2.ControlSource = 'ThisFormSet.loinv_bin.lfGetDesc()'
  .Column2.ControlSource = 'Description'
ENDWITH
*!*************************************************************
*! Name      : lfGetDesc
*: Developer : Mariam Mazhar(MMT)
*: Date      : 12/01/2005
*! Purpose   : get the warehous description
*!*************************************************************
*
FUNCTION lfGetDesc

IF This.WareHous.Seek(EVALUATE(This.lcWareFil+'.cWareCode'))
  RETURN WareHous.cDesc
ELSE
  RETURN ""
ENDIF
*!*************************************************************
*! Name      : lfAfterRowCol
*: Developer : Mariam Mazhar(MMT)
*: Date      : 12/01/2005
*! Purpose   : After Row Col Changed
*!*************************************************************
*
FUNCTION lfAfterRowCol

This.WareHous.Seek(EVALUATE(This.lcWareFil+'.cWareCode'))
WITH This.loformset.ariaForm1
  WITH .cntAddrInfo
    .txtAdd1.Value = warehous.cAddress1
    .txtAdd2.Value = warehous.cAddress2
    .txtAdd3.Value = warehous.cAddress3
    .txtAdd4.Value = warehous.cAddress4
    .txtAdd5.Value = warehous.cAddress5
    .txtAdd6.Value = warehous.cAddress6
    .cboCountry.Value =warehous.ccont_code
  ENDWITH
  .txtPhone.Value = WareHous.cPhone
  .txtFax.Value  = WareHous.cFax
  IF This.llLink_GL
    .kbGLWare.keytextbox.Value = EVALUATE(This.lcWareFil+'.GL_Link')
    .txtWareDesc.Value = IIF(This.loGl_link.Seek('03'+PADR(EVALUATE(This.lcWareFil+'.GL_Link'),6)),GL_LINK_Temp.linkdesc,"")
  ENDIF
  This.loformset.ariaForm1.cntAddrInfo.adjustcontrols ()
ENDWITH
*!*************************************************************
*! Name      : lfvGLWareHs
*! Developer : Mariam Mazhar (MMT)
*! Date      : 12/01/2005
*! Purpose   : Validation of GL_Link codes assigned to warehous
*!*************************************************************
*
FUNCTION lfvGLWareHs

PRIVATE lnAlias
lcOldValue = This.loFormSet.ariaForm1.kbGLWare.keytextbox.Value
lcGLWareHs = This.loFormSet.ariaForm1.kbGLWare.keytextbox.Value
lcGLWaDesc = This.loFormSet.ariaForm1.txtWareDesc.Value
lnAlias = SELECT(0)
*This.Parent.selectedfrombrowse = .T.
IF This.loFormSet.ariaForm1.kbGLWare.selectedfrombrowse .OR.;
 (!EMPTY(lcGLWareHs) AND AT('?',ALLTRIM(lcGLWareHs),1) > 0);
  OR (!EMPTY(lcGLWareHs) AND !This.loGl_link.SEEK(lcGLWareHs))
  *-- calling the global browse for Gl_Link
  = gfGLBrowse('03',@lcGLWareHs,@lcGLWaDesc,.F.,.T.)

  IF EMPTY(lcGLWareHs)
    lcGLWareHs = lcOldValue
  ENDIF
ENDIF
This.loFormSet.ariaForm1.kbGLWare.keytextbox.Value = lcGLWareHs
This.loFormSet.ariaForm1.txtWareDesc.Value = IIF(This.loGl_link.Seek('03'+PADR(lcGLWareHs,6)),GL_LINK_Temp.linkdesc,"")

SELECT(This.lcWareFil)
REPLACE GL_Link WITH lcGLWareHs


*-- If user change GL_Link code
IF !((lcGLWareHs = lcOldValue) OR (EVALUATE(This.lcWareFil+'.cStatus') = "A"))
  REPLACE cStatus WITH 'M'
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvWareHs
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/06/2005
*! Purpose   : calling the gfmover with warehouse arrays and handling
*!           : the returned laTarget (i.e: updating in temp. ware file
*!           : [from StyDye], and also updating temp. location file)
*!           : brifely we can say that it is prepair to save warehouses
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfMover,lfvRem
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvWareHs()
*!*************************************************************
FUNCTION lfvWareHs

PRIVATE lcSDel,lcSaveRec
DECLARE laSource[1],laTarget[1]
STORE SPACE(0) TO laSource,laTarget

*-- Prepair to use GfMover hold Warehouses
*-- Fill laSource array with all warehouses in WareHouse file
*This.WareHous.SqlRun("SELECT DISTINCT cWarecode FROM WareHous ",'WareHous')
SELECT cWarecode FROM WareHous INTO ARRAY laSource


*-- lcSelFile : Hold the file used in SQL statement
*-- lcSelExp  : Hold the expression used in SQL statement according to used file
*-- lcSaveRec : Hold the current record value to restore it after Mover
lcSelFile = This.lcWareFil
lcSelExp  = "cStatus != 'D'"

lcSaveRec = &lcSelFile..cWarecode

*-- Fill laTarget array with all warehouse assigined to this style
SELECT DISTINCT cWarecode ;
  FROM (lcSelFile)        ;
  WHERE &lcSelExp         ;
  ORDER BY cWareCode      ;
  INTO ARRAY laTarget

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laSource,@laTarget,LANG_Assign_Locations,!(This.loFormset.activemode = "V"),'mvRem',.T.,.T.,This.loFormset)
= gfMover(@laSource,@laTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Assign_Locations,this.loFormSet.GetHeaderText("LANG_Assign_Locations",this.loFormSet.HeaderAlias)),!(This.loFormset.activemode = "V"),'mvRem',.T.,.T.,This.loFormset)
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Call the Mover with function lfvRem that validate for removing warehous
*-- You are in Edit mode
IF This.loFormset.activeMode = "E"
  lnAlias = SELECT(0)

  *-- Assure that delete is off to deal with deleted records
  lcSDel = SET("DELETED")
  SET DELETED OFF

  *-- Search for warehouses for this style in temporary file
  SELECT (This.lcWareFil)
  SCAN
    *-- IF you do not find this warehouse in array this means
    *-- that it deleted we replace its status with "D" or DELETE it,
    *-- corresponding to its current status
    IF ASCAN(laTarget, ALLTRIM(cWarecode)) = 0
      *-- If it is already in file or it is modified
      IF cStatus = "S" OR cStatus = "M"
        REPLACE cStatus WITH "D"

        IF This.llMultiLoc
          SELECT (This.lcLocFil)
          REPLACE ALL cStatus WITH "D" ;
                  FOR cWarecode+cLocation+Style+Color = EVALUATE(This.lcWareFil+'.cWareCode')
        ENDIF
      ELSE
        IF cStatus = "A" AND This.llMultiLoc
          SELECT (This.lcLocFil)
          REPLACE ALL cStatus WITH "D" ;
                  FOR cWarecode+cLocation+Style+Color = ;
                      EVALUATE(This.lcWareFil+'.cWareCode')
          SELECT (This.lcWareFil)
          DELETE
        ENDIF
      ENDIF
    *-- IF you find this warehouse in array this means
    *-- it is a (A)new one or (M)modified one
    *-- we make this substituation to avoid complicating code
    ELSE
      *-- If you replace the status for warehouse with "D" in the same screen
      IF cStatus = "D"
        REPLACE cStatus WITH "M"
      ENDIF

      *-- If you delete warehouse in the same screen (New in this screen)
      IF DELETED()
        RECALL
      ENDIF
    ENDIF         && Ending array scan IF.
  ENDSCAN        && Ending Search for warehouses

  *-- Prepaire to use seek
  SELECT (This.lcWareFil)
  SET ORDER TO (this.lcAddTag)

  *-- Appending new record for new warehouse if it is not found in file
  *-- loop until you reach the end of array (For all items in array)
  FOR lnI = 1 TO ALEN(laTarget,1)
    IF !SEEK(laTarget[lnI]) AND !EMPTY(laTarget[lnI])
      *-- If you use extended size scales loop with data from
      *-- laExtend array filled in edit mode from Style file
      *-- to add multiple records
      IF This.llExtSize
        FOR lnJ = 1 TO ALEN(This.laExtend,1)
          INSERT INTO (This.lcWareFil)                                          ;
                           (Style  ,cWarecode    ,cStatus) ;
                    VALUES (This.loFormSet.AriaForm1.kbStyle.value+This.laExtend[lnJ],laTarget[lnI],"A"    )
           REPLACE Description WITH This.lfGetDesc()
        ENDFOR

      *-- If you do not use extended size scales add single record
      ELSE
        INSERT INTO (This.lcWareFil)                            ;
                         (Style    ,cWarecode    ,cStatus) ;
                  VALUES (This.loFormSet.AriaForm1.kbStyle.value,laTarget[lnI],"A"    )
        REPLACE Description WITH This.lfGetDesc()
      ENDIF      && Ending extended size IF.
    ENDIF      && Ending record not found IF.
  ENDFOR      && Ending for

  *-- Restore old delete status
  SET DELETED &lcSDel

  *-- Restore the same cursor (>) location if it is found
  IF !SEEK(lcSaveRec) OR cStatus = "D"
    LOCATE FOR Style = This.loFormSet.AriaForm1.kbStyle.value AND cStatus <> 'D'

  ENDIF

  *-- Prepaire to use seek
  SELECT (This.lcWareFil)
  SET ORDER TO (This.lcWareFil)
  SET FILTER TO cStatus <> 'D'
  This.loFormSet.ariaForm1.grdLocations.AfterRowColChange ()
  SELECT(lnAlias)
ENDIF
*!*************************************************************
*! Name      : lfvRem
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/06/2005
*! Purpose   : Validat the removed warehouses to avoid removing
*!           : ordered or have a stock ONEs
*!           : case of remove we returned to Mover by .T. to remove
*!           : or .F. to cancel remove
*!           : case of remove all we make the moving and returned to Mover
*!           : with .F. to cancel remove all
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen,lfRemCal
*!*************************************************************
*! Passed Parameters  : lnBttn
*!*************************************************************
*! Returns            : llRetVal = .T. , Mover can remove
*!                    : llRetVal = .F. , Mover can not remove
*!*************************************************************
*! Example   : =lfvRem(lnButton)
*!*************************************************************
FUNCTION lfvRem
PARAMETERS lnBttn,lnSele
PRIVATE lnI

IF (This.loFormset.activeMode = "E")
  *-- Prepair to remove valid
  lnCurAlias = SELECT(0)
  SELECT StyDye
  lcCurNdx = ORDER()
  This.StyDye.SetOrder("StyDyeW")

  lcOldExact = SET("EXACT")
  SET EXACT OFF
  llRetVal = .F.         && To hold the returned value

  *-- To distinct between lnBttn cases
  *-- If <Move> or <MoveAll> do nothing
  IF lnBttn <= 2
    llRetVal = .T.
  ENDIF

  *-- If <Remove> Call to valid function and store its return value in llRetVal
  *-- which returned to Mover.
  *-- Note that this code is executed in <Remove All> case
  *-- because we pass .T. in end of gfMover call.
  IF lnBttn = 3
    llRetVal = This.lfRemCal(lnSele)
  ENDIF
  *-- Restore previous status

  This.StyDye.SetOrder(lcCurNdx)
  SELECT (lnCurAlias)
  SET EXACT &lcOldExact
ENDIF
RETURN (llRetVal)
*!*************************************************************
*! Name      : lfRemCal
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/06/2005
*! Purpose   : This Function calculate the expression for removing
*!           : warehouse , and I separate it to keep it available to
*!           : both remove and remove all cases just refrence it by
*!           : the removed item in laTarget array
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnArrRef
*!*************************************************************
*! Returns            : llRemCal
*!*************************************************************
*! Example   : =lfRemCal(lnRef)
*!*************************************************************
FUNCTION lfRemCal
PARAMETERS lnArrRef
*-- lnTotStk  : Hold the total stock in this warehouse, (initially ZERO)
*-- lnTotOrd  : Hold the total order quantity in this warehouse, (initially ZERO)
*-- lnTotWip  : Hold the total wip quantity in this warehouse, (initially ZERO)
lnTotStk  =  0
lnTotOrd  =  0
lnTotWip  =  0
llProceed = .T.

*-- Are you sure you want to remove warehouse XXX.
*-- <  Yes > <  No  >
lnChoice = gfModalGen("QRM42031B42002","Dialog",laTarget[lnArrRef])
*-- If user choice is <  Yes > do the following
*-- Note that if user choice is <  No >, lnChoice have 2, i.e: return False
IF lnChoice = 1

  IF UPPER(ALLTRIM(This.lcDefWH)) == UPPER(ALLTRIM(laTarget[lnArrRef]))
    *-- Can not remove the default warehouse
    *-- <  OK  >
    = gfModalGen("INM42038B42001","DIALOG")
    lnChoice = 2
    llProceed = .F.
  ENDIF

  IF llProceed
    *-- If you find item in StyDye file do the following
    IF This.StyDye.SEEK(PADR(laTarget[lnArrRef],6)+This.loFormSet.AriaForm1.kbStyle.value)

      *-- Using SUM function is to make compatibality with extended size scales
      SUM TotStk, TotOrd, TotWip TO lnTotStk,  lnTotOrd, lnTotWip               ;
        FOR cwarecode+style+dyelot = PADR(laTarget[lnArrRef],6)+This.loFormSet.AriaForm1.kbStyle.value;
            AND EMPTY(DYELOT)
    ENDIF

    IF lnTotOrd >0
      *-- Style XXXX has on order quantity in this warehouse, can not remove
      *-- <  OK  >
      = gfModalGen("INM42032B42000","DIALOG",This.loFormSet.AriaForm1.kbStyle.value)
      lnChoice = 2      && To return False
      llProceed = .F.
    ENDIF            && Ending On Order confirmation IF.
  ENDIF            && Ending Proceed IF.

  IF llProceed
    IF lnTotWip >0
      *-- Style XXXX has WIPPED quantity in this warehouse, can not remove
      *-- <  OK  >
      = gfModalGen("INM42011B42001","DIALOG")
      lnChoice = 2      && To return False
      llProceed = .F.
    ENDIF          && Ending Wipped confirmation IF.
  ENDIF            && Ending Proceed IF.

  IF llProceed
    *-- You have stock in this warehouse
    IF lnTotStk > 0

      *-- You can not remove warehouse,because you are linked with GL
      IF This.llLink_Gl
        *-- Style XXXX has an inventory in this warehouse, can not remove
        *-- <  OK  >
        = gfModalGen("INM42034B42000","DIALOG",This.loFormSet.AriaForm1.kbStyle.value)
        lnChoice = 2    && To return False
      ELSE
        *-- Style XXXX has an inventory in this warehouse, are you sure
        *-- you want to remove.
        *-- <  Yes > <  No  >
        lnChoice = gfModalGen("QRM42036B42002","DIALOG",This.loFormSet.AriaForm1.kbStyle.value)
      ENDIF      && Ending llLink_GL  IF.
    ENDIF      && Ending Stock confirmation IF.
  ENDIF        && ENDING llProceed IF.
ENDIF        && Ending First confirmation IF.
RETURN (lnChoice = 1)
*!*************************************************************
*! Name      : lfvLoc
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/06/2005
*! Purpose   : calling the gfmover with the location arrays and
*!             handling the returned laTarget (i.e: updating in
*!             temp. Location file [from WhsLoc]) brifely we can
*!             say that it is prepair to save locations.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen,lfvNotMove
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvLoc()
*!*************************************************************
FUNCTION lfvLoc


PRIVATE lcSDel
This.lcStyleSel =SUBSTR(This.loFormset.AriaForm1.kbStyle.value,1,This.lnItmWid)
*-- Initialize the mover arrays.
DECLARE laSource[1],laTarget[1]
STORE SPACE(0) TO laSource,laTarget
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
=gfSeek(This.loFormset.AriaForm1.kbStyle.value,'STYLE','STYLE')
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
*-- lcWareH : Hold cWareCode used in SELECT SQL filling arrays
lcWareH = EVALUATE(This.lcWareFil+'.cWareCode')

*-- Prepair to use GfMover hold locations
*-- Fill laSource array with all locations for specific WareHouse
*:B608057,1 SSH change the where clouse condition
*SELECT DISTINCT cLocation FROM WhsLoc WHERE style+color+cwarecode+clocation = SPACE(This.lnItmWid)+SPACE(This.lnClrWid)+lcWareH INTO ARRAY laSource
SELECT DISTINCT cLocation FROM WhsLoc ;
 WHERE CWARECODE+CLOCATION+STYLE+COLOR = lcWareH INTO ARRAY laSource
*:B608057,1 SSH [END]
*-- If no collection of data from last SELECT SQL there is no locations
*-- thus you must return
IF _TALLY = 0
  *-- No Locations assigined to warehouse XXXXXXXXXX.
  *-- <  OK  >
  = gfModalGen("TRM42058B42000","DIALOG" , lcWareH)
  RETURN
ENDIF

*-- lcSelFile : Hold the file used in SQL statement
*-- lcSelExp  : Hold the expression used in SQL statement
*--             according to used file

lcSelFile = This.lcLocFil
*:B608057,1 SSH Change the condition to be two Experetions
*lcSelExp  = "style+color+cwarecode+clocation = '" + This.lcStyleSel+SPACE(This.lnClrWid)+lcWareH + "'"
lcSelExp  = "style+color+cwarecode+clocation = '" + This.lcStyleSel+ "'"
lcSelExp  = lcSelExp+" AND CWARECODE+CLOCATION+STYLE+COLOR = '" + lcWareH + "'"
*:B608057,1 SSH [END]
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
*SELECT DISTINCT cLocation ;
  FROM (This.lcLocFil) WHERE &lcSelExp  INTO ARRAY laTarget
SELECT DISTINCT cLocation ;
  FROM (This.lcLocPerSize) WHERE &lcSelExp AND EMPTY(Size) AND cStatus <> 'D' INTO ARRAY laTarget  
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
*-- If no collection of data from last SELECT SQL redimension array
*-- and store NULL to it.
IF _TALLY = 0
  *-- To avoid have spaces items in laTarget because a bug in GfMover
  IF ALEN(laTarget) > 1 AND laTarget[1] = SPACE(0)
    DIMENSION laTarget[1]
    laTarget = SPACE(0)
  ENDIF
ENDIF

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laSource,@laTarget,LANG_Assign_Bins,!(This.loFormset.activemode = "V"),'',.T.,.T.,This.loFormset)
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
*= gfMover(@laSource,@laTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Assign_Bins,this.loFormSet.GetHeaderText("LANG_Assign_Bins",this.loFormSet.HeaderAlias)),!(This.loFormset.activemode = "V"),'',.T.,.T.,This.loFormset)
llFormReturnMove =  lfBinsMover(@laSource,@laTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Assign_Bins,this.loFormSet.GetHeaderText("LANG_Assign_Bins",this.loFormSet.HeaderAlias)),!(This.loFormset.activemode = "V"),'',.T.,.T.,This.loFormset,STYLE.Scale)
 

IF llFormReturnMove AND This.loFormset.activeMode = "E"
  lnAlias = SELECT(0)
  lcSDel = SET("DELETED")
  SET DELETED OFF  
  SELECT (This.lcLocPerSize)
  lcLocPrSize = This.lcLocPerSize
  SCAN FOR cWarecode+cLocation+Style+Color = lcWareH And cStatus <> 'S'

    IF This.llExtSize AND &lcLocPrSize..cStatus = 'A' AND EMPTY(&lcLocPrSize..Size)
      FOR lnJ = 1 TO ALEN(This.laExtend,1)
        INSERT INTO (This.lcLocFil)                                             ;
                     (cWarecode,cLocation    ,Style                  ,cStatus,Size) ;
              VALUES (&lcLocPrSize..CWARECODE,&lcLocPrSize..CLOCATION, This.lcStyleSel+This.laExtend[lnJ],"A",'')
      ENDFOR
      LOOP
    ENDIF

    IF !SEEK(&lcLocPrSize..STYLE+&lcLocPrSize..COLOR+&lcLocPrSize..CWARECODE+&lcLocPrSize..CLOCATION+&lcLocPrSize..Size,This.lcLocFil,This.lcLocFil)
      INSERT INTO (This.lcLocFil)                               ;
               (cWarecode,cLocation    ,Style    ,cStatus,Size) ;
        VALUES (&lcLocPrSize..CWARECODE,&lcLocPrSize..CLOCATION,&lcLocPrSize..STYLE,&lcLocPrSize..cStatus,&lcLocPrSize..SIze)    
    ELSE
      IF DELETED(This.lcLocFil) AND &lcLocPrSize..cStatus <>'D'
        SELECT (This.lcLocFil)
        RECALL 
      ENDIF 
      IF EVALUATE(This.lcLocFil+'.CSTATUS') ='A'  AND &lcLocPrSize..cStatus ='D'
        SELECT (This.lcLocFil)
        DELETE 
      ENDIF
      REPLACE cStatus with &lcLocPrSize..cStatus IN (This.lcLocFil)
    ENDIF
  ENDSCAN 
  SET DELETED &lcSDel
  SELECT(lnAlias) 
ENDIF
RETURN 
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- You are in Edit mode
IF (This.loFormset.activemode = "E")
  lnAlias = SELECT(0)

  *-- Assure that delete is off to deal with deleted records
  lcSDel = SET("DELETED")
  SET DELETED OFF

  *-- Search for locations for specific warehouse
  SELECT (This.lcLocFil)
  SCAN FOR cWarecode+cLocation+Style+Color = lcWareH
    *-- IF you do not find this location in array this means
    *-- that it deleted we replace its status with "D" or DELETE it,
    *-- corresponding to its current status
    IF ASCAN(laTarget, ALLTRIM(cLocation)) = 0
      IF cStatus = "S"
        REPLACE cStatus WITH "D"
      ELSE
        IF cStatus = "A"
          DELETE
        ENDIF
      ENDIF

    *-- IF you find this location in array this means
    *-- it is an (A)dded one or exi(S)ting one
    ELSE
      *-- If you replace the status for location with "D" in the same screen
      IF cStatus = "D"
        REPLACE cStatus WITH "S"
      ENDIF
      *-- If you delete location in the same screen (New in this screen)
      IF DELETED()
        RECALL
      ENDIF
    ENDIF     && Ending array scan IF.
  ENDSCAN     &&   Ending Search for locations for specific warehouse

  *-- Appending new record for new location if it is not found in file
  *-- loop until you reach the end of array (For all items in array)
  FOR lnI = 1 TO ALEN(laTarget,1)
    IF !EMPTY(laTarget[lnI])
      LOCATE FOR cWarecode+cLocation+Style+Color = lcWareH+laTarget[lnI]+This.lcStyleSel
      IF !FOUND()
        *-- If you use extended size scales loop with data from
        *-- laExtend array filled in edit mode from Style file
        *-- to add multiple records
        IF This.llExtSize
          FOR lnJ = 1 TO ALEN(This.laExtend,1)
            INSERT INTO (This.lcLocFil)                                             ;
                     (cWarecode,cLocation    ,Style                  ,cStatus) ;
              VALUES (lcWareH  ,laTarget[lnI],This.loFormSet.AriaForm1.kbStyle.value+This.laExtend[lnJ],"A")
          ENDFOR
        *-- If you do not use extended size scales add single record
        ELSE
          INSERT INTO (This.lcLocFil)                               ;
                   (cWarecode,cLocation    ,Style    ,cStatus) ;
            VALUES (lcWareH,laTarget[lnI],This.loFormSet.AriaForm1.kbStyle.value,"A")
        ENDIF       && Ending extended size IF.
      ENDIF        && Ending record not found IF.
    ENDIF        && Ending not empty data IF.
  ENDFOR        && Ending for

  *-- Restore old delete status
  SET DELETED &lcSDel

  SELECT(lnAlias)
ENDIF
*!*************************************************************
*! Name      : lfSavscr
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/06/2005
*! Purpose   : 1- Saving data from temporary files to master files
*!           :    (i.e: from lcWareFil to StyDye
*!           :          from lcLocFil  to WhsLoc)
*!           : 2- Update the stocks of Style file in case of Deleted
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
PROCEDURE lfSavScr
PRIVATE lnAlias,lcSDel,llWareUp,llLocUp

This.loFormSet.AriaForm1.LockScreen = .T.
lnAlias = SELECT(0)

*-- Rising flags detemining whether to continue saving or not
SELECT (This.lcWareFil)
SET FILTER TO
LOCATE FOR INLIST(cStatus,'A','M','D')
llWareUp = FOUND()

IF This.llMultiLoc
  SELECT (This.lcLocFil)
  LOCATE FOR INLIST(cStatus,'A','D')
  llLocUp = FOUND()
ELSE
  llLocUp = .F.
ENDIF

*!*	*-- If you change data in your screen
*!*	IF llWareUp OR llLocUp
*!*	  *-- Assure that delete is off to deal with deleted records
*!*	  lcSDel = SET("DELETED")
*!*	  SET DELETED OFF
*!*	ENDIF

*-- In case of changes made in Temporary StyDye File
IF llWareUp
  *-- Save in StyDye File loop
  SELECT (This.lcWareFil)
  SCAN        && Loop in temporary StyDye file
    DO CASE
      *-- If record status is 'A' (i.e:Added record)
      CASE cStatus = 'A'
        IF !This.StyDye.Seek(EVALUATE(This.lcWareFil+'.Style')+EVALUATE(This.lcWareFil+'.cWareCode'))

           INSERT INTO (This.Stydye.lcCursorUpdate)                          ;
                   (Style,cWareCode,GL_Link,Ave_Cost) ;
                   VALUES (EVALUATE(This.lcWareFil+'.Style'),;
                           EVALUATE(This.lcWareFil+'.cWareCode'),;
                           EVALUATE(This.lcWareFil+'.GL_Link'),;
                           Style.Ave_Cost)
           =gfAdd_Info(This.Stydye.lcCursorUpdate)

         ENDIF
          *-- If record status is 'M' (i.e:Modified record)
      CASE cStatus = 'M'

        SELECT StyDye
        IF This.StyDye.Seek(EVALUATE(This.lcWareFil+'.Style')+EVALUATE(This.lcWareFil+'.cWareCode'))
          SELECT StyDye
          SCAN REST WHILE Style+cWareCode+DyeLot = EVALUATE(This.lcWareFil+'.Style') +EVALUATE(This.lcWareFil+'.cWareCode')
            This.StyDye.Replace("GL_Link WITH '"+EVALUATE(This.lcWareFil+'.GL_Link')+"'")
          ENDSCAN
          =gfAdd_Info('STYDYE')
        ENDIF
      *-- If record status is 'D' (i.e:Deleted record)
      CASE cStatus = 'D'
        *-- Pointer to the same record in StyDye
        SELECT StyDye
        IF This.StyDye.Seek(EVALUATE(This.lcWareFil+'.Style') +EVALUATE(This.lcWareFil+'.cWareCode'))
          SELECT StyDye
          SCAN REST WHILE Style+cWareCode+DyeLot = EVALUATE(This.lcWareFil+'.Style') +EVALUATE(This.lcWareFil+'.cWareCode')
            This.StyDye.Delete()
          ENDSCAN
        ENDIF
        *-- Updating Stocks in Style file
        SELECT Style
        lcWareFile = This.lcWareFil
        This.Style.SEEK(EVALUATE(This.lcWareFil+'.Style'))
        This.Style.REPLACE("Style.Stk1   WITH Style.Stk1   - &lcWareFile..Stk1 ,;
                            Style.Stk2   WITH Style.Stk2   - &lcWareFile..Stk2")
        This.Style.REPLACE("Style.Stk3   WITH Style.Stk3   - &lcWareFile..Stk3")
        This.Style.REPLACE("Style.Stk4   WITH Style.Stk4   - &lcWareFile..Stk4")
        This.Style.REPLACE("Style.Stk5   WITH Style.Stk5   - &lcWareFile..Stk5 ,;
                            Style.Stk6   WITH Style.Stk6   - &lcWareFile..Stk6")
        This.Style.REPLACE("Style.Stk7   WITH Style.Stk7   - &lcWareFile..Stk7,;
                            Style.Stk8   WITH Style.Stk8   - &lcWareFile..Stk8,;
                Style.TotStk WITH Style.TotStk - &lcWareFile..TotStk")
    ENDCASE
  ENDSCAN      && Ending Loop in temporary file

  *-- Return cursor of Style file to its original position
  This.Style.SEEK(This.loFormSet.AriaForm1.kbStyle.value)

ENDIF

*-- In case of changes made in Temporary WhsLoc File
IF llLocUp
  *-- Save in WhsLoc File loop
  SELECT (This.lcLocFil)
  SCAN        && Loop in temporary WhsLoc file
    *-- If record status is 'A' (i.e:Added record)
    IF cStatus = 'A'
      *-- badran add.
      
      IF !this.WHSLOC.SEEK(EVALUATE(This.lcLocFil+'.cWarecode')+EVALUATE(This.lcLocFil+'.cLocation')+EVALUATE(This.lcLocFil+'.Style'))
        *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
*!*	        INSERT INTO (this.WHSLOC.lcCursorUpdate)                                                    ;
*!*	                 (Style           ,cWarecode           ,cLocation           ) ;
*!*	          VALUES (EVALUATE(This.lcLocFil+'.Style'),EVALUATE(This.lcLocFil+'.cWarecode'),EVALUATE(This.lcLocFil+'.cLocation'))
        INSERT INTO (this.WHSLOC.lcCursorUpdate)                                                    ;
                 (Style           ,cWarecode           ,cLocation           ,Size) ;
          VALUES (EVALUATE(This.lcLocFil+'.Style'),EVALUATE(This.lcLocFil+'.cWarecode'),EVALUATE(This.lcLocFil+'.cLocation'),EVALUATE(This.lcLocFil+'.Size'))
      ELSE
        SELECT(this.WHSLOC.lcCursorUpdate)    
        LOCATE REST WHILE Style+cWareCode+cLocation = EVALUATE(This.lcLocFil+'.cWarecode')+EVALUATE(This.lcLocFil+'.cLocation')+EVALUATE(This.lcLocFil+'.Style') FOR Size = EVALUATE(This.lcLocFil+'.Size')
        IF !FOUND() 
          INSERT INTO (this.WHSLOC.lcCursorUpdate)                                                    ;
                 (Style           ,cWarecode           ,cLocation           ,Size) ;
           VALUES (EVALUATE(This.lcLocFil+'.Style'),EVALUATE(This.lcLocFil+'.cWarecode'),EVALUATE(This.lcLocFil+'.cLocation'),EVALUATE(This.lcLocFil+'.Size'))
        ENDIF
        *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
      ENDIF
    ELSE      && cStatus = "D"
      *-- If record status is 'D' (i.e:Deleted record)
      IF cStatus = 'D'
        SELECT WhsLoc
        IF This.WHSLOC.Seek(EVALUATE(This.lcLocFil+'.cWareCode')+EVALUATE(This.lcLocFil+'.cLocation');
                 + This.lcStyleSel)
          SELECT WhsLoc
          *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
*!*	          SCAN REST WHILE  cWareCode+cLocation+Style+Color =  EVALUATE(This.lcLocFil+'.cWareCode')+EVALUATE(This.lcLocFil+'.cLocation');
*!*	                 + This.lcStyleSel
          SCAN REST WHILE  cWareCode+cLocation+Style+Color =  EVALUATE(This.lcLocFil+'.cWareCode')+EVALUATE(This.lcLocFil+'.cLocation');
                 + This.lcStyleSel FOR Size = EVALUATE(This.lcLocFil+'.Size')
            *:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 
            This.WHSLOC.Delete()
          ENDSCAN
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN      && Ending Loop in temporary file
ENDIF        && Ending case of changes made in Temporary WhsLoc File
  DIMENSION laTableUpdate[2]
  laTableUpdate[1] = This.Style
  laTableUpdate[2] = This.StyDye
  IF This.llMultiLoc
    DIMENSION laTableUpdate[3]
    laTableUpdate[3] = This.WHSLOC
  ENDIF

  =This.lfTableUpdate()

SELECT (lnAlias)

*-- If you change data in your screen.
*!*	IF llWareUp OR llLocUp
*!*	  SET DELETED &lcSDel
*!*	ENDIF
This.loFormSet.AriaForm1.LockScreen = .F.

*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/05/2005
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
  llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.

ENDDEFINE

*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][Start] 
*!*************************************************************
*! Name      : lfBinsMover
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/05/2005
*! Purpose   : function to call bins mover
*!*************************************************************
FUNCTION lfBinsMover

PARAMETERS laSource,laTarget,lcMovTitle,llEditable,lcVldFunc,llMV1By1,llReMV1By1,oCalledFormSet,lcScaleCode

*-Make the viriables private to be seen on form.
PRIVATE laSource,laTarget,laOldSour,laOldTarg,lnOldDim,lnOldSour,;
        lcVldFunc,llMV1By1,llReMV1By1,lnLoop,llFormReturn

llFormReturn = .T.  &&Default from return is .T. , .F. for Cancel.
*- Redimension source and target array adding column dimension
DIMENSION laSource[ALEN(laSource,1),1],laTarget[ALEN(laTarget,1),1]

*** This for the mover title. ***
lcMovTitle = IIF(TYPE("lcMovTitle") $ "UL" , "Mover" , IIF(EMPTY(lcMovTitle),"Mover" ,lcMovTitle))


LOCAL oCalledFromForm
llEditable = IIF(TYPE("llEditable") = "U" ,.F., llEditable)
IF llEditable
  llCanVald = .T.
ELSE
  llCanVald = (oCalledFormSet.ActiveMode $ "EA")
ENDIF

*Add this variable to hold number of item to be valid in <ReMoveAll> case because changes occur to laTarget.
lnLoop = 0

EXTERNAL ARRAY laSource,laTarget

lnOldDim =ALEN(laTarget,1)
DECLARE laOldTarg[lnOldDim]
=ACOPY(laTarget,laOldTarg)

lnOldSour =ALEN(laSource,1)
DECLARE laOldSour[lnOldSour]
=ACOPY(laSource,laOldSour)


IF ALEN(laTarget,1) = 1 .AND. TYPE('laTarget[1]')="L"
  laTarget =' '
ENDIF

FOR lnCount = 1 TO ALEN('laSource',1)
  IF ASCAN('laTarget',ALLTRIM(laSource[lnCount])) > 0
    laSource[lnCount,1] = '\'+laSource[lnCount,1]
  ENDIF
ENDFOR


DO FORM (oAriaApplication.ScreenHome+"IC\icbinmv.scx") WITH lcMovTitle,llCanVald,oCalledFormSet,lcScaleCode

RETURN llFormReturn
*:E307076,1 MMT 10/16/2018 Assign Bin Location/Size[P20180926.0001][End] 