*:************************************************************************
*:  Program File: ARIA4XP\PRGS\PW\PWTOOLS.FXP
*:  Module      : PIECE WORK
*:  Desc.       : Tools Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/24/2012
*:  Reference   : *E303153,1 
*:************************************************************************
* Modifications
*B610136,1 [T20121021.0030] TMI 10/31/2012 [Start] reset the record source of the details grid
*B610136,2 [T20121021.0030] TMI 11/22/2012 fix problems in the screen while the R13 test 
*:************************************************************************

lcProg = JUSTSTEM(SYS(16))

*- Call the screen
lcRunScx = lfGetScx("PW\&lcProg..scx")
DO FORM (lcRunScx) 

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
loFormSet.AddProperty('lcProgName',lcProg)
loFormSet.AddProperty('CalendID',0)

lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'PW\PWGLB.FXP') ADDITIVE &&  all these functions will be copied to the ARIAGLB later

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)

*** Load program base file 
loFormSet.AddProperty('lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "SQL"
  .nWorkArea              = .lcBaseFile 
  .otoolbar.nWorkArea     = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CTOLGRPID"
  .cBrowseIndexFields     = "CTOLGRPID"
  .cBrowseIndexName       = "PWTOOLH"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	  = ALLTRIM(sydObjct.CPRGLNAME)  
  *.AriaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea,oAriaApplication.cAria4SysPath)
  .AriaBrFields.edtBrowseFields.Value = "CTOLGRPID  :H='Tool Group ID',"+;
                                        "CTOLGRPDEC :H='Tool Group name',"+;
                                        "CTOLGRPTYP :H='Type',"+;
                                        "CPLANT_ID  :H='Plant ID',"+;
                                        "NCALID     :H='Calender ID',"+;
                                        "NTOLGRPCST :H='Cost',"+;
                                        "NTOLGRPQTY :H='Qty'"
ENDWITH 

*- set the input mask of the keyfield 
WITH loFormSet
  .Ariaform1.AriaKeyField1.KeyTextbox.InputMask = REPLICATE('!',FSIZE(.cBrowseIndexFields,.lcBaseFile))
  .Ariaform1.txtDesc.MaxLength = FSIZE('CTOLGRPDEC',.lcBaseFile)  
  *E303153,1 TMI 31/05/2012 [Start] make the description widht fit
  loFormset.Ariaform1.pf.pg1.txtDesc.MaxLength = FSIZE('CTOOL_DESC','PWTOLDET')
  *E303153,1 TMI 31/05/2012 [End  ] 

ENDWITH   

WITH loFormset
  DIMENSION .laPanelObj[1,6]
  *--Object link & style picture.
  .laPanelObj[1,1]= "cmdPicture"
  .laPanelObj[1,2]= oAriaApplication.BitmapHome+"RELATE.BMP"
  .laPanelObj[1,3]= "mvObjLink"
  .laPanelObj[1,4]= "Object Links"
  .laPanelObj[1,5]= "Object Links"
  .laPanelObj[1,6]= "VAE"
  .AddProperty('lcObjType',"K")
ENDWITH
*!*	"B" Plant
*!*	"H" work center
*!*	"E" Employee
*!*	"J" Machines
*!*	"K" Tools

loFormset.HasMemo = .F.

*WITH loFormset.Ariaform1.pf.pg1.Thumbnail1
WITH loFormset.Ariaform1.cntThumbnail
  .cType = loFormSet.lcObjType
  .cObjectKey = ""  
ENDWITH   

*=lfCalender(loFormSet,loFormset.Ariaform1.pf.pg2.Calendar1,'T')
=lfCalender(loFormSet,loFormset.Ariaform1.pf.pg2.Calendar1,'W')  && I changed it for the lines to show up in the browse
                                                                 && there is a problem here and needs to be discussed with 
                                                                 && Hesham

*- Detail calender file 
loFormSet.AddProperty('DetailCalenderFile',gfTempName())
loFormSet.AddProperty('TmpDetailCalenderFile',gfTempName())
SELECT (loFormSet.DataFile)
=AFIELDS(laStru)
DIMENSION laStru[ALEN(laStru,1)+1,ALEN(laStru,2)]
lnLen = ALEN(laStru,1)
laStru[lnLen,1] = 'CTOOL_ID'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0
STORE .F. TO laStru[lnLen,5],laStru[lnLen,6]
FOR i =  7 TO 16
  laStru[lnLen,i] = ''
ENDFOR 
STORE 0 TO laStru[lnLen,17],laStru[lnLen,18]
  
CREATE CURSOR (loFormSet.DetailCalenderFile) FROM ARRAY laStru
CREATE CURSOR (loFormSet.TmpDetailCalenderFile) FROM ARRAY laStru

=lfDefinePopups_DataSource(loFormset)

*- Define grids
=lfSetToolsControlSource()

*- Tool Code should be Upper case 
loFormset.Ariaform1.pf.pg1.txtToolCode.InputMask = REPLICATE('!',FSIZE('CTOOL_ID','PWTOLDET'))

loFormset.ChangeMode('S')

*- End of lfFormInit.
************************************************************
*! Name      : lfDefinePopups_DataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : lfDefinePopups_DataSource
************************************************************
FUNCTION lfDefinePopups_DataSource
PARAMETERS loFormSet
LOCAL o 

*- Define the Plant popup source 
WITH loFormSet
  loFormSet.AddProperty('laPlant[1,2]','')
  SELECT PEPLANT
  gfSeek('')
  SELECT CPNAME,CPLANT_ID FROM PEPLANT INTO ARRAY loFormSet.laPlant
  o = loFormset.Ariaform1.cboPlant
  o.RowSource = 'Thisformset.laPlant'
  lfColumnWidthes(o)

  *- Tool group type
  o = .Ariaform1.cboType
  .AddProperty('laType[2,2]','')
  .laType[1,1] = 'Actual'
  .laType[1,2] = 'A'
  .laType[2,1] = 'Forecast'
  .laType[2,2] = 'F'
  lfDefineSource(o,'laType')
  
  *- Tool type
  o = .Ariaform1.pf.pg1.cboType  
  lfDefineSource(o,'laType')
  
ENDWITH 

*- End of lfDefinePopups_DataSource.
************************************************************
*! Name      : lfDefineSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : lfDefineSource
************************************************************
FUNCTION lfDefineSource
PARAMETERS o,lcArray
o.RowSource = 'Thisformset.'+lcArray
lfColumnWidthes(o)

*- End of lfDefineSource.

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/14/2012
*! Purpose   : Form Activate
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate.

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
 
loFormSet.Ariaform1.AriaKeyField1.Enabled = .F.

*- Set calender mode
loFormset.Ariaform1.pf.pg2.Calendar1.formmode = IIF(loFormset.ActiveMode $ 'AE' , 'E' , 'V' )
loFormset.Ariaform1.pf.pg2.Calendar1.grdCalendar.RecordSource = loFormSet.DataFile

DO CASE 
CASE loFormSet.ActiveMode = 'S'
  SELECT (loFormSet.DataFile)
  ZAP 

  loFormset.Ariaform1.pf.ActivePage = 1
  loFormSet.Ariaform1.AriaKeyField1.Enabled = .T.
  loFormSet.Ariaform1.AriaKeyField1.KeyTextbox.Setfocus()

  *WITH loFormset.Ariaform1.pf.pg1.Thumbnail1
  WITH loFormset.Ariaform1.cntThumbnail
  .cType = loFormset.lcObjType
  .cObjectKey = ""
  ENDWITH 

CASE loFormSet.ActiveMode = 'V'
  
  SELECT PWTOOLH 
  lcID = PWTOOLH.CTOLGRPID 
  =gfSeek(lcID,'PWTOOLH')     && re- seek as the previous seek was done via the browse screen and it may do not contain all fields
  loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value = CTOLGRPID 
  SCATTER MEMVAR memo 

  WITH loFormset.Ariaform1
    .AriaKeyField1.KeyTextbox.Value =  m.CTOLGRPID 
    .cboType.Value =  m.CTOLGRPTYP
    .txtDesc.Value =  m.CTOLGRPDEC
    .cboPlant.Value =  m.CPLANT_ID 
    .spnAvailableNumber.Value =  m.NTOLGRPQTY
    .spnUsageRate.Value =  m.NTOLGRPCST
  ENDWITH 

  *WITH loFormset.Ariaform1.pf.pg1.Thumbnail1
  WITH loFormset.Ariaform1.cntThumbnail
    .cType = loFormset.lcObjType  
    .cObjectKey = PWTOOLH.CTOLGRPID 
  ENDWITH 

  SELECT (loFormSet.DataFile)
  ZAP 
  
  lnW = 10
   
  gfSeek(PWTOOLH.CPLANT_ID,'PEPLANT')
  =gfSeek(STR(PEPLANT.NCALID,lnW),'SCCALDTL')
  SELECT SCCALDTL
  SCAN
    SCATTER MEMVAR MEMO
    m.LLOCK = .T.
    *m.crectyp = 'T'
    m.crectyp = 'W'
    SELECT (loFormSet.DataFile)
    APPEND BLANK
    GATHER MEMVAR MEMO    
  ENDSCAN   
  
  =gfSeek(STR(PWTOOLH.NCALID,lnW),'SCCALDTL')
  SELECT SCCALDTL
  SCAN
    SCATTER MEMVAR MEMO
    m.LLOCK = .F.
    *m.crectyp = 'T'
    m.crectyp = 'W'
    SELECT (loFormSet.DataFile)
    APPEND BLANK
    GATHER MEMVAR MEMO    
  ENDSCAN 
 
  IF RECCOUNT(loFormSet.DataFile) > 0
    loFormset.Ariaform1.pf.pg2.Calendar1.Create()  
  ENDIF 
  
  
  *- Fill in the PWTOLDET temp cursor
  loFormset.Ariaform1.pf.pg1.grdTools.RecordSource = ''
  =gfSeek(PWTOOLH.CTOLGRPID,'PWTOLDET')
  lfSetToolsControlSource()
  *loFormset.Ariaform1.pf.pg1.grdTools.Refresh()
  
  
  *- Get Calender detail lines
  SELECT (loFormSet.DetailCalenderFile)
  ZAP
  
  SELECT PWTOLDET
  SCAN 
    =gfSeek(STR(PWTOLDET.NCALID,10),'SCCALDTL')
    M.LLOCK = .F.
    M.CTOOL_ID  = PWTOLDET.CTOOL_ID  
    SELECT SCCALDTL
    SCAN 
      SCATTER MEMVAR MEMO
      SELECT (loFormSet.DetailCalenderFile)
      APPEND BLANK
      GATHER MEMVAR MEMO      
    ENDSCAN
  ENDSCAN 
  
  loFormset.Ariaform1.pf.pg1.cmdCalendar.Enabled = .T.

  loFormSet.CheckNavigation()
  loFormSet.oToolBar.Navrefresh()

CASE loFormSet.ActiveMode = 'E'
  loFormset.Ariaform1.pf.pg2.Calendar1.Enabled = .T.
  loFormset.Ariaform1.pf.pg2.Calendar1.Create()  

CASE loFormSet.ActiveMode = 'A'
  loFormset.Ariaform1.pf.pg2.Calendar1.Enabled = .T.
  loFormset.Ariaform1.pf.pg2.Calendar1.Create()  
  *E303153,4 TMI 06/06/2012 [Start] empty the detail table
  =gfSeek(loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value,'PWTOLDET')
  *E303153,4 TMI 06/06/2012 [End  ] 
  *B610136,1 [T20121021.0030] TMI 10/31/2012 [Start] reset the record source of the details grid
  loFormset.Ariaform1.pf.pg1.grdTools.RecordSource = ''
  lfSetToolsControlSource()
  *B610136,1 [T20121021.0030] TMI 10/31/2012 [End  ] 
  
  WITH loFormset.Ariaform1
    .cboType.Value =  loFormSet.laType[1,2]
    .spnAvailableNumber.Value =  1
    .spnUsageRate.Value = 0
  ENDWITH 

ENDCASE 

*E303153,1 TMI 31/05/2012 [Start] this seems to be the convers wat, to be commented
*IF loFormSet.ActiveMode $ 'AE'
IF loFormSet.ActiveMode $ 'SV'
*E303153,1 TMI 31/05/2012 [End  ] 
  WITH loFormset.Ariaform1.pf.pg1
    *.txtToolCode.Enabled = .F.
    .txtDesc.Enabled = .F.
    .cboType.Enabled = .F.
    .txtPriority.Enabled = .F.
    .spnEfficiency.Enabled = .F.
    .txtPurchaseDate.Text1.Enabled = .F.
    .txtToolLifeTime.Enabled = .F.
    .txtToolCostUsage.Enabled = .F.
    .txtActualUsage.Enabled = .F.
  ENDWITH 
ENDIF 
loFormset.Ariaform1.pf.pg1.txtToolCode.Enabled = .F.

loFormset.Ariaform1.pf.pg2.Calendar1.grdCalendar.Refresh()

SELECT (lnSlct)

*- End of lfChangeMode.
************************************************************
*! Name      : lfgrdToolsfterRowColChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : grid Tools afterRowColChange method
************************************************************
FUNCTION lfgrdToolsfterRowColChange
PARAMETERS loFormSet

*- update the fields 
*!*	PWTOLDET.CTOOL_ID
*!*	PWTOLDET.CTOOL_DESC  
*!*	PWTOLDET.CTOLGRPTYP
*!*	PWTOLDET.CPRIORITY
*!*	PWTOLDET.CTOOL_EFF
*!*	PWTOLDET.DTOOLPUR
*!*	PWTOLDET.NTOOLLIF
*!*	PWTOLDET.NTOOLCST
*!*	PWTOLDET.NTOOLUSD

WITH loFormset.Ariaform1.pf.pg1
  .txtToolCode.Value = PWTOLDET.CTOOL_ID
  .txtToolCode.Enabled = .F.  
  .txtDesc.Value = PWTOLDET.CTOOL_DESC  
  .cboType.Value = PWTOLDET.CTOLGRPTYP
  .txtPriority.Value = PWTOLDET.CPRIORITY
  .spnEfficiency.Value = PWTOLDET.CTOOL_EFF
  .txtPurchaseDate.Text1.Value = PWTOLDET.DTOOLPUR
  .txtToolLifeTime.Value = PWTOLDET.NTOOLLIF
  .txtToolCostUsage.Value = PWTOLDET.NTOOLCST
  .txtActualUsage.Value = PWTOLDET.NTOOLUSD  
  .cmdRemove.Enabled = .T.
ENDWITH 
loFormset.Ariaform1.pf.pg1.grdTools.ReadOnly = .T.
loFormset.Ariaform1.pf.pg1.grdTools.Refresh()
*- End of lfgrdToolsfterRowColChange.
************************************************************
*! Name      : lfSetToolsControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 17/05/2012
*! Purpose   : lfSetToolsControlSource
************************************************************
FUNCTION lfSetToolsControlSource

*- Set grid control Source
LOCAL i,lcI
i = 0
WITH loFormset.Ariaform1.pf.pg1.grdTools
  .RecordSource = ''
  *.ColumnCount = 9    && this is a bug in design, it prevented showing the values in the fields at all
  .RecordSource= 'PWTOLDET'
  
  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Tool Code'
  .Column&lcI..ControlSource   = 'PWTOLDET.CTOOL_ID'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 150
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Tool description'
  .Column&lcI..ControlSource   = 'PWTOLDET.CTOOL_DESC'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 100
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 
  
  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Tool type'
  .Column&lcI..ControlSource   = 'PWTOLDET.CTOLGRPTYP'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 50
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Priority'
  .Column&lcI..ControlSource   = 'PWTOLDET.CPRIORITY'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 40
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Efficiency'
  .Column&lcI..ControlSource   = 'PWTOLDET.CTOOL_EFF'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 40
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Purchased date'
  .Column&lcI..ControlSource   = 'PWTOLDET.DTOOLPUR'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 70
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Tool life time'
  .Column&lcI..ControlSource   = 'PWTOLDET.NTOOLLIF'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 50
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Tool cost'
  .Column&lcI..ControlSource   = 'PWTOLDET.NTOOLCST'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 50  
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  i = i + 1
  lcI = ALLTRIM(STR(i))
  .Column&lcI..Header1.Caption = 'Tool usage till today'
  .Column&lcI..ControlSource   = 'PWTOLDET.NTOOLUSD'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] get the column's width programatically
  *.Column&lcI..WIDTH = 50
  .Column&lcI..WIDTH = lfColumWidth(.Column&lcI..Header1)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 

  .Refresh()
  .AfterRowColChange() 
  .ReadOnly = .T.
ENDWITH   

*- Set controls control source
*!*	WITH loFormset.Ariaform1.pf.pg1
*!*	  .txtToolCode.ControlSource = 'PWTOLDET.CTOOL_ID'
*!*	  .txtDesc.ControlSource = 'PWTOLDET.CTOOL_DESC'
*!*	  .cboType.ControlSource = 'PWTOLDET.CTOLGRPTYP'
*!*	  .txtPriority.ControlSource = 'PWTOLDET.CPRIORITY'
*!*	  .spnEfficiency.ControlSource = 'PWTOLDET.CTOOL_EFF'
*!*	  .txtPurchaseDate.Text1.ControlSource = 'PWTOLDET.DTOOLPUR'
*!*	  .txtToolLifeTime.ControlSource = 'PWTOLDET.NTOOLLIF'
*!*	  .txtToolCostUsage.ControlSource = 'PWTOLDET.NTOOLCST'
*!*	  .txtActualUsage.ControlSource = 'PWTOLDET.NTOOLUSD'
*!*	ENDWITH 

*- End of lfSetQulifyControlSource.

************************************************************
*! Name      : lfColumWidth
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/04/2012
*! Purpose   : Get the column's width programatically
************************************************************
FUNCTION lfColumWidth
PARAMETERS o
LOCAL lnW
lnW = LEN(ALLTRIM(o.Caption)) * FONTMETRIC(6,o.FontName,o.FontSize) 
lnW = lnW + 20
RETURN lnW
*- End of lfColumWidth.

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


SELECT (loFormset.lcBaseFile)
SCATTER MEMVAR 
IF loFormSet.ActiveMode = 'A'
  APPEND BLANK 
  SCATTER MEMVAR blank
ENDIF

WITH loFormset.Ariaform1
  m.CTOLGRPID =  .AriaKeyField1.KeyTextbox.Value 
  m.CTOLGRPTYP=  .cboType.Value 
  m.CTOLGRPDEC=  .txtDesc.Value 
  m.CPLANT_ID =  .cboPlant.Value 
  m.NTOLGRPQTY=  .spnAvailableNumber.Value 
  m.NTOLGRPCST=  .spnUsageRate.Value 
ENDWITH 
GATHER MEMVAR
gfAdd_Info(loFormset.lcBaseFile)

*- save the calender data for the tools groups
lnSeq = PWTOOLH.NCALID
SELECT (loFormSet.DataFile)
SET FILTER TO 
LOCATE
LOCATE FOR !LLOCK
IF FOUND() 

  =lfGenNewCalID(loFormSet,@lnSeq)
  M.NCALID = lnSeq
  
  llLineAdded = .F.
  SELECT (loFormSet.DataFile)
  LOCATE 
  SCAN FOR !LLOCK
    SCATTER MEMVAR MEMO
    m.NCALID = lnSeq
    SELECT SCCALDTL
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE Rec_no WITH ''
    gfAdd_info('SCCALDTL')
    llLineAdded = .T.
  ENDSCAN
  
  IF llLineAdded
  SELECT SCCALDTL
  SCAN   
   =gfReplace('')
  ENDSCAN

  SELECT SCCALDTL
  =gfTableUpdate()
  ENDIF 
ENDIF   

SELECT (loFormset.lcBaseFile)
=gfReplace('')
=gfTableUpdate()

*- Update detailed Calender
SELECT PWTOLDET
SCAN
  lnSeq = PWTOLDET.NCALID
  =lfGenNewCalID(loFormSet,@lnSeq)
  M.NCALID = lnSeq


  *- clear SCCALDTL lines
  SELECT SCCALDTL
  gfSeek(CHR(255))
*!*	  lcDelStat = SET("Deleted")
*!*	  SET DELETED OFF
*!*	  replace rec_no WITH '' all
*!*	  DELETE ALL 
*!*	  LOCATE 
*!*	  SET DELETED &lcDelStat  

  llLineAdded = .F.
  SELECT (loFormSet.DetailCalenderFile)
  LOCATE 
  SCAN FOR CTOOL_ID = PWTOLDET.CTOOL_ID
    
    SCATTER MEMVAR MEMO
    m.NCALID = lnSeq
    SELECT SCCALDTL
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE Rec_no WITH ''
    gfAdd_info('SCCALDTL')
    llLineAdded = .T.
  ENDSCAN
  
  IF llLineAdded
  SELECT PWTOLDET
  REPLACE NCALID WITH M.NCALID
  
  SELECT SCCALDTL
  SCAN   
   =gfReplace('')
  ENDSCAN

  SELECT SCCALDTL
  =gfTableUpdate()
  ENDIF 
ENDSCAN 

*- Update detail file
lfUpdSqlFile('PWTOLDET')

SELECT (lnSlct)

*- End of lfFormSavefiles.
************************************************************
*! Name      : lfGenNewCalID
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 17/05/2012
*! Purpose   : Generate new Calender ID 
************************************************************
FUNCTION lfGenNewCalID
PARAMETERS loFormSet,lnSeq
LOCAL lnSlct
lnSlct = SELECT(0)

IF lnSeq = 0
  
  SELECT SCCALHDR
  APPEND BLANK
  *B610136,2 [T20121021.0030] TMI 11/22/2012 [Start] in case that the NCALID does not exist in the SEQUENCE table
  DO WHILE lnSeq = 0
    *B610136,2 [T20121021.0030] TMI 11/22/2012 [End  ] 
    lnSeq = INT(VAL(gfSequence('NCALID')))
    *B610136,2 [T20121021.0030] TMI 11/22/2012 [End  ] 
  ENDDO 
  *B610136,2 [T20121021.0030] TMI 11/22/2012 [End  ] 
  REPLACE NCALID WITH lnSeq
  gfAdd_info('SCCALHDR')
  =gfReplace('')
    
  SELECT SCCALHDR
  =gfTableUpdate()
  
  *- Empty the SCCALDTL cursor
  *gfSeek(CHR(255),'SCCALDTL')

ELSE
  
  SELECT SCCALDTL
  gfSeek(STR(lnSeq,10))
  SCAN
    gfDELETE()
  ENDSCAN 
  gfTableUpdate()
    
ENDIF 
 
SELECT (loFormset.lcBaseFile)
REPLACE NCALID WITH lnSeq
    
SELECT(lnSlct)    
RETURN lnSeq
*- End of lfGenNewCalID.

************************************************************
*! Name      : lfFormDelete
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
*! Purpose   : lfFormDelete
************************************************************
FUNCTION lfFormDelete
PARAMETERS loFormSet
LOCAL lnSlct,llFound,lcFile,lcFld,lcToolGrpID
lnSlct = SELECT(0)

*E303153,4 TMI 05/06/2012 [Start] check if field used in other files
lcToolGrpID = loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value
lcFld = loFormSet.cBrowseIndexExpression

IF !lfCheckifKeyUsed(lcToolGrpID , lcFld , 'PWTOLDET|PWTOOLH ')
  RETURN .F.
ENDIF   
*E303153,4 TMI 05/06/2012 [End  ] 

*- Delete the related Calenders
lfDelCalender(PWTOOLH.NCALID)
SELECT PWTOLDET
SCAN 
  lfDelCalender(PWTOLDET.NCALID)
ENDSCAN 

*- Delete the details file
lfUpdSqlFile('PWTOLDET',.T.)

*- Delet main record
SELECT (loFormset.lcBaseFile)
gfDelete()
gfTableUpdate()

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

IF EMPTY(loFormset.Ariaform1.txtDesc.Value)
  =gfModalGen('INM38267B00000','DIALOG','Tools Group Name field')
  loFormset.Ariaform1.txtDesc.SetFocus()
  RETURN .F.
ENDIF   
IF EMPTY(loFormset.Ariaform1.cboPlant.Value)
  =gfModalGen('INM38267B00000','DIALOG','Plant field')
  loFormset.Ariaform1.cboPlant.SetFocus()
  RETURN .F.
ENDIF   

SELECT PWTOLDET
LOCATE FOR EMPTY(CTOOL_ID)
IF FOUND()
  =gfModalGen('INM38267B00000','DIALOG','Tool Code field')
  *E303153,1 TMI 31/05/2012 [Start] set the active page
  loFormset.Ariaform1.pf.ActivePage = 1
  *E303153,1 TMI 31/05/2012 [End  ] 
  loFormset.Ariaform1.pf.pg1.txtToolCode.SetFocus()
  RETURN .F.
ENDIF 

*E303153,1 TMI 31/05/2012 [Start] check if tool Name is empty
SELECT PWTOLDET
LOCATE FOR EMPTY(ctool_desc)
IF FOUND()
  =gfModalGen('INM38267B00000','DIALOG','Tool Description field')
  loFormset.Ariaform1.pf.ActivePage = 1
  loFormset.Ariaform1.pf.pg1.grdTools.AfterRowColChange()
  loFormset.Ariaform1.pf.pg1.grdTools.Setfocus()
  RETURN .F.
ENDIF 
*E303153,1 TMI 31/05/2012 [End  ] 

*E303153,1 TMI 31/05/2012 [Start] calendar check 
*llRet = lfCalChk(loFormSet) &&check upon editing calender , not in the saving process
loCalGrd = loFormset.Ariaform1.pf.pg2
llRet = lfCalChk(loFormSet,.T.,loCalGrd)
*E303153,1 TMI 31/05/2012 [End  ] 

SELECT (lnSlct)
RETURN llRet
*- End of lfFormBeforeSave.

*!*	************************************************************
*!*	*! Name      : lfGetShift
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 10/05/2012
*!*	*! Purpose   : Get Shift
*!*	************************************************************
*!*	FUNCTION lfGetShift
*!*	PARAMETERS loFormSet,loFld
*!*	LOCAL lnSlct
*!*	lnSlct = SELECT(0)

*!*	SELECT EMPSHIFT
*!*	REPLACE CSHIFT_ID WITH loFld.Value
*!*	=gfSeek(loFormset.Ariaform1.cboPlant.Value+loFld.Value,'PESHIFT')
*!*	REPLACE CSHIFT_STR WITH PESHIFT.CSHIFT_STR ,;
*!*	        CSHIFT_FNS WITH PESHIFT.CSHIFT_FNS
*!*	loFormset.Ariaform1.pf.pg7.grdShift.AfterRowColChange()
*!*	*!*	loFld.parent.spnStartTime.OLEcontrol1.ObJECT.Value = IIF(EMPTY(CTOT(PESHIFT.CSHIFT_STR)),DATE(),CTOT(PESHIFT.CSHIFT_STR))
*!*	*!*	loFld.parent.spnFinishTime.OLEcontrol1.ObJECT.Value = IIF(EMPTY(CTOT(PESHIFT.CSHIFT_FNS)),DATE(),CTOT(PESHIFT.CSHIFT_FNS))

*!*	SELECT(lnSlct)
*!*	*- End of lfGetShift.

************************************************************
*! Name      : lfvTools
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/01/2012
*! Purpose   : Valid fn for tools
************************************************************
FUNCTION lfvTools
PARAMETERS loFormSet,loFld
LOCAL lnSlct

lnSlct = SELECT(0)

WITH loFld.Keytextbox
.Value     = ALLTRIM(.Value)
.Value     = PADR(.Value,LEN(.InputMask))
ENDWITH 
lcFile_Ttl = loFormSet.BrowseTitle
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value 
llView = .F.
lcBaseFile = loFormSet.lcBaseFile
llBrowse = loFld.Selectedfrombrowse 

SELECT (lcBaseFile)
IF llBrowse .OR. !gfSEEK(loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
  IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
    IF loFormSet.oToolBar.cmdFind.Click()
      llView = .T.
    ELSE
      loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
    ENDIF
  ELSE
    lnOption  = gfModalGen('QRM00001B00001','Dialog',;
                   +ALLTRIM(loFld.KeyTextBox.VALUE))  
      
    DO CASE
      CASE lnOption = 1
        IF loFormSet.oToolBar.cmdFind.Click()
          llView = .T.
        ELSE
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        ENDIF
      CASE lnOption = 2
        lfMode('A')
        RETURN
        
      CASE lnOption = 3
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        RETURN .F.
    ENDCASE
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = &lcBaseFile..CPLANT_ID
  *loFormSet.CHangeMode('V')
  llView = .T.
ENDIF

IF llView = .T.
  lfMode('V')
  =lfChkNav()
ENDIF 

SELECT (lnSlct)

*- End of lfvTools.



*!*	************************************************************
*!*	*! Name      : lfSetMaxLengh
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 06/05/2012
*!*	*! Purpose   : lfSetMaxLengh
*!*	************************************************************
*!*	FUNCTION lfSetMaxLengh
*!*	PARAMETERS loFormset

*!*	loFormset.Ariaform1.pf.pg4.spnQValue.SpinnerHighValue = 100
*!*	loFormset.Ariaform1.pf.pg4.spnQValue.SpinnerLowValue = 0
*!*	WITH loFormset.Ariaform1.pf.pg1
*!*	.txtLastName.MaxLength=FSIZE('CLASTNAME',loFormSet.lcBasefile)
*!*	.txtMidName.MaxLength=FSIZE('CMIDLENAME',loFormset.lcBasefile)
*!*	.txtPassNo.MaxLength=FSIZE('CPASPORTNO',loFormset.lcBasefile)
*!*	*.txtTerminalReason.MaxLength=FSIZE('CTER_RES',loFormset.lcBasefile)
*!*	.txtNationality.MaxLength=FSIZE('CNATIONLTY',loFormset.lcBasefile)
*!*	.txtNativeLang.MaxLength=FSIZE('CLANGUAGE',loFormset.lcBasefile)
*!*	.txtFirstName.MaxLength=FSIZE('CFIRSTNAME',loFormset.lcBasefile)
*!*	.txtSSNo.MaxLength=FSIZE('CSOCIALNO',loFormset.lcBasefile)
*!*	ENDWITH

*!*	WITH loFormset.Ariaform1.pf.pg2
*!*	.Address1.txtAdd1.MaxLength=FSIZE('CADDRESS1',loFormset.lcBasefile)
*!*	.Address1.txtAdd2.MaxLength=FSIZE('CADDRESS2',loFormset.lcBasefile)
*!*	.Address1.txtAdd3.MaxLength=FSIZE('CADDRESS3',loFormset.lcBasefile)
*!*	.Address1.txtAdd4.MaxLength=FSIZE('CADDRESS4',loFormset.lcBasefile)
*!*	.Address1.txtAdd5.MaxLength=FSIZE('CADDRESS5',loFormset.lcBasefile)
*!*	.Address1.txtAdd6.MaxLength=FSIZE('CADDRESS6',loFormset.lcBasefile)
*!*	.txtEmail.MaxLength=FSIZE('CE_MAIL',loFormset.lcBasefile)
*!*	.txtPhone1.MaxLength=FSIZE('PHONE1',loFormset.lcBasefile)
*!*	.txtPhone2.MaxLength=FSIZE('PHONE2',loFormset.lcBasefile)
*!*	.txtMobile.MaxLength=FSIZE('CMOBILE',loFormset.lcBasefile)
*!*	ENDWITH

*!*	WITH loFormset.Ariaform1.pf.pg3&&*-emergancydata
*!*	.txtEmgContactPerson.MaxLength=FSIZE('CEMG_NAME',loFormset.lcBasefile)
*!*	.EmgAddress.txtAdd1.MaxLength=FSIZE('CEMG_ADD1',loFormset.lcBasefile)
*!*	.EmgAddress.txtAdd2.MaxLength=FSIZE('CEMG_ADD2',loFormset.lcBasefile)
*!*	.EmgAddress.txtAdd3.MaxLength=FSIZE('CEMG_ADD3',loFormset.lcBasefile)
*!*	.EmgAddress.txtAdd4.MaxLength=FSIZE('CEMG_ADD4',loFormset.lcBasefile)
*!*	.EmgAddress.txtAdd5.MaxLength=FSIZE('CEMG_ADD5',loFormset.lcBasefile)
*!*	.EmgAddress.txtAdd6.MaxLength=FSIZE('CEMG_ADD6',loFormset.lcBasefile)
*!*	.txtEmail.MaxLength=FSIZE('CEMG_MAIL',loFormset.lcBasefile)
*!*	.txtPhone1.MaxLength=FSIZE('CEMG_PHON1',loFormset.lcBasefile)
*!*	.txtPhone2.MaxLength=FSIZE('CEMG_PHON2',loFormset.lcBasefile)
*!*	.txtMobile.MaxLength=FSIZE('CEMG_MOBIL',loFormset.lcBasefile)
*!*	ENDWITH

*!*	*-Payroll
*!*	WITH loFormset.Ariaform1.pf.pg5
*!*	LOCAL lcCurSmbl
*!*	lcCurSmbl = gfGetCurSmbl(oAriaApplication.BaseCurrency)

*!*	.txtSalary.InputMask = '99999.99'
*!*	.txtMinRate.InputMask = '99999.99'
*!*	.txtHourRate.InputMask = '99999.99'
*!*	.spnOvertimeRate.SpinnerHighValue=999.99
*!*	.spnOvertimeRate.SpinnerLowValue=0
*!*	.spnOvertimeRate.InputMask = '999.99'
*!*	*.txtOvertimeValue.Value
*!*	.txtTerminalReason.MaxLength=FSIZE('CTER_RES',loFormset.lcBasefile)

*!*	ENDWITH
*!*	*-EndoflfSetMaxLengh.


************************************************************
*! Name      : lfvDetails
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 18/05/2012
*! Purpose   : Detail screen functionality
************************************************************
FUNCTION lfvDetails
*B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] add a new parameter to ignore deletion confirmation
*                                                  also don't allow deletion in view mode
*PARAMETERS loFormSet,lcAction
PARAMETERS loFormSet,lcAction,llIgnoreDelConf
IF !loformset.ActiveMode $ 'AE'
  RETURN 
ENDIF   
*B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 
NOTE lcAction : A = Add
NOTE            D = Delete 
NOTE            C = Calendar Detail

DO Case
CASE lcAction = 'A'

  SELECT PWTOLDET
  APPEND BLANK
  REPLACE CTOLGRPID WITH loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value

  WITH loFormset.Ariaform1.pf.pg1
  .txtToolCode.Enabled = .T.
  .txtDesc.Enabled = .T.
  .cboType.Enabled = .T.
  .txtPriority.Enabled = .T.
  .spnEfficiency.Enabled = .T.
  .txtPurchaseDate.Text1.Enabled = .T.
  .txtToolLifeTime.Enabled = .T.
  .txtToolCostUsage.Enabled = .T.
  .txtActualUsage.Enabled = .T.
  
  .txtToolCode.Value = ''
  .txtDesc.Value = ''
  .cboType.Value  = 'A'
  .txtPriority.Value = '1'
  .spnEfficiency.Value = 100
  .txtPurchaseDate.Value = {}
  .txtToolLifeTime.Value = 0
  .txtToolCostUsage.Value = 0
  .txtActualUsage.Value = 0
  *E303153,4 TMI 06/06/2012 [Start] 
  replace CTOLGRPTYP WITH .cboType.Value  ;
          CTOOL_EFF  WITH .spnEfficiency.Value ;
          CPRIORITY  WITH .txtPriority.Value
  *E303153,4 TMI 06/06/2012 [End  ] 
  
  ENDWITH 
  *E303153,1 TMI 31/05/2012 [Start] disable add button
  loFormset.Ariaform1.pf.pg1.cmdAdd.Enabled = .F.
  *E303153,1 TMI 31/05/2012 [End  ] 

  *lfGrdToolsfterRowColChange(loFormSet)
  
  loFormset.Ariaform1.pf.pg1.txtToolCode.Setfocus()
  
CASE lcAction = 'D'
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start]   
  IF EOF('PWTOLDET')
    RETURN 
  ENDIF 
  llContinue = .F.
  IF llIgnoreDelConf 
    llContinue = .T.
  ELSE
    llContinue = gfModalGen('INM00002B00006','DIALOG','remove')=1
  ENDIF 
  IF llContinue
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 
  SELECT PWTOLDET
  DELETE 
  *E303153,1 TMI 31/05/2012 [Start] refresh afer delete
  SKIP -1
  IF BOF()
    GO TOP
    *E303153,4 TMI 05/06/2012 [Start] 
    IF EOF()
      loFormset.Ariaform1.pf.pg1.cmdRemove.Enabled = .F.
    ENDIF 
    *E303153,4 TMI 05/06/2012 [End  ] 
  ENDIF 
  loFormset.Ariaform1.pf.pg1.grdTools.Refresh()
  *E303153,1 TMI 31/05/2012 [End  ] 
  lfGrdToolsfterRowColChange(loFormSet)
  
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] 
  ENDIF 
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 
  
CASE lcAction = 'C'

  IF EMPTY(PWTOLDET.CTOOL_ID)
    =gfModalGen('INM38267B00000','DIALOG','Tool Code field')
    RETURN 
  ENDIF 

  SELECT (loFormSet.TmpDetailCalenderFile)
  ZAP
  
  SELECT (loFormset.DataFile)
  SCAN 
    SCATTER MEMVAR MEMO
    SELECT (loFormSet.TmpDetailCalenderFile)
    APPEND BLANK
    m.CTOOL_ID  = PWTOLDET.CTOOL_ID
    m.LLOCK = .T.
    GATHER MEMVAR MEMO
  ENDSCAN
  
  m.LLOCK = .F.
  *=gfSeek(STR(PWTOLDET.NCALID,10),'SCCALDTL')
  SELECT (loFormSet.DetailCalenderFile)
  SCAN FOR CTOOL_ID  = PWTOLDET.CTOOL_ID
    SCATTER MEMVAR MEMO
    SELECT (loFormSet.TmpDetailCalenderFile)
    APPEND BLANK    
    GATHER MEMVAR MEMO
  ENDSCAN  

  
  lcRunScx = lfGetScx("PW\PWTOLCLDT.scx") 
  DO FORM (lcRunScx) WITH loFormSet
  
  SELECT (loFormSet.DetailCalenderFile)
  DELETE FOR CTOOL_ID = PWTOLDET.CTOOL_ID
  SELECT (loFormSet.TmpDetailCalenderFile)
  SCAN FOR !LLOCK
    SCATTER MEMVAR MEMO
    SELECT (loFormSet.DetailCalenderFile)
    APPEND BLANK 
    GATHER MEMVAR MEMO 
    REPLACE CTOOL_ID WITH PWTOLDET.CTOOL_ID
  ENDSCAN

ENDCASE 

*- End of lfvDetails.
************************************************************
*! Name      : lfUpdSqlFile
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 18/05/2012
*! Purpose   : Update Sql File
************************************************************
FUNCTION lfUpdSqlFile
PARAMETERS lcFile,llDelete
LOCAL lcDele,lnSlct
lnSlct = SELECT(0)
lcDele = SET("Deleted")
SET DELETED OFF

SELECT &lcFile 
SCAN 
  IF DELETED() OR llDelete
    gfDelete()
  ELSE   
    gfReplace('')
  ENDIF 
ENDSCAN 
gfTableUpdate()

SET DELETED &lcDele 
SELECT (lnSlct)
*- End of lfUpdSqlFile.

************************************************************
*! Name      : lfvUpdateField
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 18/05/2012
*! Purpose   : Update Field
************************************************************
FUNCTION lfvUpdateField
PARAMETERS loFormSet,lcValue,lcFld
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT PWTOLDET
REPLACE &lcFld WITH lcValue
loFormset.Ariaform1.pf.pg1.grdTools.Refresh()

SELECT(lnSlct)
*- End of lfvUpdateField.

************************************************************
*! Name      : lfvToolID
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 18/05/2012
*! Purpose   : lfvToolID valid function
************************************************************
*E303153,1 TMI 31/05/2012 [Start] 
FUNCTION lfvToolID
PARAMETERS loFormSet,loFld,lcFld
LOCAL lnSlct
lnSlct = SELECT(0)

*E303153,1 TMI 31/05/2012 [Start] Tool ID cannot be empty.
*04074,� cannot be empty.
IF EMPTY(loFld.Value)
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] 
  *=gfModalGen('INM04074B00000','DIALOG','Tool ID')  
  *RETURN .F.
  RETURN lfRejectMsg(loFormSet,'Tool ID cannot be empty','PWTOLDET',RECNO('PWTOLDET'))
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 
ENDIF   
*E303153,1 TMI 31/05/2012 [End  ] 

SELECT PWTOLDET
*E303153,1 TMI 31/05/2012 [Start] save the record no
LOCAL lnRec
lnRec =  RECNO()
LOCATE FOR &lcFld = loFld.Value
IF FOUND()
  *� already exists. Cannot proceed.
  *00383
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [Start] 
  *=gfModalGen('INM00383B00000','DIALOG','Tool ID')  
  lcMsg = 'Tool ID ['+loFld.Value+'] was entered before'
  RETURN lfRejectMsg(loFormSet,lcMsg,'PWTOLDET',lnRec,RECNO('PWTOLDET'))
  *B610136,1 [T20121021.0030] TMI 11/04/2012 [End  ] 
  loFld.Value = ' '
  loFormset.Ariaform1.pf.pg1.txtToolCode.Value = ' '
ENDIF   
GOTO lnRec
*E303153,1 TMI 31/05/2012 [End  ] 
REPLACE &lcFld WITH loFld.Value
loFormset.Ariaform1.pf.pg1.grdTools.Refresh()

SELECT(lnSlct)
*E303153,1 TMI 31/05/2012 [Start] enable add button
loFormset.Ariaform1.pf.pg1.cmdAdd.Enabled = !EMPTY(loFld.Value)
loFld.Enabled = EMPTY(loFld.Value)
*!*	IF !EMPTY(loFld.Value)
*!*	  WITH loFormset.Ariaform1.pf.pg1
*!*	  .txtToolCode.Value = ''
*!*	  .txtDesc.Value = ''
*!*	  .cboType.Value  = 'A'
*!*	  .txtPriority.Value = '1'
*!*	  .spnEfficiency.Value = 100
*!*	  .txtPurchaseDate.Value = {}
*!*	  .txtToolLifeTime.Value = 0
*!*	  .txtToolCostUsage.Value = 0
*!*	  .txtActualUsage.Value = 0
*!*	  ENDWITH 
*!*	ENDIF 

RETURN !EMPTY(loFld.Value)
*E303153,1 TMI 31/05/2012 [End  ] 
*- End of lfvToolID.
*E303153,1 TMI 31/05/2012 [End  ] 
************************************************************
*! Name      : lfRejectMsg
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/04/2012
*! Purpose   : check if ID is empty or was entered before
************************************************************
FUNCTION lfRejectMsg
PARAMETERS loFormSet,lcMsg,lcAlias,lnRec,lnOldRec
LOCAL lnResp
  *  \!\<Reenter;\?\<Cancel
lnResp = gfModalGen('INM00000B00011',.F.,.F.,.F.,lcMsg)
SELECT &lcAlias
GO (lnRec)
IF lnResp = 1 && keep in the same field
  RETURN .F.
ELSE   && Cancel adding the new line
  =lfvDetails(loFormSet,'D',.T.)
  loFormset.Ariaform1.pf.pg1.cmdAdd.Enabled = .T.
  IF !EMPTY(lnOldRec)
    GO (lnOldRec) IN &lcAlias
    loFormset.Ariaform1.pf.pg1.grdTools.Refresh()
    lfgrdToolsfterRowColChange(loFormSet)    
  ENDIF 
  RETURN .T.
ENDIF 

*- End of lfRejectMsg.

************************************************************
*! Name      : lfColumWidth
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/04/2012
*! Purpose   : Get the column's width programatically
************************************************************
FUNCTION lfColumWidth
PARAMETERS o
LOCAL lnW
lnW = LEN(ALLTRIM(o.Caption)) * FONTMETRIC(6,o.FontName,o.FontSize) 
lnW = lnW + 20
RETURN lnW
*- End of lfColumWidth.