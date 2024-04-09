*:----------------------------------------------------------------
*: Program file        : ICGMAUPC.PRG
*: Program description : Brows style by UPC
*: For screen          : ICGMAUPC.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Inventory Control - (IC)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C102082
*:----------------------------------------------------------------
*: Called From         : 1. System Menu\IC\FILES
*:----------------------------------------------------------------
*: Passed Parameters   : None
*:----------------------------------------------------------------
*: Example             : DO ICGMAUPC
*:----------------------------------------------------------------
*:C200334,1 HBG 21/05/2002 Alow brows style/pack by UPC/SKU
*:----------------------------------------------------------------
lcTmpQuery = ""
lcMtBrowTt = "Style\Pack browse"
lcStyPck   = "\<Style"
lcCanCls   = "\<Close"
lcDesc     = "Desc"
lcUpcNum   = ""
lcAcc      = ""
llViewMod  = .F.
lcOldVal   = ""
lnRChioc2  = 1
*C200334,1 HBG 21/05/2002 Add option for UPC/SKU [Begin]
lnRChioc1  = 1
lcKeyID    = 'UPC#'  
lcTmpPckln = ""
lcAccount  = ""
ldAddDate  = {}
*C200334,1 [End]
IF !gfSetup()
  RETURN
ENDIF

= gfOpenFile(gcDataDir+"STYLEUPC", gcDataDir+"Styupcn","SH")
= gfOpenFile(gcDataDir+"SCALE", gcDataDir+"SCALE","SH")
= gfOpenFile(gcDataDir+"SPCK_LIN", gcDataDir+"SPCK_LIN","SH")
= gfOpenFile(gcDataDir+"STYLE", gcDataDir+"STYLE","SH")
= gfOpenFile(gcDataDir+"SPCK_HDR", gcDataDir+"SPCK_HDR","SH")
IF !WEXIST(gcBaseWind)
  lcTmpQuery = gfTempName()
  *C200334,1 HBG 21/05/2002 Add Temp file to handle case of SKU [Begin]
  lcTmpPckln = gfTempName()
  *C200334,1 [End]
  =lfCretBr()
ELSE
  IF WEXIST("gwcContrl1")
    RELEASE WINDOW gwcContrl1
  ENDIF
ENDIF
*C200334,1 HBG 21/05/2002 Change the header of the screen [Begin]
*lcWindtitl = "Inquire by UPC#"
SELECT (lcTmpQuery)
SCATTER MEMVAR MEMO
lcWindtitl = "Inquire Style/Pack by UPC# or SKU"
*C200334,1 [End]
DO (gcScrDir + gcWinAppl + '\ICUPC.SPR')
POP KEY
RELEASE WINDOW (lcMtBrowTt)
RELEASE PAD _Option OF _MSYSMENU
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

*!*************************************************************
*! Name      : lfActsBrow  C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
FUNCTION lfActsBrow

SELECT &lcTmpQuery
GO TOP
IF lnRChioc2 = 2
  *C200334,1 HBG 21/05/2002 Change the header of the browse [Begin]
  *lcMtBrowTt = "UPC Style"
  lcStyPck   = "\<Style"
  IF lnRChioc1 = 1
    lcMtBrowTt = "UPC Style"
  ELSE
    lcMtBrowTt = "SKU Style"
  ENDIF
  *C200334,1 [End]
  lcBrfield1 = [cMark   :02 :H = ''            :R ,] +;
               [Style   :20 :H = 'Style-Color' :R,] +;
               [Desc    :20 :H = lcDesc        :R ,] +;
               [Size    :07 :H = 'Size'        :R ,] +;
               [SPrice  :12 :H = 'S.Price'     :R ,] +;
               [AddDate :10 :H = 'Gener. Date' :R]
ELSE
  *C200334,1 HBG 21/05/2002 Change the header of the browse and the fields of it[Begin]
  *lcMtBrowTt = "UPC Pack"
  lcStyPck   = "\<Pack"
  IF lnRChioc1 = 1
    lcMtBrowTt = "UPC Pack"
  ELSE
    lcMtBrowTt = "SKU Pack"
  ENDIF
  *lcBrfield1 = [cMark   :02 :H = ''            :R ,] +;
               [Account :08 :H = "Account"     :R,] +;
               [PackId  :12 :H = 'Pack Id'     :R,] +;
               [Desc    :20 :H = lcDesc        :R,] +;
               [Style   :20 :H = 'Style-Color' :R,] +;
               [SPrice  :12 :H = 'S.Price'     :R,] +;
               [AddDate :10 :H = 'Gener. Date' :R]

  lcBrfield1 = [cMark   :02 :H = ''            :R,] +;
               [Style   :20 :H = 'Style-Color' :R,] +;
               [SPrice  :12 :H = 'S.Price'     :R ]
  *C200334,1 [End]
ENDIF
SHOW GET pbLunch,1 PROMPT lcStyPck

BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows();
       VALID :F lfvBrowse() ;
       WINDOW ICUPC2 IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcMtBrowTt)

*!*************************************************************
*! Name      : lfUnTrap C102082
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 04/02/00
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfUnTrap

* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If TOP window is not one of the browses window                                        
IF ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
  ON KEY LABEL ENTER
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfKey C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Screen When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey

PUSH KEY
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMtBrowTt KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMtBrowTt)
IF !EMPTY(lcUpcNum)
  SHOW GET lcUpcNum  DISABLE
  SHOW GET lnRChioc2 DISABLE
  *C200334,1 HBG 21/05/2002 Add option for SKY / UPC [Begin]
  SHOW GET lnRChioc1 DISABLE
  *C200334,1 [End]
ENDIF
IF llViewMod
  SHOW GET lcUpcNum  DISABLE
  SHOW GET pbLunch   ENABLE
  SHOW GET lnRChioc2 DISABLE
  *C200334,1 HBG 21/05/2002 Add option for SKY / UPC [Begin]
  SHOW GET lnRChioc1 DISABLE
  *C200334,1 [End]
  SHOW GET IBBROW    DISABLE
ENDIF

*!*************************************************************
*! Name      : lfTrap  C102082
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 04/02/00
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab
ENDIF
*-- end of lfTrap.


*!*************************************************************
*! Name      : lpTab  C102082
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 04/02/00
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
   ACTIVATE WINDOW ICUPC3
  _CUROBJ = OBJNUM(pbLunch)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 04/02/00
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  *-- Go to previous window
  ACTIVATE WINDOW ICUPC1		&&Activate popup windows
  _CUROBJ = OBJNUM(lcUpcNum)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
ENDIF  
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lfCretBr C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Create temps.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfCretBr()
*!*************************************************************
FUNCTION lfCretBr

*C200334,1 HBG 21/05/2002 Add Fields to the temp file to handle case of SKU [Begin]
*CREATE TABLE gcWorkDir+(lcTmpQuery);
             (cMark C(1),Type C(1) , Style C(20), Size C(1),;
             Account C(06) ,SPrice N(15,3) , Version C(5),;
             PackId C(16) , Desc C(20) , AddDate D)
CREATE TABLE gcWorkDir+(lcTmpQuery);
             (cMark C(1),Type C(1) , Style C(20), Size C(5),;
             Account C(06) ,SPrice N(15,3) , Version C(5),;
             PackId C(16) , Desc C(20) , AddDate D,PckColor C(6),;
             PckSize C(3),PckSzDsc C(5),PckSlPric N(8,2))
*C200334,1 [End]                       
SELECT(lcTmpQuery)
INDEX ON Type+PackId+Style TAG (lcTmpQuery)

*C200334,1 HBG 21/05/2002 Add Temp file to handle case of SKU [Begin]
SELECT SPCK_LIN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'Size'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 5
laFileStru[lnFileStru+1,4] = 0
CREATE TABLE (lcTmpPckln) FROM ARRAY laFileStru

SELECT SPCK_LIN
SCAN FOR cSKUType = 'S'
  SCATTER MEMVAR MEMO
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI))
    IF m.Qty&lcI <> 0
      EXIT
    ENDIF
  ENDFOR            
  m.Size = IIF(SEEK(m.Style,'STYLE'),IIF(SEEK('S'+STYLE.SCALE,'SCALE'),EVAL('Scale.Sz'+lcI),''),'')
  INSERT INTO (lcTmpPckln) FROM MEMVAR
ENDSCAN

*C200334,1 [End]                       

*!*************************************************************
*! Name      : lfGetData C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Temp Data.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfGetData()
*!*************************************************************
FUNCTION lfGetData

*C200334,1 HBG 21/05/2002 IF UPC [Begin]
IF lnRChioc1 = 1 
  SELECT STYLEUPC
  =SEEK(lcUpcNum)
  SCATTER MEMVAR MEMO
  =SEEK(m.Style,'Style')
  SELECT (lcTmpQuery)
  IF m.lUpcPack   && Pack
    =lfUpdPack()
  ELSE
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE Type    WITH "S",;
            SPrice  WITH Style.Pricea,;
            Desc    WITH Style.Desc,;
            AddDate WITH m.dAdd_Date,;
            Version WITH "",;
            Size    WITH IIF(SEEK(m.Style,'STYLE'),IIF(SEEK('S'+STYLE.SCALE,'SCALE'),EVAL('Scale.Sz'+LEFT(Size,1)),''),'')
  ENDIF
*C200334,1 HBG 21/05/2002 IF SKU [Begin]  
ELSE
  SELECT SPCK_LIN
  =SEEK('S'+lcAcc+ALLTRIM(lcUpcNum))
  SCATTER MEMVAR MEMO
  =SEEK(m.Style,'Style')
  SELECT (lcTmpQuery)
  IF lnRChioc2 = 1   && Pack
    =lfUpdPack()
  ELSE
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE Type    WITH "S",;
            SPrice  WITH Style.Pricea,;
            Desc    WITH Style.Desc,;
            AddDate WITH m.dAdd_Date,;
            Version WITH ""
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      IF m.Qty&lcI <> 0
        EXIT
      ENDIF
    ENDFOR            
    REPLACE Size WITH IIF(SEEK(m.Style,'STYLE'),IIF(SEEK('S'+STYLE.SCALE,'SCALE'),EVAL('Scale.Sz'+lcI),''),'')
  ENDIF
ENDIF  
*C200334,1 [End]
SHOW GET lcUpcNum DISABLE
lcCanCls = "\<Cancel"
SHOW GET pbClose,1 PROMPT lcCanCls
SHOW GET pbLunch ENABLE
SHOW GET lnRChioc2 DISABLE
*C200334,1 HBG 21/05/2002 Disable option of UPC/SKU [Begin]
SHOW GET lnRChioc1 DISABLE
*C200334,1 [End]
SHOW GET IBBROW   DISABLE
=lfActsBrow()

*!*************************************************************
*! Name      : lfvUpcNum  C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Valid UPC Number.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvUpcNum()
*!*************************************************************
FUNCTION lfvUpcNum
PARAMETER llBrowUpc
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT STYLEUPC
IF llBrowUpc .OR. !SEEK(lcUpcNum) .AND. !EMPTY(lcUpcNum)
  =lfUpcBrow()
ENDIF

IF !EMPTY(lcUpcNum)
  =lfGetData()
ELSE
  lcUpcNum = lcOldVal
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfUpdPack C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Get Packs.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfUpdPack()
*!*************************************************************
FUNCTION lfUpdPack
PRIVATE lnOldAls , lnTotQty , lcTmpSty

lnOldAls = SELECT(0)
*C200334,1 HBG 21/05/2002 take the full pack ID [Begin]
*lcTmpSty = PADR(m.Style,16)
lcTmpSty = PADR(m.Style,16)+m.cPkColor+IIF(lnRChioc1 = 1,m.cPkSize,m.cPckSize)
IF SEEK('S'+LEFT(IIF(lnRChioc1 = 1,m.cPkSize,m.cPckSize),1),'SCALE')
  lcSz = RIGHT(IIF(lnRChioc1 = 1,m.cPkSize,m.cPckSize),1)
  lcPckSzDsc = SCALE.Sz&lcSz
ELSE
  lcPckSzDsc = '*****'    
ENDIF  
SET ORDER TO Spck_hdrvr IN SPCK_HDR
*C200334,1 [End]
*--- type+account+pack_id+cpkcolor+cpcksize+cpkversion "SPCK_HDR"
=SEEK("P"+m.Account+lcTmpSty,'SPCK_HDR')
SELECT SPCK_LIN
*C200334,1 HBG 21/05/2002 Set order to the new index [Begin]
SET ORDER TO Spck_linvr
*C200334,1[End]
*--- type+account+pack_id+cpkcolor+cpcksize+cpkversion+style
=SEEK("P"+m.Account+lcTmpSty)
*C200334,1 HBG 21/05/2002 Use the full pack ID [Begin]
*SCAN REST WHILE type+account+pack_id+style = "P"+m.Account+lcTmpSty
SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion+style = "P"+m.Account+lcTmpSty
*C200334,1 [End]
  SCATTER MEMVAR MEMO
  =SEEK(m.Style,"Style")
  SELECT (lcTmpQuery)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *C200334,1 HBG 21/05/2002 Update all pack inforamtion [Begin]
  *REPLACE Type      WITH "P",;
          Desc      WITH SPCK_HDR.Desc,;
          SPrice    WITH m.TotQty*Style.Pricea,;
          AddDate   WITH m.dAdd_Date,;
          PackId    WITH Pack_Id
  REPLACE Type      WITH "P",;
          Desc      WITH SPCK_HDR.Desc,;
          SPrice    WITH SPCK_LIN.nPck_Price,;
          AddDate   WITH m.dAdd_Date,;
          Account   WITH SPCK_HDR.Account,;
          PackId    WITH SPCK_HDR.Pack_Id,;
          PckColor  WITH SPCK_HDR.cpkColor,;
          PckSize   WITH SPCK_HDR.cpckSize,;
          PckSzDsc  WITH lcPckSzDsc,;
          PckSlPric WITH SPCK_HDR.npkslprice
  *C200334,1 [End]                 
ENDSCAN
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfwBrows C102082
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 04/02/00
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows

SELECT (lcTmpQuery)
lnNewRec = RECNO()
REPLACE ALL cMark WITH ' '
GO lnNewRec
REPLACE cMark WITH '>'
lcDesc   = IIF(Type="P","Pack Desc","Style Desc")
lcStyPck = IIF(Type="S","\<Style","\<Pack")
SHOW GET pbLunch,1 PROMPT lcStyPck

*C200334,1 HBG 21/05/2002 Show the new fields of pack information [Begin]
SCATTER MEMVAR MEMO
IF lnRChioc2 = 1   && Pack
  lcAccount = m.Account
  ldAddDate = m.AddDate 
ELSE
  lcAccount = ""
  ldAddDate = {}
ENDIF  
SHOW GET lcAccount
SHOW GET m.PackId 
SHOW GET m.Desc 
SHOW GET m.PckColor 
SHOW GET m.PckSzDsc
SHOW GET m.PckSlPric 
SHOW GET ldAddDate 
SHOW GET lcKeyID
=lfRefresh()
*C200334,1 [End]

SHOW WINDOW (lcMtBrowTt) REFRESH

*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrowse  C102082
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 04/02/00
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse

IF ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.

*:*************************************************************
*: Name      : lfUpcBrow. C102082
*: Developer : Ahmed Salah Shalaby - (SSH).
*: Date      : 04/02/00
*: Purpose   : Function to browse UPC number.
*:*************************************************************
*: Calls     : ..............
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpcBrow()
*:*************************************************************
FUNCTION lfUpcBrow

*C200334,1 HBG 21/05/2002 if UPC [Begin]
IF lnRChioc1 = 1
*C200334,1 [End]
  SELECT STYLEUPC
  IF lnRChioc2 = 2
    lcPbrowtitl = 'Style UPC Number'
    lcbrfields = "cTemp = cUPCNUM1+cUPCNUM2+cUPCNUM3 :15:H='UPC',"+;
                 "STYLE :20:H='Style',"+;
                 "Size = IIF(SEEK(STYLE,'STYLE'),IIF(SEEK('S'+STYLE.SCALE,'SCALE'),EVAL('Scale.Sz'+LEFT(Size,1)),''),''):7:H='Size'"
  ELSE
    lcPbrowtitl = 'Pack UPC Number'
    lcbrfields = "cTemp = cUPCNUM1+cUPCNUM2+cUPCNUM3 :15:H='UPC',"+;
                 "STYLE :20:H='Pack Id'"
  ENDIF
  DIMENSION latemp[3]
  latemp = ''
  llReturn=ariabrow([FOR IIF(lnRChioc2 = 2,!lUpcPack,lUpcPack)],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
                    gnbrfsrow2, gnbrfscol2,'','',;
                    'cUPCNUM1,cUPCNUM2,cUPCNUM3','laTemp')
  lcUpcNum = latemp[1]+latemp[2]+latemp[3]
*C200334,1 HBG 21/05/2002 if SKU [Begin]  
ELSE
  IF lnRChioc2 = 2
    SELECT (lcTmpPckln)
    lcPbrowtitl = 'Style SKU Number'
    lcbrfields = "Pack_id :20:H='SKU',"+;
                 "STYLE   :20:H='Style',"+;
                 "Size    :7:H='Size'"
  ELSE 
    SELECT SPCK_LIN
    lcPbrowtitl = 'Pack SKU Number'
    lcbrfields = "Pack_id :20:H='SKU',"+;
                 "STYLE   :20:H='Pack Id'"
  ENDIF
  DIMENSION latemp[2]
  latemp = ''
  llReturn=ariabrow([FOR type = 'S' AND IIF(lnRChioc2 = 1,cskutype = 'P',cskutype = 'S')],;
                    lcPbrowtitl, gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2,'','',;
                    'Pack_id,Account','laTemp')
  lcUpcNum = latemp[1]
  lcAcc    = latemp[2]
ENDIF
*C200334,1 [End]


*!*************************************************************
*! Name      : lfvCls  C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Valid Close.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvCls()
*!*************************************************************
FUNCTION lfvCls

IF lcCanCls = "\<Close"
  CLEAR READ
  =gfCPClose()
  =GFCLEANUP()
ELSE
  lcCanCls = "\<Close"
  SHOW GET pbClose,1 PROMPT lcCanCls
  lcUpcNum = ""
  lcAcc    = ""
  SHOW GET lcUpcNum ENABLE
  lcDesc = "Desc"
  lcStyPck = "\<Style"
  SHOW GET pbLunch,1 PROMPT lcStyPck
  SHOW GET IBBROW    ENABLE
  SHOW GET lnRChioc2 ENABLE
  *C200334,1 HBG 21/05/2002 Enable the option of UPC/SKU [Begin]
  SHOW GET lnRChioc1 ENABLE
  *C200334,1 [End]
  SELECT (lcTmpQuery)
  DELETE ALL
  *lcKeyID = 'UPC#'
  IF lnRChioc1 = 1
    lcKeyID = 'UPC#'
  ELSE  
    lcKeyID = 'SKU'
  ENDIF
  lcAccount  = ""
  ldAddDate  = {}
  SCATTER MEMVAR MEMO BLANK
  SHOW GET lcAccount
  SHOW GET m.PackId 
  SHOW GET m.Desc 
  SHOW GET m.PckColor 
  SHOW GET m.PckSzDsc
  SHOW GET m.PckSlPric 
  SHOW GET ldAddDate 
  SHOW GET lcKeyID
  =lfRefresh()

  llViewMod = .F.
  =lfActsBrow()
  _CUROBJ = OBJNUM(lcUpcNum)
ENDIF

*!*************************************************************
*! Name      : lfvLaunch  C102082
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/00
*! Purpose   : Valid function for style/pack button.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvLaunch()
*!*************************************************************
FUNCTION lfvLaunch

IF Type = "S"
  lcParam1 = "'" + SUBSTR(&lcTmpQuery..Style,1,12) + "'"
  lcParam2 = "'" + SUBSTR(&lcTmpQuery..Style,14,6) + "'"
  lcParameter = "&lcParam1,'" + &lcParam2 + "'"
  llViewMod = .T.
  DO gpDoProg WITH "AWRICSTYLE", .F., "IC", lcParameter
ELSE
  llViewMod = .T.
  *C200334,1 HBG 21/05/2002 Call Pack screen for GMA [Begin]
  *lcParam1 = "'" + SUBSTR(&lcTmpQuery..Account,1,5) + "'"
  *lcParam2 = "'" + SUBSTR(&lcTmpQuery..PackId,1,16) + "'"
  *lcParameter = "&lcParam1,'" + &lcParam2 + "'"
  *DO gpDoProg WITH "AWRICPACK", .F., "IC", lcParameter
  lcParameter = "'"+SUBSTR(&lcTmpQuery..Account,1,5)+"','"+SUBSTR(&lcTmpQuery..PackId,1,16)+;
                "','"+SUBSTR(&lcTmpQuery..PckColor,1,6)+"','"+SUBSTR(&lcTmpQuery..PckSize,1,3)+"'"
  DO gpDoProg WITH "AWRICPACKG", .F., "IC", lcParameter
  *C200334,1 [End]
ENDIF

*!*************************************************************
*! Name      : lfvUpcSku
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/05/2002
*! Purpose   : Valid function for UPC/SKU button.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvUpcSku()
*!*************************************************************
*C200334,1 
FUNCTION lfvUpcSku

IF lnRChioc1 = 1
  lcKeyID = 'UPC#'
  
ELSE  
  lcKeyID = 'SKU'
ENDIF
SHOW GETS
