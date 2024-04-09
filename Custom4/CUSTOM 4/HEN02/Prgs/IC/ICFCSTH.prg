*:***********************************************************************
*:  Program file : ICFCSTH.PRG
*:  Program desc.: Forecasting Worksheet for Hen02
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 10/19/2008
*:      Reference: C201053[T20080208.0009]
*:************************************************************************
*: Modifications:
*: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[T20080208.0009]
*: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[T20080208.0009]
*! B609050,1 MMT 10/20/2009 fix bug of forecationg program does not work on SAAS[T20080208.0009]
*! B609050,2 MMT 11/01/2009 fix bug of Error When Call Calc. on SAAS[T20080208.0009]
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[T20080208.0009]
*! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020]
*! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038]
*! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020] 
*:************************************************************************
*B609050,1 MMT 10/20/2009 fix bug of forecationg program does not work on SAAS[Start]
*DO FORM (oAriaApplication.ScreenHome+"\ic\ICFCSTH.SCX")
IF oAriaApplication.MULTIINST
  DO FORM ("X:\aria4xp\Screens"+"\ic\ICFCSTH.SCX")
ELSE
  DO FORM (oAriaApplication.ScreenHome+"\ic\ICFCSTH.SCX")
ENDIF
*B609050,1 MMT 10/20/2009 fix bug of forecationg program does not work on SAAS[End]
RETURN
*!*************************************************************
*! Name      : lfMajTtlGet
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
LOCAL lnResult1
=gfOpenTable('FRCSTHDR','FRCSTHDR','SH','FRCSTHDR')
=gfOpenTable('Style','Style','SH','Style_x')
*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))

*--Make Temp. File From Item Location File
IF !llFirstTim
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC "
  lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"
  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  llFirstTim = .T.
ENDIF

IF lnResult1 >=1
  lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
  =CURSORSETPROP("Buffering",3,lcFabDye)
  SELECT (lcFabDye)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
ENDIF

*!*************************************************************
*! Name      : lfMajPic
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : To get major segment Picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic
*!*************************************************************
*! Name      : lfStySum
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)

lnTotcomp = 0
SELECT Style_X
IF gfSEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF

SELECT STYLE
GO lnStyRec

DO CASE
CASE lnAddToVar = 1
  lnO_T_S = lnTotcomp
CASE lnAddToVar = 2
  lnO_T_S = lnO_T_S + lnTotcomp
CASE lnAddToVar = 3
  lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE

RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfSumFab1
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : sum a specific field for the current fabric
*!                  in fabric file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab1()
*!*************************************************************
FUNCTION lfSumFab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias

lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcFabDye)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcFabDye)
  LOCATE
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfVldType
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : refresh option grid
*!*************************************************************
FUNCTION lfVldType
CLEARREAD()

*!*************************************************************
*! Name      : lfvFrcSt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : Validate Forecast ID
*!*************************************************************
FUNCTION lfvFrcSt
lcObjNam = OGSYS18()
lcObjVal = ALLTRIM(EVALUATE(lcObjNam))
IF !EMPTY(lcObjVal) AND !gfSEEK(lcObjVal,'FRCSTHDR')
  SELECT FRCSTHDR
  gfSeek('')
  DIMENSION laFrCstSelect[1]

  lcBrFields = "CFORECSTID :R :H= 'Forecasting ID' , DSTARTDATE :R :H= 'Start Date',DENDDATE :R :H= 'End Date',DDATE :R :H= 'Revision Date' "
  =AriaBrow('','Forecasting ID', gnbrfsrow1, gnbrfscol1,;
    gnbrfsrow2, gnbrfscol2, '','',;
    "CFORECSTID",'laFrCstSelect')

  IF !EMPTY(laFrCstSelect[1])
    &lcObjNam. = laFrCstSelect[1]
  ELSE
    &lcObjNam. = ''
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfInit
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : Screen Init
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet

SET MULTILOCKS ON
loFormSet.llTotView = .F.
loFormSet.lnMaxScale = 0
=gfOpenTable('FRCSTHDR','FRCSTHDR','SH','FRCSTHDR')
=gfOpenTable('FRCSTDTL','CFORECSTID','SH','FRCSTDTL')
=gfOpenTable('STYLE','STYLE','SH','STYLE')
=gfOpenTable('SCaLE','SCALE','SH','SCALE')
=gfOpenTable('INVLINE','INVLINES','SH','INVLINE')
=gfOpenTable('INVHDR','INVHDR','SH','INVHDR')

=gfOpenTable('POSLN','POSLNS','SH','POSLN')
=gfOpenTable('POSHDR','POSHDR','SH','POSHDR')

=gfOpenTable('ORDLINE','ORDLINES','SH','ORDLINE')
=gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR')
=gfOpenTable('CUSTOMER','CUSTOMER','SH','CUSTOMER')


WITH loFormSet
  .lcFormStyles =  gfTempName()
  .lcTempStyles =  gfTempName()
  .lcFRCSTHDR = gfTempName()
  .lcFRCSTDTL = gfTempName()

  .lcStyWip = gfTempName() && Hold WIP
  .lcStyleInv = gfTempName() && Hold Inv AND New Frcst && And Open To buy
  .lcStyOrd = gfTempName() && Hold Ordered
  .lcStyYTD = gfTempName() && Hold YTD
  .lcStyLYS = gfTempName() && HOLD LYS
  .lcStyScl = gfTempName()
ENDWITH

lfCreatTepFiles(loFormSet)
lfaddCntlSrc(loFormSet)
DIMENSION laHDStr[1,18]
SELECT FRCSTHDR
AFIELDS(laHDStr)
=gfCrtTmp(loFormSet.lcFRCSTHDR,@laHDStr,"CFORECSTID",loFormSet.lcFRCSTHDR,.T.)

DIMENSION laDTLStr[1,18]
SELECT FRCSTDTL
AFIELDS(laDTLStr)
=gfCrtTmp(loFormSet.lcFRCSTDTL ,@laDTLStr,"CFORECSTID+STYLE",loFormSet.lcFRCSTDTL ,.T.)
loFormSet.ariaForm1.pgfFrcst.pgFrcst.ShpTrns.VISIBLE = .F.
loFormSet.ariaForm1.pgfFrcst.pgFrcst.cmdTans.VISIBLE = .F.


*!*************************************************************
*! Name      : lfCreatExp
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : Option grid expression
*!*************************************************************
FUNCTION lfCreatExp
IF lcRpNewIn = 'N'
  lnDatePos = ASCAN(loogscroll.laogfxflt,"FORCAST.DATE")
  lcDateSel = ''
  IF lnDatePos > 0
    lnDatePos  = ASUBSCRIPT(loogscroll.laogfxflt,lnDatePos ,1)
    lcDateSel =IIF(!EMPTY(loogscroll.laogfxflt[lnDatePos ,6]),loogscroll.laogfxflt[lnDatePos ,6],'')
  ENDIF

  IF EMPTY(lcDateSel)
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"You must select forecating date range")
    RETURN .F.
  ENDIF
ENDIF


loFormSet.lcAddOrRev = lcRpNewIn
loFormSet.lcRpStat   = lcRpStat
DIMENSION loFormSet.laogfxflt[ALEN(loogscroll.laogfxflt,1),ALEN(loogscroll.laogfxflt,2)]
ACOPY(loogscroll.laogfxflt,loFormSet.laogfxflt)


*!*************************************************************
*! Name      : lfChangeMode
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/19/2008
*! Purpose   : Change Mode
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet


IF loFormSet.ActiveMode $ 'VS'

  IF USED(loFormSet.lcStyScl)
    SELECT (loFormSet.lcStyScl)
    ZAP
  ENDIF


  IF USED(loFormSet.lcStyLYS)
    SELECT (loFormSet.lcStyLYS)
    ZAP
  ENDIF

  IF USED(loFormSet.lcStyYTD)
    SELECT (loFormSet.lcStyYTD)
    ZAP
  ENDIF

  IF USED(loFormSet.lcStyWip)
    SELECT (loFormSet.lcStyWip)
    ZAP
  ENDIF

  IF USED(loFormSet.lcStyleInv)
    SELECT (loFormSet.lcStyleInv)
    ZAP
  ENDIF

  IF USED(loFormSet.lcFormStyles)
    SELECT (loFormSet.lcFormStyles)
    ZAP
  ENDIF

  IF USED(loFormSet.lcTempStyles)
    SELECT (loFormSet.lcTempStyles)
    ZAP
  ENDIF

  IF USED(loFormSet.lcFRCSTHDR)
    SELECT (loFormSet.lcFRCSTHDR)
    ZAP
  ENDIF

  IF USED(loFormSet.lcFRCSTDTL )
    SELECT (loFormSet.lcFRCSTDTL)
    ZAP
  ENDIF

  IF USED(loFormSet.lcStyOrd )
    SELECT (loFormSet.lcStyOrd )
    ZAP
  ENDIF


  IF loFormSet.ActiveMode = 'S'
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.Nodes.CLEAR()
    loFormSet.lcAddOrRev = ''
    loFormSet.lcRpStat   = ''
    loFormSet.lnMaxScale = 0
    lcExpr = gfOpGrid('ICFCSTHS' , .T.)
    IF EMPTY(lcExpr) OR lcExpr = '.F.'
      RETURN
    ENDIF
    loFormSet.ariaForm1.pgfFrcst.ACTIVEPAGE = 1

  ENDIF

  lnPosStyle = ASCAN(loFormSet.laogfxflt,'STYLE.CSTYMAJOR')
  lcStyFile = ''
  llSeleStyle = .F.
  IF lnPosStyle <> 0
    lnPosStyle = ASUBSCRIPT(loFormSet.laogfxflt,lnPosStyle,1)
    lcStyFile   =  loFormSet.laogfxflt[lnPosStyle,6]
    IF !EMPTY(lcStyFile) AND USED(lcStyFile)
      SELECT (lcStyFile)
      LOCATE
      IF !EOF()
        llSeleStyle = .T.
      ENDIF
    ENDIF
  ENDIF


  lnPosFabric = ASCAN(loFormSet.laogfxflt,'STYLE.FABRIC')
  lcFabFile = ''
  llSeleFab = .F.
  IF lnPosFabric <> 0
    lnPosFabric = ASUBSCRIPT(loFormSet.laogfxflt,lnPosFabric,1)
    lcFabFile =  loFormSet.laogfxflt[lnPosFabric,6]
    IF !EMPTY(lcFabFile ) AND USED(lcFabFile )
      SELECT (lcFabFile )
      LOCATE
      IF !EOF()
        llSeleFab = .T.
      ENDIF
    ENDIF
  ENDIF


  *SEASON
  llUseSeason  = .F.
  lnSeaPos = ASCAN(loFormSet.laogfxflt,"STYLE.SEASON")
  IF lnSeaPos > 0
    lnSeaPos = ASUBSCRIPT(loFormSet.laogfxflt,lnSeaPos,1)
    lcSeaSel =IIF(!EMPTY(loFormSet.laogfxflt[lnSeaPos,6]),loFormSet.laogfxflt[lnSeaPos,6],'')
    IF !EMPTY(lcSeaSel)
      lcSeaFile = gfTempName()
      llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
    ENDIF
  ENDIF

  *DIVISION
  llUseDiv  = .F.
  lnDivPos = ASCAN(loFormSet.laogfxflt,"STYLE.CDIVISION")
  IF lnDivPos > 0
    lnDivPos = ASUBSCRIPT(loFormSet.laogfxflt,lnDivPos,1)
    lcDivSel =IIF(!EMPTY(loFormSet.laogfxflt[lnDivPos,6]),loFormSet.laogfxflt[lnDivPos,6],'')
    IF !EMPTY(lcDivSel)
      lcDivFile = gfTempName()
      llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
    ENDIF
  ENDIF


  *Style Group
  llUseGrp  = .F.
  lnGrpPos = ASCAN(loFormSet.laogfxflt,"STYLE.CSTYGROUP")
  IF lnGrpPos  > 0
    lnGrpPos  = ASUBSCRIPT(loFormSet.laogfxflt,lnGrpPos ,1)
    lcGrpSel =IIF(!EMPTY(loFormSet.laogfxflt[lnGrpPos ,6]),loFormSet.laogfxflt[lnGrpPos ,6],'')
    IF !EMPTY(lcGrpSel)
      lcGrpFile = gfTempName()
      llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
    ENDIF
  ENDIF

  *Color
  llUseClr1  = .F.
  lnClr1Pos = ASCAN(loFormSet.laogfxflt,"STYLE.COLOR")
  IF lnClr1Pos > 0
    lnClr1Pos  = ASUBSCRIPT(loFormSet.laogfxflt,lnClr1Pos,1)
    lcClr1Sel =IIF(!EMPTY(loFormSet.laogfxflt[lnClr1Pos ,6]),loFormSet.laogfxflt[lnClr1Pos,6],'')
    IF !EMPTY(lcClr1Sel )
      lcClr1File = gfTempName()
      llUseClr1= IIF(LEN(lcClr1Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr1Sel,'CSTYCLR',lcClr1File )
    ENDIF
  ENDIF

  *Date
  llUseDAte  = .F.
  loFormSet.ldDateStart = {}
  loFormSet.ldDateEnd = {}
  lnDatePos = ASCAN(loFormSet.laogfxflt,"FORCAST.DATE")
  IF lnDatePos > 0
    lnDatePos  = ASUBSCRIPT(loFormSet.laogfxflt,lnDatePos ,1)
    lcDateSel =IIF(!EMPTY(loFormSet.laogfxflt[lnDatePos ,6]),loFormSet.laogfxflt[lnDatePos ,6],'')
    IF !EMPTY(lcDateSel)
      loFormSet.ldDateStart = CTOD(SUBSTR(loFormSet.laogfxflt[lnDatePos,6],1,10))
      loFormSet.ldDateEnd = CTOD(SUBSTR(loFormSet.laogfxflt[lnDatePos,6],12,21))
      IF !EMPTY(SUBSTR(loFormSet.laogfxflt[lnDatePos,6],1,10)) AND !EMPTY(SUBSTR(loFormSet.laogfxflt[lnDatePos,6],12,21))
        llUseDAte  = .T.
      ENDIF
    ENDIF
  ENDIF

  *FRCSTHDR.CFORECSTID
  llUseFrcst  = .F.
  lcFrcstSel = ''
  lnFrcstPos = ASCAN(loFormSet.laogfxflt,"FRCSTHDR.CFORECSTID")
  IF lnFrcstPos > 0
    lnFrcstPos  = ASUBSCRIPT(loFormSet.laogfxflt,lnFrcstPos ,1)
    lcFrcstSel =IIF(!EMPTY(loFormSet.laogfxflt[lnFrcstPos ,6]),ALLTRIM(loFormSet.laogfxflt[lnFrcstPos ,6]),'')
    IF !EMPTY(lcFrcstSel)
      llUseFrcst  = .T.
    ENDIF
  ENDIF

  lnMajorlen=LEN(gfItemMask("PM","",'0001'))
  STORE 0 TO lnClrLen,lnClrPos,lnSclLen ,lnSclPos

  DIMENSION laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    DO CASE
    CASE laItemSeg[lnCount,1]='C'
      lnClrLen = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      lcClrSpr = ALLT(laItemSeg[lnCount,6])

    CASE  laItemSeg[lnCount,1]='S'
      lnSclLen = LEN(laItemSeg[lnCount,3])
      lnSclPos = laItemSeg[lnCount,4]

    ENDCASE
  ENDFOR


  IF loFormSet.lcAddOrRev = 'I' && Inquire
    IF llUseFrcst && User Selected ID to view
      SELECT FRCSTDTL
      gfSetOrder('CFORECSTID')
      IF gfSeek(lcFrcstSel,'FRCSTHDR')
        SELECT FRCSTHDR
        SCATTER MEMO MEMVAR
        INSERT INTO (loFormSet.lcFRCSTHDR) FROM MEMVAR
        IF  gfSeek(lcFrcstSel,'FRCSTDTL')
          SELECT FRCSTDTL
          SCAN REST WHILE CFORECSTID+STYLE = lcFrcstSel FOR IIF(llSeleStyle ,SEEK(ALLTRIM(SUBSTR(STYLE,1,lnMajorlen)),lcStyFile),.T.) AND ;
              gfSeek(FRCSTDTL.STYLE,'Style') AND ;
              IIF(llSeleFab ,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
              IIF(llUseSeason,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND ;
              IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND ;
              IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
              IIF(llUseClr1,SEEK(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),lcClr1File),.T.) AND ;
              IIF(!EMPTY(loFormSet.lcRpStat),STYLE.STATUS $ loFormSet.lcRpStat,.T.)
            SCATTER MEMO MEMVAR
            IF !SEEK( m.CFORECSTID + m.STYLE,loFormSet.lcFRCSTDTL)
              INSERT INTO (loFormSet.lcFRCSTDTL) FROM MEMVAR
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
    ELSE
      IF llSeleStyle && USer Selected style
        SELECT FRCSTDTL
        gfSetOrder('STYLE')
        SELECT (lcStyFile)
        SCAN
          IF gfSeek(ALLTRIM(&lcStyFile..CSTYMAJOR),'FRCSTDTL')
            SELECT FRCSTDTL
            SCAN REST WHILE STYLE+ CFORECSTID = ALLTRIM(&lcStyFile..CSTYMAJOR) FOR ;
                gfSeek(FRCSTDTL.STYLE,'Style') AND IIF(llSeleFab ,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
                IIF(llUseSeason,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND ;
                IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND ;
                IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
                IIF(llUseClr1,SEEK(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),lcClr1File),.T.) AND ;
                IIF(!EMPTY(loFormSet.lcRpStat),STYLE.STATUS $ loFormSet.lcRpStat,.T.)

              SCATTER MEMO MEMVAR
              IF !SEEK( m.CFORECSTID + m.STYLE,loFormSet.lcFRCSTDTL)
                INSERT INTO (loFormSet.lcFRCSTDTL) FROM MEMVAR
              ENDIF
              =gfSeek( m.CFORECSTID ,'FRCSTHDR')
              IF !SEEK( m.CFORECSTID ,loFormSet.lcFRCSTHDR)
                INSERT INTO (loFormSet.lcFRCSTHDr) FROM MEMVAR
              ENDIF
            ENDSCAN
          ENDIF
        ENDSCAN
      ELSE  && USer does not select style or Forecast ID
        SELECT FRCSTDTL
        gfSetOrder('CFORECSTID')
        SELECT FRCSTHDR
        =gfSeek('')
        SCAN
          SELECT FRCSTDTL
          =gfSeek(FRCSTHDR.CFORECSTID )
          SCAN REST WHILE CFORECSTID+STYLE = FRCSTHDR.CFORECSTID FOR gfSeek(FRCSTDTL.STYLE,'Style') AND;
              IIF(llSeleFab ,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
              IIF(llUseSeason,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND ;
              IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND ;
              IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
              IIF(llUseClr1,SEEK(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),lcClr1File),.T.) AND ;
              IIF(!EMPTY(loFormSet.lcRpStat),STYLE.STATUS $ loFormSet.lcRpStat,.T.)

            SCATTER MEMO MEMVAR
            IF !SEEK( m.CFORECSTID + m.STYLE,loFormSet.lcFRCSTDTL)
              INSERT INTO (loFormSet.lcFRCSTDTL) FROM MEMVAR
            ENDIF
            SELECT FRCSTHDR
            SCATTER MEMO MEMVAR
            IF !SEEK(m.CFORECSTID ,loFormSet.lcFRCSTHDR)
              INSERT INTO (loFormSet.lcFRCSTHDr) FROM MEMVAR
            ENDIF

          ENDSCAN
        ENDSCAN
      ENDIF
    ENDIF
    lfGetFrDetails(loFormSet)
    IF loFormSet.ActiveMode = 'S'
      loFormSet.ChangeMode('V')

    ENDIF
    loFormSet.ariaform1.dtpRev.ENABLED = .F.
    loFormSet.ariaform1.kbStyle.ENABLED = .F.
    loFormSet.ariaForm1.pgfFrcst.ACTIVEPAGE = 1
  ELSE && New

    loFormSet.ChangeMode('A')
    loFormSet.ariaform1.dtpRev.ENABLED = .T.
    loFormSet.ariaform1.kbStyle.ENABLED = .F.
    loFormSet.ariaform1.dtpRev.VALUE = oariaapplication.SystemDate
    loFormSet.ariaForm1.kbFrcstID.ENABLED = .F.



    IF llSeleStyle
      SELECT STYLE
      =gfSetOrder('style')
      SELECT (lcStyFile)
      SCAN
        =gfSeek(ALLTRIM(&lcStyFile..CSTYMAJOR),'Style')
        SELECT STYLE
        SCAN REST WHILE STYLE = ALLTRIM(&lcStyFile..CSTYMAJOR) FOR ;
            IIF(llSeleFab ,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
            IIF(llUseSeason,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND ;
            IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND ;
            IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
            IIF(llUseClr1,SEEK(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),lcClr1File),.T.) AND ;
            IIF(!EMPTY(loFormSet.lcRpStat),STYLE.STATUS $ loFormSet.lcRpStat,.T.)
          m.Style = STYLE.STYLE
          m.DESC = STYLE.DESC
          m.TotInv = STYLE.totstk
          m.TotWip = STYLE.totwip
          m.TotOrd = STYLE.totord
          m.Currpln = STYLE.totplan
          m.TotOts  = STYLE.totwip +STYLE.totstk -STYLE.totord
          IF !SEEK( m.Style,loFormSet.lcTempStyles )
            INSERT INTO (loFormSet.lcTempStyles) FROM MEMVAR
          ENDIF
        ENDSCAN
      ENDSCAN
    ELSE
      SELECT STYLE
      =gfSetOrder('style')
      =gfSeek('')
      SCAN FOR IIF(llSeleFab ,SEEK(STYLE.FABRIC,lcFabFile),.T.) AND ;
          IIF(llUseSeason,SEEK(STYLE.SEASON,lcSeaFile),.T.) AND ;
          IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND ;
          IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.) AND ;
          IIF(llUseClr1,SEEK(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),lcClr1File),.T.) AND ;
          IIF(!EMPTY(loFormSet.lcRpStat),STYLE.STATUS $ loFormSet.lcRpStat,.T.)
        m.Style = STYLE.STYLE
        m.DESC = STYLE.DESC
        m.TotInv = STYLE.totstk
        m.TotWip = STYLE.totwip
        m.TotOrd = STYLE.totord
        m.Currpln = STYLE.totplan
        m.TotOts  = STYLE.totwip +STYLE.totstk -STYLE.totord
        IF !SEEK( m.Style,loFormSet.lcTempStyles )
          INSERT INTO (loFormSet.lcTempStyles) FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDIF

    *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
    SELECT(loFormSet.lcFormStyles)
    SET ORDER TO (loFormSet.lcFormStyles)
    *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

    SELECT (loFormSet.lcTempStyles)
    lcStyleTemp = loFormSet.lcTempStyles
    m.cFrcstID = SPACE(6)
    loFormSet.lnMaxScale = 0

    SCAN
      lnRecNo = RECNO()
      lcStyleClr = SUBSTR(STYLE,1,lnClrLen +lnClrPos  - 1)

      =gfSeek(&lcStyleTemp..STYLE,'STYLE','STYLE')
      =gfSeek("S"+STYLE.SCALE,'Scale','Scale')

      IF SEEK(m.cFrcstID+PADR(lcStyleClr,19),loFormSet.lcFormStyles)

        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
        IF EVALUATE(loFormSet.lcFormStyles+'.nSclCnt') < SCALE.CNT
          REPLACE nSclCnt WITH SCALE.CNT IN (loFormSet.lcFormStyles)
        ENDIF
        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]
        LOOP
      ENDIF

      SELECT (lcStyleTemp)
      m.TotInv = 0
      m.TotWip =0
      m.TotOrd = 0
      m.Currpln = 0
      m.TotOts  = 0
      m.Style = lcStyleClr
      m.DESC = STYLE.DESC
      m.nSclCnt = 0
      =SEEK(lcStyleClr)
      SCAN REST WHILE SUBSTR(STYLE,1,lnClrLen +lnClrPos  - 1) = lcStyleClr
        m.TotInv = m.TotInv + TotInv
        m.TotWip = m.TotWip + TotWip
        m.TotOrd = m.TotOrd + TotOrd
        m.Currpln = m.Currpln + Currpln
        m.TotOts = m.TotOts + TotOts
        IF SCALE.CNT > m.nSclCnt
          m.nSclCnt = SCALE.CNT
        ENDIF
      ENDSCAN
      IF  m.nSclCnt > loFormSet.lnMaxScale
        loFormSet.lnMaxScale = m.nSclCnt
      ENDIF
      m.dSTARTWEEK = loFormSet.ldDateStart
      m.dENDWEEK = loFormSet.ldDateEnd
      m.NQTYCONT = STYLE.NQTYCONT
      m.NMINQTY = STYLE.NMINQTY

      INSERT INTO (loFormSet.lcFormStyles) FROM MEMVAR
      SELECT (lcStyleTemp)
      IF BETWEEN(lnRecNo ,1,RECCOUNT())
        GO RECORD lnRecNo
      ENDIF
    ENDSCAN
  ENDIF
ENDIF


IF USED(loFormSet.lcFormStyles)
  SELECT (loFormSet.lcFormStyles)
  LOCATE
ENDIF

loFormSet.ariaForm1.pgfFrcst.pgHeadr.grdStyles.READONLY = .T.
loFormSet.ariaForm1.pgfFrcst.pgHeadr.grdStyles.REFRESH
IF loFormSet.ActiveMode = 'A' AND  loFormSet.lnMaxScale > 0
  lfCrtStylFiles(loFormSet)
  SELECT(loFormSet.lcFormStyles)
  SCAN
    lfGetStyleDetail(loFormSet)
  ENDSCAN
  SELECT(loFormSet.lcFormStyles)
  LOCATE
ENDIF
lfAftrRowCol(loFormSet)

IF loFormSet.ActiveMode $ 'EAV'
  WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst
    .cmdTans.ENABLED =.T.
    .cmdTotals.ENABLED =.T.
    .cmdExCL.ENABLED =.T.
  ENDWITH
  IF loFormSet.ActiveMode $ 'AE' AND loFormSet.lnMaxScale > 0
    lfDetGrdCntSrc(loFormSet)
    SELECT (loFormSet.lcStyleInv)
    LOCATE
  ENDIF
  IF loFormSet.ActiveMode = 'V'
    loFormSet.ariaform1.dtpRev.ENABLED = .F.
  ENDIF
ELSE
  loFormSet.ariaForm1.pgfFrcst.pgFrcst.cmdTans.ENABLED =.F.
ENDIF

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
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

*!*************************************************************
*! Name      : lfCreatTepFiles
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Create Temp Files
*!*************************************************************
FUNCTION lfCreatTepFiles
PARAMETERS loFormSet
DIMENSION laFileDim[13,4]
laFileDim[1,1] = 'Style'
laFileDim[1,2] = "C"
laFileDim[1,3] = 19
laFileDim[1,4] = 0

laFileDim[2,1] = 'Desc'
laFileDim[2,2] = "C"
laFileDim[2,3] = 60
laFileDim[2,4] = 0

laFileDim[3,1] = 'cFrcstID'
laFileDim[3,2] = "C"
laFileDim[3,3] = 6
laFileDim[3,4] = 0

laFileDim[4,1] = 'TotInv'
laFileDim[4,2] = "N"
laFileDim[4,3] = 9
laFileDim[4,4] = 0

laFileDim[5,1] = 'TotWip'
laFileDim[5,2] = "N"
laFileDim[5,3] = 9
laFileDim[5,4] = 0

laFileDim[6,1] = 'TotOrd'
laFileDim[6,2] = "N"
laFileDim[6,3] = 9
laFileDim[6,4] = 0

laFileDim[7,1] = 'TotOts'
laFileDim[7,2] = "N"
laFileDim[7,3] = 9
laFileDim[7,4] = 0

laFileDim[8,1] = 'Currpln'
laFileDim[8,2] = "N"
laFileDim[8,3] = 9
laFileDim[8,4] = 0

laFileDim[9,1] = 'nSclCnt'
laFileDim[9,2] = "N"
laFileDim[9,3] = 5
laFileDim[9,4] = 0

laFileDim[10,1] = 'dSTARTWEEK'
laFileDim[10,2] = "D"
laFileDim[10,3] = 8
laFileDim[10,4] = 0

laFileDim[11,1] = 'DENDWEEK'
laFileDim[11,2] = "D"
laFileDim[11,3] = 8
laFileDim[11,4] = 0


laFileDim[12,1] = 'NQTYCONT'
laFileDim[12,2] = "N"
laFileDim[12,3] = 7
laFileDim[12,4] = 0

laFileDim[13,1] = 'NMINQTY'
laFileDim[13,2] = "N"
laFileDim[13,3] = 7
laFileDim[13,4] = 0
= gfCrtTmp(loFormSet.lcFormStyles ,@laFileDim,"cFrcstID+Style" ,loFormSet.lcFormStyles ,.T.)

DIMENSION laFileDim[7,4]
laFileDim[1,1] = 'Style'
laFileDim[1,2] = "C"
laFileDim[1,3] = 19
laFileDim[1,4] = 0

laFileDim[2,1] = 'Desc'
laFileDim[2,2] = "C"
laFileDim[2,3] = 60
laFileDim[2,4] = 0

laFileDim[4,1] = 'TotInv'
laFileDim[4,2] = "N"
laFileDim[4,3] = 9
laFileDim[4,4] = 0

laFileDim[5,1] = 'TotWip'
laFileDim[5,2] = "N"
laFileDim[5,3] = 9
laFileDim[5,4] = 0

laFileDim[6,1] = 'TotOrd'
laFileDim[6,2] = "N"
laFileDim[6,3] = 9
laFileDim[6,4] = 0

laFileDim[7,1] = 'TotOts'
laFileDim[7,2] = "N"
laFileDim[7,3] = 9
laFileDim[7,4] = 0

laFileDim[3,1] = 'Currpln'
laFileDim[3,2] = "N"
laFileDim[3,3] = 9
laFileDim[3,4] = 0
= gfCrtTmp(loFormSet.lcTempStyles ,@laFileDim,"Style" ,loFormSet.lcTempStyles ,.T.)

*!*************************************************************
*! Name      : lfaddCntlSrc
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : add Control source
*!*************************************************************
FUNCTION lfaddCntlSrc
PARAMETERS loFormSet
WITH loFormSet.ariaForm1.pgfFrcst.pgHeadr.grdStyles
  .RECORDSOURCE = ''
  .RECORDSOURCE = loFormSet.lcFormStyles
  .column1.CONTROLSOURCE = loFormSet.lcFormStyles+".Style"
  .column1.READONLY = .T.
  .column2.CONTROLSOURCE = loFormSet.lcFormStyles+".DESC"
  .column2.READONLY = .T.
  .column3.CONTROLSOURCE = loFormSet.lcFormStyles+".TotInv"
  .column3.READONLY = .T.
  .column4.CONTROLSOURCE = loFormSet.lcFormStyles+".TotWip"
  .column4.READONLY = .T.
  .column5.CONTROLSOURCE = loFormSet.lcFormStyles+".TotOrd"
  .column5.READONLY = .T.
  .column6.CONTROLSOURCE = loFormSet.lcFormStyles+".TotOts"
  .column6.READONLY = .T.
  .column7.CONTROLSOURCE = loFormSet.lcFormStyles+".Currpln"
  .column7.READONLY = .T.
  .column8.CONTROLSOURCE = loFormSet.lcFormStyles+".cFrcstID"
  .column8.READONLY = .T.
ENDWITH

*!*************************************************************
*! Name      : lfAftrRowCol
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : After Row Column Chnaged of Styles grid
*!*************************************************************
FUNCTION lfAftrRowCol
PARAMETERS loFormSet


IF loFormSet.activeMode = 'A'
  WITH loFormSet.ariaform1
    .kbStyle.KeytextBox.VALUE = EVALUATE(loFormSet.lcFormStyles+".Style")
    .kbStyle.KeytextBox.ENABLED = .F.
    .kbStyle.Keycmd.ENABLED = .F.
    .kbFrcstID.KeytextBox.VALUE = ''
    .txtStyDesc.VALUE = EVALUATE(loFormSet.lcFormStyles+".DESC")
    .txtStyDesc.ENABLED = .F.
    .dtpRev.ENABLED = .T.
  ENDWITH
ELSE
  WITH loFormSet.ariaform1
    .kbStyle.KeytextBox.VALUE = EVALUATE(loFormSet.lcFormStyles+".Style")
    .kbFrcstID.KeytextBox.VALUE = EVALUATE(loFormSet.lcFormStyles+".cFrcstID")
    .kbFrcstID.ENABLED = .F.
    .kbStyle.KeytextBox.ENABLED = .F.
    .txtStyDesc.VALUE = EVALUATE(loFormSet.lcFormStyles+".DESC")
    .txtStyDesc.ENABLED = .F.
    =SEEK(EVALUATE(loFormSet.lcFormStyles+'.cFrcstID'),loFormSet.lcFRCSTHDR)
    .dtpRev.VALUE =EVALUATE(loFormSet.lcFRCSTHDR+'.DDATE')
    .dtpRev.ENABLED = .T.
  ENDWITH
ENDIF

WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst
  .dtpEndW.VALUE = EVALUATE(loFormSet.lcFormStyles+'.dENDWEEK')
  .dtpStrtW.VALUE = EVALUATE(loFormSet.lcFormStyles+'.dSTARTWEEK')
  .txtOrdQty.VALUE =EVALUATE(loFormSet.lcFormStyles+'.NMINQTY')
  .txtQtyCont.VALUE=EVALUATE(loFormSet.lcFormStyles+'.NQTYCONT')
ENDWITH

SELECT (loFormSet.lcFormStyles)
lnCurRec = RECNO()
COUNT FOR !DELETED() TO lnNotDeleted
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GO RECORD lnCurRec
ENDIF

IF lnNotDeleted <> 0 AND loFormSet.ActiveMode $ 'EA'
  loFormSet.ariaForm1.pgfFrcst.pgHeadr.cmdRemove.ENABLED = .T.
ELSE
  loFormSet.ariaForm1.pgfFrcst.pgHeadr.cmdRemove.ENABLED = .F.
ENDIF

IF loFormSet.ActiveMode = 'V'
  loFormSet.ariaform1.dtpRev.ENABLED = .F.
ELSE
  lfCalcOts(loFormSet)
ENDIF


*!*************************************************************
*! Name      : lfGetStyleDetail
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : get Style Details
*!*************************************************************
FUNCTION lfGetStyleDetail
PARAMETERS loFormSet,lcMode
IF TYPE('lcMode') <> 'C'
  lcMode = loFormSet.activeMode
ENDIF

*: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
*lcStyle = EVALUATE(loFormSet.lcFormStyles+'.Style')
lnMajorlen=LEN(gfItemMask("PM","",'0001'))
STORE 0 TO lnClrLen1,lnClrPos1,lnSclLen1 ,lnSclPos1

DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen1 = LEN(laItemSeg[lnCount,3])
    lnClrPos1 = laItemSeg[lnCount,4]
    lcClrSpr1 = ALLT(laItemSeg[lnCount,6])

  CASE  laItemSeg[lnCount,1]='S'
    lnSclLen1 = LEN(laItemSeg[lnCount,3])
    lnSclPos1 = laItemSeg[lnCount,4]

  ENDCASE
ENDFOR
lcStyle = SUBSTR(EVALUATE(loFormSet.lcFormStyles+'.Style'),1,lnClrLen1+lnClrPos1-1)
*: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

IF EMPTY(lcStyle)
  RETURN
ENDIF


IF lcMode= 'A'
  FOR lnJ = 1 TO 8
    lcJ = ALLTRIM(STR(lnJ))
    STORE 0 TO m.OTS&lcJ.
  ENDFOR

  STORE STR(0,1) TO m.TQty
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *IF SEEK(ALLTRIM(lcStyle),loFormSet.lcStyleInv)
  IF SEEK(lcStyle,loFormSet.lcStyleInv)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    RETURN
  ENDIF

  REPLACE TotInv WITH  0,;
    TotWip WITH  0,;
    TotOrd WITH  0 ,;
    TotOts WITH  0,;
    Currpln WITH 0 IN (loFormSet.lcFormStyles)

  =gfSetOrder('Style')

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyle))
  =gfSeek(lcStyle)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  m.NCost = STYLE.ave_cost
  m.NPrice  = STYLE.pricea
  m.Style = lcStyle

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *SCAN REST WHILE Style = ALLTRIM(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    =gfSeek("S"+STYLE.SCALE,'Scale')

    m.Ctype = 'A'
    m.cTitle = "Qty/Size"
    m.Scale = STYLE.SCALE

    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      m.Qty&lcI. = ""
    ENDFOR


    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = SCALE.Sz&lcI.
    ENDFOR
    m.TQty = 'Total'
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR

    FOR lnI = 1 TO  8
      lcI = ALLTRIM(STR(lnI))
      STORE STR(0,1) TO  m.Qty&lcI.
    ENDFOR
    m.TQty = STR(0,1)

    m.Ctype = 'B'
    m.cTitle = "Inventory"
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(STYLE.Stk&lcI.,7)
      m.OTS&lcI. = VAL(m.Qty&lcI.)
      m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    REPLACE TotInv WITH TotInv + VAL(m.TQty) IN (loFormSet.lcFormStyles)
  ENDSCAN

  m.TOTQTY = EVALUATE(loFormSet.lcFormStyles+'.TotInv')

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *m.Style = ALLTRIM(lcStyle)
  m.Style = lcStyle
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  IF !SEEK(m.Style,loFormSet.lcStyScl)
    INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
    lfCalTotCost(loFormSet)
    lfCaltotPric(loFormSet)
  ENDIF

  FOR lnJ = 1 TO 8
    lcJ = ALLTRIM(STR(lnJ))
    STORE STR(0 ,1) TO m.qty&lcJ.
  ENDFOR

  m.TQty = STR(0,1)
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyle))
  =gfSeek(lcStyle)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  m.Style = lcStyle
  m.Ctype = 'C'
  m.cTitle = "WIP"
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *SCAN REST WHILE Style = ALLTRIM(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting POs For Style:"+STYLE.STYLE NOWAIT
    =gfSeek("S"+STYLE.SCALE,'Scale')
    m.Scale = STYLE.SCALE
    m.TQty = STR(0,1)
    FOR lnI = 1 TO  8
      lcI = ALLTRIM(STR(lnI))
      STORE STR(0,1) TO  m.Qty&lcI.
    ENDFOR

    =gfSeek('0001'+STYLE.STYLE+'P','POSLN')

    SELECT POSlN
    *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
    *SCAN REST WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+ STR(lineno,5) +trancd = '0001'+Style.Style+'P' FOR trancd = '1'
    DO WHILE cinvtype+ STYLE+ cbusdocu+ cstytype+ po+ STR(LINENO,5) +trancd = '0001'+STYLE.STYLE+'P' FOR trancd <> '3'
      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

      = gfSeek(POSLN.cbusdocu+ POSLN.cstytype+ POSLN.po,'POSHDR')
      IF !(POSHDR.STATUS $ 'OH')
        SELECT POSlN        
        IF !EOF()
          SKIP 1
        ENDIF 
        LOOP
      ENDIF
      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]

      && Inilitize new variables.
      m.PoLineQTY1 = 0
      m.PoLineQTY2 = 0
      m.PoLineQTY3 = 0
      m.PoLineQTY4 = 0
      m.PoLineQTY5 = 0
      m.PoLineQTY6 = 0
      m.PoLineQTY7 = 0
      m.PoLineQTY8 = 0
      m.PoLineTotQTY = 0
      ln_niCost1     = 0
      lcCurrentLineno   = POSlN.LINENO
      lcCurrentcstytype = POSLn.cstytype

      && Scan and Accumulate on PO + line.
      SCAN REST WHILE cinvtype+ STYLE+ cbusdocu+ cstytype+ po+ STR(LINENO,5)+ trancd = '0001'+ STYLE.STYLE+ 'P'+ lcCurrentcstytype+ POSHDR.PO+ STR(lcCurrentLineno,5) FOR trancd <> '3'
        FOR lnI = 1 TO  SCALE.CNT
          lcI = ALLTRIM(STR(lnI))
          m.PoLineQTY&lcI. = m.PoLineQTY&lcI. + IIF(trancd ='1',POsLN.QTY&lcI.,(-1 * POsLN.QTY&lcI.))
        ENDFOR
        IF trancd = '1'
          ln_niCost1 = Posln.niCost1
        ENDIF
      ENDSCAN

      && Get the Max. with 0, to avoid -ve values.
      FOR lnI = 1 TO  SCALE.CNT
        lcI = ALLTRIM(STR(lnI))
        m.PoLineQTY&lcI. = MAX(0,m.PoLineQTY&lcI.)
        m.PoLineTotQTY  = m.PoLineTotQTY  + m.PoLineQTY&lcI.
      ENDFOR
      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

      FOR lnI = 1 TO  SCALE.CNT
        lcI = ALLTRIM(STR(lnI))
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
        *m.Qty&lcI. = STR(VAL(m.Qty&lcI.) + POsLN.QTY&lcI.,7)
        m.Qty&lcI. = STR(VAL(m.Qty&lcI.) +  m.PoLineQTY&lcI.,7)
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

        m.OTS&lcI. = m.OTS&lcI. +  VAL(m.Qty&lcI.)

        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
        *m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

      ENDFOR

      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
      *IF !SEEK(PADR(ALLTRIM(lcStyle),19)+POSLN.Cstytype+ POSLN.PO,loFormSet.lcStyWip)
      IF !SEEK(PADR(ALLTRIM(lcStyle),19)+lcCurrentcstytype + POSHDR.PO,loFormSet.lcStyWip)
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]
        m.TTYPE    = POSHDR.cStytype

        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
        *m.po = POSLN.PO
        m.po = POSHDR.PO
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

        m.Entered  = POSHDR.Entered
        m.Complete = POSHDR.COMPLETE
        m.Status   = POSHDR.STATUS
        m.Vendor   = IIF(m.TTYPE = 'P',POSHDR.Vendor,POSHDR.cwarecode)
        m.Title = IIF(m.TTYPE = 'P','Purchase Order','Cut Ticket' )
        m.Style = ALLTRIM(lcStyle)

        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
        *m.Qty = POsln.TOTQTY
        *m.Amount = POsln.TOTQTY * POsln.niCost1
        m.Qty = m.PoLineTotQTY
        m.Amount = m.PoLineTotQTY  * ln_niCost1
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

        INSERT INTO (loFormSet.lcStyWip) FROM MEMVAR
      ELSE
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
        *REPLACE Qty WITH Qty+POsln.TOTQTY,;
        *   	    Amount	WITH Amount+ POsln.TOTQTY * POsln.niCost1 IN (loFormSet.lcStyWip)
        REPLACE Qty WITH Qty + m.PoLineTotQTY ,;
          Amount	WITH Amount+ m.PoLineTotQTY * ln_niCost1 IN (loFormSet.lcStyWip)
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]
      ENDIF
      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
      *ENDSCAN
    ENDDO
    *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    REPLACE TotWip WITH TotWip + VAL(m.TQty) IN (loFormSet.lcFormStyles)
  ENDSCAN

  m.TOTQTY = EVALUATE(loFormSet.lcFormStyles+'.TotWip')
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyle))
  =gfSeek(lcStyle)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  m.Style = lcStyle
  m.Ctype = 'D'
  m.cTitle = "Orders"
  m.TQty = STR(0,1)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  SCAN REST WHILE Style = ALLTRIM(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting Orders For Style:"+STYLE.STYLE NOWAIT
    =gfSeek("S"+STYLE.SCALE,'Scale')
    m.Scale = STYLE.SCALE

    FOR lnI = 1 TO  8
      lcI = ALLTRIM(STR(lnI))
      STORE STR(0,1) TO  m.Qty&lcI.
    ENDFOR
    m.TQty = STR(0,1)
    =gfSeek(STYLE.STYLE,'Ordline')
    SELECT Ordline
    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) =STYLE.STYLE FOR ;
        cordtype = 'O' AND gfSeek('O'+Ordline.ORDER,'Ordhdr') AND ordHDR.STATUS $ 'OH'


      FOR lnI = 1 TO  SCALE.CNT
        lcI = ALLTRIM(STR(lnI))
        m.Qty&lcI. =STR(VAL(m.Qty&lcI.) + ORDLINE.QTY&lcI.,7)
        m.OTS&lcI. = m.OTS&lcI. -  VAL(m.Qty&lcI.)

        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
        *m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

      ENDFOR
      IF !SEEK(PADR(ALLTRIM(lcStyle),19)+ ordline.ORDER,loFormSet.lcStyOrd)
        m.Order = Ordline.ORDER
        m.Status   = Ordhdr.STATUS
        m.Style = ALLTRIM(lcStyle)
        m.Qty = Ordline.TOTQTY
        m.Amount = Ordline.TOTQTY * Ordline.Price
        m.Account = Ordline.account
        m.Store = Ordhdr.STORE
        =gfSeek(IIF(!EMPTY(Ordhdr.STORE),'S','M')+Ordhdr.Account+Ordhdr.STORE,'Customer')
        m.Name = Customer.btname
        INSERT INTO (loFormSet.lcStyOrd) FROM MEMVAR
      ELSE
        REPLACE Qty WITH Qty+ordline.TOTQTY,;
          Amount	WITH Amount+ ordline.TOTQTY * ordline.Price IN (loFormSet.lcStyOrd)
      ENDIF
    ENDSCAN

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    REPLACE TotOrd WITH TotOrd + VAL(m.TQty) IN (loFormSet.lcFormStyles)
  ENDSCAN

  m.TOTQTY = EVALUATE(loFormSet.lcFormStyles+'.TotOrd')
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.TQty = STR(0,1)
  m.Style = lcStyle
  m.Ctype = 'E'
  m.cTitle = "OTS"
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyle))
  *SCAN REST WHILE Style = ALLTRIM(lcStyle)
  =gfSeek(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    =gfSeek("S"+STYLE.SCALE,'Scale')


    FOR lnI = 1 TO  8
      lcI = ALLTRIM(STR(lnI))
      STORE STR(0,1) TO  m.Qty&lcI.
    ENDFOR
    m.TQty = STR(0,1)
    m.Scale = STYLE.SCALE
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(0,1)&&m.OTS&lcCnt.
      m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
  ENDSCAN

  m.TOTQTY = 0
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.TQty = STR(0,1)
  m.Style = lcStyle
  m.Ctype = 'F'
  m.cTitle = "Sales Forecast"
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyle))
  =gfSeek(lcStyle)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  lnCnt = 1

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  SCAN REST WHILE Style = ALLTRIM(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    =gfSeek("S"+STYLE.SCALE,'Scale')
    m.Scale = STYLE.SCALE
    FOR lnI = 1 TO  8
      lcI = ALLTRIM(STR(lnI))
      STORE STR(0,1) TO  m.Qty&lcI.
    ENDFOR
    FOR lnI = 1 TO SCALE.CNT
      *lcCnt = ALLTRIM(STR(lnCnt))
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(0,1)
      m.TQty = STR(0,1)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
  ENDSCAN


  m.TOTQTY = 0
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)



  m.TQty = STR(0,1)
  m.Style = lcStyle
  m.Ctype = 'G'
  m.cTitle = "Net OTS"
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  =gfSeek(ALLTRIM(lcStyle))
  *  SCAN REST WHILE Style = ALLTRIM(lcStyle)
  =gfSeek(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    =gfSeek("S"+STYLE.SCALE,'Scale')
    m.Scale = STYLE.SCALE
    FOR lnI = 1 TO  8
      lcI = ALLTRIM(STR(lnI))
      STORE STR(0,1) TO  m.Qty&lcI.
    ENDFOR
    m.TQty = STR(0,1)
    FOR lnI = 1 TO SCALE.CNT
      *lcCnt = ALLTRIM(STR(lnCnt))
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(0,1)&&m.OTS&lcCnt.
      m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI. ),9)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
  ENDSCAN
  m.TOTQTY = 0
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)



  m.TQty = STR(0,1)
  m.Style = lcStyle
  m.Ctype = 'H'
  m.cTitle = "YTD Sales"
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  * =gfSeek(ALLTRIM(lcStyle))
  * SCAN REST WHILE Style = ALLTRIM(lcStyle)
  =gfSeek(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting Invoices For Style:"+STYLE.STYLE NOWAIT
    =gfSeek("S"+STYLE.SCALE,'Scale')
    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)
    m.TOTQTY = 0
    m.Scale = STYLE.SCALE
    SELECT INVLINE
    =gfSeek(STYLE.STYLE)
    SCAN REST WHILE  STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE FOR ;
        BETWEEN(Invline.INVDATE,DATE(YEAR(loFormSet.ldDateStart ),1,1),loFormSet.ldDateStart ) AND gfSeek(Invline.Invoice,'Invhdr') AND Invhdr.STATUS <> 'V'
      FOR lnI = 1 TO SCALE.CNT
        lcI = ALLTRIM(STR(lnI))
        m.Qty&lcI. = STR(VAL(m.Qty&lcI.) + INVLINE.QTY&lcI.,7)

        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
        *m.TQty = STR(VAL(m.TQty) +  VAL(m.Qty&lcI.),9)
        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

      ENDFOR
      IF !SEEK(PADR(ALLTRIM(lcStyle),19)+ INVLINE.INVOICE ,loFormSet.lcStyYTD)
        m.Invoice = INVLINE.INVOICE
        m.Style = ALLTRIM(lcStyle)
        m.Account = INVHDR.Account
        m.SHPDATe = INVHDR.Shipdate
        =gfSeek(IIF(!EMPTY(INVHDR.STORE),'S','M')+INVHDR.Account+INVHDR.STORE,'Customer')
        m.Name = Customer.btname
        m.Status = INVHDR.STATUS
        m.Order = INVHDR.ORDER
        m.QTy = INVLINE.TOTQTY
        m.Amount = iNVLINE.TOTQTY*INVLINE.Price
        INSERT INTO (loFormSet.lcStyYTD) FROM MEMVAR
      ELSE
        REPLACE  QTY WITH QTy + INVLINE.TOTQTY ,;
          Amount WITH Amount + iNVLINE.TOTQTY*INVLINE.Price IN (loFormSet.lcStyYTD)
      ENDIF
    ENDSCAN

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    m.TOTQTY = m.TOTQTY + VAL(m.TQty)
  ENDSCAN

  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.Style = lcStyle
  m.Ctype = 'I'
  m.cTitle = "LY Sales"
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyle))
  =gfSeek(lcStyle)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  m.TOTQTY = 0

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  SCAN REST WHILE Style = ALLTRIM(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting Invoices For Style:"+STYLE.STYLE NOWAIT
    =gfSeek("S"+STYLE.SCALE,'Scale')
    m.Scale = STYLE.SCALE
    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)
    SELECT INVLINE
    =gfSeek(STYLE.STYLE)
    *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[Start]
    *SCAN REST WHILE  STYLE+INVOICE+STR(LINENO,6) = Style.Style FOR ;
    BETWEEN(Invline.INVDATE,DATE(YEAR(loFormSet.ldDateStart)-1,1,1),DATE(YEAR(loFormSet.ldDateStart)-1,MONTH(loFormSet.ldDateStart),DAY(loFormSet.ldDateStart)));
    AND gfSeek(Invline.Invoice,'Invhdr') AND Invhdr.Status <> 'V'

    SCAN REST WHILE  STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE FOR ;
        BETWEEN(Invline.INVDATE,DATE(YEAR(loFormSet.ldDateStart)-1,MONTH(loFormSet.ldDateStart),DAY(loFormSet.ldDateStart)),DATE(YEAR(loFormSet.ldDateEnd)-1,MONTH(loFormSet.ldDateEnd),DAY(loFormSet.ldDateEnd)));
        AND gfSeek(Invline.Invoice,'Invhdr') AND Invhdr.STATUS <> 'V'
      *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[End]
      FOR lnI = 1  TO SCALE.CNT
        lcI = ALLTRIM(STR(lnI))
        m.Qty&lcI. = STR(VAL(m.Qty&lcI.) + INVLINE.QTY&lcI.,7)

        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
        *m.TQty = STR(VAL(m.TQty) +  VAL(m.Qty&lcI.),9)
        *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

      ENDFOR
      IF !SEEK(PADR(ALLTRIM(lcStyle),19) + INVLINE.INVOICE ,loFormSet.lcStyLYS)
        m.Invoice = INVLINE.INVOICE
        m.Style = ALLTRIM(lcStyle)
        m.Account = INVHDR.Account
        m.SHPDATe = INVHDR.Shipdate
        =gfSeek(IIF(!EMPTY(INVHDR.STORE),'S','M')+INVHDR.Account+INVHDR.STORE,'Customer')
        m.Name = Customer.btname
        m.Status = INVHDR.STATUS
        m.Order = INVHDR.ORDER
        m.QTy = INVLINE.TOTQTY
        m.Amount = iNVLINE.TOTQTY*INVLINE.Price
        INSERT INTO (loFormSet.lcStyLYS) FROM MEMVAR
      ELSE
        REPLACE  QTY WITH QTy + INVLINE.TOTQTY ,;
          Amount WITH Amount + iNVLINE.TOTQTY*INVLINE.Price IN (loFormSet.lcStyLYS)
      ENDIF
    ENDSCAN

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    m.TOTQTY = m.TOTQTY + VAL(m.TQty)
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
  ENDSCAN

  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.TOTQTY = 0
  m.TQty = STR(0,1)
  m.Style = lcStyle
  m.Ctype = 'J'
  m.cTitle = "Open To Buy"
  SELECT STYLE

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyle))
  *SCAN REST WHILE Style = ALLTRIM(lcStyle)
  =gfSeek(lcStyle)
  SCAN REST WHILE STYLE = lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    =gfSeek("S"+STYLE.SCALE,'Scale')
    m.Scale = STYLE.SCALE
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(0,1)
      m.TQty = STR(0,1)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
  ENDSCAN

  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


ELSE
  =SEEK(EVALUATE(loFormSet.lcFormStyles+'.cFrcstID')+EVALUATE(loFormSet.lcFormStyles+'.Style'),loFormSet.lcFRCSTDTL)
  lcFrcst = EVALUATE(loFormSet.lcFormStyles+'.cFrcstID')
  =SEEK(lcFrcst,loFormSet.lcFRCSTHDR)

  loFormSet.ldDateStart = EVALUATE(loFormSet.lcFRCSTHDR+'.DSTARTDATE')
  loFormSet.ldDateEnd = EVALUATE(loFormSet.lcFRCSTHDR+'.DENDDATE')

  FOR lnJ = 1 TO 8
    lcJ = ALLTRIM(STR(lnJ))
    STORE 0 TO m.OTS&lcJ.
  ENDFOR



  FOR lnI = 1 TO  8
    lcI = ALLTRIM(STR(lnI))
    STORE STR(0,1) TO  m.Qty&lcI.
  ENDFOR
  STORE STR(0,1) TO m.TQty

  SELECT (loFormSet.lcFRCSTDTL)
  IF SEEK(lcFrcst+PADR(ALLTRIM(lcStyle),19),loFormSet.lcStyleInv)
    RETURN
  ENDIF

  REPLACE TotInv WITH  0,;
    TotWip WITH  0,;
    TotOrd WITH  0 ,;
    TotOts WITH  0,;
    Currpln WITH 0 IN (loFormSet.lcFormStyles)

  m.Style = lcStyle
  =gfSeek(ALLTRIM(lcStyle),'Style','Style')
  m.NCost = STYLE.ave_cost
  m.NPrice  = STYLE.pricea
  m.CFORECSTID = lcFrcst
  m.cFrcstID= lcFrcst

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  * =gfSeek(lcFrcst+ALLTRIM(lcStyle),loFormSet.lcFRCSTDTL)
  * SCAN REST WHILE CFORECSTId+Style = lcFrcst+ALLTRIM(lcStyle)
  =gfSeek(lcFrcst+lcStyle,loFormSet.lcFRCSTDTL)
  SCAN REST WHILE CFORECSTId+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    m.Ctype = 'A'
    m.cTitle = "Qty/Size"
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      m.Qty&lcI. = ""
    ENDFOR

    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = SCALE.Sz&lcI.
    ENDFOR
    m.TQty ='Total'
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR

    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)

    m.Ctype = 'B'
    m.cTitle = "Inventory"


    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(nStk&lcI.,7)
      m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
    ENDFOR
    REPLACE TotInv WITH  TotInv+ VAL(m.TQty) IN  (loFormSet.lcFormStyles)
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
  ENDSCAN

  m.TOTQTY = EVALUATE(loFormSet.lcFormStyles+'.TotInv')
  m.Style = ALLTRIM(lcStyle)
  IF !SEEK(m.Style,loFormSet.lcStyScl)
    INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
    lfCalTotCost(loFormSet)
    lfCaltotPric(loFormSet)

  ENDIF




  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  =gfSeek(lcFrcst+ALLTRIM(lcStyle),loFormSet.lcFRCSTDTL)
  =gfSeek(lcFrcst+lcStyle,loFormSet.lcFRCSTDTL)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  lnCnter = 1
  m.Style = lcStyle
  m.Ctype = 'C'
  m.cTitle = "WIP"
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting POs For Style:"+STYLE NOWAIT
    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty =STR(0,1)
    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    =gfSeek('0001'+STYLE.STYLE+'P','POSLN')

    SELECT POSlN
    *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
    *SCAN REST WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+ STR(lineno,5) +trancd = '0001'+Style.Style+'P' FOR trancd = '1'
            DO WHILE cinvtype+ STYLE+ cbusdocu+ cstytype+ po+ STR(LINENO,5) +trancd = '0001'+STYLE.STYLE+'P' FOR trancd <> '3'
      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

      = gfSeek(POSLN.cbusdocu+ POSLN.cstytype+ POSLN.po,'POSHDR')
      IF !(POSHDR.STATUS $ 'OH')
        SELECT POSlN        
        IF !EOF()
          SKIP 1
        ENDIF 
        LOOP
      ENDIF

      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]

      && Inilitize new variables.
      m.PoLineQTY1 = 0
      m.PoLineQTY2 = 0
      m.PoLineQTY3 = 0
      m.PoLineQTY4 = 0
      m.PoLineQTY5 = 0
      m.PoLineQTY6 = 0
      m.PoLineQTY7 = 0
      m.PoLineQTY8 = 0
      m.PoLineTotQTY = 0
      ln_niCost1     = 0
      lcCurrentLineno   = POSlN.LINENO
      lcCurrentcstytype = POSLn.cstytype

      && Scan and Accumulate on PO + line.
      SCAN REST WHILE cinvtype+ STYLE+ cbusdocu+ cstytype+ po+ STR(LINENO,5)+ trancd = '0001'+ STYLE.STYLE+ 'P'+ lcCurrentcstytype+ POSHDR.PO+ STR(lcCurrentLineno,5) FOR trancd <> '3'
        FOR lnI = 1 TO  SCALE.CNT
          lcI = ALLTRIM(STR(lnI))
          m.PoLineQTY&lcI. = m.PoLineQTY&lcI. + IIF(trancd ='1',POsLN.QTY&lcI.,(-1 * POsLN.QTY&lcI.))
        ENDFOR
        IF trancd = '1'
          ln_niCost1 = Posln.niCost1
        ENDIF
      ENDSCAN

      && Get the Max. with 0, to avoid -ve values.
      FOR lnI = 1 TO  SCALE.CNT
       lcI = ALLTRIM(STR(lnI))
        m.PoLineQTY&lcI. = MAX(0,m.PoLineQTY&lcI.)
        m.PoLineTotQTY  = m.PoLineTotQTY  + m.PoLineQTY&lcI.
      ENDFOR
      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][BEgin]
      *IF !SEEK(PADR(ALLTRIM(lcStyle),19)+POSLN.Cstytype+ POSLN.PO,loFormSet.lcStyWip)
      IF !SEEK(PADR(ALLTRIM(lcStyle),19)+lcCurrentcstytype+ POSHdr.PO,loFormSet.lcStyWip)
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]
        m.TTYPE    = POSHDR.cStytype
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
        *m.po       = POSLN.PO
        m.po       = POSHdr.PO
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

        m.Entered  = POSHDR.Entered
        m.Complete = POSHDR.COMPLETE
        m.Status   = POSHDR.STATUS
        m.Vendor   = IIF(m.TTYPE = 'P',POSHDR.Vendor,POSHDR.cwarecode)
        m.Title = IIF(m.TTYPE = 'P','Purchase Order','Cut Ticket' )
        m.Style = ALLTRIM(lcStyle)
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
        *m.Qty = POsln.TOTQTY
        *m.Amount = POsln.TOTQTY * POsln.niCost1
        m.Qty = m.PoLineTotQTY
        m.Amount = m.PoLineTotQTY * ln_niCost1
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]
        INSERT INTO (loFormSet.lcStyWip) FROM MEMVAR
      ELSE
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
        *REPLACE Qty WITH Qty+POsln.TOTQTY,;
        *   	    Amount	WITH Amount+ POsln.TOTQTY * POsln.niCost1 IN (loFormSet.lcStyWip)
        REPLACE Qty WITH Qty + m.PoLineTotQTY,;
          Amount	WITH Amount + m.PoLineTotQTY * ln_niCost1 IN (loFormSet.lcStyWip)
        *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]
      ENDIF
      *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
      *EndScan
    ENDDO
    *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]
    SELECT (loFormSet.lcFRCSTDTL)
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(VAL(m.Qty&lcI.)+wip&lcI.,7)

      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
      *m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    ENDFOR

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    REPLACE TotWip WITH  TotWip+ VAL(m.TQty) IN  (loFormSet.lcFormStyles)
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR

  ENDSCAN

  m.Style = ALLTRIM(lcStyle)
  m.TOTQTY = EVALUATE(loFormSet.lcFormStyles+'.TotWip')
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *   =gfSeek(lcFrcst+ALLTRIM(lcStyle))
  =gfSeek(lcFrcst+lcStyle)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  lnCnter = 1
  m.Style = lcStyle
  m.Ctype = 'D'
  m.cTitle = "Orders"
  m.TQty = STR(0,1)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting Orders For Style:"+STYLE NOWAIT
    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)
    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    =gfSeek(STYLE.STYLE,'Ordline')
    SELECT Ordline
    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) =STYLE.STYLE FOR ;
        cordtype = 'O' AND gfSeek('O'+Ordline.ORDER,'Ordhdr') AND ordHDR.STATUS $ 'OH'
      IF !SEEK(PADR(ALLTRIM(lcStyle),19)+ ordline.ORDER,loFormSet.lcStyOrd)
        m.Order = Ordline.ORDER
        m.Status   = Ordhdr.STATUS
        m.Style = ALLTRIM(lcStyle)
        m.Qty = Ordline.TOTQTY
        m.Amount = Ordline.TOTQTY * Ordline.Price
        m.Account = Ordline.account
        m.Store = Ordhdr.STORE
        =gfSeek(IIF(!EMPTY(Ordhdr.STORE),'S','M')+Ordhdr.Account+Ordhdr.STORE,'Customer')
        m.Name = Customer.btname
        INSERT INTO (loFormSet.lcStyOrd) FROM MEMVAR
      ELSE
        REPLACE Qty WITH Qty+ordline.TOTQTY,;
          Amount	WITH Amount+ ordline.TOTQTY * ordline.Price IN (loFormSet.lcStyOrd)
      ENDIF
    ENDSCAN
    SELECT (loFormSet.lcFRCSTDTL)
    FOR lnI = 1 TO SCALE.CNT
      lcI = ALLTRIM(STR(lnI))
      m.Qty&lcI. = STR(VAL(m.Qty&lcI.) + ord&lcI.,7)

      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
      *m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    ENDFOR

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    REPLACE TotOrd WITH  TotOrd + VAL(m.TQty) IN  (loFormSet.lcFormStyles)
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
  ENDSCAN


  m.Style = ALLTRIM(lcStyle)
  m.TOTQTY = EVALUATE(loFormSet.lcFormStyles+'.TotOrd')
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.TOTQTY = 0
  m.Style = lcStyle
  m.Ctype = 'E'
  m.cTitle = "OTS"
  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(lcFrcst+ALLTRIM(lcStyle))
  *SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  =gfSeek(lcFrcst+lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR

    m.TQty = STR(0,1)

    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(0,1)
      m.TQty = STR(0,1)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    m.TOTQTY = m.TOTQTY + VAL(m.Tqty)
  ENDSCAN

  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)



  m.TOTQTY =   0
  m.Style = lcStyle
  m.Ctype = 'F'
  m.cTitle = "Sales Forecast"
  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *!*    =gfSeek(lcFrcst+ALLTRIM(lcStyle))
  *!*    SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  =gfSeek(lcFrcst+lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)

    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(NFRCSTQTY&lcI.,7)
      m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    m.TOTQTY = m.TOTQTY + VAL(m.TQTY )
  ENDSCAN
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.TOTQTY = 0
  m.Style = lcStyle
  m.Ctype = 'G'
  m.cTitle = "Net OTS"
  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *!*    =gfSeek(lcFrcst+ALLTRIM(lcStyle))
  *!*    SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  =gfSeek(lcFrcst+lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)
    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(0,1)
      m.TQty = STR(0,1)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    m.TOTQTY = m.TOTQTY +  VAL(m.Tqty)
  ENDSCAN
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.TOTQTY = 0
  m.Style = lcStyle
  m.Ctype = 'H'
  m.cTitle = "YTD Sales"
  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *!*    =gfSeek(lcFrcst+ALLTRIM(lcStyle))
  *!*    SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  =gfSeek(lcFrcst+lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting Invoices For Style:"+STYLE NOWAIT
    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)

    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    SELECT INVLINE
    =gfSeek(STYLE.STYLE)
    SCAN REST WHILE  STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE FOR ;
        BETWEEN(Invline.INVDATE,DATE(YEAR(loFormSet.ldDateStart ),1,1),loFormSet.ldDateStart) AND gfSeek(Invline.Invoice,'Invhdr') AND Invhdr.STATUS <> 'V'

      IF !SEEK(PADR(ALLTRIM(lcStyle),19)+ INVLINE.INVOICE ,loFormSet.lcStyYTD)
        m.Invoice = INVLINE.INVOICE
        m.Style = ALLTRIM(lcStyle)
        m.Account = INVHDR.Account
        m.SHPDATe = INVHDR.Shipdate
        =gfSeek(IIF(!EMPTY(INVHDR.STORE),'S','M')+INVHDR.Account+INVHDR.STORE,'Customer')
        m.Name = Customer.btname
        m.Status = INVHDR.STATUS
        m.Order = INVHDR.ORDER
        m.QTy = INVLINE.TOTQTY
        m.Amount = iNVLINE.TOTQTY*INVLINE.Price
        INSERT INTO (loFormSet.lcStyYTD) FROM MEMVAR
      ELSE
        REPLACE  QTY WITH QTy + INVLINE.TOTQTY ,;
          Amount WITH Amount + iNVLINE.TOTQTY*INVLINE.Price IN (loFormSet.lcStyYTD)

      ENDIF
    ENDSCAN
    SELECT (loFormSet.lcFRCSTDTL)
    FOR lnI = 1 TO  SCALE.CNT
      lcI = ALLTRIM(STR(lnI))
      m.Qty&lcI. = STR(VAL(m.Qty&lcI.) + NYTDSLS&lcI.,7)

      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
      *m.TQty = STR(VAL(m.TQty) +  VAL(m.Qty&lcI.),9)
      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    ENDFOR

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    m.TOTQTY = m.TOTQTY + VAL(m.Tqty)
  ENDSCAN
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.Style = lcStyle
  m.Ctype = 'I'
  m.cTitle = "LY Sales"
  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  =gfSeek(lcFrcst+ALLTRIM(lcStyle))
  =gfSeek(lcFrcst+lcStyle)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  m.TOTQTY = 0

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    WAIT WINDOW "Collecting Invoices For Style:"+STYLE NOWAIT
    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)

    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    SELECT INVLINE
    =gfSeek(STYLE.STYLE)
    *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[Start]
    *SCAN REST WHILE  STYLE+INVOICE+STR(LINENO,6) = Style.Style FOR ;
    BETWEEN(Invline.INVDATE,DATE(YEAR(loFormSet.ldDateStart)-1,1,1),DATE(YEAR(loFormSet.ldDateStart)-1,MONTH(loFormSet.ldDateStart),DAY(loFormSet.ldDateStart)));
    AND gfSeek(Invline.Invoice,'Invhdr') AND Invhdr.Status <> 'V'
    SCAN REST WHILE  STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE FOR ;
        BETWEEN(Invline.INVDATE,DATE(YEAR(loFormSet.ldDateStart)-1,MONTH(loFormSet.ldDateStart),DAY(loFormSet.ldDateStart)),DATE(YEAR(loFormSet.ldDateEnd)-1,MONTH(loFormSet.ldDateEnd),DAY(loFormSet.ldDateEnd)));
        AND gfSeek(Invline.Invoice,'Invhdr') AND Invhdr.STATUS <> 'V'
      *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[End]

      IF !SEEK(PADR(ALLTRIM(lcStyle),19) + INVLINE.INVOICE ,loFormSet.lcStyLYS)
        m.Invoice = INVLINE.INVOICE
        m.Style = ALLTRIM(lcStyle)
        m.Account = INVHDR.Account
        m.SHPDATe = INVHDR.Shipdate
        =gfSeek(IIF(!EMPTY(INVHDR.STORE),'S','M')+INVHDR.Account+INVHDR.STORE,'Customer')
        m.Name = Customer.btname
        m.Status = INVHDR.STATUS
        m.Order = INVHDR.ORDER
        m.QTy = INVLINE.TOTQTY
        m.Amount = iNVLINE.TOTQTY*INVLINE.Price
        INSERT INTO (loFormSet.lcStyLYS) FROM MEMVAR
      ELSE
        REPLACE  QTY WITH QTy + INVLINE.TOTQTY ,;
          Amount WITH Amount + iNVLINE.TOTQTY*INVLINE.Price IN (loFormSet.lcStyLYS)
      ENDIF
    ENDSCAN
    SELECT (loFormSet.lcFRCSTDTL)
    FOR lnI = 1  TO SCALE.CNT
      lcI = ALLTRIM(STR(lnI))
      m.Qty&lcI. = STR(VAL(m.Qty&lcI.) + NLYSLS&lcI.,7)

      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
      *m.TQty = STR(VAL(m.TQty) +  VAL(m.Qty&lcI.),9)
      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    ENDFOR

    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    m.TQty = STR(VAL(m.Qty1)+VAL(m.Qty2)+VAL(m.Qty3)+VAL(m.Qty4)+VAL(m.Qty5)+VAL(m.Qty6)+VAL(m.Qty7)+VAL(m.Qty8),9)
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    m.TOTQTY = m.TOTQTY + VAL(m.Tqty)
  ENDSCAN
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)


  m.TOTQTY = 0
  m.TQty = STR(0,1)
  m.Style = lcStyle
  m.Ctype = 'J'
  m.cTitle = "Open To Buy"
  SELECT (loFormSet.lcFRCSTDTL)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *!*    =gfSeek(lcFrcst+ALLTRIM(lcStyle))
  *!*    SCAN REST WHILE CFORECSTID+Style = lcFrcst+ALLTRIM(lcStyle)
  =gfSeek(lcFrcst+lcStyle)
  SCAN REST WHILE CFORECSTID+STYLE = lcFrcst+lcStyle
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    FOR lnJ = 1 TO 8
      lcJ = ALLTRIM(STR(lnJ))
      STORE STR(0,1) TO m.qty&lcJ.
    ENDFOR
    m.TQty = STR(0,1)
    =gfSeek(STYLE,'Style','Style')
    m.Scale = STYLE.SCALE
    =gfSeek("S"+STYLE.SCALE,'Scale')
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      m.Qty&lcI. = STR(NOTBQTY&lcI.,7)
      m.TQty = STR(VAL(m.TQty) + VAL(m.Qty&lcI.),9)
    ENDFOR
    INSERT INTO (loFormSet.lcStyleInv) FROM MEMVAR
    m.TOTQTY = m.TOTQTY + VAL(m.Tqty)
  ENDSCAN
  m.Style = ALLTRIM(lcStyle)
  INSERT INTO (loFormSet.lcStyScl)  FROM MEMVAR
  lfCalTotCost(loFormSet)
  lfCaltotPric(loFormSet)

ENDIF
lfCalcOts(loFormSet)


*!*************************************************************
*! Name      : lfCrtStylFiles
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Create style temp. files
*!*************************************************************
FUNCTION lfCrtStylFiles
PARAMETERS loFormSet
DIMENSION laCursStru[8,4]

laCursStru[1,1] = 'Style'
laCursStru[1,2] = 'C'
laCursStru[1,3] = 19
laCursStru[1,4] = 0


laCursStru[2,1] = 'cTitle'
laCursStru[2,2] = 'C'
laCursStru[2,3] = 30
laCursStru[2,4] = 0


laCursStru[3,1] = 'TOTQTY'
laCursStru[3,2] = 'N'
laCursStru[3,3] = 9
laCursStru[3,4] = 0


laCursStru[4,1] = 'Ncost'
laCursStru[4,2] = 'N'
laCursStru[4,3] = 9
laCursStru[4,4] = 2

laCursStru[5,1] = 'Nprice'
laCursStru[5,2] = 'N'
laCursStru[5,3] = 9
laCursStru[5,4] = 2

laCursStru[6,1] = 'Ctype'
laCursStru[6,2] = 'C'
laCursStru[6,3] = 1
laCursStru[6,4] = 0


laCursStru[7,1] = 'TOTCOST'
laCursStru[7,2] = 'N'
laCursStru[7,3] = 9
laCursStru[7,4] = 2


laCursStru[8,1] = 'TOTSELL'
laCursStru[8,2] = 'N'
laCursStru[8,3] = 9
laCursStru[8,4] = 2


IF loFormSet.ActiveMode $ 'EVS'
  DIMENSION laCursStru[9 ,4]
  laCursStru[9,1] = 'cFrcstID'
  laCursStru[9,2] = 'C'
  laCursStru[9,3] = 6
  laCursStru[9,4] = 0
  =gfCrtTmp(loFormSet.lcStyScl ,@laCursStru,"cFrcstID+Style+cTYPE" ,loFormSet.lcStyScl ,.F.)
ELSE
  =gfCrtTmp(loFormSet.lcStyScl ,@laCursStru,"Style+cTYPE" ,loFormSet.lcStyScl ,.F.)
ENDIF





DIMENSION laInvStru[9,4]

laInvStru[1,1] = 'Style'
laInvStru[1,2] = 'C'
laInvStru[1,3] = 19
laInvStru[1,4] = 0

laInvStru[2,1] = 'Invoice'
laInvStru[2,2] = 'C'
laInvStru[2,3] = 6
laInvStru[2,4] = 0

laInvStru[3,1] = 'Status'
laInvStru[3,2] = 'C'
laInvStru[3,3] = 1
laInvStru[3,4] = 0

laInvStru[4,1] = 'Account'
laInvStru[4,2] = 'C'
laInvStru[4,3] = 6
laInvStru[4,4] = 0

laInvStru[5,1] = 'NAme'
laInvStru[5,2] = 'C'
laInvStru[5,3] = 30
laInvStru[5,4] = 0

laInvStru[6,1] = 'Order'
laInvStru[6,2] = 'C'
laInvStru[6,3] = 6
laInvStru[6,4] = 0

laInvStru[7,1] = 'ShpDate'
laInvStru[7,2] = 'D'
laInvStru[7,3] = 8
laInvStru[7,4] = 0

laInvStru[8,1] = 'Qty'
laInvStru[8,2] = 'N'
laInvStru[8,3] = 7
laInvStru[8,4] = 0

laInvStru[9,1] = 'Amount'
laInvStru[9,2] = 'N'
laInvStru[9,3] = 9
laInvStru[9,4] = 2

=gfCrtTmp(loFormSet.lcStyYTD,@laInvStru,"Style+Invoice" ,loFormSet.lcStyYTD,.T.)
=gfCrtTmp(loFormSet.lcStyLYS,@laInvStru,"Style+Invoice" ,loFormSet.lcStyLYS,.T.)

DIMENSION laCursStru[15 ,4]

laCursStru[1,1] = 'Style'
laCursStru[1,2] = 'C'
laCursStru[1,3] = 19
laCursStru[1,4] = 0

laCursStru[2,1] = 'cType'
laCursStru[2,2] = 'C'
laCursStru[2,3] = 1
laCursStru[2,4] = 0

laCursStru[3,1] = 'cTitle'
laCursStru[3,2] = 'C'
laCursStru[3,3] = 30
laCursStru[3,4] = 0

laCursStru[4,1] = 'TQty'
laCursStru[4,2] = 'C'
laCursStru[4,3] = 9
laCursStru[4,4] = 0

laCursStru[5,1] = 'NCost'
laCursStru[5,2] = 'N'
laCursStru[5,3] = 9
laCursStru[5,4] = 2

laCursStru[6,1] = 'NPrice'
laCursStru[6,2] = 'N'
laCursStru[6,3] = 9
laCursStru[6,4] = 2

laCursStru[7,1] = 'Scale'
laCursStru[7,2] = 'C'
laCursStru[7,3] = 3
laCursStru[7,4] = 0


lnI = 8
FOR lnCnt = 1 TO 8
  laCursStru[lnI ,1] = 'Qty'+ALLTRIM(STR(lnCnt))
  laCursStru[lnI ,2] = 'C'
  laCursStru[lnI ,3] = 7
  laCursStru[lnI ,4] = 0
  lnI = lnI + 1
ENDFOR


IF loFormSet.ActiveMode $ 'EVS'
  DIMENSION laCursStru[lnI ,4]
  laCursStru[lnI ,1] = 'cFrcstID'
  laCursStru[lnI ,2] = 'C'
  laCursStru[lnI ,3] = 6
  laCursStru[lnI ,4] = 0
  =gfCrtTmp(loFormSet.lcStyleInv,@laCursStru,"cFrcstID+Style+Scale+cTYPE" ,loFormSet.lcStyleInv,.F.)
ELSE
  =gfCrtTmp(loFormSet.lcStyleInv,@laCursStru,"Style+Scale+cTYPE" ,loFormSet.lcStyleInv,.F.)
ENDIF



DIMENSION laCursStru[10 ,4]
laCursStru[1 ,1]  = 'Style'
laCursStru[1 ,2]  = 'C'
laCursStru[1 ,3]  = 19
laCursStru[1 ,4]  = 0

laCursStru[2 ,1]  = 'TType'
laCursStru[2 ,2]  = 'C'
laCursStru[2 ,3]  = 1
laCursStru[2 ,4]  = 0

laCursStru[3 ,1]  = 'Title'
laCursStru[3 ,2]  = 'C'
laCursStru[3 ,3]  = 30
laCursStru[3 ,4]  = 0

laCursStru[4 ,1]  = 'PO'
laCursStru[4 ,2]  = 'C'
laCursStru[4 ,3]  = 6
laCursStru[4 ,4]  = 0

laCursStru[5 ,1]  = 'Status'
laCursStru[5 ,2]  = 'C'
laCursStru[5 ,3]  = 1
laCursStru[5 ,4]  = 0

laCursStru[6 ,1]  = 'Vendor'
laCursStru[6 ,2]  = 'C'
laCursStru[6 ,3]  = 10
laCursStru[6 ,4]  = 0

laCursStru[7 ,1]  = 'Entered'
laCursStru[7 ,2]  = 'D'
laCursStru[7 ,3]  = 8
laCursStru[7 ,4]  = 0

laCursStru[8 ,1]  = 'Complete'
laCursStru[8 ,2]  = 'D'
laCursStru[8 ,3]  = 8
laCursStru[8 ,4]  = 0

laCursStru[9 ,1]  = 'Qty'
laCursStru[9 ,2]  = 'N'
laCursStru[9 ,3]  = 7
laCursStru[9 ,4]  = 0

laCursStru[10 ,1]  = 'Amount'
laCursStru[10 ,2]  = 'N'
laCursStru[10 ,3]  = 9
laCursStru[10 ,4]  = 2

=gfCrtTmp(loFormSet.lcStyWip,@laCursStru,"Style+TType+PO" ,loFormSet.lcStyWip,.T.)

DIMENSION laCursStru[8 ,4]
laCursStru[1 ,1]  = 'Style'
laCursStru[1 ,2]  = 'C'
laCursStru[1 ,3]  = 19
laCursStru[1 ,4]  = 0

laCursStru[2 ,1]  = 'Order'
laCursStru[2 ,2]  = 'C'
laCursStru[2 ,3]  = 6
laCursStru[2 ,4]  = 0

laCursStru[3 ,1]  = 'Account'
laCursStru[3 ,2]  = 'C'
laCursStru[3 ,3]  = 5
laCursStru[3 ,4]  = 0

laCursStru[4 ,1]  = 'Name'
laCursStru[4 ,2]  = 'C'
laCursStru[4 ,3]  = 30
laCursStru[4 ,4]  = 0

laCursStru[5 ,1]  = 'Store'
laCursStru[5 ,2]  = 'C'
laCursStru[5 ,3]  = 8
laCursStru[5 ,4]  = 0

laCursStru[6 ,1]  = 'Qty'
laCursStru[6 ,2]  = 'N'
laCursStru[6 ,3]  = 7
laCursStru[6 ,4]  = 0

laCursStru[7 ,1]  = 'Amount'
laCursStru[7 ,2]  = 'N'
laCursStru[7 ,3]  = 9
laCursStru[7 ,4]  = 2

laCursStru[8 ,1]  = 'Status'
laCursStru[8 ,2]  = 'C'
laCursStru[8 ,3]  = 1
laCursStru[8 ,4]  = 0

=gfCrtTmp(loFormSet.lcStyOrd ,@laCursStru,"Style+Order" ,loFormSet.lcStyOrd ,.T.)


SELECT(loFormSet.lcFormStyles)

*!*************************************************************
*! Name      : lfDetGrdCntSrc
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : details grid control source
*!*************************************************************
FUNCTION lfDetGrdCntSrc
PARAMETERS loFormSet,llFromTree

WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet
  .RECORDSOURCE = ''
  .COLUMNCOUNT = 0
  .COLUMNCOUNT = EVALUATE(loFormSet.lcFormStyles +'.nSclCnt')+2
  .RECORDSOURCE = loFormSet.lcStyleInv
  .Column1.CONTROLSOURCE = loFormSet.lcStyleInv+ '.cTitle'
  .Column1.WIDTH = 146
  .Column1.Header1.CAPTION = ''
  .Column1.READONLY = .T.
  .Column1.FONTBOLD = .T.
  .Column1.ALIGNMENT = 2
  .Column1.DYNAMICBACKCOLOR="IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) = 'A',RGB(128,128,128),RGB(255,255,255))"
  .Column1.Header1.ALIGNMENT = 2
  .COLUMNS(1).DYNAMICFORECOLOR ="IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) $ 'FJ',RGB(64,128,128),IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) = 'G',RGB(255,0,0),RGB(0,0,0)))"
  .COLUMNS(1).SPARSE = .F.
  lnU = 2

  FOR lnD = 1 TO EVALUATE(loFormSet.lcFormStyles +'.nSclCnt')
    .COLUMNS(lnU).ALIGNMENT = 2
    .COLUMNS(lnU).CONTROLSOURCE = loFormSet.lcStyleInv+ '.QTY'+ALLTRIM(STR(lnD))
    .COLUMNS(lnU).Header1.CAPTION = ""
    .COLUMNS(lnU).WIDTH =  75
    .COLUMNS(lnU).Text1.INPUTMASK = '9999999'
    .COLUMNS(lnU).Text1.FORMAT = '9999999'
    .COLUMNS(lnU).Text1.ALIGNMENT = 2

    IF TYPE('loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.Columns(lnU).Text2') <> 'O'
      .COLUMNS(lnU).ADDOBJECT('Text2','AriaTextBox')
      .COLUMNS(lnU).Text2.ENABLED = .F.
      .COLUMNS(lnU).Text2.alwaysdisable = .T.
      .COLUMNS(lnU).Text2.DISABLEDFORECOLOR = RGB(0,0,0)
      .COLUMNS(lnU).Text2.VISIBLE = .T.
      .COLUMNS(lnU).Text2.BORDERSTYLE = 0
      .COLUMNS(lnU).Text2.ALIGNMENT = 2
      .COLUMNS(lnU).Text2.INPUTMASK = '9999999'
      .COLUMNS(lnU).Text2.FORMAT = '9999999'
    ENDIF

    IF TYPE('loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.Columns(lnU).Text3') <> 'O'
      .COLUMNS(lnU).ADDOBJECT('Text3','AriaTextBox')
      .COLUMNS(lnU).Text3.ENABLED = .F.
      .COLUMNS(lnU).Text3.alwaysdisable = .T.
      .COLUMNS(lnU).Text3.DISABLEDBACKCOLOR = RGB(128,128,128)
      .COLUMNS(lnU).Text3.DISABLEDFORECOLOR = RGB(0,0,0)
      .COLUMNS(lnU).Text3.VISIBLE = .T.
      .COLUMNS(lnU).Text3.FONTBOLD = .T.
      .COLUMNS(lnU).Text3.BORDERSTYLE = 0
      .COLUMNS(lnU).Text3.ALIGNMENT = 2
    ENDIF

    .COLUMNS(lnU).DYNAMICCURRENTCONTROL ="IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) $ 'FJ','Text1',IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) ='A','Text3','Text2'))"
    .COLUMNS(lnU).DYNAMICBACKCOLOR="IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) = 'A',RGB(128,128,128),RGB(255,255,255))"
    .COLUMNS(lnU).SPARSE = .F.
    BINDEVENT(.COLUMNS(lnU).Text1,'GotFocus',loFormSet,'lfChkFld')
    BINDEVENT(.COLUMNS(lnU).Text1,'LostFocus',loFormSet,'lfUpdNoTS')
    lnU = lnU + 1
  ENDFOR

  IF TYPE('loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.Columns(lnU).Text3') <> 'O'
    .COLUMNS(lnU).ADDOBJECT('Text3','AriaTextBox')
    .COLUMNS(lnU).Text3.ENABLED = .F.
    .COLUMNS(lnU).Text3.alwaysdisable = .T.
    .COLUMNS(lnU).Text3.DISABLEDBACKCOLOR = RGB(128,128,128)
    .COLUMNS(lnU).Text3.DISABLEDFORECOLOR = RGB(0,0,0)
    .COLUMNS(lnU).Text3.VISIBLE = .T.
    .COLUMNS(lnU).Text3.FONTBOLD = .T.
    .COLUMNS(lnU).Text3.BORDERSTYLE = 0
    .COLUMNS(lnU).Text3.ALIGNMENT = 2
  ENDIF
  IF TYPE('loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.Columns(lnU).Text2') <> 'O'
    .COLUMNS(lnU).ADDOBJECT('Text2','AriaTextBox')
    .COLUMNS(lnU).Text2.ENABLED = .F.
    .COLUMNS(lnU).Text2.alwaysdisable = .T.
    .COLUMNS(lnU).Text2.DISABLEDFORECOLOR = RGB(0,0,0)
    .COLUMNS(lnU).Text2.VISIBLE = .T.
    .COLUMNS(lnU).Text2.BORDERSTYLE = 0
    .COLUMNS(lnU).Text2.ALIGNMENT = 2
  ENDIF

  .COLUMNS(lnU).DYNAMICCURRENTCONTROL ="IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) $ 'FJ','Text1',IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) ='A','Text3','Text2'))"
  .COLUMNS(lnU).DYNAMICBACKCOLOR="IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyleInv+'.cType'))) = 'A',RGB(128,128,128),RGB(255,255,255))"
  .COLUMNS(lnU).SPARSE = .F.
  .COLUMNS(lnU).ALIGNMENT = 2
  .COLUMNS(lnU).CONTROLSOURCE = loFormSet.lcStyleInv+ '.TQTY'
  .COLUMNS(lnU).Header1.CAPTION = " "
  .COLUMNS(lnU).WIDTH =  75
  .COLUMNS(lnU).READONLY = .T.

  IF !(loFormSet.ActiveMode $ 'EA')
    .READONLY = .T.
  ENDIF
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
  TRY
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.GRdStyDetToolBar.LLprefrestored = .F.
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.GRdStyDetToolBar.RefreshGrids = .T.
  CATCH
  ENDTRY
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

  .REFRESH
ENDWITH

TRY
  IF !llFromTree
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.Nodes.CLEAR()


    lcKey = EVALUATE(loFormSet.lcFormStyles+'.Style')
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.Nodes.ADD (,1,ALLTRIM(lcKey),lcKey )
    oTempObj = loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.Nodes
    oTempObj.ADD(ALLTRIM(lcKey),4,"FrInf" ,'Forecasting Info.')
    lcKey = "FrInf"
    oTempObj.ADD(ALLTRIM(lcKey),4,"WIP" ,'WIP')
    oTempObj.ADD(ALLTRIM(lcKey),4,"ORD" ,'Orders')
    oTempObj.ADD(ALLTRIM(lcKey),4,"LYS" ,'LYS Sales')
    oTempObj.ADD(ALLTRIM(lcKey),4,"YTD" ,'YTD Sales')

    lcKey = "WIP"
    SELECT (loFormSet.lcStyWip)


    =SEEK(EVALUATE(loFormSet.lcFormStyles+'.Style'))
    SCAN REST WHILE STYLE+TType+PO = EVALUATE(loFormSet.lcFormStyles+'.Style')
      oTempObj.ADD(ALLTRIM(lcKey),4,"W"+PO ,PO)
    ENDSCAN


    lcKey = "ORD"
    SELECT (loFormSet.lcStyORD)
    =SEEK(EVALUATE(loFormSet.lcFormStyles+'.Style'))
    SCAN REST WHILE STYLE+ORDER = EVALUATE(loFormSet.lcFormStyles+'.Style')
      oTempObj.ADD(ALLTRIM(lcKey),4,"O"+ORDER ,ORDER)
    ENDSCAN

    lcKey = "LYS"
    SELECT(loFormSet.lcStyLYS)
    =SEEK(EVALUATE(loFormSet.lcFormStyles+'.Style'))
    SCAN REST WHILE STYLE+ORDER = EVALUATE(loFormSet.lcFormStyles+'.Style')
      oTempObj.ADD(ALLTRIM(lcKey),4,"L"+INVOICE,INVOICE)
    ENDSCAN

    lcKey ='YTD'
    SELECT(loFormSet.lcStyYTD)
    =SEEK(EVALUATE(loFormSet.lcFormStyles+'.Style'))
    SCAN REST WHILE STYLE+ORDER = EVALUATE(loFormSet.lcFormStyles+'.Style')
      oTempObj.ADD(ALLTRIM(lcKey),4,"Y"+INVOICE,INVOICE)
    ENDSCAN
  ENDIF
CATCH
ENDTRY


*!*************************************************************
*! Name      : lfUpdOts
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Update OTS Row
*!*************************************************************
FUNCTION lfUpdOts
PARAMETERS loFormSet

IF !(ALLTRIM(UPPER(EVALUATE(loFormSet.lcStyleInv+'.cType'))) $ 'FJ')
  RETURN
ENDIF
m.TQty = STR(0,1)
lcStyleCurrent = PADR(EVALUATE(loFormSet.lcStyleInv+'.Style'),19)
IF loFormSet.ActiveMode = 'E'
  lcCurFr = EVALUATE(loFormSet.lcStyleInv+'.cFrcstID')
  lcStyleCurrent= lcCurFr + lcStyleCurrent
ENDIF

IF ALLTRIM(UPPER(EVALUATE(loFormSet.lcStyleInv+'.cType'))) = 'F'
  FOR lnT = 1 TO 8
    lcT =ALLTRIM(STR(lnT))
    m.Frcs&lcT. = VAL(QTY&lcT.)
    m.TQty = STR(VAL(m.TQty) +  m.Frcs&lcT.,9)
  ENDFOR
  lnOldQty = VAL(TQty)
  REPLACE TQty WITH m.TQty

  IF SEEK(lcStyleCurrent +'F',loFormSet.lcStyScl)
    REPLACE TOTQTY WITH TOTQTY +(VAL(m.TQty) - lnOldQty ) IN (loFormSet.lcStyScl)
    lfCalTotCost(loFormSet)
    lfCaltotPric(loFormSet)

  ENDIF
ENDIF

IF ALLTRIM(UPPER(EVALUATE(loFormSet.lcStyleInv+'.cType'))) = 'J'
  m.TQty = STR(0,1)
  FOR lnT = 1 TO 8
    lcT =ALLTRIM(STR(lnT))
    m.PLn&lcT. = VAL(QTY&lcT.)
    m.TQty = STR(VAL(m.TQty) +  m.PLn&lcT.,9)
  ENDFOR
  lnOldQty = VAL(TQty)
  REPLACE TQty WITH m.TQty
  REPLACE Currpln WITH Currpln+(VAL(m.TQty) -lnOldQty) IN (loFormSet.lcFormStyles)

  IF SEEK(lcStyleCurrent+'J',loFormSet.lcStyScl)
    REPLACE TOTQTY WITH EVALUATE(loFormSet.lcFormStyles+'.Currpln') IN (loFormSet.lcStyScl)
    lfCalTotCost(loFormSet)
    lfCaltotPric(loFormSet)

  ENDIF
  RETURN
ENDIF


IF ALLTRIM(UPPER(EVALUATE(loFormSet.lcStyleInv+'.cType'))) = 'F'
  lcCurrStyle = PADR(EVALUATE(loFormSet.lcStyleInv+'.Style'),19)+ EVALUATE(loFormSet.lcStyleInv+'.Scale')
  lnCurrRec = RECNO(loFormSet.lcStyleInv)
  lcAlias= SELECT()
  SELECT (loFormSet.lcStyleInv)
  lcCurFr = ''
  IF loFormSet.ActiveMode = 'E'
    lcCurFr = EVALUATE(loFormSet.lcStyleInv+'.cFrcstID')
    lcCurrStyle = lcCurFr + lcCurrStyle
  ENDIF
  IF SEEK(lcCurrStyle+'E')
    m.TQty = STR(0,1)
    FOR lnT = 1 TO 8
      lcT =ALLTRIM(STR(lnT))
      m.OTS&lcT. = VAL(QTY&lcT.)
      m.TQty = STR(VAL(m.TQty) +  m.OTS&lcT.,9)
    ENDFOR
    REPLACE TQty WITH m.TQty
    IF SEEK(lcCurrStyle+'G')
      lnOldTQ = VAL(TQTy )
      REPLACE TQTy WITH STR(0,1)

      FOR lnT = 1 TO 8
        lcT =ALLTRIM(STR(lnT))

        REPLACE QTY&lcT. WITH STR(m.OTS&lcT. - m.Frcs&lcT.,7),;
          TQTy 	 WITH STR(VAL(TQTY) +VAL(QTY&lcT.),9)
      ENDFOR
      IF SEEK(lcStyleCurrent+'G',loFormSet.lcStyScl)
        REPLACE TOTQTY WITH TOTQTY +(VAL(EVALUATE(loFormSet.lcStyleInv+'.TQTy')) - lnOldTQ) IN (loFormSet.lcStyScl)
        lfCalTotCost(loFormSet)
        lfCaltotPric(loFormSet)
      ENDIF
    ENDIF
  ENDIF
  IF BETWEEN(lnCurrRec ,1,RECCOUNT())
    GO RECORD lnCurrRec
  ENDIF
  SELECT(lcAlias)
ENDIF

*!*************************************************************
*! Name      : lfRemoveSty
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Remove Styles
*!*************************************************************
FUNCTION lfRemoveSty
PARAMETERS loFormSet



lcCurrfr = ""
IF loFormSet.ActiveMode = 'E'
  lcCurrfr = EVALUATE(loFormSet.lcFormStyles+'.cFrcstID')
ENDIF

lcStyletoDel = IIF(loFormSet.ActiveMode = 'E',lcCurrfr,"")+PADR(ALLTRIM(EVALUATE(loFormSet.lcFormStyles+'.Style')),19)
SELECT (loFormSet.lcFormStyles)
lfGetStyleDetail(loFormSet)
SELECT (loFormSet.lcFormStyles)
DELETE
IF SEEK(lcStyletoDel,loFormSet.lcStyleInv)
  SELECT (loFormSet.lcStyleInv)
  DELETE REST WHILE (IIF(loFormSet.ActiveMode = 'E',cFrcstID+STYLE+SCALE+cTYPE,STYLE+SCALE+cTYPE)) =lcStyletoDel
ENDIF
IF EOF(loFormSet.lcFormStyles)
  SELECT (loFormSet.lcFormStyles)
  LOCATE
ELSE
  SKIP -1
ENDIF
loFormSet.ariaform1.pgfFrcst.pgHeadr.grdStyles.REFRESH

SELECT (loFormSet.lcFormStyles)
COUNT FOR !DELETED() TO lnNotDeleted
IF lnNotDeleted <> 0 AND loFormSet.ActiveMode $ 'EA'
  loFormSet.ariaForm1.pgfFrcst.pgHeadr.cmdRemove.ENABLED = .T.
ELSE
  loFormSet.ariaForm1.pgfFrcst.pgHeadr.cmdRemove.ENABLED = .F.
ENDIF





*!*************************************************************
*! Name      : lfTrnDet
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Transaction details
*!*************************************************************
FUNCTION lfTrnDet
PARAMETERS loFormSet

DO CASE

CASE  loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.INDEX =1

  SELECT (loFormSet.lcStyleInv)
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
  SET FILTER TO
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]
  SET KEY TO
  SELECT(loFormSet.lcStyScl)
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
  SET FILTER TO
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

  SET KEY TO
  SELECT (loFormSet.lcStyWip)
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
  SET FILTER TO
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

  SET KEY TO
  SELECT(loFormSet.lcStyORD)
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
  SET FILTER TO
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

  SET KEY TO
  SELECT(loFormSet.lcStyYTD)
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
  SET FILTER TO
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

  SET KEY TO
  SELECT(loFormSet.lcStyLYS)
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
  SET FILTER TO
  *! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

  SET KEY TO
  lfDetGrdCntSrc(loFormSet,.T.)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *    lcCurrStyle = EVALUATE(loFormSet.lcFormStyles+'.Style')
  lcCurrStyle = PADR(EVALUATE(loFormSet.lcFormStyles+'.Style'),19)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  lcCurrFrct  = EVALUATE(loFormSet.lcFormStyles+'.cFrcstID')
  SELECT (loFormSet.lcStyleInv)
  IF loFormSet.ActiveMode $ 'EV'
    SET KEY TO lcCurrFrct+lcCurrStyle
    LOCATE
  ELSE

    SET KEY TO lcCurrStyle

    LOCATE
  ENDIF
  SELECT(loFormSet.lcStyScl)

  IF loFormSet.ActiveMode $ 'EV'
    SET KEY TO lcCurrFrct+lcCurrStyle
    LOCATE
  ELSE
    SET KEY TO lcCurrStyle
    LOCATE
  ENDIF

  SELECT (loFormSet.lcStyWip)
  SET KEY TO lcCurrStyle
  LOCATE
  SELECT(loFormSet.lcStyORD)
  SET KEY TO lcCurrStyle
  LOCATE
  SELECT(loFormSet.lcStyYTD)
  SET KEY TO lcCurrStyle
  LOCATE
  SELECT(loFormSet.lcStyLYS)
  SET KEY TO lcCurrStyle
  LOCATE
  SELECT (loFormSet.lcStyleInv)
  LOCATE

  WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst
    .lbStrtWeek.VISIBLE =.T.
    .lblEndW.VISIBLE =.T.
    .DtpStrtW.VISIBLE =.T.
    .dtpEndW.VISIBLE =.T.
    .lblMordQ.VISIBLE =.T.
    .lblQtyCon.VISIBLE =.T.
    .txtOrdQty.VISIBLE =.T.
    .txtQtyCont.VISIBLE =.T.
    .cmdTotals.VISIBLE =.T.
    .cmdOTBCal.VISIBLE =.T.
    .cmdExcl.VISIBLE =.T.
    .Ariashape3.VISIBLE =.T.
    .cmdFRCcalc.VISIBLE =.T.
    .Ariashape2.VISIBLE =.T.
    .ShpTrns.VISIBLE = .F.
    .cmdTans.VISIBLE = .F.
  ENDWITH

CASE  ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY) = 'WIP'AND !EOF(loFormSet.lcStyWIP)
  lfDisGrd(loFormSet,'W')

CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY) = 'ORD' AND !EOF(loFormSet.lcStyORD)
  lfDisGrd(loFormSet,'O')

CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY) = 'YTD' AND !EOF(loFormSet.lcStyYTD)
  lfDisGrd(loFormSet,'Y')

CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY) = 'LYS' AND !EOF(loFormSet.lcStyLYS)
  lfDisGrd(loFormSet,'L')

ENDCASE

*!*************************************************************
*! Name      : lfDisGrd
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Change grid source
*!*************************************************************
FUNCTION lfDisGrd
PARAMETERS loFormSet,lcType

WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst
  .lbStrtWeek.VISIBLE =.F.
  .lblEndW.VISIBLE =.F.
  .DtpStrtW.VISIBLE =.F.
  .dtpEndW.VISIBLE =.F.
  .lblMordQ.VISIBLE =.F.
  .lblQtyCon.VISIBLE =.F.
  .txtOrdQty.VISIBLE =.F.
  .txtQtyCont.VISIBLE =.F.
  .cmdTotals.VISIBLE =.F.
  .cmdOTBCal.VISIBLE =.F.
  .cmdExcl.VISIBLE =.F.
  .Ariashape3.VISIBLE =.F.
  .cmdFRCcalc.VISIBLE =.F.
  .Ariashape2.VISIBLE =.F.
  .ShpTrns.VISIBLE = .T.
  .cmdTans.VISIBLE = .T.
  .grdStyDet.RECORDSOURCE = ''
ENDWITH

DO CASE
CASE lcType = 'W'

  WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet
    .COLUMNCOUNT = 0
    .COLUMNCOUNT = 7

    .RECORDSOURCE = loFormSet.lcStyWip
    .Column1.CONTROLSOURCE = loFormSet.lcStyWip+'.Title'
    .Column1.Header1.CAPTION = "Transaction"
    .Column1.WIDTH = 130
    .Column1.READONLY =.T.
    .Column1.CURRENTCONTROL ='Text1'

    .Column2.CONTROLSOURCE = loFormSet.lcStyWip+'.PO'
    .Column2.Header1.CAPTION = "Tran#"
    .Column2.WIDTH = 90
    .Column2.READONLY =.T.
    .Column2.CURRENTCONTROL ='Text1'

    .Column3.CONTROLSOURCE = loFormSet.lcStyWip+'.Status'
    .Column3.Header1.CAPTION = "Status"
    .Column3.WIDTH = 75
    .Column3.READONLY =.T.
    .Column3.CURRENTCONTROL ='Text1'

    .Column4.CONTROLSOURCE = loFormSet.lcStyWip+'.Entered'
    .Column4.Header1.CAPTION = "Entered"
    .Column4.WIDTH = 100
    .Column4.READONLY =.T.
    .Column4.CURRENTCONTROL ='Text1'

    .Column5.CONTROLSOURCE = loFormSet.lcStyWip+'.Complete'
    .Column5.Header1.CAPTION = "Complete"
    .Column5.WIDTH = 100
    .Column5.READONLY =.T.
    .Column5.CURRENTCONTROL ='Text1'

    .Column6.CONTROLSOURCE = loFormSet.lcStyWip+'.QTY'
    .Column6.Header1.CAPTION = "Total Qty"
    .Column6.WIDTH = 100
    .Column6.READONLY =.T.
    .Column6.CURRENTCONTROL ='Text1'

    .Column7.CONTROLSOURCE = loFormSet.lcStyWip+'.Amount'
    .Column7.Header1.CAPTION = "Total Amount"
    .Column7.WIDTH = 100
    .Column7.READONLY =.T.
    .Column7.CURRENTCONTROL ='Text1'
  ENDWITH

CASE lcType = 'O'
  WITH  loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet
    .COLUMNCOUNT = 0
    .COLUMNCOUNT = 7

    .RECORDSOURCE = loFormSet.lcStyORD
    .Column1.CONTROLSOURCE = loFormSet.lcStyORD+'.ORder'
    .Column1.Header1.CAPTION = "Order#"
    .Column1.WIDTH = 100
    .Column1.READONLY =.T.
    .Column1.CURRENTCONTROL ='Text1'


    .Column2.CONTROLSOURCE = loFormSet.lcStyORD+'.Status'
    .Column2.Header1.CAPTION = "Status"
    .Column2.WIDTH = 100
    .Column2.READONLY =.T.
    .Column2.CURRENTCONTROL ='Text1'

    .Column3.CONTROLSOURCE = loFormSet.lcStyORD+'.Account'
    .Column3.Header1.CAPTION = "Account#"
    .Column3.WIDTH = 100
    .Column3.READONLY =.T.
    .Column3.CURRENTCONTROL ='Text1'

    .Column4.CONTROLSOURCE = loFormSet.lcStyORD+'.Store'
    .Column4.Header1.CAPTION = "Store#"
    .Column4.WIDTH = 100
    .Column4.READONLY =.T.
    .Column4.CURRENTCONTROL ='Text1'

    .Column5.CONTROLSOURCE = loFormSet.lcStyORD+'.NAme'
    .Column5.Header1.CAPTION = "Customer Name"
    .Column5.WIDTH = 150
    .Column5.READONLY =.T.
    .Column5.CURRENTCONTROL ='Text1'

    .Column6.CONTROLSOURCE = loFormSet.lcStyORD+'.qty'
    .Column6.Header1.CAPTION = "Open Qty"
    .Column6.WIDTH = 100
    .Column6.READONLY =.T.
    .Column6.CURRENTCONTROL ='Text1'

    .Column7.CONTROLSOURCE = loFormSet.lcStyORD+'.amount'
    .Column7.Header1.CAPTION = "Open Amount"
    .Column7.WIDTH = 100
    .Column7.READONLY =.T.
    .Column7.CURRENTCONTROL ='Text1'
  ENDWITH
CASE lcType = 'Y' OR lcType = 'L'
  WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet
    .COLUMNCOUNT = 0
    .COLUMNCOUNT = 8
    .RECORDSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)

    .Column1.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.Invoice'
    .Column1.Header1.CAPTION = "Invoice#"
    .Column1.WIDTH = 100
    .Column1.READONLY =.T.
    .Column1.CURRENTCONTROL ='Text1'

    .Column2.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.Status'
    .Column2.Header1.CAPTION = "Status"
    .Column2.WIDTH = 100
    .Column2.READONLY =.T.
    .Column2.CURRENTCONTROL ='Text1'

    .Column3.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.Account'
    .Column3.Header1.CAPTION = "Account#"
    .Column3.WIDTH = 100
    .Column3.READONLY =.T.
    .Column3.CURRENTCONTROL ='Text1'

    .Column4.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.Name'
    .Column4.Header1.CAPTION = "Customer Name"
    .Column4.WIDTH = 150
    .Column4.READONLY =.T.
    .Column4.CURRENTCONTROL ='Text1'

    .Column5.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.Order'
    .Column5.Header1.CAPTION = "Order#"
    .Column5.WIDTH = 100
    .Column5.READONLY =.T.
    .Column5.CURRENTCONTROL ='Text1'

    .Column6.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.ShpDate'
    .Column6.Header1.CAPTION = "Ship Date"
    .Column6.WIDTH = 100
    .Column6.READONLY =.T.
    .Column6.CURRENTCONTROL ='Text1'

    .Column7.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.QTY'
    .Column7.Header1.CAPTION = "Total Qty"
    .Column7.WIDTH = 100
    .Column7.READONLY =.T.
    .Column7.CURRENTCONTROL ='Text1'

    .Column8.CONTROLSOURCE = IIF(lcType = 'L',loFormSet.lcStyLYS,loFormSet.lcStyYTD)+'.Amount'
    .Column8.Header1.CAPTION = "Total Amount"
    .Column8.WIDTH = 100
    .Column8.READONLY =.T.
    .Column8.CURRENTCONTROL ='Text1'
  ENDWITH
ENDCASE
loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.REFRESH

*!*************************************************************
*! Name      : lfCallTrns
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Call Tarnsactions screen
*!*************************************************************
FUNCTION lfCallTrns
PARAMETERS loFormSet
DO CASE
  *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
  *CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SElectedItem.key) = 'WIP'

CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY) = 'WIP' OR ;
    ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.PARENT.KEY) = 'WIP'
  *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]


  IF EVALUATE(loFormSet.lcStyWip+'.TTYPE') = 'U'
    lcPO = "'"+EVALUATE(loFormSet.lcStyWip+'.PO')+"'"
    oAriaApplication.DoProgram("AWRMFCUTKT", lcPO ,.F.,'MF')
  ELSE
    lcPO = "'P','P','"+EVALUATE(loFormSet.lcStyWip+'.PO')+"'"
    oAriaApplication.DoProgram("AWRPOSTY", lcPO ,.F.,'PO')
  ENDIF

  *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
  *CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SElectedItem.key) = 'ORD'

CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY) = 'ORD'   OR ;
    ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.PARENT.KEY) = 'ORD'
  *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

  lcOrder = "'"+"O"+"','"+EVALUATE(loFormSet.lcStyORD+'.ORDER')+"'"
  oAriaApplication.DoProgram("AWRSOORD", lcOrder,.F.,'SO')

  *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][Begin]
  *CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SElectedItem.key) = 'YTD' OR ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SElectedItem.key) = 'LYS'

CASE ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY)        = 'YTD' OR ;
    ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.PARENT.KEY) = 'YTD' OR ;
    ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY)        = 'LYS' OR ;
    ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.PARENT.KEY) = 'LYS'
  *! B609965,1 HIA 07/10/2012 Fix WIP calculations in Style Forcasting [T20120618.0038][End]

  lcInvoice= "'"+IIF(ALLTRIM(loFormSet.ariaForm1.pgfFrcst.pgFrcst.trvFrcstInf.SELECTEDITEM.KEY) = 'YTD',EVALUATE(loFormSet.lcStyYTD+'.INVOICE'),EVALUATE(loFormSet.lcStyLYS+'.INVOICE'))+"'"
  oAriaApplication.DoProgram("AWRARDINV", lcInvoice,.F.,'AR')
ENDCASE

*!*************************************************************
*! Name      : lfDispTotals
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Display totals grid
*!*************************************************************
FUNCTION lfDispTotals
PARAMETERS loFormSet
loFormSet.llTotView = !loFormSet.llTotView



IF loFormSet.llTotView
  WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst
    .cmdTotals.CAPTION = 'Details View'
    WITH .grdStyDet
      .RECORDSOURCE = ''
      .COLUMNCOUNT = 0
      .COLUMNCOUNT = 6
      .RECORDSOURCE = loFormSet.lcStyScl

      .Column1.CONTROLSOURCE = loFormSet.lcStyScl+ '.cTitle'
      .Column1.WIDTH = 146
      .Column1.Header1.CAPTION = 'Qty/Size'
      .Column1.READONLY = .T.
      .Column1.FONTBOLD = .T.
      .Column1.ALIGNMENT = 2
      .Column1.Header1.ALIGNMENT = 2
      .COLUMNS(1).DYNAMICFORECOLOR ="IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyScl+'.cType'))) $ 'FJ',RGB(64,128,128),IIF(ALLTRIM(UPPER(EVALUATE(ThisFormSet.lcStyScl+'.cType'))) = 'G',RGB(255,0,0),RGB(0,0,0)))"
      .COLUMNS(1).SPARSE = .F.

      .Column2.CONTROLSOURCE = loFormSet.lcStyScl+ '.TotQty'
      .Column2.Header1.CAPTION = 'Total Qty'
      .Column2.READONLY = .T.
      .Column2.WIDTH = 100

      .Column3.CONTROLSOURCE = loFormSet.lcStyScl+ '.NCost'
      .Column3.Header1.CAPTION = 'Cost'
      .Column3.READONLY = .T.
      .Column3.WIDTH = 100

      .Column4.CONTROLSOURCE = loFormSet.lcStyScl+ '.TOTCOST'
      .Column4.Header1.CAPTION = 'Total Cost'
      .Column4.READONLY = .T.
      .Column4.WIDTH = 100

      .Column5.CONTROLSOURCE = loFormSet.lcStyScl+ '.nPrice'
      .Column5.Header1.CAPTION = 'Selling Price'
      .Column5.READONLY = .T.
      .Column5.WIDTH = 100

      .Column6.CONTROLSOURCE = loFormSet.lcStyScl+ '.TOTSELL'
      .Column6.Header1.CAPTION = 'Total Selling Price'
      .Column6.READONLY = .T.
      .Column6.WIDTH = 100
    ENDWITH
  ENDWITH
  SELECT(loFormSet.lcStyScl)
  LOCATE

ELSE
  WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst
    .cmdTotals.CAPTION = 'Totals View'
    lfDetGrdCntSrc(loFormSet,.T.)
    .lbStrtWeek.VISIBLE =.T.
    .lblEndW.VISIBLE =.T.
    .DtpStrtW.VISIBLE =.T.
    .dtpEndW.VISIBLE =.T.
    .lblMordQ.VISIBLE =.T.
    .lblQtyCon.VISIBLE =.T.
    .txtOrdQty.VISIBLE =.T.
    .txtQtyCont.VISIBLE =.T.
    .cmdTotals.VISIBLE =.T.
    .cmdOTBCal.VISIBLE =.T.
    .cmdExcl.VISIBLE =.T.
    .Ariashape3.VISIBLE =.T.
    .cmdFRCcalc.VISIBLE =.T.
    .Ariashape2.VISIBLE =.T.
    .ShpTrns.VISIBLE = .F.
    .cmdTans.VISIBLE = .F.
  ENDWITH
  SELECT(loFormSet.lcStyleInv)
  LOCATE
ENDIF

*!*************************************************************
*! Name      : lfCalTotCost
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Calc. total cost
*!*************************************************************
FUNCTION lfCalTotCost
PARAMETERS loFormSet
lnRecNum = RECNO(loFormSet.lcStyScl)
lnTotal = 0
lnTotal =  EVALUATE(loFormSet.lcStyScl+ '.TotQty') * EVALUATE(loFormSet.lcStyScl+ '.NCost')
REPLACE TOTCOST WITH lnTotal IN (loFormSet.lcStyScl)
IF BETWEEN(lnRecNum ,1,RECCOUNT(loFormSet.lcStyScl))
  GO RECORD lnRecNum  IN (loFormSet.lcStyScl)
ENDIF

*!*************************************************************
*! Name      : lfCaltotPric
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Calc. total Selling price
*!*************************************************************
FUNCTION lfCaltotPric
PARAMETERS loFormSet
lnRecNum = RECNO(loFormSet.lcStyScl)
lnTotal = 0
lnTotal = EVALUATE(loFormSet.lcStyScl+ '.TotQty') * EVALUATE(loFormSet.lcStyScl+ '.NPrice')
REPLACE TOTSELL WITH lnTotal IN (loFormSet.lcStyScl)
IF BETWEEN(lnRecNum ,1,RECCOUNT(loFormSet.lcStyScl))
  GO RECORD lnRecNum  IN (loFormSet.lcStyScl)
ENDIF

*!*************************************************************
*! Name      : lfInitCal
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Calculator screen init
*!*************************************************************
FUNCTION lfInitCal
PARAMETERS loFormSetBranch,lcType

IF lcType = 'B'
  loFormSetBranch.AriaForm1.CAPTION = 'Open To Buy Calculator'

  loFormSetBranch.lainitval [1,1] = 'Inventory'
  loFormSetBranch.lainitval [1,2] = 'B'

  loFormSetBranch.lainitval [2,1] = 'WIP'
  loFormSetBranch.lainitval [2,2] = 'C'

  loFormSetBranch.lainitval [3,1] = 'Orders'
  loFormSetBranch.lainitval [3,2] = 'D'

  loFormSetBranch.lainitval [4,1] = 'OTS'
  loFormSetBranch.lainitval [4,2] = 'E'

  loFormSetBranch.lainitval [5,1] = 'Sales Forecsting'
  loFormSetBranch.lainitval [5,2] = 'F'

  loFormSetBranch.lainitval [6,1] = 'Net OTS'
  loFormSetBranch.lainitval [6,2] = 'G'

  loFormSetBranch.lainitval [7,1] = 'YTD Sales'
  loFormSetBranch.lainitval [7,2] = 'H'

  loFormSetBranch.lainitval [8,1] = 'LY Sales'
  loFormSetBranch.lainitval [8,2] = 'I'
ELSE
  loFormSetBranch.AriaForm1.CAPTION = 'Sales Forecast Calculator'

  loFormSetBranch.lainitval [1,1] = 'Inventory'
  loFormSetBranch.lainitval [1,2] = 'B'

  loFormSetBranch.lainitval [2,1] = 'WIP'
  loFormSetBranch.lainitval [2,2] = 'C'

  loFormSetBranch.lainitval [3,1] = 'Orders'
  loFormSetBranch.lainitval [3,2] = 'D'

  loFormSetBranch.lainitval [4,1] = 'OTS'
  loFormSetBranch.lainitval [4,2] = 'E'

  loFormSetBranch.lainitval [5,1] = 'Net OTS'
  loFormSetBranch.lainitval [5,2] = 'G'

  loFormSetBranch.lainitval [6,1] = 'YTD Sales'
  loFormSetBranch.lainitval [6,2] = 'H'

  loFormSetBranch.lainitval [7,1] = 'LY Sales'
  loFormSetBranch.lainitval [7,2] = 'I'

  loFormSetBranch.lainitval [8,1] = 'Open To Buy'
  loFormSetBranch.lainitval [8,2] = 'J'
ENDIF
loFormSetBranch.ariaForm1.cboBase.ROWSOURCETYPE = 5
loFormSetBranch.ariaForm1.cboBase.ROWSOURCE = 'ThisFormSet.lainitval'
loFormSetBranch.ariaForm1.cboBase.VALUE = 'B'

*!*************************************************************
*! Name      : lfCalcCall
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : call Calculator
*!*************************************************************
FUNCTION lfCalcCall
PARAMETERS loFormSet,lcType
*! B609050,2 MMT 11/01/2009 fix bug of Error When Call Calc. on SAAS[Start]
IF oAriaApplication.MULTIINST
  DO FORM ("X:\aria4xp\Screens"+"\ic\ICFCSTCL.SCX") WITH loFormSet,lcType
ELSE
  *! B609050,2 MMT 11/01/2009 fix bug of Error When Call Calc. on SAAS[End]

  DO FORM (oAriaApplication.ScreenHome+"\ic\ICFCSTCL.SCX") WITH loFormSet,lcType

  *! B609050,2 MMT 11/01/2009 fix bug of Error When Call Calc. on SAAS[Start]
ENDIF
*! B609050,2 MMT 11/01/2009 fix bug of Error When Call Calc. on SAAS[End]

*!*************************************************************
*! Name      : lfEditStartW
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Start date  edit
*!*************************************************************
FUNCTION lfEditStartW
PARAMETERS loFormset
REPLACE dSTARTWEEK WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst.dtpStrtW.VALUE IN (loFormSet.lcFormStyles)

*!*************************************************************
*! Name      : lfEditEndW
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : End date  edit
*!*************************************************************
FUNCTION lfEditEndW
PARAMETERS loFormset
REPLACE dENDWEEK WITH loFormSet.ariaForm1.pgfFrcst.pgFrcst.dtpEndW.VALUE IN (loFormSet.lcFormStyles)


*!*************************************************************
*! Name      : lfBefSave
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Before Save function
*!*************************************************************
FUNCTION lfBefSave
PARAMETERS loFormSet


IF loFormSet.lnMaxScale = 0
  RETURN .F.
ENDIF

*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SELECT(loFormSet.lcFormStyles)
SET FILTER TO
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

SELECT (loFormSet.lcStyleInv)
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SET FILTER TO
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

SET KEY TO
SELECT(loFormSet.lcStyScl)
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SET FILTER TO
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

SET KEY TO
SELECT (loFormSet.lcStyWip)
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SET FILTER TO
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

SET KEY TO
SELECT(loFormSet.lcStyORD)
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SET FILTER TO
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

SET KEY TO
SELECT(loFormSet.lcStyYTD)
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SET FILTER TO
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

SET KEY TO
SELECT(loFormSet.lcStyLYS)
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SET FILTER TO
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

SET KEY TO

IF loFormSet.ActiveMode = 'A'
  SELECT (loFormSet.lcFormStyles)
  LOCATE
  IF EOF()
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No Lines found, can not Save")
    RETURN .F.
  ENDIF

  SELECT (loFormSet.lcStyleInv)
  SET KEY TO

  m.CFORECSTID = gfsequence('CFORECSTID')
  m.DSTARTDATE = loFormSet.ldDateStart
  m.DENDDATE = loFormSet.ldDateEnd
  m.DDATE    = loFormSet.ariaForm1.dtpRev.VALUE
  INSERT INTO 'FRCSTHDR' FROM MEMVAR
  =gfAdd_Info('FRCSTHDR')
  SELECT 'FRCSTHDR'
  gfreplace()

  SELECT (loFormSet.lcFormStyles)
  SCAN FOR !DELETED()
    lcStyle = EVALUATE(loFormSet.lcFormStyles+'.Style')
    SELECT STYLE
    =gfSeek(ALLTRIM(lcStyle))
    SCAN REST WHILE STYLE = ALLTRIM(lcStyle)
      STORE 0 TO m.NTOTSTK, m.totOrd ,m.totWIP,m.NTOTFRCST,m.NTOTYTDSLS ,m.NTOTLYSLS,m.NTOTOTB
      m.Style = STYLE.STYLE
      =gfSeek("S"+STYLE.SCALE,'Scale','Scale')
      m.dSTARTWEEK = loFormSet.ldDateStart
      m.dENDWEEK = loFormSet.ldDateEnd
      FOR lnZ = 1 TO 8
        lcZ = STR(lnZ,1)
        STORE 0 TO m.nStk&lcZ.,m.Ord&lcZ.,m.WIP&lcZ.,m.NFRCSTQTY&lcZ.,m.NYTDSLS&lcZ.,m.NLYSLS&lcZ.,m.NOTBQTY&lcZ.
      ENDFOR
      lcScale = STYLE.SCALE

      SELECT (loFormSet.lcStyleInv)
      IF SEEK(PADR(ALLTRIM(lcStyle),19)+lcScale+ 'B',loFormSet.lcStyleInv)
        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          m.nStk&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
          m.NTOTSTK = m.NTOTSTK + m.nStk&lcI
        ENDFOR
      ENDIF

      IF SEEK(PADR(ALLTRIM(lcStyle),19)+lcScale+ 'D',loFormSet.lcStyleInv)
        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          m.Ord&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
          m.totOrd = m.totOrd + m.Ord&lcI
        ENDFOR
      ENDIF

      IF SEEK(PADR(ALLTRIM(lcStyle),19)+lcScale+'C',loFormSet.lcStyleInv)
        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          m.WIP&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
          m.totWIP = m.totWIP + m.WIP&lcI
        ENDFOR
      ENDIF

      IF SEEK(PADR(ALLTRIM(lcStyle),19)+lcScale+'F',loFormSet.lcStyleInv)
        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          m.NFRCSTQTY&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
          m.NTOTFRCST= m.NTOTFRCST + m.NFRCSTQTY&lcI
        ENDFOR
      ENDIF

      IF SEEK(PADR(ALLTRIM(lcStyle),19)+lcScale+'H',loFormSet.lcStyleInv)
        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          m.NYTDSLS&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
          m.NTOTYTDSLS = m.NTOTYTDSLS + m.NYTDSLS&lcI
        ENDFOR
      ENDIF

      IF SEEK(PADR(ALLTRIM(lcStyle),19)+lcScale+'I',loFormSet.lcStyleInv)

        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          m.NLYSLS&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
          m.NTOTLYSLS = m.NTOTLYSLS+ m.NLYSLS&lcI
        ENDFOR
      ENDIF

      IF SEEK(PADR(ALLTRIM(lcStyle),19)+lcScale+ 'J',loFormSet.lcStyleInv)
        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          m.NOTBQTY&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
          REPLACE plan&lcI WITH m.NOTBQTY&lcI IN STYLE
          m.NTOTOTB= m.NTOTOTB+ m.NOTBQTY&lcI
        ENDFOR
        SELECT STYLE
        FOR lnI = 1 TO SCALE.CNT
          lcI =STR(lnI,1)
          gfREPLACE("plan&lcI WITH m.NOTBQTY&lcI")
        ENDFOR
        gfREPLACE("totplan  WITH m.NTOTOTB")
      ENDIF
      INSERT INTO 'FRCSTDTL' FROM MEMVAR
      SELECT 'FRCSTDTL'
      =gfAdd_Info('FRCSTDTL')
      SELECT 'FRCSTDTL'
      gfreplace()
    ENDSCAN
  ENDSCAN
ELSE && Edit Mode


  SELECT (loFormSet.lcFormStyles)
  LOCATE
  IF EOF()
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No Lines found, can not Save")
    RETURN .F.
  ENDIF
  SELECT (loFormSet.lcStyleInv)
  SET KEY TO
  lcDel = SET("Deleted")
  SET DELETED OFF
  *Save
  SELECT (loFormSet.lcFormStyles)
  SCAN
    lcStyle = STYLE
    lcForeCst = cFrcstID

    IF !SEEK(lcForeCst,'FRCSTHDR')
      =gfSeek(lcForeCst,'FRCSTHDR')
    ENDIF
    IF !SEEK(lcForeCst,'FRCSTDTL')
      =gfSeek(lcForeCst,'FRCSTDTL')
    ENDIF

    IF FRCSTHDR.DDATE <> loFormSet.ariaForm1.dtpRev.VALUE
      SELECT FRCSTHDR
      gfReplace("DDATE With CTOD('"+DTOC(loFormSet.ariaForm1.dtpRev.VALUE)+"')")
      =gfAdd_Info('FRCSTHDR')
      gfreplace()
    ENDIF
    SELECT (loFormSet.lcFormStyles)
    IF DELETED()
      SELECT FRCSTDTL
      IF SEEK(lcForeCst+ALLTRIM(lcStyle))
        SCAN REST WHILE CFORECSTID+STYLE = lcForeCst+ALLTRIM(lcStyle)
          =gfDelete()
        ENDSCAN
      ENDIF
    ELSE
      *SELECT (loFormSet.lcStyleInv)
      *SCAN
      SELECT FRCSTDTL
      IF SEEK(lcForeCst+ALLTRIM(lcStyle)) && existing record
        SELECT STYLE
        =gfSeek(ALLTRIM(lcStyle))
        lnCnt = 1
        m.CFORECSTID = lcForeCst
        SCAN REST WHILE STYLE = ALLTRIM(lcStyle)
          =SEEK(m.CFORECSTID+STYLE.STYLE,'FRCSTDTL')
          =gfSeek("S"+STYLE.SCALE,'Scale','Scale')
          STORE 0 TO m.NTOTSTK, m.totOrd ,m.totWIP,m.NTOTFRCST,m.NTOTYTDSLS ,m.NTOTLYSLS,m.NTOTOTB
          m.Style = STYLE.STYLE
          m.dSTARTWEEK = loFormSet.ldDateStart
          m.dENDWEEK = loFormSet.ldDateEnd
          FOR lnZ = 1 TO 8
            lcZ = STR(lnZ,1)
            STORE 0 TO m.nStk&lcZ.,m.Ord&lcZ.,m.WIP&lcZ.,m.NFRCSTQTY&lcZ.,m.NYTDSLS&lcZ.,m.NLYSLS&lcZ.,m.NOTBQTY&lcZ.
          ENDFOR
          lcScale =STYLE.SCALE

          *MMT22
          SELECT (loFormSet.lcStyleInv)
          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'B',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.nStk&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTSTK= m.NTOTSTK+ m.nStk&lcI
            ENDFOR
          ENDIF


          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'D',loFormSet.lcStyleInv)

            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.Ord&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.totOrd = m.totOrd + m.Ord&lcI
            ENDFOR
          ENDIF

          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'C',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.WIP&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.totWIP= m.totWIP+ m.WIP&lcI
            ENDFOR
          ENDIF


          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'F',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NFRCSTQTY&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTFRCST= m.NTOTFRCST+ m.NFRCSTQTY&lcI
            ENDFOR
          ENDIF


          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'H',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NYTDSLS&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTYTDSLS = m.NTOTYTDSLS + m.NYTDSLS&lcI
            ENDFOR
          ENDIF

          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+ 'I',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NLYSLS&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTLYSLS = m.NTOTLYSLS + m.NLYSLS&lcI
            ENDFOR
          ENDIF

          IF SEEK( m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'J',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NOTBQTY&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTOTB= m.NTOTOTB+ m.NOTBQTY&lcI
            ENDFOR
          ENDIF
          *MMT22
          SELECT STYLE
          FOR lnI = 1 TO SCALE.CNT
            lcI =STR(lnI,1)
            gfREPLACE("plan&lcI WITH m.NOTBQTY&lcI")
          ENDFOR
          gfREPLACE("totplan  WITH m.NTOTOTB")


          SELECT FRCSTDTL
          GATHER MEMO MEMVAR
          =gfAdd_Info('FRCSTDTL')
          SELECT 'FRCSTDTL'
          gfreplace()
        ENDSCAN
      ELSE && New
        *M
        SELECT STYLE
        =gfSeek(ALLTRIM(lcStyle))
        lnCnt = 1
        m.CFORECSTID = lcForeCst
        SCAN REST WHILE STYLE = ALLTRIM(lcStyle)
          STORE 0 TO m.NTOTSTK, m.totOrd ,m.totWIP,m.NTOTFRCST,m.NTOTYTDSLS ,m.NTOTLYSLS,m.NTOTOTB
          m.Style = STYLE.STYLE
          =gfSeek("S"+STYLE.SCALE,'Scale','Scale')
          m.dSTARTWEEK = loFormSet.ldDateStart
          m.dENDWEEK = loFormSet.ldDateEnd
          FOR lnZ = 1 TO 8
            lcZ = STR(lnZ,1)
            STORE 0 TO m.nStk&lcZ.,m.Ord&lcZ.,m.WIP&lcZ.,m.NFRCSTQTY&lcZ.,m.NYTDSLS&lcZ.,m.NLYSLS&lcZ.,m.NOTBQTY&lcZ.
          ENDFOR
          lcScale =STYLE.SCALE

          *MMT22
          SELECT (loFormSet.lcStyleInv)
          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'B',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.nStk&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTSTK= m.NTOTSTK+ m.nStk&lcI
            ENDFOR
          ENDIF


          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'D',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.Ord&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.totOrd = m.totOrd + m.Ord&lcI
            ENDFOR
          ENDIF

          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'C',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.WIP&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.totWIP= m.totWIP+ m.WIP&lcI
            ENDFOR
          ENDIF


          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+ 'F',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NFRCSTQTY&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTFRCST= m.NTOTFRCST+ m.NFRCSTQTY&lcI
            ENDFOR
          ENDIF


          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'H',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NYTDSLS&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTYTDSLS = m.NTOTYTDSLS + m.NYTDSLS&lcI
            ENDFOR
          ENDIF

          IF SEEK(m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'I',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NLYSLS&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTLYSLS = m.NTOTLYSLS + m.NLYSLS&lcI
            ENDFOR
          ENDIF

          IF SEEK( m.CFORECSTID+PADR(ALLTRIM(lcStyle),19)+lcScale+'J',loFormSet.lcStyleInv)
            FOR lnI = 1 TO SCALE.CNT
              lcI =STR(lnI,1)
              m.NOTBQTY&lcI = VAL(EVALUATE(loFormSet.lcStyleInv+'.Qty'+lcI))
              m.NTOTOTB= m.NTOTOTB+ m.NOTBQTY&lcI
            ENDFOR
          ENDIF
          SELECT STYLE
          FOR lnI = 1 TO SCALE.CNT
            lcI =STR(lnI,1)
            gfREPLACE("plan&lcI WITH m.NOTBQTY&lcI")
          ENDFOR
          gfREPLACE("totplan  WITH m.NTOTOTB")

          INSERT INTO 'FRCSTDTL' FROM MEMVAR
          SELECT 'FRCSTDTL'
          =gfAdd_Info('FRCSTDTL')
          SELECT 'FRCSTDTL'
          gfreplace()
        ENDSCAN

      ENDIF

    ENDIF
  ENDSCAN

  SET DELETED &lcDel
ENDIF
SELECT 'FRCSTHDR'
gfTableUpdate()
SELECT 'FRCSTDTL'
gfTableUpdate()
SELECT STYLE
gfTableUpdate()

IF loFormSet.ActiveMode = 'A'
  =gfModalGen("TRM00000B00000","Dialog",.F.,.F.,'Forecasting has been saved as '+m.CFORECSTID)
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfAddNewSty
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Add New style
*!*************************************************************
FUNCTION lfAddNewSty
PARAMETERS loFormSet
lcCurrfr = ''
IF loFormSet.ActiveMode = 'E'
  IF EOF(loFormSet.lcFormStyles)
    SELECT (loFormSet.lcFormStyles)
    LOCATE
  ENDIF
  lcCurrfr = EVALUATE(loFormSet.lcFormStyles+'.cFrcstID')
ENDIF

lcSty =gfStyBrw('N','',"",.F.)


IF loFormSet.lnMaxScale = 0
  lfCrtStylFiles(loFormSet)
ENDIF


IF !EMPTY(lcSty)
  STORE 0 TO lnClrLen,lnClrPos,lnSclLen ,lnSclPos
  DIMENSION laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    DO CASE
    CASE laItemSeg[lnCount,1]='C'
      lnClrLen = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      lcClrSpr = ALLT(laItemSeg[lnCount,6])

    CASE  laItemSeg[lnCount,1]='S'
      lnSclLen = LEN(laItemSeg[lnCount,3])
      lnSclPos = laItemSeg[lnCount,4]

    ENDCASE
  ENDFOR




  SELECT STYLE
  =gfSetOrder('style')
  lcStyleMj = SUBSTR(lcSty,1,lnClrLen +lnClrPos  - 1)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=gfSeek(ALLTRIM(lcStyleMj),'Style')
  =gfSeek(lcStyleMj,'Style')
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  SELECT STYLE
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *SCAN REST WHILE Style = ALLTRIM(lcStyleMj)
  SCAN REST WHILE STYLE = lcStyleMj
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    m.Style = STYLE.STYLE
    m.DESC = STYLE.DESC
    m.TotInv = STYLE.totstk
    m.TotWip = STYLE.totwip
    m.TotOrd = STYLE.totord
    m.Currpln = STYLE.totplan
    m.TotOts  = STYLE.totwip +STYLE.totstk -STYLE.totord
    IF !SEEK( m.Style,loFormSet.lcTempStyles)
      INSERT INTO (loFormSet.lcTempStyles) FROM MEMVAR
    ENDIF
  ENDSCAN
  SELECT (loFormSet.lcTempStyles)

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *=SEEK(ALLTRIM(lcStyleMj))
  =SEEK(lcStyleMj)
  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

  lcStyleTemp = loFormSet.lcTempStyles

  *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
  *  SCAN REST WHILE Style = ALLTRIM(lcStyleMj)
  SCAN REST WHILE STYLE = lcStyleMj
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]

    lcStyleClr = SUBSTR(STYLE,1,lnClrLen +lnClrPos  - 1)
    =gfSeek(&lcStyleTemp..STYLE,'STYLE','STYLE')
    =gfSeek("S"+STYLE.SCALE,'Scale','Scale')
    SELECT (lcStyleTemp)
    m.TotInv = 0
    m.TotWip =0
    m.TotOrd = 0
    m.Currpln = 0
    m.TotOts  = 0
    m.Style = lcStyleClr
    m.DESC = STYLE.DESC
    m.nSclCnt = 0
    *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[Start]
    *!*       =SEEK(ALLTRIM(lcStyleClr))
    *!*      SCAN REST WHILE Style = ALLTRIM(lcStyleClr)
    =SEEK(lcStyleClr)
    SCAN REST WHILE STYLE = lcStyleClr
      =gfSeek(&lcStyleTemp..STYLE,'STYLE','STYLE')
      =gfSeek("S"+STYLE.SCALE,'Scale','Scale')
      *: B608743,2 MMT 11/23/2008 Fix bug of wrong Total Ordered in Add Mode[End]
      m.TotInv = m.TotInv + TotInv
      m.TotWip = m.TotWip + TotWip
      m.TotOrd = m.TotOrd + TotOrd
      m.Currpln = m.Currpln + Currpln
      m.TotOts = m.TotOts + TotOts
      IF m.nSclCnt <  SCALE.CNT
        m.nSclCnt = SCALE.CNT
      ENDIF
      IF loFormSet.lnMaxScale < m.nSclCnt
        loFormSet.lnMaxScale = m.nSclCnt
      ENDIF
    ENDSCAN

    IF !SEEK(IIF(loFormSet.ActiveMode = 'E',lcCurrfr,'')+m.Style,loFormSet.lcFormStyles)
      m.cFrcstID =  lcCurrfr
      INSERT INTO (loFormSet.lcFormStyles) FROM MEMVAR
      lfGetStyleDetail(loFormSet,'A')
    ENDIF
  ENDSCAN
ENDIF
loFormSet.ariaform1.pgfFrcst.pgHeadr.grdStyles.REFRESH


*!*************************************************************
*! Name      : lfGetFrDetails
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Get style details in view mode
*!*************************************************************
FUNCTION lfGetFrDetails
PARAMETERS loFormSet

*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
SELECT(loFormSet.lcFormStyles)
SET ORDER TO (loFormSet.lcFormStyles)
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]


STORE 0 TO lnClrLen,lnClrPos,lnSclLen ,lnSclPos
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])

  CASE  laItemSeg[lnCount,1]='S'
    lnSclLen = LEN(laItemSeg[lnCount,3])
    lnSclPos = laItemSeg[lnCount,4]

  ENDCASE
ENDFOR
SELECT (loFormSet.lcFRCSTDTL)
LOCATE
IF !EOF()
  SCAN
    lnRecn = RECNO()
    IF !SEEK(CFORECSTID+PADR(SUBSTR(STYLE,1,lnClrPos +lnClrLen -1),19),loFormSet.lcFormStyles)
      lcFrcst =EVALUATE(loFormSet.lcFRCSTDTL+'.CFORECSTID')
      lcStyle = SUBSTR(STYLE,1,lnClrPos +lnClrLen -1)
      m.TotInv = 0
      m.TotWip =0
      m.TotOrd = 0
      m.Currpln = 0
      m.TotOts  = 0
      m.nSclCnt = 0
      m.cFrcstID = lcFrcst
      m.Style = lcStyle
      SCAN REST WHILE CFORECSTID+STYLE = lcFrcst + lcStyle
        =gfSeek(EVALUATE(loFormSet.lcFRCSTDTL+'.Style'),'STYLE','STYLE')
        =gfSeek("S"+STYLE.SCALE,'Scale','Scale')
        m.DESC = STYLE.DESC
        m.TotInv = m.TotInv + ntotStk
        m.TotWip = m.TotWip + TotWip
        m.TotOrd = m.TotOrd + TotOrd
        m.Currpln = m.Currpln + ntotOtb
        m.NQTYCONT = STYLE.NQTYCONT
        m.NMINQTY = STYLE.NMINQTY
        m.TotOts = m.TotOts + (TotWip+ntotStk-TotOrd)
        IF SCALE.CNT > m.nSclCnt
          m.nSclCnt =   SCALE.CNT
        ENDIF
      ENDSCAN
      IF  m.nSclCnt > loFormSet.lnMaxScale
        loFormSet.lnMaxScale = m.nSclCnt
      ENDIF
      INSERT INTO (loFormSet.lcFormStyles) FROM MEMVAR
    ENDIF
    SELECT (loFormSet.lcFRCSTDTL)
    IF BETWEEN(lnRecn,1,RECCOUNT())
      GO RECORD lnRecn
    ENDIF
  ENDSCAN
ENDIF
IF loFormSet.lnMaxScale > 0
  lfCrtStylFiles(loFormSet)
ENDIF

SELECT(loFormSet.lcFormStyles)
SCAN
  lfGetStyleDetail(loFormSet)
ENDSCAN
SELECT(loFormSet.lcFormStyles)
LOCATE

*!*************************************************************
*! Name      : lfcalcval
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Calculator functionality
*!*************************************************************
FUNCTION lfcalcval
PARAMETERS loBranFormSet
lcBase = loBranFormSet.ariaForm1.cboBase.VALUE
loParentForm = loBranFormSet.loparformset
lcTotalsFile = loParentForm.lcStyScl
lnBaseQty = 0
lcDistrBy = loBranFormSet.ariaform1.cboDist.VALUE &&= 'R' E
lcSign = loBranFormSet.ariaForm1.cmdPlus.CAPTION
lnPerCent = loBranFormSet.ariaForm1.spnPer.VALUE
lcStyleCur = PADR(EVALUATE(loParentForm .lcStyleInv+'.Style'),19)
IF loParentForm .ActiveMode = 'E'
  lcCurFr = EVALUATE(loParentForm.lcStyleInv+'.cFrcstID')
  lcStyleCur = lcCurFr + lcStyleCur
ENDIF
IF SEEK(lcStyleCur+lcBase, lcTotalsFile)
  lnBaseQty = &lcTotalsFile..TOTQTY
ENDIF
lccaltype = loBranFormSet.lccaltype
lnValuetoDis = lnBaseQty &lcSign (lnBaseQty * (lnPerCent/100))

IF lnValuetoDis = 0
  RETURN
ENDIF


IF lcDistrBy = 'R'
  SELECT (loParentForm.lcStyleInv)
  lnRecNo = RECNO()
  =SEEK(lcStyleCur)
  SCAN REST WHILE (IIF(loFormSet.ActiveMode = 'E',cFrcstID+STYLE+SCALE+cTYPE,STYLE+SCALE+cTYPE)) = lcStyleCur FOR Ctype = lcBase
    lcScale = SCALE
    FOR lnI = 1 TO 8
      lcI =STR(lnI,1)
      STORE 0 TO m.OTB&lcI.
      lnQtyPer = VAL(QTY&lcI.)/lnBaseQty
      m.OTB&lcI. = CEILING(lnValuetoDis * lnQtyPer)
    ENDFOR
    lnRec2 = RECNO()
    IF SEEK(lcStyleCur +lcScale +IIF(lccaltype  = 'B','J','F'))
      lnOldTotQty = VAL(Tqty)
      *REPLACE Tqty WITH 0
      FOR lnI = 1 TO 8
        lcI =STR(lnI,1)
        REPLACE QTY&lcI. WITH STR(VAL(QTY&lcI.)+m.OTB&lcI.,7)
        *REPLACE Tqty WITH TQTY+VAL(QTY&lcI.)
      ENDFOR
      *!*	       IF SEEK(lcStyleCur +IIF(lccaltype  = 'B','J','F'),lcTotalsFile)
      *!*	         REPLACE totqty WITH totqty +(EVALUATE(loParentForm.lcStyleInv+'.Tqty') - lnOldTotQty) IN (lcTotalsFile)
      *!*	       ENDIF
      lfUpdOts(loParentForm)

    ENDIF
    IF BETWEEN(lnRec2 ,1,RECCOUNT())
      GO RECORD lnRec2
    ENDIF
  ENDSCAN
  IF BETWEEN(lnRecNo ,1,RECCOUNT())
    GO RECORD lnRecNo
  ENDIF
ELSE && Evenly
  SELECT (loParentForm.lcStyleInv)
  lnRecNo = RECNO()
  =SEEK(lcStyleCur)
  lnScalCnt = 0
  SCAN REST WHILE (IIF(loFormSet.ActiveMode = 'E',cFrcstID+STYLE+SCALE+cTYPE,STYLE+SCALE+cTYPE)) = lcStyleCur FOR Ctype = 'A'
    FOR lnI = 1 TO 8
      lcI =STR(lnI,1)
      IF !EMPTY(QTY&lcI.)
        lnScalCnt = lnScalCnt + 1
      ENDIF
    ENDFOR
  ENDSCAN
  lnQtyToAdd = 0
  IF lnScalCnt > 0
    lnQtyToAdd = CEILING(lnValuetoDis /lnScalCnt)
  ENDIF
  =SEEK(lcStyleCur)
  SCAN REST WHILE (IIF(loFormSet.ActiveMode = 'E',cFrcstID+STYLE+SCALE+cTYPE,STYLE+SCALE+cTYPE)) = lcStyleCur FOR Ctype = IIF(lccaltype  = 'B','J','F')
    =gfSeek('S'+SCALE,'Scale','Scale')
    FOR lnI = 1 TO SCALE.CNT
      lcI =STR(lnI,1)
      REPLACE QTY&lcI. WITH STR(VAL(QTY&lcI.)+lnQtyToAdd ,7)
      *,;
      TQTY     WITH TQTY + lnQtyToAdd
      *!*	       IF SEEK(lcStyleCur +IIF(lccaltype  = 'B','J','F'),lcTotalsFile)
      *!*	         REPLACE totqty WITH totqty +lnQtyToAdd  IN (lcTotalsFile)
      *!*	       ENDIF
    ENDFOR
    lfUpdOts(loParentForm)
  ENDSCAN
  IF BETWEEN(lnRecNo ,1,RECCOUNT())
    GO RECORD lnRecNo
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfExport
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export fucntion
*!*************************************************************
FUNCTION lfExport
PARAMETERS loFormset

PRIVATE  lcCursorName, lnHeaderHeight, lnRowHeight, ;
  lcFontName, lnSize, llBold, llItalic, llUnderline

lcValue = 0

PRIVATE laExportColumns[1, 1], laExportColorShades[1, 1]

lcCursorName = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.RECORDSOURCE

FOR lnCount = 1 TO loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.COLUMNCOUNT
  DIMENSION laExportColumns[lnCount, 5]
  laExportColumns[lnCount, 1] = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.COLUMNS[lnCount].Header1.CAPTION
  laExportColumns[lnCount, 2] = TYPE(loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.COLUMNS[lnCount].CONTROLSOURCE)
  laExportColumns[lnCount, 3] = STRTRAN(loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.COLUMNS[lnCount].CONTROLSOURCE, loFormSet.lcStyleInv, lcCursorName, -1, -1, 1)
  laExportColumns[lnCount, 4] = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.COLUMNS[lnCount].WIDTH / 0.75 * (216 / 372)
  laExportColumns[lnCount, 5] = STRTRAN(loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.COLUMNS[lnCount].DYNAMICBACKCOLOR, loFormSet.lcStyleInv, lcCursorName, -1, -1, 1)
ENDFOR

lnHeaderHeight = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.HEADERHEIGHT / 0.75 * (216 / 372)
lnRowHeight    = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.ROWHEIGHT / 0.75 * (216 / 372)

DIMENSION laExportColorShades[1]
STORE '' TO laExportColorShades

IF TYPE('loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.FontName') = 'C' .AND. !EMPTY(loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.FONTNAME)
  lcFontName     = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.FONTNAME
  lnSize         = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.FONTSIZE
  llBold         = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.FONTBOLD
  llItalic        = loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.FONTITALIC
  llUnderline    = .F.
ENDIF

lfexpColor(lcCursorName , 'laExportColumns', lnHeaderHeight , lnRowHeight    , laExportColorShades, ;
  lcFontName, lnSize, llBold, llItalic, llUnderline)

*!*  lcFileName = GETFILE('XLS','',"Save")
*!*  IF EMPTY(lcFileName)
*!*    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Invalid File")
*!*    RETURN
*!*  ENDIF

*!*  lcExpTemp = gfTempName()
*!*  DO CASE
*!*    CASE loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.RecordSource = loFormSet.lcStyleInv AND !loFormSet.llTotView
*!*      DIMENSION laExpCrs[EVALUATE(loFormSet.lcFormStyles+'.nSclCnt')+2,4]

*!*      laExpCrs[2,1] = 'Title'
*!*      laExpCrs[2,2] = 'C'
*!*      laExpCrs[2,3] = 30
*!*      laExpCrs[2,4] = 0
*!*
*!*      laExpCrs[1,1] = 'Style'
*!*      laExpCrs[1,2] = 'C'
*!*      laExpCrs[1,3] = 19
*!*      laExpCrs[1,4] = 0
*!*
*!*
*!*
*!*      lnCounter = 3
*!*
*!*      FOR lnC = 1 TO EVALUATE(loFormSet.lcFormStyles+'.nSclCnt')
*!*        lcC = ALLTRIM(STR(lnC))
*!*        laExpCrs[lnCounter ,1] = 'QTY'+lcC
*!*        laExpCrs[lnCounter ,2] = 'C'
*!*        laExpCrs[lnCounter ,3] = 7
*!*        laExpCrs[lnCounter ,4] = 0
*!*        lnCounter = lnCounter + 1
*!*      ENDFOR
*!*
*!*      =gfCrtTmp(lcExpTemp ,@laExpCrs,"Style",lcExpTemp ,.T.)
*!*      lcInvSty = loFormSet.lcStyleInv
*!*      SELECT (lcExpTemp)
*!*      APPEND BLANK
*!*      REPLACE Style WITH "",;
*!*              title WITH "Qty/Size"
*!*
*!*      FOR lnC = 1 TO EVALUATE(loFormSet.lcFormStyles+'.nSclCnt')
*!*        lcC = ALLTRIM(STR(lnC))
*!*        REPLACE QTY&lcC. WITH EVALUATE(loFormSet.lcStyScl +'.SCL'+lcC)
*!*
*!*      ENDFOR

*!*      SELECT(loFormSet.lcStyleInv)
*!*      lnRcNo = RECNO()
*!*      SCAN
*!*        SELECT (lcExpTemp)
*!*        APPEND BLANK
*!*        REPLACE Style WITH &lcInvSty..Style,;
*!*                title WITH &lcInvSty..Ctitle
*!*        FOR lnC = 1 TO EVALUATE(loFormSet.lcFormStyles+'.nSclCnt')
*!*          lcC = ALLTRIM(STR(lnC))
*!*          REPLACE QTY&lcC. WITH STR(&lcInvSty..QTY&lcC.,7,0)
*!*        ENDFOR
*!*      ENDSCAN



*!*      SELECT (lcExpTemp)
*!*      EXPORT TO (lcFileName) FIELDS ctitle,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TQTy   TYPE XLS
*!*
*!*      SELECT(loFormSet.lcStyleInv)
*!*      IF BETWEEN(lnRcNo ,1,RECCOUNT())
*!*        GO RECORD lnRcNo
*!*      ENDIF

*!*    CASE loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.RecordSource = loFormSet.lcStyleInv AND loFormSet.llTotView
*!*      DIMENSION laExpCrs[7,4]
*!*      laExpCrs[1,1]  = 'STYLE'
*!*      laExpCrs[1,2]  = 'C'
*!*      laExpCrs[1,3]  = 19
*!*      laExpCrs[1,4]  = 0
*!*
*!*      laExpCrs[2,1]  = 'Title'
*!*      laExpCrs[2,2]  = 'C'
*!*      laExpCrs[2,3]  = 30
*!*      laExpCrs[2,4]  = 0
*!*
*!*      laExpCrs[3,1]  = 'TOTQTY'
*!*      laExpCrs[3,2]  = 'C'
*!*      laExpCrs[3,3]  = 10
*!*      laExpCrs[3,4]  = 0
*!*
*!*      laExpCrs[4,1]  = 'Cost'
*!*      laExpCrs[4,2]  = 'C'
*!*      laExpCrs[4,3]  = 10
*!*      laExpCrs[4,4]  = 0

*!*      laExpCrs[5,1]  = 'TotCost'
*!*      laExpCrs[5,2]  = 'C'
*!*      laExpCrs[5,3]  = 12
*!*      laExpCrs[5,4]  = 0
*!*
*!*      laExpCrs[6,1]  = 'SelPrice'
*!*      laExpCrs[6,2]  = 'C'
*!*      laExpCrs[6,3]  = 15
*!*      laExpCrs[6,4]  = 0

*!*      laExpCrs[7,1]  = 'TotSelPr'
*!*      laExpCrs[7,2]  = 'C'
*!*      laExpCrs[7,3]  = 20
*!*      laExpCrs[7,4]  = 0
*!*
*!*      =gfCrtTmp(lcExpTemp ,@laExpCrs,"Style",lcExpTemp ,.T.)
*!*      lcInvSty = loFormSet.lcStyleInv
*!*      SELECT(lcExpTemp)
*!*      APPEND BLANK
*!*      REPLACE Style WITH '',;
*!*              title WITH "Qty/Size",;
*!*              TOTQTY WITH 'Total Qty' ,;
*!*              Cost  WITH  'Cost',;
*!*              TOTCost WITH  'Total Cost' ,;
*!*              SelPrice WITH 'Selling Price',;
*!*              TotSelPr  WITH 'Total Selling Price'
*!*
*!*      SELECT(lcInvSty)
*!*      lnRcNo = RECNO()
*!*      SCAN
*!*        SELECT(lcExpTemp)
*!*        APPEND BLANK
*!*        REPLACE Style WITH  &lcInvSty..Style,;
*!*                title WITH &lcInvSty..Ctitle,;
*!*                TOTQTY WITH STR(&lcInvSty..Tqty,9,0) ,;
*!*                Cost  WITH  STR(&lcInvSty..nCost,9,2),;
*!*                TOTCost WITH  STR(&lcInvSty..Tqty * &lcInvSty..nCost,10,2),;
*!*                SelPrice WITH STR(&lcInvSty..nPrice,9,2),;
*!*                TotSelPr  WITH STR(&lcInvSty..Tqty * &lcInvSty..nPrice,10,2)
*!*      ENDSCAN
*!*      SELECT(loFormSet.lcStyleInv)
*!*      IF BETWEEN(lnRcNo ,1,RECCOUNT())
*!*        GO RECORD lnRcNo
*!*      ENDIF
*!*
*!*      SELECT (lcExpTemp)
*!*      EXPORT TO (lcFileName) TYPE XLS
*!*  ENDCASE

*!*************************************************************
*! Name      : lfBefDelete
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Delete fucntion
*!*************************************************************
FUNCTION lfBefDelete
PARAMETERS loFormSet
lcFrsct = ALLTRIM(loFormSet.ariaForm1.kbFrcstID.KeyTextBox.VALUE)



IF gfModalGen('TRM00000B42002',.F.,.F.,.F.,"Are you sure you want to delete Forecasting "+lcFrsct)=2
  RETURN .F.
ENDIF

IF gfSeek(lcFrsct,'FRCSTHDR')
  SELECT 'FRCSTHDR'
  gfDelete()
  gfTableUpdate()
  IF gfSeek(lcFrsct,'FRCSTDTL')
    SELECT FRCSTDTL
    SCAN REST WHILE CFORECSTID+STYLE = lcFrsct
      gfDelete()
    ENDSCAN
    gfTableUpdate()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfCalcOts
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : calc. OTS fucntion
*!*************************************************************
FUNCTION  lfCalcOts
PARAMETERS loFormSet

IF loFormSet.lnMaxScale = 0
  RETURN
ENDIF

SELECT(loFormSet.lcStyleInv)
LOCATE
lcStyle = IIF(loFormSet.ActiveMode $ 'VE',EVALUATE(loFormSet.lcFormStyles+'.cFrcstID')+PADR( EVALUATE(loFormSet.lcFormStyles+'.Style'),19),PADR(EVALUATE(loFormSet.lcFormStyles+'.Style'),19))



IF !SEEK(ALLTRIM(lcStyle),loFormSet.lcStyleInv)
  RETURN
ENDIF

SCAN REST WHILE IIF(loFormSet.ActiveMode $'EV',cFrcstID+STYLE+SCALE+cTYPE,STYLE+SCALE)= lcStyle FOR CTYPE = 'A'
  lnRecNo = RECNO()
  lcScale = SCALE
  FOR lnE = 1 TO 8
    lcE = STR(lnE,1)
    STORE 0 TO M.OTS&lcE.
  ENDFOR
  IF SEEK(lcStyle +lcScale +'B',loFormSet.lcStyleInv)
    FOR lnE = 1 TO 8
      lcE = STR(lnE,1)
      STORE VAL(QTY&lcE.)TO M.OTS&lcE.
    ENDFOR
  ENDIF


  IF SEEK(lcStyle +lcScale+ 'C',loFormSet.lcStyleInv)
    FOR lnE = 1 TO 8
      lcE = STR(lnE,1)
      m.OTS&lcE. = M.OTS&lcE. + VAL(QTY&lcE.)
    ENDFOR
  ENDIF

  IF SEEK(lcStyle +lcScale+'D',loFormSet.lcStyleInv)
    FOR lnE = 1 TO 8
      lcE = STR(lnE,1)
      m.OTS&lcE. = M.OTS&lcE. - VAL(QTY&lcE.)
    ENDFOR
  ENDIF

  IF SEEK(lcStyle +lcScale +'E',loFormSet.lcStyleInv)
    m.Tqty = STR(0,1)
    FOR lnE = 1 TO 8
      lcE = STR(lnE,1)
      REPLACE QTY&lcE. WITH STR(M.OTS&lcE.,7)
      m.Tqty = STR(VAL(m.Tqty) + M.OTS&lcE.,9)
    ENDFOR

    *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[Start]
    lnOldQty = VAL(TQTY)
    *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[End]

    REPLACE Tqty  WITH m.Tqty

    *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[Start]
    *REPLACE TotOts WITH TotOts + VAL(m.TQty) IN (loFormSet.lcFormStyles)
    REPLACE TotOts WITH TotOts + VAL(m.TQty)-lnOldQty IN (loFormSet.lcFormStyles)
    *: B608743,1 MMT 11/19/2008 Fix bug of wrong OTS in Add Mode[End]

    IF SEEK(lcStyle +'E',loFormSet.lcStyScl)
      REPLACE TOTQTY WITH EVALUATE(loFormSet.lcFormStyles+'.TotOts') IN (loFormSet.lcStyScl)
      lfCalTotCost(loFormSet)
      lfCaltotPric(loFormSet)
    ENDIF
  ENDIF

  IF SEEK(lcStyle +lcScale+'F',loFormSet.lcStyleInv)
    FOR lnE = 1 TO 8
      lcE = STR(lnE,1)
      m.OTS&lcE. = M.OTS&lcE. - VAL(QTY&lcE.)
    ENDFOR
  ENDIF

  IF SEEK(lcStyle +lcScale+'G',loFormSet.lcStyleInv)
    m.Tqty = STR(0,1)
    FOR lnE = 1 TO 8
      lcE = STR(lnE,1)
      REPLACE QTY&lcE. WITH STR(M.OTS&lcE.,7)
      m.Tqty = STR(VAL(m.Tqty) + M.OTS&lcE.,9)
    ENDFOR
    lnOldTqty = VAL(Tqty)
    REPLACE Tqty  WITH m.Tqty
    IF SEEK(lcStyle +'G',loFormSet.lcStyScl)
      REPLACE TOTQTY WITH TOTQTY+(VAL(m.Tqty)-lnOldTqty) IN (loFormSet.lcStyScl)
      lfCalTotCost(loFormSet)
      lfCaltotPric(loFormSet)
    ENDIF
  ENDIF


  IF SEEK(lcStyle +lcScale+ 'J',loFormSet.lcStyleInv)
    m.Tqty = STR(0,1)
    FOR lnE = 1 TO 8
      lcE = STR(lnE,1)
      m.Tqty =STR( VAL(m.Tqty) + VAL(EVALUATE(loFormSet.lcStyleInv+'.QTY'+lcE)),9)
    ENDFOR
    lnOldQty = VAL(Tqty)
    REPLACE Tqty  WITH m.Tqty
    IF loFormSet.ActiveMode = 'V'
      REPLACE Currpln WITH  Currpln +VAL(m.Tqty) IN  (loFormSet.lcFormStyles)
    ELSE
      REPLACE Currpln WITH  Currpln +(VAL(m.Tqty)-lnOldQty) IN  (loFormSet.lcFormStyles)
    ENDIF

    IF SEEK(lcStyle  +'J',loFormSet.lcStyScl)
      REPLACE TOTQTY WITH EVALUATE(loFormSet.lcFormStyles+'.Currpln') IN (loFormSet.lcStyScl)
      lfCalTotCost(loFormSet)
      lfCaltotPric(loFormSet)
    ENDIF
  ENDIF

  IF BETWEEN(lnRecNo ,1,RECCOUNT())
    GO RECORD lnRecNo
  ENDIF
ENDSCAN


*!*************************************************************
*! Name      : lfChkFld
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Check if field should be enabled
*!*************************************************************
FUNCTION lfChkFld
PARAMETERS loFormset

IF loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.ACTIVECOLUMN = 0
  RETURN
ENDIF

lcCurCol = ALLTRIM(STR(loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.ACTIVECOLUMN-1))
IF !(loFormSet.ActiveMode $'EA')
  RETURN
ENDIF
IF !(EVALUATE(loFormSet.lcStyleInv+'.ctype') $ 'JF')
  RETURN
ENDIF
lnRecn = RECNO(loFormSet.lcStyleInv)
lcStyle = IIF(loFormSet.ActiveMode $'EV',EVALUATE(loFormSet.lcStyleInv+'.cFrcstID'),'')+EVALUATE(loFormSet.lcStyleInv+'.Style')+EVALUATE(loFormSet.lcStyleInv+'.Scale')

IF SEEK(lcStyle+'A',loFormSet.lcStyleInv)
  IF EMPTY(EVALUATE(loFormSet.lcStyleInv+'.QTY'+lcCurCol))
    KEYBOARD '{TAB}'
  ENDIF
ENDIF

IF BETWEEN(lnRecn ,1,RECCOUNT())
  GO RECORD lnRecn  IN (loFormSet.lcStyleInv)
ENDIF

*!*************************************************************
*! Name      : lfChkTot
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Check TOTAL Button caption
*!*************************************************************
FUNCTION lfChkTot
PARAMETERS loformSet
IF loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.RECORDSOURCE = loFormSet.lcStyScl
  loFormSet.llTotView = .T.
  loFormSet.ariaForm1.pgfFrcst.pgFrcst.cmdTotals.CAPTION = 'Details View'
ELSE
  IF loFormSet.ariaForm1.pgfFrcst.pgFrcst.grdStyDet.RECORDSOURCE = loFormSet.lcStyleInv
    loFormSet.llTotView = .F.
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.cmdTotals.CAPTION = 'Totals View'
  ENDIF
ENDIF

lcStyleCurnt = PADR(EVALUATE(loFormSet.lcStyleInv+'.Style'),19)
IF loFormSet.ActiveMode $ 'EV'
  lcCurFr = EVALUATE(loFormSet.lcStyleInv+'.cFrcstID')
  lcStyleCurnt = lcCurFr + lcStyleCurnt
ENDIF

lnCntRec = RECNO(loFormSet.lcStyScl)

IF SEEK(lcStyleCurnt + 'J',loFormSet.lcStyScl)
  IF EVALUATE(loFormSet.lcStyScl+'.TOTQty') < EVALUATE(loFormSet.lcFormStyles+'.NMINQTY')
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.txtOrdQty.DISABLEDBACKCOLOR = RGB(255,0,0)
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.txtOrdQty.DISABLEDFORECOLOR = RGB(0,0,0)
  ELSE
    loFormSet.ariaForm1.pgfFrcst.pgFrcst.txtOrdQty.DISABLEDBACKCOLOR = RGB(255,255,255)
  ENDIF
ENDIF

IF BETWEEN(lnCntRec ,1,RECCOUNT(loFormSet.lcStyScl))
  GO RECORD lnCntRec IN (loFormSet.lcStyScl)
ENDIF


*!*************************************************************
*! Name      : lfexpColor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!*************************************************************
FUNCTION lfexpColor
LPARAMETERS lcCursorName, lcColumnsArray, lnHeaderHeight, lnRowHeight, lcColorShadesArray, ;
  lcFontName, lnSize, llBold, llItalic, llUnderline




DIMENSION laColumns[ALEN(&lcColumnsArray., 1), 5]
ACOPY(&lcColumnsArray., laColumns)


lcFileName = GETFILE('XLS','',"Save")
IF lcFileName == ''
  RETURN
ENDIF

LOCAL lnFileNo
lnFileNo = FCREATE(lcFileName)

IF (lnFileNo = -1)
  MESSAGEBOX("Error occurs while creating the file, please check you have the access for this file or folder.", 16, oAriaApplication.SystemName)
  RETURN
ENDIF


lfWriteXMLHeader(lnFileNo)

*-- Styles Section
FWRITE(lnFileNo, ' <Styles>' + CHR(13) + CHR(10))

lfAddDefaultStyle(lnFileNo, 'C')
lfAddDefaultStyle(lnFileNo, 'M')
lfAddDefaultStyle(lnFileNo, 'L')
lfAddDefaultStyle(lnFileNo, 'N')
lfAddDefaultStyle(lnFileNo, 'D')
*! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
lfAddDefaultStyle(lnFileNo, 'X')
lfAddDefaultStyle(lnFileNo, 'Y')
*! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]

lfAddColorStyle(lnFileNo, 'C', RGB(128,128,128))
lfAddColorStyle(lnFileNo, 'M', RGB(128,128,128))
lfAddColorStyle(lnFileNo, 'L', RGB(128,128,128))
lfAddColorStyle(lnFileNo, 'N', RGB(128,128,128))
lfAddColorStyle(lnFileNo, 'D', RGB(128,128,128))


lfAddColorStyle(lnFileNo, 'C', RGB(64,128,128))
lfAddColorStyle(lnFileNo, 'M', RGB(64,128,128))
lfAddColorStyle(lnFileNo, 'L',RGB(64,128,128))
lfAddColorStyle(lnFileNo, 'N', RGB(64,128,128))
lfAddColorStyle(lnFileNo, 'D', RGB(64,128,128))

lfAddColorStyle(lnFileNo, 'C', RGB(255,0,0))
lfAddColorStyle(lnFileNo, 'M', RGB(255,0,0))
lfAddColorStyle(lnFileNo, 'L',RGB(255,0,0))
lfAddColorStyle(lnFileNo, 'N', RGB(255,0,0))
lfAddColorStyle(lnFileNo, 'D', RGB(255,0,0))


*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
IF loFormSet.ariaForm1.pgfFrcst.pgFrcst.GRDSTYDETTOOLBAR.cboColor.LISTINDEX > 2
  LOCAL laColorVal[1, 1]
  =gfSubStr(loFormSet.ariaForm1.pgfFrcst.pgFrcst.GRDSTYDETTOOLBAR.Colorschemes[loFormSet.ariaForm1.pgfFrcst.pgFrcst.GRDSTYDETTOOLBAR.cboColor.ListIndex - 2, 2], @laColorVal, '|')
  IF loFormSet.ariaForm1.pgfFrcst.pgFrcst.GRDSTYDETTOOLBAR.cboColor.LISTINDEX > 2 .AND. !EMPTY(laColorVal[1, 1])
    LOCAL lnCond
    FOR lnCond = 1 TO ALEN(laColorVal,1)
      IF MOD(lnCond,2) =0
        lfAddColorStyle(lnFileNo, 'C', VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'M', VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'L',VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'N', VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'D',VAL(laColorVal[lnCond, 1]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF
IF loFormSet.ariaForm1.pgfFrcst.pgHeadr.GRDSTYLESTOOLBAR.cboColor.LISTINDEX > 2
  LOCAL laColorVal[1, 1]
  =gfSubStr(loFormSet.ariaForm1.pgfFrcst.pgHeadr.GRDSTYLESTOOLBAR.Colorschemes[loFormSet.ariaForm1.pgfFrcst.pgHeadr.GRDSTYLESTOOLBAR.cboColor.ListIndex - 2, 2], @laColorVal, '|')
  IF loFormSet.ariaForm1.pgfFrcst.pgHeadr.GRDSTYLESTOOLBAR.cboColor.LISTINDEX > 2 .AND. !EMPTY(laColorVal[1, 1])
    LOCAL lnCond
    FOR lnCond = 1 TO ALEN(laColorVal,1)
      IF MOD(lnCond,2) =0
        lfAddColorStyle(lnFileNo, 'C', VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'M', VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'L',VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'N', VAL(laColorVal[lnCond, 1]))
        lfAddColorStyle(lnFileNo, 'D',VAL(laColorVal[lnCond, 1]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]


*!*    ENDFOR
*!*  ENDIF

FWRITE(lnFileNo, ' </Styles>' + CHR(13) + CHR(10))

*-- Add Table Start
FWRITE(lnFileNo, ' <Worksheet ss:Name="Sheet1">'  + CHR(13) + CHR(10))

FWRITE(lnFileNo, '  <Table ss:ExpandedColumnCount="' + ALLTRIM(STR(ALEN(laColumns, 1))) + '" ' + ;
  'ss:ExpandedRowCount="' + ALLTRIM(STR(lfGetRowsCount())) + '" ' + ;
  'x:FullColumns="1" x:FullRows="1">'  + CHR(13) + CHR(10))



LOCAL lnColumn
FOR lnColumn = 1 TO ALEN(laColumns, 1)
  lfAddColumn(lnFileNo, laColumns[lnColumn, 4])
ENDFOR

IF loFormSet.llTotView
  lfAddHeaderRow(lnFileNo, lnHeaderHeight)
ENDIF

*-- Add Rows
LOCAL lnSelected
lnSelected = SELECT()

LOCAL lnRecNo
lnRecNo = RECNO()

LOCAL lnRowCount
lnRowCount = lfGetRowsCount()

LOCAL lnRowNo
lnRowNo = 0

SELECT(lcCursorName)
SCAN
  *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
  *lfAddRow(lnFileNo, lnRowHeight)
  lfAddRow(lnFileNo, lnRowHeight,lnRowNo + 1)
  *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]
  lnRowNo = lnRowNo + 1
ENDSCAN

SELECT(lnSelected)
TRY
  GOTO RECORD lnRecNo
CATCH
ENDTRY

*-- Add Table End
FWRITE(lnFileNo, '  </Table>'  + CHR(13) + CHR(10))

lfWriteXMLFooter(lnFileNo)

FCLOSE(lnFileNo)

*!*************************************************************
*! Name      : lfWriteXMLHeader
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!*************************************************************
FUNCTION lfWriteXMLHeader
LPARAMETERS lnFileNo

FWRITE(lnFileNo, ;
  '<?xml version="1.0"?>' + CHR(13) + CHR(10) + ;
  '<?mso-application progid="Excel.Sheet"?>' + CHR(13) + CHR(10) + ;
  '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"' + CHR(13) + CHR(10) + ;
  ' xmlns:o="urn:schemas-microsoft-com:office:office"' + CHR(13) + CHR(10) + ;
  ' xmlns:x="urn:schemas-microsoft-com:office:excel"' + CHR(13) + CHR(10) + ;
  ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' + CHR(13) + CHR(10) + ;
  ' xmlns:html="http://www.w3.org/TR/REC-html40">' + CHR(13) + CHR(10) + ;
  ' <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' + CHR(13) + CHR(10) + ;
  '  <Author>Aria</Author>' + CHR(13) + CHR(10) + ;
  '  <LastAuthor>Aria</LastAuthor>' + CHR(13) + CHR(10) + ;
  '  <Created>2006-10-26T18:39:06Z</Created>' + CHR(13) + CHR(10) + ;
  '  <Company>Aria</Company>' + CHR(13) + CHR(10) + ;
  '  <Version>11.6360</Version>' + CHR(13) + CHR(10) + ;
  ' </DocumentProperties>' + CHR(13) + CHR(10) + ;
  ' <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">' + CHR(13) + CHR(10) + ;
  '  <WindowHeight>7170</WindowHeight>' + CHR(13) + CHR(10) + ;
  '  <WindowWidth>15195</WindowWidth>' + CHR(13) + CHR(10) + ;
  '  <WindowTopX>0</WindowTopX>' + CHR(13) + CHR(10) + ;
  '  <WindowTopY>60</WindowTopY>' + CHR(13) + CHR(10) + ;
  '  <ProtectStructure>False</ProtectStructure>' + CHR(13) + CHR(10) + ;
  '  <ProtectWindows>False</ProtectWindows>' + CHR(13) + CHR(10) + ;
  ' </ExcelWorkbook>' + CHR(13) + CHR(10))


*!*************************************************************
*! Name      : lfAddDefaultStyle
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION lfAddDefaultStyle
LPARAMETERS lnFileNo, lcDataType
FWRITE(lnFileNo, '  <Style ss:ID="Default' + lcDataType + '" ss:Name="Normal' + lcDataType + '">' + CHR(13) + CHR(10))
FWRITE(lnFileNo, '   <Alignment ss:Vertical="Bottom"/>' + CHR(13) + CHR(10))
FWRITE(lnFileNo, '   <Borders/>' + CHR(13) + CHR(10))

FWRITE(lnFileNo, '   <Font ss:FontName="' + lcFontName+ '" ' + ;
  'ss:Size="' + ALLTRIM(STR(lnSize)) + '" ' + ;
  IIF(llBold, 'ss:Bold="1" ', ' ') + ;
  IIF(llItalic, 'ss:Italic="1" ', ' ') + ;
  IIF(llUnderline, 'ss:Underline="Single" ', '') + ;
  '/>' + CHR(13) + CHR(10))


FWRITE(lnFileNo, '   <Interior/>' + CHR(13) + CHR(10))

DO CASE
CASE lcDataType = 'C'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="@"/>' + CHR(13) + CHR(10))

CASE lcDataType = 'M'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="@"/>' + CHR(13) + CHR(10))

CASE lcDataType = 'L'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="@"/>' + CHR(13) + CHR(10))

CASE lcDataType = 'N'
  FWRITE(lnFileNo, '   <NumberFormat/>' + CHR(13) + CHR(10))

CASE lcDataType = 'D'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="Short Date"/>' + CHR(13) + CHR(10))
  *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
CASE lcDataType = 'X'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="0"/>' + CHR(13) + CHR(10))
CASE lcDataType = 'Y'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="0.00"/>' + CHR(13) + CHR(10))
  *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]
ENDCASE

FWRITE(lnFileNo, '   <Protection/>' + CHR(13) + CHR(10))
FWRITE(lnFileNo, '  </Style>' + CHR(13) + CHR(10))


*!*************************************************************
*! Name      : lfGetRowsCount
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION lfGetRowsCount
LOCAL lnSelected

LOCAL lnCount
lnCount = 0

lnSelected = SELECT()

SELECT(lcCursorName)
SCAN
  lnCount = lnCount + 1
ENDSCAN
SELECT(lnSelected)

RETURN lnCount + IIF(loFormSet.llTotView,1,0)
*!*************************************************************
*! Name      : lfAddColumn
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION lfAddColumn
LPARAMETERS lnFileNo, lnWidth

FWRITE(lnFileNo, '   <Column ss:AutoFitWidth="0" ss:Width="' + ALLTRIM(STR(lnWidth, 10, 2)) + '"/>' + CHR(13) + CHR(10))

*!*************************************************************
*! Name      : lfAddRow
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION lfAddRow
*! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
*LPARAMETERS lnFileNo, lnRowHeight
LPARAMETERS lnFileNo, lnRowHeight,lnRowCont
*! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]

IF TYPE('lnRowHeight') = 'L'
  FWRITE(lnFileNo, '   <Row ss:AutoFitHeight="0">' + CHR(13) + CHR(10))
ELSE
  FWRITE(lnFileNo, '   <Row ss:AutoFitHeight="0" ss:Height="' + ALLTRIM(STR(lnRowHeight, 10, 2)) + '">' + CHR(13) + CHR(10))
ENDIF

*! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020] [Begin]
llQtySz = .F.
LOCAL lnColumn
TRY 
  llQtySz = ('QTY/SIZE' $ UPPER(EVALUATE(laColumns[1, 3])))
CATCH
ENDTRY 
*! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020] [End  ]

FOR lnColumn = 1 TO ALEN(laColumns, 1)
  *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
  *FWRITE(lnFileNo, '    <Cell ss:StyleID="' + lfGetCellStyle(lnColumn) + '"><Data ss:Type=')
  LOCAL lcType
  lcType = IIF(laColumns[lnColumn, 2] = "U", "C", laColumns[lnColumn, 2])
  DO CASE
  *! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020][Begin]  
  *CASE lnRowCont <> 1 AND ('QTY' $ UPPER(laColumns[lnColumn, 3]))
  CASE lnRowCont <> 1 AND ('QTY' $ UPPER(laColumns[lnColumn, 3])) AND !llQtySz
  *! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020][End  ]  
    FWRITE(lnFileNo, '    <Cell ss:StyleID="DefaultX"><Data ss:Type=')
    
  *! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020] [Begin]  
  *CASE lnRowCont <> 1 AND ('PRICE' $ UPPER(laColumns[lnColumn, 3]) OR 'COST' $ UPPER(laColumns[lnColumn, 3]) OR 'TOTAL' $ UPPER(laColumns[lnColumn, 3])) 
  CASE lnRowCont <> 1 AND ('PRICE' $ UPPER(laColumns[lnColumn, 3]) OR 'COST' $ UPPER(laColumns[lnColumn, 3]) OR 'TOTAL' $ UPPER(laColumns[lnColumn, 3])) AND !llQtySz
  *! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020] [End]
    FWRITE(lnFileNo, '    <Cell ss:StyleID="DefaultY"><Data ss:Type=')
  OTHERWISE
    FWRITE(lnFileNo, '    <Cell ss:StyleID="' + lfGetCellStyle(lnColumn) + '"><Data ss:Type=')
  ENDCASE
  *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]

  DO CASE
  CASE lcType = 'C'
    *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
    *! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020] [Begin]  
    *IF lnRowCont <> 1 AND ('QTY' $ UPPER(laColumns[lnColumn, 3]) OR 'PRICE' $ UPPER(laColumns[lnColumn, 3]) OR 'COST' $ UPPER(laColumns[lnColumn, 3]) OR 'TOTAL' $ UPPER(laColumns[lnColumn, 3]))
    IF (!llQtySz) AND (lnRowCont <> 1 AND ('QTY' $ UPPER(laColumns[lnColumn, 3]) OR 'PRICE' $ UPPER(laColumns[lnColumn, 3]) OR 'COST' $ UPPER(laColumns[lnColumn, 3]) OR 'TOTAL' $ UPPER(laColumns[lnColumn, 3])))    
    *! B610084,1 HIA 09/17/2012 Fix Column type in excel [T20120501.0020] [End]  
      FWRITE(lnFileNo, '"Number">')
      lnExpValue = EVAL(laColumns[lnColumn, 3])
      FWRITE(lnFileNo, ALLTRIM(lnExpValue))
      FWRITE(lnFileNo, '</Data></Cell>' + CHR(13) + CHR(10))
      LOOP
    ELSE
      *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]

      FWRITE(lnFileNo, '"String">')

      *B608632,1 WAM 07/24/2008 Fix bug in export style major browse when column value reads from expression
      *FWRITE(lnFileNo, THISFORM.GetXmlValue(ALLTRIM(EVAL(THISFORM.Columns[lnColumn, 3]))))

      lnExpValue = EVAL(laColumns[lnColumn, 3])
      IF TYPE('lnExpValue')='N'
        FWRITE(lnFileNo, ALLTRIM(STR(lnExpValue)))
      ELSE
        FWRITE(lnFileNo, lfGetXmlValue(ALLTRIM(lnExpValue)))
      ENDIF
      *B608632,1 WAM 07/24/2008 (End)

      *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
    ENDIF
    *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]
  CASE lcType = 'M'
    FWRITE(lnFileNo, '"String">')
    FWRITE(lnFileNo, lfGetXmlValue(ALLTRIM(EVAL(laColumns[lnColumn, 3]))))

  CASE lcType = 'L'
    FWRITE(lnFileNo, '"String">')
    FWRITE(lnFileNo, IIF(EVAL(laColumns[lnColumn, 3]), 'True', 'False'))

  CASE lcType = 'N'
    *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][Start]
    *FWRITE(lnFileNo, '"String">')
    *FWRITE(lnFileNo, ALLTRIM(STR(EVAL(laColumns[lnColumn, 3]))))
    IF ('QTY' $ UPPER(laColumns[lnColumn, 3]) OR 'PRICE' $ UPPER(laColumns[lnColumn, 3]) OR 'COST' $ UPPER(laColumns[lnColumn, 3]) OR 'SELL' $ UPPER(laColumns[lnColumn, 3]))
      FWRITE(lnFileNo, '"Number">')
      lnExpValue = EVAL(laColumns[lnColumn, 3])
      FWRITE(lnFileNo, IIF(TYPE('lnExpValue') == 'C', ALLTRIM(lnExpValue), ALLTRIM(STR(lnExpValue,9,2))))
      FWRITE(lnFileNo, '</Data></Cell>' + CHR(13) + CHR(10))
      LOOP
    ELSE
      FWRITE(lnFileNo, '"String">')
      lnExpValue = EVAL(laColumns[lnColumn, 3])
      IF TYPE('lnExpValue')='N'
        FWRITE(lnFileNo, ALLTRIM(STR(lnExpValue)))
      ELSE
        FWRITE(lnFileNo, lfGetXmlValue(ALLTRIM(lnExpValue)))
      ENDIF
    ENDIF
    *! B609926,1 SAB 05/17/2012 Fix Numeric fields in Style Forcasting [T20120501.0020][End]

  CASE lcType = 'D'
    IF YEAR(EVAL(laColumns[lnColumn, 3])) > 0
      FWRITE(lnFileNo, '"DateTime">')

      LOCAL lcDate
      lcDate = ALLTRIM(STR(YEAR(EVAL(laColumns[lnColumn, 3])), 4, 0)) + '-' + ;
        PADL(ALLTRIM(STR(MONTH(EVAL(laColumns[lnColumn, 3])), 2, 0)), 2, '0') + '-' + ;
        PADL(ALLTRIM(STR(DAY(EVAL(laColumns[lnColumn, 3])), 2, 0)), 2, '0') + ;
        'T00:00:00.000'
      lcDate = STRTRAN(lcDate, ' ', '0', -1, -1, 1)

      FWRITE(lnFileNo, lcDate)
    ELSE
      FWRITE(lnFileNo, '"String">')
    ENDIF
  ENDCASE

  FWRITE(lnFileNo, '    </Data></Cell>' + CHR(13) + CHR(10))
ENDFOR

FWRITE(lnFileNo, '   </Row>' + CHR(13) + CHR(10))


*!*************************************************************
*! Name      : lfGetXmlValue
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION lfGetXmlValue
LPARAMETERS lcValue

lcValue = STRTRAN(lcValue, '&', '&amp;', -1, -1, 1)
lcValue = STRTRAN(lcValue, "'", '&apos;', -1, -1, 1)
lcValue = STRTRAN(lcValue, '"', '&quot;', -1, -1, 1)
lcValue = STRTRAN(lcValue, '<', '&lt;', -1, -1, 1)
lcValue = STRTRAN(lcValue, '>', '&gt;', -1, -1, 1)

RETURN lcValue

*!*************************************************************
*! Name      : lfGetCellStyle
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION lfGetCellStyle
LPARAMETERS lnColumn
laColumns[lnColumn, 5] =STRTRAN(UPPER(laColumns[lnColumn, 5]),'THISFORMSET','loFormset')
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[Start]
IF 'GRDSTYDETTOOLBAR' $ UPPER(laColumns[lnColumn, 5]) AND 'THIS.PARENT' $ UPPER(laColumns[lnColumn, 5])
  laColumns[lnColumn, 5] =STRTRAN(UPPER(laColumns[lnColumn, 5]),'THIS.PARENT','loFormSet.ariaForm1.pgfFrcst.pgFrcst')
ENDIF
IF 'GRDSTYLESTOOLBAR' $ UPPER(laColumns[lnColumn, 5]) AND 'THIS.PARENT' $ UPPER(laColumns[lnColumn, 5])
  laColumns[lnColumn, 5] =STRTRAN(UPPER(laColumns[lnColumn, 5]),'THIS.PARENT','loFormSet.ariaForm1.pgfFrcst.pgHeadr')
ENDIF
*! B609118,1 MMT 12/22/2009 Modify Code to work witg grid toolbar options[End]

*-- Check if the back color is white
IF EMPTY(laColumns[lnColumn, 5]) OR "16777215" == ALLTRIM(STR(EVALUATE(laColumns[lnColumn, 5])))
  RETURN "Default" + IIF(laColumns[lnColumn, 2] = "U", "C", laColumns[lnColumn, 2])
ELSE
  RETURN "S" + ALLTRIM(STR(EVALUATE(laColumns[lnColumn, 5]))) + ;
    IIF(laColumns[lnColumn, 2] = "U", "C", laColumns[lnColumn, 2])
ENDIF

*!*************************************************************
*! Name      : lfWriteXMLFooter
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION  lfWriteXMLFooter
LPARAMETERS lnFileNo

FWRITE(lnFileNo, ;
  '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">' + CHR(13) + CHR(10) + ;
  '   <Print>' + CHR(13) + CHR(10) + ;
  '    <ValidPrinterInfo/>' + CHR(13) + CHR(10) + ;
  '    <PaperSizeIndex>9</PaperSizeIndex>' + CHR(13) + CHR(10) + ;
  '    <HorizontalResolution>1200</HorizontalResolution>' + CHR(13) + CHR(10) + ;
  '    <VerticalResolution>1200</VerticalResolution>' + CHR(13) + CHR(10) + ;
  '   </Print>' + CHR(13) + CHR(10) + ;
  '   <Selected/>' + CHR(13) + CHR(10) + ;
  '   <Panes>' + CHR(13) + CHR(10) + ;
  '    <Pane>' + CHR(13) + CHR(10) + ;
  '     <Number>3</Number>' + CHR(13) + CHR(10) + ;
  '     <ActiveRow>1</ActiveRow>' + CHR(13) + CHR(10) + ;
  '     <ActiveCol>1</ActiveCol>' + CHR(13) + CHR(10) + ;
  '    </Pane>' + CHR(13) + CHR(10) + ;
  '   </Panes>' + CHR(13) + CHR(10) + ;
  '   <ProtectObjects>False</ProtectObjects>' + CHR(13) + CHR(10) + ;
  '   <ProtectScenarios>False</ProtectScenarios>' + CHR(13) + CHR(10) + ;
  '  </WorksheetOptions>' + CHR(13) + CHR(10) + ;
  ' </Worksheet>' + CHR(13) + CHR(10) + ;
  '</Workbook>' + CHR(13) + CHR(10))


*!*************************************************************
*! Name      : lfAddColorStyle
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!**************************************************************
FUNCTION lfAddColorStyle
LPARAMETERS lnFileNo, lcDataType, lnColor

FWRITE(lnFileNo, '  <Style ss:ID="S' + ALLTRIM(STR(lnColor)) + lcDataType + '" ' + ;
  'ss:Name="s' + ALLTRIM(STR(lnColor)) + lcDataType + '">' + CHR(13) + CHR(10))

FWRITE(lnFileNo, '   <Alignment ss:Vertical="Bottom"/>' + CHR(13) + CHR(10))
FWRITE(lnFileNo, '   <Borders/>' + CHR(13) + CHR(10))


FWRITE(lnFileNo, '   <Font ss:FontName="' + lcFontName + '" ' + ;
  'ss:Size="' + ALLTRIM(STR(lnSize)) + '" ' + ;
  IIF(llBold , 'ss:Bold="1" ', ' ') + ;
  IIF(llItalic, 'ss:Italic="1" ', ' ') + ;
  IIF(llUnderline, 'ss:Underline="Single" ', '') + ;
  '/>' + CHR(13) + CHR(10))


LOCAL lcHex
lcHex = ALLTRIM(lfDecToHex(lnColor))
lcHex = LEFT("000000", 6 - LEN(lcHex)) + lcHex
lcHex = SUBSTR(lcHex, 5, 2) + SUBSTR(lcHex, 3, 2) + SUBSTR(lcHex, 1, 2)

FWRITE(lnFileNo, '   <Interior ss:Color="#' + lcHex + '" ss:Pattern="Solid"/>'  + CHR(13) + CHR(10))

DO CASE
CASE lcDataType = 'C'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="@"/>'  + CHR(13) + CHR(10))

CASE lcDataType = 'M'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="@"/>'  + CHR(13) + CHR(10))

CASE lcDataType = 'L'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="@"/>'  + CHR(13) + CHR(10))

CASE lcDataType = 'N'
  FWRITE(lnFileNo, '   <NumberFormat/>'  + CHR(13) + CHR(10))

CASE lcDataType = 'D'
  FWRITE(lnFileNo, '   <NumberFormat ss:Format="Short Date"/>'  + CHR(13) + CHR(10))
ENDCASE

FWRITE(lnFileNo, '   <Protection/>' + CHR(13) + CHR(10))

FWRITE(lnFileNo, '  </Style>'  + CHR(13) + CHR(10))

*!*************************************************************
*! Name      : lfDecToHex
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!******************************************************************
FUNCTION lfDecToHex
LPARAMETERS lnNum

DECLARE INTEGER wnsprintf IN Shlwapi STRING @lpOut, INTEGER cchLimitIn, STRING pszFmt, INTEGER

LOCAL lnResult, lcResult
lcResult = SPACE(20)
lnResult = wnsprintf(@lcResult, 20, "%x", lnNum)

RETURN LEFT(lcResult, lnResult)

*!*************************************************************
*! Name      : lfAddHeaderRow
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/19/2008
*! Purpose   : Export function related
*!******************************************************************
FUNCTION lfAddHeaderRow
LPARAMETERS lnFileNo, lnRowHeight

IF TYPE('lnRowHeight') = 'L'
  FWRITE(lnFileNo, '   <Row ss:AutoFitHeight="0">' + CHR(13) + CHR(10))
ELSE
  FWRITE(lnFileNo, '   <Row ss:AutoFitHeight="0" ss:Height="' + ALLTRIM(STR(lnRowHeight, 10, 2)) + '">' + CHR(13) + CHR(10))
ENDIF

LOCAL lnColumn
FOR lnColumn = 1 TO ALEN(laColumns, 1)
  FWRITE(lnFileNo, '    <Cell ss:StyleID="DefaultC"><Data ss:Type="String">')

  FWRITE(lnFileNo, lfGetXmlValue(laColumns[lnColumn, 1]))
  FWRITE(lnFileNo, '    </Data></Cell>' + CHR(13) + CHR(10))
ENDFOR

FWRITE(lnFileNo, '   </Row>' + CHR(13) + CHR(10))
