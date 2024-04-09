*:**************************************************************************
*: Program file  : ICDATSWR.PRG
*: Program desc. : Custom DATE SENSITIVE INVENTORY REPORT FOR (WRE10)
*: Date          : 08/29/2011
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Mariam Mazhar {MMT} 
*:**************************************************************************
*: Calls :  
*:         Procedures : lpCollData
*:             
*:         Functions  : lfwRepWhen()
*:                    : lfItmPos()
*:                    : lfNoRecord()
*:                    : gfModalGen()
*:                    : gfOpenFile()
*:                    : gfItemMask()
*:                    : gfBroWWare()
*:                    : gfStyBrw()
*:                    : FaBrow()
*:                    : lfCreatFil() 
*:                    : lfNonMaj()
*:                    : lfSRVSty()
*:                    : lfStySum()
*:                    : lfvStyle()
*:                    : lfvFabric()
*:                    : lfFillAray() 
*:                    : lfvWareHos() 
*:                    : lfwOldWare() 
*:                    : lfvSortBy()
*:                    : lfGetScale()
*:                    : lfvPrnWhD()
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*: Notes   : .... 
*:**************************************************************************
*C201380.122,C201381.exe{T20110809.0015}
*:**************************************************************************
lcStTime = TIME()       && To store the current time
loOgScroll.cCRorientation = 'P'


*--Initial the variables used in the program.
STORE 0 TO lnScaLnGl , lnScaPosGl
=lfChkStrct() && Get the length of the style , color and scale.
*-- check first of Sort by WareHouse and Empty of Warehouse Code



*-- If User changed the Filter in OG 
IF loOGScroll.llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcStyleTmp) AND RECCOUNT(lcStyleTmp) > 0
    = lfCreatFil()
  ENDIF
  DO lpCollData   && collect the data to the temporary file  
ENDIF   

SELECT (lcStyleTmp)

*B608783,1 WAM 01/05/2008 Fix report filter when the option 'Show Zero Qty' is set to 'No' 
*LOCATE 
IF llRpShzero
  lcExpr = '(ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0) OR ;
            ((ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) = 0) AND nStkVal<>0)'
ELSE
  lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0'
ENDIF

*! B609145,1 MMT  02/14/2010 Convert Option 'Suppress 0 Qty Trans.' from Aria27[Start]
IF llRpSprZQt
  lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0'
ENDIF
*! B609145,1 MMT  02/14/2010 Convert Option 'Suppress 0 Qty Trans.' from Aria27[End]

LOCATE FOR &lcExpr
*B608783,1 WAM 01/05/2008 (End)

IF EOF()             && if end of file (no records match criteria)
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ELSE
  *B608783,1 WAM 01/05/2008 Fix report filter when the option 'Show Zero Qty' is set to 'No' 
*!*	  STORE .F. TO lllink2gl,llGnglcst
*!*	  lllink2gl = IIF(gfGetMemVar('M_LINK_GL')='Y',.T.,.F.)
*!*	  llGnglcst = IIF(gfGetMemVar('M_GL_COST')='Y',.T.,.F.)
*!*	  IF lllink2gl AND llGnglcst
*!*	    lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0 OR ;
*!*	              (ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) = 0 AND nStkVal<>0)'
*!*	  ELSE
*!*	    lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0'
*!*	  ENDIF
  *B608783,1 WAM 01/05/2008 (End)

  STORE SPACE(0) TO lcScalVal,lcOrder
  STORE 0 TO lnTotStk,lnRec,lnRecn
  SELE STYLE
  lnRecn=RECNO()
  lcOrder=Order()
  gfSetOrder('STYLE')
  SELECT (lcStyleTmp)
  lnRec=RECNO()
  IF lcRpSortBy = 'W' 
    *! B609375,1 MMT 08/08/2010 Date Sensitive Inventory report deos not print style major[Start]
    *SCAN
    SCAN FOR &lcExpr
    *! B609375,1 MMT 08/08/2010 Date Sensitive Inventory report deos not print style major[End]    
      lnTotStk=ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8)
      =gfSEEK(STYLE,'STYLE')
      IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')+EVAL(lcStyleTmp+'.cwarecode') AND (lnTotStk<>0 OR NSTKVAL<>0)
        REPLACE llPrnColr WITH .T.
        lcScalVal = ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')+EVAL(lcStyleTmp+'.cwarecode')
      ENDIF  
    ENDSCAN
  ELSE
    *! B609375,1 MMT 08/08/2010 Date Sensitive Inventory report deos not print style major[Start]
    *SCAN
    SCAN FOR &lcExpr
    *! B609375,1 MMT 08/08/2010 Date Sensitive Inventory report deos not print style major[End]    
       lnTotStk=ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8)
       =gfSEEK (STYLE,'STYLE')
   
       IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale') AND (lnTotStk<>0 OR NSTKVAL<>0)
      
         REPLACE llPrnColr WITH .T.
         lcScalVal = ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')
       ENDIF  
    ENDSCAN
  ENDIF 
  
ENDIF
SELECT (lcStyleTmp)
LOCATE 
DO gfDispRe WITH EVALUATE('lcRpName') , 'FOR ' + lcExpr
RETURN 
*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh (SSE)
*! Date      : 08/05/99
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfCreatFil()
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen

lnStatPos = lfItmPos('STYLE.STATUS')    && store the Style Status position
laOGFxFlt[lnStatPos,6] = 'A'            && make status target defaulted to "A"

= lfCreatFil()       && to create the temp file 
  =gfOpenTable(oAriaApplication.DataDir+'Style',oAriaApplication.DataDir+'Style','SH')
  =gfOpenTable(oAriaApplication.DataDir+'Styinvjl',oAriaApplication.DataDir+'Styinvjl','SH')
  =gfOpenTable(oAriaApplication.DataDir+'Stydye',oAriaApplication.DataDir+'Stydye','SH')
  =gfOpenTable(oAriaApplication.DataDir+'SCALE',oAriaApplication.DataDir+'SCALE','SH')
  =gfOpenTable(oAriaApplication.DataDir+'WhsLoc',oAriaApplication.DataDir+'WHSLOCST','SH')

IF !llFirst 
	lcSqlStatment   = "SELECT ItemLoc.STYLE,ItemLoc.TOTWIP,ItemLoc.TOTSTK,ItemLoc.TOTORD,WIP1,DYELOT,ITEM.CSTYMAJOR   FROM ItemLoc(INDEX = STYDYE),ITEM(INDEX = CSTYLE) WHERE ITEMLOC.dyelot ='' AND ITEMLOC.STYLE = ITEM.STYLE AND ITEMLOC.CINVTYPE = ITEM.CINVTYPE "
	lcSqlStatment   = lcSqlStatment   +" AND ITEM.CINVTYPE = 0002"
	lcCursorLoc = loOgScroll.gfTempName()
	lcTable ='ItemLoc'
	lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursorLoc,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
	                                      'BROWSE',SET("DATASESSION"))

	IF lnConnectionHandlar >= 1
	  SELECT(lcCursorLoc)
	  lnBuffering = CURSORGETPROP("Buffering",lcCursorLoc)
	   =CURSORSETPROP("Buffering",3,lcCursorLoc)
	  SELECT (lcCursorLoc)
	  INDEX ON CSTYMAJOR TAG &lcCursorLoc
	  SET ORDER TO TAG &lcCursorLoc
	  SET RELATION TO 
	  LOCATE 
	ENDIF 
	llFirst = .T.
ENDIF 

*-- end of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh (SSE)
*! Date      : 08/05/1999
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : Position
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfCreatFil
*! Developer : Sameh (SSE)
*! Date      : 08/05/1999
*! Purpose   : Create temporary file structure.
*!**************************************************************************
*! Called from : OG When function. OR Main PRG
*!**************************************************************************
*! Example   : =lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
*-- If change Sort By from Style to Warehouse or vise versa close the temp file first 
IF USED(lcStyleTmp)
  USE IN (lcStyleTmp)
ENDIF
*-- Var lcFileName to hold the Master File whether it's STYLE or STYDYE
lcFileName = IIF(lcRpSortBy = 'S','STYLE','STYDYE')


SELECT StyInvJl
lnFieldCnt  =AFIELDS(laFileStru) 
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'PRICEA'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 12
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'SCALE'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 3
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'TOTCOST'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 10
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'DESC'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 20
laFileStru[lnNewFld,4] = 0
  
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'Ave_Cost'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 10
laFileStru[lnNewFld,4] = 2


lnNewFld = ALEN(laFileStru,1) + 1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'llWare'
laFileStru[lnNewFld,2] = 'L'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

=lfAddField("laFileStru", "ColrDesc"  , "C" ,25 ,0) &&Field hold the color description.
=lfAddField("laFileStru", "llPrnColr" , "L" ,1  ,0) &&Field check the scale change.
FOR lnCrtTmp = 1 TO 8
  lcNumQty = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("laFileStru", "ScalSz"  + lcNumQty , "C" , 5 ,0)
ENDFOR


FOR lnLoop = 1 TO  16
  STORE ' ' TO  laFileStru[lnFieldCnt +lnLoop,7],laFileStru[lnFieldCnt+lnLoop,8],;
                laFileStru[lnFieldCnt +lnLoop,9],laFileStru[lnFieldCnt+lnLoop,10],;
                laFileStru[lnFieldCnt +lnLoop,11],laFileStru[lnFieldCnt +lnLoop,12],;
                laFileStru[lnFieldCnt +lnLoop,13],laFileStru[lnFieldCnt +lnLoop,14],;
                laFileStru[lnFieldCnt +lnLoop,15],laFileStru[lnFieldCnt+lnLoop,16]
  STORE 0 TO    laFileStru[lnFieldCnt +lnLoop,17] ,laFileStru[lnFieldCnt+lnLoop,18]
ENDFOR   


IF lcRpSortBy = 'W'
  =gfCrtTmp(lcStyleTmp,@laFileStru,'cWareCode +Style',lcStyleTmp,.F.)
ELSE
  =gfCrtTmp(lcStyleTmp,@laFileStru,'Style+cWareCode',lcStyleTmp,.F.)
ENDIF
*-- End of lfCreatFil.
*!**************************************************************************
*! Name      : lpCollData
*! Developer : Khalid Mohi El-Din
*! Date      : 05/23/2000
*! Purpose   : Collect the data from StyInvJl file.
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lpCollData()
*!**************************************************************************
PROCEDURE lpCollData

SELECT StyInvJl
lcOldTag = ORDER('StyInvJl')
gfSetorder('STYDATE')
lcCursorStyle = ''
llStySelect = .F.

lnPosStyle = ASCAN(loOgScroll.laOgVRFlt,"STYLE.CSTYMAJOR")
IF lnPosStyle > 0 
  lnPosStyle = ASUBSCRIPT(loOGScroll.laOgVRFlt,lnPosStyle,1)
  lcCursorStyle= loOgScroll.laOgVRFlt[lnPosStyle,6]
  IF !EMPTY(lcCursorStyle) AND USED(lcCursorStyle)
    SELECT(lcCursorStyle)
    LOCATE 
    IF !EOF()
      llStySelect = .T.
    ENDIF 
  ENDIF 
ENDIF       

*--Location
IF llMultiWH AND lcRpSortBy = 'W'
  lcCursorLoc = ''
  llLocSelect = .F.

  lnPosLoc = ASCAN(loOgScroll.laOgFXFlt,"STYDYE.CWARECODE")
  IF lnPosLoc > 0 
    lnPosLoc = ASUBSCRIPT(loOGScroll.laOgFXFlt,lnPosLoc,1)
    lcCursorLoc = loOgScroll.laOgFXFlt[lnPosLoc,6]
    IF !EMPTY(lcCursorLoc) AND USED(lcCursorLoc)
      SELECT(lcCursorLoc)
      LOCATE 
      IF !EOF()
        llLocSelect = .T.
      ENDIF 
    ENDIF 
  ENDIF       
ENDIF 

*STYLE.FABRIC lcCursorFab llFabSelect 
*--Fabric 
lcCursorFab = ''
llFabSelect = .F.

lnPosFab = ASCAN(loOgScroll.laOgVRFlt,"STYLE.FABRIC")
IF lnPosFab > 0 
  lnPosFab = ASUBSCRIPT(loOGScroll.laOgVRFlt,lnPosFab,1)
  lcCursorFab = loOgScroll.laOgVRFlt[lnPosFab,6]
  IF !EMPTY(lcCursorFab) AND USED(lcCursorFab)
    SELECT(lcCursorFab)
    LOCATE 
    IF !EOF()
      llFabSelect = .T.
    ENDIF 
  ENDIF 
ENDIF       

*-- To get the selected Division if any.
lcDivs = ''
llUseDiv = .F.

lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'STYLE.CDIVISION'),1)
IF lnPosition > 0
  lcDivs = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  lcDivFile = loogscroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.) and lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
ENDIF
 
*--season
lcSeasons = ''
llUseSeas = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'STYLE.SEASON'),1)
IF lnPosition > 0
  lcSeasons = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  lcSeaFile = loogscroll.gfTempName()
  llUseSeas = IIF(LEN(lcSeasons)>0,.T.,.F.) and lfConvertToCursor(lcSeasons,'SEASON',lcSeaFile)
ENDIF


*--Group
lcGroups = ''
llUseGroup = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGroups  = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  lcGrpFile = loogscroll.gfTempName()
  llUseGroup = IIF(LEN(lcGroups)>0,.T.,.F.) and lfConvertToCursor(lcGroups,'CSTYGROUP',lcGrpFile)
ENDIF


*color
lcColor = ''
llUseColor = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(LOOGSCROLL.laOGVRFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
IF lnPosition > 0
  lcColor    = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  lcClrFile  = loogscroll.gfTempName()
  llUseColor  = IIF(LEN(lcColor)>0,.T.,.F.) and lfConvertToCursor(lcColor,'COLOR',lcClrFile)
ENDIF


*Pattern
lcPattern = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'STYLE.PATTERN'),1)
IF lnPosition > 0
  lcPattern  = LOOGSCROLL.laOGFXFlt[lnPosition,6]
ENDIF

*STYLE.STATUS
lcStatus = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'STYLE.STATUS'),1)
IF lnPosition > 0 
  IF LEN(LOOGSCROLL.laOGFXFlt[lnPosition,6]) > 0
    lcStatus = "INLIST(STYLE.STATUS,'"+STRTRAN(LOOGSCROLL.laOGFXFlt[lnPosition,6],'|',"','")+"')"
  ENDIF 
ENDIF

lcMaj         = gfItemMask('PM')     && Get the major pict. of the style
lnMajSize     = LEN(lcMaj)           && Length of the major

PRIVATE lcClrName , lcClrDes , lcScalVal
STORE SPACE(0) TO lcClrName , lcClrDes , lcScalVal

IF llStySelect 
  SELECT(lcCursorStyle)  
  SCAN 
   SELECT Style 
   IF gfSeek(SUBSTR(&lcCursorStyle..cstymajor,1,lnMajSize))
     SELECT Style
     *B608783,1 WAM 01/05/2008 Fix report filter when the option 'Show Zero Qty' is set to 'No' 
     *SCAN REST WHILE STYLE = SUBSTR(&lcCursorStyle..cstymajor,1,lnMajSize);
          FOR IIF(llFabSelect,SEEK(STYLE.FABRIC,lcCursorFab),.T.);
          AND IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND IIF(llUseSeas,SEEK(STYLE.SEASON,lcSeaFile),.T.);
          AND IIF(llUseColor,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcClrFile),.T.) ;
          AND IIF(llUseGroup,SEEK(STYLE.CSTYGROUP,lcGrpFile ),.T.);
          AND IIF(!EMPTY(lcPattern),Style.pattern = lcPattern ,.T.) AND IIF(!EMPTY(lcStatus),EVALUATE(lcStatus),.T.);
          AND IIF(lcRpSortBy <> 'S',.T.,IIF(llRpShzero,.T.,TOTSTK<>0))
     SCAN REST WHILE STYLE = SUBSTR(&lcCursorStyle..cstymajor,1,lnMajSize);
          FOR IIF(llFabSelect,SEEK(STYLE.FABRIC,lcCursorFab),.T.);
          AND IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND IIF(llUseSeas,SEEK(STYLE.SEASON,lcSeaFile),.T.);
          AND IIF(llUseColor,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcClrFile),.T.) ;
          AND IIF(llUseGroup,SEEK(STYLE.CSTYGROUP,lcGrpFile ),.T.);
          AND IIF(!EMPTY(lcPattern),Style.pattern = lcPattern ,.T.) AND IIF(!EMPTY(lcStatus),EVALUATE(lcStatus),.T.)
     *B608783,1 WAM 01/05/2008 (End)
     
       IF lcRpSortBy = 'W'
        SELECT StyDye
   *!B608589,1  [MOS]  T20080515.0006 missing styles when user selects Sort by Location [START]         
          gfsetorder('StyDye')
   *!B608589,1  [MOS]  T20080515.0006 missing styles when user selects Sort by Location [END]   
        IF gfSeek(Style.Style,'STYDYE','STYDYE')
          SELECT StyDye
          *B608783,1 WAM 01/05/2008 Fix report filter when the option 'Show Zero Qty' is set to 'No' 
          *SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style ;
            FOR IIF(llMultiWH and llLocSelect,SEEK(StyDye.CWARECODE,lcCursorLoc),.T.) AND IIF(llRpShzero,.T.,TOTSTK<>0)
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style ;
            FOR IIF(llMultiWH and llLocSelect,SEEK(StyDye.CWARECODE,lcCursorLoc),.T.)
          *B608783,1 WAM 01/05/2008 (End)
            *--mos   
            IF  llMultiWH and llLocSelect AND !SEEK(StyDye.CWARECODE,lcCursorLoc)
              loop
            ENDIF

            SELECT StyInvJl
            lcStyle = Stydye.Style
            lcWareCode = Stydye.CWARECODE
            lcDyelot   = STYDYE.Dyelot
            IF gfseek(lcStyle+lcWareCode)
              SCAN REST WHILE Style+cWareCode+DTOS(dTrDate)+cSession+cIrType= lcStyle+lcWareCode;
                 FOR dTrDate <= ldRpDate AND cDyelot = lcDyelot
                WAIT WINDOW 'Collecting Data for ' + lcMajTtl + ' ' + Style.Style NOWAIT
                SCATTER MEMVAR
                SELECT(lcStyleTmp)
                lcSeekExp = lcWareCode+lcStyle
                IF !SEEK(lcSeekExp)
                  INSERT INTO (lcStyleTmp) FROM MEMVAR
                  REPLACE Scale    WITH Style.Scale    ,;
                          TotCost  WITH Style.TotCost  ,;
                          PriceA   WITH Style.PriceA   ,;
                          Desc     WITH Style.Desc     ,;
                          Ave_Cost WITH Style.TotCost  
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                  REPLACE NCOST WITH Ave_Cost
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]

                  IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale 
                    lcScalVal = ALLTRIM(Style.cstymajor)+Scale
                    =gfSEEK('S'+Scale,'SCALE')
                  ENDIF
                  IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
                    lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
                    lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
                  ENDIF
                  REPLACE ColrDesc WITH lcClrDes
        
                  FOR lnCrtTmp = 1 TO 8
                    lcNumQty = ALLTRIM(STR(lnCrtTmp))
                    REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
                  ENDFOR
                  IF llRpPrnWhD AND lcRpSortBy = 'S'
                     REPLACE cWareCode WITH SPACE(0)
                  ENDIF
                ELSE
                  FOR lnStkCnt = 1 TO 8
                    lcStkCnt = STR(lnStkCnt,1)
                    REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
                  ENDFOR
                  REPLACE nTotStk WITH nTotStk + m.nTotStk,;
                          nStkVal WITH nStkVal + m.nStkVal,;
                          Ave_Cost WITH Style.TotCost  

                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                  REPLACE NCOST WITH Ave_Cost
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                          
                ENDIF
              ENDSCAN 
            ENDIF 
          ENDSCAN 
        ENDIF 
      ELSE
        SELECT StyInvJl
        lcStyle = Style.Style
        lcWareCode = ''
        lcDyelot   = SPACE(10)
        IF gfseek(lcStyle+lcWareCode)
          SCAN REST WHILE Style+cWareCode+DTOS(dTrDate)+cSession+cIrType= lcStyle+lcWareCode;
             FOR dTrDate <= ldRpDate AND cDyelot = lcDyelot
            WAIT WINDOW 'Collecting Data for ' + lcMajTtl + ' ' + Style.Style NOWAIT
            SCATTER MEMVAR
            SELECT(lcStyleTmp)
            lcSeekExp = lcStyle+lcWareCode
            IF !SEEK(lcSeekExp)
              INSERT INTO (lcStyleTmp) FROM MEMVAR
              REPLACE Scale    WITH Style.Scale    ,;
                      TotCost  WITH Style.TotCost  ,;
                      PriceA   WITH Style.PriceA   ,;
                      Desc     WITH Style.Desc     ,;
                      Ave_Cost WITH Style.TotCost  
                      
              *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
              REPLACE NCOST WITH Ave_Cost
              *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]

               IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale 
                 lcScalVal = ALLTRIM(Style.cstymajor)+Scale
                 =gfSEEK('S'+Scale,'SCALE')
               ENDIF
               IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
                 lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
                 lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
               ENDIF
               REPLACE ColrDesc WITH lcClrDes
               IF llRpPrnWhD 
                 REPLACE cWareCode WITH SPACE(0)
               ENDIF
               FOR lnCrtTmp = 1 TO 8
                 lcNumQty = ALLTRIM(STR(lnCrtTmp))
                 REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
               ENDFOR
               IF llRpPrnWhD AND lcRpSortBy = 'S'
                 REPLACE cWareCode WITH SPACE(0)
               ENDIF
             ELSE
               FOR lnStkCnt = 1 TO 8
                 lcStkCnt = STR(lnStkCnt,1)
                 REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
               ENDFOR
               REPLACE nTotStk WITH nTotStk + m.nTotStk,;
                       nStkVal WITH nStkVal + m.nStkVal,;
                       Ave_Cost WITH Style.TotCost  
               
               *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
               REPLACE NCOST WITH Ave_Cost
               *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                       
             ENDIF
             IF llRpPrnWhD
               SELECT(lcStyleTmp)
               IF !SEEK(lcStyle + StyInvJl.cWareCode)
                 INSERT INTO (lcStyleTmp) FROM MEMVAR
                 REPLACE Scale     WITH Style.Scale        ,;
                         TotCost   WITH Style.TotCost      ,;
                         PriceA    WITH Style.PriceA       ,;
                         Desc      WITH Style.Desc         ,;
                         cWareCode WITH StyInvJl.cWareCode ,;
                         llWare    WITH .T.                ,;
                         Ave_Cost  WITH Style.TotCost  

                 *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                 REPLACE NCOST WITH Ave_Cost
                 *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]

                 IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale
                  lcScalVal = ALLTRIM(Style.cstymajor)+Scale
                 ENDIF
                 IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
                   lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
                   lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
                  ENDIF
                  REPLACE ColrDesc WITH lcClrDes
                  FOR lnCrtTmp = 1 TO 8
                    lcNumQty = ALLTRIM(STR(lnCrtTmp))
                    REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
                  ENDFOR
                ELSE
                  FOR lnStkCnt = 1 TO 8
                   lcStkCnt = STR(lnStkCnt,1)
                   REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
                  ENDFOR
                  REPLACE nTotStk  WITH nTotStk + m.nTotStk ,;
                          nStkVal  WITH nStkVal + m.nStkVal ,;
                          Ave_Cost WITH Style.TotCost  
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                  REPLACE NCOST WITH Ave_Cost
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                          
                ENDIF
              ENDIF
           ENDSCAN 
         ENDIF 
       ENDIF
     ENDSCAN 
   ENDIF 
  ENDSCAN 
ELSE
  SELECT Style
  gfseek('')  
  *B608783,1 WAM 01/05/2008 Fix report filter when the option 'Show Zero Qty' is set to 'No' 
  *SCAN  FOR IIF(llFabSelect,SEEK(STYLE.FABRIC,lcCursorFab),.T.);
          AND IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND IIF(llUseSeas,SEEK(STYLE.SEASON,lcSeaFile),.T.);
          AND IIF(llUseColor,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcClrFile),.T.) ;
          AND IIF(llUseGroup,SEEK(STYLE.CSTYGROUP,lcGrpFile ),.T.);
          AND IIF(!EMPTY(lcPattern),Style.pattern = lcPattern ,.T.) AND IIF(!EMPTY(lcStatus),EVALUATE(lcStatus),.T.);
          AND IIF(lcRpSortBy <> 'S',.T.,IIF(llRpShzero,.T.,TOTSTK<>0))
  SCAN  FOR IIF(llFabSelect,SEEK(STYLE.FABRIC,lcCursorFab),.T.);
          AND IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND IIF(llUseSeas,SEEK(STYLE.SEASON,lcSeaFile),.T.);
          AND IIF(llUseColor,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcClrFile),.T.) ;
          AND IIF(llUseGroup,SEEK(STYLE.CSTYGROUP,lcGrpFile ),.T.);
          AND IIF(!EMPTY(lcPattern),Style.pattern = lcPattern ,.T.) AND IIF(!EMPTY(lcStatus),EVALUATE(lcStatus),.T.)
  *B608783,1 WAM 01/05/2008 (End)
       
       IF lcRpSortBy = 'W'
        SELECT StyDye
      *!B608589,1 [MOS]  T20080515.0006 missing styles when user selects Sort by Location [START]         
          gfsetorder('StyDye')
      *!B608589,1  [MOS]  T20080515.0006 missing styles when user selects Sort by Location [END]
        IF gfSeek(Style.Style,'STYDYE','STYDYE')
          SELECT StyDye
          *B608783,1 WAM 01/05/2008 Fix report filter when the option 'Show Zero Qty' is set to 'No' 
          *SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style ;
            FOR IIF(llMultiWH and llLocSelect,SEEK(StyDye.CWARECODE,lcCursorLoc),.T.) AND IIF(llRpShzero,.T.,TOTSTK<>0)
          SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.Style ;
            FOR IIF(llMultiWH and llLocSelect,SEEK(StyDye.CWARECODE,lcCursorLoc),.T.)
          *B608783,1 WAM 01/05/2008 (End)

            WAIT WINDOW 'Collecting Data for ' + lcMajTtl + ' ' + Style.Style NOWAIT
            SELECT StyInvJl
            lcStyle = Stydye.Style
            lcWareCode = Stydye.CWARECODE
            lcDyelot   = STYDYE.Dyelot
            IF gfseek(lcStyle+lcWareCode)
              SCAN REST WHILE Style+cWareCode+DTOS(dTrDate)+cSession+cIrType= lcStyle+lcWareCode;
                 FOR dTrDate <= ldRpDate AND cDyelot = lcDyelot
                SCATTER MEMVAR
                SELECT(lcStyleTmp)
                lcSeekExp = lcWareCode+lcStyle
                IF !SEEK(lcSeekExp)
                  INSERT INTO (lcStyleTmp) FROM MEMVAR
                  REPLACE Scale    WITH Style.Scale    ,;
                          TotCost  WITH Style.TotCost  ,;
                          PriceA   WITH Style.PriceA   ,;
                          Desc     WITH Style.Desc     ,;
                          Ave_Cost WITH Style.TotCost  
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                  REPLACE NCOST WITH Ave_Cost
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                          

                  IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale 
                    lcScalVal = ALLTRIM(Style.cstymajor)+Scale
                    =gfSEEK('S'+Scale,'SCALE')
                  ENDIF
                  IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
                    lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
                    lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
                  ENDIF
                  REPLACE ColrDesc WITH lcClrDes
        
                  FOR lnCrtTmp = 1 TO 8
                    lcNumQty = ALLTRIM(STR(lnCrtTmp))
                    REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
                  ENDFOR
                  IF llRpPrnWhD AND lcRpSortBy = 'S'
                     REPLACE cWareCode WITH SPACE(0)
                  ENDIF
                ELSE
                  FOR lnStkCnt = 1 TO 8
                    lcStkCnt = STR(lnStkCnt,1)
                    REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
                  ENDFOR
                  REPLACE nTotStk WITH nTotStk + m.nTotStk,;
                          nStkVal WITH nStkVal + m.nStkVal,;
                          Ave_Cost WITH Style.TotCost  
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                  REPLACE NCOST WITH Ave_Cost
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                          
                ENDIF
              ENDSCAN 
            ENDIF 
          ENDSCAN 
        ENDIF 
      ELSE
        SELECT StyInvJl
        lcStyle = Style.Style
        lcWareCode = ''
        lcDyelot   = SPACE(10)
        IF gfseek(lcStyle+lcWareCode)
          SCAN REST WHILE Style+cWareCode+DTOS(dTrDate)+cSession+cIrType= lcStyle+lcWareCode;
             FOR dTrDate <= ldRpDate AND cDyelot = lcDyelot
            WAIT WINDOW 'Collecting Data for '+ lcMajTtl + ' '  + Style.Style NOWAIT
            SCATTER MEMVAR
            SELECT(lcStyleTmp)
            lcSeekExp = lcStyle+lcWareCode
            IF !SEEK(lcSeekExp)
              INSERT INTO (lcStyleTmp) FROM MEMVAR
              REPLACE Scale    WITH Style.Scale    ,;
                      TotCost  WITH Style.TotCost  ,;
                      PriceA   WITH Style.PriceA   ,;
                      Desc     WITH Style.Desc     ,;
                      Ave_Cost WITH Style.TotCost  
              *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
              REPLACE NCOST WITH Ave_Cost
              *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]

               IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale 
                 lcScalVal = ALLTRIM(Style.cstymajor)+Scale
                 =gfSEEK('S'+Scale,'SCALE')
               ENDIF
               IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
                 lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
                 lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
               ENDIF
               REPLACE ColrDesc WITH lcClrDes
               IF llRpPrnWhD 
                 REPLACE cWareCode WITH SPACE(0)
               ENDIF
               FOR lnCrtTmp = 1 TO 8
                 lcNumQty = ALLTRIM(STR(lnCrtTmp))
                 REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
               ENDFOR
               IF llRpPrnWhD AND lcRpSortBy = 'S'
                 REPLACE cWareCode WITH SPACE(0)
               ENDIF
             ELSE
               FOR lnStkCnt = 1 TO 8
                 lcStkCnt = STR(lnStkCnt,1)
                 REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
               ENDFOR
               REPLACE nTotStk WITH nTotStk + m.nTotStk,;
                       nStkVal WITH nStkVal + m.nStkVal,;
                       Ave_Cost WITH Style.TotCost  
               *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
               REPLACE NCOST WITH Ave_Cost
               *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                       
             ENDIF
             IF llRpPrnWhD
               SELECT(lcStyleTmp)
               IF !SEEK(lcStyle + StyInvJl.cWareCode)
                 INSERT INTO (lcStyleTmp) FROM MEMVAR
                 REPLACE Scale     WITH Style.Scale        ,;
                         TotCost   WITH Style.TotCost      ,;
                         PriceA    WITH Style.PriceA       ,;
                         Desc      WITH Style.Desc         ,;
                         cWareCode WITH StyInvJl.cWareCode ,;
                         llWare    WITH .T.                ,;
                         Ave_Cost  WITH Style.TotCost  
                 *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                 REPLACE NCOST WITH Ave_Cost
                 *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                         
          
                 IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale
                  lcScalVal = ALLTRIM(Style.cstymajor)+Scale
                 ENDIF
                 IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
                   lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
                   lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
                  ENDIF
                  REPLACE ColrDesc WITH lcClrDes
                  FOR lnCrtTmp = 1 TO 8
                    lcNumQty = ALLTRIM(STR(lnCrtTmp))
                    REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
                  ENDFOR
                ELSE
                  FOR lnStkCnt = 1 TO 8
                   lcStkCnt = STR(lnStkCnt,1)
                   REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
                  ENDFOR
                  REPLACE nTotStk  WITH nTotStk + m.nTotStk ,;
                          nStkVal  WITH nStkVal + m.nStkVal ,;
                          Ave_Cost WITH Style.TotCost  
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[Start]
                  REPLACE NCOST WITH Ave_Cost
                  *! B609336,1 MMT  07/07/2010 Fix bug of Wrong NCOST Field when Report is Exported to Excel[End]
                          
                ENDIF
              ENDIF
           ENDSCAN 
         ENDIF 
       ENDIF
    ENDSCAN 
ENDIF 
SELECT ('StyInvJl')
gfSetorder(lcOldTag) 
*--End of lpCollData.
*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.




*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : lcParm
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    IF !USED('STYLE_X')
      =gfOpenTable(oAriaApplication.DataDir+'Style',oAriaApplication.DataDir+'Cstyle','SH','STYLE_X')
    ENDIF 
*!*	    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
	    SELECT STYLE
	    SET ORDER TO TAG Cstyle
*!*	    SET RELATION TO STYLE.STYLE INTO STYLE_X
	    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    *USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Passed Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns            : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  =gfSeek(ALLTRIM(lcSty))
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
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


*!**************************************************************************
*! Name      : lfFillAray
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : Fill Sort By Array
*!**************************************************************************
*! Example   : = lfArrDummy()
*!**************************************************************************
*
FUNCTION lfFillAray
DIMENSION laSortDesc[2,1] , laSortVal[2,1]
laSortDesc[1,1] = PROPER(lcMajTtl)
laSortDesc[2,1] = 'Location'
laSortVal[1,1]  = 'S'
laSortVal[2,1]  = 'W'
*-- End of lfFillAray.

*!**************************************************************************
*! Name      : lfvWareHos
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : Validate Warehouse Code
*!**************************************************************************
*! Example   : = lfvWareHos()
*!**************************************************************************
*
FUNCTION lfvWareHos
PRIVATE lcWareHous , lcTag

lcWareHous = VARREAD()
lcTag      = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

*B607419,1 ABD - Valid Ony if the Field is not Empty. [Begin]
IF !EMPTY(&lcWareHous) 
  *B607419,1 ABD - [End]
  IF LASTKEY() = 13 AND !MDOWN()
    IF SEEK(&lcWareHous.,'WAREHOUS') 
      &lcWareHous = WAREHOUS.cWareCode
    ELSE
      &lcWareHous = gfBroWWare(.T.)
    ENDIF
  ELSE
    &lcWareHous = lcOldWare
  ENDIF
  *B607419,1 ABD - End IF for IF Statment. [Begin]
ENDIF
*B607419,1 ABD - [End]

SET ORDER TO &lcTag IN WAREHOUS
*-- End of lfvWareHos.


*!**************************************************************************
*! Name      : lfwOldWare
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : To get the old value of warehouse
*!**************************************************************************
*! Example   : = lfwOldWare()
*!**************************************************************************
*
FUNCTION lfwOldWare
lcOldWare = EVALUATE(SYS(18))
*-- End of lfwOldWare.


*!**************************************************************************
*! Name      : lfvSortBy
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : valid sort by function
*!**************************************************************************
*! Example   : =lfvSortBy()
*!**************************************************************************
*
FUNCTION lfvSortBy
CLEARREAD()
*-- End of lfvSortBy.

*!**************************************************************************
*! Name      : lfvPrnWhD
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : valid print warehouse details
*!**************************************************************************
*! Example   : =lfvPrnWhD()
*!**************************************************************************
*
FUNCTION remlfvPrnWhD
llOldPrWhD = llRpPrnWhD   && Save current print ware house details in another 
*-- End of lfvPrnWhD.
*B126528,1 BWA 03/09/2005.[END]

*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Sameh (SSE)
*! Date      : 08/05/99
*! Purpose   : get the Scale sizes
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lfGetScale()
*!**************************************************************************
*
FUNCTION lfGetScale
PARAMETERS lcScale
PRIVATE lcOldAlias,lnX,lcString,lcScale,lcZ
lcOldAlias = ALIAS()

SELECT SCALE
gfSEEK('S'+lcScale)
lnX      = 1
lcString = ''
IF FOUND() 
  DO WHILE lnX <= CNT
    lcZ = STR(lnX,1)
    *B607273,1 ALB Fix the header alliment [Begin]
    *lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','  ')
    lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','   ')
    *B607273,1 ALB Fix the header alliment [end]
    lnX= lnX + 1
  ENDDO
ELSE
  lcString = '* * * E R R O R * * *'
ENDIF
IF LEN(TRIM(lcOldAlias)) > 0
  SELECT (lcOldAlias)
ENDIF
RETURN(lcString)
*-- End of lfGetScale.
*B126528,1 BWA 03/09/2005.[START]
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/09/2005
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/09/2005
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*B126528,1 BWA 03/09/2005.[END]
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/26/2006
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
*--
LOCAL lnAlias
lnAlias = SELECT()
*--

lnTotcomp = 0
SELECT(lcCursorLoc)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcCursorLoc)
  LOCATE 
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE cstymajor=lcFab AND DYELOT =''
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  

SELECT(lnAlias)

RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 09/14/2005
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
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
CASE   ALLTRIM(lcFieldName) = 'COLOR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYGROUP'
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