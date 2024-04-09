*!*  ************************************************************************
*!*  Descrption    : Custom Assortemnt program for GMA
*!*  Developer     : Saber A.Razek (SAB)
*!*  Date          : 08/02/2012
*!*  Entry #       : A40 ==> C201505,A27==>C201506 [T20120224.0012]
*!*  ************************************************************************
*!* Modifications;
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [T20120224.0012]
*C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[T20120224.0012]
*C202163,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL
*!*  ************************************************************************
PARAMETERS lcFromScreen

*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*IF oAriaApplication.MULTIINST 
*  DO FORM ("X:\aria4xp\Screens"+"\ic\ICASSBRKD.SCX") WITH lcFromScreen
*ELSE
*  DO FORM (oAriaApplication.ScreenHome+"\ic\ICASSBRKD.SCX") WITH lcFromScreen
*ENDIF
DO FORM (oAriaApplication.cLIENTScreenHome+"\ic\ICASSBRKD.SCX") WITH lcFromScreen
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
RETURN  

*!*************************************************************
*! Name      : lfFormInit
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : init Method of Screen
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*loFormSet.llRpSamSt = .T.
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
loFormSet.lcRpWareH = ''
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
loFormSet.lcRpBusCase = ''
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
loFormSet.lcGlFYear  = " "
loFormSet.lcGlPeriod = " "
loFormSet.ariaForm1.DtpDate.Value = oAriaApplication.SystemDate

*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*IF loFormSet.lcFromScreen = 'C'
*  loFormSet.ariaForm1.Caption = 'Style Assortment Creation'
*  loFormSet.ariaForm1.lblStyAss.Caption = 'Assortment Breakdown:'
*  loFormSet.ariaForm1.lblDown.Caption = 'Assortment Style:'
*ELSE
*  loFormSet.ariaForm1.Caption = 'Style Assortment Breakdown'
*  loFormSet.ariaForm1.lblStyAss.Caption = 'Assortment Style:'
*  loFormSet.ariaForm1.lblDown.Caption = 'Assortment Breakdown:'
*ENDIF 
loFormSet.ariaForm1.Caption = 'Style Assortment Transfer'
loFormSet.ariaForm1.lblStyAss.Caption = 'Assortment Style:'
loFormSet.ariaForm1.lblDown.Caption = 'Assortment Breakdown:'
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]

DIMENSION loFormSet.aSourceArray[1,2]
STORE '' TO loFormSet.aSourceArray
IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS')
ENDIF 
SELECT 'WAREHOUS'
=gfSeek('')
SELECT cdesc ,CWARECODE  FROM 'WAREHOUS' ORDER BY CWARECODE INTO ARRAY loFormSet.aSourceArray
loFormSet.ariaForm1.cboLocation.Requery ()

*!*************************************************************
*! Name      : lfChangeMode
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : Chnage mode Method of Screen
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
IF loFormSet.ActiveMode = 'S'
  llFromScreen = .T.
  IF loFormSet.lcFromScreen = 'C'
    llFromCreate = .T.
  ENDIF 
  WITH loFormSet.AriaForm1.cntQtyBrkdwn2
    .txtQty1.Enabled = .F.
    .txtQty2.Enabled = .F.
    .txtQty3.Enabled = .F.
    .txtQty4.Enabled = .F.
    .txtQty6.Enabled = .F.
    .txtQty5.Enabled = .F.
    .txtQty7.Enabled = .F.
    .txtQty8.Enabled = .F.
  ENDWITH   
  loFormSet.AriaForm1.cboAdjReason.Enabled = .F.
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
  *loFormSet.llRpSamSt = .F.
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  DIMENSION laFxExpr[1,6]
  STORE '' TO laFxExpr
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
  PRIVATE loFormSetObj
  loFormSetObj = loFormSet
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  
  lcExpr = gfOpGrid('ICSTYASS' , .T.)
  
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
  loFormSet = loFormSetObj
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  
  IF EMPTY(lcExpr) OR lcExpr = '.F.'
    loFormSet.ariaForm1.cboAdjReason.Enabled = .F. 
    loFormSet.ariaForm1.cboLocation.Enabled = .F.
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    loFormSet.OGOk = .F.    
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
    RETURN 	.F.
  ELSE
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    loFormSet.OGOk = .T.
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
    IF !lfCollectDate(loFormSet)
      loFormSet.ChangeMode ('S')  
    ELSE
      loFormSet.ChangeMode ('E')
    ENDIF  
  ENDIF 
  loFormSet.ariaForm1.cboLocation.Enabled = .T.
  
ELSE
  IF loFormSet.ActiveMode = 'E'  
    loFormSet.ariaForm1.DtpDate.Enabled = .T.
    loFormSet.ariaForm1.cboLocation.Enabled = .T.
    loFormSet.AriaForm1.grddown.Enabled = .T.
    loFormSet.AriaForm1.grdUpp.Enabled = .T.
    SELECT (loFormSet.lcFrstGrdCursor)
    LOCATE 
    IF EOF(loFormSet.lcFrstGrdCursor)
      WITH loFormSet.AriaForm1.cntQtyBrkdwn2
        .txtQty1.Enabled = .F.
        .txtQty2.Enabled = .F.
        .txtQty3.Enabled = .F.
        .txtQty4.Enabled = .F.
        .txtQty6.Enabled = .F.
        .txtQty5.Enabled = .F.
        .txtQty7.Enabled = .F.
        .txtQty8.Enabled = .F.
      ENDWITH   
      loFormSet.AriaForm1.cboAdjReason.Enabled = .F.
    ELSE
      WITH loFormSet.AriaForm1.cntQtyBrkdwn2
        .txtQty1.Enabled = .T.
        .txtQty2.Enabled = .T.
        .txtQty3.Enabled = .T.
        .txtQty4.Enabled = .T.
        .txtQty6.Enabled = .T.
        .txtQty5.Enabled = .T.
        .txtQty7.Enabled = .T.
        .txtQty8.Enabled = .T.
      ENDWITH
      loFormSet.AriaForm1.cboAdjReason.Enabled = .T.
    ENDIF
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    *IF loFormSet.lcFromScreen = 'B' AND loFormSet.llRpSamSt        
    *  loFormSet.AriaForm1.cmdAdd.Enabled = .F.
    *ELSE
    *  loFormSet.AriaForm1.cmdAdd.Enabled = .T.
    *ENDIF
    loFormSet.AriaForm1.cmdAdd.Enabled = .T.
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  ELSE
    loFormSet.ChangeMode ('S')
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfWgrid
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : When function of option grid
*!*************************************************************
FUNCTION lfWgrid

loogScroll.Parent.Caption =  '(' + ALLTRIM(oAriaApplication.ActiveCompanyID) + ') ' + ;
                 IIF(EMPTY(oAriaApplication.ActiveCompanyName), ;
                     '', ;
                     '- ' + ALLTRIM(oAriaApplication.ActiveCompanyName) + ' - ') +" " + 'Sorting Selection Grid'
lcOGWinTitl = loogScroll.Parent.Caption 

IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS')
ENDIF 

SELECT 'WAREHOUS'
=gfSeek('')
DIMENSION laLocVal[1]
DIMENSION laLocDesc[1]
laLocDesc = ''
laLocVal = ''
SELECT CWARECODE  FROM 'WAREHOUS' ORDER BY CWARECODE INTO ARRAY laLocVal
SELECT cdesc FROM 'WAREHOUS' ORDER BY CWARECODE INTO ARRAY laLocDesc

*!*************************************************************
*! Name      : lfDefloc 
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : function to get Default location
*!*************************************************************
FUNCTION lfDefloc 
SELECT 'WAREHOUS'
LOCATE FOR ldefware 
IF FOUND()
  lcDefloc = CWARECODE
ELSE
  lcDefloc = ''
ENDIF 
RETURN   lcDefloc

*!*************************************************************
*! Name      : lfCreatExp
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 11/10/2009
*! Purpose   : Function to copy option grid criteria
*!*************************************************************
FUNCTION lfCreatExp
  IF TYPE('llFromScreen') = 'L'
    
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    llStySelect = .F.
    lcStyFile = ''
    lnStyCount = 0
    lnStyPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.STYLE")
    IF lnStyPos > 0 
      lnStyPos  = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnStyPos,1)
      lcStyFile = loOgScroll.laOgFxFlt[lnStyPos ,6]
    ENDIF  
    IF !EMPTY(lcStyFile)  AND USED(lcStyFile)
      SELECT(lcStyFile)
      LOCATE 
      IF !EOF()
        llStySelect = .T.
        COUNT FOR !DELETED() TO lnStyCount
      ENDIF 
    ENDIF
    IF !llStySelect
      **  Message: "You have to enter The ð.       "
      **  Choices: "              ® Ok ¯           "  
      =gfModalGen("TRM02094B00000","DIALOG",'style(s)')  
      RETURN .F. 
    ENDIF
    
    IF INLIST(lcRpBusCase, 'OO', 'OM') AND lnStyCount <> 1          
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Only one Style should be selected in One to One and One to Many business Casses")
      RETURN .F. 
    ENDIF
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]

    DIMENSION laFxExpr[ALEN(loOGScroll.laOGFxFlt,1),8]
    =ACOPY(loOGScroll.laOGFxFlt , laFxExpr)
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    *loFormSet.llRpSamSt = llRpSamSt
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
    loFormSet.lcRpWareH = lcRpWareH
    
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    loFormSet.lcRpBusCase = lcRpBusCase
    loFormSet.lcFromScreen = IIF(loFormSet.lcRpBusCase = 'MO' , 'C', 'B')
    *loFormSet.llRpSamSt = IIF(loFormSet.lcRpBusCase = 'MO' , llRpSamSt, .F.)
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  ENDIF 

*!*************************************************************
*! Name      : lfCollectDate
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : function to Collect Data
*!*************************************************************  
FUNCTION lfCollectDate
PARAMETERS loFormSet
loFormSet.ariaForm1.cboLocation.Value = loFormSet.lcRpWareH 
STORE 0 TO lnClrLnAv , lnClrPosAv
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1] = 'C'
    lnClrLnAv  = LEN(laItemSeg[lnCount,3])
    lnClrPosAv = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

loFormSet.lnClrPosAv = lnClrPosAv 
loFormSet.lenclrlen  = lnClrLnAv  
loFormSet.lnmajlength = LEN(gfItemMask("PM"))

IF USED(loFormSet.lcScndGrdCursor)
  SELECT (loFormSet.lcScndGrdCursor)
  DELETE ALL 
ENDIF 

IF USED(loFormSet.lcFrstGrdCursor)
  SELECT (loFormSet.lcFrstGrdCursor)
  DELETE ALL 
ENDIF 

IF !USED('StyInvJl')
  =gfOpenTable("StyInvJl","StyInvJl","SH")
ENDIF


IF !USED('STYLE')
  =gfOpenTable('STYLE','STYLE')
ENDIF 
IF !USED('STYLE_CLR')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_CLR')
ENDIF 

IF !USED('SCALE')
  =gfOpenTable('SCALE','SCALE')
ENDIF 

IF !USED('STYDYE')
  =gfOpenTable('STYDYE','STYDYE')
ENDIF 

IF !USED('CODES')
  =gfOpenTable('CODES','CCODE_NO')
ENDIF 
IF !USED('InvtAdj')
  =gfOpenTable('InvtAdj','InvtAdj')
ENDIF 


 *--Style
 llStySelect = .F.
 lcStyFile = ''
 lnStyPos = ASCAN(laFxExpr,"STYLE.STYLE")
 IF lnStyPos > 0 
   lnStyPos  = ASUBSCRIPT(laFxExpr,lnStyPos,1)
   lcStyFile = laFxExpr[lnStyPos ,6]
 ENDIF  
 IF !EMPTY(lcStyFile)  AND USED(lcStyFile)
   SELECT(lcStyFile)
   LOCATE 
   IF !EOF()
     llStySelect = .T.
   ENDIF 
 ENDIF 

 *CSTYGRP
 llItmGrpSelect = .F.
 lcItmGrpFile   = ''
 lnItmGrpPos = ASUBSCRIPT(laFxExpr,ASCAN(laFxExpr,"STYLE.CSTYGROUP"),1)
 IF lnItmGrpPos > 0
   lcGrpStr   = laFxExpr[lnItmGrpPos,6]
   IF !EMPTY(lcGrpStr)
     lcItmGrpFile = gfTempName()  
     llItmGrpSelect = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcItmGrpFile)
   ENDIF 
 ENDIF 
 *SEASON
 llItmSeaSelect = .F.
 lcItmSeaFile   = ''
 lnItmSeaPos = ASUBSCRIPT(laFxExpr,ASCAN(laFxExpr,"STYLE.SEASON"),1)
 IF lnItmSeaPos > 0
   lcSeaStr   = laFxExpr[lnItmSeaPos,6]
   IF !EMPTY(lcSeaStr)
     lcItmSeaFile = gfTempName()  
     llItmSeaSelect = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcItmSeaFile)
   ENDIF 
 ENDIF 
 
 *CDIVISION
 llItmDivSelect = .F.
 lcItmDivFile   = ''
 lnItmDivPos = ASUBSCRIPT(laFxExpr,ASCAN(laFxExpr,"STYLE.CDIVISION"),1)
 IF lnItmDivPos > 0
   lcDivStr   = laFxExpr[lnItmDivPos,6]
   IF !EMPTY(lcDivStr)
     lcItmDivFile = gfTempName()  
     llItmDivSelect = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcItmDivFile)
   ENDIF 
 ENDIF 
IF gfSEEK('D'+'CADJREASON','Codes','CCODE_NO')
  m.cAdjRes  = Codes.CCODE_NO
ENDIF  
lnClrPosAv  = loFormSet.lnClrPosAv  
lnClrLnAv  = loFormSet.lenclrlen 
IF llStySelect 
 SELECT(lcStyFile)
 LOCATE
 SCAN 
   WAIT WINDOW 'Collecting Data For Style:'+&lcStyFile..STYLE NOWAIT 
   SELECT STYLE 
   =gfSeek(&lcStyFile..STYLE)
   IF !IIF(llItmDivSelect ,SEEK(STYLE.CDIVISION,lcItmDivFile),.T.) 
     LOOP 
   ENDIF 
   IF !IIF(llItmSeaSelect ,SEEK(STYLE.SEASON,lcItmSeaFile),.T.) 
     LOOP 
   ENDIF 
   IF !IIF(llItmGrpSelect ,SEEK(STYLE.CSTYGROUP,lcItmGrpFile),.T.)
     LOOP 
   ENDIF 
   SELECT STYDYE 
   IF !gfSeek(&lcStyFile..STYLE+loFormSet.lcRpWareH)
     LOOP 
   ENDIF 
   SCATTER MEMO MEMVAR 
   m.cStyMajor = Style.cStyMajor
   m.Color = SUBSTR(m.Style,lnClrPosAv  ,lnClrLnAv)
   m.TotNStk = m.TotStk
   FOR lnI = 1 TO 8 
     lcI = STR(lnI,1)
     m.nStk&lcI = m.Stk&lcI 
   ENDFOR  
   =gfSeek('S'+Style.Scale,'Scale')
   m.Scale = Scale.SCALE
   FOR lnJ = 1 TO 8
     lcJ = STR(lnJ,1)
     m.cSize&lcJ. = Scale.SZ&lcJ.
   ENDFOR 
   
   FOR lnJ = 1 TO 8
     lcJ = STR(lnJ,1)
     m.Mov&lcJ. = 0
   ENDFOR 
   m.TotMov = 0
   m.Date = oAriaApplication.SYSTEMDATE
   m.OLDStyle = Style.Style
   m.ClrDsc = gfCodDes(m.Color,'COLOR')
   *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
   m.Desc = STYLE.Desc
   *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [ENd]
   INSERT INTO (loFormSet.lcFrstGrdCursor) FROM MEMVAR 
   *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
   *IF loFormSet.lcFromScreen = 'B' AND loFormSet.llRpSamSt
   IF .F.
   *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
     SELECT 'STYLE_CLR'
     =gfSeek(SUBSTR(m.cStyMajor,1,loFormSet.lnmajlength ))
     m.OldStyle = m.Style
     SCAN REST WHILE Style = SUBSTR(m.cStyMajor,1,loFormSet.lnmajlength) FOR IIF(llItmDivSelect ,SEEK(STYLE_CLR.CDIVISION,lcItmDivFile),.T.) AND;
           IIF(llItmSeaSelect ,SEEK(STYLE_CLR.SEASON,lcItmSeaFile),.T.) AND ;
           IIF(llItmGrpSelect ,SEEK(STYLE_CLR.CSTYGROUP,lcItmGrpFile),.T.) 
       SELECT STYDYE 
       IF !gfSeek(STYLE_CLR.STYLE+loFormSet.lcRpWareH)
         LOOP 
       ENDIF 
       SCATTER MEMO MEMVAR 
       m.cStyMajor = STYLE_CLR.cStyMajor
       m.Color = SUBSTR(m.Style,lnClrPosAv  ,lnClrLnAv)
       m.TotNStk = m.TotStk
       FOR lnI = 1 TO 8 
         lcI = STR(lnI,1)
         m.nStk&lcI = m.Stk&lcI 
       ENDFOR 
       FOR lnJ = 1 TO 8
         lcJ = STR(lnJ,1)
         m.Mov&lcJ. = 0
       ENDFOR 
       m.TotMov = 0
 
       =gfSeek('S'+STYLE_CLR.Scale,'Scale')
       m.Scale = Scale.SCALE
       FOR lnJ = 1 TO 8
         lcJ = STR(lnJ,1)
         m.cSize&lcJ. = Scale.SZ&lcJ.
       ENDFOR 
       m.Date = oAriaApplication.SYSTEMDATE
       m.ClrDsc = gfCodDes(m.Color,'COLOR')
       INSERT INTO (loFormSet.lcScndGrdCursor) FROM MEMVAR 
     ENDSCAN   
   ENDIF   
 ENDSCAN 
ELSE
  SELECT STYLE 
  =gfSeek('')
  SCAN FOR IIF(llItmDivSelect ,SEEK(STYLE.CDIVISION,lcItmDivFile),.T.) AND;
           IIF(llItmSeaSelect ,SEEK(STYLE.SEASON,lcItmSeaFile),.T.) AND ;
           IIF(llItmGrpSelect ,SEEK(STYLE.CSTYGROUP,lcItmGrpFile),.T.)
     WAIT WINDOW 'Collecting Data For Style:'+Style.STYLE NOWAIT       
     SELECT STYDYE 
     IF !gfSeek(STYLE.STYLE+loFormSet.lcRpWareH)
       LOOP 
     ENDIF 
     SCATTER MEMO MEMVAR 
     m.cStyMajor = Style.cStyMajor
     m.Color = SUBSTR(m.Style,lnClrPosAv  ,lnClrLnAv)
     m.TotNStk = m.TotStk
     FOR lnI = 1 TO 8 
       lcI = STR(lnI,1)
       m.nStk&lcI = m.Stk&lcI 
     ENDFOR  
     =gfSeek('S'+Style.Scale,'Scale')
     m.Scale = Scale.SCALE
     FOR lnJ = 1 TO 8
       lcJ = STR(lnJ,1)
       m.cSize&lcJ. = Scale.SZ&lcJ.
     ENDFOR 
     
     FOR lnJ = 1 TO 8
       lcJ = STR(lnJ,1)
       m.Mov&lcJ. = 0
     ENDFOR 
     m.TotMov = 0
     
     m.Date = oAriaApplication.SYSTEMDATE
     m.OLDStyle = Style.Style
     m.ClrDsc = gfCodDes(m.Color,'COLOR')
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
     m.Desc = STYLE.Desc
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [ENd]
     INSERT INTO (loFormSet.lcFrstGrdCursor) FROM MEMVAR 
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
     *IF loFormSet.lcFromScreen = 'B' AND loFormSet.llRpSamSt
     IF .F.
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
       SELECT 'STYLE_CLR'
       =gfSeek(SUBSTR(m.cStyMajor,1,loFormSet.lnmajlength ))
       m.OldStyle = m.Style
       SCAN REST WHILE Style = SUBSTR(m.cStyMajor,1,loFormSet.lnmajlength ) FOR IIF(llItmDivSelect ,SEEK(STYLE_CLR.CDIVISION,lcItmDivFile),.T.) AND;
             IIF(llItmSeaSelect ,SEEK(STYLE_CLR.SEASON,lcItmSeaFile),.T.) AND ;
             IIF(llItmGrpSelect ,SEEK(STYLE_CLR.CSTYGROUP,lcItmGrpFile),.T.) 

         SELECT STYDYE 
         IF !gfSeek(STYLE_CLR.STYLE+loFormSet.lcRpWareH)
           LOOP 
         ENDIF 
         SCATTER MEMO MEMVAR 
         m.cStyMajor = STYLE_CLR.cStyMajor
         m.Color = SUBSTR(m.Style,lnClrPosAv  ,lnClrLnAv)
         m.TotNStk = m.TotStk
         FOR lnI = 1 TO 8 
           lcI = STR(lnI,1)
           m.nStk&lcI = m.Stk&lcI 
         ENDFOR  
         FOR lnJ = 1 TO 8
           lcJ = STR(lnJ,1)
           m.Mov&lcJ. = 0
         ENDFOR 
         m.TotMov = 0

         =gfSeek('S'+STYLE_CLR.Scale,'Scale')
         m.Scale = Scale.SCALE
         FOR lnJ = 1 TO 8
           lcJ = STR(lnJ,1)
           m.cSize&lcJ. = Scale.SZ&lcJ.
         ENDFOR 
         m.Date = oAriaApplication.SYSTEMDATE
         m.ClrDsc = gfCodDes(m.Color,'COLOR')
         INSERT INTO (loFormSet.lcScndGrdCursor) FROM MEMVAR 
       ENDSCAN   
     ENDIF    
  ENDSCAN 
ENDIF 
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*IF loFormSet.lcFromScreen = 'B' AND loFormSet.llRpSamSt
IF .F.
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  SELECT(loFormSet.lcScndGrdCursor)
  DELETE FOR OLDSTYLE = STYLE 
  locate 
ENDIF  
SELECT (loFormSet.lcFrstGrdCursor) 
LOCATE 
IF EOF()
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN .F.
ELSE
  RETURN .T.  
ENDIF 
loFormSet.AriaForm1.grdUpp.Refresh
loFormSet.AriaForm1.grdUpp.AfterRowColchange()

*!*************************************************************
*! Name      : lfAddCntlSrc
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : function to add Control source
*!*************************************************************  
FUNCTION lfAddCntlSrc
PARAMETERS loFormSet
WITH loFormSet.AriaForm1.grdUpp
  .RecordSource = ''
  .RecordSource = loFormSet.lcFrstGrdCursor
  lnColOrder = 0
  IF loFormSet.lcfromscreen <> 'B'
    .ColumnCount = 31
    .Columns(31).ControlSource = loFormSet.lcFrstGrdCursor + '.lSelect'
    IF TYPE('loFormSet.AriaForm1.grdUpp.Columns(31).AriaCheckBox1') <> 'O'
      loFormSet.AriaForm1.grdUpp.Columns(31).AddObject('AriaCheckBox1','AriaCheckBox')
    ENDIF 
    loFormSet.AriaForm1.grdUpp.Columns(31).AriaCheckBox1.Caption = ''
    loFormSet.AriaForm1.grdUpp.Columns(31).AriaCheckBox1.Enabled = .T.
    loFormSet.AriaForm1.grdUpp.Columns(31).CurrentControl = 'AriaCheckBox1'
    loFormSet.AriaForm1.grdUpp.Columns(31).ColumnOrder = 1
    loFormSet.AriaForm1.grdUpp.Columns(31).Sparse = .F.
    loFormSet.AriaForm1.grdUpp.Columns(31).READONLY = .F.
    loFormSet.AriaForm1.grdUpp.Columns(31).Header1.caption = ""
    lnColOrder = 1
   ELSE 
    .ColumnCount = 30
    lnColOrder = 0
   ENDIF 
  .Columns(1).ControlSource = loFormSet.lcFrstGrdCursor + '.Style'
  .Columns(1).Header1.caption = 'Style'
  .Columns(1).ColumnOrder = lnColOrder+1
  .Columns(2).ControlSource = loFormSet.lcFrstGrdCursor + '.DESC'
  .Columns(2).Header1.caption = 'Description'
  .Columns(2).ColumnOrder = lnColOrder+2
  .Columns(3).ControlSource = loFormSet.lcFrstGrdCursor + '.COLOR'
  .Columns(3).Header1.caption = 'Color'
  .Columns(3).ColumnOrder = lnColOrder+3
  lnColCnt = 4
  lnT =1 
  FOR lnF = lnColCnt TO lnColCnt + 7
    .Columns(lnF).ControlSource = loFormSet.lcFrstGrdCursor + '.Stk'+STR(lnT,1)
    .Columns(lnF).Header1.caption = "Stk" +STR(lnT,1)
    .Columns(lnF).ColumnOrder = lnColOrder + lnF
    lnT = lnT + 1
  ENDFOR
  .Columns(12).ControlSource = loFormSet.lcFrstGrdCursor + '.TOTSTK'
  .Columns(12).Header1.caption = 'TotStk'
  .Columns(12).ColumnOrder = lnColOrder + 12
  lnT =1 
  lnColCnt = 13
  FOR lnF = lnColCnt TO lnColCnt + 7
    .Columns(lnF).ControlSource = loFormSet.lcFrstGrdCursor + '.Mov'+STR(lnT,1)
    .Columns(lnF).Header1.caption = "Mov" +STR(lnT,1)
    .Columns(lnF).ColumnOrder = lnColOrder + lnF
    lnT = lnT + 1
  ENDFOR
  .Columns(21).ControlSource = loFormSet.lcFrstGrdCursor + '.TOTMov'
  .Columns(21).Header1.caption = 'TotMov'
  .Columns(21).ColumnOrder = lnColOrder + 21
  lnT =1 
  lnColCnt = 22
  FOR lnF = lnColCnt TO lnColCnt + 7
    .Columns(lnF).ControlSource = loFormSet.lcFrstGrdCursor + '.nStk'+STR(lnT,1)
    .Columns(lnF).Header1.caption = "NewStk" +STR(lnT,1)
    .Columns(lnF).ColumnOrder = lnColOrder + lnF
    lnT = lnT + 1
  ENDFOR
  .Columns(30).ControlSource = loFormSet.lcFrstGrdCursor + '.TOTnSTK'
  .Columns(30).Header1.caption = 'TotNewStk'
  .Columns(30).ColumnOrder = lnColOrder + 30
  
ENDWITH 

WITH loFormSet.AriaForm1.grddown
  .RecordSource = ''
  .RecordSource = loFormSet.lcScndGrdCursor
  .ColumnCount = 21
  .Columns(1).ControlSource = loFormSet.lcScndGrdCursor+ '.Style'
  .Columns(1).Header1.caption = 'Style'
  .Columns(2).ControlSource = loFormSet.lcScndGrdCursor+ '.DESC'
  .Columns(2).Header1.caption = 'Description'
  .Columns(3).ControlSource = loFormSet.lcScndGrdCursor+ '.COLOR'
  .Columns(3).Header1.caption = 'Color'
  lnColCnt = 4
  lnT =1 
  FOR lnF = lnColCnt TO lnColCnt + 7
    .Columns(lnF).ControlSource = loFormSet.lcScndGrdCursor + '.Mov'+STR(lnT,1)
    .Columns(lnF).Header1.caption = "Mov" +STR(lnT,1)
    lnT = lnT + 1
  ENDFOR
  .Columns(12).ControlSource = loFormSet.lcScndGrdCursor + '.TOTMov'
  .Columns(12).Header1.caption = 'TotMov'
  lnT =1 
  lnColCnt = 13
  FOR lnF = lnColCnt TO lnColCnt + 7
    .Columns(lnF).ControlSource = loFormSet.lcScndGrdCursor + '.nStk'+STR(lnT,1)
    .Columns(lnF).Header1.caption = "NewStk" +STR(lnT,1)
    lnT = lnT + 1
  ENDFOR
  .Columns(21).ControlSource = loFormSet.lcScndGrdCursor + '.TOTnStk'
  .Columns(21).Header1.caption = 'TotNewStk'  
ENDWITH 


*!*************************************************************
*! Name      : lfCreateTemp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : function to create temp. files
*!*************************************************************  
FUNCTION lfCreateTemp
PARAMETERS loFormSet
DIMENSION laFileStur[47,4]

laFileStur[1,1] = 'Style'
laFileStur[1,2] = 'C'
laFileStur[1,3] = 19
laFileStur[1,4] = 0

laFileStur[2,1] = 'cStyMajor'
laFileStur[2,2] = 'C'
laFileStur[2,3] = 19
laFileStur[2,4] = 0

laFileStur[3,1] = 'Desc'
laFileStur[3,2] = 'C'
laFileStur[3,3] = 30
laFileStur[3,4] = 0

laFileStur[4,1] = 'Color'
laFileStur[4,2] = 'C'
laFileStur[4,3] = 6
laFileStur[4,4] = 0

laFileStur[5,1] = 'TotStk'
laFileStur[5,2] = 'N'
laFileStur[5,3] = 8
laFileStur[5,4] = 0

laFileStur[6,1] = 'Stk1'
laFileStur[6,2] = 'N'
laFileStur[6,3] = 7
laFileStur[6,4] = 0

laFileStur[7,1] = 'Stk2'
laFileStur[7,2] = 'N'
laFileStur[7,3] = 7
laFileStur[7,4] = 0

laFileStur[8,1] = 'Stk3'
laFileStur[8,2] = 'N'
laFileStur[8,3] = 7
laFileStur[8,4] = 0

laFileStur[9,1] = 'Stk4'
laFileStur[9,2] = 'N'
laFileStur[9,3] = 7
laFileStur[9,4] = 0

laFileStur[10,1] = 'Stk5'
laFileStur[10,2] = 'N'
laFileStur[10,3] = 7
laFileStur[10,4] = 0

laFileStur[11,1] = 'Stk6'
laFileStur[11,2] = 'N'
laFileStur[11,3] = 7
laFileStur[11,4] = 0

laFileStur[12,1] = 'Stk7'
laFileStur[12,2] = 'N'
laFileStur[12,3] = 7
laFileStur[12,4] = 0

laFileStur[13,1] = 'Stk8'
laFileStur[13,2] = 'N'
laFileStur[13,3] = 7
laFileStur[13,4] = 0

laFileStur[14,1] = 'TotMov'
laFileStur[14,2] = 'N'
laFileStur[14,3] = 8
laFileStur[14,4] = 0

laFileStur[15,1] = 'Mov1'
laFileStur[15,2] = 'N'
laFileStur[15,3] = 7
laFileStur[15,4] = 0

laFileStur[16,1] = 'Mov2'
laFileStur[16,2] = 'N'
laFileStur[16,3] = 7
laFileStur[16,4] = 0

laFileStur[17,1] = 'Mov3'
laFileStur[17,2] = 'N'
laFileStur[17,3] = 7
laFileStur[17,4] = 0

laFileStur[18,1] = 'Mov4'
laFileStur[18,2] = 'N'
laFileStur[18,3] = 7
laFileStur[18,4] = 0

laFileStur[19,1] = 'Mov5'
laFileStur[19,2] = 'N'
laFileStur[19,3] = 7
laFileStur[19,4] = 0

laFileStur[20,1] = 'Mov6'
laFileStur[20,2] = 'N'
laFileStur[20,3] = 7
laFileStur[20,4] = 0

laFileStur[21,1] = 'Mov7'
laFileStur[21,2] = 'N'
laFileStur[21,3] = 7
laFileStur[21,4] = 0

laFileStur[22,1] = 'Mov8'
laFileStur[22,2] = 'N'
laFileStur[22,3] = 7
laFileStur[22,4] = 0


laFileStur[23,1] = 'TotNStk'
laFileStur[23,2] = 'N'
laFileStur[23,3] = 8
laFileStur[23,4] = 0

laFileStur[24,1] = 'nStk1'
laFileStur[24,2] = 'N'
laFileStur[24,3] = 7
laFileStur[24,4] = 0

laFileStur[25,1] = 'nStk2'
laFileStur[25,2] = 'N'
laFileStur[25,3] = 7
laFileStur[25,4] = 0

laFileStur[26,1] = 'nStk3'
laFileStur[26,2] = 'N'
laFileStur[26,3] = 7
laFileStur[26,4] = 0

laFileStur[27,1] = 'nStk4'
laFileStur[27,2] = 'N'
laFileStur[27,3] = 7
laFileStur[27,4] = 0

laFileStur[28,1] = 'nStk5'
laFileStur[28,2] = 'N'
laFileStur[28,3] = 7
laFileStur[28,4] = 0

laFileStur[29,1] = 'nStk6'
laFileStur[29,2] = 'N'
laFileStur[29,3] = 7
laFileStur[29,4] = 0

laFileStur[30,1] = 'nStk7'
laFileStur[30,2] = 'N'
laFileStur[30,3] = 7
laFileStur[30,4] = 0

laFileStur[31,1] = 'nStk8'
laFileStur[31,2] = 'N'
laFileStur[31,3] = 7
laFileStur[31,4] = 0

laFileStur[32,1] = 'cAdjRes'
laFileStur[32,2] = 'C'
laFileStur[32,3] = 6
laFileStur[32,4] = 0


laFileStur[33,1] = 'cSize1'
laFileStur[33,2] = 'C'
laFileStur[33,3] = 5
laFileStur[33,4] = 0

laFileStur[34,1] = 'cSize2'
laFileStur[34,2] = 'C'
laFileStur[34,3] = 5
laFileStur[34,4] = 0

laFileStur[35,1] = 'cSize3'
laFileStur[35,2] = 'C'
laFileStur[35,3] = 5
laFileStur[35,4] = 0

laFileStur[36,1] = 'cSize4'
laFileStur[36,2] = 'C'
laFileStur[36,3] = 5
laFileStur[36,4] = 0

laFileStur[37,1] = 'cSize5'
laFileStur[37,2] = 'C'
laFileStur[37,3] = 5
laFileStur[37,4] = 0

laFileStur[38,1] = 'cSize6'
laFileStur[38,2] = 'C'
laFileStur[38,3] = 5
laFileStur[38,4] = 0

laFileStur[39,1] = 'cSize7'
laFileStur[39,2] = 'C'
laFileStur[39,3] = 5
laFileStur[39,4] = 0

laFileStur[40,1] = 'cSize8'
laFileStur[40,2] = 'C'
laFileStur[40,3] = 5
laFileStur[40,4] = 0

laFileStur[41,1] = 'Scale'
laFileStur[41,2] = 'C'
laFileStur[41,3] = 3
laFileStur[41,4] = 0

laFileStur[42,1] = 'Date'
laFileStur[42,2] = 'D'
laFileStur[42,3] = 8
laFileStur[42,4] = 0

laFileStur[43,1] = 'CWARECODE'
laFileStur[43,2] = 'C'
laFileStur[43,3] = 6
laFileStur[43,4] = 0

laFileStur[44,1] = 'GLFYear'
laFileStur[44,2] = 'C'
laFileStur[44,3] = 4
laFileStur[44,4] = 0

laFileStur[45,1] = 'GLPeriod'
laFileStur[45,2] = 'C'
laFileStur[45,3] = 2
laFileStur[45,4] = 0

laFileStur[46,1] = 'lSelect'
laFileStur[46,2] = 'L'
laFileStur[46,3] = 1
laFileStur[46,4] = 0

laFileStur[47,1] = 'llNetAll'
laFileStur[47,2] = 'L'
laFileStur[47,3] = 1
laFileStur[47,4] = 0
=gfCrtTmp(loFormSet.lcFrstGrdCursor,@laFileStur,'Style',loFormSet.lcFrstGrdCursor ,.T.)

DIMENSION laFileStur[49,4]

laFileStur[48,1] = 'ClrDsc'
laFileStur[48,2] = 'C'
laFileStur[48,3] = 30
laFileStur[48,4] = 0

laFileStur[49,1] = 'OLDStyle'
laFileStur[49,2] = 'C'
laFileStur[49,3] = 19
laFileStur[49,4] = 0

=gfCrtTmp(loFormSet.lcScndGrdCursor,@laFileStur,'OLDStyle',loFormSet.lcScndGrdCursor ,.T.)
SELECT (loFormSet.lcScndGrdCursor)
INDEX on STYLE+OLDStyle TAG 'STYLE_IDX' ADDITIVE 
SET ORDER to (loFormSet.lcScndGrdCursor)

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*! Date      : 11/10/2009
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

CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
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
*! Name      : lfAftrRowColUp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : after row col. of upper grid
*!*************************************************************  
FUNCTION lfAftrRowColUp
PARAMETERS loFormSet
SELECT (loFormSet.lcScndGrdCursor)
IF loFormSet.lcfromscreen ='B'
  SET KEY TO 
  SET KEY TO EVALUATE(loFormSet.lcFrstGrdCursor+'.Style') 
ENDIF   
LOCATE 
IF EOF(loFormSet.lcScndGrdCursor)
  WITH loFormSet.AriaForm1.cntAssQty
    .txtQty1.Enabled = .F.
    .txtQty2.Enabled = .F.
    .txtQty3.Enabled = .F.
    .txtQty4.Enabled = .F.
    .txtQty6.Enabled = .F.
    .txtQty5.Enabled = .F.
    .txtQty7.Enabled = .F.
    .txtQty8.Enabled = .F.
  ENDWITH   
ELSE
  WITH loFormSet.AriaForm1.cntAssQty
    .txtQty1.Enabled = .T.
    .txtQty2.Enabled = .T.
    .txtQty3.Enabled = .T.
    .txtQty4.Enabled = .T.
    .txtQty6.Enabled = .T.
    .txtQty5.Enabled = .T.
    .txtQty7.Enabled = .T.
    .txtQty8.Enabled = .T.
  ENDWITH   
ENDIF   
loFormSet.AriaForm1.grddown.Refresh
loFormSet.AriaForm1.grddown.AfterRowColchange()
loFormSet.AriaForm1.cboAdjReason.Value = EVALUATE(loFormSet.lcFrstGrdCursor+'.cAdjRes')

WITH loFormSet.AriaForm1.cntQtyBrkdwn2
  .Scale = EVALUATE(loFormSet.lcFrstGrdCursor+'.Scale')
  .txtQty1.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov1')
  .txtQty2.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov2')
  .txtQty3.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov3')
  .txtQty4.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov4')
  .txtQty6.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov6')
  .txtQty5.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov5')
  .txtQty7.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov7')
  .txtQty8.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov8')
  .txtTotQty.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.TotMov')
  .mDisableUnused()
  FOR lnI = 1 TO 8
     lcI = STR(lnI,1)
    .txtQty&lcI..Enabled = .T. AND IIF(loFormSet.lcfromscreen <> 'B',EVALUATE(loFormSet.lcFrstGrdCursor+'.lSelect'),.T.)
  ENDFOR 
  loFormSet.AriaForm1.cboAdjReason.Enabled  =  IIF(loFormSet.lcfromscreen <> 'B',EVALUATE(loFormSet.lcFrstGrdCursor+'.lSelect'),.T.)
  IF Scale.cnt < 8
    FOR lnI = Scale.cnt+1 TO 8
       lcI = STR(lnI,1)
      .txtQty&lcI..Enabled = .F.
    ENDFOR 
  ENDIF 
  .refresh
ENDWITH   
WITH loFormSet.AriaForm1.cntQtyBrkdwn1
   .Scale = EVALUATE(loFormSet.lcFrstGrdCursor+'.Scale')
   .txtQty1.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk1')
   .txtQty2.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk2')
   .txtQty3.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk3')
   .txtQty4.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk4')
   .txtQty6.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk6')
   .txtQty5.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk5')
   .txtQty7.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk7')
   .txtQty8.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.Stk8')
   .txtTotQty.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.TotStk')
ENDWITH 
WITH loFormSet.AriaForm1.cntQtyBrkdwn3
   .Scale = EVALUATE(loFormSet.lcFrstGrdCursor+'.Scale')
   .txtQty1.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk1')
   .txtQty2.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk2')
   .txtQty3.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk3')
   .txtQty4.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk4')
   .txtQty6.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk6')
   .txtQty5.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk5')
   .txtQty7.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk7')
   .txtQty8.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.nStk8')
   .txtTotQty.VAlue= EVALUATE(loFormSet.lcFrstGrdCursor+'.TotnStk')
ENDWITH 

loFormSet.AriaForm1.kbSTyle.VAlue = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')
loFormSet.AriaForm1.txtStyDesc.VAlue = EVALUATE(loFormSet.lcFrstGrdCursor+'.Desc')
loFormSet.ariaForm1.cboLocation.VAlue = EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode')
loFormSet.ariaForm1.DtpDate.Value =  EVALUATE(loFormSet.lcFrstGrdCursor+'.Date')


lnTotalMoved  = 0

lnCurTot = 0
IF loFormSet.lcFromScreen = 'C'
  SELECT(loFormSet.lcFrstGrdCursor) 
  lcCurStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')
  lnRecNum = RECNO()
  SUM TOTMOV TO lnTotalMoved FOR !DELETED()
  lnCurTot = ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.TotMov'))
ELSE
  SELECT(loFormSet.lcScndGrdCursor) 
  lnRecNum = RECNO()
  =SEEK(EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'))
  SUM TOTMOV TO lnTotalMoved REST WHILE oldStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style') FOR !DELETED()
  lnCurTot = ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.TotMov'))
ENDIF 
IF BETWEEN(lnRecNum ,1,RECCOUNT())
  GO RECORD lnRecNum 
ENDIF 
IF ABS(lnTotalMoved) <> ABS(lnCurTot)
  loFormSet.AriaForm1.txtTotAss.disabledbackColor = RGB(255,0,0)
ELSE
  loFormSet.AriaForm1.txtTotAss.disabledbackColor= RGB(255,255,255)
ENDIF 
*MT
IF loFormSet.lcFromScreen = 'B'
  loFormSet.AriaForm1.txtTotAss.Value = lnTotalMoved
ELSE
  loFormSet.AriaForm1.txtTotAss.Value = ABS(lnTotalMoved)
ENDIF   
IF ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.TotMov')) <> 0 
  loFormSet.ariaForm1.cboLocation.Enabled = .F. 
ELSE
  loFormSet.ariaForm1.cboLocation.Enabled = .T. 
ENDIF 
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
IF loFormSet.lcRpBusCase = 'OO'  
  LOCAL lnAlias, lnRecNo
  lnAlias = SELECT()
  SELECT (loFormSet.lcScndGrdCursor)  
  lnRecNo = RECNO()
  COUNT FOR !DELETED() TO lnCount
  
  IF lnCount = 0
    loFormSet.AriaForm1.cmdAdd.Enabled = .T.
  ELSE
    loFormSet.AriaForm1.cmdAdd.Enabled = .F.
  ENDIF
  
  IF lnRecNo > 0 AND lnRecNo <= RECCOUNT()
    GOTO lnRecNo
  ENDIF
  SELECT (lnAlias)
ENDIF 
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]

*!*************************************************************
*! Name      : lfGetStyWare
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : Validate location
*!*************************************************************  
FUNCTION lfGetStyWare
PARAMETERS loFormSet
llRetValue = .T.
if loFormSet.lcFromScreen = 'B'
  SELECT StyDYE
  IF gfSeek(EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6))
    SCATTER MEMO MEMVAR FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK 
    SELECT (loFormSet.lcFrstGrdCursor)
    m.TotnStk = 0
    FOR lnC = 1 TO 8 
      lcC = STR(lnC,1)
      m.nStk&lcC. = m.Stk&lcC. - ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov'+lcC))
      m.TotnStk = m.TotnStk + m.nStk&lcC. 
    ENDFOR  
    GATHER MEMO MEMVAR 
    SELECT(loFormSet.lcScndGrdCursor)
    SCAN for OldStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')
      IF gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'STYDYE')
        SELECT StyDYE
        SCATTER MEMO MEMVAR FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK 
        SELECT (loFormSet.lcScndGrdCursor)
        m.TotnStk = 0
        FOR lnC = 1 TO 8 
          lcC = STR(lnC,1)
          m.nStk&lcC. = m.Stk&lcC. + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
          m.TotnStk = m.TotnStk + m.nStk&lcC. 
        ENDFOR  
        GATHER MEMO MEMVAR 
      ELSE
        IF gfModalGen('QRM34048B42018','DIALOG',EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+'|'+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)) = 1 
          DO gpAdStyWar WITH EVALUATE(loFormSet.lcScndGrdCursor+'.Style'),SPACE(10),PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)
          =gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'StyDYE')
          SELECT(loFormSet.lcScndGrdCursor)
          m.TOTSTK = 0
          m.TotnStk = 0
          FOR lnC = 1 TO 8 
            lcC = STR(lnC,1)
            m.Stk&lcC = 0
            m.nStk&lcC. = m.Stk&lcC + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
            m.TotnStk = m.TotnStk + m.nStk&lcC. 
          ENDFOR  
          GATHER MEMO MEMVAR 
        ELSE
          SELECT(loFormSet.lcScndGrdCursor)
          loFormSet.AriaForm1.cmdRemove.Click()
        ENDIF 
      ENDIF   
    ENDSCAN 
    llRetValue = .T.
  ELSE
    IF gfModalGen('QRM34048B34004','DIALOG',EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')+'|'+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)) = 1
      DO gpAdStyWar WITH EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'),SPACE(10),PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)
      =gfSeek(EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6))
      SELECT (loFormSet.lcFrstGrdCursor)
      m.TOTSTK = 0
      m.TotnStk = 0
      FOR lnC = 1 TO 8 
        lcC = STR(lnC,1)
        m.Stk&lcC = 0
          m.nStk&lcC. = m.Stk&lcC - ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov'+lcC))
          m.TotnStk = m.TotnStk + m.nStk&lcC. 
        ENDFOR  
        GATHER MEMO MEMVAR 
        *MT
        SELECT(loFormSet.lcScndGrdCursor)
        SCAN For IIF(loFormSet.lcFromScreen = 'B',OldStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'),.T.)
          IF gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'StyDYE')
            SELECT StyDYE
            SCATTER MEMO MEMVAR FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK 
            SELECT (loFormSet.lcScndGrdCursor)
            m.TotnStk = 0
            FOR lnC = 1 TO 8 
              lcC = STR(lnC,1)
              m.nStk&lcC. = m.Stk&lcC. + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
              m.TotnStk = m.TotnStk + m.nStk&lcC. 
            ENDFOR  
            GATHER MEMO MEMVAR 
          ELSE
            IF gfModalGen('QRM34048B42018','DIALOG',EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+'|'+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)) = 1 
              DO gpAdStyWar WITH EVALUATE(loFormSet.lcScndGrdCursor+'.Style'),SPACE(10),PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)
              =gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'StyDYE')
              SELECT(loFormSet.lcScndGrdCursor)
              m.TOTSTK = 0
              m.TotnStk = 0
              FOR lnC = 1 TO 8 
                lcC = STR(lnC,1)
                m.Stk&lcC = 0
                m.nStk&lcC. = m.Stk&lcC + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
                m.TotnStk = m.TotnStk + m.nStk&lcC. 
              ENDFOR  
              GATHER MEMO MEMVAR 
            ELSE
              SELECT(loFormSet.lcScndGrdCursor)
              loFormSet.AriaForm1.cmdRemove.Click()
            ENDIF 
          ENDIF   
        ENDSCAN 
        *MT    
        llRetValue = .T.
      ELSE
        llRetValue = .F.
      ENDIF
    ENDIF 
    return llRetValue  
  ELSE
    SELECT(loFormSet.lcScndGrdCursor)
    SET KEY TO
    SELECT(loFormSet.lcFrstGrdCursor)
    LOCATE
    SCAN  
      SELECT StyDYE
      IF gfSeek(EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6))
        SCATTER MEMO MEMVAR FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK 
        SELECT (loFormSet.lcFrstGrdCursor)
        m.TotnStk = 0
        FOR lnC = 1 TO 8 
          lcC = STR(lnC,1)
          m.nStk&lcC. = m.Stk&lcC. - ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov'+lcC))
          m.TotnStk = m.TotnStk + m.nStk&lcC. 
        ENDFOR  
        GATHER MEMO MEMVAR 
        *MT
        SELECT(loFormSet.lcScndGrdCursor)
        SCAN for IIF(loFormSet.lcFromScreen = 'B',OldStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'),.T.)
          IF gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'StyDYE')
            SELECT StyDYE
            SCATTER MEMO MEMVAR FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK 
            SELECT (loFormSet.lcScndGrdCursor)
            m.TotnStk = 0
            FOR lnC = 1 TO 8 
              lcC = STR(lnC,1)
              m.nStk&lcC. = m.Stk&lcC. + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
              m.TotnStk = m.TotnStk + m.nStk&lcC. 
            ENDFOR  
            GATHER MEMO MEMVAR 
          ELSE
            IF gfModalGen('QRM34048B42018','DIALOG',EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+'|'+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)) = 1 
              DO gpAdStyWar WITH EVALUATE(loFormSet.lcScndGrdCursor+'.Style'),SPACE(10),PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)
              =gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'StyDYE')
              SELECT(loFormSet.lcScndGrdCursor)
              m.TOTSTK = 0
              m.TotnStk = 0
              FOR lnC = 1 TO 8 
                lcC = STR(lnC,1)
                m.Stk&lcC = 0
                m.nStk&lcC. = m.Stk&lcC + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
                m.TotnStk = m.TotnStk + m.nStk&lcC. 
              ENDFOR  
              GATHER MEMO MEMVAR 
            ELSE
              SELECT(loFormSet.lcScndGrdCursor)
              loFormSet.AriaForm1.cmdRemove.Click()
            ENDIF 
          ENDIF   
        ENDSCAN 
        llRetValue = .T.
      ELSE
        IF gfModalGen('QRM34048B34004','DIALOG',EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')+'|'+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)) = 1
          DO gpAdStyWar WITH EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'),SPACE(10),PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)
          =gfSeek(EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6))
          SELECT (loFormSet.lcFrstGrdCursor)
          m.TOTSTK = 0
          m.TotnStk = 0
          FOR lnC = 1 TO 8 
            lcC = STR(lnC,1)
            m.Stk&lcC = 0
            m.nStk&lcC. = m.Stk&lcC - ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.Mov'+lcC))
            m.TotnStk = m.TotnStk + m.nStk&lcC. 
          ENDFOR  
          GATHER MEMO MEMVAR 
          SELECT(loFormSet.lcScndGrdCursor)
          SCAN For IIF(loFormSet.lcFromScreen = 'B',OldStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'),.T.)
            IF gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'StyDYE')
              SELECT StyDYE
              SCATTER MEMO MEMVAR FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK 
              SELECT (loFormSet.lcScndGrdCursor)
              m.TotnStk = 0
              FOR lnC = 1 TO 8 
                lcC = STR(lnC,1)
                m.nStk&lcC. = m.Stk&lcC. + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
                m.TotnStk = m.TotnStk + m.nStk&lcC. 
              ENDFOR  
              GATHER MEMO MEMVAR 
            ELSE
              IF gfModalGen('QRM34048B42018','DIALOG',EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+'|'+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)) = 1 
                DO gpAdStyWar WITH EVALUATE(loFormSet.lcScndGrdCursor+'.Style'),SPACE(10),PADR(loFormSet.ariaForm1.cboLocation.VAlue,6)
                =gfSeek(EVALUATE(loFormSet.lcScndGrdCursor+'.Style')+PADR(loFormSet.ariaForm1.cboLocation.VAlue,6),'StyDYE')
                SELECT(loFormSet.lcScndGrdCursor)
                m.TOTSTK = 0
                m.TotnStk = 0
                FOR lnC = 1 TO 8 
                  lcC = STR(lnC,1)
                  m.Stk&lcC = 0
                  m.nStk&lcC. = m.Stk&lcC + ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.Mov'+lcC))
                  m.TotnStk = m.TotnStk + m.nStk&lcC. 
                ENDFOR  
                GATHER MEMO MEMVAR 
              ELSE
                SELECT(loFormSet.lcScndGrdCursor)
                loFormSet.AriaForm1.cmdRemove.Click()
              ENDIF 
            ENDIF   
          ENDSCAN 
          llRetValue = .T.
        ELSE
          llRetValue = .F.
          EXIT 
        ENDIF
      ENDIF 
    ENDSCAN 
  ENDIF 
RETURN llRetValue 

*!*************************************************************
*! Name      : lfAfterRowColLow
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : After row col of low grid
*!*************************************************************  
FUNCTION lfAfterRowColLow
PARAMETERS loFormSet

WITH loFormSet.AriaForm1.cntAssQty
  .Scale = EVALUATE(loFormSet.lcScndGrdCursor+'.Scale')
  .txtQty1.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk1')
  .txtQty2.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk2')
  .txtQty3.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk3')
  .txtQty4.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk4')
  .txtQty6.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk6')
  .txtQty5.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk5')
  .txtQty7.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk7')
  .txtQty8.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.nStk8')
  .txtTotQty.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.TotnStk')
  FOR lnI = 1 TO 8
     lcI = STR(lnI,1)
    .txtQty&lcI..Enabled = .f.
  ENDFOR 
ENDWITH 

WITH loFormSet.AriaForm1.cntMovPls
  .Scale = EVALUATE(loFormSet.lcScndGrdCursor+'.Scale')
  .txtQty1.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov1')
  .txtQty2.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov2')
  .txtQty3.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov3')
  .txtQty4.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov4')
  .txtQty6.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov6')
  .txtQty5.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov5')
  .txtQty7.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov7')
  .txtQty8.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.Mov8')
  .txtTotQty.VAlue= EVALUATE(loFormSet.lcScndGrdCursor+'.TotMov')
   FOR lnI = 1 TO 8
       lcI = STR(lnI,1)
      .txtQty&lcI..Enabled =(EVALUATE(loFormSet.lcFrstGrdCursor+'.TotMov') <> 0) and !EOF(loFormSet.lcScndGrdCursor)
    ENDFOR 
    IF Scale.cnt < 8
      FOR lnI = Scale.cnt+1 TO 8
         lcI = STR(lnI,1)
        .txtQty&lcI..Enabled = .F.
      ENDFOR 
    ENDIF 
    IF EOF(loFormSet.lcScndGrdCursor)
      loFormSet.AriaForm1.cmdRemove.Enabled = .F.
    else  
      loFormSet.AriaForm1.cmdRemove.Enabled = .t.
    ENDIF 
    .refresh  
ENDWITH 

WITH loFormSet.AriaForm1
  .kbStyleMajor.KeyTextBox.VAlue  = EVALUATE(loFormSet.lcScndGrdCursor+'.cStyMajor')
  .kbNonMajor.KeyTextBox.VAlue = EVALUATE(loFormSet.lcScndGrdCursor+'.Color')
  .txtClrDsc.VAlue = EVALUATE(loFormSet.lcScndGrdCursor+'.ClrDsc')
  .txtDesc.VAlue = EVALUATE(loFormSet.lcScndGrdCursor+'.Desc')  
ENDWITH 


IF loFormSet.lcfromscreen = 'B'
  lnTotalMoved  = 0
  SELECT(loFormSet.lcScndGrdCursor) 
  lnRecNum = RECNO()
  =SEEK(EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'))
  SUM TOTMOV TO lnTotalMoved REST WHILE oldStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style') FOR !DELETED()
  IF BETWEEN(lnRecNum ,1,RECCOUNT())
    GO RECORD lnRecNum 
  ENDIF 
  IF ABS(lnTotalMoved) <> ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.TotMov'))
    loFormSet.AriaForm1.txtTotAss.disabledbackColor = RGB(255,0,0)
  ELSE
    loFormSet.AriaForm1.txtTotAss.disabledbackColor= RGB(255,255,255)
  ENDIF 
  If ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.TotMov')) <> 0 
	loFormSet.ariaForm1.cboLocation.Enabled = .F. 
  ELSE
	loFormSet.ariaForm1.cboLocation.Enabled = .T. 
  ENDIF   
ELSE
  lnTotalMoved  = 0
  SELECT(loFormSet.lcFrstGrdCursor) 
  lcCurSty =  EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')  
  lnRecNum = RECNO()
  SUM TOTMOV TO lnTotalMoved FOR !DELETED()
  IF BETWEEN(lnRecNum ,1,RECCOUNT())
    GO RECORD lnRecNum 
  ENDIF 
  IF ABS(lnTotalMoved) <> ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.TotMov'))
    loFormSet.AriaForm1.txtTotAss.disabledbackColor = RGB(255,0,0)
  ELSE
    loFormSet.AriaForm1.txtTotAss.disabledbackColor= RGB(255,255,255)
  ENDIF 
  if lnTotalMoved  <> 0 
    loFormSet.ariaForm1.cboLocation.Enabled = .F. 
  ELSE
    loFormSet.ariaForm1.cboLocation.Enabled = .T. 
  ENDIF 

ENDIF

*!*************************************************************
*! Name      : lfRemoveStyle
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : Remove Style 
*!*************************************************************  
FUNCTION lfRemoveStyle
PARAMETERS loFormSet
SELECT(loFormSet.lcScndGrdCursor)
loFormSet.AriaForm1.txtTotAss.Value = loFormSet.AriaForm1.txtTotAss.Value - EVALUATE(loFormSet.lcScndGrdCursor+'.TotMov')
DELETE
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*IF loFormSet.lcFromScreen = 'C'
*  loFormSet.AriaForm1.cmdAdd.Enabled = .T.
*ENDIF
IF loFormSet.lcFromScreen = 'C' OR loFormSet.lcRpBusCase = 'OO'
  loFormSet.AriaForm1.cmdAdd.Enabled = .T.
ENDIF
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
*!*************************************************************
*! Name      : lfAddSty
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : add Style 
*!*************************************************************  
FUNCTION lfAddSty
PARAMETERS loFormSet
DIMENSION laSelected[1]
lnClrPosAv  = loFormSet.lnClrPosAv  
lnClrLnAv  = loFormSet.lenclrlen 
SELECT Style 
=gfSeek('')
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*IF loFormSet.lcFromScreen = 'B' AND !loFormSet.llRpSamSt
  *lcSeleCur = gfTempName()
  *llStySelected  =gfstybrw('N',"","",.F.,lcSeleCur,'STYLE')
IF loFormSet.lcRpBusCase = 'OM'
  lcSeleCur = gfTempName()
  lcBrowFlds = [Style :R :25 :H='Style  -Color',DESC :R :H= 'Description', SEASON :R :H= 'Season',;
                CDIVISION :R :H= 'Division', PRICEA :R :H= 'Price', FABRIC :R :H= 'Fabric']  

  llStySelected = gfBrowse(lcBrowFlds, "Style - Color", "STYLE", "", .F., .F., .T., .F., .F., .F.,;
                        lcSeleCur, "STYLE", .F., .F., .F., .F., .F., .F., "STYLE")
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]  
ELSE
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
  *lcSelected  =gfstybrw('N',"","",.F.,.F.,'STYLE')
  lcSelected  =gfStyBrw('N',"","",.F.)
  *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
ENDIF   
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*IF loFormSet.lcFromScreen = 'B' AND !loFormSet.llRpSamSt
IF loFormSet.lcRpBusCase = 'OM'
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  if llStySelected AND !EMPTY(lcSeleCur) AND USED(lcSeleCur)
    SELECT(lcSeleCur)
    LOCATE 
    SCAN 
      lcSelected = &lcSeleCur..Style
      IF !EMPTY(lcSelected)
        IF SEEK(lcSelected+ EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'),loFormSet.lcScndGrdCursor,'STYLE_IDX')
          =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Style is already entered before")
          RETURN 
        ENDIF 
        SELECT STYDYE 
        lcStyle = lcSelected
        IF !gfSeek(lcStyle +EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'))
          IF gfModalGen('QRM34048B42018','DIALOG',lcStyle +'|'+PADR(EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'),6)) = 1 
             DO gpAdStyWar WITH lcStyle ,SPACE(10),PADR(EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'),6)
             SELECT STYDYE 
             =gfSeek(lcStyle +EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'))
          ELSE
            RETURN 
          ENDIF   
        ENDIF 
        SELECT STYDYE 
        SCATTER MEMO MEMVAR 
        =gfSeek(lcStyle ,'Style','Style')
        m.cStyMajor = Style.cStyMajor
        m.Color = SUBSTR(m.Style,lnClrPosAv  ,lnClrLnAv)
        m.TotNStk = m.TotStk
        FOR lnI = 1 TO 8 
          lcI = STR(lnI,1)
          m.nStk&lcI = m.Stk&lcI 
        ENDFOR  
        =gfSeek('S'+Style.Scale,'Scale')
        m.Scale = Scale.SCALE
        FOR lnJ = 1 TO 8
          lcJ = STR(lnJ,1)
           m.cSize&lcJ. = Scale.SZ&lcJ.
        ENDFOR 
           
         FOR lnJ = 1 TO 8
           lcJ = STR(lnJ,1)
           m.Mov&lcJ. = 0
         ENDFOR 
         m.TotMov = 0
         m.Date = oAriaApplication.SYSTEMDATE
         m.OLDStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')
         m.ClrDsc = gfCodDes(m.Color,'COLOR')
         m.cAdjRes = loFormSet.ariaform1.cboAdjReason.Value 
         m.GLFYear = loFormSet.lcGlFYear 
         m.GLPeriod = loFormSet.lcGlPeriod
         m.CWARECODE = loFormSet.ariaform1.cbolocation.Value
         m.Date = loFormSet.ariaform1.dtpDate.VAlue
         *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
         m.Desc = STYLE.Desc
         *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
         INSERT INTO (loFormSet.lcScndGrdCursor) FROM MEMVAR          
         *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
         *IF loFormSet.lcFromScreen = 'C'           
         *  loFormSet.AriaForm1.cmdAdd.Enabled = .F.           
         *ENDIF
         IF loFormSet.lcFromScreen = 'C' OR loFormSet.lcRpBusCase = 'OO'
           loFormSet.AriaForm1.cmdAdd.Enabled = .F.
         ENDIF
         *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
      ENDIF                
    ENDSCAN 
  ENDIF 
ELSE

  IF !EMPTY(lcSelected)
    IF SEEK(lcSelected+ EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'),loFormSet.lcScndGrdCursor,'STYLE_IDX')
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Style is already entered before")
      RETURN 
    ENDIF 
    SELECT STYDYE 
    lcStyle = lcSelected
    IF !gfSeek(lcStyle +EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'))
      IF gfModalGen('QRM34048B42018','DIALOG',lcStyle +'|'+PADR(EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'),6)) = 1 
         DO gpAdStyWar WITH lcStyle ,SPACE(10),PADR(EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'),6)
         SELECT STYDYE 
         =gfSeek(lcStyle +EVALUATE(loFormSet.lcFrstGrdCursor+'.cWareCode'))
      ELSE
        RETURN 
      ENDIF   
    ENDIF 
    SELECT STYDYE 
    SCATTER MEMO MEMVAR 
    =gfSeek(lcStyle ,'Style','Style')
    m.cStyMajor = Style.cStyMajor
    m.Color = SUBSTR(m.Style,lnClrPosAv  ,lnClrLnAv)
    m.TotNStk = m.TotStk
    FOR lnI = 1 TO 8 
      lcI = STR(lnI,1)
      m.nStk&lcI = m.Stk&lcI 
    ENDFOR  
    =gfSeek('S'+Style.Scale,'Scale')
    m.Scale = Scale.SCALE
    FOR lnJ = 1 TO 8
      lcJ = STR(lnJ,1)
       m.cSize&lcJ. = Scale.SZ&lcJ.
    ENDFOR 
       
     FOR lnJ = 1 TO 8
       lcJ = STR(lnJ,1)
       m.Mov&lcJ. = 0
     ENDFOR 
     m.TotMov = 0
     m.Date = oAriaApplication.SYSTEMDATE
     m.OLDStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style')
     m.ClrDsc = gfCodDes(m.Color,'COLOR')
     m.cAdjRes = loFormSet.ariaform1.cboAdjReason.Value 
     m.GLFYear = loFormSet.lcGlFYear 
     m.GLPeriod = loFormSet.lcGlPeriod
     m.CWARECODE = loFormSet.ariaform1.cbolocation.Value
     m.Date = loFormSet.ariaform1.dtpDate.VAlue
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
     m.Desc = STYLE.Desc
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
     INSERT INTO (loFormSet.lcScndGrdCursor) FROM MEMVAR      
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
     *IF loFormSet.lcFromScreen = 'C'           
     *  loFormSet.AriaForm1.cmdAdd.Enabled = .F.           
     *ENDIF
     IF loFormSet.lcFromScreen = 'C' OR loFormSet.lcRpBusCase = 'OO'
       loFormSet.AriaForm1.cmdAdd.Enabled = .F.
     ENDIF
     *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
  ENDIF                
ENDIF 
*!*************************************************************
*! Name      : lfBeforeSave
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : Before Save 
*!*************************************************************  
*B609097,1 TMI 11/25/2009 08:16:10 PM [Start] change the function name to not meet the one in SMCODES.PRG to avoide problems
*FUNCTION lfBeforeSave
FUNCTION lfBRI_BeforeSave
*B609097,1 TMI 11/25/2009 08:16:11 PM [End  ] 
PARAMETERS loFormSet


IF !loFormSet.llVldDate 
  lnRecNm = RECNO(loFormSet.lcFrstGrdCursor)
  lcRecNum = RECNO(loFormSet.lcscndgrdcursor)
  SELECT(loFormSet.lcscndgrdcursor)
  SET KEY TO
  lcGlFYear  = loFormSet.lcGlFYear
  lcGlPeriod = loFormSet.lcGlPeriod
  *--Check posting date to be in valid period.
  IF !CHECKPRD(loFormSet.ariaForm1.dtpDate.Value,'lcGLFYear','lcGLPeriod','IA')
    loFormSet.ariaForm1.dtpDate.Value = oAriaApplication.SystemDate
    =CHECKPRD(loFormSet.ariaForm1.dtpDate.Value,'lcGLFYear','lcGLPeriod','IA',.T.)
    loFormSet.llVldDate = .F.
    RETURN .F.
  ELSE
    loFormSet.lcGlFYear = lcGlFYear
    loFormSet.lcGlPeriod = lcGlPeriod
    loFormSet.llVldDate = .T.
  ENDIF
  SELECT(loFormSet.lcFrstGrdCursor)
  REPLACE date     WITH loFormSet.ariaForm1.dtpDate.Value,;
      GLPeriod WITH lcGlPeriod,;
      GLFYear  With lcGlFYear all 
      
  lcRecNum = RECNO(loFormSet.lcscndgrdcursor)
  REPLACE date     WITH loFormSet.ariaForm1.dtpDate.Value,;
      GLPeriod WITH lcGlPeriod,;
      GLFYear  With lcGlFYear all 
      
  IF BETWEEN(lnRecNm,1,RECCOUNT(loFormSet.lcFrstGrdCursor))
    GO RECORD  lnRecNm in (loFormSet.lcFrstGrdCursor)
  ENDIF 
  IF BETWEEN(lcRecNum ,1,RECCOUNT(loFormSet.lcscndgrdcursor))
    GO RECORD  lcRecNum in (loFormSet.lcscndgrdcursor)
  ENDIF 
ENDIF 

SELECT(loFormSet.lcScndGrdCursor) 
lnScndRec = RECNO()
SET KEY TO 
SELECT(loFormSet.lcFrstGrdCursor)
lnFrstRec = RECNO()
llDontSv = .F.
LOCATE FoR TotMov <> 0
IF !FOUND()
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No Movements done. cannot save")
  llDontSv =  .T.
  RETURN .F.
ELSE
  IF loFormSet.lcfromscreen = 'B' 
    SCAN FOR TotMov <> 0
      lnTotalMoved  = 0
      SELECT(loFormSet.lcScndGrdCursor) 
      =SEEK(EVALUATE(loFormSet.lcFrstGrdCursor+'.Style'))
      SUM TOTMOV TO lnTotalMoved REST WHILE oldStyle = EVALUATE(loFormSet.lcFrstGrdCursor+'.Style') FOR !DELETED()
      IF lnTotalMoved <> ABS(EVALUATE(loFormSet.lcFrstGrdCursor+'.TotMov'))
        llDontSv =  .T.
        EXIT 
      ENDIF 
    ENDSCAN 
  ELSE
    SELECT(loFormSet.lcFrstGrdCursor) 
    LOCATE
    SUM TOTMOV TO lnTotalMoved FOR !DELETED()
    IF ABS(lnTotalMoved) <> ABS(EVALUATE(loFormSet.lcScndGrdCursor+'.TotMov'))
      llDontSv =  .T.
    ENDIF 
  ENDIF    
ENDIF   
IF llDontSv 
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Total Movement(-) for some assortment styles is not equal to the Total Movement(+) for the related breakdown styles. cannot save")
  SELECT(loFormSet.lcScndGrdCursor) 
  if BETWEEN(lnScndRec ,1,RECCOUNT())
    GO record lnScndRec 
  ENDIF 
  SELECT(loFormSet.lcFrstGrdCursor) 
  if BETWEEN(lnFrstRec ,1,RECCOUNT())
    GO record lnFrstRec 
  ENDIF 
  loFormSet.AriaForm1.grdUpp.Refresh
  loFormSet.AriaForm1.grdUpp.AfterRowColchange()
  RETURN .F.
ELSE
lnSel = 0
  DO WHILE lnSel < 3 &&-- after view/print do the same screen for print/view 
                       &&-- or post or cancel .
    lnSel =gfModalGen('TRM00000B42023',.F.,.F.,.F.,"Would you like to print the Style Assortment log ?")
    IF lnSel=1 OR lnSel=2 &&-- if preview/print only
      *Call Report
      lcPriTemp = gfTempName()
      SELECT STYINVJL
      =AFIELDS(laFileStru)
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+6,18]


      
      laFileStru[lnFileStru+1,1] = 'STYMAJOR'
      laFileStru[lnFileStru+1,2] = 'C'
      laFileStru[lnFileStru+1,3] = loFormSet.lnmajlength 
      laFileStru[lnFileStru+1,4] = 0
        
      laFileStru[lnFileStru+2,1] = 'COLOR'
      laFileStru[lnFileStru+2,2] = 'C'
      laFileStru[lnFileStru+2,3] = loFormSet.lenclrlen
      laFileStru[lnFileStru+2,4] = 0   
      
      laFileStru[lnFileStru+3,1] = 'TotOrg'
      laFileStru[lnFileStru+3,2] = 'N'
      laFileStru[lnFileStru+3,3] = 7
      laFileStru[lnFileStru+3,4] = 0  
      
      laFileStru[lnFileStru+4,1] = 'TotRem'
      laFileStru[lnFileStru+4,2] = 'N'
      laFileStru[lnFileStru+4,3] = 7
      laFileStru[lnFileStru+4,4] = 0        
      
      laFileStru[lnFileStru+5,1] = 'cReasDesc'
      laFileStru[lnFileStru+5,2] = 'C'
      laFileStru[lnFileStru+5,3] = 30
      laFileStru[lnFileStru+5,4] = 0       
    
      laFileStru[lnFileStru+6,1] = 'SCALE'
      laFileStru[lnFileStru+6,2] = 'C'
      laFileStru[lnFileStru+6,3] = 3
      laFileStru[lnFileStru+6,4] = 0      

      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+8,18]
      FOR lnx = 1 TO 8 
        lcX = ALLTRIM(STR(lnX))
        laFileStru[lnFileStru+lnx,1] = 'Org'+lcX
        laFileStru[lnFileStru+lnx,2] = 'N'
        laFileStru[lnFileStru+lnx,3] = 7
        laFileStru[lnFileStru+lnx,4] = 0
      ENDFOR 
      
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+8,18]
      FOR lnx = 1 TO 8
        lcX = ALLTRIM(STR(lnX))
        laFileStru[lnFileStru+lnx,1] = 'Rem'+lcX
        laFileStru[lnFileStru+lnx,2] = 'N'
        laFileStru[lnFileStru+lnx,3] = 7
        laFileStru[lnFileStru+lnx,4] = 0
      ENDFOR
      
      FOR lnCount = 1 TO ALEN(laFileStru,1)
        STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
          laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
          laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
          laFileStru[lnCount,16]
        STORE 0  TO laFileStru[lnCount,17],laFileStru[lnCount,18]
      ENDFOR  
      
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'CSESSION+CIRTYPE+STYLE'
      laIndex[1,2] = 'SESNTYP'
      
      =gfCrtTmp(lcPriTemp,@laFileStru,@laIndex)
      lcFrstGrdCursor = loFormSet.lcFrstGrdCursor
      lcScndGrdCursor = loFormSet.lcScndGrdCursor
      *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
      *IF loFormSet.lcfromscreen = 'B'       
      IF .F.
      *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
        SELECT(lcFrstGrdCursor)
        LOCATE 
        lcTempSess = 'X'
        lnCnt = 1
        SCAN FOR TOTMOV <> 0 and !DELETED()
          m.cSession = PADR(lcTempSess+allt(STR(lnCnt)),6,'0')
          m.ciSession = m.cSession 
          m.crSeesion = ''
          m.STYMAJOR = &lcFrstGrdCursor..cStyMajor
          m.Style = &lcFrstGrdCursor..Style
          m.Color = &lcFrstGrdCursor..Color
          m.cWareCode = &lcFrstGrdCursor..cWareCode
          m.Ctrtype = '1'
          m.cirType = 'I'
          m.DtrDate = &lcFrstGrdCursor..Date
          M.sCALE =  &lcFrstGrdCursor..sCALE
          for lnC = 1 to 8
            lcC = STR(lnC,1)
            m.nStk&lcC. = &lcFrstGrdCursor..Mov&lcC.
            m.Rem&lcC.  = &lcFrstGrdCursor..nStk&lcC. 
            m.Org&lcC.  = &lcFrstGrdCursor..Stk&lcC.
          ENDFOR 
          m.nTotStk = &lcFrstGrdCursor..TOtMov
          m.TotRem  = &lcFrstGrdCursor..TotnStk
          m.TotOrg  = &lcFrstGrdCursor..TotStk
          m.cReasDesc = gfCodDes(PADR(&lcFrstGrdCursor..cAdjRes,6),'CADJREASON')
          insert into (lcPriTemp) from memvar 
          SELECT(lcScndGrdCursor)
          SCAN FOR oldStyle = &lcFrstGrdCursor..Style and TOTMOV <> 0 and !DELETED()
             m.ciSession = ''
             m.crSeesion = m.cSession 
             m.STYMAJOR = &lcScndGrdCursor..cStyMajor
             m.Style = &lcScndGrdCursor..Style
             m.Color = &lcScndGrdCursor..Color
             m.cWareCode = &lcScndGrdCursor..cWareCode
             M.sCALE =  &lcScndGrdCursor..sCALE
             m.Ctrtype = '1'
             m.cirType = 'R'
             m.DtrDate = &lcScndGrdCursor..Date
             for lnC = 1 to 8
               lcC = STR(lnC,1)
               m.nStk&lcC. = &lcScndGrdCursor..Mov&lcC.
               m.Rem&lcC.  = &lcScndGrdCursor..nStk&lcC. 
               m.Org&lcC.  = &lcScndGrdCursor..Stk&lcC.
             ENDFOR 
             m.nTotStk = &lcScndGrdCursor..TOtMov
             m.TotRem  = &lcScndGrdCursor..TotnStk
             m.TotOrg  = &lcScndGrdCursor..TotStk
             m.cReasDesc = gfCodDes(PADR(&lcScndGrdCursor..cAdjRes,6),'CADJREASON')
             insert into (lcPriTemp) from memvar 
          ENDSCAN 
          lnCnt = lnCnt + 1
        ENDSCAN  
      ELSE
        SELECT(lcScndGrdCursor)
        lcTempSess = 'X'
        lnCnt = 1
        SCAN FOR TOTMOV <> 0 and !DELETED()  
          m.cSession  = PADR(lcTempSess+allt(STR(lnCnt)),6,'0')
          m.crSeesion = m.cSession 
          m.ciSession = ''
          m.STYMAJOR  = &lcScndGrdCursor..cStyMajor
          m.Style     = &lcScndGrdCursor..Style
          m.Color     = &lcScndGrdCursor..Color
          m.cWareCode = &lcScndGrdCursor..cWareCode
          m.CTRTYPE   = '1'
          m.CIRTYPE   = 'I'
          M.sCALE =  &lcScndGrdCursor..sCALE
          m.DtrDate   = &lcScndGrdCursor..Date
          FOR lnC = 1 TO 8
            lcC = STR(lnC,1)
            m.nStk&lcC. = &lcScndGrdCursor..Mov&lcC.
            m.Rem&lcC.  = &lcScndGrdCursor..nStk&lcC. 
            *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
            IF SEEK(&lcScndGrdCursor..Style, lcFrstGrdCursor)
              m.Rem&lcC. = m.Rem&lcC. + &lcFrstGrdCursor..Mov&lcC.
            ENDIF
            *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
            m.Org&lcC.  = &lcScndGrdCursor..Stk&lcC.
          ENDFOR 
          m.nTotStk = &lcScndGrdCursor..TOtMov
          m.TotRem  = &lcScndGrdCursor..TotnStk
          m.TotOrg  = &lcScndGrdCursor..TotStk
          m.cReasDesc = gfCodDes(&lcScndGrdCursor..cAdjRes,'CADJREASON')
          INSERT INTO (lcPriTemp) FROM MEMVAR
          SELECT(lcFrstGrdCursor)
          *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
          *Scan FOR lSelect and TOTMOV <> 0 and !DELETED()
          lcForExp = ''
          IF loFormSet.lcRpBusCase = 'MO'
            lcForExp = 'lSelect AND TOTMOV <> 0 and !DELETED()'
          ELSE
            lcForExp = 'TOTMOV <> 0 and !DELETED()'
          ENDIF
          SCAN FOR &lcForExp.
          *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
          
            m.ciSession = m.cSession 
            m.crSeesion = ''
            M.sCALE =  &lcFrstGrdCursor..sCALE
            m.STYMAJOR  = &lcFrstGrdCursor..cStyMajor
            m.Style     = &lcFrstGrdCursor..Style
            m.Color     = &lcFrstGrdCursor..Color
            m.cWareCode = &lcFrstGrdCursor..cWareCode
            m.CTRTYPE   = '1'
            m.CIRTYPE   = 'R'
            m.DtrDate   = &lcFrstGrdCursor..Date
            FOR lnC = 1 TO 8
              lcC = STR(lnC,1)
              m.nStk&lcC. = &lcFrstGrdCursor..Mov&lcC.
              m.Rem&lcC.  = &lcFrstGrdCursor..nStk&lcC. 
              m.Org&lcC.  = &lcFrstGrdCursor..Stk&lcC.
            ENDFOR 
            m.nTotStk = &lcFrstGrdCursor..TOtMov
            m.TotRem  = &lcFrstGrdCursor..TotnStk
            m.TotOrg  = &lcFrstGrdCursor..TotStk
            m.cReasDesc = gfCodDes(&lcFrstGrdCursor..cAdjRes,'CADJREASON')
            INSERT INTO (lcPriTemp) FROM MEMVAR 
          ENDSCAN 
          lnCnt = lnCnt + 1  
        ENDSCAN 
      ENDIF  
    SELECT(lcPriTemp) 
    LOCATE 
    SET RELATION TO 'S'+SCALE INTO SCALE
    if lnSel <> 2 
      lcPreviewTitle = IIF(TYPE('lcOGWinTitl')='C',lcOGWinTitl,'Report') + " Preview"
      lnScaleMode = _Screen.ScaleMode
      _Screen.ScaleMode = 0    && Foxel.
      lcWindName = gfTempName()
      lcIconFile = _Screen.Icon
      lcCriteria = 'ALL'
      DEFINE WINDOW (lcWindName) FROM 0,0 TO _Screen.Height - 2, _Screen.Width ;
       FONT "Courier New",9 TITLE (lcPreviewTitle)
      _screen.ScaleMode = lnScaleMode
      *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
      *IF oAriaApplication.MULTIINST 
      *  REPORT FORM ("X:\aria4xp\REPORTS"+"\IC\ICSTYASS") PREVIEW WINDOW (lcWindName) &lcCriteria. 
      *ELSE 
      *  REPORT FORM (oAriaApplication.ReportHome+"\IC\ICSTYASS") PREVIEW WINDOW (lcWindName) &lcCriteria. 
      *ENDIF 
      REPORT FORM (oAriaApplication.ClientReportHome+"\IC\ICSTYASS") PREVIEW WINDOW (lcWindName) &lcCriteria. 
      *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
    ELSE
      LOCAL lnDataSess, lcDevice, lcClassDir, oOptionGrid
      lcDevice   = oAriaApplication.gcDevice
      oAriaApplication.gcDevice = 'PRINTER'
      lnDataSess = SET("Datasession")
      lcClassDir   = ADDBS(oAriaApplication.ClassDir)
      oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
      loOGScroll   = oOptionGrid.OptionGrid.oHost
      lcOGPlatForm = ''
      LoOGScroll.lUsePDFViewer = .F.
      loOgScroll.lcOGPlatForm  =   ''
      loOgScroll.lcOGLastForm  = 'ICSTYASS'
      loOGScroll.llPrintPDF = .F.
      loogScroll.cCROrientation = 'P'
      LoOGScroll.llCrystal = .F.
      loOGScroll.lcOGPlatform = 'WINDOWS'
      lcOGPlatForm = 'WINDOWS'
      DIMENSION loOGScroll.laSelFile[1,3]
      loOGScroll.laSelFile = ''
      GO TOP
      *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
      *IF oAriaApplication.MULTIINST 
      *  =gfDispRe("X:\aria4xp\REPORTS"+"\IC\ICSTYASS")
      *ELSE 
      *  =gfDispRe(oAriaApplication.ReportHome+"\IC\ICSTYASS")
      *ENDIF   
      =gfDispRe(oAriaApplication.ClientReportHome+"\IC\ICSTYASS")
      *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
      loOGScroll.Parent.Hide()
      SET DATASESSION TO (lnDataSess)
      oOptionGrid = .NULL.
      LOogsCROLL =  .NULL.
    ENDIF 
    ENDIF   
  ENDDO 
  IF lnSel = 4  &&-- <Canel>
    SELECT(loFormSet.lcScndGrdCursor) 
    if BETWEEN(lnScndRec ,1,RECCOUNT())
      GO record lnScndRec 
    ENDIF 
    SELECT(loFormSet.lcFrstGrdCursor) 
    if BETWEEN(lnFrstRec ,1,RECCOUNT())
      GO record lnFrstRec 
    ENDIF 
    loFormSet.AriaForm1.grdUpp.Refresh
    loFormSet.AriaForm1.grdUpp.AfterRowColchange()
    RETURN .F.
  ENDIF
  RETURN .T.
ENDIF 

*!*************************************************************
*! Name      : lfSaveFiles
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : Save Files
*!*************************************************************  
FUNCTION lfSaveFiles
PARAMETERS loFormSet
lcFrstGrdCursor = loFormSet.lcFrstGrdCursor
lcScndGrdCursor = loFormSet.lcScndGrdCursor
lcWorkDir=oAriaApplication.Workdir
llGlLink = loFormSet.llGlLink 
IF llGlLink
  =gfOpenTable('GLDist','GLDistAc','SH')
  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO (lcWorkDir+lcTmpGlDt)
  USE (lcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF  
IF !USED('StyInvJl')
  =gfOpenTable("StyInvJl","StyInvJl","SH")
ENDIF
SELECT(loFormSet.lcFrstGrdCursor)
SET FILTER TO 
LOCATE   
IF loFormSet.lcfromscreen = 'B' 
  SELECT(loFormSet.lcFrstGrdCursor)
ELSE
  SELECT(loFormSet.lcScndGrdCursor) 
ENDIF   
LOCATE 
SCAN FOR TOTMOV <> 0 AND !DELETED()
  lcStylVal = Style
  lcWareCodeVal = CWARECODE
  =gfSeek(Style,'Style')
  =gfSeek(Style+CWARECODE,'StyDYe')
  llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
           (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))
  IF !llGoOn
    *-Style XDX: XXX/XXX is in use by another user, Unable to update.
    =gfModalGen('TRM42067B42001','DIALOG',ALLTRIM(STYLE))
    DELETE
    DO lpUnLock
    LOOP
  ENDIF           
  SELECT Style
  =gfSeek(lcStylVal ,'Style')
  =gfSeek(lcStylVal + lcWareCodeVal ,'STYDYE')
  lnOldStk   = TotStk
  lnOldCost  = IIF(loFormSet.lcCostMth<>'S',Ave_Cost,TotCost)  
  lcLinkCode = IIF(loFormSet.llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  
  WAIT WINDOW 'Updating => '+ALLTRIM(Style) NOWAIT
  lcLinkCode = IIF(loFormSet.llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")
  lcRISessn = gfSequence('GLSESSION')
  *--Start Updating.
  IF !lfInvUpdt()
    IF loFormSet.lcfromscreen = 'B' 
      SELECT(loFormSet.lcFrstGrdCursor)
    ELSE
      SELECT(loFormSet.lcScndGrdCursor) 
    ENDIF   
    LOOP
  ENDIF
ENDSCAN  
DO lpUnLock
IF llGlLink 
	SELECT (lcTmpGlDt)
  *-- Generate a unique session number.
  lcGlSess = gfSequence('GLSESSION')
  REPLACE ALL GLSESSION WITH lcGlSess
*C202163,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[Begin]
*  USE
  *SELECT GLDIST  
*  APPEND FROM (lcWorkDir+lcTmpGlDt)
*  ERASE (lcWorkDir+lcTmpGlDt+'.DBF')

 SELECT (lcTmpGlDt)
 SCAN 
  SCATTER MEMVAR MEMO
  m.Oid = gfTempName()
    SELECT GLDIST
     APPEND BLANK
  GATHER MEMVAR MEMO 
  =gfreplace('')
  ENDSCAN
  SELECT (lcTmpGlDt)
  USE
  ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
  SELECT GLDIST
  =gfTableUpdate()
*C202163,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]

ENDIF
WAIT CLEAR
SELECT STYLE 
=gfTableUpdate()
SELECT STYDYE
=gfTableUpdate()
SELECT STYINVJL
=gfTableUpdate()
SELECT InvtAdj
=gfTableUpdate()
IF llGlLink 
  SELECT GLDIST
  =gfTableUpdate()
ENDIF   
SELECT(loFormSet.lcScndGrdCursor) 
SET KEY TO 



*!*************************************************************
*! Name      : lfRecLock
*! Purpose   : Record Lock.
*!*************************************************************
*! Parameters: lcFile->Locked file.
*!*************************************************************
FUNCTION lfRecLock
PARAMETERS lcFile

SET REPROCESS TO 5 SECONDS 
DO WHILE .T.
  *-This record is in use by another user !','\!\<Retry;\<Cancel'
  lnChoice=gfModalGen('INM00029B00015','DIALOG')
  IF lnChoice = 1 
    IF !RLOCK(lcFile)
      LOOP
    ELSE
      lnRet = .T.
      EXIT
    ENDIF 
  ELSE
    lnRet = .F.
    EXIT   
  ENDIF
ENDDO
SET REPROCESS TO 0
RETURN (lnRet)
*!*************************************************************
*! Name      : lpUnLock
*! Purpose   : Unlock the saved locked records.
*!*************************************************************
PROCEDURE lpUnLock
UNLOCK ALL

*!*************************************************************
*! Name      : lfInvUpdt()
*! Purpose   : Update inventory.
*!*************************************************************
FUNCTION lfInvUpdt

*-SAB Fix issue 9 and 10 [Start]
SET STEP ON
IF !USED('STYLE_INFO')
  =gfOpenTable('STYLE', 'STYLE', 'SH', 'STYLE_INFO')
ENDIF
lcFrsCrsr = (loFormSet.lcFrstGrdCursor)

IF loFormSet.lcRpBusCase = 'MO'
  SELECT(lcFrsCrsr)
  lnRecno = RECNO()
  lnQty     = 0
  lnAvgCost = 0
  SCAN FOR &lcFrstGrdCursor..lSelect AND !DELETED() AND TOTMOV <> 0
    lnQty     = lnQty + ABS(&lcFrsCrsr..TotMov)
    lcStyle = STYLE
    =gfSeek(lcStyle, 'STYLE_INFO')
    lnAvgCost = lnAvgCost + ABS(&lcFrsCrsr..TotMov) * IIF(loFormSet.lcCostMth = 'S', STYLE_INFO.TotCost, STYLE_INFO.Ave_Cost)
  ENDSCAN
  lnACost = lnAvgCost / lnQty
  GOTO lnRecno
ELSE
  SELECT(lcFrsCrsr)
  lcStyle = STYLE
  =gfSeek(lcStyle, 'STYLE_INFO')
  lnACost = IIF(loFormSet.lcCostMth = 'S', STYLE_INFO.TotCost, STYLE_INFO.Ave_Cost)
ENDIF
*-SAB Fix issue 9 and 10 [End]

IF loFormSet.lcfromscreen = 'B' 
  SELECT(loFormSet.lcFrstGrdCursor)
ELSE
  SELECT(loFormSet.lcScndGrdCursor) 
ENDIF   
*--Gl adjustment account.

lcAdjAcct = ' '
IF llGlLink AND !EMPTY(cAdjRes)
  lcAdjReason = cAdjRes
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF
lnRet = 0
IF loFormSet.lcfromscreen = 'B' 
  SELECT(loFormSet.lcFrstGrdCursor)
ELSE
  SELECT(loFormSet.lcScndGrdCursor) 
ENDIF   
*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE Date TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*--Warehouse Code.
IF loFormSet.lcfromscreen = 'B'  
 lcAdjWareH = &lcFrstGrdCursor..cWareCode
ELSE
 lcAdjWareH = &lcScndGrdCursor..cWareCode 
ENDIF     

DECLARE laAdjust[9]
SCATTER FIELDS Mov1,Mov2,Mov3,Mov4,Mov5,Mov6,Mov7,Mov8,TotMov TO laAdjust
*-SAB Fix issue 9 and 10 [Start]
*lnACost = IIF(loFormSet.lcCostMth='S',STYLE.TotCost,STYLE.Ave_Cost)
*-SAB Fix issue 9 and 10 [End]

*--Type of the adjustment.
lcAdjTyp = '1'
lcAdjRef = ''
*--Call the global function for update style inventory control.
PRIVATE lcRefer
*-SAB Fix issue 9 and 10 [Start]
PRIVATE lcReference
*-SAB Fix issue 9 and 10 [End]
*lcRefer = gfCodDes(cAdjRes,'CADJREASON')
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*IF loFormSet.lcfromscreen = 'B' 
*  lcRefer = 'Style Assortment BreakDown'
*ELSE
*  lcRefer = 'Style Assortment Creation'
*ENDIF
IF loFormSet.lcRpBusCase = 'OO'  
  LOCAL lnAlias
  lnAlias = SELECT()
  SELECT(loFormSet.lcScndGrdCursor)
  LOCATE
  *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[Start]
  *lcRefer = 'Moved To : ' + &lcScndGrdCursor..Style
  lcRefer = 'Moved To : ' + &lcScndGrdCursor..Style +","+ALLTRIM(STR(ABS(TOTMOV)))
  *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[END]  
  *-SAB Fix issue 9 and 10 [Start]
  *lcReference = "Style Assortment Issue"
  lcReference = "Style Assortment BreakDown"
  *-SAB Fix issue 9 and 10 [End]
  SELECT (lnAlias)
ELSE  
  IF loFormSet.lcRpBusCase = 'MO'
    SELECT(loFormSet.lcFrstGrdCursor)
    lcRefer = 'Moved From : '
    SCAN FOR &lcFrstGrdCursor..lSelect AND !DELETED() AND TOTMOV <> 0      
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[Start]
      *lcRefer = lcRefer + IIF(lcRefer == 'Moved From : ', '', ', ') + &lcFrstGrdCursor..Style      
      lcRefer = lcRefer + IIF(lcRefer == 'Moved From : ', '', ', ') + &lcFrstGrdCursor..Style+","+ALLTRIM(STR(ABS(TOTMOV)))
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[END]
      *-SAB Fix issue 9 and 10 [Start]
      *lcReference = "Style Assortment Receive"
      lcReference = "Style Assortment Creation"
      *-SAB Fix issue 9 and 10 [End]
    ENDSCAN
  ELSE
    SELECT(loFormSet.lcScndGrdCursor)
    lcRefer = 'Moved To : '
    SCAN FOR oldStyle = &lcFrstGrdCursor..Style AND !DELETED() AND TOTMOV <> 0
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[Start]
      *lcRefer = lcRefer + IIF(lcRefer == 'Moved To : ', '', ', ') + &lcScndGrdCursor..Style      
      lcRefer = lcRefer + IIF(lcRefer == 'Moved To : ', '', ', ') + &lcScndGrdCursor..Style+","+ALLTRIM(STR(ABS(TOTMOV)))
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[END]
      *-SAB Fix issue 9 and 10 [Start]
      *lcReference = "Style Assortment Issue"
      lcReference = "Style Assortment BreakDown"
      *-SAB Fix issue 9 and 10 [End]
    ENDSCAN
  ENDIF  
ENDIF
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]

DIMENSION laOldStkDye[9]
STORE 0 TO laOldStkDye
SELECT STYDYE 
SCATTER FIELDS STK1,STk2,STk3,STk4,STk5,STk6,STk7,STk8,totSTk TO laOldStkDye
IF loFormSet.lcfromscreen = 'B' 
  SELECT(loFormSet.lcFrstGrdCursor)
ELSE
  SELECT(loFormSet.lcScndGrdCursor) 
ENDIF
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
*lnRet = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,'',Date,'',@laAdjust,lnACost,;
                 lcRefer,lcRISessn ,&lcFrstGrdCursor..cAdjRes,0,'','',@laGLDistAr,0,"",lcAdjRef)
*-SAB Fix issue 9 and 10 [Start]
*lnRet = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,'',Date,'',@laAdjust,lnACost,;
                 'Style Assortment Transfeer',lcRISessn ,&lcFrstGrdCursor..cAdjRes,0,'','',@laGLDistAr,0,"",lcAdjRef)
lnRet = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,'',Date,'',@laAdjust,lnACost,;
                 lcReference, lcRISessn ,&lcFrstGrdCursor..cAdjRes,0,'','',@laGLDistAr,0,"",lcAdjRef)
*-SAB Fix issue 9 and 10 [End]
SELECT StyInvJl
*C202163,1 MMT  08/20/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*REPLACE AssrtRef WITH lcRefer
=gfREPLACE("AssrtRef WITH '"+lcRefer+"'")
*C202163,1 MMT  08/20/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
SELECT InvtAdj
APPEND BLANK 

REPLACE Style WITH IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..Style,&lcScndGrdCursor..Style),;
        Dyelot WITH '',;
        creason WITH '',;
        date WITH IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..date,&lcScndGrdCursor..date),;
        dpostdate WITH IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..date,&lcScndGrdCursor..date),;
        Type WITH 'A',;
        Unt_cost With lnACost ,;
        old_cost With lnACost ,;
        Adj1 with IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV1,&lcScndGrdCursor..MOV1),;
        Adj2 With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV2,&lcScndGrdCursor..MOV2),;
        Adj3 With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV3,&lcScndGrdCursor..MOV3),;
        Adj4 With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV4,&lcScndGrdCursor..MOV4),;
        Adj5 With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV5,&lcScndGrdCursor..MOV5),;
        Adj6 With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV6,&lcScndGrdCursor..MOV6),;
        Adj7 With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV7,&lcScndGrdCursor..MOV7),;
        Adj8 With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..MOV8,&lcScndGrdCursor..MOV8),;
        TOTAdj With IIF(loFormSet.lcfromscreen = 'B',&lcFrstGrdCursor..TOTMOV,&lcScndGrdCursor..TOTMOV),;
        oldQty1 With laOldStkDye[1],;
        oldQty2 With laOldStkDye[2],;
        oldQty3 With laOldStkDye[3],;
        oldQty4 With laOldStkDye[4],;
        oldQty5 With laOldStkDye[5],;
        oldQty6 WITH laOldStkDye[6],;
        oldQty7 WITH laOldStkDye[7],;
        oldQty8 WITH laOldStkDye[8],;
        Totold  WITH laOldStkDye[9],;   
        cFromWare WITH &lcFrstGrdCursor..cwarecode ,;
        glFyear  WITH &lcFrstGrdCursor..GLFYear  ,; 
        glperiod WITH &lcFrstGrdCursor..glperiod ,;
        csession WITH  lcRISessn  
=gfAdd_Info('InvtAdj')                                                                                   
=gfReplace('')                    
IF lnRet <> 0   
  IF loFormSet.lcfromscreen = 'B'              
    SELECT(lcScndGrdCursor)                 
  ELSE
    SELECT(lcFrstGrdCursor)
  ENDIF   
  SCAN FOR IIF(loFormSet.lcfromscreen = 'B',oldStyle = &lcFrstGrdCursor..Style,&lcFrstGrdCursor..lSelect) AND !DELETED() AND TOTMOV <> 0
  
    =gfSeek(Style,'Style')
    =gfSeek(Style +EVALUATE(loFormSet.lcFrstGrdCursor+'.CWARECODE'),'StyDYe')
    llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
             (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))
    IF !llGoOn
      *-Style XDX: XXX/XXX is in use by another user, Unable to update.
      =gfModalGen('TRM42067B42001','DIALOG',ALLTRIM(STYLE))
      DELETE
      DO lpUnLock
      LOOP
    ENDIF           
    DIMENSION laOldStkDye[9]
    STORE 0 TO laOldStkDye
    SELECT STYDYE 
    SCATTER FIELDS STK1,STk2,STk3,STk4,STk5,STk6,STk7,STk8,totSTk TO laOldStkDye

    SELECT Style
    lnOldStk   = TotStk
    lnOldCost  = IIF(loFormSet.lcCostMth<>'S',Ave_Cost,TotCost)  
    lcLinkCode = IIF(loFormSet.llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")

    WAIT WINDOW 'Updating => '+ALLTRIM(Style) NOWAIT
    lcLinkCode = IIF(loFormSet.llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")
    DECLARE laAdjust[9]
    IF loFormSet.lcfromscreen = 'B'              
      SELECT(lcScndGrdCursor)                  
    ELSE
      SELECT(lcFrstGrdCursor)
    ENDIF   
      
    SCATTER FIELDS Mov1,Mov2,Mov3,Mov4,Mov5,Mov6,Mov7,Mov8,TotMov TO laAdjust
    *-SAB Fix issue 9 and 10 [Start]
    *lnACost = IIF(loFormSet.lcCostMth='S',STYLE.TotCost,STYLE.Ave_Cost)
    IF loFormSet.lcRpBusCase = 'MO'
      lnACost = IIF(loFormSet.lcCostMth='S', STYLE.TotCost, STYLE.Ave_Cost)
    ENDIF
    *-SAB Fix issue 9 and 10 [End]
    
    *--Type of the adjustment.
    lcAdjTyp = '1'

    *--Call the global function for update style inventory control.
    PRIVATE lcRefer
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    **lcRefer = gfCodDes(&lcFrstGrdCursor..cAdjRes,'CADJREASON')	
	*IF loFormSet.lcfromscreen = 'B' 
	*  lcRefer = 'Style Assortment BreakDown'
	*ELSE
	*  lcRefer = 'Style Assortment Creation'
	*ENDIF   
	IF loFormSet.lcRpBusCase = 'MO'
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[Start]
      *lcRefer = 'Moved To : ' + &lcScndGrdCursor..Style      
      lcRefer = 'Moved To : ' + &lcScndGrdCursor..Style+","+ALLTRIM(STR(ABS(&lcFrstGrdCursor..TOTMOV)))
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[END]
      *-SAB Fix issue 9 and 10 [Start]
      *lcReference = "Style Assortment Issue"
      lcReference = "Style Assortment Creation"
      *-SAB Fix issue 9 and 10 [End]
    ELSE
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[Start]
      *lcRefer = 'Moved From : ' + &lcFrstGrdCursor..Style      
      lcRefer = 'Moved From : ' + &lcFrstGrdCursor..Style+","+ALLTRIM(STR(ABS(&lcScndGrdCursor..TOTMOV)))
      *C201548,1 MMT 01/14/2013 Style assortment scr:Add Qty to Reference field in Styinvjl[END]
      *-SAB Fix issue 9 and 10 [Start]
      *lcReference = "Style Assortment Receive"
      lcReference = "Style Assortment BreakDown"
      *-SAB Fix issue 9 and 10 [End]
    ENDIF    
	*C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
	
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [Start]
    *lnRet1 = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,'',&lcFrstGrdCursor..Date,'',@laAdjust,lnACost,;
                 lcRefer,lcRISessn ,&lcFrstGrdCursor..cAdjRes,0,'','',@laGLDistAr,0,"",lcAdjRef)
    *-SAB Fix issue 9 and 10 [Start]
    *lnRet1 = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,'',&lcFrstGrdCursor..Date,'',@laAdjust,lnACost,;
                 'Style Assortment Transfeer',lcRISessn ,&lcFrstGrdCursor..cAdjRes,0,'','',@laGLDistAr,0,"",lcAdjRef)
    lnRet1 = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,'',&lcFrstGrdCursor..Date,'',@laAdjust,lnACost,;
                 lcReference, lcRISessn ,&lcFrstGrdCursor..cAdjRes,0,'','',@laGLDistAr,0,"",lcAdjRef)
    *-SAB Fix issue 9 and 10 [End]
    SELECT StyInvJl
    *C202163,1 MMT  08/20/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
    *REPLACE AssrtRef WITH lcRefer
    =gfREPLACE("AssrtRef WITH '"+lcRefer+"'")
    *C202163,1 MMT  08/20/2018 use the GLDIST table remotely not native, because of conversion to SQL[ENd]
    *C201505,1 SAB 08/02/2012 Custome Style Transfer for GMA [End]
    
    IF lnRet1 <> 0
      SELECT InvtAdj
      APPEND BLANK 
      REPLACE Style WITH IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..Style,&lcFrstGrdCursor..Style),;
              Dyelot WITH '',;
              creason WITH '',;
              date WITH &lcFrstGrdCursor..date,;
              dpostdate WITH &lcFrstGrdCursor..date,;
              Type WITH 'A',;
              Unt_cost With lnACost ,;
              old_cost With lnACost ,;
              Adj1     with IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV1,&lcFrstGrdCursor..MOV1),;
              Adj2     With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV2,&lcFrstGrdCursor..MOV2),;
              Adj3     With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV3,&lcFrstGrdCursor..MOV3),;
              Adj4     With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV4,&lcFrstGrdCursor..MOV4),;
              Adj5     With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV5,&lcFrstGrdCursor..MOV5),;
              Adj6     With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV6,&lcFrstGrdCursor..MOV6),;
              Adj7     With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV7,&lcFrstGrdCursor..MOV7),;
              Adj8     With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..MOV8,&lcFrstGrdCursor..MOV8),;
              TOTAdj   With IIF(loFormSet.lcfromscreen = 'B',&lcScndGrdCursor..TOTMOV,&lcFrstGrdCursor..TOTMOV),;
              oldQty1  With laOldStkDye[1],;
              oldQty2  With laOldStkDye[2],;
              oldQty3  With laOldStkDye[3],;
              oldQty4  With laOldStkDye[4],;
              oldQty5  With laOldStkDye[5],;
              oldQty6  WITH laOldStkDye[6],;
              oldQty7  WITH laOldStkDye[7],;
              oldQty8  WITH laOldStkDye[8],;
              Totold   WITH laOldStkDye[9],;   
              cFromWare WITH &lcFrstGrdCursor..cwarecode ,;
              glFyear  WITH &lcFrstGrdCursor..GLFYear  ,; 
              glperiod WITH &lcFrstGrdCursor..glperiod ,;
              csession WITH  lcRISessn     
      =gfAdd_Info('InvtAdj')                                                                                                                                                             
      =gfReplace('')                                       
    ENDIF   
  ENDSCAN 
ENDIF   
IF lnRet = 0
  RETURN .F.
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfShowSelected
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2009
*! Purpose   : Show Selected 
*!*************************************************************  
FUNCTION lfShowSelected
PARAMETERS loFormSet
SELECT(loFormSet.lcFrstGrdCursor)
IF loFormSet.llShowSelected
  SET FILTER TO lSelect
ELSE
  SET FILTER TO 
ENDIF 
LOCATE 