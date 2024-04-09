*:**********************************************************************************************
*: Program file  : MAVMREF.PRG 
*: Program desc. : Vendor Item\Color screen
*: System        : Aria Advantage Series.
*: Module        : MA
*: Developer     : Adel Mohhamed El Gazzar (ADEL)
*: Date          : 08/21/02
*: Refer to      : C(102683)
*:**********************************************************************************************
*: B122673,1 NNA 05/15/2004 Fix bug that the price field Rounding up i.e (0.065 save as 0.07)
*: B122673,1 NNA            that By changing the Price field name to be (nFabCost) by add new record 
*: B122673,1 NNA            in sydflfld.dbf with (Vendmatl & nFabCost) and Delete record that have value
*: B122673,1 NNA            (Vendmatl & Price) and Replace all (Price) with (nFabCost) in this code
*:**********************************************************************************************
*--Does the company use "Vendor Material Reference"?
llVenRef  = ALLTRIM(gfGetMemVar('M_VENREF'))= 'Y'
llMulCurr  = gfGetMemVar('llMulCurr')  
*--Initialize needed variables
STORE ' ' TO lcWinNo,lcWinNo0,lcWinNo1,lcWinNo2,lcWinNo3,lcWinNo5,lcModal,lcWinNo4
STORE ' ' TO lcFabric,lcOldFab,lcColor,lcOldClr,lcVenName,lcIDesc,lcClrDesc,lcDetTit
STORE ' ' TO lcTmpFile,lcOldValue,lcLastFab,lcLastCol,lcTmpHdr,lcVenCode,lcPrCur
STORE 0 TO lnOldPrice ,lnOldQty,lnOldLead
STORE .F. TO llNoshow,llBrowse,lNewLine
lnMarkr = 1
lcWinTit = "Material Reference"
lcDetTit = "Detail Lines"
laDefProc[7]  = .F.     && Cancel/Uncancel procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
lcScFields = ''
DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcDetTit)

IF !gfSetup()
  RETURN
ENDIF

IF !llVenRef
  =gfModalGen('INM36188B36000','DIALOG',gcAct_Comp)
  
  DEFINE WINDOW (gcBaseWind) FROM 0,0 TO 1,1
  ACTIVATE WINDOW (gcBaseWind)
  =gfEscap()
  RELEASE BAR 100 OF P01PU01   
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)
  *--Program screen windows [begin]
  lcWinNo     = gfTempName()
  lcWinNo0    = gfTempName()
  lcWinNo1    = gfTempName()
  lcWinNo2    = gfTempName()
  lcWinNo3    = gfTempName()
  lcWinNo4    = gfTempName()
  lcWinNo5    = gfTempName()
  lcTmpFile   = gfTempName()
  lcTmpHdr    = gfTempName()
  lcScFields = 'VENDOR,FABRIC,COLOR,CVENFAB'
  lcBrFields = "vendor    :H='Vendor',"+;
               "cvencomp  :H='Vendor Name'"
ENDIF

DO (gcScrDir+gcWinAppl+'\MAVMREF.SPX')

RELEASE BAR 100 OF P01PU01 
IF glQuitting
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
  ENDIF
  ERASE (gcWorkDir+lcTmpFile+".DBF")
  ERASE (gcWorkDir+lcTmpFile+".CDX")
  
  IF USED()
    USE IN (lcTmpHdr)
  ENDIF
  ERASE (gcWorkDir+lcTmpHdr+".DBF")
  ERASE (gcWorkDir+lcTmpHdr+".CDX")
  
ENDIF

*!**************************************************************************
*! Name      : lfvVendor
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/14/02
*! Purpose   : Validating the vendor code
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************

FUNCTION lfvVendor

PRIVATE lcbrfields
IF !llBrowse AND lcVenCode=lcOldValue
  RETURN
ENDIF
IF llBrowse OR !EMPTY(lcVenCode)
  SELECT APVENDOR
  SET ORDER TO TAG VENCODE
  IF llBrowse OR !SEEK(lcVenCode)
    lcBrFields = "CVENDCODE :H='Vendor',"+;
                 "CVENCOMP  :H='Name',"+;
                 "cPhoneNo  :H='Phone',"+;
                 "cVenOurAc :H='Our Account' "
    lcForExpr = " 'M' $ cVenSupTyp "
    IF gfBrows([FOR &lcForExpr],'cVendCode', 'laTempArr','Vendors')
      lcVenCode=APVENDOR.cVendCode
      lcVenName=APVENDOR.cvencomp
      lcPrCur = ApVendor.ccurrcode
    ELSE
      lcVenCode=SPACE(8)
    ENDIF
  ELSE
    lcVenName=APVENDOR.cvencomp  
    lcPrCur = ApVendor.ccurrcode
  ENDIF
  llBrowse = .F.
ENDIF
=lfRefresh('lcWinNo0')

*-- Get this vendor records if any
IF !EMPTY(lcVenCode)
  IF SEEK(lcVenCode,'VENDMATL')
    *--Get this vendor records if any
    IF USED(lcTmpFile) AND RECCOUNT(lcTmpFile) <>0 
      SELECT (lcTmpFile)
      ZAP
    ENDIF
    SELECT VENDMATL
    SCAN REST WHILE VENDOR = lcVenCode
      SCAT MEMVAR MEMO
      m.OldFbCl = m.Fabric + m.Color
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDSCAN
    =SEEK(lcVenCode,'VENDMATL')
    =lfBrowLine()
    laScrMode=.F.
    laScrMode[2]=.T.
  ELSE
    laScrMode=.F.
    laScrMode[4]=.T.
  ENDIF
  SELECT (lcTmpHdr)
  APPEND BLANK
  REPLACE VENDOR   WITH lcVenCode,;
          cvencomp WITH APVENDOR.cvencomp
  SHOW GETS  
ELSE
  _CUROBJ = OBJNUM(lcVenCode)  
ENDIF
RETURN

*!*************************************************************
*! Name        : lfwFabric
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old fabric field in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwFabric

lcOldFab = lcFabric
lcOldClr = lcColor

*!*********************************************************************************************
*! Name        : lfvFabric
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!*********************************************************************************************
*! Purpose     : validate the fabric field
*!*********************************************************************************************
*! Calls       : 
*!*********************************************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
*--If modifying a line, check if the original Fabric\Color has POs
IF !lNewLine AND (lcFabric <> lcOldFab OR lcColor <> lcOldClr) AND (!EMPTY(&lcTmpFile..OldFbCl) AND lfChkPos(&lcTmpFile..OldFbCl))
  =gfModalGen('INM36190B36000','DIALOG',ALLTRIM(lcVenCode)+'. Cannot modify')
  lcFabric = lcOldFab
  RETURN
ENDIF

lnAlias = SELECT()
SELECT Fabric
SET FILTER TO CFABGRADE = '1' 
IF !EMPTY(lcFabric) AND !SEEK(lcFabric)
  llBrowse = .F.
  DO FaBrow WITH lcFabric,'*'
ENDIF
IF EMPTY(lcFabric)
  lcFabric = lcOldFab
ENDIF
SET FILTER TO
SHOW GET lcFabric ENABLE
SELECT(lnAlias)

*!*********************************************************************************************
*! Name        : lfwColor
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!*********************************************************************************************
*! Purpose     : take the old color fieldin a variable
*!*********************************************************************************************
*! Calls       : 
*!*********************************************************************************************
FUNCTION lfwColor

lcOldFab = lcFabric
lcOldClr = lcColor


*!*********************************************************************************************
*! Name      : lfvColor
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/14/02
*!*********************************************************************************************
*! Purpose     : validate the color field
*!*********************************************************************************************
*! Calls       : 
*!*********************************************************************************************
FUNCTION lfvColor
PRIVATE lnAlias

IF MDOWN() OR (lcFabric = lcOldFab AND lcColor = lcOldClr AND !lNewLine)
  RETURN
ENDIF
*--If modifying a line, check if the original Fabric\Color has POs
IF !lNewLine AND (lcFabric <> lcOldFab OR lcColor <> lcOldClr) AND (!EMPTY(&lcTmpFile..OldFbCl) AND lfChkPos(&lcTmpFile..OldFbCl))
  =gfModalGen('INM36190B36000','DIALOG',ALLTRIM(lcVenCode)+'. Cannot modify')
  lcColor = lcOldClr
  RETURN
ENDIF
lnAlias = SELECT()
SELECT Fabric
SET ORDER TO TAG FABRIC
IF !EMPTY(lcColor) AND !SEEK(lcFabric+lcColor)
  lcColor  = IIF(EMPTY(lcFabric),PADR(lcColor,6),CHR(240))
  lcOldF   = lcFabric
  SELECT FABRIC
  SET FILTER TO CFABGRADE = '1' 
  DO FABROW WITH lcFabric,lcColor
  SET FILTER TO
  IF EMPTY(lcColor)
    lcFabric = lcOldF
    _CUROBJ = OBJNUM(lcColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
ENDIF

IF !EMPTY(lcFabric) AND !EMPTY(lcColor)
  IF FABRIC.CFABGRADE <> '1' 
    =gfModalGen('INM36192B36000','DIALOG',ALLTRIM(lcVenCode)+'. Cannot modify')
    _CUROBJ = OBJNUM(lcColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
  
  IF Fabric.Make .AND. gfModalGen('INM36009B36000','DIALOG')=1
    IF lNewLine
      lNewLine = .F.
    ENDIF     
    IF !EOF(lcTmpFile) 
      _CUROBJ = OBJNUM(lcFabric)
    ELSE
      lcFabric = SPACE(07)
      lcColor  = SPACE(06)
    ENDIF
    =lfwLine()
  	SELECT(lnAlias)
    RETURN
  ENDIF
  lnNewRec = RECNO(lcTmpFile)  
  IF lcColor <> lcOldClr
    IF SEEK(lcVenCode+lcFabric+lcColor,lcTmpFile)
      *--This Item/Color already exists for the same vendor ð.
      =gfModalGen('INM36186B36000','DIALOG',ALLTRIM(lcVenCode))
      lNewLine = .F.
      =lfwLine()
  	  SELECT(lnAlias)
      RETURN
    ENDIF
  ENDIF
  SELECT(lcTmpFile)
  IF lNewLine
    lNewLine = .F.
    APPEND BLANK
    SHOW GET lcVenFab  ENABLE
    SHOW GET lcVenCol  ENABLE
    SHOW GET lnPrice   DISABLE
    SHOW GET lnQty     DISABLE
    SHOW GET lnLeadTim DISABLE
    lnNewRec = RECNO()
  ELSE  
    GOTO lnNewRec 
  ENDIF

  lnPrice   = IIF(apvendor.ccurrcode = FABRIC.cPriceCur,FABRIC.nFabCost,0)
  lnLeadTim = Fabric.leadtime
  
  =RLOCK()
 
  *B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
  *REPLACE VENDOR WITH lcVenCode       , ;
          FABRIC      WITH lcFabric   ,;
          COLOR       WITH lcColor    ,;
          cvenfab     WITH lcVenFab   ,;
          cvencolr    WITH lcVenCol   ,;
          Price       WITH lnPrice    ,;
          nfabtotqty  WITH lnQty      ,;
          leadtime    WITH lnLeadTim  ,;  
          Sel         WITH IIF(Sel ='A',Sel ,IIF(EMPTY(Sel),'A','M'))
  REPLACE VENDOR WITH lcVenCode       , ;
          FABRIC      WITH lcFabric   ,;
          COLOR       WITH lcColor    ,;
          cvenfab     WITH lcVenFab   ,;
          cvencolr    WITH lcVenCol   ,;
          nFabCost    WITH lnPrice    ,;
          nfabtotqty  WITH lnQty      ,;
          leadtime    WITH lnLeadTim  ,;  
          Sel         WITH IIF(Sel ='A',Sel ,IIF(EMPTY(Sel),'A','M'))
  *B122673,1 NNA (End)
  UNLOCK
  =lfwLine()
  SHOW GET pbRemove ENABLE
  =SEEK(lcFabric+lcColor,'FABRIC')        
  lcIDesc   = FABRIC.DESC
  lcClrDesc = gfCodDes(lcColor, 'COLOR')
ELSE
  IF LASTKEY() <> 15 AND EMPTY(lcFabric) AND EMPTY(lcColor)
    RETURN
  ENDIF
ENDIF
IF EMPTY(lcColor)
  STORE ' ' TO  lcClrDesc,lcVenFab,lcVenCol
  STORE 0  TO   lnPrice,lnQty,lnLeadTim 
  IF laScrMode[2] OR laScrMode[1] OR EOF(lcTmpFile)
    SHOW GET lcVenFab  DISABLE
    SHOW GET lcVenCol  DISABLE
    SHOW GET lnPrice   DISABLE
    SHOW GET lnQty     DISABLE
    SHOW GET lnLeadTim DISABLE
  ELSE
    SHOW GET lcVenFab  ENABLE
    SHOW GET lcVenCol  ENABLE
    SHOW GET lnPrice   ENABLE
    SHOW GET lnQty     ENABLE
    SHOW GET lnLeadTim ENABLE
  ENDIF    
  =lfRefresh()
ENDIF

SHOW GET lcFabric ENABLE
SHOW GET lcColor  ENABLE
IF !EMPTY(lcFabric) AND !EMPTY(lcColor)
  lNewLine = .F.
ENDIF  
=lfRefresh()
IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME
SELECT(lnAlias)


*!*************************************************************
*! Name      : lpShow
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to control the show of all objects on the screen with all modes
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
    IF !USED(lcTmpHdr)
      SELECT VENDMATH
      =AFIELDS(laFileStru)
      DECLARE laIndex[1,2]
      laIndex[1,1] = 'Vendor'
      laIndex[1,2] = 'VENDMATH'
      =gfCrtTmp(lcTmpHdr,@laFileStru,@laIndex)
      IF USED(lcTmpHdr)
         USE IN (lcTmpHdr)
         USE (gcWorkDir+lcTmpHdr) EXCL IN 0 ORDER 1
      ENDIF
    ELSE  
      IF RECCOUNT(lcTmpHdr) <>0 
        SELECT (lcTmpHdr)
        ZAP
      ENDIF
    ENDIF
    IF !USED(lcTmpFile)
      *--Create the temp file
      SELECT VENDMATL
      =AFIELDS(laFileStru)
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+2,4]
      laFileStru[lnFileStru+1,1] = 'Sel'
      laFileStru[lnFileStru+1,2] = 'C'
      laFileStru[lnFileStru+1,3] = 1
      laFileStru[lnFileStru+1,4] = 0

      laFileStru[lnFileStru+2,1] = 'OldFbCl'
      laFileStru[lnFileStru+2,2] = 'C'
      laFileStru[lnFileStru+2,3] = 13
      laFileStru[lnFileStru+2,4] = 0
      
      DECLARE laIndex[2,2]
      laIndex[1,1] = 'Vendor+fabric+color+cvenfab+cvencolr'
      laIndex[1,2] = 'MatCol'
      laIndex[2,1] = 'Vendor+cvenfab+cvencolr+fabric+color'
      laIndex[2,2] = 'VenMat'
      =gfCrtTmp(lcTmpFile,@laFileStru,@laIndex)
      IF USED(lcTmpFile)
         USE IN (lcTmpFile)
         USE (gcWorkDir+lcTmpFile) EXCL IN 0 ORDER 1
      ENDIF
    ELSE
      IF RECCOUNT(lcTmpFile) <>0 
        SELECT (lcTmpFile)
       ZAP
      ENDIF
    ENDIF
    lcVenCode  = ' '
    lcOldValue = ' '
    STORE ' ' TO  lcFabric,lcOldFab,lcColor,lcOldClr,lcVenName,lcIDesc,lcClrDesc
    STORE ' ' TO  lcVenFab,lcVenCol,lcIDesc,lcClrDesc,lcLastFab,lcLastCol
    STORE 0  TO lnOldPrice ,lnOldQty,lnOldLead,lnPrice,lnQty,lnLeadTim 
    =lfBrowLine()
    =lfwLine()
    SHOW GETS WINDOW (lcWinNo0) DISABLE ONLY
    SHOW GETS WINDOW (lcWinNo2) DISABLE ONLY    
    SHOW GETS WINDOW (lcWinNo3) DISABLE ONLY
    SHOW GET ibCheck_1 ENABLE
    SHOW GET lcVenCode ENABLE    
    SHOW GET lcVenFab  DISABLE
    SHOW GET lcVenCol  DISABLE
    SHOW GET lnPrice   DISABLE
    SHOW GET lnQty     DISABLE
    SHOW GET lnLeadTim DISABLE
    _CUROBJ = OBJNUM(lcVenCode)
    =lfRefresh('lcWinNo2')
  CASE laScrMode[2]
    lcVenCode = IIF(!EMPTY(laData[1]),laData[1],lcVenCode)
    IF USED(lcTmpFile) AND RECCOUNT(lcTmpFile) <>0 
      SELECT (lcTmpFile)
      ZAP
    ENDIF
    IF !EMPTY(lcVenCode) AND SEEK(lcVenCode,'VENDMATL')
      SELECT VENDMATL
      SCAN REST WHILE vendor+fabric+color+cvenfab+cvencolr = lcVenCode
        SCAT MEMVAR MEMO
        m.OldFbCl = m.Fabric + m.Color
        INSERT INTO (lcTmpFile) FROM MEMVAR
      ENDSCAN
      *--Always get the last record for the current vendor for the navigation button to work properly.
      SELECT (lcTmpHdr)
      APPEND BLANK
      REPLACE VENDOR   WITH lcVenCode,;
              cvencomp WITH APVENDOR.cvencomp
  
      
      SELECT (lcTmpFile)
      =SEEK(lcVenCode)
      *---
      =SEEK(lcVenCode,'ApVendor')
      
      lcFabric  = FABRIC
      lcColor   = Color
      lcVenName = APVENDOR.cvencomp

      *--
      lcPrCur = ApVendor.ccurrcode
      
      =lfBrowLine()
      =lfwLine()
      SHOW GETS WINDOW (lcWinNo2) DISABLE ONLY
      =lfRefresh()
    ENDIF
  CASE laScrMode[3]
    llCUpdate  = .T.
    =lfBrowLine()
    SHOW GETS WINDOW (lcWinNo0) DISABLE ONLY
    SHOW GETS WINDOW (lcWinNo2) ENABLE ONLY
    SHOW GETS WINDOW (lcWinNo3) ENABLE ONLY
    GOTO TOP IN (lcTmpFile) 
    IF !EOF(lcTmpFile)
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF     
  CASE laScrMode[4]
    llCUpdate  = .T.
    SHOW GETS WINDOW (lcWinNo0) DISABLE ONLY
    SHOW GETS WINDOW (lcWinNo2) DISABLE ONLY    
    SHOW GETS WINDOW (lcWinNo3) ENABLE ONLY
    GOTO TOP IN (lcTmpFile) 
    IF !EOF(lcTmpFile)
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF     
ENDCASE

*!*************************************************************
*! Name        : lfBrowLine
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/14/02
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : browse lines for the selected P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfBrowLine

lnAlias = SELECT()
SELECT (lcTmpFile)
GOTO TOP
lnMarkr = RECNO()

*B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
*lcBrowFlds = [cMarker  =IIF(RECNO()=lnMarkr ,'>',' ') :H="":R:1:W=.F.,]+;
             [Fabric  :R:H='Item' :7,]+;
             [Color   :R:H='Color':8,]+;
             [cvenfab :R:10:H='Vendor Item' :12,]+;
             [cvencolr:R:H='Vendor Color' :12,]+;
             [price   :R:13:H='Price':P='999999999.999' :13,]+;
             [nfabtotqty :R:13:H='Minimum Quantity':P='9999999.999' :16,]+;
             [leadtime :3:R:H='Lead Time':P='999' :9]
lcBrowFlds = [cMarker  =IIF(RECNO()=lnMarkr ,'>',' ') :H="":R:1:W=.F.,]+;
             [Fabric   :R:H='Item' :7,]+;
             [Color    :R:H='Color':8,]+;
             [cvenfab  :R:10:H='Vendor Item' :12,]+;
             [cvencolr :R:H='Vendor Color' :12,]+;
             [nFabCost :R:13:H='Price':P='999999999.999' :13,]+;
             [nfabtotqty :R:13:H='Minimum Quantity':P='9999999.999' :16,]+;
             [leadtime :3:R:H='Lead Time':P='999' :9]
*B122673,1 NNA (End)

BROWSE FIELDS &lcBrowFlds;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwLine() ;       
       TITLE lcDetTit    ;
       WINDOW (lcWinNo1) IN WINDOW (gcBaseWind)
=lfRefresh('lcWinNo1')
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwLine
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/14/02
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : refresh the fields when changing the record in the browse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwLine

glFromBrow = .T.
lnAlias = SELECT()
SELECT (lcTmpFile)
lnMarkr = RECNO(lcTmpFile)
IF !EOF() AND !BOF()
  lcFabric  = FABRIC
  lcColor   = COLOR
  lcVenFab  = cvenfab
  lcVenCol  = cvencolr
 
  *B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
  *lnPrice   = Price 
  lnPrice   = nFabCost
  *B122673,1 NNA (End)
  
  lnQty     = nfabtotqty
  lnLeadTim = leadtime
ENDIF
=SEEK(lcFabric+lcColor,'FABRIC')        
lcIDesc   = FABRIC.DESC
lcClrDesc = gfCodDes(lcColor, 'COLOR')

IF laScrMode[2] OR laScrMode[1] OR EOF(lcTmpFile)
  SHOW GET lcFabric  DISABLE
  SHOW GET lcColor   DISABLE
  SHOW GET ibFabric  DISABLE
  SHOW GET ibColor   DISABLE
  SHOW GET lcVenFab  DISABLE
  SHOW GET lcVenCol  DISABLE
  SHOW GET lnPrice   DISABLE
  SHOW GET lnQty     DISABLE
  SHOW GET lnLeadTim DISABLE
ELSE
  SHOW GET lcFabric  ENABLE
  SHOW GET lcColor   ENABLE
  SHOW GET ibFabric  ENABLE
  SHOW GET ibColor   ENABLE
  SHOW GET lcVenFab  ENABLE
  SHOW GET lcVenCol  ENABLE
  SHOW GET lnPrice   ENABLE
  SHOW GET lnQty     ENABLE
  SHOW GET lnLeadTim ENABLE
ENDIF    
=lfRefresh()
IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME


SELECT(lnAlias)


FUNCTION lfWhen
lcOldValue = EVAL(VARREAD())


*!**********************************************************************************************
*! Name        : lfvKFabric
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : validate the New button
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvNew

lcFabric  = SPACE(07)
lcColor   = SPACE(06)
lcIDesc   = SPACE(20) 
lcClrDesc = SPACE(15) 
lcVenFab  = SPACE(12) 
lcVenCol  = SPACE(12) 
lnPrice   = 0
lnQty     = 0
lnLeadTim = 0
lNewLine = .T.
SHOW GETS WINDOW (lcWinNo2) DISABLE ONLY
SHOW GET lcFabric  ENABLE
SHOW GET lcColor   ENABLE
SHOW GET ibFabric  ENABLE
SHOW GET ibColor   ENABLE
=lfRefresh('lcWinNo2')
=lfRefresh()
_CUROBJ = OBJNUM(lcFabric)

*!**********************************************************************************************
*! Name        : lfvKFabric
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : validate the fabric field from the key field
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvKFabric

lcFabric = PADR('?',7)
=lfvFabric()
_CUROBJ = OBJNUM(lcColor)

*!**********************************************************************************************
*! Name        : lfvKFabric
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : validate the fabric field from the key field
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvKColor

lcColor = PADR('?',7)
=lfvColor()

*!**********************************************************************************************
*! Name        : lfwColor
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : take the old color fieldin a variable
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfwColor

lcOldClr = lcColor

*!**********************************************************************************************
*! Name        : lfvVenFab
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validte Vendor Item
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvVenFab

SELECT(lcTmpFile)
PRIVATE lnRecNo
lnRecNo = RECNO()
IF !EMPTY(lcVenFab)
  lcOldOrd = ORDER()
  SET ORDER TO VenMat
  IF SEEK(lcVenCode+lcVenFab) AND RECNO() <> lnRecNo AND FABRIC <> lcFabric
    =gfModalGen('INM36189B36000','DIALOG'," : "+ALLTRIM(Fabric))
    lNewLine = .F.
    SET ORDER TO (lcOldOrd)
    GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
    lcOVenFab = lcVenFab
    =lfwLine()
    lcVenFab = lcOVenFab
    _CUROBJ = OBJNUM(lcVenFab)        
    IF !WEXIST(lcDetTit)
     =lfBrowLine()
    ENDIF  
    SHOW WINDOW (lcDetTit) REFRESH SAME

    RETURN
  ELSE
    SET ORDER TO (lcOldOrd)
    GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
  ENDIF
ENDIF  
REPLACE cvenfab WITH lcVenFab
IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME




*!**********************************************************************************************
*! Name        : lfwColor
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : take the old color fieldin a variable
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvVenCol

SELECT(lcTmpFile)
PRIVATE lcKey
lcKey = EVAL(KEY())
PRIVATE lnRecNo
lnRecNo = RECNO()
IF !EMPTY(lcVenFab) AND !EMPTY(lcVenCol)
  lcOldOrd = ORDER()
  SET ORDER TO VenMat
  IF SEEK(lcVenCode+lcVenFab+lcVenCol) AND RECNO() <> lnRecNo
    =gfModalGen('INM36189B36000','DIALOG'," : "+ALLTRIM(Fabric))
    lNewLine = .F.
    SET ORDER TO (lcOldOrd)
    GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
    =lfwLine()    
    IF !WEXIST(lcDetTit)
      =lfBrowLine()
    ENDIF  
    SHOW WINDOW (lcDetTit) REFRESH SAME

    _CUROBJ = OBJNUM(lcVenCol)        
    RETURN
  ELSE
    SET ORDER TO (lcOldOrd)
    *=SEEK(lcKey)
    GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
  ENDIF
ENDIF  

REPLACE cvencolr WITH lcVenCol
IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME


*!**********************************************************************************************
*! Name        : lfvPrice
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validte Price
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvPrice

IF lnPrice < 0 
  =gfModalGen('INM36014B36000','DIALOG','Price')
  lnPrice  = lnOldPrice
  _CUROBJ = OBJNUM(lnPrice)
  RETURN
ENDIF  

SELECT(lcTmpFile)

*B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
*REPLACE Price WITH lnPrice
REPLACE nFabCost WITH lnPrice
*B122673,1 NNA (End)

IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME



*!**********************************************************************************************
*! Name        : lfwPrice
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Save the Price Old Value
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfwPrice

*---
*lnOldPrice = Price

*B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
*lnOldPrice = &lcTmpFile..Price
lnOldPrice = &lcTmpFile..nFabCost
*B122673,1 NNA (End)

*!**********************************************************************************************
*! Name        : lfwQty
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Save the Qty Old Value
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************

FUNCTION lfwQty

lnOldQty = lnQty

*!**********************************************************************************************
*! Name        : lfvQty
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validate the Qty 
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvQty

IF lnQty <= 0 
  =gfModalGen('INM36014B36000','DIALOG','Quantity')
  lnQty   = lnOldQty
  _CUROBJ = OBJNUM(lnQty)
  RETURN
ENDIF

SELECT(lcTmpFile)
REPLACE nFabTotQty WITH lnQty
IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME



*!**********************************************************************************************
*! Name        : lfwLead
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Save the Lead time old value
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfwLead

lnOldLead = lnLeadTim

*!**********************************************************************************************
*! Name        : lfvQty
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validate the Lead time
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvLead

SELECT(lcTmpFile)
REPLACE leadtime WITH lnLeadTim 
IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME


IF laScrMode[4]
  IF SEEK(lcFabric+lcColor,'Fabric')
    SKIP IN FABRIC
    lcLastFab = Fabric.Fabric
    lcLastCol = Fabric.Color
  ELSE
    STORE ' ' TO lcLastFab,lcLastCol
  ENDIF    
  IF !WEXIST(lcDetTit)
    =lfBrowLine()
  ENDIF  
  SHOW WINDOW (lcDetTit) REFRESH SAME

  =lfvNew()
  lcFabric  = lcLastFab
  lcColor   = lcLastCol

  SHOW GET lcFabric ENABLE
  SHOW GET lcColor  ENABLE
  _CUROBJ = OBJNUM(lcFabric)
ENDIF  

*!**********************************************************************************************
*! Name        : lfvRemLine
*! Developer   : Adel Mohammed El Gazzar (AADEL)
*! Date        : 08/19/02
*!**********************************************************************************************
*! Purpose     : Validate the Remove button
*!**********************************************************************************************
*! Calls       : 
*!**********************************************************************************************
FUNCTION lfvRemLine

lnAlias = SELECT()
SELECT(lcTmpFile)

IF (!EMPTY(&lcTmpFile..OldFbCl) AND lfChkPos(&lcTmpFile..OldFbCl))
  =gfModalGen('INM36190B36000','DIALOG',ALLTRIM(lcVenCode)+'. Cannot remove')
  RETURN
ENDIF


*--Check if there's any
IF gfModalGen('INM36017B36001','DIALOG') = 1
  DELETE
ELSE
  RETURN  
ENDIF
IF !WEXIST(lcDetTit)
  =lfBrowLine()
ENDIF  
SHOW WINDOW (lcDetTit) REFRESH SAME

GOTO TOP
IF EOF(lcTmpFile)
  SHOW GET pbRemove DISABLE
  lcFabric  = SPACE(07)
  lcColor   = SPACE(06)
  lcIDesc   = SPACE(20) 
  lcClrDesc = SPACE(15) 
  lcVenFab  = SPACE(12) 
  lcVenCol  = SPACE(12) 
  lnPrice   = 0
  lnQty     = 0
  lnLeadTim = 0
ENDIF
=lfwLine()
SELECT(lnAlias)


FUNCTION lfClearTrap

IF glFromBrow
  =gfStopBrow()
ENDIF
IF WONTOP() <> lcDetTit
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF


FUNCTION lfTrapKeys

IF WONTOP() = lcDetTit
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpShiftTab
ENDIF  

PROCEDURE lpTab

ON KEY LABEL TAB
IF WONTOP() = lcDetTit
  IF (laScrMode[1] OR laScrMode[2])
    ACTIVATE WINDOW (lcWinNo3)
    IF (laScrMode[1] OR laScrMode[2])
       _CUROBJ = OBJNUM(ibFromRec)
    ELSE
       _CUROBJ = OBJNUM(pbNew)
    ENDIF
  ELSE
    *IF lNewLine
    IF !EOF(lcTmpFile) OR lNewLine
      ACTIVATE WINDOW (lcWinNo2)    
      _CUROBJ = OBJNUM(lcFabric)   
    ELSE
      ACTIVATE WINDOW (lcWinNo3)
      _CUROBJ = OBJNUM(pbNew)
    ENDIF  
  ENDIF
ENDIF

PROCEDURE lpShiftTab

ON KEY LABEL BACKTAB
DO CASE
  CASE WONTOP() = lcDetTit
    ACTIVATE WINDOW (lcWinNo0)
    _CUROBJ = OBJNUM(lcVenCode)
ENDCASE


*!*************************************************************
*! Name      : lpDelScr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/14/02
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr


SELECT (lcTmpFile)
*--Check POs
SCAN
  IF !EMPTY(OldFbCl) AND lfChkPos(OldFbCl)
    =gfModalGen('INM36191B36000','DIALOG',ALLTRIM(lcVenCode))
    RETURN
  ENDIF  
ENDSCAN
LOCATE
SCAN
  IF SEEK(vendor+fabric+color+cvenfab+cvencolr,'VENDMATL')  
    SELECT VENDMATL
    DELETE
  ENDIF
  SELECT (lcTmpFile)
ENDSCAN
SELECT (lcTmpHdr)
IF SEEK(Vendor,'VENDMATH')
  SELECT VENDMATH
  DELETE
ENDIF

laScrMode=.F.
laScrMode[1]=.T.


*!*************************************************************
*! Name      : lpSavScr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/14/02
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpSavScr

SELECT (lcTmpFile)
LOCATE
IF EOF()
  *--You cannot save this record with no details.
  =gfModalGen('INM36015B36000','DIALOG',lcVenCode)
  llcSave = .F.
  RETURN
ENDIF
*--Check if any Item\Color doesn't have correspondent Vendor Item\Color
SELECT (lcTmpFile)
SET ORDER TO VenMat
=SEEK(lcVenCode)
LOCATE REST WHILE vendor+cvenfab+cvencolr+fabric+color = lcVenCode FOR EMPTY(cvenfab) OR EMPTY(cvenColr)
IF FOUND()
  *--One or more  Items/Colors don't have any Vendor Items\Colors assigned to them. Cannot proceed.
  =gfModalGen('INM36187B36000','DIALOG',lcVenCode)
  llcSave = .F.
  SET ORDER TO MatCol
  RETURN
ENDIF
SET ORDER TO MatCol
lcSetDel = SET('DELETE')
SET DELE OFF
SCAN
  lckey  = vendor + IIF(!EMPTY(OldFbCl),OldFbCl,Fabric+Color)
  DO CASE
    *--If this record was deleted
    CASE DELETED()
      *--Note that if this record was deleted and wasn't previously added to the master
      *--file that means it's a new one. So no action will be taken.
      IF SEEK(vendor+fabric+color+cvenfab+cvencolr,'VENDMATL')
        SELECT VENDMATL
        *-If it's not deleted, Delete it.
        IF !DELETED()
          DELETE
        ELSE
          *--Sreach any non-deleted record as it might be added and deleted and readded before.
          LOCATE REST WHILE vendor+fabric+color+cvenfab+cvencolr = lckey FOR !DELETED()
          IF FOUND()
            DELETE
          ENDIF
        ENDIF
      ENDIF
    CASE !DELETED()
      SCATTER MEMVAR MEMO
      SELECT VENDMATL
      *--It it already exists, reflict the changes only.
      IF !SEEK(lckey)
        APPEND BLANK
      ELSE
        IF DELETED()
          LOCATE REST WHILE vendor+fabric+color+cvenfab+cvencolr = lckey FOR !DELETED()
          IF !FOUND()
            =SEEK(lckey)
            RECALL
          ENDIF
        ENDIF   
      ENDIF
      GATHER MEMVAR MEMO
  ENDCASE
  SELECT (lcTmpFile)
ENDSCAN

SELECT (lcTmpHdr)
SCATTER MEMVAR MEMO
IF !SEEK(Vendor,'VENDMATH')
  SELECT VENDMATH
  APPEND BLANK
  REPLACE VENDOR   WITH m.Vendor,;
          cvencomp WITH m.cvencomp 
ELSE
  SELECT VENDMATH
  IF DELETED()
    LOCATE REST WHILE VENDOR = m.Vendor FOR !DELETED()
    IF !FOUND()
      =SEEK(M.Vendor)
      RECALL
      GATHER MEMVAR MEMO
    ENDIF    
  ENDIF          
ENDIF
SET DELETE &lcSetDel


FUNCTION lfChkPos
PARAMETERS lcKey
PRIVATE lnAlias
lnAlias = SELECT()

llPos = .F.
IF !USED('POFLN')
  =gfOpenFile(gcDataDir+'POFLN','Poflnf','SH')
ENDIF
IF SEEK(lcKey+'P','POFLN')
  SELECT POFLN
  LOCATE REST WHILE fabric+color+cmattype+pomat+trancd = lcKey FOR Vendor  = lcVenCode
  llPos = FOUND()
ENDIF
SELECT (lnAlias)
RETURN llPos
