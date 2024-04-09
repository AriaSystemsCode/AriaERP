*:***************************************************************************
*: Program file  : MASCREP.PRG
*: Program desc. : FABRIC COST SHEET REPORT.
*: System        : 
*: Module        : MATERIAL (MA)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*:***************************************************************************
*: E301237,1 KHM 05/23/99
*:***************************************************************************
*: Modifications:
*: B603320,1 ABD Fix bug  wrong display cost.
*:***************************************************************************


*-- Getting the cost labels from the memory variables.
DIMENSION laSetup[5,2]
laSetUp[1,1] = 'M_CTCOST1'
laSetUp[2,1] = 'M_CTCOST2'
laSetUp[3,1] = 'M_CTCOST3'
laSetUp[4,1] = 'M_CTCOST4'
laSetUp[5,1] = 'M_WAREHOUSE'
=gfGetMemVar(@laSetUp,gcAct_Comp)
lcLngLbl1 = laSetUp[1,2]
lcLngLbl2 = laSetUp[2,2]
lcLngLbl3 = laSetUp[3,2]
lcLngLbl4 = laSetUp[4,2]

*-- Initializing the necessary variables.
STORE '' TO lcFabclrs, lcFabclrs2, lcItmclrs, lcItmclrs2
lcType     = ' '
lcBOMTemp  = gfTempName()    && Variable to hold bom temp file.
lcFabTemp  = gfTempName()    && Variable to hold fabric temp file.
lcAllClr   = laOgFxFlt[5,6]  && Variable to hold color scope.
lcFilter   = '.T.'

*-- Building the filter in order to be able to optimize the speed.
=lfBuilFltr("FABRIC.ITEM_TYPE")
=lfBuilFltr("FABRIC.VENDOR")
=lfBuilFltr("FABRIC.PATTERN")

lcRForm    = IIF(lcRpFormat = 'C','A','B')
lcRpForm   = IIF(lcRForm = 'A',"MACSREPC","MACSREPF")
= lfRepPltFr(lcRpForm)


*-- Function to build the report's work file.
IF !lfBuilForm(lcRForm)
  RETURN
ENDIF

*-- Call function to display report.
DO gfDispRe WITH EVAL('lcRpForm')

*-- Erase temp files from disk if exist.
IF USED(lcBOMTemp)
  USE IN (lcBOMTemp)
  ERASE(gcWorkDir+lcBOMTemp+'.DBF')
  ERASE(gcWorkdir+lcBOMTemp+'.CDX')
ENDIF

IF USED(lcFabTemp)
  USE IN (lcFabTemp)
  ERASE(gcWorkDir+lcFabTemp+'.DBF')
  ERASE(gcWorkDir+lcFabTemp+'.CDX')
ENDIF

RETURN

*!*************************************************************
*! Name      : lfBuilForm
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : Building the temporary file according to the form.
*!*************************************************************
*! Passed Parameters  :  lcForm
*!*************************************************************
*! Example            :  =lfBuilForm(lcRForm)
*!*************************************************************
FUNCTION lfBuilForm
PARAMETER lcForm

SELECT Fabric
COPY STRU TO &gcWorkDir.&lcFabTemp
=gfOpenFile(gcWorkDir+lcFabTemp,'','SH')
INDEX ON Fabric+Color TAG &lcFabTemp

SELECT Fabric
GOTO TOP
IF !EMPTY(lcRpLowFab) OR !EMPTY(lcRpHigFab)
  IF !EMPTY(lcRpLowFab)
    SEEK lcRpLowFab
  ENDIF
  lcScanKey = "REST WHILE Fabric+Color <= lcRpHigFab "+;
              " FOR "+lcFilter+" AND SEEK(Fabric,'Bom') AND Bom.lMaterial"  
ELSE    
 lcScanKey = "FOR "+lcFilter+" AND SEEK(Fabric,'Bom') AND Bom.lMaterial"
ENDIF

SCAN &lcScanKey
  WAIT WINDOW "Selecting records for Fabric/Color: "+ALLTRIM(Fabric)+"/"+Color NOWAIT
  IF !EMPTY(lcAllClr) AND !(Color $ lcAllClr)
    LOOP
  ENDIF
  SCATTER MEMVAR MEMO
  INSERT INTO(lcFabTemp) FROM MEMVAR
ENDSCAN
WAIT CLEAR

SELECT (lcFabTemp)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN(.F.)
ENDIF

*-- Build the work file.
SELECT BOM

COPY STRU TO &gcWorkDir.&lcBOMTemp
=gfOpenFile(gcWorkDir+lcBOMTemp,'','SH')

*-- Indexing the temporary BOM file according to the selected form.
IF lcForm = 'A'
  INDEX ON SUBSTR(cItmMajor,1,7)+cItmMask+Typ+IIF(cCatgTyp <> "M",Item,MFGCode)+IClr+STR(RECNO(),7) TAG &lcBOMTemp
ELSE
  INDEX ON SUBSTR(cItmMajor,1,7)+Typ+IIF(cCatgTyp <> "M",Item,MFGCode)+cItmMask+IClr+STR(RECNO(),7) TAG &lcBOMTemp
ENDIF  
SET ORDER TO TAG &lcBOMTemp

*-- Getting the cost sheet information of the selected fabrics.
SELECT (lcFabTemp)
SCAN
  WAIT WINDOW 'Compiling the Cost Sheet information for fabric ' + Fabric NOWAIT
  lcFabric = Fabric
  lcFabClr = Color
  lnFabRec = RECNO()

  SELECT BOM
  SEEK lcFabric
  SCAN REST WHILE SUBSTR(cItmMajor,1,7) = lcFabric
    lnUntCost = 0
    IF cCatgTyp = "M"
      SCATTER MEMVAR
      SELECT &lcBOMTemp
      APPEND BLANK
      GATHER MEMVAR
      REPLACE cItmMask WITH lcFabClr
    ELSE
      IF (cItmMask = '******') .OR. (cItmMask = lcFabClr)
        IF cCatgTyp = "F"
          lcKey = SUBSTR(Item,1,7)+IIF(IClr='******', lcFabClr, IClr)
          SELECT Fabric
          IF SEEK(lcKey)
            lnUntCost = CostUse
            lcFabDesc = Desc
          ELSE
            lnUntCost = 0
            lcFabDesc = SPACE(0)
          ENDIF
          SELECT BOM
          SCATTER MEMVAR
          SELECT &lcBOMTemp
          APPEND BLANK
          GATHER MEMVAR
          REPLACE cItmMask WITH lcFabClr,;
                  Desc     WITH IIF(IClr = '******', lcFabDesc, Desc),;
                  IClr     WITH IIF(IClr <> '******', IClr, lcFabClr),;
                  UntCost  WITH lnUntCost
          REPLACE TotCost WITH ROUND(UntCost*nBomTotQty,2)
          SELECT BOM
        ENDIF
        IF cCatgTyp = "T"
          *B603320,1 ABD Get the costuse from fabric file if trim invantory. [Begin]
          *--lltrim_inv---> hold .T. if the trim  is an  trim inventory.
          lltrim_inv = .F.
          IF Trim_Invt
            lltrim_inv = .T.
            lcKey      = SUBSTR(Item,1,7)+IIF(IClr='******', lcFabClr, IClr)
            SELECT Fabric
            IF SEEK(lcKey)
              lnUntCost = CostUse
              lcFabDesc = Desc
            ELSE
              lnUntCost  = 0
              lcFabDesc  = SPACE(0)
            ENDIF
          ENDIF
          *B603320,1 ABD [End]

          SELECT BOM
          SCATTER MEMVAR
          SELECT &lcBOMTemp
          APPEND BLANK
          GATHER MEMVAR
          REPLACE cItmMask WITH lcFabClr,;
                  IClr     WITH IIF(IClr <> '******', IClr, lcFabClr)

          *B603320,1 ABD Get the costuse from fabric file if trim invantory. [Begin]
          IF lltrim_inv
            REPLACE  UntCost WITH lnUntCost,;
                     TotCost WITH ROUND(UntCost*nBomTotQty,2)          
          ENDIF
          *B603320,1 ABD [End]
          SELECT BOM
        ENDIF
      ENDIF
    ENDIF
    SELECT BOM
  ENDSCAN
  SELECT (lcFabTemp)
ENDSCAN
WAIT CLEAR

SELECT &lcBOMTemp
GO TOP

IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN(.F.)
ENDIF

IF lcForm = 'A'
  SET RELATION TO SUBSTR(&lcBOMTemp..cItmMajor,1,7)+ALLTRIM(&lcBOMTemp..cItmMask) ;
                                               INTO (lcFabTemp) ADDITIVE
ELSE
  SET RELATION TO SUBSTR(&lcBOMTemp..cItmMajor,1,7) INTO (lcFabTemp) ADDITIVE  
ENDIF  

RETURN


*!*************************************************************
*! Name      : lfFabClr
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : function to get fabric colors.
*!*************************************************************
*! Example     : = lfFabClr()
*!*************************************************************
FUNCTION lfFabClr

STORE '' TO lcFabclrs, lcFabclrs2

lcClFlt  = IIF(!EMPTY(lcAllClr),'Color $ lcAllClr','.T.')
lcFabric = &lcFabTemp..Fabric

*-- Get all color of the fabric.
SELECT Fabric
SEEK lcFabric

SCAN REST WHILE Fabric+Color = lcFabric FOR &lcClFlt
  lcFabclrs = lcFabclrs + ALLTRIM(Color)+'/'
ENDSCAN

*-- Remove the last slash from the string.
lcFabclrs = SUBSTR(lcFabclrs,1,LEN(lcFabclrs)-1)
IF LEN(lcFabclrs) > 70
  lcFabclrs2 = SUBSTR(lcFabclrs,70,LEN(lcFabclrs)-70)
ELSE
  lcFabclrs2 = ''
ENDIF

SELECT (lcBOMTemp)
RETURN lcFabclrs

*!*************************************************************
*! Name      : lfItemClr
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : function to get fabric colors.
*!*************************************************************
*! Example     : = lfItemClr()
*!*************************************************************
FUNCTION lfItemClr
PRIVATE lnAlias

lnAlias = SELECT(0)
lcFab   = SUBSTR(&lcBOMTemp..cItmMajor,1,7) && Variable hold current fabric.
lcItem  = Item                              && Variable hold current item.
lcType  = typ                               && Variable hold current cost type.
lnBomRc = RECNO()                           && Variable hold record number.
STORE '' TO lcItmclrs,lcItmclrs2            && Variables to hold item colors.

*-- Get all color of the fabric/type/item.
IF SEEK(lcFab+lcType+lcItem,lcBOMTemp)
  SELECT (lcBOMTemp)
  SCAN REST WHILE SUBSTR(&lcBOMTemp..cItmMajor,1,7) = lcFab ;
            .AND. Typ = lcType .AND. Item = lcItem
    IF !EMPTY(IClr)
      ln1 = 1
      ln3 = 1
      DO WHILE .T.
        ln2 = AT('/',lcItmclrs,ln1) - 1
        IF ln2 = -1 
          lcItmclrs = lcItmclrs + ALLTRIM(IClr)+'/'
          EXIT
        ENDIF
        IF ALLTRIM(IClr) = SUBSTR(lcItmclrs, ln3, ln2-IIF(ln3=1,0,ln3-1))
          EXIT
        ENDIF
        ln3 = ln2+2
        ln1 = ln1+1
      ENDDO
    ENDIF  
  ENDSCAN
  lcItmclrs = SUBSTR(lcItmclrs,1,LEN(lcItmclrs)-1)
  IF LEN(lcItmclrs) > 70
    lcItmclrs2 = SUBSTR(lcItmclrs,70,LEN(lcItmclrs)-70)
  ELSE
    lcItmclrs2 = SPACE(68)+REPLICATE('-',12)
  ENDIF
ENDIF
IF BETWEEN(lnBomRc, 1, RECCOUNT())
  GO lnBomRc
ENDIF  
SELECT (lnAlias)
RETURN lcItmclrs

*!*************************************************************
*! Name      : lfgetlabel
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : function to get fabric labels. Called from Frx
*!*************************************************************
*! Example     : = lfgetlabel()
*!*************************************************************
FUNCTION lfgetlabel

IF Typ <> lcType
  lcType = Typ
  lcLabel= lcLngLbl&lcType
  lnLen=INT(((80-LEN(lcLabel))/2))
  lcLabel=REPLICATE('-',lnLen-2)+"< "+lcLabel+" >"+REPLICATE('-',lnLen-2)
  RETURN lcLabel
ELSE
  RETURN ''
ENDIF  

*!*************************************************************
*! Name      : lfBuilFltr
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : To build the filter for the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfBuilFltr()
*!*************************************************************
FUNCTION lfBuilFltr
PARAMETERS lcOptType

lnOptionNo = ASCAN(laOgFxFlt,lcOptType)
IF lnOptionNo > 0
  lnStatPos  = ASUBSCRIPT(laOgFxFlt,lnOptionNo,1)
  lcFilter   = lcFilter + IIF(!EMPTY(laOgFxFlt[lnStatPos,6]),;
               ' AND '+lcOptType+' = "&laOgFxFlt[lnStatPos,6]"','')
ENDIF

*!*************************************************************
*! Name      : lfvFabric
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : Vaildate the fabric code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)
lcObjVal = EVALUATE(SYS(18))

SELECT FABRIC
lcOldOrd = ORDER()
SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcObjVal) .AND. ('?' $ lcObjVal .OR. !SEEK(lcObjVal,'FABRIC'))
  =FABROW (@lcObjVal ,'*')
ENDIF 
&lcObjNam    = lcObjVal 
IF lcObjNam  = "LCOGVALUEF" 
  lcRpLowFab = lcObjVal 
ELSE
  lcRpHigFab = lcObjVal 
ENDIF

SELECT FABRIC
SET ORDER TO &lcOldOrd

*!*************************************************************
*! Name      : lfvVendor
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : Vaildate the vendor code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)
lcObjVal = EVALUATE(SYS(18))

SELECT APVENDOR
lcOldOrder = ORDER()
SET ORDER TO TAG VenCode 

IF !EMPTY(lcObjVal) .AND. ;
   ('?' $ lcObjVal .OR. !SEEK(lcObjVal , 'APVENDOR'))
   = gfApVnBrow(@lcObjVal)
   &lcObjNam = lcObjVal
ENDIF
SELECT APVENDOR
SET ORDER TO &lcOldOrder

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : To be processed when the option grid is loaded.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH = 'N'