*:************************************************************************
*: Program file      : MAMATRGM.PRG
*: Program desc.     : Material Requirements Report For GMA.
*:         System    : ARIA APPAREL SYSTEM 2.7
*:         Module    : MATERIALS (MA).
*:      Developer    : MHM - Mohamed Shokry
*:           Date    : 02/08/2002               
*:   Call Reports    : MAMATRGM.FRX  Summary format. ,
*:************************************************************************
*: This Report Program is due to 102584,1
*:************************************************************************
*: Modifications :
*B606966,1 TMI 02/22/2003 Fix the bug that the report not show all requered fabrics if they distributed on 
*B606966,1 TMI            more than one location 
*B606966,4 TMI 03/03/2003 Fix the bug that the report duplicates issued qty
*B127519,1 MHM 05/05/2005 Fix the bug of Po Completion date not working in filtering
*:*********************************************************************************************************
*intializing all variables [Begin]

IF llOGFltCh  
  *--variable to get Po No.
  STORE 0 TO lnCountPO
  
  IF TYPE('lcMatReq') $ 'UL'
    lcMatReq = ''
  ENDIF
  
  IF TYPE('lcPoTmpFil') $ 'UL'
    lcPoTmpFil = ''
  ENDIF
  
  IF TYPE('lcTmpDtl') $ 'UL'
    lcTmpDtl = ''
  ENDIF
  
  IF TYPE('lcPOTmpLN') $ 'UL'
    lcPOTmpLN = ''
  ENDIF
  *intializing all variables [End]
  
  STORE 0 TO lnOTotRem , lnHTotRem , lnPTotRem , lnOIssued , lnHIssued , lnPIssued
  STORE '' TO lcFDesc , lcClrDsc
  *--Print the fabric openqty by yards for Mexx
  lnfUOM = 1
  *[End]


  *--The report has 3 filter expretions.
  *--lcRpExp0  Expresion on Style file.
  *--lcRpExp1  Expresion on Fabric file.
  *--lcRpExp2  Expresion on PosHdr file.
  
  *- modify this part to follow up our logic
  *-- for style file

  *--mhmher
  *=ACOPY(laOGFxFlt,laStyFlt, 8,49)
  *DIME laStyFlt[7,7]
  =ACOPY(laOGFxFlt,laStyFlt, 8,56)
  DIME laStyFlt[8,7]
  
  
  *-- For Fabric File
  =ACOPY(laOGFxFlt,laFabFlt, 64,28)
  DIME laFabFlt[4,7]

  *-- For PosHdr File
  =ACOPY(laOGFxFlt,laPosFlt, 99,7)
  DIME laPosFlt[1,7]
 
  =ACOPY(laBrFldFlt,laOldBr)
  *--mhmher
  *laBrFldFlt[1,5] = "laStyFlt:1"
  *laBrFldFlt[2,5] = "laStyFlt:2"
  *laBrFldFlt[3,5] = "laStyFlt:3"
  *laBrFldFlt[4,5] = "laStyFlt:4"
  *laBrFldFlt[5,5] = "laStyFlt:5"
  *laBrFldFlt[6,5] = "laStyFlt:6"
  *laBrFldFlt[7,5] = "laStyFlt:7"
  *laBrFldFlt[8,5] = "laFabFlt:1"
  *laBrFldFlt[9,5] = "laFabFlt:2"
  *laBrFldFlt[10,5] = "laFabFlt:3"
  *laBrFldFlt[11,5] = "laFabFlt:4"
  *laBrFldFlt[12,5] = "laPosFlt:1"

  laBrFldFlt[2,5] = "laStyFlt:1"
  laBrFldFlt[3,5] = "laStyFlt:2"
  laBrFldFlt[4,5] = "laStyFlt:4"
  laBrFldFlt[5,5] = "laStyFlt:5"
  laBrFldFlt[6,5] = "laStyFlt:7"
  laBrFldFlt[7,5] = "laStyFlt:8"
  
  laBrFldFlt[8,5] = "laFabFlt:1"
  laBrFldFlt[9,5] = "laFabFlt:2"
  laBrFldFlt[10,5] = "laFabFlt:3"
  laBrFldFlt[11,5] = "laFabFlt:4"
  
  lcRpExp0 = gfGenFlt('laStyFlt',.T.,.T.)
  lcRpExp0 = IIF(EMPTY(lcRpExp0),".T.",lcRpExp0) 
  
  *B606954,4 TMI [Start] replace the expression of "ICDEPTHD.DEPT" with "STYLE.DEPT" 
  lnPos = AT('ICDEPTHD.DEPT+ICDEPTHD.CSTYGROUP',lcRpExp)
  IF lnPos > 0
    lcRpExp0 = STUFF(lcRpExp0,lnPos,LEN('ICDEPTHD.DEPT+ICDEPTHD.CSTYGROUP'),'STYLE.DEPT')
  ENDIF
  *B606954,4 TMI [End  ] 

  lcRpExp1 = gfGenFlt('laFabFlt',.T.,.T.)
  lcRpExp1 = IIF(EMPTY(lcRpExp1),".T.",lcRpExp1) 
  *B606966,1 TMI [Start] Remove "FABDYE.CWARECODE" from lcRpExp1
  PRIVATE lcExp1
  lcExp1 = lfRmvWH(lcRpExp1)
  *B606966,1 TMI [End  ] 

  lcRpExp2 = gfGenFlt('laPosFlt',.T.,.T.)
  lcRpExp2 = IIF(EMPTY(lcRpExp2),".T.",lcRpExp2) 
  
  *--Program variabels.
  *--Read the Style major part length and item title.
  lcMjrPct  = gfItemMask('PM')
  lcStyTtl  = gfItemMask('HI')
  lCnMJPct  = gfItemMask('PN')
  lnMajorLn = LEN(lcMjrPct)
  lnNMjrLn  = LEN(lCnMJPct)
  *--Use extended size scale.  
  
  *--Flag for style by location or no (Lcation was entered in grid).
  llByLoctn = ('STYDYE.' $ lcRpExp0)
  *--Read style location.
  lcLocation= IIF(llByLoctn,SUBSTR(lcRpExp,AT('STYDYE.',lcRpExp)+20,6),"")


   *--flag if style range was entered in grid.
  llStyRang = ('INLSIT(STYLE.STYLE' $ lcRpExp0)
  *--Low style major code in entered range.
  lcLMStyle = IIF(llStyRang,SUBSTR(lcRpExp,22,lnMajorLn),"")
  
  *--Flag for fabric by location or no (Lcation was entered in grid).
  llByFabLoc= ('FABDYE.' $ lcRpExp1)
  *--Read fabric location.
  
  *--Check if there is no filter on style.
  
  llNoStyFlter = ( lcRpExp0 = ".T." ) OR ;
    ( llByLoctn AND EMPTY(SUBSTR(lcRpExp0 ,1,AT('STYDYE.',lcRpExp0 )-1)) )
  
  *--Open report needed files.
  DIME laArOpnFl[5]
  laArOpnFl = .F.
  laArOpnFl[1] = gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
  laArOpnFl[2] = gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
  laArOpnFl[3] = gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
  laArOpnFl[4] = gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
  laArOpnFl[5] = gfOpenFile(gcDataDir+'POSLN','POSLNS','SH')
  
  *--Create MR temp file with structure.
  =lfCreateFl()
  = gfOpenFile(gcWorkDir+lcMatReq,'','EX')
  *--Open needed indeces.
  *B606966,4 TMI [Start] No need to include the "STYLE" field in the index Matreq
  *INDEX ON Item+IClr+Style+PO TAG Matreq ADDITIVE
  INDEX ON Item+IClr+PO TAG Matreq ADDITIVE
  *B606966,4 TMI [End  ] 
  INDEX ON PO+STYLE TAG MrPOSty ADDITIVE
  *B606966,4 TMI [Start] the index "Mrstyitm" is not used in whole the program , so discard it
  *INDEX ON Style+Item+IClr TAG Mrstyitm ADDITIVE
  *B606966,4 TMI [End  ] 

  ** (1) FIRST PART STEPS. ********************************************
  SELECT (lcMatReq)
  SET ORDER TO TAG MATReq
  SELECT BOM
  SET ORDER TO TAG BOM
  SET RELATION TO IIF(Bom.iclr="*****",LEFT(Bom.item,7),LEFT(Bom.item,7)+Bom.iclr) INTO FABDYE ADDITIVE
  
  SELECT BOMLINE
  *B606966,1 TMI [Start] For some reason the relation is not broken between bomline and fabdye , this line to solve this situation
  SET RELATION OFF INTO FABDYE
  *B606966,1 TMI [End  ] 
  SET RELATION TO LEFT(BomLine.item,7)+BomLine.iclr INTO FABDYE ADDITIVE

  SELECT STYLE
  SET ORDER TO TAG Style
  *--606523,1 mhm add relation to departement
  SET RELATION TO DEPT INTO ICDEPTHD  ADDITIVE
  SET RELATION TO STYLE INTO STYDYE  ADDITIVE
  *--606523,1 mhm add relation to departement
  
  *SELECT STYDYE
  *SET ORDER TO TAG Stydyew
  
  SELECT FABRIC
  SET ORDER TO TAG FABRIC
  SELECT FABDYE
  SET ORDER TO TAG FabDye
  
  SELECT PosLn
  
  *-- To get the selected Styles if any.
  lcStyFile = ''
  llSelSty = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
  IF lnPosition > 0
    lcStyFile = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcStyFile) AND USED(lcStyFile)
      SELECT (lcStyFile)
      GOTO TOP
      llSelSty = !EOF()
    ENDIF  
  ENDIF
  
  
  *- change the way of looping
  *-- To get the selected Styles if any.
  lcPoFile = ''
  llSelPO  = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.PO'),1)
  IF lnPosition > 0
    lcPoFile = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcPoFile) AND USED(lcPoFile)
      SELECT (lcPoFile)
      GOTO TOP
      llSelPO  = !EOF()
    ENDIF  
  ENDIF
  
  *-- To get the selected vendors if any
  lcVendFile = ''
  llSelVend  = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
  IF lnPosition > 0
    lcVendFile = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcVendFile) AND USED(lcVendFile)
      SELECT (lcVendFile)
      GOTO TOP
      llSelVend  = !EOF()
    ENDIF  
  ENDIF
  
  *--to know location of the fabric
  lcFabLoc = ''
  lnSelLoc  = 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'FABDYE.CWARECODE'),1)
  IF lnPosition > 0
    lcFabLoc = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcFabLoc) AND USED(lcFabLoc)
      SELECT (lcFabLoc)
      LOCATE
      lnSelLoc= RECCOUNT()
      LOCATE
    ENDIF  
  ENDIF
  IF lnSelLoc =  1
    lcWareHos = &lcFabLoc..CWARECODE
  ELSE
    lcWareHos = "ALL"
  ENDIF
  
  IF llSelPO
    =lfDataByPo()
  ELSE
    IF llSelSty
      =lfDatBySty()
    ELSE 
      IF llSelVend
         =lfDataByVn()
      ELSE          
        =lfDataByAl()
      ENDIF
    ENDIF
  ENDIF
  
  *-- Function to get Fabrics Qty which not related to any Cost sheet if has OnOrder or OnHand Or Req.
  *B606966,1 TMI [Start] Get this if at least one WH is selected
  *IF lcRpExp1 <> ".T."
  IF "FABDYE.CWARECODE" $ lcRpExp1
    *B606966,1 TMI [End  ] 
    =lfDataByFb()
  ENDIF  

  =lfClearFlt()
ENDIF

SELECT (lcMatReq)
*--mhmher
IF !llRpZeroRq 
  SET FILTER TO ReqTotQt<>0 OR IssTotQt<>0
ENDIF
*--mhmher

*B606966,1 TMI [Start] No need for this function
*=lfUpdStk()
*B606966,1 TMI [End  ] 

IF  lcRpRName ="S"
  DO gfDispRe WITH EVAL('lcRpName')
ELSE
  =lfPrtToEXl()  
ENDIF
*IF .F.
*--Close all Open Files.
*B606966,1 TMI [Start] Don't close these files here , they are closed in lfClearRep
*-* IF USED(lcMatReq)
*-*   USE IN (lcMatReq)
*-* ENDIF
*-* ERASE (gcWorkDir+lcMatReq+'.DBF')
*-* ERASE (gcWorkDir+lcMatReq+'.CDX')
*-* 
*-* IF laArOpnFl[1] AND USED('SCALE')
*-*   USE IN SCALE
*-* ENDIF
*-* 
*-* IF laArOpnFl[2] AND USED('BOMLINE')
*-*   USE IN BOMLINE
*-* ENDIF
*-* 
*-* IF laArOpnFl[3] AND USED('CTKTBOM')
*-*   USE IN CTKTBOM
*-* ENDIF
*-* 
*-* IF laArOpnFl[4] AND USED('POSHDR')
*-*   USE IN POSHDR
*-* ENDIF
*-* IF laArOpnFl[5] AND USED('POSLN')
*-*   USE IN POSLN
*-* ENDIF
*B606966,1 TMI [End  ] 
*ENDIF
RETURN
*--End...

*!*************************************************************
*! Name        : lfwRepWhen
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : Optional Grid When Function.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*--Set needed orders in grid. 
SELECT STYDYE
SET ORDER TO TAG StyDye
SELECT STYLE
SET ORDER TO TAG Style
SELECT FABDYE
SET ORDER TO TAG FabDye
SELECT FABRIC
SET ORDER TO TAG Fabric
SELECT WAREHOUS
SET ORDER TO TAG WAREHOUS

*--Temp files names.
lcMatReq   = gfTempName()
lcTmpItems = gfTempName()

lcOldValue = ' '
*--Get the style major picture.
lcStyPict = '@! '+gfItemMask('PM')
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()

RETURN

*!*************************************************************
*! Name        : lfOldValue
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN
*!*************************************************************
*! Name        : lfGetColor
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN

*!*************************************************************
*! Name        : lfUpdReq
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : Update material requirement file.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfUpdReq()
*!*************************************************************
*B606966,4 TMI [Start] Rewrite this funtion due to the many modifications in it
*FUNCTION lfUpdReq
FUNCTION XlfUpdReq
*B606966,4 TMI [End  ] 
lcCStyle = POSLN.STYLE
SELECT (lcMatReq)
IF !SEEK(lcItem+lcIClr+lcCStyle+POSLN.PO)
  IF STYLE.Make = .F.
    APPEND BLANK  
    REPLACE Style    WITH lcCStyle     ,;
            Typ      WITH BOMLINE.cBomTyp  ,;
            Item     WITH lcItem       ,;
            IClr     WITH lcIClr       ,;
            Cnt      WITH IIF(lnCnt=0,8,lnCnt),;
            lStyMake WITH STYLE.Make   ,;
            cWareCode WITH IIF(llByLoctn,lcLocation,""),;
            PO        WITH POSLN.PO,;
            Status    WITH POSHDR.Status,;
            cVenCOde  WITH POSHDR.vendor,;
            cWareCode WITH POSLN.CWareCode            

  ENDIF          
ENDIF
RETURN


*:**************************************************************************
*:* Name        : lfUpdReq                                        *B606966,4
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/03/2003
*:* Purpose     : Update material requirement file.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdReq()
*:***************************************************************************
FUNCTION lfUpdReq
PRIVATE lcPos
SELECT (lcMatReq)
IF !SEEK(lcItem+lcIClr+POSLN.PO,lcMatReq)
  APPEND BLANK  
  REPLACE Style     WITH POSLN.STYLE  ,;
          Typ       WITH BOMLINE.cBomTyp  ,;
          Item      WITH lcItem       ,;
          IClr      WITH lcIClr       ,;
          Cnt       WITH IIF(lnCnt=0,8,lnCnt),;
          PO        WITH POSLN.PO,;
          Status    WITH POSHDR.Status,;
          cVenCOde  WITH POSHDR.vendor,;
          cWareCode WITH POSLN.CWareCode,;
          ISSTOTQT  WITH IIF(SEEK("I"+POSHDR.PO+"5"+lcItem+lcIClr,'CTKTBOM'),CTKTBOM.Issue_Qty,0),;
          REQTOTQT  WITH REQTOTQT - IIF(POSHDR.STATUS='H',BOM.nBOMTotQty*PosLn.TotQty,BOMLINE.ITEMQTY)
ELSE
  REPLACE REQTOTQT  WITH REQTOTQT - IIF(POSHDR.STATUS='H',BOM.nBOMTotQty*PosLn.TotQty,BOMLINE.ITEMQTY)            
ENDIF 

*-* *--The fields "STKTOTQY" , "ONORDQTY" needs to updated once only ( per WH ), so the field lUpdonce is added for this porpous
*-* IF SEEK(lcItem+lcIClr,lcMatReq)
*-*   =SEEK(PADR(lcItem,7)+lcIClr+CWARECODE,'FABDYE')
*-*   SELECT (lcMatReq)
*-*   IF llByFabLoc
*-*     LOCATE REST WHILE Item+IClr+PO = lcItem+lcIClr ;
*-*                 FOR CWARECODE = POSLN.CWareCode                
*-*   ENDIF
*-*   IF !lUpdonce
*-*     REPLACE STKTOTQY  WITH IIF(llByFabLoc,IIF(&lcRpExp1,FABDYE.OnHand ,0),FABRIC.OnHand),;
*-*             ONORDQTY  WITH IIF(llByFabLoc,IIF(&lcRpExp1,FABDYE.ONORDER,0),FABRIC.ONORDER),;
*-*             lUpdonce  WITH .T.
*-*   ENDIF       
*-* ENDIF          

*--The fields "STKTOTQY" , "ONORDQTY" needs to updated once only ( per WH ), so the field lUpdonce is added for this porpous
IF SEEK(lcItem+lcIClr,lcMatReq) AND !&lcMatReq..lUpdonce
  IF !llByFabLoc                              && No WH selected , get all stkqty and onorder qty from fabric file
    =SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
    REPLACE STKTOTQY  WITH FABRIC.OnHand,;
            ONORDQTY  WITH FABRIC.ONORDER,;
            lUpdonce  WITH .T.
  ELSE    
    *-- if some WH are selected , sum the stock and onorder only of the selected WH's only
    lcPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'FABDYE.CWARECODE'),1)
    SELECT (laOgFxFlt[lcPos,6])
    SCAN
      =SEEK(PADR(lcItem,7)+lcIClr+CWARECODE,'FABDYE')
      SELECT (lcMatReq)
      REPLACE STKTOTQY  WITH STKTOTQY + FABDYE.OnHand ,;
              ONORDQTY  WITH ONORDQTY + FABDYE.ONORDER
    ENDSCAN
    SELECT (lcMatReq)
    REPLACE lUpdonce  WITH .T.
  ENDIF
ENDIF          
 

*!*************************************************************
*! Name        : lfReadItem
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : To read item data
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfReadItem()
*!*************************************************************
FUNCTION lfReadItem

&& CCATGTYP $ 'F'
IF PosHdr.Status = "H"
  lcItem = BOM.Item
  lcIClr = IIF(BOM.IClr='*',SUBSTR(POSLN.STYLE,lnClrSrt,lnClrEnd),BOM.IClr)
ELSE
  lcItem = BOMLINE.Item
  lcIClr = IIF(BOMLINE.IClr='*',SUBSTR(lcCStyle,lnClrSrt,lnClrEnd),BOMLINE.IClr)
ENDIF  
RETURN .T.

*!*************************************************************
*! Name        : lfvVendor
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lnSelAls , lcVar
lnSelAls = SELECT(0)

SELECT APVENDOR
SET ORDER TO TAG VenCode 
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))   && Varible to hold the current field value


IF !EMPTY(lcObj) .AND. ;
   ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
ENDIF
&lcVar = lcObj      && Update the field
SELECT (lnSelAls)
*--------------------- END OF lfvVendor

*!*************************************************************
*! Name        : lfClearRep
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/12/2002
*! Purpose     : Function that we call when Close the option grid.
*!                   : to clear our variables.
*!*************************************************************
*! Called from : [Option Grid] < Close >
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

PRIVATE laOpenFile,laTempFile,lnFilCnt
DIMENSION laOpenFile[10,1],laTempFile[5,1]

laOpenFile[1,1] = 'SYCCOMP'
laOpenFile[2,1] = 'CODES'
laOpenFile[3,1] = 'SCALE'
laOpenFile[4,1] = 'BOMLINE'
laOpenFile[5,1] = 'CTKTBOM'
laOpenFile[6,1] = 'POSHDR'
laOpenFile[7,1] = 'POSLN'
laOpenFile[8,1] = 'APVENDOR'
laOpenFile[9,1] = 'POFHDR'
laOpenFile[10,1] = 'POFLN'

*-- Close all open files. [Begin
FOR lnFilCnt = 1 TO ALEN(laOpenFile,1)
  IF USED(laOpenFile[lnFilCnt,1]) AND (ASCAN(laSelFile,laOpenFile[lnFilCnt,1]) = 0)
    USE IN (laOpenFile[lnFilCnt,1])
  ENDIF
ENDFOR
*-- Close all open files. [End

laTempFile[1,1] = 'lcTmpRmain'
laTempFile[2,1] = 'lcMatReq'
laTempFile[3,1] = 'lcTmpItems'
laTempFile[4,1] = 'lcPOTmpHD'
laTempFile[5,1] = 'lcPOTmpLN'

*-- Close all Temp. files. [Begin
FOR lnFilCnt = 1 TO ALEN(laTempFile,1)
  IF TYPE(laTempFile[lnFilCnt,1]) = 'C' AND USED(laTempFile[lnFilCnt,1])
    USE IN (laTempFile[lnFilCnt,1])
    IF FILE(gcWorkDir+laTempFile[lnFilCnt,1]+'.DBF')
      ERASE (gcWorkDir+laTempFile[lnFilCnt,1]+'.DBF')
    ENDIF
    IF FILE(gcWorkDir+laTempFile[lnFilCnt,1]+'.CDX')
      ERASE (gcWorkDir+laTempFile[lnFilCnt,1]+'.CDX')
    ENDIF
    IF FILE(gcWorkDir+laTempFile[lnFilCnt,1]+'.FPT')
      ERASE (gcWorkDir+laTempFile[lnFilCnt,1]+'.FPT')
    ENDIF
  ENDIF
ENDFOR
*-- Close all Temp. files. [End
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
*B606966,1 TMI [Start] Save the old alias
PRIVATE lnSlct
lnSlct = SELECT()
*B606966,1 TMI [End  ] 
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*B606966,1 TMI [Start] restore the old work aria
SELECT (lnSlct)
*B606966,1 TMI [End  ] 
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
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
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
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
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
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
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfFabSum.
*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
ENDCASE
*-- end of lfSRVFab.
*!*************************************************************
*! Name        : lfsrvTrans
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/12/2002
*! Purpose     : To set relation on or off when running the in range function 
*!                 in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
    SET FILTER TO STATUS $"OH"
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
    SET FILTER TO 
ENDCASE
*-- end of lfsrvTrans.

*!*************************************************************
*! Name        : lfCreateFl
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/12/2002
*! Purpose     : Create the report temp file.
*!*************************************************************
FUNCTION lfCreateFl

DIMENSION laFlSruc[20,4]
*--First array element [Name].

laFlSruc[1,1] = "STYLE"
laFlSruc[2,1] = "ITEM"
laFlSruc[3,1] = "ICLR"
laFlSruc[4,1] = "DESC"
laFlSruc[5,1] = "CWARECODE"
laFlSruc[6,1] = "TYP"
laFlSruc[7,1] = "CCATGTYP"
laFlSruc[8,1] = "CNT"
laFlSruc[9,1] = "UOM"
laFlSruc[10,1]= "STKTOTQY"
laFlSruc[11,1]= "REQTOTQT"
laFlSruc[12,1]= "ISSTOTQT"
laFlSruc[13,1]= "ONORDQTY"

laFlSruc[14,1]= "NYTOWIP"
laFlSruc[15,1]= "NUSEDREQ"
laFlSruc[16,1]= "NNETREQ"
laFlSruc[17,1]= "LSTYMAKE"

laFlSruc[18,1] = "PO"
laFlSruc[19,1] = "cVenCode"
laFlSruc[20,1] = "Status"


*--Second array element [Type].
STORE "C" TO laFlSruc[1,2] ,laFlSruc[2,2] ,laFlSruc[3,2] ,laFlSruc[4,2],;
             laFlSruc[5,2] ,laFlSruc[6,2] ,laFlSruc[7,2] ,laFlSruc[9,2],;
             laFlSruc[18,2] ,laFlSruc[19,2] ,laFlSruc[20,2]
             
STORE "N" TO laFlSruc[10,2],laFlSruc[11,2],laFlSruc[12,2],laFlSruc[13,2],;
             laFlSruc[14,2],laFlSruc[15,2],laFlSruc[16,2],laFlSruc[8,2]
STORE "L" TO laFlSruc[17,2]


*--Thered array element [Length].
STORE  1  TO laFlSruc[6,3] ,laFlSruc[7,3] ,laFlSruc[8,3],laFlSruc[16,3],laFlSruc[20,3],laFlSruc[17,3]
STORE  3  TO laFlSruc[9,3]
STORE  6  TO laFlSruc[3,3] ,laFlSruc[5,3] ,laFlSruc[18,3]

STORE  12  TO laFlSruc[10,3],laFlSruc[11,3],laFlSruc[12,3],laFlSruc[13,3]
STORE  8  TO laFlSruc[19,3]

STORE 19  TO laFlSruc[1,3] ,laFlSruc[2,3]
STORE 12  TO laFlSruc[14,3],laFlSruc[15,3],laFlSruc[16,3]
STORE 20  TO laFlSruc[4,3]

*--Forth array element [Decemal].
STORE  0  TO laFlSruc[1,4] ,laFlSruc[2,4] ,laFlSruc[3,4] ,laFlSruc[4,4], ;
             laFlSruc[5,4] ,laFlSruc[6,4] ,laFlSruc[7,4] ,laFlSruc[8,4], ;
             laFlSruc[9,4] ,laFlSruc[17,4],laFlSruc[18,4],laFlSruc[19,4],;
             laFlSruc[20,4]
             
STORE  3  TO laFlSruc[14,4],laFlSruc[15,4],laFlSruc[10,4],laFlSruc[11,4],laFlSruc[12,4],;
             laFlSruc[13,4],laFlSruc[16,4] 

*B606966,4 TMI [Start] Add the field "lUpdonce" so that the stk and OnOrder is updated once for each fabric/color
PRIVATE lnDim
lnDim = ALEN(laFlSruc,1) + 1
DIMENSION laFlSruc[lnDim,4]
laFlSruc[lnDim,1] = 'lUpdOnce'
laFlSruc[lnDim,2] = 'L'
laFlSruc[lnDim,3] = 1
laFlSruc[lnDim,4] = 0
*B606966,4 TMI [End  ] 

CREATE DBF (gcWorkDir+lcMatReq) FROM ARRAY laFlSruc
RETURN

****************************************************************************
*! PROG      : lfDataByPo.PRG   (C#102584)
*! DESC      : function To collect data in the temporary file
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfDataByPo
PRIVATE lcOrdPo

SELECT (lcPoFile)
GOTO TOP

*--change order of POSLN
SELECT POSLN   
lcOrdPo = SET('ORDER')
SET ORDER TO TAG POSLN 
SELECT (lcPoFile)
SCAN
  =SEEK('P'+PO,'PosHdr')
  IF llSelVend AND !SEEK(PosHdr.Vendor,lcVendFile)
    LOOP
  ENDIF  

  *--606523,1 check for completed satus [Start]
  IF PosHdr.Status $ "CXS"
    LOOP
  ENDIF  
  *--606523,1 check for completed satus [End]
  
  IF !EVAL(LcRpExp2)
    LOOP
  ENDIF
  IF SEEK("P"+Po,'POSLN')
    SELECT POSLN
    *--mhm2000
    lcStyMajor = ''
    SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd= "P" + &lcPoFile..PO
     lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
      IF POSLN.TranCd # "1"
        LOOP
      ENDIF      
      IF PosHdr.Status = "H"
        *--Read an item code and an item color[lcItem,lcIClr].
        IF SEEK(PADR(SUBSTR(Style,1,lnMajorLn),19)+'5','BOM')        
          SELECT BOM
          SCAN WHILE citmmajor+typ+citmmask+mfgcode+item+iclr =;
                     PADR(SUBSTR(POSLN.STYLE,1,lnMajorLn),19)+'5' ;
                 FOR LIKE(STRTRAN(cItmMask,'*','?'),POSLN.STYLE)
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF !( &lcRpExp1 ) 
            IF !( &lcExp1 ) 
              *B606966,1 TMI [End  ] 
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            *--MHM2000
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN  
        ENDIF  
      ELSE    
        IF SEEK("I"+'1'+PO+STR(PosLn.lineno,6)+'5'+PosLn.Style,'BOMLINE')        
          SELECT BOMLINE

          SCAN REST WHILE cImTyp+cType+cTktNo+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                          "I"+'1'+POSLN.PO+STR(PosLn.lineno,6)+'5'+PosLn.Style
        
            *--Read an item code and an item color[lcItem,lcIClr].
            *=SEEK(ALLTRIM(Style),'BOM')        
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF (BOMLINE.cCatgTyp = 'F' ) AND !( &lcRpExp1 ) 
            IF (BOMLINE.cCatgTyp = 'F' ) AND !( &lcExp1 )
              *B606966,1 TMI [End  ] 
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            =SEEK('P'+POSLN.PO,'PosHdr')
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN
        ENDIF      
      ENDIF    
    
      *IF lcStyMajor = SUBSTR(Style,1,lnMajorLn)
      *  LOOP
      *ENDIF
      *lcStyMajor = SUBSTR(Style,1,lnMajorLn)
      *=SEEK(Style,'Style')
      *IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
      *  LOOP
      *ENDIF
      *IF !EVAL(lcRpExp0) AND !EVAL(lcRpExp1)
      *  LOOP
      *ENDIF
      *IF llSelSty AND !SEEK(SUBSTR(Style,1,lnMajLen),lcStyFile)
      *  LOOP
      *ENDIF
      **--MHM2000
      *IF POSLN.TranCd # "1"
      *  LOOP
      *ENDIF
      **--MHM2000
      *lcStyMajor = SUBSTR(Style,1,lnMajorLn)
      *lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
      *lcCStyle = Style.Style
      *IF SEEK(ALLTRIM(lcStyMajor),'BOM')        
      *  SELECT BOM
      *  
      *  SCAN WHILE cItmMajor = SUBSTR(lcCStyle,1,lnMajorLn) ;
      *         FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
      *             cCatgTyp $ 'F'
      *    *--Read an item code and an item color[lcItem,lcIClr].
      *    STORE '' TO lcItem,lcIClr
      *    IF !lfReadItem()
      *      LOOP
      *    ENDIF 
      *    *--Check if maririal/color entered in grid is valid.
      *    IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
      *      LOOP
      *    ENDIF
      *    IF (cCatgTyp = 'F' ) AND !( &lcRpExp1 ) 
      *      LOOP
      *    ENDIF
      *    *--Append or Update record in (lcMatReq) file. 
      *     =lfUpdReq()
      *     =lfGetQty()
      *  ENDSCAN
      *ENDIF      
    ENDSCAN 
  ENDIF
ENDSCAN
SELECT POSLN   
SET ORDER TO &lcOrdPo.

****************************************************************************
*! PROG      : lfDatBySty.PRG   (C#102584)
*! DESC      : function To scan for the selected Styles
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*-* *-- comment Function to change way we collecting data by
*-* FUNCTION lfDatByStoooo
*-* *--mhmher
*-* PRIVATE llCheckMe
*-* *--mhmher
*-* 
*-* SELECT (lcStyFile)
*-* GOTO TOP
*-* SCAN
*-*   STORE .F. TO llCheckMe
*-*   =SEEK(ALLTRIM(cStyMajor),'Style')
*-*   *--mhm2000
*-*   *lcStyMajor = ''
*-*   *--mhm2000
*-*   *SELECT STYLE
*-*   *SCAN REST WHILE STYLE = ALLTRIM(&lcStyFile..cStyMajor)
*-*   
*-*     *--mhm2000
*-*   *  IF lcStyMajor = SUBSTR(STYLE.Style,1,lnMajorLn)
*-*   *    LOOP
*-*   *  ENDIF
*-*   *  lcStyMajor = SUBSTR(STYLE.Style,1,lnMajorLn)
*-*     *--mhm2000
*-*     IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
*-*       LOOP
*-*     ENDIF
*-*     IF !SEEK(ALLTRIM(cStyMajor),'POSLN')
*-*       LOOP
*-*     ENDIF
*-*     =SEEK('P'+POSLN.PO,'PosHdr')
*-*   
*-*     *--606523,1 check for completed satus [Start]
*-*     IF PosHdr.Status $ "CXS"
*-*       LOOP
*-*     ENDIF  
*-*     *--606523,1 check for completed satus [End]
*-*     
*-*     IF llSelVend AND !SEEK(PosHdr.Vendor,lcVendFile)
*-*       LOOP
*-*     ENDIF  
*-*     IF !EVAL(lcRpExp0) AND !EVAL(lcRpExp1) 
*-*       LOOP
*-*     ENDIF
*-*     lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
*-*     
*-*     lcCStyle = ALLTRIM(&lcStyFile..CStyMajor)
*-*     IF SEEK(ALLTRIM(cStyMajor),'BOM')        
*-*       SELECT BOM
*-*       *SCAN WHILE cItmMajor = SUBSTR(lcCStyle,1,lnMajorLn) ;
*-*       *       FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
*-*       *           cCatgTyp $ 'F'
*-*        lcOldClr = Iclr 
*-*       SCAN WHILE cItmMajor = SUBSTR(lcCStyle,1,lnMajorLn) ;
*-*              FOR cCatgTyp $ 'F'
*-*         *--Read an item code and an item color[lcItem,lcIClr].
*-*         *--mhmher
*-*         IF llCheckMe 
*-*           llCheckMe =.T.
*-*           LOOP
*-*         ENDIF
*-*         IF '******'$ ALLTRIM(BOM.citmmask)AND !EMPTY(Iclr) AND (lcOldClr = Iclr)
*-*           llCheckMe =.T.
*-*         ENDIF
*-*         lcOldClr = Iclr 
*-*         *--mhm2000
*-*         STORE '' TO lcItem,lcIClr
*-*         IF !lfReadItem()
*-*           LOOP
*-*         ENDIF 
*-*         IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
*-*           LOOP
*-*         ENDIF
*-*         *--Check if maririal/color entered in grid is valid.
*-*         IF (cCatgTyp = 'F' ) AND !( &lcRpExp1 )  
*-*           LOOP
*-*         ENDIF
*-*         *--Append or Update record in (lcMatReq) file. 
*-*         SELECT POSLN
*-*         =SEEK(ALLTRIM(&lcStyFile..CStyMajor))
*-*         SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd = SUBSTR(lcCStyle,1,lnMajorLn)
*-*           *--MHM2000
*-*           IF POSLN.TranCd # "1"
*-*             LOOP
*-*           ENDIF
*-*           *--MHM2000
*-*           =SEEK('P'+POSLN.PO,'PosHdr')
*-*           =lfUpdReq()
*-*           *=lfGetQty()       &&*B606966,4 TMI
*-*         ENDSCAN
*-*       ENDSCAN  
*-*     ENDIF      
*-*   *ENDSCAN
*-* ENDSCAN
*-* 
****************************************************************************
*! PROG      : lfDataByVn.PRG   (C#102584)
*! DESC      : function To scan for the selected Vendors
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*-- 
FUNCTION lfDataByVn
PRIVATE lcOrder,lcOrdPo
*--mhmher
PRIVATE llCheckMe,lcOldStyM
STORE '' TO lcOldStyM 
*--mhmher

SELECT POSHDR
lcOrder = SET('ORDER')
SET ORDER TO TAG PosHdrv

SELECT POSLN   
lcOrdPo = SET('ORDER')
SET ORDER TO TAG POSLN 

SELECT (lcVendFile)
GOTO TOP
SCAN
  IF !SEEK(CVendCode,'PosHdr')
    LOOP
  ENDIF  
  SELECT POSHDR
  SCAN REST WHILE vendor+cstytype+po = &lcVendFile..CVendCode+"P"

    *--606523,1 check for completed satus [Start]
    IF PosHdr.Status $ "CXS"
      LOOP
    ENDIF  
    *--606523,1 check for completed satus [End]
  
    =SEEK('P'+POSHDR.PO,'POSLN')
    SELECT POSLN
    SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = "P"+POSHDR.PO

*---------
      IF POSLN.TranCd # "1"
        LOOP
      ENDIF
      lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
      IF PosHdr.Status = "H"
        *--Read an item code and an item color[lcItem,lcIClr].
        IF SEEK(PADR(SUBSTR(Style,1,lnMajorLn),19)+'5','BOM')        
          SELECT BOM
          SCAN WHILE citmmajor+typ+citmmask+mfgcode+item+iclr =;
                     PADR(SUBSTR(POSLN.STYLE,1,lnMajorLn),19)+'5' ;
                 FOR LIKE(STRTRAN(cItmMask,'*','?'),POSLN.STYLE)
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF !( &lcRpExp1 ) 
            IF !( &lcExp1 )  
              *B606966,1 TMI [End  ] 
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            *--MHM2000
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN  
        ENDIF  
      ELSE    
        IF SEEK("I"+'1'+PO+STR(PosLn.lineno,6)+'5'+PosLn.Style,'BOMLINE')        
          SELECT BOMLINE

          *--tell here
          SCAN REST WHILE cImTyp+cType+cTktNo+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                          "I"+'1'+POSLN.PO+STR(PosLn.lineno,6)+'5'+PosLn.Style
        
            *--Read an item code and an item color[lcItem,lcIClr].
            *=SEEK(ALLTRIM(Style),'BOM')        
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF (BOMLINE.cCatgTyp= 'F' ) AND !( &lcRpExp1 )
            IF (BOMLINE.cCatgTyp= 'F' ) AND !( &lcExp1 )
              *B606966,1 TMI [End  ]
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN
        ENDIF      
      ENDIF    
    
*---------
      *=SEEK(Style,'Style')
      *IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
      *  LOOP
      *ENDIF
      *IF !EVAL(lcRpExp0) AND !EVAL(lcRpExp1)
      *  LOOP
      *ENDIF
      **--MHM2000
      *IF POSLN.TranCd # "1"
      *  LOOP
      *ENDIF
      **--MHM2000
      *lcStyMajor = SUBSTR(Style,1,lnMajorLn)
      *lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
      *lcCStyle = Style.Style
      *IF SEEK(ALLTRIM(lcStyMajor),'BOM')        
      *  SELECT BOM
      *  SCAN WHILE cItmMajor = SUBSTR(lcCStyle,1,lnMajorLn) ;
      *         FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
      *             cCatgTyp $ 'F'
      *    *--Read an item code and an item color[lcItem,lcIClr].
      *    STORE '' TO lcItem,lcIClr
      *    IF !lfReadItem()
      *      LOOP
      *    ENDIF 
      *    IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
      *      LOOP
      *    ENDIF
      *    *--Check if maririal/color entered in grid is valid.
      *    IF (cCatgTyp = 'F' ) AND !( &lcRpExp1 ) 
      *      LOOP
      *    ENDIF
      *    *--Append or Update record in (lcMatReq) file. 
      *    =lfUpdReq()
      *    =lfGetQty()
      *  ENDSCAN
      *ENDIF      
    ENDSCAN
  ENDSCAN
ENDSCAN

SELECT POSHDR
SET ORDER TO &lcOrder
SELECT POSLN
SET ORDER TO &&lcOrdPo

****************************************************************************
*! PROG      : lfDataByAl.PRG   (C#102584)
*! DESC      : function To collect all lines from posln
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : lfReltdDat.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfDataByAl

SELECT STYLE
*SCAN FOR &lcRpExp0 
SCAN 
  IF !(STYLE.STATUS $ "AH")
    LOOP
  ENDIF
  WAIT WINDOW 'Collecting the cost sheet information for Style : '+Style NOWAIT
  IF !SEEK(STYLE,'POSLN')
      LOOP
    ENDIF
    
    IF llSelVend AND !SEEK(PosHdr.Vendor,lcVendFile)
      LOOP
    ENDIF  
    *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
    *IF !EVAL(lcRpExp0) AND !EVAL(lcRpExp1) 
    *-IF !EVAL(lcRpExp0) AND !EVAL(lcExp1) *B606966,4 TMI [End  ] 
    IF !EVAL(lcRpExp0) OR !EVAL(lcExp1)
      *B606966,1 TMI [End  ] 
      LOOP
    ENDIF
    lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
    lcCStyle = Style.Style
    SELECT POSLN
    =SEEK(STYLE.STYLE)
    SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd = STYLE.STYLE
      =SEEK('P'+POSLN.PO,'PosHdr')      
      IF PosHdr.Status $ "CXS"
        LOOP
      ENDIF  
      *B127519,1 MHM 05/05/2005 Fix the bug of Po Completion date not working in filtering[Start]
      IF !EVAL(lcRpExp2) 
        LOOP
      ENDIF
      *B127519,1 MHM 05/05/2005 [End]
      IF POSLN.TranCd # "1"
        LOOP
      ENDIF
      IF PosHdr.Status = "H"
        IF SEEK(PADR(SUBSTR(Style,1,lnMajorLn),19)+'5','BOM')        
          SELECT BOM
          SCAN WHILE citmmajor+typ+citmmask+mfgcode+item+iclr =;
                     PADR(SUBSTR(POSLN.STYLE,1,lnMajorLn),19)+'5' ;
                 FOR LIKE(STRTRAN(cItmMask,'*','?'),POSLN.STYLE)
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF !( &lcRpExp1 )
            IF !( &lcExp1 )
              *B606966,1 TMI [End  ] 
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            *--MHM2000
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN  
        ENDIF  
      ELSE    
        IF SEEK("I"+'1'+PO+STR(PosLn.lineno,6)+'5'+PosLn.Style,'BOMLINE')        
          SELECT BOMLINE

          SCAN REST WHILE cImTyp+cType+cTktNo+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                          "I"+'1'+POSLN.PO+STR(PosLn.lineno,6)+'5'+PosLn.Style
        
            *--Read an item code and an item color[lcItem,lcIClr].
            *=SEEK(ALLTRIM(Style),'BOM')        
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF (BOMLINE.cCatgTyp = 'F' ) AND !( &lcRpExp1 )  
            IF (BOMLINE.cCatgTyp = 'F' ) AND !( &lcExp1 )  
              *B606966,1 TMI [End  ] 
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            =SEEK('P'+POSLN.PO,'PosHdr')
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN
        ENDIF      
      ENDIF
    ENDSCAN  *  lcCStyle = Style.Style
*  IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
*    LOOP
*  ENDIF
**  IF !SEEK(SUBSTR(Style,1,lnMajorLn),'POSLN')
*    LOOP
*  ENDIF
*  =SEEK('P'+POSLN.PO,'PosHdr')
*  *--606523,1 check for completed satus [Start]
*  IF PosHdr.Status $ "CXS"
*    LOOP
*  ENDIF  
  *--606523,1 check for completed satus [End]
  
*  IF !EVAL(lcRpExp2)
*    LOOP
*  ENDIF
*  lcStyMajor = SUBSTR(Style,1,lnMajorLn)
*  lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
*  lcCStyle = Style.Style

*  IF SEEK(ALLTRIM(lcStyMajor),'BOM')        
*    SELECT BOM
*    SCAN WHILE cItmMajor = SUBSTR(lcCStyle,1,lnMajorLn) ;
*           FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
*               cCatgTyp $ 'F'
*      *--Read an item code and an item color[lcItem,lcIClr].
*      STORE '' TO lcItem,lcIClr
*      IF !lfReadItem()
*        LOOP
*      ENDIF 
*      IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
*        LOOP
*      ENDIF
*      *--Check if matirial/color entered in grid is valid.
*      IF (cCatgTyp = 'F' ) AND !( &lcRpExp1 ) 
*        LOOP
*      ENDIF
*      *--Append or Update record in (lcMatReq) file. 
*      SELECT POSLN
*      =SEEK(STYLE.STYLE)
*      SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd = STYLE.STYLE
*        =SEEK('P'+POSLN.PO,'PosHdr')
*        *--606523,1 check for completed satus [Start]
*        IF PosHdr.Status $ "CXS"
*          LOOP
*        ENDIF  
*        *--606523,1 check for completed satus [End]
*        *--MHM2000
*        IF POSLN.TranCd # "1"
*          LOOP
*        ENDIF
*        *--MHM2000
*        =lfUpdReq()
*        =lfGetQty()
*      ENDSCAN
*    ENDSCAN
*  ENDIF      
ENDSCAN

****************************************************************************
*! PROG      : lfDataByFb.PRG   (C#102584)
*! DESC      : Function to get Fabrics Qty which not related to any Cost 
*!           : sheet if has OnOrder or OnHand Or Req.
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfDataByFb
PRIVATE lcOrdMat

SELECT (lcMatreq)
lcOrdMat = SET('ORDER')
SET ORDER TO TAG Matreq
*--mhm2000
*B606966,1 TMI [Start] Always set relation to fabric only
*IF llByFabLoc
*B606966,1 TMI [End  ] 

SELECT  FABDYE
SET RELATION TO FABRIC+COLOR INTO FABRIC

*B606966,1 TMI [Start] Always set relation to fabric only
*-* ELSE
*-*   SELECT FABRIC
*-*   SET RELATION TO FABRIC+COLOR INTO FABDYE   
*-* ENDIF  
*B606966,1 TMI [End  ] 
*--mhm2000

SCAN FOR &lcRpExp1
  *B606966,4 TMI [Start] if fabric/color is entred befor , ignor it
  *-* *IF SEEK(FABRIC+COLOR,lcMatreq)  
  IF SEEK(PADR(FABRIC,19)+COLOR,lcMatreq) AND &lcMatreq..lUpdonce
    *B606966,4 TMI [End  ] 
    LOOP
  ENDIF
  IF OnHand = 0 AND OnOrder=0
    LOOP
  ENDIF  
  lnOnHand  = OnHand
  lnOnOrder = OnOrder
  SELECT (lcMatreq)
  APPEND BLANK  
  *B606966,1 TMI [Start] Add the FABDYE.CWARECODE instead of "lcLocation"
  *REPLACE Item     WITH Fabric.Fabric,;
          IClr     WITH Fabric.Color ,;
          Desc     WITH Fabric.Desc  ,;
          Uom      WITH Fabric.UomBuy,;
          cWareCode WITH IIF(llByLoctn,lcLocation,""),;
          STKTOTQY  WITH lnOnHand,;
          ONORDQTY  WITH lnOnOrder          
  REPLACE Item      WITH Fabric.Fabric,;
          IClr      WITH Fabric.Color ,;
          Desc      WITH Fabric.Desc  ,;
          Uom       WITH Fabric.UomBuy,;
          cWareCode WITH FABDYE.CWARECODE,;
          STKTOTQY  WITH lnOnHand,;
          ONORDQTY  WITH lnOnOrder
  *B606966,1 TMI [End  ]                     
ENDSCAN

****************************************************************************
*! PROG      : lfGetQty.PRG   (C#102584)
*! DESC      : Function to get Fabrics Qty which not related to any Cost 
*!           : sheet if has OnOrder or OnHand Or Req.
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*B606966,4 TMI [Start] This function is always called after "lfUpdReq" fn., so remove it and add its 
*B606966,4             its code with the needed updates to "lfUpdReq"
*FUNCTION lfGetQty
FUNCTION xlfGetQty
*B606966,4 TMI [End  ] 


IF POSHDR.STATUS = "H"
  REPLACE &lcMatreq..REQTOTQT WITH &lcMatreq..REQTOTQT - BOM.nBOMTotQty * PosLn.TotQty  
ELSE
  SELECT CTKTBOM
  IF SEEK("I"+POSHDR.PO+"5"+lcItem+lcIClr)
    REPLACE &lcMatreq..ISSTOTQT WITH Issue_Qty
  ENDIF  
  SELECT(lcMatreq)
  REPLACE REQTOTQT WITH REQTOTQT - BOMLINE.ITEMQTY
ENDIF

*B606966,1 TMI [Start] if WH is selected , only consider its qty for Stock and Onorder
*REPLACE &lcMatreq..STKTOTQY WITH IIF(llByFabLoc,FABDYE.OnHand,FABRIC.OnHand);
        &lcMatreq..ONORDQTY WITH IIF(llByFabLoc,FABDYE.ONORDER,FABRIC.ONORDER) 
=SEEK(PADR(&lcMatreq..ITEM,7)+&lcMatreq..ICLR+&lcMatreq..CWARECODE,'FABDYE')
REPLACE &lcMatreq..STKTOTQY WITH IIF(llByFabLoc,IIF(&lcRpExp1,FABDYE.OnHand,0),FABRIC.OnHand);
        &lcMatreq..ONORDQTY WITH IIF(llByFabLoc,IIF(&lcRpExp1,FABDYE.ONORDER,0),FABRIC.ONORDER) 
*B606966,1 TMI [End  ]                 

****************************************************************************
*! PROG      : lfClearFlt.PRG   (C#102584)
*! DESC      : Function to get Fabrics Qty which not related to any Cost 
*!           : sheet if has OnOrder or OnHand Or Req.
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfClearFlt

laStyFlt = .F.
laFabFlt = .F.
laPosFlt = .F.
lcRpExp0 = ''
lcRpExp1 = ''
lcRpExp2 = ''
*--retrive browse fields
=ACOPY(laOldBr,laBrFldFlt)

****************************************************************************
*! PROG      : lfPrtToEXl.PRG   (C#102584)
*! DESC      : Function to print detail to Excel sheet
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfPrtToEXl

IF llOGFltCh
  *--create temp file to get All PO's , vendors and status

  *--606523,1 add towe fields to accumelate totals
  *CREATE DBF (gcWorkDir+lcPoTmpFil) ( PO C(6), Vendor C(8),Status C(1))
  CREATE DBF (gcWorkDir+lcPoTmpFil) ( PO C(6), Vendor C(8),Status C(1) , TotIssu N(14,2),TotReq N(14,2))
  *--606523,1 add towe fields to accumelate totals

  INDEX ON PO TAG (lcPoTmpFil)

  lnCountPO = 0
  lcPoNo = '      '
  SELECT(lcMatReq)
  SET ORDER TO TAg MrPOSty
  SCAN
    *--606523,1 MHM Check for not exceed 256 Culmn [Start]
    *IF (PO <> lcPoNo) 
    IF lnCountPO >123
        =gfModalGen("INM00000B42001","Dialog",.F.,.F.,;
                 "The report will print a maximum of 256 columns.")
      EXIT
    ENDIF    
    IF (PO <> lcPoNo) AND !SEEK(PO,lcPoTmpFil)

    *--606523,1 MHM Check for not exceed 256 Culmn [End]
      lnCountPO = lnCountPO+1
      lcPoNo = PO
      SELECT(lcPoTmpFil)
      APPEND BLANK
      REPLACE PO      WITH lcPoNo,;
              Vendor  WITH &lcMatReq..cVenCode,;
              Status  WITH &lcMatReq..status
    ENDIF  
  ENDSCAN

  *--function to prep. temp file to print from
  =lfPrepTemp()
  =lfUpdatQty()
ENDIF
=lfPrint()
****************************************************************************
*! PROG      : lfPrepTemp.PRG   (C#102584)
*! DESC      : Function to print detail to Excel sheet
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfPrepTemp

DIMENSION laTempStru[1,4]
laTempStru = ''

DIMENSION laTempStru[ALEN(laTempStru,1) , 4]
laTempStru[ALEN(laTempStru,1),1] = 'FabrColr'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 17
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NSTOCK'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

*--606523,1 mhm
*FOR lnCount = 1 TO lnCountPO

*  lcCount = ALLTRIM(STR(lnCount,5))
*  lcVarReq="NREQ"+lcCount
*  lcVarIss="NISSU"+lcCount
*  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
*  laTempStru[ALEN(laTempStru,1),1] = lcVarReq
*  laTempStru[ALEN(laTempStru,1),2] = 'N'
*  laTempStru[ALEN(laTempStru,1),3] = 13
*  laTempStru[ALEN(laTempStru,1),4] = 0

*  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
*  laTempStru[ALEN(laTempStru,1),1] = lcVarIss
*  laTempStru[ALEN(laTempStru,1),2] = 'N'
*  laTempStru[ALEN(laTempStru,1),3] = 13
*  laTempStru[ALEN(laTempStru,1),4] = 0
*ENDFOR

SELECT(lcPOTmpFil)
SCAN

  lcVarReq="NREQ"+PO
  lcVarIss="NISS"+PO
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
  laTempStru[ALEN(laTempStru,1),1] = lcVarReq
  laTempStru[ALEN(laTempStru,1),2] = 'N'
  laTempStru[ALEN(laTempStru,1),3] = 13
  laTempStru[ALEN(laTempStru,1),4] = 0

  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
  laTempStru[ALEN(laTempStru,1),1] = lcVarIss
  laTempStru[ALEN(laTempStru,1),2] = 'N'
  laTempStru[ALEN(laTempStru,1),3] = 13
  laTempStru[ALEN(laTempStru,1),4] = 0
  
ENDSCAN
*--606523,1 mhm

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NBALANCE'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NONORDER'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NAVALABL'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

CREATE DBF (gcWorkDir+lcTmpDtl)FROM ARRAY laTempStru
INDEX ON  FabrColr TAG lcTmpDtl 

****************************************************************************
*! PROG      : lfUpdatQty.PRG   (C#102584)
*! DESC      : Function to Update qty in new tempfile for detail option
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfUpdatQty
PRIVATE lcFbric,lcColor
PRIVATE lnTotStok,lnTotBalan,lnTotOnOrd,lnTotAval
STORE '' TO lcFbric,lcColor
STORE 0 TO lnTotStok,lnTotBalan,lnTotOnOrd,lnTotAval

SELECT(lcMatReq)
SET ORDER TO TAG Matreq
SCAN
  IF (lcFbric <> Item) OR (lcColor<>iclr)    && this is a new fabric/color
    lcFbric = Item
    lcColor = iclr
    lnCount = 1
    lcCount = ALLTRIM(STR(lnCount,5))
    lcPo = '      '
    lnTotReq&lcCount = 0
    lnTotIss&lcCount = 0
    SELECT(lcTmpDtl)
    APPEND BLANK
    *B606966,1 TMI [Start] nStock and nOnOrder will be updated incrementally in the loop below
    *REPLACE FabrColr  WITH ALLTRIM(&lcMatReq..ITEM)+'  '+ALLTRIM(&lcMatReq..ICLR),;
            NSTOCK    WITH &lcMatReq..STKTOTQY,;
            NONORDER  WITH &lcMatReq..ONORDQTY
    REPLACE FabrColr  WITH ALLTRIM(&lcMatReq..ITEM)+'  '+ALLTRIM(&lcMatReq..ICLR)           
    *B606966,1 TMI [End  ] 
    
    lnTotStok = lnTotStok +&lcTmpDtl..NSTOCK
    lnTotOnOrd = lnTotOnOrd + &lcTmpDtl..NONORDER
    
    lnCount = 0
    SELECT(lcMatReq)
    SCAN REST WHILE (lcFbric = Item) AND (lcColor=iclr)
      IF lcPo <> PO
        lnCount = lnCount  +1
        lcCount = ALLTRIM(STR(lnCount,5))

        *--606523,1 no need to those variable we add them in PO table
        *lnTotReq&lcCount =0
        *lnTotIss&lcCount = 0
        IF !SEEK(PO,lcPoTmpFil)
          LOOP
        ENDIF
        *--606523,1 no need to those variable we add them in PO table
        lcPo = PO
      ENDIF
      IF lnCountPO <> 0
        *--606523,1 mhm                 
        *REPLACE &lcTmpDtl..NREQ&lcCount   WITH &lcMatReq..REQTOTQT + &lcTmpDtl..NREQ&lcCount ,;
                &lcTmpDtl..NISSU&lcCount  WITH &lcMatReq..ISSTOTQT +&lcTmpDtl..NISSU&lcCount ,;
                &lcTmpDtl..NBALANCE       WITH &lcTmpDtl..NBALANCE +&lcTmpDtl..NSTOCK + &lcMatReq..REQTOTQT + &lcMatReq..ISSTOTQT,;
                &lcTmpDtl..NAVALABL       WITH &lcTmpDtl..NAVALABL+EVAL(lcMatReq+'.ONORDQTY')+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT') 
        *lnTotReq&lcCount  = lnTotReq&lcCount  + &lcTmpDtl..NREQ&lcCount
        *lnTotIss&lcCount = lnTotIss&lcCount + &lcTmpDtl..NISSU&lcCount

		*--mhmher
        *REPLACE &lcTmpDtl..NREQ&lcPO   WITH &lcMatReq..REQTOTQT + &lcTmpDtl..NREQ&lcPo ,;
                 &lcTmpDtl..NISS&lcPO   WITH &lcMatReq..ISSTOTQT +&lcTmpDtl..NISS&lcPo ,;
                 &lcTmpDtl..NBALANCE    WITH &lcTmpDtl..NBALANCE +&lcTmpDtl..NSTOCK + &lcMatReq..REQTOTQT + &lcMatReq..ISSTOTQT,;
                 &lcTmpDtl..NAVALABL    WITH &lcTmpDtl..NAVALABL+EVAL(lcMatReq+'.ONORDQTY')+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT') 

        *REPLACE &lcTmpDtl..NBALANCE       WITH &lcTmpDtl..NBALANCE +&lcTmpDtl..NSTOCK + &lcMatReq..REQTOTQT + &lcMatReq..ISSTOTQT,;
                &lcTmpDtl..NAVALABL       WITH &lcTmpDtl..NAVALABL+EVAL(lcMatReq+'.ONORDQTY')+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT') 
                
        REPLACE &lcTmpDtl..NBALANCE WITH &lcTmpDtl..NBALANCE+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT'),;
                &lcTmpDtl..NAVALABL WITH &lcTmpDtl..NAVALABL+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT')+EVAL(lcMatReq+'.ONORDQTY') 

        *B606966,1 TMI [Start] update nStock and nOnOrder
        REPLACE &lcTmpDtl..nStock   WITH &lcTmpDtl..nStock   + &lcMatReq..STKTOTQY,;
                &lcTmpDtl..NONORDER WITH &lcTmpDtl..NONORDER + &lcMatReq..ONORDQTY
        *B606966,1 TMI [End  ]
        
        IF !EMPTY(ALLTRIM(lcPO))        
          REPLACE &lcTmpDtl..NREQ&lcPO   WITH &lcMatReq..REQTOTQT + &lcTmpDtl..NREQ&lcPo ,;
                  &lcTmpDtl..NISS&lcPO   WITH &lcMatReq..ISSTOTQT +&lcTmpDtl..NISS&lcPo 

          REPLACE &lcPoTmpFil..TotReq  WITH &lcPoTmpFil..TotReq  + &lcMatReq..REQTOTQT
          REPLACE &lcPoTmpFil..TotIssu WITH &lcPoTmpFil..TotIssu  + &lcMatReq..ISSTOTQT
        ENDIF  
        *--606523,1 mhm                 
        
      ENDIF  
    ENDSCAN     
    lnTotBalan = lnTotBalan + &lcTmpDtl..NBALANCE
    lnTotAval  = lnTotAval  + &lcTmpDtl..NAVALABL
    SKIP-1     
  ENDIF  
ENDSCAN
SELECT (lcTmpDtl)
APPEND BLANK
REPLACE FabrColr  WITH "^^TOTAL :^^",;
        NSTOCK    WITH lnTotStok,;
        NONORDER  WITH lnTotOnOrd ,;
        NBALANCE  WITH lnTotBalan,;
        NAVALABL  WITH lnTotAval  
*--606523,1
*FOR lnCount = 1 TO lnCountPO
*  lcCount = ALLTRIM(STR(lnCount,5))
*  REPLACE NREQ&lcCount   WITH lnTotReq&lcCount+NREQ&lcCount ,;
*          NISSU&lcCount  WITH lnTotIss&lcCount+NISSU&lcCount
*ENDFOR          
*-- get totals from PoTmpfile
SELECT(lcPoTmpFil)        
lnCount = 0
SCAN
  lcPo = &lcPoTmpFil..PO
  lnCount = 1 +lnCount
  lcCount = ALLTRIM(STR(lnCount,5))
  
  IF !EMPTY(ALLTRIM(lcPO))        
    REPLACE &lcTmpDtl..NREQ&lcPo   WITH &lcPoTmpFil..TotReq+&lcTmpDtl..NREQ&lcPo ,;
            &lcTmpDtl..NISS&lcPo   WITH &lcPoTmpFil..TotIssu+&lcTmpDtl..NISS&lcPo
  ENDIF
ENDSCAN
*--606523,1

****************************************************************************
*! PROG      : lfPrint.PRG   (C#102584)
*! DESC      : Function to print to Excel sheet in detail option
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfPrint

lcRpDtDir  = gcDataDir  && Save The Data Directory.
lcRpWrDir  = gcWorkDir  && Save The Work Directory.
lcRpSysDir = gcSysHome  && Save The system  Directory.
lcRpRpDir  = gcRepHome  && Save The report Directory.
lcRpComp   = gcAct_Comp && Save The Active Company.
lcRpDefPth = gcDef_Path && Save The DefPath.
lcRpTmpFl1 = lcTmpDtl   && Save The TempName for the first  printed file.
lcRpTmpFl2 = lcPoTmpFil && Save The TempName for the second printed file.

lcRpWareHs = lcWareHos  && Save The warehouse 
lcRpTitle  = lcRpOptTtl && Printed Custom Titel

*--count Po's
lcRpCounPo   = lnCountPO
IF llOGFltCh
  SELECT (lcPoTmpFil)
  USE
  SELECT (lcTmpDtl)
  USE
ENDIF
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') ALL LIKE l?RP* 

lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')

lcLib=SYS(2004)+"foxtools.fll"

IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'MA\'+[MAMATVFP.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF

RETURN

****************************************************************************
*! PROG      : lfUpdStk   (C#102584)
*! DESC      : Function to Update StockQty And OnOrder Qty to 0 becuase it is not repeated
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfUpdStk
PRIVATE lcFbric,lcColor
STORE '' TO lcFbric,lcColor

SELECT(lcMatReq)
SET ORDER TO TAG Matreq
SCAN
  IF (lcFbric <> Item) OR (lcColor<>iclr)
    lcFbric = Item
    lcColor = iclr
    lcPo = PO
    SKIP    
    SCAN REST WHILE (lcFbric = Item) AND (lcColor=iclr)
      REPLACE &lcMatReq..STKTOTQY  WITH 0 ,;
              &lcMatReq..ONORDQTY  WITH 0
      *--       
      IF lcPo = PO
        REPLACE &lcMatReq..ISSTOTQT  WITH 0 
      ELSE
        lcPo = PO
      ENDIF
    ENDSCAN     
    SKIP-1     
  ENDIF  
ENDSCAN

*!**************************************************************************
*! Name      : lfStDepart
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/09/2002
*! Purpose   : Go top in the icdepthd file when browse.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfStDepart()
*!**************************************************************************
*606523,1
FUNCTION lfStDepart
PARAMETERS OpGrdParm
*B606966,1 TMI [Start] save the old alias
PRIVATE lnSlct
lnSlct = SELECT()
*B606966,1 TMI [End  ] 

SELECT ICDEPTHD   

DO CASE
  CASE  OpGrdParm='S'           
    lcTmpIndex =gfTempName()
    SELECT  DISTINCT DEPT,cDeptDesc FROM ICDEPTHD INTO DBF (gcWorkDir + lcTmpIndex)
     USE IN ICDEPTHD
    use in (lcTmpIndex)
    USE (gcWorkDir + lcTmpIndex) IN 0 ALIAS ICDEPTHD
    SELECT ICDEPTHD
    INDEX ON DEPT TAG Depthd
    GO TOP
  CASE  OpGrdParm='R'
    USE IN ICDEPTHD
    =gfOpenFile(gcDataDir+'ICDEPTHD',gcDataDir+'DEPTHD ','SH')
ENDCASE

*B606966,1 TMI [Start] restore the old alias
SELECT (lnSlct)
*B606966,1 TMI [End  ] 
*-- End of lfStDepart.

****************************************************************************
*! PROG      : lfDatBySty.PRG   (C#102584)
*! DESC      : function To scan for the selected Styles
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/08/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*-- 
FUNCTION lfDatBySty

SELECT (lcStyFile)
GOTO TOP
SCAN
  *B606966,4 TMI [Start] Note to use "lnMajorLn" in the seek expr in style file
  *=SEEK(ALLTRIM(cStyMajor),'Style')
  =SEEK(PADR(ALLTRIM(cStyMajor),lnMajorLn),'Style')
  *B606966,4 TMI [End  ] 

  SELECT STYLE
  *B606966,4 TMI [Start] Note to use "lnMajorLn" in the SCAN REST expr in style file
  *SCAN REST WHILE STYLE = ALLTRIM(&lcStyFile..cStyMajor)
  SCAN REST WHILE STYLE = PADR(ALLTRIM(&lcStyFile..cStyMajor),lnMajorLn)
    *B606966,4 TMI [End  ] 
  
    *B606966,4 TMI [Start] Note to use "lnMajorLn" in the SEEK expr in POSLN file
    *IF !SEEK(ALLTRIM(cStyMajor),'POSLN')
    IF !SEEK(PADR(ALLTRIM(cStyMajor),lnMajorLn),'POSLN')
      *B606966,4 TMI [End  ]     
      LOOP
    ENDIF
    IF llSelVend AND !SEEK(PosHdr.Vendor,lcVendFile)
      LOOP
    ENDIF  
    *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
    *IF !EVAL(lcRpExp0) AND !EVAL(lcRpExp1) 
    *-- IF !EVAL(lcRpExp0) AND !EVAL(lcExp1) *B606966,4 TMI 
    IF !EVAL(lcRpExp0) OR !EVAL(lcExp1) 
      *B606966,1 TMI [End  ]
      LOOP
    ENDIF
    lnCnt      = IIF( SEEK("S"+PosLn.Scale,'SCALE'), SCALE.Cnt , 8 )
    lcCStyle = Style.Style
    SELECT POSLN
    =SEEK(STYLE.STYLE,'POSLN')
    SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd = STYLE.STYLE
      =SEEK('P'+POSLN.PO,'PosHdr')      
      *--606523,1 check for completed satus [Start]
      IF PosHdr.Status $ "CXS"
        LOOP
      ENDIF  
      IF POSLN.TranCd # "1"
        LOOP
      ENDIF
      *--606523,1 check for completed satus [End]
      IF PosHdr.Status = "H"
        *--Read an item code and an item color[lcItem,lcIClr].
        
        *--Read an item code and an item color[lcItem,lcIClr].
        IF SEEK(PADR(SUBSTR(Style,1,lnMajorLn),19)+'5','BOM')        
          SELECT BOM
          SCAN WHILE citmmajor+typ+citmmask+mfgcode+item+iclr =;
                     PADR(SUBSTR(POSLN.STYLE,1,lnMajorLn),19)+'5' ;
                 FOR LIKE(STRTRAN(cItmMask,'*','?'),POSLN.STYLE)
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF !( &lcRpExp1 ) 
            IF !( &lcExp1 ) 
              *B606966,1 TMI [End  ] 
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            *--MHM2000
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN  
        ENDIF  
      ELSE    
        IF SEEK("I"+'1'+PO+STR(PosLn.lineno,6)+'5'+PosLn.Style,'BOMLINE')        
          SELECT BOMLINE
          *--tell here
          SCAN REST WHILE cImTyp+cType+cTktNo+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                          "I"+'1'+POSLN.PO+STR(PosLn.lineno,6)+'5'+PosLn.Style
        
            *--Read an item code and an item color[lcItem,lcIClr].
            *B606966,4 TMI [Start] Note to padr the style to seek correct in BOM file
            *=SEEK(ALLTRIM(Style),'BOM')      
            =SEEK(PADR(SUBSTR(Style,1,lnMajorLn),19),'BOM')        
            *B606966,4 TMI [End  ] 
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF !SEEK(PADR(lcItem,7)+lcIClr,'FABRIC')
              LOOP
            ENDIF
            *--Check if maririal/color entered in grid is valid.
            *B606966,1 TMI [Start] use lcExp1 - it has "FABDYE.CWARECODE" removed
            *IF (BOMLINE.cCatgTyp = 'F' ) AND !( &lcRpExp1 ) 
            IF (BOMLINE.cCatgTyp = 'F' ) AND !( &lcExp1 ) 
              *B606966,1 TMI [End  ] 
              LOOP
            ENDIF
            *--Append or Update record in (lcMatReq) file. 
            *--MHM2000
            =SEEK('P'+POSLN.PO,'PosHdr')
            =lfUpdReq()
            *=lfGetQty()       &&*B606966,4 TMI
          ENDSCAN
        ENDIF      
      ENDIF
    ENDSCAN  
  ENDSCAN
ENDSCAN


*:**************************************************************************
*:* Name        : lfRmvWH                                         *B606966,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/18/2003
*:* Purpose     : Remove WH codes from lcRpExp1
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRmvWH()
*:***************************************************************************
FUNCTION lfRmvWH
PARAMETERS lcExpr
PRIVATE lnFrm,lnLen
IF "BETWEEN(FABDYE.CWARECODE" $ lcExpr
  lnFrm = AT( "BETWEEN(FABDYE.CWARECODE" , lcExpr )
  lnLen = AT( ")" , SUBSTR(lcExpr,lnFrm) )
  lcExpr = STUFF( lcExpr , lnFrm , lnLen , '.T.' )
ENDIF

IF "SEEK(FABDYE.CWARECODE" $ lcExpr
  lnFrm = AT( "SEEK(FABDYE.CWARECODE" , lcExpr )
  lnLen = AT( ")" , SUBSTR(lcExpr,lnFrm) )
  lcExpr = STUFF( lcExpr , lnFrm , lnLen , '.T.' )
ENDIF

IF "INLIST(FABDYE.CWARECODE" $ lcExpr
  lnFrm = AT( "INLIST(FABDYE.CWARECODE" , lcExpr )
  lnLen = AT( ")" , SUBSTR(lcExpr,lnFrm) )
  lcExpr = STUFF( lcExpr , lnFrm , lnLen , '.T.' )
ENDIF

RETURN lcExpr
*-- end of lfRmvWH.