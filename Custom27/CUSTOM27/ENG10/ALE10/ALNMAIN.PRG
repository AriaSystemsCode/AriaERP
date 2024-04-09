*:****************************************************************************************
*: Program file  : ALNMAIN.PRG
*: Program desc. : Main Program.
*: System        : Aria Apparel System (A27).
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200149,1
*: Date          : 02/20/2001
*: Purpose       : ENG Custom Process Program .
*:****************************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:****************************************************************************************
*: Returns   : Logical value.       
*:****************************************************************************************
*C#200155,1
*:****************************************************************************************
*:Modifications :
*C200311,1 TMI 04/02/2002 Update ordline.item_no lcOrdLine.item_no/style.cvensty
*B606316,1 KHM 07/31/2002 Fix the problem of numeric overflow
*B606670,1 AMH 12/25/2002 Fix the bug of brow fabrics by pattern
*B606887,1 AMH 02/24/2003 Fix the bug of add new same fabric.
*C200512,1 KHM 05/04/2003 To add credited quantities back onto sales order
*B607269,1 ABD 06/29/2003 From Custom PO line screen after entering the item and after 
*B607269,1 ABD            Entering the Qty press the left arrow from the keyboard or CTRL+Tab
*B607269,1 ABD            The line will be repeated.
*C037892,1 MHM 06/15/2004 Cusom sales order cost sheet Validation
*C037959,1 MHM 09/20/2004 Custom Amend to Issue screen for multi primary fabric
*C037957,1 MHM 10/10/2004 Custom Amend CutPick File
*C123397,1 MHM 02/01/2005 Custom Amend To let user modify Custom screen 200200 in view mode
*C124836,1 MHM 12/15/2004 Custom Auto create material Po
*C123397,1 MHM 02/01/2005 Custom Amend To let user modify Custom screen 200200 in view mode
*B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
*B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
*B129452,1 MMR 10/11/2005 Increase number of lines in ap check to 27 lines instate of 20 lines.
*B128216,2 MMR 11/30/2005 1)Fix bug of showing the REcieve Qty by UOM buy
*B128216,2 MMR            2)Fix bug of showing the unallocated qty with wrong values in case of
*B128216,2 MMR              Over recieve. 
*B128216,2 MMR            3)Fix bug of not showing  PO that contains over recieve qty
*B128216,2 MMR              If we have only this po for this material
*B128216,2 MMR            4)Fix bug of not showing POs that Partially recieved
*:****************************************************************************************
*
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'
*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.
*:**************************************************************************
*: Name          : lfDLEALOSO
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200149,1
*: Date          : 02/20/2001
*: Purpose       : Dele all so on the PO.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfDLEALOSO()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200149,1.
*:**************************************************************************
*
FUNCTION lfDLEALOSO
PRIVATE lnAlias
lnAlias = 0

lnAlias = SELECT (0)
= gfOpenFile(gcDataDir+'MAPOALO',gcDataDir+'MAPOSO','SH')

*-- Text  Message  : There is one or more material
*-- Text  Message  : purchase order lines allocated to this sales order. 
*-- Text  Message  : Are you sure you want to cancel this order?'
*-- Message Number : Custom Message
*-- Text Button    : \<Yes;\?\<No
*-- Button  Number : 32000


*-- This flag to cheak if call from mas order cancellation program.
llMasOrder = IIF(TYPE('llMasOrder') = 'U',.F.,llMasOrder)

IF SEEK(OrdHdr.Order,'MAPOALO') 
  IF llMasOrder
    SELECT MAPOALO
    DELETE ALL FOR ORDER = OrdHdr.Order
  ELSE
    IF gfModalGen('TRM00000B32000','ALERT','','','There is one or more material;
    purchase order lines allocated to this sales order. Are you sure you want to cancel this order?') = 1
      *--mhm2004
      =lfSODELALO()
      *--mhm2004
      SELECT MAPOALO
      DELETE ALL FOR ORDER = OrdHdr.Order
    ELSE
      SELECT (lnAlias)
      RETURN .F.  
    ENDIF
  ENDIF
ENDIF

=gfCloseFile('MAPOALO')
SELECT (lnAlias)

*-- End OF lfDLEALOSO
*:**************************************************************************
*: Name          : lfDLEALOMA
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200149,1
*: Date          : 02/20/2001
*: Purpose       : Dele all so on the PO.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Parameter     : lcParam  --- > L For delete line
*:               : lcParam  --- > A For delete All lines
*:**************************************************************************
*: Example       : = lfDLEALOSO()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200149,1.
*:**************************************************************************
*
FUNCTION lfDLEALOMA
PRIVATE lnAlias
lnAlias = 0

lnAlias = SELECT (0)
= gfOpenFile(gcDataDir+'MAPOALO',gcDataDir+'MAPOALO','SH')


*-- Delete all lines for dele all the PO.
IF lcParam = 'A'
  *-- Text  Message  : This material purchase order includes one or more lines 
  *-- Text  Message  : allocated to a sales order(s). Are you sure you want to 
  *-- Text  Message  : cancel this material purchase order?
  *-- Message Number : Custom Message
  *-- Text Button    : \<Yes;\?\<No
  *-- Button  Number : 36001

  IF SEEK(laData[1],'MAPOALO') 
    IF gfModalGen('TRM00000B32000','ALERT','','','This material purchase order includes one or more lines allocated to a sales order(s). Are you sure you want to;
                                             cancel this material purchase order?') = 1
      SELECT MAPOALO
      DELETE ALL FOR POMAT = laData[1]
    ELSE
      SELECT (lnAlias)
      RETURN .F.  
    ENDIF
  ENDIF
ELSE
  *-- Delete Selected line only
  *-- Text  Message  : This line is allocated to one or more 
  *-- Text  Message  : sales orders. Are you sure you want to remove it?   
  *-- Message Number : Custom Message
  *-- Text Button    : \<Yes;\?\<No
  *-- Button  Number : 44009

  IF SEEK(laData[1],'MAPOALO') 
    IF gfModalGen('QRM00000B32000','ALERT','','','This line is allocated to one or more sales orders. Are you sure you want to remove it?') = 1
      SELECT MAPOALO
      DELETE ALL FOR POMAT+FABRIC+COLOR  = laData[1]+lcFabric+lcColor
    ELSE
      SELECT (lnAlias)
      RETURN .F.  
    ENDIF
  ENDIF

ENDIF


*--no need to clos this file

*=gfCloseFile('MAPOALO')
SELECT (lnAlias)

*-- End OF lfDLEALOMA
*:**************************************************************************
*: Name          : lfADDOPTN
*: Developer     : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date          : 06/17/2001
*: Purpose       : Dele all so on the PO.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfADDOPTN()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200200,1.
*:**************************************************************************
FUNCTION lfADDOPTN

*C200200,6 mhm Enable view mode (AS per WAB ) [Start]
*DEFINE BAR 6 OF _OPTIONPOP PROMPT 'Material PO Detail Screen'  SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnactfolder<>2))
DEFINE BAR 6 OF _OPTIONPOP PROMPT 'Material PO Detail Screen'  SKIP FOR (laScrMode[1] OR (lnactfolder<>2))
*C200200,6 mhm [End]

ON SELECTION BAR 6 OF _OPTIONPOP DO lpAddScr IN ALNMAIN

*!**************************************************************************
*! Name      : lpAddScr            (C200200)
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! Purpose   : Display Screen to Add details .
*!*************************************************************
*! Example   : DO lpAddScr
*!*************************************************************
*!
PROCEDURE lpAddScr

*lcEscInt = ON('KEY','ESC')
lcTabInt = ON('KEY','TAB')
*lcHldBtb = ON('KEY','BACKTAB')    
*lcAltB   = ON('KEY','ALT+B')    

*-- Declaration variables.
lcDetTtl = "Detail P/O Lines"
STORE 0 TO lnlPrice ,lnlQty, lnOrdQty , lnlOldQty ,lnlOldOrd,lnlOldCst2,lnlOldCst3,lnlOldCst4,lnoldPrice 
*--c 200200,5
*STORE "" TO MAPOALEN ,lclPer,lclDesc,maPoal4,maPoal3
STORE "" TO MAPOALEN ,lclPer,lclDesc,maPoal3 ,lmNotes ,lclOldFab ,lclOldClr
llOrdbrw = .F.
llbrBAK  = .F.
*--c 200200,5
lcOrdType = "O"
lcMark    = '>'
lcUnMark   = SPACE(1)
*-- Temp File.
=lfGetlines()

ON KEY LABEL ALT+G ACTIVATE WINDOW (lcDetTtl)
ON KEY LABEL TAB     DO lplTab
STORE .F. TO llInsert 
SELECT (lclDetTmp)

LOCATE
PUSH KEY

DO (gcScrDir+"MA\MAPOALEN.SPX")
SELECT (lcDetTmp)
GOTO TOP
IF EOF(lcDetTmp)
  SHOW GET pbRemove DISABLE
  lcFabric  = SPACE(07) 
  lcColor   = SPACE(06) 
  lcPattern = SPACE(10) 
  *lcWidth   = SPACE(06)
  lcClrDesc = SPACE(15)
  lcIDesc   = SPACE(20)
  lnCost1   = 0 
  lnCost2   = 0 
  lnCost3   = 0 
  lnCost4   = 0       
  lnQty     = 0
  lnTotQty  = 0
  lnLineNo  = 0
ENDIF

=lfwLine()
SHOW WINDOW (lcDet_Ttl) REFRESH

ON KEY LABEL ALT+G
POP KEY

*ON KEY LABEL ESC      &lcEscInt
ON KEY LABEL TAB      &lcTabInt
*ON KEY LABEL BACKTAB  &lcHldBtb
*ON KEY LABEL ALT+B    &lcAltB



*:*************************************************************
*: Name          : lfBrowLine
*: Developer     : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date          : 06/17/2001
*: Purpose       : browse lines for the selected P/O
*:*************************************************************
*: Calls         : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example       : =lfBrowLine()
*:*************************************************************
*
FUNCTION lfBrowLine

lnAlias = SELECT()
SELECT (lclDetTmp)
lnRecNo = RECNO(lclDetTmp)
lcBrowFlds = [lcMarker=IIF(RECNO() = lnRecNo ,'>',' '):1:H=' ':W=.F.,]+;
             [Fabric      :R:H='Item'   :10,]+;
             [Pattern     :R:H='Pattern':10,]+;
             [Color       :R:H='Color'  :7,]+;
             [cDesc       :R:H='Desc'   :25,]+;
             [nCost1      :R:H='Price'  :9,]+;
             [cPer        :R:H='Per'    :5,]+;
             [nFAbTotQty  :R:H='Qty'    :11,]+;
             [Order       :R:H='House No':12,]+;
             [nHousQty    :R:H='H. No Qty' :13]
             
BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       VALID :F lfvBrows();       
       WHEN lfwBrowUp();
       TITLE lcDetTtl ;
       WINDOW maPoal2 IN WINDOW Mapoalen

_CUROBJ = OBJNUM(PbInsrt)
*C200200,6 mhm disable all object in view mode (AS per WAB ) [Start]
IF laScrMode[2]
  SHOW GET lclFabrc   DISABLE 
  SHOW GET lclPattrn  DISABLE 
  SHOW GET lclColor   DISABLE 
  SHOW GET lclDesc    DISABLE 
  SHOW GET lclPer     DISABLE
  SHOW GET lclorder   DISABLE 
  SHOW GET lnlPrice   DISABLE 
  SHOW GET lnlQty     DISABLE 
  SHOW GET lnOrdQty   DISABLE 
  SHOW GET PblRemov   DISABLE 
  SHOW GET PbZoom     DISABLE 
  SHOW GET iblPatt    DISABLE 
  SHOW GET iblColor   DISABLE 
  SHOW GET iblOrder   DISABLE 
  SHOW GET iblFabrc   DISABLE 
  SHOW GET PbInsrt    DISABLE 
  SHOW GET PblRemov   DISABLE 
ENDIF
*--123397,1 mhm let user modify Custom screen 200200 in view mode [Start]
IF laScrMode[2] AND !(POFHDR.Status $'X')
  SHOW GET lclFabrc   ENABLE
  SHOW GET lclPattrn  ENABLE
  SHOW GET lclColor   ENABLE
  SHOW GET lclDesc    ENABLE
  SHOW GET lclPer     ENABLE
  SHOW GET lclorder   ENABLE
  SHOW GET lnlPrice   ENABLE
  SHOW GET lnlQty     ENABLE
  SHOW GET lnOrdQty   ENABLE
  SHOW GET PblRemov   ENABLE
  SHOW GET PbZoom     ENABLE
  SHOW GET iblPatt    ENABLE
  SHOW GET iblColor   ENABLE
  SHOW GET iblOrder   ENABLE
  SHOW GET iblFabrc   ENABLE
  SHOW GET PbInsrt    ENABLE
  SHOW GET PblRemov   ENABLE
  SHOW GET PbSave     ENABLE  
ELSE
    SHOW GET PbSave     DISABLE  
ENDIF
*--123397,1 mhm [End]

*C200200,6 mhm  [End]

SELECT(lnAlias)
*-- End Of lfBrowLine.
*:*************************************************************
*: Name        : lfCrTmpFls
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*: Purpose     : Create the temp file.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example     : =lfCrTmpFls()
*:*************************************************************
*
FUNCTION lfCrTmpFls
PRIVATE lnAlias
lnAlias = 0 

lnAlias = SELECT(0)

lclDetTmp   = gfTempName()

= gfOpenFile(gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
= gfOpenFile(gcDataDir+'MAPOALO',gcDataDir+'Mapoalo','SH')
*SELECT MAPOALO

SELECT POFLN
=AFIELDS(laMlnTmp)
lnNewFld = ALEN(laMlnTmp,1)

DIMENSION laMlnTmp[lnNewFld+11,4]

laMlnTmp[lnNewFld + 1,1] = 'ORDER'
laMlnTmp[lnNewFld + 1,2] = 'C'
laMlnTmp[lnNewFld + 1,3] = 6
laMlnTmp[lnNewFld + 1,4] = 0

laMlnTmp[lnNewFld + 2,1] = 'nHousQty'
laMlnTmp[lnNewFld + 2,2] = 'N'
laMlnTmp[lnNewFld + 2,3] = 11
laMlnTmp[lnNewFld + 2,4] = 2

laMlnTmp[lnNewFld + 3,1] = 'mlNotes'
laMlnTmp[lnNewFld + 3,2] = 'M'
laMlnTmp[lnNewFld + 3,3] = 0
laMlnTmp[lnNewFld + 3,4] = 0

laMlnTmp[lnNewFld + 4,1] = 'cDesc'
laMlnTmp[lnNewFld + 4,2] = 'C'
laMlnTmp[lnNewFld + 4,3] = 20
laMlnTmp[lnNewFld + 4,4] = 0

laMlnTmp[lnNewFld + 5,1] = 'cPer'
laMlnTmp[lnNewFld + 5,2] = 'C'
laMlnTmp[lnNewFld + 5,3] = 3
laMlnTmp[lnNewFld + 5,4] = 0

laMlnTmp[lnNewFld + 6,1] = 'cMarker'
laMlnTmp[lnNewFld + 6,2] = 'C'
laMlnTmp[lnNewFld + 6,3] = 1
laMlnTmp[lnNewFld + 6,4] = 0

laMlnTmp[lnNewFld + 7,1] = 'lNew'
laMlnTmp[lnNewFld + 7,2] = 'L'
laMlnTmp[lnNewFld + 7,3] = 1
laMlnTmp[lnNewFld + 7,4] = 0

laMlnTmp[lnNewFld + 8,1] = 'nRecno'
laMlnTmp[lnNewFld + 8,2] = 'N'
laMlnTmp[lnNewFld + 8,3] = 10
laMlnTmp[lnNewFld + 8,4] = 0

laMlnTmp[lnNewFld + 9,1] = 'cStatus'
laMlnTmp[lnNewFld + 9,2] = 'C'
laMlnTmp[lnNewFld + 9,3] = 1
laMlnTmp[lnNewFld + 9,4] = 0

laMlnTmp[lnNewFld + 10,1] = 'lCheckqty'
laMlnTmp[lnNewFld + 10,2] = 'L'
laMlnTmp[lnNewFld + 10,3] = 1
laMlnTmp[lnNewFld + 10,4] = 0

laMlnTmp[lnNewFld + 11,1] = 'lCheckIns'
laMlnTmp[lnNewFld + 11,2] = 'L'
laMlnTmp[lnNewFld + 11,3] = 1
laMlnTmp[lnNewFld + 11,4] = 0

CREATE DBF (gcWorkDir+lclDetTmp) FROM ARRAY laMlnTmp
=gfOpenFile(gcWorkDir+lclDetTmp,'','EX')
INDEX ON PoMat+Fabric+Color+Order TAG lclDetTmp DESC

SELECT(lnAlias)
*-- End Of lfCrTmpFls

*:*************************************************************
*: Name      : lfvBrows
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*: Purpose   : Valid Browse function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfvBrows()
*:*************************************************************
*
FUNCTION lfvBrows
*C200311,1 TMI [START]
IF TYPE('lcDetTtl') = 'C'
*C200311,1 TMI [END  ]
  IF WONTOP() # (lcDetTtl)
    = gfStopBrow()
  ENDIF
*C200311,1 TMI [START]
ENDIF
*C200311,1 TMI [END  ]
*-- End OF lfvBrows.
*!*************************************************************
*! Name        : lfvlFabrc
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the fabric field For Alena
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvlFabrc
PRIVATE lnAlias


IF MDOWN()
  RETURN
ENDIF

lnAlias = SELECT()
SELECT Fabric

*B606670,1 AMH Brow for only fabrics with pattern if not empty [Start]
*SET FILTER TO CFABGRADE = lcQuality
IF EMPTY(lclPattrn)
  SET FILTER TO CFABGRADE = lcQuality
ELSE
  SET FILTER TO CFABGRADE = lcQuality .AND. PATTERN = ALLTRIM(lclPattrn)
ENDIF
*B606670,1 AMH [End]

IF !EMPTY(lclFabrc) AND !SEEK(lclFabrc)
  llBrowse = .F.
  
  *B606670,1 AMH Brow for only fabrics with pattern if not empty [Start]
  *DO FaBrow WITH lclFabrc,'*'
  IF EMPTY(lclPattrn)
    DO FaBrow WITH lclFabrc,'*'
    IF lclfabrc <> lclOldFab
      REPLACE &lclDetTmp..COLOR WITH ''
    ENDIF
  ELSE
    PRIVATE laBrowArr
    DECLARE laBrowArr[7]
    STORE '' TO laBrowArr
    lcOldBros = lcBrFields
    lcBrFields = [Pattern:10,Fabric:10:h="Item",Color:8:h="Color",Desc:h="Description":19,]+;
                 [lcItmType = gfCodDes(item_Type,'ITEM_TYPE') :h="Type",loc:8:h="Location",]+;
                 [Vendor:11,Onhand:8:h="On Hand",OnOrder:8:h="On Order",Content:60:h="Contents"]
    =ARIABROW("","Fabric",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',;
              'FABRIC,COLOR,PATTERN,CPRICECUR,NFABCOST,DESC,UOMBUY','laBrowArr')
    lclFabrc  = laBrowArr[1]
    IF !EMPTY(laBrowArr[1])
      *--123397,2
      lnCurRec = RECNO(lclDetTmp)
      IF laScrMode[2] AND !SEEK(POFHDR.POMAT+laBrowArr[1]+laBrowArr[2],lclDetTmp) AND POFHDR.status $'CS'
        lcMsg2 = 'Fabric not found in material PO, Can not add.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        RETURN
      ENDIF
      GOTO lnCurRec IN (lclDetTmp)
      *--123397,2
      lclColor  = laBrowArr[2]
      lclPattrn = laBrowArr[3]
      lnlPrice  = IIF(laBrowArr[4] = laData[23] ,laBrowArr[5],0)
      lclDesc   = laBrowArr[6]
      lclPer    = laBrowArr[7]
      REPLACE  (lclDetTmp+'.Fabric')  WITH lclFabrc,;
               (lclDetTmp+'.Pattern') WITH lclPattrn,;
               (lclDetTmp+'.Color')   WITH lclColor,;
               (lclDetTmp+'.cDesc')   WITH lclDesc,;
               (lclDetTmp+'.cPer')    WITH lclPer,;
               (lclDetTmp+'.nCost1')  WITH lnlPrice
    ENDIF
    lcBrFields = lcOldBros
  ENDIF
  *B606670,1 AMH [End]
  
ENDIF
SET FILTER TO
SHOW GET lclFabrc   ENABLE
SHOW GET lclPattrn  ENABLE
SHOW GET iblPatt    ENABLE 
SHOW GET iblColor   ENABLE 
SHOW GET iblOrder   ENABLE 
*--123397,2
lnCurRec = RECNO(lclDetTmp)
IF laScrMode[2] AND !SEEK(POFHDR.POMAT+lclFabrc,lclDetTmp) AND POFHDR.status $'CS'
  lcMsg2 = 'Fabric ' +lclFabrc + ' not found in material PO, Can not add.'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  RETURN
ENDIF
GOTO lnCurRec IN (lclDetTmp)
*--123397,2
*B606670,1 AMH Not need to update the pattern [Start]
*IF EMPTY(lclFabrc)
*  lclPattrn = ""
*ELSE
*  lclPattrn = Fabric.pattern
*ENDIF
*IF lclfabrc <> lclOldFab
*  REPLACE &lclDetTmp..COLOR WITH ''
*ENDIF
*B606670,1 AMH [End]
REPLACE &lclDetTmp..FABRIC  WITH lclFabrc,;
        &lclDetTmp..pattern WITH lclPattrn 

SELECT(lnAlias)
=lfwBrowUp()
lclOldFab = lclfabrc 
SHOW GET lclorder   DISABLE 
SHOW GET lnlPrice   DISABLE 
SHOW GET lnlQty     DISABLE 
SHOW GET lnOrdQty   DISABLE 
*--c 200200,5
*SHOW GET lclWidth   DISABLE 
*--c 200200,5
SHOW GET PbZoom     DISABLE 
*--mhm200200,5
*SHOW GET lmNotes    DISABLE 
*--mhm200200,5
SHOW GET iblFabrc   ENABLE 
*SHOW GET iblOrder   DISABLE 
**--to disable browse
llbrBAK =.F.
*!*************************************************************
*! Name        : lfwFabric
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old fabric field in a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfwlFabrc

lclOldFab = lclFabrc

*!*************************************************************
*! Name        : lfvlColor
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! Purpose     : validate the color field
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvlColor
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT()
SELECT Fabric
SET ORDER TO TAG FABRIC
*--mhm 200200,4
*IF !EMPTY(lclColor) AND !SEEK(lclFabrc+lclColor)
*--mhm 200200,4
IF  !SEEK(lclFabrc+lclColor)
  lclColor  = IIF(EMPTY(lclFabrc),PADR(lclColor,6),CHR(240))
  lcOldF   = lclFabrc
  SELECT FABRIC
  SET FILTER TO CFABGRADE = lcQuality
  DO FaBrow WITH lclFabrc,lclColor
  SET FILTER TO
  IF EMPTY(lclColor)
    lclFabrc = lcOldF
    _CUROBJ = OBJNUM(lclColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
ENDIF
*--mhm 200200,4 
IF EMPTY(lclColor)
  _CUROBJ = OBJNUM(lclColor)
  RETURN
ENDIF
*--mhm 200200,4 
*--123397,2
lnCurRec = RECNO(lclDetTmp)
IF laScrMode[2] AND !SEEK(POFHDR.POMAT+lclFabrc+lclColor,lclDetTmp) AND POFHDR.status $'CS'
  lcMsg2 = 'Fabric ' +lclFabrc + 'color '+lclColor + ' not found in material PO, Can not add.'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  RETURN
ENDIF
GOTO lnCurRec IN (lclDetTmp)
*--123397,2
=SEEK(lclFabrc+lclColor,'FABRIC')        
lclPattrn = FABRIC.PATTERN

lnlPrice  = IIF(FABRIC.CPRICECUR = laData[23] ,FABRIC.NFABCOST,0)
lclDesc   = FABRIC.DESC
lclPer    = FABRIC.UOMBUY

SELECT (lclDetTmp)
*--c 200200,5
*REPLACE  &lclDetTmp..Fabric    WITH lclFabrc,;
         &lclDetTmp..Pattern   WITH lclPattrn,; 
         &lclDetTmp..Color     WITH lclColor,; 
         &lclDetTmp..cDesc     WITH lclDesc,;
         &lclDetTmp..cPer      WITH lclPer,;
         &lclDetTmp..nCost1    WITH lnlPrice ,;
         &lclDetTmp..Width     WITH FABRIC.Width,;
         &lclDetTmp..nCost2    WITH FABRIC.nItm_frt,;
         &lclDetTmp..nCost3    WITH FABRIC.nItem_tax,;
         &lclDetTmp..nCost4    WITH FABRIC.nItemquota

*--mhm2000
IF laScrMode[3]

 IF lclColor <> lclOldClr
  IF SEEK(lcKeyType+laData[1]+lclFabrc+lclOldClr+'2','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Shipment') = 1

    lclColor = lclOldClr
    _CUROBJ = OBJNUM(lclColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
  IF SEEK(lcKeyType+laData[1]+lclFabrc+lclOldClr+'3','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Damaged') = 1

    lclColor = lclOldClr
    _CUROBJ = OBJNUM(lclColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
  IF SEEK(lcKeyType+laData[1]+lclFabrc+lclOldClr+'4','POFLN') .AND. gfModalGen('INM36011B36000','DIALOG','Cancelled ') = 1
    lclColor = lclOldClr
    _CUROBJ = OBJNUM(lclColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
 ENDIF
ENDIF        

*--mhm200200,5
IF !SEEK(lclFabrc+lclColor +ladata[3],'FABDYE')
  IF !lfFabInWar(lclFabrc, lclColor , laData[3])
    lclFabrc   = SPACE(07) 
    lclColor   = SPACE(06) 
    lclPattrn  = SPACE(10) 
    REPLACE &lclDetTmp..FABRIC  WITH lclFabrc,;
            &lclDetTmp..pattern WITH lclPattrn 
    =lfwBrowUp()
    _CUROBJ = OBJNUM(lclFabrc)
    RETURN
  ENDIF
ENDIF
*--mhm200200,5

*--c 200200,5 TO Validate color with order [start]
lnlcCheck = 0
lnCurRec = RECNo()
REPLACE &lclDetTmp..Color WITH lclColor
lcLcPom = Pomat
lclcFab = Fabric
lclccol = Color
lclcOrd = order
LOCATE

SCAN FOR pomat+fabric+color =  lcLcPom +lclcFab +lclccol 
  IF lclcOrd = Order
    lnlcCheck = lnlcCheck +1
  ENDIF  
ENDSCAN
IF lnlcCheck >1
  =gfModalGen('QRM00000B38018','F',' ',' ','This House No has been assigned to this color before ')   
  GOTO lnCurRec 
  lclColor = lclOldClr
  SHOW GET lclColor
  REPLACE &lclDetTmp..Color WITH lclOldClr
  _CUROBJ = OBJNUM(lclorder)
  RETURN
ENDIF
GOTO lnCurRec
*--c 200200,5 TO Validate color with order [End]

*--c 200200,5
REPLACE  &lclDetTmp..Fabric    WITH lclFabrc,;
         &lclDetTmp..Pattern   WITH lclPattrn,; 
         &lclDetTmp..Color     WITH lclColor,; 
         &lclDetTmp..cDesc     WITH lclDesc,;
         &lclDetTmp..cPer      WITH lclPer,;
         &lclDetTmp..nCost1    WITH lnlPrice 
=lfwBrowUp()
SELECT (lclDetTmp)
lnlcCheck  = 0
lcLcPom = pomat
lclcFab = fabric
lclccol = color
lnCurRec = RECNo()
SCAN 
  IF pomat+fabric+color =  lcLcPom +lclcFab +lclccol 
     lnlcCheck  = lnlcCheck+1
  ENDIF  
ENDSCAN

IF lnlcCheck > 1
  SHOW GET  lnlPrice   DISABLE
  
  *B606887,1 AMH Disable the quantity field also [Start]
  SHOW GET lnlQty DISABLE
  *B606887,1 AMH [End]
  
ELSE
  SHOW GET  lnlPrice   ENABLE 
ENDIF

GOTO lnCurRec 

SELECT(lnAlias)
*!*************************************************************
*! Name        : lfwlColor
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old color fieldin a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfwlColor

lclOldClr = lclColor


*!*************************************************************
*! Name        : lfwlPatt
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old color fieldin a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfwlPatt

lcOldPat = lclPattrn

*!*************************************************************
*! Name        : lfvlPatt
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old color fieldin a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvlPatt
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
*IF EMPTY(lclPattrn) AND !EMPTY(lclFabrc)
IF EMPTY(lclPattrn) 
  RETURN
ENDIF
lnAlias = SELECT()
SELECT Fabric
*--mhm200200,5
lcOldOrdr = ORDER()
*SET ORDER TO TAG FABRIC
SET ORDER TO TAG PFABRIC
*--mhm200200,5

*B606670,1 AMH Get the correct pattern [Start]
*IF !EMPTY(lclPattrn) AND !SEEK(ALLTRIM(lclPattrn))
IF (!EMPTY(lclPattrn) .AND. !SEEK(PADR(lclPattrn,10))) .OR. (llBrowse)
*B606670,1 AMH [End]

  *B606670,1 AMH We don't need to change the color since we don't use FaBrow [Start]
  *lclColor   = IIF(EMPTY(lclFabrc),PADR(lclColor,10),CHR(240))
  llBrowse = .F.
  *B606670,1 AMH [End]
  
  lcOldF   = lclFabrc
  SELECT FABRIC
  SET FILTER TO CFABGRADE = lcQuality AND !EMPTY(PATTERN)
  
  *B606670,1 AMH Don't use FaBrow since it change the order of fabric file [Start]
  *DO FaBrow WITH lclFabrc,'*'
  PRIVATE laBrowArr
  DECLARE laBrowArr[7]
  STORE '' TO laBrowArr
  lcOldBros = lcBrFields
  =SEEK(ALLTRIM(lclPattrn))
  lcBrFields = [Pattern:10,Fabric:10:h="Item",Color:8:h="Color",Desc:h="Description":19,]+;
               [lcItmType = gfCodDes(item_Type,'ITEM_TYPE') :h="Type",loc:8:h="Location",]+;
               [Vendor:11,Onhand:8:h="On Hand",OnOrder:8:h="On Order",Content:60:h="Contents"]
  =ARIABROW("","Fabric",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',;
            'FABRIC,COLOR,PATTERN,CPRICECUR,NFABCOST,DESC,UOMBUY','laBrowArr')
  lclPattrn = laBrowArr[3]
  IF !EMPTY(laBrowArr[3])
    lclFabrc  = laBrowArr[1]
    lclColor  = laBrowArr[2]
    lnlPrice  = IIF(laBrowArr[4] = laData[23] ,laBrowArr[5],0)
    lclDesc   = laBrowArr[6]
    lclPer    = laBrowArr[7]
    REPLACE  (lclDetTmp+'.Fabric')  WITH lclFabrc,;
             (lclDetTmp+'.Pattern') WITH lclPattrn,;
             (lclDetTmp+'.Color')   WITH lclColor,;
             (lclDetTmp+'.cDesc')   WITH lclDesc,;
             (lclDetTmp+'.cPer')    WITH lclPer,;
             (lclDetTmp+'.nCost1')  WITH lnlPrice
  ENDIF
  lcBrFields = lcOldBros
  *B606670,1 AMH [End]
  
  SET FILTER TO
  *IF EMPTY(lclColor)
  *  lclFabrc = lcOldF
  *  _CUROBJ = OBJNUM(lclPattrn)
  *  SELECT(lnAlias)
  *  RETURN
  *ENDIF
  
  *B606670,1 AMH Pattern get from ariabrow so we don't need to get it again [Start]
  *lclPattrn = FABRIC.PATTERN
  *B606670,1 AMH [End]
  
ENDIF


*B606670,1 AMH We don't need to update these variable sine it updated from browse [Start]
*=SEEK(ALLTRIM(lclPattrn),'FABRIC')        
*lclFabrc  = FABRIC.FABRIC
*lclPattrn = FABRIC.PATTERN
*lclColor  = FABRIC.COLOR
*lnlPrice  = IIF(FABRIC.CPRICECUR = laData[23] ,FABRIC.NFABCOST,0)
*lclDesc   = FABRIC.DESC
*lclPer    = FABRIC.UOMBUY
*
*IF !EMPTY(lclFabrc) AND EMPTY(lclPattrn) 
*  =gfModalGen('QRM00000B38018','F',' ',' ','This pattern is not found.')   
*  *WAIT WINDOW "No Pattern Found"
*ENDIF
*
*REPLACE &lclDetTmp..Fabric    WITH lclFabrc,;
*        &lclDetTmp..PATTERN WITH lclPattrn 
*B606670,1 AMH [End]

SET ORDER TO &lcOldOrdr        
SELECT(lnAlias)
=lfwBrowUp()

*!*************************************************************
*! Name      : lfvZoom
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! Purpose   : Validate Units.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvZoom()
*!*************************************************************
*
FUNCTION lfvZoom

lnAlias = SELECT()

*--123397,1 mhm let user modify Custom screen 200200 in view mode [Start]
PRIVATE llViewMod
llViewMod = .F.
IF laScrMode[2] AND !(POFHDR.Status $'X')
  llViewMod = .T.
  STORE .F. TO laScrMode
  laScrMode[3] = .T.
ENDIF
*--123397,1 mhm  [End]

=gfZoom('lmNotes')
*_CUROBJ = OBJNUM(lmNotes)
SELECT (lclDetTmp)
REPLACE mlNotes WITH lmNotes

*--123397,1 mhm let user modify Custom screen 200200 in view mode [Start]
IF llViewMod
  llViewMod = .F.
  STORE .F. TO laScrMode
  laScrMode[2] = .T.
ENDIF
*--123397,1 mhm [End]

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfWNotes
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! Purpose   : Validate Units.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNotes()
*!*************************************************************
*
FUNCTION lfWNotes
PRIVATE lnAlias

lnAlias = SELECT()

lmNotes = &lclDetTmp..mlNotes 
SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvClos
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! Purpose   : Validate Close.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClos()
*!*************************************************************
*
FUNCTION lfvClos
PRIVATE lcFab ,lcColour
lnAlias = SELECT(0)
STORE 0 TO lnLastLine
*--to initialize Estimated
STORE 0 TO ladata[39] ,ladata[27] ,ladata[51],ladata[55]

SELECT(lcDetTmp)
LOCATE
DELE ALL
LOCATE
SELECT (lclDetTmp)

*B606316,1 KHM 07/31/2002 (Begin) Delete all the records with empty fabric or color
DELETE ALL FOR EMPTY(Fabric) OR EMPTY(Color)
LOCATE
*B606316,1 KHM 07/31/2002 (End)

lcFab    = SPACE(07)
lcColour = SPACE(06)
SCAN 
  IF (&lclDetTmp..FABRIC <> lcFab) OR (&lclDetTmp..COLOR <> lcColour)
    lcFab    = &lclDetTmp..FABRIC
    lcColour = &lclDetTmp..COLOR
    SCATTER MEMVAR MEMO
    lnLastLine = lnLastLine + 1
    SELECT(lcDetTmp)
    APPEND BLANK
    =RLOCK()
    m.LineNo = lnLastLine 
    M.nFabTotQty = 0
    m.nCost1 = 0
    GATHER MEMVAR MEMO
    UNLOCK
    =SEEK(lcFabric+lcColor,'FABRIC')        
    lcPattern = &lclDetTmp..Pattern
    *lcWidth   = IIF(lNewLine ,FABRIC.WIDTH,&lcDetTmp..WIDTH)
    lcIDesc   = &lclDetTmp..cDesc
    lcClrDesc = gfCodDes(lcColor, 'COLOR')
    
    *B606316,1 KHM 07/31/2002 (Begin) Comment the following lines
    *IF lcKeyType = 'P' AND lNewLine AND SEEK(lcFabric+lcColor,lcContLin)
    *  IF &lcContLin..cPriceCur = laData[23]
    *    lnCost1 = &lcContLin..nCost1
    *  ENDIF
      *--c 200200,5 [start]
      *IF &lcContLin..cDutyCur = laData[25]
      *  lnCost2 = &lcContLin..nCost2
      *  lnCost3 = &lcContLin..nCost3
      *  lnCost4 = &lcContLin..nCost4
      *ENDIF
      *--c 200200,5 [end]

    *ELSE
    *  lnCost1 = IIF(lNewLine,IIF(laData[23] = FABRIC.cPriceCur,FABRIC.nFabCost,0),&lcDetTmp..nCost1)
      *--c 200200,5 [Start]
      *lnCost2 = IIF(lNewLine,IIF(laData[25] = FABRIC.cDutyCur,FABRIC.nItm_frt,0),&lcDetTmp..nCost2)
      *lnCost3 = IIF(lNewLine,IIF(laData[25] = FABRIC.cDutyCur,FABRIC.nItem_tax,0),&lcDetTmp..nCost3)
      *lnCost4 = IIF(lNewLine,IIF(laData[25] = FABRIC.cDutyCur,FABRIC.nItemquota,0),&lcDetTmp..nCost4)
      *--c 200200,5 [end]
    *ENDIF
    *B606316,1 KHM 07/31/2002 (End)
    
    =RLOCK()
    *--c 200200,5 [Start]
    *REPLACE nCost1   WITH lnCost1,;
            nCost2   WITH lnCost2,;
            nCost3   WITH lnCost3,;
            nCost4   WITH lnCost4,;
            neCost1  WITH IIF(lcFrnSign1='*',ROUND(lnCost1*laData[24]/lnUnit1,3),ROUND(lnCost1/laData[24]/lnUnit1,3)),;
            neCost2  WITH IIF(lcFrnSign2='*',ROUND(lnCost2*laData[26]/lnUnit2,3),ROUND(lnCost2/laData[26]/lnUnit2,3)),;
            neCost3  WITH IIF(lcFrnSign2='*',ROUND(lnCost3*laData[26]/lnUnit2,3),ROUND(lnCost3/laData[26]/lnUnit2,3)),;
            neCost4  WITH IIF(lcFrnSign2='*',ROUND(lnCost4*laData[26]/lnUnit2,3),ROUND(lnCost4/laData[26]/lnUnit2,3)),;
            PATTERN  WITH lcPattern,;
            WIDTH    WITH lcWidth    

    REPLACE nCost1   WITH lnCost1,;
            neCost1  WITH IIF(lcFrnSign1='*',ROUND(lnCost1*laData[24]/lnUnit1,3),ROUND(lnCost1/laData[24]/lnUnit1,3)),;
            PATTERN  WITH lcPattern
    *--c 200200,5 [end]

    UNLOCK        
    *B606316,1 KHM 07/31/2002 (Begin) Comment the following line
    *IF lcKeytype = 'P' AND SEEK(lcFabric+lcColor,lcContLin) AND &lcContLin..CPRICECUR = laData[23]
    *  SHOW GET lnCost1 DISABLE
    *ENDIF
    *B606316,1 KHM 07/31/2002 (End)
    SHOW WINDOW (lcDetTtl) REFRESH SAME
  ENDIF  
ENDSCAN
SELECT (lclDetTmp)

SCAN 
  lcFab    = &lclDetTmp..FABRIC
  lcColour = &lclDetTmp..COLOR
  IF (&lclDetTmp..FABRIC = lcFab) AND (&lclDetTmp..COLOR = lcColour)
    SELECT (lcDetTmp)
    IF SEEK(&lclDetTmp..cMattype+&lclDetTmp..POMAT+&lclDetTmp..FABRIC+&lclDetTmp..COLOR)
      REPLACE NFabTotQty WITH (&lcDetTmp..NFabTotQty + &lclDetTmp..NFabTotQty)
    ENDIF
    IF &lclDetTmp..lCheckqty
      =SEEK(&lclDetTmp..cMattype+&lclDetTmp..POMAT+&lclDetTmp..FABRIC+&lclDetTmp..COLOR, lcDetTmp) 
      REPLACE nCost1   WITH (&lclDetTmp..nCost1),;
              neCost1  WITH IIF(lcFrnSign1='*',ROUND(&lclDetTmp..nCost1*laData[24]/lnUnit1,3),ROUND(&lclDetTmp..nCost1/laData[24]/lnUnit1,3))
    ENDIF
    
    lcFab    = &lclDetTmp..FABRIC
    lcColour = &lclDetTmp..COLOR
    *--mhm2000
    =SEEK(&lcLDetTmp..Fabric+&lcLDetTmp..Color,'FABRIC')        
    ladata[39] = ladata[39]+&lcLDetTmp..nCost1*(&lcLDetTmp..nfabTotQty)/FABRIC.conv
    *--mhm200200,5
    ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))

    ladata[51] = ladata[51] + (&lcLDetTmp..nfabTotQty)/FABRIC.conv
    ladata[55] = ladata[55] + (&lcLDetTmp..nfabTotQty)/FABRIC.conv
    
  ENDIF 
  *C200200,6 mhm leave QTY Empty if we ave QTY in first field [Start]
  *IF EMPTY(&lcLDetTmp..nfabTotQty) AND EMPTY(&lcLDetTmp..nhousqty ) 
  IF EMPTY(&lcLDetTmp..nfabTotQty) AND EMPTY(&lcLDetTmp..nhousqty ) AND &lcLDetTmp..lCheckqty
  *C200200,6 [End]
    lcMessage = "No quantity entered for item " + &lcLDetTmp..Fabric + ",Color " +&lcLDetTmp..Color 
    =gfModalGen('QRM00000B38018','F',' ',' ',lcMessage)   
    =SEEK(&lcLDetTmp..POMAT+&lcLDetTmp..FABRIC+&lcLDetTmp..COLOR)          
    =lfwBrowUp()
    _CUROBJ = OBJNUM(lnlQty)
    RETURN
  ENDIF
ENDSCAN 
SELECT (lcDetTmp)

SCAN
  IF SEEK(&lcDetTmp..Fabric+&lcDetTmp..Color,'FABRIC')        
    IF !EMPTY(&lcDetTmp..nfabTotQty)
      REPLACE &lcDetTmp..nfabTotQty WITH &lcDetTmp..nfabTotQty/FABRIC.conv
      *--mhm200200,5
      =SEEK(&lcDetTmp..POMAT+&lcDetTmp..FABRIC+&lcDetTmp..COLOR, lclDetTmp )
      *--mhm200200,5

      *ladata[39] = ladata[39]+lnCost1*(&lcDetTmp..nfabTotQty)
      *ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))


      *ladata[51] = ladata[51] + ( &lcDetTmp..nfabTotQty)/FABRIC.conv
      *ladata[55] = ladata[55] + (&lcDetTmp..nfabTotQty)/FABRIC.conv
    ELSE
      SELECT (lclDetTmp)
      SCAN 
        IF (&lclDetTmp..Fabric = &lcDetTmp..Fabric) AND (&lclDetTmp..Color = +&lcDetTmp..Color)
           REPLACE &lcDetTmp..nfabTotQty WITH (&lcLDetTmp..nhousqty/FABRIC.conv + &lcDetTmp..nfabTotQty)
          *--mhm200200,5
          IF &lclDetTmp..lCheckqty
            lnCost1 = &lclDetTmp..nCost1
          ENDIF

          *ladata[39] = ladata[39]+lnCost1*((&lcLDetTmp..nhousqty + &lcDetTmp..nfabTotQty) - &lcDetTmp..nfabTotQty)
          *ladata[39] = ladata[39]+lnlPrice*((&lcLDetTmp..nhousqty + &lcDetTmp..nfabTotQty) - &lcDetTmp..nfabTotQty)/FABRIC.conv
          *--mhm200200,5
          *ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))

          *ladata[51] = ladata[51] + ((&lcLDetTmp..nhousqty + &lcDetTmp..nfabTotQty) - &lcDetTmp..nfabTotQty)/FABRIC.conv
          *ladata[55] = ladata[55] + ((&lcLDetTmp..nhousqty + &lcDetTmp..nfabTotQty) - &lcDetTmp..nfabTotQty)/FABRIC.conv

        ENDIF
      ENDSCAN
      ladata[39] = ladata[39]+lnCost1*( &lcDetTmp..nfabTotQty)
      ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))
      ladata[51] = ladata[51] + (&lcDetTmp..nfabTotQty)
      ladata[55] = ladata[55] + (&lcDetTmp..nfabTotQty)

      *IF EMPTY(&lcDetTmp..nfabTotQty) AND EMPTY(&lcDetTmp..nhousqty )
      *  lcMessage = "No quantity entered for item " + &lcDetTmp..Fabric + ",Color " +&lcDetTmp..Color 
      *   =gfModalGen('QRM00000B38018','F',' ',' ',lcMessage)   
      *   =SEEK(&lcDetTmp..POMAT+&lcDetTmp..FABRIC+&lcDetTmp..COLOR)          
      *  _CUROBJ = OBJNUM(lnlQty)
      *  RETURN
      *ENDIF
    ENDIF  
  ENDIF  
ENDSCAN  
SELECT(lclDetTmp)

SELECT(lcDetTmp)
LOCATE
SELECT(lnAlias)
CLEAR READ

*!*************************************************************
*! Name      : lfvInsert
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! Purpose   : Validate Units.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvInsert()
*!*************************************************************
*
FUNCTION lfvInsert
PRIVATE lnAlias 

lnAlias = SELECT(0)
SELECT (lclDetTmp)
LOCATE
*--to update Pomat field in case of retrive data
IF !EOF()
  lclPomat   = &lclDetTmp..Pomat
  lclMatType = &lclDetTmp..cMatType
ELSE
  lclPomat   = SPACE(7) 
  lclMatType = 'P'
ENDIF
SELECT (lclDetTmp)
APPEND BLANK
REPLACE Pomat      WITH lclPomat,;
        cMatType   WITH lclMatType,;
        Vendor     WITH ladata[2],;
        cWareCode  WITH ladata[3],;
        TRANCD     WITH '1'        ,;
        cStatus    WITH IIF(cStatus ='A',cStatus ,IIF(EMPTY(cStatus),'A','M'))
        

IF llInsert
  REPLACE  &lclDetTmp..Fabric     WITH lclFabrc,;
           &lclDetTmp..Pattern    WITH lclPattrn,; 
           &lclDetTmp..Color      WITH lclColor,; 
           &lclDetTmp..cDesc      WITH lclDesc,;
           &lclDetTmp..cPer       WITH lclPer,;
           &lclDetTmp..Order      WITH '',;
           &lclDetTmp..nCost1     WITH lnlPrice,;
           &lclDetTmp..nFabTotQty WITH 0,;
           &lclDetTmp..nHousQty   WITH 0,;
           &lclDetTmp..mlNotes    WITH lmNotes

ENDIF
*lclFabrc  = &lclDetTmp..Fabric
*lclPattrn = &lclDetTmp..Pattern
*lclColor  = &lclDetTmp..Color
*lclDesc   = &lclDetTmp..cDesc
*lclPer    = &lclDetTmp..cPer
lclorder  = ''
*lnlPrice  = &lclDetTmp..nCost1
lnlQty    = 0
lnOrdQty  = 0

*B606887,1 AMH Display the correct pattern and fabric description [Start]
lclPattrn = &lclDetTmp..Pattern
lclDesc   = &lclDetTmp..cDesc
SHOW GETS
*B606887,1 AMH [End]

SHOW GET  lclFabrc   ENABLE 
SHOW GET  lclPattrn  ENABLE 
SHOW GET  lclColor   ENABLE 
SHOW GET  iblFabrc   ENABLE 
SHOW GET  PblRemov   ENABLE 

*--to update file
=lfwBrowUp()
SHOW GET lclorder   DISABLE 
SHOW GET lnlPrice   DISABLE 
SHOW GET lnlQty     DISABLE 
SHOW GET lnOrdQty   DISABLE 
*--c 200200,5 [START]
*SHOW GET lclWidth   DISABLE 
*--c 200200,5 [end]
*SHOW GET iblPatt    DISABLE 
llbrBAK =.F.
*SHOW GET iblOrder   DISABLE 

lclorder  = ''
lnlQty    = 0
lnOrdQty  = 0

*B606887,1 AMH Don't insert the same fabric from insert button [Start]
*llInsert = .T.
llInsert = .F.
*B606887,1 AMH [End]

_CUROBJ = OBJNUM(iblFabrc)

*--END function
*:*************************************************************
*: Name      : lfwBrowUp
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*: Purpose   : Refresh record pointer.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfwBrowUp()
*:*************************************************************
*
FUNCTION lfwBrowUp

lnAlias = SELECT(0)
SELECT (lclDetTmp)

IF !EOF()
  lnRecNo = RECNO(lclDetTmp)
  REPLACE ALL cMarker WITH lcUnMark
  GO lnRecNo
  REPLACE cMarker WITH lcMark

  lclFabrc   = &lclDetTmp..Fabric
  
  *B606670,1 AMH Update the pattern value only if fabric is not empty [Start]
  *lclPattrn  = &lclDetTmp..Pattern
  lclPattrn  = IIF(EMPTY(FABRIC),lclPattrn,Pattern)
  *B606670,1 AMH [End]
  
  lclColor   = &lclDetTmp..Color 
  lclDesc    = &lclDetTmp..cDesc
  lclPer     = &lclDetTmp..cPer
  lclorder   = &lclDetTmp..Order
  lnlPrice   = &lclDetTmp..nCost1
  lnlQty     = &lclDetTmp..nFabTotQty
  lnOrdQty   = &lclDetTmp..nHousQty
  lmNotes    = &lclDetTmp..mlNotes
  *--c 200200,5 [Start]
  *lnlCost2   = &lclDetTmp..nCost2 
  *lnlCost3   = &lclDetTmp..nCost3 
  *lnlCost4   = &lclDetTmp..nCost4 
  *lclWidth   = &lclDetTmp..Width 
  *--c 200200,5 [end]

  *--200200,6 mhm Disable all objects in view mode (as per wab )[Start]
  IF !laScrMode[2]
  *C200200,6 MHM [End]
    SHOW GET lclFabrc   ENABLE 
    SHOW GET lclPattrn  ENABLE 
    SHOW GET lclColor   ENABLE 
    IF !EMPTY(&lclDetTmp..Fabric) .AND. !EMPTY(&lclDetTmp..Color )
      SHOW GET lclDesc    DISABLE 
      SHOW GET lclPer     DISABLE
      SHOW GET lclorder   ENABLE 
      *--mhm 200200,5
      IF (&lclDetTmp..lCheckqty )
        SHOW GET lnlPrice   ENABLE 
      ELSE
        SHOW GET lnlPrice   DISABLE 
      ENDIF  
      *--mhm 200200,5
      SHOW GET lnlQty     ENABLE 
      IF !EMPTY(&lclDetTmp..Order)
        SHOW GET lnOrdQty   ENABLE 
      ELSE  
        SHOW GET lnOrdQty   DISABLE 
      ENDIF  
      *--mhm 200200,4
      *--c 200200,5  no need for it customer not approve[Start]
      *SHOW GET lnlCost2   DISABLE 
      *SHOW GET lnlCost3   DISABLE 
      *SHOW GET lnlCost4   DISABLE 
      *--c 200200,5 [End]
      *--mhm 200200,4
      *SHOW GET lclWidth   ENABLE 
      SHOW GET PblRemov   ENABLE 
      SHOW GET PbZoom     ENABLE 
      *--mhm200200,5
      *SHOW GET lmNotes    ENABLE 
      *--mhm200200,5
      llbrBAK =.T.
      SHOW GET iblOrder   ENABLE 
    ENDIF  
    SHOW GET iblFabrc   ENABLE 
    *IF EMPTY(&lclDetTmp..Fabric)
      SHOW GET iblPatt    ENABLE 
    *ELSE  
    *  SHOW GET iblPatt    DISABLE 
    *ENDIF  
    SHOW GET iblColor   ENABLE 
    SHOW WINDOW  (lcDetTtl) REFRESH SAME

  *C200200,6 MHM Disable all objects in View mode [Start]
  ELSE
    SHOW GET lclFabrc   DISABLE 
    SHOW GET lclPattrn  DISABLE 
    SHOW GET lclColor   DISABLE 
    SHOW GET lclDesc    DISABLE 
    SHOW GET lclPer     DISABLE
    SHOW GET lclorder   DISABLE 
    SHOW GET lnlPrice   DISABLE 
    SHOW GET lnlQty     DISABLE 
    SHOW GET lnOrdQty   DISABLE 
    SHOW GET PblRemov   DISABLE 
    SHOW GET PbZoom     DISABLE 
    SHOW GET iblPatt    DISABLE 
    SHOW GET iblColor   DISABLE 
    SHOW GET iblOrder   DISABLE 
    SHOW GET iblFabrc   DISABLE 
    SHOW GET PbInsrt    DISABLE 
    SHOW GET PblRemov   DISABLE 
    
    *--123397,1 mhm let user modify Custom screen 200200 in view mode [Start]
    IF laScrMode[2] AND !(POFHDR.Status $'X')
      SHOW GET lclFabrc   ENABLE
      SHOW GET lclPattrn  ENABLE
      SHOW GET lclColor   ENABLE
      SHOW GET lclDesc    ENABLE
      SHOW GET lclPer     ENABLE
      SHOW GET lclorder   ENABLE
      SHOW GET lnlPrice   ENABLE
      SHOW GET lnlQty     ENABLE
      SHOW GET lnOrdQty   ENABLE
      SHOW GET PblRemov   ENABLE
      SHOW GET PbZoom     ENABLE
      SHOW GET iblPatt    ENABLE
      SHOW GET iblColor   ENABLE
      SHOW GET iblOrder   ENABLE
      SHOW GET iblFabrc   ENABLE
      SHOW GET PbInsrt    ENABLE
      SHOW GET PblRemov   ENABLE
      SHOW GET PbSave     ENABLE  
    ELSE
      SHOW GET PbSave     DISABLE  
    ENDIF
    *--123397,1 mhm [End]
    
  ENDIF
  *C200200,6 MHM Disable all objects in View mode [End]
ENDIF

SELECT(lnAlias)
*-- End Of lfwBrowUp.

*!*************************************************************
*! Name        : lfwlOrdr
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old color fieldin a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfwlOrdr

lcOldOrd = lclorder
*:*************************************************************
*: Name      : lfvOrderNo
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*: Purpose   : Add sales order to item color.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfvOrderNo()
*:*************************************************************
*
FUNCTION lfvOrderNo
PRIVATE laBrowArr , lcOldFilt ,lnAlias,lcOldFiltr
DECLARE laBrowArr[1]
STORE '' TO laBrowArr , lcOldFiltr
STORE 0 To lnAlias
IF MDOWN()
  RETURN
ENDIF

*--mhm 200200,4
*IF EMPTY(lclorder) .AND. !EMPTY(lnlQty) !(llOrdbrw)
*  SHOW GET lnOrdQty   DISABLE 
*  REPLACE &lclDetTmp..lCheckIns WITH .T.
*  =lfvInsert()
*  RETURN
*ENDIF
*--mhm 200200,4 

*--mhm 200200,4
*-- lclorder : - variable holed the entered value in the inlist filed.
IF !EMPTY(lclorder) OR (llOrdbrw) 
  IF !SEEK('O'+lclorder,'ORDHDR') .OR. !(ORDHDR.Status $ 'OH') 
    lnAlias = SELECT (0)
    SELECT ORDHDR
    lcOldFiltr = FILTER()
    lcOldBros = lcBrFields
    SET FILTER TO ORDHDR.Status $ 'OH'
    lcOrder = lclorder

    *--mhm200200,5 to make soft seek [start]
    =SEEK('O'+ALLTRIM(lclorder))
    *--mhm200200,5 make soft seek[End]

    lcBrFields = [Order:H="Order#",]+;
                 [status:1:H="S",]+;
                 [ACCOUNT:H="Acct",]+;
                 [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
                 IIF(lcOrdType='T',[Dept:5:H="Dept.",],'')+;
                 [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
                 [Open:H="Open.Qty.",]+;
                 [OpenAmt:H="Open.Amt.",]+;
                 [Ship:H="Ship.Qty.",]+;
                 [Shipamt:H="Ship.Amt.",]+;
                 [start:H="Start",]+;
                 [Complete:H="Complete",]+;
                 [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
                 [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
                 [Note1:6:H="Notes"]
    lcOrder = IIF(ARIABROW("lcOrdType","Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','ORDER','laBrowArr'),;
                    OrdHdr.Order,SPACE(6))
  

    lclorder  = lcOrder
    lcBrFields = lcOldBros
    SET FILTER TO &lcOldFiltr
    SHOW GET lclorder 
    SELECT (lnAlias)
  ENDIF

  *--mhm 200200,4 
  *IF EMPTY(lclorder) AND !(llOrdbrw) AND EMPTY(lnlQty)
  *  _CUROBJ = OBJNUM(lclorder)
  *  RETURN
  *ENDIF

  lnlcCheck = 0
  SELECT (lclDetTmp)

  lnCurRec = RECNo()

  REPLACE &lclDetTmp..ORDER WITH lclorder  
  lcLcPom = Pomat
  lclcFab = Fabric
  lclccol = Color
  lclcOrd = order
  LOCATE

  SCAN FOR pomat+fabric+color =  lcLcPom +lclcFab +lclccol 
    IF lclcOrd = Order
      lnlcCheck = lnlcCheck +1
    ENDIF  
  ENDSCAN
  IF lnlcCheck >1
    *WAIT WINDOW "The Same Order"
    =gfModalGen('QRM00000B38018','F',' ',' ','This House No has been entered for the same item/color')   
    lclorder = ''
    GOTO lnCurRec 
    SHOW GET lclorder
    REPLACE &lclDetTmp..ORDER WITH lclorder  
    _CUROBJ = OBJNUM(lclorder)
    RETURN
  ENDIF
  GOTO lnCurRec 
  REPLACE &lclDetTmp..ORDER WITH lclorder  
  =lfwBrowUp()
  *--mhm 200200,4
ELSE 
  
  *B607269,1 ABD - let the program to add new item without repating.[Begin]
  *IF LASTKEY() = 13 OR !MDOWN()
  **IF EMPTY(lclorder) .AND. !EMPTY(lnlQty) AND !(llOrdbrw)
  *  SHOW GET lnOrdQty   DISABLE 
  *  REPLACE &lclDetTmp..lCheckIns WITH .T.
  *  
  *  *B606887,1 AMH insert the same fabric [Start]
  *  llInsert = .T.
  *  *B606887,1 AMH [End]
  *  
  *  =lfvInsert()
  *ENDIF  
  IF LASTKEY() = 13
    lcOldFabric = Fabric
    = lfvInsert ()
    lclFabrc = lcOldFabric
    _CUROBJ = OBJNUM(lclFabrc)
  ENDIF
  *B607269,1 ABD - [End]
  
ENDIF
IF !EMPTY(lclorder) AND (llOrdbrw)
  _CUROBJ = OBJNUM(lnOrdQty)
ENDIF
llOrdbrw = .F.

*-- End OF lfvOrderNo
*!*************************************************************
*! Name        : lfwlQty
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old quantity  in a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfwlQty
lnlOldQty = lnlQty

*:*************************************************************
*: Name      : lfvlQty
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*: Purpose   : Validate Qty
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfvlQty()
*:*************************************************************
*
FUNCTION lfvlQty
PRIVATE lnQty1,lnQty2,lcLcPom,lnlcCheck

IF lnlQty < 0 
  =gfModalGen('INM36014B36000','DIALOG','Quantity')
  lnlQty = lnlOldQty    
ELSE

  *--mhm200200,5
  lnRQty =lflQtyRec(lcFabric,lcColor)
  IF lnlQty < lnRQty*FABRIC.conv 
    =gfModalGen('INM36016B36000','DIALOG')
    lnlQty   = lnlOldQty
    _CUROBJ = OBJNUM(lnlQty)
    RETURN
  ENDIF
  *--mhm200200,5
  lnlcCheck = 0
  SELECT (lclDetTmp)
  IF !(&lclDetTmp..lCheckqty )
    lcLcPom = pomat
    lclcFab = fabric
    lclccol = color
    lclordl = ORDER
    lnCurRec = RECNo()
    SCAN 
      IF pomat+fabric+color =  lcLcPom +lclcFab +lclccol 
        IF (&lclDetTmp..lCheckqty)
            lnlcCheck  = lnlcCheck+1
        ENDIF   
      ENDIF  
    ENDSCAN
    IF lnlcCheck > 0
      lnlQty = 0
      SHOW GET lnlQty
      GOTO lnCurRec 
      RETURN
    ENDIF
    GOTO lnCurRec 
  ENDIF
*--mhm2000
  SELECT (lclDetTmp)
  IF (&lclDetTmp..lCheckqty)
    lcLcPom = pomat
    lclcFab = fabric
    lclccol = color
    lclordl = ORDER
    lnCurRec = RECNo()
    lnAcuQty = 0
    SCAN 
      IF pomat+fabric+color =  lcLcPom +lclcFab +lclccol 
              lnAcuQty =   lnAcuQty + &lclDetTmp..nHousQty
      ENDIF  
    ENDSCAN
  
    IF lnAcuQty > lnlQty 
      =gfModalGen('QRM00000B38018','F',' ',' ','Quantity can not exceed House No Quantity ')   
      lnlQty = &lclDetTmp..NFAbTotQty
      SHOW GET lnlQty
      GOTO lnCurRec 
      _CUROBJ = OBJNUM(lnlQty)
      RETURN
    ENDIF
    GOTO lnCurRec 
  ENDIF  

  REPLACE &lclDetTmp..NFAbTotQty WITH lnlQty,;
          &lclDetTmp..lCheckqty  WITH .T.
  =SEEK(&lclDetTmp..Fabric+&lclDetTmp..Color,'FABRIC')        
  *--mhm2000
  *ladata[39] = ladata[39]+lnlPrice*(lnlQty - lnlOldQty)/FABRIC.conv
  ladata[39] = ladata[39]+lnlPrice*(lnlQty - lnlOldQty)/FABRIC.conv
  *--mhm200200,5
  ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))
  ladata[40] = ladata[40]+lnCost2*(lnlQty-lnlOldQty)
  ladata[28] = IIF(lcFrnSign2='*',ROUND(ladata[40]*laData[26]/lnUnit2,3),ROUND(ladata[40]/laData[26]/lnUnit2,3))

  ladata[41] = ladata[41]   + lnCost3*(lnlQty-lnlOldQty)
  ladata[29] = IIF(lcFrnSign2='*',ROUND(ladata[41]*laData[26]/lnUnit2,3),ROUND(ladata[41]/laData[26]/lnUnit2,3))
 
  ladata[42] = ladata[42]   + lnCost4*(lnlQty-lnlOldQty)
  ladata[30] = IIF(lcFrnSign2='*',ROUND(ladata[42]*laData[26]/lnUnit2,3),ROUND(ladata[42]/laData[26]/lnUnit2,3))

  ladata[51] = ladata[51] + (lnlQty  - lnlOldQty)/FABRIC.conv
  ladata[55] = ladata[55] + (lnlQty  - lnlOldQty)/FABRIC.conv

  =lfwBrowUp()
ENDIF  
*:*************************************************************
*: Name      : lfVRemv
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*: Purpose   : Validate Remove
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfVRemv()
*:*************************************************************
*
FUNCTION lfVRemv

lnAlias = SELECT()
SELECT (lclDetTmp)
IF gfModalGen('INM36017B36001','DIALOG') = 1

  IF SEEK(lcKeyType+laData[1]+lclFabrc+lclColor+'2','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Shipment') = 1
    RETURN
  ENDIF
  IF SEEK(lcKeyType+laData[1]+lclFabrc+lclColor+'3','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Damaged') = 1
    RETURN
  ENDIF
  IF SEEK(lcKeyType+laData[1]+lclFabrc+lclColor+'4','POFLN') .AND. gfModalGen('INM36011B36000','DIALOG','Cancelled ') = 1
    RETURN
  ENDIF

  =SEEK(&lclDetTmp..Fabric+&lclDetTmp..Color,'FABRIC')        
  ladata[51] = ladata[51] - (nFabTotQty)/FABRIC.conv
  ladata[55] = ladata[55] - (nFabTotQty)/FABRIC.conv

  laData[39] = laData[39] - (&lclDetTmp..nCost1*&lclDetTmp..nFabTotQty)/FABRIC.conv

  ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))
  ladata[28] = IIF(lcFrnSign2='*',ROUND(ladata[40]*laData[26]/lnUnit2,3),ROUND(ladata[40]/laData[26]/lnUnit2,3))
  ladata[29] = IIF(lcFrnSign2='*',ROUND(ladata[41]*laData[26]/lnUnit2,3),ROUND(ladata[41]/laData[26]/lnUnit2,3))
  ladata[30] = IIF(lcFrnSign2='*',ROUND(ladata[42]*laData[26]/lnUnit2,3),ROUND(ladata[42]/laData[26]/lnUnit2,3))

  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)
  DELETE
ENDIF
SHOW WINDOW (lcDetTtl) REFRESH SAME
LOCATE
IF EOF(lclDetTmp)
  SHOW GET PblRemov DISABLE
  lclFabrc  = SPACE(07) 
  lclPattrn = SPACE(10) 
  lclColor  = SPACE(06) 
  lclDesc   = SPACE(20)
  lclPer    = SPACE(3)
  lclorder  = SPACE(6)
  lnlPrice  = 0
  lnlQty    = 0
  lnOrdQty  = 0
  lmNotes   = SPACE(60) 
  *--c 200200,5 [Start]
  *lnlCost2  = 0
  *lnlCost3  = 0
  *lnlCost4  = 0
  *lclwidth  = ''
  *--c 200200,5 [END]
  SHOW GET lclFabrc   DISABLE 
  SHOW GET lclPattrn  DISABLE 
  SHOW GET lclColor   DISABLE 
  SHOW GET lclDesc    DISABLE 
  SHOW GET lclPer     DISABLE
  SHOW GET lclorder   DISABLE 
  SHOW GET lnlPrice   DISABLE 
  SHOW GET lnlQty     DISABLE 
  SHOW GET lnOrdQty   DISABLE 
  *--mhm200200,5
  *SHOW GET lmNotes    DISABLE 
  *--mhm200200,5
  SHOW GET PblRemov   DISABLE 
  SHOW GET PbZoom     DISABLE 
  SHOW GET iblPatt    DISABLE 
  SHOW GET iblColor   DISABLE 
  SHOW GET iblOrder   DISABLE 
  *--c 200200,5 [Start]
  *SHOW GET lnlCost2   DISABLE 
  *SHOW GET lnlCost3   DISABLE 
  *SHOW GET lnlCost4   DISABLE 
  *SHOW GET lclWidth   DISABLE  
  *--c 200200,5 [End]

ENDIF
=lfwBrowUp()
SELECT (lnAlias)

*:*************************************************************
*: Name      : lfGetlines
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*: Purpose   : Add sales order to item color.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfGetlines()
*:*************************************************************
*
FUNCTION lfGetlines

lnAlias = SELECT()
IF llFlag
  SELECT (lclDetTmp)
  ZAP
  SELECT (lcDetTmp)
  LOCATE
  IF !EOF()
    SCAN
      SCATTER MEMVAR MEMO
      =SEEK(&lcDetTmp..Fabric+&lcDetTmp..Color,'FABRIC')        
      SELECT (lclDetTmp)
      APPEND BLANK
      *--mhm2000
      m.nFabTotQty = CEILING(m.nFabTotQty*FABRIC.conv)
      m.lCheckqty  = .T.
      *--mhm2000
      GATHER MEMVAR MEMO
      m.nFabTotQty = 0.00
      m.lCheckqty  = .F.
      *--c 200200,5 [Start]
      *REPLACE &lclDetTmp..cDesc     WITH FABRIC.DESC,;
              &lclDetTmp..cPer      WITH FABRIC.UomBuy,;
              &lclDetTmp..Width     WITH FABRIC.Width,;
              &lclDetTmp..nCost2    WITH FABRIC.nItm_frt,;
              &lclDetTmp..nCost3    WITH FABRIC.nItem_tax,;
              &lclDetTmp..nCost4    WITH FABRIC.nItemquota


      REPLACE &lclDetTmp..cDesc     WITH FABRIC.DESC,;
              &lclDetTmp..cPer      WITH FABRIC.UomBuy

        llChckFrst = .T.
      *--c 200200,5 [End]
      IF SEEK(&lcDetTmp..pomat+&lcDetTmp..Fabric+&lcDetTmp..Color,'MAPOALO')
        SELECT MAPOALO
        SCAN FOR pomat+Fabric+Color = &lcDetTmp..pomat+&lcDetTmp..Fabric+&lcDetTmp..Color
           IF llChckFrst
             REPLACE &lclDetTmp..Order     WITH MAPOALO.ORDER,;
                    &lclDetTmp..nHousQty  WITH MAPOALO.nhousqty,;
                    &lclDetTmp..mlnotes   WITH MAPOALO.mlnotes
           ELSE
              m.ORDER      = MAPOALO.ORDER
              m.nHousQty   = MAPOALO.nhousqty
              m.mlnotes    = MAPOALO.mlnotes
              m.cDesc      = FABRIC.DESC
              m.cPer       = FABRIC.UomBuy
              INSERT INTO (lclDetTmp) FROM MEMVAR 
           
           ENDIF         
        llChckFrst = .F.
        ENDSCAN          
      ENDIF
    ENDSCAN
  ENDIF
ELSE


  SELECT (lclDetTmp)
  LOCATE
  IF !EOF()
      SCAN
        IF !SEEK(&lclDetTmp..cMattype+&lclDetTmp..POMAT+&lclDetTmp..FABRIC+&lclDetTmp..COLOR, lcDetTmp) 
          DELE
        ENDIF    
      ENDSCAN  
  ENDIF
  SELECT (lcDetTmp)
  LOCATE
  IF !EOF()
    SCAN
      SELECT (lclDetTmp)
      IF SEEK(&lcDetTmp..POMAT+&lcDetTmp..FABRIC+&lcDetTmp..COLOR)  
        =SEEK(&lcDetTmp..Fabric+&lcDetTmp..Color,'FABRIC')        
        SCAN REST WHILE POMAT+FABRIC +COLOR = &lcDetTmp..POMAT+&lcDetTmp..FABRIC+&lcDetTmp..COLOR FOR lCheckqty
          REPLACE  &lclDetTmp..nFabTotQty WITH CEILING(&lcDetTmp..nFabTotQty*FABRIC.conv);
                   &lclDetTmp..nCost1 WITH &lcDetTmp..nCost1
        ENDSCAN           
      ELSE
        SELECT (lcDetTmp)
        SCATTER MEMVAR MEMO
        SELECT (lclDetTmp)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =SEEK(&lcDetTmp..Fabric+&lcDetTmp..Color,'FABRIC')        
        REPLACE  &lclDetTmp..nFabTotQty WITH (&lcDetTmp..nFabTotQty*FABRIC.conv)
        *--mhm2000
        llChckFrst = .T.
        IF SEEK(&lcDetTmp..pomat+&lcDetTmp..Fabric+&lcDetTmp..Color,'MAPOALO')
          SELECT MAPOALO
          SCAN FOR pomat+Fabric+Color = &lcDetTmp..pomat+&lcDetTmp..Fabric+&lcDetTmp..Color
             IF llChckFrst
               REPLACE &lclDetTmp..Order     WITH MAPOALO.ORDER,;
                      &lclDetTmp..nHousQty  WITH MAPOALO.nhousqty,;
                      &lclDetTmp..mlnotes   WITH MAPOALO.mlnotes
             ELSE
               m.ORDER      = MAPOALO.ORDER
               m.nHousQty   = MAPOALO.nhousqty
               m.mlnotes    = MAPOALO.mlnotes
               m.cDesc      = FABRIC.DESC
               m.cPer       = FABRIC.UomBuy
               INSERT INTO (lclDetTmp) FROM MEMVAR 
             ENDIF         
          llChckFrst = .F.
          ENDSCAN          
        ENDIF
        
        *--mhm2000
        
      ENDIF
    ENDSCAN  
  ELSE
    SELECT (lclDetTmp)
    ZAP
  ENDIF  
ENDIF  
llFlag = .F.

SELECT (lnAlias)

*:*************************************************************
*: Name        : lpSavScr
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*: Purpose     : Save/Update P/O.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example     : DO lpSavScr
*:*************************************************************
*
FUNCTION lfSavScr
PRIVATE laTempOrds , lcTempOrd
DIMENSION laTempOrds[1]
STORE '' TO lcTempOrd , laTempOrds

lnAlias = SELECT()
SELECT (lclDetTmp)
GOTO TOP
*--MHM200200,5
*-- Delete all related recored for this po
SELECT MAPOALO
DELETE ALL FOR POMAT= &lclDetTmp..POMAT
*--MHM200200,5
SELECT (lclDetTmp)
GOTO TOP
SCAN
  *-- Delete all related recored for this po,item,color
  *SELECT MAPOALO
  *DELETE ALL FOR POMAT+FABRIC+COLOR+ORDER = &lclDetTmp..POMAT+&lclDetTmp..FABRIC+&lclDetTmp..COLOR++&lclDetTmp..ORDER
  SELECT (lclDetTmp)
  IF !SEEK(POMAT+FABRIC+COLOR+ORDER,'MAPOALO')
    IF !EMPTY(&lclDetTmp..ORDER)
      SCATTER MEMVAR MEMO
      SELECT MAPOALO
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE POMAT     WITH laData[1]     ,;
              FABRIC    WITH &lclDetTmp..Fabric     ,;
              COLOR     WITH &lclDetTmp..Color      ,;
              ORDER     WITH &lclDetTmp..ORDER      ,;
              dAdd_Date WITH gdSysDate              ,;
              cAdd_Time WITH TIME()                 ,;
              cAdd_User WITH gcUser_id
    ENDIF
  ENDIF  
  SELECT (lclDetTmp)
ENDSCAN
*--flag to check if collecting data
llFlag = .T.
*--
SELECT POFHDR
=gfObj_Lock(.F.)

SELECT (lnAlias)
*-- End OF lfSavScr
*!*************************************************************
*! Name        : lfvKFabric
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the fabric field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvlKFabrc

lclFabrc = PADR('?',7)
=lfvlFabrc()
_CUROBJ = OBJNUM(lclPattrn)

*!*************************************************************
*! Name        : lfvlKColor
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the color field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvlKColor

lclColor = PADR('?',6)
=lfvlColor()
*!*************************************************************
*! Name        : lfvlKPatt
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Pattern field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvlKPatt

*B606670,1 AMH Don't change pattern value if not empty to can make soft seek [Start]
*lclPattrn = PADR('?',10)
lclPattrn = IIF(EMPTY(lclPattrn),PADR('?',10),lclPattrn)
llBrowse = .T.
*B606670,1 AMH [End]

=lfvlPatt()
*!*************************************************************
*! Name        : lfvlKPatt
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : Trap tap key
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
PROCEDURE lplTab

*mhm 200200,4
*ON KEY LABEL TAB
DO CASE
  CASE WONTOP() = (lcDetTtl)
    *--mhm2000
    *ACTIVATE WINDOW MAPOAL4
    ACTIVATE WINDOW MAPOAL3
    *--mhm2000
    *--mhm 200200,5
    *_CUROBJ = OBJNUM(PbInsrt)
    _CUROBJ = OBJNUM(lclFabrc)
    *--mhm 200200,5
  OTHE
    *--to validate notes
    *IF _CUROBJ = OBJNUM(lmNotes)
    *  =lfvNotes()
    *ENDIF
    *--to validate fabric
    IF _CUROBJ = OBJNUM(lclFabrc)
      =lfvlFabrc()
    ENDIF
    *--to validate color
    IF _CUROBJ = OBJNUM(lclColor)
      =lfvlColor()
    ENDIF
    *--to validate order
    IF _CUROBJ = OBJNUM(lclorder)
      =lfvOrderNo()
    ENDIF
    *--to validate pattern
    IF _CUROBJ = OBJNUM(lclPattrn)
      =lfvlPatt()
    ENDIF
    *--to validate price
    IF _CUROBJ = OBJNUM(lnlPrice)
      =lfVlprice()
    ENDIF
    *--to validate QTY
    IF _CUROBJ = OBJNUM(lnlQty)
      =lfVlQty()
    ENDIF
    *--to validate Order Qty
    IF _CUROBJ = OBJNUM(lnOrdQty)
      =lfvlOrdQty()
    ENDIF
    *--c 200200,5 [Start]
    *--to validate Order Qty
    *IF _CUROBJ = OBJNUM(lclwidth)
    *  =lfvlWidth()
    *ENDIF
    *--c 200200,5 [End]

   _CUROBJ = _CUROBJ + 1
    
ENDCASE

*!*************************************************************
*! Name        : lfvlKPatt
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Order field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvKOrdrNo

lclorder = PADR('?',10)
=lfvOrderNo()
*!*************************************************************
*! Name        : lfVlprice
*: Developer : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date      : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Price field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfVlprice
PRIVATE lnoldDat
IF lnlPrice < 0 
  =gfModalGen('INM36014B36000','DIALOG','Price')
  lnlPrice  = lnoldPrice
ELSE

  *IF !EMPTY(&lclDetTmp..nfabTotQty)
  *  =SEEK(&lclDetTmp..Fabric+&lclDetTmp..Color,'FABRIC')
  *  lnoldDat = &lclDetTmp..nCost1*(&lclDetTmp..nfabTotQty)/FABRIC.CONV
  *  REPLACE &lclDetTmp..nCost1 WITH lnlPrice 
  *  ladata[39] =ladata[39] -  lnoldDat +&lclDetTmp..nCost1*(&lclDetTmp..nfabTotQty)/FABRIC.CONV
  *  ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))
    
  *ELSE
    REPLACE &lclDetTmp..nCost1 WITH lnlPrice 
  *ENDIF  

ENDIF
=lfwBrowUp()
*!*************************************************************
*! Name        : lfvlOrdQty
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Ordered QTY field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvlOrdQty
PRIVATE lcLcPom,lclcFab,lclccol,lnlcCheck ,lnlcqty ,lnlnqty

IF MDOWN()
  RETURN
ENDIF

IF lnOrdQty< 0 
  =gfModalGen('INM36014B36000','DIALOG','House No. Quantity')
  lnOrdQty= lnlOldQty
ELSE

  STORE 0 TO lnlcqty
  lnlcCheck = 0
  SELECT (lclDetTmp)
  lcLcPom = Pomat
  lclcFab = Fabric
  lclccol = Color
  lclord  = Order
  lnlcqty = lnOrdQty - lnlOldOrd
  lnlOldOrd = lnOrdQty 
  lnlnqty  = 0
  lnCurRec = RECNo()

  LOCATE
  SCAN FOR pomat+fabric+color =  lcLcPom +lclcFab +lclccol 
    lnlcqty = &lclDetTmp..nHousQty +lnlcqty
    lnlnqty = &lclDetTmp..nFAbTotQty +lnlnqty 
  ENDSCAN
*--MHM  200200,4
  IF !(lnlnqty = 0)
    IF lnlcqty >lnlnqty 
      =gfModalGen('QRM00000B38018','F',' ',' ','The total of House No quantities can not exceed the PO quantity for the same item/color')   
      GOTO lnCurRec 
      lnOrdQty = &lclDetTmp..nHousQty
      lnlcqty  = 0
      REPLACE &lclDetTmp..nHousQty WITH 0
      _CUROBJ = OBJNUM(lnOrdQty)
      RETURN
    ENDIF
  ENDIF 
  GOTO lnCurRec 

  REPLACE &lclDetTmp..nHousQty WITH lnOrdQty
  =lfwBrowUp()
  *-mhm 200200,5

  *B607269,1 ABD - let the program to add new item without repating.[Begin]
  *IF !(&lclDetTmp..lCheckIns)
  *  REPLACE &lclDetTmp..lCheckIns WITH .T.
  *  
  *  *B606887,1 AMH insert the same fabric [Start]
  *  llInsert = .T.
  *  *B606887,1 AMH [End]
  *  
  *  =lfvInsert()
  *ENDIF  
  IF LASTKEY() = 13
    lcOldFabric = Fabric
    = lfvInsert ()
    lclFabrc = lcOldFabric
    _CUROBJ = OBJNUM(lclFabrc)
  ENDIF
  *B607269,1 ABD - [End]
  
  *- Abdou1
  *-mhm 200200,5
ENDIF

*!*************************************************************
*! Name        : lfwFabric
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old fabric field in a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfwlOrdQty

lnlOldOrd = lnOrdQty
*!*************************************************************
*! Name        : lfvlWidth
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the width field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*--c 200200,5 [Start]
*FUNCTION lfvlWidth

*=SEEK(laData[1]+lclFabrc+lclColor,(lclDetTmp))
*REPLACE &lclDetTmp..Width WITH lclWidth
*--c 200200,5 [End]


*!*************************************************************
*! Name        : lfvlCost2
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Freight field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*--c 200200,5 [Start]
*FUNCTION lfvlCost2

*IF lnlCost2 < 0 
*  =gfModalGen('INM36014B36000','DIALOG','Freight')
*  lnlCost2  = lnlOldCst2
*ELSE
*  =SEEK(laData[1]+lclFabrc+lclColor,(lclDetTmp))
*  REPLACE &lclDetTmp..nCost2 WITH lnlCost2 ,;
*          &lclDetTmp..nECost2 WITH IIF(lcFrnSign2='*',ROUND(&lclDetTmp..nCost2*laData[26]/lnUnit2,3),ROUND(&lclDetTmp..nCost2/laData[26]/lnUnit2,3))
*ENDIF
*ladata[40] = ladata[40]   + (lnlCost2 - lnlOldCst2)*&lclDetTmp..nFabTotQty
*ladata[28] = IIF(lcFrnSign2='*',ROUND(ladata[40]*laData[26]/lnUnit2,3),ROUND(ladata[40]/laData[26]/lnUnit2,3))
*--c 200200,5 [End]
*!*************************************************************
*! Name        : lfvlCost3
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the quota field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*--c 200200,5 [Start]
*FUNCTION lfvlCost3

*IF lnlCost3 < 0 
*  =gfModalGen('INM36014B36000','DIALOG','Tax')
*  lnlCost3  = lnlOldCst3
*ELSE
*  =SEEK(laData[1]+lclFabrc+lclColor,(lclDetTmp))
*  REPLACE &lclDetTmp..nCost3 WITH lnlCost3 ,;
*          &lclDetTmp..nECost3 WITH IIF(lcFrnSign2='*',ROUND(&lclDetTmp..nCost3*laData[26]/lnUnit2,3),ROUND(nCost3/laData[26]/lnUnit2,3))

*ENDIF
*ladata[41] = ladata[41]   + (lnlCost3 - lnlOldCst3)*&lclDetTmp..nFabTotQty
*ladata[29] = IIF(lcFrnSign2='*',ROUND(ladata[41]*laData[26]/lnUnit2,3),ROUND(ladata[41]/laData[26]/lnUnit2,3))
*--c 200200,5 [End]

*!*************************************************************
*! Name        : lfvlCost4
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the tax field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*--c 200200,5 [Start]
*FUNCTION lfvlCost4

*IF lnlCost4 < 0 
*  =gfModalGen('INM36014B36000','DIALOG','Quota')
*  lnlCost4  = lnlOldCst4
*ELSE
*  =SEEK(laData[1]+lclFabrc+lclColor,(lclDetTmp))
*  REPLACE &lclDetTmp..nCost4 WITH lnlCost4 ,;
*          &lclDetTmp..nECost4 WITH IIF(lcFrnSign2='*',ROUND(&lclDetTmp..nCost4*laData[26]/lnUnit2,3),ROUND(nCost4/laData[26]/lnUnit2,3))
*  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
*  IF (LASTKEY() <> 9 AND LASTKEY() <> 15)
*    _CUROBJ = OBJNUM(pbNew)
*  ENDIF
*ENDIF
*ladata[42] = ladata[42]   + (lnlCost4 - lnlOldCst4)*&lclDetTmp..nFabTotQty
*ladata[30] = IIF(lcFrnSign2='*',ROUND(ladata[42]*laData[26]/lnUnit2,3),ROUND(ladata[42]/laData[26]/lnUnit2,3))
*--c 200200,5 [END]
*!*************************************************************
*! Name        : lfwCost2
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old freight in a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*--c 200200,5 [Start]
*FUNCTION lfwlCost2
*lnlOldCst2 = lnlCost2
*--c 200200,5 [END]

*!*************************************************************
*! Name        : lfwCost3
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old quota in a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*--c 200200,5 [Start]
*FUNCTION lfwlCost3
*lnlOldCst3 = lnlCost3
*--c 200200,5 [END]

*!*************************************************************
*! Name        : lfwCost4
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old tax in a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*--c 200200,5 [Start]
*FUNCTION lfwlCost4

*lnlOldCst4 = lnlCost4
*--c 200200,5 [END]

*!*************************************************************
*! Name        : lfwlprice
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : take the old tax in a variable
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfwlprice

lnoldPrice = lnlPrice
*!*************************************************************
*! Name        : lfTrpKey
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Ordered QTY field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfTrpKey2

IF WONTOP() = lcDetTtl
  ON KEY LABEL TAB     DO lplTab
ENDIF  
*!*************************************************************
*! Name        : lfvKlColor
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Order field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvKlColor

lclcolor = PADR('?',6)
=lfvlColor()

*!*************************************************************
*! Name        : lfvkOrdrNo
*: Developer   : MOHAMED SHOKRY - (MHM) Due to C#200200,1
*: Date        : 06/17/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : validate the Order field from the key field
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfvkOrdrNo

llOrdbrw = .T.
lclorder = PADR('?',6)
=lfvOrderNo()

*!*************************************************************
*! Name        : lfQtyRec
*! Developer   : Mohamed shokry
*! Date        : 07/15/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : calculate the quantity recieved for a spacific line
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*200200,5
FUNCTION lflQtyRec
PARAMETER lcFab , lcClr

lnAlias = SELECT()
SELECT POFLN
*SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1] AND TRANCD <> '1' AND ;
*              FABRIC = lcFab  AND COLOR = lcClr
lnRecQty = 0              
=SEEK(lcKeyType + laData[1]+lcFab+ lcClr)
SCAN REST WHILE cmattype+pomat+fabric+color+trancd = lcKeyType +laData[1] +lcFab+lcClr FOR TRANCD <> '1'
  lnRecQty = lnRecQty +nFabTotQty
ENDSCAN              
SET FILTER TO 
SELECT(lnAlias)
RETURN lnRecQty 
*!*************************************************************
*! Name      : lfFabInWar
*! Developer : Mohamed shokry
*! Date      : 07/19/2001
*! Purpose   : check if the fabric/color is assigned to the warehouse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*200200,5
FUNCTION lfFabInWar
PARAMETER lcfFabric,lcfColor,lcfWare

IF !SEEK(lcfFabric+lcfColor+lcfWare,'FabDye')
    *-- AMER (Start)
    IF llMultiWH
      lnChoice=gfModalGen('TRM36049B36001','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare)) 
    ELSE
      lnChoice = 1
    ENDIF
  IF lnChoice = 1
    DO gpAdFabWar WITH lcfFabric, lcfColor,SPACE(10), lcfWare
  ELSE
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*:**************************************************************************
*:* Name        : lfLnItmSav
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/31/02
*:* Purpose     : Save the new value of item_no
*:***************************************************************************
*:* Called from : SOLNITEM.spx
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfLnitmsav()
*:* REFER TO    :  C200311,1
*:***************************************************************************
FUNCTION lfLnItmSav
IF llSvSoLn
  PRIVATE lcAlias
  lcAlias = ALIAS()
  SELECT (lcOrdLine)
  REPLACE ITEM_NO WITH lcVenSty
  SELECT (lcAlias)
ENDIF  
*-- end of lfLnitmsav.

*:**************************************************************************
*:* Name        : lfvVenSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/01/02
*:* Purpose     : Valid function for lcVenSty field
*:***************************************************************************
*:* Called from : SOLNITEM.spx
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvVenSty()
*:* Refer to    : C200311,1
*:***************************************************************************
FUNCTION lfvVenSty
lcBtn = IIF(lcVenSty # lcOldVnSty,'ENABLE','DISABLE')
ACTIVATE WINDOW 'SOLNITEM'
SHOW GET pbSave &lcBtn
*-- end of lfvVenSty.

*:**************************************************************************
*:* Name        : lfSOLNITEM
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/01/02
*:* Purpose     : Run the screen SOLNITEM.spx
*:***************************************************************************
*:* Called from : soord.prg
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = SOLNITEM()
*:* Refer to    : C200311,1
*:***************************************************************************
FUNCTION lfSOLNITEM
PRIVATE lcItem
lcItem = IIF(laScrMode[2],ORDLINE.ITEM_NO,&lcOrdLine..ITEM_NO)
IF !EMPTY(lcItem)
  STORE lcItem TO lcVenSty,lcOldVnSty
ELSE  
  =SEEK(IIF(laScrMode[2],ORDLINE.STYLE,&lcOrdLine..STYLE),'STYLE') 
  STORE STYLE.CVENSTY TO lcVenSty,lcOldVnSty
ENDIF
lcVSStat = IIF(laScrMode[2],'DISABLE','ENABLE')
DO (gcScrDir+gcWinAppl+"\SOLNITEM.SPX")
*-- end of SOLNITEM.


*:**************************************************************************
*:* Name        : lfUpdItmNo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/01/02
*:* Purpose     : Update ORDLINE.ITEM_NO from STYLE.CVENSTY/lcordline.item_no
*:***************************************************************************
*:* Called from : lfSavScr from soupdate.prg
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdItmNo()
*:* REFER TO    :  C200311,1
*:***************************************************************************
FUNCTION lfUpdItmNo
PRIVATE lcOrder
IF laScrMode[4]
  IF EMPTY(&lcOrdLine..ITEM_NO) AND SEEK(&lcOrdLine..STYLE,'STYLE')
    REPLACE ITEM_NO WITH STYLE.CVENSTY
  ENDIF
ELSE
  IF llOrdLn
    lcOrder = ORDER('ORDLINE')
    SET ORDER TO TAG ORDLINE IN ORDLINE
  
    SELECT (lcOrdline)
    SCAN
      IF SEEK(CORDTYPE+ORDER+STR(LINENO,6),'ORDLINE')
        SELECT ORDLINE
        IF EMPTY(&lcOrdLine..ITEM_NO)
          =SEEK(&lcOrdLine..STYLE,'STYLE')
          REPLACE ITEM_NO WITH STYLE.CVENSTY
        ELSE
          REPLACE ITEM_NO WITH &lcOrdLine..ITEM_NO
        ENDIF
      ENDIF
    ENDSCAN
  
    SET ORDER TO TAG &lcOrder IN ORDLINE
  ENDIF
ENDIF
*-- end of lfUpdItmNo.
*:**************************************************************************
*: Name          : lfUPMATINV
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200313,1
*: Date          : 04/15/2001
*: Purpose       : Function to update the materinal Inv.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfUPMATINV()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200313,1.
*:**************************************************************************
*
FUNCTION lfUPMATINV
PRIVATE lnAlias , lcRSession , lcTranCode 
lnAlias = 0

STORE '' TO lcTranCode , lcRSession

lnAlias = SELECT (0)

lnRecNo = RECNO()
= SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+&lcTmpJour..cRSession)
lcTranCode = MatInvJl.cTran
lcRSession = MatInvJl.cRSession

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNO
ENDIF
          
REPLACE cFabric    WITH lcFabric   ,;
        cColor     WITH lcColor    ,;
        cWareCode  WITH lcToWare   ,;
        cDyelot    WITH lcFDyelot  ,;
        Reference  WITH lcRefer    ,;
        cAdjReason WITH lcAdjCdRsn ,;
        cGlMatAdj  WITH lcAdjAcct  ,;
        dTranDate  WITH ldTrDate   ,;
        dPostDate  WITH ldPostDate ,;
        cTranType  WITH lcTrType   ,;
        cTran      WITH lcTranCode ,;
        nUnitCost  WITH &lcTmpJour..nUnitCost      ,;
        nUntCstBuy WITH &lcTmpJour..nUntCstBuy     ,;
        cRSession  WITH lcRSession                 ,;
        nReceived  WITH ABS(&lcTmpJour..nApply)    ,;
        nStkVal    WITH nReceived * nUnitCost      ,;
        dPostDate  WITH ldPostDate                 ,; 
        cTrn_Seq   WITH cRSession                  ,;
        nMPrvSQty  WITH lnPrvQty                   ,;
        nPrvSVal   WITH lnPrvVal

SELECT(lnAlias)

*-- End OF lfUPMATINV.
*:**************************************************************************
*: Name          : LFUPMATRLS
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200313,1
*: Date          : 04/15/2001
*: Purpose       : Function to update the materinal Inv.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = LFUPMATRLS()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200313,1.
*:**************************************************************************
*
FUNCTION LFUPMATRLS
PRIVATE lnAlias
lnAlias = 0

lnAlias = SELECT (0)
SELECT (lcRoTmpUse)
LOCATE
SELECT ROLLS
REPLACE cRollItem WITH &lcTmpRoll..cFabric    ,;
        Color     WITH &lcTmpRoll..cColor     ,;
        cWareCode WITH &lcTmpRoll..cWareCode  ,;
        Dyelot    WITH &lcTmpRoll..cDyelot    ,;
        cRollId   WITH &lcTmpRoll..cRollId    ,;
        nQty      WITH &lcTmpRoll..nApply * IIF(lcTrType='1',Fabric.Conv,1),;
        nQtyBal   WITH &lcTmpRoll..nApply * IIF(lcTrType='1',Fabric.Conv,1),;
        TranCd    WITH &lcTmpRoll..RolTranCd  ,;
        cSession  WITH &lcRoTmpUse..cRSession ,;
        cRSession WITH &lcRoTmpUse..cRSession
SELECT(lnAlias)

*-- End Of LFUPMATRLS.
*:**************************************************************************
*:**************************************************************************
*: Name          : LFCHNGLINE
*: Developer     : ALBERT RAIF - (ALB) Due to C#200445,1
*: Date          : 11/20/2002
*: Purpose       : Function to Incrice the check lines
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = LFCHNGLINE()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200313,1.
*:**************************************************************************
*
FUNCTION LFCHNGLINE
PARAMETER lnChkLines
*B129452,1 MMR 10/11/2005 Increase number of lines in ap check to 27 lines instate of 20 lines.[Start]
*lnRpStub = lnChkLines
lnRpStub = 27.00
*B129452,1 MMR.[End]
*:**************************************************************************
*: Name          : lfUpdRMSO
*: Developer     : Khalid Mohi El-Din (KHM)
*: Date          : 05/04/2003
*: Purpose       : To add credited quantities back onto sales order
*:**************************************************************************
*: Called from   : RMCRMEM.PRG - Credit memo
*:**************************************************************************
*: Example       : = lfUpdRMSO()
*:**************************************************************************
*: C200512,1.
*:**************************************************************************
FUNCTION lfUpdRMSO
PRIVATE lnAlias, llUseDyes, llUseOrdHd, llUseOrdLn, lnOpnQty, lnOpnAmt, llUpdOrdHd, lcStore

*-- If there is no inovice and order selected
IF EMPTY(laData[7]) AND EMPTY(laData[8])
  RETURN
ENDIF

IF gfModalGen('QRM00000B00006','?','','','Do you wish to add credited quantities back onto '+;
                                              'to Sales Order ' + laData[8] + '?') = 2
  RETURN
ENDIF                                              


STORE .F. TO llUseOrdHd, llUseOrdLn, llUpdOrdHd
STORE 0   TO lnOpnQty, lnOpnAmt
lnAlias   = SELECT(0)
llUseDyes = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp)) = 'Y'
lcStore   = laData[4]

IF !USED('OrdHdr')
  =gfOpenFile(gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
  llUseOrdHd = .T.
ELSE
  SET ORDER TO OrdHdr IN OrdHdr
ENDIF
=SEEK('O'+laData[8],'OrdHdr')
lcStore = IIF(EMPTY(lcStore), IIF(OrdHdr.Multi="Y",InvHdr.Store,OrdHdr.Store), lcStore)

IF !USED('OrdLine')
  =gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'OrdLinSt','SH')
  llUseOrdLn = .T.
ENDIF

SELECT (lcCrMemLin)
SCAN
  *--cordtype+order+store+style+STR(lineno,6) (OrdLine)
  *-- Add the returned Qty to the OrdLine Qty.
  IF SEEK('O'+laData[8]+lcStore+Style,'OrdLine')
    llUpdOrdHd = .T.
    SELECT OrdLine
    REPLACE Qty1   WITH Qty1 + &lcCrMemLin..Qty1   ;
            Qty2   WITH Qty2 + &lcCrMemLin..Qty2   ;
            Qty3   WITH Qty3 + &lcCrMemLin..Qty3   ;
            Qty4   WITH Qty4 + &lcCrMemLin..Qty4   ;
            Qty5   WITH Qty5 + &lcCrMemLin..Qty5   ;
            Qty6   WITH Qty6 + &lcCrMemLin..Qty6   ;
            Qty7   WITH Qty7 + &lcCrMemLin..Qty7   ;
            Qty8   WITH Qty8 + &lcCrMemLin..Qty8   ;
            TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    =gfTraceKey("OrdLine" , 'O'+laData[8]+lcStore+Style , "M")
    
    *-- Variables to hold the returned Qty & Amount to update the OrdHdr.
    lnOpnQty = lnOpnQty + &lcCrMemLin..TotQty
    lnOpnAmt = lnOpnAmt + &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
       
    =SEEK(Style,'Style')
    SELECT Style
    = RLOCK()
    REPLACE Ord1   WITH Ord1 + &lcCrMemLin..Qty1   ;
            Ord2   WITH Ord2 + &lcCrMemLin..Qty2   ;
            Ord3   WITH Ord3 + &lcCrMemLin..Qty3   ;
            Ord4   WITH Ord4 + &lcCrMemLin..Qty4   ;
            Ord5   WITH Ord5 + &lcCrMemLin..Qty5   ;
            Ord6   WITH Ord6 + &lcCrMemLin..Qty6   ;
            Ord7   WITH Ord7 + &lcCrMemLin..Qty7   ;
            Ord8   WITH Ord8 + &lcCrMemLin..Qty8   ;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    =gfTraceKey("Style" , &lcCrMemLin..Style , "M")

    IF llUseDyes .AND. Style.cDye_Flg = 'Y'

      *-- If the system use dyelot update the dyelot record in the stydye
      IF SEEK (&lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT, 'StyDye')
        SELECT STYDYE
        = RLOCK()
        REPLACE Ord1   WITH Ord1 + &lcCrMemLin..Qty1   ;
                Ord2   WITH Ord2 + &lcCrMemLin..Qty2   ;
                Ord3   WITH Ord3 + &lcCrMemLin..Qty3   ;
                Ord4   WITH Ord4 + &lcCrMemLin..Qty4   ;
                Ord5   WITH Ord5 + &lcCrMemLin..Qty5   ;
                Ord6   WITH Ord6 + &lcCrMemLin..Qty6   ;
                Ord7   WITH Ord7 + &lcCrMemLin..Qty7   ;
                Ord8   WITH Ord8 + &lcCrMemLin..Qty8   ;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      ENDIF       
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("StyDye" , &lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT , "M")
    ENDIF

    *-- Update the warehouse record in the StyDye file
    IF SEEK(&lcCrMemLin..Style + &lcCrMemHdr..cWareCode + SPACE(10), 'StyDye')
      SELECT StyDye
      REPLACE Ord1   WITH Ord1 + &lcCrMemLin..Qty1   ;
              Ord2   WITH Ord2 + &lcCrMemLin..Qty2   ;
              Ord3   WITH Ord3 + &lcCrMemLin..Qty3   ;
              Ord4   WITH Ord4 + &lcCrMemLin..Qty4   ;
              Ord5   WITH Ord5 + &lcCrMemLin..Qty5   ;
              Ord6   WITH Ord6 + &lcCrMemLin..Qty6   ;
              Ord7   WITH Ord7 + &lcCrMemLin..Qty7   ;
              Ord8   WITH Ord8 + &lcCrMemLin..Qty8   ;
              TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("StyDye" , &lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + SPACE(10) , "M")      
    ENDIF
  ENDIF
ENDSCAN

*-- Update OrdHdr file
IF llUpdOrdHd AND SEEK('O'+laData[8],'OrdHdr')
  SELECT OrdHdr
  REPLACE Open    WITH Open + lnOpnQty           ,;
          OpenAmt WITH OpenAmt + lnOpnAmt        ,;
          Ship    WITH MAX(Ship - lnOpnQty,0)    ,;
          ShipAmt WITH MAX(ShipAmt - lnOpnAmt,0) ,;
          Status  WITH IIF(Status='C','O',Status)
ENDIF

IF llUseOrdHd
  USE IN OrdHdr
ENDIF

IF llUseOrdLn
  USE IN OrdLine
ENDIF

SELECT(lnAlias)
*- Abdou1
*:**************************************************************************
*: Name          : lfInit
*: Developer     : Abdou Elgendy
*: Date          : 11/20/2002
*: Purpose       : Function to clear the screen and prepar it for new fabric.
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfInit()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  B#XXXXX.
*:**************************************************************************
*
FUNCTION lfInit

SHOW GET pbRemove DISABLE
lcFabric  = SPACE(07) 
lcColor   = SPACE(06) 
lcPattern = SPACE(10) 
lcClrDesc = SPACE(15)
lcIDesc   = SPACE(20)
lnCost1   = 0 
lnCost2   = 0 
lnCost3   = 0 
lnCost4   = 0       
lnQty     = 0
lnTotQty  = 0
lnLineNo  = 0


SHOW GET lcFabric   ENABLE
SHOW GET pbRemove   DISABLE
SHOW GET lcColor    DISABLE
SHOW GET lcPattern  DISABLE
SHOW GET lcClrDesc  DISABLE
SHOW GET lcIDesc    DISABLE
SHOW GET lnCost1    DISABLE
SHOW GET lnCost2    DISABLE
SHOW GET lnCost3    DISABLE
SHOW GET lnCost4    DISABLE
SHOW GET lnQty      DISABLE
SHOW GET lnTotQty   DISABLE
SHOW GET lnLineNo   DISABLE

*- End OF lfInit.
*:**************************************************************************
*: Name          : lfDelMapoa
*: Developer     : Abdou Elgendy
*: Date          : 02/24/2004
*: Purpose       : Function to delete the related recored from mapoalo.dbf
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfDelMapoa()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  B#121348.
*:**************************************************************************
*B121348,1 [Start]
FUNCTION lfDelMapoa
PRIVATE lnPr_Alis , lcFabTyps
lcFabTyps = ''

lnPr_Alis = SELECT (0)

*-- get the fabrics types from the setting files.
FOR lnStart = 1 TO 5
  lcStart = STR(lnStart,1)
  IF lcIType&lcStart $ 'TF'
    lcFabTyps = lcFabTyps + lcStart
  ENDIF
ENDFOR

lcOldSet_Del = SET('DELETED')

SET DELETED OFF
*-- Index = Pomat+fabric+color+order
llOpMapoal = gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
llCtktBom  = gfOpenFile(gcDataDir+'CtktBom','CtktBom','SH')

*-- Check first if this Po is excist into the material Po Allocation.
IF SEEK('2'+laData[1],'CUTPICK') .AND. SEEK(CUTPICK.Order,"MAPOALO")
  *-- order+fabric+color
  *-- Scan on the posln for deleted record. 
  SELECT POSLN
  IF SEEK(lcAType+laData[1])
    SCAN WHILE cStyType+PO = lcAType+laData[1] FOR  Reference ='DELETED' .AND. !DELETED()
      *-- Check for current style if allocated or not.
      IF SEEK('2'+laData[1]+POSLN.Style,'CUTPICK')
        *-- Check if the cost sheet is 
        IF SEEK("I"+laData[1] , "CtktBom")
          SELECT CtktBom
          *-- Index On cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
          SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = "I" + laData[1] FOR TYP $ lcFabTyps
            *-- Seek with the current fabric color into the mapoalo file to delete the current fabric color.
            *-- order+fabric+color
            IF SEEK(CUTPICK.order+Left(CtktBom.Item,7)+CtktBom.iclr,"MAPOALO")
              SELECT MAPOALO
              SCAN REST WHILE order+fabric+color = CUTPICK.order+Left(CtktBom.Item,7)+CtktBom.iclr
                DELETE
              ENDSCAN
              SELECT CtktBom
            ENDIF
          ENDSCAN
          SELECT CtktBom
        ENDIF
      ENDIF
    ENDSCAN  
  ENDIF  
ENDIF

*-- Close the file.
IF llOpMapoal
  USE IN MAPOALO
ENDIF

IF llCtktBom
  USE IN CtktBom  
ENDIF

SET DELETED &lcOldSet_Del

*-- return to old alias.
SELECT(lnPr_Alis)

*-- End Of DELMAPOA
*B121348,1 [End]
*:**************************************************************************
*: Name          : lfSAVCSTSH
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to save sales order cost sheet
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSAVCSTSH()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSAVCSTSH

IF !USED('ORDBOM')
  =gfOpenFile(gcDataDir+'ORDBOM',gcDataDir+'ORDBOM','SH')
ENDIF

SELECT ORDBOM
=SEEK(ORDHDR.ORDER)
SCAN REST WHILE ORDER = ORDHDR.ORDER
  IF !SEEK(ORDHDR.cOrdType+ORDER,'ORDLINE') 
    DELETE
  ENDIF
ENDSCAN

*-- Text  Message  : Do you want to create/modify the sales order Cost Sheet now ?
*-- Message Number : Custom Message
*-- Text Button    : \<Yes;\?\<No
*-- Button  Number : 32000

lcMsg2 = 'Do you want to create/modify the sales order Cost Sheet now ?'
lnMsgNo=gfModalGen("TRM00000B32000","DIALOG",.F.,.F.,lcMsg2)
IF lnMsgNo = 1
  IF !EOF('ORDHDR')
      lcOrder = "'"+ORDHDR.ORDER+"'"
      PUSH KEY
      ON KEY
      DO gpDoProg WITH 'AWRSOALCST',.F.,'SO',lcOrder 
      POP KEY
      llShow = .F.    
      STORE .F. TO laScrMode
      laScrMode[2]=.T.
      =gfStatic()                      && Save inviroment before terminate
  ENDIF  
ENDIF
*--END lfSAVCSTSH
*:**************************************************************************
*: Name          : lfSODTYPOP
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to Update popup for duty 
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSODTYPOP()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************

FUNCTION lfSODTYPOP

DECLARE laDutyable[4,2]
  
laDutyable[1,1] = "N/A"
laDutyable[2,1] = "Duty"
laDutyable[3,1] = "Material Loss"
laDutyable[4,1] = "Both"
  
laDutyable[1,2] = " "
laDutyable[2,2] = "1"
laDutyable[3,2] = "2"
laDutyable[4,2] = "3"

STORE 1 TO ibClrCls9 , ibClrCls10, lnClrClass
STORE 1 TO ibNDutyabl
SHOW GET ibNDutyabl  ENABLE

IF llMulCurr
  =SEEK(&lcOrdSty..Style,'STYLE')
  lcPricCur  = Style.cPriceCur
  lcDutyCur  = Style.cDutyCur
ENDIF  

DO CASE
  CASE lcCstType $ 'FT'
      llDifColor = .F.  && Flag to know that there is no color entered yet.
      STORE 3 TO ibClrCls9 , ibClrCls10, lnClrClass
      ibNDutyabl = 4

      SHOW GET ibNDutyabl  ENABLE
      
      SHOW GET ibItmClr10  ENABLE
      SHOW GET ibCsItmC10  ENABLE
      SHOW GET lcCsItmC10  ENABLE
      SHOW GET lcCsItmC10  ENABLE

      PRIVATE lnAlias
      lnAlias = SELECT()
      SELECT(lcOrdLine)
      lnRecNo = RECNO()
      lcMajPrt = &lcOrdSty..Style
      lnCount = 1

      STORE '' TO laColors10

      SCAN FOR lcMajPrt =  PADR(&lcOrdLine..Style,lnMajorLen)      
        IF  ASCAN(laColors10,SUBSTR(&lcOrdLine..Style , lnColorStr , lnColorLen))=0
          DIMENSION laColors10[lnCount,4]
          laColors10[lnCount,1]   = SUBSTR(&lcOrdLine..Style , lnColorStr , lnColorLen)
          laColors10[lnCount,2]   = lnCount
          laColors10[lnCount,3]   = SPACE(6)
          laColors10[lnCount,4]   = 0
          lnCount = 1 +lnCount
        ENDIF  
      ENDSCAN
      
  CASE lcCstType = 'M'
      STORE 1 TO ibClrCls9 , ibClrCls10, lnClrClass
  
      =gfwCodePop(@laCodInfo, "MFGCODE", "D")
      lcMfgCode  = laMfgCode[lnNMfgCode,2]
      IF lcMfgCode = "MATERI"
        lnNCstPrc7 = BomDef.nMatLosRat
        SHOW GET lnNCstPrc7 DISABLE
        =lfvNCstPrc("7")
      ENDIF
    
ENDCASE
*--END lfSODTYPOP
*:**************************************************************************
*: Name          : lfSOVDTYPO
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to validate popup for duty 
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOVDTYPO()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOVDTYPO

IF lcCstType = "D" 
  IF ibNDutyabl = 2
    ibNDutyabl = 1
    SHOW GET ibNDutyabl ENABLE
    RETURN
  ENDIF

  IF lcDutyable = "1"
    ibDutyabl = 1
    SHOW GET ibDutyabl  &lcLinStat
    RETURN
  ENDIF
ENDIF

IF lcCstType = "M" 
  IF ibNDutyabl = 3
    IF lcMfgCode = "MATERI"
      ibNDutyabl = 1
      SHOW GET ibNDutyabl ENABLE
      RETURN
    ENDIF  
  ENDIF
ENDIF

lcNDutyable = laDutyable[ibNDutyabl,2]
*--END lfSOVDTYPO
*:**************************************************************************
*: Name          : lfSOUPDCST
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to Update Unit Price Cost
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOUPDCST()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOUPDCST

DO lfUpdCost IN gcAppHome+"SO\SOALCST.FXP" WITH 1 , 0
*--END lfSOUPDCST
*:**************************************************************************
*: Name          : lfSOCSTMNU
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to create Style Cost Sheet Option
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOCSTMNU()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOCSTMNU

lnNextBar = 18
DEFINE BAR lnNextBar OF _INQURYPOP PROMPT 'Sales Order Cost Sheet'  SKIP FOR (laScrMode[1]) .OR. (laScrMode[2])
lnNextBar = lnNextBar+1
*--END lfSOCSTMNU
*:**************************************************************************
*: Name          : lfSORNNCST  
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to Get sales order cost sheet
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOUPDCST()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  .
*:**************************************************************************
*
FUNCTION lfSORNNCST  
PUSH KEY
ON KEY
IF !EOF('ORDHDR')
    lcOrder = "'"+ORDHDR.ORDER+"'"
    DO gpDoProg WITH 'AWRSOALCST',.F.,'SO',lcOrder 
ENDIF  
POP KEY
*--ENd lfSORNNCST  
*!*************************************************************
*! Name      : lfvNCstPrc
*: Developer : Mohamed Shokry
*: Date      : 06/10/2004
*! Purpose   : Valide function for cost percent for Duty and 
*!           : Manufactuing operations while adding new cost item.
*!*************************************************************
*! Calls     :None
*!*************************************************************
*! Passed Parameters  : lcNo-> Define which percentage
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvNCstPrc(lcNo)
*!*************************************************************
*: Due to  C#037892,1.
*!*************************************************************
*
FUNCTION lfvNCstPrc
PARAMETERS lcNo
PRIVATE lcNo

IF lcNo = "6"
  lnNCstPrc7 = lnNCstPrc6
ELSE
  lnNCstPrc6 = lnNCstPrc7
ENDIF

lnNUntCst6 = 0
lnNUntCst7 = 0

*-- Recalculate total and equevlent cost

lnTotFCst6  = IIF(lcCstType = 'S' , ROUND(CEILING(lnNEsUnt6B*(1+(lnNWstg6/100))) * lnNUntCst6 , 2) , ROUND(ROUND((lnNEsUnt6A*(1+(lnNWstg6/100))) , 3) * lnNUntCst6 , 2))

lnTotFCst7  = ROUND(ROUND((lnNEstUnt7*(1+(lnNWstg7/100))) , 3) * lnNUntCst7 , 2)

lnTotEqu6   = ROUND(lnTotFCst6 &lcRateSign lnExRate6 &lcUnitSign lnCurrUnit,2)
lnTotEqu7   = ROUND(lnTotFCst7 &lcRateSign lnExRate7 &lcUnitSign lnCurrUnit,2)

*-- Refresh the cost objects.
SHOW GET lnNUntCst&lcNo  ENABLE
SHOW GET lnTotFCst&lcNo
SHOW GET lnTotEqu&lcNo

*--END lfvNCstPrc
*:**************************************************************************
*: Name          : lfINTDVAR
*: Developer     : Mohamed Shokry
*: Date          : 06/27/2004
*: Purpose       : Function to Initiate variables and Open variables
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfINTDVAR()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  .
*:**************************************************************************
*
FUNCTION  lfINTDVAR

IF !USED('ORDBOM')
  =gfOpenFile(gcDataDir+'ORDBOM',gcDataDir+'ORDPOREC','SH')
ENDIF
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
ENDIF

*--END lfINTDVAR
*:**************************************************************************
*: Name          : lfPOCRTBOM
*: Developer     : Mohamed Shokry
*: Date          : 06/27/2004
*: Purpose       : Function to Create tempfile
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfPOCRTBOM()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  .
*:**************************************************************************
*
FUNCTION lfPOCRTBOM

=SEEK('2'+laData[1],'CUTPICK')
SELECT *,RECNO() AS nRecNo , "S" AS cStatus FROM ORDBOM ;
WHERE cItmMajor = laData[2] .AND. IIF(lcTranType='T' , lMaterial , !lMaterial) ;
INTO DBF (gcWorkDir+lcTmpBom)

*--End lfPOCRTBOM
*:**************************************************************************
*: Name          : lfPOCHKORD
*: Developer     : Mohamed Shokry
*: Date          : 06/27/2004
*: Purpose       : Function to Check Order 
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfPOCHKORD()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  .
*:**************************************************************************
*
FUNCTION lfPOCHKORD
PRIVATE lcTabOrd,lnAlias

lnAlias = 0
lnAlias = SELECT (0)

*- write triggers to use ORDBOM insted of BOM
=SEEK('2'+laData[1]+PosLn.style,'CUTPICK')
SELECT ORDBOM
lcOldOrdr = ORDER()
SET ORDER TO TAG ORDBOM

IF !SEEK(CUTPICK.Order,'ORDBOM')
  *E300725,1 Message : 38031
  *E300725,1 Cost sheet not found for style: xxx cannot purchase order cost sheet
  *E300725,1 Button : 00000
  *E300725,1 Ok
  *C200080,1 AMM Consider the dye order case
  *=gfModalGen('TRM38031B00000','ALERT','style: '+ALLTRIM(SUBSTR(Style,1,LEN(lcMjrMsk)))+'|'+'purchase order')
  =gfModalGen('TRM38031B00000','ALERT','style: '+ALLTRIM(SUBSTR(Style,1,LEN(lcMjrMsk)))+'|'+IIF(EMPTY(lcDyePO),'purchase order','Dye Order'))
  *C200080,1 AMM end
  SELECT ORDBOM
  SET ORDER TO &lcOldOrdr        
  SELECT (lnAlias)
  RETURN .F.
ELSE
  SELECT ORDBOM
  SET ORDER TO &lcOldOrdr        
  SELECT (lnAlias)
  RETURN .T.
ENDIF

*--END lfPOCHKORD
*:**************************************************************************
*: Name          : lfPOUPDBOM
*: Developer     : Mohamed Shokry
*: Date          : 06/27/2004
*: Purpose       : Function to Update Cost Sheet
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfPOUPDBOM()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  .C037960
*:**************************************************************************
*
FUNCTION lfPOUPDBOM

=SEEK('2'+laData[1]+PosLn.style,'CUTPICK')
llContinue = ;
  lfSheetItem(IIF(lcDyePO='D','D',lcTranType),laData[1],laData[29],lcItem,lcColor,;
            &lcTranFile..LineNo,lcDyelot,laData[31],;
            laData[32],@laQty,'ORDBOM',lcTktSheet,lcDetFile,lcOprHdr,;
            @lcLastOpr,IIF(lcTranType='I',PosLn.nCost1,0),;
            @lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,lcDetFile)
*--END lfPOUPDBOM           
*!*************************************************************
*! Name      : lfSheetItem
*! Developer : Mohamed Shokry
*! Date      : 06/20/2004
*! Purpose   : Generate, Modify or Delete cost sheet items for style
*!*************************************************************
*! Calls     : gfGetMemVar,gfModalGen,gfItemMask,gpAdStyWar,gpAdFabWar,lfSelDyelot
*!*************************************************************
*! Parameters: lcTranType : Transaction type   ('M'-'I'-'T')
*!             lcTicketNo : Ticket number
*!             lcLinkCode : WIP Link code
*!             lcItem     : Style/Fabric
*!             lcColor    : Color
*!             lnLineNo   : Ticket line number
*!             lcDyelot   : Dyelot
*!             lcStyWare  : Default Style Issue Warehouse
*!             lcMatWare  : Default Fabric Issue Warehouse
*!             laQty      : Quantity array
*!             lcTmpBom   : Style Cost Sheet file name
*!             lcTktSheet : Cost sheet header file name
*!             lcDetFile  : Cost sheet detailed file name
*!             lcOprHdr   : Operation header file name
*!             lcLastOpr  : Last Operation
*!             lnPrice    : Purchase Price
*!             lnEst1     : Estimated cost 1
*!             lnEst2     : Estimated cost 2
*!             lnEst3     : Estimated cost 3
*!             lnEst4     : Estimated cost 4
*!             lnEst5     : Estimated cost 5
*!             lcTmpName  : Temp name needed to arrange new fabric dyelots
*!             llBackOnly : If .T. update all records in background.
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfSheetItem('M','000001','DEFDEF','ITEMD001','',0,'Dyelot',;
*!                           'WARE1',@laQty,100,'BOM','CTKTBOM','BOMLINE',;
*!                           'MFGOPRHD','000001',0,0,0,0,0)
*!:************************************************************************
*! Modifications:
*!:************************************************************************
*C037960
FUNCTION lfSheetItem
PARAMETERS lcTranType,lcTicketNo,lcLinkCode,lcItem,lcColor,lnLineNo,lcDyelot,;
           lcStyWare,lcMatWare,laQty,lcTmpSheet,lcTktSheet,lcDetFile,lcOprHdr,;
           lcLastOpr,lnPrice,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lcTmpName,llBackOnly


PRIVATE    lcMjrMsk,lcMjrHdr,lcNMjrMsk,laItemSeg,lcClrMsk,lnClrPos,lcItemFile,;
           llDyelot,lcScale,lcItemType,lnTranQty,lcSizes,lnCount,lcCstClr,lnUnitPri,;
           lcCstItm,llContinue,laMfgRFld,lnLastSeq,lnSizePos,lcTmpBomSh,laReq,;
           lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime

PRIVATE    lcMfgGlAcnt,lcExSign,lcUntSin,lnAlias,lcPriceCur,lnPriceRate,;
           lnPriceUnit,lcDutyCur,lnDutyRate,lnDutyUnit, lcCostElem,lnOldTrnQt
lcCostElem = gfTempName()
CREATE CURSOR (lcCostElem) (cTrType C(1), cType C(1), cItem C(19), cIClr C(6),;
                            cMFGCode C(6), cDyelot C(10), cSizes C(8))
INDEX ON cTrType+cType+CItem+CIclr+cMFGCode+cDyelot TAG (lcCostElem) OF (lcCostElem)

PRIVATE lcTranLett

lcTranLett = IIF(lcTranType $ "IMTD",lcTranType ,"I")

STORE 1   TO lnPriceRate,lnPriceUnit,lnDutyRate,lnDutyUnit
STORE '/' TO lcExSign,lcUntSin
IF lcTranType = 'I' 
  =SEEK('P'+lcTicketNo,'POSHDR')
  lcPriceCur  = IIF(EMPTY(POSHDR.cPriceCur),gcBaseCurr,POSHDR.cPriceCur)
  lnPriceRate = IIF(POSHDR.nPriceRat=0,1,POSHDR.nPriceRat)
  lnPriceUnit = IIF(POSHDR.nCurrUnit=0,1,POSHDR.nCurrUnit)
  lcDutyCur   = IIF(EMPTY(POSHDR.cDutyCur),gcBaseCurr,POSHDR.cDutyCur)
  lnDutyRate  = IIF(POSHDR.nDutyRat=0,1,POSHDR.nDutyRat)
  lnDutyUnit  = IIF(POSHDR.nDCurUnit=0,1,POSHDR.nDCurUnit)
ENDIF
STORE ''  TO M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC
=gfGetMemVar('M_WAREHOUSE,M_DYELOT,M_MATDYE,M_USEEXSSC',gcAct_Comp)
lcMjrMsk  = gfItemMask("PM")
lcMjrHdr  = gfItemMask("HM")
lcNMjrMsk = gfItemMask("PN")
lcItmMsk  = gfItemMask("PI")
*--no need for BOM File

SELECT ORDBOM
lcOldOrdr = ORDER()
SET ORDER TO TAG ORDBOM
IF SEEK(CUTPICK.Order,'ORDBOM')
  llCheck = .F.
  SCAN REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode = CUTPICK.Order
    IF LEFT(Style,12) <> LEFT(PosLn.Style,12)
      LOOP
    ENDIF
    
    IF SUBSTR(citmmask,14,6)  <>'******' AND SUBSTR(cItmMask,14,6)<>SUBSTR(PosLn.Style,14,6) 
      LOOP
    ENDIF
    llCheck = .T.
    EXIT
  ENDSCAN  
  IF !llCheck
    *E300725,1 Message : 38031
    *E300725,1 Cost sheet not found for style: xxx cannot generate cutting 
    *E300725,1 ticket cost sheet
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38031B00000','ALERT',IIF(lcTranType='T','Fabric: ',lcMjrHdr+':')+;
                ALLTRIM(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))))+;
                '|'+IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))
    RETURN(.F.)
  ENDIF  
ELSE
  *E300725,1 Message : 38031
  *E300725,1 Cost sheet not found for style: xxx cannot generate cutting 
  *E300725,1 ticket cost sheet
  *E300725,1 Button : 00000
  *E300725,1 Ok
  =gfModalGen('TRM38031B00000','ALERT',IIF(lcTranType='T','Fabric: ',lcMjrHdr+':')+;
              ALLTRIM(IIF(lcTranType='T',lcItem,SUBSTR(lcItem,1,LEN(lcMjrMsk))))+;
              '|'+IIF(lcTranType='M','cutting ticket',IIF(lcTranType='I','purchase order','order')))
  RETURN(.F.)
ENDIF


lnAlias = SELECT()
DECLARE laMfgRFld[7,2],laItemSeg[1],laReq[8]
STORE '' TO lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime,lcMfgGlAcnt
laMfgRFld[1,1] = 'CCONTCODE'
laMfgRFld[1,2] = 'lcContCode'
laMfgRFld[2,1] = 'CCONTNAME'
laMfgRFld[2,2] = 'lcContName'
laMfgRFld[3,1] = 'COPERSEQ'
laMfgRFld[3,2] = 'lcOperSeq'
laMfgRFld[4,1] = 'LINHOUSE'
laMfgRFld[4,2] = 'llInHouse'
laMfgRFld[5,1] = 'LMFGOPR'
laMfgRFld[5,2] = 'llMfgOpr'
laMfgRFld[6,1] = 'LEADTIME'
laMfgRFld[6,2] = 'lnLeadTime'
laMfgRFld[7,1] = 'GLACCOUNT'
laMfgRFld[7,2] = 'lcMfgGlAcnt'

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lcClrMsk = laItemSeg[lnCount,3]
    lnClrPos = laItemSeg[lnCount,4]
  ENDIF
  IF laItemSeg[lnCount,1]='S'
    lnSizePos = laItemSeg[lnCount,4]
  ENDIF
ENDFOR
lnLastSeq = 0
IF !EMPTY(lcLastOpr)
  =gfRltFld(lcLastOpr,@laMfgRFld,'MFGCODE')
  lnLastSeq = VAL(LEFT(lcOperSeq,2))
ENDIF
STORE 0 TO lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnFEst1,lnFEst2,lnFEst3,lnFEst4,lnFEst5
lcItemFile = IIF(lcTranType='T','Fabric','Style')
llDyelot   = SEEK(lcItem+ALLTRIM(lcColor),lcItemFile) .AND. &lcItemFile..cDye_Flg='Y'
lcScale    = IIF(lcTranType='T','',Style.Scale)
llContinue = .T. 

=gfOpenFile(gcDataDir+'Ctktbom',gcDataDir+'Ctktbom','SH')
=gfOpenFile(gcDataDir+'BomLine',gcDataDir+'BomLine','SH')
=gfOpenFile(gcDataDir+'MFGOPRHD',gcDataDir+'MFGOPRHD','SH')

SET ORDER TO TAG Ctktbom  IN (lcTktSheet)
SET ORDER TO TAG BomLine  IN (lcDetFile)
SET ORDER TO TAG MFGOPRHD IN (lcOprHdr)

lcTmpBomSh = gfTempName()
SELECT BOM
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBomSh) FROM ARRAY laFileStru
INDEX ON typ+item+iclr+citmmajor+citmmask TAG (lcTmpBomSh)

SELECT (lcTmpSheet)

=SEEK(CUTPICK.Order)
SCAN REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode = CUTPICK.Order
 IF LEFT(Style,12) <> LEFT(PosLn.Style,12)
    LOOP
  ENDIF
    
  IF SUBSTR(citmmask,14,6)  <>'******' AND SUBSTR(cItmMask,14,6)<>SUBSTR(PosLn.Style,14,6) 
    LOOP
  ENDIF

  SCATTER MEMVAR MEMO
  IF cCatGTyp='S' .AND. M_USEEXSSC .AND. !EMPTY(MSZCROSREF) .AND. ;
     SUBSTR(ALLTRIM(Item),lnSizePos) = STRTRAN(SUBSTR(lcItmMsk,lnSizePos),'X','*')
    FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
      lcLine = MLINE(MSZCROSREF,lnCount)
      IF SUBSTR(lcLine,1,3) <> lcScale
        LOOP
      ENDIF
      m.Item = PADR(SUBSTR(m.Item,1,lnSizePos-1)+SUBSTR(lcLine,AT('~',lcLine)+1,3),19)
      IF !SEEK(m.typ+m.item+m.iclr+m.citmmajor+m.citmmask,lcTmpBomSh)
        INSERT INTO (lcTmpBomSh) FROM MEMVAR
      ENDIF
    ENDFOR
  ELSE
    INSERT INTO (lcTmpBomSh) FROM MEMVAR
  ENDIF
ENDSCAN

IF USED('STYLE')
  SELECT STYLE
  lcStyFIlt = SET('FILTER')
  SET FILTER TO
ENDIF

lcGrade = ''
IF lcTranType <> 'T'
  =SEEK(lcItem,'Style')
  lcGrade = Style.cStyGrade
ELSE
  =SEEK(lcItem+lcColor,'Fabric')
  lcGrade = Fabric.cFabGrade
ENDIF

IF lcTranType = 'I' OR lcTranType = 'D'
  *--Update Bom cost elements depend on cost in P/O.
  =lfUpdPOBom() 
ENDIF

*-- Flag to check if the message already appeares dont display it again.
llMsgDispd = .F.

SELECT (lcTmpBomSh)
SCAN
  *--MAN Added IF EMPTY(cCatGTyp) .OR. EMPTY(Typ)
  IF EMPTY(cCatGTyp) .OR. EMPTY(Typ)
    IF !llMsgDispd
      *--One or more cost items does not have a proper cost tyoe.
      *--Message : 38171
      =gfModalgen("INM38171B00000","ALERT",cItmMask)
      llMsgDispd = .T.
    ENDIF
    LOOP
  ENDIF
  
  IF !LIKE(STRTRAN(IIF(lcTranType='T',LEFT(cItmMask,6),cItmMask),'*','?'),IIF(lcTranType='T',lcColor,lcItem)) .OR. ;
     (!EMPTY(MSIZES) .AND. ATCLINE(lcScale+'~',MSIZES)=0) .OR. ;
     (!EMPTY(MSZCROSREF) .AND. ATCLINE(lcScale+',',MSZCROSREF)=0)     
    LOOP
  ENDIF
  lcItemType = Typ
  IF EMPTY(MSIZES)
    lnTranQty = laQty[9]
    lcSizes   = '1,2,3,4,5,6,7,8'
  ELSE
    lcSizes = SUBSTR(MLINE(MSIZES,ATCLINE(lcScale+'~',MSIZES)),5)
    lnTranQty = 0
    FOR lnCount = 1 TO 8
      lnTranQty = lnTranQty + IIF(STR(lnCount,1) $ lcSizes,laQty[lnCount],0)
    ENDFOR
  ENDIF
  STORE '' TO lcCstClr,lcCstItm,lcCmSizes
  lcCstItmDye = SPACE(10)
  DO CASE
    CASE cCatGTyp='S'
      lcCmSizes='12345678'
      FOR lnCount = 1 TO 8
        laReq[lnCount]=laQty[lnCount]
      ENDFOR
      lcCstItm=''
      FOR lnCount = 1 TO LEN(ITEM)
        IF SUBSTR(ITEM,lnCount,1)='*'
          lcCstItm = lcCstItm + SUBSTR(lcItem,lnCount,1)
        ELSE
          lcCstItm = lcCstItm + SUBSTR(ITEM,lnCount,1)
        ENDIF
      ENDFOR
      IF !SEEK(lcCstItm,'Style')
        LOOP
      ENDIF
      lcCsItmSc = Style.Scale
      =SEEK(lcItem,'Style')
      IF !EMPTY(MSZCROSREF)
        lcCmSizes = ''
        STORE 0 TO laReq
        FOR lnCount = 1 TO MEMLINES(MSZCROSREF)
          lcLine = MLINE(MSZCROSREF,lnCount)
          IF SUBSTR(lcLine,1,3) = lcScale .AND. SUBSTR(lcLine,7,3)=lcCsItmSc
            laReq[VAL(SUBSTR(lcLine,11,1))] = laReq[VAL(SUBSTR(lcLine,11,1))]+;
            laQty[VAL(SUBSTR(lcLine,5,1))]
            lcCmSizes = lcCmSizes + SUBSTR(lcLine,11,1)
          ENDIF
        ENDFOR
      ENDIF
      IF M_WAREHOUSE='Y' .AND. !EMPTY(lcStyWare) .AND. ;
         !SEEK(lcCstItm+lcStyWare+SPACE(10),'STYDYE')
        *E300725,1 Message : 38029
        *E300725,1 Style xxxxx is not assigned to warehouse xxxx
        *E300725,1 Button : 38001
        *E300725,1 Add Cancel
        
        *E300935,4 adjust condition to update without message if llBackOnly is .T.
        *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcCstItm)+'|'+lcStyWare) = 1
        IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Style: '+ALLTRIM(lcCstItm)+'|'+lcStyWare) = 1
          DO gpAdStyWar WITH lcCstItm,SPACE(10),lcStyWare
        ENDIF   
      
      ENDIF
      IF !EMPTY(lcStyWare) AND M_DYELOT='Y' .AND. SEEK(lcCstItm,'Style') .AND. Style.cDye_Flg='Y'

        SELECT (lcDetFile)
        =SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
        LOCATE REST WHILE ;
        cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
        lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
        FOR cSizes = STRTRAN(lcSizes,',','')

        lcCstItmDye = IIF(FOUND(),&lcDetFile..Dyelot,lcDyelot)
        
        IF SEEK(lcCstItm+lcStyWare+SPACE(10),'STYDYE') AND ;
          (EMPTY(lcCstItmDye) OR !SEEK(lcCstItm+lcStyWare+lcCstItmDye,'StyDye'))
          =gfSelDyelot(lcTranType,'S',lcItem,lcColor,lcStyWare,@lcCstItmDye,lcCstItm,'','',llBackOnly)
        ENDIF
      ENDIF

    CASE INLIST(cCatGTyp,'F','T')
      lcCstItm = SUBSTR(Item,1,7)
      IF IClr = '******'
        lcCstClr = IIF(lcTranType='T',lcColor,SUBSTR(lcItem,lnClrPos,LEN(lcClrMsk)))
      ELSE
        lcCstClr = IClr
      ENDIF
      IF (cCatGTyp='F' OR Trim_Invt) AND !SEEK(lcCstItm+lcCstClr,'FABRIC')
        LOOP
      ENDIF

      IF (cCatGTyp='F' .OR. Trim_Invt) .AND. M_WAREHOUSE='Y' .AND. ;
         !EMPTY(lcMatWare) .AND. ;
         !SEEK(PADR(lcCstItm,7)+PADR(lcCstClr,6)+PADR(lcMatWare,6)+SPACE(10),'FABDYE')
       
        *E300725,1 Message : 38029
        *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
        *E300725,1 Button : 38001
        *E300725,1 Add Cancel
        
        *E300935,4 adjust condition to update without message if llBackOnly is .T.
        *E300935,4 IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
        IF llBackOnly OR gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(lcCstItm)+'/'+ALLTRIM(lcCstClr)+'|'+lcMatWare) = 1
          DO gpAdFabWar WITH lcCstItm,lcCstClr,SPACE(10),lcMatWare
        ENDIF   
      ENDIF
      IF !EMPTY(lcMatWare) AND (cCatGTyp='F' OR Trim_Invt) AND M_MATDYE='Y' AND ;
         SEEK(lcCstItm+lcCstClr,'Fabric') .AND. Fabric.cDye_Flg='Y' 

        SELECT (lcDetFile)
        =SEEK(lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
        LOCATE REST WHILE ;
        cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
        lcTranLett +'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
        FOR cSizes = STRTRAN(lcSizes,',','')

        lcCstItmDye = IIF(FOUND(),&lcDetFile..Dyelot,lcDyelot)

        IF SEEK(lcCstItm+lcCstClr+lcMatWare+SPACE(10),'FABDYE') AND ;
          (EMPTY(lcCstItmDye) OR !SEEK(PADR(lcCstItm,7)+PADR(lcCstClr,6)+PADR(lcMatWare,6)+lcCstItmDye,'FabDye'))
  
          *E300935,4 Adjust calling with background parameter (llBackOnly)
          *E300935,4 =gfSelDyelot(lcTranType,'F',lcItem,lcColor,lcMatWare,@lcCstItmDye,lcCstItm,lcCstClr,lcTmpName)
          =gfSelDyelot(lcTranType,'F',lcItem,lcColor,lcMatWare,@lcCstItmDye,lcCstItm,lcCstClr,lcTmpName,llBackOnly)
        ENDIF
      ENDIF
    CASE cCatGTyp='M'
      =gfRltFld(&lcTmpBomSh..MfgCode,@laMfgRFld,'MFGCODE')
      IF llMfgOpr .AND. !SEEK(lcTranLett+lcTicketNo+&lcTmpBomSh..MfgCode,lcOprHdr)
        lcOperSeq = LEFT(lcOperSeq,2)
        IF VAL(lcOperSeq) > lnLastSeq
          lnLastSeq = VAL(lcOperSeq)
          lcLastOpr = &lcTmpBomSh..MfgCode
        ENDIF
        INSERT INTO (lcOprHdr) (cIMTYp,cTktNo,cOprCode,cOperSeq,cContCode,cContName,lInHouse,nNxtLotNo);
        VALUES (lcTranLett,lcTicketNo,&lcTmpBomSh..MfgCode,lcOperSeq,lcContCode,lcContName,llInHouse,1)
      ENDIF
      IF lcTranType = 'I'
        lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
      ENDIF  
    CASE cCatGTyp='P'
      lcExSign = gfGetExSin(@lcUntSin,lcPriceCur)
    CASE !INLIST(cCatGTyp,'S','F','T') AND lcTranType = 'I'
      lcExSign = gfGetExSin(@lcUntSin,lcDutyCur)
  ENDCASE
  SELECT (lcTktSheet)
  IF !SEEK(lcTranLett+lcTicketNo+&lcTmpBomSh..Typ+PADR(lcCstItm,19)+;
     PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+lcCstItmDye)
    APPEND BLANK
    REPLACE CutTkt    WITH lcTicketNo    ,;
            cIMTyp    WITH lcTranLett    ,;
            cCatGTyp  WITH &lcTmpBomSh..cCatGTyp  ,;
            TRIM_INVT WITH &lcTmpBomSh..TRIM_INVT ,;
            cWareCode WITH IIF(cCatGTyp='S',lcStyWare,IIF(cCatGTyp='F' OR ;
                           (cCatGTyp='T' AND TRIM_INVT),lcMatWare,'')) ,;
            Link_Code WITH lcLinkCode            ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            TYP       WITH &lcTmpBomSh..Typ      ,;
            ITEM      WITH lcCstItm              ,;
            ICLR      WITH lcCstClr              ,;
            MfgCode   WITH &lcTmpBomSh..MfgCode  ,;
            cOprCode  WITH &lcTmpBomSh..cOprCode ,;
            DESC      WITH &lcTmpBomSh..Desc     ,;
            Dyelot    WITH lcCstItmDye           ,;
            Order     WITH CutPick.Order         ,;
            UOM       WITH &lcTmpBomSh..UOM      ,;
            DATE      WITH gdSysDate             ,;
            WIDTH     WITH IIF(cCatGTyp='F' OR (cCatGTyp='T' AND TRIM_INVT),FABRIC.WIDTH,'') 
    IF lcTranType = 'M'
      REPLACE CMARKER WITH &lcTmpBomSh..cMarker
    ENDIF
  ENDIF

  SELECT (lcDetFile)
  =SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
  LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  llFoundRec = FOUND()         
  SELECT (lcTktSheet)

  IF SEEK('A'+&lcTmpBomSh..Typ+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+;
                  lcCstItmDye,lcCostElem)
    SELECT (lcCostElem)
    LOCATE REST WHILE cTrType+cType+CItem+CIclr+cMFGCode+cDyelot = 'A'+&lcTmpBomSh..Typ+;
                      PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode+lcCstItmDye;
                  FOR cSizes = STRTRAN(lcSizes,',','')
    IF !FOUND()
      SELECT (lcTktSheet)
      REPLACE Pieces  WITH Pieces  + lnTranQty
      INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                           VALUES ('A',&lcTmpBomSh..Typ,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                       &lcTmpBomSh..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
    ENDIF
  ELSE
    REPLACE Pieces  WITH Pieces  + lnTranQty
    INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                         VALUES ('A',&lcTmpBomSh..Typ,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                     &lcTmpBomSh..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
  ENDIF
  SELECT (lcTktSheet)
  REPLACE Req_Qty WITH Req_Qty + lnTranQty * &lcTmpBomSh..nBomTotQty ,;
          UntQty  WITH IIF(Pieces<>0,Req_Qty/Pieces,0)
  lnUntQty = IIF(lnTranQty=0,UNTQTY,EVALUATE(lcTmpBomSh+'.nBomTotQty'))
  
  *--If tranQty was zero take it from pieces.
  *--to make sure that the cost was updated.
  *--Read lnTotCost and lnFTotCost depend on lnTTranQty insted of lnTranQty.
  lnTTranQty = lnTranQty
  IF lnTranQty = 0
    lnTTranQty = Pieces
  ENDIF 
  DO CASE
    CASE cCatGTyp = 'P'
      lnTotCost  = lnPrice * lnTTranQty &lcExSign lnPriceRate &lcUntSin lnPriceUnit
      lnFTotCost = lnPrice * lnTTranQty 
    CASE &lcTmpBomSh..nPercent > 0
      *--get cost directlly from ORDBOM
      *lnTotCost  = lnPrice*lnTTranQty*(&lcTmpBomSh..nPercent/100) &lcExSign lnPriceRate &lcUntSin lnPriceUnit
      *lnFTotCost = lnPrice*lnTTranQty*(&lcTmpBomSh..nPercent/100)
      lnTotCost  = &lcTmpBomSh..UntCost * lnTTranQty * lnUntQty      
      lnFTotCost = lnTotCost
      
    CASE cCatGTyp = 'S'
      lnTotCost  = Style.TotCost * lnTTranQty * &lcTktSheet..UntQty        
      lnFTotCost = lnTotCost
    CASE cCatGTyp = 'F' OR (cCatGTyp='T' AND TRIM_INVT)
      *lnTotCost  = Fabric.CostBuy/Fabric.Conv * lnTTranQty * lnUntQty      
      *lnFTotCost = lnTotCost
      lnTotCost  = &lcTmpBomSh..UntCost * lnTTranQty * lnUntQty      
      lnFTotCost = lnTotCost
    OTHERWISE
      lnTotCost  = &lcTmpBomSh..TotCost * lnTTranQty &lcExSign lnDutyRate &lcUntSin lnDutyUnit
      lnFTotCost = &lcTmpBomSh..TotCost * lnTTranQty
  ENDCASE
  IF lnTranQty=0
    REPLACE Est_Cost WITH IIF(Est_Cost=0,lnTotCost,Est_Cost)
  ELSE
    REPLACE Est_Cost WITH Est_Cost + lnTotCost
  ENDIF
  REPLACE UntCost  WITH IIF(Req_Qty<>0,Est_Cost/Req_Qty,0)

  lnEst&lcItemType = lnEst&lcItemType  + lnTotCost
  lnFEst&lcItemType= lnFEst&lcItemType + lnFTotCost
  IF &lcTmpBomSh..cCatGTyp = 'S'
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      REPLACE REQ_QTY&lcCount WITH REQ_QTY&lcCount+laReq[lnCount]*&lcTktSheet..UntQty
    ENDFOR
  ENDIF
  REPLACE cCompSizes WITH lcCmSizes 
  =gfAdd_Info(lcTktSheet)
  
  IF Pieces = 0 AND !llBackOnly
    DELETE
  ENDIF
  SELECT (lcTmpBomSh)
  DO CASE
    CASE &lcTmpBomSh..CCATGTYP = 'P'
      lnUnitPri = lnPrice
    CASE INLIST(&lcTmpBomSh..CCATGTYP,'M','D')

      lnUnitPri = &lcTmpBomSh..UntCost
      *IF &lcTmpBomSh..nPercent > 0
      * lnUnitPri  = lnPrice*(&lcTmpBomSh..nPercent/100)
      *ELSE
      *  lnUnitPri = &lcTmpBomSh..UntCost
      *ENDIF  
    CASE INLIST(&lcTmpBomSh..CCATGTYP,'F','T') .AND. SEEK(SUBSTR(lcCstItm,1,7)+lcCstClr,'Fabric')
      lnUnitPri = Fabric.CostBuy/Fabric.Conv
    OTHERWISE
      lnUnitPri = &lcTktSheet..UntCost
  ENDCASE
  SELECT (lcDetFile)
  =SEEK(lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
        PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode)
  LOCATE REST WHILE ;
  cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
  lcTranLett+'1'+lcTicketNo+STR(lnLineNo,6)+&lcTmpBomSh..Typ+PADR(lcItem,19)+;
  PADR(lcColor,6)+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcTmpBomSh..MfgCode ;
  FOR cSizes = STRTRAN(lcSizes,',','')
  IF !FOUND()         
    *!B602124,1 Update Cost item dyelot in the BOMLINE file
    APPEND BLANK
    REPLACE cIMTyp     WITH lcTranLett     ,;
            cTktNo     WITH lcTicketNo     ,;
            LineNo     WITH lnLineNo       ,;
            cStyGrade  WITH lcGrade        ,;
            Style      WITH lcItem         ,;
            SClr       WITH lcColor        ,;
            cBomTyp    WITH &lcTmpBomSh..Typ,;
            cType      WITH '1'            ,;
            cCatGTyp   WITH &lcTmpBomSh..cCatGTyp   ,;
            cOprCode   WITH &lcTmpBomSh..cOprCode   ,;
            UnitQty    WITH &lcTmpBomSh..nBomTotQty ,;
            UnitCost   WITH lnUnitPri      ,;
            Item       WITH lcCstItm       ,;
            IClr       WITH lcCstClr       ,;
            MfgCode    WITH &lcTmpBomSh..MfgCode    ,;
            Dyelot     WITH lcCstItmDye ,;
            cSizes     WITH STRTRAN(lcSizes,',','') ,;
            cCompSizes WITH lcCmSizes
    REPLACE cCostStat WITH &lcTmpBomSh..cCostStat,;
            nPercent   WITH &lcTmpBomSh..nPercent
  ENDIF

  IF lnTranQty = 0
     REPLACE ItemAmt  WITH ItemQty*lnUnitPri ,;
             UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)            
  ELSE
    
    IF SEEK('B'+&lcDetFile..cBomTyp+PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcDetFile..MfgCode+;
                 lcCstItmDye,lcCostElem)
      SELECT (lcCostElem)
      LOCATE REST WHILE cTrType+cType+CItem+CIclr+cMFGCode+cDyelot = 'B'+&lcDetFile..cBomTyp+;
                        PADR(lcCstItm,19)+PADR(lcCstClr,6)+&lcDetFile..MfgCode+lcCstItmDye;
                    FOR cSizes = STRTRAN(lcSizes,',','')
      IF !FOUND()
        SELECT (lcDetFile)
        REPLACE StyQty   WITH StyQty  + lnTranQty
        INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                          VALUES ('B',&lcDetFile..cBomTyp,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                      &lcDetFile..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
      ENDIF
    ELSE
      REPLACE StyQty   WITH StyQty  + lnTranQty
      INSERT INTO (lcCostElem) (cTrType,cType,cItem,cIClr,cMFGCode,cDyelot,cSizes);
                        VALUES ('B',&lcDetFile..cBomTyp,PADR(lcCstItm,19),PADR(lcCstClr,6),;
                                    &lcDetFile..MfgCode,lcCstItmDye,STRTRAN(lcSizes,',',''))
    ENDIF
    SELECT (lcDetFile)
    REPLACE ItemQty  WITH ItemQty + lnTranQty* &lcTmpBomSh..nBomTotQty  ,;
            UnitQty  WITH IIF(StyQty=0,0,ItemQty/StyQty) ,;
            ItemAmt  WITH ItemAmt + lnTranQty * &lcTmpBomSh..nBomTotQty * lnUnitPri ,;
            UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
           
  ENDIF

  IF StyQty = 0 AND !llBackOnly
    DELETE
  ENDIF
ENDSCAN
IF llContinue
  DO CASE
    CASE lcTranType='M' .AND. SEEK(lcTicketNo,'CUTTKTH')  .AND. CUTTKTH.Status # 'H'
      SELECT CUTTKTH
      =RLOCK()
      REPLACE CLASTOPR   WITH lcLastOpr ,;
              NEST_COST1 WITH NEST_COST1 + lnEst1 ,;
              NEST_COST2 WITH NEST_COST2 + lnEst2 ,;
              NEST_COST3 WITH NEST_COST3 + lnEst3 ,;
              NEST_COST4 WITH NEST_COST4 + lnEst4 ,;
              NEST_COST5 WITH NEST_COST5 + lnEst5
      UNLOCK
    CASE lcTranType$'DI' .AND. SEEK(IIF(lcTranType='I','P','D')+lcTicketNo,'POSHDR') .AND. POSHDR.Status='O'
      SELECT POSHDR
      =RLOCK()
      REPLACE CLASTOPR WITH lcLastOpr ,;
              NICOST1  WITH NICOST1 + lnEst1 ,;
              NICOST2  WITH NICOST2 + lnEst2 ,;
              NICOST3  WITH NICOST3 + lnEst3 ,;
              NICOST4  WITH NICOST4 + lnEst4 ,;
              NICOST5  WITH NICOST5 + lnEst5 ,;
              NFCOST1  WITH NFCOST1 + lnFEst1 ,;
              NFCOST2  WITH NFCOST2 + lnFEst2 ,;
              NFCOST3  WITH NFCOST3 + lnFEst3 ,;
              NFCOST4  WITH NFCOST4 + lnFEst4 ,;
              NFCOST5  WITH NFCOST5 + lnFEst5
      UNLOCK
    CASE lcTranType='T' .AND. SEEK(lcTicketNo,'MMFGORDH') .AND. MMFGORDH.Status='O'
      SELECT MMFGORDH
      =RLOCK()
      REPLACE CLASTOPR   WITH lcLastOpr ,;
              NEST_COST1 WITH NEST_COST1 + lnEst1 ,;
              NEST_COST2 WITH NEST_COST2 + lnEst2 ,;
              NEST_COST3 WITH NEST_COST3 + lnEst3 ,;
              NEST_COST4 WITH NEST_COST4 + lnEst4
      UNLOCK
  ENDCASE
ENDIF
IF USED('STYLE')
  SELECT STYLE
  SET FILTER TO &lcStyFIlt
ENDIF
USE IN (lcTmpBomSh)

ERASE (gcWorkDir+lcTmpBomSh+'.DBF')
ERASE (gcWorkDir+lcTmpBomSh+'.CDX')
ERASE (gcWorkDir+lcTmpBomSh+'.FPT')

USE IN (lcCostElem)

SELECT (lnAlias)
RETURN(llContinue)
*:**************************************************************************
*: Name          : lfSODELBOM
*: Developer     : Mohamed Shokry
*: Date          : 06/27/2004
*: Purpose       : Function to Check deleting Order cost sheet
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSODELBOM()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  .037892
*:**************************************************************************
*
FUNCTION lfSODELBOM
PRIVATE lnAlias
IF !USED('ORDBOM')
  =gfOpenFile(gcDataDir+'ORDBOM',gcDataDir+'ORDBOM','SH')
ENDIF

IF SEEK(laData[1],'ORDBOM')
  lnAlias = SELECT (0)
  lcMsg2 = 'Sales order cost sheet found for order '+ladata[1] +' . Are you sure you want to delete ?'
  lnMsgNo=gfModalGen("TRM00000B32000","DIALOG",.F.,.F.,lcMsg2)
      
  IF lnMsgNo =1
    SELECT ORDBOM
    DELETE ALL FOR Order = laData[1]
    SELECT (lnAlias)
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
ELSE
  RETURN .T.
ENDIF
*:**************************************************************************
*: Name          : lfSOCLSCHK
*: Developer     : Mohamed Shokry
*: Date          : 06/27/2004
*: Purpose       : Function to Check Colors
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOCLSCHK()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  .
*:**************************************************************************
*
FUNCTION lfSOCLSCHK

*-- Same As color.
IF lnClrClass = 1
  lcCsItmC10 = SUBSTR(&lcOrdLine..Style , lnColorStr , lnColorLen)
  SHOW GET ibCsItmC&lcNo  DISABLE
  SHOW GET lcCsItmC&lcNo  DISABLE
ENDIF

*:**************************************************************************
*: Name          : lfSODTYPOP
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to Update popup for duty 
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSAVCSTSH()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOMFGPOP

IF lcMfgCode = "MATERI"
  lnNCstPrc7 = BomDef.nMatLosRat
  SHOW GET lnNCstPrc7 DISABLE
ELSE
  lnNCstPrc7 = 0
  SHOW GET lnNCstPrc7 DISABLE
ENDIF

*:**************************************************************************
*: Name          : lfSOUPDUTY
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to Update duty 
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOUPDUTY()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOUPDUTY

REPLACE  cCostStat  WITH lcNDutyable
IF cbPrim
  REPLACE &lcTmpBom..lprimary    WITH .T.,;
          &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
  lcItmMask = &lcTmpBom..citmmask        
  *-- divid by No of primary fabrics
  lnRecNo = RECNO(lcTmpBom)          
  *--to get only FABRIC 
  SELECT (lcTmpBom)
  lnCurRecNo = RECNO(lcTmpBom)          
  lcCurItem = ALLTRIM(Item)
  lcIclr = Iclr  
  lnCntRec = 0
  lnTotQty = 0
  lnCurWastg = 0
  lnTPrmQty  = 0
  lnCount  = 1
  SELECT (lcTmpBom)
  DIMENSION laFabric[1,2]
  SCAN 
    IF &lcTmpBom..lprimary AND (cCatgtyp = 'F')  AND (lcIclr = Iclr)
      IF  ASCAN(laFabric,ALLTRIM(Item))=0
        DIMENSION laFabric[lnCount,2]
        laFabric[lnCount,1]   = ALLTRIM(Item)
        laFabric[lnCount,2]   = 1
        lnCount = 1 +lnCount
      ELSE
        laFabric[lnCount-1,2]   = laFabric[lnCount-1,2] + 1
      ENDIF  
      *lnTotQty = IIF(!EMPTY(&lcTmpBom..npruntqty),&lcTmpBom..npruntqty,lnTotQty)
      lnTotQty = IIF(!EMPTY(&lcTmpBom..npruntqty),&lcTmpBom..npruntqty,&lcTmpBom..nbomtotqty)
      lnCurWastg = IIF(EMPTY(lnCurWastg),&lcTmpBom..nbomwastge,lnCurWastg)
      IF !EMPTY(&lcTmpBom..npruntqty  )
        lnTPrmQty  =&lcTmpBom..npruntqty  
      ENDIF
    ENDIF
  ENDSCAN

  lnAscn = ASCAN(laFabric,ALLTRIM(lcCurItem))
  lnPos = IIF(lnAscn>1,CEILING(lnAscn/2),1)
  lnCntRec = ALEN(laFabric,1)

  SELECT (lcTmpBom)
  LOCATE
  lnPCount = 0
  SCAN FOR ccatgtyp = 'F'
    IF &lcTmpBom..lprimary AND (lcIclr = Iclr)
      lnOld = &lcTmpBom..TotCost   
      REPLACE &lcTmpBom..nEstBomQty WITH (lnTotQty*100/(lnCurWastg+100))/IIF(lnCntRec=0,1,lnCntRec),;
              &lcTmpBom..nbomtotqty WITH (lnTotQty/IIF(lnCntRec=0,1,lnCntRec)),;
              &lcTmpBom..npruntqty  WITH lnTPrmQty,;
              &lcTmpBom..TotCost    WITH &lcTmpBom..untcost*&lcTmpBom..nbomtotqty,;
              &lcTmpBom..nbomwastge WITH lnCurWastg,;
              &lcTmpBom..cStatus    WITH IIF(&lcTmpBom..cStatus = "S" OR EMPTY(&lcTmpBom..cStatus), "M" , &lcTmpBom..cStatus)
      =lfUpdCost(1 , lnOld)              
      lnPCount = lnPCount+1
    ENDIF  
  ENDSCAN
  lnTotalFab = IIF(lnTPrmQty>0,lnTPrmQty,lnNUntQty7)
  LOCATE FOR &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND   lcItmMask = &lcTmpBom..citmmask AND &lcTmpBom..lFrstprm
  IF !FOUND()
    LOCATE FOR &lcTmpBom..lprimary AND (cCatgtyp = 'F') AND   lcItmMask = &lcTmpBom..citmmask 
    IF FOUND()
      REPLACE &lcTmpBom..lFrstprm WITH .T.,;
              &lcTmpBom..npruntqty WITH lnTotalFab
    ENDIF          
  ENDIF
  
  SELECT (lcTmpBom)
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT()
    GOTO lnRecNo
  ENDIF
 
ENDIF
*:**************************************************************************
*: Name          : lfSOENBPRC
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to Enable Percentage Field
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOENBPRC()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOENBPRC

IF lcPricCur <> lcDutyCur
  SHOW GET lnNCstPrc&lcNo ENABLE
  _CUROBJ = OBJNUM(lnNCstPrc&lcNo)
ENDIF


*:**************************************************************************
*: Name          : lfSOGTCOLR
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function Get Spacefic color
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOGTCOLR()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOGTCOLR

STORE '' TO laSegSorc&lcNo
FOR lnCount = 1 TO ALEN(laColors10,1)
  laSegSorc&lcNo[lnCount] = laColors10[lnCount,1]
ENDFOR  

*:**************************************************************************
*: Name          : lfPOISSUPD
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function add menu bar for issing screen
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfPOISSUPD()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037959,1.
*:**************************************************************************
*
FUNCTION lfPOISSUPD
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Style PO Issuing Screen'  
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfPoIssScr IN Alnmain
*-- end of lfPOISSUPD.

*!*************************************************************
*! Name      : lfAddOptPd
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfAddOptPd()
*!*************************************************************
*: Due to  C#037959,1.
*:**************************************************************************
FUNCTION lfAddOptPd

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT "Style PO Issuing Screen" 
ON SELECTION POPUP _OPTIONPOP DO lfPoIssScr IN Alnmain

*:**************************************************************************
*: Name          : lfPoIssScr
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function add menu bar for issing screen
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfPoIssScr()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037959,1.
*:**************************************************************************
*
FUNCTION lfPoIssScr

IF  Empty(laData[1]) 
  RETURN
ENDIF

*-- Call the Po cost sheet program.
llNoShow    = .F.
llCallShow  = .T.
llShow      = .F.

lcParameter = "'" + laData[1] + "'"
PUSH KEY
ON KEY
DO gpDoProg WITH "AWRPOIMAT", .F., "PO", lcParameter
POP KEY
llShow = .F.    
STORE .F. TO laScrMode
laScrMode[2]=.T.
=gfStatic()                      && Save inviroment before terminate
*:**************************************************************************
*: Name          : lfRELOPPAD
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to release option pad
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfRELOPPAD()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037959,1.
*:**************************************************************************
*
FUNCTION lfRELOPPAD

RELEASE PAD _Option OF _MSYSMENU
*:**************************************************************************
*: Name          : lfALEPOSUM
*: Developer     : Mohamed Shokry
*: Date          : 010/10/2004
*: Purpose       : Function to Sum Correct Duty
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfALEPOSUM()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037960,1.
*:**************************************************************************
*
FUNCTION lfALEPOSUM

=SEEK(lcimtyp+'1'+lctktno)
SUM REST ItemAmt TO lnDutyAmt WHILE ;
cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=lcimtyp+'1'+lctktno ;
FOR cCatgTyp ='D' AND Style = lcStyle
=SEEK(lcimtyp+'1'+lctktno)

SUM REST nPercent TO lnPercent WHILE ;
cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=lcimtyp+'1'+lctktno ;
FOR cCatgTyp ='D' AND Style = lcStyle

lnDutyAmt = lnDutyAmt*100/lnPercent

*:**************************************************************************
*: Name          : lfSOUPCTPK
*: Developer     : Mohamed Shokry
*: Date          : 010/10/2004
*: Purpose       : Function to Update CutPick in cases of So 
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOUPCTPK()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfSOUPCTPK

llDispMess  = gfGetMemVar('M_DISPSOPO')   && setting Display So/Po Alocation message
IF llDispMess
  RETURN .F.
ENDIF

IF m.Qty&lcSize < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcSize = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF m.Qty&lcSize <> &lcOrdLine..Qty&lcSize AND !llStores

  IF m.Picked AND !EMPTY(m.PikTKt) AND m.Qty&lcSize < m.Pik&lcSize
    *E300420,1 Message : 32015
    *E300420,1 Quantity cannot be below than what has been picked for this style/color
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32015B00000','ALERT')
    m.Qty&lcSize = &lcOrdLine..Qty&lcSize
    RETURN
  ENDIF
  *B802805,1 AMM Check the sold out date of the style edited its quantity
  *B802805,1 AMM Message :Style sold out date is XX/XX/XXXX
  *B802805,1 AMM   < Accept >  < Reenter >
  *C200587,4  TMI [Start] Disable this check for David Luck
  IF ASCAN(laEvntTrig,PADR('CHKENTRD',10))=0 .AND. !EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
       gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2
    m.Qty&lcSize = &lcOrdLine..Qty&lcSize
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF

  IF m.Qty&lcSize < Cut&lcSize
    lnAlias    = SELECT()
    IF !gfUserPriv('MF','MFCUTKT')
      llIgnorAll = .T.  
      *-- Message: This order has an allocated quantity, you may need to edit the 
      *-- quantities on the cutting ticket, but the access to do this is denied.'
      =gfModalGen('INM32075B00000','DIALOG')
    ENDIF
    IF 'PO' $ gcCmpModules
      =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
    ENDIF
    IF 'MF' $ gcCmpModules
      =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
    ENDIF

    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')

    SET ORDER TO TAG 'CUTORD' IN (lcAlocated)
    IF !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcAlocated) .AND. ;
       SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),'CutPick')
      SELECT CutPick
      SCAN REST WHILE TranCd+Order+cOrdLine = ;
        IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
        SCATTER TO laCutPick
        INSERT INTO (lcAlocated) FROM ARRAY laCutPick
      ENDSCAN
    ENDIF

    =gfCloseFile('CUTPICK')
    
    =SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcAlocated)
    
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE TotCut WITH TotCut - &lcAlocated..Qty&lcSize + m.Qty&lcSize
    UNLOCK
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE Cut&lcSize WITH Cut&lcSize - ;
            &lcAlocated..Qty&lcSize + m.Qty&lcSize ,;
            TotCut WITH TotCut - &lcAlocated..Qty&lcSize + m.Qty&lcSize
            
    UNLOCK
    SELECT (lcAlocated)
    =RLOCK()
    REPLACE TotQty         WITH TotQty - Qty&lcSize + m.Qty&lcSize ,;
            Qty&lcSize WITH m.Qty&lcSize 
            
    llUpdCTkt = (TranCd='1' .AND. SEEK(CTKTNO,'CUTTKTH') .AND. ;
                 CUTTKTH.Status = 'H') .OR. ;
                (TranCd='2' .AND. SEEK('P'+CTKTNO,'POSHDR') .AND. ;
                 POSHDR.Status='H' )

    IF llUpdCTkt .AND. !(lcSize $ cUpdSizes)
      REPLACE cUpdSizes WITH ALLTRIM(cUpdSizes)+lcSize
    ENDIF
    
    IF !llUpdCTkt .AND. (lcSize $ cUpdSizes)
      REPLACE cUpdSizes WITH STRTRAN(cUpdSizes,lcSize,'')
    ENDIF
    UNLOCK
    SELECT (lnAlias)
  ENDIF
ENDIF

*-- IF the user insert qty. in a size not included in the prepack sizes I get the prepack qty.= Zero
lnScalRec = RECNO('Scale')
= SEEK('P'+m.Scale+m.Prepak,'Scale')      
SCATTER FIELDS PP1,PP2,PP3,PP4,PP5,PP6,PP7,PP8 TO laPPq
FOR I = 1 TO 8
  lcSz = STR(I,1)
  IF SYS(18) = 'QTY' + STR(I,1)
    *-- if the size not included in the prepack (his Qty.=Zero) .and. the user input qty. in 
    *-- this size , in this case the order's sizes not compatible with the prepack's Sizes so
    *-- The prepack's Qty. becomes Zero
    IF laPPq[I] = 0 AND m.Qty&LCSZ > 0
      REPLACE &lcOrdLine..PPQty WITH 0 
      m.PPQTY = 0
      SHOW GET m.PPQty
    ENDIF
  ENDIF
ENDFOR
GOTO lnScalRec IN Scale	

RETURN .T.

*:**************************************************************************
*: Name          : lfSOUPSTY
*: Developer     : Mohamed Shokry
*: Date          : 010/10/2004
*: Purpose       : Function to Update Style File
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOUPCTPK()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfSOUPSTY

llDispMess  = gfGetMemVar('M_DISPSOPO')   && setting Display So/Po Alocation message
IF llDispMess
  lnAlias    = SELECT()
  SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
  SELECT (lcAlocated)
  GO TOP
  IF !EOF()
    WAIT 'Updating alocated quantity...' WINDOW NOWAIT
    IF 'PO' $ gcCmpModules  && if we have style purchase order.
      =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
      =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
    ENDIF
    IF 'MF' $ gcCmpModules && if we have munf. module.
      =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
      =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
    ENDIF
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPKORD','SH')
    
    PRIVATE lcOrdLnOrd , lnOrdLnRec
    SELECT (lcOrdLine)
    lnOrdLnRec = RECNO()
    lcOrdLnOrd = TAG()
    SET ORDER TO TAG OrdLine    
    
    SELECT (lcAlocated)
    DO WHILE !EOF()
      lcKey = TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE
      SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE = lcKey
        lcHdrAlo = IIF(TranCd='1','CutTktH','PosHdr')
        lcDetAlo = IIF(TranCd='1','CutTktL','PosLn')
        =SEEK(lcKey,'CUTPICK')
        SCATTER MEMVAR
        SELECT (lcHdrAlo)
        =SEEK(IIF(m.TranCd='1',m.CTKTNO,'P'+m.CTKTNO))
        =RLOCK()
        IF &lcAlocated..nSteps < 1
          REPLACE TotOrd WITH TotOrd - CUTPICK.TotQty + m.TotQty
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
          SELECT (lcHdrAlo)
        ENDIF
        IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 2
          
          PRIVATE lnQtyUpd , lnLoop , lcLoopStr , lnUpdSNo
          lnQtyUpd = CutPick.TotQty  
          lnUpdSNo = LEN(ALLTRIM(&lcAlocated..cUpdSizes))
          FOR lnLoop = 1 TO lnUpdSNo 
            lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
            lcLoopStr = ALLTRIM(lcLoopStr)
            lnQtyUpd = lnQtyUpd - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
          ENDFOR
          
          IF m.TranCd = '1'
            REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty + lnQtyUpd ,;
                    Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty + lnQtyUpd
          ELSE
            REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty + lnQtyUpd ,;
                    Open      WITH Open  - CUTPICK.TotQty + lnQtyUpd
          ENDIF

          IF SEEK(&lcAlocated..Style,'Style')
            PRIVATE lnLoop , lcLoopStr
            FOR lnLoop = 1 TO lnUpdSNo
              lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
              lcLoopStr = ALLTRIM(lcLoopStr)
              SELECT Style
              =RLOCK()
              REPLACE WIP&lcLoopStr WITH WIP&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      TOTWIP WITH TOTWIP - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      NWO&lcLoopStr WITH NWO&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ; 
                      NTOTWO WITH NTOTWO - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
              UNLOCK
            ENDFOR
          ENDIF  
          IF SEEK(lcOrdType+&lcAlocated..Order+PADL(ALLTRIM(&lcAlocated..cOrdLine),6),lcOrdLine) AND ;
             SEEK(&lcAlocated..Style + &lcOrdLine..cWareCode  ,'StyDye')
            PRIVATE lnLoop , lcLoopStr
            FOR lnLoop = 1 TO lnUpdSNo
              lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
              lcLoopStr = ALLTRIM(lcLoopStr)
              SELECT StyDye
              =RLOCK()
              REPLACE WIP&lcLoopStr WITH WIP&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      TOTWIP WITH TOTWIP - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                      NWO&lcLoopStr WITH NWO&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ; 
                      NTOTWO WITH NTOTWO - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
              UNLOCK
            ENDFOR
          ENDIF  

          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
          SELECT (lcHdrAlo)
        ENDIF
        UNLOCK
        SELECT (lcDetAlo)
        =SEEK(IIF(m.TranCd='2','P','')+m.CTKTNO+m.Style+m.cTktLineNo+'1')
        =RLOCK()
        IF &lcAlocated..nSteps < 3
          REPLACE ORD1   WITH ORD1   - CUTPICK.Qty1   + m.Qty1 ,;
                  ORD2   WITH ORD2   - CUTPICK.Qty2   + m.Qty2 ,;
                  ORD3   WITH ORD3   - CUTPICK.Qty3   + m.Qty3 ,;
                  ORD4   WITH ORD4   - CUTPICK.Qty4   + m.Qty4 ,;
                  ORD5   WITH ORD5   - CUTPICK.Qty5   + m.Qty5 ,;
                  ORD6   WITH ORD6   - CUTPICK.Qty6   + m.Qty6 ,;
                  ORD7   WITH ORD7   - CUTPICK.Qty7   + m.Qty7 ,;
                  ORD8   WITH ORD8   - CUTPICK.Qty8   + m.Qty8 ,;
                  TOTORD WITH TOTORD - CUTPICK.TotQty + m.TotQty
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 3
          UNLOCK
          SELECT (lcDetAlo)
        ENDIF
        IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 4
          
          REPLACE Qty1   WITH IIF('1' $ &lcAlocated..cUpdSizes,Qty1-CUTPICK.Qty1+m.Qty1,Qty1) ,;
                  Qty2   WITH IIF('2' $ &lcAlocated..cUpdSizes,Qty2-CUTPICK.Qty2+m.Qty2,Qty2) ,;
                  Qty3   WITH IIF('3' $ &lcAlocated..cUpdSizes,Qty3-CUTPICK.Qty3+m.Qty3,Qty3) ,;
                  Qty4   WITH IIF('4' $ &lcAlocated..cUpdSizes,Qty4-CUTPICK.Qty4+m.Qty4,Qty4) ,;
                  Qty5   WITH IIF('5' $ &lcAlocated..cUpdSizes,Qty5-CUTPICK.Qty5+m.Qty5,Qty5) ,;
                  Qty6   WITH IIF('6' $ &lcAlocated..cUpdSizes,Qty6-CUTPICK.Qty6+m.Qty6,Qty6) ,;
                  Qty7   WITH IIF('7' $ &lcAlocated..cUpdSizes,Qty7-CUTPICK.Qty7+m.Qty7,Qty7) ,;
                  Qty8   WITH IIF('8' $ &lcAlocated..cUpdSizes,Qty8-CUTPICK.Qty8+m.Qty8,Qty8) ,;
                  TotQty WITH TotQty - CUTPICK.TotQty + lnQtyUpd                  

          SELECT (lcAlocated)
          =RLOCK()
          REPLACE nSteps WITH 4
          UNLOCK
          SELECT (lcDetAlo)
        ENDIF
        UNLOCK
        SELECT CUTPICK
        =RLOCK()
        IF m.TOTQTY = 0
          DELETE
        ELSE
          =RLOCK()
          GATHER MEMVAR
          UNLOCK
        ENDIF
        UNLOCK
      ENDSCAN
    ENDDO

    =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
    =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
    =gfCloseFile('CUTPICK')

    SELECT (lcOrdLine)
    SET ORDER TO TAG &lcOrdLnOrd
    IF BETWEEN(RECNO(),1,RECCOUNT())
      GOTO lnOrdLnRec
    ENDIF
  ENDIF
ELSE
  SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
  SELECT (lcAlocated)
  GO TOP
  IF !EOF()
    WAIT 'Updating alocated quantity...' WINDOW NOWAIT
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPKORD','SH')

    SELECT (lcAlocated)
    SCAN
      SCATTER MEMVAR
      =SEEK(TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE,'CUTPICK')
      SELECT CUTPICK
      =RLOCK()
      IF m.TOTQTY = 0
        DELETE
      ELSE
        =RLOCK()
        GATHER MEMVAR
        UNLOCK
      ENDIF
      UNLOCK
    ENDSCAN
  ENDIF
ENDIF

*:**************************************************************************
*: Name          : lfPOUPCTPK
*: Developer     : Mohamed Shokry
*: Date          : 010/10/2004
*: Purpose       : Function to Update CutPick in cases of PO
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfPOUPCTPK()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfPOUPCTPK

llDispMess  = gfGetMemVar('M_DISPSOPO')   && setting Display So/Po Alocation message
IF llDispMess
  RETURN .F.
ENDIF

IF lcProgName = "POSTREC"
   lcFldNo = STR(lnCnxt,1)
   lcPOline = lcTmpLine
ELSE
   lcFldNo= lcSz
ENDIF

lnAlias    = SELECT()
lcAloPQty = ALLTRIM(STR(&lcPOline..Ord&lcFldNo,6))
lcStr     = ALLTRIM(SCALE.Sz&lcFldNo) + "|" +lcAloPQty

IF lcProgName = "POSTREC"
  lcCtPkKey = '2'+PO+&lcPOline..Style
ELSE
  lcCtPkKey = '2'+laData[1]+&lcPOline..Style
ENDIF
IF !USED(lcTmpCtPk)
  *-Create Temp Cut Pick file. 
  SELECT CUTPICK
  =AFIELDS(laFStru)
  lnFStru = ALEN(laFStru,1)
  DIMENSION laFStru[lnFStru+9,4]
  
  FOR I=1 TO 8 
   laFStru[lnFStru+I,1] = 'nCurPck'+STR(I,1)
   laFStru[lnFStru+I,2] = 'N'
   laFStru[lnFStru+I,3] = 6
   laFStru[lnFStru+I,4] = 0
  ENDFOR
  
  IF lcProgName = "POSTREC"
   laFStru[lnFStru+9,1] = 'nCurPck9'
   laFStru[lnFStru+9,2] = 'N'
   laFStru[lnFStru+9,3] = 7
   laFStru[lnFStru+9,4] = 0
  ELSE
    laFStru[lnFStru+9,1] = 'nTotCur'
    laFStru[lnFStru+9,2] = 'N'
    laFStru[lnFStru+9,3] = 7
    laFStru[lnFStru+9,4] = 0
  ENDIF
  
  =gfCrtTmp(lcTmpCtPk,@laFStru,'Trancd+cTktNo+Style+cOrdLine+Order',lcTmpCtPk)
  SELECT (lcTmpCtPk)      
  SET ORDER TO 1
ENDIF

lcSzNo     = lcFldNo
lnNewVal   = m.Qty&lcSzNo 
IF lcProgName = "POSTREC"
  lnNewVal =   laCan[lnCnxt]
ENDIF
SELECT CUTPICK
=SEEK(lcCtPkKey)
lnCurRec = RECNO()
SCAN REST WHILE TranCd+cTktNo+Style=lcCtPkKey
  SCATTER TO laFields
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOdOrd
  IF !SEEK(lcCtPkKey+CUTPICK.cOrdLine+CUTPICK.Order,lcTmpCtPk)
    SELECT (lcTmpCtPk)
    APPEND BLANK
    GATHER FROM laFields

    IF lcProgName = "POSTREC"
      GATHER FROM laOdOrd FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9
    ELSE
      GATHER FROM laOdOrd FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nTotCur
    ENDIF
    
  ENDIF
ENDSCAN
SELECT CUTPICK
GOTO lnCurRec 

SELECT (lcPOline)

REPLACE TotOrd     WITh m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
        Ord&lcSzNo WITH  lnNewVal
        

SELECT (lcTmpCtPk)
=SEEK(lcCtPkKey+CUTPICK.cOrdLine+CUTPICK.Order)

IF lcProgName = "POSTREC"
  REPLACE TotQty     WITH TotQty - Qty&lcSzNo + MAX((Qty&lcSzNo - lnNewVal),0),;
          Qty&lcSzNo WITH MAX((Qty&lcSzNo - lnNewVal),0)
ELSE
  REPLACE TotQty     WITh m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
          Qty&lcSzNo WITH  lnNewVal
ENDIF
SELECT(lnAlias)
RETURN .T.

*:**************************************************************************
*: Name          : lfPOUPSTRC
*: Developer     : Mohamed Shokry
*: Date          : 010/10/2004
*: Purpose       : Function to Update Style File
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfPOUPSTRC()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfPOUPSTRC

llDispMess  = gfGetMemVar('M_DISPSOPO')   && setting Display So/Po Alocation message
IF !llDispMess
  IF llSOInstld AND !(lcPType $ 'RN') AND _CUROBJ = OBJNUM(laCan[lnCnxt]) 
    lcCtPkKey = IIF(llMfCall,'1'+&lcTmpLine..Cuttkt,'2'+&lcTmpLine..PO)+&lcTmpLine..Style
    IF SEEK(lcCtPkKey,'CutPick')
      lnAlias    = SELECT()
      lcFldNo = STR(lnCnxt,1)
      lcPOline = lcTmpLine
      lcAloPQty = ALLTRIM(STR(&lcPOline..Ord&lcFldNo,6))
      lcStr     = ALLTRIM(SCALE.Sz&lcFldNo) + "|" +lcAloPQty

      IF !USED(lcTmpCtPk)
        *-Create Temp Cut Pick file. 
        SELECT CUTPICK
        =AFIELDS(laFStru)
        lnFStru = ALEN(laFStru,1)
        DIMENSION laFStru[lnFStru+9,4]
  
        FOR I=1 TO 9 
         laFStru[lnFStru+I,1] = 'nCurPck'+STR(I,1)
         laFStru[lnFStru+I,2] = 'N'
         laFStru[lnFStru+I,3] = 6
         laFStru[lnFStru+I,4] = 0
        ENDFOR
        
        =gfCrtTmp(lcTmpCtPk,@laFStru,'Trancd+cTktNo+Style+cOrdLine+Order',lcTmpCtPk)
        SELECT (lcTmpCtPk)      
        SET ORDER TO 1

      ENDIF

      lcSzNo     = lcFldNo
      lnNewVal =   laCan[lnCnxt]

      SELECT CUTPICK
      SCAN REST WHILE TranCd+cTktNo+Style=lcCtPkKey
        SCATTER TO laFields 
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOdOrd
        IF !SEEK(lcCtPkKey+CUTPICK.cOrdLine+CUTPICK.Order,lcTmpCtPk)
          SELECT (lcTmpCtPk)
          APPEND BLANK
          GATHER FROM laFields

          GATHER FROM laOdOrd FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9
        ENDIF
      ENDSCAN
      SELECT (lcPOline)
      REPLACE TotOrd     WITH TotOrd - Ord&lcSzNo + MAX((Ord&lcSzNo - lnNewVal),0),;
              Ord&lcSzNo WITH MAX((Ord&lcSzNo - lnNewVal),0)
        

      SELECT (lcTmpCtPk)
      REPLACE TotQty     WITH TotQty - Qty&lcSzNo + MAX((Qty&lcSzNo - lnNewVal),0),;
              Qty&lcSzNo WITH MAX((Qty&lcSzNo - lnNewVal),0)

      SELECT(lnAlias)
    ENDIF  
  ENDIF
ENDIF

*:**************************************************************************
*: Name          : lfSOCHKSTY
*: Developer     : Mohamed Shokry
*: Date          : 010/10/2004
*: Purpose       : Function to Check repeated or not File
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOCHKSTY()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfSOCHKSTY
PRIVATE lcCurSty,lnRecNo

llDispMess  = gfGetMemVar('M_DISPSOPO')   && setting Display So/Po Alocation message
IF llDispMess
  RETURN .F.
ENDIF

IF laScrMode [3] .AND. !lladdline .AND. Style <> m.Style
  lcCurSty = &lcOrdLine..Style
  lnRecNo = RECNO()
  lnCountr = 0
  LOCATE
  SCAN FOR Style =lcCurSty
    lnCountr = lnCountr+1
  ENDSCAN
  IF lnCountr > 1
    lcMsg2 = 'Style (' + lcCurSty+ ') found in another line, Please be sure to update all lines with style (' +m.style + ')'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  ENDIF 
ENDIF
*:**************************************************************************
*: Name          : lfSOCHGSTY
*: Developer     : Mohamed Shokry
*: Date          : 10/10/2004
*: Purpose       : Function to Change style for all req. files
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOCHKSTY()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfSOCHGSTY
PRIVATE lcCurSty,lnRecNo,lcCutOrd,lcStyOrd,lcOrdBomOr,lcBomLnOrd,lnAlias

llDispMess  = gfGetMemVar('M_DISPSOPO')   && setting Display So/Po Alocation message
IF llDispMess
  RETURN .F.
ENDIF
lnAlias = SELECT (0)
=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPKORD','SH')
=gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
=gfOpenFile(gcDataDir+'BOMLINE',gcDataDir+'BOMLINE','SH')
=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
=gfOpenFile(gcDataDir+'ORDBOM',gcDataDir+'ORDBOM','SH')
=gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')


SELECT CUTPICK
lcCutOrd = ORDER()
SET ORDER TO Cutord

SELECT STYINVJL
lcStyOrd= ORDER()
SET ORDER TO Mfgopr

SELECT ORDBOM
lcOrdBomOr = ORDER()
SET ORDER TO STYBOM

SELECT BOMLINE
lcBomLnOrd = ORDER()
SET ORDER TO Mfgopr
SELECT (lcOrdLine)
SCAN
  IF !SEEK(cordtype+order+STR(lineno,6),'ORDLINE')
    LOOP
  ENDIF
  IF Style <> OrdLine.Style
    *--update order Cancelation 
    IF SEEK(cordtype+order+STR(lineno,6),'ORDCANLN')
      IF OrdCanLn.Style <> &lcOrdLine..Style
        REPLACE OrdCanLn.Style WITH &lcOrdLine..Style
      ENDIF
    ENDIF
    
    **-- Update Sales Order Cost Sheet
    llflgCLr1 = .F.
    llflgCLr2 = .F.
    IF SEEK(PADR(PADR(OrdLine.Style,lnMajorLen),19)+ORDLINE.order,'ORDBOM')
      SELECT ORDBOM
      lcColor = SUBSTR(OrdLine.Style,lnMajorLen+2) 
      SCAN REST WHILE style+order+STR(lineno,6) = PADR(PADR(OrdLine.Style,lnMajorLen),19)+OrdLine.order
         IF SUBSTR(cItmMask,lnMajorLen+2) ='******'
           llflgCLr1 = .T.
         ENDIF
         IF  !EMPTY(iclr) AND lcColor <> iclr
           llflgCLr2 = .T.
         ENDIF
      ENDSCAN
      =SEEK(PADR(PADR(OrdLine.Style,lnMajorLen),19)+ORDLINE.order,'ORDBOM')
      SCAN FOR style+order+STR(lineno,6) = PADR(PADR(OrdLine.Style,lnMajorLen),19)+OrdLine.order
        IF SUBSTR(cItmMask,lnMajorLen+2) <> '******' AND cItmMask <>OrdLine.Style
          LOOP
        ENDIF        
        lnCurRec = RECNO()
        *REPLACE Style     WITH  PADR(PADR(&lcOrdLine..Style,lnMajorLen),19),;
                cItmMajor WITH  PADR(PADR(&lcOrdLine..Style,lnMajorLen),19)
        REPLACE cItmMajor WITH  PADR(PADR(&lcOrdLine..Style,lnMajorLen),19)
        DO CASE
            *--if *** found and no colors(user assign P.P.)
          CASE llflgCLr1 AND ! llflgCLr2
             IF SUBSTR(cItmMask,lnMajorLen+2) ='******'
               REPLACE cItmMask  WITH PADR(&lcOrdLine..Style,lnMajorLen)+'-'+'******'
             ELSE
               REPLACE cItmMask  WITH &lcOrdLine..Style,;
                       iclr      WITH lcColor 
             ENDIF
          
          *--if *** Not found and no colors(user assign P.P.)
          CASE !llflgCLr1 AND llflgCLr2
               REPLACE cItmMask  WITH &lcOrdLine..Style
        
          CASE llflgCLr1 AND  llflgCLr2
             IF SUBSTR(cItmMask,lnMajorLen+2) ='******'
               REPLACE cItmMask  WITH PADR(&lcOrdLine..Style,lnMajorLen)+'-'+'******'
             ELSE
               REPLACE cItmMask  WITH &lcOrdLine..Style,;
                       iclr      WITH lcColor 
             ENDIF
        ENDCASE
        IF lnCurRec  > 0 .AND. lnCurRec <= RECCOUNT()
          GOTO lnCurRec 
        ENDIF
      ENDSCAN
      LOCATE
      REPLACE ALL STYLE WITH PADR(PADR(&lcOrdLine..Style,lnMajorLen),19);
              FOR style+order+STR(lineno,6) = PADR(PADR(OrdLine.Style,lnMajorLen),19)+OrdLine.order
    ENDIF

    *--update cutpick file
    SELECT (lcOrdLine)
    IF SEEK("2"+order+STR(lineno,6),'CUTPICK')
      IF CUTPICK.Style <> &lcOrdLine..Style
        REPLACE CUTPICK.Style WITH &lcOrdLine..Style
      ENDIF
      *--update PoFile
      IF SEEK("P"+CUTPICK.ctktno+OrdLine.Style,'POSLN')
        IF POSLN.Style <> &lcOrdLine..Style
          SELECT POSLN
          =lfRebalPo(OrdLine.Style,&lcOrdLine..Style,&lcOrdLine..cWareCode)
          =SEEK("P"+CUTPICK.ctktno+OrdLine.Style,'POSLN')
          REPLACE ALL STYLE WITH &lcOrdLine..Style FOR cstytype+po+style+STR(lineno,6)+trancd= "P"+CUTPICK.ctktno+OrdLine.Style
        ENDIF
      ENDIF
      IF SEEK(CUTPICK.ctktno,'STYINVJL')
        SELECT STYINVJL
        SCAN FOR ctrcode+coprcode+clotno+ctrtype+style+cwarecode = CUTPICK.ctktno
          IF Style = OrdLine.Style
            lnCurrRec = RECNO()
            REPLACE Style WITH &lcOrdLine..Style
            *--call repalance the program
            =lfRebalnce(OrdLine.Style,&lcOrdLine..Style,&lcOrdLine..cWareCode)
            SELECT STYINVJL
            IF lnCurrRec > 0 .AND. lnCurrRec <= RECCOUNT()
              GOTO lnCurrRec
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF

      *--update BomLine File 
      IF SEEK('I'+CUTPICK.ctktno,'BOMLINE')
        SELECT BOMLINE
        REPLACE ALL Style WITH &lcOrdLine..Style FOR cimtyp+ctktno+coprcode+ctype+style+sclr+IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr = 'I'+CUTPICK.ctktno ;
          AND (Style = OrdLine.Style)
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

SELECT CUTPICK
SET ORDER TO &lcCutOrd

SELECT STYINVJL
SET ORDER TO &lcStyOrd

SELECT ORDBOM
SET ORDER TO &lcOrdBomOr

SELECT BOMLINE
SET ORDER TO &lcBomLnOrd

SELECT (lnAlias)

*:**************************************************************************
*: Name          : lfRebalnce
*: Developer     : Mohamed Shokry
*: Date          : 10/10/2004
*: Purpose       : Function to Rebalance the programs
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfRebalnce()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfRebalnce
PARAMETER lcOldSty,lcNewSty,lcWareCode

lnAlias = SELECT (0)
SELECT STYINVJL
lcStyOrd= ORDER()
SET ORDER TO Styinvjl
*first scan
SELECT STYDYE
IF SEEK(lcOldSty+lcWareCode)
    REPLACE Stk1 WITH 0 ,;
            Stk2 WITH 0,;
            Stk3 WITH 0,;
            Stk4 WITH 0,;
            Stk5 WITH 0,;
            Stk6 WITH 0,;
            Stk7 WITH 0,;
            Stk8 WITH 0,;
          TotStk WITH 0
ENDIF
SELECT STYLE
IF SEEK(lcOldSty)
    REPLACE Stk1   WITH 0,;
            Stk2   WITH 0,;
            Stk3   WITH 0,;
            Stk4   WITH 0,;
            Stk5   WITH 0,;
            Stk6   WITH 0,;
            Stk7   WITH 0,;
            Stk8   WITH 0,;
          TotStk   WITH 0,;
          nStkVal  WITH 0,;
          Ave_Cost WITH 0
ENDIF

IF SEEK(lcOldSty+lcWareCode,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =lcOldSty+lcWareCode
      SELECT STYDYE
      REPLACE Stk1 WITH Stk1   + STYINVJL.nStk1 ,;
              Stk2 WITH Stk2   + STYINVJL.nStk2 ,;
              Stk3 WITH Stk3   + STYINVJL.nStk3 ,;
              Stk4 WITH Stk4   + STYINVJL.nStk4 ,;
              Stk5 WITH Stk5   + STYINVJL.nStk5 ,;
              Stk6 WITH Stk6   + STYINVJL.nStk6 ,;
              Stk7 WITH Stk7   + STYINVJL.nStk7 ,;
              Stk8 WITH Stk8   + STYINVJL.nStk8 ,;
              TotStk WITH TotStk + STYINVJL.nTotStk
      SELECT STYLE
      REPLACE Stk1 WITH Stk1   + STYINVJL.nStk1 ,;
              Stk2 WITH Stk2   + STYINVJL.nStk2 ,;
              Stk3 WITH Stk3   + STYINVJL.nStk3 ,;
              Stk4 WITH Stk4   + STYINVJL.nStk4 ,;
              Stk5 WITH Stk5   + STYINVJL.nStk5 ,;
              Stk6 WITH Stk6   + STYINVJL.nStk6 ,;
              Stk7 WITH Stk7   + STYINVJL.nStk7 ,;
              Stk8 WITH Stk8   + STYINVJL.nStk8 ,;
              TotStk WITH TotStk + STYINVJL.nTotStk,;
              nStkVal WITH nStkVal + STYINVJL.nStkVal,;
              Ave_Cost WITH  IIF(TotStk=0,Ave_Cost ,ABS(nStkVal/TotStk)) 
  ENDSCAN
ENDIF

SELECT STYDYE
IF SEEK(lcNewSty+lcWareCode)
    REPLACE Stk1 WITH 0 ,;
            Stk2 WITH 0,;
            Stk3 WITH 0,;
            Stk4 WITH 0,;
            Stk5 WITH 0,;
            Stk6 WITH 0,;
            Stk7 WITH 0,;
            Stk8 WITH 0,;
          TotStk WITH 0
ELSE
  APPEND BLANK
  REPLACE STYLE     WITH lcNewSty,;
          cWareCode WITH lcWareCode
ENDIF
SELECT STYLE
IF SEEK(lcNewSty)
    REPLACE Stk1   WITH 0,;
            Stk2   WITH 0,;
            Stk3   WITH 0,;
            Stk4   WITH 0,;
            Stk5   WITH 0,;
            Stk6   WITH 0,;
            Stk7   WITH 0,;
            Stk8   WITH 0,;
          TotStk   WITH 0,;
          nStkVal  WITH 0,;
          Ave_Cost WITH 0
ENDIF

IF SEEK(lcNewSty+lcWareCode,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =lcNewSty+lcWareCode
      SELECT STYDYE
      REPLACE Stk1 WITH Stk1   + STYINVJL.nStk1 ,;
              Stk2 WITH Stk2   + STYINVJL.nStk2 ,;
              Stk3 WITH Stk3   + STYINVJL.nStk3 ,;
              Stk4 WITH Stk4   + STYINVJL.nStk4 ,;
              Stk5 WITH Stk5   + STYINVJL.nStk5 ,;
              Stk6 WITH Stk6   + STYINVJL.nStk6 ,;
              Stk7 WITH Stk7   + STYINVJL.nStk7 ,;
              Stk8 WITH Stk8   + STYINVJL.nStk8 ,;
              TotStk WITH TotStk + STYINVJL.nTotStk
      SELECT STYLE
      REPLACE Stk1 WITH Stk1   + STYINVJL.nStk1 ,;
              Stk2 WITH Stk2   + STYINVJL.nStk2 ,;
              Stk3 WITH Stk3   + STYINVJL.nStk3 ,;
              Stk4 WITH Stk4   + STYINVJL.nStk4 ,;
              Stk5 WITH Stk5   + STYINVJL.nStk5 ,;
              Stk6 WITH Stk6   + STYINVJL.nStk6 ,;
              Stk7 WITH Stk7   + STYINVJL.nStk7 ,;
              Stk8 WITH Stk8   + STYINVJL.nStk8 ,;
              TotStk WITH TotStk + STYINVJL.nTotStk,;
              nStkVal WITH nStkVal + STYINVJL.nStkVal,;
              Ave_Cost WITH  IIF(TotStk=0,Ave_Cost ,ABS(nStkVal/TotStk)) 
  ENDSCAN
ENDIF
SELECT STYINVJL
SET ORDER TO &lcStyOrd

SELECT (lnAlias)

*:**************************************************************************
*: Name          : lfRebalPo
*: Developer     : Mohamed Shokry
*: Date          : 10/10/2004
*: Purpose       : Function to Rebalance the Po
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfRebalPo()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C037957,1.
*:**************************************************************************
*
FUNCTION lfRebalPo
PARAMETER lcOldSty,lcNewSty,lcWareCode
lnAlias = SELECT (0)

SELECT POSLN
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = "P"+CUTPICK.ctktno+OrdLine.Style
  IF trancd = '1'
    lnWIP1 = Qty1
    lnWIP2 = Qty2
    lnWIP3 = Qty3
    lnWIP4 = Qty4
    lnWIP5 = Qty5
    lnWIP6 = Qty6
    lnWIP7 = Qty7
    lnWIP8 = Qty8
    lnTotWIP = TotQty

    lnWO1 = Qty1
    lnWO2 = Qty2
    lnWO3 = Qty3
    lnWO4 = Qty4
    lnWO5 = Qty5
    lnWO6 = Qty6
    lnWO7 = Qty7
    lnWO8 = Qty8
    lnTotWO = TotQty
  ELSE
    lnWIP1 = lnWIP1 - Qty1
    lnWIP2 = lnWIP2 - Qty2
    lnWIP3 = lnWIP3 - Qty3
    lnWIP4 = lnWIP4 - Qty4
    lnWIP5 = lnWIP5 - Qty5
    lnWIP6 = lnWIP6 - Qty6
    lnWIP7 = lnWIP7 - Qty7
    lnWIP8 = lnWIP8 - Qty8
    lnTotWIP = lnTotWIP- TotQty
   
  ENDIF

ENDSCAN

SELECT STYDYE
IF SEEK(lcOldSty+lcWareCode)
    REPLACE WIP1 WITH WIP1- lnWIP1 ,;
            WIP2 WITH WIP2- lnWIP2 ,;
            WIP3 WITH WIP3- lnWIP3 ,;
            WIP4 WITH WIP4- lnWIP4 ,;
            WIP5 WITH WIP5- lnWIP5 ,;
            WIP6 WITH WIP6- lnWIP6 ,;
            WIP7 WITH WIP7- lnWIP7 ,;
            WIP8 WITH WIP8- lnWIP8 ,;
          TotWIP WITH TOTWIP- lnTotWIP 

    REPLACE nWO1 WITH nWO1- lnWO1 ,;
            nWO2 WITH nWO2- lnWO2 ,;
            nWO3 WITH nWO3- lnWO3 ,;
            nWO4 WITH nWO4- lnWO4 ,;
            nWO5 WITH nWO5- lnWO5 ,;
            nWO6 WITH nWO6- lnWO6 ,;
            nWO7 WITH nWO7- lnWO7 ,;
            nWO8 WITH nWO8- lnWO8 ,;
          NTotWO WITH NTOTWO- lnTotWO 
ENDIF
SELECT STYDYE
IF SEEK(lcNewSty+lcWareCode)
    REPLACE WIP1 WITH WIP1+ lnWIP1 ,;
            WIP2 WITH WIP2+ lnWIP2 ,;
            WIP3 WITH WIP3+ lnWIP3 ,;
            WIP4 WITH WIP4+ lnWIP4 ,;
            WIP5 WITH WIP5+ lnWIP5 ,;
            WIP6 WITH WIP6+ lnWIP6 ,;
            WIP7 WITH WIP7+ lnWIP7 ,;
            WIP8 WITH WIP8+ lnWIP8 ,;
          TotWIP WITH TOTWIP+ lnTotWIP 

    REPLACE NWO1 WITH NWO1+ lnWO1 ,;
            NWO2 WITH NWO2+ lnWO2 ,;
            NWO3 WITH NWO3+ lnWO3 ,;
            NWO4 WITH NWO4+ lnWO4 ,;
            NWO5 WITH NWO5+ lnWO5 ,;
            NWO6 WITH NWO6+ lnWO6 ,;
            NWO7 WITH NWO7+ lnWO7 ,;
            NWO8 WITH NWO8+ lnWO8 ,;
          NTotWO WITH NTOTWO+ lnTotWO 
          
ELSE
  APPEND BLANK
  REPLACE STYLE     WITH lcNewSty,;
          cWareCode WITH lcWareCode

  REPLACE WIP1 WITH WIP1+ lnWIP1 ,;
          WIP2 WITH WIP2+ lnWIP2 ,;
          WIP3 WITH WIP3+ lnWIP3 ,;
          WIP4 WITH WIP4+ lnWIP4 ,;
          WIP5 WITH WIP5+ lnWIP5 ,;
          WIP6 WITH WIP6+ lnWIP6 ,;
          WIP7 WITH WIP7+ lnWIP7 ,;
          WIP8 WITH WIP8+ lnWIP8 ,;
        TotWIP WITH TOTWIP+ lnTotWIP 

    REPLACE NWO1 WITH NWO1+ lnWO1 ,;
            NWO2 WITH NWO2+ lnWO2 ,;
            NWO3 WITH NWO3+ lnWO3 ,;
            NWO4 WITH NWO4+ lnWO4 ,;
            NWO5 WITH NWO5+ lnWO5 ,;
            NWO6 WITH NWO6+ lnWO6 ,;
            NWO7 WITH NWO7+ lnWO7 ,;
            NWO8 WITH NWO8+ lnWO8 ,;
          NTotWO WITH NTOTWO+ lnTotWO 
          
ENDIF

SELECT STYLE

IF SEEK(lcOldSty)
    REPLACE WIP1 WITH WIP1- lnWIP1 ,;
            WIP2 WITH WIP2- lnWIP2 ,;
            WIP3 WITH WIP3- lnWIP3 ,;
            WIP4 WITH WIP4- lnWIP4 ,;
            WIP5 WITH WIP5- lnWIP5 ,;
            WIP6 WITH WIP6- lnWIP6 ,;
            WIP7 WITH WIP7- lnWIP7 ,;
            WIP8 WITH WIP8- lnWIP8 ,;
          TotWIP WITH TOTWIP- lnTotWIP 

    REPLACE NWO1 WITH NWO1- lnWO1 ,;
            NWO2 WITH NWO2- lnWO2 ,;
            NWO3 WITH NWO3- lnWO3 ,;
            NWO4 WITH NWO4- lnWO4 ,;
            NWO5 WITH NWO5- lnWO5 ,;
            NWO6 WITH NWO6- lnWO6 ,;
            NWO7 WITH NWO7- lnWO7 ,;
            NWO8 WITH NWO8- lnWO8 ,;
          NTotWO WITH NTOTWO- lnTotWO 
ENDIF
SELECT STYLE
IF SEEK(lcNewSty)
    REPLACE WIP1 WITH WIP1+ lnWIP1 ,;
            WIP2 WITH WIP2+ lnWIP2 ,;
            WIP3 WITH WIP3+ lnWIP3 ,;
            WIP4 WITH WIP4+ lnWIP4 ,;
            WIP5 WITH WIP5+ lnWIP5 ,;
            WIP6 WITH WIP6+ lnWIP6 ,;
            WIP7 WITH WIP7+ lnWIP7 ,;
            WIP8 WITH WIP8+ lnWIP8 ,;
          TotWIP WITH TOTWIP+ lnTotWIP 

    REPLACE NWO1 WITH NWO1+ lnWO1 ,;
            NWO2 WITH NWO2+ lnWO2 ,;
            NWO3 WITH NWO3+ lnWO3 ,;
            NWO4 WITH NWO4+ lnWO4 ,;
            NWO5 WITH NWO5+ lnWO5 ,;
            NWO6 WITH NWO6+ lnWO6 ,;
            NWO7 WITH NWO7+ lnWO7 ,;
            NWO8 WITH NWO8+ lnWO8 ,;
          NTotWO WITH NTOTWO+ lnTotWO 
          
ENDIF

SELECT (lnAlias)

*:**************************************************************************
*: Name          : lfSOATMTMN
*: Developer     : Mohamed Shokry
*: Date          : 06/10/2004
*: Purpose       : Function to create Auto Create Material PO's Option
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfSOATMTMN()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037892,1.
*:**************************************************************************
*
FUNCTION lfSOATMTMN

lnNextBar = 19
DEFINE BAR lnNextBar OF _INQURYPOP PROMPT 'Auto Create Material PO'  SKIP FOR !(laScrMode[3]) 
lnNextBar = lnNextBar+1

ON SELECTION BAR lnNextBar-1 OF _INQURYPOP DO lfAutMat IN ALNMAIN

*:**************************************************************************
*: Name          : lfAutMat
*: Developer     : Mohamed Shokry
*: Date          : 15/11/2004
*: Purpose       : Function to create Auto Create Material PO's Option
*:**************************************************************************
*: Called from   : The Main Program
*:**************************************************************************
*: Calls         : None.
*:**************************************************************************
*: Return        : None.
*:**************************************************************************
*: Example       : = lfAutMat()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
*
FUNCTION lfAutMat
PRIVATE lnAlias
lnAlias = SELECT (0)

IF !USED('ORDBOM')
  =gfOpenFile(gcDataDir+'ORDBOM',gcDataDir+'ORDBOM','SH')
ENDIF


SELECT ORDBOM
lcOldOrdr = ORDER()
SET ORDER TO TAG ORDBOM
LOCATE
IF !SEEK(OrdHdr.Order,'ORDBOM')
  lcMsg2 = 'No sales order cost sheet found for style'+&lcOrdLine..Style
  lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  
  SELECT ORDBOM
  SET ORDER TO &lcOldOrdr        
  SELECT (lnAlias)
  RETURN
ELSE
  SELECT (lcOrdLine)
  LOCATE
  SCAN
    llCheck = .F.
    SELECT ORDBOM
    =SEEK(OrdHdr.Order)
    SCAN REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode = OrdHdr.Order
      IF LEFT(Style,12) <> LEFT(&lcOrdLine..Style,12)
        LOOP
      ENDIF
      IF SUBSTR(citmmask,14,6)  <>'******' AND SUBSTR(cItmMask,14,6)<>SUBSTR(&lcOrdLine..Style,14,6) 
        LOOP
      ENDIF
      llCheck = .T.
      EXIT
    ENDSCAN  
  ENDSCAN
  IF !llCheck
    lcMsg2 = 'No sales order cost sheet found for style'+&lcOrdLine..Style
    lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
   
    SELECT ORDBOM
    SET ORDER TO &lcOldOrdr        
    SELECT (lnAlias)
    RETURN
  ENDIF  
  
  SELECT (lcOrdLine)
  LOCATE
  SELECT ORDBOM
  SET ORDER TO &lcOldOrdr        
  SELECT (lnAlias)
ENDIF
llOtFrmEdt = .F.
llOpenSCR = .F.
STORE SPACE(1) TO lcTmpQuery,lcMtBrowTt
lcTmpQuery = gfTempName()
lcTmpCutPk = gfTempName()
lcTmpRel   = gfTempName()
lcTmpSlAt  = gfTempName()
lcTmpSlAt2 = gfTempName()
lcTmpSlMal = gfTempName()
lcTmpPoNot = gfTempName()


lcTmpNewPo = gfTempName()
llChkRtrn  = .F.
llCurLoop = .F.
llCheckOpen = .F.

lnToOrdALo = 0
lnToOrdULo = 0

=lfCretTmp()
=lfGetData()
SELECT (lcTmpQuery)
GO TOP
IF EOF()
  = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "No materials have been assigned to this PO.")
  RETURN            
ENDIF

lcMtBrowTt = "Summary Screen"
lcDet_ttl = lcMtBrowTt
lcPrompt  = "S\<elect"
lcDisMul  = "Alocated Po"
lcRealWin = "Release Po"  
lcSlAtWin = "Auto Create PO"
lcNewBrow = "New Po's"


DO WHILE .T.
  
  IF llCurLoop
    EXIT
  ENDIF  
  =lfvBKToSum()
ENDDO 
ON KEY LABEL ALT+B

lnAlias = SELECT (0)

*!*************************************************************
*! Name      : lfActsBrow
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfActsBrow

SELECT &lcTmpQuery
GO TOP

lcBrfield1 = [cMark    :02  :H = ''         :R,] +;
             [Item     :10  :H = 'Item'     :R,] +;
             [cDesc    :20  :H = 'Desc'     :R,] +;
             [Iclr     :08  :H = 'Colour'   :R,] +;
             [REQ      :14  :H = 'Required' :R,] +;
             [Ordered  :14  :H = 'Ordered'  :R,]+;
             [ToOrder  :14  :H = 'To Order' :R]

BROWSE FIELDS &lcBrField1 ;
       WINDOW SOAUTOB   ;
       WHEN lfwBrows();
       VALID :F lfvBrowse() ;
       IN WINDOW SOAUTO ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       TITLE (lcMtBrowTt)

*!*************************************************************
*! Name      : lfwBrows
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfwBrows

SELECT (lcTmpQuery)
lnNewRec = RECNO()
REPLACE ALL cMark WITH ' '
GO lnNewRec
REPLACE cMark WITH ">"
SHOW WINDOW (lcMtBrowTt) REFRESH
lcPrompt = IIF(cSelect="�","\<Unselect","S\<elect")
SHOW GET pbSelect,1 PROMPT lcPrompt


*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrowse
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Passed Parameters  : lcFrmScr
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
*!
FUNCTION lfvBrowse

IF ALLTRIM(WONTOP()) # ALLTRIM(lcMtBrowTt)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.

*!*************************************************************
*! Name      : lfCretTmp
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To create the temporary file
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCretTmp()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfCretTmp

SELECT ORDBOM
=AFIELDS(laMlnTmp)
lnNewFld = ALEN(laMlnTmp,1)

DIMENSION laMlnTmp[lnNewFld+8,4]

laMlnTmp[lnNewFld + 1,1] = 'cMark'
laMlnTmp[lnNewFld + 1,2] = 'C'
laMlnTmp[lnNewFld + 1,3] = 1
laMlnTmp[lnNewFld + 1,4] = 0

laMlnTmp[lnNewFld + 2,1] = 'cSelect'
laMlnTmp[lnNewFld + 2,2] = 'C'
laMlnTmp[lnNewFld + 2,3] = 2
laMlnTmp[lnNewFld + 2,4] = 0

laMlnTmp[lnNewFld + 3,1] = 'cDesc'
laMlnTmp[lnNewFld + 3,2] = 'C'
laMlnTmp[lnNewFld + 3,3] = 20
laMlnTmp[lnNewFld + 3,4] = 0

laMlnTmp[lnNewFld + 4,1] = 'Req'
laMlnTmp[lnNewFld + 4,2] = 'N'
laMlnTmp[lnNewFld + 4,3] = 12
laMlnTmp[lnNewFld + 4,4] = 3

laMlnTmp[lnNewFld + 5,1] = 'Ordered '
laMlnTmp[lnNewFld + 5,2] = 'N'
laMlnTmp[lnNewFld + 5,3] = 12
laMlnTmp[lnNewFld + 5,4] = 3

laMlnTmp[lnNewFld + 6,1] = 'ToOrder'
laMlnTmp[lnNewFld + 6,2] = 'N'
laMlnTmp[lnNewFld + 6,3] = 12
laMlnTmp[lnNewFld + 6,4] = 3

laMlnTmp[lnNewFld + 7,1] = 'Content'
laMlnTmp[lnNewFld + 7,2] = 'C'
laMlnTmp[lnNewFld + 7,3] = 60
laMlnTmp[lnNewFld + 7,4] = 0

laMlnTmp[lnNewFld + 8,1] = 'Delete'
laMlnTmp[lnNewFld + 8,2] = 'C'
laMlnTmp[lnNewFld + 8,3] = 1
laMlnTmp[lnNewFld + 8,4] = 0

CREATE DBF (gcWorkDir+lcTmpQuery) FROM ARRAY laMlnTmp

=gfOpenFile(gcWorkDir+lcTmpQuery,"","SH")
SELECT (lcTmpQuery)

INDEX ON cCatgTyp + Item +iclr  TAG (lcTmpQuery)

*--create 
CREATE TABLE (gcWorkDir+lcTmpCutPk);
             (Vendor C(8),PO C(6) , Item C(07) , iclr C(06),Allocat N(12,3),;
               Order C(6),UnAlocat N(12,3) , Ddelvery D(8),cWareCode C(6),ReqQty N(12,3),QtyToAlct N(12,3))

=gfOpenFile(gcWorkDir+lcTmpCutPk,"","SH")
SELECT (lcTmpCutPk)

INDEX ON Vendor+ PO + Item + iclr+ Order TAG   (lcTmpCutPk)

*--create 
CREATE TABLE (gcWorkDir+lcTmpRel);
             (Vendor C(8),PO C(6) , Item C(07) , iclr C(06),Allocat N(12,3),;
               Order C(6),UnAlocat N(12,3) , Ddelvery D(8),cWareCode C(6),ReqQty N(12,3), QtyToAlct N(12,3))

=gfOpenFile(gcWorkDir+lcTmpRel,"","SH")
SELECT (lcTmpRel)

INDEX ON Vendor+ PO + Item + iclr + Order TAG (lcTmpRel)

CREATE TABLE (gcWorkDir+lcTmpSlAt);
             (Item C(7), Iclr C(6) ,Rating N(9,3),Requierd N(12,3) , Ordered N(12,3) , ToOrder N(12,3),Allocat N(12,3),;
               Order C(1),UnAlocat C(1) , Ddelvery D(8),cWareCode C(6),Price N(12,3),UOM C(3),;
               cCurrCode C(3),cShipTo C(6),FSupplier C(8),Po C(6),PSupplier C(8),OrdDat D(8),;
               OrdQty N(12,3) ,Delete C(1))

=gfOpenFile(gcWorkDir+lcTmpSlAt,"","SH")
SELECT (lcTmpSlAt)

INDEX ON PO + PSupplier + cShipTo+  cCurrCode + DTOS(Ddelvery) TAG (lcTmpSlAt)


*---
CREATE TABLE (gcWorkDir+lcTmpSlAt2);
             (Item C(7), Iclr C(6) ,Rating N(9,3),Requierd N(12,3) , Ordered N(12,3) , ToOrder N(12,3),Allocat N(12,3),;
               Order C(1),UnAlocat C(1) , Ddelvery D(8),cWareCode C(6),Price N(12,3),UOM C(3),;
               cCurrCode C(3),cShipTo C(6),FSupplier C(8),Po C(6),PSupplier C(8),OrdDat D(8),;
               OrdQty N(12,3) ,Delete C(1),mnotes M(10))

=gfOpenFile(gcWorkDir+lcTmpSlAt2,"","SH")
SELECT (lcTmpSlAt2)

INDEX ON PO + PSupplier + cShipTo+  cCurrCode + DTOS(Ddelvery) TAG (lcTmpSlAt2)
*--

CREATE TABLE (gcWorkDir+lcTmpSlMal);
             ( Ddelvery D(8),cCurrCode C(3),cShipTo C(6),FSupplier C(8),Po C(6),mnotes M(10))

=gfOpenFile(gcWorkDir+lcTmpSlMal,"","SH")
SELECT (lcTmpSlMal)

INDEX ON PO + FSupplier + cShipTo+  cCurrCode + DTOS(Ddelvery) TAG (lcTmpSlMal)

CREATE TABLE (gcWorkDir+lcTmpNewPo);
             (Po C(6),Ddelvery D(8),PSupplier C(8),EmailFrom C(30) ,EmailTo C(30),Email C(1),Print C(1))

=gfOpenFile(gcWorkDir+lcTmpNewPo,"","SH")
INDEX ON  PO TAG (lcTmpNewPo)

CREATE TABLE (gcWorkDir+lcTmpPoNot);
             (Po C(6),PSupplier C(8))

=gfOpenFile(gcWorkDir+lcTmpPoNot,"","SH")
INDEX ON  PO TAG (lcTmpPoNot)

*!*************************************************************
*! Name      : lpEnter
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To Change the select button prompt.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpEnter()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lpEnter
SELECT (lcTmpQuery)
lcPrompt = IIF(cSelect="�","\<Unselect","S\<elect")

*!*************************************************************
*! Name      : lfGetData
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To get the fabric & trims for the selected PO
*!             screen mode.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetData()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfGetData
PRIVATE lnOldAls
= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH')
= gfOpenFile(gcDataDir+'Fabdye',gcDataDir+'Fabdye','SH')
= gfOpenFile(gcDataDir+'POFLN',gcDataDir+'POFLN','SH')
= gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'Vencode','SH')

lnOldAls = SELECT(0)

SELECT ORDBOM
=SEEK(laData[1])
SCAN REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode = laData[1];
             FOR   cCatgTyp $ "F|T"
    SCATTER MEMVAR MEMO
    IF '***' $ m.IClr 
      =SEEK(PADL(m.Item,7),'Fabric')    
      SELECT FABRIC
      SCAN REST WHILE FABRIC = PADL(m.Item,7)
        m.Desc    = Fabric.Desc
        m.UOM     = Fabric.UOMUSE
        m.Content = Fabric.Content
        m.Iclr    = FABRIC.Color 
        SELECT (lcTmpQuery)
        IF SEEK(m.cCatgTyp + m.Item+Fabric.color,lcTmpQuery)
          REPLACE Req      WITH Req     +lfRequred('ORDBOM')
          REPLACE ToOrder  WITH (Req - Ordered)
        ELSE
          APPEND BLANK
          GATHER MEMVAR MEMO
          REPLACE Item     WITH m.item,;
                  iclr     WITH m.iclr,;
                  cDesc    WITH m.Desc,;
                  Content  WITH m.Content,;
                  cCatgTyp WITH m.cCatgTyp ,;
                  Req      WITH lfRequred('ORDBOM'),;
                  Ordered  WITH lfGtOnOrd()
            
          REPLACE ToOrder  WITH (Req - Ordered)
        ENDIF  
      ENDSCAN
    ELSE
      =SEEK(PADL(m.Item,7)+m.IClr,'Fabric')    
      m.Desc    = Fabric.Desc
      m.UOM     = Fabric.UOMUSE
      m.Content = Fabric.Content
    
      SELECT (lcTmpQuery)
      IF SEEK(m.cCatgTyp + m.Item+m.IClr,lcTmpQuery)
        REPLACE Req      WITH Req     +lfRequred('ORDBOM')
        REPLACE ToOrder  WITH (Req - Ordered)
      ELSE
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE Item     WITH m.item,;
                iclr     WITH m.iClr,;
                cDesc    WITH m.Desc,;
                Content  WITH m.Content,;
                cCatgTyp WITH m.cCatgTyp ,;
                Req      WITH lfRequred('ORDBOM'),;
                Ordered  WITH lfGtOnOrd()
        REPLACE ToOrder  WITH (Req - Ordered)
      ENDIF  
    ENDIF  
  ENDSCAN
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfRdActMan
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Read activate for automatic issue screen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRdActMan()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfRdActMan

PRIVATE llChkNew,llChkClos

STORE .F. TO llChkNew,llChkClos

SELECT (lcTmpSlAt2)
SET FILTER TO 
SCAN FOR EMPTY(PO)
  llChkNew = .T.
  EXIT
ENDSCAN

SELECT (lcTmpNewPo)
LOCATE
IF !EOF()
  llChkClos = .T.
ENDIF

DO CASE
  CASE llChkNew AND llChkClos 
    SHOW GET pbNew   ENABLE
    SHOW GET pbClose DISABLE
    
  CASE !llChkNew AND llChkClos 
    SHOW GET pbNew   ENABLE
    SHOW GET pbClose ENABLE

  CASE llChkNew AND !llChkClos 
    SHOW GET pbNew   ENABLE
    SHOW GET pbClose DISABLE
  
  CASE !llChkNew AND !llChkClos 
    SHOW GET pbNew   DISABLE
    SHOW GET pbClose ENABLE

ENDCASE

IF lnActFolder = 2 AND !llCheckOpen
  llCheckOpen = .T.
  _CUROBJ = OBJNUM(pbSelect)
ENDIF  
  
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcMtBrowTt)

*!*************************************************************
*! Name      : lfDMainLot
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : De-activate for automatic issue screen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDMainLot()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfDMainLot

ON KEY LABEL CTRL+Q lnDummy = 1
ON KEY LABEL CTRL+W lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM
RETURN .F.

*!*************************************************************
*! Name      : lfClearKey
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Clear the keys
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClearKey()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*!*************************************************************
*! Name      : lfGtOnOrd
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To get the on order for the open lots
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGtOnOrd()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfGtOnOrd
PRIVATE lnOldAls , lcScanExp , lnOnOrd

lnOldAls = SELECT(0)
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')

SELECT (lcOrdLine)
lcScanExp = ""
lnOnOrd   = 0

lcOrder = laData[1]
SELECT mapoalo
SET ORDER TO Maposo
IF !EMPTY(lcOrder) .AND. SEEK(lcOrder+PADL(m.Item,7)+m.Iclr)
  SCAN REST WHILE order+fabric+color = lcOrder+PADL(m.Item,7)+m.Iclr
    IF !SEEK('P'+POMAT,'POFHDR') OR (POFHDR.Status $'SX')
      LOOP
    ENDIF
    lnOnOrd   = lnOnOrd + nhousqty
  ENDSCAN
ENDIF
SELECT(lnOldAls)

*-- Seek in the fabric file to get the conversion factor.
*=SEEK(PADL(m.Item,7)+m.IClr,'Fabric')
*lnOnOrd = lnOnOrd* IIF(Fabric.Conv = 0 ,1,Fabric.Conv)

RETURN(MAX(lnOnOrd,0))


*!*************************************************************
*! Name      : lfRequred
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Function to collect the Required quantity.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfRequred
PARAMETER lcTable
STORE 0 TO lnReqedEn
lcAlias = SELECT(0)
SELECT(lcTable)

lnReqedEn = lfSzTot(NBOMTOTQTY)

SELECT (lcAlias)
RETURN lnReqedEn

*:**************************************************************************
*:* Name        : lfSzTot
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Get requirement based on sizes
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSzTot()
*:***************************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfSzTot
PARAMETERS lnUntQty
PRIVATE lnReqedEn ,m.IClr ,lcAlias

lcAlias = SELECT(0)

= gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')

=SEEK(&lcOrdLine..cordtype+&lcOrdLine..order+STR(&lcOrdLine..lineno,6),'ORDCANLN')
SELECT (lcTable)

SCATTER MEMV MEMO
lcSizes = m.Msizes
lcSizes = SUBSTR(m.Msizes,5,LEN(m.Msizes))

lnReqedEn = 0

IF '*' $ citmmask
  lnTotQty = 0
  SELECT (lcOrdLine)
  LOCATE 
  IF &lcTable..LBASONSIZ 
    FOR lnCount = 1 TO 8
      lcCnt = STR(lnCount,1)
      SELECT (lcOrdLine)
      LOCATE 
      lnTotQty = 0
      SCAN FOR Style = LEFT(&lcTable..citmmask ,12)
        IF SEEK(cordtype+order+STR(lineno,6),'ORDCANLN')
         lnTotQty = lnTotQty +  &lcOrdLine..Book&lcCnt  - ORDCANLN.Qty&lcCnt
        ELSE
         lnTotQty = lnTotQty + &lcOrdLine..Book&lcCnt 
        ENDIF
      ENDSCAN

      lnReqedEn = lnReqedEn + IIF( lcCnt $ lcSizes , lnTotQty , 0 ) * lnUntQty
    ENDFOR
  ELSE
    SELECT (lcOrdLine)
    LOCATE 
    lnTotQty = 0
    SCAN FOR Style = LEFT(&lcTable..citmmask ,12)
      IF Fabric.Color <> RIGHT(&lcOrdLine..Style , 6)
        LOOP
      ENDIF
      IF SEEK(cordtype+order+STR(lineno,6),'ORDCANLN')
       lnTotQty = lnTotQty +  &lcOrdLine..TotBook - ORDCANLN.TotQty
      ELSE
       lnTotQty = lnTotQty + &lcOrdLine..TotBook 
      ENDIF
    ENDSCAN
    lnReqedEn = lnTotQty * lnUntQty
  ENDIF
ELSE
  SELECT (lcOrdLine)
  LOCATE 
  IF &lcTable..LBASONSIZ 
    FOR lnCount = 1 TO 8
      lcCnt = STR(lnCount,1)
      lnTotQty = 0
      SELECT (lcOrdLine)
      LOCATE 
      SCAN FOR Style = &lcTable..citmmask 
        IF SEEK(cordtype+order+STR(lineno,6),'ORDCANLN')
         lnTotQty = lnTotQty +  &lcOrdLine..Book&lcCnt  - ORDCANLN.Qty&lcCnt
        ELSE
         lnTotQty = lnTotQty + &lcOrdLine..Book&lcCnt 
        ENDIF
      ENDSCAN
      lnReqedEn = lnReqedEn + IIF( lcCnt $ lcSizes , lnTotQty , 0 ) * lnUntQty
    ENDFOR
  ELSE
    lnTotQty = 0
    SELECT (lcOrdLine)
    LOCATE 
    SCAN FOR Style = &lcTable..citmmask 
      IF SEEK(cordtype+order+STR(lineno,6),'ORDCANLN')
       lnTotQty = lnTotQty +  &lcOrdLine..TotBook - ORDCANLN.TotQty
      ELSE
       lnTotQty = lnTotQty + &lcOrdLine..TotBook 
      ENDIF
    ENDSCAN
    lnReqedEn = lnTotQty * lnUntQty
  ENDIF
ENDIF  
SELECT (lcOrdLine)
LOCATE 

SELECT (lcAlias)
RETURN lnReqedEn
*-- end of lfSzTot.

*---For Screen Alocate
*!*************************************************************
*! Name      : lfAlcBrow
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfAlcBrow()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfAlcBrow

SELECT (lcTmpCutPk)
GO TOP

lcAlcBrow =  [Vendor   :08  :H = 'Vendor'         :R,] +;
             [PO       :08  :H = 'PO'             :R,] +;
             [Allocat  :14  :H = 'Allocated'      :R,] +;
             [Order    :8   :H = 'Order'          :R,] +;
             [UnAlocat :14  :H = 'Unallocated'    :R,]+;
             [Ddelvery :10  :H = 'Delivery date'  :R,]+;
             [cWareCode:8   :H = 'Location'       :R,]+;
             [ReqQty   :14  :H = 'Rec Qty'        :R]


BROWSE FIELDS &lcAlcBrow;
       WINDOW SOALOCT3 IN WINDOW SOALOCT;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcDisMul)


*!*************************************************************
*! Name      : lfReadAMul
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfReadAMul()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfReadAMul
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDisMul)

*!*************************************************************
*! Name      : lfDDisMul
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfDDisMul()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfDDisMul
PARAMETERS lcType

ON KEY LABEL CTRL+Q lnDummy = 1
ON KEY LABEL CTRL+W lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM
RETURN .F.

*!*************************************************************
*! Name      : lfvSelect
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfvSelect()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfvSelect
PRIVATE lnAlias 
lnAlias = SELECT (0)

lcClearBmp  = gcBmpHome + "Clear.Bmp"
lcNextBmp   = gcBmpHome + "Next.Bmp"
lcNotesBmp  = gcBmpHome + "Notes1.Bmp"
lcPriorBmp  = gcBmpHome + "Prior.Bmp"
lcRelBmp    = gcBmpHome + "Release2.Bmp"
lcSavBmp    = gcBmpHome + "Save.Bmp"
lcAllocBmp  = gcBmpHome + "allocate.Bmp"
lcExtBmp    = gcBmpHome + "extkey.bmp"
PRIVATE lnARate,lnARqured,lnAOrdrd,lnATOrdr,lnAUnAllct,lnAPrice
PRIVATE lcAUOM,lcACurrncy,lcAShipTo,lcASupplir,lcAOrder,lcAUnAllct
STORE 0  TO lnARate,lnARqured,lnAOrdrd,lnATOrdr,lnAPrice
STORE '' TO lcAUOM,lcACurrncy,lcAShipTo,lcASupplir,lcAOrder,lcAUnAllct
STORE '' TO lcDelete 

=lfGetSlAut()
SELECT (lcOrdLine)
LOCATE
SELECT (lcTmpSlAt2)
SET FILTER TO Item+iclr = PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr
SELECT (lcTmpSlAt)
LOCATE
SCAN FOR !EMPTY(PO)
  SCATT MEMVAR MEMO
  IF !SEEK( PO ,lcTmpSlAt2)
    SELECT (lcTmpSlAt2)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF  
ENDSCAN
SELECT (lcTmpSlAt2)
SET FILTER TO Item+iclr = PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr
SELECT (lcTmpSlAt)
LOCATE
=lfChckAlct()
lnARate    =Rating 
lnARqured  =Requierd 
lnAOrdrd   =Ordered 
lnATOrdr   =ToOrder 
lcAUnAllct =IIF(EMPTY(lcAUnAllct),UnAlocat,lcAUnAllct)
lnAPrice   =Price 
lcAUOM     =UOM 
lcACurrncy =cCurrCode 
lcAShipTo  =cShipTo 
lcASupplir = FSupplier 
lcAOrder   = Order 


lnQtyToAlo = 0
lnQtyToulo = 0
lcOldVal = ""
PUSH KEY
DO (gcScrDir+"SO\soSLAUT.SPX")
POP KEY
lnAlias = SELECT (0)

*!*************************************************************
*! Name      : lfGetAloct
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Get Data for alocated Qty
*!*************************************************************
*! Example   : = lfGetAloct()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfGetAloct
PRIVATE lnOldAls,lnOnOrd,lnRecQty,lcOldOrd , lnPofQty
lnOldAls = SELECT()
= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')

SELECT (lcTmpCutPk)
ZAP
SELECT MAPOALO
lcMapOrd =ORDER()
SET ORDER TO Mapoalo

SELECT POFLN
lcOldOrd =ORDER()
SET ORDER TO POFLNF
*B128216,2 MMR 11/30/2005 4)Fix bug of not showing POs that Partially recieved.[Start]
lnPoQty = 0
*B128216,2 MMR.[End] 
IF SEEK(PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr)
  SCAN REST while fabric+color+cmattype+pomat+trancd=   PADL(&lcTmpQuery..Item,7)+  &lcTmpQuery..iclr
    STORE 0 TO lnOnOrd,lnRecQty ,lnHusQty
    
    *--mhm as per customer request
    *IF SEEK('P'+POMAT,'POFHDR') AND !(POFHDR.Status $'SX')
    IF SEEK('P'+POMAT,'POFHDR') AND !(POFHDR.Status $'X')
    *--mhm as per customer request
    
      lcWarehose = POFHDR.cWareCode
      PRIVATE lnTotOrd
      lnTotOrd = 0
      *B128216,2 MMR 11/30/2005 4)Fix bug of not showing POs that Partially recieved.[Start]
      IF Trancd ="1"
        lnPoQty  = nFabTotQty*Fabric.CONV
      ENDIF
      *B128216,2 MMR.[End] 
      IF Trancd ="2"
        *B128216,2 MMR 11/30/2005 1)Fix bug of showing the REcieve Qty by UOM buy
        *lnRecQty  = lnRecQty + nFabTotQty
        lnRecQty  = lnRecQty + nFabTotQty*Fabric.CONV
        *B128216,2 MMR.[End]
        lcWarehose = PofLn.cWareCode
      ENDIF
      lcScanExp = POMat+PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr
      lcord=POMat
      SELECT MAPOALO
      *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
      *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
      *=SEEK(lcScanExp)
      IF SEEK(lcScanExp)
      *B128216,1 MMR[End]
        lcOrdMat = ''
        SCAN REST WHILE pomat+fabric+color+order = lcScanExp
          lnTotOrd = lnTotOrd + MAPOALO.nhousqty
          lnOnOrd  = MAPOALO.nhousqty
          lcOrdMat = ORDER
      
        SELECT (lcTmpCutPk) 
        IF SEEK(POFHDR.vendor+PofLn.POMAT+PofLn.fabric+PofLn.Color+lcOrdMat )
          *B128216,2 MMR 11/30/2005 4)Fix bug of not showing POs that Partially recieved.[Start]
          *IF (PofLn.nFabTotQty - lnOnOrd)<0 OR  (PofLn.nFabTotQty - lnOnOrd)=0
          IF (lnPoQty- lnOnOrd)<0 OR  (lnPoQty- lnOnOrd)=0
          *B128216,2 MMR.[End]
            DELETE 
            LOOP
          ENDIF
          IF PofLn.Trancd ="1"
            *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
            *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
            *REPLACE Vendor     WITH POFHDR.vendor,;
            *        Allocat    WITH Allocat+PofLn.nFabTotQty,;
            *        UnAlocat   WITH UnAlocat +PofLn.nFabTotQty
            REPLACE Vendor     WITH POFHDR.vendor,;
                    Allocat    WITH Allocat+(PofLn.nFabTotQty*Fabric.CONV),;
                    UnAlocat   WITH UnAlocat +(PofLn.nFabTotQty*Fabric.CONV)
            *B128216,1 MMR[End]         
          ENDIF
          IF PofLn.Trancd ="5"
            *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
            *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
            *REPLACE Vendor     WITH POFHDR.vendor,;
            *        Allocat    WITH Allocat- PofLn.nFabTotQty,;
            *        UnAlocat   WITH UnAlocat - PofLn.nFabTotQty
            REPLACE Vendor     WITH POFHDR.vendor,;
                    Allocat    WITH Allocat- (PofLn.nFabTotQty*Fabric.CONV),;
                    UnAlocat   WITH UnAlocat - (PofLn.nFabTotQty*Fabric.CONV)
            *B128216,1 MMR[End]        
          ENDIF
        
          IF PofLn.Trancd ="2"
            *B128216,2 MMR 11/30/2005 1)Fix bug of showing the REcieve Qty by UOM buy
            *REPLACE ReqQty     WITH ReqQty +PofLn.nFabTotQty
            REPLACE ReqQty     WITH ReqQty +(PofLn.nFabTotQty*Fabric.CONV)
            *B128216,2 MMR.[End]
          ENDIF
        ELSE
          *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
          *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
          *IF (PofLn.nFabTotQty - lnOnOrd)<0 OR  (PofLn.nFabTotQty - lnOnOrd) =0
          IF ((PofLn.nFabTotQty*Fabric.CONV) - lnOnOrd)<0 OR  ((PofLn.nFabTotQty*Fabric.CONV) - lnOnOrd) =0
          *B128216,1 MMR[End]
            LOOP
          ENDIF
          APPEND BLANK
          *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
          *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
          *REPLACE Vendor     WITH POFHDR.vendor,;
          *        PO         WITH PofLn.POMAT ,;
          *        Item       WITH PofLn.fabric,;
          *        iclr       WITH PofLn.Color,;
          *        Allocat    WITH lnOnOrd,;
          *        Order      WITH lcOrdMat ,;
          *        UnAlocat   WITH PofLn.nFabTotQty ,;
          *        Ddelvery   WITH POFHDR.COMPLETE,;
          *        cWareCode  WITH lcWarehose;
          *        ReqQty     WITH lnRecQty
          *B128216,2 MMR 11/30/2005 2)Fix bug of showing the unallocated qty with wrong values in case of
          *B128216,2 MMR              Over recieve.[Start] 
          *REPLACE Vendor     WITH POFHDR.vendor,;
                  PO         WITH PofLn.POMAT ,;
                  Item       WITH PofLn.fabric,;
                  iclr       WITH PofLn.Color,;
                  Allocat    WITH lnOnOrd,;
                  Order      WITH lcOrdMat ,;
                  UnAlocat   WITH PofLn.nFabTotQty*Fabric.CONV ,;
                  Ddelvery   WITH POFHDR.COMPLETE,;
                  cWareCode  WITH lcWarehose;
                  ReqQty     WITH lnRecQty
         REPLACE Vendor     WITH POFHDR.vendor,;
                  PO         WITH PofLn.POMAT ,;
                  Item       WITH PofLn.fabric,;
                  iclr       WITH PofLn.Color,;
                  Allocat    WITH lnOnOrd,;
                  Order      WITH lcOrdMat ,;
                  Ddelvery   WITH POFHDR.COMPLETE,;
                  cWareCode  WITH lcWarehose;
                  ReqQty     WITH lnRecQty
          IF PofLn.Trancd ="2"
            REPLACE UnAlocat   WITH (PofLn.nFabTotQty*Fabric.CONV) - lnOnOrd
          ELSE  
            REPLACE UnAlocat   WITH PofLn.nFabTotQty*Fabric.CONV
          ENDIF  
          *B128216,2 MMR.[End]       
          *B128216,1 MMR[End]       
        ENDIF          
        ENDSCAN
        SELECT (lcTmpCutPk) 
        =SEEK(POFHDR.vendor+PofLn.POMAT+PofLn.fabric+PofLn.Color )
        SCAN REST WHILE Vendor+ PO + Item + iclr+ Order  = POFHDR.vendor+PofLn.POMAT+PofLn.fabric+PofLn.Color
          IF PofLn.Trancd ="1"
            REPLACE &lcTmpCutPk..UnAlocat WITH  MAX(UnAlocat - lnTotOrd,0)
          ENDIF  
        ENDSCAN  
      *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
      *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
      ELSE
        SELECT PofLn
        lnOnOrd  = PofLn.nFabTotQty*Fabric.CONV
        lcOrdMat = PofLn.pomat
      
        SELECT (lcTmpCutPk) 
        IF SEEK(POFHDR.vendor+PofLn.POMAT+PofLn.fabric+PofLn.Color)
          IF (lnOnOrd)<0 OR  (lnOnOrd)=0
            DELETE 
            LOOP
          ENDIF
          IF PofLn.Trancd ="1"
            REPLACE Vendor     WITH POFHDR.vendor,;
                    Allocat    WITH 0.000        ,;
                    UnAlocat   WITH UnAlocat +lnOnOrd
          ENDIF
          IF PofLn.Trancd ="5"
            REPLACE Vendor     WITH POFHDR.vendor,;
                    Allocat    WITH 0.000        ,;
                    UnAlocat   WITH UnAlocat - lnOnOrd
          ENDIF
        
          IF PofLn.Trancd ="2"
            REPLACE ReqQty     WITH ReqQty +lnOnOrd
          ENDIF
        ELSE
          IF (lnOnOrd)<0 OR  (lnOnOrd) =0
            LOOP
          ENDIF
          APPEND BLANK

          REPLACE Vendor     WITH POFHDR.vendor  ,;
                  PO         WITH PofLn.POMAT    ,;
                  Item       WITH PofLn.fabric   ,;
                  iclr       WITH PofLn.Color    ,;
                  Allocat    WITH 0.000          ,;
                  Order      WITH ''             ,;
                  UnAlocat   WITH lnOnOrd        ,;
                  Ddelvery   WITH POFHDR.COMPLETE,;
                  cWareCode  WITH lcWarehose      ;
                  ReqQty     WITH lnRecQty
        ENDIF          
       ENDIF
       *B128216,1 MMR[End]  
      ENDIF        
  ENDSCAN
ENDIF


SELECT MAPOALO
SET ORDER TO &lcMapOrd

SELECT POFLN
SET ORDER TO &lcOldOrd 

SELECT(lnOldAls)
*!*************************************************************
*! Name      : lfQtyToAlo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfQtyToAlo()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfQtyToAlo

SELECT (lcTmpCutPk)
IF SEEK('P'+PO,'POFHDR') AND (POFHDR.Status $'C')
  IF lnQtyToAlo > (ReqQty -Allocat )
    lcMsg2 = "PO Completed, Can not allocate more what has been received"
    WAIT WINDOW lcMsg2
    lnQtyToAlo = (ReqQty -Allocat )
  ENDIF
ENDIF
IF lnQtyToAlo > UnAlocat
  lcMsg2 = " Can not allocate more than unallocated quantity"
  WAIT WINDOW lcMsg2
  lnQtyToAlo = 0
  _CUROBJ = OBJNUM(lnQtyToAlo)  
  RETURN
ENDIF
lnToOrdALo = lnToOrdALo - lnQtyToAlo
SELECT (lcTmpCutPk)
REPLACE QtyToAlct WITH lnQtyToAlo,;
        UnAlocat  WITH UnAlocat - lnQtyToAlo,;
        Allocat   WITH Allocat + lnQtyToAlo
SHOW GET lnToOrdALo
*!*************************************************************
*! Name      : lfvUpdAlo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfvUpdAlo()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfvUpdAlo
PRIVATE lnToOrd

lnToOrd = 0
SELECT (lcTmpCutPk)
SCAN FOR !EMPTY(QtyToAlct)
  lnToOrd = lnToOrd+&lcTmpCutPk..QtyToAlct        
ENDSCAN

SELECT (lcTmpQuery)
REPLACE ToOrder   WITH ToOrder - lnToOrd,;
        Ordered   WITH Ordered +lnToOrd

SELECT(lcTmpSlAt2)
REPLACE ToOrder   WITH ToOrder - lnToOrd,;
        Ordered   WITH Ordered +lnToOrd
*-- add new record 
SELECT (lcTmpCutPk)
SCAN FOR !EMPTY(QtyToAlct)
  SCATT MEMVAR MEMO
  IF SEEK('P'+m.PO,'Pofhdr')
    m.OrdDat    = Pofhdr.entered
    m.Ddelvery  = Pofhdr.Complete
    m.cWareCode = POFHDR.cWareCode
    m.PSupplier = POFHDR.VENDOR
  ENDIF
  
  SELECT(lcTmpSlAt2)
  IF SEEK(m.PO)
    REPLACE OrdQty    WITH  OrdQty + m.QtyToAlct
    lnAOrdrd   =Ordered 
    lnATOrdr   =ToOrder 
    
  ELSE
    APPEND BLANK
    REPLACE PO        WITH   m.PO,;
            Item      WITH   m.Item,;
            iclr      WITH   m.iclr,;
            Ddelvery  WITH   m.Ddelvery ,;
            PSupplier WITH   m.Vendor,;
            OrdQty    WITH   m.QtyToAlct,;
            Delete    WITH   'N',;
            OrdDat    WITH   m.OrdDat ,;
            cWareCode WITH   m.cWareCode 
    SELECT (lcTmpQuery)
    lnAOrdrd   =Ordered 
    lnATOrdr   =ToOrder 

  ENDIF          
ENDSCAN

*--update Alocated
SELECT (lcTmpCutPk)
LOCATE 
IF !EOF()
  SCAN FOR !EMPTY(QtyToAlct)
    SELECT MAPOALO 
    SET ORDER TO Mapoalo
    IF SEEK(&lcTmpCutPk..PO+PADL(&lcTmpCutPk..Item,7)+&lcTmpCutPk..iclr+OrdHdr.Order)
      REPLACE nhousqty WITH nhousqty + &lcTmpCutPk..QtyToAlct
    ELSE
      APPEND BLANK
      REPLACE pomat    WITH &lcTmpCutPk..PO,;
              Fabric   WITH &lcTmpCutPk..Item,;
              Color    WITH &lcTmpCutPk..Iclr,;
              Order    WITH OrdHdr.order,;
              nhousqty WITH &lcTmpCutPk..QtyToAlct
      =gfAdd_Info('MAPOALO')
    ENDIF
  ENDSCAN
ENDIF
*--update UnAlocated

CLEAR READ
SHOW WINDOW (lcSlAtWin) REFRESH
SHOW GET lnAOrdrd ENABLE
SHOW GET lnATOrdr ENABLE
        
*!*************************************************************
*! Name      : lfRELBrow
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfAlcBrow()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfRELBrow

SELECT (lcTmpRel)
GO TOP

lcAlcBrow =  [Vendor   :08  :H = 'Vendor'         :R,] +;
             [PO       :08  :H = 'PO'             :R,] +;
             [Allocat  :14  :H = 'Allocated'      :R,] +;
             [Order    :8   :H = 'Order'          :R,] +;
             [Ddelvery :10  :H = 'Delivery date'  :R,]+;
             [cWareCode:8   :H = 'Location'       :R,]+;
             [ReqQty   :14  :H = 'Rec Qty'        :R]


BROWSE FIELDS &lcAlcBrow;
       WINDOW sorels3 IN WINDOW sorels;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcRealWin)


*!*************************************************************
*! Name      : lfReadREL
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfReadAMul()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfReadREL
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcRealWin)

*!*************************************************************
*! Name      : lfDDisMul
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfDDisMul()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfDDisMul
PARAMETERS lcType

ON KEY LABEL CTRL+Q lnDummy = 1
ON KEY LABEL CTRL+W lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM
RETURN .F.

*!*************************************************************
*! Name      : lfvRelaS
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfvRelaS()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvRelaS
llChkRtrn = .F.


=lfGetUAlct()
lnToOrdULo = &lcTmpQuery..ToOrder
lnQtyToAlo = 0
SELECT (lcTmpRel)

PUSH KEY
ON KEY
DO (gcScrDir+"SO\sorels.SPX")
POP KEY
KEYBOARD "{ALT+B}" CLEAR
=lfChckAlct()
SHOW WINDOW (lcSlAtWin) REFRESH

*!*************************************************************
*! Name      : lfGetUAlct
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfGetUAlct()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfGetUAlct
PRIVATE lnOldAls , lcOldOrd
lnOldAls = SELECT()
= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
SELECT POFLN
SET ORDER TO POFLN
SELECT (lcTmpRel)
ZAP

lcWarehose = POFHDR.cWareCode

SELECT MAPOALO
lcOldOrd =ORDER()
SET ORDER TO Mapofab
=SEEK(PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr)
SCAN REST while fabric+color+order =   PADL(&lcTmpQuery..Item,7)+  &lcTmpQuery..iclr
  STORE 0 TO lnOnOrd,lnRecQty

  STORE 0 TO lnOnOrd,lnRecQty

  *--mhm as per customer req.
  *IF SEEK('P'+POMAT,'POFHDR') AND !(POFHDR.Status $'SX')
  IF SEEK('P'+POMAT,'POFHDR') AND !(POFHDR.Status $'X')
  *--mhm as per customer req.
  
    lcScanExp = "P"+POMat+PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr
    SELECT POFLN
    =SEEK(lcScanExp)
    SCAN REST WHILE cmattype+pomat+fabric+color+trancd = lcScanExp
      IF Trancd ="1"
        lnOnOrd   = lnOnOrd  + nFabTotQty
      ENDIF
      IF Trancd ="5"
        lnOnOrd   = lnOnOrd  - nFabTotQty
      ENDIF
      IF Trancd ="2"
        lnRecQty  = lnRecQty + nFabTotQty
        lcWarehose = PofLn.cWareCode
      ENDIF
    ENDSCAN
    IF lnOnOrd = 0
      LOOP
    ENDIF
    SELECT (lcTmpRel) 
    *IF SEEK(POFHDR.vendor+MAPOALO.POMAT+MAPOALO.fabric+MAPOALO.Color)
    IF SEEK(POFHDR.vendor+MAPOALO.POMAT+MAPOALO.fabric+MAPOALO.Color+MAPOALO.ORDER)
      REPLACE Vendor     WITH POFHDR.vendor,;
             Allocat    WITH Allocat+MAPOALO.nhousqty,;
             UnAlocat   WITH UnAlocat -MAPOALO.nhousqty
    ELSE
      APPEND BLANK
      REPLACE Vendor     WITH POFHDR.vendor,;
              PO         WITH MAPOALO.POMAT ,;
              Item       WITH MAPOALO.fabric,;
              iclr       WITH MAPOALO.Color,;
              Allocat    WITH MAPOALO.nhousqty,;
              Order      WITH MAPOALO.ORDER ,;
              UnAlocat   WITH lnOnOrd-MAPOALO.nhousqty,;
              Ddelvery   WITH POFHDR.COMPLETE,;
              cWareCode  WITH lcWarehose;
              ReqQty     WITH lnRecQty
    ENDIF          
  ENDIF        
ENDSCAN

SELECT MAPOALO
SET ORDER TO &lcOldOrd 

SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfQtyToUlo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfQtyToUlo()
*!*************************************************************
*: Due to  C#124836,1.
*:*************************************************************
FUNCTION lfQtyToUlo

IF lnQtyToAlo > Allocat
  lcMsg2 = " Can not unallocate more than allocated quantity"
  WAIT WINDOW lcMsg2
  lnQtyToAlo = 0
  _CUROBJ = OBJNUM(lnQtyToAlo)  
  RETURN
ENDIF
SELECT (lcTmpRel)
REPLACE QtyToAlct  WITH lnQtyToulo,;
        Allocat    WITH Allocat - lnQtyToulo
=lfRefresh()

*!*************************************************************
*! Name      : lfvUpdUAlo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To Update UnAlocated 
*!*************************************************************
*! Example   : = lfvUpdUAlo()
*!*************************************************************
*: Due to  C#124836,1.
*:*************************************************************
FUNCTION lfvUpdUAlo
PRIVATE lnToOrd , lcOldOrdr
lnToOrd = 0
SELECT MAPOALO  
lcOldOrdr = ORDER()
SET ORDER TO Mapoalo

lnToOrd = 0
SELECT (lcTmpRel)
SCAN FOR !EMPTY(QtyToAlct)
  IF ORDER = ORDHDR.ORDER
    SELECT(lcTmpSlAt2)
    LOCATE FOR PO+Item +Iclr =&lcTmpRel..PO+&lcTmpRel..Item +&lcTmpRel..Iclr
    
    SELECT(lcTmpSlAt2)
    REPLACE ToOrder   WITH ToOrder + &lcTmpRel..QtyToAlct,;
            Ordered   WITH Ordered - &lcTmpRel..QtyToAlct,;
            OrdQty    WITH OrdQty  - &lcTmpRel..QtyToAlct
    IF OrdQty = 0
      DELETE
      LOCATE
    ENDIF 

    SELECT (lcTmpQuery)
    REPLACE ToOrder   WITH ToOrder + &lcTmpRel..QtyToAlct,;
            Ordered   WITH Ordered - &lcTmpRel..QtyToAlct

    lnAOrdrd   =Ordered 
    lnATOrdr   =ToOrder 
    
    SELECT(lcTmpSlAt)
    lnARqured  =Requierd 

  ENDIF  
ENDSCAN

lnQtyToAlo = 0

SELECT (lcTmpRel)
SCAN FOR !EMPTY(QtyToAlct)
  SELECT MAPOALO  
  SET ORDER TO Mapoalo
  =SEEK(&lcTmpRel..PO+PADL(&lcTmpRel..Item,7)+&lcTmpRel..iclr+&lcTmpRel..Order,'MAPOALO')
  SELECT MAPOALO  
  lnSubQty = nhousqty - &lcTmpRel..QtyToAlct
  SCAN REST WHILE pomat+fabric+color+order = &lcTmpRel..PO+PADL(&lcTmpRel..Item,7)+&lcTmpRel..iclr+&lcTmpRel..Order
    IF lnSubQty>0
      REPLACE nhousqty WITH nhousqty - &lcTmpRel..QtyToAlct
      EXIT
    ELSE
      IF  lnSubQty = 0 
        REPLACE nhousqty WITH 0
        EXIT
      ENDIF
      lnSubQty = &lcTmpRel..QtyToAlct - nhousqty 
      REPLACE nhousqty WITH 0
    ENDIF  
  ENDSCAN
  SELECT MAPOALO  
  DELETE ALL FOR nhousqty = 0
ENDSCAN

SELECT (lcTmpRel)
ZAP

CLEAR READ


*!*************************************************************
*! Name      : lfSLATBrow
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfSLATBrow()
*!*************************************************************
*: Due to  C#124836,1.
*:*************************************************************
FUNCTION lfSLATBrow

SELECT (lcTmpSlAt2)
GO TOP
             
lcSlAtBrow = [PO       :10  :H = 'Po No '         :R,]+;
             [PSupplier:15  :H = 'Supplier '      :R,]+;
             [OrdDat   :15  :H = 'Order Date'     :R,]+; 
             [Ddelvery :15  :H = 'Delivery Date'  :V=.T.,]+;
             [OrdQty   :15  :H = 'Ordered'        :R]

BROWSE FIELDS &lcSlAtBrow;
       WINDOW soslaut3 IN WINDOW soslaut;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       WHEN lfwSlBrow();
       TITLE ALLTRIM(lcSlAtWin)

*!*************************************************************
*! Name      : lfwSlBrow
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfwSlBrow()
*!*************************************************************
*: Due to  C#124836,1.
*:**************************************************************************
FUNCTION lfwSlBrow

IF UPPER(lcAUnAllct)  = 'Y'
  SHOW GET PbAlocate ENABLE
ELSE
  SHOW GET PbAlocate DISABLE
ENDIF

SHOW GETS 

*!*************************************************************
*! Name      : lfvAlocat
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfvAlocat()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvAlocat

=lfGetAloct()
lnToOrdALo = &lcTmpQuery..ToOrder
llChkRtrn = .F.

lnQtyToAlo = 0
PUSH KEY

*DO (gcScrDir+"SO\soaloct.SPX")
DO (gcScrDir+"SO\Soukalct.SPX")
POP KEY
KEYBOARD "{ALT+B}" CLEAR

*!*************************************************************
*! Name      : lfReadAMul
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfReadAMul()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfRedSlAut
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcSlAtWin)
SHOW GET lnATOrdr  ENABLE
SHOW GET lnARqured DISABLE
SHOW GET lnAOrdrd  ENABLE
IF UPPER(lcAUnAllct)  = 'Y'
  SHOW GET PbAlocate ENABLE
ELSE
  SHOW GET PbAlocate DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvRating
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfvRating()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvRating
PRIVATE lnTotQty,lnAlias
lnTotQty = 0
lnAlias = SELECT(0)
SELECT (lcTmpSlAt)
REPLACE &lcTmpSlAt..Rating WITH lnARate
lnTotQty =lfvColtQty(&lcTmpQuery..Item , &lcTmpQuery..Iclr)

SELECT (lcTmpSlAt) 
REPLACE Requierd  WITH  lnTotQty ,;
        ToOrder   WITH  lnTotQty - &lcTmpQuery..Ordered 

REPLACE &lcTmpQuery..NBOMTOTQTY  WITH  &lcTmpSlAt..Rating
REPLACE &lcTmpQuery..Req         WITH  lnTotQty 
REPLACE &lcTmpQuery..ToOrder     WITH  &lcTmpQuery..Req - &lcTmpQuery..Ordered 
lnARqured  =Requierd 
lnAOrdrd   =Ordered 
lnATOrdr   =ToOrder 
SHOW GETS
 
SELECT(lcTmpQuery)
SCATT MEMVAR MEMO
=SEEK(order+STR(lineno,6)+style+sclr+item+iclr+mfgcode,'ORDBOM')
IF EOF('ORDBOM') 
  =SEEK(order+STR(lineno,6)+style+sclr+item+"******"+mfgcode,'ORDBOM')
  m.iclr = '******'
ENDIF
SELECT ORDBOM
GATHER MEMVAR MEMO
SELECT (lnAlias) 

*!*************************************************************
*! Name      : lfvPrice
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfvPrice()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvPrice
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT(lcTmpSlAt)
REPLACE &lcTmpSlAt..Price WITH lnAPrice
SELECT(lcTmpQuery)
 
REPLACE &lcTmpQuery..UntCost  WITH  &lcTmpSlAt..Price
REPLACE &lcTmpQuery..TOtCost  WITH  &lcTmpQuery..NBOMTOTQTY *&lcTmpSlAt..Price
REPLACE &lcTmpQuery..nExCost  WITH  &lcTmpSlAt..Price
SELECT(lcTmpQuery)

SCATT MEMVAR MEMO
=SEEK(order+STR(lineno,6)+style+sclr+item+iclr+mfgcode,'ORDBOM')
IF EOF('ORDBOM') 
  =SEEK(order+STR(lineno,6)+style+sclr+item+"******"+mfgcode,'ORDBOM')
  m.iclr = '******'
ENDIF
SELECT ORDBOM
GATHER MEMVAR MEMO

SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvToOrder
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfvRating()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvToOrder

REPLACE &lcTmpSlAt..ToOrder WITH  lnATOrdr
REPLACE &lcTmpQuery..ToOrder  WITH  &lcTmpSlAt..ToOrder

*!*************************************************************
*! Name      : lfvUnAlct
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfvUnAlct()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvUnAlct
PARAMETER lcType

IF lcType = 'U'
  IF !(UPPER(UnAlocat) == "Y" OR UPPER(UnAlocat) == "N")
    WAIT WINDOW "Invalid key"
    REPLACE UnAlocat WITH UPPER(lcOldVal)
    SHOW WINDOW (lcSlAtWin) REFRESH
  ELSE
    REPLACE UnAlocat WITH UPPER(UnAlocat)
    SHOW WINDOW (lcSlAtWin) REFRESH
  ENDIF
ELSE
  IF !(UPPER(lcAOrder) == "Y" OR UPPER(lcAOrder) == "N")
    WAIT WINDOW "Invalid key"
    REPLACE Order WITH UPPER(lcOldVal)
    lcAOrder = UPPER(lcOldVal)
    _CUROBJ = OBJNUM(lcAOrder)
  ELSE 
    IF (UPPER(lcOldVal) == "N") AND (UPPER(lcAOrder) == "Y") AND ToOrder >0
      lcMsg2 = "Do You wish to create a new Order?"
      lnMsgNo=gfModalGen("TRM00000B32000","DIALOG",.F.,.F.,lcMsg2)
      IF  lnMsgNo = 1
        REPLACE Order WITH UPPER(lcAOrder)
        =lfCretLn()
      ELSE
        REPLACE Order WITH 'N'
      ENDIF
    ELSE
      REPLACE Order WITH UPPER(lcAOrder)
    ENDIF
    lcAOrder = UPPER(lcAOrder)
    SHOW WINDOW (lcSlAtWin) REFRESH
  ENDIF
ENDIF
SHOW GETS
*!*************************************************************
*! Name      : lfCretLn
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Create New Line
*!*************************************************************
*! Example   : = lfCretLn()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfCretLn
SCATT MEMVAR MEMO
m.Po       = ''
m.Ddelvery = gdsysdate+ 7

m.ToOrder   =  lnATOrdr
m.PSupplier = lcASupplir
m.OrdQty    = lnATOrdr
m.cCurrCode = lcACurrncy
cShipTo     = lcAShipTo

SELECT (lcTmpSlAt2)
APPEND BLANK
GATHER MEMVAR MEMO

m.Ordered  = &lcTmpQuery..Ordered + lnATOrdr
m.ToOrder = m.Requierd  - lnATOrdr

lnARqured  =m.Requierd 
lnAOrdrd   =m.Ordered 
lnATOrdr   =lnARqured - lnAOrdrd 

REPLACE &lcTmpQuery..Ordered WITH lnAOrdrd,;
        &lcTmpQuery..ToOrder WITH lnATOrdr   

*!*************************************************************
*! Name      : lfvCurr
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfvRating()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvCurr

lnAlias=SELECT(0)

= gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
SELECT(lnAlias)
IF EMPTY(lcACurrncy) .OR. !SEEK(lcACurrncy,'SYCCURR') OR llbrowse
  SELECT SYCCURR
  GO TOP
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  REPLACE &lcTmpSlAt..cCurrCode WITH laTemp[1]
  lcACurrncy = laTemp[1]
  SHOW GET lcACurrncy ENABLE
  *SHOW WINDOW (lcSlAtWin) REFRESH
ENDIF


SELECT(lnAlias)
*-- end 

*!*************************************************************
*! Name      : lfvShipTo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfvShipTo()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvShipTo

lnAlias=SELECT(0)

= gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'Vencode','SH')
= gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
SELECT APVENDOR

IF EMPTY(lcAShipTo) OR llBrowse 
  llBrowse = .F.

  SELECT WAREHOUS
  GO TOP
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcFile_Ttl = "Ship To"
  lcBrFields = "cwarecode :R :H= 'WareHouse Code'," +;
               "cdesc     :R :H= 'WareHouse Desc.'"
  =gfBrows('','cwarecode','laTemp')
  lcBrFields = lcSavBrFld

  SELECT (lcTmpSlAt)
  REPLACE &lcTmpSlAt..cShipTo WITH laTemp[1]
  REPLACE &lcTmpSlAt2..cShipTo WITH laTemp[1]
  lcAShipTo = laTemp[1]
ENDIF

SELECT(lnAlias)
*-- end 
*!*************************************************************
*! Name      : lfvSupplr
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfvShipTo()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvSupplr

lnAlias=SELECT(0)

= gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'Vencode','SH')
SELECT APVENDOR
lcForExpr = "FOR 'M'$cvensuptyp AND cCurrCode = &lcTmpSlAt..cCurrCode"

IF EMPTY(lcASupplir) .OR. !SEEK(lcASupplir) OR   llBrowse 
  llBrowse = .F.
  SELECT APVENDOR
  GO TOP
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcFile_Ttl = "Supplier"
  lcBrFields = "cvendcode :R :H= 'Vendor Code'," +;
               "cvencomp  :R :H= 'Name',  " +;
               "caddress1 :R :H= 'Address'"
  =gfBrows(lcForExpr,'cvendcode','laTemp')
  lcBrFields = lcSavBrFld
  =SEEK(ALLTRIM(laTemp[1]),'APVENDOR')
  SELECT APVENDOR
  IF EMPTY(cshipto) 
    lcMsg2 = "Ship to not found for vendor " + laTemp[1]
    lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    REPLACE &lcTmpSlAt..FSupplier WITH ''
    lcASupplir = ''
    SHOW WINDOW (lcSlAtWin) REFRESH
    SELECT(lnAlias)
    _CUROBJ = OBJNUM(lcASupplir)    
    RETURN    
  ENDIF
  
  IF EMPTY(cvenemail) 
    lcMsg2 = "E.mail not found for vendor " + laTemp[1]
    lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    REPLACE &lcTmpSlAt..FSupplier WITH ''
    SHOW WINDOW (lcSlAtWin) REFRESH
    lcASupplir = ''
    _CUROBJ = OBJNUM(lcASupplir)    
    SELECT(lnAlias)
    RETURN    
  ENDIF

  REPLACE &lcTmpSlAt..FSupplier WITH laTemp[1]
  REPLACE &lcTmpSlAt2..PSupplier WITH laTemp[1]
  REPLACE &lcTmpSlAt2..FSupplier WITH laTemp[1]
  
  lcASupplir = laTemp[1]
  SHOW WINDOW (lcSlAtWin) REFRESH
ENDIF

SELECT(lnAlias)
*-- end 

*!*************************************************************
*! Name      : lfGetOld
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetOld()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfGetOld
PARAMETER lcType
SELECT (lcTmpSlAt)
IF lcType = 'U'
  lcOldVal = UPPER(UnAlocat)
  REPLACE UnAlocat WITH UPPER(UnAlocat)
ELSE
  lcOldVal = UPPER(Order)
  REPLACE Order WITH UPPER(Order)
ENDIF

*!*************************************************************
*! Name      : lfGetSlAut
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetSlAut()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfGetSlAut
PRIVATE lnTotQty,lnHsQty,lnFabQty
PRIVATE m.Required,m.Allocat,m.Ddelvery,m.cWareCode,;
        m.Price,m.UOM,m.cShipTo,m.FSupplier,m.Po,m.PSupplier,m.OrdDat

STORE '' TO m.UOM,m.cShipTo,m.FSupplier,m.Po,m.PSupplier,m.Allocat,m.cWareCode
STORE 0 TO m.Required,m.Price
STORE DATE() TO m.OrdDat,m.Ddelvery
STORE 0 TO lnTotQty,lnHsQty,lnFabQty
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
SELECT (lcTmpSlAt)
ZAP
*---------------m.Required------------------

m.Required = &lcTmpQuery..Req 

*-----------------m.Allocat  , m.PO , m.PSupplier ,m.OrdDat , m.Ddelvery ,------------------
PRIVATE lcCurOrd , lcPoMat ,lnHsQty 
lcPoMat = ''
lnHsQty  = 0
SELECT POFLN
SET ORDER TO Pofln

SELECT MAPOALO
lcCurOrd = ORDER()
SET ORDER TO Maposo
LOCATE

IF SEEK(OrdHdr.Order+ PADR(&lcTmpQuery..Item,7) +&lcTmpQuery..iclr)
  lcPoMat = MAPOALO.POMAT 
  m.Po     = ''
  
  SCAN REST WHILE order+fabric+color =  OrdHdr.Order+ PADR(&lcTmpQuery..Item,7) +&lcTmpQuery..iclr
    IF !SEEK('P'+PoMat,'POFHDR') OR (POFHDR.Status $'SX')
      LOOP
    ENDIF
    IF lcPoMat <> MAPOALO.POMAT AND !EMPTY(m.Po)
      lcPoMat = MAPOALO.POMAT 
      *--------------- POFLN.VENDOR
      SELECT (lcTmpSlAt)
      APPEND BLANK
      REPLACE Rating    WITH &lcTmpQuery..NBOMTOTQTY,;
              Requierd  WITH m.Required,;
              Ordered   WITH &lcTmpQuery..Ordered,;
              ToOrder   WITH m.Required - &lcTmpQuery..Ordered ,;
              Order     WITH 'N',;
              UnAlocat  WITH m.Allocat,;
              Ddelvery  WITH m.Ddelvery,;
              cWareCode WITH m.cWareCode,;
              Price     WITH m.Price,;
              UOM       WITH m.UOM,;
              cCurrCode WITH &lcTmpQuery..ccurrcode,;
              cShipTo   WITH m.cShipTo,;
              FSupplier WITH m.FSupplier,;
              Po        WITH m.Po,;
              PSupplier WITH m.PSupplier,;
              OrdDat    WITH m.OrdDat,;
              OrdQty    WITH lnHsQty,;
              Delete    WITH 'N'
      REPLACE ITEM WITH PADR(&lcTmpQuery..Item,7),;
              ICLR WITH &lcTmpQuery..iclr

      lnHsQty  = 0
      lnFabQty = 0
      m.Po     = MAPOALO.POMAT 
    ENDIF
    lnHsQty  = lnHsQty  + MAPOALO.NHOUSQTY 
  
    m.Po     = MAPOALO.POMAT 
    SELECT POFLN
    =SEEK('P'+MAPOALO.POMAT+PADR(&lcTmpQuery..Item,7) + &lcTmpQuery..iclr)
    SCAN REST WHILE cmattype+pomat+fabric+color+trancd='P'+MAPOALO.POMAT+PADR(&lcTmpQuery..Item,7) + &lcTmpQuery..iclr
      IF TRANCD = '1'
        lnFabQty  = lnFabQty +NFABTOTQTY 
      ENDIF
      IF TRANCD = '5'
       lnFabQty  = lnFabQty - NFABTOTQTY 
      ENDIF
      m.FSupplier = VENDOR
      IF SEEK(cmattype+pomat,'Pofhdr')
        m.OrdDat    = Pofhdr.entered
        m.Ddelvery  = Pofhdr.Complete
        m.cWareCode = POFHDR.cWareCode
        m.PSupplier = POFHDR.VENDOR
      ENDIF
    ENDSCAN

    *--------------- m.Price , m.UOM , m.FSupplier---------------
    SELECT FABRIC
    IF '*' $ &lcTmpQuery..iclr
      lcColr = ''
    ELSE
      lcColr = &lcTmpQuery..iclr
    ENDIF

    IF SEEK(PADR(&lcTmpQuery..Item,7) + lcColr)
      IF FABRIC.CONV <> 1.00
        m.Price = FABRIC.COSTBUY
      ELSE
        IF &lcTmpQuery..nexrate = 1
          m.Price = &lcTmpQuery..UntCost
        ELSE
          m.Price = &lcTmpQuery..nexcost
        ENDIF
      ENDIF
      m.UOM = Fabric.UOMBUY  
      IF EMPTY(m.FSupplier)
        m.FSupplier = Fabric.Vendor 
        IF SEEK(m.FSupplier,'APVENDOR')
          m.cShipTo = APVENDOR.cShipTo
        ENDIF
      ELSE
        IF SEEK(m.FSupplier,'APVENDOR')
          m.cShipTo = APVENDOR.cShipTo
        ENDIF
      ENDIF
    ENDIF 
  ENDSCAN
  lcPoMat = MAPOALO.POMAT 
  m.Allocat = 'N'  
  *--------------- POFLN.VENDOR
  SELECT (lcTmpSlAt)
  APPEND BLANK
  REPLACE Rating    WITH &lcTmpQuery..NBOMTOTQTY,;
          Requierd  WITH m.Required,;
          Ordered   WITH &lcTmpQuery..Ordered,;
          ToOrder   WITH m.Required - &lcTmpQuery..Ordered ,;
          Order     WITH 'N',;
          UnAlocat  WITH m.Allocat,;
          Ddelvery  WITH m.Ddelvery,;
          cWareCode WITH m.cWareCode,;
          Price     WITH m.Price,;
          UOM       WITH m.UOM,;
          cCurrCode WITH &lcTmpQuery..ccurrcode,;
          cShipTo   WITH m.cShipTo,;
          FSupplier WITH m.FSupplier,;
          Po        WITH m.Po,;
          PSupplier WITH m.PSupplier,;
          OrdDat    WITH m.OrdDat,;
          OrdQty    WITH lnHsQty,;
          Delete    WITH 'N'
  REPLACE ITEM WITH PADR(&lcTmpQuery..Item,7),;
          ICLR WITH &lcTmpQuery..iclr
ELSE
  SELECT POFLN
  lcCrrOrdr = ORDER()
  SET ORDER TO POFLNF
  
  =SEEK(PADR(&lcTmpQuery..Item,7) + &lcTmpQuery..iclr)
  SCAN REST WHILE fabric+color+cmattype+pomat+trancd=PADR(&lcTmpQuery..Item,7) + &lcTmpQuery..iclr
    IF !SEEK('P'+m.Po,'POFHDR') OR (POFHDR.Status $'SX')
      LOOP
    ENDIF
    IF TRANCD = '1'
      lnFabQty  = lnFabQty +NFABTOTQTY 
    ENDIF
    IF TRANCD = '5'
     lnFabQty  = lnFabQty - NFABTOTQTY 
    ENDIF
    m.FSupplier = VENDOR
    IF SEEK(cmattype+pomat,'Pofhdr')
      m.OrdDat    = Pofhdr.entered
      m.Ddelvery  = Pofhdr.Complete
      m.cWareCode = POFHDR.cWareCode
      m.PSupplier = POFHDR.VENDOR
    ENDIF
  ENDSCAN

  m.Allocat = 'N'  
  SELECT FABRIC
  IF '*' $ &lcTmpQuery..iclr
    lcColr = ''
  ELSE
    lcColr = &lcTmpQuery..iclr
  ENDIF

  IF SEEK(PADR(&lcTmpQuery..Item,7) + lcColr)
    IF FABRIC.CONV <> 1.00
      m.Price = FABRIC.COSTBUY
    ELSE
      IF &lcTmpQuery..nexrate = 1
        m.Price = &lcTmpQuery..UntCost
      ELSE
        m.Price = &lcTmpQuery..nexcost
      ENDIF
    ENDIF
    m.UOM = Fabric.UOMBUY  
    IF EMPTY(m.FSupplier)
      m.FSupplier = Fabric.Vendor 
      IF SEEK(m.FSupplier,'APVENDOR')
        m.cShipTo = APVENDOR.cShipTo
      ENDIF
    ELSE
      IF SEEK(m.FSupplier,'APVENDOR')
        m.cShipTo = APVENDOR.cShipTo
      ENDIF
    ENDIF
  ENDIF 
  m.Allocat = 'N'  
  SELECT (lcTmpSlAt)
  APPEND BLANK
  REPLACE Rating    WITH &lcTmpQuery..NBOMTOTQTY,;
          Requierd  WITH m.Required,;
          Ordered   WITH &lcTmpQuery..Ordered,;
          ToOrder   WITH m.Required - &lcTmpQuery..Ordered ,;
          Order     WITH 'N',;
          UnAlocat  WITH m.Allocat,;
          Ddelvery  WITH m.Ddelvery,;
          cWareCode WITH m.cWareCode,;
          Price     WITH m.Price,;
          UOM       WITH m.UOM,;
          cCurrCode WITH &lcTmpQuery..ccurrcode,;
          cShipTo   WITH m.cShipTo,;
          FSupplier WITH m.FSupplier,;
          Po        WITH m.Po,;
          PSupplier WITH m.PSupplier,;
          OrdDat    WITH m.OrdDat,;
          OrdQty    WITH lnHsQty,;
          Delete    WITH 'N'
  REPLACE ITEM WITH PADR(&lcTmpQuery..Item,7),;
          ICLR WITH &lcTmpQuery..iclr
ENDIF
SELECT MAPOALO
SET ORDER TO &lcCurOrd 

*!*************************************************************
*! Name      : lfvClear
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvClear()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvClear

llChkRtrn = .T.

lcMsg2 = 'Changes will be lost. Are you sure you want to clear ?'
lnMsgNo=gfModalGen("TRM00000B32000","DIALOG",.F.,.F.,lcMsg2)
IF  lnMsgNo = 1
  lnToOrdULo = 0
  lnToOrdALo = 0
  SELECT (lcTmpCutPk)
  ZAP
  SELECT (lcTmpRel)
  ZAP
  SELECT (lcTmpSlAt)
  ZAP
  SELECT (lcTmpSlAt2)
  ZAP

  SELECT (lcTmpQuery)
  ZAP
  CLEAR READ
  =lfGetData()
  =lfvOpSumm()
  
  SELECT (lcTmpSlAt2)
  DELETE ALL FOR Item+iclr = PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr
  
ENDIF
	
*!*************************************************************
*! Name      : lfvGenNot
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvGenNot()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvGenNot
PRIVATE lcCurPo
lcCurPo = &lcTmpSlAt2..PO
lcMsg2 = "Update Notes to all Purchase Orders attached to Sales Order " + OrdHdr.order
lnMsgNo=gfModalGen("TRM00000B32000","DIALOG",.F.,.F.,lcMsg2)
=NotePad('M','P'+lcCurPo)
=lfgetPos()
= gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')
= gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH')
SELECT NOTEPAD
SET ORDER TO NOTEPAD
=SEEK('M' + 'P'+lcCurPo)
SCATTER FIELDS mnotes MEMVAR MEMO
IF EMPTY(lcCurPo)
  DELETE
  SELECT(lcTmpSlAt2)
  lnCurRecno = RECNO()
  SCAN FOR EMPTY(PO)
    GATHER FIELDS mnotes MEMVAR MEMO
  ENDSCAN
  GOTO lnCurRecno
ENDIF

*-Update the note to all PO's

IF lnMsgNo = 1
  SELECT (lcTmpPoNot)
  *SCAN FOR !EMPTY(PO)
  SCAN 
    IF !EMPTY(PO)
      IF SEEK('P'+PO,'POFHDR')
        SELECT POFHDR
        REPLACE lhasnotes WITH .T.
      ENDIF
      IF SEEK('MP' + &lcTmpPoNot..PO,'NOTEPAD')
        SELECT NOTEPAD  
        GATHER FIELDS mnotes MEMVAR MEMO
      ELSE
        SELECT NOTEPAD  
        APPEND BLANK
        GATHER FIELDS mnotes MEMVAR MEMO
        REPLACE Type  WITH 'M',;
                KEY   WITH 'P'+&lcTmpPoNot..PO,;
                cdesc WITH "Notes For Item P/o Number : " + &lcTmpPoNot..PO
      ENDIF  
    ELSE
      GATHER FIELDS mnotes MEMVAR MEMO
    ENDIF
  ENDSCAN
ELSE  &&just PO's for the vendor displayed
  lcCurVend = &lcTmpSlAt2..PSupplier 
  SELECT (lcTmpPoNot)
  SCAN FOR  PSupplier = lcCurVend
    IF !EMPTY(PO) 
      IF SEEK('P'+PO,'POFHDR')
        SELECT POFHDR
        REPLACE lhasnotes WITH .T.
      ENDIF
      IF SEEK('MP' + &lcTmpPoNot..PO,'NOTEPAD')
        SELECT NOTEPAD  
        GATHER FIELDS mnotes MEMVAR MEMO
      ELSE
        SELECT NOTEPAD  
        APPEND BLANK
        GATHER FIELDS mnotes MEMVAR MEMO
        REPLACE Type  WITH 'M',;
                KEY   WITH 'P' + &lcTmpPoNot..PO,;
                cdesc WITH "Notes For Item P/o Number : " + &lcTmpPoNot..PO
      ENDIF
    ELSE
      GATHER FIELDS mnotes MEMVAR MEMO
    ENDIF  
  ENDSCAN
ENDIF
*!*************************************************************
*! Name      : lfvNext
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNext()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvNext

SELECT (lcTmpCutPk)
ZAP
SELECT (lcTmpRel)
ZAP
SELECT (lcTmpSlAt)
ZAP

SELECT (lcTmpQuery)
SKIP
IF EOF()
  GOTO BOTT
  lcMsg2 = "Last record, can not proceed"
  lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  =lfSLATBrow()
  SHOW WINDOW (lcSlAtWin) REFRESH
  RETURN
ENDIF
=lfGetSlAut()
SELECT (lcTmpSlAt)
LOCATE
SCAN FOR !EMPTY(PO)
  SCATT MEMVAR MEMO
  IF !SEEK( PO + PSupplier + cShipTo+  cCurrCode + DTOS(Ddelvery) ,lcTmpSlAt2)
    SELECT (lcTmpSlAt2)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF  
ENDSCAN
SELECT (lcTmpSlAt2)
SET FILTER TO Item+iclr = PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr

SELECT (lcTmpSlAt)
LOCATE
lnARate    =Rating 
lnARqured  =Requierd 
lnAOrdrd   =Ordered 
lnATOrdr   =ToOrder 
lcAUnAllct = ''
=lfChckAlct()
lnAPrice   =Price 
lcAUOM     =UOM 
lcACurrncy =cCurrCode 
lcAShipTo  =cShipTo 
lcASupplir = FSupplier 
lcAOrder   = Order 
SHOW GETS

IF UPPER(lcAUnAllct)  = 'Y'
  SHOW GET PbAlocate ENABLE
ELSE
  SHOW GET PbAlocate DISABLE
ENDIF

=lfSLATBrow()
SHOW WINDOW (lcSlAtWin) REFRESH
=lfRefresh()
*!*************************************************************
*! Name      : lfvPrev
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPrev()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvPrev

SELECT (lcTmpCutPk)
ZAP
SELECT (lcTmpRel)
ZAP
SELECT (lcTmpSlAt)
ZAP
SELECT (lcTmpQuery)
SKIP-1
IF BOF()
  LOCATE
  lcMsg2 = "First record, can not proceed"
  lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  =lfSLATBrow()
  SHOW WINDOW (lcSlAtWin) REFRESH
  RETURN
ENDIF
=lfGetSlAut()
SELECT (lcTmpSlAt)
LOCATE
SCAN FOR !EMPTY(PO)
  SCATT MEMVAR MEMO
  IF !SEEK( PO + PSupplier + cShipTo+  cCurrCode + DTOS(Ddelvery) ,lcTmpSlAt2)
    SELECT (lcTmpSlAt2)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF  
ENDSCAN
SELECT (lcTmpSlAt2)
SET FILTER TO Item+iclr = PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr

SELECT (lcTmpSlAt)
LOCATE
lnARate    =Rating 
lnARqured  =Requierd 
lnAOrdrd   =Ordered 
lnATOrdr   =ToOrder 
lcAUnAllct = ''
=lfChckAlct()
lnAPrice   =Price 
lcAUOM     =UOM 
lcACurrncy =cCurrCode 
lcAShipTo  =cShipTo 
lcASupplir = FSupplier 
lcAOrder   = Order 
SHOW GETS
IF UPPER(lcAUnAllct)  = 'Y'
  SHOW GET PbAlocate ENABLE
ELSE
  SHOW GET PbAlocate DISABLE
ENDIF

=lfSLATBrow()
SHOW WINDOW (lcSlAtWin) REFRESH
=lfRefresh()

*!*************************************************************
*! Name      : lfNewPo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfNewPo()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfNewPo
PRIVATE llUpdate , llChkMsg
STORE .F. TO llUpdate , llChkMsg
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
= gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')
= gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')

SELECT (lcTmpSlAt2)
SET FILTER TO 
SCAN FOR EMPTY(PO)
  llChkMsg = .T.
  EXIT
ENDSCAN
IF llChkMsg

  lcMsg2 = "Do you wish to create new PO's ?"
  lnMsgNo=gfModalGen("TRM00000B32000","DIALOG",.F.,.F.,lcMsg2)
  IF lnMsgNo = 1
    lcVendor  = ''
    lcCurrncy = ''
    lcshipto  = ''
    ldComplt  =  {}
    lnLastLine = 0
  
    SELECT (lcTmpSlMal)
    ZAP
    SELECT (lcTmpSlAt2)
    lcCurPo = PO
    SET FILTER TO
    SCAN FOR EMPTY(PO)
      IF !SEEK(lcCurPo+PSupplier+cShipTo+ cCurrCode+DTOS(Ddelvery),lcTmpSlMal)
        lnLastLine = 0
      
        SCATTER MEMVAR MEMO
        SELECT (lcTmpSlMal)
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF  
    ENDSCAN

    SELECT (lcTmpSlMal)
    LOCATE
    SCAN
        lcCurPo = PO
        m.Po = gfSequence('POMAT')
        m.Ddelvery  = Ddelvery
        m.FSupplier = FSupplier
        lcVendor   =  FSupplier
        lcCurrncy = cCurrCode
        lcshipto  = cShipTo
        m.cWareCode = lcshipto
        ldComplt  =  Ddelvery
        lnLastLine = 0
        =SEEK(lcCurrncy,'syccurr')
        lnUnit1     = syccurr.nCurrUnit
        m.PurRate = gfchkrate('lnUnit1',lcCurrncy,gdSysDate,.F.,gcAct_Comp,.F.) 

        SELECT (lcTmpSlAt2)
        SCAN FOR PO+FSupplier+cShipTo+cCurrCode+DTOS(Ddelvery) = lcCurPo+lcVendor+lcshipto+ lcCurrncy+DTOS(ldComplt)
          lnLastLine  = 1+lnLastLine 
          =lfUpdLines()
        ENDSCAN
        =lfUpdHdr()
        =lfUpdVend(&lcTmpSlAt2..PSupplier)
        =SEEK(gcuser_id,'SYUUSER')
        =SEEK(m.PSupplier ,'APVENDOR')
        SELECT (lcTmpNewPo)
        APPEND BLANK
        REPLACE Po        WITH m.Po,;
                Ddelvery  WITH m.Ddelvery,;
                PSupplier WITH m.FSupplier,;
                Email     WITH 'N',;
                Print     WITH 'Y',;
                EmailFrom WITH SYUUSER.cusremail; 
                EmailTo   WITH APVENDOR.cvenemail
    ENDSCAN
    SELECT (lcTmpSlMal) 
    LOCATE
    SCAN
      IF !EMPTY(&lcTmpSlMal..mnotes )
          SCATTER FIELDS mnotes MEMVAR MEMO
      ENDIF
    ENDSCAN  
      
    SELECT (lcTmpNewPo)
    SCAN
      IF !SEEK('MP' + &lcTmpNewPo..PO,'NOTEPAD')
        SELECT NOTEPAD  
        APPEND BLANK
        GATHER FIELDS mnotes MEMVAR MEMO
        REPLACE Type  WITH 'M',;
                KEY   WITH 'P'+&lcTmpNewPo..PO,;
                cdesc WITH "Notes For Item P/o Number : " + &lcTmpNewPo..PO
      ENDIF  
    ENDSCAN  
    
    =lfUpdFab()
  
    SELECT (lcTmpSlAt)
    DELETE ALL FOR EMPTY(PO)

    SELECT (lcTmpQuery)
    ZAP
    =lfGetData()
    =lfActsBrow()  
  
    PUSH KEY
    DO (gcScrDir+"SO\SonewPo.SPX")
    POP KEY
    KEYBOARD "{ALT+B}" CLEAR
    SELECT (lcTmpSlAt2)
    ZAP

  ELSE
    SELECT (lcTmpCutPk)
    ZAP
    SELECT (lcTmpRel)
    ZAP
    SELECT (lcTmpSlAt)
    ZAP
    SELECT (lcTmpSlAt2)
    ZAP
  ENDIF
ELSE
  PUSH KEY
  DO (gcScrDir+"SO\SonewPo.SPX")
  POP KEY
  KEYBOARD "{ALT+B}" CLEAR
ENDIF  
*!*************************************************************
*! Name      : lfNewBrow
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfNewBrow()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfNewBrow

SELECT (lcTmpNewPo)
GO TOP
lcNewLBrow  = [Po        :06  :H = 'Po no'         :R,]+;
             [Ddelvery  :08  :H = 'Delivery Date'  :R,]+;
             [PSupplier :08  :H = 'Supplier'       :R,]+;
             [EmailFrom :30  :H = 'Email From'     :V=.T.,]+;
             [EmailTo   :30  :H = 'Email To'       :V=.T.,]+;
             [Email     :05  :H = 'Email'          :V=.T.,]+;
             [Print     :05  :H = 'Print'          :V=.T.]
             
BROWSE FIELDS &lcNewLBrow;
       WINDOW soNewPo1 IN WINDOW soNewPo;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcNewBrow)

*!*************************************************************
*! Name      : lfReadNew
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfReadAMul()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfReadNew
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcNewBrow)

*!*************************************************************
*! Name      : lfvReNew
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfDDisMul()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvReNew

ON KEY LABEL CTRL+Q lnDummy = 1
ON KEY LABEL CTRL+W lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM
RETURN .F.

*!*************************************************************
*! Name      : lfvSndEmal
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNext()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvSndEmal
PRIVATE  lcBasTabl , lcOldMod 
STORE '' TO lcBasTabl , lcOldMod 

SELECT (lcTmpNewPo)
LOCATE FOR UPPER(Email) = 'Y'
IF EOF()
  _CUROBJ = OBJNUM(IbMulDist)
  RETURN
ENDIF

SELECT (lcTmpNewPo)
SCAN FOR UPPER(Email) = 'Y'
 IF EMPTY(EmailFrom)
    lcMsg2 = 'Please enter Email From'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    RETURN  
 ENDIF
 
 IF EMPTY(EmailTo)
   lcMsg2 = 'Please enter Email TO'
   =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
   RETURN  
 ENDIF
ENDSCAN

= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
= gfOpenFile(gcDataDir+'POFLN','POFLN','SH')
lcResouce = SET('RESOU')
SET RESOU ON
=SYS(1037)
SET RESOU &lcResouce

lcOldMod = gcAct_Appl
lcBasTabl = lcBaseFile
lcReportID = "MAMATP"
lcBaseFile = "POFHDR"
gcAct_Appl = "MA"
gcWinAppl  = "MA"
SELECT  POFHDR

lcNewMPos = gfTempName()
*--show printer driver in windows

CREATE CURSOR (lcNewMPos) (POMAT C(6))
INDEX ON POMAT TAG (lcNewMPos) OF (lcNewMPos)

SELECT (lcTmpNewPo)
SCAN FOR UPPER(Email) = 'Y'
  lcOldMod = gcAct_Appl
  lcBasFil = lcBaseFile
  lcReportID = "MAMATP"
  lcBaseFile = "POFHDR"
  gcAct_Appl = "MA"
  gcWinAppl  = "MA"

  SELECT(lcNewMPos)
  APPEND BLANK
  REPLACE POMAT WITH   &lcTmpNewPo..PO
  =gfCPPrint()

  lcBaseFile = lcBasTabl  
  gcWinAppl = lcOldMod
  gcAct_Appl = lcOldMod
  lcFrmEmail = ALLTRIM(&lcTmpNewPo..EmailFrom)
  lcToEmail  = ALLTRIM(&lcTmpNewPo..EmailTo)  

  lcSubject  = "Po No. "+ ALLTRIM(&lcTmpNewPo..PO)
  RENAME gcWorkdir + "FoxPro.pdf" TO gcWorkdir + ALLTRIM(&lcTmpNewPo..PO) +".pdf"
  lcFile1   = gcWorkdir + ALLTRIM(&lcTmpNewPo..PO) +".pdf"
  lcMailServ  = gfGetMemVar('M_MAILSERV')   && setting Get Mail Server

  lcPath    = gcDef_Path+"SOSend.EXE"

  run /n &lcPath &lcFrmEmail,&lcToEmail,&lcSubject,&lcFile1,&lcMailServ
  run /n &lcPath &lcFrmEmail,&lcFrmEmail,&lcSubject,&lcFile1,&lcMailServ
  DELETE FILE gcWorkdir + ALLTRIM(&lcTmpNewPo..PO) +".pdf"
  SELECT (lcTmpNewPo)
  REPLACE   Email     WITH 'N'
  SELECT(lcNewMPos)
  ZAP

ENDSCAN

lcBaseFile = lcBasTabl  
gcWinAppl  = lcOldMod
gcAct_Appl = lcOldMod


SELECT (lcTmpNewPo)
LOCATE

SELECT (lnAlias)
_CUROBJ = OBJNUM(IbMulDist)
*!*************************************************************
*! Name      : lfvPrint
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To Print the report needed
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPrint()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvPrint
PRIVATE  lcBasTabl , lcOldMod 
STORE '' TO lcBasTabl , lcOldMod 

lnAlias=SELECT(0)

= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
= gfOpenFile(gcDataDir+'POFLN','POFLN','SH')
SELECT (lcTmpNewPo)
LOCATE FOR UPPER(Print) = 'Y'
IF EOF()
  _CUROBJ = OBJNUM(IbMulDist)
  RETURN
ENDIF

lcNewMPos = gfTempName()

CREATE CURSOR (lcNewMPos) (POMAT C(6))
INDEX ON POMAT TAG (lcNewMPos) OF (lcNewMPos)
SELECT (lcTmpNewPo)
SCAN  FOR UPPER(Print) = 'Y'
  SELECT(lcNewMPos)
  APPEND BLANK
  REPLACE POMAT WITH   &lcTmpNewPo..PO
ENDSCAN
*--show printer driver in windows
lcResouce = SET('RESOU')
SET RESOU ON
=SYS(1037)
SET RESOU &lcResouce

lcOldMod = gcAct_Appl
lcBasTabl = lcBaseFile
lcReportID = "MAMATP"
lcBaseFile = "POFHDR"
gcAct_Appl = "MA"
gcWinAppl  = "MA"
SELECT  POFHDR

=gfCPPrint()

lcBaseFile = lcBasTabl
gcWinAppl = lcOldMod
gcAct_Appl = lcOldMod

SELECT (lcTmpNewPo)
REPLACE ALL Print     WITH 'N'

SELECT (lcTmpNewPo)
GO TOP
=lfNewBrow()

SELECT (lnAlias)
_CUROBJ = OBJNUM(IbMulDist)

*!*************************************************************
*! Name      : lfSODELALO
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To Print the report needed
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = SODELALO()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfSODELALO
lnAlias=SELECT(0)

= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')

SELECT MAPOALO
lcFormName   = 'SODELAlO'
lcSetConsl = SET('CONSOLE')    && save console setting
OGPlatForm   = 'DOS'
lcOGPlatForm = OGPlatForm
gcDevice     = "PRINTER"
SET CONSOLE OFF
IF !pSetup(.T.)
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SELECT MAPOALO
SET RELATION TO 'P'+pomat INTO PofHdr
SELECT MAPOALO
LOCATE
lcRpExp = "order= ORDHDR.ORDER"
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

SET DEVICE TO SCREEN
SET CONSOLE &lcSetConsl

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvSavPo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : To Save the PO
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSavPo()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvSavPo
PRIVATE  lcOldOrd ,lnCurrRec , lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpQuery)

= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
= gfOpenFile(gcDataDir+'POFLN','POFLN','SH')
= gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'Vencode','SH')

SELECT(lcTmpSlAt)
DELETE ALL FOR UPPER(Delete ) ='Y' AND EMPTY(PO)

SELECT(lcTmpSlAt)
SCAN FOR EMPTY(PO)
  IF EMPTY(cshipto) 
    lcMsg2 = "Ship TO can not be empty"
    lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    RETURN    
  ENDIF
  
  IF EMPTY(FSupplier) 
    lcMsg2 = "Vendor code can not be empty"
    lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    SELECT(lnAlias)
    RETURN    
  ENDIF
ENDSCAN

*--Validate Delete records Needed
SELECT (lcTmpSlAt)
LOCATE

SCAN FOR UPPER(Delete ) ='Y' AND !EMPTY(PO)
  SELECT MAPOALO  
  SET ORDER TO Mapoalo
  IF SEEK(&lcTmpSlAt..PO+PADL(&lcTmpSlAt..Item,7)+&lcTmpSlAt..iclr)
    
    IF SEEK('P'+POMAT,'POFHDR') AND (POFHDR.Status $'O')
      lcScanExp = "P"+POMat+PADL(&lcTmpSlAt..Item,7)+&lcTmpSlAt..iclr
      SELECT POFLN
      =SEEK(lcScanExp)
      llChkRecv = .F. 
      SCAN REST WHILE cmattype+pomat+fabric+color+trancd = lcScanExp
        IF Trancd ="2"
          llChkRecv = .T. 
          EXIT
        ENDIF
      ENDSCAN
      IF llChkRecv 
        LOOP
      ENDIF
      SELECT POFLN
      =SEEK(lcScanExp)
      REPLACE  POFHDR.NFabOrder WITH  POFHDR.NFabOrder - POFLN.nfabtotqty
      
      DELETE
    ELSE
      LOOP  
    ENDIF  
    SELECT MAPOALO  
    DELETE
    IF SEEK(&lcTmpSlAt..Item+&lcTmpSlAt..Iclr,'FABRIC')
      SELECT FABRIC
      =RLOCK()
      REPLACE FABRIC.ONORDER WITH FABRIC.ONORDER - &lcTmpSlAt..OrdQty*CONV
      UNLOCK
      SELECT FABDYE
      SET ORDER TO TAG FABDYE
      IF SEEK(&lcTmpSlAt..Item+&lcTmpSlAt..Iclr+&lcTmpSlAt..cWareCode,'FABDYE')
        REPLACE FABDYE.ONORDER WITH FABDYE.ONORDER - &lcTmpSlAt..OrdQty*FABRIC.CONV
      ENDIF
      UNLOCK
    ENDIF
    SELECT APVENDOR  
    =SEEK(&lcTmpSlAt..FSupplier)
    =RLOCK()
    REPLACE nVenOpnPO WITH nVenOpnPO - POFLN.nEcost1 
    UNLOCK        
    
  ENDIF

ENDSCAN
SELECT (lcTmpSlAt)

DELETE ALL FOR UPPER(Delete ) ='Y' AND !EMPTY(PO)
LOCATE
llChkRtrn = .T.
lnToOrdULo = 0
lnToOrdALo = 0

REPLACE &lcTmpQuery..NBOMTOTQTY  WITH  &lcTmpSlAt..Rating


SELECT MAPOALO  
lcOldOrd = ORDER()

SELECT(lcTmpQuery)
lnCurrRec = RECNO()
SCAN 
  SCATT MEMVAR MEMO
  =SEEK(order+STR(lineno,6)+style+sclr+item+iclr+mfgcode,'ORDBOM')
  IF EOF('ORDBOM') 
    =SEEK(order+STR(lineno,6)+style+sclr+item+"******"+mfgcode,'ORDBOM')
    m.iclr = '******'
  ENDIF
  SELECT ORDBOM
  GATHER MEMVAR MEMO
ENDSCAN

*--update Alocated
SELECT (lcTmpCutPk)
LOCATE 
IF !EOF()
  SCAN FOR !EMPTY(QtyToAlct)
    SELECT MAPOALO 
    SET ORDER TO Mapoalo
    IF SEEK(&lcTmpCutPk..PO+PADL(&lcTmpCutPk..Item,7)+&lcTmpCutPk..iclr+OrdHdr.Order)
      REPLACE nhousqty WITH nhousqty + &lcTmpCutPk..QtyToAlct
    ELSE
      APPEND BLANK
      REPLACE pomat    WITH &lcTmpCutPk..PO,;
              Fabric   WITH &lcTmpCutPk..Item,;
              Color    WITH &lcTmpCutPk..Iclr,;
              Order    WITH OrdHdr.order,;
              nhousqty WITH &lcTmpCutPk..QtyToAlct
      =gfAdd_Info('MAPOALO')
    ENDIF
  ENDSCAN
ENDIF
*--update UnAlocated

SELECT (lcTmpRel)
SCAN FOR !EMPTY(QtyToAlct)
  SELECT MAPOALO  
  SET ORDER TO Mapoalo
  =SEEK(&lcTmpRel..PO+PADL(&lcTmpRel..Item,7)+&lcTmpRel..iclr+&lcTmpRel..Order,'MAPOALO')
  SELECT MAPOALO  
  lnSubQty = nhousqty - &lcTmpRel..QtyToAlct
  SCAN REST WHILE pomat+fabric+color+order = &lcTmpRel..PO+PADL(&lcTmpRel..Item,7)+&lcTmpRel..iclr+&lcTmpRel..Order
    IF lnSubQty>0
      REPLACE nhousqty WITH nhousqty - &lcTmpRel..QtyToAlct
      EXIT
    ELSE
      IF  lnSubQty = 0 
        REPLACE nhousqty WITH 0
        EXIT
      ENDIF
      lnSubQty = &lcTmpRel..QtyToAlct - nhousqty 
      REPLACE nhousqty WITH 0
    ENDIF  
  ENDSCAN
  SELECT MAPOALO  
  DELETE ALL FOR nhousqty = 0
ENDSCAN


SELECT (lcTmpRel)
ZAP
SELECT (lcTmpQuery)
ZAP
=lfGetData()
SELECT (lcTmpQuery)
IF BETWEEN(lnCurrRec,1,RECCOUNT())
  GOTO lnCurrRec
  SELECT (lcTmpSlAt)
  SCAN FOR EMPTY(PO)
    SCATT MEMVAR MEMO
    SELECT (lcTmpSlAt2)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
  =lfGetSlAut()
  SELECT (lcTmpSlAt2)
  SCAN 
    SCATT MEMVAR MEMO
    SELECT (lcTmpSlAt)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
  
  =lfSLATBrow()
ENDIF

SELECT (lcTmpSlAt)
LOCATE
lnARate    =Rating 
lnARqured  =Requierd 
lnAOrdrd   =Ordered 
lnATOrdr   =ToOrder 
lcAUnAllct =IIF(EMPTY(lcAUnAllct),UnAlocat,lcAUnAllct)
lnAPrice   =Price 
lcAUOM     =UOM 
lcACurrncy =cCurrCode 
lcAShipTo  =cShipTo 
lcASupplir = FSupplier 
lcAOrder   = Order 
SHOW GETS

SELECT MAPOALO  
SET ORDER TO &lcOldOrd        

SHOW WINDOW (lcSlAtWin) REFRESH
*!*************************************************************
*! Name        : lfUpdFab
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : update the fabric file
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*! Modifications: 
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfUpdFab

= gfOpenFile(gcDataDir+'FABDYE','FABDYE','SH')

lnAlias = SELECT()
SELECT(lcTmpSlAt2)
SCAN FOR EMPTY(PO)
  IF SEEK(&lcTmpSlAt2..Item+&lcTmpSlAt2..Iclr,'FABRIC')
    SELECT FABRIC
    =RLOCK()
    REPLACE FABRIC.ONORDER WITH FABRIC.ONORDER + &lcTmpSlAt2..OrdQty*CONV
    UNLOCK
    SELECT FABDYE
    SET ORDER TO TAG FABDYE
    IF !SEEK(&lcTmpSlAt2..Item+&lcTmpSlAt2..Iclr+&lcTmpSlAt2..cWareCode,'FABDYE')
      DO gpAdFabWar WITH &lcTmpSlAt2..Item, &lcTmpSlAt2..Iclr,SPACE(10), &lcTmpSlAt2..cWareCode
    ENDIF
    REPLACE FABDYE.ONORDER WITH FABDYE.ONORDER + &lcTmpSlAt2..OrdQty*FABRIC.CONV
    UNLOCK

  ENDIF

ENDSCAN

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdVend
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose   : update for the vendor file
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls     : 
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfUpdVend
PARAMETER lcVencode
lnAlias = SELECT()
SELECT APVENDOR  
=SEEK(lcVencode)
=RLOCK()
REPLACE nVenOpnPO WITH nVenOpnPO + POFHDR.nEcost1 
UNLOCK        
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdLines
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose   : to update POFLN file 
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴         
*! Calls     : 
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfUpdLines

lnAlias = SELECT()
SELECT (lcTmpSlAt2)

laData[23] = gcbasecurr
laData[25] = gcbasecurr
lcFrnSign1 = gfGetExSin("",ALLTRIM(laData[23]))
lcFrnSign2 = gfGetExSin("",ALLTRIM(laData[25]))  
lcFrnSmbl1 = '*'
lcFrnSmbl2 = '*'

m.FABRIC   = &lcTmpSlAt2..Item
m.COLOR    = &lcTmpSlAt2..Iclr
 
m.LineNo   = lnLastLine 
m.TRANCD   = '1'        
m.cMatType = 'P' 
  
SELECT FABRIC
IF '*' $ &lcTmpSlAt2..iclr
  lcColr = ''
ELSE
  lcColr = &lcTmpSlAt2..iclr
ENDIF

=SEEK(&lcTmpSlAt2..Item + lcColr)
m.cWareCode = &lcTmpSlAt2..cShipTo
m.nCost1   = &lcTmpSlAt2..Price
m.nCost2   = FABRIC.nItm_frt
m.nCost3   = FABRIC.nItem_tax
m.nCost4   = FABRIC.nItemquota
m.neCost1  = &lcTmpSlAt2..Price
m.neCost2  = FABRIC.nItm_frt
m.neCost3  = FABRIC.nItem_tax
m.neCost4  = FABRIC.nItemquota
m.PATTERN  = FABRIC.PATTERN
m.WIDTH    = FABRIC.WIDTH    
m.POMAT      = m.Po
m.VENDOR     = &lcTmpSlAt2..FSupplier
m.cMatType   = 'P'
m.cFabGrade  = FABRIC.cFabGrade
m.nFabTotQty = &lcTmpSlAt2..ToOrder
m.Price = 0
SELECT POFLN
APPEND BLANK
GATHER MEMVAR MEMO
=gfAdd_Info('POFLN')
 
SELECT MAPOALO
APPEND BLANK
REPLACE pomat    WITH m.PO,;
        Fabric   WITH &lcTmpSlAt2..Item,;
        Color    WITH &lcTmpSlAt2..Iclr,;
        Order    WITH OrdHdr.order,;
        nhousqty WITH &lcTmpSlAt2..ToOrder
=gfAdd_Info('MAPOALO')

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdHdr
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose   : to update POFHDR file 
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls     : 
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfUpdHdr

m.POMAT      = m.Po
SELECT POFLN
SET ORDER TO POFLN
ladata[27] = 0
ladata[28] = 0
ladata[29] = 0
ladata[30] = 0
ladata[39] = 0
ladata[40] = 0
ladata[41] = 0
ladata[42] = 0
=SEEK('P'+m.POMAT)
m.VENDOR     = VENDOR
m.cWareCode  = cWareCode 
m.NFabOrder  = 0

SCAN REST WHILE cmattype+pomat+fabric+color+trancd= 'P'+m.POMAT
  ladata[39] = ladata[39] + (nCost1*nFabTotQty)
  ladata[40] = ladata[40] + (nCost2*nFabTotQty)
  ladata[41] = ladata[41] + (nCost3*nFabTotQty)
  ladata[42] = ladata[42] + (nCost4*nFabTotQty)     

  ladata[27] = ladata[27] + (nECost1*nFabTotQty)
  ladata[28] = ladata[28] + (nECost2*nFabTotQty)
  ladata[29] = ladata[29] + (nECost3*nFabTotQty)
  ladata[30] = ladata[30] + (nECost4*nFabTotQty)     
  m.NFabOrder  =m.NFabOrder + POFLN.nfabtotqty
ENDSCAN

SELECT POFHDR
APPEND BLANK
REPLACE POMAT      WITH m.Po ,;
        cMatType   WITH 'P' ,;  
        VENDOR     WITH m.VENDOR,;
        ENTERED    WITH gdSysDate,;
        COMPLETE   WITH m.Ddelvery,;
        STATUS     WITH 'O',;
        cdivision  WITH 'N/A   ',;
        link_Code  WITH 'DEFDEF',;
        CONTACT    WITH ApVendor.cvencont,;
        PHONE      WITH ApVendor.cphoneno,;
        CPRICECUR  WITH lcCurrncy,;
        CDUTYCUR   WITH gcbasecurr,;
        NPRICERAT  WITH m.PurRate,;
        NDUTYRAT   WITH 1,;
        cWareCode  WITH m.cWareCode,; 
        NFabOrder  WITH m.NFabOrder,;  
        lastline   WITH lnLastLine     
        
REPLACE nCost1     WITH ladata[39] ,;
        nCost2     WITH ladata[40] ,;
        nCost3     WITH ladata[41] ,;
        nCost4     WITH ladata[42] ,;
        nECost1    WITH ladata[27] ,;
        nECost2    WITH ladata[28] ,;
        nECost3    WITH ladata[29] ,;
        nECost4    WITH ladata[30] ,;
        POTotal    WITH nEcost1+nEcost2+nEcost3+nEcost4,;
        npo_open   WITH m.NFabOrder

=gfAdd_Info('POFHDR')
IF !EMPTY(&lcTmpSlMal..mnotes)
  REPLACE lhasnotes WITH .T.
  SELECT NOTEPAD  
  APPEND BLANK
  GATHER FIELDS mnotes MEMVAR MEMO
  REPLACE Type   WITH 'M',;
          KEY    WITH 'P'+m.Po,;
          cdesc  WITH "Notes For Item P/o Number : " + m.Po
ENDIF
*:**************************************************************************
*: Name        : lfSMEMAIL
*: Developer   : Mohamed Shokry
*: Date        : 15/11/2004
*: Purpose     : Add option pad to the "msusers" screen to Open the screen SMEMAIL
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfSMEMAIL
PRIVATE lnBarNo

IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P,' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF

lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Update User \<E-mail' SKIP FOR laScrMode[1]
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfOpnEmail IN ALNMAIN.PRG

*-- end of lfSMEMAIL.

*:**************************************************************************
*:* Name        : lfFoundPad
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*!* Purpose     : check if any pad menu is exit in _sysmenu
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfFoundPad
PARAMETER lcPadName

PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
    llfound = .T.
	EXIT
  ENDIF
ENDFOR
RETURN(llFOund)
*-- end of lfFoundPad.

*:**************************************************************************
*:* Name        : lfOpnEmail
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Open screen SMEMAIL
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfOpnEmail
PRIVATE lnSlct,lcTempFl,lcOldValue

lnSlct = SELECT()
lcTempFl = '_'+SUBSTR(lcPrv_Tmp1,2)
IF !USED(lcTempFl) .OR. &lcTempFl..CUSER_ID <> laData[1]
  CREATE CURSOR (lcTempFl) (CUSER_ID C(10),CUSREMAIL C(50))
  APPEND BLANK
  REPLACE CUSER_ID  WITH laData[1] ;
          CUSREMAIL WITH IIF(laScrMode[4],'',SYUUSER.CUSREMAIL)
ENDIF
SELECT &lcTempFl
GO TOP
STORE &lcTempFl..CUSREMAIL TO M.CUSREMAIL,lcOldValue
lcTitle = 'Update User E-mail Address.'

DO (gcScrDir + gcWinAppl + '\SMUSRMAL.SPX')

REPLACE CUSREMAIL WITH M.CUSREMAIL

SELECT (lnSlct)
*-- end of lfOpnEmail.

*:**************************************************************************
*:* Name        : lfRELPAD
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Release Added option pad
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfRELPAD

RELEASE PAD _Option OF _MSYSMENU
*-- end of lfRELPAD.

*:**************************************************************************
*:* Name        : lfvEmail
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Valid function for Email field
*:***************************************************************************
*:* Called from : SMEMAIL.SCX
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvEmail

IF !EMPTY(m.CUSREMAIL)
  IF !'.'$m.CUSREMAIL .OR. !'@'$m.CUSREMAIL
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This is not a Valid e-mail Address.')
    _CUROBJ = OBJNUM(m.CUSREMAIL)
    RETURN 
  ENDIF
ENDIF

IF m.CUSREMAIL <> lcOldValue
  SHOW GET pbOk ENABLE
ELSE
  SHOW GET pbOk DISABLE
ENDIF
*-- end of lfvEmail.

*:**************************************************************************
*:* Name        : lfUPDEMAIL
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Update the array element related to the field "CUSREMAIL" in the file syuuser for niknak
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfUPDEMAIL
PRIVATE lnSlct,lnPos,lcTempFl

lnSlct = SELECT()
lcTempFl = '_'+SUBSTR(lcPrv_Tmp1,2)
IF !USED(lcTempFl)
  SELECT (lnSlct)
  RETURN
ENDIF

*- Update the field "lcScFields"
IF !'CUSREMAIL' $ lcScFields
  lcScFields = lcScFields + ',CUSREMAIL'
  lnPos = OCCURS([,] , lcScFields)+1
  DIMENSION laData[lnPos]
  laData[lnPos] = &lcTempFl..CUSREMAIL
ENDIF

*- Relase the temp file used to save a the  user-email 
IF USED(lcTempFl)
  USE IN &lcTempFl
ENDIF
SELECT (lnSlct)
*-- end of lfUPDEMAIL.


*!*************************************************************
*! Name      : lfvShipTo
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Valid Function in user defined field screen
*!*************************************************************
*! Example   : = lfvShipTo()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvShpToV
PARA lcPara
PRIVATE lnCurAlas 
lnCurAlas=SELECT(0)
IF TYPE('LAUSRFIELD') <> "U"
  PRIVATE lnRep3Pos , lcVar , lcObj
  lnRep3Pos = ASCAN(laUsrField,"CSHIPTO")
  IF lnRep3Pos > 0
    lnRep3Pos = ASUBSCRIPT(laUsrField,lnRep3Pos,1)
  ENDIF
  lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control

  lcObj = EVALUATE(SYS(18))      && Variable to hold the current field value
  IF lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnRep3Pos)) + ",6)"
    *lcStateVal = laUsrFields[2,6]      && Varible to hold  the value of the current GET field
    = gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
    SELECT APVENDOR
    lnCurrRec = RECNO()

    LOCATE FOR ALLTRIM(cShipTo) = ALLTRIM(lcObj) 

    IF EMPTY(lcObj) OR !FOUND() 
      lcObj = gfBroWWare(.T.)
    ENDIF

    &lcVar = lcObj
    laUsrField[lnRep3Pos,6] = lcObj
    IF TYPE('laOGFxFlt[lnRep3Pos,6]') <> 'U'
      laOGFxFlt[lnRep3Pos,6] = lcObj
    ENDIF

    SELECT APVENDOR
    IF BETWEEN(lnCurrRec ,1,RECCOUNT())
      GOTO lnCurrRec 
    ENDIF
    
  ENDIF 
ENDIF  
SELECT(lnCurAlas)
lcPara = .T.
RETURN lcPara


*!*************************************************************
*! Name      : lfvDel
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Valid Function in user defined field screen
*!*************************************************************
*! Example   : = lfvShipTo()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvDel

SELECT (lcTmpSlAt2)

lnAOrdrd   =&lcTmpQuery..Ordered-&lcTmpSlAt2..OrdQty 
lnATOrdr   =lnARqured - lnAOrdrd 
SELECT(lcTmpQuery)
REPLACE &lcTmpQuery..Ordered WITH lnAOrdrd,;
        &lcTmpQuery..ToOrder WITH lnATOrdr   
  
IF !EMPTY(PO)
  SELECT MAPOALO  
  SET ORDER TO Mapoalo
  IF SEEK(&lcTmpSlAt2..PO+PADL(&lcTmpSlAt2..Item,7)+&lcTmpSlAt2..iclr)
    IF SEEK('P'+POMAT,'POFHDR') AND (POFHDR.Status $'O')
      lcScanExp = "P"+POMat+PADL(&lcTmpSlAt2..Item,7)+&lcTmpSlAt2..iclr
      SELECT POFLN
      =SEEK(lcScanExp)
      llChkRecv = .F. 
      SCAN REST WHILE cmattype+pomat+fabric+color+trancd = lcScanExp
        IF Trancd ="2"
          llChkRecv = .T. 
          EXIT
        ENDIF
      ENDSCAN
      IF llChkRecv 
        RETURN
      EnDIF  
      SELECT POFLN
      =SEEK(lcScanExp)
      SELECT APVENDOR  
      =SEEK(&lcTmpSlAt2..FSupplier)
      =RLOCK()
      REPLACE nVenOpnPO WITH nVenOpnPO - POFLN.nEcost1 
      UNLOCK       
       
      IF POFHDR.lastline =1 OR POFHDR.NFabOrder = POFLN.nfabtotqty
        SELECT POFHDR
        REPLACE POFHDR.Status      WITH 'X',;
                POFHDR.nfabcancel  WITH POFHDR.NFabOrder,; 
                POFHDR.npo_open    WITH 0
      ELSE
        SELECT POFHDR
        REPLACE  POFHDR.NFabOrder WITH  POFHDR.NFabOrder - POFLN.nfabtotqty

        REPLACE  POFHDR.lastline WITH POFHDR.lastline - 1,;
                 POFHDR.nfabcancel  WITH POFLN.nfabtotqty,; 
                 POFHDR.npo_open    WITH POFHDR.npo_open - POFLN.nfabtotqty
                 
        SELECT POFLN
        DELETE
      ENDIF
    ELSE
      RETURN  
    ENDIF  
    SELECT MAPOALO  
    DELETE
    IF SEEK(&lcTmpSlAt2..Item+&lcTmpSlAt2..Iclr,'FABRIC')
      SELECT FABRIC
      =RLOCK()
      REPLACE FABRIC.ONORDER WITH FABRIC.ONORDER - &lcTmpSlAt2..OrdQty*CONV
      UNLOCK
      SELECT FABDYE
      SET ORDER TO TAG FABDYE
      IF SEEK(&lcTmpSlAt2..Item+&lcTmpSlAt2..Iclr+&lcTmpSlAt2..cWareCode,'FABDYE')
        REPLACE FABDYE.ONORDER WITH FABDYE.ONORDER - &lcTmpSlAt2..OrdQty*FABRIC.CONV
      ENDIF
      UNLOCK
    ENDIF
  ENDIF
ENDIF
SELECT (lcTmpSlAt2)
DELETE 
LOCATE
SHOW WINDOW (lcSlAtWin) REFRESH 
=lfRefresh()
SHOW GETS

*!*************************************************************
*! Name      : lfwDel
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Valid Function in user defined field screen
*!*************************************************************
*! Example   : = lfvShipTo()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfwDel

lcOldVal = UPPER(&lcTmpSlAt..Delete)
REPLACE &lcTmpSlAt..Delete WITH UPPER(&lcTmpSlAt..Delete)

*:**************************************************************************
*:* Name        : lfAPVENMAL
*:* Developer   : Mohamed Shokry
*:* Date        : 10/31/2004
*:* Purpose     : Add option pad to the "msusers" screen to Open the screen SMEMAIL
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfAPVENMAL
PRIVATE lnBarNo

lnBarNo = CNTBAR('_INQURYPOP') + 1
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT 'Update User \<E-mail' SKIP FOR laScrMode[1]
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfVenEmail IN ALNMAIN.PRG


*:**************************************************************************
*:* Name        : lfOpnEmail
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*:* Purpose     : Open screen SMEMAIL
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfVenEmail
PRIVATE lnSlct,lcTempFl,lcOldValue

lnSlct = SELECT()
lcTempFl = '_'+SUBSTR(lcContFile,2)
IF !USED(lcTempFl) .OR. &lcTempFl..cvendcode <> laData[1]
  CREATE CURSOR (lcTempFl) (cvendcode C(8),cvenemail C(30))
  APPEND BLANK
  REPLACE cVendCode WITH laData[1] ;
          cVenEmail WITH IIF(laScrMode[4],'',APVENDOR.cVenEmail)
ENDIF
SELECT &lcTempFl
GO TOP
STORE &lcTempFl..cVenEmail TO M.CUSREMAIL,lcOldValue
lcTitle = 'Update Vendor E-mail Address.'

DO (gcScrDir + 'SM\SMUSRMAL.SPX')

REPLACE cVenEmail WITH M.CUSREMAIL

SELECT (lnSlct)
*-- end of lfOpnEmail.

*:**************************************************************************
*:* Name        : lfSAVEMAIL
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Update the array element related to the field "CUSREMAIL" in the file syuuser for niknak
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfSAVEMAIL
PRIVATE lnSlct,lnPos,lcTempFl

lnSlct = SELECT()
lcTempFl = '_'+SUBSTR(lcContFile,2)
IF !USED(lcTempFl)
  SELECT (lnSlct)
  RETURN
ENDIF

REPLACE APVENDOR.cVenEmail WITH &lcTempFl..cVenEmail
laUsrFields[1,3] = "G" 

*- Relase the temp file used to save a the  user-email 
IF USED(lcTempFl)
  USE IN &lcTempFl
ENDIF
SELECT (lnSlct)
*-- end of lfUPDEMAIL.

*:**************************************************************************
*:* Name        : lfvColtQty
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : To Collect Order Qty
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvColtQty
PARAMETER lcCurItem , lcCurIclr 
PRIVATE lnOldAls ,lnReqQty
STORE 0 TO lnReqQty
= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH')
= gfOpenFile(gcDataDir+'POFLN',gcDataDir+'POFLN','SH')
= gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'Vencode','SH')
lnOldAls = SELECT(0)
lcTable = 'ORDBOM'

SELECT ORDBOM
=SEEK(laData[1])
SCAN REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode = laData[1];
             FOR   cCatgTyp $ "F|T"
    SCATTER MEMVAR MEMO
    IF lcCurItem <>m.Item 
      LOOP
    ENDIF
    IF '***' $ m.IClr 
      =SEEK(PADL(m.Item,7)+lcCurIclr,'Fabric')    
      SELECT FABRIC
      SCAN REST WHILE FABRIC+Color = PADL(m.Item,7) + lcCurIclr
          lnReqQty = lnReqQty  + lfSzTot(&lcTmpSlAt..Rating)
      ENDSCAN
    ELSE
      IF lcCurIclr =m.IClr
          lnReqQty = lnReqQty  + lfSzTot(&lcTmpSlAt..Rating)
      ENDIF    
    ENDIF  
  ENDSCAN
SELECT(lnOldAls)

RETURN lnReqQty
*:**************************************************************************
*:* Name        : lfvColtQty
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : To Collect Order Qty
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvOpSel
llOpenSCR = .T.
*:**************************************************************************
*:* Name        : lfvBKToSum
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Back to summary Screen
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvBKToSum
PRIVATE lnAlias 

lnAlias = SELECT (0)

SELECT (lcTmpQuery)
PUSH KEY

DO (gcScrDir+"SO\soauto.SPX")
POP KEY
IF llOpenSCR 
  llOpenSCR = .F.
  =lfvSelect()
ENDIF

ON KEY LABEL ALT+B
lnAlias = SELECT (0)

*:**************************************************************************
*:* Name        : lfvOpSumm
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Back to summary Screen
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvOpSumm
SELECT (lcTmpSlAt2)
SCAN FOR EMPTY(PO)
  IF EMPTY(cshipto) 
    lcMsg2 = "Ship TO can not be empty"
    lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    RETURN    
  ENDIF
  
  IF EMPTY(PSupplier) 
    lcMsg2 = "Vendor code can not be empty"
    lnMsgNo=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    SELECT(lnAlias)
    RETURN    
  ENDIF
ENDSCAN
CLEAR READ


*!*************************************************************
*! Name      : lfChckAlct
*: Developer : Mohamed Shokry
*: Date      : 15/11/2004
*! Purpose   : Check if there is any allocated or not
*!*************************************************************
*! Example   : = lfChckAlct()
*!*************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfChckAlct
PRIVATE lnOldAls,lcOldOrd , lnPofQty,lnHusQty

lnOldAls = SELECT()
= gfOpenFile(gcDataDir+'MAPOALO','MAPOSO','SH')
= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
SELECT MAPOALO
lcMapOrd =ORDER()
SET ORDER TO Mapoalo

SELECT POFLN
lcOldOrd =ORDER()
SET ORDER TO POFLNF
IF SEEK(PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr)
  SCAN REST while fabric+color+cmattype+pomat+trancd=   PADL(&lcTmpQuery..Item,7)+  &lcTmpQuery..iclr
    STORE 0 TO lnHusQty,lnPofQty
    *-- as per customer req.
    *IF SEEK('P'+POMAT,'POFHDR') AND !(POFHDR.Status $'SX')
    IF SEEK('P'+POMAT,'POFHDR') AND !(POFHDR.Status $'X')
    *-- as per customer req.
    
      lcWarehose = POFHDR.cWareCode
      
      IF TRANCD = '1'
        *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
        *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
        *lnPofQty= lnPofQty +NFABTOTQTY 
        lnPofQty= lnPofQty +(NFABTOTQTY*FABRIC.CONV) 
        *B128216,1 MMR.[End]
      ENDIF
      IF TRANCD = '5'
       *B128216,1 MMR 07/27/2005 FIXING BUG OF NOT SHOWING UNALOCATED QNTY For PO that not assigned
       *B128216,1 MMR            to any SO also Fix bug of showing quantites in UOM BUY not USE
       *lnPofQty = lnPofQty - NFABTOTQTY 
       lnPofQty = lnPofQty - (NFABTOTQTY*FABRIC.CONV)
       *B128216,1 MMR.[End] 
      ENDIF
      *B128216,2 MMR 11/30/2005 3)Fix bug of not showing  PO that contains over recieve qty
      *B128216,2 MMR              If we have only this po for this material
      IF TRANCD = '2'
        lnPofQty= lnPofQty +(NFABTOTQTY*FABRIC.CONV) 
      ENDIF
      *B128216,2 MMR.[End]
      lcScanExp = POMat+PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..iclr
      SELECT MAPOALO
      =SEEK(lcScanExp)
      lcOrdMat = ''
      SCAN REST WHILE pomat+fabric+color+order = lcScanExp
        lnHusQty = lnHusQty + MAPOALO.nhousqty
      ENDSCAN
      IF lnPofQty > lnHusQty
        lcAUnAllct = 'Y'  
        EXIT
      ELSE
        lcAUnAllct = 'N'  
      ENDIF
      
    ENDIF        
  ENDSCAN
ENDIF

SELECT MAPOALO
SET ORDER TO &lcMapOrd

SELECT POFLN
SET ORDER TO &lcOldOrd 


SELECT(lnOldAls)
*:**************************************************************************
*:* Name        : lfvOpSumm
*: Developer    : Mohamed Shokry
*: Date         : 15/11/2004
*:* Purpose     : Back to summary Screen
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfvTrmit

llCurLoop = .T.
CLEAR READ


*:**************************************************************************
*:* Name        : lfgetPos
*:* Developer   : Mohamed Shokry MHM
*:* Date        : 11/15/2004
*:* Purpose     : Get all Po's related to the Sales order 
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*: Due to  C#124836,1.
*!*************************************************************
FUNCTION lfGetPos
PRIVATE lnCurAlis,lnCurRec,lcCurOrd

lnCurAlis = SELECT (0)

SELECT MAPOALO
lcCurOrd = ORDER()
SET ORDER TO Maposo
LOCATE

SELECT (lcTmpQuery)
lnCurRec = RECNO()
SCAN
  SELECT MAPOALO
  IF SEEK(OrdHdr.Order+ PADR(&lcTmpQuery..Item,7) +&lcTmpQuery..iclr)
    SCAN REST WHILE order+fabric+color =  OrdHdr.Order+ PADR(&lcTmpQuery..Item,7) +&lcTmpQuery..iclr
      IF !SEEK('P'+PoMat,'POFHDR') OR (POFHDR.Status $'SX')
        LOOP
      ENDIF
      lcPoMat = MAPOALO.POMAT 
      IF !SEEK(lcPoMat,lcTmpPoNot)
        m.PSupplier = POFHDR.VENDOR
        SELECT (lcTmpPoNot)
        APPEND BLANK
        REPLACE PO WITH lcPoMat,;
        PSupplier WITH m.PSupplier
      ENDIF
    ENDSCAN  
  ENDIF    
ENDSCAN

SELECT (lcTmpQuery)
GOTO lnCurRec 

SELECT MAPOALO
SET ORDER TO &lcCurOrd 

SELECT (lnCurAlis)
