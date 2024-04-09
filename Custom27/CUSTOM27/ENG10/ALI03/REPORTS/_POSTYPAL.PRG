*:****************************************************************
*: Program file  : POSTYAL.PRG [Custom PO form for Alison Hayes]
*: Program desc. : Print Custom Style Purchase Order Form.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Purchase Order  (PO)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 12/31/2003
*: Tracking Job Number : C#037305,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  : lfgetStub , lfgetHeadr , lfCreatTmp
*:               :            : lfCollect , lfEvalSegs , lfInsertRc
*:               :            : lfColCtkt , lfNotPdDec , lfTemNote
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#037305,1
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:

IF USED(lcMainTemp )
  USE IN (lcMainTemp )
  ERASE (gcWorkDir+lcMainTemp +".DBF")
  ERASE (gcWorkDir+lcMainTemp +".CDX")
ENDIF

lcMainTemp = gfTempName()
lcWorkFile = gfTempName()
*-- Remove the relation to get the pcture after the report finish printing,
*-- that beacuse the file after printing was at the end of the file.


SELECT POSLN
SET RELATION OFF INTO Objlink
SELECT POSHDR
SET SKIP TO 

*-- Get the note
= lfTemNote()

lnExtSzWd = gfGetMemVar('M_EXTWIDTH')
*-- Evaluate NonMajor Type and variables.
= lfEvalSegs ()

*-- Function To create Temp File to print from
=lfCreatTmp()

*-- Function To Collect Data
=lfCollect()

SELECT (lcMainTemp)
SET RELATION TO 'P'+PO INTO POSHDR ADDITIVE


RETURN
*-- End OF Code.
*:*************************************************************
*: Name      : lfgetStub
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 12/31/2003
*: Purpose   : Function to Get the purchase order Stub information
*:*************************************************************
*: Called from : POSTYPAL.FRX
*:*************************************************************
*: Calls       :  None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : True.
*:*************************************************************
*
FUNCTION lfgetStub
PARAMETER lcReturn
PRIVATE   lnPrvAlias , lnLoop , llOpenNote , lnMemLins
DIMENSION laCompAdd[7] , laNotes [18]
laNotes    = ''
llOpenNote = .F.
lnMemLins  = 0

lnPrvAlias = SELECT (0)
IF !USED('NOTEPAD')
 llOpenNote  =gfOpenFile(gcDataDir+"NOTEPAD","NOTEPAD",'SH')
ENDIF
*-- 
IF SEEK('P'+Poshdr.Po,'NOTEPAD')
  lnMemLins = MEMLINES(NOTEPAD.mNotes)
  *-- Get the first 6 Lines from the notes.
  IF lnMemLins >= 6  
    FOR lnLoop = 1 TO MIN(MEMLINES(NOTEPAD.mNotes),6)
      laNotes[lnLoop] = MLINE(NOTEPAD.MNOTES,lnLoop)
    ENDFOR
  ENDIF
  lnLoop = 0
  *-- get the Secand 6 Lines from the notes.
  IF lnMemLins >= 7
    FOR lnLoop = 7 TO MIN(MEMLINES(NOTEPAD.mNotes),12)
      laNotes[lnLoop] = MLINE(NOTEPAD.MNOTES,lnLoop)
    ENDFOR
  ENDIF

  lnLoop = 0
  *-- get the thired 6 Lines from the notes.
    IF lnMemLins >= 13
    FOR lnLoop = 13 TO MIN(MEMLINES(NOTEPAD.mNotes),18)
      laNotes[lnLoop] = MLINE(NOTEPAD.MNOTES,lnLoop)    
    ENDFOR
  ENDIF

ENDIF


IF llOpenNote
  USE IN NOTEPAD
ENDIF


*-- Get the company adress.
=SEEK (gcAct_Comp,'SYCCOMP')
lcCompName = SYCCOMP.cCom_Name
lcCompPhon = SYCCOMP.cCom_Phon     && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompFax  = SYCCOMP.cCom_Fax      && Variable to hold the Company Fax

*-- Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone '+TRANSFORM(lcCompPhon , lcPhonPict)
laCompAdd[7]    = 'Fax   '+TRANSFORM(lcCompFax  , lcPhonPict)

DO lfShiftArr WITH laCompAdd
* Pack the addresses array

=SEEK ('S'+SUBSTR(lCurnstyle,1,lnMajSize),'Objlink')

SELECT (lnPrvAlias)
lcReturn = .T.

RETURN
*-- End OF lfgetStub
*:*************************************************************
*: Name      : lfgetHeadr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 12/31/2003
*: Purpose   : Function to Get the purchase order Header information
*:*************************************************************
*: Called from : POSTYPAL.FRX
*:*************************************************************
*: Calls       :  None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : True.
*:*************************************************************
*
FUNCTION lfgetHeadr
PARAMETER lcReturn
PRIVATE   lnPrvAlias

DIMENSION laVenrAd[7]
STORE '' TO laVenrAd

lnPrvAlias = SELECT (0)

IF POSHDR.cStyType # 'N'
  *-- Get the vendor addresses
  laVenrAd[1] = APVENDOR.CVenComp
  laVenrAd[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVenrAd[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVenrAd[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVenrAd[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVenrAd[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ENDIF

DO lfShiftArr WITH laVenrAd

*-- Get The Customer btname.
IF !EMPTY(&lcMainTemp..Order) .AND. SEEK('O'+&lcMainTemp..Order,'ORDLINE') ;
   .AND. SEEK(IIF(EMPTY(Ordline.Store),'M','S')+Ordline.Account+Ordline.Store,'Customer')
  lCustBtNam = Customer.Btname
ENDIF



*-- Get the Fabric Contant.
IF !EMPTY(STYLE.Fabric) .AND. SEEK(Style.Fabric,'Fabric')
  lcFabCont   = ALLTRIM(Fabric.Content)
ELSE
  lcFabCont = ''
ENDIF

SELECT (lnPrvAlias)
lcReturn = .T.

RETURN

*-- End OF lfgetHeadr
*:*************************************************************
*: Name      : lfCreatTmp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 12/31/2003
*: Purpose   : Create Temp File
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfCreatTmp()
*:*************************************************************
*:
FUNCTION lfCreatTmp
PRIVATE lnOld_Als

lnOld_Als = SELECT(0)
*-- Open CutPick File.
IF USED('CUTPICK')
  SELECT CUTPICK
  SET ORDER TO CUTPICK
ELSE
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
ENDIF
SELECT CutPick
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+33,4]

laFileStru[lnFileStru+01,1] = 'Qty9'
laFileStru[lnFileStru+01,2] = 'N'
laFileStru[lnFileStru+01,3] = 8
laFileStru[lnFileStru+01,4] = 0

laFileStru[lnFileStru+02,1] = 'Qty10'
laFileStru[lnFileStru+02,2] = 'N'
laFileStru[lnFileStru+02,3] = 8
laFileStru[lnFileStru+02,4] = 0

laFileStru[lnFileStru+03,1] = 'Qty11'
laFileStru[lnFileStru+03,2] = 'N'
laFileStru[lnFileStru+03,3] = 8
laFileStru[lnFileStru+03,4] = 0

laFileStru[lnFileStru+04,1] = 'Qty12'
laFileStru[lnFileStru+04,2] = 'N'
laFileStru[lnFileStru+04,3] = 8
laFileStru[lnFileStru+04,4] = 0


laFileStru[lnFileStru+05,1] = 'TotCutQty'
laFileStru[lnFileStru+05,2] = 'N'
laFileStru[lnFileStru+05,3] = 12
laFileStru[lnFileStru+05,4] = 0

laFileStru[lnFileStru+06,1] = 'Complete'
laFileStru[lnFileStru+06,2] = 'D'
laFileStru[lnFileStru+06,3] = 8
laFileStru[lnFileStru+06,4] = 0

laFileStru[lnFileStru+07,1] = 'cGroup'
laFileStru[lnFileStru+07,2] = 'C'
laFileStru[lnFileStru+07,3] = 6
laFileStru[lnFileStru+07,4] = 0

laFileStru[lnFileStru+08,1] = 'cPack'
laFileStru[lnFileStru+08,2] = 'C'
laFileStru[lnFileStru+08,3] = 10
laFileStru[lnFileStru+08,4] = 0

laFileStru[lnFileStru+09,1] = 'CustPO'
laFileStru[lnFileStru+09,2] = 'C'
laFileStru[lnFileStru+09,3] = 15
laFileStru[lnFileStru+09,4] = 0

*-- Save the scale.
laFileStru[lnFileStru+10,1] = 'Sz1'
laFileStru[lnFileStru+10,2] = 'C'
laFileStru[lnFileStru+10,3] = 5
laFileStru[lnFileStru+10,4] = 0

laFileStru[lnFileStru+11,1] = 'Sz2'
laFileStru[lnFileStru+11,2] = 'C'
laFileStru[lnFileStru+11,3] = 5
laFileStru[lnFileStru+11,4] = 0


laFileStru[lnFileStru+12,1] = 'Sz3'
laFileStru[lnFileStru+12,2] = 'C'
laFileStru[lnFileStru+12,3] = 5
laFileStru[lnFileStru+12,4] = 0

laFileStru[lnFileStru+13,1] = 'Sz4'
laFileStru[lnFileStru+13,2] = 'C'
laFileStru[lnFileStru+13,3] = 5
laFileStru[lnFileStru+13,4] = 0

laFileStru[lnFileStru+14,1] = 'Sz5'
laFileStru[lnFileStru+14,2] = 'C'
laFileStru[lnFileStru+14,3] = 5
laFileStru[lnFileStru+14,4] = 0

laFileStru[lnFileStru+15,1] = 'Sz6'
laFileStru[lnFileStru+15,2] = 'C'
laFileStru[lnFileStru+15,3] = 5
laFileStru[lnFileStru+15,4] = 0

laFileStru[lnFileStru+16,1] = 'Sz7'
laFileStru[lnFileStru+16,2] = 'C'
laFileStru[lnFileStru+16,3] = 5
laFileStru[lnFileStru+16,4] = 0

laFileStru[lnFileStru+17,1] = 'Sz8'
laFileStru[lnFileStru+17,2] = 'C'
laFileStru[lnFileStru+17,3] = 5
laFileStru[lnFileStru+17,4] = 0

laFileStru[lnFileStru+18,1] = 'Sz9'
laFileStru[lnFileStru+18,2] = 'C'
laFileStru[lnFileStru+18,3] = 5
laFileStru[lnFileStru+18,4] = 0

laFileStru[lnFileStru+19,1] = 'Sz10'
laFileStru[lnFileStru+19,2] = 'C'
laFileStru[lnFileStru+19,3] = 5
laFileStru[lnFileStru+19,4] = 0

laFileStru[lnFileStru+20,1] = 'Sz11'
laFileStru[lnFileStru+20,2] = 'C'
laFileStru[lnFileStru+20,3] = 5
laFileStru[lnFileStru+20,4] = 0

laFileStru[lnFileStru+21,1] = 'Sz12'
laFileStru[lnFileStru+21,2] = 'C'
laFileStru[lnFileStru+21,3] = 5
laFileStru[lnFileStru+21,4] = 0

laFileStru[lnFileStru+22,1] = 'PO'
laFileStru[lnFileStru+22,2] = 'C'
laFileStru[lnFileStru+22,3] = 6
laFileStru[lnFileStru+22,4] = 0

laFileStru[lnFileStru+23,1] = 'SCALE'
laFileStru[lnFileStru+23,2] = 'C'
laFileStru[lnFileStru+23,3] = 3
laFileStru[lnFileStru+23,4] = 0

laFileStru[lnFileStru+24,1] = 'nScaleNo'
laFileStru[lnFileStru+24,2] = 'N'
laFileStru[lnFileStru+24,3] = 2
laFileStru[lnFileStru+24,4] = 0

laFileStru[lnFileStru+25,1] = 'Fabric'
laFileStru[lnFileStru+25,2] = 'C'
laFileStru[lnFileStru+25,3] = 7
laFileStru[lnFileStru+25,4] = 0

laFileStru[lnFileStru+26,1] = 'Width'
laFileStru[lnFileStru+26,2] = 'C'
laFileStru[lnFileStru+26,3] = 6
laFileStru[lnFileStru+26,4] = 0

laFileStru[lnFileStru+27,1] = 'Desc'
laFileStru[lnFileStru+27,2] = 'C'
laFileStru[lnFileStru+27,3] = 20
laFileStru[lnFileStru+27,4] = 0

laFileStru[lnFileStru+28,1] = 'Colordes'
laFileStru[lnFileStru+28,2] = 'C'
laFileStru[lnFileStru+28,3] = 30
laFileStru[lnFileStru+28,4] = 0

laFileStru[lnFileStru+29,1] = 'cItemTyp'
laFileStru[lnFileStru+29,2] = 'C'
laFileStru[lnFileStru+29,3] = 30
laFileStru[lnFileStru+29,4] = 0

laFileStru[lnFileStru+30,1] = 'nUntQty'
laFileStru[lnFileStru+30,2] = 'N'
laFileStru[lnFileStru+30,3] = 8
laFileStru[lnFileStru+30,4] = 3

laFileStru[lnFileStru+31,1] = 'cvencomp'
laFileStru[lnFileStru+31,2] = 'C'
laFileStru[lnFileStru+31,3] = 30
laFileStru[lnFileStru+31,4] = 0

laFileStru[lnFileStru+32,1] = 'Req_Qty'
laFileStru[lnFileStru+32,2] = 'N'
laFileStru[lnFileStru+32,3] = 12
laFileStru[lnFileStru+32,4] = 3

laFileStru[lnFileStru+33,1] = 'Used_Qty'
laFileStru[lnFileStru+33,2] = 'N'
laFileStru[lnFileStru+33,3] = 12
laFileStru[lnFileStru+33,4] = 3


*-- Create an index.
DECLARE laIndex[1,2]
*- Modi
*laIndex[1,1] = 'cTktno+cGroup+style+DTOS(Complete)'
laIndex[1,1] = 'cTktno+cGroup+DTOS(Complete)+style'
*- Modi
laIndex[1,2] = lcMainTemp


=gfCrtTmp(lcMainTemp,@laFileStru,@laIndex)

*-- Create a copy from main file and create a new index. 
DECLARE laIndex[1,2]
laIndex[1,1] = 'PO+DTOS(COMPLETE)+STYLE'
laIndex[1,2] = lcWorkFile
=gfCrtTmp(lcWorkFile,@laFileStru,@laIndex)

*-- Open Needed files & set order after select the file in case 
*-- the file was opend before. 
IF USED('ORDLINE')
  SELECT ORDLINE
  SET ORDER TO ORDLINE
ELSE
  =gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
ENDIF

IF USED('CUSTOMER')
  SELECT CUSTOMER
  SET ORDER TO Customer
ELSE
  =gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
ENDIF

IF USED('FABRIC')
  SELECT FABRIC
  SET ORDER TO FABRIC
ELSE
  =gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH')
ENDIF

IF USED('CTKTBOM')
  SELECT CTKTBOM
  SET ORDER TO CTKTBOM
ELSE
  =gfOpenFile(gcDataDir+'CTKTBOM',gcDataDir+'CTKTBOM','SH')
ENDIF

SELECT(lnOld_Als)

*-- End OF lfCreatTmp
*:*************************************************************
*: Name      : lfCollect
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 12/31/2003
*: Purpose   : Collect Data to Print.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfCollect()
*:*************************************************************
*:
FUNCTION lfCollect
PRIVATE lnOld_Als , llFirstRec , lcOldPo
lcOldPo = ''
llFirstRec = .T.
*-- Get the Scale Width.
lnStyleWid = 19-(3-lnExtSzWd)

lnOld_Als = SELECT(0)
*-- Get the recored for filter Expration.
SELECT POSHDR
SCAN FOR &lcRpExp
  Wait Window 'Please wait ... checking data for Po#: ' + PO NOWAIT
  *-- Add the Style IF Not Found.
  SELECT POSLN
  *-- Get the Pick Qty
  *-- Index trancd+ctktno+style
  IF SEEK('2'+Poshdr.Po,'CutPick')
    SELECT CutPick
    SCAN REST WHILE  trancd+ctktno+style = '2' + Poshdr.Po
      SCATTER MEMVAR MEMO
      M.Po = Poshdr.Po
      *-- Get the Order complete date.
      *-- Index cordtype+order+STR(lineno,6)
      IF SEEK('O'+M.Order+M.cOrdLine,'Ordline')
        M.Complete = OrdLine.Complete
        M.cPack    = LEFT(ALLTRIM(Ordline.Note_mem),10)
        M.CustPO   = OrdLine.Custpo
        M.Scale    = OrdLine.Scale
      ENDIF

      *-- Update 
      IF !EMPTY(M.Scale) .AND. SEEK('S'+M.Scale,'SCALE')
          M.nScaleNo = Scale.CNT
      ENDIF
      
      *-- Add record into work file.
      SELECT (lcWorkFile)
      IF SEEK(M.Po+DTOS(M.Complete)+M.Style)
        REPLACE Qty1   WITH Qty1   + M.Qty1 ;
                Qty2   WITH Qty2   + M.Qty2 ;
                Qty3   WITH Qty3   + M.Qty3 ;
                Qty4   WITH Qty4   + M.Qty4 ;
                Qty5   WITH Qty5   + M.Qty5 ;
                Qty6   WITH Qty6   + M.Qty6 ;
                Qty7   WITH Qty7   + M.Qty7 ;
                Qty8   WITH Qty8   + M.Qty8 ;
                TotQty WITH TotQty + M.TotQty
      ELSE
        APPEN BLANK
        GATHER MEMVAR MEMO
      ENDIF
    ENDSCAN
  ENDIF  

ENDSCAN


SELECT (lcWorkFile)
SET RELATION TO 'P'+PO INTO POSHDR ADDITIVE

SELECT (lcWorkFile)
LOCATE
lcOldPo = PO
SCAN
  Wait Window 'Please wait ... collecting data for Po#: ' + PO NOWAIT
  IF lcOldPo # PO
    lcOldPo = PO
    llFirstRec = .T.
    *--  Add last record for the totals.
    SELECT (lcMainTemp)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cGroup WITH 'AAAAA3'
    *- Function to get the Cuting ticket for current PO.
    = lfColCtkt ()
  ENDIF
  *-- Current Style Without Scale.
  SELECT (lcWorkFile)
  SCATTER MEMVAR MEMO
  lcStyle = SUBSTR(Style,1,lnMajorLen)+lcSepart+SUBSTR(Style,lnNonMajSt,lnColorLen-3)+LEFT(M.Scale,lnExtSzWd) 
  SELECT (lcMainTemp)
  
  IF SEEK(M.ctktno)
    LOCATE REST WHILE ctktno = M.ctktno FOR LEFT(Style,lnStyleWid) = lcStyle .AND. DTOS(Complete) = DTOS(M.Complete)
    IF FOUND() 
      IF nScaleNo < 12
        =SEEK('S'+M.Scale,'SCALE')
        lnStart = 1
        lnlastSze = nScaleNo
      
        *-- update the qty for the new scale.
        FOR lnStart = 1 TO Scale.Cnt
          *-- Variable hold the last sizez after the first style.
          lnlastSze = lnlastSze + 1
          lclastSze = ALLTRIM(STR(lnlastSze,8))
          lcSize    = STR(lnStart,1)
          REPLACE Qty&lclastSze WITH Qty&lclastSze + M.Qty&lcSize,;
                  SZ&lclastSze  WITH Scale.Sz&lcSize
          IF lnlastSze = 12
            EXIT
          ENDIF
        ENDFOR
        
        *--  Update the scale
        IF !(m.Scale = Scale)
          *-- Get the Size for Old Scale.
          REPLACE nScaleNo WITH nScaleNo + Scale.Cnt,;
                  Scale WITH Scale.Scale
        ENDIF
        
        *-- Update the Total Qty.
         M.TotCutQty = 0
         FOR lnLoop = 1 To 12
           lcSize     = ALLTRIM(STR(lnLoop,2))
           M.TotCutQty  = M.TotCutQty + Qty&lcSize
         ENDFOR
         REPLACE TotCutQty WITH M.TotCutQty
       ENDIF  
    ELSE
      *-- Insert new record.
      = lfInsertRc ()
    ENDIF

  ELSE
    *-- Insert new record.
    = lfInsertRc ()
  ENDIF
  IF llFirstRec 
    llFirstRec = .F.
    REPLACE cGroup WITH 'AAAAA1'
  ELSE
    *-- Donit Update the 'AAAAAA' recored in case it's the same style.
    IF EMPTY(cGroup)
      REPLACE cGroup WITH 'AAAAA2'
    ENDIF
  ENDIF
  
ENDSCAN

*-- Print The Total Record.
IF lcOldPo # PO
  SELECT (lcMainTemp)
  *--  Add last record for the totals.
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE cGroup WITH 'AAAAA3'
  *- Function to get the Cuting ticket for current PO.
  = lfColCtkt ()
ENDIF


SELECT (lcWorkFile)
SET RELATION OFF INTO POSHDR

IF USED(lcWorkFile)
  USE IN (lcWorkFile)
  ERASE (gcWorkDir+lcWorkFile+".DBF")
  ERASE (gcWorkDir+lcWorkFile+".CDX")
ENDIF

SELECT(lnOld_Als)
*-- End OF lfCollect.
*:*************************************************************
*: Name        : lfEvalSegs
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/31/2003
*: Purpose     : Evaluate NonMajor Type and variables.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfEvalSegs()
*:*************************************************************
*:
FUNCTION lfEvalSegs

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')  
lcItemTl  = gfItemMask('HI')
lcMjrTtl  = gfItemMask('HM')
lcNMjrTl  = gfItemMask('HN')
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')

lnMajorLen = LEN(lcMjrPct)
lnColorLen =  LEN(lcNMjrPt)
lcSepart   = SUBSTR(lcIMjrPt,lnMajorLen+1,1)
lnNonMajSt = IIF(EMPTY(lcSepart),lnMajorLen+1,lnMajorLen+2)



*-- End OF lfEvalSegs.
*:*************************************************************
*: Name        : lfInsertRc
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/01/2004
*: Purpose     : Add record into tem file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfEvalSegs()
*:*************************************************************
*:
FUNCTION lfInsertRc

*-- Add New record.
M.TotCutQty = M.Qty1+M.Qty2+M.Qty3+M.Qty4+M.Qty5+M.Qty6+M.Qty7+M.Qty8
APPEND BLANK
GATHER MEMVAR MEMO
        
*-- Update the Scale.
*- Modi
*IF !EMPTY(M.Scale) .AND. SEEK('S'+M.Scale,'SCALE')
*  REPLACE Sz1      WITH SCALE.Sz1 ,;
*          Sz2      WITH SCALE.Sz2 ,;
*          Sz3      WITH SCALE.Sz3 ,;
*          Sz4      WITH SCALE.Sz4 ,;
*          Sz5      WITH SCALE.Sz5 ,;
*          Sz6      WITH SCALE.Sz6 ,;
*          Sz7      WITH SCALE.Sz7 ,;
*          Sz8      WITH SCALE.Sz8 ,;
*          nScaleNo WITH Scale.Cnt
*ENDIF
*-- lnExtSzWd
lnStartSiz = 0
IF !EMPTY(M.Scale) .AND. SEEK('S'+LEFT(M.Scale,lnExtSzWd),'SCALE')
  REPLACE nScaleNo WITH Scale.Cnt
  SELECT SCALE
  SCAN  REST WHILE type+scale+prepak = 'S'+LEFT(M.Scale,lnExtSzWd)
    SELECT (lcMaintemp)
    FOR lnStLoop = 1 TO Scale.Cnt
      lnStartSiz = lnStartSiz + 1
      lcRplSize  = ALLTRIM(STR(lnStartSiz,2))
      lcRepSize = STR(lnStLoop,1)
      REPLACE Sz&lcRplSize      WITH SCALE.Sz&lcRepSize
      IF lnStartSiz = 12 
        EXIT
      ENDIF
    ENDFOR
    SELECT SCALE    
  ENDSCAN
  SELECT (lcMaintemp)  
ENDIF

*- Modi
*-- End OF lfInsertRc.
*:*************************************************************
*: Name        : lfColCtkt
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/01/2004
*: Purpose     : Function to get the cuting tickect for current Po.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfColCtkt()
*:*************************************************************
*:
FUNCTION lfColCtkt
PRIVATE lnPrv_Als , llFirstCtK , lcCurnVndr
llFirstCtK = .T.

lnPrv_Als = SELECT(0)

*-- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
SELECT ctktbom
=SEEK('I'+M.ctktno)

*- MODI
*SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = 'I' + M.ctktno FOR cCatgtyp = 'F'
SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = 'I' + M.ctktno FOR Typ $ '23'
*- Modi

  *-- Got ot Cuurent Fabric at fabric file.
  = SEEK(LEFT(ctktbom.Item,7)+ctktbom.iclr,'Fabric')

  *- Get the Vendor name.
  lcCurnVndr = ''
  IF !EMPTY(FABRIC.VENDOR) .AND. SEEK(FABRIC.VENDOR,'APVENDOR')
    lcCurnVndr = ALLTRIM(Apvendor.cvencomp)
  ENDIF
  
  SELECT (lcMainTemp)
  APPEN BLANK
  GATHER MEMVAR MEMO
  REPLACE Fabric   WITH LEFT(ctktbom.Item,7)  ,;
          Width    WITH Fabric.Width          ,;
          DESC     WITH ALLTRIM(Fabric.Desc)  ,;
          Colordes WITH ALLTRIM(gfCodDes(ctktbom.iclr,'COLOR'))        ,;
          cItemTyp WITH ALLTRIM(gfCodDes(FABRIC.Item_Type,'ITEM_TYPE')),;
          nUntQty  WITH Ctktbom.UntQty        ,;
          cvencomp WITH lcCurnVndr            ,;
          Used_Qty WITH Ctktbom.Used_Qty      ,;
          Req_Qty  WITH Ctktbom.Req_Qty
          
  *-- Check for the Primary Fabric.
  IF SEEK(M.Style,'STYLE') .AND. !EMPTY(Style.Fabric) .AND. Style.Fabric = LEFT(ctktbom.ITEM,7)
    REPLACE cGroup WITH 'BBBBB1'
    llFirstCtK = .F.
  ENDIF
  
  IF llFirstCtK
    REPLACE cGroup WITH 'BBBBB2'
    llFirstCtK = .F.
  ELSE
    IF EMPTY(cGroup)
      REPLACE cGroup WITH 'BBBBB3'
    ENDIF
  ENDIF
  SELECT ctktbom
ENDSCAN

SELECT (lcMainTemp)

SELECT(lnPrv_Als)
*-- End OF lfColCtkt
*:*************************************************************
*: Name        : lfNotPdDec
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/01/2004
*: Purpose     : Function to get the declaration notepad.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfNotPdDec()
*:*************************************************************
*:
FUNCTION lfNotPdDec
PARAMETER lcReturn
PRIVATE lnPrv_Alias

lnPrv_Alias  = SELECT(0)

SELECT NOTEPAD
IF !EMPTY(lcRpTmpNot) .AND. !SEEK('T'+lcRpTmpNot)
  DECLARE laData[1]
  laData = ''

  lcPBrowTitl = "NotePad Declaration."            && Browse title.

  lcBrFields = [Key:H="Template Code",cdesc:H="Description"]
             
  llSele = ARIABROW(" For Type = 'T'",lcPBrowTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
                         "","","Key","laData")
  
  *-- Restore browse fields.
  IF llSele
   lcRpTmpNot = laData[1]
   SHOW GET lcSafe
  ELSE
    lcRpTmpNot = laOldVal
  ENDIF
ENDIF

lcReturn = .T.
RETURN 
SELECT (lnPrv_Alias)
*-- End OF lfNotPdDec
*:*************************************************************
*: Name        : lfTemNote
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/01/2004
*: Purpose     : Function to get the declaration notepad.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfTemNote()
*:*************************************************************
*:
FUNCTION lfTemNote
PRIVATE lnOldWidth , lnAlias 

lnAlias = SELECT(0)
DIME laTempNote [12]

laTempNote = ''
lnCountr   = 0
SELECT NOTEPAD
lnOldWidth = SET('MEMOWIDTH')
SET MEMOWIDTH TO 80

IF !EMPTY(lcRpTmpNot) .AND. SEEK('T'+lcRpTmpNot)
  lnMemLins = MEMLINES(NOTEPAD.mNotes)
  FOR lnLoop = 1 To lnMemLins
  
    IF !EMPTY(ALLTRIM(MLINE(NOTEPAD.MNOTES,lnLoop)))
      lnCountr = lnCountr + 1
      laTempNote [lnCountr] = MLINE(NOTEPAD.MNOTES,lnLoop)
      
      *-- Print 8 line only.
      IF lnCountr = 12
        EXIT
      ENDIF
    ENDIF
  ENDFOR
ENDIF

SET MEMOWIDTH TO lnOldWidth
SELECT(lnAlias)
*-- End OF lfTemNote
*:*************************************************************
