*:****************************************************************
*: Program file  : MAPMMOB.PRG
*: Program desc. : Material Manufacturing Dye Order.
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#000389,1
*: Date          : 05/16/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfCreatemp , lFilOpArry , lfCodeSeq
*:               :            : lfAddIssue.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#000389,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
DIMENSION laOpration[1,3] , laVendor[6],laIssueCod[1]
STORE ''  TO lcmfgordno,lcColor,lcMfgCode,laOpration,lccmfgordn,;
             lcOpration,lcIssueOrd,laIssueCod
             
STORE .F. TO llMfgOprhd , llOpenBom , llAPVENDOR , llCTKTBOM , llRolls
llUpEndGrp = .T.
lcTmpFile  = gfTempName()
*-- Keep trake Rolls.
llTrkRolls = ALLTRIM(gfGetMemVar('M_TrkRolls'))= 'Y'
*-- End Declaration variables.

SELECT (lcMainF)
SET RELATION TO

*- Function to create the temp file
= lfCreatemp()

IF !USED('BOM') .AND. !llOpenBom
  llOpenBom  = gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
ENDIF

IF !USED('MFGOPRHD') .AND.  !llMfgOprhd
  llMfgOprhd = gfOpenFile(gcDataDir+'MFGOPRHD',gcDataDir+'MFGOPRHD','SH')
ENDIF

IF !USED('APVENDOR') .AND.  !llAPVENDOR
  llAPVENDOR = gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'Vencode','SH')
ENDIF

IF !USED('CTKTBOM') .AND. !llCTKTBOM
  llCTKTBOM = gfOpenFile(gcDataDir+'CTKTBOM',gcDataDir+'Ctktbom','SH')
ENDIF

IF llTrkRolls
  llRolls = gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'Rollitem','SH')
ENDIF

SELECT (lcMainF)
LOCATE
SCAN
  lcOpration = ''
  *-- Didin't call this function if the mfgorder not change.
  IF lccmfgordn # cmfgordno
    lccmfgordn = cmfgordno
    = lFilOpArry()
  ENDIF
  IF !EMPTY(laOpration[1,1])
    *-- loop for the lan of the array.
    FOR I = 1 To ALEN(laOpration,1)
      SELECT (lcMainF)  
      *-- this line to close the detail box if we have notes.
      SCATTER MEMVAR MEMO
      SELECT (lcTmpFile)
      APPEND BLANK
      GATHER MEMVAR MEMO
     
     
      REPLACE MfgCode   WITH laOpration[I,1]     ,;
              MfgCodeNo WITH VAL(laOpration[I,2])
     *-- Update the cOrder field in every case.              
     IF NoteFlag = 'N'
       REPLACE cOrder    WITH 'A'
     ELSE
       REPLACE cOrder    WITH 'B'
     ENDIF        
     REPLACE cType WITH "1"
            
      IF lcOpration <> laOpration[I,1] .AND. !EMPTY(laOpration[I,3])
        IF SEEK(laOpration[I,3],'APVENDOR')
        
          *-- Fill Vendor array with its data [Begin]
          laVendor[1] = APVENDOR.CVenComp
          laVendor[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
          laVendor[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
          laVendor[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
          laVendor[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
          laVendor[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
          *-- Get the vendor addresses
          DO lfShiftArr WITH laVendor
          REPLACE  cvencomp  WITH laVendor[1] ,;
                   caddress1 WITH laVendor[2] ,;
                   caddress2 WITH laVendor[3] ,;
                   caddress3 WITH laVendor[4] ,;
                   caddress4 WITH laVendor[5] ,;
                   caddress5 WITH laVendor[6]
        ELSE
          REPLACE  cvencomp  WITH laOpration[I,3]
        ENDIF  
      ENDIF
      
      *-- add recored for the last part 
      *-- we should call this function once for one mmfgordno & one opration.
      IF lcIssueOrd # cmfgordno .OR. ASCAN(laIssueCod,laOpration[I,1]) = 0
        *-- Empty the array in case new order.
        IF lcIssueOrd #  cmfgordno
          DECLARE laIssueCod[1]
          laIssueCod = ''
          llUpEndGrp = .T.
        ENDIF
        *-- Assign the order no.
        lcIssueOrd =  cmfgordno
        *-- Declare the array in case new Opration will add
        IF !EMPTY(laIssueCod[1])
          DECLARE laIssueCod[Alen(laIssueCod)+1]
        ENDIF
        laIssueCod[Alen(laIssueCod)] = laOpration[I,1]
        = lfAddIssue()
      ENDIF
    ENDFOR
  ENDIF  
  SELECT (lcMainF)
ENDSCAN

lcMainF = lcTmpFile
SELECT (lcMainF)
SET RELATION TO cFabric+color INTO Fabric ADDITIVE
SET RELATION TO cMfgOrdNo INTO MMFGORDH ADDITIVE

LOCATE

IF llOpenBom
  USE IN BOM
ENDIF

IF llMfgOprhd
  USE IN MFGOPRHD
ENDIF

IF llAPVENDOR
  USE IN APVENDOR
ENDIF  

IF llCTKTBOM
  USE IN CTKTBOM
ENDIF  
RETURN

IF llTrkRolls .AND. llRolls
  USE IN ROLLS
ENDIF


*-- End of Code.
*:****************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
*: Purpose   : Create the temp files.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCreatemp()
*:****************************************************************
*
FUNCTION lfCreatemp


SELECT (lcMainF)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+11,4]

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'llEndLine'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'MfgCode'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'MfgCodeNo'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 2
laFileStru[lnFileStru,4] = 0

*-- Vendor address.

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cvencomp'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'caddress1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'caddress2'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'caddress3'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'caddress4'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'caddress5'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cOrder'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cType'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0


Create Table (gcWorkDir+lcTmpFile) FROM ARRAY laFileStru
INDEX ON cMfgOrdNo+cWareCode+STR(MfgCodeNo)+cType+cFabric+Color+Dyelot+cOrder+NoteFlag TAG (lcTmpFile)

*-- End OF lfCreatemp
*:****************************************************************
*: Name      : lFilOpArry
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
*: Purpose   : Create arrya Hold the Operation.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCreatemp()
*:****************************************************************
*
FUNCTION lFilOpArry
PRIVATE lnAlias
DIMENSION laOpration[1,3]
laOpration = ''

STORE 0  TO lnAryCount
lnAlias = SELECT (0)

*-- Get the Operation from the CtkBom file.
IF SEEK('T'+&lcMainf..cMfgOrdNo,'MFGOPRHD')
  SELECT MFGOPRHD
  SCAN WHILE cImtyp+cTktno+cOprcode = 'T'+&lcMainf..cMfgOrdNo
    lnAryCount = lnAryCount + 1  
    IF !EMPTY(laOpration[1,1])
      DECLARE laOpration[lnAryCount,3]
    ENDIF
    laOpration[lnAryCount,1] = ALLTRIM(cOprcode)
    laOpration[lnAryCount,2] = ALLTRIM(cOperseq)
    laOpration[lnAryCount,3] = IIF(linhouse,ALLTRIM(cContname),ALLTRIM(cContCode))
  ENDSCAN
ELSE
  *-- Get the operation from the BOM file.
  IF SEEK(&lcMainf..cFabric,'BOM')
  SELECT BOM
    SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = &lcMainf..cFabric;
      FOR cCatgTyp $ 'M' .AND. !EMPTY(ALLTRIM(MfgCode))
      lnAryCount = lnAryCount + 1  
      IF !EMPTY(laOpration[1])
        DECLARE laOpration[lnAryCount,3]
      ENDIF
      laOpration[lnAryCount,1] = ALLTRIM(MfgCode)
      laOpration[lnAryCount,2] = lfCodeSeq(MfgCode)
    ENDSCAN
  ENDIF  
ENDIF

SELECT (lnAlias)
RETURN 

*-- End OF lFilOpArry
*:****************************************************************
*: Name      : lfCodeSeq
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
*: Purpose   : get the sequence number for the opratio from codes file.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCodeSeq()
*:****************************************************************
*
FUNCTION lfCodeSeq
PARAMETER lcParam
PRIVATE lnAlias , lcReturn , lcOldOrder

STORE '' TO lcReturn ,lcOldOrder 

lnAlias = SELECT(0)

SELECT CODES
lcOldOrder = ORDER()
SET ORDER TO Ccode_no
*-cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
IF SEEK('N'+'MFGCODE   '+lcParam)
  LOCATE REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = ;
  'N'+'MFGCODE   '+lcParam FOR crltd_nam = 'COPERSEQ  '
  IF FOUND()
    lcReturn = ALLTRIM(crltd_vlu)
  ENDIF
ENDIF

SELECT CODES
SET ORDER TO &lcOldOrder
SELECT (lnAlias)
RETURN lcReturn

*-- End Of lfCodeSeq
*:****************************************************************
*: Name      : lfAddIssue
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
*: Purpose   : get the issue Qty.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCodeSeq()
*:****************************************************************
*
FUNCTION lfAddIssue
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
IF SEEK('T' +&lcMainf..cMfgOrdNo,'CTKTBOM')
  SELECT Ctktbom
  SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = 'T' + &lcMainf..cMfgOrdNo ;
    FOR cOprCode = laOpration[I,1]
    =SEEK(LEFT(Ctktbom.Item,7),'FABRIC')
    =SEEK(Fabric.Vendor,'APVENDOR') 
    SELECT (lcTmpFile)
    APPEND BLANK
    GATHER MEMVAR MEMO
    
    REPLACE  cFabric    WITH Ctktbom.ITEM        ,;
             COLOR      WITH Ctktbom.iclr        ,;
             cvencomp   WITH Fabric.Vendor       ,;
             caddress1  WITH APVENDOR.cvencomp   ,;
             MfgCodeNo  WITH VAL(laOpration[I,2]),;
             NoteFlag   WITH 'Z'                 ,;
             cOrder     WITH 'C'                 ,;
             MfgCode    WITH laOpration[I,1]     ,;
             nmfgtotqty WITH Ctktbom.Issue_qty   ,;
             Reference  WITH IIF(llUpEndGrp,'~!',''),;
             cType      WITH "2"
    llUpEndGrp = .F.
    *-- Update With Rolls
    
    IF llTrkRolls 
      SCATTER MEMVAR MEMO
      *-- crollitem+color+cwarecode+dyelot+crollid+trancd+crsession
      IF SEEK(cFabric+Color+cwarecode+Dyelot,'Rolls')
        SELECT Rolls
        LOCATE REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
        &lcTmpFile..cFabric+ &lcTmpFile..Color + &lcTmpFile..cWareCode+&lcTmpFile..Dyelot;
        FOR Trancd = "2" .AND. Nqty > 0 .AND. ctktno = &lcTmpFile..cmfgordno
      
        IF FOUND()
          lnRolls = 1
          SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
            &lcTmpFile..cFabric+ &lcTmpFile..Color + &lcTmpFile..cWareCode+&lcTmpFile..Dyelot;
            FOR Trancd = "2" .AND. Nqty > 0 .AND. ctktno = &lcTmpFile..cmfgordno
            SELECT (lcTmpFile)
            APPEND BLANk
            GATHER MEMVAR MEMO
            REPLACE Reference   WITH Rolls.cRollid,;
                    nmfgtotqty  WITH Rolls.Nqty   ,;
                    Cfabric1    WITH IIF(lnRolls = 1,'ROLL' ,''),;
                    cOrder      WITH 'D',;
                    cType       WITH "2"
            SELECT ROLLS
            lnRolls = lnRolls + 1
          ENDSCAN
        
        ENDIF
      ENDIF
    ENDIF
    SELECT Ctktbom
  ENDSCAN

ENDIF
SELECT (lnAlias)
RETURN 

*-- End OF lfAddIssue
*:****************************************************************