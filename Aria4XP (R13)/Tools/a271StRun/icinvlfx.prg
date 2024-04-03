*:************************************************************************
*: Program file  : ICINVLFX
*: Program desc. : Fix program to add new record in the sequence file
*:                 for MdInvnTh file and delete the cBatch field from
*:                 SydFlFld of MdInvnTh and MdInvnTl files
*: Module: Inventory Control
*: Date  : 06/20/2000
*: Developer: Khalid Mohi El-din
*:************************************************************************
*: B603596,1 KHM 06/20/2000
*:************************************************************************
PARAMETER lcDataDir

*-- Inserting a record for the sequence of the mark down locking inventory
*-- by using the "MDINVNTH" file.
IF USED("SEQUENCE")
  USE IN SEQUENCE
ENDIF
USE lcDataDir+'SEQUENCE.DBF' IN 0 ORDER Cseq_type
SELECT Sequence

IF SEEK("CBATCH    ")
  LOCATE REST WHILE cSeq_Type+cSeq_Group = "CBATCH    " ;
              FOR cFile_Nam = "GLBATCH "
  SCATTER MEMVAR MEMO
  m.cSeq_Type  = "CLKBATCH"
  m.cFile_Nam  = "MDINVNTH"
  m.cFile_Tag  = "MDINVNTH"
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDIF
*-- Deleting the record that is related to the GlBatch.
IF SEEK ("CBATCH    ")
  LOCATE REST WHILE cSeq_Type+cSeq_Group = "CBATCH    " ;
              FOR cFile_Nam = "GLBATCH "
  IF FOUND()
    BLANK
    DELETE
  ENDIF  
ENDIF  

*-- Deleting the record that is related to the MDINVNTH.
IF SEEK ("CBATCH    ")
  LOCATE REST WHILE cSeq_Type+cSeq_Group = "CBATCH    " ;
              FOR cFile_Nam = "MDINVNTH"
  IF FOUND()
    BLANK
    DELETE
  ENDIF
ENDIF  

*-- Replace the cLkBatch with cBatch in the MDINVNTH
IF FILE(lcDataDir+'MDINVNTH.DBF')
  IF USED('MDINVNTH')
    USE IN MDINVNTH
  ENDIF
  USE lcDataDir+'MDINVNTH.DBF' IN 0
  SELECT MDINVNTH
  =AFIELD(laFields)
  IF ASCAN(laFields,'CBATCH') > 0

    IF TYPE("CLKBATCH") = "C"
      REPLACE ALL cLkBatch WITH cBatch  
    ELSE
      *-- Get a unique name for the Inventory Locking header file.
      lcTmpMdHdr = ("X"+SUBSTR(SYS(2015),4))

      *-- Create an array to hold the indexes of the file.
      DIME laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cBatType+cLkBatch'
      laIndex[1,2] = 'Mdinvnth'
    
      *-- Copy the structure with the new field in the temporary file
      SELECT *, cBatch AS cLkBatch  FROM (lcDataDir+'MDINVNTH');
         INTO DBF (lcDataDir+lcTmpMdHdr)
    
      SELECT (lcTmpMdHdr)
   
      PRIVATE lnCount
      *-- Create the indexes of the file
      FOR lnCount = 1 TO ALEN(laIndex,1)
         INDEX ON &laIndex[lnCount,1] TAG &laIndex[lnCount,2] ADDITIVE
      ENDFOR
      USE IN MDINVNTH
      USE IN (lcTmpMdHdr)
    
      *-- Erase the original file
      ERASE (lcDataDir+'MDINVNTH.DBF')
      ERASE (lcDataDir+'MDINVNTH.CDX')
    
      *-- Rename the temporary file to be the standard file.
      RENAME (lcDataDir+lcTmpMdHdr+'.DBF') TO (lcDataDir+"MDINVNTH.DBF")
      RENAME (lcDataDir+lcTmpMdHdr+'.CDX') TO (lcDataDir+"MDINVNTH.CDX")
    ENDIF 
  ENDIF  
ELSE
  RETURN(.F.)  
ENDIF

*-- Replace the cLkBatch with cBatch in the MDINVNTL
IF FILE(lcDataDir+'MDINVNTL.DBF')
  IF USED('MDINVNTL')
    USE IN MDINVNTL
  ENDIF
  USE lcDataDir+'MDINVNTL.DBF' IN 0
  SELECT MDINVNTL
  =AFIELD(laLFields)
  IF ASCAN(laLFields,'CBATCH') > 0

    IF TYPE("CLKBATCH") = "C"
      REPLACE ALL cLkBatch WITH cBatch  
    ELSE
      *-- Get a unique name for the Inventory Locking header file.
      lcTmpMdLn = ("X"+SUBSTR(SYS(2015),4))

      *-- Create an array to hold the indexes of the file.
      DIME laIndex[2,2]
      laIndex = ''
      laIndex[1,1] = 'cBatType+cLkBatch+Style+Color+Dyelot+cLocation'
      laIndex[1,2] = 'Mdinvntl'
      laIndex[2,1] = 'Style+Color+Dyelot+cLocation+cBattype+cLkBatch'
      laIndex[2,2] = 'Mdinvntls'
    
      *-- Copy the structure with the new field in the temporary file
      SELECT *, cBatch AS cLkBatch  FROM (lcDataDir+'MDINVNTL');
         INTO DBF (lcDataDir+lcTmpMdLn)
    
      SELECT (lcTmpMdLn)
  
      PRIVATE lnCount
      *-- Create the indexes of the file
      FOR lnCount = 1 TO ALEN(laIndex,1)
         INDEX ON &laIndex[lnCount,1] TAG &laIndex[lnCount,2] ADDITIVE
      ENDFOR
      USE IN MDINVNTL
      USE IN (lcTmpMdLn)
    
      *-- Erase the original file
      ERASE (lcDataDir+'MDINVNTL.DBF')
      ERASE (lcDataDir+'MDINVNTL.CDX')
    
      *-- Rename the temporary file to be the standard file.
      RENAME (lcDataDir+lcTmpMdLn+'.DBF') TO (lcDataDir+"MDINVNTL.DBF")
      RENAME (lcDataDir+lcTmpMdLn+'.CDX') TO (lcDataDir+"MDINVNTL.CDX")
    ENDIF
  ENDIF  
ELSE
  RETURN(.F.)  
ENDIF

*-- Open the file fields in order to remove the cBatch field from
*-- MdInvnTh and MdInvnTl
IF !USED('SYDFLFLD')
  SELECT 0
  USE (gcSysHome+'SYDFLFLD')
  SET ORDER TO TAG Cfile_nam
  llOpnFlFld = .T.
ELSE
  SELECT SYDFLFLD
  SET ORDER TO TAG Cfile_nam
  llOpnFlFld = .F.
ENDIF

*-- Searching for field cBatch in MdInvnTh and delete it
SELECT SYDFLFLD
IF SEEK ("MDINVNTH")
  LOCATE REST WHILE cFile_Nam = "MDINVNTH" FOR cFld_Name = "CBATCH    "
  IF FOUND()
    BLANK
    DELETE
  ENDIF
ENDIF

*-- Searching for field cBatch in MdInvnTl and delete it
IF SEEK ("MDINVNTL")
  LOCATE REST WHILE cFile_Nam = "MDINVNTL" FOR cFld_Name = "CBATCH    "
  IF FOUND()
    BLANK
    DELETE
  ENDIF
ENDIF
*-- Closing the SydFlFld if opened in this session
IF llOpnFlFld
  USE IN SYDFLFLD
ENDIF