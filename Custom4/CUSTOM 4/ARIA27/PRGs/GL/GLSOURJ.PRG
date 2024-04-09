*:************************************************************************
*:
*: Procedure file: GLSOURJ.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:               
*:      Documented   /  /
*:************************************************************************
*B600347,1 ORABY  29/5/95   When deleting a record the system try to open a file with name = LEFT(filename,3) (nonsense)
*B600347,1                  This was meant to be the alias not the file name. I stopped this feature we do not need it here
*E500152,1 MAB    10/22/97  Adding a new Push button called <Selected Transactions>
*E500152,1                  When we push it it give a mover help us selecting transactions
*E500152,1                  Adding a new Invisible button having the new field name.
*E500152,1                  Adding a new MEMO Field called mSJTrans in Table GLSUBJOR
*E500152,1                  This new field contains codes for Selected Transactions.
*B602241,1 RENEE 12/20/98   Call *.SPR from the screens directory
*

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE laKeyField [1,4]

*E500152,1    : Declare arrays used by transactions
*-- laVdEnt   : Array that Hold the Hard Coded items of Transactions
*-- laFromEnt : Array that Hold the Valid Entries items of Transactions
*-- laSource  : Array that Hold the available Transactions of mover
*-- laTarget  : Array that Hold the selected  Transactions of mover
DIMENSION laVdEnt[20,2],laFromEnt[1,1],laSource[1],laTarget[1]
*E500152,1 (End)
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'SRCJRNL'
laKeyField[1,4] = 1

laDefProc    = .T.
laDefProc[7] = .F.    && control the delete function
laDefProc[9] = .F.    && control the save   function
llBrowse     = .F.

IF !gfSetup()
  RETURN
ENDIF
 
IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
 

  *E500152,1  
  *--      : Hard coded array elements we initially have
  *-- Note : The following items are compared with Valid Entries and 
  *--      : the new items found in laFromEnt are added to laVdEnt. 
  *---------------------------------------------- 
  STORE '' TO laFromEnt,laSource,laTarget

  laVdEnt[1,2]  = "IN"
  laVdEnt[2,2]  = "VI"
  laVdEnt[3,2]  = "CR"
  laVdEnt[4,2]  = "CA"
  laVdEnt[5,2]  = "DA"
  laVdEnt[6,2]  = "RM"
  laVdEnt[7,2]  = "VR"
  laVdEnt[8,2]  = "IP"
  laVdEnt[9,2]  = "IA"
  laVdEnt[10,2] = "PO"
  laVdEnt[11,2] = "CT"
  laVdEnt[12,2] = "ZE"
  laVdEnt[13,2] = "MP"
  laVdEnt[14,2] = "MA"
  laVdEnt[15,2] = "MO"
  laVdEnt[16,2] = "I"
  laVdEnt[17,2] = "P"
  laVdEnt[18,2] = "V"
  laVdEnt[19,2] = "B"
  laVdEnt[20,2] = "A"

  laVdEnt[1,1]  = "A/R Invoice"
  laVdEnt[2,1]  = "Void Invoice"
  laVdEnt[3,1]  = "Cash Receipt"
  laVdEnt[4,1]  = "Credit Adjustment"
  laVdEnt[5,1]  = "Debit  Adjustment"
  laVdEnt[6,1]  = "Return Merchandise"
  laVdEnt[7,1]  = "Void Returns"
  laVdEnt[8,1]  = "Inventory Physical"
  laVdEnt[9,1]  = "Inventory Adjustment"
  laVdEnt[10,1] = "P/O Receiving"
  laVdEnt[11,1] = "C/T Receiving"
  laVdEnt[12,1] = "Zero Out Stock"
  laVdEnt[13,1] = "Material Inventory Physical  "
  laVdEnt[14,1] = "Material Inventory Adjustment"
  laVdEnt[15,1] = "Material P/O Receiving"
  laVdEnt[16,1] = "Payable Invoices"
  laVdEnt[17,1] = "Vendor Payments"
  laVdEnt[18,1] = "Void Payments"
  laVdEnt[19,1] = "Bank Adjustments"
  laVdEnt[20,1] = "Apply Debit Memo"

  *-- Getting elements of array contains Valid Entries
  =gfGetVld('MSJTRANS',@laFromEnt) 

  lcExact = SET("EXACT")
  SET EXACT ON

  *-- Check if valid entry array element not in hard coded array,
  *-- we add it to hard coded array.
  FOR lnI = 1 TO ALEN(laFromEnt,1)
    IF ASCAN(laVdEnt,ALLTRIM(laFromEnt[lnI,2])) = 0
      DIMENSION laVdEnt[ALEN(laVdEnt,1)+1,2]
      laVdEnt[ALEN(laVdEnt,1),1] = laFromEnt[lnI,1]
      laVdEnt[ALEN(laVdEnt,1),2] = laFromEnt[lnI,2]
    ENDIF
  ENDFOR 

  SET EXACT &lcExact
  *E500152,1 (End)
  
ENDIF
 


*B602241,1 Call *.SPR from screens directory
*DO Glsourj.SPR
DO (gcScrDir + gcWinAppl + '\Glsourj.SPR')
*B602241,1 end   
 

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

*E500152,1 : I have to change comment of pbZoom to #OBJCNT and put 
*--        : the comment of pbTrans the same to control it by my self
*--        : because an error occured in windows mode.
DO CASE
  CASE laScrMode[1]
    *E500152,1
    SHOW GET pbZoom  DISABLE
    SHOW GET pbTrans DISABLE
    *E500152,1 (End)
  CASE laScrMode[2]
    IF laData[1] = lcSj_Def  && the user can not delete the "GJ" Id 
      SHOW GET pbDlt DISABLE
    ELSE
      SHOW GET pbDlt ENABLE
    ENDIF
    *E500152,1
    SHOW GET pbZoom  ENABLE
    SHOW GET pbTrans ENABLE
    *-- I have to call lfLoadTran function that accomplish the 
    *-- task of filling laSource and laTarget prepairing call to Mover.  
    =lfLoadTran()
    *E500152,1 (End)

  *E500152,1   : Doing the same prior action in both Edit and Add modes.
  CASE laScrMode[3] OR laScrMode[4]
    SHOW GET pbZoom  ENABLE
    SHOW GET pbTrans ENABLE
    =lfLoadTran()
  *E500152,1 (End)
ENDCASE 

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
 
IF llBrowse .OR. (!EMPTY(laData[1]) AND LASTKEY() = 13)
  IF ATC("?",laData[1]) > 0 .OR. llBrowse
    =gfBrows()
  ELSE
    IF !SEEK(laData[1],'SYDAPPL')
      =gfSeekRec()
    ELSE
      =gfModalGen("TRM02211B00000","Dialog",laData[1])
      STORE SPACE(2) TO laData[1]
      _CURROBJ = OBJNUM(laData[1])
      SHOW GET laData[1]
    ENDIF  
  ENDIF
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      PROCEDURE: lpDelScr
*!
*!**************************************************************************
*
 
PROCEDURE lpDelScr
llUsed     = .F.
lnNOfFiles = 0
IF NOT USED("SYDFLFLD")
  SELECT 0
  USE &gcSysHome.SYDFLFLD 
  llUsed = .T.
ENDIF
SELECT SYDFLFLD

* select the files that used this field from sydflfld into array
SELECT cFile_nam FROM SYDFLFLD WHERE cFld_name = "CSRCJRNL";
 AND cFile_nam <> "GLSUBJOR.DBF";
 GROUP BY cFile_nam INTO ARRAY laFiles
 
* store the number of array   
lnNOfRec = _TALLY

* store the number of array  
IF llUsed
  USE IN   SYDFLFLD
  llUsed = .F.
ENDIF

* check if the record information used by another files
FOR lnCount = 1 TO lnNOfRec
 
  * store the alias of the file 
 laFiles[lnCount] = ALLTRIM(laFiles[lnCount])  
*B600347,1 Stop the next silly  statment 
* laFiles[lnCount] = STUFF(laFiles[lnCount],LEN(laFiles[lnCount])-3,4,'')

  IF NOT USED(laFiles[lnCount])
      SELECT 0
    USE gcDataDir+laFiles[lnCount]
    llUsed = .T.
  ENDIF
  
  SELECT (laFiles[lnCount])
  
  *  search for the record which match the record information
  LOCATE FOR CSRCJRNL = laData[1]
  
  IF FOUND()  
    lnNOfFiles = lnNOfFiles + 1
    EXIT
  ENDIF
  IF llUsed
    USE IN laFiles[lnCount]
    llUsed = .F.
  ENDIF
ENDFOR

IF NOT USED("GLSUBJOR")
  USE &gcDataDir.GLSUBJOR IN SELECT(1)
ENDIF

SELECT GLSUBJOR 

* check if there is any file share information
IF lnNOfFiles > 0

  ****   Find the file logical  name
  
  IF NOT USED("SYDFILES")
    SELECT 0
    USE &gcSysHome.SYDFILES
    llUsed = .T.
  ENDIF
  
  SELECT SYDFILES
  SELECT CFILE_TTL FROM SYDFILES WHERE CFILE_NAM = (laFiles[lnCount]+".DBF");
  INTO ARRAY laPhiName
  IF NOT USED("GLSUBJOR")
    USE &gcDataDir.GLSUBJOR IN SELECT(1)
  ENDIF

  SELECT GLSUBJOR 
  
  ****   Source journal ð is used by the ð file *******
  =gfModalGen("TRM02172B00000","DIALOG",laData[1]+"|"+ALLTRIM(laPhiName[1]))
  
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS
ELSE
  DELETE  
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF

*!**************************************************************************
*!
*!      PROCEDURE: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

** Convert the first letter into upper case
laData[2] = UPPER(SUBSTR(laData[2],1,1))+SUBSTR(laData[2],2)
laData[3] = UPPER(SUBSTR(laData[3],1,1))+SUBSTR(laData[3],2)
laData[4] = UPPER(SUBSTR(laData[4],1,1))+SUBSTR(laData[4],2)

IF laScrMode[4]
  APPEND BLANK
ENDIF  

GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()


*!*************************************************************
*! Name      : lfLoadTran
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/22/97
*! Purpose   : Filling laSource and laTarget by desired items
*!           : prepairing call to Mover.  
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfSubStr
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfLoadTran()
*!*************************************************************
*E500152,1
*
FUNCTION lfLoadTran
*-- lcString   : Hold all data of the (memo field mSJTrans) except this I am in.
*-- laString   : Array holding the date of lcString but in items form. 
*-- lcScanCond : Hold Scan condition.
*--            : All variables initially have spaces
PRIVATE lcString,lcScanCond
DIMENSION laString[1],laSource[1],laTarget[1]
STORE '' TO laString,laSource,laTarget,lcString

SELECT GLSUBJOR 
lnRec = RECNO()

lcScanCond = IIF(SEEK(laData[1]),"FOR CSRCJRNL <> laData[1]","")

*-- Scan to have a string holds all items in other records.
SCAN &lcScanCond
  IF !EMPTY(ALLTRIM(msjTrans))
    lcString = IIF( EMPTY(ALLTRIM(lcString))         ,;
                    ALLTRIM(msjTrans)                ,;                    
                    lcString + "~" + ALLTRIM(msjTrans))
  ENDIF  
ENDSCAN

*-- Returning cursor to its original position
IF BETWEEN(lnRec,1,RECCOUNT('GLSUBJOR'))
   GO lnRec
ENDIF

*-- If the string not empty concating it filling laString 
*-- otherwise fill laSource with all description data in laVdEnt
IF !EMPTY(ALLTRIM(lcString))
  =gfSubStr(ALLTRIM(lcString),@laString,'~')
ELSE
  DIMENSION laSource[ALEN(laVdEnt,1)]
  FOR lnI = 1 TO ALEN(laVdEnt,1)
    laSource[lnI] = laVdEnt[lnI,1]
  ENDFOR  
ENDIF  

lcExact = SET("EXACT")
SET EXACT ON

*-- If laString have data fill laSource by the equavelent 
*-- description data from laVdEnt.
IF !EMPTY(laString[1])
  FOR lnI = 1 TO ALEN(laVdEnt,1)
    IF ASCAN(laString,ALLTRIM(laVdEnt[lnI,2])) = 0
      IF !EMPTY(laSource[1])
        DIMENSION laSource[ALEN(laSource,1)+1]
      ENDIF
      laSource[ALEN(laSource,1)] = laVdEnt[lnI,1]
    ENDIF
  ENDFOR 
ENDIF

IF !EMPTY(laData[5])
  *-- Filling laTarget by coded data from laData[5]
  =gfSubStr(ALLTRIM(laData[5]),@laTarget,'~')
ENDIF  

*-- Filling laTarget by equavelent description data from laVdEnt,
*-- If we found data in it. 
IF !EMPTY(laTarget[1])
  FOR lnI = 1 TO ALEN(laTarget,1)
    *-- We use the CEILING because laVdEnt is a two dimension array 
    lnItem = CEILING(ASCAN(laVdEnt,ALLTRIM(laTarget[lnI]))/2)
    laTarget[lnI] = laVdEnt[lnItem,1]
  ENDFOR 
ENDIF

SET EXACT &lcExact

*E500152,1 (End)


*!*************************************************************
*! Name      : lfvTrans
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/22/97
*! Purpose   : Validating Selected Transactions after using Mover
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvTrans()
*!*************************************************************
*E500152,1
*
FUNCTION lfvTrans
*E500152,1
*-- New Message
*-- If no transactions found say that 
*-- "All transactions have been assigned to other source journals."
*--                          <  OK  > 
IF EMPTY(laSource[1])
  =gfModalGen("TRM02248B02014","Dialog")
  RETURN
ENDIF
*--          : Descrip. # 1
*-- llCont   : A flag if it is False this means that we either choice 
*--          : < Cancel >, Press ESCAPE or < OK >, Press ENTER but 
*--          : does not change any thing in mover 
*-- laTemp   : Array holding the Temporary date of laTarget, and after mover 
*--          : it compaired with it, if it is not the same continue temporary 
*--          : saving, else return because data does not change.  
PRIVATE llCont
llCont = .F.
DIMENSION laTemp[ALEN(laTarget,1)]
=ACOPY(laTarget,laTemp)

= gfMover(@laSource,@laTarget,"Selected Transactions",!laScrMode[2])  

*-- START   : Descrip. # 1

IF EMPTY(laTarget[1]) AND EMPTY(laTemp[1])
  RETURN
ENDIF

IF ALEN(laTemp,1) = ALEN(laTarget,1)
  IF !EMPTY(laTarget[1])
    FOR lnI = 1 TO ALEN(laTarget,1)
      llCont = (ASCAN(laTemp,laTarget[lnI]) = 0)
      IF llCont
        EXIT
      ENDIF 
    ENDFOR
  ELSE
    llCont = .T.
  ENDIF  
ELSE
  llCont = .T.
ENDIF 

IF !llCont
  RETURN
ENDIF
*-- END    : Descrip. # 1

*-- Saving equavelent code data in array if any in laData[5]
*-- separating it by ~ .
IF laScrMode[3] OR laScrMode[4]
  ladata[5] = ''
  IF !EMPTY(laTarget[1])
    FOR lnI = 1 TO ALEN(laTarget,1)
      lnItem = CEILING(ASCAN(laVdEnt,ALLTRIM(laTarget[lnI]))/2)
      ladata[5] = IIF( EMPTY(ALLTRIM(ladata[5]))                 ,;
                       ALLTRIM(laVdEnt[lnItem,2])                ,;
                       ALLTRIM(ladata[5]) + "~" + ALLTRIM(laVdEnt[lnItem,2]))
    ENDFOR 
  ENDIF
ENDIF

*E500152,1 (End)
