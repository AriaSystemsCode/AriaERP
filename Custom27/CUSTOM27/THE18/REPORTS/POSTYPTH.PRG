*:***************************************************************************
*: Program file  : POSTYPTH.PRG
*: Program desc. : CUSTOMIZED PURCHASE ORDER FOR .
*: Date          : 10/22/2000
*: System        : Aria Advantage Series.
*: Module        : PURCHASE ORDER (PO)
*: Developer     : Walid A. Wahab (WAB)
*: Tracking Job Number: C102803
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYPEE
*:***************************************************************************
*:Modifications  :
*: B607165,1 AMH 04/24/2003 Fix bug of wrong page number x of y.
*:***************************************************************************
lnAlias=Alias()

laCompAdd[6]    = 'Fax#   : '+TRANSFORM(lcCompFax , lcPhonPict)
* Pack the addresses array
DO lfShiftArr WITH laCompAdd

*-llFlgNote   >> Variable used to check if the notepade is finished or not.
*-lnContPO    >> Hold the last lineno of the invoice.
STORE 0  TO  lnContPO

*-- Open the POSLN.DBF again to get the last lineno for the PO.
IF !USED(lcPosln_A)
  =gfOpenFile(gcDataDir+"POSLN","POSLN",'SH', @lcPosln_A, .T.)
ENDIF

IF USED(lcTmpPoHd)
  SELECT (lcTmpPoHd)
  DELETE ALL
ENDIF
SELECT POSHDR
SET SKIP TO 

SELECT POSHDR
SCAN FOR &lcRpExp 
  STORE 0  TO LcPageOf , lnBoth 

  *-- The function which we get the last recno for the po.
  =lfRecNo()
  
  *-- Get the page counters.
  IF lnContPO # 0
    IF llRpPrtPn AND SEEK('P' + POSHDR.PO , 'NOTEPAD')
      
      *B607165,1 AMH Correct the count method [Start]
      *lnBoth = lnContPO + CEILING(MEMLINES(NOTEPAD.MNOTES)/ 5)
      lnBoth = lnContPO + MEMLINES(NOTEPAD.MNOTES)
      *B607165,1 AMH [End]
      
    ELSE
     lnBoth = lnContPO
    ENDIF
  ENDIF
  *-- 1 for the header
  
  *B607165,1 AMH Correct the count method [Start]
  *LcPageOf =  CEILING(lnBoth/5)
  LcPageOf =  CEILING(lnBoth/25)
  *B607165,1 AMH [End]

  *--Prepare the Temp file record to print the first report (The PO Header)
  WAIT WINDOW "Collecting data for PO#: " + PO NOWAIT
  SCATTER MEMVAR MEMO
  SELECT (lcTmpPoHd)
  APPEND BLANK
  GATHER MEMVAR MEMO 
  =SEEK(POSLN.STYLE,'STYLE')
  REPLACE PageOf     WITH LcPageOf         ;
          Content    WITH STYLE.Content1      

  *-- Case for the notepad.

  *-- In case the lines < 11 and print notpad = YES and there is no notepad in the notepad file ,
  *-- get in to create records till the end of the page.
ENDSCAN
SELECT POSHDR
SET RELATION TO Poshdr.cstytype+ Poshdr.po INTO  (lcTmpPoHd) ADDITIVE

SET SKIP TO &lcSkipExpr
SELECT (lnAlias)
locate
RETURN

*-- End of lfvMsgSave.
*!*************************************************************
*! Name      : lfFirstTim
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Function to initial the variabls in the first time.
*!*************************************************************
*! Called from : SYREPUVR.DBF
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFirstTim()
*!*************************************************************
FUNCTION lfFirstTim
PARAMETER lcReturn

SELECT POSHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]


*-- Field for the Content.
laFileStru[lnFileStru + 1 , 1] = 'Content'
laFileStru[lnFileStru + 1 , 2] = 'C'
laFileStru[lnFileStru + 1 , 3] = 20
laFileStru[lnFileStru + 1 , 4] = 0

*-- Field for the 2nd part of the page no.
laFileStru[lnFileStru + 2 , 1] = 'PageOf'
laFileStru[lnFileStru + 2 , 2] = 'N'
laFileStru[lnFileStru + 2 , 3] = 6
laFileStru[lnFileStru + 2 , 4] = 0


CREATE TABLE (gcWorkDir+lcTmpPoHd) FROM ARRAY laFileStru
INDEX ON  cStyType + PO  TAG (lcTmpPoHd) OF (gcWorkDir+lcTmpPoHd+".CDX")

RETURN ''
*-- End of lfFirstTim.
*!*************************************************************
*! Name        : lfRecNo
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 10/09/2000
*! Purpose     : To get the last Record for the po to know how
*!             : many records in this po.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : lfRecNo()
*!*************************************************************
FUNCTION lfRecNo
PRIVATE lcAlias
lcAlias = ALIAS()
lnContPO = 0 

*B607165,1 AMH Flag to know if the last line is notepad line or not [Start]
PRIVATE llNoteLine
llNoteLine = .F.
*B607165,1 AMH [Start]

IF SEEK(LCRPFORM + POSHDR.PO,lcPosln_A)
  SELECT (lcPosln_A)
  lcStyMAjor = ''
  SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = LCRPFORM + POSHDR.PO FOR tranCd = '1'
    
    *B607165,1 AMH Correct the count method [Start]
    *lnContPO  = lnContPO  + 1
    *By Trying to print full page with notepad lines we get that :
    *-- full page contined maximum 25 notepad lines.
    *-- each po line = 4 notepad lines in case of print sizes yes.
    *-- each po line = 2 notepad lines in case of print sizes no.
    *-- Special case for po lines without sizes full page contined maximum 11 lines only.
    * Notes : All modifications under this entry will base the above rolls.
    IF llRpSize
      lnContPo = lfGetTotLn(lnContPo,4,25)
    ELSE
      lnContPo = lfGetTotLn(lnContPo,2,23)
    ENDIF
    llNoteLine = .F.
    *B607165,1 AMH [End]
    
    IF llRpPrtSn AND SEEK(STYLE,'STYLE') AND  SEEK('F'+STYLE.cStyMajor,'Notepad') .AND. ;
                       !EMPTY(ALLTRIM(Notepad.MNotes)) AND lcStyMAjor <> Style.cStyMajor
      
      *B607165,1 AMH Correct the count method [Start]
      *lnContPO  = lnContPO  + CEILING(MEMLINES(NOTEPAD.MNOTES)/ 5)
      lnContPO  = lnContPo + MEMLINES(NOTEPAD.MNOTES)
      llNoteLine = .T.
      *B607165,1 AMH [End]
      
      lcStyMAjor = Style.cStyMajor
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)
RETURN ''

*-- End of lfRecNo.

*!*************************************************************
*! Name        : lfGetTotLn
*! Developer   : Ahmed Maher (AMH)
*! Date        : 4/24/2003
*! Purpose     : Calculate how many lines will printing for the PO
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : lfGetTotLn()
*!*************************************************************
*B607165,1 AMH
FUNCTION lfGetTotLn
PARAMETERS lnTotLines,lnNewLines,lnMaxLines

PRIVATE lnNewTotLn
IF llNoteLine
  lnTotLines = lnTotLines + 1
ENDIF

IF lnMaxLines - MOD(lnTotLines,lnMaxLines) > lnNewLines
  lnNewTotLn = lnTotLines + lnNewLines
ELSE
  lnNewTotLn = (CEILING(lnTotLines / 25) * 25) + lnNewLines
ENDIF

RETURN (lnNewTotLn)
*--end of lfGetTotLn.