*:***************************************************************************
*: Program file  : POSTYPTH.PRG
*: Program desc. : CUSTOMIZED PURCHASE ORDER FOR .
*: Date          : 05/26/2008
*: System        : Aria 4 XP
*: Module        : PURCHASE ORDER (PO)
*: Developer     : Mariam Mazhar [MMT]
*: Tracking Job Number: C201005 {T20080415.0044}
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
*:***************************************************************************
lnAlias=Alias()
laCompAdd[6]    = 'Fax#   : '+TRANSFORM(lcCompFax , '@R '+lcPhonPict)
DO lfShiftArr WITH laCompAdd
STORE 0  TO  lnContPO



IF !USED(lcPosln_A)
  =gfOpenTable("POSLN","POSLN",'SH', @lcPosln_A)
ENDIF
SELECT POSHDR
SET SKIP TO 
SCAN
   STORE 0  TO LcPageOf , lnBoth 
     *-- The function which we get the last recno for the po.
  =lfRecNo()
  
  *-- Get the page counters.
  IF lnContPO # 0
    IF llRpPrtPn AND SEEK('P' + POSHDR.PO , 'NOTEPAD')
      lnBoth = lnContPO + MEMLINES(NOTEPAD.MNOTES)
    ELSE
     lnBoth = lnContPO
    ENDIF
  ENDIF
  *-- 1 for the header
  

  LcPageOf =  CEILING(lnBoth/25)

  *--Prepare the Temp file record to print the first report (The PO Header)
  WAIT WINDOW "Collecting data for PO#: " + PO NOWAIT
  =SEEK(POSLN.STYLE,'STYLE')
  REPLACE cadd_user   WITH ALLTRIM(STR(LcPageOf))         ;
          BUYER       WITH STYLE.Content1      
ENDSCAN 
SET SKIP TO &lcSkipExpr
SELECT (lnAlias)
locate
RETURN


*!*************************************************************
*! Name        : lfRecNo
*: Developer   : Mariam Mazhar [MMT]
*! Date        : 05/26/2008
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

PRIVATE llNoteLine
llNoteLine = .F.

IF gfSEEK(POSHDR.CBUSDOCU+POSHDR.CSTYTYPE+ POSHDR.PO,lcPosln_A)
  SELECT (lcPosln_A)
  lcStyMAjor = ''
  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = POSHDR.CBUSDOCU+POSHDR.CSTYTYPE+ POSHDR.PO FOR tranCd = '1'
    
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
    
    IF llRpPrtSn AND SEEK(STYLE,'STYLE') AND  SEEK('F'+STYLE.cStyMajor,'Notepad') .AND. ;
                       !EMPTY(ALLTRIM(Notepad.MNotes)) AND lcStyMAjor <> Style.cStyMajor
      
      lnContPO  = lnContPo + MEMLINES(NOTEPAD.MNOTES)
      llNoteLine = .T.
      
      lcStyMAjor = Style.cStyMajor
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)
RETURN ''

*-- End of lfRecNo.


*!*************************************************************
*! Name        : lfGetTotLn
*: Developer   : Mariam Mazhar [MMT]
*! Date        : 05/26/2008
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
FUNCTION lfGetTotLn
PARAMETERS lnTotLines,lnNewLines,lnMaxLines

PRIVATE lnNewTotLn
IF llNoteLine
  lnTotLines = lnTotLines + 1
ENDIF

*IF lnMaxLines - MOD(lnTotLines,lnMaxLines) > lnNewLines
  lnNewTotLn = lnTotLines + lnNewLines
*ELSE
 * lnNewTotLn = (CEILING(lnTotLines / 25) * 25) + lnNewLines
*ENDIF

RETURN (lnNewTotLn)
*--end of lfGetTotLn.