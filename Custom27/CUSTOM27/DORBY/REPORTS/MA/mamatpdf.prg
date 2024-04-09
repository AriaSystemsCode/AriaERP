*!*************************************************************
*! Name      : MAMATPDF     (C#101456)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 05/16/1999
*! Purpose   : Fabric Purchase Order For Dorby.
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!C101456,1 SSH 05/16/1999 
*!B802437,1 SSH 20/07/99 1- Leave left space
*!B802437,1 SSH          2- Dont Print notPad  line start with '*'
*!*************************************************************
lcNotes = ''
*---Functuns
*!*************************************************************
*! Name      : lfvMessg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 05/16/1999
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvMessg()
*!*************************************************************
FUNCTION lfvMessg
PARAMETER lcReturn,lcParam2

PRIVATE laMsg
lcMftype = 'DORMATDF'

IF FILE(gcDataDir+lcMftype+'.MEM')
  RESTORE FROM gcDataDir+lcMftype+'.MEM' ADDITIVE
ENDIF

DECLARE laMsg[3,2]       && Array to hold the name and length of the variables to be used in the Legal Liability screen

laMsg[1,1] = 'lcRpMg1'        && 1st. line Variable
laMsg[2,1] = 'lcRpMg2'        && 2nd. line Variable
laMsg[3,1] = 'lcRpMg3'        && 3rd. line Variable
laMsg[1,2] = 75                && Line length
lcReturn= gfOptMsg('laMsg','Lab dips /Strike Offs.')
SET MEMOWIDTH TO 75
SAVE TO gcDataDir+lcMftype+'.MEM' ALL LIKE lcRpMg*
lcReturn1 = SPACE(01)
RETURN lcReturn

*!*************************************************************
*! Name      : lfvMegVar
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 05/16/1999
*! Purpose   : Function to get memory Variable.
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvMegVar()
*!*************************************************************
FUNCTION lfvMegVar
PARAMETER lcReturn,lcParam2

lcMftype = 'DORMATDF'
SET MEMOWIDTH TO 75
IF FILE(gcDataDir+lcMftype+'.MEM')
  RESTORE FROM gcDataDir+lcMftype+'.MEM' ADDITIVE
ENDIF
RETURN lcReturn
*!*************************************************************
*! Name      : lfDNotes
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 05/17/1999
*! Purpose   : Delete Note pad variable
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : lfDNotes()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDNotes()
*!*************************************************************
FUNCTION lfDNotes
PARAMETER lcReturn,lcParam2,lcType

lcNotes = ''
lcTitle = ''
lcReturn = SPACE(01)
RETURN lcReturn
*!*************************************************************
*! Name      : lfvNotes
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 05/17/1999
*! Purpose   : NotePad Valid Function
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : lfvNotes()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvNotes()
*!*************************************************************
FUNCTION lfvNotes
PARAMETER lcReturn,lcParam2


IF llExternal
  RETURN ''
ENDIF

STORE '' TO lcTitle,lcNotes

*-- if you are at First position in dummy file [See notes in program header]
*

IF &lcNoteLns..cRecord = 'N1'
*(EVALUATE(lcNoteLns+'.cRecord') = 'N1' AND llRpPrtMn AND (RECNO('POFLN') = lnFabEnd) AND !EMPTY(ALLTRIM(NOTEPAD.MNOTES)))
  *-- if user want to print Item notepad and you find notes in notepad file.
  IF llRpPrtMn AND (RECNO('POFLN') = lnFabEnd) AND ;
     !EMPTY(ALLTRIM(NOTEPAD.MNOTES)) AND EMPTY(lcNotes)
    lnNotLine  = 1
    lcTitle = 'Item  ' + POFLN.Fabric + ' Notepad'
    SET MEMOWIDTH TO 75
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    DO WHILE lnNotLine <= MEMLINES(NOTEPAD.MNOTES)
       IF !EMPTY(MLINE(NOTEPAD.MNOTES,lnNotLine));
          AND SUBSTR(MLINE(NOTEPAD.MNOTES,lnNotLine),1,1) <> '*'
         lcNotes =  lcNotes + MLINE(NOTEPAD.MNOTES,lnNotLine) + CHR(13)
       ENDIF
       lnNotLine = lnNotLine+1
    ENDDO
    lcNotes =  ALLTRIM(lcNotes)
  ENDIF  && end if user want to print Item notepad.
ELSE  && else you are at second position in dummy file.
  *-- if user want to print Material PO notepad and you find notes in notepad file.
  IF llRpPrtPn AND (RECNO('POFLN') = lnMPOEnd) AND ;
     !EMPTY(ALLTRIM(NOTEPAD_A.MNOTES)) AND EMPTY(lcNotes)
    lcTitle = IIF(POFHDR.CMATTYPE = 'P','Purchase Order ',;
              IIF(POFHDR.CMATTYPE = 'R','Return Purchase Order ','Contract ')) +;
              'Notepad'
    lnNotLine = 1
    SET MEMOWIDTH TO 75
    lnMemLins = MEMLINES(NOTEPAD_A.MNOTES)
    DO WHILE lnNotLine <= MEMLINES(NOTEPAD_A.MNOTES)
       IF !EMPTY(MLINE(NOTEPAD_A.MNOTES,lnNotLine));
          AND SUBSTR(MLINE(NOTEPAD_A.MNOTES,lnNotLine),1,1) <> '*'
         lcNotes =  lcNotes + MLINE(NOTEPAD_A.MNOTES,lnNotLine) + CHR(13)
       ENDIF
       lnNotLine = lnNotLine + 1
    ENDDO
    lcNotes =  ALLTRIM(lcNotes)
  ENDIF   && end if user want to print Material PO notepad.
ENDIF     && end if you are at First position in dummy file.
RETURN ''