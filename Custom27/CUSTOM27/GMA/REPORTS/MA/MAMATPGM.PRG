*:***************************************************************************
*: Program file  : MAMATPGM.PRG
*: Program desc. : CUSTOMIZED MATERIAL PO FOR GMA.
*: Date          : 14/06/2004
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: B123666
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAMATPGM
*:***************************************************************************
*: Modifications :
*:C125092,1 BWA 11/18/2004 Add new field in the FRX which is the Status field.
*:B124844,1 BWA 12/1/2004 Fix the bug of printing the long notpad line in 2 lines.
*****************************************************************************






*!*************************************************************
*! Name      : lfGetNotGm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/07/2004
*! Purpose   : Function to Get NotePad.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetNotGm
PARAMETER lcNoteGm
PRIVATE lcAlias , lnNotLine

*B124844,1 BWA 12/1/2004 Increase the memowidth of the line to be 125.[START]
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 125
*B124844,1 BWA 12/1/2004.[END]

lcAlias = SELECT(0)
lnNotLine = 1
 
*--If external call.
IF llExternal
  RETURN ''
ENDIF

STORE '' TO lcTitle,lcNotes

*-- if you are at First position in dummy file [See notes in program header]
IF &lcNoteLns..cRecord = 'N1'

  *-- if user want to print Item notepad and you find notes in notepad file.
  IF llRpPrtMn AND (RECNO('POFLN') = lnFabEnd) AND ;
     !EMPTY(ALLTRIM(NOTEPAD.MNOTES))
            
    lcTitle = 'Item  ' + POFLN.Fabric + ' Notepad'
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    DO WHILE lnNotLine <= lnMemLins
      IF  !('*' == LEFT(MLINE(NOTEPAD.MNOTES,lnNotLine),1))
        lcNotes = lcNotes + IIF( SPACE(0) = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine)) , CHR(32), MLINE(NOTEPAD.MNOTES,lnNotLine)) +;
                            IIF( lnNotLine < lnMemLins, CHR(13) + CHR(10) , "")
      ENDIF
      lnNotLine = lnNotLine + 1
    ENDDO
  ENDIF

ELSE

  *-- if user want to print Material PO notepad and you find notes in notepad file.
  IF llRpPrtPn AND (RECNO('POFLN') = lnMPOEnd) AND ;
     !EMPTY(ALLTRIM(NOTEPAD_A.MNOTES))

    lcTitle = IIF(POFHDR.CMATTYPE = 'P','Purchase Order ',;
              IIF(POFHDR.CMATTYPE = 'R','Return Purchase Order ','Contract ')) +;
              'Notepad'

    lnMemLins = MEMLINES(NOTEPAD_A.MNOTES)
    DO WHILE lnNotLine <= lnMemLins
      IF  !('*' == LEFT(MLINE(NOTEPAD_A.MNOTES,lnNotLine),1))
        lcNotes = lcNotes + IIF( SPACE(0) = ALLTRIM(MLINE(NOTEPAD_A.MNOTES,lnNotLine)) , CHR(32), MLINE(NOTEPAD_A.MNOTES,lnNotLine)) +;
                            IIF( lnNotLine < lnMemLins, CHR(13) + CHR(10) , "")
      ENDIF
      lnNotLine = lnNotLine + 1
    ENDDO
  ENDIF
ENDIF

*B124844,1 BWA 12/1/2004 Return to the default value.[START]
SET MEMOWIDTH TO lnOldMemW
*B124844,1 BWA 12/1/2004.[END]

SELECT(lcAlias)
RETURN ''

*--End of lfGetNotGm.