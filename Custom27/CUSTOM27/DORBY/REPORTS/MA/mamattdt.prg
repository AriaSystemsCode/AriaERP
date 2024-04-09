*:***************************************************************************
*: Program file  : MAMATPDT
*: Program desc. : Trim Purchase Order
*! Date          : 05/17/1999
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (M)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : Cus. # 101484
*:***************************************************************************
*: Example : DO MAMATPDT
*:***************************************************************************
*! MODIFICATIONS :
*!B802437,1 SSH 20/07/99 1- Leave left space
*!B802437,1 SSH          2- Dont Print notPad  line start with '*'
*:***************************************************************************
*THE MAIN PROGRAM WHICH WE CALL THIS PROGRAM FRoM IT IS (MA\MAMATP.PRG)

*-- lcType  && Variable that is hold the name of the legal memo file
*-- lcType = 'LegLbNot'
IF FILE(gcDataDir+lcType+'.MEM')
  RESTORE FROM gcDataDir+lcType+'.MEM' ADDITIVE
ENDIF

*-- lcType1  && Variable that is hold the name of the billing memo file
*-- lcType1 = 'BilInsNot'
IF FILE(gcDataDir+lcType1+'.MEM')
  RESTORE FROM gcDataDir+lcType1+'.MEM' ADDITIVE
ENDIF

*!*************************************************************
*! Name      : lfvLeLbty
*! Developer : Bassem Rafaat (BWA)
*! Date      : 05/17/1999
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
*! Example     : = lfvLeLbty()
*!*************************************************************
FUNCTION lfvLeLbty
PARAMETER lcReturn,lcx
PRIVATE laLeLbty,laBilins
DECLARE laLeLbty[3,2]       && Array to hold the name and length of the variables to be used in the Legal Liability screen
DECLARE laBilins[3,2]       && Array to hold the name and length of the variables to be used in the Billing Instructions screen

DO CASE
  CASE  lcx = "L"
    laLeLbty[1,1] = 'lcRpLeg1'        && 1st. line Variable
    laLeLbty[2,1] = 'lcRpLeg2'        && 2nd. line Variable
    laLeLbty[3,1] = 'lcRpLeg3'        && 3rd. line Variable
    laLeLbty[1,2] = 75                && Line length

    IF FILE(gcDataDir+lcType+'.MEM')
      RESTORE FROM gcDataDir+lcType+'.MEM' ADDITIVE
    ENDIF

    lcReturn= gfOptMsg('laLeLbty','Legal Liability')    && Call Function to write Legal Liability
    
    SET MEMOWIDTH TO 75
    SAVE TO gcDataDir+lcType+'.MEM' ALL LIKE lcRpLeg*
    
  CASE  lcx = "B"
    laBilins[1,1] = 'lcRpBil1'        && 1st. line Variable
    laBilins[2,1] = 'lcRpBil2'        && 1st. line Variable
    laBilins[3,1] = 'lcRpBil3'        && 1st. line Variable
    laBilins[1,2] = 33

    IF FILE(gcDataDir+lcType1+'.MEM')
      RESTORE FROM gcDataDir+lcType1+'.MEM' ADDITIVE
    ENDIF 
    
    lcReturn= gfOptMsg('laBilins','Billing Instruction')    && Call Function to write Billing instruction
    SET MEMOWIDTH TO 33
    SAVE TO gcDataDir+lcType1+'.MEM' ALL LIKE lcRpBil*
ENDCASE

RETURN lcReturn
*------------------------- End -------------------------*
*------------------- Functions section -----------------*

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