*:*************************************************************
*: Name      : lfPOSTYPGF
*: Developer : Mariam Mazhar (MMT)
*: Date      : 07/07/2004
*: Purpose   : Call the POSTYGF.FRX form for GMA
*:*************************************************************
*: due to issue #038203
*:*************************************************************

STORE 0 TO lnClrLen , lnClrPos
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
llPoDisp = .F.
=gfOpenFile(oAriaApplication.DataDir+'Codes',oAriaApplication.DataDir+'Codes','SH')

SELECT POSHDR
LOCATE 
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

*-- End of Report.

*!*************************************************************
*! Name        : lfGetNotPd
*! Developer   : Mariam Mazhar (MMT)
*! Date        : 07/07/2004
*! Purpose     : Function to fill the apropriate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : POSTYPGF.FRX [Variable lcGetN in the report]
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : lfBoxPrn,lfNoteHead,lfNoteData
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotPd()
*!*************************************************************
*
FUNCTION lfGetNotPd
PARAMETER lcDummy
llDummy = ''
PRIVATE lnAlias
lcTitle = ''
lcNotes = ''
lnAlias = SELECT(0)
*-- If we are to print both notes.
IF llPrntBoth
  *-- Note that the following Scheme
  *-- ....... cRecord = 'N1' ............. Style Notepad.
  *-- ....... cRecord = 'N2' ............. PO or Contract , or return Notepad.
  DO CASE
    CASE &lcNoteLns..cRecord = 'N1' AND SEEK('F'+STYLE.cStyMajor,;
        'Notepad') .AND. !EMPTY(ALLTRIM(Notepad.MNotes))
      lcTitle = IIF(llPrtSn,'Style Notepad','')
      lcNotes = IIF(llPrtSn,ALLTRIM(Notepad.MNotes),'')
      
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('P' + POSHDR.PO , 'NOTEPAD');
        .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cbusdocu+POSHDR.cStyType='PP','Purchase Order Notepad',;
        IIF(POSHDR.cbusdocu+POSHDR.cStyType='CC','Contract Notepad',;
        IIF(POSHDR.cbusdocu+POSHDR.cStyType='RP','Return Purchase Order Notepad' ,;
        IIF(POSHDR.cbusdocu+POSHDR.cStyType='NN','Inter-Location P/O Notepad',;
        'Dye Order Notepad')))),'')

      lcNotes = IIF(RECNO('POSLN') = lnLstLn, ;
        ALLTRIM(NotePad.mNotes),'')

      llTitle = .F.
  ENDCASE
ELSE && Else You print either Style  or PO/Contract/Ret PO Notepad.
  DO CASE
    CASE llRpPrtSn AND SEEK('F'+STYLE.cStyMajor,'Notepad') .AND. ;
        !EMPTY(ALLTRIM(Notepad.MNotes))
      lcTitle = IIF(llPrtSn,'Style Notepad','')
      lcNotes  =  IIF(llPrtSn,ALLTRIM(Notepad.MNotes),'')

    CASE llRpPrtPn .AND. SEEK('P' + POSHDR.PO , 'NOTEPAD');
        .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))

      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cbusdocu+POSHDR.cStyType='PP','Purchase Order Notepad',;
        IIF(POSHDR.cbusdocu+POSHDR.cStyType='CC','Contract Notepad',;
        IIF(POSHDR.cbusdocu+POSHDR.cStyType='RP','Return Purchase Order Notepad',;
        IIF(POSHDR.cbusdocu+POSHDR.cStyType='NN','Inter-Location P/O Notepad',;
        'Dye Order Notepad')))),'')

      lcNotes  = IIF(RECNO('POSLN') = lnLstLn, ALLTRIM(NotePad.mNotes),'')
      llTitle = .F.
  ENDCASE
ENDIF


lcLstNote = ''
IF !EMPTY(lcNotes)
  lnNotLine = 1 
  lnOldMemW = SET("MEMOWIDTH")

  SET MEMOWIDTH TO 100


  lnMemLins = MEMLINES(lcNotes)
  DIMENSION laNot[lnMemLins]
  DO WHILE lnNotLine <= lnMemLins
    IF  '*' <> LEFT(MLINE(lcNotes,lnNotLine),1)
      laNot[lnNotLine]= MLINE(lcNotes,lnNotLine)
    ELSE
      laNot[lnNotLine] = ''
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO
  FOR i=1 TO   lnMemLins
    IF !EMPTY(laNot[I])
      IF i <> lnMemLins
        lcLstNote = lcLstNote + laNot[i]+ CHR(13)
      ELSE
        lcLstNote = lcLstNote + laNot[i]
      ENDIF
    ENDIF
  ENDFOR
  lcNotes = lcLstNote
ENDIF

llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
* State that we have already printed the style notepad (required to be
* printed once)
llPrtSn    = .F.

SELECT (lnAlias)

RETURN lcDummy


