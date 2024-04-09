*:*************************************************************
*: Name      : lfPOSTYPGF
*: Developer : Waleed Hamed (WLD)
*: Date      : 04/14/2003
*: Purpose   : Call the POSTYGF.FRX form for GMA
*:*************************************************************
*: Modifications : 
*:C200523,4 TMI 05/06/03 Enhance browsing data for the graphic form POSTYGF
*:C200553,1 SSE 05/08/03 Add Color N.R.F. Code and Long Name from Codes.
*:C200553,1              Modifications is in FRX only.
*:C200563,1 SSE 05/28/03 Custom modifications to print header once for 1st page
*:C200563,1              All modifications is in FRX only.
*:B607343,1 SSE 06/12/2003 Fix bug NotePad is not printing across the whole line.
*:B124541,1 NNA 10/21/2004 Fix bug about error while trying to print without selecting any trans.
*:C125092,1 BWA 11/18/2004 Add new field in the FRX which is the Status field.
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
*C200523,4  TMI [Start] Run the report from the temp file that replaced POSHDR file
*DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
=lfBrFrmTmp()
*C200523,4  TMI [End  ] 

*-- End of Report.

*!*************************************************************
*! Name        : lfGetNotPd
*! Developer   : Waleed Hamed  (WLD)
*! Date        : 04/14/2003
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
    lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
      IIF(POSHDR.cStyType='C','Contract Notepad',;
      IIF(POSHDR.cStyType='R','Return Purchase Order Notepad' ,;
      IIF(POSHDR.cStyType='N','Inter-Location P/O Notepad',;
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

    lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
      IIF(POSHDR.cStyType='C','Contract Notepad',;
      IIF(POSHDR.cStyType='R','Return Purchase Order Notepad',;
      IIF(POSHDR.cStyType='N','Inter-Location P/O Notepad',;
      'Dye Order Notepad')))),'')

    lcNotes  = IIF(RECNO('POSLN') = lnLstLn, ALLTRIM(NotePad.mNotes),'')
    llTitle = .F.
  ENDCASE
ENDIF

*Delete '*' form Style and PO notes (commeted lines) 
lcLstNote = ''
IF !EMPTY(lcNotes)
  lnNotLine = 1 
  lnOldMemW = SET("MEMOWIDTH")

  *B607343,1 Increase line width. [Begin]  
  *SET MEMOWIDTH TO 75
  SET MEMOWIDTH TO 100
  *B607343,1 Increase line width. [End]

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


*:**************************************************************************
*:* Name        : lfBrFrmTmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/06/03
*:* Purpose     : Browse data from temp files instead of POSHDR and POSLINE,to overcome slow in 
*                 report display due to huge amount of data in posln
*:***************************************************************************
FUNCTION lfBrFrmTmp
PRIVATE lcHdrReln,lcHdrSkip,lcHdrOrd,lcLnReln,lcLnSkip,lnPos,lcTmpPOhdr
*--Save POSHDR,POSLN relations
SELECT POSHDR
lcHdrReln = SET('RELATION')
lcHdrSkip = SET('SKIP')
lcHdrOrd = ORDER()
SET RELATION TO
lcTmpPOhdr = gfTempName()
COPY STRU TO (gcWorkDir+lcTmpPOhdr) WITH CDX

*-* SELECT POSLN
*-* lcLnReln = SET('RELATION')
*-* lcLnSkip = SET('SKIP')
*-* SET RELATION TO 

*--Make a new copy of POSHDR and POSLN files in a temp file with only selected po's
lnPos = ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,'POSHDR.PO') , 1)
lcSlcted = laOgFxFlt[lnPos,6]

*B124541,1 NNA 10/21/2004 (Begin) Return if user didn't select any transaction
IF !USED(lcSlcted) .OR. RECCOUNT(lcSlcted)=0
  =gfModalGen("TRM000000B00000","DIALOG",'','','You have to select a transaction')
  RETURN
ELSE
*B124541,1 NNA (End)

  SELECT (lcSlcted)
  LOCATE
  SCAN FOR SEEK('P'+&lcSlcted..PO,'POSHDR')
    SELECT POSHDR
    SCATTER MEMVAR MEMO
    INSERT INTO (gcWorkDir+lcTmpPOhdr) FROM MEMVAR
  ENDSCAN
  IF USED(lcTmpPoHdr)
    USE IN (lcTmpPoHdr)
  ENDIF

*B124541,1 NNA (Begin)
ENDIF
*B124541,1 NNA (End)

*--Open the temp file with the same alias of POSHDR
USE (gcWorkDir+lcTmpPOhdr) AGAIN IN POSHDR ALIAS POSHDR

SELECT POSHDR
SET RELATION TO &lcHdrReln
SET SKIP TO &lcHdrSkip
SET ORDER TO TAG &lcHdrOrd
LOCATE

*--Run the report
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

*--Reopen the original POSHDR file
USE (gcDataDir+'POSHDR') IN POSHDR ORDER TAG (lcHdrOrd)
SET RELATION TO &lcHdrReln
SET SKIP TO &lcHdrSkip

*--Erase the temp file
ERASE (gcWorkDir+lcTmpPOhdr+'.DBF')
ERASE (gcWorkDir+lcTmpPOhdr+'.CDX')
ERASE (gcWorkDir+lcTmpPOhdr+'.FTP')
*-- end of lfBrFrmTmp.