*:***************************************************************************
*: Program file  : SOORCNLW.PRG
*: Program desc. : CUSTOMIZED SALES ORDER FOR LUCY WERNICK INC.
*: Date          : 27/02/2006
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C130307
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOORCNLW
*:***************************************************************************
*: Modifications :
*! B610155,1 SAB 11/14/2012 Modify the Order Confirmation to work in Request Builder [T20121019.0003]
*****************************************************************************
PRIVATE lnMainDbf
lnMainDbf = SELECT(0)

*-- Get the note
=lfTemNote()  &&--POSTYPAL IN ENG10

*! B610155,1 SAB 11/14/2012 Modify the Order Confirmation to work in Request Builder [Start]
IF !USED('CODES')
  gfOpenTable('CODES', 'CODES', 'SH')
ENDIF
*! B610155,1 SAB 11/14/2012 Modify the Order Confirmation to work in Request Builder [End]

SELECT CCODE_NO,CRLTD_VLU FROM CODES WHERE CFLD_NAME='CDIVISION' AND CRLTD_NAM='CDIVIMGP' INTO CURSOR CODIMG READWRITE 
SELECT CODIMG
INDEX ON CCODE_NO TAG CODS

SELECT ORDHDR
SET RELATION TO CDIVISION INTO CODIMG ADDITIVE 

SELECT(lnMainDbf)
			************** End of Program **************
*:*************************************************************
*: Name        : lfTemNote
*: Developer   : BASSEM RAFAAT ERNEST (BWA)
*: Date        : 27/02/2006
*: Purpose     : Function to get the declaration notepad.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfTemNote()
*:*************************************************************
FUNCTION lfTemNote
PRIVATE lnOldWidth , lnAlias

lnAlias = SELECT(0)
DIMENSION laTempNote[5]

laTempNote = ''
lnCountr   = 0
SELECT NOTEPAD
lnOldWidth = SET('MEMOWIDTH')
SET MEMOWIDTH TO 132

IF SEEK('T' + ORDHDR.CDIVISION)
  lnMemLins = MEMLINES(NOTEPAD.mNotes)
  FOR lnLoop = 1 To lnMemLins
  
    IF !EMPTY(ALLTRIM(MLINE(NOTEPAD.MNOTES,lnLoop)))
      lnCountr = lnCountr + 1
      laTempNote[lnCountr] = MLINE(NOTEPAD.MNOTES,lnLoop)
      
      *-- Print 5 line only.
      IF lnCountr = 5
        EXIT
      ENDIF
    ENDIF
  ENDFOR
ENDIF

SET MEMOWIDTH TO lnOldWidth
SELECT(lnAlias)

*-- End OF lfTemNote