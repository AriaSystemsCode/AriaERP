*:***************************************************************************
*: Program file  : FIXFISY.PRG
*: Program desc. : CUSTOMIZED FIX PROGRAM TO FIX THE FISCAL YEAR.
*! Date          : 07/25/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: B#604739 {OPEN ISSUE #101060}
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO FIXFISY
*:***************************************************************************

*--Parameter to check the right directory
PARAMETERS lcDataDir
PRIVATE lcDataDir

IF TYPE('lcDataDir') # 'C' OR EMPTY(lcDataDir)
  lcDataDir = ""
  lcDataDir = GETDIR('','Select data directory')
ENDIF

IF EMPTY(lcDataDir) OR !FILE(lcDataDir+'ARCUSHST.DBF') OR !FILE(lcDataDir+'ICSTYHST.DBF') OR !FILE(lcDataDir+'APVENHST.DBF')
  WAIT WINDOW 'Wrong company data directory!' TIMEOUT 3
  RETURN
ENDIF

           *--Section of Initializing the variables --*
*--llArcsthst >> ARCUSHST.DBF
*--llCustomr  >> CUSTOMER.DBF
*--llFishd    >> FISHD.DBF
*--llStyle    >> STYLE.DBF
*--llIcstyhst >> ICSTYHST.DBF
*--llApvendor >> APVENDOR.DBF
*--llApvenhst >> APVENHST.DBF

STORE .F. TO llArcsthst , llCustomr , llFishd , llStyle , llIcstyhst , llApvendor , llApvenhst
STORE SPACE(0) TO lcArcsthst , lcCustomr , lcFishd , lcStyle , lcIcstyhst , lcApvendor , lcApvenhst

=lfOpenFils()
=lfColctDat()
=lfClosFils()
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfOpenFils
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/25/2001
*! Purpose   : Open the files used in the program.
*!*************************************************************
*! Called from : FIXFISY.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfOpenFils

IF !USED('FISHD')
  llFishd = .T.
  USE (lcDataDir+'FISHD.DBF') IN 0
  SELECT FISHD
  SET ORDER TO TAG COMPFYEAR
ELSE
  SELECT FISHD
  lcFishd = ORDER()
  SET ORDER TO TAG COMPFYEAR
ENDIF

IF !USED('CUSTOMER')
  llCustomr = .T.
  USE (lcDataDir+'CUSTOMER.DBF') IN 0
  SELECT CUSTOMER
  SET ORDER TO TAG CUSTOMER
ELSE
  SELECT CUSTOMER
  lcCustomr = ORDER()
  SET ORDER TO TAG CUSTOMER
ENDIF

IF !USED('ARCUSHST')
  llArcsthst = .T.
  USE (lcDataDir+'ARCUSHST.DBF') IN 0
  SELECT ARCUSHST
  SET ORDER TO TAG ACTHST
ELSE
  SELECT ARCUSHST
  lcArcsthst = ORDER()
  SET ORDER TO TAG ACTHST
ENDIF

IF !USED('STYLE')
  llStyle = .T.
  USE (lcDataDir+'STYLE.DBF') IN 0
  SELECT STYLE
  SET ORDER TO TAG STYLE
ELSE
  SELECT STYLE
  lcStyle = ORDER()
  SET ORDER TO TAG STYLE
ENDIF

IF !USED('ICSTYHST')
  llIcstyhst = .T.
  USE (lcDataDir+'ICSTYHST.DBF') IN 0
  SELECT ICSTYHST
  SET ORDER TO TAG STYHST
ELSE
  SELECT ICSTYHST
  lcIcstyhst = ORDER()
  SET ORDER TO TAG STYHST
ENDIF

IF !USED('APVENDOR')
  llApvendor = .T.
  USE (lcDataDir+'APVENDOR.DBF') IN 0
  SELECT APVENDOR
  SET ORDER TO TAG VENCODE
ELSE
  SELECT APVENDOR
  lcApvendor = ORDER()
  SET ORDER TO TAG VENCODE
ENDIF

IF !USED('APVENHST')
  llApvenhst = .T.
  USE (lcDataDir+'APVENHST.DBF') IN 0
  SELECT APVENHST
  SET ORDER TO TAG VENDYEAR
ELSE
  SELECT APVENHST
  lcApvenhst = ORDER()
  SET ORDER TO TAG VENDYEAR
ENDIF

*--End of lfOpenFils.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the date from the credit and 
*!           : the debit file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias

lcAlias = SELECT(0)
SELECT FISHD
LOCATE
SCAN FOR cfisystat = "C"
  lcCrntYar = VAL(FISHD.cfisfyear)
  EXIT
ENDSCAN

*--Case the customer and Arcushst
SELECT CUSTOMER
=SEEK('M')
SCAN REST WHILE TYPE + ACCOUNT + STORE = "M"
  WAIT WINDOW 'Update the Customer history file' NOWAIT
  SELECT ARCUSHST
  FOR I = -1 TO 1
    IF !SEEK(CUSTOMER.ACCOUNT + STR(lcCrntYar+I,4,2) , 'ARCUSHST')
      APPEND BLANK
      REPLACE ACCOUNT   WITH CUSTOMER.ACCOUNT ,;
              CFISFYEAR WITH STR(lcCrntYar+I,4,2)
    ENDIF
  ENDFOR
ENDSCAN

*--Case the Style and Icstyhst
SELECT STYLE
SCAN
  WAIT WINDOW 'Update the Style history file' NOWAIT
  SELECT ICSTYHST
  FOR I = -1 TO 1
    IF !SEEK(STYLE.STYLE + STR(lcCrntYar+I,4,2) , 'ICSTYHST')
      APPEND BLANK
      REPLACE STYLE     WITH STYLE.STYLE ,;
              CFISFYEAR WITH STR(lcCrntYar+I,4,2)
    ENDIF
  ENDFOR
ENDSCAN

*--Case the Apvendor and Apvenhst
SELECT APVENDOR
SCAN
  WAIT WINDOW 'Update the Vendor history file' NOWAIT
  SELECT APVENHST
  FOR I = -1 TO 1
    IF !SEEK(APVENDOR.CVENDCODE + STR(lcCrntYar+I,4,2) , 'APVENHST')
      APPEND BLANK
      REPLACE CVENDCODE WITH APVENDOR.CVENDCODE ,;
              CFISFYEAR WITH STR(lcCrntYar+I,4,2)
    ENDIF
  ENDFOR
ENDSCAN

SELECT(lcAlias)
*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfClosFils
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/25/2001
*! Purpose   : Close the files used in the program.
*!*************************************************************
*! Called from : FIXFISY.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfClosFils

IF llFishd
  USE IN FISHD
ELSE
  SELECT FISHD
  SET ORDER TO &lcFishd
ENDIF

IF llCustomr
  USE IN CUSTOMER
ELSE
  SELECT CUSTOMER
  SET ORDER TO &lcCustomr
ENDIF

IF llArcsthst
  USE IN ARCUSHST
ELSE
  SELECT ARCUSHST
  SET ORDER TO &lcArcsthst
ENDIF

IF llStyle
  USE IN STYLE
ELSE
  SELECT STYLE
  SET ORDER TO &lcStyle
ENDIF

IF llIcstyhst
  USE IN ICSTYHST
ELSE
  SELECT ICSTYHST
  SET ORDER TO &lcIcstyhst
ENDIF

IF llApvendor
  USE IN APVENDOR
ELSE
  SELECT APVENDOR
  SET ORDER TO &lcApvendor
ENDIF

IF llApvenhst
  USE IN APVENHST
ELSE
  SELECT APVENHST
  SET ORDER TO &lcApvenhst
ENDIF
*--End of lfClosFils.