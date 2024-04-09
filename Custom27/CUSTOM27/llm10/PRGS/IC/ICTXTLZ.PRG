*:***************************************************************************
*: Program file  : ICTXTLZ.PRG
*: Program desc. : CUSTOMIZED UPC STYLE INVENTORY.
*: Date          : 10/24/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102491
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICTXTLZ
*:***************************************************************************
*: Modifications :
***************************************************************************

PRIVATE llStyUpc , llStyle
STORE .F. TO llStyUpc , llStyle

lcAriaDir = LEFT(gcWorkDir , ATC("\" ,gcWorkDir , 2))
IF FILE(lcAriaDir + "UPCINVEN" + ".TXT")
  ERASE (lcAriaDir + "UPCINVEN" + ".TXT")
ENDIF
lcOutput = lcAriaDir + "UPCINVEN" + ".TXT"
lnHandle = FCREATE(lcOutput)

IF lnHandle < 0  
  =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot Open Output File.')
  RETURN
ENDIF

=lfOpenFils()
=lfColctDat()
=gfModalGen('INM00000B40011','DIALOG','','','Data have been successfully updated in the Text file.')
=lfClosFile()
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfOpenFils
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/24/2001
*! Purpose   : Function to open the files used in the program.
*!*************************************************************
*! Called from : ICTXTLZ.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOpenFils

IF !USED('STYLEUPC')
  llStyUpc = gfOpenFile(gcDataDir + "STYLEUPC" , "STYLEUPC" , 'SH')
ENDIF

IF !USED('STYLE')
  llStyle = gfOpenFile(gcDataDir + "STYLE" , "STYLE" , 'SH')
ENDIF

*--End of lfOpenFils.
*!*************************************************************
*! Name      : lfColctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/24/2001
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Called from : ICTXTLZ.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT STYLEUPC
SET RELATION TO STYLE INTO STYLE ADDITIVE

SCAN
  IF !STYLEUPC.LUPCPACK
    WAIT WINDOW "Collecting data to the text file..." NOWAIT
    STORE SPACE(0) TO lcString
    lcString = STYLEUPC.CUPCNUM1 + STYLEUPC.CUPCNUM2 + STYLEUPC.CUPCNUM3
    lcSzT  = ALLTRIM(SIZE)
    lcString = lcString + "," + ALLTRIM(STR(STYLE.STK&lcSzT))
    =FPUTS(lnHandle,lcString)
  ENDIF
ENDSCAN
WAIT CLEAR

=FCLOSE(lnHandle)
SET RELATION TO
SELECT(lcAlias)
*--End of lfColctDat.
*!*************************************************************
*! Name      : lfClosFile
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/24/2001
*! Purpose   : Function to close the files used in the program.
*!*************************************************************
*! Called from : ICTXTLZ.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfClosFile

IF llStyUpc
  USE IN STYLEUPC
ENDIF

IF llStyle
  USE IN STYLE
ENDIF

*--End of lfClosFile.