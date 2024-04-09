*:**************************************************************************
*: Program file  : FIXSo.PRG
*: Program desc. : Fix Program For GMA
*: System        : Aria 2.7
*: Developer     : MOHAMED SHOKRY (MHM)
*: Date          : 01/14/2004   B121238
*:**************************************************************************
PRIVATE lcPath , lcCurPath , lcInvNo , lcVendCode
lcPath = SET("DEFAULT")

lcCurPath = GETDIR("","Select the company directory")
IF ALLTRIM(lcCurPath) == "" 
  WAIT WINDOW "Invalid diectory! Program will be terminated"  NOWAIT 
  RETURN
ENDIF
SET DEFAULT TO lcCurPath+"\"

USE OrdLine IN 0 ORDER TAG OrdLine
USE InvLine IN 0 ORDER TAG InvLine
USE SPCK_HDR IN 0 ORDER TAG SPCK_HDRVR

lcFilHandl = FCREAT('&lcCurPath.SoFixPrg.txt')

lcMessg = ""
lcOrder = ""
lcInvoice = ""
SELECT OrdLine
SCAN 
  *-- if not in the required date then loop
  WAIT WINDOW "Update Order No"+ OrdLine.Order  NOWAIT 
  llchkUpdat = .F.
  IF SEEK("P"+OrdLine.ACCOUNT+OrdLine.PACK_ID+OrdLine.CPKCOLOR+OrdLine.CPCKSIZE+OrdLine.CPKVERSION,"SPCK_HDR") OR;
     SEEK("P"+"*****"+OrdLine.PACK_ID+OrdLine.CPKCOLOR+OrdLine.CPCKSIZE+OrdLine.CPKVERSION,"SPCK_HDR")    
    IF OrdLine.lrange <> SPCK_HDR.lrange
      REPLACE OrdLine.lrange WITH SPCK_HDR.lrange
      llchkUpdat = .T.
    ENDIF
    
    IF OrdLine.lpckprpiec <> SPCK_HDR.lpckprpiec
      REPLACE OrdLine.lpckprpiec WITH SPCK_HDR.lpckprpiec
      llchkUpdat = .T.
    ENDIF
    IF llchkUpdat AND lcOrder <> ORDER
      lcMessg = "Order No " + OrdLine.Order + " Changed  Line No " + STR(OrdLine.lineno)
      =lfVryRport()
      lcOrder = ORDER
    ENDIF  
  ENDIF  
ENDSCAN

SELECT InvLine
SCAN 
  WAIT WINDOW "Update Invoice No"+ InvLine.Invoice  NOWAIT 
  llchkUpdat = .F.
  *-- if not in the required date then loop
  IF SEEK("P"+InvLine.ACCOUNT+InvLine.PACK_ID+InvLine.CPKCOLOR+InvLine.CPCKSIZE+InvLine.CPKVERSION,"SPCK_HDR") OR;
     SEEK("P"+"*****"+InvLine.PACK_ID+InvLine.CPKCOLOR+InvLine.CPCKSIZE+InvLine.CPKVERSION,"SPCK_HDR")    
    IF InvLine.lrange <> SPCK_HDR.lrange
      REPLACE InvLine.lrange WITH SPCK_HDR.lrange
      llchkUpdat = .T.
    ENDIF
    IF InvLine.lpckprpiec <> SPCK_HDR.lpckprpiec
      REPLACE InvLine.lpckprpiec WITH SPCK_HDR.lpckprpiec
      llchkUpdat = .T.
    ENDIF
    IF llchkUpdat 
      lcMessg = "Invoice No " + InvLine.Invoice + " Changed Line No " 
      lcMessg = lcMessg +  STR(InvLine.lineno)
      =lfVryRport()
    ENDIF  
  ENDIF  
ENDSCAN
=FCLOSE(lcFilHandl)

WAIT CLEAR
*-- Scan loop around ApDist file [End]

IF USED('InvLine')
  USE IN InvLine
ENDIF

IF USED('OrdLine')
  USE IN OrdLine
ENDIF

IF USED('SPCK_HDR')
  USE IN SPCK_HDR 
ENDIF


WAIT WINDOW  'Finished Fixing data.'   nowait


SET DEFAULT TO &lcPath

*!*************************************************************
*! Function  : lfVryRport
*! Developer : Mohamed Shokry
*! Date      : 01/13/2004
*! Purpose   : - Create a low level text file (SoFixPrg.txt). 
*!*************************************************************
FUNCTION lfVryRport

=FPUTS(lcFilHandl,lcMessg)
=FPUTS(lcFilHandl," ")
    

*-- End of lfVryRport()
