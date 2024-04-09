*!**********************************************************************************************
*: Program file  : MARECQTY.PRG
*: Program desc. : Print Issued/Received for MEXX.
*:         Module: Aria Apparel Series.
*:         Date  : 12/31/2001
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*!**********************************************************************************************
*! Refer to (C200256)
*!**********************************************************************************************
*...

*----Initialize needed variables
*--lcTmpRep   && Holds tmp file name.
lcRpExp   = STRTRAN(lcRpExp,"POFLN.CAPINVNO",'cVoucherNo')
lcRpExp   = STRTRAN(lcRpExp,'POFLN.DATE','DTRANDATE')
lcRpExp   = STRTRAN(lcRpExp,'POFHDR.CWARECODE','CWARECODE')
llFromOg  = .T.
STORE " " TO lcGroup,lcGroupH,lcWarCode
lnPrgnO = 0
*--- Open files 
DO lpOpenFiles
SELECT MATINVJL
LOCATE FOR &lcRpExp
lcWarCode = cWareCode
*--- Collect Data
IF EOF() OR !lfCollect()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO SCREEN
IF lcRpSort  = 'R'
  lcFormName    = 'MARECQT'
  =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
ELSE
  lcFormName     = 'MARECQTY'
  =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
ENDIF

SELECT (lcTmpRep)
LOCATE
lcWareName = ""
IF SEEK(lcWarCode,'WAREHOUS')
  lcWareName = WAREHOUS.CDESC
ENDIF
lcOgPlatform = 'WINDOW'
llFromCost   = .F.
lnNoOfCop    = 1
lcTranTit    = "TRAN#"
lcTrans      = "ORDER"
lcTotQty     = "TOTQTY"
lcRecDate    = "DATE"
lcVenCode    = "VENDOR"
lcVoucNo     = IIF(AT("cVoucherNo",lcRpExp)<>0,VoucherNo,"**Multi*")  
DO gfDispRe WITH EVAL('lcFormName')

*!*************************************************************
*! Name      : lfvVen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/31/2001
*! Purpose   : Validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVen()
*!*************************************************************

FUNCTION lfvVen
PRIVATE lcVenFld,lcVendor,lcSelFile,lcApvenTag

lcVenFld  = VARREAD()
lcVendor  = EVAL(lcVenFld) 

lnSelFile = SELECT(0)
IF !USED('APVENDOR')
  =gfOpenFile(gcDataDir+"APVENDOR", "Vencode", "SH")
ENDIF
SELECT APVENDOR
lcApvenTag  = ORDER('APVENDOR')

SET ORDER TO TAG VENCODE IN APVENDOR

IF !EMPTY(lcVendor) .AND.('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF
  
&lcVenFld = lcVendor

SET ORDER TO  lcApvenTag
 
SELECT (lnSelFile)

*!*************************************************************
*! Name      : lpOpenFiles
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/31/2001
*! Purpose   : Open needed files.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOpenFiles
*!*************************************************************
PROCEDURE lpOpenFiles

IF !USED('WareHous')
  =gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
ENDIF
IF !USED('POFHDR')
  =gfOpenFile(gcDataDir+"POFHDR", "POFHDR", "SH")
ENDIF
IF !USED('mmfgordd')
  =gfOpenFile(gcDataDir+'mmfgordd',gcDataDir+'mmfgordd','SH')
ENDIF
IF !USED('APVENDOR')
  =gfOpenFile(gcDataDir+"APVENDOR", "Vencode", "SH")
ENDIF
IF !USED('matinvjl')
  =gfOpenFile(gcDataDir+'matinvjl',gcDataDir+'Mfgopr','SH')
ENDIF
IF !USED('mmfgordh')
  =gfOpenFile(gcDataDir+'mmfgordh',gcDataDir+'mmfgordh','SH')
ENDIF
IF !USED('mfgoprhd')
  =gfOpenFile(gcDataDir+'mfgoprhd',gcDataDir+'mfgoprhd','SH')
ENDIF

*!*************************************************************
*! Name      : lfCollect
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/31/2001
*! Purpose   : Collect Data.
*!*************************************************************
*! Passed Parameters : llRecollect : Was the filter changed?
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lpCollect()
*!*************************************************************
PROCEDURE lfCollect

IF USED(lcTmpRep)
  SELECT (lcTmpRep)
  ZAP
ELSE
  *--Create report temp.
  CREATE TABLE (gcWorkDir+lcTmpRep) (VoucherNo C(12),Order  C(6) , VENDOR C(8),CFABRIC C(7),;
                                     COLOR C(6),DATE D ,REFerence C(20),;
                                     Dyelot C(10), TOTQTY N(8,3), PRICE N(8,3), AMOUNT N(11,3))
  *-- Create new main index
  INDEX ON Order      TAG (lcTmpRep)
  INDEX ON VENDOR     TAG VENDOR
  INDEX ON DTOC(DATE) TAG DATE
  INDEX ON VoucherNo  TAG VOUCHER
ENDIF
=lfSort()
*--Collect data
SELECT MATINVJL
SCAN FOR &lcRpExp AND !EMPTY(CVOUCHERNO)
  *wab
  *lnTotQty = IIF(INLIST(ctrantype,'1','4') AND !EMPTY(cIsession),NISSUED,NRECEIVED)
  lnTotQty = ABS(NRECEIVED - NISSUED)
  *wab
  lnPrice  = IIF(ctrantype='1',NUNITCOST,NUNITCOST)
  lcVenDor = lfVENCODE()
  INSERT INTO (lcTmpRep) (VoucherNo,Order,Vendor,cFabric,Color,Date,Reference,Dyelot,TotQty,Price,Amount);
               VALUES    (MATINVJL.CVoucherNo,MATINVJL.CTRAN,lcVenDor,MATINVJL.CFabric,MATINVJL.Ccolor,;
               MATINVJL.DTRANDATE,MATINVJL.Reference,;
               MATINVJL.cDyelot,lnTotQty,lnPrice,lnPrice*lnTotQty)
  lnPrgnO =  lnPrgnO + 1
ENDSCAN
SELECT (lcTmpRep)
LOCATE
RETURN !EOF()

*!*************************************************************
*! Name      : lfSort
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/31/2001
*! Purpose   : Sort reprot.
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfSort

IF !USED(lcTmpRep)
  RETURN
ENDIF

DO CASE
  CASE lcRpSort  = 'T'
    SET ORDER TO (lcTmpRep) IN (lcTmpRep)    
    lcGroup   = "ORDER"
    lcGroupH  = "Tran#"
  CASE lcRpSort  = 'D'
    SET ORDER TO DATE IN (lcTmpRep)    
    lcGroup   = "DATE"
    lcGroupH  = "Date"
  CASE lcRpSort  = 'V'
    SET ORDER TO VENDOR IN (lcTmpRep)  
    lcGroup   = "VENDOR"
    lcGroupH  = "Vendor"
  OTHER
    SET ORDER TO VOUCHER IN (lcTmpRep)  
    lcGroup   = "VOUCHERNO"
    lcGroupH  = "Voucher#"
ENDCASE  

*!**************************************************************************
*! Name      : lfVENCODE
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Get contactor
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200256
FUNCTION lfVENCODE

PRIVATE lnAlias
lnAlias = SELECT()
STORE "" TO LCVENDOR
IF EMPTY(MATINVJL.cimtyp)
  IF SEEK('P'+MATINVJL.CTRAN,'POFHDR') OR SEEK('R'+MATINVJL.CTRAN,'POFHDR')
    LCVENDOR = POFHDR.VENDOR
  ENDIF
  RETURN LCVENDOR
ENDIF
SELECT Mfgoprhd
IF SEEK('T'+MATINVJL.CTRAN) OR SEEK('M'+MATINVJL.CTRAN)
  LCVENDOR = Mfgoprhd.cContCode
ENDIF
SELECT (lnAlias)
RETURN LCVENDOR


*!*************************************************************
*! Name      : lfvWareCod
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Validation function for the Warehouse Code field
*!*************************************************************
*! Called from : Warehouse Code field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvWareCod

PRIVATE lcObjName , lcObjVal

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

