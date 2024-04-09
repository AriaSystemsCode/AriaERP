*!**********************************************************************************************
*: Program file  : MAMXDYE.PRG
*: Program desc. : Print the dyelot for MFG Orders for MEXX.
*:         Module: Aria Apparel Series.
*:         Date  : 12/24/2001
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*!**********************************************************************************************
*! Refer to (C200257)
*!**********************************************************************************************
*...

*----Initialize needed variables
*--lcTmpRep   && Holds tmp file name.
*--lcContExp  && Holds Contarctor filter expression.
*--lcVoutExp  && Holds Voucher filter expression.
STORE "" TO lcContExp,lcVoutExp

*--- Prepare filter
DO lpPrpFltr

*--- Open files 
DO lpOpenFiles

lnNoOfRec = 0
SELECT mmfgordd
LOCATE FOR &lcRpExp
*--- Collect Data
IF EOF() OR !lfCollect()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO SCREEN
SELECT (lcTmpRep)
LOCATE
lcWareName = ""
IF SEEK(Order,'MMFGORDD') AND SEEK(MMFGORDD.CWARECODE,'WAREHOUS')
  lcWareName = WAREHOUS.CDESC
ENDIF
lcRPForm     = 'MAMXDYE'
lcOgPlatform = 'WINDOW'
DO gfDispRe WITH EVAL('lcRPForm')
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
lnPrnDyeP = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRpDyelot'),1)
laOGObjCnt[lnPrnDyeP] = llDyelot
= lfOGShowGet('lcRpDyelot')

*!*************************************************************
*! Name      : lfvFabBet
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : Showes range screen for fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabBet()
*!*************************************************************

FUNCTION lfvFabBet

*-- Here do the between screen
lcTitle = 'Item'
STORE lcRPFrmFab TO lcFromFab
STORE lcRPToFab TO lcToFab
DO (gcRepHome + gcAct_Appl + '\FabRng.SPR')
lcRPFrmFab = lcFromFab
lcRpToFab  = lcToFab

*!*************************************************************
*! Name      : lfvFabRng
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : Vaildate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabRng()
*!*************************************************************

FUNCTION lfvFabRng

*-- this is the validation of from_item

lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.

IF !EMPTY(lcRngVarVa)
  SELECT FABRIC
  IF !SEEK(lcRngVarVa)
    DO FABROW WITH lcRngVarVa,'*'
    &lcRngVarNa = lcRngVarVa
  ENDIF
ENDIF

SHOW GET &lcRngVarNa

*!*************************************************************
*! Name      : lfvpbFabOk  
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : clear read
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfvpbFabOk()
*!*************************************************************
FUNCTION lfvpbFabOk
*-- this is the validation of to_item
IF !EMPTY(lcFromFab) .OR. !EMPTY(lcToFab)
  IF EMPTY(lcToFab) 
    lcToFab = lcFromFab
  ENDIF
  IF lcFromFab > lcToFab
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFromFab)
  ELSE
    CLEAR READ
  ENDIF
ELSE
  *-- CLEAR READ exits the active READ, 
  *-- and returns control to the previous READ level if the active READ 
  *-- is nested.
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfvVen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
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
*! Name      : lfvOrd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : Validate the piktkt
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOrd()
*!*************************************************************

FUNCTION lfvOrd
PRIVATE lcOrdFld,lcOrder,lnSelFile,lcOrdTag
	
lcOrdFld  = VARREAD()
lcOrder   = EVAL(lcOrdFld)
lnSelFile =  SELECT(0)

SELECT MMFGORDH
lcOrdTag  = ORDER('MMFGORDH')
SET ORDER TO TAG Mmfgordh IN MMFGORDH

IF !EMPTY(lcOrder) .AND. (!SEEK(lcOrder , 'MMFGORDH'))
  DIMENSION laTemp[1]
  laTemp = ''     
  lcBrFields = "CMFGORDNO :R :H= 'Order #' , CFABRIC  :R :H= 'Fabric' , ;
                CWARECODE  :R :H= 'Warehouse' , STATUS   :R :H= 'S'  ,  ;
                ENTERED  :R :H= 'Entered'  ,COMPLETE  :R :H= 'Complete' "
  
   = gfBrows('','MMFGORDH','laTemp')
  IF !EMPTY(laTemp[1])
    lcOrder = laTemp[1]
  ELSE 
    lcOrder = ''
  ENDIF
ENDIF

&lcOrdFld = lcOrder
SET ORDER TO lcOrdTag
SELECT (lnSelFile)
 
*!*************************************************************
*! Name      : lpPrpFltr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : Prepare filter.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrpFltr
*!*************************************************************
PROCEDURE lpPrpFltr

*--Prepare Fabric into filter expression
lcFabFlt = ""
DO CASE
  CASE !EMPTY(lcRPFrmFab) AND !EMPTY(lcRpToFab)
    lcFabFlt = "AND BETWEEN(cFabric,lcRPFrmFab,lcRpToFab)"
  CASE EMPTY(lcRPFrmFab) AND !EMPTY(lcRpToFab)
    lcFabFlt = " AND cFabric <= lcRpToFab"
ENDCASE
IF !EMPTY(lcFabFlt)
  lcRpExp = lcRpExp + lcFabFlt
ENDIF
*--Prepare Fabric into filter expression
lcRpExp = STRTRAN(lcRpExp,'FABRIC.COLOR','COLOR')

*--Prepare Fabric into filter expression
lcRpExp = STRTRAN(lcRpExp,'FABRIC.COLOR','COLOR')

*--Prepare Dyelot into filter expression
IF !EMPTY(lcRpDyelot)
  lcRpExp = lcRpExp + ' AND DYELOT = lcRpDyelot'
ENDIF

*--Prepare Contractor into filter expression
*-Cut the Contractor from the filter.
lcContExp = ""
lnContPos = AT("INLIST(MMFGORDD.CLOK_TIME",lcRpExp)
IF lnContPos > 0
  lnExpEnd  = AT(')',SUBSTR(lcRpExp,lnContPos))
  lcContExp = SUBSTR(lcRpExp,lnContPos,lnExpEnd)
  lcRpExp   = STRTRAN(lcRpExp,lcContExp,'.T.')
ENDIF
lcContExp = STRTRAN(lcContExp,'MMFGORDD.CLOK_TIME','CCONTCODE')

*--Prepare Voucher into filter expression
*-Cut the Voucher from the filter.
lcVoutExp = ""
lnVoutPos = AT("INLIST(FABRIC.COWNER",lcRpExp)
IF lnVoutPos > 0
  lnExpEnd  = AT(')',SUBSTR(lcRpExp,lnVoutPos))
  lcVoutExp = SUBSTR(lcRpExp,lnVoutPos,lnExpEnd)
  lcRpExp   = STRTRAN(lcRpExp,lcVoutExp,'.T.')
ENDIF
*ADLE
*lcVoutExp  = STRTRAN(lcVoutExp,'FABRIC.WIDTH','cVoucherNo')
lcVoutExp  = STRTRAN(lcVoutExp,'FABRIC.COWNER','cVoucherNo+"    "')

*!*************************************************************
*! Name      : lpOpenFiles
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
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

IF !USED('mmfgordd')
  =gfOpenFile(gcDataDir+'mmfgordd',gcDataDir+'mmfgordd','SH')
ENDIF

IF !USED('matinvjl')
  =gfOpenFile(gcDataDir+'matinvjl',gcDataDir+'Mfgopr','SH')
ELSE
  SET ORDER TO Mfgopr IN matinvjl
ENDIF
IF !USED('mmfgordh')
  =gfOpenFile(gcDataDir+'mmfgordh',gcDataDir+'mmfgordh','SH')
ENDIF
IF !USED('mfgoprhd')
  =gfOpenFile(gcDataDir+'mfgoprhd',gcDataDir+'mfgoprhd','SH')
ENDIF
SELECT mmfgordd
SET RELATION TO cmfgordno INTO Mmfgordh ADDITIVE

*!*************************************************************
*! Name      : lfCollect
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
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
  CREATE TABLE (gcWorkDir+lcTmpRep) (Order  C(6) , Status C(1), Contractor C(8),MatCol C(14),;
                                     Dyelot C(10), Budget N(11,3), Received N(11,3), Open N(11,3),;
                                     Cancel N(11,3), VoucherNo C(12))
  *-- Create new main index
  INDEX ON Order+ALLTRIM(MatCol)+dyelot+VoucherNo TAG (lcTmpRep)
ENDIF
*--Collect data
SELECT mmfgordd
llUpdate = .T.
SCAN REST FOR &lcRpExp
  IF lfIsOk()
    DO lpUpodate 
  ENDIF
ENDSCAN
SELECT (lcTmpRep)
LOCATE
RETURN !EOF()

*!*************************************************************
*! Name      : lfIsOk
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : If it ok to get this record?
*!*************************************************************
*! Passed Parameters : llRecollect : Was the filter changed?
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfIsOk()
*!*************************************************************
FUNCTION lfIsOk

llUpdate  = EMPTY(lcContExp) AND EMPTY(lcVoutExp)
llUpdCont = EMPTY(lcContExp)
llUpdVout = EMPTY(lcVoutExp)
IF !llUpdate
  IF !EMPTY(lcContExp)
    *--Check the contractor filter.
    IF SEEK('T'+cMfgOrdNo,'mfgoprhd')
      SELECT mfgoprhd
      LOCATE REST WHILE cimtyp+ctktno+coprcode = 'T'+mmfgordd.cMfgOrdNo;
             FOR &lcContExp
       llUpdCont = FOUND()
    ENDIF
  ENDIF
  IF !EMPTY(lcVoutExp)
    SELECT Matinvjl
    *--Check the Voucher filter.
    IF SEEK(mmfgordd.cMfgOrdNo)
      LOCATE REST WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode =mmfgordd.cMfgOrdNo;
             FOR  (cFabric + cColor = mmfgordd.cFabric+mmfgordd.Color) AND cDyelot = mmfgordd.Dyelot AND &lcVoutExp
             
    ENDIF
    llUpdVout = FOUND()
  ENDIF
  llUpdate = llUpdCont AND llUpdVout
ENDIF
RETURN llUpdate  

*!*************************************************************
*! Name      : lpUpodate
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/24/2001
*! Purpose   : Update Record.
*!*************************************************************
*! Passed Parameters : llRecollect : Was the filter changed?
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpUpodate
*!*************************************************************
PROCEDURE lpUpodate

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcTmpRep)
IF !SEEK(mmfgordd.cMfgOrdNo + ALLTRIM(mmfgordd.cFabric) +"/"+ ALLTRIM(mmfgordd.Color)+mmfgordd.Dyelot)
  lnNoOfRec =   lnNoOfRec + 1
  APPEND BLANK
  REPLACE Order      WITH mmfgordd.cMfgOrdNo           ,;
          Status     WITH mmfgordh.Status              ,;
          Contractor WITH IIF(!EMPTY(lcContExp) OR SEEK('T'+mmfgordd.cMfgOrdNo,'mfgoprhd'),mfgoprhd.cContCode,""),;
          MatCol     WITH ALLTRIM(mmfgordd.cFabric) + "/"+ ALLTRIM(mmfgordd.Color),;
          Dyelot     WITH mmfgordd.Dyelot              ,;
          Budget     WITH mmfgordd.nmfgtotqty          ,;
          Open       WITH mmfgordd.nmfgtotqty          
  IF EMPTY(lcVoutExp)
    SELECT MatInvJl
    =SEEK(mmfgordd.cMfgOrdNo)
    LOCATE REST WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode =mmfgordd.cMfgOrdNo;
           FOR  (cFabric + cColor = mmfgordd.cFabric+mmfgordd.Color) AND cDyelot = mmfgordd.Dyelot
  ENDIF
  REPLACE &lcTmpRep..VoucherNo WITH IIF(EMPTY(lcVoutExp),IIF(FOUND(),MatInvJl.cVoucherNo,""),MatInvJl.cVoucherNo)
ELSE
  DO CASE
    CASE mmfgordd.Trancd = '2'
      REPLACE Received WITH Received + mmfgordd.nmfgtotqty
    CASE mmfgordd.Trancd = '3'
      REPLACE Open     WITH Open     - mmfgordd.nmfgtotqty
    CASE mmfgordd.Trancd = '4'
      REPLACE Cancel   WITH Cancel   + mmfgordd.nmfgtotqty
  ENDCASE  
  REPLACE Open     WITH MAX(Open     - mmfgordd.nmfgtotqty,0)
ENDIF
SELECT (lnAlias)

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


