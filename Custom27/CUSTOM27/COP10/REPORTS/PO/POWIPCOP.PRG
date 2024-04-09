*:***************************************************************************
*: Program file  : POWIPCOP   C121470
*: Program desc. : Custom Import WIP for Copy Cats
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : Mohamed Shokry Mohamed (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POWIPCOP
*:***************************************************************************
*: Modifications  : 
*:C037949,1 BWA 14/04/2004 1- Print the report in letter size.
*:C037949,1                2- Print Sales Rep 2 field in the Confo column.
*:C037949,1                3- Print the complete date from the allocated sales order in the cancel date column.
*:C037949,1                4- Fix bug of printing wrong Units.
*:C037949,1                5- Fix bug of printing wrong In Transit.
*:C037949,1                6- Add another option for print the information from either the insurance field or the buyer field.
*:***************************************************************************
*

=lfWIPFile()
SELECT POSHDR
IF OCCURS(".STATUS",lcRpExp) = 0
  lcRpExp = lcRpExp + " AND POSHDR.STATUS $ 'OH'"
ENDIF  
SCAN FOR &lcRpExp
  =lfGetMem()  && Fill Memory variables with its corresponding values.
ENDSCAN

SELECT (lcTmpFile)
LOCATE

DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfGetMem
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Fill Tempfile memory variables with its corresponding data.
*!*************************************************************
*! Called from : Program code,
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMem()
*!*************************************************************
*
FUNCTION lfGetMem
PRIVATE lnUnits 
STORE 0 TO lnUnits

*-- get Needed data from CutPik File and OrdHdr 
IF SEEK('2'+PosHdr.Po,'CUTPICK') And SEEK('O'+CUTPICK.Order,'ORDHDR')
  m.Account   = OrdHdr.Account

  *C037949,1 BWA 14/04/2004 Print Sales Rep 2 field in the Confo column and 
  *C037949,1                Print the complete date from the allocated sales order in the cancel date column.[START]
  *m.Confo     = CutPick.Order
  *m.cancelled = OrdHdr.cancelled
  m.Confo     = ORDHDR.REP2
  m.cancelled = OrdHdr.Complete
  *C037949,1 BWA 14/04/2004.[END]

  
ELSE
  m.Account   = " "
  m.Confo     = " "
  m.cancelled = {}
ENDIF

*-- get Needed data from PosHdr File and Apvendor
m.cVendCode = PosHdr.Vendor
m.PO        = PosHdr.Po

*C037949,1 BWA 14/04/2004 either the insurance field or the buyer field. or leave it blank based on the new option added.[START]
M.Notes = IIF(lcRpPrNot = "I" , ALLTRIM(POSHDR.INSURANCE) , IIF(lcRpPrNot = "B" , ALLTRIM(POSHDR.BUYER) , SPACE(0)))
*C037949,1 BWA 14/04/2004.[END]

IF SEEK(PosHdr.Vendor,'APVENDOR')
  m.cVenComp = Apvendor.cVenComp
ENDIF

m.complete = PosHdr.complete
m.Period   = IIF(MONTH(PosHdr.complete) > 9,"B"+ALLTRIM(STR(MONTH(PosHdr.complete))),"A"+ALLTRIM(STR(MONTH(PosHdr.complete))))

IF SEEK(PosHdr.cStyType+PosHdr.Po,'POSLN')
  m.Style    =PosLn.Style

  *C037949,1 BWA 14/04/2004 Fix the bug of wrong computing in transit field.[START]
  m.InTransit = 0
  *C037949,1 BWA 14/04/2004.[END]

  *--Posln
  SELECT POSLN
  SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.Po
    DO CASE
       CASE trancd = '1' 

         *C037949,1 BWA 14/04/2004 Fix the bug of wrong computing the units field.[START]
         *lnUnits = TotQty
         lnUnits = lnUnits + TotQty
         *C037949,1 BWA 14/04/2004.[END]

       CASE trancd = '2' 
         *as per Toni's request
         *lnUnits = lnUnits - TotQty
         m.InTransit = m.InTransit + TotQty
       CASE trancd = '3' 
         m.InTransit = m.InTransit + TotQty
         IF !EMPTY(ShipNo) AND SEEK(ShipNo,'SHPMTHDR')
           m.ETA = SHPMTHDR.ETA
         ENDIF
       CASE trancd = '4' 
         lnUnits = lnUnits - TotQty
       CASE trancd = '5' 
         lnUnits = lnUnits - TotQty
    ENDCASE
  ENDSCAN
  m.Units = lnUnits
ENDIF
INSERT INTO (lcTmpFile) FROM MEMVAR
 
*-- end of lfGetMem.
*!*************************************************************
*! Name      : lfvVendor
*! Developer : Mohamed Shokry Mohamed (MHM)
*! Date      : 03/02/2004
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************

FUNCTION lfvVendor

lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)
SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF
&lcVenFld = lcVendor 

*-- end of lfvVendor.
*!*************************************************************
*! Name      : lfWIPFile
*! Developer : Mohamed Shokry Mohamed (MHM)
*! Date      : 03/02/2004
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfWIPFile()
*!*************************************************************
*
FUNCTION lfWIPFile

*C037949,1 BWA 14/04/2004 Add notes field to print either the insurance field or the buyer field
*C037949,1                or leave it blank based on the new option added [START]
*CREATE CURSOR (lcTmpFile) (PO C(6) , Account C(6),cVendCode C(8), cVenComp C(30),;
                           Confo C(6), cancelled D(8), complete D(8), Style C(19), ;
                           Units N(14,2), InTransit N(14,2), ETA D(8), Period C(3))

CREATE CURSOR (lcTmpFile) (PO C(6) , Account C(6),cVendCode C(8), cVenComp C(30),;
                           Confo C(6), cancelled D(8), complete D(8), Style C(19), ;
                           Units N(11), InTransit N(11), ETA D(8), Period C(3) , Notes C(30))
*C037949,1 BWA 14/04/2004.[END]

INDEX ON cVendCode+ALLTRIM(STR(YEAR(Complete)))+Period TAG (lcTmpFile) OF (lcTmpFile)

*-- end of lfWIPFile.
*!*************************************************************
*! Name      : lfGetMonth
*! Developer : Mohamed Shokry Mohamed (MHM)
*! Date      : 03/02/2004
*! Purpose   : Get Month Data
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMonth()
*!*************************************************************
*
FUNCTION lfGetMonth

DO CASE
 CASE Period = 'A1 '
      lcRPMonth = 'JANUARY'
 CASE Period = 'A2 '
      lcRPMonth = 'FEBRUARY'
 CASE Period = 'A3 ' 
      lcRPMonth = 'MARCH'
 CASE Period = 'A4 '
      lcRPMonth = 'APRIL'
 CASE Period = 'A5 '
      lcRPMonth = 'MAY'
 CASE Period = 'A6 '
      lcRPMonth = 'JUNE'
 CASE Period = 'A7 '
      lcRPMonth = 'JULY'
 CASE Period = 'A8 '
      lcRPMonth = 'AUGUST'
 CASE Period = 'A9 '
      lcRPMonth = 'SEPTEMBER'
 CASE Period = 'B10'
      lcRPMonth = 'OCTOBER'
 CASE Period = 'B11'
      lcRPMonth = 'NOVEMBER'
 CASE Period = 'B12'
      lcRPMonth = 'DECEMBER'
ENDCASE