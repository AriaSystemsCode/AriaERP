********************************************************************************
*! Porgram       : SOVAN30.PRG                
*! DESC          : Convert Van Mar VAN300 order report(C#102147)use order confi.
*! Date          : 01/11/2001
*: System        : Aria 27 System 
*: Module        : Sales Order
*: Developer     : Mohamed Shokry (MHM)
*:******************************************************************************
*: Calls Procedures : 
*:******************************************************************************
*: Passed Parameters  : None
*:******************************************************************************
*: Modifications      :
*:B606439,1 Abd 11/27/2002 Print sub total at the FRX only.
*:******************************************************************************

*-- Section to check if there is account value selected or not.
*-- And the date is not empty.
IF EMPTY(lcRpAcc)
  =gfModalGen('QRM00000B38018','F',' ',' ','Account field is empty')   
  RETURN
ENDIF
IF !EMPTY(lcRpAcc)
  lcRpExp = lcRpExp + " .AND. ACCOUNT = lcRpAcc"
ENDIF  
*--- Opening DBF files
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCKLINS','SH')
=gfOpenFile(gcDataDir+'SKUTMPL',gcDataDir+'SKUTMPL','SH')
*--- Variable declaration

lcSKU      = SPACE(16)
lcSiz      = ''

*--- CREATE TEMPRARY FILES
*--- (ORDHTEMP)TEMP FILE TO COLECT SELECTED ORDER
*--- FROM ORDHDR.
ORDHTEMP = gfTempName()

*--- (lcColOrdL)TEMP FILE TO COLECT LINES OF THE SELECTED
*--- ORDER FROM ORDLINE FILE AND STYLE ON THE SAME STYLE
*--- GROUP FROM STYLE FILE.
lcColOrdL  = gfTempName()

*---START MAIN PROGRAM

*--- Function to collect and prepare data for printing
SELECT OrdHdr
COPY ALL TO &gcWorkDir.&OrdHTemp FOR &lcRpExp
=gfOpenFile(gcWorkDir+OrdHTemp,'','EX')

= lfColOrdL()
SELECT(lcColOrdL)
*--- Start printing report
DO gfDispRe WITH EVAL('lcRpName')
*---END MAIN PROGRAM

****************************************************************************
*! PROG : lfColOrdL.PRG   (C#102147)
*! DESC : FUNCTION TO COLLECT ORDER LINES + STYLE/COLOR GROUP.
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
****************************************************************************
*! Called from : SOVAN30.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ....
****************************************************************************
FUNCTION lfColOrdL
PRIVATE lnAlias
lnAlias = SELECT()
CREATE DBF (gcWorkDir+lcColOrdL) ;
                        (STYLE   C(19)  , ORDER C(06),cFlag C(01), ;
                         OTOTQTY N(8,0) , SGROUP  C(06)  , ;
                         SSKU    C(16)  , SQTY_CN N(3,0) , SSTYDESC C(20),;
                         SSIZE   C(50)  , PRICE   N(8,2) , PpTot    N(3,0),Amount N(10,2))

INDEX ON SGROUP+ORDER+SSKU+STYLE TAG(lcColOrdL)

SELECT (ORDHTEMP)
SCAN
  lcOrder = ORDER
  SELECT ORDLINE
  = SEEK('O'+lcOrder)
  SCAN REST WHILE ORDER + STR(LINENO,6) = lcOrder
    lcStyle  = ORDLINE.Style
    = SEEK(lcStyle ,'STYLE')
    *---Function to get SKU#
    = lfPrtSku(lcStyle)
    *---Function to get SIZES
    = lfPrnSiz()
    *IF SEEK('N'+STYLE.CSTYGROUP+'N'+'CSTYGROUP ','CODES')
      lcGroup = STYLE.CSTYGROUP
    *ELSE
    *  lcGroup = ""
    *ENDIF  
    SELECT(lcColOrdL)
    IF !SEEK(lcGroup+ORDLINE.ORDER+PADR(lcSKU,16)+lcStyle)
      APPEND BLANK
      REPLACE STYLE    WITH lcStyle       ;
              ORDER    WITH ORDLINE.ORDER ;
              SGROUP   WITH lcGroup       ;
              SSKU     WITH lcSKU         ;
              SQTY_CN  WITH STYLE.QTY_CTN ;
              cFlag    WITH SPACE(01)     ;
              SSTYDESC WITH STYLE.DESC    ;
              SSIZE    WITH lcSiz         ;
              PpTot    WITH IIF (SEEK('P'+ STYLE.SCALE+STYLE.PrePak,'SCALE'),SCALE.PpTot,0)
     ENDIF
     REPLACE OTOTQTY WITH OTOTQTY + ORDLINE.TOTQTY;
             Amount  WITH Amount + ORDLINE.TOTQTY*ORDLINE.PRICE;
             PRICE   WITH IIF(OTOTQTY=0,0,Amount/OTOTQTY)
  ENDSCAN
  *--- Start selecting style with no order from style file
  SELECT STYLE
ENDSCAN
SELECT (lcColOrdL)
LOCATE 
INDEX ON SGROUP + ORDER + cFlag + SSKU TAG(lcColOrdL)
SET RELATION TO 'O' + ORDER INTO ORDHDR
SELECT (lnAlias)

****************************************************************************
*! PROG : lfPrtSku.PRG   (C#102147)
*! DESC : FUNCTION TO GET SKU.
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
****************************************************************************
*! Called from : SOVAN30.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ....
****************************************************************************
FUNCTION lfPrtSku
PARAMETERS lcStyle
IF !SEEK('S'+&ORDHTEMP..account+lcStyle,'Spck_Lin')
  lcSKU      = SPACE(16)
  RETURN
ENDIF
= SEEK('M'+&ORDHTEMP..account,'Customer')
lcSkuTmpl = IIF(EMPTY(Customer.SkuTmpl),'DEF',Customer.SkuTmpl)
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3+SkuTmpl.Len4
ELSE
  lnDime1 = 16
ENDIF
lcSku = SUBSTR(Spck_Lin.Pack_Id,1,lnDime1)

****************************************************************************
*! PROG : lfPrnSiz.PRG   (C#102147)
*! DESC : FUNCTION TO GET STYLE SIZES.
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
****************************************************************************
*! Called from : SOVAN30.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ....
****************************************************************************
FUNCTION lfPrnSiz
lcSiz = ''
IF SEEK('S'+ STYLE.SCALE,'SCALE')
  FOR lnIndex = 1 TO SCALE.Cnt
    lcIndex = ALLTRIM(STR(lnIndex))
    lcSiz   = lcSiz + ALLTRIM(SCALE.SZ&lcIndex) + IIF(lnIndex <> SCALE.Cnt,'/','')
  ENDFOR
ELSE
  lcSiz = ''
ENDIF
****************************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
*! Purpose   : Function to Get the Description of the Ship Via
*!           : and Terms.
****************************************************************************
*! Called from : FRX
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfCodDes
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ....
****************************************************************************
FUNCTION lfSolSpAdr

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
*--End Of lfSolSpAdr
****************************************************************************
*! Name      : lfSROrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
*! Purpose   : Rise change order flag, in range browse screen.
****************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
****************************************************************************
*! Called from : Option Grid
****************************************************************************
*! Passed Parameters  : None
****************************************************************************
*! Returns            : None
****************************************************************************
*! Example   : =lfSROrder()
****************************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
****************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    SET ORDER TO Customer IN Customer
    SET RELATION TO IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store);
           INTO CUSTOMER && To customer file.
    LOCATE       
  
    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE cOrdType = "O" 
            SET FILTER TO ACCOUNT= lcRpAcc .AND. STATUS $'OHB' 
            LOCATE FOR CORDTYPE+ORDER = "O" 
      CASE cOrdType = "C"
            SET FILTER TO ACCOUNT= lcRpAcc .AND. STATUS $'OHB' 
            LOCATE FOR CORDTYPE+ORDER = "C" 
      CASE cOrdType  = "T" 
            SET FILTER TO ACCOUNT= lcRpAcc .AND. STATUS $'OHB' 
            LOCATE FOR CORDTYPE+ORDER = "T" 
    ENDCASE
    
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    SET FILTER TO 
ENDCASE
*-- end of lfsChOrder.

****************************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
*! Purpose   : When function to get the Old value
****************************************************************************
*! Called from : Some of the Option Grid fields
****************************************************************************
*! Calls       : None
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : None
****************************************************************************
*! Example     : = lfwOldVal()
****************************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*-- end of lfwOldVal.
*!*************************************************************
*! Name      : lfvAcct
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
*! Purpose   : Validate the acount from the option grid.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAcct()
*!*************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field


*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.                  && Browse for all cases not click the botton
  lcRpAcc = lcObjVal
  DO CUSBROWM WITH lcRpAcc
  lcObjVal = lcRpAcc
  llBrowse = .F.
ENDIF
&lcObjNam = lcObjVal

IF lcRpAcc # laOldVal
  llClearOrd = .T.
ELSE
  llClearOrd = .F.
ENDIF

*--End of lfvAcct.
*!*************************************************************
*! Name      : lfvOrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcVar , lcObj , laTemp
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))


IF !EMPTY(lcObj) AND ('?' $ lcObj OR SEEK('O'+lcObj , 'ORDHDR') OR SEEK('C'+lcObj , 'ORDHDR') OR SEEK('T'+lcObj , 'ORDHDR') )

  SELECT ORDHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CTYPE=IIF(cOrdType='C','Contract',IIF(cOrdType='O','Order',IIF(cOrdType='T','EDI Temp. Order','')))"+;
               ":R :H= 'Order Type' :20, "          +;
               "ORDER     :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "

  lcFile_Ttl = "Orders/Contracts"

  lcBrowCond = [FOR ACCOUNT= lcRpAcc .AND. STATUS $'OHB' ]
  
  = gfBrows(lcBrowCond,'ORDER','laTemp')  
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvOrder.
