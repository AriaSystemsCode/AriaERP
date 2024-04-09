*:************************************************************************
*: Program file       : ARCODTG.PRG
*: Program description: Print C.O.D. Tags From Invoices
*: Module             : ACCOUNT RECEIVABLE (AR)
*: Developer          : Noha Mohamed(NMM)   
*: Tracking Job Number: 037403
*  Date               : 01/12/2004
*:************************************************************************
*: Calls:
*:      Programs        : .....
*:      Screens         : .....
*:      Functions       : lfPrnLbl, lfvSortBy, lfvInvNo, lfwOldVal, lfInvSet,lfAdrShift
*:      Global Function : gfGetMemVar,gfTempName,gfOpenFile,gfModalGen,gfPhoneTem,
*:                        gfGetAdr,gfRltFld,gfCodDes,gfBrows
*:************************************************************************
*: Called From: 
*:**********************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO ARCODTG
*:************************************************************************

#INCLUDE R:\Aria4xp\reports\ar\arcodtg.H

LOCAL lnResult,lcSelectCommand

*-- Option Grid Layout :
*-- Print       : (W)arehouse / (C)ompany Address default (W)
*-- Enter Range of Date       : from .. To ..
*-- Enter Range of Invoices   : from .. To ..

llUseUpsBx = IIF(ALLTRIM(UPPER(gfGetMemVar('M_UPSBOX')))='Y',.T.,.F.)

IF llUseUpsBx
  =gfOpenFile(oAriaApplication.DataDir+'UpsBox',oAriaApplication.DataDir+'UpsBox','SH')
ENDIF

*-- lcWhComp   : print either warehouse or company address
*-- laCompAdd  : company address
*-- laWareAdd  : warehouse address
*-- laShipTo   : ship to address
*-- laDivLName : division long name
*-- lcDesc     : division code description
*-- HDRL1 : either company or warehouse name to be printed in the label
*-- HDRL2 ,HDRL3 ,HDRL4 : either company or warehouse addersses depend on the user seletion.
*-- lnCodAmt : code amount of the invoice

lcWhComp  = lcRpAddPr   && lcRpAddPr(the address to be printed) defined in option grid
DECLARE laCompAdd[6,1] ,laWareAdd[5,1] ,laDivLName[1,2] ,laShipTo[5,1]
STORE SPACE(0) TO laCompAdd, laWareAdd, laShipTo, lcDivLName, lcDesc,HDRL1,;
                  HDRL2, HDRL3, HDRL4, lcShipr, lcUpsAct
STORE 0 TO lnCodAmt

laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

LBLTEMP = gfTempName()    &&global function gives temporarily name for the workfile.
*WAIT WINDOW 'Selecting records for report ...' NOWAIT
 WAIT WINDOW LANG_ArCodTG_WaitWind1 NOWAIT

*-- If nothing is selected then lcrpexp will be .T.
*-- AND if selected a range of invoices and date range then lcrpexp will be:
*-- " seek (invhdr.invoice,"tempfile") and between (invhdr.invdate,dtos(alltrim(ldate)),dtos(alltrim(hdate)))".
*-- filter condition must be done twice , on range of invoice number (in syrepuvr)
*-- and (for if no selected range at all) lcrpexp.
*-- modifying lcrpexp to include (hidden filter) the payment terms that have
*-- cod_flag = 'Y' and also invoices that of cod_amt <> 0

lcRpExp = lcRpExp + " .AND. (COD_FLAG = 'Y' .AND. COD_AMT <> 0)"

SELECT INVHDR
COPY TO (oAriaApplication.WorkDir+LBLTEMP) FOR &lcRpExp
=gfOpenFile(oAriaApplication.WorkDir+LBLTEMP,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
INDEX ON INVOICE TAG (LBLTEMP)
SELECT (LBLTEMP)
GO TOP

IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SET RELATION TO INVOICE INTO INVHDR,'O'+ORDER INTO ORDHDR,;
                IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE)INTO CUSTOMER
*-- if llUseUpsBx
IF llUseUpsBx
  SET RELATION TO Invoice INTO UpsBox ADDITIVE
ENDIF  && endif llUseUpsBx
*-- warehouse/company addresses.
*-- if user select to print company addresses
IF !(lcWhComp = 'W')
  IF !USED('SYCCOMP')
    = gfOpenFile(oAriaApplication.SysPath +'SYCCOMP',oAriaApplication.SysPath +'Ccomp_id','SH')
  ENDIF
  SELECT SYCCOMP
  *SEEK gcAct_Comp
  lcSelectCommand=[SELECT * FROM SYCCOMP WHERE cComp_id = '] + oAriaApplication.ActiveCompanyID+ [']
  lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
  IF lnResult >= 1 
    lcCompName   = cCom_Name             && Variable to hold the Company Name
    lcCompPhon   = cCom_Phon             && Variable to hold the Company Phone
    lcPhonPict   = gfPhoneTem()          && Variable to hold the Company Phone Format
    laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
    laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
    laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
    =lfAdrShift('laCompAdd')
    HDRL1        = lcCompName
    HDRL2        = SUBSTR(laCompAdd[1],1,26)
    HDRL3        = SUBSTR(laCompAdd[2],1,26)
    HDRL4        = SUBSTR(laCompAdd[3],1,26)
  ENDIF 
ELSE  && else user select to print warehouse addresses
  SELECT WAREHOUS
  =SEEK(INVHDR.cWareCode,'WAREHOUS')
  HDRL1        = cDesc
  laWareAdd[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laWareAdd[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laWareAdd[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laWareAdd[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laWareAdd[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  HDRL2        = SUBSTR(laWareAdd[1],1,26)
  HDRL3        = SUBSTR(laWareAdd[2],1,26)
  HDRL4        = SUBSTR(laWareAdd[3],1,26)
ENDIF  && endif user select to print company addresses

lcShipr  = gfGetMemVar('XUPSFROM')
lcUpsAct = gfGetMemVar('XUPSACCT')
R_WIDTH = 'N'            && report width
XSPCINS = SPACE(25)
*-- Print label sets ,loop on (lbltemp) ,major print loop
SELECT (LBLTEMP)
SCAN WHILE Inkey()  <> 32
  *WAIT WINDOW 'Printing Labels for Invoice No. '+INVOICE NOWAIT
  WAIT WINDOW LANG_ArCodTG_WaitWind2 +INVOICE NOWAIT
  *-- fill the division information from the codes file.
  *-- gfRltFld retrieves division long name form codes for rltfld='N'.
  *-- gfCodDes retrieves division code description from codes for the specified code rltfield.

  =gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
  
  IF !EMPTY(lcDivLName)
    Hdrl1 = lcDivLName
  ELSE
    lcDesc = gfCodDes(INVHDR.cDivision,'cDIVISION')
    IF !EMPTY(lcDesc)
      Hdrl1 = lcDesc
    ENDIF
  ENDIF
  
  XSPCINS = SUBSTR(ALLTRIM(gfCodDes(&LBLTEMP..SPCINST,'SPCINST')),1,25)
  *-- customer shipto address
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
  =lfAdrShift('laShipTo')
*!*	*HMA,we need to preview each invoice label only one time not for each carton(as in case of print)
*!*	*    because the data is the same for each carton per one invoice [BEGIN].
  IF llUseUpsBx 
    IF !EOF('UpsBox')
      SELECT UpsBox
      *SCAN WHILE InvHdr.Invoice = &LBLTEMP..Invoice 
      SCAN WHILE UpsBox.Invoice = &LBLTEMP..Invoice 
        =lfPrnLbl('UpsBox.TotalCod')
      ENDSCAN
    ELSE
      =lfPrnLbl('InvHdr.Cod_Amt')
    ENDIF
  ELSE
    =lfPrnLbl('InvHdr.Cod_Amt')
  ENDIF
ENDSCAN
*!*	    IF !EOF('UpsBox')
*!*	      SELECT UpsBox
*!*	      lcInv = &LBLTEMP..Invoice 
*!*	      SCAN WHILE UpsBox.Invoice = &LBLTEMP..Invoice 
*!*	        IF (lcInv <> lcOldInv)
*!*	           =lfPrnLbl('UpsBox.TotalCod')
*!*	        ENDIF 
*!*	      ENDSCAN
*!*	    ELSE
*!*	      =lfPrnLbl('InvHdr.Cod_Amt')
*!*	    ENDIF
*!*	  ELSE
*!*	    =lfPrnLbl('InvHdr.Cod_Amt')
*!*	  ENDIF
*!*	ENDSCAN
*!*	*HMA,[END].
*B603763,1 BWA 07/31/2000 Fix the bug of When preview the Report print
*B603763,1                on the printer and Preview windows appears empty.[START]
*-DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
*B603763,1 [END]

SET DEVICE TO SCREEN
RETURN
*-- end of report code...

*--------------------- Functions' And Procedures' Section ----------------------*

*!*************************************************************
*! Name     : lfPrnLbl
*! Developer: Noha Mohamed Mostafa
*! Date     : 02/03/2004
*!*************************************************************
*! Purpose  : print label.
*!*************************************************************
*! Parameters  	: lcFieldName
*!*************************************************************

FUNCTION lfPrnLbl

PARAMETER lcFieldName

lnCodAmt = EVAL(lcFieldName)  && To be used in the label
*IF OGVARREAD() = "PBRUN"
IF oAriaApplication.gcDevice="PRINTER"
  LABEL FORM (GCREPHOME+GCACT_APPL+'\ARCODTG.LBX')  NEXT 1 TO PRINT NOCONSOL
ELSE
  LABEL FORM (GCREPHOME+GCACT_APPL+'\ARCODTG.LBX')  NEXT 1 PREVIEW 
*!*	*HMA,we need to preview each invoice label only one time not for each carton(as in case of print)
*!*	*    because the data is the same for each carton per one invoice [BEGIN].
*!*	lcOldInv=lcInv  &&save the old value of invoice
*!*	*HMA,[END].
ENDIF
*B603763,1 [END]

*!*************************************************************
*! Name       : lfvInvNo
*! Developer  : Haytham El_Sheltawi
*! Date       : 01/11/1998
*! Purpose    : Validation function for the Invoice number field
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return     : None
*!*************************************************************

FUNCTION lfvInvNo

PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')

SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*-- If The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
*!*	lcBrFields = "Invoice:R:H='Invoice'," + "Printed =IIF(PrtFlag ='P','Yes','No'):R:H='Printed',"+;
*!*	             "InvDate:R:H='Date'," + "Account:R:H='Account'," + "Order:R:H='Order'," + ;
*!*              "CustPO :R:H='Reference'," + "CUSTOMER.BTName:R:H='Bill to'," + "Rep1:R:H='SalesRep.'," +;
*!*	             "Ship   :R:H='Pieces'," + "ShipAmt:R:H='Merchandise'"
  lcBrFields = "Invoice :R :H= LANG_ArCodTG_Inv  , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= LANG_ArCodTG_Prntd  , " +;
               "InvDate :R :H= LANG_ArCodTG_Date , " +;
               "Account :R :H= LANG_ArCodTG_Acct , " +;
               "Order   :R :H= LANG_ArCodTG_Ord , " +;
               "CustPO  :R :H= LANG_ArCodTG_Ref  , " +;
               "CUSTOMER.BTName :R :H= LANG_ArCodTG_BillTo  , " +;
               "Rep1    :R :H= LANG_ArCodTG_SRep , " +;
               "Ship    :R :H= LANG_ArCodTG_Pieces  , " +;
               "ShipAmt :R :H= LANG_ArCodTG_Merch "
  
  *lcFile_Ttl = 'Receivable invoices'
  lcFile_Ttl = LANG_ArCodTG_FileTitle
  
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && Else
    &lcObjName = laOldVal
  ENDIF    && End of IF
  SET RELATION OFF INTO CUSTOMER
ENDIF    && End of IF

*-- If The INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  *SET ORDER TO IN INVHDR
  SET ORDER TO 0 IN INVHDR
ELSE    && Else
   SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF

*-- If The CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  *SET ORDER TO IN CUSTOMER
  SET ORDER TO 0 IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return     : None
*!*************************************************************

FUNCTION lfwOldVal

laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Haytham El_Sheltawi
*! Date      : 08/19/1998
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfInvSet

PARAMETERS lcParm

IF lcParm = 'S'
  lcInvHdTag = ORDER('INVHDR')
  lcCstmrTag = ORDER('CUSTOMER')
  SET ORDER TO TAG INVHDR IN INVHDR
  SET ORDER TO TAG CUSTOMER IN CUSTOMER
ELSE
  *-- If The INVHDR file did not have an active index
  IF EMPTY(lcInvHdTag)
    *SET ORDER TO IN INVHDR
    SET ORDER TO 0 IN INVHDR
  ELSE    && Else
    SET ORDER TO TAG (lcInvHdTag) IN INVHDR
  ENDIF    && End of IF
  *-- If The CUSTOMER file did not have an active index
  IF EMPTY(lcCstmrTag)
    *SET ORDER TO IN CUSTOMER
    SET ORDER TO 0 IN CUSTOMER
  ELSE    && Else
    SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
  ENDIF    && End of IF
ENDIF

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : IHB
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Parameters : lcArrayNam
*!*************************************************************
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  *-- If The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- For Loop to loop the Address Array

FOR lnCount = 1 TO 5
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
