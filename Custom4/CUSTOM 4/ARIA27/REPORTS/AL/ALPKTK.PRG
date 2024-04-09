*:************************************************************************
*:
*: Procedure file: ALPKTK.PRG
*:
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: Haytham el_Sheltawi
*:      Copyright (c) 
*:  Last modified: 11/02/1997
*:
*:  Procs & Fncts: lfwRepWhen
*:               : lfvOptMsg
*:               : lfwOldVal
*:               : lfvPikTkt
*:               : lfvcustom
*:               : lfvWareCod
*:               : lfGTmpOrdL
*:               : lfAFields
*:               : lfSolSpAdr
*:               : lfAdrShift
*:               : lfEndGroup
*:
*:  Documented    11/02/1997
*:************************************************************************
*E300744,1 This program was coded by HS for the Enhancement
*:***************************************************************************
*: Modifications :
*:B602491,1 MAB Mohamed Badran Pick Ticket Date is defaulted by current date
*:B602881,1 ADEL 09/05/1999 Set device to screen in the function using the thermometer
*:B602881,1                 to show the thermometer title and meter on the screen instead
*:B602881,1                 of sending them to the printer.       
*:C101524,1 ADEL 05/13/1999 Call the optional program function to be used in calling
*:C101524,1                 any external program doing something not found in this program.
*:B802330,1 BWA  06/15/1999 Modify variable hold the phone format to use it
*:B802330,1                 in the forms befor shifting the array.
*:B802479,1 HDM  08/02/1999 - Remove phone # from company address array
*:                          - Add Sold to & ship to Phone#
*:B602916,1 RENEE 08/11/1999 Invalid path or file name when creating
*:   					     indeces for cursors
*:B802265,1 BWA 08/16/1999   In Picking Ticket form, selection grid, "Pick Ticket#" In List needs
*:                           to be changed : "Select Range". Currently, it takes a lot of time to 
*:                           select one by one. [The fix of the bug in the surepuvr records]
*:B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
*:B802711,1 RENEE 11/09/99 Save/restore page length so as to be able to 
*:B802711,1                change it in custom forms
*:B603295,1 RAMY  11/22/99 Fix the bug of defaulting the transaction date 
*:                         with the current system date
*:                         Note : changed also in the file 'SYREPUVR'
*:B603301,1 BWA 11/28/1999 Fix the bug of not printing a complete pick tickets.
*:B603313,1 ABD 12/02/1999 Fix the bug If you have picktkt on multi store sales order,
*:B603313,1                And each store has different ship via it Print  word 'ALL'.
*:B802931,1 SSE 01/16/2000 Printing in FRX (Total pieces to pick,Actual Pick) on a second 
*:B802931,1                page which is waste of paper (Modifications in FRX only)
*:B803256,1 BWA 06/11/2000 Fix the bug of not preview the piktkt due to the field of the 
*:B803256,1                PRTFLAG update anyway even if teh user doesn't select the piktkt to print.
*:B802619,1 WMA 07/03/2000 Add company fax number. (Form A)
*:B803317,1 HBG 07/31/2000 Add 'custpo' field to temp order line file
*:C101854,1 MAB 09/05/2000 Call any user defined when if a form contains.
*:B803626,1 HBG 10/09/2000 Fixing bug of not printing store #
*:B803749,1 AAN 10/30/2000 Fixing bug of not Printing Custpo correctly when be "at Multi PO"
*:B803867,1 AAN 01/03/2001 Remove the added notes line if there's no order notes
*:B604206,1 AME 02/15/2001 In Option grid change the option title 
*:B604206,1 AME            (print styl location) to be (Print Style Bins)
*:C102022,1 AAN 02/21/2001 Fixing bug that report printing hold orders whatever selecting it or not.
*:C102332,1 ABD 05/14/2001 Add relation bettwen ordhdr & ordline if we Print
*:C102332,1 ABD            Picking Ticket form for (Cathy Danial) .
*:B804273,1 IAA Add custom function to refresh option grid.
*:C200202,1 mhm 07/18/2001 Add new option for MBI (England) To SORT BY (STYLE/LINE NO)
*:C200219,1 HBG 06/09/2001 Add option for MBI (England) To Print By Picking Ticket Number or Account Code 
*:B604943,1 BWA 10/23/2001 Fix the bug of Cancelled Pick tickts doesn't appear in pick ticket list.[Fix in syrepuvr records]
*:B804457,1 HBG 10/24/2001 Get the currency symbol for the currency of the order and useit in the FRX
*:C200263,1 BWA 02/06/2002 initial variable to check if print the report from the main or from the optional program.
*:B605554,1 BWA 02/14/2002 Add 'Pack_id' field to temp order line file
*:B605689,1 BWA 03/18/2002 Fix the bug of the screen get trancated after printing.
*:B604299,1 BWA 04/02/2002 Add "Complete" to the status filter and browse the piktkt due to the status.
*:B606156,1 BWA 07/03/2002 Remove function lfclear used by CAT10.
*:C102665,1 BWA 07/18/2002 Add variables to hold the length and postion of color and style.
*:B607323,1 ASH 06/03/2003 
*:B130165,1 NNA 12/14/2005 Fix bug that when select to show printing or not printing piktkts it shows all piktkts
*:B130165,1 NNA            this bug fixed in the Syrepuvr file record (Picking Ticket )
*:B607931,1 TMI 01/14/2007 add the sort by style to the standard , suppress this option if a custom form is used
*:C200848,1 TMI 09/09/2007 Update the date for the first time the piktkt will be printed ( this was made as standard ) ( ticket T20070615.0001)
*:B608388,1 NNA 12/27/2007 Fix bug that P/T forms show values in base currency even order is in Foreign Currency
*****************************************************************************
*B802711,1 Declare a private variable to Store page length setting
*PRIVATE lnCount
PRIVATE lnCount,lnOldpLen 
*B802711,1 end

lcCurChr = ALLTRIM(SET('CURRENCY' , 1))        && Variable to hold the currency symbol
llCurLeft = (SET('CURRENCY') = 'LEFT')         && Flag to know if the currency symbol is left
lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
lcShipVia = ''         && Variable to hold the Ship Via Description
lcSeason = ''          && Variable to hold the Season Description
lcSpcInst = ''         && Variable to hold the Special Instructions Description
lcTerms = ''           && Variable to hold the Terms Description
lcDivLName = ''        && Variable to hold the Division long name
llEndGroup = .F.       && Flag to know if we are at the end of the Group

llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo

*C200263,1 BWA 02/06/2002 initial variable to check if print the report 
*                         from the main or from the optional prorgram.[START]
llAlpktk = .T.            && llAlpktk it will be a commen variable.
*C200263,1 [END]

*C102665,1 BWA 07/18/2002 Add variables to hold the length and postion of color and style.[START]
                  *--Section to get the style and color length --*
*--THE COLOR LENGTH
STORE 0 TO lnClrLnM1 , lnClrPosM1
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnM1  = LEN(laItemSeg[lnCount,3])
    lnClrPosM1 = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenthM1
lnLenthM1 = LEN(gfItemMask('PM'))

               *--End Section to get the Style/Color Lengths --*
*C102665,1 BWA 07/18/2002.[END]

lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)
llPrntComp = TYPE('llPrntComp') = 'L' .AND. llPrntComp 

*--HDM B802479,1 [Start] Remove phone # from company address array
*DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
*--HDM B802479,1 [End]

laCompAdd = ''          && Array to hold the Company address
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

lcTime = TIME()          && Variable to hold the Time


SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
*B802619,1 WMA [Begin]
lcCompFax = cCom_Fax               && Variable to hold the Company Fax
*B802619,1 WMA [End]
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
*B802619,1 WMA [Begin]
lcCompFax = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
*B802619,1 WMA [End]
*--HDM B802479,1 [Start] Remove phone # from company address array
*laCompAdd[6] = 'Phone# : '+ TRANSFORM(lcCompPhon , lcPhonPict)
*--HDM B802479,1[End]
*B802330,1 BWA 06/15/99
lcXphone = TRANSFORM(lcCompPhon , lcPhonPict) && variable hold the phone format to use it
                          && in the forms befor shifting the array.
*B802330,1 BWA 06/15/99

=lfAdrShift('laCompAdd')

=lfGTmpOrdL()

SELECT (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH

SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

*IF We are to Print Order Lines Note Pad
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF    && End of IF

SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
*B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
*B603117,1 [END]
SELECT (lcTmpOrdL)
GO TOP
*C101524,1 (Begin)  Add the optional program function to be used in getting anything special not found in this program(alpktk).
=lfOptProg()
*C101524,1 (End)

*B607931,1 TMI [Start] add the sort by style to the standard , suppress this option if a custom form is used
llSuppress = LEN(lcFormName)=8 .OR. RIGHT(lcFormName,1)$'BE'
IF !llSuppress .AND. lcRpSortBy = 'S'
  lcKey = STRTRAN(KEY(),'STR(LINENO,6)','STYLE+STR(LINENO,6)')
  INDEX ON &lcKey TAG ('Z'+SUBSTR(lcTmpOrdL,2))
ENDIF
*B607931,1 TMI [End  ] 

*-- HDM Added for Running @SAY[start] ==>KOFTA
IF llIsAparel
  *B802711,1 Store page length setting
  lnOldpLen = _pLength 
  *B802711,1 end
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT

    *B605689,1 BWA 03/18/2002 Fix the bug of the screen get trancated after printing.[START]
    SET DEVICE TO SCREEN
    *B605689,1 BWA 03/18/2002.[END]

  ENDIF
  *B802711,1 restore page length setting
  _pLength  = lnOldpLen 
  *B802711,1 end
ELSE
  
  *C200263,1 BWA 02/06/2002 initial variable to check if print the report from the main or from the optional program.[START]
  *DO gfDispRe WITH EVAL('lcFormName')
  IF llAlpktk
    DO gfDispRe WITH EVAL('lcFormName')
  ENDIF
  *C200263,1 BWA 02/06/2002.[END]
  *DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
ENDIF  

*C200848,1 TMI [Start] Update the date for the first time the piktkt will be printed
=lfUpdPrtDt()
*C200848,1 TMI [End  ] 

*-- HDM Added for Running @SAY[end]

SELECT PIKTKT
SET RELATION TO

*B803256,1 BWA Check if the user select to print the piktkt or not to update the prtflag field.[START]
IF gcDevice <> 'SCREEN'
  SELECT (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdU)
  SCAN
    IF SEEK(EVAL(lcTmpOrdL+'.PIKTKT'),"PIKTKT")
      SELECT PIKTKT
      REPLACE PIKTKT.PrtFlag WITH 'P'
    ENDIF  
  ENDSCAN
ENDIF    && End of IF
*B803256,1 [END]

*IF The Temp. Order Line file is Opened in one of the work areas
IF USED(lcTmpOrdL)
  SELECT (lcTmpOrdL)
  SET RELATION TO
  USE
ENDIF    && End of IF

*IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  SELECT (lcTmpOrdH)
  SET RELATION TO
  USE
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfADel() , lfAFields() , gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lnWareHElm , lnVrFltElm , llRefresh
*B603295,1 RAMY Mark these lines for not defaulting the date with the 
*B603295,1 RAMY current system date [start]

*B602491,1 MAB Pick Ticket Date is defaulted by current date [Begin
*IF lnOGSeting = 1  && if it's default setting.
*  lnDatePos = ASCAN(laOGVrFlt,'PIKTKT.DATE')  && Date position
*  IF lnDatePos > 0
*    lnDatePos = ASUBSCRIPT(laOGVrFlt,lnDatePos,1)
*    IF 'BETWEEN' $ UPPER(laOGVrFlt[lnDatePos,5])
*      laOGVrFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
*    ELSE
*      laOGVrFlt[lnDatePos,6] = DTOC(gdSysDate)
*    ENDIF  
*  ENDIF
*ENDIF             && end if it's default setting.
*B602491,1 MAB Pick Ticket Date is defaulted by current date [Begin

*B603295,1 RAMY  [end]
*IF The multi Warehouse is No
IF !llMultWare
  
  lnWareHElm = ASCAN(laOGFieldN , 'PIKTKT.CWARECODE')      && Variable to hold the number of the array elment for the Warehouse in the Option grid Available for felter array
  
  *IF The Warehouse is in the Available for felter fields
  IF lnWareHElm > 0
    =gfADel(@laOGFieldN , lnWareHElm , 1)
    =gfADel(@laOGFieldH , lnWareHElm , 1)
  ENDIF    && End of IF
    
  llRefresh = .F.          && Flag to know if we need to refresh the Option Grid or not
  
  *DO WHILE Loop to loop until the Warehouse is no more in the Variable
  *filter
  DO WHILE .T.
    lnVrFltElm = ASCAN(laOGVrFlt , 'PIKTKT.CWARECODE')      && Variable to hold the number of the array elment for the Warehouse in the Option grid Variable felter array
    lnVrFltElm = IIF(lnVrFltElm = 0 , 0 , ASUBSCRIPT(laOGVrFlt , lnVrFltElm , 1))
  
    *IF There is a row for the Warehouse in the Variable filter
    IF lnVrFltElm > 0
      =gfADel(@laOGVrFlt , lnVrFltElm , 1)
      lnOGVarFl = lnOGVarFl - 1        && Variable to hold the number of rows in the variable filter [Used by Option grid]
      llRefresh = .T.
    ELSE
      EXIT
    ENDIF    && End of IF
  ENDDO

  *IF We are to refresh the option grid
  IF llRefresh
    CLEAR READ
    RETURN
  ENDIF    && End of IF
ENDIF    && End of IF

*IF The Add notes to order lines seting is set to No
IF gfGetMemVar('M_OR_NOTE') <> 'Y'
  *C101854 Order line notepad Enable / Disable [Begin]
  *llRpOrdLNt = .F.
  *laOGObjCnt[4] = .F.
  *=lfOGShowGet('llRpOrdLNt')          && Disable the Print order line note pad Object
  lnOrdLn = ASCAN(laOgObjType,"llRpOrdLNt")
  IF lnOrdLn > 0
    lnOrdLn = ASUBSCRIPT(laOgObjType,lnOrdLn,1)
    laOgObjCnt[lnOrdLn] = .F.
    llRpOrdLNt = .F.
    =lfOgShowGet("llRpOrdLNt")	  
  ENDIF
  *C101854 Order line notepad Enable / Disable [End  ]
ENDIF    && End of IF

DIMENSION laTmpOrdLS[1,4]
*B803317,1 HBG 07/31/20000 Add 'custpo' field[Begin]
*=lfAFields('laTmpOrdLS' , 'ORDLINE' , 'LIKE QTY* , ORD* , PIK* , LINENO' +;
                          ' , SCALE , TOTQTY , TOTPIK , PRICE , GROUP' +;
                          ' , STYLE , DYELOT')
*B803626,1 HBG 10/09/2000 Add "Store" field to Temp order line [Begin]
*=lfAFields('laTmpOrdLS' , 'ORDLINE' , 'LIKE QTY* , ORD* , PIK* , LINENO' +;
                          ' , SCALE , TOTQTY , TOTPIK , PRICE , GROUP' +;
                          ' , STYLE , DYELOT , CUSTPO')
*C200219,1 HBG 06/09/2001 Check if this is for MBI [Begin]
IF TYPE('lcRpPrtBy') = 'U'
*C200219,1 [End]
  *B605554,1 BWA 02/14/2002 (Begin) Add 'Pack_id' field to temp order line file.
  =lfAFields('laTmpOrdLS' , 'ORDLINE' , 'LIKE QTY* , ORD* , PIK* , LINENO' +;
                            ' , SCALE , TOTQTY , TOTPIK , PRICE , GROUP' +;
                            ' , STYLE , DYELOT , CUSTPO , STORE,Pack_id')
  *B605554,1 BWA 02/14/2002 (End)
*C200219,1 HBG 06/09/2001 Add account field to be used in sorting in case this is for MBI [Begin]
ELSE
  =lfAFields('laTmpOrdLS' , 'ORDLINE' , 'LIKE QTY* , ORD* , PIK* , LINENO' +;
                            ' , SCALE , TOTQTY , TOTPIK , PRICE , GROUP' +;
                            ' , STYLE , DYELOT , CUSTPO , STORE , Account')
ENDIF
*C200219,1 [End]

*B803626,1 HBG [End]
*B803317,1 HBG 07/31/20000 Add 'custpo' field[End  ]
                          
DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'cGrupDetal'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 1
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0


*B804457,1 HBG 24/10/2001 Add field for currency symbol for the currency of the order [Begin]
DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'CurrSmbl'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 3
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0

DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'CurrCode'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 3
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0

=gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'cCurrCode','SH')
*B804457,1 [END]

*-- B803749,1 AAN Increase laTmpOrdH array 1 row to add field custpo [Start]
*DIMENSION laTmpOrdHS[4,4]
*C200219,1 HBG 06/09/2001 Increase laTmpOrdH array 1 row to add Field Account [Begin]
*DIMENSION laTmpOrdHS[5,4]
DIMENSION laTmpOrdHS[6,4]
*C200219,1 [End]
*-- B803749,1 AAN Increase laTmpOrdH array 1 row to add field custpo [End]
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'ORDER') , 4 , 1)
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'PIKTKT') , 4 , 5)
laTmpOrdHS[3 , 1] = 'NWEIGHT'
laTmpOrdHS[3 , 2] = 'N'
laTmpOrdHS[3 , 3] = 10
laTmpOrdHS[3 , 4] = 2
laTmpOrdHS[4 , 1] = 'STATUS'
laTmpOrdHS[4 , 2] = 'C'
laTmpOrdHS[4 , 3] = 1
laTmpOrdHS[4 , 4] = 0
*-- B803749,1 AAN Increase laTmpOrdH array 1 row to add field custpo [Start]
laTmpOrdHS[5 , 1] = 'CUSTPO'
laTmpOrdHS[5 , 2] = 'C'
laTmpOrdHS[5 , 3] = 15
laTmpOrdHS[5 , 4] = 0
*-- B803749,1 AAN Increase laTmpOrdH array 1 row to add field custpo [End]
*C200219,1 HBG 06/09/2001 Add Field Account [Begin]
laTmpOrdHS[6 , 1] = 'Account'
laTmpOrdHS[6 , 2] = 'C'
laTmpOrdHS[6 , 3] = 5
laTmpOrdHS[6 , 4] = 0
*C200219,1 [End]
SET ORDER TO PIKTKT IN PIKTKT
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO WAREHOUS IN WAREHOUS
SET ORDER TO PIKLINE IN PIKLINE
SET ORDER TO ORDHDR IN ORDHDR
SET ORDER TO ORDLINE IN ORDLINE

*C101854,1 Call any user defined when if a form contains [Begin]
IF (TYPE("lcSpWhen") = "C") AND lcSpWhen = "Y"
  =lfUsrVldFn("lfSpcWhen",lcFormName)
ENDIF
*C101854,1 Call any user defined when if a form contains [End  ]


*B604299,1 BWA 04/02/2002 Function to fill the variable lcText used in browsing the piktkt due to the status.[START]
=lfBrowStat()
*B604299,1 BWA 04/02/2002.[END]

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOptMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

=gfOptMsg('laOptMsg')

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
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

*!*************************************************************
*! Name      : lfvPikTkt
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Validation function for the Picking ticket field
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*B802265,1 BWA 08/16/1999   [start]
*FUNCTION lfvPikTkt

*PRIVATE lcVar , lcObj , laTemp

*lcVar = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET control
*lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

**IF Statment to check if we are going to Browse
*IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'PIKTKT'))

*  SELECT PIKTKT
*  SET RELATION TO 'O' + Order INTO ORDHDR
*  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
  
*  DIMENSION laTemp[1]
*  laTemp = ''      && Array to hold the Selected value

*  lcBrFields = "PIKTKT    :R :H= 'Pick tikit' ,"+;
               "PRINTED = IIF(PRTFLAG = 'P' , 'Yes' , 'No') :R :H= 'Printed' ,"+;
               "ORDER     :R :H= 'Order' ,"+;
               "STORE     :R :H= 'Store' ,"+;
               "ORDHDR.COMPLETE :R :H= 'Complete' ,"+;
               "DATE      :R :H= 'Pick Date' ,"+;
               "ACCOUNT   :R :H= 'Account' ,"+;
               "CUSTOMER.STNAME :R :H= 'Name' ,"+;
               "STATUS    :R :H= 'S' ,"+;
               "LABELS    :R :H= 'Labels' ,"+;
               "CWARECODE :R :H= 'WH. Code' ,"+;
               "CUSTPO    :R :H= 'Customer PO' "
  
*  lcFile_Ttl = "Pick tickets"
          
*  =gfBrows('FOR PIKTKT <> "******" ','PIKTKT','laTemp')
    
*  SET RELATION TO
  
*  *IF The user selected a record
*  IF !EMPTY(laTemp[1])
*    lcObj = laTemp[1]
*  ELSE    && Else
*    lcObj = laOldVal
*  ENDIF    && End of IF
  
*ENDIF    && End of IF

*&lcVar = lcObj      && Update the field
*B802265,1 BWA 08/16/1999   [END]
*!*************************************************************
*! Name      : lfvcustom
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvcustom

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvWareCod
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
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

*IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfGTmpOrdL
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to colect the neede data in a temp. Order lines file
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
 *! Return      : None
*!*************************************************************
*
FUNCTION lfGTmpOrdL
*B602916,1 Add private variables lcCDXName, lcFullPath
*PRIVATE lcForExp , lnTotRec , lnCurRec , lnRest , lnSavRec , lcOrder
PRIVATE lcForExp , lnTotRec , lnCurRec , lnRest , lnSavRec , lcOrder,;
 	    lcCDXName, lcFullPath
*B602916,1 end 	   
lcForExp = '!EOF("PIKTKT")' + IIF(EMPTY(lcRpExp) , '' ,;
           ' .AND. (' + lcRpExp + ')')        && Variable to hold the FOR Condition to SCAN for on the ORDLINE file
*-- C102022 AAN Fixing bug that report printing hold orders whatever selecting it or not[Begin].
lcForExp = IIF(llRpInHOrd,lcForExp,lcForExp + " AND (ORDHDR.Status <> 'H')")
*-- C102022 AAN [End].

*IF The Temp. Order Line file is Opened in one of the work areas
IF USED(lcTmpOrdL)
  USE IN (lcTmpOrdL)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdL) FROM ARRAY laTmpOrdLS
*B602916,1 Get the path of the created cursor and use it to get the index
*INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) TAG (lcTmpOrdL)
ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName  = STRTRAN(DBF() , ".TMP" , ".CDX")

SET FULLPATH &lcFullPath

*B803256,1 BWA Add new index to use it in updating the prtflag field.[START]
IF gcDevice <> 'SCREEN'
  INDEX ON PikTkt TAG (lcTmpOrdU) UNIQUE
ENDIF  
*B803256,1 [END]

*: *C200202,1 check if option is defined then choose new index[Start]
*INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) ;
          TAG (lcTmpOrdL) OF (lcCDXName)

*C200219,1 HBG 06/09/2001 Check for option of Print By too in case of MBI [Begin]
*IF TYPE('lcRpSortBy ') = 'U'
IF TYPE('lcRpSortBy') = 'U' AND TYPE('lcRpPrtBy') = 'U'
*C200219,1 [End]
  INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) ;
     TAG (lcTmpOrdL) OF (lcCDXName)
ELSE
  *C200219,1 HBG 06/09/2001 IF Print By Picking Ticket Number, the sorting will be as is [Begin]
  *B607323,1 ASH 06/03/2003 (Begin) Fix the bug of "'lcrpprtby' not found".
  *IF lcRpPrtBy = 'P'
  IF TYPE('lcRpPrtBy') = 'U' OR lcRpPrtBy = 'P'
  *B607323,1 ASH 06/03/2003 (End)
  *C200219,1 [End]
    IF lcRpSortBy = 'S'
      INDEX ON PikTkt + Order + cGrupDetal + STYLE  ;
         TAG (lcTmpOrdL) OF (lcCDXName)
    ELSE
      INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) ;
         TAG (lcTmpOrdL) OF (lcCDXName)
    ENDIF
  *C200219,1 HBG 06/09/2001 IF Print By Account Code, the sorting will be on Account code [Begin]
  ELSE
    IF lcRpSortBy = 'S'
      INDEX ON Account + PikTkt + Order + cGrupDetal + STYLE  ;
         TAG (lcTmpOrdL) OF (lcCDXName)
    ELSE
      INDEX ON Account + PikTkt + Order + cGrupDetal + STR(LineNo , 6) ;
         TAG (lcTmpOrdL) OF (lcCDXName)
    ENDIF
  ENDIF
  *C200219,1 [End]  
ENDIF
*: *C200202,1 [END]

*B602916,1 end

*IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  USE IN (lcTmpOrdH)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdH) FROM ARRAY laTmpOrdHS

*B602916,1 Get the path of the created cursor and use it to get the index
*INDEX ON Order + PikTkt TAG (lcTmpOrdH)
ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName = STRTRAN(DBF() , ".TMP" , ".CDX")
SET FULLPATH &lcFullPath
INDEX ON Order + PikTkt ; 
	 TAG (lcTmpOrdH) OF (lcCDXName)
*B602916,1 end

SELECT (lcTmpOrdH)
SET ORDER TO

SELECT PIKTKT
SET ORDER TO
SET RELATION TO 'O' + Order INTO ORDHDR

*B603301,1 BWA 11/28/1999 And remove the Thermo. to make the collecting the data faster[START]
*lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
*lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]
*B603301,1 BWA 11/28/1999 [END]

*B602881,1 (Begin) Save the old device status and set device to screen.
PRIVATE lcOldSelD
lcOldSelD = SET ("DEVICE")
SET DEVICE TO SCREEN
*B602881,1 (End)
*SCAN Loop to scan the PIKTKT file FOR the Option Grid condition

*B603301,1 BWA 11/28/1999 And remove the Thermo. to make the collecting the data faster[START]
WAIT 'Selecting picking tickets...' WINDOW NOWAIT

SCAN FOR &lcRpExp

*  lnCurRec = lnCurRec + 1
  *C200219,1 HBG 06/09/2001 Add Field Account [Begin]
  *INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status) VALUES;
                          (PIKTKT.Order , PIKTKT.PikTkt , PIKTKT.Status)
  INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status , Account) VALUES;
                          (PIKTKT.Order , PIKTKT.PikTkt , PIKTKT.Status , PIKTKT.Account)
  *C200219,1 [End]
*  =gfThermo(lnTotRec , lnCurRec , "Selecting picking tickets. " ,;
                       'Pick ticket # :' + PIKTKT.PikTkt)

ENDSCAN    && End of SCAN Loop

*IF The number of records Processed is less than the Total count to be done

* IF lnTotRec > lnCurRec .AND. lnCurRec <> 0
  
  *FOR Loop to loop for the remains of Total count to be done
*  FOR lnRest = lnCurRec TO lnTotRec
*    =gfThermo(lnTotRec , lnRest , "Selecting picking tickets. " , '')
*  ENDFOR    && End of FOR Loop
* ENDIF    && End of IF
*B603301,1 BWA 11/28/1999 [END]

SET ORDER TO TAG PIKTKT
SET RELATION TO

SELECT ORDLINE
SET RELATION TO PikTkt INTO PIKTKT
SET RELATION TO Style INTO STYLE ADDITIVE
SELECT (lcTmpOrdH)
SET ORDER TO TAG (lcTmpOrdH)
*B804457,1 HBG 24/10/2001 Set relation with ORDHDR to get the currency of the order [Begin]
SET RELATION TO 'O'+Order INTO ORDHDR ADDITIVE
*B804457,1 [End]
lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]

*SCAN Loop to scan the Temp. Order Header file FOR Status <> 'X'
*[Not Released]
SCAN FOR Status <> 'X'


  lnCurRec = lnCurRec + 1
  lnSavRec = RECNO()
  
  *IF There is one or more records for this Order in the ORDLINE file
  IF SEEK('O' + &lcTmpOrdH..Order , 'ORDLINE')
    M.cGrupDetal = 'D'
    
    lcOrder = Order
    SELECT ORDLINE
    
    *SCAN Loop to scan the ORDLINE file FOR The current Order and FOR the
    *Option Grid condition

    SCAN REST;
        WHILE Order = lcOrder;
          FOR &lcForExp
    
      SELECT (lcTmpOrdH)
      SEEK ORDLINE.Order + ORDLINE.PikTkt
      REPLACE nWeight WITH nWeight + (STYLE.nStyWeight * ORDLINE.totpik)
*-- B803749,1 AAN Fill a field custpo in lcTmpOrdH file [Start]
      IF(SEEK("O"+ORDLINE.ORDER,"ORDHDR") AND ORDHDR.Multipo)
        REPLACE Custpo WITH IIF(Custpo <> ORDLINE.Custpo,IIF(EMPTY(Custpo),ORDLINE.Custpo,;
        "Multi PO"),ORDLINE.Custpo)
      ELSE
        REPLACE Custpo WITH OrdHdr.Custpo
      ENDIF      
*-- B803749,1 AAN Fill a field custpo in lcTmpOrdH file [End]

      SELECT ORDLINE
      SCATTER MEMVAR MEMO
      *B804457,1 HBG 24/10/2001 Update currency symbol field by the symbol of the currency of the order [Begin]
      =SEEK(OrdHdr.cCurrCode,'SYCCURR')
      m.CurrCode = OrdHdr.cCurrCode
      m.CurrSmbl = SYCCURR.ccurrsmbl
      *B804457,1 [End]
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop
    
    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
    GO lnSavRec
    M.Order = Order
    
    *IF We are to Print the Order Note Pad
    *ahmed
    *IF llRpOrdNot
    IF llRpOrdNot .AND. SEEK('B'+order,'NOTEPAD')
    *ahmed end
      M.cGrupDetal = 'H'
      lnCurRec = lnCurRec - 1
      
      *SCAN Loop to scan the Temp. Order Header file FOR the current Order
      *and FOR Status <> 'X' [Not Released]
      
      *B603301,1 BWA 11/28/1999 Fix the bug of not printing a complete pick tickets.[START]
      *SCAN REST;
          WHILE Order = M.Order;
            FOR Status <> 'X' 

      SCAN REST;
          WHILE Order = M.Order;
            FOR Status <> 'X' AND Status <> 'C'
      *B603301,1 BWA 11/28/1999 [END]
        *C200219,1 HBG 06/09/2001 Fill Account field [Begin]
        m.Account = Account
        *C200219,1 [End]
        lnCurRec = lnCurRec + 1
        M.PikTkt = PikTkt
        *B804457,1 HBG 24/10/2001 Update currency symbol field by the symbol of the currency of the order [Begin]
        =SEEK(OrdHdr.cCurrCode,'SYCCURR')
        m.CurrCode = OrdHdr.cCurrCode
        m.CurrSmbl = SYCCURR.ccurrsmbl       
        *B804457,1 [End]
        INSERT INTO (lcTmpOrdL)  FROM MEMVAR
      ENDSCAN    && End of SCAN Loop
    ELSE    && Else
      LOCATE REST FOR Order + PikTkt > M.Order
    ENDIF    && End of IF
    SKIP -1
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for pick tickets. " ,;
                       'Pick ticket # :' + PikTkt)
ENDSCAN    && End of SCAN Loop

SELECT ORDLINE
SET RELATION TO

SELECT PIKLINE
SET RELATION TO Style INTO STYLE

SELECT (lcTmpOrdH)

*SCAN Loop to scan the Temp. Order Header file FOR Status = 'X' [Released]

*B603301,1 BWA 11/28/1999 Fix the bug of not printing a complete pick tickets.[START]
*SCAN FOR Status = 'X'
SCAN FOR Status $ 'XC'
*B603301,1 BWA 11/28/1999 [END]

  lnCurRec = lnCurRec + 1
  
  *IF There is one or more records for this Order in the PIKLINE file
  IF SEEK(&lcTmpOrdH..PikTkt + &lcTmpOrdH..Order , 'PIKLINE')
    M.cGrupDetal = 'D'
    SELECT PIKLINE
    
    *SCAN Loop to scan the PIKLINE file FOR The current Pick Ticket & Order
    SCAN REST;
        WHILE PikTkt + Order = &lcTmpOrdH..PikTkt + &lcTmpOrdH..Order
    
      SELECT (lcTmpOrdH)
      REPLACE nWeight WITH nWeight + (STYLE.nStyWeight * PIKLINE.totpik)
      
      SELECT PIKLINE
      SCATTER MEMVAR MEMO
      *B804457,1 HBG 24/10/2001 Update currency symbol field by the symbol of the currency of the order [Begin]
      =SEEK(OrdHdr.cCurrCode,'SYCCURR')
      m.CurrCode = OrdHdr.cCurrCode
      m.CurrSmbl = SYCCURR.ccurrsmbl       
      *B804457,1 [End]
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop
    
    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
    
    *IF We are to Print the Order Note Pad
    *ahmed
    *IF llRpOrdNot
    IF llRpOrdNot .AND. SEEK('B'+order,'NOTEPAD')
    *ahmed end 
      M.cGrupDetal = 'H'
      *C200219,1 HBG 06/09/2001 Fill Account field [Begin]
      m.Account = Account
      *C200219,1 [End]
      M.PikTkt = PikTkt
      M.Order = Order
      *B804457,1 HBG 24/10/2001 Update currency symbol field by the symbol of the currency of the order [Begin]
      =SEEK(OrdHdr.cCurrCode,'SYCCURR')
      m.CurrCode = OrdHdr.cCurrCode
      m.CurrSmbl = SYCCURR.ccurrsmbl       
      *B804457,1 [End]
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDIF    && End of IF
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for pick tickets. " ,;
                       'Pick ticket # :' + PikTkt)
ENDSCAN    && End of SCAN Loop

SELECT PIKLINE
SET RELATION TO

*B804457,1 HBG 24/10/2001 Release the relation with ORDHDR [Begin]
SELECT (lcTmpOrdH)
SET RELATION TO
*B804457,1 [End]

*IF The number of records Processed is less than the Total count to be done
IF lnTotRec > lnCurRec .AND. lnCurRec <> 0
  
  *FOR Loop to loop for the remains of Total count to be done
  FOR lnRest = lnCurRec TO lnTotRec
    =gfThermo(lnTotRec , lnRest ,;
              "Selecting order lines for pick tickets. " , '')
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF
*B602881,1 (Begin)  Restore the old device status.
SET DEVICE TO &lcOldSelD
*B602881,1 (End)

*!*************************************************************
*! Name      : lfAFields
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to places information about the structure of the
*!             a selected table into an array for a selected fields
*!*************************************************************
*! Called from : lfwRepWhen()
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : 1) The Array name
*!                     2) The File name
*!                     3) The Fields to get information about 
*!                        [You can use keleton <skel>]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAFields

PARAMETERS lcArrayNam , lcFile , lcFields

PRIVATE lcOldAlias , lcTempName

*IF The Array name Parameter is not of type character or empty
IF TYPE('lcArrayNam') <> 'C' .OR. EMPTY(lcArrayNam)
  RETURN
ENDIF    && End of IF

lcFile = IIF(TYPE('lcFile') <> 'C' , '' , lcFile)
lcFields = IIF(TYPE('lcFields') <> 'C' .OR. EMPTY(lcFields) , '' ,;
               'FIELDS ' + lcFields)

lcOldAlias = ALIAS()               && Variable to save the old Alis

*IF The File name Parameter is not empty and this file is Opened in one
*of the work areas
IF !EMPTY(lcFile) .AND. USED(lcFile)
  SELECT (lcFile)
ENDIF    && End of IF

*IF The Fields Parameter is empty
IF EMPTY(lcFields)
  =AFIELDS(&lcArrayNam)
ELSE    && Else
  lcTempName = gfTempName()          && Variable to hold a temp name
  COPY STRUCTURE EXTENDED TO &gcWorkDir.&lcTempName &lcFields
  
  SELECT * FROM &gcWorkDir.&lcTempName INTO ARRAY &lcArrayNam
  
  
  *IF The temp. file (lcTempName) is Opened in one of the work areas
  *[The file we used for COPY STRUCTURE EXTENDED]
  IF USED(lcTempName)
    USE IN &lcTempName
  ENDIF    && End of IF
  
  *IF The temp. file (lcTempName) exist in the Temp. Dir.
  *[The file we used for COPY STRUCTURE EXTENDED]
  IF FILE(gcWorkDir + lcTempName + '.DBF')
    ERASE &gcWorkdir.&lcTempName..DBF
  ENDIF    && End of IF
ENDIF    && End of IF

*IF The Variable we used to save the old Alias is not empty
IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Season ,
*!             Special Instructions , Terms
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdr

PRIVATE lcDistCntr

llEndGroup = .F.
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')

SELECT CUSTOMER
*B603313,1 ABD Get the ship via code from the customer file if it's multi store order [ Begin ] 
*SEEK 'M' + PIKTKT.Account
*lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia , ORDHDR.ShipVia) , 'SHIPVIA')
SEEK IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE 
lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')
*B603313,1 ABD [ End ]

lcSeason = gfCodDes(ORDHDR.Season , 'SEASON')
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST')
lcTerms = gfCodDes(ORDHDR.cTermCode , 'CTERMCODE')

SELECT CUSTOMER
SEEK IIF(EMPTY(PIKTKT.Store) , 'M' , 'S') + PIKTKT.Account + PIKTKT.Store

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')

*--HDM B802479,1 [Start] Get Sold To Phone#
*laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
laSoldTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))+ ' Phone# '+ TRANSFORM(Customer.Phone1 , lcPhonPict)
*--HDM B802479,1 [End]
=lfAdrShift('laSoldTo')

*IF ORDHDR.Alt_ShpTo = .T.
IF ORDHDR.Alt_ShpTo
  
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
  
ELSE    && Else
  
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  
  *If there is a distribution center
  IF !EMPTY(lcDistCntr)
    SEEK 'S' + PIKTKT.Account + lcDistCntr
  ENDIF
  
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  
  *--HDM B802479,1 [Start] Get Ship To Phone#
  *laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)

  *--HDM B802479,1 [End]
  
ENDIF    && End of IF

=lfAdrShift('laShipTo')

*B608388,1 NNA 12/27/2007 (Begin) Get the Currency Symbol for the firsttime to overwrite (lcCurChr) that
*B608388,1 NNA            have got in the first of the program 
=lfCurrSymb()
*B608388,1 NNA (End)

SELECT (lcTmpOrdL)
RETURN ''

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKT.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the PIKTKT file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup
llEndGroup = .T.

*IF The Divice is not Screen
IF gcDevice <> 'SCREEN'
  REPLACE PIKTKT.PrtFlag WITH 'P'
ENDIF    && End of IF

*B608388,1 NNA 12/27/2007 (Begin) Get lcCurChr for every order based on order's currency
=lfCurrSymb()
*B608388,1 NNA (End)

RETURN ''

*!*************************************************************
*! Name      : lfPktktSet
*! Developer : Haytham El_Sheltawi
*! Date      : 05/27/1998
*! Purpose   : Function to set and release the relations needed
*!             for the Pick ticket # field [For the In range]
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPktktSet

PARAMETERS lcParm

IF lcParm = 'S'
  
  SELECT PIKTKT
  SET RELATION TO 'O' + Order INTO ORDHDR
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
  
ELSE
  
  SELECT PIKTKT
  SET RELATION TO
  
ENDIF
*:****************************************************************
*: Name        : lfvOrderNo
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 05/14/2001
*: Purpose     : Function to Show the orders in the browse.
*:****************************************************************
*: Called from : Option Gird.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfvOrderNo()
*:****************************************************************
*C102332,1.
FUNCTION lfvOrderNo

*-- Create curser.
IF !USED(lcTempStyle)
  CREATE CURSOR (lcTempStyle) (Order C(06))
  INDEX ON Order TAG (lcTempStyle)
ELSE
  SELECT(lcTempStyle)
ENDIF
*ZAP

lcBrowFlds = [Order     :H= 'Order#':08,]+;
             [account   :H= 'Acct#' :10,]+;
             [Store     :H = 'Store' :08,]+;
             [Season    :H = 'Season'  :06,]+;
             [cDivision :H = 'Division' :06]       

SELECT ORDHDR
LOCATE
lcExpsty = '"O"'
=gfrange(lcBrowFlds,lcTempStyle,"Order",(lcExpsty),"","","@! XXXXXXXXXXXXXXXXXXX")

*-- End Of lfvOrderNo.
*:****************************************************************
*:B#804273,1 IAA Add custom function to refresh option grid.
*B606156,1 BWA 07/03/2002 Comment this lines due to the form will use one form only.[START]
*FUNCTION lfClear
*lcFormName = IIF(llRpLayOut="O","alpktkc2","alpktkcd")
*CLEAR READ
*B606156,1 BWA 07/03/2002.[END]

*--End of lfClear.
*B604299,1 BWA 04/02/2002 Function to fill the variable lcText used in browsing the piktkt due to the status.[START]
*!*************************************************************
*! Name      : lfBrowStat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/02/2002
*! Purpose   : Calling from the when function to fill lcText.
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfBrowStat

STORE SPACE(0) TO lcText1 , lcText
lnDataPos = ASCAN(laOgVrFlt,'PIKTKT.STATUS')
IF lnDataPos > 0
  lnDataPos  = ASUBSCRIPT(laOgVrFlt,lnDataPos,1)
  lcText1 = ALLTRIM(laOgVrFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgVrFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgVrFlt[lnDataPos,6] + '"' )
ENDIF
lcText  = lcText1  + " .AND. PIKTKT # '******' "

RETURN lcText
*--End of lfBrowStat
*!*************************************************************
*! Name      : lfvstate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/02/2002
*! Purpose   : Called from PIKTKT.STATUS field to match the user selection.
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvstate

STORE SPACE(0) TO lcText1 , lcText
lnDataPos = ASCAN(laOgVrFlt,'PIKTKT.STATUS')
IF lnDataPos > 0
  lnDataPos  = ASUBSCRIPT(laOgVrFlt,lnDataPos,1)
  lcText1 = ALLTRIM(laOgVrFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgVrFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgVrFlt[lnDataPos,6] + '"' )
ENDIF
lcText  = lcText1  + " .AND. PIKTKT # '******' "

*--End of lfvstate.
*B604299,1 BWA 04/02/2002.[END]


*:**************************************************************************
*:* Name        : lfUpdPrtDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/05/2007
*:* Purpose     : Update the date for the first time the piktkt will be printed
*:***************************************************************************
*C200848,1 
FUNCTION lfUpdPrtDt
PRIVATE lnSlct
lnSlct = SELECT(0)
SELECT &lcTmpOrdH
SCAN
  =SEEK(PIKTKT,'PIKTKT')
  IF PIKTKT.PRTFLAG = 'P' .AND. EMPTY(PIKTKT.PRTDATE)
    SELECT PIKTKT
    REPLACE PRTDATE WITH gdSysDate
  ENDIF
ENDSCAN

SELECT (lnSlct)
*-- end of lfUpdPrtDt.

*!*************************************************************
*! Name      : lfCurrSymb
*! Developer : Nader Nabil (NNA)
*! Date      : 12/24/2007
*! Purpose   : get the Currency symbol for the order Currancy
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*!B608388,1
FUNCTION lfCurrSymb
PRIVATE lnSelect
lcCurChr =''
lnSelect=SELECT(0)
IF !USED('SYCCURR')
  =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'CCURRCODE','SH')
ENDIF
IF SEEK(ORDHDR.CCURRCODE,'SYCCURR') 
  lcCurChr = ALLTRIM(SYCCURR.CCURRSMBL)
ENDIF
SELECT(lnSelect)
RETURN
*-->End of Function lfCurrSymb.