*:***************************************************************************
*: Program file  : SOORCNB
*: Program desc. : Order confirmation Form B (Standard form)
*: For Report    : Order confirmation
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Sales Order (SO)
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Reference     : *B802772
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfwOldVal, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORCNB
*:***************************************************************************
*: Notes : This form was for EILEEN FISHER converted from 2.6 to 2.7 under number 
*:         (C101403) did by (adel) and we make as standard form B.
*Modifications
*B603569,1 AMM 04/11/2000 Fix the bug of not printing EDI order line in case of multi store order
*B603579,1 ABD 04/13/2000 Fix bug that Didn't print correct style  description 
*B603579,1 ABD            If you print more than one style.
*E301401,1 ABD 06/07/2000 For EDI Temporary Orders, change price columns to labelled
*E301401,1 ABD            "Transmitted Price" and Amount to labelled "Wholesale Price," 
*E301401,1 ABD            Both on-screen and in outputs.
*B803329,1 MHM 06/25/2000 print the optional message, add comp name, fax and picture to the phone
*B803659,1 MHM 09/28/2000 Fix Bug does not printing proberly (The footer is printed in the next page)
*B603713,1 MHM 10/17/2000 increase the length of the fields (Gros_Price ,Price, 
*B603713,1                PriceA , PriceB , PriceC , Amount) With 2 digit we add 
*B603713,1                record in SYREPUVR to get print dicemal yes or no 
*B604311,1  AAN 04/19/2001 Correct the dicemal value of net amount in the report
*B804153,1  AME 05/07/2001 fix the bug of not printing continued next page,and print scale header after stars line 
*B804153,1                 for orders which take more than one page
*B804441,1 MHM 10/25/2001 Fix bug of wrong ShipVia in Multi Store Order.  
*B804551,1 MHM 12/20/2001 Fix bug of wrong Distrebution Center in Multi Store Order.  
*B605711,1 TMI 03/25/2002 Fix the bug that if multistore order with  lines >=8 per a store then
*B605711,1                styles are not printed correctly.
*B605910,1 TMI 05/02/2002 Fix the bug that the Department is not printed.
*E301939,1 RAE 09/05/2002 Add an option filter - Print Book Qty  (Y/N) - to print booked 
*                         quantity for partially shipped orders.
*B606435,1 RAE 09/18/2002 Print 80 characters in * lines instead of 85 characters.
*B606406,1 TMI 10/27/2002 Fix the bug that only one character of the department is printed 
*:*****************************************************************************************
*-- If No records selected.
SELECT ORDHDR
SET SKIP TO
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
*B802772,4 AMM start
ELSE
  llNoRec = .F.
*B802772,4 AMM end
ENDIF
IF FILE('&gcDataDir.SOORCNZ.MEM')
  RESTORE FROM &gcDataDir.SOORCNZ.MEM ADDITIVE
ELSE
  lcDevice = SET('DEVICE')
  *--Would you like to print the report by Pack_Id ?
  lcMpack  = IIF(gfModalGen('TRM32063B32000','DIALOG' )=1,'Y','N')
ENDIF
llPack = (lcMpack = 'Y')
SAVE ALL LIKE lcM* TO &gcDataDir.SOORCNZ.MEM
RELEASE ALL LIKE lcM*
*--Open needed files.
=gfOpenFile(gcDataDir+'Spck_Lin',gcDataDir+'Spck_Lin','SH')
=gfOpenFile(gcDataDir+'Spck_Hdr',gcDataDir+'Spck_Hdr','SH')
*--Initilize variables
DIMENSION laAddress[1]
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6,laAddress
STORE '' TO lcOrder,lcAccount,lcMulti,lcStore,laAddress,lcTermData,lcShipVia,lcDivison
STORE ' ' TO lcSpcInst,lcSeason,lcPrePack,XMER_DISC,lcScale,lcPrevScale
STORE 0 TO lnTotQty,lnTotOrd,XMER_AMNT,XNET_AMNT,ROW,lnScount
*B802772,4 AMM comment out
*E301939,1 RAE variable to store total book qty. [start]
STORE 0 TO lnTotBook
*E301939,1 RAE [end]
*:B804551,1 MHM 12/20/2001 [Start]
STORE '' To lcDcStore
*:B804551,1 MHM 12/20/2001 [End]

*llNoRec = .T.
*B802772,4 AMM end
llNrmPrt = .T.
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'CDIVISION'
laDivLName[1,2] = 'lcDivLName'
*-- Get the style and color
lcMajTitl  = ALLTRIM(gfItemMask('HM'))
lcNonMajTl = ''
=lfGetColor()
*B606435,1 RAE Print 80 characters in * lines instead of 85 characters. [start]
*A= REPLICATE('*',85)
A= REPLICATE('*',80)
*B606435,1 RAE [end]

*   ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
B='|  START  | COMPLETE |    TERMS      |   SHIPVIA     | SEASON   |REP1 REP2'
*   MM/DD/YY   MM/DD/YY  123456789012345 123456789012345 1234567890 123  123
*E301401,1 ABD For EDI Temporary Orders, change price columns to labelled
*E301401,1 ABD "Transmitted Price" and Amount to labelled "Wholesale Price" [Begin] 

IF lcRpOrdTyp = 'T'
  C='      GRP '+lcMajTitl+SPACE(19-LEN(lcMajTitl+lcNonMajTl))+lcNonMajTl+'            TRANSMITTED PRICE  PIECES WH.SALE PRICE'
ELSE
  C='      GRP '+lcMajTitl+SPACE(19-LEN(lcMajTitl+lcNonMajTl))+lcNonMajTl+'                         PRICE  PIECES       AMOUNT'
ENDIF
*E301401,1 ABD [End]

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    STYLE   CLR                                     PRICE   PIECES       AMOUNT
*123 1234567 123 12345678901234567890              1234.99   123456   123,456.99
*--Fill the company address variables.
*--Center the address
laCompAdd1 = ''
laCompAdd2 = SPACE(ROUND((80-LEN(laCompAdd[2]))/2,0))+laCompAdd[2]
laCompAdd3 = SPACE(ROUND((80-LEN(laCompAdd[3]))/2,0))+laCompAdd[3]
laCompAdd4 = SPACE(ROUND((80-LEN(laCompAdd[4]))/2,0))+laCompAdd[4]

*--B803329 (MHM) add fax no and dashes to telephone [begin]
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
laCompAdd7 = ' Fax : ' + TRANSFORM(SyCcomp.Ccom_Fax, lcPhonPict)
laCompAdd[5] = 'Phone : '+TRANSFORM(SyCcomp.Ccom_phon, lcPhonPict)+ '   ' +laCompAdd7
laCompAdd6 = SPACE(ROUND((80-LEN(gcCom_Name))/2,0))+gcCom_Name
laCompAdd5 = SPACE(ROUND((80-LEN(laCompAdd[5]))/2,0))+laCompAdd[5]
*--B803329 (MHM) add fax no and dashes to telephone [end]
*B803659,1 MHM 09/28/2000 [start]
*lnMaxRow = 64
lnMaxRow = 51
*B803659,1 MHM 09/28/2000 [end]
STORE 1 TO lnNoteLn,lnSvNoteLn 
lcMulTOrDT = gfTempName()
*-- SET ORDER TO cordtype+order+STR(lineno,6)
SET ORDER TO TAG ORDLINE IN ORDLINE
SET DEVICE TO PRINT
*------------------------------
* SECTION: MAIN LOOP
*------------------------------
lnPageNo=1
SELECT ORDHDR
*--Refresh the relation.
GO TOP

SCAN REST WHILE INKEY()<> 32 FOR &lcRpExp
  WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
  *-----------------------------------------------------------------
  * Get and print invoice header.
  * Initialize document totals.
  *-----------------------------------------------------------------
  *-- Function to get order HEADER information.
  lnPageNo=1
  =lfGetOdHdr()
  SELECT OrdHdr
  *-- Function to print order HEADER information.  
  =lfPrintHdr()
  *B803659,1 MHM 09/28/2000 [Start]
  *ROW = ROW+2
  llRowInc= .F.
  *B803659,1 MHM 09/28/2000 [End]
  IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*' 
    *B803659,1 MHM 09/28/2000 [start]
     llRowInc = .T.
     ROW = ROW +1
    *ROW=IIF(ROW=25,26,15)
    *B803659,1 MHM 09/28/2000 [end]
    @ ROW,00 SAY  Note1
  ENDIF       
  IF !EMPTY(Note2) .AND. SUBSTR(Note2,1,1) <> '*' 
     IF llRowInc = .F.
       ROW = ROW + 1
     ENDIF
    *B803659,1 MHM 09/28/2000 [start]
    *ROW=IIF(ROW>=25,26,15)
    *B803659,1 MHM 09/28/2000 [end]
    @ ROW,32 SAY Note2
  ENDIF       
  *-- START PRINTING THE ORDER LINES
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  *LN#   GRP STYLE           CLR                        PRICE   PIECES       AMOUNT
  *123456 X  123456789012345 123 12345678901234567890 1234.99   123456   123,456.99
  *            11111 22222 33333 44444 55555 66666 77777 88888
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  *B605711,1 [START] 
  SET ORDER TO TAG ORDLINE IN ORDLINE
  *B605711,1 [END  ]

  IF lcMulti = 'Y'
    IF !USED(lcMulTOrDT)
      =gfOpenFile(gcWorkDir+lcMulTOrDT,'','EX')
    ELSE
      SELECT (lcMulTOrDT)
    ENDIF  
  ELSE
    SELECT ORDLINE
    *B605711,1 [START] SET the needed sort by.
    SET ORDER TO TAG IIF(lcRpSortBy = 'L' , 'ORDLINE' , 'ORDLINST' ) IN ORDLINE
    LOCATE
    *B605711,1 [END  ] SET the needed sort by.
    *B802772,4 AMM start
    *=SEEK('O'+lcOrder)
    =SEEK(ORDHDR.cOrdType+lcOrder)
    *B802772,4 AMM end
    
  ENDIF
  *B803659,1 MHM 09/28/2000 [Start]
  *ROW = IIF(Row >20, IIF(ROW=26,27,26), IIF(ROW=15,16,15) )
  ROW =ROW +1
  *B803659,1 MHM 09/28/2000 [End]
  *B802772,4 AMM start
  *SCAN WHILE EVAL(KEY()) = 'O'+lcOrder
  *B605711,1 TMI [Start] If TotQty <= 0 , skip this line
  *SCAN WHILE EVAL(KEY()) = ORDHDR.cOrdType+lcOrder

  *E301939,1 RAE Filter on TOTQTY > 0 for open quantities noly [Start]
  *SCAN WHILE EVAL(KEY()) = ORDHDR.cOrdType+lcOrder FOR TOTQTY > 0
  SCAN WHILE EVAL(KEY()) = ORDHDR.cOrdType+lcOrder FOR IIF(lcRpBook = 'N',TOTQTY > 0,TOTQTY > 0 OR TOTQTY = 0 )
  *E301939,1 RAE [End]

  *B605711,1 TMI [End  ] If TotQty <= 0 , skip this line
  *B802772,4 AMM end
    *B605711,1 TMI [Start] The second case must be the first since the store may be changed in
    *B605711,1 TMI         the middle of the page,so it is best to convert it to an if then else statement
    *DO CASE
    *  CASE ROW >= lnMaxRow
    IF lcMulti # 'Y' OR &lcMulTOrDT..Store = lcStore  &&this is the reverse of the second case
      IF ROW >= lnMaxRow
    *B605711,1 TMI [End  ]
        lnPageNo = IIF(lcMulti <> 'Y' OR &lcMulTOrDT..Store = lcStore,lnPageNo+1,1 )
        *-- Function to print order HEADER information.  
        *B804153,1 AME [Start] INFORM THE USER THAT WE ARE BEGINING NEW PAGE
        ROW = ROW+1
        @ ROW,00 SAY "--------------------C O N T I N U E D    N E X T    P A G E --------------------"
        *B804153,1 AME [End]
        =lfPrintHdr()
        
        *B804153,1 AME [Start] 
        ROW = ROW+1
        *B804153,1 AME [End]

        *B803659,1 MHM 09/28/2000 [START]
        *ROW = ROW+2
        *ROW=IIF(ROW>=25,26,15)        
        *B803659,1 MHM 09/28/2000 [END]
    *B605711,1 TMI [Start] Close the above inner if statement , ALSE Open the else part
    * CASE  lcMulti = 'Y' AND (&lcMulTOrDT..Store <> lcStore)
      ENDIF  && ROW >= lnMaxRow
      
    ELSE
    
    *B605711,1 TMI [End  ] Close the above inner if statement , ALSE Open the else part
        *-- Print the note pad
        =lfPrtNtPad()
        *-- Say the total for the previous store
        XMER_DISC = OrdHdr->Disc
        XMER_AMNT = (XMER_DISC * lnTotOrd)/100
        XNET_AMNT = lnTotOrd - XMER_AMNT

        *B803329 (MHM) START, shift lines upp to find space to the optional message
        *@ 57,00 SAY A
        *@ 58,10 SAY 'GROSS AMOUNT      =>'
        *@ 58,69 SAY lnTotOrd PICTURE '999,999.99'
        *@ 59,10 SAY 'MERCHANDISE DISC. =>'
        *@ 59,40 SAY XMER_DISC PICTURE '999.99' + " %"
        *@ 59,69 SAY XMER_AMNT PICTURE '999,999.99'
        *@ 60,10 SAY 'NET AMOUNT        =>'
        *@ 60,60 SAY lnTotQty PICTURE '999999'
        *@ 60,69 SAY XNET_AMNT PICTURE '999,999.99'
        *@ 61,00 SAY A
        *@ 62,22 SAY '______________________________________   ____________'
        *@ 63,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
        @ 53,00 SAY A
        *E301939,1 RAE Change GROSS AMOUNT to GROSS OPEN AMOUNT when print book qty.[Start]
        *@ 54,10 SAY 'GROSS AMOUNT      =>'
        IF lcRpBook = 'Y'
          @ 54,10 SAY 'GROSS OPEN AMOUNT      =>'          
        ELSE
          @ 54,10 SAY 'GROSS AMOUNT      =>'
        ENDIF
        *E301939,1 RAE [End]
        *:B603713,11  MHM 10/17/2000 [start]
        *@ 54,69 SAY lnTotOrd PICTURE '999,999.99'
        *@ 55,10 SAY 'MERCHANDISE DISC. =>'
        *@ 55,40 SAY XMER_DISC PICTURE '999.99' + " %"
        *@ 55,69 SAY XMER_AMNT PICTURE '999,999.99'
        *@ 56,10 SAY 'NET AMOUNT        =>'
        *@ 56,60 SAY lnTotQty PICTURE '999999'
        *@ 56,69 SAY XNET_AMNT PICTURE '999,999.99'
        IF lcRpDeciml = "Y"
          @ 54,69 SAY lnTotOrd PICTURE '999,999.99'
          @ 55,10 SAY 'MERCHANDISE DISC. =>'
          @ 55,40 SAY XMER_DISC PICTURE '999.99' + " %"
          @ 55,69 SAY XMER_AMNT PICTURE '999,999.99'
          *E301939,1 RAE Change NET AMOUNT to NET OPEN AMOUNT when print book qty.[Start]
          *@ 56,10 SAY 'NET AMOUNT        =>'
          IF lcRpBook = 'Y'
            @ 56,10 SAY 'NET OPEN AMOUNT      =>'
          ELSE
            @ 56,10 SAY 'NET AMOUNT      =>'
          ENDIF
          *E301939,1 RAE [End]
          @ 56,60 SAY lnTotQty PICTURE '999999'
          *--B604311,1  AAN Correct the dicemal value of net amount in the report[Begin]
          *@ 56,69 SAY XNET_AMNT PICTURE '999,999.99'
          @ 56,69 SAY ROUND(XNET_AMNT,2) PICTURE '999,999.99'
          *--B604311,1  AAN Correct the dicemal value of net amount in the report[End]
        ELSE
          @ 54,69 SAY lnTotOrd PICTURE '999,999999'
          @ 55,10 SAY 'MERCHANDISE DISC. =>'
          @ 55,40 SAY XMER_DISC PICTURE '999999' + " %"
          @ 55,69 SAY XMER_AMNT PICTURE '999,999999'
          *E301939,1 RAE Change NET AMOUNT to NET OPEN AMOUNT when print book qty.[Start]
          *@ 56,10 SAY 'NET AMOUNT        =>'
          IF lcRpBook = 'Y'
            @ 56,10 SAY 'NET OPEN AMOUNT        =>'
          ELSE
            @ 56,10 SAY 'NET AMOUNT        =>'
          ENDIF
          *E301939,1 RAE [End]
          @ 56,60 SAY lnTotQty PICTURE '999999'
          @ 56,69 SAY XNET_AMNT PICTURE '999,999999'
        ENDIF
        *:B603713,11  MHM 10/17/2000 [end] 
        *E301939,1 RAE add new line to print total book qty. [start]
        *@ 57,00 SAY A
        *@ 58,22 SAY '______________________________________   ____________'
        *@ 59,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
        **-- B803329 (MHM) ADD OPTIONAL MESSAGE [begin]
        *@ 60,00 SAY REPLICATE('*',85)
        *@ 61,00 SAY lcRpMsg1
        *@ 62,00 SAY lcRpMsg2
        *@ 63,00 SAY lcRpMsg3
        IF lcRpBook = 'Y'
          @ 57,10 SAY 'BOOK QTY      =>'
          @ 57,60 SAY lnTotBook PICTURE '999999'

          @ 58,00 SAY A
          @ 59,22 SAY '______________________________________   ____________'
          @ 60,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '          
          *B606435,1 RAE Print 80 characters in * lines instead of 85 characters. [start]
          *@ 61,00 SAY REPLICATE('*',85)
          @ 61,00 SAY REPLICATE('*',80)
          *B606435,1 RAE [end]
          @ 62,00 SAY lcRpMsg1
          @ 63,00 SAY lcRpMsg2
          @ 64,00 SAY lcRpMsg3
        ELSE
          @ 57,00 SAY A
          @ 58,22 SAY '______________________________________   ____________'
          @ 59,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
          *B606435,1 RAE Print 80 characters in * lines instead of 85 characters. [start]
          *@ 60,00 SAY REPLICATE('*',85)
          @ 60,00 SAY REPLICATE('*',80)
          *B606435,1 RAE [end]
          @ 61,00 SAY lcRpMsg1
          @ 62,00 SAY lcRpMsg2
          @ 63,00 SAY lcRpMsg3          
        ENDIF
          *E301939,1 RAE [end]
        *-- B803329 (MHM) ADD OPTIONAL MESSAGE [end]
        
        *-- Get the new store address.
        =lfGetStAd()
        *-- Function to print order HEADER information.
        lnPageNo=1
        =lfPrintHdr()
    *B605711,1 TMI [Start] Close the above outer if statement 
        ROW = ROW+1        && Increase row line
    
    ENDIF  && lcMulti # 'Y' OR &lcMulTOrDT..Store = lcStore  
    
    *B605711,1 TMI [End  ] Close the above outer if statement 
        *B803659,1 MHM 09/28/2000 [Start]
        *ROW = ROW+2
        *ROW=IIF(ROW>=25,26,15)        
        *B803659,1 MHM 09/28/2000 [END]
      *B605711,1 TMI [Start] Move this case above as separete if condition and remove the endcase.
      *CASE TotQty<=0
      *  LOOP
    *ENDCASE
      *B605711,1 TMI [End  ] Move this case above as separete if condition and remove the endcase. 
    *B603579,1 ABD Seek in the style file to get the style description .  [Begin]
    =SEEK(IIF(lcMulti = 'Y' , &lcMulTOrDT..Style,Ordline.Style),'STYLE' )
    *B603579,1 ABD [End]
    lcStyDesc  = STYLE.Desc
    IF lcMulti = 'Y'
      *B603569,1 AMM Start
      *=SEEK('O'+Order+STR(LineNo,6),'OrdLine')
      =SEEK(ORDHDR.cOrdType+Order+STR(LineNo,6),'OrdLine')
      *B603569,1 AMM end      
    ENDIF
    lcPackKey = 'P'+lcAccount+OrdLine.Pack_Id
    IF !SEEK('P'+lcAccount+OrdLine.Pack_Id+Style,'Spck_Lin')
      =SEEK('P'+'*****'+OrdLine.Pack_Id+Style,'Spck_Lin')
      lcPackKey = 'P'+'*****'+OrdLine.Pack_Id
    ENDIF
    IF !SEEK('P'+lcAccount+OrdLine.Pack_Id,'Spck_Hdr')
      =SEEK('P'+'*****'+OrdLine.Pack_Id,'Spck_Hdr')
    ENDIF
    IF OrdLine.Pack_Id <> lcPrePack .AND. llPack
      lcOldAlias = ALIAS()
      SELECT Spck_Lin
      SUM REST WHILE Type+Account+Pack_Id+Style = lcPackKey ;
                     Spck_Lin.Qty1,Spck_Lin.Qty2,Spck_Lin.Qty3,;
                     Spck_Lin.Qty4,Spck_Lin.Qty5,Spck_Lin.Qty6 TO ARRAY lnQty

      SELECT (lcOldAlias)
      lcPrePack = OrdLine.Pack_Id
      IF !EMPTY(OrdLine.Pack_Id)
        @ ROW,00 SAY 'PACK_ID : '+OrdLine.Pack_Id+'  '+'DESC.:'+Spck_Hdr.Desc+' ' +;
                     +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnQty[3],3)+' ' +;
                     STR(lnQty[4],3)+' '+STR(lnQty[5],3)+' '+STR(lnQty[6],3)
        ROW=ROW+1
      ENDIF 
    ENDIF
    IF Scale<>lcPrevScale .AND. llNrmPrt   
      lcPrevScale = Scale 
      lcScale     = GETSCALE(Style.Scale,SPACE(1))   
      lnScount    = Scale.Cnt
      IF lnScount >1
        @ ROW,18 SAY lcScale
        ROW = ROW+1
      ENDIF
    ENDIF
    IF llNrmPrt
      @ ROW,00 SAY LineNo
      @ ROW,07 SAY Group
      @ ROW,10 SAY Style
      @ ROW,30 SAY lcStyDesc
      *:B603713,11  MHM 10/17/2000 [start]
      *@ ROW,51 SAY Price    PICTURE '9999.99'
      *@ ROW,60 SAY TotQty   PICTURE '999999'
      *lnAmount= TotQty * Price
      *@ ROW,69 SAY lnAmount  PICTURE '999,999.99'
      IF lcRpDeciml = "Y"
        @ ROW,51 SAY Price    PICTURE '9999.99'
        @ ROW,60 SAY TotQty   PICTURE '999999'
        lnAmount= TotQty * Price
        @ ROW,69 SAY lnAmount  PICTURE '999,999.99'
      ELSE
        @ ROW,51 SAY Price    PICTURE '9999999'
        @ ROW,60 SAY TotQty   PICTURE '999999'
        lnAmount= TotQty * Price
        @ ROW,69 SAY lnAmount  PICTURE '999,999999'
      ENDIF
      *:B603713,11  MHM 10/17/2000 [end]
      ROW =ROW+1
      *E301939,1 RAE print book quantities [Start]
      IF lcRpBook = 'Y'
        @ ROW,00 SAY 'BOOK QTY'
        X = 1
        DO WHILE lnScount >1 .AND. X<=8
          Z =STR(X,1)
          @ ROW,(X*6)+12 SAY BOOK&Z   PICTURE '@Z 99999'
          X = X+1
        ENDDO
      ENDIF
      ROW = ROW+1
      @ ROW,00 SAY 'OPEN QTY'            
      *E301939,1 RAE [End]
      X = 1
      DO WHILE lnScount >1 .AND. X<=8
        Z =STR(X,1)
        @ ROW,(X*6)+12 SAY QTY&Z   PICTURE '@Z 99999'
        X = X+1
      ENDDO
      ROW = ROW+1
      * CUM ORDER TOTALS
      lnTotQty  = lnTotQty+ TotQty
      lnTotOrd  = lnTotOrd+ lnAmount
      *E301939,1 RAE [start]
      lnTotBook = lnTotBook + TotBook
      *E301939,1 RAE [end]     
    ENDIF
    *-- Print the order line notes.
    IF llRpOrdLnt .AND. MEMLINES(Note_Mem) <> 0 .AND. ! lfPrtONte() 
      llNrmPrt = .F.
      lcAlias = ALIAS()
      SELECT ORDHDR
      *-- Function to print order HEADER information.  
      lnPageNo=lnPageNo+1
      =lfPrintHdr()
      *B803659,1 MHM 09/28/2000 [Start]
      *ROW = ROW+2
       llRowInc= .F.
      *B803659,1 MHM 09/28/2000 [END]
      IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*' 
        *B803659,1 MHM 09/28/2000 [Start]
         llRowInc= .F.
         ROW = ROW +1
        *ROW=IIF(ROW=25,26,15)
        *B803659,1 MHM 09/28/2000 [END]
        @ ROW,00 SAY  Note1
      ENDIF       
      IF !EMPTY(Note2) .AND. SUBSTR(Note2,1,1) <> '*' 
        *B803659,1 MHM 09/28/2000 [Start]
        *ROW=IIF(ROW>=25,26,15)
        IF llRowInc = .F.
          ROW = ROW +1
        ENDIF
        *B803659,1 MHM 09/28/2000 [END]
        @ ROW,32 SAY Note2
      ENDIF
      SELECT (lcAlias)
      SKIP -1
      LOOP
    ELSE
      llNrmPrt = .T.
    ENDIF
    ROW = ROW+1
  ENDSCAN
  *------------------------ END PRINT LINE LOOP ----------------------
  *-- Print the note pad
  =lfPrtNtPad()
  *-- Print totals.
  XMER_DISC = OrdHdr->Disc
  XMER_AMNT = (XMER_DISC * lnTotOrd)/100
  XNET_AMNT = lnTotOrd - XMER_AMNT

  @ 53,00 SAY A
  
  *E301939,1 RAE Change GROSS AMOUNT to GROSS OPEN AMOUNT when print book qty.[Start]
  *@ 54,10 SAY 'GROSS AMOUNT      =>'
  IF lcRpBook = 'Y'
    @ 54,10 SAY 'GROSS OPEN AMOUNT      =>'          
  ELSE
    @ 54,10 SAY 'GROSS AMOUNT      =>'
  ENDIF     
  *E301939,1 RAE [End]

  *:B603713,11  MHM 10/17/2000 [start]
  *@ 54,69 SAY lnTotOrd PICTURE '999,999.99'
  *@ 55,10 SAY 'MERCHANDISE DISC. =>'
  *@ 55,40 SAY XMER_DISC PICTURE '999.99' + " %"
  *@ 55,69 SAY XMER_AMNT PICTURE '999,999.99'
  *@ 56,10 SAY 'NET AMOUNT        =>'
  *@ 56,60 SAY lnTotQty PICTURE '999999'
  *@ 56,69 SAY XNET_AMNT PICTURE '999,999.99'

  IF lcRpDeciml = "Y"
    @ 54,69 SAY lnTotOrd PICTURE '999,999.99'
    @ 55,10 SAY 'MERCHANDISE DISC. =>'
    @ 55,40 SAY XMER_DISC PICTURE '999.99' + " %"
    @ 55,69 SAY XMER_AMNT PICTURE '999,999.99'
    *E301939,1 RAE Change NET AMOUNT to NET OPEN AMOUNT when print book qty.[Start]
    *@ 56,10 SAY 'NET AMOUNT        =>'
    IF lcRpBook = 'Y'
      @ 56,10 SAY 'NET OPEN AMOUNT        =>'
    ELSE
      @ 56,10 SAY 'NET AMOUNT        =>'
    ENDIF
    *E301939,1 RAE [End]
    @ 56,60 SAY lnTotQty PICTURE '999999'
    *--B604311,1 AAN Correct the dicemal value of net amount in the report[Begin]
    *@ 56,69 SAY XNET_AMNT PICTURE '999,999.99'
    @ 56,69 SAY ROUND(XNET_AMNT,2) PICTURE '999,999.99'
    *--B604311,1 AAN Correct the dicemal value of net amount in the report[End]
  ELSE
    @ 54,69 SAY lnTotOrd PICTURE '999,999999'
    @ 55,10 SAY 'MERCHANDISE DISC. =>'
    @ 55,40 SAY XMER_DISC PICTURE '999999' + " %"
    @ 55,69 SAY XMER_AMNT PICTURE '999,999999'
    *E301939,1 RAE Change NET AMOUNT to NET OPEN AMOUNT when print book qty.[Start]
    *@ 56,10 SAY 'NET AMOUNT        =>'
    IF lcRpBook = 'Y'
      @ 56,10 SAY 'NET OPEN AMOUNT        =>'
    ELSE
      @ 56,10 SAY 'NET AMOUNT        =>'
    ENDIF
    *E301939,1 RAE [End]
    @ 56,60 SAY lnTotQty PICTURE '999999'
    @ 56,69 SAY XNET_AMNT PICTURE '999,999999'
  ENDIF
  *:B603713,11  MHM 10/17/2000 [end]
  *E301939,1 RAE add new line to print total book qty. [start]
  *@ 57,00 SAY A
  *@ 58,22 SAY '______________________________________   ____________'
  *@ 59,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
  *-- B803329 (MHM) ADD OPTIONAL MESSAGE [begin]
  *@ 60,00 SAY REPLICATE('*',85)
  *@ 61,00 SAY lcRpMsg1
  *@ 62,00 SAY lcRpMsg2
  *@ 63,00 SAY lcRpMsg3
 
  IF lcRpBook = 'Y'
    @ 57,10 SAY 'BOOK QTY      =>'
    @ 57,60 SAY lnTotBook PICTURE '999999'

    @ 58,00 SAY A
    @ 59,22 SAY '______________________________________   ____________'
    @ 60,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
    *B606435,1 RAE Print 80 characters in * lines instead of 85 characters. [start]
    *@ 61,00 SAY REPLICATE('*',85)
    @ 61,00 SAY REPLICATE('*',80)
    *B606435,1 RAE [end]
    @ 62,00 SAY lcRpMsg1
    @ 63,00 SAY lcRpMsg2
    @ 64,00 SAY lcRpMsg3
  ELSE
    @ 57,00 SAY A
    @ 58,22 SAY '______________________________________   ____________'
    @ 59,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
    *B606435,1 RAE Print 80 characters in * lines instead of 85 characters. [start]
    *@ 60,00 SAY REPLICATE('*',85)
    @ 60,00 SAY REPLICATE('*',80)
    *B606435,1 RAE [end]
    @ 61,00 SAY lcRpMsg1
    @ 62,00 SAY lcRpMsg2
    @ 63,00 SAY lcRpMsg3          
  ENDIF
  *E301939,1 RAE [end]
  *--B803329 (MHM) ADD OPTIONAL MESSAGE [end]
ENDSCAN
*B802772,4 AMM Comment out
*DO ENDREPORT
*B802772,4 AMM end
*--Close files opened in this file.
IF USED(lcMulTOrDT)
  USE IN (lcMulTOrDT)
  ERASE (gcWorkDir+lcMulTOrDT) + '.DBF'
  ERASE (gcWorkDir+lcMulTOrDT) + '.CDX'
  ERASE (gcWorkDir+lcMulTOrDT) + '.FPT'
ENDIF
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfPrtONte()
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Synopsis  : Print Order line notes.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfPrtONte

ROW = ROW-1
lnNoteLn = lnSvNoteLn 
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= lnMaxRow - 1
    lnSvNoteLn = lnNoteLn
    RETURN(.F.)
  ELSE
    ROW = ROW + 1
    @ ROW,01 SAY MLINE(Note_Mem,lnNoteLn)
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  ROW = ROW + 1
ENDIF
lnSvNoteLn = 1
RETURN(.T.)

*!*************************************************************
*! Name      : lfGetOdHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO get the information of the ORDER header.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfGetOdHdr

*:B804551,1 MHM 12/20/2001 define Curr Key	[Start]
*PRIVATE lcAlias
PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""
*:B804551,1 MHM 12/20/2001 [End]

lcAlias = ALIAS()
SELECT ORDHDR
lcOrder   = Order
lcAccount = Account
lcMulti   = Multi
IF lcMulti = 'Y'
  IF USED(lcMulTOrDT)
    SELECT (lcMulTOrDT)
    USE
  ENDIF
  SELECT ORDLINE
  *B603569,1 AMM start
  *=SEEK('O'+lcOrder)
  *COPY REST TO (gcWorkDir+lcMulTOrDT) WHILE cordtype+order+STR(lineno,6) = 'O'+lcOrder  
  =SEEK(ordhdr.cOrdtype+lcOrder)  
  COPY REST TO (gcWorkDir+lcMulTOrDT) WHILE cordtype+order+STR(lineno,6) = ordhdr.cOrdtype+lcOrder
  *B603569,1 AMM end
  =gfOpenFile(gcWorkDir+lcMulTOrDT,'','EX')
  *B605711,1 TMI [START] Check if sort by Style
  IF lcRpSortBy = 'S'
  *B605711,1 TMI [END  ] Check if sort by Style  
    IF llPack
      INDEX ON cOrdType+Order+Store+Style+STR(LineNo,6)+Pack_Id TAG (lcMulTOrDT)
    ELSE 
      INDEX ON cOrdType+Order+Store+Style+STR(LineNo,6) TAG (lcMulTOrDT)
    ENDIF
  *B605711,1 TMI [START] Sort by line #
  ELSE
    INDEX ON CORDTYPE+ORDER+STORE+STR(LINENO,6) TAG (lcMulTOrDT)
  ENDIF
  *B605711,1 TMI [END  ] Sort by line #
  GO TOP  
  lcStore = Store
  =SEEK('S'+lcAccount+lcStore,'CUSTOMER')  
ENDIF
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)

*:B804551,1 MHM 12/20/2001 get distrepution center [Start]
* Print distribution center address if found 
IF lcMulti = 'Y'
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    *B605910,4 TMI [START] Prints only DC
    lcDCStore = lcStore + ' DC:' + Customer.Store
    *lcDCStore = Customer.Store
    *B605910,4 TMI [END  ] Prints only DC
  ELSE
    lcDcStore = lcStore 
  ENDIF
  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ELSE
  lcDcStore = lcStore 
ENDIF  
*:B804551,1 MHM 12/20/2001 [End]

STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  *B605711,1 TMI [START] Trim Bill To Address
  *lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  *SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  *B605711,1 TMI [END  ] Trim Bill To Address
ENDFOR
*-- SHIP_TO ADDRESS FOR THIS STORE
STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
IF OrdHdr.Alt_ShpTo
  *B605711,1 TMI [START] Trim Addresses
  *lcStName  = OrdHdr.STNAME
  *lcStAddr1 = OrdHdr.cAddress1
  *lcStAddr2 = OrdHdr.cAddress2
  *lcStAddr3 = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
  lcStName  = ALLTRIM(OrdHdr.STNAME)
  lcStAddr1 = ALLTRIM(OrdHdr.cAddress1)
  lcStAddr2 = ALLTRIM(OrdHdr.cAddress2)
  lcStAddr3 = ALLTRIM(OrdHdr.cAddress3)+','+ALLTRIM(OrdHdr.cAddress4)+','+ALLTRIM(OrdHdr.cAddress5)
  *B605711,1 TMI [END  ] Trim Addresses

ELSE
  
  *:B804551,1 MHM 12/20/2001 [Start]
  *lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  *=gfGetAdr('CUSTOMER','','','',1,'')
  *--Get the Ship To adddess except the country.    
  *FOR lnCount = 1 TO ALEN(laAddress,1)
  *  lcCount = STR(laAddress[lnCount,1],1)
  *  lcStAddr&lcCount = lcStAddr&lcCount + IIF(EMPTY(lcStAddr&lcCount),'',',')+;
  *  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  *ENDFOR

    IF !EMPTY(CUSTOMER.DIST_CTR)  
      lcCurrKey = 'S' + Customer.Account + Customer.Store
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    ENDIF

    
    lcStName  = ALLTRIM(IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA))
    *B605711,1 TMI [START] Trim Adresses
    *lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    *lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    *lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
    *lcStAddr4 = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
    *lcStAddr5 = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
    lcStAddr1 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    lcStAddr2 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
    lcStAddr3 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
    lcStAddr4 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
    lcStAddr5 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
    *B605711,1 TMI [END  ] Trim Adresses

    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
    *:B804551,1 MHM 12/20/2001 [End]
ENDIF  
*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia

*B804441,1 MHM 10/25/2001 Get ShipVia at store level [Start]
*lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
IF lcMulti = 'Y'
  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ELSE
    lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
  ENDIF  
ELSE
  lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
ENDIF
*B804441,1 MHM [End]

*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.
laCompAdd1 = IIF(!EMPTY(lcDivLName),lcDivLName,laCompAdd[1])
laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1
NoteLoop = .F.
*E301939,1 RAE [start]
*STORE 0.00 TO lnTotQty,lnTotOrd
STORE 0.00 TO lnTotQty , lnTotOrd , lnTotBook
*E301939,1 RAE [end]
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrintHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO print the information of the ORDER header.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfPrintHdr
PRIVATE lcAlias

lcAlias = ALIAS()
@ 01,01 SAY laCompAdd6 
@ 02,01 SAY laCompAdd1
@ 03,01 SAY laCompAdd2
@ 04,01 SAY laCompAdd3
@ 05,01 SAY laCompAdd4
@ 06,01 SAY laCompAdd5
IF lnPageNo=1
  @ 08,24 SAY 'O R D E R   C O N F I R M A T I O N'
ENDIF
@ 09,60 SAY 'Page : ' +ALLTRIM(STR(lnPageNo))
SELECT OrdHdr
@ 10,03 SAY 'ORDER: '+Order
@ 10,60 SAY 'DATE: '
@ 10,66 SAY ENTERED
IF lnPageNo>1
  @ 11,03 SAY 'STORE: ' + lcStore
ENDIF
@ 11,60 SAY 'PO #: '
@ 11,66 SAY IIF(MultiPO,&lcMulTOrDT..CUSTPO,CUSTPO) 
ROW=13  
IF lnPageNo=1
  @ 13,04 SAY '........ SOLD TO ........'
  @ 13,46 SAY '........ SHIP TO ........'
  @ 14,04 SAY Account
  
  *:B804551,1 MHM 12/20/2001 [Start]
  *@ 14,46 SAY 'STORE: ' + lcStore + '  DEPT:' + Dept
  *B605910,1 TMI [START] Allow printing the department
  *@ 14,46 SAY 'STORE:' + lcDCStore
  *B606406,1 TMI [START] Reposition printing (for department to appear)
  *@ 14,46 SAY 'STORE:' + lcDCStore + '  DEPT:' + Dept 
  *:B804551,1 MHM 12/20/2001 [End]
  
  *@ 15,04 SAY lcBtName
  *@ 15,46 SAY lcStName
  *@ 16,04 SAY lcBtAddr1
  *@ 16,46 SAY lcStAddr1
  *@ 17,04 SAY lcBtAddr2
  *@ 17,46 SAY lcStAddr2
  *@ 18,04 SAY lcBtAddr3
  *@ 18,46 SAY lcStAddr3

  @ 14,40 SAY 'STORE:' + lcDCStore + '  DEPT:' + Dept 
  @ 15,04 SAY lcBtName
  @ 15,40 SAY lcStName
  @ 16,04 SAY lcBtAddr1
  @ 16,40 SAY lcStAddr1
  @ 17,04 SAY lcBtAddr2
  @ 17,40 SAY lcStAddr2
  @ 18,04 SAY lcBtAddr3
  @ 18,40 SAY lcStAddr3
  *B605910,1 TMI [END  ] Reposition printing (for department to appear)
  *B606406,1 TMI [END  ] The department is not printed in the page

  @ 20,00 SAY A
  @ 21,00 SAY B
  @ 22,00 SAY A
  @ 23,01 SAY START
  @ 23,12 SAY COMPLETE
  @ 23,22 SAY lcTermData  
  *B605910,4 TMI [START] Remove Extra spaces in the Ship Via
  *@ 23,38 SAY lcShipVia
  @ 23,38 SAY SUBSTR(lcShipVia,1,15)
  *B605910,4 TMI [END  ] Remove Extra spaces in the Ship Via
  @ 23,54 SAY lcSeason
  @ 23,65 SAY REP1
  @ 23,70 SAY REP2
  ROW=24
ENDIF
@ ROW,00 SAY A
@ ROW+1,00 SAY C
@ ROW+2,00 SAY A
*B803659,1 MHM 09/28/2000 [Start]
ROW = ROW+2
*B803659,1 MHM 09/28/2000 [End]
lcScale     = ' '
lcPrevScale = ' ' 
lnScount    = 0
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfGetStAd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO get the store address.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfGetStAd
PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""

lcAlias = ALIAS()
lcStore   = Store
lcAccount = Account
=SEEK('S'+lcAccount+lcStore,'CUSTOMER')    
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*B804441,1 MHM 10/25/2001 Get ShipVia at store level [Start]
IF ALLTRIM(ORDHDR.ShipVia) = '*'
  lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
ELSE
  lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
ENDIF  
*B804441,1 MHM [End]

*:B804551,3 MHM 01/3/2002 get distrepution center [Start]
* Print distribution center address if found 
IF lcMulti = 'Y'
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    *B605910,4 TMI [START] Prints only DC
    lcDCStore = lcStore + ' DC:' + Customer.Store
    *lcDCStore = Customer.Store
    *B605910,4 TMI [END  ] Prints only DC
  ELSE
    lcDcStore = lcStore 
  ENDIF
  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ELSE
  lcDcStore = lcStore 
ENDIF  
*:B804551,3 MHM 01/3/2002  [End]

lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  *B605711,1 TMI [START] Trim Bill To Address
  *lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  *SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  *B605711,1 TMI [END  ] Trim Bill To Address
ENDFOR
*B603287,1 ADEL 11/18/1999 Initialse address vars

*:B804551,3 MHM 01/3/2002  [Start]
*STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
*lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
*=gfGetAdr('CUSTOMER','','','',1,'')

*--Get the Ship To adddess except the country.    
*FOR lnCount = 1 TO ALEN(laAddress,1)
*  lcCount = STR(laAddress[lnCount,1],1)
*  lcStAddr&lcCount = lcStAddr&lcCount + IIF(EMPTY(lcStAddr&lcCount),'',',')+;
*  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
*ENDFOR

IF !EMPTY(CUSTOMER.DIST_CTR)  
  lcCurrKey = 'S' + Customer.Account + Customer.Store
  =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
ENDIF

lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
lcStAddr4 = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
lcStAddr5 = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

IF !EMPTY(lcCurrKey)
  = SEEK(lcCurrKey , 'CUSTOMER')
ENDIF  
*:B804551,3 MHM 01/3/2002  [End]

*E301939,1 RAE [start]
*STORE 0.00 TO lnTotQty,lnTotOrd
STORE 0.00 TO lnTotQty , lnTotOrd , lnTotBook
*E301939,1 RAE [end]
NoteLoop = .F.    
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrtNtPad
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO print the note pad.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfPrtNtPad
PRIVATE lcAlias

lcAlias = ALIAS()
IF llRpOrdNot
  SELECT NotePad
  SEEK 'B'+lcOrder
  lnMline = 1
  *---------------------------------------------------
  * [NOTEPAD] COMMENT LINES
  *---------------------------------------------------
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMTotLin = MEMLINES(NotePad.MNOTES)
  *B803659,1 MHM 09/28/2000 [START] print note in case of more than one page
  *DO WHILE .T.
  * SELECT NotePad
  * SELECT NotePad
  * IF ROW >= lnMaxRow
  *  *NoteLoop = IIF(lnMline = lnMTotLin,.F.,.T.)   
  *   *EXIT
  * ENDIF
  * IF lnMline > lnMTotLin
  *   EXIT
  * ENDIF
   DO WHILE lnMline <= lnMTotLin
    SELECT NotePad
    IF ROW >= lnMaxRow
      =lfPrnConti()
      =lfPrintHdr()
      ROW = ROW+1
    ENDIF
    *B803659,1 MHM 09/28/2000 [END]

    IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
      @ ROW,04 SAY MLINE(MNOTES,lnMline)
      ROW = ROW+1
    ENDIF
    lnMline = lnMline + 1
  ENDDO
  IF llRpOrdNot
    SET MEMOWIDTH TO lnMemWidth
  ENDIF  
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! DATE      : 02/23/99
*! Purpose   : To get the length to of the color.
*! Refer To  : (C101403)
*!*************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
*!*************************************************************
*! Name      : lfPrnConti
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/26/2000
*! Purpose   : Function to print the continue if the order
*!           : Was more than one page
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None. 
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnConti()
*!*************************************************************
*803659,1
FUNCTION lfPrnConti

*B606435,1 RAE Print 80 characters in * lines instead of 85 characters. [start]
*@ 53,00 SAY REPLICATE('-',85)
@ 53,00 SAY REPLICATE('-',80)
*B606435,1 RAE [end]
@ 54,13 SAY 'C O N T I N U E D ...'
@ 54,69    SAY '**********'
*B606435,1 RAE Print 80 characters in * lines instead of 85 characters. [start]
*@ 55,00 SAY REPLICATE('-',85)
@ 55,00 SAY REPLICATE('-',80)
*B606435,1 RAE [end]
*--End Of lfPrnConti.
