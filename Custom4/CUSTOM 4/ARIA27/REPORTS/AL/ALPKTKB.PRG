*:***************************************************************************
*: Program file  : ALPKTKB.PRG
*: Program desc. : SALES ORDER ALLOCATION PICKING TICKET FORM 'B'.
*: Date          : 11/05/2001
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : 
*: Tracking Job Number: 
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKB
*:***************************************************************************
*: Modifications
*: *B602847,1 KHM 04/29/99 Fixed the bug of not displaying a message when
*: *B602847,1              there were no records to print.
*: *B802227,1 BWA 06/05/99 Get the customer addresses from the right fields
*: *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
*: *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
*: *B802520,1 BWA 15/08/1999 Fix the bug of always print the company name and
*: *B802520,1 address even if the parameter of "Print company name"  is setup to NO.
*: *B802642,1 RAMY 12/2/1999 Avoid print the notes if starting with '*'
*: *B802985,1 WAB 01/23/2000 fix the bug of the form didn't print the line 
*: *B802985,1 WAB            order notes and the problem with the page break.
*: *B802987,4 WAB 01/24/2000 Avoid print the ordlines notes and headr notes 
*: *B802987,4 WAB            if starting with '*'.
*: *B803010,1 SAM 01/30/2000 Fix wrong Sold to addresses bug.
*: *B603398,1 SAM 02/01/2000 Fix not printing all the color code bug.  
*: *B803031,1 SAM 02/08/2000 Print 2 pages for only one style color
*: *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*: *B803311,1 AMH 06/20/2000 Print Size with 5 Characters
*: *B803324,1 MHM 06/25/2000 get Division long name
*: *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store
*: *E301440,1 HBG 07/17/2000 Print Bulk under option grid option :
*: *E301440,1                "Print Bulk Picking Ticket (Yes/No)"
*: *B802470,1 HBG 07/20/2000 Fix some bugs in the P/T (As per SWK)
*: *B802470,1              1 - The telephone for the company and the customer
*: *B802470,1              2 - The sold to sddress
*: *B802470,1              3 - Remove the End of notes
*: *B802470,1              4 - Remove the part of the Bulk P/T
*: *B802470,1			         5 - Print the total merchendise under condition of printing the price 
*: *B803589,1 HBG 08/27/2000 Fixing bug of:
*: *B803589,1                - Page # not prined.
*: *B803589,1                - Scales not aliament.
*: *B803589,1                - Total pieces not prined.
*: *B802962,1 HBG 03/09/2000 Remove the comma from end of 2 letter state code
*: *B803626,1 HBG 10/09/2000 Fixing bug of not printing store #
*: *B803749,1 AAN 10/30/2000 Fixing bug of not Printing ShipVia correctly when be "at store level"
*:							 and not printing PO correctly at multipo
*: *B804182,1 ABD 06/03/2001 When you try to print more than one P/T in a batch using
*: *B804182,1 ABD            Form B , the first P/T will be printed ok but starting 
*: *B804182,1 ABD            from the 2nd , the first line will not be printed.
*: *B605418,1 BWA 01/24/2002 Print the warehous address instead of company address.
*: *B605973,1 BWA 05/28/2002 Add filter to let the user be able to print the Warehouse or the company address.
*: *E302147,1 NAD 05/12/2003 Standard Text Picking Ticket form should not print 0    
*: *B124651,1 NNA 11/02/2004 fix bug that if you changed the style.Desc in the SO this Change not appear
*: *B124651,1 NNA            in the piktkt Report So I Changed in the Report to get the desc. from the ordline
*: *B124651,1 NNA            file instead of Style File 
*: *B131750,1 NNA 04/12/2006 Fix bug that the report display wrong discreptions for styles and not alignment
************************************************************************** 

*******************  From Original *******************
*-- We Will use the temp file of pktkt.prg to make this report work
*B803749,1 AAN declare variabe to hold shipvia code [Start]
lcShipVia=""
*B803749,1 AAN declare variabe to hold shipvia code [Start]

*B803324,1 MHM [start] get Division long name
lcDivLName = ''        && Variable to hold the Division long name
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
*B803324,1 MHM [End] get Division long name

*B603398,1 SAM [Start] get item header 
lcStyTitl = gfItemMask("HI")
*B603398,1  [End]

*B803031,1 [Start] No of order in temp. file 
lnRecord = 1 
*B803031,1 [End]

*E302147,1 NAD (Start)
PRIVATE lnCol ,lnX , lnScaleCnt 
*E302147,1 NAD (END)

PRTPRICE = IIF(llRpStyPrc , 'Y' , 'N')
STORE 0.00 TO XORDTOT, XVALUE , SVALUE
STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3

*B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
*STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE TRIM(laCompAdd[3])                                    TO HLINE4

*B802330,1 BWA [END]

*B802520,1 BWA 15/08/1999 [start]
HLINE2 = IIF(llPrntComp,HLINE2,'')
HLINE3 = IIF(llPrntComp,HLINE3,'')
HLINE4 = IIF(llPrntComp,HLINE4,'')
*B802520,1 [END]
*B803589,1 HBG 08/27/2000 To begin with page # 1 [Begin]
*STORE 0 TO PAGECT
STORE 1 TO PAGECT
ROW = 0
*B803589,1 HBG 08/27/2000 [End]
*B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
*XPHONE = laCompAdd[6]
XPHONE = lcXphone
*B802330,1 BWA [END]

MSGL1 = lcRpMsg1
MSGL2 = lcRpMsg2
MSGL3 = lcRpMsg3
IF LEN(HLINE3) = 0
   STORE HLINE4 TO HLINE3
   *STORE HLINE5 TO HLINE4
   *STORE ' '    TO HLINE5
ENDIF

*--Get Memorey Variables[Start]
XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)
*--Get Memorey Variables[End]
SELECT (lcTmpOrdL)
IF EOF()
  llNoRec = .T.
  *B602847,1 KHM 04/29/99 (Begin) Displaying a message to indicate that no
  *B602847,1              records were found to display.
  =gfModalGen('TRM00052B00000','DIALOG' )
  *B602847,1 KHM 04/29/99 (End)
  SET DEVICE TO SCREEN
  RETURN
ENDIF

RELEASE ALL LIKE M_*

    *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
 A='-------------------------------------------------------------------------------'
 B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
B1='                |        |                  |          |       |              |'

IF XDYELOT_S
 
  *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form  
  *C='   G STYLE   CLR     DESCRIPTION              DYELOT #        PRICE      AMOUNT'

  *B603398,1 SAM [Start] Print Style title.
  *C='   G STYLE       CLR     DESCRIPTION          DYELOT #        PRICE      AMOUNT' 
  C='   G ' + lcStyTitl + ' DESCRIPTION          DYELOT #        PRICE      AMOUNT' 
  *B603398,1 SAM [End  ] Print Style title.

ELSE
  *C='   G STYLE   CLR     DESCRIPTION                              PRICE      AMOUNT'

  *B603398,1 SAM [Start] Print Style title.
  *C='   G STYLE       CLR     DESCRIPTION          DYELOT #        PRICE      AMOUNT'  
  C='   G ' + lcStyTitl + ' DESCRIPTION          DYELOT #        PRICE      AMOUNT'  
  *B603398,1 SAM [End  ] Print Style title.

  *B802330,1 BWA [END]
ENDIF
 
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'

*B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
*F='|                 |           |       |           |           |               |'
 F='|                 |           |       |           |           |'
*B802330,1 BWA [END]

G='|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS| TERMS     | COMMENTS:     |'
H='|  $              | $         | $     | $         |           |               |'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

** DYELOT # IS PRINTED ONLY FOR SYSTEMS USING DYELOTS ***
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890         XXXXXXXXXX     1234.99  123,456.99
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
SELECT ORDHDR
lcWDesc  = CWARECODE
*B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .      
*XBTNAME  = CUSTOMER->BTNAME
SELECT CUSTOMER
*B802227,1 BWA Get customer addresses from the right fields 
*XBTADDR1 = CUSTOMER->BTADDR1
*XBTADDR2 = CUSTOMER->BTADDR2
*XBTADDR3 = TRIM(CUSTOMER->BTCITY) + ' ' +TRIM(CUSTOMER->BTSTATE) +;
            ' ' + CUSTOMER->BTZIP
 
*XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
*XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
*XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2') 
     
*B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
*XBTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')),;
                      gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2') ,;
                      gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')  ;
             + ' , ' +gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))
*B603117,1 [END]
     
*B802227,1 BWA [END]
    
*IF LEN(TRIM(XBTADDR2)) =0
*   XBTADDR2 = XBTADDR3
*   XBTADDR3 = ''
*ENDIF
*B603117,1 BWA [END]
NEWDOC = .T.

CLEAR TYPEAHEAD
XTIME = TIME()
SET DEVICE TO PRINT

SELECT (lcTmpOrdL)
LOCATE

*-->HDM<-- DO WHILE INKEY() <>32
*SELECT (lcTmpOrdL)
*GO TOP
*B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
*SCAN 
 *IF LINENO = 0
  *SELECT (lcTmpOrdL)
  *SKIP
 *ENDIF

*B803031,1 [Start] No of order in temp. file 
XPIKTKT = SPACE(6)
*B803031,1 [End]

SCAN FOR LINENO # 0

  *B605418,1 BWA 01/24/2002 Print the warehous address instead of company address.[START]

  *B605973,1 BWA 05/28/2002 Add filter to let the user be able to print the Warehouse or the company address.[START]
  *HLINE2 = IIF(llPrntComp,gfGetAdr('WAREHOUS' , '' , '' , '' , 1),'')
  *HLINE3 = IIF(llPrntComp,gfGetAdr('WAREHOUS' , '' , '' , '' , 2),'')
  *HLINE4 = IIF(llPrntComp,gfGetAdr('WAREHOUS' , '' , '' , '' , 3),'')
  *HLINE5 = IIF(llPrntComp,TRANSFORM(WAREHOUS.CPHONE,lcPhonPict),'')

  HLINE2 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 1) , HLINE2 ) ,'')
  HLINE3 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 2) , HLINE3 ) ,'')
  HLINE4 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 3) , HLINE4 ) ,'')
  HLINE5 = IIF(llPrntComp , IIF(llRpWareH , TRANSFORM(WAREHOUS.CPHONE,lcPhonPict)   , IIF(TYPE('HLINE5') = 'U' , '', HLINE5) ) ,'')
  *B605973,1 BWA 05/28/2002.[END]

  IF LEN(HLINE3) = 0
    STORE HLINE4 TO HLINE3
    STORE HLINE5 TO HLINE4
    STORE ' '    TO HLINE5
  ENDIF
  *B605418,1 BWA 01/24/2002.[END]

  *B803031,1 [Start] No of order in temp. file 

  IF PIKTKT <> XPIKTKT
    *B803589,1 HBG 08/27/2000 increment page # [Begin]
    PAGECT = 1
    *B803589,1 HBG 08/27/2000 [End] 
    
    lnRecord = RECNO()
    
    *B803324,1 MHM [start] get Division long name    
    =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
    *B803324,1 MHM [end  ] get Division long name
    *(HBG) Initializing (XVALUE) if it is new pick ticket [Begin]
    XVALUE = 0.00
    *(HBG) [End  ]
  ENDIF
  *B803031,1 [End]

  *B802330,1 BWA [END]
  XPIKTKT = PIKTKT
  
  *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
  SELECT CUSTOMER
     
  *B803010,1 Seek either Main account or store [Start]
  *=seek('S' + PIKTKT.ACCOUNT)
  =SEEK(IIF(EMPTY(PIKTKT.STORE),"M","S")+PIKTKT.ACCOUNT+PIKTKT.STORE)
  *B803010,1 Seek either Main account or store [End]

  XBTNAME  = CUSTOMER->BTNAME
  *B802227,1 BWA Get customer addresses from the right fields 
  *XBTADDR1 = CUSTOMER->BTADDR1
  *XBTADDR2 = CUSTOMER->BTADDR2
  *XBTADDR3 = TRIM(CUSTOMER->BTCITY) + ' ' +TRIM(CUSTOMER->BTSTATE) +;
              ' ' + CUSTOMER->BTZIP
  
  XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
  XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2') 
     
  *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
  *B802962,1 HBG 03/09/2000 Remove the comma [Begin]
  *XBTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')),;
                        gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2') ,;
                        gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')  ;
               + ' , ' +gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))

  XBTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')),;
                       gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2') ,;
                       gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')  ;
              + ' ' +  gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))
  *B802962, HBG 03/09/2000 [End]            
  *B603117,1 [END]
     
  *B802227,1 BWA [END]
     
  IF LEN(TRIM(XBTADDR2)) =0
    XBTADDR2 = XBTADDR3
    XBTADDR3 = ''
  ENDIF
  SELECT (lcTmpOrdL)
  *B603117,1 BWA [END]

  *IF LINENO <> 0
   
  *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [Begin]
  *XSTORE = ORDHDR.STORE
  *B803626,1 HBG 10/09/2000 take the store from Temp order line [Begin]
  *XSTORE = ORDLine.STORE
  XSTORE = STORE
  *B803626,1 [End]
  *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [End  ]

  * GET DESCRIPTIONS FOR CODED FIELDS [start]
  SELECT CODES
  SET ORDER TO CODES IN CODES 
  
  *TERMS
  XTERMS = gfCodDes(ORDHDR->CTERMCODE , 'CTERMCODE')

  *SHIPVIA
  *B803749,1 AAN Fixing bug of not Printing ShipVia correctly when be "at store level" [Start]
  lcShipVia = IIF(OrdHdr.ShipVia="*",;
              IIF(SEEK("S"+PIKTKT.ACCOUNT+PIKTKT.STORE,"CUSTOMER"),Customer.ShipVia,"*"),;
              OrdHdr.ShipVia)

  *XSHIPVIA = gfCodDes(ORDHDR->SHIPVIA , 'SHIPVIA')
  XSHIPVIA = gfCodDes(lcShipVia , 'SHIPVIA')
  *B803749,1 AAN Fixing bug of not Printing ShipVia correctly when be "at store level" [End]
  *SPC INST.
  XSPCINST = gfCodDes(ORDHDR->SPCINST , 'SPCINST')

  SEEK 'Z' + ORDHDR->SEASON
  XSEASON  = gfCodDes(ORDHDR->SEASON , 'SEASON')

  *B605418,1 BWA 01/24/2002 Print the warehous address instead of company address.[START]      
  *HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)

  *B605973,1 BWA 05/28/2002 Add filter to let the user be able to print the Warehouse or the company address.[START]
  *HLINE1 = LEFT(WAREHOUS.CDESC,30)
  HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
  HLINE1 = IIF(llRpWareH , LEFT(WAREHOUS.CDESC,30) , HLINE1 )
  *B605973,1 BWA 05/28/2002.[END]

  *B605418,1 BWA 01/24/2002.[END]

  *B802520,1 BWA 15/08/1999 [start]
  HLINE1 = IIF(llPrntComp,HLINE1,'')
  *B802520,1 [END]


  STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4

  XPIECES = 0
  * GET DESCRIPTIONS FOR CODED FIELDS [end]
  
  *--Get Alt Ship to if exsits if !exist account
  SELE OrdHdr
  lcDistCntr = ""
    
  IF Alt_ShpTo
    
    *B803010,1
    *B802227,1 BWA Get customer addresses from the right fields 
    *XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    *XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    *XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )

    lcShpTName  = OrdHdr.STName   
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3
    laShipTo[4] = OrdHdr.cAddress4
    laShipTo[5] = OrdHdr.cAddress5

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
      
      
    *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
    *XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    XSTADDR3 = TRIM(laShipTo[3]) 
    XSTADDR4 = IIF(EMPTY(TRIM(laShipTo[4])),TRIM(laShipTo[5]),;
                          TRIM(laShipTo[4]) + ' ' + laShipTo[5])
    *B603117,1 [END]
    *B802227,1 BWA [END]
    *B803010,1
      
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT CUSTOMER
    *lnRecNo = RECNO()       && Variable to hold the record number.
    IF !EMPTY(Dist_Ctr) 
      lcDistCntr = ALLTRIM(Dist_Ctr)
      *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
      *=seek('S' + XACCOUNT + lcDistCntr)        
      =seek('S' + PIKTKT.ACCOUNT + lcDistCntr)        
      *B603117,1 [END]

    ENDIF
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
      
    *B802227,1 BWA Get customer addresses from the right fields 
    *XSTADDR1 = laShipTo[1]
    *XSTADDR2 = laShipTo[2]
    *XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
      
    *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
    *B802962, HBG 03/09/2000 Remove the comma [Begin]
    XSTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )) ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )  ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )   ;
                    +','+gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ))
    XSTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )) ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )  ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )   ;
                     +''+gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ))
    *B802962, HBG 03/09/2000 [End]                  
    *B603117,1 [END]
            
    *B802227,1 BWA [END]
    IF !EMPTY(lcDistCntr) 
*     GO lnRecNo
    ENDIF
    *B801524,1 OWA 03/19/98 (End) 
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  *------Begin Say ((Printing))
  SELECT (lcTmpOrdL)
  * LINE 1
    
     
  @ 00,01 SAY 'P I C K I N G   T I C K E T'
  @ 00,35 SAY HLINE1
  *B803589,1 HBG 08/27/2000 moving the date position[Begin]
  *@ 00,70 SAY DATE()
  @ 00,68 SAY DATE()
  *B803589,1 HBG 08/27/2000 [End]
  @ 01,01 SAY 'TICKET: '+ PIKTKT       && XPIKTKT
  *@ 01,16 SAY XDATE
  @ 01,16 SAY PIKDATE
  @ 01,35 SAY HLINE2
  *B803589,1 HBG 08/27/2000 Print page # [Begin]
  @ 01,68 SAY 'PAGE  '+STR(PAGECT,3)
  *B803589,1 HBG 08/27/2000 [End]
  XORDER = ORDER
  @ 02,01 SAY 'ORDER : '+ORDER    &&TTT
  @ 02,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
  @ 02,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
  @ 02,35 SAY HLINE3
  *B803589,1 HBG 08/27/2000 moving the time position[Begin]
  *@ 02,70 SAY XTIME
  @ 02,68 SAY XTIME
  *B803589,1 HBG 08/27/2000 [End]
  @ 03,01 SAY IIF(!EMPTY(ORDHDR.APPROVAL),'APPRVL: '+ORDHDR.APPROVAL,'') 
    
  @ 03,35 SAY HLINE4
  *B800087,1 TAK 08/20/95
  IF llWareHous .AND. !EMPTY(lcWDesc)
    @ 04,01 SAY lcWDesc
  ENDIF
  *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
  *@ 04,35 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
    
   
  *@ 04,35 SAY XPHONE SIZE 1,16
  *B802520,1 BWA 15/08/1999 [start]
  *B802470,1 Fix the company phone format
  *@ 04,35 SAY IIF(llPrntComp,LEFT(XPHONE,16),'')

  *B605418,1 BWA 01/24/2002 Print the warehous address instead of company address.[START]
  *@ 04,35 SAY IIF(llPrntComp,XPHONE,'')
  *B605418,1 BWA 01/24/2002.[END]

  *B802470,1 (END)
  *B802520,1 [END]
   
    
  *@ 04,35 SAY HLINE5
  *B500772,1 HISH 06/29/95.  ( End )
  @ 06,04 SAY '.....SOLD TO .....'
  @ 06,46 SAY '.....SHIP TO .....'
    
  @ 07,04 SAY ORDHDR.ACCOUNT
    
  
  *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
  *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
  *@ 07,10 SAY XPHONE SIZE 1,16
  *B802470,1 Fix the customer phone format
  *@ 07,10 SAY CUSTOMER.PHONE1 SIZE 1,16
  @ 07,10 SAY TRANSFORM(ORDHDR.PHONE, lcPhonPict)
  *B802470,1 (END)
  *B603117,1 BWA [END]
       
    
  *@ 07,10 SAY PHONE
  *B500772,1 HISH 06/29/95.  ( End )
  IF LEN(TRIM(XSTORE))<>0 .AND. XSTORE<>'*'
    *B801524,1 OWA 03/19/98 (Begin) Printed the distribution center if 
    *B801524,1              exists instead of the store.
    *@ 07,46 SAY 'STORE#: ' + XSTORE
    @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
    *B801524,1 OWA 03/19/98 (End) 
  ENDIF
  @ 08,04 SAY XBTNAME
  @ 08,46 SAY XSTNAME
   
  *B802227,1 BWA Get customer addresses from the right fields 
  *@ 09,04 SAY laSoldTo[1]
  *@ 09,46 SAY laShipTo[1]
   
  
  @ 09,04 SAY XBTADDR1
  @ 09,46 SAY XSTADDR1

  *@ 10,04 SAY laSoldTo[2]
  *@ 10,46 SAY laShipTo[2]

  @ 10,04 SAY XBTADDR2
  @ 10,46 SAY XSTADDR2
   
  *@ 11,04 SAY laSoldTo[3]
  *@ 11,46 SAY laShipTo[3]
     
  @ 11,04 SAY XBTADDR3
  @ 11,46 SAY XSTADDR3
  *B802227,1 BWA [END]
   
  *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
  @ 12,04 SAY XBTADDR4
  @ 12,46 SAY XSTADDR4
  *B603117,1 [END]
   
  @ 13,00 SAY A
  @ 14,00 SAY B
  @ 15,00 SAY A

  SELECT ORDHDR
  *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
  *@ 16,00 SAY B1
  *@ 16,01 SAY XSHIPVIA
  *@ 16,18 SAY SUBSTR(XSEASON,1,7) 
  *@ 16,26 SAY XSPCINST
  *@ 16,47 SAY ORDLINE->STORE
  *@ 16,58 SAY DEPT
  *@ 16,66 SAY IIF(MultiPO , lcCustPO , CUSTPO)  &&TAK 08/22/94
  
  @ 16,01 SAY LEFT(XSHIPVIA,15) + '|'
  @ 16,18 SAY LEFT(XSEASON,7) + '|'
  @ 16,26 SAY LEFT(XSPCINST,18) + '|'
  *B803626,1 HBG 10/09/2000 Print the store from Temp order line [Begin]
  *@ 16,46 SAY ORDLINE->STORE + ' ' + '|'
  @ 16,46 SAY &lcTmpOrdL..STORE + ' ' + '|'
  *B803626,1 HBG [End]
  @ 16,58 SAY DEPT + '|'
   
  *802985,1 ARH 01/25/2000 (Begin) Fixed the variable LCCUTPO not found bug
  *802985,1 ARH            in case of multi customer PO orders
  *@ 16,65 SAY LEFT(IIF(MultiPO , lcCustPO , CUSTPO),13) +'|'

  *B803749,1 AAN Fix bug not print PO correctly[Start]
  *@ 16,65 SAY LEFT(IIF(MultiPO , OrdLine.CustPo , CUSTPO),13) +'|'
  @ 16,65 SAY LEFT(&lcTmpOrdH..Custpo,13) +'|'
  *B803749,1 AAN Fix bug not print PO correctly[End]
  *802985,1 ARH 01/25/2000 (End)
   
  *B802330,1 BWA [END]
  @ 17,00 SAY A
  @ 18,00 SAY C
  @ 19,00 SAY A
  ROW=20
  *--Header Has Finished
  *--Begin Lines Loop
  XTOTQTY = 0
  SELECT (lcTmpOrdL)
  
  *B131750,1 NNA 04/12/2006 (Begin) Set Relation into ordline file to get the correct description
  SET RELATION OFF INTO ORDLINE
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
  *B131750,1 NNA (End)
  
  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0
       
    *DO CASE
    *  CASE EOF()
    *    NEWDOC = .T.
    *    EXIT
    *  CASE STORE # XSTORE
    *    NEWDOC = .F.
    *    EXIT
    *  CASE TOTPIK<=0
    *    SKIP
    *    LOOP
    *  CASE ROW>= 47
    *    NEWDOC=.F.
    *    EXIT
    *ENDCASE
    SELECT (lcTmpOrdL)
    lnCurRec = RECNO()

    SELECT STYLE

    *B124651,1 NNA 11/02/2004 (Begin) Get Style Desc. from ordline file instead of Style File
    *XSTYDESC =  DESC

    *B131750,1 NNA 04/12/2006 (Begin) Get only the first 20 Chr. as in Style.Desc
    *XSTYDESC =  ORDLINE.DESC1
    XSTYDESC = SUBSTR(ORDLINE.DESC1,1,20)
    *B131750,1 NNA (End)
  
    *B124651,1 NNA (End)
    
    *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form       
    *@ ROW,03 SAY CSTYGROUP
    @ ROW,03 SAY &lcTmpOrdL..GROUP
    *-- MFM 06/27/94.
    *@ ROW,05 SAY SUBSTR( STYLE, 1, 7 )
    *@ ROW,13 SAY SUBSTR( COLOR, 1, 3 )
      
    *B603398,1 [Start] display the complete style 
    *@ ROW,05 SAY SUBSTR( STYLE, 1, 12)
    *@ ROW,17 SAY SUBSTR( STYLE,14,20)
    @ ROW,05 SAY STYLE
    *B603398,1 [End]
      
    SELECT (lcTmpOrdL)
    *--Say Dyelot[start]
    *@ ROW,17 SAY XSTYDESC
    @ ROW,25 SAY XSTYDESC
    *B802330,1 BWA [END]
      
    IF XDYELOT_S
      SELECT (lcTmpOrdL)
      @ ROW,46 SAY DYELOT
    ENDIF
    *--Say Dyelot[end]
 
    IF PRTPRICE ='Y'
      XAMOUNT = PRICE * TOTPIK
      @ ROW,60 SAY PRICE        PICTURE '9999.99'
      @ ROW,69 SAY XAMOUNT      PICTURE '999,999.99'
    ENDIF
      
    Row = Row + 1
    *B803589 HBG 08/27/2000 This variable won't be used [Begin]
    *Y   = '   '
    *B803589 HBG 08/27/2000 [End]
    lcScl = SPACE(0)
    
    *E302147,1 NAD (Start)
    lnScaleCnt = 8 
    *E302147,1 NAD (End)
    
    *B803311,1 AMH 06/20/2000 Print Size with 5 Characters (Start)
    *B803589 HBG 08/27/2000 Fixing bug of scales not aliament[Begin]
    *IF SEEK('S'+Style.SCALE,'SCALE') 
       *lcScl=Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')+Y+;
                     PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')+Y+;
                     PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')+Y+;
                     PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')   
      
    *ENDIF
    IF SEEK('S'+Style.SCALE,'SCALE') 
      lcScl=' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,5),5,' ')   
      
      *E302147,1 NAD (Start)
      lnScaleCnt = Scale.Cnt
      *E302147,1 NAD (End)
      
    ENDIF
    *lcScl = GETSCALE(SCALE,SPACE(1) )
    *@ ROW,11 SAY lcScl
    *@ ROW,12 SAY lcScl
    @ ROW,11 SAY lcScl
    *B803589 HBG 08/27/2000 Fixing bug of scales not aliament[End]
      
    *B803311,1 AMH 06/20/2000 Print Size with 5 Characters (End)
    *--Actual Lines Printing[start]
      
    SELECT (lcTmpOrdL)
    ROW = ROW+2

    @ ROW,05 SAY 'ORDR:'

    *E302147,1 NAD (SART)
    lnCol =6
    FOR lnX =1 to lnScaleCnt 
      @ ROW,lnCol+(6*lnx) SAY EVAL('QTY'+STR(lnx,1))     PICTURE '99999'
    NEXT lnX
   * @ ROW,12 SAY QTY1     PICTURE '99999'
   * @ ROW,18 SAY QTY2     PICTURE '99999'
   * @ ROW,24 SAY QTY3     PICTURE '99999'
   * @ ROW,30 SAY QTY4     PICTURE '99999'
   * @ ROW,36 SAY QTY5     PICTURE '99999'
   * @ ROW,42 SAY QTY6     PICTURE '99999'
   * @ ROW,48 SAY QTY7     PICTURE '99999'
   * @ ROW,54 SAY QTY8     PICTURE '99999'   
   *E302147,1 NAD (END)
   
     @ ROW,61 SAY TOTQTY   PICTURE '999999'
    * SUM ORDER TOTALS
    XTOTQTY  = XTOTQTY+ TOTQTY
    XVALUE   = XVALUE + TOTPIK * PRICE
      
    *PRTPIK=.F.
    *PRTPIK = IIF(PIK1 # QTY1, .T., PRTPIK)
    *PRTPIK = IIF(PIK2 # QTY2, .T., PRTPIK)
    *PRTPIK = IIF(PIK3 # QTY3, .T., PRTPIK)
    *PRTPIK = IIF(PIK4 # QTY4, .T., PRTPIK)
    *PRTPIK = IIF(PIK5 # QTY5, .T., PRTPIK)
    *PRTPIK = IIF(PIK6 # QTY6, .T., PRTPIK)
    *PRTPIK = IIF(PIK7 # QTY7, .T., PRTPIK)
    *PRTPIK = IIF(PIK8 # QTY8, .T., PRTPIK)
    PRTPIK = PIK1 # QTY1 .OR. PIK2 # QTY2 .OR. PIK3 # QTY3 .OR. ;
             PIK4 # QTY4 .OR. PIK5 # QTY5 .OR. PIK6 # QTY6 .OR. ;
             PIK7 # QTY7 .OR. PIK8 # QTY8
     
    IF PRTPIK
      ROW = ROW+1
      @ ROW,05 SAY 'ALLO:'
      
    *E302147,1 (START)
    lnCol =6
    FOR lnX =1 to lnScaleCnt 
     @ ROW,lnCol+(6*lnx) SAY EVAL('PIK'+STR(lnx,1))     PICTURE '99999'
    NEXT lnX
      *@ ROW,12 SAY PIK1     PICTURE '99999'
      *@ ROW,18 SAY PIK2     PICTURE '99999'
      *@ ROW,24 SAY PIK3     PICTURE '99999'
      *@ ROW,30 SAY PIK4     PICTURE '99999'
      *@ ROW,36 SAY PIK5     PICTURE '99999'
      *@ ROW,42 SAY PIK6     PICTURE '99999'
      *@ ROW,48 SAY PIK7     PICTURE '99999'
      *@ ROW,54 SAY PIK8     PICTURE '99999'
      
      *E302147,1(END)
    ENDIF
    @ ROW,61 SAY TOTPIK   PICTURE '999999'         
    XPIECES = XPIECES + TOTPIK
     
    ROW =ROW+1
    @ ROW,05 SAY 'PICK:'
     *E302147,1(Start)
    lnCol =6
    FOR lnX =1 to lnScaleCnt 
      @ ROW,lnCol+(6*lnx) SAY '_____'
    NEXT lnX
    
    *@ ROW,12 SAY '_____'
    *@ ROW,18 SAY '_____'
    *@ ROW,24 SAY '_____'
    *@ ROW,30 SAY '_____'
    *@ ROW,36 SAY '_____'
    *@ ROW,42 SAY '_____'
    *@ ROW,48 SAY '_____'
    *@ ROW,54 SAY '_____'
    *@ ROW,62 SAY '_____'
    ROW=ROW+2
    *E302147,1(END)
    *B802985,1 WAB - if the detail printed line  more than 58 , we must eject and
    *B802985,1 WAB - reprint the header again.
    *B802985,1 WAB - check if there are line note in order line and print it 
    *B802985,1 WAB - if the option of print ordline notes is yes.
    IF ROW > 58
      *-- call function to print header
       
      *B803589,1 HBG 08/27/2000 Start new page no.[Begin]
      PAGECT = PAGECT + 1
      *B803589,1 [End] 
        
      =lfPrntHdr()
    ENDIF
      

    *B802987,4 WAB - check if the ordline.note_mem have any lines don't strat with '*'
    *IF llRpOrdLNt  .AND. !EMPTY(ORDLINE.Note_Mem) 
    IF llRpOrdLNt  .AND. !EMPTY(ORDLINE.Note_Mem) .AND. lfFndLine(ORDLINE.Note_Mem)
    *B802987,4 WAB - END
      @ ROW,02 SAY '*-- Line Note Pad --*'
      ROW = ROW + 1
      lnMemoLnNo = 0
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      *-- get no of lines in ordline.note_mem
      lnMemolins = MEMLINES(ORDLINE.Note_Mem)
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
          lnMemoLnNo = lnMemoLnNo + 1
          lcNotes = MLINE(ORDLINE.Note_Mem,lnMemoLnNo)
          *B802987,4 WAB - Avoid print the ordlines notes if starting with '*'.
          IF LEFT(lcNotes , 1) # '*'                        
          *B802987,4 WAB - END
            IF ROW > 58
              *-- call function to print header             
              
              *B803589,1 HBG 08/27/2000 Start new page no.[Begin]
              PAGECT = PAGECT + 1
              *B803589,1 [End] 
                
              =lfPrntHdr()
            ENDIF
            @ ROW,02 SAY lcNotes
            ROW = ROW + 1
            *B802987,4 WAB - end if -> if LEFT(lcNotes , 1) # '*' 
          ENDIF
          *B802987,4 WAB -END
        ENDDO
      ENDIF 
      @ ROW,02 SAY '*-- END OF Line NotePad --*'
      ROW = ROW + 2
    ENDIF
    *B802985,1 WAB - END
    SELECT (lcTmpOrdL)
    SKIP
  ENDDO
  SELECT (lcTmpOrdL)
  GOTO lnCurRec
  *------------------------
  * [NOTEPAD] COMMENT LINES
  *------------------------
  *B802985,1 WAB - set the right variable that holding the accepting of user 
  *B802985,1 WAB - to print order line note
  *PRTNPAD = IIF(llRpOrdLNt ,'Y','N')
  PRTNPAD = IIF(llRpOrdNot,'Y','N')
  *B802985,1 WAB - END
  IF PRTNPAD='Y' .AND. NEWDOC
    SELECT NOTEPAD
    IF SEEK('B' + XORDER)
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      lnMemoLnNo = 0
      lnMemolins = 0
      ROW = ROW +1
      @ ROW,02 SAY '*-- N O T E P A D --*'
      ROW = ROW + 1
      lnMemolins = MEMLINES(mNotes)
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
          IF ROW > 51                 &&62
            ROW = ROW + 1
            @ ROW,2 SAY 'CONTINUED NEXT PAGE ...'
            *B802987,4 WAB - Avoid print the order headr notes if starting with '*'.
            *@ 54,01 SAY ORDHDR->NOTE1
            *@ 54,41 SAY ORDHDR->NOTE2
            *B803589 HBG 08/27/2000 Printing Total pieces[Begin]
            *@ 54,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
            *@ 54,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
            @ 53,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
            @ 53,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
            *B802987,4 WAB - END
       	    @ 54,50 SAY 'TOTPIK'
       	    @ 54,61 SAY XPIECES
       	    *B803589 HBG 08/27/2000 [End]
        	@ 55,00 SAY A
        	@ 56,00 SAY E
    	    @ 57,00 SAY F
		    @ 57,64 SAY SUBSTR(XSHIPVIA,1,14) + '|'
			@ 58,00 SAY A
      		@ 59,00 SAY G
			 
            *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form
		 	*@ 60,00 SAY H
            *IF XSTORE = ORDHDR.STORE .AND. !NEWDOC
            *  @ 61,05 SAY '*******.**'
            *ELSE
            *  @ 61,05 SAY XVALUE     PICTURE '9999999.99'
            *  XVALUE   = 0.00
            *ENDIF
            ROW = 60

            *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [Begin]
            *IRE = ORDHDR.STORE .AND. !NEWDOC
            *B803626,1 HBG 10/09/2000 take the store from Temp order line [Begin]
            *IF XSTORE = ORDLine.STORE .AND. !NEWDOC
            IF XSTORE = &lcTmpOrdL..STORE .AND. !NEWDOC
            *B803626,1 HBG [End]
            *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [End  ]
              @ 60,00 SAY '|' 
              @ 60,02 SAY '$' 
              @ 60,05 SAY '*******.**'
            ELSE
              @ 60,00 SAY '|' 
              @ 60,02 SAY '$' 
              *B802470,1 Print the total merchendise under condition
              *@ 60,05 SAY XVALUE     PICTURE '9999999.99' 
              IF PRTPRICE ='Y'
                @ 60,05 SAY XVALUE     PICTURE '9999999.99' 
              ENDIF
              *B802470,1 (End)
              *(HBG) comment this line to fix the bug of : if the notes exceed one page,the
              *(HBG) value of the price is zero [Begin]
              *XVALUE   = 0.00
              *(HBG) [End]
            ENDIF
            *@ 61,52 SAY LEFT(XTERMS,10)
            *@ 62,00 SAY A
            *@ 63,00 SAY MSGL1
            *@ 64,00 SAY MSGL2
            *@ 65,00 SAY MSGL3
            @ 60,18 SAY '|' 
            @ 60,30 SAY '|' 
            @ 60,38 SAY '|' 
            @ 60,50 SAY '|' 
            @ 60,52 SAY LEFT(XTERMS,10) 
            @ 60,61 SAY '|' 
            @ 60,78 SAY '|' 
            @ 61,00 SAY A
            @ 62,00 SAY MSGL1
            @ 63,00 SAY MSGL2
            ROW = 64
            @ 64,00 SAY MSGL3
            ROW = 0
            *B802330,1 BWA [END]
            SELECT ORDHDR         
             
            @ 00,01 SAY 'P I C K I N G   T I C K E T'
            @ 00,35 SAY HLINE1
            *B803589,1 HBG 08/27/2000 moving the date position[Begin]
            *@ 00,70 SAY DATE()
            @ 00,68 SAY DATE()
            *B803589,1 HBG 08/27/2000 [End]
            @ 01,35 SAY HLINE2
            *B803589,1 HBG 08/27/2000 Print page # [Begin]
            @ 01,68 SAY 'PAGE  '+STR(PAGECT,3)
            *B803589,1 HBG 08/27/2000 [End]
            
            @ 02,01 SAY 'TICKET: '+XPIKTKT
            *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
            *@ 02,16 SAY XDATE
            @ 02,16 SAY PIKTKT.DATE
            *B802330,1 BWA [END]
            @ 02,35 SAY HLINE3
            *B803589,1 HBG 08/27/2000 moving the time position[Begin]
            *@ 02,70 SAY XTIME
            @ 02,68 SAY XTIME
            *B803589,1 HBG 08/27/2000 [End]
            XORDER=ORDER
            @ 03,01 SAY 'ORDER : '+ORDER
            @ 03,16 SAY SUBSTR(DTOC(START),1,5)
            @ 03,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
            @ 03,35 SAY HLINE4
            @ 04,01 SAY IIF(!EMPTY(ORDHDR.APPROVAL),;              
                            'APPRVL: '+ORDHDR.APPROVAL,'') 

            *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
            *@ 04,35 SAY XPHONE SIZE 1,16
             
            *B802520,1 BWA 15/08/1999 [start]

            *B605418,1 BWA 01/24/2002 Print the warehous address instead of company address.[START]
            *@ 04,35 SAY IIF(llPrntComp,LEFT(XPHONE,16),'')
            *B605418,1 BWA 01/24/2002.[END]

            *B802520,1 [END]
             
            *@ 04,35 SAY HLINE5
            *B500772,1 HISH 06/29/95.  ( End )

            @ 06,04 SAY '.....SOLD TO .....'
            @ 06,46 SAY '.....SHIP TO .....'
            @ 07,04 SAY ACCOUNT

            *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
            *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
            *@ 07,10 SAY XPHONE SIZE 1,16
            *B802470,1 Fix the customer phone format
            *@ 07,10 SAY CUSTOMER.PHONE1 SIZE 1,16
            @ 07,10 SAY TRANSFORM(ORDHDR.PHONE, lcPhonPict)
            *B802470,1 (END)
            *B603117,1 BWA [END]
             
            *@ 07,10 SAY PHONE
            *B500772,1 HISH 06/29/95.  ( End )
             

            IF LEN(TRIM(XSTORE))<>0 .AND. XSTORE<>'*'
              *B801524,1 OWA 03/19/98 (Begin) Printed the distribution  
              *B801524,1              center if exists instead of the store.
              *@ 07,46 SAY 'STORE#: ' + XSTORE
              @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,;
                                           lcDistCntr)
              *B801524,1 OWA 03/19/98 (End) 
            ENDIF

            @ 08,04 SAY XBTNAME
            @ 08,46 SAY XSTNAME
             
                          
            @ 09,04 SAY XBTADDR1
            @ 09,46 SAY XSTADDR1

            @ 10,04 SAY XBTADDR2
            @ 10,46 SAY XSTADDR2

            @ 11,04 SAY XBTADDR3
            @ 11,46 SAY XSTADDR3
             
            *B603117,1 BWA 08/16/99 Fix the bug of the ship to and bill to address .
            @ 12,04 SAY XBTADDR4
            @ 12,46 SAY XSTADDR4
            *B603117,1 [END]

            @ 13,00 SAY A
            @ 14,00 SAY B
            @ 15,00 SAY A

            SELECT ORDHDR
            *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
            *@ 16,00 SAY B1
            *@ 16,01 SAY XSHIPVIA
            *@ 16,18 SAY SUBSTR(XSEASON,1,7) 
            *@ 16,26 SAY XSPCINST
            *@ 16,47 SAY &LNTEMP->STORE
            *@ 16,58 SAY DEPT
            *@ 16,66 SAY IIF(MultiPO,lcCustPO,CUSTPO)  &&TAK 08/22/94
             
            @ 16,01 SAY LEFT(XSHIPVIA,15) + '|'
            @ 16,18 SAY LEFT(XSEASON,7) + '|'
            @ 16,26 SAY LEFT(XSPCINST,18) + '|'
            *B803626,1 HBG 10/09/2000 Print the store from Temp order line [Begin]
            *@ 16,46 SAY ORDLINE->STORE + ' ' + '|'
            @ 16,46 SAY &lcTmpOrdL..STORE + ' ' + '|'
            *B803626,1 HBG [End]
            @ 16,58 SAY DEPT + '|'
             
            *802985,1 ARH 01/25/2000 (Begin) Fixed the variable LCCUTPO not 
            *802985,1 ARH            found bug in case of multi customer PO orders

            *B803749,1 AAN Fix bug not print PO correctly[Start]
            *@ 16,65 SAY LEFT(IIF(MultiPO , lcCustPO , CUSTPO),13) +'|'
            @ 16,65 SAY LEFT(&lcTmpOrdH..Custpo,13) +'|'
            *B803749,1 AAN Fix bug not print PO correctly[End]
            *802985,1 ARH 01/25/2000 (End)
             
            *B802330,1 BWA [END]
            @ 17,00 SAY A
            @ 18,00 SAY C
            @ 19,00 SAY A
            ROW=20

          ENDIF
          lnMemoLnNo = lnMemoLnNo + 1
          lcNotes = MLINE(NOTEPAD.mNotes,lnMemoLnNo)

          *B802642,1 RAMY Add these lines to remove the commented lines from the notepad [start]
          *@ ROW,02 SAY lcNotes
          IF LEFT(lcNotes , 1) # '*'
            @ ROW,02 SAY lcNotes
            ROW = ROW + 1
          ENDIF
          *B802642,1 RAMY [end]
        ENDDO
      ENDIF
      *B802470,1 Remove this line 
      *@ ROW,02 SAY '*-- END OF NOTEPAD --*'
      *B802470,1 (END)
      *lnMemoLnNo = 0      
      *lnMemolins = 0
      *SET MEMOWIDTH TO (lnMemoWdth)
    ENDIF
  ENDIF
  ROW = 54
  *B802987,4 WAB - Avoid print the order headr notes if starting with '*'.
  *@ 54,01 SAY ORDHDR->NOTE1
  *@ 54,41 SAY ORDHDR->NOTE2
  *B803589 HBG 08/27/2000 Moving Notes line upove[Begin]
  *@ 54,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
  *@ 54,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
  @ 53,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
  @ 53,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
  *B803589 HBG 08/27/2000 [End]
  *B802987,4 WAB - END

  SELECT (lcTmpOrdL)
  *B803589 HBG 08/27/2000 Printing Total pieces[Begin]
  @ 54,50 SAY 'TOTPIK'
  @ 54,61 SAY XPIECES
  *B803589 HBG 08/27/2000 [End]
  @ 55,00 SAY A
  @ 56,00 SAY E
  *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form    
  @ 57,00 SAY F
  *@ 57,64 SAY SUBSTR(XSHIPVIA,1,10)
  @ 57,64 SAY 	LEFT(XSHIPVIA,14) + '|'
  *B802330,1 BWA [END]
  @ 58,00 SAY A
  @ 59,00 SAY G
   
  *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form
  *@ 60,00 SAY H
  *IF PRTPRICE ='Y'   &&B500816,1 TAK 06/28/95 Added the condition.
  *  IF XSTORE = ORDHDR.STORE .AND. !NEWDOC
  *     @ 61,05 SAY '*******.**'
  *   ELSE
  *     @ 61,05 SAY XVALUE     PICTURE '9999999.99'
  *     XVALUE   = 0.00
  *   ENDIF
  *ENDIF
  *@ 61,52 SAY SUBSTR(XTERMS,1,10)
  *@ 62,00 SAY A
  *@ 63,00 SAY MSGL1
  *@ 64,00 SAY MSGL2
  *@ 65,00 SAY MSGL3
  *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [Begin]
  *IF XSTORE = ORDHDR.STORE .AND. !NEWDOC
  *B803626,1 HBG 10/09/2000 take the store from Temp order line [Begin]
  *IF XSTORE = ORDLine.STORE .AND. !NEWDOC
  IF XSTORE = &lcTmpOrdL..STORE .AND. !NEWDOC
  *B803626,1 HBG [End]
  *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [End  ]

    @ 60,00 SAY '|' 
    @ 60,02 SAY '$' 
    @ 60,05 SAY '*******.**'
  ELSE
    @ 60,00 SAY '|' 
    @ 60,02 SAY '$' 
    *B802470,1 Printing the total merchendise under condition
    *@ 60,05 SAY XVALUE     PICTURE '9999999.99' 
    IF PRTPRICE ='Y'
      @ 60,05 SAY XVALUE     PICTURE '9999999.99' 
    ENDIF  
    *B802470,1 (End)
    XVALUE   = 0.00
  ENDIF
  @ 60,18 SAY '|' 
  @ 60,30 SAY '|' 
  @ 60,38 SAY '|' 
  @ 60,50 SAY '|' 
  @ 60,52 SAY LEFT(XTERMS,10) 
  @ 60,61 SAY '|' 
  @ 60,78 SAY '|' 
  @ 61,00 SAY A
  @ 62,00 SAY MSGL1
  @ 63,00 SAY MSGL2
  @ 64,00 SAY MSGL3
  *B802330,1 BWA [END]
  
  IF !NEWDOC
    LOOP
    *B802470,1 Add this condition for the priting for the next page 
  ELSE
    *B804182,1 ABD When you try to print more than one P/T in a batch using
    *B804182,1 ABD Form B , the first P/T will be printed ok but starting 
    *B804182,1 ABD From the 2nd , the first line will not be printed that 
    *B804182,1 ABD Because we should skip in case this line # = 0. [Begin]
    IF LINENO = 0
      SKIP
    ENDIF
    *B804182,1 ABD [End]
    *B802470,1 (End) 
  ENDIF
  SELE PIKTKT
  *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
  *REPL PRTFLAG WITH 'P'
  *B603487,1 BWA 03/08/2000 [END]

  *---Print Piktkt details
  *   DO WHILE INKEY() <>32 .AND. !EOF()
  SELECT (lcTmpOrdL)
  *   lnRecNo = RECNO()

  *E301440,1 HBG 07/17/2000 IF option "print bulk pick ticket" in OG is 'Yes'  
  *E301440,1                go to th first line in temp order line file and print 
  *E301440,1                the block of Bulk Pick ticket. [Begin]
  IF llRpPrtBlk
    *E301440,1 HBG 07/17/2000
    *B803031,1 [Start] 
    ROW = 65 && To print in next page 
    IF BETWEEN(lnRecord,1,RECCOUNT())
      GOTO lnRecord && right record in temp. file
    ENDIF
    *B803031,1 [End] 

    DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0
      *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
      *B803031,1 [Start]  increment record no. if not eof
      *ROW = 65

      *IF lnRecord <= RECCOUNT() 
      *  lnRecord = lnRecord + 1  && increment record no.
      *ENDIF    

      *B803031,1 [End] 
      *B802330,1 BWA [END]
      IF ROW > 55
        PAGECT = PAGECT+1
                            
        *Ren
        *@ 01,00 SAY 'PICKING TICKET FORM S'
        *Ren end
        @ 01,30 SAY 'BULK PICKING TICKET'
        @ 01,65 SAY 'PAGE  '+STR(PAGECT,3)
        @ 02,00 SAY DATE()
        @ 02,65 SAY 'TIME  ' + XTIME
        @ 04,00 SAY '-------------------------------------------------------------------------------'
        *B603398,1 [Start]  print style header 
        *@ 05,00 SAY '     STYLE   CLR     DESCRIPTION                                               '
        @ 05,00 SAY '   '+lcStyTitl+' DESCRIPTION                                               '
        *B603398,1 [End]
        @ 06,00 SAY '-------------------------------------------------------------------------------'
        ROW=8
      ENDIF
      SELECT STYLE
      *B803031,1 [Start] go to the right record in style file
      = SEEK (&lcTmpOrdL..Style)
      *B803031,1 [End]    
      *-- MFM 06/27/94.
      *B603398,1 [Start]  print style
      *@ ROW,05 SAY SUBSTR( STYLE, 1, 7 )
      @ ROW,03 SAY STYLE
      *B603398,1 [End]
      *   @ ROW,13 SAY SUBSTR( COLOR, 1, 3 )
      *B603398,1 [Start] shift description
      *@ ROW,17 SAY STYLE->DESC
      @ ROW,23 SAY STYLE->DESC
      *B603398,1 [End] shift description
      ROW = ROW+1
      
      *E302147,1 NAD (Start)
      lnScaleCnt = 8 
      IF SEEK('S'+Style.SCALE,'SCALE') 
        lnScaleCnt = Scale.Cnt
      ENDIF
      *E302147,1 NAD (End)
    
      SELECT (lcTmpOrdL)
      @ ROW,05 SAY 'ORDR:'

    *E302147,1 NAD (SART)
    lnCol =6
    FOR lnX =1 to lnScaleCnt 
      @ ROW,lnCol+(6*lnx) SAY EVAL('QTY'+STR(lnx,1))     PICTURE '99999'
    NEXT lnX
      *@ ROW,12 SAY QTY1     PICTURE '99999'
      *@ ROW,18 SAY QTY2     PICTURE '99999'
      *@ ROW,24 SAY QTY3     PICTURE '99999'
      *@ ROW,30 SAY QTY4     PICTURE '99999'
      *@ ROW,36 SAY QTY5     PICTURE '99999'
      *@ ROW,42 SAY QTY6     PICTURE '99999'
      *@ ROW,48 SAY QTY7     PICTURE '99999'
      *@ ROW,54 SAY QTY8     PICTURE '99999'
   *E302147,1 NAD (END)      
      @ ROW,61 SAY TOTQTY   PICTURE '999999'
      
      ROW = ROW+1

      @ ROW,05 SAY 'ALLO:'

    *E302147,1 NAD (SART)
    lnCol =6
    FOR lnX =1 to lnScaleCnt 
      @ ROW,lnCol+(6*lnx) SAY EVAL('PIK'+STR(lnx,1))     PICTURE '99999'
    NEXT lnX
      *@ ROW,12 SAY PIK1     PICTURE '99999'
      *@ ROW,18 SAY PIK2     PICTURE '99999'
      *@ ROW,24 SAY PIK3     PICTURE '99999'
      *@ ROW,30 SAY PIK4     PICTURE '99999'
      *@ ROW,36 SAY PIK5     PICTURE '99999'
      *@ ROW,42 SAY PIK6     PICTURE '99999'
      *@ ROW,48 SAY PIK7     PICTURE '99999'
      *@ ROW,54 SAY PIK8     PICTURE '99999'
      
      *E302147,1 NAD (END)      
         
      @ ROW,61 SAY TOTPIK   PICTURE '999999'
      ROW = ROW+1
 
      @ ROW,00 SAY REPLICATE('-',79)
      ROW=ROW+2
      SELECT (lcTmpOrdL)
      SKIP
    ENDDO
  *E301440,1 HBG 07/17/2000 End IF option "print bulk pick ticket" in OG is 'Yes'  
  *E301440,1                go to th first line in temp order line file and print 
  *E301440,1                the block of Bulk Pick ticket. [End  ]
  ENDIF
  *E301440,1 HBG 07/17/2000
 
 SELECT (lcTmpOrdL)
 IF LINENO = 0 
   *lnRecord = lnRecord + 1  && increment record no.
   XORDER=ORDER
   XPIKTKT=PIKTKT
 ENDIF
* GOTO lnRecNo
  *B804182,1 ABD 06/03/2001 (Begin)
  IF !EOF() AND !BOF() AND LINENO <> 0 AND llRpPrtBlk
    SKIP -1
  ENDIF
  *B804182,1 ABD 06/03/2001 (End)
*------------------
ENDSCAN
SET DEVICE TO SCREEN

*!**************************************************************************
*! Name      : lfPrntHdr
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/23/2000
*! Purpose   : print report headr
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfPrntHdr()
*!**************************************************************************
*B802985,1 
*!**************************************************************************
FUNCTION lfPrntHdr
* LINE 1
@ 00,01 SAY 'P I C K I N G   T I C K E T'
@ 00,35 SAY HLINE1
*B803589,1 HBG 08/27/2000 moving the date position [Begin]
*@ 00,70 SAY DATE()
@ 00,68 SAY DATE()
*B803589,1 HBG 08/27/2000 [End]
@ 01,01 SAY 'TICKET: '+ PIKTKT       && XPIKTKT
@ 01,16 SAY PIKDATE
@ 01,35 SAY HLINE2
*B803589,1 HBG 08/27/2000 Print page # [Begin]
@ 01,68 SAY 'PAGE  '+STR(PAGECT,3)
*B803589,1 HBG 08/27/2000 [End]
@ 02,01 SAY 'ORDER : '+ORDER    &&TTT
@ 02,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 02,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
@ 02,35 SAY HLINE3
*B803589,1 HBG 08/27/2000 moving the time position [Begin]
*@ 02,70 SAY XTIME
@ 02,68 SAY XTIME
*B803589,1 HBG 08/27/2000 [End]
@ 03,01 SAY IIF(!EMPTY(ORDHDR.APPROVAL),'APPRVL: '+ORDHDR.APPROVAL,'') 
@ 03,35 SAY HLINE4
IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF

*B605418,1 BWA 01/24/2002 Print the warehous address instead of company address.[START]
*@ 04,35 SAY IIF(llPrntComp,LEFT(XPHONE,16),'') 
*B605418,1 BWA 01/24/2002.[END]

@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ORDHDR.ACCOUNT
@ 07,10 SAY CUSTOMER.PHONE1 SIZE 1,16
IF LEN(TRIM(XSTORE))<>0 .AND. XSTORE<>'*'
  @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
ENDIF

@ 08,04 SAY XBTNAME
@ 08,46 SAY XSTNAME
@ 09,04 SAY XBTADDR1
@ 09,46 SAY XSTADDR1
@ 10,04 SAY XBTADDR2
@ 10,46 SAY XSTADDR2
@ 11,04 SAY XBTADDR3
@ 11,46 SAY XSTADDR3
@ 12,04 SAY XBTADDR4
@ 12,46 SAY XSTADDR4
@ 13,00 SAY A
@ 14,00 SAY B
@ 15,00 SAY A
SELECT ORDHDR
@ 16,01 SAY LEFT(XSHIPVIA,15) + '|'
@ 16,18 SAY LEFT(XSEASON,7) + '|'
@ 16,26 SAY LEFT(XSPCINST,18) + '|'
*B803626,1 HBG 10/09/2000 print the store from Temp order line [Begin]
*@ 16,46 SAY ORDLINE->STORE + ' ' + '|'
@ 16,46 SAY &lcTmpOrdL..STORE + ' ' + '|'
*B803626,1 HBG [End]
@ 16,58 SAY DEPT + '|'

*802985,1 ARH 01/25/2000 (Begin) Fixed the variable LCCUTPO not found
*802985,1 ARH            bug in case of multi customer PO orders
*@ 16,65 SAY LEFT(IIF(MultiPO , lcCustPO , CUSTPO),13) +'|'

*B803749,1 AAN Fix bug not print PO correctly[Start]
*@ 16,65 SAY LEFT(IIF(MultiPO , OrdLine.CustPo , CUSTPO),13) +'|'
@ 16,65 SAY LEFT(&lcTmpOrdH..Custpo,13) +'|'
*B803749,1 AAN Fix bug not print PO correctly[End]
*802985,1 ARH 01/25/2000 (End)

@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
ROW=20
*--Header Has Finished

*!**************************************************************************
*! Name      : lfFndLine
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/24/2000
*! Purpose   : check if there are lines in notes filed didn't start with '*'
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfFndLine()
*!**************************************************************************
*B802987,1 
*!**************************************************************************
FUNCTION lfFndLine
PARAMETER lcMemField
PRIVATE lnMemlins,lnMmLnNo,lnCount,lcNotes
lnMemlins = MEMLINES(lcMemField)
STORE 0 TO lnMmLnNo,lnCount
IF lnMemlins > 0
  DO WHILE lnMmLnNo <> lnMemlins
    lnMmLnNo = lnMmLnNo + 1
    lcNotes = MLINE(lcMemField,lnMmLnNo)
    IF LEFT(lcNotes , 1) # '*'                        
      lnCount = lnCount + 1
    ENDIF
  ENDDO
ENDIF 
RETURN IIF(lncount>0,.T.,.F.)

