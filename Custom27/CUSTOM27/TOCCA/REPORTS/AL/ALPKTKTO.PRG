**************************************************************************** 
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
*: *C101748,1 ARD 01/17/2000 Printing Duns# and Vendor#
*: *B803031,1 SAM 02/08/2000 Print 2 pages for only one style color
*: *B603453,1 SAM 02/14/2000 don't print Summary page and print Total Pieces ,Actual Pick.   
*: *B603486,1 SAM 02/25/2000 fix bug of not Printing Duns# and Vendor# 
*: *B603486,1 SAM 02/25/2000 in all pages if multi pages P/T
*: *B803273,1 RAMY 05/28/2000 Change the calculation of the total qty to calculate
*                             the piked qty
*: *B803256,1 BWA 06/07/2000 Comment the update of the field PRTFLAG.
*: *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store
*: *B803311,1 AMH 06/25/2000 Print Size with 5 Characters
*: *B803324,1 MHM 06/26/2000  get Division long name
*: *B802470,1 HBG 07/20/2000 Fix some bugs in the P/T (As per SWK)
*: *B802470,1              1 - The telephone for the company and the customer
*: *B802470,1              2 - The sold to sddress
*: *B802470,1              3 - Remove the End of notes
*: *B802470,1              4 - Remove the part of the Bulk P/T
*: *B802470,1			   5 - Print the total merchendise under condition of printing the price 
*: *B803589,1 HBG 08/27/2000 Fixing bug of:
*: *B803589,1                     - Page # not prined.
*: *B803589,1                     - Scales not aliament.
*: *B802962,1 HBG 03/09/2000 Remove the comma from end of 2 letter state code
*: *B803626,1 HBG 10/09/2000 Fixing bug of not printing store #
**************************************************************************** 

*******************  From Original *******************
*-- We Will use the temp file of pktkt.prg to make this report work

*B603398,1 SAM [Start] get item header 
lcStyTitl = gfItemMask("HI")
*B603398,1  [End]

*B803324,1 MHM [start] get Division long name
lcDivLName = ''        && Variable to hold the Division long name
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
*B803324,1 MHM [End] get Division long name

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

*B603453,1 [Start] set page no. to 1
*STORE 0 TO PAGECT
*B803589,1 HBG 08/27/2000 To begin with bage # 1 [Begin]]
*STORE 1 TO PAGECT
STORE 0 TO PAGECT
*B803589,1 [End]
*B603453,1 [End] set page no. to 1

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
 
  *B603398,1 [Start] Print Style title.
  *C='   G STYLE       CLR     DESCRIPTION          DYELOT #        PRICE      AMOUNT' 
  C='   G ' + lcStyTitl + ' DESCRIPTION          DYELOT #        PRICE      AMOUNT' 
  *B603398,1 [End  ] Print Style title.

ELSE

  *B603398,1 [Start] Print Style title.
  *C='   G STYLE       CLR     DESCRIPTION          DYELOT #        PRICE      AMOUNT'  
  C='   G ' + lcStyTitl + ' DESCRIPTION          DYELOT #        PRICE      AMOUNT'  
  *B603398,1 [End  ] Print Style title.

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
      
      select CUSTOMER

      *-- C101748,1 ARD,MAB Printing Duns# and Vendor# [Begin]
      DIMENSION laSetup[1,2]
      
      laSetUp[1,1] = 'XDUNS'
      =gfGetMemVar(@laSetUp,gcAct_Comp)
      lcDuns = "DUNS # : " + laSetUp[1,2]
      *-- C101748,1 ARD,MAB Printing Duns# and Vendor# [End  ]

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
GO TOP

*B603453,1 [Start] clear piktkt no. 
XPIKTKT = SPACE(6)
*B603453,1 [End] clear piktkt no. 

SELECT (lcTmpOrdL)
GO TOP
SCAN FOR LINENO # 0
  
  *B603453,1 [Start] new piktkt 
  IF XPIKTKT <> PIKTKT
    *B803589,1 HBG 08/27/2000 increment page # [Begin]
    PAGECT =  1
    *B803589,1 [End]
    
    *B803324,1 MHM [start] get Division long name    
    =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
    *B803324,1 MHM [end  ] get Division long name
    *(HBG) Initializing (XVALUE) if it is new pick ticket [Begin]
    XVALUE = 0.00
    *(HBG) [End  ]
  ENDIF
  *B603453,1 [End] new piktkt 
  
  XPIKTKT = PIKTKT
     select CUSTOMER
      
     *-- C101748,1 MAB Printing Vendor # and Vendor# [Begin]
     =SEEK("M"+PIKTKT.ACCOUNT)
     lcVendor = "   VENDOR # : " + CCUSVEND
     = !EMPTY(PIKTKT.STORE) AND SEEK("S"+PIKTKT.ACCOUNT+PIKTKT.STORE)
     *-- C101748,1 MAB Printing Vendor # and Vendor# [End  ]

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
     XBTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')),;
                          gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2') ,;
                          gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')  ;
                 + ' , ' +gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))
     *B802962, HBG 03/09/2000 Remove the comma [Begin]
     XBTADDR4 = STRTRAN(ALLTRIM(XBTADDR4),",","")
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
  *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [Start]
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
  XSHIPVIA = gfCodDes(ORDHDR->SHIPVIA , 'SHIPVIA')

  *SPC INST.
  XSPCINST = gfCodDes(ORDHDR->SPCINST , 'SPCINST')

  SEEK 'Z' + ORDHDR->SEASON
  XSEASON  = gfCodDes(ORDHDR->SEASON , 'SEASON')
      
  HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
  
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
      *B802962, HBG 03/09/2000 Remove the comma [Begin]
      XSTADDR4 = STRTRAN(ALLTRIM(XSTADDR4),",","")
      *B802962, HBG 03/09/2000 [End]                           
      *B603117,1 [END]
      *B802227,1 BWA [END]
      *B803010,1
      
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF
    ELSE
      SELECT CUSTOMER
*      lnRecNo = RECNO()       && Variable to hold the record number.
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
      XSTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )) ,;
                           gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )  ,;
                           gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )   ;
                      +','+gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ))
      *B802962, HBG 03/09/2000 Remove the comma [Begin]
      XSTADDR4 = STRTRAN(ALLTRIM(XSTADDR4),",","")
      *B802962, HBG 03/09/2000 [End]                          
      *B603117,1 [END]
            
      *B802227,1 BWA [END]
      IF !EMPTY(lcDistCntr) 
*        GO lnRecNo
      ENDIF
      *B801524,1 OWA 03/19/98 (End) 
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF
    ENDIF
    *------Begin Say
    SELECT (lcTmpOrdL)
    * LINE 1
    
    @ 00,01 SAY 'P I C K I N G   T I C K E T'
    @ 00,35 SAY HLINE1
    @ 00,70 SAY DATE()
    @ 01,01 SAY 'TICKET: '+ PIKTKT       && XPIKTKT
    *   @ 01,16 SAY XDATE
    @ 01,16 SAY PIKDATE
    @ 01,35 SAY HLINE2

    *B603453,1 [Start] print page no.
    @ 01,70 SAY 'Page:'
    @ 01,75 SAY PAGECT PICTURE '999' 
    *B603453,1 [End] print page no.

    XORDER = ORDER
    @ 02,01 SAY 'ORDER : '+ORDER    &&TTT
    @ 02,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
    @ 02,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
    @ 02,35 SAY HLINE3
    @ 02,70 SAY XTIME
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
    @ 04,35 SAY IIF(llPrntComp,XPHONE,'')
    *B802470,1 (END)
    *B802520,1 [END]

    *-- C101748,1 Printing Duns# and Vendor# [Begin]
    @ 05,35 SAY lcDuns
    @ 05,50 SAY lcVendor 
    *-- C101748,1 Printing Duns# and Vendor# [End  ]
    
    *@ 04,35 SAY HLINE5
    *B500772,1 HISH 06/29/95.  ( End )
    @ 06,04 SAY '.....SOLD TO .....'
    @ 06,46 SAY '.....SHIP TO .....'
    
    @ 07,04 SAY ORDHDR.ACCOUNT
    
    *B802470,1 Fix the customer phone format
    *@ 07,10 SAY CUSTOMER.PHONE1 SIZE 1,16
    @ 07,10 SAY TRANSFORM(ORDHDR.PHONE, lcPhonPict)
    *B802470,1 (END)
    
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
   @ 16,65 SAY LEFT(IIF(MultiPO , OrdLine.CustPo , CUSTPO),13) +'|'
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
   DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0
*       DO CASE
*         CASE EOF()
*           NEWDOC = .T.
*           EXIT
*         CASE STORE # XSTORE
*           NEWDOC = .F.
*           EXIT
*         CASE TOTPIK<=0
*           SKIP
*           LOOP
*         CASE ROW>= 47
*           NEWDOC=.F.
*           EXIT
*      ENDCASE
       SELECT (lcTmpOrdL)
       lnCurRec = RECNO()

      SELECT STYLE
      XSTYDESC =  DESC
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
      *B803311,1 AMH 06/25/2000 Print Size with 5 Characters (Start)
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
      
      ENDIF
      *lcScl = GETSCALE(SCALE,SPACE(1) )
      *@ ROW,11 SAY lcScl
      *@ ROW,12 SAY lcScl
      @ ROW,11 SAY lcScl
      *B803589 HBG 08/27/2000 Fixing bug of scales not aliament[End  ]
      *B803311,1 AMH 06/25/2000 Print Size with 5 Characters (End)
      *--Actual Lines Printing[start]
      
      SELECT (lcTmpOrdL)
      ROW = ROW+2

      @ ROW,05 SAY 'ORDR:'
      @ ROW,12 SAY QTY1     PICTURE '99999'
      @ ROW,18 SAY QTY2     PICTURE '99999'
      @ ROW,24 SAY QTY3     PICTURE '99999'
      @ ROW,30 SAY QTY4     PICTURE '99999'
      @ ROW,36 SAY QTY5     PICTURE '99999'
      @ ROW,42 SAY QTY6     PICTURE '99999'
      @ ROW,48 SAY QTY7     PICTURE '99999'
      @ ROW,54 SAY QTY8     PICTURE '99999'
      @ ROW,61 SAY TOTQTY   PICTURE '999999'
     
      * SUM ORDER TOTALS
      *B803273,1 RAMY Change the calculation of the total qty to calculate 
      *               the piked qty [start]
      *XTOTQTY  = XTOTQTY+ TOTQTY
      XTOTQTY  = XTOTQTY+ TOTPIK
      *B803273,1 RAMY [END]
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
         @ ROW,12 SAY PIK1     PICTURE '99999'
         @ ROW,18 SAY PIK2     PICTURE '99999'
         @ ROW,24 SAY PIK3     PICTURE '99999'
         @ ROW,30 SAY PIK4     PICTURE '99999'
         @ ROW,36 SAY PIK5     PICTURE '99999'
         @ ROW,42 SAY PIK6     PICTURE '99999'
         @ ROW,48 SAY PIK7     PICTURE '99999'
         @ ROW,54 SAY PIK8     PICTURE '99999'
         @ ROW,61 SAY TOTPIK   PICTURE '999999'
      ENDIF
         
      XPIECES = XPIECES + TOTPIK
     
      ROW =ROW+1
      @ ROW,05 SAY 'PICK:'
      @ ROW,12 SAY '_____'
      @ ROW,18 SAY '_____'
      @ ROW,24 SAY '_____'
      @ ROW,30 SAY '_____'
      @ ROW,36 SAY '_____'
      @ ROW,42 SAY '_____'
      @ ROW,48 SAY '_____'
      @ ROW,54 SAY '_____'
      @ ROW,62 SAY '_____'
      ROW=ROW+2
      *B802985,1 WAB - if the detail printed line  more than 58 , we must eject and
      *B802985,1 WAB - reprint the header again.
      *B802985,1 WAB - check if there are line note in order line and print it 
      *B802985,1 WAB - if the option of print ordline notes is yes.
      IF ROW > 58
        *-- call function to print header
        
        *B603453,1 [Start] new page no.
        PAGECT = PAGECT + 1
        *B603453,1 [End] new page no.
        
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
                
                *B603453,1 [Start] new page no.
                PAGECT = PAGECT + 1
                *B603453,1 [End] new page no.
                
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
   
   *B603453,1 [Start] print total pieces and actual pick
   ROW = ROW + 1
   IF ROW > 62
     *-- call function to print header             
     PAGECT = PAGECT + 1
     =lfPrntHdr()
     ROW = 22
   ENDIF
   @ ROW,42 SAY 'Total Pieces :'
   @ ROW,57 SAY XTOTQTY  PICTURE '9999999999'
   ROW = ROW + 2
   @ ROW,42 SAY ' Actual Pick :'
   @ ROW,60 SAY '_______'
   ROW = ROW + 1 
   IF ROW > 58
     *-- call function to print header             
     PAGECT = PAGECT + 1
     =lfPrntHdr()
     ROW = 57
   ENDIF
   *B603453,1 [End] print total pieces and actual pick  

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
             @ 54,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
             @ 54,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
             *B802987,4 WAB - END
        	 
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
             *B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [Start]
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
             @ 00,70 SAY DATE()
             @ 01,35 SAY HLINE2
             

             @ 02,01 SAY 'TICKET: '+XPIKTKT

             *B802330,1 BWA 06/15/99 Fix the bug of shifting the data in the form 
             *@ 02,16 SAY XDATE
             @ 02,16 SAY PIKTKT.DATE
             *B802330,1 BWA [END]
             @ 02,35 SAY HLINE3
             @ 02,70 SAY XTIME

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
             @ 04,35 SAY IIF(llPrntComp,LEFT(XPHONE,16),'') 
             *B802520,1 [END]
             
             *@ 04,35 SAY HLINE5
             *B500772,1 HISH 06/29/95.  ( End )
                             
             *B603486,1 [Start] print vendor and DUNS
             @ 05,35 SAY lcDuns
             @ 05,50 SAY lcVendor 
             *B603486,1 [End]
             
             @ 06,04 SAY '.....SOLD TO .....'
             @ 06,46 SAY '.....SHIP TO .....'

             @ 07,04 SAY ACCOUNT

            *B802470,1 Fix the customer phone format
            *@ 07,10 SAY CUSTOMER.PHONE1 SIZE 1,16
            @ 07,10 SAY TRANSFORM(ORDHDR.PHONE, lcPhonPict)
            *B802470,1 (END)
             
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
             *@ 16,65 SAY LEFT(IIF(MultiPO , lcCustPO , CUSTPO),13) +'|'
             @ 16,65 SAY LEFT(IIF(MultiPO , OrdLine.CustPo , CUSTPO),13) +'|'
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
   @ 54,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
   @ 54,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
   *B802987,4 WAB - END

   SELECT (lcTmpOrdL)
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
*B803307,1 AMH 06/25/2000 Use OrdLine.Store instead of OrdHdr.Store [Start]
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
     SKIP 
   *B802470,1 (End) 
   ENDIF
   
  *B803256,1 BWA 06/07/2000 Comment the update of the field PRTFLAG.[START]
  *SELE PIKTKT
  *REPL PRTFLAG WITH 'P'
  *B803256,1 BWA 06/07/2000 [END]
  
   *---Print Piktkt details

*   DO WHILE INKEY() <>32 .AND. !EOF()
   SELECT (lcTmpOrdL)
*   lnRecNo = RECNO()
 
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
@ 00,70 SAY DATE()
@ 01,01 SAY 'TICKET: '+ PIKTKT       && XPIKTKT
@ 01,16 SAY PIKDATE
@ 01,35 SAY HLINE2

*B603453,1 [Start] print page no.
@ 01,70 SAY 'Page:'
@ 01,75 SAY PAGECT PICTURE '999' 
*B603453,1 [End] print page no.

@ 02,01 SAY 'ORDER : '+ORDER    &&TTT
@ 02,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 02,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
@ 02,35 SAY HLINE3
@ 02,70 SAY XTIME
@ 03,01 SAY IIF(!EMPTY(ORDHDR.APPROVAL),'APPRVL: '+ORDHDR.APPROVAL,'') 
@ 03,35 SAY HLINE4
IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF
@ 04,35 SAY IIF(llPrntComp,LEFT(XPHONE,16),'') 

*B603486,1 [Start] print vendor and DUNS
@ 05,35 SAY lcDuns
@ 05,50 SAY lcVendor 
*B603486,1 [End]

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
@ 16,65 SAY LEFT(IIF(MultiPO , OrdLine.CustPo , CUSTPO),13) +'|'
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