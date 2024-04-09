*:***************************************************************************
*: Program file : SOEIL140.PRG    Ver 2.6   (C#101404)
*: DESC : OPEN ORDER DETAIL REPORT (E.Fisher)
*:        A copy from form ORD920 with some changes .           
*: Module: Aria Apparel Series.
*: DATE : 
*: Developer: Ahmed SAlah Shalaby (SSH).
*C101034 Refer to.
*:************************************************************************
*: Calls : 
*:         FUNCTION  :lfEvalSegs,lfNMajType,lfwRepWhen,lfSRVSty,
*:                    lfStySum,lfvFabric,lfvAccount,lfvSelcBy,lfvSortBy,
*:                    lfvOrder,lfsrAcc,lfUpdFltVar
*:************************************************************************
*E301249,1 IHB 06/16/1999 Make priority field C(3) in CUSTOMER 
*E301249,1 				  and ORDHDR files
*B802842,1 AMM 12/02/1999 Fix Some bugs in report
*B802975,1 M.H 01/18/2000 Fix the duplication of the lines.
*C101826,1 AME 04/06/2000 Make Modification in the layout of some types of sorting.
*B803405,1 ABD 07/19/2000 Fix some bugs as follow:-
*B803405,1 ABD            1) need the words "Status" and "Priority" to be 
*B803405,1 ABD               printed in capital letters.
*B803405,1 ABD            2) We need to make the store# printed closer to where the style/ color is printed. 
*B803405,1 ABD               move the store # to the right (about 10 characters)
*B803405,1 ABD            3) Fix  problem happens if the order has printed all the detail lines 
*B803405,1 ABD               And only needs to print the summary at the page break.   
*B803661,1 ADEL 09/14/2000 Print the scale sizes desciption when it chnages only.
*B803970,1 AME  05/24/2001 Fix The bug of summrize Multi store order option doesn't work.
*B607310,1 KHM  07/08/2003 Get only the sales orders with cOrdType = 'O'.
*:************************************************************************
lcMajPic = "@! " + gfItemMask("PM")
lnMajPic = LEN(gfItemMask("PM"))
NULDATE   = {}
TS        = ''
TD        = ''
DIMENSION XSEASON(6), XDIVISION(6)
STORE SPACE(6) TO LOR,HOR
XSEASON   = SPACE(2)
XDIVISION = SPACE(2)
STORE NULDATE  TO LCO,HCO,LST,HS
XCOORGRP  = 'N'
lcStatus  = SPACE(3)
XGR       = SPACE(2)
STORE SPACE(6) TO CLR1,CLR2,CLR3,CLR4,CLR5,CLR6,CLR7,CLR8
STORE SPACE(1) TO lnPer1,lnPer2,lnPer3,lnPer4,lnPer5,lnPer6,lnPer7,lnPer8,lnPer9,;
                  lnAllPer,lcFilter,ZST,lcAccAls,lcStyAls
STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,XTOTQTY,XAMT
xTitle     = SPACE(40)
llStyRange = .T.
llAccRange = .T.
lcNote     = .F.
lcSortCode = ' '

*B803970,1 AME [Start] comment next statment because llSummarize is a logical variable
*B803970,1 AME         And its value is based on lcrpSumm
*llSummarize= 'N'
llSummarize = (lcRpSumm = 'Y')
*B803970,1 AME [End]

XPRTWPSTK  = 'N'
xTitle     = SPACE(40)
lcCancel   = .F.
llNotes    = (gfGetMemVar('M_LN_NOTE',gcAct_Comp) = 'Y')
lcNotes    = ''
lcSele     = ''
TCLR       = ''
lcAccount = SPACE(5)
lcFabric  = SPACE(7)
STORE SPACE(12) TO lStyle,hStyle
STORE SPACE(6) TO LOR,HOR
LFCR = CHR(10) + CHR(13)

IF !lfUpdFltVar()
  DO CASE
    *B802842,1 AMM Set the right variable
    *CASE lcRpSelcBy = 'S' AND !llAccRange
    CASE lcRpSelcBy = 'S' AND !llStyRange
    *B802842,1 AMM end
      WAIT WINDOW "Must select Style range...!"
      SET DEVICE TO SCREE
      RETURN
    CASE lcRpSelcBy = 'A' AND !llAccRange
      WAIT WINDOW "Must select Account range...!"
      SET DEVICE TO SCREE
      RETURN
  ENDCASE
ENDIF
*B802842,1 AMM Get the Priority without spaces
lcRpExp = STRTRAN(lcRpExp, 'OrdHdr.PRIORITY' , 'ALLTRIM(OrdHdr.PRIORITY)')
*B802842,1 AMM end
TEMP = gfTempName()
*------------------------
* CREATE WORK FILE
*------------------------
SELECT ORDLINE
COPY STRUCTURE TO &gcWorkDir.&TEMP
=gfOpenFile(gcWorkDir+TEMP,'','EX')
ldCompDate = {}

DIMENSION XTOTAL(3,10)
DIMENSION CLRTOT(10)
DIMENSION ORDTOT(10)

STORE 0 TO lnPerCent,lnTotalQty,lnTotalPik
WORK = gfTempName()

*-----------------------------------------------
* [ACCOUNT] APPEND BY ACCOUNT#
*-----------------------------------------------
IF lcSele='A'
   SELECT OrdHdr
   SET ORDER TO TAG ORDACCT
   SELECT ORDLINE
   SET ORDER TO TAG ORDLINE
   SELECT &lcAccAls
   SCAN
     lcAccount = Account
     SELECT OrdHdr
     =SEEK(lcAccount)
     SCAN REST WHILE account+cordtype+order = lcAccount+'O'
       lcOrder = ORDER
       WAIT WINDOW 'SELECTING ORDER: &lcOrder  ACCT: &lcAccount ...' NOWAIT
       SELECT ORDLINE
       =SEEK('O'+lcOrder)
       COPY REST TO &gcWorkDir.&WORK ;
       WHILE cordtype+order+STR(lineno,6) = 'O'+lcOrder FOR TOTQTY > 0
       SELECT &TEMP
       APPEND FROM &gcWorkDir.&WORK
     ENDSCAN
   ENDSCAN
ENDIF
*-------------------- END SELECTION BY ACCOUNT# -------------------

*-------------------------------------------------------
* [STYLE] APPEND BY STYLE NUMBER
*-------------------------------------------------------
IF lcSele = 'S'
  SELECT ORDLINE
  SET ORDER TO TAG ORDLINES
  SELECT &lcStyAls
  SCAN
*B802975 M.H Begin.
*    WAIT WINDOW 'Selecting orders for styles: ALLTRIM(&lcStyAls..cStyMajor) ...' NOWAIT
    WAIT WINDOW 'Selecting orders for styles: PADR(&lcStyAls..cStyMajor,lnMajPic) ...' NOWAIT
    SELECT ORDLINE
*    =SEEK(ALLTRIM(&lcStyAls..cStyMajor))
    =SEEK(PADR(&lcStyAls..cStyMajor,lnMajPic))
*    COPY REST TO &gcWorkDir.&WORK ;
    WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
          ALLTRIM(&lcStyAls..cStyMajor) FOR TOTQTY > 0

    *B607310,1 KHM 07/08/2003 (Begin) Get the sales orders with cOrdType = 'O' Only.
    *COPY REST TO &gcWorkDir.&WORK ;
    *WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = PADR(&lcStyAls..cStyMajor,lnMajPic) FOR TOTQTY > 0          

    COPY REST TO &gcWorkDir.&WORK ;
    WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = ;
          PADR(&lcStyAls..cStyMajor,lnMajPic) FOR cOrdType = 'O' AND TOTQTY > 0
    *B607310,1 KHM 07/08/2003 (End)
    
*B802975 M.H End.
    SELECT &TEMP
    APPEND FROM &gcWorkDir.&WORK
  ENDSCAN
ENDIF
*----------------- END SELECTION BY STYLE NUMBER -------------------

*--------------------------------------------------
* [FABRIC] APPEND BY FABRIC CODE
*--------------------------------------------------
IF lcSele = 'F'
   SELECT ORDLINE
   SET ORDER TO TAG ORDLINES
   SELECT STYLE
   STYFILTER = "FABRIC=lcFabric"
   LOCATE ALL FOR &STYFILTER
   IF EOF()
      WAIT WINDOW 'NO STYLES IN FABRIC GROUP &lcFabric' NOWAIT
      SET DEVICE TO SCREE
      RETURN
   ENDIF
   SCAN REST FOR FABRIC = lcFabric
     STYKEY = STYLE
     WAIT WINDOW 'SELECTING ORDERS FOR STYLE: &STYKEY ...' NOWAIT
     SELECT ORDLINE
     =SEEK(STYKEY)

     *B607310,1 KHM 07/08/2003 (Begin) Get the sales orders with cOrdType = 'O' Only.
     *COPY REST TO &gcWorkDir.&WORK ;
     *WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
     *STYKEY FOR TOTQTY > 0
     
     COPY REST TO &gcWorkDir.&WORK ;
     WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
     STYKEY FOR cOrdType = 'O' AND TOTQTY > 0
     *B607310,1 KHM 07/08/2003 (End)
     
     SELECT &TEMP
     APPEND FROM &gcWorkDir.&WORK
   ENDSCAN
   lcSele = ' '  &&-ONLY ONE FABRIC SELECTION PERMITTED
ENDIF
*----------------- END SELECTION BY FABRIC -------------------

*------------------ END BUILD TEMPORARY LINE ITEM FILE -------------------
DO WHILE .T.
  WAIT WINDOW 'Selecting records. Please stand by' NOWAIT
  SELE OrdHdr
  SET ORDER TO TAG OrdHdr
  SELE &TEMP
  IF lcSele = 'E'
    USE
    SELECT ORDLINE
    SET RELATION TO 'O'+ORDER INTO OrdHdr
    SET RELATION TO STYLE  INTO STYLE ADDITIVE
    GO TOP
    SET TALK ON
    LOCATE FOR &lcFilter
    COPY REST TO &gcWorkDir.&TEMP FOR &lcFilter
    SET TALK OFF

    SELECT ORDLINE
    SET RELATION TO

    SELECT 0
    DO NETUSE WITH '&gcWorkDir.&TEMP',' ','EX'
    SET RELATION TO 'O'+ORDER INTO OrdHdr
    SET RELATION TO STYLE  INTO STYLE ADDITIVE
  ELSE
    SET RELATION TO 'O'+ORDER INTO OrdHdr
    SET RELATION TO STYLE  INTO STYLE ADDITIVE
    SET FILTER TO &lcFilter
  ENDIF
  SELECT &TEMP
  GOTO TOP
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
      SET DEVICE TO SCREE
    RETURN
  ENDIF
  *------ SET UP RELATIONS --------*
  SELECT &TEMP
  IF lcSortCode ='S' .AND. !llSummarize
    SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
  ELSE
    SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER  ADDITIVE
  ENDIF
  GO TOP
  *-----------------------------------------------------------
  * [BREAK] SET UP SORT FILEDS & CONTROL BREAKS
  *-----------------------------------------------------------
  lcSort = ' '
  DO CASE
    * ACCOUNT NUMBER SEQUENCE
    CASE lcSortCode='A'
      lcSort = 'ACCOUNT+ORDER+STYLE+STR(LINENO,6)'
      BREAK  = 'CUSTOMER.ACCOUNT + SPACE(3) + CUSTOMER.BTNAME + LFCR + CUSTOMER.PHONE1'
      CTLSEQ = 'ACCOUNT NUMBER'
    * ORDER NUMBER SEQUENCE
    CASE lcSortCode='O'
      lcSort  = 'ORDER+STYLE+STR(LINENO,6)'
      BREAK  = 'ORDER + SPACE(3) + ACCOUNT' && ACCOUNT IS INCLUDED HERE
                                            && ONLY TO DISPLAY ON SUBTOTAL.
                                            && IT IS NEEDED TO BREAK
      CTLSEQ = 'ORDER'
    * FABRIC GROUP
    CASE lcSortCode='F'
      lcSort  = 'STYLE.FABRIC+STYLE+ORDER+STR(LINENO,6)'
      BREAK  = 'STYLE.FABRIC'
      CTLSEQ = 'PRIMARY FABRIC'
    * STYLE/COLOR
    CASE lcSortCode = 'S'
      lcSort    = 'STYLE+OrdHdr.Priority+DTOS(OrdHdr.COMPLETE)+Account+ORDER+Store+STR(LINENO,6)'
      BREAK     = ' '
      CTLSEQ    = 'STYLE'
    CASE lcSortCode = 'D'
      lcSort  = 'DTOS(OrdHdr.COMPLETE)+STYLE+ORDER+STR(LINENO,6)'
      BREAK  = 'DTOC(OrdHdr.COMPLETE)'
      CTLSEQ = 'COMPLETION DATE'
    CASE lcSortCode = 'R'    
      lcSort  = 'OrdHdr.REP1+STYLE+ORDER+STR(LINENO,6)'
      BREAK  = 'OrdHdr.REP1'
      CTLSEQ = 'SALESREP'
    CASE lcSortCode = 'P'
      lcSort  = 'OrdHdr.Priority+DTOS(OrdHdr.COMPLETE)+Account+order+store+Style+STR(LINENO,6)'
      BREAK  = 'OrdHdr.Order'
      *C101826,1 AME[Start]
      *CTLSEQ = 'Order'   
      CTLSEQ = 'Priority'        
      *C101826,1 AME[End]
  ENDCASE
  *----------------------------------------------
  * SORT TEMPORARY FILE
  *----------------------------------------------
  SELECT &TEMP
  Z = LTRIM(STR(RECCOUNT(),7))
  WAIT WINDOW 'Sorting &Z selected records for report ...' NOWAIT
  INDEX ON &lcSort TAG &TEMP
  SET ORDER TO TAG &TEMP
  
  * PRINT REPORT
  SELECT &TEMP
  GOTO TOP
  lnAlias = SELECT()
  IF BREAK <> ' '
    HBREAK = &BREAK
  ENDIF
  lcStyle   = SUBSTR(Style,1,lnMajPic)
  CTLCOLOR  = STYLE
  CTLOR     = ACCOUNT+ORDER+IIF(OrdHdr.MULTIPO,'*MULTI-PO*',OrdHdr.CUSTPO)
  *---C101826,1 new Variable to  hold order to check for order subtotal , AME[start] 
  Xord      = ORDER
  *---C101826,1 AME[end] 
  lcOrder   = ORDER
  lcStyDesc = STYLE.DESC
  lcStyFab  = STYLE.FABRIC
  *B803661,1 (Begin) Initialize lcScale
  *lcScale   = STYLE.SCALE
  lcScale   = ' '
  *B803661,1 (End)
  
  ldCompDate = &Temp..Complete
  NEWSTYLE   = .T.
  

  XSUM_STORE = .F.      &&   Flag to multi order style sort.

  
  PAGENO = 0
  ROW    = 99
  XTIME  = SUBSTR(TIME(),1,5)
  XPRV_KEY=SPACE(1)
  *--------------------------------------------------------
  *  (1) STYLE
  *  (2) USER DEFINED CONTROL BREAK
  *  (3) GRAND TOTALS
  *--------------------------------------------------------
  XTOTAL  = 0.00   &&-ARRAY(3,10)
  CLRTOT  = 0.00   &&-ARRAY(10)
  ORDTOT  = 0.00   &&-ARRAY(10)
  *-----------------------------------------------------
  * [REPORT] BEGIN REPORT LOOP
  *-----------------------------------------------------
  R_TITLE = " OPEN ORDER DETAIL REPORT "
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  *---C101826,1 Increase the NO. of Sizes , AME[start]   
  *STORE 0 TO lnAllAdd1,lnAllAdd2,lnAllAdd3,lnAllAdd4
  STORE 0 TO lnAllAdd1,lnAllAdd2,lnAllAdd3,lnAllAdd4,lnAllAdd5,lnAllAdd6,lnAllAdd7,lnAllAdd8
  *---C101826,1 AME[end]  
  STORE 0 TO lnFinTot, lnFinAmnt
  SET DEVICE TO PRINT
  
  *B803405,1 ABD Define Variable hold the order. [Begin]
  lcNewOrder = ''
  *B803405,1 ABD [End]
  DO WHILE INKEY() <>32
    IF ROW >=54
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'SOEIL140','(SUB TOTALS BY: ' + CTLSEQ +') '+ xTitle,R_WIDTH
      IF lcSortCode = 'S'
        IF !llSummarize        

          *E301249,1 Make priority field C(3) [start]
          *@ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER      (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
          *---C101826,1 Remove Complete from header & Reduce Dyelot. to Add sdditional 4 sizes AME[start]         
          *@ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER     (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
          @ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    SP  CUSTOMER     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          *---C101826,1  AME[End]
          *E301249,1 [end]
          
        ELSE

          *E301249,1 Make priority field C(3) [start]
          *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER     STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
          *---C101826,1 Remove Complete,Price from header & Reduce Dyelot. to Add sdditional 4 sizes AME[start]         
          *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
           @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          *---C101826,1  AME[End]
          *E301249,1 [end]
           
        ENDIF
        @ 06,000 SAY REPLICATE('-',132) 
        ROW = 7
        *---C101826,1 Print the header in case of Style,Fabric AME[End] 
      ELSE
        *---C101826,1 Print the header in case of Acc. , Prio.  Or  Orer AME[start] 
        IF lcSortCode $ "APO"
          *B803405,1 ABD Change the words "Status" and "Priority" to be printed in capital letters. [Begin]
          *@ 05,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  Status Priority'
          @ 05,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  STATUS PRIORITY'
          *B803405,1 ABD [END]
          @ 06,00 SAY REPLICATE('-',132)        
         
         *B803405,1 ABD Fix  problem happens if the order has printed all the detail lines 
         *B803405,1 ABD And only needs to print the summary at the page break.   
         *B803405,1 ABD We Will cheak if Eof and order is the same. [begin]
         IF !EOF() .AND. Order = lcNewOrder
           *B803405,1 ABD [End]
           lcNewOrder = Order
           *B803405,1 ABD Make the store# printed closer to where the style/ color is printed. 
           *B803405,1 ABD Move the store # to the right (about 10 characters) [Begin]
           *@ 07,000 SAY 'STORE #                       STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
            @ 07,000 SAY '          STORE #             STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
           *B803405,1 ABD [End]
            @ 08,000 SAY REPLICATE('-',132)
            @ 09 , 00 SAY ordhdr.Account 
            @ 09  , 07 SAY LEFT(CUSTOMER.Stname,15)
            @ 09  , 23 SAY ORDER
            @ 09  , 30 SAY ORDHDR.Entered
            @ 09  , 40 SAY SUBSTR(IIF(OrdHdr.MULTIPO,&temp..custpo,OrdHdr.CUSTPO),1,10)
            @ 09  , 51 SAY OrdHdr.Dept
            @ 09  , 57 SAY OrdHdr.START 
            @ 09  , 67 SAY OrdHdr.COMPLETE 
            @ 09  , 77 SAY OrdHdr.STATUS
            @ 09  , 84 SAY ALLTRIM(OrdHdr.PRIORITY)
            @ 10,000 SAY REPLICATE('-',132)
            ROW = 11    && set the row to 11 instead of 7
            *B803405,1 ABD Else we will beginning print from Row 7. [Begin]
          ELSE
            ROW = 07  
          ENDIF
          *B803405,1 ABD [End]
        ELSE
          IF 	lcSortCode = "F"
            @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    SP  STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          ELSE
            *---C101826,1 AME[end] 
            *E301249,1 Make priority field C(3) [start]
            *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
            @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  STYLE        COLOR   (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
            *E301249,1 [end]
          ENDIF
          *---C101826,1 print The "--" line & set row to 7 here AME[start] 
          @ 06,000 SAY REPLICATE('-',132)
          ROW = 7
          *---C101826,1 AME[end] 
        ENDIF
      ENDIF
      *---C101826,1 Remove  The "--" line & set row to 7 from here AME[start] 
      *@ 06,000 SAY REPLICATE('-',132)
      *ROW = 7
      *---C101826,1 AME[end]  
    ENDIF
    SELECT &TEMP
    LEVEL = 0
    IF SUBSTR(&TEMP..STYLE,1,lnMajPic) <> lcStyle
      LEVEL = 1
    ENDIF
    DOCLR = IIF(lcSortCode='S' .AND. STYLE<>CTLCOLOR, .T., .F.)
    DoDate = IIF(lcSortCode='S' .AND. &Temp..Complete<>ldCompDate,.T.,.F.)
    DOORD = IIF(lcSortCode='A' .AND. ;
      ACCOUNT+ORDER+IIF(OrdHdr.MULTIPO,'*MULTI-PO*',OrdHdr.CUSTPO)<>CTLOR, .T., .F.)
    *---C101826,1 new Variable to check order subtotal , AME[start] 
    DOORN = IIF(lcSortCode $ "APO" .AND. ORDER <> XOrd, .T. , .F.)
    *---C101826,1 AME[end] 
    XSUM_STORE=IIF(llSummarize .AND. OrdHdr.MULTI='Y',.T.,.F.)
    IF !EMPTY(BREAK)
      IF HBREAK <> &BREAK
        LEVEL = 2
      ENDIF
    ENDIF
    *----------------------------------------------------
    * [DOCLR] STYLE/CLR SUB TOTALS
    *----------------------------------------------------
    IF DOCLR
      ROW= ROW+1
      @ ROW,00 SAY REPLICATE('-',132)        
      ROW= ROW+1
      @ROW,00 SAY  ldCompDate
      
      *C101826,1 Add $ Additional sizes in case of style or fabric
      *@ROW,74 SAY  lnAllAdd1 PICTURE '@Z 99999'
      *@ROW,80 SAY  lnAllAdd2 PICTURE '@Z 99999'
      *@ROW,86 SAY  lnAllAdd3 PICTURE '@Z 99999'      
      *@ROW,92 SAY  lnAllAdd4 PICTURE '@Z 99999'      
      *@ROW,98 SAY lnFinTot  PICTURE '@Z 999999'      
      
      @ROW,65 SAY  lnAllAdd1 PICTURE '@Z 99999'
      @ROW,71 SAY  lnAllAdd2 PICTURE '@Z 99999'
      @ROW,77 SAY  lnAllAdd3 PICTURE '@Z 99999'      
      @ROW,83 SAY  lnAllAdd4 PICTURE '@Z 99999'      
      @ROW,89 SAY  lnAllAdd5 PICTURE '@Z 99999'
      @ROW,95 SAY  lnAllAdd6 PICTURE '@Z 99999'
      @ROW,101 SAY  lnAllAdd7 PICTURE '@Z 9999'      
      @ROW,106 SAY  lnAllAdd8 PICTURE '@Z 9999'      
      @ROW,111 SAY lnFinTot  PICTURE '@Z 999999'      
      *@ROW,116 SAY "$"
      *@ROW,118 SAY lnFinAmnt PICTURE '@Z 99999999.99'      
      @ROW,119 SAY "$"
      @ROW,120 SAY lnFinAmnt PICTURE '@Z 9999999.99'      
      *C101826,1 AME[End]

      ROW= ROW+1
      @ ROW,00 SAY REPLICATE('-',132)                
      ldCompDate = &Temp..Complete
      STORE 0 TO lnAllAdd1,lnAllAdd2,lnAllAdd3,lnAllAdd4,lnAllAdd5,lnAllAdd6,lnAllAdd7,lnAllAdd8
      STORE 0 TO lnFinTot, lnFinAmnt
      ROW = ROW+1
      @ ROW,030 SAY '***'
      @ ROW,034 SAY LEFT(CTLCOLOR,12)
      @ ROW,047 SAY RIGHT(CTLCOLOR,6)
      
      *C101826,1 Add $ Additional sizes in case of style or fabric
      *@ ROW,074 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
      *@ ROW,080 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
      *@ ROW,086 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
      *@ ROW,092 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
      *@ ROW,098 SAY CLRTOT(1,9)  PICTURE '9999999'
      *@ ROW,118 SAY CLRTOT(1,10) PICTURE '$9999999.99'
      @ ROW,065 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
      @ ROW,071 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
      @ ROW,077 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
      @ ROW,083 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
      @ ROW,089 SAY CLRTOT(1,5)  PICTURE '@Z 99999'
      @ ROW,095 SAY CLRTOT(1,6)  PICTURE '@Z 99999'
      @ ROW,101 SAY CLRTOT(1,7)  PICTURE '@Z 9999'
      @ ROW,106 SAY CLRTOT(1,8)  PICTURE '@Z 9999'
      @ ROW,110 SAY CLRTOT(1,9)  PICTURE '9999999'
      @ ROW,119 SAY CLRTOT(1,10) PICTURE '$9999999.99'
      *C101826,1 AME[End]
      
      ROW = ROW+2
      CTLCOLOR = STYLE
      CLRTOT = 0.00
    ELSE
      IF DoDate
        ROW= ROW+1
        @ ROW,00 SAY REPLICATE('-',132)        
        ROW= ROW+1          
        @ROW,00 SAY  ldCompDate

        *C101826,1 Add $ Additional sizes in case of style or fabric
        *@ROW,74 SAY  lnAllAdd1 PICTURE '@Z 99999'
        *@ROW,80 SAY  lnAllAdd2 PICTURE '@Z 99999'
        *@ROW,86 SAY  lnAllAdd3 PICTURE '@Z 99999'      
        *@ROW,92 SAY  lnAllAdd4 PICTURE '@Z 99999'      
        *@ROW,98 SAY lnFinTot  PICTURE '@Z 9999999'      
        *@ROW,116 SAY "$"
        *@ROW,118 SAY lnFinAmnt PICTURE '@Z 99999.99'      

        @ROW,65 SAY  lnAllAdd1 PICTURE '@Z 99999'
        @ROW,71 SAY  lnAllAdd2 PICTURE '@Z 99999'
        @ROW,77 SAY  lnAllAdd3 PICTURE '@Z 99999'      
        @ROW,83 SAY  lnAllAdd4 PICTURE '@Z 99999'      
        @ROW,89 SAY  lnAllAdd5 PICTURE '@Z 99999'
        @ROW,95 SAY  lnAllAdd6 PICTURE '@Z 99999'
        @ROW,101 SAY  lnAllAdd7 PICTURE '@Z 9999'      
        @ROW,106 SAY  lnAllAdd8 PICTURE '@Z 9999'      
        @ROW,110 SAY lnFinTot  PICTURE '@Z 9999999'      
        @ROW,119 SAY "$"
        @ROW,122 SAY lnFinAmnt PICTURE '@Z 99999.99'      
        *C101826,1 AME[End]

        ROW= ROW+1
        @ ROW,00 SAY REPLICATE('-',132)                
        ROW= ROW+1          
        ldCompDate = &Temp..Complete
        STORE 0 TO lnAllAdd1,lnAllAdd2,lnAllAdd3,lnAllAdd4,lnAllAdd5,lnAllAdd6,lnAllAdd7,lnAllAdd8
        STORE 0 TO lnFinTot, lnFinAmnt
      ENDIF
    ENDIF
    IF !XSUM_STORE
      *---C101826,1 sizes are 8 instead of 4 in case of "APOSF" AME[start] 
      *---C101826,1 Note that variables lnallAdd used only in case of style AME[Start]
      *FOR lnAdd = 1 TO 3
      *  lnAdd1 = ALLTRIM(STR(lnAdd))
      *  lnAllAdd&lnAdd1 = lnAllAdd&lnAdd1 + Qty&lnAdd1
      *ENDFOR
      *lnAllAdd4 = lnAllAdd4 + Qty4+Qty5+Qty6+Qty7+Qty8
      FOR lnAdd = 1 TO 8
        lnAdd1 = ALLTRIM(STR(lnAdd))
        lnAllAdd&lnAdd1 = lnAllAdd&lnAdd1 + Qty&lnAdd1
      ENDFOR
      *---C101826,1 AME[end] 
      lnFinTot =lnFinTot+ TotQty
      lnFinAmnt =lnFinAmnt + (TotQty * Price ) 
    ENDIF
    *----------------------------------------------------
    * [DOORD] ACCOUNT/ORDER SUB TOTALS
    *----------------------------------------------------
    IF DOORD
      @ ROW,000 SAY '***'
      @ ROW,006 SAY LEFT(CTLOR,5)
      @ ROW,012 SAY SUBSTR(CTLOR,6,6)
      @ ROW,019 SAY 'CUST. PO# :'
      @ ROW,031 SAY RIGHT(CTLOR,10)
      *---C101826,1 sizes are 8 instead of 4 AME[start] 
      *@ ROW,074 SAY ORDTOT(1,1)  PICTURE '@Z 99999'
      *@ ROW,080 SAY ORDTOT(1,2)  PICTURE '@Z 99999'
      *@ ROW,086 SAY ORDTOT(1,3)  PICTURE '@Z 99999'
      *@ ROW,092 SAY ORDTOT(1,4)  PICTURE '@Z 99999'
      @ ROW,050 SAY ORDTOT(1,1)  PICTURE '@Z 99999'
      @ ROW,056 SAY ORDTOT(1,2)  PICTURE '@Z 99999'
      @ ROW,062 SAY ORDTOT(1,3)  PICTURE '@Z 99999'
      @ ROW,068 SAY ORDTOT(1,4)  PICTURE '@Z 99999'
      @ ROW,074 SAY ORDTOT(1,5)  PICTURE '@Z 99999'
      @ ROW,080 SAY ORDTOT(1,6)  PICTURE '@Z 99999'
      @ ROW,086 SAY ORDTOT(1,7)  PICTURE '@Z 99999'
      @ ROW,092 SAY ORDTOT(1,8)  PICTURE '@Z 99999'
      *@ ROW,098 SAY ORDTOT(1,9)  PICTURE '9999999' 
      @ ROW,097 SAY ORDTOT(1,9)  PICTURE '9999999'
      *@ ROW,118 SAY ORDTOT(1,10)  PICTURE '$9999999.99'
      @ ROW,117 SAY ORDTOT(1,10)  PICTURE '$9999999.99'
      *---C101826,1 AME[end]
      ROW = ROW+2
      CTLOR = ACCOUNT+ORDER+IIF(OrdHdr.MULTIPO,'*MULTI-PO*',OrdHdr.CUSTPO)
      ORDTOT = 0.00
      IF lcNote
        XPRV_ROW=ROW
        XREPORT='SOEIL140'
        DO PRT_NOTE WITH 'B',lcOrder
        ROW=IIF(ROW=XPRV_ROW,ROW,ROW+1)   && INCREMNET ROW ONLY IF NOTEPAD PRINTED
        lcOrder=ORDER
      ENDIF
    ENDIF
    
    *----------------------------------------------------
    * [LEVEL 1] STYLE SUB TOTALS
    *----------------------------------------------------
    IF LEVEL >=1
      NEWSTYLE = .F.
      * PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F
      IF lcSortCode $ 'SF'
        @ ROW,000 SAY REPLICATE('-',132)
        ROW = ROW+1
        @ ROW,000 SAY SUBSTR(lcStyDesc,1,12)
        @ ROW,013 SAY '('+lcStyFab+')'          
        @ ROW,026 SAY lcStyle
        @ ROW,039 SAY '******'
        
        *C101826,1 add additional 4 sies in case of sorting by style AME[Start]
        *@ ROW,074 SAY XTOTAL(1,1)  PICTURE '@Z 99999'
        *@ ROW,080 SAY XTOTAL(1,2)  PICTURE '@Z 99999'
        *@ ROW,086 SAY XTOTAL(1,3)  PICTURE '@Z 99999'
        *@ ROW,092 SAY XTOTAL(1,4)  PICTURE '@Z 99999'
        *@ ROW,98 SAY XTOTAL(1,9)  PICTURE '9999999'
        *@ ROW,118 SAY XTOTAL(1,10)  PICTURE '$9999999.99'
        @ ROW,065 SAY XTOTAL(1,1)  PICTURE '@Z 99999'
        @ ROW,071 SAY XTOTAL(1,2)  PICTURE '@Z 99999'
        @ ROW,077 SAY XTOTAL(1,3)  PICTURE '@Z 99999'
        @ ROW,083 SAY XTOTAL(1,4)  PICTURE '@Z 99999'
        @ ROW,089 SAY XTOTAL(1,5)  PICTURE '@Z 99999'
        @ ROW,095 SAY XTOTAL(1,6)  PICTURE '@Z 99999'
        @ ROW,101 SAY XTOTAL(1,7)  PICTURE '@Z 9999'
        @ ROW,106 SAY XTOTAL(1,8)  PICTURE '@Z 9999'
        @ ROW,110 SAY XTOTAL(1,9)  PICTURE '9999999'
        @ ROW,119 SAY XTOTAL(1,10)  PICTURE '$9999999.99'
        *C101826,1 AME[End]
        ROW = ROW+1
        @ ROW,000 SAY REPLICATE('-',132)
        ROW = ROW+2
        lcStyle = SUBSTR(Style,1,lnMajPic)
        lcStyDesc  = STYLE.DESC
        lcStyFab   = STYLE.FABRIC
        *B803661,1 (Begin) Don't update it here.
        *lcScale   = STYLE.SCALE
        *B803661,1 (End)
        NEWSTYLE = .T.
      ENDIF
      X = 1
      DO WHILE X<=10
        XTOTAL(2,X) = XTOTAL(2,X) + XTOTAL(1,X)
        XTOTAL(3,X) = XTOTAL(3,X) + XTOTAL(1,X)
        XTOTAL(1,X) =0.00
        X = X+1
      ENDDO
    ENDIF
    *------------------ END USER DEFINED SUB TOTALS -------------------
    *----------------------------------------------------
    * [LEVEL 2] USER DEFINED SUB TOTALS
    *----------------------------------------------------
    IF BREAK<>' ' .AND. LEVEL >=2
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+1
      IF lcSortCode='O'
        @ ROW,000 SAY SUBSTR(HBREAK,10,5)
        @ ROW,006 SAY SUBSTR(HBREAK,1,6)
      ELSE
        IF lcSortCode='R'   
          SELE SALESREP
          SEEK HBREAK
            XNAME=NAME
            SELECT &TEMP
        ELSE
          XNAME=''
        ENDIF
        @ ROW,000 SAY HBREAK+' '+XNAME
        IF lcSortCode='P'   
          lnPerCent = (lnTotalPik/lnTotalQty)*100
          @ ROW,020 SAY "%Allocated:"
          @ ROW,035 SAY lnPerCent
          STORE 0 TO lnTotalQty,lnTotalPik,lnPerCent
        ENDIF  
      ENDIF
      IF LFCR $ HBREAK
        ROW = ROW+1
      ENDIF
      *---C101826,1 sizes are 8 instead of 4 in case of "APO" AME[start] 
      *@ ROW,074 SAY XTOTAL(2,1)  PICTURE '@Z 99999'
      *@ ROW,080 SAY XTOTAL(2,2)  PICTURE '@Z 99999'
      *@ ROW,086 SAY XTOTAL(2,3)  PICTURE '@Z 99999'
      *@ ROW,092 SAY XTOTAL(2,4)  PICTURE '@Z 99999'
      IF lcSortCode $ "APO"
        @ ROW,050 SAY XTOTAL(2,1)  PICTURE '@Z 99999'
        @ ROW,056 SAY XTOTAL(2,2)  PICTURE '@Z 99999'
        @ ROW,062 SAY XTOTAL(2,3)  PICTURE '@Z 99999'
        @ ROW,068 SAY XTOTAL(2,4)  PICTURE '@Z 99999'
        @ ROW,074 SAY XTOTAL(2,5)  PICTURE '@Z 99999'
        @ ROW,080 SAY XTOTAL(2,6)  PICTURE '@Z 99999'
        @ ROW,086 SAY XTOTAL(2,7)  PICTURE '@Z 99999'
        @ ROW,092 SAY XTOTAL(2,8)  PICTURE '@Z 99999'
      ELSE
        *C101826,1 Add additional 4 sizes in case of sorting by style or fabric AME [Start]
        *@ ROW,074 SAY XTOTAL(2,1)  PICTURE '@Z 99999'
        *@ ROW,080 SAY XTOTAL(2,2)  PICTURE '@Z 99999'
        *@ ROW,086 SAY XTOTAL(2,3)  PICTURE '@Z 99999'
        *@ ROW,092 SAY XTOTAL(2,4)  PICTURE '@Z 99999'
        IF lcSortCode $ "SF"
          @ ROW,065 SAY XTOTAL(2,1)  PICTURE '@Z 99999'
          @ ROW,071 SAY XTOTAL(2,2)  PICTURE '@Z 99999'
          @ ROW,077 SAY XTOTAL(2,3)  PICTURE '@Z 99999'
          @ ROW,083 SAY XTOTAL(2,4)  PICTURE '@Z 99999'
          @ ROW,089 SAY XTOTAL(2,5)  PICTURE '@Z 99999'
          @ ROW,095 SAY XTOTAL(2,6)  PICTURE '@Z 99999'
          @ ROW,101 SAY XTOTAL(2,7)  PICTURE '@Z 9999'
          @ ROW,106 SAY XTOTAL(2,8)  PICTURE '@Z 9999'
        ELSE
          @ ROW,074 SAY XTOTAL(2,1)  PICTURE '@Z 99999'
          @ ROW,080 SAY XTOTAL(2,2)  PICTURE '@Z 99999'
          @ ROW,086 SAY XTOTAL(2,3)  PICTURE '@Z 99999'
          @ ROW,092 SAY XTOTAL(2,4)  PICTURE '@Z 99999'
        ENDIF
        *C101826,1 AME [End]
      ENDIF
      *---C101826,1 AME[end] 
      *C101826,1  Add additional 4 sizes in case of sorting by style or fabric AME [Start]
      *@ ROW,98 SAY XTOTAL(2,9)  PICTURE '9999999'
      *@ ROW,118 SAY XTOTAL(2,10)  PICTURE '$9999999.99'
      IF lcSortCode $ "SF"
        @ ROW,110 SAY XTOTAL(2,9)  PICTURE '9999999'
        @ ROW,119 SAY XTOTAL(2,10)  PICTURE '$9999999.99'
      ELSE
        @ ROW,97 SAY XTOTAL(2,9)  PICTURE '9999999'
        @ ROW,117 SAY XTOTAL(2,10)  PICTURE '$9999999.99'
      ENDIF  
      *C101826,1 AME[End]
      ROW = ROW+1
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+2
      HBREAK = &BREAK
      X = 1
      DO WHILE X<=10
        XTOTAL(2,X) =0.00
        X = X+1
      ENDDO
    ENDIF
    *------------------ END USER DEFINED SUB TOTALS -------------------
    
    IF EOF(lnAlias)
      EXIT
    ENDIF

    *---C101826,1 Print Order Header , AME[start] 
    *----------------------------------------------------
    * [DOORN] ORDER# Header
    *----------------------------------------------------
    IF DOORN    
      ROW= ROW+1
      *B803405,1 ABD Change the words "Status" and "Priority" to be printed in capital letters. [Begin]
      *@ ROW,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  Status Priority'
       @ ROW,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  STATUS PRIORITY'
      *B803405,1 ABD [END]
      ROW= ROW+1
      @ ROW,00 SAY REPLICATE('-',132)        
      ROW= ROW+1
      *B803405,1 ABD Make the store# printed closer to where the style/ color is printed. 
      *B803405,1 ABD Move the store # to the right (about 10 characters) [Begin]
      *@ ROW,000 SAY 'STORE #                       STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
       @ ROW,000 SAY '          STORE #             STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
      *B803405,1 ABD [END]
      ROW= ROW+1
      @ ROW,000 SAY REPLICATE('-',132)
      ROW= ROW+1 
      @ ROW , 00 SAY ordhdr.Account 
      @ ROW , 07 SAY LEFT(CUSTOMER.Stname,15)
      @ ROW , 23 SAY ORDER
      @ ROW , 30 SAY ORDHDR.Entered
      @ ROW , 40 SAY SUBSTR(IIF(OrdHdr.MULTIPO,&temp..custpo,OrdHdr.CUSTPO),1,10)
      @ ROW , 51 SAY OrdHdr.Dept
      @ ROW , 57 SAY OrdHdr.START 
      @ ROW , 67 SAY OrdHdr.COMPLETE 
      @ ROW , 77 SAY OrdHdr.STATUS
      @ ROW , 84 SAY ALLTRIM(OrdHdr.PRIORITY)
      ROW = ROW+2
      XOrd = ORDER
    ENDIF
    *---C101826,1 AME[end] 

    *------------------------------------------------
    * PROCESS DETAIL RECORD
    *------------------------------------------------
    *---C101826,1 Print The Sizes header every time the style changes , AME[Start] 
    *B803661,1 (Begin) Print scale sizes desc when it changes.
    *IF NEWSTYLE .OR.   LEVEL >= 1
    IF (NEWSTYLE .OR.   LEVEL >= 1) AND SCALE <> lcScale
    *B803661,1 (End)
    *IF NEWSTYLE 
    *---C101826,1 AME[end] 
      IF ROW >=54
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'SOEIL140','(SUB TOTALS BY: ' + CTLSEQ +') '+ xTitle,R_WIDTH
        *---C101826,1 Print the header in case of Acc. , Prio. ,Order ,Style Or  Fabric AME[start] 
        IF lcSortCode = 'S'
          IF !llSummarize        
            @ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    SP  CUSTOMER     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          ELSE
            @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          ENDIF 
          @ 06,000 SAY REPLICATE('-',132)
          ROW = 7
        ELSE
          IF lcSortCode $ "APO"
          
           *B803405,1 ABD Change the words "Status" and "Priority" to be printed in capital letters. [Begin]
            *@ 05,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  Status Priority'
             @ 05,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  STATUS PRIORITY'
            *B803405,1 ABD [END]
            @ 06,00 SAY REPLICATE('-',132)
            *B803405,1 ABD Make the store# printed closer to where the style/ color is printed. 
            *B803405,1 ABD Move the store # to the right (about 10 characters) [Begin]
            *@ 07,000 SAY 'STORE #                       STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
            @ 07,000 SAY '          STORE #              STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
            *B803405,1 ABD [End]
            @ 08,000 SAY REPLICATE('-',132)
            @ 09 , 00 SAY ordhdr.Account 
            @ 09  , 07 SAY LEFT(CUSTOMER.Stname,15)
            @ 09  , 23 SAY ORDER
            @ 09  , 30 SAY ORDHDR.Entered
            @ 09  , 40 SAY SUBSTR(IIF(OrdHdr.MULTIPO,&temp..custpo,OrdHdr.CUSTPO),1,10)
            @ 09  , 51 SAY OrdHdr.Dept
            @ 09  , 57 SAY OrdHdr.START 
            @ 09  , 67 SAY OrdHdr.COMPLETE 
            @ 09  , 77 SAY OrdHdr.STATUS
            @ 09  , 84 SAY ALLTRIM(OrdHdr.PRIORITY)
            @ 10,000 SAY REPLICATE('-',132)
            ROW = 11
          ELSE
            IF lcSortCode = "F"
              @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    SP  STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
            ELSE
              @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  STYLE        COLOR   (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
            ENDIF  
            @ 06,000 SAY REPLICATE('-',132)
            ROW = 7
          ENDIF
        ENDIF
        *IF lcSortCode='S' .AND. !llSummarize        

            *E301249,1 Make priority field C(3) [start]
            *@ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER      (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
       *     @ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER     (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
            *E301249,1 [end]
            
        *ELSE
        *   IF lcSortCode = 'S'

              *E301249,1 Make priority field C(3) [start]
              *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER     STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
        *      @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
              *E301249,1 [end]

        *   ELSE

              *E301249,1 Make priority field C(3) [start]
              *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
        *      @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  STYLE        COLOR   (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
              *E301249,1 [end]

        *    ENDIF
        *ENDIF    
        *@ 06,000 SAY REPLICATE('-',132)
        *ROW = 7
      *---C101826,1 AME[end] 
      ENDIF
      NEWSTYLE = .F.
      lcScale = Style.Scale

      SELECT SCALE
      SEEK 'S'+lcScale
      IF ROW >=53             &&start a new page to print size descriptions in top of page.
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'SOEIL140','(SUB TOTALS BY: ' + CTLSEQ +') '+ xTitle,R_WIDTH
        *---C101826,1 Print the header in case of Acc. , Prio. ,Order ,Style Or  Fabric AME[start] 
        *---C101826,1 we also select temp & then reselect scale AME[start] 
        SELECT &TEMP  
        IF lcSortCode = 'S'
          IF !llSummarize        
            @ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    SP  CUSTOMER     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          ELSE
            @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          ENDIF 
          @ 06,000 SAY REPLICATE('-',132)
          ROW = 7
        ELSE
          IF lcSortCode $ "APO"
            *B803405,1 ABD Change the words "Status" and "Priority" to be printed in capital letters. [Begin]
            *@ 05,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  Status Priority'
             @ 05,000 SAY 'ACCT   ACCT.NAME       ORDER  ENTERED   CUST PO#   DEPT. START     COMPLETE  STATUS PRIORITY'
            *B803405,1 ABD [END]
            @ 06,00 SAY REPLICATE('-',132)
            *B803405,1 ABD Make the store# printed closer to where the style/ color is printed. 
            *B803405,1 ABD Move the store # to the right (about 10 characters) [Begin]
            *@ 07,000 SAY 'STORE #                       STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
            @ 07,000 SAY '          STORE #              STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES  PRICE  PIKTKT DYELOT.'
            *B803405,1 ABD [End]
            @ 08,000 SAY REPLICATE('-',132)
            @ 09 , 00 SAY ordhdr.Account 
            @ 09  , 07 SAY LEFT(CUSTOMER.Stname,15)
            @ 09  , 23 SAY ORDER
            @ 09  , 30 SAY ORDHDR.Entered
            @ 09  , 40 SAY SUBSTR(IIF(OrdHdr.MULTIPO,&temp..custpo,OrdHdr.CUSTPO),1,10)
            @ 09  , 51 SAY OrdHdr.Dept
            @ 09  , 57 SAY OrdHdr.START 
            @ 09  , 67 SAY OrdHdr.COMPLETE 
            @ 09  , 77 SAY OrdHdr.STATUS
            @ 09  , 84 SAY ALLTRIM(OrdHdr.PRIORITY)
            @ 10,000 SAY REPLICATE('-',132)
            ROW = 11
          ELSE
            IF lcSortCode = "F"
              @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    SP  STYLE        COLOR   (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
            ELSE
              @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  STYLE        COLOR   (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
            ENDIF  
            @ 06,000 SAY REPLICATE('-',132)
            ROW = 7
          ENDIF
        ENDIF
        
        *IF lcSortCode = 'S'
        *  IF !llSummarize        

            *E301249,1 Make priority field C(3) [start]
            *@ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER      (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
        *    @ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER     (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
            *E301249,1 [end]

        * ELSE

            *E301249,1 Make priority field C(3) [start]
            *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER     STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
        *    @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
            *E301249,1 [end]

        *  ENDIF    
        *ELSE

          *E301249,1 Make priority field C(3) [start]
          *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
        *  @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  STYLE        COLOR   (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
          *E301249,1 [end]

        *ENDIF
        *@ 06,000 SAY REPLICATE('-',132)
        *ROW = 7
        SELECT SCALE
      *---C101826,1 AME[end]   
      ENDIF
      *C101826,1 add 4 additional sizes in case of style & Fabric AME[Start]
      *IF lcSortCode='S' .AND. !llSummarize
        *@ ROW,074 SAY SZ1
        *@ ROW,080 SAY SZ2
        *@ ROW,086 SAY SZ3
        *@ ROW,092 SAY SZ4
      IF lcSortCode $ "SF"
        @ ROW,065 SAY SZ1
        @ ROW,071 SAY SZ2
        @ ROW,077 SAY SZ3
        @ ROW,083 SAY SZ4
        @ ROW,089 SAY SZ5
        @ ROW,095 SAY SZ6
        @ ROW,101 SAY SZ7
        @ ROW,106 SAY SZ8
      *C101826,1 AME[End]   
      ELSE
      *---C101826,1 sizes are 8 instead of 4 in case of "APO" AME[start] 
        *@ ROW,074 SAY SZ1
        *@ ROW,080 SAY SZ2
        *@ ROW,086 SAY SZ3
        *@ ROW,092 SAY SZ4
        IF lcSortCode $ "APO"
          @ ROW,050 SAY SZ1
          @ ROW,056 SAY SZ2
          @ ROW,062 SAY SZ3
          @ ROW,068 SAY SZ4
          @ ROW,074 SAY SZ5
          @ ROW,080 SAY SZ6
          @ ROW,086 SAY SZ7
          @ ROW,092 SAY SZ8
        ELSE
          @ ROW,074 SAY SZ1
          @ ROW,080 SAY SZ2
          @ ROW,086 SAY SZ3
          @ ROW,092 SAY SZ4
        ENDIF
        *---C101826,1 AME[end]   
      ENDIF  
      ROW=ROW+1
    ENDIF
    SELE &TEMP
    IF lcSortCode='S' .AND. XPRV_KEY <> STYLE
      XPRV_KEY=STYLE
      SELE STYLE
      SEEK XPRV_KEY
      IF FOUND()
        IF ROW >=54 &&start a new page to get style and its quantities in one page.
          PAGENO = PAGENO +1
          DO RPT_HDR WITH 'SOEIL140','(SUB TOTALS BY: ' + CTLSEQ +') '+ xTitle,R_WIDTH
          *---C101826,1 Print the header in case of Acc. , Prio.  Or  Orer AME[start] 
          *---C101826,1 we also select temp & then reselect Style AME[start] 
          SELECT &TEMP
          IF !llSummarize        
              @ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    SP  CUSTOMER     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          ELSE
            @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   (5)   (6)  (7)  (8)   PIECES PIKTKT DYE.'
          ENDIF 
          @ 06,000 SAY REPLICATE('-',132)
          ROW = 7

          *IF lcSortCode = 'S'
          *  IF !llSummarize        
             
              *E301249,1 Make priority field C(3) [start]
              *@ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER      (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
          *    @ 05,000 SAY 'ACCT  ORDER  STORE   ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER     (1)   (2)   (3)   (4)   PIECES         PIKTKT DYELOT.'
              *E301249,1 [end]
            
          *  ELSE
             
              *E301249,1 Make priority field C(3) [start]
              *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP CUSTOMER     STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
          *   @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  CUSTOMER    STR#     (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
              *E301249,1 [end]
            
          *  ENDIF   
          *ELSE

            *E301249,1 Make priority field C(3) [start]
            *@ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
          *  @ 05,000 SAY 'ACCT  ORDER  ENTERED  CUST PO#  START    COMPLETE SP  STYLE        COLOR   (1)   (2)   (3)   (4)   PIECES  PRICE  PIKTKT DYELOT.'
            *E301249,1 [end]
            
          *ENDIF
          *@ 06,000 SAY REPLICATE('-',132)
          *ROW = 7
          SELECT STYLE
          *---C101826,1 AME[end]  
        ENDIF
        lcColor = SUBSTR(&TEMP..Style,14,6)
        lcClrDes = gfCodDes(lcColor,'COLOR     ')
        SELE STYLE
        @ ROW,000 SAY STYLE
        @ ROW,020 SAY SUBSTR(lcClrDes,1,10)
        DO CASE
          CASE XPRTWPSTK = 'S'
            @ ROW,032 SAY 'STOCK :'
            *C101826,1 Add 4 ADDITIONAL SIZES IN CASE OF SORTING by Style or Fabric AME[Start]
            *@ ROW,074 SAY STK1  PICTURE '@Z 99999'
            *@ ROW,080 SAY STK2  PICTURE '@Z 99999'
            *@ ROW,086 SAY STK3  PICTURE '@Z 99999'
            *lnTotStk = STK4+STK5+STK6+STK7+STK8
            *@ ROW,092 SAY lnTotStk  PICTURE '@Z 99999'
            *@ ROW,98 SAY TOTSTK  PICTURE '9999999'
            @ ROW,065 SAY STK1  PICTURE '@Z 99999'
            @ ROW,071 SAY STK2  PICTURE '@Z 99999'
            @ ROW,077 SAY STK3  PICTURE '@Z 99999'
            @ ROW,083 SAY STK4  PICTURE '@Z 99999'
            @ ROW,089 SAY STK5  PICTURE '@Z 99999'
            @ ROW,095 SAY STK6  PICTURE '@Z 99999'
            @ ROW,101 SAY STK7  PICTURE '@Z 9999'
            @ ROW,106 SAY STK8  PICTURE '@Z 9999'
            @ ROW,110 SAY TOTSTK  PICTURE '9999999'
            *C101826,1 AME[End]
          CASE XPRTWPSTK = 'W'           
            @ ROW,032 SAY 'WIP + STOCK :'
            *C101826,1 Add 4 ADDITIONAL SIZES IN CASE OF SORTING by Style or Fabric AME[Start]
            *@ ROW,074 SAY WIP1+STK1  PICTURE '@Z 999999'
            *@ ROW,080 SAY WIP2+STK2  PICTURE '@Z 999999'
            *@ ROW,086 SAY WIP3+STK3  PICTURE '@Z 999999'
            *lnTotWipStk= (Wip4+STK4)+(Wip5+STK5)+(Wip6+STK6)+(Wip7+STK7)+(Wip8+STK8)
            *@ ROW,092 SAY lnTotWipStk  PICTURE '@Z 999999'
            *@ ROW,98 SAY TOTWIP+TOTSTK  PICTURE '9999999'
            @ ROW,065 SAY WIP1+STK1  PICTURE '@Z 99999'
            @ ROW,071 SAY WIP2+STK2  PICTURE '@Z 99999'
            @ ROW,077 SAY WIP3+STK3  PICTURE '@Z 99999'
            @ ROW,083 SAY WIP4+STK4  PICTURE '@Z 99999'
            @ ROW,089 SAY WIP5+STK5  PICTURE '@Z 99999'
            @ ROW,095 SAY WIP6+STK6  PICTURE '@Z 99999'
            @ ROW,101 SAY WIP7+STK7  PICTURE '@Z 9999'
            @ ROW,106 SAY WIP8+STK8  PICTURE '@Z 9999'
            @ ROW,110 SAY TOTWIP+TOTSTK  PICTURE '9999999'
            *C101826,1 AME[End]
          CASE XPRTWPSTK = 'I'             
            @ ROW,032 SAY 'WIP :'
            *C101826,1 Add 4 ADDITIONAL SIZES IN CASE OF SORTING by Style or Fabric AME[Start]
            *@ ROW,074 SAY WIP1  PICTURE '@Z 99999'
            *@ ROW,080 SAY WIP2  PICTURE '@Z 99999'
            *@ ROW,086 SAY WIP3  PICTURE '@Z 99999'
            *lnTotWip = WIP4+WIP5+WIP6+WIP7+WIP8
            *@ ROW,092 SAY lnTotWip  PICTURE '@Z 99999'
            *@ ROW,98 SAY TOTWIP  PICTURE '9999999'
            @ ROW,065 SAY WIP1  PICTURE '@Z 99999'
            @ ROW,071 SAY WIP2  PICTURE '@Z 99999'
            @ ROW,077 SAY WIP3  PICTURE '@Z 99999'
            @ ROW,083 SAY WIP4  PICTURE '@Z 99999'
            @ ROW,089 SAY WIP5  PICTURE '@Z 99999'
            @ ROW,095 SAY WIP6  PICTURE '@Z 99999'
            @ ROW,101 SAY WIP7  PICTURE '@Z 9999'
            @ ROW,106 SAY WIP8  PICTURE '@Z 9999'
            @ ROW,110 SAY TOTWIP  PICTURE '9999999'
            *C101826,1 AME[End]
        ENDCASE
        ROW = ROW+1
      ENDIF
      SELE &TEMP
    ENDIF
    *********************************************************************
    ****************** PRINT THE LINE ITEM DETAIL ***********************
    *********************************************************************
    SELECT &TEMP
    IF lcSortCode='S' .AND. !llSummarize
       @ ROW,000 SAY ACCOUNT
       @ ROW,006 SAY ORDER
       @ ROW,013 SAY IIF(OrdHdr.MULTI='Y',&temp..Store,OrdHdr.STORE)
       @ ROW,021 SAY OrdHdr.ENTERED &&9
       @ ROW,030 SAY SUBSTR(IIF(OrdHdr.MULTIPO,&temp..custpo,OrdHdr.CUSTPO),1,10)
       @ ROW,040 SAY OrdHdr.START  &&9
       *C101826,1 Rmove complete date to add 4 sizes AME[Start]
       *@ ROW,049 SAY OrdHdr.COMPLETE &&9
       *@ ROW,058 SAY OrdHdr.STATUS &&1
       @ ROW,049 SAY OrdHdr.STATUS &&1 
       *C101826,1 AME[End] 
       
       *E301249,1 Make priority field C(3) [start]
       *@ ROW,059 SAY OrdHdr.PRIORITY  &&2
       *@ ROW,061 SAY SUBSTR(CUSTOMER.STNAME,1,10) &&2
       
       *C101826,1 Rmove complete date to add 4 sizes AME[Start]
       *@ ROW,059 SAY ALLTRIM(OrdHdr.PRIORITY)  &&3
       @ ROW,050 SAY ALLTRIM(OrdHdr.PRIORITY)  &&3
       *@ ROW,062 SAY SUBSTR(CUSTOMER.STNAME,1,10) &&2
       @ ROW,053 SAY SUBSTR(CUSTOMER.STNAME,1,10) &&2
       *C101826,1 AME[End]
       
       *E301249,1 [end]
       
    ELSE
         *---C101826,1 in case of "APO" print just Store AME[start] 
         *@ ROW,000 SAY ACCOUNT
         *@ ROW,006 SAY ORDER
         *@ ROW,013 SAY OrdHdr.entered
         *@ ROW,022 SAY SUBSTR(IIF(OrdHdr.MULTIPO,&temp..custpo,OrdHdr.CUSTPO),1,10)
         *@ ROW,032 SAY OrdHdr.START
         *@ ROW,041 SAY OrdHdr.COMPLETE
         *@ ROW,050 SAY OrdHdr.STATUS
       
         *E301249,1 Make priority field C(3) [start]
         *@ ROW,051 SAY OrdHdr.PRIORITY
      *   @ ROW,051 SAY ALLTRIM(OrdHdr.PRIORITY)
         *E301249,1 [end]
      IF lcSortCode $ "APO"
         *B803405,1 ABD Make the store# printed closer to where the style/ color is printed. 
         *B803405,1 ABD Move the store # to the right (about 10 characters) [Begin]
         *@ ROW,000 SAY IIF(OrdHdr.MULTI='Y',&temp..Store,OrdHdr.STORE)
        @ ROW,010 SAY IIF(OrdHdr.MULTI='Y',&temp..Store,OrdHdr.STORE)
         *B803405,1 ABD [End]
       ELSE 
         @ ROW,000 SAY ACCOUNT
         @ ROW,006 SAY ORDER
         @ ROW,013 SAY OrdHdr.entered
         @ ROW,022 SAY SUBSTR(IIF(OrdHdr.MULTIPO,&temp..custpo,OrdHdr.CUSTPO),1,10)
         @ ROW,032 SAY OrdHdr.START
         *C101826,1 Rmove complete date to add 4 sizes in case of style or fabric sorting AME[Start]  
         *@ ROW,041 SAY OrdHdr.COMPLETE
         *@ ROW,050 SAY OrdHdr.STATUS
         *@ ROW,051 SAY ALLTRIM(OrdHdr.PRIORITY)
         IF lcSortCode $ "SF"
           @ ROW,041 SAY OrdHdr.STATUS
           @ ROW,042 SAY ALLTRIM(OrdHdr.PRIORITY)
         ELSE
           @ ROW,041 SAY OrdHdr.COMPLETE
           @ ROW,050 SAY OrdHdr.STATUS
           @ ROW,051 SAY ALLTRIM(OrdHdr.PRIORITY)
         ENDIF  
         *C101826,1  AME[End]
       ENDIF
       *---C101826,1 AME[end] 
       IF lcSortCode = 'S'
         
         *E301249,1 Make priority field C(3) [start]
         *@ ROW,053 SAY SUBSTR(CUSTOMER.STNAME,1,10)
         @ ROW,045 SAY SUBSTR(CUSTOMER.STNAME,1,10)
         *E301249,1 [end]
         *B803970,1 AME [Start]  
         *@ ROW,056 SAY IIF(OrdHdr.MULTI='Y',&temp..Store,OrdHdr.STORE)
         @ ROW,056 SAY IIF(XSUM_STORE,'',IIF(OrdHdr.MULTI='Y',&temp..Store,OrdHdr.STORE))
         *B803970,1 AME [End]  
       ELSE
       *---C101826,1 adjust place of printing style AME[start] 
         IF lcSortCode $ "APO"
           @ ROW,030 SAY STYLE           
         ELSE  
           *C101826,1  add 4 sizes in case of style or fabric sorting AME[Start]  
           *@ ROW,054 SAY STYLE
           IF lcSortCode = "F"  
             @ ROW,045 SAY STYLE 
           ELSE
             @ ROW,054 SAY STYLE
           ENDIF
           *C101826,1  AME[End]
         ENDIF  
         *E301249,1 Make priority field C(3) [start]
         *@ ROW,053 SAY STYLE
       *  @ ROW,054 SAY STYLE
         *E301249,1 [end]
       *---C101826,1 AME[end]   
       ENDIF
    ENDIF
    *--Check condition of printing
      XSUM_STORE=IIF(llSummarize .AND. OrdHdr.MULTI='Y',.T.,.F.)
      IF XSUM_STORE
        STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,XTOTQTY,XAMT
        xStyle =STYLE
        XCOMDT=DTOS(OrdHdr.COMPLETE)
        lcOrder=ORDER
        lcStore = STORE
        *B803970,1 AME [Start]  this case is Summ. Mult Store Order
        *B803970,1 AME          it shuldn't scan for the same store

        *SCAN WHILE STYLE+DTOS(OrdHdr.COMPLETE)+ORDER+STORE = xStyle+;
                    XCOMDT+lcOrder+lcStore
        
        SCAN WHILE STYLE+DTOS(OrdHdr.COMPLETE)+ORDER+STORE = xStyle+;
                    XCOMDT+lcOrder
        
        *B803970,1 AME [End]  
          *C101826,1  add 4 sizes in case of style or fabric sorting AME[Start]  
          *FOR XMCOUNT = 1 TO 3
            *Z=STR(XMCOUNT,1)
            *XQTY&Z=XQTY&Z+QTY&Z
          *ENDFOR
          *XQTY4=XQTY4+QTY4+QTY5+QTY6+QTY7+QTY8             
          FOR XMCOUNT = 1 TO 8
            Z=STR(XMCOUNT,1)
            XQTY&Z=XQTY&Z+QTY&Z
          ENDFOR
          XTOTQTY=XTOTQTY+TOTQTY
          XAMT   =XAMT   +(TOTQTY*PRICE)
        ENDSCAN
        *C101826,1  AME[End]
        
        XTOTQTY=XQTY1+XQTY2+XQTY3+XQTY4+XQTY5+XQTY6+XQTY7+XQTY8
        
        *C101826,1  add 4 sizes in case of style or fabric sorting AME[Start]  
        *@ ROW,074 SAY XQTY1    PICTURE '@Z 99999'
        *@ ROW,080 SAY XQTY2    PICTURE '@Z 99999'
        *@ ROW,086 SAY XQTY3    PICTURE '@Z 99999'
        
        *B803970,1 AME [Start]  Print Totals (Xqty) instead of qty
        
        *@ ROW,065 SAY QTY1    PICTURE '@Z 99999'
        *@ ROW,071 SAY QTY2    PICTURE '@Z 99999'
        *@ ROW,077 SAY QTY3    PICTURE '@Z 99999'
        *@ ROW,083 SAY QTY4    PICTURE '@Z 99999'
        *@ ROW,089 SAY QTY5    PICTURE '@Z 99999'
        *@ ROW,095 SAY QTY6    PICTURE '@Z 99999'
        *@ ROW,0101 SAY QTY7    PICTURE '@Z 9999'
        *@ ROW,0106 SAY QTY8    PICTURE '@Z 9999'
        
        @ ROW,065 SAY XQTY1    PICTURE '@Z 99999'
        @ ROW,071 SAY XQTY2    PICTURE '@Z 99999'
        @ ROW,077 SAY XQTY3    PICTURE '@Z 99999'
        @ ROW,083 SAY XQTY4    PICTURE '@Z 99999'
        @ ROW,089 SAY XQTY5    PICTURE '@Z 99999'
        @ ROW,095 SAY XQTY6    PICTURE '@Z 99999'
        @ ROW,0101 SAY XQTY7    PICTURE '@Z 9999'
        @ ROW,0106 SAY XQTY8    PICTURE '@Z 9999'
        
        *B803970,1 AME [End]
        
        *lnTotX = XQTY4+XQTY5+XQTY6+XQTY7+XQTY8
        *@ ROW,092 SAY lnTotX    PICTURE '@Z 99999'
        *@ ROW,98 SAY XTOTQTY  PICTURE '9999999'
        @ ROW,110 SAY XTOTQTY  PICTURE '9999999'
        *FOR lnAdd = 1 TO 3
        FOR lnAdd = 1 TO 8
        *C101826,1 AME[end]
        
          lnAdd1 = ALLTRIM(STR(lnAdd))
          lnAllAdd&lnAdd1 = lnAllAdd&lnAdd1 + XQty&lnAdd1
        ENDFOR
        
        *C101826,1  add 4 sizes in case of style or fabric sorting AME[Start]  
        *lnAllAdd4 = lnAllAdd4+XQty4+XQty5+XQty6+XQty7+XQty8
        *C101826,1 AME[end]
        
        lnFinTot =lnFinTot+XTotQty
        lnFinAmnt =lnFinAmnt + xAmt  
      ELSE
      *---C101826,1 sizes are 8 instead of 4 in case of "APO" AME[start] 
        *@ ROW,074 SAY QTY1    PICTURE '@Z 99999'
        *@ ROW,080 SAY QTY2    PICTURE '@Z 99999'
        *@ ROW,086 SAY QTY3    PICTURE '@Z 99999'
        *lnAllQty = QTY4+QTY5+QTY6+QTY7+QTY8
        *@ ROW,092 SAY lnAllQty    PICTURE '@Z 99999'
        IF lcSortCode $ "APO"
          @ ROW,050 SAY QTY1    PICTURE '@Z 99999'
          @ ROW,056 SAY QTY2    PICTURE '@Z 99999'
          @ ROW,062 SAY QTY3    PICTURE '@Z 99999'
          @ ROW,068 SAY QTY4    PICTURE '@Z 99999'
          @ ROW,074 SAY QTY5    PICTURE '@Z 99999'
          @ ROW,080 SAY QTY6    PICTURE '@Z 99999'
          @ ROW,086 SAY QTY7    PICTURE '@Z 99999'
          @ ROW,092 SAY QTY8    PICTURE '@Z 99999'
        ELSE
          IF lcSortCode $"SF"
            @ ROW,065 SAY QTY1    PICTURE '@Z 99999'
            @ ROW,071 SAY QTY2    PICTURE '@Z 99999'
            @ ROW,077 SAY QTY3    PICTURE '@Z 99999'
            @ ROW,083 SAY QTY4    PICTURE '@Z 99999'
            @ ROW,089 SAY QTY5    PICTURE '@Z 99999'
            @ ROW,095 SAY QTY6    PICTURE '@Z 99999'
            @ ROW,0101 SAY QTY7    PICTURE '@Z 9999'
            @ ROW,0106 SAY QTY8    PICTURE '@Z 9999'
          ELSE 
            @ ROW,074 SAY QTY1    PICTURE '@Z 99999'
            @ ROW,080 SAY QTY2    PICTURE '@Z 99999'
            @ ROW,086 SAY QTY3    PICTURE '@Z 99999'
            lnAllQty = QTY4+QTY5+QTY6+QTY7+QTY8
            @ ROW,092 SAY lnAllQty    PICTURE '@Z 99999'
          ENDIF  
        ENDIF
        *@ ROW,98 SAY TOTQTY  PICTURE '999999'
        IF lcSortCode $ "SF"
          @ ROW,111 SAY TOTQTY  PICTURE '999999'
        ELSE
          @ ROW,98 SAY TOTQTY  PICTURE '999999'
        ENDIF 

        *C101826,1   add 4 sizes in case of style or fabric sorting AME[Start]   
        *IF !(lcSortCode='S' .AND. !llSummarize) 
        IF !(lcSortCode $ "SF")
        *C101826,1 AME[end] 

          @ ROW,106 SAY PRICE   PICTURE '9999.99'
        ENDIF    

        *---C101826,1 AME[end] 
        *C101826,1 add 4 sizes in case of style or fabric sorting AME[Start]   
        *@ ROW,114 SAY PIKTKT
        *@ ROW,121 SAY DyeLot
        IF lcSortCode $ "SF"
          @ ROW,119 SAY PIKTKT 
          @ ROW,126 SAY LEFT(DyeLot,5)
        ELSE
          @ ROW,114 SAY PIKTKT
          @ ROW,121 SAY DyeLot
        ENDIF    
        *---C101826,1 AME[End]
     ENDIF
     ROW = ROW+1
     SELECT &TEMP
     IF llNotes .AND. lcNotes = 'Y' .AND. !llSummarize
       IF !EMPTY(NOTE_MEM)
         ROW = ROW + 1
         @ ROW , 00 SAY 'ORDER NOTES  :'
         FOR X = 1 TO MEMLINES(NOTE_MEM)
           @ ROW,15 SAY MLINE(NOTE_MEM,X)
           ROW = ROW + 1
        ENDFOR
       ENDIF
     ENDIF
     **********************************************************************
     ********************* END PRINT THE LINE DETAIL **********************
     **********************************************************************
     ***  sum qty under condition XSUM_STORE
     XTOTAL(1,1) = XTOTAL(1,1) +  IIF(XSUM_STORE,XQTY1,QTY1)
     XTOTAL(1,2) = XTOTAL(1,2) +  IIF(XSUM_STORE,XQTY2,QTY2)
     XTOTAL(1,3) = XTOTAL(1,3) +  IIF(XSUM_STORE,XQTY3,QTY3)
     
      *---C101826,1 sizes are 8 instead of 4 in case of "APOSF" AME[start] 
     *XTOTAL(1,4) = XTOTAL(1,4) +  IIF(XSUM_STORE,(XQTY4+XQTY5+XQTY6+XQTY7+XQTY8),(QTY4+QTY5+QTY6+QTY7+QTY8))      
     IF lcSortCode $ "APOSF"
       XTOTAL(1,4) = XTOTAL(1,4) + QTY4
       XTOTAL(1,5) = XTOTAL(1,5) + QTY5
       XTOTAL(1,6) = XTOTAL(1,6) + QTY6
       XTOTAL(1,7) = XTOTAL(1,7) + QTY7
       XTOTAL(1,8) = XTOTAL(1,8) + QTY8
     ELSE
       XTOTAL(1,4) = XTOTAL(1,4) +  IIF(XSUM_STORE,(XQTY4+XQTY5+XQTY6+XQTY7+XQTY8),(QTY4+QTY5+QTY6+QTY7+QTY8))      
     ENDIF
     *---C101826,1 AME[end] 
     XTOTAL(1,9) = XTOTAL(1,9) +  IIF(XSUM_STORE,XTOTQTY,TOTQTY)
     XTOTAL(1,10) = XTOTAL(1,10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
     IF lcSortCode = 'P'
       lnTotalQty = lnTotalQty + TotQty
       lnTotalPik = lnTotalPik + TotPik
     ENDIF
     IF lcSortCode = 'S'
       ***  sum qty under condition XSUM_STORE
       CLRTOT(1) = CLRTOT(1) +  IIF(XSUM_STORE,XQTY1,QTY1)
       CLRTOT(2) = CLRTOT(2) +  IIF(XSUM_STORE,XQTY2,QTY2)
       CLRTOT(3) = CLRTOT(3) +  IIF(XSUM_STORE,XQTY3,QTY3)
       
       *C101826,1 add 4 sizes in case of style or fabric sorting AME[Start]   
       *CLRTOT(4) = CLRTOT(4) +  IIF(XSUM_STORE,(XQTY4+XQTY5+XQTY6+XQTY7+XQTY8),(QTY4+QTY5+QTY6+QTY7+QTY8))
       CLRTOT(4) = CLRTOT(4) +  IIF(XSUM_STORE,XQTY4,QTY4)
       CLRTOT(5) = CLRTOT(5) +  IIF(XSUM_STORE,XQTY5,QTY5)
       CLRTOT(6) = CLRTOT(6) +  IIF(XSUM_STORE,XQTY6,QTY6)
       CLRTOT(7) = CLRTOT(7) +  IIF(XSUM_STORE,XQTY7,QTY7)
       CLRTOT(8) = CLRTOT(8) +  IIF(XSUM_STORE,XQTY8,QTY8)
       *C101826,1 AME[End]
       
       CLRTOT(9) = CLRTOT(9) +  IIF(XSUM_STORE,XTOTQTY,TOTQTY)
       CLRTOT(10) = CLRTOT(10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
     ENDIF
     IF lcSortCode = 'A'
        ORDTOT(1) = ORDTOT(1) +  QTY1
        ORDTOT(2) = ORDTOT(2) +  QTY2
        ORDTOT(3) = ORDTOT(3) +  QTY3
        *---C101826,1 sizes are 8 instead of 4 AME[start] 
        *ORDTOT(4) = ORDTOT(4) +  QTY4+QTY5+QTY6+QTY7+QTY8
        ORDTOT(4) = ORDTOT(4) +  QTY4
        ORDTOT(5) = ORDTOT(5) +  QTY5
        ORDTOT(6) = ORDTOT(6) +  QTY6
        ORDTOT(7) = ORDTOT(7) +  QTY7
        ORDTOT(8) = ORDTOT(8) +  QTY8
        *---C101826,1 AME[end] 
        ORDTOT(9) = ORDTOT(9) +  TOTQTY 
        ORDTOT(10) = ORDTOT(10) +  TOTQTY * PRICE
     ENDIF
     SELE &TEMP
     IF !XSUM_STORE   && Put SKIP under condition
         SKIP              && All cases except summurize case due to SCAN.
     ENDIF
  ENDDO
  *---------------------- END OF REPORT -------------------------
  *----------------------------------------------------
  * [LEVEL 3] GRAND TOTALS
  *----------------------------------------------------
  IF ROW >=60
    ROW = 5
  ENDIF
  @ ROW,000 SAY REPLICATE('=',132)
  ROW = ROW+1
  @ ROW,010 SAY 'GRAND TOTALS'
  *---C101826,1 sizes are 8 instead of 4 in case of "APOSF" AME[start] 
  *@ ROW,074 SAY XTOTAL(3,1)  PICTURE '@Z 99999'
  *@ ROW,080 SAY XTOTAL(3,2)  PICTURE '@Z 99999'
  *@ ROW,086 SAY XTOTAL(3,3)  PICTURE '@Z 99999'
  *@ ROW,092 SAY XTOTAL(3,4)  PICTURE '@Z 99999'
  IF lcSortCode $ "APO"
    @ ROW,050 SAY XTOTAL(3,1)  PICTURE '@Z 99999'
    @ ROW,056 SAY XTOTAL(3,2)  PICTURE '@Z 99999'
    @ ROW,062 SAY XTOTAL(3,3)  PICTURE '@Z 99999'
    @ ROW,068 SAY XTOTAL(3,4)  PICTURE '@Z 99999'
    @ ROW,074 SAY XTOTAL(3,5)  PICTURE '@Z 99999'
    @ ROW,080 SAY XTOTAL(3,6)  PICTURE '@Z 99999'
    @ ROW,086 SAY XTOTAL(3,7)  PICTURE '@Z 99999'
    @ ROW,092 SAY XTOTAL(3,8)  PICTURE '@Z 99999'
  ELSE
    IF lcSortCode $ "SF"
      @ ROW,065 SAY XTOTAL(3,1)  PICTURE '@Z 99999'
      @ ROW,071 SAY XTOTAL(3,2)  PICTURE '@Z 99999'
      @ ROW,077 SAY XTOTAL(3,3)  PICTURE '@Z 99999'
      @ ROW,083 SAY XTOTAL(3,4)  PICTURE '@Z 99999'
      @ ROW,089 SAY XTOTAL(3,5)  PICTURE '@Z 99999'
      @ ROW,095 SAY XTOTAL(3,6)  PICTURE '@Z 99999'
      @ ROW,101 SAY XTOTAL(3,7)  PICTURE '@Z 9999'
      @ ROW,106 SAY XTOTAL(3,8)  PICTURE '@Z 9999'
    ELSE
      @ ROW,074 SAY XTOTAL(3,1)  PICTURE '@Z 99999'
      @ ROW,080 SAY XTOTAL(3,2)  PICTURE '@Z 99999'
      @ ROW,086 SAY XTOTAL(3,3)  PICTURE '@Z 99999'
      @ ROW,092 SAY XTOTAL(3,4)  PICTURE '@Z 99999'
    ENDIF
  ENDIF
  *---C101826,1 AME[end] 

  *C101826,1 add 4 additional sizes in case of sorting by style or fabric AME[Start]
  *@ ROW,98 SAY XTOTAL(3,9)  PICTURE '999999'
  *@ ROW,118 SAY XTOTAL(3,10)  PICTURE '$9999999.99'
  IF lcSortCode $ "SF"
    @ ROW,111 SAY XTOTAL(3,9)  PICTURE '999999'
    @ ROW,119 SAY XTOTAL(3,10)  PICTURE '$9999999.99'
  ELSE
    @ ROW,98 SAY XTOTAL(3,9)  PICTURE '999999'
    @ ROW,117 SAY XTOTAL(3,10)  PICTURE '$9999999.99'
  ENDIF  
  *C101826,1 AME [End]
  
  
  

  ROW = ROW+1
  @ ROW,000 SAY REPLICATE('=',132)
  DO ENDREPORT
  EXIT
ENDDO
      SET DEVICE TO SCREE
RETURN
*---------------------------
*   END SOEIL140.PRG
*---------------------------
*!*************************************************************
*! Name      : lfEvalSegs    (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfNMajType   (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Mask NonMajor segments .
*!*************************************************************
*! Called from : lfEvalSegs.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajType()
*!*************************************************************
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 

*!*************************************************************
*! Name      : lfwRepWhen    (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
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

R_WIDTH  = 'W'          && STANDARD REPORT 'WIDE'

*!*************************************************************
*! Name      : lfsrvSty   (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.


*!*************************************************************
*! Name      : lfStySum   (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfvFabric  (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
*!*************************************************************
*! Name      : lfvAccount  (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
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
FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user wants to Browse or the Account is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvSelcBy  (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Validate select by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : 
*!           : 
*!           : 
*!*************************************************************
*! Example   : =lfvSelcBy()
*!*************************************************************
FUNCTION lfvSelcBy

STORE .T. TO llChSelect,llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
CLEAR READ


*!*************************************************************
*! Name      : lfvSortBy  (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Validate Sort by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : 
*!           : 
*!           : 
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
*--- llRpSupr
llRpSupr = .F.
DO CASE
  CASE lcRPSort $ 'OSFCR'
    llRpSupr = .T.
  OTHERWISE
    llRpSupr = .F.
ENDCASE
CLEAR READ

*!*************************************************************
*! Name      : lfvOrder  (C#101404)
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcVar , lcObj , laTemp,lcAlias

lcAlias = ALIAS()
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*--IF Statment to check if we are going to Browse
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK ('O'+lcObj))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "ORDER     :R :H= 'ORDER#' , "    +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "Terms=gfCodDes(CTERMCODE , 'CTERMCODE') :R :H= 'Terms' ,"  +;
               "ShipV=gfCodDes(ShipVia , 'SHIPVIA')   :R :H= 'ShipVia' ,"  +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "
  lcFile_Ttl = "Orders..."
  lcBrowCond = [FOR STATUS != "X" AND OPEN >= 1 ]
  = gfBrows(lcBrowCond,'ORDER','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = ''
  ENDIF
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
SELECT (lcAlias)


*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

lcNotes     = lcRpLinNot
lcSele      = lcRpSelcBy
lcNote      = (lcRpNotPd='Y')
lcSortCode  = lcRpSort
llSummarize = (lcRpSumm = 'Y')
XPRTWPSTK   = lcRpPrnStk
xTitle      = lcRpOpTitl
XCOORGRP    = lcRpCorGrp
FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
  
*--- Add Fabric# to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcFabric = ALLTRIM(laOgFxFlt[lnInd,6])
      
*--- Add Style Temp File to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.STYLE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      IF !USED(laOgFxFlt[lnInd,6]))
        *B802842,1 AMM Arrange lines 
        *RETURN .F.
        *llStyRange = .F.
        llStyRange = .F.
        RETURN .F.
        *B802842,1 AMM end
      ENDIF
      lcStyAls = laOgFxFlt[lnInd,6]
      
*--- Add Order# to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ORDER' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LOR = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,6))
      HOR = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],8,20))
      
*--- Add Season list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      TS = laOgFxFlt[lnInd,6]
      
*--- Add Order Status list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.STATUS' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ZST = laOgFxFlt[lnInd,6]

*--- Add Division list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      TD = laOgFxFlt[lnInd,6]
      
*--- Add Prioroty list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.PRIORITY' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lnAllPer = laOgFxFlt[lnInd,6]
      
*--- Add Color list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      TCLR = laOgFxFlt[lnInd,6]

*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      XGR = laOgFxFlt[lnInd,6]

*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *B802842,1 AMM Consider the Set of century in calculation
      *LCO = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10))
      *HCO = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20))
      LCO =  CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1, ATC('|',laOgFxFlt[lnInd,6])-1 )) )
      HCO =  CTOD( ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],ATC('|',laOgFxFlt[lnInd,6])+1,10)) )
      *B802842,1 AMM end
*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.START' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *B802842,1 AMM Consider the set of century in calculation
      *LST = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10))
      *HS  = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20))
      LST = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,ATC('|',laOgFxFlt[lnInd,6])-1)) )
      HS  = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],ATC('|',laOgFxFlt[lnInd,6])+1,10) ))
      *B802842,1 AMM end
*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      IF !USED(laOgFxFlt[lnInd,6]))
        llAccRange = .F.
        RETURN .F.
      ENDIF
      lcAccAls = laOgFxFlt[lnInd,6]
  ENDCASE
ENDFOR

*B607310,1 KHM 07/08/2003 (Begin) Get the sales orders with cOrdType = 'O' Only.
*lcFilter = 'TOTQTY>0'
lcFilter = 'cOrdType = "O" AND TOTQTY > 0'
*B607310,1 KHM 07/08/2003 (End)

IF !EMPTY(LOR)
  lcFilter = lcFilter + ".AND.BETWEEN(ORDER,LOR,HOR)"
ENDIF
IF !EMPTY(ZST)
  lcFilter = lcFilter + '.AND.OrdHdr.STATUS $ ZST'
ENDIF
IF !EMPTY(XGR)
  lcFilter = lcFilter + '.AND.STYLE.GROUP = XGR'
ENDIF
IF !EMPTY( TS )
  lcFilter = lcFilter + ".AND. ORDLINE.SEASON $ TS "
ENDIF
IF !EMPTY( TD )
  lcFilter = lcFilter + ".AND. OrdHdr.cDIVISION $ TD "
ENDIF
*B802842,1 AMM Adjust
*IF LCO <> NULDATE
IF !EMPTY(LCO)
*B802842,1 AMM end
  lcFilter = lcFilter + '.AND.BETWEEN(OrdHdr.COMPLETE,LCO,HCO)'
ENDIF
*B802842,1 AMM Adjust
*IF LST <> NULDATE
IF !EMPTY(LST)
*B802842,1 AMM end
  lcFilter = lcFilter + '.AND.BETWEEN(OrdHdr.START,LST,HS)'
ENDIF
IF !EMPTY(TCLR)
  lcFilter = lcFilter + '.AND.SUBSTR(Style,14,6) $ TCLR'
ENDIF
IF XCOORGRP = 'Y'
  lcFilter  = lcFilter + ".AND. !EMPTY(GROUP)"
ENDIF
IF !EMPTY(lnAllPer)
  *B802842,1 AMM Priority is 3 chrs.
  *lcFilter = lcFilter + '.AND. OrdHdr.PRIORITY $ lnAllPer'
  lcFilter = lcFilter + '.AND. ALLTRIM(OrdHdr.PRIORITY) $ lnAllPer'
  *B802842,1 AMM end
ENDIF