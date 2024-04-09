*:***************************************************************************
*: Program file  : SRSPSAL
*: Program desc. : SHIPMENTS BY SALESREP
*! Date          : 04/29/1999
*: System        : Aria Advantage Series.
*: Module        : SALES REPRESENTATIVES (SR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : Enh. # 301207
*:***************************************************************************
*: Example : DO SRSPSAL
*:***************************************************************************
*:Modifications :
*:B802701,1 SSH 25/10/99 Fix the bug of incorrect commission amount .
*:B802889,1 ABD 24/02/2000 Fix the bug that If you select to print this report 
*:B802889,1                For multiple salesrep, the first invoice for each 
*:B802889,1                Salesrep does not print.
*:B603314,1 ADEL 03/08/2000 Fix the bug of enaling 'Advanced' button in OG while
*:B603314,1                 no variable filter found for this reports. This fix happens 
*:B603314,1                 in SYDREPRT file.
*:B603713,8  MHM 10/16/2000 increase the length of the fields (Gros_Price ,Price, 
*:B603713,8                 PriceA , PriceB , PriceC , Amount) With 2 digit we add 
*:B603713,8                 record in SYREPUVR to get print dicemal yes or no 
*:B606110,1 TMI 06/16/2002  Fix the bug that When choose a specific account with date range 
*:B606110,1                 then no records disply 
*:B606969,1 WAB 03/05/2003 Fix the probem of the report is very slow 
*:B124211,1 NNA 09/22/2004 Fix bug that when you try to filter on styles, you'll get an empty browse
*:B124211,1 NNA            (fixed in the Syrepuvr File - ~lcStyTitle Record - Remove ['' FOR Make] from
*:B124211,1 NNA            Mbrwfltexp Field)
*:E132227,1 TMI 06/04/2006 Adding color filter to the report
*T20061103.0011 TMI ( B132744 ) Fix a bug that when printing and there is no data to show the system crashs when reopen any option grid
*B607892,1 TMI 12/19/2006 When selected colors are more than 40 the report crashs. T20061116.0012
*B607946,1 TMI 01/23/2007 When selecting a color no data is returned ( same ticket # T20061116.0012 )
*B608004,1 TMI 03/14/2007 reset device to screen in case no data is found ( ticket # T20070304.0020  )
*:***************************************************************************
*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint

*B606110,1 TMI [Start] Let the type for customers by 'M' as choosed from the IN List of the Customer file
IF 'CUSTOMER.TYPE+CUSTOMER.ACCOUNT+CUSTOMER.STORE'$lcRpExp
  lcRpExp = STRTRAN(lcRpExp,'CUSTOMER.TYPE+CUSTOMER.ACCOUNT+CUSTOMER.STORE','"M"+CUSTOMER.ACCOUNT+CUSTOMER.STORE')
ENDIF
*B606110,1 TMI [End  ] Let the type for customers by 'M' as choosed from the IN List of the Customer file

llprint     = .F.

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name
*-- XTITLE   variable that hold the report salesrep name

R_WIDTH     = 'W'
R_TITLE     = 'SHIPMENTS BY SALESREP'
XREPORT     = 'SRSPSAL'
XTITLE      = ''

*-- llStyCom   variable that hold the salesrep commention
llStyCom  = gfGetMemVar('M_STY_COM') = 'Y'

*-- XFILTER  variable of the option grid
XSL         = lcRpSales
XNEWPAGE    = lcRpStart
XREP        = lcRpPrint

*-- lcINVLTEMP  Variables that hold the temporary files name
lcINVLTEMP  = gfTempName()

*-- THE MAIN PROGRAM 

*B606969,1 WAB (Start) - intiales variable will be used to collect the filters
*--- lcEnt	   	---> Hold the current Centery setting
*--- llCustomer ---> Hold .T. if the user select specific Customer(s)
*--- lcMainExp  ---> Hold the Filter of the INVHDR 
*--- lcCustFile ---> Hold the temp cusror that hold the selectd customer(s)
*--- lcStylFile ---> Hold the temp cusror that hold the selectd Style(s)
*--- lcWorkFile ---> Hold the Temp. Cursor that hold the selected style/customer or hold 'INVHDR'
*------------------> if the user didn't selelect any customer or Style
*--- lcDivFltr  ---> hold the Division filter
*--- lcSeasFltr ---> Hold the season Filter 
*--- lcDatFltr  ---> hold the invdate filter

lcCent = SET('CENT') 
SET CENT ON
llCustomer = .F.
lcMainExp  = ''
lcWorkFile = ''
lcDivFltr  = ''
lcSeasFltr = ''
lcDatFltr  = ''
lcCustFile = ''
lcStylFile = ''
*E132227,1  TMI [Start] define varialbes
lcClrFltr = ''                        && Define a color filter
lnMajorLen = LEN(gfItemMask("PM"))    && Define major len variable
*E132227,1  TMI [End  ] 

*--Call Function to collect all the selected filters
=lfGetFltrs()

*B606969,1 WAB (End)

DO lpCollData

*B606969,1 WAB (Start) -- return back the Centery cetting
SET CENT &lcCent
*B606969,1 WAB (End)

*-- PRINT THE REPORT FROM THE WORKFILE
IF llPrint
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

IF USED(lcINVLTEMP)
  USE
ENDIF
  
*-- ERASE THE lcWorkFile
ERASE (gcWorkDir+lcINVLTEMP+'.DBF')
ERASE (gcWorkDir+lcINVLTEMP+'.CDX')
ERASE (gcWorkDir+lcINVLTEMP+'.FPT')

*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT 
*! Date      : 04/29/1999
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollData
*!*************************************************************
PROCEDURE lpCollData
  
SELE INVLINE
*B606969,1 WAB (Start) -- no need to the relation 
*SET RELATION TO INVOICE INTO INVHDR
*SET RELATION TO STYLE   INTO STYLE ADDITIVE
*SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE ) ;
                        INTO CUSTOMER ADDITIVE
*B606969,1 WAB (End)

IF !EMPTY(XSL)
  lcRpExp = lcRpExp  + " .AND. (INVHDR.REP1 = '" + XSL + "' .OR. INVHDR.REP2 = '" + XSL + "')"
ENDIF                     
lcRpExp = lcRpExp + ".AND. INVHDR.STATUS <> 'V' "

*B606969,1 WAB (Start) optimize the locate function
*LOCATE ALL FOR &lcRpExp
lcSeekExp = lcMainExp + IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr 
SELECT INVHDR
LOCATE FOR Invoice = '' AND &lcSeekExp

*B606969,1 WAB (End)

IF EOF()
  *-TEXT (There are no records to display)
  =gfModalGen('TRM00052B00000','DIALOG')
  *B608004,1 TMI [Start] reset device to screen in case no data is found
  =lfClearRep()
  *B608004,1 TMI [End  ] 
  RETURN
ELSE
  SELECT INVLINE
  *-- CREATE THE lcINVLTEMP FILE
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'REPCODE'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 3
  laFileStru[lnFileStru,4] = 0

  Create Table (gcWorkDir+lcINVLTEMP) FROM ARRAY laFileStru
  INDEX ON REPCODE+INVOICE TAG &lcINVLTEMP
  llprint = .T.

  *B606969,1 WAB (Start) -- commet the collection lines and rewrite it again to be more faster
  *SELECT INVLINE    
  *SCAN FOR &lcRpExp
  *  SCATTER MEMVAR MEMO
  *  WAIT 'Selecting records for report , Invoice# '+m.Invoice       WINDOW NOWAIT
  *  IF EMPTY(XSL)
  *    IF EMPTY(INVHDR.Rep1)  .AND. EMPTY(INVHDR.Rep2)
  *      m.FLAG = ''
  *      m.REPCODE = SPACE(0)
  *      INSERT INTO (lcINVLTEMP) FROM MEMVAR 
  *    ELSE
  *      IF !EMPTY(INVHDR.Rep1)
  *        m.FLAG = '1'
  *        M.REPCODE = INVHDR.Rep1
  *        INSERT INTO (lcINVLTEMP) FROM MEMVAR 
  *      ENDIF
  *      IF !EMPTY(INVHDR.Rep2) 
  *        m.FLAG = '2'
  *        M.REPCODE = INVHDR.Rep2
  *        INSERT INTO (lcINVLTEMP) FROM MEMVAR 
  *      ENDIF
  *    ENDIF
  *  ELSE
  *    m.FLAG = IIF(INVHDR.REP1 = XSL ,'1','2')
  *    M.REPCODE = IIF(INVHDR.REP1 = XSL,INVHDR.Rep1,INVHDR.Rep2)
  *    INSERT INTO (lcINVLTEMP) FROM MEMVAR 
  *  ENDIF
  *ENDSCAN
  *SET RELATION TO
  
  *--- i will collect the data in 3 diffrente ways , depend on the user selections
  *--- 1- if the user select specific Customer(s) , i will scan the Temp cursor that hold all the 
  *---    Selected customer(s) , then seek in the invhdr to collect all the invoices for the specific customer
  *--- 2- if the user select specific Style(s) , i will scan the Temp cursor that hold all the 
  *---    Selected Style(s) ,then seek in the invLine to collect all the invoices for the specific Style
  *--- 3- if the user didn't select specific Style(s) or customer(s), i will scan the Invhdr then collect 
  *---    the data from the invline 
  
  SELECT INVHDR
  lcOrder = ORDER()
  IF llCustomer
    SET ORDER TO TAG INVHDRA
  ENDIF
  SELECT INVLINE
  SET ORDER TO TAG IIF(llcustomer OR lcWorkFile = 'INVHDR','INVLINE','INVLINES')
  SELECT (lcWorkFile)
  IF llCustomer
    *--if the user select specific Customer
    SCAN
      WAIT 'Selecting records for report , Account # '+Account       WINDOW NOWAIT
      IF SEEK(Account,'INVHDR') 
        SELECT INVHDR
        lcSeekExp = lcMainExp + IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr 
        SCAN REST WHILE account+invoice = &lcWorkFile..Account FOR &lcSeekExp
          WAIT 'Selecting records for report , Invoice # '+INVOICE       WINDOW NOWAIT
          IF SEEK(INVOICE,'INVLINE') 
            SELECT INVLINE
            lcLineExp = IIF(EMPTY(lcSeasFltr),".T.",lcSeasFltr) 
            *E132227,1  TMI [Start] Update lineExp with lcClrFltr variable
            lcLineExp = lcLineExp + IIF(EMPTY(lcClrFltr),"",".AND."+lcClrFltr) 
            *E132227,1  TMI [End  ]  
            SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE FOR &lcLineExp
              WAIT 'Selecting records for report , Style # '+Style       WINDOW NOWAIT
              *E132227,1  TMI [Start] check that lcStylFile has records
              *lcChkExp = IIF(EMPTY(lcStylFile),".T.","SEEK(STYLE.cStyMajor,lcStylFile)")
              lcChkExp = IIF(!EMPTY(lcStylFile) .AND. RECCOUNT(lcStylFile)>0 , "SEEK(STYLE.cStyMajor,lcStylFile)" ,".T." )
              *E132227,1  TMI [End  ] 
              IF SEEK(INVLINE.Style,'STYLE') AND &lcChkExp
                =lfGetData()
              ENDIF
            ENDSCAN 
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  ELSE
    IF lcWorkFile = 'INVHDR'
      *--if the user didn't select any Style(s) or  Customer(s) .

      lcSeekExp = lcMainExp + IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr 
      SCAN FOR Invoice = '' AND &lcSeekExp 
        WAIT 'Selecting records for report , Invoice # '+Invoice    WINDOW NOWAIT
        IF SEEK(INVOICE,'INVLINE') 
          SELECT INVLINE
          lcLineExp = IIF(EMPTY(lcSeasFltr),".T.",lcSeasFltr)
          *E132227,1  TMI [Start] Update lineExp with lcClrFltr variable
          lcLineExp = lcLineExp + IIF(EMPTY(lcClrFltr),"",".AND."+lcClrFltr) 
          *E132227,1  TMI [End  ]  
          SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE FOR &lcLineExp
            =lfGetData()
          ENDSCAN
        ENDIF
      ENDSCAN
    ELSE
      *--if the user select specific Style(s)
      SCAN
        *WAIT 'Selecting records for report , Style # '+cStyMajor       WINDOW NOWAIT
        *E132227,1  TMI [Start] pad the style with spaces up to major lenght
        *lcStyle = &lcWorkFile..cStyMajor
        *IF SEEK(ALLTRIM(lcStyle),'INVLINE') AND SEEK(ALLTRIM(lcStyle),'STYLE') 
        lcStyle = PADR(ALLTRIM(&lcWorkFile..cStyMajor),lnMajorLen)
        IF SEEK(lcStyle,'INVLINE') AND SEEK(lcStyle,'STYLE') 
          *E132227,1  TMI [End  ] 
          SELECT INVLINE
          lcSeekExp = lcSeasFltr + IIF(!EMPTY(lcDatFltr) AND !EMPTY(lcSeasFltr)," .AND. ","") + lcDatFltr 
          lcSeekExp = IIF(EMPTY(lcSeekExp),".T.",lcSeekExp)
          *E132227,1  TMI [Start] Update lineExp with lcClrFltr variable , and use lcStyle variable only with no alltrim
          *SCAN REST WHILE style+invoice+STR(lineno,6) = ALLTRIM(lcStyle) FOR &lcSeekExp
          lcSeekExp = lcSeekExp + IIF(EMPTY(lcClrFltr),"",".AND."+lcClrFltr) 
          SCAN REST WHILE style+invoice+STR(lineno,6) = lcStyle FOR &lcSeekExp
            *E132227,1  TMI [End  ]            
            WAIT 'Selecting records for report , Invoice # '+Invoice    WINDOW NOWAIT
            IF SEEK(INVLINE.INVOICE,'INVHDR') AND &lcMainExp 
              =lfGetData()
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  SELECT INVHDR
  SET ORDER TO &lcOrder
  SELECT INVLINE    
  SET ORDER TO INVLINES
  SELE (lcINVLTEMP)
  IF EOF()
    *-TEXT (There are no records to display)
    =gfModalGen('TRM00052B00000','DIALOG')
    llprint = .F.
    RETURN
  ENDIF
 *B606969,1 WAB (End) 

  SET RELATION TO INVOICE INTO INVHDR
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE ) ;
                      INTO CUSTOMER ADDITIVE
ENDIF  
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT 
*! Date      : 04/29/1999
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint


ROW = 99
PAGENO  = 0
XSAVFLD = ''
*B802889,1 ABD add the initialization of Next Variables & Made Them Global. [ Begin]
STORE '' TO XREPNAME
*B802889,1 ABD [ End ]

XSUBPCS = 0
XSUBEXT = 0
STORE 0 TO lnSubComm1,lnSubComm2

SELE &lcINVLTEMP

*B606969,1 WAB (Start) 
*GO TOP
*SCAN
LOCATE
SCAN FOR REPCODE+INVOICE = ''
*B606969,1 WAB (End) 

  *B802889,1 ABD Remark the next lines and made it in a local PROCEDURE lpPrtRpHdr.
  *B802889,1 ABD To print the header at the beginning of the report. [ Begin ]
  *IF ROW > 55
  *  IF EMPTY(XSAVFLD)
  *    SELE &lcINVLTEMP
  *    XSAVFLD = REPCODE
  *    SELE SALESREP
  *    SEEK XSAVFLD
  *    XREPNAME = IIF( FOUND() , NAME , '' )
  *    XTITLE   = IIF(XNEWPAGE = 'Y' , XREPNAME , '')
  *    SELE &lcINVLTEMP
  *  ENDIF
  *
  * PAGENO = PAGENO + 1
  *  DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
  *  ROW = 5
  *  IF XREP='D'
  *    *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....2....+....3..
  *       @ ROW,0 SAY 'INVOIC INVDATE  ACCT# NAME... STYLE        COLOR   PIECE   PRICE       EXT ORDER  CUST P/O#  '+;
  *                IIF( llStyCom, 'REP1 COMM  COMM.AMT REP2 COMM  COMM.AMT ', '' )
  *  ELSE
  *    @ ROW,01 SAY '.............SALESREP..............              PIECES               EXT     '+;
  *                  IIF( llStyCom, '                        COMM.AMT1           COMM.AMT2','')
  *  ENDIF
  *  ROW = ROW + 1
  *  IF XREP='D'
  *    @ ROW,00 SAY REPLICATE('=',132)
  *    ROW = ROW + 1
  *  ENDIF
  *ENDIF
  DO lpPrtRpHdr
  *B802889,1 ABD [ End ]

  SELE &lcINVLTEMP
  IF REPCODE # XSAVFLD
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    IF !EMPTY(XREPNAME)
      @ ROW,01 SAY '**'+XSAVFLD+'  '+XREPNAME+'**'
    ELSE
      @ ROW,01 SAY '**'+" Shipments with no salesrep"+SPACE(2)+'**'
    ENDIF
    @ ROW,49 SAY XSUBPCS PICTURE '9999999'
    *:B603713,8  MHM 10/16/2000  [start]
    *@ ROW,63 SAY XSUBEXT PICTURE '99999999.99'
    IF lcRpDeciml = "Y"
      @ ROW,63 SAY XSUBEXT PICTURE '99999999.99'
    ELSE
      @ ROW,63 SAY XSUBEXT PICTURE '99999999.99'
    ENDIF
    *:B603713,8  MHM 10/16/2000  [end]
    IF llStyCom    
      *:B603713,8  MHM 10/16/2000  [start]
      *@ ROW,102 SAY lnSubComm1 PICTURE '9999999.99'
      *@ ROW,122 SAY lnSubComm2 PICTURE '9999999.99'
      IF lcRpDeciml = "Y"
        @ ROW,102 SAY lnSubComm1 PICTURE '9999999.99'
        @ ROW,122 SAY lnSubComm2 PICTURE '9999999.99'
      ELSE
        @ ROW,102 SAY lnSubComm1 PICTURE '9999999999'
        @ ROW,122 SAY lnSubComm2 PICTURE '9999999999'
      ENDIF 
      *:B603713,8  MHM 10/16/2000  [end]
    ENDIF
    IF XREP='D'
      ROW  = ROW + 1
      @ ROW,00 SAY REPLICATE('-',132)
    ENDIF

    XSUBPCS  = 0
    XSUBEXT  = 0
    STORE 0 TO lnSubComm1,lnSubComm2
    XSAVFLD  = REPCODE
    SELE SALESREP
    SEEK XSAVFLD
    XREPNAME = IIF( FOUND() , NAME , '' )
    XTITLE   = IIF( XNEWPAGE = 'Y' , XREPNAME , '' )
    SELE &lcINVLTEMP
    IF XNEWPAGE = 'Y'
      ROW       = 99
      PAGENO    = 0
      *B802889,1 ABD Remark the loop and call the procedure lpPrtRpHdr to Print The
      *B802889,1 ABD Header becouse it move the recored pointer one recored . [ Begin]
      *LOOP
      Do lpPrtRpHdr
      *B802889,1 ABD [ End ]
      
    ELSE
      ROW = ROW + 1
    ENDIF
  ENDIF
  SELE &lcINVLTEMP
  IF llStyCom
    lnStyAmt = Price * TotQty
    *:B802701,1 SSH 25/10/99 Fix the bug of incorrect commission amount .
    *lnStyNet = lnStyAmt-( InvHdr.DiscPcnt * lnStyAmt / 100 ) ;
               * ( 1 - InvHdr.Trde_Disc / 100 )
    lnStyNet = (lnStyAmt-( InvHdr.DiscPcnt * lnStyAmt / 100 )) ;
               * ( 1-InvHdr.Trde_Disc / 100 )
    *:B802701,1 SSH(END)
  ENDIF

  IF XREP='D'
    @ ROW,0  SAY INVOICE
    @ ROW,07 SAY INVDATE
    @ ROW,16 SAY ACCOUNT
    @ ROW,22 SAY LEFT(CUSTOMER.STNAME,7)
    @ ROW,30 SAY STYLE
    @ ROW,50 SAY TOTQTY  PICTURE '999999'
    *:B603713,8  MHM 10/16/2000  [start]
    *@ ROW,57 SAY PRICE   PICTURE '9999.99'
    *@ ROW,64 SAY TOTQTY*PRICE PICTURE '9999999.99'
    IF lcRpDeciml = "Y"
      @ ROW,57 SAY PRICE   PICTURE '9999.99'
      @ ROW,64 SAY TOTQTY*PRICE PICTURE '9999999.99'
    ELSE
      @ ROW,57 SAY PRICE   PICTURE '9999999'
      @ ROW,64 SAY TOTQTY*PRICE PICTURE '9999999999'
    ENDIF  
    *:B603713,8  MHM 10/16/2000  [End]
    @ ROW,75 SAY ORDER
    @ ROW,82 SAY LEFT(INVHDR.CUSTPO,10)
    IF llStyCom
      *:B603713,8  MHM 10/16/2000  [start]
      *@ Row,93 SAY InvHdr.Rep1            PICTURE '!!!'
      *@ Row,97 SAY Comm1                  PICTURE '99.99'
      *@ Row,103 SAY lnStyNet * Comm1 / 100 PICTURE '999999.99'
      *@ Row,113 SAY InvHdr.Rep2            PICTURE '!!!'
      *@ Row,117 SAY Comm2                  PICTURE '99.99'
      *@ Row,123 SAY lnStyNet * Comm2 / 100 PICTURE '999999.99'
      IF lcRpDeciml = "Y"
        @ Row,93 SAY InvHdr.Rep1            PICTURE '!!!'
        @ Row,97 SAY Comm1                  PICTURE '99.99'
        @ Row,103 SAY lnStyNet * Comm1 / 100 PICTURE '999999.99'
        @ Row,113 SAY InvHdr.Rep2            PICTURE '!!!'
        @ Row,117 SAY Comm2                  PICTURE '99.99'
        @ Row,123 SAY lnStyNet * Comm2 / 100 PICTURE '999999.99'
      ELSE
        @ Row,93 SAY InvHdr.Rep1            PICTURE '!!!'
        @ Row,97 SAY Comm1                  PICTURE '99999'
        @ Row,103 SAY lnStyNet * Comm1 / 100 PICTURE '999999999'
        @ Row,113 SAY InvHdr.Rep2            PICTURE '!!!'
        @ Row,117 SAY Comm2                  PICTURE '99999'
        @ Row,123 SAY lnStyNet * Comm2 / 100 PICTURE '999999999'
      ENDIF  
    *:B603713,8  MHM 10/16/2000  [End]
    ENDIF
    ROW = ROW + 1
  ENDIF
  IF llStyCom
    lnSubComm1 = lnSubComm1 + (lnStyNet * Comm1 / 100)
    lnSubComm2 = lnSubComm2 + (lnStyNet * Comm2 / 100)
  ENDIF
  XSUBPCS = XSUBPCS + TOTQTY
  XSUBEXT = XSUBEXT + (TOTQTY * PRICE )
ENDSCAN

IF REPCODE # XSAVFLD
  @ ROW,00 SAY REPLICATE('-',132)
  ROW = ROW + 1
  @ ROW,01 SAY '**'+XSAVFLD+'  '+XREPNAME+'**'
  @ ROW,49 SAY XSUBPCS PICTURE '9999999'
  *:B603713,8  MHM 10/16/2000  [start]
  *@ ROW,63 SAY XSUBEXT PICTURE '99999999.99'
  IF lcRpDeciml = "Y"
    @ ROW,63 SAY XSUBEXT PICTURE '99999999.99'
  ELSE
    @ ROW,63 SAY XSUBEXT PICTURE '99999999999'
  ENDIF  
  *:B603713,8  MHM 10/16/2000  [end]
  IF llStyCom    
    *:B603713,8  MHM 10/16/2000  [start]
    *@ ROW,102 SAY lnSubComm1 PICTURE '9999999.99'
    *@ ROW,122 SAY lnSubComm2 PICTURE '9999999.99'
    IF lcRpDeciml = "Y"
      @ ROW,102 SAY lnSubComm1 PICTURE '9999999.99'
      @ ROW,122 SAY lnSubComm2 PICTURE '9999999.99'
    ELSE
      @ ROW,102 SAY lnSubComm1 PICTURE '9999999999'
      @ ROW,122 SAY lnSubComm2 PICTURE '9999999999'
    ENDIF
    *:B603713,8  MHM 10/16/2000  [end]
  ENDIF
  IF XREP='D'
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvSalesr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/1999
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSalesr()
*!*************************************************************
FUNCTION lfvSalesr
PRIVATE lcVar , lcObj , laTemp

lcSaleF   = VARREAD()
lcSales   = EVALUATE(lcSaleF) 
lnSelFile =  SELECT(0)
SELECT SALESREP
lcCusTag  = ORDER('SALESREP')
SET ORDER TO TAG SALESREP IN SALESREP
IF !EMPTY(lcSales) .AND. ('?' $ lcSales OR !SEEK(lcSales , 'SALESREP'))
  DIMENSION laTemp[1]
  laTemp = ''    
  lcBrFields  = "REPCODE   :R :H= 'Code' , "   +;
                "NAME      :R :H= 'Name' ,"    +;
                "cAddress6 :R :H= 'Country' ," +;
                "PHONE     :R :H= 'Phone' ,"   +;
                "BALANCE   :R :H= 'Balance' "
  lcFile_Ttl  = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
  IF !EMPTY(laTemp[1])
    lcSales   = laTemp[1]
  ELSE   
    lcSales   = ""
  ENDIF    
ENDIF    
&lcSaleF = lcSales 
SET ORDER TO lcCusTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvAcc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/1999
*! Purpose   : Validate Account
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc
PRIVATE lcCusFld,lcCustom,lnSelFile,lcCusTag

lcCusFld  = VARREAD()
lcCustom  = EVAL(lcCusFld)
lnSelFile =  SELECT(0)
SELECT CUSTOMER
lcCusTag  = ORDER('CUSTOMER')
SET ORDER TO TAG CUSTOMER IN CUSTOMER
IF !EMPTY(lcCustom) .AND. ( '?' $ lcCustom .OR. !SEEK((IIF( EMPTY(STORE) , 'M'+lcCustom , 'S'+lcCustom+STORE )) , 'CUSTOMER'))
  =CUSBROWM (@lcCustom)
ENDIF
&lcCusFld = lcCustom
SET ORDER TO lcCusTag
SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/1999
*! Purpose   : To set relation on or off when running the in 
*!           : range function in the option grid.
*!*************************************************************
*! Called from : SRSPSAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE  IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lnDatapos = ASCAN(laOGFxFlt,'INVHDR.INVDATE')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDataPos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF
ENDIF

*E132227,1  TMI [Start] Get lnColorLen and lnClrPo variables
PRIVATE laItemSeg,lnCount
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnColorLen = LEN(laItemSeg[lnCount,3])
    lnClrPo    = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*- Update the color title variable
=lfNonMaj()
*E132227,1  TMI [End  ] 

*!*************************************************************
*! Name        : lpPrtRpHdr
*! Developer   : ABDOU ELGENDI (ABD)
*! Date        : 02/24/2000
*! Purpose     : Print The Heder Of The Report (B802889)
*!*************************************************************
*! Called from : lpPrint
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpPrtRpHdr
*!*************************************************************
*B802889,1 ABD
PROCEDURE lpPrtRpHdr

IF ROW > 55
  IF EMPTY(XSAVFLD)
    SELE &lcINVLTEMP
    XSAVFLD = REPCODE
    SELE SALESREP
    SEEK XSAVFLD
    XREPNAME = IIF( FOUND() , NAME , '' )
    XTITLE   = IIF(XNEWPAGE = 'Y' , XREPNAME , '')
    SELE &lcINVLTEMP
  ENDIF

  PAGENO = PAGENO + 1
  DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
  ROW = 5
  IF XREP='D'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....2....+....3..
       @ ROW,0 SAY 'INVOIC INVDATE  ACCT# NAME... STYLE        COLOR   PIECE   PRICE       EXT ORDER  CUST P/O#  '+;
                IIF( llStyCom, 'REP1 COMM  COMM.AMT REP2 COMM  COMM.AMT ', '' )
  ELSE
    @ ROW,01 SAY '.............SALESREP..............              PIECES               EXT     '+;
                  IIF( llStyCom, '                        COMM.AMT1           COMM.AMT2','')
  ENDIF
  ROW = ROW + 1
  IF XREP='D'
    @ ROW,00 SAY REPLICATE('=',132)
    ROW = ROW + 1
  ENDIF
ENDIF
*-- End OF lpPrtRpHdr.  
*!*************************************************************k



*!*************************************************************
*! Name      : lfGetFltrs
*! Developer : WAB - Walid A. Wahab
*! Date      : 03/05/2003
*! Purpose   : collect all the selected filters 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetFltrs()
*!*************************************************************
*B606969,1 WAB 
*!*************************************************************
FUNCTION lfGetFltrs
lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
lnStyPos   = lfItmPos('STYLE.CSTYMAJOR')
lnDivPos   = lfItmPos('INVHDR.CDIVISION')
lnSeaSPos  = lfItmPos('INVHDR.SEASON')
lnDatePos  = lfItmPos('INVHDR.INVDATE')
*E132227,1  TMI [Start] Get Color filter position 
lnClrFlPos = lfItmPos("SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
*E132227,1  TMI [End  ] 

IF !EMPTY(XSL)
  lcMainExp = "(INVHDR.REP1 = '" + XSL + "' .OR. INVHDR.REP2 = '" + XSL + "')"
ENDIF                     
lcMainExp  = lcMainExp + IIF(!EMPTY(lcMainExp)," .AND. ","")+" INVHDR.STATUS <> 'V' "

llCustomer =  USED(laOgFxFlt[lnAccPos,6]) AND RECCOUNT(laOgFxFlt[lnAccPos,6]) > 0
lcCustFile = IIF(llCustomer,laOgFxFlt[lnAccPos,6],"")
lcStylFile = IIF(USED(laOgFxFlt[lnStyPos,6]),laOgFxFlt[lnStyPos,6],"")

*E132227,1  TMI [Start] check if no style is selected 
*lcWorkFile = IIF(llCustomer,lcCustFile,IIF(!EMPTY(lcStylFile),lcStylFile,'INVHDR'))
lcWorkFile = IIF(llCustomer,lcCustFile,IIF(!EMPTY(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,lcStylFile,'INVHDR'))
*E132227,1  TMI [End  ] 
 
lcDivFltr  = IIF(!EMPTY(laOgFxFlt[lnDivPos,6]),lfMakeExpr(lnDivPos),"")

lcMainExp = lcMainExp + IIF(!EMPTY(lcDivFltr)," .AND. ","")+lcDivFltr

lcSeasFltr = IIF(!EMPTY(laOgFxFlt[lnSeaSPos,6]),lfMakeExpr(lnSeaSPos),"")
lcSeasFltr = STRTRAN(lcSeasFltr,'INVHDR.','')

lcDatFltr  = IIF(!EMPTY(laOgFxFlt[lnDatePos,6]),lfMakeExpr(lnDatePos),"")
lcDatFltr  = STRTRAN(lcDatFltr,'INVHDR.','')

*B607892,1 TMI [Start] make the lcCrlFltr as the rest of other filters by calling the lfMakeExpr function and correct the fucntion when building the expression
**E132227,1  TMI [Start] Get the color filter expression
*lcClrFltr  = IIF(!EMPTY(laOgFxFlt[lnClrFlPos,6]),"SUBSTR(Style,lnClrPo,lnColorLen)" + "$ '" + laOgFxFlt[lnClrFlPos,6] + "'","")
**E132227,1  TMI [End  ] 
lcClrFltr  = IIF(!EMPTY(laOgFxFlt[lnClrFlPos,6]),lfMakeExpr(lnClrFlPos),"")
lcClrFltr  = STRTRAN(lcClrFltr,'INVHDR.','')
*B607892,1 TMI [End  ] 

*B607946,1 TMI [Start] remove the 'STYLE.' string from the lcClrFltr string
lcClrFltr  = STRTRAN(lcClrFltr ,'STYLE.','')
*B607946,1 TMI [End  ] 

*!*************************************************************
*! Name      : lfItmPos
*! Developer : WAB - Walid A. Wahab
*! Date      : 03/05/2003
*! Purpose   : get the item position in laOgFxFlter
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfItmPos()
*!*************************************************************
*B606969,1 WAB 
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos


*!*************************************************************
*! Name      : lfMakeExpr
*! Developer : WAB - Walid A. Wahab
*! Date      : 03/05/2003
*! Purpose   : creat an expersion for specific filter
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMakeExpr()
*!*************************************************************
*B606969,1 WAB 
*!*************************************************************
FUNCTION lfMakeExpr
PARAMETERS lnFxFltItm
PRIVATE lcItmExpr

lcItmExpr = ''
*-- if operator is like
IF 'LIKE' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
  lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' = "' +  laOGFxFlt[lnFxFltItm,6] + '"'

ELSE  && operator is either Between or InList
  *-- if operator is between.
  IF 'BETWEEN' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
    *-- if Type is Date
    PRIVATE lcFrom,lcTo
    lcFrom = SUBSTR(laOGFxFlt[lnFxFltItm,6],1,ATC('|',laOGFxFlt[lnFxFltItm,6]) - 1)
    lcTo   = SUBSTR(laOGFxFlt[lnFxFltItm,6],ATC('|',laOGFxFlt[lnFxFltItm,6]) + 1)

    IF laOGFxFlt[lnFxFltItm,3] = 'D'
      
      lcFrom = CTOD(lcFrom)
      lcTo   = CTOD(lcTo)

      IF !EMPTY(lcFrom)
        lcFrom = DTOS(lcFrom)
        lcTo   = DTOS(lcTo)
        lcItmExpr = 'BETWEEN( DTOS(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ') , "' + ;
                                 lcFrom + '" , "' + lcTo + '")'
      ENDIF  
    ELSE  && Type is not date (this case not occured in this report)
    ENDIF
  ELSE  && else operator is in list

    *-- if in range case.
    IF laOGFxFlt[lnFxFltItm,7] = 'R'
      lcItmExpr = 'SEEK(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ', "' +;
                            ALLTRIM(laOGFxFlt[lnFxFltItm,6]) + '")'
    ELSE  && default in list case
      *B607892,1 TMI [Start] include the variable name in the expression not the value it includes
      *lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' $ "' + laOGFxFlt[lnFxFltItm,6] + '"'
      lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' $ laOGFxFlt[' + ALLT(STR(lnFxFltItm)) + ',6]'
      *B607892,1 TMI [End  ] 
    ENDIF                           
    
  ENDIF

ENDIF
RETURN IIF(EMPTY(lcItmExpr),lcItmExpr,'(' + lcItmExpr + ')')
*-- end of lfMakeExpr.

*!*************************************************************
*! Name      : lfGetData
*! Developer : WAB - Walid A. Wahab
*! Date      : 03/05/2003
*! Purpose   : creat a record to the temp file 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetData()
*!*************************************************************
*B606969,1 WAB 
*!*************************************************************
FUNCTION lfGetData
SCATTER MEMVAR MEMO
WAIT 'Selecting records for report , Invoice# '+m.Invoice       WINDOW NOWAIT
IF EMPTY(XSL)
  IF EMPTY(INVHDR.Rep1)  .AND. EMPTY(INVHDR.Rep2)
    m.FLAG = ''
    m.REPCODE = SPACE(0)
    INSERT INTO (lcINVLTEMP) FROM MEMVAR 
  ELSE
    IF !EMPTY(INVHDR.Rep1)
      m.FLAG = '1'
      M.REPCODE = INVHDR.Rep1
      INSERT INTO (lcINVLTEMP) FROM MEMVAR 
    ENDIF
    IF !EMPTY(INVHDR.Rep2) 
      m.FLAG = '2'
      M.REPCODE = INVHDR.Rep2
      INSERT INTO (lcINVLTEMP) FROM MEMVAR 
    ENDIF
  ENDIF
ELSE
  m.FLAG = IIF(INVHDR.REP1 = XSL ,'1','2')
  M.REPCODE = IIF(INVHDR.REP1 = XSL,INVHDR.Rep1,INVHDR.Rep2)
  INSERT INTO (lcINVLTEMP) FROM MEMVAR 
ENDIF



*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed Mohamed Mohamed  (AMM)
*! Date      : 08/27/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj
PRIVATE lnClrPo
llCodeF = .F.
lnClrPo = 0
lnNonMajPo = 0
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.

* Array to collect data about all segments in the style code structure
DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' .AND. !llCodeF
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
    lnFreeLen = LEN(lcNonMajPi)
  ENDIF

  *-- If you Find Color Type or Find previous Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      
      *-- Color position
      lnClrPo    = laMajSeg[lnI,4]
      
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      *-- Picture
      lcNonMajPi = laMajSeg[lnI,3]
      *-- NonMajor title
      lcColorTt = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnColorLen = LEN(lcNonMajPi)
      EXIT
    ELSE
      llCodeF = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

*-- Compute Free/Color Items in Style Structure. [End]

* get the style major segement title
lcMajTtl =gfItemMask("HM")

lcMajPic = "@! " + gfItemMask("PM")

RETURN ''


*:**************************************************************************
*:* Name        : lfClearRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/16/2006
*:* Purpose     : When closing the file be sure to set the device to screen
*                 and also be sure that the out file global variable "gcOutFile" 
*                 is empty
*:***************************************************************************
*:* Called from : automatically called from gfOpGrid.prg when closing a report
*:***************************************************************************
*T20061103.0011 TMI [Start] 
FUNCTION lfClearRep

*- set device to screen
SET DEVICE TO SCREEN

*- release temp out file
IF !EMPTY(gcOutFile) .AND. FILE(gcOutFile)
  ERASE (gcOutFile)
ENDIF
gcOutFile = ''
*-- end of lfClearRep.