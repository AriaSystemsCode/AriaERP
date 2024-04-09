*:************************************************************************
*: Program file  : ARPINVEQ.PRG
*:         Entry : C102220,1
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Ahmed Mohamed ElAnwar (AME)
*:      Copyright (c) 
*:  Procs & Fncts: lpGtScl(),lfGetcont(),lfGetInHdr()
*:  Documented    03/12/2001
*:  Modifications:
*:************************************************************************

XNOTE_LOOP = .F.    && indicate if note lines exceeds maxrow Number.



lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95



NEWDOC = .T.
MAXROW = 47
lcDist = ' '
lcZone = ''
STORE .F. TO llNoRec
lnNotLine = 1

CLEAR TYPEAHEAD

DECLARE laZone[1,2]
laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

*---- If No rec match criteria
*---- outpot messg. then return

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  llNoRec = .T.
  *-- Displaying a message to indicate that no
  *-- records were found to display.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SET DEVICE TO PRINT
*-- Get the STYLE and COLOR length
DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos 
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR


SELECT INVHDR
SCAN FOR &lcRpExp     && main Scan loop
  XINVOICE = INVOICE
  WAIT WINDOW 'PRINTING INVOICE -   '+ XINVOICE  NOWAIT
  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL
    llErr     = .F.
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = IIF(EMPTY(PHONE),'',PHONE)    
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION
    *** GET THE BILL TO AND SHIP TO ADDRESS
    
    
    =lfSolSpAdr() && from Calling program.

    SELECT CUSTOMER
    IF !EMPTY(DIST_CTR)
      lcDist = DIST_CTR
      = SEEK('S'+XACCOUNT+lcDist)
    ENDIF

    ***
    ****** FIND THE INVOICE LINES
    ***
    IF !SEEK(XINVOICE,'INVLINE')
      llErr = .T.
    ENDIF
    *** GET THE DESCRIPTION ABOUT THE CODES
    
    *C102220,4 AME [START] JUST TAKE 15 CHR OF TERMS CODE.
    *PTERMS = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PTERMS = LEFT(gfCodDes(INVHDR.CTERMCODE,'CTERMCODE'),15)
    *C102220,4 AME [END]
    PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST= gfCodDes(INVHDR.SPCINST,'SPCINST')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')


    XZN = lfGetZone(LEFT(lcZone,7),LEFT(SYCCOMP.CADDRESS5,3),LEFT(CUSTOMER.CADDRESS52,3))
    XZN = IIF(!EMPTY(XZN),'('+XZN+')',XZN)
    
    PSHIPVIA = SUBSTR(TRIM(PSHIPVIA),1,9)+  XZN
    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------
    *C102220,4 AME [START] GET THE RIGHT VALUE OF PRINTFACT.
    PRINTFCTR = llPrnFact
    *C102220,4 AME [END]
    
    STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
    
    *C102220,4 AME [START] CHECK OF LOCAL VARIABLE INSTEAD OF GLOBAL ONE. 
    *C102220,4 AME         IF NO FACTOR SET LOCAL VAR. TO FALSE.
    *IF llPrnFact
    IF PRINTFCTR
      IF EMPTY(INVHDR.CFACCODE)
        PRINTFCTR = .F.
      ELSE
    *C102220,4 AME [END]  
        IF !SEEK(INVHDR.CFACCODE,'SYCFACT')
          STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
        ELSE
          XFNAME  = SYCFACT.CFACCOMP
          XFADDR1 = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
          XFADDR2 = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
          XFADDR3 = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
          IF XFADDR2 = ' '
            XFADDR2 = XFADDR3
            XFADDR3 = ' '
          ENDIF
        ENDIF
      *C102220,4 AME [START] CLOSE THE IF CONDITION.  
      ENDIF  &&EMPTY(INVHDR.FACTOR)
      *C102220,4 AME [END]  
    ENDIF
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF && NEWDOC
  ****** END NEWDOC
  IF llErr 
    NEWDOC = .T.
    LOOP
  ENDIF
  SELECT INVHDR
  * LINE 2
  @ 02,05 SAY 'INVOICE: '+XINVOICE
  @ 03,05 SAY INVDATE
  @ 04,05 SAY IIF(!EMPTY(INVHDR.CUSTPO),'PO: '+INVHDR.CUSTPO,"")

  IF PRINTFCTR
    @ 04,50 SAY 'R E M I T   T O:'
    @ 05,50 SAY XFNAME
    @ 06,50 SAY XFADDR1
    @ 07,50 SAY XFADDR2
    @ 08,50 SAY XFADDR3
  ENDIF

  
  * LINE 10
  @ 10,10 SAY lcSolTName
  @ 10,47 SAY lcShpTName
 
  * LINE 11
  @ 11,10 SAY laSoldTo[1]
  @ 11,47 SAY laShipTo[1]


  * LINE 12
  @ 12,10 SAY laSoldTo[2]
  @ 12,47 SAY laShipTo[2]
 
  * LINE 13
  @ 13,10 SAY laSoldTo[3]
  @ 13,47 SAY laShipTo[3]

  @ 17,01 SAY ACCOUNT
  *C102220,4 AME [START]  TAKE JUST 10 cHR OF CUSTPO
  *@ 17,09 SAY CUSTPO
  @ 17,09 SAY LEFT(CUSTPO,10)
  *C102220,4 AME [END]
  @ 17,21 SAY IIF(!EMPTY(lcDist),lcDist,XSTORE)
  @ 17,30 SAY DEPT
  @ 17,36 SAY ORDER
  @ 17,44 SAY PTERMS
  @ 17,60 SAY REP1
  @ 17,64 SAY REP2
  @ 17,67 SAY PSHIPVIA

  *------------------
  * PRINT SCALES
  *------------------
  lcScalStr = SPACE(0)
  lcInvNo   = InvHdr.Invoice

  =lpGtScl(lcInvNo)
  SELECT Scale
  Row = 19
  FOR lnI = 1 TO LEN(lcScalStr)/3
    lcSizeScal = SUBSTR( lcScalStr, 1+(lnI-1)*3,3  )
    SEEK 'S' + lcSizeScal
    @ Row,22 SAY LEFT(lcSizeScal,1)
     
    @ Row,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
    @ Row,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
    @ Row,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
    @ Row,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
    @ Row,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
    @ Row,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
    @ Row,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
    @ Row,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
     
    Row = Row + 1
  ENDFOR

  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT INVLINE
  XSTORE = STORE
  XSCALE = ' '
  ROW    = 24

  DO WHILE .NOT. XNOTE_LOOP           
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW 
      IF ROW >= MAXROW
        =lfGetcont()
        =lfGetInHdr()
        LOOP
      ENDIF
      EXIT
    ENDIF

    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF
    lcKEY = INVLINE.STYLE         
    =SEEK(lcKEY,'STYLE')
    SELECT INVLINE

    @ ROW,00 SAY STRTRAN(STYLE,SUBSTR(STYLE,lnClrPos,lnClrLen),'')

    @ ROW,13 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
    
    @ ROW,22 SAY LEFT(STYLE.SCALE,1)
    
    @ ROW,23 SAY QTY1   PICTURE '@Z 9999'  
    @ ROW,27 SAY QTY2   PICTURE '@Z 9999'  
    @ ROW,31 SAY QTY3   PICTURE '@Z 9999'  
    @ ROW,35 SAY QTY4   PICTURE '@Z 9999'  
    @ ROW,39 SAY QTY5   PICTURE '@Z 9999'  
    @ ROW,43 SAY QTY6   PICTURE '@Z 9999'  
    @ ROW,47 SAY QTY7   PICTURE '@Z 9999'  
    @ ROW,51 SAY QTY8   PICTURE '@Z 9999'  
    @ ROW,55 SAY TOTQTY PICTURE '99999'   

    * SUM INVOICE TOTALS
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT
    @ ROW,63  SAY PRICE     PICTURE '9999.99'
    @ ROW,71  SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW + 1
   
    @ ROW,00 SAY 'STYLE DESC : '+STYLE.Desc
    ROW = ROW + 1
      
    * GET NEXT LINE ITEM
    SELECT INVLINE
    SKIP
  ENDDO
  *** END LINE PROCESSING
  *C102220,4 AME [START] PRITN NOTES DUE TO OG CONDITION  
  IF llRpInvNot
  *C102220,4 AME [end]
    *** Print invoice Notepad.
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
   
    IF TYPE + KEY <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    IF TYPE + KEY = 'C' + XINVOICE
     
      @ ROW,02 SAY '* -- N O T E S -- *' 
      ROW = ROW + 1 
     
      DO WHILE lnNotLine <= lnMemLins

        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW      && if there is a line will de delete from 
          =lfGetcont()        && the memo field
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
       
      IF .NOT. XNOTE_LOOP
        @ ROW,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        ROW = ROW + 1 
      ELSE
        =lfGetcont()        && the memo field
        =lfGetInHdr()
      ENDIF
    ENDIF

    SET MEMOWIDTH TO lnOldMemW
  *C102220,4 AME [START] PRITN NOTES DUE TO OG CONDITION  
  ENDIF
  *C102220,4 AME [END]
  SELECT INVHDR
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,54 SAY XPIECES              PICTURE '999999'
  @ ROW,70 SAY XSUBTOTAL            PICTURE '9999999.99'
  ROW=ROW+1

  IF DISCOUNT <> 0
    @ ROW,12 SAY 'TOTAL - D I S C O U N T'
    @ ROW,69 SAY DISCOUNT         PICTURE '99999999.99'
    ROW=ROW+1
  ENDIF

  *** Print the tax rate and tax amount 
  IF llTax .AND. lcTaxMeth = 'M' 

    XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
    @ ROW,12 SAY lcTaxDesc
    @ Row,33 SAY lcTaxRefr
    @ ROW,64 SAY XSTRING_RATE + '%'
    @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
    ROW = ROW + 1


    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF  
  
  ENDIF  

  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
    @ ROW,12 SAY 'TOTAL - F R E I G H T'
    @ ROW,70 SAY WKAMT            PICTURE '9999999.99'
    ROW=ROW+1
  ENDIF

  *** Print the tax rate and tax amount 
  IF llTax .AND. lcTaxMeth = 'A'
    XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
    @ ROW,12 SAY lcTaxDesc
    @ Row,33 SAY lcTaxRefr
    @ ROW,64 SAY XSTRING_RATE + '%'
    @ ROW,70 SAY INVHDR.TAX_AMT PICTURE '9999999.99'
    ROW = ROW + 1

    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF  
  ENDIF  

  @ 54,01 SAY XPHONE  PICTURE lcPhonPict SIZE 1,16
  @ 54,18 SAY XNOTE1 + ' ' + XNOTE2
  IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
    @ 56,10 SAY lcRpMsg1
    @ 57,10 SAY lcRpMsg2
  ENDIF

  IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
    @ 59,56 SAY 'APPROVAL: ' + INVHDR.APPROVAL
  ENDIF
  @ 60,00 SAY INVHDR.PIKTKT
   
  @ 60,08 SAY ShipDate
  @ 60,17 SAY CARTONS               PICTURE '@Z 999'
  @ 60,25 SAY WEIGHT                PICTURE '@Z 99999'
  @ 60,35 SAY INVHDR.INVOICE
  @ 60,70 SAY INVHDR.TOTALCHG      PICTURE '9999999.99'

  SELECT INVHDR

  IF EOF()
    NEWDOC = .F.
    SET DEVICE TO SCREEN
    RETURN
  ELSE
    NEWDOC = .T.
  ENDIF
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
ENDSCAN   && End of main Scan loop
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : Ahmed Mohamed ElAnwar (AME)
*! Date      : 03/15/2001
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetInHdr()
*!*************************************************************
FUNCTION lfGetInHdr

lcAlias = SELECT(0)

SELECT INVHDR
* LINE 2
@ 02,05 SAY 'INVOICE: '+XINVOICE

* LINE 3
@ 03,05 SAY INVDATE

* LINE 4
@ 04,05 SAY IIF(!EMPTY(INVHDR.CUSTPO),'PO: '+INVHDR.CUSTPO,"")
IF PRINTFCTR
  @ 04,50 SAY 'R E M I T   T O:'
  @ 05,50 SAY XFNAME
  @ 06,50 SAY XFADDR1
  @ 07,50 SAY XFADDR2
  @ 08,50 SAY XFADDR3
ENDIF

  
* LINE 10
@ 10,10 SAY lcSolTName
@ 10,47 SAY lcShpTName
 
* LINE 11
@ 11,10 SAY laSoldTo[1]
@ 11,47 SAY laShipTo[1]


* LINE 12
@ 12,10 SAY laSoldTo[2]
@ 12,47 SAY laShipTo[2]
 
* LINE 13
@ 13,10 SAY laSoldTo[3]
@ 13,47 SAY laShipTo[3]

@ 17,01 SAY ACCOUNT
@ 17,09 SAY CUSTPO
@ 17,21 SAY IIF(!EMPTY(lcDist),lcDist,XSTORE)
@ 17,30 SAY DEPT
@ 17,36 SAY ORDER
@ 17,44 SAY PTERMS
@ 17,60 SAY REP1
@ 17,64 SAY REP2
@ 17,67 SAY PSHIPVIA


SELECT Scale
Row = 19
FOR lnI = 1 TO LEN(lcScalStr)/3
  lcSizeScal = SUBSTR( lcScalStr, 1+(lnI-1)*3,3  )
  SEEK 'S' + lcSizeScal
  @ Row,22 SAY LEFT(lcSizeScal,1)
   
  @ Row,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ Row,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ Row,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ Row,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ Row,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ Row,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ Row,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  @ Row,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
    
  Row = Row + 1

ENDFOR
  
ROW    = 24
SELECT (lcAlias)
*!*************************************************************
*! Name      : lfGetcont
*! Developer : Ahmed Mohamed ElAnwar (AME)
*! Date      : 03/15/2001
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfGetcont

lcAlias = SELECT(0)
@ ROW+1,12 SAY 'C O N T I N U E D ...'
SELECT INVHDR
@ 54,01 SAY XPHONE  PICTURE lcPhonPict SIZE 1,16
@ 54,18 SAY INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
IF LEN(TRIM(lcRpMsg1  + lcRpMsg2)) >0
  @ 56,10 SAY lcRpMsg1 
  @ 57,10 SAY lcRpMsg2
ENDIF

@ 60,02 SAY INVHDR.PIKTKT
@ 60,09 SAY ShipDate
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE    
@ 60,71 SAY '******.**'
SELECT (lcAlias)

*!*************************************************************
*! Name    : lpGtScl.
*! Auth    : Ahmed Mohamed ElAnwar (AME)
*! Date    : 03/15/2001
*! Purpose : Build a string holding the first uniqe 5 scales
*!            from the invoice line scal.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Passed :
*!        Parameters : 
*!           lcInvNo : The Current invoice number.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Returned : 
*!        Variables  : lcScalStr.
*!*************************************************************
PROCEDURE lpGtScl

PARAMETERS lcInvNo

PRIVATE lnRecNo, lcAlias, lnCount

*-- Save the current alias.
lcAlias = SELECT(0)

SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)

lnCount = 1

SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 5
  IF !( Scale $ lcScalStr )
    lcScalStr = lcScalStr + Scale
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN

= SEEK(lcExp,"InvLine")

*-- Select the old alias.
SELECT (lcAlias)

*!*************************************************************
*! Name : lfGetZone.
*! Auth : Ahmed Mohamed ElAnwar (AME)
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Get the zone to be printed in the invoice format.
*!*************************************************************
FUNCTION lfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
PRIVATE lnOldWrk

IF !USED('SYSZONES')
  lnOldWrk = SELECT()
  SELECT 0
  =gfOpenFile(GcSysHome+'SYSZONES',GcSysHome+'SYSZONES','SH')
  SELECT (lnOldWrk)
ENDIF

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'SYSZONES'),'',SYSZONES.ZONE)

