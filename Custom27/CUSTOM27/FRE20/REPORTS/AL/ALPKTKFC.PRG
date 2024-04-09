*:************************************************************************
*:
*: Procedure file: ALPKTKFC.PRG   FOR CUST #102193
*:                 Custom Pik Ticket for Free Country   
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: AHMED MOHAMED EL_ANWAR (AME)
*:      Copyright (c) 
*:  Last modified: 02/18/2001
*:
*:  Procs & Fncts: lfPrnStrn
*:               : lfGetLoc
*:				 : lfContuLoc
*:               : lpFilEmpLn
*:               : lpSidLabls 
*:				 : lpHrdLabls 
*:				 : lpOldFoter
*:				 : lpNewFoter
*:				 : lpPrtSku
*:************************************************************************
*:This Program is based on AlPktkE.prg  / In A26 it was C101246 & c101302
*:************************************************************************ 
*: Modifications
**************************************************************************** 


SELECT (lcTmpOrdL)
lcOldKey = ORDER()
INDEX ON PikTkt + Order +IIF(PIKTKT = "******",ORDLINE.CWARECODE,'') + cGrupDetal + STR(LineNo , 6) ;
	 TAG NewIndex 
*-- Set the memory variables.
XPIKNAME     =  lcPrtAdr # "N"


STORE 0 TO  lnClrLen, lnClrPos
*lnClrLen      && color segment length.
*lnClrPos      && color segment position.  

*---- Get The Color Segment Position & length
= lfGetMaskD('C',@lnClrLen,@lnClrPos)


STORE 0.00 TO XORDTOT, XVALUE , SVALUE , XTOTQTY

STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4

HLINE5 = lcXphone

IF lcPrtAdr = "C"
  IF LEN(HLINE3) = 0
    STORE HLINE4 TO HLINE3
    STORE HLINE5 TO HLINE4
    STORE ' '    TO HLINE5
  ENDIF
ENDIF
*--Get Memorey Variables[Start]
XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)
*--Get Memorey Variables[End]
SELECT (lcTmpOrdL)

IF EOF()
  llNoRec = .T.
  *-- Displaying a message to indicate that no
  *-- records were found to display.
  =gfModalGen('TRM00052B00000','DIALOG' )

  SET DEVICE TO SCREEN
  RETURN
ENDIF

lcSpcOrd = ORDER("SPCK_LIN")
lcWhsOrd = ORDER("WHSLOC") 
SET ORDER TO TAG WHSLOCST IN WHSLOC
SET ORDER TO TAG SPCKLINS IN SPCK_LIN


STORE ' ' TO lcStrToPrn, lcSkuSize, lcSclStrn, lcStr


lnLength=07
lnLen     = 0
llScale = .T.

*-- MAIN LOOP
llEndPT = .F.
NEWDOC  = .T.

lnNotLine = 1
SET DEVICE TO PRINT

SELECT (lcTmpOrdL)

SCAN

  lcWareH = OrdLine.CWareCode     &&variable to hold warehouse code
  IF lcPrtAdr = "W"
    IF PIKTKT = "******"     && just for ALLOCATED lines
      =SEEK(ORDLINE.CWARECODE,"WAREHOUS")
    ENDIF
    HLINE2     = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    HLINE3     = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    HLINE4     = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    HLINE5     = TRANSFORM(WAREHOUS.CPHONE,lcPhonPict)
    IF LEN(HLINE3) = 0
      STORE HLINE4 TO HLINE3
      STORE HLINE5 TO HLINE4
      STORE ' '    TO HLINE5
    ENDIF
  ENDIF
 

  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
     XPIKTKT  = PIKTKT
     XORDER   = ORDER
     XDATE    = PIKDATE
     XSTORE   = ORDHDR.STORE
     SELECT ORDHDR
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XORDTOT, XVALUE , SVALUE

     IF ORDHDR.MultiPO 
       lcCustPO=LEFT(CustPO,10)
     ENDIF 

     XBTNAME  = CUSTOMER.BTNAME
     
     XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2') 
     XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
     XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
     
     IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
     ENDIF

     *-- GET DESCRIPTIONS FOR CODED FIELDS

     SELECT CODES
     SET ORDER TO CODES IN CODES 

     *TERMS
     XTERM_DATA = gfCodDes(ORDHDR.CTERMCODE , 'CTERMCODE')

     *SHIPVIA
     XSHIP_DATA = gfCodDes(ORDHDR.SHIPVIA , 'SHIPVIA')

     *SPC INST.
     XSPCI_DATA = gfCodDes(ORDHDR.SPCINST , 'SPCINST')

     *SEASON
     XSEAS_DATA  = gfCodDes(ORDHDR.SEASON , 'SEASON')

     *DIVISION
     XDIVI_DATA  = gfCodDes(ORDHDR.CDIVISION , 'CDIVISION')

     *COLOR
     lcClrDesc  = gfCodDes(SUBSTR(ORDLINE.STYLE,lnClrPos,lnClrLen) , 'COLOR')

     SELECT (lcTmpOrdL)
     IF PIKTKT = "******"
       =SEEK(ORDLINE.CWARECODE,"WAREHOUS")
     ENDIF
     HLINE1 = IIF(EMPTY(lcDivLName) ,IIF(lcPrtAdr = "W", SUBSTR(WAREHOUS.CDESC,1,30) , lcCompName) , lcDivLName)      


     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0
     * GET DESCRIPTIONS FOR CODED FIELDS [end]

     *-- GET THE SIZE SCALES
     *-- Moved and enhanced to get the printed scales
     *-- once outside the loop not every time in the loop because it will 
     *-- be the same REM1
     XSCALE2   = SPACE(1)
     SELECT (lcTmpOrdL)
     lcKey = EVAL(KEY())   && Variable to hold the record number.
     STORE 0 TO lnSca
     DIMENSION laSca[1]
     laSca = .F.

     DO WHILE lcKey = PIKTKT+ORDER .AND. lnSca < 5 .AND. !EOF()
       IF ASCAN(laSca,SCALE) <> 0
         SKIP
         LOOP
       ENDIF
       DIMENSION laSca[lnSca+1]  
       laSca[lnSca+1] = SCALE
       lnSca = lnSca +1
       SKIP
     ENDDO
     =SEEK(lcKey)  && Back to original record number.
  ENDIF
  *-- END PICK TKT/ORDER SELECTION.
  
  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT OrdHdr
  
  lcDistCntr = ""
  
  IF Alt_ShpTo
    
    lcShpTName  = OrdHdr.STName   
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3
    laShipTo[4] = OrdHdr.cAddress4
    laShipTo[5] = OrdHdr.cAddress5

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]

    
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer

    
    lnRecNo = RECNO()       && Variable to hold the record number.
    IF !EMPTY(Dist_Ctr) 
      lcDistCntr = Dist_Ctr
      =seek('S'+XACCOUNT+lcDistCntr)
    ENDIF  
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)

    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 ) 
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 ) 
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
    
    IF !EMPTY(lcDistCntr) 
      GO lnRecNo        
    ENDIF         

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR
  *-- START PRINTING
  
  DO lpHrdLabls

  *-- LINE LOOP  rem2
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  ROW     = 24
    

  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT  .AND. LINENO <> 0 .AND. IIF(PIKTKT = "******",ORDLINE.CWareCode = lcWareh,.T.)
    IF Row >= 55
      DO lpOldFoter
      DO lpHrdLabls
      Row = 24
    ELSE
      IF ROW >= 51
        DO WHILE ROW<=55
          DO lpSidLabls
          ROW = ROW +1
        ENDDO
        DO lpOldFoter
        DO lpHrdLabls
        Row = 24
      ENDIF
    ENDIF

    *-- Modified to get the Style/Color location.     
    SELECT Style
    IF SEEK(&lcTmpOrdL..Style)    HDM         
      XSTYDESC   = DESC
      lcStyLocat = Location
      lcScale    = Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF
    
    *-- To get the color description.
    SELECT (lcTmpOrdL)
    
    @ ROW,01 SAY STYLE
    @ ROW,29 SAY LEFT(lcScale,2)
    
    @ ROW,31 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,35 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,40 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,44 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,49 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,58 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,62 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,68 SAY TOTPIK PICTURE '99999'
    @ ROW,75 SAY TOTPIK PICTURE '9999'  
    
    
    DO lpSidLabls
    ROW = ROW + 1

    IF !llRpSkuBck
      @ ROW,01 SAY 'STYLE DESC.:'+XStyDesc
      @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.: ' + lcClrDesc, '')
      IF llRpStyLoc
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
          DO lpSidLabls
          Row = Row + 1
          IF LEN(lcStr) > 18
            =lfContuLoc()
          ENDIF  
        ELSE
          DO lpSidLabls
          Row = Row + 1  
        ENDIF
      ENDIF  
    ELSE

      IF SEEK('P' + ORDHDR.Account + &lcTmpOrdL..Style , 'SPCK_LIN')
        @ ROW,01 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY 'STYLE DESC.:'+XStyDesc
        @ ROW,59 SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,10), '')        
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          @ ROW,lcCol SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,76-lcCol), '')
          DO lpSidLabls
          Row = Row + 1
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'BINS    :'+ SUBSTR(lcStr,1,85)
              DO lpSidLabls
              Row = Row + 1
              IF LEN(lcStr) > 85
                =lfContuLoc()
              ENDIF
            ELSE
              DO lpSidLabls
              Row = Row + 1  
            ENDIF
          ENDIF  
          DO lpSidLabls
          Row = Row + 1
        ELSE
          @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
              DO lpSidLabls
              Row = Row + 1

              IF LEN(lcStr) > 18
                =lfContuLoc()
              ENDIF  
            ELSE
              DO lpSidLabls
              Row = Row + 1  
           ENDIF
          ELSE
            DO lpSidLabls
            Row = Row + 1  
          ENDIF  
          = lfPrnStrn() 
          lcSkuSize=lcStrToPrn+' '+lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)
          IF !EMPTY(lcSkuSize)
            DO lpSidLabls
            Row = Row + 1  
          ENDIF
        ENDIF
      ENDIF  
    ENDIF  

    *-- To print the color description, And the 
    *-- Style/Color location.
    DO lpSidLabls
    Row = Row + 1

    * SUM ORDER TOTALS
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK
 
    SKIP                        
  ENDDO                         
  *-- END PRINT LINE LOOP
  IF NewDoc
     
    *-- To print the notepad.
    IF llRpOrdLNt
      SELECT NotePad
      IF SEEK('B' + OrdHdr.Order)

        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)

        @ Row,00 SAY '* -- N O T E S -- *' 
        DO lpSidLabls
        Row = Row + 1
        

        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 55
            DO lpOldFoter
            DO lpHrdLabls
          ENDIF
          
          @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
          DO lpSidLabls
          ROW = ROW + 1
          lnNotLine = lnNotLine + 1
        ENDDO
    
        SET MEMOWIDTH TO lnOldMemW

      ENDIF
    ENDIF  
   
    lnNotLine = 1
    llEndPT = .T.
    DO lpFilEmpLn        
    DO lpNewFoter        
  ELSE
    llEndPT = .F.
    DO lpFilEmpLn   
    DO lpOldFoter   
    LOOP
  ENDIF

  SELECT PIKTKT
  SELECT (lcTmpOrdL)
  IF PIKTKT = "******" .AND. LINENO <> 0 .AND. !EOF()
    SKIP -1
  ENDIF
  llScale = .T.
ENDSCAN
SET DEVICE TO SCREEN

*-- RESTORE ORIGINAL ORDERS
SET ORDER TO &lcSpcOrd IN SPCK_LIN
SET ORDER TO &lcWhsOrd IN WHSLOC
SELECT (lcTmpOrdL)
SET ORDER TO lcOldKey

RETURN
*-- END ALPKTKFC.PRG


*!*************************************************************
*! Name : lpFilEmpLn.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Fill the empty pick ticit lines.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedures : ALPKTKE.
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE Row <= 53
  DO lpSidLabls 
  Row = Row + 1
ENDDO


*!*************************************************************
*! Name : lpSidLabls.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Print the Side labels.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedures : ALPKTKFC.
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,093 SAY IIF(XPIKNAME=.T.,HLINE1,'')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,093 SAY IIF(XPIKNAME=.T.,HLINE2,'')
  CASE ROW = 36 .OR. ROW = 52
     @ ROW,093 SAY IIF(XPIKNAME=.T.,HLINE3,'')
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53 .AND. NewDoc
      @ 53,000 SAY lcRpMsg1                    
    ENDIF  
    @ ROW,093 SAY IIF(XPIKNAME=.T.,HLINE4,'')
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,093 SAY XSTNAME
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,093 SAY XSTADDR1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,093 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,093 SAY XSTADDR3
  CASE ROW = 30 .OR. ROW = 46
    @ ROW,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,LEFT(OrdHdr.CUSTPO,10)) 
    @ ROW,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
    @ ROW,111 SAY ORDHDR.DEPT
    @ ROW,120 SAY XORDER
  CASE ROW = 31 .OR. ROW = 48
    @ ROW,101 SAY XPIKTKT
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ENDCASE


*!*************************************************************
*! Name : lpHrdLabls.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Print the Header labels.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedures : ALPKTKFC.
*!*************************************************************
PROCEDURE lpHrdLabls
*-- Line 0
@ 02,001 SAY IIF(XPIKNAME=.T.,HLINE1,'')            
@ 02,095 SAY IIF(XPIKNAME=.T.,HLINE1,'')            

*-- Line 1
@ 03,001 SAY IIF(XPIKNAME=.T.,HLINE2,'')            
@ 03,095 SAY IIF(XPIKNAME=.T.,HLINE2,'')            

*-- Line 2
@ 04,001 SAY IIF(XPIKNAME=.T.,HLINE3,'')            
@ 04,095 SAY IIF(XPIKNAME=.T.,HLINE3,'')            
  
*-- Line 3
@ 05,001 SAY IIF(XPIKNAME=.T.,HLINE4,'')            
@ 05,061 SAY SUBSTR(ALLTRIM(XDIVI_DATA),1,7)
@ 05,074 SAY XPIKTKT

@ 05,95 SAY IIF(XPIKNAME=.T.,HLINE4,'')            
  
*-- Line 4
@ 07,095 SAY XSTNAME                                
  
*-- Line 5
@ 08,061 SAY ORDHDR.APPROVAL
@ 08,74 SAY XDATE
@ 08,095 SAY XSTADDR1                               
  
*-- Line 6
@ 09,095 SAY XSTADDR2
  
*-- Line 7
@ 10,005 SAY XBTNAME
@ 10,048 SAY XSTNAME
@ 10,095 SAY XSTADDR3                               
  
*-- Line 8
@ 11,005 SAY XBTADDR1
@ 11,048 SAY XSTADDR1
  
*-- Line 9
@ 12,005 SAY XBTADDR2
@ 12,048 SAY XSTADDR2
  
*-- Line 10
@ 13,005 SAY XBTADDR3
@ 13,048 SAY XSTADDR3
@ 13,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,LEFT(OrdHdr.CUSTPO,10))  

@ 13,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 13,111 SAY ORDHDR.DEPT                           
@ 13,120 SAY XORDER
  
*-- Line 11
@ 15,101 SAY XPIKTKT
@ 15,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10) 
*-- Line 13

@ 17,093 SAY IIF(XPIKNAME=.T.,HLINE1,'')            

*-- Line 15
@ 18,001 SAY XACCOUNT
@ 18,007 SAY XORDER
@ 18,013 SAY ORDHDR.REP1
@ 18,017 SAY ORDHDR.REP2
@ 18,021 SAY ORDHDR.ENTERED
@ 18,030 SAY ORDHDR.START
@ 18,038 SAY ORDHDR.COMPLETE
@ 18,046 SAY SUBSTR(XTERM_DATA,1,14)                
@ 18,061 SAY XSTORE         PICTURE '####'
@ 18,069 SAY IIF(ORDHDR.MultiPO,lcCustPO,LEFT(OrdHdr.CUSTPO,10))   
@ 18,080 SAY ORDHDR.DEPT 
@ 18,093 SAY IIF(XPIKNAME=.T.,HLINE2,'')            

@ 19,093 SAY IIF(XPIKNAME=.T.,HLINE3,'')            

ROW = 20
IF llScale
  lnSca = IIF(lnSca > 4,4,lnSca)
  FOR I = 1 TO lnSca
    IF !EMPTY(laSca[I])
      @ ROW,29 SAY LEFT(laSca[I],2)
      lcScale = GETSCALE(laSca[I],SPACE(1))
      @ ROW,31 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),4,' ')
      @ ROW,35 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),4,' ')
      @ ROW,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),4,' ')
      @ ROW,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),4,' ')
      @ ROW,49 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),4,' ')
      @ ROW,53 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),4,' ')
      @ ROW,58 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,4),4,' ')
      @ ROW,62 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,4),4,' ')
      IF I = 1
        @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE4,'')            
      ENDIF
      DO lpSidLabls
      ROW = ROW+1
    ENDIF    
  ENDFOR
  llScale =.F.
  ROW= ROW + 1
ENDIF

*!*************************************************************
*! Name : lpOldFoter.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Print the form footer at in the middle of a spacific 
*!            pick ticket. 
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedures : ALPKTKFC.
*!*************************************************************
PROCEDURE lpOldFoter

@ 55,93 SAY XSTNAME
@ 56,93 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY XSTADDR2
@ 58,93 SAY XSTADDR3
@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,LEFT(OrdHdr.CUSTPO,10))  

@ 63,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)

@ 63,111 SAY ORDHDR.DEPT            
@ 63,120 SAY XORDER
@ 64,101 SAY XPIKTKT
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ROW = ROW + 1

*!*************************************************************
*! Name : lpNewFoter.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Print the form footer at the end of a spacific 
*!            pick ticket. 
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedures : ALPKTKFC.
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 54,000 SAY lcRpMsg2          
@ 55,000 SAY lcRpMsg3          
@ 56,93 SAY XSTNAME

@ 57,93 SAY XSTADDR1

lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', SUBSTR(OrdHdr.Note2,1,25), '')


@ 58,076 SAY XPIECES PICTURE '999999' 
@ 58,093 SAY XSTADDR2
@ 59,093 SAY XSTADDR3
@ 62,060 SAY SUBSTR(XSHIP_DATA,1,11) 
@ 62,076 SAY IIF(llRpStyPrc ,XVALUE,'') PICTURE '99999.99'

@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,LEFT(OrdHdr.CUSTPO,10))  

@ 63,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)

@ 63,111 SAY ORDHDR.DEPT            
@ 63,120 SAY XORDER

@ 64,101 SAY XPIKTKT
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)

XVALUE = 0.00
ROW = ROW + 1

*!***************************************************************
*! Name : lpPrtSku.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

PROCEDURE lpPrtSku

IF ! SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = " "
  
  RETURN
ENDIF
lcAlias=ALIAS()

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M' + ORDHDR.Account , 'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 
  = SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)

  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn)+2
  lnLocCol = LEN(lcStrToPrn) + 5

ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
 
ENDIF
SELECT (lcAlias)
RETURN

*!***************************************************************
*!Name : lfPrnStrn.
*!Auth : AHMED MOHAMED EL_ANWAR (AME)
*!Date : 02/18/2001
*!***************************************************************
*!Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfPrnStrn
lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '
IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style , 'Spck_Lin')
  lcKey='S' + ORDHDR.Account + &lcTmpOrdL..Style
  lnSep=1
  Q=1
  W=STR(Q,1)

  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin
  SCAN REST WHILE 'S' + ORDHDR.Account + &lcTmpOrdL..Style = lcKey

    IF &lcTmpOrdL..Qty&Z > 0
        lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,9,5)+' '
      
    ENDIF
    lnSep=lnSep+6
    X=X+1
    Z=STR(X,1) 
    Q=Q+1
    W=STR(Q,1)


    IF Z='9'
      EXIT
    ENDIF  
  ENDSCAN
  lcSkuSize=ALLTRIM(lcSkuSize)
ENDIF
SELECT (lcAlias)
RETURN

*!***************************************************************
*! Name : lfGetLoc.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfGetLoc
lcAlias=ALIAS()

lcStr=" "

SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)   
  SCAN REST WHILE Style= &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)

*!***************************************************************
*! Name : lfContuLoc.
*! Auth : AHMED MOHAMED EL_ANWAR (AME)
*! Date : 02/18/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfContuLoc
FOR I = 12 TO lnLen  

   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   
   DO lpSidLabls
   Row = Row + 1
ENDFOR

*!*************************************************************
*! FUNCTION Name  : lfGetMaskD
*! Developer       :AHMED MOHAMED EL_ANWAR (AME)
*! Date            : 02/14/2001
*! Purpose         : To get segment position & length.
*!*************************************************************
*: Calls              :  gfItemMask()
*:*************************************************************
*! Example            :  =lfGetMaskD(Segment,@length,@position)
*!*************************************************************
FUNCTION lfGetMaskD 
PARAMETERS Seg,lnLen,lnPos
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]=Seg
    lnLen = LEN(laItemSeg[lnCount,3])
    lnPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*---END OF FUNCTION lfGetMaskD

*--------------------------------
*    END ALPKTKFC.PRG
*--------------------------------
