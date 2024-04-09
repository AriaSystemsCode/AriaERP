*!********************************************************************
*: Program file  : SOAPP100  (REFERENCE, C102209)
*: Program desc. : CUSTOMIZED OPEN TO SELL REPORT FOR APPAREL RESOURCE
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 2.7
*:         Module: SO
*:      Developer: AHMED MOHAMED ELANWER (AME)
*:      Date     : 03/22/2001 
*!********************************************************************
*: Calls         : 
*!********************************************************************
*: Passed Parameters  :NONE
*!********************************************************************
*: Example            : SOAPP100
*!********************************************************************
*:Modifications:
*!********************************************************************

STORE 0 TO lnClrLen,lnClrPos && Vars. hold color seg. position and length.
=lfGetMaskC(@lnClrLen,@lnClrPos) && get color seg. position and length.

************************Make tables relations*************************************

SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR

SELECT POSLN
SET RELATION TO CSTYTYPE+PO INTO POSHDR

SELECT RETLINE
SET RELATION TO CRMEMO INTO RETHDR

SELECT ORDLINE
SET RELATION TO 'O'+ORDLINE.ORDER INTO ORDHDR

SELECT CUTTKTL
SET RELATION TO CUTTKT INTO CUTTKTH

*******************End of relations part*************************************

******************Collecting Temp File Data*********************************
IF llOGFltCh 						&& IF WE HAVE CHANGED THE OG
 
  DIMENSION LAPUR(10),LAREC(10),LASHIP(10),LAORDS(10),LARET(10)  
  
  lcTmpOTS = gfTempName()
  = lfCrtCrs() && CREATE TEMP OTS FILE
 
  SELECT STYLE
  SCAN FOR &lcRpExp
    STORE 0 TO LAPUR,LAREC,LASHIP,LAORDS,LARET
    
    lcStyle = STYLE
    
    *******************SELECT VALID RECORDS FROM STYLE P/O FILE*************  
    SELECT POSLN
    SEEK lcStyle
    
    SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = lcStyle FOR TRANCD = '1' .OR. TRANCD = '2'
      
      IF POSHDR.STATUS <> 'X'

        *C102209,4 AME [Start]
        WAIT WINDOW  "Colecting Data of Style: " + lcStyle  + "PO # " + PO NOWAIT 
        *C102209,4 AME [End]        

        FOR I=1 TO 8
          Z=STR(I,1)
          IF TRANCD='1'
            laPur(I)=laPur(I)+QTY&Z
          ELSE
            laRec(I)=laRec(I)+QTY&Z
          ENDIF
        ENDFOR
        IF TRANCD='1'
          laPur(9)=laPur(9)+TOTQTY
          laPur(10)=laPur(10)+(TOTQTY*STYLE.PRICEA)
        ELSE
          laRec(9)=laRec(9)+TOTQTY
          laRec(10)=laRec(10)+(TOTQTY*STYLE.PRICEA)
        ENDIF
                  
      ENDIF
    ENDSCAN  && PosLn
    ********END OF SELECTING PURCHASED & RECEIVED QUANTITIES FROM POSLN******

    *******************SELECT VALID RECORDS FROM CUTTING TICKET FILE******
    SELECT CUTTKTL
    SEEK lcStyle
    
    SCAN REST WHILE STYLE+CUTTKT+TRANCD = lcStyle FOR TRANCD='1' .OR. TRANCD='2'
      IF CUTTKTH.STATUS <> 'X' && IF CUTTKT IS NOT CANCELLED
        
        *C102209,4 AME [Start]
        WAIT WINDOW  "Colecting Data of Style: " + lcStyle  + "CutTkt  # " + CUTTKT NOWAIT  
        *C102209,4 AME [End] 
        
        FOR I=1 TO 8
          Z=STR(I,1)
          IF TRANCD='1'
            laPur(I)=laPur(I)+QTY&Z
          ELSE
            laRec(I)=laRec(I)+QTY&Z
          ENDIF
        ENDFOR
        IF TRANCD='1'
          laPur(9)=laPur(9)+TOTQTY
          laPur(10)=laPur(10)+(TOTQTY*STYLE.PRICEA)
        ELSE
          laRec(9)=laRec(9)+TOTQTY
          laRec(10)=laRec(10)+(TOTQTY*STYLE.PRICEA)
        ENDIF          

      ENDIF && IF CUTTKT IS NOT CANCELLED
      
    ENDSCAN && CUTTKTL
    ********END OF SELECTING PURCHASED & RECEIVED QUANTITIES FROM CUTTKTL*****

    *****************SELECT VALID RECORDS FROM INVENTORY ADJUSTMENT FILE****     
    SELECT INVTADJ
    SEEK lcStyle
    
    SCAN REST WHILE STYLE = lcStyle FOR TYPE='A'
    
      FOR I=1 TO 8
        Z=STR(I,1)
        laRec(I)=laRec(I)+ADJ&Z
      ENDFOR
      laRec(9)=laRec(9)+TOTADJ
      laRec(10)=laRec(10)+(TOTADJ*STYLE.PRICEA)
    
    ENDSCAN  && INVTADJ
    **********************END SELECTION FROM INVENTORY ADJUSTMENT***********    

    ***************SELECT VALID RECORDS FROM INVOICE LINE FILE************  
    SELECT INVLINE
    SEEK lcStyle
    
    SCAN REST WHILE  STYLE+INVOICE+STR(LINENO,6) = lcStyle FOR INVHDR.STATUS <> 'V'
      *C102209,4 AME [Start]
      WAIT WINDOW  "Colecting Data of Style: " + lcStyle  + "Invoice # " + INVOICE NOWAIT  
      *C102209,4 AME [End]

      FOR I=1 TO 8
        Z=STR(I,1)
        laShip(I)=laShip(I)+QTY&Z
      ENDFOR
      laShip(9)=laShip(9)+TOTQTY
      laShip(10)=laShip(10)+(TOTQTY*STYLE.PRICEA)

    ENDSCAN && INVLINE
    ************END CALCULATING SHIPPED QUANTITIES FROM INVLINE************

    *******************SELECT ORDERS FROM ORDLINE FILE*********************
    SELECT ORDLINE   
    SEEK lcStyle
    
    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcStyle ;
        FOR  IIF(llRpHolOrd , ORDHDR.STATUS<>'X' ,  !ORDHDR.STATUS $ 'XH' )
        
     *FOR IIF(!llRpHolOrd , !(ORDHDR.STATUS $ 'HX' ) , ORDHDR.STATUS<>'X') 
    
    *IIF(!llRpHolOrd ,ORDHDR.STATUS<>'H' .AND. ORDHDR.STATUS<>'X',ORDHDR.STATUS<>'X') 

      *C102209,4 AME [Start]
      WAIT WINDOW  "Colecting Data of Style: " + lcStyle  + "SO # " + ORDER NOWAIT  
      *C102209,4 AME [End]

      FOR I=1 TO 8
        Z=STR(I,1)
        laOrds(I)=laOrds(I)+QTY&Z
      ENDFOR

      laOrds(9)=laOrds(9)+TOTQTY
      laOrds(10)=laOrds(10)+(TOTQTY*STYLE.PRICEA)

    ENDSCAN && OrdLine
    *******************END ACCUMULATING ORDER LINES*************************
    
    *********************SELECT RETURNS FROM RETLINE************************
    SELECT RETLINE
    SEEK lcStyle
    
    SCAN REST WHILE STYLE+CRMEMO = lcStyle FOR RETHDR.STATUS <> 'V'

      *C102209,4 AME [Start]
      WAIT WINDOW  "Colecting Data of Style: " + lcStyle  + "Credit Memo # " + CRMEMO NOWAIT  
      *C102209,4 AME [End]

      FOR I=1 TO 8
        Z=STR(I,1)
        laRet(I)=laRet(I)+QTY&Z
      ENDFOR

      laRet(9)=laRet(9)+TOTQTY
      laRet(10)=laRet(10)+(TOTQTY*STYLE.PRICEA)
    
    ENDSCAN && RETLINE
  *******************END VALID RECORDS SELECTION FROM RETLINE*************    
  *****************END VALID RECORDS SELECTION FROM ALL FILES*************    
  
  *****FILL THE TEMP FILE FROM ARRAYS LARET/LAPUR/LAREC/LASHIP/LAORDS*****  
  
    SELECT &lcTmpOTS 
      FOR I=1 TO 9
        Z=STR(I,1)
        DO CASE
          CASE I=1
          *C102209,4 AME [START] stk instead of qty
          *APPEND FROM ARRAY laPur FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,;
          QTY8,TOTQTY,AVE_COST
          APPEND FROM ARRAY laPur FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,;
          STK8,TOTQTY,AVE_COST
          *C102209,4 AME [End]
        CASE I=2
          APPEND BLANK
          FOR X=1 TO 8
            Y=STR(X,1)
            *C102209,4 AME [Start] STK instead of QTY
            *REPLACE QTY&Y WITH laShip(X)+laOrds(X)
            REPLACE STK&Y WITH laShip(X)+laOrds(X)
            *C102209,4 AME [End]
          ENDFOR
          REPLACE TOTQTY     WITH laShip(9)+laOrds(9)
          REPLACE AVE_COST WITH laShip(10)+laOrds(10)
        CASE I=3
          APPEND BLANK
          FOR X=1 TO 8
            Y=STR(X,1)
            *C102209,4 AME [Start] STK instead of QTY
            *REPLACE QTY&Y WITH laPur(X)+STYLE.STK&Y-(laShip(X)+laOrds(X))
            REPLACE STK&Y WITH laPur(X)+STYLE.STK&Y-(laShip(X)+laOrds(X))
            *C102209,4 AME [End]
          ENDFOR
          REPLACE TOTQTY     WITH laPur(9)-(laShip(9)+laOrds(9))
          REPLACE AVE_COST WITH laPur(10)-(laShip(10)+laOrds(10))
        CASE I=4
          *C102209,4 AME [Start] STK instead of QTY
          *APPEND FROM ARRAY laRec FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,;
          QTY8,TOTQTY,AVE_COST
         
          APPEND FROM ARRAY laRec FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,;
          STK8,TOTQTY,AVE_COST
          *C102209,4 AME [End]
        CASE I=5
          *C102209,4 AME [Start] STK instead of QTY
          *APPEND FROM ARRAY laRet FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,;
          QTY8,TOTQTY,AVE_COST     

          APPEND FROM ARRAY laRet FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,;
          STK8,TOTQTY,AVE_COST     
          *C102209,4 AME [End]
        CASE I=6
          APPEND BLANK
          FOR X=1 TO 8
            Y=STR(X,1)
            *C102209,4 AME [Start] STK instead of QTY
            *REPLACE QTY&Y WITH laShip(X)-laRet(X)
            REPLACE STK&Y WITH laShip(X)-laRet(X)
            *C102209,4 AME [End]
          ENDFOR
          REPLACE TOTQTY     WITH laShip(9)-laRet(9)
          REPLACE AVE_COST WITH laShip(10)-laRet(10)
        CASE I=7
          *C102209,4 AME [Start] STK instead of QTY
          *APPEND FROM ARRAY laOrds FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,;
          QTY8,TOTQTY,AVE_COST
          
          APPEND FROM ARRAY laOrds FIELDS STK1,STK2,STK3,STK4,STK5,STK6,STK7,;
          STK8,TOTQTY,AVE_COST
          *C102209,4 AME [End]
        CASE I=8
          APPEND BLANK
          FOR X=1 TO 8
            Y=STR(X,1)
            *C102209,4 AME [Start] STK instead of QTY
            *REPLACE QTY&Y WITH laRec(X)-laShip(X)-laOrds(X)
            REPLACE STK&Y WITH laRec(X)-laShip(X)-laOrds(X)
            *C102209,4 AME [End]
          ENDFOR
          REPLACE TOTQTY WITH laRec(9)-laShip(9)-laOrds(9)
          REPLACE AVE_COST WITH laRec(10)-laShip(10)-laOrds(10)
        CASE I=9
          APPEND BLANK
          FOR X=1 TO 8
            Y=STR(X,1)
            *C102209,4 AME [Start] STK instead of QTY
            *REPLACE QTY&Y WITH STYLE.STK&Y
            REPLACE STK&Y WITH STYLE.STK&Y
            *C102209,4 AME [End]
          ENDFOR
          REPLACE TOTQTY WITH STYLE.TOTSTK 
          REPLACE AVE_COST WITH STYLE.TOTSTK*STYLE.PRICEA 
      ENDCASE
      REPLACE &lcTmpOTS..TRANCD     WITH Z,;          
              &lcTmpOTS..SCALE      WITH STYLE.SCALE,;
              &lcTmpOTS..PRICEA     WITH STYLE.PRICEA,;
              &lcTmpOTS..DESC       WITH STYLE.DESC,;
              &lcTmpOTS..FABRIC     WITH STYLE.FABRIC,;
              &lcTmpOTS..STYLE      WITH STYLE.STYLE,;
              &lcTmpOTS..cStyMajor  WITH STYLE.cStyMajor,;
              &lcTmpOTS..CSTYGROUP  WITH STYLE.CSTYGROUP,;
              &lcTmpOTS..SEASON     WITH STYLE.SEASON
    ENDFOR        

  ENDSCAN  && Style (Main Scan loop)
  ***************Collecting Data Part End*************************************
  
    
  **************CHECK THE TEMP FILE TO SEE IF THERE WAS ANY RECORDS***********

  SELECT &lcTmpOTS
  IF RECCOUNT()=0
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

ENDIF && IF WE HAVE CHANGED THE OG






******************************************************************
*                          REPORT  LOOP                          * 
******************************************************************

ROW    = 99
PAGENO = 0
TIME   = TIME()
XREPORT='SOAPP100'
R_WIDTH='W'
R_TITLE='OPEN TO SELL REPORT'


SELE &lcTmpOTS
LOCATE
DIMENSION SESN_SB(9,2),GROP_SB(9,2),FBRC_SB(9,2),STYL_SB(9,10)
STORE 0 TO SESN_SB,GROP_SB,FBRC_SB,STYL_SB
XSEASON=SPACE(6)

*0....+....1....+...2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* SOAPP100                                                        FIRST COMPANY                                             DATE     
* TIME                                                         OPEN TO SELL REPORT                                          PAGE#    
*====================================================================================================================================
*SEASON: XXXXXXXXX                                                                                                                  
*====================================================================================================================================
*                                                                                                                                   
*------------------------------------------------------------------------------------------------------------------------------------ 
* STYLE : XXXXXXXX    COLOR: XXX     FABRIC: XXXXXXX     DESC: XXXXXXXXXXXXXXXXXXXX              LINE PRICE: 9999999.99              
*------------------------------------------------------------------------------------------------------------------------------------ 
*                 SCALE: X =>  XXX     XXX      XXX    XXX     XXX     XXX     XXX     XXX       TOTAL      EXTENSION              
*                                                                                                                                    
* BOOKINGS        PURCHASED : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99            
*                 SOLD      : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99             
*                 OTS       : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99             
*                                                                                                                                                                          
* SHIPPING        RECEIVED  : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99              
*                 RETURNS   : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99                             
*                 NET SHIP  : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99             
*                 OPEN ORDS : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99               
*                 IMM. OTS  : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99              
*                 INVENTORY : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99              
*                                                                                                                                      
*====STYLE RECAP====================================================================================================================
*------------------------------------------------------------------------------------------------------------------------------------ 
* STYLE : XXXXXXXX                   FABRIC: XXXXXXX     DESC: XXXXXXXXXXXXXXXXXXXX                            
*------------------------------------------------------------------------------------------------------------------------------------ 
*                 SCALE: X =>  XXX     XXX      XXX    XXX     XXX     XXX     XXX     XXX       TOTAL      EXTENSION              
*                                                                                                                                    
* BOOKINGS        PURCHASED : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99            
*                 SOLD      : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99             
*                 OTS       : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99             
*                                                                                                                                                                          
* SHIPPING        RECEIVED  : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99              
*                 RETURNS   : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99                             
*                 NET SHIP  : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99             
*                 OPEN ORDS : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99               
*                 IMM. OTS  : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99              
*                 INVENTORY : 999999  999999  999999  999999  999999  999999  999999  999999    9999999    9999999999.99              
*                                                                                                                                                               
*====FABRIC RECAP===================================================================================================================
* BOOKINGS        PURCHASED : 9999999         9999999999.99                                                                         
*                 SOLD      : 9999999         9999999999.99
*                 OTS       : 9999999         9999999999.99
*                                                                                             
* SHIPPING        RECEIVED  : 9999999         9999999999.99                   
*                 RETURNS   : 9999999         9999999999.99
*                 NET SHIP  : 9999999         9999999999.99
*                 OPEN ORDS : 9999999         9999999999.99                   
*                 IMM. OTS  : 9999999         9999999999.99                   
*                 INVENTORY : 9999999         9999999999.99
*                                                                                                                                  
*====STYLE GROUP RECAP==============================================================================================================
* BOOKINGS        PURCHASED : 9999999         9999999999.99                                                                         
*                 SOLD      : 9999999         9999999999.99
*                 OTS       : 9999999         9999999999.99
*                                                                                             
* SHIPPING        RECEIVED  : 9999999         9999999999.99                   
*                 RETURNS   : 9999999         9999999999.99
*                 NET SHIP  : 9999999         9999999999.99
*                 OPEN ORDS : 9999999         9999999999.99                   
*                 IMM. OTS  : 9999999         9999999999.99                   
*                 INVENTORY : 9999999         9999999999.99
*                                                                                                                                   
*====SEASON RECAP====================================================================================================================
* BOOKINGS        PURCHASED : 9999999         9999999999.99                                                                         
*                 SOLD      : 9999999         9999999999.99
*                 OTS       : 9999999         9999999999.99
*                                                                                             
* SHIPPING        RECEIVED  : 9999999         9999999999.99                   
*                 RETURNS   : 9999999         9999999999.99
*                 NET SHIP  : 9999999         9999999999.99
*                 OPEN ORDS : 9999999         9999999999.99                   
*                 IMM. OTS  : 9999999         9999999999.99                   
*                 INVENTORY : 9999999         9999999999.99
*0....+....1....+...2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.. 

********************************PRINTING LOOP************************************
SET DEVICE TO PRINT
DO WHILE !EOF() .AND. INKEY()<>32
  IF LCRPDET_SUM='S'
    XSEASON=SEASON
  ENDIF  
  XGROUP=CSTYGROUP
  XFABRIC=FABRIC
  XSTYLE=CSTYMAJOR
  XDESC=DESC 
  XSCALE=SCALE
  *******PRINT THE HEADER********
  IF ROW>56
    PAGENO=PAGENO+1
    DO RPT_HDR WITH XREPORT,'',R_WIDTH
    ROW=5
  ENDIF  
  ********END HEADER PRINTING*********
  =SEEK('S'+XSCALE,'SCALE') 
  XPRNT_SCL= PADL(ALLTRIM(SCALE.SZ1),5,' ')+'   '+;
             PADL(ALLTRIM(SCALE.SZ2),5,' ')+'   '+;
             PADL(ALLTRIM(SCALE.SZ3),5,' ')+'   '+;
             PADL(ALLTRIM(SCALE.SZ4),5,' ')+'   '+;
             PADL(ALLTRIM(SCALE.SZ5),5,' ')+'   '+;
             PADL(ALLTRIM(SCALE.SZ6),5,' ')+'   '+;
             PADL(ALLTRIM(SCALE.SZ7),5,' ')+'   '+;                                                        
             PADL(ALLTRIM(SCALE.SZ8),5,' ')

  IF XSEASON<>SEASON .AND. lcRpDet_Sum='D'
    XSEASON=SEASON

    @ ROW,000 SAY 'SEASON:'
    lcSeaDesc =gfCodDes(Season , 'SEASON')
    @ ROW,008 SAY lcSeaDesc
  ENDIF       
  IF lcRpDet_Sum='D'
    *===========ASK FOR PAGE BREAK============*
    IF ROW>41
      PAGENO=PAGENO+1
      DO RPT_HDR WITH XREPORT,'',R_WIDTH
      ROW=5
    ENDIF  
    *==========================================*
    SELE &lcTmpOTS
    ROW=ROW+1
    @ ROW,000 SAY REPLICATE('-',132)
    ROW=ROW+1
    @ ROW,001 SAY 'STYLE :'
    @ ROW,009 SAY cStyMajor
    @ ROW,022 SAY 'COLOR:'
    @ ROW,029 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
    @ ROW,037 SAY 'FABRIC:'
    @ ROW,045 SAY FABRIC
    @ ROW,057 SAY 'DESC:'
    @ ROW,063 SAY DESC
    @ ROW,097 SAY 'LINE PRICE:'
    @ ROW,109 SAY PRICEA PICTURE '9999.99'
    ROW=ROW+1
    @ ROW,000 SAY REPLICATE('-',132)
  
    ROW=ROW+1
    @ ROW,017 SAY 'SCALE:'
    @ ROW,024 SAY LEFT(SCALE,2)
    @ ROW,026 SAY '=>'
    @ ROW,031 SAY XPRNT_SCL
    @ ROW,098 SAY 'TOTAL'
    @ ROW,110 SAY 'EXTENSION'
  
    ROW=ROW+2
  ENDIF
  DO WHILE .T.
       
    DO CASE
      CASE TRANCD='1'
        IF lcRpDet_Sum='D'
          @ ROW,001 SAY 'BOOKINGS'
          @ ROW,017 SAY 'PURCHASED :'
        ENDIF
        SESN_SB(1,1)=SESN_SB(1,1)+TOTQTY
        SESN_SB(1,2)=SESN_SB(1,2)+AVE_COST
        GROP_SB(1,1)=GROP_SB(1,1)+TOTQTY
        GROP_SB(1,2)=GROP_SB(1,2)+AVE_COST
        FBRC_SB(1,1)=FBRC_SB(1,1)+TOTQTY
        FBRC_SB(1,2)=FBRC_SB(1,2)+AVE_COST
        
        FOR X=1 TO 8 
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty
          *STYL_SB(1,X)=STYL_SB(1,X)+QTY&Y
          STYL_SB(1,X)=STYL_SB(1,X)+STK&Y
          *C102209,4 AME [END]
        ENDFOR   
        
        STYL_SB(1,9)=STYL_SB(1,9)+TOTQTY
        STYL_SB(1,10)=STYL_SB(1,10)+AVE_COST
      CASE TRANCD='2'             
        IF lcRpDet_Sum='D'
          ROW=ROW+1
          @ ROW,017 SAY 'SOLD      :'
        ENDIF
        SESN_SB(2,1)=SESN_SB(2,1)+TOTQTY
        SESN_SB(2,2)=SESN_SB(2,2)+AVE_COST
        GROP_SB(2,1)=GROP_SB(2,1)+TOTQTY
        GROP_SB(2,2)=GROP_SB(2,2)+AVE_COST
        FBRC_SB(2,1)=FBRC_SB(2,1)+TOTQTY
        FBRC_SB(2,2)=FBRC_SB(2,2)+AVE_COST
        
        FOR X=1 TO 8 
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty
          *STYL_SB(2,X)=STYL_SB(2,X)+QTY&Y
          STYL_SB(2,X)=STYL_SB(2,X)+STK&Y
          *C102209,4 AME [End]
        ENDFOR   
        
        STYL_SB(2,9)=STYL_SB(2,9)+TOTQTY
        STYL_SB(2,10)=STYL_SB(2,10)+AVE_COST
      CASE TRANCD='3'              
        IF lcRpDet_Sum='D'
          ROW=ROW+1
          @ ROW,017 SAY 'OTS       :'
        ENDIF
        SESN_SB(3,1)=SESN_SB(3,1)+TOTQTY
        SESN_SB(3,2)=SESN_SB(3,2)+AVE_COST
        GROP_SB(3,1)=GROP_SB(3,1)+TOTQTY
        GROP_SB(3,2)=GROP_SB(3,2)+AVE_COST
        FBRC_SB(3,1)=FBRC_SB(3,1)+TOTQTY
        FBRC_SB(3,2)=FBRC_SB(3,2)+AVE_COST
        
        FOR X=1 TO 8
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty
          *STYL_SB(3,X)=STYL_SB(3,X)+QTY&Y
          STYL_SB(3,X)=STYL_SB(3,X)+STK&Y
          *C102209,4 AME [End]
        ENDFOR   

        STYL_SB(3,9)=STYL_SB(3,9)+TOTQTY
        STYL_SB(3,10)=STYL_SB(3,10)+AVE_COST
      CASE TRANCD='4'              
        IF lcRpDet_Sum='D'
          ROW=ROW+2                 
          @ ROW,001 SAY 'SHIPPING'    
          @ ROW,017 SAY 'RECEIVED  :'
        ENDIF
        SESN_SB(4,1)=SESN_SB(4,1)+TOTQTY
        SESN_SB(4,2)=SESN_SB(4,2)+AVE_COST
        GROP_SB(4,1)=GROP_SB(4,1)+TOTQTY
        GROP_SB(4,2)=GROP_SB(4,2)+AVE_COST
        FBRC_SB(4,1)=FBRC_SB(4,1)+TOTQTY
        FBRC_SB(4,2)=FBRC_SB(4,2)+AVE_COST
        
        FOR X=1 TO 8
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty
          *STYL_SB(4,X)=STYL_SB(4,X)+QTY&Y
          STYL_SB(4,X)=STYL_SB(4,X)+STK&Y
          *C102209,4 AME [End]
        ENDFOR   

        STYL_SB(4,9)=STYL_SB(4,9)+TOTQTY
        STYL_SB(4,10)=STYL_SB(4,10)+AVE_COST
      CASE TRANCD='5'
        IF lcRpDet_Sum='D'
          ROW=ROW+1
          @ ROW,017 SAY 'RETURNS   :'
        ENDIF
        SESN_SB(5,1)=SESN_SB(5,1)+TOTQTY
        SESN_SB(5,2)=SESN_SB(5,2)+AVE_COST
        GROP_SB(5,1)=GROP_SB(5,1)+TOTQTY
        GROP_SB(5,2)=GROP_SB(5,2)+AVE_COST
        FBRC_SB(5,1)=FBRC_SB(5,1)+TOTQTY
        FBRC_SB(5,2)=FBRC_SB(5,2)+AVE_COST
        
        FOR X=1 TO 8
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty
          *STYL_SB(5,X)=STYL_SB(5,X)+QTY&Y
          STYL_SB(5,X)=STYL_SB(5,X)+STK&Y
          *C102209,4 AME [End]
        ENDFOR   
      
        STYL_SB(5,9)=STYL_SB(5,9)+TOTQTY
        STYL_SB(5,10)=STYL_SB(5,10)+AVE_COST         
      CASE TRANCD='6'               
        IF lcRpDet_Sum='D'
          ROW=ROW+1
          @ ROW,017 SAY 'NET SHIP  :'
        ENDIF
        SESN_SB(6,1)=SESN_SB(6,1)+TOTQTY
        SESN_SB(6,2)=SESN_SB(6,2)+AVE_COST
        GROP_SB(6,1)=GROP_SB(6,1)+TOTQTY
        GROP_SB(6,2)=GROP_SB(6,2)+AVE_COST
        FBRC_SB(6,1)=FBRC_SB(6,1)+TOTQTY
        FBRC_SB(6,2)=FBRC_SB(6,2)+AVE_COST
        
        FOR X=1 TO 8
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty
          *STYL_SB(6,X)=STYL_SB(6,X)+QTY&Y
          STYL_SB(6,X)=STYL_SB(6,X)+STK&Y
          *C102209,4 AME [End]          
        ENDFOR   
      
        STYL_SB(6,9)=STYL_SB(6,9)+TOTQTY
        STYL_SB(6,10)=STYL_SB(6,10)+AVE_COST
      CASE TRANCD='7'
        IF lcRpDet_Sum='D'
          ROW=ROW+1
          @ ROW,017 SAY 'OPEN ORDS :'
        ENDIF
        SESN_SB(7,1)=SESN_SB(7,1)+TOTQTY
        SESN_SB(7,2)=SESN_SB(7,2)+AVE_COST
        GROP_SB(7,1)=GROP_SB(7,1)+TOTQTY
        GROP_SB(7,2)=GROP_SB(7,2)+AVE_COST
        FBRC_SB(7,1)=FBRC_SB(7,1)+TOTQTY
        FBRC_SB(7,2)=FBRC_SB(7,2)+AVE_COST
        
        FOR X=1 TO 8
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty          
          *STYL_SB(7,X)=STYL_SB(7,X)+QTY&Y
          STYL_SB(7,X)=STYL_SB(7,X)+STK&Y
          *C102209,4 AME [End]
        ENDFOR   
      
        STYL_SB(7,9)=STYL_SB(7,9)+TOTQTY
        STYL_SB(7,10)=STYL_SB(7,10)+AVE_COST
      CASE TRANCD='8'             
        IF lcRpDet_Sum='D'
          ROW=ROW+1
          @ ROW,017 SAY 'IMM. OTS  :'
        ENDIF
        SESN_SB(8,1)=SESN_SB(8,1)+TOTQTY
        SESN_SB(8,2)=SESN_SB(8,2)+AVE_COST
        GROP_SB(8,1)=GROP_SB(8,1)+TOTQTY
        GROP_SB(8,2)=GROP_SB(8,2)+AVE_COST
        FBRC_SB(8,1)=FBRC_SB(8,1)+TOTQTY
        FBRC_SB(8,2)=FBRC_SB(8,2)+AVE_COST
        
        FOR X=1 TO 8
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty          
          *STYL_SB(8,X)=STYL_SB(8,X)+QTY&Y
          STYL_SB(8,X)=STYL_SB(8,X)+STK&Y
          *C102209,4 AME [End]          
        ENDFOR   
      
        STYL_SB(8,9)=STYL_SB(8,9)+TOTQTY
        STYL_SB(8,10)=STYL_SB(8,10)+AVE_COST
      CASE TRANCD='9'              
        IF lcRpDet_Sum='D'
          ROW=ROW+1
          @ ROW,017 SAY 'INVENTORY :'
        ENDIF
        SESN_SB(9,1)=SESN_SB(9,1)+TOTQTY
        SESN_SB(9,2)=SESN_SB(9,2)+AVE_COST
        GROP_SB(9,1)=GROP_SB(9,1)+TOTQTY
        GROP_SB(9,2)=GROP_SB(9,2)+AVE_COST
        FBRC_SB(9,1)=FBRC_SB(9,1)+TOTQTY
        FBRC_SB(9,2)=FBRC_SB(9,2)+AVE_COST
        
        FOR X=1 TO 8
          Y=STR(X,1)
          *C102209,4 AME [START] stk instead of qty                    
          *STYL_SB(9,X)=STYL_SB(9,X)+QTY&Y
          STYL_SB(9,X)=STYL_SB(9,X)+STK&Y
          *C102209,4 AME [End]
        ENDFOR   
    
        STYL_SB(9,9)=STYL_SB(9,9)+TOTQTY
        STYL_SB(9,10)=STYL_SB(9,10)+AVE_COST
    ENDCASE
    IF lcRpDet_Sum='D'
      *C102209,4 AME [START] shift one chr to left                            
      *XCOL=030
      XCOL=029
      *C102209,4 AME [START] stk instead of qty                            
      FOR I=1 TO 8
        Z=STR(I,1)
        *C102209,4 AME [START] stk instead of qty                            
        *@ ROW,XCOL SAY QTY&Z
        @ ROW,XCOL SAY STK&Z
        *C102209,4 AME [End]
        XCOL=XCOL+8
      ENDFOR
      @ ROW,096 SAY TOTQTY
      @ ROW,107 SAY AVE_COST
    ENDIF
    IF TRANCD='9' 
      SKIP
      DO CASE
        CASE XSEASON<>SEASON
          DO lpStyRec WITH XSTYLE,XFABRIC,XDESC,XSCALE
          DO lpPrnRec WITH 3
        CASE XGROUP<>CSTYGROUP
          DO lpStyRec WITH XSTYLE,XFABRIC,XDESC,XSCALE
          DO lpPrnRec WITH 2
        CASE XFABRIC<>FABRIC
          DO lpStyRec WITH XSTYLE,XFABRIC,XDESC,XSCALE 
          DO lpPrnRec WITH 1
        CASE XSTYLE<>CSTYMAJOR
          DO lpStyRec WITH XSTYLE,XFABRIC,XDESC,XSCALE 
      ENDCASE                   
      EXIT
    ENDIF
    IF !EOF()
      SKIP          
    ENDIF

  ENDDO  && WHILE .T.

ENDDO  && WHILE !EOF() 

@ ROW,000 SAY REPLICATE('=',132)
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

*----------------------E N D  M A I N  P R O G R A M-----------------------*



*!*************************************************************
*! Name      : lfMajExp
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Reconstruct the major seg. expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajExp()
*!*************************************************************

FUNCTION lfMajExp

RETURN lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Warehouse')-LEN(lcMajTtl),0)) + ALLTRIM(EVAL(lcTrns+".cStyMajor"))

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyMaj = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyMaj = STYLE.cStyMajor
  ELSE
    &lcStyMaj = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyMaj = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Get major seg. picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfsrvSty  (C# 102209)
*! Developer : AHMED MOHAMED ELANWAR (AME)
*! Date      : 04/05/2001
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
*C102209,4 
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfOGShowGet
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

lnClrSgPo = ASUBSCRIPT(laOGFxFlt,;
            ASCAN(laOGFxFlt,'SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen)'),1)

*-- Disable/enable Only These colors  [begin]

IF lcFreeClr = 'C'
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnClrSgPo] = .T.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ELSE
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnClrSgPo] = .F.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ENDIF

*-- Disable/enable Only These colors [end]

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
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
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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

llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
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

IF llUseByMe
  USE IN FABRIC
ENDIF  

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : lcNMajDum from Option Grid
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

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lpStyRec
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : To PRINT STYLE RECAP
*!*************************************************************
*! Called from : SOAPP100 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpStyRec(,,,)
*!*************************************************************
*
PROCEDURE lpStyRec
PARAMETER XSTYLE,XFABRIC,XDESC,XSCALE
*============CHECK FOR PAGE BREAK===============*
IF ROW>39
  PAGENO=PAGENO+1
  DO RPT_HDR WITH XREPORT,'',R_WIDTH
  ROW=5
ENDIF  
*===============================================* 
ROW=ROW+1
@ ROW,000 SAY '====='
@ ROW,005 SAY 'STYLE RECAP'
@ ROW,016 SAY REPLICATE('=',116)
ROW=ROW+2
@ ROW,000 SAY REPLICATE('-',132)
ROW=ROW+1
@ ROW,001 SAY 'STYLE :'
@ ROW,009 SAY XSTYLE
@ ROW,037 SAY 'FABRIC:'
@ ROW,045 SAY XFABRIC
@ ROW,057 SAY 'DESC:'
@ ROW,063 SAY XDESC
ROW=ROW+1
@ ROW,000 SAY REPLICATE('-',132)
ROW=ROW+1
@ ROW,017 SAY 'SCALE:'
@ ROW,024 SAY LEFT(XSCALE,2)
@ ROW,026 SAY '=>'
*B800582,1 change the scale.
=SEEK('S'+XSCALE,'SCALE') 
XPRNT_SCL= PADL(ALLTRIM(SCALE.SZ1),5,' ')+'     '+;
           PADL(ALLTRIM(SCALE.SZ2),5,' ')+'     '+;
           PADL(ALLTRIM(SCALE.SZ3),5,' ')+'     '+;
           PADL(ALLTRIM(SCALE.SZ4),5,' ')+'     '+;
           PADL(ALLTRIM(SCALE.SZ5),5,' ')+'     '+;
           PADL(ALLTRIM(SCALE.SZ6),5,' ')+'     '+;
           PADL(ALLTRIM(SCALE.SZ7),5,' ')+'     '+;
           PADL(ALLTRIM(SCALE.SZ8),5,' ')
@ ROW,034 SAY XPRNT_SCL    &&36
@ ROW,114 SAY 'TOTAL'      &&117
@ ROW,123 SAY 'EXTENSION'  &&123

ROW=ROW+2
FOR X=1 TO 9
  DO CASE
    CASE X=1
      @ ROW,001 SAY 'BOOKINGS'
      @ ROW,017 SAY 'PURCHASED :'
    CASE X=2                    
      @ ROW,017 SAY 'SOLD      :'
    CASE X=3
      @ ROW,017 SAY 'OTS       :'
    CASE X=4
      ROW=ROW+1
      @ ROW,001 SAY 'SHIPPING  :'
      @ ROW,017 SAY 'RECEIVED  :'
    CASE X=5
      @ ROW,017 SAY 'RETURNS   :' 
    CASE X=6
      @ ROW,017 SAY 'NET SHIP  :'
    CASE X=7
      @ ROW,017 SAY 'OPEN ORDS :'
    CASE X=8
      @ ROW,017 SAY 'IMM. OTS  :'
    CASE X=9
      @ ROW,017 SAY 'INVENTORY :'
  ENDCASE                   
  XCOL=029
  FOR I=1 TO 8
    @ ROW,XCOL SAY STYL_SB(X,I)
    XCOL=XCOL+10
  ENDFOR
  @ ROW,112 SAY STYL_SB(X,9) PICTURE '9999999'
  @ ROW,119 SAY STYL_SB(X,10) 
  ROW=ROW+1

ENDFOR  
ROW=ROW+1
STORE 0 TO STYL_SB
RETURN



*!*************************************************************
*! Name      : lpPrnRec
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : To PRINT STYLE GROUP,FABRIC AND SEASON RECAPS
*!           : ACCORDING TO LNCOUNT VALUE
*!*************************************************************
*! Called from : SOAPP100 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPrnRec(2)
*!*************************************************************
*

PROCEDURE lpPrnRec
PARAMETER lnCount

ROW=ROW+1
FOR Y=1 TO lnCount
  *============CHECK FOR PAGE BREAK===============*
  IF ROW>45
    PAGENO=PAGENO+1
    DO RPT_HDR WITH XREPORT,'',R_WIDTH
    ROW=5
  ENDIF  
  *===============================================* 
  @ ROW,000 SAY '====='		
  DO CASE 
    CASE Y=1
      @ ROW,005 SAY 'FABRIC RECAP'
      @ ROW,017 SAY REPLICATE('=',115)
      lcRecap='FBRC'
    CASE Y=2
      @ ROW,005 SAY 'STYLE GROUP RECAP'
      @ ROW,022 SAY REPLICATE('=',110)
      lcRecap='GROP'
    CASE Y=3
      @ ROW,005 SAY 'SEASON RECAP'
      @ ROW,017 SAY REPLICATE('=',115)
      lcRecap='SESN'
  ENDCASE
  ROW=ROW+1
  FOR X=1 TO 9        
    DO CASE
      CASE X=1
        @ ROW,001 SAY 'BOOKINGS'
        @ ROW,017 SAY 'PURCHASED :'
      CASE X=2                    
        @ ROW,017 SAY 'SOLD      :'
      CASE X=3
        @ ROW,017 SAY 'OTS       :'
      CASE X=4
        ROW=ROW+1
        @ ROW,001 SAY 'SHIPPING  :'
        @ ROW,017 SAY 'RECEIVED  :'
      CASE X=5
        @ ROW,017 SAY 'RETURNS   :' 
      CASE X=6
        @ ROW,017 SAY 'NET SHIP  :'
      CASE X=7
        @ ROW,017 SAY 'OPEN ORDS :'
      CASE X=8
        @ ROW,017 SAY 'IMM. OTS  :'
      CASE X=9
        @ ROW,017 SAY 'INVENTORY :'
    ENDCASE
    XARRAY=lcRecap+'_SB'
    @ ROW,030 SAY &XARRAY(X,1)
    @ ROW,046 SAY &XARRAY(X,2)
    ROW=ROW+1
    
  ENDFOR
  STORE 0 TO &XARRAY
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfCrtCrs
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/27/2001
*! Purpose   : To Creat Temp O.T.S. File
*!*************************************************************
*! Called from : SOAPP100 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtCrs()
*!*************************************************************
*
FUNCTION lfCrtCrs


*C102209,4 AME [Start]  Fields skt (with 7 digits ) instead of qty (with 6 digits)
*COPY STRUCTURE TO &GCWorkDir.&lcTmpOTS FIELDS STYLE.SEASON,STYLE.CSTYGROUP,STYLE.STYLE,;
               STYLE.FABRIC,STYLE.DESC,STYLE.PRICEA,;
               STYLE.SCALE,POSLN.TRANCD,STYLE.AVE_COST,POSLN.TOTQTY,;
               POSLN.QTY1,POSLN.QTY2,POSLN.QTY3,POSLN.QTY4,POSLN.QTY5,;
               POSLN.QTY6,POSLN.QTY7,POSLN.QTY8,STYLE.cStyMajor

COPY STRUCTURE TO &GCWorkDir.&lcTmpOTS FIELDS STYLE.SEASON,STYLE.CSTYGROUP,STYLE.STYLE,;
               STYLE.FABRIC,STYLE.DESC,STYLE.PRICEA,;
               STYLE.SCALE,POSLN.TRANCD,STYLE.AVE_COST,POSLN.TOTQTY,;
               STYLE.STK1,STYLE.STK2,STYLE.STK3,STYLE.STK4,STYLE.STK5,;
               STYLE.STK6,STYLE.STK7,STYLE.STK8,STYLE.cStyMajor
*CREATE TABLE &GCWorkDir.&lcTmpOTS (SEASON C(),CSTYGROUP C(),STYLE C(19),FABRIC C(),DESC C(),;
*               PRICEA N(),SCALE C(),TRANCD C(), AVE_COST N())
*C102209,4 AME [End]
SELECT 0
USE &GCWorkDir.&lcTmpOTS EXCLUSIVE
INDEX ON SEASON+CSTYGROUP+FABRIC+STYLE TAG &lcTmpOTS
SET ORDER TO TAG &lcTmpOTS


*!*************************************************************
*! FUNCTION Name  : lfGetMaskC
*! Developer       :AHMED MOHAMED EL_ANWAR (AME)
*! Date            : 03/27/2001
*! Purpose         : To get color segment position & length.
*!*************************************************************
*: Calls              :  gfItemMask()
*:*************************************************************
*! Example            :  =lfGetMaskC(@length,@position)
*!*************************************************************
FUNCTION lfGetMaskC 
PARAMETERS lnLen,lnPos
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnLen = LEN(laItemSeg[lnCount,3])
    lnPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*---END OF FUNCTION lfGetMaskD

*!*************************************************************
*! Name      : lfStySum  (C# 102209)
*! Developer : AHMED MOHAMED ELANWAR (AME)
*! Date      : 04/05/2001
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
*C102209,4
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.

