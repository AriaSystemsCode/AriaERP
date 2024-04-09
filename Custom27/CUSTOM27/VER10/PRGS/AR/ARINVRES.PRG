 lcDir= GETDIR('','Select DBfs Directory :')  
 IF FILE(lcDir+'\InvHdr.dbf')
   =lpOpenFile()
   =lfScan()
   CLOSE ALL
 ELSE
   IF EMPTY(lcDir)
      
   ELSE 
     WAIT WINDOW "Wrong Data Directory"
   ENDIF
  
 ENDIF
 
 FUNCTION lpOpenFile
  CLOSE ALL 
  USE lcDir+'\InvHdr.dbf' in 0 ORDER INVHDR
  USE lcDir+'\InvLine.dbf'in 0 ORDER INVLINEs 
  USE lcDir+'\OrdLine.dbf'in 0 ORDER ORDLINE
  USE lcDir+'\STYINVJL.dbf'in 0 ORDER mfgopr
  USE LcDir+'\Customer.dbf'in 0 ORDER 1
  USE LcDir+'\Style.dbf' in 0 ORDER 1
  USE LcDir+'\GL_LINK.dbf' in 0 ORDER 1
 
 FUNCTION lfScan

 STORE SPACE(0) TO LcSalAcnt,lcICAcnt
 
 SELECT INVHDR  
 SCAN     
   WAIT WINDOW 'Restoring invoice lines for invoice No # '+INVHDR.INVOICE NOWAIT
   IF INVHDR.STATUS='C'
    
     IF INVHDR.SHIPAMT =0
       
       REPLACE SHIPAMT WITH TOTALCHG - Discount - Tax_AMT- COD - Freight -Insur - COD - nPstAmt-nHstAmt
     ENDIF 
     
     IF SEEK(InvHdr.invoice,'StyINVJL')
       SELECT STYINVJL
       SCAN REST WHILE CTRCODE =InvHdr.invoice
         IF CIRTYPE="I" AND CTRTYPE="3"  
           IF ! SEEK(STYINVJL.Style+ STYINVJL.Ctrcode+STR(STYINVJL.lineno,6),"INVLINE")
             SCATTER MEMVAR MEMO
             M.Invoice=INVHDR.INVOICE
             M.InvDate=INVHDR.INVDATE
             M.Qty1=ABS(StyINVJL.nstk1)
             M.Qty2=ABS(StyINVJL.nstk2)
             M.Qty3=ABS(StyINVJL.nstk3)
             M.Qty4=ABS(StyINVJL.nstk4)
             M.Qty5=ABS(StyINVJL.nstk5)
             M.Qty6=ABS(StyINVJL.nstk6)
             M.Qty7=ABS(StyINVJL.nstk7)
             M.Qty8=ABS(StyINVJL.nstk8)
             m.TOTQTY=ABS(StyINVJL.ntotstk)
             m.Cost=StyInvJl.NCOST
             SELECT INVLINE
             APPEND BLANK
             GATHER MEMVAR MEMO
           ENDIF
         ENDIF                         
       ENDSCAN
     ENDIF
       
     lnrec=0  
     IF SEEK('O'+INVHDR.ORDER,'ORDLINE')
       SELECT ORDLINE
       SCAN REST WHILE ORDER = INVHDR.ORDER
         lnRec=RECNO('INVLINE')
         IF SEEK ( ORDLINE.STYLE +INVHDR.INVOICE + STR(ORDLINE.lineno,6),'INVLINE')
           *SCATTER MEMVAR MEMO
           =SEEK(ORDLINE.STYLE,"STYLE")
           =SEEK('M'+ORDLINE.ACCOUNT,"CUSTOMER")
           SELECT INVLINE
           REPLACE ACCOUNT WITH INVHDR.ACCOUNT;
                   ALTCOLOR   WITH ORDLINE.ALTCOLOR;
                   ALTSTYLE   WITH ORDLINE.ALTSTYLE;
                   DESC1      WITH ORDLINE.DESC1 ;
                   FLAG       WITH ORDLINE.FLAG;
                   DYeLOT     WITH ORDLINE.DYeLOT;
                   GL_COST    WITH ORDLINE.GL_COST;
                   GL_SALES   WITH ORDLINE.GL_SALES;
                   GROUP      WITH ORDLINE.GROUP;
                   ORDER      WITH ORDLINE.ORDER;
                   PACK_ID    WITH ORDLINE.PACK_ID;
                   PREPAK     WITH ORDLINE.PREPAK;
                   SCALE      WITH ORDLINE.SCALE;
                   SEASON     WITH ORDLINE.SEASON;
                   STORE      WITH ORDLINE.STORE;
                   NOTE_MEM   WITH ORDLINE.NOTE_MEM;
                   COMM1      WITH ORDLINE.COMM1;
                   COMM2      WITH ORDLINE.COMM2;
                   DISC_PCNT  WITH ORDLINE.DISC_PCNT;
                   GROS_PRICE WITH ORDLINE.GROS_PRICE;
                   PPQTY      WITH ORDLINE.PPQTY;
                   PRICE      WITH ORDLINE.PRICE;
                   SHIPMENT   WITH ORDLINE.SHIPMENTS;
                   GL_SALES   WITH ALLTRIM(CUSTOMER.CSlsGLLink)+ALLTRIM(Style.CSlsGLLink);
                   nEqvamnt   WITH (Invline.Price * TOTQTY)/InvHdr.nEXrate
                    
                   IF ! SEEK(GL_SALES , 'GL_LINK')
                     LcSalAcnt= 'DEFDEF'
                   ELSE                     
                     LcSalAcnt=GL_SALES
                   ENDIF
                
                   IF ! SEEK(GL_COST,'GL_LINK')
                     lcICAcnt='DEFDEF'
                   ELSE                                  
                     lcICAcnt=GL_COST
                   ENDIF
                  
                  IF SEEK (lcICAcnt+"006" ,'GL_LINK')
                    REPLACE CICACNT WITH GL_LINK.GLACNT
                  ENDIF
                  
                  IF SEEK (LcSalAcnt+"003" ,'GL_LINK')
                    REPLACE CSALESACNT WITH GL_LINK.GLACNT
                  ENDIF
                  
                  IF SEEK (LcSalAcnt+"008",'GL_LINK')
                    REPLACE CCOGSACNT WITH GL_LINK.GLACNT
                  ENDIF
                  
                  IF SEEK (LcSalAcnt+"005",'GL_LINK')
                    REPLACE CDiscacnt WITH GL_LINK.GLACNT
                  ENDIF
         ENDIF
       ENDSCAN
     ENDIF
   ENDIF
 ENDSCAN
 WAIT WINDOW INVHDR.INVOICE NOWAIT
   

 SELECT INVLINE
         
 LOCATE 
 SCAN 
   WAIT WINDOW 'Restoring invoice lines for invoice No # '+INVLINE.INVOICE NOWAIT 
   IF EMPTY(account)
     
     lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')                        
     SELECT INVLINE
     =SEEK (INVLINE.INVOICE,'INVHDR')
     REPLACE PRICE WITH  lfGetprice (Invline.Style,lcPriceLvl,invline.totqty);
             GROS_PRICE WITH  lfGetprice (Invline.Style,lcPriceLvl,invline.totqty);
             ACCOUNT    WITH  InvHdr.ACCOUNT;
             STORE      WITH  InvHdr.Store;
             ORDER      WITH  INVHDR.ORDER ;  
             SEASON     WITH  sTYLE.SEASON;
             DESC1      WITH  sTYLE.DESC1;
             SCALE      WITH  STYLE.SCALE;
             SEASON     WITH  sTYLE.SEASON;
             nEqvamnt   WITH (Invline.Price * TOTQTY)
             
     =SEEK ('M'+InvLine.Account ,'CUSTOMER')
     REPLACE GL_SALES   WITH ALLTRIM(CUSTOMER.CSlsGLLink)+ALLTRIM(Style.CSlsGLLink)          
             IF ! SEEK(GL_SALES , 'GL_LINK')
               LcSalAcnt= 'DEFDEF'
             ELSE                     
               LcSalAcnt=GL_SALES
             ENDIF
              
            IF ! SEEK(GL_COST,'GL_LINK')
              lcICAcnt='DEFDEF'
            ELSE                                  
              lcICAcnt=GL_COST
            ENDIF
                  
            IF SEEK (lcICAcnt+"006" ,'GL_LINK')
              REPLACE CICACNT WITH GL_LINK.GLACNT
            ENDIF
                  
            IF SEEK (LcSalAcnt+"003" ,'GL_LINK')
              REPLACE CSALESACNT WITH GL_LINK.GLACNT
            ENDIF
                  
            IF SEEK (LcSalAcnt+"008",'GL_LINK')
              REPLACE CCOGSACNT WITH GL_LINK.GLACNT
            ENDIF
                  
            IF SEEK (LcSalAcnt+"005",'GL_LINK')
              REPLACE CDiscacnt WITH GL_LINK.GLACNT
            ENDIF
   ENDIF
 ENDSCAN

 WAIT CLEAR                  
 
 
FUNCTION lfGetprice
PARAMETERS lcStyle,lcLevel,lnQuantity
PRIVATE lnPrice

=SEEK(lcStyle,'Style')
IF lcLevel = 'Q'
  DO CASE
    CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
      lcLevel = 'C'
    CASE Style.nAtQtyB > 0 AND lnQuantity >Style.nAtQtyB
      lcLevel = 'B'
    OTHERWISE
      lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(lcLevel,'A','B','C'),lcLevel,'A')
ENDIF
lnPrice = Style.Price&lcLevel
                    
*-- if price= 0 then get the price from the user                      
RETURN(lnPrice)
 
 