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

 STORE SPACE(0) TO LcSalAcnt
 SELECT INVHDR 

 IF SEEK ('007596')
   SCAN REST WHILE INVOICE <> "009515" 
     WAIT WINDOW 'Restoring invoice lines for invoice No # '+INVHDR.INVOICE NOWAIT
     IF INVHDR.STATUS='C' 
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
       
       
       IF SEEK('O'+INVHDR.ORDER,'ORDLINE')
         SELECT ORDLINE
         SCAN REST WHILE ORDER = INVHDR.ORDER
           IF SEEK ( ORDLINE.STYLE +INVHDR.INVOICE + STR(ORDLINE.lineno,6),'INVLINE')
             *SCATTER MEMVAR MEMO
             =SEEK(ORDLINE.STYLE,"STYLE")
             =SEEK('M'+ORDLINE.ACCOUNT,"CUSTOMER")
             SELECT INVLINE
             REPLACE ACCOUNT WITH INVHDR.ACCOUNT;
                     ALTCOLOR WITH ORDLINE.ALTCOLOR;
                     ALTSTYLE WITH ORDLINE.ALTSTYLE;
                     DESC1    WITH ORDLINE.DESC1 ;
                     FLAG     WITH ORDLINE.FLAG;
                     DYeLOT    WITH ORDLINE.DYeLOT;
                     GL_COST  WITH ORDLINE.GL_COST;
                     GL_SALES  WITH ORDLINE.GL_SALES;
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
                     GL_COST    WITH "DEFDEF";
                     GL_SALES   WITH ALLTRIM(CUSTOMER.CSlsGLLink)+ALLTRIM(Style.CSlsGLLink);
                     CICACNT    WITH "036-000-000             ";
                     nEqvamnt   WITH (Invline.Price * TOTQTY)/InvHdr.nEXrate
                     
                     IF ! SEEK(GL_SALES , 'GL_LINK')
                       LcSalAcnt= 'DEFDEF'
                     ELSE                     
                       LcSalAcnt=GL_SALES
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
                     *Invoice    WITH INVHDR.INVOICE;
                     *InvDate    WITH INVHDR.INVDATE
             *M.Qty1=invline.Qty1
             *M.Qty2=invline.Qty2
             *M.Qty3=invline.Qty3
             *M.Qty4=invline.Qty4
             *M.Qty5=invline.Qty5
             *M.Qty6=invline.Qty6
             *M.Qty7=invline.Qty7
             *M.Qty8=invline.Qty8
             *m.TOTQTY=invline.TOTQty
             *SELECT INVLINE
             *GATHER MEMVAR MEMO             
           ENDIF
         ENDSCAN
       ENDIF
     ENDIF
   ENDSCAN
    WAIT WINDOW INVHDR.INVOICE NOWAIT
   
 ELSE
 ENDIF 