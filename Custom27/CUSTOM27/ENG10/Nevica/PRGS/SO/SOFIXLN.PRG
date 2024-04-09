*:****************************************************************
*: Program file  : SOFIXLn.PRG
*: Program desc. : Fix program For Nevica.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order (SO)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 04/17/2003
*: Tracking Job Number : B#607005,1
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  :  None.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200451,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:

*- Message Text   :- Process complete successfully.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF gfModalGen('QRM00000B32000','F','ALERT',' ','Are you sure, you want to run the fix program now ?') = 2
  RETURN
ENDIF

lcFilHandl = ''
llOpenRep = .F.
DIMENSION laOpenFile[9,3]
*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .
STORE '' To lcDelLntmp , lcTmpOrdln , lcOrdhdNln , lcTmpInvln

*-- Open needed files
= lfOpenFls ()

*-- get all record that didn't have any detail at ordline, or invline, or
*-- Conslidated invoice or cancel order.
SELECT * FROM ORDHDR ;
  WHERE !SEEK(Ordhdr.cordtype+Ordhdr.order,'Ordline') ;
  .AND. !SEEK(Ordhdr.cordtype+Ordhdr.order,'ORDCANLN') ;
  .AND. !SEEK(Ordhdr.order,'Invline')  ;
  .AND. !SEEK(Ordhdr.order,'CONSINVL') ;
  INTO TABLE (lcDelLntmp)

lCancreson = IIF(SEEK('D'+'N'+'CCANCRESON','CODES'),CODES.cCode_No,'000000')

*-- Create temp File.
= lfCrtTemp ()

*-- Delete the header record that didn't have any lines.
SELECT (lcDelLntmp)
LOCATE
SCAN
  IF SEEK(cordtype+order,'ORDHDR')
    SELECT ORDHDR
    WAIT WINDOW 'Please wait ... delete order # : '+ Order noWait
    DELETE
  ENDIF
  SELECT (lcDelLntmp)
ENDSCAN

*-- Function to get the recored from consInl file and update invline.
= lfupdinvln ()


llComplete = .F.

SELECT ORDHDR 
SCAN
  llComplete = (Ordhdr.Status = 'C')
  WAIT WINDOW 'Please wait ... verify order # :' + Order NOWAIT
  SELECT (lcTmpOrdln)
  DELETE ALL
  STORE 0 TO lnTotOpen , lnTotShip , lnTotCancl ,;
             lnOpenAmt , lnShipAmt , lnCanlAmt  ,;
             lnCanlQty , lnCanAmt  , lnBookAmt , lnTotBook 
             
  SELECT ORDHDR
  *-- Open  
  IF SEEK(Ordhdr.cordtype+Ordhdr.order,'ORDLINE')
    SELECT ORDLINE 
    SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) =  Ordhdr.cordtype+Ordhdr.order
      lnTotBook = lnTotBook + TotBook
      lnBookAmt = lnBookAmt + (TotBook * Price)
      
      lnTotOpen = lnTotOpen + TotQty
      lnOpenAmt = lnOpenAmt + (TotQty * Price)
      
      IF !SEEK(order+STR(lineno,6),'INVLINE') .AND. !SEEK(order+STR(lineno,6),'CONSINVL'),;
        .AND. !SEEK(cordtype+order+STR(lineno,6),'ORDCANLN')
        SELECT ORDLINE 
        SCATTER MEMVAR MEMO
        INSERT INTO (lcTmpOrdln) FROM MEMVAR
        
        lnCanlQty = lnCanlQty + M.TotBook 
        lnCanAmt  = lnCanAmt  + (M.TotBook * M.Price)
      ENDIF

      DO CASE 
        CASE SEEK(order+STR(lineno,6),'INVLINE')
          SELECT ORDLINE
          SCATTER MEMVAR MEMO
          SELECT INVLINE
           
          STORE 0 TO M.Book1,M.Book2,M.Book3,M.Book4,M.Book5,M.Book6,M.Book7,M.Book8,;
                     M.TotBook , lnTmpCanlQ , lnTmpCanla
          DIME laShip[1,9]
          laShip = 0 
          SCAN REST WHILE order+STR(lineno,6)+invoice = M.order+STR(M.lineno,6)
            =SEEK(Invoice,'INVHDR') 
            lcSign = IIF (INVHDR.Status = 'V',' + ',' - ')
            For I = 1 To 8
              Sz = STR(I,1)
              M.Book&SZ = M.Book&SZ &lcSign INVLINE.Qty&Sz
              laShip[1,I] = laShip[1,I] + INVLINE.Qty&Sz
            ENDFOR
              M.TotBook = M.TotBook &lcSign INVLINE.TotQty
              laShip[1,9] = laShip[1,9] + INVLINE.TotQty
              lnTmpCanlQ = lnTmpCanlQ + M.TotBook 
              lnTmpCanlA = lnTmpCanlA + (M.TotBook * M.Price)
           ENDSCAN
           
           IF M.TotBook > 0 .OR. ABS(M.TotBook) # Ordline.TotBook
             IF M.TotBook < 0 .AND. InvHdr.Status # 'V'
               For I = 1 To 8
                 Sz = STR(I,1)
                 M.Book&SZ = OrdLine.Book&SZ &lcSign laShip[1,I]
               ENDFOR
               M.TotBook = Ordline.TotBook &lcSign laShip[1,9]
               lnCanlQty = lnCanlQty + M.TotBook
               lnCanAmt  = lnCanAmt + (M.TotBook * M.Price)
             ELSE
               INSERT INTO (lcTmpOrdln) FROM MEMVAR
               lnCanlQty = lnCanlQty + lnTmpCanlQ
               lnCanAmt  = lnCanAmt + lnTmpCanlA
             ENDIF
           ENDIF
            
        CASE SEEK(order+STR(lineno,6),'CONSINVL')
           SCATTER MEMVAR MEMO
           SELECT CONSINVL
           
           STORE 0 TO M.Book1,M.Book2,M.Book3,M.Book4,M.Book5,M.Book6,M.Book7,M.Book8,;
                      M.TotBook , lnTmpCanlQ , lnTmpCanla
           DIME laShip[1,9]
           laShip = 0
           SCAN REST WHILE order+STR(lineno,6)+invoice = M.order+STR(M.lineno,6)
             =SEEK(Invoice,'CONSINVH') 
             lcSign = IIF (CONSINVH.Status = 'V',' + ',' - ')
             For I = 1 To 8
               Sz = STR(I,1)
               M.Book&SZ = M.Book&SZ &lcSign CONSINVL.Qty&Sz
               laShip[1,I] = laShip[1,I] + CONSINVL.Qty&Sz
             ENDFOR
             M.TotBook = M.TotBook &lcSign CONSINVL.TotQty
             laShip[1,9] = laShip[1,9] + CONSINVL.TotQty
             lnTmpCanlQ = lnTmpCanlQ + M.TotBook 
             lnTmpCanlA = lnTmpCanlA + (M.TotBook * M.Price)
           ENDSCAN
           
           IF M.TotBook > 0 .OR. ABS(M.TotBook) # Ordline.TotBook
             IF M.TotBook < 0 .AND. CONSINVH.Status # 'V'
               For I = 1 To 8
                 Sz = STR(I,1)
                 M.Book&SZ = OrdLine.Book&SZ &lcSign laShip[1,I]
               ENDFOR
               M.TotBook = Ordline.TotBook &lcSign laShip[1,9]
               lnCanlQty = lnCanlQty + M.TotBook
               lnCanAmt  = lnCanAmt + (M.TotBook * M.Price)
             ELSE
               INSERT INTO (lcTmpOrdln) FROM MEMVAR
               lnCanlQty = lnCanlQty + lnTmpCanlQ
               lnCanAmt  = lnCanAmt + lnTmpCanlA
             ENDIF
           ENDIF
         ENDCASE
      SELECT ORDLINE 
    ENDSCAN
  ENDIF
  
  *-- Ship
  IF SEEK(Ordhdr.order,'INVLINE')
    SELECT INVLINE
    SCAN REST WHILE order+STR(lineno,6)+invoice =  Ordhdr.order
      IF SEEK(Invoice,'INVHDR') .AND. INVHDR.Status # 'V'
        lnTotShip = lnTotShip + TotQty
        lnShipAmt = lnShipAmt + (TotQty * Price)
      ENDIF
    ENDSCAN
  ENDIF

  *- Ship From Cons.
  IF SEEK(Ordhdr.order,'CONSINVL')
    SELECT CONSINVL
    SCAN REST WHILE order+STR(lineno,6)+invoice =  Ordhdr.order
      IF !SEEK(order+STR(lineno,6)+invoice,'INVLINE') .AND. SEEK(Invoice,'CONSINVH') .AND. CONSINVH.Status # 'V'
        lnTotShip = lnTotShip + TotQty
        lnShipAmt = lnShipAmt + (TotQty * Price)
      ENDIF
    ENDSCAN
  ENDIF


  *-- cordtype+order+STR(lineno,6)
  IF SEEK(Ordhdr.cordtype+Ordhdr.order,'ORDCANLN')
    SELECT ORDCANLN
    SCAN REST WHILE cordtype+order+STR(lineno,6) =  Ordhdr.cordtype+Ordhdr.order
      lnTotCancl = lnTotCancl + TotQty
      lnCanlAmt = lnCanlAmt   + (TotQty * Price)
    ENDSCAN
  ENDIF
  
  llOrdHdr = (Ordhdr.Book = Ordhdr.Open + Ordhdr.Cancel + Ordhdr.Ship)
  llCount  = !(Ordhdr.Book = lnTotOpen + lnTotShip + lnTotCancl )
  IF llOrdHdr .AND. llCount
    SELECT ORDHDR
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdhdNln) FROM MEMVAR
    
    IF OrdHdr.Status = 'C'
        REPLACE Book      WITH lnTotBook ,;
                BookAmt   WITH lnBookAmt ,;
                Open      WITH 0         ,;
                OpenAmt   WITH 0.0       ,;
                Cancel    WITH lnTotCancl+ lnCanlQty,;
                Cancelamt WITH lnCanlAmt + lnCanAmt
         
         
         IF lnTotOpen + lnTotShip + lnTotCancl = 0
           IF SEEK(Ordhdr.cordtype+Ordhdr.order,'Ordline')
             SELECT ORDLINE
             SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) =  Ordhdr.cordtype+Ordhdr.order
               REPLACE Qty1 WITH Book1,;
                       QTY2 WITH Book2,;
                       QTY3 WITH Book3,;
                       QTY4 WITH Book4,;
                       QTY5 WITH Book5,;
                       QTY6 WITH Book6,;
                       QTY7 WITH Book7,;
                       QTY8 WITH Book8,;
                       TotQty With TotBook

             ENDSCAN
           ENDIF  
           SELECT ORDHDR
           REPLACE STATUS WITH 'X'
           
           lcSendErr = Ordhdr.Order + ' Change status to Cancel and update the order line file with booked Qty.'
           = lfOrdError (lcSendErr)
         ENDIF
    ENDIF
    
  ELSE
    IF llCount
      *-- Update header file.
      SELECT ORDHDR
      REPLACE Ship    WITH lnTotShip,;
              ShipAmt WITH lnShipAmt
       
      DO CASE
      CASE OrdHdr.Status = 'C'
        REPLACE Book      WITH lnTotBook ,;
                BookAmt   WITH lnBookAmt ,;
                Open      WITH 0         ,;
                OpenAmt   WITH 0.0       ,;
                Cancel    WITH lnTotCancl+ lnCanlQty,;
                Cancelamt WITH lnCanlAmt + lnCanAmt
         
         
         IF lnTotOpen + lnTotShip + lnTotCancl+ lnCanlQty = 0
           IF SEEK(Ordhdr.cordtype+Ordhdr.order,'Ordline')
             SELECT ORDLINE
             SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) =  Ordhdr.cordtype+Ordhdr.order
               REPLACE Qty1 WITH Book1,;
                       QTY2 WITH Book2,;
                       QTY3 WITH Book3,;
                       QTY4 WITH Book4,;
                       QTY5 WITH Book5,;
                       QTY6 WITH Book6,;
                       QTY7 WITH Book7,;
                       QTY8 WITH Book8,;
                       TotQty With TotBook

             ENDSCAN
           ENDIF  
           SELECT ORDHDR
           REPLACE STATUS WITH 'X'
           lcSendErr = Ordhdr.Order + ' Change status to Cancel and update the order line file with booked Qty.'
           = lfOrdError (lcSendErr)
         ENDIF
         
         IF lnTotOpen > 0
           IF SEEK(Ordhdr.cordtype+Ordhdr.order,'Ordline')
             SELECT ORDLINE
             SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) =  Ordhdr.cordtype+Ordhdr.order
               REPLACE Qty1 WITH Book1,;
                       QTY2 WITH Book2,;
                       QTY3 WITH Book3,;
                       QTY4 WITH Book4,;
                       QTY5 WITH Book5,;
                       QTY6 WITH Book6,;
                       QTY7 WITH Book7,;
                       QTY8 WITH Book8,;
                       TotQty With TotBook

             ENDSCAN
           ENDIF  
           SELECT ORDHDR
         ENDIF
         
         IF lnTotBook = lnTotCancl+ lnCanlQty
           REPLACE STATUS WITH 'X'
         ENDIF
         
      CASE OrdHdr.Status = 'X' .AND. Ordhdr.Ship # 0
        REPLACE Book      WITH lnTotBook,;
                BookAmt   WITH lnBookAmt,;
                Open      WITH lnTotOpen ,;
                OpenAmt   WITH lnOpenAmt ,;
                Cancel    WITH lnTotCancl + lnCanlQty,;
                Cancelamt WITH lnCanlAmt + lnCanAmt

         REPLACE OrdHdr.Status WITH  'C' 
         lcSendErr = Ordhdr.Order + ' Update the order header file with the booked, cancel, and open.'
         = lfOrdError (lcSendErr)
      OTHERWISE
        REPLACE Book      WITH lnTotBook ,;
                BookAmt   WITH lnBookAmt ,;
                Open      WITH lnTotOpen ,;
                OpenAmt   WITH lnOpenAmt ,;
                Cancel    WITH lnTotCancl + lnCanlQty,;
                Cancelamt WITH lnCanlAmt + lnCanAmt
           lcSendErr = Ordhdr.Order + ' Update the order header file with the booked, cancel, and open.'
           = lfOrdError (lcSendErr)
      ENDCASE
    
      *-- Update the cancel Qty.
      IF OrdHdr.Status = 'X' .AND. Ordhdr.Ship = 0
        IF SEEK(Ordhdr.cordtype+Ordhdr.order,'Ordline')
          SELECT ORDLINE
          SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) =  Ordhdr.cordtype+Ordhdr.order
            
            REPLACE Qty1 WITH Book1,;
                    QTY2 WITH Book2,;
                    QTY3 WITH Book3,;
                    QTY4 WITH Book4,;
                    QTY5 WITH Book5,;
                    QTY6 WITH Book6,;
                    QTY7 WITH Book7,;
                    QTY8 WITH Book8,;
                    TotQty With TotBook

          ENDSCAN
          SELECT ORDHDR
          lcSendErr = Ordhdr.Order + ' Update the order header file with the booked, cancel, and open.'
          = lfOrdError (lcSendErr)
        ENDIF
      ELSE
        = lfUpdatCan()
      ENDIF
    ENDIF
  ENDIF
  
  
  SELECT ORDHDR
ENDSCAN


*-- Close opened files 
= lfClosFls ()

*- Message Text   :- Process complete successfully.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
= gfModalGen('INM00000B00000','F','ALERT',' ','Process complete successfully, and fix log file [Orders.txt] saved at work directory.')

RETURN
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 04/17/2003
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .

laOpenFile[1,1] = 'ORDHDR'
laOpenFile[1,2] = 'ORDHDR'

laOpenFile[2,1] = 'ORDLINE'
laOpenFile[2,2] = 'ORDLINE'


laOpenFile[3,1] = 'INVLINE'
laOpenFile[3,2] = 'INVLINEO'

laOpenFile[4,1] = 'ORDHDR'
laOpenFile[4,2] = 'ORDHDR'

laOpenFile[5,1] = 'CONSINVL'
laOpenFile[5,2] = 'CINVLINE'

laOpenFile[6,1] = 'ORDCANLN'
laOpenFile[6,2] = 'ORDCANLN'

laOpenFile[7,1] = 'CODES'
laOpenFile[7,2] = 'IDRLTFNAME'

laOpenFile[8,1] = 'CONSINVH'
laOpenFile[8,2] = 'CONSINVH'

laOpenFile[9,1] = 'INVHDR'
laOpenFile[9,2] = 'INVHDR'




FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR

lcDelLntmp = gftempName()
lcTmpOrdln = gftempName()
lcOrdhdNln = gftempName()
lcTmpInvln = gftempName()


*-- End OF lfOpenFls.
*-- End of lfOpenFiles.
*:*************************************************************
*: Name      : lfClosFls
*: Developer : Abdou Elgendy
*: Date      : 04/17/2003
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfClosFls ()
*:*************************************************************
*:
FUNCTION lfClosFls

FOR I = 1 To ALEN(laOpenFile,1)
  IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
    = gfCloseFile(laOpenFile[I,1])
  ENDIF
ENDFOR

IF USED(lcDelLntmp)
  USE IN (lcDelLntmp)
  ERASE (gcWorkDir+lcDelLntmp+".DBF")
  ERASE (gcWorkDir+lcDelLntmp+".CDX")
ENDIF

IF USED(lcTmpOrdln)
  USE IN (lcTmpOrdln)
  ERASE (gcWorkDir+lcTmpOrdln+".DBF")
  ERASE (gcWorkDir+lcTmpOrdln+".CDX")
ENDIF

IF USED(lcOrdhdNln)
  USE IN (lcOrdhdNln)
  ERASE (gcWorkDir+lcOrdhdNln+".DBF")
  ERASE (gcWorkDir+lcOrdhdNln+".CDX")
ENDIF

IF USED(lcTmpInvln)
  USE IN (lcTmpInvln)
  ERASE (gcWorkDir+lcTmpInvln+".DBF")
  ERASE (gcWorkDir+lcTmpInvln+".CDX")
ENDIF

= FCLOSE(lcFilHandl)

*-- End of lfClosFls
*:*************************************************************
*: Name      : lfUpdatCan
*: Developer : Abdou Elgendy
*: Date      : 04/17/2003
*: Purpose   : Function to Update cancel file with open Qty.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfUpdatCan ()
*:*************************************************************
*:
FUNCTION lfUpdatCan
PRIVATE lnPrvAlis

lnPrvAlis = SELECT (0)
SELECT (lcTmpOrdln)
SCAN
  SCATTER MEMVAR MEMO
  *-- cordtype+order+STR(lineno,6)
  SELECT ORDCANLN
  IF SEEK(M.cordtype+M.order+STR(M.lineno,6))
    DELETE
  ENDIF  
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE Qty1   WITH Qty1+ABS(M.Book1) ,;
          Qty2   WITH Qty2+ABS(M.Book2) ,;
          Qty3   WITH Qty3+ABS(M.Book3) ,;
          Qty4   WITH Qty4+ABS(M.Book4) ,;
          Qty5   WITH Qty5+ABS(M.Book5) ,;
          Qty6   WITH Qty6+ABS(M.Book6) ,;
          Qty7   WITH Qty7+ABS(M.BooK7) ,;
          Qty8   WITH Qty8+ABS(M.Book8) ,;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
          Cancelled  WITH gdSysdate,;
          cCancreson WITH lCancreson
  
  SELECT ORDLINE
  IF SEEK(M.cordtype+M.order+STR(M.lineno,6))
    REPLACE Qty1   WITH 0,;
            Qty2   WITH 0,;
            Qty3   WITH 0,;
            Qty4   WITH 0,;
            Qty5   WITH 0,;
            Qty6   WITH 0,;
            Qty7   WITH 0,;
            Qty8   WITH 0,;
            TotQty WITH 0
  ENDIF
  SELECT (lcTmpOrdln)
ENDSCAN

IF OrdHdr.Status = 'X'
  REPLACE Ordhdr.Status WITH 'C'
  lcSendErr = Ordhdr.Order + ' Change status to Complete and add cancelled  quantities.'
  = lfOrdError (lcSendErr)
ELSE
  lcSendErr = Ordhdr.Order + ' Add cancelled  quantities in order cancellation file.'
  = lfOrdError (lcSendErr)
ENDIF
SELECT (lnPrvAlis)    

*-- End OF lfUpdatCan
*:*************************************************************
*: Name      : lfupdinvln
*: Developer : Abdou Elgendy
*: Date      : 04/17/2003
*: Purpose   : Function to Update invline with missed data.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfupdinvln ()
*:*************************************************************
*:
FUNCTION lfupdinvln
PRIVATE lnAlias , lcOldOrder

lnAlias  = SELECT (0)
SELECT Consinvl
lcOldOrder = ORDER()
SET ORDER TO Consinvl

SELECT INVLINE
lcTmpOrder = ORDER()
SET ORDER TO Invlines

SELECT ConsInvH
SCAN
  WAIT WINDOW 'Please wait ... update invoice # :' + Invoice NOWAIT 
  *-- check for the cureent invoice.
  IF SEEK(ConsInvH.Invoice,'Consinvl')
    SELECT Consinvl
    SCAN REST WHILE invoice+store+order+style+STR(lineno,6) = ConsInvH.Invoice ;
      FOR Order = ConsInvH.Order
      SCATTER MEMVAR MEMO
      IF SEEK(M.Style+M.Order,lcTmpInvln)
        SELECT (lcTmpInvln)
        REPLACE QTy1       WITH QTy1 + M.QTy1 ,;
                QTy2       WITH QTy2 + M.QTy2 ,;
                QTy3       WITH QTy3 + M.QTy3 ,;
                QTy4       WITH QTy4 + M.QTy4 ,;
                QTy5       WITH QTy5 + M.QTy5 ,;
                QTy6       WITH QTy6 + M.QTy6 ,;
                QTy7       WITH QTy7 + M.QTy7 ,;
                QTy8       WITH QTy8 + M.QTy8 ,;
                TotQty     WITH TotQty + M.TotQty
        SELECT ConsInvH
      ELSE
        SELECT (lcTmpInvln)
        APPEN BLANK
        GATHER MEMVAR MEMO
        SELECT Consinvl
        lnRecNo = RECNO()
        SEEK M.Invoice
        AVERAGE Price While Invoice = M.Invoice FOR Style = M.Style To lnPrice
        IF BETWEEN(lnRecNo,1,RECCOUNT())
          GOTO lnRecNo
        ENDIF
        SELECT (lcTmpInvln)
        REPLACE Price      With lnPrice ,;
                Gros_price WitH lnPrice ,;
                Neqvamnt   WITH (TotQty * Price)
      ENDIF
    ENDSCAN
    
    *-- Update the invoice line with the correct consol Qty.    
    SELECT (lcTmpInvln)
    SCAN
      SCATTER MEMVAR MEMO
      IF !SEEK(M.style+M.invoice,'INVLINE')
        INSERT INTO INVLINE FROM MEMVAR
      ENDIF
    ENDSCAN
    
    SELECT (lcTmpInvln)
    DELETE ALL
    
    SELECT ConsInvH
  ENDIF
ENDSCAN

SELECT INVLINE
SET ORDER TO &lcTmpOrder

SELECT Consinvl
SET ORDER TO &lcOldOrder

SELECT (lnAlias)
*-- End OF lfupdinvln
*:*************************************************************
*: Name      : lfCrtTemp
*: Developer : Abdou Elgendy
*: Date      : 04/17/2003
*: Purpose   : Function to Create temp files
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfupdinvln ()
*:*************************************************************
*:
FUNCTION lfCrtTemp

SELECT INVLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DECLARE laIndex[1,2]

laIndex[1,1] = 'Style+Order'
laIndex[1,2] = 'STYLE'
=gfCrtTmp(lcTmpInvln,@laFileStru,@laIndex)


SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DECLARE laIndex[1,2]

laIndex[1,1] = 'cordtype+order'
laIndex[1,2] = 'Ordhdr'
=gfCrtTmp(lcOrdhdNln,@laFileStru,@laIndex)


SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DECLARE laIndex[1,2]

laIndex[1,1] = 'cordtype+order'
laIndex[1,2] = 'Ordline'
=gfCrtTmp(lcTmpordln,@laFileStru,@laIndex)

*-- End OF lfCrtTemp.
*:*************************************************************
FUNCTION lfOrdError 
PARAMETER lcErr_Mesage

IF !llOpenRep
  llOpenRep = .T.
  lcFilHandl = FCREAT('&gcWorkdir.Orders.txt')
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,"*                FILE VERIFICATION ORDER STATUS REPORT               *")
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,' ')
  =FPUTS(lcFilHandl,' ')
ENDIF

=FPUTS(lcFilHandl,lcErr_Mesage)

*-- End OF lfOrdError
*:*************************************************************