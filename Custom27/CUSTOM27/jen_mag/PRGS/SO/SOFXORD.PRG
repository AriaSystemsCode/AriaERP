*:************************************************************
*: Program file  : SOFXORDH.PRG
*: Program desc. : Fix data in ORDHDR.DBF file 
*                  replace  Status field with "X" for Orders with status = 'C' and Qty = 0 and Amnt = 0 
*:        System : Aria Advantage Series.
*:        Module : SALES ORDERS
*:     Developer : tarek Mohammed Ibrahim TMI
*:     Entry     : B606088,1
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO SOFXORDH.PRG
*:************************************************************


lnRespns = gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,;
                      'This program will update status in Ordhdr file to "X" for orders with no lines , Qty=0 and Amount=0 ?')

IF lnRespns = 1
  *--Open needed files
  =gfOpenFile(gcDataDir+'ORDHDR','','SH')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
  SELECT ORDHDR
  SET RELATION TO CORDTYPE+ORDER INTO ORDLINE

  GO TOP
  SCAN FOR STATUS='O' ;
           .AND. BOOK+BOOKAMT+CANCEL+CANCELAMT+SHIP+SHIPAMT+OPEN+OPENAMT=0 ;
           .AND. EOF('ORDLINE')
    WAIT WINDOW 'Updating Order '+ORDHDR.ORDER NOWAIT
    REPLACE STATUS WITH 'X'
  ENDSCAN

  *--CLOSE Opened files
  USE IN ORDHDR
  USE IN ORDLINE
  
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,;
                      "Ordhdr File Updated" )
ENDIF                      