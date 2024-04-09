**:***********************************************************************
*:  Program file : ARADJDS.PRG
*:  Program desc.: Adjust Dropship Invoice Consolidated Invoice#
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 10/26/2021
*:      Reference: C202430,1
*:************************************************************************
=gfOpenTable('SMART_INVOICE_HEADER','SMINVHDR')
=gfOpenTable('SMART_INVOICE_LINES','SMINVLINE')
=gfOpenTable('INVHDR','INVHDR')
SELECT SMART_INVOICE_HEADER
=gfSeek('')
SCAN FOR  !Empty(Invoice) 
  WAIT WINDOW 'Checking invoice# '+SMART_INVOICE_HEADER.SMART_INVOICE_NO NOWAIT 
  IF gfSeek(SMART_INVOICE_HEADER.Invoice,'INVHDR','INVHDR')
    IF INVHDR.STATUS ='V'
      SELECT SMART_INVOICE_LINES
      =gfSeek(SMART_INVOICE_HEADER.SMART_INVOICE_NO)
      SCAN REST WHILE SMART_INVOICE_NO+STR(LINENO,6) = SMART_INVOICE_HEADER.SMART_INVOICE_NO FOR CTRCODE = SMART_INVOICE_HEADER.Invoice
        REPLACE cTrCode WITH ''
        =gfReplace('')        
      ENDSCAN
      SELECT SMART_INVOICE_HEADER
      REPLACE INVOICE WITH '',;
              Status WITH '',;
              Paydate WITH {},;
              Invdate WITH {}
      =gfReplace('')        
    ENDIF
  ENDIF
ENDSCAN 
SELECT SMART_INVOICE_LINES
=gfTableUpdate()
SELECT SMART_INVOICE_HEADER
=gfTableUpdate()

=gfModalGen('INM00000B00000',.F.,.F.,.F.,"Dropship invoices related consolidated invoice# updated successfully.")