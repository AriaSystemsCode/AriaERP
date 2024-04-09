**:***********************************************************************
*:  Program file : ARDRINV.PRG
*:  Program desc.: Dropship invoices for BBC10
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 09/15/2021
*:      Reference: C202430,1
*:************************************************************************
=gfOpenTable('SMART_INVOICE_HEADER','SMINVHDR')
lcFields    = "smart_invoice_no"
lcBrFields  = "invoice_no:R :H= 'Dropship Invoice #',S_BATCH_NO  :H= 'Batch #' , drpshpdate:R :H= 'Dropship Invoice Date',"+;
             " account :R :H= 'Account #' , Store:R :H= 'Store' , custpo :R :H= 'Customer PO#' ,"+;
             "ship :R :H= 'Pieces' ,shipamt:R :H= 'Shipped Amount', Rec_date:R :H= 'Receive Date', Paydate:R :H= 'Payment Date',"+;
             "Invoice:R :H= 'AR Invoice #', Invdate:R :H= 'AR Invoice Date',Status :R :H= 'Status' "
SELECT SMART_INVOICE_HEADER
=gfSeek('')
lcTmpFile = gfTempName()
SELECT smart_invoice_no as 'invoice_no',S_BATCH_NO,Drop_ship_date as 'drpshpdate',account,Store,custpo,ship,shipamt,Rec_date,Paydate,Invoice,Invdate,Status From;
   SMART_INVOICE_HEADER INTO CURSOR (lcTmpFile) ORDER BY invoice_no
SELECT(lcTmpFile)   
LOCATE 
DECLARE laTemp[1]
llReturn  = gfBrows("",'invoice_no','laTemp','Dropship Invoices')


