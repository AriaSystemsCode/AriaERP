#DEFINE LANG_Hist 			  'History'
#DEFINE LANG_Key_date 		  'Key-off Date'
#DEFINE LANG_Tot_Deb		  'Total Debits'
#DEFINE LANG_Tot_Cred		  'Total Credits'
#DEFINE LANG_Opn_Amt		  'Open Amount'
#DEFINE LANG_Tran_no 		  'Transaction No'
#DEFINE LANG_Check 			  'Check #'
#DEFINE LANG_Trn_Date		  'Trans. Date'
#DEFINE LANG_Due_Date		  'Due Date'
#DEFINE LANG_Batch			  'Batch No.'
#DEFINE	LANG_Amt			  'Amount'	
#DEFINE	LANG_Descr			  'Description'
#DEFINE	LANG_Date			  'Date'
#DEFINE LANG_Select 	   	  'Se\<lect'
#DEFINE LANG_unSelect		  'UnSe\<lect'
#DEFINE LANG_No_Trans   	  "No Transactions have been selected. Cannot proceed."
#DEFINE LANG_Reserve 		  'reverse payment# '
#DEFINE LANG_Upd_Gl			  'Update general ledger.'
#DEFINE LANG_Rev_Session 	  'reverse key off session# '
#DEFINE LANG_Rev_Key	  	  'Reverse Key off # '
#DEFINE LANG_CB				  'There are C/B and/or Credit on account entries generated in key off # '
#DEFINE LANG_already_key	  ' have already been keyeed off, Therefore unable to reverse this key off.'
#DEFINE LANG_No_Session 	  "No Key off session has payments with check # "
#DEFINE LANG_Tran_date     	  'The transaction date should not be greater than the reverse date. Cannot select.'
#DEFINE LANG_Tran_Fail 		  'One or more transactions that fall after the reverse date will be ignored.'
*B610975,1 MMT 04/01/2015 Reverse key off screen gives error while selection payment[T20150330.0012][START]
#DEFINE LANG_Revrs_Fail "This payment is partially used. Can't reverse!"
#DEFINE LANG_RevrsA_Fail "the Payment of one or more transactions is partially used. Can't reverse!"
*B610975,1 MMT 04/01/2015 Reverse key off screen gives error while selection payment[T20150330.0012][END]
*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005][Begin]
#DEFINE LANG_NONARCASH 'Non AR Cash'
#DEFINE LANG_ArCash  		"AR Cash    "
*!B611755,1 HMS 31/3/2019  Aria 5 - Reverse the Non A/R transaction [T20181227.0005]End]
* E611824, 1 MMT 12 / 25 / 2019 call the new Global function to update GLTRNHD, GLTRNDT[GL Enhancement][Start]
#DEFINE LANG_GLDIST_INVOICE 'INVOICE             '
#DEFINE LANG_GLDIST_VOIDINVOICE 'VOID INVOICE        '
#DEFINE LANG_GLDIST_CASHR 'CASH RECEIPT        '
#DEFINE LANG_GLDIST_CREDITADJ 'CREDIT ADJUSTMENT   '
#DEFINE LANG_GLDIST_DEBITADJ 'DEBIT ADJUSTMENT    '
#DEFINE LANG_GLDIST_RM 'RETURN MERCHANDISE  '
#DEFINE LANG_GLDIST_VOIDRM 'VOID RETURN         '
#DEFINE LANG_GLDIST_INVPHY 'INVENTORY PHYSICAL  '
#DEFINE LANG_GLDIST_INVADJ 'INVENTORY ADJUSTMENT'
#DEFINE LANG_GLDIST_MATINVPHY 'MATERIAL INV. PHYSI.'
#DEFINE LANG_GLDIST_MATINVDJ 'MATERIAL INV. ADJUS.'
#DEFINE LANG_GLDIST_POREC 'P/O RECEIVING       '
#DEFINE LANG_GLDIST_MAPOREC 'MATERIAL P/O RECEIV.'
#DEFINE LANG_GLDIST_CTREC 'C/T RECEIVING       '
#DEFINE LANG_GLDIST_ZEROOUT 'ZERO OUT STOCK      '
#DEFINE LANG_GLDIST_NONMATLIB 'NON MAT. LIABILITY  '
#DEFINE LANG_GLDIST_POJOBCLOSE 'P/O JOB COST CLOSING'
#DEFINE LANG_GLDIST_MATOPREC 'MATERIAL OP. RECEIVE'
#DEFINE LANG_GLDIST_STYOPREC 'STYLE OP. RECEIVE   '
#DEFINE LANG_GLDIST_RECMFGORDER 'RECEIVE M.F.G. ORDER'
#DEFINE LANG_GLDIST_EXRATEDIFF 'EX. RATE DIFFERENCES'
#DEFINE LANG_GLDIST_KEYOFF 'KEY OFF'
#DEFINE LANG_GLDIST_MAJOBCLOSE 'MATERIAL JOB ClOSING'
#DEFINE LANG_GLDIST_CTJOBCLOSE 'C/T JOB COST CLOSING'
#DEFINE LANG_GLDIST_INVLOCK 'INVENTORY LOCKING'

* E611824, 1 MMT 12 / 25 / 2019 call the new Global function to update GLTRNHD, GLTRNDT[GL Enhancement][End]