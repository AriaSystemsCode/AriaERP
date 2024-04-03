#DEFINE LANG_SMREBAL_SHPCOLL1   "Shipped Quantities for order #: "
#DEFINE LANG_SMREBAL_COLL       " are being collected."
#DEFINE LANG_SMREBAL_STARTREB   "Rebalancing selected database(s) for company "
#DEFINE LANG_SMREBAL_FINISH     "Finishing rebalance selected database(s) for company "
#DEFINE LANG_SMREBAL_REB        "Rebalancing:"
#DEFINE LANG_SMREBAL_PROCESS    "START PROCESSING     "
#DEFINE LANG_SMREBAL_REBLOG     "Rebalance Log Report"
#DEFINE LANG_SMREBAL_RECALC     "Recalculating balances..."
#DEFINE LANG_SMREBAL_UPDBAL     "Updating balances..."
#DEFINE LANG_SMREBAL_REBCOMP    "Rebalancing Company "
#DEFINE LANG_SMREBAL_REBVERI    "*                                REBALANCE VERIFICATION  REPORT                                 *"
#DEFINE LANG_SMREBAL_TABLE      "Table"
#DEFINE LANG_SMREBAL_NOTFND     "not found in database"
#DEFINE LANG_SMREBAL_ENDPROC    "END PROCESSING       "
#DEFINE LANG_SMREBAL_BADFY      "Fiscal year is not valid, You have to enter valid year"
#DEFINE LANG_SMREBAL_COMP       "Company "
#DEFINE LANG_SMREBAL_DIFFAMT    " has quantity or amount different than its lines."
#DEFINE LANG_SMREBAL_NOTUPD     "Can not update "
#DEFINE LANG_SMREBAL_DATACHG    " because its data was changed before updating."
#DEFINE LANG_SMREBAL_PONO       "P/O #"
#DEFINE LANG_SMREBAL_ORDNO      "Order #"
#DEFINE LANG_SMREBAL_INVNO      "Invoice #"
#DEFINE LANG_SMREBAL_WRONGWO    " has wrong Work Order quantity."
#DEFINE LANG_SMREBAL_WRONGWIP   " has wrong Work in Process quantity."
#DEFINE LANG_SMREBAL_WRONGSHP   " has wrong Shipped quantity."
#DEFINE LANG_SMREBAL_WRONGALO   " has wrong Allocation quantity."
#DEFINE LANG_SMREBAL_WRONGORD   " has wrong Ordered quantity."
#DEFINE LANG_SMREBAL_WRONGRET   " has wrong Return quantity."
#DEFINE LANG_SMREBAL_WRONGTRN   " has wrong In-Transit quantity."
#DEFINE LANG_SMREBAL_WRONGHND   " has wrong Inventory quantity."
#DEFINE LANG_SMREBAL_UPDERR     "Cannot update table "
#DEFINE LANG_SMREBAL_NOERR      "No errors found."
#DEFINE LANG_SMREBAL_CONNERR    "Connection level error..."
#DEFINE LANG_SMREBAL_WRONGUSE    " has wrong Usage quantity."

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
#DEFINE LANG_SMREBAL_CTNO		"C/T "	
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

*B608518,1 MMT 04/17/2008 Add Allocation Module To rebalance program[Start]
#DEFINE LANG_SMREBAL_PACKNO    "PACK #"
#DEFINE LANG_SMREBAL_PACKDIFF  " has total quantity,total Cartons,or total Weight different than its lines."
*B608518,1 MMT 04/17/2008 Add Allocation Module To rebalance program[End]

*B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[Start]
#DEFINE LANG_SMREBAL_STKVAL_MISMTCH  " has wrong Inventory value"
*B608923,1 MMT 07/05/2009 change error log message when stk val mismatch found[End]

*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[Start]
#DEFINE LANG_SMREBAL_VENDOR  ' Vendor: '
#DEFINE LANG_SMREBAL_BARACT  ' Vendor('
#DEFINE LANG_SMREBAL_WRONGDEBIT ") has wrong open debit amount."
#DEFINE LANG_SMREBAL_CURRENTBAL ") has wrong current balance."
#DEFINE LANG_SMREBAL_INFORMATION " information has changed since rebalancing. Balances are not updated."
#DEFINE LANG_SMREBAL_INVOICE  ") Invoice("
#DEFINE LANG_SMREBAL_WRONGPAY ') Has Wrong paid amount'
#DEFINE LANG_SMREBAL_INVOICE2 ' Invoice:'
*! B609188,1 MMT 03/24/2010 Add AP Modeule to Aria4xp Rebalance Program[End]

*B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [T20110622.0022][Start]
#DEFINE LANG_SMREBAL_WRONGOPNPOS ") has wrong open POs."
*B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [T20110622.0022][END]

*N000682,1 12/13/12 TMI Globlization changes[Start] 
#DEFINE LANG_Select_Company 'Select Company'
#DEFINE LANG_Print '\<Print'
#DEFINE LANG_Close '\<Close'
*N000682,1 12/13/12 TMI Globlization changes[End  ] 

*N000682,1 12/24/2012 TMI Globlization changes[Start] 
#DEFINE LANG_Select_Module 'Select Module'
*N000682,1 12/24/2012 TMI Globlization changes[End  ] 

*N000682,1 04/16/2013 HES Globlization changes[Start]
#DEFINE LANG_Purge 'Purge'
#DEFINE LANG_No_Trans "No transactions selected to purge."
#DEFINE LANG_No_Comp "No company selected, Cannot purge."
#DEFINE LANG_cannot_Run 'Cannot run purge program, the system'
#DEFINE LANG_cannot_Run_A27 'Cannot run purge program, the Aria27 system'
#DEFINE LANG_Purged 'Purged'
#DEFINE LANG_Purge_Data 'purge the data without taking a backup'
#DEFINE LANG_Purged_date 'Purged date'
#DEFINE LANG_Backup_Comp "Backup Company Tables Please Wait ..."
#DEFINE LANG_Backup_SQL "Backup SQL Company Tables Please Wait ..."
#DEFINE LANG_Backup_Failed "Backup Failed!"
#DEFINE LANG_Backup_Completed "Backup Completed Successfully."
#DEFINE LANG_companies 'companies'
#DEFINE LANG_comp_Not_Hist "The selected company does not have a history company."
#DEFINE LANG_Select_Module 'Select Module'
#DEFINE LANG_Date_Cannot_Empty "Date cannot be empty."
#DEFINE LANG_Date_NotValid "Date cannot be greater than or equal the Start Date of the Previous Year."
#DEFINE LANG_Purge_Complete "Purge is complete."
#DEFINE LANG_ALL 'All'
#DEFINE LANG_copy_Master_Failed "Copy master files failed."
#DEFINE LANG_copy_Master_Complete 'Copy Master files Complete'
#DEFINE LANG_Transaction 'Transaction '
#DEFINE LANG_failed  ' failed.'
#DEFINE LANG_Purge_Transaction 'Purge Transaction '
#DEFINE LANG_Complete ' Complete'
#DEFINE LANG_Purge_file 'Purge file '
#DEFINE LANG_record  ' record '
#DEFINE LANG_Copy_Style "Copy Style Inventory Journal, Session Number : "
#DEFINE LANG_Copy_Fabric "Copy Fabric Inventory Journal, Session Number : "
#DEFINE LANG_Physical_Count "Physical Count"
*N000682,1 04/16/2013 HES Globlization changes[End  ]

*B610814,1 TMI 08/20/2014 11:34 t20140715.0001 [Start] 
#DEFINE LANG_SMREBAL_EXTRFIELDSIZE "Style Cost is too high."
*B610814,1 TMI 08/20/2014 11:34 [End  ] 
*E303873,1 MMT 09/26/2017 Adding GL Module to Rebalance program[P20170905.0001][START]
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
#DEFINE LANG_SMREBAL_RECONCREPTITL 'GL Entries Reconciliation'
#DEFINE LANG_SMREBAL_GLENTRYUPDATED "One or more GL Entries are added, Deleted, or updated."
#DEFINE LANG_SMREBAL_GLBATCHNUMBER "GL Batch#: "
#DEFINE LANG_SMREBAL_GLBATCHUNRELEASED " has been un-released as one or more GL entries included in it are updated."
#DEFINE LANG_SMREBAL_GLENTRYACCOUNT "GL Entry For GL Account: "
#DEFINE LANG_SMREBAL_GLTRANSACTION ", Transaction#: "
#DEFINE LANG_SMREBAL_GLCANNOTUPDATE " Could not be updated as its related GL Batch#: "
#DEFINE LANG_SMREBAL_GLISPOSTED " is Posted."
#DEFINE LANG_SMREBAL_GLLOCKEDPERIOD " Could not be updated as it falls in Locked period: "
#DEFINE LANG_SMREBAL_GLINVALIDPERIOD " Could not be updated as it falls in invalid period"
#DEFINE LANG_SMREBAL_GLENTRIESFILEUPDATE 'Updating GL Entries File.'
#DEFINE LANG_SMREBAL_GLTRANNUM "Transaction: "
#DEFINE LANG_SMREBAL_GLACCNUM "), GL Account: "
#DEFINE LANG_SMREBAL_GLINCORRECTENTRY " has incorrect GL entry"  
#DEFINE LANG_SMREBAL_GLMISSINGENTRY "  has missing GL entry"  
#DEFINE LANG_SMREBAL_GLMISMATCH "  has GL entry amount mismatch"  
#DEFINE LANG_SMREBAL_GLHISTRECONC 'Reconciling History File GL Entries.'
#DEFINE LANG_SMREBAL_GLCREDITRECONC 'Reconciling Credit File GL Entries.'
#DEFINE LANG_SMREBAL_GLDEBITRECONC 'Reconciling Debit File GL Entries.'
#DEFINE LANG_SMREBAL_GLMAINVJLRECONC 'Reconciling Material Inventory Journal File GL Entries.'
#DEFINE LANG_SMREBAL_GLCRMEMORECONC 'Reconciling Credit Memos File GL Entries.'
#DEFINE LANG_SMREBAL_GLINVOICERECONC 'Reconciling Invoice File GL Entries.'
#DEFINE LANG_SMREBAL_GLSTYLEINVJLRECONC 'Reconciling Style Inventory Journal File GL Entries.'
#DEFINE LANG_SMREBAL_GLFISCALYEARINVALID "Fiscal year is not valid, You have to enter valid year"
#DEFINE LANG_SMREBAL_GLFISCALYEARBRWTITLE 'Fiscal Year'
#DEFINE LANG_SMREBAL_GLENDDATE 'End date'
#DEFINE LANG_SMREBAL_GLBEGDATE  'Begin date'
#DEFINE LANG_SMREBAL_GLYEAR 'Year'
#DEFINE LANG_SMREBAL_GLUPDATE 'Update'
#DEFINE LANG_SMREBAL_GLMSGCHRT1 "Found mismatch between the current chart of account's, and"
#DEFINE LANG_SMREBAL_GLMSGCHRT2 "the recalculated balances file. You need to run the"
#DEFINE LANG_SMREBAL_GLMSGCHRT3 "recalculation process again.       "
#DEFINE LANG_SMREBAL_GLMSGTRAN1 "One or more transaction were posted since the last "
#DEFINE LANG_SMREBAL_GLMSGTRAN2 "recaculation of balances. You need to run the "
#DEFINE LANG_SMREBAL_GLMSGTRAN3 "recalculation process again.                       " 
#DEFINE LANG_SMREBAL_GLACBALSFILE 'Fixing Account balance file'
#DEFINE LANG_SMREBAL_GLBATCHFILE "Fixing Batches file"
#DEFINE LANG_SMREBAL_GLNOBATCHREPOST "No batche's or transaction's were reposted."
#DEFINE LANG_SMREBAL_GLSOMEBATCHSUCC "Some of the Batches were not succesfully reposted to the " 
#DEFINE LANG_SMREBAL_GLBALFILESUCC "balances file , Unable to proceed with rebalancing."
#DEFINE LANG_SMREBAL_GLBEGINNINGBATCH "Beginning Batches"
#DEFINE LANG_SMREBAL_GLBATCHES "Batches"
#DEFINE LANG_SMREBAL_GLREBALANCE "GL Rebalance"
#DEFINE LANG_SMREBAL_GLSTARTYEAR1 "You have selected to rebalance starting previous year."
#DEFINE LANG_SMREBAL_GLSTARTYEAR2 "The system allows purging files of the previous year."
#DEFINE LANG_SMREBAL_GLSTARTYEAR3 "Rebalancing starting from previous year won't be accurate"
#DEFINE LANG_SMREBAL_GLSTARTYEAR4 "if you purged the files through this year.                "
*E303873,1 MMT 09/26/2017 Adding GL Module to Rebalance program[P20170905.0001][END]

*E303990,1 MMT 05/28/2018 Add AR History rebalance option to AR module[T20180511.0001][Start]
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_INV 'Fix Customer History File from Invoice # '
#DEFINE LANG_SMREBAL_HAS_TRAN_DATE " has Transaction date "
#DEFINE LANG_SMREBAL_FALLIN_ANY_PERIOD " which does not fall within any period."
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_ORD 'Fix Customer History File from Order # '
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_CR 'Fix Customer History File from Return # '
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_RETURN 'Return #'
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_DEBIT 'Fix Customer History File from Debit Transaction # ' 
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_CREDIT 'Fix Customer History File from Credit Transaction # '
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_ARHIST 'Fix Customer History File from History Transaction # ' 
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_DEBITTR "Debit Tran # "
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_CREDITTR "Credit Tran # " 
#DEFINE LANG_SMREBAL_CUSTOMER_HISTORY_HISTTRAN "History Tran # "
#DEFINE LANG_SMREBAL_CUSTOMER_REPORT 'Reporting differences in Customer History file for Account : '
#DEFINE LANG_SMREBAL_ORDER_AMOUNT_ACCOUNT  "Order Amount for Account"
#DEFINE LANG_SMREBAL_ORDER_QTY_ACCOUNT "Order Quantity for Account"
#DEFINE LANG_SMREBAL_SALES_AMOUNT_ACCOUNT "Sales Amount for Account"
#DEFINE LANG_SMREBAL_SALES_QTY_AMOUNT "Sales Quantity for Account"
#DEFINE LANG_SMREBAL_DISCOUNT_AMOUNT "Discount Amount for Account"
#DEFINE LANG_SMREBAL_COST_OF_GOODS "Cost of goods sold Amount for Account"
#DEFINE LANG_SMREBAL_RETURNED_AMOUNT "Returned Amount for Account"
#DEFINE LANG_SMREBAL_RETURNED_QTY "Returned Quantity for Account"
#DEFINE LANG_SMREBAL_ALLOWANCE "Allowance Amount for Account"
#DEFINE LANG_SMREBAL_PAY_ACCNT "Payment Amount for Account"
#DEFINE LANG_SMREBAL_DEBIT_AMT_ACT "Debit Amount for Account"
#DEFINE LANG_SMREBAL_CREDIT_AMT_ACT "Credit Amount for Account"
#DEFINE LANG_SMREBAL_HAS_NO_RECORD_ACT "has no record for Account code"
#DEFINE LANG_SMREBAL_HAS_AMNT_DIFF " has amount different than Customer History file."
#DEFINE LANG_SMREBAL_HAS_QTY_DIFF  " has quantity different than Customer History file."
#DEFINE LANG_SMREBAL_AND_PERIOD " and Period # " 
#DEFINE LANG_SMREBAL_IN_CUSTOMER_HIST " in Customer History file."
*E303990,1 MMT 05/28/2018 Add AR History rebalance option to AR module[T20180511.0001][End]