***********************************************************************
*:  Program File: APMNCHP.prg
*:  Desc.       : Approve for Payment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 01/11/2012 
*:  Reference   : *E303016,1 TMI 
*:  Purpose     : Header file for the screen APVDPAY.SCX
*:************************************************************************
*B610660,1 TMI 01/22/2014 20:37 [ T20140121.0039 task] add lcTNpPay variable
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022]
#DEFINE LANG_APVDPAY_CPAYDOCNO       "Pay. No."
#DEFINE LANG_APVDPAY_ADVANCED        "Adv"
#DEFINE LANG_APVDPAY_Pay_Meth        "Pay. Meth"
#DEFINE LANG_APVDPAY_PAYMENT_DATE    "Payment Date"
#DEFINE LANG_APVDPAY_PAYMENT_AMOUNT  "Payment Amount"
#DEFINE LANG_APVDPAY_Vendor          "Vendor"
#DEFINE LANG_APVDPAY_Company         "Company"
#DEFINE LANG_APVDPAY_SESSION         "Session:"
#DEFINE LANG_APVDPAY_INVNO 'Inv. no.'
#DEFINE LANG_APVDPAY_INVDATE 'Inv. date'
#DEFINE LANG_APVDPAY_REFRENCE 'Reference'
#DEFINE LANG_APVDPAY_FACTOR 'Factor'
#DEFINE LANG_APVDPAY_OTHER 'Other'
#DEFINE LANG_APVDPAY_REMIT 'Remit'
#DEFINE LANG_APVDPAY_PTINTEDCHECK 'Printed checks'
#DEFINE LANG_APVDPAY_MANULCHECK 'Manual checks'
#DEFINE LANG_APVDPAY_NONCHECK 'Non check payments'
#DEFINE LANG_APVDPAY_CASHPAY 'Cash payment'
#DEFINE LANG_APVDPAY_PAYMTH 'Payment method'
#DEFINE LANG_APVDPAY_DISCOFFER 'Disc. Offerd'
#DEFINE LANG_APVDPAY_APPRPAY 'Appr. to pay'
#DEFINE LANG_APVDPAY_DISCAPPR 'Disc. appr.'
#DEFINE LANG_APVDPAY_ADJAPP 'Adj. appr.'
#DEFINE LANG_APVDPAY_PAYPRI 'Payment priority'
#DEFINE LANG_APVDPAY_NETDUE 'Net due days'
#DEFINE LANG_APVDPAY_DISCDAY 'Disc. days'
#DEFINE LANG_APVDPAY_DISCPER 'Disc. percent'
#DEFINE LANG_APVDPAY_DUEDATE 'Due date'
#DEFINE LANG_APVDPAY_BANKCODE 'Bank code'
#DEFINE LANG_APVDPAY_CHECKACC 'Bank checking account'
#DEFINE LANG_APVDPAY_GLCHECK 'GL checking account'
#DEFINE LANG_APVDPAY_CHECKNUM 'Check number'
#DEFINE LANG_APVDPAY_CHECKDATE 'Check date'
#DEFINE LANG_APVDPAY_CREDITCARDVEND 'Credit card vendor'
#DEFINE LANG_APVDPAY_CREDITCARDINV 'Credit card invoice'
#DEFINE LANG_APVDPAY_APACC 'AP account'
#DEFINE LANG_APVDPAY_TOTAL 'Total'
#DEFINE LANG_APVDPAY_1099AMNT '1099 amount'
#DEFINE LANG_APVDPAY_AMNTPAD 'Amount paid'
#DEFINE LANG_APVDPAY_ADJAPPL 'Adj. applied'
#DEFINE LANG_APVDPAY_DISCTAKE 'Disc. taken'
#DEFINE LANG_APVDPAY_TOTPAID 'Total paid'
#DEFINE LANG_APVDPAY_TOTDISC 'Total Discount'
#DEFINE LANG_APVDPAY_TOTADJ 'Total Adjustment'
#DEFINE LANG_APVDPAY_TOT1099 'Total 1099 amount'
#DEFINE LANG_APVDPAY_INVPAYBY 'Invoice paid by'
#DEFINE LANG_APVDPAY_NUMBER 'number'
#DEFINE LANG_APVDPAY_PAYMENTS 'payments'
#DEFINE LANG_APVDPAY_NOVOID 'No payments have been voided'
#DEFINE LANG_APVDPAY_LOSECHANGES 'lose all your changes?'
#DEFINE LANG_APVDPAY_PAYVIDED 'Payment has already been voided.'
#DEFINE LANG_APVDPAY_VOID 'void'
#DEFINE LANG_APVDPAY_VOIDDATE 'void date' 
#DEFINE LANG_APVDPAY_OREQUAL "or equal the payment date"
#DEFINE LANG_APVDPAY_SVOIDDATE "Void date"
#DEFINE LANG_APVDPAY_OPTIONGRID "Option Grid"
#DEFINE LANG_APVDPAY_VOIDINGDATE 'Voiding Date'
#DEFINE LANG_APVDPAY_YES "Yes"
#DEFINE LANG_APVDPAY_NO "No"
*B610660,1 TMI 01/22/2014 20:37 [Start] add lcTNpPay  
#DEFINE LANG_lcTNpPay 'payments to void'
*B610660,1 TMI 01/22/2014 20:37 [End  ] 
*E303442,1 TMI 02/19/2014 14:30 [Start] check if the void date is in different period than the paymnet date
#DEFINE LANG_DIFFERENT_PERIODS_DONT_CONTINUE 'The void date is in a different period than the payment date, can not proceed'
#DEFINE LANG_DIFFERENT_PERIODS_DONT_WARNING  'The void date is in a different period than the payment date, are you sure to proceed?'
*E303442,1 TMI 02/19/2014 14:30 [End  ] 
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
#DEFINE LANG_APVDPAY_APPLYDEBIT 'Apply Debit'
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]