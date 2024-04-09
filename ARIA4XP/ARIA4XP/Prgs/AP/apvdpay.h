***********************************************************************
*:  Program File: APMNCHP.prg
*:  Desc.       : Approve for Payment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 01/11/2012 
*:  Reference   : *E303016,1 TMI 
*:  Purpose     : Header file for the screen APVDPAY.SCX
*:************************************************************************
#DEFINE LANG_APVDPAY_CPAYDOCNO       "Pay. No."
#DEFINE LANG_APVDPAY_ADVANCED        "Adv"
#DEFINE LANG_APVDPAY_Pay_Meth        "Pay. Meth"
#DEFINE LANG_APVDPAY_PAYMENT_DATE    "Payment Date"
#DEFINE LANG_APVDPAY_PAYMENT_AMOUNT  "Payment Amount"
#DEFINE LANG_APVDPAY_Vendor          "Vendor"
#DEFINE LANG_APVDPAY_Company         "Company"
*E303442,1 TMI 02/19/2014 14:30 [Start] check if the void date is in different period than the paymnet date
#DEFINE LANG_DIFFERENT_PERIODS_DONT_CONTINUE 'The void date is in a different period than the payment date, can not proceed'
#DEFINE LANG_DIFFERENT_PERIODS_DONT_WARNING  'The void date is in a different period than the payment date, are you sure to proceed?'
*E303442,1 TMI 02/19/2014 14:30 [End  ] 