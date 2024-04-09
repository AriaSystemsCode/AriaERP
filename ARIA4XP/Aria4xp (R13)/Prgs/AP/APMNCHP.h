***********************************************************************
*:  Program File: APMNCHP.prg
*:  Desc.       : Approve for Payment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 01/03/2011 
*:  Reference   : *E303014,1 
*:  Purpose     : Header file for the screen APMNCHP.SCX
*:************************************************************************
#DEFINE LANG_APMNCHP_Total_Approved    "Total approved"
#DEFINE LANG_APMNCHP_Invoice           "invoice"
#DEFINE LANG_APMNCHP_Debit_memo        "debit memo"
#DEFINE LANG_APMNCHP_Through           "Through"
#DEFINE LANG_APMNCHP_From              "From"

*- The titles used in the grdAPINVHDR [start]
#DEFINE LANG_APMNCHP_Inv_No            "Inv. No."
#DEFINE LANG_APMNCHP_Vendor            "Vendor"
#DEFINE LANG_APMNCHP_Priority          "P"
#DEFINE LANG_APMNCHP_Appr_to_pay       "Appr. to pay"
#DEFINE LANG_APMNCHP_Apr_foreign_amt   "Apr foreign amt"
#DEFINE LANG_APMNCHP_Appr_Disc         "Appr. disc."
#DEFINE LANG_APMNCHP_Appr_adj          "Appr. adj."
#DEFINE LANG_APMNCHP_Approved_1099     "Appr. 1099"
#DEFINE LANG_APMNCHP_Open_Amount       "Open amount"
#DEFINE LANG_APMNCHP_Pay_Meth          "Pay. Meth"
#DEFINE LANG_APMNCHP_Inv_Date          "Inv. Date"
#DEFINE LANG_APMNCHP_Due_Date          "Due Date"
#DEFINE LANG_APMNCHP_Discount_Days     "Disc. Days"
#DEFINE LANG_APMNCHP_Division          "Division"
#DEFINE LANG_APMNCHP_Bank              "Bank"
#DEFINE LANG_APMNCHP_Checking_Account  "Checking Account"
#DEFINE LANG_APMNCHP_GL_Account        "GL Account"
#DEFINE LANG_APMNCHP_Curr_code         "Curr. code"
#DEFINE LANG_APMNCHP_Curr_rate         "Curr. rate"

*- The titles used in the grdAprove [end ]

*N000682,1 MMT 11/20/2012 Globalization project[Start]
#DEFINE LANG_APMNCHP_APPROVE_ALL     'Approve All'
#DEFINE LANG_APMNCHP_SELECT 'Se\<lect'
#DEFINE LANG_APMNCHP_UNSELECT 'Unse\<lect'
#DEFINE LANG_APMNCHP_MANUALCHECKPAY 'Manual Check Payment'
#DEFINE LANG_APMNCHP_NONCHECKPAYMENT 'Non-Check Payment'
#DEFINE LANG_APMNCHP_CASHPAYMENT'Cash Payment'
#DEFINE LANG_APMNCHP_SESSION '      Session : '
#DEFINE LANG_APMNCHP_PAYNUMBER 'Payment Number :'
#DEFINE LANG_APMNCHP_CHECKNUMBER 'Check Number :'
#DEFINE LANG_APMNCHP_VENDORS "Vendors"
#DEFINE LANG_APMNCHP_CURRENCY 'Currency '
#DEFINE LANG_APMNCHP_PHONE 'Phone'
#DEFINE LANG_APMNCHP_COMPANY 'Company'
#DEFINE LANG_APMNCHP_DESC 'Description'
#DEFINE LANG_APMNCHP_BANKCODE 'Bank Code'
#DEFINE LANG_APMNCHP_BANKTITLE 'Bank Codes '
#DEFINE LANG_APMNCHP_CURRENCYCODE 'Currency code'
#DEFINE LANG_APMNCHP_SYMBOL 'Symbol'
#DEFINE LANG_APMNCHP_NOINVOICESPAID 'No invoices have been paid to save'
#DEFINE LANG_APMNCHP_LOSECHANGES 'lose all your changes?'
#DEFINE LANG_APMNCHP_PAYDATE 'payment date'
#DEFINE LANG_APMNCHP_MSG_PAYNUMBER 'payment number'
#DEFINE LANG_APMNCHP_MSG_CHECKNUMBER 'Check number'
#DEFINE LANG_APMNCHP_VENDCODE 'Vendor code'
#DEFINE LANG_APMNCHP_CHKACCCODE 'Checking Account code'
#DEFINE LANG_APMNCHP_HISTORYPERIOR 'history prior   current  future  '
#DEFINE LANG_APMNCHP_INVOICE_S "Invoice "
#DEFINE LANG_APMNCHP_FORVENDOR " for vendor "
#DEFINE LANG_APMNCHP_BEINGEDITED " is being edited by user "
*N000682,1 MMT 11/20/2012 Globalization project[End]
