***********************************************************************
*:  Program File: APAPRPA.prg
*:  Desc.       : Approve for Payment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/13/2011 
*:  Reference   : *E303011,1 
*:  Purpose     : Header file for the screen APAPRPA.SCX
*:************************************************************************
#DEFINE LANG_APAPRPA_Total_Approved    "Total approved"
#DEFINE LANG_APAPRPA_Invoice           "invoice"
#DEFINE LANG_APAPRPA_Debit_memo        "debit memo"
#DEFINE LANG_APAPRPA_Through           "Through"
#DEFINE LANG_APAPRPA_From              "From"

*- The titles used in the grdAprove [start]
#DEFINE LANG_APAPRPA_Inv_No            "Inv. No."
#DEFINE LANG_APAPRPA_Vendor            "Vendor"
#DEFINE LANG_APAPRPA_Priority          "P"
#DEFINE LANG_APAPRPA_Appr_to_pay       "Appr. to pay"
#DEFINE LANG_APAPRPA_Appr_Disc         "Appr. disc."
#DEFINE LANG_APAPRPA_Appr_adj          "Appr. adj."
#DEFINE LANG_APAPRPA_Approved_1099     "Appr. 1099"
#DEFINE LANG_APAPRPA_Open_Amount       "Open amount"
#DEFINE LANG_APAPRPA_Pay_Meth          "Pay. Meth"
#DEFINE LANG_APAPRPA_Inv_Date          "Inv. Date"
#DEFINE LANG_APAPRPA_Due_Date          "Due Date"
#DEFINE LANG_APAPRPA_Discount_Days     "Disc. Days"
#DEFINE LANG_APAPRPA_Division          "Division"
#DEFINE LANG_APAPRPA_Bank              "Bank"
#DEFINE LANG_APAPRPA_Checking_Account  "Checking Account"
#DEFINE LANG_APAPRPA_GL_Account        "GL Account"
*- The titles used in the grdAprove [end ]

*N000682,1 MMT 11/20/2012 Globalization project[Start]
#DEFINE LANG_APAPRPA_APPROVE_ALL     'Approve All'
#DEFINE LANG_APAPRPA_APPRAMOUNT  'Appr. amount '
#DEFINE LANG_APAPRPA_CURRENCY "Currency"
#DEFINE LANG_APAPRPA_CURRENCYCODE 'Currency code'
#DEFINE LANG_APAPRPA_DESC 'Description'
#DEFINE LANG_APAPRPA_SYMBOL 'Symbol'
#DEFINE LANG_APAPRPA_OPENINVOICES 'open invoices'
#DEFINE LANG_APAPRPA_LOSEALLYOURCHANGES 'lose all your changes'
#DEFINE LANG_APAPRPA_CASHPAMENTAPPROVEDCURRENCY 'cash payment approved currency'
#DEFINE LANG_APAPRPA_APPROVE "approve"
#DEFINE LANG_APAPRPA_DEBITMEMO "Debit memo "
#DEFINE LANG_APAPRPA_C_INVOICE "Invoice "
#DEFINE LANG_APAPRPA_APPROVEAMOUNT "approved amount to pay"
#DEFINE LANG_APAPRPA_CASHPAYMENTACCOUNT 'Cash Payment Account'
#DEFINE LANG_APAPRPA_OPTIONGRID "Option Grid"
#DEFINE LANG_APAPRPA_INVNO 'Invoice No.'
*N000682,1 MMT 11/20/2012 Globalization project[End]

*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
#DEFINE LANG_APAPRPA_INVOICE_S "Invoice "
#DEFINE LANG_APAPRPA_FORVENDOR " for vendor "
#DEFINE LANG_APAPRPA_BEINGEDITED " is being edited by user "
*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]