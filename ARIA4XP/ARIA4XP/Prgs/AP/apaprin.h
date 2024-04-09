*:************************************************************************
*:  Program File: APRCRIN.PRG
*:  Desc.       : Recurring Payable
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Reference   : *E303065,1 TMI 03/01/2012 
*:************************************************************************
#DEFINE LANG_APAPRIN_lcBrInvTtl    "Amounts to be approved"
#DEFINE LANG_APAPRIN_lcVendTtl     "Vendor summary"
#DEFINE LANG_APAPRIN_lcInvoice     "Invoice summary"
#DEFINE LANG_APAPRIN_lcTInsDate    "installment date"
#DEFINE LANG_APAPRIN_lcTIns        "installment"
#DEFINE LANG_APAPRIN_lcTZero       "zero"
#DEFINE LANG_APAPRIN_lcTSelect     "\<Select"
#DEFINE LANG_APAPRIN_lcTUnSelect   "Un\<select"
#DEFINE LANG_APAPRIN_lcTBrowse     "\<Browse"
#DEFINE LANG_APAPRIN_lcTGenIns     "Generating installment..."
#DEFINE LANG_APAPRIN_lcTAprIns     "Approving installment..."
#DEFINE LANG_APAPRIN_lcTAll        "All"
#DEFINE LANG_APAPRIN_lcDueFrom     "due from"
#DEFINE LANG_APAPRIN_lcDueTo       "due to"
#DEFINE LANG_APAPRIN_lcDiscFrom    "discount from"
#DEFINE LANG_APAPRIN_lcDiscTo      "discount to"
#DEFINE LANG_APAPRIN_lcTApr        "The total approved amount"
#DEFINE LANG_APAPRIN_lcTOpenAmt    "the open invoice amount"
#DEFINE LANG_APAPRIN_lcT1099       "The approved 1099 amount"
#DEFINE LANG_APAPRIN_lcTAprInst    "the approved installment amount" 
#DEFINE LANG_APAPRIN_lcTVendor     "Vendor : " 
#DEFINE LANG_APAPRIN_lcTThisVend   "This vendor" 
#DEFINE LANG_APAPRIN_lcTVenInst    "The installment for invoice no. " 
#DEFINE LANG_APAPRIN_lcTInvoice    "Invoice : "
#DEFINE LANG_APAPRIN_lcTThisInv    "This invoice"
#DEFINE LANG_APAPRIN_lcTInvInst    "The installment for this invoice " 
#DEFINE LANG_APAPRIN_lcTNoSInv     "selected invoices"    
#DEFINE LANG_APAPRIN_lcTNoAprInv   "installments to approve"