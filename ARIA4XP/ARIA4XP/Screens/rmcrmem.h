#DEFINE LANG_CreditMemo_Void        'Void'
#DEFINE LANG_CreditMemo_Edit        'Edit'
#DEFINE LANG_CreditMemo_ObjectLink  "Object Link"
#DEFINE LANG_CreditMemo_Voided      'Voided'
#DEFINE LANG_CreditMemo_Active      'Active'
#DEFINE LANG_CreditMemo_CrMemo      'Credit Memo'
#DEFINE LANG_CreditMemo_HdrConfig   'Configuration'
#DEFINE LANG_CreditMemo_HdrDyelot   'Dyelot'
#DEFINE LANG_CreditMemo_InvBrowHdr  'Invoices'
#DEFINE LANG_CreditMemo_HdrStyle     'Style'
#DEFINE LANG_CreditMemo_HdrDesc      "Description"
#DEFINE LANG_CreditMemo_HdrQuantity  "Quantity"
#DEFINE LANG_CreditMemo_HdrPrice     "Price"
#DEFINE LANG_CreditMemo_HdrAmount    "Amount"
#DEFINE LANG_CreditMemo_HdrReason    "Reason"
#DEFINE LANG_CreditMemo_DelMessage   "void"
#DEFINE LANG_CreditMemo_VoidMSG1     'System date '
#DEFINE LANG_CreditMemo_VoidMSG2     ' does not fall within any period. Not allowed to void returns for this date.' 

*B608555,1 MMT 05/15/2008 Fix bug of credit memo screen print button is not working[Start]
#DEFINE REPORT_DATA "laData[1]=RETHDR.CRMEMO"
*B608555,1 MMT 05/15/2008 Fix bug of credit memo screen print button is not working[End]
*C200876 TMI 05/16/2008 [Start] Add the Options title
#DEFINE LANG_Options                     "O\<ptions"
*C200876 TMI 05/16/2008 [End  ] 

*-- E303020,1 MAB 12/15/2011  .. Add Options Menu Bars .. BEGIN
#DEFINE LANG_OptionCreditMemoNotes       "\<Credit Memo Notes"
#DEFINE LANG_OptionObjectLink            "Ima\<ge Link"
#DEFINE LANG_OptionCustomerInquire       "Customer \<Inquire"
#DEFINE LANG_OptionCustomerNotepad       "Customer \<Notepad"
#DEFINE LANG_OptionStyleInquire          "\<Style Inquire"

#DEFINE BAR_OptionCreditMemoNotes  1
#DEFINE BAR_OptionObjectLink       2
#DEFINE BAR_OptionCustomerInquire  3
#DEFINE BAR_OptionCustomerNotepad  4
#DEFINE BAR_OptionStyleInquire     5
*-- E303020,1 MAB 12/15/2011  .. Add Options Menu Bars .. END
