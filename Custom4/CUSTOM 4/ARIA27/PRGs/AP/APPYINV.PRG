*:************************************************************************
*:
*: Procedure file: APPYINV.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow
*:               : lfDelShow
*:               : lfActivate
*:               : lfwData_1
*:               : lfvData_1
*:               : lfwData_2
*:               : lfvData_2
*:               : lfwPhone
*:               : lfwCompany
*:               : lfvVendor
*:               : lfwDivision
*:               : lfvDivision
*:               : lfwData_10
*:               : lfvData_10
*:               : lfwPayMethd
*:               : lfvPayMethd
*:               : lfvRemit
*:               : lfwData_12
*:               : lfvData_12
*:               : lfwTermCode
*:               : lfvTermCode
*:               : lfwData_16
*:               : lfvData_16
*:               : lfwData_17
*:               : lfvData_17
*:               : lfwData_18
*:               : lfvData_18
*:               : lfvDicount
*:               : lfvPartial
*:               : lfvDisc
*:               : lfvOk
*:               : lfwData_19
*:               : lfvData_19
*:               : lfwData_20
*:               : lfvData_20
*:               : lfwData_21
*:               : lfvData_21
*:               : lfwApAcct 
*:               : lfvApAcct
*:               : lfvData_22
*:               : lfwData_23 
*:               : lfvData_23
*:               : lfwData_24
*:               : lfvData_24
*:               : lfvAprovPay
*:               : lfShow
*:               : lfwData_27
*:               : lfwData_28
*:               : lfvBnkChk
*:               : lfwData_29
*:               : lfvData_29
*:               : lfwData_13
*:               : lfwData_14
*:               : lfwData_30
*:               : lfvApprove
*:               : lfvOkApr
*:               : lfvOkPay
*:               : lfvOkCash
*:               : lfvClrApr
*:               : lfvCanPay
*:               : lfvDistrib
*:               : lfvDisAct
*:               : lfwDiscAcct
*:               : lfvDiscAcct
*:               : lfBrowse
*:               : lfwDisBrow
*:               : lfvBrowse
*:               : lfwData_33
*:               : lfvData_33
*:               : lfwData_36
*:               : lfvData_36
*:               : lfwData_38
*:               : lfvData_38
*:               : lfwDistAmnt
*:               : lfvDistAmnt
*:               : lfwApGlAcc
*:               : lfvApGlAcc
*:               : lfwTaxCode
*:               : lfvTaxCode
*:               : lfDispObjct
*:               : lfvLineNo
*:               : lfvTrapKey
*:               : lfvClearTrap
*:               : lfvNewLine
*:               : lfvRemLine
*:               : lfvClsChld
*:               : lpSavScr
*:               : lpDelScr
*:               : lfvUpdTemp
*:               : lpTab
*:               : lpShiftTab
*:               : lfActBrows
*:               : lfSetTrap
*:               : lfResetTrap
*:               : lfRelease
*:               : lfTaxAct
*:               : lfGetArrElem
*:               : lfvAASAcct
*:               : lfVendView
*:               : lfwScrolVar
*:               : lfShiftlaData
*:               : lfvMoveToData
*:               : lfwData_44
*:               : lfvData_44
*:
*:      Documented   /  /  
*:************************************************************************
*B500684,1 MAN   06/01/95 Fixed the Posittion off the screen when you tab
*B500684,1 MAN   06/01/95 in the dist. screen, & Enhanced the speed of the scr.
*B500684,1 MAN   06/01/95 & Fix Browsing from the invoice number field
*B600375,1 MAN   06/03/95 Fixed Updating the Mat PO Actual Cost
*B600411,1 MAN   06/11/95 Fixed the seek expr. on the POSLN to update the
*B600411,1 MAN   06/11/95 actual cost fields
*B600417,1 MAN   06/12/95 Updating the Actual cost fields on the receiving
*B600417,1 MAN   06/12/95 records in the CT Line file & updating the issued
*B600417,1 MAN   06/12/95 and used fields on the CTKTBOM in case of PO or CT
*B600417,1 MAN   06/12/95 invoicing
*B600446,1 MAN   06/14/95 Updating the actual cost fields on the PO header
*B600446,1 MAN   06/14/95 in case of invoicing shipments
*B600446,1 MAN   06/14/95 by updating the costing fields while scanning on
*B600446,1 MAN   06/14/95 the temp. BOMLINE
*B600460,1 MAN   06/18/95 Fix Red error while browse the ticket number
*B600460,1 MAN   06/18/95 Notice that the bug is fixing goes through APMATST.PRG
*B600583,1 MAN   07/25/95 Fixing GL entries created in APDIST When changing 
*B600583,1                the invoice amount
*B600582,1 ORABY 07/26/95 Fix the approved payment calculation When changing 
*B600582,1                the invoice amount
*B600592,1 ORABY 08/01/95 Adjusting the creditor invoice number field width to 12 chr.
*B600593,1 ORABY 08/01/95 After the using of the credit child window the record pointer in the
*B600593,1                APVENDOR file is not in its initial position, so we move it to its initial pos.
*B600595,1 ORABY 08/01/95 In some cases the distribution button is enable in the view mode
*B600619,1 Malak 08/16/95 Browse(Inv.no.) will not show the data of DIVISION.
*B600658,1 ORABY 08/23/95 If you changed the payemnt method for a previously saved invoice to  Credit Card
*B600658,1                the cumulative purchase field in the vendor file is increased which is wrong
*E100219,1 Malak 09/12/95 Add new option "Cutting ticket operations"
*B600693,1 M.H   09/19/95 Create the reverse transaction in the APDIST file
*B600693,1 M.H   09/19/95 if the user change the invoice date.
*E300296,1 M.H   10/10/95 Add the currency to the AP module.
*B600813,1 RENEE 11/15/95 Fix the bug of being able to approve amounts
*B600813,1                greater than the open invoice amount.
*B600843,1 WAM   11/28/95 Fix a bug while recomputing fabric average cost
*B600840,1 WAM   11/28/95 Do not update landed cost while invoicing the
*B600840,1                PO receipt.
*B600849,1 HISH 11/29/95  Used gfChkRate to get exchange rate and unit to changed currency
*B600849,1                from invoice currency to approve currency.
*B600855,1 M.H  12/03/95  Update the unit cost with the actual cost.
*E300316,1 HISH 12/06/95. Used global function gfGetExSin to get currency 
*E300316,1                 equation signs.
*B600880,1 M.H   12/11/95 Fix the problem of returnning to the select mode after selecting P.O. receipts.
*E300316,1 HISH 01/08/96  Passed pointer parameter to get Unit sgin.
*B600971,1 M.H   02/11/95 Every time we do a payable invoice entry, the system checks the credit available. Could we have this optional
*B600971,1 M.H            to eliminate this key stroke?? If The credit limit iz zero do not display the message.
*B600971,1 M.H Begin.
*  IF laData[12] > lnVenCrAvl
*  IF laData[12] > lnVenCrAvl .AND. APVENDOR.NVENCRLIM <> 0
*B600971,1 M.H End.
*B800476,1 ORABY 02/27/95 If the vendor is on hold ( IF APVENDOR.CVENPRIOR = '0') do not let the user approve the invoice 
*B600986,1 MAN   02/28/96 Fixing the bug of not showing the receiving session for the CT
*B601013,1 RENEE 03/31/96 Multi currency :
*B601013,1                APDIST.nEqvAmnt has not always been updated with
*B601013,1                correct equivalent amounts, as concerning
*B601013,1                the signs of the currency fields
*B601014,1 RENEE 03/31/96 In a Credit Card payment invoice, 2 records are
*B601014,1                added to APDIST. Both of these should have
*B601014,1                APDIST.cApdActId = 'A'
*N16,4     WAM   04/03/96 Update MFG Order Actual Cost in the MMFGORDH file
*N16,4                    while invoicing Material Purchase order.
*B601020,1 WAM   04/03/96 Fix a 'Variable Not Found' Error in the GL Account
*B601020,1                validation Screen while invoicing Material PO.
*B601120,1 M.H   06/19/96 If you modify the invoice amount and click on the Dist. lines button and then close the dist. screen without 
*B601120,1 M.H   06/19/96 modifying anything.  Do this procedure more that once you will find extra record in the distribution file which is not correct.
*B800689,1 M.H   07/30/96 Fix the bug of voiding a debit memo that is alredy payed.
*B800735,1 M.H   08/28/96 In both add or edit mode if the user add an extra line in the dist. lines wich is greater than the invoice amount
*B800735,1 M.H   08/28/96 and then increase the invoice amount with the same amount added in the extra invoice line.
*B601249,1 WAM   09/06/96 Upon Invoicing material POs, Update the WIP 
*B601249,1                account only if total issues and returns greater 
*B601249,1                than zero.
*B601425,1  HS   11/21/96 Refresh the line number and long description 
*B601425,1                SAY FIELDS in the Dist.Lines Screen  
*B601448,1  HS   12/02/96 Fix the Credit Card Vendor validation to prevant
*B601448,1                Invoice No. duplicate
*C100728,1 M.H   12/08/96 Add the invoice posting date to the screen.
*B601419,1 HS    12/16/96 Fix the Template Field Label [Change it to a say Field]  
*B601419,1                and other things [look at the bug describtion]
*B601416,1 HS    12/26/96 Change the calling of gfChkRate and gfGetExSin 
*B601416,1                in the Approve for payment case so if the Invoice 
*B601416,1                currency is the base currency I make it the To  
*B601416,1                currency and if not the Approve currency is 
*B601416,1                the To currency 
*B601526,1  HS  01/15/97  Use the new field [APINVHDR.nInvFAAp] in the 
*B601526,1                Approve for payments screens [Fill the new field
*B601526,1                with the approved amount in the approve currency] 
*B601526,1                by using laData[51] instaed of lnExchange
*B601612,1  HS  02/05/97  1)Fix the currency code in the approve for payment
*B601612,1                  screen.
*B601612,1                2)Clear the approved amount if the user changed 
*B601612,1                  the invoice currency. 
*B601667,1  HS  03/31/97  Fix the from currency code and the to currency
*B601667,1                code in the Message :
*B601667,1                    " A valid ð to ð exchange rate could not "
*B601667,1                    " be found for ð.                        "
*B601655,1 HS    04/06/96 IF any of this files APINVHDR,APDIST,APVENDOR
*B601655,1                ,APVENHST was not locked update the flag llCSave
*B601655,1                and give the user message that the saving process
*B601655,1                is canceled.
*E300643,1  HS 04/15/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300643,1              3)Make some changes for we have change the function
*E300643,1              [gfRltFld]
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300683,1 & RENEE        AND Add prgs directory path to the calling of programs
*B601750,1 RENEE 06/22/97 Tax code popup is not refreshed
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*B801300,1 HS    10/06/97 Accept the Invoice date if it belong to a locked
*B801300,1                period 
*B801308,1 HS    10/08/97 Fiscal year and period is not saved in APDIST file
*B601897,1 HS    12/04/97 Fix the Void problem [Select an invoice then
*B601897,1                Void (Delete) you get a message "Missing )"]
*B601897,1                [I added lines to get the Exchange sin].
*B801498,1  HS   03/26/98 1) If the user is editing an invoice ,clear the
*B801498,1                   batch number and transaction number and entry
*B801498,1                   number in the new APDIST records if he has
*B801498,1                   changed the posting date.
*B801498,1                2) Clear the entry number whenever we clear
*B801498,1                   the batch number.
*B801499,1  HS   03/29/98 If the Posting date falls within a locked period.
*B801499,1                Prevent the user from editing : 
*B801499,1                1) The Invoice amount.
*B801499,1                2) The Posting date.
*B801499,1                3) The Distribution lines.
*B801499,1                Note : I made some changes in the screen APINVDS1
*E300807,1 HS    03/30/98 Adding a popup [Inquiry] with one bar [Payments]
*E300807,1                that activate a browse window that shows the 
*E300807,1                payments for the current invoice.
*B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
*B602238,1 WAM 12/01/98 Update exchange rate and unit signs.
*B602274,1 WAM 12/05/98 Fix browse WIP account and updating total actual cost.
*B602294,1 WAM 12/05/98 Default approve currency to invoice currency upon 
*B602294,1              cash payment approve.
*B801894,1 WAM 01/10/99 Disable Invoice lines option when the vendor supply
*B801894,1              only cutting ticket and invoicing with forign currency.
*B602422,1 WAM 01/10/99 Check if MFG Code has been setup to be applied from AP
*B602337,1 WAM 01/11/99 Read opration lot when invoicing operation cost items only.
*B602466,1 WAM 01/25/99 Default operation while generate invoice lines automatically
*B801912,1 AMM 02/04/1999 Allow entering a posting date less than the invoice date
*B602554,1 WAM 02/17/99 Fix compare invoice currency with ticket currency
*E301077,71 AMM 02/18/99 Reduce Opened files.
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*B602654,1 WAM 03/09/99 Get PO style price for styles have no detail costing
*B602617,1 WAM 03/20/99 Round invoice lines amount to match ap dist amount.
*B602830,1 HS  05/19/99 1) Adding the Decimal point to the "Quantity" fields
*B602830,1                 in the case of document types Material PO,
*B602830,1                 Material PO Return and Material MFG Order.
*B602830,1              2) Rounding problems in the "Automatic Contribution"
*B602830,1                 (Base Cost Contribution on: Budget Cost and
*B602830,1                 Received Quantities).
*B602830,1              3) Prevent entering an "Approved Amount" less than
*B602830,1                 the "Contributed Amount".
*B602830,1              4) The refresh of the "Contributed Amount" in the
*B602830,1                 Contribution screen when you enter the screen
*B602830,1                 for the second time.
*B602830,1              5) Giving the user the capability to select the
*B602830,1                 "GL Account" in the case of document type
*B602830,1                 "General Expense".
*B602830,1              6) Giving the user the capability to select the
*B602830,1                 "WIP Account" if he don't have GL Link.
*B602830,1              7) Prevent the user from saving and from entering
*B602830,1                 the "Invoice Detail" screen if the "Cur." field
*B602830,1                 is empty.
*B602830,1              8) Some errors when you try to browse from the
*B602830,1                 "Cur." Field.
*B602830,1              9) When you delete an "Invoice Detail Line" in the
*B602830,1                 "Add" mode the corresponding "Distribution Line"
*B602830,1                 is not deleted instead the "Distribution Line
*B602830,1                 Amount" is set as 0.
*B602830,1              10) Refreshing of WIP account, and General expense
*B602830,1                 account.
*B802110,1 HS  05/20/99 Give the user the capability to add debit memo
*B802110,1              lines in the "Invoice Detail" screen.
*B802286,1 HS  05/20/99 1) When updating the BOMCOST file update "Actualize"
*B802286,1                 field with "Y" in the records that has a
*B802286,1                 receiving session.
*B802286,1              2) Update the records with no receiving session in
*B802286,1                 the BOMCOST file with the uncontributed amount
*B802286,1                 and not with the total amount.
*B602367,1 HS  06/02/99 Fix the default value of the credit card vendor
*B602367,1              AP Account in the "Credit card payment" screen.
*B602946,1 HS  06/03/99 1) When you edit the contribution of an existing
*B602946,1                 line in the edit mode some of the files are not
*B602946,1                 updated.
*B602946,1              2) Prevent saving if the applied debit amounts will
*B602946,1                 produce a negative actual cost.
*B602946,1              3) Accumulate the actual cost in the PO and CT lines
*B602946,1                 instead of recalculating them every time.
*B602946,1              4) Enable the template field only if the "Vendor
*B602946,1                 Invoice Types" is empty.
*B602946,1              5) Fix the defaulting of the "AP Account" and
*B602946,1                 "Expense Account".
*B802348,1 AKA 06/22/99  Avoid error message file not found in case the Style Purchase Orders 
*B802348,1               Module is not installed.
*B603067,1 AKA  07/18/99 Fixing the bug of reversing transaction in the APDIST file
*B603067,1      07/18/99 if the user change the invoice date.
*B802385,1 AKA  07/25/99 Can not use incremental serach in vendor browse in AP invoice.
*B602997,1 AKA  07/19/99  bug # 1) Fixing bug of AP invoice was not generate entry for discount 
*B602997,1      07/19/99           in case credit card payment methoed was used. 
*B602997,1      07/26/99  bug # 4) Can't enter shipment code in case if it was a character you must 
*                                  select it from browse, this bug fixed in APINVD13.SPR by changing the format
*                                  from 'X99999' to 'XNNNNN'
*B802471,1 AKA  07/28/99  Fix error variable 'natcost4n' not found.
*B802710,1 MAN  10/24/99  Fix getting a numeric overflow when invoicing a material po
*B802783,1 MAN  11/15/99  Fix wrong display for AP Account 
*B603483,1 NAD  02/23/2000 Fix bug wrong session number update
*B603520,1 SSE 03/19/2000 Fix bug of wrong Updating fields nEActCost1..5,
*B603520,1                nAct_Cost1..5 in both PoFHdr & PoFLn 
*B603484,1 SSE 03/20/2000 Fix Bug of filling Tax Code field with Tax desc.
*B603484,1                 in the Distribution line screen
*B600981,1 SSE 03/21/2000 Fix Bug of Emptying and Disabling Checking Account
*B600981,1                 in case of empty Bank Code
*B603371,1 SSE 03/20/2000 Fix Bug of not Browsing Manufacturing Order file
*B603371,1                and also Error Missing Expression when trying to 
*B603371,1                browse Item for specific manufacturing Order Typed
*B803165,1 SSE 04/02/2000 If Vendor is Factored when saving and choosing remit 
*B803165,1                to Factor , no address is added for this Factor in 
*B803165,1                the ApInvHdr file 
*B603390,1 KHM 07/20/2000 Fix the bug of record is locked by another user
*B603390,1                while its not locked because of the wrong used 
*B603390,1                of posln index incase of shipment.
*B603738,1 KHM 07/20/2000 Fix the bug of not updating the actual cost in 
*B603738,1                case of shipment
*B603653,1 KHM 07/20/2000 Fix the bug of allowing the Misc. cost item to
*B603653,1                be invoiced even when the invoice currency is 
*B603653,1                different than the Misc. currency (which is the 
*B603653,1                duty currency as they are in the same category 
*B603653,1                of the duty currency)
*B603769,1 KHM 07/24/2000 Enlarge the size of the price and amount in the
*B603769,1                screen(APINVD12) of the screen set(APINVDT)
*B603827,1 KHM 08/27/2000 Fix the bug of not correctly updating the Actual 
*B603827,1                cost in the PosHdr file in case of invoicing by 
*B603827,1                shipment. And fix the bug of displaying a message 
*B603827,1                "record is edit by another user" when selecting 
*B603827,1                shipment and not entereing a style.
*B803689,1 SSE 10/12/2000 Fix bug when voiding an invoice it doesn't check if period is locked
*B603885,4 SSE 11/06/2000 Fix bug of increasing Quantity in Invoice detail screen in 
*B603885,4                Case of Material PO and all the 8 sizes in screen.
*B603885,4                This bug has been fixed in screen only.
*B604017,1 SSE 11/12/2000 Fix bug of asteriks that appear in Multi Approve , Contribution screen
*B604017,1                In case of large numbers in Amount fields. (Prg for documentaion) 
*B803839,1 SSE 12/07/2000 Fix bug of displaying message "Actual cost exceeds Landed cost" in Contribute option.
*B602500,1 SSE 21/09/2000  Prevent user from modify if one or more of vendor
*B602500,1 SSE 21/09/2000  supplies removed
*B604216,1 AKA 15/02/2001  Improve the performance of the edit transaction 
*B803999,1 SSE 03/07/2001 Fix bug of displaying Message that Cost item was not added in the PO.
*C200178,1 AAN 03/15/2001 Add a user define field "CVENPYTYPE" in APINVHDR file for Cathy Daniles 
*B604309,1 AAN 04/23/2001 Update the Due Days when the user change the due date
*B604454,1 AAN 05/07/2001 Adding a warning message to be displayed if the posting date
*B604454,1                is prior to the invoice date.
*B604455,1 SSE 06/05/2001 Fix bug of not adding 2 records for adjustment in ApDist in case of -ve Adjustment.
*B604549,1 SSE 06/17/2001 Fix bug of displaying a negative amount in Shipment type , Duty or freight 
*B604549,1                When pressing Contribute Push button and selecting Budget method.
*B604561,1 SSE 06/17/2001 Add an option to ask user to Proceed/Cancel if Posting date greater than invoice date.
*B604970,1 SSE 06/17/2001 Fixes missing part in Bug 604549.
*B605040,1 AKA 10/17/2001 Fixes different issues. 
*B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
*B605485,1 SSH 11/Feb/2002 Add Validation on user Privileges in adding new.
*B605485,1 SSH 11/Feb/2002 vendor.
*B605612,1 SSH 02/28/2002 Fix the bug of calculate the amount depending on 
*B605612,1 SSH            all style grade not first grade only.
*B605486,1 SSH 26/Feb/2002 Incorect paymnt method in appaymnt file
*B605838,1 SSH Alwayes get the WIP account from the object of WIP account in the
*B605838,1 SSH auto contribution screen.
*B605986,1 TMI [Start] Browse from ApDist such that approved amt <> 0
*B606359,1 SSH [Start] Initiale lcWipAcct.
*B605967,1 RAE 06/09/2002 Do not edit any invoices which have a closed or completed PO.
*B605746,1 RAE 06/13/2002 Check the balance in the APDIST file before saving.
*B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
*B606054,1 ALB 07/14/2002 Control invoice lines and master files update
*B606135,1 ALB 07/21/2002 Update unit cost in bocost file
*B605911,1 ALB 07/27/2002 Fix the contribution process  to save the invoice
*B606137,1 ALB 09/03/2002 Display the cost title in Invoice line in edit and view mode
*B606531,1 ALB 10/10/2002 Fix the invoice saving with po inside shippment order
*E301995,1 Alb 09/10/2002 Add Adorment/Dying/MMFG order to invoice line
*B606373,1 ALB 10/17/2002 Display all information of vendors
*B606002,1 ALB 10/17/2002 Fix rounding problem in the Equ. Actual cost that update from AP
*B606283,1 ALB 10/27/2002 The Approved Amount in invoice line should be <= the inv. amount
*B606432,1 ALB 10/29/2002 Contorl Currency code and rate with Multisession
*B605090,1 ALB 11/11/2002 Keep the program in the same mode and pointed to the posted date 
*B604310,1 ALB 11/13/2002 Fix the due date according to the payment method
*B804535,1 ALB 11/14/2002 Display advanced payments in Option -> Payments
*B606696,1 ALB 12/18/2002 Check that the Document currency same as invoice currency
*C200447,1 ALB 12/24/2002 Change the defaulte WIP Account to LFNM Account (ENGLAND)
*B606949,1 MAN 02/15/2003 Fix variable lnEomDay not Found
*B606980,1 ALB 02/20/2003 Fix error Alias not found when the AP not linked with GL
*B606939,1 KHM 03/20/2003 Fix the bug of update the apdist with the invoice date not the 
*B606939,1                posting date.
*B607161,1 ASH 04/14/2003 1- Fix bug of not generating lines if selecting shipment without recieving session.
*B607161,1                2- Fix bug of not displaying styles that have mfg code in its cost sheet <> mfg code
*B607161,1                   selected in the automatic screen.
*B607095,1 ALB 04/23/2003 Fix problem in AP Invoice lines that tax code applies to all lines
*B607278,1 ALB 06/11/2003 Get the correct WIP account with shipment ticket
*B607158,1 ALB 06/15/2003 Add validation that invoice date could not be after 
*B607158,1                the invoice posted date.
*B607126,1 ALB 06/18/2003 Disable dicount/Adjs fields in case of invoice < 0
*B607361,1 ALB 06/19/2003 Update the apinvtkt with the new amoount in case the cost = 0
*B607353,1 ALB 06/22/2003 Fix bugs in Matirial PO when useing mouse to browse the matiril
*B607341,1 ALB 07/02/2003 Fix Bug happend by bug# B607161,1 and fix the original bug
*B607390,1 ALB 07/03/2003 Fix Bug when removing line from dist. lines that effect in apdist
*B607430,1 ALB 07/20/2003 Get the invoice address from the invoice file not vendor file in edit and view mode
*B607416,1 ALB 07/21/2003 Fix bug when Saving the debit memo againest Return PO
*B607481,1 ALB 08/18/2003 Fix bug in the adjustment record in APDIST
*B121306,1 NNA 01/22/2004 Fix Bug happend by bug# B607341,1 That if the PO module is not 
*B121306,1 NNA            installed and the user try to enter the invoice details using the automatic
*B121306,1 NNA            option error message will occur "Alias POSLN not found"
*B121188,1 MHM 02/25/2004 Enhance Speed of Shipment and contripution in invoice line screen
*B121324,1 TMI 03/17/2004 Do not care with adjustments in deleted records, only sum with new added,modified or selected records
*B122951,1 NNA 05/31/2004 fix bug that you can't Reinput a correct invoice NO# If you're on the invoice Amount
*B122951,1 NNA            Box because system Stop you on the Posted date. this fix Related To B#605090
*B606754,1 MHM 09/13/2004 Fix bug of Cannot Contribute CT on Damages
*B124737,1 NNA 12/16/2004 fix bug that when you choose an invoice and edit it then you try to save
*B124737,1 NNA            it you'll get a massage 'the Journal entries  for the invoice are unbalanced'
*B124737,1 NNA            that because there is a Fraction that prevent from Saving.
*B125114,1 MHM 03/27/2005 fix bug that when you Remove saved line and add another 2 lines one of them disappear
*B129411,1 NNA 09/22/2005 Fix bug that the Ap invoice for Style PO's do not show values if this po is matching to a Shipment
*B128880,1 EIH 03/14/2006 Fix bug if we edit the invoice detail screen in description or note data then we save without 
*B128880,1 EIH 03/14/2006 Saving in apdist file.
*B132275,1 TMI 06/22/2006 Fix the bug that voiding payment while Ap Invoice screen is opened in edit mode
*B040278,1 TMI 07/05/2006 fix a bug that due date in apinvoice is calculated wrong
*B607916,1 TMI 12/28/2006 if the same line in PO is received in more than receiving session then distribute it into separete lines  ( Ticket#T20060915.0001 )
*B608066,1 NNA 04/30/2007 Fix bug that if you made an AP invoice for a vendor that has a currancy X with Exchane rate
*B608066,1 NNA            99 then you changed this currancy from the Screen to be XX with Exchange rate 999 then save
*B608066,1 NNA            you'll find that it saved at the Apdist file with the new currancy but with the old rate in the
*B608066,1 NNA            ncurrunit field
*B608263,1 WAM 09/10/2007 Fix error "Alias POSLN not found" when generate invoice lines automatically for a cutting ticket
*B608688,1 TMI 09/13/2008 Fix a bug that when sending some characters to the invoice search it shows all the invoices
*B608942,1 TMI 07/22/2009 if a record is used by a user check that this user is in SYUSTATC of both A27 and A4XP [T20090224.0001]
***************************************************************************************************************
PARAMETERS lcVenCode,lcInvNo,lnReceipts

PRIVATE laBankObjs 


*B601526,1 Add this line and varible [Begin]
lnFAAp = 0     && Varible to hold the old valeu of laData[51]
*B601526,1 Add this line and varible [End]

llInquiry = IIF(TYPE('lcVenCode') $ 'UL',.F.,.T.)

lnReceipts = IIF(TYPE('lnReceipts') $ 'UL',0,lnReceipts)

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laField_H,laCtrStat

DECLARE laKeyField [2,4],laPayMethd[1,2],laFileStru[1], laBankObjs[3,3],;
        laAddrOrd[3,2],laRemitTo[3,2],laCostType[1],laTermAry[3,2],laTaxAry[1,2],;
        laAddrs[3]
*B604310,1 ALB Fix the due date according to the payment method [Begin]
DECLARE laTermAry[4,2]
*B604310,1 ALB Fix the due date according to the payment method [end]

DECLARE laWndObj[8,3]    && Have screen name & first & last obj. for each screen.

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
STORE .F. TO llDebitM  
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

laDefProc[7]    = .F.    && Use the custom delete procedure.
laDefProc[9]    = .F.    && Use the custom save procedure.

lcDumCol        = SCHEME(1,1)
lcLoclShow      = "lfDelShow" 

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'VENDINV'
laKeyField[1,4] = 1

laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'VENDINV'
laKeyField[2,4] = 2

*B603484,1 SSE 03/20/2000 Define Flag to detect whether we are inside 
*B603484,1                Automatic invoice screen [Begin]
*llAutoScr = Flag to detect if we are in the Automatic invoice screen
llAutoScr = .F.
*B603484,1 SSE 03/20/2000 [End]
*B605967,1 RAE [Start]
STORE .F. TO llClosed
STORE '' TO lcCutTktNo
*B605967,1 RAE [End]
*B605746,1 RAE [Start]
STORE .F. TO llSave
*B605746,1 RAE [End]
************ Documentation for the previos variables ***************

** lcTmpDist    :  Random name for the Distribution Temp. file.
** lcTempSave   :  Random name for the Temp. Cursor to use in save.
** lcDisChld1   :  Random name for the Dist. child screen no 1.
** lcDisChld2   :  Random name for the Dist. child screen no 2.
** lcDisChld3   :  Random name for the Dist. child screen no 3.
** lcDisChld4   :  Random name for the Dist. child screen no 4.
** lcDisChld5   :  Random name for the Dist. child screen no 5.
** lcDisChld6   :  Random name for the Dist. child screen no 6.
** lcAPvInvDt   :  Random name for Invoice lines temp. file 
** lcAPInvTkt   :  Random name for Invoice Applied lines
** lcAutoInv    :  Random name for crtae Invoice lines automatically
** lcTmpApr     :  Random name for crtae Multiple Approve
** lcApplied    :  Random name for Invoice contribution file 
** laOpnFiles   :  Array holds names of files opened
** lcVenInvTypes:  Vendor Invoice Types 
** lcTmpTkt     :  Random name for Ticket details file
  
** lcTPayAcct   :  Variable to hold the word 'non check payment G\L'
** lcTCashAcct  :  Variable to hold the word 'cash payment'
** lcTPayTtl    :  Variable to hold the title of the approved for payment
** lcTCashTtl   :  Variable to hold the title of the cash screen.
** lcTInvAmnt   : 
** lcTInvDate   : 
** lcTVendor    : 
** lcTInvoice   : 
** lcTApAcct    : 
** lcTAprov     : 
** lcTCashPy    : 
** lcTDisc      : 
** lcTInvAmont  : 
** lcTCrdtCor   : 
** lcTDelCor    : 
** lcTVendCode  : 
** lcTVenKey    : Vendor code field logical name (from SYDFIELD) 
** lcTVenInv    : Vendor code  + invoice number fields concatenated
**                logical names (from SYDFIELD) 

** lcDefFact    :  Default factor code (of selected vendor)
** lcCodeFilt   :  Variable to hold the filter criteria.
** lcDivision   :  Variable to hold the display of the division popup.
** lcTermCode   :  Variable to hold the display of the terms popup.
** lcPayMethd   :  Variable to hold the display of the meth. popup.
** lcDistAcct   :  Variable to hold the distribution account.
** lcTaxDes     :  Variable to hold the description of the Tax popup.
** lcRemitTo    :  Say field for remit to popup (DOS)
** lcTaxCode    :  Variable to hold the description of the Tax popup.
** lcData23St   :  Variable to hold the ladata[23] -- enable or disable.

** lcTAprAmnt   :  Variable to hold the word 'Total aproved'
** lcTSave      :  Variable to hold the word 'save' in the screen setup.
** lcTVoid      :  Variable to hold the word 'void' in the screen setup.
** lcTFully     :  Variable to hold the word 'fully' in the screen setup.
** lcTPart      :  Variable to hold the word 'partialy' in the screen setup.
** lcTChldTitl  :  Variable to hold the title of the child screen.
** lcTFactor    :  Variable to hold the factor code for the message.
** lcTInvAmnU   :  Variable to hold 'Invoice amount'.
** lcBrowsTtl   :  Variable to hold the browse title 'Distribution'.

** lcOldPayMth  :  Variable to hold the payment method.
** lcOldVenCr   :  Variable to hold the vendor credit card.
** lcOldVenInv  :  Variable to hold the vendor Invoice No.
** lcOldGLAcc   :  Variable to hold the Old Division.
** lcOldVend    :  Variable to hold the Old vendor.
** lcOldInvNo   :  Variable to hold the Old invoice no.
** lcOldPhone   :  Variable to hold the Old phone.
** lcOldComp    :  Variable to hold the Old company.
** lcOldPrior   :  Variable to hold the Old priority.
** lcOldFact    :  Variable to hold the Old factor code
** lcOldTmplt   :  Variable to hlod the Old template code.
** lcBrowStr    :  Variable to hold the browse string.
** lcTTitle     :  Variable to hold the screen title.
** lcOldVal     :  Variable to hold the old bank or check

** This group of variables is declare specially for the cancel in the **
** approve screen to retrive the old data of the variables.           **

** lcEmptyCode  :
** lcBankCode   :  Variable to hold the bank code.
** lcCheckAcc   :  Variable to hold the check account.
** lcGLAcount   :  Variable to hold the GL account.
** lcAcctDes    :  Variable to hold the account description.
** lcApprPay    :  Variable to hold the Appr. to pay.
** lcApprDis    :  Variable to hold the Appr. Disc.
** lc1099Amnt   :  Variable to hold the approved 1099 amount.
** lcApprAdj    :  Variable to hold the Appr. Adjust.
** lcFisYer     :  Variable to hold the fiscal year of the invoice date.
** lcFisPrd     :  Variable to hold the fiscal period of the invoice date.
** lcPuDiv      :

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
** lcOldCurr    :  Variable to hold the old currency.
** lcAprCurr    :  Variable to hlod the old currency.
*E300296,1 M.H 10/10/95 Add the currency to the AP module.


*E300316,1 HISH 12/06/95. Added currency equation sign variables. (Begin)
** in case of exchange currency from invoice currency to company base currency.
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
** in case of exchange currency from invoice currency to approved currency.
** lcExSin3             Variable to hold the first sign in the equation.
** lcExSin4             Variable to hold the second sign in the equation.
*E300316,1 (End)


** puDivision   :
** lcPuTerm     :
** puTermDes    :
** puTax        :
** puTaxDes     :
**
******** Variable for the link with Aria Apparel System ********
** lcTOperat    :  Variable to hold the title of the operation screen.
** lcTReceipt   :  Variable to hold the title of the receipt screen.
** lcTmpAplCst  :  Random name for Temp.
** lcTAplCost   :  Variable to hold the title of the apply cost screen.

** lcCostTtl    :  Variable to hold the cost browse title
** lcContTtl    :  Variable to hold the cost browse title
** lcFileDir    :  Variable to hold ARIA APPAREL Files DIRECTORY
** lcTAASTtl    : 
** lcFileInUse  : 
** lcCondition  : 
** lcForCond    :  Variable to hold the for condition.

** lnPrice      :  Variable to hold the price.
** lnFreight    :  Variable to hold the freight.
** lnTax        :  Variable to hold the tax.
** lnQuota      :  Variable to hold the quota.
** lnTotRecAss  :  Variable to hold the total receipts assigned.
** lnOldValue   :  Variable to hold the old values.
** lnBrRecNo    :  Browse record number.
** lnCount      :  Variable to hold the counter.
** lnInvRecNo   : 
** lnOldRate    :  Variable to hold the old rate.
** lnExchange   :  Variable to hold the exchange approved rate.  E300296,1
** llConFirm    :  Variable to hold if the zero is confirmed.
** llWareHous   :  Variable to hold if there is a ware house or not.
** llShipment   :  Variable to hold if there is a shipmnet or not.
** llInvCan     : 
** llBrowse     : 
** llNewStat    : 
** llGLActSta   : 
** llSamVendor  : 
** llPaidByCrd  :  Variable to hold if the invoice is paid the credit card.

****************************************************************

** puMethod     : 
** lnPartial    :  Variable to hold the partial.
** lnOldAmnt    :  Variable to hold the old amount of the invoice.
** lnOldDisc    :  Variable to hold the old Discount for the invoice.
** lnInvDisc    :  Variable to hold the old amount of the discount.
** lnTotAppPay  :  Variable to hold the total payment.
** lnOld1099    :  Variable to hold the 1099 amount.
** lnLineNo     :  Variable to hold the line no in the browse.
** lnDueDays    :  Variable to hold the old value of the due days.
** lnDisDays    :  Variable to hold the old value of the discount days.
** lnRemitLen   :  Remit to popup width

** This variable is used in the save procedure only.

** lnOldInvAmt  :  Variable to hold the old invoice amount.
** lnOldDiscOf  :  Variable to hold the old discount offerd amount.
** lnDistAmnt   :  Variable to hold the distribution amount.
** lnInvAdj     :  Variable to hold the invoice adjustement.
** lnApdAmnt    :  Variable to hold the amount paid in the Dist. Screen.
** lnOldRemit   :  Variable to hold the old remit.

** lcOldYear    :  Variable to hold the old fiscal year of the Inv. date.
** lcOldPeriod  :  Variable to hold the old fiscal period of the Inv. date.
** lcSequence   :  Variable to hold the sequence No.
** lcDisAcct    :  Variable to hold the discount account.
** lcOldTaxCod  :  Varibale to hold the old tax code.
 
** ldOldDatInv  :  Variable to hold the old invoice date.
** ldOldData    :  Variable to hold the old invoice date.
** ldOldInvDat  :  Variable to hold the old invoice date.

** llCanSav     :  Logical variable to idicate if the saving process is canceled.
** llLocate     : 
** llInvByCr    :  Logical variable to indicate that the invoice is paid by credit card.
** llFirstShow  : 
** llFromVend   :  Logical variable to indicate if comming from vendor program. 
** llDoLocal    :  Global variable to force the program to use the;
                   show function of the program.

** lcFactStat   : 
** lcRemitStat  :  Variable to hold the object status of the remit.
** lcTermsDisp  :  Variable to hold the display of the terms related fields.
** lcAprStat    : 
** lcDistStat   : 

** puRemitTo    : 
** lnOldRemit   :  Variable to hold the old remit.

******************* End of documentation *************************************************

*B601419,1 This line and Varible was added by HS [Begin]
lcLabel = 'Template :'         && Varible to hold the Template Field Label
*B601419,1 This line and Varible was added by HS [End]

laPayMethd = ''
lcPhone    = SPACE(16)   && Variable to hold the phone no.
lcCompany  = SPACE(30)   && Variable to hold the company name.

STORE ' '       TO lcTmpDist  , lcTempSave , lcDisChld1 , lcDisChld2 ,;
                   lcDisChld3 , lcDisChld4 , lcDisChld5 , lcDisChld6 ,;
                   lcTPayAcct , lcTCashAcct, lcTPayTtl  , lcTCashTtl ,;
                   lcTInvAmnt , lcTInvDate , lcTVendor  , lcTInvoice ,;
                   lcTApAcct  , lcTAprov   , lcTCashPy  , lcTDisc    ,;
                   lcTInvAmont, lcTCrdtCor , lcTDelCor  , lcTVendCode,;
                   lcDefFact  , lcCodeFilt , lcDivision , lcTermCode ,;
                   lcPayMethd , lcDistAcct , lcTaxDes   , lcRemitTo  ,;
                   lcTaxCode  , lcData23St , lcTAprAmnt , lcTSave    ,;
                   lcTVoid    , lcTFully   , lcTPart    , lcTChldTitl,;
                   lcTFactor  , lcTInvAmnU , lcBrowsTtl , lcOldPayMth,;
                   lcOldBank  , lcOldCheck , lcOldVenCr , lcOldVenInv,;
                   lcOldGLAcc , lcOldVend  , lcOldInvNo , lcOldPhone ,;
                   lcOldComp  , lcOldPrior , lcOldFact  , lcOldTmplt ,;
                   lcBrowStr  , lcTTitle   , lcVendor   , lcEmptyCode,;
                   lcBankCode , lcCheckAcc , lcGLAcount , lcAcctDes  ,;
                   lcApprPay  , lcApprDis  , lc1099Amnt , lcApprAdj  ,;
                   lcFisYer   , lcFisPrd   , lcPuDiv    , puDivision ,;
                   lcPuTerm   , puTermDes  , puTax      , puTaxDes   ,;
                   lcOldYear  , lcOldPeriod, lcSequence , lcDisAcct  ,;
                   lcOldTaxCod, lcTmpAplCst, lcTAplCost , lcCostTtl  ,;
                   lcContTtl  , lcFileDir  , lcTAASTtl  , lcFileInUse,;
                   lcCondition, lcForCond  , lcTVenInv  , lcTVenKey  ,;
                   lcOldVal   , laAddrs    , lcTOperat  , lcOldCurr  ,;
                   lcExSin1   , lcExSin2   , lcExSin3   , lcExSin4

* B605040,1 Create temp file from AP dist [Start]
STORE ' ' TO lcTmpDist2   
* B605040,1 Create temp file from AP dist [End]


*E300798,1 Intialize New variables 
STORE ''  TO lcAPvInvDt   && Invoice lines temp. file name
STORE ''  TO lcAPInvTkt   && Invoice Applied lines temp. file name
STORE ''  TO lcAutoInv    && Crtae Invoice lines automatically temp. file name
STORE ''  TO lcTmpApr     && Crtae Multiple Approve temp. file name
STORE ''  TO lcApplied
STORE ''  TO lcTmpTkt     && Create Ticket details Temp. file
STORE .F. TO llZoomInvD   && Zoom Invoice Detail Browse
STORE ''  TO lcMfgGlAcnt  && Operation GL Account
DECLARE laOpnFiles[1]     && Array holds names of files opened
DECLARE laSetups[3,2]     && Array holds system setups
DECLARE laCodes[2,10],laMfgCode[1,2],laTaxCode[1,2]
DECLARE laICstType[1],laMCstType[1],laLCstType[1],laTCstType[1],laMfgRFld[1,2]
STORE '' TO laSetups
lcVenInvTypes = ''        && Vendor Invoice Types 
STORE ''  TO laCodes,laMfgCode,laTaxCode
STORE 1   TO lnMfgOpr,lnTaxCode

*B602238,1 Check if the invoice lines called
STORE .F. TO llUpdInvLn
STORE 0   TO  lnTInvAmt,lnTAprAmt
*E300798,1 (END)

*B601526,1 Change this line (ther is no longer need for lnExchange ) [Begin]
*STORE 0         TO lnPrice    , lnFreight  , lnTax      , lnQuota    ,;
*                   lnTotRecAss, lnOldValue , lnBrRecNo  , lnCount    ,;
*                   lnInvRecNo , puMethod   , lnPartial  , lnOldAmnt  ,;
*                   lnOldDisc  , lnInvDisc  , lnTotAppPay, lnOld1099  ,;
*                   lnLineNo   , lnDueDays  , lnDisDays  , lnRemitLen ,;
*                   lnOldInvAmt, lnOldDiscOf, lnDistAmnt , lnInvAdj   ,;
*                   lnApdAmnt  , lnOldInvAmt, lnOldDiscOf, lnDistAmnt ,;
*                   lnInvAdj   , lnApdAmnt  , lnOldRate  , lnExchange ,;
*                   lnAprRate  , lnAprUnit
                  
STORE 0         TO lnPrice    , lnFreight  , lnTax      , lnQuota    ,;
                   lnTotRecAss, lnOldValue , lnBrRecNo  , lnCount    ,;
                   lnInvRecNo , puMethod   , lnPartial  , lnOldAmnt  ,;
                   lnOldDisc  , lnInvDisc  , lnTotAppPay, lnOld1099  ,;
                   lnLineNo   , lnDueDays  , lnDisDays  , lnRemitLen ,;
                   lnOldInvAmt, lnOldDiscOf, lnDistAmnt , lnInvAdj   ,;
                   lnApdAmnt  , lnOldInvAmt, lnOldDiscOf, lnDistAmnt ,;
                   lnInvAdj   , lnApdAmnt  , lnOldRate  , lnAprRate  ,;
                   lnAprUnit

*B601526,1 Change this line [End]

*B602946,1 Add this line to add some new variables [Begin]

*-- lcActCstHd  Variable to hold random name for the "Actual Cost" temp.
*--             header file
*-- lcActCstDt  Variable to hold random name for the "Actual Cost" temp.
*--             detail file
STORE '' TO lcActCstHd , lcActCstDt

*B602946,1 Add this line to add some new variables [End]

*B602830,1 Add this line to add some new variables [Begin]
*-- lnQtyObj      Variable to hold the object number of the Invoice Quantity
*--               Get Field to be used.
*-- lnApQtyObj    Variable to hold the object number of the Approve Quantity
*--               Get Field to be used.

STORE 0         TO lnQtyObj , lnApQtyObj
*B602830,1 Add this line to add some new variables [End]

STORE .F.       TO llConFirm  , llWareHous , llShipment , ;
                   llInvCan   , llBrowse   , llNewStat  ,;
                   llGLActSta , llSamVendor, llPaidByCrd, llCanSav   ,;
                   llLocate   , llInvByCr  , llFirstShow, llFromVend ,;
                   llFrmScrl

*B602830,1 Add this line to add some new variables [Begin]
*-- llDocNSupp    Flag to know if some of the document types that already
*--               exist in the Invoice are not supplied by the vendor
*--               anymore.

STORE .F.       TO llDocNSupp
*B602830,1 Add this line to add some new variables [End]

STORE ''        TO lcDistYer , lcDistPrd
STORE {}        TO ldOldDatInv, ldOldData  , ldOldInvDat

STORE 'DISABLE' TO lcFactStat , lcRemitStat, lcTermsDisp,;
                   lcAprStat  , lcDistStat , lcTmplate,lcScUpStat,lcScDnStat

STORE 1         TO puRemitTo  , lnOldRemit ,lnStartAdr

*B128880,1 EIH 03/14/2006 Declare llApdistUp variable to determine if we can update apdist file or not [Begin]
STORE .F. TO llApdistUp
*B128880,1 EIH 03/14/2006 [End]



*lcActProg  = IIF(lnReceipts = 0,'APINVPY',IIF(lnReceipts = 1,'APMATPO',IIF(lnReceipts = 2,'APSTYPO','APCUTTK')))
lcActProg   = 'APPYINV'

lcApdGLAct = lcEmptyAcc
lcAccount  = lcEmptyAcc && Variable to hold the AP Account.

lcColorDis = SUBSTR(SCHEME(1,1),ATC('/',SCHEME(1,1))+1)
lcColorDis = lcColorDis+'/'+STRTRAN(lcColorDis,'*','+')
lcColorInv = SCHEME(1,1)

lcSchm7Dis = SCHEME(7,10)
lcSchm7Enb = SCHEME(7,9)

lcDelMesag = "void"   
lcBFactCol = lcHidObjNrm
lcFactCol  = lcHidObjNrm

lcRecTtl   = 'Receipts'
lcOprTtl   = 'Operations Cost' 
lcAAsAcc   = lcEmptyAcc

lcStyCol   = ''  && Variable to be add to the relation to have the ware house.

lnTime     = _DBLCLICK + SECONDS()  && Variable to hold time to consider duble click.
lnPercent  = 0.00  && Variable to hold the old value of the discount percent.

llDoLocal  = .T.   && Global variable to force the program to use the;
                      show function of the program.

*B801499,1 Add these line to add a new variable [Begin]
  **Flag to know if the Posting date falls within a locked period
  llLokPer = .F.
*B801499,1 Add these line to add a new variable [End]

*B605485,1 SSH 11/Feb/2002 [Start] Add Variable for adding new vendor preveli.
llAddVen = .T.
*B605485,1 SSH [END]

*B606949,1 MAN Define the variable 
STORE 0 TO lnEomDay

IF !gfSetup()    
  RETURN
ENDIF

*B605485,1 SSH 11/Feb/2002 [Start] Add Variable for adding new vendor preveli.
llAddVen = lfGetprev()
*B605485,1 SSH [END]

llInquiry = IIF(TYPE('lcVenCode') $ 'UL',.F.,.T.)
IF llInquiry
  lcLoclShow     = 'lpShow'
  laScrMode      = .F.
  laScrMode[2]   = .T.
  FOR lnCounter  = 1 TO 11
    laCtrStat[lnCounter] = 'DISABLE'  && Disable all objects in the control panel
  ENDFOR
  laCtrStat[12]  = 'ENABLE'           && Enable close buttom in the control panel
  lcModal        = 'MODAL'
ELSE
  lcModal        = ''
ENDIF

** Setting the relation between the invoice header and the vendor,
** invoice history and the distribution file.

SELECT APINVHDR

IF !WEXIST(gcBaseWind)
  *E300789,4  AMM Open the FISHD from the righ location
  llOpCmp = gfSysOpen(gcSysHome+'SYCCOMP','Ccomp_id','SH')
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  =gfOpenFile(lcDataDir+'FISHD','Compfyear','SH')
  IF llOpCmp
    =gfCloseFile('SYCCOMP')
  ENDIF
  *E300789,4  AMM end
  llLocate    = .T.
  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcEmptyCode = gfCodDes(' ')    
  lcEmptyCode = gfCodDes(' ' , ' ')
  *E300643,1 Change this line [End]
  *E301077,71 AMM Open file 
  llOpFld = gfOpenFile(gcSysHome+'SYDFIELD','','SH')
  *E301077,71 AMM end
  lcTVenKey   =  ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CVENDCODE',;
                        SYDFIELD.cFld_Name,'CFLD_NAME'))
  lcTVenInv   = lcTVenKey + '\' + ;
                ALLTRIM(LOOKUP(SYDFIELD.cFld_Head, 'CINVNO',;
                        SYDFIELD.cFld_NAme,'CFLD_NAME')) + ' : '
  lcDivision  = lcEmptyCode
  lcPuDiv     = lcEmptyCode
  lcTermCode  = lcEmptyCode
  lcPuTerm    = lcEmptyCode

  laTermAry[1,1] = 'NTERDISCR'
  laTermAry[1,2] = 'laData[16]'  
  laTermAry[2,1] = 'NTERDUED'
  laTermAry[2,2] = 'laData[19]'  
  laTermAry[3,1] = 'NTERDISCD'
  laTermAry[3,2] = 'laData[24]'  

  *B604310,1 ALB Fix the due date according to the payment method [Begin]
  lnEomDay = 0
  laTermAry[4,1] = 'EOMDAY'
  laTermAry[4,2] = 'lnEomDay'
  *B604310,1 ALB Fix the due date according to the payment method [end]

  laTaxAry[1,1]  = 'CGLINPACCT'
  laTaxAry[1,2]  = 'lcApdGLAct'  

  ** Prepare an array to hold bank objects to be used for
  ** global bank and checking accounts validations as follows :
  ** One row for every object, such that 
  ** row no. 1 holds bank object names,
  ** row no. 2 holds checking account object names
  ** row no. 3 holds the corresponding G/L account object names,
  ** Columns are ordered as follows :
  ** Column no. 1 : invisible button name for corresponding object
  ** Column no. 2 : object name (e.g. bank object name)
  ** Column no. 3 : object description name(if required)

  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'laData[27]'     && Bank code 
  laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
  laBankObjs[2,2] = 'laData[28]'     && Checking account 
  laBankObjs[3,1] = 'ibGlAcc'        && G/L account invisible button  
  laBankObjs[3,2] = 'laData[29]'     && G/L account
 
  LOCATE  && To Refresh the relation

  IF llInquiry
    lcTTitle = lcWindTitl
  ELSE
    IF EMPTY(lcSequence)

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcSequence = PADL(ALLTRIM(STR(gfSequence('APSESS',1))),;
      *             FSIZE('cApSessNo','APDIST'),'0')
      lcSequence = gfSequence('CAPSESSNO')
      *E300663,1 Change this line [End]
      
    ENDIF
    lcTTitle = PADR(SPACE((75-LEN(lcWindTitl))/2)+lcWindTitl+SPACE((74-LEN(lcWindTitl))/2-20)+'Session : '+ALLTRIM(lcSequence),76)
  ENDIF  
  
  llFirstTime = .T.
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  ** Prepare Remit to array from SYDFIELD and get its maximum width
  lnRemitLen   = gfGetVld('cInvRemit',@laRemitTo)
  lcRemitTo    = laRemitTo[1,1]   

  =gfGetVld('CVENPMETH',@laPayMethd)
  *
  IF llOpFld
    =gfCloseFile('SYDFIELD')
  ENDIF
  * end
  
  lcTmpDist   = gfTempName()   && Random name for the Distribution Temp. file.
  lcTempSave  = gfTempName()   && Random name for the Temp. Cursor to use in save.
  lcDisChld1  = gfTempName()   && Random name for the Dist. child screen no 1.
  lcDisChld2  = gfTempName()   && Random name for the Dist. child screen no 2.
  lcDisChld3  = gfTempName()   && Random name for the Dist. child screen no 3.
  lcDisChld4  = gfTempName()   && Random name for the Dist. child screen no 4.
  lcDisChld5  = gfTempName()   && Random name for the Dist. child screen no 5.
  lcDisChld6  = gfTempName()   && Random name for the Dist. child screen no 6.

  * B605040,1 Create temp file from AP dist [Start]
  lcTmpDist2   = gfTempName()   && Random name for the Distribution Temp. file.
  * B605040,1 Create temp file from AP dist [End]
  

  SELECT APDIST
  =AFIELDS(laFileStru)
  ** Creat temp  file from the array.
  CREATE TABLE &gcWorkDir.&lcTempSave FROM ARRAY laFileStru

  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+5,4]

  laFileStru[lnFileStru+1,1] = 'CDISCRIP'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 60
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CSTATUS'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  
  laFileStru[lnFileStru+3,1] = 'CUPDSERNO'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0
  
  laFileStru[lnFileStru+4,1] = 'NRECNO'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 10
  laFileStru[lnFileStru+4,4] = 0

  laFileStru[lnFileStru+5,1] = 'CTAXDES'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 8
  laFileStru[lnFileStru+5,4] = 0

  ** Creat temp ditribution file from the array.
  CREATE TABLE &gcWorkDir.&lcTmpDist FROM ARRAY laFileStru

  *E300798,1 Create index to temp dist file
  INDEX ON cVendCode+cInvNo+STR(nApDLinNo,4) TAG (lcTmpDist)
  *E300798,1 (End)

  


  *E300798,1 Declare Operations related fields
  laMfgRFld[1,1] = 'GLACCOUNT'
  laMfgRFld[1,2] = 'lcMfgGlAcnt'

  *E300798,1 Get importing style costing elements
  IF 'PO' $ gcCmpModules
    STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,;
                M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5
    =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5',gcAct_Comp)
    lnElements = 0
    FOR lnCount = 1 TO 5
      IF !INLIST(EVAL('M_CITYPE'+STR(lnCount,1)),'F','S','T')  
        lnElements = lnElements + 1
        DIMENSION laICstType[lnElements,3]
        laICstType[lnElements,1] = EVAL('M_CISLBL'+STR(lnCount,1))
        laICstType[lnElements,2] = STR(lnCount,1)
        laICstType[lnElements,3] = EVAL('M_CITYPE'+STR(lnCount,1))
      ENDIF
    ENDFOR
  ENDIF
  *E300798,1 Get Manufacturing style costing elements
  IF 'MF' $ gcCmpModules
    *IF 'A'$ ALLTRIM(APVENDOR.cVenInvTyp) &&alb
      DIMENSION laACstType[1,3]
      laACstType[1,1] = 'Adorment Cost'
      laACstType[1,2] = '2'
      laACstType[1,3] = 'A'
    *ENDIF

    STORE '' TO M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,;
                M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5
    =gfGetMemVar('M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5',gcAct_Comp)
    lnElements = 0
    FOR lnCount = 1 TO 5
      IF !INLIST(EVAL('M_CMTYPE'+STR(lnCount,1)),'F','S','T')
        lnElements = lnElements + 1
        DIMENSION laMCstType[lnElements,3]
        laMCstType[lnElements,1] = EVAL('M_CMSLBL'+STR(lnCount,1))
        laMCstType[lnElements,2] = STR(lnCount,1)
        laMCstType[lnElements,3] = EVAL('M_CMTYPE'+STR(lnCount,1))
      ENDIF
    ENDFOR
  ENDIF
  *E300798,1 If Material Module is Installed
  IF 'MA' $ gcCmpModules
    *E300798,1 Force Importing Materials Costing Elements
    DIMENSION laLCstType[4,3]
    laLCstType[1,1] = 'Purchase Price'
    laLCstType[1,2] = '1'
    laLCstType[1,3] = 'P'
    laLCstType[2,1] = 'Freight'
    laLCstType[2,2] = '2'
    laLCstType[2,3] = 'D'
    laLCstType[3,1] = 'Tax'
    laLCstType[3,2] = '3'
    laLCstType[3,3] = 'D'
    laLCstType[4,1] = 'Quota'
    laLCstType[4,2] = '4'
    laLCstType[4,3] = 'D'
    *E300798,1 Get Manufacturing Materials Costing Elements
    STORE '' TO M_CTSLBL1,M_CTSLBL2,M_CTSLBL3,M_CTSLBL4,;
                M_CTTYPE1,M_CTTYPE2,M_CTTYPE3,M_CTTYPE4
                
                     
    =gfGetMemVar('M_CTSLBL1,M_CTSLBL2,M_CTSLBL3,M_CTSLBL4,;
                  M_CTTYPE1,M_CTTYPE2,M_CTTYPE3,M_CTTYPE4',gcAct_Comp)
    lnElements = 0
    FOR lnCount = 1 TO 4

      *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
      *B605040,1 Displaing the "Mfg.Oper" and "Misc." only if the Documnet type is "Material MFG order" [Start]
      *IF !INLIST(EVAL('M_CTTYPE'+STR(lnCount,1)),'F','S','T')
      IF !INLIST(EVAL('M_CTTYPE'+STR(lnCount,1)),'F','S','T','A','D') AND !EMPTY(EVAL('M_CTTYPE'+STR(lnCount,1))) 
      *B605040,1  Displaing the "Mfg.Oper" and "Misc." only if the Documnet type is "Material MFG order" [End]
      *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
        lnElements = lnElements + 1
        DIMENSION laTCstType[lnElements,3]
        laTCstType[lnElements,1] = EVAL('M_CTSLBL'+STR(lnCount,1))
        laTCstType[lnElements,2] = STR(lnCount,1)
        laTCstType[lnElements,3] = EVAL('M_CTTYPE'+STR(lnCount,1))
      ENDIF
    ENDFOR
  ENDIF
  
  *E300798,1 Get needed system setup
  laSetups[1,1] = 'M_DYELOT'
  laSetups[2,1] = 'M_MATDYE'
  laSetups[3,1] = 'M_LINK_GL'
  STORE '' TO M_DYELOT,M_MATDYE,M_LINK_GL
  =gfGetMemVar('M_DYELOT,M_MATDYE,M_LINK_GL',gcAct_Comp)
  laSetups[1,2] = M_DYELOT
  laSetups[2,2] = M_MATDYE
  laSetups[3,2] = M_LINK_GL
  *=gfGetMemVar(@laSetups,gcAct_Comp)

  *E300798,1 Declare Codes Array
  laCodes[1,1] = "MFGCODE"
  laCodes[1,2] = "laMfgCode"
  laCodes[1,3] = "lnMfgOpr"
  laCodes[1,4] = ""
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,9] = [cVendCode+cApInvNo+cAPVILNo]
  laCodes[1,10] = "COPRCODE"

  laCodes[2,1] = "CTAXCODE"
  laCodes[2,2] = "laTaxCode"
  laCodes[2,3] = "lnTaxCode"
  laCodes[2,4] = ""
  laCodes[2,5] = .F.
  laCodes[2,6] = .T.
  laCodes[2,9] = [cVendCode+cApInvNo+cAPVILNo]
  laCodes[2,10] = "CTAXCODE"

  *E300798,1 Get Invoice lines temp. file name
  lcAPvInvDt = gfTempName()   && Random name for Invoice lines temp. file 
  lcAPInvTkt = gfTempName()   && Random name for Invoice Applied lines temp. file 
  lcAutoInv  = gfTempName()   && Random name for Invoice lines automatically temp. file name
  lcTmpApr   = gfTempName()   && Random name for multiple approve temp file.
  lcApplied  = gfTempName()
  lcTmpTkt   = gfTempName()   && Random name for Ticket details file
  
  *B602946,1 Add these lines to get random names for the "Actual Cost" temp.
  *          header and detail files [Begin]
  lcActCstHd = gfTempName()   && Random name for the "Actual Cost" temp. header file
  lcActCstDt = gfTempName()   && Random name for the "Actual Cost" temp. detail file
  *B602946,1 Add these lines to get random names for the "Actual Cost" [End]
  
  *E300798,1 Create Invoice lines temp. file
  *MAN Change the decimal place from 3 to 2 for nTktAmnt,nInvAmnt,nAplAmnt 
  *CREATE TABLE (gcWorkDir+lcAutoInv) ;
  (cSelect C(1), Item C(19),Color C(6),cWareCode C(6), cDyelot C(10),LineNo N(6),;
   cTktNo C(6),nTktQty1 N(6),nTktQty2 N(6),nTktQty3 N(6),nTktQty4 N(6),nTktQty5 N(6),;
   nTktQty6 N(6),nTktQty7 N(6),nTktQty8 N(6), nTktQty N(7),nTktCst N(13,3),;
   nTktAmnt N(14,3), nAplQty1 N(6),nAplQty2 N(6),nAplQty3 N(6),nAplQty4 N(6),;
   nAplQty5 N(6),nAplQty6 N(6),nAplQty7 N(6),nAplQty8 N(6),nAplQty N(7), nAplAmnt N(14,3),;
   nInvQty1 N(6),nInvQty2 N(6),nInvQty3 N(6),nInvQty4 N(6), nInvQty5 N(6),;
   nInvQty6 N(6),nInvQty7 N(6),nInvQty8 N(6), nInvQty N(7), nInvAmnt N(14,3),cRecvType C(1))
   
  *B602830,1 Change this line to add the decimal place for the fields
  *          nTktQty, nInvQty and nAplQty, to be able to add the decimal
  *          place to the "Quantity" fields in the case of document types
  *          Material PO, Material PO Return and Material MFG Order [Begin]
  *CREATE TABLE (gcWorkDir+lcAutoInv) ;
  *(cSelect C(1), Item C(19),Color C(6),cWareCode C(6), cDyelot C(10),LineNo N(6),;
  * cTktNo C(6),nTktQty1 N(6),nTktQty2 N(6),nTktQty3 N(6),nTktQty4 N(6),nTktQty5 N(6),;
  * nTktQty6 N(6),nTktQty7 N(6),nTktQty8 N(6), nTktQty N(7),nTktCst N(13,3),;
  * nTktAmnt N(14,2), nAplQty1 N(6),nAplQty2 N(6),nAplQty3 N(6),nAplQty4 N(6),;
  * nAplQty5 N(6),nAplQty6 N(6),nAplQty7 N(6),nAplQty8 N(6),nAplQty N(7), nAplAmnt N(14,2),;
  * nInvQty1 N(6),nInvQty2 N(6),nInvQty3 N(6),nInvQty4 N(6), nInvQty5 N(6),;
  * nInvQty6 N(6),nInvQty7 N(6),nInvQty8 N(6), nInvQty N(7), nInvAmnt N(14,2),cRecvType C(1))
  *aminamin
  *CREATE TABLE (gcWorkDir+lcAutoInv) ;
  *(cSelect C(1), Item C(19),Color C(6),cWareCode C(6), cDyelot C(10),LineNo N(6),;
  * cTktNo C(6),nTktQty1 N(6),nTktQty2 N(6),nTktQty3 N(6),nTktQty4 N(6),nTktQty5 N(6),;
  * nTktQty6 N(6),nTktQty7 N(6),nTktQty8 N(6), nTktQty N(11,3),nTktCst N(13,3),;
  * nTktAmnt N(14,2), nAplQty1 N(6),nAplQty2 N(6),nAplQty3 N(6),nAplQty4 N(6),;
  * nAplQty5 N(6),nAplQty6 N(6),nAplQty7 N(6),nAplQty8 N(6),nAplQty N(11,3), nAplAmnt N(14,2),;
  * nInvQty1 N(6),nInvQty2 N(6),nInvQty3 N(6),nInvQty4 N(6), nInvQty5 N(6),;
  * nInvQty6 N(6),nInvQty7 N(6),nInvQty8 N(6), nInvQty N(11,3), nInvAmnt N(14,2),cRecvType C(1))
  **B602830,1 Change this line to add the decimal place for the fields [End]
  
  CREATE TABLE (gcWorkDir+lcAutoInv) ;
  (cSelect C(1), Item C(19),Color C(6),cWareCode C(6), cDyelot C(10),LineNo N(6),;
   cTktNo C(6),nTktQty1 N(6),nTktQty2 N(6),nTktQty3 N(6),nTktQty4 N(6),nTktQty5 N(6),;
   nTktQty6 N(6),nTktQty7 N(6),nTktQty8 N(6), nTktQty N(11,3),nTktCst N(13,3),;
   nTktAmnt N(14,2), nAplQty1 N(6),nAplQty2 N(6),nAplQty3 N(6),nAplQty4 N(6),;
   nAplQty5 N(6),nAplQty6 N(6),nAplQty7 N(6),nAplQty8 N(6),nAplQty N(11,3), nAplAmnt N(14,2),;
   nInvQty1 N(6),nInvQty2 N(6),nInvQty3 N(6),nInvQty4 N(6), nInvQty5 N(6),;
   nInvQty6 N(6),nInvQty7 N(6),nInvQty8 N(6), nInvQty N(11,3), nInvAmnt N(14,2),cRecvType C(1),cWIPAcct C(24) )

  
  *AMINAMIN   
  
  INDEX ON cSelect+Item+cWareCode+cDyelot TAG 'SELECT'
  INDEX ON Item+cWareCode+cDyelot+cTktNo TAG (lcAutoInv)  ADDITIVE

  *E300798,1 Create Invoice lines temp. file
  SELECT APVINVDT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'cStatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'NRECNO'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 5
  laFileStru[lnFileStru+2,4] = 0
  
  *B607916,1 TMI [Start] Add "CRSESSION" field
  lnFileStru = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'CRSESSION'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  *B607916,1 TMI [End  ] 

  CREATE TABLE &gcWorkDir.&lcAPvInvDt FROM ARRAY laFileStru
  
  *B607916,1 TMI [Start] Add the field "CRSESSION" to the temp key
  *INDEX ON cIMTyp+cTktNo+cLotNo+STR(LineNo,6)+cBomType+cOprCode+Item+Color+cDyelot TAG ITEM
  INDEX ON cIMTyp+cTktNo+cLotNo+STR(LineNo,6)+cBomType+cOprCode+Item+Color+cDyelot+CRSESSION TAG ITEM
  *B607916,1 TMI [End  ] 
  
  INDEX ON cVendCode+cApInvNo+cAPVILNo TAG (lcAPvInvDt) ADDITIVE
  INDEX ON cApDGlAct TAG ADJUST ADDITIVE

  *E300798,1 Create Invoice Applied lines temp. file
  SELECT APINVTKT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'cStatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'NRECNO'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 5
  laFileStru[lnFileStru+2,4] = 0
  CREATE TABLE &gcWorkDir.&lcAPInvTkt FROM ARRAY laFileStru
  INDEX ON cIMTyp+cTktNo+STR(LineNo,6)+cRSession TAG Ticket
  INDEX ON cVendCode+cApInvNo+cApVILNo+cRSession TAG (lcAPInvTkt) ADDITIVE
  
  *E300798,1 (End)
  
ELSE
  IF lnInvRecNo < RECCOUNT() .AND. lnInvRecNo > 0
    GO lnInvRecNo
  ENDIF
  
  llFirstShow= .T.

  IF laData[3] = 'C' .AND. laScrmode[2]
    laCtrStat[7]  = "DISABLE"                && Edit button 
  ENDIF
  IF !EMPTY(laData[3])
    lcPayMethd = laPayMethd[AT(laData[3],'PMNCH'),1]
    puMethod   = AT(laData[3],'PMNCH')
  ENDIF  
  *E300789,4  AMM No need to this seek because of the new structure
  *IF SEEK(gcPrnt_cmp,'SYCCOMP')
  *E300789,4  AMM end
  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
    *IF SEEK(gcPrnt_cmp+laData[32],'SYCFISHD')
      *IF SYCFISHD.CFISYSTAT <> 'H';
        .AND. laData[3] <> 'C';
        .AND. APINVHDR.CINVSTAT <> 'A';
        .AND. laScrMode[2]
  *E300789,4  AMM Adjust to fit the new index
  *IF SEEK(gcPrnt_cmp+laData[32],'FISHD')      
  IF SEEK(laData[32],'FISHD')      
  *E300789,4  AMM end
    IF FISHD.CFISYSTAT <> 'H';
      .AND. laData[3] <> 'C';
      .AND. APINVHDR.CINVSTAT <> 'A';
      .AND. laScrMode[2]
  *E300692,1 end   
      laCtrStat[7]  = "ENABLE"                && Edit button 
      laCtrStat[8]  = "ENABLE"                && Delete button
    ELSE
      laCtrStat[7]  = "DISABLE"               && Edit button 
      laCtrStat[8]  = "DISABLE"               && Delete button
    ENDIF
  ELSE
    laCtrStat[7]  = "DISABLE"                 && Edit button 
    laCtrStat[8]  = "DISABLE"                 && Delete button
  ENDIF  
  *E300789,4  AMM start
  *ENDIF
  *E300789,4  AMM end
  puRemitTo  = lnOldRemit
ENDIF

*B600610,1 START
IF _WINDOWS
  IF EMPTY(laData[15]) 

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *lcPuTerm = SYCCODES.cdiscrep
    *E300789,4  AMM Default by an empty code
    *lcPuTerm = CODES.cdiscrep
    lcPuTerm = lcEmptyCode
    *E300789,4  AMM end
    *E300643,1 Change this line [End]
  ENDIF

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puTermDes PROMPT field SYCCODES.cdiscrep scroll;
  *  FROM 10.83,2.25 TO 18.83,35.35 ;
  *  MESSAGE gfObj_msg()

  DEFINE POPUP puTermDes PROMPT field CODES.cdiscrep scroll;
    FROM 10.83,2.25 TO 18.83,35.35 ;
    MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  
  ON SELECTION POPUP puTermDes DO lfvTermCode
  IF EMPTY(laData[4])

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *lcPuDiv = SYCCODES.cdiscrep
    *E300789,4  AMM Default by an empty code
    *lcPuDiv = CODES.cdiscrep
    lcPuDiv = lcEmptyCode
    *E300789,4  AMM end
    *E300643,1 Change this line [End]
    
  ENDIF

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puDivision PROMPT field SYCCODES.cdiscrep scroll;
  *  FROM 14.00,2.25 TO 22.00,37.00 ;
  *  MESSAGE gfObj_msg()

  DEFINE POPUP puDivision PROMPT field CODES.cdiscrep scroll;
    FROM 14.00,2.25 TO 22.00,37.00 ;
    MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  
  ON SELECTION POPUP puDivision DO lfvDivision

  *E300643,1 Change this lines for the changes we have made 
  *          to SYCCODES [Begin]
  *puTax = SYCCODES.cdiscrep
  *
  *DEFINE POPUP puTaxDes PROMPT field SYCCODES.cdiscrep scroll;
  *  FROM 2.35,38.00 TO 9.35,69.30 ;
  *       MESSAGE gfObj_msg()

  puTax = CODES.cdiscrep

  DEFINE POPUP puTaxDes PROMPT field CODES.cdiscrep scroll;
    FROM 2.35,38.00 TO 9.35,69.30 ;
         MESSAGE gfObj_msg()
  *E300643,1 Change this lines [End]
  
  ON SELECTION POPUP puTaxDes DO lfvTaxCode
ENDIF  
*B600610,1 END

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET ORDER  TO TAG IDRLTFNAME
*E300789,4  AMM Adjust to fit the new structure
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+lcCodeFilt) OR cFld_Name ='N/A'
*SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+lcCodeFilt) OR cFld_Name ='N/A'
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+lcCodeFilt) OR CDEFCODE+CRLTFIELD+CFLD_NAME = ' '+'N'+'N/A'
*E300789,4  AMM end
SELECT APINVHDR
SET ORDER TO TAG VENDINV

SET FILTER TO CINVSTAT <> 'V'

SET RELATION TO laData[1] INTO APVENDOR ADDITIVE
*E301077,71 AMM Comment out, the file hasn't opened yet.
*SET RELATION TO APINVHDR.CVENDCODE + APINVHDR.CFISFYEAR INTO APVENHST ADDITIVE
*E301077,71 AMM end
SET RELATION TO APINVHDR.CINVNO+APINVHDR.CVENDCODE INTO APDIST ADDITIVE
SET RELATION TO laData[4] INTO APDIV ADDITIVE

SELECT APDIST
SCATTER MEMVAR MEMO

SELECT APINVHDR

IF llLocate
  GO BOTTOM
  IF !EOF()
    SKIP
  ENDIF  
  llLocate = .F.
ENDIF
  
laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "IBVENDOR"
laWndObj [1,3] = "PBNOTE"

laWndObj [2,1] = lcDisChld1
laWndObj [2,2] = "laData[33]"
laWndObj [2,3] = "PBCLOSE"

laWndObj [3,1] = lcDisChld3
laWndObj [3,2] = "laData[33]"
laWndObj [3,3] = "PBCLOSE"

laWndObj [4,1] = lcDisChld4
laWndObj [4,2] = "laData[33]"
laWndObj [4,3] = "PBCLOSE"

laWndObj [5,1] = lcDisChld5
laWndObj [5,2] = "laData[33]"
laWndObj [5,3] = "PBCLOSE"

laWndObj [6,1] = lcDisChld6
laWndObj [6,2] = "laData[33]"
laWndObj [6,3] = "PBCLOSE"

laWndObj [7,1] = "CWRAPDISTR"
laWndObj [7,2] = "laData[33]"
laWndObj [7,3] = "PBCLOSE"

laWndObj [8,1] = "GWCCONTRL1"
laWndObj [8,2] = "PBTOP"
laWndObj [8,3] = "PBCLS"

PUSH KEY

ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL Ctrl+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1 
ON KEY LABEL CTRL+END   lnDummy = 1 
ON KEY LABEL Ctrl+ENTER lnDummy = 1

*E300807,1 Add these line to Call the function that activate the popup [Begin]
=lfActPInq()
*E300807,1 Add these line to Call the function that activate the popup [End]

*E300798,1 Add new push button for invoice lines
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbAPVInvDt'
laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
laPanelObj[1,3] = [VALID lfvInvDt() MESSAGE 'Invoice Details' DISABLE]
*E300798,1 (End)

*E300683,1 Call *.SPR from screens directory
* DO APPYINV.SPR
DO (gcScrDir + gcWinAppl + '\APPYINV.SPR')
*E300683,1 end
    
*E300807,1 Add these lines to Release the popup [Begin]
RELEASE POPUPS _INQURYPOP
RELEASE PAD _INQUIRY OF _MSYSMENU
*E300807,1 Add these lines to Release the popup [End]

RELEASE WINDOW (lcBrowsTtl)
RELEASE PAD _BROWSE OF _MSYSMENU
RELEASE POPUPS puDivision,puTermDes,puTaxDes

POP KEY

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO 

SELECT APINVHDR
SET FILTER TO
SET RELATION TO

lnInvRecNo = RECNO()

IF glQuitting

  RELEASE WINDOW CWRAPDISTR

  SELECT APINVHDR
  SET RELATION TO

  IF USED(lcTmpDist)  && We are going to close the Temp codes & Erase it.
    SELECT(lcTmpDist)
    USE IN &lcTmpDist
  ENDIF
  ERASE &gcWorkdir.&lcTmpDist..DBF  && Erase the Temp file.
  ERASE &gcWorkdir.&lcTmpDist..CDX  && Erase the Temp index.

  IF USED(lcTempSave)  && We are going to close the Temp codes & Erase it.
    SELECT(lcTempSave)
    USE IN &lcTempSave
  ENDIF
  ERASE &gcWorkdir.&lcTempSave..DBF  && Erase the Temp file.
  ERASE &gcWorkdir.&lcTempSave..CDX  && Erase the Temp index.

  *E300798,1 Erase Invoice lines temp. file
  IF USED(lcAPvInvDt)
    USE IN (lcAPvInvDt)
  ENDIF  
  ERASE &gcWorkDir.&lcAPvInvDt..DBF
  ERASE &gcWorkDir.&lcAPvInvDt..CDX

  *E300798,1 Erase Invoice Applied lines temp. file
  IF USED(lcAPInvTkt)
    USE IN (lcAPInvTkt)
  ENDIF  
  ERASE &gcWorkDir.&lcAPInvTkt..DBF
  ERASE &gcWorkDir.&lcAPInvTkt..CDX
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcAutoInv)
    USE IN (lcAutoInv)
  ENDIF  
  ERASE &gcWorkDir.&lcAutoInv..DBF
  ERASE &gcWorkDir.&lcAutoInv..CDX

  IF USED(lcTmpApr)
    USE IN (lcTmpApr)
  ENDIF  
  ERASE &gcWorkDir.&lcTmpApr..DBF
  ERASE &gcWorkDir.&lcTmpApr..CDX

  IF USED(lcApplied)
    USE IN (lcApplied)
  ENDIF  
  ERASE &gcWorkDir.&lcApplied..DBF
  ERASE &gcWorkDir.&lcApplied..CDX
  
  IF USED(lcTmpTkt)
    USE IN (lcTmpTkt)
  ENDIF  
  ERASE &gcWorkDir.&lcTmpTkt..DBF
  ERASE &gcWorkDir.&lcTmpTkt..CDX
  *B602179,1 AMM  end
ENDIF  
*E300798,1 Close Files Opened by Invoice lines screen.
IF !EMPTY(laOpnFiles)
  FOR lnCount = 1 TO ALEN(laOpnFiles)
    IF USED(laOpnFiles[lnCount])
      USE IN laOpnFiles[lnCount]
    ENDIF  
  ENDFOR
ENDIF
*E300798,1 (End)
glFromBrow  = .F.

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

*B605967,1 RAE
llClosed = .F.
*B605967,1 RAE
*E300798,1 Codes alternate fiels
IF !EMPTY(lcVenInvType)
  laCodes[1,7] = IIF(laScrMode[2],'APVINVDT',lcAPvInvDt)
  laCodes[1,8] = IIF(laScrMode[2],'ORGVINV',lcAPvInvDt)
  laCodes[2,7] = IIF(laScrMode[2],'APDIST',lcTmpDist)
  laCodes[2,8] = IIF(laScrMode[2],'APDIST',lcTmpDist)
ENDIF
*E300798,1 Enable Invoice Lines

*B801894,1 Disable Invoice lines option when the vendor supply only cutting 
*B801894,1 ticket and invoicing with forign currency.
*IF laScrMode[1] OR EMPTY(lcVenInvType)
IF laScrMode[1] OR EMPTY(lcVenInvType) OR ;
   ('M'=lcVenInvType AND gfGetMemVar('LLMULCURR') AND PADR(ladata[44],3) <> PADR(gcBasecurr,3))
*B801894,1 (End)
  SHOW GET pbAPVInvDt DISABLE
ELSE
  SHOW GET pbAPVInvDt ENABLE
ENDIF
*E300798,1 (End)

*B801499,1 Add these lines to get the Posting date period lock status
*in the Edit mode [Begin]

*B801499,1 If Edit mode
IF laScrMode[3]
  =lfVlDate(gcPrnt_Cmp , @lcDistPrd , @lcDistYer , laData[50] , @llLokPer)
ENDIF    && End of IF laScrMode[3]

*B801499,1 If Select mode or Add mode
IF laScrMode[1] .OR. laScrMode[4]
  llLokPer = .F.
ENDIF    && End of IF laScrMode[1] .OR. laScrMode[4]

*B801499,1 Add these lines to get the Posting date period lock status [End]

IF llInquiry
  laData[1]      = lcVenCode
  laData[2]      = lcInvNo
  =SEEK(laData[1]+laData[2],'APINVHDR')
  SCATTER FIELDS &lcScFields MEMO TO laData
  IF _DOS
    SHOW GET pbDlt,1 PROMPT '\<Void' DISABLE  
  ELSE
    SHOW GET pbDlt DISABLE  
  ENDIF  
  SHOW GET pbVendor DISABLE
ENDIF

** Replace the 'Delete' to 'Void' in the control panel **
** without enter the show procedure.                   **

IF llFirstShow
  llFirstShow= .F.
  *B606432,1 ALB Contorl Currency code and rate with Multisession [Begin]
  IF laScrMode[2] .OR. laScrMode[3]
    SHOW GET laData[44] DISABLE
    SHOW GET ibCurrency DISABLE
    SHOW GET laData[45] DISABLE
  ENDIF
  *B606432,1 ALB Contorl Currency code and rate with Multisession [end]
  
  =lfDelShow()
  ** Check if return from vendor program after adding a new vendor code.
  IF llFromVend
    llFromVend = .F.
    KEYBOARD "{ENTER}"
  ENDIF  
  RETURN
ENDIF

DO CASE
  CASE laScrMode[1]  && Select Mode
    *B602238,1 Check if the invoice lines called
    llUpdInvLn = .F.
    
    lnStartAdr=1
    =lfShiftlaData(1)
    STORE  'DISABLE' TO lcScUpStat,lcScdNStat
    SHOW GET ibshiftup DISABLE    
    SHOW GET ibshiftDn DISABLE        
       
    STORE 'DISABLE' TO lcData23St,lcFactStat,lcAprStat,lcDistStat,;
                       lcTermsDisp,lcRemitStat

    SELECT(lcTmpDist)
    ZAP

    IF WVISIBLE('CWRAPDISTR')
      =gfChClose('CWRAPDISTR')  
    ENDIF

    SELECT APINVHDR

    ** If the child window is open so we are going to close it because
    ** there is no need to live it open in the select mode.

    IF llSamVendor
      laData[1]   = ALLTRIM(lcVendor)
      SHOW GET laData[1]
      _CUROBJ = OBJNUM(laData[2])
      llSamVendor = .F.
    ELSE
      lcPhone     = SPACE(16) && Variable to hold the phone no.
      lcCompany   = SPACE(30) && Variable to hold the company name.
      LOCATE
    ENDIF    

    lcRemitTo   = laRemitTo[1,1]

    STORE ' '        TO lcPayMethd,lcOldYear,lcOldPeriod

    STORE lcEmptyAcc TO lcDisAcct, lcApdGLAct,laData[29], laData[33], lcAccount

    *B601526,1 Change this line (ther is no longer need for lnExchange ) [Begin]
    *STORE 0          TO lnTotAppPay,lnOldInvAmt,lnOldDiscOf,lnDistAmnt,lnInvAdj,;
    *                    lnLineNo,puMethod,lnExChange

    STORE 0          TO lnTotAppPay,lnOldInvAmt,lnOldDiscOf,lnDistAmnt,lnInvAdj,;
                        lnLineNo,puMethod
    *B601526,1 Change this line [End]

    STORE 1          TO lnOldRemit,puRemitTo

    *B600880,1 M.H 12/11/95 Fix the problem of returnning to the select mode after selecting P.O. receipts.
    *STORE .F.        TO llGLActSta,llCanSav,llInvByCr,llPaidByCrd
    STORE .F.        TO llGLActSta,llCanSav,llInvByCr,llPaidByCrd,llInvCan
    *B600880,1 M.H End.

    ldOldDatInv = {}        && Variable to hold the old invoice date.

    SHOW GET ibVendor  ENABLE
    SHOW GET ibPhone   ENABLE
    SHOW GET ibCompany ENABLE
    SHOW GET ibInvoice ENABLE
    
    SHOW GET ibTaxCode DISABLE COLOR ,,,,,,,,&lcColorInv,&lcColorInv
    SHOW GET puTax DISABLE
    SHOW GET pbApprove,1 PROMPT '\<Approve for payment...' &lcAprStat
    STORE lcEmptyCode TO lcDivision,lcPuDiv,lcTermCode,lcPuTerm

  CASE laScrMode[2] .OR. laScrMode[3]  && View or Edit Mode.
    *B602238,1 Intialize Vendor Available ticket types
    SET ORDER TO TAG VENCODE IN APVENDOR
    =SEEK(laData[1],'APVENDOR')
    lcVenInvTypes = ALLTRIM(APVENDOR.cVenInvTyp)
    *E301995 Alb [Begin]
    lcVenInvTypes = lcVenInvTypes + IIF(gfGetmemvar('M_BOMVAR') AND 'M' $ lcVenInvTypes,'A','')  
    lcVenInvTypes = lcVenInvTypes + IIF(!EMPTY(gfGetmemvar('M_DYEOPR')) AND 'M' $ lcVenInvTypes,'D','')
    *E301995 Alb [End]
    *B602500,1 Prevent user from modify if one or more of vendor supplies removed [Begin]
    lnOldAls = SELECT(0)
    SELECT APVINVDt
    lnOldInd = ORDER()
    SET ORDER TO Orgvinv
    =lfActPInq()
    IF SEEK(laData[1]+laData[2])
      LOCATE REST WHILE cvendcode+capinvno+capvilno = laData[1]+laData[2];
                FOR   !(cimtyp $ lcVenInvTypes)
      IF FOUND()
        =gfModalGen('TRM04196B00000','DIALOG')
        SHOW GET pbAPVInvDt DISABLE
        SHOW GET pbEdt DISABLE
        DEFINE BAR 2 OF _INQURYPOP PROMPT 'Invoice \<Lines' SKIP FOR .T.
        SET ORDER TO lnOldInd
        SELECT (lnOldAls)        
        RETURN
      ENDIF
    ENDIF
    SET ORDER TO lnOldInd
    SELECT (lnOldAls)
    *B602500,1 Prevent user from modify if one or more of vendor supplies removed [End]
    
    *B802783,1 Don't Default the control account its already loaded with the saved data
    *B602238,1 Default the AP Account. 
    *laData[33] = IIF(!EMPTY(APVENDOR.CAPACCT),APVENDOR.CAPACCT,;
    IIF(SEEK(laData[4],'APDIV') AND !EMPTY(APDIV.CAPACCT),APDIV.CAPACCT,APSETUP.CAPACCT))
    *B602238,1 Default the distribution Account
    lcDistAcct = IIF(!EMPTY(APVENDOR.CEXPACCT),APVENDOR.CEXPACCT,;
    IIF(SEEK(laData[4],'APDIV') AND !EMPTY(APDIV.CEXPACCT),APDIV.CEXPACCT,APSETUP.CEXPACCT))
    *B602238,1 (End)

    *C100728,1 Fill post date if empty
    IF TYPE('APINVHDR.DPOSTDATE') = 'D' .AND. EMPTY(laData[50])
       laData[50] = laData[10]
    ENDIF 
    *C100728,1 end

    IF laData[3] = 'C'
      SHOW GET pbApprove,1 PROMPT 'Credit c\<ard payment...'
    ELSE
      SHOW GET pbApprove,1 PROMPT '\<Approve for payment...'
    ENDIF

    IF laScrMode[2]
      lnStartAdr=1
      =lfShiftlaData(1)
      lcScUpStat='DISABLE'
      lcScdNStat='ENABLE'
      SHOW GET ibShiftUp DISABLE
      SHOW GET ibShiftDn ENABLE
      
      IF llInquiry .OR. laData[3] = 'C' 
        SHOW GET pbEdt DISABLE
      ELSE
        SHOW GET pbEdt ENABLE
      ENDIF
      lcTermsDisp= 'DISABLE'
      *E300789,4  AMM No need to seek because of the new structure
      *IF !llInquiry .AND. SEEK(gcPrnt_cmp,'SYCCOMP')
      IF !llInquiry 
      *E300789,4  AMM end
        *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
        *IF SEEK(gcPrnt_cmp+laData[32],'SYCFISHD')
          *IF SYCFISHD.CFISYSTAT <> 'H' .AND. laData[3] <> 'C' .AND. APINVHDR.CINVSTAT <> 'A'
         *E300789,4  AMM Adjust to fit the new structure
         *IF SEEK(gcPrnt_cmp+laData[32],'FISHD')          
         IF SEEK(laData[32],'FISHD')          
         *E300789,4  AMM end
          IF FISHD.CFISYSTAT <> 'H' .AND. laData[3] <> 'C' .AND. APINVHDR.CINVSTAT <> 'A'
        *E300692,1 end
            SHOW GET pbEdt ENABLE
            IF _DOS
              SHOW GET pbDlt,1 PROMPT '\<Void' ENABLE
            ELSE
              SHOW GET pbDlt ENABLE
            ENDIF  
          ELSE
            SHOW GET pbEdt DISABLE
            IF _DOS
              SHOW GET pbDlt,1 PROMPT '\<Void' DISABLE
            ELSE
              SHOW GET pbDlt DISABLE
            ENDIF  
          ENDIF
        ELSE
          SHOW GET pbEdt DISABLE
          IF _DOS
            SHOW GET pbDlt,1 PROMPT '\<Void' ENABLE
          ELSE
            SHOW GET pbDlt ENABLE
          ENDIF  
        ENDIF  
      ENDIF
      lcData23St = 'DISABLE'
    ENDIF

    IF APINVHDR.CINVSTAT = 'A'
      lcAprStat  = 'DISABLE'
      lcDistStat = 'DISABLE'
      SHOW GET pbApprove &lcAprStat
      SHOW GET pbDisLine &lcDistStat
    ELSE
      lcAprStat  = 'ENABLE'
      lcDistStat = 'ENABLE'
      SHOW GET pbApprove &lcAprStat
      SHOW GET pbDisLine &lcDistStat
    ENDIF
    
    *B801499,1 Change these line to prevent the user from editing the
    *Distribution lines if the Posting date falls within a locked
    *period [Begin]
    *lcObjShow = IIF(laScrMode[2],'DISABLE','ENABLE')
    lcObjShow = IIF(laScrMode[2] .OR. llLokPer , 'DISABLE' , 'ENABLE')
    *B801499,1 Change these line to prevent the user from editing [End]

    SHOW GET lcApdGlAct &lcObjShow
    SHOW GET ibApGlAcc  &lcObjShow
    SHOW GET lnApdAmnt  &lcObjShow
    
    lnTotAppPay = laData[14] + laData[13] + laData[30]

    IF laData[3] <> 'C' ;
       .AND. ((laScrMode[2] .AND. lnTotAppPay = 0);
       .OR. laData[12]-laData[25]-laData[26] = 0)) 
      lcAprStat = 'DISABLE' 
      SHOW GET pbApprove &lcAprStat
    ELSE
      IF APINVHDR.CINVSTAT = 'A'
        lcAprStat = 'DISABLE' 
        SHOW GET pbApprove &lcAprStat
      ELSE
        lcAprStat = 'ENABLE' 
        SHOW GET pbApprove &lcAprStat
      ENDIF
    ENDIF

    lcPhone     = APVENDOR.cPhoneNo
    lcCompany   = APVENDOR.cVenComp
    lnDistAmnt  = laData[12]

    *** Remit to objects 
    lnElemNum   = lfGetArrElem(laData[5], 'laRemitTo')
    lcRemitTo   = IIF(lnElemNum > 0, laRemitTo[lnElemNum, 1]," ")
    puRemitTo   = lnElemNum

    *B607430,1 Get the invoice address from the invoice file not vendor file in edit and view mode [BEGIN]
    *=lfvRemit(.T.)
    DO CASE
      CASE laData[5] = 'F'
        lcFactStat   = IIF(!laScrMode[2], 'ENABLE', 'DISABLE')
      OTHERWISE
        lcFactStat   = 'DISABLE'     
    ENDCASE
    =lfShiftlaData(lnStartAdr)
    SHOW GET laAddrs[1] &lcRemitStat
    SHOW GET laAddrs[2] &lcRemitStat
    SHOW GET laAddrs[3] &lcRemitStat
    SHOW GET laData [6] &lcRemitStat
    SHOW GET laData[36] &lcFactStat
    SHOW GET ibFactor   &lcFactStat
    *B607430,1 Get the invoice address from the invoice file not vendor file in edit and view mode [END]


    IF laScrMode[3]
      IF EMPTY(APVENDOR.CVEN1099t)  && If the vendor 1099 amount is empty.
        lcData23St = 'DISABLE'
      ELSE
        lcData23St = 'ENABLE'
      ENDIF

      *B601419,1 Change this line [Begin]
      *SELECT *,SPACE(60) AS 'CDISCRIP',;
      *       'S' AS 'cStatus',;
      *        SPACE(1) AS 'cUpdSerNo',;
      *        RECNO() AS 'NRECNO';
      *        FROM &gcDataDir.APDIST;
      *       WHERE CINVNO + CVENDCODE = laData[2] + laData[1];
      *         AND CAPDSTAT <> 'V';
      *        INTO DBF &gcWorkDir.&lcTmpDist
      
      *B604216,1 AKA 15/02/2001  Improve the performance of the SELECT statment [Start]
      *SELECT *,SPACE(60) AS 'CDISCRIP',;
      *       'S' AS 'cStatus',;
      *       SPACE(1) AS 'cUpdSerNo',;
      *       RECNO() AS 'NRECNO';
      *  FROM &gcDataDir.APDIST;
      * WHERE CINVNO + CVENDCODE = laData[2] + laData[1];
      *   AND CAPDSTAT <> 'V';
      *   AND cAPDTrTyp = 'I';
      *  INTO DBF &gcWorkDir.&lcTmpDist

      SELECT *,SPACE(60) AS 'CDISCRIP',;
             'S' AS 'cStatus',;
             SPACE(1) AS 'cUpdSerNo',;
             RECNO() AS 'NRECNO';
        FROM &gcDataDir.APDIST;
       WHERE CINVNO + CVENDCODE + cAPDTrTyp = laData[2] + laData[1] + "I" ;
         AND CAPDSTAT <> 'V';
        INTO DBF &gcWorkDir.&lcTmpDist
      *B604216,1 AKA 15/02/2001  Improve the performance of the SELECT statment [End]        



      *E300798,1 Create index to temp dist file
      INDEX ON cVendCode+cInvNo+STR(nApDLinNo,4) TAG (lcTmpDist)
      *E300798,1 (End)
      *B601419,1 Change this line [End]
      
      SELECT(lcTmpDist)

      lnLineNo = RECCOUNT() - 1
      
      RELEASE WINDOW (lcBrowsTtl)
      =lfBrowse()  
      =lfDispObjct()
      =lfRefresh()

      lcTmplate = 'DISABLE'
      SHOW GET laData[38] DISABLE
      SHOW GET ibTemplate DISABLE
      SHOW GET pbDlt DISABLE
    ELSE
      lcRemitStat = 'DISABLE'
      RELEASE WINDOW (lcBrowsTtl)
      SELECT APINVHDR
      IF !EOF()
        GO RECNO()
      ENDIF  
      =lfBrowse()
      =lfDispObjct()
      =lfRefresh()
    ENDIF  

    IF laData[3] = 'C'
      SHOW GET pbOk       DISABLE
      SHOW GET pbDiscount DISABLE
      SHOW GET laData[17] DISABLE
    ELSE
      IF !EMPTY(laData[20]) .AND. WVISIBLE('CWRAPDISTR')
        =gfChClose('CWRAPDISTR')  
      ENDIF
      SHOW GET pbOk ENABLE
    ENDIF  

    IF !EMPTY(APVENDOR.CAPACCT)
      lcAccount = APVENDOR.CAPACCT
    ELSE   
      IF !EMPTY(APDIV.CAPACCT)
        lcAccount = APDIV.CAPACCT
      ELSE
        lcAccount = APSETUP.CAPACCT
      ENDIF  
    ENDIF  

    ** Default the payment method.
    lcPayMethd = laPayMethd[AT(laData[3],'PMNCH'),1]
    puMethod   = AT(laData[3],'PMNCH')

    ** Default the division popup.
    lcCodeFilt = ''

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *lcDivision = gfCodDes(laData[4])
    lcDivision = gfCodDes(laData[4] , 'CDIVISION')
    *E300643,1 Change this line [End]
    
    lcPuDiv    = lcDivision

    ** Default the terms popup.

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *lcTermCode = gfCodDes(laData[15])
    lcTermCode = gfCodDes(laData[15] , 'CTERMCODE')
    *E300643,1 Change this line [End]
    
    lcPuTerm   = lcTermCode

    IF EMPTY(lcTermCode) .OR. EMPTY(laData[15])  && If empty of the Terms popup or not applicable.
      IF laScrMode[3]
        lcTermsDisp= 'ENABLE'
      ELSE
        lcTermsDisp= 'DISABLE'  
      ENDIF  
    ELSE
      lcTermsDisp= 'DISABLE'

      *E300643,1 Change this line for the changes we have made 
      *          to (gfRltFld) [Begin]
      *=gfRltFld(laData[15],@laTermAry)
      *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[Begin]
      *=gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
      IF laScrMode[4]
        =gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
      ENDIF
      *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[End]      
      *E300643,1 Change this line [End]
      
    ENDIF

    SHOW GET laData[19] &lcTermsDisp
    SHOW GET laData[24] &lcTermsDisp
    SHOW GET laData[16] &lcTermsDisp

    IF laScrMode[3]
      lnOldInvAmt = laData[12] && Variable to hold the old invoice amount.
    ENDIF

    IF laScrMode[3] .AND. lnOldDiscOf = 0
      lnOldDiscOf= laData[17]  && Variable to hold the old discount offerd amount.
    ENDIF

    IF laScrMode[3] .AND. EMPTY(lcOldYear)
      lcOldYear  = laData[32]  && Variable to hold the old fiscal year of the Inv. date.
    ENDIF

    IF laScrMode[3] .AND. EMPTY(lcOldPeriod)
      lcOldPeriod= laData[31]  && Variable to hold the old fiscal period of the Inv. date.
    ENDIF

    IF laScrMode[3] .AND. ldOldDatInv = {}
      *C100728,1 M.H Begin
      *ldOldDatInv= laData[10]  && Variable to hold the old invoice date.
      IF TYPE('APINVHDR.DPOSTDATE') = 'D'
        ldOldDatInv= laData[50]  && Variable to hold the old posting invoice date.
      ELSE
        ldOldDatInv= laData[10]  && Variable to hold the old invoice date.
      ENDIF
      *C100728,1 M.H End.
    ENDIF

    IF laScrMode[3] .AND. (laData[25]+laData[26]+laData[34]) > 0
      *C100728,1 M.H Begin.
      *SHOW GET laData[10] DISABLE
      IF TYPE('APINVHDR.DPOSTDATE') = 'D'
        
        *B801308,1 Add this line to get the APDIST fiscal year and period
        *(Posting date fiscal year and period) [Begin]
        =lfVlDate(gcPrnt_Cmp , @lcDistPrd , @lcDistYer , laData[50])
        *B801308,1 Add this line to get the APDIST fiscal year and period [End]
        
        SHOW GET laData[10] DISABLE
        SHOW GET laData[50] DISABLE
      ELSE
        SHOW GET laData[10] DISABLE
      ENDIF
      *C100728,1 M.H End.
    ENDIF

    IF EMPTY(laData[29])
      laData[29] = lcEmptyAcc
    ENDIF  

    IF EMPTY(laData[33])
      laData[33] = lcEmptyAcc
    ENDIF  

    IF laScrMode[3]
      IF laData[3] <> 'C' .AND. !EMPTY(laData[20])
        llPaidByCrd = IIF(llPaidByCrd,llPaidByCrd,.T.)
      ENDIF
    ELSE
      llPaidByCrd = .F.
    ENDIF    
    SHOW GET laData[44] DISABLE
    SHOW GET laData[45] DISABLE
    SHOW GEt ibCurrency DISABLE

    *E300798,1 Fill Invoice Details and Invoice Applied lines Temp. Files
    IF laScrMode[3]
      lcExSin2 = ''
      lcExSin1 = gfGetExSin(@lcExSin2,laData[44])
      
      SELECT *, 'S' AS cStatus,RECNO() AS nRecNo FROM ApVInvDt WHERE ;
      cvendcode+capinvno+capvilno = PADR(laData[1],8)+PADR(laData[2],12) ;
      INTO DBF (gcWorkDir+lcAPvInvDt)
      INDEX ON cIMTyp+cTktNo+cLotNo+cBomType+cOprCode+Item+Color+cDyelot TAG ITEM
      INDEX ON cVendCode+cApInvNo+cAPVILNo TAG (lcAPvInvDt) ADDITIVE
      INDEX ON cApDGlAct TAG ADJUST ADDITIVE
      
      SELECT *, 'S' AS cStatus,RECNO() AS nRecNo FROM ApInvTkt WHERE ;
      cvendcode+capinvno+capvilno = PADR(laData[1],8)+PADR(laData[2],12) ;
      INTO DBF (gcWorkDir+lcAPInvTkt)
      INDEX ON cIMTyp+cTktNo+STR(LineNo,6)+cRSession TAG Ticket
      INDEX ON cVendCode+cApInvNo+cApVILNo+cRSession TAG (lcAPInvTkt) ADDITIVE
      
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      SELECT (lcAPInvTkt)
      REPLACE ALL nAPAplAmnt WITH nAPAplAmnt * -1,;
                  nAPAplPric WITH nAPAplPric * -1 FOR (cIMTyp $ 'RF' AND (nAPAplAmnt > 0 ))

      SELECT (lcApVInvDt)
      REPLACE ALL nApAprAmnt WITH nApAprAmnt * -1,;
                  nApAprPric WITH nApAprPric * -1,;
                  nApPrice   WITH nApPrice   * -1,;
                  nApAmnt    WITH nApAmnt    * -1 FOR (cIMTyp $ 'RF' AND (nApAprAmnt > 0 ))
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
    ENDIF  
    *E300798,1 (End)


  CASE laScrMode[4]
    *C200178,1 AAN Check for the trigger (Cathy Daniel) if found add defa value to the
    * custom field cvenpytype[Begin]
    IF ASCAN(laEvntTrig , PADR('APINVTYPE',10)) <> 0
      =lfvUsrFld()
    ENDIF
    *C200178,1 AAN [End]

    *E300798,1 Zap Invoice lines and Invoice Applied lines temp files.
    SELECT (lcAPvInvDt)
    ZAP
    SELECT (lcAPInvTkt)
    ZAP
    *E300798,1 (End)
    
    llSamVendor = .T.
    llPaidByCrd = .F.
    
    laData[10]  = gdSysDate

    *C100728,1 M.H Begin.
    IF TYPE('APINVHDR.DPOSTDATE') = 'D'
      laData[50]  = gdSysDate
    ENDIF
    *C100728,1 M.H End. 

    IF EMPTY(APVENDOR.CVEN1099t) && If empty Vendor 1099 amount type
      lcData23St = 'DISABLE'
    ELSE
      lcData23St = 'ENABLE'
    ENDIF

    *** Remit to fields
    laData[5]   = laRemitTo[1,2] && default remit to vendor
    lcRemitTo   = laRemitTo[1,1]
    puRemitTo   = 1
    =lfvRemit(.T.)

    SHOW GET laData[6]  DISABLE
    SHOW GET laData[7]  DISABLE
    SHOW GET laData[8]  DISABLE
    SHOW GET laData[9]  DISABLE
    
*--Hesham el-sheltawi
    SHOW GET laData[39]  DISABLE
    SHOW GET laData[40]  DISABLE
    SHOW GET laData[41]  DISABLE
    SHOW GET laData[42]  DISABLE
    SHOW GET laData[43]  DISABLE    
*--Hesham el-sheltawi
   
    SHOW GET laData[27] ENABLE
    SHOW GET laData[28] ENABLE
    
    laData[3]  = APVENDOR.CVENPMETH   && Assign the payment meth from vendor. 
    laData[4]  = APVENDOR.CDIVISION   && Assign the division from vendor. 
    laData[15] = APVENDOR.CTERMCODE   && Assign the terms from vendor. 
    laData[18] = APVENDOR.CVENPRIOR
    
    lcPayMethd = laPayMethd[AT(laData[3],'PMNCH'),1]
    puMethod   = AT(laData[3],'PMNCH') &&laPayMethd[AT(laData[3],'PMNCH'),1]
    
    IF laData[3] = 'C' 
      ** We are going to disable the approved payment screen.
      SHOW GET pbApprove,1 PROMPT 'Credit c\<ard payment...'
      SHOW GET pbOk       DISABLE
      SHOW GET pbDiscount DISABLE
      SHOW GET laData[17] DISABLE
    ELSE
      SHOW GET pbApprove,1 PROMPT '\<Approve for payment...'
      IF !EMPTY(laData[20]) .AND. WVISIBLE('CWRAPDISTR')
        = gfChClose('CWRAPDISTR')  
      ENDIF  
    ENDIF  
    
    IF EMPTY(lcPhone)
      lcPhone   = APVENDOR.cPhoneNo
    ENDIF

    IF EMPTY(lcCompany)
      lcCompany = APVENDOR.cVenComp
    ENDIF

    ** Default the division popup.
    lcCodeFilt = ''

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *lcDivision = gfCodDes(laData[4])
    lcDivision = gfCodDes(laData[4] , 'CDIVISION')
    *E300643,1 Change this line [End]
    
    lcPuDiv    = lcDivision

    ** Default the terms popup.

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *lcTermCode = gfCodDes(laData[15])
    lcTermCode = gfCodDes(laData[15] , 'CTERMCODE')
    *E300643,1 Change this line [End]
    
    lcPuTerm   = lcTermCode

    *E300643,1 Change this line for the changes we have made 
    *          to (gfRltFld) [Begin]
    *=gfRltFld(laData[15],@laTermAry)

    *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[Begin]
    *=gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
    IF laScrMode[4]
      =gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
    ENDIF
    *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[End]      
    
    *E300643,1 Change this line for [End]
    
    IF EMPTY(lcTermCode) .OR. EMPTY(laData[15])  && If empty of the Terms popup or not applicable.
      lcTermsDisp= 'ENABLE'
    ELSE
      IF TYPE('laData[16]') = 'N'
        lcTermsDisp= 'DISABLE'
      ELSE
        laData[16] = 0 && Discount %
        laData[19] = 0 && Due days
        laData[24] = 0 && Dis. Days
        lcTermsDisp= 'ENABLE'
      ENDIF
    ENDIF

    SHOW GET laData[16] &lcTermsDisp
    SHOW GET laData[19] &lcTermsDisp
    SHOW GET laData[24] &lcTermsDisp

    ** Set the filter to file.
    SHOW GET lcPhone
    SHOW GET lcCompany
    SHOW GET puMethod
    SHOW GET lcPuDiv
    SHOW GET lcPuTerm

    IF laData[12] = 0
      lcDistStat = 'DISABLE'
      SHOW GET pbDisLine DISABLE
    ELSE
      lcDistStat = 'ENABLE'
      SHOW GET pbDisLine ENABLE
    ENDIF  
ENDCASE

IF laScrMode[3]
  *C100728,1 M.H Begin.
  *_CUROBJ = OBJNUM(laData[10])
  IF TYPE('APINVHDR.DPOSTDATE') = 'D'
    _CUROBJ = OBJNUM(laData[50])
  ELSE
    _CUROBJ = OBJNUM(laData[10])
  ENDIF
  *C100728,1 M.H End.
ENDIF
  
IF laData[12] <= 0  && If the invoice amount = 0
  ** We are going to disable the discount push button.
  SHOW GET pbDiscount DISABLE
  SHOW GET laData[17] DISABLE
ENDIF

=lfRefresh()    && refresh says filed on the screen

lcColor1  = SCHEME(1,6)
lcColor2  = SCHEME(1,2)

lcBFactCol= IIF(laData[5] = 'F', lcSchm7Dis, lcHidObjNrm)
lcFactCol = IIF(laData[5] = 'F', lcDisCont, lcHidObjNrm)

*** Show get all hand made popups with the appropirate color. ***
SHOW GET ibDivision  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
SHOW GET ibTermCode  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
SHOW GET ibPayMethod COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont

SHOW GET laData[36] &lcFactStat
SHOW GET ibFactor   &lcFactStat

SELECT APINVHDR

*B601419,1 This lines was added by HS [Begin]
*B601419,1 IF Statment to check if the screen mode is the Add Mode

*B602946,1 Change this condition to enable the template field only if the
*          "Vendor Invoice Types" is empty [Begin]
*IF laScrmode[4]
IF laScrmode[4] .AND. EMPTY(lcVenInvTypes)
*B602946,1 Change this condition to enable the template field [End]

  lcTmplate = 'ENABLE'
  SHOW GET laData[38] &lcTmplate
  SHOW GET ibTemplate &lcTmplate
ENDIF      && End of IF
*B601419,1 IF Statment to check if the invoice is a automatic Recurring
IF laData[37] = 'R'
   lcLabel = 'Recurring:'
ELSE     && Else
   lcLabel = 'Template :'
ENDIF    && End of IF
*B601419,1 IF Statment to check if the screen mode is the Select or View Mode
IF laScrmode[1] .OR. laScrmode[2] 
  lcTmplate = 'DISABLE'
  SHOW GET laData[38] &lcTmplate
  SHOW GET ibTemplate &lcTmplate
ENDIF   && End of IF   
*B601419,1 This lines was added by HS [End]

*E300807,1 Add these lines to control the enabling and disabling of the
*popup Inquiry [Begin]

*E300807,1 IF Statment to check if the screen mode is Selest or Add

*E300798,1 Add new bar in the option pad for invoice lines
*IF laScrMode[1] .OR. laScrMode[4]
*  SET SKIP OF PAD _INQUIRY OF _MSYSMENU .T.
*ELSE         && Else (if the screen mode is View or Edit)
*  SET SKIP OF PAD _INQUIRY OF _MSYSMENU .F.
*ENDIF        && End of the IF laScrMode[1] .OR. laScrMode[4]

SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
*E300798,1 (End)

*E300807,1 Add these lines to control the enabling and disabling of [End]

*T20060823.0017   TMI [Start] remove the definition of the Ctrl+E,Ctrl+D shortcut keys
laCtrStat[7] = 'DISABLE'
laCtrStat[8] = 'DISABLE'
*T20060823.0017   TMI [End  ] 


*!**************************************************************************
*!
*!      Function: lfDelShow
*!
*!**************************************************************************
* Validation function of the ok push button of the discont screen.
*
FUNCTION lfDelShow 

lcLoclShow = "lpShow" 
*E300789,4  AMM No need to SEEK because of the new structure
*IF SEEK(gcPrnt_cmp,'SYCCOMP')
*E300789,4  AMM end
  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *IF SEEK(gcPrnt_cmp+laData[32],'SYCFISHD')
      *IF SYCFISHD.CFISYSTAT <> 'H' .AND. laData[3] <> 'C' .AND. APINVHDR.CINVSTAT <> 'A'
  *E300789,4  AMM Adjust to fit the new structure
  *IF SEEK(gcPrnt_cmp+laData[32],'FISHD')      
  IF SEEK(laData[32],'FISHD')      
  *E300789,4  AMM end
      IF FISHD.CFISYSTAT <> 'H' .AND. laData[3] <> 'C' .AND. APINVHDR.CINVSTAT <> 'A'
  *E300692,1 end
      IF laScrmode[2]
        IF _DOS
          SHOW GET pbDlt,1 PROMPT '\<Void' ENABLE
        ELSE
          SHOW GET pbDlt ENABLE
        ENDIF
      ELSE
        IF _DOS
          SHOW GET pbDlt,1 PROMPT '\<Void' DISABLE
        ELSE
          SHOW GET pbDlt DISABLE
        ENDIF
      ENDIF
    ELSE
      IF _DOS
        SHOW GET pbDlt,1 PROMPT '\<Void' DISABLE
      ELSE
        SHOW GET pbDlt DISABLE
      ENDIF  
    ENDIF
  ELSE
    IF _DOS
      SHOW GET pbDlt,1 PROMPT '\<Void' DISABLE
    ELSE
      SHOW GET pbDlt DISABLE
    ENDIF  
  ENDIF  
*E300789,4  AMM Start
*ENDIF
*E300789,4  AMM end
IF laData[3] = 'C'
  SHOW GET pbApprove,1 PROMPT 'Credit c\<ard payment...' &lcAprStat
ELSE
  SHOW GET pbApprove,1 PROMPT '\<Approve for payment...' &lcAprStat
ENDIF

SHOW GET laData[38] &lcTmplate
SHOW GET ibTemplate &lcTmplate
SHOW GET pbDisLine  &lcDistStat

*!**************************************************************************
*!
*!      Function: lfActivate
*!
*!**************************************************************************
*
FUNCTION lfActivate

SELECT APINVHDR

IF !WVISIBLE(WPARENT(WOUTPUT()))
  SHOW WINDOW (WPARENT(WOUTPUT())) TOP
ENDIF
IF WPARENT(WOUTPUT()) = 'CWRAPDISTR'
  IF !llFromVend
    =lfTaxAct()
  ENDIF
  ON KEY LABEL ALT+C ACTIVATE WINDOW (lcBrowsTtl)  
  IF !WEXIST(lcBrowsTtl)
    =lfBrowse()
  ELSE
    SHOW WINDOW(lcBrowsTtl) REFRESH SAME
  ENDIF  

 *!B500684,1 MAN 06/01/95 Added Condition  IF ELSE
 IF WONTOP() = lcBrowsTtl
  =lfSetTrap()
 ELSE 
  =lfResetTrap()
  =lfRelease()
 ENDIF 
ELSE
  IF WEXIST(lcBrowsTtl)
    SHOW WINDOW (lcBrowsTtl) REFRESH SAME
  ENDIF  
  =lfResetTrap()
  =lfRelease()
ENDIF

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwData_1
*!
*!**************************************************************************
*
FUNCTION lfwData_1

lcOldVend = laData[1]
laData[1] = PADR(laData[1],8,' ')

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

SELECT APVENDOR
SET ORDER TO TAG VENCODE

*B802385,1  AKA (Start)
*lcSavExact= SET('EXACT')
*SET EXACT ON
*B802385,1  AKA (End)


lcVendor  = laData[1]

*B802385,1  AKA (Start)
*laData[1] = IIF(EMPTY(laData[1]),laData[1],ALLTRIM(laData[1]))
*B802385,1  AKA (End)


IF llBrowse .OR. !EMPTY(laData[1]) .AND. LASTKEY() = 13
  IF llBrowse .OR. !SEEK(laData[1]) .OR. ATC("?",laData[1]) > 0
    lnClosRec  = RECNO(0)
    ** MESSAGE : " This record is not found in the  "
    **           " data file.                       "
    **           "      < Browse >   < Reenter>     "
    
    IF llBrowse .OR. ATC("?",laData[1]) > 0
      DIMENSION laTemp[3]
      laTemp = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      
      *B606373,1 ALB Display all information of vendors [Begin]
      *lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                     CVENCOMP  :H= 'Company',;
                     CPHONENO  :H= 'Phone'"
      lcBrFields = lfGetBrHd('APVENDOR')
      *B606373,1 ALB Display all information of vendors [end]
      
      lcFile_Ttl = "Vendor"
      IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF
      *B606373,1 ALB Display all information of vendors [Begin]
      *=gfBrows(.F.,'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
      =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp')
      *B606373,1 ALB Display all information of vendors [end]
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
      IF !EMPTY(laTemp[1])
        lcVendor  = laTemp[1]
        laData[1] = ALLTRIM(laTemp[1])
        lcPhone   = laTemp[2]
        lcCompany = laTemp[3]
        _CUROBJ   = OBJNUM(laData[2])
      ELSE
        laData[1] = lcOldVend
      ENDIF
    ELSE
      *B605485,1 SSH 11/Feb/2002 Add Validation on user Privileges in adding new.
      IF llAddVen
      *B605485,1 SSH 11/Feb/2002 Add Validation on user Privileges in adding new.
        lnOption = gfModalGen('QRM00001B00001','Dialog',;
                               lcTVenKey + " : " +ALLTRIM(laData[1]))
      *B605485,1 SSH 11/Feb/2002 Add Validation on user Privileges in adding new.
      ELSE
        lnOption = gfModalGen('QRM00001B00014','Dialog',;
                               lcTVenKey + " : " +ALLTRIM(laData[1]))
        lnOption = IIF(lnOption=2,3,lnOption)
      ENDIF
      *B605485,1 SSH 11/Feb/2002 Add Validation on user Privileges in adding new.
      DO CASE
        CASE lnOption = 1
      
          DIMENSION laTemp[3]
      
          laTemp = ''
          lcSavBrFld = lcBrFields
          lcSavTitle = lcFile_Ttl

          *B606373,1 ALB Display all information of vendors [Begin]
          *lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                         CVENCOMP  :H= 'Company',;
                         CPHONENO  :H= 'Phone'"
          lcBrFields = lfGetBrHd('APVENDOR')
          *B606373,1 ALB Display all information of vendors [end]
 
          lcFile_Ttl = "Vendor"
          
          IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
            GO lnClosRec
          ELSE
            GO TOP
          ENDIF

          *B606373,1 ALB Display all information of vendors [Begin]
          *=gfBrows(.F.,'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
          =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp')
          *B606373,1 ALB Display all information of vendors [end]

          lcBrFields = lcSavBrFld
          lcFile_Ttl = lcSavTitle
      
          IF !EMPTY(laTemp[1])
            lcVendor  = laTemp[1]
            laData[1] = ALLTRIM(laTemp[1])
            lcPhone   = laTemp[2]
            lcCompany = laTemp[3]
            _CUROBJ   = OBJNUM(laData[2])
          ELSE
            laData[1] = lcOldVend
          ENDIF
          =lfRefresh()
        
        CASE lnOption = 2
          llFromVend = .T.
          
	      *B802385,1  AKA (Start)
          *SET EXACT &lcSavExact
          *B802385,1  AKA (End)
          
           *** B600261  line was added by YI
           =gfStatic()
           ***
           *E300683,1 Add parameters to GPDOPROG
		   *DO gpDoProg WITH 'AWRAPVENDR WITH "&laData[1]"'
           *DO gpDoProg WITH 'AWRAPVENDR WITH "&lcVendor"'
           DO gpDoProg WITH 'AWRAPVENDR', .F., 'AP',"'" + lcVendor + "'"
           *E300683,1 end
        CASE lnOption = 3
          laData[1] = lcOldVend
          _CUROBJ   = _CUROBJ
          =lfRefresh()
      ENDCASE
    ENDIF    
  ELSE
    lcPhone   = APVENDOR.CPHONENO
    lcCompany = APVENDOR.CVENCOMP
    _CUROBJ   = OBJNUM(laData[2])
  ENDIF
ELSE  
  laData[1] = lcOldVend
ENDIF
*B802385,1  AKA (Start)
*SET EXACT &lcSavExact
*B802385,1  AKA (End)

SHOW GET laData[1]
SHOW GET ibVendor  ENABLE
SHOW GET lcPhone   ENABLE
SHOW GET ibPhone   ENABLE
SHOW GET lcCompany ENABLE
SHOW GET ibCompany ENABLE
SHOW GET laData[2]
SHOW GET ibInvoice ENABLE

llBrowse = .F.
*E301995 Alb [Begin]
*E300798,1 Intialize Vendor Available ticket types
lcVenInvTypes = ALLTRIM(APVENDOR.cVenInvTyp)
lcVenInvTypes = lcVenInvTypes + IIF(gfGetmemvar('M_BOMVAR') AND 'M' $ lcVenInvTypes,'A','')
lcVenInvTypes = lcVenInvTypes + IIF(!EMPTY(gfGetmemvar('M_DYEOPR')) AND 'M' $ lcVenInvTypes,'D','')
*E300798,1 Default the AP Account.
*E301995 Alb [End]
laData[33] = IIF(!EMPTY(APVENDOR.CAPACCT),APVENDOR.CAPACCT,;
IIF(SEEK(laData[4],'APDIV') AND !EMPTY(APDIV.CAPACCT),APDIV.CAPACCT,APSETUP.CAPACCT))
*E300798,1 Default the distribution Account
lcDistAcct = IIF(!EMPTY(APVENDOR.CEXPACCT),APVENDOR.CEXPACCT,;
IIF(SEEK(laData[4],'APDIV') AND !EMPTY(APDIV.CEXPACCT),APDIV.CEXPACCT,APSETUP.CEXPACCT))
*E300798,1 (End)

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwData_2
*!
*!**************************************************************************
*
FUNCTION lfwData_2

laData[2]  = PADR(laData[2],12)
lcOldInvNo = laData[2]

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2

SELECT APINVHDR

lcSavExact = SET('EXACT')
SET EXACT ON
*!B500684,1 MAN 06/01/95 Added !EMPTY(ALLTRIM(laData[1])) .AND.
IF !EMPTY(ALLTRIM(laData[1])) .AND. !SEEK(ALLTRIM(laData[1]),'APVENDOR')
  _CUROBJ = OBJNUM(laData[1])
  SHOW GET laData[1]
  RETURN
ENDIF

laData[2]  = IIF(EMPTY(laData[2]),laData[2],ALLTRIM(laData[2]))

*B600619,1 Malak 8/16/95 {Begin}  Browse(Inv.no.) will not show the data of DIVISION.          
lcCodeFilt = ''
*B600619,1 Malak 8/16/95 {End}

IF llBrowse .OR. !EMPTY(laData[2]) .AND. LASTKEY() = 13
  IF llBrowse .OR. ATC("?",laData[2]) > 0 .OR. EMPTY(laData[1])
    DIMENSION laTemp[2]
    laTemp = ''

    IF EMPTY(laData[1])
      =gfBrows(.F.,'CVENDCODE,CINVNO','laTemp')
    ELSE
      SET EXACT &lcSavExact
      IF SEEK(laData[1])
        =gfBrows([lcVendor],'CVENDCODE,CINVNO','laTemp')
      ELSE
        ** MESSAGE : " No ð for vendor ð"
        **           "                  ® Ok ¯              "
        =gfModalGen("TRM04042B00000","DIALOG","invoices|"+laData[1])
      ENDIF  
    ENDIF  
    
    IF !EMPTY(laTemp[1])
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.
      SET EXACT &lcSavExact
      SHOW GETS
    ELSE
      laData[2] = lcOldInvNo
      SHOW GET laData[2]
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
  ELSE
    SET FILTER TO
    IF SEEK(lcVendor+laData[2],'APINVHDR') .AND. APINVHDR.CINVSTAT <> 'V'
      SET FILTER TO CINVSTAT <> 'V'    
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.
      SET EXACT &lcSavExact
      SHOW GETS
    ELSE
      IF APINVHDR.CINVSTAT = 'V'
        SET FILTER TO CINVSTAT <> 'V'
        ** Message : " Vendor ð Invoice No. ð is voided."
        **           " It cannot be used.               "
        **           "                ® Ok ¯            "
        =gfModalGen('QRM04146B00000','Dialog',ALLTRIM(laData[1])+'|'+ALLTRIM(laData[2]))
        laData[2] = lcOldInvNo
        SHOW GET laData[2]
        _CUROBJ = OBJNUM(laData[2])
      ELSE
        SET FILTER TO CINVSTAT <> 'V'
        *B608688,1   / TMI 09/08/2008 [Start] 
        *B608688,3 TMI 11/30/2008 [Start] Comment this code
        *SET EXACT &lcSavExact
        *IF !SEEK(lcVendor+laData[2])
        *  lnOption = gfModalGen('QRM00001B00031','Dialog',;
        *                       lcTVenInv + ALLTRIM(laData[1])+' '+ALLTRIM(laData[2]))
        *  lnOption = lnOption + 1                               
        *ELSE
        *B608688,3 TMI 11/30/2008 [End  ] 
        *B608688,1   / TMI 09/08/2008 [End  ] 

        lnOption = gfModalGen('QRM00001B00001','Dialog',;
                             lcTVenInv + ALLTRIM(laData[1])+' '+ALLTRIM(laData[2]))

        
          *T20071102.0018,10/C200876 TMI 09/13/2008 [Start] 
        *B608688,3 TMI 11/30/2008 [Start] comment this line        
        *ENDIF 
        *B608688,3 TMI 11/30/2008 [End  ] 
        *T20071102.0018,10/C200876 TMI 09/13/2008 [End  ]                                
        DO CASE
          CASE lnOption = 1             && Browse
            SET EXACT &lcSavExact
            IF SEEK(laData[1])
              DIMENSION laTemp[2]
              laTemp = ''
        
              *B608688,1   / TMI 09/08/2008 [Start] 
              *=gfBrows([FOR CVENDCODE = lcVendor],'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
              *B608688,3 TMI 11/30/2008 [Start] locate the line by soft seek
              *=gfBrows("lcVendor+laData[2]",'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
              IF !SEEK(lcVendor+PADR(laData[2],12))
                IF RECNO(0)<>0
                  GOTO (RECNO(0))
               ENDIF        
              ENDIF
              =gfBrows("lcVendor",'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
              *B608688,3 TMI 11/30/2008 [End  ] 
              *B608688,1   / TMI 09/08/2008 [End  ] 
    
              IF !EMPTY(laTemp[1])
                laData[1] = ALLTRIM(laTemp[1])
                laData[2] = ALLTRIM(laTemp[2])
                SCATTER FIELDS &lcScFields MEMO TO laData
                laScrMode = .F.
                laScrMode[2] = .T.
                SET EXACT &lcSavExact
                SHOW GETS
              ELSE
                laData[2] = lcOldInvNo
                SHOW GET laData[2]
                _CUROBJ = OBJNUM(laData[2])
              ENDIF
            ELSE
              ** MESSAGE : " No ð for vendor ð"
              **           "                  ® Ok ¯              "
              =gfModalGen("TRM04042B00000","DIALOG","invoices|"+laData[1])
              laData[2] = lcOldInvNo
              SHOW GET laData[2]
              _CUROBJ = OBJNUM(laData[2])
            ENDIF  
        
          CASE lnOption = 2       && Add
            *B602238,1 Intialize Vendor Available ticket types
            lcVenInvTypes = ALLTRIM(APVENDOR.cVenInvTyp)
            *E301995 Alb [Begin]
            lcVenInvTypes = lcVenInvTypes + IIF(gfGetmemvar('M_BOMVAR') AND 'M' $ lcVenInvTypes,'A','')
            lcVenInvTypes = lcVenInvTypes + IIF(!EMPTY(gfGetmemvar('M_DYEOPR')) AND 'M' $ lcVenInvTypes,'D','')
            *E301995 Alb [End]
            *B602238,1 Default the AP Account.
            laData[33] = IIF(!EMPTY(APVENDOR.CAPACCT),APVENDOR.CAPACCT,;
            IIF(SEEK(laData[4],'APDIV') AND !EMPTY(APDIV.CAPACCT),APDIV.CAPACCT,APSETUP.CAPACCT))
            *B602238,1 Default the distribution Account
            lcDistAcct = IIF(!EMPTY(APVENDOR.CEXPACCT),APVENDOR.CEXPACCT,;
            IIF(SEEK(laData[4],'APDIV') AND !EMPTY(APDIV.CEXPACCT),APDIV.CEXPACCT,APSETUP.CEXPACCT))
            *B602238,1 (End)

            laScrMode    = .F.
            laScrMode[4] = .T.
            *C100728 M.H Begin.
            *_CUROBJ = OBJNUM(laData[10])
            IF TYPE('APINVHDR.DPOSTDATE') = 'D'
              _CUROBJ = OBJNUM(laData[50])
            ELSE
              _CUROBJ = OBJNUM(laData[10])
            ENDIF
            *C100728 M.H End.
            SET EXACT &lcSavExact
            SHOW GETS
                    
          CASE lnOption = 3       && Reenter

            laData[2] = lcOldInvNo
            SHOW GET laData[2]
            SHOW GET ibInvoice ENABLE
            _CUROBJ = OBJNUM(laData[2])
        ENDCASE
      ENDIF  
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2])
  =lfTaxAct()
ENDIF

SET EXACT &lcSavExact

IF !laScrMode[1]
  lcDefFact = APVENDOR.cFacCode
ENDIF

llBrowse = .F.

*B601419,1 This lines was added by HS [Begin]
*B601419,1 IF Statment to check if the invoice is a automatic Recurring
IF laData[37] = 'R'
   lcLabel = 'Recurring:'
ELSE      && Else
   lcLabel = 'Template :'
ENDIF     && End of IF
=lfRefresh(gcBaseWind)
*B601419,1 This lines was added by HS [End]

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwPhone
*!
*!**************************************************************************
*
FUNCTION lfwPhone

lcOldPhone = lcPhone

*!**************************************************************************
*!
*!      Function: lfwCompany
*!
*!**************************************************************************
*
FUNCTION lfwCompany

lcOldComp = lcCompany

*!**************************************************************************
*!
*!      Function: lfvVendor
*!
*!**************************************************************************
*
FUNCTION lfvVendor
PARAMETERS lcObjNum

SELECT APVENDOR
lcSavOrder = SET('ORDER')

IF lcObjNum = '1' .AND. lcOldPhone = lcPhone .AND. !llBrowse
  RETURN
ENDIF  

IF lcObjNum = '2' .AND. lcOldComp = lcCompany .AND. !llBrowse
  RETURN
ENDIF  

SET ORDER TO TAG IIF(lcObjNum = '1','VENPHONE','VENCOMP')

lcObjName  = ALLTRIM(UPPER(EVALUATE(SYS(18))))

lcSavExact = SET('EXACT')
SET EXACT ON

IF llBrowse .OR. !EMPTY(lcObjName) .AND. LASTKEY() = 13
  IF llBrowse .OR. !SEEK(lcObjName) .OR. ATC("?",lcObjName) > 0
    lnClosRec  = RECNO(0)
    DIMENSION laTemp[3]

    laTemp = ''

    lcSavBrFld = lcBrFields
    lcSavTitle = lcFile_Ttl

    lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                  CVENCOMP :H= 'Company',;
                  CPHONENO :H= 'Phone'"

    lcFile_Ttl = "Vendor"

    IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
      GO lnClosRec
    ELSE
      GO TOP
    ENDIF
        
    =gfBrows(' ','CVENDCODE,CPHONENO,CVENCOMP','laTemp')
  
    lcBrFields = lcSavBrFld
    lcFile_Ttl = lcSavTitle
     
    IF !EMPTY(laTemp[1])
      lcVendor  = laTemp[1]
      laData[1] = ALLTRIM(laTemp[1])
      lcPhone   = laTemp[2]
      lcCompany = laTemp[3]
      _CUROBJ   = OBJNUM(laData[2])
    ELSE
      IF lcObjNum = '1'
        lcPhone   = lcOldPhone
      ELSE  
        lcCompany = lcOldComp
      ENDIF
    ENDIF
  ELSE
    IF !EOF()
      SKIP 1
      lcNxtPhone = APVENDOR.CPHONENO
      lcNxtComp  = UPPER(ALLTRIM(APVENDOR.CVENCOMP))
      IF lcObjName = IIF(lcObjNum = '1',lcNxtPhone,lcNxtComp)
        lnClosRec  = RECNO(0)
        SKIP - 1
        DIMENSION laTemp[3]

        laTemp = ''

        lcSavBrFld = lcBrFields
        lcSavTitle = lcFile_Ttl

        lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                      CVENCOMP :H= 'Company',;
                      CPHONENO :H= 'Phone'"

        lcFile_Ttl = "Vendor"
  
        IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
          GO lnClosRec
        ELSE
          GO TOP
        ENDIF
        
        IF lcObjNum = '1'
          =gfBrows([FOR CPHONENO = lcObjName],'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
        ELSE
          =gfBrows([FOR UPPER(ALLTRIM(CVENCOMP)) = lcObjName],'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
        ENDIF  
  
        lcBrFields = lcSavBrFld
        lcFile_Ttl = lcSavTitle
          
        IF !EMPTY(laTemp[1])
          lcVendor  = laTemp[1]
          laData[1] = ALLTRIM(laTemp[1])
          lcPhone   = laTemp[2]
          lcCompany = laTemp[3]
          _CUROBJ   = OBJNUM(laData[2])
        ELSE
          IF lcObjNum = '1'
            lcPhone   = lcOldPhone
          ELSE  
            lcCompany = lcOldComp
          ENDIF
        ENDIF
      ELSE
        SKIP - 1  
        lcVendor  = CVENDCODE
        laData[1] = ALLTRIM(CVENDCODE)
        lcPhone   = CPHONENO
        lcCompany = CVENCOMP
        _CUROBJ   = OBJNUM(laData[2])
      ENDIF  
    ENDIF
  ENDIF
ELSE
  IF lcObjNum = '1'
    lcPhone   = lcOldPhone
  ELSE  
    lcCompany = lcOldComp
  ENDIF
ENDIF

SET EXACT &lcSavExact

SHOW GET laData[1]
SHOW GET ibVendor  ENABLE
SHOW GET lcPhone   ENABLE
SHOW GET ibPhone   ENABLE
SHOW GET lcCompany ENABLE
SHOW GET ibCompany ENABLE
SHOW GET laData[2]
SHOW GET ibInvoice ENABLE

SELECT APVENDOR
SET ORDER TO &lcSavOrder

llBrowse = .F.

=lfRefresh()

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwDivision
*!
*!**************************************************************************
*
FUNCTION lfwDivision

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcCodeFilt = 'CDIVISION'
LOCATE

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
* Valid function of the division popup.
*
FUNCTION lfvDivision
 
DO CASE
  CASE _DOS
    laData[4] = gfActPop(7,1,13,34,'SYCCODES','cCode_No','cDiscrep',@lcDivision)

  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *lcPuDiv   = SYCCODES.cdiscrep
    *laData[4] = SYCCODES.cCode_No
    lcPuDiv   = CODES.cdiscrep
    laData[4] = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET lcPuDiv
    =gfUpdate()
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF  

=lfRefresh()    && refresh says filed on the screen

*!**************************************************************************
*!
*!      Function: lfwData_10
*!
*!**************************************************************************
* When of the invoce date.
*
FUNCTION lfwData_10

IF llInvCan 
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
  _CUROBJ  = OBJNUM(laData[1])
  llInvCan = .F.
ENDIF

ldOldData = laData[10]

*!**************************************************************************
*!
*!      Function: lfvData_10
*!
*!**************************************************************************
* Vaildation of the invioce date.
*
FUNCTION lfvData_10

IF laData[10] = {}
  ** MESSAGE : " The invoice ð.                    "
  **           " Do you want to cancel the invoice?"
  **           "          < Yes >   < No >         "

  IF gfModalGen("TRM04005B00006","DIALOG",'date cannot be empty') = 1
    laScrMode    = .F.
    laScrMode[1] = .T.
    llInvCan     = .T.
    SHOW GETS
    _CUROBJ      = OBJNUM(laData[1])
  ELSE 
    laData[10]   = ldOldData
    _CUROBJ      = _CUROBJ
  ENDIF  
ELSE
  
  *B801300,1 Add this lines to add new variables to save the Fiscal year
  *and period
  PRIVATE lcOldFisPd , lcOldFisYr
  lcOldFisPd = lcFisPrd
  lcOldFisYr = lcFisYer
  lcFisPrd = ''
  lcFisYer = ''
  *B801300,1 Add this lines to add new variables [End]
  
  *B801300,1 Change this line to accept the date if it belong to a locked
  *period [Begin]
  *IF lfVDtMsg(gcPrnt_Cmp,@lcFisPrd,@lcFisYer)
  =lfVDtMsg(gcPrnt_Cmp,@lcFisPrd,@lcFisYer)
  
  *B801300,1 IF the date belong to an existing Fiscal year and period
  IF !EMPTY(lcFisPrd) .AND. !EMPTY(lcFisYer)
  *B801300,1 Change this line to accept the date [End]
    
    laData[31] = lcFisPrd
    laData[32] = lcFisYer
    IF laScrMode[4]
      *E300296,1 M.H 10/10/95 Add the currency to the AP module.
      IF gfGetMemVar('LLMULCURR')
        DO CASE
          CASE laData[10] <> ldOldData .AND. EMPTY(laData[44])
            laData[44] = IIF(EMPTY(APVENDOR.CCURRCODE),gcBaseCurr,APVENDOR.CCURRCODE)
            laData[45] = gfChkRate('laData[48]',laData[44],laData[10],.T.)
          CASE laData[10] <> ldOldData .AND. !EMPTY(laData[44])
            laData[45] = gfChkRate('laData[48]',laData[44],laData[10],.T.)
          CASE EMPTY(laData[44])
            laData[44] = IIF(EMPTY(APVENDOR.CCURRCODE),gcBaseCurr,APVENDOR.CCURRCODE)
            laData[45] = gfChkRate('laData[48]',laData[44],laData[10],.T.)
        ENDCASE
        
        *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
        *E300316,1 to exchange currency from invoice currency to company base currency.
        *E300316,1 laData[44] -----> cCurrCode
        *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,laData[44])
        *lcExSin2 = IIF(lcExSin1 = '*','/','*')
        *E300316,1 (End)
        
        IF laData[45] = 0
          laData[44] = gcBaseCurr 
          laData[45] = 1
          laData[48] = 1
        ENDIF
        SHOW GET laData[44] ENABLE
        IF gfGetMemVar('LLEDITEXRA') .AND. laData[44] <> gcBaseCurr
          SHOW GET laData[45] ENABLE
        ELSE
          SHOW GET laData[45] DISABLE
        ENDIF  
      ELSE
        laData[44] = gcBaseCurr
        laData[45] = 1
        laData[48] = 1
        SHOW GET laData[44] DISABLE
        SHOW GET laData[45] DISABLE
        SHOW GEt ibCurrency DISABLE
      ENDIF
      *E300296,1 M.H 10/10/95 End.
    ELSE
      SHOW GET laData[44] DISABLE
      SHOW GET laData[45] DISABLE
      SHOW GEt ibCurrency DISABLE
    ENDIF
    _CUROBJ    = OBJNUM(laData[12])
    
  ELSE
    
    *B801300,1 Add this lines to restore the old Fiscal year and period [Begin]
      lcFisPrd = lcOldFisPd
      lcFisYer = lcOldFisYr
    *B801300,1 Add this lines to restore the old Fiscal year and period [End]
    
    laData[10] = ldOldData
    _CUROBJ    = _CUROBJ
  ENDIF
  
  *B801912,1 AMM Remove this validation, Allow posting date before invoice date
  *C100728,1 M.H Begin.
  *IF TYPE('APINVHDR.DPOSTDATE') = 'D'
    *IF laData[10] > laData[50]
      ** MESSAGE : " The invoice cannot be greater than"
      **           " the invoice posting date.         "
      **           "               ® Ok ¯              "
      *=gfModalGen("TRM04028B00000","DIALOG",'posting|invoice')
      *laData[10]   = ldOldData
      *_CUROBJ = _CUROBJ
      *SHOW GET laData[10]
      *RETURN
    *ENDIF
  *ENDIF
  *C100728,1 M.H End.
  *B801912,1 AMM end
  *B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [Begin]
  IF TYPE('APINVHDR.DPOSTDATE') = 'D'
    IF laData[10] > laData[50]
      ** MESSAGE : " The invoice cannot be greater than"
      **           " the invoice posting date.         "
      **           "         Modifay / Procced          "
      IF gfModalGen("TRM04197B04012","DIALOG","invoices|"+laData[1]) = 1
        laData[10]   = ldOldData
        _CUROBJ = _CUROBJ
        SHOW GET laData[10]
        RETURN
      ENDIF
    ENDIF
  ENDIF
  *B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [end]
  
ENDIF

*B604310,1 ALB Fix the due date according to the payment method [Begin]
*laData[22] = laData[10]+laData[19]
laData[22]  = lfGetdueD(laData[10],laData[19],lnEomDay)
*B604310,1 ALB Fix the due date according to the payment method [end]

SHOW GET laData[22]
SHOW GET laData[10]

*!**************************************************************************
*!
*!      Function: lfwPayMethd
*!
*!**************************************************************************
*
FUNCTION lfwPayMethd

lcOldPayMth = laData[3]

*!**************************************************************************
*!
*!      Function: lfvPayMethd
*!
*!**************************************************************************
*
FUNCTION lfvPayMethd
 
DO CASE
  CASE _DOS
    laData[3] = gfActPop(10,14,17,35,'laPayMethd',2,1,@lcPayMethd)

  CASE _WINDOWS
    laData[3] = laPayMethd[puMethod,2]
ENDCASE

IF laData[3] = 'C' .AND. laData[12] < 0
  ** MESSAGE : " You can not pay the debit invoice "
  **           " by credit card.                   "
  **           "                 ® Ok ¯            "
  =gfModalGen("TRM04041B00000","DIALOG")
  laData[3]  = lcOldPayMth
  lcPayMethd = laPayMethd[AT(laData[3],'PMNCH'),1]
  _CUROBJ    = _CUROBJ
  =lfRefresh()
  RETURN
ENDIF

IF !EMPTY(laData[20]) .AND. laData[3] = 'C'
  ** MESSAGE : " This invoice is a credit card pqayment   "
  **           " for vendor ð Invoice ð. You cannot change"
  **           " to a credit card payment.                "
  **           "                    ® Ok ¯                "
  =gfModalGen("TRM04145B00000","DIALOG",ALLTRIM(laData[20])+"|"+ALLTRIM(laData[21])))
  laData[3]  = lcOldPayMth
  lcPayMethd = laPayMethd[AT(laData[3],'PMNCH'),1]
  _CUROBJ    = _CUROBJ
  =lfRefresh()
  RETURN
ENDIF

SELECT APINVAHD
lcSavOrder = SET('ORDER')
SET ORDER TO TAG TVENDINV

lcSavExact = SET('EXACT')
SET EXACT ON

IF SEEK('I'+lcVendor+laData[2]) .AND. laScrMode[3]
  ** MESSAGE : " This invoice has corresponding instalment "
  **           " record.  ð.
  **           "                     ® Ok ¯                "

  lcNewPay  = laPayMethd[AT(laData[3],'PMNCH'),1]
  lcTIstCor = lcTCrdtCor+ALLTRIM(LOWER(lcNewPay))

  =gfModalGen("TRM04120B00000","DIALOG",lcTIstCor)
  
  laData[3]  = lcOldPayMth
  lcPayMethd = laPayMethd[AT(laData[3],'PMNCH'),1]
  _CUROBJ    = _CUROBJ
  
  =lfRefresh()
  
  SET ORDER TO &lcSavOrder
  SET EXACT &lcSavExact
  RETURN
ENDIF

SET ORDER TO &lcSavOrder
SET EXACT &lcSavExact

IF laData[3] = 'C'    && If the user select the credit card payment.
  ** If the amount paid + the discount taken not equal to 0.
  IF laData[25]+laData[26]+laData[34] > 0 
    ** MESSAGE : " You can not change to credit card payment "
    **           " for fully or partially paid invoice.      "
    **           "                    ® Ok ¯                 "

    =gfModalGen("TRM04012B00000","DIALOG")

    SHOW GET pbApprove,1 PROMPT '\<Approve for payment...'
    laData[3] =  lcOldPayMth

    _CUROBJ   = _CUROBJ

    =lfRefresh()
    
  ELSE
    SHOW GET pbApprove,1 PROMPT 'Credit c\<ard payment...'

    ** We are going to intialize the approved payments screen objects with 0.
    laData[27]  = SPACE(8)
    laData[28]  = SPACE(12)
    laData[29]  = lcEmptyAcc
    laData[13]  = 0.00      
    laData[14]  = 0.00

    *B601526,1 Add this line [Begin] 
    laData[51]  = 0.00
    *B601526,1 Add this line [End] 

    laData[30]  = 0.00
    laData[23]  = 0.00
    lnTotAppPay = 0.00
  ENDIF  
ELSE
  SHOW GET pbApprove,1 PROMPT '\<Approve for payment...'
  IF (lcOldPayMth = 'H' .AND. laData[3] <> 'H') .OR.;
     (lcOldPayMth $ 'MPN' .AND. laData[3] = 'H')
     
    laData[13]  = 0.00
    laData[14]  = 0.00
    
    *B601526,1 Add this line [Begin] 
    laData[51]  = 0.00
    *B601526,1 Add this line [End] 

    laData[23]  = 0.00    
    laData[27]  = ' '
    laData[28]  = ' '
    laData[29]  = lcEmptyAcc
    laData[30]  = 0
    lnTotAppPay = 0.00
  ENDIF
    
  laData[20] = IIF(llPaidByCrd,laData[20],SPACE(8))
  laData[21] = IIF(llPaidByCrd,laData[21],SPACE(12))
  lcAccount  = lcEmptyAcc
  
ENDIF

=lfRefresh()    && refresh says filed on the screen

*!**************************************************************************
*!
*!      Function: lfvRemit
*!
*!**************************************************************************
*
FUNCTION lfvRemit
PARAMETERS llJustShow

*** Default factor for the vendor
laData[36] = IIF(EMPTY(laData[36]), APVENDOR.cFacCode, laData[36])
*--hesham el_sheltawi remarked by 
*laData[5]  = lfRemit(laData[5], !llJustShow, laData[1], laData[36],;
              @lcRemitStat, 'laData[6]', 'laData[7]', 'laData[8]', 'laData[9]',;
              3, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
*--hesham el_sheltawi
laData[5]  = gfRemit(laData[5], !llJustShow, laData[1], laData[36],;
              @lcRemitStat, 'laData[6]', 'laData[39]', 'laData[40]', 'laData[41]','laData[42]','laData[43]',;
              3, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
              
laData[36] = IIF(laData[5] = 'F', laData[36], SPACE(6))

lnOldRemit = puRemitTo


*** If a vendor code exists, 
DO CASE
  CASE laData[5] = 'F'
    lcFactStat   = IIF(!laScrMode[2], 'ENABLE', 'DISABLE')
    
  OTHERWISE
    lcFactStat   = 'DISABLE'     
ENDCASE
=lfShiftlaData(lnStartAdr)
SHOW GET laAddrs[1] &lcRemitStat
SHOW GET laAddrs[2] &lcRemitStat
SHOW GET laAddrs[3] &lcRemitStat


SHOW GET laData[36] &lcFactStat
SHOW GET ibFactor   &lcFactStat

=lfRefresh()

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwData_12
*!
*!**************************************************************************
* When for the amount.
FUNCTION lfwData_12


IF llInvCan 
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
  _CUROBJ  = OBJNUM(laData[1])
  llInvCan = .F.
ENDIF

lnOldAmnt = laData[12]
*!**************************************************************************
*!
*!      Function: lfvData_12
*!
*!**************************************************************************
* Validation for the amount.
FUNCTION lfvData_12

*B801499,1 Add these lines to prevent the user from editing the
*Invoice amount if the Posting date falls within a locked
*period [Begin]

*B801499,1 If the Posting date falls within a locked period and the screen
*mode is Edit mode and the user has changed the Invoice amount

IF llLokPer .AND. laScrMode[3] .AND. laData[12] <> lnOldAmnt
  
  ** MESSAGE : " The posting date falls within a locked period. "
  **           " You cannot change ð.                           "
  **           "                    <    Ok    >                "
  =gfModalGen("TRM04163B00000" , "DIALOG" , 'the invoice amount')
  laData[12] = lnOldAmnt          && Restore the old Invoice amount
  _CUROBJ    = _CUROBJ
  RETURN
ENDIF    && End of IF llLokPer .AND. laScrMode[3] .AND. laData[12] <> lnOldAmnt

*B801499,1 Add these lines to prevent the user from editing [End]

IF laData[12] = 0
  IF laScrMode[4]
    ** MESSAGE : " The invoice ð.                    "
    **           " Do you want to cancel the invoice?"
    **           "          < Yes >   < No >         "
    IF gfModalGen("TRM04005B00006","DIALOG",lcTInvAmnt) = 1
      *B605090,1 ALB Keep the program in the same mode and pointed to the posted date [Begin]
      *B122951,1 NNA 05/31/2004 (Begin) Cancel Fixing of Bug#605090
      laScrMode    = .F.
      laScrMode[1] = .T.
      llInvCan = .T.
      SHOW GETS
      _CUROBJ  = OBJNUM(laData[1])
      *_CUROBJ  = OBJNUM(laData[50])
      *B122951,1 NNA (End)
      *B605090,1 ALB Keep the program in the same mode and pointed to the posted date [end]
    ELSE 
      laData[12] = lnOldAmnt
      _CUROBJ    = _CUROBJ
    ENDIF  
  ELSE
    ** MESSAGE : " ð should be greater than ð."
    **           "            ® Ok ¯          "
    =gfModalGen("TRM04072B00000","DIALOG",lcTInvAmnU+"|"+'0')
    laData[12] = lnOldAmnt
    _CUROBJ    = _CUROBJ
  ENDIF  
ELSE
  lnVenCrAvl = APVENDOR.NVENCRLIM - ROUND(APVENDOR.NVENBAL,0)
  IF laScrMode[3]
    lnVenCrAvl = lnVenCrAvl + APINVHDR.nInvAmnt
  ENDIF
  *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
  *E300316,1 to exchange currency from invoice currency to company base currency.
  *E300316,1 laData[44] -----> cCurrCode
  *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,laData[44])
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *E300316,1 (End)

  *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
  *E300316,1 laData[12] ----> nInvAmnt
  *E300316,1 laData[45] ----> nExRate
  *E300316,1 laData[48] ----> nCurrUnit
  *IF ROUND(laData[12] * laData[45] / laData[48],2) > lnVenCrAvl
  *B600971,1 M.H Begin.
  *IF ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2) > lnVenCrAvl
  IF ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2) > lnVenCrAvl .AND. APVENDOR.NVENCRLIM <> 0
  *B600971,1 M.H End.
    ** MESSAGE : " The invoice amount exceeds the "
    **           " available credit by  ð.        "
    **           "                ® Ok ¯          "

    ** ð = laData[12] - lnVenCrAvl
    *=gfModalGen("TRM04043B00000","DIALOG",ALLTRIM(STR(ROUND(laData[12] * laData[45] / laData[48],2) - lnVenCrAvl,15,2)))
    =gfModalGen("TRM04043B00000","DIALOG",;
     ALLTRIM(STR(ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2);
     - lnVenCrAvl,15,2)))
  ENDIF 
  *E300316,1 (End)

  IF laScrMode[3]     && Edit Mode
    DO CASE
      CASE SIGN(lnOldAmnt) <> SIGN(laData[12])
        ** MESSAGE : " You can not switch between invoice "
        **           " and debit memo.                    "
        **           "                ® Ok ¯              "
        =gfModalGen("TRM04008B00000","DIALOG")
        laData[12] = lnOldAmnt
        _CUROBJ    = _CUROBJ

      CASE lnOldAmnt > 0 AND laData[12] < laData[14] + laData[13] + laData[30]
        ** MESSAGE : " The invoice amount cannot be less    "
        **           " than the total approved to pay amount"
        **           " + the approved discount amount + the "
        **           " approved adjustment amount.          "
        **           "                  ® Ok ¯              "
        =gfModalGen("TRM04009B00000","DIALOG")
        laData[12] = lnOldAmnt
        _CUROBJ    = _CUROBJ

      CASE lnOldAmnt < 0  AND ABS(laData[12]) < ABS(laData[14] + laData[13] + laData[30])
        ** MESSAGE : " The invoice amount cannot be less    "
        **           " than the total approved to pay amount"
        **           " + the approved discount amount + the "
        **           " approved adjustment amount.          "
        **           "                  ® Ok ¯              "
        =gfModalGen("TRM04009B00000","DIALOG")
        laData[12] = lnOldAmnt
        _CUROBJ    = _CUROBJ

      CASE ABS(laData[12]) < ABS(laData[25] + laData[26] + laData[34])
        ** MESSAGE : " The invoice amount cannot be less  "
        **           " than the total paid amount + the   "
        **           " discount taken amount + the applied"
        **           " adjustment.                        "
        **           "                ® Ok ¯              "
        =gfModalGen("TRM04010B00000","DIALOG")
        laData[12] = lnOldAmnt
        _CUROBJ    = _CUROBJ
    ENDCASE
  ELSE
    IF laScrMode[4] .AND. laData[12] < 0 .AND. SIGN(lnOldAmnt) <> SIGN(laData[12])  && Add Mode.
      ** MESSAGE : " Do you want to add a debit memo?"
      **           "         < Yes >    < No >       "
      IF gfModalGen("TRM04006B00006","DIALOG") = 2
        laData[12] = 0
        _CUROBJ    = _CUROBJ
        *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
        llDebitM  = .F.
        *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
      ELSE
        *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
        llDebitM  = .T.
        *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

        IF laData[3] = 'C'
          laData[3]  = 'P'
          lcPayMethd = laPayMethd[AT(laData[3],'PMNCH'),1]
          SHOW GET pbApprove,1 PROMPT '\<Approve for payment...'
          =lfRefresh()
        ENDIF  
      ENDIF
    ENDIF  
  ENDIF  

  IF ABS(laData[12]) < ABS(laData[14] + laData[13] + laData[30]) .AND. laData[3] <> 'C'
    ** MESSAGE : " The invoice amount cannot be less     "
    **           " than the total approved to pay amount "
    **           " + the approved discount amount + the  "
    **           " approved adjustment amount.           "
    **           "                  ® Ok ¯               "
    =gfModalGen("TRM04009B00000","DIALOG")
    laData[12] = lnOldAmnt
    _CUROBJ    = _CUROBJ
  ENDIF   

  IF laData[12]-laData[25]-laData[26] = 0 .AND. laData[3] <> 'C'
    lcAprStat = 'DISABLE'
    SHOW GET pbApprove &lcAprStat
  ELSE
    lcAprStat = 'ENABLE'
    SHOW GET pbApprove &lcAprStat
  ENDIF

  IF laData[12] > 0
    IF laData[12] <> lnOldAmnt .OR. laData[17] = 0
      laData[17] = (laData[16] * laData[12]) / 100
    ENDIF
    SHOW GET pbDiscount ENABLE
    SHOW GET laData[17] ENABLE
  ELSE
    laData[17] = 0
    SHOW GET pbDiscount DISABLE
    SHOW GET laData[17] DISABLE
  ENDIF
  SHOW GET laData[12]
ENDIF
*B600592,1 Disable the distribution in the view mode also
*IF laData[12] = 0 .AND. laScrMode[4]
IF laData[12] = 0 .AND. (laScrMode[4] .OR. laScrMode[1])
  lcDistStat = 'DISABLE'
  SHOW GET pbDisLine DISABLE
ELSE
  lcDistStat = 'ENABLE'
  SHOW GET pbDisLine ENABLE
ENDIF
*B800735,1 M.H Begin.
IF laData[12] <> lnOldAmnt
  SELECT(lcTmpDist)
  DO CASE
    CASE laScrmode[4]
      IF RECCOUNT() > 0
        LOCATE FOR CAPDACTID = 'A'
        REPLACE NAPDAMNT  WITH -laData[12]
      ENDIF  

    CASE laScrMode[3]
      LOCATE FOR CAPDACTID = 'A' .AND. EMPTY(CAPDSTAT)
      lnSavApRec = RECNO()
      IF CSTATUS = 'A'
        SCATTER MEMVAR MEMO
        m.cStatus  = 'A'
        m.cApdStat = ' '
        m.nApdAmnt = -laData[12]
        m.lAPDPost = .F.
        m.cBatchNo = ' '
        m.cTrnSledn= ' '
        m.cAPSessNo= lcSequence
        
        *B801498,1 Add these line to clear the entry number [Begin]
        m.nEntryNo = 0
        *B801498,1 Add these line to clear the entry number [End]
        
        GATHER MEMVAR MEMO
      ELSE
        *B603483,1 NAD (Start)  Not to replace the CAPSESSNO for the old transaction  
        *REPLACE CAPDSTAT  WITH 'V'        ,;
                CAPSESSNO WITH lcSequence ,;
                CSTATUS   WITH 'M'
                
        REPLACE CAPDSTAT  WITH 'V'        ,;
                CSTATUS   WITH 'M'
        *B603483,1 NAD (End)
        SCATTER MEMVAR MEMO
        m.nApdAmnt = -1 * m.nApdAmnt
        m.cStatus  = 'A'
        m.lAPDPost = .F.
        m.cBatchNo = ' '
        m.cTrnSledn= ' '
        m.nEqvAmnt = -1 * m.nEqvAmnt        
        *B603483,1 NAD (Start)   replace the CAPSESSNO  to update it in the new transaction 
        M.CAPSESSNO = lcSequence
        *B603483,1 NAD (End) 
        
        *B801498,1 Add these line to clear the entry number [Begin]
        m.nEntryNo = 0
        *B801498,1 Add these line to clear the entry number [End]
        
        APPEND BLANK
        GATHER MEMVAR MEMO

        IF BETWEEN(lnSavApRec,0,RECCOUNT())
          GO lnSavApRec
        ENDIF

        SCATTER MEMVAR MEMO
        m.cStatus  = 'A'
        m.cApdStat = ' '
        m.nApdAmnt = -laData[12]
        m.lAPDPost = .F.
        m.cBatchNo = ' '
        m.cTrnSledn= ' '
        m.cAPSessNo= lcSequence
        
        *B801498,1 Add these line to clear the entry number [Begin]
        m.nEntryNo = 0
        *B801498,1 Add these line to clear the entry number [End]
        
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF
  ENDCASE
ENDIF
*B800735,1 M.H End.

IF laData[12] <> lnOldAmnt .AND. WVISIBLE('CWRAPDISTR')
  =gfChClose('CWRAPDISTR')  
ENDIF

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
IF laData[12] > 0
  llDebitM  = .F.
ELSE
  llDebitM  = .T.
ENDIF
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

*!**************************************************************************
*!
*!      Function: lfwTermCode
*!
*!**************************************************************************
* When function of the terms popup.
*
FUNCTION lfwTermCode

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcCodeFilt = 'CTERMCODE'
LOCATE

SELECT APINVHDR
lcOldTerms = laData[15]

*!**************************************************************************
*!
*!      Function: lfvTermCode
*!
*!**************************************************************************
* Validation of the terms popup
*
FUNCTION lfvTermCode

DO CASE
  CASE _DOS
    laData[15]  =gfActPop(5,1,12,34,'SYCCODES','cCode_No','cDiscrep',@lcTermCode)
  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *lcPuTerm   = SYCCODES.cdiscrep
    *laData[15] = SYCCODES.cCode_No
    lcPuTerm   = CODES.cdiscrep
    laData[15] = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET lcPuTerm
ENDCASE

IF LASTKEY() <> 27  && ESC

  *E300643,1 Change this line for the changes we have made 
  *          to (gfRltFld) [Begin]
  *=gfRltFld(laData[15],@laTermAry)

  *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[Begin]
  =gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
  *IF laScrMode[4]
  *  =gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
  *ENDIF
  *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[End]      
  
  *E300643,1 Change this line [End]
  
  lcTermsDisp = IIF(EMPTY(laData[15]),'ENABLE','DISABLE')
  
  *B604310,1 ALB Fix the due date according to the payment method [Begin]
  *laData[22]  = laData[10]+laData[19]
  laData[22]  = lfGetdueD(laData[10],laData[19],lnEomDay)
  *B604310,1 ALB Fix the due date according to the payment method [end]
  
  IF laData[12] > 0
    IF laData[12] <> lnOldAmnt
      laData[17] = (laData[16] * laData[12]) / 100
      SHOW GET laData[17]
    ENDIF
  ENDIF

  SHOW GET laData[16] &lcTermsDisp
  SHOW GET laData[19] &lcTermsDisp
  SHOW GET laData[24] &lcTermsDisp
  SHOW GET laData[22]

  =gfUpdate()
  =lfRefresh() && refresh says filed on the screen
ENDIF

SELECT APINVHDR

IF _WINDOWS
  DEACTIVATE POPUP puTermDes
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_16
*!
*!**************************************************************************
*
FUNCTION lfwData_16

lnPercent = laData[16]
 
*!**************************************************************************
*!
*!      Function: lfvData_16
*!
*!**************************************************************************
*
FUNCTION lfvData_16
*Execute the validation only in case of the disc. amount is > 0 
*OR the Disc % is changed
IF (laData[16] <> lnPercent) .OR. (laData[16] > 0 AND laData[17] > 0)
  IF laData[16] < 0
    laData[16] = lnPercent
    SHOW GET laData[16]
  ELSE
    IF laData[12] > 0
      laData[17] = (laData[16] * laData[12]) / 100
      SHOW GET laData[17]
    ENDIF
  ENDIF
ENDIF  

SHOW GET laData[17]

*!**************************************************************************
*!
*!      Function: lfwData_17
*!
*!**************************************************************************
*
FUNCTION lfwData_17

lnOldDisc = laData[17]
 
*!**************************************************************************
*!
*!      Function: lfvData_17
*!
*!**************************************************************************
* Validation of the offerd discount.
FUNCTION lfvData_17

IF laData[17] < 0
  laData[17] = lnOldDisc
  SHOW GET laData[17]
  RETURN
ENDIF

IF laData[17] > laData[12]
  ** MESSAGE : " The offered discount can not be    "
  **           " greater than the invoice amount.   "
  **           "                ® Ok ¯              "
  =gfModalGen("TRM04011B00000","DIALOG")
  laData[17] = IIF(lnOldDisc > 0,lnOldDisc,0)
ENDIF

SHOW GET laData[17]

*!**************************************************************************
*!
*!      Function: lfwData_18
*!
*!**************************************************************************
*
FUNCTION lfwData_18

lcOldPrior = laData[18]
 
*!**************************************************************************
*!
*!      Function: lfvData_18
*!
*!**************************************************************************
* Validation of the offerd discount.
FUNCTION lfvData_18

IF EMPTY(laData[18])
  laData[18] = lcOldPrior
ENDIF
SHOW GET laData[18]
  
*!**************************************************************************
*!
*!      Function: lfvDicount
*!
*!**************************************************************************
* Valiadation of the discount push button.
*
FUNCTION lfvDicount

rbDiscount = 1
lnPartial  = laData[12]
ON KEY LABEL ESC

*E300683,1 Call *.SPR from screens directory
* DO APDISC.SPR
DO (gcScrDir + gcWinAppl + '\APDISC.SPR')
*E300683,1 end
ON KEY LABEL ESC DO lfEscap
SHOW GET laData[17]

*!**************************************************************************
*!
*!      Function: lfvPartial
*!
*!**************************************************************************
* Validation of the Radio button of the biscount screen.
*
FUNCTION lfvPartial

IF rbDiscount = 1
  lnPartial = laData[12]
  SHOW GET lnPartial DISABLE
ELSE
  SHOW GET lnPartial ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDisc
*!
*!**************************************************************************
* Validation of the Radio button of the biscount screen.
*
FUNCTION lfvDisc

IF lnPartial > 0
  IF lnPartial > laData[12]
    ** MESSAGE : " The offered discount can not be    "
    **           " greater than the invoice amount.   "
    **           "                ® Ok ¯              "
    =gfModalGen("TRM04011B00000","DIALOG")
    lnPartial = laData[12]
    _CUROBJ   = _CUROBJ
  ENDIF
ELSE
  lnPartial = laData[12]
ENDIF

SHOW GET lnPartial

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
* Validation function of the ok push button of the discont screen.
*
FUNCTION lfvOk

DO CASE
  CASE rbDiscount = 1
    laData[17] = (laData[16] * laData[12]) / 100
  CASE rbDiscount = 2
    laData[17] = (laData[16] * lnPartial) / 100
ENDCASE

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfwData_19
*!
*!**************************************************************************
* 
FUNCTION lfwData_19

lnDueDays = laData[19]

*!**************************************************************************
*!
*!      Function: lfvData_19
*!
*!**************************************************************************
* Valid of the due date.
*
FUNCTION lfvData_19
IF laData[19] < 0
  laData[19] = lnDueDays
  SHOW GET laData[19]
ELSE
  *B604310,1 ALB Fix the due date according to the payment method [Begin]
  *laData[22] = laData[10]+laData[19]
  laData[22]  = lfGetdueD(laData[10],laData[19],lnEomDay)
  *B604310,1 ALB Fix the due date according to the payment method [end]
  SHOW GET laData[22]
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_20
*!
*!**************************************************************************
* When function of the vendor credit card.
*
FUNCTION lfwData_20

lcOldVenCr = laData[20]

*!**************************************************************************
*!
*!      Function: lfvData_20
*!
*!**************************************************************************
*
FUNCTION lfvData_20

SELECT APVENDOR

lcSavExact= SET('EXACT')
SET EXACT ON

IF llBrowse .OR. !EMPTY(laData[20])
  IF llBrowse .OR. !SEEK(laData[20]) .OR. ATC("?",laData[20]) > 0
    llBrowse = .F.
    DIMENSION laTemp[1]

    laTemp = ''

    lcSavBrFld = lcBrFields
    lcSavTitle = lcFile_Ttl

    lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                  CVENCOMP :H= 'Company',;
                  CPHONENO :H= 'Phone'"

    lcFile_Ttl = "Vendor"
    
    =gfBrows(.F.,'CVENDCODE,CVENCOMP,CPHONENO','laTemp')

    lcBrFields = lcSavBrFld
    lcFile_Ttl = lcSavTitle

    IF !EMPTY(laTemp[1])
      laData[20] = laTemp[1]
      IF !EMPTY(laData[20]) .AND. laData[20] = laData[1]
        ** MESSAGE : " The credit card vendor can not be  "
        **           " the same as the invoice vendor.    "
        **           "                ® Ok ¯              "

        =gfModalGen("TRM04013B00000","DIALOG")

        laData[20] = SPACE(8)
        laData[21] = SPACE(12)
        lcAccount  = lcEmptyAcc
        _CUROBJ    = _CUROBJ
      ELSE
        *B600592,1 Adjusting the invoice number field width to 12 chr.
        *laData[21] = laData[2]

        *B601448,1 Change this line [Begin]
        *laData[21] = PADR(laData[2],12)
        
        *B602367,1 Remove this line because we are going to move it after
        *          the part that get the credit card vendor default
        *          AP Account [Begin]
        *laData[21] = lfCrCINo()
        *B602367,1 Remove this line because we are going to move it [End]
        
        *B601448,1 Change this line [End]
        
        IF !EMPTY(APVENDOR.CAPACCT)
          lcAccount = APVENDOR.CAPACCT
        ELSE   
          IF !EMPTY(APDIV.CAPACCT)
            lcAccount = APDIV.CAPACCT
          ELSE
            lcAccount = APSETUP.CAPACCT
          ENDIF  
        ENDIF  
        
        *B602367,1 Add this line to move it after the part that get the
        *          credit card vendor default AP Account [Begin]
        laData[21] = lfCrCINo()
        *B602367,1 Add this line to move it after the part that get [End]
        
      ENDIF  
    ELSE
      laData[20] = SPACE(8)
    ENDIF
  ELSE
    IF !EMPTY(laData[20]) .AND. laData[20] = lcVendor
      ** MESSAGE : " The credit card vendor can not be  "
      **           " the same as the invoice vendor.    "
      **           "                ® Ok ¯              "
      =gfModalGen("TRM04013B00000","DIALOG")
      laData[20] = SPACE(8)
      laData[21] = SPACE(12)
      lcAccount  = lcEmptyAcc
      _CUROBJ    = _CUROBJ
    ELSE
      IF SEEK(laData[20])
        *B600592,1 Adjusting the invoice number field width to 12 chr.
        *laData[21] = laData[2]

        *B601448,1 Change this line [Begin]
        *laData[21] = PADR(laData[2],12)
        
        *B602367,1 Remove this line because we are going to move it after
        *          the part that get the credit card vendor default
        *          AP Account [Begin]
        *laData[21] = lfCrCINo()
        *B602367,1 Remove this line because we are going to move it [End]
        
        *B601448,1 Change this line [End]

        IF !EMPTY(APVENDOR.CAPACCT)
          lcAccount = APVENDOR.CAPACCT
        ELSE   
          IF !EMPTY(APDIV.CAPACCT)
            lcAccount = APDIV.CAPACCT
          ELSE
            lcAccount = APSETUP.CAPACCT
          ENDIF
        ENDIF
        
        *B602367,1 Add this line to move it after the part that get the
        *          credit card vendor default AP Account [Begin]
        laData[21] = lfCrCINo()
        *B602367,1 Add this line to move it after the part that get [End]
        
      ENDIF
    ENDIF
  ENDIF
ENDIF
SET EXACT &lcSavExact

SHOW GET laData[20]
SHOW GET laData[21]
SHOW GET lcAccount
*B600593,1 Return to the initial record number in the vendor record
=SEEK (laData[1],'APVENDOR')

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwData_21
*!
*!**************************************************************************
*
FUNCTION lfwData_21

lcOldVenInv = laData[21]

*!**************************************************************************
*!
*!      Function: lfvData_21
*!
*!**************************************************************************
*
FUNCTION lfvData_21

SELECT APINVHDR
lnSavRecNo = RECNO()

lcSavExact= SET('EXACT')
SET EXACT ON

IF !EMPTY(laData[20]) .AND. !EMPTY(laData[21])
  IF SEEK(laData[20]+laData[21])
    ** MESSAGE : " The invoice number already exist   "
    **           "                ® Ok ¯              "
    =gfModalGen("TRM04014B00000","DIALOG",laData[21])
    laData[21] = SPACE(12)
    _CUROBJ    = _CUROBJ
  ELSE
    IF EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0'))
      IF !EMPTY(APVENDOR.CAPACCT)
        lcAccount = APVENDOR.CAPACCT
      ELSE   
        IF !EMPTY(APDIV.CAPACCT)
          lcAccount = APDIV.CAPACCT
        ELSE
          lcAccount = APSETUP.CAPACCT
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

SET EXACT &lcSavExact

SELECT APINVHDR

IF BETWEEN(lnSavRecNo,0,RECCOUNT())
  GO lnSavRecNo
ENDIF

SHOW GET laData[21]
SHOW GET lcAccount

*!**************************************************************************
*!
*!      Function: lfwApAcct
*!
*!**************************************************************************
*
FUNCTION lfwApAcct

lcOldGLAcc = lcAccount

*!**************************************************************************
*!
*!      Function: lfvApAcct
*!
*!**************************************************************************
* Validation of the AP Account.
*
FUNCTION lfvApAcct

IF llBrowse .OR. lcAccount <> lcOldGlAcc
  =lfApAcs(.F.,llBrowse)
  =gfUpdate()
  IF EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0'))
    lcAccount = lcOldGLAcc
  ENDIF
  SHOW GET lcAccount
ENDIF

=lfRefresh()

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfvData_22
*!
*!**************************************************************************
* Validation of the due date.
*
FUNCTION lfvData_22

IF laData[22] < laData[10]
  ** MESSAGE : " The due date can not be less than  "
  **           " the invoice date.                  "
  **           "                ® Ok ¯              "
  =gfModalGen("TRM04016B00000","DIALOG")
  _CUROBJ = _CUROBJ
  SHOW GET laData[22]
*B604309,1 AAN 04/23/2001 Update the Due Days when the user change the due date[Begin]
ELSE
  laData[19] = laData[22] - laData[10]
  SHOW GET laData[19]
*B604309,1 AAN 04/23/2001 Update the Due Days when the user change the due date[End]  
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_23
*!
*!**************************************************************************
* Validation function of 1099 amount.
*
FUNCTION lfwData_23

lnOld1099 = laData[23]

*!**************************************************************************
*!
*!      Function: lfvData_23
*!
*!**************************************************************************
* Validation function of 1099 amount.
*
FUNCTION lfvData_23

DO CASE
  CASE laData[14] > 0
    IF !BETWEEN(laData[23],0,laData[14])
      ** MESSAGE : " The 1099 amount must be between 0 and ð."
      **           "                   ® OK ¯                "
      =gfModalGen("TRM04017B00000","DIALOG",'0'+"|"+ALLTRIM(STR(laData[14],15,2)))
      laData[23] = lnOld1099
    ENDIF
  CASE laData[14] < 0
    IF !BETWEEN(laData[23],laData[14],0)
      ** MESSAGE : " The 1099 amount must be between ð and 0."
      **           "                   ® OK ¯                "
      =gfModalGen("TRM04017B00000","DIALOG",ALLTRIM(STR(laData[14],15,2))+"|"+'0')
      laData[23] = lnOld1099
    ENDIF
  CASE laData[14] = 0 .AND. laData[23] > laData[14]
    ** MESSAGE : " The 1099 amount can not be greater than "
    **           " the approve to pay amount.              "
    **           "                   ® OK ¯                "
    =gfModalGen("TRM04147B00000","DIALOG")
    laData[23] = lnOld1099
ENDCASE

SHOW GET laData[23] &lcData23St

*!**************************************************************************
*!
*!      Function: lfwData_24
*!
*!**************************************************************************
*
FUNCTION lfwData_24

lnDisDays = laData[24]

*!**************************************************************************
*!
*!      Function: lfvData_24
*!
*!**************************************************************************
*
FUNCTION lfvData_24

IF laData[24] < 0
  laData[24] = lnDisDays
ENDIF

SHOW GET laData[24]

*!**************************************************************************
*!
*!      Function: lfvAprovPay
*!
*!**************************************************************************
* E300396,1 M.H 10/15/95 Add the currency to the AP module.
*
FUNCTION lfvAprovPay

*B800476,1 [Start]               
IF APVENDOR.CVENPRIOR = '0' 
  lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
  ** MESSAGE : " Vendor XXXXXX has payment priority 0."
  **           " This vendor in on hold.              " 
  **           "                ® OK ¯                " 
  =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
  IF lnTotAppPay = 0
    RETURN
  ENDIF
ENDIF
*B800476,1 [END]
*E301077,71 AMM Open files
llopchk = gfOpenFile(gcDataDir+'APCHECKS','BANKCHECK','SH')
llopBNK = gfOpenFile(gcDataDir+'APBANKS','BANKCODE','SH')
*E301077,71 AMM end

** If bank code in the vendor file is not empty.

IF laData[3] <> 'C'
  lcBankCode = laData[27]
  lcCheckAcc = laData[28]
  lcGLAcount = laData[29]
  lcApprPay  = laData[14]
  
  *B601526,1 Add this line [Begin]
  lnFAAp = laData[51]
  *B601526,1 Add this line [End]
  
  lcApprDis  = laData[13]
  lc1099Amnt = laData[23]
  lcApprAdj  = laData[30]
  *E300296,1 M.H 10/10/95 Add the currency to the AP module.
  lcAprCurr  = laData[47]
  lnAprRate  = laData[46]
  lnAprUnit  = laData[49]
  *E300296,1 M.H 10/10/95 End.
ENDIF
IF !laScrMode[2]
  DO CASE
    CASE laData[3] $ 'PMN'
      IF EMPTY(laData[14])
        *B600582,1  The discount taken was not subtracted from the Invoice amount
        *laData[14] = laData[12] - laData[25] - laData[34] - IIF(laData[26] < laData[17],laData[17] - laData[26],0)
        *B600582,1 Apr. Pay= Inv. Amnt. - Paid Amnt. - Adj. Amnt. - Disc. Takn.- Total Disc. - Disc. Takn.
        laData[14] = laData[12] - laData[25] - laData[34] - laData[26] - IIF(laData[26] < laData[17],laData[17] - laData[26],0)

        IF EMPTY(laData[13])
          laData[13] = IIF(laData[26] < laData[17],laData[17] - laData[26],0)
        ENDIF
      ENDIF

      IF EMPTY(laData[27])
        llBnkChkChng = .F.
        IF !EMPTY(APVENDOR.CBNKCODE)
          laData[27] = APVENDOR.CBNKCODE && Assign the bank code from vendor.
          laData[28] = APVENDOR.CCHKACCT && Assign the check code from vendor.
          llBnkChkChng = .T.
        ELSE
          IF !EMPTY(APDIV.CBNKCODE)
            laData[27] = APDIV.CBNKCODE  && Assign the bank code from division.
            laData[28] = APDIV.CCHKACCT  && Assign the bank code from division.
            llBnkChkChng = .T.
          ELSE
            laData[27] = APSETUP.CBNKCODE  && Assign the bank code from division.
            laData[28] = APSETUP.CCHKACCT  && Assign the bank code from division.
            llBnkChkChng = .T.
          ENDIF
        ENDIF

        IF llBnkChkChng
          IF SEEK(laData[27]+laData[28],'APCHECKS')
            laData[29] = IIF(!EMPTY(APCHECKS.CCHKGLACC),;
                             APCHECKS.CCHKGLACC,lcEmptyAcc)
          ENDIF
        ENDIF

        *E300296,1 M.H 10/10/95 Add the currency to the AP module.
        IF gfGetMemVar('LLMULCURR') .AND. !EMPTY(laData[28])
          IF SEEK(laData[27]+laData[28],'APCHECKS')
            laData[47] = IIF(EMPTY(APCHECKS.CCURRCODE),laData[44],APCHECKS.CCURRCODE)
          ELSE
            laData[47] = laData[44]
          ENDIF
        ELSE
          laData[47] = laData[44]
        ENDIF

        IF laData[47] = laData[44]
          laData[46] = 1
          laData[49] = 1
        ELSE
          *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
          *B600849,1                currency to approved currency.
          *B600849,1                laData[47]--> Approved currency code.
          *B600849,1                laData[44]--> Invoice currency code.
          *laData[46] = gfChkRate('laData[49]',laData[47],laData[10],.F.,.F.,laData[44])

          *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
          * Invoice currency is the base currency I make it the To currency 
          * and if not the Approve currency is the To currency 
          *laData[46] = gfChkRate('laData[49]',laData[44],laData[10],.F.,.F.,laData[47])
          *B601416,1 IF Statment to check if the Invoice currency is the 
          * same as the base currency
          IF laData[44] = gcBaseCurr
            laData[46] = gfChkRate('laData[49]',laData[47],laData[10],.F.,.F.,laData[44])
          ELSE      && Else
            laData[46] = gfChkRate('laData[49]',laData[44],laData[10],.F.,.F.,laData[47])
          ENDIF     && End of IF
          *B601416,1 Change the calling of gfChkRate [END]
          *B600849,1 (End)

          *B601612,1 Remove this lines [Begin]
          *IF laData[46] = 0
          *   laData[47] = laData[44]
          *   laData[46] = 1
          *   laData[49] = 1
          * ENDIF
          *B601612,1 Remove this lines [End]

        ENDIF
        *E300296,1 M.H 10/15/95
      ENDIF

      *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
      *E300316,1 to exchange currency from invoice currency to approved currency.
      *E300316,1 laData[44] -----> cCurrCode
      *E300316,1 laData[47] -----> cAprCurCod
      *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)
      lcExSin4 = ' '

      *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency 
      * and if not the Approve currency is the To currency 
      *lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF laData[44] = gcBaseCurr 
        lcExSin3 = gfGetExSin(@lcExSin4,laData[47],laData[44])  
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE   && Else
        lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])  
      ENDIF  && End of IF 
      *B601416,1 Change the calling of gfGetExSin [END]

      *lcExSin4 = IIF(lcExSin3 = '*','/','*')
      *E300316,1 (End)

      *E300296,1 M.H 10/10/95 Add the currency to the AP module.
      IF laScrMode[4] .AND. laData[44] = laData[47]
        laData[46] = 1
        laData[49] = 1
      ENDIF
      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[14] -----> nInvAmntAp
      *E300316,1 laData[46] -----> nAprExRat
      *E300316,1 laData[49] -----> nAprCurUnt
      *lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

      *B601526,1 Change this line using laData[51] instaed of lnExchange [Begin]
      *lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      *B601526,1 Change this line [End]

      *E300316,1 (End)
      *E300296,1 M.H 10/10/95 End.
      ON KEY LABEL ESC DO lfvCanPay
      lcOldGlAcc = laData[29]

      *E300296,1 M.H 10/10/95 Add the currency to the AP module.
      IF gfGetMemVar('LLMULCURR')
        *E300683,1 Call *.SPR from screens directory
        * DO APAPROVC.SPR  && Approved screen if multi currency = .T.
        DO (gcScrDir + gcWinAppl + '\APAPROVC.SPR')
        *E300683,1 end

      ELSE
        *E300683,1 Call *.SPR from screens directory
        * DO APAPROV.SPR  && Approved screen if multi currency = .F.
        DO (gcScrDir + gcWinAppl + '\APAPROV.SPR')
        *E300683,1 end
        
        *B601526,1 Add this line [Begin]
        laData[51] = laData[14]
        *B601526,1 Add this line [End]

      ENDIF
      *E300296,1 M.H 10/10/95 End.
      ON KEY LABEL ESC DO lfEscap  

    CASE laData[3] = 'H'

      IF EMPTY(laData[14])
        *B600582,1  The discount taken was not subtracted from the Invoice Amount
        *laData[14] = laData[12] - laData[25] - laData[34] - IIF(laData[26] < laData[17],laData[17] - laData[26],0)
        *B600582,1 
        * Apr. Pay = Inv. Amnt. - Paid Amnt. - Adj. Amnt. - Disc. Takn.- Total Disc. - Disc. Takn.
        laData[14] = laData[12] - laData[25] - laData[34] - laData[26] - IIF(laData[26] < laData[17],laData[17] - laData[26],0)

        IF EMPTY(laData[13])
          laData[13] = IIF(laData[26] < laData[17],laData[17] - laData[26],0)
        ENDIF
      ENDIF

      IF EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0'))
        DO CASE
          CASE !EMPTY(APVENDOR.CCASHACCT)
            laData[29] = APVENDOR.CCASHACCT
          CASE !EMPTY(laData[4]) .AND. !EMPTY(APDIV.CCASHACCT)
            laData[29] = APDIV.CCASHACCT
          OTHERWISE
            laData[29] = APSETUP.CCASHACCT
        ENDCASE  
      ENDIF
      
      *E300296,1 M.H 10/10/95 Add the currency to the AP module.
      *B602294,1 Default approve currency to invoice currency.
      *IF gfGetMemVar('LLMULCURR') .AND. EMPTY(laData[47])
      IF EMPTY(laData[47])
      *B602294,1 (End)

        laData[47] = laData[44]
        *B601416,1 Ther is no need for this lines [Begin]
        *laData[46] = laData[45]
        *laData[49] = laData[48]
        *B601416,1 Ther is no need for this lines [End]
      ENDIF
      
      *B601416,1 Change this line [Begin]
      *IF laScrMode[4] .AND. laData[44] = laData[47]
      *B601416,1 IF Statment to check if the Invoice currency is the same 
      * as the Approve currency in Edit and Add Mode for it can be 
      * Edit Mode but the Invoice did not had any Approve for Payment befor
      IF laData[44] = laData[47]
      *B601416,1 Change this line [End]
        laData[46] = 1
        laData[49] = 1
      ENDIF

      *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
      *E300316,1 to exchange currency from invoice currency to approved currency.
      *E300316,1 laData[44] -----> cCurrCode
      *E300316,1 laData[47] -----> cAprCurCod
      *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)      
      lcExSin4 = ' '

      *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency 
      * and if not the Approve currency is the To currency 
      *lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF laData[44] = gcBaseCurr 
        lcExSin3 = gfGetExSin(@lcExSin4,laData[47],laData[44])  
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE    && Else
        lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])  
      ENDIF   && End of IF 
      *B601416,1 Change the calling of gfGetExSin [END]
      *lcExSin4 = IIF(lcExSin3 = '*','/','*')
      *E300316,1 (End)

      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[14] -----> nInvAmntAp
      *E300316,1 laData[46] -----> nAprExRat
      *E300316,1 laData[49] -----> nAprCurUnt
      *lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

      *B601526,1 Change this line using laData[51] instaed of lnExchange [Begin]
      *lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      *B601526,1 Change this line [End]

      *E300316,1 (End)
      *E300296,1 M.H 10/15/95
      ON KEY LABEL ESC DO lfvCanPay
      lcOldGlAcc = laData[29]

      IF gfGetMemVar('LLMULCURR')
        *E300683,1 Call *.SPR from screens directory
        * DO APCSHPYC.SPR  && Approved screen if multi currency = .T.
        DO (gcScrDir + gcWinAppl + '\APCSHPYC.SPR')
        *E300683,1 end
        
        
      ELSE
        *E300683,1 Call *.SPR from screens directory
        * DO APCASHPY.SPR  && Approved screen if multi currency = .F.
        DO (gcScrDir + gcWinAppl + '\APCASHPY.SPR')
        *E300683,1 end

        *B601526,1 Add this line [Begin]
        laData[51] = laData[14]
        *B601526,1 Add this line [End]
        
      ENDIF
      ON KEY LABEL ESC DO lfEscap  

    CASE laData[3] = 'C'

      ON KEY LABEL ESC

      *E300296,1 M.H 10/10/95 Add the currency to the AP module.

      laData[47] = laData[44]
      laData[46] = laData[45]
      laData[49] = laData[48]
      *E300296,1 M.H 10/15/95

      *E300683,1 Call *.SPR from screens directory
      * DO APINVCRD.SPR  
      DO (gcScrDir + gcWinAppl + '\APINVCRD.SPR')
      *E300683,1 end
      ON KEY LABEL ESC DO lfEscap  
  ENDCASE
ELSE
  DO CASE
    CASE laData[3] = 'H'

      ON KEY LABEL ESC DO lfvCanPay
      lcOldGlAcc = laData[29]
      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[14] -----> nInvAmntAp
      *E300316,1 laData[46] -----> nAprExRat
      *E300316,1 laData[49] -----> nAprCurUnt
      *lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

      *B601416,1 This lines was added by HS [BEGIN]
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF laData[44] = gcBaseCurr 
        lcExSin3 = gfGetExSin(@lcExSin4,laData[47],laData[44])  
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE   && Else
        lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])  
      ENDIF  && End of IF
      *B601416,1 This lines was added by HS [END]

      *B601526,1 Change this line using laData[51] instaed of lnExchange [Begin]
      *lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      *B601526,1 Change this line [End]

      *E300316,1 (End)

      IF gfGetMemVar('LLMULCURR')
        *E300683,1 Call *.SPR from screens directory
        * DO APCSHPYC.SPR && Approved screen if multi currency = .T.  
        DO (gcScrDir + gcWinAppl + '\APCSHPYC.SPR')
        *E300683,1 end
        
      ELSE
        *E300683,1 Call *.SPR from screens directory
        * DO APCASHPY.SPR && Approved screen if multi currency = .F.
        DO (gcScrDir + gcWinAppl + '\APCASHPY.SPR')
        *E300683,1 end
        
        *B601526,1 Add this line [Begin]
        laData[51] = laData[14]
        *B601526,1 Add this line [End]

      ENDIF
      ON KEY LABEL ESC DO lfEscap  

    CASE laData[3] $ 'PMN'

      ON KEY LABEL ESC DO lfvCanPay
      lcOldGlAcc = laData[29]      
      *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
      *E300316,1 to exchange currency from invoice currency to approved currency.
      *E300316,1 laData[44] -----> cCurrCode
      *E300316,1 laData[47] -----> cAprCurCod
      *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)            
      lcExSin4 = ' '

      *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency 
      * and if not the Approve currency is the To currency 
      *lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF laData[44] = gcBaseCurr 
        lcExSin3 = gfGetExSin(@lcExSin4,laData[47],laData[44])  
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE   && Else
        lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])  
      ENDIF  && End of IF
      *B601416,1 Change the calling of gfGetExSin [END]
      
      *lcExSin4 = IIF(lcExSin3 = '*','/','*')
      *E300316,1 (End)

      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[14] -----> nInvAmntAp
      *E300316,1 laData[46] -----> nAprExRat
      *E300316,1 laData[49] -----> nAprCurUnt
      *lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

      *B601526,1 Change this line using laData[51] instaed of lnExchange [End]
      *lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
      *B601526,1 Change this line [End]

      *E300316,1 (End)

      IF gfGetMemVar('LLMULCURR')
        *E300683,1 Call *.SPR from screens directory
        * DO APAPROVC.SPR && Approved screen if multi currency = .T.
        DO (gcScrDir + gcWinAppl + '\APAPROVC.SPR')
        *E300683,1 end

      ELSE
        *E300683,1 Call *.SPR from screens directory
        * DO APAPROV.SPR && Approved screen if multi currency = .F.
        DO (gcScrDir + gcWinAppl + '\APAPROV.SPR')
        *E300683,1 end
        
        *B601526,1 Add this line [Begin]
        laData[51] = laData[14]
        *B601526,1 Add this line [End]

      ENDIF
      ON KEY LABEL ESC DO lfEscap  

    CASE laData[3] = 'C'

      ON KEY LABEL ESC
      *E300683,1 Call *.SPR from screens directory
      * DO APINVCRD.SPR 
      DO (gcScrDir + gcWinAppl + '\APINVCRD.SPR')
      *E300683,1 end
      ON KEY LABEL ESC DO lfEscap  
  ENDCASE 
ENDIF

lnTotAppPay = laData[14] + laData[13] + laData[30]
*E301077,71 AMM Close files
IF USED('APCHECKS') .AND. llopchk
  =gfCloseFile('APCHECKS')
ENDIF
IF USED('APBANKS') .AND. llopBNK
  =gfCloseFile('APBANKS')
ENDIF
*E301077,71 AMM end


=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
*
FUNCTION lfShow

IF laScrMode[2]
  IF laData[3] = 'C'
    SHOW GETS DISABLE ONLY
    SHOW GET pbClsCrd ENABLE
  ELSE

    IF laData[3] $ 'PMN'
      SHOW GET laData[27] DISABLE
      SHOW GET ibBank     DISABLE
      SHOW GET laData[28] DISABLE
      SHOW GET ibChecks   DISABLE
    ELSE
      SHOW GET ibAprCur   DISABLE
    ENDIF
    
    SHOW GET laData[29] DISABLE
    SHOW GET ibGLAcc    DISABLE
    SHOW GET laData[13] DISABLE
    SHOW GET laData[14] DISABLE
    SHOW GET laData[23] &lcData23St
    SHOW GET laData[30] DISABLE
    SHOW GET pbCancel   DISABLE
    SHOW GET pbClrApr   DISABLE
    SHOW GET laData[46] DISABLE
    SHOW GET laData[47] DISABLE
    
    IF _DOS
      SHOW GET pbOk,1 PROMPT '\!\<Ok' ENABLE
    ELSE
      SHOW GET pbOk ENABLE
    ENDIF  
  ENDIF
ELSE

  *B600981,1 SSE 03/21/2000 Disable Checking Account in case 
  *B600981,1                of Empty Bank Code[Begin]
  IF EMPTY(laData[27]) 
    SHOW GET laData[28] DISABLE      && Disable check account field
    SHOW GET ibChecks   DISABLE      && Disable check account browse push button
  ENDIF
  *B600981,1 SSE 03/21/2000 [End]
  
  IF laData[3] = 'H' .AND. laData[47] = laData[44]
    SHOW GET laData[46] DISABLE
  ELSE
    IF laData[47] = laData[44]
      SHOW GET laData[46] DISABLE
    ELSE
      SHOW GET laData[46] ENABLE
    ENDIF
  ENDIF
ENDIF

IF laData[3] <> 'C'
  IF laScrMode[3] .OR. laScrMode[4]
    _CUROBJ = OBJNUM(laData[14])
  ENDIF
ENDIF
*B607126,1 ALB Disable dicount/Adjs fields in case of inoice < 0 [Begin]
IF laData[12] < 0
  Show GET laData[13] DISABLE
  Show GET laData[30] DISABLE
ENDIF
*B607126,1 ALB Disable dicount/Adjs fields in case of inoice < 0 [end]
*!**************************************************************************
*!
*!      Function: lfwData_27
*!
*!**************************************************************************
*
FUNCTION lfwData_27

lcOldVal = laData[27]

*!**************************************************************************
*!
*!      Function: lfwData_28
*!
*!**************************************************************************
* When of checks.
*
FUNCTION lfwData_28
lcOldVal = laData[28]

*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields laData[27] (bank code), and laData[28] 
* (checking account code)
*
FUNCTION lfvBnkChk

IF !lfBnkChk(@laBankObjs,lcOldVal,@llBrowse)
  =lfRefresh()
  RETURN 1
ENDIF  

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
IF gfGetMemVar('LLMULCURR') .AND. !EMPTY(laData[28])
  IF SEEK(laData[27]+laData[28],'APCHECKS')
    laData[47] = IIF(EMPTY(APCHECKS.CCURRCODE),laData[44],APCHECKS.CCURRCODE)
  ELSE
    laData[47] = laData[44]
  ENDIF
ELSE
  laData[47] = laData[44]
ENDIF

IF laData[47] = laData[44]
  laData[46] = 1
  laData[49] = 1
  SHOW GET laData[46] DISABLE
ELSE
  *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
  *B600849,1                currency to approved currency.
  *B600849,1                laData[47]--> Approved currency code.
  *B600849,1                laData[44]--> Invoice currency code.
  *laData[46] = gfChkRate('laData[49]',laData[47],laData[10],.F.,.F.,laData[44])

  *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
  * Invoice currency is the base currency I make it the To currency 
  * and if not the Approve currency is the To currency 
  *laData[46] = gfChkRate('laData[49]',laData[44],laData[10],.F.,.F.,laData[47])
  *B601416,1 IF Statment to check if the Invoice currency is the 
  * same as the base currency
  IF laData[44] = gcBaseCurr
    laData[46] = gfChkRate('laData[49]',laData[47],laData[10],.F.,.F.,laData[44])
  ELSE   && Else
    laData[46] = gfChkRate('laData[49]',laData[44],laData[10],.F.,.F.,laData[47])
  ENDIF  && End of IF 
  *B601416,1 Change the calling of gfChkRate [END]

  *B600849,1 (End)

  IF laData[46] = 0
    ** MESSAGE : " A valid ð to ð exchange rate could not "
    **           " be found for ð.                        "
    **           "                  ® Ok ¯                "

    *B601667,1 Change this line to fix the parameters [Begin]
    *=gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(laData[44])+'|'+ALLTRIM(laData[47])+'|'+DTOC(laData[10]))

    *B601667,1 IF Statment to check if the Invoice currency is the
    *          Base Currency
    IF laData[44] = gcBaseCurr
      =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(laData[47])+'|'+ALLTRIM(laData[44])+'|'+DTOC(laData[10]))
    ELSE     && Else
      =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(laData[44])+'|'+ALLTRIM(laData[47])+'|'+DTOC(laData[10]))
    ENDIF    && End of IF
    *B601667,1 Change this line to fix the parameters [End]

    laData[46] = 1
    _CUROBJ = OBJNUM(laData[29])
  ENDIF
  SHOW GET laData[46] ENABLE
ENDIF
*E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
*E300316,1 to exchange currency from invoice currency to approved currency.
*E300316,1 laData[44] -----> cCurrCode
*E300316,1 laData[47] -----> cAprCurCod
*E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)            
lcExSin4 = ' '

*B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency 
* and if not the Approve currency is the To currency 
*lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])
*B601416,1 IF Statment to check if the Invoice currency is the 
* same as the base currency
IF laData[44] = gcBaseCurr 
  lcExSin3 = gfGetExSin(@lcExSin4,laData[47],laData[44])  
  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
ELSE   && Else
  lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])  
ENDIF  && End of IF
*B601416,1 Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')
*E300316,1 (End)


*E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
*E300316,1 laData[14] -----> nInvAmntAp
*E300316,1 laData[46] -----> nAprExRat
*E300316,1 laData[49] -----> nAprCurUnt
*lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

*B601526,1 Change this line using laData[51] instaed of lnExchange [End]
*lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
*B601526,1 Change this line [End]

*E300316,1 (End)
*E300296,1 M.H 10/10/95 End.

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfwData_29
*!
*!**************************************************************************
*
FUNCTION lfwData_29

lcOldGLAcc = laData[29]

*!**************************************************************************
*!
*!      Function: lfvData_29
*!
*!**************************************************************************
*
FUNCTION lfvData_29

IF llBrowse .OR. laData[29] <> lcOldGlAcc
  =lfApAcs(.F.,llBrowse)
  llBrowse = .F.
  IF EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0'))
    laData[29] = lcOldGLAcc
  ENDIF
  SHOW GET laData[29]
ENDIF

=lfRefresh()

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwData_13
*!
*!**************************************************************************
* When function of discount.
*
FUNCTION lfwData_13

lnInvDisc = laData[13]

*!**************************************************************************
*!
*!      Function: lfwData_14
*!
*!**************************************************************************
*
FUNCTION lfwData_14

lnInvDisc = laData[14]

*!**************************************************************************
*!
*!      Function: lfwData_30
*!
*!**************************************************************************
* When function of discount.
*
FUNCTION lfwData_30

lnInvDisc = laData[30]

*!**************************************************************************
*!
*!      Function: lfvApprove
*!
*!**************************************************************************
* Valid function of the discount.
*
FUNCTION lfvApprove
PARAMETERS lnObjNum
IF laData[12] > 0
  *B600813,1 Include the adjustment amount field (nInvAdj--> laData[34]),
  *B600813,1 in the open amount calculation forcomparison such that :
  *B600813,1 (laData[13] + laData[14] + laData[30]),
  *B600813,1 (nInvDisApr + nInvAmtAp  + nInvAgjAp ), 
  *B600813,1 is between 
  *B600813,1 (0, laData[12] - laData[25] - laData[26] - laData[34])
  *B600813,1 (0, nInvAmnt   - nInvPaid   - nInvDisTk  - nInvAdj   )
  *IF !BETWEEN(laData[13]+laData[14]+laData[30],;
               0,laData[12]-laData[25]-laData[26])
  IF !BETWEEN(laData[13]+laData[14]+laData[30],;
              0, laData[12]-laData[25]-laData[26]-laData[34])
  *B600813,1 end.            
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       ® Ok ¯                  "
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+'invoice')
    DO CASE
      CASE lnObjNum = 1
        laData[13]  = lnInvDisc
      CASE lnObjNum = 2
        laData[14]  = lnInvDisc
      CASE lnObjNum = 3
        laData[30]  = lnInvDisc
    ENDCASE
  ENDIF
ELSE
  *B600813,1 Include the adjustment amount field (nInvAdj--> laData[34]),
  *B600813,1 in the open amount calculation forcomparison such that :
  *B600813,1 (laData[13] + laData[14] + laData[30]),
  *B600813,1 (nInvDisApr + nInvAmtAp  + nInvAgjAp ), 
  *B600813,1 is between 
  *B600813,1 (laData[12] - laData[25] - laData[26] - laData[34], 0)
  *B600813,1 (nInvAmnt   - nInvPaid   - nInvDisTk  - nInvAdj   , 0)
  *IF !BETWEEN(laData[13]+laData[14]+laData[30],;
              laData[12]-laData[25]-laData[26],0)
  IF !BETWEEN(laData[13]+laData[14]+laData[30],;
              laData[12]-laData[25]-laData[26]-laData[34],0)
  *B600813,1 end.            
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       ® Ok ¯                  "
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+'debit memo ')
    DO CASE
      CASE lnObjNum = 1
        laData[13]  = lnInvDisc
      CASE lnObjNum = 2
        laData[14]  = lnInvDisc
      CASE lnObjNum = 3
        laData[30]  = lnInvDisc
    ENDCASE
  ENDIF
ENDIF

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
*E300316,1 laData[14] -----> nInvAmntAp
*E300316,1 laData[46] -----> nAprExRat
*E300316,1 laData[49] -----> nAprCurUnt
*lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

*B601526,1 Change this line using laData[51] instaed of lnExchange [Begin]
*lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
*B601526,1 Change this line [End]

*E300316,1 (End)
*E300296,1 M.H 10/10/95 End.

SHOW GET laData[13]
SHOW GET laData[14]
SHOW GET laData[23] &lcData23St
SHOW GET laData[30]

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvOkApr
*!
*!**************************************************************************
* Validation function of the ok push button of the Approved for payment.
*
FUNCTION lfvOkApr

*B601612,1 Add this lines to check if the exchange rate is empty and if so
*          do not approve [Begin]
*B601612,1 IF Statment to check if the exchange rate = 0 or empty
IF EMPTY(laData[46])

  ** MESSAGE : "     ð should be greater than ð       "
  **           "                  ® Ok ¯              "
  **
  =gfModalGen("TRM04072B00000" , "DIALOG" , "Exchange rate|zero")
  _CUROBJ = OBJNUM(laData[46])
  RETURN
ENDIF   && End of IF 
*B601612,1 Add this lines [End]
 
lcEmptyObj = ''

IF laScrMode[2]   && In case of view mode ® Ok ¯ is terminating
  CLEAR READ
  RETURN
ENDIF

DO CASE
  CASE laData[14] = 0
    ** MESSAGE : " You have to enter the approved to pay amount."
    **                ® Ok ¯         

    =gfModalGen("TRM04022B00000","DIALOG")
    _CUROBJ = OBJNUM(laData[14])
    RETURN
    
  CASE EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0'))
    IF laData[3] = 'H'
      ** MESSAGE : " You have to enter the ð account.     "
      **           "                  ® Ok ¯              "
      =gfModalGen("TRM04020B00000","DIALOG",lcTPayAcct)
      _CUROBJ = OBJNUM(laData[29])
      RETURN
    ELSE
      lcEmptyObj = 'laData[29]'
    ENDIF 

  CASE laData[3] <> 'H' .AND. EMPTY(laData[27]) 
    lcEmptyObj = 'laData[27]'

  CASE laData[3] <> 'H' .AND. EMPTY(laData[28]) 
    lcEmptyObj = 'laData[28]'

ENDCASE

IF !EMPTY(lcEmptyObj)
  ** MESSAGE : " You have to enter the bank code,the  "
  **           " checking account,and the GL account. "
  **           "                  ® Ok ¯              "
  =gfModalGen("TRM04021B00000","DIALOG")
  _CUROBJ = OBJNUM(&lcEmptyObj)
  RETURN
ENDIF 

IF lcData23St = 'ENABLE'
  IF laData[14] >= 0
    lnLowerVal = 0
    lnUpperVal = laData[14]
  ELSE
    lnLowerVal = laData[14]
    lnUpperVal = 0
  ENDIF

  IF !BETWEEN(laData[23],lnLowerVal,lnUpperVal)
    ** MESSAGE : " The 1099 amount must be between 0 and ð."
    **           "                   ® OK ¯                "
    =gfModalGen("TRM04017B00000","DIALOG",;
    ALLTRIM(STR(lnLowerVal,15,2))+"|"+ALLTRIM(STR(lnUpperVal,15,2)))
    _CUROBJ = OBJNUM(laData[23])
    RETURN
  ENDIF
ENDIF

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
IF !gfGetMemVar('LLMULCURR') .AND. laScrMode[4] && If not multi currrency.
  laData[47] = gcBaseCurr
  laData[46] = 1
  laData[49] = 1
ENDIF
*E300296,1 M.H 10/10/95 End.

CLEAR READ    

*!**************************************************************************
*!
*!      Function: lfvOkPay
*!
*!**************************************************************************
* Validation function of the ok push button of the Approved for payment.
*
FUNCTION lfvOkPay

IF !laScrMode[2]
  IF lcData23St = 'ENABLE'
    IF laData[14] >= 0
      IF !BETWEEN(laData[23],0,laData[14])
        ** MESSAGE : " The 1099 amount must be between 0 and ð."
        **           "                   ® OK ¯                "
        =gfModalGen("TRM04017B00000","DIALOG",'0'+"|"+ALLTRIM(STR(laData[14],15,2)))
        _CUROBJ = OBJNUM(laData[23])
        RETURN
      ENDIF
    ELSE
      IF !BETWEEN(laData[23],laData[14],0)
        ** MESSAGE : " The 1099 amount must be between 0 and ð."
        **           "                   ® OK ¯                "
        =gfModalGen("TRM04017B00000","DIALOG",ALLTRIM(STR(laData[14],15,2))+"|"+'0')
        _CUROBJ = OBJNUM(laData[23])
        RETURN
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(laData[27]+laData[28]+STRTRAN(STRTRAN(laData[29],'-'),'0'));
      .AND. laData[14]+laData[13]+laData[30] >= 0

    DO CASE
      CASE laData[3] = 'N' .AND. EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0'))
        ** MESSAGE : " You have to enter the ð account.     "
        **           "                  ® Ok ¯              "
        =gfModalGen("TRM04020B00000","DIALOG",lcTPayAcct)
        SHOW GET laData[29]
        _CUROBJ = OBJNUM(laData[29])

      CASE laData[3] <> 'N'      ;
        .AND. (EMPTY(laData[27]) ;
         .OR. EMPTY(laData[28])  ;
         .OR. EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0')))

        ** MESSAGE : " You have to enter the bank code,the  "
        **           " checking account,and the GL account. "
        **           "                  ® Ok ¯              "
        =gfModalGen("TRM04021B00000","DIALOG")
        IF EMPTY(laData[27])
          SHOW GET laData[27]
          _CUROBJ = OBJNUM(laData[27])
        ELSE
          IF EMPTY(laData[28])
            SHOW GET laData[28]
            _CUROBJ = OBJNUM(laData[28])
          ELSE
            SHOW GET laData[29]
            _CUROBJ = OBJNUM(laData[29])
          ENDIF
        ENDIF

      CASE !EMPTY(laData[27]+laData[28]+STRTRAN(STRTRAN(laData[29],'-'),'0')) ;
           .AND. laData[14] = 0
        ** MESSAGE : " You have to enter the approved to pay "
        **           " amount.                               "
        **           "                  ® Ok ¯               "
        =gfModalGen("TRM04022B00000","DIALOG")

        SHOW GET laData[14]
        _CUROBJ = OBJNUM(laData[14])

      OTHERWISE
        CLEAR READ
    ENDCASE
  ENDIF
ELSE
  CLEAR READ
ENDIF

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
IF !gfGetMemVar('LLMULCURR') .AND. laScrMode[4]  && If not multi currrency.
  laData[46] = 1
  laData[47] = gcBaseCurr
  laData[49] = 1
ENDIF
*E300296,1 M.H 10/10/95 End.

*!**************************************************************************
*!
*!      Function: lfvOkCash
*!
*!**************************************************************************
* Validation function of the ok push button of the Approved for payment.
*
FUNCTION lfvOkCash

IF !laScrMode[2]
  IF lcData23St = 'ENABLE'
    IF laData[14] >= 0
      IF !BETWEEN(laData[23],0,laData[14])
        ** MESSAGE : " The 1099 amount must be between 0 and ð."
        **           "                   ® OK ¯                "
        =gfModalGen("TRM04017B00000","DIALOG",'0'+"|"+ALLTRIM(STR(laData[14],15,2)))
        _CUROBJ = OBJNUM(laData[23])
        RETURN
      ENDIF
    ELSE
      IF !BETWEEN(laData[23],laData[14],0)
        ** MESSAGE : " The 1099 amount must be between 0 and ð."
        **           "                   ® OK ¯                "
        =gfModalGen("TRM04017B00000","DIALOG",ALLTRIM(STR(laData[14],15,2))+"|"+'0')
        _CUROBJ = OBJNUM(laData[23])
        RETURN
      ENDIF
    ENDIF
  ENDIF

  IF EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0')) .OR. laData[14]+laData[13]+laData[30] = 0

    DO CASE
      CASE laData[3] = 'H' .AND. EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0'))
        ** MESSAGE : " You have to enter the non check      "
        **           "  payment GL account.                 "
        **           "                  ® Ok ¯              "
        =gfModalGen("TRM04020B00000","DIALOG",lcTCashAcct)
        SHOW GET laData[29]
        _CUROBJ = OBJNUM(laData[29])

      CASE !EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0')) .AND. laData[14] = 0
        ** MESSAGE : " You have to enter the approved to pay "
        **           " amount.                               "
        **           "                  ® Ok ¯               "
        =gfModalGen("TRM04022B00000","DIALOG")

        SHOW GET laData[14]
        _CUROBJ = OBJNUM(laData[14])

      OTHERWISE
        CLEAR READ
    ENDCASE
  ELSE
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ
ENDIF

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
IF !gfGetMemVar('LLMULCURR') .AND. laScrMode[4]  && If not multi currrency.
  laData[46] = 1
  laData[47] = gcBaseCurr
  laData[49] = 1
ENDIF
*E300296,1 M.H 10/10/95 End.

*!**************************************************************************
*!
*!      Function: lfvClrApr
*!
*!**************************************************************************
* Validation function of the cancel push button of the Approved for payment.
*
FUNCTION lfvClrApr

*B601612,1 Add this line to passe a parameter to control the terminating
*        of the screen (.T. dont terminate , .F. terminate) [Begin]
PARAMETERS llNoTermnt
*B601612,1 Add this line [End]

laData[29] = lcEmptyAcc
laData[13] = 0
laData[14] = 0

*B601526,1 Add this line [Begin] 
laData[51]  = 0
*B601526,1 Add this line [End] 

laData[23] = 0
laData[30] = 0
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
laData[46] = 0
laData[47] = ''
laData[49] = 0
*E300296,1 M.H 10/10/95 End.

laData[27] = SPACE(8)
laData[28] = SPACE(12)

*B601612,1 Change this line to check if we are going to terminate the screen
*		 or not [Begin]
*CLEAR READ
*B601612,1 IF Statment to check if we are going to terminate the screen
IF !llNoTermnt
  CLEAR READ
ENDIF  && End of IF
*B601612,1 Change this line [End]

*!**************************************************************************
*!
*!      Function: lfvCanPay
*!
*!**************************************************************************
* Validation function of the cancel push button of the Approved for payment.
*
FUNCTION lfvCanPay

IF laData[3] <> 'H'
  laData[27] = lcBankCode
  laData[28] = lcCheckAcc
ENDIF
laData[29] = IIF(!EMPTY(lcGLAcount),lcGlAcount, lcEmptyAcc)
laData[14] = lcApprPay

*B601526,1 Add this line [Begin]
laData[51] = lnFAAp
*B601526,1 Add this line [End]

laData[13] = lcApprDis
laData[23] = lc1099Amnt
laData[30] = lcApprAdj

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
laData[47] = lcAprCurr
laData[46] = lnAprRate
laData[49] = lnAprUnit
*E300296,1 M.H 10/10/95 End.

CLEAR READ

*!**************************************************************************
*!
*!      Function : lfvDistrib
*!
*!**************************************************************************
*
FUNCTION lfvDistrib
*E301077,71 AMM Initialize
llOpInvad = .F.
*E301077,71 AMM end
IF laScrMode[1] .OR. laScrMode[2]
  ON KEY LABEL ALT+N
  ON KEY LABEL ALT+V
ELSE
  ON KEY LABEL ALT+N DO LFVNEWLINE
  ON KEY LABEL ALT+V DO LFVREMLINE
ENDIF

IF laData[3] <> 'C' .AND. !EMPTY(laData[20])
  ** MESSAGE : " This invoice is a credit card payment for "
  **           " vendor ð invoice ð.                       "
  **           "                    ® Ok ¯                 "
  =gfModalGen("TRM04034B00000","DIALOG",ALLTRIM(laData[20])+"|"+ALLTRIM(laData[21]))
  RETURN
ELSE
  *E301077,71 AMM Open file
  llOpInvad= gfOpenFile(gcDataDir+'APINVADT','DTYPCOD','SH')
  *E301077,71 AMM end
  IF laScrMode[3] .OR. laScrmode[4]
    SELECT(lcTmpDist)
    IF laScrMode[4]
      IF RECCOUNT() = 0
        SELECT(lcTmpDist)
        
        *B602946,1 Add this line to get the default AP Account [Begin]
        laData[33] = IIF(!EMPTY(APVENDOR.cAPAcct) , APVENDOR.cAPAcct ,;
                         IIF(SEEK(laData[4] , 'APDIV') .AND.;
                             !EMPTY(APDIV.cAPAcct) , APDIV.cAPAcct ,;
                             APSETUP.cAPAcct))
        *B602946,1 Add this line to get the default AP Account [End]
        
        *** Add default AP Account. ***
        APPEND BLANK
        REPLACE CVENDCODE WITH laData[1]  ,;
                CINVNO    WITH laData[2]  ,;
                DAPDTRDAT WITH laData[50] ,;
                LAPDPOST  WITH .F.        ,;
                CAPDGLACT WITH laData[33] ,;
                NAPDAMNT  WITH -laData[12],;
                CAPDACTID WITH 'A'        ,;
                CAPDREF   WITH laData[2]  ,;
                CFISFYEAR WITH lcDistYer  ,;
                CFSPPRDID WITH lcDistPrd  ,;
                CAPSESSNO WITH lcSequence ,;
                CAPDTRTYP WITH 'I'        ,;
                CSTATUS   WITH 'A'
        =gfAdd_Info(lcTmpDist)
      ELSE
        SELECT(lcTmpDist)
        LOCATE FOR CAPDACTID = 'A'
        REPLACE NAPDAMNT WITH -laData[12]
      ENDIF  
    ENDIF
    IF laScrMode[3] .AND. laData[12] <> lnOldAmnt
      LOCATE FOR CAPDACTID = 'A' .AND. EMPTY(CAPDSTAT)
      lnSavApRec = RECNO()
      IF CSTATUS = 'A'
        SCATTER MEMVAR MEMO
        m.cStatus  = 'A'
        m.cApdStat = ' '
        m.nApdAmnt = -laData[12]
        m.lAPDPost = .F.
        m.cBatchNo = ' '
        m.cTrnSledn= ' '
        m.cAPSessNo= lcSequence
        m.nEntryNo = 0
        GATHER MEMVAR MEMO
      ELSE
        REPLACE CAPDSTAT  WITH 'V'        ,;
                CAPSESSNO WITH lcSequence ,;
                CSTATUS   WITH 'M'
        SCATTER MEMVAR MEMO
        m.nApdAmnt = -1 * m.nApdAmnt
        m.cStatus  = 'A'
        m.lAPDPost = .F.
        m.cBatchNo = ' '
        m.cTrnSledn= ' '
        m.nEqvAmnt = -1 * m.nEqvAmnt
        m.nEntryNo = 0
        APPEND BLANK
        GATHER MEMVAR MEMO
        IF BETWEEN(lnSavApRec,0,RECCOUNT())
          GO lnSavApRec
        ENDIF
        SCATTER MEMVAR MEMO
        m.cStatus  = 'A'
        m.cApdStat = ' '
        m.nApdAmnt = -laData[12]
        m.lAPDPost = .F.
        m.cBatchNo = ' '
        m.cTrnSledn= ' '
        m.cAPSessNo= lcSequence
        m.nEntryNo = 0
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF
    ENDIF
    SELECT(lcTmpDist)
    GO TOP
    IF !EOF()
      SKIP 1
    ENDIF  
    DO CASE
      CASE !EMPTY(laData[38]) .AND. lnLineNo = 0
        SELECT APINVADT
        IF SEEK('T'+laData[38])
          SCAN REST WHILE CAUTMTYPE+CAUTMCODE = 'T'+laData[38]
            IF llApGlLink
              IF !SEEK(APINVADT.CAPDGLACT,'lcLinkChar')
                =gfModalGen("TRM04110B00000","DIALOG",ALLTRIM(laData[38])+'|'+ALLTRIM(APINVADT.CAPDGLACT))
                SELECT(lcTmpDist)
                SCAN
                  IF CAPDACTID <> 'A'
                    REPLACE CSTATUS WITH 'S'
                    DELETE
                  ENDIF
                ENDSCAN
                SELECT APINVHDR
                _CUROBJ = OBJNUM(laData[38])
                lcTmplate = 'ENABLE'
                SHOW GET laData[38]
                RETURN
              ELSE
                SELECT(lcTmpDist)
                *** Add the distribution account. ***
                APPEND BLANK
                REPLACE CVENDCODE WITH laData[1]  ,;
                        CINVNO    WITH laData[2]  ,;
                        DAPDTRDAT WITH laData[50] ,;
                        LAPDPOST  WITH .F.        ,;
                        CAPDREF   WITH laData[2]  ,;
                        CAPDACTID WITH 'D'        ,;
                        CFISFYEAR WITH lcDistYer  ,;
                        CFSPPRDID WITH lcDistPrd  ,;
                        CAPSESSNO WITH lcSequence ,;
                        CAPDTRTYP WITH 'I'        ,;
                        CSTATUS   WITH 'A'        ,;  
                        NAPDLINNO WITH lfvLineNo(),;
                        CTAXCODE  WITH APINVADT.CTAXCODE,;
                        CAPDGLACT WITH APINVADT.CAPDGLACT,;
                        NAPDAMNT  WITH APINVADT.NAPDAMNT * laData[12] / 100
                =gfAdd_Info(lcTmpDist)
                lnDistAmnt = lnDistAmnt + NAPDAMNT
              ENDIF
            ELSE
              SELECT(lcTmpDist)
              *** Add the distribution account. ***
              APPEND BLANK
              REPLACE CVENDCODE WITH laData[1]  ,;
                      CINVNO    WITH laData[2]  ,;
                      DAPDTRDAT WITH laData[50] ,;
                      LAPDPOST  WITH .F.        ,;
                      CAPDREF   WITH laData[2]  ,;
                      CAPDACTID WITH 'D'        ,;
                      CFISFYEAR WITH lcDistYer  ,;
                      CFSPPRDID WITH lcDistPrd  ,;
                      CAPSESSNO WITH lcSequence ,;
                      CAPDTRTYP WITH 'I'        ,;
                      CSTATUS   WITH 'A'        ,;  
                      NAPDLINNO WITH lfvLineNo(),;
                      CTAXCODE  WITH APINVADT.CTAXCODE,;
                      CAPDGLACT WITH APINVADT.CAPDGLACT,;
                      NAPDAMNT  WITH APINVADT.NAPDAMNT * laData[12] / 100
              =gfAdd_Info(lcTmpDist)
              lnDistAmnt = lnDistAmnt + NAPDAMNT
            ENDIF
            SELECT APINVADT
          ENDSCAN
        ENDIF
      *E300798,1 Intialize New variables 
      *CASE laScrMode[4] .AND. RECCOUNT(lcTmpDist) = 1
      CASE laScrMode[4] .AND. RECCOUNT(lcTmpDist) = 1 AND EMPTY(lcVenInvTypes)
        SELECT(lcTmpDist)
        
        *B602946,1 Add this line to get the default Expense Account [Begin]
        lcDistAcct = IIF(!EMPTY(APVENDOR.cExpAcct) , APVENDOR.cExpAcct ,;
                         IIF(SEEK(laData[4] , 'APDIV') .AND.;
                             !EMPTY(APDIV.cExpAcct) , APDIV.cExpAcct ,;
                             APSETUP.cExpAcct))
        *B602946,1 Add this line to get the default Expense Account [End]
        
        *** Add the distribution account. ***
        APPEND BLANK
        REPLACE CVENDCODE WITH laData[1]  ,;
                CINVNO    WITH laData[2]  ,;
                DAPDTRDAT WITH laData[50] ,;
                LAPDPOST  WITH .F.        ,;
                CAPDGLACT WITH lcDistAcct ,;
                NAPDAMNT  WITH laData[12] ,;
                CAPDREF   WITH laData[2]  ,;
                CAPDACTID WITH 'D'        ,;
                CFISFYEAR WITH lcDistYer  ,;
                CFSPPRDID WITH lcDistPrd  ,;
                CAPSESSNO WITH lcSequence ,;
                CAPDTRTYP WITH 'I'        ,;
                CSTATUS   WITH 'A'        ,;  
                NAPDLINNO WITH lfvLineNo()
        =gfAdd_Info(lcTmpDist)
        lnDistAmnt = lnDistAmnt + NAPDAMNT
    ENDCASE
    SELECT(lcTmpDist)
    GO TOP
    IF !EOF()
      SKIP 1
    ENDIF  
    *E300798,1 Disable Editing Distribution lines created for invoice lines
    *IF llLokPer
    *  llNewStat  = .F.
    *  SHOW GET pbNew      DISABLE
    *  SHOW GET pbRemove   DISABLE
    *  SHOW GET laData[33] DISABLE
    *  SHOW GET ibApAcc    DISABLE
    *ENDIF    && End of IF llLokPer
    IF llLokPer OR !EMPTY(lcVenInvType)
      llNewStat  = .F.
      SHOW GET pbNew    DISABLE
      SHOW GET pbRemove DISABLE
    ENDIF
    IF llLokPer
      SHOW GET laData[33] DISABLE
      SHOW GET ibApAcc    DISABLE
    ENDIF    && End of IF llLokPer
    *E300798,1 (End)
  ELSE
    SELECT APINVHDR
    IF !EOF()
      GO RECNO()
    ENDIF
    SELECT APDIST
    llNewStat = .F.
    SHOW GET pbNew      DISABLE
    SHOW GET pbRemove   DISABLE
    SHOW GET laData[33] DISABLE
    SHOW GET ibApAcc    DISABLE
  ENDIF
  RELEASE WINDOW (lcBrowsTtl)
  lcTmplate = 'DISABLE'
  SHOW GET laData[38] DISABLE
  SHOW GET ibTemplate DISABLE
  lcCodeFilt = ''
  =lfDispObjct()
  =lfBrowse()
  =lfRefresh()
  =gfActWind('CWRAPDISTR',lcTChldTitl,gcBaseWind)
ENDIF
*E301077,71 AMM Close file
IF USED('APINVADT') .AND. llOpInvad
  =gfCloseFile('APINVADT')
ENDIF
*E301077,71 AMM end

*!**************************************************************************
*!
*!      Function : lfvDisAct
*!
*!**************************************************************************
*
FUNCTION lfvDisAct

llCanSav = .T.

CLEAR READ

*!**************************************************************************
*!
*!      Function : lfwDiscAcct
*!
*!**************************************************************************
*
FUNCTION lfwDiscAcct

lcOldGLAcc = lcDisAcct

*!**************************************************************************
*!
*!      Function : lfvDiscAcct
*!
*!**************************************************************************
*
FUNCTION lfvDiscAcct

IF llBrowse .OR. lcDisAcct <> lcOldGlAcc
  =lfApAcs(.F.,llBrowse)
  llBrowse  = .F.
  IF EMPTY(STRTRAN(STRTRAN(lcDisAcct,'-'),'0'))
    lcDisAcct = lcOldGlAcc
    _CUROBJ   = _CUROBJ
  ENDIF
ENDIF
SHOW GET lcDisAcct

*!**************************************************************************
*!
*!      Function : lfBrowse
*!
*!**************************************************************************
*
FUNCTION lfBrowse
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")
IF laScrMode[3] .OR. laScrMode[4]
  SELECT(lcTmpDist)
  lcBrowsTtl = IIF(llLokPer,'View distribution','Add / Edit distribution')
ELSE
  SELECT APDIST
  lcBrowsTtl = 'View distribution'
ENDIF
IF APVENDOR.CTAXTYPE = 'T'
  lnDesWdth = 45 - lnApsAcLen
  lcBrowStr = "NAPDLINNO :4 :W=.F. :R :H='No.',"+;
              "CAPDGLACT :"+STR(lnApsAcLen,2)+" :H='Account' :P=lcApsAcMas :R ,"+;
              "CDESC=IIF(llApGlLink,ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,CAPDGLACT,lcLinkChar.CACCTCODE,'ACCTCODE')),' ') :"+STR(lnDesWdth,2)+" :R :H='Description' :R :W=.F.,"+;
              "NAPDAMNT :15 :H='Dist. amount' :R :W=.F."
  IF laScrMode[2] .OR. llLokPer
    SHOW GET ibDumm1   ENABLE
    SHOW GET ibTaxCode DISABLE
    SHOW GET puTax     DISABLE
    SHOW WINDOW (lcDisChld4) TOP IN WINDOW CWRAPDISTR
  ENDIF
ELSE
  lnDesWdth = 36 - lnApsAcLen
  *E300789,4  AMM Adjust to fit the new structure
  *lcBrowStr = "NAPDLINNO :4 :W=.F. :R :H='No.',"+;
              "CTAXDES=IIF(EMPTY(CTAXCODE),lcEmptyCode,LOOKUP(CODES.cdiscrep,gcAct_COMP+'CTAXCODE  '+CTAXCODE,CODES.CCODE_NO,'CCODE_NO')) :8 :H='Tax Code' :R :W=.F.,"+;
              "CAPDGLACT :"+STR(lnApsAcLen,2)+" :H='Account' :P=lcApsAcMas :R :W=.F.,"+;
              "CDESC=IIF(llApGlLink,ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,CAPDGLACT,lcLinkChar.CACCTCODE,'ACCTCODE')),' ') :"+STR(lnDesWdth,2)+" :R :H='Description' :W=.F.,"+;
              "NAPDAMNT :15 :H='Dist. amount' :R :W=.F."
  lcBrowStr = "NAPDLINNO :4 :W=.F. :R :H='No.',"+;
              "CTAXDES=IIF(EMPTY(CTAXCODE),lcEmptyCode,LOOKUP(CODES.cdiscrep,'N'+'CTAXCODE  '+CTAXCODE,CODES.CCODE_NO,'CCODE_NO')) :8 :H='Tax Code' :R :W=.F.,"+;
              "CAPDGLACT :"+STR(lnApsAcLen,2)+" :H='Account' :P=lcApsAcMas :R :W=.F.,"+;
              "CDESC=IIF(llApGlLink,ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,CAPDGLACT,lcLinkChar.CACCTCODE,'ACCTCODE')),' ') :"+STR(lnDesWdth,2)+" :R :H='Description' :W=.F.,"+;
              "NAPDAMNT :15 :H='Dist. amount' :R :W=.F."
  *E300789,4  AMM end
  IF laScrMode[2] .OR. llLokPer
    SHOW GET ibDumm1   DISABLE
    SHOW GET ibTaxCode DISABLE
    SHOW GET puTax     DISABLE
    SHOW WINDOW (lcDisChld3) TOP IN WINDOW CWRAPDISTR
  ENDIF
ENDIF
BROWSE FIELDS &lcBrowStr;
          FOR cInvNo + cVendCode + cApdTrTyp = (PADR(laData[2] , 12)) +;
              (laData[1]) .AND. cApdActId = 'D' .AND. cApdStat <> 'V';
              WINDOW (lcDischld2) ;
              WHEN lfwDisBrow();
              VALID :F (WONTOP() = lcBrowsTtl .AND. lfvBrowse()) .OR. gfStopBrow() ;
              IN WINDOW CWRAPDISTR;
              LOCK 0;
              NOAPPEND;
              NOCLEAR;
              NODELETE;
              NOWAIT;
              SAVE;
              TITLE lcBrowsTtl;
              NOEDIT &lcClrSchm
IF laScrMode[2]
  SELECT APINVHDR
  IF !EOF()
    GO RECNO()
  ENDIF
  SELECT APDIST
  LOCATE REST FOR CAPDSTAT <> 'V' .AND. CAPDACTID <> 'A'
  =lfDispObjct()
ENDIF  

*!**************************************************************************
*!
*!      Function : lfwDisBrow
*!
*!**************************************************************************
*
FUNCTION lfwDisBrow

IF SUBSTR(gcBaseWind,4,LEN(gcBaseWind)) = lcActProg
  =lfSetTrap()
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+C
  IF laScrMode[3] .OR. laScrMode[4]
    SELECT(lcTmpDist)
    IF cStatus = 'S'
      SCATTER MEMVAR MEMO
    ENDIF
  ELSE
    SELECT APDIST
  ENDIF
  SHOW WINDOW (lcBrowsTtl) REFRESH
  glFromBrow = .T.
  =lfDispObjct()
  =lfvTrapKey()
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse

SHOW WINDOW (lcBrowsTtl) REFRESH

=lfDispObjct()
=lfvClearTrap()
=lfResetTrap()

*!**************************************************************************
*!
*!      Function : lfwData_33
*!
*!**************************************************************************
*
FUNCTION lfwData_33

lcOldGLAcc = laData[33]
SELECT(lcTmpDist)
=lfResetTrap()
=lfDispObjct()
=lfvClearTrap()
IF SUBSTR(gcBaseWind,4,LEN(gcBaseWind)) = lcActProg
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      Function : lfvData_33
*!
*!**************************************************************************
*
FUNCTION lfvData_33

IF lcOldGLAcc = laData[33] .AND. !llBrowse
  RETURN
ENDIF

IF llBrowse .OR. laData[33] <> lcOldGlAcc
  RELEASE PAD _BROWSE OF _MSYSMENU
  *ON KEY LABEL ALT+B
  ON KEY LABEL ALT+C
  =lfApAcs(.F.,llBrowse)
  llBrowse = .F.

  IF EMPTY(STRTRAN(STRTRAN(laData[33],'-'),'0'))
    laData[33] = lcOldGLAcc
  ELSE
    IF laData[33] <> lcOldGlAcc
      IF (laData[25]+laData[26]+laData[34]) > 0
        ** MESSAGE : " This invoice is ð paid.  You have "
        **           " to make an adjustment entry in GL "
        **           " to clear the old AP account.      "
        **           "                ® Ok ¯             "
        =gfModalGen('QRM04039B00000','Dialog',IIF(laData[25]+laData[26]+laData[34] = laData[12],lcTFully,lcTPart)) 
      ENDIF

      SELECT(lcTmpDist)
      lnSavRecNo = RECNO()
      LOCATE FOR CAPDACTID = 'A'
      lnSavApRec = RECNO()
      IF laScrMode[4]
        REPLACE CAPDGLACT WITH laData[33]
        =gfAdd_Info(lcTmpDist)

        IF BETWEEN(lnSavRecNo,0,RECCOUNT())
          GO lnSavRecNo
        ENDIF

      ELSE
        IF CSTATUS = 'S' .AND. UPDATED() && IF the record is modified.

          =lfvUpdTemp()
          IF BETWEEN(lnSavApRec,0,RECCOUNT())
            GO lnSavApRec
          ENDIF
          REPLACE CAPDGLACT WITH laData[33] ,;
                  CSTATUS   WITH 'M'        ,;  
                  LAPDPOST  WITH .F.        ,;
                  CBATCHNO  WITH ' '        ,;
                  CTRNSLEDN WITH ' '        ,;
                  CAPSESSNO WITH lcSequence ,;
                  CUPDSERNO WITH ' '        ,;
                  nEntryNo  WITH 0
          =gfAdd_Info(lcTmpDist)
          IF BETWEEN(lnSavRecNo,0,RECCOUNT())
            GO lnSavRecNo
          ENDIF
        ENDIF
      ENDIF
      IF BETWEEN(lnSavRecNo,0,RECCOUNT())
        GO lnSavRecNo
      ENDIF
    ENDIF
  ENDIF
ENDIF
llBrowse = .F.
SHOW GET laData[33]
SELECT(lcTmpDist)
=lfvTrapKey()
IF SUBSTR(gcBaseWind,4,LEN(gcBaseWind)) = lcActProg
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_36
*!
*!**************************************************************************
* Valid function for get field laData[36] representing factor
*
FUNCTION lfwData_36

lcOldFact = laData[36]

*!**************************************************************************
*!
*!      Function: lfvData_36
*!
*!**************************************************************************
* Valid function for get field laData[36] representing factor
*
FUNCTION lfvData_36

IF !llBrowse .AND. lcOldFact = laData[36]
  RETURN
ENDIF
*E301077,71 AMM Open file
llopFact = gfOpenFile(gcSysHome+'SYCFACT','CFACCODE','SH')
*E301077,71 AMM end
IF llBrowse .OR. !EMPTY(laData[36])
  IF lfGetFac(lcOldFact, llBrowse)
    laData[6] = SYCFACT.cFacComp
    laData[7] = SYCFACT.cAddress1
    laData[8] = SYCFACT.cAddress2
    laData[9] = lfGetAdr('SYCFACT','CFACCODE')
    laData[39] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,1)
    laData[40] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,2)
    laData[41] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,3)
    laData[42] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,4)
    laData[43] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,5)
  ENDIF
ELSE
  STORE SPACE(40) TO laData[6],laData[7],laData[8],laData[9],laData[39],;
                     laData[40],laData[41],laData[42],laData[43]
ENDIF
SHOW GET laData[6]
SHOW GET laData[7] 
SHOW GET laData[8] 
SHOW GET laData[9] 

=lfShiftlaData(lnStartAdr)
SHOW GET laAddrs[1]
SHOW GET laAddrs[2]
SHOW GET laAddrs[3]
llBrowse = .F.
IF USED('SYCFAC') .AND. llopFact
  =gfCloseFile('SYCFAC')
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_38
*!
*!**************************************************************************
* Valid function for get field laData[38] representing template code.
*
FUNCTION lfwData_38

lcOldTmplt = laData[38]

*!**************************************************************************
*!
*!      Function: lfvData_38
*!
*!**************************************************************************
* Valid function for get field laData[38] representing template code.
*
FUNCTION lfvData_38

*B601419 IF Statment to check if the template field is empty
IF EMPTY(laData[38]) .AND. !llBrowse
  RETURN
ENDIF   && End of IF

SELECT APINVAHD

laData[38]     = ALLTRIM(laData[38])
laData[38]     = IIF(ISDIGIT(LEFT(laData[38],1)),;
                         PADL(laData[38], FSIZE('cAutMCode','APINVAHD'),'0'),;
                         PADR(laData[38],FSIZE('cAutMCode','APINVAHD')))
lcTmplate = 'ENABLE'
SHOW GET laData[38]

IF !llBrowse .AND. lcOldTmplt = laData[38]
  RETURN
ENDIF

IF llBrowse .OR. !EMPTY(laData[38]) .AND. LASTKEY() = 13
  IF llBrowse .OR. !SEEK('T'+laData[38]) .OR. ATC("?",laData[38]) > 0
    lnClosRec  = RECNO(0)
    IF SEEK('T')
      DIMENSION laTemp[2]

      laTemp = ''

      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl

      lcBrFields = "CAUTMCODE :H= 'Template code'"

      lcFile_Ttl = "Automatic invoices header"
      
      IF BETWEEN(lnClosRec,1,RECCOUNT('APINVAHD'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF

      =gfBrows([FOR CAUTMTYPE = 'T'],'CAUTMTYPE,CAUTMCODE','laTemp')

      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle

      IF !EMPTY(laTemp[1])
        laData[37] = laTemp[1]
        laData[38] = laTemp[2]
      ELSE

        *B601419,1 No need for This line [Begin]
        *laData[37] = ' '
        *B601419,1 No need for This line [End]

        laData[38] = lcOldTmplt
      ENDIF
    ELSE
      ** MESSAGE : " There are no records to display. "
      **           "               ® Ok ¯             "
      =gfModalGen('QRM00052B00000','Dialog')
      laData[37] = ' '
      laData[38] = lcOldTmplt
    ENDIF
  ELSE
    IF SEEK('T'+laData[38])
      laData[37]  = APINVAHD.CAUTMTYPE
      laData[38]  = APINVAHD.CAUTMCODE
    ENDIF
  ENDIF
ELSE
  laData[37] = ' '
  laData[38] = lcOldTmplt
ENDIF

IF !EMPTY(laData[38])
  IF APVENDOR.CTAXTYPE <> APINVAHD.CAUTMBASE
    ** MESSAGE : "You cannot use this distribution    "
    **           "template since the vendor tax type  "
    **           "is ð and the template lines uses tax"
    **           "type ð.                             "
    **           "                ® Ok ¯              "
    lcVendTax = IIF(APVENDOR.CTAXTYPE  = 'L','line item tax','percentage of the total')
    lcTempTax = IIF(APINVAHD.CAUTMBASE = 'L','line item tax','percentage of the total')
    =gfModalGen("TRM04109B00000","DIALOG",lcVendTax+"|"+lcTempTax)
    laData[37] = ' '
    laData[38] = lcOldTmplt
  ELSE
    IF WVISIBLE('CWRAPDISTR')
      =gfChClose('CWRAPDISTR')
    ENDIF
  ENDIF
ENDIF

llBrowse = .F.
lcTmplate = 'ENABLE'
SHOW GET laData[38]

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function : lfwDistAmnt
*!
*!**************************************************************************
*
FUNCTION lfwDistAmnt

SHOW GET lcAPDGlAct
lnOldAmnt = lnApdAmnt
=lfvClearTrap()

*!**************************************************************************
*!
*!      Function : lfvDistAmnt
*!
*!**************************************************************************
*
FUNCTION lfvDistAmnt

SELECT(lcTmpDist)

IF lnOldAmnt <> lnApdAmnt
  lnDistAmnt = lnDistAmnt - lnOldAmnt + lnApdAmnt
ENDIF

IF lnOldAmnt <> lnApdAmnt
  IF laScrMode[3]
    lnSavRecNo = RECNO()
    IF cStatus = 'S' .AND. UPDATED() && IF the record is modified.

      =lfvUpdTemp()

      IF BETWEEN(lnSavRecNo,0,RECCOUNT())
        GO lnSavRecNo
      ENDIF

      REPLACE NAPDAMNT  WITH lnApdAmnt  ,;
              CSTATUS   WITH 'M'        ,;  
              LAPDPOST  WITH .F.        ,;
              CBATCHNO  WITH ' '        ,;
              CTRNSLEDN WITH ' '        ,;
              CAPSESSNO WITH lcSequence ,;
              CUPDSERNO WITH ' '
      
      *B801498,1 Add these line to clear the entry number [Begin]
      REPLACE nEntryNo  WITH 0
      *B801498,1 Add these line to clear the entry number [End]
      
      =gfAdd_Info(lcTmpDist)
    ELSE
      REPLACE NAPDAMNT WITH lnApdAmnt
    ENDIF
  ELSE
    REPLACE NAPDAMNT WITH lnApdAmnt
  ENDIF
ENDIF

SHOW GET lnApdAmnt
*!B500684,1 MAN 06/01/95
*=lfRefresh()
=lfRefresh(lcDisChld1)
=lfvTrapKey()

*!**************************************************************************
*!
*!      Function: lfwApGlAcc
*!
*!**************************************************************************
*
FUNCTION lfwApGlAcc

SELECT(lcTmpDist)
lcOldGLAcc = lcApdGLAct
=lfvClearTrap()

*!**************************************************************************
*!
*!      Function: lfvApGlAcc
*!
*!**************************************************************************
*
FUNCTION lfvApGlAcc

IF lcOldGLAcc = lcApdGLAct .AND. !llBrowse
  RETURN
ENDIF

IF llBrowse .OR. lcApdGlAct <> lcOldGLAcc
  RELEASE PAD _BROWSE OF _MSYSMENU
  *ON KEY LABEL ALT+B
  ON KEY LABEL ALT+C
  =lfApAcs(.F.,llBrowse)
  IF EMPTY(STRTRAN(STRTRAN(lcApdGlAct,'-'),'0'))
    lcApdGlAct = lcOldGLAcc
  ENDIF
ENDIF

llBrowse = .F.

SELECT(lcTmpDist)

IF lcOldGLAcc <> lcApdGLAct .AND. !EMPTY(STRTRAN(STRTRAN(lcApdGLAct,'-'),'0'))
  IF laScrMode[3]
    lnSavRecNo = RECNO()
    IF cStatus = 'S' .AND. UPDATED() && IF the record is modified.
      =lfvUpdTemp()

      IF BETWEEN(lnSavRecNo,0,RECCOUNT())
        GO lnSavRecNo
      ENDIF

      REPLACE CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApdGLAct,'-'),'0')),;
                             SPACE(lcApdGlAct), lcApdGlAct),;
              CSTATUS   WITH 'M'        ,;
              LAPDPOST  WITH .F.        ,;
              CBATCHNO  WITH ' '        ,;
              CTRNSLEDN WITH ' '        ,;
              CAPSESSNO WITH lcSequence ,;
              CUPDSERNO WITH ' '
      
      *B801498,1 Add these line to clear the entry number [Begin]
      REPLACE nEntryNo  WITH 0
      *B801498,1 Add these line to clear the entry number [End]
      
      =gfAdd_Info(lcTmpDist)
    ELSE
      REPLACE CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApdGLAct,'-'),'0')),;
                             SPACE(lcApdGlAct), lcApdGlAct)
    ENDIF
  ELSE
    REPLACE CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApdGLAct,'-'),'0')),;
                             SPACE(lcApdGlAct), lcApdGlAct)
  ENDIF
ELSE
  lcApdGLAct = lcOldGLAcc
ENDIF

SHOW GET lcApdGLAct

SHOW WINDOW (lcBrowsTtl) REFRESH
*!B500684,1 MAN 06/01/95
*=lfRefresh()
=lfRefresh(lcDisChld5)
=lfvTrapKey()

SELECT(lcTmpDist)

*!**************************************************************************
*!
*!      Function: lfwTaxCode
*!
*!**************************************************************************
* When function of the tax popup.
FUNCTION lfwTaxCode

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcCodeFilt = 'CTAXCODE'
LOCATE

SELECT (lcTmpDist)
lcOldTaxCod = lcTaxCode
*!B500684,1 MAN 06/01/95
=lfResetTrap()
=lfvClearTrap()

*!**************************************************************************
*!
*!      Function: lfvTaxCode
*!
*!**************************************************************************
* Validation of the tax popup
*
FUNCTION lfvTaxCode

PRIVATE lcOldTax, lcOldAcct

lcOldTax  = &lcTmpDist..cTaxCode
lcOldAcct = &lcTmpDist..cApDGlAct
puTax     = CODES.cdiscrep
lcTaxCode = CODES.cCode_No
SHOW GET puTax
lcCodeFilt = ''

IF LASTKEY() = 13 OR LASTKEY() = 32
  IF !EMPTY(lcTaxCode)
    =gfRltFld(lcTaxCode , @laTaxAry , 'CTAXCODE')
    lcApdGlAct = IIF(!EMPTY(STRTRAN(STRTRAN(lcApdGlAct,'-'),'0')),;
                     SUBSTR(lcApdGlAct,1,lnApsAcLen), lcEmptyAcc)

    ** If there is a G/L link, validate the G/L input account,
    ** from the chart of accounts.
    ** If it is not found, present the following message and
    ** reset tax code to N/A.
    
    ** Message :    "    G/L input account ð  for tax code ð     "
    **              "    is not found in the chart of accounts.  "
    **              "    You cannot select this tax code.        "
    
    IF llApGlLink .AND. !SEEK(lcApDGlAct, 'lcLinkChar')
      lcTaxDesMs = IIF(_DOS,lcTaxDes, puTax)
      =gfModalGen("TRM04091B00000","DIALOG",ALLTRIM(lcApDGlAct)+'|'+ALLTRIM(lcTaxDesMs))
      IF lcTaxCode <> lcOldTax
        lcApDGlAct = lcOldAcct
        lcTaxCode  = lcOldTax
        lcTaxDes   = gfCodDes(lcTaxCode , 'CTAXCODE')
        puTax      = lcTaxDes
      ELSE
        lcTaxCode  = SPACE(6)
        lcApdGlAct = lcOldAcct
        lcTaxDes   = lcEmptyCode
        puTax      = lcTaxDes
      ENDIF
      SHOW GET puTax
    ENDIF
  ENDIF
  IF !EMPTY(lcTaxCode)
    SHOW GET lcApdGlAct DISABLE
    SHOW GET ibApGlAcc  DISABLE
    llGLActSta = .F.
  ELSE
    SHOW GET lcApdGlAct ENABLE
    SHOW GET ibApGlAcc  ENABLE
    llGLActSta = .T.
  ENDIF
ENDIF
SELECT(lcTmpDist)
IF lcOldTaxCod <> lcTaxCode
  IF laScrMode[3]
    lnSavRecNo = RECNO()
    IF cStatus = 'S' .AND. UPDATED() && IF the record is modified.
      =lfvUpdTemp()
      IF BETWEEN(lnSavRecNo,0,RECCOUNT())
        GO lnSavRecNo
      ENDIF  
      REPLACE CTAXCODE  WITH lcTaxCode       ,;
              CAPDGLACT WITH lcApdGLAct      ,;
              CSTATUS   WITH 'M'             ,;  
              LAPDPOST  WITH .F.             ,;
              CBATCHNO  WITH ' '             ,;
              CTRNSLEDN WITH ' '             ,;
              CAPSESSNO WITH lcSequence      ,;
              CUPDSERNO WITH ' '
      
      *B801498,1 Add these line to clear the entry number [Begin]
      REPLACE nEntryNo  WITH 0
      *B801498,1 Add these line to clear the entry number [End]
      
      =gfAdd_Info(lcTmpDist)
    ELSE
      REPLACE CTAXCODE WITH lcTaxCode        ,;
              CAPDGLACT WITH lcApdGLAct
    ENDIF  
  ELSE
    REPLACE CTAXCODE WITH lcTaxCode        ,;
            CAPDGLACT WITH lcApdGLAct
  ENDIF
ENDIF
=lfRefresh(lcDisChld1+','+lcDisChld3+','+lcDisChld4+','+lcDisChld5)
=lfvTrapKey()
DEACTIVATE POPUP puTaxDes

*!**************************************************************************
*!
*!      Function: lfDispObjct
*!
*!**************************************************************************
* Valid function for the objects display in the child screen.
*
FUNCTION lfDispObjct

SELECT IIF(laScrMode[2],'APDIST',lcTmpDist)
lcApdGLAct = IIF(!EMPTY(cApdGLAct),cApdGlAct,lcEmptyAcc)
lnApdAmnt  = nApdAmnt
lcTaxCode  = CTAXCODE

*B607390,1 ALB Fix Bug when removing line from dist. lines that effect in apdist [Begin]
IF !laScrMode[2] AND lnLineNo <= 0
  lnApdAmnt  = 0
  lcTaxCode  = ''
ENDIF
*B607390,1 ALB Fix Bug when removing line from dist. lines that effect in apdist [end]

STORE gfCodDes(lcTaxCode,'CTAXCODE') TO lcTaxDes, puTax
IF laScrMode[2] OR llLokPer OR !EMPTY(lcVenInvTypes) OR lnLineNo = 0
  SHOW GET ibApGlAcc  DISABLE
  SHOW GET lcApdGLAct DISABLE
  SHOW GET lnApdAmnt  DISABLE
  SHOW GET ibTaxCode  DISABLE
  SHOW GET puTax      DISABLE
ELSE
  IF EMPTY(lcTaxCode)
    SHOW GET lcApdGLAct ENABLE
    SHOW GET ibApGlAcc  ENABLE
  ELSE
    SHOW GET ibApGlAcc  DISABLE
    SHOW GET lcApdGLAct DISABLE
  ENDIF  
  SHOW GET lnApdAmnt ENABLE
  SHOW GET puTax     ENABLE
ENDIF  
lcTaxDes = gfCodDes(lcTaxCode , 'CTAXCODE')
IF SUBSTR(gcBaseWind,4,LEN(gcBaseWind)) = lcActProg
  =lfRefresh(lcDisChld1+','+lcDisChld3+','+lcDisChld4+','+lcDisChld5)
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvLineNo
*!
*!**************************************************************************
* 
FUNCTION lfvLineNo

lnLineNo = lnLineNo + 1

RETURN lnLineNo

*!**************************************************************************
*!
*!      Function: lfvTrapKey
*!
*!**************************************************************************
* 
FUNCTION lfvTrapKey

*B801499,1 Change these IF Condition to prevent the user from editing the
*Distribution lines if the Posting date falls within a locked
*period [Begin]

*IF !laScrMode[2]

*B801499,1 If not View mode and the Posting date do not falls within a
*locked period
*E300798,1 Disable Editing Distribution lines created for invoice lines
*IF !laScrMode[2] .AND. !llLokPer
IF !laScrMode[2] .AND. !llLokPer .AND. EMPTY(lcVenInvType)
*E300798,1 (End)
*B801499,1 Change these IF Condition to prevent the user from editing [End]
  
  IF lnDistAmnt = laData[12]
    SHOW GET pbNew ENABLE   && New 10/02/1994 
    llNewStat = .F.
    ON KEY LABEL ALT+N
  ELSE
    SHOW GET pbNew ENABLE  
    llNewStat = .T.
    ON KEY LABEL ALT+N DO LFVNEWLINE
  ENDIF
  
  *B801499,1 Add these lines to prevent the user from editing the
  *Distribution lines if the Posting date falls within a locked
  *period [Begin]
  
  SHOW GET laData[33] ENABLE
  SHOW GET ibApAcc ENABLE
  
  *B801499,1 Add these lines to prevent the user from editing [END]
  
  IF lnLineNo <> 0
    SHOW GET pbRemove ENABLE
    ON KEY LABEL ALT+V DO LFVREMLINE
  ELSE
    SHOW GET pbRemove DISABLE
    ON KEY LABEL ALT+V 
  ENDIF
ELSE
  
  *B801499,1 Add these lines to prevent the user from editing the
  *Distribution lines if the Posting date falls within a locked
  *period [Begin]

  *E300798,1 Disable Editing Distribution lines created for invoice lines
  *SHOW GET laData[33] DISABLE
  *SHOW GET ibApAcc    DISABLE
  IF laScrMode[2] .OR. llLokPer
    SHOW GET laData[33] DISABLE
    SHOW GET ibApAcc    DISABLE
  ELSE
    SHOW GET laData[33] ENABLE
    SHOW GET ibApAcc    ENABLE
  ENDIF
  *E300798,1 (End)
  
  *B801499,1 Add these lines to prevent the user from editing [END]
  llNewStat = .F.  
  ON KEY LABEL ALT+N 
  ON KEY LABEL ALT+V 
  SHOW GET pbNew DISABLE
  SHOW GET pbRemove DISABLE
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvClearTrap
*!
*!**************************************************************************
* 
FUNCTION lfvClearTrap

*B801499,1 Change these IF Condition to prevent the user from editing the
*Distribution lines if the Posting date falls within a locked
*period [Begin]

*IF !laScrMode[2]

*B801499,1 If not View mode and the Posting date do not falls within a
*locked period

*E300798,1 Disable Editing Distribution lines created for invoice lines
*IF !laScrMode[2] .AND. !llLokPer
IF !laScrMode[2] .AND. !llLokPer .AND. EMPTY(lcVenInvType)
*E300798,1 (End)

*B801499,1 Change these IF Condition to prevent the user from editing [End]

  IF lnDistAmnt = laData[12]
    SHOW GET pbNew ENABLE
    llNewStat = .F.
    ON KEY LABEL ALT+N 
  ELSE
    SHOW GET pbNew ENABLE
    llNewStat = .T.
    ON KEY LABEL ALT+N DO LFVNEWLINE
  ENDIF
  
  *B801499,1 Add these lines to prevent the user from editing the
  *Distribution lines if the Posting date falls within a locked
  *period [Begin]
  
  SHOW GET laData[33] ENABLE
  SHOW GET ibApAcc ENABLE
  
  *B801499,1 Add these lines to prevent the user from editing [END]
  
  IF lnLineNo <> 0
    SHOW GET pbRemove ENABLE
    ON KEY LABEL ALT+V DO LFVREMLINE
  ELSE
    SHOW GET pbRemove DISABLE
    ON KEY LABEL ALT+V
  ENDIF
ELSE
  llNewStat = .F.
  ON KEY LABEL ALT+N
  ON KEY LABEL ALT+V
  SHOW GET pbNew DISABLE
  SHOW GET pbRemove DISABLE
  
  *B801499,1 Add these lines to prevent the user from editing the
  *Distribution lines if the Posting date falls within a locked
  *period [Begin]

  *E300798,1 Disable Editing Distribution lines created for invoice lines  
  *SHOW GET laData[33] DISABLE
  *SHOW GET ibApAcc DISABLE
  IF laScrMode[2] .OR. llLokPer 
    SHOW GET laData[33] DISABLE
    SHOW GET ibApAcc DISABLE
  ENDIF
  *E300798,1 (End)
  
  *B801499,1 Add these lines to prevent the user from editing [END]
  
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvNewLine
*!
*!**************************************************************************
* 
FUNCTION lfvNewLine

*B602946,1 Remove this condition to refresh the default Expense Account every
*          time a new line is added [Begin]
*IF EMPTY(lcDistAcct)
*B602946,1 Remove this condition to refresh the default Expense Account [End]
  
  IF !EMPTY(APVENDOR.CEXPACCT)
    lcDistAcct = APVENDOR.CEXPACCT
  ELSE
    IF SEEK(laData[4],'APDIV') .AND. !EMPTY(APDIV.CEXPACCT)
      lcDistAcct = APDIV.CEXPACCT
    ELSE
      lcDistAcct = APSETUP.CEXPACCT
    ENDIF
  ENDIF
  
*B602946,1 Remove this condition to refresh the default Expense Account every
*          time a new line is added [Begin]
*ENDIF  
*B602946,1 Remove this condition to refresh the default Expense Account [End]

SELECT(lcTmpDist)
** Add the distribution account. **
APPEND BLANK
*C100728,1 M.H Begin.
*REPLACE CVENDCODE WITH laData[1]  ,;
        CINVNO    WITH laData[2]  ,;
        DAPDTRDAT WITH laData[10] ,;
        LAPDPOST  WITH .F.        ,;
        CAPDGLACT WITH lcDistAcct ,;
        NAPDAMNT  WITH laData[12] - lnDistAmnt,;
        CAPDACTID WITH 'D'        ,;
        CFISFYEAR WITH laData[32] ,;
        CFSPPRDID WITH laData[31] ,;
        CAPSESSNO WITH lcSequence,;
        CAPDTRTYP WITH 'I'        ,;
        CSTATUS   WITH 'A'        ,;
        NAPDLINNO WITH lfvLineNo(),;
        CAPDREF   WITH laData[2]
IF TYPE('APINVHDR.DPOSTDATE') = 'D'
  REPLACE CVENDCODE WITH laData[1]  ,;
          CINVNO    WITH laData[2]  ,;
          DAPDTRDAT WITH laData[50] ,;
          LAPDPOST  WITH .F.        ,;
          CAPDGLACT WITH lcDistAcct ,;
          NAPDAMNT  WITH laData[12] - lnDistAmnt,;
          CAPDACTID WITH 'D'        ,;
          CFISFYEAR WITH lcDistYer  ,;
          CFSPPRDID WITH lcDistPrd  ,;
          CAPSESSNO WITH lcSequence,;
          CAPDTRTYP WITH 'I'        ,;
          CSTATUS   WITH 'A'        ,;
          NAPDLINNO WITH lfvLineNo(),;
          CAPDREF   WITH laData[2]
ELSE
  REPLACE CVENDCODE WITH laData[1]  ,;
          CINVNO    WITH laData[2]  ,;
          DAPDTRDAT WITH laData[10] ,;
          LAPDPOST  WITH .F.        ,;
          CAPDGLACT WITH lcDistAcct ,;
          NAPDAMNT  WITH laData[12] - lnDistAmnt,;
          CAPDACTID WITH 'D'        ,;
          CFISFYEAR WITH laData[32] ,;
          CFSPPRDID WITH laData[31] ,;
          CAPSESSNO WITH lcSequence,;
          CAPDTRTYP WITH 'I'        ,;
          CSTATUS   WITH 'A'        ,;
          NAPDLINNO WITH lfvLineNo(),;
          CAPDREF   WITH laData[2]
ENDIF
*C100728,1 M.H End.
=gfAdd_Info(lcTmpDist)

IF APVENDOR.CTAXTYPE = 'T'
  _CUROBJ = OBJNUM(lcApdGLAct)
ELSE
  _CUROBJ = OBJNUM(ibTaxCode)
ENDIF  

lnDistAmnt = lnDistAmnt + NAPDAMNT

lcTmplate = 'DISABLE'
SHOW GET laData[38] DISABLE
SHOW GET ibTemplate DISABLE

*SHOW WINDOW ALL REFRESH SAME
SHOW WINDOW (lcBrowsTtl) REFRESH
SHOW WINDOW (lcBrowsTtl) TOP IN WINDOW CWRAPDISTR

=lfDispObjct()
*!B500684,1 MAN 06/01/95
=lfvTrapKey()
=lfRefresh()

*!**************************************************************************
*!
*!      FUNCTION : lfvRemLine
*!
*!**************************************************************************
* 
FUNCTION lfvRemLine

SELECT(lcTmpDist)

IF gfModalGen("QRM00007B00007","ALERT") = 1

  lnSavRecNo = 0
  lnDistAmnt = lnDistAmnt - nApdAmnt
  *B607390,1 ALB Fix Bug when removing line from dist. lines that effect in apdist [Begin]
  *lnLineNo   = lnLineNo - 1
  *B607390,1 ALB Fix Bug when removing line from dist. lines that effect in apdist [end]
  
  DO CASE
    CASE cStatus = 'A'
      REPLACE CSTATUS WITH 'S'
      DELETE
      IF lnLineNo > 0 .AND. !EOF()
        SKIP
        lnSavRecNo = RECNO()
        REPLACE REST NAPDLINNO WITH NAPDLINNO - 1,;
                     CUPDSERNO WITH IIF(laScrMode[3] .AND. CSTATUS $ 'MS','M',' ')
      ENDIF

    CASE cStatus = 'S'
      REPLACE CAPDSTAT WITH 'V',;
              CSTATUS  WITH 'M'
            
      SCATTER MEMVAR MEMO
      m.lApdPost = .F.
      m.cBatchNo = ' '
      m.cTrnSledn= ' '
      m.cApSessNo= lcSequence
      m.nAPDAmnt = 0 - m.nAPDAmnt
      m.cApdStat = 'V'
      m.cStatus  = 'A'
      m.nEqvAmnt = 0 - m.nEqvAmnt
      
      *B801498,1 Add these line to clear the entry number [Begin]
      m.nEntryNo = 0
      *B801498,1 Add these line to clear the entry number [End]
      
      DELETE

      IF lnLineNo > 0 .AND. !EOF()
        SKIP
        lnSavRecNo = RECNO()
        REPLACE REST NAPDLINNO WITH NAPDLINNO - 1,;
                     CUPDSERNO WITH IIF(laScrMode[3] .AND. CSTATUS $ 'MS','M',' ')
      ENDIF
      APPEND BLANK
      GATHER MEMVAR MEMO
    
    CASE cStatus = 'M'
      REPLACE CSTATUS WITH 'D'
      DELETE
      IF lnLineNo > 0 .AND. !EOF()
        SKIP
        lnSavRecNo = RECNO()
        REPLACE REST NAPDLINNO WITH NAPDLINNO - 1,;
                     CUPDSERNO WITH IIF(laScrMode[3] .AND. CSTATUS $ 'MS','M',' ')
      ENDIF
  ENDCASE  
  *B607390,1 ALB Fix Bug when removing line from dist. lines that effect in apdist [Begin]
  lnLineNo   = lnLineNo - 1
  *B607390,1 ALB Fix Bug when removing line from dist. lines that effect in apdist [end]
  
  IF lnSavRecNo <> 0 .AND. lnSavRecNo <= RECCOUNT()
    GO lnSavRecNo
  ELSE
    IF !BOF()
      SKIP -1
    ENDIF  
  ENDIF  
ENDIF

*B602946,1 Change this condition to enable the template field only if the
*          "Vendor Invoice Types" is empty [Begin]
*IF lnLineNo = 0
IF lnLineNo = 0 .AND. EMPTY(lcVenInvTypes)
*B602946,1 Change this condition to enable the template field [End]
  
  lcTmplate = 'ENABLE'
  SHOW GET laData[38] ENABLE
  SHOW GET ibTemplate ENABLE
ELSE
  lcTmplate = 'DISABLE'
  SHOW GET laData[38] DISABLE
  SHOW GET ibTemplate DISABLE
ENDIF  

*SHOW WINDOW ALL REFRESH SAME
SHOW WINDOW (lcBrowsTtl) REFRESH
SHOW WINDOW (lcBrowsTtl) TOP IN WINDOW CWRAPDISTR

=lfDispObjct()

=lfvTrapKey()
  
=lfRefresh()

*B601419,1 This lines was added by HS [Begin]
*B601419,1 IF Statment to check if ther was no lines in the browse
*          and the invoice was a automatic Recurring  
IF lnLineNo = 0 .AND. laData[37] = 'R'
  laData[37] = ' '
  laData[38] = '        '
  SHOW GET laData[38]
  lcLabel = 'Template :'
  =lfRefresh(gcBaseWind)
ENDIF    && End of IF
*B601419,1 This lines was added by HS [End]

*!**************************************************************************
*!
*!      Function: lfvClsChld
*!
*!**************************************************************************
*
FUNCTION lfvClsChld

*B601419,1 This lines was added by HS [Begin]
*B601419,1 IF Statment to check if the screen mode is Edit or Add mode
IF laScrMode[3] .OR. laScrMode[4] 
  *B601419,1 IF Statment to check if ther is no lines in the browse
  
  *B602946,1 Change this condition to enable the template field only if the
  *          "Vendor Invoice Types" is empty [Begin]
  *IF lnLineNo = 0 
  IF lnLineNo = 0 .AND. EMPTY(lcVenInvTypes)
  *B602946,1 Change this condition to enable the template field [End]
  
    lcTmplate = 'ENABLE'
  ELSE     && Else 
    lcTmplate = 'DISABLE'
  ENDIF    && End of IF
  SHOW GET laData[38] &lcTmplate
  SHOW GET ibTemplate &lcTmplate
ENDIF    && End of IF
*B601419,1 This lines was added by HS [End]

ON KEY LABEL ALT+N
ON KEY LABEL ALT+V

IF laScrMode[3] .OR. laScrMode[4]
  _CUROBJ = OBJNUM(pbSav)
ELSE
  _CUROBJ = OBJNUM(pbCls)
ENDIF

RELEASE PAD _BROWSE OF _MSYSMENU
*ON KEY LABEL ALT+B
ON KEY LABEL ALT+C

=gfChClose('CWRAPDISTR')

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"
PROCEDURE lpSavScr
EXTERNAL ARRAY laScrMode


*--B604454,1 AAN Get the Period# and Year for both dates invoice & Posting. If the Invoice date
*-- prior than postinf date give a warning message to the user. [Begin]
PRIVATE lcPstPrd,lcPstYer,lcInvPrd,lcInvYer
STORE '' to lcPstPrd,lcPstYer,lcInvPrd,lcInvYer
=lfVlDate(gcPrnt_Cmp , @lcPstPrd , @lcPstYer , laData[50])
=lfVlDate(gcPrnt_Cmp , @lcInvPrd , @lcInvYer , laData[10])
IF VAL(lcPstYer) < VAL(lcInvYer)

  *B604561,1 Add an option to ask user to Proceed/Modify. [Begin]
  *=gfModalGen("TRM04197B00000","DIALOG","invoices|"+laData[1])
  *llCSave = .F.
  ** Return to posting date 
  *_CUROBJ = OBJNUM(laData[50])
  *RETURN
  *B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [Begin]
  *IF gfModalGen("TRM04197B42008","DIALOG","invoices|"+laData[1]) = 2
  IF gfModalGen("TRM04197B04012","DIALOG","invoices|"+laData[1]) = 1
  *B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [end]
    llCSave = .F.
    *-- Return to posting date 
    _CUROBJ = OBJNUM(laData[50])
    RETURN      
  ENDIF
  *B604561,1 Add an option to ask user to Proceed/Modify. [End]

ELSE
  IF VAL(lcPstYer) = VAL(lcInvYer)
    IF VAL(lcPstPrd) < VAL(lcInvPrd)
      
      *B604561,1 Add an option to ask user to Proceed/Modify. [Begin]
      *=gfModalGen("TRM04197B00000","DIALOG","invoices|"+laData[1])
      *llCSave = .F.
      ** Return to posting date 
      *_CUROBJ = OBJNUM(laData[50])
      *RETURN
      *B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [Begin]
      *IF gfModalGen("TRM04197B42008","DIALOG","invoices|"+laData[1]) = 2
      IF gfModalGen("TRM04197B04012","DIALOG","invoices|"+laData[1]) = 1
      *B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [end]
        llCSave = .F.
        *-- Return to posting date 
        _CUROBJ = OBJNUM(laData[50])
        RETURN      
      ENDIF
      *B604561,1 Add an option to ask user to Proceed/Modify. [End]
      
    ENDIF
  ENDIF
ENDIF  
*--B604454,1 AAN Adding a warning message [End]
llCanSav = .F.
lcSavExact= SET('EXACT')
SET EXACT ON
IF !gfGetMemVar('LLMULCURR')  && If not multi currrency.
  laData[44] = gcBaseCurr
  laData[45] = 1
  laData[48] = 1
ENDIF

IF laScrMode[4] .AND. !SEEK(lcVendor,'APVENDOR')
  ** MESSAGE : " ð not found.         "
  **           "         ® Ok ¯       "
  ** ð = 'Vendor name'
  lnOption = gfModalGen("TRM04002B00000","DIALOG",lcTVendCode+laData[1])
  llCSave     = .F.
  llSamVendor = .F.
  SET EXACT &lcSavExact
  SELECT APINVHDR
  RETURN
ENDIF
*E301077,71 AMM Open files and make necessary relation
llopFact = gfOpenFile(gcSysHome+'SYCFACT','CFACCODE','SH')
llOpVHst = gfOpenFile(gcDataDir+'APVENHST','VENDYEAR','SH')
SELECT APINVHDR
SET RELATION OFF INTO APVENHST
SET RELATION TO APINVHDR.CVENDCODE + APINVHDR.CFISFYEAR INTO APVENHST ADDITIVE

*E301077,71 AMM end
*E300798,1 Check that total approved amount equal total invoice amount when
*E300798,1 payment method is credit card.
IF laData[3] = 'C' AND !EMPTY(lcVenInvTypes) AND lnTAprAmt <> 0 AND ;
   laData[12] <> lnTAprAmt
  ** MESSAGE : " Credit card payment does not support adjustments. Therefor,"
  **           " the approved invoice amount has to total the invoice amount."
  =gfModalGen("TRM04184B00000","DIALOG",IIF(laData[12] > 0,'invoice|invoice','debit memo|debit memo'))
  llCSave     = .F.
  llSamVendor = .F.
  SET EXACT &lcSavExact
  SELECT APINVHDR
  RETURN
ENDIF
IF laScrMode[4] .AND. !EMPTY(APVENDOR.CFACCODE) .AND. laData[5] <> 'F'
  ** MESSAGE : " Vendor ð is factored, this invoice has not been  "
  **           " remited to the factor.                           "
  **           " <Remit to factor> <Stop saving> <Continue saving>"
  ** ð = 'Vendor name'
  lnOption = gfModalGen("TRM04065B04003","DIALOG",laData[1])
  
  DO CASE 
    CASE lnOption = 1
      laData[5]  = 'F'
      laData[36] = APVENDOR.cFacCode
      IF SEEK(laData[36],'SYCFACT')
        laData[6]  = SYCFACT.cFacComp
        laData[7]  = SYCFACT.cAddress1
        laData[8]  = SYCFACT.cAddress2
        laData[9]  = lfGetAdr('SYCFACT','CFACCODE')
        
        *B803165,1 SSE 04/02/2000 Adding the Factor Addresses [Begin] 
        laData[39] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,1)
        laData[40] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,2)
        laData[41] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,3)
        laData[42] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,4)
        laData[43] =  gfGetAdr('SYCFACT','CFACCODE',.F.,.F.,5)
        *B803165,1 SSE 04/02/2000 [End]
        
      ELSE
        STORE SPACE(40) TO laData[6], laData[7], laData[8], laData[9]
      ENDIF 
    CASE lnOption = 2
      llCanSav = .T.
  ENDCASE
ENDIF

IF laData[5] = 'F' .AND. EMPTY(laData[36])
  ** MESSAGE : " You have to enter the ð."
  **           "         ® Ok ¯          "
  ** ð = 'factor code'
  =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
  _CUROBJ = OBJNUM(laData[36])
  SHOW GET laData[36]
  llCanSav = .T.
ENDIF

IF EMPTY(laData[31]) .OR. EMPTY(laData[32])
  IF lfVDtMsg(gcPrnt_Cmp,@lcFisPrd,@lcFisYer,laData[10])
    laData[31] = lcFisPrd
    laData[32] = lcFisYer
  ELSE
    _CUROBJ = OBJNUM(laData[10])
    SHOW GET laData[10]
    llCanSav = .T.
  ENDIF  
ENDIF

IF laData[12] = 0
  ** MESSAGE : " You have to enter the ð."
  **           "         ® Ok ¯          "
  ** ð = 'invoice amount'
  =gfModalGen("TRM04066B00000","DIALOG",lcTInvAmont)
  _CUROBJ = OBJNUM(laData[12])
  SHOW GET laData[12]
  llCanSav = .T.
ENDIF

*B602830,1 Add these lines to prevent the user from saving if the company is
*          using multi-currency and the currency field is empty [Begin]
IF gfGetMemVar('LLMULCURR') .AND. EMPTY(laData[44])
  ** MESSAGE : " You have to enter the ð."
  **           "         ® Ok ¯          "
  ** ð = 'invoice currency'
  =gfModalGen("TRM04066B00000" , "DIALOG" , "invoice currency")
  
  _CUROBJ = OBJNUM(laData[44])
  SHOW GET laData[44]
  llCanSav = .T.
ENDIF
*B602830,1 Add these lines to prevent the user from saving [End]

IF llCanSav
  llCSave = .F.
  SET EXACT &lcSavExact
  RETURN
ENDIF
IF lnDistAmnt <> laData[12]
  ** MESSAGE : " The distribution amount does not "
  **           " total the invoice amount.        "
  **           "               ® Ok ¯             "
  =gfModalGen("TRM04031B00000","DIALOG")
  llCSave = .F.
  SET EXACT &lcSavExact
  RETURN
ENDIF

*E300798,1 Check that total approved to pay amount is not greater than the 
*E300798,1 open approved invoice amount."
STORE 0 TO lnOTInvAmt,lnOTAprAmt
SET EXACT OFF
IF laScrMode[3] AND !EMPTY(lcVenInvTypes)
  SELECT 'APVINVDT'
  SET ORDER TO TAG Orgvinv
  =SEEK(PADR(laData[1],8)+PADR(laData[2],12))
  SUM REST IIF(CIMTYP $ 'RF',-ROUND(nAPAMnt,2),ROUND(nAPAMnt,2)),;
           IIF(CIMTYP $ 'RF',-ROUND(nApAprAmnt,2),ROUND(nApAprAmnt,2));
           TO lnOTInvAmt,lnOTAprAmt ;
      WHILE cVendCode+cApInvNo+cAPVILNo = PADR(laData[1],8)+PADR(laData[2],12)
ENDIF
DO WHILE !EMPTY(lcVenInvTypes) AND llUpdInvLn AND ;
         IIF(laData[12] > 0,!BETWEEN(laData[13]+laData[14]+laData[30],0,;
         laData[12]-laData[25]-laData[26]-laData[34]+;
         (lnOTInvAmt-lnOTAprAmt)-(lnTInvAmt-lnTAprAmt)),;
         !BETWEEN(laData[13]+laData[14]+laData[30],;
         laData[12]-laData[25]-laData[26]-laData[34]+;
         (lnOTInvAmt-lnOTAprAmt)-(lnTInvAmt-lnTAprAmt),0))
         
  ** MESSAGE : " Total approved to pay amount can not be greater than "
  **           " the open approved invoice amount."
  IF gfModalGen("QRM04183B04011","DIALOG",IIF(laData[12] > 0,'invoice','debit memo'))=2
    llCSave     = .F.
    llSamVendor = .F.
    SET EXACT &lcSavExact
    SELECT APINVHDR
    RETURN
  ENDIF
  =lfvAprovPay()
ENDDO
SELECT APINVHDR
SET EXACT ON
*E300798,1 (End)

IF laData[3] = 'C'
  IF EMPTY(laData[20])
    ** MESSAGE : " You have to enter the credit "
    **           " card ð.                      "
    **           "             ® Ok ¯           "
    ** ð = 'Vendor'
    =gfModalGen("TRM04044B00000","DIALOG",lcTVendor)
    _CUROBJ = OBJNUM(laData[20])
    SHOW GET laData[20]
    llCanSav = .T.
  ELSE
    IF EMPTY(laData[21])
      ** MESSAGE : " You have to enter the credit "
      **           " card ð.                      "
      **           "             ® Ok ¯           "
      ** ð = 'Invoice number'
      =gfModalGen("TRM04044B00000","DIALOG",lcTInvoice)
      _CUROBJ = OBJNUM(laData[21])
      SHOW GET laData[21]
      llCanSav = .T.  
    ELSE
      IF EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0'))
        ** MESSAGE : " You have to enter the credit "
        **           " card ð.                      "
        **           "             ® Ok ¯           "
        ** ð = 'A/P account'
        =gfModalGen("TRM04044B00000","DIALOG",lcTApAcct)
        _CUROBJ = OBJNUM(lcAccount)
        SHOW GET lcAccount
        llCanSav = .T.  
      ELSE
        IF laData[17] > 0   && The discount offered
          ** We are going to branch to a dialog screen to enter the 
          ** discount account.
          lcDisAcct = APDIV.CDISCACCT
          IF EMPTY(lcDisAcct)
            lcDisAcct = APSETUP.CDISCACCT
          ENDIF
          ON KEY LABEL ESC
          lcOldGlAcc = lcDisAcct
          *E300683,1 Call *.SPR from screens directory
          * DO APDISACT.SPR 
          DO (gcScrDir + gcWinAppl + '\APDISACT.SPR')
          *E300683,1 end          
          ON KEY LABEL ESC DO lfEscap  
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF llCanSav
    ** MESSAGE : " ð process is canceled "
    **           "            ® Ok ¯            "
    =gfModalGen("TRM00103B00000","DIALOG",lcTSave)
    llCSave = .F.
    SET EXACT &lcSavExact
    lcDisAcct = lcEmptyAcc
    RETURN
  ENDIF

  laData[26] = laData[17]
  
  ** This variable will be in the distribution screen.
  llInvByCr  = .T.
ENDIF  

*B602946,1 Add this line to make sure that applied debit amounts will not
*          produce a negative actual cost [Begin]
*B606903,1 KHM 02/22/2003 (Begin) Move it down because lfChckACst lock the transaction
*B606903,1                header file.
*IF !EMPTY(lcVenInvTypes) .AND. llUpdInvLn .AND. !lfChckACst()
*  llCSave = .F.
*  SET EXACT &lcSavExact
*  RETURN
*ENDIF
*B606903,1 KHM 02/22/2003 (End)
*B602946,1 Add this line to make sure that applied debit amounts [End]

IF laScrMode[3]
  *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
  *E300316,1 to exchange currency from invoice currency to company base currency.
  *E300316,1 laData[44] -----> cCurrCode
  *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,laData[44])
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *E300316,1 (End)
  SELECT(lcTmpDist)
  GO TOP
  REPLACE ALL CSTATUS WITH 'M' FOR CUPDSERNO = 'M'
ENDIF

*B600693,1 M.H 09/19/95 Create the reverse transaction in the APDIST file
*B600693,1 M.H 09/19/95 if the user change the invoice date.

** Begin B600693,1 M.H

SET EXACT &lcSavExact
SELECT APINVHDR
*C100728,1 M.H Begin.
*IF laScrMode[3] .AND. laData[10] <> APINVHDR.DINVDATE
IF laScrMode[3] .AND. IIF(TYPE('APINVHDR.DPOSTDATE')='D',laData[50] <> APINVHDR.DPOSTDATE,laData[10] <> APINVHDR.DINVDATE)
*C100728,1 M.H End.

  *B603067,1 AKA  (Start)
  * Copy APDIST structures into laFileStru array
  SELECT APDIST
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'CSTATUS'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'NRECNO'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 10
  laFileStru[lnFileStru+2,4] = 0
  *B603067,1 AKA  (End)
  
  lcTmpCurs = gfTempName()
  CREATE TABLE (gcWorkDir+lcTmpCurs) FROM ARRAY laFileStru
  SELECT APDIST
  SCAN FOR APINVHDR.CINVNO+APINVHDR.CVENDCODE = APDIST.CINVNO+APDIST.CVENDCODE .AND. CAPDSTAT <> 'V'
    SCATTER MEMVAR MEMO
    m.cStatus  = 'A'
    m.cApdStat = 'V'
    m.nRecNo   = 0  
    SELECT(lcTmpCurs)
    APPEND BLANK
    GATHER MEMVAR MEMO
          
    m.nApdAmnt = -1 * m.nApdAmnt
    m.nEqvamnt = -1 * m.nEqvamnt
    m.lAPDPost = .F.
    m.cBatchNo = ' '
    m.cTrnSledn= ' '
    m.cAPSessNo= lcSequence
    
    *B801498,1 Add these line to clear the entry number in the new
    *APDIST records [Begin]
    m.nEntryNo = 0
    *B801498,1 Add these line to clear the entry number [End]
    
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
  USE IN (lcTmpCurs)
  SELECT(lcTmpDist)
  APPEND FROM &gcWorkdir.&lcTmpCurs..DBF
  ERASE &gcWorkdir.&lcTmpCurs..DBF  && Erase the Temp file.
  SCAN FOR cApdStat <> 'V'
*C100728,1 M.H Begin.
*    REPLACE DAPDTRDAT WITH laData[10] ,;
            CAPSESSNO WITH lcSequence ,;
            CFISFYEAR WITH laData[32] ,;
            CFSPPRDID WITH laData[31] ,;
            LAPDPOST  WITH .F.        ,;
            CSTATUS   WITH IIF(CSTATUS $ 'MS','M',CSTATUS)
    IF TYPE('APINVHDR.DPOSTDATE') = 'D'
      REPLACE DAPDTRDAT WITH laData[50] ,;
              CAPSESSNO WITH lcSequence ,;
              CFISFYEAR WITH lcDistYer  ,;
              CFSPPRDID WITH lcDistPrd  ,;
              LAPDPOST  WITH .F.        ,;
              CSTATUS   WITH IIF(CSTATUS $ 'MS','M',CSTATUS)
    ELSE
      REPLACE DAPDTRDAT WITH laData[10] ,;
              CAPSESSNO WITH lcSequence ,;
              CFISFYEAR WITH laData[32] ,;
              CFSPPRDID WITH laData[31] ,;
              LAPDPOST  WITH .F.        ,;
              CSTATUS   WITH IIF(CSTATUS $ 'MS','M',CSTATUS)
    ENDIF
*C100728,1 M.H End
    
    *B801498,1 Add these line to clear the batch number and transaction
    *number and entry number in the new APDIST records [Begin]
    REPLACE cBatchNo  WITH '' ,;
            cTrnSledn WITH '' ,;
            nEntryNo  WITH 0
    
    *B801498,1 Add these line to clear the batch number [End]
    
  ENDSCAN
ENDIF

*B606903,1 KHM 02/22/2003 (Begin) Check if adding a new invoice and link to GL and there is
*B606903,1                inactive account then do not save.
IF laScrMode[4] AND llApGlLink AND !lfvldApAct()
  llCSave = .F.
  SET EXACT &lcSavExact
  RETURN
ENDIF
IF !EMPTY(lcVenInvTypes) .AND. llUpdInvLn .AND. !lfChckACst()
  llCSave = .F.
  SET EXACT &lcSavExact
  RETURN
ENDIF
*B606903,1 KHM 02/22/2003 (End)

** Begin of adding the currency to the Distribution lines.
SELECT(lcTmpDist)
lnTotal = 0

SCAN FOR cApdStat <> 'V'
  *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
  *REPLACE CCURRCODE WITH laData[44] ,;
          NEXRATE   WITH laData[45] ,;
          NCURRUNIT WITH laData[48] ,;
          NEQVAMNT  WITH ROUND(NAPDAMNT * laData[45]/laData[48],2)

  REPLACE CCURRCODE WITH laData[44] ,;
          NEXRATE   WITH laData[45] ,;
          NCURRUNIT WITH laData[48] ,;
          NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45] &lcExSin2 laData[48],2)
  *E300316,1 (End)           

  *B606939,1 KHM 03/20/2003 (Begin) Replacing the date and period of the posting date.
  REPLACE DAPDTRDAT WITH laData[50],;
          CFISFYEAR WITH lcPstYer  ,;
          CFSPPRDID WITH lcPstPrd
  *B606939,1 KHM 03/20/2003 (End)

  IF CAPDACTID <> 'A'

    *B124737,1 NNA 12/16/2004 (Begin) get Neqvamnt by Multiple Napdamnt (Total amount) with 
    *B124737,1 NNA            Exchange Rate to get the number with out Fraction to equal to 
    *B124737,1 NNA            (total amount of the invoice * Rating)
    *lnTotal = lnTotal + NEQVAMNT
    *--lntotal = all the amounts that paid from this invoice
    lnTotal = lnTotal + (NAPDAMNT &lcExSin1 laData[45] &lcExSin2 laData[48])
    *B124737,1 NNA (End)

  ENDIF  
ENDSCAN

*B124737,1 NNA 12/16/2004 (Begin) Round lntotal to 2 Decimal
lnTotal = ROUND(lnTotal,2)
*B124737,1 NNA (End)

LOCATE FOR cApdStat <> 'V' .AND. CAPDACTID <> 'A' .AND. CSTATUS $ 'AM'

IF FOUND()
  *B601013,1 Get equivalent amounts
  *REPLACE NEQVAMNT WITH NEQVAMNT+((ROUND(laData[12]*laData[45]/laData[48],2))-lnTotal)
  REPLACE NEQVAMNT WITH NEQVAMNT+;
                       ((ROUND(laData[12] &lcExSin1 laData[45];
                                          &lcExSin2 laData[48],2))-lnTotal)   
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  *REPLACE NAPDAMNT WITH NAPDAMNT + laData[12] -lnTotal
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  *B601013,1 end.                                          
ENDIF  
** End of adding the currency to the Distribution lines.

SET EXACT ON
** END B600693,1 M.H

** Locking the 4 files ***
IF gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.T.)
  
  IF EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0'))
    laData[29] = SPACE(24)
  ENDIF  

  IF EMPTY(STRTRAN(STRTRAN(laData[33],'-'),'0'))
    laData[33] = SPACE(24)
  ENDIF  
  *B605746,1 RAE [Start]
  llSave = lfChckVoid()
  IF !llSave
    *-- If lcTmpDist is not balanced, invoice cannot be saved. 
    =gfModalGen('TRM04199B00000','ALERT')
    =gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.F.)
    IF WVISIBLE('CWRAPDISTR')
      =gfChClose('CWRAPDISTR')  
    ENDIF
    IF USED('APVENHST') .AND. llOpVHst
      =gfCloseFile('APVENHST')
    ENDIF
    IF USED('SYCFACT') .AND. llopFact
      =gfCloseFile('SYCFACT')
    ENDIF
    SELECT APINVHDR
    SET EXACT &lcSavExact
    RETURN
  ENDIF
  *B605746,1 RAE [End]

  SELECT APINVHDR
  
  IF laScrMode[4]
    APPEND BLANK
  ENDIF  

  ** Add the new data to the record.
  GATHER FROM laData FIELDS &lcScFields MEMO 

  IF laData[3] = 'C'
    SELECT APINVHDR
    REPLACE NINVADJ WITH laData[12] - laData[17]

    =gfTmp2Mast('APDIST',lcTmpDist)
    *B600658,1 Only update the cumulative purchase if you are in ADD mode because 
    *B600658,1 if you are in Edit mode this means that you changed the payment method
    *B600658,1 to Credit Card and in this case you did not purchase new thing
    SELECT APVENDOR
    *REPLACE NVENBAL   WITH NVENBAL - lnOldInvAmt,;
            DVENLPURD WITH laData[10],;
            NVENLPURA WITH laData[12],;
            DVENLPAYD WITH laData[10],;
            NVENLPAYA WITH laData[12] - laData[17],;
            NVENCPAY  WITH NVENCPAY + (laData[12] - laData[17]),;
            NVENCPUR  WITH NVENCPUR + laData[12]
    *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
    *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
    *E300316,1 laData[45] -----> nExRate
    *E300316,1 laData[48] -----> nCurrUnit
    *E300316,1 laData[12] -----> nInvAmnt
    *E300316,1 laData[17] -----> nInvDisOf

    *REPLACE NVENBAL   WITH NVENBAL - ROUND(lnOldInvAmt * laData[45]/laData[48],2),;
            DVENLPURD WITH laData[10],;
            NVENLPURA WITH ROUND(laData[12] * laData[45]/laData[48],2),;
            DVENLPAYD WITH laData[10],;
            NVENLPAYA WITH ROUND((laData[12] - laData[17]) * laData[45]/laData[48],2),;
            NVENCPAY  WITH NVENCPAY + ROUND((laData[12] - laData[17]) * laData[45]/laData[48],2),;
            NVENCPUR  WITH NVENCPUR + IIF(laScrMode[4],ROUND(laData[12] * laData[45]/laData[48],2),0)

    *B601013,1 Use lcExSin1, lcExSin2
    *REPLACE NVENBAL   WITH NVENBAL -;
                           ROUND(lnOldInvAmt &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            DVENLPURD WITH laData[10],;
            NVENLPURA WITH ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            DVENLPAYD WITH laData[10],;
            NVENLPAYA WITH ROUND((laData[12] - laData[17]) &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            NVENCPAY  WITH NVENCPAY +;
                           ROUND((laData[12] - laData[17]) &lcExSin1 laData[45] &lcExSin1 laData[48],2),;
            NVENCPUR  WITH NVENCPUR +;
                           IIF(laScrMode[4],ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2),0)
    REPLACE NVENBAL   WITH NVENBAL -;
                           ROUND(lnOldInvAmt &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            DVENLPURD WITH laData[10],;
            NVENLPURA WITH ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            DVENLPAYD WITH laData[10],;
            NVENLPAYA WITH ROUND((laData[12] - laData[17]) &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            NVENCPAY  WITH NVENCPAY +;
                           ROUND((laData[12] - laData[17]) &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            NVENCPUR  WITH NVENCPUR +;
                           IIF(laScrMode[4],ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2),0)                           
    *B601013,1 end.
    *E300316,1 (End)            
    =gfAdd_Info('APVENDOR')

    SELECT APVENHST
    ** Confirm of seek with the content of ladata[1]
    lcNewPayFld  = 'NVNHPAY'+ALLTRIM(STR(INT(VAL(laData[31]))))

    *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
    *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
    *E300316,1 laData[26] -----> nInvDisTk
    *E300316,1 laData[45] -----> nExRate
    *E300316,1 laData[12] -----> nInvAmnt
    *E300316,1 laData[48] -----> nCurrUnit
    *REPLACE NVNHDISTKN   WITH NVNHDISTKN + ROUND(laData[26] * laData[45]/laData[48],2),;
            NVNHCCPAY    WITH NVNHCCPAY  + ROUND((laData[12] - laData[26]) * laData[45]/laData[48],2),;
            NVNHTOTPA    WITH NVNHTOTPA  + ROUND((laData[12] - laData[26]) * laData[45]/laData[48],2),;
            &lcNewPayFld WITH EVALUATE(lcNewPayFld) + ROUND((laData[12] - laData[26]) * laData[45]/laData[48],2)

    REPLACE NVNHDISTKN   WITH NVNHDISTKN + ROUND(laData[26];
                              &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            NVNHCCPAY    WITH NVNHCCPAY  + ROUND((laData[12] - laData[26]);
                              &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            NVNHTOTPA    WITH NVNHTOTPA  + ROUND((laData[12] - laData[26]);
                              &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            &lcNewPayFld WITH EVALUATE(lcNewPayFld) + ROUND((laData[12] - laData[26]);
                              &lcExSin1 laData[45] &lcExSin2 laData[48],2)
    *E300316,1 (End)            
  ELSE
    =gfTmp2Mast('APDIST',lcTmpDist)
    SELECT APVENDOR
    *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
    *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
    *E300316,1 laData[12] -----> nInvAmnt
    *E300316,1 laData[45] -----> nExRate
    *E300316,1 laData[48] -----> nCurrUnit
    *REPLACE NVENBAL   WITH NVENBAL  + ROUND((laData[12]-lnOldInvAmt) * laData[45]/laData[48],2),;
            NVENCPUR  WITH NVENCPUR + ROUND((laData[12]-lnOldInvAmt) * laData[45]/laData[48],2),;
            DVENLPURD WITH laData[10],;
            NVENLPURA WITH ROUND(laData[12] * laData[45]/laData[48],2)

    REPLACE NVENBAL   WITH NVENBAL  + ROUND((laData[12]-lnOldInvAmt);
                           &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            NVENCPUR  WITH NVENCPUR + ROUND((laData[12]-lnOldInvAmt);
                           &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            DVENLPURD WITH laData[10],;
            NVENLPURA WITH ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2)
    *E300316,1 (End)

    IF laData[12] < 0 
      *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[12] -----> nInvAmnt
      *E300316,1 laData[45] -----> nExRate
      *E300316,1 laData[48] -----> nCurrUnit
      *REPLACE NVENOPNDR WITH NVENOPNDR + ROUND((lnOldInvAmt - laData[12]) * laData[45]/laData[48],2)
      REPLACE NVENOPNDR WITH NVENOPNDR + ROUND((lnOldInvAmt - laData[12]);
                             &lcExSin1 laData[45] &lcExSin2 laData[48],2)
      *E300316,1 (End)
    ENDIF
    =gfAdd_Info('APVENDOR')
  ENDIF
  SELECT APVENHST
  lcNewPerFld  = 'NVNHPUR'+ALLTRIM(STR(INT(VAL(laData[31]))))
  
  lcOldPerFld  = 'NVNHPUR'+ALLTRIM(STR(INT(VAL(lcOldPeriod))))
  
  IF lcOldYear = laData[32] .OR. laScrMode[4]
    *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
    *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
    *E300316,1 laData[12] -----> nInvAmnt
    *E300316,1 laData[45] -----> nExRate
    *E300316,1 laData[48] -----> nCurrUnit
    *E300316,1 laData[17] -----> nInvDisOf
    *REPLACE NVNHDISOF WITH NVNHDISOF + ROUND((laData[17] - lnOldDiscOf) * laData[45]/laData[48],2),;
            NVNHPURCH WITH NVNHPURCH + ROUND((laData[12] - lnOldInvAmt) * laData[45]/laData[48],2)
    REPLACE NVNHDISOF WITH NVNHDISOF + ROUND((laData[17] - lnOldDiscOf);
                           &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
            NVNHPURCH WITH NVNHPURCH + ROUND((laData[12] - lnOldInvAmt);
                           &lcExSin1 laData[45] &lcExSin2 laData[48],2)
    *E300316,1 (End)        
    =gfAdd_Info('APVENHST')
    IF lcOldPeriod = laData[31] .OR. laScrMode[4]
      *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[12] -----> nInvAmnt
      *E300316,1 laData[45] -----> nExRate
      *E300316,1 laData[48] -----> nCurrUnit
      *REPLACE &lcNewPerFld WITH EVALUATE(lcNewPerFld) + ROUND((laData[12] - lnOldInvAmt) * laData[45]/laData[48],2)
      REPLACE &lcNewPerFld WITH EVALUATE(lcNewPerFld) + ROUND((laData[12] - lnOldInvAmt);
                                &lcExSin1 laData[45] &lcExSin2 laData[48],2)
      *E300316,1 (End)
    ELSE
      *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *REPLACE &lcOldPerFld WITH EVALUATE(lcOldPerFld) - ROUND(lnOldInvAmt * laData[45]/laData[48],2)
      REPLACE &lcOldPerFld WITH EVALUATE(lcOldPerFld) -;
                                ROUND(lnOldInvAmt &lcExSin1 laData[45] &lcExSin2 laData[48],2)
      *REPLACE &lcNewPerFld WITH EVALUATE(lcNewPerFld) + ROUND(laData[12] * laData[45]/laData[48],2)
      REPLACE &lcNewPerFld WITH EVALUATE(lcNewPerFld) +;
                                ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2)
      *E300316,1 (End)
    ENDIF
  ELSE
    *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
    *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
    *E300316,1 laData[17] -----> nInvDisOf
    *E300316,1 laData[45] -----> nExRate
    *E300316,1 laData[48] -----> nCurrUnit
    *REPLACE NVNHDISOF    WITH NVNHDISOF + ROUND(laData[17] * laData[45]/laData[48],2),;
            NVNHPURCH    WITH NVNHPURCH + ROUND(laData[12] * laData[45]/laData[48],2),;
            &lcNewPerFld WITH EVALUATE(lcNewPerFld) + ROUND(laData[12] * laData[45]/laData[48],2)
    REPLACE NVNHDISOF    WITH NVNHDISOF + ROUND(laData[17] &lcExSin1;
                                          laData[45] &lcExSin2 laData[48],2),;
            NVNHPURCH    WITH NVNHPURCH + ROUND(laData[12] &lcExSin1;
                                          laData[45] &lcExSin2 laData[48],2),;
            &lcNewPerFld WITH EVALUATE(lcNewPerFld) + ROUND(laData[12];
                              &lcExSin1 laData[45] &lcExSin2 laData[48],2)
    *E300316,1 (End)            
    =gfAdd_Info('APVENHST')
    IF SEEK(laData[1]+lcOldYear)
      *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[45] -----> nExRate
      *E300316,1 laData[48] -----> nCurrUnit
      *REPLACE NVNHDISOF  WITH NVNHDISOF - ROUND(lnOldDiscOf * laData[45]/laData[48],2),;
               NVNHPURCH  WITH NVNHPURCH - ROUND(lnOldInvAmt * laData[45]/laData[48],2)
      REPLACE NVNHDISOF  WITH NVNHDISOF - ROUND(lnOldDiscOf &lcExSin1;
                                          laData[45] &lcExSin2 laData[48],2),;
              NVNHPURCH  WITH NVNHPURCH - ROUND(lnOldInvAmt &lcExSin1;
                                          laData[45] &lcExSin2 laData[48],2)
      *E300316,1 (End)             
      IF laScrMode[3]        
        *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
        *REPLACE &lcOldPerFld WITH EVALUATE(lcOldPerFld) - ROUND(lnOldInvAmt * laData[45]/laData[48],2)
        REPLACE &lcOldPerFld WITH EVALUATE(lcOldPerFld) - ROUND(lnOldInvAmt &lcExSin1 laData[45] &lcExSin2 laData[48],2)
        *E300316,1 (End)
      ENDIF  
      =gfAdd_Info('APVENHST')
    ENDIF
  ENDIF

  SELECT APINVHDR
  lnInvAdj  = NINVADJ
  lcVenPrior= APVENDOR.CVENPRIOR

  IF laData[3] = 'C'
    *B600597,1 Start
    *B600597,1 Save old array data
    laData_15 = laData[15]
    laData_19 = laData[19]
    laData_16 = laData[16]
    laData_24 = laData[24]
    lcoldDiv  = lcDivision           && Division

    *B600597,1 Get the creditor data from the vendor file
    SELECT APVENDOR

    *B600597,1 get the current record number
    lnCr_Rec = Recno()

    *B600597,1 Seek the creditor record
    SEEK laData[20]

    *B600597,1 Save the creditor data in temp variables
    lcVenPmeth = cVenPmeth           && Payment method
    lcDivision = cDivision           && Division
    laData[15] = cTermCode           && Term code

    *B600597,1 Return to the initial record
    GOTO lnCr_Rec

    *B600597,1 Get the creditor data from the codes file
    DECLARE  laTermArry[3,2]
    laTermArry[1,1] = 'NTERDISCR'
    laTermArry[1,2] = 'laData[16]'  
    laTermArry[2,1] = 'NTERDUED'
    laTermArry[2,2] = 'laData[19]'  
    laTermArry[3,1] = 'NTERDISCD'
    laTermArry[3,2] = 'laData[24]'  

    *E300643,1 Change this line for the changes we have made 
    *          to (gfRltFld) [Begin]
    *=gfRltFld(laData[15],@laTermArry)   

    *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[Begin]
    *=gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
    IF laScrMode[4]
      =gfRltFld(laData[15] , @laTermAry , 'CTERMCODE')
    ENDIF
    *B604309,4 AAN Not calculate the due days from term code if the screen in edit mode[End]      
    
    *E300643,1 Change this line [End]
    
    SELECT APINVHDR

    *B600597,1 END

    lnSavRecNo = RECNO()
    APPEND BLANK
    *B600597,1 Change the payemnt method and division saving
    *REPLACE CVENDCODE WITH laData[20],;
            CINVNO    WITH laData[21],;
            CAPACCT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                               SPACE(lnApsAcLen), lcAccount) ,;
            CDIVISION WITH laData[4] ,;
            DINVDATE  WITH laData[10],;
            CFISFYEAR WITH laData[32],;
            CFSPPRDID WITH laData[31],;
            CINVREF   WITH laData[2] ,;
            CINVREMIT WITH 'V'       ,;
            NINVAMNT  WITH lnInvAdj  ,;
            CVENPRIOR WITH lcVenPrior,;
            CVENPMETH WITH 'P',;
            CTERMCODE WITH laData[15],;
            NTERDUED  WITH laData[19],;
            NTERDISCD WITH laData[24],;
            NTERDISCR WITH laData[16],;
            DINVDUDAT WITH laData[22],;
            CVENCCVEN WITH laData[1] ,;
            CVENCCINV WITH laData[2]

    *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
*C100728,1 M.H Begin.
*    REPLACE CVENDCODE WITH laData[20],;
            CINVNO    WITH laData[21],;
            CAPACCT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                               SPACE(lnApsAcLen), lcAccount) ,;
            CDIVISION WITH lcDivision,;
            DINVDATE  WITH laData[10],;
            CFISFYEAR WITH laData[32],;
            CFSPPRDID WITH laData[31],;
            CINVREF   WITH laData[2] ,;
            CINVREMIT WITH 'V'       ,;
            NINVAMNT  WITH lnInvAdj  ,;
            CVENPRIOR WITH lcVenPrior,;
            CVENPMETH WITH lcVenPmeth,;
            CTERMCODE WITH laData[15],;
            NTERDUED  WITH laData[19],;
            NTERDISCD WITH laData[24],;
            NTERDISCR WITH laData[16],;
            DINVDUDAT WITH laData[22],;
            CVENCCVEN WITH laData[1] ,;
            CVENCCINV WITH laData[2],;
            CCURRCODE WITH laData[44],;
            NEXRATE   WITH laData[45],;
            NCURRUNIT   WITH laData[48]

    IF TYPE('APINVHDR.DPOSTDATE') = 'D'
      REPLACE CVENDCODE WITH laData[20],;
              CINVNO    WITH laData[21],;
              CAPACCT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                                 SPACE(lnApsAcLen), lcAccount) ,;
              CDIVISION WITH lcDivision,;
              DINVDATE  WITH laData[10],;
              CFISFYEAR WITH laData[32],;
              CFSPPRDID WITH laData[31],;
              CINVREF   WITH laData[2] ,;
              CINVREMIT WITH 'V'       ,;
              NINVAMNT  WITH lnInvAdj  ,;
              CVENPRIOR WITH lcVenPrior,;
              CVENPMETH WITH lcVenPmeth,;
              CTERMCODE WITH laData[15],;
              NTERDUED  WITH laData[19],;
              NTERDISCD WITH laData[24],;
              NTERDISCR WITH laData[16],;
              DINVDUDAT WITH laData[22],;
              CVENCCVEN WITH laData[1] ,;
              CVENCCINV WITH laData[2],;
              CCURRCODE WITH laData[44],;
              NEXRATE   WITH laData[45],;
              NCURRUNIT WITH laData[48],;
              DPOSTDATE WITH laData[50]
    ELSE
      REPLACE CVENDCODE WITH laData[20],;
              CINVNO    WITH laData[21],;
              CAPACCT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                                 SPACE(lnApsAcLen), lcAccount) ,;
              CDIVISION WITH lcDivision,;
              DINVDATE  WITH laData[10],;
              CFISFYEAR WITH laData[32],;
              CFSPPRDID WITH laData[31],;
              CINVREF   WITH laData[2] ,;
              CINVREMIT WITH 'V'       ,;
              NINVAMNT  WITH lnInvAdj  ,;
              CVENPRIOR WITH lcVenPrior,;
              CVENPMETH WITH lcVenPmeth,;
              CTERMCODE WITH laData[15],;
              NTERDUED  WITH laData[19],;
              NTERDISCD WITH laData[24],;
              NTERDISCR WITH laData[16],;
              DINVDUDAT WITH laData[22],;
              CVENCCVEN WITH laData[1] ,;
              CVENCCINV WITH laData[2],;
              CCURRCODE WITH laData[44],;
              NEXRATE   WITH laData[45],;
              NCURRUNIT WITH laData[48]
    ENDIF
*C100728,1 M.H End.
            
    =gfAdd_Info('APINVHDR')
            
    *B600597,1 Start.   Save old array data
    laData[15] = laData_15 
    laData[19] = laData_19 
    laData[16] = laData_16 
    laData[24] = laData_24 
    lcDivision  = lcoldDiv            && Division
  
    *B600597,1 End.
  
    SELECT APDIST
    APPEND BLANK
    *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
    *E300316,1 laData[12] -----> nInvAmnt
    *E300316,1 laData[45] -----> nExRate
    *E300316,1 laData[48] -----> nCurrUnit

    *B601013,1 Use lcExSin1, lcExSin2 to get the equivalent amount
    *B601014,1 Update APDIST.cApdActId with 'A'
    *REPLACE CVENDCODE WITH laData[1] ,;
             CINVNO    WITH laData[2] ,;
             DAPDTRDAT WITH laData[10],;
             LAPDPOST  WITH .F.       ,;
             CAPDREF   WITH laData[2] ,;
             CAPDGLACT WITH laData[33],;
             CAPDTRTYP WITH 'I'       ,;
             NAPDAMNT  WITH laData[12],;
             CFISFYEAR WITH laData[32],;
             CFSPPRDID WITH laData[31],;
             CAPSESSNO WITH lcSequence,;
             CCURRCODE WITH laData[44],;
             NEXRATE   WITH laData[45],;
             NCURRUNIT WITH laData[48],;
             NEQVAMNT  WITH ROUND(laData[12] * laData[45]/laData[48],2) 
                       
            *NEQVAMNT  WITH ROUND(laData[12] * laData[45]/laData[48],2)  
*C100728,1 M.H Begin.
*    REPLACE CVENDCODE WITH laData[1] ,;
            CINVNO    WITH laData[2] ,;
            DAPDTRDAT WITH laData[10],;
            LAPDPOST  WITH .F.       ,;
            CAPDREF   WITH laData[2] ,;
            CAPDGLACT WITH laData[33],;
            CAPDTRTYP WITH 'I'       ,;
            NAPDAMNT  WITH laData[12],;
            CFISFYEAR WITH laData[32],;
            CFSPPRDID WITH laData[31],;
            CAPSESSNO WITH lcSequence,;
            CCURRCODE WITH laData[44],;
            NEXRATE   WITH laData[45],;
            NCURRUNIT WITH laData[48],;
            NEQVAMNT  WITH ROUND(laData[12] &lcExSin1 laData[45];
                                            &lcExSin2 laData[48],2),;
            CAPDACTID with 'A'

    IF TYPE('APINVHDR.DPOSTDATE') = 'D'
      *B602997,1 AKA  (Start)
      *Vendor code and invoice no were replaced with wrong data
      
      *REPLACE CVENDCODE WITH laData[1] ,;
      *        CINVNO    WITH laData[2] ,;
      *        DAPDTRDAT WITH laData[50],;
      *        LAPDPOST  WITH .F.       ,;
      *        CAPDREF   WITH laData[2] ,;
      *        CAPDGLACT WITH laData[33],;
      *        CAPDTRTYP WITH 'I'       ,;
      *        NAPDAMNT  WITH laData[12],;
      *        CFISFYEAR WITH lcDistYer ,;
      *        CFSPPRDID WITH lcDistPrd ,;
      *        CAPSESSNO WITH lcSequence,;
      *        CCURRCODE WITH laData[44],;
      *        NEXRATE   WITH laData[45],;
      *        NCURRUNIT WITH laData[48],;
      *        NEQVAMNT  WITH ROUND(laData[12] &lcExSin1 laData[45];
      *                                       &lcExSin2 laData[48],2),;
      *       cApdActId with 'A'

      REPLACE CVENDCODE WITH laData[20],;
              CINVNO    WITH laData[21],;
              DAPDTRDAT WITH laData[50],;
              LAPDPOST  WITH .F.       ,;
              CAPDREF   WITH laData[2] ,;
              CAPDGLACT WITH laData[33],;
              CAPDTRTYP WITH 'I'       ,;
              NAPDAMNT  WITH laData[12],;
              CFISFYEAR WITH lcDistYer ,;
              CFSPPRDID WITH lcDistPrd ,;
              CAPSESSNO WITH lcSequence,;
              CCURRCODE WITH laData[44],;
              NEXRATE   WITH laData[45],;
              NCURRUNIT WITH laData[48],;
              NEQVAMNT  WITH ROUND(laData[12] &lcExSin1 laData[45];
                                             &lcExSin2 laData[48],2),;
             cApdActId with 'A'
      *B602997,1 AKA  (End)      
      
      
    ELSE
      *B602997,1 AKA  (Start)
      *Vendor code and invoice no were replaced with wrong data
      *REPLACE CVENDCODE WITH laData[1] ,;
      *        CINVNO    WITH laData[2] ,;
      *        DAPDTRDAT WITH laData[10],;
      *        LAPDPOST  WITH .F.       ,;
      *        CAPDREF   WITH laData[2] ,;
      *        CAPDGLACT WITH laData[33],;
      *        CAPDTRTYP WITH 'I'       ,;
      *        NAPDAMNT  WITH laData[12],;
      *        CFISFYEAR WITH laData[32],;
      *        CFSPPRDID WITH laData[31],;
      *        CAPSESSNO WITH lcSequence,;
      *        CCURRCODE WITH laData[44],;
      *        NEXRATE   WITH laData[45],;
      *        NCURRUNIT WITH laData[48],;
      *        NEQVAMNT  WITH ROUND(laData[12] &lcExSin1 laData[45];
      *                                        &lcExSin2 laData[48],2),;
      *        cApdActId with 'A'
    
      REPLACE CVENDCODE WITH laData[20] ,;
              CINVNO    WITH laData[21] ,;
              DAPDTRDAT WITH laData[10],;
              LAPDPOST  WITH .F.       ,;
              CAPDREF   WITH laData[2] ,;
              CAPDGLACT WITH laData[33],;
              CAPDTRTYP WITH 'I'       ,;
              NAPDAMNT  WITH laData[12],;
              CFISFYEAR WITH laData[32],;
              CFSPPRDID WITH laData[31],;
              CAPSESSNO WITH lcSequence,;
              CCURRCODE WITH laData[44],;
              NEXRATE   WITH laData[45],;
              NCURRUNIT WITH laData[48],;
              NEQVAMNT  WITH ROUND(laData[12] &lcExSin1 laData[45];
                                              &lcExSin2 laData[48],2),;
              cApdActId with 'A'
      *B602997,1 AKA  (End)              
    ENDIF
*C100728,1 M.H End.
    *B601014,1 end.        
    *B601013,1 end.
    *E300316,1 (End)
            
    ** Bug we have to replace CAPDACTID with 'A'  Mohamed **
    ** Bug {CAPDACTID with 'A'} fixed on 03/31/96 (B601014,1 RENEE)
    =gfAdd_Info('APDIST')

    APPEND BLANK
    
    *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
    *E300316,1 laData[45] -----> nExRate
    *E300316,1 laData[48] -----> nCurrUnit
    *B601013,1 Use lcExSin1, lcExSin2 to get the equivalent amount
    *B601014,1 Update APDIST.cApdActId with 'A'
    *REPLACE CVENDCODE WITH laData[1] ;
             CINVNO    WITH laData[2] ;
             DAPDTRDAT WITH laData[10],;
             LAPDPOST  WITH .F.       ,;
             CAPDREF   WITH laData[2] ,;
             CAPDTRTYP WITH 'I'       ,;
             CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                                SPACE(lnApsAcLen), lcAccount) ,;
             NAPDAMNT  WITH 0 - laData[12];
             CFISFYEAR WITH laData[32],;
             CFSPPRDID WITH laData[31],;
             CAPSESSNO WITH lcSequence,;
             CCURRCODE WITH laData[44],;
             NEXRATE   WITH laData[45],;
             NCURRUNIT WITH laData[48],;
             NEQVAMNT  WITH ROUND(NAPDAMNT * laData[45]/laData[48],2)

*C100728,1 M.H Begin.    
*    REPLACE CVENDCODE WITH laData[1] ;
            CINVNO    WITH laData[2] ;
            DAPDTRDAT WITH laData[10],;
            LAPDPOST  WITH .F.       ,;
            CAPDREF   WITH laData[2] ,;
            CAPDTRTYP WITH 'I'       ,;
            CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                               SPACE(lnApsAcLen), lcAccount) ,;
            NAPDAMNT  WITH 0 - laData[12];
            CFISFYEAR WITH laData[32],;
            CFSPPRDID WITH laData[31],;
            CAPSESSNO WITH lcSequence,;
            CCURRCODE WITH laData[44],;
            NEXRATE   WITH laData[45],;
            NCURRUNIT WITH laData[48],;
            NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45];
                                          &lcExSin2 laData[48],2),;
            cApdActId with 'A'

    IF TYPE('APINVHDR.DPOSTDATE') = 'D'
      *B602997,1 AKA  (Start)
      *Vendor code , invoice no and amount were replaced with wrong data    
      
      *REPLACE CVENDCODE WITH laData[1] ;
      *        CINVNO    WITH laData[2] ;
      *        DAPDTRDAT WITH laData[50],;
      *        LAPDPOST  WITH .F.       ,;
      *        CAPDREF   WITH laData[2] ,;
      *        CAPDTRTYP WITH 'I'       ,;
      *        CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
      *                           SPACE(lnApsAcLen), lcAccount) ,;
      *        NAPDAMNT  WITH 0 - laData[12];
      *       CFISFYEAR WITH lcDistYer ,;
      *        CFSPPRDID WITH lcDistPrd ,;
      *        CAPSESSNO WITH lcSequence,;
      *        CCURRCODE WITH laData[44],;
      *        NEXRATE   WITH laData[45],;
      *        NCURRUNIT WITH laData[48],;
      *        NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45];
      *                                      &lcExSin2 laData[48],2),;
      *        cApdActId with 'A'

      REPLACE CVENDCODE WITH laData[20] ;
              CINVNO    WITH laData[21] ;
              DAPDTRDAT WITH laData[50],;
              LAPDPOST  WITH .F.       ,;
              CAPDREF   WITH laData[2] ,;
              CAPDTRTYP WITH 'I'       ,;
              CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                                 SPACE(lnApsAcLen), lcAccount) ,;
              NAPDAMNT  WITH laData[17] - laData[12];
              CFISFYEAR WITH lcDistYer ,;
              CFSPPRDID WITH lcDistPrd ,;
              CAPSESSNO WITH lcSequence,;
              CCURRCODE WITH laData[44],;
              NEXRATE   WITH laData[45],;
              NCURRUNIT WITH laData[48],;
              NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45];
                                            &lcExSin2 laData[48],2),;
              cApdActId with 'A'      
              
      *B602997,1 AKA  (End)              
    ELSE
      *B602997,1 AKA  (Start)
      *Vendor code , invoice no and amount were replaced with wrong data    

      *REPLACE CVENDCODE WITH laData[1] ;
      *        CINVNO    WITH laData[2] ;
      *        DAPDTRDAT WITH laData[10],;
      *        LAPDPOST  WITH .F.       ,;
      *        CAPDREF   WITH laData[2] ,;
      *        CAPDTRTYP WITH 'I'       ,;
      *        CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
      *                           SPACE(lnApsAcLen), lcAccount) ,;
      *        NAPDAMNT  WITH 0 - laData[12];
      *        CFISFYEAR WITH laData[32],;
      *        CFSPPRDID WITH laData[31],;
      *        CAPSESSNO WITH lcSequence,;
      *        CCURRCODE WITH laData[44],;
      *        NEXRATE   WITH laData[45],;
      *        NCURRUNIT WITH laData[48],;
      *        NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45];
      *                                      &lcExSin2 laData[48],2),;
      *        cApdActId with 'A'
      
              
      REPLACE CVENDCODE WITH laData[20] ;
              CINVNO    WITH laData[21] ;
              DAPDTRDAT WITH laData[10],;
              LAPDPOST  WITH .F.       ,;
              CAPDREF   WITH laData[2] ,;
              CAPDTRTYP WITH 'I'       ,;
              CAPDGLACT WITH IIF(EMPTY(STRTRAN(STRTRAN(lcAccount,'-'),'0')),;
                                 SPACE(lnApsAcLen), lcAccount) ,;
              NAPDAMNT  WITH laData[17] - laData[12];
              CFISFYEAR WITH laData[32],;
              CFSPPRDID WITH laData[31],;
              CAPSESSNO WITH lcSequence,;
              CCURRCODE WITH laData[44],;
              NEXRATE   WITH laData[45],;
              NCURRUNIT WITH laData[48],;
              NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45];
                                            &lcExSin2 laData[48],2),;
              cApdActId with 'A'
      *B602997,1 AKA  (End)                            
      
    ENDIF
    *C100728,1 M.H End.
            
    *B601014,1 end.
    *B601013,1 end.
    *E300316,1 (End)
    =gfAdd_Info('APDIST')

    *B602997,1 AKA  (Start)
    * Adding new entry of discount account if discount amount is greater than zero.
    IF laData[17] > 0  
      APPEND BLANK
      IF TYPE('APINVHDR.DPOSTDATE') = 'D'
        REPLACE CVENDCODE WITH laData[20] ;
                CINVNO    WITH laData[21] ;
                DAPDTRDAT WITH laData[50],;
                LAPDPOST  WITH .F.       ,;
                CAPDREF   WITH laData[2] ,;
                CAPDTRTYP WITH 'I'       ,;
                cApdActId with 'S'       ,;              
                CAPDGLACT WITH lcDisAcct ,; 
                NAPDAMNT  WITH -laData[17];
                CFISFYEAR WITH lcDistYer ,;
                CFSPPRDID WITH lcDistPrd ,;
                CAPSESSNO WITH lcSequence,;
                CCURRCODE WITH laData[44],;
                NEXRATE   WITH laData[45],;
                NCURRUNIT WITH laData[48],;
                NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45];
                                              &lcExSin2 laData[48],2)
      ELSE
        REPLACE CVENDCODE WITH laData[20] ;
                CINVNO    WITH laData[21] ;
                DAPDTRDAT WITH laData[10],;
                LAPDPOST  WITH .F.       ,;
                CAPDREF   WITH laData[2] ,;
                CAPDTRTYP WITH 'I'       ,;
                cApdActId with 'S'       ,;               
                CAPDGLACT WITH lcDisAcct ,;
                NAPDAMNT  WITH -laData[17];
                CFISFYEAR WITH laData[32],;
                CFSPPRDID WITH laData[31],;
                CAPSESSNO WITH lcSequence,;
                CCURRCODE WITH laData[44],;
                NEXRATE   WITH laData[45],;
                NCURRUNIT WITH laData[48],;
                NEQVAMNT  WITH ROUND(NAPDAMNT &lcExSin1 laData[45];
                                              &lcExSin2 laData[48],2)
  
      ENDIF
      =gfAdd_Info('APDIST')
   ENDIF   
  *B602997,1 AKA  (End)
  
    ** Update the Vendor & the Vendor History files **
    lcOlData1 = laData[1]
    laData[1] = laData[20]
    
    SELECT APINVHDR
    IF !EOF()
      GO RECNO()
    ENDIF  
    
    SELECT APVENDOR
    REPLACE NVENBAL   WITH NVENBAL + ROUND((laData[12] - laData[17]) * laData[45]/laData[48],2) ,;
            NVENCPUR  WITH NVENCPUR + ROUND((laData[12]-laData[17]) * laData[45]/laData[48],2),;
            DVENLPURD WITH laData[10],;
            NVENLPURA WITH ROUND((laData[12] - laData[17]) * laData[45]/laData[48],2)

    =gfAdd_Info('APVENDOR')

    SELECT APVENHST
    REPLACE NVNHPURCH    WITH NVNHPURCH + ROUND((laData[12] - laData[17]) * laData[45]/laData[48],2) ,;
            &lcNewPerFld WITH EVALUATE(lcNewPerFld) + ROUND((laData[12] - laData[17]) * laData[45]/laData[48],2)
    =gfAdd_Info('APVENHST')
    laData[1] = lcOlData1 

    SELECT APINVHDR
    GO lnSavRecNo
    SHOW GET pbEdt DISABLE
  ENDIF
  *E300798,1 Make adjutment if invoice amount does not total approved amount
  =IIF(EMPTY(lcVenInvTypes) OR !llUpdInvLn,.T.,lfInvAdjust())
  *E300798,1 (End)
  
  =gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.F.)

  *E300798,1 Update Invoice Details File

  *B602238,1 Update costing files if invoice lines called
  *=IIF(EMPTY(lcVenInvTypes),.T.,lfUpdFiles())
  
  =IIF(EMPTY(lcVenInvTypes) OR !llUpdInvLn,.T.,lfUpdFiles())
  *B602238,1 (End)

ELSE
  =gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.F.)

  *B601655,1 Add this line to update the flag llCSave and give the user
  *          message that the saving process is canceled [Begin]
  llCSave = .F.
  =gfModalGen("TRM00103B00000","DIALOG",'Saving')
  *B601655,1 Add this line to update the flag llCSave [End]
ENDIF

IF WVISIBLE('CWRAPDISTR')
  =gfChClose('CWRAPDISTR')  
ENDIF
*E301077,71 AMM Close files
IF USED('APVENHST') .AND. llOpVHst
  =gfCloseFile('APVENHST')
ENDIF
IF USED('SYCFACT') .AND. llopFact
  =gfCloseFile('SYCFACT')
ENDIF
*E301077,71 AMM end
SELECT APINVHDR
SET EXACT &lcSavExact

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
* This procedure replaces the default Delete procedure for the push button
* "DELETE"
PROCEDURE lpDelScr
EXTERNAL ARRAY laScrMode

*B803689,1 Define 2 variable to deal with new posting date screen [Begin]
PRIVATE llVoidDate , ldVoidDate , lnDateWdth , llEnterScr
lnDateWdth = IIF('ON'$SET('CENT'),10,8)*(fontme(7,'MS SANS SERIF',9)/9)
STORE .F. TO llVoidDate , llEnterScr
ldVoidDate  = {}

*-- If Period is locked , cannot proceed
IF !lfLockPerd() AND llEnterScr
  =gfObj_Lock(.F.)
  RETURN
ENDIF
*B803689,1 Define 2 variable to deal with new posting date screen [End]

llCancel     = .F.
llIsntalment = .F.
*B800689,1 M.H  Begin.
*IF laData[25]+laData[26]+laData[34] > 0
IF laData[25]+laData[26]+laData[34] <> 0
*B800689,1 M.H  End.
  ** MESSAGE : " You can not void partially or fully paid  "
  **           " invoice.                                  "
  **           "                    ® Ok ¯                 "
  =gfModalGen("TRM04032B00000","DIALOG")
  =gfObj_Lock(.F.)
  RETURN  
ENDIF
SELECT APINVTKT
SET ORDER TO TAG Lncont
=SEEK(PADR(laData[1],8)+PADR(laData[2],12))
LOCATE REST WHILE cVendCode+cApInvNo+cApVILNo+cRSession = ;
                  PADR(laData[1],8)+PADR(laData[2],12) 
IF FOUND()
  *E300798,1 Message : 04178
  *E300798,1 Vendor xxx\Invoice xxx is applied againest one or more receipts. Canot void.
  *E300798,1 Button : 00000 
  *E300798,1 Ok
  =gfModalGen("TRM04178B00000","DIALOG",laData[1]+'|'+laData[2])
  =gfObj_Lock(.F.)
  RETURN  
ENDIF
                  
lcSavExact= SET('EXACT')
SET EXACT ON

*B601897,1 Add this lines to get the Exchange sin [Begin]
lcExSin2 = ' '
lcExSin1 = gfGetExSin(@lcExSin2,laData[44])
*B601897,1 Add this lines to get the Exchange sin [End]
*E301077,71 AMM Open file and make necessary relation
llOpVHst = gfOpenFile(gcDataDir+'APVENHST','VENDYEAR','SH')
SELECT APINVHDR
SET RELATION TO APINVHDR.CVENDCODE + APINVHDR.CFISFYEAR INTO APVENHST ADDITIVE
*E301077,71 AMM end


IF !EMPTY(laData[20])
  ** MESSAGE : " Voiding this invoice will reopen "
  **           " invoice ð for vendor ð .         "
  **           "      < Void >      < Cancel >    "
  lnOption = gfModalGen("TRM04033B04001","DIALOG",ALLTRIM(laData[21])+"|"+ALLTRIM(laData[20]))
  
  IF lnOption = 1
    IF gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.T.)
      SELECT APINVHDR
      lnSavRecNo = RECNO()
      IF SEEK(laData[20]+laData[21])
        lcNewPayFld  = 'NVNHPAY'+ALLTRIM(STR(INT(VAL(APINVHDR.CFSPPRDID))))
        SELECT APVENHST
        *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
        *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
        *E300316,1 laData[45] -----> nExRate
        *E300316,1 laData[48] -----> nCurrUnit
        *REPLACE NVNHCCPAY    WITH NVNHCCPAY  - ROUND(APINVHDR.NINVADJ * laData[45]/laData[48],2),;
                NVNHDISTKN   WITH NVNHDISTKN - ROUND(APINVHDR.NINVDISTK * laData[45]/laData[48],2),;
                NVNHTOTPA    WITH NVNHTOTPA  - ROUND(APINVHDR.NINVADJ * laData[45]/laData[48],2),;
                &lcNewPayFld WITH EVALUATE(lcNewPayFld) - ROUND(APINVHDR.NINVADJ * laData[45]/laData[48],2)

        REPLACE NVNHCCPAY    WITH NVNHCCPAY  - ROUND(APINVHDR.NINVADJ;
                                  &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
                NVNHDISTKN   WITH NVNHDISTKN - ROUND(APINVHDR.NINVDISTK;
                                  &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
                NVNHTOTPA    WITH NVNHTOTPA  - ROUND(APINVHDR.NINVADJ &lcExSin1;
                                  laData[45] &lcExSin2 laData[48],2),;
                &lcNewPayFld WITH EVALUATE(lcNewPayFld) - ROUND(APINVHDR.NINVADJ;
                                  &lcExSin1 laData[45] &lcExSin2 laData[48],2)
        *E300316,1 (End)                
        =gfAdd_Info('APVENHST')
      ENDIF

      SELECT APVENDOR
      *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
      IF SEEK(laData[20])
        *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
        *E300316,1 laData[45] -----> nExRate
        *E300316,1 laData[48] -----> nCurrUnit
        *REPLACE NVENBAL  WITH NVENBAL  + ROUND(APINVHDR.NINVAMNT * laData[45]/laData[48],2),;
                NVENCPAY WITH NVENCPAY - ROUND((APINVHDR.NINVAMNT-APINVHDR.NINVDISOF) * laData[45]/laData[48],2)
        REPLACE NVENBAL  WITH NVENBAL  + ROUND(APINVHDR.NINVAMNT;
                              &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
                NVENCPAY WITH NVENCPAY - ROUND((APINVHDR.NINVAMNT-APINVHDR.NINVDISOF);
                              &lcExSin1 laData[45] &lcExSin2 laData[48],2)
        *E300316,1 (End)                
        =gfAdd_Info('APVENDOR')
      ENDIF  

      SELECT APINVHDR
      REPLACE NINVDISTK WITH 0,;
              NINVADJ   WITH 0,;
              CVENPMETH WITH IIF(APVENDOR.CVENPMETH <> 'C',APVENDOR.CVENPMETH,'P'),;
              CVENCCINV WITH ' ',;
              CVENCCVEN WITH ' '
      =gfAdd_Info('APINVHDR')
      GO lnSavRecNo
    ENDIF  
  ELSE
    llCancel = .T.
  ENDIF
  =gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.F.)
ENDIF

SELECT APINVAHD
lcSavOrder = SET('ORDER')
SET ORDER TO TAG TVENDINV

IF SEEK('I'+lcVendor+laData[2])
  ** MESSAGE : " This invoice has corresponding instalment "
  **           " record.  ð.                               "
  **           "          ® Procced ¯    < Cancel >        "

  IF gfModalGen("TRM04120B00012","DIALOG",lcTDelCor) = 2
    llCancel = .T.
  ELSE
    *B132275,1  TMI [Start] use the local version of gfObj_Lock function instead
    *=gfObj_lock(.T.)
    *llIsntalment = .T.
    IF lfObj_lock(.T.)
      llIsntalment = .T.
    ELSE
      llCancel = .T.
    ENDIF    
    *B132275,1  TMI [End  ] 
  ENDIF
ENDIF

SET ORDER TO &lcSavOrder
SET EXACT &lcSavExact

SELECT APINVHDR

IF llCancel
  ** MESSAGE : " ð process is canceled "
  **           "            ® Ok ¯            "
  =gfModalGen("TRM00103B00000","DIALOG",lcTVoid)
  =gfObj_Lock(.F.)
ELSE 
  SELECT APINVHDR
  IF !EOF()
    GO RECNO()
  ENDIF  
 
  IF gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.T.)

    IF llIsntalment
      SELECT APINVAHD
      =gfObj_Lock(.F.)
      DELETE
    ENDIF  
    
    SELECT APDIST
    SCAN REST WHILE CINVNO + CVENDCODE = laData[2] + lcVendor;
                FOR CAPDSTAT <> 'V'
      REPLACE CAPDSTAT WITH 'V'

      SCATTER MEMVAR MEMO
      
      m.nAPDAmnt = -1 * m.nAPDAmnt
      m.cBatchNo = ' '
      m.lApdPost = .F.
      m.cTrnSleDn= ' '
      m.cAPSessNo= lcSequence
      m.lApdPost = .F.
      m.neqvamnt = -1 * m.neqvamnt

      *B803689,1 if the new date is changed [Begin]
      IF llVoidDate
        m.CFISFYEAR = lcDistYer 
        m.CFSPPRDID = lcDistPrd 
        m.dApdTrDat = ldVoidDate
      ENDIF  
      *B803689,1 if posting date is changed [End]

      
      *B801498,1 Add these line to clear the entry number [Begin]
      m.nEntryNo = 0
      *B801498,1 Add these line to clear the entry number [End]
      
      SELECT(lcTempSave)
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info()

      SELECT APDIST
    ENDSCAN
    
    SELECT(lcTempSave)
    
    IF RECCOUNT() > 0
      SELECT APDIST
      APPEND FROM &gcWorkDir.&lcTempSave
    ENDIF  

    SELECT(lcTempSave)
    ZAP

    lcSavExact= SET('EXACT')
    SELECT APVENDOR
    SET EXACT ON

    IF SEEK(laData[1])

      *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
      *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
      *E300316,1 laData[12] -----> nInvAmnt
      *E300316,1 laData[45] -----> nExRate
      *E300316,1 laData[48] -----> nCurrUnit
      *REPLACE NVENBAL   WITH NVENBAL   - ROUND(laData[12] * laData[45]/laData[48],2),;
              NVENCPUR  WITH NVENCPUR  - ROUND(laData[12] * laData[45]/laData[48],2),;
              NVENOPNDR WITH NVENOPNDR + ROUND(MIN(laData[12],0) * laData[45]/laData[48],2)
      
      REPLACE NVENBAL   WITH NVENBAL   - ROUND(laData[12];
                             &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
              NVENCPUR  WITH NVENCPUR  - ROUND(laData[12];
                             &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
              NVENOPNDR WITH NVENOPNDR + ROUND(MIN(laData[12],0);
                             &lcExSin1 laData[45] &lcExSin2 laData[48],2)
      *E300316,1 (End)
      =gfAdd_Info()

      SELECT APVENHST
      IF SEEK(laData[1]+laData[32])
        lcNewPerFld  = 'NVNHPUR'+ALLTRIM(STR(INT(VAL(laData[31]))))
        *E300296,1 M.H 10/15/95 Add the multiplication by the laData[45]
        *E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
        *E300316,1 laData[17] -----> nInvDisOf
        *E300316,1 laData[12] -----> nInvAmnt
        *E300316,1 laData[45] -----> nExRate
        *E300316,1 laData[48] -----> nCurrUnit

        REPLACE NVNHDISOF    WITH NVNHDISOF - ROUND(laData[17] * laData[45]/laData[48],2),;
                NVNHPURCH    WITH NVNHPURCH - ROUND(laData[12] * laData[45]/laData[48],2),;
                &lcNewPerFld WITH EVALUATE(lcNewPerFld) - ROUND(laData[12] * laData[45]/laData[48],2)

        REPLACE NVNHDISOF    WITH NVNHDISOF - ROUND(laData[17] &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
                NVNHPURCH    WITH NVNHPURCH - ROUND(laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48],2),;
                &lcNewPerFld WITH EVALUATE(lcNewPerFld) - ROUND(laData[12] * laData[45]/laData[48],2)
        *E300316,1 (End)                
        =gfAdd_Info()
      ENDIF
    ENDIF

    SELECT APINVHDR
    REPLACE CINVSTAT  WITH 'V',;
            LLOK_STAT WITH .F.,;
            CLOK_USER WITH ' ',;
            DLOK_DATE WITH {} ,;
            CLOK_TIME WITH ' '

    ** Change the screen mode to the Select mode.
    laScrMode    = .F.
    laScrMode[1] = .T.
  ENDIF  
  =gfFLOCK("APINVHDR,APDIST,APVENDOR,APVENHST",.F.)
ENDIF

IF WVISIBLE('CWRAPDISTR')
  =gfChClose('CWRAPDISTR')  
ENDIF
*E301077,71 AMM Close file
IF USED('APVENHST') .AND. llOpVHst
  =gfCloseFile('APVENHST')
ENDIF
*E301077,71 AMM end

SELECT APINVHDR
SET EXACT &lcSavExact
=gfObj_Lock(.F.)

*!***************************************************************************
*! Name      : lfLockPerd
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/12/2000
*! Purpose   : Check if period is locked or not
*!***************************************************************************
*! Example   : =lfLockPerd()
*!***************************************************************************
*B803689,1 This function return True if period is locked Or False if period is not
FUNCTION lfLockPerd

*B803689,1 Check posting date if it is in Closed period before Voiding the invoice [Begin]
=lfVlDate(gcPrnt_Cmp , @lcDistPrd , @lcDistYer , laData[50] , @llLokPer)
  
*-- Check to see if this period and year is locked 
IF llLokPer
  *Message : " The posting date falls within a locked period.  "
  *          " You cannot change invoice status to Void, "
  *          " unless you enter a voiding date "
  *          " that falls within an open period. "    
  *Buttons : "                    <    Ok    >                "
  =gfModalGen("INM04163B00000" , "DIALOG" , 'invoice status to Void, unless you ' + ;
                                            'enter a voiding date that falls ' +;
                                            'within an open period')
  ldVoidDate = gdSysDate
  DO (gcScrDir + gcWinAppl + '\APVoidDt.SPX')
  llEnterScr = .T.     && Raise flag to ensure that screen was entered
ENDIF
RETURN llVoidDate
*B803689,1 Check posting date if it is in Closed period before Voiding the invoice [End]
*-- End of lfLockPerd.

*!***************************************************************************
*! Name      : lfvCancel
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/12/2000
*! Purpose   : Valid for CANCEL Push button (New Post Date Screen)
*!***************************************************************************
*! Example   : =lfvCancel()
*!***************************************************************************
*B803689,1 Validation function of CANCEL push button
FUNCTION lfvCancel
llVoidDate = .F.
CLEAR READ
*-- End of lfvCancel

*!***************************************************************************
*! Name      : lfvOKDate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/12/2000
*! Purpose   : Valid for OK Push button (New Post Date Screen)
*!***************************************************************************
*! Example   : =lfvOKDate()
*!***************************************************************************
*B803689,1 Validation function of OK push button
FUNCTION lfvOKDate
PRIVATE lnNDateObj
lnNDateObj = OBJNUM(ldVoidDate)

*-- If date is Empty
IF EMPTY(ldVoidDate)
  *Message : " You have to select a date. "
  *Buttons : "         <    Ok    >       "
  = gfModalGen('INM44078B00000','Dialog')
  ldVoidDate = gdSysDate
  _CUROBJ = lnNDateObj
  RETURN
ENDIF

llLokPer = .F.
IF !lfVlDate(gcPrnt_Cmp, @lcDistPrd, @lcDistYer , ldVoidDate , @llLokPer)
  *Message : " The voiding date should be within the posting window. "
  *Buttons : "                      <    Ok    >                     "
  =gfModalGen('TRM04113B00000', 'DIALOG', "voiding ")
  ldVoidDate = gdSysDate
  _CUROBJ = lnNDateObj
  RETURN
ENDIF

*-- if Period and Year is found in Fiscal period file
IF llLokPer
  *Message : " This date falls in a locked period."
  *Buttons : "            <    Ok    >            "
  = gfModalGen('TRM01258B00000','Dialog')
  ldVoidDate = gdSysDate
  _CUROBJ = lnNDateObj
  RETURN
ELSE
  *-- If either Invoice date or Posting date is greater than the new Date entered
  IF laData[10] > ldVoidDate OR laData[50] > ldVoidDate
    *Message : " The voiding date cannot precede invoice date or posting date."
    *Buttons : "                   <    Ok    >                   "
    =gfModalGen("TRM04028B00000","DIALOG",'The voiding |invoice date or posting ')
    ldVoidDate = gdSysDate
    _CUROBJ = lnNDateObj
    RETURN 
  ELSE    && invoice date and posting date is less than new date and also period is not locked and date is not empty
    llVoidDate = .T.     && new posting date is approved
    CLEAR READ   
  ENDIF   
ENDIF
*-- End of lfvOKDate

*!**************************************************************************
*!
*!      Function: lfvUpdTemp
*!
*!**************************************************************************
*
FUNCTION lfvUpdTemp

SCATTER MEMVAR MEMO
m.cStatus  = 'A'
m.cApdStat = 'V'
          
APPEND BLANK
GATHER MEMVAR MEMO
          
m.nApdAmnt  = -1 * m.nApdAmnt
m.nEqvAmnt  = -1 * m.nEqvAmnt
m.lAPDPost  = .F.
m.cBatchNo  = ' '
m.cTrnSledn = ' '
m.cAPSessNo = lcSequence

*B801498,1 Add these line to clear the entry number [Begin]
m.nEntryNo = 0
*B801498,1 Add these line to clear the entry number [End]

APPEND BLANK
GATHER MEMVAR MEMO

*!**************************************************************************
*!
*!      Procedure: lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab

ON KEY LABEL TAB
DO CASE
  CASE WONTOP(lcBrowsTtl)
    IF APVENDOR.CTAXTYPE <> 'T'
      ACTIVATE WINDOW (lcDisChld3)
      _CUROBJ=OBJNUM(ibTaxCode)    
    ELSE
      ACTIVATE WINDOW (lcDisChld4)
      _CUROBJ=OBJNUM(lcApdGlAct)
    ENDIF  
ENDCASE  
ON KEY LABEL TAB DO lpTab

*!**************************************************************************
*!
*!      Procedure: lpShiftTab
*!
*!**************************************************************************
*
PROCEDURE lpShiftTab

ON KEY LABEL BACKTAB
DO CASE
  CASE WONTOP(lcBrowsTtl)
    IF laScrMode[2]
      _CUROBJ = OBJNUM(pbClose)
      ACTIVATE WINDOW (lcDisChld6)
    ELSE  
      _CUROBJ=OBJNUM(laData[33])
      ACTIVATE WINDOW (lcDisChld1)
    ENDIF  
ENDCASE  
ON KEY LABEL BACKTAB DO lpShiftTab 

*!**************************************************************************
*!
*!      Function: lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows

RELEASE PAD _BROWSE OF _MSYSMENU
*ON KEY LABEL ALT+B
ON KEY LABEL ALT+C
ACTIVATE WINDOW (lcBrowsTtl)

*!**************************************************************************
*!
*!      Function: lfSetTrap
*!
*!**************************************************************************
*
FUNCTION lfSetTrap

ON KEY LABEL ESC     lnDummy = 1
ON KEY LABEL TAB     DO lpTab
ON KEY LABEL BACKTAB DO lpShiftTab 

*!**************************************************************************
*!
*!      Function: lfResetTrap
*!
*!**************************************************************************
*
FUNCTION lfResetTrap

ON KEY LABEL ESC DO lfEscap  
ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    

*!**************************************************************************
*!
*!      Function: lfRelease
*!
*!**************************************************************************
*
FUNCTION lfRelease

IF WONTOP() = lcBrowsTtl
*!B500684,1 MAN 06/01/95
*  ON KEY LABEL ESC lnDummy = 1
 =lfSetTrap()
  glFromBrow = .T.
ELSE
 =lfResetTrap()
ENDIF

IF WONTOP() = lcBrowsTtl .OR. WPARENT(WOUTPUT()) <> 'CWRAPDISTR'
  IF WEXIST(lcBrowsTtl)
    *!B500684,1 MAN 06/01/95 Added SAME
    SHOW WINDOW (lcBrowsTtl) REFRESH SAME
    =lfDispObjct()
  ENDIF  
  RELEASE PAD _BROWSE OF _MSYSMENU
  *ON KEY LABEL ALT+B
  ON KEY LABEL ALT+C
  RETURN .T.
ENDIF

RETURN .F. 

*!**************************************************************************
*!
*!      Function: lfTaxAct
*!
*!**************************************************************************
* Valid function for the objects display in the child screen.
*
FUNCTION lfTaxAct

*B801499,1 Change these line to prevent the user from editing the
*Distribution lines if the Posting date falls within a locked
*period [Begin]

*lcObjShow = IIF(laScrMode[2] .OR. laScrMode[1],'DISABLE','ENABLE')

lcObjShow = IIF(laScrMode[2] .OR. laScrMode[1] .OR. llLokPer , 'DISABLE' ,;
                'ENABLE')

*B801499,1 Change these line to prevent the user from editing [End]

IF APVENDOR.CTAXTYPE <> 'T'
  SHOW GET ibTaxCode &lcObjShow
  SHOW GET ibDumm1   DISABLE
  IF _WINDOWS

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *puTax = gfCodDes(lcTaxCode)
    puTax = gfCodDes(lcTaxCode , 'CTAXCODE')
    *E300643,1 Change this line [End]
    
    SHOW GET puTax &lcObjShow
  ENDIF  
   lcActWPos = IIF(WPARENT(WONTOP())='CWRAPDISTR','TOP','BOTTOM')
   lcWinTop  = IIF(lcActWPos = 'TOP','3','4')
   SHOW WINDOW (lcDisChld&lcWinTop) &lcActWPos IN WINDOW CWRAPDISTR
ELSE
  SHOW GET ibDumm1   &lcObjShow
  SHOW GET ibTaxCode DISABLE
  IF _WINDOWS
    SHOW GET puTax DISABLE
  ENDIF  
   lcActWPos =IIF( WPARENT(WONTOP())='CWRAPDISTR','TOP','BOTT')
   lcWinTop  = IIF(lcActWPos = 'TOP','4','3')
   SHOW WINDOW (lcDisChld&lcWinTop) &lcActWPos IN WINDOW CWRAPDISTR
ENDIF
  
*B801499,1 Change these IF Condition to prevent the user from editing the
*Distribution lines if the Posting date falls within a locked
*period [Begin]

*IF laScrMode[3] .OR. laScrMode[4] 

*B801499,1 If Edit mode or Add mode and the Posting date do not falls
*within a locked period
IF (laScrMode[3] .OR. laScrMode[4]) .AND. !llLokPer

*B801499,1 Change these IF Condition to prevent the user from editing [End]
  
  IF lnLineNo = 0
    SHOW GET lnApdAmnt  DISABLE
    SHOW GET ibApGlAcc  DISABLE
    SHOW GET lcApdGLAct DISABLE
    SHOW GET ibTaxCode  DISABLE
    SHOW GET puTax      DISABLE
    SHOW GET pbRemove   DISABLE
  ENDIF
ELSE
  SHOW GET lnApdAmnt  DISABLE
  SHOW GET ibApGlAcc  DISABLE
  SHOW GET lcApdGLAct DISABLE
  SHOW GET ibTaxCode  DISABLE
  SHOW GET puTax      DISABLE
  SHOW GET pbNew      DISABLE
  SHOW GET pbRemove   DISABLE
  
  *B801499,1 Add these lines to prevent the user from editing the
  *Distribution lines if the Posting date falls within a locked
  *period [Begin]
  
  SHOW GET laData[33] DISABLE
  SHOW GET ibApAcc DISABLE
  
  *B801499,1 Add these lines to prevent the user from editing [END]
  
ENDIF

*!**************************************************************************
*!
*!      Function: lfGetArrElem
*!
*!**************************************************************************
*
FUNCTION lfGetArrElem
PARAMETERS lcSrchCode,laSrchArr
PRIVATE lnArrElem
lnArrElem = 1
DO WHILE lcSrchCode <> &laSrchArr[lnArrElem, 2] .AND. ;
         lnArrElem < ALEN((laSrchArr),1)
  lnArrElem = lnArrElem + 1
ENDDO  
RETURN IIF(BETWEEN(lnArrElem, 1, ALEN((laSrchArr), 1)),lnArrElem, 0)

*********************** This session belong to the recipt ******************


*!**************************************************************************
*!
*!      Function: lfVendView
*!
*!**************************************************************************
*
FUNCTION lfVendView

*B600261,1 Line was added by YI-Yasser in 4-6-95 
=gfStatic()

*E300683,1 Add parameters to GPDOPROG
*DO gpDoProg WITH 'AWRAPVENDR WITH "&laData[1]"'
DO gpDoProg WITH 'AWRAPVENDR', .F., 'AP',"'" + laData[1] + "'"
*E300683,1 end

*!**************************************************************************
*!
*!      FUNCTION : lfwScrolVar
*!
*!**************************************************************************
* Hesham el-sheltawi
*
FUNCTION lfwScrolVar
PARAMETERS lnDirection,llScrBar
IF  !llScrBar
  IF MDOWN() OR llFrmScrl
    llFrmScrl = .F.
    RETURN .F.
  ENDIF
ELSE
  DO WHILE MDOWN()
  ENDDO  
  llFrmScrl = .T.
ENDIF  
IF lnDirection=1
  IF (INLIST(LASTKEY(),5,19,127,15) OR llScrBar) AND lnStartAdr>1
    lnStartAdr=lnStartAdr-1
    =lfShiftlaData(lnStartAdr)
    IF lcRemitStat='ENABLE'
      _CUROBJ=OBJNUM(laAddrs[1])
    ENDIF  
  ELSE
    IF INLIST(LASTKEY(),24,9,4,13)
      IF lcRemitStat='ENABLE'    
        _CUROBJ=OBJNUM(laAddrs[1])      
      ENDIF  
    ELSE
*      RETURN .F.
    *  _CUROBJ=_CUROBJ-1
    ENDIF  
  ENDIF
ELSE
  IF (INLIST(LASTKEY(),24,9,4,13) OR llScrBar) AND lnStartAdr+2<5
    lnStartAdr=lnStartAdr+1
    =lfShiftlaData(lnStartAdr)    
    IF lcRemitStat='ENABLE'    
      _CUROBJ=OBJNUM(laAddrs[3])    
    ENDIF  
  ELSE
    IF INLIST(LASTKEY(),5,19,127,15)
      IF lcRemitStat='ENABLE'    
        _CUROBJ=OBJNUM(laAddrs[3])        
      ENDIF  
    ELSE
*      _CUROBJ=_CUROBJ+1
    ENDIF  
  ENDIF
ENDIF
IF lnStartAdr=1
  lcScUpStat='DISABLE'
  SHOW GET ibshiftup DISABLE
ELSE
 lcScUpStat='ENABLE'
 SHOW GET ibshiftup ENABLE
ENDIF

IF lnStartAdr+2=5
  lcScdNStat='DISABLE'
  SHOW GET ibshiftDn DISABLE
ELSE
  lcScdNStat='ENABLE'
  SHOW GET ibshiftDn ENABLE
ENDIF
RETURN llScrBar

*!**************************************************************************
*!
*!      FUNCTION : lfShiftlaData
*!
*!**************************************************************************
*
FUNCTION lfShiftlaData
PARAMETERS lnStartAdr
*=ACOPY(laData,laAddrs,38+lnStartAdr,3)
laAddrs[1] = laData[38+lnStartAdr]
laAddrs[2] = laData[38+lnStartAdr+1]
laAddrs[3] = laData[38+lnStartAdr+2]
SHOW GET laAddrs[1]
SHOW GET laAddrs[2]
SHOW GET laAddrs[3]

*!**************************************************************************
*!
*!      FUNCTION : lfvMoveToData
*!
*!**************************************************************************
*
FUNCTION lfvMoveToData

PARAMETERS lnStartMove
laData[37+lnStartAdr+lnStartMove] = laAddrs[lnStartMove]

*!**************************************************************************
*!
*!      FUNCTION: lfwData_44
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfwData_44

lcOldCurr = laData[44]

*!**************************************************************************
*!
*!      FUNCTION: lfvData_44
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfvData_44

*B602830,1 Add these lines to make sure that the currency file is open
*          before using it [Begin]
IF !USED('SYCCURR')
  =gfOpenFile(gcSysHome + 'SYCCURR' , 'CCURRCODE' , 'SH')
ENDIF    && End of IF !USED('SYCCURR')
*B602830,1 Add these lines to make sure that the currency file is open [End]

IF llBrowse .OR. !SEEK(laData[44],'SYCCURR') .OR. ATC("?",laData[44]) > 0 .AND. LASTKEY() = 13
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laData[44] = lcOldCurr
  ELSE
    laData[44] = laTemp[1]
  ENDIF
ENDIF

IF laData[44] <> lcOldCurr

  *B601612,1 Add this lines to clear the approve for payment amount if the 
  *        invoice currency was changed [Begin]    
  *B601612,1 IF Statment to check that the approve amount do not = 0 
  IF !EMPTY(laData[14])
    ** MESSAGE : " Changing the invoice currency will clear the "
    **           " approved amounts.                            "
    **           "            <   Ok   >  < Cancel >            "
    **
    lnChange = gfModalGen("TRM04160B04004" , "DIALOG")
    *B601612,1 IF the user selected to cancel
    IF lnChange = 2 
      laData[44] = lcOldCurr   && Restore the old value
      llBrowse = .F.
      RETURN 
    ELSE   && Else clear the approve amount
      = lfvClrApr(.T.) 
    ENDIF   && End of IF
  ENDIF   && End of IF
  *B601612,1 Add this lines [End]
  
  IF gfGetMemVar('LLMULCURR') .AND. laData[44] <> gcBaseCurr
    laData[45] = gfChkRate('laData[48]',laData[44],laData[10],.T.)
    
    *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
    *E300316,1 to exchange currency from invoice currency to company base currency.
    *E300316,1 laData[44] ------> cCurrCode
    *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)                
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,laData[44])
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    *E300316,1 (End)

    
    IF laData[45] = 0
      laData[44] = gcBaseCurr
      laData[45] = 1
    ENDIF
    IF gfGetMemVar('LLEDITEXRA')
      SHOW GET laData[45] ENABLE
    ELSE
      SHOW GET laData[45] DISABLE
    ENDIF  
  ELSE

    *B608066,1 NNA 04/30/2007 (Begin) Call Function (gfChkRate) to get information for the new Currancy
    *laData[45] = 1
    laData[45] = gfChkRate('laData[48]',laData[44],laData[10],.T.)
    *B608066,1 NNA (End)

    SHOW GET laData[45] DISABLE
  ENDIF
  *B801894,1 Disable Invoice lines option when the vendor supply only cutting 
  *B801894,1 ticket and invoicing with forign currency.
  IF EMPTY(lcVenInvType) OR  ('M'=lcVenInvType AND gfGetMemVar('LLMULCURR') AND PADR(ladata[44],3)<>PADR(gcBasecurr,3) )
    SHOW GET pbAPVInvDt DISABLE
  ELSE
    SHOW GET pbAPVInvDt ENABLE
  ENDIF
  *B801894,1 (End)
ENDIF
llBrowse = .F.
SELECT APINVHDR

*!**************************************************************************
*!
*!      FUNCTION: lfwData_45
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfwData_45

lnOldRate = laData[45]

*!**************************************************************************
*!
*!      FUNCTION: lfvData_45
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfvData_45

IF laData[45] <= 0
  laData[45] = lnOldRate
ENDIF
SHOW GET laData[45]

*!**************************************************************************
*!
*!      FUNCTION: lfwData_46
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfwData_46

lnOldRate = laData[46]

*!**************************************************************************
*!
*!      FUNCTION: lfvData_46
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfvData_46

IF laData[46] <= 0
  laData[46] = lnOldRate
ENDIF
*E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
*E300316,1 laData[14] -----> nInvAmntAp
*E300316,1 laData[46] -----> nAprExRat
*E300316,1 laData[49] -----> nAprCurUnt
*lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

*B601526,1 Change this line using laData[51] instaed of lnExchange [Begin]
*lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
*B601526,1 Change this line [End]

*E300316,1 (End)
SHOW GET laData[46]
=lfRefresh()

*!**************************************************************************
*!
*!      FUNCTION: lfwData_47
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfwData_47

lcOldCurr = laData[47]

*!**************************************************************************
*!
*!      FUNCTION: lfvData_47
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfvData_47

*B602830,1 Add these lines to make sure that the currency file is open
*          before using it [Begin]
IF !USED('SYCCURR')
  =gfOpenFile(gcSysHome + 'SYCCURR' , 'CCURRCODE' , 'SH')
ENDIF    && End of IF !USED('SYCCURR')
*B602830,1 Add these lines to make sure that the currency file is open [End]

IF llBrowse .OR. !SEEK(laData[47],'SYCCURR') .OR. ATC("?",laData[47]) > 0 .AND. LASTKEY() = 13
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laData[47] = lcOldCurr
  ELSE
    laData[47] = laTemp[1]
  ENDIF
ENDIF

IF laData[47] <> lcOldCurr
  IF gfGetMemVar('LLMULCURR') .AND. laData[47] <> laData[44]
    *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
    *B600849,1                currency to approved currency.
    *B600849,1                laData[47]--> Approved currency code.
    *B600849,1                laData[44]--> Invoice currency code.
    *laData[46] = gfChkRate('laData[49]',laData[47],laData[10],.F.,.F.,laData[44])

    *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
    * Invoice currency is the base currency I make it the To currency 
    * and if not the Approve currency is the To currency 
    *laData[46] = gfChkRate('laData[49]',laData[44],laData[10],.F.,.F.,laData[47])
    *B601416,1 IF Statment to check if the Invoice currency is the 
    * same as the base currency
    IF laData[44] = gcBaseCurr
      laData[46] = gfChkRate('laData[49]',laData[47],laData[10],.F.,.F.,laData[44])
    ELSE      && Else
      laData[46] = gfChkRate('laData[49]',laData[44],laData[10],.F.,.F.,laData[47])
    ENDIF     && End of IF
    *B601416,1 Change the calling of gfChkRate [END]

    *B600849,1 (End)
    
    *E300316,1 HISH 12/06/95. Got the equation signs. (Begin)
    *E300316,1 to exchange currency from invoice currency to approved currency.
    *E300316,1 laData[44] -----> cCurrCode
    *E300316,1 laData[47] -----> cAprCurCode
    *E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin. (Begin)                
    lcExSin4 = ' '

    *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
    * Invoice currency is the base currency I make it the To currency 
    * and if not the Approve currency is the To currency 
    *lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])
    *B601416,1 IF Statment to check if the Invoice currency is the 
    * same as the base currency
    IF laData[44] = gcBaseCurr 
      lcExSin3 = gfGetExSin(@lcExSin4,laData[47],laData[44])  
      lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
      lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
    ELSE
      lcExSin3 = gfGetExSin(@lcExSin4,laData[44],laData[47])  
    ENDIF
    *B601416,1 Change the calling of gfGetExSin [END]

    *lcExSin4 = IIF(lcExSin3 = '*','/','*')
    *E300316,1 (End)
    
    IF laData[46] = 0
      ** MESSAGE : " A valid ð to ð exchange rate could not "
      **           " be found for ð.                        "
      **           "                  ® Ok ¯                "

      *B601667,1 Change this line to fix the parameters [Begin]
      *=gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(laData[44])+'|'+ALLTRIM(laData[47])+'|'+DTOC(laData[10]))

      *B601667,1 IF Statment to check if the Invoice currency is the
      *          Base Currency
      IF laData[44] = gcBaseCurr
        =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(laData[47])+'|'+ALLTRIM(laData[44])+'|'+DTOC(laData[10]))
      ELSE     && Else
        =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(laData[44])+'|'+ALLTRIM(laData[47])+'|'+DTOC(laData[10]))
      ENDIF    && End of IF
      *B601667,1 Change this line to fix the parameters [End]

      laData[46] = 1
      laData[49] = 1
    ENDIF

    IF laData[47]  = laData[44]
      SHOW GET laData[46] DISABLE
    ELSE
      SHOW GET laData[46] ENABLE
    ENDIF  
  ELSE
    laData[47]  = laData[44]
    laData[46]  = 1
    laData[49]  = 1
    SHOW GET laData[46] DISABLE
  ENDIF
ENDIF
*E300316,1 HISH 12/06/95. Used the variables hold signs in the equation. (Begin)    
*E300316,1 laData[14] -----> nInvAmntAp
*E300316,1 laData[46] -----> nAprExRat
*E300316,1 laData[49] -----> nAprCurUnt
*lnExchange = ROUND(laData[14] * laData[46]/laData[49],2)

*B601526,1 Change this line using laData[51] instaed of lnExchange [Begin]
*lnExchange = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
laData[51] = ROUND(laData[14] &lcExSin3 laData[46] &lcExSin4 laData[49],2)
*B601526,1 Change this line [End]

*E300316,1 (End)

=lfRefresh()

SELECT APINVHDR

*!*************************************************************
*! Name      : lfCrCINo
*! Developer : Haytham El_Sheltawi
*! Date      : 12/02/1996
*! Purpose   : This function is to create the Invoice No. 
*!             in the Credit Card screen  
*!*************************************************************
*! Called from : lfvData_20
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B601448,1 This function was added by HS
*!*************************************************************
*
FUNCTION lfCrCINo
PRIVATE lcRetVal

SELECT APINVHDR
lnCurRco = RECNO()     &&Varible to hold the current record number

lcOldVal = IIF(laData[20] = lcOldVenCr , laData[21] , SPACE(12))
lcRetVal = IIF(SEEK(PADR(laData[20] , 8) + PADR(laData[2] , 12)),;
               lcOldVal,IIF(EMPTY(lcOldVal),PADR(laData[2],12) , lcOldVal) )

*IF Statment to check if lnCurRco is a valid record number 
IF lnCurRco <= RECCOUNT()
   GO lnCurRco
ELSE      &&Else go to the end of file
   GO BOTTOM
   SKIP
ENDIF      &&End of IF

SELECT APVENDOR
RETURN lcRetVal

*!**************************************************************************
*!
*!      Function: lfwData_50
*!
*!**************************************************************************
*C100728,1 M.H   Add the invoice posting date to the screen.
* When of the invoce posting date.
*
FUNCTION lfwData_50

IF llInvCan 
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
  _CUROBJ  = OBJNUM(laData[1])
  llInvCan = .F.
ENDIF

ldOldData = laData[50]

*!**************************************************************************
*!
*!      Function: lfvData_50
*!
*!**************************************************************************
*C100728,1 M.H   Add the invoice posting date to the screen.
* Vaildation of the invioce posting date.
*
FUNCTION lfvData_50

*B801499,1 Add these lines to prevent the user from editing the
*Posting date if it falls within a locked period [Begin]

*B801499,1 If Edit mode and the Posting date falls within a locked period
IF llLokPer .AND. laScrMode[3]
  
  *B801499,1 If the user has changed the Posting date
  IF laData[50] <> ldOldData
    
    ** MESSAGE : " The posting date falls within a locked period. "
    **           " You cannot change ð.                           "
    **           "                    <    Ok    >                "
    =gfModalGen("TRM04163B00000" , "DIALOG" , 'it')
    
    laData[50] = ldOldData          && Restore the old Posting date
    _CUROBJ    = _CUROBJ
  
  ELSE    && Else [If the user has not changed the Posting date]
    
    _CUROBJ    = OBJNUM(laData[10])
    
  ENDIF    && End of IF laData[39] <> ldOldData
  RETURN
ENDIF    && End of IF llLokPer .AND. laScrMode[3]

*B801499,1 Add these lines to prevent the user from editing [End]

IF laData[50] = {}
  ** MESSAGE : " The invoice ð.                    "
  **           " Do you want to cancel the invoice?"
  **           "          < Yes >   < No >         "

  IF gfModalGen("TRM04005B00006","DIALOG",'posting date cannot be empty') = 1
    laScrMode    = .F.
    laScrMode[1] = .T.
    llInvCan     = .T.
    SHOW GETS
    _CUROBJ      = OBJNUM(laData[1])
  ELSE 
    laData[50]   = ldOldData
    _CUROBJ      = _CUROBJ
  ENDIF  
ELSE
  IF lfVDtMsg(gcPrnt_Cmp,@lcFisPrd,@lcFisYer)
    lcDistPrd = lcFisPrd
    lcDistYer = lcFisYer
    _CUROBJ   = OBJNUM(laData[10])
  ELSE
    laData[50] = ldOldData
    _CUROBJ    = _CUROBJ
  ENDIF  
ENDIF
*B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [Begin]
IF TYPE('APINVHDR.DPOSTDATE') = 'D'
  IF laData[10] > laData[50]
    ** MESSAGE : " The invoice cannot be greater than"
    **           " the invoice posting date.         "
    **           "         Modifay / Procced          "
    IF gfModalGen("TRM04197B04012","DIALOG","invoices|"+laData[1]) = 1
      laData[50]   = ldOldData
      _CUROBJ = _CUROBJ
      SHOW GET laData[50]
      RETURN
    ENDIF
  ENDIF
ENDIF
*B607158,1 ALB Add validation that invoice date could not be after the invoice posted date [end]

SHOW GET laData[50]

*!*************************************************************
*! Name      : lfActPInq
*! Developer : Haytham El_Sheltawi
*! Date      : 03/30/98
*! Purpose   : Function to activate the POPUP [Inquiry]
*!*************************************************************
*! Called from : APPYINV.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return     : None
*!*************************************************************
*E300807,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfActPInq

*E300798,1 Add new bar in the option pad for invoice lines
*DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT '\<Inquiry'  SKIP FOR (TYPE("laScrMode[1]")="U" .OR. (laScrMode[1] .OR. laScrMode[4])) KEY "ALT+I" 
*DEFINE POPUP _INQURYPOP MARGIN SHADOW
*DEFINE BAR 1 OF _INQURYPOP PROMPT 'Pay\<ments' 
*ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP
*ON SELECTION POPUP _INQURYPOP DO lfInquiry

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions'  SKIP FOR (TYPE("laScrMode[1]")="U" .OR. laScrMode[1]) KEY "ALT+P" 
DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1 OF _INQURYPOP PROMPT 'Pay\<ments' SKIP FOR (laScrMode[1] OR laScrMode[4])

*B801894,1 Disable Invoice lines option when the vendor supply only cutting 
*B801894,1 ticket and invoicing with forign currency.
*DEFINE BAR 2 OF _INQURYPOP PROMPT 'Invoice \<Lines' SKIP FOR (laScrMode[1] OR EMPTY(lcVenInvType) OR EMPTY(laData[44]))
DEFINE BAR 2 OF _INQURYPOP PROMPT 'Invoice \<Lines' SKIP FOR ;
(laScrMode[1] OR EMPTY(lcVenInvType) OR ;
 IIF('M'=lcVenInvType,laData[44]<>gcBaseCurr,EMPTY(laData[44])) )
*B801894,1 (End)

ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP 
ON SELECTION POPUP _INQURYPOP DO lfInquiry
*E300798,1 (End)

RETURN glReadWhen

*!*************************************************************
*! Name      : lfInquiry
*! Developer : Haytham El_Sheltawi
*! Date      : 03/30/98
*! Purpose   : Function to activate the Payments Browse
*!             (From the POPUP [Inquiry])
*!*************************************************************
*! Called from : The Popup [Inquiry] Bar [Payments]
*!*************************************************************
*! Calls       : gfBrows() , gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return     : None
*!*************************************************************
*E300807,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfInquiry
PRIVATE laNothing, lcSavBrFld, lcSavTitle, lcOldAlias,;
        lcOldOrder, lnOldRec

*E300798,1 Add new bar in the option pad for invoice lines
DO CASE
  CASE BAR() = 1
DIMENSION laNothing (6)          && Dummy array to hold the selection from the payments browse

lcSavBrFld = lcBrFields          && Varible to hold the old browse fields
lcSavTitle = lcFile_Ttl          && Varible to hold the old Title
lcOldAlias = ALIAS()             && Varible to hold the old Alias

SELECT APDIST

lcOldOrder = SET('ORDER')                  && Varible to hold the old APDIST.DBF Order
lnOldRec   = IIF(EOF() , 0 , RECNO())      && Save the record number

SET ORDER TO TAG INVVEND

*--IF Statment to place the record pointer in the first record for this
*--invoice and this vendor in the APDIST.DBF    
IF SEEK (laData[2] + laData[1])
  
  *B605986,1 TMI [Start] Browse from ApDist such that approved amt <> 0
  *LOCATE REST;
        WHILE cInvNo = laData[2] .AND. cVendCode = laData[1] ;
          FOR cApdActId = 'C' .AND. cApdStat <> 'V'
  LOCATE REST;
        WHILE cInvNo = laData[2] .AND. cVendCode = laData[1] ;
          FOR cApdActId = 'C' .AND. cApdStat <> 'V' .AND. nApdAmnt # 0
  *B605986,1 TMI [End  ] Browse from ApDist such that approved amt <> 0
  
  *--IF Statment to check if ther is a payments for this invoice
  IF FOUND()
    
    SET ORDER TO
    lcFile_Ttl = "Payments"         && Varible to hold the new Title
    
    *--IF Statment to check if the Platform is DOS or UNIX
    IF _DOS .OR. _UNIX
      
      lcBrFields = "CAPDREF    :R:H= 'Payment#'     :8," +;
                   "PAIDAMOUNT = (-1 * NAPDAMNT)   :R:H= 'Paid amount'  :14," +;
                   "DAPDTRDAT  :R:H= 'Pay. Date'    :10," +;
                   "PAYMETHOD = IIF(CAPDTRTYP = 'P','Printed check',IIF(CAPDTRTYP = 'M','Manual check',IIF(CAPDTRTYP = 'N','Non check payment','Cash payment'))) :R:H= 'Payment method' :17," +;
                   "CBNKCODE   :R:H= 'Bank'         :8," +;
                   "CCHKACCT   :R:H= 'Check. Acct.' :12"          && Varible to hold the new Browse Fields
      
    ELSE    && Else [if the platform is WINDOWS or MAC]
      
      lcBrFields = "CAPDREF    :R:H= 'Payment#'     :8," +;
                   "PAIDAMOUNT = (-1 * NAPDAMNT)   :R:H= 'Paid amount'  :12," +;
                   "DAPDTRDAT  :R:H= 'Pay. Date'    :10," +;
                   "PAYMETHOD = IIF(CAPDTRTYP = 'P','Printed check',IIF(CAPDTRTYP = 'M','Manual check',IIF(CAPDTRTYP = 'N','Non check payment','Cash payment'))) :R:H= 'Payment method' :17," +;
                   "CBNKCODE   :R:H= 'Bank'         :6," +;
                   "CCHKACCT   :R:H= 'Check. Acct.' :12"          && Varible to hold the new Browse Fields
      
    ENDIF    && End of IF _DOS .OR. _UNIX
*---SSHAdj    
    *B605986,1 TMI [Start] Browse from ApDist such that approved amt # 0
    *=gfBrows("FOR cInvNo + cVendCode + cApdTrTyp = laData(2) + laData(1)" +;
             " .AND. cApdActId = 'C' .AND. cApdStat <> 'V'" ,;
             'cApdRef' , 'laNothing')
    =gfBrows("FOR cInvNo + cVendCode + cApdTrTyp = laData(2) + laData(1)" +;
             " .AND. cApdActId = 'C' .AND. cApdStat <> 'V' .AND. nApdAmnt # 0" ,;
             'cApdRef' , 'laNothing')
    *B605986,1 TMI [End  ] Browse from ApDist such that approved amt # 0 

  ELSE    && Else [if ther is no payments for the invoice]
    
    ** MESSAGE : " No ð[payments] found for vendor ð[CVENDCODE + 'invoice No.' +CINVNO ] "
    **           "                  ® Ok ¯ "
    =gfModalGen("TRM04042B00000" , "DIALOG" , "payments|" +;
                ALLTRIM(laData[1]) + " invoice No. " + ALLTRIM(laData[2]))
    
  ENDIF    && End of IF FOUND()
  
ELSE    && Else [if ther is no record for this invoice and this vendor in the APDIST.DBF]
  
  ** MESSAGE : " No ð[payments] found for vendor ð[CVENDCODE + 'invoice No.' +CINVNO ] "
  **           "                  ® Ok ¯ "
  =gfModalGen("TRM04042B00000" , "DIALOG" , "payments|" +;
              ALLTRIM(laData[1]) + " invoice No. " + ALLTRIM(laData[2]))
  
ENDIF    && End of IF SEEK (laData[2] + laData[1])


SELECT APDIST
SET ORDER TO &lcOldOrder

*--Restore the old record number
IF lnOldRec = 0
  GO BOTTOM
  SKIP
ELSE
  GO lnOldRec
ENDIF

SELECT(lcOldAlias)

lcBrFields = lcSavBrFld           && Restoring the old Browse Fields
lcFile_Ttl = lcSavTitle           && Restoring the old Title
  CASE BAR() = 2
    =lfvInvDt()
ENDCASE
*E300798,1 (End)

*!*************************************************************
*! Name      : lfvInvDt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Lines
*!*************************************************************
*! Calls     : APINVDT.SPR
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvDt()
*!*************************************************************
FUNCTION lfvInvDt

*B602830,1 Add these lines to prevent the user from entering the
*          "Invoice Detail" screen if the company is using multi-currency
*          and the currency field is empty [Begin]
IF gfGetMemVar('LLMULCURR') .AND. EMPTY(laData[44])
  ** MESSAGE : " You have to enter the ð."
  **           "         ® Ok ¯          "
  ** ð = 'invoice currency'
  =gfModalGen("TRM04066B00000" , "DIALOG" , "invoice currency")
  
  _CUROBJ = OBJNUM(laData[44])
  SHOW GET laData[44]
  RETURN
ENDIF
*B602830,1 Add these lines to prevent the user from entering [End]

PRIVATE lnAlias

STORE .F. TO llZoomInvD   && Zoom Invoice Detail Browse
lcCstType  = ''
lcOprCode  = ''
lcCodeFilt = ''
lnAlias = SELECT()
STORE '' TO lcBrowseTl,lcLinkCode
DECLARE laBrowArr[1],laOprLots[1]
STORE 'N/A' TO laOprLots[1]
lcSize1 = 'Size1'
lcSize2 = 'Size2'
lcSize3 = 'Size3'
lcSize4 = 'Size4'
lcSize5 = 'Size5'
lcSize6 = 'Size6'
lcSize7 = 'Size7'
lcSize8 = 'Size8'

STORE 1 TO lnCostType
STORE 0 TO lnMarker
lcTktType = 'M'
lcInvDtBr = 'Invoice Details'
lcInvZoom = 'Zoom Invoice Details'
lcStyHdr   = gfItemMask("HI")
lnStyLngth = LEN(lcStyHdr) 

SELECT CODES
lcCodeFilt = SET('FILTER')
SET FILTER TO
*B602238,1 Invoice lines called
llUpdInvLn = .T.
*B602238,1 Update exchange rete and unit signs.
lcExSin1   = gfGetExSin(@lcExSin2,laData[44])
*B602238,1 (End)

STORE 1 TO lnTktType,lnLotNo
=lfOpenFile('Style','Style')
*B602238,1 Set GL_LINK approperiate tag
=IIF(laSetups[3,2]='Y',lfOpenFile('GL_LINK','GL_LINK'),.T.)
=IIF(laSetups[1,2]='Y',lfOpenFile('STYDYE','STYDYE'),.T.)
=lfOpenFile('Scale','Scale')
=lfOpenFile('BOMLINE','BOMLINE')
SELECT STYLE
SET RELATION TO 'S'+Scale INTO Scale
DECLARE laCostType[1]
STORE '' TO laCostType,lcOldValue,lcInvTktNo
lnTktNo = 0
IF 'G' $ lcVenInvTypes
  DECLARE laTktType[1,3]
  laTktType[1,1]='General Expense'
  laTktType[1,2]='G'
  laTktType[1,3]='Document#'
  lnTktNo = 1
ENDIF
*E301995 Alb [Begin]
*E300798,1 If Style Purchase Orders Module is Installed
*IF ('PO' $ gcCmpModules AND ;
  (('I' $ lcVenInvTypes) OR ('S' $ lcVenInvTypes) OR ('R' $ lcVenInvTypes)))
IF ('PO' $ gcCmpModules AND INLIST(lcVenInvTypes,'I','S','R','A','D'))
  =lfOpenFile('POSHDR','POSHDR')
  =lfOpenFile('POSLN','POSLN')
  =lfOpenFile('MFGOPRDT','MFGOPRDT')
  *E300798,1 Adjust Ticket Types Popup
*E301995 Alb [End]
  IF 'I' $ lcVenInvTypes
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Style Purchase Order'
    laTktType[lnTktNo,2]='I'
    laTktType[lnTktNo,3]='Style PO#'
  ENDIF
  IF 'R' $ lcVenInvTypes
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Style Purchase Order Return'
    laTktType[lnTktNo,2]='R'
    laTktType[lnTktNo,3]='Style PO Return#'
  ENDIF
  IF 'S' $ lcVenInvTypes
    =lfOpenFile('SHPMTHDR','SHPMTHDR')
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Shipment'
    laTktType[lnTktNo,2]='S'
    laTktType[lnTktNo,3]='Shipment#'
  ENDIF
ENDIF
*E300798,1 If Style Manufacturing Module is Installed and invoicing with 
*E300798,1 base currency
IF 'MF' $ gcCmpModules AND laData[44] = gcBaseCurr AND 'M' $ lcVenInvTypes
  =lfOpenFile('CUTTKTH','CUTTKTH')
  =lfOpenFile('CUTTKTL','CUTTKTL')
  =lfOpenFile('MFGOPRDT','MFGOPRDT')

  *E300798,1 Adjust Ticket Types Popup
  lnTktNo = lnTktNo + 1
  DIMENSION laTktType[lnTktNo,3]
  laTktType[lnTktNo,1]='Cutting Ticket'
  laTktType[lnTktNo,2]='M'
  laTktType[lnTktNo,3]='Cutting Ticket#'

*E301995 Alb [Begin]
  IF 'A' $ lcVenInvTypes
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Adornment Order'
    laTktType[lnTktNo,2]='A'
    laTktType[lnTktNo,3]='Ador P/O#'
  ENDIF
  IF 'D' $ lcVenInvTypes
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Dying Order'
    laTktType[lnTktNo,2]='D'
    laTktType[lnTktNo,3]='Dye P/O#'
  ENDIF
*E301995 Alb [End]

ENDIF
*E300798,1 If Material Module is Installed
IF 'MA' $ gcCmpModules AND ('L' $ lcVenInvTypes OR 'T' $ lcVenInvTypes OR 'F' $ lcVenInvTypes)
  =lfOpenFile('Fabric','Fabric')
  =IIF(laSetups[2,2]='Y',lfOpenFile('FABDYE','FABDYE'),.T.)
  =lfOpenFile('POFHDR','POFHDR')
  =lfOpenFile('POFLN','POFLN')
  =lfOpenFile('MFGOPRDT','MFGOPRDT')
  IF 'L' $ lcVenInvTypes
    *E300798,1 Adjust Ticket Types Popup
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Material PO'
    laTktType[lnTktNo,2]='L'
    laTktType[lnTktNo,3]='Material PO#'
  ENDIF
  IF 'F' $ lcVenInvTypes
    *E300798,1 Adjust Ticket Types Popup
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Material PO Return'
    laTktType[lnTktNo,2]='F'
    laTktType[lnTktNo,3]='Material PO Return#'
  ENDIF
  *E300798,1 Only when invoicing with base currency
  IF 'T' $ lcVenInvTypes AND laData[44] = gcBaseCurr
    =lfOpenFile('MMFGORDH','MMFGORDH')
    =lfOpenFile('MMFGORDD','MMFGORDD')
    lnTktNo = lnTktNo + 1
    DIMENSION laTktType[lnTktNo,3]
    laTktType[lnTktNo,1]='Material MFG Order'
    laTktType[lnTktNo,2]='T'
    laTktType[lnTktNo,3]='Order#'
  ENDIF
ENDIF
*E300798,1 Restore Screen Defaults
STORE .T. TO llDefQtyDt,llDefAuApr,llDefEdApr
IF FILE("APVINVDT.MEM")
  RESTORE ADDITIVE FROM ("APVINVDT.MEM")
ENDIF
STORE '' TO lcFabric,lcFabClr,lcFabDye,lcFabDesc,lcStyle,lcStyDye,lcStyDesc
IF laScrMode[2]
  SELECT 'APVINVDT'
  SET ORDER TO TAG Orgvinv
ELSE
  SELECT (lcAPvInvDt)
  SET ORDER TO TAG (lcAPvInvDt)
ENDIF
=SEEK(PADR(laData[1],8)+PADR(laData[2],12))
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
*SUM REST IIF(CIMTYP $ 'RF',-ROUND(nAPAMnt,2),ROUND(nAPAMnt,2)),IIF(CIMTYP $ 'RF',-ROUND(nApAprAmnt,2),ROUND(nApAprAmnt,2)) TO lnTInvAmt,lnTAprAmt ;
    WHILE cVendCode+cApInvNo+cAPVILNo = PADR(laData[1],8)+PADR(laData[2],12)
SUM REST IIF(CIMTYP $ 'RF' AND laScrMode[2],-ROUND(nAPAMnt,2),ROUND(nAPAMnt,2)),;
         IIF(CIMTYP $ 'RF' AND laScrMode[2],-ROUND(nApAprAmnt,2),ROUND(nApAprAmnt,2)) TO lnTInvAmt,lnTAprAmt ;
    WHILE cVendCode+cApInvNo+cAPVILNo = PADR(laData[1],8)+PADR(laData[2],12)
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

=SEEK(PADR(laData[1],8)+PADR(laData[2],12))
SCATTER MEMVAR

lcAutoMode= IIF(laScrMode[2] OR lcVenInvTypes=='G','DISABLE','ENABLE')
lcScrMode = IIF(laScrMode[2],'DISABLE','ENABLE')
lcDetMode = IIF(laScrMode[2] OR EOF(),'DISABLE','ENABLE')
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
*lcInvQTot = IIF(laScrMode[2] OR EOF() OR (llDefQtyDt AND INLIST(m.cIMTyp,'I','S','M','R')),'DISABLE','ENABLE')
*lcInvQDet = IIF(laScrMode[2] OR EOF() OR !INLIST(m.cIMTyp,'I','S','M','R') OR !llDefQtyDt,'DISABLE','ENABLE')
*lcAprQDet = IIF(laScrMode[2] OR EOF() OR !INLIST(m.cIMTyp,'I','S','M','R') OR !llDefQtyDt OR !llDefEdApr,'DISABLE','ENABLE')
*lcAprQty  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr OR (llDefQtyDt AND INLIST(m.cIMTyp,'I','S','M','R')),'DISABLE','ENABLE')

lcInvQTot = IIF(laScrMode[2] OR EOF() OR (llDefQtyDt AND INLIST(m.cIMTyp,'I','S','M','R','A','D')),'DISABLE','ENABLE')
lcInvQDet = IIF(laScrMode[2] OR EOF() OR !INLIST(m.cIMTyp,'I','S','M','R','A','D') OR !llDefQtyDt,'DISABLE','ENABLE')
lcAprQDet = IIF(laScrMode[2] OR EOF() OR !INLIST(m.cIMTyp,'I','S','M','R','A','D') OR !llDefQtyDt OR !llDefEdApr,'DISABLE','ENABLE')
lcAprQty  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr OR (llDefQtyDt AND INLIST(m.cIMTyp,'I','S','M','R','A','D')),'DISABLE','ENABLE')
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
lcAprPri  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr,'DISABLE','ENABLE')
lcWIPStat = IIF(laScrMode[2] OR EOF(),'DISABLE','ENABLE')
lcGlAccTxt= IIF(m.cIMTyp='G','GL Account','WIP Account')
IF lnTktNo > 0
  DO (gcScrDir + gcWinAppl + '\APINVDT.SPR')
  
 *B605040,1 Rebuild the TmpDist file in case there is Shipmnet [Start]
 
 
 *B128880,1 EIH 03/14/2006 Add llApdistUp to check if we can update apdist file or not [Begin]
 *IF laScrMode[3] OR laScrMode[4]
 llApdistUp = IIF(laScrMode[4],.T.,llApdistUp)
 IF (laScrMode[3] OR laScrMode[4]) AND llApdistUp
   llApdistUp = .F.
 *B128880,1 EIH 03/14/2006 [End]
 
 
    =lfUpdDist()
 ENDIF  
 *B605040,1 Rebuild the TmpDist file in case there is Shipmnet [END]

ELSE
  *B801894,1 Message : 04182
  *B801894,1 Vendor ð has no access to use Detailed Invoices. 
  *B801894,1 You may only use Distribution Lines option instead.
  *B801894,1 Button : 00000 
  *B801894,1 Ok
  =gfModalGen("TRM04182B00000","DIALOG",laData[1])
  SHOW GET pbAPVInvDt DISABLE
  lcVenInvTypes =''
ENDIF

*E300798,1 Save Screen Defaults
SAVE ALL LIKE llDef* TO ("APVINVDT.MEM")
SELECT STYLE
SET RELATION OFF INTO Scale
*E300798,1 Restore Previous Options Menu Pad.
=lfActPInq()
SELECT CODES
SET FILTER TO &lcCodeFilt
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfDetOpt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Define Screen Options Menu Bar
*!*************************************************************
*! Calls     : lfOptions
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDetOpt()
*!*************************************************************
FUNCTION lfDetOpt

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' SKIP FOR laScrMode[2]
DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1 OF _INQURYPOP PROMPT 'Show Quantity Details'
DEFINE BAR 2 OF _INQURYPOP PROMPT 'Automatic Approve'
DEFINE BAR 3 OF _INQURYPOP PROMPT 'Edit Approved Amount'
DEFINE BAR 4 OF _INQURYPOP PROMPT 'Zoom Invoice Lines'
SET MARK OF BAR 1 OF _INQURYPOP TO llDefQtyDt
SET MARK OF BAR 2 OF _INQURYPOP TO llDefAuApr
SET MARK OF BAR 3 OF _INQURYPOP TO llDefEdApr
SET MARK OF BAR 4 OF _INQURYPOP TO llZoomInvD
ON PAD _INQUIRY OF _MSYSMENU ACTIVATE POPUP _INQURYPOP
ON SELECTION POPUP _INQURYPOP DO lfOptions

*!*************************************************************
*! Name      : lfOptions
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Screen Options Menu Bar
*!*************************************************************
*! Calls     : lfOptions
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOptions()
*!*************************************************************
FUNCTION lfOptions
DO CASE
  CASE BAR() = 1
    llDefQtyDt = !llDefQtyDt
    SET MARK OF BAR 1 OF _INQURYPOP TO llDefQtyDt
  CASE BAR() = 2
    llDefAuApr = !llDefAuApr
    SET MARK OF BAR 2 OF _INQURYPOP TO llDefAuApr
  CASE BAR() = 3
    llDefEdApr = !llDefEdApr
    SET MARK OF BAR 3 OF _INQURYPOP TO llDefEdApr
  CASE BAR() = 4
    llZoomInvD= !llZoomInvD
    =lfInvDtBrow()
    SET MARK OF BAR 4 OF _INQURYPOP TO llZoomInvD
ENDCASE
=lfShowWind()

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Open files and store name of files opened.
*!*************************************************************
*! Calls     : gfOpenFile
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFile()
*!*************************************************************
FUNCTION lfOpenFile
PARAMETERS lcFile,lcTag
IF !USED(lcFile)
  =gfOpenFile(gcDataDir+lcFile,gcDataDir+lcTag,'SH')
  IF EMPTY(laOpnFiles)
    lnInstPos=1
  ELSE
    lnInstPos = ALEN(laOpnFiles)+1
    DIMENSION laOpnFiles[lnInstPos]
    =AINS(laOpnFiles,lnInstPos)
  ENDIF
  laOpnFiles[lnInstPos]=lcFile
ELSE
  SET ORDER TO TAG (lcTag) IN (lcFile)
ENDIF

*!*************************************************************
*! Name      : lfReadAct
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Details Main Screen When Function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfDInvDet
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Details Main Screen Deactivate Function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDInvDet()
*!*************************************************************
FUNCTION lfDInvDet

SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[2]
IF WONTOP()=lcInvDtBr .OR. WONTOP()=lcInvZoom
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTabMove WITH ;
  IIF(WONTOP()=lcInvDtBr,'APINVD11','APINVDT4'),;
  IIF(WONTOP()=lcInvDtBr,'lcInvTktNo','pbAuto')
  ON KEY LABEL BACKTAB DO lpBackMove WITH 'APINVDT4','pbClose'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTabMove
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTabMove WITH 'APINVD11', 'pbClose'
*!*************************************************************
PROCEDURE lpTabMove
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lpBackMove
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackMove WITH 'APINVD11', 'pbClose'
*!*************************************************************
PROCEDURE lpBackMove
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfInvDtBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse Invoice Lines
*!*************************************************************
*! Calls     : lfwInvDt
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfInvDtBrow()
*!*************************************************************
FUNCTION lfInvDtBrow

SELECT IIF(laScrMode[2],'APVINVDT',lcAPvInvDt)
=SEEK(PADR(laData[1],8)+PADR(laData[2],12))
lnMarker = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,cCostDesc=IIF(EMPTY(cBomType),'',lfCostDesc()) :H='Cost Type' :15 :R,"
*lcFields = lcFields +;
           "nApTotQty :H='Inv. Qty.' :R,nApPrice  :H='Inv. Price' :R,nApAmnt :H='Inv. Amount' :R,"+;
           "nApTAprQty:H='Appr. Qty.':R,nApAprPric:H='Appr. Price':R,nApAprAmnt :H='Appr. Amount' :R"
*MAN Added Round 2 to the total amount           
*lcFields = lcFields +;
           "nQty=IIF(CIMTYP $ 'RF',-nApTotQty,nApTotQty) :H='Inv. Qty.' :R,nApPrice  :H='Inv. Price' :R,nAmnt=IIF(CIMTYP $ 'RF',-nApAmnt,nApAmnt) :H='Inv. Amount' :R,"+;
           "nAprQty=IIF(CIMTYP $ 'RF',-nApTAprQty,nApTAprQty):H='Appr. Qty.':R,nApAprPric:H='Appr. Price':R,nAprAmnt=IIF(CIMTYP $ 'RF',-nApAprAmnt,nApAprAmnt) :H='Appr. Amount' :R"

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
*lcFields = lcFields +;
           "nQty=IIF(CIMTYP $ 'RF',-nApTotQty,nApTotQty) :H='Inv. Qty.' :R,nApPrice  :H='Inv. Price' :R,nAmnt=ROUND(IIF(CIMTYP $ 'RF',-nApAmnt,nApAmnt),2) :H='Inv. Amount' :R,"+;
           "nAprQty=IIF(CIMTYP $ 'RF',-nApTAprQty,nApTAprQty):H='Appr. Qty.':R,nApAprPric:H='Appr. Price':R,nAprAmnt=ROUND(IIF(CIMTYP $ 'RF',-nApAprAmnt,nApAprAmnt),2) :H='Appr. Amount' :R"

lcFields = lcFields +;
           "nQty=IIF(CIMTYP $ 'RF' AND laScrMode[2] ,-nApTotQty,nApTotQty) :H='Inv. Qty.' :R,nApPrice  :H='Inv. Price' :R,nAmnt=ROUND(IIF(CIMTYP $ 'RF' AND laScrMode[2],-nApAmnt,nApAmnt),2) :H='Inv. Amount' :R,"+;
           "nAprQty=IIF(CIMTYP $ 'RF' AND laScrMode[2],-nApTAprQty,nApTAprQty):H='Appr. Qty.':R,nApAprPric:H='Appr. Price':R,nAprAmnt=ROUND(IIF(CIMTYP $ 'RF' AND laScrMode[2],-nApAprAmnt,nApAprAmnt),2) :H='Appr. Amount' :R"
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

IF WEXIST(lcInvZoom)
  HIDE WINDOW (lcInvZoom)
  RELEASE WINDOW (lcInvZoom)
ENDIF
IF WEXIST(lcInvDtBr)
  HIDE WINDOW (lcInvDtBr)
  RELEASE WINDOW (lcInvDtBr)
ENDIF
IF llZoomInvD
  BROWSE FIELDS &lcFields  ;
         WINDOW APINVD01   ;
         IN WINDOW APINVDT ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         KEY PADR(laData[1],8)+PADR(laData[2],12) ;
         WHEN lfwInvDt()   ;
         TITLE lcInvZoom
ELSE
  BROWSE FIELDS &lcFields  ;
         WINDOW APINVD0    ;
         IN WINDOW APINVDT ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         KEY PADR(laData[1],8)+PADR(laData[2],12) ;
         WHEN lfwInvDt()   ;
         TITLE lcInvDtBr
ENDIF

*!*************************************************************
*! Name      : lfwInvDt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh Invoice Lines Browse
*!*************************************************************
*! Calls     : lfShowWind()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwInvDt()
*!*************************************************************
FUNCTION lfwInvDt

*B605967,1 RAE
*-- In Edit Mode
IF laScrMode[3]
  llClosed = lfvCheck()
ENDIF
*B605967,1 RAE

=SEEK(cVendCode+cApInvNo+cAPVILNo,IIF(laScrMode[2],'APDIST',lcTmpDist))
lcExStat=SET('EXACT')
SET EXACT ON
lnTktType = IIF(EMPTY(cIMTyp),1,ASUBSCRIPT(laTktType,ASCAN(laTktType,cImTyp),1))
SET EXACT &lcExStat
lnMarker = RECNO()
IF llZoomInvD
  SHOW WINDOW (lcInvZoom) REFRESH SAME
ELSE
  SCATTER MEMVAR
  =lfShowWind()
  SHOW WINDOW (lcInvDtBr) REFRESH
ENDIF
*!*************************************************************
*! Name      : lfCostDesc
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Get Cost Type Description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  Cost Type Description
*!*************************************************************
*! Example            :  =lfCostDesc()
*!*************************************************************
FUNCTION lfCostDesc
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
*RETURN EVAL('la'+IIF(INLIST(CIMTyp,'S','R'),'I',IIF(CIMTYP='F','L',CIMTYP))+'CSTTYPE'+;
            "[ASUBSCRIPT('la'+IIF(INLIST(CIMTyp,'S','R'),'I',IIF(CIMTYP='F','L',CIMTYP))+;
            'CSTTYPE',ASCAN('la'+IIF(INLIST(CIMTyp,'S','R'),'I',IIF(CIMTYP='F','L',CIMTYP))+;
            'CSTTYPE',cBomType),1),1]")

RETURN EVAL('la'+IIF(INLIST(CIMTyp,'S','R'),'I',IIF(CIMTYP='F','L',IIF(CIMTYP='D','M',CIMTYP)))+'CSTTYPE'+;
            "[ASUBSCRIPT('la'+IIF(INLIST(CIMTyp,'S','R'),'I',IIF(CIMTYP='F','L',IIF(CIMTYP='D','M',CIMTYP)))+;
            'CSTTYPE',ASCAN('la'+IIF(INLIST(CIMTyp,'S','R'),'I',IIF(CIMTYP='F','L',IIF(CIMTYP='D','M',CIMTYP)))+;
            'CSTTYPE',cBomType),1),1]")
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
*!*************************************************************
*! Name      : lfvNInvLine
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add new invoice line
*!*************************************************************
*! Calls     : lfwInvDt
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNInvLine()
*!*************************************************************
FUNCTION lfvNInvLine
*?AMIN
SCATTER MEMVAR BLANK
SHOW GETS DISABLE ONLY
SHOW GET lnTktType ENABLE
_CUROBJ = OBJNUM(lnTktType)

*!*************************************************************
*! Name      : lfvTktType
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate applied ticket type
*!*************************************************************
*! Calls     : lfGetCstEle()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTktType()
*!*************************************************************
FUNCTION lfvTktType

SCATTER MEMVAR BLANK
=gfwCodePop(@laCodes,'MFGCODE','N')
=IIF(APVENDOR.CTAXTYPE<>'T',gfwCodePop(@laCodes,'CTAXCODE','N'),.T.)
m.cIMTyp = laTktType[lnTktType,2]
IF m.cIMTyp = 'G'
  
  *B602946,1 Add this line to refresh the default Expense Account [Begin]
  lcDistAcct = IIF(!EMPTY(APVENDOR.cExpAcct) , APVENDOR.cExpAcct ,;
                   IIF(SEEK(laData[4] , 'APDIV') .AND.;
                       !EMPTY(APDIV.cExpAcct) , APDIV.cExpAcct ,;
                       APSETUP.cExpAcct))
  *B602946,1 Add this line to refresh the default Expense Account [End]
  
  *B605040,1 Get the next sequence line no. [Start]
  laData[52]  = lfGetMaxLin()
  *B605040,1 Get the next sequence line no. [End]
  SELECT (lcAPvInvDt)
  laData[52]  = laData[52] + 1
  m.cStatus   = 'A'
  m.cVendCode = laData[1]
  m.cAPInvNo  = laData[2]
  m.cApVILNo  = STR(laData[52],4)
  m.cApDGlAct = lcDistAcct
  =IIF(APVENDOR.CTAXTYPE<>'T',gfwCodePop(@laCodes,'CTAXCODE','D'),.T.)
  INSERT INTO (lcAPvInvDt) FROM MEMVAR
  =lfUpdApDist(m.cApVILNo,0,m.cApDGlAct,m.cIMTyp)
  =lfwInvDt()
ELSE
  =lfGetCstEle()
  =lfRefresh('APINVD13')
  SHOW GET lnCostType ENABLE
ENDIF

*!*************************************************************
*! Name      : lfGetCstEle
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Get Available Cost Elements for selected Ticket
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetCstEle()
*!*************************************************************
FUNCTION lfGetCstEle

*?AMIN


DECLARE laCostType[1]
STORE 'N/A' TO laCostType
DO CASE
  *E300798,1 Invoice Applied To Style PO, Shipments or Cutting Tickets
  CASE INLIST(m.cIMTyp,'I','S','M','R')
    DECLARE laCostType[ALEN(IIF(m.cIMTyp='M','laMCstType','laICstType'),1),3]
    =ACOPY(IIF(m.cIMTyp='M','laMCstType','laICstType'),laCostType)
    lnCostType=IIF(EMPTY(m.cBomType),1,ASUBSCRIPT(laCostType,ASCAN(laCostType,m.cBomType),1))
  *E300798,1 Invoice Applied To Material PO or Material MFG Orders
  CASE INLIST(m.cImTyp,'T','L','F')
    DECLARE laCostType[ALEN(IIF(m.cIMTyp='T','laTCstType','laLCstType'),1),3]
    =ACOPY(IIF(m.cIMTyp='T','laTCstType','laLCstType'),laCostType)
    lnCostType=IIF(EMPTY(m.cBomType),1,ASUBSCRIPT(laCostType,ASCAN(laCostType,m.cBomType),1))

  CASE INLIST(m.cIMTyp,'A')		&&Alb*E301995
    DECLARE laCostType[ALEN(IIF(m.cIMTyp='A','laACstType','laICstType'),1),3]
    =ACOPY(IIF(m.cIMTyp='A','laACstType','laICstType'),laCostType)
    lnCostType=IIF(EMPTY(m.cBomType),1,ASUBSCRIPT(laCostType,ASCAN(laCostType,m.cBomType),1))

  CASE INLIST(m.cIMTyp,'D')		&&Alb*E301995
    DECLARE laCostType[ALEN(IIF(m.cIMTyp='D','laMCstType','laICstType'),1),3]
    =ACOPY(IIF(m.cIMTyp='D','laMCstType','laICstType'),laCostType)
    lnCostType=IIF(EMPTY(m.cBomType),1,ASUBSCRIPT(laCostType,ASCAN(laCostType,m.cBomType),1))

ENDCASE

*!*************************************************************
*! Name      : lfvCostType
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoiced Cost type
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCostType()
*!*************************************************************
FUNCTION lfvCostType

*B605040,1 Get the next sequence line no. [Start]
laData[52]  = lfGetMaxLin()
*B605040,1 Get the next sequence line no. [End]
SELECT (lcAPvInvDt)
laData[52]  = laData[52] + 1
m.cStatus   = 'A'
m.cVendCode = laData[1]
m.cAPInvNo  = laData[2]
m.cApVILNo  = STR(laData[52],4)
m.cIMTyp    = laTktType[lnTktType,2]
m.cBomType  = laCostType[lnCostType,2]
m.cCostType = laCostType[lnCostType,3]
m.cApDGlAct = lcDistAcct

*B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
llApdistUp = .T.
*B128880,1 EIH 03/14/2006 [End]


IF m.cCostType = 'M'
  =gfwCodePop(@laCodes,'MFGCODE','D')
  m.cOprCode = laMfgCode[lnMfgOpr,2]
  SHOW GET lnMfgOpr ENABLE
ELSE
  =gfwCodePop(@laCodes,'MFGCODE','N')
  m.cOprCode = REPLICATE(IIF(m.cCostType='P','*',IIF(m.cCostType='D','#',' ')),6)
  INSERT INTO (lcAPvInvDt) FROM MEMVAR
  =lfwInvDt()
  *B606054,1 ALB Control the cut tickit and style [Begin]
  *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
  *IF INLIST(m.cIMTyp,'I','S','M')
  IF INLIST(m.cIMTyp,'I','S','M','A','D','R')
  *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
    SHOW GET ibStyle    ENABLE
    SHOW GET lcStyle    ENABLE
    SHOW GET ibStyDye   ENABLE
    SHOW GET lcStyDye   ENABLE
  ELSE
    SHOW GET lcFabric   ENABLE
    SHOW GET lcFabClr   ENABLE
    SHOW GET ibFabric   ENABLE
    SHOW GET ibFabClr   ENABLE
    SHOW GET ibFabDye   ENABLE
    SHOW GET lcFabDye   ENABLE
  ENDIF
  SHOW GET ibTktNo    ENABLE
  SHOW GET lcInvTktNo ENABLE
  *B606054,1 ALB Control the cut tickit and style [End]

ENDIF

*!*************************************************************
*! Name      : lfvMfgOpr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate MFG Operation
*!*************************************************************
*! Calls     : lfLots()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvMfgOpr()
*!*************************************************************
FUNCTION lfvMfgOpr

m.cOprCode = laMfgCode[lnMfgOpr,2]
=gfRltFld(m.cOprCode,@laMfgRFld,'MFGCODE')
IF !EMPTY(lcMfgGlAcnt)
  *E300798,1 Message : 04152
  *E300798,1 MFG Code has been setup to be applied directly through the
  *E300798,1 cost sheet program
  *E300798,1 Button : 00000 
  *E300798,1 Ok
  =gfModalGen("TRM04152B00000","DIALOG",laMfgCode[lnMfgOpr,1])
  m.cOprCode = cOprCode
  SHOW GET lnMfgOpr
  RETURN
ENDIF
INSERT INTO (lcAPvInvDt) FROM MEMVAR
=lfwInvDt()
*B606054,1 ALB Control the cut tickit and style [Begin]
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
*IF INLIST(m.cIMTyp,'I','S','M')
IF INLIST(m.cIMTyp,'I','S','M','D')
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
  SHOW GET ibStyle    ENABLE
  SHOW GET lcStyle    ENABLE
  SHOW GET ibStyDye   ENABLE
  SHOW GET lcStyDye   ENABLE
ELSE
  SHOW GET lcFabric   ENABLE
  SHOW GET lcFabClr   ENABLE
  SHOW GET ibFabric   ENABLE
  SHOW GET ibFabClr   ENABLE
  SHOW GET ibFabDye   ENABLE
  SHOW GET lcFabDye   ENABLE
ENDIF
SHOW GET ibTktNo    ENABLE
SHOW GET lcInvTktNo ENABLE
*B606054,1 ALB Control the cut tickit and style [End]

*!*************************************************************
*! Name      : lfvAplTkt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Ticket Applied to Invoice Line
*!*************************************************************
*! Calls     : lfvTicket(),lfItemQty(),lfUpdAplTkt()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplTkt()
*!*************************************************************
FUNCTION lfvAplTkt
*B605967,1 RAE -- Store the exist value of cTktNo.
lcCutTktNo = cTktNo
*B605967,1 RAE

PRIVATE lcBrFields,lcLinkCode
IF lcInvTktNo <> IIF(m.cIMTyp='S',ShipNO,cTktNo) AND lfIsContrib()
  llBrowse = .F.
  lcInvTktNo = IIF(m.cIMTyp='S',ShipNo,cTktNo)
  RETURN
ENDIF
lcLinkCode = SPACE(6)
=lfvTicket(laTktType[lnTktType,2],@lcInvTktNo,@lcLinkCode,m.cCostType)
IF lcInvTktNo <> IIF(m.cIMTyp='S',ShipNO,cTktNo)
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  
  *B603371,1 SSE 03/20/2000 Assign the new Ticket no to cTktNo [Begin]
  *B605967,1 RAE -- Store the old value of cTktNo if the chosen cTktNo is not valid.
  *cTktNo = lcInvTktNo
  IF !EMPTY(lcInvTktNo)
    cTktNo = lcInvTktNo
  ELSE
    cTktNo = lcCutTktNo
  ENDIF
  *B605967,1 RAE

  *B603371,1 SSE 03/20/2000 [End]
  
  *E300798,1 If Item has been entered, Check if this Item exists in the
  *E300798,1 select Ticket
  IF !EMPTY(lcInvTktNo) AND !EMPTY(Item)
    IF !lfItemQty(m.cIMTyp,lcInvTktNo,m.cLotNo,m.cBomType,m.cOprCode,;
                  m.Item,m.Color,m.cDyelot)
      lcInvTktNo = IIF(m.cIMTyp='S',ShipNo,cTktNo)
      RETURN
    ENDIF
    =lfShowWind()
  ENDIF
  IF laSetups[3,2]='Y'
    IF !SEEK(lcLinkCode+'013','GL_LINK')
      =SEEK('DEFDEF'+'013','GL_LINK')
    ENDIF
    STORE GL_Link.GlAcnt TO m.cApDGlAct
  ELSE
    STORE lcDistAcct TO m.cApDGlAct
  ENDIF
  *C200447,1 ALB Change the defaulte WIP Account to LFNM Account (ENGLAND)[Begin]
  IF ASCAN(laEvntTrig , PADR('UPDTAPGL',10)) <> 0
    =gfDoTriger('APPYINV',PADR('UPDTAPGL',10))
  ENDIF
  *C200447,1 ALB Change the defaulte WIP Account to LFNM Account (ENGLAND)[end]
  SELECT (lcAPvInvDt)
  REPLACE cTktNo    WITH IIF(m.cImTyp='S','',lcInvTktNo) ,;
          ShipNO    WITH IIF(m.cIMTyp='S',lcInvTktNo,'') ,;
          cApDGlAct WITH m.cApDGlAct ,;
          cStatus   WITH IIF(cStatus='S','M',cStatus)
  m.cTktNo = cTKtNo
  m.ShipNo = ShipNo
  IF !EMPTY(lcInvTktNo) AND m.cCostType='M' AND lfLots()
    SHOW GET lnLotNo ENABLE
  ELSE
    SHOW GET lnLotNo DISABLE
  ENDIF
  IF EMPTY(lcInvTktNo)
    SHOW GET ibWIPAcnt   DISABLE
    SHOW GET m.cApDGlAct DISABLE
  ELSE
    =lfUpdAplTkt()
    SHOW GET ibWIPAcnt   ENABLE
    SHOW GET m.cApDGlAct ENABLE
    *B606054,1 ALB  [Begin]
    SHOW GET ibTktNo    DISABLE
    SHOW GET lcInvTktNo DISABLE
    *B606054,1 ALB  [END]
  ENDIF  
ENDIF
llBrowse = .F.

*!*************************************************************
*! Name      : lfIsContrib
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Check if this invoice line has been contrinuted
*!*************************************************************
*! Calls     : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIsContrib()
*!*************************************************************
FUNCTION lfIsContrib
PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcAPInvTkt)
=SEEK(&lcAPvInvDt..cVendCode+&lcAPvInvDt..cApInvNo+&lcAPvInvDt..cApVILNo)
LOCATE REST WHILE cVendCode+cApInvNo+cApVILNo=;
&lcAPvInvDt..cVendCode+&lcAPvInvDt..cApInvNo+&lcAPvInvDt..cApVILNo FOR !EMPTY(cRSession)
IF FOUND()
  *E300798,1 Message : 04174
  *E300798,1 This Invoice line has been contributed. Cannot Modify.
  *E300798,1 Button : 00000 
  *E300798,1 Ok
  =gfModalGen("TRM04174B00000","DIALOG")
ENDIF
SELECT (lnAlias)
RETURN (FOUND(lcAPInvTkt))

*!*************************************************************
*! Name      : lfRefLot
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh Operation Lot
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRefLot()
*!*************************************************************
FUNCTION lfRefLot
PARAMETERS lcLot

SELECT DIST cLotNo FROM MFGOPRDT ;
WHERE cIMTyp+cTktNo+cOprCode+cLotNo+TranCd = m.cIMTyp+m.cTktNo+m.cOprCode ;
INTO ARRAY laOprLots

lnLotNo = ASCAN(laOprLots,lcLot)

*!*************************************************************
*! Name      : lfLots
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Get Operation Lots
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfLots()
*!*************************************************************
FUNCTION lfLots
PRIVATE lnLots

SELECT DIST cLotNo FROM MFGOPRDT ;
WHERE cIMTyp+cTktNo+cOprCode+cLotNo+TranCd = m.cIMTyp+m.cTktNo+m.cOprCode ;
INTO ARRAY laOprLots
lnLots = _TALLY
DO CASE
  CASE lnLots = 0
    lnLotNo = 0
    m.cLotNo = SPACE(2)
  OTHERWISE
    lnLotNo = 1
    m.cLotNo = laOprLots[1]
ENDCASE
REPLACE cStatus WITH IIF(cStatus='S' AND cLotNo <> m.cLotNo,'M',cStatus) ,;
        cLotNo  WITH m.cLotNo
RETURN(lnLots > 1)

*!*************************************************************
*! Name      : lfvLotNo
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Operation Lot #
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLotNo()
*!*************************************************************
FUNCTION lfvLotNo

m.cLotNo = laOprLots[lnLotNo]
IF m.cLotNo <> cLotNo
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  REPLACE cLotNo  WITH m.cLotNo ,;
          cStatus WITH IIF(cStatus='S','M',cStatus)
ENDIF

*!*************************************************************
*! Name      : lfShowWind
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show Invoice line Detail
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowWind()
*!*************************************************************
FUNCTION lfShowWind

*B605967,1 RAE -- Include llClosed 
*lcDetMode = IIF(laScrMode[2] OR EOF(),'DISABLE','ENABLE')
*lcInvQTot = IIF(laScrMode[2] OR EOF() OR (llDefQtyDt AND INLIST(cIMTyp,'I','S','M','R') AND !EMPTY(Item)),'DISABLE','ENABLE')
*lcInvQDet = IIF(laScrMode[2] OR EOF() OR !INLIST(cIMTyp,'I','S','M','R') OR !llDefQtyDt,'DISABLE','ENABLE')
*lcAprQDet = IIF(laScrMode[2] OR EOF() OR !INLIST(cIMTyp,'I','S','M','R') OR !llDefQtyDt OR !llDefEdApr,'DISABLE','ENABLE')
*lcAprQty  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr OR (llDefQtyDt AND INLIST(cIMTyp,'I','S','M','R')  AND !EMPTY(Item)),'DISABLE','ENABLE')
*lcAprPri  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr,'DISABLE','ENABLE')
*lcWIPStat = IIF(laScrMode[2] OR EOF() ,'DISABLE','ENABLE')

lcDetMode = IIF(laScrMode[2] OR EOF() OR llClosed,'DISABLE','ENABLE')
*E301995,1 ALB Add adorment order to ap [BEGIn]
*lcInvQTot = IIF(laScrMode[2] OR EOF() OR llClosed OR (llDefQtyDt AND INLIST(cIMTyp,'I','S','M','R') AND !EMPTY(Item)),'DISABLE','ENABLE')
*lcInvQDet = IIF(laScrMode[2] OR EOF() OR llClosed OR !INLIST(cIMTyp,'I','S','M','R') OR !llDefQtyDt,'DISABLE','ENABLE')
*lcAprQDet = IIF(laScrMode[2] OR EOF() OR llClosed OR !INLIST(cIMTyp,'I','S','M','R') OR !llDefQtyDt OR !llDefEdApr,'DISABLE','ENABLE')
*lcAprQty  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr OR llClosed OR (llDefQtyDt AND INLIST(cIMTyp,'I','S','M','R')  AND !EMPTY(Item)),'DISABLE','ENABLE')

lcInvQTot = IIF(laScrMode[2] OR EOF() OR llClosed OR (llDefQtyDt AND INLIST(cIMTyp,'I','S','M','R','A','D') AND !EMPTY(Item)),'DISABLE','ENABLE')
lcInvQDet = IIF(laScrMode[2] OR EOF() OR llClosed OR !INLIST(cIMTyp,'I','S','M','R','A','D') OR !llDefQtyDt,'DISABLE','ENABLE')
lcAprQDet = IIF(laScrMode[2] OR EOF() OR llClosed OR !INLIST(cIMTyp,'I','S','M','R','A','D') OR !llDefQtyDt OR !llDefEdApr,'DISABLE','ENABLE')
lcAprQty  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr OR llClosed OR (llDefQtyDt AND INLIST(cIMTyp,'I','S','M','R','A','D')  AND !EMPTY(Item)),'DISABLE','ENABLE')

*E301995,1 ALB Add adorment order to ap [end]
lcAprPri  = IIF(laScrMode[2] OR EOF() OR !llDefEdApr OR llClosed,'DISABLE','ENABLE')
lcWIPStat = IIF(laScrMode[2] OR EOF() OR llClosed,'DISABLE','ENABLE')
*B605967,1 RAE

IF llZoomInvD
  HIDE WINDOW 'APINVD10','APINVD11','APINVD22','APINVD13','APINVD21','APINVD22','APINVD23','APINVD24' SAME
  SHOW GETS DISABLE
ELSE
  lcGlAccTxt= IIF(m.cIMTyp='G','GL Account','WIP Account')
  SHOW WINDOW 'APINVD10' TOP
  SHOW WINDOW 'APINVD11' TOP
  SHOW WINDOW 'APINVD12' TOP
  SHOW WINDOW 'APINVD13' TOP
  =lfRefresh('APINVD11')
  =lfRefresh('APINVD13')
  =lfGetCstEle()
  *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [Begin]
  IF EMPTY(&lcAPVInvDt..CTAXCODE)
     =gfwCodePop(@laCodes,'CTAXCODE','N')
  ELSE
    =gfwCodePop(@laCodes,'CTAXCODE','T')
  ENDIF
  *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [end]
  
  IF m.cCostType = 'M'
    *B606137,1 ALB Display the cost title in Invoice line in edit and view mode [Begin]
    *=gfwCodePop(@laCodes,'MFGCODE','T')
    =gfwCodePop(@laCodes,'MFGCODE','T')	
    *B606137,1 ALB Display the cost title in Invoice line in edit and view mode [End]
    =lfRefLot(m.cLotNo)
  ELSE
    *=gfwCodePop(@laCodes,'MFGCODE','N')	
  ENDIF
  lcInvTktNo = IIF(m.cIMTyp='S',m.ShipNo,m.cTktNo)
  SHOW GETS WINDOW 'APINVD13' DISABLE ONLY
  SHOW GET ibInvDet     ENABLE
  SHOW GET m.nAPPrice   &lcDetMode
  
  *B602830,1 Change this line because we are going to switch between 2 Get
  *          Fields for the "Invoice Quantity" to be able to add the
  *          decimal points to the "Invoice Quantity" in the case of
  *          document types Material PO, Material PO Return and
  *          Material MFG Order [Begin]
  *SHOW GET m.nAPTotQty  &lcInvQTot
  
  *-- If the document type is Material PO, Material PO Return or Material
  *-- MFG Order.
  IF INLIST(m.cImTyp,'T','L','F')
    
    *-- Disable the Get Field that has no decimal points.
    SHOW OBJECT OBJNUM(m.nAPTotQty) + 1 DISABLE
    *-- Use the Get Field that has decimal points.
    lnQtyObj = OBJNUM(m.nAPTotQty)
    SHOW OBJECT lnQtyObj &lcInvQTot
    
  ELSE    && Else (If the document type is anything but Material PO, Material PO Return and Material MFG Order.)
    
    *-- Disable the Get Field that has decimal points.
    SHOW OBJECT OBJNUM(m.nAPTotQty) DISABLE
    *-- Use the Get Field that has no decimal points.
    lnQtyObj = OBJNUM(m.nAPTotQty) + 1
    SHOW OBJECT lnQtyObj &lcInvQTot
    
  ENDIF    && End of IF INLIST(m.cImTyp,'T','L','F')
  *B602830,1 Change this line because we are going to switch between 2 [End]
  
  SHOW GET m.nApAmnt    &lcDetMode
  *E300798,1 Show Approved Amount
  SHOW GET m.nAPAprPric &lcAprPri
  
  *B602830,1 Change this line because we are going to switch between 2 Get
  *          Fields for the "Approve Quantity" to be able to add the
  *          decimal points to the "Approve Quantity" in the case of
  *          document types Material PO, Material PO Return and
  *          Material MFG Order [Begin]
  *SHOW GET m.nApTAprQty &lcAprQty
  
  *-- If the document type is Material PO, Material PO Return or Material
  *-- MFG Order.
  IF INLIST(m.cImTyp,'T','L','F')
    
    *-- Disable the Get Field that has no decimal points.
    SHOW OBJECT OBJNUM(m.nApTAprQty) + 1 DISABLE
    *-- Use the Get Field that has decimal points.
    lnApQtyObj = OBJNUM(m.nApTAprQty)
    SHOW OBJECT lnApQtyObj &lcAprQty
    
  ELSE    && Else (If the document type is anything but Material PO, Material PO Return and Material MFG Order.)
    
    *-- Disable the Get Field that has decimal points.
    SHOW OBJECT OBJNUM(m.nApTAprQty) DISABLE
    *-- Use the Get Field that has no decimal points.
    lnApQtyObj = OBJNUM(m.nApTAprQty) + 1
    SHOW OBJECT lnApQtyObj &lcAprQty
    
  ENDIF    && End of IF INLIST(m.cImTyp,'T','L','F')
  *B602830,1 Change this line because we are going to switch between 2 [End]
  
  SHOW GET m.nAPAprAmnt &lcAprPri
  DO CASE
    *E300798,1 Free Format Invoice
    CASE INLIST(m.cIMTyp,'G',' ')
      HIDE WINDOW 'APINVD21','APINVD22','APINVD23' SAME
      SHOW GETS WINDOW 'APINVD21' DISABLE ONLY
      SHOW GETS WINDOW 'APINVD22' DISABLE ONLY
      SHOW GETS WINDOW 'APINVD23' DISABLE ONLY
      SHOW WINDOW 'APINVD24' TOP
      SHOW GETS WINDOW 'APINVD24' &lcDetMode ONLY
      IF APVENDOR.CTAXTYPE<>'T'
        *=gfwCodePop(@laCodes,'CTAXCODE','T'),.T.)
        *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [Begin]
        =gfwCodePop(@laCodes,'CTAXCODE','L')
        IF laScrMode[2]
          IF !EMPTY(APVInvDt.CTAXCODE)
            lnTaxCode = ASUBSCRIPT(laTaxCode,ASCAN(laTaxCode,APVInvDt.CTAXCODE),1)
            SHOW GET lnTaxCode
          ENDIF
        ENDIF
        *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [End]
        lcWIPStat = IIF(laScrMode[2] OR EOF() OR !EMPTY(&lcTmpDist..CTAXCODE),'DISABLE','ENABLE')
      ENDIF  
  

    *E300798,1 Invoice Applied To Style PO, Shipments or Cutting Tickets
    *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
    *CASE INLIST(m.cIMTyp,'I','S','M','R')
    CASE INLIST(m.cIMTyp,'I','S','M','R','A','D')
    *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
      HIDE WINDOW 'APINVD23','APINVD24' SAME
      SHOW GETS WINDOW 'APINVD23' DISABLE ONLY
      SHOW GETS WINDOW 'APINVD24' DISABLE ONLY
      SHOW WINDOW 'APINVD21' TOP
      *B606054,1 ALB Control the cut tickit [Begin]
      *SHOW GET ibTktNo    &lcScrMode
      *SHOW GET lcInvTktNo &lcScrMode
      *B606054,1 ALB Control the cut tickit [End]
      STORE Item     TO lcStyle
      STORE cDyelot  TO lcStyDye
      STORE cApVIDes TO lcStyDesc
      SHOW GETS WINDOW 'APINVD21' &lcScrMode ONLY
      *B606054,1 ALB Control the Style [Begin]
      SHOW GET ibStyle    DISABLE    
      SHOW GET lcStyle    DISABLE
      SHOW GET lcStyDye   DISABLE
      SHOW GET ibStyDye   DISABLE    
      SHOW GET lcFabric   DISABLE
      SHOW GET lcFabClr   DISABLE
      SHOW GET ibFabric   DISABLE
      SHOW GET ibFabClr   DISABLE
      *B606054,1 ALB Control the Style [End]

      *B607278,1 ALB Get the correct WIP account with shipment ticket [Begin]
      lcWIPStat = IIF(EMPTY(m.cAPDGLAct),'DISABLE','ENABLE')
      SHOW GET m.cAPDGLAct &lcWIPStat.
      *B607278,1 ALB Get the correct WIP account with shipment ticket [end]
      
      IF !SEEK(lcStyle,'Style') OR Style.cDye_Flg <> 'Y'
        SHOW GET lcStyDye DISABLE
      ENDIF
      IF llDefQtyDt
        *E300798,1 Show Quantity per size
        SHOW WINDOW 'APINVD22' TOP
        =lfRefresh('APINVD22')
        SHOW GET m.nAPQty1  &lcInvQDet
        SHOW GET m.nAPQty2  &lcInvQDet
        SHOW GET m.nAPQty3  &lcInvQDet
        SHOW GET m.nAPQty4  &lcInvQDet
        SHOW GET m.nAPQty5  &lcInvQDet
        SHOW GET m.nAPQty6  &lcInvQDet
        SHOW GET m.nAPQty7  &lcInvQDet
        SHOW GET m.nAPQty8  &lcInvQDet
        SHOW GET m.nAPAprQty1 &lcAprQDet
        SHOW GET m.nAPAprQty2 &lcAprQDet
        SHOW GET m.nAPAprQty3 &lcAprQDet
        SHOW GET m.nAPAprQty4 &lcAprQDet
        SHOW GET m.nAPAprQty5 &lcAprQDet
        SHOW GET m.nAPAprQty6 &lcAprQDet
        SHOW GET m.nAPAprQty7 &lcAprQDet
        SHOW GET m.nAPAprQty8 &lcAprQDet
      ELSE
        *E300798,1 Hide Quantity per size
        HIDE WINDOW 'APINVD22' SAME
        SHOW GETS WINDOW 'APINVD22' DISABLE ONLY
      ENDIF
    *E300798,1 Invoice Applied To Material PO or Material MFG Orders
    CASE INLIST(m.cImTyp,'T','L','F')
      HIDE WINDOW 'APINVD21','APINVD22','APINVD24' SAME
      SHOW GETS WINDOW 'APINVD21' DISABLE ONLY
      SHOW GETS WINDOW 'APINVD22' DISABLE ONLY
      SHOW GETS WINDOW 'APINVD24' DISABLE ONLY
      STORE SUBSTR(Item,1,7) TO lcFabric
      STORE Color    TO lcFabClr
      STORE cDyelot  TO lcFabDye
      STORE cApVIDes TO lcFabDesc
      *B606054,1 ALB Control the cut tickit [Begin]
      *SHOW GET ibTktNo    &lcScrMode
      *SHOW GET lcInvTktNo &lcScrMode
      *B606054,1 ALB Control the cut tickit [End]
      SHOW WINDOW 'APINVD23' TOP
      SHOW GETS WINDOW 'APINVD23' &lcScrMode ONLY
      *adalb IF EMPTY(lcFabric)
        SHOW GET ibFabClr DISABLE
        SHOW GET lcFabClr DISABLE
        SHOW GET ibFabric DISABLE
        SHOW GET lcFabric DISABLE
      *ENDIF
      IF !SEEK(lcFabric+lcFabClr,'Fabric') OR Fabric.cDye_Flg <> 'Y'
        SHOW GET ibFabDye DISABLE
        SHOW GET lcFabDye DISABLE
      ENDIF
  ENDCASE  
  *B602337,1 Read opration lot when invoicing operation cost items only.
  *=IIF(laScrMode[2],.T.,lfLots())
  *B602337,1 (End)
  
  SHOW GET ibWIPAcnt   &lcWIPStat
  SHOW GET m.cApDGlAct &lcWIPStat
  *B605967,1 RAE
  *SHOW GET pbNewLine   &lcScrMode
  IF laScrMode[3]
    SHOW GET pbNewLine ENABLE
  ELSE
    SHOW GET pbNewLine &lcScrMode
  ENDIF
  *B605967,1 RAE
  =lfRefresh('APINVDT1')
ENDIF
IF lcVenInvTypes=='G'
  SHOW GET pbAuto DISABLE
ELSE
  *B605967,1 RAE 
  *SHOW GET pbAuto &lcScrMode
  IF laScrMode[3]
    SHOW GET pbAuto ENABLE
  ELSE
    SHOW GET pbAuto &lcScrMode
  ENDIF
  *B605967,1 RAE
ENDIF
SHOW GET pbRemLine &lcDetMode

*B802110,1 Change this IF condition to give the user the capability to add
*          debit memo lines, compare the absolute values [Begin]
*IF !EMPTY(lcInvTktNo) AND nApAprAmnt > 0 AND m.cImTyp <> 'G'
IF !EMPTY(lcInvTktNo) AND ABS(nApAprAmnt) > 0 AND m.cImTyp <> 'G'
*B602830,1 Change this IF condition to give the user the capability [End]
  
  SHOW GET pbContrib ENABLE
ELSE
  SHOW GET pbContrib DISABLE
ENDIF
SHOW GET pbApprove &lcDetMode
SHOW GET pbClose ENABLE
*!*************************************************************
*! Name      : lfvTicket
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Tickets
*!*************************************************************
*! Calls     : POSBrow(),ARIABROW(),CutBrow(),POFBROW(),lfChkCstCur()
*!*************************************************************
*! Passed Parameters  :  lcType     : Ticket Type
*!                                   'S' : Shipment
*!                                   'I' : Style PO
*!                                   'R' : Style PO Return
*!                                   'M' : Style Cutting Ticket
*!                                   'L' : Material PO
*!                                   'F' : Material PO Return
*!                                   'T' : Material MFG Order
*!                       lcTicket   : Ticket #
*!                       lcLinkCode : WIP Link Code
*!                       lcCstType  : Cost Type
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTicket()
*!*************************************************************
FUNCTION lfvTicket
PARAMETERS lcType,lcTicket,lcLinkCode,lcCstType
PRIVATE lcBrFields,lnAlias

lnAlias = SELECT()
*B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines[Begin]
lnVendRecNo = RECNO('APVENDOR')
*B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines[End]
DO CASE
  *E301995,1 ALB
  CASE INLIST(lcType,'A','D')
    IF llBrowse OR (!EMPTY(lcTicket) AND !SEEK(lcType+lcTicket,'POSHDR'))
      =gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
      =POSBrow(@lcTicket,'',lcType)
    ENDIF
    lcLinkCode = IIF(EMPTY(lcTicket),SPACE(6),POSHDR.Link_Code)
  *E300798,1 Style Purchase Order
  CASE INLIST(lcType,'I','R')	
    IF llBrowse OR (!EMPTY(lcTicket) AND !SEEK(IIF(lcType='I','P','R')+lcTicket,'POSHDR'))
      =POSBrow(@lcTicket,'',IIF(lcType='I','P','R'))
    ENDIF
    lcLinkCode = IIF(EMPTY(lcTicket),SPACE(6),POSHDR.Link_Code)
  *E300798,1 Style Shipments
  CASE lcType = 'S'
    IF llBrowse OR (!EMPTY(lcTicket) AND !SEEK(lcTicket,'SHPMTHDR'))
      lcBrFields = "ShipNo    :R :H='Shipment #',"+;
                   "Status    :R :H='S',"+;
                   "Entered   :R :H='Entered',"+;
  	               "Cartons   :R :H='Cartons',"+;
    	           "AirWayB   :R :H='Air-Way Bill #',"+;
        	       "ETA       :R :H='E.T.A.',"+;
    	           "TotQty    :R :H='In-Transit',"+;
    	           "Recv_Stk  :R :H='Received',"+;
        	       "Recv_Dam  :R :H='Damaged',"+;
        	       "Recv_Can  :R :H='Canceled',"+;
    	           "cVessel   :R :H='Airline/Vessel',"+;
       	           "Reference :R :H='Reference'"
      SELECT SHPMTHDR
      lcTicket= IIF(ARIABROW('','Style Purchase Orders Shipments',gnBrHSRow1,;
                  gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','ShipNo',;
                  'laBrowArr'),ShipNo,SPACE(6))
    ENDIF
    *B607278,1 ALB Get the correct WIP account with shipment ticket [Begin]
    lcLinkCode = IIF(EMPTY(lcTicket),SPACE(6),SUBSTR(lfGetGlLnk(lcTicket,lcStyle,''),1,6))
    *B607278,1 ALB Get the correct WIP account with shipment ticket [end]

  *E300798,1 Style Cutting Tickets
  CASE lcType = 'M'
    IF llBrowse OR (!EMPTY(lcTicket) AND !SEEK(lcTicket,'CUTTKTH'))
      =CutBrow(@lcTicket)
    ENDIF
    lcLinkCode = IIF(EMPTY(lcTicket),SPACE(6),CUTTKTH.Link_Code)
  *E300798,1 Material Purchase Orders
  CASE INLIST(lcType,'L','F')
    IF llBrowse OR (!EMPTY(lcTicket) AND !SEEK(IIF(lcType='F','R','P')+lcTicket,'POFHDR'))
      =POFBROW(IIF(lcType='F','R','P'),'',@lcTicket)
    ENDIF
    lcLinkCode = IIF(EMPTY(lcTicket),SPACE(6),POFHDR.Link_Code)
  *E300798,1 Material MFG Orders
  CASE lcType = 'T'
    IF llBrowse OR (!EMPTY(lcTicket) AND !SEEK(lcTicket,'MMFGORDH'))
      *B603371,1 SSE 03/20/2000 Global function to browse Manufacturing Order file [Begin]
      =gfMFGOrdBr(@lcTicket) 
      *B603371,1 SSE 03/20/2000 [End]
    ENDIF
    lcLinkCode = IIF(EMPTY(lcTicket),SPACE(6),MMFGORDH.Link_Code)
ENDCASE
*B605967,1 RAE
*lcTicket=IIF(lfChkCstCur(lcType,lcTicket,lcCstType),lcTicket,SPACE(6))
lcTicket=IIF(lfChkCstCur(lcType,lcTicket,lcCstType),lcTicket,lcCutTktNo)
*B605967,1 RAE

*B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines[Begin]
SELECT APVENDOR
GOTO (lnVendRecNo)
*B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines[End]

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfChkCstCur
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Check Ticket Currency againest Invoice Currency
*!*************************************************************
*! Calls     : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  lcTktType  : Ticket Type
*!                                   'S' : Shipment
*!                                   'I' : Style PO
*!                                   'R' : Style PO Return
*!                                   'M' : Style Cutting Ticket
*!                                   'L' : Material PO
*!                                   'F' : Material PO Return
*!                                   'T' : Material MFG Order
*!                                   'A' : Adorment Order
*!                                   'D' : Dying Order
*!                       lcTicket   : Ticket #
*!                       lcCstType  : Cost Type
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkCstCur()
*!*************************************************************
FUNCTION lfChkCstCur
PARAMETERS lcTktType,lcTicket,lcCstType
DO CASE
  *B606696,1
  CASE INLIST(lcTktType,'S') AND !EMPTY(lcTicket)
    IF lcCstType $ 'PD'
      *B121188,1 MHM 02/25/2004 Enhance Speed of Shipment and contripution in invoice line screen[Start]
      *SELECT Poshdr.*;
*        FROM Poshdr, Posln;
*        WHERE Poshdr.cstytype +Poshdr.po = Posln.cstytype + posln.po ;
*          AND Posln.shipno = lcTicket;
*          AND IIF(lcCstType = 'P',Poshdr.cpricecur,Poshdr.cdutycur) <> laData[44] ;
*          INTO ARRAY laDummyArr
    
      SELECT  Poshdr.*;
         FROM Poshdr, Posln;
        WHERE Poshdr.cstytype +Poshdr.po = Posln.cstytype + posln.po ;
          AND POSLN.shipno+POSLN.crsession+POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd = lcTicket;
          AND IIF(lcCstType = 'P',Poshdr.cpricecur,Poshdr.cdutycur) <> laData[44] ;
         INTO ARRAY laDummyArr
      *B121188,1 MHM [End]

      IF _TALLY > 0 
        =gfModalGen(.F.,'DIALOG',.F.,.F.,;
         'Placing an invoice for a shipment that includes Purchase Orders with several currencies'+;
         'is not allowed. Please use the option to invoice by Purchase Orders.')
        RETURN(.F.)
      ENDIF
    ENDIF
  *B606696,1
  
  CASE INLIST(lcTktType,'I','R') AND !EMPTY(lcTicket) AND SEEK(IIF(lcTktType='I','P','R')+lcTicket,'POSHDR')
    IF POSHDR.Status = 'B'
      *E300798,1 Message : 04177
      *E300798,1 Purchase Order# 9999 has a 'BID' Status. Invoicing is not allowed
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04177B00000","DIALOG",'Purchase Order')
      RETURN(.F.)
    ENDIF
    IF lcTktType = 'I' AND POSHDR.Status = 'H'
      *E300798,1 Message : 04169
      *E300798,1 Cost Sheet has not been generated for Purchase Order.
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04169B00000","DIALOG",'Purchase Order')
      RETURN(.F.)
    ENDIF
    *E300798,1 Message : 04176
    *E300798,1 Purchase Order has been canceled.
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    IF (POSHDR.Status='X' AND gfModalGen("TRM04176B00000","DIALOG",'Purchase Order|canceled')=1) OR ;
       (POSHDR.Status='S' AND gfModalGen("TRM04176B00000","DIALOG",'Purchase Order|closed')=1)
      RETURN(.F.)
    ENDIF
    *B602554,1 Fix compare invoice currency with PO currency
    *IF lcCstType ='P' AND laData[44] <> POSHDR.cPriceCur
    IF lcCstType ='P' AND PADR(laData[44],3) <> POSHDR.cPriceCur
    *B602554,1 (End)
    
      *E300798,1 Message : 04168
      *E300798,1 Only Purchase Orders with Price in xxxx are allowed.
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04168B00000","DIALOG",'Purchase Orders|Price|'+laData[44])
      RETURN(.F.)
    ENDIF
    *B602554,1 Fix compare invoice currency with PO currency
    *IF lcCstType ='D' AND laData[44] <> POSHDR.cDutyCur
    IF lcCstType ='D' AND PADR(laData[44],3) <> POSHDR.cDutyCur
    *B602554,1 (End)
    
      *E300798,1 Message : 04168
      *E300798,1 Only Purchase Orders with Duty in xxxx are allowed.
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04168B00000","DIALOG",'Purchase Orders|Duty|'+laData[44])
      RETURN(.F.)
    ENDIF
    
    *B603653,1 KHM 07/20/2000 (Begin) Adding the checking of the invoice
    *B603653,1                currency with the duty currency when selecting 
    *B603653,1                Misc. as the Misc. currency will be treated 
    *B603653,1                as the duty currency.
    IF lcCstType ='M' AND PADR(laData[44],3) <> POSHDR.cDutyCur
      *-- Only Purchase Orders with Misc. in xxxx are allowed.
      =gfModalGen("TRM04168B00000","DIALOG",'Purchase Orders|Misc.|'+laData[44])
      RETURN(.F.)
    ENDIF
    *B603653,1 KHM 07/20/2000 (End)
    
  CASE lcTktType ='M' AND !EMPTY(lcTicket) AND SEEK(lcTicket,'CUTTKTH')
    *E300798,1 Message : 04176
    *E300798,1 Cutting Ticket has been canceled.
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    IF (CUTTKTH.Status='X' AND gfModalGen("TRM04176B00000","DIALOG",'Cutting Ticket|canceled')=1) OR ;
       (CUTTKTH.Status='S' AND gfModalGen("TRM04176B00000","DIALOG",'Cutting Ticket|closed')=1)
      RETURN(.F.)
    ENDIF
    IF CUTTKTH.Status = 'H'
      *E300798,1 Message : 04169
      *E300798,1 Cost Sheet has not been generated for Cutting Ticket.
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04169B00000","DIALOG",'Cutting Ticket')
      RETURN(.F.)
    ENDIF
  CASE lcTktType ='T' AND !EMPTY(lcTicket) AND SEEK(lcTicket,'MMFGORDH')
    *E300798,1 Message : 04176
    *E300798,1 Material MFG Order has been canceled.
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    IF (MMFGORDH.Status='X' AND gfModalGen("TRM04176B00000","DIALOG",'Material MFG Order|canceled')=1) OR ;
       (MMFGORDH.Status='S' AND gfModalGen("TRM04176B00000","DIALOG",'Material MFG Order|closed')=1)
      RETURN(.F.)
    ENDIF
    IF MMFGORDH.Status = 'H'
      *E300798,1 Message : 04169
      *E300798,1 Cost Sheet has not been generated for Material MFG Order.
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04169B00000","DIALOG",'Material MFG Order')
      RETURN(.F.)
    ENDIF
  CASE INLIST(lcTktType,'L','F') AND !EMPTY(lcTicket) AND SEEK(IIF(lcTktType='F','R','P')+lcTicket,'POFHDR')
    IF POFHDR.Status = 'B'
      *E300798,1 Message : 04177
      *E300798,1 Purchase Order# 9999 has a 'BID' Status. Invoicing is not allowed
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04177B00000","DIALOG",'Purchase Order')
      RETURN(.F.)
    ENDIF
    *E300798,1 Message : 04176
    *E300798,1 Material Purchase Order has been canceled.
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    IF (POFHDR.Status='X' AND gfModalGen("TRM04176B00000","DIALOG",'Material Purchase Order|canceled')=1) OR ;
       (POFHDR.Status='S' AND gfModalGen("TRM04176B00000","DIALOG",'Material Purchase Order|closed')=1)
      RETURN(.F.)
    ENDIF
    *B602554,1 Fix compare invoice currency with PO currency
    *IF lcCstType ='P' AND laData[44] <> POFHDR.cPriceCur
    IF lcCstType ='P' AND PADR(laData[44],3) <> POFHDR.cPriceCur
    *B602554,1 (End)
    
      *E300798,1 Message : 04168
      *E300798,1 Only Purchase Orders with Price in xxxx are allowed.
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04168B00000","DIALOG",'Purchase Orders|Price|'+laData[44])
      RETURN(.F.)
    ENDIF
    *B602554,1 Fix compare invoice currency with PO currency
    *IF lcCstType ='D' AND laData[44] <> POFHDR.cDutyCur
    IF lcCstType ='D' AND PADR(laData[44],3) <> POFHDR.cDutyCur
    *B602554,1 (End)
    
      *E300798,1 Message : 04168
      *E300798,1 Only Purchase Orders with Duty in xxxx are allowed.
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04168B00000","DIALOG",'Purchase Orders|Duty|'+laData[44])
      RETURN(.F.)
    ENDIF
    *B603653,1 KHM 07/20/2000 (Begin) Adding the checking of the invoice
    *B603653,1                currency with the duty currency when selecting 
    *B603653,1                Misc. as the Misc. currency will be treated 
    *B603653,1                as the duty currency.
    IF lcCstType ='M' AND PADR(laData[44],3) <> POFHDR.cDutyCur
      *-- Only Purchase Orders with Misc. in xxxx are allowed.
      =gfModalGen("TRM04168B00000","DIALOG",'Purchase Orders|Misc.|'+laData[44])
      RETURN(.F.)
    ENDIF
    *B603653,1 KHM 07/20/2000 (End)
    
  *E301995,1 Alb Add Adorment/DYING order to invoice line [Begin]
  CASE INLIST(lcTktType,'D','A') AND !EMPTY(lcTicket) AND SEEK(IIF(lcTktType='I','P',lcTktType)+lcTicket,'POSHDR')
    lcDesc = IIF(lcTktType = 'D','Dying','Adorment')
    IF POSHDR.Status = 'B'
      *E301995,1 Message : 04177
      *E301995,1 Dying Order# 9999 has a 'BID' Status. Invoicing is not allowed
      *E301995,1 Button : 00000 
      *E301995,1 Ok
      =gfModalGen("TRM04177B00000","DIALOG",'Dying Order')
      RETURN(.F.)
    ENDIF
    IF POSHDR.Status = 'H'
      *E301995,1 Message : 04169
      *E301995,1 Cost Sheet has not been generated for Dying Order.
      *E301995,1 Button : 00000 
      *E301995,1 Ok
      =gfModalGen("TRM04169B00000","DIALOG",lcDesc +' Order')
      RETURN(.F.)
    ENDIF
    *E301995,1 Message : 04176
    *E301995,1 Dying Order has been canceled.
    *E301995,1 Button : 00000 
    *E301995,1 Ok
    IF (POSHDR.Status='X' AND gfModalGen("TRM04176B00000","DIALOG",lcDesc +' Order|canceled')=1) OR ;
       (POSHDR.Status='S' AND gfModalGen("TRM04176B00000","DIALOG",lcDesc +' Order|closed')=1)
      RETURN(.F.)
    ENDIF
    IF lcCstType ='P' AND PADR(laData[44],3) <> POSHDR.cPriceCur
      *E301995,1 Message : 04168
      *E301995,1 Only Purchase Orders with Price in xxxx are allowed.
      *E301995,1 Button : 00000 
      *E301995,1 Ok
      =gfModalGen("TRM04168B00000","DIALOG",lcDesc +' Orders|Price|'+laData[44])
      RETURN(.F.)
    ENDIF
    IF lcCstType ='D' AND PADR(laData[44],3) <> POSHDR.cDutyCur
      *E301995,1 Message : 04168
      *E301995,1 Only Purchase Orders with Duty in xxxx are allowed.
      *E301995,1 Button : 00000 
      *E301995,1 Ok
      =gfModalGen("TRM04168B00000","DIALOG",lcDesc +' Orders|Duty|'+laData[44])
      RETURN(.F.)
    ENDIF
    IF lcCstType ='M' AND PADR(laData[44],3) <> POSHDR.cDutyCur
      *-- Only Dying Orders with Misc. in xxxx are allowed.
      =gfModalGen("TRM04168B00000","DIALOG",lcDesc +' Orders|Misc.|'+laData[44])
      RETURN(.F.)
    ENDIF
    *E301995,1 Alb Add Adorment/DYING order to invoice line [End]
ENDCASE

*!*************************************************************
*! Name      : lfvInvPrice
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvPrice()
*!*************************************************************
FUNCTION lfvInvPrice

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
IF (m.CIMTYP $ 'RF' OR llDebitM) AND (m.nAPPrice > 0)
  m.nAPPrice = m.nAPPrice * -1
ENDIF
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

IF m.nAPPrice <> nAPPrice
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  *B602830,1 Add these lines to make sure that the "Invoice Unit Cost" and
  *          the "Approved Unit Cost" have the same sign [Begin]
  
  *-- If the "Invoice Unit Cost" and the "Approved Unit Cost" have
  *-- different signs
  IF SIGN(m.nAPPrice) <> 0 .AND. SIGN(m.nAPAprPric) <> 0 .AND.;
     SIGN(m.nAPPrice) <> SIGN(m.nAPAprPric)
    
    *-- Message : 04187
    *-- "Invoice unit cost and approved unit cost must have the same sign."
    *-- Button : 00000 
    *-- "                          <    Ok    >                           "
    =gfModalGen('TRM04187B00000' , 'ALERT')
    
    m.nAPPrice = nAPPrice        && Restore the old value
    RETURN
  ENDIF    && End of IF SIGN(m.nAPPrice) <> 0 .AND. ...
  *B602830,1 Add these lines to make sure that the "Invoice Unit Cost" [End]
  
  m.nAPAprPric = IIF(llDefAuApr,m.nAPPrice,m.nAPAprPric)
  m.nAPAprPric = IIF(lfChkAplTkt(''),m.nAPAprPric,nAPAprPric)
  m.nApAmnt    = m.nAPTotQty*m.nAPPrice
  m.nAPAprAmnt = m.nApTAprQty*m.nAPAprPric
  
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  *lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  *lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2))
  lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  =lfRefresh('APINVD10')
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  PRIVATE lnDiffAmnt
  lnDiffAmnt =  (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2)) * IIF((m.CIMTYP $ 'RF'),1,IIF(llDebitM,-1,1))
  *=lfUpdApDist(cApVILNo,ROUND(m.nAPAMnt,2)-ROUND(nAPAMnt,2),cApDGlAct,m.cIMTyp)
  =lfUpdApDist(cApVILNo,lnDiffAmnt,cApDGlAct,m.cIMTyp)
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  =IIF(llDefAuApr,lfUpdAplTkt(),.T.)
  REPLACE nAPPrice   WITH m.nAPPrice   ,;
          nAPAprPric WITH m.nAPAprPric ,;
          nApAmnt    WITH m.nApAmnt    ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
  SHOW GET m.nApAmnt ENABLE
  IF llDefEdApr
    SHOW GET m.nAPAprPric ENABLE
    SHOW GET m.nAPAprAmnt ENABLE
  ELSE
    SHOW GET m.nAPAprPric DISABLE
    SHOW GET m.nAPAprAmnt DISABLE
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvAprPrice
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Approved Price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAprPrice()
*!*************************************************************
FUNCTION lfvAprPrice

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
IF (m.CIMTYP $ 'RF' OR llDebitM) AND (m.nAPAprPric > 0)
  m.nAPAprPric = m.nAPAprPric * -1
ENDIF
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
*B606283,1 ALB The Approved Amount in invoice line should be <= the inv. amount [Begin]
*IF (m.nAPAprPric * m.nAPTAprQty) > m.nAPAmnt
IF ABS((m.nAPAprPric * m.nAPTAprQty)) > ABS(m.nAPAmnt)
  =gfModalGen(.F.,'DIALOG',.F.,.F.,'The approved amount should be less than or equal to the invoice amount')
  m.nAPAprPric = nAPAprPric
  RETURN
ENDIF
*B606283,1 ALB The Approved Amount in invoice line should be <= the inv. amount [end]
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

m.nAPAprPric = IIF(lfChkAplTkt(''),m.nAPAprPric,nAPAprPric)

IF m.nAPAprPric <> nAPAprPric
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]  
  
  *B602830,1 Add these lines to make sure that the "Invoice Unit Cost" and
  *          the "Approved Unit Cost" have the same sign [Begin]
  
  *-- If the "Invoice Unit Cost" and the "Approved Unit Cost" have
  *-- different signs
  IF SIGN(m.nAPPrice) <> 0 .AND. SIGN(m.nAPAprPric) <> 0 .AND.;
     SIGN(m.nAPPrice) <> SIGN(m.nAPAprPric)
    
    *-- Message : 04187
    *-- "Invoice unit cost and approved unit cost must have the same sign."
    *-- Button : 00000 
    *-- "                          <    Ok    >                           "
    =gfModalGen('TRM04187B00000' , 'ALERT')
    
    m.nAPAprPric = nAPAprPric        && Restore the old value
    RETURN
  ENDIF    && End of IF SIGN(m.nAPPrice) <> 0 .AND. SIGN(m.nAPAprPric) ...
  *B602830,1 Add these lines to make sure that the "Invoice Unit Cost" [End]
  
  m.nAPAprAmnt = m.nApTAprQty*m.nAPAprPric
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  *lnTAprAmt    = lnTAprAmt  + (ROUND(m.nApAprAmnt,2) - ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  lnTAprAmt    = lnTAprAmt  + (ROUND(m.nApAprAmnt,2) - ROUND(nApAprAmnt,2))
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

  =lfRefresh('APINVD10')
  =lfUpdAplTkt()
  REPLACE nAPAprPric WITH m.nAPAprPric ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
  SHOW GET m.nAPAprAmnt ENABLE
  SHOW WINDOW (lcInvDtBr) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvInvTQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice line total Quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvTQty()
*!*************************************************************
FUNCTION lfvInvTQty

IF m.nAPTotQty <> nAPTotQty
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  m.nApTAprQty = IIF(llDefAuApr,m.nAPTotQty,m.nApTAprQty)
  m.nApTAprQty = IIF(lfChkAplTkt(''),m.nApTAprQty,nApTAprQty)
  m.nApAmnt    = m.nAPTotQty*m.nAPPrice
  m.nAPAprAmnt = m.nApTAprQty*m.nAPAprPric
  lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)

  =lfRefresh('APINVD10')
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  PRIVATE lnDiffAmnt
  lnDiffAmnt =  (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2)) * IIF((m.CIMTYP $ 'RF'),1,IIF(llDebitM,-1,1))
  *=lfUpdApDist(cApVILNo,ROUND(m.nAPAMnt,2)-ROUND(nAPAMnt,2),cApDGlAct,m.cIMTyp)
  =lfUpdApDist(cApVILNo,lnDiffAmnt,cApDGlAct,m.cIMTyp)
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

  =IIF(llDefAuApr,lfUpdAplTkt(),.T.)

  REPLACE nAPTotQty  WITH m.nAPTotQty  ,;
          nApTAprQty WITH m.nApTAprQty ,;
          nApAmnt    WITH m.nApAmnt    ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
  SHOW GET m.nApAmnt ENABLE
  IF llDefEdApr
    
    *B602830,1 Change this line because we are going to switch between 2 Get
    *          Fields for the "Approve Quantity" to be able to add the
    *          decimal points to the "Approve Quantity" in the case of
    *          document types Material PO, Material PO Return and
    *          Material MFG Order [Begin]
    *SHOW GET m.nApTAprQty ENABLE
    SHOW OBJECT lnApQtyObj ENABLE
    *B602830,1 Change this line because we are going to switch between 2 [End]
    
    SHOW GET m.nAPAprAmnt ENABLE
  ELSE
    
    *B602830,1 Change this line because we are going to switch between 2 Get
    *          Fields for the "Approve Quantity" to be able to add the
    *          decimal points to the "Approve Quantity" in the case of
    *          document types Material PO, Material PO Return and
    *          Material MFG Order [Begin]
    *SHOW GET m.nApTAprQty DISABLE
    SHOW OBJECT lnApQtyObj DISABLE
    *B602830,1 Change this line because we are going to switch between 2 [End]
    
    SHOW GET m.nAPAprAmnt DISABLE
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvAprTQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice line total Approved Quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAprTQty()
*!*************************************************************
FUNCTION lfvAprTQty

*B606283,1 ALB The Approved Amount in invoice line should be <= the inv. amount [Begin]
IF (m.nAPAprPric * m.nAPTAprQty) > m.nAPAmnt
  =gfModalGen(.F.,'DIALOG',.F.,.F.,'The approved amount should be less than or equal invoice amount')
  m.nApTAprQty = nApTAprQty
  RETURN
ENDIF
*B606283,1 ALB The Approved Amount in invoice line should be <= the inv. amount [end]

m.nApTAprQty = IIF(lfChkAplTkt(''),m.nApTAprQty,nApTAprQty)
IF m.nApTAprQty <> nApTAprQty
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  m.nAPAprAmnt = m.nApTAprQty*m.nAPAprPric
  lnTAprAmt = lnTAprAmt +(ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  =lfRefresh('APINVD10')
  =lfUpdAplTkt()

  REPLACE nApTAprQty WITH m.nApTAprQty ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
  SHOW GET m.nAPAprAmnt ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvInvAmnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice line total Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvAmnt()
*!*************************************************************
FUNCTION lfvInvAmnt

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
IF (m.CIMTYP $ 'RF' OR llDebitM) AND (m.nApAmnt > 0)
  m.nApAmnt = m.nApAmnt * -1
ENDIF
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

IF m.nApAmnt <> nApAmnt
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  *B602830,1 Add these lines to make sure that the "Invoice Amount" and
  *          the "Approved Amount" have the same sign [Begin]
  
  *-- If the "Invoice Amount" and the "Approved Amount" have
  *-- different signs
  IF SIGN(m.nApAmnt) <> 0 .AND. SIGN(m.nAPAprAmnt) <> 0 .AND.;
     SIGN(m.nApAmnt) <> SIGN(m.nAPAprAmnt)
    
    *-- Message : 04186
    *-- "Invoice amount and approved amount must have the same sign."
    *-- Button : 00000 
    *-- "                          <    Ok    >                     "
    =gfModalGen('TRM04186B00000' , 'ALERT')
    
    m.nApAmnt = nApAmnt        && Restore the old value
    RETURN
  ENDIF    && End of IF SIGN(m.nApAmnt) <> 0 .AND. SIGN(m.nAPAprAmnt) ...
  *B602830,1 Add these lines to make sure that the "Invoice Amount" [End]
  
  m.nAPTotQty  = IIF(m.nAPTotQty=0,1,m.nAPTotQty)
  *MAN Added the following line
  m.nApAmnt = m.nApAmnt * 1.000
  m.nAPPrice   = m.nApAmnt/m.nAPTotQty
  
  *B602830,1 Change these lines to make sure that the "Approved Amount" could
  *          not be less than the "Contributed Amount" [Begin]
  *m.nAPAprAmnt = IIF(llDefAuApr,m.nApAmnt,m.nAPAprAmnt)
  *m.nApTAprQty = IIF(llDefAuApr AND m.nApTAprQty=0,1,m.nApTAprQty)
  *m.nAPAprPric = IIF(m.nApTAprQty=0,0,m.nAPAprAmnt/m.nApTAprQty)
  
  *-- If Automatic Approve
  IF llDefAuApr
    
    *-- If there is no "Approved Amount" (there is no "Contributed Amount")
    IF m.nAPAprAmnt = 0
      *-- Default the "Approved Amount" with the "Invoice Amount"
      m.nAPAprAmnt = m.nApAmnt
      *-- Default the "Approved Quantity" with 1 if it's empty otherwise
      *-- live it as it is
      m.nApTAprQty = IIF(m.nApTAprQty = 0 , 1 , m.nApTAprQty)
      *-- Calculate the "Approved Unit Cost"
      m.nAPAprPric = m.nAPAprAmnt / m.nApTAprQty
    ELSE    && Else (If there is an "Approved Amount")
      *-- Calculate the "Approved Unit Cost" using the "Invoice Amount"
      m.nAPAprPric = m.nApAmnt / m.nApTAprQty
      
      *-- If the new "Approved Unit Cost" - that was calculated using the
      *-- "Invoice Amount" - produced an "Approved Amount" greater than
      *-- the "Contributed Amount"
      IF !lfChkAplTkt('')
        *-- Restore the old "Approved Unit Cost"
        m.nAPAprPric = nAPAprPric
      ELSE    && Else (If the new "Approved Amount" is less than the "Contributed Amount")
      *-- Default the "Approved Amount" with the "Invoice Amount"
        m.nAPAprAmnt = m.nApAmnt
      ENDIF    && End of IF !lfChkAplTkt('')
    ENDIF    && End of IF m.nAPAprAmnt = 0
  ENDIF    && End of IF llDefAuApr
  *B602830,1 Change these lines to make sure that the "Approved Amount" [End]
  
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  *lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2)- ROUND(nAPAMnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  *lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2)- ROUND(nAPAMnt,2))
  lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  =lfRefresh('APINVD10')
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  PRIVATE lnDiffAmnt
  lnDiffAmnt =  (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2)) * IIF((m.CIMTYP $ 'RF'),1,IIF(llDebitM,-1,1))
  *=lfUpdApDist(cApVILNo,ROUND(m.nAPAMnt,2)-ROUND(nAPAMnt,2),cApDGlAct,m.cIMTyp)
  =lfUpdApDist(cApVILNo,lnDiffAmnt,cApDGlAct,m.cIMTyp)
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

  =IIF(llDefAuApr,lfUpdAplTkt(),.T.)
  REPLACE nAPPrice   WITH m.nAPPrice   ,;
          nAPTotQty  WITH m.nAPTotQty  ,;
          nApAmnt    WITH m.nApAmnt    ,;
          nAPAprPric WITH m.nAPAprPric ,;
          nApTAprQty WITH m.nApTAprQty ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
  SHOW GET m.nAPPrice  ENABLE
  
  *B602830,1 Change this line because we are going to switch between 2 Get
  *          Fields for the "Invoice Quantity" to be able to add the
  *          decimal points to the "Invoice Quantity" in the case of
  *          document types Material PO, Material PO Return and
  *          Material MFG Order [Begin]
  *SHOW GET m.nAPTotQty &lcInvQTot
  SHOW OBJECT lnQtyObj &lcInvQTot
  *B602830,1 Change this line because we are going to switch between 2 [End]
  
  IF llDefEdApr
    SHOW GET m.nAPAprPric ENABLE
    
    *B602830,1 Change this line because we are going to switch between 2 Get
    *          Fields for the "Approve Quantity" to be able to add the
    *          decimal points to the "Approve Quantity" in the case of
    *          document types Material PO, Material PO Return and
    *          Material MFG Order [Begin]
    *SHOW GEt m.nApTAprQty &lcAprQty
    SHOW OBJECT lnApQtyObj &lcAprQty
    *B602830,1 Change this line because we are going to switch between 2 [End]
    
    SHOW GET m.nAPAprAmnt ENABLE
  ELSE
    SHOW GET m.nAPAprPric DISABLE
    
    *B602830,1 Change this line because we are going to switch between 2 Get
    *          Fields for the "Approve Quantity" to be able to add the
    *          decimal points to the "Approve Quantity" in the case of
    *          document types Material PO, Material PO Return and
    *          Material MFG Order [Begin]
    *SHOW GEt m.nApTAprQty DISABLE
    SHOW OBJECT lnApQtyObj DISABLE
    *B602830,1 Change this line because we are going to switch between 2 [End]
    
    SHOW GET m.nAPAprAmnt DISABLE
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvAprAmnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice line total Approved Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAprAmnt()
*!*************************************************************
FUNCTION lfvAprAmnt

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
IF (m.CIMTYP $ 'RF'OR llDebitM) AND (m.nAPAprAmnt > 0)
  m.nAPAprAmnt = m.nAPAprAmnt * -1
ENDIF
*B606283,1 ALB The Approved Amount in invoice line should be <= the inv. amount [Begin]
*IF m.nAPAprAmnt > m.nAPAmnt
IF ABS(m.nAPAprAmnt) > ABS(m.nAPAmnt)
  =gfModalGen(.F.,'DIALOG',.F.,.F.,'The approved amount should be less than or equal invoice amount')
  m.nAPAprAmnt = nAPAprAmnt
  RETURN
ENDIF
*B606283,1 ALB The Approved Amount in invoice line should be <= the inv. amount [end]
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

*MAN
m.nAPAprAmnt = IIF(lfChkAplTkt(''),m.nAPAprAmnt,nAPAprAmnt)

IF m.nAPAprAmnt <> nAPAprAmnt
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  
  *B602830,1 Add these lines to make sure that the "Invoice Amount" and
  *          the "Approved Amount" have the same sign [Begin]
  
  *-- If the "Invoice Amount" and the "Approved Amount" have
  *-- different signs
  IF SIGN(m.nApAmnt) <> 0 .AND. SIGN(m.nAPAprAmnt) <> 0 .AND.;
     SIGN(m.nApAmnt) <> SIGN(m.nAPAprAmnt)
    
    *-- Message : 04186
    *-- "Invoice amount and approved amount must have the same sign."
    *-- Button : 00000 
    *-- "                          <    Ok    >                     "
    =gfModalGen('TRM04186B00000' , 'ALERT')
    
    m.nAPAprAmnt = nAPAprAmnt        && Restore the old value
    RETURN
  ENDIF    && End of IF SIGN(m.nApAmnt) <> 0 .AND. SIGN(m.nAPAprAmnt) ...
  *B602830,1 Add these lines to make sure that the "Invoice Amount" [End]
  
  *MAN Added the following line
  m.nAPAprAmnt = m.nAPAprAmnt * 1.000
  m.nApTAprQty = IIF(m.nApTAprQty=0,1,m.nApTAprQty)
  m.nAPAprPric = m.nAPAprAmnt/m.nApTAprQty
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  *lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
  lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  =lfRefresh('APINVD10')
  =lfUpdAplTkt()

  REPLACE nAPAprPric WITH m.nAPAprPric ,;
          nApTAprQty WITH m.nApTAprQty ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)        
  SHOW GET m.nAPAprPric ENABLE
  
  *B602830,1 Change this line because we are going to switch between 2 Get
  *          Fields for the "Approve Quantity" to be able to add the
  *          decimal points to the "Approve Quantity" in the case of
  *          document types Material PO, Material PO Return and
  *          Material MFG Order [Begin]
  *SHOW GEt m.nApTAprQty &lcAprQty
  SHOW OBJECT lnApQtyObj &lcAprQty
  *B602830,1 Change this line because we are going to switch between 2 [End]
  
ENDIF

*!*************************************************************
*! Name      : lfvStyle
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate styles
*!*************************************************************
*! Calls     : gfStyBrw,lfItemQty,lfRefresh,lfShowWind
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

IF !llBrowse AND lcStyle=m.Item
  RETURN
ENDIF
IF lfIsContrib()
  lcStyle=Item
  llBrowse = .F.
  RETURN
ENDIF

*B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
llApdistUp = .T.
*B128880,1 EIH 03/14/2006 [End]

IF EMPTY(lcInvTktNo)
  IF llBrowse .OR. (!EMPTY(lcStyle) AND !SEEK(lcStyle,'STYLE'))
     lcStyle = PADR(gfStyBrw("I" , "" , "" , .F.),19)
     lcStyle = IIF(EMPTY(lcStyle),Item,lcStyle)
  ENDIF
  llBrowse = .F.
  IF lcStyle = m.Item
    RETURN
  ENDIF
  IF !EMPTY(lcStyle)
    IF m.cIMTyp='M' AND !Style.Make
      *E300798,1 Message : 04173
      *E300798,1 Only Manufacturing Styles are allowed
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04173B00000","DIALOG",'Manufacturing Styles')
      lcStyle = Item
      RETURN
    ENDIF
    IF INLIST(m.cIMTyp,'I','S','R') AND Style.Make
      *E300798,1 Message : 04173
      *E300798,1 Only Importing Styles are allowed
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04173B00000","DIALOG",'Importing Styles')
      lcStyle = Item
      RETURN
    ENDIF
  ENDIF
ELSE
  lcStyle = IIF(llBrowse,'?',lcStyle)
  llBrowse = .F.
  IF !lfItemQty(m.cIMTyp,lcInvTktNo,m.cLotNo,m.cBomType,m.cOprCode,;
               @lcStyle,SPACE(6),@lcStyDye)
    lcStyle = Item
    RETURN
  ENDIF
ENDIF
=SEEK(lcStyle,'Style')
lcStyDesc = Style.Desc1
IF EMPTY(lcStyle) OR Style.cDye_Flg <> 'Y'
  STORE SPACE(10) TO lcStyDye
ENDIF
SHOW GET lcStyDesc

*B607278,1 ALB Get the correct WIP account with shipment ticket [Begin]
IF !EMPTY(lcStyle) AND m.cIMTyp = 'S' 
  lcLinkCode = IIF(EMPTY(lcInvTktNo),SPACE(6),SUBSTR(lfGetGlLnk(lcInvTktNo,lcStyle,''),1,6))
  IF !SEEK(lcLinkCode+'013','GL_LINK')
    =SEEK('DEFDEF'+'013','GL_LINK')
  ENDIF
  STORE GL_Link.GlAcnt TO m.cApDGlAct
  SELECT (lcAPvInvDt)
  IF cApDGlAct <> m.cApDGlAct
    =lfUpdApDist(cApVILNo,0,m.cApDGlAct,m.cIMTYP)
    REPLACE cApDGlAct WITH m.cApDGlAct ,;
            cStatus   WITH IIF(cStatus='S','M',cStatus)
  ENDIF
ENDIF
*B607278,1 ALB Get the correct WIP account with shipment ticket [end]

=lfRefresh('APINVD10')
SELECT (lcAPvInvDt)
REPLACE Item     WITH lcStyle   ,;
        cDyelot  WITH lcStyDye  ,; 
        cAPVIDes WITH lcStyDesc ,;
        cStatus  WITH IIF(cStatus='S','M',cStatus)


    *        cBomType   WITH laAplCst[lnAplCst,2]  ,;
    *        cCostType  WITH laAplCst[lnAplCst,3]  ,;
    *        cOprCode   WITH lcMfgCode   
   
*?Amin
*B605040,1 update MFgOper with proper value   [Start]      
IF &lcAPvInvDt..cOprCode = REPLICATE('*',6)
  REPLACE cOprCode WITH IIF(!STYLE.lDetCost,PADR('*'+&lcAPvInvDt..cBomType,6),&lcAPvInvDt..cOprCode)
ENDIF
IF &lcAPvInvDt..cOprCode  =REPLICATE('#',6)
  REPLACE cOprCode WITH IIF(!STYLE.lDetCost,PADR('*'+&lcAPvInvDt..cBomType,6),&lcAPvInvDt..cOprCode)
ENDIF      
*B605040,1 update MFgOper with proper value   [End]      
        
*?Amin we should update the cOprCode with the proper value here        
*B605040,1   [Start]
*IF !STYLE.lDetCost
*   XXX= PADR('*'+laAplCst[lnAplCst,2],6)
*ELSE
*  IF laAplCst[lnAplCst,3]  = "P"
*    XXX= PADR('*',6)
*  ENDIF
*  IF laAplCst[lnAplCst,3]  = "D"
*    XXX= PADR('#',6)  
*  ENDIF
*ENDIF
*B605040,1   [End]



*B603827,1 KHM 08/27/2000 (Begin) The following code is commented out due 
*B603827,1                to the changes that we are not going to update 
*B603827,1                the actual cost for the PosHdr in case of 
*B603827,1                invoicing by shipment.

*B603390,1 KHM 07/20/2000 (Begin) Check if by shipment and the cTktNo is
*B603390,1                empty then get Po No. from the PosLn. Because
*B603390,1                when saving the invoice the cTktNo in the temporary 
*B603390,1                file is empty in case of shipment and it displays 
*B603390,1                a message that the record is locked by another user
*IF cIMTyp = 'S' AND EMPTY(cTktNo)
 
*SELECT PosLn
*lcOldOrd = SET('ORDER')
*SET ORDER TO PosHRec
*lcSeekExpr = &lcAPvInvDt..ShipNo
* lcForExpr = ".T."
* lcForExpr = lcForExpr+IIF(!EMPTY(lcStyle),;
*       " AND Style=lcStyle AND STR(LineNo,6)=STR(&lcAPvInvDt..LineNo, 6)","")
* IF SEEK(lcSeekExpr)
*   LOCATE REST WHILE EVALUATE(SYS(14,VAL(SYS(21)))) = lcSeekExpr;
*                FOR EVALUATE(lcForExpr)
*   IF FOUND() 
*     SELECT (lcAPvInvDt)
*     REPLACE cTktNo WITH PosLn.PO
*     m.cTktNo = cTktNo
*   ENDIF  
* ENDIF
* SET ORDER TO &lcOldOrd IN PosLn
* SELECT (lcAPvInvDt)
*ENDIF
*B603390,1 KHM 07/20/2000 (End)

*B603827,1 KHM 08/27/2000 (End)
=lfShowWind()

*!*************************************************************
*! Name      : lfvStyDye
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate style Dyelots
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStyDye()
*!*************************************************************
FUNCTION lfvStyDye

IF !llBrowse AND lcStyDye=cDyelot
  RETURN
ENDIF
IF lfIsContrib()
  lcStyDye=cDyelot
  llBrowse = .F.
  RETURN
ENDIF

*B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
llApdistUp = .T.
*B128880,1 EIH 03/14/2006 [End]

IF EMPTY(lcInvTktNo)
  SELECT STYDYE
  =SEEK(lcStyle)
  LOCATE REST WHILE Style+cWarecode+Dyelot = lcStyle FOR Dyelot = lcStyDye
  IF llBrowse OR !FOUND()
    =SDYEBROW(lcStyle,@lcStyDye,.T.))
  ENDIF
  llBrowse = .F.
ELSE
  lcStyDye = IIF(llBrowse,'?',lcStyDye)
  llBrowse = .F.
  IF !lfItemQty(m.cIMTyp,lcInvTktNo,m.cLotNo,m.cBomType,m.cOprCode,;
               @lcStyle,SPACE(6),@lcStyDye)
    lcStyDye = cDyelot
    RETURN
  ENDIF
ENDIF
=SEEK(lcStyle,'Style')
lcStyDesc = Style.Desc1
SHOW GET lcStyDesc
=lfRefresh('APINVD10')
SELECT (lcAPvInvDt)
REPLACE Item     WITH lcStyle   ,;
        cDyelot  WITH lcStyDye  ,; 
        cAPVIDes WITH lcStyDesc ,;
        cStatus  WITH IIF(cStatus='S','M',cStatus)
=lfShowWind()

*!*************************************************************
*! Name      : lfvStyDesc
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate style description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStyDesc()
*!*************************************************************
FUNCTION lfvStyDesc
IF cApVIDes <> lcStyDesc
  REPLACE cApVIDes WITH lcStyDesc ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
ENDIF

*!*************************************************************
*! Name      : lfvFabric
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Fabrics
*!*************************************************************
*! Calls     : FABROW
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
*B607353,1 ALB Fix bugs in Matirial PO when useing mouse to browse the matiril[Begin]
lcFabric = IIF(EMPTY(lcFabric) AND llBrowse,'?      ',lcFabric)
*B607353,1 ALB Fix bugs in Matirial PO when useing mouse to browse the matiril[End]

IF !llBrowse AND lcFabric=SUBSTR(m.Item,1,7)
  *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
  IF EMPTY(lcFabric)
    SHOW GET ibFabClr DISABLE
    SHOW GET lcFabClr DISABLE
    SHOW GET ibFabDye DISABLE
    SHOW GET lcFabDye DISABLE
  ENDIF
  *B605911,1 ALB Fix the contribution process  to save the invoice [End]
  RETURN
ENDIF
IF lfIsContrib()
  lcFabric = SUBSTR(Item,1,7)
  llBrowse = .F.
  RETURN
ENDIF

*B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
llApdistUp = .T.
*B128880,1 EIH 03/14/2006 [End]


IF EMPTY(lcInvTktNo)
  IF llBrowse OR (!EMPTY(lcFabric) AND !SEEK(lcFabric+ALLTRIM(lcFabClr),'FABRIC'))
    lcFabric = IIF(FABROW(@lcFabric,'*',.T.),lcFabric,SUBSTR(Item,1,7))
  ENDIF
ELSE
  lcFabClr = IIF(llBrowse,'?',lcFabClr)
  IF !lfItemQty(m.cIMTyp,lcInvTktNo,m.cLotNo,m.cBomType,m.cOprCode,@lcFabric,;
                @lcFabClr,@lcFabDye)
    lcFabric = SUBSTR(Item,1,7)
    lcFabClr = Color
  ENDIF
ENDIF

IF EMPTY(lcFabric)
  SHOW GET ibFabClr DISABLE
  SHOW GET lcFabClr DISABLE
  SHOW GET ibFabDye DISABLE
  SHOW GET lcFabDye DISABLE
ELSE
  *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
  *SHOW GET lcFabric DISABLE
  *SHOW GET ibFabric DISABLE
  *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
  SHOW GET ibFabClr ENABLED
  SHOW GET lcFabClr ENABLED
ENDIF
SELECT (lcAPvInvDt)
llBrowse = .F.

*B607353,1 ALB Fix bugs in Matirial PO when useing mouse to browse the matiril[Begin]
lcFabric = SUBSTR(lcFabric,1,7)
llseek=SEEK(lcFabric+lcFabClr,'FABRIC')
lcFabDye  = IIF(Fabric.cDye_Flg='Y',lcFabDye,SPACE(10))
lcFabDesc = Fabric.Desc
SHOW GET lcFabDesc 
SHOW GET lcFabric
REPLACE Item     WITH lcFabric  ,;
        Color    WITH lcFabClr  ,;
        cDyelot  WITH lcFabDye  ,;
        cAPVIDes WITH lcFabDesc ,;
        cStatus  WITH IIF(cStatus='S','M',cStatus)        
=lfRefresh('APINVD10')
*B607353,1 ALB Fix bugs in Matirial PO when useing mouse to browse the matiril[end]
*!*************************************************************************************
*! Name      : lfvFabClr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Fabric colors
*!**************************************************************************************
*! Calls     : FABROW(),lfItemQty(),lfShowWind()
*!**************************************************************************************
*! Passed Parameters  :  None
*!**************************************************************************************
*! Returns            :  None
*!**************************************************************************************
*! Example            :  =lfvFabClr()
*!**************************************************************************************
FUNCTION lfvFabClr
IF lfIsContrib()
  lcFabClr = m.Color
  llBrowse = .F.
  RETURN
ENDIF
IF EMPTY(lcInvTktNo)
  IF llBrowse .OR. !SEEK(lcFabric+lcFabClr,'FABRIC')
    lcFabClr = CHR(240)
    lcFabric=IIF(FABROW(lcFabric,@lcFabClr,.T.),lcFabric,m.Color)
  ENDIF
  SELECT (lcAPvInvDt)
  llBrowse = .F.
  IF PADR(lcFabric,19)+lcFabClr = m.Item+m.Color
    RETURN
  ENDIF
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  IF !EMPTY(lcFabric)
    IF INLIST(m.cIMTyp,'L','F') AND Fabric.Make
      *E300798,1 Message : 04173
      *E300798,1 Only Importing Materials are allowed
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04173B00000","DIALOG",'Importing Materials')
      lcFabric = SUBSTR(m.Item,1,7)
      lcFabClr = m.Color
      RETURN
    ENDIF
    IF m.cIMTyp = 'T' AND !Fabric.Make
      *E300798,1 Message : 04173
      *E300798,1 Only Manufacturing Materials are allowed
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen("TRM04173B00000","DIALOG",'Manufacturing Materials')
      lcFabric = SUBSTR(m.Item,1,7)
      lcFabClr = m.Color
      RETURN
    ENDIF
  ENDIF
ELSE
  lcFabClr = IIF(llBrowse,'?',lcFabClr)
  *--alb llBrowse = .F.
  IF !lfItemQty(m.cIMTyp,lcInvTktNo,m.cLotNo,m.cBomType,m.cOprCode,@lcFabric,;
                @lcFabClr,@lcFabDye)
    lcFabric = SUBSTR(Item,1,7)
    lcFabClr = Color
  ENDIF
ENDIF
lcFabric = SUBSTR(lcFabric,1,7)
=SEEK(lcFabric+lcFabClr,'FABRIC')
lcFabDye  = IIF(Fabric.cDye_Flg='Y',lcFabDye,SPACE(10))
lcFabDesc = Fabric.Desc
SHOW GET lcFabDesc 
SHOW GET lcFabric
REPLACE Item     WITH lcFabric  ,;
        Color    WITH lcFabClr  ,;
        cDyelot  WITH lcFabDye  ,;
        cAPVIDes WITH lcFabDesc ,;
        cStatus  WITH IIF(cStatus='S','M',cStatus)        
=lfRefresh('APINVD10')

*B607353,1 ALB Fix bugs in Matirial PO when useing mouse to browse the matiril[Begin]
*=lfShowWind()
*B607353,1 ALB Fix bugs in Matirial PO when useing mouse to browse the matiril[end]

*!***************************************************************************************
*! Name      : lfvFabDye
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Fabric/Color/Dyelots
*!****************************************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFabDye()
*!*************************************************************
FUNCTION lfvFabDye

IF !llBrowse AND lcFabDye = cDyelot
  RETURN
ENDIF
IF lfIsContrib()
  lcFabDye = cDyelot
  llBrowse = .F.
  RETURN
ENDIF
IF EMPTY(lcInvTktNo)
  SELECT FABDYE
  =SEEK(lcFabric+lcFabClr)
  LOCATE REST WHILE fabric+color+cwarecode+dyelot = lcFabric+lcFabClr FOR Dyelot = lcFabDye
  IF llBrowse OR !FOUND()
    =FDYEBROW(lcFabric,lcFabClr,@lcFabDye,.T.)
  ENDIF
  llBrowse = .F.
ELSE
  lcFabDye = IIF(llBrowse,'?',lcFabDye)
  llBrowse = .F.
  IF !lfItemQty(m.cIMTyp,lcInvTktNo,m.cLotNo,m.cBomType,m.cOprCode,@lcFabric,;
                @lcFabClr,@lcFabDye)
    lcFabDye = cDyelot
    RETURN
  ENDIF
ENDIF
lcFabric = SUBSTR(lcFabric,1,7)
=SEEK(lcFabric+lcFabClr,'FABRIC')
lcFabDesc = Fabric.Desc

*B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
llApdistUp = .T.
*B128880,1 EIH 03/14/2006 [End]

SHOW GET lcFabDesc 
SHOW GET lcFabric
SELECT (lcAPvInvDt)
REPLACE Item     WITH lcFabric  ,;
        Color    WITH lcFabClr  ,;
        cDyelot  WITH lcFabDye  ,;
        cAPVIDes WITH lcFabDesc ,;
        cStatus  WITH IIF(cStatus='S','M',cStatus)        
=lfRefresh('APINVD10')
=lfShowWind()

*!*************************************************************
*! Name      : lfvFabDesc
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Fabric/Color Description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFabDesc()
*!*************************************************************
FUNCTION lfvFabDesc
IF cAPVIDes <> lcFabDesc
  REPLACE cAPVIDes WITH lcFabDesc ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
ENDIF
=lfShowWind()

*!*************************************************************
*! Name      : lfvItem
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Free Formate Invoice item
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvItem()
*!*************************************************************
FUNCTION lfvItem

IF Item <> m.Item 
  REPLACE Item WITH m.Item ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
ENDIF
*!*************************************************************
*! Name      : lfvItemDesc
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Free Formate Invoice item Description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvItemDesc()
*!*************************************************************
FUNCTION lfvItemDesc
IF cAPVIDes <> m.cAPVIDes 
  REPLACE cAPVIDes WITH m.cAPVIDes ,;
          cStatus  WITH IIF(cStatus='S','M',cStatus)
ENDIF
*!*************************************************************
*! Name      : lfvLTaxCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice Line Tax Code
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLTaxCode()
*!*************************************************************
FUNCTION lfvLTaxCode

=SEEK(cVendCode+cApInvNo+cAPVILNo,lcTmpDist)
IF &lcTmpDist..CTAXCODE <> laTaxCode[lnTaxCode,2]
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  =gfRltFld(laTaxCode[lnTaxCode,2],@laTaxAry,'CTAXCODE')
  lcApdGLAct = IIF(!EMPTY(STRTRAN(STRTRAN(lcApdGlAct,'-'),'0')),;
                     SUBSTR(lcApdGlAct,1,lnApsAcLen), lcEmptyAcc)

  *E300798,1 Message : 04091
  *E300798,1 G/L Input Account xxxx for tax xxxx is not found in the chart of
  *E300798,1 account. You cannot select this tax code
  *E300798,1 Button : 00000 
  *E300798,1 Ok
  IF llApGlLink .AND. !SEEK(lcApDGlAct, 'lcLinkChar')
    =gfModalGen("TRM04091B00000","DIALOG",ALLTRIM(lcApDGlAct)+'|'+ALLTRIM(laTaxCode[lnTaxCode,1]))
    =gfwCodePop(@laCodes,'CTAXCODE','T')
    RETURN
  ENDIF
  m.cApDGlAct = lcApdGLAct
  SELECT (lcAPvInvDt)
  IF cApDGlAct <> m.cApDGlAct
    REPLACE cApDGlAct WITH m.cApDGlAct ,;
            cStatus   WITH IIF(cStatus='S','M',cStatus)
  ENDIF
  =lfUpdApDist(cApVILNo,0,cApDGlAct,m.cIMTyp)
  *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [Begin]
  *IF EMPTY(laTaxCode[lnTaxCode,2])
  *  SHOW GET ibWIPAcnt   ENABLE
  *  SHOW GET m.cApDGlAct ENABLE
  *ELSE
  *  SHOW GET ibWIPAcnt   DISABLE
  *  SHOW GET m.cApDGlAct DISABLE
  *ENDIF
ENDIF  

SELECT (lcAPvInvDt)
REPLACE CTaxCode  WITH ALLTRIM(laTaxCode[lnTaxCode,2])
IF EMPTY(laTaxCode[lnTaxCode,2])
  IF laSetups[3,2]='Y'
    STORE lcDistAcct TO m.cApDGlAct
    IF cApDGlAct <> m.cApDGlAct
      REPLACE cApDGlAct WITH m.cApDGlAct ,;
              cStatus   WITH IIF(cStatus='S','M',cStatus)
    ENDIF
  ENDIF
  SHOW GET ibWIPAcnt   ENABLE
  SHOW GET m.cApDGlAct ENABLE
ELSE
  SHOW GET ibWIPAcnt   DISABLE
  SHOW GET m.cApDGlAct DISABLE
ENDIF
*B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [End]

*!*************************************************************
*! Name      : lfvInvQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice Size Quantity 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lnSize : Style Size Number
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvQty()
*!*************************************************************
FUNCTION lfvInvQty
PARAMETERS lnSize
PRIVATE lcSize

lcSize = STR(lnSize,1)
IF m.nAPQty&lcSize <> nAPQty&lcSize
  
  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  m.nAPAprQty&lcSize = IIF(llDefAuApr,m.nAPQty&lcSize,m.nAPAprQty&lcSize)
  m.nAPAprQty&lcSize = IIF(lfChkAplTkt(lcSize),m.nAPAprQty&lcSize,nAPAprQty&lcSize)
  m.nApTAprQty= m.nAPAprQty1+m.nAPAprQty2+m.nAPAprQty3+m.nAPAprQty4+;
                m.nAPAprQty5+m.nAPAprQty6+m.nAPAprQty7+m.nAPAprQty8
  m.nApTotQty = m.nAPQty1+m.nAPQty2+m.nAPQty3+m.nAPQty4+;
                m.nAPQty5+m.nAPQty6+m.nAPQty7+m.nAPQty8

  m.nApAmnt   = m.nAPTotQty*m.nAPPrice
  m.nAPAprAmnt= m.nApTAprQty*m.nAPAprPric
  lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2)- ROUND(nAPAMnt,2)) 
  lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2)) 
 
  =lfRefresh('APINVD10')
  
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  PRIVATE lnDiffAmnt
  lnDiffAmnt =  (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2)) * IIF((m.CIMTYP $ 'RF'),IIF(llDebitM,1,1),IIF(llDebitM,-1,1))
  *=lfUpdApDist(cApVILNo,ROUND(m.nAPAMnt,2)-ROUND(nAPAMnt,2),cApDGlAct,m.cIMTyp)
  =lfUpdApDist(cApVILNo,lnDiffAmnt,cApDGlAct,m.cIMTyp)
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

  =IIF(llDefAuApr,lfUpdAplTkt(),.T.)
  REPLACE nAPQty&lcSize    WITH m.nAPQty&lcSize  ,;
          nAPAprQty&lcSize WITH m.nAPAprQty&lcSize ,;
          nAPTotQty  WITH m.nAPTotQty  ,;
          nApTAprQty WITH m.nApTAprQty ,;
          nApAmnt    WITH m.nApAmnt    ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus   WITH IIF(cStatus='S','M',cStatus)
  
  *B602830,1 Change this line because we are going to switch between 2 Get
  *          Fields for the "Invoice Quantity" to be able to add the
  *          decimal points to the "Invoice Quantity" in the case of
  *          document types Material PO, Material PO Return and
  *          Material MFG Order [Begin]
  *SHOW GET m.nAPTotQty  DISABLE
  SHOW OBJECT lnQtyObj DISABLE
  *B602830,1 Change this line because we are going to switch between 2 [End]
  
  SHOW GET m.nApAmnt    ENABLE
  
  *B602830,1 Change this line because we are going to switch between 2 Get
  *          Fields for the "Approve Quantity" to be able to add the
  *          decimal points to the "Approve Quantity" in the case of
  *          document types Material PO, Material PO Return and
  *          Material MFG Order [Begin]
  *SHOW GET m.nApTAprQty DISABLE
  SHOW OBJECT lnApQtyObj DISABLE
  *B602830,1 Change this line because we are going to switch between 2 [End]
  
  IF llDefEdApr
    SHOW GET m.nAPAprQty&lcSize ENABLE
    SHOW GET m.nAPAprAmnt ENABLE
  ELSE
    SHOW GET m.nAPAprQty&lcSize DISABLE
    SHOW GET m.nAPAprAmnt DISABLE
  ENDIF
  SHOW WINDOW (lcInvDtBr) REFRESH SAME
ENDIF
*!*************************************************************
*! Name      : lfvAprQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice Approved Size Quantity 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lnSize : Style Size Number
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAprQty()
*!*************************************************************
FUNCTION lfvAprQty
PARAMETERS lnSize
PRIVATE lcSize

lcSize = STR(lnSize,1)
m.nAPAprQty&lcSize = IIF(lfChkAplTkt(lcSize),m.nAPAprQty&lcSize,nAPAprQty&lcSize)
IF m.nAPAprQty&lcSize <> nAPAprQty&lcSize

  *B128880,1 EIH 03/14/2006 We can update in apdist file now [Begin]
  llApdistUp = .T.
  *B128880,1 EIH 03/14/2006 [End]
  
  
  m.nApTAprQty = m.nAPAprQty1+m.nAPAprQty2+m.nAPAprQty3+m.nAPAprQty4+;
                 m.nAPAprQty5+m.nAPAprQty6+m.nAPAprQty7+m.nAPAprQty8
  m.nAPAprAmnt = m.nApTAprQty*m.nAPAprPric
  lnTAprAmt = lnTAprAmt  + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)

  =lfRefresh('APINVD10')
  =lfUpdAplTkt()
  REPLACE nAPAprQty&lcSize WITH m.nAPAprQty&lcSize ,;
          nApTAprQty WITH m.nApTAprQty ,;
          nAPAprAmnt WITH m.nAPAprAmnt ,;
          cStatus   WITH IIF(cStatus='S','M',cStatus)
  
  *B602830,1 Change this line because we are going to switch between 2 Get
  *          Fields for the "Approve Quantity" to be able to add the
  *          decimal points to the "Approve Quantity" in the case of
  *          document types Material PO, Material PO Return and
  *          Material MFG Order [Begin]
  *SHOW GET m.nApTAprQty DISABLE
  SHOW OBJECT lnApQtyObj DISABLE
  *B602830,1 Change this line because we are going to switch between 2 [End]
  
  SHOW GET m.nAPAprAmnt ENABLE
  SHOW WINDOW (lcInvDtBr) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvWipAcnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate GL Account
*!*************************************************************
*! Calls     : lfApAcs
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWipAcnt()
*!*************************************************************
FUNCTION lfvWipAcnt

*B602274,1 Fix browse WIP account
*=lfApAcs(.F.,llBrowse)
lcOldAcnt = cApDGlAct
REPLACE cApDGlAct WITH m.cApDGlAct
=lfApAcs(.F.,llBrowse)
REPLACE cApDGlAct WITH lcOldAcnt
*B602274,1 (End)

IF EMPTY(STRTRAN(STRTRAN(m.cApDGlAct,'-'),'0'))
  m.cApDGlAct = cApDGlAct
ENDIF
llBrowse=.F.

SELECT (lcAPvInvDt)
IF cApDGlAct <> m.cApDGlAct
  =lfUpdApDist(cApVILNo,0,m.cApDGlAct,m.cIMTYP)
  REPLACE cApDGlAct WITH m.cApDGlAct ,;
          cStatus   WITH IIF(cStatus='S','M',cStatus)
ENDIF
*B605040,1 Update InvoiceTkt with GL account and Dist. line [Start]
=IIF(llDefAuApr,lfUpdAplTkt(),.T.)
*B605040,1 Update InvoiceTkt with GL account and Dist. line [End]

SELECT (lcAPvInvDt)
*!*************************************************************
*! Name      : lfvRInvLine
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Remove Invoice Line
*!*************************************************************
*! Calls     : gfModalGen,lfShowWind
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRInvLine()
*!*************************************************************
FUNCTION lfvRInvLine
IF gfModalGen("QRM00007B00007","ALERT") = 1
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  *lnTInvAmt = lnTInvAmt - ROUND(&lcapvinvdt..nApAmnt,2)*IIF(&lcAPvInvDt..CIMTYP $ 'RF',-1,1)
  *lnTAprAmt = lnTAprAmt - ROUND(&lcapvinvdt..nApAprAmnt,2)*IIF(&lcAPvInvDt..CIMTYP $ 'RF',-1,1)
  lnTInvAmt = lnTInvAmt - ROUND(&lcapvinvdt..nApAmnt,2)
  lnTAprAmt = lnTAprAmt - ROUND(&lcapvinvdt..nApAprAmnt,2)
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  IF SEEK(&lcAPvInvDt..cVendCode+&lcAPvInvDt..cApInvNo+&lcAPvInvDt..cAPVILNo,lcAPInvTkt)
  
    SELECT (lcAPInvTkt)
    SCAN REST WHILE cVendCode+cApInvNo+cAPVILNo = ;
         &lcAPvInvDt..cVendCode+&lcAPvInvDt..cApInvNo+&lcAPvInvDt..cAPVILNo
      *B606054,1 ALB change the status to delete [Begin]
      *REPLACE CSTATUS WITH IIF(CSTATUS='A','S','D')
      REPLACE CSTATUS WITH IIF(!EMPTY(nrecno),'D','S')
      *B606054,1 ALB change the status to delete [Begin]
      DELETE
    ENDSCAN
  ENDIF  
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  Private lnAmAmunt
  lnAmAmunt = ROUND(&lcAPvInvDt..nAPAMnt,2) * IIF(llDebitM ,1,-1) * IIF(&lcAPvInvDt..CIMTYP $ 'RF' AND llDebitM,-1,1)
  *=lfUpdApDist(&lcAPvInvDt..cApVILNo,-ROUND(&lcAPvInvDt..nAPAMnt,2),&lcAPvInvDt..cApDGlAct,&lcAPvInvDt..cIMTyp)
  =lfUpdApDist(&lcAPvInvDt..cApVILNo,lnAmAmunt,&lcAPvInvDt..cApDGlAct,&lcAPvInvDt..cIMTyp)
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  SELECT (lcAPvInvDt)
  REPLACE CSTATUS WITH IIF(CSTATUS='A','S','D')
  DELETE
  SKIP
  IF EOF()
    SHOW GET pbRemLine DISABLE
    GO TOP
  ENDIF
  =lfwInvDt()
  =lfRefresh('APINVD10')
ENDIF

*!*************************************************************
*! Name      : lfvAuto
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Automatically create AP invoice lines.
*!*************************************************************
*! Calls     : APAINVD.SPR,lfwInvDt(),lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAuto()
*!*************************************************************
FUNCTION lfvAuto
PRIVATE lcApDGlAct,lcAInvDtBr,laAplCst,laTickets,laAplLots

llFirst = .T.
lcAInvDtBr = 'Document Lines'
DECLARE laAplCst[1,3],laTickets[1,3],laAplLots[1]
STORE 0         TO lnAplLotNo,lnAMarker
STORE SPACE(24) TO lcApDGlAct
STORE SPACE(6)  TO lcTktNo
STORE 1         TO lnAplTkt,lnAplCst,lnAplOpr
STORE ''        TO laAplCst,lcMfgCode,lcRSession,lcBrowseTl,lcWIPAcnt
STORE 'N/A'     TO laOprLots[1]
STORE SPACE(2)  TO lcAplLotNo
laTickets[1,1] = 'Select Document Type'
laTickets[1,2] = ' '
laTickets[1,3] = 'Document#'
laAplCst[1,1]  = 'Select Cost Type'
laAplCst[1,2]  = ' '
laAplCst[1,3]  = ' '
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)
SET ORDER TO TAG BOMPOREC IN BOMLINE

laCodes[1,2] = "laMfgCode"
laCodes[1,3] = "lnAplOpr"
SELECT (lcAutoInv)
ZAP

*B603484,1 SSE 03/20/2000 Raise Flag to .T. while we are in this screen [Begin] 
llAutoScr = .T.
DO (gcScrDir + gcWinAppl + '\APAINVD.SPR')
llAutoScr = .F.
*B603484,1 SSE 03/20/2000 [End]

SELECT(lcAPvInvDt)
laCodes[1,2] = "laMfgCode"
laCodes[1,3] = "lnMfgOpr"
=lfwInvDt()
=lfRefresh('APINVD10')

*!*************************************************************
*! Name      : lfAInvBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse Ticket lines to invoice
*!*************************************************************
*! Calls     : lfwAInvDt
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAInvBrow()
*!*************************************************************
FUNCTION lfAInvBrow

SELECT (lcAutoInv)
lnAMarker = RECNO()
IF EMPTY(lcRSession)
  lcFields = "nTktQty :H='Bud. Qty.' :R,nTktCst :H='Bud. Cost' :R,nTktAmnt :H='Bud. Amnt.' :R,"
ELSE
  *MAN Added Round 2 to Rec. Amnt. 
  *lcFields = "x=IIF(cRecvType='R','Receive',IIF(cRecvType='S','Sec Qlty.','Damaged')) :H='Type',nTktQty :H='Rec. Qty.' :R,nTktCst :H='Rec. Cost' :R,nTktAmnt :H='Rec. Amnt.' :R,"
  lcFields = "x=IIF(cRecvType='R','Receive',IIF(cRecvType='S','Sec Qlty.','Damaged')) :H='Type',nTktQty :H='Rec. Qty.' :R,nTktCst :H='Rec. Cost' :R,nTktAmnt=ROUND(nTktAmnt,2) :H='Rec. Amnt.' :R,"

  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  lcTktType = laTickets[lnAplTkt,2]
  IF lcTktType $ 'RF'
    lcFields = "x=IIF(cRecvType='R','Receive',IIF(cRecvType='S','Sec Qlty.','Damaged')) :H='Type',"+;
               "nTktQty :H='Issue Qty.' :R,nTktCst :H='Issue Cost':R,nTktAmnt=ROUND(nTktAmnt,2) :H='Issue Amnt.' :R,"
  ENDIF
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]  
ENDIF
*MAN Added Round 2 to Inv. Amnt. and Apl. Amnt.
lcFields = lcFields + "nInvQty :H='Inv. Qty.' :R,nInvCst=IIF(nInvQty=0,0,nInvAmnt/nInvQty) :H='Inv. Cost' :R :P='999999999.999',nInvAmnt=ROUND(nInvAmnt,2) :H='Inv. Amnt.' :R,"+;
           "nAplQty :H='Apl. Qty.' :R,nAplCst=IIF(nAplQty=0,0,nAplAmnt/nAplQty) :H='Apl. Cost' :R,nAplAmnt=ROUND(nAplAmnt,2) :H='Apl. Amnt.' :R"
DO CASE
  CASE EMPTY(laTickets[1,2])
    lcFields = "cMarker=IIF(RECNO()=lnAMarker,'>',' '):H=' ':R:1:W=.F.,CSELECT :H=' ' :R,Item :H='Item' :R,"+lcFields
  CASE INLIST(laTickets[lnAplTkt,2],'T','L','F')
    lcFields = "cMarker=IIF(RECNO()=lnAMarker,'>',' '):H=' ':R:1:W=.F.,CSELECT :H=' ' :R,cItem=SUBSTR(Item,1,7) :H='Fabric' :R,Color :H='Color' :R,"+IIF(laSetups[2,2]='Y',"cDyelot :H='Dyelot',",'')+lcFields

  *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
  *CASE INLIST(laTickets[lnAplTkt,2],'S','I','M','R')
  CASE INLIST(laTickets[lnAplTkt,2],'S','I','M','R','A','D')
  *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
    lcFields = "cMarker=IIF(RECNO()=lnAMarker,'>',' '):H=' ':R:1:W=.F.,CSELECT :H=' ' :R,"+;
    IIF(laTickets[lnAplTkt,2]='S',"cTktNo :H='PO#' :R,"+"cWIPAcct :H='WIP A/C' :R,","")+"Item :H=lcStyHdr :R,"+IIF(laSetups[1,2]='Y',"cDyelot :H='Dyelot',",'')+lcFields
    lcFields = lcFields+;
               ",nTktQty1 :H='Doc Qty1' :R,nTktQty2 :H='Doc Qty2' :R,nTktQty3 :H='Doc Qty3' :R,nTktQty4 :H='Doc Qty4' :R"+;
               ",nTktQty5 :H='Doc Qty5' :R,nTktQty6 :H='Doc Qty6' :R,nTktQty7 :H='Doc Qty7' :R,nTktQty8 :H='Doc Qty8' :R"+;
               ",nInvQty1 :H='Inv Qty1' :R,nInvQty2 :H='Inv Qty2' :R,nInvQty3 :H='Inv Qty3' :R,nInvQty4 :H='Inv Qty4' :R"+;
               ",nInvQty5 :H='Inv Qty5' :R,nInvQty6 :H='Inv Qty6' :R,nInvQty7 :H='Inv Qty7' :R,nInvQty8 :H='Inv Qty8' :R"+;
               ",nAplQty1 :H='Apl Qty1' :R,nAplQty2 :H='Apl Qty2' :R,nAplQty3 :H='Apl Qty3' :R,nAplQty4 :H='Apl Qty4' :R"+;
               ",nAplQty5 :H='Apl Qty5' :R,nAplQty6 :H='Apl Qty6' :R,nAplQty7 :H='Apl Qty7' :R,nAplQty8 :H='Apl Qty8' :R"
ENDCASE


BROWSE FIELDS &lcFields  ;
       WINDOW APAINVD1   ;
       IN WINDOW APAINVD ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwAInvDt()  ;
       TITLE lcAInvDtBr



*!*************************************************************
*! Name      : lfwAInvDt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh Ticket lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwAInvDt()
*!*************************************************************
FUNCTION lfwAInvDt

lnAMarker = RECNO()
SHOW WINDOW (lcAInvDtBr) REFRESH SAME
IF cSelect = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
ENDIF


*B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
* Shipment case
IF laTickets[lnAplTkt,2] = "S"  .AND. SEEK("P"+CtktNo,"POSHDR") .AND. POSHDR.Status = "S"
  REPLACE cSelect WITH IIF(cSelect <> SPACE(1) ,SPACE(1),cSelect)
  SHOW GET pbSelect  DISABLE
  SHOW GET pbSelAll  DISABLE
  SHOW GET pbSelNone DISABLE
  SHOW GET pbInvert  DISABLE
ELSE
  SHOW GET pbSelect  ENABLE
  SHOW GET pbSelAll  ENABLE
  SHOW GET pbSelNone ENABLE
  SHOW GET pbInvert  ENABLE
ENDIF
*B605967,4 SSH [End]


*!*************************************************************
*! Name      : lfDAInvDet
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate Ticket lines screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDAInvDet()
*!*************************************************************
FUNCTION lfDAInvDet

IF WONTOP()=lcAInvDtBr
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTabMove WITH 'APAINVD2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackMove WITH 'APAINVD0','pbClear'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfwATktType
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create Array with available Document Types
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwATktType()
*!*************************************************************
FUNCTION lfwATktType

IF llFirst
  llFirst = .F.
  RETURN(.F.)
ENDIF
IF laTktType[1,2]='G'
  DECLARE laTickets[ALEN(laTktType,1)-1,3]
  =ACOPY(laTktType,laTickets,4)
ELSE
  DECLARE laTickets[ALEN(laTktType,1),3]
  =ACOPY(laTktType,laTickets)
ENDIF
SHOW GET lnAplTkt

*!*************************************************************
*! Name      : lfvATktType
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Selected Document Types
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvATktType()
*!*************************************************************
FUNCTION lfvATktType

=lfRefresh('APAINVD0')
SHOW GET lnAplTkt DISABLE
SHOW GET ibTktNo  ENABLE
SHOW GET lcTktNo  ENABLE
*SHOW GET lnAplCst ENABLE

*!*************************************************************
*! Name      : lfvAutoTkt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Selected Document #
*!*************************************************************
*! Calls     : lfvTicket(),lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAutoTkt()
*!*************************************************************
FUNCTION lfvAutoTkt
PRIVATE lcLinkCode
STORE SPACE(6) TO lcLinkCode
=lfvTicket(laTickets[lnAplTkt,2],@lcTktNo,@lcLinkCode,laAplCst[lnAplCst,2])
IF laSetups[3,2]='Y'
  IF !SEEK(lcLinkCode+'013','GL_LINK')
    =SEEK('DEFDEF'+'013','GL_LINK')
  ENDIF
  STORE GL_Link.GlAcnt TO lcApDGlAct,lcWIPAcnt
ELSE
  STORE lcDistAcct TO lcApDGlAct,lcWIPAcnt
ENDIF

*B602466,1 Enable Generate button after selecting costing type
*IF EMPTY(lcTktNo)
*  STORE '' TO lcRSession,lcApDGlAct
*  SHOW GET ibRSession DISABLE
*  SHOW GET lcRSession DISABLE
*  SHOW GET ibWIPAcnt  DISABLE
*  SHOW GET lcApDGlAct DISABLE
*  SHOW GET pbGenerate DISABLE
*ELSE
*  SHOW GET ibRSession ENABLE
*  SHOW GET lcRSession ENABLE
*  SHOW GET ibWIPAcnt  ENABLE
*  SHOW GET lcApDGlAct ENABLE
*  SHOW GET pbGenerate ENABLE
*ENDIF
IF !EMPTY(lcTktNo)
  SHOW GET ibTktNo  DISABLE
  SHOW GET lcTktNo  DISABLE
  SHOW GET lnAplCst ENABLE
ENDIF
*B602466,1 (End)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvAWipAcnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate GL Account
*!*************************************************************
*! Calls     : lfApAcs
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAWipAcnt()
*!*************************************************************
FUNCTION lfvAWipAcnt
=lfApAcs(.F.,llBrowse)
IF EMPTY(STRTRAN(STRTRAN(lcApDGlAct,'-'),'0'))
  lcApDGlAct = lcWIPAcnt
ENDIF
llBrowse=.F.

*!*************************************************************
*! Name      : lfwAplCst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create Array with available cost types
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwAplCst()
*!*************************************************************
FUNCTION lfwAplCst

IF EMPTY(laAplCst[1,2])
  DO CASE
    CASE INLIST(laTickets[lnAplTkt,2],'I','S','M','R')
      DECLARE laAplCst[ALEN(IIF(laTickets[lnAplTkt,2]='M','laMCstType','laICstType'),1),3]
      =ACOPY(IIF(laTickets[lnAplTkt,2]='M','laMCstType','laICstType'),laAplCst)
    
    CASE INLIST(laTickets[lnAplTkt,2],'T','L','F')
      DECLARE laAplCst[ALEN(IIF(laTickets[lnAplTkt,2]='T','laTCstType','laLCstType'),1),3]
      =ACOPY(IIF(laTickets[lnAplTkt,2]='T','laTCstType','laLCstType'),laAplCst)

    CASE INLIST(laTickets[lnAplTkt,2],'A')		&&Alb*E301995
      DECLARE laAplCst[ALEN(IIF(laTickets[lnAplTkt,2]='A','laACstType','laICstType'),1),3]
      =ACOPY(IIF(laTickets[lnAplTkt,2]='A','laACstType','laICstType'),laAplCst)

    CASE INLIST(laTickets[lnAplTkt,2],'D')		&&Alb*E301995
      DECLARE laAplCst[ALEN(IIF(laTickets[lnAplTkt,2]='D','laMCstType','laICstType'),1),3]
      =ACOPY(IIF(laTickets[lnAplTkt,2]='D','laMCstType','laICstType'),laAplCst)

  ENDCASE
  SHOW GET lnAplCst
ENDIF

*!*************************************************************
*! Name      : lfvAplCst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Selected Cost Type
*!*************************************************************
*! Calls     : gfwCodePop()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplCst()
*!*************************************************************
FUNCTION lfvAplCst
IF laAplCst[lnAplCst,3] = 'M'
  lnAplOpr =1
  =gfwCodePop(@laCodes,'MFGCODE','L')
  lcMfgCode=laMfgCode[lnAplOpr,2]

  SHOW GET lnAplOpr ENABLE
  *B606696,1 ALB Check that the Document currency same as invoice currency [BEGIN]
  SHOW GET lnAplCst   DISABLE
  SHOW GET ibRSession ENABLE
  SHOW GET lcRSession ENABLE
  SHOW GET ibWIPAcnt  ENABLE
  SHOW GET lcApDGlAct ENABLE
  SHOW GET pbGenerate ENABLE
  *B606696,1 ALB Check that the Document currency same as invoice currency [END]
ELSE
  *B606696,1 ALB Check that the Document currency same as invoice currency [BEGIN]
  IF lfChkCstCur(laTickets[lnAplTkt,2],lcTktNo,laAplCst[lnAplCst,3])
    =gfwCodePop(@laCodes,'MFGCODE','N')
    SHOW GET lnAplOpr DISABLE
    lcMfgCode=REPLICATE(IIF(laAplCst[lnAplCst,3]='P','*',;
                        IIF(laAplCst[lnAplCst,3]='D','#',' ')),6)
    SHOW GET lnAplCst   DISABLE
    SHOW GET ibRSession ENABLE
    SHOW GET lcRSession ENABLE
    SHOW GET ibWIPAcnt  ENABLE
    SHOW GET lcApDGlAct ENABLE
    SHOW GET pbGenerate ENABLE
  ELSE
    SHOW GET ibTktNo  ENABLE
    SHOW GET lcTktNo  ENABLE
    SHOW GET lnAplCst DISABLE
    _CUROBJ = OBJNUM(lcTktNo)
  ENDIF  
  *B606696,1 ALB Check that the Document currency same as invoice currency [END]
ENDIF

*B606696,1 ALB Check that the Document currency same as invoice currency [BEGIN]
*B602466,1 Enable Generate button after selecting costing type
*SHOW GET lnAplCst   DISABLE
*SHOW GET ibRSession ENABLE
*SHOW GET lcRSession ENABLE
*SHOW GET ibWIPAcnt  ENABLE
*SHOW GET lcApDGlAct ENABLE
*SHOW GET pbGenerate ENABLE
*B602466,1 (End)
*B606696,1 ALB Check that the Document currency same as invoice currency [END]
*!*************************************************************
*! Name      : lfvAplOpr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Selected MFG Operation
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplOpr()
*!*************************************************************
FUNCTION lfvAplOpr
lcMfgCode=laMfgCode[lnAplOpr,2]
=gfRltFld(lcMfgCode,@laMfgRFld,'MFGCODE')
IF !EMPTY(lcMfgGlAcnt)
  *E300798,1 Message : 04152
  *E300798,1 MFG Code has been setup to be applied directly through the
  *E300798,1 cost sheet program
  *E300798,1 Button : 00000 
  *E300798,1 Ok
  =gfModalGen("TRM04152B00000","DIALOG",laMfgCode[lnAplOpr,1])
  RETURN
ENDIF
SELECT DIST cLotNo FROM MFGOPRDT ;
WHERE cIMTyp+cTktNo+cOprCode+cLotNo+TranCd = ;
laTickets[lnAplTkt,2]+lcTktNo+lcMfgCode INTO ARRAY laAplLots
lnAplLotNo = 1
DO CASE
  CASE _TALLY = 0
    DECLARE laAplLots[1]
    laAplLots[1] = 'N/A'
    SHOW GET lnAplLotNo DISABLE
  CASE _TALLY = 1
    SHOW GET lnAplLotNo DISABLE
  OTHERWISE
    SHOW GET lnAplLotNo ENABLE
ENDCASE
*B603738,1 KHM 07/20/2000 (Begin) Enabling the RSession object in case
*B603738,1                of there are no lots or one and only one lot.
*IF _TALLY = 0
IF _TALLY <= 1
*B603738,1 KHM 07/20/2000 (End)
  lcAplLotNo = SPACE(2)
  SHOW GET ibRSession ENABLE
  SHOW GET lcRSession ENABLE
ELSE
  lcAplLotNo = laAplLots[lnAplLotNo]
  STORE '' TO lcRSession
  SHOW GET ibRSession DISABLE
  SHOW GET lcRSession DISABLE
ENDIF

*C200447,1 ALB Change the defaulte WIP Account to LFNM Account (ENGLAND)[Begin]
IF ASCAN(laEvntTrig , PADR('UPDAAPGL',10)) <> 0
  =gfDoTriger('APPYINV',PADR('UPDAAPGL',10))
ENDIF
*C200447,1 ALB Change the defaulte WIP Account to LFNM Account (ENGLAND)[end]

*!*************************************************************
*! Name      : lfvAplLotNo
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Selected MFG Operation Lot#
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplLotNo()
*!*************************************************************
FUNCTION lfvAplLotNo
lcAplLotNo = laAplLots[lnAplLotNo]

*!*************************************************************
*! Name      : lfvRSession
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Selected Document Receiving Session
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  lcType    : Document Type
*!                       lcTicket  : Document #
*!                       llClrRead : Terminate Read
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRSession()
*!*************************************************************
FUNCTION lfvRSession
PARAMETERS lcType,lcTicket,llClrRead,lcItem,lcColor,lcCstType

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
PRIVATE lcBrFields,lcFile_Ttl,lcRcvSess
STORE '' TO lcBrFields,lcFile_Ttl,lcRcvSess
lcRcvSess = gfTempName()
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

DO CASE 
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  *CASE INLIST(lcType,'I','R')
  CASE INLIST(lcType,'I','R','A','D')
  *E301995,1 Alb Add Adorment order to invoice line [End]
    SET ORDER TO TAG Posrec IN POSLN
    IF llBrowse OR (!EMPTY(lcRSession) AND !SEEK(IIF(lcType='I','P',lcType)+lcTicket+lcRSession+SPACE(6)+ALLTRIM(lcItem),'POSLN'))
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      *MAN
      *E301995,1 Alb Add Adorment order to invoice line [Begin]
      *SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nCost&lcCstType) AS nRecAmnt;
      FROM POSLN WHERE cStyType+Po+cRSession+ShipNo+Style+STR(LineNo,6)+TranCd=IIF(lcType='I','P',lcType)+lcTicket ;
      AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem) GROUP BY cStyType,Po,cRSession INTO CURSOR RcvSess
      *SELECT cRSession ,Date ,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nLan_Cst&lcCstType) AS nRecAmnt;
      FROM POSLN WHERE cStyType+Po+cRSession+ShipNo+Style+STR(LineNo,6)+TranCd=IIF(lcType='I','P',lcType)+lcTicket ;
      AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem) AND IIF(lcType = 'A',trancd<>'6',.T.);
      GROUP BY cStyType,Po,cRSession INTO CURSOR RcvSess

      SELECT cRSession ,Date ,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nLan_Cst&lcCstType) AS nRecAmnt;
      FROM POSLN WHERE cStyType+Po+cRSession+ShipNo+Style+STR(LineNo,6)+TranCd=IIF(lcType='I','P',lcType)+lcTicket ;
      AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem) AND IIF(lcType = 'A',trancd<>'6',.T.);
      GROUP BY cStyType,Po,cRSession INTO DBF &gcWorkDir.&lcRcvSess
      *E301995,1 Alb Add Adorment order to invoice line [End]
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
      llBrowse = .T.
    ENDIF
    SET ORDER TO TAG PosLn IN POSLN
  CASE lcType='S'
    SET ORDER TO TAG Poshrec IN POSLN
    IF llBrowse OR (!EMPTY(lcRSession) AND !SEEK(lcTicket+lcRSession,'POSLN'))
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      *MAN 
      *SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nCost&lcCstType) AS nRecAmnt;
      FROM POSLN ;
      WHERE ShipNo+cRSession+cStyType+Po+PosLn.Style+;
      STR(LineNo,6)+TranCd = lcTicket AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem);
      GROUP BY ShipNo,cRSession INTO CURSOR RcvSess
      *SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nLan_Cst&lcCstType) AS nRecAmnt;
      FROM POSLN ;
      WHERE ShipNo+cRSession+cStyType+Po+PosLn.Style+;
      STR(LineNo,6)+TranCd = lcTicket AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem);
      GROUP BY ShipNo,cRSession INTO CURSOR RcvSess
      SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nLan_Cst&lcCstType) AS nRecAmnt;
      FROM POSLN ;
      WHERE ShipNo+cRSession+cStyType+Po+PosLn.Style+;
      STR(LineNo,6)+TranCd = lcTicket AND !EMPTY(cRSession) AND Style=ALLTRIM(lcItem);
      GROUP BY ShipNo,cRSession INTO DBF &gcWorkDir.&lcRcvSess
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
      llBrowse = .T.        
    ENDIF
    SET ORDER TO TAG PosLn IN POSLN
  CASE lcType='M'
    SET ORDER TO TAG Cutrec IN CutTktL
    IF llBrowse OR (!EMPTY(lcRSession) AND !SEEK(lcRSession+lcTicket+ALLTRIM(lcItem),'CutTktL'))
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      *SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nCost&lcCstType) AS nRecAmnt;
      FROM CutTktL ;
      WHERE CutTkt+Style+Dyelot+TranCd=lcTicket+ALLTRIM(lcItem) AND !EMPTY(cRSession) ;
      GROUP BY CutTkt,cRSession INTO CURSOR RcvSess
      SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,;
      SUM(TotQty*nCost&lcCstType) AS nRecAmnt;
      FROM CutTktL ;
      WHERE CutTkt+Style+Dyelot+TranCd=lcTicket+ALLTRIM(lcItem) AND !EMPTY(cRSession) ;
      GROUP BY CutTkt,cRSession INTO DBF &gcWorkDir.&lcRcvSess
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
      llBrowse = .T.    
    ENDIF
    SET ORDER TO TAG CutTktL IN CutTktL
  CASE INLIST(lcType,'L','F')
    SELECT POFLN
    SET ORDER TO TAG POFREC
    IF llBrowse OR (!EMPTY(lcRSession) AND ;
       !SEEK(IIF(lcType='L','P','R')+lcTicket+lcRSession+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor)))
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      *SELECT cRSession,Date,SUM(nFabTotQty) AS nRecQty,;
      SUM(nFabTotQty*nLan_Cost&lcCstType) AS nRecAmnt;
      FROM POFLN ;
      WHERE cMatType+PoMat+Fabric+Color+TranCd=;
      IIF(lcType='L','P','R')+lcTicket+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor) AND ;
      !EMPTY(cRSession) GROUP BY PoMat,cRSession INTO CURSOR RcvSess
      SELECT cRSession,Date,SUM(nFabTotQty) AS nRecQty,;
      SUM(nFabTotQty*nLan_Cost&lcCstType) AS nRecAmnt;
      FROM POFLN ;
      WHERE cMatType+PoMat+Fabric+Color+TranCd=;
      IIF(lcType='L','P','R')+lcTicket+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor) AND ;
      !EMPTY(cRSession) GROUP BY PoMat,cRSession INTO DBF &gcWorkDir.&lcRcvSess
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
      llBrowse = .T.    
    ENDIF
  CASE lcType='T'
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    SELECT MMFGORDD
    SET ORDER TO TAG MMFGORDD
    IF llBrowse OR (!EMPTY(lcRSession) AND ;
       !SEEK(lcTicket+lcRSession+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor)))
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      *SELECT cRSession,Drecvdate as Date,SUM(nmfgtotQty) AS nRecQty,;
      SUM(nmfgtotQty*nLan_Cost&lcCstType) AS nRecAmnt;
      FROM MMFGORDD;
      WHERE Cmfgordno+CFabric+Color+TranCd=;
      lcTicket+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor) AND ;
      !EMPTY(cRSession) GROUP BY cRSession INTO CURSOR RcvSess
      SELECT cRSession,Drecvdate as Date,SUM(nmfgtotQty) AS nRecQty,;
      SUM(nmfgtotQty*nLan_Cost&lcCstType) AS nRecAmnt;
      FROM MMFGORDD;
      WHERE Cmfgordno+CFabric+Color+TranCd=;
      lcTicket+IIF(EMPTY(lcItem),'',SUBSTR(lcItem,1,7)+lcColor) AND ;
      !EMPTY(cRSession) GROUP BY cRSession INTO DBF &gcWorkDir.&lcRcvSess
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
      llBrowse = .T.    
    ENDIF
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
ENDCASE
IF llBrowse
  IF llClrRead
    CLEAR READ
    RETURN
  ENDIF

  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  lcFile_Ttl = IIF(lcType$'RF','Issuing Sessions','Receiving Sessions')
  *lcBrFields = [cRSession :H = "Session#", Date :H = "Date", nRecQty :H = "Rcv. Qty", nRecAmnt :H = "Rcv. Amount"]
  IF lcType $ 'RF'
    lcBrFields = [cRSession :R:H = "Session#", Date :R:H = "Date", nRecQty :R:H = "Iss. Qty", nRecAmnt :R:H = "Iss. Amount"]
  ELSE
    lcBrFields = [cRSession :R:H = "Session#", Date :R:H = "Date", nRecQty :R:H = "Rcv. Qty", nRecAmnt :R:H = "Rcv. Amount"]
  ENDIF
  *lcRSession = IIF(ARIABROW('','Receiving Sessions',gnBrHSRow1,;
                  gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','cRSession',;
                  'laBrowArr'),cRSession,SPACE(6))
  lcRSession = IIF(ARIABROW('',lcFile_Ttl,gnBrHSRow1,gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','cRSession',;
                  'laBrowArr'),cRSession,SPACE(6))

  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

  IF USED(lcRcvSess)
    USE IN (lcRcvSess)
    ERASE &gcWorkDir.&lcRcvSess
  ENDIF
ENDIF
llBrowse = .F.
*B607278,1 ALB Get the correct WIP account with shipment ticket [Begin]
IF lcType = 'S' AND !llClrRead
  lcLinkCodN = IIF(EMPTY(lcTicket),SPACE(6),lfGetGlLnk(lcTicket,'',lcRSession))
  lcLinkCode = SUBSTR(lcLinkCodN,1,6)
  IF VAL(SUBSTR(lcLinkCodN,7)) > 1 
    lcMasse = IIF(EMPTY(lcRSession),"This shipment","This receiving session")
    lcMasse = lcMasse + " has more than one WIP account."+;
    " Would you like to keep the PO's using their default WIP accounts, or assign one WIP account for all PO's? "
    IF gfModalGen("TRM00000B04013","DIALOG",.F.,.F.,lcMasse ) = 1
      lcOldAlias = ALIAS()
      STORE '' TO lcApDGlAct,lcWIPAcnt
      SELECT (lcAPVInvDt)
      REPLACE cAPDGLAct WITH '' 
      SHOW GET ibWIPAcnt   DISABLE
      SHOW GET lcApDGlAct  DISABLE
      SHOW GET m.cApDGlAct DISABLE
    ENDIF
  ELSE
    IF !SEEK(lcLinkCode+'013','GL_LINK')
      =SEEK('DEFDEF'+'013','GL_LINK')
    ENDIF
  STORE GL_Link.GlAcnt TO lcApDGlAct,lcWIPAcnt
  SHOW GET lcApDGlAct
  ENDIF 
ENDIF
*B607278,1 ALB Get the correct WIP account with shipment ticket [end]

*!*************************************************************
*! Name      : lfvAutoGen
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Select Document Lines to Invoice.
*!*************************************************************
*! Calls     : lfGenInvLn(),lfAInvBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAutoGen()
*!*************************************************************
FUNCTION lfvAutoGen

*B602422,1 Check only when costing type is MFG Codes
IF laAplCst[lnAplCst,3] = 'M'
*B602422,1 (End)

  =gfRltFld(lcMfgCode,@laMfgRFld,'MFGCODE')
  IF !EMPTY(lcMfgGlAcnt)
    *E300798,1 Message : 04152
    *E300798,1 MFG Code has been setup to be applied directly through the
    *E300798,1 cost sheet program
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    =gfModalGen("TRM04152B00000","DIALOG",laMfgCode[lnAplOpr,1])
    RETURN
  ENDIF
ENDIF
=lfGenInvLn(laTickets[lnAplTkt,2],lcTktNo,lcMfgCode,lcAplLotNo,lcRSession,;
            laAplCst[lnAplCst,2],'','','',.T.)
SELECT (lcAutoInv)
IF EOF()
ELSE
  SHOW GETS WINDOW 'APAINVD0' DISABLE
  SHOW GET ibAInvDet ENABLE
  SHOW GET pbClear   ENABLE
  SHOW GETS WINDOW 'APAINVD2' ENABLE
  SHOW GET pbSelAll DISABLE
  
  =lfAInvBrow()
ENDIF
=lfwAInvDt() &&>>> 

*!*************************************************************
*! Name      : lfClrInvLn
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Clear Selected Document Lines
*!*************************************************************
*! Calls     : lfAInvBrow(),lfRefresh(),gfwCodePop()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrInvLn()
*!*************************************************************
FUNCTION  lfClrInvLn
DECLARE laAplCst[1,3],laTickets[1,3],laAplLots[1]
STORE 0         TO lnAplLotNo,lnAMarker
STORE SPACE(24) TO lcApDGlAct
STORE SPACE(6)  TO lcTktNo
STORE 1         TO lnAplTkt,lnAplCst
STORE ''        TO lcMfgCode,lcRSession
STORE 'N/A'     TO laOprLots[1]
STORE SPACE(2)  TO lcAplLotNo
laTickets[1,1] = 'Select Document Type'
laTickets[1,2] = ' '
laTickets[1,3] = 'Document#'
laAplCst[1,1]  = 'Select Cost Type'
laAplCst[1,2]  = ' '
laAplCst[1,3]  = ' '

SELECT (lcAutoInv)
ZAP
=lfAInvBrow()
=lfRefresh('APAINVD0')
SHOW GETS WINDOW 'APAINVD0' DISABLE ONLY
SHOW GETS WINDOW 'APAINVD2' DISABLE ONLY
SHOW GET ibAInvDet  ENABLE
SHOW GET lnAplTkt   ENABLE
SHOW GET pbCancel   ENABLE
=gfwCodePop(@laCodes,'MFGCODE','N')
_CUROBJ = OBJNUM(ibAInvDet)

*!*************************************************************
*! Name      : lfvSelect
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Select Some Ticket line to Invoice.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcType  : 'S' : Select/UnSelect
*!                               : 'A' : Select All
*!                               : 'N' : Select None
*!                               : 'V' : Invert
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSelect('S')
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcType
PRIVATE lcDataSel,lcDataUSel,lcData,cKey

lcKey=Item+cWareCode+cDyelot 
DO CASE
  CASE lcType = 'S'
   *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
   IF !(laTickets[lnAplTkt,2] = "S"  .AND. SEEK("P"+CtktNo,"POSHDR") .AND. POSHDR.Status = "S")
   *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
     REPLACE cSelect WITH IIF(cSelect=SPACE(1),"»",SPACE(1))
   *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
   ENDIF
   *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
  CASE lcType = 'A'
   *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
     *REPLACE ALL cSelect WITH "»"
     REPLACE ALL cSelect WITH IIF(!(laTickets[lnAplTkt,2] = "S"  .AND. SEEK("P"+CtktNo,"POSHDR") .AND. POSHDR.Status = "S"),"»","")
   *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
  CASE lcType = 'N'
   REPLACE ALL cSelect WITH SPACE(1)
  CASE lcType = 'V'
    *REPLACE ALL cSelect WITH IIF(cSelect=SPACE(1),"»",SPACE(1))
    REPLACE ALL cSelect WITH IIF(cSelect=SPACE(1) .AND. !(laTickets[lnAplTkt,2] = "S"  .AND. SEEK("P"+CtktNo,"POSHDR") .AND. POSHDR.Status = "S"),"»",SPACE(1))
ENDCASE
SET ORDER TO TAG 'SELECT'
lcDataSel   = IIF(SEEK("»"),'ENABLE','DISABLE')
lcDataUSel  = IIF(SEEK(' '),'ENABLE','DISABLE')
SET ORDER TO TAG (lcAutoInv)
=SEEK(lcKey)
lcData  = IIF(EOF(),'DISABLE','ENABLE')
SHOW GET pbSelAll  &lcDataUSel
SHOW GET pbSelNone &lcDataSel
SHOW GET pbInvert  &lcData
SHOW GET pbProceed &lcDataSel
IF cSelect = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select' &lcData
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select' &lcData
ENDIF

*!*************************************************************
*! Name      : lfvProAuto
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create Invoice Lines Automatically
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProAuto()
*!*************************************************************
FUNCTION lfvProAuto

*aminamin
*PRIVATE lcItem
PRIVATE lcItem , lcWipAcct
*aminamin

SET ORDER TO TAG ITEM IN (lcAPvInvDt)
SELECT (lcAutoInv)
SET ORDER TO TAG 'SELECT'
=SEEK("»")

*B605040,1  Get the Max. line from invoicetkt  [Start]
*laData[52] = lfGetMaxLin()
*B605040,1  Get the Max. line invoicetkt [End]

SCAN REST WHILE cSelect+Item+cWareCode+cDyelot = "»"
  lcItem = IIF(INLIST(laTickets[lnAplTkt,2],'L','T'),PADR(SUBSTR(Item,1,7),19),Item)
  SELECT (lcAPvInvDt)
  *B606359,1 SSH [Start] Initiale lcWipAcct.[Start]
  lcWipAcct = IIF(  laTickets[lnAplTkt,2]= 'S'  ,&lcAutoInv..cWIPAcct , lcApDGlAct)
  *B606359,1 SSH [Start] Initiale lcWipAcct.[END]
  
  *B607916,1 TMI [Start] Add the lcRSession to the key
  *IF !SEEK(laTickets[lnAplTkt,2]+lcTktNo+lcAplLotNo+STR(&lcAutoInv..LineNo,6)+;
           laAplCst[lnAplCst,2]+lcMfgCode+lcItem+&lcAutoInv..Color+&lcAutoInv..cDyelot)
  IF !SEEK(laTickets[lnAplTkt,2]+lcTktNo+lcAplLotNo+STR(&lcAutoInv..LineNo,6)+;
           laAplCst[lnAplCst,2]+lcMfgCode+lcItem+&lcAutoInv..Color+&lcAutoInv..cDyelot+lcRSession)
    *B607916,1 TMI [End  ]            
    lcAPVIDes = ''
    DO CASE
      *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
      *CASE INLIST(laTickets[lnAplTkt,2],'I','S','M','R')
      CASE INLIST(laTickets[lnAplTkt,2],'I','S','M','R','D','A')
      *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
        =SEEK(lcItem,'Style')
        lcAPVIDes = Style.Desc1
      CASE INLIST(laTickets[lnAplTkt,2],'T','L')
        =SEEK(SUBSTR(lcItem,1,7)+&lcAutoInv..Color,'Fabric')
        lcAPVIDes = Fabric.Desc
    ENDCASE  
    laData[52] = laData[52] + 1
    APPEND BLANK
    
    *?Amin 
    *B605040,1 1)Update the GL account with proper on in case of Shipmnet  [Begin]
    *B605040,1 2)Replace cOprCode field with correct values based on Style Detail costing Yes or No
    
    *REPLACE cStatus    WITH 'A'  ,;
    *        cVendCode  WITH laData[1] ,;
    *        cAPInvNo   WITH laData[2] ,;
    *        cApVILNo   WITH STR(laData[52],4) ,;
    *        cIMTyp     WITH laTickets[lnAplTkt,2] ,;
    *        LineNo     WITH &lcAutoInv..LineNo    ,;
    *        cBomType   WITH laAplCst[lnAplCst,2]  ,;
    *        cCostType  WITH laAplCst[lnAplCst,3]  ,;
    *        cOprCode   WITH lcMfgCode   ,; 
    *        cTktNo     WITH IIF(cIMTyp='S',&lcAutoInv..cTktNo,lcTktNo) ,;
    *        ShipNo     WITH IIF(cIMTyp='S',lcTktNo,'') ,;
    *        cLotNo     WITH lcAplLotNo  ,;
    *        Item       WITH lcItem      ,;
    *        Color      WITH &lcAutoInv..Color ,;
    *        cDyelot    WITH &lcAutoInv..cDyelot,;
    *        cAPVIDes   WITH lcAPVIDes ,;
    *        cApDGlAct  WITH lcApDGlAct

    lcWipAcct = IIF(  laTickets[lnAplTkt,2]= 'S'  ,&lcAutoInv..cWIPAcct , lcApDGlAct)          
      
    =SEEK(lcItem,'Style')
    IF lcMfgCode =REPLICATE('*',6)
      lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+laAplCst[lnAplCst,2],6),lcMfgCode )
    ENDIF
    IF lcMfgCode =REPLICATE('#',6)
      lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+laAplCst[lnAplCst,2],6),lcMfgCode )
    ENDIF      
    *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
    *REPLACE cStatus    WITH 'A'  ,;
            cVendCode  WITH laData[1] ,;
            cAPInvNo   WITH laData[2] ,;
            cApVILNo   WITH STR(laData[52],4) ,;
            cIMTyp     WITH laTickets[lnAplTkt,2] ,;
            LineNo     WITH &lcAutoInv..LineNo    ,;
            cBomType   WITH laAplCst[lnAplCst,2]  ,;
            cCostType  WITH laAplCst[lnAplCst,3]  ,;
            cOprCode   WITH lcMfgCode   ,; 
            cTktNo     WITH IIF(cIMTyp='S',&lcAutoInv..cTktNo,lcTktNo) ,;
            ShipNo     WITH IIF(cIMTyp='S',lcTktNo,'') ,;
            cLotNo     WITH lcAplLotNo  ,;
            Item       WITH lcItem      ,;
            Color      WITH &lcAutoInv..Color ,;
            cDyelot    WITH &lcAutoInv..cDyelot,;
            cAPVIDes   WITH lcAPVIDes ,;
            cApDGlAct  WITH lcWipAcct 

    REPLACE cStatus    WITH 'A'  ,;
            cVendCode  WITH laData[1] ,;
            cAPInvNo   WITH laData[2] ,;
            cApVILNo   WITH STR(laData[52],4) ,;
            cIMTyp     WITH laTickets[lnAplTkt,2] ,;
            LineNo     WITH &lcAutoInv..LineNo    ,;
            cBomType   WITH laAplCst[lnAplCst,2]  ,;
            cCostType  WITH laAplCst[lnAplCst,3]  ,;
            cOprCode   WITH lcMfgCode   ,; 
            cTktNo     WITH IIF(cIMTyp='S',&lcAutoInv..cTktNo,lcTktNo) ,;
            ShipNo     WITH IIF(cIMTyp='S',lcTktNo,'') ,;
            cLotNo     WITH lcAplLotNo  ,;
            Item       WITH lcItem      ,;
            Color      WITH &lcAutoInv..Color ,;
            cDyelot    WITH &lcAutoInv..cDyelot,;
            cAPVIDes   WITH lcAPVIDes ,;
            cApDGlAct  WITH lcWipAcct ,;
            CLOTNO     WITH IIF(cIMTyp='D',laAplLots[lnAplLotNo],'') 
    *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
  ENDIF
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  lnDisAmnt = &lcAutoInv..nInvAmnt * IIF(laTickets[lnAplTkt,2] $ 'RF',-1,1)
  *=lfUpdApDist(cApVILNo,&lcAutoInv..nInvAmnt,lcWipAcct,laTickets[lnAplTkt,2])
  =lfUpdApDist(cApVILNo,lnDisAmnt,lcWipAcct,laTickets[lnAplTkt,2])
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  *B605040,1 1)Update the GL account with proper on in case of Shipmnet  [End]
  *B605040,1 2)Replace cOprCode field with correct values based on Style Detail costing Yes or No
  
  REPLACE nAPQty1    WITH nAPQty1 + &lcAutoInv..nInvQty1   ,;
          nAPQty2    WITH nAPQty2 + &lcAutoInv..nInvQty2   ,;
          nAPQty3    WITH nAPQty3 + &lcAutoInv..nInvQty3   ,;
          nAPQty4    WITH nAPQty4 + &lcAutoInv..nInvQty4   ,;
          nAPQty5    WITH nAPQty5 + &lcAutoInv..nInvQty5   ,;
          nAPQty6    WITH nAPQty6 + &lcAutoInv..nInvQty6   ,;
          nAPQty7    WITH nAPQty7 + &lcAutoInv..nInvQty7   ,;
          nAPQty8    WITH nAPQty8 + &lcAutoInv..nInvQty8   ,;
          nApTotQty  WITH nApTotQty + &lcAutoInv..nInvQty  ,;
          nApAmnt    WITH nApAmnt + &lcAutoInv..nInvAmnt   ,;
          nApPrice   WITH IIF(nApTotQty=0,0,nApAmnt/nApTotQty)
  IF !EMPTY(lcRSession) OR llDefAuApr
    REPLACE nAPAprQty1 WITH nAPAprQty1 + &lcAutoInv..nInvQty1 ,;
            nAPAprQty2 WITH nAPAprQty2 + &lcAutoInv..nInvQty2 ,;
            nAPAprQty3 WITH nAPAprQty3 + &lcAutoInv..nInvQty3 ,;
            nAPAprQty4 WITH nAPAprQty4 + &lcAutoInv..nInvQty4 ,;
            nAPAprQty5 WITH nAPAprQty5 + &lcAutoInv..nInvQty5 ,;
            nAPAprQty6 WITH nAPAprQty6 + &lcAutoInv..nInvQty6 ,;
            nAPAprQty7 WITH nAPAprQty7 + &lcAutoInv..nInvQty7 ,;
            nAPAprQty8 WITH nAPAprQty8 + &lcAutoInv..nInvQty8 ,;
            nApTAprQty WITH nApTAprQty + &lcAutoInv..nInvQty  ,;
            nAPAprAmnt WITH nAPAprAmnt + &lcAutoInv..nInvAmnt ,;
            nAPAprPric WITH IIF(nApTAprQty=0,0,nAPAprAmnt/nApTAprQty)
    *MAN Fix Round Problem to match total invoice        
    *lnTAprAmt = lnTAprAmt + &lcAutoInv..nInvAmnt*IIF(laTickets[lnAplTkt,2] $ 'RF',-1,1)
  ENDIF
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  REPLACE nApAmnt    WITH nApAmnt    * IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1),;
          nApPrice   WITH nApPrice   * IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1),;
          nAPAprAmnt WITH nAPAprAmnt * IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1),;
          nAPAprPric WITH nAPAprPric * IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1)
  lnTAprAmt = lnTAprAmt + ROUND(&lcAutoInv..nInvAmnt*IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1),2)
  *lnTInvAmt = lnTInvAmt + ROUND(&lcAutoInv..nInvAmnt*IIF(laTickets[lnAplTkt,2] $ 'RF',-1,1),2)
  lnTInvAmt = lnTInvAmt + ROUND(&lcAutoInv..nInvAmnt*IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1),2)
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  SELECT (lcAPInvTkt)
  *--MAN Automatic feature always fully contribute, therefore it shouldn't add this rec
*  IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo+SPACE(6))
*    INSERT INTO (lcAPInvTkt) ;
    (cVendCode,cApInvNo,cApVILNo,Item,Color,cWareCode,cDyelot,cStatus,LineNo,;
     cIMTyp,cTktNo,cRecvType) VALUES ;
    (laData[1],laData[2],&lcAPvInvDt..cApVILNo,&lcAPvInvDt..Item,;
     &lcAPvInvDt..Color,&lcAutoInv..cWareCode,&lcAPvInvDt..cDyelot,'A',;
     &lcAutoInv..LineNo,laTickets[lnAplTkt,2],lcTktNo,&lcAutoInv..cRecvType)
*  ENDIF
  *aminamin2
  *IF EMPTY(lcRSession)
  *  REPLACE nApAplQty1 WITH nApAplQty1 + &lcAutoInv..nInvQty1 ,;
  *          nApAplQty2 WITH nApAplQty2 + &lcAutoInv..nInvQty2 ,;
  *          nApAplQty3 WITH nApAplQty3 + &lcAutoInv..nInvQty3 ,;
  *          nApAplQty4 WITH nApAplQty4 + &lcAutoInv..nInvQty4 ,;
  *          nApAplQty5 WITH nApAplQty5 + &lcAutoInv..nInvQty5 ,;
  *          nApAplQty6 WITH nApAplQty6 + &lcAutoInv..nInvQty6 ,;
  *          nApAplQty7 WITH nApAplQty7 + &lcAutoInv..nInvQty7 ,;
  *          nApAplQty8 WITH nApAplQty8 + &lcAutoInv..nInvQty8 ,;
  *          nApTAplQty WITH nApTAplQty + &lcAutoInv..nInvQty  ,;
  *          nApAplAmnt WITH nApAplAmnt + &lcAutoInv..nInvAmnt ,;
  *          nApAPlPric WITH IIF(nApTAplQty=0,0,nApAplAmnt/nApTAplQty)
  *ELSE
 
    *IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo+lcRSession)
  *aminamin2    
     
*     IF ( laTickets[lnAplTkt,2] <> 'S' OR ;
*        ( laTickets[lnAplTkt,2]  = 'S' AND !EMPTY(lcRSession) ) )  ;
*      AND !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo+lcRSession)
*     
    IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo+lcRSession)
      *B603827,1 KHM 08/27/2000 (Begin) Replacing the cTktNo field with the
      *B603827,1                correct value in case of shipment.
      *INSERT INTO (lcAPInvTkt) ;
      (cVendCode,cApInvNo,cApVILNo,cRSession,Item,Color,cWareCode,cDyelot,;
       cStatus,LineNo,cIMTyp,cTktNo,cRecvType) VALUES  ;
      (laData[1],laData[2],&lcAPvInvDt..cApVILNo,lcRSession,&lcAPvInvDt..Item,;
       &lcAPvInvDt..Color,&lcAutoInv..cWareCode,&lcAutoInv..cDyelot,'A',;
       &lcAutoInv..LineNo,laTickets[lnAplTkt,2],lcTktNo,&lcAutoInv..cRecvType)
       
      *B605040,1  Update GlAccount and APDistLine in invoicetkt  [Start]
      *INSERT INTO (lcAPInvTkt) ;
      *(cVendCode,cApInvNo,cApVILNo,cRSession,Item,Color,cWareCode,cDyelot,;
      * cStatus,LineNo,cIMTyp,cTktNo,cRecvType) VALUES  ;
      *(laData[1],laData[2],&lcAPvInvDt..cApVILNo,lcRSession,;
      * &lcAPvInvDt..Item,&lcAPvInvDt..Color,&lcAutoInv..cWareCode,;
      * &lcAutoInv..cDyelot,'A',&lcAutoInv..LineNo,laTickets[lnAplTkt,2],;
      * IIF(laTickets[lnAplTkt,2]='S',&lcAutoInv..cTktNo,lcTktNo),;
      * &lcAutoInv..cRecvType)
      *B603827,1 KHM 08/27/2000 (End)
      
       INSERT INTO (lcAPInvTkt) ;
      (cVendCode,cApInvNo,cApVILNo,cRSession,Item,Color,cWareCode,cDyelot,;
       cStatus,LineNo,cIMTyp,cTktNo,cRecvType,cApdlinNo,cApdGlAct) VALUES  ;
      (laData[1],laData[2],&lcAPvInvDt..cApVILNo,lcRSession,;
       &lcAPvInvDt..Item,&lcAPvInvDt..Color,&lcAutoInv..cWareCode,;
       &lcAutoInv..cDyelot,'A',&lcAutoInv..LineNo,laTickets[lnAplTkt,2],;
       IIF(laTickets[lnAplTkt,2]='S',&lcAutoInv..cTktNo,lcTktNo),;
       &lcAutoInv..cRecvType,&lcAPvInvDt..cApVILNo,&lcAPvInvDt..cApDGlAct)
       *B605040,1 AKA  Update GlAccount and APDistLine in invoicetkt [End]       
       
    ENDIF
    REPLACE nApAplQty1 WITH nApAplQty1 + &lcAutoInv..nInvQty1 ,;
            nApAplQty2 WITH nApAplQty2 + &lcAutoInv..nInvQty2 ,;
            nApAplQty3 WITH nApAplQty3 + &lcAutoInv..nInvQty3 ,;
            nApAplQty4 WITH nApAplQty4 + &lcAutoInv..nInvQty4 ,;
            nApAplQty5 WITH nApAplQty5 + &lcAutoInv..nInvQty5 ,;
            nApAplQty6 WITH nApAplQty6 + &lcAutoInv..nInvQty6 ,;
            nApAplQty7 WITH nApAplQty7 + &lcAutoInv..nInvQty7 ,;
            nApAplQty8 WITH nApAplQty8 + &lcAutoInv..nInvQty8 ,;
            nApTAplQty WITH nApTAplQty + &lcAutoInv..nInvQty  ,;
            nApAplAmnt WITH nApAplAmnt + &lcAutoInv..nInvAmnt ,;
            nApAPlPric WITH IIF(nApTAplQty=0,0,nApAplAmnt/nApTAplQty)
    
    *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
    REPLACE nApAplAmnt WITH nApAplAmnt * IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1),;
            nApAPlPric WITH nApAPlPric * IIF((laTickets[lnAplTkt,2] $ 'RF') OR llDebitM ,-1,1)
    *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  *aminamin2            
  *ENDIF
  *aminamin2
ENDSCAN
SET ORDER TO TAG (lcAPvInvDt) IN (lcAPvInvDt)

*!*************************************************************
*! Name      : lfvAprMore
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Line Multiple Approve
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAprMore()
*!*************************************************************
FUNCTION lfvAprMore
PRIVATE lcInvLine

lnMMarker = 0
lcMAPrTtl = 'Approved Invoice Line'
lcInvLine = &lcAPvInvDt..cApVILNo
llDetails = INLIST(&lcAPvInvDt..cIMTyp,'M','I','S','R') AND llDefQtyDt

*B602830,1 Add this line to check if the quantity field is going to use
*          decimal points or not, the quantity field is going to use
*          decimal points in the case of document types Material PO
*          , Material PO Return and Material MFG Order [Begin]
llQtyUsDec = INLIST(&lcAPvInvDt..cIMTyp , 'L' , 'T' , 'F')
lcQtyFldPc = IIF(llQtyUsDec , '9999999.999' , '9999999')
*B602830,1 Add this line to check if the quantity field is going to use [End]

SELECT *,RECNO() AS nLRecNo FROM (lcAPvInvDt) WHERE cVendCode+cApInvNo+cApVILNo =;
PADR(laData[1],8)+PADR(laData[2],12)+lcInvLine ;
INTO DBF (gcWorkDir+lcTmpApr)
SCATTER MEMVAR
DO (gcScrDir + gcWinAppl + '\APIMAPR.SPR')
=lfRefresh('APINVD10')
=lfwInvDt()

*!*************************************************************
*! Name      : lfMAprBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse Invoice Line Multiple Approve
*!*************************************************************
*! Calls     : lfWMApr
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfMAprBrow()
*!*************************************************************
FUNCTION lfMAprBrow

SELECT (lcTmpApr)
lnMMarker = RECNO()
*MAN Added Round 2 to total inv and total appr
lcFields = "cMarker=IIF(RECNO()=lnMMarker ,'>',' '):H=' ':R:1:W=.F.," +;
           "nApTotQty :H='Inv. Qty.' :R,nApPrice :H='Inv. Price' :R,nApAmnt=ROUND(nApAmnt,2) :H='Inv. Amount' :R,"+;
           "nApTAprQty :H='Appr. Qty.' :R,nApAprPric :H='Appr. Price' :R,nApAprAmnt=ROUND(nApAprAmnt,2) :H='Appr. Amount' :R"
BROWSE FIELDS &lcFields  ;
       WINDOW APIMAPR1   ;
       IN WINDOW APIMAPR ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfWMApr()    ;
       TITLE lcMAPrTtl

*!*************************************************************
*! Name      : lfWMApr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh Prroved Line
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWMApr()
*!*************************************************************
FUNCTION lfWMApr

lnMMarker  = RECNO()
SHOW WINDOW (lcMAPrTtl) REFRESH SAME
SCATTER MEMVAR
IF llDetails
  =lfRefresh('APIMAPR2')
  SHOW GETS WINDOW 'APIMAPR2' ONLY
ELSE
  =lfRefresh('APIMAPR3')
  SHOW GETS WINDOW 'APIMAPR3' ONLY
ENDIF

*!*************************************************************
*! Name      : lfDMApprove
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate Multiple Approve Screen
*!*************************************************************
*! Calls     : lfWMApr()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDMApprove()
*!*************************************************************
FUNCTION lfDMApprove

IF WONTOP()=lcMAPrTtl
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTabMove WITH ;
  IIF(llDetails,'APIMAPR2','APIMAPR3'),IIF(llDetails,'m.nAPAprQty1','m.nAPTAprQty')
  ON KEY LABEL BACKTAB DO lpBackMove WITH ;
  IIF(llDetails,'APIMAPR2','APIMAPR3'),'pbCancel'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvNewApr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add new Prroved Line
*!*************************************************************
*! Calls     : lfWMApr()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewApr()
*!*************************************************************
FUNCTION lfvNewApr

SELECT (lcAPvInvDt)
=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLine)
SCATTER MEMVAR FIELDS cVendCode,cApInvNo,cApVILNo,cIMTyp,cBOMType,cCostType,;
                      cOprCode,cLotNo,Item,Color,cDyelot,cApVIDes,cTktNO,ShipNo,cApDGlAct
m.nApAprPric = nApPrice
SELECT (lcTmpApr)
SUM ALL nApAprQty1,nApAprQty2,nApAprQty3,nApAprQty4,nApAprQty5,nApAprQty6,;
        nApAprQty7,nApAprQty8 TO m.nApAprQty1,m.nApAprQty2,m.nApAprQty3,;
        m.nApAprQty4,m.nApAprQty5,m.nApAprQty6,m.nApAprQty7,m.nApAprQty8

m.nApAprQty1 = MAX(&lcAPvInvDt..nApQty1-m.nApAprQty1,0)
m.nApAprQty2 = MAX(&lcAPvInvDt..nApQty2-m.nApAprQty2,0)
m.nApAprQty3 = MAX(&lcAPvInvDt..nApQty3-m.nApAprQty3,0)
m.nApAprQty4 = MAX(&lcAPvInvDt..nApQty4-m.nApAprQty4,0)
m.nApAprQty5 = MAX(&lcAPvInvDt..nApQty5-m.nApAprQty5,0)
m.nApAprQty6 = MAX(&lcAPvInvDt..nApQty6-m.nApAprQty6,0)
m.nApAprQty7 = MAX(&lcAPvInvDt..nApQty7-m.nApAprQty7,0)
m.nApAprQty8 = MAX(&lcAPvInvDt..nApQty8-m.nApAprQty8,0)
m.nApTAprQty = m.nApAprQty1+m.nApAprQty2+m.nApAprQty3+m.nApAprQty4+;
               m.nApAprQty5+m.nApAprQty6+m.nApAprQty7+m.nApAprQty8
m.nApAprAmnt = m.nApTAprQty*m.nApAprPric
SELECT (lcTmpApr)
APPEND BLANK
GATHER MEMVAR FIELDS cVendCode,cApInvNo,cApVILNo,cIMTyp,cBOMType,cCostType,;
cOprCode,cLotNo,Item,Color,cDyelot,cApVIDes,cTktNO,ShipNo,cApDGlAct,nApAprPric,;
nApAprQty1,nApAprQty2,nApAprQty3,nApAprQty4,nApAprQty5,nApAprQty6,nApAprQty7,;
nApAprQty8,nApTAprQty,nApAprAmnt
=lfWMApr()

*B602830,1 Change this line because we are going to switch between 2 Get
*          Fields for the "Approve Quantity" to be able to add the
*          decimal points to the "Approve Quantity" in the case of
*          document types Material PO, Material PO Return and
*          Material MFG Order [Begin]
*_CUROBJ = IIF(llDetails,OBJNUM(m.nAPAprQty1),OBJNUM(m.nAPTAprQty))
_CUROBJ = IIF(llDetails , OBJNUM(m.nAPAprQty1) , lnApQtyObj)
*B602830,1 Change this line because we are going to switch between 2 [End]

*!*************************************************************
*! Name      : lfvRemApr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Remove Invoice Approve Line
*!*************************************************************
*! Calls     : gfModalGen,lfWMApr
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemApr()
*!*************************************************************
FUNCTION lfvRemApr
IF gfModalGen("QRM00007B00007","ALERT") = 1
  DELETE
  GO TOP
  =lfWMApr()
ENDIF

*!*************************************************************
*! Name      : lfvMAprQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Approved Total Quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvMAprQty()
*!*************************************************************
FUNCTION lfvMAprQty

m.nAPAprPric = IIF(m.nAPTAprQty=0,m.nAPAprAmnt,m.nAPAprPric)
m.nAPTAprQty = IIF(m.nAPTAprQty=0,1,m.nAPTAprQty)
m.nAPAprAmnt = m.nAPTAprQty*m.nAPAprPric
SHOW GET m.nAPAprAmnt
GATHER MEMVAR FIELDS nAPAprAmnt,nAPTAprQty,nAPAprPric
SHOW WINDOW (lcMAPrTtl) REFRESH SAME

*!*************************************************************
*! Name      : lfvMAprCst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Approved Cost
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvMAprCst()
*!*************************************************************
FUNCTION lfvMAprCst

m.nAPAprAmnt = m.nAPTAprQty*m.nAPAprPric
GATHER MEMVAR FIELDS nAPAprAmnt,nAPAprPric
SHOW WINDOW (lcMAPrTtl) REFRESH SAME

*!*************************************************************
*! Name      : lfvMAprAmnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Approved Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvMAprAmnt()
*!*************************************************************
FUNCTION lfvMAprAmnt
*man Adeed the following line
m.nAPAprAmnt = m.nAPAprAmnt*1.000
m.nAPAprPric = IIF(m.nAPTAprQty=0,0,m.nAPAprAmnt/m.nAPTAprQty)
SHOW GET m.nAPAprPric
GATHER MEMVAR FIELDS nAPAprAmnt,nAPAprPric
SHOW WINDOW (lcMAPrTtl) REFRESH SAME

*!*************************************************************
*! Name      : lfvMAprSQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Approved Size Quantity
*!*************************************************************
*! Calls     : lfvMAprQty,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvMAprSQty()
*!*************************************************************
FUNCTION lfvMAprSQty

m.nAPTAprQty = m.nAPAprQty1+m.nAPAprQty2+m.nAPAprQty3+m.nAPAprQty4+;
               m.nAPAprQty5+m.nAPAprQty6+m.nAPAprQty7+m.nAPAprQty8
GATHER MEMVAR FIELDS nAPAprQty1,nAPAprQty2,nAPAprQty3,nAPAprQty4,nAPAprQty5,;
                     nAPAprQty6,nAPAprQty7,nAPAprQty8
=lfvMAprQty()
SHOW WINDOW (lcMAPrTtl) REFRESH SAME
=lfRefresh('APIMAPR2')

*!*************************************************************
*! Name      : lfvProMApr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add Invoice Approved Lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProMApr()
*!*************************************************************
FUNCTION lfvProMApr

SET DELETE OFF
SCAN
  DO CASE
    CASE DELETED() AND nLRecNo <> 0
      SELECT (lcAPvInvDt)
      GO &lcTmpApr..nLRecNo
      REPLACE CSTATUS WITH IIF(CSTATUS='A','S','D')
      DELETE
      lnTAprAmt = lnTAprAmt - ROUND(nApAprAmnt,2)*IIF(m.CIMTYP $ 'RF',-1,1)
    CASE !DELETED() AND nLRecNo <> 0
      SCATTER MEMVAR
      SELECT (lcAPvInvDt)
      GO &lcTmpApr..nLRecNo 
      m.CSTATUS = IIF(cStatus='S','M',cStatus)
      lnTAprAmt = lnTAprAmt+(ROUND(m.nApAprAmnt,2)-ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
      GATHER MEMVAR
    CASE !DELETED() AND nLRecNo = 0
      SCATTER MEMVAR
      m.cStatus = 'A'
      SELECT (lcAPvInvDt)
      APPEND BLANK
      GATHER MEMVAR
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      *lnTAprAmt = lnTAprAmt + ROUND(m.nApAprAmnt,2)*IIF(m.CIMTYP $ 'RF',-1,1)
      lnTAprAmt = lnTAprAmt + ROUND(m.nApAprAmnt,2)
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  ENDCASE
ENDSCAN
SET DELETE ON
SELECT (lcAPvInvDt)

*!*************************************************************
*! Name      : lfChkAplTkt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Check Approved Quantity/Amount againest Contributed Quantity/Amount
*!*************************************************************
*! Calls     : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkAplTkt()
*!*************************************************************
FUNCTION lfChkAplTkt
PARAMETERS lcSize

*MAN Start
*-- If there is one automatic contributed line just update its value 
PRIVATE lnCurAls,lcPInvLNo,laContTot
lcPInvLNo = &lcAPvInvDt..cApVILNo
lnCurAls  = SELECT(0)
SELECT COUNT(*),SUM(nApAplQty1),SUM(nApAplQty2),SUM(nApAplQty3),SUM(nApAplQty4),;
                SUM(nApAplQty5),SUM(nApAplQty6),SUM(nApAplQty7),SUM(nApAplQty8),; 
                SUM(nApAplAmnt),SUM(IIF(!EMPTY(cRsession),nApAplAmnt,0)) ;
 FROM (lcAPInvTkt)             ;
WHERE cVendCode         + cApInvNo           + cApVILNo  + cRSession= ;
      PADR(laData[1],8) + PADR(laData[2],12) + lcPInvLNo + ""         ;
 INTO ARRAY laContTot

*-- No Contribution
IF _TALLY = 0 OR laContTot[11] = 0
  RETURN
ENDIF  
SELECT (lcAPInvTkt)
*-- If there is only one contributed line update the contribution generated 
*-- from the automatic option.
IF laContTot[1] = 1 AND SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo) ;
   AND !EMPTY(&lcAPInvTkt..cRSession)

  *-- Update the contribution.   
*--- SSH
  REPLACE nApAplQty1 WITH m.nApAprQty1, nApAplQty2 WITH m.nApAprQty2,;
          nApAplQty3 WITH m.nApAprQty3, nApAplQty4 WITH m.nApAprQty4,;
          nApAplQty5 WITH m.nApAprQty5, nApAplQty6 WITH m.nApAprQty6,;
          nApAplQty7 WITH m.nApAprQty7, nApAplQty8 WITH m.nApAprQty8,;
  		  nApTAplQty WITH m.nApTAprQty, nApAplPric WITH m.nApAprPric,; 	
  		  nApAplAmnt WITH m.nApTAprQty * m.nApAprPric               ,;
  		  cStatus    WITH IIF(cStatus='S','M',cStatus)

*  REPLACE nApAplQty1 WITH m.nApAprQty1, nApAplQty2 WITH m.nApAprQty2,;
          nApAplQty3 WITH m.nApAprQty3, nApAplQty4 WITH m.nApAprQty4,;
          nApAplQty5 WITH m.nApAprQty5, nApAplQty6 WITH m.nApAprQty6,;
          nApAplQty7 WITH m.nApAprQty7, nApAplQty8 WITH m.nApAprQty8,;
  		  nApTAplQty WITH m.nApTAprQty, nApAplPric WITH m.nApAprPric,; 	
  		  nApAplAmnt WITH m.nApTAprQty * nApAplPric                 ,;
  		  cStatus    WITH IIF(cStatus='S','M',cStatus)
*--- SSH
  SELECT (lnCurAls)		  
  RETURN
ENDIF
SELECT (lnCurAls)
*MAN END        
      
 
IF SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo+SPACE(6),lcAPInvTkt)
  
  *B602830,1 Add these lines to make sure that the "Approved Amount" and
  *          the "Applied Amount" have the same sign [Begin]
  
  *-- Calculate the new approved amount
  lnNAprAmnt = IIF(EMPTY(lcSize) , m.nAPTAprQty * m.nAPAprPric ,;
                   m.nAPAprAmnt - (nAPAprQty&lcSize * m.nAPAprPric) +;
                   (m.nAPAprQty&lcSize * m.nAPAprPric))
  
  *-- If the "Approved Amount" and the "Applied Amount" have
  *-- different signs.
  *-- Note that the "Applied Amount" have the same sign as the old
  *-- "Approved Amount".
  IF SIGN(lnNAprAmnt) <> 0 .AND.;
     SIGN(lnNAprAmnt) <> SIGN(nAPAprAmnt)
    
    *-- Message : 04185
    *-- "Invoice amount and approved amount must have the same sign."
    *-- Button : 00000 
    *-- "                          <    Ok    >                     "
    =gfModalGen('TRM04185B00000' , 'ALERT')
    
    RETURN (.F.)
  ENDIF    && End of IF SIGN(lnNAprAmnt) <> 0 .AND. ...
  *B602830,1 Add these lines to make sure that the "Approved Amount" [End]
  
  IF !EMPTY(lcSize) AND m.nApAprQty&lcSize < nApAprQty&lcSize-&lcAPInvTkt..nApAplQty&lcSize
    *E300798,1 Message : 04167
    *E300798,1 Approved Quantity Cannot be less than Contributed Quantity
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    =gfModalGen("TRM04167B00000","DIALOG",'quantity')
    RETURN (.F.)
  ENDIF
  IF m.nApTAprQty < nApTAprQty-&lcAPInvTkt..nApTAplQty
    *E300798,1 Message : 04167
    *E300798,1 Approved Quantity Cannot be less than Contributed Quantity
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    =gfModalGen("TRM04167B00000","DIALOG",'quantity')
    RETURN (.F.)
  ENDIF
  
  *B602830,1 Change this IF condition to give the user the capability to add
  *          debit memo lines, compare the absolute values [Begin]
  *IF m.nApTAprQty*m.nApAprPric < nApTAprQty*nApAprPric-&lcAPInvTkt..nApTAplQty*&lcAPInvTkt..nApAPlPric
  IF ABS(m.nApTAprQty * m.nApAprPric) < ABS(nApTAprQty * nApAprPric -;
     &lcAPInvTkt..nApTAplQty * &lcAPInvTkt..nApAPlPric)
  *B602830,1 Change this IF condition to give the user the capability [End]
    
    *E300798,1 Message : 04167
    *E300798,1 Approved amount Cannot be less than Contributed amount.
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    
    *B602830,1 Change this line to add the second variable part of the
    *          message [Begin]
    *=gfModalGen("TRM04167B00000","DIALOG",'amount')
    =gfModalGen("TRM04167B00000" , "DIALOG" , 'amount|amount')
    *B602830,1 Change this line to add the second variable part [End]
    
    RETURN (.F.)
  ENDIF
*-- MAN Added the else condition
*-- If there is no record with empty rec. session that means that amount is fully  
*-- contributed, therefore we have to check if the new approved amount is less
*-- than the current approved amount which match the contributed amount.
ELSE
  
  *B602830,1 Add this line to calculate the new approved amount [Begin]
  lnNAprAmnt = IIF(EMPTY(lcSize) , m.nAPTAprQty * m.nAPAprPric ,;
                   m.nAPAprAmnt - (nAPAprQty&lcSize * m.nAPAprPric) +;
                   (m.nAPAprQty&lcSize * m.nAPAprPric))
  *B602830,1 Add this line to compare the new approved amount [Begin]
  
  *B602830,1 Add these lines to make sure that the "Approved Amount" and
  *          the "Applied Amount" have the same sign [Begin]
  
  *-- If the "Approved Amount" and the "Applied Amount" have
  *-- different signs.
  *-- Note that the "Applied Amount" have the same sign as the old
  *-- "Approved Amount".
  IF SIGN(lnNAprAmnt) <> 0 .AND.;
     SIGN(lnNAprAmnt) <> SIGN(nAPAprAmnt)
    
    *-- Message : 04185
    *-- "Invoice amount and approved amount must have the same sign."
    *-- Button : 00000 
    *-- "                          <    Ok    >                     "
    =gfModalGen('TRM04185B00000' , 'ALERT')
    
    RETURN (.F.)
  ENDIF    && End of IF SIGN(lnNAprAmnt) <> 0 .AND. ...
  *B602830,1 Add these lines to make sure that the "Approved Amount" [End]
  
  *B602830,1 Change this line to compare the new approved amount with the
  *          old one and to give the user the capability to add debit memo
  *          lines, compare the absolute values [Begin]
  *IF EVAL("m."+VARREAD()) < EVAL(VARREAD())
  
  IF ABS(lnNAprAmnt) < ABS(nAPAprAmnt)
  *B602830,1 Change this line to compare the new approved amount [End]
    
    *B602830,1 Change this line to add the second variable part of the
    *          message [Begin]
    *=gfModalGen("TRM04167B00000","DIALOG",'amount')
    =gfModalGen("TRM04167B00000","DIALOG",'amount|amount')
    *B602830,1 Change this line to add the second variable part [End]
    
    RETURN (.F.)
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfUpdAplTkt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update Ticket Applied Quantity & Amount 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdAplTkt()
*!*************************************************************
FUNCTION lfUpdAplTkt
PRIVATE lnAlias

*?AMIN
IF EMPTY(&lcAPvInvDt..cTktNo) AND EMPTY(&lcAPvInvDt..ShipNo)
  RETURN
ENDIF

*MAN START
PRIVATE lnCurAls,lcPInvLNo,laContTot, llUpdTktL
lcPInvLNo = &lcAPvInvDt..cApVILNo
lnCurAls  = SELECT(0)
SELECT COUNT(*),SUM(IIF(!EMPTY(cRsession),nApAplAmnt,0)) ;
 FROM (lcAPInvTkt)             ;
WHERE cVendCode         + cApInvNo           + cApVILNo  + cRSession= ;
      PADR(laData[1],8) + PADR(laData[2],12) + lcPInvLNo + ""         ;
 INTO ARRAY laContTot

*-- No Contribution OR there is only one contributed line update the contribution.
*-- that has been contributed through the automatic option
*B607361,1 ALB Update the apinvtkt with the new amoount in case the cost = 0 [Begin]
*IF _TALLY <> 0 AND laContTot[1] = 1 AND SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo,lcAPInvTkt) ;
    AND !EMPTY(&lcAPInvTkt..cRSession)
IF _TALLY <> 0 AND laContTot[1] = 1 AND SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo,lcAPInvTkt) ;
    AND !EMPTY(&lcAPInvTkt..cRSession) AND !(&lcAPInvTkt..nApAplAmnt = 0 )

*B607361,1 ALB Update the apinvtkt with the new amoount in case the cost = 0 [End]

  llUpdTktL = .F.
ELSE  
  llUpdTktL = .T.
ENDIF  

*MAN END
*MAN Added IF llUpdTktL
IF llUpdTktL
  lnAlias = SELECT()
  SELECT (lcAPInvTkt)
  
  *B602830,1 Add these lines to restore the old uncontributed record, if there
  *          is one.
  *          Note: we will restore the old record for 2 purposes.
  *          1 - To minimize the size of the file
  *          2 - To be able to add the uncontributed record from the
  *          "Contribution" screen easily by restoring the old one. [Begin]
  lcSetDelet = SET('DELETED')        && Save the SET DELETED status
  SET DELETED OFF
  *B602830,1 Add these lines to restore the old uncontributed record [End]
  
  IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+&lcAPvInvDt..cApVILNo+SPACE(6))
    
    INSERT INTO (lcAPInvTkt) (cVendCode,cApInvNo,cApVILNo,cStatus,cIMTyp,cTktNo) VALUES  ;
    (laData[1],laData[2],&lcAPvInvDt..cApVILNo,'A',&lcAPvInvDt..cIMTyp,&lcAPvInvDt..cTktNo)
  
  *B602830,1 Add the else condition to restore the old uncontributed record,
  *          if there is one.
  *          Note: we will restore the old record for 2 purposes.
  *          1 - To minimize the size of the file
  *          2 - To be able to add the uncontributed record from the
  *          "Contribution" screen easily by restoring the old one. [Begin]
  ELSE    && Else (if there is uncontribitued amount line)
    *-- if the uncontribitued amount line is deleted
    IF DELETED()
      RECALL        && Recall the deleted record
      REPLACE cStatus WITH IIF(nRecNo = 0 , 'A' , 'M')
    ENDIF    && End of IF DELETED()
  *B602830,1 Add the else condition to restore the old uncontributed [End]
  
  ENDIF 
  
  *B602830,1 Add this line to restore the old uncontributed record, if there
  *          is one.
  *          Note: we will restore the old record for 2 purposes.
  *          1 - To minimize the size of the file
  *          2 - To be able to add the uncontributed record from the
  *          "Contribution" screen easily by restoring the old one. [Begin]
  SET DELETED &lcSetDelet        && Restore the old SET DELETED status
  *B602830,1 Add this lines to restore the old uncontributed record [End]
  
  *MAN
  *REPLACE LineNo     WITH m.LineNo     ,;
          Item       WITH m.Item       ,;
          Color      WITH m.Color      ,;
          cDyelot    WITH m.cDyelot    ,; 
          nApAplQty1 WITH nApAplQty1 - &lcAPvInvDt..nApAprQty1 + m.nApAprQty1 ,;
          nApAplQty2 WITH nApAplQty2 - &lcAPvInvDt..nApAprQty2 + m.nApAprQty2 ,;
          nApAplQty3 WITH nApAplQty3 - &lcAPvInvDt..nApAprQty3 + m.nApAprQty3 ,;
          nApAplQty4 WITH nApAplQty4 - &lcAPvInvDt..nApAprQty4 + m.nApAprQty4 ,;
          nApAplQty5 WITH nApAplQty5 - &lcAPvInvDt..nApAprQty5 + m.nApAprQty5 ,;
          nApAplQty6 WITH nApAplQty6 - &lcAPvInvDt..nApAprQty6 + m.nApAprQty6 ,;
          nApAplQty7 WITH nApAplQty7 - &lcAPvInvDt..nApAprQty7 + m.nApAprQty7 ,;
          nApAplQty8 WITH nApAplQty8 - &lcAPvInvDt..nApAprQty8 + m.nApAprQty8 ,;
          nApTAplQty WITH nApTAplQty - &lcAPvInvDt..nApTAprQty + m.nApTAprQty ,;
          nApAPlPric WITH m.nAPAprPric ,;
          nApAplAmnt WITH nApTAplQty*nApAPlPric ,;
          cStatus    WITH IIF(cStatus='S','M',cStatus)
  REPLACE LineNo     WITH m.LineNo     ,;
          Item       WITH m.Item       ,;
          Color      WITH m.Color      ,;
          cDyelot    WITH m.cDyelot    ,; 
          nApAplQty1 WITH nApAplQty1 - &lcAPvInvDt..nApAprQty1 + m.nApAprQty1 ,;
          nApAplQty2 WITH nApAplQty2 - &lcAPvInvDt..nApAprQty2 + m.nApAprQty2 ,;
          nApAplQty3 WITH nApAplQty3 - &lcAPvInvDt..nApAprQty3 + m.nApAprQty3 ,;
          nApAplQty4 WITH nApAplQty4 - &lcAPvInvDt..nApAprQty4 + m.nApAprQty4 ,;
          nApAplQty5 WITH nApAplQty5 - &lcAPvInvDt..nApAprQty5 + m.nApAprQty5 ,;
          nApAplQty6 WITH nApAplQty6 - &lcAPvInvDt..nApAprQty6 + m.nApAprQty6 ,;
          nApAplQty7 WITH nApAplQty7 - &lcAPvInvDt..nApAprQty7 + m.nApAprQty7 ,;
          nApAplQty8 WITH nApAplQty8 - &lcAPvInvDt..nApAprQty8 + m.nApAprQty8 ,;
          nApTAplQty WITH nApTAplQty - &lcAPvInvDt..nApTAprQty + m.nApTAprQty ,;
          nApAPlPric WITH m.nAPAprPric ,;
          nApAplAmnt WITH nApAplAmnt - &lcAPvInvDt..nApApRAmnt + m.nAPAprPric*m.nApTAprQty,;
          cStatus    WITH IIF(cStatus='S','M',cStatus)        

   *B605040,1 Amin Update the InvoiceTkt with the Gl account and the Dist. line [Start]    
   REPLACE cApdGlAct  WITH &lcAPvInvDt..cApDGlAct ,;
           cApdLinNo  WITH &lcAPvInvDt..cApVILNo
   *B605040,1 Amin Update the InvoiceTkt with the Gl account and the Dist. line [End]               
  
  *B602830,1 Add this line to delete the uncontributed record if the whole
  *          amount was contributed [Begin]
  IF nApAplAmnt = 0
    DELETE
    REPLACE cStatus WITH IIF(nRecNo = 0 , 'S' , 'D')
  ENDIF
  *B602830,1 Add this line to delete the uncontributed record [End]
  
  SELECT (lnAlias)
ENDIF  

*B602830,1 Change this IF condition to give the user the capability to add
*          debit memo lines, compare the absolute values [Begin]
*IF m.nApAprAmnt > 0 AND ;
*   IIF(&lcAPvInvDt..cIMTyp='S',!EMPTY(&lcAPvInvDt..ShipNO),!EMPTY(&lcAPvInvDt..cTktNo))
IF ABS(m.nApAprAmnt) > 0 AND ;
   IIF(&lcAPvInvDt..cIMTyp = 'S' , !EMPTY(&lcAPvInvDt..ShipNO) ,;
       !EMPTY(&lcAPvInvDt..cTktNo))

*B602830,1 Change this IF condition to give the user the capability [End]
  
  SHOW GET pbContrib ENABLE
ELSE
  SHOW GET pbContrib DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvContrib
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Contribute Invoice Line 
*!*************************************************************
*! Calls     : APMATST.SPR
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvContrib()
*!*************************************************************
FUNCTION lfvContrib

PRIVATE lnCMarker,lcCntrbBr,lnAplAmnt,lcPopStat,lcRemStat,;
        lcNewStat,laRSession,puRSession,lcInvLineNo

SELECT IIF(laScrMode[2],'APVINVDT',lcAPvInvDt)
lcLotNo    = cLotNo
lcTicket   = cTktNo
lcShipNo   = ShipNo
lcCstType  = cBomType
lcCstCatg  = cCostType
lcOprCode  = cOprCode
lcTktType  = CIMTYP
*MANAMIN
*lnAplAmnt  = nApAprAmnt
lcInvLineNo= cApVILNo
lnLineNo   = LineNO
*E301995,1 Alb Add Adorment order to invoice line [Begin]
*lcItem     = IIF(INLIST(lcTktType,'S','I','M','R'),Item,SUBSTR(Item,1,7)+Color)
lcItem     = IIF(INLIST(lcTktType,'S','I','M','R','A','D'),Item,SUBSTR(Item,1,7)+Color)
*E301995,1 Alb Add Adorment order to invoice line [End]
lnCMarker  = 0
lcCntrbBr  = 'Contributed Lines'
*E301995,1 Alb Add Adorment order to invoice line [Begin]
*llShowSize = llDefQtyDt AND INLIST(lcTktType,'S','I','M','R')
llShowSize = llDefQtyDt AND INLIST(lcTktType,'S','I','M','R','A','D')
*E301995,1 Alb Add Adorment order to invoice line [end]
*MANAMIN Start
lnRecNo= RECNO()
SUM nApAprAmnt TO lnAplAmnt FOR cvendcode+capinvno+capvilno = PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
*MANAMIN End

*B602830,1 Add this line to check if the quantity field is going to use
*          decimal points or not, the quantity field is going to use
*          decimal points in the case of document types Material PO
*          , Material PO Return and Material MFG Order [Begin]
llQtyUsDec = INLIST(lcTktType , 'L' , 'T' , 'F')
lcQtyFldPc = IIF(llQtyUsDec , '9999999.999' , '9999999')
*B602830,1 Add this line to check if the quantity field is going to use [End]

STORE 0 TO lnRecQty1,lnRecQty2,lnRecQty3,lnRecQty4,lnRecQty5,lnRecQty6,;
           lnRecQty7,lnRecQty8,lnTRecQty,lnRecCost,lnRecAmnt,;
           lnActQty1,lnActQty2,lnActQty3,lnActQty4,lnActQty5,lnActQty6,;
           lnActQty7,lnActQty8,lnTActQty,lnActCost,lnActAmnt
IF lcTktType = 'S'
  SET ORDER TO TAG BOMSHREC IN BOMLINE
ELSE
  SET ORDER TO TAG BOMPOREC IN BOMLINE
ENDIF  
*SELECT APInvTkt.cRSession,APInvTkt.Item,APInvTkt.Color,APInvTkt.cWareCode,;
*       APInvTkt.cDyelot,SUM(APInvTkt.nApAplQty1) AS nAplQty1,;
*       SUM(APInvTkt.nApAplQty2) AS nAplQty2,SUM(APInvTkt.nApAplQty3) AS nAplQty3,;
*       SUM(APInvTkt.nApAplQty4) AS nAplQty4,SUM(APInvTkt.nApAplQty5) AS nAplQty5,;
*       SUM(APInvTkt.nApAplQty6) AS nAplQty6,SUM(APInvTkt.nApAplQty7) AS nAplQty7,;
*       SUM(APInvTkt.nApAplQty8) AS nAplQty8,SUM(APInvTkt.nApTAplQty) AS nAplQty,;
*      SUM(APInvTkt.nApAplAmnt) AS nAplAmnt FROM APInvTkt,APVINVDT WHERE ;
*APVINVDT.cIMTyp+APVINVDT.cTktNo+APVINVDT.cLotNo+APVINVDT.cBomType+;
*APVINVDT.cOprCode+APVINVDT.Item+APVINVDT.Color+APVINVDT.cDyelot=;
*lcTktType+lcTicket+lcLotNo+lcCstType+lcOprCode AND ;
*APInvTkt.cvendcode+APInvTkt.capinvno+APInvTkt.capvilno+APInvTkt.crsession=;
*APVINVDT.cvendcode+APVINVDT.capinvno+APVINVDT.capvilno GROUP BY ;
*APInvTkt.cRSession,APInvTkt.Item,APInvTkt.Color,APInvTkt.cWareCode,APInvTkt.cDyelot ;
*INTO DBF (gcWorkDir+lcApplied)

*B121188,1 MHM 02/25/2004 Enhance Speed of Shipment and contripution in invoice line screen[Start]

*SELECT APInvTkt.cRSession,APInvTkt.Item,APInvTkt.Color,APInvTkt.cWareCode,;
*       APInvTkt.cDyelot,SUM(APInvTkt.nApAplQty1) AS nAplQty1,;
*       SUM(APInvTkt.nApAplQty2) AS nAplQty2,SUM(APInvTkt.nApAplQty3) AS nAplQty3,;
*       SUM(APInvTkt.nApAplQty4) AS nAplQty4,SUM(APInvTkt.nApAplQty5) AS nAplQty5,;
*       SUM(APInvTkt.nApAplQty6) AS nAplQty6,SUM(APInvTkt.nApAplQty7) AS nAplQty7,;
*       SUM(APInvTkt.nApAplQty8) AS nAplQty8,SUM(APInvTkt.nApTAplQty) AS nAplQty,;
*      SUM(APInvTkt.nApAplAmnt) AS nAplAmnt FROM APInvTkt,APVINVDT WHERE ;
*APVINVDT.cIMTyp+APVINVDT.cTktNo+APVINVDT.cLotNo+APVINVDT.cBomType+;
*APVINVDT.cOprCode+APVINVDT.Item+APVINVDT.Color+APVINVDT.cDyelot=;
*lcTktType+lcTicket+lcLotNo+lcCstType+lcOprCode AND ;
*APInvTkt.cvendcode+APInvTkt.capinvno+APInvTkt.capvilno+APInvTkt.crsession=;
*APVINVDT.cvendcode+APVINVDT.capinvno+APVINVDT.capvilno AND ;
*!EMPTY(APInvTkt.cRSession+APInvTkt.Item+APInvTkt.Color+APInvTkt.cWareCode) ;
*GROUP BY ;
*APInvTkt.cRSession,APInvTkt.Item,APInvTkt.Color,APInvTkt.cWareCode,APInvTkt.cDyelot ;
*INTO DBF (gcWorkDir+lcApplied)
*INDEX ON cRSession+Item+Color+cWareCode+cDyelot TAG (lcApplied)

SELECT APInvTkt.cRSession,APInvTkt.Item,APInvTkt.Color,APInvTkt.cWareCode,;
       APInvTkt.cDyelot,SUM(APInvTkt.nApAplQty1) AS nAplQty1,;
       SUM(APInvTkt.nApAplQty2) AS nAplQty2,SUM(APInvTkt.nApAplQty3) AS nAplQty3,;
       SUM(APInvTkt.nApAplQty4) AS nAplQty4,SUM(APInvTkt.nApAplQty5) AS nAplQty5,;
       SUM(APInvTkt.nApAplQty6) AS nAplQty6,SUM(APInvTkt.nApAplQty7) AS nAplQty7,;
       SUM(APInvTkt.nApAplQty8) AS nAplQty8,SUM(APInvTkt.nApTAplQty) AS nAplQty,;
      SUM(APInvTkt.nApAplAmnt) AS nAplAmnt FROM APInvTkt,APVINVDT WHERE ;
APVINVDT.cIMTyp+APVINVDT.cTktNo+APVINVDT.cLotNo+APVINVDT.cBomType+;
APVINVDT.cOprCode+APVINVDT.Item+APVINVDT.Color+APVINVDT.cDyelot=;
lcTktType+lcTicket+lcLotNo+lcCstType+lcOprCode AND ;
APInvTkt.cvendcode+APInvTkt.capinvno+APInvTkt.capvilno+APInvTkt.crsession=;
APVINVDT.cvendcode+APVINVDT.capinvno+APVINVDT.capvilno  ;
GROUP BY ;
APInvTkt.cRSession,APInvTkt.Item,APInvTkt.Color,APInvTkt.cWareCode,APInvTkt.cDyelot ;
INTO DBF (gcWorkDir+lcApplied)
INDEX ON cRSession+Item+Color+cWareCode+cDyelot TAG (lcApplied)
SELECT (lcApplied)
DELETE FOR EMPTY(cRSession+Item+Color+cWareCode) 

*B121188,1 MHM [End]

SELECT IIF(laScrMode[2],'APInvTkt',lcAPInvTkt)
SET RELATION TO cRSession+Item+Color+cWareCode+cDyelot INTO (lcApplied)
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
*IF INLIST(lcTktType,'S','I','M','R')
IF INLIST(lcTktType,'S','I','M','R','A','D')
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
  SET RELATION TO Item INTO Style ADDITIVE
ENDIF

DO CASE
  CASE lcTktType = 'S'
    lcFileInUse = 'POSLN'
    lcLnFldName = 'POSLN.nLan_Cst'
    lcAcFldName = 'POSLN.nAct_Cst'
    lcRcvQty    = 'POSLN.TotQty'
    SET ORDER TO TAG Poshrec IN POSLN
    
    SELECT * FROM POSLN WHERE shipno+cstytype+po+style+STR(lineno,6)+trancd =;
    lcShipNo+'P'+ALLTRIM(lcTicket) ;
    AND IIF(lnLineNo=0,.T.,style+STR(lineno,6)=lcItem+STR(lnLineNo,6)) AND;
    clotNo=lcLotNo INTO DBF (gcWorkDir+lcTmpTkt)
    INDEX ON PO+STR(LineNo,6)+cRSession+TranCd+cStyGrade TAG (lcTmpTkt)
    
    SELECT IIF(laScrMode[2],'APInvTkt',lcAPInvTkt)
    SET RELATION TO cTktNo+STR(LineNo,6)+cRSession+IIF(cRecvType='R','21',IIF(cRecvType='S','42','43')) INTO (lcTmpTkt) ADDITIVE 
    lcBrowQty = lcTmpTkt+'.TotQty'
    lcBrowCst = lcTmpTkt+'.nLan_Cst'
    
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  *CASE INLIST(lcTktType,'I','R')
  CASE INLIST(lcTktType,'I','R','A','D')
  *E301995,1 Alb Add Adorment order to invoice line [End]
    lcFileInUse = 'POSLN'
    lcLnFldName = 'POSLN.nLan_Cst'
    lcAcFldName = 'POSLN.nAct_Cst'
    lcRcvQty    = 'POSLN.TotQty'
    SET ORDER TO TAG Posrec IN POSLN
    SELECT * FROM POSLN WHERE cStyType+Po+Style+STR(LineNo,6)+TranCd =;
    IIF(lcTktType='I','P',lcTktType)+lcTicket+IIF(lnLineNo=0,'',lcItem+STR(lnLineNo,6)) AND;
    clotNo=lcLotNo INTO DBF (gcWorkDir+lcTmpTkt)
    INDEX ON STR(LineNo,6)+cRSession+TranCd+cStyGrade TAG (lcTmpTkt)
    SELECT IIF(laScrMode[2],'APInvTkt',lcAPInvTkt)
    SET RELATION TO STR(LineNo,6)+cRSession+IIF(cRecvType='R','21',IIF(cRecvType='S','42','43')) INTO (lcTmpTkt) ADDITIVE 
    lcBrowQty = lcTmpTkt+'.TotQty'
    lcBrowCst = lcTmpTkt+'.nLan_Cst'
  CASE lcTktType = 'M'
    lcFileInUse = 'CutTktL'
    lcLnFldName = 'CutTktL.nLan_Cst'
    lcAcFldName = 'CutTktL.nAct_Cst'
    lcRcvQty    = 'CutTktL.TotQty'
    SET ORDER TO TAG Cutrec IN CutTktL
    SELECT * FROM CutTktL WHERE Cuttkt+Style+STR(Lineno,6)+TranCd =;
    lcTicket+IIF(lnLineNo=0,'',lcItem+STR(lnLineNo,6)) ;
    INTO DBF (gcWorkDir+lcTmpTkt)
    INDEX ON STR(LineNo,6)+cRSession+TranCd+cStyGrade TAG (lcTmpTkt)
    SELECT IIF(laScrMode[2],'APInvTkt',lcAPInvTkt)
    SET RELATION TO STR(LineNo,6)+cRSession+IIF(cRecvType='R','21',IIF(cRecvType='S','42','43')) INTO (lcTmpTkt) ADDITIVE 
    lcBrowQty = lcTmpTkt+'.TotQty'
    lcBrowCst = lcTmpTkt+'.nLan_Cst'
  CASE INLIST(lcTktType,'L','F')
    lcFileInUse = 'POFLN'
    lcLnFldName = 'POFLN.nLan_Cost'
    lcAcFldName = 'POFLN.nAct_Cost'
    lcRcvQty    = 'POFLN.nFabTotQty'
    SET ORDER TO TAG POFREC IN POFLN
    SELECT * FROM POFLN WHERE cmattype+pomat+fabric+color+trancd =;
    IIF(lcTktType='L','P','R')+lcTicket+IIF(lnLineNo=0,'',lcItem) ;
    AND IIF(lnLineNo=0,.T.,LineNo = lnLineNo) ;
    INTO DBF (gcWorkDir+lcTmpTkt)
    INDEX ON STR(LineNo,6)+cRSession+TranCd+cFabGrade TAG (lcTmpTkt)
    SELECT IIF(laScrMode[2],'APInvTkt',lcAPInvTkt)
    SET RELATION TO STR(LineNo,6)+cRSession+IIF(cRecvType='R','21',IIF(cRecvType='S','42','43')) INTO (lcTmpTkt) ADDITIVE 
    lcBrowQty = lcTmpTkt+'.nFabTotQty'
    lcBrowCst = lcTmpTkt+'.nLan_Cost'
  CASE lcTktType = 'T'
    lcFileInUse = 'MMFGORDD'
    lcLnFldName = 'MMFGORDD.nLan_Cost'
    lcAcFldName = 'MMFGORDD.nAct_Cost'
    lcRcvQty    = 'MMFGORDD.nMfgTotQty'
    SELECT * FROM MMFGORDD WHERE cmfgordno+cfabric+color+dyelot+trancd =;
    lcTicket+IIF(lnLineNo=0,'',lcItem) ;
    AND IIF(lnLineNo=0,.T.,LineNo = lnLineNo) ;
    INTO DBF (gcWorkDir+lcTmpTkt)
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    *INDEX ON STR(LineNo,6)+cRSession+TranCd+cFabGrade TAG (lcTmpTkt)
    INDEX ON cRSession+cfabric+SPACE(12)+color+TranCd+cFabGrade TAG (lcTmpTkt)
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    SELECT IIF(laScrMode[2],'APInvTkt',lcAPInvTkt)
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    *SET RELATION TO STR(LineNo,6)+cRSession+IIF(cRecvType='R','21',IIF(cRecvType='S','42','43')) INTO (lcTmpTkt) ADDITIVE 
    SET RELATION TO cRSession+item+color+IIF(cRecvType='R','21',IIF(cRecvType='S','42','43')) INTO (lcTmpTkt) ADDITIVE
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    lcBrowQty = lcTmpTkt+'.nMfgTotQty'
    lcBrowCst = lcTmpTkt+'.nLan_Cost'
ENDCASE
IF laScrMode[2]
  SELECT DIST cRSession FROM APINVTKT ;
  WHERE cvendcode+capinvno+capvilno+crsession = ;
        PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo AND !EMPTY(crsession);
        INTO ARRAY laRSession
ELSE
  SELECT DIST cRSession FROM (lcAPInvTkt) ;
  WHERE cvendcode+capinvno+capvilno+crsession = ;
        PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo AND !EMPTY(crsession);
        INTO ARRAY laRSession
ENDIF
*B605967,1 RAE -- Include llClosed
*lcPopStat = IIF(_TALLY > 0,'ENABLE','DISABLE')
*lcRemStat = IIF(_TALLY > 0 AND !laScrMode[2],'ENABLE','DISABLE')
*lcNewStat = IIF(!laScrMode[2],'ENABLE','DISABLE')

lcPopStat = IIF(_TALLY > 0 .AND. !llClosed,'ENABLE','DISABLE')
lcRemStat = IIF(_TALLY > 0 .AND. !laScrMode[2] .AND. !llClosed,'ENABLE','DISABLE')
lcNewStat = IIF(!laScrMode[2] .AND. !llClosed,'ENABLE','DISABLE')
*B605967,1 RAE


IF _TALLY = 0
  DECLARE laRSession[1]
  STORE '' TO laRSession
ENDIF
STORE 1 TO puRSession
SET KEY TO
=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+SPACE(6))
lnCntrbAmnt = lnAplAmnt - IIF(laScrMode[2],APInvTkt.nApAplAmnt,&lcAPInvTkt..nApAplAmnt)
SET KEY TO PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+laRSession[puRSession]
=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+laRSession[puRSession])
SCATTER MEMVAR
DO (gcScrDir + gcWinAppl + '\APMATST.SPR')


USE IN (lcApplied)
SELECT IIF(laScrMode[2],'ApInvTkt',lcAPInvTkt)
SET RELATION TO 
SET KEY TO
IF !laScrMode[2]
  
  *B602830,1 Add these lines to take into consideration that the
  *          uncontributed record should be deleted if the amount is totally
  *          contributed. And so, if the amount was totally contributed we
  *          are going to delete the uncontributed record, and if not we
  *          will restore this record if it has been deleted [Begin]
  lcSetDelet = SET('DELETED')        && Save the SET DELETED status
  SET DELETED OFF
  *B602830,1 Add these lines to take into consideration [End]
  
  *B602830,1 Change these lines to add the uncontribution record,
  *          if there is non [Begin]
  *=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+SPACE(6))
  *REPLACE nApAplAmnt WITH lnAplAmnt - lnCntrbAmnt
  
  IF SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+SPACE(6))
    REPLACE nApAplAmnt WITH lnAplAmnt - lnCntrbAmnt
  ELSE
    INSERT INTO (lcAPInvTkt) (cVendCode,cApInvNo,cApVILNo,cStatus,cIMTyp,cTktNo) VALUES  ;
    (laData[1],laData[2],&lcAPvInvDt..cApVILNo,'A',&lcAPvInvDt..cIMTyp,&lcAPvInvDt..cTktNo)
    
    REPLACE LineNo     WITH &lcAPvInvDt..LineNo     ,;
            Item       WITH &lcAPvInvDt..Item       ,;
            Color      WITH &lcAPvInvDt..Color      ,;
            cDyelot    WITH &lcAPvInvDt..cDyelot    ,; 
            nApAplQty1 WITH &lcAPvInvDt..nApAprQty1 ,;
            nApAplQty2 WITH &lcAPvInvDt..nApAprQty2 ,;
            nApAplQty3 WITH &lcAPvInvDt..nApAprQty3 ,;
            nApAplQty4 WITH &lcAPvInvDt..nApAprQty4 ,;
            nApAplQty5 WITH &lcAPvInvDt..nApAprQty5 ,;
            nApAplQty6 WITH &lcAPvInvDt..nApAprQty6 ,;
            nApAplQty7 WITH &lcAPvInvDt..nApAprQty7 ,;
            nApAplQty8 WITH &lcAPvInvDt..nApAprQty8 ,;
            nApTAplQty WITH &lcAPvInvDt..nApTAprQty ,;
            nApAPlPric WITH &lcAPvInvDt..nAPAprPric ,;
            nApAplAmnt WITH lnAplAmnt - lnCntrbAmnt ,;
            cStatus    WITH IIF(cStatus='S','M',cStatus)        
  ENDIF
  *B602830,1 Change these lines to add the uncontribution record [End]
  
  *B602830,1 Add these lines to take into consideration that the
  *          uncontributed record should be deleted if the amount is totally
  *          contributed. And so, if the amount was totally contributed we
  *          are going to delete the uncontributed record, and if not we
  *          will restore this record if it has been deleted [Begin]
  *-- if the amount is totally contributed
  
  
  *605040,1 We should check the TmpDist file for Shipmnet [Start]
  
  IF lnAplAmnt = lnCntrbAmnt
    REPLACE cStatus WITH IIF(nRecNo = 0 , 'S' , 'D')
    DELETE        && Delete the uncontributed record
  ELSE    && Else (if the amount is partially contributed)
    RECALL        && Recall the uncontributed record
    REPLACE cStatus WITH IIF(nRecNo = 0 , 'A' , 'M')
  ENDIF    && End of IF lnAplAmnt = lnCntrbAmnt
  
  SET DELETED &lcSetDelet        && Restore the old SET DELETED status
  *B602830,1 Add these lines to take into consideration [End]
ENDIF

*B605040,1 In some case the Vendor field saved with blank value. [Start]
IF !laScrMode[2] 
  SELECT (lcAPvInvDt)
  SCAN FOR EMPTY(cVendCode) OR EMPTY(cApInvNo)
    REPLACE cVendCode WITH PADR(laData[1],8),;
             cApInvNo WITH PADR(laData[2],12)
  ENDSCAN
ENDIF
*B605040,1 In some case the Vendor code field saved with blank value [End]
SELECT IIF(laScrMode[2],'APVINVDT',lcAPvInvDt)
=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo)
SCATTER MEMVAR
=lfShowWind()

*!*************************************************************
*! Name      : lfDCntrbScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate Contribute Browse Screen
*!*************************************************************
*! Calls     : lpTabMove,lpBackMove
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDCntrbScr()
*!*************************************************************
FUNCTION lfDCntrbScr

IF WONTOP()=lcCntrbBr
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTabMove WITH IIF(llShowSize,'APMATST2','APMATST3'),'m.nAPAplPric'
  ON KEY LABEL BACKTAB DO lpBackMove WITH 'APMATST0','puRSession'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfBCntrbScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse Contribute Browse Screen
*!*************************************************************
*! Calls     : lfWCntrbScr(),lfwInvQty(),lfvInvQty(),lfwUnCst(),lfvUnCst()
*!             lfwItmAmnt(),lfvItmAmnt()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBCntrbScr()
*!*************************************************************
FUNCTION lfBCntrbScr

lnCMarker = RECNO()
lcCFields = "cMarker=IIF(RECNO()=lnCMarker,'>',' '):H=' ':R:1:W=.F.,"
*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
*B605040,1 Add WIP account to the browse in case of (S)hipment.  [Start]
*lcCFields = lcCFields +IIF(lcTktType='S',"cTktNo :R:H='PO#',",'')+IIF(INLIST(lcTktType,'I','S','M','R'),"Item :H=lcStyHdr :R,"
*lcCFields  = lcCFields +IIF(lcTktType='S',"cTktNo :R:H='PO#',",'');
                       +IIF(lcTktType='S',"cApdGlAct :R:H='WIP Account',",'');
                       +IIF(INLIST(lcTktType,'I','S','M','R'),"Item :H=lcStyHdr :R,",;
                       +"cItem=SUBSTR(Item,1,7) :H='Fabric' :R,Color :H='Color' :R,")
*B605040,1 Add WIP account to the browse in case of (S)hipment.  [End]
lcCFields  = lcCFields +IIF(lcTktType='S',"cTktNo :R:H='PO#',",'');
                       +IIF(lcTktType='S',"cApdGlAct :R:H='WIP Account',",'');
                       +IIF(INLIST(lcTktType,'I','S','M','R','A','D'),"Item :H=lcStyHdr :R,",;
                       +"cItem=SUBSTR(Item,1,7) :H='Fabric' :R,Color :H='Color' :R,")

*E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]

lcCFields = lcCFields +IIF(llWareHous,"cWareCode :H='Warehouse :R,",'')

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
lcCFields = lcCFields + lcBrowQty+" :H='Rec. Qty' :R,"+;
            lcBrowCst+lcCstType+" :H='Rec. Cost' :P='999999.999':R,"+;
            "nExtRcv="+lcBrowQty+'*'+lcBrowCst+lcCstType+" :H='Ex Rec Cst' :P='9999999.999':R,"

lcCFields = lcCFields + lcBrowQty+" :H=IIF(lcTktType$'RF','Issue Qty','Rec. Qty') :R,"+;
            lcBrowCst+lcCstType+" :H=IIF(lcTktType$'RF','Issue Cost','Rec. Cost') :P='999999.999':R,"+;
            "nExtRcv="+lcBrowQty+'*'+lcBrowCst+lcCstType+" :H=IIF(lcTktType$'RF','Ex Isse Cst','Ex Rec Cst') :P='9999999.999':R,"
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

*MAN Added Round 2 to nApAplAmnt, nAPlAmnt
lcCFields = lcCFields +"nApTAplQty :H='Inv. Qty.' :R,nApAplPric :H='Inv. Cst.' :R,nApAplAmnt=ROUND(nApAplAmnt,2) :H='Inv. Amnt.' :R,"
lcCFields = lcCFields + "nAPlAmnt=ROUND(&lcApplied..nAPlAmnt,2) :H='Act Cst' :P='9999999.999':R,;
           nConPer=nApAplAmnt/lnAplAmnt*100 :R :H='Con. %' :P = '999.99'"
BROWSE FIELDS &lcCFields ;
       WINDOW APMATST1   ;
       IN WINDOW APMATST ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfWCntrbScr();
       FOR !EMPTY(cRSession) ;
       TITLE lcCntrbBr
=lfWCntrbScr()

*!*************************************************************
*! Name      : lfWCntrbScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh Contribute Browse Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWCntrbScr()
*!*************************************************************
FUNCTION lfWCntrbScr

lnCMarker = RECNO()
SHOW WINDOW (lcCntrbBr) REFRESH SAME

lnTRecQty=&lcBrowQty
lnRecCost=EVAL(lcBrowCst+lcCstType)
*MAN Added Round 2
lnRecAmnt=ROUND(&lcBrowQty*EVAL(lcBrowCst+lcCstType),2)

lnTActQty=&lcApplied..nAplQty
lnActAmnt=&lcApplied..nAplAmnt
lnActCost=IIF(lnTActQty=0,0,lnActAmnt/lnTActQty)
SCATTER MEMVAR
IF llShowSize
  lnRecQty1=&lcTmpTkt..Qty1
  lnRecQty2=&lcTmpTkt..Qty2
  lnRecQty3=&lcTmpTkt..Qty3
  lnRecQty4=&lcTmpTkt..Qty4
  lnRecQty5=&lcTmpTkt..Qty5
  lnRecQty6=&lcTmpTkt..Qty6
  lnRecQty7=&lcTmpTkt..Qty7
  lnRecQty8=&lcTmpTkt..Qty8
  lnActQty1=&lcApplied..nAplQty1
  lnActQty2=&lcApplied..nAplQty2
  lnActQty3=&lcApplied..nAplQty3
  lnActQty4=&lcApplied..nAplQty4
  lnActQty5=&lcApplied..nAplQty5
  lnActQty6=&lcApplied..nAplQty6
  lnActQty7=&lcApplied..nAplQty7
  lnActQty8=&lcApplied..nAplQty8
  =lfRefresh('APMATST2')
ELSE
  =lfRefresh('APMATST3')
ENDIF
lcStatus=IIF(EMPTY(laRSession) OR laScrMode[2],'DISABLE','ENABLE')
SHOW GET m.nApAplQty1 &lcStatus
SHOW GET m.nApAplQty2 &lcStatus
SHOW GET m.nApAplQty3 &lcStatus
SHOW GET m.nApAplQty4 &lcStatus
SHOW GET m.nApAplQty5 &lcStatus
SHOW GET m.nApAplQty6 &lcStatus
SHOW GET m.nApAplQty7 &lcStatus
SHOW GET m.nApAplQty8 &lcStatus
SHOW GET m.nApAplQty1 &lcStatus

SHOW GET m.nApTAplQty &lcStatus
SHOW GET m.nApAplPric &lcStatus
SHOW GET m.nApAplAmnt &lcStatus

*!*************************************************************
*! Name      : lfvAplRSess
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh Contribute Browse Screen
*!*************************************************************
*! Calls     : lfWCntrbScr()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplRSess()
*!*************************************************************
FUNCTION lfvAplRSess

SET KEY TO PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+laRSession[puRSession]
=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+laRSession[puRSession])
LOCATE REST WHILE cVendCode+cApInvNo+cApVILNo+cRSession = ;
PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+laRSession[puRSession] ;
FOR !EMPTY(cRSession)
=lfWCntrbScr()

*!*************************************************************
*! Name      : lfvAplQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Contributed Quantity
*!*************************************************************
*! Calls     : gfModalGen(),lfRefresh()
*!*************************************************************
*! Passed Parameters  :  lcSize : Size
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplQty('1')
*!*************************************************************
FUNCTION lfvAplQty
PARAMETERS lcSize

IF !EMPTY(lcSize)
  m.nApTAplQty = m.nApTAplQty-nApAplQty&lcSize+m.nApAplQty&lcSize
ENDIF
IF m.nApTAplQty  <> nApTAplQty
  *E300798,1 Message : 04171
  *E300798,1 The invoice quantity Cannot be greater than the received Quantity
  *E300798,1 Button : 00000 
  *E300798,1 Ok

  *E300798,1 Message : 04172
  *E300798,1 Increasing the invoice quantity causes the total contributed
  *E300798,1 amount to be greater than the total cost
  *E300798,1 Button : 00000 
  *E300798,1 Ok

  *B602830,1 Change this IF condition to give the user the capability to
  *          add debit memo lines, compare the absolute values [Begin]
  *IF (m.nApTAplQty > &lcBrowQty AND ;
  *    gfModalGen('TRM04171B00000','ALERT','The invoice quantity|The received quantity.')=1) OR ;
  *   ((lnCntrbAmnt-nApAplAmnt+ m.nApTAplQty*nApAplPric) > lnAplAmnt AND ;
  *    gfModalGen('TRM04172B00000','ALERT','invoice quantity')=1)
  IF (m.nApTAplQty > &lcBrowQty AND ;
      gfModalGen('TRM04171B00000' , 'ALERT' ,;
                 'The invoice quantity|The received quantity.') = 1) OR ;
     (ABS(lnCntrbAmnt - nApAplAmnt + m.nApTAplQty * nApAplPric) >;
      ABS(lnAplAmnt) AND ;
      gfModalGen('TRM04172B00000' , 'ALERT' , 'invoice quantity') = 1)
  *B602830,1 Change this IF condition to give the user the capability [End]

    m.nApTAplQty = nApTAplQty
    IF !EMPTY(lcSize)
      m.nApAplQty&lcSize = nApAplQty&lcSize
    ENDIF
    RETURN
  ENDIF
  lnCntrbAmnt  = lnCntrbAmnt - nApAplAmnt + m.nApTAplQty*nApAplPric
  *B605911,1 ALB Fix the contribution process [Begin]
  IF cStatus = 'S'
    REPLACE cStatus WITH 'D'
    DELETE
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cStatus WITH 'A';
            nrecno WITH 0
  ENDIF
  *B605911,1 ALB Fix the contribution process [End]

  m.nApAplPric = IIF(m.nApTAplQty=0,0,m.nApAplPric)
  m.nApAplAmnt = m.nApTAplQty * m.nApAplPric
  REPLACE nApTAplQty WITH m.nApTAplQty ,;
          nApAplAmnt WITH m.nApAplAmnt ,;
          nApAplPric WITH m.nApAplPric
  IF !EMPTY(lcSize)
    REPLACE nApAplQty&lcSize WITH m.nApAplQty&lcSize
  ENDIF
  
  *B602946,1 Add these lines to update the status field to "Modified" in
  *          the temp. Invoice Detail file (lcAPvInvDt) if the user
  *          changed the contribution of the line [Begin]
  
  *-- If the status of the temp. Invoice Detail file (lcAPvInvDt) record
  *-- is "Same".
  IF &lcAPvInvDt..cStatus = 'S'
    REPLACE &lcAPvInvDt..cStatus WITH 'M'
  ENDIF
  *B602946,1 Add these lines to update the status field to "Modified" [End]
  
  =lfRefresh('APMATST0')
  =IIF(llShowSize,lfRefresh('APMATST2'),lfRefresh('APMATST3'))
  SHOW GET m.nApAplAmnt
  SHOW GET m.nApAplPric
  SHOW WINDOW (lcCntrbBr) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvAplPrice
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Contributed Cost
*!*************************************************************
*! Calls     : gfModalGen(),lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplPrice()
*!*************************************************************
FUNCTION lfvAplPrice

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
IF (m.CIMTYP $ 'RF' OR llDebitM) AND (m.nApAplPric > 0)
  m.nApAplPric = m.nApAplPric * -1
ENDIF
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]


IF m.nApAplPric <> nApAplPric
  *B602830,1 Add these lines to make sure that the "Approved Amount" and
  *          the "Applied Amount" have the same sign [Begin]
  
  *-- If the "Approved Amount" and the "Applied Amount" have different signs
  IF SIGN(m.nApAplPric) <> 0 .AND.;
     SIGN(m.nApAplPric) <> SIGN(&lcApVInvDt..nAPAprPric)
    
    *-- Message : 04185
    *-- "Approved amount and applied amount must have the same sign.   "
    *-- Button : 00000 
    *-- "                         <    Ok    >                         "
    =gfModalGen('TRM04185B00000' , 'ALERT')
    
    m.nApAplPric = nApAplPric        && Restore the old value
    RETURN
  ENDIF    && End of IF SIGN(m.nApAplPric) <> 0 .AND. ...
  *B602830,1 Add these lines to make sure that the "Approved Amount" [End]
  
  *B602830,1 Change this IF condition to give the user the capability to
  *          add debit memo lines, compare the absolute values [Begin]
  *IF (lnCntrbAmnt-nApAplAmnt+ nApTAplQty*m.nApAplPric) > lnAplAmnt
  *IF ABS(lnCntrbAmnt-nApAplAmnt+ nApTAplQty*m.nApAplPric) > ABS(lnAplAmnt)
  *B605911,1 ALB Fix the contribution process [Begin]
  lnOldCont = lnCntrbAmnt
  lnCntrbAmnt = lnCntrbAmnt - nApAplAmnt + nApTAplQty*m.nApAplPric
  *B605911,1 ALB Fix the contribution process [End]
  IF ABS(lnCntrbAmnt) - ABS(nApAplAmnt) + ABS(m.nApAplAmnt) > ABS(lnAplAmnt)
  *B602830,1 Change this IF condition to give the user the capability [End]
    
    *E300798,1 Message : 04172
    *E300798,1 Increasing the unit cost causes the total contributed
    *E300798,1 amount to be greater than the total cost
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    =gfModalGen('TRM04172B00000','ALERT','unit cost')
    m.nApAplPric = nApAplPric
    *B605911,1 ALB Fix the contribution process [Begin]
    lnCntrbAmnt = lnOldCont
    *B605911,1 ALB Fix the contribution process [End]
    RETURN
  ENDIF
  *B605911,1 ALB Fix the contribution process [Begin]
  *--Alb lnCntrbAmnt = lnCntrbAmnt - nApAplAmnt + nApTAplQty*m.nApAplPric
  IF cStatus = 'S'
    REPLACE cStatus WITH 'D'
    DELETE
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cStatus WITH 'A';
            nrecno WITH 0
  ENDIF
  *B605911,1 ALB Fix the contribution process [End]
  m.nApAplAmnt=m.nApTAplQty * m.nApAplPric
  REPLACE nApAplPric WITH m.nApAplPric,;
          nApAplAmnt WITH m.nApAplAmnt
  
  *B602946,1 Add these lines to update the status field to "Modified" in
  *          the temp. Invoice Detail file (lcAPvInvDt) if the user
  *          changed the contribution of the line [Begin]
  
  *-- If the status of the temp. Invoice Detail file (lcAPvInvDt) record
  *-- is "Same".
  IF &lcAPvInvDt..cStatus = 'S'
    REPLACE &lcAPvInvDt..cStatus WITH 'M'
  ENDIF
  *B602946,1 Add these lines to update the status field to "Modified" [End]
  
  =lfRefresh('APMATST0')
  SHOW GET m.nApAplAmnt
  SHOW WINDOW (lcCntrbBr) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvAplAMnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Contributed Amount
*!*************************************************************
*! Calls     : gfModalGen(),lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAplAMnt()
*!*************************************************************
FUNCTION lfvAplAMnt

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
IF (m.CIMTYP $ 'RF' OR llDebitM) AND (m.nApAplAmnt > 0)
  m.nApAplAmnt = m.nApAplAmnt * -1
ENDIF
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

IF m.nApAplAmnt <> nApAplAmnt
  
  *B602830,1 these lines to make sure that the "Approved Amount" and
  *          the "Applied Amount" have the same sign [Begin]
  
  *-- If the "Approved Amount" and the "Applied Amount" have different signs
  IF SIGN(m.nApAplAmnt) <> 0 .AND.;
     SIGN(m.nApAplAmnt) <> SIGN(&lcApVInvDt..nAPAprAmnt)
    
    *-- Message : 04185
    *-- "Approved amount and applied amount must have the same sign.   "
    *-- Button : 00000 
    *-- "                         <    Ok    >                         "
    =gfModalGen('TRM04185B00000' , 'ALERT')
    
    m.nApAplAmnt = nApAplAmnt        && Restore the old value
    RETURN
  ENDIF    && End of IF SIGN(m.nApAplAmnt) <> 0 .AND. ...
  *B602830,1 Add these lines to make sure that the "Approved Amount" [End]
  
  *B602830,1 Change this IF condition to give the user the capability to
  *          add debit memo lines, compare the absolute values [Begin]
  *IF (lnCntrbAmnt-nApAplAmnt+m.nApAplAmnt) > lnAplAmnt
  *aminamin2
  
  IF ABS(lnCntrbAmnt - nApAplAmnt + m.nApAplAmnt) > ABS(lnAplAmnt)
  *IF ABS(lnCntrbAmnt) > ABS(lnAplAmnt)
  *aminamin2
  *B602830,1 Change this IF condition to give the user the capability [End]
  
    *E300798,1 Message : 04123
    *E300798,1 The Total Contributed AMount Cannot be greater than the total cost
    *E300798,1 Button : 00000 
    *E300798,1 Ok
    =gfModalGen('TRM04123B00000','ALERT')
    m.nApAplAmnt = nApAplAmnt
    RETURN
  ENDIF
  lnCntrbAmnt = lnCntrbAmnt - nApAplAmnt+ m.nApAplAmnt
  * MAN Added the following line
  *B605911,1 ALB Fix the contribution process [Begin]
  IF cStatus = 'S'
    REPLACE cStatus WITH 'D'
    DELETE
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cStatus WITH 'A';
            nrecno WITH 0
  ENDIF
  *B605911,1 ALB Fix the contribution process [End]
  m.nApAplAmnt = m.nApAplAmnt * 1.000
  m.nApAplPric =IIF(nApTAplQty=0,0,m.nApAplAmnt/nApTAplQty)
  REPLACE nApAplAmnt WITH m.nApAplAmnt ,;
          nApAplPric WITH m.nApAplPric
  
  *B602946,1 Add these lines to update the status field to "Modified" in
  *          the temp. Invoice Detail file (lcAPvInvDt) if the user
  *          changed the contribution of the line [Begin]
  
  *-- If the status of the temp. Invoice Detail file (lcAPvInvDt) record
  *-- is "Same".
  IF &lcAPvInvDt..cStatus = 'S'
    REPLACE &lcAPvInvDt..cStatus WITH 'M'
  ENDIF
  *B602946,1 Add these lines to update the status field to "Modified" [End]
  
  =lfRefresh('APMATST0')
  SHOW GET m.nApAplPric
  SHOW WINDOW (lcCntrbBr) REFRESH SAME
ENDIF  

*!*************************************************************
*! Name      : lfvNewCntrb
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add new Contribution Session
*!*************************************************************
*! Calls       : APMATST5.SPR,lfWCntrbScr(),lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return     : None
*!*************************************************************
*! Example            :  =lfvNewCntrb()
*!*************************************************************
FUNCTION lfvNewCntrb
PRIVATE lcRSession,lnAmount,rbCntrb,lcContItem,lcContClr,lcMfgCode


lcContItem = &lcAPvInvDt..Item
lcContClr  = &lcAPvInvDt..Color
STORE SPACE(6) TO lcRSession
STORE lnAplAmnt - lnCntrbAmnt TO lnAmount
STORE 1 TO rbCntrb,lnCurObj

DO WHILE .T.
  llProceed=.F.
  DO (gcScrDir + gcWinAppl + '\APMATST5.SPR')
  IF llBrowse
    =lfvRSession(lcTktType,IIF(lcTktType='S',lcShipNo,lcTicket),.F.,lcContItem,lcContClr,lcCstType)
    lnCurObj = 3
    LOOP
  ENDIF
 
 
  IF llProceed
    IF EMPTY(lcRsession)
      *E300798,1 Message : 04066
      *E300798,1 You have to enter the Receiving Session#
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen('TRM04066B00000','ALERT','Receiving Session #')
      lnCurObj = 2
      LOOP
    ENDIF
    
    *B602830,1 Change this IF condition to give the user the capability to
    *          add debit memo lines, compare the absolute values [Begin]
    *IF lnAmount <= 0
    IF ABS(lnAmount) <= 0
    *B602830,1 Change this IF condition to give the user the capability [End]
      
      *E300798,1 Message : 04121
      *E300798,1 The cost amount must be greater
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen('TRM04121B00000','ALERT')
      lnCurObj = 3
      LOOP
    ENDIF
    
    *B602830,1 Add these lines to make sure that the "Approved Amount" and
    *          the "Applied Amount" have the same sign [Begin]
    
    *-- If the "Approved Amount" and the "Applied Amount" have
    *-- different signs.
    IF SIGN(lnAmount) <> SIGN(&lcAPvInvDt..nApAprAmnt)
      
      *-- Message : 04185
      *-- "Invoice amount and approved amount must have the same sign."
      *-- Button : 00000 
      *-- "                          <    Ok    >                     "
      =gfModalGen('TRM04185B00000' , 'ALERT')
      
      lnCurObj = 3
      LOOP
    ENDIF    && End of IF SIGN(lnNAprAmnt) <> 0 .AND. ...
    *B602830,1 Add these lines to make sure that the "Approved Amount" [End]
    
    *B602830,1 Change this IF condition to give the user the capability to
    *          add debit memo lines, compare the absolute values [Begin]
    *IF lnAmount+lnCntrbAmnt > &lcAPvInvDt..nApAprAmnt
    IF ABS(lnAmount+lnCntrbAmnt) > ABS(&lcAPvInvDt..nApAprAmnt)
    *B602830,1 Change this IF condition to give the user the capability [End]
      
      *E300798,1 Message : 04175
      *E300798,1 Applied Amount Cannot exceed Approved Amount
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      =gfModalGen('TRM04175B00000','ALERT')
      lnCurObj = OBJNUM(lnAmount)
      lnCurObj = 3
      LOOP
    ENDIF
    IF SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+lcRSession,lcAPInvTkt)
      *E300798,1 Message : 04127
      *E300798,1 This cost item has been applied to
      *E300798,1 Button : 00000 
      *E300798,1 Ok
      lcMsgTxt=laTktType[lnTktType,3]+' '+&lcAPvInvDt..cTktNo+' Session#: '+lcRSession
      =gfModalGen("TRM04127B00000","DIALOG",lcMsgTxt)
      lnCurObj = 2
      LOOP
    ENDIF  
    *B602654,1 Get PO style price for styles have no detail costing
    *IF !INLIST(lcTktType,'L','R','F') AND rbCntrb = 1 .AND. ;
    *  IIF(lcTktType='S',!SEEK('I2'+lcCstType+lcOprCode+lcshipNo+lcRSession,'BOMLINE'),;
    *  !SEEK(lcTktType+'2'+lcCstType+lcOprCode+lcTicket+lcRSession,'BOMLINE'))
    *  *E300798,1 Message : 04122
    *  *E300798,1 The entered cost type does not exist in the budget for this receiving.
    *  *E300798,1 You cannot make your contribution based on the budget cost.
    *  *E300798,1 Button : 00000 
    *  *E300798,1 Ok
    *  =gfModalGen("TRM04122B00000","DIALOG")
    *  lnCurObj = 4
    *  LOOP
    *ENDIF
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    *IF !INLIST(lcTktType,'L','R','F') AND rbCntrb = 1
    IF !INLIST(lcTktType,'L','R','F','A') AND rbCntrb = 1
    *E301995,1 Alb Add Adorment order to invoice line [enD]
      lcMfgCode = lcOprCode

      *B803999,1 Check if Style is not empty [Begin]
      *IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6)
      *  =SEEK(PADR(lcContItem,19),'STYLE')
      *  lcMfgCode=IIF(!STYLE.lDetCost,PADR('*1',6),lcOprCode)
      *ENDIF
      *IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6)
      *  =SEEK(PADR(lcContItem,19),'STYLE')
      *  lcMfgCode=IIF(!STYLE.lDetCost,PADR('*2',6),lcOprCode)
      *ENDIF

      IF EMPTY(lcContItem) 
        IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6) AND ;
          IIF(lcTktType='S',SEEK('I2'+lcCstType+PADR('*'+lcCstType,6)+lcshipNo+lcRSession,'BOMLINE') OR ;
             SEEK('I2'+lcCstType+'******'+lcshipNo+lcRSession,'BOMLINE'),;
             SEEK(lcTktType+'2'+lcCstType+PADR('*'+lcCstType,6)+lcTicket+lcRSession,'BOMLINE') OR ;
             SEEK(lcTktType+'2'+lcCstType+'******'+lcTicket+lcRSession,'BOMLINE'))
          lcMfgCode = PADR(BOMLINE.MFGCODE,6)
        ENDIF
        IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6) AND ;
          IIF(lcTktType='S',SEEK('I2'+lcCstType+PADR('*'+lcCstType,6)+lcshipNo+lcRSession,'BOMLINE') OR ;
             SEEK('I2'+lcCstType+'######'+lcshipNo+lcRSession,'BOMLINE'),;
             SEEK(lcTktType+'2'+lcCstType+PADR('*'+lcCstType,6)+lcTicket+lcRSession,'BOMLINE') OR ;
             SEEK(lcTktType+'2'+lcCstType+'######'+lcTicket+lcRSession,'BOMLINE'))
          lcMfgCode = PADR(BOMLINE.MFGCODE,6)
        ENDIF              
      ELSE
        IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6)
          =SEEK(PADR(lcContItem,19),'STYLE')
          lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
        ENDIF
        IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6)
          =SEEK(PADR(lcContItem,19),'STYLE')
          lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
        ENDIF      
      ENDIF  
      *B803999,1 Check if Style is not empty [End]
      IF IIF(lcTktType='S',!SEEK('I2'+lcCstType+lcMfgCode+lcshipNo+lcRSession,'BOMLINE'),;
        !SEEK(lcTktType+'2'+lcCstType+lcMfgCode+lcTicket+lcRSession,'BOMLINE'))
        *E300798,1 Message : 04122
        *E300798,1 The entered cost type does not exist in the budget for this receiving.
        *E300798,1 You cannot make your contribution based on the budget cost.
        *E300798,1 Button : 00000 
        *E300798,1 Ok
        =gfModalGen("TRM04122B00000","DIALOG")
        lnCurObj = 4
        LOOP
      ENDIF
    ENDIF
    *B602654,1 (End)
    
    *B602946,1 Add these lines to update the status field to "Modified" in
    *          the temp. Invoice Detail file (lcAPvInvDt) if the user
    *          changed the contribution of the line [Begin]
    
    *-- If the status of the temp. Invoice Detail file (lcAPvInvDt) record
    *-- is "Same".
    IF &lcAPvInvDt..cStatus = 'S'
      SELECT (lcAPvInvDt)
      REPLACE cStatus WITH 'M'
    ENDIF
    *B602946,1 Add these lines to update the status field to "Modified" [End]
    
    =lfvProCntrb()
  ENDIF
  RELEASE WINDOW 'APMATST5'
  EXIT
ENDDO
DO CASE
  CASE lcTktType = 'S'
    SET ORDER TO TAG Poshrec IN POSLN
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  *CASE INLIST(lcTktType,'I','R')
  CASE INLIST(lcTktType,'I','R','A','D')
  *E301995,1 Alb Add Adorment order to invoice line [End]
    SET ORDER TO TAG Posrec IN POSLN
  CASE lcTktType = 'M'
    SET ORDER TO TAG Cutrec IN CutTktL
  CASE INLIST(lcTktType,'L','F')
    SET ORDER TO TAG POFREC IN POFLN
  CASE lcTktType = 'T'
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    SET ORDER TO TAG MFGREC IN MMFGORDD
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
ENDCASE

SELECT DIST cRSession FROM (lcAPInvTkt) ;
WHERE cvendcode+capinvno+capvilno+crsession = ;
      PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo AND !EMPTY(crsession);
      INTO ARRAY laRSession
      
IF _TALLY = 0
  DECLARE laRSession[1]
  STORE '' TO laRSession
  STORE 1 TO puRSession
  SHOW GET puRSession DISABLE
ELSE
  STORE IIF(llProceed,ASCAN(laRSession,lcRSession),puRSession) TO puRSession
  SHOW GET puRSession ENABLE
  SHOW GET pbRemCntrb ENABLE
ENDIF
SELECT (lcAPInvTkt)
=lfvAplRSess()
=lfRefresh('APMATST0')

*!*************************************************************
*! Name      : lfvRemCntrb
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Proceed with new Contribution Session
*!*************************************************************
*! Calls       : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return     : None
*!*************************************************************
*! Example            :  =lfvRemCntrb()
*!*************************************************************
FUNCTION lfvRemCntrb
*E301995,1 Alb Add Adorment order to invoice line [Begin]
*PRIVATE lnApTAplQty,lnApAplAmnt,lnApAplQty1,lnApAplQty2,lnApAplQty3,;
        lnApAplQty4,lnApAplQty5,lnApAplQty6,lnApAplQty7,lnApAplQty8

PRIVATE lnTAplQty,lnAplAmnt,lnAplQty1,lnAplQty2,lnAplQty3,;
        lnAplQty4,lnAplQty5,lnAplQty6,lnAplQty7,lnAplQty8
*E301995,1 Alb Add Adorment order to invoice line [end]

IF .T.
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  *STORE 0 TO  lnApTAplQty,lnApAplAmnt,lnApAplQty1,lnApAplQty2,lnApAplQty3,;
              lnApAplQty4,lnApAplQty5,lnApAplQty6,lnApAplQty7,lnApAplQty8
  STORE 0 TO  lnTAplQty,lnAplAmnt,lnAplQty1,lnAplQty2,lnAplQty3,;
              lnAplQty4,lnAplQty5,lnAplQty6,lnAplQty7,lnAplQty8
  *E301995,1 Alb Add Adorment order to invoice line [end]

  SELECT (lcAPInvTkt)
  =SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+laRSession[puRSession])
  SCAN REST WHILE cVendCode+cApInvNo+cApVILNo+cRSession  = ;
   PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+laRSession[puRSession]
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
*    lnApTAplQty = lnApTAplQty + nApTAplQty
*    lnApAplAmnt = lnApAplAmnt + nApAplAmnt
*    lnApAplQty1 = lnApAplQty1 + nApAplQty1
*    lnApAplQty2 = lnApAplQty2 + nApAplQty2
*    lnApAplQty3 = lnApAplQty3 + nApAplQty3
*    lnApAplQty4 = lnApAplQty4 + nApAplQty4
*    lnApAplQty5 = lnApAplQty5 + nApAplQty5
*    lnApAplQty6 = lnApAplQty6 + nApAplQty6
*    lnApAplQty7 = lnApAplQty7 + nApAplQty7
*    lnApAplQty8 = lnApAplQty8 + nApAplQty8

    lnTAplQty = lnTAplQty + nApTAplQty
    lnAplAmnt = lnAplAmnt + nApAplAmnt
    lnAplQty1 = lnAplQty1 + nApAplQty1
    lnAplQty2 = lnAplQty2 + nApAplQty2
    lnAplQty3 = lnAplQty3 + nApAplQty3
    lnAplQty4 = lnAplQty4 + nApAplQty4
    lnAplQty5 = lnAplQty5 + nApAplQty5
    lnAplQty6 = lnAplQty6 + nApAplQty6
    lnAplQty7 = lnAplQty7 + nApAplQty7
    lnAplQty8 = lnAplQty8 + nApAplQty8
    *E301995,1 Alb Add Adorment order to invoice line [end]
    
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    
    *REPLACE cStatus WITH IIF(CSTATUS='A','S','D')
    REPLACE cStatus WITH IIF(!EMPTY(nrecno),'D','S')
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    DELETE
  ENDSCAN
  SET KEY TO

  =SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+SPACE(6))
  REPLACE nApAplQty1 WITH nApAplQty1 + lnAplQty1 ,;
          nApAplQty2 WITH nApAplQty2 + lnAplQty2 ,;
          nApAplQty3 WITH nApAplQty3 + lnAplQty3 ,;
          nApAplQty4 WITH nApAplQty4 + lnAplQty4 ,;
          nApAplQty5 WITH nApAplQty5 + lnAplQty5 ,;
          nApAplQty6 WITH nApAplQty6 + lnAplQty6 ,;
          nApAplQty7 WITH nApAplQty7 + lnAplQty7 ,;
          nApAplQty8 WITH nApAplQty8 + lnAplQty8 ,;
          nApTAplQty WITH nApTAplQty + lnTAplQty ,;
          nApAplAmnt WITH nApAplAmnt + lnAplAmnt ,;
          nApAPlPric WITH IIF(nApTAplQty=0,0,nApAplAmnt/nApTAplQty) ,;
          cStatus    WITH IIF(cStatus='S','M',cStatus)
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  *lnCntrbAmnt = lnCntrbAmnt - lnApAplAmnt
  lnCntrbAmnt = lnCntrbAmnt - lnAplAmnt
  *E301995,1 Alb Add Adorment order to invoice line [end]
  
  =ADEL(laRSession,puRSession)
  puRSession = 1

  *B605911,1 ALB Fix the contribution process [Begin]
  SELECT (lcAPVInvDt)
  llSeek = SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo)
  IF llSeek
    REPLACE cStatus WITH IIF(cStatus='S','M',cStatus)
  ENDIF
  SELECT (lcAPInvTkt)
  *B605911,1 ALB Fix the contribution process [End]
  IF ALEN(laRSession) > 1
    DECLARE laRSession[ALEN(laRSession)-1]
    SHOW GET puRSession ENABLE
  ELSE
    STORE '' TO laRSession
    SHOW GET puRSession DISABLE
    SHOW GET pbRemCntrb DISABLE
  ENDIF
  =lfRefresh('APMATST0')
  =lfvAplRSess()
ENDIF

*!*************************************************************
*! Name      : lfvProCntrb
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Proceed with new Contribution Session
*!*************************************************************
*! Calls       : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return     : None
*!*************************************************************
*! Example            :  =lfvProCntrb()
*!*************************************************************
FUNCTION lfvProCntrb
*B605040,1 [Start]
*PRIVATE lcMfgCode 
PRIVATE lcMfgCode , lcMfgTmp
*B605040,1 [End]

SELECT (lcAPInvTkt)
*B602654,1 Get PO style price for styles have no detail costing
lcMfgCode = lcOprCode

*B605040,1  Get distribution line no. from invoice tkt    [Start]
laData[52]= lfGetMaxLin()
*B605040,1  Get distribution line no. from invoice tkt    [End]

*B803999,1 Check if Style is not empty [Begin]
*IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6)
*  =SEEK(PADR(lcContItem,19),'STYLE')
*  lcMfgCode=IIF(!STYLE.lDetCost,PADR('*1',6),lcOprCode)
*ENDIF
*IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6)
*  =SEEK(PADR(lcContItem,19),'STYLE')
*  lcMfgCode=IIF(!STYLE.lDetCost,PADR('*2',6),lcOprCode)
*ENDIF

IF EMPTY(lcContItem) 
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6) AND ;
    IIF(lcTktType='S',SEEK('I2'+lcCstType+PADR('*'+lcCstType,6)+lcshipNo+lcRSession,'BOMLINE') OR ;
    SEEK('I2'+lcCstType+'******'+lcshipNo+lcRSession,'BOMLINE'),;
    SEEK(lcTktType+'2'+lcCstType+PADR('*'+lcCstType,6)+lcTicket+lcRSession,'BOMLINE') OR ;
    SEEK(lcTktType+'2'+lcCstType+'******'+lcTicket+lcRSession,'BOMLINE'))
    lcMfgCode = PADR(BOMLINE.MFGCODE,6)
  ENDIF
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6) AND ;
    IIF(lcTktType='S',SEEK('I2'+lcCstType+PADR('*'+lcCstType,6)+lcshipNo+lcRSession,'BOMLINE') OR ;
    SEEK('I2'+lcCstType+'######'+lcshipNo+lcRSession,'BOMLINE'),;
    SEEK(lcTktType+'2'+lcCstType+PADR('*'+lcCstType,6)+lcTicket+lcRSession,'BOMLINE') OR ;
    SEEK(lcTktType+'2'+lcCstType+'######'+lcTicket+lcRSession,'BOMLINE'))
    lcMfgCode = PADR(BOMLINE.MFGCODE,6)
  ENDIF              
ELSE
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('*',6)
    =SEEK(PADR(lcContItem,19),'STYLE')
    lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
  ENDIF
  IF INLIST(lcTktType,'S','I','M') AND lcOprCode=REPLICATE('#',6)
    =SEEK(PADR(lcContItem,19),'STYLE')
    lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
  ENDIF      
ENDIF  
*B803999,1 Check if Style is not empty [End]

*B602654,1 (End)
*B605911,1 ALB Fix the contribution process [Begin]
*--Alb Save old order in BOMLINE
lcBomOrd = ORDER('BOMLINE')
llSeekBom = .F.
*B605911,1 ALB Fix the contribution process [End]
DO CASE
  CASE lcTktType  = 'S'
    lcFileInUse = 'POSLN'
    SELECT POSLN
    SET ORDER TO TAG Poshrec
    *B605911,1 ALB Fix the contribution process  to save the invoice[Begin]
*    lcForCond   = "LOOKUP(POSHDR.STATUS,'P'+POSLN.PO,POSHDR.PO,'POSHDR') $ 'ACO';
                   AND Style=ALLTRIM(&lcAPvInvDt..Item) "
    lcForCond   = "LOOKUP(POSHDR.STATUS,'P'+BOMLINE.CTKTNO,POSHDR.PO,'POSHDR') $ 'ACO';
                   AND Style=ALLTRIM(&lcAPvInvDt..Item) AND crsession = lcRSession"
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
                 Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+style+sclr+cstygrade"
    lcBomKey  = 'I2'+lcCstType+lcMfgCode+lcShipNo
    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom =SEEK(lcBomKey)
    *B605911,1 ALB Fix the contribution process  to save the invoice[END]
    
    lcCondition = 'SHIPNO+CRSESSION+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD'
    lcKey       = lcShipNo+lcRSession
    
    *SET RELATION TO 'I2'+lcCstType+lcMfgCode+lcShipNo+CRSESSION+PO+;
    *                 STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE INTO BOMLINE
    
    lcItem   = 'PosLn.Style'
    lcGrade  = 'POSLN.cStyGrade'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'SPACE(10)'
    *B605040,1 Define variable holds the header file   [Start]
    lcHdrFile= 'POSHDR'
    lcHdrOred= 'Poshdr' 
    lcHdrKey = "P"
    
    *B605911,1 ALB Fix the contribution process  to save the invoice[Begin]
    *lcBomKey = 'I2'+lcCstType+lcMfgCode+lcShipNo+CRSESSION+PO+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE 
    *B605040,1 Define variable holds the header file   [End]    

  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  CASE INLIST(lcTktType,'A')
    lcFileInUse = 'POSLN'
    lcCondition = 'CSTYTYPE+PO+CRSESSION+SHIPNO+STYLE+STR(LINENO,6)+TRANCD'
    lcForCond   = ".T."
    SELECT POSLN
    SET ORDER TO TAG Posrec IN POSLN
    IF SEEK(lcTktType + lcTicket + lcRSession,'PosLn')
      lcShipNo = PosLn.ShipNo
    ENDIF
    lcKey      = lcTktType+lcTicket+lcRSession+lcShipNo+ALLTRIM(&lcAPvInvDt..Item)
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
                 Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+style+sclr+cstygrade"
    lcBomKey  = 'A3'+lcCstType+lcMfgCode+lcShipNo+lcRSession+lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                     "style = '"+&lcAPvInvDt..Item+"'")

    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom =SEEK(lcBomKey)
    lcItem   = 'PosLn.Style'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'SPACE(10)'
    lcGrade  = 'POSLN.cStyGrade'
    lcHdrFile= 'POSHDR'
    lcHdrOred= 'Poshdr'
    lcHdrKey = "A"

  CASE INLIST(lcTktType,'D')
    lcFileInUse = 'POSLN'
    lcCondition = 'CSTYTYPE+PO+CRSESSION+SHIPNO+STYLE+STR(LINENO,6)+TRANCD'
    lcForCond   = ".T."
    SELECT POSLN
    SET ORDER TO TAG Posrec IN POSLN
    IF SEEK(lcTktType + lcTicket + lcRSession,'PosLn')
      lcShipNo = PosLn.ShipNo
    ENDIF
    lcKey      = lcTktType+lcTicket+lcRSession+lcShipNo+ALLTRIM(&lcAPvInvDt..Item)
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
                 Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+style+sclr+cstygrade"
    lcBomKey  = 'D2'+lcCstType+lcMfgCode+lcShipNo+lcRSession+lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                     "style = '"+&lcAPvInvDt..Item+"'")

    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom =SEEK(lcBomKey)
    lcItem   = 'PosLn.Style'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'SPACE(10)'
    lcGrade  = 'POSLN.cStyGrade'
    lcHdrFile= 'POSHDR'
    lcHdrOred= 'Poshdr'
    lcHdrKey = "D"

  *E301995,1 Alb Add Adorment order to invoice line [End]
  CASE INLIST(lcTktType,'I','R')
    lcFileInUse = 'POSLN'
    lcCondition = 'CSTYTYPE+PO+CRSESSION+SHIPNO+STYLE+STR(LINENO,6)+TRANCD'

    *B803839,1 Change space(6) to lcShipNo that stores the ShipNo [Begin]
    *lcKey       = IIF(lcTktType='I','P','R')+lcTicket+lcRSession+SPACE(6)+ALLTRIM(&lcAPvInvDt..Item)
    *B803839,1 Change space(6) to lcShipNo that stores the ShipNo [End]
    
    lcForCond   = ".T."
    SELECT POSLN
    SET ORDER TO TAG Posrec IN POSLN

    *B803839,1 Get Value of Ship No to add to index [Begin]
    IF SEEK(IIF(lcTktType='I','P','R') + lcTicket + lcRSession,'PosLn')
      lcShipNo = PosLn.ShipNo
    ENDIF
    lcKey       = IIF(lcTktType='I','P','R')+lcTicket+lcRSession+lcShipNo+ALLTRIM(&lcAPvInvDt..Item)
    *B803839,1 Get Value of Ship No to add to index [End]

    *B605911,1 ALB Fix the contribution process  to save the invoice[Begin]
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
                 Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+style+sclr+cstygrade"
    lcBomKey  = 'I2'+lcCstType+lcMfgCode+lcShipNo+lcRSession+lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                     "style = '"+&lcAPvInvDt..Item+"'")

    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom =SEEK(lcBomKey)
    *B605911,1 ALB Fix the contribution process  to save the invoice[END]

    *SET RELATION TO 'I2'+lcCstType+lcMfgCode+PO+CRSESSION+SHIPNO+;
    *                 STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE INTO BOMLINE
    
    lcItem   = 'PosLn.Style'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'SPACE(10)'
    lcGrade  = 'POSLN.cStyGrade'

    *B605040,1 Define variable holds the header file    [Start]
    lcHdrFile= 'POSHDR'
    lcHdrOred= 'Poshdr'
    lcHdrKey = "P"
*-- Remark Alb  lcBomKey = 'I2'+lcCstType+lcMfgCode+PO+CRSESSION+SHIPNO+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE
    *B605040,1 Define variable holds the header file   [End]    
    
  CASE lcTktType = 'M'
    lcFileInUse = 'CUTTKTL'
    lcCondition = 'CRSESSION+CUTTKT+STYLE+TRANCD'
    lcKey       = lcRSession+lcTicket+ALLTRIM(&lcAPvInvDt..Item)
    lcForCond   = ".T."
    SELECT CUTTKTL
    SET ORDER TO TAG Cutrec IN CutTktL

    *B605911,1 ALB Fix the contribution process  to save the invoice[Begin]
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +Bomline.shipno +Bomline.crsession+Bomline.ctktno"
    lcBomKey  = 'M2'+lcCstType+lcMfgCode+lcShipNo + lcRSession + lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                         "style+sclr = '"+&lcAPvInvDt..Item+&lcAPvInvDt..Color+"'")
    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom = SEEK(lcBomKey)
    *B605911,1 ALB Fix the contribution process  to save the invoice[END]
    

    *SET RELATION TO 'M2'+lcCstType+lcMfgCode+CUTTKT+CRSESSION+SPACE(6)+;
    *                STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE INTO BOMLINE
    lcItem   = 'CUTTKTL.Style'
    lcColor  = 'SPACE(6)'
    lcDyelot = 'CUTTKTL.Dyelot'
    lcGrade  = 'CUTTKTL.cStyGrade'
    *B605040,1 Define variable holds the header file   [Start]
    lcHdrFile= 'CUTTKTH'
    lcHdrOred= 'Cuttkth' 
    lcHdrKey = ""
*---Alb  lcBomKey = 'M2'+lcCstType+lcMfgCode+CUTTKT+CRSESSION+SPACE(6)+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE 
    *B605040,1 Define variable holds the header file   [End]    

  CASE INLIST(lcTktType,'L','F')
    lcFileInUse = 'POFLN'
    SET ORDER TO TAG POFREC IN POFLN
    lcCondition = 'cMatType+PoMat+cRSession+Fabric+Color+Trancd'
    lcKey       = IIF(lcTktType='L','P','R')+lcTicket+lcRSession+IIF(EMPTY(&lcAPvInvDt..Item),'',;
                  SUBSTR(&lcAPvInvDt..Item,1,7)+&lcAPvInvDt..Color)
    *B605911,1 ALB Fix the contribution process [Begin]
    *lcForCond   = ".T."
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                         "Fabric+Color = '"+&lcAPvInvDt..Item+&lcAPvInvDt..Color+"'")
    *B605911,1 ALB Fix the contribution process [End]
    lcItem      = 'POFLN.Fabric'
    lcColor     = 'POFLN.Color'
    lcDyelot    = 'POFLN.Dyelot'
    lcGrade     = 'POFLN.cFabGrade'
    *B605040,1 Define variable holds the header file   [Start]
    lcHdrFile= 'POFHDR'
    lcHdrOred= 'Pofhdr' 
    lcHdrKey = IIF(lcTktType='L','P','R')
    lcBomKey = ""
    *B605040,1 Define variable holds the header file   [End]    
    
    
  CASE lcTktType = 'T'
  
    lcFileInUse = 'MMFGORDD'
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    *lcCondition = 'cmfgordno+cRSession+cfabric+color+dyelot+trancd'
    lcCondition = 'cRSession+cmfgordno+cfabric+color+dyelot+trancd'
    *lcKey       = lcTicket+lcRSession+IIF(EMPTY(&lcAPvInvDt..Item),'',;
                  SUBSTR(&lcAPvInvDt..Item,1,7)+&lcAPvInvDt..Color)
    lcKey       = lcRSession+lcTicket+IIF(EMPTY(&lcAPvInvDt..Item),'',;
                  SUBSTR(&lcAPvInvDt..Item,1,7)+&lcAPvInvDt..Color)
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    lcForCond   = ".T." 
    SELECT MMFGORDD
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    *SET ORDER TO TAG Cutrec IN MMFGORDD
    SET ORDER TO TAG MFGREC IN MMFGORDD
    lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +Bomline.shipno "+;
                 "+Bomline.crsession+Bomline.ctktno+STR(lineno,6)+style+sclr+cstygrade"
                 
    lcBomKey  = 'T2'+lcCstType+lcMfgCode+lcShipNo + lcRSession + lcTicket
    lcForCond = ".T. AND " +IIF(EMPTY(&lcAPvInvDt..Item),'.T.',;
                             "style+sclr = '"+&lcAPvInvDt..Item+&lcAPvInvDt..Color+"'")
    SELECT BOMLINE
    SET ORDER TO TAG Bomshrec
    llSeekBom = SEEK(lcBomKey)
    *B605911,1 ALB Fix the contribution process  to save the invoice[END]

    *SET RELATION TO 'T2'+lcCstType+lcMfgCode+cmfgordno+CRSESSION+SPACE(6)+;
    *                STR(0,6)+PADR(cfabric,19)+color+cFabGrade INTO BOMLINE
    lcItem      = 'MMFGORDD.cFabric'
    lcColor     = 'MMFGORDD.Color'
    lcDyelot    = 'MMFGORDD.Dyelot'
    lcGrade     = 'MMFGORDD.cFabGrade'
    *B605040,1 Define variable holds the header file   [Start]
    lcHdrFile= 'MMFGORDH'
    lcHdrOred= 'Mmfgordh' 
    lcHdrKey = ""
*--Alb    lcBomKey = 'T2'+lcCstType+lcMfgCode+cmfgordno+CRSESSION+SPACE(6)+STR(0,6)+PADR(cfabric,19)+color+cFabGrade 
    *B605040,1 Define variable holds the header file   [End]    

ENDCASE
*SELECT (lcFileInUse)
*=SEEK(lcKey)
lnTotBud   = 0
lnTItmAmt  = 0
lnTotActCt = 0          && Total Actual Cost
lnTotLanCt = 0          && Total Landed Cost

DO CASE
  *E300798,1 Contribution Based On Budget Cost
  CASE rbCntrb = 1
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    *IF INLIST(lcTktType,'L','F','R')
    IF INLIST(lcTktType,'L','F','R','A')
    *E301995,1 Alb Add Adorment order to invoice line [end]
      SELECT (lcFileInUse)
      =SEEK(lcKey)
      SUM REST &lcLnFldName&lcCstType*&lcRcvQty WHILE EVAL(lcCondition)=lcKey  ;
          FOR &lcForCond TO lnTotBud
      =SEEK(lcKey)
      *B605911,1 ALB Fix the contribution process [Begin]
      lcForCond = ".T. "
      *B605911,1 ALB Fix the contribution process [End]
      SCAN REST WHILE EVAL(lcCondition)=lcKey FOR &lcForCond
        lnTotLanCt = lnTotLanCt + &lcRcvQty * &lcLnFldName&lcCstType
        lnTotActCt = lnTotActCt + &lcRcvQty * &lcAcFldName&lcCstType
        
        *B602830,1 Remove this line to accumulate the value that was actually
        *          stored in the file and not the value that should be
        *          stored, to take into consideration the rounding
        *          problems [Begin]
        *lnTItmAmt  = lnTItmAmt  + IIF(lnTotBud=0,0,&lcLnFldName&lcCstType*&lcRcvQty/lnTotBud*lnAmount)
        *B602830,1 Remove this line to accumulate the value [End]
        
        SELECT (lcAPInvTkt)
        APPEND BLANK
        REPLACE cStatus    WITH 'A'          ,;
                cVendCode  WITH laData[1]    ,;
                cApInvNo   WITH laData[2]    ,;
                cRecvType  WITH IIF(&lcGrade='1','R',IIF(&lcGrade='2','S','D')) ,;
                cApVILNo   WITH lcInvLineNo  ,;
                cIMTyp     WITH lcTktType    ,;
                cTktNo     WITH lcTicket     ,;
                cRSession  WITH lcRSession   ,;
                LineNo     WITH EVAL(lcFileInUse+'.LineNo') ,;
                Item       WITH &lcItem      ,;
                Color      WITH &lcColor     ,;
                cDyelot    WITH &lcDyelot    ,;
                cWareCode  WITH EVAL(lcFileInUse+'.cWareCode') ,;
                nApTAplQty WITH &lcRcvQty  ,;
                nApAPlPric WITH IIF(lnTotBud=0,0,&lcLnFldName&lcCstType*lnAmount/lnTotBud)   ,;
                nApAplAmnt WITH IIF(lnTotBud=0,0,&lcLnFldName&lcCstType*&lcRcvQty/lnTotBud*lnAmount)
        

         *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [Start]
         * I put this cond. "lcTktType<>'T'" untill to confirm with Khalid about how we can get
         * the gl link code for MMFGORDD file. 
         laData[52] = laData[52]  + 1            
         REPLACE cApdGlAct WITH lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred) ;
                   cApdLinNo WITH STR(laData[52],4)
         *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [End]

         *B607278,1 ALB Get the correct WIP account with shipment ticket [Begin]
         REPLACE cApdGlAct WITH IIF(!EMPTY(&lcAPVInvDt..cAPDGLAct),&lcAPVInvDt..cAPDGLAct,lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred)) ;
                   cApdLinNo WITH STR(laData[52],4)
         *B607278,1 ALB Get the correct WIP account with shipment ticket [end]

        *B602830,1 Add this line to accumulate the value that was actually
        *          stored in the file and not the value that should be
        *          stored, to take into consideration the rounding
        *          problems [Begin]
        lnTItmAmt  = lnTItmAmt  + nApAplAmnt
        *B602830,1 Add this line to accumulate the value [End]
        
        *E301995,1 Alb Add Adorment order to invoice line [Begin]
        *IF lcTktType = 'R'
        IF INLIST(lcTktType,'R','A')
        *E301995,1 Alb Add Adorment order to invoice line [end]
          REPLACE nApAplQty1 WITH POSLN.Qty1 ,;
                  nApAplQty2 WITH POSLN.Qty2 ,;
                  nApAplQty3 WITH POSLN.Qty3 ,;
                  nApAplQty4 WITH POSLN.Qty4 ,;
                  nApAplQty5 WITH POSLN.Qty5 ,;
                  nApAplQty6 WITH POSLN.Qty6 ,;
                  nApAplQty7 WITH POSLN.Qty7 ,;
                  nApAplQty8 WITH POSLN.Qty8
        ENDIF  
      ENDSCAN
    ELSE
      *B605911,1 ALB Fix the contribution process  to save the invoice[Begin]
      IF !llSeekBom
        SET ORDER TO TAG &lcBomOrd IN BOMLINE
        =gfModalGen(.F.,'DIALOG',.F.,.F.,'There is no such cost lines to contribute')
        RETURN 
      ENDIF
      *SUM REST BOMLINE.ITEMAMT WHILE EVAL(lcCondition)= lcKey  ;
          FOR &lcForCond TO lnTotBud
      SUM REST BOMLINE.ITEMAMT WHILE EVAL(lcBomCond)= lcBomKey ;
          FOR &lcForCond TO lnTotBud
      IF INLIST(lcTktType,'I','R','T','M','D')
        lcForCond = ".T. "
      ELSE
        lcForCond = "LOOKUP(POSHDR.STATUS,'P'+POSLN.PO,POSHDR.PO,'POSHDR') $ 'ACO';
                       AND IIF(EMPTY(ALLTRIM(&lcAPvInvDt..Item)),.T.,Style=ALLTRIM(&lcAPvInvDt..Item)) AND crsession = lcRSession"
      ENDIF
      *B605911,1 ALB Fix the contribution process  to save the invoice[End]
      
      SELECT (lcFileInUse)
      llSeek = SEEK(lcKey)				&& add llseek for teacing
      SCAN REST WHILE EVAL(lcCondition)=lcKey FOR &lcForCond .AND. &lcRcvQty <> 0
        *B605040,1 Get the proper MfgCode for each style and move to the proper record in BOMLINE [Start]
        *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
        *=SEEK(&lcFileInUse..Style,'Style')
        =SEEK(&lcItem,'Style') 
        *B605911,1 ALB Fix the contribution process  to save the invoice [END]
        IF !Style.lDetCost
          lcMfgTmp = PADR('*'+lcCstType,6)
        ELSE
          IF laCostType[lnCostType,3]= "P"
            lcMfgTmp = REPLICATE('*',6)
          ENDIF
          IF laCostType[lnCostType,3]=  "D"
            lcMfgTmp = REPLICATE('#',6)
          ENDIF
          IF !(laCostType[lnCostType,3]  $ "PD")
            lcMfgTmp = lcMfgCode
          ENDIF
        ENDIF
        DO CASE
        CASE lcTktType  = 'S'
          lcBomKey = 'I2'+lcCstType+lcMfgTmp+lcShipNo+CRSESSION+PO+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE 
        *E301995,1 Alb Add Adorment order to invoice line [Begin]
        CASE INLIST(lcTktType,'A')
          lcBomKey = 'A3'+lcCstType+lcMfgTmp+SHIPNO+CRSESSION+lcticket+STR(LINENO,6)+STYLE+SPACE(6)
        CASE INLIST(lcTktType,'D')
          lcBomKey = 'D2'+lcCstType+lcMfgTmp+SHIPNO+CRSESSION+lcticket+STR(LINENO,6)+STYLE+SPACE(6)
        *E301995,1 Alb Add Adorment order to invoice line [End]
        CASE INLIST(lcTktType,'I','R')
          *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
          *lcBomKey = 'I2'+lcCstType+lcMfgTmp+PO+CRSESSION+SHIPNO+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE
          lcBomKey = 'I2'+lcCstType+lcMfgTmp+SHIPNO+CRSESSION+lcticket+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE
          *B605911,1 ALB Fix the contribution process  to save the invoice [END]
        CASE lcTktType = 'M'          
          *B605911,1 Alb [Begin]
          *lcBomKey = 'M2'+lcCstType+lcMfgTmp+CUTTKT+CRSESSION+SPACE(6)+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE
          lcBomKey = 'M2'+lcCstType+lcMfgTmp+SPACE(6)+CRSESSION+CUTTKT+STR(LINENO,6)+STYLE+SPACE(6)+CSTYGRADE
          *B605911,1 Alb [End]
        CASE INLIST(lcTktType,'L','F')          
          lcBomKey = ""
        CASE lcTktType = 'T'                    
          *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
          *lcBomKey = 'T2'+lcCstType+lcMfgTmp+cmfgordno+CRSESSION+SPACE(6)+STR(0,6)+PADR(cfabric,19)+color+cFabGrade
          lcBomKey = 'T2'+lcCstType+lcMfgCode+SPACE(6)+CRSESSION+cmfgordno+STR(LINENO,6)+PADR(cfabric,19)+color
          *B605911,1 ALB Fix the contribution process  to save the invoice [End]
      ENDCASE  
      
      llSeek =SEEK(lcBomKey, 'BOMLINE')			&& Alb add llseek for tracing
      *B605040,1 Get the proper MfgCode for each style and move to the proper record in BOMLINE [End]      
      SELECT (lcFileInUse)    
      IF !EOF('BOMLINE')
          m.UnitCost = IIF(lnTotBud=0,0,BomLine.ItemAmt/lnTotBud*lnAmount/&lcRcvQty)
          m.ItemAmt  = IIF(lnTotBud=0,0,BomLine.ItemAmt/lnTotBud*lnAmount)
          lnTotLanCt = lnTotLanCt + &lcRcvQty * EVAL(lcLnFldName+lcCstType)
          lnTotActCt = lnTotActCt + &lcRcvQty * EVAL(lcAcFldName+lcCstType)
          
          *B602830,1 Remove this line to accumulate the value that was
          *          actually stored in the file and not the value that
          *          should be stored, to take into consideration the
          *          rounding problems [Begin]
          *lnTItmAmt  = lnTItmAmt + m.ItemAmt
          *B602830,1 Remove this line to accumulate the value [End]
          
          SELECT (lcAPInvTkt)
          APPEND BLANK
          REPLACE cStatus    WITH 'A'        ,;
                  cVendCode  WITH laData[1]  ,;
                  cRecvType  WITH IIF(&lcGrade='1','R',IIF(&lcGrade='2','S','D')) ,;
                  cApInvNo   WITH laData[2]  ,;
                  cApVILNo   WITH lcInvLineNo,;
                  cRSession  WITH lcRSession ,;
                  LineNo     WITH IIF(lcTktType='T',0,&lcFileInUse..LineNo) ,;
                  cIMTyp     WITH lcTktType    ,;
                  cTktNo     WITH IIF(lcTktType='S',POSLN.PO,lcTicket)     ,;
                  Item       WITH BomLine.Style ,;
                  Color      WITH BomLine.SClr  ,;
                  cDyelot    WITH &lcDyelot  ,;
                  nApTAplQty WITH &lcRcvQty  ,;
                  nApAPlPric WITH m.UnitCost ,;
                  nApAplAmnt WITH m.ItemAmt  ,;
                  cWareCode  WITH &lcFileInUse..cWareCode
         *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [Start]
         laData[52] = laData[52]  + 1            
         REPLACE cApdGlAct WITH lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred) ;
                 cApdLinNo WITH STR(laData[52],4)
         *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [End]

         *B607278,1 ALB Get the correct WIP account with shipment ticket [Begin]
         REPLACE cApdGlAct WITH IIF(!EMPTY(&lcAPVInvDt..cAPDGLAct),&lcAPVInvDt..cAPDGLAct,lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred)) ;
                   cApdLinNo WITH STR(laData[52],4)
         *B607278,1 ALB Get the correct WIP account with shipment ticket [end]
          *B602830,1 Add this line to accumulate the value that was
          *          actually stored in the file and not the value that
          *          should be stored, to take into consideration the
          *          rounding problems [Begin]
          lnTItmAmt  = lnTItmAmt + nApAplAmnt
          *B602830,1 Add this line to accumulate the value [End]
          *E301995,1 Alb Add Adorment order to invoice line [Begin]
          *IF INLIST(lcTktType,'S','I','M')
          IF INLIST(lcTktType,'S','I','M','A','D','R')
          *E301995,1 Alb Add Adorment order to invoice line [End]
            REPLACE nApAplQty1 WITH &lcFileInUse..Qty1 ,;
                    nApAplQty2 WITH &lcFileInUse..Qty2 ,;
                    nApAplQty3 WITH &lcFileInUse..Qty3 ,;
                    nApAplQty4 WITH &lcFileInUse..Qty4 ,;
                    nApAplQty5 WITH &lcFileInUse..Qty5 ,;
                    nApAplQty6 WITH &lcFileInUse..Qty6 ,;
                    nApAplQty7 WITH &lcFileInUse..Qty7 ,;
                    nApAplQty8 WITH &lcFileInUse..Qty8
          
          ENDIF  
        ENDIF
      ENDSCAN
    ENDIF

  *E300798,1 Contribution Based On Received Quantity
  CASE rbCntrb = 2
    *B605911,1 ALB Fix the contribution process [Begin]
    SELECT (lcFileInUse)
    =SEEK(lcKey)
    IF !INLIST(lcTktType,'S')
      lcForCond = ".T. "
    ELSE
      lcForCond = "LOOKUP(POSHDR.STATUS,'P'+POSLN.PO,POSHDR.PO,'POSHDR') $ 'ACO';
                   AND IIF(EMPTY(ALLTRIM(&lcAPvInvDt..Item)),.T.,Style=ALLTRIM(&lcAPvInvDt..Item)) AND crsession = lcRSession"
    ENDIF
    *B605911,1 ALB Fix the contribution process [End]
    SUM REST &lcRcvQty WHILE EVAL(lcCondition)=lcKey FOR &lcForCond TO lnTotBud
*--Alb  SELECT (lcFileInUse)
    =SEEK(lcKey)
    SCAN REST WHILE EVAL(lcCondition)=lcKey FOR &lcForCond .AND. &lcRcvQty <> 0
      SELECT (lcAPInvTkt)
      APPEND BLANK
      *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
*--Alb      REPLACE cStatus    WITH 'A'           ,;
              cVendCode  WITH laData[1]     ,;
              cApInvNo   WITH laData[2]     ,;
              cApVILNo   WITH lcInvLineNo   ,;
              cIMTyp     WITH lcTktType     ,;
              cTktNo     WITH IIF(lcTktType='S',POSLN.PO,lcTicket) ,;
              cRSession  WITH lcRSession    ,;
              cRecvType  WITH IIF(&lcGrade='1','R',IIF(&lcGrade='2','S','D')) ,;
              LineNo     WITH IIF(m.cImTyp='T',0,&lcFileInUse..LineNo) ,;
              Item       WITH EVAL(lcItem)  ,;
              Color      WITH EVAL(lcColor) ,; 
              cDyelot    WITH EVAL(lcDyelot),;
              nApTAplQty WITH &lcRcvQty     ,;
              nApAPlPric WITH lnAmount/ lnTotBud ,;
              cWareCode  WITH &lcFileInUse..cWareCode ,;
              nApAplAmnt WITH &lcRcvQty*lnAmount/lnTotBud 

      REPLACE cStatus  WITH 'A'           ,;
            cVendCode  WITH laData[1]     ,;
            cApInvNo   WITH laData[2]     ,;
            cApVILNo   WITH lcInvLineNo   ,;
            cIMTyp     WITH lcTktType     ,;
            cTktNo     WITH IIF(lcTktType='S',POSLN.PO,lcTicket) ,;
            cRSession  WITH lcRSession    ,;
            cRecvType  WITH IIF(&lcGrade='1','R',IIF(&lcGrade='2','S','D')) ,;
            LineNo     WITH IIF(lcTktType='T',0,&lcFileInUse..LineNo) ,;
            Item       WITH EVAL(lcItem)  ,;
            Color      WITH EVAL(lcColor) ,; 
            cDyelot    WITH EVAL(lcDyelot),;
            nApTAplQty WITH &lcRcvQty     ,;
            nApAPlPric WITH lnAmount/ lnTotBud ,;
            cWareCode  WITH &lcFileInUse..cWareCode ,;
            nApAplAmnt WITH &lcRcvQty*lnAmount/lnTotBud 
      *B605911,1 ALB Fix the contribution process  to save the invoice [end]    
         *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [Start]
         laData[52] = laData[52]  + 1            
         REPLACE cApdGlAct WITH lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred) ;
                   cApdLinNo WITH  STR(laData[52],4)
        *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [End]

        *B607278,1 ALB Get the correct WIP account with shipment ticket [Begin]
        REPLACE cApdGlAct WITH IIF(!EMPTY(&lcAPVInvDt..cAPDGLAct),&lcAPVInvDt..cAPDGLAct,lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred)) ;
                 cApdLinNo WITH STR(laData[52],4)
        *B607278,1 ALB Get the correct WIP account with shipment ticket [end]
        
      *B602830,1 Add this line to accumulate the value that was
      *          actually stored in the file and not the value that
      *          should be stored, to take into consideration the
      *          rounding problems [Begin]
      lnTItmAmt  = lnTItmAmt  + nApAplAmnt
      *B602830,1 Add this line to accumulate the value [End]
      *E301995,1 Alb Add Adorment order to invoice line [Begin]
      *IF INLIST(lcTktType,'S','I','M','R')
      IF INLIST(lcTktType,'S','I','M','R','A','D')
      *E301995,1 Alb Add Adorment order to invoice line [End]
        REPLACE nApAplQty1 WITH &lcFileInUse..Qty1 ,;
                nApAplQty2 WITH &lcFileInUse..Qty2 ,;
                nApAplQty3 WITH &lcFileInUse..Qty3 ,;
                nApAplQty4 WITH &lcFileInUse..Qty4 ,;
                nApAplQty5 WITH &lcFileInUse..Qty5 ,;
                nApAplQty6 WITH &lcFileInUse..Qty6 ,;
                nApAplQty7 WITH &lcFileInUse..Qty7 ,;
                nApAplQty8 WITH &lcFileInUse..Qty8
      ENDIF
      lnTotLanCt = lnTotLanCt + &lcRcvQty * EVAL(lcLnFldName+lcCstType)
      lnTotActCt = lnTotActCt + &lcRcvQty * EVAL(lcAcFldName+lcCstType)
      
      *B602830,1 Remove this line to accumulate the value that was
      *          actually stored in the file and not the value that
      *          should be stored, to take into consideration the
      *          rounding problems [Begin]
      *lnTItmAmt  = lnTItmAmt  + IIF(lnTotBud=0,0,&lcRcvQty* lnAmount / lnTotBud)
      *B602830,1 Remove this line to accumulate the value [End]
      
    ENDSCAN

  *E300798,1 Contribute Manually
  CASE rbCntrb = 3
    SELECT (lcFileInUse)
*--Alb
    llSeek = SEEK(lcKey)				&&Alb
    IF !INLIST(lcTktType,'S')
      lcForCond = ".T. "
    ELSE
      lcForCond = "LOOKUP(POSHDR.STATUS,'P'+POSLN.PO,POSHDR.PO,'POSHDR') $ 'ACO';
                   AND IIF(EMPTY(ALLTRIM(&lcAPvInvDt..Item)),.T.,Style=ALLTRIM(&lcAPvInvDt..Item)) AND crsession = lcRSession"
    ENDIF
*--Alb
    SCAN REST WHILE EVAL(lcCondition)=lcKey FOR &lcForCond .AND. &lcRcvQty <> 0
      SELECT (lcAPInvTkt)
      APPEND BLANK
*--Alb
      *REPLACE cStatus    WITH 'A'       ,;
              cVendCode  WITH laData[1] ,;
              cApInvNo   WITH laData[2] ,;
              cApVILNo   WITH lcInvLineNo ,;
              cRSession  WITH lcRSession    ,;
              cRecvType  WITH IIF(&lcGrade='1','R',IIF(&lcGrade='2','S','D')) ,;
              LineNo     WITH IIF(m.cImTyp='T',0,&lcFileInUse..LineNo) ,;
              cIMTyp     WITH lcTktType     ,;
              cTktNo     WITH IIF(lcTktType='S',POSLN.PO,lcTicket)     ,;
              Item       WITH EVAL(lcItem)  ,;
              Color      WITH EVAL(lcColor) ,; 
              cDyelot    WITH EVAL(lcDyelot),;
              cWareCode  WITH &lcFileInUse..cWareCode ,;
              nApTAplQty WITH &lcRcvQty ,;
              nApAPlPric WITH 0 ,;
              nApAplAmnt WITH 0

      REPLACE cStatus  WITH 'A'           ,;
            cVendCode  WITH laData[1]     ,;
            cApInvNo   WITH laData[2]     ,;
            cApVILNo   WITH lcInvLineNo   ,;
            cIMTyp     WITH lcTktType     ,;
            cTktNo     WITH IIF(lcTktType='S',POSLN.PO,lcTicket) ,;
            cRSession  WITH lcRSession    ,;
            cRecvType  WITH IIF(&lcGrade='1','R',IIF(&lcGrade='2','S','D')) ,;
            LineNo     WITH IIF(lcTktType='T',0,&lcFileInUse..LineNo) ,;
            Item       WITH EVAL(lcItem)  ,;
            Color      WITH EVAL(lcColor) ,; 
            cDyelot    WITH EVAL(lcDyelot),;
            nApTAplQty WITH &lcRcvQty     ,;
            nApAPlPric WITH 0 ,;
            cWareCode  WITH &lcFileInUse..cWareCode ,;
            nApAplAmnt WITH 0 

*--Alb
      *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [Start]
      laData[52] = laData[52]  + 1            
      REPLACE cApdGlAct WITH lfGetWIPAc(lcHdrKey+IIF(lcTktType='S',POSLN.PO,lcTicket),lcHdrFile,lcHdrOred) ;
              cApdLinNo WITH STR(laData[52],4)
     *B605040,1 Uppdate invoicetkt file with the WIP account in case of (S)hipment.  [End]
      *E301995,1 Alb Add Adorment order to invoice line [Begin]
      *IF INLIST(lcTktType,'S','I','M','R')
      IF INLIST(lcTktType,'S','I','M','R','A','D')
      *E301995,1 Alb Add Adorment order to invoice line [End]
        REPLACE nApAplQty1 WITH &lcFileInUse..Qty1 ,;
                nApAplQty2 WITH &lcFileInUse..Qty2 ,;
                nApAplQty3 WITH &lcFileInUse..Qty3 ,;
                nApAplQty4 WITH &lcFileInUse..Qty4 ,;
                nApAplQty5 WITH &lcFileInUse..Qty5 ,;
                nApAplQty6 WITH &lcFileInUse..Qty6 ,;
                nApAplQty7 WITH &lcFileInUse..Qty7 ,;
                nApAplQty8 WITH &lcFileInUse..Qty8
      ENDIF  
      lnTotLanCt = lnTotLanCt + &lcRcvQty * EVAL(lcLnFldName+lcCstType)
      lnTotActCt = lnTotActCt + &lcRcvQty * EVAL(lcAcFldName+lcCstType)
    ENDSCAN
ENDCASE



*--Alb add function to return the act. cost that remove from the contrib. file
*B605911,1 ALB Fix the contribution process [Begin]
*IF lnTotActCt + lnAmount > lnTotLanCt
IF lfActCstRmv(.F.)+lnTotActCt + lnAmount - lfActCstRmv(.T.) > lnTotLanCt
*B605911,1 ALB Fix the contribution process [End]
  *E300798,1 Message : 04170
  *E300798,1 The actual cost for ð has exceeded the landed cost.
  *E300798,1 Button : 00000  
  *E300798,1 Ok
  =gfModalGen('QRM04170B00000','Dialog',ALLTRIM(laCostType[ASCAN(laCostType,lcCstType)-1]))
ENDIF
IF !INLIST(lcTktType,'L','F')
  SELECT (lcFileInUse)
  SET RELATION OFF INTO BOMLINE
ENDIF
SELECT (lcAPInvTkt)
SET KEY TO
*B602830,1 Change these lines to apply the over contributed amount to the
*          last contribution line and not to the uncontributed amount
*          record, if there is an over contributed amount due to rounding
*          problems [Begin]
*=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+SPACE(6))
*REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt

*-- If over contributed.
IF ABS(lnTItmAmt) > ABS(&lcApVInvDt..nAPAprAmnt)
  *-- Apply the difference to the last line
  *B604549,1 Correct the amount applied. [Begin] 
  *REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt
*MANAMIN
*  REPLACE nApAplAmnt WITH nApAplAmnt - (ABS(lnTItmAmt) - ABS(&lcApVInvDt..nAPAprAmnt))
REPLACE nApAplAmnt WITH nApAplAmnt - (ABS(lnTItmAmt) - ABS(lnAmount))
  *B604549,1 Correct the amount applied. [End] 

  *B604970,1 Apply the difference to the last line. [Begin]
*MANAMIN  
*  lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(&lcApVInvDt..nAPAprAmnt))  
  lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(lnAmount))  
  =SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+SPACE(6))
  REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt  
  *B604970,1 Apply the difference to the last line. [End]
  
ELSE    && Else (If not over contributed)
  *-- Apply the difference to the uncontributed amount record

  *B604970,1 Apply the difference to the last line. [Begin]
  IF ABS(lnTItmAmt) < ABS(&lcApVInvDt..nAPAprAmnt)
*MANAMIN  
*    REPLACE nApAplAmnt WITH nApAplAmnt - (ABS(lnTItmAmt) - ABS(&lcApVInvDt..nAPAprAmnt))  
    REPLACE nApAplAmnt WITH nApAplAmnt - (ABS(lnTItmAmt) - ABS(lnAmount))      
*MANAMIN  
*    lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(&lcApVInvDt..nAPAprAmnt))  
    lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(lnAmount))      
  ENDIF
  *B604970,1 Apply the difference to the last line. [End]
  =SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo+SPACE(6))
  REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt  
    
ENDIF    && End of IF ABD(lnTItmAmt) > ABS(nApAplAmnt)
*B602830,1 Change this line to apply the over contributed amount to [End]

*-- MAN is the full amount is contributed delete the record
IF nApAplAmnt = 0
  
  *B602830,1 Add this line to update the line status as deleted [Begin]
  REPLACE cStatus WITH IIF(nRecNo = 0 , 'S' , 'D')
  *B602830,1 Add this line to update the line status as deleted [End]
  DELETE
ENDIF
lnCntrbAmnt = lnCntrbAmnt + lnTItmAmt
*--Alb Restore the old order in BOMLINE
*B605911,1 ALB Fix the contribution process [Begin]
SET ORDER TO TAG &lcBomOrd IN BOMLINE
*B605911,1 ALB Fix the contribution process [End]
*!*************************************************************
*! Name      : lfItemQty
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Default Invoice line quantity and amount with ticket budget 
*!             quantity after deducting invoices applied to this ticket
*!*************************************************************
*! Calls       : lfGenInvLn(),ARIABROW()
*!*************************************************************
*! Passed Parameters  :  lcTktType  : Ticket Type
*!                       lcTicketNo : Ticket#
*!                       lcLotNo    : Lot#
*!                       lcCostType : Cost Type
*!                       lcMfgCode  : MFG Operation
*!                       lcItem     : Item
*!                       lcColor    : Color
*!                       lcDyelot   : Dyelot
*!*************************************************************
*! Return     : None
*!*************************************************************
*! Example            :  =lfItemQty()
*!*************************************************************
FUNCTION lfItemQty
PARAMETERS lcTktType,lcTicketNo,lcLotNo,lcCostType,lcMfgCode,lcItem,lcColor,lcDyelot
PRIVATE    lcInvLineNo,lcBrFields

lcInvLineNo = &lcAPvInvDt..cApVILNo
IF !lfGenInvLn(lcTktType,lcTicketNo,lcMfgCode,lcLotNo,SPACE(6),lcCostType,;
           lcItem,lcColor,lcDyelot)
  =lfGenInvLn(lcTktType,lcTicketNo,lcMfgCode,lcLotNo,SPACE(6),lcCostType,'','','')
  SELECT (lcAutoInv)
  lcBrFields = "nTktQty :H='Bud. Qty',nTktCst :H= 'Bud. Cost',nTktAmnt :H='Bud. Amnt',"
  lcBrFields = lcBrFields+"nInvQty :H='Inv. Qty.',nInvCst=IIF(nInvQty=0,0,nInvAmnt/nInvQty) :P='999999999.999' :H='Inv. Cost',nInvAmnt :H='Inv. Amnt.',"
  lcBrFields = lcBrFields+"nAplQty :H='Apl. Qty.',nAplCst=IIF(nAplQty =0,0,nAplAmnt/nAplQty ) :P='999999999.999' :H='Inv. Cost',nAplAmnt :H='Apl. Amnt.'"
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  *IF INLIST(lcTktType,'S','I','M','R')
  IF INLIST(lcTktType,'S','I','M','R','A','D')
  *E301995,1 Alb Add Adorment order to invoice line [End]
    lcBrFields = "Item :H=lcStyHdr,"+IIF(laSetups[1,2]='Y',"cDyelot :H='Dyelot',",'')+lcBrFields
  ELSE
    lcBrFields = "cItem=SUBSTR(Item,1,7) :H='Fabric',Color :H='Color',"+IIF(laSetups[2,2]='Y',"cDyelot :H='Dyelot',",'')+lcBrFields
  ENDIF
  IF !ARIABROW('','Ticket Items',gnBrHSRow1,;
                  gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','Item','laBrowArr')
    SELECT (lcAPvInvDt)
    =SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo)
    RETURN(.F.)
  ENDIF
ENDIF
STORE &lcAutoInv..Item    TO lcItem,m.Item
STORE &lcAutoInv..Color   TO lcColor,m.Color
STORE &lcAutoInv..cDyelot TO lcDyelot,m.cDyelot
SELECT (lcAPvInvDt)
=SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo)
IF lcItem+lcColor+lcDyelot = &lcAPvInvDt..Item+&lcAPvInvDt..Color+&lcAPvInvDt..cDyelot
  RETURN(.F.)
ENDIF
STORE &lcAutoInv..nInvQty  TO m.nAPTotQty
STORE &lcAutoInv..nInvAmnt TO m.nApAmnt
STORE &lcAutoInv..LineNo   TO m.LineNo
STORE IIF(m.nAPTotQty=0,0,m.nApAmnt/m.nAPTotQty) TO m.nAPPrice
*E301995,1 Alb Add Adorment order to invoice line [Begin]
*IF INLIST(lcTktType,'I','S','M','R')
IF INLIST(lcTktType,'I','S','M','R','A','D')
*E301995,1 Alb Add Adorment order to invoice line [End]
  STORE &lcAutoInv..nInvQty1 TO m.nAPQty1
  STORE &lcAutoInv..nInvQty2 TO m.nAPQty2
  STORE &lcAutoInv..nInvQty3 TO m.nAPQty3
  STORE &lcAutoInv..nInvQty4 TO m.nAPQty4
  STORE &lcAutoInv..nInvQty5 TO m.nAPQty5
  STORE &lcAutoInv..nInvQty6 TO m.nAPQty6
  STORE &lcAutoInv..nInvQty7 TO m.nAPQty7
  STORE &lcAutoInv..nInvQty8 TO m.nAPQty8
ENDIF
IF llDefAuApr
  STORE m.nAPTotQty TO m.nApTAprQty
  STORE m.nAPPrice  TO m.nAPAprPric
  STORE m.nApAmnt   TO m.nAPAprAmnt
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  *IF INLIST(lcTktType,'I','S','M','R')
  IF INLIST(lcTktType,'I','S','M','R','A','D')
  *E301995,1 Alb Add Adorment order to invoice line [End]
    STORE m.nAPQty1 TO m.nAPAprQty1
    STORE m.nAPQty2 TO m.nAPAprQty2
    STORE m.nAPQty3 TO m.nAPAprQty3
    STORE m.nAPQty4 TO m.nAPAprQty4
    STORE m.nAPQty5 TO m.nAPAprQty5
    STORE m.nAPQty6 TO m.nAPAprQty6
    STORE m.nAPQty7 TO m.nAPAprQty7
    STORE m.nAPQty8 TO m.nAPAprQty8
  ENDIF
ENDIF
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
m.nAPPrice   = m.nAPPrice   * IIF((m.CIMTYP $ 'RF') OR llDebitM ,-1,1)
m.nApAmnt    = m.nApAmnt    * IIF((m.CIMTYP $ 'RF') OR llDebitM ,-1,1)
m.nAPAprPric = m.nAPAprPric * IIF((m.CIMTYP $ 'RF') OR llDebitM ,-1,1)
m.nAPAprAmnt = m.nAPAprAmnt * IIF((m.CIMTYP $ 'RF') OR llDebitM ,-1,1)
*lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2)- ROUND(nAPAMnt,2))*IIF(m.CIMTYP $ 'RF' ,-1,1)
*lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))*IIF(m.CIMTYP $ 'RF',-1,1)
lnTInvAmt = lnTInvAmt + (ROUND(m.nAPAMnt,2)- ROUND(nAPAMnt,2))
lnTAprAmt = lnTAprAmt + (ROUND(m.nApAprAmnt,2)- ROUND(nApAprAmnt,2))
*=lfUpdApDist(cApVILNo,ROUND(m.nAPAmnt,2)-ROUND(nAPAmnt,2),cApDGlAct,lcTktType)
PRIVATE lnDiffAmnt
lnDiffAmnt =  (ROUND(m.nAPAMnt,2) - ROUND(nAPAMnt,2)) * IIF((m.CIMTYP $ 'RF'),1,IIF(llDebitM,-1,1))
=lfUpdApDist(cApVILNo,lnDiffAmnt,cApDGlAct,lcTktType)
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

=lfUpdAplTkt()
GATHER MEMVAR

*!*************************************************************
*! Name      : lfGenInvLn
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Select Document Lines to Invoice.
*!*************************************************************
*! Calls     : lfwAInvDt()
*!*************************************************************
*! Passed Parameters  :  lcTktType    : Ticket Type
*!                       lcTicket     : Ticket#
*!                       lcOperation : MFG Operation
*!                       lcLotNo      : Lot#
*!                       lcRSession   : Session#
*!                       lcCostType   : Cost Type
*!                       lcItem       : Item
*!                       lcColor      : Color
*!                       lcDyelot     : Dyelot
*!                       llDetails    : Shipment Details
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGenInvLn()
*!*************************************************************
FUNCTION lfGenInvLn
PARAMETERS lcTktType,lcTicket,lcOperation,lcLotNo,lcRSession,lcCostType,;
           lcItem,lcColor,lcDyelot,llDetails
PRIVATE    lnAlias,lcTktFile,lcTktItem,lcTktColor,lcTktDyelot,lcTktTotQty,;
           lnApQTy1,lnApQTy2,lnApQTy3,lnApQTy4,lnApQTy5,lnApQTy6,lnApQTy7,;
           lnApQTy8,lnApTotQty,lnApAmount,lnUnitCost,lcMfgCode

SET ORDER TO TAG BOMPOREC IN BOMLINE
lnAlias = SELECT()
SET ORDER TO TAG ITEM IN APVINVDT
lcRSession = PADR(lcRSession,6)
DO CASE
  *E301995,1 Alb Add Adorment order to invoice line [Begin]
  CASE INLIST(lcTktType,'A','D')  && Adorment/DYING order
    lcTktFile   = 'POSLN'
    lcFileTag   = 'POSREC'
    lcTktIdx    = 'cStyType+Po+cRSession+ShipNo+Style+STR(LineNo,6)+TranCd'
    lcTktKey    = lcTktType+lcTicket+lcRSession+SPACE(6)+ALLTRIM(lcItem)
    lcForCond   = IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '24'")
    lcTktItem   = 'POSLN.Style'
    lcTktColor  = 'SPACE(6)'
    lcTktDyelot = 'POSLN.Dyelot'
    lcTktTotQty = 'POSLN.TotQty'
    lcTktLine   = 'STR(POSLN.LINENO,6)'
    lcGrade     = 'POSLN.cStyGrade'
  *E301995,1 Alb Add Adorment order to invoice line [End]

  CASE INLIST(lcTktType,'I','R')  && Style PO & Style PO Returns
    lcTktFile   = 'POSLN'
    lcFileTag   = 'POSREC'
    lcTktIdx    = 'cStyType+Po+cRSession+ShipNo+Style+STR(LineNo,6)+TranCd'
    *B606696,1 ALB Check that the Document currency same as invoice currency[Begin]
    *lcTktKey    = IIF(lcTktType='I','P','R')+lcTicket+lcRSession+SPACE(6)+ALLTRIM(lcItem)
    lcTktKey    = IIF(lcTktType='I','P','R')+lcTicket+lcRSession
    *lcForCond   = IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '24'")
    lcForCond   = IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '24'") + ' AND Style = ALLTRIM(lcItem)'
    *B606696,1 ALB Check that the Document currency same as invoice currency[end]
    lcTktItem   = 'POSLN.Style'
    lcTktColor  = 'SPACE(6)'
    lcTktDyelot = 'POSLN.Dyelot'
    lcTktTotQty = 'POSLN.TotQty'
    lcTktLine   = 'STR(POSLN.LINENO,6)'
    lcGrade     = 'POSLN.cStyGrade'
  CASE lcTktType= 'S'              && Shipments
    lcTktFile   = 'POSLN'
    lcFileTag   = 'POSHREC'
    lcTktIdx    = 'ShipNo+cRSession+cStyType+Po+Style+STR(LineNo,6)+TranCd'
    lcTktKey    = lcTicket+IIF(EMPTY(lcRSession),'',lcRSession+'P')
    lcForCond   = '.T.' && IIF(EMPTY(lcRSession),"TranCd='3'","TranCd $ '24'")
    lcForCond   = lcForCond+IIF(EMPTY(lcItem),"","AND Style=lcItem")
    lcTktItem   = 'POSLN.Style'
    lcTktColor  = 'SPACE(6)'
    lcTktDyelot = 'POSLN.Dyelot'
    lcTktTotQty = 'POSLN.TotQty'
    lcTktLine   = 'STR(POSLN.LINENO,6)'
    lcGrade     = 'POSLN.cStyGrade'
  CASE lcTktType= 'M'               && Cutting Tickets
    lcTktFile   = 'CUTTKTL'
    lcFileTag   = 'CUTREC'
    lcTktIdx    = 'cRSession+cuttkt+Style+TranCd'
    lcTktKey    = lcRSession+lcTicket+ALLTRIM(lcItem)
    *B606754,1 MHM 09/13/2004 add TRANCD "3" Damged to For condition [Start]
    *lcForCond   = "Dyelot=ALLTRIM(lcDyelot) AND "+IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '24'")
    lcForCond   = "Dyelot=ALLTRIM(lcDyelot) AND "+IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '234'")
    *B606754,1 [End]
    
    lcTktItem   = 'CUTTKTL.Style'
    lcTktColor  = 'SPACE(6)'
    lcTktDyelot = 'CUTTKTL.Dyelot'
    lcTktTotQty = 'CUTTKTL.TotQty'
    lcTktLine   = 'STR(CUTTKTL.LINENO,6)'
    lcGrade     = 'CUTTKTL.cStyGrade'

  CASE lcTktType= 'T'                && Material Orders
    
    lcTktFile   = 'MMFGORDD'
    *Alb lcFileTag   = 'MMFGORDD'
    lcFileTag   = 'MFGREC'
    *B603371,1 SSE 03/20/2000 Modify the Scan condition [Begin]
    *lcTktKey    =  lcTicket
    lcTktKey    =  lcRSession+lcTicket + IIF(EMPTY(lcItem),"",SUBSTR(lcItem,1,7)+ALLTRIM(lcColor))
    *B603371,1 SSE 03/20/2000 [End]
        
   *Alb lcTktIdx    = 'cMfgOrdNo+cFabric+Color+Dyelot+TranCd'
    lcTktIdx    = 'cRSession+cMfgOrdNo+cFabric+Color+TranCd'
  *Alb  lcForCond   = "cRSession = lcRSession AND "+IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '24'")
    lcForCond   = "Dyelot=ALLTRIM(lcDyelot) AND "+IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '24'")
    lcTktItem   = 'MMFGORDD.cFabric'
    lcTktColor  = 'MMFGORDD.Color'
    lcTktDyelot = 'MMFGORDD.Dyelot'
    lcTktTotQty = 'MMFGORDD.nMfgTotQty'
    lcTktLine   = 'STR(0,6)'

    *B603371,1 SSE 03/20/2000 adding this missing variable [Begin]
    lcGrade     = 'MMFGORDD.cFabGrade'
    *B603371,1 SSE 03/20/2000 [End]
  CASE INLIST(lcTktType,'L','F')     && Material PO & Material PO Retuns
    lcTktFile   = 'POFLN'
    lcFileTag   = 'POFREC'
    lcTktIdx    = 'cmattype+pomat+crsession+fabric+color+trancd'
    lcTktKey    =  IIF(lcTktType='L','P','R')+lcTicket+lcRSession+IIF(EMPTY(lcItem),'',;
                   SUBSTR(lcItem,1,7)+ALLTRIM(lcColor))
    lcForCond   = "Dyelot=ALLTRIM(lcDyelot) AND "+IIF(EMPTY(lcRSession),"TranCd='1'","TranCd $ '24'")
    lcTktItem   = 'POFLN.Fabric'
    lcTktColor  = 'POFLN.Color'
    lcTktDyelot = 'POFLN.Dyelot'
    lcTktTotQty = 'POFLN.nfabtotqty'
    lcTktLine   = 'STR(POFLN.LINENO,6)'
    lcGrade     = 'POFLN.cFabGrade'
ENDCASE

SELECT (lcAutoInv)
DELETE ALL
IF EMPTY(lcLotNo)
  SELECT (lcTktFile)
  SET ORDER TO TAG (lcFileTag)
  =SEEK(lcTktKey)
  SCAN REST WHILE &lcTktIdx = lcTktKey FOR &lcForCond
    *E300798,1 Get invoiced amount applied againest this item
    SELECT APVINVDT
    =SEEK(lcTktType+lcTicket+lcLotNo+EVAL(lcTktLine)+lcCostType+lcOperation+;
          PADR(EVAL(lcTktItem),19)+EVAL(lcTktColor)+EVAL(lcTktDyelot))
    IF EMPTY(lcRSession)
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
      *SUM REST ;
      nApQty1,nApQty2,nApQty3,nApQty4,nApQty5,nApQty6,nApQty7,nApQty8,nApTotQty,nApTotQty*nApPrice;
      TO lnApQTy1,lnApQTy2,lnApQTy3,lnApQTy4,lnApQTy5,lnApQTy6,lnApQTy7,lnApQTy8,lnApTotQty,lnApAmount;
      WHILE cIMTyp+cTktNO+cLotNo+cBomType+cOprCode+Item+Color+cDyelot= ;
            lcTktType+lcTicket+SPACE(2)+lcCostType+lcOperation+;
            PADR(EVAL(lcTktItem),19)+EVAL(lcTktColor)+EVAL(lcTktDyelot)

      SUM REST IIF(nApPrice>0,nApQty1,-nApQty1),IIF(nApPrice>0,nApQty2,-nApQty2),IIF(nApPrice>0,nApQty3,-nApQty3),;
               IIF(nApPrice>0,nApQty4,-nApQty4),IIF(nApPrice>0,nApQty5,-nApQty5),IIF(nApPrice>0,nApQty6,-nApQty6),;
               IIF(nApPrice>0,nApQty7,-nApQty7),IIF(nApPrice>0,nApQty8,-nApQty8),IIF(nApPrice>0,nApTotQty,-nApTotQty),;
               nApTotQty*nApPrice TO lnApQTy1,lnApQTy2,lnApQTy3,lnApQTy4,lnApQTy5,lnApQTy6,lnApQTy7,lnApQTy8,lnApTotQty,lnApAmount;
      WHILE cIMTyp+cTktNO+cLotNo+cBomType+cOprCode+Item+Color+cDyelot= ;
            lcTktType+lcTicket+SPACE(2)+lcCostType+lcOperation+;
            PADR(EVAL(lcTktItem),19)+EVAL(lcTktColor)+EVAL(lcTktDyelot)
      *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
    ELSE
      STORE 0 TO lnApQTy1,lnApQTy2,lnApQTy3,lnApQTy4,lnApQTy5,lnApQTy6,;
                 lnApQTy7,lnApQTy8,lnApTotQty,lnApAmount
      SCAN REST WHILE cIMTyp+cTktNO+cLotNo+STR(LineNo,6)+cBomType+cOprCode+Item+Color+cDyelot= ;
                      lcTktType+lcTicket+SPACE(2)+EVAL(lcTktLine)+lcCostType+lcOperation+;
                      PADR(EVAL(lcTktItem),19)+EVAL(lcTktColor)+EVAL(lcTktDyelot)
        IF SEEK(cVendCode+cApInvNo+cApVILNo+lcRSession,'APINVTKT')
          SELECT APINVTKT
          SCAN REST WHILE cVendCode+cApInvNo+cApVILNo+cRSession = ;
                          &lcAPvInvDt..cVendCode+&lcAPvInvDt..cApInvNo+;
                          &lcAPvInvDt..cApVILNo+lcRSession
            lnApQTy1   = lnApQTy1   + nApAPlQty1
            lnApQTy2   = lnApQTy2   + nApAPlQty2
            lnApQTy3   = lnApQTy3   + nApAPlQty3
            lnApQTy4   = lnApQTy4   + nApAPlQty4
            lnApQTy5   = lnApQTy5   + nApAPlQty5
            lnApQTy6   = lnApQTy6   + nApAPlQty6
            lnApQTy7   = lnApQTy7   + nApAPlQty7
            lnApQTy8   = lnApQTy8   + nApAPlQty8
            lnApTotQty = lnApTotQty + nApTAplQty
            lnApAmount = lnApAmount + nApAPlAmnt
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
    *E300798,1 Get Ticket estimated amount or session received amount
    DO CASE
      CASE INLIST(lcTktType,'L','F')
        lnAmount = IIF(EMPTY(lcRSession),;
                         EVAL('POFLN.nCost'+lcCostType),;
                         EVAL('POFLN.nLan_Cost'+lcCostType))*&lcTktTotQty
      *E301995,1 Alb Add Adorment order to invoice line [Begin]
      CASE INLIST(lcTktType,'A','D')
        lnAmount = IIF(EMPTY(lcRSession),;
                       EVAL('POSLN.nCost'+lcCostType),;
                       EVAL('POSLN.nLan_Cst'+lcCostType))*&lcTktTotQty
      *E301995,1 Alb Add Adorment order to invoice line [End]
      CASE lcTktType='R'
        lnAmount = IIF(EMPTY(lcRSession),;
                       EVAL('POSLN.nCost'+lcCostType),;
                       EVAL('POSLN.nLan_Cst'+lcCostType))*&lcTktTotQty
      OTHERWISE
        *B602654,1 Get PO style price for styles have no detail costing
        *SELECT BOMLINE
        *=SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcOperation+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
              IIF(INLIST(lcTktType,'M','I','S'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcOperation+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
        IIF(INLIST(lcTktType,'M','I','S'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor

        lcMfgCode=lcOperation
        IF INLIST(lcTktType,'S','I','M') AND lcOperation=REPLICATE('*',6)   
          =SEEK(PADR(&lcTktItem,19),'STYLE')
          lcMfgCode=IIF(!STYLE.lDetCost,PADR('*1',6),lcOperation)
        ENDIF
        IF INLIST(lcTktType,'S','I','M') AND lcOperation=REPLICATE('#',6)
          =SEEK(PADR(&lcTktItem,19),'STYLE')
          lcMfgCode=IIF(!STYLE.lDetCost,PADR('*2',6),lcOperation)
        ENDIF
        SELECT BOMLINE
        *B607341,1 ALB Fix Bug happend by bug# B607161,1 and fix the original bug [BEGIN]
        *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
        *=SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
              IIF(INLIST(lcTktType,'M','I','S'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')
        *B607161,1 ASH 04/14/2003 (Begin) Don't use shipment number in seeking.
        *=SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,POSLN.SHIPNO)+;
              IIF(INLIST(lcTktType,'M','I','S','T'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')
        *=SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
              IIF(INLIST(lcTktType,'M','I','S','T'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')              
        *=SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType<> 'S' ,POSLN.SHIPNO,IIF(EMPTY(lcRSession),SPACE(6),lcTicket))+;
              IIF(INLIST(lcTktType,'M','I','S','T'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')

        *B129411,1 NNA 09/22/2005 (Begin) if the PO is matching to a shipment then get the Posln.Shipno not Space(6)
        *=SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
              IIF(INLIST(lcTktType,'M','I','S','T'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')              

        *B608263,1 WAM 09/10/2007 Fix error "Alias POSLN not found" when generate invoice lines automatically for a cutting ticket
        *=SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,IIF(!EMPTY(POSLN.SHIPNO),POSLN.SHIPNO,SPACE(6)))+;
              IIF(INLIST(lcTktType,'M','I','S','T'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')              
        
        =SEEK(IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
              lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
              lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,IIF(lcTktType='I',POSLN.SHIPNO,SPACE(6)))+;
              IIF(INLIST(lcTktType,'M','I','S','T'),;
              STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor,'BomLine')              
        *B608263,1 WAM 09/10/2007 (End)
  
        *B129411,1 NNA (End)
        
        *B607161,1 ASH 04/14/2003 (End)
        *B605612,1 SSH 02/28/2002 Fix the bug of calculate the amount depending on 
        *B605612,1 SSH            all style grade not first grade only.
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
        IIF(INLIST(lcTktType,'M','I','S'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor
    
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
        IIF(INLIST(lcTktType,'M','I','S'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+"1"
        *B605612,1 SSH[End]
        *B602654,1 (End)
        *B607161,1 ASH 04/14/2003 (Begin) Don't use shipment number in seeking.
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,POSLN.SHIPNO)+;
        IIF(INLIST(lcTktType,'M','I','S','T'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+IIF(lcTktType='T','','1')
        
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
        IIF(INLIST(lcTktType,'M','I','S','T'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+IIF(lcTktType='T','','1')        
        *B121306,1 NNA 01/22/2004 Fix Bug happend by bug# B607341,1 By Cancel his Fix (Begin)
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType<> 'S' ,POSLN.SHIPNO,IIF(EMPTY(lcRSession),SPACE(6),lcTicket))+;
        IIF(INLIST(lcTktType,'M','I','S','T'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+IIF(lcTktType='T','','1')
         
        *B606754,1 MHM 09/13/2004 add lcGrade to get allgrades( First Quality, Second Quality,Damaged) [Start]
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
        IIF(INLIST(lcTktType,'M','I','S','T'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+IIF(lcTktType='T','','1')        
        
        *B129411,1 NNA 09/22/2005 (Begin) if the PO is matching to a shipment then get the Posln.Shipno not Space(6)
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,SPACE(6))+;
        IIF(INLIST(lcTktType,'M','I','S','T'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+IIF(lcTktType='T','',&lcGrade)        

        *B608263,1 WAM 09/10/2007 Fix error "Alias POSLN not found" when generate invoice lines automatically for a cutting ticket
        *SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,IIF(!EMPTY(POSLN.SHIPNO),POSLN.SHIPNO,SPACE(6)))+;
        IIF(INLIST(lcTktType,'M','I','S','T'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+IIF(lcTktType='T','',&lcGrade)        

        SUM REST UnitCost*UnitQty*&lcTktTotQty TO lnAmount WHILE ;
        cimtyp+ctype+cbomtyp+mfgcode+ctktno+crsession+shipno+STR(lineno,6)+style+sclr+cstygrade=;
        IIF(lcTktType='S','I',lcTktType)+IIF(EMPTY(lcRSession),'1','2')+;
        lcCostType+lcMfgCode+IIF(lcTktType='S',POSLN.PO,lcTicket)+;
        lcRSession+IIF(lcTktType='S' AND !EMPTY(lcRSession),lcTicket,IIF(lcTktType='I',POSLN.SHIPNO,SPACE(6)))+;
        IIF(INLIST(lcTktType,'M','I','S','T'),;
        STR(&lcTktFile..LineNo,6),SPACE(6))+PADR(&lcTktItem,19)+&lcTktColor+IIF(lcTktType='T','',&lcGrade)        
        *B608263,1 WAM 09/10/2007 (End)

        *B129411,1 NNA (End)

        *B606754,1 [End]

        *B121306,1 NNA 01/22/2004  (End)      
        *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]
        *B607161,1 ASH 04/14/2003 (End)
        *B607341,1 ALB Fix Bug happend by bug# B607161,1 and fix the original bug [END]
        
    ENDCASE
    SELECT (lcAutoInv)
    *B605911,1 ALB 07/27/2002 Fix the contribution process  to save the invoice
    *IF !SEEK(&lcTktItem+&lcTktFile..cWareCode+&lcTktDyelot+IIF(lcTktType='S' AND llDetails,POSLN.PO,SPACE(6)))
    *B607161,1 ASH 04/14/2003 (Begin) Don't check if lnAmount <> 0.
    *IF lnAmount <> 0 .AND. !SEEK(&lcTktItem+&lcTktFile..cWareCode+&lcTktDyelot+IIF(lcTktType='S' AND llDetails,POSLN.PO,SPACE(6)))
    IF !SEEK(&lcTktItem+&lcTktFile..cWareCode+&lcTktDyelot+IIF(lcTktType='S' AND llDetails,POSLN.PO,SPACE(6)))
    *B607161,1 ASH 04/14/2003 (End)
    *B605911,1 ALB 07/27/2002 Fix the contribution process  to save the invoice
      APPEND BLANK
      *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
      *REPLACE cSelect   WITH "»"          ,;
              cTktNo    WITH IIF(lcTktType='S' AND llDetails,POSLN.PO,'') ,;
              cRecvType WITH IIF(EMPTY(lcRSession),'',IIF(&lcTktFile..TranCd='2','R',IIF(&lcGrade='2','S','D'))) ,;
              Item      WITH &lcTktItem   ,;
              Color     WITH &lcTktColor  ,;
              cDyelot   WITH &lcTktDyelot ,;
              cWareCode WITH &lcTktFile..cWareCode ,;
              LineNo    WITH IIF(lcTktType='T',0,&lcTktFile..LineNo)
      REPLACE cSelect   WITH IIF(lcTktType= 'S' .AND. SEEK("P"+POSLN.PO,"POSHDR") .AND. POSHDR.Status="S","","»"),;
              cTktNo    WITH IIF(lcTktType='S' AND llDetails,POSLN.PO,'') ,;
              cRecvType WITH IIF(EMPTY(lcRSession),'',IIF(&lcTktFile..TranCd='2','R',IIF(&lcGrade='2','S','D'))) ,;
              Item      WITH &lcTktItem   ,;
              Color     WITH &lcTktColor  ,;
              cDyelot   WITH &lcTktDyelot ,;
              cWareCode WITH &lcTktFile..cWareCode ,;
              LineNo    WITH IIF(lcTktType='T',0,&lcTktFile..LineNo)
      *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
      *AMINAMIN
      *IF lcTktType='S' AND !EMPTY(lcRSession) AND ;
      *  !EMPTY(cTktNo) AND EMPTY(&lcAutoInv..cWIPAcct) 
        
      *--Alb607278
      *IF lcTktType='S' AND !EMPTY(cTktNo) AND EMPTY(&lcAutoInv..cWIPAcct) 
      IF lcTktType='S' AND !EMPTY(cTktNo)
        SELECT POSHDR
        lcPosHdr = ORDER()
        lnPosRcNo = RECNO()
        SET ORDER TO TAG Poshdr 
        IF SEEK('P'+POSLN.PO,'POSHDR')
          *B605838,1 SSH Alwayes get the WIP account from the object of WIP account in the
          *B605838,1 SSH auto contribution screen.
          *REPLACE &lcAutoInv..cWIPAcct WITH lfvAASAct(POSHDR.LINK_CODE,"013")
          REPLACE &lcAutoInv..cWIPAcct WITH IIF(!EMPTY(lcApdglAct),lcApdglAct,lfvAASAct(POSHDR.LINK_CODE,"013"))
          *B605838,1 SSH [END]
        ENDIF
        SELECT POSHDR
        SET ORDER TO (lcPosHdr)
        IF BETWEEN(lnPosRcNo , 1, RECCOUNT() ) 
          GOTO lnPosRcNo 
        ENDIF 
        SELECT (lcAutoInv)
      ENDIF  
      *AMINAMIN              
    ENDIF
    REPLACE nTktQty   WITH nTktQty + &lcTktTotQty ,;
            nTktAmnt  WITH nTktAmnt+ lnAmount,;
            nTktCst   WITH IIF(nTktQty=0,0,nTktAmnt/nTktQty) ,;
            nAplQty   WITH nAplQty+lnApTotQty ,;
            nAplAmnt  WITH nAplAmnt+lnApAmount ,;
            nInvQty   WITH nInvQty+MAX(&lcTktTotQty-lnApTotQty,0),;
            nInvAmnt  WITH nInvAmnt+MAX(lnAmount-lnApAmount,0)
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    *IF INLIST(lcTktType,'I','S','M','R')
    IF INLIST(lcTktType,'I','S','M','R','A','D')
    *E301995,1 Alb Add Adorment order to invoice line [End]
      REPLACE nTktQty1 WITH nTktQty1+&lcTktFile..Qty1 ,;
              nTktQty2 WITH nTktQty2+&lcTktFile..Qty2 ,;
              nTktQty3 WITH nTktQty3+&lcTktFile..Qty3 ,;
              nTktQty4 WITH nTktQty4+&lcTktFile..Qty4 ,;
              nTktQty5 WITH nTktQty5+&lcTktFile..Qty5 ,;
              nTktQty6 WITH nTktQty6+&lcTktFile..Qty6 ,;
              nTktQty7 WITH nTktQty7+&lcTktFile..Qty7 ,;
              nTktQty8 WITH nTktQty8+&lcTktFile..Qty8 ,;
              nAplQty1 WITH nAplQty1+lnApQTy1  ,;
              nAplQty2 WITH nAplQty2+lnApQTy2  ,;
              nAplQty3 WITH nAplQty3+lnApQTy3  ,;
              nAplQty4 WITH nAplQty4+lnApQTy4  ,;
              nAplQty5 WITH nAplQty5+lnApQTy5  ,;
              nAplQty6 WITH nAplQty6+lnApQTy6  ,;
              nAplQty7 WITH nAplQty7+lnApQTy7  ,;
              nAplQty8 WITH nAplQty8+lnApQTy8
      REPLACE nInvQty1 WITH nInvQty1+MAX(&lcTktFile..Qty1-lnApQty1,0),;
              nInvQty2 WITH nInvQty2+MAX(&lcTktFile..Qty2-lnApQty2,0),;
              nInvQty3 WITH nInvQty3+MAX(&lcTktFile..Qty3-lnApQty3,0),;
              nInvQty4 WITH nInvQty4+MAX(&lcTktFile..Qty4-lnApQty4,0),;
              nInvQty5 WITH nInvQty5+MAX(&lcTktFile..Qty5-lnApQty5,0),;
              nInvQty6 WITH nInvQty6+MAX(&lcTktFile..Qty6-lnApQty6,0),;
              nInvQty7 WITH nInvQty7+MAX(&lcTktFile..Qty7-lnApQty7,0),;
              nInvQty8 WITH nInvQty8+MAX(&lcTktFile..Qty8-lnApQty8,0)
    ENDIF
  ENDSCAN
ELSE
  SELECT MFGOPRDT
  =SEEK(lcTktType+lcTicket+lcOperation+lcLotNo+'1')
  SCAN REST WHILE cIMTyp+cTktNo+cOprCode+cLotNo+TranCd = ;
            lcTktType+lcTicket+lcOperation+lcLotNo+'1' ;
       FOR Item+Color = lcItem+lcColor

    *E300798,1 Get invoiced amount applied againest this item to this lot
    SELECT APVINVDT
    =SEEK(lcTktType+lcTicket+lcLotNo)
    SUM REST nApQty1,nApQty2,nApQty3,nApQty4,nApQty5,nApQty6,nApQty7,nApQty8,nApTotQty,nApTotQty*nApPrice;
          TO lnApQTy1,lnApQTy2,lnApQTy3,lnApQTy4,lnApQTy5,lnApQTy6,lnApQTy7,lnApQTy8,lnApTotQty,lnApAmount;
    WHILE cIMTyp+cTktNO+cLotNo+STR(LineNO,6)+cBomType+cOprCode+Item+Color+cDyelot= ;
          lcTktType+lcTicket+lcLotNo ;
    FOR cBomType+cOprCode+Item+Color =;
        lcCostType+lcOperation+MFGOPRDT.Item+MFGOPRDT.Color

    *E300798,1 Get Lot estimated amount or session received amount
    *B602654,1 Get PO style price for styles have no detail costing
    *SELECT BomLine
    *=SEEK(lcTktType+'1'+lcCostType+lcOperation+lcTicket+lcRSession+SPACE(6))
    *LOCATE REST WHILE ;
    cIMTyp+cType+cBomTyp+MfgCode+cTktNo+cRSession+ShipNo+STR(LineNo,6)+Style+SClr=;
    lcTktType+'1'+lcCostType+lcOperation+lcTicket+lcRSession+SPACE(6) ;
    FOR Style+SClr = MFGOPRDT.Item+MFGOPRDT.Color
    lcMfgCode=lcOperation
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    *IF INLIST(lcTktType,'S','I','M') AND lcOperation=REPLICATE('*',6)
    IF INLIST(lcTktType,'S','I','M','A','D') AND lcOperation=REPLICATE('*',6)
    *E301995,1 Alb Add Adorment order to invoice line [End]
      =SEEK(PADR(&lcTktItem,19),'STYLE')
      lcMfgCode=IIF(!STYLE.lDetCost,PADR('*1',6),lcOperation)
    ENDIF
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    *IF INLIST(lcTktType,'S','I','M') AND lcOperation=REPLICATE('#',6)
    IF INLIST(lcTktType,'S','I','M','A','D') AND lcOperation=REPLICATE('#',6)
    *E301995,1 Alb Add Adorment order to invoice line [End]
      =SEEK(PADR(&lcTktItem,19),'STYLE')
      lcMfgCode=IIF(!STYLE.lDetCost,PADR('*2',6),lcOperation)
    ENDIF
    SELECT BomLine
    =SEEK(lcTktType+'1'+lcCostType+lcMfgCode+lcTicket+lcRSession+SPACE(6))
    LOCATE REST WHILE ;
    cIMTyp+cType+cBomTyp+MfgCode+cTktNo+cRSession+ShipNo+STR(LineNo,6)+Style+SClr=;
    lcTktType+'1'+lcCostType+lcMfgCode+lcTicket+lcRSession+SPACE(6) ;
    FOR Style+SClr = MFGOPRDT.Item+MFGOPRDT.Color
    *B602654,1 (End)

    SELECT (lcAutoInv)
    APPEND BLANK
    *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.
  
    *REPLACE cSelect  WITH "»" ,;
            Item     WITH MFGOPRDT.Item,;
            Color    WITH MFGOPRDT.Color,;
            nTktQty1 WITH MFGOPRDT.nLotQty1,;
            nTktQty2 WITH MFGOPRDT.nLotQty2,;
            nTktQty3 WITH MFGOPRDT.nLotQty3,;
            nTktQty4 WITH MFGOPRDT.nLotQty4,;
            nTktQty5 WITH MFGOPRDT.nLotQty5,;
            nTktQty6 WITH MFGOPRDT.nLotQty6,;
            nTktQty7 WITH MFGOPRDT.nLotQty7,;
            nTktQty8 WITH MFGOPRDT.nLotQty8,;
            nTktQty  WITH MFGOPRDT.nLotTotQty,;
            nTktCst  WITH BomLine.UnitCost ,;
            nTktAmnt WITH MFGOPRDT.nLotTotQty*BomLine.UnitCost,;
            nAplQty1 WITH lnApQTy1  ,;
            nAplQty2 WITH lnApQTy2  ,;
            nAplQty3 WITH lnApQTy3  ,;
            nAplQty4 WITH lnApQTy4  ,;
            nAplQty5 WITH lnApQTy5  ,;
            nAplQty6 WITH lnApQTy6  ,;
            nAplQty7 WITH lnApQTy7  ,;
            nAplQty8 WITH lnApQTy8  ,;
            nAplQty  WITH lnApTotQty ,;
            nAplAmnt WITH lnApAmount ,;
            nInvQty1 WITH MAX(MFGOPRDT.nLotQty1-lnApQty1,0),;
            nInvQty2 WITH MAX(MFGOPRDT.nLotQty2-lnApQty2,0),;
            nInvQty3 WITH MAX(MFGOPRDT.nLotQty3-lnApQty3,0),;
            nInvQty4 WITH MAX(MFGOPRDT.nLotQty4-lnApQty4,0),;
            nInvQty5 WITH MAX(MFGOPRDT.nLotQty5-lnApQty5,0),;
            nInvQty6 WITH MAX(MFGOPRDT.nLotQty6-lnApQty6,0),;
            nInvQty7 WITH MAX(MFGOPRDT.nLotQty7-lnApQty7,0),;
            nInvQty8 WITH MAX(MFGOPRDT.nLotQty8-lnApQty8,0),;
            nInvQty  WITH MAX(MFGOPRDT.nLotTotQty-lnApTotQty,0),;
            nInvAmnt WITH MAX(MFGOPRDT.nLotTotQty*BomLine.UnitCost-lnApAmount,0)

    REPLACE cSelect  WITH IIF(lcTktType= 'S' .AND. SEEK("P"+MFGOPRDT.cTktNo,"POSHDR") .AND. POSHDR.Status="S","","»"),;
            Item     WITH MFGOPRDT.Item,;
            Color    WITH MFGOPRDT.Color,;
            nTktQty1 WITH MFGOPRDT.nLotQty1,;
            nTktQty2 WITH MFGOPRDT.nLotQty2,;
            nTktQty3 WITH MFGOPRDT.nLotQty3,;
            nTktQty4 WITH MFGOPRDT.nLotQty4,;
            nTktQty5 WITH MFGOPRDT.nLotQty5,;
            nTktQty6 WITH MFGOPRDT.nLotQty6,;
            nTktQty7 WITH MFGOPRDT.nLotQty7,;
            nTktQty8 WITH MFGOPRDT.nLotQty8,;
            nTktQty  WITH MFGOPRDT.nLotTotQty,;
            nTktCst  WITH BomLine.UnitCost ,;
            nTktAmnt WITH MFGOPRDT.nLotTotQty*BomLine.UnitCost,;
            nAplQty1 WITH lnApQTy1  ,;
            nAplQty2 WITH lnApQTy2  ,;
            nAplQty3 WITH lnApQTy3  ,;
            nAplQty4 WITH lnApQTy4  ,;
            nAplQty5 WITH lnApQTy5  ,;
            nAplQty6 WITH lnApQTy6  ,;
            nAplQty7 WITH lnApQTy7  ,;
            nAplQty8 WITH lnApQTy8  ,;
            nAplQty  WITH lnApTotQty ,;
            nAplAmnt WITH lnApAmount ,;
            nInvQty1 WITH MAX(MFGOPRDT.nLotQty1-lnApQty1,0),;
            nInvQty2 WITH MAX(MFGOPRDT.nLotQty2-lnApQty2,0),;
            nInvQty3 WITH MAX(MFGOPRDT.nLotQty3-lnApQty3,0),;
            nInvQty4 WITH MAX(MFGOPRDT.nLotQty4-lnApQty4,0),;
            nInvQty5 WITH MAX(MFGOPRDT.nLotQty5-lnApQty5,0),;
            nInvQty6 WITH MAX(MFGOPRDT.nLotQty6-lnApQty6,0),;
            nInvQty7 WITH MAX(MFGOPRDT.nLotQty7-lnApQty7,0),;
            nInvQty8 WITH MAX(MFGOPRDT.nLotQty8-lnApQty8,0),;
            nInvQty  WITH MAX(MFGOPRDT.nLotTotQty-lnApTotQty,0),;
            nInvAmnt WITH MAX(MFGOPRDT.nLotTotQty*BomLine.UnitCost-lnApAmount,0)

    *B605967,4 SSH 06/09/2002 Do not edit any invoices which have a closed PO in case of Auto Contr.

  ENDSCAN
ENDIF
SET ORDER TO TAG Orgvinv IN APVINVDT
GO TOP IN (lcAutoInv)
SELECT (lnAlias)
RETURN(!EOF(lcAutoInv))

*!*************************************************************
*! Name      : lfUpdFiles
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update Master Files
*!*************************************************************
*! Calls     : lfOpenFile(),gfTmp2Mast()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdFiles()
*!*************************************************************
FUNCTION lfUpdFiles
PRIVATE lcExStatus

*B602946,1 Add this line to update the actual cost fields in the Style PO,
*          Material PO and Cut Ticket header and detail files using the
*          temp. files that was created by the function lfChckACst (), to
*          calculate the actual cost and check for negative actual cost
*          before saving [Begin]
=lfUpdatPOF()
*B602946,1 Add this line to update the actual cost fields [End]

lcExStatus = SET('EXACT')
SET EXACT OFF
=lfOpenFile('BOMCOST','BOMCOST')
=lfOpenFile('CTKTBOM','CTKTBOM')
SET ORDER TO TAG Bomcstkt IN BOMCOST
SET ORDER TO TAG CTKTBOM IN CTKTBOM

*B802348,1 (Start)
*IF 'I' $ lcVenInvTypes OR 'R' $ lcVenInvTypes
IF ('I' $ lcVenInvTypes OR 'R' $ lcVenInvTypes ) AND USED("POSHDR")
*B802348,1 (End)
  SET ORDER TO TAG POSHDR IN POSHDR
  SET ORDER TO TAG POSREC IN POSLN
ENDIF
IF 'MF' $ gcCmpModules AND laData[44] = gcBaseCurr AND 'M' $ lcVenInvTypes
  SET ORDER TO TAG CUTTKTH IN CUTTKTH 
  SET ORDER TO TAG CUTREC  IN CUTTKTL
ENDIF
*B802348,1 (Start) 
*IF 'L' $ lcVenInvTypes OR 'F' $ lcVenInvTypes
IF ('L' $ lcVenInvTypes OR 'F' $ lcVenInvTypes) AND USED("POFHDR")
*B802348,1 (End)
  =lfOpenFile('MATINVJL','MATINVJL')
  SET ORDER TO TAG POFHDR IN POFHDR
  SET ORDER TO TAG POFREC IN POFLN
ENDIF
*B605911,1 ALB Fix the contribution process [Begin]
SELECT BOMLINE
SET ORDER TO TAG BOMPOREC IN BOMLINE
*B605911,1 ALB Fix the contribution process [End]
SELECT (lcAPInvTkt)
RECALL ALL
SELECT (lcApVInvDt)
RECALL ALL

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
SELECT (lcAPInvTkt)
REPLACE ALL nAPAplAmnt WITH nAPAplAmnt * -1,;
            nAPAplPric WITH nAPAplPric * -1 FOR (cIMTyp $ 'RF' AND (nAPAplAmnt < 0 ))

SELECT (lcApVInvDt)
REPLACE ALL nApAprAmnt WITH nApAprAmnt * -1,;
            nApAprPric WITH nApAprPric * -1,; 
            nApPrice   WITH nApPrice   * -1,;
            nApAmnt    WITH nApAmnt    * -1 FOR (cIMTyp $ 'RF' AND (nApAprAmnt < 0 ))
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]


SCAN FOR cStatus <> 'S' AND SEEK(cVendCode+cApInvNo+cAPVILNo,lcAPInvTkt)
  
  *B605040,1  Spliting the files updates in case of Shipmnet [Begin] . 
  IF &lcApVInvDt..cIMTyp= 'S' 
    =lfUpdShip()
    SELECT (lcApVInvDt)
    LOOP
  ENDIF  
  *B605040,1 Spliting the files updates in case of Shipmnet [End]. 
  
  *B605040,1  We need to update the CTKTBOM and BOMCOST file even if there is no contibution [start]
  * IF EMPTY(&lcAPInvTkt..cRSession) AND &lcApVInvDt..cIMTyp = 'S'  
  *    LOOP
  *  ENDIF
  *B605040,1  We need to update the CTKTBOM and BOMCOST file even if there is no contibution [End]

  *B602946,1 Remove these lines because we have already updated the Style
  *          PO, Material PO and Cut Ticket header files [Begin]
  *DO CASE
  *  CASE INLIST(cIMTyp,'I','R','S')
  *    lcHdrFile   = 'POSHDR'
  *    lcHActFild  = 'NACT_COST'
  *    lcHFActFild = 'NFACTCOST'      
  *    =SEEK(IIF(cIMTyp='R','R','P')+cTktNo,'POSHDR')
  *  CASE cIMTyp='M'
  *    lcHdrFile  = 'CUTTKTH'
  *    lcHActFild = 'NACT_COST'
  *    =SEEK(cTktNo,'CUTTKTH')
  *  CASE INLIST(cIMTyp,'L','F')
  *    lcHdrFile   = 'POFHDR'
  *    lcHActFild  = 'NEACTCOST'
  *    lcHFActFild = 'NACT_COST'
  *    =SEEK(IIF(cIMTyp='L','P','R')+cTktNo,'POFHDR')
  *  CASE cIMTyp='T'
  *ENDCASE
  *B602946,1 Remove these lines because we have already updated [End]
  
  *B605911,1 ALB Fix the contribution process [Begin]
  *llDeleted = (cStatus='D')
  llDeleted = (cStatus='D') OR (cStatus='S' AND EMPTY(nRecNo))
  *B605911,1 ALB Fix the contribution process [End]
  lnRecNo   = nRecNo

  *B602274,1 Fix upting total actual cost.
  *IF !llDeleted AND lnRecNo > 0
  *IF lnRecNo > 0
  *B602274,1 (End)
  *B605040,1 Voiding record out of range [Start]
  IF BETWEEN(lnRecNo ,1,RECCOUNT('APVINVDT'))
  *B605040,1 Voiding record out of range [End]
    SELECT APVINVDT
    GO lnRecNo
  ENDIF
  
  *B602946,1 Remove these lines because we have already updated the Style
  *          PO, Material PO and Cut Ticket header files [Begin]
  **E300798,1 Update Total Actual Cost in Ticket Header File
  *SELECT (lcHdrFile)
  *REPLACE (lcHActFild+&lcApVInvDt..cBomType) WITH ;
  *        EVAL(lcHActFild+&lcApVInvDt..cBomType) - (IIF(lnRecNo=0,0,APVINVDT.nApAprAmnt) - ;
  *        IIF(llDeleted,0,&lcApVInvDt..nApAprAmnt)) &lcExSin1 laData[45] &lcExSin2 laData[48]
  *IF INLIST(&lcApVInvDt..cIMTyp,'I','S','R','L','F')
  *  REPLACE (lcHFActFild+&lcApVInvDt..cBomType) WITH ;
  *          EVAL(lcHFActFild+&lcApVInvDt..cBomType) - IIF(lnRecNo=0,0,APVINVDT.nApAprAmnt) + ;
  *          IIF(llDeleted,0,&lcApVInvDt..nApAprAmnt)
  *ENDIF
  *B602946,1 Remove these lines because we have already updated [End]


  IF !INLIST(&lcApVInvDt..cIMTyp,'L','F','R')
  
    *E300798,1 Update Issued and used quantity
    *MANAMIN
    *IF SEEK(&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno+&lcApVInvDt..cbomtype+;
    *        SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(10),'cTktBom')

    IF SEEK(IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+&lcApVInvDt..cbomtype+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(10),'cTktBom')
      SELECT cTktBom
      REPLACE ISSUE_QTY WITH ISSUE_QTY - IIF(lnRecNo=0,0,APVINVDT.nApTAprQty) + IIF(llDeleted,0,&lcApVInvDt..nApTAprQty),;
              USED_QTY  WITH USED_QTY  - IIF(lnRecNo=0,0,APVINVDT.nApTAprQty) + IIF(llDeleted,0,&lcApVInvDt..nApTAprQty)
    ENDIF

    *B802286,1 Add these lines to update this record of BOMCOST file
    *          with the uncontributed amount only, and not with the
    *          approved amount [Begin]
    
    *-- If the Invoice line was deleted or if the uncontributed amount was
    *-- changed.
    IF llDeleted .OR.;
       (EMPTY(&lcAPInvTkt..cRSession) .AND. &lcAPInvTkt..cStatus <> 'S')



      *B605040,1 Voiding record out of range [Start]      
      *IF &lcAPInvTkt..nRecNo <> 0 
      IF BETWEEN(&lcAPInvTkt..nRecNo ,1,RECCOUNT('APINVTKT'))
      *B605040,1 Voiding record out of range [End]
        GO &lcAPInvTkt..nRecNo IN APINVTKT
      ENDIF
      *B802286,1 Add these lines to update this record of BOMCOST [End]
      
      *E300798,1 Update BOMCOST file
      SELECT BOMCOST
      *MANAMIN 
      *=SEEK(&lcApVInvDt..cbomtype+&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno+;
      *      SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+;
      *      SPACE(6)+SPACE(6))
      * LOCATE REST WHILE ;
      *cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      *&lcApVInvDt..cbomtype+&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno+SPACE(19)+;
      *SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6) ;
      *FOR cVendCode+cApInvNo = PADR(laData[1],8)+PADR(laData[2],12)
       
      lcKey = &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6)
             
      =SEEK(&lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+;
            SPACE(6)+SPACE(6))
      LOCATE REST WHILE ;
      cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+SPACE(19)+;
      SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6) ;
      FOR cVendCode+cApInvNo = PADR(laData[1],8)+PADR(laData[2],12)
            
      
      DO CASE
        
        *B802286,1 Change this condition to update this record of BOMCOST
        *          file with the uncontributed amount only, and not with
        *          the approved amount [Begin]
        CASE !llDeleted AND !FOUND() .AND. &lcAPInvTkt..cStatus <> 'D'
        *B802286,1 Change this condition to update this record of BOMCOST [End]
          
          APPEND BLANK
          
          *B802286,1 Change this line to update this record of BOMCOST file
          *          with the uncontributed amount only, and not with the
          *          approved amount [Begin]
          *REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
          *        CIMTYP    WITH &lcApVInvDt..cIMTyp   ,;
          *        CTKTNO    WITH &lcApVInvDt..ctktno   ,;
          *        ITEM      WITH ''                    ,;
          *        ICLR      WITH ''                    ,;
          *        MFGCODE   WITH &lcApVInvDt..cOprCode ,;
          *        CWARECODE WITH ''                    ,;
          *        CDYELOT   WITH ''                    ,;
          *        CRSESSION WITH SPACE(6)  ,;
          *        CVENDCODE WITH laData[1] ,;
          *        CAPINVNO  WITH laData[2] ,;
          *        CISESSION WITH ''        ,;
          *        SHIPNO    WITH ''        ,;
          *        DTRANDATE WITH laData[10],;
          *        CCOSTTYPE WITH &lcApVInvDt..cCostType  ,;
          *        NTOTQTY   WITH &lcApVInvDt..nApTAprQty ,;
          *        NTOTCST   WITH &lcApVInvDt..nApAprAmnt ,;
          *        NUNITCST  WITH &lcAPvInvDt..nApAprPric ,;
          *        NTOTACST  WITH NTOTCST  ,;
          *        NUNITACST WITH NUNITCST ,;
          *        Actualize WITH 'N'
          *MANAMIN IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) insteated of WITH &lcApVInvDt..cIMTyp
          *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
          *REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp),;
                  CTKTNO    WITH IIF(&lcApVInvDt..cIMTyp="S",&lcAPInvTkt..ctktno,&lcApVInvDt..ctktno),;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH SPACE(6)  ,;
                  CVENDCODE WITH laData[1] ,;
                  CAPINVNO  WITH laData[2] ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH laData[10],;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType  ,;
                  NTOTQTY   WITH &lcAPInvTkt..nAPTAplQty -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH &lcAPInvTkt..nAPAplAmnt -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH &lcAPInvTkt..nAPAplPric -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplPric) ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'N'

          REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp),;
                  CTKTNO    WITH IIF(&lcApVInvDt..cIMTyp="S",&lcAPInvTkt..ctktno,&lcApVInvDt..ctktno),;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH SPACE(6)  ,;
                  CVENDCODE WITH laData[1] ,;
                  CAPINVNO  WITH laData[2] ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH laData[50],;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType  ,;
                  NTOTQTY   WITH &lcAPInvTkt..nAPTAplQty -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH &lcAPInvTkt..nAPAplAmnt -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH &lcAPInvTkt..nAPAplPric -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplPric) ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'N'
          *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
          *B802286,1 Change this line to update this record of BOMCOST [End]                  
          
        CASE !llDeleted AND FOUND()
          
          *B606135,1 ALB 07/21/2002 Update unit cost in bocost file [Begin]
          *B802286,1 Change this line to update this record of BOMCOST file
          *          with the uncontributed amount only, and not with the
          *          approved amount [Begin]
          REPLACE NTOTQTY   WITH NTOTQTY +;
                                 IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                     &lcAPInvTkt..nAPTAplQty) -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH NTOTCST +;
                                 IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                     &lcAPInvTkt..nAPAplAmnt) -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                 NTOTCST/NTOTQTY);
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST

*          REPLACE NTOTQTY   WITH NTOTQTY +;
                                 IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                     &lcAPInvTkt..nAPTAplQty) -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH NTOTCST +;
                                 IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                     &lcAPInvTkt..nAPAplAmnt) -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH NUNITCST +;
                                 IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;  
                                     &lcAPInvTkt..nAPAplPric) -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplPric) ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST

          *B802286,1 Change this line to update this record of BOMCOST [End]
          *B606135,1 ALB 07/21/2002 Update unit cost in bocost file [End]
          
          *B606054 ALB [Begin]
          *IF NTOTQTY = 0
          IF (&lcAPInvTkt..cStatus = 'D') .OR. (NTOTQTY = 0)
          *B606054 ALB [End]
            DELETE
          ENDIF
        
        CASE llDeleted AND FOUND()
          *B802286,1 Change this line to update this record of BOMCOST
          *          file with the uncontributed amount only, and not with
          *          the approved amount
          *          Note: Note that this line could hold the uncontributed
          *          amount of more than one invoice line [Begin]
          *DELETE
          REPLACE NTOTQTY   WITH NTOTQTY -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPTAplQty) ,;
                  NTOTCST   WITH NTOTCST -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplAmnt) ,;
                  NUNITCST  WITH NUNITCST -;
                                 IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                       APINVTKT.nAPAplPric) ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST
          
          *B606054 ALB [Begin]
          *IF NTOTQTY = 0
          IF (&lcAPInvTkt..cStatus = 'D') .OR. (NTOTQTY = 0)
          *B606054 ALB [End]
            DELETE
          ENDIF
          *B802286,1 Change this line to update this record of BOMCOST [End]
          
      ENDCASE
      
    *B802286,1 Add this if condition to update this record of BOMCOST file
    *          with the uncontributed amount only, and not with the
    *          approved amount [Begin]
    ENDIF    && End of IF EMPTY(&lcAPInvTkt..cRSession) .AND. ...
    *B802286,1 Add this if condition to update this record of BOMCOST [End]

    *E300798,1 Update BOMLINE file
    SELECT (lcAPInvTkt)
    SCAN REST WHILE cVendCode+cApInvNo+cAPVILNo = ;
                    &lcApVInvDt..cVendCode+&lcApVInvDt..cApInvNo+&lcApVInvDt..cAPVILNo ;
              FOR   !EMPTY(cRSession)      
      *B605911,1 ALB Fix the contribution process [Begin] 
      *llDeleted = (cStatus='D')
      llDeleted = (cStatus='D') OR (cStatus='S' AND EMPTY(nRecNo))
      *B605911,1 ALB Fix the contribution process [End]
      lnRecNo = nRecNo
      
      *B605040,1 Voiding record out of range [Start]
      *IF !llDeleted AND lnRecNo > 0
      IF !llDeleted AND BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
      *B605040,1 Voiding record out of range [End]      
        SELECT APINVTKT
        GO lnRecNo
      ENDIF
      SELECT BomLine
      *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [BEGIN]
      *=SEEK(&lcApVInvDt..cIMTyp+"3"+&lcApVInvDt..cBomType+&lcApVInvDt..cOprCode+;
            &lcApVInvDt..ctktno+&lcAPInvTkt..cRSession+&lcApVInvDt..shipno+;
            STR(&lcApInvTkt..LineNo,6)+&lcApInvTkt..Item+&lcApVInvDt..Color)
            
      =SEEK(&lcApVInvDt..cIMTyp+"3"+&lcApVInvDt..cBomType+&lcApVInvDt..cOprCode+;
            &lcApVInvDt..ctktno+&lcAPInvTkt..cRSession+&lcApVInvDt..shipno+;
            STR(&lcApInvTkt..LineNo,6)+&lcApInvTkt..Item+&lcApInvTkt..Color)
      *E301995,1 Alb Add Adorment/Dying/MMFG order to invoice line [END]

      DO CASE
        CASE !llDeleted AND !FOUND()
           APPEND BLANK
           *B605911,1 ALB add style info to bomline [Begin]
           *B605040,1 Some changes in BomLine  file [Start]

           *REPLACE cimtyp    WITH &lcApVInvDt..cIMTyp ,;
           *        ctype     WITH '3'                   ,;
           *        cbomtyp   WITH &lcApVInvDt..cBomType ,;
           *        mfgcode   WITH &lcApVInvDt..cOprCode ,;
           *        ctktno    WITH &lcApVInvDt..ctktno,;
           *        crsession WITH &lcAPInvTkt..cRSession,;
           *        shipno    WITH &lcApVInvDt..shipno   ,;
           *        lineno    WITH &lcApVInvDt..LineNo   ,;
           *        style     WITH &lcApVInvDt..Item     ,;
           *        sclr      WITH &lcApVInvDt..Color    ,;
           *        cCatgTyp  WITH &lcApVInvDt..cCostType,;
           *        UnitQty   WITH  1 ,;
           *        StyQty    WITH &lcAPInvTkt..nApTAplQty ,;
           *        ItemQty   WITH StyQty ,;
           *        ItemAmt   WITH &lcAPInvTkt..nApAplAmnt ,;
           *        UnitCost  WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
           
*           REPLACE cimtyp    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) ,;
                   ctype     WITH '3'                   ,;
                   cbomtyp   WITH &lcApVInvDt..cBomType ,;
                   mfgcode   WITH &lcApVInvDt..cOprCode ,;
                   ctktno    WITH IIF(&lcApVInvDt..cIMTyp="S",&lcAPInvTkt..ctktno,&lcApVInvDt..ctktno),;
                   crsession WITH &lcAPInvTkt..cRSession,;
                   shipno    WITH &lcApVInvDt..shipno   ,;
                   lineno    WITH &lcApVInvDt..LineNo   ,;
                   style     WITH &lcApVInvDt..Item     ,;
                   sclr      WITH &lcApVInvDt..Color    ,;
                   cCatgTyp  WITH &lcApVInvDt..cCostType,;
                   UnitQty   WITH  1 ,;
                   StyQty    WITH &lcAPInvTkt..nApTAplQty ,;
                   ItemQty   WITH StyQty ,;
                   ItemAmt   WITH &lcAPInvTkt..nApAplAmnt ,;
                   UnitCost  WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
        *B605040,1 Some changes in BomLine  file [End]                   
           REPLACE cimtyp    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) ,;
                   ctype     WITH '3'                   ,;
                   cbomtyp   WITH &lcApVInvDt..cBomType ,;
                   mfgcode   WITH &lcApVInvDt..cOprCode ,;
                   ctktno    WITH IIF(&lcApVInvDt..cIMTyp="S",&lcAPInvTkt..ctktno,&lcApVInvDt..ctktno),;
                   crsession WITH &lcAPInvTkt..cRSession,;
                   shipno    WITH &lcApVInvDt..shipno   ,;
                   lineno    WITH &lcAPInvTkt..LineNo   ,;
                   style     WITH &lcAPInvTkt..Item     ,;
                   sclr      WITH &lcAPInvTkt..Color    ,;
                   cCatgTyp  WITH &lcApVInvDt..cCostType,;
                   UnitQty   WITH  1 ,;
                   StyQty    WITH &lcAPInvTkt..nApTAplQty ,;
                   ItemQty   WITH StyQty ,;
                   ItemAmt   WITH &lcAPInvTkt..nApAplAmnt ,;
                   UnitCost  WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
        
        *B605911,1 ALB add style info to bomline [End]
        
        CASE !llDeleted AND FOUND()
          *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
          IF llDebitM AND !(&lcApVInvDt..cIMTyp $'RF')
            REPLACE StyQty   WITH StyQty - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) - IIF(&lcAPInvTkt..nApTAplQty >=StyQty,0,&lcAPInvTkt..nApTAplQty),;
                    ItemQty  WITH StyQty ,;
                    ItemAmt  WITH ItemAmt -IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
                    UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)

          ELSE
            REPLACE StyQty   WITH StyQty - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + &lcAPInvTkt..nApTAplQty ,;
                    ItemQty  WITH StyQty ,;
                    ItemAmt  WITH ItemAmt -IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
                    UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
          ENDIF
          *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [ENd]
          
        CASE llDeleted AND FOUND()
          *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
          
          *E300599,1 ALB Fix the contribution process [Begin]
          *DELETE
          IF llDebitM AND !(&lcApVInvDt..cIMTyp $'RF')
            REPLACE StyQty   WITH StyQty +  IIF(&lcAPInvTkt..nApTAplQty >=StyQty,0,&lcAPInvTkt..nApTAplQty) ,;
                    ItemQty  WITH StyQty ,;
                    ItemAmt  WITH ItemAmt - &lcAPInvTkt..nApAplAmnt ,;
                    UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)

          ELSE
            REPLACE StyQty   WITH StyQty - &lcAPInvTkt..nApTAplQty ,;
                    ItemQty  WITH StyQty ,;
                    ItemAmt  WITH ItemAmt - &lcAPInvTkt..nApAplAmnt ,;
                    UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
          ENDIF
          *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [ENd]
          IF ItemQty = 0
            DELETE
          ENDIF
          *E300599,1 ALB Fix the contribution process [End]
      ENDCASE
      *E300798,1 Update BOMCOST file
      SELECT BOMCOST
      *B605040,1 Some changes in BOMCOST file [Start]
      *=SEEK(&lcApVInvDt..cbomtype+&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno+;
      *      SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+;
      *      &lcAPInvTkt..cRSession+SPACE(6))
      *LOCATE REST WHILE ;
      *cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      *&lcApVInvDt..cbomtype+&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno+SPACE(19)+;
      *SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6) ;
      *FOR cVendCode+cApInvNo = PADR(laData[1],8)+PADR(laData[2],12)
      
      *LOCATE REST WHILE ;
      *cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      *&lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp)+&lcApVInvDt..ctktno+SPACE(19)+;
      *SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6) ;
      *FOR cVendCode+cApInvNo = PADR(laData[1],8)+PADR(laData[2],12)
      
      lcKey = &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I"+&lcApInvTkt..ctktno,&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno)+;
              SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6)
            
      =SEEK(&lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I"+&lcApInvTkt..ctktno,&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno)+;
            SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+;
            &lcAPInvTkt..cRSession+SPACE(6))
            
      LOCATE REST WHILE ;
      cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      &lcApVInvDt..cbomtype+IIF(&lcApVInvDt..cIMTyp="S","I"+&lcApInvTkt..ctktno,&lcApVInvDt..cIMTyp+&lcApVInvDt..ctktno)+SPACE(19)+;
      SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6) ;
      FOR cVendCode+cApInvNo = PADR(laData[1],8)+PADR(laData[2],12)
      
      DO CASE
        CASE !llDeleted AND !FOUND()
          APPEND BLANK
          
          *B802286,1 Change this line to update the (Actualize) field
          *          properly, update it with "Y" [Begin]
          *REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
          *        CIMTYP    WITH &lcApVInvDt..cIMTyp   ,;
          *        CTKTNO    WITH &lcApVInvDt..ctktno   ,;
          *        ITEM      WITH ''                    ,;
          *        ICLR      WITH ''                    ,;
          *        MFGCODE   WITH &lcApVInvDt..cOprCode ,;
          *        CWARECODE WITH ''                    ,;
          *        CDYELOT   WITH ''                    ,;
          *        CRSESSION WITH &lcAPInvTkt..cRSession,;
          *        CVENDCODE WITH laData[1] ,;
          *        CAPINVNO  WITH laData[2] ,;
          *        CISESSION WITH ''        ,;
          *        SHIPNO    WITH ''        ,;
          *        DTRANDATE WITH laData[10],;
          *        CCOSTTYPE WITH &lcApVInvDt..cCostType ,;
          *        NTOTQTY   WITH &lcAPInvTkt..nApTAplQty ,;
          *        NTOTCST   WITH &lcAPInvTkt..nApAplAmnt ,;
          *        NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
          *        NTOTACST  WITH NTOTCST  ,;
          *        NUNITACST WITH NUNITCST ,;
          *        Actualize WITH 'N'
          *MANAMIN   IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) instead &lcApVInvDt..cIMTyp 
          *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
          *REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) ,;
                  CTKTNO    WITH IIF(&lcApVInvDt..cIMTyp="S", &lcApInvTkt..ctktno ,&lcApVInvDt..ctktno ),;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH &lcAPInvTkt..cRSession,;
                  CVENDCODE WITH laData[1] ,;
                  CAPINVNO  WITH laData[2] ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH laData[10],;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType ,;
                  NTOTQTY   WITH &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          

          REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH IIF(&lcApVInvDt..cIMTyp="S","I",&lcApVInvDt..cIMTyp) ,;
                  CTKTNO    WITH IIF(&lcApVInvDt..cIMTyp="S", &lcApInvTkt..ctktno ,&lcApVInvDt..ctktno ),;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH &lcAPInvTkt..cRSession,;
                  CVENDCODE WITH laData[1] ,;
                  CAPINVNO  WITH laData[2] ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH laData[50],;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType ,;
                  NTOTQTY   WITH &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
          *B802286,1 Change this line to update the (Actualize) [End]
          
        CASE !llDeleted AND FOUND()
          
          *B802286,1 Change this line to update the (Actualize) field
          *          properly, update it with "Y" [Begin]
          *REPLACE NTOTQTY   WITH NTOTQTY - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + &lcAPInvTkt..nApTAplQty ,;
          *        NTOTCST   WITH NTOTCST - IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
          *        NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
          *        NTOTACST  WITH NTOTCST  ,;
          *        NUNITACST WITH NUNITCST
          REPLACE NTOTQTY   WITH NTOTQTY - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH NTOTCST - IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          
          *B802286,1 Change this line to update the (Actualize) [End]
          
        CASE llDeleted AND FOUND()
          *B605911,1 ALB Fix the contribution process [Begin]
          *DELETE
          REPLACE NTOTQTY   WITH NTOTQTY -  IIF(lnRecNo=0,0,&lcAPInvTkt..nApTAplQty) ,;
                  NTOTCST   WITH NTOTCST -  IIF(lnRecNo=0,0,&lcAPInvTkt..nApAplAmnt) ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          IF NTOTQTY = 0
            DELETE
          ENDIF
          *B605911,1 ALB Fix the contribution process [End]
      ENDCASE
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lcApVInvDt)
DELETE ALL FOR INLIST(cStatus,'D','S')
SELECT (lcAPInvTkt)
DELETE ALL FOR INLIST(cStatus,'D','S')
*E300798,1 Update Invoice Details File
=gfTmp2Mast('APVINVDT',lcAPvInvDt)
*E300798,1 Update Invoice Applied Tickets File
=gfTmp2Mast('APINVTKT',lcAPInvTkt)

*B602946,1 Remove these lines because we have already updated the Style
*          PO, Material PO and Cut Ticket header files [Begin]
**E300798,1 Update Actual Cost in Ticket Lines File
*SET ORDER TO TAG TICKET  IN APINVTKT
*SET ORDER TO TAG Orgvinv IN APVINVDT
*
*SELECT (lcApVInvDt)
*RECALL ALL FOR cStatus='D'
*SELECT (lcAPInvTkt)
*RECALL ALL FOR cStatus='D'
*SET ORDER TO TAG TICKET
*GO TOP
*
*DO WHILE !EOF()
*  lcType=cIMTyp
*  DO CASE
*    CASE INLIST(lcType,'I','R','S')
*      lcDetFile  = 'POSLN'
*      lcActFild  = 'NEACTCOST'
*      lcFActFild = 'NACT_CST'
*      lcQtyFild  = 'TotQty'
*      lcKey      = [IIF(lcType='R','R','P')+cTKtNo+cRSession+&lcApVInvDt..ShipNo+Item+STR(LineNo,6)]
*      lcForCond  = [.T.]
*    CASE lcType='M'
*      lcDetFile  = 'CUTTKTL'
*      lcActFild  = 'NACT_CST'
*      lcQtyFild  = 'TotQty'
*      lcKey      = [cRSession+cTKtNo+Item]
*      lcForCond  = [STR(LineNo,6)=STR(&lcAPInvTkt..LineNo,6)]
*    CASE INLIST(lcType,'L','F')
*      lcDetFile  = 'POFLN'
*      lcActFild  = 'NEACTCOST'
*      lcFActFild = 'NACT_COST'
*      lcQtyFild  = 'nFabTotQty'
*      lcKey      = [IIF(lcType='L','P','R')+cTKtNo+cRSession+SUBSTR(ITEM,1,7)+color]
*      lcForCond  = [STR(LineNo,6)=STR(&lcAPInvTkt..LineNo,6)]
*    CASE lcType='T'
*  ENDCASE
*  SCAN REST WHILE cIMTyp+cTKtNo+STR(LineNo,6)+cRSession = lcType FOR !EMPTY(cRSession)
*    =SEEK(cVendCode+cApInvNo+cAPVILNo,lcAPvInvDt)
*    lcKeyValue = EVAL(lcKey)
*    SELECT (lcDetFile)
*    =SEEK(lcKeyValue)
*    LOCATE REST WHILE EVAL(SYS(14,VAL(SYS(21)))) = lcKeyValue FOR EVAL(lcForCond)
*    SELECT APINVTKT
*    STORE 0 TO lnAplAmnt1,lnAplAmnt2,lnAplAmnt3,lnAplAmnt4,lnAplAmnt5
*    =SEEK(&lcAPInvTkt..cIMTyp+&lcAPInvTkt..cTKtNo+STR(&lcAPInvTkt..LineNo,6)+&lcAPInvTkt..cRSession)
*    SCAN REST WHILE cIMTyp+cTKtNo+STR(LineNo,6)+cRSession=;
*                    &lcAPInvTkt..cIMTyp+&lcAPInvTkt..cTKtNo+;
*                    STR(&lcAPInvTkt..LineNo,6)+&lcAPInvTkt..cRSession
*      =SEEK(cVendCode+cApInvNo+cAPVILNo,'APVINVDT')
*      lcCstType=APVINVDT.cBomType
*      lnAplAmnt&lcCstType = lnAplAmnt&lcCstType + nApAplAmnt
*    ENDSCAN
*    SELECT (lcDetFile)
*    REPLACE (lcActFild+'1')  WITH lnAplAmnt1/EVAL(lcQtyFild) &lcExSin1 laData[45] &lcExSin2 laData[48],;
*            (lcActFild+'2')  WITH lnAplAmnt2/EVAL(lcQtyFild) &lcExSin1 laData[45] &lcExSin2 laData[48],;    
*            (lcActFild+'3')  WITH lnAplAmnt3/EVAL(lcQtyFild) &lcExSin1 laData[45] &lcExSin2 laData[48],;
*            (lcActFild+'4')  WITH lnAplAmnt4/EVAL(lcQtyFild) &lcExSin1 laData[45] &lcExSin2 laData[48]
*    IF INLIST(lcType,'I','R','S','L','F')
*      REPLACE (lcFActFild+'1') WITH lnAplAmnt1/EVAL(lcQtyFild) ,;
*              (lcFActFild+'2') WITH lnAplAmnt2/EVAL(lcQtyFild) ,;    
*              (lcFActFild+'3') WITH lnAplAmnt3/EVAL(lcQtyFild) ,;
*              (lcFActFild+'4') WITH lnAplAmnt4/EVAL(lcQtyFild)
*    ENDIF
*    IF INLIST(lcType,'I','R','S','M')
*      REPLACE (lcActFild+'5')  WITH lnAplAmnt5/EVAL(lcQtyFild) &lcExSin1 laData[45] &lcExSin2 laData[48]
*    ENDIF
*    IF INLIST(lcType,'I','R','S')
*      REPLACE (lcFActFild+'5') WITH lnAplAmnt5/EVAL(lcQtyFild)
*    ENDIF
*    IF INLIST(lcType,'L','F')
*      SELECT MATINVJL
*      =SEEK(SUBSTR(&lcAPInvTkt..Item,1,7)+&lcAPInvTkt..Color+;
*            &lcAPInvTkt..cWareCode+&lcAPInvTkt..cDyelot+&lcAPInvTkt..cRSession)
*      REPLACE cAPInvNo WITH laData[2]
*    ENDIF
*  ENDSCAN  
*ENDDO
*B602946,1 Remove these lines because we have already updated [End]

*B602238,1 Set the approperiate tag
SET ORDER TO TAG (lcAPInvTkt) IN (lcAPInvTkt)
SET EXACT &lcExStatus

*!*************************************************************
*! Name      : lfGetAdjNo
*! Developer : WAM
*! Date      : 03/11/1999
*! Purpose   : Get next adjustment number
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetAdjNo()
*!*************************************************************
FUNCTION lfGetAdjNo
PRIVATE lnLastAdj,lcLastAdj

SELECT MAX(SUBSTR(cApdRef,4)),LEFT(capdref,3) AS ADJ FROM APDIST ;
WHERE cinvno+cvendcode+capdtrtyp=PADR(laData[2],12)+PADR(laData[1],8)+'H' AND;
 LEFT(capdref,3)='ADJ' GROUP BY ADJ INTO ARRAY lcLastAdj
IF _TALLY = 0
  lcLastAdj = '0'
ENDIF
lnLastAdj = INT(VAL(lcLastAdj))+1
DO WHILE .T.
  SELECT ApPaymnt
  =SEEK('P'+ApInvHdr.cVenPMeth+'ADJ'+PADL(lnLastAdj,5,'0'))
  *MAN
  *LOCATE REST WHILE cpaytype+cpaymeth+cpaydocno+cbnkcode+cchkacct = ;
                   'P'+ApInvHdr.cVenPMeth+'ADJ'+PADL(lnLastAdj,5,'0') ;
              FOR   cPayClNo = PADR(laData[1],8)
  *B605486,1 SSH 26/Feb/2002 Incorect paymnt method in appaymnt file
  *LOCATE REST WHILE cpaytype+cpaymeth+cpaydocno+cbnkcode+cchkacct = ;
                   'P'+'C'+'ADJ'+PADL(lnLastAdj,5,'0') ;
              FOR   cPayClNo = PADR(laData[1],8)

  LOCATE REST WHILE cpaytype+cpaymeth+cpaydocno+cbnkcode+cchkacct = ;
                   'P'+'H'+'ADJ'+PADL(lnLastAdj,5,'0') ;
              FOR   cPayClNo = PADR(laData[1],8)
  *B605486,1 SSH 26/Feb/2002 Incorect paymnt method in appaymnt file
  IF FOUND()            
    lnLastAdj = lnLastAdj +1
  ELSE
    EXIT
  ENDIF  
ENDDO
RETURN('ADJ'+PADL(lnLastAdj,5,'0'))

*!*************************************************************
*! Name      : lfInvAdjust
*! Developer : WAM
*! Date      : 03/11/1999
*! Purpose   : Add invoice adjustment
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfInvAdjust()
*!*************************************************************
FUNCTION lfInvAdjust
PRIVATE lnAlias,llAdjust,lnTOldApr,lcInvAdjNo,lcCashAct,lnAdjAmnt,lnAccAmnt,;
        lcApDGlAct,llAdjustment

lnAlias = SELECT()
=gfOpenFile(gcDataDir+'ApPaymnt',gcDataDir+'Typmethdoc','SH')
lcExStatus = SET('EXACT')
SET EXACT OFF

llAdjust  = .F.
lnTOldApr =0
IF laScrMode[3]
  WAIT 'Checking for Previous adjustments' WINDOW NOWAIT
  SELECT APVINVDT
  SET ORDER TO TAG Orgvinv
  =SEEK(PADR(laData[1],8)+PADR(laData[2],12))
  SUM REST IIF(CIMTYP $ 'RF',-nApAPrAmnt,nApAPrAmnt) TO lnTOldApr ;
      WHILE cVendCode+cApInvNo+cAPVILNo = PADR(laData[1],8)+PADR(laData[2],12)
  SELECT (lcApVInvDt)
  SCAN
    *B605040,1 Void Record is out of range [Start]
    *IF NRECNO > 0  
    IF NRECNO > 0  AND BETWEEN(NRECNO,1,RECCOUNT('ApVInvDt'))
    *B605040,1 Void Record is out of range [Start]
      lnRecNo = NRECNO
      GOTO lnRecNo IN 'ApVInvDt'

      *B604455,1 Invoice amount not equal to the old amount (Invoice amount changed). [Begin]
      *IF nApAprAmnt <> ApVInvDt.nApAprAmnt  
      IF nApAprAmnt <> ApVInvDt.nApAprAmnt OR nApPrice <> ApVInvDt.nApPrice
      *B604455,1 Invoice amount not equal to the old amount (Invoice amount changed). [End]

        llAdjust = .T.
        EXIT
      ENDIF
    ENDIF
  ENDSCAN
  IF llAdjust OR (lnTOldApr<>0 AND lnTAprAmt <> lnTOldApr)
    =lfVAdjust()
  ENDIF
  WAIT CLEAR
ENDIF
IF lnTAprAmt=lnTOldApr AND !llAdjust
  SET EXACT &lcExStatus
  SELECT (lnAlias)
  RETURN
ENDIF
WAIT 'Saving adjustment' WINDOW NOWAIT
lcInvAdjNo = lfGetAdjNo()
DO CASE
  CASE !EMPTY(STRTRAN(STRTRAN(laData[29],'-'),'0'))
    lcCashAct = laData[29]
  CASE !EMPTY(APVENDOR.CCASHACCT)
    lcCashAct = APVENDOR.CCASHACCT
  CASE !EMPTY(laData[4]) .AND. !EMPTY(APDIV.CCASHACCT)
    lcCashAct = APDIV.CCASHACCT
  OTHERWISE
    lcCashAct = APSETUP.CCASHACCT
ENDCASE  
SELECT (lcApVInvDt)
SET ORDER TO TAG ADJUST
lnAdjAmnt = 0
GO TOP
llAdjustment = .F.
DO WHILE !EOF()
  lcApDGlAct = cApDGlAct 
  
  *C121324,1  TMI [Start] Do not care with adjustments in deleted records, only sum with new added,modified or selected records
  *SUM REST (ROUND(nAPAMnt,2)-ROUND(nApAprAmnt,2))*IIF(CIMTYP $ 'RF',-1,1) ;
      TO   lnAccAmnt WHILE cApDGlAct = lcApDGlAct
  SUM REST (ROUND(nAPAMnt,2)-ROUND(nApAprAmnt,2))*IIF(CIMTYP $ 'RF',-1,1) ;
      TO   lnAccAmnt WHILE cApDGlAct = lcApDGlAct ;
                     FOR CSTATUS $ 'AMS'
  *C121324,1  TMI [End  ] 
  IF lnAccAmnt <> 0
    SELECT (lcTmpDist)
    APPEND BLANK
    REPLACE CVENDCODE WITH laData[1]  ,;
            CINVNO    WITH laData[2]  ,;
            DAPDTRDAT WITH laData[50] ,;
            LAPDPOST  WITH .F.        ,;
            CAPDGLACT WITH lcApDGlAct ,;
            CAPDACTID WITH 'J'        ,;
            CAPDREF   WITH lcInvAdjNo ,;
            CSTATUS   WITH 'A'        ,;
            CFISFYEAR WITH lcDistYer  ,;
            CFSPPRDID WITH lcDistPrd  ,;
            CAPSESSNO WITH lcSequence ,;
            CAPDTRTYP WITH 'H'        ,;
            CCURRCODE WITH laData[44] ,;
            NEXRATE   WITH laData[45] ,;
            NCURRUNIT WITH laData[48] ,;
            NAPDAMNT  WITH -lnAccAmnt ,;
            NEQVAMNT  WITH -lnAccAmnt &lcExSin1 laData[45] &lcExSin2 laData[48]
    =gfAdd_Info(lcTmpDist)
    llAdjustment = .T.
  ENDIF
  lnAdjAmnt = lnAdjAmnt + lnAccAmnt
  SELECT (lcApVInvDt)
ENDDO
IF llAdjustment
  SELECT (lcTmpDist)
  APPEND BLANK
  REPLACE CVENDCODE WITH laData[1]  ,;
          CINVNO    WITH laData[2]  ,;
          DAPDTRDAT WITH laData[50] ,;
          LAPDPOST  WITH .F.        ,;
          CAPDGLACT WITH laData[33] ,;
          CAPDACTID WITH 'A'        ,;
          CAPDREF   WITH lcInvAdjNo ,;
          CFISFYEAR WITH lcDistYer  ,;
          CFSPPRDID WITH lcDistPrd  ,;
          CSTATUS   WITH 'A'        ,;
          CAPSESSNO WITH lcSequence ,;
          CAPDTRTYP WITH 'H'        ,;
          CCURRCODE WITH laData[44] ,;
          NEXRATE   WITH laData[45] ,;
          NCURRUNIT WITH laData[48] ,;
          NAPDAMNT  WITH lnAdjAmnt  ,;
          NEQVAMNT  WITH lnAdjAmnt &lcExSin1 laData[45] &lcExSin2 laData[48]
  =gfAdd_Info(lcTmpDist)
  APPEND BLANK
  REPLACE CVENDCODE WITH laData[1]  ,;
          CINVNO    WITH laData[2]  ,;
          DAPDTRDAT WITH laData[50] ,;
          LAPDPOST  WITH .F.        ,;
          CAPDGLACT WITH lcCashAct  ,;
          CAPDACTID WITH 'C'        ,;
          CSTATUS   WITH 'A'        ,;
          CAPDREF   WITH lcInvAdjNo ,;
          CFISFYEAR WITH lcDistYer  ,;
          CFSPPRDID WITH lcDistPrd  ,;
          CAPSESSNO WITH lcSequence ,;
          CAPDTRTYP WITH 'H'        ,;
          CCURRCODE WITH laData[44] ,;
          NEXRATE   WITH laData[45] ,;
          NCURRUNIT WITH laData[48] ,;
          NAPDAMNT  WITH 0          ,;
          NEQVAMNT  WITH 0
  =gfAdd_Info(lcTmpDist)

  =gfTmp2Mast('APDIST',lcTmpDist)
  
  =SEEK(PADR(laData[1],8),'ApVendor')
*MAN    
*  INSERT INTO ('ApPaymnt') ;
         (cPayType,cPAyMeth,cPayDocNo,cPayStat,dpayDate,cFisFYear,cFspprdid,;
          cPayClNo,cPayComp,npayadj,cpayrecst,ccurrcode,ncurrunit,nexrate) VALUES ;
         ('P',ApInvHdr.cVenPMeth,lcInvAdjNo,'',laData[50],lcDistYer,lcDistPrd,;
          laData[1],ApVendor.cVenComp,lnAdjAmnt,'O',laData[44],laData[48],laData[45])
*B605486,1 SSH 26/Feb/2002 Incorect paymnt method in appaymnt file
  *INSERT INTO ('ApPaymnt') ;
         (cPayType,cPAyMeth,cPayDocNo,cPayStat,dpayDate,cFisFYear,cFspprdid,;
          cPayClNo,cPayComp,npayadj,cpayrecst,ccurrcode,ncurrunit,nexrate) VALUES ;
         ('P','C',lcInvAdjNo,'',laData[50],lcDistYer,lcDistPrd,;
          laData[1],ApVendor.cVenComp,lnAdjAmnt,'O',laData[44],laData[48],laData[45])

  INSERT INTO ('ApPaymnt') ;
         (cPayType,cPAyMeth,cPayDocNo,cPayStat,dpayDate,cFisFYear,cFspprdid,;
          cPayClNo,cPayComp,npayadj,cpayrecst,ccurrcode,ncurrunit,nexrate) VALUES ;
         ('P','H',lcInvAdjNo,'',laData[50],lcDistYer,lcDistPrd,;
          laData[1],ApVendor.cVenComp,lnAdjAmnt,'O',laData[44],laData[48],laData[45])

*B605486,1 SSH [END]
  =gfAdd_Info('ApPaymnt')
  SELECT ApInvHdr
  =RLOCK()
  REPLACE nInvAdj WITH nInvAdj + lnAdjAmnt
  UNLOCK
  SELECT ApVendor
  =RLOCK()
  REPLACE nVenBal WITH nVenBal - lnAdjAmnt
  UNLOCK
  SELECT ApVenHst
  =SEEK(PADR(laData[1],8)+lcDistYer)
  =RLOCK()
  REPLACE nVnHAdj WITH nVnHAdj + lnAdjAmnt
  UNLOCK
ENDIF
WAIT CLEAR
SET EXACT &lcExStatus
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfVAdjust
*! Developer : WAM
*! Date      : 03/11/1999
*! Purpose   : Void invoice adjustment
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfVAdjust()
*!*************************************************************
FUNCTION lfVAdjust

WAIT 'Voiding Previous adjustment...' WINDOW NOWAIT 

*B604455,1 check for Adjustment if Positive or Negative. [Begin]
*IF ApInvHdr.nInvAdj > 0 AND SEEK(PADR(laData[2],12)+PADR(laData[1],8)+'H','APDIST')
IF ApInvHdr.nInvAdj <> 0 AND SEEK(PADR(laData[2],12)+PADR(laData[1],8)+'H','APDIST')
*B604455,1 check for Adjustment if Positive or Negative. [End]

  =SEEK(PADR(laData[1],8),'ApVendor')
  SELECT APDIST
  LOCATE REST WHILE cInvNo+cVendCode+cApdtrtyp = PADR(laData[2],12)+PADR(laData[1],8)+'H' ;
              FOR   cApDActId='C' AND cAPDStat <> 'V' AND ;
                    nApDAmnt =0   AND LEFT(cApDRef,3)='ADJ'
  lcApDRef = cApDRef
  SELECT ApPaymnt
  *B607481,1 ALB Fix bug in the adjustment record in APDIST [BEGIN]
  *=SEEK('P'+ApInvHdr.cVenPMeth+APDIST.cApDRef)
  =SEEK('P'+ApDIST.cApDTRTYP+APDIST.cApDRef)
  *LOCATE REST WHILE cpaytype+cpaymeth+cpaydocno+cbnkcode+cchkacct = ;
                    'P'+ApInvHdr.cVenPMeth+APDIST.cApDRef ;
              FOR cPayClNo = PADR(laData[1],8) AND cPayStat <> 'V' AND nPayAmnt = 0
  LOCATE REST WHILE cpaytype+cpaymeth+cpaydocno+cbnkcode+cchkacct = ;
                    'P'+ApDIST.cApDTRTYP+APDIST.cApDRef ;
              FOR cPayClNo = PADR(laData[1],8) AND cPayStat <> 'V' AND nPayAmnt = 0
  *B607481,1 ALB Fix bug in the adjustment record in APDIST [END]
  IF FOUND()
    SELECT APDIST
    =SEEK(PADR(laData[2],12)+PADR(laData[1],8)+'H')
    SCAN REST WHILE cInvNo+cVendCode+cApdtrtyp = PADR(laData[2],12)+PADR(laData[1],8)+'H' ;
              FOR   cAPDStat <> 'V' AND cApDRef = lcApDRef
      =RLOCK()
      REPLACE cAPDStat WITH 'V'
      UNLOCK
      SCATTER MEMVAR
      IF m.cApDActId='J'
        SELECT ApInvHdr
        =RLOCK()
        REPLACE nInvAdj WITH nInvAdj + m.nApDAmnt
        UNLOCK
        SELECT ApVendor
        =RLOCK()
        REPLACE nVenBal WITH nVenBal - m.nApDAmnt
        UNLOCK
        SELECT ApVenHst
        =SEEK(PADR(laData[1],8)+lcDistYer)
        =RLOCK()
        REPLACE nVnHAdj WITH nVnHAdj + m.nApDAmnt
        UNLOCK
      ENDIF
      m.nApDAmnt = -m.nApDAmnt
      m.nEqvAmnt = -m.nEqvAmnt
      m.cStatus  = 'A'
      INSERT INTO (lcTmpDist) FROM MEMVAR
      =gfAdd_Info(lcTmpDist)
    ENDSCAN
    SELECT ApPaymnt
    =RLOCK()
    REPLACE cPayStat  WITH 'V' ,;
            dPayVDate WITH gdSysDate
    UNLOCK
    =gfTmp2Mast('APDIST',lcTmpDist)
  ENDIF
ENDIF
WAIT CLEAR

*!*************************************************************
*! Name      : lfChckACst
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 05/27/99
*! Purpose   : Function to calculate the actual cost and check
*!             that none of the actual cost will have a negative amount.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : .T. If none of the actual cost will have
*!                          a negative amount.
*!                      .F. Otherwise.
*!*************************************************************
*! Example            : =lfChckACst()
*!*************************************************************
*B602946,1 this function was added by HS for the bug entry B602946,1.
*!*************************************************************
*
FUNCTION lfChckACst

PRIVATE llReturn  , lcExStatus , lcFullStat , lcCDXFile , lcMssgText ,;
        lcHdrFile , lcHActFild , lcHFActFld , lcHKeyVal , lnActCstNo ,;
        lcDetFile , lcDActFild , lcDFActFld , lcDKey    , lcDForCond ,;
        lcDKeyVal , lcDQtyFild , m.cIMTyp   , lcBomType , lnAmntChng ,;
        lnCount   , lcCount

llReturn = .T.

STORE '' TO lcExStatus , lcFullStat , lcCDXFile , lcMssgText

lcFullStat = SET('FULLPATH')
SET FULLPATH ON

*-- Create a temp. file to hold the calculated actual cost for PO and CT
*-- header files.
CREATE CURSOR &lcActCstHd;
       (cIMTyp C(1)        , cTktNo C(6)        , nRecNo N(10,0)    ,;
        nActCost1 N(13,3)  , nActCost2 N(13,3)  , nActCost3 N(13,3) ,;
        nActCost4 N(13,3)  , nActCost5 N(13,3)  ,;
        nFActCost1 N(13,3) , nFActCost2 N(13,3) , nFActCost3 N(13,3) ,;
        nFActCost4 N(13,3) , nFActCost5 N(13,3))

ZAP
lcCDXFile = STRTRAN(DBF() , '.TMP' , '.CDX')
INDEX ON cIMTyp + cTktNo TAG (lcActCstHd) OF (lcCDXFile)

*B802710,1 MAN Changed the total quantity to have 3 decimal places 
*-- Create a temp. file to hold the calculated actual cost for PO and CT
*-- detail files.
*CREATE CURSOR &lcActCstDt;
       (cIMTyp C(1)        , cTktNo C(6)        , nRecNo N(10,0)    ,;
        cRSession C(6)     , cShipNo C(6)       , cItem C(19)       ,;
        cColor C(6)        , cLineNo C(6)       , nTotQty N(7,0)    ,;
        nActCost1 N(13,3)  , nActCost2 N(13,3)  , nActCost3 N(13,3) ,;
        nActCost4 N(13,3)  , nActCost5 N(13,3)  ,;
        nFActCost1 N(13,3) , nFActCost2 N(13,3) , nFActCost3 N(13,3) ,;
        nFActCost4 N(13,3) , nFActCost5 N(13,3))
*B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
*CREATE CURSOR &lcActCstDt;
       (cIMTyp C(1)        , cTktNo C(6)        , nRecNo N(10,0)    ,;
        cRSession C(6)     , cShipNo C(6)       , cItem C(19)       ,;
        cColor C(6)        , cLineNo C(6)       , nTotQty N(10,3)    ,;
        nActCost1 N(13,3)  , nActCost2 N(13,3)  , nActCost3 N(13,3) ,;
        nActCost4 N(13,3)  , nActCost5 N(13,3)  ,;
        nFActCost1 N(13,3) , nFActCost2 N(13,3) , nFActCost3 N(13,3) ,;
        nFActCost4 N(13,3) , nFActCost5 N(13,3))

CREATE CURSOR &lcActCstDt;
       (cIMTyp C(1)        , cTktNo C(6)        , nRecNo N(10,0)    ,;
        cRSession C(6)     , cShipNo C(6)       , cItem C(19)       ,;
        cColor C(6)        , cLineNo C(6)       , nTotQty N(10,3)    ,;
        nActCost1 N(13,6)  , nActCost2 N(13,6)  , nActCost3 N(13,6) ,;
        nActCost4 N(13,6)  , nActCost5 N(13,6)  ,;
        nFActCost1 N(13,6) , nFActCost2 N(13,6) , nFActCost3 N(13,6) ,;
        nFActCost4 N(13,6) , nFActCost5 N(13,6))
*B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
ZAP
lcCDXFile = STRTRAN(DBF() , '.TMP' , '.CDX')
INDEX ON cIMTyp + cRSession + cShipNo + cTKtNo + cItem + cColor + cLineNo;
         TAG (lcActCstDt) OF (lcCDXFile)

SET FULLPATH &lcFullStat

lcExStatus = SET('EXACT')
SET EXACT OFF

*-- Set the needed order tags
*B802348,1 (Start)
*IF 'I' $ lcVenInvTypes OR 'R' $ lcVenInvTypes
IF ('I' $ lcVenInvTypes OR 'R' $ lcVenInvTypes) AND USED("POSHDR")
*B802348,1 (End)
  SET ORDER TO TAG POSHDR IN POSHDR
  SET ORDER TO TAG POSREC IN POSLN
ENDIF
IF 'MF' $ gcCmpModules AND laData[44] = gcBaseCurr AND 'M' $ lcVenInvTypes
  SET ORDER TO TAG CUTTKTH IN CUTTKTH 
  SET ORDER TO TAG CUTREC  IN CUTTKTL
ENDIF

*B802348,1 (Start)
*IF 'L' $ lcVenInvTypes OR 'F' $ lcVenInvTypes
IF ('L' $ lcVenInvTypes OR 'F' $ lcVenInvTypes) AND USED("POFHDR")
*B802348,1 (End)
  SET ORDER TO TAG POFHDR IN POFHDR
  SET ORDER TO TAG POFREC IN POFLN
ENDIF

*-- Recall the deleted records
SELECT (lcAPInvTkt)
RECALL ALL

*-- Recall the deleted records and set the needed order tag
SELECT (lcApVInvDt)
RECALL ALL

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
SELECT (lcAPInvTkt)
REPLACE ALL nAPAplAmnt WITH nAPAplAmnt * -1,;
            nAPAplPric WITH nAPAplPric * -1 FOR (cIMTyp $ 'RF' AND (nAPAplAmnt < 0 ))

SELECT (lcApVInvDt)
REPLACE ALL nApAprAmnt WITH nApAprAmnt * -1,;
            nApAprPric WITH nApAprPric * -1,;
            nApPrice   WITH nApPrice   * -1,;
            nApAmnt    WITH nApAmnt    * -1 FOR (cIMTyp $ 'RF' AND (nApAprAmnt < 0 ))
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
SET ORDER TO TAG ITEM
GO TOP

*-- Do while there is no troubles in the records locking and the end of file
*-- has not been reached yet.
DO WHILE llReturn .AND. !EOF()
  
  *-- lcHdrFile   Variable to hold the name of the header file for the
  *--             current document type.
  *-- lcHActFild  Variable to hold the name of the actual cost field in the
  *--             header file for the current document type.
  *-- lcHFActFld  Variable to hold the name of the actual cost in foreign
  *--             currency field in the header file for the current
  *--             document type.
  *-- lcHKeyVal   Variable to hold the key value to be used while searching
  *--             for the record that will be updated in the header file.
  *-- lcDetFile   Variable to hold the name of the detail file for the
  *--             current document type.
  *-- lcDActFild  Variable to hold the name of the actual cost field in the
  *--             detail file for the current document type.
  *-- lcDFActFld  Variable to hold the name of the actual cost in foreign
  *--             currency field in the detail file for the current
  *--             document type.
  *-- lcDKey      Variable to hold the expression that will be used to get
  *--             the key value that will be used for searching for the
  *--             record that will be updated in the detail file.
  *-- lcDForCond  Variable to hold a string of the expression that will be
  *--             used as the FOR expression while searching for the record
  *--             that will be updated in the detail file.
  *-- lcDQtyFild  Variable to hold the name of the total quantity field in
  *--             the detail file for the current document type.
  
  STORE '' TO lcHdrFile  , lcHActFild , lcHFActFld , lcHKeyVal  ,;
              lcDetFile  , lcDActFild , lcDFActFld , lcDKey     ,;
              lcDForCond , lcDQtyFild
  
  
  *-- lnActCstNo  Variable to hold the number of actual cost fields for the
  *--             current document type.
  
  STORE 0  TO lnActCstNo
  
  DO CASE
    *-- Case of document type "General Expense"
    CASE cIMTyp = 'G'
      SEEK 'H'
      IF RECNO(0) <> 0
        GO RECNO(0)
      ENDIF
      LOOP
    
    *-- Case of document types "Style Purchase Order", "Style Purchase
    *-- Order Return" and "Shipment"
    CASE INLIST(cIMTyp , 'I' , 'R' , 'S')
      
      *-- If the document type is not "Shipment"
      *B603738,1 KHM 07/20/2000 (Begin) Commenting the IF command in order
      *B603738,1                to update the actual cost of the shipment 
      *B603738,1                in the poshdr file as well as the posln file.
      *IF cIMTyp <> 'S'
      *B603738,1 KHM 07/20/2000 (End)
      
        lcHdrFile  = 'POSHDR'
        lcHActFild = 'nAct_Cost'
        lcHFActFld = 'nFActCost'
        lcHKeyVal  = IIF(cIMTyp = 'R' , 'R' , 'P')
      
      *B603738,1 KHM 07/20/2000 (Begin) Commented out.
      *ENDIF    && End of IF cIMTyp <> 'S'
      *B603738,1 KHM 07/20/2000 (End)
      
      lnActCstNo  = 5
      lcDetFile  = 'POSLN'
      lcDActFild = 'nEActCost'
      lcDFActFld = 'nAct_Cst'

      *B603390,1 KHM 07/20/2000 (Begin) Assigning the appropriate values
      *B603390,1                and setting the correct index in case 
      *B603390,1                of shipment.
      *lcDKey     = "IIF(m.cIMTyp = 'R' , 'R' , 'P') + cTKtNo + cRSession " +;
                   "+ &lcApVInvDt..ShipNo + Item + STR(LineNo , 6)"
      IF cIMTyp <> 'S'
        *B606531,1 ALB Fix the invoice saving with po inside shippment order [Begin]
        *lcDKey     = "IIF(m.cIMTyp = 'R' , 'R' , 'P') + cTKtNo + cRSession " +;
                     "+ &lcApVInvDt..ShipNo + Item + STR(LineNo , 6)"
        lcDKey     = "IIF(m.cIMTyp = 'R' , 'R' , 'P') + cTKtNo + cRSession " 
        *B606531,1 ALB Fix the invoice saving with po inside shippment order [end]
      ELSE
        SET ORDER TO TAG POSHREC IN POSLN
        lcDKey = "&lcApVInvDt..ShipNo + cRSession + 'P' "
      ENDIF
      *B603390,1 KHM 07/20/2000 (End)
      
      *B603390,1 KHM 07/20/2000 (Begin) Assinging the appropriate values
      *B603390,1                for the FOR condition in case of shipment.
      *lcDForCond = ".T."
      IF cIMTyp <> 'S'
        *B606531,1 ALB Fix the invoice saving with po inside shippment order [Begin]
        *lcDForCond = ".T."
        lcDForCond = "Style = &lcAPInvTkt..Item AND STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
        *B606531,1 ALB Fix the invoice saving with po inside shippment order [end]
      ELSE
        *B603827,1 KHM 08/27/2000 (Begin) Adding the check of the Po in 
        *B603827,1                order to update the actual cost for each 
        *B603827,1                PO when the shipment has multipule POs.
        *lcDForCond = "Style = &lcAPInvTkt..Item AND STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
        lcDForCond = "PO = cTKtNo AND Style = &lcAPInvTkt..Item AND STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
        *B603827,1 KHM 08/27/2000 (End)
      ENDIF  
      *B603390,1 KHM 07/20/2000 (End)
      
      lcDQtyFild = 'TotQty'
    
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcHdrFile  = 'CUTTKTH'
      lcHActFild = 'nAct_Cost'
      lcHKeyVal  = ''
      lnActCstNo = 5
      lcDetFile  = 'CUTTKTL'
      lcDActFild = 'nAct_Cst'
      lcDKey     = "cRSession + cTKtNo + Item"
      lcDForCond = "STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
      lcDQtyFild = 'TotQty'

    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')    
      lcHdrFile  = 'POFHDR'
      lcHActFild = 'nEActCost'
      lcHFActFld = 'nAct_Cost'
      lcHKeyVal  = IIF(cIMTyp = 'L' , 'P' , 'R')
      lnActCstNo = 4
      lcDetFile  = 'POFLN'
      lcDActFild = 'nEActCost'
      lcDFActFld = 'nAct_Cost'
      lcDKey     = "IIF(m.cIMTyp = 'L' , 'P' , 'R') + cTKtNo + cRSession " +;
                   "+ SUBSTR(ITEM , 1 , 7) + Color"
      
      lcDForCond = "STR(LineNo , 6) = STR(&lcAPInvTkt..LineNo , 6)"
      lcDQtyFild = 'nFabTotQty'

    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    CASE INLIST(cIMTyp , 'T')
      lcAlias = ALIAS()    
      lcHdrFile  = 'MMFGORDH'
      lcHActFild = 'nAct_Cost'
      lcHKeyVal  = IIF(cIMTyp = 'T' , '' , '')
      lnActCstNo = 4
      lcDetFile  = 'MMFGORDD'
      lcDActFild = 'nAct_Cost'
      lcDKey     = "cRSession + cTKtNo + SUBSTR(ITEM , 1 , 7) + Color"
      lcDForCond = ".T."
      lcDQtyFild = 'nmfgTotQty'
      SELECT(lcDetFile)
      SET ORDER TO TAG Mfgrec
      SELECT (lcAlias)
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    CASE INLIST(cIMTyp , 'A' )
      lcHdrFile  = 'POSHDR'
      lcHActFild = 'nAct_Cost'
      lcHFActFld = 'nFActCost'
      lcHKeyVal  = cIMTyp
      lnActCstNo  = 5
      lcDetFile  = 'POSLN'
      lcDActFild = 'nEActCost'
      lcDFActFld = 'nAct_Cst'
      lcDKey     = "'A' + cTKtNo + cRSession + &lcApVInvDt..ShipNo + Item + STR(LineNo , 6)"
      lcDForCond = ".T."
      lcDQtyFild = 'TotQty'

    CASE INLIST(cIMTyp , 'D' )
      lcHdrFile  = 'POSHDR'
      lcHActFild = 'nAct_Cost'
      lcHFActFld = 'nFActCost'
      lcHKeyVal  = cIMTyp
      lnActCstNo  = 5
      lcDetFile  = 'POSLN'
      lcDActFild = 'nEActCost'
      lcDFActFld = 'nAct_Cst'
      lcDKey     = "'D' + cTKtNo + cRSession + &lcApVInvDt..ShipNo + Item + STR(LineNo , 6)"
      lcDForCond = ".T."
      lcDQtyFild = 'TotQty'

    *E301995,1 Alb Add Adorment order to invoice line [End]
  ENDCASE
  
  m.cIMTyp = cIMTyp
  
  *-- Scan while there no troubles in the records locking and the document
  *-- type didn't change for the records with status <> same
  *-- Note: The seek() function in the for condition is not used as a
  *--       condition it's used to replace the record pointer on the first
  *--       corresponding record in the file (lcAPInvTkt).
  SCAN WHILE llReturn .AND. cIMTyp = m.cIMTyp;
         FOR cStatus <> 'S' .AND.;
             SEEK(cVendCode + cApInvNo + cAPVILNo , lcAPInvTkt)
    
    *-- Flag to know if the record has been deleted
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]    
    *llDeleted = (cStatus='D')
    llDeleted = (cStatus='D') OR ((nRecNo = 0) AND (cStatus= 'S'))
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    *-- Variable to hold the record number of the corresponding record in
    *-- the master file
    lnRecNo   = nRecNo
    
    *-- Replace the record pointer on the corresponding record in the
    *-- master file.
    *?AMIN
    *B605040,1  Voiding record out of range [Start]
    *IF lnRecNo > 0  
    IF BETWEEN( lnRecNo ,1, RECCOUNT('APVINVDT'))
    *B605040,1  Voiding record out of range [End]    
      GO lnRecNo IN APVINVDT
    ENDIF    && End of IF lnRecNo > 0
    
    lcBomType  = cBomType
    
    *-- Get the change that has been made to the applied amount for this
    *-- record
    lnAmntChng = IIF(llDeleted   , 0 , nApAprAmnt) -;
                 IIF(lnRecNo = 0 , 0 , APVINVDT.nApAprAmnt)
    
    *B603827,1 KHM 08/27/2000 (Begin) Adding the IF command to check if
    *B603827,1                invoicing by shimpment the do lfUpdPoHAc() 
    *B603827,1                that will update the PosHdr actual cost
    IF cIMTyp = "S"
      *-- This function is used to update the actual cost for the poshdr
      *-- file in case of invoicing by shimpment.
      =lfUpdPoHAC()
    ELSE
    *B603827,1 KHM 08/27/2000 (End)
    
      *-- If there is a header file that should be updated for the current
      *-- document type.
      IF !EMPTY(lcHdrFile)
      
        *-- If there is no records for this document number in the actual
        *-- cost temp. header file
        IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
          m.cTktNo = cTktNo
         
          *-- Go to the record that will be updated in the header file
          SELECT (lcHdrFile)
          *B603827,1 KHM 08/27/2000 (Begin) Changing the seek command to be 
          *B603827,1                IF seek in order to don't try to lock the 
          *B603827,1                record or update the actual cost in the 
          *B603827,1                header file in case of not seek in the 
          *B603827,1                header file

          *SEEK lcHKeyVal + m.cTktNo
          IF SEEK (lcHKeyVal + m.cTktNo)
          *B603827,1 KHM 08/27/2000 (End)
          
            *-- Attempt to lock the record
            *B132275,1  TMI [Start] use the local version of gfObj_Lock function instead
            *IF gfObj_Lock(.T.)
            IF lfObj_Lock(.T.)
              *B132275,1  TMI [End  ] 
            
              m.nRecNo = RECNO()
          
              STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3 ,;
                         m.nActCost4  , m.nActCost5  ,;
                         m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,;
                         m.nFActCost4 , m.nFActCost5
          
              *-- If there is no fields for the actual cost in foreign currency
              IF EMPTY(lcHFActFld)
                *-- Get the actual cost
                FOR lnCount = 1 TO lnActCstNo
                  lcCount            = ALLTRIM(STR(lnCount))
                  m.nActCost&lcCount = &lcHActFild.&lcCount
                ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
              ELSE    && Else [If there is fields for the actual cost in foreign currency]
                *-- Get the actual cost
                FOR lnCount = 1 TO lnActCstNo
                  lcCount             = ALLTRIM(STR(lnCount))
                  m.nActCost&lcCount  = &lcHActFild.&lcCount
                  m.nFActCost&lcCount = &lcHFActFld.&lcCount
                ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
              ENDIF    && End of IF EMPTY(lcHFActFld)
          
              *-- Add a corresponding record in the temp. file
              INSERT INTO &lcActCstHd FROM MEMVAR
            ELSE    && Else [If the attempt to lock the record failed]
              llReturn = .F.
              EXIT
            ENDIF    && End of IF gfObj_Lock(.T.)
    
          *B603827,1 KHM 08/27/2000 (Begin) End of IF SEEK(lcHKeyVal + m.cTktNo)
          ENDIF      
          *B603827,1 KHM 08/27/2000 (End)
          
        ENDIF    && End of IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
      
        *-- Update total actual cost in the temp. header file
        SELECT (lcActCstHd)
      
        REPLACE nActCost&lcBomType WITH nActCost&lcBomType + (lnAmntChng;
                                        &lcExSin1 laData[45] &lcExSin2;
                                        laData[48])
      
        *-- Update total actual cost in foreign currency in the temp. header
        *-- file
        IF !EMPTY(lcHFActFld)
          REPLACE nFActCost&lcBomType WITH nFActCost&lcBomType + lnAmntChng
        ENDIF    && End of IF !EMPTY(lcHFActFld)
      ENDIF    && End of IF !EMPTY(lcHdrFile)
    
    *B603827,1 KHM 08/27/2000 (Begin) End of IF cIMTyp = 'S'
    ENDIF
    *B603827,1 KHM 08/27/2000 (End)
    SELECT (lcAPInvTkt)
    *-- Scan the corresponding records in the detail file (lcAPInvTkt) with
    *-- receiving session not empty.
    
    SCAN REST;
        WHILE cVendCode + cApInvNo + cApVILNo = &lcApVInvDt..cVendCode +;
              &lcApVInvDt..cApInvNo + &lcApVInvDt..cAPVILNo;
              FOR !EMPTY(cRSession)

      *-- Flag to know if the record has been deleted
      *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]    
      *llDeleted = (cStatus='D')
      llDeleted = (cStatus='D') OR ((nRecNo = 0) AND (cStatus= 'S'))
      *B605911,1 ALB Fix the contribution process  to save the invoice [End]
      *-- Variable to hold the record number of the corresponding record in
      *-- the master file
      lnRecNo   = nRecNo
      
      *-- Replace the record pointer on the corresponding record in the
      *-- master file.
      *? amin
      *B605040,1  Voiding record out of range  [Start]
      *IF lnRecNo > 0  
      IF BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
        GO lnRecNo IN APINVTKT
      ENDIF
      *B605040,1  Voiding record out of range  [End]
      
      *-- Get the change that has been made to the applied amount for this
      *-- record
      
      lnAmntChng = IIF(llDeleted   , 0 , nApAplAmnt) -;
                   IIF(lnRecNo = 0 , 0 , APINVTKT.nApAplAmnt)
      
      *-- If there is no records for this document number in the actual
      *-- cost temp. detail file

      IF !SEEK(cIMTyp + cRSession + &lcApVInvDt..ShipNo + cTKtNo + Item +;
               Color + STR(LineNo,6) , lcActCstDt)
        STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3 ,;
                   m.nActCost4  , m.nActCost5  ,;
                   m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,;
                   m.nFActCost4 , m.nFActCost5
        
        m.cIMTyp    = cIMTyp
        m.cRSession = cRSession
        m.cShipNo   = &lcApVInvDt..ShipNo
        m.cTktNo    = cTKtNo
        m.cItem     = Item
        m.cColor    = Color
        m.cLineNo   = STR(LineNo,6)
        *-- Go to the record that will be updated in the header file
        lcDKeyVal = EVALUATE(lcDKey)
        SELECT (lcDetFile)
        LLSEEK = SEEK (lcDKeyVal)
        LOCATE REST;
              WHILE EVALUATE(SYS(14,VAL(SYS(21)))) = lcDKeyVal;
                FOR EVALUATE(lcDForCond)
        
        *-- Attempt to lock the record
        *B132275,1  TMI [Start] use the local version of gfObj_Lock function instead
        *IF gfObj_Lock(.T.)
        IF lfObj_Lock(.T.)
          *B132275,1  TMI [End  ] 
          m.nRecNo    = RECNO()
          m.nTotQty   = &lcDQtyFild
          
          *-- If there is no fields for the actual cost in foreign currency
          IF EMPTY(lcDFActFld)
            
            *-- Get the actual cost
            FOR lnCount = 1 TO lnActCstNo
              lcCount            = ALLTRIM(STR(lnCount))
              m.nActCost&lcCount = &lcDActFild.&lcCount
            ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
          ELSE    && Else [If there is fields for the actual cost in foreign currency]
            
            *-- Get the actual cost
            FOR lnCount = 1 TO lnActCstNo
              lcCount             = ALLTRIM(STR(lnCount))
              m.nActCost&lcCount  = &lcDActFild.&lcCount
              m.nFActCost&lcCount = &lcDFActFld.&lcCount
            ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
          ENDIF    && End of IF EMPTY(lcDFActFld)
          
          *-- Add a corresponding record in the temp. file
          INSERT INTO &lcActCstDt FROM MEMVAR
        ELSE    && Else [If the attempt to lock the record failed]
          llReturn = .F.
          EXIT
        ENDIF    && End of IF gfObj_Lock(.T.)
      ENDIF    && End of IF !SEEK(cIMTyp + cRSession + ...
      
      SELECT (lcActCstDt)
      
      *-- Update total actual cost in the temp. detail file
      REPLACE nActCost&lcBomType WITH nActCost&lcBomType + ((lnAmntChng;
                                      / nTotQty) &lcExSin1 laData[45];
                                      &lcExSin2 laData[48])
      
      *-- Update total actual cost in foreign currency in the temp. detail
      *-- file
      IF !EMPTY(lcDFActFld)
        REPLACE nFActCost&lcBomType WITH nFActCost&lcBomType +;
                                         (lnAmntChng / nTotQty)
        
      ENDIF    && End of IF !EMPTY(lcDFActFld)
    ENDSCAN    && End of SCAN REST WHILE cVendCode + cApInvNo + cApVILNo ...
    
    SELECT (lcApVInvDt)
  ENDSCAN    && End of   SCAN WHILE llReturn .AND. cIMTyp = m.cIMTyp ...
ENDDO    && End of DO WHILE llReturn .AND. !EOF()

*-- If there was no troubles found in the records locking
 IF llReturn
  
  *-- Search for a record with negative actual cost in the temp. actual
  *-- cost header file
  SELECT (lcActCstHd)
  LOCATE FOR nActCost1 < 0 .OR.;
             nActCost2 < 0 .OR.;
             nActCost3 < 0 .OR.;
             nActCost4 < 0 .OR.;
             nActCost5 < 0
  
  *-- If there is a record with negative actual cost in the temp. actual
  *-- cost header file
  IF FOUND()
    llReturn  = .F.
    
    *-- Get the variable part of the error message
    DO CASE
      CASE cIMTyp = 'I'
        lcMssgText = 'style purchase order'
        
      CASE cIMTyp = 'R'
        lcMssgText = 'style purchase order return'
        
      CASE cIMTyp = 'M'
        lcMssgText = 'cutting ticket'
        
      CASE cIMTyp = 'L'
        lcMssgText = 'material PO'
        
      CASE cIMTyp = 'F'
        lcMssgText = 'material PO return'
      *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]        
      CASE cIMTyp = 'T'
        lcMssgText = 'material MFG order'
      *B605911,1 ALB Fix the contribution process  to save the invoice [End]
      *E301995,1 Alb Add Adorment order to invoice line [Begin]
      CASE cIMTyp = 'A'
        lcMssgText = 'Ador P/O'
      CASE cIMTyp = 'D'
        lcMssgText = 'Dye P/O'

      *E301995,1 Alb Add Adorment order to invoice line [End]
    ENDCASE
    lcMssgText = lcMssgText + ' No. ' + cTKtNo
    
    *-- Message : 04188
    *-- "The debit amount applied to (lcMssgText) will produce a negative "
    *-- "actual cost. Cannot proceed with the saving process.             "
    *-- Button : 00000 
    *-- "                          <    Ok    >                           "
    =gfModalGen("TRM04188B00000" , "DIALOG" , lcMssgText)
  ENDIF    && End of IF FOUND()
ENDIF    && End of IF llReturn

*-- If there was no troubles found in the records locking and no records
*-- with negative actual cost found in the temp. actual cost header file.
IF llReturn
  
  *-- Search for a record with negative actual cost in the temp. actual
  *-- cost detail file.
  SELECT (lcActCstDt)
  LOCATE FOR nActCost1 < 0 .OR.;
             nActCost2 < 0 .OR.;
             nActCost3 < 0 .OR.;
             nActCost4 < 0 .OR.;
             nActCost5 < 0
  
  *-- If there is a record with negative actual cost in the temp. actual
  *-- cost detail file
  IF FOUND()
    llReturn  = .F.
    
    *-- Get the variable part of the error message
    DO CASE
      CASE cIMTyp = 'I'
        lcMssgText = 'style purchase order No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'R'
        lcMssgText = 'style purchase order return No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'S'
        lcMssgText = 'shipment No. ' + cShipNo +;
                     ', receiving session No. ' + cRSession +;
                     ', style purchase order No. ' + cTKtNo +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'M'
        lcMssgText = 'cutting ticket No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'L'
        lcMssgText = 'material PO No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
        
      CASE cIMTyp = 'F'
        lcMssgText = 'material PO return No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
      *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]        
      CASE cIMTyp = 'T'
        lcMssgText = 'material MFG order No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession
      *B605911,1 ALB Fix the contribution process  to save the invoice [End]
      *E301995,1 Alb Add Adorment order to invoice line [Begin]
      CASE cIMTyp = 'A'
        lcMssgText = 'Adorment order No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)
      CASE cIMTyp = 'D'
        lcMssgText = 'Dying order No. ' + cTKtNo +;
                     ', receiving session No. ' + cRSession +;
                     ', line No. ' + ALLTRIM(cLineNo)

      *E301995,1 Alb Add Adorment order to invoice line [end]
    ENDCASE
    
    *-- Message : 04188
    *-- "The debit amount applied to (lcMssgText) will produce a negative "
    *-- "actual cost. Cannot proceed with the saving process.             "
    *-- Button : 00000 
    *-- "                          <    Ok    >                           "
    =gfModalGen("TRM04188B00000" , "DIALOG" , lcMssgText)
  ENDIF    && End of IF FOUND()
ENDIF    && End of IF llReturn

*-- If troubles where found in the records locking or some of the records
*-- in the temp. actual cost header file or detail file where found.
IF !llReturn
  
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
  SELECT (lcAPInvTkt)
  REPLACE ALL nAPAplAmnt WITH nAPAplAmnt * -1,;
              nAPAplPric WITH nAPAplPric * -1 FOR (cIMTyp $ 'RF' AND (nAPAplAmnt > 0 ))

  SELECT (lcApVInvDt)
  REPLACE ALL nApAprAmnt WITH nApAprAmnt * -1,;
              nApAprPric WITH nApAprPric * -1,;
              nApPrice   WITH nApPrice   * -1,;
              nApAmnt    WITH nApAmnt    * -1 FOR (cIMTyp $ 'RF' AND (nApAprAmnt > 0 ))
  *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
  
  *-- Delete the records that have status same or deleted from the temp.
  *-- files (lcApVInvDt) and (lcAPInvTkt)
  SELECT (lcApVInvDt)
  DELETE ALL FOR INLIST(cStatus,'D','S')
  SELECT (lcAPInvTkt)
  DELETE ALL FOR INLIST(cStatus,'D','S')
  
  *-- Unlock the records that had been locked
  =lfUnLckPOF()
ENDIF    && End of IF !llReturn

RETURN llReturn

*!*************************************************************
*! Name      : lfUnLckPOF
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 06/01/99
*! Purpose   : Function to unlock the records that we have locked
*!             in the PO files and CT files (POSHDR, POSLN,
*!             CUTTKTH, CUTTKTL, POFHDR and POFLN).
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =lfUnLckPOF()
*!*************************************************************
*B602946,1 this function was added by HS for the bug entry B602946,1.
*!*************************************************************
*
FUNCTION lfUnLckPOF

PRIVATE lcFile , lcIMTyp , lnRecNo

*-- Unlock the header files [Begin]
SELECT (lcActCstHd)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp
  
  *-- Get the name of the header file of the current document type to
  *-- unlock the records that had been locked in it
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"
    
    *B603738,1 KHM 07/20/2000 (Begin) Unlock the PosHdr in case of shipment also.
    *CASE INLIST(cIMTyp , 'I' , 'R')
    CASE INLIST(cIMTyp , 'I' , 'R','S')
    *B603738,1 KHM 07/20/2000 (End)
    
      lcFile = 'POSHDR'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTH'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFHDR'
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    CASE INLIST(cIMTyp , 'A','D')
      lcFile = 'POSHDR'
    *E301995,1 Alb Add Adorment order to invoice line [end]
  ENDCASE
  
  lcIMTyp = cIMTyp
  
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    
    *-- Unlock the records that had been locked in the master file
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    =gfObj_Lock(.F.)
    SELECT (lcActCstHd)
  ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost header temp. file 
USE

*-- Unlock the header files [End]

*-- Unlock the detail files [Begin]
SELECT (lcActCstDt)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp
  
  *-- Get the name of the detail file for the current document type to
  *-- unlock the records that had been locked in it
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"
    CASE INLIST(cIMTyp , 'I' , 'R' , 'S')
      lcFile = 'POSLN'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTL'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFLN'
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    CASE INLIST(cIMTyp , 'A','D')
      lcFile = 'POSLN'
    *E301995,1 Alb Add Adorment order to invoice line [end]
  ENDCASE
  
  lcIMTyp = cIMTyp
  
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    
    *-- Unlock the records that had been locked in the master file
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    =gfObj_Lock(.F.)
    SELECT (lcActCstDt)
  ENDSCAN
ENDDO

*-- Erase the actual cost detail temp. file 
USE

*-- Unlock the detail files [End]

*!*************************************************************
*! Name      : lfUpDatPOF
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 06/01/99
*! Purpose   : Function to Update the PO files and CT files
*!             (POSHDR, POSLN, CUTTKTH, CUTTKTL, POFHDR and POFLN).
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =lfUpDatPOF()
*!*************************************************************
*B602946,1 this function was added by HS for the bug entry B602946,1.
*!*************************************************************
*
FUNCTION lfUpdatPOF
PRIVATE lcFile , lcIMTyp , lnRecNo , lcScatFlds , lcGathFlds , laScatVal

*-- Update and unlock the header files [Begin]
SELECT (lcActCstHd)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp , lcScatFlds , lcGathFlds
  DIMENSION laScatVal[1]
  laScatVal = 0
  
  *-- Get the name of the header file, the scatter fields and the gather
  *-- fields for the current document type to update it and unlock the
  *-- records that had been locked.
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"

    *B603738,1 KHM 07/20/2000 (Begin) Updating the actual cost in the
    *B603738,1                poshdr file in case of shipment also.
    *CASE INLIST(cIMTyp , 'I' , 'R')
    CASE INLIST(cIMTyp , 'I' , 'R','S')
    *B603738,1 KHM 07/20/2000 (End)
    
      lcFile     = 'POSHDR'

      *B603520,1 SSE  03/19/2000 Swap the fields scattered [Begin]
      *lcScatFlds = 'LIKE nActCost? , nFActCost?'
      lcScatFlds = 'nActCost1 , nActCost2 , nActCost3 , nActCost4 ,;
                    nActCost5 , nFActCost1 , nFActCost2 , nFActCost3, ;
                    nFActCost4 , nFActCost5'
      *B603520,1 SSE  03/19/2000 [End]

      lcGathFlds = 'LIKE nAct_Cost? , nFActCost?'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTH'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cost?'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFHDR'
      *B603520,1 SSE  03/19/2000 Swap the fields scattered [Begin]
      *lcScatFlds = 'nActCost1  , nActCost2  , nActCost3  , nActCost4  , ;
      *              nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4'
      lcScatFlds = 'nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4 ,;
                    nActCost1 , nActCost2 , nActCost3 , nActCost4'
      *B603520,1 SSE  03/19/2000 [End]
      lcGathFlds = 'LIKE nEActCost? , nAct_Cost?'
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    *-- Case of document type "Material Cutting Ticket"
    CASE cIMTyp = 'T'
      lcFile = 'MMFGORDH'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cost?'
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    CASE INLIST(cIMTyp , 'A','D')
      lcFile     = 'POSHDR'
      lcScatFlds = 'nActCost1 , nActCost2 , nActCost3 , nActCost4 ,;
                    nActCost5 , nFActCost1 , nFActCost2 , nFActCost3, ;
                    nFActCost4 , nFActCost5'
      lcGathFlds = 'LIKE nAct_Cost? , nFActCost?'
    *E301995,1 Alb Add Adorment order to invoice line [end]
  ENDCASE
  
  lcIMTyp = cIMTyp
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    
    *-- Update the records of the master file
    SCATTER FIELDS &lcScatFlds TO laScatVal
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    GATHER FIELDS &lcGathFlds FROM laScatVal
    
    *-- Unlock the records that had been locked in the master file
    =gfObj_Lock(.F.)
    SELECT (lcActCstHd)
  ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost header temp. file 
USE

*-- Update and unlock the header files [End]


*-- Update and unlock the detail files [Begin]
SELECT (lcActCstDt)
GO TOP

DO WHILE !EOF()
  STORE '' TO lcFile , lcIMTyp , lcScatFlds , lcGathFlds
  DIMENSION laScatVal[1]
  laScatVal = 0
  
  *-- Get the name of the detail file, the scatter fields and the gather
  *-- fields for the current document type to update it and unlock the
  *-- records that had been locked.
  DO CASE
    *-- Case of document types "Style Purchase Order" and "Style Purchase
    *-- Order Return"

    *B603738,1 KHM 07/20/2000 (Begin) Updating the actual cost in the
    *B603738,1                posln file in case of shipment also.
    *CASE INLIST(cIMTyp , 'I' , 'R')
    CASE INLIST(cIMTyp , 'I' , 'R','S')
    *B603738,1 KHM 07/20/2000 (End)
    
      lcFile     = 'POSLN'

      *B603520,1 SSE  03/19/2000 Swap the fields scattered [Begin]
      *lcScatFlds = 'LIKE nActCost? , nFActCost?'
      lcScatFlds = "nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4 , ;
                    nFActCost5 , nActCost1 , nActCost2 , nActCost3 , ;
                    nActCost4 ,nActCost5"
      *B603520,1 SSE  03/19/2000 [End]
      lcGathFlds = 'LIKE nEActCost? , nAct_Cst?'
    *-- Case of document type "Cutting Ticket"
    CASE cIMTyp = 'M'
      lcFile = 'CUTTKTL'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cst?'
    *-- Case of document types "Material PO" and "Material PO Return"
    CASE INLIST(cIMTyp , 'L' , 'F')
      lcFile = 'POFLN'
      *B802471,1 (Start)
      *lcScatFlds = 'nActCost1  , nActCost2  , nActCost3  , nActCost4' +;
      *             'nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4'

      *B603520,1 SSE  03/19/2000 Swap the fields scattered [Begin]
      *lcScatFlds = 'nActCost1  , nActCost2  , nActCost3  , nActCost4 , ' +;
      *             'nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4'
      lcScatFlds = 'nFActCost1  , nFActCost2  , nFActCost3  , nFActCost4 , ' +;
                   'nActCost1 , nActCost2 , nActCost3 , nActCost4'
      *B603520,1 SSE  03/19/2000 [End]
      *B802471,1 (End)                 

      lcGathFlds = 'LIKE nEActCost? , nAct_Cost?'
    *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
    *-- Case of document type "Material Cutting Ticket"
    CASE cIMTyp = 'T'
      lcFile = 'MMFGORDD'
      lcScatFlds = 'LIKE nActCost?'
      lcGathFlds = 'LIKE nAct_Cost?'
    *B605911,1 ALB Fix the contribution process  to save the invoice [End]
    *E301995,1 Alb Add Adorment order to invoice line [Begin]
    CASE INLIST(cIMTyp , 'A','D')
      lcFile     = 'POSLN'
      lcScatFlds = "nFActCost1 , nFActCost2 , nFActCost3 , nFActCost4 , ;
                    nFActCost5 , nActCost1 , nActCost2 , nActCost3 , ;
                    nActCost4 ,nActCost5"
      lcGathFlds = 'LIKE nEActCost? , nAct_Cst?'
    *E301995,1 Alb Add Adorment order to invoice line [end]
  ENDCASE
  
  lcIMTyp = cIMTyp
  
  *-- Scan while the document type didn't change
  SCAN WHILE cIMTyp = lcIMTyp
    *-- Update the records of the master file
    SCATTER FIELDS &lcScatFlds TO laScatVal
    lnRecNo = nRecNo
    SELECT (lcFile)
    GO lnRecNo
    GATHER FIELDS &lcGathFlds FROM laScatVal
    
    *-- Unlock the records that had been locked in the master file
    =gfObj_Lock(.F.)
    SELECT (lcActCstDt)
  ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost detail temp. file 
USE
*-- Update and unlock the detail files [End]


*!*************************************************************
*! Name      : lfUpdPoHAC
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 08/27/2000
*! Purpose   : To update the actual cost in the poshdr file in case 
*!             of invoicing by shipment.
*!*************************************************************
*! Example            :  =lfUpdPoHAC()
*!*************************************************************
*B603827,1 KHM 08/27/2000 Added to properly update the actual
*B603827,1                cost in the poshdr file in case of shipment.
*!*************************************************************
FUNCTION lfUpdPoHAC
PRIVATE lnAlias

lnAlias = SELECT()

IF !EMPTY(lcHdrFile)
  *-- Scan the corresponding records in the detail file (lcAPInvTkt) with
  *-- receiving session not empty.
  SELECT (lcAPInvTkt)
  
  
  *B605040,1 Update the POSHdr file even if there is not contributation [Start]
  *SCAN REST;
  *      WHILE cVendCode + cApInvNo + cApVILNo = &lcApVInvDt..cVendCode +;
  *            &lcApVInvDt..cApInvNo + &lcApVInvDt..cAPVILNo;
  *      FOR !EMPTY(cRSession)

  SCAN REST;
        WHILE cVendCode + cApInvNo + cApVILNo = &lcApVInvDt..cVendCode +;
              &lcApVInvDt..cApInvNo + &lcApVInvDt..cAPVILNo
  *B605040,1 Update the POSHdr file even if there is not contributation [End]        


*MANSTART
    *-- Flag to know if the record has been deleted
    llDeleted = (cStatus='D')
    *-- Variable to hold the record number of the corresponding record in
    *-- the master file
    lnRecNo   = nRecNo
    
    *-- Replace the record pointer on the corresponding record in the
    *-- master file.
    *B605040,1  Voiding record out of range   [Start]
    *IF lnRecNo > 0    
    IF BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
      GO lnRecNo IN APINVTKT
    ENDIF    && End of IF lnRecNo > 0
   *B605040,1  Voiding record out of range   [Start]    

*    lnAmntChng = IIF(llDeleted   , 0 , nApAplAmnt) -;
*                 IIF(lnRecNo = 0 , 0 , APVINVDT.nApAprAmnt)
    lnAmntChng = IIF(llDeleted   , 0 , nApAplAmnt) -;
                 IIF(lnRecNo = 0 , 0 , APINVTKT.nApAplAmnt)

*MANEND

    *-- If there is no records for this document number in the actual
    *-- cost temp. header file
    IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
      m.cTktNo = cTktNo
      
      SELECT (lcHdrFile)    
      IF SEEK (lcHKeyVal + m.cTktNo)        
        *-- Attempt to lock the record
        *B132275,1  TMI [Start] use the local version of gfObj_Lock function instead
        *IF gfObj_Lock(.T.)
        IF lfObj_Lock(.T.)
          *B132275,1  TMI [End  ] 
          m.nRecNo = RECNO()
          
          STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3 ,;
                     m.nActCost4  , m.nActCost5  ,;
                     m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,;
                     m.nFActCost4 , m.nFActCost5
          
          *-- If there is no fields for the actual cost in foreign currency
          IF EMPTY(lcHFActFld)
            *-- Get the actual cost
            FOR lnCount = 1 TO lnActCstNo
              lcCount            = ALLTRIM(STR(lnCount))
              m.nActCost&lcCount = &lcHActFild.&lcCount
            ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
          ELSE    && Else [If there is fields for the actual cost in foreign currency]
            *-- Get the actual cost
            FOR lnCount = 1 TO lnActCstNo
              lcCount             = ALLTRIM(STR(lnCount))
              m.nActCost&lcCount  = &lcHActFild.&lcCount
              m.nFActCost&lcCount = &lcHFActFld.&lcCount
            ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
          ENDIF    && End of IF EMPTY(lcHFActFld)
          
          *-- Add a corresponding record in the temp. file
          INSERT INTO &lcActCstHd FROM MEMVAR
        ELSE    && Else [If the attempt to lock the record failed]
          llReturn = .F.
        ENDIF    && End of IF gfObj_Lock(.T.)
      ENDIF
    ENDIF    && End of IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
      
    *-- Update total actual cost in the temp. header file
    SELECT (lcActCstHd)      
    REPLACE nActCost&lcBomType WITH nActCost&lcBomType + (lnAmntChng;
                                    &lcExSin1 laData[45] &lcExSin2;
                                     laData[48])
      
    *-- Update total actual cost in foreign currency in the temp. header
    *-- file
    IF !EMPTY(lcHFActFld)
      REPLACE nFActCost&lcBomType WITH nFActCost&lcBomType + lnAmntChng
    ENDIF    && End of IF !EMPTY(lcHFActFld)
  ENDSCAN
ENDIF    && End of IF !EMPTY(lcHdrFile)

SELECT (lcApVInvDt)
=SEEK(cVendCode + cApInvNo + cAPVILNo , lcAPInvTkt)

SELECT(lnAlias)



*!**************************************************************************
*!
*!      Function: lfvUsrFld
*!
*!**************************************************************************
*C200178,1 AAN Function for update user field array with the vendor payment type.
FUNCTION lfvUsrFld
*-- Get position of packs in laUsrField
PRIVATE lnVenPYPs
lnVenPYPs = ASCAN(laUsrField,"CVENPYTYPE")
IF lnVenPYPs  > 0
  lnVenPYPs = ASUBSCRIPT(laUsrField,lnVenPYPs ,1)
  laUsrField[lnVenPYPs ,6] = APVENDOR.cVenPyType
ENDIF


 
*!*************************************************************
*! Name      : lfvAASAct
*! Developer : Amin Khodary  (KHM)
*! Date      : 10/23/2001
*! Purpose   : To get the WIP account based on Gl link.
*!*************************************************************
*! Example            :  =lfGetWIPAc()
*!*************************************************************
*B603827,1 AKA 10/23/2001
*!*************************************************************
FUNCTION lfvAASAct
PARAMETERS lcLinkCode,lcCatKey
PRIVATE lcGlAcnt
lcGlAcnt= ''
*B606980,1 ALB Fix error Alias not found when the AP not linked with GL [Begin]
IF llApGlLink
*IF laSetups[3,2]='Y'
*B606980,1 ALB Fix error Alias not found when the AP not linked with GL [End]
  IF SEEK(lcLinkCode+lcCatKey,'GL_LINK')
    lcGlAcnt = GL_LINK.GLACNT
    IF !SEEK(lcGlAcnt,'lcLinkChar') 
      lcGlAcnt = ''
    ENDIF  
  ELSE  
    =SEEK('DEFDEF','GL_LINK')
    lcGlAcnt = GL_LINK.GLACNT
  ENDIF
ELSE
  STORE lcDistAcct TO lcGlAcnt 
ENDIF  
RETURN lcGlAcnt 

*!*************************************************************
*! Name      : lfGetWIPAc
*! Developer : Amin Khodary  (KHM)
*! Date      : 10/23/2001
*! Purpose   : To get the WIP account based on Gl link code defined in poshdr file 
*!*************************************************************
*! Example            :  =lfGetWIPAc()
*!*************************************************************
*B603827,1 AKA 10/23/2001
*!*************************************************************
FUNCTION lfGetWIPAc
PARAMETER  lcHdrKey , lcHdrFile , lcHdrOred

PRIVATE lcGlAcct , lnArea, lcOrder , lnRecNo  

IF EMPTY(lcHdrKey) OR EMPTY(lcHdrFile) OR EMPTY(lcHdrOred)
  RETURN ""
ENDIF

lcGlAcct = ""
lnArea= SELECT()
lcOrder = ORDER()
lnRecNo = RECNO() 

SELECT (lcHdrFile) 
SET ORDER TO TAG (lcHdrOred)
IF SEEK(lcHdrKey)
  lcGlAcct = lfvAASAct(&lcHdrFile..LINK_CODE,"013") 
ENDIF

SELECT (lnArea)
SET ORDER TO (lcOrder)
IF BETWEEN(lnRecNo , 1, RECCOUNT() ) 
  GOTO lnRecNo 
ENDIF 
RETURN lcGlAcct 


*!*************************************************************
*! Name      : lfUpdApDist
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update AP Distribution 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcInvLineNo : Invoice Line#
*!                       lnAmount    : Amount
*!                       lcGlAccount : Gl Account
*!                       llRemvLine  : .T., Remove the line.
*!                                     Otherwise, Add or Edit the line.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdApDist()
*!*************************************************************
FUNCTION lfUpdApDist

PARAMETERS lcInvLineNo,lnAmount,lcGlAccount,lcType
PRIVATE lnAlias
lnAlias = SELECT()
*MAN Fix Total Amount to match the GLDIST
PRIVATE lnOApdAmnt 
lnOApdAmnt  = 0

*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
lnAmount = lnAmount * IIF(llDebitM AND !(lcType $ 'RF') ,-1,1)
*B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]

SELECT (lcTmpDist)
IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+STR(0,4))
  APPEND BLANK
  REPLACE CVENDCODE WITH laData[1]  ,;
          CINVNO    WITH laData[2]  ,;
          DAPDTRDAT WITH laData[50] ,;
          LAPDPOST  WITH .F.        ,;
          CAPDGLACT WITH laData[33] ,;
          CAPDACTID WITH 'A'        ,;
          CAPDREF   WITH laData[2]  ,;
          CFISFYEAR WITH lcDistYer  ,;
          CFSPPRDID WITH lcDistPrd  ,;
          CAPSESSNO WITH lcSequence ,;
          CAPDTRTYP WITH 'I'        ,;
          CSTATUS   WITH 'A'        ,;
          CCURRCODE WITH laData[44] ,;
          NEXRATE   WITH laData[45] ,;
          NCURRUNIT WITH laData[48] ,;
          NAPDAMNT WITH -laData[12] ,;
          NEQVAMNT WITH -laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48]
ENDIF

*B125114,1 MHM 03/27/2005 Seek for the correct record (exclude Voided one) [Start]
IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo) OR ;
    (SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo) AND CAPDSTAT  = 'V')
*B125114,1 MHM 03/27/2005 [End]
  
  *B602830,1 Add this condition to prevent adding a distribution line with
  *          0 amount [Begin]
  IF lnAmount <> 0
  *B602830,1 Add this condition to prevent adding a distribution line [End]
    
    APPEND BLANK
    *B603484,1 SSE 03/20/2000 Make Tax Code added in General Expenses [Begin] 
    *REPLACE CVENDCODE WITH laData[1]  ,;
    *        CINVNO    WITH laData[2]  ,;
    *        DAPDTRDAT WITH laData[50] ,;
    *        LAPDPOST  WITH .F.        ,;
    *        CAPDACTID WITH 'D'        ,;
    *        CAPDREF   WITH laData[2]  ,;
    *        CFISFYEAR WITH lcDistYer  ,;
    *        CFSPPRDID WITH lcDistPrd  ,;
    *        CAPSESSNO WITH lcSequence ,;
    *        CAPDTRTYP WITH 'I'        ,;
    *        CSTATUS   WITH 'A'        ,;  
    *        NAPDLINNO WITH VAL(lcInvLineNo) ,;
    *        CCURRCODE WITH laData[44] ,;
    *        NEXRATE   WITH laData[45] ,;
    *        NCURRUNIT WITH laData[48] ,;
    *        CAPDGLACT WITH lcGlAccount,;
    *        NAPDAMNT  WITH lnAmount   ,;
    *        NEQVAMNT  WITH lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
    *        CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',laTaxCode[lnTaxCode,2],'')

    REPLACE CVENDCODE WITH laData[1]  ,;
            CINVNO    WITH laData[2]  ,;
            DAPDTRDAT WITH laData[50] ,;
            LAPDPOST  WITH .F.        ,;
            CAPDACTID WITH 'D'        ,;
            CAPDREF   WITH laData[2]  ,;
            CFISFYEAR WITH lcDistYer  ,;
            CFSPPRDID WITH lcDistPrd  ,;
            CAPSESSNO WITH lcSequence ,;
            CAPDTRTYP WITH 'I'        ,;
            CSTATUS   WITH 'A'        ,;  
            NAPDLINNO WITH VAL(lcInvLineNo) ,;
            CCURRCODE WITH laData[44] ,;
            NEXRATE   WITH laData[45] ,;
            NCURRUNIT WITH laData[48] ,;
            CAPDGLACT WITH lcGlAccount,;
            NAPDAMNT  WITH lnAmount   ,;
            NEQVAMNT  WITH lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
            CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',;
            IIF(laTktType[lnTktType,2]='G' AND !llAutoScr,laTaxCode[lnTaxCode,2],''),'')
    *B603484,1 SSE 03/20/2000 [End] 

  *B602830,1 Add this condition to prevent adding a distribution line with
  *          0 amount [Begin]
  ENDIF   && End of IF lnAmount <> 0
  *B602830,1 Add this condition to prevent adding a distribution line [End]
  
ELSE
  LOCATE REST WHILE cVendCode+cInvNo+STR(nApDLinNo,4) = ;
              PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo FOR CAPDSTAT <> 'V'
  *MAN Fix Total Amount to match the GLDIST              
  lnOApdAmnt = nApdAmnt              
  
  IF cStatus = 'A'
    *B603484,1 SSE 03/20/2000 Make Tax Code added in General Expenses [Begin] 
    *REPLACE CAPDGLACT WITH lcGlAccount,;
    *        NAPDAMNT  WITH NAPDAMNT + lnAmount ,;
    *        NEQVAMNT  WITH NEQVAMNT + lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
    *        CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',laTaxCode[lnTaxCode,2],'')

    REPLACE CAPDGLACT WITH lcGlAccount,;
            NAPDAMNT  WITH NAPDAMNT + lnAmount ,;
            NEQVAMNT  WITH NEQVAMNT + lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
            CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',;
            IIF(laTktType[lnTktType,2]='G' AND !llAutoScr,laTaxCode[lnTaxCode,2],''),'')
    *B603484,1 SSE 03/20/2000 [End] 

    IF NAPDAMNT=0
      REPLACE cStatus WITH 'S'
      DELETE
    ENDIF
  ELSE
    *E300798,1 Void Modified Line
    REPLACE CAPDSTAT  WITH 'V' ,;
            CSTATUS   WITH 'M' ,;
            CAPSESSNO WITH lcSequence
    SCATTER MEMVAR MEMO
    m.cStatus   = 'A'
    m.cBatchNo  = ' '
    m.cTrnSledn = ' '
    m.nEntryNo  = 0
    m.lAPDPost  = .F.
    m.nApdAmnt  = -1 * m.nApdAmnt
    m.nEqvAmnt  = -1 * m.nEqvAmnt
    APPEND BLANK
    GATHER MEMVAR MEMO

    *E300798,1 Add New Line
    m.cApDGlAct = lcGlAccount
    m.cApdStat  = ' '
    m.nApdAmnt  = -1 * m.nApdAmnt + lnAmount
    m.nEqvAmnt  = -1 * m.nEqvAmnt + lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48]
    IF m.nApdAmnt <> 0
      APPEND BLANK
      GATHER MEMVAR MEMO
    *MAN Added the Else Condition  to fix the total in edit mode
    ELSE
      lnOApdAmnt = 0  
    ENDIF  
  ENDIF
ENDIF
*MAN Fix Total Amount to match the GLDIST
*lnDistAmnt = lnDistAmnt + lnAmount
lnDistAmnt = lnDistAmnt + nApdAmnt - lnOApdAmnt

SELECT (lnAlias)

 
*!*************************************************************
*! Name      : lfGetMaxLin
*! Developer : AKA 
*! Date      : 10/30/2001
*! Purpose   : Get the maximum line no from invoicetkt and update LaData[52]
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetMaxLin()
*!*************************************************************
*!B605040,1  Get distribution line no. from invoice tkt    
FUNCTION lfGetMaxLin
PRIVATE lcSumStr, lcSumKey , lnDistLnNo  , lnRecNo , lnGenType , lcArea 
lcArea = SELECT()
lnDistLnNo = 0 
lnGenType  = 0 
*lcSumKey = "cVendCode+cApInvNo+cApVILNo"  
lcSumKey = "cVendCode+cApInvNo"  
lcSumStr = ALLTRIM(laData[1]) +ALLTRIM(laData[2]) 

*B606054 Alb Get the max line no including the deleted one [Begin]
SET DELETE OFF       
SELECT MAX(VAL(cApdLinNo)) FROM (lcAPInvTkt) WHERE ALLTRIM(cVendCode)+ALLTRIM(cApInvNo) = lcSumStr  ;
       INTO ARRAY laMaxDistLn
*SELECT MAX(VAL(cApdLinNo)) FROM (lcAPInvTkt) WHERE ALLTRIM(cVendCode)+ALLTRIM(cApInvNo) = lcSumStr  ;
       AND !DELE() INTO ARRAY laMaxDistLn
SET DELETE ON 
*B606054 Alb Get the max line no including the deleted one[END]

IF _TALLY <> 0  AND laMaxDistLn[1] > 0 
  lnDistLnNo = laMaxDistLn[1] 
ENDIF

SELECT (lcAPvInvDt)
lnRecNo = RECNO()
COUNT TO lnGenType FOR cImTyp = 'G' 
IF BETWEEN (lnRecNo , 1 , RECCOUNT())
  GOTO lnRecNo 
ENDIF

SELECT (lcArea)
RETURN lnDistLnNo + lnGenType 
*B605040,1  Get distribution line no. from invoice tkt    [End]

*!*************************************************************
*! Name      : lfUpdDist
*! Developer : AKA
*! Date      : 11/05/2001
*! Purpose   : Update Temporary AP Distribution file in case of Shipment. 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdDist()
*!*************************************************************
*!B605040,1 Amin, If the invoice line detail is contributed we should update the Apdist with 
*!B605040,1       line for each contirbuted account. This should be per vendor+invoice in case of
*!B605040,1       Shipmnet only   [start]
FUNCTION lfUpdDist 


PRIVATE lnAlias , lcInvLineNo , lcApdLinNo , lcGlAccount , lnAmount, lnTotCntAmt
STORE '' TO lnAlias , lcInvLineNo , lcApdLinNo , lcGlAccount 
STORE 0  TO lnAmount , lnTotCntAmt
lnAlias = SELECT()
*E301995,1 Alb Add Adorment order to invoice line [Begin]
SELECT (lcAPInvTkt)
* No need to rebuild the TmpDist file if there is no contributed Shipment.
*LOCATE FOR cImTyp = "S"  AND !EMPTY(cRsession)
*IF EOF()
  * Nothing do to with this invoice. The TempDist file should be ok 
*  RETURN 
*ENDIF
*E301995,1 Alb Add Adorment order to invoice line [end]

* Before rebuilding the Temp.Dist file we should zap it.
SELECT (lcTmpDist)
ZAP
APPEND BLANK
REPLACE CVENDCODE WITH laData[1]  ,;
        CINVNO    WITH laData[2]  ,;
        DAPDTRDAT WITH laData[50] ,;
        LAPDPOST  WITH .F.        ,;
        CAPDGLACT WITH laData[33] ,;
        CAPDACTID WITH 'A'        ,;
        CAPDREF   WITH laData[2]  ,;
        CFISFYEAR WITH lcDistYer  ,;
        CFSPPRDID WITH lcDistPrd  ,;
        CAPSESSNO WITH lcSequence ,;
        NAPDLINNO WITH 0          ,;        
        CAPDTRTYP WITH 'I'        ,;
        CSTATUS   WITH 'A'        ,;
        CCURRCODE WITH laData[44] ,;
        NEXRATE   WITH laData[45] ,;
        NCURRUNIT WITH laData[48] ,;
        NAPDAMNT WITH -laData[12] ,;
        NEQVAMNT WITH -laData[12] &lcExSin1 laData[45] &lcExSin2 laData[48]

SELECT (lcAPvInvDt)
SCAN  
  lcInvLineNo = &lcAPvInvDt..cAPVILNo 
  lnTotCntAmt = 0 
  SELECT (lcAPInvTkt)
  *If there is Contributed Shipmnet, update TmpDist with the lines from invoicetkt 
  IF SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo)  AND &lcAPInvTkt..cImTyp = "S" ;
      AND !EMPTY(&lcAPInvTkt..cRsession) AND !EMPTY(&lcAPInvTkt..cTktNo)         
     
    SCAN REST WHILE cVendCode+cApInvNo+cApvilNo= PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo;
              FOR &lcAPInvTkt..cImTyp = "S" AND !EMPTY(&lcAPInvTkt..cRsession) AND  !EMPTY(&lcAPInvTkt..cTktNo) 
      *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
      * ALB lnTotCntAmt = lnTotCntAmt + &lcAPInvTkt..nApaplAmnt
      lnTotCntAmt = lnTotCntAmt + ROUND(&lcAPInvTkt..nApaplAmnt,2)
      *B605911,1 ALB Fix the contribution process  to save the invoice [End]
      lnAmount    = &lcAPInvTkt..nApaplAmnt
      
      *B607278,1 ALB Refresh the WIP account when changing [Begin]
      lcGlAccount = &lcAPInvTkt..cApdGlAct
      *REM lcGlAccount = &lcAPvInvDt..cApdGlAct
      *B607278,1 ALB Refresh the WIP account when changing [end]
      
      lcApdLinNo = &lcAPInvTkt..cApdLinNo
      SELECT (lcTmpDist)         
      IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcApdLinNo)
        IF lnAmount <> 0
          APPEND BLANK
          REPLACE CVENDCODE WITH laData[1]  ,;
                  CINVNO    WITH laData[2]  ,;
                  DAPDTRDAT WITH laData[50] ,;
                  LAPDPOST  WITH .F.        ,;
                  CAPDACTID WITH 'D'        ,;
                  CAPDREF   WITH laData[2]  ,;
                  CFISFYEAR WITH lcDistYer  ,;
                  CFSPPRDID WITH lcDistPrd  ,;
                  CAPSESSNO WITH lcSequence ,;
                  CAPDTRTYP WITH 'I'        ,;
                  CSTATUS   WITH 'A'        ,;  
                  NAPDLINNO WITH VAL(lcApdLinNo) ,;
                  CCURRCODE WITH laData[44] ,;
                  NEXRATE   WITH laData[45] ,;
                  NCURRUNIT WITH laData[48] ,;
                  CAPDGLACT WITH lcGlAccount,;
                  NAPDAMNT  WITH lnAmount   ,;
                  NEQVAMNT  WITH lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
                  CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',;
                  IIF(laTktType[lnTktType,2]='G' AND !llAutoScr,laTaxCode[lnTaxCode,2],''),'')
           *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [Begin]
           REPLACE CTAXCODE WITH IIF(ApVendor.cTaxType<>'T',;
                  IIF(&lcAPvInvDt..CIMTYP='G' AND !llAutoScr,&lcAPvInvDt..CTaxCode,''),'')
           *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [End]
         ENDIF    && End of IF lnAmount <> 0          
      ELSE
        * The following part is to handle the old records that were created thru old version.
        * That is becasue the new fields NAPDLINNO and CAPDGLACT were created and updated thru the new version
        * and it replaced with the value of APINVDTL.cAPVILNo and APINVDTL.cApdGlAct respectively.
        IF lnAmount <> 0
          IF lcGlAccount = &lcTmpDist..cApdGlAct 
            REPLACE &lcTmpDist..NAPDAMNT  WITH &lcTmpDist..NAPDAMNT  + lnAmount,;
                    &lcTmpDist..NEQVAMNT  WITH &lcTmpDist..NEQVAMNT  + ( lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48])
          ELSE
            APPEND BLANK
            REPLACE CVENDCODE WITH laData[1]  ,;
                    CINVNO    WITH laData[2]  ,;
                    DAPDTRDAT WITH laData[50] ,;
                    LAPDPOST  WITH .F.        ,;
                    CAPDACTID WITH 'D'        ,;
                    CAPDREF   WITH laData[2]  ,;
                    CFISFYEAR WITH lcDistYer  ,;
                    CFSPPRDID WITH lcDistPrd  ,;
                    CAPSESSNO WITH lcSequence ,;
                    CAPDTRTYP WITH 'I'        ,;
                    CSTATUS   WITH 'A'        ,;  
                    NAPDLINNO WITH VAL(lcApdLinNo) ,;
                    CCURRCODE WITH laData[44] ,;
                    NEXRATE   WITH laData[45] ,;
                    NCURRUNIT WITH laData[48] ,;
                    CAPDGLACT WITH lcGlAccount,;
                    NAPDAMNT  WITH lnAmount   ,;
                    NEQVAMNT  WITH lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
                    CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',;
                    IIF(laTktType[lnTktType,2]='G' AND !llAutoScr,laTaxCode[lnTaxCode,2],''),'')
            *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [Begin]
            REPLACE CTAXCODE WITH IIF(ApVendor.cTaxType<>'T',;
                    IIF(&lcAPvInvDt..CIMTYP='G' AND !llAutoScr,&lcAPvInvDt..CTaxCode,''),'')
            *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [End]
          ENDIF   && End of IF lcGlAccount = &lcTmpDist..cApdGlAct 
        ENDIF    && End of IF lnAmount <> 0                      
      ENDIF      &&End of IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcApdLinNo) 
    ENDSCAN 
    * If the total contribued amount is not equal to the invoice amount. add new line with the
    * difference. 
    IF lnTotCntAmt <> 0 .AND. lnTotCntAmt <> &lcAPvInvDt..nAPAMnt 
      *B605911,1 ALB Fix the contribution process  to save the invoice [Begin]
      *ALb lnAmount    =  IIF( ABS(lnTotCntAmt) > ABS(&lcAPvInvDt..nAPAMnt) ,; 
                          ABS(lnTotCntAmt) - ABS(&lcAPvInvDt..nAPAMnt) ,;
                          ABS(&lcAPvInvDt..nAPAMnt) - ABS(lnTotCntAmt) ) 
      lnAmount    =  ABS(&lcAPvInvDt..nAPAMnt) - ABS(lnTotCntAmt)
      *B605911,1 ALB Fix the contribution process  to save the invoice [End]
                          
      SELECT (lcTmpDist)                                   
      *B605911,1 fIX THE APDIST TO ADD THE DIFF. AT THE END OF THE RECORD [BEGIN]
      *APPEND BLANK
      *REPLACE CVENDCODE WITH laData[1]  ,;
              CINVNO    WITH laData[2]  ,;
              DAPDTRDAT WITH laData[50] ,;
              LAPDPOST  WITH .F.        ,;
              CAPDACTID WITH 'D'        ,;
              CAPDREF   WITH laData[2]  ,;
              CFISFYEAR WITH lcDistYer  ,;
              CFSPPRDID WITH lcDistPrd  ,;
              CAPSESSNO WITH lcSequence ,;
              CAPDTRTYP WITH 'I'        ,;
              CSTATUS   WITH 'A'        ,;  
              NAPDLINNO WITH 0          ,;
              CCURRCODE WITH laData[44] ,;
              NEXRATE   WITH laData[45] ,;
              NCURRUNIT WITH laData[48] ,;
              CAPDGLACT WITH &lcAPvInvDt..cApdGlAct,;
              NAPDAMNT  WITH lnAmount    ,;
              NEQVAMNT  WITH lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
              CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',;
                    IIF(laTktType[lnTktType,2]='G' AND !llAutoScr,laTaxCode[lnTaxCode,2],''),'')
    
      REPLACE NAPDAMNT  WITH NAPDAMNT + lnAmount    ,;
              NEQVAMNT  WITH NEQVAMNT + (lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48])

      *B605911,1 fIX THE APDIST TO ADD THE DIFF. AT THE END OF THE RECORD [END]
    ENDIF   &&End of IF lnTotCntAmt <> 0 .AND. lnTotCntAmt <> &lcAPvInvDt..nAPAMnt 
  ELSE
    *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [BEGIN]
    lnAmount    = &lcAPvInvDt..nAPAMnt
    *lnAmount    = &lcAPvInvDt..nAPAMnt * IIF(&lcAPvInvDt..CIMTYP $'RF',-1,1)
    *B607416,1 ALB Fix bug when Saving the debit memo againest Return PO [END]
    
    lcGlAccount = &lcAPvInvDt..cApdGlAct
    lcInvLineNo = &lcAPvInvDt..cAPVILNo 
    SELECT (lcTmpDist)
    IF !SEEK(PADR(laData[1],8)+PADR(laData[2],12)+lcInvLineNo) AND lnAmount <> 0
      APPEND BLANK
      REPLACE CVENDCODE WITH laData[1]  ,;
              CINVNO    WITH laData[2]  ,;
              DAPDTRDAT WITH laData[50] ,;
              LAPDPOST  WITH .F.        ,;
              CAPDACTID WITH 'D'        ,;
              CAPDREF   WITH laData[2]  ,;
              CFISFYEAR WITH lcDistYer  ,;
              CFSPPRDID WITH lcDistPrd  ,;
              CAPSESSNO WITH lcSequence ,;
              CAPDTRTYP WITH 'I'        ,;
              CSTATUS   WITH 'A'        ,;  
              NAPDLINNO WITH VAL(lcInvLineNo),;
              CCURRCODE WITH laData[44] ,;
              NEXRATE   WITH laData[45] ,;
              NCURRUNIT WITH laData[48] ,;
              CAPDGLACT WITH lcGlAccount,;
              NAPDAMNT  WITH lnAmount   ,;
              NEQVAMNT  WITH lnAmount &lcExSin1 laData[45] &lcExSin2 laData[48],;
              CTAXCODE  WITH IIF(ApVendor.cTaxType<>'T',;
              IIF(laTktType[lnTktType,2]='G',laTaxCode[lnTaxCode,2],''),'')
      *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [Begin]
      REPLACE CTAXCODE WITH IIF(ApVendor.cTaxType<>'T',;
              IIF(&lcAPvInvDt..CIMTYP='G' AND !llAutoScr,&lcAPvInvDt..CTaxCode,''),'')
      *B607095,1 ALB Fix problem in AP Invoice lines that tax code applies to all lines [End]
    ENDIF       
  ENDIF
ENDSCAN 


SELECT *,SPACE(60) AS 'CDISCRIP',;
    'S' AS 'cStatus',;
    SPACE(1) AS 'cUpdSerNo',;
    RECNO() AS 'NRECNO';
    FROM &gcDataDir.APDIST;
    WHERE CINVNO + CVENDCODE + cAPDTrTyp = laData[2] + laData[1] + "I"  ;
    AND CAPDSTAT <> 'V';
    INTO DBF &gcWorkDir.&lcTmpDist2

SELECT (lcTmpDist2)
REPLACE ALL CAPDSTAT  WITH 'V' ,;
            CSTATUS   WITH 'M' 


   
SELECT (lcTmpDist2)
* Void all records in the TmpDist file
SCAN FOR nApdAmnt <> 0 
 
  SELECT (lcTmpDist2)
  SCATTER MEMVAR MEMO
  
  SELECT (lcTmpDist)
  APPEND BLANK
  GATHER MEMVAR MEMO

  * Add reverse line. 
   m.cStatus   = 'V'
   m.cBatchNo  = ' '
   m.cTrnSledn = ' '
   m.cStatus   = 'A'
   m.nEntryNo  = 0
   m.lAPDPost  = .F.
   m.nApdAmnt  = -1 * m.nApdAmnt
   m.nEqvAmnt  = -1 * m.nEqvAmnt
   M.CAPSESSNO = lcSequence 
   APPEND BLANK
   GATHER MEMVAR MEMO
ENDSCAN           

SELECT (lnAlias)
RETURN 




*!*************************************************************
*! Name      : lfUpdShip
*! Developer : AKA
*! Date      : 11/05/2001
*! Purpose   : Update cTktBom,BomCost and BomLine in case of Shipment
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdShip()
*!*************************************************************
FUNCTION lfUpdShip
PRIVATE lcInvDtKey , lnRecNo , llDeleted , lcOrder

SELECT BOMLINE
lcOrder =  ORDER()
SET ORDER TO (lcOrder)
SET ORDER TO TAG Bomshrec

lcInvDtKey= &lcAPVINVDT..cVendCode+&lcAPVINVDT..cApInvNo+&lcAPVINVDT..cAPVILNo
llDeleted = (&lcAPVINVDT..cStatus='D')
lnRecNo   = &lcAPVINVDT..nRecNo

SELECT (lcAPVINVDT)
IF lnRecNo > 0  AND BETWEEN( lnRecNo , 1 , RECCOUNT('APVINVDT'))
  SELECT APVINVDT
  GO lnRecNo
ENDIF

SELECT (lcAPInvTkt)
=SEEK(lcInvDtKey)
SCAN REST  WHILE cVendCode+cApInvNo+cAPVILNo= lcInvDtKey FOR &lcAPInvTkt..cStatus <> 'S'
  SELECT (lcAPInvTkt)      
  llDeleted = (&lcAPVINVDT..cStatus='D')
  lnRecNo   =  &lcAPVINVDT..nRecNo

  * Go to the corresponding record in master file in case of deleted line.
  IF BETWEEN(&lcAPInvTkt..nRecNo,1,RECCOUNT('APInvTkt'))
    GO &lcAPInvTkt..nRecNo IN APINVTKT
  ENDIF
  IF SEEK("I"+&lcAPInvTkt..ctktno+&lcApVInvDt..cbomtype+SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(10),'cTktBom')
  
    SELECT cTktBom
    *    REPLACE ISSUE_QTY WITH ISSUE_QTY - IIF(&lcAPInvTkt..nRecNo=0,0,APInvTkt.nApTAplQty) + &lcAPInvTkt..nApTAplQty,;
    *            USED_QTY  WITH USED_QTY  - IIF(&lcAPInvTkt..nRecNo=0,0,APInvTkt.nApTAplQty) + &lcAPInvTkt..nApTAplQty
    DO CASE
      CASE &lcAPInvTkt..cStatus ='D' AND &lcAPInvTkt..nRecNo <> 0 
        IF BETWEEN(&lcAPInvTkt..nRecNo,1,RECCOUNT('APInvTkt'))
          GO &lcAPInvTkt..nRecNo IN APINVTKT
          REPLACE ISSUE_QTY WITH  MAX(ISSUE_QTY-APInvTkt.nApTAplQty,0 ),;
                  USED_QTY  WITH  MAX(ISSUE_QTY-APInvTkt.nApTAplQty,0 )
        ENDIF          
      CASE &lcAPInvTkt..cStatus ='M' AND &lcAPInvTkt..nRecNo <> 0 
        IF BETWEEN(&lcAPInvTkt..nRecNo,1,RECCOUNT('APInvTkt'))
          GO &lcAPInvTkt..nRecNo IN APINVTKT
          REPLACE ISSUE_QTY WITH (ISSUE_QTY - APInvTkt.nApTAplQty )+ &lcAPInvTkt..nApTAplQty,;
                  USED_QTY  WITH (USED_QTY  - APInvTkt.nApTAplQty )+ &lcAPInvTkt..nApTAplQty
        ENDIF                     
      OTHERWISE
        **&lcAPInvTkt..cStatus ='A'      
        REPLACE ISSUE_QTY WITH ISSUE_QTY +  &lcAPInvTkt..nApTAplQty,;
                USED_QTY  WITH USED_QTY  +  &lcAPInvTkt..nApTAplQty
    ENDCASE
  ENDIF
  *-- If the Invoice line was deleted or if the uncontributed amount was
  *-- changed.
  IF llDeleted .OR. (EMPTY(&lcAPInvTkt..cRSession) .AND. &lcAPInvTkt..cStatus <> 'S')
    SELECT BOMCOST

    
    =SEEK(&lcApVInvDt..cbomtype+"I"+&lcAPInvTkt..ctktno+;
          SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+;
          SPACE(6)+SPACE(6))      

    LOCATE REST WHILE ;
    cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
    &lcApVInvDt..cbomtype+"I"+&lcAPInvTkt..ctktno+SPACE(19)+;
    SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6) ;
    FOR cVendCode+cApInvNo = PADR(laData[1],8)+PADR(laData[2],12)
    
    DO CASE
      CASE !llDeleted AND !FOUND() .AND. &lcAPInvTkt..cStatus <> 'D'
        APPEND BLANK
        *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
        *REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                CIMTYP    WITH "I"                   ,; 
                CTKTNO    WITH &lcAPInvTkt..ctktno   ,;
                ITEM      WITH ''                    ,;
                ICLR      WITH ''                    ,;
                MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                CWARECODE WITH ''                    ,;
                CDYELOT   WITH ''                    ,;
                CRSESSION WITH SPACE(6)  ,;
                CVENDCODE WITH laData[1] ,;
                CAPINVNO  WITH laData[2] ,;
                CISESSION WITH ''        ,;
                SHIPNO    WITH ''        ,;
                DTRANDATE WITH laData[10],;
                CCOSTTYPE WITH &lcApVInvDt..cCostType  ,;
                NTOTQTY   WITH &lcAPInvTkt..nAPTAplQty -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPTAplQty) ,;
                NTOTCST   WITH &lcAPInvTkt..nAPAplAmnt -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPAplAmnt) ,;
                NUNITCST  WITH &lcAPInvTkt..nAPAplPric -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPAplPric) ,;
                NTOTACST  WITH NTOTCST  ,;
                NUNITACST WITH NUNITCST ,;
                Actualize WITH 'N'

        REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                CIMTYP    WITH "I"                   ,; 
                CTKTNO    WITH &lcAPInvTkt..ctktno   ,;
                ITEM      WITH ''                    ,;
                ICLR      WITH ''                    ,;
                MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                CWARECODE WITH ''                    ,;
                CDYELOT   WITH ''                    ,;
                CRSESSION WITH SPACE(6)  ,;
                CVENDCODE WITH laData[1] ,;
                CAPINVNO  WITH laData[2] ,;
                CISESSION WITH ''        ,;
                SHIPNO    WITH ''        ,;
                DTRANDATE WITH laData[50],;
                CCOSTTYPE WITH &lcApVInvDt..cCostType  ,;
                NTOTQTY   WITH &lcAPInvTkt..nAPTAplQty -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPTAplQty) ,;
                NTOTCST   WITH &lcAPInvTkt..nAPAplAmnt -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPAplAmnt) ,;
                NUNITCST  WITH &lcAPInvTkt..nAPAplPric -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPAplPric) ,;
                NTOTACST  WITH NTOTCST  ,;
                NUNITACST WITH NUNITCST ,;
                Actualize WITH 'N'
        *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
      CASE !llDeleted AND FOUND()
        
        REPLACE NTOTQTY   WITH NTOTQTY +;
                               IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                   &lcAPInvTkt..nAPTAplQty) -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPTAplQty) ,;
                NTOTCST   WITH NTOTCST +;
                               IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                   &lcAPInvTkt..nAPAplAmnt) -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPAplAmnt) ,;
                NUNITCST  WITH NUNITCST +;
                               IIF(&lcAPInvTkt..cStatus = 'D' , 0 ,;
                                   &lcAPInvTkt..nAPAplPric) -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPAplPric) ,;
                NTOTACST  WITH NTOTCST  ,;
                NUNITACST WITH NUNITCST
        IF NTOTQTY = 0
          DELETE
        ENDIF
        
      CASE llDeleted AND FOUND()
        REPLACE NTOTQTY   WITH NTOTQTY -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPTAplQty) ,;
                NTOTCST   WITH NTOTCST -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                   APINVTKT.nAPAplAmnt) ,;
                NUNITCST  WITH NUNITCST -;
                               IIF(&lcAPInvTkt..nRecNo = 0 , 0 ,;
                                     APINVTKT.nAPAplPric) ,;
                NTOTACST  WITH NTOTCST  ,;
                NUNITACST WITH NUNITCST
          
        IF NTOTQTY = 0
          DELETE
        ENDIF
    ENDCASE
  ENDIF    && End of IF EMPTY(&lcAPInvTkt..cRSession) .AND. ...

  SELECT (lcAPInvTkt)
  IF !EMPTY(&lcAPInvTkt..cRSession)
    llDeleted = (cStatus='D')
    lnRecNo = nRecNo
    *IF !llDeleted AND lnRecNo > 0 
    IF !llDeleted AND BETWEEN(lnRecNo ,1, RECCOUNT('APINVTKT'))
      SELECT APINVTKT
      GO lnRecNo
    ENDIF
     
    SELECT BomLine
    
    =SEEK("I"+"3"+&lcApVInvDt..cBomType+&lcApVInvDt..cOprCode+;
          &lcApVInvDt..shipno+&lcAPInvTkt..cRSession+&lcApInvTkt..ctktno+;
          STR(&lcApInvTkt..LineNo,6)+&lcApInvTkt..Item+&lcApVInvDt..Color)
            
    DO CASE
      CASE !llDeleted AND !FOUND()
         APPEND BLANK
         REPLACE cimtyp    WITH "I"                   ,;
                 ctype     WITH '3'                   ,;
                 cbomtyp   WITH &lcApVInvDt..cBomType ,;
                 mfgcode   WITH &lcApVInvDt..cOprCode ,;
                 ctktno    WITH &lcAPInvTkt..ctktno   ,;
                 crsession WITH &lcAPInvTkt..cRSession,;
                 shipno    WITH &lcApVInvDt..shipno   ,;
                 lineno    WITH &lcAPInvTkt..LineNo   ,;
                 style     WITH &lcAPInvTkt..Item     ,;
                 sclr      WITH &lcApVInvDt..Color    ,;
                 cCatgTyp  WITH &lcApVInvDt..cCostType,;
                 UnitQty   WITH  1 ,;
                 StyQty    WITH &lcAPInvTkt..nApTAplQty ,;
                 ItemQty   WITH StyQty ,;
                 ItemAmt   WITH &lcAPInvTkt..nApAplAmnt ,;
                 UnitCost  WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)

      CASE !llDeleted AND FOUND()
        REPLACE StyQty   WITH StyQty - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + &lcAPInvTkt..nApTAplQty ,;
                ItemQty  WITH StyQty ,;
                ItemAmt  WITH ItemAmt -IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
                UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
        CASE llDeleted AND FOUND()
          DELETE
      ENDCASE

      SELECT BOMCOST
      =SEEK(&lcApVInvDt..cbomtype+"I"+&lcApInvTkt..ctktno+SPACE(19)+SPACE(6)+&lcApVInvDt..cOprCode+;
             SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6))
      LOCATE REST WHILE ;
      cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      &lcApVInvDt..cbomtype+"I"+&lcApInvTkt..ctktno+SPACE(19)+;
      SPACE(6)+&lcApVInvDt..cOprCode+SPACE(6)+SPACE(10)+&lcAPInvTkt..cRSession+SPACE(6) ;
      FOR cVendCode+cApInvNo = PADR(laData[1],8)+PADR(laData[2],12)
      DO CASE
        CASE !llDeleted AND !FOUND()
          APPEND BLANK
          *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
          *REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH "I"                   ,;
                  CTKTNO    WITH &lcApInvTkt..ctktno   ,;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH &lcAPInvTkt..cRSession,;
                  CVENDCODE WITH laData[1] ,;
                  CAPINVNO  WITH laData[2] ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH laData[10],;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType ,;
                  NTOTQTY   WITH &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'

          REPLACE CBOMTYPE  WITH &lcApVInvDt..cbomtype ,;
                  CIMTYP    WITH "I"                   ,;
                  CTKTNO    WITH &lcApInvTkt..ctktno   ,;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH &lcApVInvDt..cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH &lcAPInvTkt..cRSession,;
                  CVENDCODE WITH laData[1] ,;
                  CAPINVNO  WITH laData[2] ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH ''        ,;
                  DTRANDATE WITH laData[50],;
                  CCOSTTYPE WITH &lcApVInvDt..cCostType ,;
                  NTOTQTY   WITH &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          *B605493,1 SSH 10/Feb/2002 Incorrect update for BOMCOST.DTRANDATE.
        CASE !llDeleted AND FOUND()
          REPLACE NTOTQTY   WITH NTOTQTY - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + &lcAPInvTkt..nApTAplQty ,;
                  NTOTCST   WITH NTOTCST - IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + &lcAPInvTkt..nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'
          
        CASE llDeleted AND FOUND()
          DELETE
      ENDCASE
  ENDIF   && End of If !EMPTY(&lcAPInvTkt..cRSession)
ENDSCAN


SELECT BOMLINE
SET ORDER TO (lcOrder)

SELECT (lcAPVINVDT)
RETURN 



*APINVTKT   10482
*APVINVDT   49365
*APDIST     339627


****************************************************************************
*B605485,1 SSH 11/Feb/2002 Add Validation on user Privileges in adding new.
****************************************************************************
FUNCTION lfGetprev
****************************************************************************
*Function to check if the user have preveligies to add new vendor or no.
****************************************************************************
PRIVATE lnOldAls,  llToReturn

lnOldAls = SELECT(0)
llToReturn = .T.
llUsrPrv   = gfSysOpen(gcSysHome+"SYUUSRPR",'CUSER_ID')
lcPross_ID = UPPER(SUBSTR("AWRAPVENDR",4))

IF glLog_Requ
  SELECT SYUUSRPR
  IF SEEK(ALLTRIM(gcUser_ID)+gcAct_Appl+gcAct_Comp+lcPross_ID) OR;
     SEEK(ALLTRIM(gcUser_ID)+'SY'+gcAct_Comp+lcPross_ID)
    llToReturn = SYUUSRPR.lAddRec   
  ELSE
    IF SEEK(ALLTRIM(gcUser_Grp)+gcAct_Appl+gcAct_Comp+lcPross_ID) OR;
       SEEK(ALLTRIM(gcUser_Grp)+'SY'+gcAct_Comp+lcPross_ID)
      llToReturn = SYUUSRPR.lAddRec   
    ENDIF  
  ENDIF
ELSE
  llToReturn = .T.
ENDIF
USE IN IIF(llUsrPrv,'SYUUSRPR',0)
SELECT(lnOldAls)
RETURN(llToReturn)

*:*************************************************************
*: Name      : lfvCheck
*: Developer : Rania Abdel Razik (RAE)
*: Date      : 09/06/2002
*: Purpose   : Disable the Invoice detail screen for closed PO
*:*************************************************************
*: Calls     : NONE
*:*************************************************************
*: Returns            : llClosed
*:*************************************************************
*: Example            : =lfvCheck()
*:*************************************************************
*: B605967
FUNCTION lfvCheck

lnOldAls = SELECT(0)
DO CASE
  *-- Style PO and Style Return PO
  CASE INLIST(&lcAPvInvDt..cIMTyp,'I','R')
    =SEEK(laData[3]+&lcAPvInvDt..CTKTNO,'POSHDR')
    llClosed = (POSHDR.STATUS = 'S')  

  *-- Shipment
  CASE INLIST(&lcAPvInvDt..cIMTyp,'S')
    =SEEK(laData[3]+&lcAPvInvDt..CTKTNO,'POSHDR')
    llClosed = (POSHDR.STATUS = 'S')
  
  *-- Material PO and Return Material PO
  CASE INLIST(&lcAPvInvDt..cIMTyp,'L','F')
    =SEEK(laData[3]+&lcAPvInvDt..CTKTNO,'POFHDR')
    llClosed = (POFHDR.STATUS = 'L')
  
  *-- Cutting Ticket
  CASE INLIST(&lcAPvInvDt..cIMTyp,'M')
    =SEEK(&lcAPvInvDt..CTKTNO,'CUTTKTH')
    llClosed = (CUTTKTH.STATUS = 'S')
  *-- Material MFG
  CASE INLIST(&lcAPvInvDt..cIMTyp,'T')
    =SEEK(&lcAPvInvDt..CTKTNO,'MMFGORDH')
    llClosed = (MMFGORDH.STATUS = 'S')  
ENDCASE

lcAutoMode= IIF(llClosed,'DISABLE','ENABLE')
lcScrMode = IIF(llClosed,'DISABLE','ENABLE')
lcDetMode = IIF(llClosed,'DISABLE','ENABLE')
lcInvQTot = IIF(llClosed,'DISABLE','ENABLE')
lcInvQDet = IIF(llClosed,'DISABLE','ENABLE')
lcAprQDet = IIF(llClosed,'DISABLE','ENABLE')
lcAprQty  = IIF(llClosed,'DISABLE','ENABLE')
lcAprPri  = IIF(llClosed,'DISABLE','ENABLE')
lcWIPStat = IIF(llClosed,'DISABLE','ENABLE')

SELECT (lnOldAls)
RETURN llClosed

*:********************************************************************
*: Name      : lfChckVoid
*: Developer : Rania Abdel Razik (RAE)
*: Date      : 09/06/2002
*: Purpose   : Check the balancing in (lcTmpDist) file before saving.
*:********************************************************************
*: Calls     : NONE
*:********************************************************************
*: Returns            : llSave
*:********************************************************************
*: Example            : =lfChckVoid()
*:********************************************************************
*: B605746
FUNCTION lfChckVoid
PRIVATE lnTotAmnt , ll2Return , lcOldExt , lnEqvAmnt
STORE 0 TO lnTotAmnt , lnEqvAmnt

lnOldAls = SELECT(0)
lcOldExt = SET("EXACT")
SET EXACT OFF
SELECT (lcTmpDist)
GO TOP
ll2Return = !EOF()
IF ll2Return

  *B124737,1 NNA 12/16/2004 (Begin) Get the equivalent Amount by Multiple (amount with Rating)
  *B124737,1 NNA            to be away from the fraction  
  *SUM ALL napdamnt , neqvamnt TO lnTotAmnt , lnEqvAmnt ;
      FOR cVendCode+cInvNo+STR(nApDLinNo,4) = PADR(laData[1],8) + PADR(laData[2],12);
      .AND. CAPDSTAT <> 'V'
  SUM ALL napdamnt , (NAPDAMNT &lcExSin1 laData[45] &lcExSin2 laData[48]) TO lnTotAmnt , lnEqvAmnt ;
      FOR cVendCode+cInvNo+STR(nApDLinNo,4) = PADR(laData[1],8) + PADR(laData[2],12);
      .AND. CAPDSTAT <> 'V'
  *B124737,1 NNA (End)

  ll2Return = 	(lnTotAmnt = 0) .AND. (lnEqvAmnt =0)
ENDIF
SELECT (lnOldAls)
SET EXACT &lcOldExt
RETURN ll2Return
*:********************************************************************
*: Name      : lfActCstRmv
*: Developer : Albert Raif  (ALB)
*: Date      : 08/22/2002
*: Purpose   : Return the Act cost that removed from (lcApInTkt) file.llOldRec = .T.
*:             Return the new act cost from the other invoice lines. llOldRec = .F.
*:********************************************************************
*: Calls     : NONE
*:********************************************************************
*: Returns            : lnSumAmnt
*:********************************************************************
*: Example            : =lfActCstRmv()
*:********************************************************************
*: B605911
FUNCTION lfActCstRmv
PARAMETER llOldRec
PRIVATE lnSumAmnt,lcAlias,lnRecNo,lcWhere
lnSumAmnt = 0
lcAlias = ALIAS()
SELECT (lcAPInvTkt)
lnRecNo = IIF(!EOF(),RECNO(),0)
SET DELETE OFF
lcWhere = IIF(llOldRec,"(cStatus = 'D' AND !EMPTY(nrecno))",;
                       "cStatus = 'A' AND cAPVILNo <> lcInvLineNo")
lcWhere = "ctktno = lcInvTktNo AND " + lcWhere
IF !EMPTY(ALLTRIM(&lcApVInvDt..item))
  lcWhere = lcWhere + " AND item = '" + &lcApVInvDt..item+"'"
ENDIF
SELECT SUM(napaplamnt) FROM &lcApInvTkt WHERE &lcWhere INTO ARRAY laSum 
IF _TALLY > 0
  lnSumAmnt = laSum[1]
ENDIF
SET DELETE ON
IF lnRecNo > 0
  GOTO lnRecNo
ENDIF
SELECT (lcAlias)
RETURN lnSumAmnt

*:********************************************************************
*: Name      : lfGetBrHd
*: Developer : Albert Raif  (ALB)
*: Date      : 11/5/2002
*: Purpose   : Return the Brows Fields and its header
*:********************************************************************
*: Calls     : NONE
*:********************************************************************
*: Returns            : lcBrReturn
*:********************************************************************
*: Example            : =lfGetBrHd()
*:********************************************************************
*:B606373,1 ALB 10/17/2002 Display all information of vendors
FUNCTION lfGetBrHd
PARAMETER lcBrFile

PRIVATE lcBrReturn

lcBrReturn = ''
lcAlias = ALIAS()
IF !USED('SYDFIELD')
  =gfOpenFile(gcSysHome+'SYDFIELD','','SH')
ENDIF
IF !USED('SYDFLFLD')
  =gfOpenFile(gcSysHome+'SYDFLFLD','','SH')
ENDIF

SELECT sydfield.cfld_name,sydfield.cfld_head FROM sydfield,sydflfld WHERE ;
       sydflfld.cfile_nam = lcBrFile AND sydfield.cfld_name = sydflfld.cfld_name ;
       ORDER BY sydflfld.nfld_pos INTO ARRAY laFldName
       
IF _TALLY <> 0
  lnCount = 1
  lnMaxColm = IIF(_TALLY-10 > 29,29,_TALLY-10)
  lcBrReturn = lcBrReturn + laFldName[lnCount,1] + " :H= '" + ALLTRIM(laFldName[lnCount,2]) + "'"
  FOR lnCount = 2 TO lnMaxColm
    lcBrReturn = lcBrReturn + ", "+ laFldName[lnCount,1] + " :H= '" + ALLTRIM(laFldName[lnCount,2]) + "'"
  ENDFOR
ENDIF
SELECT (lcAlias)
RETURN lcBrReturn

*:********************************************************************
*: Name      : lfGetdueD
*: Developer : Albert Raif  (ALB)
*: Date      : 11/13/2002
*: Purpose   : Return the Due Date
*:********************************************************************
*: Calls     : NONE
*:********************************************************************
*: Returns   : ldDueDay
*:********************************************************************
*: Example   : =lfGetdueD()
*:********************************************************************
*:B604310,1 ALB 11/13/2002 Fix the due date according to the payment method

FUNCTION lfGetdueD
PARAMETER ldInvDate,lnDueDays,lnEOM

*B040278,1  TMI [Start] Comment this code to rewrite the equation as it in AR module
**PRIVATE ldDueDay,lnMonthDays,lnAddDays,lnDiffDays
**lnDiffDays = 0
**
**IF lnEOM > 0
**  DO CASE
**    CASE INLIST(ALLTRIM(STR(MONTH(ldInvDate))),'01','03','05','07','08','10','12')
**      lnMonthDays = 31
**    CASE INLIST(ALLTRIM(STR(MONTH(ldInvDate))),'04','06','09','11')
**      lnMonthDays = 30
**    OTHERWISE
**      lnMonthDays = IIF(MOD(YEAR(ldInvDate),4)=0,29,28)
**  ENDCASE
**  lnAddDays = IIF(DAY(ldInvDate)>lnEOM,lnMonthDays - DAY(ldInvDate),lnEOM-DAY(ldInvDate))
**ELSE
**  lnAddDays = 0
**ENDIF
**
**ldDueDay = ldInvDate + lnDueDays+lnAddDays - lnDiffDays

IF UPPER(ALLTRIM(gcContCode))='ENG'   && in case of England
  ldDueDay = IIF(lnEOM > 0 ,CTOD('01'+SUBSTR(DTOC(GOMONTH(ldInvDate,1)),3))-1+lnDueDays , ;
                            ldInvDate+lnDueDays )
ELSE
  ldDueDay = IIF(lnEOM > 0 ,GOMONTH(CTOD(SUBSTR(DTOC(ldInvDate),1,3) +'10'+;
                SUBSTR(DTOC(ldInvDate),6,5)),IIF(DAY(ldInvDate)>lnEOM,2,1))+lnDueDays ,;
                ldInvDate + lnDueDays)
ENDIF
*B040278,1  TMI [End  ] 
RETURN ldDueDay

*:********************************************************************
*: Name      : lfvldApAct
*: Developer : Khalid Mohi El-Din
*: Date      : 02/25/2003
*: Purpose   : Check if there is inactive account.
*:********************************************************************
*: Example   : =lfvldApAct()
*:********************************************************************
*B606903,1 KHM 02/22/2003 
*:********************************************************************
FUNCTION lfvldApAct
PRIVATE lnRetVal

lnRetVal = .T.

SELECT('lcLinkChar')
SET ORDER TO TAG ACCTCODE

SELECT(lcTmpDist)
SCAN
  lcApdGlAct = cApdGlAct
  SELECT('lcLinkChar')
  IF SEEK(lcApdGlAct,'lcLinkChar') AND cSegActiv = "I"
    lnRetVal = .F.
    EXIT
  ENDIF

ENDSCAN

IF !lnRetVal
  =gfModalGen("TRM04203B00000","DIALOG")
ENDIF

RETURN lnRetVal


*:********************************************************************
*: Name      : lfGetGlLnk
*: Developer : Albert Raif  (ALB)
*: Date      : 11/13/2002
*: Purpose   : Return the Due Date
*:********************************************************************
*: Calls     : NONE
*:********************************************************************
*: Returns   : lcGl_Link
*:********************************************************************
*: Example   : =lfGetGlLnk()
*:           : Return the first gl link code in the shipment POs
*:********************************************************************
*:B607278,1 ALB Get the correct WIP account with shipment ticket 

FUNCTION lfGetGlLnk
PARAMETER lcShipNo,lcStyleCode,lcReSess
PRIVATE lcGl_Link

STORE ' ' TO lcGl_Link

*B121188,1 MHM 02/25/2004 Enhance Speed of Shipment and contripution in invoice line screen[Start]
*SELECT POSHDR.LINK_CODE FROM POSHDR,POSLN;
*       WHERE POSLN.SHIPNO = lcShipNo AND IIF(EMPTY(lcStyleCode),.T.,POSLN.STYLE = lcStyleCode);
*       AND IIF(EMPTY(lcReSess),.T.,POSLN.CRSESSION = lcReSess);
*       AND POSHDR.CSTYTYPE+POSHDR.PO = POSLN.CSTYTYPE+POSLN.PO GROUP BY 1 INTO ARRAY laGl_Link

SELECT POSHDR.LINK_CODE FROM POSHDR,POSLN;
       WHERE POSLN.shipno+POSLN.crsession+POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd= lcShipNo+lcReSess;
       AND IIF(EMPTY(lcStyleCode),.T.,POSLN.style+POSLN.cstytype+POSLN.po+STR(POSLN.lineno,6)+POSLN.trancd = lcStyleCode);
       AND POSHDR.CSTYTYPE+POSHDR.PO = POSLN.CSTYTYPE+POSLN.PO GROUP BY 1 INTO ARRAY laGl_Link
*B121188,1 MHM [End]

IF _TALLY > 0
  lcGl_Link = laGl_Link[1,1]+ALLTRIM(STR(_TALLY))
ENDIF       
RETURN lcGl_Link






*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Mohamed Shokry
*=> Added here by TMI for the issue *B132275,1
*! Date      : 06/21/2006
*! Purpose   : To object lock any record in any file
*:*************************************************************
FUNCTION lfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag


PRIVATE lnOldrpSt

lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.

  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
    llLocked = RLOCK() 
    IF DELETED()
      UNLOCK
      =gfModalGen('INM00095B00000','ALERT')
      laScrMode     = .F.
      laScrMode [1] = .T.
      SHOW GETS
      RETURN .F.
    ENDIF
   
  ENDIF  

  *B608942,1 TMI 07/20/2009 06:14:25 PM [Start] 
  IF glRunFrmA4
    lnSlct = SELECT(0)
    IF !USED('A4USTATC')
      =gfOpenFile(gcA4SYSFLDR+'SYUSTATC','CUSER_ID','SH','A4USTATC')
    ENDIF
    SELECT (lnSlct)
  ENDIF    
  *B608942,1 TMI 07/20/2009 06:14:25 PM [End  ] 

  *** Chek if the record is in use by another user
  IF lLok_Set 
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      IF !EMPTY(cLok_User)

        lnOldrpSt = SET('REPROCESS')          
        SET REPROCESS TO 1
        *B608942,1 TMI 07/20/2009 07:23:22 PM [Start] check if the user recorded in cLok_user field lies in SYUSTATC file of either A27 or A4xp
        *IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') 
          IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') .OR. IIF(glRunFrmA4,SEEK('INI'+'OLDVARS'+cLok_User,'A4USTATC'),.F.)
          *B608942,1 TMI 07/20/2009 07:23:29 PM [End  ] 
          UNLOCK
          *** Display the message "Record is in use by user AAAA"
          lnSavRec   = IIF(RECNO('SYUUSER')>RECCOUNT('SYUUSER'),0,;
                       RECNO('SYUUSER'))
          lcLok_User = ALLTRIM(PROPER(LOOKUP(syuUser.cUsr_name,cLok_User,;
                       syuUser.cUser_id,'cUser_id')))
          IF lnSavRec > 0
            GO lnSavRec IN SYUUSER
          ENDIF  
             
          *** Record is in use by user ????    
          *IF gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
          lcRtyCncMs = "Invoice "+ALLTRIM(APINVHDR.CINVNO)+" for vendor "+ ALLTRIM(APINVHDR.CVendCode)+" is being edited by user " + lcLok_User+"."
          IF  gfModalGen("INM00274B00015","ALERT",lcRtyCncMs) = 1
            LOOP
          ENDIF  
          lLok_It    = .F.
          lRet_Flag  = .F.
        ELSE
          lLok_It    = .T. 
        ENDIF          
        SET REPROCESS TO  lnOldrpSt
      ELSE
        *** Display the message "Record is in use by another"
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF  
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF   
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
       TYPE ('dLok_Date') <> "U" .AND. ;
       TYPE ('cLok_Time') <> "U" 

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;   
              cLok_User WITH ""  , ;
              dLok_Date WITH {}  , ;
              cLok_Time WITH ""
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    WAIT WINDOW NOWAIT ' ... Locking ... '
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH gcUser_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()
    WAIT CLEAR
    lRet_Flag  = .T.    
  ENDIF
ENDIF


UNLOCK


RETURN lRet_Flag




*!*************************************************************
*! Name      : gfCPEdit
*! Developer : Yasser Saad Ibrahime , 
*=> Added here by TMI to use lfObj_Lock instead of gfObj_Lock for the issue *B132275,1  06/22/2006
*! Date      : 1993-1995 
*! Purpose   : Validation of edit button in the pannel
*!*************************************************************
FUNCTION gfCPEdit
EXTERNAL ARRAY laData,laDefProc,laScrMode

ACTIVATE WINDOW GWCCONTRL1 TOP

IF TYPE('lcBaseFile') = "C" 
  IF !EMPTY(lcBaseFile)
    SELECT (lcBaseFile)
  ENDIF
ENDIF

llUpDate=.F.
*E300842,1 Hesham (Start)
*E300842,1 check if the current site is the owner of the record before
*E300842,1 editing it
*IF !llEditRec
IF !llEditRec OR IIF(TYPE('COWNER')#'C',.F.,IIF(EMPTY(COWNER),.F.,COWNER<>gcCurSite))
*E300842,1 Hesham (End)
*E300842,1 Hesham (Start)
*E300842,1 give the user the right message if he do not have privl. to Edit
*E300842,1 the record or if he is not the owner of the record
*  =gfModalGen("TRM00088B00000","ALERT",'Editing') 
  IF !llEditRec 
    =gfModalGen("TRM00088B00000","ALERT",'Editing') 
  ELSE
    =gfModalGen("TRM00299B00000","ALERT",'Modified')
  ENDIF  
ELSE
  *E300704,1 Hesham El-Sheltawi (Start)
  IF EMPTY(lcBaseFile) OR lfObj_Lock(.T.)   &&>>> 
  *E300704,1 Hesham El-Sheltawi (ENDIF)  
    *** Befor edit get fresh copy from the record 
  *E300704,1 Hesham El-Sheltawi (Start)
  IF !EMPTY(lcBaseFile)
    SCATTER FIELDS &lcScFields MEMO TO laData
    lcStamp       =  cAdd_User+IIF(EMPTY(dAdd_Date),'',DTOC(dAdd_Date))+;
                     cAdd_Time
  ENDIF                  
  *E300704,1 Hesham El-Sheltawi (END)
    laScrMode    = .F.                  && Reset screen mode
    laScrMode[3] = .T.                  && Go to edit mode
    IF TYPE('laWndObj') ="C" 
       IF !EMPTY(laWndObj[1,1])
         lnWindNum = ASCAN(laWndObj,WLAST())
         IF lnWindNum >0
           _CUROBJ = OBJNUM(&laWndObj[ASUBSCRIPT(laWndObj,lnWindNum,1),2])
         ELSE
           _CUROBJ      = 1
         ENDIF
       ELSE  
         _CUROBJ      = 1
       ENDIF
    ELSE
      _CUROBJ      = 1
    ENDIF
    
    SHOW GETS
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : gfCPDelete
*! Developer : Yasser Saad Ibrahime
*=> Added here by TMI to use lfObj_Lock instead of gfObj_Lock for the issue *B132275,1  06/22/2006
*! Date      : 1993-1995 
*! Purpose   : Validation of the delete button in the pannel
*!*************************************************************
*! Calls     : 
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: GFOBJ_LOCK()             (function  in ARIA3.PRG)
*!          Calls: LPDELSCR.PRG             
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel Top push Delete
*:->
FUNCTION gfCPDelete
EXTERNAL ARRAY laData,laDefProc,laScrMode

ACTIVATE WINDOW GWCCONTRL1 TOP
*E300704,1 Hesham El-Sheltawi (Start)
IF !EMPTY(lcBaseFile)
  SELECT (lcBaseFile)
ENDIF
*E300704,1 Hesham El-Sheltawi (END)
lcDelMesag = IIF(TYPE('lcDelMesag')='C',lcDelMesag,'delete')
*E300842,1 Hesham (Start)
*E300842,1 check if the current site is the owner of the record before
*E300842,1 deleting it
*IF !llDeleRec
IF !llDeleRec OR IIF(TYPE('COWNER')#'C',.F.,IIF(EMPTY(COWNER),.F.,COWNER<>gcCurSite))
*E300842,1 Hesham (End)
*E300842,1 Hesham (Start)
*E300842,1 give the user the right message if he do not have privl. to delete
*E300842,1 the record or if he is not the owner of the record
*    =gfModalGen("TRM00088B00000","ALERT",'Deleting') 
  IF !llDeleRec
    =gfModalGen("TRM00088B00000","ALERT",'Deleting') 
  ELSE
    =gfModalGen("TRM00299B00000","ALERT",'Deleted')
  ENDIF  
*E300842,1 Hesham (eND)  
ELSE
  IF gfModalGen("QRM00002B00006","ALERT",lcDelMesag) = 1
    IF EMPTY(lcBaseFile) OR lfObj_Lock(.T.)
      IF laDefProc[7]
        *E300704,1 Hesham El-Sheltawi (Start)
        IF !EMPTY(lcBaseFile)
          SCATTER FIELDS &lcScFields MEMO TO laData BLANK
          GATHER FROM laData FIELDS &lcScFields MEMO 
          DELETE
        ENDIF  
        *E300704,1 Hesham El-Sheltawi (END)
        laScrMode    = .F.
        laScrMode[1] = .T.
      ELSE  
        DO lpDelScr
        *B600447,1 Check whether the record was deleted
        *B600447,1 if not then unlock the record
        IF laSCrMode[2]
          *E300704,1 Hesham El-Sheltawi (Start)
          IF !EMPTY(lcBaseFile)
            =gfObj_Lock(.F.)
          ENDIF  
          *E300704,1 Hesham El-Sheltawi (END)          
        ENDIF
      ENDIF
     
      *E301297,1 Change these lines because we are going to use one function
      *          called gfDoTriger() to control any triggers (customized
      *          process and workflow server requests including creating
      *          new project) found in the triggers file [Begin]
      **E300775,1 Hesham (Start)
      **E300775,1 if the workflow module is installed for the company
      **E300775,1 then call the global function to control the workflow
      **E300775,1 data
      *IF 'WF' $ gcCmpModules
      *  =gfCtrlFlow(SUBSTR(gcProg_Nam,4),'DELETE')    
      *ENDIF  
      
      *-- If the Deletion/Cancellation action was complete successfully
      IF ASCAN(laEvntTrig , PADR('DELETE' , 10)) <> 0 .AND.;
         (laScrMode[1] = .T. .OR.;
          (TYPE('lcBaseFile') = 'C' .AND. !EMPTY(lcBaseFile) .AND.;
           TYPE(ALLTRIM(lcBaseFile) + '.Status') = 'C' .AND.;
           EVALUATE(ALLTRIM(lcBaseFile) + '.Status') = 'X'))
        *B603210,1 MAN-WAB -'gcProg_nam' is not a global variable that hold the 
        *B603210,1         active prog. so we use the gcbasewin cause it is hold
        *B603210,1         the active prog name start from pos no 4
        *=gfDoTriger(SUBSTR(gcProg_Nam,4) , 'DELETE') 
        =gfDoTriger(SUBSTR(gcBaseWind,4) , 'DELETE') 
      ENDIF    && End of IF ASCAN(laEvntTrig , PADR('DELETE' , 10)) <> 0
      **E300775,1 Hesham (End)
      *E301297,1 Change these lines because we are going to use one [End]
     
      SHOW GETS
    ENDIF  
  ENDIF
ENDIF

