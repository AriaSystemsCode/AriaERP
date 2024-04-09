*:************************************************************************
*: Program file  : RMCRMEM.PRG
*: Program desc. : Credit Memo
*: For screen    : RMCRMEM.SCX (0,1,2,2A,2B)
*: System        : ARIA BUSINESS SYSTEM
*: Module        : Returns Merchandise Module
*: Developer     : Reham Al-Allamy
*:************************************************************************
*: Calls         : 
*:         Procedures : lpTab
*:                      lpShTab
*:                      lpShow
*:         Programs   : GetObj      (Object Linking Program)
*:                      NotePad     (Note Pad Program)
*:         Screens    : RMCRMEM.SPX (Main Credit Memo Screen)
*:                      RMEDCST.SPX (Edit Cost Screen)
*:         Functions  : gfSetup
*:                      lfvData_1
*:                      lfActFolder 
*:                      lfRefHdr
*:                      lfFilTmpHd
*:                      lfwWhenAll
*:                      lfvData_2
*:                      lfActInfo
*:                      lfvData_3
*:                      lfRAInfo
*:                      lfvData_4
*:                      lfwData_41
*:                      lfvData_41
*:                      lfwData_5
*:                      lfvData_5
*:                      lfvData_7
*:                      lfvData_8
*:                      lfvData_9
*:                      lfvData_15
*:                      lfwAllNmrc
*:                      lfwData_27
*:                      lfvData_27
*:                      lfvData_29
*:                      lfvRepCode
*:                      lfvData_17
*:                      lfvData_20
*:                      lfvData_31
*:                      lfvData_33
*:                      lfvData_34
*:                      lfvTerms
*:                      lfvReson1
*:                      lfvDiv
*:                      lfvInQInv
*:                      lfvTaxRate
*:                      lfvCrNotes
*:                      lfvLinkTo
*:                      lfActBrow
*:                      lfDeactBrw
*:                      lfBrowLin
*:                      lfvBrows
*:                      lfwBrowLin
*:                      lfvNewLine
*:                      lfvRemLine
*:                      lfvRemove
*:                      lfvStyle
*:                      lfvStyGrd
*:                      lfvDyelot
*:                      lfvRetSty
*:                      lfvReson2
*:                      lfvGrs_Pct
*:                      lfvPstRate
*:                      lfvQty
*:                      lfGetCost
*:                      lpDelScr
*:                      lpSavScr
*:                      lpClsScr
*:                      lfChgTax
*:                      lfMechDisc
*:                      lfTotalTax
*:                      lfGetInfo
*:                      lfGetLines
*:                      lfGetRALn
*:                      lfGetInvLn
*:                      lfCrtUnComp
*:                      lfChkUnComS
*:                      lfUpdVars
*:                      lfAdUnCmSR
*:*************************************************************
*: Passed Parameters  : lcCredit : Credit Memo #
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRRMCRMEM',.F.,'RM',lcCrMemo
*:*************************************************************
*:  Modifications :
*E301038,1 TAK 10/12/1998 Change the sign of the Void Credit memo
*E301038,1                record in the STYINVJL file to be -ve.
*B602322,1 TAK 12/03/1998 Pass GL Session sequence as a parameter to 
*B602322,1                the GFSTYCRL function to make sure that the record
*B602322,1                in journal with the same session no.
*E301090,1 Reham On 13/12/98
*E301090,1 Do not accept any voided invoices...
*E301077,4 Reham On 12/27/98
*E301077,4 Reduce the opened file on the program level:
*E301077,4 1- Change the files in SydObjct:
*E301077,4    From : RETAUTH,CUSTOMER,RETHDR,RETLINE,STYDYE,STYINVJL,STYLE,RALINE,INVHDR,INVLINE,INVCHRG,SALESREP,SCALE,REPCOMM,CREDIT,CONSINVH,SYCINT,SYCCOMP,CODES,ORDHDR,CONSINVL,NOTEPAD,UNCMSESS,SYUEROR,WAREHOUS,ARCUSHST,ICSTYHST,SYCFACT|RETAUTH,CUSTOMER,RETHDR,RETLINE,STYDYE,MFGOPR,STYLE,RALINE,INVHDR,INVLINE,INVCHRG,SALESREP,SCALE,REPCOMM,CREDIT,CONSINVH,CCONTCODE,CCOMP_ID,CCODE_NO,ORDACCT,CONSINVL,NOTEPAD,TRANS,CSESSION,WAREHOUS,ACTHST,STYHST,CFACCODE
*E301077,4    To   : RETHDR,RETLINE,CUSTOMER,STYLE,RETAUTH,RALINE,SCALE,CODES,UNCMSESS|RETHDR,RETLINE,CUSTOMER,STYLE,RETAUTH,RALINE,SCALE,CCODE_NO,TRANS
*E301077,4 2- Open the removed files from SydObjct in the current program.
*B602641,1 Reham On 03/04/99
*B602641,1 To void the error of not finding file "RepComm" upon saving.
*E301142,4 Reham On 03/08/99
*E301142,4 Change the way of using style price level if there is price level Q 
*E301142,4 or the current price level is zero.
*B602688,1 Reham On 03/18/99
*B602688,1 1- If Gl is set at division level, get GL Link code from division.
*E301176,1 HDM 03/23/1999 
*E301176,1 Prevent programs from displaying notepad icon as it's now controlled globally
*B602719,1 Reham On 03/30/99
*B602719,1 1- Upon check if the style was shipped to this customer before or not, do not
*B602719,1    check the invoice currency if the system was single currency.
*B602719,1 2- Open the invoice file in the save procedure to void an error if saving 
*B602719,1    uncomplete session.
*B602719,1 3- Save the record to be voided in the temp header file to be retrieved if 
*B602719,1    uncomplete session.
*B602732,1 Reham On 04/04/99
*B602732,1 1- Fix the problem of voiding credit memo to void all lines instead of the 1st one only.
*B602732,1 2- Get the right GL year & period upon voiding credit memo.
*B602732,1 3- Update the next step variable before & after updating it from gfStyCrl().
*B802088,1 Reham On 04/07/99
*B802088,1 Pick the store address in the from address if the store field 
*B802088,1 is not empty.
*B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*B602753,1 Will Call it
*B602773,1 Reham On 04/11/1999
*B602773,1 1- Remove the posting date from the void date screen.
*B602773,1 2- Void date should not be less than the credit memo date.
*E301200,1 AHM 04/15/99 Add parameter for return lineno to update the added field(lineno)
*E301200,1 in StyInvJl
*E301192,1 AHM 04/19/1999 Update reference field in styinvjl with descriptive text
*B802213,1 Reham On 05/02/99
*B802213,1 1- If the customer tax rate is zero, do not use the tax rate of the company setup.
*B802213,1 2- If change the gross price value, refresh the received amount in the header folder.
*B602841,1 Reham On 05/02/99
*B602841,1 Validate the at Qty. price only if not from invoice or RA
*B602783,1 Reham On 05/02/99
*B602783,1 Force to push button new if last size.
*E301216,1 Reham On 05/04/99
*E301216,1 Do the following if void credit memo:
*E301216,1 1- Stop deleting the credit record in the credit file.
*E301216,1 2- Add debit transaction in the Debit file.
*E301216,1 3- Put both credit & debit transaction in temp. history file and
*E301216,1    send it to the keyoff program.
*B802222,1 Reham On 05/05/99
*B802222,1 Point to the first line in the browse if collecting data.
*B602863,1 Reham On 05/12/1999
*B602863,1 1- Disable the division if entered style.
*B602863,1 2- Change the scan condition in the style validation.
*B602897,1 Reham On 05/16/1999
*B602897,1 Change the transaction type for the debit adjustment to be "R" instead of "2"
*E301224,1 Reham On 05/18/1999
*E301224,1 Void credit memo with the receiving cost if there is no transactions
*E301224,1 has been done between the receiving & issuing.
*B602750,1 Reham On 05/25/1999
*B602750,1 1- Disable the detail folder in the select mode
*B602750,1 2- Point to the header folder after save or cancel.
*B602926,1 Reham On 05/25/1999
*B602926,1 Fix the error of "Variable lcTmpAr Not Found."
*B602927,1 Reham On 05/25/1999
*B602927,1 Do not blank the location popup if blank the RA#.
*B602932,1 Reham On 05/26/1999
*B602932,1 Do not change style price if change the qty. if this style was 
*B602932,1 shipped to this customer before.
*B602959,1 Reham On 06/10/1999
*B602959,1 Give the ability to change the location.
*E301245,1 SSE 06/09/1999 Added a new field nHgWtrMark and calcualte its value
*E301245,1 from NETBAL field , if less than NETBAL update with 
*E301245,1 NETBAL else leave it with existing value
*B802221,1 Reham On 06/10/1999
*B802221,1 Disable the tax amount in the header folder.
*B802223,1 Reham On 06/13/1999
*B802223,1 Fix the received Qty & amount if from R/A.
*B603003,1 Reham On 06/21/1999
*B603003,1 Do not update the lines numbers in the credit memo lines
*B603003,1 in the save procedure if called from point of sale to 
*B603003,1 prevent the error of not adding some lines.
*B603054,1 Reham On 07/07/1999
*B603054,1 Use the return merchandise account instead of the sales revenue
*B603054,1 account if the return from invoice for category key "020".
*B802379,1 Reham On 07/19/1999
*B802379,1 Adjust the qty. objects in the screen to void accepting negative values.
*B802414,1 Reham On 07/19/1999
*B802414,1 Display only the received qty. in the details browse.
*B602415,1 HDM 08/18/1999 Incorrect Open & received Quantities values when updating
*                         RETAUTH File (case Void Credit Memo)
*B603149,1 TAK 09/07/1999 Added STR(lineNo,6) to the STYINVJL file index expresion
*B603149,1                and Make a coresponding changes due to that.
*B603159,1 HDM 09/19/1999 Fixing bug of saving the system date in arhist instead of void date
*B802671,1 HDM 10/14/1999 We should disable the return entire invoice case 
*                         we found there is a vaild invoice While validating
*                         R/A Number
*B802736,1 HDM 10/25/1999 - Fixing bug of not collecting RA data case we have
*                           invoice not fount in RA hedaer
*                         - Position is off the screen in thermo.
*B603220,1 HDM 11/01/1999 Don't update the OPENCR AND NETBAL in Customer file
*                         as they're updated by lfKeyOff before
*B802803,1 Reham On 11/21/1999
*B802803,1 Change the program & its screens to be shared between modules.
*B802907,1 Reham On 12/24/1999
*B802907,1 Fix the problem of updating the original style instead of the returned style upon saving.
*B603339,1 HDM 01/30/2000 Incorrect calculations for taxes case country is England
*B603538,1 Reham On 03/29/2000
*B603538,1 Fix the error of "Variable lcOpenLine not found" if called from PS module.
*B803169,1 Reham On 04/04/2000
*B803169,1 Fix the error of "Variable lnLineCnt not found" if create from consolidated invoice.
*B803252,1 Ramy on 05/10/2000 When return to 2nd or damaged Quality we should select style with equivalent or less scale count
*B603666,1 Ramy on 06/19/2000 If the country is England we should calculate the taxes in the store level
*B603741,1 WAB 07/17/2000 fix the bug of not saving the credit memoline in case of having the same 
*B603741,1 WAB            style/colour showing twice on two separate lines.
*B603820,1 NAD 08/22/2000 Fix bug even though the system was set to Standard Costing 
*B603820,1                (the average cost was being written into the COST field on Retline)
*E500374,6 WAB 09/05/2000 get the Gl Link_code from factor file if the customer has factor.
*B603881,1 ABD 09/27/2000 Fix bug numeric over flow while saving a credit memo.
*B603939,1 WAB 10/02/2000 Fix the bug of not saving all styles if we add more than 10 styles
*B603713,7 KAM 10/11/2000 change the width of fields price and amount,also
*B603713,7 KAM             we change the object of screen rmcrmem.scx
*B603967,1 ADEL 10/30/2000 CASE ENGLAND . When creating CR without entering an Inv #, Calculate trade disc
*B603967,1                 from term code entered in this CR not from the last invoice shipped 
*B603967,1                 on the enetred style. Also don't overwrite the invoice term code 
*B603967,1                 upon the selected term code.
*E301490,1 AHM 11/27/2000 Move lfSavRet,lfGetTrdDs,lfCrtUnComp,lfHgWUpdat functions to program
*E301490,1                RMSave to be used for Inter-Company Communication Module
*B604060,1 KAM 24/12/2000 KAM recalculate the other amount in case more than 
*B604060,1                one item each of them has different tax rate 
*C102212,1 ADEL 03/14/2001 Add Canadian HST tax to setup and add it on the Charge folder.
*B604219,1 KAM 04/17/2001  reclalculate the  tax amonut while change the terms payment codes
*B604763,1 MHM 08/16/2001  FIx the bug of wrong discount in case of come from invoice 
*B604763,1               (inqure not checked)and style belong to the invoice 
*B604896,1 MHM 10/11/2001 The Sales link on the style for the credit memo is wrong for RetLine
*B605036,1 ABD 10/30/2001 Let the Credit Memo change the commision if 
*B605036,1 ABD            Commisionset by division   
*B605151,1 ADEL 11/28/01 Prevent non-active customers to create Crefit Memos.
*B605165,1 ADEL 12/6/01 Disable 'Return entire invoice' when generating CR from RA.
*B605256,1 RAE 01/10/2002 Assign the damaged style to different warehouse.
*B605438,1 RAE 02/11/2002 the system accept a posted date less than the Credit memo date.
*B605322,1 RAE 03/07/2002 Fix the bug of not gettihg the commison from the customer table if the 
*B605322,1 RAE            rep doesn't have any commison on the devision.
*B605881,1 ABD 05/23/2002 Get the main account addresses or main store addresses not 
*B605881,1 ABD            the alternatives addresses for the account or the store.
*C200341,1 ABD 05/23/2002 Add new Custom trigger for customer stuncroft.
*B606120,5 ASH 06/20/2002 Prevent the reduce Sales rep commision message from appearing based on trigger.
*B606206,1 TMI 07/04/2002 Validate sales rep by store
*B605539,1 ADEL 07/08/2002 Don't save credit memo with empty reason per line.
*B606193,1 ADEL 07/09/2002 Decrease Retun qty\Amount in R\A header file when Voiding CR.
*B605679,1 ADEL 07/09/2002 Initialize lnDiv before gfSetup() for the customer screens program not to overwrite it.
*B605679,1 ADEL            when calling it when creating new CR. 
*B605261,1 ADEL 07/16/02   Calulate the tax properly in case of making CR from Invoice
*B605495,1 ABD  08/04/2002 Fix problem that the Credit memo calculating tax CUSTOMER.lVatExem = .T.
*B606248,1 ADEL 08/05/02   In case of England, when returning with invoice, the invoice has
*B606248,1                 charges and the charges has tax, once we enter the invoice we must add
*B606248,1                 the charges tax also.
*B605261,4 ADEL 08/12/02 Adjust Tax amount entirely in case of ! England
*B605135,1 ADEL 08/25/02 Once we entered a RA, disable the Division without waiting getting into 
*B605135,1 ADEL          the 2nd folder and collecting lines.
*B606264,1 ADEL 08/25/02 When voiding a CR made from RA with quantity less than in the RA and 
*B606264,1 ADEL          cancelling the RA, put balanced records in GLDIST.
*B606272,1 ADEL 08/26/02 When consolidated invoice, Get all lines not the first one.
*B606152,1 ADEL 08/26/02 In add mode, if the customer has S/R enable its commission fileds also.
*B606152,1 ADEL          Also get the rep commission at the store level for the customer
*B606404,1 TMI 09/12/2002 Fix the bug that The stock is dropped when voiding a cridet memo 
*                         depends on RA with Qtys less RA's qtys
*C102676,1 SSE 09/03/2002 Custom modifications add 3rd Rep and comm for OrdHdr file.
*B606480,1 ADEL 09/19/02 When voiding the CR don't duplicate the open qty.
*B606429,1 ADEL 09/22/02 Don't let the open be greater than the budget of RA when avoiding CRs.
*B606429,4 ABD  11/26/2002 The status of R/A does not alert when Void and the user didn't 
*B606429,4 ABD             Need to rebuild the R/A.
*B606726,1 ABD  12/04/2002 Fix bug that pieces on size which does not exist in style in case we
*B606726,1 ABD             Return on a damage style with different scale number.
*B606720,1 ABD  12/22/2002 Fix Bug that the credit memo get the incorrect tax.
*B606774,1 ABD  01/19/2003 Fix bug that the program does not create stydye for new warhouse.
*B607008,1 ASH 03/02/2003 Credit Memo for entire invoice displays no details for cons invoice.
*B607104,1 ABD April 20, 2003 upon mail from Tony & Approve from Amin.
* All we need to be able to do here is simpley edit the Tax amount which will obviously change 
* The tax amount and the total.   The Charges values will have nothing to 
* Do with this.  Simply, the customer sees the tax calculation, decides that it needs to 
* Be amended up or down for rounding and edits it.
* After this mail I we will enable Tax amount field only.
*C200517,1 KHM 04/30/2003 New trigger to customize the tax amount calculation.
*C200512,1 KHM 05/04/2003 New trigger to add credited quantities to sales order.
*B607190,1 ABD 05/26/2003 Fix bug STYLE /DIVISION COMFLICT when browse for 
*B607190,1 ABD            RA then cancel without selecting any RA.
*B607195,1 KHM 06/03/2003 Fix the bug of not including the trade discount percentage in case of
*B607195,1                no invoice is selected, and update the gldist with charges record
*B607195,1                in case of England
*B607289,1 KHM 06/16/2003 Prevent the user to leave the return to style empty in case of 
*B607289,1                damage/second quality.
*B607308,1 KHM 06/17/2003 Fix the problem of getting the wrong customer tax rate and laVtExem.
*B607308,1                because of changing the pointer of the customer.
*B607363,1 AMH 07/08/2003 Fix the bug of sizes fields don't match the return style sizes.
*B607152,1 TMI 07/30/2003 Filter Rethdr only to credit memos for the current account if screen is opened from Customer Screen
*:E302198,1 ALB 08/17/2003 change tax rate to print 99.999 formate
*:B119936,1 ABD 10/02/2003 Fix bug that GST tax field didn't  calculated automatically and placed 
*:B119936,1 ABD            In the credit memo's "Tax Amount" field. the field is remaining empty. 
*:B120085,1 ASH 11/05/2003 Fix bug Incorrect tax amount on credited/returned merchandise in case return Entire invoice.
*:B120187,1 ASH 11/05/2003 Fix the bug of computing tax on charges even if the customer vat exempted.
*:B120763,1 ASH 12/15/2003 Don't allow to select Completed or Cancelled R/A's.
*:B121145,1 ABD 01/19/2004 Fix bug that the user Unable to void a credit memo for any account.
*:B121250,1 ABD 02/05/2004 Voiding CM allows user to post to a locked period.
*:B037264,1 NNA 01/29/2004 Fix Bug That occurs when we Generate credit memo with invoice# and 
*:B037264,1 NNA            return entire Invoice "Yes".and we remove all the invoice lines
*:B037264,1 NNA            and we change the Invoice# No. then the new invoice lines not shown
*:B121842,1 NNA 03/01/2004 Fix bug of that the system show the style cost screen for the user (Operator)
*:B121842,1 NNA            even he not granted "Style cost"
*:C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*:B037963,1 NNA 04/15/2004 fix bug of that the invoice charges not updated in 'Other' Field when we
*:B037963,1 NNA            Check the [Return entire Invoice]
*:B122626,1 ASH 04/21/2004 Fix the bug of saving wrong gl link code if the customer's factor has link code other than DEFDEF.
*:B122824,1 NNA 05/26/2004 fix bug that the Location Popup don't show the location correctly specially
*:B122824,1 NNA            if the Location Code is the same of the Location Description i.e (the Code is [LIC]
*:B122824,1 NNA            and the description is [LIC] too)
*:C123847,1 TMI 12/28/2004 Add a trigger for DIR03 to update the EMPLOYEE field from RALINE
*:C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*:B126474,1 NNA 04/12/2005 Fix bug that when you Return a Style to another Style and Quality,system
*:B126474,1 NNA            updates cost of the Original Style not the Return to style.
*:B127806,1 EIH 05/22/2005 Fix bug that when you select entire invoice  in a Credit Memo the 
*:B127806,1 EIH            values  should exactly match the invoice AND another bug when we making credit memo
*:B127806,1 EIH            with return authorization.
*:B127290,1 NNA 06/24/2005 Fix bug that if you do not access the Charges screen while generating the credit memo and then 
*:B127290,1 NNA            save the credit memo, the charges will not update.
*:B127187,1 EIH 07/15/2005 Fix bug of duplicated lines when transfer from detail and header folder
*:B127187,1 EIH            And another bug that when select entire invoice before select invoice# tax is wrong.
*:B127187,1 EIH            128084 also fixed in the same issue.
*:B129286,1 MMR 12/20/2005 Fix bug of retaining the header amounts after saving.
*:B130434,1 MMR 12/20/2005 Fix bug of showing wrong tax amount if u choose entire invoice then
*:B130434,1 MMR            add line to the invoice and remove it.
*:B129286,2 MMR 01/03/2005 Fix bug of retaining the header amounts after saving then make new one.
*:B131092,1 MHM 03/07/2005 Fix bug of Invalid charge fields on new credit memo.
*:B130326,1 MHM 03/27/2006 Fix bug of Notepad disabled in case of go to detail folder them come back to header in add mode.
*:B131790,1 MHM 03/07/2005 Fix bug of Tax amount on cr memos being removed on saving .
*:B131583,1 MHM 03/07/2005 Fix bug of Cr memo does not compute correctly with line discount  .
*:E131400,1 NNA 04/12/2006 get the default warehous as saved in the file that if there is no default warehouse assigned 
*:E131400,1 NNA            to the invoice screen
*:B131872,1 NNA 04/20/2006 Fix bug that if you have an invoice with freight and you want to return it , this charges not 
*:B131872,1 NNA            display in others field, and fix some errors as a result of B#127806
*:B039660,1 NNA 06/06/2006 Convert CP#123853 and CP#037816 (Bin Location) to be used within Binmain.Prg.
*T20060817.0014 TMI 09/19/2006 run the lfStycrl that replaces the global one in BINMAIN to issue from the correct receiving session
*:B607818,1 ASH 11/02/2006 Fix bug of styles duplicated when retrun entire invoice if the invoice is consolidated.
*:B607994,1 TMI 03/11/2007 Fix rounding problems ( T20061031.0001 )
*:B608078,1 NNA 05/11/2007 Fix bug that Credit Memo incorrectly applying merch discount when assigned to an invoice (T20070327.0010)
*:B608078,1 NNA            and also Fix bug that Credit memo charges not updating RETHDR (T20070423.0009)
*:B608150,1 TMI 07/03/2007 allow the return cost to be of the returned style only when this credit memo is not based on invoice ( T20070417.0029 ) 
*:B608300,1 WAM 10/02/2007 fix bug of not updating all credit memo lines when some lines are returned to damage
*:B608305,1 NNA 10/04/2007 Fix bug of wrong updates the STK fields for out of range sizes if the customer created a 
*:B608305,1 NNA            credit memo and select a style with fewer sizes as a return to style.
*:E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] 
*:*********************************************************************************************************************

PARAMETERS lcCredit
EXTERNAL ARRAY laData , laKeyField , laDefProc , laFoldWinds

*-- Array to behave the working of gfSeekRec.
DECLARE laKeyField [1,4]
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'RetHdr'
laKeyField[1,4] = 1

*-- Add more buttons to the control pannel.
*E301176,1 HDM 03/23/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*DECLARE laPanelObj[2,3]
DECLARE laPanelObj[1,3]

*laPanelObj[1,1] = "pbNotes"
*laPanelObj[1,2] = gcBmpHome + "NOTES2.BMP"
*laPanelObj[1,3] = "VALID lfvCrNotes() MESSAGE 'Credit Memo Notes'"

laPanelObj[1,1] = "pbLinkTo"
laPanelObj[1,2] = gcBmpHome + "Relate.BMP"
laPanelObj[1,3] = "VALID lfvLinkTo() MESSAGE 'Object Link'"

*E301176,1 HDM 03/23/1999[End]


*B127187,1 EIH 05/22/2005 Declare variable to hold if we collect lines or not [Begin].
STORE .F. TO llCollect
*B127187,1 EIH [End].



*-- Variables hold the window names.
lcCrMem0   = ""     && RmCrMem0 Screen.
lcCrMem1   = ""     && RmCrMem1 Screen.
lcCrMem2   = ""     && RmCrMem2 Screen.
lcCrMem2A  = ""     && RmCrMe2A Screen.
lcCrMem2B  = ""     && RmCrMe2B Screen.

*-- Array hold the folders info.
DECLARE laFoldWinds[2,2]
laFoldWinds = SPACE(0)

lcfoldprnt  = SPACE(0)        && window parent name for the folder.
lcfolder    = SPACE(0)
lcWFoldChng = SPACE(0)        && function to control shows after change the folder.
lcfoldpush  = SPACE(0)        && push button name for the next folder.
lnActfolder = 1               && Current active folder.

*-- Position of the folders on the base window.
lnFolderCen = 0
lnFolderRen = 0

*B605261,4 (Begin) This variable to hold the amount taxable only.
lnDueTax =0 
*B605261,4 (End)

*B037963,1 NNA 04/15/2004 (Begin) New variable To know if the user checked the [Return Entire Invoice]
*B037963,1 NNA            For the First time   
STORE .F. TO llChkRinv
*B037963,1 NNA (End)

*B127806,1 EIH 05/22/2005 Define varaible if entire invoice or not [Begin].
STORE .F. TO llEntrInv ,llInvHdChg
*B127806,1 EIH 05/22/2005 [End].


*-- Arrays hold the different codes used in the codes popups.
DECLARE laCodInfo[3,10]   && Array hold codes info in header folder & file.
DECLARE laReson1[1]       && Array hold the reason code info in header folder & file.
DECLARE laDiv[1]          && Array hold the division code info in header folder & file.
DECLARE laTerms[1]        && Array hold the terms code info in header folder & file.
DECLARE laCodInfo1[1,10]  && Array hold codes info in detail folder & file.
DECLARE laReson2[1]       && Array hold the reason code info in detail folder & file.

*-- Arrays hold the strucure of RETHDR & RETLINE files.
DECLARE laCrMemHdr[1,4] , laCrMemLin[1,4]

*-- Array hold all the available warehouses.
DECLARE laWareHous[1,2]

lcDelMesag    = 'void'  && Change the message bar of the delete button.
laDefProc[7]  = .F.     && Force to local void procedure  (lpDelScr)
laDefProc[9]  = .F.     && Force to local Save procedure  (lpSavScr)
laDefProc[10] = .F.     && Force to local close procedure (lpClsScr)

*-- Get the prompt of the available pictures on the screen.
lcBrowKey  = gcBmpHome + "ExtKey.BMP"
lcNwCrBmp  = gcBmpHome + "New1.BMP"
lcRmCrBmp  = gcBmpHome + "Rem1.BMP"
lcRmOkBmp  = gcBmpHome + "Ok.BMP"
lcRmCnBmp  = gcBmpHome + "Can.BMP"

*** Initialeze the needed variables ***
lcScFields  = ""        && Var. hold screen fields.
lcVScFields = ""        && Var. Hold screen void fields.
lcCrMemHdr  = ""        && Hold temp. return header file name.
lcCrMemLin  = ""        && Hold temp. return lines file name.
lcGLDstTmp  = ""        && Hold temp. gl distribution file name.

*B603741,1 WAB - Initialize 2 variable
*B603741,1 WAB - StART
lcCrMmStyl = gfTempName()	&&hold  name of 1st index tag for temp. file lcCrMemLin 
lcCrMmLine = gfTempName()   &&hold  name of 2st index tag for temp. file lcCrMemLin 
*B603741,1 WAB - END
*B602926,1 Reham On 05/25/99   *** Begin ***
*B602926,1 Define name for temp. file using in key off.
lcTmpAR   = ""
*B602926,1 Reham On 05/25/99   *** End   ***

*B606720,1 ABD - Define New Variable to the new tax screen for England. [Begin] 
lcEngChrg = gfTempName()
lcAcMask   = SPACE(0)         && Account mask
lnAcLen    = 0                && Account Length
lcAcntBrwF = SPACE(0)         && Chart of account browse fields.
lcAcntFld  = SPACE(0)         && Account field that will be validated.
lcAcntDesF = SPACE(0)         && Account Description.
STORE '' TO lcBrowseTl , lcInvPiktk

*:B131583,1 MHM 03/07/2005  [Start]
STORE SPACE(6) TO lcInvPiktk
*:B131583,1 MHM [End]

STORE 0 TO lnMerchTax,lnChrgtax
llNoThing  = lfSetGLMsk()
llFrstTime = .T.
*B606720,1 ABD - [End]


lcScrMode  = "S"                     && Hold screen mode.
lcBrowTitl = "Credit Memo Lines"     && Hold browse title.

*-- Variables used to deal with the uncomplete program session concept.
lcSession  = SPACE(1)                && Var. hold session no.
lnUnCmSeRc = 0                       && Hold record no. in uncomplete session file.
lcProgID   = PADR('CRDT_MEMO',10)    && Hold current program Id to be saved in uncomplete session file.
lnThisSess = gnProgCopy              && Var. use in global function uncomplete session.
llGoAndChk = .T.                     && Flag to know if recheck the uncomplete session.

lcRA_Inv   = SPACE(6)   &&  Var. hold the original invoice for the entered RA#.
lcCurrCode = SPACE(3)   &&  Variable to hold currency code.
lnExRate   = 0          &&  Variable to hold exchange rate.
lnCurrUnit = 0          &&  Variable to hold currency unit.
lcUntSin   = '/'        &&  Variable to hold unit sign.
lcExRSin   = '/'        &&  Variable to hold exchange rate sign.

lcGLFYear  = SPACE(4)   && Variable hold Fiscal Year
lcGLPeriod = SPACE(2)   && Variable hold Period

llReBrowse = .T.        && Flag to know if execute the browse or not.
llAddMode  = .F.        && Flag to know if entered the add mode or not.
llContinue = .F.        && Flag to know if there is uncomplete session.
llComSess  = .F.        && Flag to know if there is uncomplete session.
llCUpDate  = .F.        && Flag to determine if there is something changed in the screen.
llFoldFrst = .F.        && Flag to know if enter once the detail folder in add mode.


*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.

*-- Variable hold the file used in the detail browse.
lcRCurAlis = IIF(laScrMode[1] .OR. laScrMode[4] , lcCrMemLin , 'RETLINE')

*-- Variable hold the browse key.
lcKeyRet   = IIF(laScrMode[4] , laData[2] , laData[1])

*-- Flags used in the program.
STORE .F. TO llBrowse , cbEntr_Inv , llVatExem , llRet2Dam , ;
             llShipped , llAddLine

*-- Variables hold the seq. # for create or void credit memo.
STORE ""  TO lcVRGLSess , lcRmGlSess

*-- Flags to know if the seq. # created or not for create or void credit memo.
STORE .F. TO llVRGLSess , llRmGlSess

*-- Variables hold the object status {Disable or Enable} in the header
*-- and detail screen to control the disabling & enabling of the screen.
STORE "DISABLE" TO lcInvStat , lcEdtStat , lcInqStat , lcOrdStat , ;
                   lcLinStat , lcNewStat , lcWarStat , lcDivStat , ;
                   lcCurStat , lcExRStat , lcEntrStat , lcGLStat , ;
                   lcRetStat , lcStorStat , lcDyeStat , lcRAStat , ;
                   lcRp1Stat , lcRp2Stat

*B606720,1 ABD - Variable to hold the charge status. [Begin]
lcChgStat = IIF(laScrMode[4],"ENABLE", "DISABLE")
*B606720,1 ABD - [End]

*-- String variables hold values used in the program.
STORE " " TO lcAccName , lcStatus , lcCstCntCd , lcOldValue , ;
             lcCstSlLnk , lcInvSlLnk , lcStySlLnk , lcDefWare , ;
             lcTaxName , lcCustLink

*-- Variables hold the address values.
STORE " " TO lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,lcAddress6

*-- Define the dates used in the program.
STORE {}  TO ldRaDate , ldRaVoid , ldOldDate

*-- Define numeric variables used in the header folder.
STORE 0   TO lnAuthPcs , lnAuthAmt , lnInvTrdDs , ;
             lnDiscPcnt , lnPstRate , lnTrdeAmt  , ;
             lnOldValue , lnDfltPstR , lnTaxLine , lnMarker
*C102212,1 (Begin) Intialize HST Tax Rate.
STORE 0 TO lnHstRate,lnDefHst
*C102212,1 (End)
*-- Define the string variables used in the detail folder.
STORE " " TO lcStyle , lcStyDesc , lcReason , lcCurLine ,;
             lcTranCd , lcRetSty , lcStyGrade ,lcScale  ,;
             lcDyelot , lcOrgStyGr , lcTax_Meth , lcPstRule

*-- Define the numeric variables used in the detail folder.
STORE 0   TO lnTotQty , lnPrice , lnAmount , lnGrsPrice , lnDiscLnPc ,;
             lnDfTaxRat , lnTaxRate , lnScaleCnt , lnCost , lnDisc_Amt ,;
             lnTrde_Amt , lnPstTotal

*-- Set the different popups variables to be the first value.
STORE 1 to lnReson2
STORE 1 TO puGrdPop , puWareHous

*-- Array hold the needed variables if lost the current session.
*E301216,1 Reham On 04/05/99   *** Begin ***
*E301216,1 Define variables will be used by the key off program if void credit memo.
STORE ""  TO lcAdTrnSeq , lcAcTrnSeq , lcHisSeq , lcGlSess , lcRepBat
STORE .T. TO llUpdGlDif , llUpdMstGL
*DECLARE laVars[9]
DECLARE laVars[16]
*E301216,1 Reham On 04/05/99   *** End   ***
*-- Fill the variables array.
laVars[1]  = 'lcInvSlLnk'
laVars[2]  = 'lcCstSlLnk'
laVars[3]  = 'lcVRGLSess'
laVars[4]  = 'lcRmGlSess'
laVars[5]  = 'llVRGLSess'
laVars[6]  = 'llRmGlSess'
laVars[7]  = 'lcScrMode'
laVars[8]  = 'lnUnCmSeRc'
laVars[9]  = 'llFoldFrst'
*E301216,1 Reham On 05/04/99   *** Begin ***
laVars[10] = 'lcAdTrnSeq'    && Used by Key off program
laVars[11] = 'lcAcTrnSeq'    && Used by Key off program
laVars[12] = 'lcHisSeq'      && Used by Key off program
laVars[13] = 'lcGlSess'      && Used by Key off program
laVars[14] = 'lcRepBat'      && Used by Key off program
laVars[15] = 'llUpdGlDif'    && Used by Key off program
laVars[16] = 'llUpdMstGL'    && Used by Key off program
*E301216,1 Reham On 05/04/99   *** End   ***

*-- laStyGrade -> Hold the styles grade.
*-- laSize     -> Hold the scales header.
*-- laQtyStk   -> Hold the qty. stock.
DECLARE laStyGrade[1] , laSize[8] , laQtyStk[8]
laStyGrade = " "
laSize     = " "
laQtyStk   = 0

lcStyHdr   = ""    && Hold the style header
lnStyLngth = 0     && Hold the style code lenght.
lnCrMemNo  = 0     && Hold the lines no.
lcInvTerms = ""    && Var. hold the terms code if receive from invoice.

*B802907,1 Reham On 12/26/1999   *** Begin ***
*B802907,1 Variable to hold the counter line no.
lnCrLinCnt = 0
*B802907,1 Variable hold name of temp. file 
lcOpenLine = ""
*B802907,1 Variables hold the RA qty. & amount.
lnRAQty = 0 
lnRAAmt = 0
*B802907,1 Reham On 12/26/1999   *** End   ***

*-- Logical variables will hold the company or module setup values.
STORE .F. TO llMulCurr , llEditExRt , llMultiWH , llUseDyes ,;
             llDiv_Seq , llAlowNew , llIsCanada , llIsEnglnd ,;
             llLink_GL , llAvg_Cost , llDiv_Link , llTax ,;
             llPostfInv , llValidDt
             
*B605036,1 ABD - Define new variable to know if the sales rep
*B605036,1 ABD - Work with division. [Begin]
STORE .F. TO llRep_div  &&  Salesrep commission base on on Division.
*B605036,1 ABD - [End]

*B603820,1 (Start) variable to check if the cost method is Standerd or not
STORE .F. TO llStdCost
llStdCost = ALLTRIM(gfGetMemVar('M_COST_METH',gcAct_Comp)) = 'S'
*B603820,1 (End)
*B605679,1 (Begin) Initialize lnDiv here.
PRIVATE lnDiv
lnDiv = 1
*B605679,1 (End)
*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF

*-- Array for the header data, & variable hold the header fields.
*C102212,1 (Begin) Increase it one element for HST Tax.
*DECLARE laData[41]
DECLARE laData[42]
*C102212,1 (End)
*-- Define the normal screen fields.
lcScFields = "CrMemo , Account , RaNo , Store , CrDate , Status , "+;
             "Invoice , Order , cFacCode , Cartons , CustPo , "+;
             "Reference , Reason , cDivision , cIntr_Inv , SalesRep1 , "+;
             "CommPcnt1 , CommAmt1 , SalesRep2 , CommPcnt2 , "+;
             "CommAmt2 , Pieces , Gross_Amt , Disc_Amt , Amount , "+;
             "Tax_Amt , Other , TotCredit , cWareCode , nPstAmt , "+;
             "cCurrCode , nCurrUnit , nExRate , link_code , cFrgtAcnt , "+;
             "cTaxAcnt , cArAcnt , cRetNote1 , cRetNote2 , cTermCode , "+;
             "dPostDate"
*-- Define the void screen fields.
lcVScFields= "CrMemo , Account , RaNo , Store , CrDate , Status , "+;
             "Invoice , Order , cFacCode , Cartons , CustPo , "+;
             "Reference , Reason , cDivision , cIntr_Inv , SalesRep1 , "+;
             "CommPcnt1 , vCommAmt1 , SalesRep2 , CommPcnt2 , "+;
             "vCommAmt2 , vPieces , vGross_Amt , vDisc_Amt , vAmount , "+;
             "Tax_Amt , vOther , vTotCredit , cWareCode , nPstAmt , "+;
             "cCurrCode , nCurrUnit , nExRate , link_code , cFrgtAcnt , "+;
             "cTaxAcnt , cArAcnt , cRetNote1 , cRetNote2 , cTermCode , "+;
             "dPostDate"
*C102212,1 (Begin) Add nHstAmt field.
lcScFields  = lcScFields  + ',nHstAmt'
lcVScFields = lcVScFields + ',nHstAmt'
*C102212,1 (End)
llAlowNew = .T.     && Flag to allow adding new credit memos.
llNoShow  = .F.     && Flag to force the execution of the show procedure.

*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Open the company & international files.
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
=gfOpenFile(gcSysHome+'SycInt',gcSysHome+'cContCode','SH')
*E301077,4 Reham On 12/27/98   *** End   ***

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)
  *-- Var. hold the style header.
  lcStyHdr   = gfItemMask("HI")
  *-- Var. hold the length of the style.
  lnStyLngth = LEN(lcStyHdr)
  *** Get the main features for the current company ***
  *-- Post factored A/R to customer accounts or not.
  llPostfInv = ALLTRIM(gfGetMemVar('XPOSTFINV',gcAct_Comp)) = 'N'
  *-- Multi Currency.
  llMulCurr  = gfGetMemVar('llMulCurr',gcAct_Comp)
  *-- Edit Exchange rate.
  llEditExRt = gfGetMemVar('LLEDITEXRA',gcAct_Comp)
  *-- Multi warehouse.
  llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse',gcAct_Comp)) = 'Y'
  *-- Use Dyelot or not.
  llUseDyes  = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp))    = 'Y'
  *-- Sequence on Division or not.
  llDiv_Seq  = ALLTRIM(gfGetMemVar('M_Div_Seq',gcAct_Comp))   = 'Y'
  *-- See if there is GL link or not.
  llLink_GL  = ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp))   = 'Y'
  *-- If link by division.
  llDiv_Link = ALLTRIM(gfGetMemVar('M_DIV_LINK',gcAct_Comp))  = 'Y'
  *-- If use the style average cost or not.
  llAvg_Cost = ALLTRIM(gfGetMemVar('M_COST_METH',gcAct_Comp)) = 'A'
  *-- If country is Canada.
  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)
  *-- If country is England
  llIsEnglnd = IIF(UPPER(ALLTRIM(gcContCode)) = 'ENG', .T., .F.)
  *-- If calculate tax.
  llTax      = ALLTRIM(gfGetMemVar('M_TAX',gcAct_Comp)) = 'Y'
  *-- Tax method.
  lcTax_Meth = gfGetMemVar('M_TAX_METH',gcAct_Comp)
  *-- Get the tax rate.
  lnDfTaxRat = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
  *lnTaxRate  = lnDfTaxRat
  *-- Fill the style grade from the dictionary.
  lnGrdLen   = gfGetVld("cStyGrade" , @laStyGrade)
  
  lcTaxName  = gfGetMemVar('M_TAX_DESC' , gcAct_Comp)
  lcTaxName  = IIF(EMPTY(lcTaxName) , 'Gst Tax' , lcTaxName)
  
  *B605036,1 ABD - Define new variable to know if the sales rep
  *B605036,1 ABD - Work with division & open needed file. [Begin]
  *-- &&  Salesrep commission base on Division. 
  llRep_div = ALLTRIM(gfGetMemVar('M_REP_COMM' , gcAct_Comp)) = 'D'
  IF llRep_div
    = gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
  ENDIF
  *B605036,1 ABD - [End]
  
  *-- If multi currency.
  IF llMulCurr
    lcCurrCode = SPACE(3)   &&  Variable to hold currency code.
    lnExRate   = 0          &&  Variable to hold exchange rate.
    lnCurrUnit = 0          &&  Variable to hold currency unit.
    lcUntSin   = '/'        &&  Variable to hold unit sign.
    lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
  ELSE
    lcCurrCode = gcBaseCurr &&  Variable to hold currency code.
    lnExRate   = 1          &&  Variable to hold exchange rate.
    lnCurrUnit = 1          &&  Variable to hold currency unit.
    lcUntSin   = '/'        &&  Variable to hold unit sign.
    lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
  ENDIF
  
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the warehouse file.
  =gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  *-- Fill the warehouse array.
  IF !llMultiWH
    SELECT WAREHOUS
    GO TOP
    lcDefWare = WAREHOUS.cWareCode
    laWareHous[1,1] = WAREHOUS.cDesc
    laWareHous[1,2] = WAREHOUS.cWareCode
  ELSE
    lcDefWare = ""
    SELECT cDesc , cWareCode ;
      FROM WAREHOUS ;
      INTO ARRAY laWareHous
  ENDIF
  
  *-- Get the default country code.
  lcCstCntCd = ALLTRIM(gcContCode) && Set default to the country code.
  
  *-- Array hold the header's codes info.
  laCodInfo[1,01] = "REASON"       && Field Name
  laCodInfo[1,02] = "laReson1"     && Array Name
  laCodInfo[1,03] = "lnReson1"     && Popup Name
  laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = "RETHDR"       && Alternative File (For default val.)
  laCodInfo[1,08] = "RETHDR"       && Use this index for the Alternative file.
  laCodInfo[1,09] = "laData[1]"    && Seek this expretion.
  laCodInfo[1,10] = "REASON"       && Alternative Field Name

  laCodInfo[2,01] = "CDIVISION"    && Field Name
  laCodInfo[2,02] = "laDiv"        && Array Name
  laCodInfo[2,03] = "lnDiv"        && Popup Name
  laCodInfo[2,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[2,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[2,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[2,07] = "RETHDR"       && Alternative File (For default val.)
  laCodInfo[2,08] = "RETHDR"       && Use this index for the Alternative file.
  laCodInfo[2,09] = "laData[1]"    && Seek this expretion.
  laCodInfo[2,10] = "cDivision"    && Alternative Field Name
  
  laCodInfo[3,01] = "CTERMCODE "   && Field Name
  laCodInfo[3,02] = "laTerms"      && Array Name
  laCodInfo[3,03] = "lnTerms"      && Popup Name
  laCodInfo[3,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[3,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[3,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[3,07] = "RETHDR"       && Alternative File (For default val.)
  laCodInfo[3,08] = "RETHDR"       && Use this index for the Alternative file.
  laCodInfo[3,09] = "laData[1]"    && Seek this expretion.
  laCodInfo[3,10] = "cTermCode"    && Alternative Field Name
  
  *-- Array hold the lines' codes info.
  laCodInfo1[1,01] = "REASON"     && Field Name
  laCodInfo1[1,02] = "laReson2"   && Array Name
  laCodInfo1[1,03] = "lnReson2"   && Popup Name
  laCodInfo1[1,04] = ""           && Popup Status  ("D"->Default,"A"->All)
  laCodInfo1[1,05] = .F.          && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo1[1,06] = .F.          && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo1[1,07] = "RETLINE"    && Alternative File (For default val.)
  laCodInfo1[1,08] = "RETLINE"    && Use this index for the Alternative file.
  laCodInfo1[1,09] = "laData[1]+PADR(lcStyle,19)+lcCurLine+lcTranCd"    && Seek this expretion.
  laCodInfo1[1,10] = "REASON"     && Alternative Field Name
  
  *-- Fill the codes popups with N/A.
  = gfwCodePop(@laCodInfo , "CTERMCODE" , "N")  && Terms codes.
  = gfwCodePop(@laCodInfo , "REASON"    , "N")  && Header reason code.
  = gfwCodePop(@laCodInfo , "CDIVISION" , "N")  && Division code.
  = gfwCodePop(@laCodInfo1, "REASON"    , "N")  && Detail reason code.
  *-- If the program was called from another program, force to view mode.
  IF TYPE('lcCredit') = 'C' .AND. SEEK(lcCredit,'RetHdr')
    SELECT RetHdr
    SCATTER FIELDS &lcScFields TO laData
    laScrMode    = .F.
    laScrMode[2] = .T.
    *B607152,1  TMI [Start] Filter rethdr only to credit memos for the current account
    lnCurrRec = RECNO('RETHDR')
    lcSvOrd = ORDER('RETHDR')
    SET ORDER TO RETHDRA IN RETHDR
    =SEEK('M'+RETHDR.ACCOUNT,'CUSTOMER')    
    SET FILTER TO ACCOUNT = CUSTOMER.ACCOUNT    
    GOTO TOP
    GOTO (lnCurrRec)
    SET ORDER TO &lcSvOrd IN RETHDR
    *B607152,1  TMI [End  ] 
  ELSE
    *-- Fill the header array with the fields features.
    SELECT RetHdr
    SCATTER FIELDS &lcScFields TO laData BLANK
    *-- Go to the select mode at the begining.
    laScrMode    = .F.
    laScrMode[1] = .T.
  ENDIF
  
  *-- Get a unique session number, to be used to update the uncomplete 
  *-- session file.
  lcSession = gfsequence('CSESSION')
  
  *-- Get Temp. names for the screen set.
  lcCrMem0  = gfTempName()
  lcCrMem1  = gfTempName()
  lcCrMem2  = gfTempName()
  lcCrMem2A = gfTempName()
  lcCrMem2B = gfTempName()
  
  *-- Temp. name for the header file.
  lcCrMemHdr = gfTempName()

  *-- Temp. name for the lines file.
  lcCrMemLin = gfTempName()
  
  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Variable hold name of temp. file 
  lcOpenLine = gfTempName()
  *B802907,1 Reham On 12/26/1999   *** End   ***
  
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *B602719,1 Create temp. name for the GLDIST file.
  lcGLDstTmp = gfTempName()
  *B602719,1 Reham On 04/05/99   *** Begin ***
  
  *E301216,1 Reham On 05/04/99   *** Begin ***
  *E301216,1 Define name for temp. file using in key off.
  lcTmpAR   =gfTempName()
  *E301216,1 Reham On 05/04/99   *** End   ***
  
  *-- Adjust &fill the folders array.
  laFoldWinds[1,1] = "Header"
  laFoldWinds[1,2] = lcCrMem1
  laFoldWinds[2,1] = "Detail"
  laFoldWinds[2,2] = lcCrMem2
  
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnActfolder = 1                 && Current active folder.
  lcfolder    = gfTempName()
  lcWFoldChng = '=lfActFolder()'  && function to control shows after change the folder
  lcfoldpush  = 'pbFolders'       && push button name for the next folder
  
  *-- Position of the folders on the base window.
  lnFolderCen = 102.00
  lnFolderRen = 2
  *C102212,1 (Begin) Get HST Tax Rate.
  lnDefHst  = gfGetMemVar('M_HST_RATE',gcAct_Comp)
  lnHstRate = lnDefHst
  *C102212,1 (End)
ENDIF
*E500374,6 WAB - (Start) open Factore File
IF llLink_GL
  =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
ENDIF  
*E500374,6 WAB - (END) 


*-- Assign the right refreshing to the objects.
lcEdtStat = IIF(laScrMode[4] , "ENABLE" , "DISABLE")
lcRAStat  = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcStorStat= IIF(laScrMode[4] .AND. EMPTY(laData[3]) , "ENABLE" , "DISABLE")
lcInqStat = IIF(!EMPTY(laData[7]) .AND. !laScrMode[1] , "ENABLE" , "DISABLE")
lcInvStat = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcOrdStat = IIF(EMPTY(laData[8]) .AND. EMPTY(laData[3]) .AND. laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcWarStat = IIF(laScrMode[4] .AND. llMultiWH , "ENABLE" , "DISABLE")

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division popup if no invice & entered styles..
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) , "ENABLE" , "DISABLE")
*B605135,1 (Begin) Once we entered a RA, disable the Division without waiting getting into the 2nd folder and collecting lines.
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 AND (EMPTY(laData[3]) OR llFoldFrst), "ENABLE" , "DISABLE")
*B605135,1 (End)
*B602863,1 Reham On 05/12/1999   *** End   ***

lcCurStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') , "ENABLE" , "DISABLE")
lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")
lcEntrStat= IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcGLStat  = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. llLink_GL .AND. llDiv_Link , "ENABLE" , "DISABLE")
lcRp1Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[16]) , "ENABLE" , "DISABLE")
lcRp2Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[19]) , "ENABLE" , "DISABLE")

*-- Call local function to open the needed files in this session.
=lfOpnFil()

*-- Define the browse bar.
PUSH MENU _MSYSMENU
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcBrowTitl KEY ALT+B

*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C037816,1 MHM 04/06/2004 Define New Option Bar for entring Bin Location[Start]
IF ASCAN(laEvntTrig,PADR("RMDFNMNU",10)) <> 0
  =gfDoTriger("RMCRMEM",PADR("RMDFNMNU",10))
ENDIF
*C037816,1 MHM [End]
*B039660,1 NNA (End)

*-- Call the screen.
SELECT RetHdr
*B802803,1 Reham On 11/21/1999    *** Begin ***
*B802803,1 Change the program & its screens to be shared between modules.
*DO (gcScrDir+gcWinAppl+"\RMCRMEM.SPX")
DO (gcScrDir+"RMCRMEM.SPX")
*B802803,1 Reham On 11/21/1999    *** End   ***

*-- Release the browse window.
RELEASE WINDOW (lcBrowTitl)
POP MENU _MSYSMENU

*-- If quitting the program erase the temp. files.
IF glQuitting
  
  IF USED(lcCrMemLin)
    USE IN (lcCrMemLin)
  ENDIF
  ERASE &gcWorkDir.&lcCrMemLin..DBF
  ERASE &gcWorkDir.&lcCrMemLin..CDX
  
  IF USED(lcCrMemHdr)
    USE IN (lcCrMemHdr)
  ENDIF
  ERASE &gcWorkDir.&lcCrMemHdr..DBF
  ERASE &gcWorkDir.&lcCrMemHdr..CDX
  
  IF llLink_GL
    IF USED(lcGLDstTmp)
      USE IN (lcGLDstTmp)
    ENDIF
    ERASE &gcWorkDir.&lcGLDstTmp..DBF
    ERASE &gcWorkDir.&lcGLDstTmp..CDX
  ENDIF
  
  *E301216,1 Reham On 05/04/99   *** Begin ***
  *E301216,1 Close key off temp. File.
  IF USED(lcTmpAR)
    USE IN (lcTmpAR)
    ERASE (gcWorkDir+lcTmpAR+'.DBF')
    ERASE (gcWorkDir+lcTmpAR+'.CDX')
  ENDIF
  *E301216,1 Reham On 05/04/99   *** End   ***
  
  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Erase the temp. file 
  IF USED(lcOpenLine)
    USE IN (lcOpenLine)
  ENDIF
  ERASE &gcWorkDir.&lcOpenLine..DBF
  ERASE &gcWorkDir.&lcOpenLine..CDX
  *B802907,1 Reham On 12/26/1999   *** End   ***
  
  *B606720,1 ABD - Close the used file in case england charge. [Begin]
  IF llIsEnglnd
    IF USED(lcEngChrg)
      USE IN (lcEngChrg)
    ENDIF
    ERASE &gcWorkDir.&lcEngChrg..DBF
    ERASE &gcWorkDir.&lcEngChrg..CDX
  ENDIF
  *B606720,1 ABD - [End]
ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Show procedure.
*!*************************************************************
*! Calls     : lfChngFolder, lfChkUnComS
*!           : gfwCodePop, lfBrowLin, lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
*
PROCEDURE lpShow

*-- Change the prompt of the delete bar in the edit pad to be canceled 
*-- instead of delete.
DEFINE BAR 10 OF P03PU03 PROMPT '\<Void' SKIP FOR !(laScrMode[2] AND laData[6]<>'V')

*-- To disable the edit bar in the record pad in the system menu.
laCtrStat[7] = 'DISABLE'

*-- Disable the edit button on the control pannel all the time.
SHOW GET pbEdt DISABLE

*-- Determine the alias in the different modes.
lcRCurAlis = IIF(laScrMode[1] .OR. laScrMode[4] , lcCrMemLin , 'RETLINE')

DO CASE
  CASE laScrMode[1]
    *-- Flag to know if enter once the detail folder in add mode.
    llFoldFrst = .F.
    
    *B606720,1 ABD - Dele all data from used file in case england charge. [Begin]
    IF llIsEnglnd
      IF USED(lcEngChrg)
        lnOldAls = Select(0)
        SELECT (lcEngChrg)
        DELE ALL
        SELECT (lnOldAls)
      ENDIF
    ENDIF
    llFrstTime = .T.
    *B606720,1 ABD - [End]
    
    *B605261,4 (Begin) Reset it
    lnDueTax =0 
    *B605261,4 (End)
    
    *-- Check if there is previos uncompleted session.
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    
    *-- Variable hold the current mode type.
    lcScrMode  = "S"
    *-- Set default to the country code.
    lcCstCntCd = ALLTRIM(gcContCode)
    
    *-- Fill the codes popups with N/A.
    = gfwCodePop(@laCodInfo, "CTERMCODE" , "N")
    = gfwCodePop(@laCodInfo, "REASON", "N")
    = gfwCodePop(@laCodInfo, "CDIVISION", "N")
    = gfwCodePop(@laCodInfo1, "REASON", "N")
    STORE .F. TO llBrowse , cbEntr_Inv , llVatExem , llValidDt
    STORE " " TO lcAccName , lcStatus , ;
                 lcCstSlLnk , lcInvSlLnk , lcStySlLnk
    STORE " " TO lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,lcAddress6
    STORE {}  TO ldRaDate , ldRaVoid
    STORE 0   TO lnCrMemNo , lnAuthPcs , lnAuthAmt , lnTrdeAmt
    STORE " " TO lcStyle , lcStyDesc , lcReason , lcCurLine ,;
                 lcTranCd , lcRetSty , lcStyGrade ,;
                 lcScale  , lcDyelot
    *B603339,1 [Start] Zero out lnDiscPcnt
    *STORE 0   TO lnTotQty , lnPrice , lnAmount , lnGrsPrice , lnDiscLnPc ,;
                 lnTaxRate , lnScaleCnt , lnCost , lnDisc_Amt , lnTrde_Amt ,;
                 lnPstTotal , lnInvTrdDs
                 
    STORE 0   TO lnTotQty , lnPrice , lnAmount , lnGrsPrice , lnDiscLnPc ,;
                 lnTaxRate , lnScaleCnt , lnCost , lnDisc_Amt , lnTrde_Amt ,;
                 lnPstTotal , lnInvTrdDs ,lnDiscPcnt
    *B603339,1 [End]

    *B802907,1 Reham On 12/26/1999   *** Begin ***
    *B802907,1 Variable to hold the counter line no.
    lnCrLinCnt = 0
    *B802907,1 Variables hold the RA qty. & amount.
    lnRAQty = 0 
    lnRAAmt = 0
    *B802907,1 Reham On 12/26/1999   *** End   ***
    
    puWareHous = 1
    laSize   = " "
    laQtyStk = 0
    
    *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
    *E131400,1 NNA             close file again
    lnOldAlias = SELECT(0)
    SELECT WAREHOUS
    LOCATE FOR ldefware
    IF FOUND()
      PUWAREHOUS = ASCAN(laWareHouses,WAREHOUS.CWARECODE)
      PUWAREHOUS = IIF(PUWAREHOUS=0,1,ASUBSCRIPT(laWareHouses,PUWAREHOUS,1))
    ENDIF  
    SELECT(lnOldAlias)
    *E131400,1 NNA (End)

    *B602750,1 Reham On 05/25/1999   *** Begin ***
    *B602750,1 Point to the header folder after save or cancel.
    lnlastfold = lnActFolder
    lnActFolder= 1
    =lfchngfolder(lnActFolder)
    *B602750,1 Disable the detail folder in the select mode
    SHOW GET ibFolder(2) DISABLE
    *B602750,1 Reham On 05/25/1999   *** End   ***
    
    *-- Disable the notes button in the control pannel.
    SHOW GET pbNotes  DISABLE
    SHOW GET pbLinkTo DISABLE
  CASE laScrMode[2]
    *B131092,1 MHM 04/03/2006 Inintiate llEntrInv  to be False [Start]
    STORE .F. TO llEntrInv 
    *B131092,1  [End]

    *-- Flag to know if enter once the detail folder in add mode.
    llFoldFrst = .F.
    lcScrMode = "V"
    *-- If the credit memo was voided.
    IF laData[6] = "V"
      *-- Disable the void button.
      SHOW GET pbDlt DISABLE
      *-- Fill ladata array with the void fields.
      SCATTER FIELDS &lcVScFields TO laData
    ELSE
      *-- Enable the void button.
      SHOW GET pbDlt ENABLE
    ENDIF
    lnCrMemNo = 1
    
    *B802088,1 Reham On 04/07/99  *** Begin ***
    *B802088,1 Pick the store address in the from address if the store field 
    *B802088,1 is not empty.
    IF (!EMPTY(laData[4]) .AND. SEEK('S'+laData[2]+laData[4] , 'CUSTOMER')) .OR. ;
       (EMPTY(laData[4]) .AND.  SEEK('M'+laData[2],'CUSTOMER'))
      *-- Get the account information from the customer file.
      *IF SEEK("M"+laData[2],"CUSTOMER")
      *B802088,1 Reham On 04/07/99  *** End   ***
      *B605881,1 ABD Get the main account addresses or main store addresses not 
      *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
      *lcAddress1 = CUSTOMER.cAddress12
      *lcAddress2 = CUSTOMER.cAddress22
      *lcAddress3 = CUSTOMER.cAddress32
      *lcAddress4 = SUBSTR(CUSTOMER.cAddress42,1,10)
      *lcAddress5 = SUBSTR(CUSTOMER.cAddress52,1,10)
      *lcAddress6 = CUSTOMER.cAddress62

      lcAddress1 = CUSTOMER.cAddress1
      lcAddress2 = CUSTOMER.cAddress2
      lcAddress3 = CUSTOMER.cAddress3
      lcAddress4 = SUBSTR(CUSTOMER.cAddress4,1,10)
      lcAddress5 = SUBSTR(CUSTOMER.cAddress5,1,10)
      lcAddress6 = CUSTOMER.cAddress6
      *B605881,1 ABD [End]
    ELSE
      STORE " " TO lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,lcAddress6
    ENDIF
    
    *B802088,1 Reham On 04/07/99  *** Begin ***
    IF SEEK("M"+laData[2],"CUSTOMER")
      lcCstCntCd = IIF(EMPTY(CUSTOMER.cCont_Code),ALLTRIM(gcContCode),CUSTOMER.cCont_Code)
      lcAccName  = CUSTOMER.BtName
      llVatExem  = CUSTOMER.lVatExem
    ELSE
      lcCstCntCd = ALLTRIM(gcContCode)
      lcAccName  = ""
      llVatExem  = .F.
    ENDIF
    *B603666,1 Ramy On 06/20/2000 Begin 
    IF !EMPTY(laData[4]) AND SEEK("S"+laData[2]+laData[4],"CUSTOMER")
      llVatExem  = CUSTOMER.lVatExem
    ENDIF
    *B603666,1 Ramy On 06/20/2000 End
    *B802088,1 Reham On 04/07/99  *** End   ***
    
    *-- Get the status variable.
    lcStatus   = IIF(laData[6] = "V" , "Voided" , "Active")
    *-- Fill the check box for intire invoice with its value.
    cbEntr_Inv = IIF(laData[15] = "Y" , .T. , .F.)
    *-- Get the R/A info.
    IF SEEK(laData[3] , "RETAUTH")
      ldRaDate  = RETAUTH.RaDate
      ldRaVoid  = RETAUTH.Void
      lnAuthPcs = RETAUTH.nreta_opn
      lnAuthAmt = RETAUTH.nRtOpnAmt
      *B802907,1 Reham On 12/26/1999   *** Begin ***
      *B802907,1 Variables hold the RA qty. & amount.
      lnRAQty = RETAUTH.nreta_opn
      lnRAAmt = RETAUTH.nRtOpnAmt
      *B802907,1 Reham On 12/26/1999   *** End   ***
    ELSE
      STORE {}  TO ldRaDate , ldRaVoid
      STORE 0   TO lnAuthPcs , lnAuthAmt
      *B802907,1 Reham On 12/26/1999   *** Begin ***
      *B802907,1 Variables hold the RA qty. & amount.
      STORE 0 TO lnRAQty , lnRAAmt 
      *B802907,1 Reham On 12/26/1999   *** End   ***
    ENDIF

    *B122824,1 NNA 05/26/2004 (Begin) Get the array Position for the Credit memo's Location
    *B122824,1 NNA              and then get the line of the array for this Position 
    *puWareHous = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29])/2 , 0)
    lnArPos = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29]) , 0)
    puWareHous = ASUBSCRIPT(laWareHous,lnArPos,1)
    *B122824,1 NNA (End)

    *-- Fill codes popups with its real values.
    = gfwCodePop(@laCodInfo, "CTERMCODE" , "T")
    = gfwCodePop(@laCodInfo, "REASON", "T")
    = gfwCodePop(@laCodInfo, "CDIVISION", "T")
    = gfwCodePop(@laCodInfo1, "REASON", "T")
    
    *-- Get the tax rate
    IF SEEK(laData[1] , "RETLINE")
      lnTaxRate = RETLINE.Tax_Rate
    ENDIF
    *-- Set flag to execute the browse.
    llReBrowse = .T.
    
    *-- Enable the notes & object linking buttons in the control pannel.
    SHOW GET pbNotes  ENABLE
    SHOW GET pbLinkTo ENABLE
    
    *B602750,1 Reham On 05/25/1999   *** Begin ***
    *B602750,1 Enable the detail folder.
    SHOW GET ibFolder(2) ENABLE
    *B602750,1 Reham On 05/25/1999   *** End   ***
  CASE laScrMode[4]
    
    *B127187,1 EIH 05/22/2005 llCollect = false to collect lines if we need [Begin].
    STORE .F. TO llCollect
    *B127187,1 EIH [End].
    
    *B131092,1 MHM 04/03/2006 Inintiate llEntrInv  to be False [Start]
    STORE .F. TO llEntrInv 
    *B131092,1  [End]

    *-- Create an "Open" record in the uncomplete session file.
    llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
    llGoAndChk = .T.
    lcScrMode  = "A"
    IF !llAddMode .AND. !llContinue
      *-- Flag to know if enter once the detail folder in add mode.
      llFoldFrst = .F.
      *-- Flag to force entering the lpClsScr procedure.
      llCUpDate  = .T.
      *-- Flag to know if enter the add mode before or not.
      llAddMode  = .T.
      lcCstCntCd = ALLTRIM(gcContCode) && Set default to the country code.
      STORE .F. TO llBrowse , llVatExem
      STORE " " TO lcAccName , lcStatus , ;
                   lcCstSlLnk , lcInvSlLnk , lcStySlLnk
      STORE " " TO lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,lcAddress6
      STORE {}  TO ldRaDate , ldRaVoid
      STORE 0   TO lnAuthPcs , lnAuthAmt , lnTrdeAmt
      *B802907,1 Reham On 12/26/1999   *** Begin ***
      *B802907,1 Variables hold the RA qty. & amount.
      STORE 0 TO lnRAQty , lnRAAmt 
      *B802907,1 Reham On 12/26/1999   *** End   ***
      STORE " " TO lcStyle , lcStyDesc , lcReason , lcCurLine ,;
                   lcTranCd , lcRetSty , lcStyGrade ,;
                   lcScale  , lcDyelot
      *B603339,1 [Start] Zero out lnDiscPcnt 
      *STORE 0   TO lnTotQty , lnPrice , lnAmount , lnGrsPrice , lnDiscLnPc ,;
                   lnScaleCnt , lnCost , lnDisc_Amt , lnTrde_Amt ,;
                   lnPstTotal , lnInvTrdDs 

      STORE 0   TO lnTotQty , lnPrice , lnAmount , lnGrsPrice , lnDiscLnPc ,;
                   lnScaleCnt , lnCost , lnDisc_Amt , lnTrde_Amt ,;
                   lnPstTotal , lnInvTrdDs ,lnDiscPcnt
      *B603339,1 [End]
      =gfwCodePop(@laCodInfo, "CTERMCODE" , "D")
      laData[40] = laTerms[lnTerms,2]
      
      *-- Call local function to get the trade discount.
      *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
      *E301490,1                and add new parameter to get the return value
      *E301490,1 (Start)
      *lnInvTrdDs = lfGetTrdDs(laData[40] , laData[7])
      DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH laData[40] , laData[7] , lnInvTrdDs
      *E301490,1 (End)
      
      *-- Fill the popups with the default.
      =gfwCodePop(@laCodInfo, "REASON", "D")
      laData[13] = laReson1[lnReson1,2]
      =gfwCodePop(@laCodInfo, "CDIVISION", "D")
      laData[14] = laDiv[lnDiv,2]
      *B602688,1 Reham On 03/18/99   *** Begin ***
      IF llLink_GL
        *B602688,1 If Gl is set at division level, get GL Link code from division.
        IF llDiv_Link .AND. !EMPTY(laData[14])
          DECLARE laDRltFld[2,2]
          laDRltFld[1,1] = 'LINK_CODE'
          laDRltFld[1,2] = 'lcCustLink'
          laDRltFld[2,1] = 'CSLSGLLINK'
          laDRltFld[2,2] = 'lcCstSlLnk'
          
          =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
          laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) , "DEFDEF")
          lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) , "DEF")

          *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factore.          
          IF SEEK('M'+laData[2],'CUSTOMER') AND !EMPTY(CUSTOMER.cFacCode)
            IF SEEK(CUSTOMER.cFacCode,'Factor')      
              laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
            ENDIF  
          ENDIF   
         *E500374,6 WAB - (End)
         
        ELSE
          laData[34] = "DEFDEF"
          lcCstSlLnk = "DEF"
        ENDIF
      ELSE
        laData[34] = ""
        lcCstSlLnk = ""
      ENDIF
      *B602688,1 Reham On 03/18/99   *** Begin ***
      
      =gfwCodePop(@laCodInfo1, "REASON", "N")
      lnReson2  = 1
      
      lnTaxRate = lnDfTaxRat
      laSize    = " "
      laQtyStk  = 0
      llValidDt = .T.
      
      *B602719,1 Reham On 04/06/99   *** Begin ***
      *-- Blank the temp. files.
      *=lfCrtUnComp(.T. , .T.)
      
      *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
      *E301490,1 (Start)
      *=lfCrtUnComp(.T. , .T. , .T.)
      DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .T. , .T. , .T.
      *E301490,1 (End)

      *B602719,1 Reham On 04/06/99   *** End   ***
      
      lnCrMemNo = 0
      *-- Set default if going to add mode.
      llVatExem  = .F.
      lcAccName  = LOOKUP(CUSTOMER.BtName,"M"+laData[2],CUSTOMER.Account,'CUSTOMER')
      lcStatus   = IIF(laData[6] = "V" , "Voided" , "Active")
      
      ldRaDate   = LOOKUP(RETAUTH.RaDate,laData[3],RETAUTH.RaNo,'RETAUTH')

      *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
      *E131400,1 NNA             close file again
      *laData[29] = laWareHous[1,2]
      lnOldAlias = SELECT(0)
      SELECT WAREHOUS
      LOCATE FOR ldefware
      IF FOUND()
        PUWAREHOUS = ASCAN(laWareHouses,WAREHOUS.CWARECODE)
        PUWAREHOUS = IIF(PUWAREHOUS=0,1,ASUBSCRIPT(laWareHouses,PUWAREHOUS,1))
        laData[29] = laWareHous[PUWAREHOUS,2]
      ELSE
        laData[29] = laWareHous[1,2]
      ENDIF  
      SELECT(lnOldAlias)
      *E131400,1 NNA (End)

      laData[41] = gdSysDate
      *-- Set flag to execute the browse.
      llReBrowse = .T.
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
    cbEntr_Inv = IIF(laData[15] = "Y" , .T. , .F.)
    *B122824,1 NNA 05/26/2004 (Begin) Get the array Position for the Credit memo's Location
    *B122824,1 NNA              and then get the line of the array for this Position 
    *puWareHous = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29])/2 , 0)
    lnArPos = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29]) , 0)
    puWareHous = ASUBSCRIPT(laWareHous,lnArPos,1)
    *B122824,1 NNA (End)
    *-- Assign the codes popups values.    
    = gfwCodePop(@laCodInfo, "CTERMCODE" , "V,"+laData[40])
    = gfwCodePop(@laCodInfo, "REASON"    , "V,"+laData[13])
    
    *-- Call local function to update the temp. files & needed variables 
    *-- fields in the uncompleted session file record.
    =lfUpdVars()
    *-- Disable the notes & object linking buttons in the control pannel.
    SHOW GET pbNotes  DISABLE
    SHOW GET pbLinkTo DISABLE
    
    *B602750,1 Reham On 05/25/1999   *** Begin ***
    *B602750,1 Enable the detail folder.
    SHOW GET ibFolder(2) ENABLE
    *B602750,1 Reham On 05/25/1999   *** End   ***
ENDCASE

*-- Get the right refreshing for the objects on the screen.
lcEdtStat = IIF(laScrMode[4] , "ENABLE" , "DISABLE")
lcRAStat  = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcStorStat= IIF(laScrMode[4] .AND. EMPTY(laData[3]) , "ENABLE" , "DISABLE")
lcInqStat = IIF(!EMPTY(laData[7]) .AND. !laScrMode[1] , "ENABLE" , "DISABLE")
lcInvStat = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcOrdStat = IIF(EMPTY(laData[8]) .AND. EMPTY(laData[3]) .AND. laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcWarStat = IIF(laScrMode[4] .AND. llMultiWH , "ENABLE" , "DISABLE")

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division popup if no invice & entered styles..
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) , "ENABLE" , "DISABLE")
*B605135,1 (Begin) Once we entered a RA, disable the Division without waiting getting into the 2nd folder and collecting lines.
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 AND (EMPTY(laData[3]) OR llFoldFrst), "ENABLE" , "DISABLE")
*B605135,1 (End)
*B602863,1 Reham On 05/12/1999   *** End   ***

lcCurStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') , "ENABLE" , "DISABLE")
lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")
lcEntrStat= IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcGLStat  = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. llLink_GL .AND. llDiv_Link , "ENABLE" , "DISABLE")
lcObjStat = IIF(laScrMode[4] , "ENABLE" , "DISABLE")
lcRp1Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[16]) , "ENABLE" , "DISABLE")
lcRp2Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[19]) , "ENABLE" , "DISABLE")

*B606720,1 ABD - Enable and disable charge & tax fields. [Begin]
IF llIsEnglnd
  lcChgStat = IIF(laScrMode[4],"ENABLE", "DISABLE")
  *B607104,1 ABD - Enable Tax amount field.  [Begin]
  *SHOW GET laData[26] DISABLE
  *B607104,1 ABD - [End]
  SHOW GET laData[27] DISABLE
ENDIF
*B606720,1 ABD - [End]

*-- Refresh the objects in screen : (lcCrMem0)
SHOW GET ibAccount  &lcEdtStat
SHOW GET laData[2]  &lcEdtStat
SHOW GET lcAccName  &lcEdtStat
SHOW GET ibRaNo     &lcRAStat
SHOW GET laData[3]  &lcRAStat
SHOW GET ibStore    &lcStorStat
SHOW GET laData[4]  &lcStorStat

*-- Set flag to rebrowse in the detail folder.
llReBrowse = .T.

*-- Call local function to change between the header & the detail folder
*-- If not coming from the main screen.
IF !llFromSPR
  = lfActFolder()
ENDIF

SELECT RETHDR

*!*************************************************************
*! Name      : lfRefHdr
*! Developer : Reham Al-Allamy
*! Date      : 02/16/1998
*! Purpose   : Function to refresh the objects in the header folder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfRefHdr()
*!*************************************************************
*
FUNCTION lfRefHdr
PRIVATE lnCount

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division popup if no invice & entered styles..
*B605135,1 (Begin) Once we entered a RA, disable the Division without waiting getting into the 2nd folder and collecting lines.
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 AND (EMPTY(laData[3]) OR llFoldFrst), "ENABLE" , "DISABLE")
*B605135,1 (End)
*B602863,1 Reham On 05/12/1999   *** End   ***

*-- Refresh the objects in screen : (lcCrMem1)
SHOW GET pbInqInv   &lcInqStat
SHOW GET ibInvoice  &lcInvStat
SHOW GET laData[7]  &lcInvStat
SHOW GET ibOrder    &lcOrdStat
SHOW GET laData[8]  &lcOrdStat
*B605165,1 (Begin) Disable 'Return entire invoice' when generating CR from RA.
*SHOW GET cbEntr_Inv &lcEntrStat
IF !EMPTY(laData[3])
  SHOW GET cbEntr_Inv DISABLE
ELSE
  SHOW GET cbEntr_Inv &lcEntrStat
ENDIF

*B605165,1 (End)

SHOW GET puWareHous &lcWarStat
SHOW GET lnDiv      &lcDivStat
SHOW GET ibCurCode  &lcCurStat
SHOW GET laData[31] &lcCurStat
SHOW GET laData[33] &lcExRStat
SHOW GET ibGLCode   &lcGLStat
SHOW GET laData[34] &lcGLStat
SHOW GET laData[17] &lcRp1Stat
SHOW GET laData[20] &lcRp2Stat


SHOW GET laData[41] &lcObjStat
SHOW GET laData[5]  &lcObjStat
SHOW GET ldRaDate   &lcObjStat
SHOW GET ldRaVoid   &lcObjStat
SHOW GET ibFactor   &lcObjStat
SHOW GET ibRep1     &lcObjStat
SHOW GET ibRep2     &lcObjStat
SHOW GET lnTerms    &lcObjStat
SHOW GET lnReson1   &lcObjStat
SHOW GET lnAuthPcs  &lcObjStat
SHOW GET lnAuthAmt  &lcObjStat
SHOW GET lnTaxRate  &lcObjStat

FOR lnCount = 9 TO 12
  SHOW GET laData[lnCount] &lcObjStat
ENDFOR

SHOW GET laData[16] &lcObjStat
SHOW GET laData[18] &lcObjStat
SHOW GET laData[19] &lcObjStat
SHOW GET laData[21] &lcObjStat
SHOW GET laData[22] &lcObjStat

*B604219,4 KAM 04/24/2001 calculate tax amount according terms payment[start]
*FOR lnCount = 25 TO 28
 * SHOW GET laData[lnCount] &lcObjStat
*ENDFOR
IF laScrMode[4]   && add mode only
  IF EMPTY(laData[7]) AND !EMPTY(laData[3]) && not come from invoice  &&come from return not invoic 
     lcOldArae=SELEC()
     *--calculate lninvtrdds
     DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH laData[40] , laData[7] , lnInvTrdDs
     
     SELECT(lcCrMemLin) 
     laData[26] = 0 
     SCAN
       *--calculate tax amount
       *B605261,4 (Begin)  Consider if the style is taxable of not.
       *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
       laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax AND SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable, (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
       *B605261,4 (End)
     ENDSCAN  
     
     *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
     IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
       =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
     ENDIF
     *C200517,1 KHM 04/13/2003 (End)
   
     *--here we recalculate the net amount
     laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
     SELECT(lcOldArae)
   ENDIF  &&EMPTY(laData[7])
  *:B130326,1 MHM 03/27/2006 enable notes in add mode when changing folders[Start]
  SHOW GET laData[38] ENABLE
  SHOW GET laData[39] ENABLE
  *:B130326,1 MHM 03/27/2006 [End]
   
ENDIF  && laScrMode[4]
FOR lnCount = 25 TO 28
  SHOW GET laData[lnCount] &lcObjStat
ENDFOR 
*B604219,4 KAM[end]'
*E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
laData[26] = ROUND(laData[26],2)
*E302198,1 ALB change tax rate to print 99.999 formate [END]

*B127806,1 EIH 05/22/2005 If entir invoice get different amounts of invoice from invhdr [Begin].
=lfEntInvHd()
*B127806,1 EIH 05/22/2005 [End].


*-- Refresh the say fields.
=lfRefresh()

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Function to validate the folder changing.
*!*************************************************************
*! Calls     : lfGetLines, lfBrowLin
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActFolder()
*!*************************************************************
*
FUNCTION lfActFolder

*-- If coming from screen, do not execute changing the folders.
IF llFromSPR
  llFromSPR = .F.
ELSE  
  DO CASE
    CASE lnActFolder = 1
      *-- Disable the detail window.
      SHOW GETS WINDOW (lcCrMem2B) DISABLE ONLY
      *-- Call local function to refresh the header objects.
      =lfRefHdr()
    CASE lnActFolder = 2
      *E301077,4 Reham On 12/27/98   *** Begin ***
      *E301077,4 Open consolidated header & lines files, invoice header & lines files and style dyelot file.
      =gfOpenFile(gcDataDir+'CONSINVH',gcDataDir+'CONSINVH','SH')
      =gfOpenFile(gcDataDir+'CONSINVL',gcDataDir+'CONSINVL','SH')
      =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
      =gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
      =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
      *E301077,4 Reham On 12/27/98   *** End   ***
      
      *-- In add mode only and enter the detail folder for the first 
      *-- time and there is no lines entered yet.
	  *B037264,1 NNA 01/29/2004 (Begin) Check if the invoice No. Changed or not to recollect the data
      *IF !llFoldFrst .AND. laScrMode[4] .AND. lnCrMemNo = 0

       *B608078,1 NNA 05/11/2007 (Begin) correct the next line that because within an existing Credit Memo and while in View mode, 
       *B608078,1 NNA             go to Detail folder and switch to Header folder again. You will find that the Received pieces 
       *B608078,1 NNA             and amount have become zeros 

       *IF laData[7]<>lcOldValue .or. !llFoldFrst .AND. laScrMode[4] .AND. lnCrMemNo = 0
       IF (laScrMode[4] .AND. laData[7]<>lcOldValue) .or. (!llFoldFrst .AND. laScrMode[4] .AND. lnCrMemNo = 0)
       *B608078,1 NNA (End)

	  *B037264,1 NNA (End)
        *-- Call local function to get the credit memo lines according to
        *-- the entered data in the credit memo header.
        IF lfGetLines()
          *-- Invert the flag to prevent entering the folder again.
          llFoldFrst = .T.
          *-- Call local function to update the temp. files & needed variables 
          *-- fields in the uncompleted session file record.
          =lfUpdVars()
        ENDIF
	 *B037264,1 NNA 01/29/2004 (Begin) after collecting the data lcoldvalue=The new inoice No. ;
	 									to not recollecting the data
		lcOldValue=laData[7]
	 *B037264,1 NNA (End)		
      ENDIF

      *-- Disable the header window.
      SHOW GETS WINDOW (lcCrMem1) DISABLE ONLY
      SHOW GET ibTab2b_1 ENABLE
      *-- Call the browse function.
      IF llReBrowse
        =lfBrowLin()
        llReBrowse = .F.
      ENDIF
      *-- Call the browse when function.
      =lfwBrowLin()
  ENDCASE
ENDIF

*B127806,1 EIH 05/22/2005 If entir invoice get different amounts of invoice from invhdr [Begin].
*B127806,1 EIH            llInvHdChg --> hold if we come from invoice screen. in that case
*B127806,1 EIH            we seek for invoice# .
IF llInvHdChg
  STORE .F. TO llInvHdChg 
  SELECT INVHDR
  =SEEK(laData[7])
ENDIF
=lfEntInvHd()
*B127806,1 EIH 05/22/2005 [End].


*B606720,1 ABD - Enable and disable charge & tax fields. [Begin]
IF llIsEnglnd
  lcChgStat = IIF(laScrMode[4],"ENABLE", "DISABLE")
  
  *B607195,1 KHM 06/03/2003 (Begin) Enable the charges button to allow the user to see the charages
  *SHOW GET pbCharges  &lcEdtStat
  SHOW GET pbCharges  ENABLE
  *B607195,1 KHM 06/03/2003 (End)
  
  *B607104,1 ABD - Enable Tax amount field.  [Begin]
  *SHOW GET laData[26] DISABLE
  *B607104,1 ABD - [End]
  SHOW GET laData[27] DISABLE
ENDIF
*B606720,1 ABD - [End]


SELECT RETHDR

*!*************************************************************
*! Name      : lfFilTmpHd
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1997
*! Purpose   : Function to fill the header temp. file.
*!*************************************************************
*! Calls     : lfCrtUnComp
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfFilTmpHd()
*!*************************************************************
*
FUNCTION lfFilTmpHd

*-- Update the header temp file from laData array to be reference 
*-- for any uncompleted session happene.
SELECT (lcCrMemHdr)
IF !SEEK(laData[2] , lcCrMemHdr)

  *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
  *E301490,1 (Start)
  *=lfCrtUnComp(.T. , .F.)
  DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .T. , .F.
  *E301490,1 (End)

  *C200517,1 KHM 04/30/2003 (Begin) To replace laData[36] with the related filed for
  *C200517,1                (cGLOutAcct) for tax code = "000100"
  IF ASCAN(laEvntTrig , PADR('RMGLOTAC',10)) <> 0
    =gfDoTriger('RMCRMEM',PADR('RMGLOTAC',10))
  ENDIF
  *C200517,1 KHM 04/13/2003 (End)


  SELECT (lcCrMemHdr)
  APPEND BLANK
*B037264,1 NNA 01/29/2004 (Begin) Function To make the header fields = zero if the [Return Entir invoice]=NO
ELSE
  *B037963,1 NNA 04/15/2004 (Begin) Make the Charge's Values Zero if I checked [Return Entir invoice] Before
  IF llChkRinv
  *B037963,1 NNA (End)
    =lfHdrFields()
  *B037963,1 NNA 04/15/2004 (Begin) Close If Statement
  ENDIF  
  *B037963,1 NNA (End)
*B037264,1 NNA (End)
ENDIF

*-- Lock the record to grantee the phiscal update.
= RLOCK()
GATHER FIELDS &lcScFields FROM laData
UNLOCK
llCUpDate = .T.

SELECT RETHDR

*!*************************************************************
*! Name      : lfwWhenAll
*! Developer : Reham Al-Allamy
*! Date      : 05/07/1997
*! Purpose   : When function to save the old value.
*!             (called from different objects).
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwWhenAll()
*!*************************************************************
*
FUNCTION lfwWhenAll

*-- Save the current value of the current object.
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Valid function of the credit memo object.
*!*************************************************************
*! Calls     : gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_1()
*!*************************************************************
*
FUNCTION lfvData_1

*-- Fill the credit memo with zeros on the left if not browsing.
llBrowse  = IIF((!llBrowse .AND. "?" $ laData[1]) .OR. llBrowse , .T. , .F.)
laData[1] = IIF(llBrowse .OR. EMPTY(laData[1]) , laData[1] , PADL(ALLTRIM(laData[1]) , 6 , "0"))

IF (llBrowse .OR. !EMPTY(laData[1])) .AND. LASTKEY() = 13
  SHOW GET laData[1]
  *-- Call global function to handle the action & the screen mode 
  *-- according to this action.
  =gfSeekRec()
  *-- Blank the credit memo field to get a seq. no. upon saving.
  laData[1] = IIF(laScrMode[4] , SPACE(6) , laData[1])
ENDIF
llBrowse = .F.

SELECT RETHDR

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Reham Al-Allamy
*! Date      : 05/07/1997
*! Purpose   : Valid function of the lfv object.
*!*************************************************************
*! Calls     : CUSBROWM, lfActInfo, lfRAInfo, lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_2()
*!*************************************************************
*
FUNCTION lfvData_2

IF (llBrowse .OR. !(laData[2] == lcOldValue))
  IF !SEEK ('M'+laData[2] , "CUSTOMER") .OR. llBrowse
    *-- Call the main customer browse.
    XACCOUNT  = laData[2]
    DO CUSBROWM WITH XACCOUNT
    laData[2] = XACCOUNT
  ENDIF
  *B605151,1 (Begin) Prevent non-active customers to create Crefit Memos.
  IF !EMPTY(laData[2]) .AND. ! CUSTOMER.Status $ 'AH'
    *-- Message : 46042
    *-- "Unable to create a new Credit memo, customer status might be canceled ;" 
    *--  or potential."
    *--     <  Ok  >
    =gfModalGen('TRM46038B00000','ALERT','Credit Memo')
    laData[2] = ''
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF && END (IF !EMPTY(laData[2]) .AND. CUSTOMER NOT POTENTIAL OR CANCELDED
  *B605151,1 (End)

  SHOW GET laData[2]
  *-- Call function to get all the related info. to the current customer.
  =lfActInfo()
  *B606120,5 ASH 06/20/2002 (Begin) Load Sales Rep for the selected Customer/division
  IF ASCAN(laEvntTrig , PADR('LOADRMSR',10)) <> 0
    llContino = .F.
    =gfDoTriger('RMCRMEM',PADR('LOADRMSR',10))  
    IF !llContino
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
    =lfRefresh()
  ENDIF
  *B606120,5 ASH 06/20/2002 (End)
  
  IF !(laData[2] == lcOldValue)
    laData[4] = SPACE(8)
    IF !EMPTY(laData[3])
      *-- If the customer changes, Blank the R/A no. & call its local
      *-- function to refresh 
      laData[3] = SPACE(6)
      =lfRAInfo()
    ENDIF
  ENDIF
ENDIF

*B605036,1 ABD - Let the Credit Memo change the commision if 
*B605036,1 ABD - Commisionset by division. [Begin]
IF llRep_div
  *-- Cheak for first sales rep.
  IF !EMPTY(laData[16]) .AND. SEEK(laData[16]+laData[14],'REP_DIV')
    STORE REP_DIV.Comm_Rate TO laData[17]
    *-- Valid function of the sales reps. commissions.
    = lfvData_17()
  ENDIF
    
  *-- Cheak for secand sales rep.
  IF !EMPTY(laData[19]) .AND. SEEK(laData[19]+laData[14],'REP_DIV')
    STORE REP_DIV.Comm_Rate TO laData[20]
    *-- Valid function of the sales reps. commissions.
    = lfvData_20()
  ENDIF
ENDIF
*B605036,1 ABD - [End]

*C200341,1 ABD Add new Custom trigger for customer stuncroft. [Begin]
IF ASCAN(laEvntTrig , PADR('RSLSREP1',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RSLSREP1',10))
ENDIF
  
IF ASCAN(laEvntTrig , PADR('RSLSREP2',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RSLSREP2',10))
ENDIF
*C200341,1 ABD [End]


SHOW GET lcAccName &lcEdtStat
SHOW GETS WINDOW (lcCrMem0) ONLY
lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")

*-- Refresh objects in the header folder if it was active.
IF lnActFolder = 1
  SHOW GET pbInqInv  &lcInqStat
  SHOW GET ibInvoice &lcInvStat
  SHOW GET laData[7] &lcInvStat
  SHOW GET ibOrder   &lcOrdStat
  SHOW GET laData[8] &lcOrdStat
  SHOW GET laData[33] &lcExRStat
  SHOW GETS WINDOW (lcCrMem1) ONLY
  =lfRefresh(lcCrMem1)
ENDIF

*B606152,1 (Begin) In add mode, if the customer has S/R enable its commission fileds also.
lcRp1Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[16]) , "ENABLE" , "DISABLE")
lcRp2Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[19]) , "ENABLE" , "DISABLE")
SHOW GET laData[17] &lcRp1Stat
SHOW GET laData[20] &lcRp2Stat
*B606152,1 (End)

llBrowse = .F.
SELECT RETHDR

*!*************************************************************
*! Name      : lfActInfo
*! Developer : Reham Al-Allamy
*! Date      : 05/12/1997
*! Purpose   : Function to refresh the info. related to account.
*!*************************************************************
*! Calls     : gfwCodePop, lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActInfo()
*!*************************************************************
*
FUNCTION lfActInfo

IF !EMPTY(laData[2]) .AND. SEEK ('M'+laData[2] , "CUSTOMER")
  *-- If valid account code, get its info. from the customer file.
  *wab
  *laData[41] = gdSysDate
  *laData[5]  = gdSysDate
  laData[41] = IIF(EMPTY(laData[41]),gdSysDate,laData[41])
  laData[5]  = IIF(EMPTY(laData[5]),gdSysDate,laData[5])
  =CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM',.T.)
  *wab
  laData[9]  = CUSTOMER.cFacCode
  laData[10] = 1
  *B606152,1 (Begin) Get the rep Commission at the store level if any.
  IF !EMPTY(laData[4])
    =SEEK('S'+laData[2]+laData[4],'CUSTOMER')
  ENDIF
  *B606152,1 (End)
  laData[16] = CUSTOMER.SalesRep
  laData[17] = CUSTOMER.Comm
  laData[19] = CUSTOMER.Rep2
  laData[20] = CUSTOMER.Comm2
  *B606152,1 (Begin) Retore the main account record as it may get information at the main level.
  
  *B607308,1 KHM 06/17/2003 (Begin) Commented the following code.
  *IF !EMPTY(laData[4])
  *  =SEEK("M" + laData[2] , "CUSTOMER")
  *ENDIF
  *B607308,1 KHM 06/17/2003 (End)
  
  *B606152,1 (End)
  *B602719,1 Reham On 03/30/99   *** Begin ***
  *B602719,1 Default the currency to the base currency if the customer currency is empty.
  *laData[31] = CUSTOMER.cCurrCode
  laData[31] = IIF(!EMPTY(CUSTOMER.cCurrCode) , CUSTOMER.cCurrCode , gcBaseCurr)
  *B602719,1 Reham On 03/30/99   *** End   ***
  
  *-- Get the exchange rate for this currency.
  laData[33] = IIF(llMulCurr , gfChkRate('lnCurrUnit' , laData[31] , laData[5],;
                         .T. , gcAct_Comp , .F. , llEditExRt) , 1)
  *-- Get the unit for the current currency code.
  laData[32] = IIF(llMulCurr , IIF(lnCurrUnit = 0 , 1 , lnCurrUnit) , 1)
  laData[33] = IIF(laData[33] = 0 , 1 , laData[33])
  
  *B802213,1 Reham On 05/02/99   *** Begin ***
  *B802213,1 If the customer tax rate is zero, do not use the tax rate of the company setup.
  *lnTaxRate  = IIF(!EMPTY(CUSTOMER.nTaxRate) .AND. !llIsCanada , CUSTOMER.nTaxRate , lnDfTaxRat)
  lnTaxRate  = IIF(!llIsCanada , CUSTOMER.nTaxRate , 0)
  *B802213,1 Reham On 05/02/99   *** End   ***
  
  lnPstRate  = IIF(llIsCanada , CUSTOMER.nTaxRate , 0)
  lnDfltPstR = IIF(llIsCanada , CUSTOMER.nTaxRate , 0)
  lcPstRule  = CUSTOMER.cTaxRule
  lcCstCntCd = IIF(EMPTY(CUSTOMER.cCont_Code),ALLTRIM(gcContCode),CUSTOMER.cCont_Code)
  lcAccName  = CUSTOMER.BtName
  
  *B605881,1 ABD Get the main account addresses or main store addresses not 
  *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
  *lcAddress1 = CUSTOMER.cAddress12
  *lcAddress2 = CUSTOMER.cAddress22
  *lcAddress3 = CUSTOMER.cAddress32
  *lcAddress4 = SUBSTR(CUSTOMER.cAddress42,1,10)
  *lcAddress5 = SUBSTR(CUSTOMER.cAddress52,1,10)
  *lcAddress6 = CUSTOMER.cAddress62
  lcAddress1 = CUSTOMER.cAddress1
  lcAddress2 = CUSTOMER.cAddress2
  lcAddress3 = CUSTOMER.cAddress3
  lcAddress4 = SUBSTR(CUSTOMER.cAddress4,1,10)
  lcAddress5 = SUBSTR(CUSTOMER.cAddress5,1,10)
  lcAddress6 = CUSTOMER.cAddress6
  *B605881,1 ABD [End]
  
  
  llVatExem  = CUSTOMER.lVatExem
  
  *B602688,1 Reham On 03/18/99   *** Begin ***
  *B602688,1 If Gl is set at division level, get GL Link code from division.
  *lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
  *laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
  IF llLink_GL
    IF llDiv_Link
      DECLARE laDRltFld[2,2]
      laDRltFld[1,1] = 'LINK_CODE'
      laDRltFld[1,2] = 'lcCustLink'
      laDRltFld[2,1] = 'CSLSGLLINK'
      laDRltFld[2,2] = 'lcCstSlLnk'
      
      =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
      laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) ,IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF"))
      lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
    ELSE   
      laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
      lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
    ENDIF
    *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factore.
    IF SEEK('M'+laData[2],'CUSTOMER') AND !EMPTY(CUSTOMER.cFacCode)
      IF SEEK(CUSTOMER.cFacCode,'Factor')      
        laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
      ENDIF  
    ENDIF   
    *E500374,6 WAB - (End)
  ELSE
    laData[34] = ""
    lcCstSlLnk = ""
  ENDIF
  *B602688,1 Reham On 03/18/99   *** Begin ***
  
  laData[40] = CUSTOMER.cTermCode
  = gfwCodePop(@laCodInfo, "CTERMCODE" , "V,"+laData[40])
  
  *B603666,1 Ramy calculate the taxes on the store level is the countery is england [start]
  IF llIsEnglnd .AND. !EMPTY(laData[4]) .AND. SEEK('S' + laData[2] + laData[4])
    lnTaxRate  = CUSTOMER.nTaxRate
    llVatExem  = CUSTOMER.lVatExem
  ENDIF
  *B603666,1 Ramy [end]
ELSE
  *-- If invalid account code, blank the objects related to the account code.
  laData[41] = {}
  laData[5]  = {}
  laData[9]  = SPACE(5)
  STORE "   " TO laData[16] , laData[19]
  STORE 0 TO laData[10] , laData[17] , laData[20] , lnPstRate
  *C102212,1 (Begin) Reintialize HST Tax Rate.
  lnHstRate  = 0
  *C102212,1 (End)
  lcCstCntCd = ALLTRIM(gcContCode)
  STORE " " TO lcAccName,lcAddress1,lcAddress2,lcAddress3,;
               lcAddress4,lcAddress5,lcAddress6,lcPstRule
  laData[31] = gcBaseCurr
  STORE 1 TO laData[32] , laData[33]
  
  llVatExem  = .F.

  *B602688,1 Reham On 03/18/99   *** Begin ***
  *B602688,1 If Gl is set at division level, get GL Link code from division.
  *laData[34] = "DEFDEF"
  *lcCstSlLnk = "DEF"
  IF llLink_GL
    IF llDiv_Link .AND. !EMPTY(laData[14])
      DECLARE laDRltFld[2,2]
      laDRltFld[1,1] = 'LINK_CODE'
      laDRltFld[1,2] = 'lcCustLink'
      laDRltFld[2,1] = 'CSLSGLLINK'
      laDRltFld[2,2] = 'lcCstSlLnk'
      
      =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
      laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) , "DEFDEF")
      lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,"DEF")
      *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factore.
      IF SEEK('M'+laData[2],'CUSTOMER') AND !EMPTY(CUSTOMER.cFacCode)
        IF SEEK(CUSTOMER.cFacCode,'Factor')      
          laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
        ENDIF  
      ENDIF   
      *E500374,6 WAB - (End)
    ELSE   
      laData[34] = "DEFDEF"
      lcCstSlLnk = "DEF"
    ENDIF
  ELSE
    laData[34] = ""
    lcCstSlLnk = ""
  ENDIF
  *B602688,1 Reham On 03/18/99   *** Begin ***


  lnTaxRate  = lnDfTaxRat
  lnDfltPstR = 0
  =gfwCodePop(@laCodInfo, "CTERMCODE" , "D")
  laData[40] = laTerms[lnTerms,2]
ENDIF

*B802088,1 Reham On 04/07/99  *** Begin ***
FOR lnNo = 1 TO 6
  lcNo = STR(lnNo,1)
  SHOW GET lcAddress&lcNo
ENDFOR
*B802088,1 Reham On 04/07/99  *** End   ***

*-- Call local function to get the trade discount.
*E301490,1 AHM 11/27/2000 Move Call this function from RMSave Program
*E301490,1                and add new parameter to get the return value
*E301490,1 (Start)
*lnInvTrdDs = lfGetTrdDs(laData[40] , laData[7])
DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH laData[40] , laData[7] , lnInvTrdDs
*E301490,1 (End)

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

*!*************************************************************
*! Name      : lfvData_3
*! Developer : Reham Al-Allamy
*! Date      : 05/07/1997
*! Purpose   : Valid function of the RA no. object.
*!*************************************************************
*! Calls     : RABROW, lfRAInfo, lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_3()
*!*************************************************************
*
FUNCTION lfvData_3

*B127806,1 EIH 05/22/2005 If we edit in credit memo then entire invoice = .F.  [Begin].
STORE .F. TO llEntrInv 
*B127806,1 EIH [End].


IF (llBrowse .OR. !(laData[3] == lcOldValue))
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the invoice header file.
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  *-- Add zeros to the right of the R/A no.
  laData[3] = IIF(llBrowse .OR. EMPTY(laData[3]) , laData[3] , PADL(ALLTRIM(laData[3]) , 6 , "0"))
  IF !SEEK(laData[3] , "RETAUTH") .OR. llBrowse
    xAccount  = laData[2]
    *-- Display the R/As related to the current account code.
    laData[3] = RABROW(laData[3])
  ENDIF
  *B120763,1 ASH 12/15/2003 (Begin) Don't allow to select Completed or Cancelled R/A's.
  IF RETAUTH.Status $ 'XC'
    =gfModalGen('TRM46043B00000','ALERT',IIF(RETAUTH.Status='C','Completed','Canceled'))
    laData[3]  = lcOldValue
    SHOW GET laData[3]
    _CUROBJ    = OBJNUM(laData[3])
    RETURN
  ENDIF
  *B120763,1 ASH 12/15/2003 (End)
  
  *E301090,1 Reham On 01/04/99   *** Begin ***
  *E301090,1 Do not accept any voided invoices.
  IF !EMPTY(laData[3]) AND !EMPTY(RETAUTH.Invoice)
    IF SEEK(RETAUTH.Invoice , "InvHdr") AND InvHdr.Status = 'V'
      *** The invoice related to this R/A has been voided. ***
      *** < Ok > ***
      =gfModalGen("INM46033B00000" , "Dialog")
      laData[3]  = lcOldValue
      SHOW GET laData[3]
      _CUROBJ    = OBJNUM(laData[3])
      RETURN
    ENDIF
  ENDIF
  
  *B605165,1 (Begin) If the user emptys the RA after he selected it before and removed its lines
  *B605165,1         consider the case as if he enters for the first time to collect the invoice 
  *B605165,1         lines if he selectes one and wants to return it entirely.
  llFoldFrst = IIF(laScrMode[4] .AND. lnCrMemNo = 0 AND laData[3] <> lcOldValue AND EMPTY(laData[3]), .F. , llFoldFrst)
  *B605165,1 (End)
  *E301090,1 Reham On 01/04/99   *** End   ***
  SHOW GET laData[3]
  *-- Display the R/As related info.
  
  *B607190,1 ABD -  Remark the next line and don't call this function in case ladata[3] OR "R/A" is empty
  *B607190,1 ABD -  That because this function clear the division and othre variable that change 
  *B607190,1 ABD -  The Division with Empty while is full with data at the screen. [Begin]
  *=lfRAInfo()
  IF !EMPTY(laData[3])
    =lfRAInfo()
  ENDIF
  *B607190,1 ABD -  [End]
  
  *-- If entered RA # , call function to calculate the header totals.
  IF !EMPTY(laData[3]) .AND. (EMPTY(laData[7]) .OR. laData[7] = lcRA_Inv)
    =lfCalFrmRA()
  ENDIF
ENDIF
*-- Refresh the header windows.
SHOW GETS WINDOW (lcCrMem0) ONLY

*-- Disable or enable the objects related to the R/A # & existance of lines.
*-- Refresh store objects.
lcStorStat= IIF(laScrMode[4] .AND. EMPTY(laData[3]) , "ENABLE" , "DISABLE")
SHOW GET ibStore    &lcStorStat
SHOW GET laData[4]  &lcStorStat

lcWarStat = IIF(laScrMode[4] .AND. llMultiWH , "ENABLE" , "DISABLE")
lcGLStat  = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. llLink_GL .AND. llDiv_Link , "ENABLE" , "DISABLE")

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division popup if no invice & entered styles..
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) , "ENABLE" , "DISABLE")
*B605135,1 (Begin) Once we entered a RA, disable the Division without waiting getting into the 2nd folder and collecting lines.
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 AND (EMPTY(laData[3]) OR llFoldFrst), "ENABLE" , "DISABLE")
*B605135,1 (End)
*B602863,1 Reham On 05/12/1999   *** End   ***

*-- HDM B802671,1 [Start] We should disable the return entire invoice case 
*--                       we found there is a vaild invoice
*lcEntrStat= IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
*lcEntrStat= IIF(laScrMode[4] .AND. EMPTY(laData[7]),"ENABLE" , "DISABLE")
lcEntrStat= IIF(laScrMode[4] .AND. EMPTY(laData[3]),"ENABLE" , "DISABLE")
*-- Default the Return entire invoice Check Box 
*-- to the Return authorization Real Status
cbEntr_Inv = IIF(retauth.cIntr_Inv = 'Y' , .T. , .F.)
*-- HDM B802671,1 [End]
*B605261,1 (Begin) Update this variable as in lfGetLines, if the user change the invoice entered
*B605261,1         in the RA, it must go into the case of asking him if he wants the Invoice lines
*B605261,1         or RA lines which never enterd after the bug B802736,1                   
laData[15] = IIF(cbEntr_Inv , "Y" , "N")
*B605261,1 (End)        

*B608078,1 NNA 05/11/2007 (Begin) Calculate the Charges Field that if the check box that called 
*B608078,1 NNA            [Return Entire Invoice] is checked (with R/A that assigned with invoice)
IF !USED('INVHDR')
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
ENDIF
IF cbEntr_Inv
  IF llIsEnglnd
    laData[27] = INVHDR.ncharges
    laData[26] = laData[26] + ROUND(INVHDR.Tax_Rate*(1-INVHDR.Trde_Disc/100)*INVHDR.ncharges/100,2)
  ELSE
    laData[27] = INVHDR.freight + INVHDR.cod + INVHDR.insur
  ENDIF
ENDIF
*B608078,1 NNA (End)


lcCurStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') , "ENABLE" , "DISABLE")
lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")
lcRp1Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[16]) , "ENABLE" , "DISABLE")
lcRp2Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[19]) , "ENABLE" , "DISABLE")

*-- Refresh objects in the header folder if it was active.
IF lnActFolder = 1
  SHOW GET puWareHous &lcWarStat
  SHOW GET ibGLCode   &lcGLStat
  SHOW GET laData[34] &lcGLStat
  SHOW GET lnDiv      &lcDivStat
  *B605165,1 (Begin) Disable 'Return entire invoice' when generating CR from RA.
  *SHOW GET cbEntr_Inv &lcEntrStat
  IF !EMPTY(laData[3])
    SHOW GET cbEntr_Inv DISABLE
  ELSE
    SHOW GET cbEntr_Inv &lcEntrStat
  ENDIF
  *B605165,1 (End)
    
  SHOW GET ibCurCode  &lcCurStat
  SHOW GET laData[31] &lcCurStat
  SHOW GET laData[33] &lcExRStat
  SHOW GET pbInqInv   &lcInqStat
  SHOW GET ibInvoice  &lcInvStat
  SHOW GET laData[7]  &lcInvStat
  SHOW GET ibOrder    &lcOrdStat
  SHOW GET laData[8]  &lcOrdStat
  SHOW GET laData[17] &lcRp1Stat
  SHOW GET laData[20] &lcRp2Stat
  
  SHOW GETS WINDOW (lcCrMem1) ONLY
  *-- Refresh the say objects on the header window.
  =lfRefresh(lcCrMem1)
ELSE
  *-- Call local function to change the folder & get the lines.
  =lfActFolder()
ENDIF
llBrowse = .F.

SELECT RETHDR

*!*************************************************************
*! Name      : lfRAInfo
*! Developer : Reham Al-Allamy
*! Date      : 05/12/1997
*! Purpose   : Function to refresh the info. related to R/A.
*!*************************************************************
*! Calls     : lfActInfo, gfwCodePop, lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRAInfo()
*!*************************************************************
*
FUNCTION lfRAInfo

*B127806,1 EIH 05/22/2005 entire invoice = .F.  [Begin].
STORE .F. TO llEntrInv 
*B127806,1 EIH 05/22/2005 [End].

IF !EMPTY(laData[3]) .AND. SEEK(laData[3] , "RETAUTH")
  *-- If the Ra was valid, display its related info.
  IF EMPTY(laData[2]) .OR. !(laData[2] = RETAUTH.Account)
    laData[2] = RETAUTH.Account
    =lfActInfo()
  ENDIF
  laData[4]  = IIF(EMPTY(laData[4]) .OR. !(laData[4] = RETAUTH.Store) , RETAUTH.Store , laData[4])
  *B802088,1 Reham On 04/07/99  *** Begin ***
  *B802088,1 Pick the store address in the from address if the store field 
  *B802088,1 is not empty.
  IF (!EMPTY(laData[4]) .AND. SEEK('S'+laData[2]+laData[4] , 'CUSTOMER')) .OR. ;
     (EMPTY(laData[4]) .AND.  SEEK('M'+laData[2],'CUSTOMER'))
    *B605881,1 ABD Get the main account addresses or main store addresses not 
    *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
    *lcAddress1 = CUSTOMER.cAddress12
    *lcAddress2 = CUSTOMER.cAddress22
    *lcAddress3 = CUSTOMER.cAddress32
    *lcAddress4 = SUBSTR(CUSTOMER.cAddress42,1,10)
    *lcAddress5 = SUBSTR(CUSTOMER.cAddress52,1,10)
    *lcAddress6 = CUSTOMER.cAddress62

    lcAddress1 = CUSTOMER.cAddress1
    lcAddress2 = CUSTOMER.cAddress2
    lcAddress3 = CUSTOMER.cAddress3
    lcAddress4 = SUBSTR(CUSTOMER.cAddress4,1,10)
    lcAddress5 = SUBSTR(CUSTOMER.cAddress5,1,10)
    lcAddress6 = CUSTOMER.cAddress6
    *B605881,1 ABD [End]
  ELSE
    STORE " " TO lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,lcAddress6
  ENDIF
  *B802088,1 Reham On 04/07/99  *** End   ***
  
  lcRA_Inv   = RETAUTH.Invoice
  laData[7]  = RETAUTH.Invoice
  laData[8]  = RETAUTH.Order
  laData[10] = RETAUTH.Cartons
  laData[11] = RETAUTH.CustPo
  laData[38] = RETAUTH.cRetNote1
  laData[39] = RETAUTH.cRetNote2
  
  IF SEEK(laData[3] , "RALINE")
    lnTaxRate  = RALINE.Tax_Rate
    lnPstRate  = RALINE.nPstRate
  ENDIF
  
  laData[13] = RETAUTH.Reason
  = gfwCodePop(@laCodInfo, "REASON"  , "V,"+laData[13])

  laData[14] = RETAUTH.cDivision
  =gfwCodePop(@laCodInfo, "CDIVISION", "V,"+laData[14])
  
  *B602688,1 Reham On 03/18/99   *** Begin ***
  *B602688,1 If Gl is set at division level, get GL Link code from division.
  IF llLink_GL
    IF llDiv_Link
      DECLARE laDRltFld[2,2]
      laDRltFld[1,1] = 'LINK_CODE'
      laDRltFld[1,2] = 'lcCustLink'
      laDRltFld[2,1] = 'CSLSGLLINK'
      laDRltFld[2,2] = 'lcCstSlLnk'
      
      =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
      laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) ,IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF"))
      lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
    ELSE   
      laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
      lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
    ENDIF
    *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factor.
    IF SEEK('M'+laData[2],'CUSTOMER') AND !EMPTY(CUSTOMER.cFacCode)
      IF SEEK(CUSTOMER.cFacCode,'Factor')      
        laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
      ENDIF  
    ENDIF   
    *E500374,6 WAB - (End)
  ELSE
    laData[34] = ""
    lcCstSlLnk = ""
  ENDIF
  *B602688,1 Reham On 03/18/99   *** End   ***
  
  
  laData[29] = IIF(llMultiWH , RETAUTH.cWareCode , lcDefWare)
  *B122824,1 NNA 05/26/2004 (Begin) Get the array Position for the Credit memo's Location
  *B122824,1 NNA              and then get the line of the array for this Position 
  *puWareHous = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29])/2 , 0)
  lnArPos = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29]) , 0)
  puWareHous = ASUBSCRIPT(laWareHous,lnArPos,1)
  *B122824,1 NNA (End)
  ldRaDate   = RETAUTH.RaDate
  ldRaVoid   = RETAUTH.Void
  lnAuthPcs  = RETAUTH.nreta_opn
  lnAuthAmt  = RETAUTH.nRtOpnAmt
  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Variables hold the RA qty. & amount.
  lnRAQty = RETAUTH.nreta_opn
  lnRAAmt = RETAUTH.nRtOpnAmt
  *B802907,1 Reham On 12/26/1999   *** End   ***
  IF !EMPTY(laData[7]) .AND. SEEK(laData[7] , "INVHDR")
    laData[40] = INVHDR.cTermCode
    lcInvTerms = INVHDR.cTermCode
    lnInvTrdDs = INVHDR.trde_disc
    *B602688,1 Reham On 03/21/99   *** Begin ***
    *B602688,1 Get the link code from the invoice file.
    laData[34] = IIF(llLink_GL , Invhdr.link_code , "")
    *B602688,1 Get the accounts from the invoice file.
    laData[35] = IIF(llLink_GL , Invhdr.cfrgtacnt , "")
    laData[36] = IIF(llLink_GL , Invhdr.ctaxacnt , "")

    *C200517,1 KHM 04/30/2003 (Begin) To replace laData[36] with the related filed for
    *C200517,1                (cGLOutAcct) for tax code = "000100"
    IF ASCAN(laEvntTrig , PADR('RMGLOTAC',10)) <> 0
      =gfDoTriger('RMCRMEM',PADR('RMGLOTAC',10))
    ENDIF
    *C200517,1 KHM 04/13/2003 (End)

    laData[37] = IIF(llLink_GL , Invhdr.caracnt , "")
    *B602688,1 Reham On 03/21/99   *** End   ***
    
    laData[16] = Invhdr.Rep1
    laData[17] = Invhdr.Comm1
    laData[19] = Invhdr.Rep2
    laData[20] = Invhdr.Comm2

    *C102676,1 Custom process for A.S.T. [Begin]
    IF ASCAN(laEvntTrig,PADR('GETREP3',10))<>0
      =gfDoTriger('RMCRMEM',PADR('GETREP3',10))
    ENDIF  
    *C102676,1 Custom process for A.S.T. [End]
    
  ELSE
    laData[40] = CUSTOMER.cTermCode
    lcInvTerms = ""
    lnInvTrdDs = 0
  ENDIF
  = gfwCodePop(@laCodInfo, "CTERMCODE" , "V,"+laData[40])
  
  *-- If multi currency.
  IF llMulCurr
    *-- Get the currency info from invoice header file 
    *-- if there is an invoice is available.
    IF !EMPTY(laData[7]) .AND. SEEK(laData[7] , "INVHDR")
      laData[31] = INVHDR.ccurrcode
      laData[32] = INVHDR.ncurrunit
      laData[33] = INVHDR.nexrate
    ELSE
    *-- Get the currency info from R/A header file 
    *-- if there is no invoice available.
      laData[31] = RETAUTH.cCurrCode
      laData[32] = RETAUTH.nCurrUnit
      laData[33] = RETAUTH.nExRate
    ENDIF
  ELSE
    laData[31] = gcBaseCurr
    laData[32] = 1
    laData[33] = 1
  ENDIF
ELSE
  *B602927,1 Reham On 05/25/1999   *** Begin ***
  *B602927,1 Do not blank the location popup.
  *-- If the R/A was invalid, blank the related objects.
  *STORE SPACE(6) TO lcRA_Inv , laData[7] , laData[8] , laData[13] , laData[14] , laData[29]
  STORE SPACE(6) TO lcRA_Inv , laData[7] , laData[8] , laData[13] , laData[14]
  laData[29] = laWareHous[1,2]
  *puWareHous = 0
  *B122824,1 NNA 05/26/2004 (Begin) Get the array Position for the Credit memo's Location
  *B122824,1 NNA              and then get the line of the array for this Position 
  *puWareHous = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29])/2 , 0)
  lnArPos = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29]) , 0)
  puWareHous = ASUBSCRIPT(laWareHous,lnArPos,1)
  *B122824,1 NNA (End)
  
  laData[4]  = IIF(EMPTY(laData[2]) , SPACE(8) , laData[4])
  laData[10] = 0
  laData[11] = SPACE(15)
  laData[15] = "N"
  laData[38] = SPACE(75)
  laData[39] = SPACE(75)
  
  cbEntr_Inv = .F.
  STORE {} TO ldRaDate , ldRaVoid
  STORE 0 TO lnAuthPcs , lnAuthAmt , lnTrdeAmt , lnInvTrdDs
  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Variables hold the RA qty. & amount.
  STORE 0 TO lnRAQty , lnRAAmt 
  *B802907,1 Reham On 12/26/1999   *** End   ***
  IF SEEK("M" + laData[2] , "CUSTOMER")
    *B606152,1 (Begin) Get the rep Commission at the store level if any.
    IF !EMPTY(laData[4])
      =SEEK('S'+laData[2]+laData[4],'CUSTOMER')
    ENDIF
    *B606152,1 (End)
    laData[16] = CUSTOMER.SalesRep
    laData[17] = CUSTOMER.Comm
    laData[19] = CUSTOMER.Rep2
    laData[20] = CUSTOMER.Comm2
    laData[40] = CUSTOMER.cTermCode
    *B606152,1 (Begin) Retore the main account record as it may get information at the main level.
    
    *B607308,1 KHM 06/17/2003 (Begin) Commented the following code.
    *IF !EMPTY(laData[4])
    *  =SEEK("M" + laData[2] , "CUSTOMER")
    *ENDIF
    *B607308,1 KHM 06/17/2003 (End)
    
    *B606152,1 (End)
    *B802213,1 Reham On 05/02/99   *** Begin ***
    *B802213,1 If the customer tax rate is zero, do not use the tax rate of the company setup.
    *lnTaxRate  = IIF(!EMPTY(CUSTOMER.nTaxRate) .AND. !llIsCanada , CUSTOMER.nTaxRate , lnDfTaxRat)
    lnTaxRate  = IIF(!llIsCanada , CUSTOMER.nTaxRate , 0)
    *B802213,1 Reham On 05/02/99   *** End   ***
    lnPstRate  = IIF(llIsCanada , CUSTOMER.nTaxRate , 0)
  ELSE
    lnTaxRate  = lnDfTaxRat
    lnPstRate  = 0
  ENDIF
  *C102212,1 (Begin) Get HST Tax Rate.
  lnHstRate = lnDefHst
  *C102212,1 (End)
  *B602688,1 Reham On 03/21/99   *** Begin ***
  IF llLink_GL
    *B602688,1 If Gl is set at division level, get GL Link code from division.
    IF llDiv_Link
      DECLARE laDRltFld[2,2]
      laDRltFld[1,1] = 'LINK_CODE'
      laDRltFld[1,2] = 'lcCustLink'
      laDRltFld[2,1] = 'CSLSGLLINK'
      laDRltFld[2,2] = 'lcCstSlLnk'
      
      =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
      laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) ,IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF"))
      lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
    ELSE   
      laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
      lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
    ENDIF
    *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factor.
    IF SEEK('M'+laData[2],'CUSTOMER') AND !EMPTY(CUSTOMER.cFacCode)
      IF SEEK(CUSTOMER.cFacCode,'Factor')      
        laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
      ENDIF  
    ENDIF   
    *E500374,6 WAB - (End)
  ELSE
    laData[34] = ""
    lcCstSlLnk = ""
  ENDIF
  *B602688,1 Reham On 03/21/99   *** End   ***
  
  lcInvTerms = ""
  lnInvTrdDs = 0
  laData[31] = gcBaseCurr
  laData[32] = 1
  laData[33] = 1
  *B603666,1 Ramy calculate the taxes on the store level is the countery is england [start]
  IF llIsEnglnd .AND. !EMPTY(laData[4]) .AND. SEEK('S' + laData[2] + laData[4])
    lnTaxRate  = CUSTOMER.nTaxRate
    llVatExem  = CUSTOMER.lVatExem
  ENDIF
  *B603666,1 Ramy [end]
ENDIF

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

lcInqStat = IIF(!EMPTY(laData[7]) , "ENABLE" , "DISABLE")
lcInvStat = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcOrdStat = IIF(EMPTY(laData[8]) .AND. EMPTY(laData[3]) .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcRp1Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[16]) , "ENABLE" , "DISABLE")
lcRp2Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[19]) , "ENABLE" , "DISABLE")

*!*************************************************************
*! Name      : lfvData_4
*! Developer : Reham Al-Allamy
*! Date      : 05/11/1997
*! Purpose   : Valid function of the store.
*!*************************************************************
*! Calls     : CUSBROWS, lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_4()
*!*************************************************************
*
FUNCTION lfvData_4

IF (llBrowse .OR. !(laData[4] == lcOldValue))
  IF !SEEK ('S'+laData[2]+laData[4] , "CUSTOMER") .OR. llBrowse
    xStore = laData[4]
    *-- Call the main browse hold all the available stores for the current customer.
    IF !CUSBROWS(laData[2],.T.)
      llBrowse = .F.
      laData[4] = SPACE(8)
    ELSE
      laData[4] = xStore
    ENDIF
  ENDIF
  *B605151,1 (Begin) Prevent non-active customers to create Crefit Memos.
  IF !EMPTY(laData[4]) .AND. ! CUSTOMER.Status $ 'AH'
    *-- Message : 46042
    *-- "Unable to create a new Credit memo, customer status might be canceled ;" 
    *--  or potential."
    *--     <  Ok  >
    =gfModalGen('TRM46038B00000','ALERT','Credit Memo')
    laData[4] = xStore
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF && END (IF !EMPTY(laData[2]) .AND. CUSTOMER NOT POTENTIAL OR CANCELDED
  *B605151,1 (End)

  *B606206,1 TMI (Begin) Load Sales Rep for the selected Customer/division for a store
  IF ASCAN(laEvntTrig , PADR('LOADRMSR',10)) <> 0
    llContino = .F.
    =gfDoTriger('RMCRMEM',PADR('LOADRMSR',10))  
    IF !llContino
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
    =lfRefresh()
  ENDIF
  *B606206,1 TMI (End ) Load Sales Rep for the selected Customer/division for a store
  
  *B802088,1 Reham On 04/07/99  *** Begin ***
  IF !(laData[4] == lcOldValue)
    *-- If there is valid store entered, get its address.
    IF !EMPTY(laData[4]) .OR. (EMPTY(laData[4]) .AND. SEEK("M" + laData[2] , "CUSTOMER"))

      *B605881,1 ABD Get the main account addresses or main store addresses not 
      *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
      *lcAddress1 = CUSTOMER.cAddress12
      *lcAddress2 = CUSTOMER.cAddress22
      *lcAddress3 = CUSTOMER.cAddress32
      *lcAddress4 = SUBSTR(CUSTOMER.cAddress42,1,10)
      *lcAddress5 = SUBSTR(CUSTOMER.cAddress52,1,10)
      *lcAddress6 = CUSTOMER.cAddress62
      lcAddress1 = CUSTOMER.cAddress1
      lcAddress2 = CUSTOMER.cAddress2
      lcAddress3 = CUSTOMER.cAddress3
      lcAddress4 = SUBSTR(CUSTOMER.cAddress4,1,10)
      lcAddress5 = SUBSTR(CUSTOMER.cAddress5,1,10)
      lcAddress6 = CUSTOMER.cAddress6
      *B605881,1 ABD [End]
      *B606152,1 (Begin) Get the rep Commission at the store level if any.
      IF !EMPTY(laData[4])
        =SEEK('S'+laData[2]+laData[4],'CUSTOMER')
      ENDIF
      *B606152,1 (End)
      laData[16] = CUSTOMER.SalesRep
      laData[17] = CUSTOMER.Comm
      laData[19] = CUSTOMER.Rep2
      laData[20] = CUSTOMER.Comm2
      *B606152,1 (Begin) Retore the main account record as it may get information at the main level.
      
      *B607308,1 KHM 06/17/2003 (Begin) Commented the following code.
      *IF !EMPTY(laData[4])
      *  =SEEK("M" + laData[2] , "CUSTOMER")
      *ENDIF  
      *B607308,1 KHM 06/17/2003 (End)
      
      *B606152,1 (End)
      SHOW GET laData[16]        
      SHOW GET laData[17]        
      SHOW GET laData[19]        
      SHOW GET laData[20]        
      
    ELSE
      STORE " " TO lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,lcAddress6
    ENDIF
  ENDIF
  FOR lnNo = 1 TO 6
    lcNo = STR(lnNo,1)
    SHOW GET lcAddress&lcNo
  ENDFOR
  *B802088,1 Reham On 04/07/99  *** End   ***
  SHOW GET laData[4]
ENDIF

llBrowse = .F.
*B603666,1 Ramy calculate the taxes on the store level is the countery is england [start]
IF llIsEnglnd
  *B605495,1 ABD - Fix problem that the Credit memo calculating tax CUSTOMER.lVatExem = .T. 
  *B605495,1 ABD - The Program should go to the main Account If not found a store. [Begin]
  IF EOF() .AND. EMPTY(laData[4])
    = SEEK('M' + laData[2])
  ENDIF
  *B605495,1 ABD - [End]
  lnTaxRate = CUSTOMER.nTaxRate
  llVatExem = CUSTOMER.lVatExem
ENDIF
*B603666,1 Ramy [end]
*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

SELECT RETHDR

*!*************************************************************
*! Name      : lfwData_41
*! Developer : Reham Al-Allamy
*! Date      : 12/18/1997
*! Purpose   : When function of the posting date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwData_41()
*!*************************************************************
*
FUNCTION lfwData_41

*-- Save the old posting date.
ldOldDate = laData[41]

*!*************************************************************
*! Name      : lfvData_41
*! Developer : Reham Al-Allamy
*! Date      : 12/18/1997
*! Purpose   : Valid function of the posting date.
*!*************************************************************
*! Calls     : gfModalGen, CHECKPRD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_41()
*!*************************************************************
*
FUNCTION lfvData_41

IF !(ldOldDate == laData[41]) .OR. llValidDt
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the invoice header file.
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  *-- Get the invoice date if available.
  IF !EMPTY(laData[7])
    ldInvDate = IIF(SEEK(laData[7] , "InvHdr") , InvHdr.InvDate , {})
  ELSE
    ldInvDate = {}
  ENDIF
  
  IF !EMPTY(ldInvDate) .AND. laData[41] < ldInvDate
    *** Posting date cannot be less than invoice date. ***
    *** < Ok > ***
    =gfModalGen("INM46012B00000" , "DIALOG")
    laData[41] = ldOldDate
    *wab
    IF !EMPTY(laData[41])
      =CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM',.T.)
    ENDIF
    *wab

    RETURN
  ENDIF
  *WAB  
  IF EMPTY(ldInvDate) .AND.  laData[41] < laData[5]
    =gfModalGen("INM46024B00000" , "DIALOG")
    laData[41] = laData[5]
    llcheck =CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM',.T.)
    IF !llcheck
      laData[41] = ldOldDate
      =CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM',.T.)
    ENDIF
    RETURN
  ENDIF
  *WAB
  *B602771,1 Reham On 04/11/99   *** Begin ***
  *IF !EMPTY(laData[5]) .AND. !EMPTY(laData[41]) .AND. laData[41] < laData[5]
  *  *** Transaction date cannot be greater than the posting date. ***
  *  *** < Ok > ***
  *  =gfModalGen("INM46024B00000" , "DIALOG")
  *  laData[41] = ldOldDate
  *  RETURN
  *ENDIF
  *B602771,1 Reham On 04/11/99   *** End   ***
  
  *-- Check the current period.
  IF !CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM')
    laData[41] = ldOldDate
    *wab
    IF !EMPTY(laData[41])
      =CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM',.T.)
    ENDIF
    _CUROBJ = OBJNUM(laData[41])
    *wab
    RETURN
  ENDIF
  llValidDt = .F.
ENDIF

*!*************************************************************
*! Name      : lfwData_5
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1997
*! Purpose   : When function of the credit date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwData_5()
*!*************************************************************
*
FUNCTION lfwData_5

*-- Save the old credit memo date.
ldOldDate = laData[5]

*!*************************************************************
*! Name      : lfvData_5
*! Developer : Reham Al-Allamy
*! Date      : 07/01/1997
*! Purpose   : Valid function of the credit date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_5()
*!*************************************************************
*
FUNCTION lfvData_5
PRIVATE lnCurAlis , lnCurLinRc

IF !(ldOldDate == laData[5])
  *B602771,1 Reham On 04/11/99   *** Begin ***
  *-- Check the current period.
  *IF !EMPTY(laData[5]) .AND. !EMPTY(laData[41]) .AND. laData[5] > laData[41]
  *  *** Transaction date cannot be greater than the posting date. ***
  *  *** < Ok > ***
  *  =gfModalGen("INM46024B00000" , "DIALOG")
  *  laData[5] = ldOldDate
  *  RETURN
  *ELSE
  *B602771,1 Reham On 04/11/99   *** End   ***
    lnCurAlis  = ALIAS()
    *-- Save the record pointer in the credit memo line.
    SELECT (lcCrMemLin)
    lnCurLinRc = RECNO(lcCrMemLin)
    
    *-- Update all the lines with the new transaction date.
    REPLACE ALL CrDate WITH laData[5]
    
    *-- Restore record pointer in credit memo line.
    IF lnCurLinRc > 0 .AND. lnCurLinRc <= RECCOUNT(lcCrMemLin)
      GOTO lnCurLinRc
    ENDIF
    
    *-- Restore the previous alias.
    SELECT (lnCurAlis)
  *B602771,1 Reham On 04/11/99   *** Begin ***
  *ENDIF
  *B602771,1 Reham On 04/11/99   *** End   ***
ENDIF

*!*************************************************************
*! Name      : lfvData_7
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1997
*! Purpose   : Valid function of the invoice #.
*!*************************************************************
*! Calls     : gfBrows, gfwCodePop, lfActInfo
*!           : lfFilTmpHd, lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_7()
*!*************************************************************
*
FUNCTION lfvData_7

IF (llBrowse .OR. !(laData[7] == lcOldValue))

  *B606720,1 ABD - assign true to variable to recollect data again for 
  *B606720,1 ABD - New tax screen for England. [Begin] 
  llFrstTime = .T.
  *B606720,1 ABD - [End]

  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the consolidated header & invoice header file.
  =gfOpenFile(gcDataDir+'CONSINVH',gcDataDir+'CONSINVH','SH')
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  lcInvExp = IIF(EMPTY(laData[4]) , ".T." , "((INVHDR.CONSOL='Y' AND SEEK(laData[7]+laData[4],'CONSINVH')) OR INVHDR.STORE=laData[4])")
  IF !EMPTY(laData[7]) .OR. llBrowse
    *-- Add zeros to the right of the invoice no.
    laData[7] = IIF(llBrowse .OR. EMPTY(laData[7]) , laData[7] , PADL(ALLTRIM(laData[7]) , 6 , "0"))
    SELECT INVHDR
    SET ORDER TO TAG INVHDRA
    IF SEEK(laData[2] + laData[7] , "INVHDR") .AND. &lcInvExp .AND. !llBrowse
      *E301090,1 Reham On 13/12/98   *** Begin ***
      *E301090,1 Do not accept any voided invoices.
      IF InvHdr.Status = 'V'
        *** Cannot accept voided invoices. ***
        *** < Ok > ***
        =gfModalGen("INM46032B00000" , "Dialog")
        laData[7]  = lcOldValue
      ENDIF
      *E301090,1 Reham On 13/12/98   *** End   ***
    ELSE
      *E301090,1 Reham On 13/12/98   *** Begin ***
      *E301090,1 Do not accept any voided invoices.
      *LOCATE FOR Account = laData[2] .AND. ;
                 IIF(EMPTY(laData[4]),.T.,;
                     ((CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[4],'CONSINVH')) ;
                     OR STORE=laData[4]))
      LOCATE FOR Account = laData[2] .AND. ;
                 IIF(EMPTY(laData[4]),InvHdr.Status<>'V',;
                     ((InvHdr.Status<>'V' AND CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[4],'CONSINVH')) ;
                     OR (InvHdr.Status<>'V' AND STORE=laData[4])))
      *E301090,1 Reham On 13/12/98   *** End   ***
      IF !FOUND()
        *** There is no records to display. ***
        *** <  Ok  > ***
        =gfModalGen("INM00052B00000" , "Dialog")
        laData[7]  = lcOldValue
      ELSE
        IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
          GO RECNO(0)
        ELSE
          GO TOP
        ENDIF
        
        lcSaveBrow = lcBrFields
        lcBrfields = "INVOICE :H='Invoice' , STATUS :H='S' ,"+;
                     "INVDATE :H='Date' , STORE :H='Store' ,"+;
                     "ORDER :H='Order' ,"+;
                     "lcDummy = LOOKUP(CUSTOMER.StName,'M'+laData[2],CUSTOMER.Account,'CUSTOMER'):H='Name':22 ,"+;
                     "lcTrmDesc = gfCodDes(cTermCode,'CTERMCODE'):23:R:H='Terms' ,"+;
                     "Rep1 :H='Rp1' , Rep2 :H='Rp2', TOTALCHG :H='Amount'"
        
        *E301090,1 Reham On 13/12/98   *** Begin ***
        *E301090,1 Do not accept any voided invoices.
        *lcKey      = "laData[2] FOR IIF(EMPTY(laData[4]),.T.,;
                     ((CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[4],'CONSINVH')) ;
                     OR STORE=laData[4]))"
        lcKey      = "laData[2] FOR IIF(EMPTY(laData[4]),InvHdr.Status<>'V',;
                     ((InvHdr.Status<>'V' AND CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[4],'CONSINVH')) ;
                     OR (InvHdr.Status<>'V' AND STORE=laData[4])))"
        *E301090,1 Reham On 13/12/98   *** End   ***
        
        lcOld_ttl    = lcFile_ttl
        lcFile_ttl   = "Invoices"
        DECLARE laInvoice[1]
        laInvoice[1] = laData[7]
        
        *-- Call the global browse with the invoice file.
        =gfBrows(lcKey,"invoice","laInvoice")
        
        lcBrFields   = lcSaveBrow
        lcFile_ttl   = lcOld_ttl
        
        laData[7]  = IIF(laData[7] = laInvoice[1] , lcOldValue , laInvoice[1])
      ENDIF
    ENDIF
  ENDIF
  
  *E301090,1 Reham On 13/12/98   *** Begin ***
  *E301090,1 Do not accept any voided invoices.
  lcInvocExp = IIF(EMPTY(laData[4]) , "InvHdr.Status<>'V'" , "((InvHdr.Status<>'V' AND INVHDR.CONSOL='Y' AND SEEK(laData[7]+laData[4],'CONSINVH')) OR (InvHdr.Status<>'V' AND INVHDR.STORE=laData[4]))")
  *-- If got valid invoice.
  *IF SEEK(laData[2] + laData[7] , "INVHDR") .AND. &lcInvExp
  IF SEEK(laData[2] + laData[7] , "INVHDR") .AND. &lcInvocExp
  *E301090,1 Reham On 13/12/98   *** End   ***
  
    *-- Get the invoice info.
    ldInvDate  = InvHdr.InvDate
    laData[14] = INVHDR.cDivision
    =gfwCodePop(@laCodInfo, "CDIVISION" , "V,"+laData[14])
    laData[8]  = INVHDR.order
    laData[11] = INVHDR.custpo
    lnDiscPcnt = INVHDR.discpcnt
    *hdm
    lnDisclnPcnt = INVHDR.discpcnt
    *hdm
    
    *B037963,1 NNA 04/15/2004 (Begin) Calculate the [Other] Field Only if the user Checked 
    *B037963,1 NNA            [Return Entire Invoice] , Because the Value Displayed Before I Check the Box
    *IF llIsEnglnd
    *  laData[27] = INVHDR.ncharges
    *ELSE
    *  laData[27] = INVHDR.freight + INVHDR.cod + INVHDR.insur
    *ENDIF
    *B037963,1 NNA (End)

    *C102212,1 (Begin) Add HST Tax.
    *laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
    
    *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
    IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
      =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
    ENDIF
    *C200517,1 KHM 04/13/2003 (End)

    laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
    *C102212,1 (End)
    laData[9]  = INVHDR.cFacCode
    lnInvTrdDs = INVHDR.trde_disc
    lcInvTerms = INVHDR.cTermCode
    laData[40] = INVHDR.cTermCode
    =gfwCodePop(@laCodInfo, "CTERMCODE", "V,"+laData[40])
    laData[16] = INVHDR.rep1
    laData[17] = INVHDR.comm1
    laData[19] = INVHDR.rep2
    laData[20] = INVHDR.comm2

    *C102676,1 Custom process for A.S.T. [Begin]
    IF ASCAN(laEvntTrig,PADR('GETREP3',10))<>0
      =gfDoTriger('RMCRMEM',PADR('GETREP3',10))
    ENDIF  
    *C102676,1 Custom process for A.S.T. [End]

    *B119936,1 ABD - Start calculate the GST tax field in case england and taxable and we have an invoive. [Begin]
    *lnTaxRate  = INVHDR.tax_rate
    IF llIsCanada .AND. llTax .AND. EMPTY(laData[3])
      lnTaxRate  = INVHDR.tax_rate
    ENDIF
    *B119936,1 ABD - [End]
    
    lnPstRate  = INVHDR.npstrate
    lcPstRule  = INVHDR.ctaxrule
    *C102212,1 (Begin) Get HST Tax Rate.
    lnHstRate = INVHDR.nHstrate
    *C102212,1 (End)
    *B602688,1 Reham On 03/21/99   *** Begin ***
    IF llLink_GL 
      *B602688,1 Get the accounts from the invoice file.
      laData[35] = Invhdr.cfrgtacnt
      laData[36] = Invhdr.ctaxacnt
      laData[37] = Invhdr.caracnt

      *C200517,1 KHM 04/30/2003 (Begin) To replace laData[36] with the related filed for
      *C200517,1                (cGLOutAcct) for tax code = "000100"
      IF ASCAN(laEvntTrig , PADR('RMGLOTAC',10)) <> 0
        =gfDoTriger('RMCRMEM',PADR('RMGLOTAC',10))
      ENDIF
      *C200517,1 KHM 04/13/2003 (End)
      
      IF !EMPTY(INVHDR.Link_Code) 
        laData[34] = INVHDR.Link_Code
      ELSE
        *B602688,1 If Gl is set at division level, get GL Link code from division.
        IF llDiv_Link
          DECLARE laDRltFld[2,2]
          laDRltFld[1,1] = 'LINK_CODE'
          laDRltFld[1,2] = 'lcCustLink'
          laDRltFld[2,1] = 'CSLSGLLINK'
          laDRltFld[2,2] = 'lcCstSlLnk'
          
          =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
          laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) ,IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF"))
          lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
        ELSE   
          laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
          lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
        ENDIF
        *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factor.
        IF SEEK('M'+laData[2],'CUSTOMER') AND !EMPTY(CUSTOMER.cFacCode)
          IF SEEK(CUSTOMER.cFacCode,'Factor')      
            laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
          ENDIF  
        ENDIF   
        *E500374,6 WAB - (End)
      ENDIF
    ENDIF
    *B602688,1 Reham On 03/18/99   *** End   ***
    
    IF llMulCurr
      laData[31] = INVHDR.ccurrcode
      laData[32] = INVHDR.ncurrunit
      laData[33] = INVHDR.nexrate
    ELSE
      laData[31] = gcBaseCurr
      laData[32] = 1
      laData[33] = 1
    ENDIF
    laData[29] = INVHDR.cwarecode
    *B122824,1 NNA 05/26/2004 (Begin) Get the array Position for the Credit memo's Location
    *B122824,1 NNA              and then get the line of the array for this Position 
    *puWareHous = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29])/2 , 0)
    lnArPos = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29]) , 0)
    puWareHous = ASUBSCRIPT(laWareHous,lnArPos,1)
    *B122824,1 NNA (End)
    *-- If valid inv.# & there is no ra, calculate the totals from the invoice lines.
    IF EMPTY(laData[3]) .AND. !EMPTY(laData[7]) .AND. laData[15] = "Y"
      *-- Fill the credit memo header file with the current data.
      =lfFilTmpHd()
      =lfCalFrmIn()
    ENDIF
  ELSE
    =lfActInfo()
    ldInvDate  = {}
    lcInvTerms = ""
    STORE 0 TO lnInvTrdDs , lnDiscPcnt , laData[27]
    *-- Fill the codes popups with the default.
    =gfwCodePop(@laCodInfo, "CDIVISION", "D")
    laData[14] = laDiv[lnDiv,2]

    laData[8]  = SPACE(5)
    laData[11] = SPACE(15)
    
    *lnTaxRate  = lnDfTaxRat
    IF llMulCurr
      *B602719,1 Reham On 03/30/99   *** Begin ***
      *B602719,1 Default the currency to the base currency if the customer currency is empty.
      *laData[31] = CUSTOMER.cCurrCode
      laData[31] = IIF(!EMPTY(CUSTOMER.cCurrCode) , CUSTOMER.cCurrCode , gcBaseCurr)
      *B602719,1 Reham On 03/30/99   *** End   ***
      *-- Get the exchange rate for this currency.
      laData[33] = gfChkRate('lnCurrUnit' , laData[31] , laData[5],;
                             .T. , gcAct_Comp , .F. , llEditExRt)
      laData[33] = IIF(laData[33] = 0 , 1 , laData[33])
      *-- Get the unit for the current currency code.
      laData[32] = IIF(lnCurrUnit = 0 , 1 , lnCurrUnit)
    ELSE
      laData[31] = gcBaseCurr
      laData[32] = 1
      laData[33] = 1
    ENDIF
    laData[29] = SPACE(6)
  ENDIF
  
  SELECT INVHDR
  SET ORDER TO TAG INVHDR
  
  IF !EMPTY(ldInvDate) .AND. laData[41] < ldInvDate
    *** Credit memo date cannot be less than invoice date. ***
    *** < Ok > ***
    =gfModalGen("INM46012B00000" , "DIALOG")
    llValidDt = .T.
    _CUROBJ = OBJNUM(laData[41])
  ENDIF
  
  *B119936,1 ABD - Start calculate the GST tax field in case england and taxable. [Begin]
  IF llIsCanada .AND. llTax .AND. EMPTY(laData[7]) .AND. !EMPTY(laData[3]) .AND. SEEK(laData[3] , "RETAUTH")
    =lfRAInfo()
  ENDIF
  *B119936,1 ABD - [End]
  
ENDIF

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()
*B606248,1 (Begin) Add the charges tax.
*laData[26] = IIF(llIsEnglnd , lfchgTax2() , InvHdr.Tax_Amt)
IF !EMPTY(laData[7])
  *B119936,1 ABD - Start calculate the GST tax field in case england and Taxable. [Begin]
  *laData[26] = IIF(llIsEnglnd , lfchgTax2() , IIF(cbEntr_Inv,InvHdr.Tax_Amt,laData[27]*(lnTaxRate / 100)))
  IF llIsEnglnd
    laData[26] = lfchgTax2()
  ELSE
    IF !llIsCanada .AND. !llTax
      laData[26] = IIF(cbEntr_Inv,InvHdr.Tax_Amt,laData[27]*(lnTaxRate / 100))
    ENDIF
  ENDIF
  *B119936,1 ABD - [End]
    
  *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
  laData[26] = ROUND(laData[26],2)
  *E302198,1 ALB change tax rate to print 99.999 formate [END]
ENDIF  
*B606248,1 (End)

*-- Change the status of the object enabling & disabling according 
*-- to the existance of the invoice.
lcWarStat = IIF(laScrMode[4] .AND. llMultiWH , "ENABLE" , "DISABLE")

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division popup if no invice & entered styles..
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) , "ENABLE" , "DISABLE")
*B605135,1 (Begin) Once we entered a RA, disable the Division without waiting getting into the 2nd folder and collecting lines.
*lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 AND (EMPTY(laData[3]) OR llFoldFrst), "ENABLE" , "DISABLE")
*B605135,1 (End)
*B602863,1 Reham On 05/12/1999   *** End   ***

lcCurStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') , "ENABLE" , "DISABLE")
lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")
lcGLStat  = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. llLink_GL .AND. llDiv_Link , "ENABLE" , "DISABLE")
lcInqStat = IIF(laScrMode[4] .AND. !EMPTY(laData[7]) , "ENABLE" , "DISABLE")
lcRp1Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[16]) , "ENABLE" , "DISABLE")
lcRp2Stat = IIF(laScrMode[4] .AND. !EMPTY(laData[19]) , "ENABLE" , "DISABLE")

*-- Refresh objects in the header folder if it was active.
IF lnActFolder = 1
  SHOW GETS WINDOW (lcCrMem1) ONLY
  SHOW GET puWareHous &lcWarStat
  SHOW GET lnTerms    &lcObjStat
  SHOW GET lnDiv      &lcDivStat
  SHOW GET ibCurCode  &lcCurStat
  SHOW GET laData[31] &lcCurStat
  SHOW GET laData[33] &lcExRStat
  SHOW GET ibGLCode   &lcGLStat
  SHOW GET laData[34] &lcGLStat
  SHOW GET pbInqInv   &lcInqStat
  SHOW GET laData[17] &lcRp1Stat
  SHOW GET laData[20] &lcRp2Stat
  
  *-- Refresh the say objects on the header window.
  =lfRefresh(lcCrMem1)
ENDIF

llBrowse = .F.

*B127187,1 EIH 05/22/2005 Fix another bug that when select entire invoice before select invoice# tax is wrong [Begin].
=lfEntInvHd()
*B127187,1 EIH [End].

SELECT RETHDR

*!*************************************************************
*! Name      : lfvData_8
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1997
*! Purpose   : Valid function of the order #.
*!*************************************************************
*! Calls     : gfBrows, lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_8()
*!*************************************************************
*
FUNCTION lfvData_8

IF (llBrowse .OR. !(laData[8] == lcOldValue))
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the order header file.
  =gfOpenFile(gcDataDir+'OrdHdr',gcDataDir+'OrdAcct','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  laData[8] = IIF(!llBrowse .AND. !EMPTY(laData[8]) .AND. !("?" $ laData[8]) , PADL(ALLTRIM(laData[8]) , 6 , "0") , laData[8])
  SELECT ORDHDR
  SET ORDER TO TAG ORDACCT
  IF (!EMPTY(laData[8]) .AND. !SEEK(laData[2]+"O"+laData[8] , "ORDHDR")) .OR. llBrowse
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    lcSaveBrow = lcBrFields
    lcBrFields = [Order:H="Order#",status:1:H="S",Season:H="SE",cDivision:H="DI",]+;
                 [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
                 [ACCOUNT:H="Acct",store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
                 [Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
                 [start:H="Start",Complete:H="Complete"]
    lcOld_ttl  = lcFile_ttl
    lcFile_ttl = "Orders"
    DECLARE laOrders[1]
    laOrders[1] = laData[8]
    
    *-- Call the browse with the order file.
    =gfBrows("laData[2]" , "Order" , "laOrders")
    lcBrFields   = lcSaveBrow
    lcFile_ttl   = lcOld_ttl
    laData[8]  = IIF(laData[8] = laOrders[1] , lcOldValue , laOrders[1])
  ENDIF
  SHOW GET laData[8]
ENDIF

llBrowse = .F.

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

SELECT RETHDR

*!*************************************************************
*! Name      : lfvData_9
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1997
*! Purpose   : Valid function of the factor.
*!*************************************************************
*! Calls     : FacChk, lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_9()
*!*************************************************************
*
FUNCTION lfvData_9

IF (llBrowse .OR. !(laData[9] == lcOldValue))
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the factor file.
  =gfOpenFile(gcSysHome+'SycFact',gcSysHome+'cFacCode','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***

  IF (!EMPTY(laData[9]) .AND. !SEEK(laData[9] , "SycFact")) .OR. llBrowse
    SELECT SycFact
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    lcSaveBrow = lcBrFields
    lcBrFields = [cFacCode :10:H="Factor",cFacComp :32:H="Factor Company",]+;
                 [cPhoneNo :22:H="Phone",cFacCont :32:H="Contact",]+;
                 [cCont_Code :17:H="Country Code"]
    
    lcOld_ttl  = lcFile_ttl
    lcFile_ttl = "Factors"
    DECLARE laFactors[1]
    laFactors[1] = laData[9]
    
    *-- Call the browse with the order file.
    =gfBrows(.F. , "cFacCode" , "laFactors")
    
    lcBrFields = lcSaveBrow
    lcFile_ttl = lcOld_ttl
    laData[9]  = IIF(laData[9] = laFactors[1] , lcOldValue , laFactors[1])
  ENDIF
  SHOW GET laData[9]
  *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factore.
  *--- if user change the factore & there was no invoice no or invoice not have link_code 
  *--- get the link code from factor file but  if the factore has no link code take the 
  *--- link_code from the customer     
  IF (EMPTY(laData[7]) .OR. EMPTY(INVHDR.Link_Code)) .AND. !EMPTY(laData[9]) ;
         .AND. llLink_GL .AND. SEEK(laData[9],'Factor')
    laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code," ")
  ELSE
    laData[34] = IIF(EMPTY(laData[7]) .OR. EMPTY(INVHDR.Link_Code)," ",laData[34])
  ENDIF   
  IF (EMPTY(laData[7]) .OR. EMPTY(INVHDR.Link_Code)) .AND. ; 
     (EMPTY(laData[9]) .OR. EMPTY(laData[34])) .AND.  ;
      SEEK ('M'+laData[2] , "CUSTOMER")
    IF llLink_GL
      IF llDiv_Link
        DECLARE laDRltFld[2,2]
        laDRltFld[1,1] = 'LINK_CODE'
        laDRltFld[1,2] = 'lcCustLink'
        laDRltFld[2,1] = 'CSLSGLLINK'
        laDRltFld[2,2] = 'lcCstSlLnk'
        =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
        laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) ,IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF"))
        lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
      ELSE   
        laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
        lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
      ENDIF
    ELSE
      laData[34] = ""
      lcCstSlLnk = ""
    ENDIF
  ENDIF
  SHOW GET laData[34]
  *E500374,6 WAB - (End)
ENDIF

llBrowse = .F.

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

SELECT RETHDR

*!*************************************************************
*! Name      : lfvData_15
*! Developer : Reham Al-Allamy
*! Date      : 05/19/1997
*! Purpose   : Valid function of the check box (Return entire invoice)
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_15()
*!*************************************************************
*
FUNCTION lfvData_15

laData[15] = IIF(cbEntr_Inv , "Y" , "N")
*:B131583,1 MHM 03/07/2005  [Start]
IF !EMPTY(laData[7])
  SHOW GET cbEntr_Inv DISABLE
ENDIF
*:B131583,1 MHM 03/07/2005  [End]

*B037963,1 NNA 04/15/2004 (Begin) Make The Variable True that mean the user Checked the box for the first time
llChkRinv = .T.
*B037963,1 NNA (End)

*B127806,1 EIH 05/22/2005 entire invoice = .T.  [Begin].
*B129286,2 MMR 01/03/2005 Fix bug of retaining the header amounts after saving then make new one.
*STORE .T. TO llEntrInv 
IF laData[15] ="N"
  STORE .F. TO llEntrInv 
ELSE
  STORE .T. TO llEntrInv 
ENDIF  
*B129286,2 MMR.[End]
*B127806,1 EIH   [End].


*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()


*-- If valid inv.# & there is no ra, calculate the totals from the invoice lines.
IF EMPTY(laData[3]) .AND. !EMPTY(laData[7]) .AND. laData[15] = "Y"
  =lfCalFrmIn()
ENDIF

**:B131092,1 MHM 03/07/2005 Fix bug of Invalid charge fields on new credit memo.[Start]
llFrstTime = .T.
*:B131092,1 MHM 03/07/2005 [End]


*B605261,4 (Begin) If The user uncheck 'Return Entire Invoice'
IF EMPTY(laData[3]) .AND. !EMPTY(laData[7]) .AND. laData[15] = "N"
  laData[26] = IIF(llIsEnglnd , lfchgTax() , IIF(llTax , IIF(cbEntr_Inv,InvHdr.Tax_Amt,laData[27]*(lnTaxRate / 100)) , 0))  
  *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
  laData[26] = ROUND(laData[26],2)
  *E302198,1 ALB change tax rate to print 99.999 formate [END]
  SHOW GET laData[26]
ENDIF
*B605261,4 (End)

SELECT RETHDR

*!*************************************************************
*! Name      : lfwAllNmrc
*! Developer : Reham Al-Allamy
*! Date      : 06/23/1997
*! Purpose   : When function of all the numeric fields.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwAllNmrc()
*!*************************************************************
*
FUNCTION lfwAllNmrc

lnOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvData_27
*! Developer : Reham Al-Allamy
*! Date      : 05/22/1997
*! Purpose   : Valid function of the others field.
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_27()
*!*************************************************************
*
FUNCTION lfvData_27
*-- Validate the other field.
IF !(lnOldValue == laData[27])
  *B604060,1 KAM 24/12/2000 stoping calculate tax for other valu[start]
  *laData[26] = ((laData[25]+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
  *B604060,1 KAM [end]
  *B605261,4 (Begin)  Recaculate taxes for ! England
  IF !llIsEnglnd AND llTax 
    laData[26] = ((lnDueTax+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
    *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
    laData[26] = ROUND(laData[26],2)
    *E302198,1 ALB change tax rate to print 99.999 formate [END]
    
    *B119936,1 ABD - Start calculate the GST tax field in case england and not taxable. [Begin]
   ELSE
     IF llIsCanada .AND. llTax
       laData[26] = ((laData[25]+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
       laData[26] = ROUND(laData[26],2)
     ENDIF
    *B119936,1 ABD - [End]
  ENDIF  

  *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
  IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
    =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
  ENDIF
  *C200517,1 KHM 04/13/2003 (End)

  *B605261,4 (End)
  *C102212,1 (Begin) Add HST Tax.
  *laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
  laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
  *C102212,1 (End)
ENDIF
SHOW GET laData[26]
SHOW GET laData[27]
SHOW GET laData[28]

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

*!*************************************************************
*! Name      : lfwData_29
*! Developer : Reham Al-Allamy
*! Date      : 07/15/1997
*! Purpose   : When function of the warehouse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwData_29()
*!*************************************************************
*
FUNCTION lfwData_29

*-- Save the old warehouse.
lnOldValue = puWareHous

*!*************************************************************
*! Name      : lfvData_29
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1997
*! Purpose   : Valid function of the warehouse.
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_29()
*!*************************************************************
*
FUNCTION lfvData_29

*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Open the warehouse file.
=gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
*E301077,4 Reham On 12/27/98   *** End   ***

IF lnCrMemNo > 0
  *B602959,1 Reham On 06/10/1999    *** Begin ***
  *** You have entered credit memo lines, You ***
  *** cannot change the current warehouse. ***
  *** < Ok > ***
  *=gfModalGen("INM46023B00000" , "DIALOG")
  *puWareHous = lnOldValue
  *=SEEK(laData[29],'WareHous')
  
  *B602959,1 Check if there is any styles not assigned to the current 
  *B602959,1 warehouse or not.
  lcPrvAlis = ALIAS()      && Save current alias.
  llFirst   = .T.          && flag to display the message only once.
  llAssign  = .F.          && Flag to assign all styles to changed warehouse.
  SELECT (lcCrMemLin)      && Select temp. file hold credit memo lines.
  SCAN
    *B602959,1 Check if there is any styles not assigned to the changed warehouse.
    IF !SEEK(PADR(&lcCrMemLin..Style,19) + laWareHous[puWareHous,2] + SPACE(10) , 'StyDye')
      *B602959,1 If the message was not displayed before.
      IF llFirst
        llFirst = .F.
        *** There is one or more styles not assigned to location: laWareHous[puWareHous,2]. ***
        *** <  Assign  > - < Cancel > ***
        IF gfModalGen("QRM46034B46003" , "DIALOG" , laWareHous[puWareHous,2]) = 1
          *B602959,1 Set flag to assign the unsaaigned styles to the current warehouse.
          llAssign = .T.
        ENDIF
      ENDIF
      *B602959,1 Assign the styles to the selected location.
      IF llAssign
        laData[29] = laWareHous[puWareHous,2]
        =SEEK(laData[29],'WareHous')
        DO gpAdStyWar WITH PADR(&lcCrMemLin..Style,19) , SPACE(10) , laWareHous[puWareHous,2]
      ELSE
        *B602959,1 Do not assign & restore the old location.
        puWareHous = lnOldValue
        =SEEK(laData[29],'WareHous')
        EXIT
      ENDIF
    ENDIF
    SELECT (lcCrMemLin)
  ENDSCAN
  *-- Fill the credit memo header file with the current data.
  =lfFilTmpHd()
  SELECT (lcPrvAlis)
  *B602959,1 Reham On 06/10/1999   *** End   ***
ELSE
  laData[29] = laWareHous[puWareHous,2]
  =SEEK(laData[29],'WareHous')
  
  *-- Fill the credit memo header file with the current data.
  =lfFilTmpHd()
ENDIF

SHOW GET puWareHous
SELECT RETHDR

*!*************************************************************
*! Name      : lfvRepCode
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1997
*! Purpose   : Valid function of the sales reps. codes.
*!*************************************************************
*! Calls     : REPCHK, lfFilTmpHd
*!*************************************************************
*! Parameters: Array element that hold the current rep code.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRepCode("laData[16]")
*!*************************************************************
*
FUNCTION lfvRepCode
PARAMETERS lcCurRep , lnCommNo
PRIVATE lnCommNo

IF (llBrowse .OR. !(&lcCurRep == lcOldValue))
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the salesreps file.
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  IF (!EMPTY(&lcCurRep) .AND. !SEEK(&lcCurRep , "SALESREP")) .OR. llBrowse
    XSALESREP = &lcCurRep
    *-- call the global function for the sales rep.
    DO REPCHK WITH XSALESREP
    IF EMPTY(XSALESREP)
      &lcCurRep = lcOldValue
    ELSE
      &lcCurRep = XSALESREP
    ENDIF
  ENDIF
  SHOW GET &lcCurRep
ENDIF

*-- If sales rep. code empty, blank its commession & disable it.
IF EMPTY(&lcCurRep)
  STORE 0 TO laData[lnCommNo] , laData[lnCommNo+1]
  IF lnCommNo = 17
    lcRp1Stat  = "DISABLE"
  ELSE
    lcRp2Stat  = "DISABLE"
  ENDIF
  SHOW GET laData[lnCommNo] DISABLE
  SHOW GET laData[lnCommNo+1]
ELSE
  IF lnCommNo = 17
    lcRp1Stat  = "ENABLE"
  ELSE
    lcRp2Stat  = "ENABLE"
  ENDIF
  SHOW GET laData[lnCommNo] ENABLE
  SHOW GET laData[lnCommNo+1]
  
  *C200341,1 ABD Add new Custom trigger for customer stuncroft. [Begin]
  IF ASCAN(laEvntTrig , PADR('RSLSREP1',10)) <> 0 .AND. lnCommNo = 17
    =gfDoTriger('RMCRMEM',PADR('RSLSREP1',10))
  ENDIF

  IF ASCAN(laEvntTrig , PADR('RSLSREP2',10)) <> 0 .AND. lnCommNo # 17
     =gfDoTriger('RMCRMEM',PADR('RSLSREP2',10))
    ENDIF
  *C200341,1 ABD [End]
  
ENDIF

llBrowse = .F.

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

SELECT RETHDR

*!*************************************************************
*! Name      : lfvData_17
*! Developer : Reham Al-Allamy
*! Date      : 07/23/1997
*! Purpose   : Valid function of the sales reps. commissions.
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_17
*!*************************************************************
*
FUNCTION lfvData_17

IF !EMPTY(laData[16])
  *-- Calculate the charge back if change the commission.
  IF lnInvTrdDs <> 0
    laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
  ELSE
    laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
  ENDIF
  
  SHOW GET laData[17]
  SHOW GET laData[18]
  
  *-- Fill the credit memo header file with the current data.
  =lfFilTmpHd()
  
  SELECT RETHDR
ENDIF

*!*************************************************************
*! Name      : lfvData_20
*! Developer : Reham Al-Allamy
*! Date      : 07/23/1997
*! Purpose   : Valid function of the sales reps. commissions.
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_20
*!*************************************************************
*
FUNCTION lfvData_20

IF !EMPTY(laData[19])
  *-- Calculate the charge back if change the commission.
  IF lnInvTrdDs <> 0
    laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
  ELSE
    laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
  ENDIF
  
  SHOW GET laData[20]
  SHOW GET laData[21]
  
  *-- Fill the credit memo header file with the current data.
  =lfFilTmpHd()
  
  SELECT RETHDR
ENDIF

*!*************************************************************
*! Name      : lfvData_31
*! Developer : Reham Al-Allamy
*! Date      : 05/19/1997
*! Purpose   : Valid function of the currency code.
*!*************************************************************
*! Calls     : lfFilTmpHd, gfCurrBrow, gfChkRate, gfGetExSin
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_31()
*!*************************************************************
*
FUNCTION lfvData_31

*-- Added condition to prevent user from editing currency 
*--if the this RA has lines or return by invioce.
IF (llBrowse .OR. (!(laData[31] == lcOldValue)))
  lcCurCode = laData[31]
  *-- Call the global browse for currency codes.
  IF !gfCurrBrow(@lcCurCode)
    laData[31] = lcOldValue
  ELSE
    laData[31] = lcCurCode
  ENDIF
  
  IF !EMPTY(laData[31])
    *-- Get the exchange rate for this currency.
    laData[33] = gfChkRate('lnCurrUnit' , laData[31] , laData[5],;
                         .T. , gcAct_Comp , .F. , llEditExRt)
    
    IF laData[31] = gcBaseCurr
      STORE 1 TO laData[33] , laData[32]
    ELSE
      *-- Get the unit for the current currency code.
      *B603881,1 ABD Fix bug numeric over flow while saving a credit memo. [Begin]
      *laData[32] = lnCurrUnit
      laData[32] = IIF(lnCurrUnit = 0 ,1,lnCurrUnit)
      *B603881,1 ABD [End]
      IF laData[33] = 0
        *-- If editting the exchange rate.
        IF llEditExRt
          *** A valid ALLTRIM(laData[31]) to ALLTRIM(gcBaseCurr) ***
          *** exchange rate could not be found on DTOC(gdSysDate) ***
          *** <  Ok  > ***
          lcTmpTxt = ALLTRIM(laData[31]) + "|" + ALLTRIM(gcBaseCurr) + "|" + DTOC(gdSysDate)
          =gfModalGen("INM46000B00000" , "DIALOG" , lcTmpTxt)
        ENDIF
      ENDIF
    ENDIF
    *-- Got exchange rate sign and unit sign.
    lcUntSin = ' '
    lcExRSin = gfGetExSin(@lcUntSin, laData[31])
  ENDIF
ENDIF

lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")

SHOW GET laData[31]
SHOW GET laData[33] &lcExRStat

llBrowse = .F.

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

SELECT RETHDR

*!*************************************************************
*! Name      : lfvData_33
*! Developer : Reham Al-Allamy
*! Date      : 05/19/1997
*! Purpose   : Valid function of the exchange rate.
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_33()
*!*************************************************************
*
FUNCTION lfvData_33

IF laData[33] <=0
  *** The exchange rate must be greater than zero. ***
  *** <  Ok  > ***
  =gfModalGen("INM46001B00000" , "DIALOG")
  RETURN .F.
ELSE
  RETURN .T.
ENDIF

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

SELECT RETHDR

*!*************************************************************
*! Name      : lfvData_34
*! Developer : Reham Al-Allamy
*! Date      : 05/22/1997
*! Purpose   : Valid function of the GL Link Code.
*!*************************************************************
*! Calls     : lfFilTmpHd, gfGLBrowse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_34()
*!*************************************************************
*
FUNCTION lfvData_34

IF EMPTY(laData[7]) .AND. llLink_GL .AND. llDiv_Link
  IF (llBrowse .OR. !(laData[34] == lcOldValue))
    lcTmpLink = laData[34]
    lcTmpDesc = ""
    *-- call the global browse for GL link codes.
    =gfGLBrowse('01', @lcTmpLink , @lcTmpDesc )
    laData[34] = lcTmpLink
    *E500374,6 WAB - (Start) - if user didn't choose any link_code (press ESC) get oldvalue
    laData[34] = IIF(EMPTY(laData[34]),lcOldValue,laData[34])
    *E500374,6 WAB - (Start)
  ENDIF
  SHOW GET laData[34]
  *-- Fill the credit memo header file with the current data.
  =lfFilTmpHd()
ENDIF

llBrowse = .F.

SELECT RETHDR

*!*************************************************************
*! Name      : lfvTerms
*! Developer : Reham Al-Allamy
*! Date      : 06/16/1997
*! Purpose   : Valid function of the terms code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTerms()
*!*************************************************************
*
FUNCTION lfvTerms

lcOldValue = laData[40]
laData[40] = laTerms[lnTerms,2]

IF !(laData[40] == lcOldValue)
  *-- Call local function to get the trade discount.
  *E301490,1 AHM 11/27/2000 Move Call this function from RMSave Program
  *E301490,1                and add new parameter to get the return value
  *E301490,1 (Start)
  *lnInvTrdDs = lfGetTrdDs(laData[40] , laData[7])
  DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH laData[40] , laData[7] , lnInvTrdDs
  *E301490,1 (End)
  
  *-- Calculate the sales reps. charge back.
  IF lnInvTrdDs <> 0
    laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
  ELSE
    laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
  ENDIF
  SHOW GET laData[18]
  SHOW GET laData[21]

  *B604219,1 KAM [start]
  *--here we select credit memo temp file and scan through it
  *--then we call function to calculate the tax amount
  
   IF EMPTY(laData[7])  && not come from invoice
   
     lcOldArae=SELEC()
     SELECT(lcCrMemLin) 
     laData[26] = 0 
     SCAN
       laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
     ENDSCAN  
     *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
     laData[26] = ROUND(laData[26],2)
     *E302198,1 ALB change tax rate to print 99.999 formate [END]
     
     *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
     IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
       =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
     ENDIF
     *C200517,1 KHM 04/13/2003 (End)
   
     *--here we recalculate the net amount
     laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
     SHOW GET laData[26]
     SHOW GET laData[27]
     SHOW GET laData[28]
     SELECT(lcOldArae)
   ENDIF  &&EMPTY(laData[7])
  *B604219,1 KAM[end]
ENDIF

*!*************************************************************
*! Name      : lfvReson1
*! Developer : Reham Al-Allamy
*! Date      : 05/05/1997
*! Purpose   : Valid function of the header reason popup.
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvReson1()
*!*************************************************************
*
FUNCTION lfvReson1

*-- Validate the header reason.
laData[13] = laReson1[lnReson1,2]

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

*!*************************************************************
*! Name      : lfvDiv
*! Developer : Reham Al-Allamy
*! Date      : 05/05/1997
*! Purpose   : Valid function of the division popup.
*!*************************************************************
*! Calls     : lfFilTmpHd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDiv()
*!*************************************************************
*
FUNCTION lfvDiv

*B605036,1 ABD - Valid  the sales reps. commissions. [Begin]
*-- Validate the header division.
*laData[14] = laDiv[lnDiv,2]
IF laData[14] <> SUBSTR(laDiv[lnDiv,2],1,6)

  laData[14] = SUBSTR(laDiv[lnDiv,2],1,6)
  IF llRep_div
    *-- Cheak for first sales rep.
    IF !EMPTY(laData[16]) .AND. SEEK(laData[16]+laData[14],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[17]
      *B605036,1 message text  : xxx commission decreased to xxx%      
      *B605036,1 Message Number: 40023
      *B605036,1 Button Text   : Ok    
      *B605036,1 Button number : 00000

      *B605322,1 RAE [START]
      *=gfModalGen('INM40023B00000','ALERT',laData[19]+'|'+STR(laData[20],5,2))
      *B606120,5 ASH 06/20/2002 (Begin) Don't display this message for this customer.
      IF ASCAN(laEvntTrig , PADR('RSLSREP1',10)) = 0
      *B606120,5 ASH 06/20/2002 (End)
        =gfModalGen('INM40931B00000','ALERT',laData[16]+'|'+STR(laData[17],5,2))   
      ENDIF
      STORE CUSTOMER.comm2 TO laData[20]
      SHOW GET laData[20]
      *B605322,1 RAE [END]
      
      *-- Valid function of the sales reps. commissions.
      = lfvData_17()
      
    *B605322,1 RAE [START]
    ELSE
      *B606152,1 (Begin) Get the rep Commission at the store level if any.
      IF !EMPTY(laData[4])
        =SEEK('S'+laData[2]+laData[4],'CUSTOMER')
      ENDIF
      *B606152,1 (End)
      STORE CUSTOMER.comm TO laData[17]
      *B606152,1 (Begin) Retore the main account record as it may get information at the main level.
      
      *B607308,1 KHM 06/17/2003 (Begin) Commented the following code.
      *IF !EMPTY(laData[4])
      *  =SEEK("M" + laData[2] , "CUSTOMER")
      *ENDIF  
      *B607308,1 KHM 06/17/2003 (End)
      
      *B606152,1 (End)
      *B606120,5 ASH 06/20/2002 (Begin) Don't display this message for this customer.
      IF ASCAN(laEvntTrig , PADR('RSLSREP1',10)) = 0
      *B606120,5 ASH 06/20/2002 (End)
        =gfModalGen('INM40931B00000','ALERT',laData[16]+'|'+STR(laData[17],5,2))
      ENDIF
      SHOW GET laData[17]
      = lfvData_17()      
   *B605322,1 RAE [END]
    
    ENDIF
    
    *-- Cheak for secand sales rep.
    IF !EMPTY(laData[19]) .AND. SEEK(laData[19]+laData[14],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[20]
      SHOW GET laData[20]
      *B605036,1 message text  : xxx commission decreased to xxx%      
      *B605036,1 Message Number: 40023
      *B605036,1 Button Text   : Ok    
      *B605036,1 Button number : 00000

      *B605322,1 RAE [START]
      *=gfModalGen('INM40023B00000','ALERT',laData[19]+'|'+STR(laData[20],5,2))
      *B606120,5 ASH 06/20/2002 (Begin) Don't display this message for this customer.
      IF ASCAN(laEvntTrig , PADR('RSLSREP1',10)) = 0
      *B606120,5 ASH 06/20/2002 (End)
        =gfModalGen('INM40931B00000','ALERT',laData[19]+'|'+STR(laData[20],5,2))
      ENDIF
      
      IF !SEEK(laData[16]+laData[14],'REP_DIV')
        *B606152,1 (Begin) Get the rep Commission at the store level if any.
        IF !EMPTY(laData[4])
          =SEEK('S'+laData[2]+laData[4],'CUSTOMER')
        ENDIF
        *B606152,1 (End)
        STORE CUSTOMER.comm TO laData[17]
        *B606152,1 (Begin) Retore the main account record as it may get information at the main level.
  
        *B607308,1 KHM 06/17/2003 (Begin) Commented the following code.
        *IF !EMPTY(laData[4])
        *  =SEEK("M" + laData[2] , "CUSTOMER")
        *ENDIF
        *B607308,1 KHM 06/17/2003 (End)
        
        *B606152,1 (End)
        SHOW GET laData[17]
      ENDIF
      *B605322,1 RAE [END]
      
      *-- Valid function of the sales reps. commissions.
      = lfvData_20()
    *B605322,1 RAE [START]
    ELSE
      STORE CUSTOMER.comm2 TO laData[20]
      *B606120,5 ASH 06/20/2002 (Begin) Don't display this message for this customer.
      IF ASCAN(laEvntTrig , PADR('RSLSREP1',10)) = 0
      *B606120,5 ASH 06/20/2002 (End)
        =gfModalGen('INM40931B00000','ALERT',laData[19]+'|'+STR(laData[20],5,2))      
      ENDIF
      SHOW GET laData[20]
      = lfvData_20()
   *B605322,1 RAE [END]
  
    ENDIF
  ENDIF
  *C200341,1 ABD Add new Custom trigger for customer stuncroft. [Begin]
  IF ASCAN(laEvntTrig , PADR('LOADRMSR',10)) <> 0
    =gfDoTriger('RMCRMEM',PADR('LOADRMSR',10))
  ENDIF
  *C200341,1 ABD [End]
ENDIF
*B605036,1 ABD - [End]

*E500374,6 WAB - (Start)
=SEEK('M'+laData[2],'CUSTOMER')
*E500374,6 WAB - (END)

*B602688,1 Reham On 03/18/99   *** Begin ***
IF llLink_GL
  *IF EMPTY(laData[7]) .AND. EMPTY(laData[34])
  IF EMPTY(laData[7])
    *B602688,1 If Gl is set at division level, get GL Link code from division.
    IF llDiv_Link
      DECLARE laDRltFld[2,2]
      laDRltFld[1,1] = 'LINK_CODE'
      laDRltFld[1,2] = 'lcCustLink'
      laDRltFld[2,1] = 'CSLSGLLINK'
      laDRltFld[2,2] = 'lcCstSlLnk'
      
      =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
      laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) ,IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF"))
      lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
    ELSE   
      laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
      lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
    ENDIF
    *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factor.
    *-- if customer has a factore get the factore link code but if user change the factor code
    *-- get the link code from this factore
    IF !EMPTY(CUSTOMER.cFacCode) 
      IF SEEK(IIF(laData[9] = CUSTOMER.cFacCode,CUSTOMER.cFacCode,laData[9]),'Factor')      
        laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
      ENDIF  
    ELSE
      IF !EMPTY(laData[9]) .AND. SEEK(laData[9],'Factor')
        laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
      ENDIF  
    ENDIF   
    *E500374,6 WAB - (End)
  ENDIF
  SHOW GET laData[34]
ELSE
  laData[34] = ""
  lcCstSlLnk = ""
ENDIF
*B602688,1 Reham On 03/18/99   *** Begin ***

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()

*!*************************************************************
*! Name      : lfvInQInv
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Valid function to inquire the current invoice info.
*!*************************************************************
*! Calls     : gpDoProg 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvInQInv()
*!*************************************************************
*
FUNCTION lfvInQInv

*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Open the invoice header file.
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
*E301077,4 Reham On 12/27/98   *** End   ***

*-- If the invoice exist in the invoice header file.
SELECT INVHDR

SET ORDER TO TAG INVHDR
IF SEEK(laData[7] , "INVHDR")
  *-- Clear the trapped keys.
  PUSH KEY
  ON KEY
  
  *B127806,1 EIH 05/22/2005 We come from invoice screen [Begin].
  llInvHdChg = .T.
  *B127806,1 EIH [End].
  
  *-- Call the direct invoice screen.
  lcInv = "'"+laData[7]+"'"
  DO gpDoProg WITH 'AWRARDINV' , .F. , 'AR' , lcInv
  
  *-- Store the trapped keys.
  POP KEY
ELSE
  *** Invoice # laData[7] is missing, Unable to inquire. ***
  *** <  Ok  > ***
  =gfModalGen("TRM46011B00000" , "DIALOG" , laData[7])
ENDIF






*!*************************************************************
*! Name      : lfvTaxRate
*! Developer : Reham Al-Allamy
*! Date      : 07/27/1997
*! Purpose   : Valid function for the Gst Rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTaxRate()
*!*************************************************************
*
FUNCTION lfvTaxRate

IF !(lnTaxRate == lnOldValue)
  IF lnCrMemNo > 0
    *-- If change the tax rate & there is lines for the current R/A.
    SELECT (lcCrMemLin)
    *-- Save the record pointer.
    lnLinRec = RECNO(lcCrMemLin)
    GO TOP
    *-- Update the R/A lines with the Gst rate & calculate the tax amount.
    SCAN
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE Tax_Rate WITH lnTaxRate
      UNLOCK
    ENDSCAN
    *-- Calculate the tax amount if the company is "England" & the RA 
    *-- related to the invoice, otherwise the program does not Calculate
    *-- the tax amount.
    laData[26] = 0

    *B605261,4 (Begin)  Calculate tax on taxable amount only.
    *laData[26] = ((laData[25]+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
    
    *B119936,1 ABD - Start calculate the GST tax field in case england and not taxable. [Begin]    
    *laData[26] = ((lnDueTax+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
    IF llIsCanada .AND. llTax
      laData[26] = ((laData[25]+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
    ELSE
      laData[26] = ((lnDueTax+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
    ENDIF
    *B119936,1 ABD - [End]
    
    *B605261,4 (End)
    *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
    laData[26] = ROUND(laData[26],2)
    *E302198,1 ALB change tax rate to print 99.999 formate [END]
    
    *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
    IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
      =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
    ENDIF
    *C200517,1 KHM 04/13/2003 (End)
    
    *-- Calculate the total credit memo.
    *C102212,1 (Begin) Add HST Tax.
    *laData[28]   = laData[27] + laData[25] + laData[26] + laData[30]
    laData[28]    = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
    *C102212,1 (End)
    *-- Restore the record pointer in the temp. lines.
    IF lnLinRec > 0 .AND. lnLinRec <= RECCOUNT(lcCrMemLin)
      GOTO lnLinRec
    ENDIF
    SHOW GET laData[26]
    SHOW GET laData[28]
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvCrNotes
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Valid function to display the current record notes.
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCrNotes()
*!*************************************************************
*
Function lfvCrNotes

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Open the note pad file.
=gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
*E301077,4 Reham On 12/27/98   *** End   ***

*-- Call the notepad program to add notes
*-- for the current credit memo.
=NOTEPAD('R',laData[1])

*E301077,4 Reham On 12/27/98   *** Begin ***
=gfCloseFile('NotePad')
*E301077,4 Reham On 12/27/98   *** End   ***

*-- Restore the trapped keys.
POP KEY

*!*************************************************************
*! Name      : lfvLinkTo
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Valid function of the link to button.
*!*************************************************************
*! Calls     : GetObj 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvLinkTo()
*!*************************************************************
*
Function lfvLinkTo
PRIVATE lnAlias

*-- Save the current alias.
lnAlias = SELECT()

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*-- Call the object linking program.
DO GetObj WITH 'D',laData[1]

*-- Restore the trapped keys.
POP KEY

*-- Restore the old alias.
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfActBrow
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Activate the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActBrow()
*!*************************************************************
*
FUNCTION lfActBrow

*-- Clear browse as you are coming out of a browse window
IF glFromBrow
  =gfStopBrow()
  *-- Set the browse flag to false.
  glFromBrow = .F.
ENDIF

*-- If the screen browse is not active then clear the trapped keys.
IF !INLIST(WONTOP(), lcBrowTitl)
  ON KEY LABEL Alt+N
  ON KEY LABEL Alt+V
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
ENDIF

*!*************************************************************
*! Name      : lfDeactBrw
*! Developer : Reham Al-Allamy
*! Date      : 05/25/1997
*! Purpose   : Function executed upon entering the browse.
*!*************************************************************
*! Calls     : lpTab , lpShTab , lpTrapKey
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDeactBrw()
*!*************************************************************
*
FUNCTION lfDeactBrw

*-- Set the global flag "glFromBrow" to true only the screen browse is active.
glFromBrow = INLIST(WONTOP(), lcBrowTitl)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Alt+N      DO lpTrapKey WITH lcCrMem2B , "pbNewLine" , .T.
  ON KEY LABEL Alt+V      DO lpTrapKey WITH lcCrMem2B , "pbRemLine" , .T.
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShTab
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
*
PROCEDURE lpTab

*-- Clear the TAB trap.
ON KEY LABEL TAB
DO CASE
  *-- In select & view modes, activate push button <TOP> in the toolbar.
  CASE laScrMode[1] .OR. laScrMode[2]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(pbTop)
  *-- In edit & add mode, activate push button <NEW> in the detail folder.
  CASE laScrMode[3] .OR. laScrMode[4]
    ACTIVATE WINDOW (lcCrMem2B)
    _CUROBJ = OBJNUM(pbNewLine)
ENDCASE
*-- Trap the TAB key.
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
*! Purpose   : Trap of backtab key.
*!*************************************************************
*! Calls     : lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShTab
*!*************************************************************
*
PROCEDURE lpShTab

*-- Clear the backtab key.
ON KEY LABEL BACKTAB
DO CASE
  *-- In case of select mode, activate the R/A# object in the main screen.
  CASE laScrMode[1]
    ACTIVATE WINDOW (lcCrMem0)
    _CUROBJ = OBJNUM(laData[1])
  *-- In view, activate the <CLOSE> push button in the toolbar.
  CASE laScrMode[2]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(PBCLS)
  *-- In edit or add mode, activate the store field.
  CASE laScrMode[3] .OR. laScrMode[4]
    ACTIVATE WINDOW (lcCrMem0)
    _CUROBJ = OBJNUM(laData[41])
ENDCASE
*-- Trap the BACKTAB key.
ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Trap of keys
*!*************************************************************
*! Passed Parameters  :  lcWindName : Window name to activate
*!                       lcObjName  : object to focus.
*!                       llExecEntr : If enter will be executed or not.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTrapKey WITH 'gwcContrl1','pbCls'
*!*************************************************************
*
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName , llExecEntr

IF laScrMode[4]
  ACTIVATE WINDOW (lcWindNAme)
  _CUROBJ = OBJNUM(&lcObjName)
  *-- If enter was send to the trap, send CHR(13) to the buffer.
  IF llExecEntr
    KEYBOARD CHR(13) CLEAR
  ENDIF
ELSE
  ACTIVATE WINDOW gwcContrl1
  IF UPPER(lcObjName) = "PBNEWLINE"
    _CUROBJ = OBJNUM(PBTOP)
  ELSE
    _CUROBJ = OBJNUM(PBCLS)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfBrowLin
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Function to browse the credit memo lines.
*!*************************************************************
*! Calls     : lfwBrowLin, lfvBrows, lfwBrowLin
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowLin()
*!*************************************************************
*
FUNCTION lfBrowLin

llReBrowse = .F.

*-- Prepare the alias & the key & the title according to the mode.
lcRCurAlis = IIF(laScrMode[1] .OR. laScrMode[4] , lcCrMemLin , 'RETLINE')

lcKeyRet   = IIF(laScrMode[4] , laData[2] , laData[1])

SELECT (lcRCurAlis)
lnMarker = RECNO()

*-- Release the old browse window.
RELEASE WINDOW (lcBrowTitl)

*B802414,1 Reham On 07/19/1999  *** Begin ***
*B802414,1 Display only the received qty. in the details browse not the canceled qty.
*-- Browse the right lines.
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,;
       Style  :H='Style' :28 :R,;
       lcDummy2 = LOOKUP(Style.Desc,&lcRCurAlis..Style,Style.Style,'Style') :H='Description':26:R,;
       TotQty :H='Quantity' :8 :R,;
       Price  :H='Price' :P='999999.99' :R,;
       lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',;
       lcResDesc = gfCodDes(REASON , 'REASON'):20:R:H='Reason';
       WINDOW (lcCrMem2A)  ;
       IN WINDOW (lcCrMem2);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcKeyRet      ;
       WHEN lfwBrowLin() ;
       VALID :F lfvBrows() ;
       TITLE lcBrowTitl
*B603713,7 KAM 10/11/2000 change the price and amount fields [start]
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,;
       Style  :H='Style' :28 :R,;
       lcDummy2 = LOOKUP(Style.Desc,&lcRCurAlis..Style,Style.Style,'Style') :H='Description':26:R,;
       TotQty :H='Quantity' :8 :R,;
       Price  :H='Price' :P='999999.99' :R,;
       lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='9999999.99',;
       lcResDesc = gfCodDes(REASON , 'REASON'):20:R:H='Reason';
       WINDOW (lcCrMem2A)  ;
       IN WINDOW (lcCrMem2);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcKeyRet      ;
       FOR cRet_TrnCd = '2' ;
       WHEN lfwBrowLin() ;
       VALID :F lfvBrows() ;
       TITLE lcBrowTitl

*B127806,1 EIH 05/22/2005 Fix bug when we browse lines of credit memo [Begin].
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,;
       Style  :H='Style' :28 :R,;
       lcDummy2 = LOOKUP(Style.Desc,&lcRCurAlis..Style,Style.Style,'Style') :H='Description':26:R,;
       TotQty :H='Quantity' :8 :R,;
       Price  :H='Price' :P='999999999.99' :R,;
       lnAmount=PRICE*TOTQTY :13:H='Amount':R:P='9999999999.99',;
       lcResDesc = gfCodDes(REASON , 'REASON'):20:R:H='Reason';
       WINDOW (lcCrMem2A)  ;
       IN WINDOW (lcCrMem2);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcKeyRet      ;
       FOR cRet_TrnCd = '2' ;
       WHEN lfwBrowLin() ;
       VALID :F lfvBrows() ;
       TITLE lcBrowTitl


*:B131583,1 MHM 03/07/2005  [Start]
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,;
       Style  :H='Style' :28 :R,;
       lcDummy2 = LOOKUP(Style.Desc,&lcRCurAlis..Style,Style.Style,'Style') :H='Description':26:R,;
       TotQty :H='Quantity' :8 :R,;
       Price  :H='Price' :P='999999999.99' :R,;
       lnAmount=ROUND(gros_PRICE * (1 - disc_pcnt /100 )*TOTQTY,2) :13:H='Amount':R:P='9999999999.99',;
       lcResDesc = gfCodDes(REASON , 'REASON'):20:R:H='Reason';
       WINDOW (lcCrMem2A)  ;
       IN WINDOW (lcCrMem2);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcKeyRet      ;
       FOR cRet_TrnCd = '2' ;
       WHEN lfwBrowLin() ;
       VALID :F lfvBrows() ;
       TITLE lcBrowTitl
       
*B607994,1 TMI [Start] do not recalculate the amount in the browse
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,;
       Style  :H='Style' :28 :R,;
       lcDummy2 = LOOKUP(Style.Desc,&lcRCurAlis..Style,Style.Style,'Style') :H='Description':26:R,;
       TotQty :H='Quantity' :8 :R,;
       Price  :H='Price' :P='999999999.99' :R,;
       lnAmount=(TOTQTY * ROUND((gros_PRICE * (1-(disc_pcnt /100))),2)) :13:H='Amount':R:P='9999999999.99',;
       lcResDesc = gfCodDes(REASON , 'REASON'):20:R:H='Reason';
       WINDOW (lcCrMem2A)  ;
       IN WINDOW (lcCrMem2);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcKeyRet      ;
       FOR cRet_TrnCd = '2' ;
       WHEN lfwBrowLin() ;
       VALID :F lfvBrows() ;
       TITLE lcBrowTitl
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,;
       Style  :H='Style' :28 :R,;
       lcDummy2 = LOOKUP(Style.Desc,&lcRCurAlis..Style,Style.Style,'Style') :H='Description':26:R,;
       TotQty :H='Quantity' :8 :R,;
       Price  :H='Price' :P='999999999.99' :R,;
       lnAmount = AMOUNT :13:H='Amount':R:P='9999999999.99',;
       lcResDesc = gfCodDes(REASON , 'REASON'):20:R:H='Reason';
       WINDOW (lcCrMem2A)  ;
       IN WINDOW (lcCrMem2);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcKeyRet      ;
       FOR cRet_TrnCd = '2' ;
       WHEN lfwBrowLin() ;
       VALID :F lfvBrows() ;
       TITLE lcBrowTitl
*B607994,1 TMI [End  ] 

*:B131583,1 MHM 03/07/2005  [End]

*B127806,1 EIH 05/22/2005 [End].  



*B603713,7 KAM [end]

*B802414,1 Reham On 07/19/1999  *** End   ***

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Reham Al-Allamy
*! Date      : 04/27/1997
*! Purpose   : Valid function for the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBrowse()
*!*************************************************************
*
FUNCTION lfvBrows

*-- Valid function executed inside the browse. to stop the browse if the 
*-- current window not the browse window.
IF !WONTOP(lcBrowTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfwBrowLin
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Function called from the browse when function.
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrowLin()
*!*************************************************************
*
FUNCTION lfwBrowLin

lcRCurAlis = IIF(laScrMode[1] .OR. laScrMode[4] , lcCrMemLin , 'RETLINE')

SELECT (lcRCurAlis)
*-- Change the record pointer.
lnMarker   = RECNO()

IF BOF() .OR. EOF()
  lcLinStat = "DISABLE"
  lcRetStat = "DISABLE"
  lcDyeStat = "DISABLE"
  
  lcStyle    = ""
  lcStyDesc  = ""
  lcCurLine  = ""
  lcTranCd   = ""
  lcRetSty   = ""
  lcStyGrade = ""
  puGrdPop   = 1
  lcDyelot   = ""
  lcReason   = ""
  laSize     = " "
  laQtyStk   = 0
  *-- Get the right scale.
  lcScale  = ""
  *-- Get the scale count.
  lnScaleCnt = 0
  
  STORE 0 TO lnPrice , lnGrsPrice , lnDiscLnPc , lnAmount , ;
             lnPstRate , lnCost , lnDisc_Amt , lnTrde_Amt , lnTotQty
  *C102212,1 (Begin) Intialize HST Tax Rate.
  lnHstRate  = 0
  *C102212,1 (End)
  *-- Refresh the scale sizes & qty.
  FOR lnCount = 1 TO 8
    SHOW GET laSize  [lnCount] DISABLE
    SHOW GET laQtyStk[lnCount] DISABLE
  ENDFOR
  
  = gfwCodePop(@laCodInfo1, "REASON", "N")
ELSE
  *-- Refresh the objects in screen : (lcCrMem2B)
  lcLinStat = IIF(laScrMode[4] , "ENABLE" , "DISABLE")
  lcRetStat = IIF(laScrMode[4] .AND. llRet2Dam , "ENABLE" , "DISABLE")
  lcDyeStat = IIF(llUseDyes .AND. SEEK(PADR(lcStyle,19) , "STYLE") .AND. ;
                  STYLE.cdye_flg = 'Y' , "ENABLE" , "DISABLE")

  *-- Get the right info for the current line.
  lcStyle    = Style
  lcStyDesc  = LOOKUP(Style.Desc,&lcRCurAlis..Style,Style.Style,'Style')
  lcCurLine  = cRet_LinNo
  lcTranCd   = cRet_TrnCd
  lcRetSty   = cRetSty
  lcStyGrade = cStyGrade
  puGrdPop   = VAL(lcStyGrade)
  lcDyelot   = Dyelot
  lnPrice    = Price
  lnGrsPrice = gros_price
  lnDiscLnPc = disc_pcnt
  lnAmount   = Amount
  lnPstRate  = nPstRate
  *C102212,1 (Begin) Intialize HST Tax Rate.
  lnHstRate  = nHstRate
  *C102212,1 (End)
  lnCost     = cost
  lnDisc_Amt = DISC_AMT
  lnTrde_Amt = TRDE_AMT 
  lnTotQty   = TotQty
  lcReason   = Reason

  *-- Get the right scale.
  lcScale  = IIF(!EMPTY(lcRetSty) , IIF(SEEK(PADR(lcRetSty,19) , "STYLE") , STYLE.Scale , "") , ;
                 IIF(!EMPTY(lcStyle) .AND. SEEK(PADR(lcStyle,19) , "STYLE") , Style.Scale , ""))
  
  *-- Get the scale count.
  lnScaleCnt = IIF(!EMPTY(lcScale) .AND. SEEK("S" + lcScale , "SCALE") , SCALE.Cnt , 0)
  
  *-- Get the sizes & the qty.
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    IF lnCount <= lnScaleCnt
      laQtyStk[lnCount] = Qty&lcCount
      laSize[lnCount]   = IIF(lnScaleCnt = 0 , "" , SCALE.SZ&lcCount)
      SHOW GET laSize[lnCount]   &lcLinStat
      SHOW GET laQtyStk[lnCount] &lcLinStat
    ELSE
      laQtyStk[lnCount] = 0
      laSize[lnCount]   = ""
      SHOW GET laSize[lnCount]   DISABLE
      SHOW GET laQtyStk[lnCount] DISABLE
    ENDIF
  ENDFOR
  *-- Fill the reason popup with its data.
  =gfwCodePop(@laCodInfo1, "REASON"    , "V,"+lcReason)
ENDIF

*-- Set the new button status.
lcNewStat = IIF(laScrMode[4] , "ENABLE" , "DISABLE")

*-- Refresh the objects in screen : (lcCrMem2B)
SHOW GET ibStyle    &lcLinStat
SHOW GET lcStyle    &lcLinStat
SHOW GET lcStyDesc  &lcLinStat
SHOW GET puGrdPop   &lcLinStat
SHOW GET ibRetSty   &lcRetStat
SHOW GET lcRetSty   &lcRetStat
SHOW GET lnReson2   &lcLinStat
SHOW GET lnTotQty   &lcLinStat
SHOW GET lnPrice    &lcLinStat
SHOW GET lnAmount   &lcLinStat
SHOW GET lcDyelot   &lcDyeStat
SHOW GET lnGrsPrice &lcLinStat
SHOW GET lnDiscLnPc &lcLinStat
SHOW GET lnPstRate  &lcLinStat
*C102212,1 (Begin) Intialize HST Tax Rate.
SHOW GET lnHstRate  &lcLinStat
*C102212,1 (End)

SHOW GET pbRemLine  &lcLinStat
SHOW GET pbNewLine  &lcNewStat

*-- Refresh the browse window.
SHOW WINDOW (lcBrowTitl) REFRESH

*!*************************************************************
*! Name      : lfvNewLine
*! Developer : Reham Al-Allamy
*! Date      : 05/25/1997
*! Purpose   : Valid function of the < New > push button.
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNewLine()
*!*************************************************************
*
FUNCTION lfvNewLine

llCUpdate = .T.
llAddLine = .T.

*B127806,1 EIH 05/22/2005 entire invoice = .F.  [Begin].
STORE .F. TO llEntrInv 
*B127806,1 EIH [End].

*-- Initialze the needed variables & put defaults for the current added line.
STORE " " TO lcStyle , lcStyDesc , lcReason , lcCurLine ,;
             lcTranCd , lcRetSty , lcStyGrade ,;
             lcScale  , lcDyelot
STORE 0   TO lnTotQty , lnPrice , lnAmount , lnGrsPrice , lnDiscLnPc ,;
             lnDfTaxRat , lnScaleCnt , lnCost , lnDisc_Amt , ;
             lnTrde_Amt
DECLARE laSize[8] , laQtyStk[8]
laSize     = " "
laQtyStk   = 0
puGrdPop   = 1
lcReason   = laData[13]
=gfwCodePop(@laCodInfo1, "REASON", "V,"+lcReason)

lcLinStat  = "ENABLE"
lcRetStat  = "DISABLE"
lcDyeStat  = "DISABLE"

*-- Refresh the line's object.
SHOW GET ibStyle    &lcLinStat
SHOW GET lcStyle    &lcLinStat

*B607289,1 KHM 06/16/2003 (Begin) Commented out.
*SHOW GET lcStyDesc  &lcLinStat
*SHOW GET puGrdPop   &lcLinStat
*B607289,1 KHM 06/16/2003 (End)

SHOW GET ibRetSty   &lcRetStat
SHOW GET lcRetSty   &lcRetStat

*B607289,1 KHM 06/16/2003 (Begin) Commented out.
*SHOW GET lnReson2   &lcLinStat
*SHOW GET lnTotQty   &lcLinStat
*SHOW GET lnPrice    &lcLinStat
*SHOW GET lnAmount   &lcLinStat
*B607289,1 KHM 06/16/2003 (End)

SHOW GET lcDyelot   &lcDyeStat

*B607289,1 KHM 06/16/2003 (Begin) Commented out.
*SHOW GET lnGrsPrice &lcLinStat
*SHOW GET lnDiscLnPc &lcLinStat
*B607289,1 KHM 06/16/2003 (End)

FOR lnCount = 1 TO 8
  SHOW GET laSize[lnCount]   DISABLE
  SHOW GET laQtyStk[lnCount] DISABLE
ENDFOR

_CUROBJ = OBJNUM(lcStyle)

*!*************************************************************
*! Name      : lfvRemLine
*! Developer : Reham Al-Allamy
*! Date      : 05/25/1997
*! Purpose   : Valid function of the < Remove > push button.
*!*************************************************************
*! Calls     : lfvRemove
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRemLine()
*!*************************************************************
*
FUNCTION lfvRemLine

*-- Confirm Removing of the record.
*** Are you sure you want to "Remove" this record? ***
*** < Yes > - < No > ***
*B603741,1 WAB - to display the message correct add text "remove" 
*B603741,1 WAB - cause the message displayed "are you sure you want to this record"
*B603741,1 WAB - StART
*IF gfModalGen("QRM00002B00006","ALERT") = 1
IF gfModalGen("QRM00002B00006","ALERT","remove") = 1
*B603741,1 WAB - END
  =lfvRemove()
  
  *B127806,1 EIH 05/22/2005 entire invoice = .F.  [Begin].
  STORE .F. TO llEntrInv 
  *B127806,1 EIH [End].
  
  
ENDIF

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Reham Al-Allamy
*! Date      : 01/11/1998
*! Purpose   : Function to remove line without confirming.
*!*************************************************************
*! Calls     : lfwBrowLin
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRemove()
*!*************************************************************
*
FUNCTION lfvRemove

llAddLine = .F.
llCUpDate = .T.
*-- Subtract the current line quantities, Amount, tax, discount, 
*-- pst total & adjust the total credit amount.
laData[22] = laData[22] - &lcCrMemLin..TotQty
laData[23] = laData[23] - &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
laData[24] = laData[24] - &lcCrMemLin..DISC_AMT
laData[25] = laData[25] - &lcCrMemLin..Amount

*B037963,1 NNA 04/15/2004 (Begin) Remove the [Other] Field Value If the invoice's lines Removed
IF laData[22] + laData[23] + laData[24] + laData[25] = 0
  laData[27] = 0
ENDIF
*B037963,1 NNA (End)

*B605261,4 (Begin)  Calculate taxable amount
llTaxable = SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable
lnDueTax = lnDueTax - IIF(llTaxable,&lcCrMemLin..Amount,0)
*B605261,4 (End)
*laData[26] = laData[26] - ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A"  AND !llIsEnglnd , laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/100))
*B605261,4 (Begin)  Consider if the style is taxable of not.
*laData[26] = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
IF !llIsEnglnd AND llTax AND SEEK(&lcCrMemLin..Style,'Style') AND Style.lTaxable
  laData[26] = MAX(laData[26] - IIF(llTaxable,(&lcCrMemLin..Amount* &lcCrMemLin..TAX_RATE/ 100),0),0)
ELSE
  *B119936,1 ABD - Get the the GST tax in case canada and use tax. [Begin]
  *laData[26] = MAX(laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) ,0),0)
  IF llIsCanada .AND. llTax
    laData[26] = MAX(laData[26] - (&lcCrMemLin..Amount* &lcCrMemLin..TAX_RATE/ 100),0)
  ELSE    
    laData[26] = MAX(laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) ,0),0)
  ENDIF
  *B119936,1 ABD - [End]
ENDIF  
*B605261,4 (End)
*E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
laData[26] = ROUND(laData[26],2)
*E302198,1 ALB change tax rate to print 99.999 formate [END]

laData[30] = IIF(llIsCanada , laData[30] - &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)

*C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
ENDIF
*C200517,1 KHM 04/13/2003 (End)


*C102212,1 (Begin) Calculate HST Tax Rate.
*laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
laData[42] = IIF(llIsCanada , laData[42] - &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
*C102212,1 (End)

lnTrdeAmt  = lnTrdeAmt  - &lcCrMemLin..TRDE_AMT

*-- Calculate the charge back because the total amount is changed.
IF lnInvTrdDs <> 0
  laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
  laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
ELSE
  laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
  laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
ENDIF

*B802907,1 Reham On 12/27/1999    *** Begin ***
IF SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
  lnAuthPcs = Max(lnRAQty - laData[22] , 0)
  lnAuthAmt = Max(lnRAAmt - laData[23] , 0)
ENDIF
SHOW GET lnAuthPcs
SHOW GET lnAuthAmt

*B802907,1 Reham On 12/27/1999    *** End   ***

*-- Decrease the lines with 1.
lnCrMemNo  = lnCrMemNo - 1

*-- Delete the current line.
SELECT (lcCrMemLin)
DELETE
GO TOP
*B037264,1 NNA 01/29/2004 (Begin) Recalculate The Tax Amount After deleting the record not before
*B130434,1 MMR 12/20/2005 Fix bug of showing wrong tax amount if u choose entire invoice then
*B130434,1 MMR            add line to the invoice and remove it.[Start]
*IF !llIsEnglnd AND llTax AND SEEK(&lcCrMemLin..Style,'Style') AND Style.lTaxable
*  laData[26] = MAX(laData[26] - IIF(llTaxable,(&lcCrMemLin..Amount* &lcCrMemLin..TAX_RATE/ 100),0),0)
*ELSE
*  IF llIsCanada .AND. llTax
*    laData[26] = MAX(laData[26] - (&lcCrMemLin..Amount* &lcCrMemLin..TAX_RATE/ 100),0)
*  ELSE    
*    laData[26] = MAX(laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) ,0),0)
*  ENDIF
*ENDIF
*B130434,1 MMR.[End]
*B037264,1 NNA (End)
*-- Refresh the header info.
SHOW GET laData[22]
SHOW GET laData[23]
SHOW GET laData[24]
SHOW GET laData[25]
SHOW GET laData[26]
SHOW GET laData[28]
SHOW GET laData[30]

*B037963,1 NNA 04/15/2004 (Begin) Show The [Other] Field Value After the Removing
SHOW GET laData[27]
*B037963,1 NNA (End)

*C102212,1 (Begin) Show get HST Tax amount.
SHOW GET laData[42]  &lcLinStat
*C102212,1 (End)
*-- Get the right refreshing for the objects on the screen.
lcRAStat  = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
SHOW GET ibRaNo     &lcRAStat
SHOW GET laData[3]  &lcRAStat

lcInvStat = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcOrdStat = IIF(EMPTY(laData[8]) .AND. EMPTY(laData[3]) .AND. laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
lcEntrStat= IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")

*-- Set flag to false if there is no lines.
*llFoldFrst = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , .F. , llFoldFrst)

lcinvsllnk = ""

*-- Refresh the line's object screen.
=lfwBrowLin()

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid func. for the style object.
*!*************************************************************
*! Calls     : lfwBrowLin, gfStyBrw, gfwCodePop, gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyle()
*!*************************************************************
*
FUNCTION lfvStyle

*-- If the style value did not change or empty value & the browse icon was 
*-- not pressed , do not validate the style field.
IF (lcStyle = lcOldValue .OR. EMPTY(lcStyle)) .AND. !llBrowse
  RETURN
ENDIF

*B127806,1 EIH 05/22/2005 If we edit in credit memo then entire invoice = .F.  [Begin].
STORE .F. TO llEntrInv 
*B127806,1 EIH [End].

SELECT STYLE

IF llBrowse .OR. !SEEK(PADR(lcStyle,19) , 'STYLE')
  *B602771,1 Reham On 04/11/99   *** Begin ***
  *B602771,1 Enable the soft seek in the style browse.
  *-- Call the style global browse.
  *lcStyle = gfStyBrw("I" , "" , "" , .F.)
  lcStyle = PADR(gfStyBrw("I" , lcStyle , "" , .F.) , 19)
  *B602771,1 Reham On 04/11/99   *** End   ***
  IF !WEXIST(lcBrowTitl)
    =lfwBrowLin()
  ENDIF
ENDIF
IF !EMPTY(lcStyle)

  IF SEEK(PADR(lcStyle,19) , "STYLE")
    *-- Get the discount code for the selected style.
    lcDiscCode = Style.cDiscCode
    IF STYLE.cDivision <> laData[14]
      *** Style / Division confilct! ***
      *** <  Ok  > ***
      =gfModalGen("INM46003B00000" , "DIALOG")
      lcStyle = lcOldValue
      _CUROBJ = OBJNUM(lcStyle)
      RETURN
    ENDIF
    
    lcStyGrade = STYLE.cStyGrade
    puGrdPop   = VAL(lcStyGrade)
    
    *-- Get the scale & scale count.
    lcScale    = Style.Scale
    IF SEEK("S" + lcScale , "SCALE")
      lnScaleCnt = SCALE.Cnt
    ELSE
      *** Scale (lcScale) has been deleted.  Cannot proceed! ***
      *** <  Ok  > ***
      =gfModalGen("INM46006B00000" , "DIALOG" , lcScale)
      lcStyle = lcOldValue
      _CUROBJ = OBJNUM(lcStyle)
      RETURN
    ENDIF
    *B603820,1 (Start) Get the total cost if the cost method was standered.
    *lnCost    = STYLE.AVE_COST
    lnCost= IIF(llStdCost,Style.TotCost,Style.Ave_Cost)
    *B603820,1 (End)
    SELECT StyDye
    IF !SEEK(PADR(lcStyle,19) + laData[29] + SPACE(10) , 'StyDye')
      *** Style: ALLTRIM(lcStyle) ***
      *** is not assigned to warehouse: ALLTRIM(laData[29])  ***
      *** <  Add  > - < Reenter > ***
      lcTmpTxt = ALLTRIM(lcStyle) + "|" + ALLTRIM(laData[29])
      IF gfModalGen("QRM46004B00031" , "DIALOG" , lcTmpTxt) = 1
        DO gpAdStyWar WITH PADR(lcStyle,19) , SPACE(10) , laData[29]
      ELSE
        lcStyle = lcOldValue
        _CUROBJ = OBJNUM(lcStyle)
        RETURN
      ENDIF
    ENDIF
    lnCost  = StyDye.Ave_Cost   
   
    *B602688,1 Reham On 03/21/99   *** Begin ***
    *-- Initialize cost and cost link code from style file
    IF llLink_GL
      lcGlCost = IIF(!EMPTY(Style.Link_Code),Style.Link_Code,'DEFDEF')
      IF llMultiWH
        *-- Get cost and cost link code for this warehouse.
        lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
      ENDIF
    ENDIF
    *B602688,1 Reham On 03/21/99   *** End   ***
    
    lcPriceLvl = IIF(EMPTY(CUSTOMER.PRICELVL) , "A" , CUSTOMER.PRICELVL)
    *lnLstPrice = STYLE.PRICE&lcPriceLvl
  
    *-- Called gfStyPrice to default the style price in case of
    *-- insert mode other wise got default value from raline.
    *IF llMulCurr 
    *  lnTmpPrice = gfStyPrice(PADR(lcStyle,19) , lcPriceLvl , laData[31])
    *  lnLstPrice = IIF(lnTmpPrice = -1 , 0 , lnTmpPrice)
    *ENDIF

    *E301142,4 Reham On 03/08/99     *** Begin ***
    *E301142,4 Change the way of using style price level if there is price level Q 
    *E301142,4 or the current price level is zero.
    *lnLstPrice = IIF(!llMulCurr OR laData[31] = gcBaseCurr , ;
                     Style.Price&lcPriceLvl , ;
                     gfStyPrice(PADR(lcStyle,19),lcPriceLvl,laData[31]) )
    *lnLstPrice = IIF(lnLstPrice < 0 , 0 , lnLstPrice )
    
    *B602932,1 Reham On 05/26/1999   *** Begin ***
    *B602932,1 Move this line after checking if this style was shipped to the customer or not.
    *lnLstPrice = lfGetprice(lcStyle,lcPriceLvl,lnTotQty)
    *E301142,4 Reham On 03/08/99     *** End   ***
    *B602932,1 Reham On 05/26/1999   *** End   ***
    
    llRetry    = .F.
    llShipped  = .F.
    lcDyelot   = SPACE(10)
    
    *B602688,1 Reham On 03/21/99    *** Begin ***
    STORE "" TO lcCOGSAcnt , lcICAcnt , lcSalesAcnt , lcDiscAcnt
    *B602688,1 Reham On 03/21/99    *** End   ***
    
    IF !EMPTY(laData[7])
      *-- Get the tax rate from the invoice header file.
      =SEEK(laData[7] , "INVHDR")
      SELECT INVLINE
      SET ORDER TO INVLINES
      IF SEEK(PADR(lcStyle,19) + laData[7] , "INVLINE")
        
        lnLstPrice  = INVLINE.PRICE
        *B604763,1 MHM 08/16/2001  Get discount from InvHdr[start]
        lnDiscLnPc  = INVHDR.DISCPCNT
        *B604763,1 MHM  [End]
        lnDiscPcnt  = INVHDR.DISCPCNT
        llShipped   = .T.
        lcDyelot    = INVLINE.DYELOT
        
        *-- Calculate the tax in the case of england.
        IF llIsEnglnd
          *-- This tax will be for the current line.
          lnTaxRate = lfMechDisc(PADR(lcStyle,19))
        ENDIF
        lnPstRate  = INVHDR.nPstRate
        *C102212,1 (Begin) Intialize HST Tax Rate.
        lnHstRate  = INVHDR.nHstRate
        *C102212,1 (End)
        lnInvTrdDs = INVHDR.trde_disc
        lnCost     = INVLINE.COST
        IF llLink_GL
          lcGLCost = IIF(!EMPTY(INVLINE.GL_COST) , INVLINE.GL_COST , lcGLCost)
        ENDIF
        
        *B602688,1 Reham On 03/21/99    *** Begin ***
        lcInvSlLnk  = InvLine.GL_Sales
        lcCOGSAcnt  = Invline.ccogsacnt
        lcICAcnt    = Invline.cicacnt
        *B603054,1 Reham On 07/07/1999   *** Begin ***
        *B603054,1 This account should hold the return merchandise 
        *B603054,1 account instead of the sales revenue.
        *lcSalesAcnt = Invline.csalesacnt
        *B603054,1 Reham On 07/07/1999   *** End   ***
        lcDiscAcnt  = Invline.cdiscacnt
        *B602688,1 Reham On 03/21/99    *** End   ***
      ELSE
        llRetry   = .T.
        *-- Get the lnCost variable.
        *B603820,1 (Start) Get the total cost if the cost method was standered.
        *lnCost = IIF(SEEK(PADR(lcStyle,19) + laData[29] + SPACE(10) , 'StyDye') , ;
                     StyDye.Ave_Cost , Style.Ave_Cost)      
          
        IF SEEK(PADR(lcStyle,19) + laData[29] + SPACE(10) , 'StyDye') 
          lnCost=IIF(llStdCost,Style.TotCost,StyDye.Ave_Cost)
        ELSE
          lnCost=IIF(llStdCost,Style.TotCost,Style.Ave_Cost)
        ENDIF  
        *B603820,1 (End)
        lcInvSlLnk = ""
      ENDIF
      SELECT INVLINE
      SET ORDER TO INVLINE
    ELSE
      llRetry   = .T.
      *-- Get the lnCost variable.
      
      *B603820,1 (Start) Get the total cost if the cost method was standered.
      *lnCost    = IIF(SEEK(PADR(lcStyle,19) + laData[29] + SPACE(10) , "StyDye") , ;
                      StyDye.Ave_Cost , Style.Ave_Cost)      
      IF SEEK(PADR(lcStyle,19) + laData[29] + SPACE(10) , 'StyDye') 
        lnCost=IIF(llStdCost,Style.TotCost,StyDye.Ave_Cost)
      ELSE
        lnCost=IIF(llStdCost,Style.TotCost,Style.Ave_Cost)
      ENDIF  
      *B603820,1 (End) 
      lcInvSlLnk = ""
    ENDIF

    *-- Search for the style in the invoice header file if there is no invoice #.
    
    IF llRetry
      SELECT INVLINE
      SET ORDER TO INVLINES
      SELECT INVHDR
      *-- Make invhdra the active index tag
      SET ORDER TO TAG INVHDRA DESCENDING
      =SEEK(laData[2] , "INVHDR")
      *-- Searching for the last invoice this style was shipped on using
      *-- the same current currency.
      
      *B602863,1 Reham On 05/24/1999  *** Begin ***
      *B602863,1 Change the scan condition to find the related styles.
      *E301090,1 Reham On 13/12/98   *** Begin ***
      *SCAN WHILE ACCOUNT = laData[2]
      *SCAN WHILE ACCOUNT = laData[2] AND InvHdr.Status <> 'V'
      *E301090,1 Reham On 13/12/98   *** End   ***
      SCAN REST WHILE ACCOUNT = laData[2] FOR InvHdr.Status <> 'V'
      *B602863,1 Reham On 05/24/1999   *** End   ***
        *B602719,1 Reham On 03/30/99   *** Begin ***
        *B602719,1 Check the currency field only if the system is multi currency.
        *IF INVHDR.cCurrCode = laData[31]
        IF (llMulCurr .AND. (INVHDR.cCurrCode = laData[31])) .OR. !llMulCurr
        *B602719,1 Reham On 03/30/99   *** End   ***
          lcSrchInv = INVHDR.INVOICE
          SELECT INVLINE
          IF SEEK(PADR(lcStyle,19) + lcSrchInv , "INVLINE")
            lnLstPrice = PRICE
            
            *B603339,1 [Start] Discount percent should take the value of 
            *                  merchndise discount of the last invoice
            *lnDiscPcnt = INVHDR.DISCPCNT
            lnDiscLnPc = INVHDR.DISCPCNT
            *B603339,1 [End]
            
            llShipped  = .T.
            *B603967,1 (Begin) As no invoice intered, don't overrite the term code.
            *laData[40] = INVHDR.cTermCode
            *=gfwCodePop(@laCodInfo, "CTERMCODE", "V,"+laData[40])
            *B603967,1 (End)
                        
            lcDyelot   = DYELOT
            
            *-- Calculate the tax in the case of england.
            IF llIsEnglnd
              *** This tax will be for the current line.
              lnTaxRate = lfMechDisc(PADR(lcStyle,19))
            ENDIF
            lnPstRate  = INVHDR.nPstRate
            *C102212,1 (Begin) Intialize HST Tax Rate.
            lnHstRate  = INVHDR.nHstRate
            *C102212,1 (End)
            *B603967,1 (Begin) Trade disc comes from Term code when no invoice entered.
            *lnInvTrdDs = INVHDR.trde_disc
            *B603967,1 (End)
            
            *-- Get the lnCost variable all the time
            *-- regardless the GL link condition.
            lnCost     = INVLINE.COST
            
            *B602688,1 Reham On 06/13/1999   *** Begin ***
            *IF llLink_GL
            *  lcGLCost = IIF(!EMPTY(INVLINE.GL_COST) , INVLINE.GL_COST , lcGLCost)
            *ENDIF
            *B602688,1 Reham On 06/13/1999   *** End   ***
            
            *B602863,1 Reham On 05/25/1999   *** Begin ***
            *B602863,1 Exit for the scan to take the latest invoice price.
            EXIT
            *B602863,1 Reham On 05/25/1999   *** End   ***
          ENDIF
        ENDIF
        SELECT INVHDR
      ENDSCAN
      SELECT INVLINE
      SET ORDER TO INVLINE
      SELECT INVHDR
      SET ORDER TO TAG INVHDR
    ENDIF
    
    IF !llShipped
      *** This style was not shipped to this customer! continue? ***
      *** <  Yes  > - <  No  > ***
      IF gfModalGen("QRM46005B00006" , "DIALOG") = 2
        lcStyle = lcOldValue
        _CUROBJ = OBJNUM(lcStyle)
        RETURN
      ENDIF
      lnPstRate = lnDfltPstR
      *C102212,1 (Begin) Intialize HST Tax Rate.
      lnHstRate  = lnDefHst
      *C102212,1 (End)
      *B602932,1 Reham On 05/26/1999   *** Begin ***
      *B602932,1 Move this line after checking if this style was shipped to the customer or not.
      lnLstPrice = lfGetprice(lcStyle,lcPriceLvl,lnTotQty)
      *B602932,1 Reham On 05/26/1999   *** End   ***
      
      *-- If the discount code for the selected style not empty.
      IF !EMPTY(lcDiscCode)
        *-- Get the discount percentage for this discount code from the codes file.
        DECLARE laStyDisc[1,2]
        laStyDisc[1,1] = 'DISCPCNT'
        laStyDisc[1,2] = 'lnDiscPcnt'
        =gfRltFld(lcDiscCode , @laStyDisc , "CDISCCODE")
        *-- If the discount percentage greater than zero...
        IF lnDiscPcnt > 0
          *B602863,1 Reham On 05/24/1999   *** Begin ***
          *lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnLstPrice)) + ;
                     "|" + ALLTRIM(STR(lnLstPrice * lnDiscPcnt /100)) + "|" + ;
                     ALLTRIM(STR(lnLstPrice - (lnLstPrice * lnDiscPcnt / 100)))
          lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnLstPrice,10,3)) + ;
                     "|" + ALLTRIM(STR((lnLstPrice * lnDiscPcnt /100),10,3)) + "|" + ;
                     ALLTRIM(STR(lnLstPrice - (lnLstPrice * lnDiscPcnt / 100),10,3))
          *B602863,1 Reham On 05/24/1999   *** End   ***
          
          *** There is discount percentage on this style : {lnDiscPcnt} ***
          *** Gross Price is     : {lnLstPrice}
          *** Discount amount is : {lnLstPrice * lnDiscPcnt /100}
          *** Net Price is       : {lnLstPrice - (lnLstPrice * lnDiscPcnt / 100)}
          =gfModalGen("INM46030B00000" , "DIALOG" , lcTmpStr)
          
          *-- Get a discount on the total gross price, & inform the user.
          lnLstPrice = lnLstPrice - (lnLstPrice * lnDiscPcnt / 100)
        ENDIF
      ENDIF
      
      *-- Calculate the tax in the case of england.
      IF llIsEnglnd
        *** This tax will be for the current line.
        =SEEK('M' + laData[2] , "Customer")
        lnTaxRate  = lfMechDisc(PADR(lcStyle,19))
        laData[40] = IIF(EMPTY(laData[40]) , Customer.cTermCode , laData[40])
        =gfwCodePop(@laCodInfo, "CTERMCODE", "V,"+laData[40])
        
        *-- Call local function to get the trade discount.
        *E301490,1 AHM 11/27/2000 Move Call this function from RMSave Program
        *E301490,1                and add new parameter to get the return value
        *E301490,1 (Start)
        *lnInvTrdDs = lfGetTrdDs(laData[40] , laData[7])
        DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH laData[40] , laData[7] , lnInvTrdDs
        *E301490,1 (End)
      ELSE
        laData[40] = IIF(EMPTY(laData[40]) , IIF(SEEK(laData[2] , "CUSTOMER") , CUSTOMER.cTermCode , SPACE(6)) , laData[40])
        = gfwCodePop(@laCodInfo, "CTERMCODE", "V,"+laData[40])
        lnInvTrdDs = 0
      ENDIF
      lnDiscPcnt= 0

      *-- Get the lnCost variable.
      *B603820,1 (Start) Get the total cost if the cost method was standered.
      *lnCost    = IIF(SEEK(PADR(lcStyle,19) + laData[29] + SPACE(10) , "StyDye") , ;
                      StyDye.Ave_Cost , Style.Ave_Cost)
     
      IF SEEK(PADR(lcStyle,19) + laData[29] + SPACE(10) , 'StyDye') 
        lnCost=IIF(llStdCost,Style.TotCost,StyDye.Ave_Cost)
      ELSE
        lnCost=IIF(llStdCost,Style.TotCost,Style.Ave_Cost)
      ENDIF  
      *B603820,1 (End) 
    
    ENDIF
    
    *B121842,1 NNA 03/01/2004 (Begin) Seek if the Operator has privilege to see the "style cost" or not
    =IIF(gcUser_lvl='O',IIF(gfUserPriv('IC','ICSTYLE','COSTING'),lnCost = lfGetCost(),''),lnCost = lfGetCost())
    *B121842,1 NNA (End) 

    *-- Add the current line in the credit memo temp. file.
    SELECT (lcCrMemLin)
    IF llAddLine
      *-- If there was no lines & this is the first line to be added,
      *-- Refresh the objects related to the line no. in the header folder.
      IF lnCrMemNo = 0
        *-- Increment the lines counter with the first line.
        lnCrMemNo  = 1
        *-- Get the right refreshing for the objects on the screen.
        lcRAStat  = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
        SHOW GET ibRaNo     &lcRAStat
        SHOW GET laData[3]  &lcRAStat
        
        lcInvStat = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
        lcOrdStat = IIF(EMPTY(laData[8]) .AND. EMPTY(laData[3]) .AND. laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
        lcEntrStat= IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
      ELSE
        *-- increment the lines counter with one.
        lnCrMemNo  = lnCrMemNo + 1
      ENDIF
      
      *B802907,1 Reham On 12/26/1999   *** Begin ***
      *B802907,1 Variable to hold the counter line no.
      lnCrLinCnt = lnCrLinCnt + 1
      *B802907,1 Reham On 12/26/1999   *** End   ***
      
      llAddLine = .F.
      *-- Find an empty record to reuse it instead of adding new line.
      LOCATE FOR EMPTY(STYLE)
      IF !FOUND()
        APPEND BLANK
      ENDIF
      BLANK
    ENDIF
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B603339,1 [Start] Disc_Pcnt Shouldn't be 0
    *REPLACE STYLE      WITH PADR(lcStyle,19) ;
            CRET_TRNCD WITH "2" ;
            CSTYGRADE  WITH lcStyGrade ;
            ACCOUNT    WITH laData[2] ;
            INVOICE    WITH laData[7] ;
            CRDATE     WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
            DYELOT     WITH lcDyelot ;
            REASON     WITH lcReason ;
            GROS_PRICE WITH lnLstPrice ;
            DISC_PCNT  WITH 0          ;
            PRICE      WITH GROS_PRICE * (1 - DISC_PCNT/100) ;
            TAX_RATE   WITH lnTaxRate ;
            nPstRate   WITH lnPstRate ;
            COST       WITH lnCost

    REPLACE STYLE      WITH PADR(lcStyle,19) ;
            CRET_TRNCD WITH "2" ;
            CSTYGRADE  WITH lcStyGrade ;
            ACCOUNT    WITH laData[2] ;
            INVOICE    WITH laData[7] ;
            CRDATE     WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
            DYELOT     WITH lcDyelot ;
            REASON     WITH lcReason ;
            GROS_PRICE WITH lnLstPrice ;
            DISC_PCNT  WITH lnDiscLnPC ;
            PRICE      WITH GROS_PRICE * (1 - DISC_PCNT/100) ;
            TAX_RATE   WITH lnTaxRate ;
            nPstRate   WITH lnPstRate ;
            COST       WITH lnCost
    *C102212,1 (Begin) Update HST RATE.
    REPLACE nHstRate   WITH lnHstRate
    *C102212,1 (End)
    *B603339,1 [End]
    *B602932,1 Reham On 05/26/1999   *** Begin ***
    *B602932,1 Add true in the logical field to know that this line was 
    *B602932,1 shipped to the customer befor.
    REPLACE lShipped  WITH llShipped
    *B602932,1 Reham On 05/26/1999   *** End   ***
    
    *B802907,1 Reham On 12/26/1999   *** Begin ***
    REPLACE cRet_LinNo WITH ALLTRIM(STR(lnCrLinCnt))
    IF SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
      REPLACE nOpnQty1   WITH RALINE.nOpnQty1 ;
              nOpnQty2   WITH RALINE.nOpnQty2 ;
              nOpnQty3   WITH RALINE.nOpnQty3 ;
              nOpnQty4   WITH RALINE.nOpnQty4 ;
              nOpnQty5   WITH RALINE.nOpnQty5 ;
              nOpnQty6   WITH RALINE.nOpnQty6 ;
              nOpnQty7   WITH RALINE.nOpnQty7 ;
              nOpnQty8   WITH RALINE.nOpnQty8 ;
              nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
    ENDIF
    *B802907,1 Reham On 12/26/1999   *** End   ***
    
    IF llLink_GL
      lcStySlLnk = Style.cslsgllink
      *B602688,1 Reham On 03/21/99   *** Begin ***
      IF EMPTY(lcInvSlLnk)
        *-- Get the customer link & the style link.
        =SEEK("M" + laData[2] , "CUSTOMER")
        IF llDiv_Link
          DECLARE laDRltFld[2,2]
          laDRltFld[1,1] = 'LINK_CODE'
          laDRltFld[1,2] = 'lcCustLink'
          laDRltFld[2,1] = 'CSLSGLLINK'
          laDRltFld[2,2] = 'lcCstSlLnk'
          
          =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
          lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
        ELSE   
          lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
        ENDIF
      ENDIF
      *B602688,1 Reham On 03/21/99   *** End   ***
      
      SELECT (lcCrMemLin)
      REPLACE GL_COST  WITH lcGLCost ;
              GL_SALES WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
      
      *B602688,1 Reham On 03/21/99   *** Begin ***
      REPLACE cCOGSAcnt  WITH lcCOGSAcnt  ;
              cICAcnt    WITH lcICAcnt    ;
              cSalesAcnt WITH lcSalesAcnt ;
              cDiscAcnt  WITH lcDiscAcnt
      *B602688,1 Reham On 03/21/99   *** End   ***
    ENDIF
    UNLOCK
    
    SELECT (lcCrMemLin)
    lcStyDesc = STYLE.DESC
    llCUpDate = .T.
  ELSE
    lcStyle = lcOldValue
    RETURN
  ENDIF
ELSE
  lcStyle = lcOldValue
  RETURN
ENDIF

*-- Call local function to update the temp. files & needed variables 
*-- fields in the uncompleted session file record.
=lfUpdVars()

lcDyeStat = IIF(lnCrMemNo > 0 .AND. llUseDyes .AND. ;
                SEEK(PADR(lcStyle,19) , "STYLE") .AND. STYLE.cdye_flg = 'Y' , ;
                "ENABLE" , "DISABLE")
SHOW GET lcStyle

llBrowse = .F.
*B604896,1 MHM 10/08/2001 Empty Variable to collect right data [Start]
lcinvsllnk = ""
*B604896,1 MHM [End]

*-- Call the browse when function.
=lfwBrowLin()

*!*************************************************************
*! Name      : lfvStyGrd
*! Developer : Reham Al-Allamy
*! Date      : 05/25/1997
*! Purpose   : Valid function of the Style Grade popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyGrd()
*!*************************************************************
*
FUNCTION lfvStyGrd

lcStyGrade = IIF(puGrdPop > 0 , laStyGrade[puGrdPop,2],"")

IF !EMPTY(lcStyle)
  IF SEEK(PADR(lcStyle,19) , "STYLE")
    *-- Get the grade for the original style.
    lcOrgStyGr = STYLE.cStyGrade
  ELSE
    lcOrgStyGr = ""
  ENDIF
ELSE
  lcOrgStyGr = ""
ENDIF

*-- Do not accept any returned style with different grade from the original.
IF !(lcStyGrade == lcOrgStyGr)
  DO CASE
    CASE lcOrgStyGr = "1"
      lcRetSty  = IIF(lcStyGrade = "2" , STYLE.cRetSty , STYLE.cRetSty2)
    CASE lcOrgStyGr = "2"
      lcRetSty  = IIF(lcStyGrade = "1" , STYLE.cRetSty , STYLE.cRetSty2)
    CASE lcOrgStyGr = "3"
      lcRetSty  = IIF(lcStyGrade = "1" , STYLE.cRetSty , STYLE.cRetSty2)
  ENDCASE
  
  lcRetStat = "ENABLE"
  _CUROBJ = OBJNUM(lcRetSty)
  
  *B607363,1 AMH Call the valied function of return style to validate the sizes [Start]
  IF !EMPTY(lcRetSty)
    lcOldValue = " "
    =lfvRetSty()
  ENDIF
  *B607363,1 AMH [End]
  
ELSE
  lcRetSty  = SPACE(19)
  lcRetStat = "DISABLE"
  *B803252,1 Ramy If selected the orignal style grade enable the sizes objects [start]
  SELECT STYLE
  SET FILTER TO
  SET ORDER TO TAG Style
  =SEEK(lcStyle)
  =SEEK('S' + STYLE.SCALE , 'SCALE')
  
  *607363,1 AMH Restore the scale count of original style [Start]
  lnScaleCnt = SCALE.CNT
  *607363,1 AMH [End]
  
  SELECT SCALE
  STORE SPACE(0) TO laSize
*  FOR lnCnt = 1 TO SCALE.CNT
*    laSize[lnCnt] = EVALUATE('SZ'+STR(lnCnt,1)) &&SCALE.SZ&lnCnt
*    SHOW GET laSize[lnCnt]   ENABLE
*    SHOW GET laQtyStk[lnCnt] ENABLE
*  ENDFOR
*  *-- Disable the rest of the array objects
*  FOR lnCnt = SCALE.CNT+1 TO 8
*    SHOW GET laSize[lnCnt]   DISABLE
*    laQtyStk[lnCnt] = 0
*    SHOW GET laQtyStk[lnCnt] DISABLE
*  ENDFOR

  lnQty = 0
  FOR lnCnt = 1 TO 8
    IF  lnCnt <= SCALE.CNT
      SELECT SCALE
      laSize[lnCnt] = EVALUATE('SZ'+STR(lnCnt,1)) &&SCALE.SZ&lnCnt
      SHOW GET laSize[lnCnt]   ENABLE
      SHOW GET laQtyStk[lnCnt] ENABLE
      lnOldValue = 1
      =lfvQty(lnCnt)
      lnQty = lnQty + laQtyStk[lnCnt]
    ELSE
      SHOW GET laSize[lnCnt]   DISABLE
      SHOW GET laQtyStk[lnCnt] DISABLE
      lnOldValue      = laQtyStk[lnCnt]
      laQtyStk[lnCnt] = 0
      =lfvQty(lnCnt)
    ENDIF
  ENDFOR
  lnTotQty = lnQty
  lnAmount = lnTotQty * lnPrice
  SHOW GET lnAmount
  SHOW GET lnTotQty
  *B803252,1 Ramy [end]
ENDIF
*-- Update the current line.
SELECT (lcCrMemLin)
*-- Lock the record to grantee the phiscal update.
= RLOCK()
REPLACE CSTYGRADE  WITH lcStyGrade ;
        cretsty    WITH PADR(lcRetSty,19)

UNLOCK

llCUpDate = .T.

SHOW GET puGrdPop
SHOW GET ibRetSty &lcRetStat
SHOW GET lcRetSty &lcRetStat

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Reham Al-Allamy
*! Date      : 05/26/1997
*! Purpose   : Valid function of the dyelot Object.
*!*************************************************************
*! Calls     : SDYEBROW, lfvRemove
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDyelot()
*!*************************************************************
*
FUNCTION lfvDyelot

IF !EMPTY(lcDyelot) .AND. lcOldValue <> lcDyelot .AND. LASTKEY() = 13
  IF !SEEK(PADR(lcStyle,19) + laData[7] + lcDyelot , 'StyDye') .OR. "?" $ lcDyelot
    *-- Call the dyelot global browse.
    IF !SDYEBROW(PADR(lcStyle,19) , @lcDyelot , .T. , laData[29])
      IF EMPTY(lcDyelot)
        *** The dyelot cannot be empty. ***
        *** <  OK  > ***
        =gfModalGen("INM46007B00000" , "DIALOG")
        =lfvRemove()
        _CUROBJ = OBJNUM(pbNewLine)
        RETURN
      ENDIF
    ENDIF
  ENDIF
  *-- Update the current line.
  SELECT (lcCrMemLin)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE DYELOT WITH lcDyelot
  UNLOCK
  SHOW GET lcDyelot
  llCUpDate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvRetSty
*! Developer : Reham Al-Allamy
*! Date      : 05/25/1997
*! Purpose   : Valid function of the Damaged Return Style Object.
*!*************************************************************
*! Calls     : gfStyBrw, lfwBrowLin
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRetSty()
*!*************************************************************
*
FUNCTION lfvRetSty
*B607289,1 KHM 06/16/2003 (Begin) Changnig the IF command.
*IF (llBrowse .OR. !(lcRetSty == lcOldValue))
IF (llBrowse .OR. !SEEK(PADR(lcRetSty,19), "Style") OR !(lcRetSty == lcOldValue))
*B607289,1 KHM 06/16/2003 (End)
  
  lcOrgStyGr = IIF(SEEK(PADR(lcStyle,19) , "STYLE") , STYLE.cStyGrade , "")
  
  *-- Set order to the quality index.
  SELECT STYLE
  SET ORDER TO TAG StyQlty
  
  *-- Set filter to the selected style grade or quality.
  SET FILTER TO 
  SET FILTER TO cStyGrade = lcStyGrade
  
  IF !SEEK(lcStyGrade + PADR(lcRetSty,19) , "STYLE") .OR. llBrowse
    *B602771,1 Reham On 04/11/99   *** Begin ***
    *B602771,1 Enable the soft seek in the style browse.
    *-- Call the style global browse.
    *lcRetSty = gfStyBrw("I" , "" , "" , .F.)
    lcRetSty = PADR(gfStyBrw("I" , lcRetSty , "" , .F.),19)
    *B602771,1 Reham On 04/11/99   *** End   ***
    IF !WEXIST(lcBrowTitl)
      =lfwBrowLin()
    ENDIF
  ENDIF
  
  IF !EMPTY(lcRetSty)
    
    *B608300,1 WAM 10/02/2007 (begin)
    *=SEEK(lcStyGrade + PADR(lcRetSty,19) , "STYLE")
    *B608300,1 WAM (End)
    
    lcScale    = Style.Scale
    lcRetStat  = "ENABLE"
    
    *-- Get the scale for the alternative reurned style.
    IF SEEK("S" + lcScale , "SCALE")
      lnScaleCnt  = SCALE.Cnt
      *B803252,1 Ramy [start]
      *-- Restore old index.
      SELECT STYLE
      SET FILTER TO
      SET ORDER TO TAG Style
      =SEEK(lcStyle)
      =SEEK("S" + STYLE.Scale , "SCALE")
      IF lnScaleCnt < SCALE.CNT
        FOR lnCount = lnScaleCnt+1 TO 8

          *B608305,1 NNA 10/04/2007 (Begin) Comment the following Code and message and if user choose a return to style with
          *B608305,1 NNA            fewer sizes that the original style then I'll hold only sizes that match with the original style sizes
          *IF laQtyStk[lnCount] > 0
            *** You have to enter a style with scale count equal or less than the ***
            *** returned style scale. ***
            *** < Ok > ***
            *=gfModalGen("TRM46037B00000" , "Dialog")
            *lcRetSty = ''
            *607363,1 AMH Restore the scale count of original style [Start]
            *lcRetStat = "DISABLE"
            *lnScaleCnt = SCALE.CNT
            *SHOW GET ibRetSty DISABLE
            *607363,1 AMH [End]
            **PUGRDPOP = 1
            *PUGRDPOP = VAL(lcOrgStyGr)
            *_CUROBJ = OBJNUM(PUGRDPOP)
            *SHOW GET lcRetSty DISABLE
            *SHOW GET PUGRDPOP
            *RETURN
          *ENDIF
          lnOldValue = laQtyStk[lnCount]		&& Hold the old value to use it with the Check in (LfvQty)
          laQtyStk[lnCount]=0                   && Eliminate the increased sizes over the return to style's sizes
          =lfvQty(lnCount)   					&& Calling the Qty. validation function to do as standard
          laSize[lnCount] = ""
          SHOW GET laSize[lnCount]   DISABLE
          SHOW GET laQtyStk[lnCount] DISABLE
          *B608305,1 NNA (End)
          
        ENDFOR
      ENDIF
      *-- Return to the returned style
      =SEEK(lcRetSty , "STYLE")
      =SEEK("S" + STYLE.Scale , "SCALE")
      lnScaleCnt  = SCALE.Cnt
      *B803252,1 Ramy [END]
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        *B802709,1 Reham On 12/26/1999    *** Begin ***
        *laQtyStk[lnCount] = 0
        *B802709,1 Reham On 12/26/1999    *** End   ***
        IF lnCount <= lnScaleCnt
          laSize[lnCount] = IIF(lnScaleCnt = 0 , "" , SCALE.SZ&lcCount)
          SHOW GET laSize[lnCount]   ENABLE
          SHOW GET laQtyStk[lnCount] ENABLE
        ELSE
          *B606726,1 ABD - assigne 0 to the Qty array if scale is Diefferent. [Begin]
          laQtyStk[lnCount]=0
          *B606726,1 ABD - [End]
          laSize[lnCount] = ""
          SHOW GET laSize[lnCount]   DISABLE
          SHOW GET laQtyStk[lnCount] DISABLE
        ENDIF
      ENDFOR
    ENDIF
 
    *B605256 RAE [BEGIN] Assign the damaged style to different warehouse 
    lnAlias = SELECT()
    SELECT StyDye
    IF !SEEK(PADR(lcRetsty,19) + laData[29] + SPACE(10) , 'StyDye')
      *** Style: ALLTRIM(lcRetsty) ***
      *** is not assigned to warehouse: ALLTRIM(laData[29])  ***
      *** <  Add  > - < Reenter > ***
      lcTmpTxt = ALLTRIM(lcRetsty) + "|" + ALLTRIM(laData[29])
      IF gfModalGen("QRM46004B00031" , "DIALOG" , lcTmpTxt) = 1
        DO gpAdStyWar WITH PADR(lcRetsty,19) , SPACE(10) , laData[29]
      ELSE
        SELECT (lnAlias)
        lcRetsty= lcOldValue
        _CUROBJ = OBJNUM(lcRetsty)
        RETURN
      ENDIF
    ENDIF
    *B605256 RAE [END]
 
    SELECT (lcCrMemLin)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    
    IF llLink_GL
      lcStySlLnk = Style.cslsgllink
      REPLACE GL_SALES WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
    ENDIF
    
    *-- Update the current line.
    SELECT (lcCrMemLin)
    REPLACE cretsty   WITH PADR(lcRetSty,19) ;
            cstygrade WITH lcStyGrade
    
    *B608150,1 TMI [Start] allow the return cost to be of the returned style only when this credit memo is not based on invoice
    IF EMPTY(laData[7])
      *B608150,1 TMI [End  ] 
  
      *B126474,1 NNA 04/12/2005 (BEGIN) Seek for the Return To Style to get its Cost
      STORE 0 TO lnRecNo
      lnRecNo = RECNO('STYLE')
      IF SEEK(PADR(lcRetSty,19),'STYLE')
        REPLACE &lcCrMemLin..COST WITH IIF(Style.Ave_Cost>0,Style.Ave_Cost,IIF(Style.MAKE,Style.Nmcost1,Style.Nicost1))
      ENDIF
      IF BETWEEN(lnRecNo,1,RECCOUNT('STYLE'))
        GOTO lnRecNo IN STYLE
      ENDIF
      *B126474,1 NNA (END)
  
      *B608150,1 TMI [Start] 
    ENDIF
    *B608150,1 TMI [End  ] 

    *B606726,1 ABD - sum the Qty again from the array and update the file. [Begin]
    lnTotQty = 0
    For I = 1 TO 8
      lcSize = Str(I,1)
      REPLACE Qty&lcSize WITH laQtyStk[I]
      lnTotQty = lnTotQty + Qty&lcSize
    ENDFOR
    REPLACE TotQty WITH lnTotQty
    *B606726,1 ABD - [End]
    UNLOCK
    
    llCUpDate = .T.
  ELSE
    lcRetSty = lcOldValue

    *B607289,1 KHM 06/16/2003 (Begin) Display a message to inform the user that to select
    *B607289,1                a damage/second style and if not change the quality to 1st.
    IF EMPTY(lcRetSty)

      lcOrgStyGr = IIF(SEEK(PADR(lcStyle,19) , "STYLE") , STYLE.cStyGrade , "")
      lcOrgStyGr = IIF(EMPTY(lcOrgStyGr),"1",lcOrgStyGr)
      lcStyGrdDs = IIF(lcOrgStyGr = "2", "2nd", "Damaged")
      
      *B607363,1 AMH Don't display this message only if there is style with the current quality [Start]
      *=gfModalGen("QRM46042B00000" , "DIALOG" , lcStyGrdDs+"|"+lcStyGrdDs) 
      IF !EOF('STYLE')
        =gfModalGen("QRM46042B00000" , "DIALOG" , lcStyGrdDs+"|"+lcStyGrdDs)
      ENDIF
      *B607363,1 AMH [End]
      
      SELECT (lcCrMemLin)
	  *-- Lock the record to grantee the phiscal update.
	  = RLOCK()
	  REPLACE CSTYGRADE  WITH lcOrgStyGr ;
              cRetSty    WITH PADR(lcRetSty,19)
      UNLOCK
      
      PUGRDPOP = VAL(lcOrgStyGr)
      lcRetStat = "DISABLE"
      SHOW GET PUGRDPOP
      KEYBOARD "{ALT+B}" CLEAR 
    ENDIF
    *B607289,1 KHM 06/16/2003 (End)

  ENDIF
  
  *-- Restore old index.
  SELECT STYLE
  SET FILTER TO
  SET ORDER TO TAG Style
ENDIF

*-- Call local function to update the temp. files & needed variables 
*-- fields in the uncompleted session file record.
=lfUpdVars()

llBrowse = .F.

SHOW GET lcRetSty &lcRetStat
SHOW GET puGrdPop

*!*************************************************************
*! Name      : lfvReson2
*! Developer : Reham Al-Allamy
*! Date      : 05/06/1997
*! Purpose   : Valid function of the lines reason popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvReson2()
*!*************************************************************
*
FUNCTION lfvReson2

*-- Validate the line reason & update the current line.
llCUpDate = .T.
lcReason = laReson2[lnReson2,2]
SELECT (lcCrMemLin)
*-- Lock the record to grantee the phiscal update.
= RLOCK()
REPLACE REASON WITH lcReason
UNLOCK
SHOW WINDOW (lcBrowTitl) REFRESH

*!*************************************************************
*! Name      : lfvGrs_Pct
*! Developer : Reham Al-Allamy
*! Date      : 06/01/1997
*! Purpose   : Valid function of the Gross price & discount percentage.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGrs_Pct()
*!*************************************************************
*
FUNCTION lfvGrs_Pct

*B127806,1 EIH 05/22/2005 If we edit in credit memo then entire invoice = .F.  [Begin].
STORE .F. TO llEntrInv 
*B127806,1 EIH [End].

*-- Subtract the old amount.
*B802213,1 Reham On 05/02/99   *** Begin ***
laData[23] = laData[23] - &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
*B802213,1 Reham On 05/02/99   *** End   ***

laData[24] = laData[24] - &lcCrMemLin..DISC_AMT
laData[25] = laData[25] - &lcCrMemLin..Amount
*B605261,4 (Begin)  Calculate taxable amount
lnDueTax = lnDueTax - IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0)
*B605261,4 (End)

laData[30] = IIF(llIsCanada , laData[30] - &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
*C102212,1 (Begin) Calculate HST Tax Rate.
laData[42] = IIF(llIsCanada , laData[42] - &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
*C102212,1 (End)
*laData[26] = laData[26] - ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A" AND !llIsEnglnd , laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/100))

*B603339,1 [START] Reduce taxes by the usin net price
*laData[26] = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))

*B603967,1 (Begin) The following applies only when this function called from Discount fields not from Gross Price...
*lnTmpDisc   = lnDisclnPc
*lnDisclnPc  = lnOldValue
*laData[26]  = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
*lnDisclnPc  = lnTmpDisc

IF VARREAD() = 'LNDISCLNPC'
  lnTmpDisc   = lnDisclnPc
  lnDisclnPc  = lnOldValue
  *B605261,4 (Begin)  Consider if the style is taxable of not.
  *laData[26]  = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  laData[26]  = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (  IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0) + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  *B605261,4 (End)
  lnDisclnPc  = lnTmpDisc
ELSE
  *B605261,4 (Begin)  Consider if the style is taxable of not.
  *laData[26]  = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  laData[26]  = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0) + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  *B605261,4 (End)  
ENDIF


*B603967,1 (End)
*laData[26] = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , lnPrice, lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
*B603339,1 [END]

*-- Update the current line.
SELECT (lcCrMemLin)
*-- Lock the record to grantee the phiscal update.
= RLOCK()

*:B131583,1 MHM 03/07/2005  [Start]
*REPLACE DISC_PCNT  WITH lnDiscLnPc ;
        GROS_PRICE WITH lnGrsPrice ;
        DISC_AMT   WITH lnGrsPrice * lnTotQty * lnDiscLnPc / 100 ;
        PRICE      WITH lnGrsPrice * (1 - lnDiscLnPc /100) ;
        AMOUNT     WITH (lnTotQty * lnGrsPrice) * (1-(lnDiscLnPc/100)) ;
        TRDE_AMT   WITH AMOUNT * lnInvTrdDs/100

REPLACE DISC_PCNT  WITH lnDiscLnPc ;
        GROS_PRICE WITH lnGrsPrice ;
        DISC_AMT   WITH (lnGrsPrice-lnPrice) * lnTotQty  ;
        PRICE      WITH lnGrsPrice * (1 - lnDiscLnPc /100) ;
        AMOUNT     WITH lnTotQty * ROUND((lnGrsPrice * (1-(lnDiscLnPc/100))),2) ;
        TRDE_AMT   WITH AMOUNT * lnInvTrdDs/100

*:B131583,1 MHM  [End]

*B607994,1 TMI [Start] fix a bug that the DISC_AMT field is not updated while the DISC_PCNT field is changed
REPLACE DISC_AMT   WITH (GROS_PRICE-PRICE) * lnTotQty  
*B607994,1 TMI [End  ] 

UNLOCK

*-- Fill the amounts variable with its values.
lnAmount   = &lcCrMemLin..Amount
lnPrice    = &lcCrMemLin..Price
lnDisc_Amt = &lcCrMemLin..DISC_AMT
lnTrde_Amt = &lcCrMemLin..TRDE_AMT

*-- Add the new amount.
*B802213,1 Reham On 05/02/99   *** Begin ***
laData[23] = laData[23] + &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
*B802213,1 Reham On 05/02/99   *** End   ***
laData[24] = laData[24] + &lcCrMemLin..DISC_AMT
laData[25] = laData[25] + &lcCrMemLin..Amount

*B605261,4 (Begin)  Calculate tax on taxable amount only.
lnDueTax = lnDueTax + IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0)
*B605261,4 (End)


laData[30] = IIF(llIsCanada , laData[30] + &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
*C102212,1 (Begin) Calculate HST Tax Rate.
laData[42] = IIF(llIsCanada , laData[42] + &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
*C102212,1 (End)
*laData[26] = laData[26] + ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A" AND !llIsEnglnd, laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/100))
*B603339,1 [Start] use net price for calculating taxes instead of gross price
*laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
*B605261,4 (Begin)  Consider if the style is taxable of not.
*laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE, lcCrMemLin) , IIF(llTax , &lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE, lcCrMemLin) , IIF(llTax , (IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0) + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
*B605261,4 (End)
*B603339,1 [end]

*E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
laData[26] = ROUND(laData[26],2)
*E302198,1 ALB change tax rate to print 99.999 formate [END]

*C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
ENDIF
*C200517,1 KHM 04/13/2003 (End)

*C102212,1 (Begin) Add HST Tax.
*laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
*C102212,1 (End)

*-- Calculate the salesreps charge back.
IF lnInvTrdDs <> 0
  laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
  laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
ELSE
  laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
  laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
ENDIF

llCUpDate  = .T.

*C102676,1 Custom process for A.S.T. [Begin]
IF ASCAN(laEvntTrig,PADR('CRUPCOM3',10))<>0
  =gfDoTriger('RMCRMEM',PADR('CRUPCOM3',10))
ENDIF  
*C102676,1 Custom process for A.S.T. [End]

*-- Refresh the line's objects.
SHOW GET lnAmount
SHOW GET lnPrice
SHOW GET lnGrsPrice
SHOW GET lnDiscLnPc

SHOW GET laData[18]
SHOW GET laData[21]
SHOW GET laData[24]
SHOW GET laData[25]
SHOW GET laData[26]
SHOW GET laData[28]
*B802213,1 Reham On 05/02/99   *** Begin ***
SHOW GET laData[23]
*B802213,1 Reham On 05/02/99   *** End   ***
*C102212,1 (Begin) Show get HST Tax amount.
SHOW GET laData[42] &lcLinStat
SHOW GET laData[30]
*C102212,1 (End)
SHOW WINDOW (lcBrowTitl) REFRESH

*!*************************************************************
*! Name      : lfvPstRate
*! Developer : Reham Al-Allamy
*! Date      : 06/01/1997
*! Purpose   : Valid function of the Pst tax Rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPstRate()
*!*************************************************************
*
FUNCTION lfvPstRate

*-- Validate the pst rate & update the current line.
SELECT (lcCrMemLin)
laData[30] = IIF(llIsCanada , laData[30] - &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
*-- Lock the record to grantee the phiscal update.
= RLOCK()
REPLACE NPSTRATE WITH lnPstRate
UNLOCK
laData[30] = IIF(llIsCanada , laData[30] + &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)

*C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
ENDIF
*C200517,1 KHM 04/13/2003 (End)

*C102212,1 (Begin) Show get PST Tax amount.
laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
SHOW GET laData[30]
*C102212,1 (End)
llCUpDate = .T.

*!*************************************************************
*! Name      : lfvQty
*! Developer : Reham Al-Allamy
*! Date      : 05/29/1997
*! Purpose   : Valid function of the Qtuantities.
*!*************************************************************
*! Calls     : lnQtyNo -> Which qty.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvQty(1)
*!*************************************************************
*
FUNCTION lfvQty
PARAMETERS lnQtyNo
PRIVATE lnQtyNo , lnCount

*B127806,1 EIH 05/22/2005 entire invoice = .F.  [Begin].
STORE .F. TO llEntrInv 
*B127806,1 EIH [End].


IF laQtyStk[lnQtyNo] <> lnOldValue
  lcQtyNo = ALLTRIM(STR(lnQtyNo))
  
  *-- Subtract the old amount.
  laData[22] = laData[22] - &lcCrMemLin..TotQty
  laData[23] = laData[23] - &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
  laData[24] = laData[24] - &lcCrMemLin..DISC_AMT
  *laData[26] = laData[26] - ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A" AND !llIsEnglnd , laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/100))
  *B603339,1 [Start] pass net prive instead of gross price to calculate taxes
  *B605261,4 (Begin)  Consider if the style is taxable of not.
  *laData[26] = laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  IF !llIsEnglnd AND llTax AND SEEK(&lcCrMemLin..Style,'Style') AND Style.lTaxable
    laData[26] = MAX(laData[26] - (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0)
  ELSE
    *B119936,1 ABD - Get the the GST tax in case canada and use tax. [Begin]
    *laData[26] = MAX(laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) ,0),0)
    IF llIsCanada .AND. llTax
      laData[26] = MAX(laData[26] - (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0)
    ELSE
      laData[26] = MAX(laData[26] - IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) ,0),0)
    ENDIF
    *B119936,1 ABD - [End]
    
  ENDIF  
  *B605261,4 (End)

  *laData[26] = laData[26] - ;
  IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , lnPrice , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  *B603339,1 [End]
  laData[25] = laData[25] - &lcCrMemLin..Amount
  *B605261,4 (Begin)  Calculate tax on taxable amount only.
  lnDueTax = lnDueTax - IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0)
  *B605261,4 (End)
  
  laData[30] = IIF(llIsCanada , laData[30] - &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
  *C102212,1 (Begin) Calculate HST Tax Rate.
  laData[42] = IIF(llIsCanada , laData[42] - &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
  *C102212,1 (End)
  lnTrdeAmt  = lnTrdeAmt  - &lcCrMemLin..TRDE_AMT

  *-- Update the current line.
  SELECT (lcCrMemLin)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()

  *:B131583,1 MHM 03/07/2005  [Start]
  *REPLACE Qty&lcQtyNo WITH laQtyStk[lnQtyNo] ;
          TOTQTY      WITH laQtyStk[1] + laQtyStk[2] + laQtyStk[3] + ;
                           laQtyStk[4] + laQtyStk[5] + laQtyStk[6] + ;
                           laQtyStk[7] + laQtyStk[8] ;
          AMOUNT      WITH (TOTQTY * lnGrsPrice) * (1-(lnDiscLnPc/100)) ;
          DISC_AMT    WITH lnGrsPrice * TOTQTY * lnDiscLnPc / 100 ;
          TRDE_AMT    WITH AMOUNT * lnInvTrdDs/100
        
  REPLACE Qty&lcQtyNo WITH laQtyStk[lnQtyNo] ;
          TOTQTY      WITH laQtyStk[1] + laQtyStk[2] + laQtyStk[3] + ;
                           laQtyStk[4] + laQtyStk[5] + laQtyStk[6] + ;
                           laQtyStk[7] + laQtyStk[8] ;
          AMOUNT      WITH TOTQTY * ROUND((lnGrsPrice * (1-(lnDiscLnPc/100))),2) ;
          DISC_AMT    WITH (lnGrsPrice-lnPrice) * TOTQTY ;
          TRDE_AMT    WITH AMOUNT * lnInvTrdDs/100

  *:B131583,1 MHM 03/07/2005  [End]
  
  UNLOCK
  
  *B602932,1 Reham On 05/26/1999   *** Begin ***
  *B602932,1 Check if this style was shipped to this customer before, do not change the price.
  *B602841,1 Reham On 05/02/99     *** Begin ***
  *B602841,1 Validate the at Qty. price only if not from invoice or RA.
  *SET ORDER TO TAG InvLines IN InvLine
  *IF (!EMPTY(laData[3]) .OR. !EMPTY(laData[7])) .AND. laData[15] = "Y"
  *IF (!EMPTY(laData[7]) .AND. laData[15] = "Y" .AND. SEEK(&lcCrMemLin..Style+laData[7] , "InvLine")) .OR. ;
  *   (!EMPTY(laData[3]) .AND. SEEK(laData[3]+&lcCrMemLin..style , "RaLine"))
  *ELSE
  *B602841,1 Reham On 05/02/99     *** End   ***
  IF !&lcCrMemLin..lShipped
  *B602932,1 Reham On 05/26/1999   *** End   ***
    *E301142,4 Reham On 03/08/99   *** Begin ***
    *E301142,4 Get the price level per qty.
    lcPriceLvl = IIF(EMPTY(CUSTOMER.PRICELVL) , "A" , CUSTOMER.PRICELVL)
    IF lcPriceLvl = "Q"
      lnGrsPrice = lfGetprice(lcStyle,lcPriceLvl,&lcCrMemLin..TOTQTY)
      
      *E301142,4 Calculate the discount for the current style price.
      lnGrsPrice = lfCalcDisc(lcStyle , lnGrsPrice)
      
      *E301142,4 Update the price.
      SELECT (lcCrMemLin)
      = RLOCK()
      REPLACE GROS_PRICE WITH lnGrsPrice ;
              PRICE      WITH GROS_PRICE * (1 - DISC_PCNT/100)
      UNLOCK
    ENDIF
    *E301142,4 Reham On 03/08/99   *** End   ***
  *B602841,1 Reham On 05/02/99     *** Begin ***
  ENDIF
  *B602932,1 Reham On 05/26/1999   *** Begin ***
  *SET ORDER TO TAG InvLine IN InvLine
  *B602932,1 Reham On 05/26/1999   *** End   ***
  *B602841,1 Reham On 05/02/99     *** End   ***
  
  *-- Add the new amount.
  laData[22] = laData[22] + &lcCrMemLin..TotQty
  laData[23] = laData[23] + &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
  laData[24] = laData[24] + &lcCrMemLin..DISC_AMT
  laData[25] = laData[25] + &lcCrMemLin..Amount
  *B605261,4 (Begin)  Calculate tax on taxable amount only.
  lnDueTax = lnDueTax + IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0)
  *B605261,4 (End)
    
  *laData[26] = laData[26] + ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A" AND !llIsEnglnd , laData[27],0)) * (&lcCrMemLin..TAX_RATE/100))
  *B603339,1 [Start]pass net price instead of gross price
  *B605261,4 (Begin)  Consider if the style is taxable of not.
  *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  IF !llIsEnglnd AND llTax AND SEEK(&lcCrMemLin..Style,'Style') AND Style.lTaxable
    laData[26] = laData[26] + (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27],0)) * &lcCrMemLin..TAX_RATE/ 100
  ELSE
    *B119936,1 ABD - Get the the GST tax in case canada and use tax. [Begin]
    *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin),0)
    IF llIsCanada .AND. llTax
      laData[26] = laData[26] + (&lcCrMemLin..Amount +IIF(lcTax_Meth = "A" , laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/ 100)
    ELSE
      laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin),0)
    ENDIF
    *B119936,1 ABD - [End]
    
  ENDIF  
  *B605261,4 (End)

  *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
  laData[26] = ROUND(laData[26],2)
  *E302198,1 ALB change tax rate to print 99.999 formate [END]

  *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , IIF(EMPTY(laData[7]) AND laData[15]#'Y',lnPrice,&lcCrMemLin..GROS_PRICE), lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
  *B603339,1 [end]
  laData[30] = IIF(llIsCanada , laData[30] + &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
  *C102212,1 (Begin) Calculate HST Tax Rate.
  laData[42] = IIF(llIsCanada , laData[42] + &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
  *C102212,1 (End)
  *C102212,1 (Begin) Add HST Tax.
  *laData[28] = laData[27] + laData[25] + laData[26] + laData[30]

  *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
  IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
    =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
  ENDIF
  *C200517,1 KHM 04/13/2003 (End)

  laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
  *C102212,1 (End)
  lnTrdeAmt  = lnTrdeAmt  + &lcCrMemLin..TRDE_AMT
  
  *-- Calculate the salesreps. charge back.
  IF lnInvTrdDs <> 0
    laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
  ELSE
    laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
  ENDIF
  
  lnTotQty = &lcCrMemLin..TOTQTY
  lnAmount = &lcCrMemLin..AMOUNT
  
  *B802907,1 Reham On 12/27/1999    *** Begin ***
  IF SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
    lnAuthPcs = Max(lnRAQty - laData[22] , 0)
    lnAuthAmt = Max(lnRAAmt - laData[23] , 0)
  ENDIF
  SHOW GET lnAuthPcs
  SHOW GET lnAuthAmt
  *B802907,1 Reham On 12/27/1999    *** End   ***
  
  
  *E301142,4 Reham On 03/08/99   *** Begin ***
  lnGrsPrice = &lcCrMemLin..GROS_PRICE
  lnPrice    = &lcCrMemLin..PRICE
  SHOW GET lnGrsPrice
  SHOW GET lnPrice
  *E301142,4 Reham On 03/08/99   *** End   ***
  
  *-- Refresh the line's object.
  SHOW GET laQtyStk[lnQtyNo]
  SHOW GET lnTotQty
  SHOW GET lnAmount
  SHOW GET laData[18]
  SHOW GET laData[21]
  SHOW GET laData[22]
  SHOW GET laData[23]
  SHOW GET laData[24]
  SHOW GET laData[25]
  SHOW GET laData[26]
  SHOW GET laData[28]
  SHOW GET laData[30]
  *C102212,1 (Begin) Show get HST Tax amount.
  SHOW GET laData[42]
  *C102212,1 (End)
  SHOW WINDOW (lcBrowTitl) REFRESH
  llCUpDate = .T.
ENDIF

*C102676,1 Custom process for A.S.T. [Begin]
IF ASCAN(laEvntTrig,PADR('CRUPCOM3',10))<>0
  =gfDoTriger('RMCRMEM',PADR('CRUPCOM3',10))
ENDIF  
*C102676,1 Custom process for A.S.T. [End]

*B602783,1 Reham On 05/02/99   *** Begin ***
*B602783,1 Force to push button new if last size.
IF lnQtyNo = lnScaleCnt
  _CUROBJ = OBJNUM(pbNewLine)
ENDIF
*B602783,1 Reham On 05/02/99   *** End   ***

*!*************************************************************
*! Name      : lfGetCost
*! Developer : Reham Al-Allamy
*! Date      : 06/01/1997
*! Purpose   : Get the cost for the current style.
*!*************************************************************
*! Calls     : RMEDCST.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetCost()
*!*************************************************************
*
FUNCTION lfGetCost

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*B802803,1 Reham On 11/21/1999    *** Begin ***
*B802803,1 Change the program & its screens to be shared between modules.
*-- Call the Edit Cost screen for the current style.
*DO (gcScrDir+gcWinAppl+"\RMEDCST.SPX")
DO (gcScrDir+"RMEDCST.SPX")
*B802803,1 Reham On 11/21/1999    *** End   ***

*-- Restore the trapped keys.
POP KEY

RETURN lnCost

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Reham Al-Allamy
*! Date      : 06/01/1997
*! Purpose   : Save Credit Memo
*!*************************************************************
*! Calls     : lfFilTmpHd, GLDIST, NOTPAD, gpUdtSJl 
*!             gfOpenFile, lfCrtUnComp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
PRIVATE lnAmount

*B606206,1 TMI [Start] If Sales rep not entred for any reason , Deny Saving ( FOR ENG07 )
IF ASCAN(laEvntTrig,PADR('RMSAVEOK',10))<>0 AND ;
  !gfDoTriger('RMCRMEM',PADR('RMSAVEOK',10))
  RETURN .F.
ENDIF
*B606206,1 TMI [End  ] If Sales rep not entred for any reason , Deny Saving ( FOR ENG07 )

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C037816,1 MHM 04/06/2004 [Start] Save to custom Bin location files. 
IF ASCAN(laEvntTrig,PADR('RMCKSVBN',10))<>0 AND ;
  !gfDoTriger('RMCRMEM',PADR('RMCKSVBN',10))
  RETURN .F.
ENDIF
*C037816,1 MHM 04/06/2004 [End]
*B039660,1 NNA (End)

*B127290,1 NNA 06/24/2005 (Begin)
IF laScrMode[4] AND !USED(lcEngChrg) .AND. !EMPTY(ladata[7]) .AND. llFrstTime
  *--Opening the CRMEMO charges file.
  =gfOpenFile(gcDataDir+'CrmChrg',gcDataDir+'CrmChrg','SH')
  *--Opening the Invoice charges file.
  =gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
  SELECT InvChrg
  =AFIELDS(laFileStru)
  =gfCloseFile('InvChrg')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  *--  Create temp file for the english Charges
  =gfCrtTmp(lcEngChrg,@laFileStru,[Order+cStore+PikTkt+cchrgcode],lcEngChrg)
  llFrstTime = .F.
  lnTrdDisc = IIF(SEEK(ladata[7],'InvHdr'),Invhdr.trde_disc,0)
  = lfgetData ()
ENDIF
*B127290,1 NNA (End)

SELECT (lcCrMemLin)
LOCATE FOR !EMPTY(STYLE)
IF !FOUND() OR lnCrMemNo = 0
  *** No lines entered! Cannot save. ***
  *** <  Ok  > ***
  =gfModalGen('INM46008B00000','ALERT')
  llCSave = .F.
  RETURN
  *B605438,1 RAE [BEGIN] check if the posting date is less than credit memo date
ELSE
  IF laData[41] < laData[5]
    =gfModalGen("INM46024B00000" , "DIALOG")
    llCSave = .F.
    RETURN
  ENDIF
*B605438,1 RAE [END]
  
ENDIF

*-- If the user did not validate the transaction date.
IF llValidDt .OR. EMPTY(lcGLFYear) .OR. EMPTY(lcGLPeriod)
  IF !CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM')
    IF lnActFolder = 1
      _CUROBJ = OBJNUM(laData[41])
    ENDIF
    *WAB
    *laData[41] = {}
    laData[41] = laData[5]
    =CHECKPRD(laData[41] , 'lcGLFYear' , 'lcGLPeriod' , 'RM',.T.)
    *WAB
    llCSave = .F.
    RETURN
  ENDIF
  llValidDt = .F.
ENDIF

IF llMultiWH
  IF EMPTY(laData[29])
    *** You have to enter a warehouse. Cannot save! ***
    *** <  Ok  > ***
    =gfModalGen('INM46013B00000','ALERT')
    IF lnActFolder = 1
      _CUROBJ = OBJNUM(puWareHous)
    ENDIF
    llCSave = .F.
    RETURN
  ENDIF
ELSE
  laData[29] = lcDefWare
ENDIF

IF llMulCurr
  DO CASE
    CASE EMPTY(laData[31])
      *** You cannot leave the currency code empty! ***
      *** <  Ok  > ***
      =gfModalGen("INM46015B00000" , "DIALOG")
      IF lnActFolder = 1
        _CUROBJ = OBJNUM(laData[31])
      ENDIF
      llCSave = .F.
      RETURN
    CASE EMPTY(laData[33])
      *** The exchange rate must be greater than zero. ***
      *** <  Ok  > ***
      =gfModalGen("INM46001B00000" , "DIALOG")
      IF lnActFolder = 1
        _CUROBJ = OBJNUM(laData[33])
      ENDIF
      llCSave = .F.
      RETURN
  ENDCASE
ELSE
  laData[31] = gcBaseCurr &&  Variable to hold currency code.
  laData[32] = 1          &&  Variable to hold currency unit.
  laData[33] = 1          &&  Variable to hold exchange rate.
  lcUntSin   = '/'        &&  Variable to hold unit sign.
  lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
ENDIF

*-- Call local function to update the temp. files & needed variables 
*-- fields in the uncompleted session file record.
=lfUpdVars()

*-- Save the save push button in the session processes.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH 'pbSav'
  UNLOCK
  =RLOCK()
ENDIF

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.

IF &lcCrMemHdr..nSteps < 1
  *-- Assign next CRMEMO number.
  laData[1] = gfSequence('CRMEMO','','',laData[14])
  
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ELSE
  laData[1] = &lcCrMemHdr..CrMemo
ENDIF

*B602719,1 Reham On 03/30/99   *** Begin ***
*B602719,1 Open the invoice file in the save procedure to void an error if saving 
*B602719,1 uncomplete session.
=gfOpenFile(gcDataDir+'InvHdr' , gcDataDir+'InvHdr' , 'SH')
*B602719,1 Reham On 03/30/99   *** End   ***

*B602688,1 Reham On 03/21/99   *** Begin ***
IF llLink_GL
  *IF !EMPTY(laData[7]) .AND. !EMPTY(laData[34])
  IF !EMPTY(laData[7])
    IF SEEK(laData[7] , "InvHdr")
      laData[34] = Invhdr.link_code
    ENDIF
  ELSE
    *B602688,1 If Gl is set at division level, get GL Link code from division.
    IF llDiv_Link
      DECLARE laDRltFld[1,2]
      laDRltFld[1,1] = 'LINK_CODE'
      laDRltFld[1,2] = 'lcCustLink'
      
      =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
      laData[34] = IIF(!EMPTY(lcCustLink), PADR(lcCustLink,6) ,IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF"))
    ELSE   
      laData[34] = IIF(!EMPTY(CUSTOMER.link_code) , CUSTOMER.link_code , "DEFDEF")
    ENDIF
    *E500374,6 WAB - (Start) get the Gl Link_code from factore if the customer has factor.
    *B122626,1 ASH 04/21/2004 (Begin) Fix the bug of saving wrong gl link code if the customer's factor has link code other than DEFDEF.
    IF !EMPTY(laData[9])
      IF SEEK(laData[9],'Factor')      
        laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
      ENDIF  
    ELSE
    *B122626,1 ASH 04/21/2004 (End)
      IF SEEK('M'+laData[2],'CUSTOMER') AND !EMPTY(CUSTOMER.cFacCode)
        IF SEEK(CUSTOMER.cFacCode,'Factor')      
          laData[34] =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,laData[34])
        ENDIF  
      ENDIF   
    ENDIF
    *E500374,6 WAB - (End)
  ENDIF
ELSE
  laData[34] = ""
ENDIF
*B602688,1 Reham On 03/21/99   *** Begin ***

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()


*B802907,1 Reham On 12/24/1999   *** Begin ***
*B802907,1 Fix the problem of updating the original style instead of the returned style upon saving.
RELEASE WINDOW (lcBrowTitl)
*B802907,1 Reham On 12/24/1999   *** End   ***

*-- Call local function to execute saving procedure...
*-- .F.        -> Flag to know that the calling program is not point of sale.
*-- lcCrMemHdr -> Variable for temp. credit memo header.
*-- lcCrMemLin -> Variable for temp. credit memo lines.
=lfSavRet(.F. , lcCrMemHdr , lcCrMemLin)

*C200512,1 KHM 05/04/2003 (Begin) Add credited quantities back onto to sales order.
IF ASCAN(laEvntTrig , PADR('UPDRMSO',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('UPDRMSO',10))
ENDIF
*C200512,1 KHM 05/04/2003 (End)

*B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
*C037816,1 MHM 04/06/2004 Save data to Bin Location Files[Start]
IF ASCAN(laEvntTrig , PADR('DLSAVRM',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('DLSAVRM',10))
ENDIF
*C037816,1 MHM [End]
*B039660,1 NNA (End)

*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Close the files that will be opened in the save procedure.
=gfCloseFile('ICSTYHST')
*B602641,1 Reham On 03/04/99   *** Begin ***
*B602641,1 To void the error of not finding the file upon saving
*=gfCloseFile('REPCOMM')
*B602641,1 Reham On 03/04/99   *** End   ***
=gfCloseFile('CREDIT')
=gfCloseFile('ARCUSHST')
*E301077,4 Reham On 12/27/98   *** End   ***

*-- Inform the user with the created credit memo no.
IF laScrMode[4]
  *** Credit Memo has been saved as : {laData[1]}. ***
  *** <   OK   > ***
  =gfModalGen('INM46028B00000','DIALOG' , laData[1])
*B129286,2 MMR 01/03/2005 Fix bug of retaining the header amounts after saving then make new one.
STORE .F. TO llEntrInv 
*B129286,2 MMR.[End]
ENDIF
*B602719,1 Reham On 04/05/99   *** Begin ***
*-- Create the temp files.
*=lfCrtUnComp(.T. , .T.)

*E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
*E301490,1 (Start)
*=lfCrtUnComp(.T. , .T. , .T.)
DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .T. , .T.,.T.
*E301490,1 (End)

*B602719,1 Reham On 04/05/99   *** End   ***

*-- Finish the whole session.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF
lnUnCmSeRc = 0

SELECT RETHDR

*!*************************************************************
*! Name      : lfSavRet
*! Developer : Reham Al-Allamy
*! Date      : 02/26/1998
*! Purpose   : Save credit memo.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : llPontSale -> .T. if from point of sale prg.
*!                   : lcCrMemHdr -> Temp. cerdit memo header.
*!                   : lcCrMemLin -> Temp. credit memo line.
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfSavRet()
*!*************************************************************
FUNCTION lfSavRet
PARAMETERS llPontSale , lcCrMemHdr , lcCrMemLin

*E301490,1 AHM 11/27/2000 Move the body of lfSavRet function to function called lfCMSav in 
*E301490,1                program RMSave to be used for Inter-Company Communication Module
*E301490,1 (Start)
DO lfCMSav IN (gcapphome+'\RMSave.PRG') WITH llPontSale , lcCrMemHdr , lcCrMemLin

*!*	*-- If called from point of sale program.
*!*	IF llPontSale
*!*	  *B603538,1 Reham On 03/29/2000   *** Begin ***
*!*	  *B603538,1 Fix the error of "Variable lcOpenLine not found" if called from PS module.
*!*	  lcOpenLine = gfTempName()
*!*	  *B603538,1 Reham On 03/29/2000   *** End   ***
*!*	  
*!*	  *-- Open the gl distribution file if it was not open and set flag 
*!*	  *-- to close before quitting the program.
*!*	  =gfOpenFile(gcDataDir+'GLDIST' , gcDataDir+'GLDISTAC' , 'SH')
*!*	  
*!*	  *E301077,4 Reham On 12/27/98   *** Begin ***
*!*	  *E301077,4 Open the invoice header file.
*!*	  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
*!*	  *E301077,4 Reham On 12/27/98   *** End   ***
*!*	  
*!*	  *-- Check if the current country is england or not.
*!*	  llIsEnglnd = IIF(UPPER(ALLTRIM(gcContCode)) = 'ENG', .T., .F.)
*!*	  
*!*	  *-- If country is Canada.
*!*	  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)
*!*	  
*!*	  *-- If calculate tax.
*!*	  llTax      = ALLTRIM(gfGetMemVar('M_TAX',gcAct_Comp)) = 'Y'
*!*	  
*!*	  *-- Tax method.
*!*	  lcTax_Meth = gfGetMemVar('M_TAX_METH',gcAct_Comp)
*!*	  
*!*	  *-- See if there is GL link or not.
*!*	  llLink_GL  = ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp)) = 'Y'
*!*	  
*!*	  *B603003,1 Reham On 06/21/1999  *** Begin ***
*!*	  *B603003,1 Define this variable to prevent the error if called
*!*	  *B603003,1 from point of sale.
*!*	  *-- If link by division.
*!*	  llDiv_Link = ALLTRIM(gfGetMemVar('M_DIV_LINK',gcAct_Comp))  = 'Y'
*!*	  *B603003,1 Reham On 06/21/1999   *** End   ***
*!*	  
*!*	  *-- Post factored A/R to customer accounts or not.
*!*	  llPostfInv = ALLTRIM(gfGetMemVar('XPOSTFINV',gcAct_Comp)) = 'N'
*!*	  
*!*	  *-- If use the style average cost or not.
*!*	  llAvg_Cost = ALLTRIM(gfGetMemVar('M_COST_METH',gcAct_Comp)) = 'A'
*!*	  
*!*	  *-- Use Dyelot or not.
*!*	  llUseDyes  = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp))    = 'Y'
*!*	  
*!*	  lcGLDstTmp = ""      && Var. hold the temp. name of gl distribution file.
*!*	  *B602719,1 Rehan On 04/05/99   *** Begin ***
*!*	  *B602719,1 If called from point of sale module, Create the temp. GLDIST file.
*!*	  lcGLDstTmp = gfTempName()
*!*	  IF llLink_GL
*!*	    SELECT GLDIST
*!*	    COPY STRUCTURE TO (gcWorkDir+lcGLDstTmp)
*!*	    =gfOpenFile(gcWorkDir+lcGLDstTmp,'','EX')
*!*	    INDEX ON GLaccount TAG lcGLDstTmp
*!*	  ENDIF
*!*	  *B602719,1 Rehan On 04/05/99   *** End   ***
*!*	  
*!*	  lcInvSlLnk = ""      && Var. hold the invoice GL link sales account.
*!*	  lcCstSlLnk = ""      && Var. hold the customer GL link sales account.
*!*	  lcGLFYear  = ""      && Var. hold GL fisacl year.
*!*	  lcGLPeriod = ""      && Var. hold GL fisacl period.
*!*	  lcRmGlSess = ""      && Var. hold the GL session no.
*!*	  llRmGlSess = .F.     && Flag to know if create GL session no. or not.
*!*	  lnPstRate  = 0       && Var. hold the default pst rate for canada.
*!*	  lnPstTotal = 0       && Var. hold the total pst amount.
*!*	  lnInvTrdDs = 0       && Var. hold the trade discount variable.
*!*	  
*!*	  *-- Get the default of Canada tax rule from invoice file if there is invoice no.
*!*	  IF !EMPTY(&lcCrMemHdr..Invoice) .AND. SEEK(&lcCrMemHdr..Invoice , "INVHDR")
*!*	    lcPstRule  = InvHdr.cTaxRule
*!*	    lcInvTerms = INVHDR.cTermCode
*!*	  ELSE
*!*	    lcInvTerms = ""
*!*	    *-- Get the default tax rule from the customer file.
*!*	    IF !EMPTY(&lcCrMemHdr..Account) .AND. SEEK("M"+&lcCrMemHdr..Account , "CUSTOMER")
*!*	      lcPstRule  = Customer.cTaxRule
*!*	    ELSE
*!*	      lcPstRule  = ""
*!*	    ENDIF
*!*	  ENDIF

*!*	  *-- Get the invoice trade discount if it was not defined.
*!*	  lnInvTrdDs = IIF(!EMPTY(&lcCrMemHdr..Invoice) .AND. ;
*!*	               SEEK(&lcCrMemHdr..Invoice , "INVHDR") , INVHDR.trde_disc , 0)
*!*	  
*!*	  *-- Default the tax rate to the first tax rate.
*!*	  GO TOP IN (lcCrMemLin)
*!*	  lnTaxRate = &lcCrMemLin..Tax_Rate
*!*	ENDIF

*!*	*E301077,4 Reham On 12/27/98   *** Begin ***
*!*	*E301077,4 Open the style dyelot file.
*!*	=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
*!*	*E301077,4 Reham On 12/27/98   *** End   ***

*!*	*B602719,1 Rehan On 04/05/99   *** Begin ***
*!*	*-- Create temp. file to hold the gl distribution records.
*!*	*IF llLink_GL
*!*	*  lcGLDstTmp = gfTempName()
*!*	*  SELECT GLDIST
*!*	*  COPY STRUCTURE TO (gcWorkDir+lcGLDstTmp)
*!*	*  =gfOpenFile(gcWorkDir+lcGLDstTmp,'','EX')
*!*	*  INDEX ON GLaccount TAG lcGLDstTmp
*!*	*ENDIF
*!*	*B602719,1 Rehan On 04/05/99   *** End   ***

*!*	*B602771,1 Reham On 04/11/99   *** Begin ***
*!*	IF llMulCurr
*!*	*B602771,1 Reham On 04/11/99   *** End   ***
*!*	  *-- Get exchange rate sign and unit sign.
*!*	  lcUntSin = ' '
*!*	  lcExRSin = gfGetExSin(@lcUntSin , &lcCrMemHdr..cCurrCode)
*!*	*B602771,1 Reham On 04/11/99   *** Begin ***
*!*	ELSE
*!*	  lcUntSin   = '/'        &&  Variable to hold unit sign.
*!*	  lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
*!*	ENDIF
*!*	*B602771,1 Reham On 04/11/99   *** End   ***

*!*	*-- Validate the posting date if called from point of sale program & one of
*!*	*-- GL fiscal year or period is empty.
*!*	IF llPontSale .AND. (EMPTY(lcGLFYear) .OR. EMPTY(lcGLPeriod))
*!*	  =CheckPrd(&lcCrMemHdr..dPostDate , 'lcGLFYear' , 'lcGLPeriod' , 'RM' , .T.)
*!*	ENDIF

*!*	*-- Get the year & period for the transaction date.
*!*	STORE "" TO lcTrYear , lcTrPeriod
*!*	=CHECKPRD(&lcCrMemHdr..CrDate , 'lcTrYear' , 'lcTrPeriod' , 'RM' , .T.)

*!*	*-- Check if the current customer is vat exempted or not from the customer file.
*!*	*B603666,1 Ramy On 06/20/2000   Begin
*!*	*IF !EMPTY(&lcCrMemHdr..Account) .AND. SEEK("M"+&lcCrMemHdr..Account , "CUSTOMER")
*!*	IF !EMPTY(&lcCrMemHdr..Account)
*!*	  IF !EMPTY(&lcCrMemHdr..STORE) .AND. SEEK("S"+&lcCrMemHdr..Account+&lcCrMemHdr..Store , "CUSTOMER")
*!*	    llVatExem  = CUSTOMER.lVatExem
*!*	  ELSE
*!*	    IF SEEK("M"+&lcCrMemHdr..Account , "CUSTOMER")
*!*	      llVatExem  = CUSTOMER.lVatExem
*!*	    ENDIF
*!*	  ENDIF
*!*	  *B603666,1 Ramy On 06/20/2000   End
*!*	  
*!*	  *B602688,1 Reham On 03/18/99   *** Begin ***
*!*	  *lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
*!*	  *B602688,1 Reham On 03/18/99   *** End   ***
*!*	ELSE
*!*	  llVatExem  = .F.
*!*	  *B602688,1 Reham On 03/18/99   *** Begin ***
*!*	  *lcCstSlLnk = "DEF"
*!*	  *B602688,1 Reham On 03/18/99   *** End   ***
*!*	ENDIF

*!*	*B602688,1 Reham On 03/18/99   *** Begin ***
*!*	IF llLink_GL
*!*	  *B602688,1 If Gl is set at division level, get GL Link code from division.
*!*	  IF llDiv_Link .AND. !EMPTY(&lcCrMemHdr..cDivision)
*!*	    DECLARE laDRltFld[2,2]
*!*	    laDRltFld[1,1] = 'LINK_CODE'
*!*	    laDRltFld[1,2] = 'lcCustLink'
*!*	    laDRltFld[2,1] = 'CSLSGLLINK'
*!*	    laDRltFld[2,2] = 'lcCstSlLnk'
*!*	    
*!*	    =gfRltFld(&lcCrMemHdr..cDivision , @laDRltFld,'CDIVISION')
*!*	    lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
*!*	  ELSE
*!*	    lcCstSlLnk = "DEF"
*!*	  ENDIF
*!*	ELSE
*!*	  lcCstSlLnk = ""
*!*	ENDIF
*!*	*B602688,1 Reham On 03/18/99   *** Begin ***

*!*	*-- Call local function to get the trade discount.
*!*	lnInvTrdDs = lfGetTrdDs(&lcCrMemHdr..cTermCode , &lcCrMemHdr..Invoice )

*!*	SELECT (lcCrMemHdr)
*!*	*-- Lock the current header record to grantee the phiscal update.
*!*	=RLOCK()
*!*	*-- Calculate the salesreps. charge back.
*!*	IF lnInvTrdDs <> 0
*!*	  REPLACE CommAmt1 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
*!*	          CommAmt2 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
*!*	ELSE
*!*	  REPLACE CommAmt1 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
*!*	          CommAmt2 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
*!*	ENDIF
*!*	UNLOCK

*!*	*-- Calculate taxes for England.
*!*	*IF llIsEnglnd
*!*	*  lnEngTax = lfTotalTax()
*!*	*  SELECT (lcCrMemHdr)
*!*	*  *-- Lock the current record in the header file to grantee the phiscal update.
*!*	*  =RLOCK()
*!*	*  REPLACE Tax_Amt WITH lnEngTax
*!*	*  UNLOCK
*!*	*ENDIF

*!*	*-- Define temp. var. to hold the calculated taxes.
*!*	*lnCurTax   = &lcCrMemHdr..Tax_Amt
*!*	IF llTax
*!*	  *IF lcTax_Meth = 'A'
*!*	    *-- Calculate the tax amount if the company is "England" & the RA 
*!*	    *-- related to the invoice, otherwise the program does not Calculate
*!*	    *-- the tax amount.
*!*	  *  IF llIsEnglnd
*!*	  *    *-- Changed the way of calculating tax amount.
*!*	  *    IF !EMPTY(&lcCrMemHdr..Invoice) .AND. &lcCrMemHdr..cIntr_Inv = "Y"
*!*	  *      lnCurTax = lnCurTax + lfchgTax()
*!*	  *    ENDIF
*!*	  *  ELSE
*!*	  *    IF !llIsCanada
*!*	  *      *lnCurTax = lnCurTax + &lcCrMemHdr..Other * (lnTaxRate/100)
*!*	  *    ENDIF
*!*	  *  ENDIF
*!*	  *ENDIF
*!*	  
*!*	  *-- To display the total PST tax amount if the country was canada.
*!*	  IF llIsCanada
*!*	    SELECT (lcCrMemLin)
*!*	    SUM Amount * (nPstRate/100) , Amount * (Tax_Rate/100) ;
*!*	        FOR !EMPTY(Style) TO lnPstTotal , lnTot_Tax
*!*	    lnPstTotal = lnPstTotal + &lcCrMemHdr..Other * (lnPstRate/100)
*!*	    *-- Take PST due to rule.
*!*	    IF VAL(ALLTRIM(lcPstRule)) <> 1
*!*	      lnPstTotal = lnPstTotal + lnTot_Tax * (lnPstRate/100)
*!*	    ENDIF
*!*	    SELECT (lcCrMemHdr)
*!*	    *-- Lock the current header record to grantee the phiscal update.
*!*	    =RLOCK()
*!*	    *-- Update the tax field.
*!*	    REPLACE Tax_Amt  WITH lnTot_Tax
*!*	    UNLOCK
*!*	  ENDIF
*!*	ENDIF

*!*	SELECT (lcCrMemLin)
*!*	*-- Calculate the totals for the following:
*!*	*-- Qty, Amount, discount amount, trade discount amount.
*!*	SUM ALL TotQty , (Gros_Price*TotQty)*(1-(Disc_Pcnt/100)) , ;
*!*	        (TotQty*Gros_Price) , Disc_amt , Trde_Amt FOR !EMPTY(Style) ;
*!*	        TO lnTPieces , lnTAmount , lnTGrosAmt , lnTDiscAmt , lnTTrdeAmt

*!*	lnPstTotal = IIF(llTax .AND. llIsCanada , lnPstTotal , 0)

*!*	SELECT (lcCrMemHdr)
*!*	*-- Lock the current record to grantee the phisacl update.
*!*	=RLOCK()
*!*	REPLACE Pieces    WITH lnTPieces  ;
*!*	        Gross_Amt WITH lnTGrosAmt ;
*!*	        Disc_Amt  WITH lnTDiscAmt ;
*!*	        Amount    WITH lnTAmount  ;
*!*	        Tax_Amt   WITH IIF(llTax , Tax_Amt , 0) ;
*!*	        nPstAmt   WITH lnPstTotal ;
*!*	        TotCredit WITH Other + Amount + Tax_Amt + nPstAmt
*!*	UNLOCK

*!*	IF llLink_GL
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *B602719,1 Check the header steps that done.
*!*	  IF &lcCrMemHdr..nSteps < 2
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	        
*!*	    *-- Call GL Distribution Procedure To Add The Following 
*!*	    *-- Accounts For This Return.
*!*	    *B602688,1 Reham On 03/21/99   *** Begin ***
*!*	    *B602688,1 Send the frieght account to the function.
*!*	    *** 1) FREIGHT + INSURANCE + COD   <DEBIT>  FREIGHT INCOME
*!*	    *** CATEGORY KEY FOR "Freight"................=> '004'
*!*	    *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004',&lcCrMemHdr..OTHER , 'RM' ,;
*!*	                   &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
*!*	                   lcGLFYear,lcGLPeriod , '&lcGLDstTmp' , '' ,;
*!*	                   &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
*!*	                   &lcCrMemHdr..nExRate
*!*	    DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004',&lcCrMemHdr..OTHER , 'RM' ,;
*!*	                   &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
*!*	                   lcGLFYear,lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cFrgtAcnt ,;
*!*	                   &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
*!*	                   &lcCrMemHdr..nExRate
*!*	    *B602688,1 Reham On 03/21/99   *** End   ***
*!*	    
*!*	    SELECT (lcCrMemHdr)
*!*	    *-- Lock the record to grantee the phiscal update.
*!*	    = RLOCK()
*!*	    *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	    IF &lcCrMemHdr..OTHER <> 0
*!*	      REPLACE cFrgtAcnt WITH &lcGLDstTmp..glaccount
*!*	    ENDIF
*!*	    *B602719,1 Svae the header steps that done.
*!*	    REPLACE nSteps    WITH 2
*!*	    *B602719,1 Reham On 04/05/99   *** End   ***
*!*	    UNLOCK
*!*	  ENDIF
*!*	  
*!*	  *-- If The System Has Been Setup To Compute Tax.
*!*	  IF llTax

*!*	    *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	    *B602719,1 Check the header steps that done.
*!*	    IF &lcCrMemHdr..nSteps < 3
*!*	    *B602719,1 Reham On 04/05/99   *** End   ***
*!*	      *B602688,1 Reham On 03/21/99   *** Begin ***
*!*	      *B602688,1 Send the tax account to the function.
*!*	      *** 2) SALES TAX LIABILITY       <DEBIT>  TAX INCOME
*!*	      *** CATEGORY KEY FOR "Sales tax liability"..=> '014'
*!*	      *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
*!*	                     &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt , 'RM' ,;
*!*	                     &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
*!*	                     lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , '' ,;
*!*	                     &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
*!*	                     &lcCrMemHdr..nExRate
*!*	      DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
*!*	                     &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt , 'RM' ,;
*!*	                     &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
*!*	                     lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
*!*	                     &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
*!*	                     &lcCrMemHdr..nExRate
*!*	      *B602688,1 Reham On 03/21/99   *** End   ***
*!*	      
*!*	      SELECT (lcCrMemHdr)
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	      IF (&lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt) <> 0
*!*	        REPLACE cTaxAcnt WITH &lcGLDstTmp..glaccount
*!*	      ENDIF
*!*	      REPLACE nSteps   WITH 3
*!*	      *B602719,1 Reham On 04/05/99   *** End   ***
*!*	      UNLOCK
*!*	    ENDIF
*!*	  ENDIF
*!*	  
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *B602719,1 Check the header steps that done.
*!*	  IF &lcCrMemHdr..nSteps < 4
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	    *B602688,1 Reham On 03/21/99   *** Begin ***
*!*	    *B602688,1 Send the AR account to the function.
*!*	    *** 3) TOTAL CHARGING              <CREDIT>  ACCOUNT RECEIVABLE
*!*	    *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
*!*	    *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '001' , -(&lcCrMemHdr..TOTCREDIT),;
*!*	                   'RM' , &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
*!*	                   lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , '' ,;
*!*	                   &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
*!*	                   &lcCrMemHdr..nExRate
*!*	    DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '001' , -(&lcCrMemHdr..TOTCREDIT),;
*!*	                   'RM' , &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
*!*	                   lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cARAcnt ,;
*!*	                   &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
*!*	                   &lcCrMemHdr..nExRate
*!*	    *B602688,1 Reham On 03/21/99   *** End   ***
*!*	    
*!*	    SELECT (lcCrMemHdr)
*!*	    *-- Lock the record to grantee the phiscal update.
*!*	    = RLOCK()
*!*	    *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	    IF &lcCrMemHdr..TOTCREDIT <> 0
*!*	      REPLACE cArAcnt WITH &lcGLDstTmp..glaccount
*!*	    ENDIF
*!*	    REPLACE nSteps  WITH 4
*!*	    *B602719,1 Reham On 04/05/99   *** End   ***
*!*	    UNLOCK
*!*	  ENDIF
*!*	ENDIF

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*B602719,1 Check the header steps that done.
*!*	*IF &lcCrMemHdr..nSteps < 2
*!*	IF &lcCrMemHdr..nSteps < 5
*!*	*B602719,1 Reham On 04/05/99   *** End   ***
*!*	  SELECT (lcCrMemHdr)
*!*	  SCATTER MEMVAR MEMO
*!*	  
*!*	  *-- Save the current credit memo to the master return header file.
*!*	  SELECT RETHDR
*!*	  APPEND BLANK
*!*	  *-- Lock the record to grantee the phiscal update.
*!*	  = RLOCK()
*!*	  GATHER MEMVAR MEMO
*!*	  
*!*	  *-- Call global function to add audit fields info.
*!*	  =gfAdd_Info('RETHDR')
*!*	  UNLOCK
*!*	  
*!*	  *-- Call Global Function to transmit the local data.
*!*	  =gfTraceKey("RetHdr" , RetHdr.CRMEMO , "A")
*!*	  
*!*	  SELECT (lcCrMemHdr)
*!*	  *-- Lock the record to grantee the phiscal update.
*!*	  = RLOCK()
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *REPLACE nSteps WITH 2
*!*	  REPLACE nSteps WITH 5
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	  UNLOCK
*!*	ENDIF

*!*	*-- Do not call the notepad program if coming from the point of sale program.
*!*	IF !llPontSale
*!*	  *B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
*!*	  *                         Will Call it
*!*	  **-- If add notes for the current credit memo.
*!*	  **** Do you want to add notes for the current cedit memo? ***
*!*	  **** < Yes > - < No > ***
*!*	  *IF gfModalGen("QRM00271B00006" , "DIALOG") = 1
*!*	  *  *E301077,4 Reham On 12/17/98   *** Begin ***
*!*	  *  *E301077,4 Open the note pad file.
*!*	  *  =gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
*!*	  *  *E301077,4 Reham On 12/17/98   *** End   ***
*!*	    
*!*	  *  =NOTEPAD('R',&lcCrMemHdr..CrMemo)
*!*	  *  *-- Call Global Function to transmit the local data.
*!*	  *  =gfTraceKey("NotePad" , "R"+&lcCrMemHdr..CrMemo , "A")
*!*	    
*!*	  *  *E301077,4 Reham On 12/17/98   *** Begin ***
*!*	  *  =gfCloseFile('NotePad')
*!*	  *  *E301077,4 Reham On 12/17/98   *** End   ***
*!*	  *ENDIF
*!*	  *B602753,1 HDM 04/07/1999[End]
*!*	ENDIF

*!*	SELECT (lcCrMemLin)
*!*	*-- Lock the record to grantee the phiscal update.
*!*	= RLOCK()

*!*	*B802907,1 Reham On 12/26/1999    *** Begin ***
*!*	*-- Delete all empty styles records.
*!*	*DELETE ALL FOR EMPTY(Style)
*!*	DELETE ALL FOR EMPTY(Style) .OR. (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8) = 0
*!*	*B802907,1 Reham On 12/26/1999    *** End   ***
*!*	UNLOCK

*!*	*-- Flag to know if cancel the difference between 
*!*	*-- the open qty. & the received qty.
*!*	llCancRest = .F.

*!*	*B802907,1 Reham On 12/26/1999   *** Begin ***
*!*	=lfCrtUnComp(.F. , .F. , .F. , .T.)
*!*	SELECT (lcCrMemLin)
*!*	GO TOP
*!*	DECLARE laOrgQty[8] , laOpnLin[8]
*!*	SCAN
*!*	  STORE 0 TO laOpnLin , laOrgQty
*!*	  SCATTER MEMO TO MEMVAR
*!*	  SELECT (lcOpenLine)
*!*	  
*!*	  *B603741,1 WAB - add the Cret-linNo in the seek expression so it can add new line if the style 
*!*	  *B603741,1 WAB - is dublicated
*!*	  *B603741,1 WAB - StART
*!*	  *IF !SEEK(&lcCrMemLin..ACCOUNT+&lcCrMemLin..STYLE , lcOpenLine)
*!*	  IF !SEEK(&lcCrMemLin..ACCOUNT+&lcCrMemLin..STYLE+&lcCrMemLin..Cret_LinNo, lcOpenLine)
*!*	  *B603741,1 WAB - end
*!*	    APPEND BLANK
*!*	  ENDIF
*!*	  SCATTER FIELDS Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 TO laOrgQty
*!*	*  SCATTER FIELDS nOpnQty1 , nOpnQty2 , nOpnQty3 , nOpnQty4 , nOpnQty5 , nOpnQty6 , nOpnQty7 , nOpnQty8 TO laOpnLin
*!*	  GATHER MEMO FROM MEMVAR
*!*	  FOR lnOpn = 1 TO 8
*!*	    lcOpn = ALLTRIM(STR(lnOpn))
*!*	*    REPLACE Qty&lcOpn     WITH Qty&lcOpn + laOrgQty[lnOpn] ;
*!*	*            nOpnQty&lcOpn WITH nOpnQty&lcOpn + laOpnLin[lnOpn]
*!*	    REPLACE Qty&lcOpn     WITH Qty&lcOpn + laOrgQty[lnOpn]
*!*	  ENDFOR
*!*	*  REPLACE TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
*!*	*          nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
*!*	  REPLACE TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
*!*	  SELECT (lcCrMemLin)
*!*	ENDSCAN
*!*	SELECT (lcOpenLine)
*!*	*B802907,1 Reham On 12/26/1999   *** End   ***

*!*	*-- Locate for any open qty in all the lines & ask the user to leave it
*!*	*-- open or cancel the difference between the open & the received.
*!*	LOCATE FOR Qty1 < nOpnQty1 .OR. Qty2 < nOpnQty2 .OR. Qty3 < nOpnQty3 .OR. ;
*!*	           Qty4 < nOpnQty4 .OR. Qty5 < nOpnQty5 .OR. Qty6 < nOpnQty6 .OR. ;
*!*	           Qty7 < nOpnQty7 .OR. Qty8 < nOpnQty8
*!*	IF FOUND()
*!*	  *** There is open qty. in the current RA.  Do you want to ***
*!*	  *** receive this RA completely and consider the open qty. ***
*!*	  *** as canceled or leave the RA open and consider the     ***
*!*	  *** previous receive as partially receive?
*!*	  *** < Completely > - < Partially >
*!*	  IF gfModalGen("QRM46009B46000" , "DIALOG") = 1
*!*	    llCancRest = .T.
*!*	  ENDIF
*!*	ENDIF

*!*	*B802907,1 Reham On 12/26/1999   *** Begin ***
*!*	SELECT (lcCrMemLin)
*!*	*B802907,1 Reham On 12/26/1999   *** End   ***

*!*	lnLineCnt  = 1    && Var. hold line counter.

*!*	lnTotCancl = 0    && Var. hold total qty. canceled.

*!*	*!B602322,1 Moved-Read the gl session if not readed to be used in GfStyclr.
*!*	IF !llRmGlSess
*!*	  lcRmGlSess = gfsequence('GLSESSION')
*!*	  llRmGlSess = .T.
*!*	ENDIF
*!*	*!B602322,1 End.

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*-- Call local function to update the temp. files & needed variables 
*!*	*-- fields in the uncompleted session file record.
*!*	llNothing = IIF(llPontSale , .F. , lfUpdVars())
*!*	*B602719,1 Reham On 04/05/99   *** End   ***

*!*	*-- Var. hold the incremental # will be saved in the nSteps field in the
*!*	*-- credit memo line file.
*!*	lnNxtStp   = 0

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*B602719,1 Check the header steps that done.
*!*	*IF &lcCrMemHdr..nSteps < 3
*!*	IF &lcCrMemHdr..nSteps < 6
*!*	*B602719,1 Reham On 04/05/99   *** End   ***
*!*	  lnCurRec  = 1                     && Var. hold current record no.
*!*	  lnTotRec  = RECCOUNT(lcCrMemLin)  && Var. hold total lines record count.

*!*	  *B603741,1 WAB - in case of !calling from POS set the order to 2nd tag (account+Cret_LinNo+style)
*!*	  *B603741,1 WAB - cause in some case if we used the 1st tag (account+style+Cret-linNo+cret_trncd) 
*!*	  *B603741,1 WAB - and we have dublicate style/color in 2 diffrent lines during the save 1st record 
*!*	  *B603741,1 WAB - get his Crmemo # so they rotate their order the 2nd one did not get his Crmemo# 
*!*	  *B603741,1 WAB - so the prog didn't save this line in the mastr file.
*!*	  *B603741,1 WAB - StART
*!*	  IF !llPontSale
*!*	    lcCurrtag= ORDER()
*!*	    SET ORDER TO TAG lcCrMmLine 
*!*	  ENDIF
*!*	  *B603741,1 WAB - END
*!*	  
*!*	  SCAN FOR !EMPTY(STYLE)
*!*	    *B602732,1 Reham On 04/04/99   *** Begin ***
*!*	    *B602732,1 Blank the steps counter for the current line
*!*	    lnNxtStp = 0
*!*	    *B602732,1 Reham On 04/04/99   *** End   ***
*!*	    
*!*	    *-- Call the global function that execute the thermometer.
*!*	    =gfTherm(lnTotRec,lnCurRec,"Saving credit memo # : "+&lcCrMemHdr..CrMemo)
*!*	    lnCurRec = lnCurRec + 1
*!*	    
*!*	    SELECT (lcCrMemLin)
*!*	    *-- Lock the record to grantee the phiscal update.
*!*	    = RLOCK()
*!*	    
*!*	    *B603003,1 Reham On 06/21/1999   *** Begin ***
*!*	    *B603003,1 Do not update the lines numbers in the credit memo lines
*!*	    *B603003,1 in the save procedure if called from point of sale to 
*!*	    *B603003,1 prevent the error of not adding some lines.
*!*	    *REPLACE CRMEMO WITH &lcCrMemHdr..CrMemo ;
*!*	            CRET_LINNO WITH ALLTRIM(STR(lnLineCnt))
*!*	    REPLACE CRMEMO     WITH &lcCrMemHdr..CrMemo ;
*!*	            CRET_LINNO WITH IIF(llPontSale , CRET_LINNO , ALLTRIM(STR(lnLineCnt)))
*!*	    *B603003,1 Reham On 06/21/1999   *** End   ***
*!*	    
*!*	    *-- If there is a R/A no. & the line is already exist in RALINE file.
*!*	    IF !EMPTY(&lcCrMemHdr..RaNo) .AND. SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
*!*	      llRALinExs = .T.    && The current line exist in RALINE file.
*!*	    ELSE
*!*	      llRALinExs = .F.    && The current line does not exist in RALINE file.
*!*	    ENDIF
*!*	    
*!*	    SELECT STYLE
*!*	    *-- If there is return style , get its sales code
*!*	    IF !EMPTY(&lcCrMemLin..cretSty) .AND. SEEK(&lcCrMemLin..cretSty)
*!*	      lcStySlLnk = Style.cslsgllink
*!*	    ELSE
*!*	      lcStySlLnk = ""
*!*	    ENDIF
*!*	    =SEEK(&lcCrMemLin..STYLE)
*!*	    llInvSty = Style.lInvSty
*!*	    
*!*	    *-- If return to the original style, get its sales code.
*!*	    lcStySlLnk = IIF(EMPTY(lcStySlLnk) , Style.cslsgllink , lcStySlLnk )
*!*	    SELECT (lcCrMemLin)
*!*	    REPLACE GL_SALES WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
*!*	    UNLOCK
*!*	    
*!*	    IF &lcCrMemLin..nSteps < 1
*!*	      *-- Now Update The Style Dyelot Inventory For Dyelot 
*!*	      *-- System And Styles That Come In Dyelots.
*!*	      IF llUseDyes .AND. Style.cDye_Flg = 'Y' .AND. llInvSty
*!*	        SELECT STYDYE
*!*	        
*!*	        IF SEEK (&lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT)
*!*	          *-- Lock the record to grantee the phiscal update.
*!*	          = RLOCK()
*!*	          REPLACE Ret1   WITH Ret1 + &lcCrMemLin..Qty1   ;
*!*	                  Ret2   WITH Ret2 + &lcCrMemLin..Qty2   ;
*!*	                  Ret3   WITH Ret3 + &lcCrMemLin..Qty3   ;
*!*	                  Ret4   WITH Ret4 + &lcCrMemLin..Qty4   ;
*!*	                  Ret5   WITH Ret5 + &lcCrMemLin..Qty5   ;
*!*	                  Ret6   WITH Ret6 + &lcCrMemLin..Qty6   ;
*!*	                  Ret7   WITH Ret7 + &lcCrMemLin..Qty7   ;
*!*	                  Ret8   WITH Ret8 + &lcCrMemLin..Qty8   ;
*!*	                  TotRet WITH Ret1+Ret2+Ret3+Ret4+Ret5+Ret6+Ret7+Ret8
*!*	          
*!*	          *-- If this line from RA & exist in RALINE file, Adjust RA fields.
*!*	          IF llRALinExs
*!*	            REPLACE RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1) ;
*!*	                    RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2) ;
*!*	                    RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3) ;
*!*	                    RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4) ;
*!*	                    RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5) ;
*!*	                    RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6) ;
*!*	                    RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7) ;
*!*	                    RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8) ;
*!*	                    TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
*!*	          ENDIF
*!*	          UNLOCK
*!*	          
*!*	          *-- Call Global Function to transmit the local data.
*!*	          =gfTraceKey("StyDye" , &lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT , "M")
*!*	        ENDIF
*!*	      ENDIF
*!*	      SELECT (lcCrMemLin)
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      REPLACE nSteps WITH 1
*!*	      UNLOCK
*!*	    ENDIF
*!*	    
*!*	    IF &lcCrMemLin..nSteps < 2
*!*	      *-- Update the warehouse record in the StyDye file if the
*!*	      *-- system is setup to use the multiple warehouses.
*!*	      SELECT StyDye
*!*	      IF SEEK(&lcCrMemLin..Style + &lcCrMemHdr..cWareCode + SPACE(10))
*!*	        *-- Lock the record to grantee the phiscal update.
*!*	        = RLOCK()
*!*	        IF llInvSty
*!*	          REPLACE Ret1   WITH Ret1 + &lcCrMemLin..Qty1   ;
*!*	                  Ret2   WITH Ret2 + &lcCrMemLin..Qty2   ;
*!*	                  Ret3   WITH Ret3 + &lcCrMemLin..Qty3   ;
*!*	                  Ret4   WITH Ret4 + &lcCrMemLin..Qty4   ;
*!*	                  Ret5   WITH Ret5 + &lcCrMemLin..Qty5   ;
*!*	                  Ret6   WITH Ret6 + &lcCrMemLin..Qty6   ;
*!*	                  Ret7   WITH Ret7 + &lcCrMemLin..Qty7   ;
*!*	                  Ret8   WITH Ret8 + &lcCrMemLin..Qty8   ;
*!*	                  TotRet WITH Ret1+Ret2+Ret3+Ret4+Ret5+Ret6+Ret7+Ret8
*!*	          *-- If this line from RA & exist in RALINE file, Adjust RA fields.
*!*	          IF llRALinExs
*!*	            REPLACE  RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1) ;
*!*	                     RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2) ;
*!*	                     RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3) ;
*!*	                     RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4) ;
*!*	                     RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5) ;
*!*	                     RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6) ;
*!*	                     RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7) ;
*!*	                     RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8) ;
*!*	                     TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
*!*	          ENDIF
*!*	        ENDIF
*!*	        
*!*	        UNLOCK
*!*	        *-- Call Global Function to transmit the local data.
*!*	        =gfTraceKey("StyDye" , &lcCrMemLin..Style + &lcCrMemHdr..cWareCode + SPACE(10) , "M")
*!*	      ENDIF
*!*	      
*!*	      SELECT (lcCrMemLin)
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      REPLACE nSteps WITH 2
*!*	      UNLOCK
*!*	    ENDIF
*!*	    
*!*	    IF &lcCrMemLin..nSteps < 3
*!*	      SELECT STYLE
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      IF llInvSty
*!*	        REPLACE RET1   WITH RET1 + &lcCrMemLin..QTY1 ;
*!*	                RET2   WITH RET2 + &lcCrMemLin..QTY2 ;
*!*	                RET3   WITH RET3 + &lcCrMemLin..QTY3 ;
*!*	                RET4   WITH RET4 + &lcCrMemLin..QTY4 ;
*!*	                RET5   WITH RET5 + &lcCrMemLin..QTY5 ;
*!*	                RET6   WITH RET6 + &lcCrMemLin..QTY6 ;
*!*	                RET7   WITH RET7 + &lcCrMemLin..QTY7 ;
*!*	                RET8   WITH RET8 + &lcCrMemLin..QTY8 ;
*!*	                TOTRET WITH RET1+RET2+RET3+RET4+RET5+RET6+RET7+RET8
*!*	        *-- If this line from RA & exist in RALINE file, Adjust RA fields.
*!*	        IF llRALinExs
*!*	          REPLACE RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1) ;
*!*	                  RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2) ;
*!*	                  RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3) ;
*!*	                  RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4) ;
*!*	                  RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5) ;
*!*	                  RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6) ;
*!*	                  RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7) ;
*!*	                  RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8) ;
*!*	                  TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
*!*	        ENDIF
*!*	      ENDIF
*!*	      UNLOCK
*!*	      *-- Call Global Function to transmit the local data.
*!*	      =gfTraceKey("Style" , &lcCrMemLin..Style , "M")
*!*	      
*!*	      SELECT (lcCrMemLin)
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      *B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	      IF EMPTY(&lcCrMemLin..cRetSty) .AND. Style.cStyGrade <> &lcCrMemLin..cStyGrade
*!*	        REPLACE cStyGrade WITH Style.cStyGrade
*!*	      ENDIF
*!*	      *B802907,1 Reham On 12/28/1999   *** End   ***
*!*	      REPLACE nSteps WITH 3
*!*	      UNLOCK
*!*	    ENDIF
*!*	    
*!*	    *-- Update general ledger entreis
*!*	    IF llLink_GL
*!*	      *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	      IF &lcCrMemLin..nSteps < 4
*!*	      *B602719,1 Reham On 04/06/99   *** End   ***
*!*	        *B602688,1 Reham On 03/21/99   *** Begin ***
*!*	        *B602688,1 Send the sales account to the function.
*!*	        *-- Post to Return Merchandise
*!*	        *-- 1) TOTAL GROSS MERCHANDISE         <DEBIT>  Return Merchandise
*!*	        *-- CATEGORY KEY FOR "Return Merchaandise....=> '020'
*!*	        *DO GLDIST WITH &lcCrMemLin..GL_Sales , '020' , ;
*!*	                       &lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price , 'RM', ;
*!*	                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
*!*	                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', '' , ;
*!*	                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
*!*	                       &lcCrMemHdr..nExRate
*!*	        DO GLDIST WITH &lcCrMemLin..GL_Sales , '020' , ;
*!*	                       &lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price , 'RM', ;
*!*	                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
*!*	                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', &lcCrMemLin..cSalesAcnt , ;
*!*	                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
*!*	                       &lcCrMemHdr..nExRate
*!*	        *B602688,1 Reham On 03/21/99   *** End   ***
*!*	      
*!*	        *-- Lock the record to grantee the phiscal update.
*!*	        = RLOCK()
*!*	        *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	        IF (&lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price) <> 0
*!*	          REPLACE &lcCrMemLin..cSalesAcnt WITH &lcGLDstTmp..glaccount
*!*	        ENDIF
*!*	        REPLACE &lcCrMemLin..nSteps     WITH 4
*!*	        *B602719,1 Reham On 04/06/99   *** End ***
*!*	        UNLOCK
*!*	      ENDIF
*!*	      
*!*	      *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	      IF &lcCrMemLin..nSteps < 5
*!*	      *B602719,1 Reham On 04/06/99   *** End   ***
*!*	        *B602688,1 Reham On 03/21/99   *** Begin ***
*!*	        *B602688,1 Send the discount account to the function.
*!*	        *-- 2) DISCOUNT AMOUNT                 <CREDIT>  SALES DISCOUNT
*!*	        *-- CATEGORY KEY FOR "Discount"...............=> '005'
*!*	        *DO GLDIST WITH &lcCrMemLin..GL_Sales , '005' , ;
*!*	                       -(&lcCrMemLin..DISC_AMT),'RM', ;
*!*	                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
*!*	                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', '' , ;
*!*	                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
*!*	                       &lcCrMemHdr..nExRate
*!*	        DO GLDIST WITH &lcCrMemLin..GL_Sales , '005' , ;
*!*	                       -(&lcCrMemLin..DISC_AMT),'RM', ;
*!*	                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
*!*	                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', &lcCrMemLin..cDiscAcnt , ;
*!*	                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
*!*	                       &lcCrMemHdr..nExRate
*!*	        *B602688,1 Reham On 03/21/99   *** End   ***
*!*	        
*!*	        *-- Lock the record to grantee the phiscal update.
*!*	        = RLOCK()
*!*	        *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	        IF &lcCrMemLin..DISC_AMT <> 0
*!*	          REPLACE &lcCrMemLin..cDiscAcnt WITH &lcGLDstTmp..glaccount
*!*	        ENDIF
*!*	        REPLACE &lcCrMemLin..nSteps   WITH 5
*!*	        *B602719,1 Reham On 04/06/99   *** End   ***
*!*	        UNLOCK
*!*	      ENDIF
*!*	    ENDIF
*!*	    
*!*	    SELECT (lcCrMemLin)
*!*	    IF llLink_GL
*!*	      *B602688,1 Reham On 06/13/1999   *** Begin ***
*!*	      =SEEK(&lcCrMemLin..Gl_Sales+'008','GL_LINK')
*!*	      REPLACE cCOGSAcnt WITH GL_LINK.GlAcnt
*!*	      =SEEK(&lcCrMemLin..Gl_Cost+'006','GL_LINK')
*!*	      REPLACE cICAcnt   WITH GL_LINK.GlAcnt
*!*	      *B602688,1 Reham On 06/13/1999   *** End   ***
*!*	      
*!*	      DECLARE laGLDistAr[2,13]
*!*	      *B602688,1 Reham On 03/21/99   *** Begin ***
*!*	      *STORE &lcCrMemLin..GL_Cost TO laGLDistAr[1,1] , laGLDistAr[2,1]
*!*	      laGLDistAr[1,1] = &lcCrMemLin..GL_Cost
*!*	      laGLDistAr[2,1] = &lcCrMemLin..GL_Sales
*!*	      *B602688,1 Send the related account to the function.
*!*	      laGLDistAr[1,10] = &lcCrMemLin..cICAcnt
*!*	      laGLDistAr[2,10] = &lcCrMemLin..cCOGSAcnt
*!*	      *B602688,1 Reham On 03/21/99   *** Begin ***
*!*	      
*!*	      laGLDistAr[1,2] = '006'
*!*	      laGLDistAr[2,2] = '008'
*!*	      laGLDistAr[1,3] =  1
*!*	      laGLDistAr[2,3] = -1
*!*	      STORE 'RM' TO laGLDistAr[1,4],laGLDistAr[2,4]
*!*	      STORE &lcCrMemLin..CrMemo    TO laGLDistAr[1,5],laGLDistAr[2,5]
*!*	      STORE &lcCrMemHdr..dPostDate TO laGLDistAr[1,6],laGLDistAr[2,6]
*!*	      STORE lcGLFYear              TO laGLDistAr[1,7],laGLDistAr[2,7]
*!*	      STORE lcGlPeriod             TO laGLDistAr[1,8],laGLDistAr[2,8]
*!*	      STORE lcGLDstTmp             TO laGLDistAr[1,9],laGLDistAr[2,9]
*!*	    ELSE
*!*	      DIME laGLDistAr[1,1]
*!*	      laGLDistAr = ''
*!*	    ENDIF
*!*	    
*!*	    *B602732,1 Reham On 04/04/99   *** Begin ***
*!*	    *B602732,1 Get the steps counter from the current line
*!*	    lnNxtStp = &lcCrMemLin..nSteps + 1
*!*	    *B602732,1 Reham On 04/04/99   *** End   ***

*!*	    SELECT (lcCrMemLin)
*!*	    DECLARE laAdjQty[9]
*!*	    FOR lnCount = 1 TO 8
*!*	      lcCount = STR(lnCount,1)
*!*	      laAdjQty[lnCount] = Qty&lcCount
*!*	    ENDFOR
*!*	    laAdjQty[9] = TotQty
*!*	    
*!*	    *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	    IF &lcCrMemLin..nSteps < 6
*!*	    *B602719,1 Reham On 04/06/99   *** End   ***
*!*	      *--Call the global function for update style inventory control.
*!*	      *B602322,1 Pass gl session as a parameter.
*!*	      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
*!*	                          &lcCrMemHdr..cWareCode,Dyelot,;
*!*	                          CrDate,CrMemo,@laAdjQty,Cost,'',.T.,'',;
*!*	                          4,lcCrMemLin,'nSteps',@laGLDistAr)

*!*	      *E301200 (AAMER) Start
*!*	      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
*!*	                          &lcCrMemHdr..cWareCode,Dyelot,;
*!*	                          CrDate,CrMemo,@laAdjQty,Cost,'',lcRmGlSess,'',;
*!*	                          4,lcCrMemLin,'nSteps',@laGLDistAr)

*!*	      *Pass lnNxtStp instead of 4 (AAMER) 04/19/999 (Start)
*!*	      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
*!*	                          &lcCrMemHdr..cWareCode,Dyelot,;
*!*	                          CrDate,CrMemo,@laAdjQty,Cost,'',lcRmGlSess,'',;
*!*	                          4,lcCrMemLin,'nSteps',@laGLDistAr,;
*!*	                          VAL(&lcCrMemLin..cRet_LinNo))

*!*	      *E301192 AAMER (Start) 
*!*	      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
*!*	                          &lcCrMemHdr..cWareCode,Dyelot,;
*!*	                          CrDate,CrMemo,@laAdjQty,Cost,'',lcRmGlSess,'',;
*!*	                          lnNxtStp,lcCrMemLin,'nSteps',@laGLDistAr,;
*!*	                          VAL(&lcCrMemLin..cRet_LinNo))
*!*	      PRIVATE lcRefer
*!*	      lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
*!*	      lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
*!*	                          &lcCrMemHdr..cWareCode,Dyelot,;
*!*	                          CrDate,CrMemo,@laAdjQty,Cost,lcRefer,lcRmGlSess,'',;
*!*	                          lnNxtStp,lcCrMemLin,'nSteps',@laGLDistAr,;
*!*	                          VAL(&lcCrMemLin..cRet_LinNo))
*!*	      *E301192 AAMER (End)

*!*	      *Pass lnNxtStp instead of 4 (AAMER) 04/19/999 (End)
*!*	      *E301200 (AAMER) End
*!*	      *B602322,1 end.
*!*	      
*!*	      *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      REPLACE &lcCrMemLin..nSteps WITH lnNxtStp
*!*	      UNLOCK
*!*	    ENDIF
*!*	    
*!*	    IF &lcCrMemLin..nSteps < lnNxtStp + 1
*!*	    *B602719,1 Reham On 04/06/99   *** End ***
*!*	    
*!*	      *-- Update the GL accounts in the credit memo line temp. file.
*!*	      IF llLink_GL
*!*	        *-- Update cost of goods sold account in the credit memo line
*!*	        *-- temp. file from the gl distribution temp. file.
*!*	        SELECT (lcGLDstTmp)
*!*	        LOCATE FOR Tran_Type = "RM" AND Catg_Key = "008"
*!*	        IF FOUND()
*!*	          REPLACE &lcCrMemLin..cCoGsAcnt WITH GLAccount
*!*	        ENDIF
*!*	        
*!*	        *-- Update inventory control account in the credit memo line
*!*	        *-- temp. file from the gl distribution temp. file.
*!*	        LOCATE FOR Tran_Type = "RM" AND Catg_Key = "006"
*!*	        IF FOUND()
*!*	          REPLACE &lcCrMemLin..cICAcnt WITH GLAccount
*!*	        ENDIF
*!*	      ENDIF
*!*	      
*!*	      *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      REPLACE &lcCrMemLin..nSteps WITH lnNxtStp
*!*	      UNLOCK
*!*	    ENDIF
*!*	    *B602719,1 Reham On 04/06/99   *** End   ***
*!*	    
*!*	    *B602719,1 Reham On 04/06/99   *** Begin  ***
*!*	    *IF &lcCrMemLin..nSteps < lnNxtStp + 1
*!*	    IF &lcCrMemLin..nSteps < lnNxtStp + 2
*!*	    *B602719,1 Reham On 04/06/99   *** End   ***
*!*	      *-- Save the current line from the temp. file lines to 
*!*	      *-- the master return line file.
*!*	      SELECT (lcCrMemLin)
*!*	      SCATTER MEMVAR MEMO
*!*	      SELECT RETLINE
*!*	      APPEND BLANK
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      GATHER MEMVAR MEMO
*!*	      *-- Call global function to add audit fields info.
*!*	      =gfAdd_Info('RETLINE')
*!*	      UNLOCK
*!*	      
*!*	      *-- Call Global Function to transmit the local data.
*!*	      =gfTraceKey("RetLine" , CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD , "A")

*!*	      *-- If cancel the rest of open qty.
*!*	      IF llCancRest
*!*	        SELECT (lcCrMemLin)
*!*	        IF Qty1 < nOpnQty1 .OR. Qty2 < nOpnQty2 .OR. Qty3 < nOpnQty3 .OR. ;
*!*	           Qty4 < nOpnQty4 .OR. Qty5 < nOpnQty5 .OR. Qty6 < nOpnQty6 .OR. ;
*!*	           Qty7 < nOpnQty7 .OR. Qty8 < nOpnQty8
*!*	          lnLineCnt = lnLineCnt + 1
*!*	          SCATTER MEMVAR MEMO
*!*	          *-- Create record in the R/A lines with status cancel has the 
*!*	          *-- difference between the open qty. & returned qty.
*!*	          SELECT RETLINE
*!*	          APPEND BLANK
*!*	          *-- Lock the record to grantee the phiscal update.
*!*	          = RLOCK()
*!*	          GATHER MEMVAR MEMO
*!*	          REPLACE CRET_TRNCD WITH "4" ;
*!*	                  CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
*!*	                  QTY1       WITH &lcCrMemLin..nOpnQty1 - &lcCrMemLin..QTY1 ;
*!*	                  QTY2       WITH &lcCrMemLin..nOpnQty2 - &lcCrMemLin..QTY2 ;
*!*	                  QTY3       WITH &lcCrMemLin..nOpnQty3 - &lcCrMemLin..QTY3 ;
*!*	                  QTY4       WITH &lcCrMemLin..nOpnQty4 - &lcCrMemLin..QTY4 ;
*!*	                  QTY5       WITH &lcCrMemLin..nOpnQty5 - &lcCrMemLin..QTY5 ;
*!*	                  QTY6       WITH &lcCrMemLin..nOpnQty6 - &lcCrMemLin..QTY6 ;
*!*	                  QTY7       WITH &lcCrMemLin..nOpnQty7 - &lcCrMemLin..QTY7 ;
*!*	                  QTY8       WITH &lcCrMemLin..nOpnQty8 - &lcCrMemLin..QTY8 ;
*!*	                  TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
*!*	          
*!*	          *-- Call global function to add audit fields info.
*!*	          =gfAdd_Info('RETLINE')
*!*	          UNLOCK
*!*	          
*!*	          *-- Call Global Function to transmit the local data.
*!*	          =gfTraceKey("RetLine" , CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD , "A")
*!*	          
*!*	          *-- Calculate the canceled qty.
*!*	          lnTotCancl = lnTotCancl + RETLINE.TotQty
*!*	        ENDIF
*!*	      ENDIF
*!*	      SELECT (lcCrMemLin)
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	      *REPLACE nSteps WITH lnNxtStp + 1
*!*	      REPLACE nSteps WITH lnNxtStp + 2
*!*	      *B602719,1 Reham On 04/06/99   *** End   ***
*!*	      UNLOCK
*!*	    ENDIF
*!*	    
*!*	    *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	    *IF &lcCrMemLin..nSteps < lnNxtStp + 2
*!*	    IF &lcCrMemLin..nSteps < lnNxtStp + 3
*!*	    *B602719,1 Reham On 04/06/99   *** End   ***
*!*	      IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)
*!*	        *E301077,4 Reham On 12/27/98   *** Begin ***
*!*	        *E301077,4 Open the style history file.
*!*	        =gfOpenFile(gcDataDir+'ICSTYHST',gcDataDir+'STYHST','SH')
*!*	        *E301077,4 Reham On 12/27/98   *** End   ***
*!*	        
*!*	        lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )
*!*	        
*!*	        *-- Update the style history file for current line.
*!*	        SELECT ICSTYHST
*!*	        IF SEEK(&lcCrMemLin..Style + lcTrYear , "ICSTYHST")
*!*	          *-- Lock the record to grantee the phiscal update.
*!*	          = RLOCK()
*!*	          REPLACE nRetAmt            WITH nRetAmt + ROUND(&lcCrMemLin..Amount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
*!*	                  nRetQty            WITH nRetQty + &lcCrMemLin..TOTQTY ;
*!*	                  nDisAmt            WITH nDisAmt - ROUND(&lcCrMemLin..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
*!*	                  nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod + ROUND(&lcCrMemLin..Amount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
*!*	                  nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod + &lcCrMemLin..TOTQTY ;
*!*	                  nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod - ROUND(&lcCrMemLin..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
*!*	          UNLOCK
*!*	          
*!*	          *-- Call Global Function to transmit the local data.
*!*	          =gfTraceKey("ICSTYHST" , &lcCrMemLin..Style + lcTrYear , "M")
*!*	        ENDIF
*!*	      ENDIF
*!*	      SELECT (lcCrMemLin)
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	      *REPLACE nSteps WITH lnNxtStp + 2
*!*	      REPLACE nSteps WITH lnNxtStp + 3
*!*	      *B602719,1 Reham On 04/06/99   *** End   ***
*!*	      UNLOCK
*!*	    ENDIF
*!*	    
*!*	    lnLineCnt  = lnLineCnt  + 1
*!*	    SELECT (lcCrMemLin)
*!*	  ENDSCAN

*!*	  *B603741,1 WAB - Return the order to the 1st tag (account+style+Cret-linNo+cret_trncd)
*!*	  *B603741,1 WAB - StART
*!*	  IF !llPontSale
*!*	    SELECT (lcCrMemLin)
*!*	    SET ORDER TO TAG lcCurrtag
*!*	  ENDIF
*!*	  *B603741,1 WAB - END

*!*	  *-- If the thermometer was not closed, call global function to close it.
*!*	  *B802736,1 [Start]the Current RecCount lnCurRec always exceeds the real
*!*	  *                 reccount by one (MAN)
*!*	  *IF lnCurRec # lnTotRec
*!*	  IF lnCurRec-1 # lnTotRec
*!*	  *B802736,1 [End]
*!*	    =gfTherm(lnTotRec , lnTotRec , "Saving credit memo # : "+&lcCrMemHdr..CrMemo)
*!*	  ENDIF
*!*	  
*!*	  SELECT (lcCrMemHdr)
*!*	  *-- Lock the record to grantee the phiscal update.
*!*	  = RLOCK()
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *REPLACE nSteps WITH 3
*!*	  REPLACE nSteps WITH 6
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	  UNLOCK
*!*	ENDIF

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*IF &lcCrMemHdr..nSteps < 4
*!*	IF &lcCrMemHdr..nSteps < 7
*!*	*B602719,1 Reham On 04/05/99   *** End   ***
*!*	  *-- Updating General Ledger Distribution File the customer payment transaction.
*!*	  IF llLink_GL
*!*	    *B602322,1 comment this part since it moved at the bigining of saving.
*!*	    *IF !llRmGlSess
*!*	    *  lcRmGlSess = gfsequence('GLSESSION')
*!*	    *  llRmGlSess = .T.
*!*	    *ENDIF
*!*	    *B602322,1 End.

*!*	    *-- Call local function to update the temp. files & needed variables 
*!*	    *-- fields in the uncompleted session file record.
*!*	    llNothing = IIF(llPontSale , .F. , lfUpdVars())
*!*	    
*!*	    *-- Update the Gl distribution file with created entries.
*!*	    SELECT (lcGLDstTmp)
*!*	    
*!*	    *B602719,1 Reham On 04/06/99   *** Begin ***
*!*	    *REPLACE ALL GLSESSION WITH lcRmGlSess
*!*	    REPLACE ALL GLSESSION WITH lcRmGlSess ;
*!*	                cAdd_User WITH gcUser_ID  ;
*!*	                dAdd_Date WITH DATE()     ;
*!*	                cAdd_Time WITH gfGetTime()
*!*	    *B602719,1 Reham On 04/06/99   *** End   ***
*!*	    
*!*	    USE
*!*	    SELECT GLDIST
*!*	    APPEND FROM (gcWorkDir+lcGLDstTmp)
*!*	    
*!*	    *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	    *B602719,1 Erase the temp. GLDIST file only if called from point of sale.
*!*	    *-- If called from point of sale program.
*!*	    IF llPontSale
*!*	    *B602719,1 Reham On 04/05/99   *** End   ***
*!*	      *-- Erase the temp. file that hold the gl distribution records.
*!*	      ERASE &gcWorkDir.&lcGLDstTmp..DBF
*!*	      ERASE &gcWorkDir.&lcGLDstTmp..CDX
*!*	    *B602719,1 Reham On 04/05/99   *** Begin  ***
*!*	    ELSE
*!*	      *B602719,1 Recreate the temp. GLDIST.
*!*	      =lfCrtUnComp(.F. , .F. , .T.)
*!*	    ENDIF
*!*	    *B602719,1 Reham On 04/05/99   *** End   ***
*!*	  ENDIF
*!*	  
*!*	  SELECT (lcCrMemHdr)
*!*	  *-- Lock the record to grantee the phiscal update.
*!*	  = RLOCK()
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *REPLACE nSteps WITH 4
*!*	  REPLACE nSteps WITH 7
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	  UNLOCK
*!*	ENDIF

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*IF &lcCrMemHdr..nSteps < 5
*!*	IF &lcCrMemHdr..nSteps < 8
*!*	*B602719,1 Reham On 04/05/99   *** End   ***
*!*	  *E301077,4 Reham On 12/27/98   *** Begin ***
*!*	  *E301077,4 Open the salesreps file.
*!*	  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
*!*	  *E301077,4 Reham On 12/27/98   *** End   ***
*!*	  
*!*	  *-- Update [COMMISSIONS] Salesreps commissions / chargebacks
*!*	  SELECT SALESREP
*!*	  IF &lcCrMemHdr..CommPcnt1 + &lcCrMemHdr..CommPcnt2 > 0
*!*	    FOR lnCount = 1 TO 2
*!*	      lcCount = ALLTRIM(STR(lnCount))
*!*	      IF !EMPTY(&lcCrMemHdr..SalesRep&lcCount) .AND. &lcCrMemHdr..CommPcnt&lcCount > 0
*!*	        IF SEEK(&lcCrMemHdr..SalesRep&lcCount , "SALESREP")
*!*	          *-- Calculated the equivalent amount.
*!*	          lnFrnAmnt = &lcCrMemHdr..CommAmt&lcCount
*!*	          lnCommDue = ROUND(&lcCrMemHdr..CommAmt&lcCount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit , 2)
*!*	          
*!*	          lnNewBal = SALESREP.BALANCE  + lnCommDue
*!*	          SELECT SALESREP
*!*	          *-- Lock the record to grantee the phiscal update.
*!*	          = RLOCK()
*!*	          REPLACE CURRENT WITH CURRENT + lnCommDue ;
*!*	                  BALANCE WITH lnNewBal
*!*	          UNLOCK
*!*	          *-- Call Global Function to transmit the local data.
*!*	          =gfTraceKey("SALESREP" , REPCODE , "M")
*!*	          
*!*	          IF &lcCrMemHdr..TotCredit > 0
*!*	            *E301077,4 Reham On 12/27/98   *** Begin ***
*!*	            *E301077,4 Open the salesreps commissions file.
*!*	            =gfOpenFile(gcDataDir+'REPCOMM',gcDataDir+'REPCOMM','SH')
*!*	            *E301077,4 Reham On 12/27/98   *** End   ***

*!*	            SELECT REPCOMM
*!*	            APPEND BLANK
*!*	            *-- Lock the record to grantee the phiscal update.
*!*	            = RLOCK()
*!*	          
*!*	            REPLACE STATUS     WITH 'O' ;
*!*	                    REPCODE    WITH &lcCrMemHdr..SalesRep&lcCount ;
*!*	                    ACCOUNT    WITH &lcCrMemHdr..Account ;
*!*	                    ORDER      WITH &lcCrMemHdr..Order ;
*!*	                    DATE       WITH &lcCrMemHdr..CrDate ;
*!*	                    TRANTYPE   WITH '5' ;
*!*	                    DESC       WITH 'RETURN/I#'+&lcCrMemHdr..Invoice ;
*!*	                    TRAN       WITH &lcCrMemHdr..CrMemo ;
*!*	                    CUSTPO     WITH &lcCrMemHdr..CustPo ;
*!*	                    COMMPCNT   WITH &lcCrMemHdr..CommPcnt&lcCount ;
*!*	                    AMOUNT     WITH lnFrnAmnt ;
*!*	                    BALANCE    WITH lnNewBal ;
*!*	                    cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
*!*	                    nExRate    WITH &lcCrMemHdr..nExRate ;
*!*	                    nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
*!*	                    nForAmnt   WITH &lcCrMemHdr..CommAmt&lcCount
*!*	            

*!*	            
*!*	            *-- Call global function to add audit fields info.
*!*	            =gfAdd_Info("REPCOMM")
*!*	            UNLOCK
*!*	            
*!*	            *-- Call Global Function to transmit the local data.
*!*	            =gfTraceKey("REPCOMM" , REPCODE+DTOS(DATE)+TRAN+TRANTYPE , "A")
*!*	            
*!*	            *B602641,1 Reham On 03/04/99   *** Begin ***
*!*	            =gfCloseFile('REPCOMM')
*!*	            *B602641,1 Reham On 03/04/99   *** End   ***
*!*	          ENDIF
*!*	        ENDIF
*!*	      ENDIF
*!*	    ENDFOR
*!*	  ENDIF
*!*	  SELECT (lcCrMemHdr)
*!*	  *-- Lock the record to grantee the phiscal update.
*!*	  = RLOCK()
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *REPLACE nSteps WITH 5
*!*	  REPLACE nSteps WITH 8
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  UNLOCK
*!*	ENDIF

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*IF &lcCrMemHdr..nSteps < 6
*!*	IF &lcCrMemHdr..nSteps < 9
*!*	*B602719,1 Reham On 04/05/99   *** End   ***
*!*	  *-- Update Return Authorization.
*!*	  *IF !EMPTY(&lcCrMemHdr..RaNo) .AND. &lcCrMemHdr..cIntr_Inv = "Y"
*!*	  IF !EMPTY(&lcCrMemHdr..RaNo)
*!*	    SELECT RALINE
*!*	    *-- Set order descending into the RALINE file to get the last line.
*!*	    SET ORDER TO RALINE DESCENDING
*!*	    *-- Get the last line in the R/A.
*!*	    lnRaCnt = IIF(SEEK(&lcCrMemHdr..RaNo , "RALINE") , VAL(RALINE.cRa_LinNo) + 1 , 1)
*!*	    SET ORDER TO RALINE ASCENDING
*!*	    lnTotDedct = 0    && Var. hold the total difference between open & received
*!*	    lnAmtDedct = 0    && Var. hold the total amount of difference between open & received
*!*	    lnTotRecvd = 0    && Var. hold total received qty.
*!*	    
*!*	    *B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	    lcSaveFile = lcCrMemLin
*!*	    lcCrMemLin = lcOpenLine
*!*	    *B802907,1 Reham On 12/28/1999   *** End   ***
*!*	    SELECT (lcCrMemLin)
*!*	    SCAN
*!*	      *-- If this line related to the selected invoice & not related to 
*!*	      *-- the selected R/A, Add this line with zero qty. in RALINE file.
*!*	      IF &lcCrMemLin..lInvoice .AND. ;
*!*	         !SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
*!*	        SELECT RALINE
*!*	        APPEND BLANK
*!*	        *-- Lock the record to grantee the phiscal update.
*!*	        = RLOCK()
*!*	        REPLACE Rano      WITH &lcCrMemHdr..RaNo ;
*!*	                Account   WITH &lcCrMemHdr..Account ;
*!*	                Style     WITH &lcCrMemLin..Style ;
*!*	                cRa_LinNo WITH ALLTRIM(STR(lnRaCnt)) ;
*!*	                Dyelot    WITH &lcCrMemLin..Dyelot ;
*!*	                Reason    WITH &lcCrMemHdr..Reason ;
*!*	                Price     WITH &lcCrMemLin..Price  ;
*!*	                Tax_Rate  WITH &lcCrMemLin..Tax_Rate ;
*!*	                nPstRate  WITH &lcCrMemLin..nPstRate
*!*	        
*!*	        *-- Call global function to add audit fields info.
*!*	        =gfAdd_Info('RALINE')
*!*	        UNLOCK
*!*	        
*!*	        *-- Call Global Function to transmit the local data.
*!*	        =gfTraceKey("RALINE" , RANO+STYLE+CRA_LINNO , "A")
*!*	        
*!*	        *-- Increment the line no. in the raline file.
*!*	        lnRaCnt = lnRaCnt + 1
*!*	      ENDIF
*!*	      
*!*	      DO CASE
*!*	        *-- If received line.
*!*	        CASE &lcCrMemLin..cRet_TrnCd = "2"
*!*	          IF SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
*!*	            SELECT RALINE
*!*	            *-- Lock the record to grantee the phiscal update.
*!*	            = RLOCK()
*!*	            REPLACE nOpnQty1 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty1-&lcCrMemLin..Qty1 , 0)) ;
*!*	                    nOpnQty2 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty2-&lcCrMemLin..Qty2 , 0)) ;
*!*	                    nOpnQty3 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty3-&lcCrMemLin..Qty3 , 0)) ;
*!*	                    nOpnQty4 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty4-&lcCrMemLin..Qty4 , 0)) ;
*!*	                    nOpnQty5 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty5-&lcCrMemLin..Qty5 , 0)) ;
*!*	                    nOpnQty6 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty6-&lcCrMemLin..Qty6 , 0)) ;
*!*	                    nOpnQty7 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty7-&lcCrMemLin..Qty7 , 0)) ;
*!*	                    nOpnQty8 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty8-&lcCrMemLin..Qty8 , 0)) ;
*!*	                    nTotOpnQty WITH nOpnQty1 + nOpnQty2 + nOpnQty3 + nOpnQty4 + ;
*!*	                                    nOpnQty5 + nOpnQty6 + nOpnQty7 + nOpnQty8
*!*	            UNLOCK
*!*	            FOR lnCount = 1 TO 8
*!*	              lcCount = ALLTRIM(STR(lnCount))
*!*	              lnTotDedct = lnTotDedct + MIN(&lcCrMemLin..nOpnQty&lcCount , &lcCrMemLin..Qty&lcCount)
*!*	              lnAmtDedct = lnAmtDedct + (MIN(&lcCrMemLin..nOpnQty&lcCount , &lcCrMemLin..Qty&lcCount) * &lcCrMemLin..Price)
*!*	              lnTotRecvd = lnTotRecvd + &lcCrMemLin..Qty&lcCount
*!*	            ENDFOR
*!*	            
*!*	            *-- Call Global Function to transmit the local data.
*!*	            =gfTraceKey("RALINE" , RANO+STYLE+CRA_LINNO , "M")
*!*	          ENDIF
*!*	        *-- If canceled line.
*!*	        CASE &lcCrMemLin..cRet_TrnCd = "4"
*!*	          lnTotCancl = lnTotCancl + &lcCrMemLin..TotQty
*!*	      ENDCASE
*!*	      SELECT (lcCrMemLin)
*!*	    ENDSCAN
*!*	    
*!*	    *B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	    lcCrMemLin = lcSaveFile
*!*	    *B802907,1 Reham On 12/28/1999   *** End   ***
*!*	    
*!*	    *-- Update the RA header file with the changed fields.
*!*	    SELECT RETAUTH
*!*	    SET ORDER TO TAG RETAUTHA
*!*	    IF SEEK(&lcCrMemHdr..Account + &lcCrMemHdr..RaNo)
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      REPLACE INVOICE    WITH &lcCrMemHdr..Invoice ;
*!*	              TRAN       WITH &lcCrMemHdr..CrMemo ;
*!*	              ORDER      WITH &lcCrMemHdr..Order ;
*!*	              CUSTPO     WITH &lcCrMemHdr..CustPo ;
*!*	              RETDATE    WITH &lcCrMemHdr..CrDate ;
*!*	              RETURN     WITH RETURN + &lcCrMemHdr..Pieces ;
*!*	              RETURNAMT  WITH RETURNAMT + &lcCrMemHdr..Gross_Amt ;
*!*	              cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
*!*	              nExRate    WITH &lcCrMemHdr..nExRate ;
*!*	              nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
*!*	              nreta_opn  WITH IIF(llCancRest , 0 , MAX(nreta_opn-lnTotDedct , 0)) ;
*!*	              nreta_rec  WITH nreta_rec + lnTotRecvd ;
*!*	              nreta_can  WITH nreta_can + lnTotCancl ;
*!*	              nRtOpnAmt  WITH IIF(llCancRest , 0 , MAX(nRtOpnAmt-lnAmtDedct , 0)) ;
*!*	              STATUS     WITH IIF(llCancRest .OR. nreta_opn = 0 , "C" , STATUS)
*!*	      UNLOCK
*!*	      
*!*	      *-- Call Global Function to transmit the local data.
*!*	      =gfTraceKey("RETAUTH" , RANO , "M")
*!*	    ENDIF
*!*	    SET ORDER TO TAG RETAUTH
*!*	  ENDIF
*!*	  SELECT (lcCrMemHdr)
*!*	  *-- Lock the record to grantee the phiscal update.
*!*	  = RLOCK()
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *REPLACE nSteps WITH 6
*!*	  REPLACE nSteps WITH 9
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	  UNLOCK
*!*	ENDIF

*!*	*-- Post customer's A/R
*!*	llPost = IIF(llPostfInv .AND. !EMPTY(&lcCrMemHdr..cFacCode) , .F. , .T.)

*!*	IF llPost .AND. &lcCrMemHdr..TotCredit <> 0
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *IF &lcCrMemHdr..nSteps < 7
*!*	  IF &lcCrMemHdr..nSteps < 10
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	    *E301077,4 Reham On 12/27/98   *** Begin ***
*!*	    *E301077,4 Open the credit file.
*!*	    =gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
*!*	    *E301077,4 Reham On 12/27/98   *** End   ***
*!*	    
*!*	    lnTotCredit = ABS(&lcCrMemHdr..TotCredit) * -1
*!*	    
*!*	    *-- Add record in the credit file for the total credit amount.
*!*	    SELECT CREDIT
*!*	    APPEND BLANK
*!*	    *-- Lock the record to grantee the phiscal update.
*!*	    = RLOCK()
*!*	    REPLACE TRAN       WITH &lcCrMemHdr..CrMemo ;
*!*	            cFacCode   WITH &lcCrMemHdr..cFacCode ;
*!*	            ACCOUNT    WITH &lcCrMemHdr..Account ;
*!*	            TRANDATE   WITH &lcCrMemHdr..CrDate ;
*!*	            dPostDate  WITH &lcCrMemHdr..dPostDate ;
*!*	            TRANTYPE   WITH '0' ;
*!*	            CCREDITCOD WITH &lcCrMemHdr..Reason ;
*!*	            DESC       WITH 'RET.INV#'+&lcCrMemHdr..Invoice ;
*!*	            STORE      WITH &lcCrMemHdr..Store ;
*!*	            DSC_AMT    WITH ABS(lnTTrdeAmt) * -1 ;
*!*	            cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
*!*	            nExRate    WITH &lcCrMemHdr..nExRate ;
*!*	            nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
*!*	            AMOUNT     WITH lnTotCredit ;
*!*	            cArGlAcc   WITH &lcCrMemHdr..cArAcnt
*!*	            
*!*	    *-- Call global function to add audit fields info.
*!*	    =gfAdd_Info("CREDIT")
*!*	  
*!*	    IF !EMPTY(&lcCrMemHdr..RaNo) .AND. EMPTY(&lcCrMemHdr..Reference)
*!*	      REPLACE REFERENCE WITH 'R/A# '+&lcCrMemHdr..RaNo
*!*	    ELSE
*!*	      IF EMPTY(&lcCrMemHdr..RaNo) .AND. !EMPTY(&lcCrMemHdr..Reference)
*!*	        REPLACE REFERENCE WITH &lcCrMemHdr..Reference
*!*	      ELSE
*!*	        IF !EMPTY(&lcCrMemHdr..RaNo) .AND. !EMPTY(&lcCrMemHdr..Reference)
*!*	          REPLACE REFERENCE WITH 'RA'+&lcCrMemHdr..RaNo+'/'+&lcCrMemHdr..Reference
*!*	        ELSE
*!*	          REPLACE REFERENCE WITH ' '
*!*	        ENDIF
*!*	      ENDIF
*!*	    ENDIF
*!*	    UNLOCK
*!*	    
*!*	    *-- Call Global Function to transmit the local data.
*!*	    =gfTraceKey("CREDIT" , TRANTYPE+TRAN , "A")
*!*	    
*!*	    SELECT (lcCrMemHdr)
*!*	    *-- Lock the record to grantee the phiscal update.
*!*	    = RLOCK()
*!*	    *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	    *REPLACE nSteps WITH 7
*!*	    REPLACE nSteps WITH 10
*!*	    *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	    UNLOCK
*!*	  ENDIF
*!*	  
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *IF &lcCrMemHdr..nSteps < 8
*!*	  IF &lcCrMemHdr..nSteps < 11
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	    *-- Compute new aged A/R applying credits to oldest balance first
*!*	    *-- Return new customer a/r balance.
*!*	    SELECT CUSTOMER
*!*	    *-- Lock the record to grantee the phiscal update.
*!*	    = RLOCK()
*!*	    *-- Calculated the equivalent amount.
*!*	    REPLACE OPENCR  WITH OPENCR + ROUND((ABS(&lcCrMemHdr..TotCredit) * -1) &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
*!*	            NETBAL  WITH TOTAGE + OPENCR
*!*	    UNLOCK
*!*	    
*!*	    *E301245,1 function to update nHgWtrMark Field with NETBAL field [Begin.]
*!*	    =lfHgWUpdat() 
*!*	    *E301245,1 function to update nHgWtrMark Field with NETBAL field [End.]
*!*	    
*!*	    *-- Call Global Function to transmit the local data.
*!*	    =gfTraceKey("CUSTOMER" , "M"+Account+Store , "M")
*!*	    
*!*	    SELECT (lcCrMemHdr)
*!*	    *-- Lock the record to grantee the phiscal update.
*!*	    = RLOCK()
*!*	    *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	    *REPLACE nSteps WITH 8
*!*	    REPLACE nSteps WITH 11
*!*	    *B602719,1 Reham On 04/05/99   *** End   ***
*!*	    UNLOCK
*!*	  ENDIF
*!*	ENDIF

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*IF &lcCrMemHdr..nSteps < 9
*!*	IF &lcCrMemHdr..nSteps < 12
*!*	*B602719,1 Reham On 04/05/99   *** End   ***
*!*	  IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)
*!*	    *E301077,4 Reham On 12/27/98   *** Begin ***
*!*	    *E301077,4 Open the customer history file.
*!*	    =gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
*!*	    *E301077,4 Reham On 12/27/98   *** End   ***
*!*	    
*!*	    lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )
*!*	    *-- Update the customer history file.
*!*	    SELECT ARCUSHST
*!*	    IF SEEK(&lcCrMemHdr..Account + lcTrYear , "ARCUSHST")
*!*	      *-- Lock the record to grantee the phiscal update.
*!*	      = RLOCK()
*!*	      REPLACE nRetAmt            WITH nRetAmt + ROUND(&lcCrMemHdr..Gross_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
*!*	              nRetQty            WITH nRetQty + &lcCrMemHdr..Pieces ;
*!*	              nDisAmt            WITH nDisAmt - ROUND(&lcCrMemHdr..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
*!*	              nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod + ROUND(&lcCrMemHdr..Gross_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
*!*	              nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod + &lcCrMemHdr..Pieces ;
*!*	              nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod - ROUND(&lcCrMemHdr..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
*!*	      UNLOCK
*!*	      
*!*	      *-- Call Global Function to transmit the local data.
*!*	      =gfTraceKey("ARCUSHST" , &lcCrMemHdr..Account + lcTrYear , "M")
*!*	    ENDIF
*!*	  ENDIF
*!*	  SELECT (lcCrMemHdr)
*!*	  *-- Lock the record to grantee the phiscal update.
*!*	  = RLOCK()
*!*	  *B602719,1 Reham On 04/05/99   *** Begin ***
*!*	  *REPLACE nSteps WITH 9
*!*	  REPLACE nSteps WITH 12
*!*	  *B602719,1 Reham On 04/05/99   *** End   ***
*!*	  UNLOCK
*!*	ENDIF

*E301490,1 (End)

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Reham Al-Allamy
*! Date      : 06/01/1997
*! Purpose   : Void Credit Memo
*!*************************************************************
*! Calls     : CHECKPRD, lfCrtUnComp, lfUpdVars, gfGetExSin, GLDIST
*!             gpUdtSJl, gfOpenFile
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr
PRIVATE lnAmount

IF ASCAN(laEvntTrig,PADR('RMCKVDBN',10))<>0 AND ;
  !gfDoTriger('RMCRMEM',PADR('RMCKVDBN',10))
  RETURN .F.
ENDIF

SELECT RETHDR
*B602773,1 Reham On 04/11/99   *** Begin ***
*STORE gdSysDate TO ldVdDate , ldOldVoid
IF gdSysDate < laData[5]
  STORE laData[5] TO ldVdDate , ldOldVoid
ELSE
  STORE gdSysDate TO ldVdDate , ldOldVoid
ENDIF
*B602773,1 Reham On 04/11/99   *** Begin ***

*-- Flag to cancel void process.
llCancVoid = .T.

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*B802803,1 Reham On 11/21/1999    *** Begin ***
*B802803,1 Change the program & its screens to be shared between modules.
*-- Call screen to validate the void credit memo date.
*DO (gcScrDir+gcWinAppl+"\RMVdDat.SPX")
DO (gcScrDir+"RMVdDat.SPX")
*B802803,1 Reham On 11/21/1999    *** End   ***

*-- Restore the trapped keys.
POP KEY

*-- If not validating the void date, cancel the void process.
IF llCancVoid
  RETURN
ENDIF

*-- Get the year & period for the void date
=CHECKPRD(ldVdDate , 'lcGLFYear' , 'lcGLPeriod' , 'RM' , .T.)

*B121250,1 ABD - Voiding CM allows user to post to a locked period. [Begin]
*-- If the user did not validate the transaction date.
IF !EMPTY(ldVdDate) 
  IF EMPTY(lcGLFYear) .OR. EMPTY(lcGLPeriod)
    lcMessage = 'System date '+DTOC(ldVdDate)+' does not fall within any period.'+;
                ' Not allowed to void returns for this date.' 
    =gfModalGen('INM00274B00000','ALERT',lcMessage)
    RETURN
  ELSE
    IF !CHECKPRD(ldVdDate , 'lcGLFYear' , 'lcGLPeriod' , 'RM')
      RETURN
    ENDIF
  ENDIF
ENDIF
*B121250,1 ABD - [End]

*B602732,1 Reham On 04/04/99   *** Begin ***
*-- Get the year & period for the transaction date.
STORE "" TO lcTrYear , lcTrPeriod
*=CHECKPRD(&lcCrMemHdr..CrDate , 'lcTrYear' , 'lcTrPeriod' , 'RM' , .T.)
=CHECKPRD(RetHdr.CrDate , 'lcTrYear' , 'lcTrPeriod' , 'RM' , .T.)
*B602732,1 Reham On 04/04/99   *** End   ***

*B602719,1 Reham On 04/05/99   *** Begin ***
*-- Create temp. file to hold the gl distribution records.
*IF llLink_GL
*  lcGLDstTmp = gfTempName()
*  SELECT GLDIST
*  COPY STRUCTURE TO (gcWorkDir+lcGLDstTmp)
*  =gfOpenFile(gcWorkDir+lcGLDstTmp,'','EX')
*  INDEX ON GLaccount TAG lcGLDstTmp
*ENDIF
*B602719,1 Reham On 04/05/99   *** End   ***

*E301216,1 Reham On 05/04/99   *** Begin ***
=gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHISTT','SH')
*E301216,1 Reham On 05/04/99   *** End   ***

*-- If not completing a previous uncompleting session.
IF !llContinue
  *E301216,1 Reham On 05/04/99   *** Begin ***
  *E301216,1 Create temporary History file which used key off process
  =lfCreatHst()
  *E301216,1 Reham On 05/04/99   *** End   ***
  
  *-- Prepare the files for new session.

  *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
  *E301490,1 (Start)
  *=lfCrtUnComp(.T. , .T. , .T.)
  DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .T. , .T.,.T.
  *E301490,1 (End)

  *B602719,1 Reham On 04/05/99   *** Begin ***
  *B602719,1 Save the current credit memo to be voided in the temp header file to be 
  *B602719,1 retreived if uncomplete session happen.
  DECLARE laCredit[1]
  laCredit = ""
  SELECT RetHdr
  SCATTER TO laCredit
  SELECT (lcCrMemHdr)
  *INSERT INTO (lcCrMemHdr) (nSteps) VALUES (0)
  APPEND BLANK
  GATHER FROM laCredit
  *B602719,1 Reham On 04/05/99   *** End   ***
  
  SELECT (lcCrMemLin)
  INSERT INTO (lcCrMemLin) (nSteps) VALUES (0)
  
  *-- Create an "Open" record in the uncomplete session file.
  llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR("pbDlt"), .T.)
  
  *E301216,1 Reham On 05/04/99   *** Begin ***
  *E301216,1 Add Temporary history file to uncomplete session files
  lcFiles = 'lcCrMemHdr,'+lcCrMemHdr+','+lcCrMemHdr+';'+ ;
            'lcCrMemLin,'+lcCrMemLin+','+lcCrMemLin+';'+ ;
            'lcGLDstTmp,'+lcGLDstTmp+','+lcGLDstTmp+';'+ ;
            'lcTmpAR,'+lcTmpAR+','+ORDER(lcTmpAR)+';'
  *E301216,1 If there is record added in the uncompleted session file, call global 
  *E301216,1 function to update the temp. files & variables fields in the uncompleted
  *E301216,1 session file.
  llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars))
  *E301216,1 Reham On 05/04/99   *** End   ***
ENDIF

llVoid = .T.

*B602771,1 Reham On 04/11/99   *** Begin ***
IF llMulCurr
*B602771,1 Reham On 04/11/99   *** End   ***
  *-- Got exchange rate sign and unit sign.
  lcUntSin = ' '
  lcExRSin = gfGetExSin(@lcUntSin, laData[31])
*B602771,1 Reham On 04/11/99   *** Begin ***
ELSE
  lcUntSin   = '/'        &&  Variable to hold unit sign.
  lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
ENDIF
*B602771,1 Reham On 04/11/99   *** End   ***


*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Open the style dyelot file.
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
*E301077,4 Reham On 12/27/98   *** End   ***

llPost = IIF(llPostfInv .AND. !EMPTY(&lcCrMemHdr..cFacCode) , .F. , .T.)
*IF !llPostfInv .AND. !EMPTY(laData[9]))
IF llPost
  IF &lcCrMemHdr..nSteps < 1
    *E301077,4 Reham On 12/27/98   *** Begin ***
    *E301077,4 Open the credit file.
    =gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
    *E301077,4 Reham On 12/27/98   *** End   ***
    
    *E301216,1 Reham On 05/04/99   *** Begin ***
    =gfOpenFile(gcDataDir+'DEBIT', gcDataDir+'DEBIT','SH')
    *E301216,1 Reham On 05/04/99   *** End   ***
    IF SEEK(laData[2] + laData[1] , "CREDIT")
      llVoid = .T.
      
      *-- Updating customer A/R .
      SELECT CREDIT
      
      *E301216,1 Reham On 05/04/99   *** Begin ***
      *E301216,1 Stop deleting the credit record.
      *DELETE
      *-- Call Global Function to transmit the local data.
      *=gfTraceKey("CREDIT" , laData[2] + laData[1] , "D")

      *E301216,1 Scatter the credit field to be added in the temp. history file.
      SCATTER MEMVAR MEMO
      m.cShToOpn  = 'Y'
      *E301216,1 Insert the credit record in the temp. history file.
      INSERT INTO (lcTmpAr) FROM MEMVAR
      
      *-- Add Credit Data to master credit file[Begin]
      m.Batch     = gfsequence('BATCH')
      *RAM
      *m.Tran      = gfsequence('DEBIT', gcAct_Comp, "", "", "TRAN")
      m.Tran      = RetHdr.CrMemo
      *RAM
      *B602897,1 Reham On 05/16/1999   *** Begin ***
      *B602897,1 Change the transaction type to "R" instead of "2"
      *m.TranType  = '2'
      m.TranType  = 'R'
      *B602897,1 Reham On 05/16/1999   *** End   ***
      m.Desc      = 'DEBIT ADJ./VOID RET'
      m.Amount    = ABS(m.Amount)
      m.Reference = 'Void Return'
      m.cInstalNo = ''

      *B121145,1 ABD - Save the old record position. [Begin]
      SELECT RETHDR
      lcOldRecNo = RECNO()
      *B121145,1 ABD - [End]

      SELECT DEBIT
      =RLOCK()
      INSERT INTO ('DEBIT') FROM MEMVAR
      =gfAdd_Info('DEBIT')
      *B602897,1 Reham On 05/16/1999   *** Begin ***
      *B602897,1 Change the transaction type to "R" instead of "2"
      *=gfTraceKey('DEBIT','2'+Tran,'A')
      =gfTraceKey('DEBIT','R'+Tran,'A')
      *B602897,1 Reham On 05/16/1999   *** End   ***
      UNLOCK

      *E301216,1 Add Debit record to temporary history file.
      =RLOCK(lcTmpAr)
      INSERT INTO (lcTmpAr) FROM MEMVAR
      UNLOCK  
      
      *E301216,1 Call key off program.
      *-- HDM B603159,1 [Start] Fixing bug of saving the system date in arhist instead of void date
      *DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
                     RETHDR.ACCOUNT,gdSysDate,ABS(DEBIT.Amount),CREDIT.Amount,lcTmpAR
      
      DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
                     RETHDR.ACCOUNT,ldVdDate,ABS(DEBIT.Amount),CREDIT.Amount,lcTmpAR

      *B121145,1 ABD - Return the pointer to the old record position. [Begin]
      SELECT RETHDR
      GOTO lcOldRecNo
      *B121145,1 ABD - [End]
                     
      *-- HDM B603159,1 [End]
      *E301216,1 Reham On 05/04/99    *** End   ***
      *B603220,1 [Start] Don't update the OPENCR AND NETBAL in Customer file
      *                  as they're updated by lfKeyOff before
      *-- Update the customer a/r balance.
      SELECT CUSTOMER
      *-- Lock the record to grantee the phiscal update.
      *= RLOCK()
      *REPLACE OPENCR  WITH OPENCR + ROUND(laData[28] &lcExRSin laData[33] &lcUntSin laData[32],2) ;
              NETBAL  WITH TOTAGE + OPENCR
      *UNLOCK
      *B603220,1 [End]
      *E301245,1 function to update nHgWtrMark Field with NETBAL field [Begin.]
      
      *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
      *E301490,1 (Start)
      *=lfHgWUpdat() 
      DO lfHgWUpdat IN (gcapphome+'\RMSave.PRG')
      *E301490,1 (End)

      *E301245,1 function to update nHgWtrMark Field with NETBAL field [End.]
      
      *-- Call Global Function to transmit the local data.
     *=gfTraceKey("CUSTOMER" , Customer.Account , "M")
      =gfTraceKey("CUSTOMER" , Customer.Type+Customer.Account+Customer.Store , "M")
    ELSE
      IF laData[28] <> 0
        llVoid = .F.
        *** This credit memo has already been applied.  Cannot void! ***
        *** <  Ok  > ***
        =gfModalGen("TRM46010B00000" , "DIALOG")
        
        *B602719,1 Reham On 04/05/99   *** Begin ***
        *-- Create the temp files.
        *=lfCrtUnComp(.T. , .T.)
        
        *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
        *E301490,1 (Start)
        *=lfCrtUnComp(.T. , .T. , .T.)
        DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .T. , .T.,.T.
        *E301490,1 (End)

        *B602719,1 Reham On 04/05/99   *** Begin ***
        
        *-- Finish the whole session.
        SELECT unCmSess
        IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
          GOTO lnUnCmSeRc
          REPLACE STATUS WITH 'C'
          llContinue = .F.
          UNLOCK
        ENDIF
        lnUnCmSeRc = 0
        
        SELECT RETHDR
        RETURN
      ENDIF
    ENDIF
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
ENDIF

lnTotAdded = 0    && Var. hold the total difference between open & received
lnTotAmtAd = 0    && Var. hold the total of open amount added.
lnTotRecvd = 0    && Var. hold total received qty.
lnTotCancl = 0    && Var. hold total canceled qty.

*-- Flag to know if rebuilding RA fields in style & style dyelot if the 
*-- credit memo was created from RA.
llReBld_RA = .F.

*-- If credit memo was created from RA.
*** Would you like to rebuild return authorization fields? ***
*** < Yes > - < No > ***
IF !EMPTY(laData[3]) .AND. gfModalGen("QRM46026B00006" , "DIALOG") = 1
  llReBld_RA = .T.
ENDIF

*C123847,1  TMI [Start] adjust data in trnhist and styhist file if credit memo is voided
&&>>> 
*C123847,1  TMI [End  ] 

*!B602322,1 Moved-Read the gl session if not readed to be used in GfStyclr.
IF !llVRGLSess
  lcVRGLSess = gfsequence('GLSESSION')
  llVRGLSess = .T.
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *-- Call local function to update the temp. files & needed variables 
  *-- fields in the uncompleted session file record.
  =lfUpdVars()
  *B602719,1 Reham On 04/05/99   *** End   ***
ENDIF
*!B602322,1 End.

*-- Var. hold the incremental # will be saved in the nSteps field in the
*-- credit memo line file.
lnNxtStp   = 0

*E301224,1 Reham On 05/19/1999    *** Begin ***
*E301224,1 Get the session # of the receiving from GLDIST file.
=gfOpenFile(gcDataDir+'Styinvjl' , gcDataDir+'Styinvjl' , 'SH')
*E301224,1 Reham On 05/19/1999    *** End   ***

*** Loop to update files
IF llVoid
  IF &lcCrMemHdr..nSteps < 2
    SELECT STYLE
    SET ORDER TO TAG STYLE
    *-- Update the style inventory.
    SELECT RETLINE
    SEEK laData[1]
    lnWhichLin = 1
    *B606480,1 (Begin) Don't scan cancelled lines.
    *SCAN WHILE CRMEMO = laData[1]
    *B608300,1 WAM 10/02/2007 fix bug of not updating all credit memo lines when some lines are returned to damage
    *SCAN WHILE CRMEMO = laData[1] AND cret_trncd <> "4"
    SCAN REST WHILE CRMEMO = laData[1] FOR cret_trncd <> "4"
    *B608300,1 WAM 10/02/2007 (End)

    *B606480,1 (End)
      *-- If the user confirmed rebuilding RA fields in the style and
      *-- style dyelot file.
      IF llReBld_RA
        *-- If this line from RA & exist in RALINE file, Adjust RA fields.
        llRALinExs = IIF(SEEK(laData[3] + RETLINE.Style , "RALINE") , .T. , .F.)
      ELSE
        *-- If this line does not exist in RALINE file, set to false.
        llRALinExs = .F.
      ENDIF
      
      SELECT STYLE
      IF SEEK(RETLINE.Style , "STYLE")
        IF &lcCrMemLin..nSteps < lnWhichLin
          *-- Now Update The Style Dyelot Inventory For Dyelot System &
          *-- Styles That Come in Dyelots.
          IF llUseDyes .AND. Style.cDye_Flg = 'Y' .AND. Style.lInvSty
            SELECT STYDYE
            IF SEEK (RETLINE.Style + laData[29] + RETLINE.Dyelot)
              *-- Lock the record to grantee the phiscal update.
              = RLOCK()
              *B606404,1 TMI [Start] Include only lines with type '2'
              IF RETLINE.CRET_TRNCD = '2'              
                *B606404,1 TMI [End  ] 
                REPLACE RET1   WITH RET1 - RETLINE.QTY1  ;
                        RET2   WITH RET2 - RETLINE.QTY2  ;
                        RET3   WITH RET3 - RETLINE.QTY3  ;
                        RET4   WITH RET4 - RETLINE.QTY4  ;
                        RET5   WITH RET5 - RETLINE.QTY5  ;
                        RET6   WITH RET6 - RETLINE.QTY6  ;
                        RET7   WITH RET7 - RETLINE.QTY7  ;
                        RET8   WITH RET8 - RETLINE.QTY8  ;
                        TOTRET WITH RET1+RET2+RET3+RET4+RET5+RET6+RET7+RET8
                *B606404,1 TMI [Start] 
              ENDIF
              *B606404,1 TMI [End  ] 
              *-- If this line from RA & exist in RALINE file, Adjust RA fields.
              IF llRALinExs
                REPLACE RA1    WITH RA1 + RETLINE.QTY1 ;
                        RA2    WITH RA2 + RETLINE.QTY2 ;
                        RA3    WITH RA3 + RETLINE.QTY3 ;
                        RA4    WITH RA4 + RETLINE.QTY4 ;
                        RA5    WITH RA5 + RETLINE.QTY5 ;
                        RA6    WITH RA6 + RETLINE.QTY6 ;
                        RA7    WITH RA7 + RETLINE.QTY7 ;
                        RA8    WITH RA8 + RETLINE.QTY8 ;
                        TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
              ENDIF
              UNLOCK
              
              *-- Call Global Function to transmit the local data.
              =gfTraceKey("STYDYE" , RETLINE.Style + laData[29] + RETLINE.Dyelot , "M")
            ENDIF
          ENDIF
          SELECT (lcCrMemLin)
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE nSteps WITH lnWhichLin
          UNLOCK
        ENDIF
        lnWhichLin = lnWhichLin + 1
        
        *-- Update the warehouse record.
        IF &lcCrMemLin..nSteps < lnWhichLin
          SELECT STYDYE
          IF SEEK(RETLINE.STYLE + laData[29] + SPACE(10)) .AND. Style.lInvSty
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            *B606404,1 TMI [Start] Include only lines with type '2'
            IF RETLINE.CRET_TRNCD = '2'              
              *B606404,1 TMI [End  ] 
              REPLACE RET1   WITH RET1 - RETLINE.QTY1  ;
                      RET2   WITH RET2 - RETLINE.QTY2  ;
                      RET3   WITH RET3 - RETLINE.QTY3  ;
                      RET4   WITH RET4 - RETLINE.QTY4  ;
                      RET5   WITH RET5 - RETLINE.QTY5  ;
                      RET6   WITH RET6 - RETLINE.QTY6  ;
                      RET7   WITH RET7 - RETLINE.QTY7  ;
                      RET8   WITH RET8 - RETLINE.QTY8  ;
                      TOTRET WITH RET1+RET2+RET3+RET4+RET5+RET6+RET7+RET8
              *B606404,1 TMI [Start] 
            ENDIF
            *B606404,1 TMI [End  ] 
            *-- If this line from RA & exist in RALINE file, Adjust RA fields.
            IF llRALinExs
              REPLACE RA1    WITH RA1 + RETLINE.QTY1 ;
                      RA2    WITH RA2 + RETLINE.QTY2 ;
                      RA3    WITH RA3 + RETLINE.QTY3 ;
                      RA4    WITH RA4 + RETLINE.QTY4 ;
                      RA5    WITH RA5 + RETLINE.QTY5 ;
                      RA6    WITH RA6 + RETLINE.QTY6 ;
                      RA7    WITH RA7 + RETLINE.QTY7 ;
                      RA8    WITH RA8 + RETLINE.QTY8 ;
                      TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
            ENDIF
            UNLOCK
            
            *-- Call Global Function to transmit the local data.
            =gfTraceKey("STYDYE" , RETLINE.STYLE + laData[29] + SPACE(10) , "M")
          ENDIF
          SELECT (lcCrMemLin)
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE nSteps WITH lnWhichLin
          UNLOCK
        ENDIF
        lnWhichLin = lnWhichLin + 1
        
        IF &lcCrMemLin..nSteps < lnWhichLin
          IF Style.lInvSty
            *-- Update the stock.
            SELECT STYLE
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            *B606404,1 TMI [Start] Include only lines with type '2'
            IF RETLINE.CRET_TRNCD = '2'
              *B606404,1 TMI [End  ] 
              REPLACE RET1   WITH RET1-RETLINE.QTY1 ;
                      RET2   WITH RET2-RETLINE.QTY2 ;
                      RET3   WITH RET3-RETLINE.QTY3 ;
                      RET4   WITH RET4-RETLINE.QTY4 ;
                      RET5   WITH RET5-RETLINE.QTY5 ;
                      RET6   WITH RET6-RETLINE.QTY6 ;
                      RET7   WITH RET7-RETLINE.QTY7 ;
                      RET8   WITH RET8-RETLINE.QTY8 ;
                      TOTRET WITH RET1+RET2+RET3+RET4+RET5+RET6+RET7+RET8
              *B606404,1 TMI [Start] 
            ENDIF
            *B606404,1 TMI [End  ] 
            *-- If this line from RA & exist in RALINE file, Adjust RA fields.
            IF llRALinExs
              REPLACE RA1    WITH RA1 + RETLINE.QTY1 ;
                      RA2    WITH RA2 + RETLINE.QTY2 ;
                      RA3    WITH RA3 + RETLINE.QTY3 ;
                      RA4    WITH RA4 + RETLINE.QTY4 ;
                      RA5    WITH RA5 + RETLINE.QTY5 ;
                      RA6    WITH RA6 + RETLINE.QTY6 ;
                      RA7    WITH RA7 + RETLINE.QTY7 ;
                      RA8    WITH RA8 + RETLINE.QTY8 ;
                      TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
            ENDIF
            UNLOCK
            *-- Call Global Function to transmit the local data.
            =gfTraceKey("STYLE" , Style.Style , "M")
          ENDIF
          
          SELECT (lcCrMemLin)
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE nSteps WITH lnWhichLin
          UNLOCK
        ENDIF
        lnWhichLin = lnWhichLin + 1
        SELECT RETLINE
        *B606264,1 (Begin) Don't put records in GlDist for Cancelled lines as it has
        *B606264,1         no effoect on it.
        *IF llLink_GL
        IF llLink_GL AND cRet_Trncd <> "4"
        *B606264,1 (End)
          *B602719,1 Reham On 04/05/99   *** Begin ***
          IF &lcCrMemLin..nSteps < lnWhichLin
          *B602719,1 Reham On 04/05/99   *** End   ***
            *** CALL GL DISTRIBUTION PROCEDURE 4 TIMES FOR EACH 
            *** LINE OF THIS RETURN.
    
            *B602688,1 Reham On 03/21/99   *** Begin ***
            *B602688,1 Send the sales account to the function.
            *-- 1) TOTAL GROSS MERCHANDISE         <DEBIT>  Return Merchandise
            *-- CATEGORY KEY FOR "Return Merchaandise....=> '020'
            *DO GLDIST WITH RETLINE.GL_Sales , '020' , ;
                          -(RETLINE.TotQty * RETLINE.Gros_Price) , 'VR' , ;
                           RETLINE.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                           '&lcGLDstTmp' , '' , RETHDR.cCurrCode , ;
                           RETHDR.nCurrUnit , RETHDR.nExRate
            DO GLDIST WITH RETLINE.GL_Sales , '020' , ;
                           -(RETLINE.TotQty * RETLINE.Gros_Price) , 'VR' , ;
                           RETLINE.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                           '&lcGLDstTmp' , RETLINE.cSalesAcnt , RETHDR.cCurrCode , ;
                           RETHDR.nCurrUnit , RETHDR.nExRate
            *B602719,1 Reham On 04/05/99   *** Begin ***
            SELECT (lcCrMemLin)
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            REPLACE nSteps WITH lnWhichLin
            UNLOCK
          ENDIF
          lnWhichLin = lnWhichLin + 1
          *B602719,1 Reham On 04/05/99   *** End   ***
          
          *B602719,1 Reham On 04/05/99   *** Begin ***
          IF &lcCrMemLin..nSteps < lnWhichLin
          *B602719,1 Reham On 04/05/99   *** End   ***
            *B602688,1 Send the discount account to the function.
            *-- 2) DISCOUNT AMOUNT                 <CREDIT>  SALES DISCOUNT
            *-- CATEGORY KEY FOR "Discount"...............=> '005'
            *DO GLDIST WITH RETLINE.GL_Sales , '005' , RETLINE.DISC_AMT , 'VR' , ;
                           RETLINE.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                           '&lcGLDstTmp', '' , RETHDR.cCurrCode , ;
                           RETHDR.nCurrUnit , RETHDR.nExRate
            DO GLDIST WITH RETLINE.GL_Sales , '005' , RETLINE.DISC_AMT , 'VR' , ;
                           RETLINE.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                           '&lcGLDstTmp', RETLINE.cDiscAcnt , RETHDR.cCurrCode , ;
                           RETHDR.nCurrUnit , RETHDR.nExRate
            *B602688,1 Reham On 03/21/99   *** End   ***
            
            *B602719,1 Reham On 04/05/99   *** Begin ***
            SELECT (lcCrMemLin)
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            REPLACE nSteps WITH lnWhichLin
            UNLOCK
          ENDIF
          lnWhichLin = lnWhichLin + 1
          *B602719,1 Reham On 04/05/99   *** End   ***
        ENDIF
        SELECT RETLINE
        
        *--Update Style Inventory (Start step 2 & Next Step lnNxtStp)---------------
        *--G/L Array difinition and initialization.
        *-- Update general ledger entreis in gfStyCrl()
        IF llLink_GL
          DECLARE laGLDistAr[2,13]

          *B602688,1 Reham On 03/21/99   *** Begin ***
          *B602688,1 Use the GL sales code  for the cost of goods sold instead of GL cost code
          *STORE RETLINE.GL_Cost TO laGLDistAr[1,1] , laGLDistAr[2,1]
          laGLDistAr[1,1]  = RETLINE.GL_Sales
          laGLDistAr[2,1]  = RETLINE.GL_Cost
          *B602688,1 Send the related account to the function.
          laGLDistAr[1,10] = RETLINE.cCOGSAcnt
          laGLDistAr[2,10] = RETLINE.cICAcnt
          *B602688,1 Reham On 03/21/99   *** Begin ***

          laGLDistAr[1,2] = '008'
          laGLDistAr[2,2] = '006'
          
          *E301038,1 Change the Gl transaction sign.
          *laGLDistAr[1,3] =  1
          *laGLDistAr[2,3] = -1
          laGLDistAr[1,3] = -1
          laGLDistAr[2,3] =  1
          *E301038,1 End.
          STORE 'VR' TO laGLDistAr[1,4],laGLDistAr[2,4]
          STORE RETLINE.CrMemo TO laGLDistAr[1,5],laGLDistAr[2,5]
          STORE ldVdDate       TO laGLDistAr[1,6],laGLDistAr[2,6]
          STORE lcGLFYear      TO laGLDistAr[1,7],laGLDistAr[2,7]
          STORE lcGlPeriod     TO laGLDistAr[1,8],laGLDistAr[2,8]
          STORE lcGLDstTmp     TO laGLDistAr[1,9],laGLDistAr[2,9]
        ELSE
          DIME laGLDistAr[1,1]
          laGLDistAr = ''
        ENDIF
        
        DECLARE laAdjQty[9]
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          laAdjQty[lnCount] = RETLINE.Qty&lcCount
        ENDFOR
        laAdjQty[9] = RETLINE.TotQty
        
        *B602719,1 Reham On 04/05/99   *** Begin ***
        IF &lcCrMemLin..nSteps < lnWhichLin
        *B602719,1 Reham On 04/05/99   *** End   ***
          *--Call the global function for update style inventory control.
          *B602322,1 Pass gl session as a parameter.       
          *lnNxtStp = gfStyCrl('8',IIF(!EMPTY(cRetSty) , cRetSty , Style),;
                               laData[29],Dyelot,CrDate,CrMemo,@laAdjQty,0,;
                               '',.T.,'',lnWhichLin,lcCrMemLin,'nSteps',@laGLDistAr)
          
          *E301200 (AAMER) Start
          *lnNxtStp = gfStyCrl('8',IIF(!EMPTY(cRetSty) , cRetSty , Style),;
                              laData[29],Dyelot,CrDate,CrMemo,@laAdjQty,0,;
                              '',lcVRGLSess,'',lnWhichLin,lcCrMemLin,'nSteps',@laGLDistAr)
          
          *E301192 AAMER (Start)
          *lnNxtStp = gfStyCrl('8',IIF(!EMPTY(cRetSty) , cRetSty , Style),;
                              laData[29],Dyelot,CrDate,CrMemo,@laAdjQty,0,;
                              '',lcVRGLSess,'',lnWhichLin,lcCrMemLin,'nSteps',@laGLDistAr,;
                              VAL(RetLine.cRet_LinNo))
          
          lnWhichLin = &lcCrMemLin..nSteps + 1
          PRIVATE lcRefer
          lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
          
          *E301224,1 Reham On 05/18/1999    *** Begin ***
          *E301224,1 Get the session # of the receiving from Style inventory journal file.
          lcRecSess = ""
          SELECT Styinvjl
          *E301224,1 Seek with the approperate style if the style was not 1st quality.
          lcCrSekSty = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.Style)
          IF SEEK(lcCrSekSty+laData[29] , "Styinvjl")
            *E301224,1 Scan for the current transaction code & transaction type.
            *B603149,1 Start, Change the scan expresion to be rush more afer adding the lineno.
            *SCAN REST WHILE Styinvjl.style+Styinvjl.cwarecode+Styinvjl.csession+;
                            DTOS(Styinvjl.dtrdate)+Styinvjl.ctrcode = ;
                            lcCrSekSty+laData[29] ;
                        FOR Styinvjl.ctrtype = "7" .AND. Styinvjl.ctrcode = laData[1]
            SCAN REST WHILE Styinvjl.style+Styinvjl.cwarecode+Styinvjl.csession+;
                            DTOS(Styinvjl.dtrdate)+Styinvjl.ctrcode+STR(LineNo,6) = ;
                            lcCrSekSty+laData[29] ;
                        FOR Styinvjl.ctrtype = "7" .AND. Styinvjl.ctrcode = laData[1]
            *B603149,1 End.
              lcRecSess = Styinvjl.csession
            ENDSCAN
          ENDIF
          SELECT RETLINE
          *E301224,1 Void credit memo with the receiving cost from the current line.
          *E301224,1 and the session # from style inventory journal.
          *lnNxtStp = gfStyCrl('8',IIF(!EMPTY(cRetSty) , cRetSty , Style),;
                              laData[29],Dyelot,CrDate,CrMemo,@laAdjQty,0,;
                              lcRefer,lcVRGLSess,'',lnWhichLin,lcCrMemLin,'nSteps',@laGLDistAr,;
                              VAL(RetLine.cRet_LinNo))
          *B606404,1 TMI [Start] Include only lines with type '2'
          IF RETLINE.CRET_TRNCD = '2'
            *B606404,1 TMI [End  ]  
            
            *T20060817.0014 TMI [START] run the lfStycrl that replaces the global one in BINMAIN to issue from the correct receiving session
            IF ASCAN(laEvntTrig,'RMSTYCRL')<>0 .AND. !EMPTY(gfGetMemVar('M_DLUSEBIN'))
              lnNextSt = gfDoTriger('RMCRMEM','RMSTYCRL') 
            ELSE
             *T20060817.0014 TMI [END]
            
              lnNxtStp = gfStyCrl('8',IIF(!EMPTY(cRetSty) , cRetSty , Style),;
                                  laData[29],Dyelot,CrDate,CrMemo,@laAdjQty,RetLine.Cost,;
                                  lcRefer,lcVRGLSess,'',lnWhichLin,lcCrMemLin,'nSteps',@laGLDistAr,;
                                  VAL(RetLine.cRet_LinNo) , lcRecSess)

              *T20060817.0014   TMI [Start] 
            ENDIF
            *T20060817.0014   TMI [End  ] 

            *B606404,1 TMI [Start] 
            *B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
            *C123853,1 MHM 01/15/2005  Void Invoice[Start]
            IF ASCAN(laEvntTrig , PADR('DLVODRM',10)) <> 0
              =gfDoTriger('RMCRMEM',PADR('DLVODRM',10)) 
            ENDIF  
            *C123853,1 MHM 01/15/2005  MHM [End]
            *B039660,1 NNA (End)
           
          ENDIF
          *B606404,1 TMI [End  ] 
          *E301224,1 Reham On 05/18/1999    *** End   ***
          *E301192 AAMER (End)
          
          *E301200 (AAMER) End
          *!B602322,1 End.
        
          *B602732,1 Reham On 04/04/99   *** Begin ***
          *B602732,1 Replace with the new step #.
          lnWhichLin = lnNxtStp
          *B602732,1 Reham On 04/04/99   *** End   ***
          
          *B602719,1 Reham On 04/05/99   *** Begin ***
          SELECT (lcCrMemLin)
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE nSteps WITH lnWhichLin
          UNLOCK
        ENDIF
        lnWhichLin = lnWhichLin + 1
        *B602719,1 Reham On 04/05/99   *** End   ***
      ENDIF
      
      *B602732,1 Reham On 04/04/99   *** Begin ***
      *B602732,1 Use the original variable instead of the var. updated from gfStyCrl
      *IF &lcCrMemLin..nSteps < lnNxtStp + 1
      IF &lcCrMemLin..nSteps < lnWhichLin
      *B602732,1 Reham On 04/04/99   *** End   ***
        *-- Update Return Authorization.
        *--HDM B602415,1 [Start] Return Int. Invoice prevents correct calculation of
        *--                       lnTotAdded & lnTotRecvd Remove this Condition
        *IF !EMPTY(laData[3]) .AND. laData[15] = "Y"
        IF !EMPTY(laData[3])
        *--HDM B602415,1 [End]
          SELECT RALINE
          SET ORDER TO RALINE
          DO CASE
            *-- If received line.
            CASE RETLINE.cRet_TrnCd = "2"
              IF SEEK(laData[3] + RETLINE.Style , "RALINE")
                SELECT RALINE
                FOR lnCount = 1 TO 8
                  lcCount = ALLTRIM(STR(lnCount))
                  lnTotAdded = lnTotAdded + RETLINE.Qty&lcCount
                  lnTotRecvd = lnTotRecvd + RETLINE.Qty&lcCount
                ENDFOR
                lnTotAmtAd  = lnTotAmtAd + (lnTotAdded * RETLINE.Price)
                *-- Lock the record to grantee the phiscal update.
                = RLOCK()
                *B606429,1 (Begin) Don't let the open be greater than the budget of RA when voiding CRs.
                *REPLACE nOpnQty1 WITH MAX(nOpnQty1 + RETLINE.Qty1 , RALINE.QTY1) ;
                        nOpnQty2 WITH MAX(nOpnQty2 + RETLINE.Qty2 , RALINE.QTY2) ;
                        nOpnQty3 WITH MAX(nOpnQty3 + RETLINE.Qty3 , RALINE.QTY3) ;
                        nOpnQty4 WITH MAX(nOpnQty4 + RETLINE.Qty4 , RALINE.QTY4) ;
                        nOpnQty5 WITH MAX(nOpnQty5 + RETLINE.Qty5 , RALINE.QTY5) ;
                        nOpnQty6 WITH MAX(nOpnQty6 + RETLINE.Qty6 , RALINE.QTY6) ;
                        nOpnQty7 WITH MAX(nOpnQty7 + RETLINE.Qty7 , RALINE.QTY7) ;
                        nOpnQty8 WITH MAX(nOpnQty8 + RETLINE.Qty8 , RALINE.QTY8) ;
                        nTotOpnQty WITH nOpnQty1 + nOpnQty2 + nOpnQty3 + nOpnQty4 + ;
                                        nOpnQty5 + nOpnQty6 + nOpnQty7 + nOpnQty8
                REPLACE nOpnQty1 WITH MIN(nOpnQty1 + RETLINE.Qty1 , RALINE.QTY1) ;
                        nOpnQty2 WITH MIN(nOpnQty2 + RETLINE.Qty2 , RALINE.QTY2) ;
                        nOpnQty3 WITH MIN(nOpnQty3 + RETLINE.Qty3 , RALINE.QTY3) ;
                        nOpnQty4 WITH MIN(nOpnQty4 + RETLINE.Qty4 , RALINE.QTY4) ;
                        nOpnQty5 WITH MIN(nOpnQty5 + RETLINE.Qty5 , RALINE.QTY5) ;
                        nOpnQty6 WITH MIN(nOpnQty6 + RETLINE.Qty6 , RALINE.QTY6) ;
                        nOpnQty7 WITH MIN(nOpnQty7 + RETLINE.Qty7 , RALINE.QTY7) ;
                        nOpnQty8 WITH MIN(nOpnQty8 + RETLINE.Qty8 , RALINE.QTY8) ;
                        nTotOpnQty WITH nOpnQty1 + nOpnQty2 + nOpnQty3 + nOpnQty4 + ;
                                        nOpnQty5 + nOpnQty6 + nOpnQty7 + nOpnQty8
                *B606429,1 (End)
                UNLOCK
                *-- Call Global Function to transmit the local data.
                =gfTraceKey("RALINE" , RANO+STYLE+CRA_LINNO , "M")
              ENDIF
            *-- If canceled line.
            CASE &lcCrMemLin..cRet_TrnCd = "4"
              lnTotCancl = lnTotCancl + &lcCrMemLin..TotQty
          ENDCASE
        ENDIF
        SELECT (lcCrMemLin)
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        
        *B602732,1 Reham On 04/04/99   *** Begin ***
        *B602732,1 Use the original variable instead of the var. updated from gfStyCrl
        *REPLACE nSteps WITH lnNxtStp + 1
        REPLACE nSteps WITH lnWhichLin
        *B602732,1 Reham On 04/04/99   *** End   ***
        UNLOCK
      ENDIF
      
      *B602732,1 Reham On 04/04/99   *** Begin ***
      *B602732,1 Use the original variable instead of the var. updated from gfStyCrl
      lnWhichLin = lnWhichLin + 1
      *IF &lcCrMemLin..nSteps < lnNxtStp + 2
      IF &lcCrMemLin..nSteps < lnWhichLin
      *B602732,1 Reham On 04/04/99   *** End   ***
        IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)
          *E301077,4 Reham On 12/27/98   *** Begin ***
          *E301077,4 Open the style history file.
          =gfOpenFile(gcDataDir+'ICSTYHST',gcDataDir+'STYHST','SH')
          *E301077,4 Reham On 12/27/98   *** End   ***
          
          lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )
          *-- Update the style history file for the current line.
          SELECT ICSTYHST
          IF SEEK(RETLINE.Style + lcTrYear , "ICSTYHST")
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            REPLACE nRetAmt            WITH nRetAmt - ROUND(RETLINE.Amount &lcExRSin laData[33] &lcUntSin laData[32],2) ;
                    nRetQty            WITH nRetQty - RETLINE.TOTQTY ;
                    nDisAmt            WITH nDisAmt + ROUND(RETLINE.Disc_Amt &lcExRSin laData[33] &lcUntSin laData[32],2) ;
                    nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod - ROUND(RETLINE.Amount &lcExRSin laData[33] &lcUntSin laData[32],2) ;
                    nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod - RETLINE.TOTQTY ;
                    nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod + ROUND(RETLINE.Disc_Amt &lcExRSin laData[33] &lcUntSin laData[32],2)
            UNLOCK
             *-- Call Global Function to transmit the local data.
            =gfTraceKey("ICSTYHST" , RETLINE.Style + lcTrYear , "M")
          ENDIF
          *E301077,4 Reham On 12/27/98   *** Begin ***
          =gfCloseFile('ICSTYHST')
          *E301077,4 Reham On 12/27/98   *** End   ***
        ENDIF
        SELECT (lcCrMemLin)
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        *B602732,1 Reham On 04/04/99   *** Begin ***
        *B602732,1 Use the original variable instead of the var. updated from gfStyCrl
        *REPLACE nSteps WITH lnNxtStp + 2
        REPLACE nSteps WITH lnWhichLin
        *B602732,1 Reham On 04/04/99   *** End   ***
        UNLOCK
      ENDIF
      
      *B602732,1 Reham On 04/04/99   *** Begin ***
      *B602732,1 Update the step counter.
      lnWhichLin = lnWhichLin + 1
      *B602732,1 Reham On 04/04/99   *** End   ***
      
      SELECT RETLINE
    ENDSCAN
    
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF

  IF &lcCrMemHdr..nSteps < 3
    *-- Update the RA header file with the changed fields.
    
    SELECT RETAUTH
    SET ORDER TO TAG RETAUTHA
    IF SEEK(laData[2] + laData[3])
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      *--HDM B602415,1 [Start] Incorrect open qty , open amount calculations 

      *REPLACE TRAN       WITH IIF(TRAN = RETHDR.CrMemo , "" , TRAN) ;
              nReta_opn  WITH MIN(nreta_opn + lnTotAdded , nreta_opn) ;
              nRtOpnAmt  WITH MIN(nrtopnamt + lnTotAmtAd , nrtopnamt) ;
              nReta_rec  WITH MAX(nreta_rec - lnTotRecvd , 0) ;
              nReta_can  WITH MAX(nreta_can - lnTotCancl , 0) ;
              STATUS     WITH IIF(STATUS = "C" , "O" , STATUS)
      *B606429,4 ABD - The status of R/A does not alert when Void and the user didn't 
      *B606429,4 ABD - need to rebuild the R/A. [Begin]
      *REPLACE TRAN       WITH IIF(TRAN = RETHDR.CrMemo , "" , TRAN) ;
              nReta_opn  WITH MIN(nreta_opn + lnTotAdded , nreta_bud) ;
              nRtOpnAmt  WITH MIN(nrtopnamt + lnTotAmtAd , AUTHAMT) ;
              nReta_rec  WITH MAX(nreta_rec - lnTotRecvd , 0) ;
              nReta_can  WITH MAX(nreta_can - lnTotCancl , 0) ;
              STATUS     WITH IIF(STATUS = "C" , "O" , STATUS)
      
      REPLACE TRAN       WITH IIF(TRAN = RETHDR.CrMemo , "" , TRAN) ;
              nReta_opn  WITH MIN(nreta_opn + lnTotAdded , nreta_bud) ;
              nRtOpnAmt  WITH MIN(nrtopnamt + lnTotAmtAd , AUTHAMT) ;
              nReta_rec  WITH MAX(nreta_rec - lnTotRecvd , 0) ;
              nReta_can  WITH MAX(nreta_can - lnTotCancl , 0) ;
              STATUS     WITH IIF(llReBld_RA,IIF(STATUS = "C" , "O" , STATUS),STATUS)
      *B606429,4 ABD - [End]
      *B606193,1 (Begin) Drecrease Retun qty\Amount in R\A header file when Voiding CR.
      *--Decrease the Returned fileds with the CR Qty\Amount
      REPLACE RETURN     WITH MAX(RETURN    - lnTotAdded, 0) ;
              RETURNAMT  WITH MAX((MIN(RETURNAMT - lnTotAmtAd , AUTHAMT)),0),;
              RETDATE    WITH IIF(RETURN = 0,{},RETDATE)
      *B606193,1 (End)
              
      *--HDM B602415,1 [End]
      UNLOCK
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("RETAUTH" , RANO , "M")
    ENDIF
    SET ORDER TO TAG RETAUTH
    
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
  
  *-- [COMMISSIONS] Salesreps commissions / chargebacks.
  IF &lcCrMemHdr..nSteps < 4
    SELECT RETHDR
    IF RETHDR.COMMAMT1 + RETHDR.COMMAMT2 <> 0
      *E301077,4 Reham On 12/27/98   *** Begin ***
      *E301077,4 Open the salesreps file.
      =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
      *E301077,4 Reham On 12/27/98   *** End   ***
      
      FOR lnCount = 1 To 2
        lcCount = ALLTRIM(STR(lnCount))
        IF !EMPTY(RETHDR.SALESREP&lcCount) .AND. RETHDR.COMMPCNT&lcCount > 0
          
          lnCommDue = ABS(RETHDR.COMMAMT&lcCount)
          
          *-- Update commissions for salesreps.
          SELECT SALESREP
          IF SEEK(RETHDR.SALESREP&lcCount)
          
            *-- Calculated the equivalent amount.
            lnFrnAmnt = lnCommDue
            lnCommDue  = ROUND(lnCommDue &lcExRSin laData[33] &lcUntSin laData[32],2)
            
            lnNewBal = SALESREP.BALANCE + lnCommDue
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            REPLACE CURRENT  WITH CURRENT + lnCommDue ;
                    BALANCE  WITH lnNewBal
            UNLOCK
            *-- Call Global Function to transmit the local data.
            =gfTraceKey("SALESREP" , REPCODE , "M")
            
            IF RETHDR.TOTCREDIT <> 0
              *E301077,4 Reham On 12/27/98   *** Begin ***
              *E301077,4 Open the salesreps commission file.
              =gfOpenFile(gcDataDir+'REPCOMM',gcDataDir+'REPCOMM','SH')
              *E301077,4 Reham On 12/27/98   *** End   ***
              
              SELECT REPCOMM
              APPEND BLANK
              *-- Lock the record to grantee the phiscal update.
              = RLOCK()
              REPLACE STATUS     WITH 'O' ;
                      REPCODE    WITH RETHDR.SALESREP&lcCount ;
                      ACCOUNT    WITH RETHDR.ACCOUNT ;
                      ORDER      WITH RETHDR.ORDER ;
                      DATE       WITH RETHDR.CrDate ;
                      TRANTYPE   WITH '7' ;
                      DESC       WITH 'C/B-VOID CR/ME' ;
                      TRAN       WITH RETHDR.CRMEMO ;
                      CUSTPO     WITH RETHDR.CUSTPO ;
                      COMMPCNT   WITH RETHDR.CommPcnt&lcCount ;
                      AMOUNT     WITH lnCommDue ;
                      BALANCE    WITH lnNewBal ;
                      cCurrCode  WITH RETHDR.cCurrCode ;
                      nExRate    WITH RETHDR.nExRate ;
                      nCurrUnit  WITH RETHDR.nCurrUnit ;
                      nForAmnt   WITH lnFrnAmnt
              *-- Call global function to add audit fields info.
              =gfAdd_Info("REPCOMM")
              UNLOCK
              *-- Call Global Function to transmit the local data.
              =gfTraceKey("REPCOMM" , REPCODE+DTOS(DATE)+TRAN+TRANTYPE , "A")
              
              *E301077,4 Reham On 12/27/98   *** Begin ***
              =gfCloseFile('REPCOMM')
              *E301077,4 Reham On 12/27/98   *** End   ***
            ENDIF
          ENDIF
        ENDIF
      ENDFOR
    ENDIF
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF

  *C102676,1 Custom process for A.S.T. [Begin]
  IF ASCAN(laEvntTrig,PADR('VMEMREP3',10))<>0
    =gfDoTriger('RMCRMEM',PADR('VMEMREP3',10))
  ENDIF  
  *C102676,1 Custom process for A.S.T. [End]
  
  *-- Update transaction header & lines to permanent files
  *-- Updating permanent transaction files.

  *** CALL GL DISTRIBUTION PROCEDURE THREE TIMES FOR THIS RETURN .
  IF llLink_GL
    
    *B602719,1 Reham On 04/05/99   *** Begin ***
    IF &lcCrMemHdr..nSteps < 5
    *B602719,1 Reham On 04/05/99   *** End   ***
      *B602688,1 Reham On 03/21/99   *** Begin ***
      *B602688,1 Send the freight account to the function.
      *-- 1) FREIGHT + INSURANCE + COD       <CREDIT>  FREIGHT INCOME
      *-- CATEGORY KEY FOR "Freight"................=> '004'
      *DO GLDIST WITH RETHDR.LINK_CODE , '004' , -(RETHDR.OTHER) , 'VR' , ;
                     RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                     '&lcGLDstTmp' , '' , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                     RETHDR.nExRate
       
       *B607195,1 KHM 06/03/2003 (Begin) Updating the gldist with the charges in case of England
       *DO GLDIST WITH RETHDR.LINK_CODE , '004' , -(RETHDR.OTHER) , 'VR' , ;
                     RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                     '&lcGLDstTmp' , RETHDR.cfrgtacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                     RETHDR.nExRate
      IF llIsEnglnd 
        =gfOpenFile(gcDataDir+'CrmChrg',gcDataDir+'CrmChrg','SH')
        IF SEEK(RetHdr.CrMemo+RetHdr.Store, 'CrmChrg')
           SELECT CrmChrg
           SCAN REST WHILE CrMemo+Store+cChrgCode= RetHdr.CrMemo+RetHdr.Store
             DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004', -(CrmChrg.nChrgAmnt), 'VR' ,;
                            RetHdr.CRMEMO , ldVdDate , lcGLFYear, lcGLPeriod ,;
                            '&lcGLDstTmp' , CrmChrg.cFrgtAcnt , RetHdr.cCurrCode ,;
                            RetHdr.nCurrUnit , RetHdr.nExRate
           ENDSCAN
        ENDIF 
      ELSE
        DO GLDIST WITH RETHDR.LINK_CODE , '004' , -(RETHDR.OTHER) , 'VR' , ;
                       RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                       '&lcGLDstTmp' , RETHDR.cfrgtacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                       RETHDR.nExRate
      
      ENDIF   
      *B607195,1 KHM 06/03/2003 (End)
      
      *B602719,1 Reham On 04/05/99   *** Begin ***
      SELECT (lcCrMemHdr)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    *B602719,1 Reham On 04/05/99   *** End   ***
    
    *-- IF THE SYSTEM HAS BEEN SETUP TO COMPUTE TAX.
    IF llTax
      *B602719,1 Reham On 04/05/99   *** Begin ***
      IF &lcCrMemHdr..nSteps < 6
      *B602719,1 Reham On 04/05/99   *** End   ***
        *B602688,1 Send the discount account to the function.
        *-- 2) SALES TAX LIABILITY           <CREDIT>  TAX INCOME
        *-- CATEGORY KEY FOR "Sales tax liability"..=> '014'
        *DO GLDIST WITH RETHDR.LINK_CODE , '014' , ;
                        -(RETHDR.TAX_AMT + RETHDR.nPstAmt) , 'VR' , ;
                        RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                        '&lcGLDstTmp' , '' , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                        RETHDR.nExRate
        *C102212,1 (Begin) Update HST Tax .
        *DO GLDIST WITH RETHDR.LINK_CODE , '014' , ;
                       -(RETHDR.TAX_AMT + RETHDR.nPstAmt) , 'VR' , ;
                       RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                       '&lcGLDstTmp' , RETHDR.ctaxacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                       RETHDR.nExRate

        *E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]
        *DO GLDIST WITH RETHDR.LINK_CODE , '014' , ;
                       -(RETHDR.TAX_AMT + RETHDR.nPstAmt+RETHDR.nHstAmt) , 'VR' , ;
                       RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                       '&lcGLDstTmp' , RETHDR.ctaxacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                       RETHDR.nExRate
        IF llIsCanada
            llRpHSTTax = .F.
            =lfGetHST()
            IF !llRpHSTTax
              DO GLDIST WITH RETHDR.LINK_CODE , '014' , ;
                           -RETHDR.TAX_AMT  , 'VR' , ;
                           RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                           '&lcGLDstTmp' , RETHDR.ctaxacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                           RETHDR.nExRate


              DO GLDIST WITH RETHDR.LINK_CODE , '027' , ;
                           - RETHDR.nPstAmt , 'VR' , ;
                           RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                           '&lcGLDstTmp' , RETHDR.ctaxacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                           RETHDR.nExRate

            ELSE
              DO GLDIST WITH RETHDR.LINK_CODE , '028' , ;
                           -RETHDR.nHstAmt , 'VR' , ;
                           RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                           '&lcGLDstTmp' , RETHDR.ctaxacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                           RETHDR.nExRate
          
            ENDIF
          ELSE
          
            DO GLDIST WITH RETHDR.LINK_CODE , '014' , ;
                         -(RETHDR.TAX_AMT + RETHDR.nPstAmt+RETHDR.nHstAmt) , 'VR' , ;
                         RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                         '&lcGLDstTmp' , RETHDR.ctaxacnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                         RETHDR.nExRate
        ENDIF
        *E302472,1 MHM [End]
        
        *C102212,1 (End)
        *B602719,1 Reham On 04/05/99   *** Begin ***
        SELECT (lcCrMemHdr)
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        REPLACE nSteps WITH 6
        UNLOCK
      ENDIF
      *B602719,1 Reham On 04/05/99   *** End   ***
    ENDIF

    *B602719,1 Reham On 04/05/99   *** Begin ***
    IF &lcCrMemHdr..nSteps < 7
    *B602719,1 Reham On 04/05/99   *** End   ***
      *B602688,1 Send the AR account to the function.
      *-- 3) TOTAL CHARGING                  <DEBIT>  ACCOUNT RECEIVABLE
      *-- CATEGORY KEY FOR "Accounts Receivable"....=> '001'
      *DO GLDIST WITH RETHDR.LINK_CODE , '001' , RETHDR.TOTCREDIT , 'VR' , ;
                      RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                      '&lcGLDstTmp' , '' , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                      RETHDR.nExRate
      DO GLDIST WITH RETHDR.LINK_CODE , '001' , RETHDR.TOTCREDIT , 'VR' , ;
                     RETHDR.CRMEMO , ldVdDate , lcGLFYear , lcGLPeriod , ;
                     '&lcGLDstTmp' , RETHDR.caracnt , RETHDR.cCurrCode , RETHDR.nCurrUnit , ;
                     RETHDR.nExRate
      *B602688,1 Reham On 03/21/99   *** End   ***
      *B602719,1 Reham On 04/05/99   *** Begin ***
      SELECT (lcCrMemHdr)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE nSteps WITH 7
      UNLOCK
    ENDIF
    *B602719,1 Reham On 04/05/99   *** End   ***
  ENDIF
  
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *IF &lcCrMemHdr..nSteps < 5
  IF &lcCrMemHdr..nSteps < 8
  *B602719,1 Reham On 04/05/99   *** End   ***
    *-- Update the voided fields in the header amount.
    SELECT RETHDR
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE STATUS     WITH 'V' ;
            VDATE      WITH ldVdDate ;
            VGROSS_AMT WITH GROSS_AMT ;
            GROSS_AMT  WITH 0 ;
            VDISC_AMT  WITH DISC_AMT ;
            DISC_AMT   WITH 0 ;
            VPIECES    WITH PIECES ;
            PIECES     WITH 0 ;
            VOTHER     WITH OTHER ;
            OTHER      WITH 0 ;
            VTOTCREDIT WITH TOTCREDIT ;
            TOTCREDIT  WITH 0 ;
            VCOMMAMT1  WITH COMMAMT1 ;
            COMMAMT1   WITH 0 ;
            VCOMMAMT2  WITH COMMAMT2 ;
            COMMAMT2   WITH 0 ;
            VAMOUNT    WITH AMOUNT ;
            AMOUNT     WITH 0

    *C102676,1 Custom process for A.S.T. sportwear. [Begin]
    IF ASCAN(laEvntTrig,PADR('VOIDCOM3',10))<>0
      =gfDoTriger('ARDINV',PADR('VOIDCOM3',10))
    ENDIF  
    *C102676,1 Custom process for A.S.T. sportwear. [End]
            
    UNLOCK
    *-- Call Global Function to transmit the local data.
    =gfTraceKey("RETHDR" , CRMEMO , "M")
    
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B602719,1 Reham On 04/05/99   *** Begin ***
    *REPLACE nSteps WITH 5
    REPLACE nSteps WITH 8
    *B602719,1 Reham On 04/05/99   *** End   ***
    UNLOCK
  ENDIF
  
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *IF &lcCrMemHdr..nSteps < 6
  IF &lcCrMemHdr..nSteps < 9
  *B602719,1 Reham On 04/05/99   *** End   ***
    *-- Update the General Ledger Distribution  file.
    IF llLink_GL
      *-- Updating General Ledger Distribution File.
      *-- GENERATE A UNIQUE SESSION NUMBER.
      
      *-- If a session # allready generated do not generate onther one
      *!B602322,1 comment this part since it moved at the bigining of saving.
      *IF !llVRGLSess
      *  lcVRGLSess = gfsequence('GLSESSION')
      *  llVRGLSess = .T.
      *ENDIF
      *!B602322,1 End.
      
      *B602719,1 Reham On 04/05/99   *** Begin ***
      *-- Call local function to update the temp. files & needed variables 
      *-- fields in the uncompleted session file record.
      *=lfUpdVars()
      *B602719,1 Reham On 04/05/99   *** End   ***
      
      *-- Update the gl distribution file with the created entries.
      SELECT (lcGLDstTmp)
      *B602719,1 Reham On 04/06/99   *** Begin ***
      *REPLACE ALL GLSESSION WITH lcVRGLSess
      REPLACE ALL GLSESSION WITH lcVRGLSess ;
                  cAdd_User WITH gcUser_ID  ;
                  dAdd_Date WITH DATE()     ;
                  cAdd_Time WITH gfGetTime()
      *B602719,1 Reham On 04/06/99   *** End   ***
      USE
      SELECT GLDIST
      APPEND FROM (gcWorkDir+lcGLDstTmp)
      
      *B602719,1 Reham On 04/05/99   *** Begin ***
      *B602719,1 Recreate the GLDIST file instead of erasing it..
      *-- Erase the temp. file that hold the gl distribution records.
      *ERASE &gcWorkDir.&lcGLDstTmp..DBF
      *ERASE &gcWorkDir.&lcGLDstTmp..CDX

      *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
      *E301490,1 (Start)
      *=lfCrtUnComp(.F. , .F. , .T.)
      DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .F. , .F.,.T.
      *E301490,1 (End)

      *B602719,1 Reham On 04/05/99   *** END   ***
      
    ENDIF
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B602719,1 Reham On 04/05/99   *** Begin ***
    *REPLACE nSteps WITH 6
    REPLACE nSteps WITH 9
    *B602719,1 Reham On 04/05/99   *** End   ***
    UNLOCK
  ENDIF
  
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *IF &lcCrMemHdr..nSteps < 7
  IF &lcCrMemHdr..nSteps < 10
  *B602719,1 Reham On 04/05/99   *** End   ***
    IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)
      *E301077,4 Reham On 12/27/98   *** Begin ***
      *E301077,4 Open the customer history file.
      =gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
      *E301077,4 Reham On 12/27/98   *** End   ***
      
      lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )
      *-- Update the customer history file.
      SELECT ARCUSHST
      IF SEEK(RETHDR.Account + lcTrYear , "ARCUSHST")
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        REPLACE nRetAmt            WITH nRetAmt - ROUND(RETHDR.vGross_Amt &lcExRSin laData[33] &lcUntSin laData[32],2) ;
                nRetQty            WITH nRetQty - RETHDR.vPieces ;
                nDisAmt            WITH nDisAmt + ROUND(RETHDR.vDisc_Amt &lcExRSin laData[33] &lcUntSin laData[32],2) ;
                nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod - ROUND(RETHDR.vGross_Amt &lcExRSin laData[33] &lcUntSin laData[32],2) ;
                nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod - RETHDR.vPieces ;
                nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod + ROUND(RETHDR.vDisc_Amt &lcExRSin laData[33] &lcUntSin laData[32],2)
        UNLOCK
        *-- Call Global Function to transmit the local data.
        =gfTraceKey("ARCUSHST" , RETHDR.Account + lcTrYear , "M")
      ENDIF
      *E301077,4 Reham On 12/27/98   *** Begin ***
      =gfCloseFile('ARCUSHST')
      *E301077,4 Reham On 12/27/98   *** End   ***
    ENDIF
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B602719,1 Reham On 04/05/99   *** Begin ***
    *REPLACE nSteps WITH 7
    REPLACE nSteps WITH 10
    *B602719,1 Reham On 04/05/99   *** End   ***
    UNLOCK
  ENDIF
ENDIF

*B602719,1 Reham On 04/05/99  *** Begin ***
*-- Create the temp files.
*=lfCrtUnComp(.T. , .T.)

*E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
*E301490,1 (Start)
*=lfCrtUnComp(.T. , .T. , .T.)
DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .T. , .T.,.T.
*E301490,1 (End)

*B602719,1 Reham On 04/05/99  *** End   ***

*E301216,1 Reham On 05/04/99   *** Begin ***
=gfCloseFile('ARHIST')
=gfCloseFile('DEBIT')
*E301216,1 Reham On 05/04/99   *** End   ***

*-- Finish the whole session.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF
lnUnCmSeRc = 0

SELECT RETHDR
SCATTER FIELDS &lcScFields TO laData

*!*************************************************************
*! Name      : lfvVdDate
*! Developer : Reham Al-Allamy
*! Date      : 07/01/1998
*! Purpose   : Valid function for void credit memo date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVdDate()
*!*************************************************************
*
FUNCTION lfvVdDate

*B602773,1 Reham On 04/11/99   *** Begin ***
*IF ldVdDate < laData[41]
IF ldVdDate < laData[5]
*B602773,1 Reham On 04/11/99   *** End   ***
  *** Void date has to be greater than credit memo posting date. ***
  *** < OK > ***
  =gfModalGen("INM46031B00000" , "DIALOG")
  ldVdDate   = ldOldVoid
  llCancVoid = .T.
  SHOW GET pbVdOk DISABLE
  _CurObj    = OBJNUM(ldVdDate)
ELSE
  llCancVoid = .F.
  SHOW GET pbVdOk ENABLE
ENDIF

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Reham Al-Allamy
*! Date      : 06/23/1996
*! Purpose   : Cancel new credit memo.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
*
FUNCTION lpClsScr

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.
llContinue = .F.

*-- Save status "I" for the ignored record in the uncompleted session file.
SELECT unCmSess
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'I'
  UNLOCK
ENDIF
lnUnCmSeRc = 0

*-- Blank the object on the screen.
SELECT RetHdr
SCATTER FIELDS &lcScFields TO laData BLANK

*!*************************************************************
*! Name      : lfchgTax
*! Developer : Reham Al-Allamy
*! Date      : 06/02/1997
*! Purpose   : Calculate the charge taxes in case of country 
*!           : is England.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfchgTax()
*!*************************************************************
*
FUNCTION lfchgTax

*-- Calculate the tax charges if the country code is england and the 
*-- invoice no. is not empty.
STORE 0 TO lnTChgTax
*B607008,1 ASH 03/02/2003 (Begin) Save old Alias.
lcOldAlias = ALIAS()
*B607008,1 ASH 03/02/2003 (End)
IF llIsEnglnd AND lcTax_Meth = "A" AND !EMPTY(&lcCrMemHdr..Invoice) AND &lcCrMemHdr..cintr_inv = "Y"
  
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the invoice header & charges files.
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  =gfOpenFile(gcDataDir+'INVCHRG',gcDataDir+'INVCHRG','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***

  IF SEEK(&lcCrMemHdr..Invoice , "INVCHRG")
    *B607008,1 ASH 03/02/2003 (Begin) Comment out and move it at the beginning of the function.
    *lcOldAlias = ALIAS()
    *B607008,1 ASH 03/02/2003 (End)
    SELECT INVHDR
    SET ORDER TO TAG INVHDR
    =SEEK(&lcCrMemHdr..Invoice)
    SELECT INVCHRG
    SCAN WHILE INVCHRG.Invoice = &lcCrMemHdr..Invoice
      lnChgTax  = INVCHRG.nChrgAmnt * INVCHRG.nTaxRate/100
      *lnChgTax  = lnChgTax  - (lnChgTax * INVHDR.Trde_Disc/100)

      *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
      *E301490,1                and add new parameter to get the return value
      *E301490,1 (Start)
      *lnChgTax  = lnChgTax  - (lnChgTax * lfGetTrdDs(&lcCrMemHdr..cTermCode , &lcCrMemHdr..Invoice)/100)
      DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH &lcCrMemHdr..cTermCode , &lcCrMemHdr..Invoice , lnInvTrdDs
      lnChgTax  = lnChgTax  - (lnChgTax * lnInvTrdDs/100)
      *E301490,1 (End)

      lnTChgTax = lnTChgTax + lnChgTax
    ENDSCAN
  ENDIF
ENDIF
SELECT (lcOldAlias)
RETURN (lnTChgTax)

*!*************************************************************
*! Name      : lfMechDisc
*! Developer : Reham Al-Allamy
*! Date      : 05/20/1997
*! Purpose   : Get the Tax for the current style.
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Parameters: lcRetLnSty-> Style code.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfMechDisc(lcRetLnSty)
*!*************************************************************
*
FUNCTION lfMechDisc
PARAMETERS lcRetLnSty
PRIVATE lcRetLnSty , lnTaxRate

lnTaxLine = 0

=SEEK(lcRetLnSty , "STYLE")
IF llIsEnglnd .AND. !llVatExem .AND. Style.nTaxBreak <> 0
  DECLARE laEngStyTax[1,2]
  laEngStyTax[1,1] = 'NTAXRATE'
  laEngStyTax[1,2] = 'lnTaxLine'
  =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  
ENDIF

RETURN (lnTaxLine)

*!*************************************************************
*! Name      : lfCurTax
*! Developer : Reham Al-Allamy
*! Date      : 09/28/1997
*! Purpose   : Compute England Merchendise tax.
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCurTax()
*!*************************************************************
*
FUNCTION lfCurTax
PARAMETER lcStyle , lnPrice , lcFile
PRIVATE lcStyle , lnPrice , lcFile , lnCurTax , lnQuantity
STORE 0 TO lnCurTax , lnQuantity


IF SEEK(lcStyle,'Style') .AND. llIsEnglnd .AND. !llVatExem .AND. Style.nTaxBreak <> 0
  DECLARE laEngStyTax[1,2]
  laEngStyTax[1,1] = 'NTAXRATE'
  laEngStyTax[1,2] = 'lnTaxLine'
  =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  
  IF lnTaxLine <> 0
    FOR lnCount = Style.nTaxBreak TO 8
      lcCount    = STR(lnCount,1)
      lnQuantity = lnQuantity + &lcFile..Qty&lcCount
    ENDFOR
    *lnCurTax = lnQuantity * lnPrice * (lnTaxLine / 100) * ((100 - lnInvTrdDs)/100) * ((100 - lnDiscPcnt)/100)
    *B607994,1 TMI [Start] calculate the tax on the price rounded after discount
    *lnCurTax = lnQuantity * lnPrice * (lnTaxLine / 100) * ((100 - lnInvTrdDs)/100) * ((100 - lnDiscLNPcnt)/100)
    lnCurTax = lnQuantity*ROUND(lnPrice*(1-lnDiscLNPcnt/100),2) * (lnTaxLine/100) * (1-lnInvTrdDs/100) 
    lnCurTax = ROUND(lnCurTax,2)
    *B607994,1 TMI [End  ] 
  ENDIF
ENDIF

RETURN lnCurTax

*!*************************************************************
*! Name      : lfTotalTax
*! Developer : Reham Al-Allamy
*! Date      : 06/12/1997
*! Purpose   : Compute England Merchendise tax.
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpDelScr()
*!*************************************************************
*
FUNCTION lfTotalTax
PRIVATE lnMerchTax , lnQuantity , lnAmount
STORE 0 TO lnMerchTax

SELECT (lcCrMemLin)
GO TOP
SCAN
  STORE 0 TO lnQuantity , lnAmount
  
  =SEEK(&lcCrMemLin..Style,'Style')
  IF llIsEnglnd .AND. !llVatExem .AND. Style.nTaxBreak <> 0
    DECLARE laEngStyTax[1,2]
    laEngStyTax[1,1] = 'NTAXRATE'
    laEngStyTax[1,2] = 'lnTaxLine'
    =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  

    FOR lnCount = Style.nTaxBreak TO 8
      lcCount    = STR(lnCount,1)
      lnQuantity = lnQuantity + &lcCrMemLin..Qty&lcCount
    ENDFOR
    
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *-- Calculate trde_amt for break qty only if company is England.
    REPLACE Trde_Amt WITH (lnQuantity * &lcCrMemLin..Price) * lnInvTrdDs/100
    UNLOCK
    
    *-- Commented out & Get vat % from retltemp file instead of SycCodes
    lnAmount   = (lnQuantity * &lcCrMemLin..Price) - &lcCrMemLin..Trde_Amt
    lnMerchTax = lnMerchTax + (lnAmount * lnTaxLine / 100)
  ENDIF
  SELECT (lcCrMemLin)
ENDSCAN
RETURN lnMerchTax

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Reham Al-Allamy
*! Date      : 06/25/1996
*! Purpose   : Restore credit memo information
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
*
FUNCTION lfGetInfo

*B802088,1 Reham On 04/07/99  *** Begin ***
*B802088,1 Pick the store address in the from address if the store field 
*B802088,1 is not empty.
IF (!EMPTY(laData[4]) .AND. SEEK('S'+laData[2]+laData[4] , 'CUSTOMER')) .OR. ;
   (EMPTY(laData[4]) .AND.  SEEK('M'+laData[2],'CUSTOMER'))
*-- Get the customer information.
*IF SEEK("M"+laData[2],"CUSTOMER")

  *B605881,1 ABD Get the main account addresses or main store addresses not 
  *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
  *lcAddress1 = CUSTOMER.cAddress12
  *lcAddress2 = CUSTOMER.cAddress22
  *lcAddress3 = CUSTOMER.cAddress32
  *lcAddress4 = SUBSTR(CUSTOMER.cAddress42,1,10)
  *lcAddress5 = SUBSTR(CUSTOMER.cAddress52,1,10)
  *lcAddress6 = CUSTOMER.cAddress62
  lcAddress1 = CUSTOMER.cAddress1
  lcAddress2 = CUSTOMER.cAddress2
  lcAddress3 = CUSTOMER.cAddress3
  lcAddress4 = SUBSTR(CUSTOMER.cAddress4,1,10)
  lcAddress5 = SUBSTR(CUSTOMER.cAddress5,1,10)
  lcAddress6 = CUSTOMER.cAddress6
  *B605881,1 ABD [End]
  
ELSE
  STORE " " TO lcAddress1,lcAddress2,lcAddress3,;
               lcAddress4,lcAddress5,lcAddress6
ENDIF

*B802088,1 Reham On 04/07/99  *** Begin ***
IF SEEK("M"+laData[2],"CUSTOMER")
  lcCstCntCd = IIF(EMPTY(CUSTOMER.cCont_Code),ALLTRIM(gcContCode),CUSTOMER.cCont_Code)
  lcAccName  = CUSTOMER.BtName
  llVatExem  = CUSTOMER.lVatExem
ELSE
  lcCstCntCd = ALLTRIM(gcContCode)
  lcAccName  = ""
  llVatExem  = .F.
ENDIF
*B802088,1 Reham On 04/07/99  *** End   ***

*B603666,1 Ramy On 06/20/2000   Begin
IF !EMPTY(laData[4]) .AND. SEEK("S"+laData[2]+laData[4],"CUSTOMER")
  llVatExem  = CUSTOMER.lVatExem
ENDIF
*B603666,1 Ramy On 06/20/2000   End

*-- Get the status variable.
lcStatus   = IIF(laData[6] = "V" , "Voided" , "Active")
*-- Fill the check box for intire invoice with its value.
cbEntr_Inv = IIF(laData[15] = "Y" , .T. , .F.)
*-- Get the R/A info.
IF SEEK(laData[3] , "RETAUTH")
  ldRaDate  = RETAUTH.RaDate
  ldRaVoid  = RETAUTH.Void
  lnAuthPcs = RETAUTH.nreta_opn
  lnAuthAmt = RETAUTH.nRtOpnAmt
  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Variables hold the RA qty. & amount.
  lnRAQty = RETAUTH.nreta_opn
  lnRAAmt = RETAUTH.nRtOpnAmt
  *B802907,1 Reham On 12/26/1999   *** End   ***
ELSE
  STORE {}  TO ldRaDate , ldRaVoid
  STORE 0   TO lnAuthPcs , lnAuthAmt
  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Variables hold the RA qty. & amount.
  STORE 0 TO lnRAQty , lnRAAmt 
  *B802907,1 Reham On 12/26/1999   *** End   ***
ENDIF
*B122824,1 NNA 05/26/2004 (Begin) Get the array Position for the Credit memo's Location
*B122824,1 NNA              and then get the line of the array for this Position 
*puWareHous = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29])/2 , 0)
lnArPos = IIF(ASCAN(laWareHous,laData[29]) > 0 , ASCAN(laWareHous,laData[29]) , 0)
puWareHous = ASUBSCRIPT(laWareHous,lnArPos,1)
*B122824,1 NNA (End)
*-- Fill codes popups with its real values.
= gfwCodePop(@laCodInfo, "CTERMCODE" , "V,"+laData[40])
= gfwCodePop(@laCodInfo, "REASON"    , "V,"+laData[13])
= gfwCodePop(@laCodInfo, "CDIVISION" , "V,"+laData[14])

*-- Call local function to get the trade discount.

*E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
*E301490,1                and add new parameter to get the return value
*E301490,1 (Start)
*lnInvTrdDs = lfGetTrdDs(laData[40] , laData[7])
DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH laData[40] , laData[7] , lnInvTrdDs
*E301490,1 (End)

*!*************************************************************
*! Name      : lfGetLines
*! Developer : Reham Al-Allamy
*! Date      : 02/02/1998
*! Purpose   : Get all the Credit memo lines into the detail browse.
*!*************************************************************
*! Calls     : lfGetRALn, lfGetInvLn, 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetLines()
*!*************************************************************
*
FUNCTION lfGetLines


DO CASE
  *-- If enter RA # without invoice # or the invoice # did not change
  *-- Get the entered RA # lines into the browse.
  *B802736,1 [start] We will collect R/A data even we have valid invoice
  *CASE !EMPTY(laData[3]) .AND.  (EMPTY(laData[7]) .OR. laData[7] = lcRA_Inv)
  *B605261,1 (Begin) Retore the old condition as the new one force entring this CASE if there's
  *B605261,1         RA which prevent from entering the case of deffirent invoice from the
  *B605261,1         oen in the RA.    
  *CASE !EMPTY(laData[3])
  CASE !EMPTY(laData[3]) .AND.  (EMPTY(laData[7]) .OR. laData[7] = lcRA_Inv)
  *B605261,1 (End)
  *B802736,1 [End]
    *-- Call local function to get the RA lines.
    =lfGetRALn()
  
  *-- If enter invoice # only without RA # & press the check box :
  *-- [] Return Entire Invoice , Get the invoice lines.
  CASE  EMPTY(laData[3]) .AND. !EMPTY(laData[7]) .AND. laData[15] = "Y"
    *-- Flag to know if insert the selected invoice lines in the RA upon saving.
    llInsrtInv = .F.
    
    *-- Call local function to get the Invoice lines.
    =lfGetInvLn()
    
    *B606774,1 ABD - Check on the warehouse if change check on the style 
    *B606774,1 ABD - if assign to this new warehouse. [Begin]
    = lfUpdWarHs ()
    *B606774,1 ABD - [End]
    
  *-- If enter RA # & enter Invoice # different from the invoice # exist 
  *-- in the RA header record, Give the user the ability to choose the 
  *-- lines he want to display.
  *B119936,1 ABD - get the lines from the RA lines in case we have a RA# and Invoive # , and the 
  *B119936,1 ABD -  RA# not include the invoice # into the Invoice field. [Begin]
  *CASE !EMPTY(laData[3]) .AND. !EMPTY(laData[7]) .AND. !EMPTY(lcRA_Inv) ;
  *     .AND. laData[7] <> lcRA_Inv .AND. laData[15] = "Y"
       
  CASE !EMPTY(laData[3]) .AND. !EMPTY(laData[7]) .AND.  ;
    (EMPTY(lcRA_Inv) .OR. (!EMPTY(lcRA_Inv) .AND. laData[7] <> lcRA_Inv .AND. laData[15] = "Y"))
    *-- Flag to know if insert the selected invoice lines in the RA upon saving.
    llInsrtInv = .F.
    *** Invoice # : {laData[7]} not related to R/A # : {laData[3]}. ***
    *** Do you want to receive the invoice lines or the R/A lines?  ***
    *** < Invoice Lines > - < R/A Lines > ***
    *IF gfModalGen("QRM46025B46001" , "DIALOG" , laData[7]+"|"+laData[3]) = 1
    *  *-- Set flag to true to insert the current invoice lines in the RA upon saving.
    *  llInsrtInv = .T.
    *  *-- Call local function to get the Invoice lines.
    *  =lfGetInvLn()
    *ELSE
    *  *-- Call local function to get the RA lines.
    *  =lfGetRALn()
    *ENDIF
    *-- Call local function to get the RA lines.
    =lfGetRALn()
    *B119936,1 ABD - [End]
    
  OTHERWISE
    RETURN .F.
ENDCASE

*B127806,1 EIH 05/22/2005 If entir invoice get different amounts of invoice from invhdr [Begin].
=lfEntInvHd()
*B127806,1 EIH [End].


*!*************************************************************
*! Name      : lfGetRALn
*! Developer : Reham Al-Allamy
*! Date      : 02/02/1998
*! Purpose   : Get all the RA lines into the detail browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetRALn()
*!*************************************************************
*
FUNCTION lfGetRALn


*B127187,1 EIH 05/22/2005 If we collect lines before then return [Begin].
IF llCollect
  RETURN
ENDIF
llCollect = .T.
*B127187,1 EIH [End].



*-- Get the lines from the Raline file.
SELECT RALINE
IF SEEK(laData[3] , "RALINE")
  lnLineCnt = 1
  STORE 0 To laData[22] , laData[23] , laData[24] , laData[25] , laData[26] , laData[30]
  *B120085,1 ASH 11/05/2003 (Begin) Assign Zero to due tax variable. 
  lndueTax = 0
  *B120085,1 ASH 11/05/2003 (End)
  *C102212,1 (Begin) Zero out HST Tax.
  laData[42] = 0
  *C102212,1 (End)
  *B605261,4 (Begin)  In case of invoice, get tax from the invoice directly..
  *laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)
  IF !EMPTY(laData[7])
    *B119936,1 ABD - Start calculate the GST tax field in case england and taxable. [Begin]
    *laData[26] = IIF(llIsEnglnd , lfchgTax() , IIF(llTax , InvHdr.Tax_Amt , 0))
    IF !llIsCanada .AND. !llTax
      laData[26] = IIF(llIsEnglnd , lfchgTax() , IIF(llTax , InvHdr.Tax_Amt , 0))
    ENDIF
    *B119936,1 ABD - [End]
  ELSE
    laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)  
  ENDIF
  *B605261,4 (End)
  *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
  laData[26] = ROUND(laData[26],2)
  *E302198,1 ALB change tax rate to print 99.999 formate [END]

  *-- Loop in the RA lines to be sure that all styles exist in the style file.
  SCAN WHILE RALINE.RANO = laData[3]
    IF SEEK(RALINE.STYLE , "STYLE")
      SELECT (lcCrMemLin)
      
      *B127806,1 EIH 05/22/2005 Another bug with out issue# . the bug happen when we making new credit
      *B127806,1 EIH            memo with return authorization and return any style with edit in Quality
      *B127806,1 EIH            "return to" part and change in quantity when we transfer from header folder
      *B127806,1 EIH            and  detail folder the program insert new line by style where founded 
      *B127806,1 EIH            in authorization . and that is false. fixed by Nader NNA [begin].    
      
      *B127187,1 EIH 05/22/2005 That is wrong [Begin].
      *IF !SEEK(RALINE.ACCOUNT+RALINE.STYLE+ RALINE.CRA_LINNO)
      *B127187,1 EIH [End].
      
      
      *B127806,1 EIH 05/22/2005  [End]
      
        APPEND BLANK
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        
        *B127806,1 EIH 05/22/2005 Another bug with out issue# . the bug happen when we making new credit
        *B127806,1 EIH            memo with return authorization and return any style with edit in Quality
        *B127806,1 EIH            "return to" part and change in quantity when we transfere from header folder
        *B127806,1 EIH            and  detail folder the programe insert new line by style where founded 
        *B127806,1 EIH            in authorization . and that is false. fixed by Nader NNA [begin].    
        
        *REPLACE ACCOUNT    WITH RALINE.ACCOUNT ;
                STYLE      WITH RALINE.STYLE ;
                CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
                CRET_TRNCD WITH "2" ;
                Dyelot     WITH RaLine.Dyelot ;
                REASON     WITH RALINE.REASON ;
                PRICE      WITH RALINE.PRICE ;
                QTY1       WITH RALINE.nOpnQty1 ;
                QTY2       WITH RALINE.nOpnQty2 ;
                QTY3       WITH RALINE.nOpnQty3 ;
                QTY4       WITH RALINE.nOpnQty4 ;
                QTY5       WITH RALINE.nOpnQty5 ;
                QTY6       WITH RALINE.nOpnQty6 ;
                QTY7       WITH RALINE.nOpnQty7 ;
                QTY8       WITH RALINE.nOpnQty8 ;
                TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
                AMOUNT     WITH RALINE.AMOUNT ;
                INVOICE    WITH laData[7] ;
                CRDATE     WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
                nPstRate   WITH RALINE.nPstRate ;
                TAX_RATE   WITH IIF(llTax , lnTaxRate , 0) ;
                nOpnQty1   WITH RALINE.nOpnQty1 ;
                nOpnQty2   WITH RALINE.nOpnQty2 ;
                nOpnQty3   WITH RALINE.nOpnQty3 ;
                nOpnQty4   WITH RALINE.nOpnQty4 ;
                nOpnQty5   WITH RALINE.nOpnQty5 ;
                nOpnQty6   WITH RALINE.nOpnQty6 ;
                nOpnQty7   WITH RALINE.nOpnQty7 ;
                nOpnQty8   WITH RALINE.nOpnQty8 ;
                nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
      
        *B127187,1 EIH 05/22/2005 That is wrong [Begin].
        *REPLACE ACCOUNT    WITH RALINE.ACCOUNT ;
                STYLE      WITH RALINE.STYLE ;
                CRET_LINNO WITH IIF(!EMPTY(laData[3]),RALINE.CRA_LINNO,ALLTRIM(STR(lnLineCnt))) ;
                CRET_TRNCD WITH "2" ;
                Dyelot     WITH RaLine.Dyelot ;
                REASON     WITH RALINE.REASON ;
                PRICE      WITH RALINE.PRICE ;
                QTY1       WITH RALINE.nOpnQty1 ;
                QTY2       WITH RALINE.nOpnQty2 ;
                QTY3       WITH RALINE.nOpnQty3 ;
                QTY4       WITH RALINE.nOpnQty4 ;
                QTY5       WITH RALINE.nOpnQty5 ;
                QTY6       WITH RALINE.nOpnQty6 ;
                QTY7       WITH RALINE.nOpnQty7 ;
                QTY8       WITH RALINE.nOpnQty8 ;
                TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
                AMOUNT     WITH RALINE.AMOUNT ;
                INVOICE    WITH laData[7] ;
                CRDATE     WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
                nPstRate   WITH RALINE.nPstRate ;
                TAX_RATE   WITH IIF(llTax , lnTaxRate , 0) ;
                nOpnQty1   WITH RALINE.nOpnQty1 ;
                nOpnQty2   WITH RALINE.nOpnQty2 ;
                nOpnQty3   WITH RALINE.nOpnQty3 ;
                nOpnQty4   WITH RALINE.nOpnQty4 ;
                nOpnQty5   WITH RALINE.nOpnQty5 ;
                nOpnQty6   WITH RALINE.nOpnQty6 ;
                nOpnQty7   WITH RALINE.nOpnQty7 ;
                nOpnQty8   WITH RALINE.nOpnQty8 ;
                nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8

        REPLACE ACCOUNT    WITH RALINE.ACCOUNT ;
                STYLE      WITH RALINE.STYLE ;
                CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
                CRET_TRNCD WITH "2" ;
                Dyelot     WITH RaLine.Dyelot ;
                REASON     WITH RALINE.REASON ;
                PRICE      WITH RALINE.PRICE ;
                QTY1       WITH RALINE.nOpnQty1 ;
                QTY2       WITH RALINE.nOpnQty2 ;
                QTY3       WITH RALINE.nOpnQty3 ;
                QTY4       WITH RALINE.nOpnQty4 ;
                QTY5       WITH RALINE.nOpnQty5 ;
                QTY6       WITH RALINE.nOpnQty6 ;
                QTY7       WITH RALINE.nOpnQty7 ;
                QTY8       WITH RALINE.nOpnQty8 ;
                TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
                AMOUNT     WITH RALINE.AMOUNT ;
                INVOICE    WITH laData[7] ;
                CRDATE     WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
                nPstRate   WITH RALINE.nPstRate ;
                TAX_RATE   WITH IIF(llTax , lnTaxRate , 0) ;
                nOpnQty1   WITH RALINE.nOpnQty1 ;
                nOpnQty2   WITH RALINE.nOpnQty2 ;
                nOpnQty3   WITH RALINE.nOpnQty3 ;
                nOpnQty4   WITH RALINE.nOpnQty4 ;
                nOpnQty5   WITH RALINE.nOpnQty5 ;
                nOpnQty6   WITH RALINE.nOpnQty6 ;
                nOpnQty7   WITH RALINE.nOpnQty7 ;
                nOpnQty8   WITH RALINE.nOpnQty8 ;
                nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
                
        
        *B127187,1 EIH  [End].
        
        *B127806,1 EIH 05/22/2005  [End]
	   
	   *B127187,1 EIH 05/22/2005 That is wrong [Begin].
       *ENDIF
       *B127187,1 EIH [End].
       
      *C123847,1  TMI [Start] Add a trigger for DIR03 to update the EMPLOYEE field from RALINE in the temp file
      IF ASCAN(laEvntTrig , PADR('UPEMPFLD',10)) <> 0
        =gfDoTriger('RMCRMEM',PADR('UPEMPFLD',10))
      ENDIF
      *C123847,1  TMI [End  ] 
      *-- Save correct line tax if England.
      IF llIsEnglnd
        REPLACE TAX_RATE WITH lfMechDisc(&lcCrMemLin..Style)
      ENDIF
      *-- Get the numeric values.
      REPLACE GROS_PRICE WITH (RALINE.PRICE/ (1-lnDiscPcnt/100)) ;
              DISC_PCNT  WITH lnDiscPcnt ;
              DISC_AMT   WITH (GROS_PRICE * TOTQTY * lnDiscPcnt/100) ;
              TRDE_AMT   WITH AMOUNT * lnInvTrdDs/100 ;
              cStyGrade  WITH IIF(SEEK(&lcCrMemLin..Style , "STYLE") , Style.cStyGrade , "")
      
      *B802223,1 Reham On 06/13/1999   *** Begin ***
      *B802223,1 Update amount field with total open amount.
      REPLACE AMOUNT  WITH (TOTQTY * PRICE) * (1-(lnDiscPcnt/100))
      *B802223,1 Reham On 06/13/1999   *** End   ***
      
      *-- Get the Style cost.
      IF !EMPTY(&lcCrMemLin..Invoice) AND SEEK(&lcCrMemLin..Invoice , "INVLINE")
        lcOldTag = ORDER('INVLINE')
        SET ORDER TO INVLINES IN INVLINE
        *B603820,1 (Start) Get the total cost if the cost method was standered.
        *lnCost = IIF(SEEK(&lcCrMemLin..Style + &lcCrMemLin..Invoice , "INVLINE") , ;
                     INVLINE.Cost , ;
                 IIF(SEEK(&lcCrMemLin..Style + laData[29] + SPACE(10) , "STYDYE") , ;
                     STYDYE.Ave_Cost , ;
                 IIF(SEEK(&lcCrMemLin..Style , "STYLE") , STYLE.Ave_Cost , 0)))
                 
        lnCost = IIF(SEEK(&lcCrMemLin..Style + &lcCrMemLin..Invoice , "INVLINE") , ;
                     INVLINE.Cost , ;
                 IIF(SEEK(&lcCrMemLin..Style + laData[29] + SPACE(10) , "STYDYE") , ;
                    IIF(llStdCost,IIF(SEEK(&lcCrMemLin..Style , "STYLE"),Style.TotCost,0), STYDYE.Ave_Cost) , ;
                 IIF(SEEK(&lcCrMemLin..Style , "STYLE") , IIF(llStdCost,STYLE.TotCost,STYLE.Ave_Cost) , 0)))
        
        *B603820,1 (End) 
        lcInvSlLnk = InvLine.GL_Sales
        
        *B602688,1 Reham On 03/21/99     *** Begin ***
        lcCOGSAcnt  = Invline.ccogsacnt
        lcICAcnt    = Invline.cicacnt
        *B603054,1 Reham On 07/07/1999   *** Begin ***
        *B603054,1 This account should hold the return merchandise 
        *B603054,1 account instead of the sales revenue.
        *lcSalesAcnt = Invline.csalesacnt
        lcSalesAcnt = ""
        *B603054,1 Reham On 07/07/1999   *** End   ***
        lcDiscAcnt  = Invline.cdiscacnt
        *B602688,1 Reham On 03/21/99     *** End   ***
        *B602932,1 Reham On 05/26/1999   *** Begin ***
        *B602932,1 Add true in the logical field to know that this line was 
        *B602932,1 shipped to the customer befor.
        REPLACE lShipped  WITH .T.
        *B602932,1 Reham On 05/26/1999   *** End   ***
        SET ORDER TO (lcOldTag) IN INVLINE
      ELSE
        *B603820,1 (Start) Get the total cost if the cost method was standered.
        *lnCost = IIF(SEEK(&lcCrMemLin..Style + laData[29] + SPACE(10) , "STYDYE") , ;
                     STYDYE.Ave_Cost , ;
                 IIF(SEEK(&lcCrMemLin..Style , "STYLE") , STYLE.Ave_Cost,0))
        
        lnCost = IIF(SEEK(&lcCrMemLin..Style + laData[29] + SPACE(10) , "STYDYE") , ;
                     IIF(llStdCost,IIF(SEEK(&lcCrMemLin..Style , "STYLE"),Style.TotCost,0),STYDYE.Ave_Cost) , ;
                 IIF(SEEK(&lcCrMemLin..Style , "STYLE") ,IIF(llStdCost,Style.TotCost,Style.Ave_Cost),0))
        *B603820,1 (End) 
        lcInvSlLnk = ""
        
        *B602688,1 Reham On 03/21/99    *** Begin ***
        STORE "" TO lcCOGSAcnt , lcICAcnt , lcSalesAcnt , lcDiscAcnt
        *B602688,1 Reham On 03/21/99    *** End   ***
      ENDIF
      REPLACE COST WITH lnCost
      
      *B602688,1 Reham On 03/21/99    *** Begin ***
      REPLACE cCOGSAcnt  WITH lcCOGSAcnt  ;
              cICAcnt    WITH lcICAcnt    ;
              cSalesAcnt WITH lcSalesAcnt ;
              cDiscAcnt  WITH lcDiscAcnt
      *B602688,1 Reham On 03/21/99    *** End   ***
      
      *-- If invoice number was entered, Read the cost and cost link code
      *-- invoice file. Else read average cost and cost link code from
      *-- style file.
      IF llLink_GL
        lcGlCost   = "DEFDEF"
        IF !EMPTY(&lcCrMemLin..Invoice) .AND. SEEK(&lcCrMemLin..Invoice , "InvLine")
          lcGlCost   = IIF(!EMPTY(InvLine.Gl_Cost) , InvLine.Gl_Cost , "DEFDEF")
        ELSE
          *B602688,1 Reham On 03/21/99   *** Begin ***
          *B602688,1 If single warehouse get the style link code, If multi warehouse
          *B602688,1 get the style dyelot link code.
          IF !llMultiWH
          *B602688,1 Reham On 03/21/99   *** End   ***
            IF SEEK(&lcCrMemLin..Style , "Style")
              lcGlCost =IIF(!EMPTY(Style.Link_Code) , Style.Link_Code , "DEFDEF")
            ENDIF
          ELSE
            *- If multi warehouse and the invoice number left empty, Get
            *- average cost and G.L. cost link code for that warehouse.
            IF SEEK(&lcCrMemLin..Style + laData[29] + SPACE(10) , "StyDye")
              lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
            ENDIF
          ENDIF
        ENDIF

        *-- Get the customer link & the style link.
        =SEEK("M" + laData[2] , "CUSTOMER")
        
        *B602688,1 Reham On 03/21/99   *** End   ***
        *lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
        IF EMPTY(lcInvSlLnk)
          *B602688,1 If Gl link set at division level.
          IF llDiv_Link
            DECLARE laDRltFld[2,2]
            laDRltFld[1,1] = 'LINK_CODE'
            laDRltFld[1,2] = 'lcCustLink'
            laDRltFld[2,1] = 'CSLSGLLINK'
            laDRltFld[2,2] = 'lcCstSlLnk'
            
            =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
            lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
          ELSE   
            lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
          ENDIF
        ENDIF
        *B602688,1 Reham On 03/21/99   *** End   ***
        
        =SEEK(&lcCrMemLin..Style , "STYLE")
        lcStySlLnk = Style.cslsgllink
        REPLACE GL_COST   WITH lcGlCost ;
                GL_SALES  WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
        UNLOCK
      ENDIF
      
      *-- Calculate the pieces, amount, discount, tax, pst amount.
      laData[22] = laData[22] + &lcCrMemLin..TotQty
      laData[23] = laData[23] + &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
      laData[24] = laData[24] + &lcCrMemLin..DISC_AMT
      laData[25] = laData[25] + &lcCrMemLin..Amount
      *B605261,4 (Begin)  Calculate tax on taxable amount only.
      lnDueTax = lnDueTax + IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0)
      *B605261,4 (End)
      
      *laData[26] = laData[26] + ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A" AND !llIsEnglnd , laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/100))
      *B605261,4 (Begin)  Consider if the style is taxable of not.
      *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
      *B120085,1 ASH 11/05/2003 (Begin) Compute the tax from RALINE file.
      *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(RALINE.Style , RALINE.Price , "RALINE") , IIF(llTax , IIF(EMPTY(laData[7]),lnOpAmot * lnTaxRate / 100 , 0),0))
      IF !llIsEnglnd AND llTax AND !EMPTY(laData[7]) 
        laData[26] = laData[26] + (RALINE.Amount * RALINE.Tax_Rate/100)
      *B120085,1 ASH 11/05/2003 (End)
      ELSE
        laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax AND EMPTY(laData[7]) AND Style.lTaxable, (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
      ENDIF
      *B605261,4 (End)

      *B608078,1 NNA 05/11/2007 (Begin) Recalculate the Tax Amount field when user navigate between Pages
      IF llIsEnglnd AND llTax AND !EMPTY(laData[7]) AND cbEntr_Inv
         laData[26] = laData[26] + ROUND(INVHDR.Tax_Rate*(1-INVHDR.Trde_Disc/100)*INVHDR.ncharges/100,2)
      ENDIF
      *B608078,1 NNA (End)

      laData[30] = IIF(llIsCanada , laData[30] + &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
      *C102212,1 (Begin) Calculate HST Tax Rate.
      laData[42] = IIF(llIsCanada , laData[42] + &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
      *C102212,1 (End)
      lnTrdeAmt  = lnTrdeAmt + &lcCrMemLin..TRDE_AMT
      lnLineCnt  = lnLineCnt + 1
    ENDIF
    SELECT RALINE
  ENDSCAN
  lnCrMemNo = lnLineCnt - 1

  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Variable to hold the counter line no.
  lnCrLinCnt = lnLineCnt - 1
  *B802907,1 Reham On 12/26/1999   *** End   ***
  
  *Adjust tax value.
  *-- Get the tax rate defaulted from the R/A lines.
  *lnTaxRate = RALINE.TAX_RATE
  
  *-- Calculate the cahrge back.
  IF lnInvTrdDs <> 0
    laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
  ELSE
    laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
  ENDIF
  
  *B802907,1 Reham On 12/27/1999    *** Begin ***
  lnAuthPcs = Max(lnRAQty - laData[22] , 0)
  lnAuthAmt = Max(lnRAAmt - laData[23] , 0)
  SHOW GET lnAuthPcs
  SHOW GET lnAuthAmt
  *B802907,1 Reham On 12/27/1999    *** End   ***
  
  *-- Disable or enable the objects related to the R/A # & existance of lines.
  *-- Refresh store objects.
  lcStorStat= IIF(laScrMode[4] .AND. EMPTY(laData[3]) , "ENABLE" , "DISABLE")
  SHOW GET ibStore    &lcStorStat
  SHOW GET laData[4]  &lcStorStat
  *-- Refresh R/A objects.
  lcRAStat  = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  SHOW GET ibRaNo     &lcRAStat
  SHOW GET laData[3]  &lcRAStat
  *-- Refresh invoice objects.
  lcInvStat = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  *-- Refresh order objects.
  lcOrdStat = IIF(EMPTY(laData[8]) .AND. EMPTY(laData[3]) .AND. laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  *-- Refresh check box "return entire invoice" object.
  lcEntrStat= IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  *-- Refresh currency code objects.
  lcCurStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') , "ENABLE" , "DISABLE")
  *-- Refresh exchange rate object.
  lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")
  *C102212,1 (Begin) Add HST Tax.
  *laData[28] = laData[27] + laData[25] + laData[26] + laData[30]

  *B119936,1 ABD - Start calculate the GST tax field in case england and taxable. [Begin]
  IF !EMPTY(laData[3]) .AND. llIsCanada .AND. llTax  
    laData[26] = ((laData[25]+IIF(lcTax_Meth="A" , laData[27] , 0)) * (lnTaxRate/100))
    laData[26] = ROUND(laData[26],2)
  ENDIF
  *B119936,1 ABD - [End]

  *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
  IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
    =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
  ENDIF
  *C200517,1 KHM 04/13/2003 (End)

  laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
  *C102212,1 (End)
  SHOW GET laData[28]
  *-- Set flag to execute the browse.
  llReBrowse = .T.
  
  *B802222,1 Reham On 05/05/99   *** Begin ***
  SELECT (lcCrMemLin)
  GO TOP
  *B802222,1 Reham On 05/05/99   *** End   ***
ENDIF
*! B604896,1 MHM 10/08/2001 Empty Variable to collect right data [Start]
lcinvsllnk = ""
*! B604896,1 MHM [End]

*!*************************************************************
*! Name      : lfGetInvLn
*! Developer : Reham Al-Allamy
*! Date      : 02/02/1998
*! Purpose   : Get all the invoice lines into the detail browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInvLn()
*!*************************************************************
*
FUNCTION lfGetInvLn

*B127187,1 EIH 05/22/2005 If we collect lines before then return [Begin].
IF llCollect
  RETURN
ENDIF
llCollect = .T.
*B127187,1 EIH  [End].

llFondLins = .T.
SELECT INVHDR
=SEEK(laData[7])
*-- If the invoice is consolidated, get the lines from CONSINVL.
IF INVHDR.Consol = 'Y'
  SELECT CONSINVH
  IF SEEK(laData[7])
    *B803169,1 Reham On 04/04/2000    *** Begin ***
    *B803169,1 Fix the error of "Variable lnLineCnt not found" if create from consolidated invoice.
    lnLineCnt = 1
    *B803169,1 Get the right index for the invoice lines file.
    SELECT INVLINE
    SET ORDER TO INVLINES
    SELECT CONSINVH
    *B803169,1 Reham On 04/04/2000    *** End   ***
    
    STORE 0 TO laData[22] , laData[23] , laData[24] , laData[25] , laData[26] , laData[30]
    *C102212,1 (Begin) Zero out HST Tax.
    STORE 0 TO laData[42]
    *C102212,1 (End)
    laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)
    *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
    laData[26] = ROUND(laData[26],2)
    *E302198,1 ALB change tax rate to print 99.999 formate [END]

    SCAN WHILE Invoice = laData[7] FOR cWareCode = laData[29]
      *B605261,1 (Begin) When NOT ENGLAND and making CR from an invoice, get the invoice tax directly.
      IF !llIsEnglnd
        laData[26] = laData[26] + IIF(llTax , CONSINVH.Tax_Amt , 0)
      ENDIF   
      *B605261,1 (End)
     *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN] 
     laData[26] = ROUND(laData[26],2)
     *E302198,1 ALB change tax rate to print 99.999 formate [END]

      SELECT CONSINVL
      *B606272,1 (Begin) Get the exact order inside the consolidated invoice.
      *IF SEEK(laData[7]+CONSINVH.Store)
      IF SEEK(laData[7] + CONSINVH.Store+CONSINVH.Order)
      *B606272,1 (End)
        *-- Loop in the invoice lines to be sure of its existance in the style file.
        SCAN WHILE Invoice+Store+Order = laData[7]+CONSINVH.Store+CONSINVH.Order ;
                   .AND. SEEK(CONSINVL.Style , "Style")
          *B607818,1 ASH 11/02/2006 (Begin) Get only the lines for the CONSINVH piktkt's.
          IF PIKTKT <> CONSINVH.PIKTKT
            LOOP
          ENDIF
          *B607818,1 ASH 11/02/2006 (End)
          SELECT (lcCrMemLin)
          APPEND BLANK
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE ACCOUNT   WITH CONSINVL.ACCOUNT ;
                  STYLE     WITH CONSINVL.STYLE ;
                  CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
                  CRET_TRNCD WITH "2" ;
                  INVOICE   WITH laData[7] ;
                  CRDATE    WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
                  Dyelot    WITH CONSINVL.Dyelot ;
                  QTY1      WITH CONSINVL.QTY1 ;
                  QTY2      WITH CONSINVL.QTY2 ;
                  QTY3      WITH CONSINVL.QTY3 ;
                  QTY4      WITH CONSINVL.QTY4 ;
                  QTY5      WITH CONSINVL.QTY5 ;
                  QTY6      WITH CONSINVL.QTY6 ;
                  QTY7      WITH CONSINVL.QTY7 ;
                  QTY8      WITH CONSINVL.QTY8 ;
                  TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
                  PRICE     WITH CONSINVL.PRICE * ( 1 - CONSINVH.DISCPCNT/100) ;
                  AMOUNT    WITH TOTQTY * (CONSINVL.PRICE * ( 1 - CONSINVH.DISCPCNT/100)) ;
                  nPstRate  WITH IIF(llTax .AND. llIsCanada , lnPstRate , 0) ;
                  TAX_RATE  WITH IIF(llTax , lnTaxRate , 0) ;
                  lInvoice  WITH llInsrtInv
                  *B605539,1 (Begin) Update the season with the header one.
                  REPLACE REASON     WITH laData[13]  
                  *B605539,1 (End)
          *C102212,1 (Begin) Intialize HST Tax Rate.
          REPLACE nHstRate  WITH IIF(llTax .AND. llIsCanada , lnHstRate , 0)
          *C102212,1 (End)
          *-- Get the tax if england.
          IF llIsEnglnd
            REPLACE TAX_RATE WITH lfMechDisc(&lcCrMemLin..Style)
          ENDIF
          *-- Get the numeric value.
          *B803169,1 Reham On 04/04/2000    *** Begin ***
          *B803169,1 Get the line cost from the invoice lines.
          *REPLACE GROS_PRICE WITH CONSINVL.PRICE ;
                  DISC_PCNT  WITH lnDiscPcnt ;
                  DISC_AMT   WITH (GROS_PRICE * TOTQTY * lnDiscPcnt / 100) ;
                  TRDE_AMT   WITH AMOUNT * lnInvTrdDs / 100 ;
                  COST       WITH CONSINVL.COST ;
                  cStyGrade  WITH IIF(SEEK(&lcCrMemLin..Style , "STYLE") , Style.cStyGrade , "")
          
          REPLACE GROS_PRICE WITH CONSINVL.PRICE ;
                  DISC_PCNT  WITH lnDiscPcnt ;
                  DISC_AMT   WITH (GROS_PRICE * TOTQTY * lnDiscPcnt / 100) ;
                  TRDE_AMT   WITH AMOUNT * lnInvTrdDs / 100 ;
                  cStyGrade  WITH IIF(SEEK(&lcCrMemLin..Style , "STYLE") , Style.cStyGrade , "")
          SELECT INVLINE
          
          *B127806,1 EIH 05/22/2005 Fix bug that Not updated ncost in styinvjl 
          *B127806,1 EIH            File .set invline to correct index .not related to issue[Begin].
          SET ORDER TO INVLINES
          *B127806,1 EIH 05/22/2005  [End]
          
          IF SEEK(CONSINVL.style+CONSINVL.invoice+STR(CONSINVL.lineno,6))
            SELECT (lcCrMemLin)
            REPLACE COST WITH INVLINE.COST
          ENDIF
          SELECT (lcCrMemLin)
          *B803169,1 Reham On 04/04/2000    *** End   ***
          
          *B602932,1 Reham On 05/26/1999   *** Begin ***
          *B602932,1 Add true in the logical field to know that this line was 
          *B602932,1 shipped to the customer befor.
          REPLACE lShipped  WITH .T.
          *B602932,1 Reham On 05/26/1999   *** End   ***
          
          *-- If the Gl linked to the current company, get the cost.
          IF llLink_GL
            SELECT INVLINE
            SET ORDER TO INVLINES
            
            *B803169,1 Reham On 04/04/2000    *** Begin ***
            *B803169,1 Fix the error of missing expression.
            *=SEEK(SEEK(&lcCrMemLin..Style + laData[7] )
            =SEEK(&lcCrMemLin..Style + laData[7])
            *B803169,1 Reham On 04/04/2000    *** End   ***
            
            SET ORDER TO INVLINE

            *B602688,1 Reham On 03/21/99   *** Begin ***
            lcGlCost = "DEFDEF"
            IF !EMPTY(InvLine.Gl_Cost)
              lcGlCost   = InvLine.Gl_Cost
            ELSE
              IF !llMultiWH
                IF SEEK(&lcCrMemLin..Style , "Style")
                  lcGlCost =IIF(!EMPTY(Style.Link_Code) , Style.Link_Code , "DEFDEF")
                ENDIF
              ELSE
                *- If multi warehouse and the invoice number left empty, Get
                *- average cost and G.L. cost link code for that warehouse.
                IF SEEK(&lcCrMemLin..Style + laData[29] + SPACE(10) , "StyDye")
                  lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
                ENDIF
              ENDIF
            ENDIF
            
            IF !EMPTY(INVLINE.GL_Sales)
              lcInvSlLnk = INVLINE.GL_Sales
            ELSE
              lcInvSlLnk = ""
              *-- Get the customer link & the style link.
              =SEEK("M" + laData[2] , "CUSTOMER")
              
              IF llDiv_Link
                DECLARE laDRltFld[2,2]
                laDRltFld[1,1] = 'LINK_CODE'
                laDRltFld[1,2] = 'lcCustLink'
                laDRltFld[2,1] = 'CSLSGLLINK'
                laDRltFld[2,2] = 'lcCstSlLnk'
                
                =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
                lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
              ELSE
                lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
              ENDIF
            ENDIF
            *B602688,1 Reham On 03/21/99   *** End   ***
            
            SELECT (lcCrMemLin)
            lcStySlLnk = IIF(SEEK(&lcCrMemLin..Style , "STYLE") , Style.cslsgllink , "")
            REPLACE GL_COST   WITH lcGlCost ;
                    GL_SALES  WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
            
            *B603054,1 Reham On 07/07/1999   *** Begin ***
            *B603054,1 This account should hold the return merchandise 
            *B603054,1 account instead of the sales revenue.
            *B602688,1 Reham On 03/21/99    *** Begin ***
            *B602688,1 Get the accounts from the invoice lines.
            *REPLACE cCOGSAcnt  WITH InvLine.cCOGSAcnt  ;
                    cICAcnt    WITH InvLine.cICAcnt    ;
                    cSalesAcnt WITH InvLine.cSalesAcnt ;
                    cDiscAcnt  WITH InvLine.cDiscAcnt
            REPLACE cCOGSAcnt  WITH InvLine.cCOGSAcnt  ;
                    cICAcnt    WITH InvLine.cICAcnt    ;
                    cDiscAcnt  WITH InvLine.cDiscAcnt
            *B602688,1 Reham On 03/21/99    *** End   ***
            *B603054,1 Reham On 07/07/1999  *** End   ***
          ENDIF
          UNLOCK
          
          *-- Calculate the pieces, amount, discount, tax, pst amount.
          laData[22] = laData[22] + &lcCrMemLin..TotQty
          laData[23] = laData[23] + &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
          laData[24] = laData[24] + &lcCrMemLin..DISC_AMT
          laData[25] = laData[25] + &lcCrMemLin..Amount
          *B605261,4 (Begin)  Calculate tax on taxable amount only.
          lnDueTax = lnDueTax + IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0)
          *B605261,4 (End)
          
          *laData[26] = laData[26] + ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A" AND !llIsEnglnd , laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/100))
          *B605261,1 (Begin) Remark it here FOR USA .
          *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
          laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , 0)
          *B605261,1 (End)
          laData[30] = IIF(llIsCanada , laData[30] + &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
          *C102212,1 (Begin) Calculate HST Tax Rate.
          laData[42] = IIF(llIsCanada , laData[42] + &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
          *C102212,1 (End)
          lnTrdeAmt  = lnTrdeAmt + &lcCrMemLin..TRDE_AMT
          lnLineCnt  = lnLineCnt + 1
          SELECT CONSINVL
        ENDSCAN
        *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
        laData[26] = ROUND(laData[26],2)
        *E302198,1 ALB change tax rate to print 99.999 formate [END]

      ENDIF
      SELECT CONSINVH
    ENDSCAN
    llFondLins = .T.  && Found all the invoice lines.
    
    *B803169,1 Reham On 04/04/2000    *** Begin ***
    *B803169,1 Get the right index for the invoice lines file.
    SELECT INVLINE
    SET ORDER TO INVLINE
    *B803169,1 Reham On 04/04/2000    *** End   ***
  ELSE
    llFondLins = .F.  && No lines have been found for the selected invoice.
  ENDIF
ELSE
  *-- Get the lines from the invline file in case the invoice is not consolidated.
  SELECT INVLINE
  IF SEEK(laData[7] , "INVLINE")
    STORE 0 TO laData[22] , laData[23] , laData[24] , laData[25] , laData[26] , laData[30]
    *C102212,1 (Begin) Zero out HST Tax.
    STORE 0 TO laData[42]
    *C102212,1 (End)
    *B605261,1 (Begin) When making CR from an invoice, get the invoice tax directly.
    *laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)
    laData[26] = IIF(llIsEnglnd , lfchgTax() , IIF(llTax , InvHdr.Tax_Amt , 0))
  
    *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
    laData[26] = ROUND(laData[26],2)
    *E302198,1 ALB change tax rate to print 99.999 formate [END]

    lnDueTAX = 0
    *B605261,1 (End)
    lnLineCnt = 1
    *MAN
    SELECT INVLINE
    *-- Loop in the invoice lines to be sure of its existance in the style file.
    SCAN WHILE INVLINE.Invoice = laData[7] ;
           FOR SEEK(INVLINE.Style , "STYLE")
      SELECT (lcCrMemLin)
      APPEND BLANK
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      *-- Changed the calculating of AMOUNT for 
      *-- each ret. line to be on Price before deducting
      *-- the discount and calcuating the discount on 
      *-- the total merch. of the line.      

      *B131872,1 NNA 04/20/2006 (Begin) change the amount field to get by another way
      *REPLACE ACCOUNT    WITH INVLINE.ACCOUNT ;
              STYLE      WITH INVLINE.STYLE ;
              CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
              CRET_TRNCD WITH "2" ;
              INVOICE    WITH INVLINE.INVOICE ;
              CRDATE     WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
              QTY1       WITH INVLINE.QTY1 ;
              QTY2       WITH INVLINE.QTY2 ;
              QTY3       WITH INVLINE.QTY3 ;
              QTY4       WITH INVLINE.QTY4 ;
              QTY5       WITH INVLINE.QTY5 ;
              QTY6       WITH INVLINE.QTY6 ;
              QTY7       WITH INVLINE.QTY7 ;
              QTY8       WITH INVLINE.QTY8 ;
              TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
              PRICE      WITH INVLINE.PRICE*(1-lnDiscPcnt/100) ;
              AMOUNT     WITH TOTQTY * (INVLINE.PRICE*(1-lnDiscPcnt/100)) ;
              DYELOT     WITH INVLINE.DYELOT ;
              nPstRate   WITH IIF(llTax .AND. llIsCanada , lnPstRate , 0) ;
              TAX_RATE   WITH IIF(llTax , lnTaxRate , 0) ;
              lInvoice   WITH llInsrtInv
              *B605539,1 (Begin) Update the season with the header one.
              REPLACE REASON     WITH laData[13]  
              *B605539,1 (End)
       REPLACE ACCOUNT    WITH INVLINE.ACCOUNT ;
              STYLE      WITH INVLINE.STYLE ;
              CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
              CRET_TRNCD WITH "2" ;
              INVOICE    WITH INVLINE.INVOICE ;
              CRDATE     WITH IIF(!EMPTY(laData[5]) , laData[5] , gdSysDate) ;
              QTY1       WITH INVLINE.QTY1 ;
              QTY2       WITH INVLINE.QTY2 ;
              QTY3       WITH INVLINE.QTY3 ;
              QTY4       WITH INVLINE.QTY4 ;
              QTY5       WITH INVLINE.QTY5 ;
              QTY6       WITH INVLINE.QTY6 ;
              QTY7       WITH INVLINE.QTY7 ;
              QTY8       WITH INVLINE.QTY8 ;
              TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
              PRICE      WITH INVLINE.PRICE*(1-lnDiscPcnt/100) ;
              AMOUNT     WITH (TOTQTY * INVLINE.PRICE) - ROUND(TOTQTY * INVLINE.PRICE * lnDiscPcnt/100,2) ;
              DYELOT     WITH INVLINE.DYELOT ;
              nPstRate   WITH IIF(llTax .AND. llIsCanada , lnPstRate , 0) ;
              TAX_RATE   WITH IIF(llTax , lnTaxRate , 0) ;
              lInvoice   WITH llInsrtInv                 ;
              REASON     WITH laData[13]  
       *B131872,1 NNA (End)
  
      *C102212,1 (Begin) Intialize HST Tax Rate.
      REPLACE nHstRate  WITH IIF(llTax .AND. llIsCanada , lnHstRate , 0)
      *C102212,1 (End)
      *-- Save correct line tax if england.
      IF llIsEnglnd
        REPLACE TAX_RATE WITH lfMechDisc(&lcCrMemLin..Style)
      ENDIF
      
      *-- Get the numeric data.
      REPLACE GROS_PRICE WITH INVLINE.PRICE ;
              DISC_PCNT  WITH lnDiscPcnt ;
              DISC_AMT   WITH (&lcCrMemLin..GROS_PRICE * &lcCrMemLin..TOTQTY * lnDiscPcnt/100) ;
              TRDE_AMT   WITH &lcCrMemLin..AMOUNT * lnInvTrdDs/100 ;
              COST       WITH INVLINE.COST ;
              cStyGrade  WITH IIF(SEEK(&lcCrMemLin..Style , "STYLE") , Style.cStyGrade , "")
      
      *B602932,1 Reham On 05/26/1999   *** Begin ***
      *B602932,1 Add true in the logical field to know that this line was 
      *B602932,1 shipped to the customer befor.
      REPLACE lShipped  WITH .T.
      *B602932,1 Reham On 05/26/1999   *** End   ***
      
      *-- If there is Gl linked to the current company, get the cost.
      IF llLink_GL
        *B602688,1 Reham On 03/21/99   *** Begin ***
        lcGlCost = "DEFDEF"
        IF !EMPTY(InvLine.Gl_Cost)
          lcGlCost   = InvLine.Gl_Cost
        ELSE
          IF !llMultiWH
            IF SEEK(&lcCrMemLin..Style , "Style")
              lcGlCost =IIF(!EMPTY(Style.Link_Code) , Style.Link_Code , "DEFDEF")
            ENDIF
          ELSE
            *- If multi warehouse and the invoice number left empty, Get
            *- average cost and G.L. cost link code for that warehouse.
            IF SEEK(&lcCrMemLin..Style + laData[29] + SPACE(10) , "StyDye")
              lcGlCost = IIF(!EMPTY(StyDye.Gl_Link) , StyDye.Gl_Link , lcGlCost)
            ENDIF
          ENDIF
        ENDIF
        
        lcCstSlLnk = "DEF"
        IF !EMPTY(INVLINE.GL_Sales)
          lcInvSlLnk = INVLINE.GL_Sales
        ELSE
          lcInvSlLnk = ""
          *-- Get the customer link & the style link.
          =SEEK("M" + laData[2] , "CUSTOMER")
          
          IF llDiv_Link
            DECLARE laDRltFld[2,2]
            laDRltFld[1,1] = 'LINK_CODE'
            laDRltFld[1,2] = 'lcCustLink'
            laDRltFld[2,1] = 'CSLSGLLINK'
            laDRltFld[2,2] = 'lcCstSlLnk'
            
            =gfRltFld(laData[14] , @laDRltFld,'CDIVISION')
            lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
          ELSE
            lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
          ENDIF
        ENDIF
        *B602688,1 Reham On 03/21/99   *** End   ***
        
        lcStySlLnk = IIF(SEEK(&lcCrMemLin..Style , "STYLE") , Style.cslsgllink , "")
        REPLACE GL_COST  WITH lcGlCost ;
                GL_SALES WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
        *B603054,1 Reham On 07/07/1999   *** Begin ***
        *B603054,1 This account should hold the return merchandise 
        *B603054,1 account instead of the sales revenue.
        *B602688,1 Reham On 03/21/99    *** Begin ***
        *B602688,1 Get the accounts from the invoice lines.
        *REPLACE cCOGSAcnt  WITH InvLine.cCOGSAcnt  ;
                cICAcnt    WITH InvLine.cICAcnt    ;
                cSalesAcnt WITH InvLine.cSalesAcnt ;
                cDiscAcnt  WITH InvLine.cDiscAcnt
        REPLACE cCOGSAcnt  WITH InvLine.cCOGSAcnt  ;
                cICAcnt    WITH InvLine.cICAcnt    ;
                cDiscAcnt  WITH InvLine.cDiscAcnt
        *B602688,1 Reham On 03/21/99    *** End   ***
        *B603054,1 Reham On 07/07/1999  *** End   ***
      ENDIF
      UNLOCK
      
      *-- Calculate the pieces, amount, discount, tax, pst amount.
      laData[22] = laData[22] + &lcCrMemLin..TotQty
      laData[23] = laData[23] + &lcCrMemLin..TotQty * &lcCrMemLin..GROS_PRICE
      laData[24] = laData[24] + &lcCrMemLin..DISC_AMT
      laData[25] = laData[25] + &lcCrMemLin..Amount
      *B605261,4 (Begin)  Calculate tax on taxable amount only.
      lnDueTax = lnDueTax + IIF(SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable,&lcCrMemLin..Amount,0)
      *B605261,4 (End)
      
      *laData[26] = laData[26] + ((&lcCrMemLin..Amount + IIF(lcTax_Meth="A" AND !llIsEnglnd , laData[27] , 0)) * (&lcCrMemLin..TAX_RATE/100))
      *B605261,1 (Begin) Remark it here for USA.
      *laData[26] = laData[26] + IIF(llIsEnglnd , ;
        lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , ;
        IIF(llTax , (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
      laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) ,0)
      *B605261,1 (End)
      laData[30] = IIF(llIsCanada , laData[30] + &lcCrMemLin..Amount * (&lcCrMemLin..nPstRate/100) , 0)
      *C102212,1 (Begin) Calculate HST Tax Rate.
      laData[42] = IIF(llIsCanada , laData[42] + &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
      *C102212,1 (End)
      lnTrdeAmt  = lnTrdeAmt  + &lcCrMemLin..TRDE_AMT
      
      lnLineCnt = lnLineCnt + 1
      SELECT INVLINE
    ENDSCAN
    *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
    laData[26] = ROUND(laData[26],2)
    *E302198,1 ALB change tax rate to print 99.999 formate [END]

    llFondLins = .T.  && Found all the invoice lines.
  ELSE
    llFondLins = .F.  && No lines have been found for the selected invoice.
  ENDIF
ENDIF
*-- If there is no lines for the current invoice, give the user a warning message.
IF !llFondLins
  *** The invoice lines are missing. ***
  *** <  Ok  > ***
  =gfModalGen("INM46002B00000" , "DIALOG")
ELSE
  *-- Count the inserted lines in the current credit memo.
  lnCrMemNo = lnLineCnt - 1
  
  *B802907,1 Reham On 12/26/1999   *** Begin ***
  *B802907,1 Variable to hold the counter line no.
  lnCrLinCnt = lnLineCnt - 1
  *B802907,1 Reham On 12/26/1999   *** End   ***
  
  *-- Calculate the sales reps. charge back.
  IF lnInvTrdDs <> 0
    laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
  ELSE
    laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
    laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
  ENDIF

  *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
  IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
    =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
  ENDIF
  *C200517,1 KHM 04/13/2003 (End)
  
  *-- calculate the total credit amount.
  *C102212,1 (Begin) Add HST Tax.
  *laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
  laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
  *C102212,1 (End)
  
  
  *B127806,1 EIH 05/22/2005 When we recalculate credit memo in view mode in case of entire invoice
  *B127806,1 EIH            we get the numeric data of header from invhdr   [Begin].

  *B131872,1 NNA 04/20/2006 (Begin) Comment this code because it is not logic specially if you changed in the header information
    *laData[23] = INVHDR.shipamt
    *laData[24] = -1*(INVHDR.discount)
    *laData[26] = INVHDR.Tax_Amt
    *laData[27] = INVHDR.Ncharges
  *B131872,1 NNA (End)

  *B127806,1 EIH 05/22/2005   [End]
  
  SHOW GET laData[28]
  
  *-- Disable or enable the objects related to the selected 
  *-- Invoice # & existance of lines.
  
  *-- Refresh R/A objects.
  lcRAStat  = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  SHOW GET ibRaNo     &lcRAStat
  SHOW GET laData[3]  &lcRAStat
  *-- Refresh invoice objects.
  lcInvStat = IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  *-- Refresh order objects.
  lcOrdStat = IIF(EMPTY(laData[8]) .AND. EMPTY(laData[3]) .AND. laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  *-- Refresh check box "return entire invoice".
  lcEntrStat= IIF(laScrMode[4] .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  *-- Refresh inquire invoice button.
  lcInqStat = IIF(!EMPTY(laData[7]) .AND. !laScrMode[1] , "ENABLE" , "DISABLE")
  *-- Refresh the division popup.
  
  *B602863,1 Reham On 05/12/1999   *** Begin ***
  *B602863,1 Disable the division popup if no invice & entered styles..
  *lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) , "ENABLE" , "DISABLE")
  *B605135,1 (Begin) Once we entered a RA, disable the Division without waiting getting into the 2nd folder and collecting lines.
  *lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 , "ENABLE" , "DISABLE")
  lcDivStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. IIF(llDiv_Seq , EMPTY(laData[3]) , .T.) .AND. lnCrMemNo = 0 AND (EMPTY(laData[3]) OR llFoldFrst), "ENABLE" , "DISABLE")
  *B605135,1 (End)
  *B602863,1 Reham On 05/12/1999   *** End   ***
  
  *-- Refresh currency code objects.
  lcCurStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') , "ENABLE" , "DISABLE")
  *-- Refresh exchange rate object.
  lcExRStat = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. !SEEK(laData[3] , 'RALINE') .AND. !EMPTY(laData[31]) .AND. llEditExRt .AND. laData[31] <> gcBaseCurr , "ENABLE" , "DISABLE")
  *-- Refresh GL link objects.
  lcGLStat  = IIF(laScrMode[4] .AND. EMPTY(laData[7]) .AND. llLink_GL .AND. llDiv_Link , "ENABLE" , "DISABLE")
  
  *-- Set flag to execute the browse.
  llReBrowse = .T.
  
  *B802222,1 Reham On 05/05/99   *** Begin ***
  SELECT (lcCrMemLin)
  GO TOP
  *B802222,1 Reham On 05/05/99   *** End   ***
ENDIF
*! B604896,1 MHM 10/08/2001 Empty Variable to collect right data [Start]
lcinvsllnk = ""
*! B604896,1 MHM [End]

*!*************************************************************
*! Name      : lfCrtUnComp
*! Developer : Reham Al-Allamy
*! Date      : 12/21/1997
*! Purpose   : Create all the needed temp. files.
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Parameters: llHeader  -> Flag to know if recreate header file.
*!             llDetails -> Flag to know if recreate detail file.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfCrtUnComp()
*!*************************************************************
*
**E301490,1 Move this function to program RMSave to be used for
**E301490,1 Inter-Company Communication Module
**E301490,1 (Start)

*!*	FUNCTION lfCrtUnComp
*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*B602719,1 Add new parameter for GLDIST file.
*!*	*PARAMETERS llHeader , llDetails
*!*	*PRIVATE llHeader , llDetails
*!*	*B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	*PARAMETERS llHeader , llDetails , llGLDist
*!*	PARAMETERS llHeader , llDetails , llGLDist , llOpenLine
*!*	PRIVATE llHeader , llDetails , llGLDist , llOpenLine
*!*	*B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	*B602719,1 Reham On 04/05/99   *** Begin ***

*!*	*-- If parameter send to create the temp. header file, create it from the 
*!*	*-- master Return header file.
*!*	IF llHeader
*!*	  SELECT RETHDR
*!*	  =AFIELDS(laCrMemHdr)
*!*	  lnCrMemHdr = ALEN(laCrMemHdr , 1)
*!*	  *-- Add field to know at which step the saving has done.
*!*	  DIMENSION laCrMemHdr [lnCrMemHdr+1,4]
*!*	  laCrMemHdr[lnCrMemHdr + 1 , 1] = 'NSTEPS'
*!*	  laCrMemHdr[lnCrMemHdr + 1 , 2] = 'N'
*!*	  laCrMemHdr[lnCrMemHdr + 1 , 3] = 6
*!*	  laCrMemHdr[lnCrMemHdr + 1 , 4] = 0
*!*	  *-- Call global function to create the temp. header file.
*!*	  =gfCrtTmp(lcCrMemHdr , @laCrMemHdr , [ACCOUNT] , lcCrMemHdr)
*!*	ENDIF

*!*	*B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	*-- If parameter send to create the temp. lines file, create it from the 
*!*	*-- master Return lines file.
*!*	*IF llDetails
*!*	*B802907,1 Reham On 12/28/1999   *** Begin ***

*!*	*-- Temp. name for the lines file.
*!*	SELECT RETLINE
*!*	=AFIELDS(laCrMemLin)
*!*	lnCrMemLin = ALEN(laCrMemLin , 1)
*!*	*B602932,1 Reham On 05/26/1999   *** Begin ***
*!*	*B602932,1 Increase the array dimension to add new logical field to the temp. file.
*!*	*-- Add 8 fields for open qty. to the lines file.
*!*	*DIMENSION laCrMemLin [lnCrMemLin+11,4]
*!*	DIMENSION laCrMemLin [lnCrMemLin+12,4]
*!*	*B602932,1 Reham On 05/26/1999   *** End   ***
*!*	FOR lnCount = 1 TO 8
*!*	  lcCount = ALLTRIM(STR(lnCount))
*!*	  laCrMemLin[lnCrMemLin + lnCount , 1] = 'NOPNQTY&lcCount'
*!*	  laCrMemLin[lnCrMemLin + lnCount , 2] = 'N'
*!*	  laCrMemLin[lnCrMemLin + lnCount , 3] = 6
*!*	  laCrMemLin[lnCrMemLin + lnCount , 4] = 0
*!*	ENDFOR
*!*	*-- Add field for total open qty. to the lines file.
*!*	laCrMemLin[lnCrMemLin + 9 , 1] = 'NTOTOPNQTY'
*!*	laCrMemLin[lnCrMemLin + 9 , 2] = 'N'
*!*	laCrMemLin[lnCrMemLin + 9 , 3] = 6
*!*	laCrMemLin[lnCrMemLin + 9 , 4] = 0
*!*	*-- Add logical field to know if we need to add the valid lines of the 
*!*	*-- selected invoice in the RA line file.
*!*	laCrMemLin[lnCrMemLin + 10 , 1] = 'LINVOICE'
*!*	laCrMemLin[lnCrMemLin + 10 , 2] = 'L'
*!*	laCrMemLin[lnCrMemLin + 10 , 3] = 1
*!*	laCrMemLin[lnCrMemLin + 10 , 4] = 0
*!*	*-- Add field to know at which step the saving has done.
*!*	laCrMemLin[lnCrMemLin + 11 , 1] = 'NSTEPS'
*!*	laCrMemLin[lnCrMemLin + 11 , 2] = 'N'
*!*	laCrMemLin[lnCrMemLin + 11 , 3] = 6
*!*	laCrMemLin[lnCrMemLin + 11 , 4] = 0
*!*	*B602932,1 Reham On 05/26/1999   *** Begin ***
*!*	*B602932,1 Add logical field to know if this style was shipped to this customer before or not.
*!*	laCrMemLin[lnCrMemLin + 12 , 1] = 'LSHIPPED'
*!*	laCrMemLin[lnCrMemLin + 12 , 2] = 'L'
*!*	laCrMemLin[lnCrMemLin + 12 , 3] = 0
*!*	laCrMemLin[lnCrMemLin + 12 , 4] = 0
*!*	*B602932,1 Reham On 05/26/1999   *** End   ***

*!*	*B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	*-- If parameter send to create the temp. lines file, create it from the 
*!*	*-- master Return lines file.
*!*	IF llDetails
*!*	*B802907,1 Reham On 12/28/1999   *** Begin ***
*!*	  *-- Call global function to create the temp. lines file.

*!*	  *B603741,1 WAB - create the temp file with 2 index tag 
*!*	  *B603741,1 WAB - StART
*!*	  *=gfCrtTmp(lcCrMemLin , @laCrMemLin , [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD] , lcCrMemLin)
*!*	  DECLARE laIndexTag[2,2]
*!*	  laIndexTag[1,1] = [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD]
*!*	  laIndexTag[1,2] = lcCrMmStyl 
*!*	  *B603939,1 WAB (START)- Change the index to val(CRET_LINNO) to sort correctly
*!*	  *laIndexTag[2,1] = [ACCOUNT+CRET_LINNO+STYLE]
*!*	  laIndexTag[2,1] = [ACCOUNT+STR(VAL(CRET_LINNO),4)+STYLE]
*!*	  *B603939,1 WAB (END)
*!*	  laIndexTag[2,2] = lcCrMmLine 
*!*	  =gfCrtTmp(lcCrMemLin , @laCrMemLin , @laIndexTag )  
*!*	  *B603741,1 WAB - END

*!*	ENDIF

*!*	*B802907,1 Reham On 12/26/1999   *** Begin ***
*!*	IF llOpenLine
*!*	  =gfCrtTmp(lcOpenLine , @laCrMemLin , [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD] , lcOpenLine)
*!*	ENDIF
*!*	*B802907,1 Reham On 12/26/1999   *** End   ***

*!*	*B602719,1 Reham On 04/05/99   *** Begin ***
*!*	*B602719,1 Create the GLDIST file only if linked to GL.
*!*	IF llLink_GL .AND. llGLDist
*!*	  SELECT GLDIST
*!*	  =AFIELDS(laGLDstTmp)
*!*	  *-- Call global function to create the temp. header file.
*!*	  =gfCrtTmp(lcGLDstTmp , @laGLDstTmp , [GLaccount] , lcGLDstTmp)
*!*	ENDIF
*!*	*B602719,1 Reham On 04/05/99   *** End   ***

**E301490,1 (End)

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Reham Al-Allamy
*! Date      : 02/16/1998
*! Purpose   : Add new record in the uncompleted session file.
*!*************************************************************
*! Calls     : lfUpdVars
*!*************************************************************
*! Parameters: lcAddObj -> Current object pressed.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfAdUnCmSR()
*!*************************************************************
*
FUNCTION lfAdUnCmSR
PARAMETERS lcAddObj
PRIVATE lcCurAlias , lcAddObj

*-- If the current object was not sent as a parameter, define it.
IF TYPE("lcAddObj") $ "UL"
  lcAddObj = SPACE(10)
ENDIF

*-- Save current alias.
lcCurAlias = ALIAS()

*-- Add record in the uncompleted session file.
SELECT 'UNCMSESS'
IF !SEEK('I')
  APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE Status     WITH 'O'       ;
        cUTranType WITH lcProgID  ;
        cUserId    WITH gcUser_id ;
        cSession   WITH lcSession ;
        cProgram   WITH 'RMCRMEM' ;
        cCurrScr   WITH 'RMCRMEM' ;
        dTranDate  WITH gdSysDate ;
        cCurrObj   WITH lcAddObj  ;
        cTranTime  WITH TIME()

*-- Lock the current record in the uncompleted session file.
=RLOCK()

*-- Call local function to update the temp. files & needed variables 
*-- fields in the uncompleted session file record.
=lfUpdVars()

*-- Restore the previous alias.
SELECT (lcCurAlias)

*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Reham Al-Allamy
*! Date      : 02/16/1998
*! Purpose   : Function to update the temp. files & the needed 
*!             variables for the uncompleted session file.
*!*************************************************************
*! Calls     : gfSavSess
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdVars()
*!*************************************************************
*
FUNCTION lfUpdVars
PRIVATE lcCurAlias

*-- Save the current alias.
lcCurAlias = ALIAS()

*B602719,1 Reham On 04/05/99  *** Begin ***
*B602719,1 Add the GLDIST info to the files variable to be saved in the uncomplete session file.
*-- Concatinate the files string to be send to the global function : gfSavSess
*lcFiles = 'lcCrMemHdr,'+lcCrMemHdr+','+lcCrMemHdr+';'+ ;
          'lcCrMemLin,'+lcCrMemLin+','+lcCrMemLin+';'
lcFiles = 'lcCrMemHdr,'+lcCrMemHdr+','+lcCrMemHdr+';'+ ;
          'lcCrMemLin,'+lcCrMemLin+','+lcCrMemLin+';'+ ;
          'lcGLDstTmp,'+lcGLDstTmp+','+lcGLDstTmp+';'
*B602719,1 Reham On 04/05/99  *** End   ***

*-- If there is record added in the uncompleted session file, call global 
*-- function to update the temp. files & variables fields in the uncompleted
*-- session file.
llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars))

*-- Restore the previous alias.
SELECT (lcCurAlias)

*!*************************************************************
*! Name      : lfChkUnComS
*! Developer : Reham Al-Allamy
*! Date      : 02/16/1998
*! Purpose   : Check if there is previous uncompleted session.
*!*************************************************************
*! Calls     : gfUnCompSession
*!*************************************************************
*! Parameters: llFrmSetup -> From screen setup or lpShow
*!*************************************************************
*! Returns   : llFondSess -> .T. if there is session found.
*!*************************************************************
*! Example   :  =lfChkUnComS()
*!*************************************************************
*
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE    llFondSess

*-- Flag to know if found uncompleted session or not.
llFondSess = .F.
*-- Flag to know if check the uncompleted session.
llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)

*-- If coming from the screen setup & should check the uncompleted session 
*-- Or coming from the lpshow procedure.
IF (llFrmSetup .AND. llChkUnCom) .OR. !llFrmSetup
  llChkUnCom = .F.
  *-- Call global function to know if there is uncompleted session was saved in
  *-- the uncompleted session file.
  IF gfUnCompSession(lcProgID, lnThisSess, "Credit Memo")
    *-- Flag to execute the activate folders.
    llFromSPR  = .T.
    *-- Restore the previous mode.
    STORE .F. TO laScrMode
    laScrMode[ATC(lcScrMode,"SVEA")] = .T.
    STORE .T. TO llContinue, llCUpDate
    lcSession = UnCmSess.cSession
    *-- Get the object should be pressed.
    lcCurObj  = UPPER(ALLTRIM(UnCmSess.cCurrObj))
    
    *-- Get the header fields values in the screen array.
    SELECT (lcCrMemHdr)
    IF &lcCrMemHdr..Status = "V"
      SCATTER FIELDS &lcVScFields TO laData
    ELSE
      SCATTER FIELDS &lcScFields  TO laData
    ENDIF
    *-- Get the default information for the current mode.
    =lfGetInfo()
    
    *-- If there is no object was saved and not coming from setup.
    IF EMPTY(lcCurObj) .OR. !llFrmSetup
      *-- Set the found session flag to true.
      llFondSess = .T.
      *-- Go to refresh the screen.
      SHOW GETS
    ELSE
      DO CASE
        *-- If the saved object was the <SAVE> button.
        CASE lcCurObj  = "PBSAV"
          *-- Call local procedure save.
          =lpSavScr()
          *-- Set the found session flag to false.
          llFondSess   = .F.
          *-- Set the current mode to be select mode.
          laScrMode    = .F.
          laScrMode[1] = .T.
          *-- Call the show procedure.
          DO lpShow
        *-- If the saved object was the <DELETE> button.
        CASE lcCurObj  = "PBDLT"
          *B602719,1 Reham On 04/05/99   *** Begin ***
          *B602719,1 Select the Credit Memo to be voided.
          =SEEK(&lcCrMemHdr..CrMemo , "RetHdr")
          *B602719,1 Reham On 04/05/99   *** End   ***
          *-- Call local procedure void.
          =lpDelScr()
          *-- Set the found session flag to false.
          llFondSess   = .F.
          *-- Set the current mode to be select mode.
          laScrMode    = .F.
          laScrMode[1] = .T.
          *-- Call the show procedure.
          DO lpShow
        OTHERWISE
          *-- Set the found session flag to true.
          llFondSess = .T.
          *-- Refresh the screen.
          SHOW GETS
      ENDCASE
    ENDIF
  ELSE
    *B602719,1 Reham On 04/05/99   *** Begin ***
    *-- Call local function to create the needed temp. files.
    *=lfCrtUnComp(.T. , .T.)
    
    *E301490,1 AHM 11/27/2000 Move Call this functiopn from RMSave Program
    *E301490,1 (Start)
    *=lfCrtUnComp(.T. , .T. , .T.)
    DO lfCrtUnComp IN (gcapphome+'\RMSave.PRG') WITH .T. , .T.,.T.
    *E301490,1 (End)
    
    *B602719,1 Reham On 04/05/99   *** End   ***
    
    *-- Set flag to execute the browse.
    llReBrowse = .T.
  ENDIF
ENDIF

*-- Return with flag if found uncompleted session or not.
RETURN (llFondSess)

*!*************************************************************
*! Name      : lfOpnFil
*! Developer : Reham Al-Allamy
*! Date      : 06/15/1998
*! Purpose   : Open the needed files in this session
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfOpnFil()
*!*************************************************************
*
FUNCTION lfOpnFil

*-- If GL Link.
IF llLink_GL
  *-- Open the gl link file if it was not open and set flag 
  *-- to close before quitting the program.
  IF !USED("GL_LINK")
    =gfOpenFile(gcDataDir+'GL_LINK' , gcDataDir+'GL_LINK' , 'SH')
  ENDIF
  
  *-- Open the gl distribution file if it was not open and set flag 
  *-- to close before quitting the program.
  IF !USED("GLDIST")
    =gfOpenFile(gcDataDir+'GLDIST' , gcDataDir+'GLDISTAC' , 'SH')
  ENDIF
ENDIF

*-- If multi currency.
IF llMulCurr
  *-- Open the Currency codes file if it was not open and set flag 
  *-- to close before quitting the program.
  IF !USED("SycCurr")
    =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
  ENDIF
  *-- Open the exchange rate file if it was not open and set flag 
  *-- to close before quitting the program.
  IF !USED("SycExch")
    =gfOpenFile(gcSysHome+'SycExch','','SH')
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfGetTrdDs
*! Developer : Reham Al-Allamy
*! Date      : 06/16/1998
*! Purpose   : Local function to get the trade discount.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcCurTerm -> Term Code
*!             lcCurInv  -> Invoice #
*!*************************************************************
*! Returns   : lnInvTrdDs
*!*************************************************************
*! Example   :  lnInvTrdDs = lfGetTrdDs(lcCurTerm , lcCurInv)
*!*************************************************************
*
**E301490,1 Move this function to program RMSave to be used for
**E301490,1 Inter-Company Communication Module
**E301490,1 (Start)
*
*!*	FUNCTION lfGetTrdDs
*!*	PARAMETERS lcCurTerm , lcCurInv
*!*	PRIVATE lcCurTerm , lcCurInv

*!*	lnInvTrdDs = 0

*!*	*-- If there is invoice #.
*!*	IF !EMPTY(lcCurInv)
*!*	  *-- If the terms used in the invoice changed.
*!*	  IF !(lcCurTerm == lcInvTerms)
*!*	    *-- Get the trade discount from the terms related fields in the codes file.
*!*	    IF !EMPTY(lcCurTerm)
*!*	      *-- Get the trade discount for the current term.
*!*	      DECLARE laTrmRltFd[1,2]
*!*	      laTrmRltFd[1,1] = 'NTERDISCR'
*!*	      laTrmRltFd[1,2] = 'lnInvTrdDs'
*!*	      =gfRltFld(lcCurTerm , @laTrmRltFd , "CTERMCODE")
*!*	    ELSE
*!*	      lnInvTrdDs = 0
*!*	    ENDIF
*!*	  ELSE
*!*	    *E301077,4 Reham On 12/27/98   *** Begin ***
*!*	    *E301077,4 Open the invoice header file.
*!*	    =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
*!*	    *E301077,4 Reham On 12/27/98   *** End   ***
*!*	    *-- If the terms code used in the invoice does not change, get the 
*!*	    *-- trade discount saved in the invoice header file.
*!*	    lnInvTrdDs = IIF(SEEK(lcCurInv , "INVHDR") , InvHdr.Trde_Disc , 0)
*!*	  ENDIF
*!*	ELSE
*!*	  *-- If there is no invoice # , use the trade discount from the related
*!*	  *-- fields in the codes file for the used terms code.
*!*	  *-- If not empty of the terms field.
*!*	  IF !EMPTY(lcCurTerm)
*!*	    *-- Get the trade discount for the current term.
*!*	    DECLARE laTrmRltFd[1,2]
*!*	    laTrmRltFd[1,1] = 'NTERDISCR'
*!*	    laTrmRltFd[1,2] = 'lnInvTrdDs'
*!*	    =gfRltFld(lcCurTerm , @laTrmRltFd , "CTERMCODE")
*!*	  ELSE
*!*	    lnInvTrdDs = 0
*!*	  ENDIF
*!*	ENDIF

*!*	RETURN lnInvTrdDs
**E301490,1 (End)

*!*************************************************************
*! Name      : lfCalFrmRA
*! Developer : Reham Al-Allamy
*! Date      : 07/01/1998
*! Purpose   : Function to calculate the header totals from RA lines.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCalFrmRA()
*!*************************************************************
*
FUNCTION lfCalFrmRA

*-- Blank the data will be affected by this function.
STORE 0 TO laData[18] , laData[21] , laData[22] , laData[23] , ;
           laData[24] , laData[25] , laData[26] , laData[30],lnduetax
*C102212,1 (Begin) Zero out HST Tax.
laData[42] = 0
*C102212,1 (End)
*-- If valid RA # & its lines exist in the RA lines file
IF !EMPTY(laData[3]) .AND. SEEK(laData[3] , "RALINE")
  *B605261,4 (Begin)  In case of invoice, get tax from the invoice directly..
  *laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)
  IF !EMPTY(laData[7])
    laData[26] = IIF(llIsEnglnd , lfchgTax() , IIF(llTax , InvHdr.Tax_Amt , 0))
  ELSE
    laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)  
  ENDIF
  *B605261,4 (End)

  *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
  laData[26] = ROUND(laData[26],2)
  *E302198,1 ALB change tax rate to print 99.999 formate [END]
  *B120085,1 ASH 11/05/2003 (Begin) Zero the tax amount to recompute it from the RALINE in case of entire invoice.
  IF !llIsEnglnd AND llTax AND !EMPTY(laData[7])
    laData[26] = 0
  ENDIF
  *B120085,1 ASH 11/05/2003 (End)
  SELECT RALINE
  *-- Loop to calculate the pieces, amount, discount, tax, pst amount.
  SCAN REST WHILE RANO = laData[3]
    IF SEEK(RALINE.Style , "STYLE")
      *B802223,1 Reham On 06/13/1999    *** Begin ***
      lnOpQtRA = (RALINE.nOpnQty1+RALINE.nOpnQty2+RALINE.nOpnQty3+RALINE.nOpnQty4+RALINE.nOpnQty5+RALINE.nOpnQty6+RALINE.nOpnQty7+RALINE.nOpnQty8)
      *B605261,4 (Begin) Don't calculate tax on non taxable styles 
      *lnOpAmot = (lnOpQtRA * RALINE.PRICE) * (1-(lnDiscLnPc/100))
      lnOpAmot  = IIF(STYLE.lTaxable,(lnOpQtRA * RALINE.PRICE) * (1-(lnDiscLnPc/100)),0)
      *B605261,4 (End)
      *-- Calculate the pieces, amount, discount, tax, pst amount.
      *laData[22] = laData[22] + RALINE.TotQty
      *laData[23] = laData[23] + RALINE.TotQty * (RALINE.PRICE/ (1-lnDiscPcnt/100))
      *laData[24] = laData[24] + (RALINE.TotQty * (RALINE.PRICE/ (1-lnDiscPcnt/100))) * lnDiscPcnt/100
      *laData[25] = laData[25] + RALINE.Amount
      *laData[26] = laData[26] + RALINE.Amount * (IIF(llIsEnglnd , lfMechDisc(RALINE.Style) , IIF(llTax , lnTaxRate , 0))/100)
      *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(RALINE.Style , RALINE.Price , "RALINE") , IIF(llTax , RALINE.Amount * lnTaxRate / 100 , 0))
      *laData[30] = IIF(llIsCanada , laData[30] + RALINE.Amount * (RALINE.nPstRate/100) , 0)
      *-- Calculate the pieces, amount, discount, tax, pst amount.
      laData[22] = laData[22] + lnOpQtRA
      laData[23] = laData[23] + lnOpQtRA * (RALINE.PRICE/ (1-lnDiscPcnt/100))
      laData[24] = laData[24] + (lnOpQtRA * (RALINE.PRICE/ (1-lnDiscPcnt/100))) * lnDiscPcnt/100
      laData[25] = laData[25] + lnOpAmot
      *B605261,4 (Begin) Caculate taxable amount
      *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(RALINE.Style , RALINE.Price , "RALINE") , IIF(llTax , lnOpAmot * lnTaxRate / 100 , 0))      
      lnDueTax = lnDueTax + lnOpAmot
      *B120085,1 ASH 11/05/2003 (Begin) Compute the tax from RALINE file.
      *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(RALINE.Style , RALINE.Price , "RALINE") , IIF(llTax , IIF(EMPTY(laData[7]),lnOpAmot * lnTaxRate / 100 , 0),0))
      IF !llIsEnglnd AND llTax AND !EMPTY(laData[7]) 
        laData[26] = laData[26] + (RALINE.nTotOpnQty * RALINE.Price * RALINE.Tax_Rate/100)
      *B120085,1 ASH 11/05/2003 (End)
      ELSE
        laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(RALINE.Style , RALINE.Price , "RALINE") , IIF(llTax , IIF(EMPTY(laData[7]),lnOpAmot * lnTaxRate / 100 , 0),0))
      ENDIF
      *B605261,4 (End)
  
      laData[30] = IIF(llIsCanada , laData[30] + lnOpAmot * (RALINE.nPstRate/100) , 0)
      *B802223,1 Reham On 06/13/1999    *** Begin ***
      
      *-- Calculate the cahrge back for sales reps.
      IF lnInvTrdDs <> 0
        laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
        laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
      ELSE
        laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
        laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
      ENDIF
    ENDIF
  ENDSCAN
  *E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
  laData[26] = ROUND(laData[26],2)
  *E302198,1 ALB change tax rate to print 99.999 formate [END]

  *C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
  IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
    =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
  ENDIF
  *C200517,1 KHM 04/13/2003 (End)

  *C102212,1 (Begin) Add HST Tax.
  *laData[28] = laData[27] + laData[25] + laData[26] + laData[30]
  laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
  *C102212,1 (End)
ENDIF

*-- Refresh the changed objects.
SHOW GET laData[18] 
FOR lnCount = 21 TO 26
  SHOW GET laData[lnCount] 
ENDFOR
SHOW GET laData[28]
SHOW GET laData[30] 
*C102212,1 (Begin) Show get HST Tax amount.
SHOW GET laData[42]  &lcLinStat
*C102212,1 (End)

*!*************************************************************
*! Name      : lfCalFrmIn
*! Developer : Reham Al-Allamy
*! Date      : 07/01/1998
*! Purpose   : Function to calculate the header totals from 
*!           : invoice lines.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCalFrmIn()
*!*************************************************************
*
FUNCTION lfCalFrmIn

*-- Blank the data will be affected by this function.
STORE 0 TO laData[18] , laData[21] , laData[22] , laData[23] , ;
           laData[24] , laData[25] , laData[26] , laData[30]
*C102212,1 (Begin) Zero out HST Tax.
laData[42] = 0
*C102212,1 (End)
*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Open the consolidated header & lines & invoiceheader & lines file.
=gfOpenFile(gcDataDir+'CONSINVH',gcDataDir+'CONSINVH','SH')
=gfOpenFile(gcDataDir+'CONSINVL',gcDataDir+'CONSINVL','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
*E301077,4 Reham On 12/27/98   *** End   ***
*-- If valid invoice # & consolidated invoice.
IF !EMPTY(laData[7]) .AND. SEEK(laData[7] , "INVHDR") .AND. INVHDR.Consol = 'Y'
  
  *B127806,1 EIH 05/22/2005 get charges in case of consolidates [Begin].
  IF llIsEnglnd
    laData[27] = INVHDR.ncharges
  ELSE
     laData[27] = INVHDR.freight + INVHDR.cod + INVHDR.insur
  ENDIF
  *B127806,1 EIH [End].
  
  laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)
  IF SEEK(laData[7] , "CONSINVH")
    SELECT CONSINVH
    SCAN WHILE Invoice = laData[7] FOR cWareCode = laData[29]
      *B605261,1 (Begin) When making CR from an invoice, get the invoice tax directly.
      laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(CONSINVL.Style , CONSINVL.PRICE , "CONSINVL") , IIF(llTax ,CONSINVH.Tax_Amt , 0))
      *B605261,1 (End)
      SELECT CONSINVL
      *B606272,1 (Begin) Get the exact order inside the consolidated invoice.
      *IF SEEK(laData[7] + CONSINVH.Store)
      IF SEEK(laData[7] + CONSINVH.Store+CONSINVH.Order)
      *B606272,1 (End)
        *-- Loop in the invoice lines to be sure of its existance in the style file.
        SCAN WHILE Invoice+Store+Order = laData[7]+CONSINVH.Store+CONSINVH.Order ;
                   .AND. SEEK(CONSINVL.Style , "Style")
          *B607818,1 ASH 11/02/2006 (Begin) Get only the lines for the CONSINVH piktkt's.
          IF PIKTKT<> CONSINVH.PIKTKT
            LOOP
          ENDIF
          *B607818,1 ASH 11/02/2006 (End)
          *-- Calculate the pieces, amount, discount, tax, pst amount.
          lnCurQty   = CONSINVL.Qty1 + CONSINVL.Qty2 + CONSINVL.Qty3 + ;
                       CONSINVL.Qty4 + CONSINVL.Qty5 + CONSINVL.Qty6 + ;
                       CONSINVL.Qty7 + CONSINVL.Qty8
          lnCurAmt   = lnCurQty * (CONSINVL.PRICE * ( 1 - CONSINVH.DISCPCNT/100))
          laData[22] = laData[22] + lnCurQty
          laData[23] = laData[23] + lnCurQty * CONSINVL.PRICE
          laData[24] = laData[24] + (CONSINVL.PRICE * lnCurQty * lnDiscPcnt / 100)
          
          *B131872,1 NNA 04/20/2006 (Begin) Round the discount field
          laData[24] = ROUND(laData[24],2)
          *B131872,1 NNA (End)
          
          laData[25] = laData[25] + lnCurAmt
          *B605261,4 (Begin) Caculate taxable amount
          lnDueTax = lnDueTax + IIF(SEEK(STYLE,'STYLE') AND Style.lTaxable,lnCurAmt,0)
          *B605261,4 (End)
          *laData[26] = laData[26] + ((lnCurAmt + IIF(lcTax_Meth = "A" AND !llIsEnglnd , laData[27] , 0)) * (IIF(llIsEnglnd , lfMechDisc(CONSINVL.Style) , IIF(llTax , lnTaxRate , 0))/100))
          *B605261,1 (Begin) Remark it here.
          *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(CONSINVL.Style , CONSINVL.PRICE , "CONSINVL") , IIF(llTax , (lnCurAmt + IIF(lcTax_Meth = "A" , laData[27] , 0)) * lnTaxRate / 100 , 0))
          laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(CONSINVL.Style , CONSINVL.PRICE , "CONSINVL") , 0)
           
          *B605261,1 (End)
          laData[30] = IIF(llIsCanada , laData[30] + lnCurAmt * (lnPstRate/100) , 0)
          SELECT CONSINVL
        ENDSCAN
      ENDIF
      SELECT CONSINVH
    ENDSCAN
  ENDIF
ELSE
  *B605261,1 (Begin) When making CR from an invoice, get the invoice tax directly.
  *laData[26] = IIF(llIsEnglnd , lfchgTax() , 0)
  laData[26] = IIF(llIsEnglnd , lfchgTax() , IIF(llTax , IIF(cbEntr_Inv,InvHdr.Tax_Amt,laData[27]*(lnTaxRate / 100)) , 0))  
  *B120085,1 ASH 11/05/2003 (Begin) Get the tax rate from the invoice.
  lnTaxRate = INVHDR.Tax_Rate
  *:B120085,1 ASH 11/05/2003 (End)
  *B605261,1 (End)
  *-- Get the lines from the invline file in case the invoice is not consolidated.
  SELECT INVLINE
  IF SEEK(laData[7] , "INVLINE")
    *-- Loop in the invoice lines to be sure of its existance in the style file.
    SCAN WHILE INVLINE.Invoice = laData[7] FOR SEEK(INVLINE.Style , "STYLE")
      *-- Calculate the pieces, amount, discount, tax, pst amount.
      lnCurQty   = InvLine.Qty1 + InvLine.Qty2 + InvLine.Qty3 + InvLine.Qty4 + ;
                   InvLine.Qty5 + InvLine.Qty6 + InvLine.Qty7 + InvLine.Qty8
      lnCurAmt   = lnCurQty * (INVLINE.PRICE*(1-(lnDiscPcnt/100)))
      laData[22] = laData[22] + lnCurQty
      laData[23] = laData[23] + lnCurQty * InvLine.PRICE
      *laData[24] = laData[24] + (InvLine.GROS_PRICE * lnCurQty * lnDiscPcnt/100)
      laData[24] = laData[24] + (InvLine.PRICE * lnCurQty * lnDiscPcnt/100)

      *B131872,1 NNA 04/20/2006 (Begin) Round the discount field then get the net amount
      *laData[25] = laData[25] + lnCurAmt
      laData[24] = ROUND(laData[24],2)      
      laData[25] = laData[23] - laData[24]
      *B131872,1 NNA (End)
  
      *B037963,1 NNA 04/15/2004 (Begin) Calculate the [Other] Field Value
      IF llIsEnglnd
        laData[27] = INVHDR.ncharges
      ELSE
        laData[27] = INVHDR.freight + INVHDR.cod + INVHDR.insur
      ENDIF
      *B037963,1 NNA (End)

      *B605261,4 (Begin) Caculate taxable amount
      lnDueTax = lnDueTax + IIF(SEEK(STYLE,'STYLE') AND Style.lTaxable,lnCurAmt,0)
      *B605261,4 (End)
      
      *laData[26] = laData[26] + ((lnCurAmt + IIF(lcTax_Meth="A" AND !llIsEnglnd ,laData[27],0)) * (IIF(llIsEnglnd , lfMechDisc(InvLine.Style) , IIF(llTax , lnTaxRate , 0))/100))
      *B605261,1 (Begin) Remark it here.
      *laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(InvLine.Style , InvLine.PRICE , "InvLine") , IIF(llTax , (lnCurAmt + IIF(lcTax_Meth = "A" , laData[27] , 0)) * lnTaxRate / 100 , 0))
      laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(InvLine.Style , InvLine.PRICE , "InvLine") ,0)
      *B605261,1 (End)
      laData[30] = IIF(llIsCanada , laData[30] + lnCurAmt * (lnPstRate/100) , 0)
    ENDSCAN
  ENDIF
ENDIF
*C102212,1 (Begin) Calculate HST Tax Rate.
=SEEK(laData[7],'INVHDR')
laData[42] = IIF(llIsCanada , laData[42] + INVHDR.nHstAmt , 0)
*C102212,1 (End)

IF lnInvTrdDs <> 0
  laData[18] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[17]/100),2)) * -1
  laData[21] = ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (laData[20]/100),2)) * -1
ELSE
  laData[18] = ABS(ROUND(laData[25] * (laData[17]/100),2)) * -1
  laData[21] = ABS(ROUND(laData[25] * (laData[20]/100),2)) * -1
ENDIF

*C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
ENDIF
*C200517,1 KHM 04/13/2003 (End)

*E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
laData[26] = ROUND(laData[26],2)
*E302198,1 ALB change tax rate to print 99.999 formate [END]

*C102212,1 (Begin) Add HST Tax.
*laData[28] = laData[27] + laData[25] + laData[26] + laData[30]


*B127806,1 EIH 05/21/2005 round the total credit to calc same as invoice [Begin].
*laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
laData[28] = ROUND(laData[27] + laData[25] + laData[26] + laData[30] + laData[42],2)
*If entir invoice get different amounts of invoice from invhdr
=lfEntInvHd()
*B127806,1 EIH 05/21/2005 [End].





*C102212,1 (End)
*-- Refresh the changed objects.
SHOW GET laData[18] 
FOR lnCount = 21 TO 26
  SHOW GET laData[lnCount] 
ENDFOR

*B037963,1 NNA 04/15/2004 (Begin) Displayed the [Other] Field Value
SHOW GET laData[27]
*B037963,1 NNA (End)

SHOW GET laData[28]
SHOW GET laData[30] 
*B120085,1 ASH 11/05/2003 (Begin) Refresh the tax rate field.
SHOW GET lnTaxRate
*:B120085,1 ASH 11/05/2003 (End)
*C102212,1 (Begin) Show get HST Tax amount.
SHOW GET laData[42]  &lcLinStat
*C102212,1 (End)

*!*************************************************************
*! Name      : lfGetprice
*! Developer : Reham Al-Allamy
*! Date      : 03/08/1999
*! Purpose   : Get Style Price
*!*************************************************************
*! Calls     : lfCheckPri()
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : Total Quantity
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfGetprice(lcStyle,'A',100)
*!*************************************************************
*
FUNCTION lfGetprice
PARAMETERS lcStyle,lcLevel,lnQuantity
PRIVATE lnStyPric

*E301142,4 Reham On 03/08/99
*E301142,4 Get the style price level.
=SEEK(lcStyle,'Style')
IF lcLevel = 'Q'
  DO CASE
    CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
      lcLevel = 'C'
    CASE Style.nAtQtyB > 0 AND lnQuantity > Style.nAtQtyB
      lcLevel = 'B'
    OTHERWISE
      lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(lcLevel,'A','B','C'),lcLevel,'A')
ENDIF
*E301142,4 Call global function to get the price level.
lnStyPric = IIF(!llMulCurr OR laData[31] = gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcPriceLvl,laData[31]))

*E301142,4 Call local function to select between the different existing 
*E301142,4 price levels if the selected one is zero.
RETURN(IIF(lnStyPric = 0 , lfCheckPric(lcStyle,lcLevel) , lnStyPric))

*!*************************************************************
*! Name      : lfCheckPric
*! Developer : Reham Al-Allamy
*! Date      : 03/08/1999
*! Purpose   : Select alternative price level
*!*************************************************************
*! Calls     : gfStyPrice(),SOSTYPRI.SPX
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfCheckPric(lcstyle,'A')
*!*************************************************************
*
FUNCTION lfCheckPric
PARAMETERS lcStyle,lcLevel
PRIVATE lcTitle,lcPrompt,lcPrompt1,lcPrompt2,lcPrompt3,lnStyPric,lnStyPric1,;
        lnStyPric2,lnStyPric3, lnPrice

*E301142,4 Seek in the style file to get the right price level.
=SEEK(lcStyle,'Style')
lcTitle  = PROPER(lcStyHdr)+': '+lcStyle
lnStyPric1 = IIF(!llMulCurr OR laData[31]=gcBaseCurr,Style.PriceA,;
                       gfStyPrice(lcStyle,'A',laData[31]))
lnStyPric2 = IIF(!llMulCurr OR laData[31]=gcBaseCurr,Style.PriceB,;
                       gfStyPrice(lcStyle,'B',laData[31]))
lnStyPric3 = IIF(!llMulCurr OR laData[31]=gcBaseCurr,Style.PriceC,;
                       gfStyPrice(lcStyle,'C',laData[31]))
lnStyPric  = IIF(lnStyPric1 >0,1,IIF(lnStyPric2 >0,2,AT(lcLevel,'ABC')))
lcPrompt   = "Price level '"+lcLevel+"' is zero. Proceed with"
lcPrompt1  = 'Price level A, '+ALLTRIM(STR(lnStyPric1,12,2))
lcPrompt2  = 'Price level B, '+ALLTRIM(STR(lnStyPric2,12,2))
lcPrompt3  = 'Price level C, '+ALLTRIM(STR(lnStyPric3,12,2))

lnPrice    = 1

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*E301142,4 Call screen to select price levels.
DO (gcScrDir+"SOSTYPRI.SPX")

*-- Restore the trapped keys.
POP KEY

*RETURN(EVAL('lnStyPric'+STR(lnStyPric,1)))
RETURN(EVAL('lnStyPric'+STR(lnPrice,1)))

*!*************************************************************
*! Name      : lfCalcDisc
*! Developer : Reham Al-Allamy
*! Date      : 03/09/1999
*! Purpose   : Calculate style dicount for the selected price.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcStyle   : Style
*!                       lnCurPric : Price
*!*************************************************************
*! Returns            :  Price after discount
*!*************************************************************
*! Example            :  =lfCalcDisc(lcStyle,lnCurPric)
*!*************************************************************
*
FUNCTION lfCalcDisc
PARAMETERS lcStyle , lnCurPric
PRIVATE lcStyle , lnCurPric

*E301142,4 Seek to find there is a discount for the current style price.
IF SEEK(lcStyle , 'Style')
  *-- If the discount code for the selected style not empty.
  IF !EMPTY(Style.cDiscCode)
    *-- Get the discount percentage for this discount code from the codes file.
    DECLARE laStyDisc[1,2]
    laStyDisc[1,1] = 'DISCPCNT'
    laStyDisc[1,2] = 'lnDiscPcnt'
    =gfRltFld(Style.cDiscCode , @laStyDisc , "CDISCCODE")
    
    *-- If the discount percentage greater than zero...
    IF lnDiscPcnt > 0
      IF !(&lcCrMemLin..GROS_PRICE == (lnCurPric - (lnCurPric * lnDiscPcnt / 100)))
        *B602863,1 Reham On 05/24/1999   *** Begin ***
        *lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnCurPric)) + ;
                   "|" + ALLTRIM(STR(lnCurPric * lnDiscPcnt /100)) + "|" + ;
                   ALLTRIM(STR(lnCurPric - (lnCurPric * lnDiscPcnt / 100)))
        lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnCurPric,10,3)) + ;
                   "|" + ALLTRIM(STR((lnCurPric * lnDiscPcnt /100),10,3)) + "|" + ;
                   ALLTRIM(STR(lnCurPric - (lnCurPric * lnDiscPcnt / 100),10,3))
        *B602863,1 Reham On 05/24/1999   *** End   ***
        
        *** There is discount percentage on this style : {lnDiscPcnt} ***
        *** Gross Price is     : {lnCurPric}
        *** Discount amount is : {lnCurPric * lnDiscPcnt /100}
        *** Net Price is       : {lnCurPric - (lnCurPric * lnDiscPcnt / 100)}
        =gfModalGen("INM46030B00000" , "DIALOG" , lcTmpStr)
      ENDIF
      *-- Get a discount on the total gross price, & inform the user.
      lnCurPric = lnCurPric - (lnCurPric * lnDiscPcnt / 100)
    ENDIF
  ENDIF
ENDIF

*E301142,4 Return with the current price.
RETURN lnCurPric

*!*************************************************************
*! Name      : lfCreatHst
*! Developer : Reham Al-Allamy
*! Date      : 05/04/1999
*! Purpose   : Create Temporary file for ARHIST.
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfCreatHst()
*!*************************************************************
*E301216,1 Reham On 05/04/99
*E301216,1 Function to create the temp. ARHIST file will be sent 
*E301216,1 to the keyoff program
*!*************************************************************
*
FUNCTION lfCreatHst

SELECT ARHIST
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*! B604896,1 MHM 10/08/2001 delete this record becuase we add it in standerd on[Start]
*DIMENSION laFileStru[lnFileStru+7,4]
*laFileStru[lnFileStru+1,1] = "nStep"
*laFileStru[lnFileStru+1,2] = "N"
*laFileStru[lnFileStru+1,3] = 2
*laFileStru[lnFileStru+1,4] = 0
*laFileStru[lnFileStru+2,1] = "cYear"
*laFileStru[lnFileStru+2,2] = "C"
*laFileStru[lnFileStru+2,3] = 4
*laFileStru[lnFileStru+2,4] = 0
*laFileStru[lnFileStru+3,1] = "cPrd"
*laFileStru[lnFileStru+3,2] = "C"
*laFileStru[lnFileStru+3,3] = 2
*laFileStru[lnFileStru+3,4] = 0
*laFileStru[lnFileStru+4,1] = "cbnkcode"
*laFileStru[lnFileStru+4,2] = "C"
*laFileStru[lnFileStru+4,3] = 8
*laFileStru[lnFileStru+4,4] = 0
*laFileStru[lnFileStru+5,1] = "cchkacct"
*laFileStru[lnFileStru+5,2] = "C"
*laFileStru[lnFileStru+5,3] = 12
*laFileStru[lnFileStru+5,4] = 0
*laFileStru[lnFileStru+6,1] = "cShToOpn"
*laFileStru[lnFileStru+6,2] = "C"
*laFileStru[lnFileStru+6,3] = 1
*laFileStru[lnFileStru+6,4] = 0
*laFileStru[lnFileStru+7,1] = "nTrnNewAmn"
*laFileStru[lnFileStru+7,2] = "N"
*laFileStru[lnFileStru+7,3] = 11
*laFileStru[lnFileStru+7,4] = 2

DIMENSION laFileStru[lnFileStru+5,4]
laFileStru[lnFileStru+1,1] = "nStep"
laFileStru[lnFileStru+1,2] = "N"
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = "cYear"
laFileStru[lnFileStru+2,2] = "C"
laFileStru[lnFileStru+2,3] = 4
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = "cPrd"
laFileStru[lnFileStru+3,2] = "C"
laFileStru[lnFileStru+3,3] = 2
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = "cShToOpn"
laFileStru[lnFileStru+4,2] = "C"
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = "nTrnNewAmn"
laFileStru[lnFileStru+5,2] = "N"
laFileStru[lnFileStru+5,3] = 11
laFileStru[lnFileStru+5,4] = 2

*! B604896,1 MHM 10/08/2001 [End]

*--Index tags array.
DIMENSION laTags[3,2]
laTags[1,1] = 'Account+TranType+Tran+cInstalNo'
laTags[1,2] = lcTmpAR
laTags[2,1] = 'Account+Tran+cInstalNo'
laTags[2,2] = 'Tran'
laTags[3,1] = 'cShToOpn'
laTags[3,2] = 'Show'

*--Call Create temp file.
=gfCrtTmp(lcTmpAR,@laFileStru,@laTags)
SELECT (lcTmpAR)
SET ORDER TO TAG (lcTmpAR)

*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!*************************************************************************
*E301245,1

**E301490,1 Move this function to program RMSave to be used for
**E301490,1 Inter-Company Communication Module
**E301490,1 (Start)

*!* FUNCTION lfHgWUpdat
*!* =RLOCK()
*!* REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
*!* UNLOCK

*-- End of lfHgWUpdat.
**E301490,1 (End)

*!*************************************************************
*! Name      : lfvPstRate
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/20/2001
*! Purpose   : Valid function of the Hst tax Rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvHstRate()
*!*************************************************************
*C102212,1
FUNCTION lfvHstRate

*-- Validate the pst rate & update the current line.
SELECT (lcCrMemLin)
laData[42] = IIF(llIsCanada , laData[42] - &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)
*-- Lock the record to grantee the phiscal update.
= RLOCK()
REPLACE NHSTRATE WITH lnHstRate
UNLOCK
laData[42] = IIF(llIsCanada , laData[42] + &lcCrMemLin..Amount * (&lcCrMemLin..nHstRate/100) , 0)

*C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
ENDIF
*C200517,1 KHM 04/13/2003 (End)

*C102212,1 (Begin) Show get HST Tax amount.
laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
SHOW GET laData[42]
SHOW GET laData[28]
*C102212,1 (End)
llCUpDate = .T.
*!*************************************************************
*! Name      : lfchgTax2
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 08/05/02
*! Purpose   : Calculate the charge taxes in case of country 
*!           : is England WITHOUT checking Entire invoice or not.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfchgTax2()
*!*************************************************************
*B606248,1 
FUNCTION lfchgTax2
*-- Calculate the tax charges if the country code is england and the 
*-- invoice no. is not empty.
STORE 0 TO lnTChgTax
IF llIsEnglnd AND lcTax_Meth = "A" AND !EMPTY(&lcCrMemHdr..Invoice)
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  =gfOpenFile(gcDataDir+'INVCHRG',gcDataDir+'INVCHRG','SH')
  IF SEEK(&lcCrMemHdr..Invoice , "INVCHRG")
    lcOldAlias = ALIAS()
    SELECT INVHDR
    SET ORDER TO TAG INVHDR
    =SEEK(&lcCrMemHdr..Invoice)
    SELECT INVCHRG
    SCAN WHILE INVCHRG.Invoice = &lcCrMemHdr..Invoice
      lnChgTax  = INVCHRG.nChrgAmnt * INVCHRG.nTaxRate/100
      DO lfGetTrdDs IN (gcapphome+'\RMSave.PRG') WITH &lcCrMemHdr..cTermCode , &lcCrMemHdr..Invoice , lnInvTrdDs
      lnChgTax  = lnChgTax  - (lnChgTax * lnInvTrdDs/100)
      lnTChgTax = lnTChgTax + lnChgTax
    ENDSCAN
    SELECT (lcOldAlias)
  ENDIF
ENDIF
RETURN (lnTChgTax)

*:*************************************************************
*: Name      : lfvCharges
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : England Charges Screen
*:*************************************************************
*: Calls     : RMCHRG.SPX
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvCharges()
*:*************************************************************
*:B606720,1
FUNCTION lfvCharges
PRIVATE lnAlias
lnTrdDisc = 0


lnAlias = SELECT()
IF laData[15] = "Y" .AND. !EMPTY(ladata[7])
  = lfHdrBchrg ()
ENDIF

*B607195,1 KHM 06/03/2003 (Begin) In case of the view mode display the charges
IF laScrMode[2] 
  =lfGetChrgs()
  RETURN
ENDIF  
*B607195,1 KHM 06/03/2003 (End)
IF EMPTY(ladata[2])
  Wait Window 'Please try to select account first.' TIMEOUT 3 NOWAIT
  RETURN
ENDIF

IF laScrMode[4] AND !USED(lcEngChrg)
  
  *B607195,1 KHM 06/03/2003 (Begin) Opening the CRMEMO charges file.
  =gfOpenFile(gcDataDir+'CrmChrg',gcDataDir+'CrmChrg','SH')
  *B607195,1 KHM 06/03/2003 (End) 
  
  =gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
  SELECT InvChrg
  =AFIELDS(laFileStru)
  =gfCloseFile('InvChrg')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  *--  Create temp file for the english Charges
  =gfCrtTmp(lcEngChrg,@laFileStru,[Order+cStore+PikTkt+cchrgcode],lcEngChrg)
ENDIF

*-- in case use intier invoice, get the data for this invoice.
IF !EMPTY(ladata[7]) .AND. llFrstTime
  *:B131092,1 MHM 03/07/2005 Fix bug of Invalid charge fields on new credit memo[Start].
  IF cbEntr_Inv
  *:B131092,1 MHM [End].
  
    llFrstTime = .F.
    lnTrdDisc = IIF(SEEK(ladata[7],'InvHdr'),Invhdr.trde_disc,0)
    = lfgetData ()
  *:B131092,1 MHM 03/07/2005 Fix bug of Invalid charge fields on new credit memo[Start].
  ENDIF
  *:B131092,1 MHM [End].

*B607195,1 KHM 06/03/2003 (Begin) Getting the trade discount percentage in case of no invoice
ELSE
  IF EMPTY(ladata[7])
    DECLARE laEngTrmTx[1,2]
    STORE 0 TO lnTrdDisc
    STORE '' TO laEngTrmTx
    laEngTrmTx[1,1] = 'NTERDISCR'
    laEngTrmTx[1,2] = 'lnTrdDisc'
    =gfRltFld(laData[40],@laEngTrmTx,'CTERMCODE') 
  ELSE
    lnTrdDisc = IIF(SEEK(ladata[7],'InvHdr'),Invhdr.trde_disc,0)
  ENDIF
*B607195,1 KHM 06/03/2003 (End)

ENDIF
*-

DO lpCharges WITH lcEngChrg,laData[7],laData[2],laData[8],PADR(ALLTRIM(laData[4]),8),;
                            lcInvPiktk,lnTrdDisc,'laData[27]','lnChrgtax'

*:B131092,1 MHM 03/07/2005 Fix bug of Invalid charge fields on new credit memo[Start].
IF laScrMode[4]
*IF laScrMode[4] AND cbEntr_Inv
*:B131092,1 MHM [End].
  =lfMechTax()
ENDIF


SELECT (lnAlias)

*-- End OF lfvCharges
*:*************************************************************
*: Name      : lpCharges
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : England Charges Screen
*:*************************************************************
*: Calls     : ARChrg.SPX
*:*************************************************************
*: Parameters: lcChrgFile: Charges Temp. file name
*:             lcInvoice : Invoice Number 
*:             lcAccount : Account
*:             lcOrderNo : Order Number
*:             lcStore   : Store 
*:             lcPikTkt  : PikTkt Number
*:             lnTrdDsc  : Trade Discount Percent
*:             lcTotChrg : Invoice total charges
*:             lcTotTax  : Invoice total charges tax
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lpCharges()
*:*************************************************************
*:B606720,1
FUNCTION lpCharges

PARAMETERS lcChrgFile,lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,;
           lnTrdDsc,lcTotChrg,lcTotTax
           
PRIVATE lnAlias,laLCodes,laCharges,lnChrgMark,lnTotChrg,lnTotTax,llCompTax,;
        laChRltFld

PRIVATE lnOldChrg
STORE 0 TO lnOldChrg

lnAlias = SELECT()
*-- company setup information
*B120187,1 ASH 11/05/2003 (Begin) Fix the bug of computing tax on charges even if the customer vat exempted.
*llCompTax = (gfGetMemVar('M_TAX',gcAct_Comp)='Y')  && Use Taxes = Y 
llVatExmp = .F.
IF !EMPTY(laData[4])
  lnCusRec = RECNO('CUSTOMER')
  =SEEK('S'+laData[2]+laData[4],'CUSTOMER')
  llVatExmp = Customer.lvatexem 
  GOTO lnCusRec in Customer
ELSE
  llVatExmp = Customer.lvatexem 
ENDIF

llCompTax = (gfGetMemVar('M_TAX',gcAct_Comp)='Y')  AND !llVatExmp && Use Taxes = Y and customer is not vat exempted
*B120187,1 ASH 11/05/2003 (End)
llGlLink  = (gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y')  && GL link
lnChrgMark = 0
DECLARE laLCodes[1,10],laCharges[1,2]
STORE '' TO laCharges
lnTotChrg = &lcTotChrg

=IIF(laScrMode[2] OR laScrMode[3] ,gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH'),.T.)
SELECT (lcChrgFile)
IF laScrMode[4]  && Add Mode
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE Order+cStore+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
ELSE
  =SEEK(lcInvoice+lcStore)
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE invoice+cstore+cchrgcode = lcInvoice+lcStore
  =SEEK(lcInvoice+lcStore)
ENDIF  
llNewChrg = .F.
SCATTER MEMVAR
laLCodes[1,1]  = 'CCHRGCODE'
laLCodes[1,2]  = 'laCharges'
laLCodes[1,3]  = 'puCharges'
laLCodes[1,4]  = ''
laLCodes[1,5]  = .F.
laLCodes[1,6]  = .F.
laLCodes[1,7]  = lcChrgFile
laLCodes[1,8]  = lcChrgFile
laLCodes[1,9]  = 'lcOrderNo+lcStore+lcPikTkt+m.cchrgcode'
laLCodes[1,10] = 'CCHRGCODE'
=gfwCodePop(@laLCodes,'CCHRGCODE','N')

PUSH KEY
ON KEY
lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpDetEsc
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)
DO (gcScrDir+gcWinAppl+"\RMChrg.SPX")
POP KEY

ON KEY LABEL ESC     &lcHldEsc

IF laScrMode[4]
  SELECT (lcChrgFile)
  =SEEK(IIF(laScrMode[2],lcInvoice,lcOrderNo+lcStore+lcPikTkt))
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE Order+cStore+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
  &lcTotChrg = lnTotChrg
  &lcTotTax  = lnTotTax
ENDIF

=IIF(laScrMode[2] OR laScrMode[3] ,gfCloseFile('InvChrg'),.T.)


*B127806,1 EIH 05/22/2005 If entir invoice get different amounts of invoice from invhdr [Begin].
=lfEntInvHd()
=lfRefresh()
*B127806,1 EIH [End].



*-- End OF lpCharges
*:*************************************************************
*: Name      : lfMechTax
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Compute England merchandise tax
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfMechTax()
*:*************************************************************
*:B606720,1
FUNCTION lfMechTax
PRIVATE lnAlias

lnAlias = SELECT()

=SEEK(IIF(EMPTY(laData[4]),'M'+laData[2],'S'+laData[2]+laData[4]),'CUSTOMER')

lnChrgtax  = 0
IF USED(lcEngChrg)
  SELECT (lcEngChrg)
  =SEEK(laData[8]+laData[4]+lcInvPiktk)
  SUM REST nChrgAmnt*(1-(lnTrdDisc)/100)*nTaxRate/100 TO lnChrgtax  ;
  WHILE Order+cStore+PikTkt+cchrgcode = laData[8]+laData[4]+lcInvPiktk
ENDIF

SELECT(lcCrMemLin) 
laData[26] = 0 
SCAN
  *--calculate tax amount
  laData[26] = laData[26] + IIF(llIsEnglnd , lfCurTax(&lcCrMemLin..Style , &lcCrMemLin..GROS_PRICE , lcCrMemLin) , IIF(llTax AND SEEK(&lcCrMemLin..STYLE,'STYLE') AND Style.lTaxable, (&lcCrMemLin..Amount + IIF(lcTax_Meth = "A" , laData[27] , 0)) * &lcCrMemLin..TAX_RATE/ 100 , 0))
ENDSCAN  

laData[26] = laData[26] + lnChrgtax

*C200517,1 KHM 04/30/2003 (Begin) To calculate tax amount based on tax code = "000100"
IF ASCAN(laEvntTrig , PADR('RMTXBYWH',10)) <> 0
  =gfDoTriger('RMCRMEM',PADR('RMTXBYWH',10))
ENDIF
*C200517,1 KHM 04/13/2003 (End)

laData[28] = laData[27] + laData[25] + laData[26] + laData[30]

*B127806,1 EIH 05/22/2005 If entir invoice get different amounts of invoice from invhdr [Begin].
=lfEntInvHd()
*B127806,1 EIH 05/22/2005 [End].

FOR lnCount = 25 TO 28
  SHOW GET laData[lnCount]
ENDFOR 

*E302198,1 ALB change tax rate to print 99.999 formate [BEGIN]
laData[26] = ROUND(laData[26],2)
*E302198,1 ALB change tax rate to print 99.999 formate [END]

*B131872,1 NNA 04/20/2006 (Begin) refresh tax rate field 
SHOW GET laData[26]
*B131872,1 NNA (End)

SELECT (lnAlias)

=lfRefresh()
*-- End OF lfMechTax
*:*************************************************************
*: Name      : lfChrgBrow
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Browse England Charges
*:*************************************************************
*: Calls     : lfShowChrg
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfChrgBrow()
*:*************************************************************
*:B606720,1
FUNCTION lfChrgBrow
PRIVATE lcFields

lcFields = "cMarker =IIF(RECNO()=lnChrgMark,'>',' '):H=' ':R:1:W=.F.,"+;
           "cDesc=gfCodDes(cChrgCode,'CCHRGCODE') :H= 'Charge' :30 :R,"+;
           "nChrgAmnt :H= 'Amount' :9 :R"
lcFields = lcFields + IIF(llCompTax,",nTaxRate :H='Tax Rate' :R,;
nTaxAmt=nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 :H='Tax Amount' :P='99999999.99' :R",'')
lcFields = lcFields + IIF(llGlLink,",cFrgtAcnt :H='G.L. Account' :R",'')

lnChrgMark = RECNO()

*B607195,1 KHM 06/03/2003 (Begin) Changing the key command in case of view mode because
*B607195,1                we have created a standard file that will hold the charges for 
*B607195,1                the credit memo.
*BROWSE FIELDS &lcFields  ;
       WINDOW RMChrg1    ;
       IN WINDOW RMChrg  ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShowChrg() ;
       KEY IIF(laScrMode[2],lcInvoice+lcStore,lcOrderNo+lcStore+lcPikTkt) ;
       TITLE lcBrowChrg

BROWSE FIELDS &lcFields  ;
       WINDOW RMChrg1    ;
       IN WINDOW RMChrg  ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShowChrg() ;
       KEY IIF(laScrMode[2],laData[1]+lcStore,lcOrderNo+lcStore+lcPikTkt) ;
       TITLE lcBrowChrg
*B607195,1 KHM 06/03/2003 (End)

*--End Of lfChrgBrow.
*:*************************************************************
*: Name      : lfShowChrg
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Show England Charges
*:*************************************************************
*: Calls     : gfwCodePop
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfShowChrg()
*:*************************************************************
*:B606720,1
FUNCTION lfShowChrg

lnChrgMark = RECNO()
SHOW WINDOW (lcBrowChrg) REFRESH SAME
SCATTER MEMVAR
=gfwCodePop(@laLCodes,'CCHRGCODE','T')
SHOW GETS WINDOW 'RMCHRG2'

*-- End OF lfShowChrg.
*:*************************************************************
*: Name      : lfvNewChrg
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Add new charge
*:*************************************************************
*: Calls     : gfModalGen
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvNewChrg()
*:*************************************************************
*:B606720,1
FUNCTION lfvNewChrg
PRIVATE lcCurrKey,lnChrgCount

SELECT (lcChrgFile)
lcCurrKey = Order+cStore+PikTkt+cchrgcode 
COUNT TO lnChrgCount 
=SEEK(lcCurrKey)
IF lnChrgCount = 10
  *E300817,1  Message : 40026
  *E300817,1  You can enter upto 10 differennt charge codes.
  *E300817,1  Button : 00000
  *E300817,1  Ok
  =gfModalGen('INM40026B00000','ALERT')
  RETURN
ENDIF
SCATTER MEMVAR BLANK
SHOW GETS WINDOW 'RMCHRG2' DISABLE
SHOW GET puCharges  ENABLE
_CUROBJ = OBJNUM(puCharges)
llNewChrg = .T.

*-- End OF lfvNewChrg.  
*:*************************************************************
*: Name      : lfDChrgBrow
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Deactivate function for Charges screen
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfDChrgBrow()
*:*************************************************************
*:B606720,1
FUNCTION lfDChrgBrow

IF WONTOP() = lcBrowChrg
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpCTab WITH 'RMCHRG2','puCharges','1'
  ON KEY LABEL BACKTAB DO lpCTab WITH 'RMCHRG2','pbClsChrg','2'
ENDIF  

RETURN .F.

*-- End OF lfDChrgBrow  
*:*************************************************************
*: Name      : lfvChrgCode
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Validate England Charge Codes
*:*************************************************************
*: Calls     : gfRltFld
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvChrgCode()
*:*************************************************************
*:B606720,1
FUNCTION lfvChrgCode

m.cChrgCode = ALLTRIM(laCharges[puCharges,2])
IF !llNewChrg .AND. m.cChrgCode = cChrgCode
  RETURN
ENDIF
lcOldChrg = cChrgCode
*E300817,1  Message : 40025
*E300817,1  Charge code xxx has already been selected!
*E300817,1  Button : 00000
*E300817,1  Ok
IF SEEK(lcOrderNo+lcStore+lcPikTkt+m.cChrgCode,lcChrgFile) .AND. ;
  gfModalGen('INM40025B00000','ALERT',m.cChrgCode)=1
  SCATTER MEMVAR
  SHOW GETS WINDOW 'RMCHRG2' ENABLE
  RETURN
ENDIF
=SEEK(lcOrderNo+lcStore+lcPikTkt+lcOldChrg,lcChrgFile)
IF llCompTax
  DECLARE laChRltFld[1,2]
  STORE '' TO lcTaxRate
  laChRltFld[1,1] = 'CTAXCODE'
  laChRltFld[1,2] = 'lcTaxRate'
  =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
  IF !EMPTY(lcTaxRate)
    laChRltFld[1,1] = 'NTAXRATE'
    laChRltFld[1,2] = 'lcTaxRate'
    =gfRltFld(ALLTRIM(lcTaxRate),@laChRltFld,'CTAXCODE')
    m.nTaxRate = lcTaxRate
  ENDIF
  *B602891,1 AMM end
  SELECT (lcChrgFile)  
ENDIF

IF llGlLink
  DECLARE laChRltFld[1,2]
  STORE '' TO lcFrgtAcnt
  laChRltFld[1,1] = 'CFRGTACNT'
  laChRltFld[1,2] = 'lcFrgtAcnt'
  =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
  m.cFrgtAcnt= lcFrgtAcnt
  SELECT (lcChrgFile)  
ENDIF
*E300883,1 (End)

IF llNewChrg
  INSERT INTO (lcChrgFile) (Invoice,Order,PikTkt,cStore) ;
  VALUES (lcInvoice,lcOrderNo,lcPikTkt,lcStore)
  llNewChrg = .F.
ENDIF
lnTotTax = lnTotTax - (nTaxRate-m.nTaxRate)*m.nChrgAmnt*(1-lnTrdDsc/100)/100
=RLOCK()

REPLACE cChrgCode WITH m.cChrgCode ,;
        nTaxRate  WITH m.nTaxRate  ,;
        cFrgtAcnt WITH m.cFrgtAcnt

UNLOCK
SHOW WINDOW (lcBrowChrg) REFRESH SAME
SHOW GETS WINDOW 'RMCHRG2' ENABLE
SHOW GETS WINDOW 'RMCHRG0' OFF
RETURN

*-- End OF lfvChrgCode
*:*************************************************************
*: Name      : lfvChrgCode
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Remove England Charge Code
*:*************************************************************
*: Calls     : gfRltFld
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvChrgCode()
*:*************************************************************
*:B606720,1
FUNCTION lfvRemChrg

*E300817,1  Message : 40011
*E300817,1  Are you sure you want to remove this xxx?
*E300817,1  Button : 40000
*E300817,1  Yes No
IF gfModalGen('QRM40011B40000','ALERT','charge code')= 1
  lnTotChrg = lnTotChrg - nChrgAmnt
  lnTotTax  = lnTotTax  - nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100
  DELETE
  GO TOP
  SCATTER MEMVAR
  IF EOF()
    =gfwCodePop(@laLCodes,'CCHRGCODE','N')
    SHOW GETS WINDOW 'RMCHRG2' DISABLE
    SHOW GET pbNewChrg ENABLE
    SHOW GET pbClsChrg ENABLE
  ELSE
    SHOW GETS WINDOW 'RMCHRG2' ENABLE
  ENDIF
  SHOW WINDOW (lcBrowChrg) REFRESH SAME
  =lfRefresh()
ENDIF

*-- End OF lfvChrgCode
*:*************************************************************
*: Name      : lfvChrgAmnt
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : England Charge Code Amount
*:*************************************************************
*: Calls     : gfRltFld
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvChrgAmnt()
*:*************************************************************
*:B606720,1
FUNCTION lfvChrgAmnt
IF m.nChrgAmnt <0 
  * Message : 42000
  * Negative values are not allowed.
  * Buttfon  : 40011
  * Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nChrgAmnt=lnOldChrg
  _CUROBJ = _CUROBJ  
  RETURN
ENDIF


lnTotChrg = lnTotChrg - nChrgAmnt + m.nChrgAmnt
lnTotTax  = lnTotTax  - (nChrgAmnt-m.nChrgAmnt)*(1-lnTrdDsc/100)*nTaxRate/100
=RLOCK()
REPLACE nChrgAmnt WITH m.nChrgAmnt
UNLOCK
SHOW GETS WINDOW 'RMCHRG0' OFF
SHOW GETS WINDOW 'RMCHRG2' OFF

*-- End of lfvChrgAmnt.
*:*************************************************************
*: Name      : lfvChrgAcnt
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : England Charge Code Gl Account
*:*************************************************************
*: Calls     : gfRltFld
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvChrgAcnt()
*:*************************************************************
*:B606720,1
FUNCTION lfvChrgAcnt
llFrmBrow = llBrowse

REPLACE cFrgtAcnt WITH m.cFrgtAcnt
=lfvGlAccnt()
llBrowse=.F.
=RLOCK()
REPLACE cFrgtAcnt WITH m.cFrgtAcnt
UNLOCK

*-- End OF lfvChrgAcnt.
*:*************************************************************
*: Name      : lfTaxDesc
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : England Charge Code tax description
*:*************************************************************
*: Calls     : gfRltFld,gfCodDes
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfTaxDesc()
*:*************************************************************
*:B606720,1
FUNCTIO lfTaxDesc

DECLARE laChRltFld[1,2]
STORE '' TO lcTaxRate
laChRltFld[1,1] = 'CTAXCODE'
laChRltFld[1,2] = 'lcTaxRate'
=gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
RETURN(gfCodDes(ALLTRIM(lcTaxRate),'CTAXCODE'))

*-- End OF lfTaxDesc
*:*************************************************************
*: Name      : lfSetGLMsk
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : This function is to load the GL account mask and width
*:           : To variables that will be used in all the screens that
*:           : display any GL account.
*:*************************************************************
*: Calls     : gfRltFld,gfCodDes
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfSetGLMsk()
*:*************************************************************
*:B606720,1
FUNCTION lfSetGLMsk
PRIVATE lnAlias

lnAlias    = SELECT(0)

llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',gcAct_Comp)))
*E301148,1 AMM Initialize flag
llOpAcCh = .F.

IF llGlLink
  IF lcLinkWith $ "AO"
    USE (gcSyshome + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    llNoThing  = SEEK(gcAct_Comp, "CompFile")
    lcPthToUse = gcDataDir
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      llNoThing  = SEEK(lcPrntComp, "CompFile")
      lcPthToUse = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
    ENDIF
    USE IN CompFile
    USE (lcPthToUse + "ACCOD") IN 0 AGAIN ALIAS CodeStr ORDER AccSegNo
    SELECT CodeStr
    GOTO TOP
    lcRep     = IIF(lcLinkWith = "A", "9", "X")
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    USE IN CodeStr
    IF lcLinkWith = "A" AND !USED('lcLinkChar')
      USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE AGAIN ALIAS lcLinkChar
      llOpAcCh = .T.
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    lcAcntBrwF = "cAcctCode:"+ALLTRIM(STR(lnAcLen))+":H='Account Code',"+;
                 "cAccNlDes:65:H='Description'"
    lcAcntFld  = "cAcctCode"
    lcAcntDesF = "cAccNlDes"
  ELSE
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , gcAct_Comp))
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', gcAct_Comp))
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    lcAcMask   = SPACE(0)
    USE (lcAcntStrc) IN 0 AGAIN ALIAS AcntStrc ORDER SegID
    SELECT AcntStrc
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    USE IN AcntStrc
    IF !USED("lcLinkChar")
      USE (lcAcntChrt) IN 0 AGAIN ALIAS lcLinkChar ORDER GlAcnt
      llOpAcCh = .T.
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    lcAcntBrwF = "glAcnt:"+ALLTRIM(STR(lnAcLen))+":H='Account Code',"+;
                 "glDesc:53:H='Description'"
    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"
  ENDIF
  
ENDIF

SELECT (lnAlias)

*-- End of lfSetGLMsk.
*:*************************************************************
*: Name      : lfClearKey
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Clear key
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfClearKey()
*:*************************************************************
*:B606720,1
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+Z
ON KEY LABEL ALT+A
ON KEY LABEL ALT+C

*-- End OF lfClearKey.
*:*************************************************************
*: Name      : lfReadAct
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : READ Activate function of invoice screen
*:*************************************************************
*: Calls     : gfStopBrow,lfClearKey.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfReadAct()
*:*************************************************************
*:B606720,1
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
IF WONTOP() <> lcCrMem2 .AND. WONTOP() <> lcFolder .AND. WONTOP() <> 'GWCCONTRL1' .AND. ;
   laScrMode[4] .AND. EMPTY(laData[2])
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*-- End OF lfReadAct.
*:************************************************************
*: Name      : lfvGLAccnt
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : This is a global AR function to validate any enterd
*:           : GL account on any AR screen, it will update the object
*:           : that is called from, and refresh it on the screen.
*:*************************************************************
*: Calls     : gfStopBrow,lfClearKey.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfvGLAccnt()
*:*************************************************************
*:B606720,1
FUNCTION lfvGLAccnt
PARAMETERS lcAccDscOb, lcRefFunNm
PRIVATE lnAlias, lcCurVar, lcCurVal  , lcEmpty
PRIVATE llEmpty, laTemp  , lcBrFields, lcFile_Ttl
PRIVATE lcToGet, lcNoThing

lnAlias    = SELECT(0)
lcNoThing  = SPACE(0)
lcAccDscOb = IIF(TYPE("lcAccDscOb")#"C", "lcNoThing", lcAccDscOb)
lcRefFunNm = IIF(TYPE("lcRefFunNm")#"C", SPACE(0)   , lcRefFunNm)
llFrmBrow  = IIF(TYPE("llFrmBrow" )#"L", .F.        , llFrmBrow )
lcCurVar   = SYS(18)
lcCurVal   = EVAL(lcCurVar)
lcEmpty    = STRTRAN(lcAcMask,"9",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"!",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"X",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"#",SPACE(1))
llEmpty    = EMPTY(lcCurVal) OR (lcCurVal = lcEmpty)
lcRetVal   = SPACE(0)

IF (!llEmpty AND !SEEK(lcCurVal, "lcLinkChar")) OR llFrmBrow
  SELECT lcLinkChar
  DIMENSION laTemp[2]
  laTemp     = SPACE(0)
  llFrmBrow  = .F.
  lcBrFields = lcAcntBrwF
  lcFile_Ttl = "Chart of Accounts"
  lcToGet    = lcAcntFld + "," + lcAcntDesF
  IF gfBrows(.F.,lcToGet,'laTemp')
    &lcCurVar   = laTemp[1]
    &lcAccDscOb = laTemp[2]
  ELSE
    &lcCurVar   = SPACE(lnAcLen)
    &lcAccDscOb = SPACE(0)
    _CUROBJ     = _CUROBJ
  ENDIF
  IF !EMPTY(lcRefFunNm)
    DO &lcRefFunNm
  ENDIF
  SHOW GET &lcCurVar
  SELECT(lnAlias)
ENDIF  

*-- End OF lfvGLAccnt
*:*************************************************************
*: Name      : lfgetData
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : England Charge Code tax description
*:*************************************************************
*: Calls     : gfRltFld,gfCodDes
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfgetData()
*:*************************************************************
*:B606720,1
FUNCTION lfgetData

IF SEEK(laData[8],lcEngChrg)
  RETURN
ENDIF
=gfOpenFile(gcDataDir+'INVCHRG',gcDataDir+'INVCHRG','SH')
SELECT INVCHRG
=SEEK(laData[7])
SCAN REST WHILE invoice+cstore+cchrgcode= laData[7]
  SCATTER MEMVAR MEMO
  = SEEK(laData[7],'Invhdr')
  m.Order   = InvHdr.Order
  M.PikTkt  = InvHdr.piktkt
  INSERT INTO (lcEngChrg) FROM MEMVAR
ENDSCAN
lcInvPiktk = InvHdr.piktkt

SELECT (lcEngChrg)

*-- End OF lfgetData
*:*************************************************************
*: Name      : lpDetEsc
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpDetEsc
*:*************************************************************
*
PROCEDURE lpDetEsc

_CUROBJ = OBJNUM(pbClsChrg)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpDetEsc
*:*************************************************************
*: Name      : lpCTab
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Trap of tab key.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpCTab WITH 'ARDINV0', 'pbClose'
*:*************************************************************
*:
PROCEDURE lpCTab
PARAMETERS lcWindName, lcObjName , lcType

IF  lcType = '1'
  ON KEY LABEL TAB 
ELSE
  ON KEY LABEL BACKTAB
ENDIF

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)


*-- End OF lpCTab.
*:*************************************************************
*: Name      : lfHdrBchrg
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : England Charge Code tax description
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : = lfHdrBchrg()
*:*************************************************************
*:B606720,1
FUNCTION lfHdrBchrg
PRIVATE lnAlias
 
lnAlias = SELECT(0)
IF !llFoldFrst .AND. laScrMode[4] .AND. lnCrMemNo = 0
  =gfOpenFile(gcDataDir+'CONSINVH',gcDataDir+'CONSINVH','SH')
  =gfOpenFile(gcDataDir+'CONSINVL',gcDataDir+'CONSINVL','SH')
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  =gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  *-- In add mode only and enter the detail folder for the first 
  *-- time and there is no lines entered yet.
  *-- Call local function to get the credit memo lines according to
  *-- the entered data in the credit memo header.
  IF lfGetLines()
  *-- Invert the flag to prevent entering the folder again.
  llFoldFrst = .T.
  *-- Call local function to update the temp. files & needed variables 
  *-- fields in the uncompleted session file record.
  =lfUpdVars()
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN 

*-- End of lfHdrBchrg
*:*************************************************************
*: Name      : lfUpdWarHs
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Update the header warehouse.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : = lfUpdWarHs ()
*:*************************************************************
*:B606774,1
FUNCTION lfUpdWarHs
PRIVATE lcInvwrCode , lcPrvAlis
lcInvwrCode = ''

lcInvwrCode = IIF(SEEK(laData[7],'INVHDR'),Invhdr.cWarecode,'')
IF !EMPTY(lcInvwrCode) .AND. laData[29] # lcInvwrCode
  *--  Check if there is any styles not assigned to the current 
  *-- warehouse or not.
  lcPrvAlis = ALIAS()      && Save current alias.
  llFirst   = .T.          && flag to display the message only once.
  llAssign  = .F.          && Flag to assign all styles to changed warehouse.
  SELECT (lcCrMemLin)      && Select temp. file hold credit memo lines.
  SCAN
    *-- Check if there is any styles not assigned to the changed warehouse.
    IF !SEEK(PADR(&lcCrMemLin..Style,19) + laWareHous[puWareHous,2] + SPACE(10) , 'StyDye')
      *-- If the message was not displayed before.
      IF llFirst
        llFirst = .F.
        *** There is one or more styles not assigned to location: laWareHous[puWareHous,2]. ***
        *** <  Assign  > - < Cancel > ***
        IF gfModalGen("QRM46034B46003" , "DIALOG" , laWareHous[puWareHous,2]) = 1
          *-- Set flag to assign the unsaaigned styles to the current warehouse.
          llAssign = .T.
        ENDIF
      ENDIF
      *-- Assign the styles to the selected location.
      IF llAssign
        laData[29] = laWareHous[puWareHous,2]
        =SEEK(laData[29],'WareHous')
        DO gpAdStyWar WITH PADR(&lcCrMemLin..Style,19) , SPACE(10) , laWareHous[puWareHous,2]
      ELSE
        *-- Do not assign & restore the old location.
        lnOldValue =  IIF(ASCAN(laWareHous,lcInvwrCode) > 0 , ASCAN(laWareHous,lcInvwrCode)/2 , 0)
        puWareHous = lnOldValue
        laData[29] = laWareHous[puWareHous,2]
        =SEEK(laData[29],'WareHous')
        EXIT
      ENDIF
    ENDIF
    SELECT (lcCrMemLin)
  ENDSCAN
  *-- Fill the credit memo header file with the current data.
  =lfFilTmpHd()
  SELECT (lcPrvAlis)
ENDIF

*-- End OF lfUpdWarHs
*:*************************************************************
*: Name      : lfvData_26()
*: Developer : Abdou Elgendy [Abd]
*: Date      : 04/22/2003
*: Purpose   : Recacluate the total tax and charges ..etc.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : = lfvData_26 ()
*:*************************************************************
*:B607104,1 [Begin]
FUNCTION lfvData_26

*-- Validate the other field.
IF !(lnOldValue == laData[26])
  laData[28] = laData[27] + laData[25] + laData[26] + laData[30] + laData[42]
ENDIF

SHOW GET laData[26]
SHOW GET laData[27]
SHOW GET laData[28]

*-- Fill the credit memo header file with the current data.
=lfFilTmpHd()


*-- End OF lfvData_26
*:B607104,1
*:*************************************************************

*:*************************************************************
*: Name      : lfGetChrgs
*: Developer : Khalid Mohi El-Din Mohamed KHM
*: Date      : 06/03/2003
*: Purpose   : display the charges for the selected CR or Void mem
*:*************************************************************
*: Example   : = lfGetChrgs ()
*:*************************************************************
*: B607195,1 KHM 06/03/2003 
*:*************************************************************
FUNCTION lfGetChrgs
PRIVATE lnAlias,laLCodes,laCharges,lnChrgMark,lnTotChrg,lnTotTax,llCompTax,;
        laChRltFld, lnOldChrg

IF USED(lcEngChrg)
  USE IN (lcEngChrg)
  ERASE &gcWorkDir.&lcEngChrg..DBF
  ERASE &gcWorkDir.&lcEngChrg..CDX
ENDIF

*-- company setup information
llCompTax = (gfGetMemVar('M_TAX',gcAct_Comp)='Y')  && Use Taxes = Y
llGlLink  = (gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y')  && GL link

DECLARE laLCodes[1,10],laCharges[1,2]
STORE '' TO laCharges
lnTotChrg = laData[27]

STORE 0 TO lnOldChrg, lnTrdDsc, lnChrgMark, lnTotTax
lnAlias = SELECT(0)

=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
=gfOpenFile(gcDataDir+'CrmChrg',gcDataDir+'CrmChrg','SH')

SELECT CrmChrg
IF !USED(lcEngChrg)
  =AFIELDS(laFileStru)
  =gfCrtTmp(lcEngChrg,@laFileStru,[CrMemo+Store+cchrgcode],lcEngChrg)
ELSE
  SELECT (lcEngChrg)
  DELETE ALL
ENDIF  

SELECT CrmChrg
IF SEEK(laData[1]+laData[4])
  SCAN REST WHILE CrMemo+Store+cChrgCode = laData[1]+laData[4]
    SCATTER MEMVAR
    INSERT INTO (lcEngChrg) FROM MEMVAR
  ENDSCAN
ENDIF


IF !EMPTY(ladata[7])
  lnTrdDsc = IIF(SEEK(ladata[7],'InvHdr'),Invhdr.trde_disc,0)
  lcInvoice = laData[7]
ELSE
  DECLARE laEngTrmTx[1,2]
  STORE 0 TO lnTrdDsc
  STORE '' TO laEngTrmTx
  laEngTrmTx[1,1] = 'NTERDISCR'
  laEngTrmTx[1,2] = 'lnTrdDsc'
  =gfRltFld(laData[40],@laEngTrmTx,'CTERMCODE') 
  lcInvoice = ''
ENDIF

SELECT (lcEngChrg)
SUM ALL nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax 
lnTotChrg = laData[27]
LOCATE


SCATTER MEMVAR
laLCodes[1,1]  = 'CCHRGCODE'
laLCodes[1,2]  = 'laCharges'
laLCodes[1,3]  = 'puCharges'
laLCodes[1,4]  = ''
laLCodes[1,5]  = .F.
laLCodes[1,6]  = .F.
laLCodes[1,7]  = lcEngChrg
laLCodes[1,8]  = lcEngChrg
laLCodes[1,9]  = 'laData[1]+laData[4]+m.cchrgcode'
laLCodes[1,10] = 'CCHRGCODE'
=gfwCodePop(@laLCodes,'CCHRGCODE','N')

lcStore   = laData[4]

PUSH KEY
ON KEY
lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpDetEsc
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)
DO (gcScrDir+gcWinAppl+"\RMChrg.SPX")
POP KEY

ON KEY LABEL ESC  &lcHldEsc

IF USED(lcEngChrg)
  USE IN (lcEngChrg)
  ERASE &gcWorkDir.&lcEngChrg..DBF
  ERASE &gcWorkDir.&lcEngChrg..CDX
ENDIF

SELECT(lnAlias)
*:*************************************************************
*: Name      : lfHdrFields
*: Developer : Nader Nabil Abd Elmonem NNA
*: Date      : 01/29/2004
*: Purpose   : Zero Header Fields when Return entire invoice ='N'
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : = lfHdrFields ()
*:*************************************************************
*:B037264,1 NNA 01/29/2004
*:*************************************************************
FUNCTION lfHdrFields

*:B131790,1 MHM 03/07/2005 Fix bug of Tax amount on cr memos being removed on saving . [Start]
*IF cbEntr_Inv = .F. AND !(llAddLine)
IF cbEntr_Inv = .F. AND !(llCUpDate)
*:B131790,1  [End]

  laData[18]=0
  laData[21]=0
  laData[22]=0
  laData[23]=0
  laData[24]=0
  laData[25]=0    
  laData[26]=0
  laData[27]=0
  laData[30]=0
  laData[28]=0
  laData[42]=0

*:B131790,1 MHM 03/07/2005 Fix bug of Tax amount on cr memos being removed on saving . [Start]
ELSE
  RETURN  
*:B131790,1 . [End]

ENDIF  
SHOW GET laData[18]
SHOW GET laData[21]
SHOW GET laData[22]
SHOW GET laData[23]
SHOW GET laData[24]
SHOW GET laData[25]
SHOW GET laData[26]
SHOW GET laData[27]
SHOW GET laData[28]
SHOW GET laData[30]
SHOW GET laData[42]



*:*************************************************************
*: Name      : lfEntInvHd
*: Developer : Ehab Ismail Hamed EIH
*: Date      : 01/26/2005
*: Purpose   : Get the data from invhdr if entire invoice ='Y'
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : = lfEntInvHd()
*:*************************************************************
FUNCTION lfEntInvHd
*B129286,1 MMR 12/20/2005 Fix bug of retaining the header amounts after saving.
*IF llEntrInv 

*:B131583,1 MHM 03/07/2005  [Start]
*IF llEntrInv AND !LASCRMODE[1]
IF llEntrInv AND LASCRMODE[2]
*:B131583,1 MHM 03/07/2005  [End]

*B129286,1 MMR.[End]
  laData[23] = INVHDR.shipamt
  laData[24] = -1*(INVHDR.discount)
  laData[26] = INVHDR.Tax_Amt

  *B131872,1 NNA 04/20/2006 (Begin)
  *laData[27] = INVHDR.Ncharges
  IF llIsEnglnd
    laData[27] = INVHDR.ncharges
  ELSE
    laData[27] = INVHDR.freight + INVHDR.cod + INVHDR.insur
  ENDIF
  *B131872,1 NNA (End)
  
  laData[28] = INVHDR.totalchg
  
  *B127187,1 EIH 05/22/2005 Fix another bug that when select entire invoice before select invoice# tax is wrong [Begin].
  SHOW GET laData[26] ENABLE
  *B127187,1 EIH  [End].
  
ENDIF

*!*************************************************************
*! Name        : lfGetHST
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 11/18/2007
*! Purpose     : To get the HST from Codes file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : 
*!*************************************************************
FUNCTION lfGetHST
lcAlias = ALIAS()

SELECT CODES
lcOldOrd = ORDER()
SET ORDER TO Codes
llRpHSTTax = .F.
IF SEEK("N"+ LEFT(Customer.cAddress4,6)+"YSTATE")
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = "N"+ LEFT(Customer.cAddress4,6)+"YSTATE"
    IF crltd_nam = "LHSTTAX   "
       llRpHSTTax = IIF(ALLTRIM(crltd_vlu) = "T",.T.,.F.)
       EXIT
    ENDIF
  ENDSCAN  
ENDIF

SELECT CODES
SET ORDER TO &lcOldOrd 

SELECT (lcAlias)

*:*************************************************************


