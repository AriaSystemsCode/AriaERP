*C101960,1 ASH 09/07/2000 Change the text printed at the bottom of the invoice form due to the specs.
*laText1 = IIF(gcAct_Comp $ '0102','THIS ACCOUNT HAS BEEN SOLD AND ASSIGNED TO, IS OWNED BY AND IS PAYABLE IN U.S. FUNDS ONLY TO FINOVA',;
*                                  'This account is payable to MARAN INC., in US Funds at par value only. All checks under the tittle of') 
*laText2 = IIF(gcAct_Comp $ '0102','CAPITAL CORPORATION, P.O. BOX 13627 NEWARK, N.J. 07188-0627, (212)403-0700, TO WHOM PROMPT NOTICE ',;
*                                  'MARAN INC., need to be sent to the lock box of our collection agent, "ATLAS BUSINESS SERVICE')
*laText3 = IIF(gcAct_Comp $ '0102','MUST BE GIVEN OF ANY OBJECTIONS TO PAYMENT OF THIS INVOICE AS RENDERED, GOODS RETURNABLE FOR ANY ',;
*                                  'CORP., POBOX 817 Madison Square Station, New York, NY 10010." To whom notice must be given of any')
*laText4 = IIF(gcAct_Comp $ '0102','REASON SHALL BE RETURNED ONLY UPON WRITTEN NOTICE TO FINOVA',;
                                  'merchandise returns or claims for shortage, non-delivery, or for other grounds.')

*C102823,1 KHM 05/01/2003 Change the text printed at the bottom of the invoice form due to the specs.
*laText1 = IIF(gcAct_Comp $ '0102','THIS ACCOUNT HAS BEEN SOLD AND ASSIGNED TO, IS OWNED BY AND IS PAYABLE IN U.S. FUNDS ONLY TO GMAC',;
*                                  'This account is payable to MARAN INC., in US Funds at par value only. All checks under the tittle of') 
*laText2 = IIF(gcAct_Comp $ '0102','COMMERCIAL CREDIT LLC, P.O.BOX 13728 NEWARK, N.J. 07188-0728, TO WHOM PROMPT NOTICE MUST BE GIVEN',;
*                                  'MARAN INC., need to be sent to the lock box of our collection agent, "ATLAS BUSINESS SERVICE')
*laText3 = IIF(gcAct_Comp $ '0102','OF ANY OBJECTIONS TO PAYMENT OF THIS INVOICE AS RENDERED, GOODS RETURNABLE FOR ANY REASON SHALL',;
*                                  'CORP., POBOX 817 Madison Square Station, New York, NY 10010." To whom notice must be given of any')
*laText4 = IIF(gcAct_Comp $ '0102','BE RETURNED ONLY UPON WRITTEN NOTICE TO GMAC',;
                                  'merchandise returns or claims for shortage, non-delivery, or for other grounds.')

laText1 = 'Payable only to:'
laText2 = 'GMAC Commercial Credit LLC,'
laText3 = 'P.O. Box 403058'
laText4 = 'Atlanta GA 30384-3058'
laText5 = 'Owner/Assignee to whom prompt written notice must be given to any objection to payment.'
*C102823,1 KHM 05/01/2003 (End)