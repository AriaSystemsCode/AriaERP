*:***************************************************************************
*: Program file  : ARPINVRG
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR ROBERT GASPARD
*! Date          : 12/08/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVRG
*:***************************************************************************
*: This Report Program is due to C ...101717
*:***************************************************************************
*: Modification
*: B802999,1 BWA 27/01/2000 Fix the bug in the FRX of ...
*: B802999,1                1) Include the COD charges and insurance amounts
*:                             in the freight field.
*:                          2) Change the program to pick the style/color description 
*:                             from the order line file instead of the style file.
*: B803409,1 BWA 07/25/2000 Change the pick up of the descreption to be from the invline
*: B803409,1                not from the ordline [Fix in the FRX].
*:***************************************************************************
*Variables in .FRX
*LcScale  && Search if the seconed field in the scale file is empty or not to put 
*            in the unit field in the FRX the word EACH or the scale size descreption.
*:***************************************************************************

*--Declare an arrays to hold the Credit Data to print them in the <FRX> 
DECLARE laCredit[5,1]

*-- lcTypeRg  && Variable that is hold the name of the message memo file
*-- lcTypeRg = 'ArpinvRg'
IF FILE(gcDataDir+lcTypeRg+'.MEM')
  RESTORE FROM gcDataDir+lcTypeRg+'.MEM' ADDITIVE
ENDIF

PRIVATE lcAlias
lcAlias = ALIAS()

*-- Open the Ordline file to get from it the OrdLine TotQty. which is the back order quantity.

IF !USED(lcOrdLine)  
  =gfOpenFile(gcDataDir+"Ordline","Ordlinst", 'SH', @lcOrdLine, .T.)
ENDIF  

*-- Open the Credit  or Arhist file to get from it Invoice TotCr. which is the Payment .

IF !USED(lcCredit)
  =gfOpenFile(gcDataDir+"Credit","Credit", 'SH', @lcCredit, .T.)
ENDIF  

IF !USED(lcArhist)
  =gfOpenFile(gcDataDir+"Arhist","Arhistt", 'SH', @lcArhist, .T.)
ENDIF  

*-- Open the Credit  or Arhist [END]
SELECT Invhdr
SET RELATION TO Invhdr.account INTO &lcArhist ADDITIVE
SET RELATION TO Invhdr.account INTO &lcCredit ADDITIVE

SELECT InvLine
SET RELATION TO 'O'+order+store+style+STR(lineno,6) INTO &lcOrdLine ADDITIVE

SELECT (lcAlias)
*!*************************************************************
*! Name        : lfInvNote
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 12/08/1999
*! Purpose     : To seek with the invoice in the notepad.
*!*************************************************************
*! Called from : ARPINVRG.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfInvNote()
*!*************************************************************

FUNCTION lfInvNote
PARAMETERS lcDumdy
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT INVHDR
=SEEK('C' + InvHdr.Invoice,'Notepad') 
lcInvNote1 = ALLTRIM(NOTEPAD.mNotes)

SELECT (lcAlias)
RETURN !EMPTY(lcInvNote1)

*!*************************************************************
*! Name        : lfOrdNote
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 12/08/1999
*! Purpose     : To seek with the order in the notepad.
*!*************************************************************
*! Called from : ARPINVRG.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOrdNote()
*!*************************************************************
FUNCTION lfOrdNote
PARAMETERS lcDumdy
PRIVATE lcAlias , lcOrder
lcAlias    = ALIAS()

SELECT (lcOrdLine)
lcOrder    = ORDER()
SET ORDER TO TAG Ordline
=SEEK('O'+ INVLINE.Order + STR(INVLINE.lineno,6) , lcOrdLine )
lcInvNote2= ALLTRIM(SUBSTR(&lcOrdLine..Note_mem,1,35))

SET ORDER TO &lcOrder
SELECT (lcAlias)
RETURN !EMPTY(lcInvNote2)

*!*************************************************************
*! Name      : lfvRgMsg
*! Developer : Bassem Rafaat (BWA)
*! Date      : 12/08/1999
*! Purpose   : Function to get Optional Message from the User
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRgMsg()
*!*************************************************************
FUNCTION lfvRgMsg
PARAMETER lcReturn
PRIVATE laRgMsg

DECLARE laRgMsg[4,2]       && Array to hold the name and length of the variables to be used in the message screen

laRgMsg[1,1] = 'lcRgMes1'        && 1st.   line Variable
laRgMsg[2,1] = 'lcRgMes2'        && 2nd.   line Variable
laRgMsg[3,1] = 'lcRgMes3'        && 3rd.   line Variable
laRgMsg[4,1] = 'lcRgMes4'        && Forth. line Variable
laRgMsg[1,2] = 30                && Line length

IF FILE(gcDataDir+lcTypeRg+'.MEM')
  RESTORE FROM gcDataDir+lcTypeRg+'.MEM' ADDITIVE
ENDIF

lcReturn= gfOptMsg('laRgMsg','Optional message')    && Call Function to write message
   
SET MEMOWIDTH TO 75
SAVE TO gcDataDir+lcTypeRg+'.MEM' ALL LIKE lcRgMes*
    
RETURN lcReturn
*!*************************************************************
*! Name        : lfInvCred
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 12/08/1999
*! Purpose     : To seek with the invoice in the credit or arhist
*!*************************************************************
*! Called from : ARPINVRG.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvCred()
*!*************************************************************
FUNCTION lfInvCred
PARAMETERS lcDumdy
PRIVATE lcAlias , lncountcr , lcSet

lcAlias   = ALIAS()
lncountcr = 0
lnMisc    = 0
lcSet     = SET('EXACT')

STORE '' TO laCredit
SELECT (lcCredit)

=SEEK(InvHdr.Account , lcCredit )
SCAN WHILE &lcCredit..Account = InvHdr.Account
  SCAN WHILE LEFT(&lcCredit..Reference,6) =  InvHdr.ORDER 
         
    IF ASCAN(laCredit,&lcCredit..Tran) = 0   .AND. lncountcr < 5
      lncountcr = lncountcr + 1
      laCredit[lncountcr,1]  = ALLTRIM(&lcCredit..Desc) + ' ' + ALLTRIM(STR(&lcCredit..Amount,10,2)) + SPACE(2) + IIF("*" $ &lcCredit..Reference , "CC #" , "CHK #") + ' ' +;
                               ALLTRIM(IIF(SUBSTR(&lcCredit..Reference,7,1) = "*" ,SUBSTR(&lcCredit..Reference,8,20), SUBSTR(&lcCredit..Reference,7,20))) + ' ' + ; 
                               DTOC(&lcCredit..TranDate)
                                
    ENDIF 
    lnMisc = lnMisc + &lcCredit..Amount
  ENDSCAN
ENDSCAN 

*SEEK IN THE ARHIST FILE
SET EXACT ON
lncountcr = ASCAN(laCredit,' ')
SET EXACT &lcSet

IF lncountcr > 0
  SELECT (lcArhist)
  =SEEK(InvHdr.Account , lcArhist )
  SCAN WHILE &lcCredit..Account = InvHdr.Account
    SCAN WHILE LEFT(&lcArhist..Reference,6) =  InvHdr.ORDER 
       
      IF ASCAN(laCredit,&lcArhist..Tran) = 0   .AND. lncountcr < 5
        lncountcr = lncountcr + 1
        laCredit[lncountcr,1]  = ALLTRIM(&lcArhist..Desc) + ' ' + ALLTRIM(STR(&lcArhist..Amount,10,2)) + SPACE(2) + IIF("*" $ &lcArhist..Reference , "CC #" , "CHK #") + ' ' + ;
                                 ALLTRIM(IIF(SUBSTR(&lcArhist..Reference,7,1) = "*" , SUBSTR(&lcArhist..Reference,8,20) , SUBSTR(&lcArhist..Reference,7,20))) + ' ' + ;
                                 DTOC(&lcArhist..TranDate)
      ENDIF 
      lnMisc = lnMisc + &lcArhist..Amount
    ENDSCAN
  ENDSCAN  
ENDIF 

SELECT (lcAlias)
RETURN lcDumdy
