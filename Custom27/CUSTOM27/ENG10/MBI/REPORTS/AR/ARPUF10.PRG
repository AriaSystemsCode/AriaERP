*:**************************************************************************
*: Program file  : ARPUF10
*: Program desc. : Export to ASCII File report
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 06/25/2002
*: Reference     : C200371,1
*:**************************************************************************
*: Calls : 
*:    Functions  : gfModalGen, lfwRepWhen, lfClearRep, lfItmPos, lfPrevRun, 
*:                 lfInvSet, lfSSet, lfvSelcBy
*:    Procedures : lpGetPos, lpCreatFil, lpAddHeadr, lpAddBlAdr, lpAddLines,  
*:                 lpAddFoter
*:**************************************************************************
*: Example : DO ARPUF10
*:**************************************************************************
*B606261,1 SSE 07/14/2002 Fix bug of adding some changes to the report.
*B606470,1 SSE 09/17/2002 Fix bug of not including Voided transaction when
*B606470,1                Selecting by Range in the InList. 

IF EMPTY(lcAcctRef)
  *-- Message : < HSBC Account Ref. is empty, Cannot proceed. >
  *-- Buttons : <             OK               >
  =gfModalGen("TRM000000B00000","DIALOG",'','','HSBC Account Ref. is empty, Cannot proceed.')
  RETURN
ENDIF

PRIVATE lcSetCent , lcSetDate
lcSetCent = SET('CENTURY')
SET CENTURY ON
lcSetDate = SET('DATE')
SET DATE TO BRITISH

*-- Creating the text file which will be used for printing 
lcOutput = PUTFILE("Type a file name","","")
IF EMPTY(lcOutput)
  *-- Message : < Operation has been cancelled >
  *-- Buttons : <             OK               >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
  SET CENTURY &lcSetCent     && Restore old Century settings (On/Off)
  SET DATE TO &lcSetDate     && Restore old date type (British/American...etc)
  RETURN
ENDIF
  
lnHandle = FCREATE(lcOutput)
IF lnHandle < 0  
  *-- Message : < Cannot open output file. >
  *-- Buttons : <           OK             >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open output file.')
  SET CENTURY &lcSetCent     && Restore old Century settings (On/Off)
  SET DATE TO &lcSetDate     && Restore old date type (British/American...etc)
  RETURN
ENDIF

IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
  DO lpCreatFil
ENDIF
  
SELECT InvHdr
SET RELATION TO Invoice INTO Invline ADDITIVE
SET RELATION TO 'M' + Account INTO Customer ADDITIVE
  
SELECT RetHdr
SET RELATION TO CrMemo INTO Retline ADDITIVE
SET RELATION TO 'M' + Account INTO Customer ADDITIVE
  
llLinExist = .F.
PRIVATE lcInvHdExp , lcRetHdExp
IF lcRpSelcBy = "D"   && Transaction Date.
  
  IF lcRpInvCrd $ 'BI'
    lcInvHdExp = SUBSTR(lcRpExp,1,IIF(AT("  AND  ",lcRpExp)>0,AT("  AND  ",lcRpExp),LEN(lcRpExp)))
    lcInvHdExp = lcInvHdExp + IIF(EMPTY(lcInvHdExp),"",[ AND ]) + [Status <> "V" AND !EMPTY(cFacCode)]
    
    SELECT InvHdr
    SCAN FOR &lcInvHdExp
      IF !llLinExist
        llLinExist = .T.
      ENDIF
      WAIT WINDOW 'Printing to ASCII file, Invoice # ' + Invoice NOWAIT      
      m.cType    = "I"
      m.cAccount = Account
      m.cBtName  = Customer.BtName
      m.dDate    = InvDate
      m.cNumber  = Invoice
      m.nAmount  = TotalChg
      INSERT INTO (lcWorkFile) FROM MEMVAR
      
      DO lpAddHeadr WITH 'I'
      DO lpAddBlAdr WITH 'I'
      DO lpAddLines WITH 'I'
      DO lpAddFoter WITH 'I'
    ENDSCAN
    WAIT CLEAR
  ENDIF
  
  IF lcRpInvCrd $ 'BC'  
    
    *B606261,1 Fix bug of displaying all records when selecting date. [Begin]
    *lcRetHdExp = SUBSTR(lcRpExp,IIF(AT("  AND  ",lcRpExp)>0,AT("  AND  ",lcRpExp)+5,0))
    *lcRetHdExp = SUBSTR(lcRpExp,IIF(AT("  AND  ",lcRpExp)>0,AT("  AND  ",lcRpExp)+5,LEN(lcRpExp)))
    lcRetHdExp = ''
    IF !(lcRpExp == ".T.") AND "RETHDR.CRDATE" $ lcRpExp
      lcRetHdExp = SUBSTR(lcRpExp,IIF(AT("  AND  ",lcRpExp)>0,AT("  AND  ",lcRpExp)+5,LEN(lcRpExp)))
    ENDIF
    *B606261,1 Fix bug of displaying all records when selecting date. [End]
    
    lcRetHdExp = lcRetHdExp + IIF(EMPTY(lcRetHdExp),"",[ AND ]) + [Status <> "V" AND !EMPTY(cFacCode)]

    SELECT RetHdr
    SCAN FOR &lcRetHdExp
      IF !llLinExist
        llLinExist = .T.
      ENDIF
      WAIT WINDOW 'Printing to ASCII file, CrMemo # ' + CrMemo NOWAIT
      m.cType    = "R"
      m.cAccount = Account
      m.cBtName  = Customer.BtName
      m.dDate    = CrDate
      m.cNumber  = CrMemo
      m.nAmount  = TotCredit
      INSERT INTO (lcWorkFile) FROM MEMVAR

      DO lpAddHeadr WITH 'R'
      DO lpAddBlAdr WITH 'R'
      DO lpAddLines WITH 'R'
      DO lpAddFoter WITH 'R'
    ENDSCAN
    WAIT CLEAR
  ENDIF  
    
ELSE                  && Transaction Number.
  IF lcRpInvCrd $ 'BI'
    PRIVATE lcInvHdr , lcInvoicNo
    lcInvHdExp = IIF(USED(laOGFxFlt[lnInvHdPos,6]) AND RECCOUNT(laOGFxFlt[lnInvHdPos,6])>0 , ;
                 ".T.",[Status <> "V" AND !EMPTY(cFacCode)])
    lcInvHdr = IIF(USED(laOGFxFlt[lnInvHdPos,6]) AND RECCOUNT(laOGFxFlt[lnInvHdPos,6])>0 , ;
               laOGFxFlt[lnInvHdPos,6],"InvHdr")     

    SELECT (lcInvHdr)
    SCAN FOR &lcInvHdExp
      IF !llLinExist
        llLinExist = .T.
      ENDIF  
      WAIT WINDOW 'Printing to ASCII file, Invoice # ' + Invoice NOWAIT    
      lcInvoicNo = Invoice
      
      *B606470,1 Include this expression if user select by Range in the Inlist. [Begin]
      *IF SEEK(lcInvoicNo,'InvHdr')
      IF SEEK(lcInvoicNo,'InvHdr') AND InvHdr.Status <> "V" AND !EMPTY(InvHdr.cFacCode)
      *B606470,1 Include this expression if user select by Range in the Inlist. [End]
      
        SELECT InvHdr
        m.cType    = "I"
        m.cAccount = Account
        m.cBtName  = Customer.BtName
        m.dDate    = InvDate
        m.cNumber  = Invoice
        m.nAmount  = TotalChg
        INSERT INTO (lcWorkFile) FROM MEMVAR

        DO lpAddHeadr WITH 'I'
        DO lpAddBlAdr WITH 'I'
        DO lpAddLines WITH 'I'
        DO lpAddFoter WITH 'I'
      ENDIF  
    ENDSCAN
    WAIT CLEAR
  ENDIF  
  
  IF lcRpInvCrd $ 'BC'
    PRIVATE lcRetHdr , lcCrMemoNo
    lcRetHdExp = IIF(USED(laOGFxFlt[lnRetHdPos,6]) AND RECCOUNT(laOGFxFlt[lnRetHdPos,6])>0 , ;
                 ".T.",[Status <> "V" AND !EMPTY(cFacCode)])
    lcRetHdr = IIF(USED(laOGFxFlt[lnRetHdPos,6]) AND RECCOUNT(laOGFxFlt[lnRetHdPos,6])>0 , ;
               laOGFxFlt[lnRetHdPos,6],"RetHdr")     
    
    SELECT (lcRetHdr)    
    SCAN FOR &lcRetHdExp
      IF !llLinExist
        llLinExist = .T.
      ENDIF  
      WAIT WINDOW 'Printing to ASCII file, CrMemo # ' + CrMemo NOWAIT
      lcCrMemoNo = CrMemo
      
      *B606470,1 Include this expression if user select by Range in the Inlist. [Begin]
      *IF SEEK(lcCrMemoNo,'RetHdr')
      IF SEEK(lcCrMemoNo,'RetHdr') AND RetHdr.Status <> "V" AND !EMPTY(RetHdr.cFacCode)
      *B606470,1 Include this expression if user select by Range in the Inlist. [End]
      
        SELECT RetHdr
        m.cType    = "R"
        m.cAccount = Account
        m.cBtName  = Customer.BtName
        m.dDate    = CrDate
        m.cNumber  = CrMemo
        m.nAmount  = TotCredit
        INSERT INTO (lcWorkFile) FROM MEMVAR

        DO lpAddHeadr WITH 'R'
        DO lpAddBlAdr WITH 'R'
        DO lpAddLines WITH 'R'
        DO lpAddFoter WITH 'R'
      ENDIF  
    ENDSCAN
    WAIT CLEAR
  ENDIF  
    
ENDIF

SELECT InvHdr
SET RELATION TO      && Break InvHdr relation 

SELECT RetHdr
SET RELATION TO      && Break RetHdr relation 

=FCLOSE(lnHandle)
IF llLinExist
  *-- Ask about printing.
  IF llRpPrint
    SELECT (lcWorkFile)
    gcDevice = "PRINTER"
    DO gfDispRe WITH EVAL('lcRpName')
    PRIVATE lnCancel
    lnCancel = 1
    DO WHILE lnCancel = 1      
      *-- Message : < Do you want to print again >
      *-- Buttons : <     YES          NO        >
      IF gfModalGen('TRM00000B40000','DIALOG','','','Do you want to print again') = 1
        *-- Print here
        DO gfDispRe WITH EVAL('lcRpName')
        LOOP
      ELSE
        lnCancel = 2
      ENDIF
    ENDDO
    gcDevice = "SCREEN"
  ENDIF
ELSE  
  *-- Message : < There are no records to display >
  *-- Buttons : <               OK                >  
  = gfModalGen('TRM00052B00000','DIALOG')
ENDIF
*-- Checking if Text File is Empty [End]

SET CENTURY &lcSetCent     && Restore old Century settings (On/Off)
SET DATE TO &lcSetDate     && Restore old date type (British/American...etc)
*-- End of report 

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : OG when function
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
SHOW GET pbOGPrevie DISABLE

*-- This variable is not used anymore with also its line in Option grid.
lnTaxRate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)

lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
lcPassword = gfGetMemVar('M_PASSWORD',gcAct_Comp)
lcAcctRef = gfGetMemVar('M_ACCT_REF',gcAct_Comp)

PRIVATE llSycComp
llSycComp = gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_ID','SH')
SELECT SycComp
SEEK gcAct_Comp
STORE '' TO lcCompName , lcCompAdd1 , lcCompAdd2 , lcCompAdd5
lcCompName = cCom_Name             && Variable to hold the Company Name
lcCompAdd1 = cAddress1
lcCompAdd2 = cAddress2
lcCompAdd5 = cAddress5

IF llSycComp
  USE IN SycComp
ENDIF

DO lpGetPos
DO lpCreatFil
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : Create work Cursor.
*!**************************************************************************
*! Called from : Report code.
*!**************************************************************************
*! Example   : DO lpCreatFil
*!**************************************************************************
*
PROCEDURE lpCreatFil

CREATE CURSOR (lcWorkFile) ;
(cType C(1) , cAccount C(5) , cBtName C(30) , dDate D(8) , cNumber C(6) , ;
 nAmount N(13,2))

SELECT (lcWorkFile)
ZAP

INDEX ON cType TAG (lcWorkFile)
*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lpGetPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Example   : DO lpGetPos
*!**************************************************************************
*
PROCEDURE lpGetPos
IF lcRpSelcBy = "D"   && Transaction Date.
  lnInvHdPos = lfItmPos('INVHDR.INVDATE')
  lnRetHdPos = lfItmPos('RETHDR.CRDATE')
ELSE                  && Transaction Number. 
  lnInvHdPos = lfItmPos('INVHDR.INVOICE')
  lnRetHdPos = lfItmPos('RETHDR.CRMEMO')
ENDIF
*-- End of lpGetPos

*!**************************************************************************
*! Name      : lpAddHeadr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : To print Inv./Crd. headers to the output file.
*!**************************************************************************
*! Example   : DO lpAddHeadr
*!**************************************************************************
*
PROCEDURE lpAddHeadr
PARAMETER lcType
PRIVATE lcAlias
lcAlias = ALIAS()

=FWRITE(lnHandle,"|PASSWRD| STGU")
=FWRITE(lnHandle,lcPassword)
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|INV TYP| ")
=FWRITE(lnHandle,"FREE")
=FWRITE(lnHandle,CHR(13)+CHR(10))
      
=FWRITE(lnHandle,"|CLI NAM| ")
=FWRITE(lnHandle,lcCompName)
=FWRITE(lnHandle,CHR(13)+CHR(10))
      
=FWRITE(lnHandle,"|ADD LN1| ")
=FWRITE(lnHandle,lcCompAdd1)
=FWRITE(lnHandle,CHR(13)+CHR(10))      

=FWRITE(lnHandle,"|ADD LN2| ")
=FWRITE(lnHandle,lcCompAdd2)
=FWRITE(lnHandle,CHR(13)+CHR(10))      

=FWRITE(lnHandle,"|POSTCOD| ")
=FWRITE(lnHandle,lcCompAdd5)
=FWRITE(lnHandle,CHR(13)+CHR(10))      

=FWRITE(lnHandle,"|VAT NUM| ")
=FWRITE(lnHandle,ALLTRIM(SUBSTR(lcTaxRefr,1,3))+" "+ALLTRIM(SUBSTR(lcTaxRefr,4,4))+" "+ALLTRIM(SUBSTR(lcTaxRefr,8)))
=FWRITE(lnHandle,CHR(13)+CHR(10))      

=FWRITE(lnHandle,"|CLI NUM| ")

*B606261,1 Change to be 4 characters padded with one zero according to Tony's mail. [Begin]
*=FWRITE(lnHandle,PADL(lcAcctRef,9,'0'))
=FWRITE(lnHandle,IIF(LEN(ALLTRIM(lcAcctRef))<9,'0','')+ALLTRIM(lcAcctRef))
*B606261,1 Change to be 4 characters padded with one zero according to Tony's mail. [End]

=FWRITE(lnHandle,CHR(13)+CHR(10))      
=FWRITE(lnHandle,"|T TERMS| ")

*B606261,1 Always in Credit note leave the Term code empty. [Begin]
*=FWRITE(lnHandle,"DAYS " + PADL(ALLTRIM(STR(IIF(lcType="I",DueDate-InvDate,0))),3,'0'))
*IF lcType ="I"    && tony asked to change credit notes to be empty 
*  =FWRITE(lnHandle,"DAYS " + PADL(ALLTRIM(STR(DueDate-InvDate)),3,'0'))
*ENDIF
*-- tony asked to change return credit notes as it is
=FWRITE(lnHandle,"DAYS " + PADL(ALLTRIM(STR(IIF(lcType="I",DueDate-InvDate,0))),3,'0'))
*B606261,1 Always in Credit note leave the Term code empty. [End]

=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|INV/CRD| ")
=FWRITE(lnHandle,IIF(lcType="I","Invoice","Credit"))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|INV NOP| ")
=FWRITE(lnHandle,PADL(IIF(lcType="I",Invoice,CrMemo),8," "))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|INV DAT| ")
=FWRITE(lnHandle,IIF(lcType="I",DTOC(InvDate),DTOC(CrDate)))
=FWRITE(lnHandle,CHR(13)+CHR(10))

SELECT (lcAlias)
*-- End of lpAddHeadr.

*!**************************************************************************
*! Name      : lpAddBlAdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : To print Bill name and addresses to the output file.
*!**************************************************************************
*! Example   : DO lpAddBlAdr
*!**************************************************************************
*
PROCEDURE lpAddBlAdr
PARAMETER lcType
PRIVATE lcAlias
lcAlias = ALIAS()

=FWRITE(lnHandle,"|CUS NAM| ")
=FWRITE(lnHandle,PADR(Customer.BtName,40,' '))
=FWRITE(lnHandle," |" + ALLTRIM(Customer.cHSBC_No) + "|"))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|CUS AD1| ")
=FWRITE(lnHandle,PADR(Customer.cAddress12,35,' '))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|CUS AD2| ")
=FWRITE(lnHandle,PADR(Customer.cAddress22,35,' '))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|CUS AD3| ")
=FWRITE(lnHandle,PADR(Customer.cAddress32,35,' '))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|CUS AD4| ")
=FWRITE(lnHandle,PADR(Customer.cAddress42,35,' '))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|CUS AD5| ")
=FWRITE(lnHandle,PADR(Customer.cAddress52,35,' '))
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|ORD NUM| ")
=FWRITE(lnHandle,Order)
IF lcType = "I"
  =FWRITE(lnHandle," " + SUBSTR(ALLTRIM(CustPo),1,8))
ELSE
  =FWRITE(lnHandle," " + Invoice)
ENDIF  
=FWRITE(lnHandle,CHR(13)+CHR(10))

SELECT (lcAlias)
*-- End of lpAddBlAdr.

*!**************************************************************************
*! Name      : lpAddLines
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : To print Invoice lines to the output file.
*!**************************************************************************
*! Example   : DO lpAddLines
*!**************************************************************************
*
PROCEDURE lpAddLines
PARAMETER lcType
PRIVATE lcAlias
lcAlias = ALIAS()

=FWRITE(lnHandle,"|ILD STR| ")
=FWRITE(lnHandle,CHR(13)+CHR(10))
IF lcType="I"
  SELECT InvLine
  SCAN REST WHILE Invoice + STR(LineNo,6) = InvHdr.Invoice 
    =FWRITE(lnHandle,Desc1)
    
    *B606261,1 Pad all numbers with spaces. [Begin]
    *=FWRITE(lnHandle," " + STRTRAN(STR(Price*TotQty,14,2),' ','0') + " ")
    =FWRITE(lnHandle," " + STR(Price*TotQty,14,2) + " ")
    *B606261,1 Pad all numbers with spaces. [End]

    *B606261,1 Pad all numbers with spaces. [Begin]
    *=FWRITE(lnHandle,STRTRAN(STR(IIF(InvHdr.Tax_Rate<=0,InvHdr.Tax_Rate,Customer.nTaxRate),6,2),' ','0')+ " ")
    *=FWRITE(lnHandle,STRTRAN(STR(IIF(InvHdr.Tax_Amt>0,IIF(InvHdr.Tax_Rate>0,InvHdr.Tax_Rate,lnTaxRate),0),6,2),' ','0')+ " ")
    *=FWRITE(lnHandle,STRTRAN(STR(IIF(InvHdr.Tax_Amt>0 AND InvHdr.Tax_Rate>0,InvHdr.Tax_Rate,Customer.nTaxRate),6,2),' ','0')+ " ")
    =FWRITE(lnHandle,STR(IIF(InvHdr.Tax_Amt>0 AND InvHdr.Tax_Rate>0,InvHdr.Tax_Rate,Customer.nTaxRate),6,2)+ " ")
    *B606261,1 Pad all numbers with spaces. [End]

    *B606261,1 Pad all numbers with spaces. [Begin]
    *=FWRITE(lnHandle,STRTRAN(STR(Gros_Price,8,2),' ','0'))
    =FWRITE(lnHandle,STR(Gros_Price,8,2))
    *B606261,1 Pad all numbers with spaces. [End]

    =FWRITE(lnHandle," T1")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
  ENDSCAN
ELSE
  SELECT RetLine
  SCAN REST WHILE CrMemo + Style + cRet_LinNo + cRet_TrnCD = RetHdr.CrMemo
    =FWRITE(lnHandle,IIF(SEEK(Style,'Style'),Style.Desc1,''))

    *B606261,1 Pad all numbers with spaces. [Begin]
    *=FWRITE(lnHandle," " + STRTRAN(STR(Gros_Price*TotQty,14,2),' ','0') + " ")
    =FWRITE(lnHandle," " + STR(Gros_Price*TotQty,14,2) + " ")
    *B606261,1 Pad all numbers with spaces. [End]

    *IF SEEK(RetHdr.Invoice,'InvHdr')
    *  *Modify the line as per Tony request.
    *  *=FWRITE(lnHandle,STRTRAN(STR(IIF(InvHdr.Tax_Rate<=0,InvHdr.Tax_Rate,Customer.nTaxRate),6,2),' ','0')+ " ")
    *  =FWRITE(lnHandle,STRTRAN(STR(IIF(InvHdr.Tax_Amt>0,IIF(InvHdr.Tax_Rate>0,InvHdr.Tax_Rate,lnTaxRate),0),6,2),' ','0')+ " ")
    *ELSE
    *  =FWRITE(lnHandle,REPLICATE('0',7))
    *ENDIF  

    *B606261,1 Pad all numbers with spaces. [Begin]
    *=FWRITE(lnHandle,STRTRAN(STR(IIF(RetHdr.Tax_Amt>0,Customer.nTaxRate,0),6,2),' ','0')+ " ")
    *=FWRITE(lnHandle,STRTRAN(STR(Gros_Price,8,2),' ','0'))
    =FWRITE(lnHandle,STR(IIF(RetHdr.Tax_Amt>0,Customer.nTaxRate,0),6,2)+ " ")
    =FWRITE(lnHandle,STR(Gros_Price,8,2))
    *B606261,1 Pad all numbers with spaces. [End]
      
    =FWRITE(lnHandle," T1")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
  ENDSCAN
ENDIF
=FWRITE(lnHandle,"|ILD END| ")
=FWRITE(lnHandle,CHR(13)+CHR(10))

SELECT (lcAlias)
*-- End of lpAddLines.

*!**************************************************************************
*! Name      : lpAddFoter
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : To print Invoice lines to the output file.
*!**************************************************************************
*! Example   : DO lpAddFoter
*!**************************************************************************
*
PROCEDURE lpAddFoter
PARAMETER lcType
PRIVATE lcAlias
lcAlias = ALIAS()

=FWRITE(lnHandle,"|ITM NET| ")
IF lcType = "I"
  =FWRITE(lnHandle,ALLTRIM(STR(ShipAmt+Discount,14,2)))
ELSE
  =FWRITE(lnHandle,ALLTRIM(STR(Amount,14,2)))
ENDIF  
=FWRITE(lnHandle,CHR(13)+CHR(10))
      
=FWRITE(lnHandle,"|TOT TAX| ")
IF lcType = "I"
  =FWRITE(lnHandle,ALLTRIM(STR(Tax_Amt,14,2)))
ELSE
  =FWRITE(lnHandle,ALLTRIM(STR(Tax_Amt,14,2)))
ENDIF  
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|CARRIAG| ")
IF lcType = "I"
  =FWRITE(lnHandle,ALLTRIM(STR(nCharges,14,2)))
ELSE
  =FWRITE(lnHandle,ALLTRIM(STR(Other,14,2)))
ENDIF  
=FWRITE(lnHandle,CHR(13)+CHR(10))

=FWRITE(lnHandle,"|INV TOT| ")
IF lcType = "I"
  =FWRITE(lnHandle,ALLTRIM(STR(TotalChg,14,2)))
ELSE
  =FWRITE(lnHandle,ALLTRIM(STR(TotCredit,14,2)))
ENDIF  
=FWRITE(lnHandle,CHR(13)+CHR(10))
SELECT (lcAlias)
*-- End of lpAddFoter.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfPrevRun
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : Function that we call in place of the Functions called when
*!             Pressing <PREVIEW> or <RUN>
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfPrevRun()
*!**************************************************************************
*
FUNCTION lfPrevRun
llOGEscPrsd = .F.
lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
lcOGSelect=SELECT()
=lfInitValue('laOGVrFlt','T')
=lfInitValue('laOGFXFlt','T')
  
lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
  
lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))  
=lfInitValue('laOGVrFlt','C')
=lfInitValue('laOGFXFlt','C')

lcRpExp = lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
          lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
          IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')

lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)
lcogExc=SET('EXACT')        
SET EXACT ON
llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        

SET EXACT &lcOGExc
IF llOGFltCh
  lcOGFltr=lcRpExp
ENDIF
SET CONSOLE OFF

IF !EMPTY(lcOGFormV)
  IF !(lcOGLastForm==&lcOGFormV)
      
    lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      
    lcOGLastForm=&lcOGFormV
    llOGStyleCh=.T.
  ENDIF  

  IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
    =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
  ENDIF

ENDIF  

DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.

SET CONSOLE ON

IF !VARREAD() == "PBRUN")
  gcDevice=lcOGDevice
  gcOutFile=lcOGOutFile
  glHeader = llOGHeader
ENDIF  

llOGFltCh = .F.

RETURN .F.
*-- End of lfPrevRun.

*!**************************************************************************
*! Name      : lfInvSet
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*
FUNCTION lfInvSet
PARAMETERS lcParm
DO CASE 
  CASE lcParm = "S"
    lcInvHdTag = ORDER('INVHDR')
    lcCstmrTag = ORDER('CUSTOMER')
    SET ORDER TO TAG INVHDR IN INVHDR
    SET ORDER TO TAG CUSTOMER IN CUSTOMER
  CASE lcParm = "R"

    *-- If The INVHDR file did not have an active index
    IF EMPTY(lcInvHdTag)
      SET ORDER TO IN INVHDR
    ELSE
      SET ORDER TO TAG (lcInvHdTag) IN INVHDR
    ENDIF

    *-- If The CUSTOMER file did not have an active index 
    IF EMPTY(lcCstmrTag)
      SET ORDER TO IN CUSTOMER
    ELSE
      SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
    ENDIF
ENDCASE
*-- End of lfInvSet.

*!**************************************************************************
*! Name      : lfSSet
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : Set function for in range
*!**************************************************************************
*! Example     : =lfSSet()
*!**************************************************************************
*
FUNCTION lfSSet
PARAMETER lcParam
GO TOP IN RETHDR
*-- End of lfSSet.

*!**************************************************************************
*! Name      : lfvSelcBy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : Validation of the Select by option in OG.
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfvSelcBy()
*!**************************************************************************
*
FUNCTION lfvSelcBy
DO lpGetPos
CLEAR READ
*-- End of lfvSelcBy.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : Clear the Text file before closing the report
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .F.
IF lnHandle > 0
  =FCLOSE(lnHandle)
ENDIF  
*-- Erase the Text File used in Printing.
ERASE (gcWorkDir+lcTextFile+".TXT")
*-- End of lfClearRep.

*!**************************************************************************
*! Name      : lfPrint
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/25/2002
*! Purpose   : Validation of the Print option in OG.
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfPrint()
*!**************************************************************************
*
FUNCTION lfPrint
CLEAR READ
*-- End of lfPrint.
