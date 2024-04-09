*:****************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

FUNCTION lfSelctAcc
  lcAccount = lcSelctAcc 
RETURN

FUNCTION lfAccBrows
lcKey = ['M']
llSycInt = .F.
IF !USED('SycInt')
  USE (oAriaApplication.SysPath+'SycInt') AGAIN ALIAS 'SYCINT' IN 0 ORDER TAG 'CCONTCODE' SHARED
  llSycInt = .T.
ENDIF
=SEEK(ALLTRIM(oAriaApplication.DefaultCountry),'SycInt')
lcBrFields = "Account :H='Acct#', BtName :H='Name':R,"+;
             "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
             "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
             "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
             "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
             "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
             "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
             "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
             "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
             "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
             "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
             "Phone1 :P= '@RXXX-XXX-XXXX/XXXXX' :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
IF gfBrowse(lcBrFields,'Customers',"Customer",lcKey,.F.,'FFTF',.T.)
  SELECT Customer
  lcSelctAcc = customer.Account
ELSE
  lcSelctAcc = ''
ENDIF
IF llSycInt
  USE IN 'SycInt'
ENDIF
RETURN
