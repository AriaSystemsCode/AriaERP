*!******************************************************************************
*! Program file  : ARTRI30   (C # 102376)
*! Program desc. : A Comma DELIMITED for TRIO  
*! System        : Aria Advantage Series.
*! Module        : Sales Order (AR)
*! Developer     : Ahmed Mohmaed ElAnwar (AME)
*! Date          : 06/24/2001
*! Reference     : C102376
*!******************************************************************************
*! Notes : Also refer to C101512. (Aria26)
*!******************************************************************************
*
MEMFILE = gcDataDir+'ARTRI300'
lcPath = ''
DO WHILE .T.
  lcExpr = gfOpGrid('ARTRI30' , .T.)
  IF TYPE('lcExpr')= 'L' .AND. lcExpr = .F.
    RETURN
  ENDIF
  llError = .F.
  lcOldErr = ON('ERROR')
  ON ERROR llError = .T.
  lcOldDef = FULLPATH('')
  IF OCCURS('\',lcPath) > 0
    lcPathNam = ALLTRIM(SUBSTR(lcPath,1,ATC('\',lcPath,;
                                       OCCURS('\',lcPath))-1))
  ELSE
    LOOP
  ENDIF
  SET DEFAULT TO (lcPathNam)
  ON ERROR &lcOldErr
  SET DEFAULT TO (lcOldDef)
  IF llError .OR.  EMPTY(lcPath)
    IF  gfDialog('!','Invalid Path!','\<Reenter;\!\?\<Cancel')=2
      RETURN
    ELSE
      LOOP
    ENDIF  
  ENDIF
  IF EMPTY(ALLTRIM(SUBSTR(lcpath,ATC('\',lcPath,;
                                         OCCURS('\',lcpath))+1,LEN(lcPath))))
    IF  gfDialog('?','No output file name was entered!!','\<Proceed;\!\?\<Cancel')=2
      RETURN
    ELSE
      LOOP
    ENDIF  
  ENDIF
  EXIT
ENDDO
SAVE ALL LIKE LCPATH TO &MEMFILE
IF FILE("&MEMFILE..MEM")
  RESTORE ADDITIVE FROM &MEMFILE
ENDIF
lcOutFile = ALLTRIM(lcPath)
IF FILE(lcOutFile) 
  lnOutFile = 1
  IF  gfDialog('?','Output file '+lcOutFile+' already exists. Overwrite it?','\<Proceed;\!\?\<Cancel')=2
    RETURN
  ENDIF  
ENDIF
lnOutFile = FCREATE(lcOutFile)
IF lnOutFile < 0
  =gfDialog('I','Can not open output file '+lcOutFile+'. Cannot proceed.')
  RETURN
ENDIF

*-- Opening the necessary DataBase file.
=gfOpenFile(GCDataDir+'CUSTOMER','CUSTOMER')
SELECT CUSTOMER
=SEEK('S')
SCAN WHILE TYPE+ACCOUNT+STORE = 'S' FOR &lcExpr 
  WAIT 'Account/Store: '+Account+'/'+ALLTRIM(Store) WINDOW NOWAIT
  lcAccCode  = IIF(LEN(ALLTRIM(STORE))<8,ACCOUNT+LEFT(STORE,7),;
                 SUBSTR(ACCOUNT,2)+STORE)
  lcWriteStr = lcAccCode+StName+PADR(Buyer,30)+cAddress1+LEFT(cAddress2,20)+;
               PADR(LEFT(cAddress3,15),20)+LEFT(cAddress4,2)+LEFT(cAddress5,5)+'0C'
  =FPUTS(lnOutFile,lcWriteStr)               
ENDSCAN

=FSEEK(lnOutFile,0)

IF FEOF(lnOutFile)
  IF !('.T.' $ lcExpr)
    =gfDialog("I","No Stores found for the selected customer.")
  ELSE
    =gfDialog('I','No records to update the text file')
  ENDIF
ELSE
  =gfDialog('I','Output file '+lcOutFile+' has been created.')
ENDIF
=FCLOSE(lnOutFile)
=gfClsData()


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Ahmed Moahmed Elanwer (AME)
*! Date      : 06/24/2001
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvAccount()
*!*************************************************************
*
FUNCTION lfvAccount
PRIVATE lcObjVal , lnAlsNo , lcCustOrd , lcObjName
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

lnAlsNo = SELECT(0)
  
SELECT CUSTOMER
lcCustOrd = ORDER()
SET ORDER TO TAG CUSTOMER
  
lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field  
*IF The user want to Browse or if the Account he entered is not in the file

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , '')
  &lcObjName = lcObjVal
ENDIF    && End of IF
  
SET ORDER TO &lcCustOrd
SELECT(lnAlsNo)
*-- end of lfvAcct.

*!*************************************************************
*! Name      : lfPath  (C# 102376)
*! Developer : AHMED MOHAMED ELANWAR (AME)
*! Date      : 06/24/2001
*! Purpose   : Default Value of Path .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPath()
*!*************************************************************
FUNCTION lfPath
lcPath = ''
IF FILE("&MEMFILE..MEM")
  RESTORE ADDITIVE FROM &MEMFILE
ENDIF
RETURN lcPath
