*:****************************************************************
*: Program file  : SOAPROR.PRG
*: Program desc. : Custom Approved Order(s)
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order (SO)
*: Developer     : Abdou Elgendy - [ABD]
*: Date          : 02/09/2004.
*: Tracking Job Number : C#121045,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  : lfSeTOrdr , lfvAccount , lfwOldVal
*:               :            : lfsrAcct  , lfvClerRed , lfvApprAmt
*:               :            : lfApprOrds, lfvShowApr , 
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#121045,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:

*-- Start Declaration Variabls
STORE "" TO laOldVal , lcRpCuster,lcShowExpr,lcRpAprAmt,;
            lcRpTrnTyp , lcRpAprCd
STORE 0  TO lnRpAprAmt , lnRngAlias
lcShowExpr = "Appramt = 0"
*-- End Declaration Variabls

IF !gfSetup()
  RETURN
ENDIF


DO WHILE .T.
  *-- Cal the Option Grid.
  lcExpr = gfOpGrid("SOAPROR" , .T.)

  DO CASE 
    CASE Type("lcExpr") = "C"
      *- Message Text   :- Are you sure you want to do Global Update for all fabric(s) - style(s).
      *- Message No.    :- 000000.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      lcMessage = "Are you sure you want to run the appoved order(s)."
      IF gfModalGen("INM00000B00006","F","ALERT"," ",lcMessage) = 1
        EXIT
      ENDIF
  CASE Type("lcExpr") = "L" .AND.  lcExpr = .F.
    EXIT
  ENDCASE  
ENDDO

IF  Type("lcExpr") = "L" .AND.  lcExpr = .F.
  RETURN
ENDIF

*-- Function to update the selected orders.
= lfApprOrds ()

*- Message Text   :- Process completed successful.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
lcMessage = "Process completed successful."
= gfModalGen("INM00000B00000","F","ALERT"," ",lcMessage)


RETURN

*-- End OF Code.
*:****************************************************************
*: Name      : lfSeTOrdr
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : Set Relation,Reset Relation, in range browse screen.
*:****************************************************************
*: Calls     : 
*:****************************************************************
*: Called from        : Option Grid && Report DYE DEMOND REPORT
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns            : None
*:****************************************************************
*: Example            : =fSeTOrdr()
*:****************************************************************
*: Note               : symbol is [S,Set- R,ReSet]
*:****************************************************************
*:
FUNCTION lfSeTOrdr
PARAMETERS OpGrdParm
SELECT ORDHDR
DO CASE
  CASE OpGrdParm = "S"    
    lcRelation = [IIF(EMPTY(Store) , "M" + Account,"S" + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER 
    GO TOP
  CASE OpGrdParm = "R"
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER
ENDCASE

*-- End OF lfSetOrdr
*:****************************************************************
*: Name      : lfvAccount
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : Valid function of the Account
*:****************************************************************
*: Called from : Option grid [Account Get field]
*:****************************************************************
*: Calls       : CusBrowM()
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*
FUNCTION lfvAccount

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF "?" $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK("M" + lcObjVal , "CUSTOMER"))
  llObjRet = CusBrowM(@lcObjVal , "" , "M")
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

lcRpCuster = lcObjVal

*-- End Of lfvAccount.
*:****************************************************************
*: Name      : lfwOldVal
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : When function to get the Old value
*:****************************************************************
*: Called from : Some of the Get fields and some of the Option grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*:
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*-- End OF lfwoldVal.
*:****************************************************************
*: Name      : lfsrAcct
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : Account In Range Filter. 2
*:****************************************************************
*: Called from : Some of the Get fields and some of the Option grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*:
FUNCTION lfsrAcct
PARAMETERS lcParm

DO CASE
  CASE lcParm = "S"
    lnRngAlias = SELECT(0)
    GO TOP IN CUSTOMER
  CASE lcParm = "R"
    SELECT (lnRngAlias)
ENDCASE

*-- end of lfsrAcct.
*:****************************************************************
*: Name      : lfvClerRed
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : Clear read the option grid.
*:****************************************************************
*: Called from : Some of the Get fields and some of the Option grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*:
FUNCTION lfvClerRed

DO CASE
  CASE lcRpAprAmt = "M"
    lnRpAprAmt = ABS(lnRpAprAmt)
  CASE lcRpAprAmt = "B"
    lnRpAprAmt = 0
ENDCASE 

CLEAR READ

*-- End Of lfvClerRed.
*:****************************************************************
*: Name      : lfvApprAmt
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : Valid function to valid the approve amount.
*:****************************************************************
*: Called from : Some of the Get fields and some of the Option grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*:
FUNCTION lfvApprAmt

IF lcRpAprAmt = "M"
  lnRpAprAmt = ABS(lnRpAprAmt)
  =lfOGShowGet("lnRpAprAmt")
ENDIF


*-- End Of lfvApprAmt.
*:****************************************************************
*: Name      : lfApprOrds
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : function to approve / Unaprove the selected sales order.
*:****************************************************************
*: Called from : the program.
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*:
FUNCTION lfApprOrds
PRIVATE lnAlias , lcWrkField
lnAlias = SELECT (0)
lcWrkField =""
*-- get the update fied.
DO CASE
  *-- Approve with book amount
  CASE lcRpAprAmt = "B"
    lcWrkField = "Ordhdr.Bookamt"

  *-- Approve with dollar amount from the option grid.
  CASE lcRpAprAmt = "M"
    lcWrkField = "lnRpAprAmt"

  *-- Approve with book amount + the dollar amount from the option grid.
  CASE lcRpAprAmt = "P"
    lcWrkField = "Ordhdr.Bookamt + lnRpAprAmt"
ENDCASE

*-- Case Approve , I shouldn"t reapprove the approved order"s
lcScnExpr = " .AND. Ordhdr.Status $ 'OH' .AND. IIF(lcRpTrnTyp = 'A',Appramt = 0,.T.)"
SELECT ORDHDR 
SCAN FOR &lcExpr &lcScnExpr
  WAIT WINDOW "Update order #:-  " + Order +" ...Pleae Wait" NOWAIT
  REPLACE Appramt  WITH IIF(lcRpTrnTyp = "U",0,&lcWrkField);
          Approval WITH IIF(lcRpTrnTyp = "U","",IIF(lcRpTrnTyp = 'C' .AND. EMPTY(lcRpAprCd) ,Approval,lcRpAprCd))
  IF !(lcRpTrnTyp = "U")
    REPLACE Status   WITH IIF(Status = "H","O",Status)
  ENDIF
ENDSCAN
SELECT(lnAlias)

*-- End OF lfApprOrds
*:****************************************************************
*: Name      : lfvShowApr
*: Developer : Abdou Elgendy [Abd]
*: Date      : 02/09/2004
*: Purpose   : show the approve or not approve orders.
*:****************************************************************
*: Called from : the option grid.
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*:
FUNCTION lfvShowApr

IF lcRpTrnTyp = "A"
  lcShowExpr = "Appramt = 0"
ELSE
  lcShowExpr = "Appramt # 0"
ENDIF

CLEAR READ
*-- End OF lfvShowApr
*:****************************************************************