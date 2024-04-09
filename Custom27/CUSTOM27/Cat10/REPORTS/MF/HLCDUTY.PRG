*:****************************************************************
*: Program file  : HLCDUTY.PRG
*: Program desc. : Custom Duty Sheet.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/20/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvInvcNo
*:               -----------------------------------------------
*:               : PROCEDURE  : lfFillCode , lfFillOne
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcTemp    :- Variable Hold the temp file that collect the data.
*-- lcRpInvce :- Variable Hold invoice number.
*-- lcRpVenVal:- Variable Hold the Vendor.
*-- End Declaration variables.

*- Message Text   :- You must enter a invoice number for the report to be displayed.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF EMPTY(ALLTRIM(lcRpInvce) )
  = gfModalGen('INM00000B00000','F','ALERT',' ','You must enter an invoice number for the report to be displayed.')
  RETURN
ENDIF


*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh

  SELECT Hlcinvhd.ccinvno,Hlcinvhd.dcinvdate  ,;
         Hlcinvln.cstynum,Hlcinvhd.ctsuvencd  ,;
         Hlcinvln.ncindozen,Hlcinvln.ncinunits,;
         Hlcinvhd.ncintunit,Hlcinvhd.ncintdoz ,;
         Hlcstyle.nstytotus,Hlcstyle.nstytduti,;
         Hlcstyle.nstytot,Hlcstyle.ctsusufix  ,;
         Hlcstyle.ntsudtrat,Hlcstyle.ctsugarcd,;
         Hlcinvln.ccvllinno                    ;
  FROM &gcDataDir.Hlcinvhd,&gcDataDir.Hlcinvln,;
       &gcDataDir.Hlcstyle                     ;
  WHERE Hlcinvln.ctsuvencd = Hlcinvhd.ctsuvencd;
       AND Hlcinvln.ccinvno = Hlcinvhd.ccinvno ;
       AND Hlcstyle.cstynum = Hlcinvln.cstynum ;
       AND Hlcinvhd.ctsuvencd = lcRpVenVal     ;
       AND Hlcinvhd.ccinvno = lcRpInvce        ;
       ORDER BY Hlcinvln.ccvllinno             ;
       INTO DBF &gcWorkDir.&lcTemp
ENDIF

SELECT (lcTemp)
LOCATE
DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen


*-- lcRpSewVal :- hold the def. so the filter can work in default case
lcRpVenVal = PADR(laVenVal,6)
lcOldVal   = lcRpVenVal


*-- End OF lfwRepWhen.
*:*************************************************************
*: Name      : lfFillCode
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfFillCode ()
*:*************************************************************
*
PROCEDURE lfFillCode
PRIVATE lcCodeOrd , lnCount

lcCodeOrd = ORDER("CODES")

SET ORDER TO 0 IN CODES
DIMENSION laVenVal[1] , laVenCode[1]

= lfFillOne("laVenCode" ,"laVenVal","CTSUVENCD "   )


*-- End OF lfFillCode
*:*************************************************************
*: Name      : lfFillOne
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : Fill one array from codes file
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters  : lcShowArry  : Array that appears in popup
*:                      lcValArray  : Return values array
*:                      lcCodeVal   : Filter Codes table on this Code 
*:*************************************************************
*: Returns            : Position
*:*************************************************************
*: Example   : = lfFillOne("laVenCode","laVenVal","CTSUVENCD ")
*:*************************************************************
*:
PROCEDURE lfFillOne
PARAMETERS lcShowArry , lcValArray , lcCodeVal
DECLARE &lcShowArry.[1,1]

SELECT CCODE_NO+"-"+CDISCREP FROM CODES ;
  WHERE cDefCode + cRltField + cFld_Name = "NN" + lcCodeVal ;
  ORDER BY CCODE_NO ;
  INTO ARRAY &lcShowArry.

IF _TALLY > 0
  DECLARE &lcValArray.[_TALLY,1]
  lnCount = 0
  FOR lnCount = 1 TO _TALLY
    &lcValArray.[lnCount] = PADL(&lcShowArry.[lnCount],6) 
  ENDFOR
ENDIF

*-- End of lfFillOne.
*:*************************************************************
*: Name      : lfvInvcNo
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   :Browse The invoices for the selected vendor,
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvInvcNo ()
*:*************************************************************
*
FUNCTION lfvInvcNo
PRIVATE lnAlias
lnAlias = SELECT (0)

IF !EMPTY(lcRpInvce) .AND. !EMPTY(lcRpVenVal) .AND. !SEEK(lcRpVenVal+PADR(lcRpInvce,12),'HLCINVHD')
  SELECT HLCINVHD
  DECLARE  laBrowArr[1]
  laBrowArr = ''
  lcBrFields = [lcTsuvencd=gfCodDes(CTSUVENCD,'CTSUVENCD'):H='Vendor',cinvtype:H='Invoice Type', ccinvno:H='Invoice Number' , dcinvdate:H='Invoice Date',]+;
               [ccinvesel:H='Vessel' , ccinconno:H='Container Number',ccinterms:H='Terms',ncinpackg:H='Packages',]+;
               [ccinshipv:H='Shipped Via' , ncintunit:H='Total Units' , ncintdoz:H='Total Dozens' , ncintval:H='Total Value',]+;
               [namountp:H='Amount Paid']
  
  lcRpInvce   = IIF(ARIABROW('FOR cTsuvencd = lcRpVenVal ',"CommercialInvoice Header",gnBrFSRow1, gnBrFSCol1,;
                    gnBrFSRow2, gnBrFSCol2,'','','cTsuvencd','laBrowArr'),;
                    HLCINVHD.ccInvNo,SPACE(12))


ENDIF

SELECT(lnAlias)

*-- End OF lfvInvcNo
*:*************************************************************