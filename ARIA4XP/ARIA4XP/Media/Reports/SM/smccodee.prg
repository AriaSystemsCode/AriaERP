*:***************************************************************************
*: Program file  : SMCCODE
*: Program desc. : Codes report
*: For Report    : (SMCCODE.FRX)
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : System Manager (SM)
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*: B609000,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[T20090805.0001]
*: E302857,1 HES 02/10/2011 Avoid 'X:\Aria4xp\SRVRPTS' Fixed Path [T20110206.0017]
*:***************************************************************************
loogscroll.cCROrientation = 'L'

*B609000,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[Start]
IF oAriaApplication.MULTIINST 
  *: E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN 
*!*	  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\SM\SMCCODE.FXP ADDITIVE
*!*	  DO X:\ARIA4XP\SRVRPTS\SM\SMCCODE.FXP 
  SET PROCEDURE TO oAriaApplication.CLIENTSRVREPORTHOME+"SM\SMCCODE.FXP" ADDITIVE
  DO oAriaApplication.CLIENTSRVREPORTHOME+"SM\SMCCODE.FXP"   
  *: E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END      
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"SM\SMCCODE.FXP" WITH .F.,.F.
ENDIF 
*B609000,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[End]
*!*************************************************************
*! Name      : lfFillRelt
*! Developer : Mariam MAzhar
*! Date      : 09/28/2008
*! Purpose   : Fill Related fields array
*!*************************************************************
FUNCTION lfFillRelt

*-- Array to Hold headers of releated fields
DIMENSION laRltFdInf[45,3]
laRltFdInf[1,1]="CADJACCT"
laRltFdInf[1,2]="Adj. Account"
laRltFdInf[1,3]=24
laRltFdInf[2,1]="CLRLNAME"
laRltFdInf[2,2]="Long Name"
laRltFdInf[2,3]=30
laRltFdInf[3,1]="CNRFCODE"
laRltFdInf[3,2]="NRF Code"
laRltFdInf[3,3]=5
laRltFdInf[4,1]="ALLOW_TYPE"
laRltFdInf[4,2]="Allowance Type"
laRltFdInf[4,3]=1
laRltFdInf[5,1]="CBNKCODE"
laRltFdInf[5,2]="Bank Code"
laRltFdInf[5,3]=8
laRltFdInf[6,1]="CCHKACCT"
laRltFdInf[6,2]="Bank Checking Acc."
laRltFdInf[6,3]=12
laRltFdInf[7,1]="DISCPCNT"
laRltFdInf[7,2]="Discount Type"
laRltFdInf[7,3]=6
laRltFdInf[8,1]="START"
laRltFdInf[8,2]="Start Date"
laRltFdInf[8,3]=8
laRltFdInf[9,1]="DENDATE"
laRltFdInf[9,2]="End Date"
laRltFdInf[9,3]=8
laRltFdInf[10,1]="DIVLNAME"
laRltFdInf[10,2]="Division Long Name"
laRltFdInf[10,3]=30
laRltFdInf[11,1]="LINK_CODE"
laRltFdInf[11,2]="GL Link Code"
laRltFdInf[11,3]=6
laRltFdInf[12,1]="CSLSGLLINK"
laRltFdInf[12,2]="GL Sales Link Code"
laRltFdInf[12,3]=3
laRltFdInf[13,1]="DIVGROUP"
laRltFdInf[13,2]="Division Group"
laRltFdInf[13,3]=3
laRltFdInf[14,1]="CUPCMAN"
laRltFdInf[14,2]="U.C.C. Manufacture ID"
laRltFdInf[14,3]=6
laRltFdInf[15,1]="CUPCGENTYP"
laRltFdInf[15,2]="Eropune UPC"
laRltFdInf[15,3]=1
laRltFdInf[16,1]="GLACCOUNT"
laRltFdInf[16,2]="GL Account"
laRltFdInf[16,3]=24
laRltFdInf[17,1]="COPERSEQ"
laRltFdInf[17,2]="Operation Seq."
laRltFdInf[17,3]=2
laRltFdInf[18,1]="LINHOUSE"
laRltFdInf[18,2]="In House (Y/N)"
laRltFdInf[18,3]=1
laRltFdInf[19,1]="CCONTCODE"
laRltFdInf[19,2]="Contractore/Department"
laRltFdInf[19,3]=8
laRltFdInf[20,1]="CCONTNAME"
laRltFdInf[20,2]="Contractore Name"
laRltFdInf[20,3]=30
laRltFdInf[21,1]="LMFGOPR"
laRltFdInf[21,2]="Consider As Operation(Y/N)"
laRltFdInf[21,3]=1
laRltFdInf[22,1]="LEADTIME"
laRltFdInf[22,2]="Lead Time"
laRltFdInf[22,3]=3
laRltFdInf[23,1]="CFRGTACNT"
laRltFdInf[23,2]="GL Freight Account"
laRltFdInf[23,3]=24
laRltFdInf[24,1]="CTAXCODE"
laRltFdInf[24,2]="Tax Code"
laRltFdInf[24,3]=6
laRltFdInf[25,1]="CARGLACC"
laRltFdInf[25,2]="AR/Non AR Account"
laRltFdInf[25,3]=24
laRltFdInf[26,1]="NTERDUED"
laRltFdInf[26,2]="Net Due Days"
laRltFdInf[26,3]=3
laRltFdInf[27,1]="NTERDISCD"
laRltFdInf[27,2]="Discount Days"
laRltFdInf[27,3]=3
laRltFdInf[28,1]="NTERDISCR"
laRltFdInf[28,2]="Discount Percent"
laRltFdInf[28,3]=6
laRltFdInf[29,1]="EOM"
laRltFdInf[29,2]="E.O.M (Y/N)"
laRltFdInf[29,3]=1
laRltFdInf[30,1]="EOMDAY"
laRltFdInf[30,2]="End Of Month Day"
laRltFdInf[30,3]=2
laRltFdInf[31,1]="CODYN"
laRltFdInf[31,2]="C.O.D (Y/N)"
laRltFdInf[31,3]=1
laRltFdInf[32,1]="LINSTALLM"
laRltFdInf[32,2]="Use Installments (Y/N)"
laRltFdInf[32,3]=1
laRltFdInf[33,1]="LLCASH"
laRltFdInf[33,2]="Cash Payment (Y/N)"
laRltFdInf[33,3]=1
laRltFdInf[34,1]="NRYLRATE"
laRltFdInf[34,2]="Royalty Rate"
laRltFdInf[34,3]=6
laRltFdInf[35,1]="CARRIERCOD"
laRltFdInf[35,2]="Carrier Code"
laRltFdInf[35,3]=4
laRltFdInf[36,1]="CUPC"
laRltFdInf[36,2]="UPC Type"
laRltFdInf[36,3]=13
laRltFdInf[37,1]="NCODCHARGE"
laRltFdInf[37,2]="COD Charge"
laRltFdInf[37,3]=5
laRltFdInf[38,1]="NFXDPRCNT"
laRltFdInf[38,2]="Merchandise Charge"
laRltFdInf[38,3]=5
laRltFdInf[39,1]="NINSCHARGE"
laRltFdInf[39,2]="Insurance Charge/100$"
laRltFdInf[39,3]=5
laRltFdInf[40,1]="NTAXRATE"
laRltFdInf[40,2]="Tax Rate"
laRltFdInf[40,3]=6
laRltFdInf[41,1]="CTAXRULE"
laRltFdInf[41,2]="Tax Rule"
laRltFdInf[41,3]=2
laRltFdInf[42,1]="CGLINPACCT"
laRltFdInf[42,2]="GL Input Account"
laRltFdInf[42,3]=24
laRltFdInf[43,1]="CGLOUTACCT"
laRltFdInf[43,2]="GL Output Account"
laRltFdInf[43,3]=24
laRltFdInf[44,1]="C1099CODE"
laRltFdInf[44,2]="1099 Code(Rent,Royalties)"
laRltFdInf[44,3]=2

*B605226,1 RAE [START]
laRltFdInf[45,1]="LLOBSOLETE"
laRltFdInf[45,2]="Obsolete"
laRltFdInf[45,3]=10
*B605226,1 RAE [END]
*-- end of lfFillRelt.


FUNCTION lfrepwhen
gfopenTable('CODES','CODES','SH','CODES')
*gfopenTable('SYSCCOMP','CCOMP_ID','SH','SYSCCOMP')
gfopenTable('Sydfield','CFLD_NAME','SH','Sydfield')
*!************************************************************************
*! Name      : lfFillCode
*! Developer : Mariam MAzhar
*! Date      : 09/28/2008
*! Purpose   : Fill The arrays of companys information & code information
*!************************************************************************
FUNCTION lfFillCode
PRIVATE lnI

*-- Fill Company Array
SELECT SYCCOMP
lnI = 1
*-- scaning "SYCCOMP" to get the companys information
SCAN
  DIMENSION laCompDesc[lnI,1] , laCompVal[lnI,1]
  laCompDesc[lnI,1] = cComp_Id + "-" + cCom_Name    && array to hold the companys information
                                                    && (ID & Name)
                                                    
  laCompVal[lnI,1]  = cComp_Id                      && array to hold the companys ID
  lnI = lnI + 1
ENDSCAN   && end scaning "SYCCOMP" to get the companys information

*-- Fill codes arrays.
lcTempCode = gfTempName()  && temp cursor to hold codes 


IF USED('Sydfield')
  USE IN Sydfield
endif 
gfopenTable('Sydfield','CFLD_NAME','SH','Sydfield')
*-- select the needed information for codes from "Sydfield" and save it in the temp cuesor
*-- lcTempCode 
SELECT  cfld_name , cfld_head ,mrltfields ,lrltfields ,mcodeinfo;
 FROM Sydfield;
 WHERE lvldentry ;
 ORDER BY cfld_head ;
 INTO CURSOR (lcTempCode)

*-- if there is no codes found and saved in (lcTempCode)
*-- terminate the Option Grid , else fill codes array
IF _TALLY = 0
  WAIT WINDOW "No Codes found"
  llOgTrmnat = .T.
  RETURN .F.
ELSE
  DIMENSION laCodeDesc[_TALLY + 1,1] , laCodeRet[_TALLY + 1,1]  && array to hold codes information
  laCodeDesc[1] = "All"  
  laCodeRet[1]  = ""

  SELECT (lcTempCode)
  lnI = 2
  *-- scan temp cursor to fill the codes array
  SCAN
    laCodeDesc[lnI,1] = ALLTRIM(cfld_head)
    laCodeRet [lnI,1] = cfld_name
    lnI = lnI + 1
  ENDSCAN  && scan temp cursor to fill the codes array
  
ENDIF  && if there is no codes found and saved in (lcTempCode)

SELECT (lcTempCode)
INDEX ON cfld_name TAG (lcTempCode) &&OF (oAriaapplication.WorkDir+lcTempCode+".CDX")


*-- end of lfFillCode.

*!*************************************************************

