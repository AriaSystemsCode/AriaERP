*:***************************************************************************
*: Program file  : ARSCDACT.PRG
*: Program desc. : Scheduled Activities Report.
*: 				   The program is a conversion from 2.6 ..CONT910.PRG
*! Date          : 07/04/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: New Program 000153
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARSCDACT
*:***************************************************************************
*: Modifications :
*:               
*:E500372,1 MHM 08/08/2000 Replace time with customer name and reason
*:E301449,1 MHM 08/15/2000 Add (print details Yes/No )
*;B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*:B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:E301553,1 MHM 02/15/2001 Add Sort by date ,change The second line "Account/Name .." to        
*:E301553,1                be the first line and the Reason in the place of the Result  
*:E301553,1                and the Result should be in the place of Reason and
*:E301553,1                Change the status "P" to "Pending" , "H" to "History"
*:E301584,1 MHM 03/15/2001 Add Sort by complete date
*:B804387,1 MHM 09/20/2001 changes in FRX to print from A4 to 8 1/2 * 11
*:E302014,1 RAE 09/26/2002 Add Sort by Customer.
*:E302048,1 BWA 11/05/2002 Add filter Customer status and customer list.
*:*****************************************************************************************
*: E500372  MHM 08/08/2000   [BEGIN]
STORE '' TO lcResult
*: E500372  MHM 08/08/2000   [END]

*E302048,1 BWA 11/05/2002 Initial variables hold the filter of the list and the status.[START]
*-- Get the Variables its Initial value
STORE SPACE(0) TO lcFilter , lcStatus
=lfvCrATVar()
*E302048,1 BWA 11/05/2002.[END]

SELECT SYUUSER
GO TOP
IF EOF()
  =gfDialog("I","You cannot run this report if you are logged in with no user ID.")
  RETURN
ENDIF

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*-- Collectting the data.
* -WHERE  CUSER_ID = lcUser_Id ;
*: E500372  MHM 08/08/2000   [BEGIN]
*SELECT ctrantype, dtrandate , ctrantime , contact ,;
*       cphone , csubject , ctranreson ,ccont_id,cuser_id ;

*:E301553,1  MHM 02/15/2001 add Sort By Date[start]
*SELECT ctrantype, dtrandate , ctrantime , contact ,;
*       cphone , csubject , ctranreson ,ccont_id,cuser_id , DAdd_Date , dcmpltdate,mNotes ,cResultcd ,cCompleted ;
*FROM SYSCHDUL ;
*WHERE        IIF(!EMPTY(lcUser_Id), CUSER_ID = lcUser_Id ,.T.);
*       .AND. IIF(lcRpTask  = 'L' , CTRANTYPE = "" , CTRANTYPE = lcRpTask);
*       .AND. IIF(lcRpStats = 'B' , CCOMPLETED = "" , CCOMPLETED = lcRpStats);
*       .AND. IIF(ldFDate ={} .AND. ldTDate = {} , DTRANDATE   = DTRANDATE ,;
*             IIF(ldFDate ={} .AND. ldTDate <>{} , DTRANDATE  <= LDTDATE   , BETWEEN(DTRANDATE,ldFDate,LDTDATE)));
*       .AND. IIF(ldFAdd  ={} .AND. ldTAdd  = {} , DADD_DATE   = DADD_DATE ,;
*             IIF(ldFAdd  ={} .AND. ldTAdd  <>{} , DADD_DATE  <= ldTAdd    , BETWEEN(DADD_DATE,ldFAdd,ldTAdd)));
*       .AND. IIF(ldFCmplt={} .AND. ldTCmplt= {} , DcmpltDATE  = DcmpltDATE,;
*             IIF(ldFCmplt={} .AND. ldTCmplt<>{} , DcmpltDATE <= ldTCmplt , BETWEEN(DcmpltDATE,ldFCmplt,ldTCmplt)));
*       INTO TABLE (GCWORKDIR+lcTempFile);
*ORDER BY CUSER_ID , CTRANTYPE,DTRANDATE ,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME

*--lcRpSortBy  = "T" Sort by task type
*--lcRpSortBy  = "D" Sort by schadule date
*--lcRpSortBy  = "C" Sort by Complete date
*--lcRpSortBy  = "S" Sort by Customer
DO CASE 
  CASE lcRpSortBy = "D"
    lcOrd = "CUSER_ID , DTRANDATE, CTRANTYPE,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME"
  CASE lcRpSortBy = "C"
    lcOrd = "CUSER_ID , DcmpltDATE, CTRANTYPE,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME"

  *E302014,1 RAE Add Sort by Customer. [start]
  CASE lcRpSortBy = "S"
    lcOrd = "CUSER_ID,CCONT_ID,DcmpltDATE,CTRANTYPE,CTRANTIME,CCONTTYPE,STORE,CTRANTIME"
  *E302014,1 RAE [end]

  OTHERWISE
    lcOrd = "CUSER_ID , CTRANTYPE,DTRANDATE,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME"       
ENDCASE 

SELECT ctrantype, dtrandate , ctrantime , contact ,;
       cphone , csubject , ctranreson ,ccont_id,cuser_id , DAdd_Date , dcmpltdate,mNotes ,cResultcd ,cCompleted ;
  FROM SYSCHDUL ;
  WHERE        IIF(!EMPTY(lcUser_Id), CUSER_ID = lcUser_Id ,.T.);
         .AND. IIF(lcRpTask  = 'L' , CTRANTYPE = "" , CTRANTYPE = lcRpTask);
         .AND. IIF(lcRpStats = 'B' , CCOMPLETED = "" , CCOMPLETED = lcRpStats);
         .AND. IIF(ldFDate ={} .AND. ldTDate = {} , DTRANDATE   = DTRANDATE ,;
               IIF(ldFDate ={} .AND. ldTDate <>{} , DTRANDATE  <= LDTDATE   , BETWEEN(DTRANDATE,ldFDate,LDTDATE)));
         .AND. IIF(ldFAdd  ={} .AND. ldTAdd  = {} , DADD_DATE   = DADD_DATE ,;
               IIF(ldFAdd  ={} .AND. ldTAdd  <>{} , DADD_DATE  <= ldTAdd    , BETWEEN(DADD_DATE,ldFAdd,ldTAdd)));
         .AND. IIF(ldFCmplt={} .AND. ldTCmplt= {} , DcmpltDATE  = DcmpltDATE,;
               IIF(ldFCmplt={} .AND. ldTCmplt<>{} , DcmpltDATE <= ldTCmplt , BETWEEN(DcmpltDATE,ldFCmplt,ldTCmplt)));
         INTO TABLE (GCWORKDIR+lcTempFile);
  ORDER BY &lcOrd
*:E301553,1  MHM 02/15/2001 add Sort By Date[End]

*: E500372  MHM 08/08/2000   [END]

*-- End Collectting the data.

IF _TALLY = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET CENTURY &lcCentury
  RETURN
ENDIF
*-- Relation to the codes file.
=gfOpenFile(gcdatadir+'CUSTOMER','CUSTOMER','SH')
SELECT (lcTempFile)
*: E500372  MHM 08/08/2000 correct index  [Begin] 
*SET RELATION TO 'W'+ ctranreson INTO CODES
SET RELATION TO 'N'+ctranreson+'N'+PADR('CTRANRESON',10) INTO CODES
*: E500372  MHM 08/08/2000 correct index  [End] 


*: E500372  MHM 08/08/2000   [Begin] 
SET RELATION TO 'M'+ CCONT_ID INTO CUSTOMER ADDITIVE
*--set relation with syuuser to get user name
SET RELATION TO cuser_id INTO SYUUSER ADDITIVE
*: E500372  MHM 08/08/2000   [End] 
*:E301553,1  MHM 02/15/2001  [start]
IF lcRpSortBy = "T" 
 lcRpForm = "ARSCDACT"
ELSE
 lcRpForm = "ARSCDACD"
ENDIF
*:E301553,1  MHM 02/15/2001  [end]

*E302048,1 BWA 11/05/2002 Add the filter hold the status and the selected customer.[START]
*DO gfDispRe WITH EVALUATE('lcRpForm')
DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + lcFilter
*E302048,1 BWA 11/05/2002.[END]

SET CENTURY &lcCentury


*!*************************************************************
*! Name      : lfRtrnHad
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Function to return printed header according to the
*:           : passed type.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfRtrnHad()
*!*************************************************************
*: E500372  MHM 08/08/2000   [BEGIN]
*FUNCTION lfRtrnHad
*PARAMETERS lcGroup
*: E500372  MHM 08/08/2000   [BEGIN]
*DO CASE
*  CASE lcGroup = 'C'
*    RETURN 'DATE      NAME                          CONTACT           REASON         REFERENCE      SUBJECT        PHONE '
*  CASE lcGroup = 'T'
*    RETURN 'DATE      NAME                          CONTACT           REASON         REFERENCE      SUBJECT'
*  CASE lcGroup = 'A'
*    RETURN 'DATE      NAME                          CONTACT           REASON         REFERENCE      SUBJECT'
*  OTHERWISE
*    RETURN ' '  
*    RETURN 'DATE      NAME                          CONTACT           REASON         REFERENCE      SUBJECT        PHONE '

*RETURN 'Status Contact            Reason             Subject                        Result              Reference           Phone'
*: E500372  MHM 08/08/2000   [END]

*-- End of lfRtrnHad.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = ldfDate
LDTO   = ldtDate
lcDateType = 'D'

lcTitle = 'Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbok
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Validate date range screen's OK button.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbok()
*!*************************************************************
FUNCTION lfvpbok

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  DO CASE
    CASE lcDateType = 'D'
      ldfDate = ldFrom
      ldtDate = ldTo
    CASE lcDateType = 'E'
      ldFAdd = ldFrom  
      ldTAdd = LDTO 
    CASE lcDateType = 'C'
      ldFCmplt = ldFrom 
      ldTCmplt = LDTO
  ENDCASE  
  CLEAR READ
ENDIF

*-- End of lfvpbok.
*!*************************************************************
*! Name      : lfvUserId
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Validation function for the user id.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvUserId()
*!*************************************************************
FUNCTION lfvUserId

SELECT SYUUSER
IF !SEEK(lcUser_Id)
  =gfVlFld('SYUUSER','CUser_ID',VARREAD(),'',.F.,.F.,[CUser_ID:H="User ID",CUsr_name:H="User name"],'1',.F.,.t.)
ENDIF

*-- End of lfvUserId.
*!*************************************************************
*! Name      : lfsrUSer
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : To get the pointer to the first record.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrUSer()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol is Reset
*!*************************************************************
FUNCTION lfsrUSer
PARAMETERS lcParm
SELECT SYUUSER
GO TOP

*-- End of lfsrUSer.
*!*************************************************************
*! Name      : lfvEntrd
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes add date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvEntrd()
*!*************************************************************
FUNCTION lfvEntrd
PRIVATE ldFrom,ldTo

ldFrom = ldFAdd
LDTO   = ldTAdd
lcDateType = 'E'

lcTitle = 'Entered Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]


*-- End of lfvEntrd.
*!*************************************************************
*! Name      : lfvComplt
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes Complete date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvComplt()
*!*************************************************************
FUNCTION lfvComplt
PRIVATE ldFrom,ldTo

ldFrom = ldFCmplt
LDTO   = ldTCmplt
lcDateType = 'C'

lcTitle = 'Complete Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]


*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvRus
*! Developer : MOHAMED SHOKRY
*! Date      : 07/04/2000
*! Purpose   : Get Result From Codes
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvRus()
*!*************************************************************
*E500372,1 
FUNCTION lfvRus
PRIVATE lnAlias , lcRelat
lnAlias = SELECT(0)
lcRelat = SET('RELATION')
SET RELATION OFF INTO CODES
SET RELATION TO 'N'+cresultcd+'N'+PADR('CRESULTCD',10) INTO CODES
SELECT CODES
=SEEK('N'+&lcTempFile..cresultcd+'N'+PADR('CRESULTCD',10) )
lcResult = cdiscrep
SELECT(lnAlias)
SET RELATION TO &lcRelat
RETURN ''
*-- End of lfvDateRng.

*!*************************************************************
*! Name      : lfvSortBy                   (E301553)
*! Developer : MOHAMED SHOKRY
*! Date      : 02/15/2001
*! Purpose   : to Update Form name
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy

IF lcRpSortBy  = "T" 
 lcRpForm = "ARSCDACT"
ELSE
 lcRpForm = "ARSCDACD"
ENDIF

*--End of lfvSortBy.
*E302048,1 BWA 11/05/2002 Functions used to browse the customer status and list and there validations.[START]
*!**********************************************************
*! Name      : lfAccount
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : The valedation function for the customer.
*!**********************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**********************************************************
*! Return      : None
*!**********************************************************
*! Example     : =lfSRInv (S-> , R->)
*!**********************************************************
FUNCTION lfAccount
PARAMETERS lcParm

IF lcParm = "S"
  GO TOP IN CUSTOMER
ENDIF

*--End of lfAccount.
*!*************************************************************
*! Name      : lfFillAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Function to fill Customer status mover.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillAll
PRIVATE lcAlasCust

lcAlasCust = SELECT(0)

DIMENSION laRpSource[1,1],laRpTarget[1,1]
STORE '' TO laRpSource,laRpTarget

*--The Customer status.
SELECT CUSTOMER
SELECT DISTINCT STATUS FROM CUSTOMER WHERE TYPE + ACCOUNT + STORE = "M" INTO ARRAY laRpSource

*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpSource,1)
  laRpSource[lnI] = IIF(laRpSource[lnI] = 'A', 'Active'       ,;
                    IIF(laRpSource[lnI] = 'H', 'Hold'         ,;
                    IIF(laRpSource[lnI] = 'P', 'Potential'    ,;
                    IIF(laRpSource[lnI] = 'X', 'Canceled',    	''))))

ENDFOR

SELECT(lcAlasCust)

*-- End of lfFillAll.
*!*************************************************************
*! Name      : lfvCrATVar
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

*--Section for the customer salesrep.
lnSalsPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"SALESREP.REPCODE"),1)
IF lnSalsPos > 1
  lnSalsPos = AT('INLIST(SALESREP.REPCODE', lcRPExp)
  IF lnSalsPos = 0
    lnSalsPos  = AT('BETWEEN(SALESREP.REPCODE', lcRPExp)
    IF lnSalsPos > 0
      lnSalesPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"SALESREP.REPCODE"),1)
      lcHoldNam  = laOGFxFlt[lnSalesPos,6]
      lnPosTmp   = AT(lcHoldNam , SUBSTR(lcRPExp,lnSalsPos)) + 9
      lcSalsPos  = SUBSTR(lcRpExp , lnSalsPos , lnPosTmp)
    ENDIF
    lnSalsPos   = 0
  ENDIF

  IF lnSalsPos > 0
    lnPos1     = AT('")' , SUBSTR(lcRPExp,lnSalsPos))
    lcSalsPos  = SUBSTR(lcRpExp , lnSalsPos , lnPos1+1)


    *--Section to creat the string check the first salesrep and the 2nd one in the customer.
    lcHoldSals = lcSalsPos
    lcSalsPos  = STRTRAN(lcSalsPos,"SALESREP.REPCODE" , "CUSTOMER.SALESREP")
    lcSalsPos2 = STRTRAN(lcSalsPos, "CUSTOMER.SALESREP" , "CUSTOMER.REP2" )
    lcAddSals = "( " + lcSalsPos + " .OR. " + lcSalsPos2 + " )"
    lcRpExp = STRTRAN(lcRpExp,lcHoldSals , lcAddSals)
  ENDIF
ENDIF


*--The filter
lcFilter = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  laRpTarget[lnI] = IIF(laRpTarget[lnI] = 'Active','A',;
                    IIF(laRpTarget[lnI] = 'Hold','H',;
                    IIF(laRpTarget[lnI] = 'Potential','P',;
                    IIF(laRpTarget[lnI] = 'Canceled','X',''))))
ENDFOR

*--The Customer status.
FOR lnInd = 1 TO ALEN(laRpTarget)
  lcStatus = lcStatus + PADR(laRpTarget[lnInd],6) + ' | '
ENDFOR
lcStatus = IIF(ALLTRIM(lcStatus) = '|','',lcStatus)

IF !EMPTY(lcStatus)
  lcFilter = lcFilter + ' AND CUSTOMER.STATUS $ lcStatus'
ENDIF

*--End of lfvCrATVar.
*!*************************************************************
*! Name      : lfvStatus
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : Function to call mover for the status.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvStatus

*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  laRpTarget[lnI] = IIF(laRpTarget[lnI] = 'A', 'Active'    ,;
                    IIF(laRpTarget[lnI] = 'H', 'Hold'      ,;
                    IIF(laRpTarget[lnI] = 'P', 'Potential' ,;
                    IIF(laRpTarget[lnI] = 'X', 'Canceled'  ,''))))
ENDFOR
= gfMover(@laRpSource,@laRpTarget,'Customer Status',.T.,'')

*--End of lfvStatus.
*:*************************************************************
*: Name      : lfSeTSRep 
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*: Purpose   : Go top in Sales Rep file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetSRep()
*:*************************************************************
FUNCTION lfSetSRep
PARAMETERS OpGrdParm
PRIVATE lcAlasSals

lcAlasSals = SELECT(0)

IF OpGrdParm = 'S'
   SELECT SALESREP
   LOCATE
ENDIF

SELECT(lcAlasSals)

*--End of lfSetSRep.
*E302048,1 BWA 11/05/2002.[END]

*-- 03/11/2003 MAB ARIA4 COMPATABILITY ..... BEGIN
FUNCTION lfwOGWhen
*-- end of lfwOGWhen.
*-- 03/11/2003 MAB ARIA4 COMPATABILITY ..... END
