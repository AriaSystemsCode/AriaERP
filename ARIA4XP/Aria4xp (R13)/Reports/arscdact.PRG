*-- Start of converting by MAB (BADRAN 02/27/2003)
*-- N000504 : BADRAN (Schedule Activity Report.)
*:***************************************************************************
*: Program file  : ARSCDACT.PRG
*: Program desc. : Scheduled Activities Report.
*: 				   The program is a conversion from 2.6 ..CONT910.PRG
*! Date          : 07/04/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: New Program 000153
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*!***************************************************************************
* N000682 ,1 Thabet Handle globalization issues [Start]
#INCLUDE r:\aria4xp\reports\arscdact.h
* N000682 ,1 Thabet Handle globalization issues [End]
STORE SPACE(0) TO lcResult, lcFilter , lcStatus

*--
=lfvCrATVar()

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*-- Collectting the data ..... BEGIN
*--lcRpSortBy  = "T" Sort by task type
*--lcRpSortBy  = "D" Sort by schadule date
*--lcRpSortBy  = "C" Sort by Complete date
*--lcRpSortBy  = "S" Sort by Customer
*--E124743,[MMT] 12/22/04 Add Priority to sort by option
*--lcRpSortBy  = "P" Sort by Priority
*--E124743,[MMT] 12/22/04 Add Priority to sort by option
DO CASE
  CASE lcRpSortBy = "D"
    lcOrd = "CUSER_ID , DTRANDATE, CTRANTYPE,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME"
  CASE lcRpSortBy = "C"
    lcOrd = "CUSER_ID , DcmpltDATE, CTRANTYPE,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME"

  CASE lcRpSortBy = "S"
    lcOrd = "CUSER_ID,CCONT_ID,DcmpltDATE,CTRANTYPE,CTRANTIME,CCONTTYPE,STORE,CTRANTIME"
  *--E124743,[MMT] 12/22/04 Add Priority to sort by option
  CASE lcRpSortBy = "P"
    lcOrd = "CUSER_ID ,cPRIORITY ,DTRANDATE, CTRANTYPE,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME"
  *--E124743,[MMT] 12/22/04 Add Priority to sort by option

  OTHERWISE
    lcOrd = "CUSER_ID , CTRANTYPE,DTRANDATE,CTRANTIME,CCONTTYPE,CCONT_ID,STORE,CTRANTIME"
ENDCASE

*-- Get the Schedule date (Remotely) ..... BEGIN
*--   SELECT ctrantype, dtrandate , ctrantime , contact ,;
*--          cphone , csubject , ctranreson ,ccont_id,cuser_id , DAdd_Date , dcmpltdate,mNotes ,cResultcd ,cCompleted ;
*--     FROM SYSCHDUL ;
*--     WHERE        IIF(!EMPTY(lcUser_Id), CUSER_ID = lcUser_Id ,.T.);
*--            .AND. IIF(lcRpTask  = 'L' , CTRANTYPE = "" , CTRANTYPE = lcRpTask);
*--            .AND. IIF(lcRpStats = 'B' , CCOMPLETED = "" , CCOMPLETED = lcRpStats);
*--            .AND. IIF(ldFDate ={} .AND. ldTDate = {} , DTRANDATE   = DTRANDATE ,;
*--                  IIF(ldFDate ={} .AND. ldTDate <>{} , DTRANDATE  <= LDTDATE   , BETWEEN(DTRANDATE,ldFDate,LDTDATE)));
*--            .AND. IIF(ldFAdd  ={} .AND. ldTAdd  = {} , DADD_DATE   = DADD_DATE ,;
*--                  IIF(ldFAdd  ={} .AND. ldTAdd  <>{} , DADD_DATE  <= ldTAdd    , BETWEEN(DADD_DATE,ldFAdd,ldTAdd)));
*--            .AND. IIF(ldFCmplt={} .AND. ldTCmplt= {} , DcmpltDATE  = DcmpltDATE,;
*--                  IIF(ldFCmplt={} .AND. ldTCmplt<>{} , DcmpltDATE <= ldTCmplt , BETWEEN(DcmpltDATE,ldFCmplt,ldTCmplt)));
*--            INTO TABLE (GCWORKDIR+lcTempFile);
*--     ORDER BY &lcOrd
*--   *-- Collectting the data ..... END

*--   IF _TALLY = 0
*--     *---Text : 'No Record Selected for the report..!'
*--     =gfModalGen('TRM00052B00000','DIALOG')
*--     SET CENTURY &lcCentury
*--     RETURN .T.
*--   ENDIF

LOCAL lnRemoteResult, lcSelectCommand, lcWhereCond, llNoData
*--E124743,[MMT] 12/22/04 Add Priority to sort by option
*lcSelectCommand = [SELECT ctrantype, dtrandate , ctrantime , contact ,] +;
                  [cphone , csubject , ctranreson ,ccont_id,cuser_id ,] +;
                  [DAdd_Date , dcmpltdate,mNotes ,cResultcd ,cCompleted FROM SYSCHDUL]
lcSelectCommand = [SELECT ctrantype, dtrandate , ctrantime , contact ,] +;
                  [cphone , csubject , ctranreson ,ccont_id,cuser_id ,] +;
                  [DAdd_Date , dcmpltdate,mNotes ,cResultcd ,cCompleted ,cPriority FROM SYSCHDUL]

*--E124743,[MMT] 12/22/04 Add Priority to sort by option
lcWhereCond = GetWhereCond()
IF !EMPTY(lcWhereCond)
  lcSelectCommand = lcSelectCommand + " WHERE " + lcWhereCond
ENDIF
lcSelectCommand = lcSelectCommand + " ORDER BY " + lcOrd

lnRemoteResult = loOGScroll.SQLExecute("SYSCHDUL", lcSelectCommand,"",lcTempFile,"",;
    oAriaApplication.SystemConnectionString,3,"")
IF lnRemoteResult >= 1
  SELECT (lcTempFile)
  LOCATE
  llNoData = EOF()
ELSE
  llNoData = .T.
ENDIF
IF llNoData
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET CENTURY &lcCentury
  RETURN .F.
ENDIF
*-- Get the Schedule date (Remotely) ..... END

*-- Set relation between tables ..... BEGIN
=gfOpenFile(gcdatadir+'CUSTOMER','CUSTOMER','SH')
SELECT (lcTempFile)
SET RELATION TO 'N'+ctranreson+'N'+PADR('CTRANRESON',10) INTO CODES
SET RELATION TO 'M'+ CCONT_ID INTO CUSTOMER ADDITIVE
SET RELATION TO cuser_id INTO SYUUSER ADDITIVE
*-- Set relation between tables ..... END

IF lcRpSortBy = "T"
 lcRpForm = "ARSCDACT"
ELSE
 lcRpForm = "ARSCDACD"
ENDIF
DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + lcFilter
SET CENTURY &lcCentury.
RETURN .T.
*-- end of main code.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes date range screen
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo
ldFrom = ldfDate
LDTO   = ldtDate
lcDateType = 'D'
* N000682 ,1 Thabet Handle globalization issues [Start]
*lcTitle = 'Date range'
lcTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATERANGE,oAriaApplication.GetHeaderText("LANG_DATERANGE",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]
DO DateRng
*-- End of lfvDateRng.

*!*************************************************************
*! Name      : lfvpbok
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Validate date range screen's OK button.
*!*************************************************************
FUNCTION lfvpbok
IF ldFrom > ldTo

   * N000682 ,1 Thabet Handle globalization issues [Start]
   *WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FROM_VALU,oAriaApplication.GetHeaderText("LANG_FROM_VALU",AHEADERFILE)) NOWAIT
   * N000682 ,1 Thabet Handle globalization issues [END]
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
ENDIF
*-- End of lfvpbok.

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
FUNCTION lfvEntrd
PRIVATE ldFrom,ldTo
ldFrom = ldFAdd
LDTO   = ldTAdd
lcDateType = 'E'
* N000682 ,1 Thabet Handle globalization issues [Start]
*lcTitle = 'Entered Date range'
lcTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENTEREDDATERANGE,oAriaApplication.GetHeaderText("LANG_ENTEREDDATERANGE",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]
DO DateRng
*-- End of lfvEntrd.

*!*************************************************************
*! Name      : lfvComplt
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Showes Complete date range screen
*!*************************************************************
FUNCTION lfvComplt
PRIVATE ldFrom,ldTo
ldFrom = ldFCmplt
ldTo   = ldTCmplt
lcDateType = 'C'
* N000682 ,1 Thabet Handle globalization issues [Start]
*lcTitle = 'Complete Date range'
lcTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPDATERANGE,oAriaApplication.GetHeaderText("LANG_COMPDATERANGE",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [End]
DO DateRng
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
FUNCTION lfvSortBy
IF lcRpSortBy  = "T"
 lcRpForm = "ARSCDACT"
ELSE
 lcRpForm = "ARSCDACD"
ENDIF
*--End of lfvSortBy.

*!**********************************************************
*! Name      : lfAccount
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*! Purpose   : The valedation function for the customer.
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
  * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	  laRpSource[lnI] = IIF(laRpSource[lnI] = 'A', 'Active'       ,;
*!*	                    IIF(laRpSource[lnI] = 'H', 'Hold'         ,;
*!*	                    IIF(laRpSource[lnI] = 'P', 'Potential'    ,;
*!*	                    IIF(laRpSource[lnI] = 'X', 'Canceled',    	''))))
  laRpSource[lnI] = IIF(laRpSource[lnI] = 'A', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACTIVE,oAriaApplication.GetHeaderText("LANG_ACTIVE",AHEADERFILE))  ,;
                    IIF(laRpSource[lnI] = 'H', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE)) ,;
                    IIF(laRpSource[lnI] = 'P', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POTENTIAL,oAriaApplication.GetHeaderText("LANG_POTENTIAL",AHEADERFILE))  ,;
					IIF(laRpSource[lnI] = 'X', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCELLLED,oAriaApplication.GetHeaderText("LANG_CANCELLLED",AHEADERFILE)),    	''))))
* N000682 ,1 Thabet Handle globalization issues [End]
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
FUNCTION lfvCrATVar

*--Section for the customer salesrep.
lnSalsPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(laOGFxFlt,"SALESREP.REPCODE"),1)
IF lnSalsPos > 1
  lnSalsPos = AT('INLIST(SALESREP.REPCODE', lcRPExp)
  IF lnSalsPos = 0
    lnSalsPos  = AT('BETWEEN(SALESREP.REPCODE', lcRPExp)
    IF lnSalsPos > 0
            lnSalesPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(laOGFxFlt,"SALESREP.REPCODE"),1)
            lcHoldNam  = loOGScroll.laOGFxFlt[lnSalesPos,6]
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
  * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	  laRpTarget[lnI] = IIF(laRpTarget[lnI] = 'Active','A',;
*!*	                    IIF(laRpTarget[lnI] = 'Hold','H',;
*!*	                    IIF(laRpTarget[lnI] = 'Potential','P',;
*!*	                    IIF(laRpTarget[lnI] = 'Canceled','X',''))))
  laRpTarget[lnI] = IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACTIVE,oAriaApplication.GetHeaderText("LANG_ACTIVE",AHEADERFILE)),'A',;
                    IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE)),'H',;
                    IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POTENTIAL,oAriaApplication.GetHeaderText("LANG_POTENTIAL",AHEADERFILE)),'P',;
					IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCELLLED,oAriaApplication.GetHeaderText("LANG_CANCELLLED",AHEADERFILE)),'X',''))))
  * N000682 ,1 Thabet Handle globalization issues [End]
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
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*!***************************************************************************
FUNCTION lfvStatus
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	  laRpTarget[lnI] = IIF(laRpTarget[lnI] = 'A', 'Active'    ,;
*!*	                    IIF(laRpTarget[lnI] = 'H', 'Hold'      ,;
*!*	                    IIF(laRpTarget[lnI] = 'P', 'Potential' ,;
*!*	                    IIF(laRpTarget[lnI] = 'X', 'Canceled'  ,''))))
  laRpTarget[lnI] = IIF(laRpTarget[lnI] = 'A', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACTIVE,oAriaApplication.GetHeaderText("LANG_ACTIVE",AHEADERFILE)),;
                    IIF(laRpTarget[lnI] = 'H', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE)),;
                    IIF(laRpTarget[lnI] = 'P', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POTENTIAL,oAriaApplication.GetHeaderText("LANG_POTENTIAL",AHEADERFILE)) ,;
					IIF(laRpTarget[lnI] = 'X', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCELLLED,oAriaApplication.GetHeaderText("LANG_CANCELLLED",AHEADERFILE)),''))))
  * N000682 ,1 Thabet Handle globalization issues [END]
ENDFOR

* B123663,1 SMM Change gfMover to lfOGMover
*-- = gfMover(@laRpSource,@laRpTarget,'Customer Status',.T.,'')
* N000682 ,1 Thabet Handle globalization issues [Start]
*= lfOGMover(@laRpSource,@laRpTarget,'Customer Status',.T.,'')
 = lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTOMERSTATUS,oAriaApplication.GetHeaderText("LANG_CUSTOMERSTATUS",AHEADERFILE)),.T.,'')
  * N000682 ,1 Thabet Handle globalization issues [End]
* B123663,1 SMM End

*--End of lfvStatus.

*:*************************************************************
*: Name      : lfSeTSRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/05/2002
*: Purpose   : Go top in Sales Rep file.
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

*-- End of converting by MAB (BADRAN 02/27/2003)

*!*****************************************************************************************
*! Name      : AfterOpenDataFiles
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 02/27/2003 02:20:34 PM
*! Purpose   : Event occurs after open data files.
*!*****************************************************************************************
*! Parameters: laOGTables - Data Tables array passed By Ref (@)
*!****************************************************************************************
*!
FUNCTION AfterOpenDataFiles
  LPARAMETERS laOGTables


  LOCAL lnRemoteResult, lcSelectCommand, lcUserTable
  lcSelectCommand = [SELECT DISTINCT cUser_ID, cUsr_Name FROM SYUUSER ORDER BY cUser_ID]

  lcUserTable    = gfTempName()
  lnRemoteResult = loOGScroll.SQLExecute("SYUUSER", lcSelectCommand,"",lcUserTable,"",;
      oAriaApplication.SystemConnectionString,3,"")
  IF lnRemoteResult < 1
    RETURN .F.
  ELSE
    LOCAL laUserStru[1,18]
    =AFIELDS(laUserStru)
    CREATE TABLE (oAriaApplication.WorkDir+lcUserTable+".DBF") FROM ARRAY laUserStru
    *-- Amin
    USE  && Close the Temporary user table.
    USE IN SYUUSER
    *-- Amin

    USE (oAriaApplication.WorkDir+lcUserTable+".DBF") IN 0 ALIAS SYUUSER EXCLUSIVE
    SELECT SYUUSER
    INDEX ON cUser_ID TAG cUser_ID OF (oAriaApplication.WorkDir+lcUserTable+".CDX")
    APPEND FROM FULLPATH(DBF(lcUserTable))
    USE IN (lcUserTable)

  ENDIF
  RETURN .T.
ENDFUNC
*-- end of AfterOpenDataFiles.

*!*****************************************************************************************
*! Name      : lfwOGWhen
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/02/2003 02:20:34 PM
*! Purpose   : Option Grid when function
*!*****************************************************************************************
*!
FUNCTION lfwOGWhen
  *-- Adjust date range captions.
  LOCAL loDateObj
  *-- Schedule Date.
  loDateObj = GetObjectRef("lnDumScd")
  IF ATC("Between",loDateObj.Caption) = 0
    loDateObj.Caption = "Between ..."
    loDateObj = .NULL.

    *-- Entered Date.
    loDateObj = GetObjectRef("lnDumAdd")
    loDateObj.Caption = "Between ..."
    loDateObj = .NULL.

    *-- Complete Date.
    loDateObj = GetObjectRef("lnDumCmpl")
    loDateObj.Caption = "Between ..."
    loDateObj = .NULL.
  ELSE
    loDateObj = .NULL.
  ENDIF

ENDFUNC
*-- end of lfwOGWhen.

*!*****************************************************************************************
*! Name      : RefSchdule
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/02/2003 11:18:46 AM
*! Purpose   :
*! Entry no. :
*!*****************************************************************************************
*!
FUNCTION RefSchdule
  LPARAMETERS cDateType
  LOCAL lcSelDate, ldFromDate, ldToDate, lcFromDate, lcToDate
  DO CASE
    CASE cDateType = 'D'
      ldFromDate = ldfDate
      ldToDate   = ldtDate
    CASE cDateType = 'E'
      ldFromDate = ldFAdd
      ldToDate   = ldTAdd
    CASE cDateType = 'C'
      ldFromDate = ldFCmplt
      ldToDate   = ldTCmplt
  ENDCASE

  lcFromDate = ALLTRIM(DTOC(ldFromDate))
  lcToDate   = ALLTRIM(DTOC(ldToDate))
  IF lcFromDate == lcToDate
    IF EMPTY(ldToDate)
      lcSelDate = ""
    ELSE
      lcSelDate = lcFromDate
    ENDIF
  ELSE
    lcSelDate = lcFromDate + ", " + lcToDate
  ENDIF
  RETURN lcSelDate
ENDFUNC
*-- End of RefSchdule.

*!*****************************************************************************************
*! Name      : RefStatus
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/02/2003 11:18:46 AM
*! Purpose   :
*! Entry no. :
*!*****************************************************************************************
*!
FUNCTION RefStatus
  LOCAL lcCustStatus, lnI
  lcCustStatus = ""
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcCustStatus = lcCustStatus + IIF(EMPTY(lcCustStatus),"",",")
    * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	    lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="A","Active","")
*!*	    lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="H","Hold","")
*!*	    lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="P","Potential","")
*!*	    lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="X","Canceled","")
   lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="A",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACTIVE,oAriaApplication.GetHeaderText("LANG_ACTIVE",AHEADERFILE)) ,"")
   lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="H",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE)) ,"")
   lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="P",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POTENTIAL,oAriaApplication.GetHeaderText("LANG_POTENTIAL",AHEADERFILE)),"")
   lcCustStatus = lcCustStatus + IIF(LEFT(laRpTarget[lnI],1)="X",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCELLLED,oAriaApplication.GetHeaderText("LANG_CANCELLLED",AHEADERFILE)),"")
   * N000682 ,1 Thabet Handle globalization issues [END]
  ENDFOR
  RETURN lcCustStatus
ENDFUNC
*-- end of RefStatus.

*!*****************************************************************************************
*! Name      : GetWhereCond
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 03/02/2003 11:18:46 AM
*! Purpose   :
*! Entry no. :
*!*****************************************************************************************
*!
FUNCTION GetWhereCond
  LOCAL lcWhereCond
  lcWhereCond = ""
  IF !EMPTY(lcUser_Id)
    lcWhereCond = lcWhereCond + 'CUSER_ID = "' + lcUser_Id + '"'
  ENDIF

  IF lcRpTask  != 'L'
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'CTRANTYPE = "' + lcRpTask + '"'
  ENDIF

  IF lcRpStats != 'B'
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'CCOMPLETED = "' + lcRpStats + '"'
  ENDIF

  IF !EMPTY(ldFDate) OR !EMPTY(ldTDate)
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'BETWEEN(DTOS(DTRANDATE),"' + DTOS(ldFDate) + '","' + DTOS(LDTDATE) + '")'
  ENDIF

  IF !EMPTY(ldFAdd) OR !EMPTY(ldTAdd)
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'BETWEEN(DTOS(DADD_DATE),"' + DTOS(ldFAdd) + '","' + DTOS(ldTAdd) + '")'
  ENDIF

  IF !EMPTY(ldFCmplt) OR !EMPTY(ldTCmplt)
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +;
         'BETWEEN(DTOS(DcmpltDATE),"' + DTOS(ldFCmplt) + '","' + DTOS(ldTCmplt) + '")'
  ENDIF
  *--E124743,[MMT] 12/22/04 Add Priority option to option grid
  IF !EMPTY(lcPriority)
    lcWhereCond = lcWhereCond + IIF(EMPTY(lcWhereCond),""," AND ") +"cPriority = '"+lcPriority+"'"
  ENDIF
  *--E124743,[MMT] 12/22/04 Add Priority option to option grid
  RETURN lcWhereCond
ENDFUNC
*-- end of GetWhereCond.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
FUNCTION lfClearRep
  LOCAL lcUserTable
  lcUserTable = UPPER(ALLTRIM(FULLPATH(DBF("SYUUSER"))))
  USE IN SYUUSER
  ERASE (lcUserTable)
  lcUserTable = STRTRAN(lcUserTable,".DBF",".CDX")
  ERASE (lcUserTable)
ENDFUNC
*-- end of lfClearRep.
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
*IF !SEEK(lcUser_Id)
*  =gfVlFld('SYUUSER','CUser_ID',VARREAD(),'',.F.,.F.,[CUser_ID:H="User ID",CUsr_name:H="User name"],'1',.F.,.t.)
*ENDIF
