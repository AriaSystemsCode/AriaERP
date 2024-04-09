*:***************************************************************************
*: Program file  : ALPKTKDL.PRG
*: Program desc. : CUSTOMIZED PICKING TICKET Form FOR DAVID LUKE LTD.
*: Date          : *04/17/2008
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : TAREK MOHAMMED IBRAHIM
*: Tracking Job Number: *T20071102.0018,7 
*!**************************************************************************
*! Modifications :
*! B608946,1 WAM 07/23/2009 Update group key sequence in temporary file [T20090716.0084]
*****************************************************************************

*--In case the user select YES in "Product Group Sequence".
PRIVATE lcAlasDl , lcOrdDl , lcKeyDL , lcOldName

lcAlasDl = SELECT(0)
lcOrdDl  = ORDER()
lcKeyDL  = IIF(!EMPTY(ALIAS()),EVAL(KEY()),'')
*lcDCCode = ''
llAlpktk = .F.
SELECT (lcTmpOrdL)

*--Save the Original name to restore it in the end of the program.
lcOldName = lcTmpOrdL

=AFIELDS(laTmpStru)
lnTmpStru = ALEN(laTmpStru,1)

*C200511,1 ABD - Add 3 new fields to the temp file and add them into the Index. [Begin]
*DIMENSION laTmpStru[lnTmpStru + 2 ,4]
*C200582,1 ABD - [Begin]
*DIMENSION laTmpStru[lnTmpStru + 5 ,4]

*C037816,1 MHM 04/06/2004 Increase Array with Location Field[Start]
*DIMENSION laTmpStru[lnTmpStru + 7 ,4]
DIMENSION laTmpStru[lnTmpStru + 7 ,18]
*C037816,1 MHM [End]

*C200582,1 ABD - [End]

*C200511,1 ABD - [End]

*-- Field hold the style group data.

lnI = 0
lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'StyGrop'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

*-- Field hold the style group data.
lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'StyLoc'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

*!*	laTmpStru[lnTmpStru + 3 ,1] = 'Account'
*!*	laTmpStru[lnTmpStru + 3 ,2] = 'C'
*!*	laTmpStru[lnTmpStru + 3 ,3] = 5
*!*	laTmpStru[lnTmpStru + 3 ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'CGroupKey'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cDelivery'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cConslPikt'
laTmpStru[lnTmpStru + lnI ,2] = 'M'
laTmpStru[lnTmpStru + lnI ,3] = 10
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cStyMajor'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 19
laTmpStru[lnTmpStru + lnI ,4] = 0

*-- Field hold the style group data.
lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cLocation'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 10
laTmpStru[lnTmpStru + lnI ,4] = 0

*!*	laTmpStru[lnTmpStru + 9 ,1] = 'cWareCode'
*!*	laTmpStru[lnTmpStru + 9 ,2] = 'C'
*!*	laTmpStru[lnTmpStru + 9 ,3] = 6
*!*	laTmpStru[lnTmpStru + 9 ,4] = 0


FOR lnI = 1 TO ALEN(laTmpStru,1)-lnTmpStru
  STORE .F. TO laTmpStru[lnTmpStru+lnI,5],laTmpStru[lnTmpStru+lnI,6]
  STORE ''  TO laTmpStru[lnTmpStru+lnI,7],laTmpStru[lnTmpStru+lnI,8],laTmpStru[lnTmpStru+lnI,9],laTmpStru[lnTmpStru+lnI,10],laTmpStru[lnTmpStru+lnI,11],;
               laTmpStru[lnTmpStru+lnI,12],laTmpStru[lnTmpStru+lnI,13],laTmpStru[lnTmpStru+lnI,14],laTmpStru[lnTmpStru+lnI,15],laTmpStru[lnTmpStru+lnI,16]
  STORE 0  TO laTmpStru[lnTmpStru+lnI,17],laTmpStru[lnTmpStru+lnI,18]
ENDFOR  

lcAdStyGrp = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir + lcAdStyGrp) FROM ARRAY laTmpStru

INDEX ON ACCOUNT + STORE + cStyMajor TAG lcGroup1
INDEX ON ACCOUNT + STORE + Style TAG lcGroup2 ADDITIVE
IF lcRpPrGrS = "G"
  INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
ELSE
  INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT+ StyGrop + STYLE + StyLoc + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
ENDIF
SET ORDER TO lcGroup1

lcTmpGroup = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+lcTmpGroup) ( CGroupKey C(6) , Piktkt C(6))
INDEX ON CGroupKey  Tag (lcTmpGroup)

SELECT (lcTmpOrdL)
*T20071102.0018,7 TMI [Start] 
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
LOCATE
*T20071102.0018,7 TMI [End  ] 

SCAN
  IF LineNo = 0 .AND. Empty(Style)
    Loop
  ENDIF
  SCATTER MEMVAR MEMO
  SELECT (lcAdStyGrp)
  IF CUSTOMER.llDelivery .AND. SEEK(CUSTOMER.Account+Customer.Store+Style.cStyMajor,lcAdStyGrp)
    lcReplPikt = IIF(OCCURS(Piktkt.Piktkt,cConslPikt)>0,cConslPikt,cConslPikt + ','+ Piktkt.Piktkt)
    lcCurPiktk = Piktkt
    *--037816,2  mhm2004
    *REPLACE cConslPikt  WITH lcReplPikt
    REPLACE ALL cConslPikt  WITH lcReplPikt FOR Account+Store+LEFT(Style,8) = CUSTOMER.Account+Customer.Store+LEFT(Style.Style,8)
    *--037816,2  mhm2004
    SET ORDER TO lcGroup2
    IF SEEK(CUSTOMER.Account+Customer.Store+Style.Style)
      REPLACE Pik1        WITH Pik1 + M.Pik1,;
              Pik2        WITH Pik2 + M.Pik2,;
              Pik3        WITH Pik3 + M.Pik3,;
              Pik4        WITH Pik4 + M.Pik4,;
              Pik5        WITH Pik5 + M.Pik5,;
              Pik6        WITH Pik6 + M.Pik6,;
              Pik7        WITH Pik7 + M.Pik7,;
              Pik8        WITH Pik8 + M.Pik8
      *C037816,2 MHM 04/06/2004   as per mail from tony [Start]
      *Add piktkts to memofield in case of 
      *style color because we calculate our program on location for all color    
      REPLACE cConslPikt  WITH lcReplPikt
      *C037816,2 MHM 04/06/2004   [End]
    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE Account    WITH CUSTOMER.Account                ,;
              cDelivery  WITH IIF(CUSTOMER.llDelivery,'Y','N'),;
              cGroupkey  WITH 'zzzzzz'                        ,;
              StyGrop    WITH STYLE.CSTYGROUP                 ,;
              StyLoc     WITH STYLE.LOCATION                  ,;
              PIKTKT     WITH lcCurPiktk
      *: B607509,1 MHM 12/30/2004  as per mail from tony [Start]
      *Add piktkts to memofield in case of 
      *style color because we calculate our program on location for all color    
      REPLACE cConslPikt  WITH lcReplPikt 
      *: B607509,1 [End]
    ENDIF       
    SET ORDER TO lcGroup1
  ELSE
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE Account    WITH CUSTOMER.Account                ,;
            cDelivery  WITH IIF(CUSTOMER.llDelivery,'Y','N'),;
            cGroupkey  WITH 'zzzzzz'                        ,;
            StyGrop    WITH STYLE.CSTYGROUP                 ,;
            StyLoc     WITH STYLE.LOCATION                  ,;
            cStyMajor   WITH Style.cStyMajor                ,;
            cConslPikt WITH 'Picking Tickets:  '+ Piktkt.Piktkt
            
  ENDIF
  *C200582,1 ABD - [End]
 
 
ENDSCAN

*C200582,1 ABD - Set ORDER  to lcGroup the orginal order. [End]
SELECT (lcAdStyGrp)
SET ORDER TO lcGroup
*C200582,1 ABD - [End]

SELECT (lcTmpOrdL)
SET RELATION TO

lcTmpOrdL = lcAdStyGrp

*C200511,1 ABD - Update the temp file with the groupkey, this will help me to print correct
*C200511,1 ABD - data in case the customer note is yes. [Begin]
SELECT (lcTmpOrdL)
LOCATE
lnGroupKey = 1
lcOldPiktk = Piktkt
lcCurtpktk = ''
lcOldKey   = Account + cDelivery + Store
SCAN
  IF (lcOldKey # Account + cDelivery + Store) .OR. (lcCurtpktk # Piktkt .AND. cDelivery = 'N') .OR. EOF(lcTmpGroup)
    lnGroupKey = lnGroupKey + 1

    *-- Update the group file
    SELECT (lcTmpGroup)
    APPEND BLANK
    *B608946,1 WAM 07/23/2009 Update group key sequence in temporary file
    *REPLACE Piktkt WITH &lcTmpOrdL..Piktkt,;
            CGroupKey WITH ALLTRIM(STR(lnGroupKey))
    REPLACE Piktkt WITH &lcTmpOrdL..Piktkt,;
            CGroupKey WITH PADL(lnGroupKey,6,'0')
    *B608946,1 WAM 07/23/2009 (End)

    SELECT (lcTmpOrdL)
  ENDIF  
  *B608946,1 WAM 07/23/2009 Update group key sequence in temporary file
  *REPLACE CGroupKey WITH ALLTRIM(STR(lnGroupKey))
  REPLACE CGroupKey WITH PADL(lnGroupKey,6,'0')
  *B608946,1 WAM 07/23/2009 (End)
  
  IF lcOldPiktk # Piktkt .AND. lnOldGroup = lnGroupKey .OR. OCCURS(',',cConslPikt) > 0
    IF EOF(lcTmpGroup)
      SELECT (lcTmpGroup)
      APPEN BLANK
      *B608946,1 WAM 07/23/2009 Update group key sequence in temporary file
      *REPLACE Piktkt WITH &lcTmpOrdL..Piktkt,;
              CGroupKey WITH ALLTRIM(STR(lnGroupKey))
      REPLACE Piktkt WITH &lcTmpOrdL..Piktkt,;
              CGroupKey WITH PADL(lnGroupKey,6,'0')
      *B608946,1 WAM 07/23/2009 (End)
      SELECT (lcTmpOrdL)
   ELSE
      REPLACE &lcTmpGroup..Piktkt WITH '******'
    ENDIF
  ENDIF

  
  lcOldKey   = Account + cDelivery + Store
  STORE Piktkt TO lcOldPiktk , lcCurtpktk
  lnOldGroup = lnGroupKey
ENDSCAN

*C037816,1 MHM 04/06/2004 Collect Data to print in case of use bin location[Start]
IF llUseBin
 lcBnStyGrp = gfTempName()
 *--mhm123853
 lcReptSty = gfTempName()
 *--mhm123853
  =lfUseBin()
ENDIF
*C037816,1 MHM [End]


*-- ABD - remark the next few lines and make them up before collect the data.
*--Section Creat the index and the relations.
*SELECT (lcTmpOrdL)
**C200466,1 BWA 12/17/2002 Make the index due to the sort of the user.[START]
*IF lcRpPrGrS = "G"
*  INDEX ON PIKTKT + ORDER + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup
*ELSE
*  INDEX ON PIKTKT + ORDER + StyLoc + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup
*ENDIF
**C200466,1 BWA 12/17/2002.[END]

*T20071102.0018,10/C200876 TMI 05/28/2008 [Start] 
lcSvCentSet = SET("Century")
SET CENTURY ON
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ] 

SELECT (lcTmpOrdL)
*T20071102.0018,7 TMI [Start] 
SET RELATION TO 
SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET RELATION TO CGroupKey INTO (lcTmpGroup) ADDITIVE
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SELECT (lcTmpOrdL)
*T20071102.0018,10/C200876 TMI 10/05/2008 [Start] 
IF !'INTO '+lcORDHDR $ SET('RELATION')
  SET RELATION TO cordtype+order INTO &lcORDHDR addit
ENDIF  
*T20071102.0018,10/C200876 TMI 10/05/2008 [End  ] 
LOCATE

DO gfDispRe WITH EVAL('lcFormName')

*--Restore the Original name.
lcTmpOrdL = lcOldName

SELECT (lcAdStyGrp)
SET RELATION TO

SELECT(lcAlasDl)
SET ORDER TO TAG &lcOrdDl
=SEEK(lcKeyDL)

*--Function to clear the Temp. file.
=lfBasToClr(lcAdStyGrp , 'F')

*T20071102.0018,10/C200876 TMI 05/28/2008 [Start] restore century setting
SET CENTURY &lcSvCentSet
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ] 

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/03/2002
*! Purpose   : deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*--BAS
*!*************************************************************
*! Name      : LFDelPhon
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/16/2002
*! Purpose   : Function to delete the phone number.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =LFDelPhon()
*!*************************************************************
FUNCTION LFDelPhon
PARAMETER lcReturn

FOR LnPh = 1 TO ALEN(laSoldTo,1)
  IF 'Phone#' $ laSoldTo[LnPh]
    laSoldTo[LnPh,1] = SPACE(0)
  ENDIF
ENDFOR

FOR LnPh = 1 TO ALEN(laShipTo,1)
  IF 'Phone#' $ laShipTo[LnPh]
    laShipTo[LnPh,1] = SPACE(0)
  ENDIF
ENDFOR

RETURN ""
*--End of LFDelPhon

*!*************************************************************
*! Name      : lfUseBin
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/16/2002
*! Purpose   : Function follow concept of Using Bin Locations.
*!*************************************************************
*! Called from : ALPKLSDB.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUseBin()
*!*************************************************************
*!*C037816,1 MHM 04/06/2004 Collect Data to print 
FUNCTION lfUseBin

IF !USED('PKBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

=lfCrtTmp()

SELECT (lcTmpOrdL)
SCAN
  SCATT MEMVAR MEMO

  *: B607509,1 MHM 12/30/2004 [Start]
  IF !(OCCURS(',',m.cConslPikt)>0)
    IF SEEK(Account + Store +  PIKTKT + STYLE,lcReptSty)
      LOOP
    ELSE
      INSERT INTO (lcReptSty) FROM MEMVAR
    ENDIF
  ENDIF  
  *: B607509,1 MHM 12/30/2004 [Start]
  
  lcBNPIK = SUBSTR(m.cConslPikt,AT(':',m.cConslPikt)+1,LEN(m.cConslPikt))
  lcBNPIK = ALLTRIM(lcBNPIK)
  FOR lnCount = 1 TO OCCURS(',',lcBNPIK)+1
    IF OCCURS(',',lcBNPIK) >0
      lcBnPikTkt = SUBSTR(lcBNPIK,1,AT(',',lcBNPIK )-1)
      lcBNPIK = SUBSTR(lcBNPIK,AT(',',lcBNPIK)+1,LEN(lcBNPIK))
    ELSE
      IF !EMPTY(lcBNPIK)
        lcBnPikTkt = ALLTRIM(lcBNPIK)
      ELSE
        lcBnPikTkt = PIKTKT
      ENDIF  
    ENDIF
    SELECT PKBINLOC  
    IF gfSEEK(lcBnPikTkt,'PKBINLOC')
      *SCAN REST WHILE PikTkt+cWareCode+cLocation+Style = ALLTRIM(lcBnPikTkt) FOR (PKBINLOC.STYLE = &lcTmpOrdL..Style)
      SCAN REST WHILE PikTkt+cWareCode+cLocation+Style = ALLTRIM(lcBnPikTkt) 
        IF !(PKBINLOC.STYLE = &lcTmpOrdL..Style)
          LOOP
        ENDIF

        *: B607509,1 MHM 12/30/2004 Problem in printing multi Picking Ticket [Start]
        *m.Pik1 = Qty1
        *m.Pik2 = Qty2
        *m.Pik3 = Qty3
        *m.Pik4 = Qty4
        *m.Pik5 = Qty5
        *m.Pik6 = Qty6
        *m.Pik7 = Qty7
        *m.Pik8 = Qty8
        FOR lncI = 1 TO 8 
          lccI = ALLTRIM(STR(lncI,1))
          *IF !EMPTY(&lcTmpOrdL..Qty&lccI)
            m.Pik&lccI  =  Qty&lccI 
          *ENDIF  
        ENDFOR
        *: B607509,1 MHM  [End]
        
        m.cLocation = cLocation
        *Add ware code to seek 
        m.cWareCode = cWareCode
        IF OCCURS(',',m.cConslPikt)>0
          llCurseek =SEEK(m.Account+m.Store+cWareCode+cLocation+Style,lcBnStyGrp)
        ELSE
          llCurseek =SEEK(m.Account+m.Store+cWareCode+cLocation+Style+PikTkt,lcBnStyGrp)
        ENDIF
        IF !llCurseek 
          SELECT (lcBnStyGrp)
          APPEND BLANK
          GATHER MEMVAR MEMO
        ELSE
          SELECT (lcBnStyGrp)
          REPLACE Pik1 WITH Pik1 + m.Pik1,;
                  Pik2 WITH Pik2 + m.Pik2;
                  Pik3 WITH Pik3 + m.Pik3;
                  Pik4 WITH Pik4 + m.Pik4;
                  Pik5 WITH Pik5 + m.Pik5;
                  Pik6 WITH Pik6 + m.Pik6;
                  Pik7 WITH Pik7 + m.Pik7;
                  Pik8 WITH Pik8 + m.Pik8
        ENDIF 
      ENDSCAN
    ELSE
      SELECT (lcBnStyGrp)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF  
  ENDFOR
ENDSCAN

lcTmpOrdL = lcBnStyGrp 
SELECT (lcTmpOrdL)

*: B607509,1 MHM 12/30/2004 Change Index[Start]
*SET ORDER TO lcGroup2
SET ORDER TO lcGroup
*: B607509,1 MHM  [End]

LOCATE
*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/16/2002
*! Purpose   : Function follow concept of Using Bin Locations.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCrtTmp()
*!*************************************************************
*C037816,1 MHM 04/06/2004 Collect Data to print 
FUNCTION lfCrtTmp

SELECT (lcTmpOrdL)

=AFIELDS(laTmpStru)
CREATE TABLE (oAriaApplication.WorkDir + lcBnStyGrp) FROM ARRAY laTmpStru

INDEX ON cLocation+ACCOUNT + STORE + cStyMajor TAG lcGroup1
INDEX ON cLocation+ACCOUNT + STORE + Style TAG lcGroup2 ADDITIVE
IF lcRpPrGrS = "G"
  *: B607509,1 MHM 12/30/2004 Change the index to work in PikTkt Level [Start]
  *INDEX ON cLocation+Account + Store + cDelivery + cGroupkey + PIKTKT + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
  INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT + cLocation+ StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
  *: B607509,1 MHM [End]
ELSE
  *: B607509,1 MHM 12/30/2004 Change the index to work in PikTkt Level [Start]
  *INDEX ON cLocation+Account + Store + cDelivery + cGroupkey + PIKTKT+ StyGrop + STYLE + StyLoc + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
  INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT + cLocation + StyGrop + STYLE + StyLoc + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
  *: B607509,1 MHM [End]
ENDIF

INDEX ON Account + Store + cWareCode+cLocation+Style+PIKTKT TAG lcGroup3
SET ORDER TO lcGroup3

*: B607509,1 MHM 
CREATE TABLE (oAriaApplication.WorkDir + lcReptSty) FROM ARRAY laTmpStru
INDEX ON Account + Store +  PIKTKT + STYLE  TAG lcReptSty ADDITIVE
*: B607509,1 MHM [END]

*:***************************************************************************
*:***************************************************************************
FUNCTION  LFGTTOTPK
PARAMETER LNRETURN
LNRETURN = 0
LNALIAS = ALIAS()
IF PIKTKT.STATUS='O'
   IF SEEK('O'+&lcTmpOrdL..ORDER,'ORDLINE')
      SELECT ORDLINE
      SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + &lcTmpOrdL..Order
         IF &lcTmpOrdL..Piktkt <> OrdLine.Piktkt
            LOOP
         ENDIF
         LNRETURN = LNRETURN+TOTPIK
      ENDSCAN
   ENDIF
ELSE
   IF SEEK(&lcTmpOrdL..PIKTKT,'PIKLINE')
      SELECT PIKLINE
      SCAN REST WHILE piktkt+order+STR(lineno,6) = &lcTmpOrdL..Piktkt
         LNRETURN = LNRETURN+TOTPIK
      ENDSCAN
   ENDIF
ENDIF
SELECT (LNALIAS)
RETURN LNRETURN
*


*:**************************************************************************
*:* Name        : lfchkShpAd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/24/2008
*:* Purpose     : check the shipping address
*:***************************************************************************
*:* Called from : frx
*:***************************************************************************
FUNCTION lfchkShpAd
IF !&lcORDHDR..Alt_ShpTo
  lcShpTName  = IIF(!EMPTY(lcShpTName  ),lcShpTName  ,CUSTOMER.STNAME)
  laShipTo[1] = IIF(!EMPTY(laShipTo[1] ),laShipTo[1] ,CUSTOMER.CADDRESS1)
  laShipTo[2] = IIF(!EMPTY(laShipTo[2] ),laShipTo[2] ,CUSTOMER.CADDRESS2)
  laShipTo[3] = IIF(!EMPTY(laShipTo[3] ),laShipTo[3] ,CUSTOMER.CADDRESS3)
  laShipTo[4] = IIF(!EMPTY(laShipTo[4] ),laShipTo[4] ,CUSTOMER.CADDRESS4)
  laShipTo[5] = IIF(!EMPTY(laShipTo[5] ),laShipTo[5] ,CUSTOMER.CADDRESS5)
ENDIF  

lcRet = ALLTRIM(lcShpTName) + ;
        IIF(!EMPTY(laShipTo[1]),", ",'') + ALLTRIM(laShipTo[1]) + ;
        IIF(!EMPTY(laShipTo[2]),", ",'') + ALLTRIM(laShipTo[2]) + ;
        IIF(!EMPTY(laShipTo[3]),", ",'') + ALLTRIM(laShipTo[3]) + ;
        IIF(!EMPTY(laShipTo[4]),", ",'') + ALLTRIM(laShipTo[4]) + ;
        IIF(!EMPTY(laShipTo[5]),", ",'') + ALLTRIM(laShipTo[5])
RETURN lcRet
*-- end of lfchkShpAd.