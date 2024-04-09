*:***************************************************************************
*: Program file  : ALPKTKDL.PRG
*: Program desc. : CUSTOMIZED PICKING TICKET Form FOR DAVID LUKE LTD.
*: Date          : 12/03/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200429
*:
*:***************************************************************************
*: Calls : 
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO ALPKTKDL
*:***************************************************************************
*: Modification:
*: C200466,1 BWA 12/17/2002 Add new filter to sort by group or location.
*: C200511,1 ABD 04/15/2003 Add an user definable field to the customer file
*: C200511,1 ABD            And print the piktkt collected based on this custom
*: C200511,1 ABD            Field if yes or No.
*: C200582,1 ABD 07/29/2003 Amlagamate the quantities on one line for the same 
*: C200582,1 ABD            Account/Store/Style. 
*: C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*: B607509,1 MHM 12/30/2004 Problem in printing multi Picking Ticket 
*: B130967,1 MHM 02/07/2006 Add check for Total PIKTK 
*****************************************************************************

*--In case the user select YES in "Product Group Sequence".
PRIVATE lcAlasDl , lcOrdDl , lcKeyDL , lcOldName
lcAlasDl = SELECT(0)
lcOrdDl  = ORDER()
lcKeyDL  = EVAL(KEY())
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
DIMENSION laTmpStru[lnTmpStru + 9 ,4]
*C037816,1 MHM [End]

*C200582,1 ABD - [End]

*C200511,1 ABD - [End]

*-- Field hold the style group data.
laTmpStru[lnTmpStru + 1 ,1] = 'StyGrop'
laTmpStru[lnTmpStru + 1 ,2] = 'C'
laTmpStru[lnTmpStru + 1 ,3] = 6
laTmpStru[lnTmpStru + 1 ,4] = 0

*C200466,1 BWA 12/17/2002 Add new field for the location.[START]
*-- Field hold the style group data.
laTmpStru[lnTmpStru + 2 ,1] = 'StyLoc'
laTmpStru[lnTmpStru + 2 ,2] = 'C'
laTmpStru[lnTmpStru + 2 ,3] = 6
laTmpStru[lnTmpStru + 2 ,4] = 0

*C200466,1 BWA 12/17/2002.[END]

*C200511,1 ABD - Add 3 new fields to the temp file and add them into the Index. [Begin]
*lcAdStyGrp = gfTempName()
*CREATE TABLE (gcWorkDir + lcAdStyGrp) FROM ARRAY laTmpStru

laTmpStru[lnTmpStru + 3 ,1] = 'Account'
laTmpStru[lnTmpStru + 3 ,2] = 'C'
laTmpStru[lnTmpStru + 3 ,3] = 5
laTmpStru[lnTmpStru + 3 ,4] = 0

laTmpStru[lnTmpStru + 4 ,1] = 'CGroupKey'
laTmpStru[lnTmpStru + 4 ,2] = 'C'
laTmpStru[lnTmpStru + 4 ,3] = 6
laTmpStru[lnTmpStru + 4 ,4] = 0

laTmpStru[lnTmpStru + 5 ,1] = 'cDelivery'
laTmpStru[lnTmpStru + 5 ,2] = 'C'
laTmpStru[lnTmpStru + 5 ,3] = 6
laTmpStru[lnTmpStru + 5 ,4] = 0

*C200582,1 ABD - Add new memo field to transaction file , this field will hold the Amlagamate Piktkt. [Begin]
laTmpStru[lnTmpStru + 6 ,1] = 'cConslPikt'
laTmpStru[lnTmpStru + 6 ,2] = 'M'
laTmpStru[lnTmpStru + 6 ,3] = 10
laTmpStru[lnTmpStru + 6 ,4] = 0

laTmpStru[lnTmpStru + 7 ,1] = 'cStyMajor'
laTmpStru[lnTmpStru + 7 ,2] = 'C'
laTmpStru[lnTmpStru + 7 ,3] = 19
laTmpStru[lnTmpStru + 7 ,4] = 0

*C037816,1 MHM 04/06/2004 Increase Array with Location Field[Start]
*-- Field hold the style group data.
laTmpStru[lnTmpStru + 8 ,1] = 'cLocation'
laTmpStru[lnTmpStru + 8 ,2] = 'C'
laTmpStru[lnTmpStru + 8 ,3] = 10
laTmpStru[lnTmpStru + 8 ,4] = 0

laTmpStru[lnTmpStru + 9 ,1] = 'cWareCode'
laTmpStru[lnTmpStru + 9 ,2] = 'C'
laTmpStru[lnTmpStru + 9 ,3] = 6
laTmpStru[lnTmpStru + 9 ,4] = 0

*C037816,1 MHM [End]

*C200582,1 ABD - [End]

lcAdStyGrp = gfTempName()
CREATE TABLE (gcWorkDir + lcAdStyGrp) FROM ARRAY laTmpStru

*C200582,1 ABD - Change the sort to be by style group + Style in the 2 sort cases. [Begin]
*IF lcRpPrGrS = "G"
*INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT + ORDER + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup
*ELSE
*INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT + ORDER + StyLoc + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup
*ENDIF

INDEX ON ACCOUNT + STORE + cStyMajor TAG lcGroup1
INDEX ON ACCOUNT + STORE + Style TAG lcGroup2 ADDITIVE
IF lcRpPrGrS = "G"
  INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
ELSE
  INDEX ON Account + Store + cDelivery + cGroupkey + PIKTKT+ StyGrop + STYLE + StyLoc + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
ENDIF
SET ORDER TO lcGroup1
*C200582,1 ABD - [End]

lcTmpGroup = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpGroup) ( CGroupKey C(6) , Piktkt C(6))
INDEX ON CGroupKey  Tag (lcTmpGroup)
*C200511,1 ABD - [End]
SELECT (lcTmpOrdL)

SCAN
  IF LineNo = 0 .AND. Empty(Style)
    Loop
  ENDIF
  SCATTER MEMVAR MEMO
  SELECT (lcAdStyGrp)
  
  *C200582,1 ABD - Remark the next line and and add code to update the new 
  *C200582,1 ABD - Memofield and Amlagamate the quantities on one line for the same 
  *C200582,1 ABD - Account/Store/Style. [End]
  *APPEND BLANK
  *GATHER MEMVAR MEMO
  **ash (as per tony in an email on 12/24/2002)
  **  REPLACE &lcAdStyGrp..StyGrop WITH STYLE.CSTYGROUP ,;
  *        &lcAdStyGrp..StyLoc WITH STYLE.CDEFWARE
  *REPLACE &lcAdStyGrp..StyGrop WITH STYLE.CSTYGROUP ,;
  *        &lcAdStyGrp..StyLoc WITH STYLE.LOCATION
  **C200511,1 ABD - update the 3 new fields to the temp file  [Begin]
  *REPLACE Account   WITH CUSTOMER.Account,;
  *        cDelivery WITH IIF(CUSTOMER.llDelivery,'Y','N'),;
  *        cGroupkey WITH 'zzzzzz'
  **C200511,1 ABD - [End]

  IF CUSTOMER.llDelivery .AND. SEEK(CUSTOMER.Account+Customer.Store+Style.cStyMajor)
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
    REPLACE Piktkt WITH &lcTmpOrdL..Piktkt,;
            CGroupKey WITH ALLTRIM(STR(lnGroupKey))
    SELECT (lcTmpOrdL)
  ENDIF  
  REPLACE CGroupKey WITH ALLTRIM(STR(lnGroupKey))
  
  IF lcOldPiktk # Piktkt .AND. lnOldGroup = lnGroupKey .OR. OCCURS(',',cConslPikt) > 0
    IF EOF(lcTmpGroup)
      SELECT (lcTmpGroup)
      APPEN BLANK
      REPLACE Piktkt WITH &lcTmpOrdL..Piktkt,;
              CGroupKey WITH ALLTRIM(STR(lnGroupKey))
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

SELECT (lcTmpOrdL)
*C200511,1 ABD - [End]

SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
*C200511,1 ABD - set relation bettwen the temp file and group file. [Begin]
SET RELATION TO CGroupKey INTO (lcTmpGroup) ADDITIVE
*C200511,1 ABD - [End]

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
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
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
    IF SEEK(lcBnPikTkt)
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

CREATE TABLE (gcWorkDir + lcBnStyGrp) FROM ARRAY laTmpStru

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
CREATE TABLE (gcWorkDir + lcReptSty) FROM ARRAY laTmpStru
INDEX ON Account + Store +  PIKTKT + STYLE  TAG lcReptSty ADDITIVE
*: B607509,1 MHM [END]



*!*************************************************************
*! Name      : lfGtTotpk
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/07/2006
*! Purpose   : Get Total Piktkt from Pikline file
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDelNote()
*!*************************************************************
*B130967
FUNCTION lfGtTotpk
PARAMETERS lnReturn
lnReturn =0
lnAlias = ALIAS()

IF PIKTKT.STATUS = 'O'
  IF SEEK('O'+&lcTmpOrdL..ORDER,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + &lcTmpOrdL..Order
      IF &lcTmpOrdL..Piktkt <> OrdLine.Piktkt
        LOOP
      ENDIF
      lnReturn = lnReturn+ TotPik 
    ENDSCAN
  ENDIF
ELSE
  IF SEEK(&lcTmpOrdL..PIKTKT,'PIKLINE')
    SELECT PIKLINE
    SCAN REST WHILE piktkt+order+STR(lineno,6) = &lcTmpOrdL..Piktkt
      lnReturn = lnReturn+ TotPik 
    ENDSCAN
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN lnReturn

