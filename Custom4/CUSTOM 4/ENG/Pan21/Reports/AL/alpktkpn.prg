*:***************************************************************************
*: Program file  : ALPKTKPN.PRG
*: Program desc. : CUSTOMIZED PICKING TICKET Form FOR PANACH.
*: Date          : 01/10/2007
*: System        : ARIA4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : AYMAN MAHMOUD AHMED(AYM)
*: Tracking NO   : C200732
*: TICKET NO     : T20061108.0007
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
*Modifications:
*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location [T20061108.0007]
*:***************************************************************************

*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location[Start]
SET RELATION TO 
SET RELATION TO Order + PikTkt INTO &lcTmpOrdH

SET RELATION TO PikTkt INTO &lcPiktktTemp ADDITIVE
SET RELATION TO 'O' + Order INTO &lcOrdHdr ADDITIVE

*--IF We are to Print Order Lines Note Pad
IF llRpOrdLNt
SET RELATION TO 'O' + Order + STR(LineNo,6) INTO &lcOrdLnTmp ADDITIVE
ENDIF    && End of IF

SET RELATION TO Style INTO &lcStyleFile ADDITIVE
SET RELATION TO 'S' + Scale INTO &lcScaleFile ADDITIVE
SELECT(lcPiktktTemp)
SET RELATION TO cWareCode INTO &lcWareHous
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                'S' + Account + Store) INTO &lcCustomer ADDITIVE
*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location[End]



private LCALASDL, LCORDDL, LCKEYDL, LCOLDNAME
LCALASDL = SELECT(0)
LCORDDL = ORDER()
LCKEYDL = EVALUATE(KEY())
LLALPKTK = .F.
SELECT  (LCTMPORDL)
LCOLDNAME = LCTMPORDL
= AFIELDS(LATMPSTRU)
LNTMPSTRU = ALEN(LATMPSTRU,1)
DIMENSION  LATMPSTRU[ LNTMPSTRU+16, 18]
LATMPSTRU[ LNTMPSTRU+1, 1] = 'StyGrop'
LATMPSTRU[ LNTMPSTRU+1, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+1, 3] = 6
LATMPSTRU[ LNTMPSTRU+1, 4] = 0
LATMPSTRU[ LNTMPSTRU+2, 1] = 'StyLoc'
LATMPSTRU[ LNTMPSTRU+2, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+2, 3] = 6
LATMPSTRU[ LNTMPSTRU+2, 4] = 0
LATMPSTRU[ LNTMPSTRU+3, 1] = 'Account11'
LATMPSTRU[ LNTMPSTRU+3, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+3, 3] = 5
LATMPSTRU[ LNTMPSTRU+3, 4] = 0
LATMPSTRU[ LNTMPSTRU+4, 1] = 'CGroupKey'
LATMPSTRU[ LNTMPSTRU+4, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+4, 3] = 6
LATMPSTRU[ LNTMPSTRU+4, 4] = 0
LATMPSTRU[ LNTMPSTRU+5, 1] = 'cDelivery'
LATMPSTRU[ LNTMPSTRU+5, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+5, 3] = 6
LATMPSTRU[ LNTMPSTRU+5, 4] = 0
LATMPSTRU[ LNTMPSTRU+6, 1] = 'cConslPikt'
LATMPSTRU[ LNTMPSTRU+6, 2] = 'M'
LATMPSTRU[ LNTMPSTRU+6, 3] = 10
LATMPSTRU[ LNTMPSTRU+6, 4] = 0
LATMPSTRU[ LNTMPSTRU+7, 1] = 'cStyMajor'
LATMPSTRU[ LNTMPSTRU+7, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+7, 3] = 19
LATMPSTRU[ LNTMPSTRU+7, 4] = 0
LATMPSTRU[ LNTMPSTRU+8, 1] = 'cWareCode1'
LATMPSTRU[ LNTMPSTRU+8, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+8, 3] = 6
LATMPSTRU[ LNTMPSTRU+8, 4] = 0
LATMPSTRU[ LNTMPSTRU+9, 1] = 'cLocation1'
LATMPSTRU[ LNTMPSTRU+9, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+9, 3] = 10
LATMPSTRU[ LNTMPSTRU+9, 4] = 0
LATMPSTRU[ LNTMPSTRU+10, 1] = 'cLocation2'
LATMPSTRU[ LNTMPSTRU+10, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+10, 3] = 10
LATMPSTRU[ LNTMPSTRU+10, 4] = 0
LATMPSTRU[ LNTMPSTRU+11, 1] = 'cLocation3'
LATMPSTRU[ LNTMPSTRU+11, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+11, 3] = 10
LATMPSTRU[ LNTMPSTRU+11, 4] = 0
LATMPSTRU[ LNTMPSTRU+12, 1] = 'cLocation4'
LATMPSTRU[ LNTMPSTRU+12, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+12, 3] = 10
LATMPSTRU[ LNTMPSTRU+12, 4] = 0
LATMPSTRU[ LNTMPSTRU+13, 1] = 'cLocation5'
LATMPSTRU[ LNTMPSTRU+13, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+13, 3] = 10
LATMPSTRU[ LNTMPSTRU+13, 4] = 0
LATMPSTRU[ LNTMPSTRU+14, 1] = 'cLocation6'
LATMPSTRU[ LNTMPSTRU+14, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+14, 3] = 10
LATMPSTRU[ LNTMPSTRU+14, 4] = 0
LATMPSTRU[ LNTMPSTRU+15, 1] = 'cLocation7'
LATMPSTRU[ LNTMPSTRU+15, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+15, 3] = 10
LATMPSTRU[ LNTMPSTRU+15, 4] = 0
LATMPSTRU[ LNTMPSTRU+16, 1] = 'cLocation8'
LATMPSTRU[ LNTMPSTRU+16, 2] = 'C'
LATMPSTRU[ LNTMPSTRU+16, 3] = 10
LATMPSTRU[ LNTMPSTRU+16, 4] = 0


FOR lnI=LNTMPSTRU+1 TO LNTMPSTRU+16
  FOR lnK=7 TO 16
    STORE '' TO LATMPSTRU[ lnI, lnK] 
  ENDFOR 
  STORE 0 TO LATMPSTRU[ lnI, 17] 
  STORE 0 TO LATMPSTRU[ lnI, 18] 
ENDFOR 
LCADSTYGRP = loogscroll.GFTEMPNAME()

CREATE  DBF (oAriaApplication.WorkDir+LCADSTYGRP) FROM ARRAY LATMPSTRU
INDEX  ON ACCOUNT+STORE+CSTYMAJOR TAG LCGROUP1
INDEX  ON ACCOUNT+STORE+STYLE TAG LCGROUP2 ADDITIVE
	
IF  LCRPPRGRS='G'
   INDEX  ON ACCOUNT+STORE+CDELIVERY+CGROUPKEY+PIKTKT+STYGROP+STYLE+CGRUPDETAL+STR(LINENO,6) TAG LCGROUP ADDITIVE
ELSE 
   *C200732,2 MMT 05/28/2007 fix bug of wrong sort by location[Start]
   *INDEX  ON ACCOUNT+STORE+CDELIVERY+CGROUPKEY+PIKTKT+STYLOC+STYGROP+STYLE+CGRUPDETAL+STR(LINENO,6) TAG LCGROUP ADDITIVE
   INDEX  ON ACCOUNT+STORE+CDELIVERY+CGROUPKEY+PIKTKT+STYLOC+STYLE+CGRUPDETAL+STR(LINENO,6) TAG LCGROUP ADDITIVE
	*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location[End]
   
ENDIF 

SET  ORDER TO lcGroup1
LCTMPGROUP = loogscroll.GFTEMPNAME()
CREATE  DBF (oAriaApplication.WorkDir+LCTMPGROUP) ( CGROUPKEY C ( 6 ), PIKTKT C ( 6 ) )
INDEX  ON CGROUPKEY TAG (LCTMPGROUP)



SELECT  (LCTMPORDL)
SCAN 
   IF  LINENO=0 .and. EMPTY(STYLE)
      LOOP 
   ENDIF 
   SCATTER  MEMO MEMVAR
   WAIT WINDOW "Collecting Data for " +PIKTKT NOWAIT 
   SELECT  (LCADSTYGRP)
   IF  &lcCUSTOMER..LLDELIVERY .and. SEEK(&lcCUSTOMER..ACCOUNT+&lcCUSTOMER..STORE+&LCSTYLEFILE..CSTYMAJOR)
      LCREPLPIKT = IIF(OCCURS(&lcPiktktTemp..PIKTKT,CCONSLPIKT)>0,CCONSLPIKT,CCONSLPIKT+','+&lcPiktktTemp..PIKTKT)
      LCCURPIKTK = PIKTKT
      replace  CCONSLPIKT WITH LCREPLPIKT ALL FOR ACCOUNT+STORE+LEFT(STYLE,8)=&lcCUSTOMER..ACCOUNT+&lcCUSTOMER..STORE+LEFT(&LCSTYLEFILE..STYLE,8)
      SET  ORDER TO lcGroup2
      IF  SEEK(&lcCUSTOMER..ACCOUNT+&lcCUSTOMER..STORE+&LCSTYLEFILE..STYLE)
         replace  PIK1 WITH PIK1+M.PIK1, PIK2 WITH PIK2+M.PIK2, PIK3 WITH PIK3+M.PIK3, PIK4 WITH PIK4+M.PIK4, PIK5 WITH PIK5+M.PIK5, PIK6 WITH PIK6+M.PIK6, PIK7 WITH PIK7+M.PIK7, PIK8 WITH PIK8+M.PIK8
         replace  CCONSLPIKT WITH LCREPLPIKT
      ELSE 
         APPEND  BLANK
         GATHER  MEMVAR MEMO
         replace   ACCOUNT WITH &lcCUSTOMER..ACCOUNT, CDELIVERY WITH IIF(&lcCUSTOMER..LLDELIVERY,'Y','N'), CGROUPKEY WITH 'zzzzzz', STYGROP WITH &LCSTYLEFILE..CSTYGROUP, STYLOC WITH &LCSTYLEFILE..LOCATION, PIKTKT WITH LCCURPIKTK
         replace  CCONSLPIKT WITH LCREPLPIKT
      ENDIF 
      SET  ORDER TO lcGroup1
   ELSE 
      APPEND  BLANK
      m.cLocation1=&LCSTYLEFILE..CPRIMCLSS1 
      m.cLocation2=&LCSTYLEFILE..CPRIMCLSS2 
      m.cLocation3=&LCSTYLEFILE..CPRIMCLSS3 
      m.cLocation4=&LCSTYLEFILE..CPRIMCLSS4
      m.cLocation5=&LCSTYLEFILE..CPRIMCLSS5
      m.cLocation6=&LCSTYLEFILE..CPRIMCLSS6 
      m.cLocation7=&LCSTYLEFILE..CPRIMCLSS7 
      m.cLocation8=&LCSTYLEFILE..CPRIMCLSS8
      GATHER  MEMVAR MEMO
      replace  ACCOUNT WITH &lcCUSTOMER..ACCOUNT, CDELIVERY WITH IIF(&lcCUSTOMER..LLDELIVERY,'Y','N'), CGROUPKEY WITH 'zzzzzz', STYGROP WITH &LCSTYLEFILE..CSTYGROUP, STYLOC WITH &LCSTYLEFILE..LOCATION, CSTYMAJOR WITH &LCSTYLEFILE..CSTYMAJOR, CCONSLPIKT WITH 'Picking Tickets:  '+&lcPiktktTemp..PIKTKT
   ENDIF 
ENDSCAN 

SELECT  (LCADSTYGRP)
SET  ORDER TO lcGroup
SELECT  (LCTMPORDL)
SET  RELATION TO
LCTMPORDL = LCADSTYGRP
SELECT  (LCTMPORDL)
LOCATE 
LNGROUPKEY = 1
LCOLDPIKTK = PIKTKT
LCCURTPKTK = ''
LCOLDKEY = ACCOUNT+CDELIVERY+STORE
SCAN 
   IF  (LCOLDKEY#ACCOUNT+CDELIVERY+STORE) .or. (LCCURTPKTK#PIKTKT .and. CDELIVERY='N') .or. EOF(LCTMPGROUP)
      LNGROUPKEY = LNGROUPKEY+1
      SELECT  (LCTMPGROUP)
      APPEND  BLANK
      REPLACE Piktkt WITH &lcTmpOrdL..Piktkt, CGroupKey WITH ALLTRIM(STR(lnGroupKey))
      SELECT  (LCTMPORDL)
   ENDIF 
   replace CGROUPKEY WITH ALLTRIM(STR(LNGROUPKEY))
   if LCOLDPIKTK#PIKTKT .and. LNOLDGROUP=LNGROUPKEY .or. OCCURS(',',CCONSLPIKT)>0
      if EOF(LCTMPGROUP) 
         select (LCTMPGROUP)
         append BLANK
         REPLACE Piktkt WITH &lcTmpOrdL..Piktkt, CGroupKey WITH ALLTRIM(STR(lnGroupKey))
         select (LCTMPORDL)
      ELSE 
         REPLACE &lcTmpGroup..Piktkt WITH '******'
      ENDIF 
   ENDIF 
   LCOLDKEY = ACCOUNT+CDELIVERY+STORE
   store PIKTKT TO LCOLDPIKTK, LCCURTPKTK
   LNOLDGROUP = LNGROUPKEY
ENDSCAN 

  SELECT (lcTmpOrdL)
*!*	  SET ORDER TO TAG (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH
  SET RELATION TO Order + PikTkt INTO &lcTmpOrdH

  SET RELATION TO PikTkt INTO &lcPiktktTemp ADDITIVE
  SET RELATION TO 'O' + Order INTO &lcOrdHdr ADDITIVE
  set RELATION TO CGROUPKEY INTO (LCTMPGROUP) ADDITIVE

  *--IF We are to Print Order Lines Note Pad
  IF llRpOrdLNt
    SET RELATION TO 'O' + Order + STR(LineNo,6) INTO &lcOrdLnTmp ADDITIVE
  ENDIF    && End of IF

  SET RELATION TO Style INTO &lcStyleFile ADDITIVE
  SET RELATION TO 'S' + Scale INTO &lcScaleFile ADDITIVE
  SELECT(lcPiktktTemp)
  SET RELATION TO cWareCode INTO &lcWareHous
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO &lcCustomer ADDITIVE


SELECT  (LCTMPORDL)
LOCATE 

*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location[Start]
IF lcRpPrGrS = 'L'
  =lfLocDist()
ENDIF
SELECT  (LCTMPORDL)
LOCATE 
*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location[End]



do GFDISPRE WITH EVALUATE('lcFormName')
LCTMPORDL = LCOLDNAME
SELECT  (LCADSTYGRP)
SET  RELATION TO
SELECT  (LCALASDL)
SET ORDER TO TAG &lcOrdDl
= SEEK(LCKEYDL)
= LFBASTOCLR(LCADSTYGRP,'F')
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
PROCEDURE  LFBASTOCLR
parameter LCFILNAME, LCTYPFUN

if LCTYPFUN='F'
   if USED(LCFILNAME)
      select (LCFILNAME)
      use
   endif
else
   for LNLOP = 1 TO ALEN(LCFILNAME,1)
      if USED(LCFILNAME(LNLOP))
         select (LCFILNAME(LNLOP))
         use
      endif
   endfor
endif
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
function  LFDELPHON
PARAMETERS  LCRETURN
for LNPH = 1 TO ALEN(LASOLDTO,1)
   if 'Phone#'$LASOLDTO(LNPH)
      LASOLDTO[ LNPH, 1] = SPACE(0)
   endif
endfor
for LNPH = 1 TO ALEN(LASHIPTO,1)
   if 'Phone#'$LASHIPTO(LNPH)
      LASHIPTO[ LNPH, 1] = SPACE(0)
   endif
endfor
return ''


*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location[Start]
*:**************************************************************************
*:* Name        : lfLocDist
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/28/2007
*:* Purpose     : Distribute a style line with more than one bin to several lines
*:***************************************************************************
*C200736,1 TMI 
FUNCTION lfLocDist

*T20070116.0007,2 TMI [Start] 
DIMENSION laRecNo[1]
*T20070116.0007,2 TMI [End  ] 

*- collect the record numbers in one array
SELECT RECNO() FROM &lcTmpOrdL INTO ARRAY laRecNo

*T20070116.0007,2 TMI [Start] 
IF EMPTY(laRecNo[1])
  RETURN
ENDIF
*T20070116.0007,2 TMI [End  ] 

*- create a temp name to store bins
lcTmpLcSty = loogscroll.gfTempName()
  
*-- loop on the lcTmpOrdL file to distribute its lines
SELECT (lcTmpOrdL)
FOR lnRecno = 1 TO ALEN(laRecNo)
  SELECT &lcTmpOrdL
  GOTO (laRecNo[lnRecno])
  SCATTER MEMVAR MEMO
  CREATE CURSOR &lcTmpLcSty (LOC C(8),SZ C(1),PIK N(6))
  INDEX ON LOC TAG LOC
  FOR lnSz = 1 TO 8
    lcSz = STR(lnSz,1)
    INSERT INTO &lcTmpLcSty VALUES (&LCSTYLEFILE..CPRIMCLSS&lcSz,lcSz,&lcTmpOrdL..PIK&lcSz)
  ENDFOR
  DELETE FOR PIK = 0
  
  SELECT &lcTmpOrdL
  BLANK FIELDS PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8

  llAddLine = .F.    

  SELECT &lcTmpLcSty 
  LOCATE
  DO WHILE !EOF(lcTmpLcSty)
    IF llAddLine
      INSERT INTO &lcTmpOrdL FROM MEMVAR
      SELECT &lcTmpOrdL
      BLANK FIELDS PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8
      SELECT &lcTmpLcSty 
    ENDIF
      
    lcLoc = &lcTmpLcSty..LOC
    SCAN REST WHILE LOC = lcLoc
      lcSz = &lcTmpLcSty..SZ
      SELECT &lcTmpOrdL
      REPLACE PIK&lcSz WITH &lcTmpLcSty..PIK  ;
              STYLOC   WITH &lcTmpLcSty..LOC 
    ENDSCAN
    llAddLine = .T.
  ENDDO
  
  USE IN &lcTmpLcSty  
  
ENDFOR

SELECT &lcTmpOrdL

*-- end of lfLocDist.

*:**************************************************************************
*:* Name        : lfCountBins
*:* Developer   : Mariam Mazhar 
*:* Date        : 05/28/2007
*:* Purpose     : get the number of bins in each piktkt
*:***************************************************************************
FUNCTION lfCountBins
PARAMETERS lcPikNO

lcBinExp = ""
lnBinNum = 0

SELECT  (LCTMPORDL)
lnRecNum = RECNO()
SCAN FOR piktkt = lcPikNO
  FOR lnCntr = 1 TO 8
   lcCntr = STR(lnCntr,1)
   IF !EMPTY(EVAL(lcScaleFile+'.Sz'+lcCntr)) AND EVALUATE(LCTMPORDL+'.Pik'+lcCntr) <> 0 AND;
      !EMPTY(EVALUATE(LCTMPORDL+'.Clocation'+lcCntr)) AND !(EVALUATE(LCTMPORDL+'.Clocation'+lcCntr) $ lcBinExp) 
     lcBinExp = lcBinExp +'|'+ EVALUATE(LCTMPORDL+'.Clocation'+lcCntr)
	 lnBinNum = lnBinNum + 1   
   ENDIF 
  ENDFOR 
ENDSCAN 

SELECT  (LCTMPORDL)
IF BETWEEN(lnRecNum ,1,RECCOUNT())
  GO RECORD lnRecNum 
ENDIF

RETURN lnBinNum 
*C200732,2 MMT 05/28/2007 fix bug of wrong sort by location [End]


