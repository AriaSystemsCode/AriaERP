*:***************************************************************************
*: Program file       : ALPBOLIK.PRG
*: Program description: Custom Bill of Lading Report for IKK00
*: Module             : Sales Order Allocation (AL)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: C201554.EXE
*: Date               : 02/13/2013
*:***************************************************************************
*Modifications:
*!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023]
*:***************************************************************************

LOCAL lcOldAlias
lcOldAlias = SELECT(0)

=lfCrtTmp()
=lfCollData()
*!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][Start]
*=lfAdjCRSet()
*!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][End]

IF USED('TMPSHIP')
  USE IN TMPSHIP
ENDIF
SELECT (lcOldAlias)

*!*************************************************************
*! Name      : lfShipTo
*! Developer : Saber A.Razek
*! Date      : 02/10/2013
*! Purpose   : Function to get Ship To info
*!*************************************************************
FUNCTION lfShipTo

PRIVATE laShipTo
DECLARE laShipTo[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laShipTo[1,1] = 'lcRpShp1'        && 1st. line Variable
laShipTo[2,1] = 'lcRpShp2'        && 2nd. line Variable
laShipTo[3,1] = 'lcRpShp3'        && 2nd. line Variable

laShipTo[1,2] = 30                && Line length

*IF EMPTY(lcRpShp1) .AND. EMPTY(lcRpShp1) .AND. EMPTY(lcRpShp1)
  =gfOptMsg('laShipTo')
*ENDIF
SET MEMOWIDTH TO 30              && the length of the memo field.


*!*****************************************************************************************
*! Name      : RefreshShpTo
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 11:18:46 PM
*! Purpose   : Refresh the optional message area
*!*****************************************************************************************
FUNCTION RefreshShpTo
  IF EMPTY(lcRpShp1) .AND. EMPTY(lcRpShp2) .AND. EMPTY(lcRpShp3)
    RETURN ""
  ELSE 
    RETURN ALLTRIM(lcRpShp1) + IIF(EMPTY(lcRpShp2),"",", ") +;
           ALLTRIM(lcRpShp2) + IIF(EMPTY(lcRpShp3),"",", ") +;
           ALLTRIM(lcRpShp3)
  ENDIF 
ENDFUNC 

*!*************************************************************
*! Name      : lfCrtTmp
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/10/2013
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
FUNCTION lfCrtTmp

*!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][Start]
*!*	LOCAL lnALen
*!*	lnALen = 0
*!*	DIMENSION laFields(9, 18)

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'BOL_NO'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 6
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'ADDRESS1'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'ADDRESS2'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'ADDRESS3'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'SPCINTL1'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'SPCINTL2'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'SPCINTL3'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'SPCINTL4'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	lnALen = lnALen + 1
*!*	laFields(lnALen, 1) = 'SPCINTL5'
*!*	laFields(lnALen, 2) = 'C'
*!*	laFields(lnALen, 3) = 30
*!*	laFields(lnALen, 4) = 0

*!*	FOR lnCount = 1 TO ALEN(laFields, 1)
*!*	  STORE '' TO laFields[lnCount,7], laFields[lnCount,8], laFields[lnCount,9],;
*!*	    laFields[lnCount,10],laFields[lnCount,11],laFields[lnCount,12],;
*!*	    laFields[lnCount,13],laFields[lnCount,14],laFields[lnCount,15],;
*!*	    laFields[lnCount,16]
*!*	  STORE 0  TO laFields[lnCount,17],laFields[lnCount,18]
*!*	ENDFOR 

*!*	CREATE TABLE (oAriaApplication.WorkDir+lcRepDir+'\tmpShip') FROM ARRAY laFields

*!*	INDEX ON BOL_NO TAG BOL_NO

ALTER TABLE BOLHDR ADD COLUMN ADDRESS1 C(30)
ALTER TABLE BOLHDR ADD COLUMN ADDRESS2 C(30)
ALTER TABLE BOLHDR ADD COLUMN ADDRESS3 C(30)
ALTER TABLE BOLHDR ADD COLUMN SPCINTL1 C(30)
ALTER TABLE BOLHDR ADD COLUMN SPCINTL2 C(30)
ALTER TABLE BOLHDR ADD COLUMN SPCINTL3 C(30)
ALTER TABLE BOLHDR ADD COLUMN SPCINTL4 C(30)
ALTER TABLE BOLHDR ADD COLUMN SPCINTL5 C(30)
*!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][End]

ENDFUNC


*!*************************************************************
*! Name      : lfCollData
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/10/2013
*! Purpose   : Procedure to make replenishment table have 14 size per line
*!*************************************************************
PROCEDURE lfCollData

*!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][Start]
*!*	SELECT BolHdr
*!*	SCAN
*!*	  SCATTER MEMO MEMVAR
*!*	  IF EMPTY(lcRpShp1) .AND. EMPTY(lcRpShp2) .AND. EMPTY(lcRpShp3)
*!*	    m.ADDRESS1 = STADD1
*!*	    m.ADDRESS2 = STADD2
*!*	    m.ADDRESS3 = STADD3
*!*	  ELSE 
*!*	    m.ADDRESS1 = lcRpShp1
*!*	    m.ADDRESS2 = lcRpShp2
*!*	    m.ADDRESS3 = lcRpShp3
*!*	  ENDIF
*!*	  
*!*	  m.SPCINTL1 = MLINE(mSpc_Inst, 1)
*!*	  m.SPCINTL2 = MLINE(mSpc_Inst, 2)
*!*	  m.SPCINTL3 = MLINE(mSpc_Inst, 3)
*!*	  m.SPCINTL4 = MLINE(mSpc_Inst, 4)
*!*	  m.SPCINTL5 = MLINE(mSpc_Inst, 5)
*!*	  
*!*	  SELECT tmpShip
*!*	  APPEND BLANK
*!*	  GATHER MEMO MEMVAR  
*!*	ENDSCAN

LOCAL llEmpRpShp
SELECT BOLHDR
SCAN
  llEmpRpShp = EMPTY(lcRpShp1) .AND. EMPTY(lcRpShp2) .AND. EMPTY(lcRpShp3)
  REPLACE BOLHDR.ADDRESS1 WITH IIF(llEmpRpShp, STADD1, lcRpShp1),;
          BOLHDR.ADDRESS2 WITH IIF(llEmpRpShp, STADD2, lcRpShp2),;
          BOLHDR.ADDRESS3 WITH IIF(llEmpRpShp, STADD3, lcRpShp3),;
          BOLHDR.SPCINTL1 WITH MLINE(mSpc_Inst, 1),;
          BOLHDR.SPCINTL2 WITH MLINE(mSpc_Inst, 2),;
          BOLHDR.SPCINTL3 WITH MLINE(mSpc_Inst, 3),;
          BOLHDR.SPCINTL4 WITH MLINE(mSpc_Inst, 4),;
          BOLHDR.SPCINTL5 WITH MLINE(mSpc_Inst, 5)
ENDSCAN
*!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][End]

ENDPROC


***************************************************************
*! Name      : lfAdjCRSet
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/10/2013
*! Purpose   : To set the report data files and parameters
*!*************************************************************
PROCEDURE lfAdjCRSet

DIMENSION loOgScroll.laCRTables[3]

loOgScroll.laCRTables[3] = oAriaApplication.WorkDir + lcRepDir + '\tmpShip.DBF'

ENDPROC