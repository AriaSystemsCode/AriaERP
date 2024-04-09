*:***************************************************************************
*: Program file  : POSTYPDB.PRG copied from POSTYPMA.PRG
*: Program desc. : Get fabric pattern and vendor information	
*: For Report    : POSTYPDB
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO)
*: Developer     : Ken Li (KEN)
*: Customer      : Dorby
*:
*C101598 KEN, 08/05/99
*:***************************************************************************
*Modifications:
*C101823,1 ADEL 04/20/2000 Print all POs cost sheet farics (fi any) after the 
*C101823,1                 primary fabic printed. (Also needed modifications done in FRX)
*B804167,1 KHM  05/02/2001 Changing the name of the field cMarker to be cMark
*B804167,1                 in the FRX
*:***************************************************************************
lnAlias = SELECT()

IF !(llrPrtCs .AND. lcRpForm='P')
  =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
ENDIF
*IF !USED("SCALE_B")
  =gfOpenFile(gcDataDir+'SCALE','SCALE','SH',@lcTmpScl,.T.)
*ENDIF

IF !USED("STYLE_B")
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH',@lcTmpSty,.T.)
  SELECT(lcTmpSty)
ENDIF

SELECT POSLN
SET RELATION OFF INTO (lcTmpScl)
SET RELATION OFF INTO (lcTmpSty)
SELECT POSLN
SET RELATION TO 'P'+ Posln.scale+ Posln.prepak INTO (lcTmpScl) ADDITIVE
SET RELATION TO Style INTO (lcTmpSty) ADDITIVE
SELECT(lcTmpSty)
SET RELATION TO fabric INTO Fabric ADDITIVE

*C101823,1 (Begin) Prepare the temp file holding all PO cost sheet's fabrics.
IF llOgFltCh
  IF !USED(lcTmpFab)
    lcTmpFab = gfTempName()
    CREATE TABLE (lcTmpFab) (PO C(6), STYLE C(19), LINENO N(6),Fabrics m)
    INDEX ON PO TAG (lcTmpFab)
    SELECT POSHDR
    SET RELATION TO PO INTO (lcTmpFab) ADDI
  ELSE
    IF RECCOUNT(lcTmpFab)>0
      SELECT (lcTmpFab)
      ZAP
    ENDIF
  ENDIF
ENDIF  
*C101823,1 (End)
SELECT(lnAlias)


******************************************************
FUNCTION lfGetColor
PARAMETER lcClrDesc
PRIVATE lcClrDesc

lcClrDesc = gfCodDes(SUBSTR(POSLN.STYLE,14,6), 'COLOR')
RETURN 
******************************************************


****************************************************************************
* FUNC: lfGetFabrcs
* DESC: To collect the PO cost sheet's fabric(s) for selected PO(s).
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 04/16/2000
* Refer To : (C101823)
****************************************************************************
FUNCTION lfGetFabrcs
PARAMETERS lcDummy

PRIVATE lnAlias,laFab
lnAlias = SELECT(0)
IF !USED('CTKTBOM')
  =gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
ENDIF
*--Create a new dimension for not duplicating fabrics as we scan all FABRIC\COLORS.
DIMENSION laFab[1] 
laFab = ''
*-- If we will print cost sheet , insert cost sheet records in the temprorary file.
IF SEEK('I'+POSHDR.PO,'CTKTBOM') AND !SEEK(POSHDR.PO,(lcTmpFab))
  SELECT CTKTBOM
  SCAN WHILE cImTyp+CutTkt = 'I'+POSHDR.PO FOR cCatgTyp = 'F'
    IF ASCAN(laFab,SUBSTR(ITEM,1,7)) = 0 AND SUBSTR(ITEM,1,7) <> &lcTmpSty..FABRIC    
      IF !SEEK(POSHDR.PO,(lcTmpFab))
        INSERT INTO (lcTmpFab) (PO,STYLE,LINENO) VALUES (POSHDR.PO,POSLN.STYLE,POSLN.LINENO)
      ENDIF
      =SEEK(SUBSTR(ITEM,1,7),'FABRIC')
      DIMENSION laFab[ALEN(laFab,1)+1] 
      laFab[ALEN(laFab,1)] = SUBSTR(ITEM,1,7)
      SELECT (lcTmpFab)
      REPLACE FABRICS WITH IIF(!EMPTY(FABRICS),CHR(13),'')+Fabric.Fabric+SPACE(2)+Fabric.Pattern+SPACE(1)+Fabric.Vendor+;
                            SPACE(1)+Fabric.Width+SPACE(1)+Fabric.Content ADDI
    ENDIF
  ENDSCAN
  *--Get the primary fabric record.
  =SEEK(&lcTmpSty..FABRIC,'FABRIC')
  *--This variable was inirialized in FRX and has its value here to print the title of STYLE AND PO NOTEPAD.
  lcAll = IIF(!EMPTY(lcNotes),REPLICATE('=',97)+lcTitle+chr(13)+replicate('=',LEN(lcTitle)),'')
ENDIF
SELECT (lnAlias)