*:***************************************************************************
*: Program file  : MAPMMORM.PRG
*: Program desc. : Custom MFG order form RM(Rustin and Mallory)
*: Date          : 10/30/2012
*: System        : Aria Advantage Series 4XP.
*: Module        : Material (MA)
*: Developer     : Mariam Mazhar(MMT)
*: Tracking Job Number: C201532[T20110914.0019]
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAPMMORM
*:***************************************************************************
*: Modifications :
*****************************************************************************
IF !USED('POSHDR_CNTR')
  =gfOpenTable('POSHDR','POSHDR','SH','POSHDR_CNTR')
ENDIF
lcoldmainf = lcmainf 
lcmainf = loogscroll.gftempname()
lfcrttemp()
SELECT (lcoldmainf)
LOCATE 
SCAN FOR !DELETED()
  SCATTER MEMO MEMVAR 
  =gfSeek('PF'+m.PO,'POSHDR_CNTR')
  m.cVendcode = IIF(ISNULL(POSHDR_CNTR.contrctr),"",POSHDR_CNTR.contrctr) 
  INSERT INTO (lcmainf) FROM MEMVAR
ENDSCAN 
SELECT (lcmainf) 
LOCATE 
*:*************************************************************
*: Name        : lfcrttemp
*: Developer   : Mariam Mazhar{MMT}
*: Date        : 10/30/2012
*: Purpose     : Function to Create Tmp.
*:*************************************************************
FUNCTION lfcrttemp
 SELECT posln
 = AFIELDS(lafilestru)
 lnfilestru = ALEN(lafilestru, 1)
 DIMENSION lafilestru[lnfilestru+14, 18] && 24/10/2012 Increased to 14
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'cTktType'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 6
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'HPattern'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 10
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'HdrStyle'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 19
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'HEntered'
 lafilestru[lnfilestru, 2] = 'D'
 lafilestru[lnfilestru, 3] = 10
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'HComplete'
 lafilestru[lnfilestru, 2] = 'D'
 lafilestru[lnfilestru, 3] = 10
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'Desc'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 30
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'Desc1'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 60
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'OComplete'
 lafilestru[lnfilestru, 2] = 'D'
 lafilestru[lnfilestru, 3] = 10
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'NoteFlag'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 1
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'Notes'
 lafilestru[lnfilestru, 2] = 'M'
 lafilestru[lnfilestru, 3] = 0
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'cDivision'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 6
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'Status'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 1
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1
 lafilestru[lnfilestru, 1] = 'UOMBUY'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 3
 lafilestru[lnfilestru, 4] = 0
 lnfilestru = lnfilestru+1 && 24/10/2012 extra field for cVendcode
 lafilestru[lnfilestru, 1] = 'cVendcode'
 lafilestru[lnfilestru, 2] = 'C'
 lafilestru[lnfilestru, 3] = 8
 lafilestru[lnfilestru, 4] = 0
 FOR lnlen = 7 TO 18
    FOR lncount = 0 TO 14 && 24/10/2012 Increased to 14
       STORE SPACE(1) TO lafilestru[lnfilestru-lncount, lnlen]
    ENDFOR
 ENDFOR
 gfcrttmp(lcmainf, @lafilestru, "PO+cWareCode+Style+Dyelot+NoteFlag", lcmainf, .F.)
*:*************************************************************
*: Name        : lfgetrmcodes
*: Developer   : Mariam Mazhar{MMT}
*: Date        : 10/30/2012
*: Purpose     : Function to Codes and Vendor address
*:*************************************************************
FUNCTION lfgetrmcodes
 = gfrltfld(cdivision, @ladivlnam, 'CDIVISION')
 llendgroup = .F.
 DO CASE
    CASE status='O'
       lcst = 'OPEN'
    CASE status='H'
       lcst = 'HOLD'
    CASE status='X'
       lcst = 'CANCELLED'
    CASE status='S'
       lcst = 'CLOSED'
    CASE status='A'
       lcst = 'ACTUAL'
    CASE status='C'
       lcst = 'COMPLETED'
    CASE status='B'
       lcst = 'Bid'
 ENDCASE
 IF TYPE('loWareHous')<>'O'
    lowarehous = CREATEOBJECT('RemoteTable', 'WAREHOUS', 'WAREHOUS', 'WAREHOUS', SET("Datasession"))
 ENDIF
 IF lowarehous.seek(ALLTRIM(cwarecode))
    lcshpname = warehous.cdesc
    lashpadr[1] = gfgetadr('WAREHOUS', '', '', '', 1)
    lashpadr[2] = gfgetadr('WAREHOUS', '', '', '', 2)
    lashpadr[3] = gfgetadr('WAREHOUS', '', '', '', 3)
    lashpadr[4] = gfgetadr('WAREHOUS', '', '', '', 4)
    lashpadr[5] = gfgetadr('WAREHOUS', '', '', '', 5)
    DO lfshiftarr WITH lashpadr
 ELSE
    STORE SPACE(0) TO lcshpname, lashpadr
 ENDIF
 IF TYPE('loApVendor')<>'O'
    loApVendor = CREATEOBJECT('RemoteTable', 'ApVendor', 'vencode', 'ApVendor', SET("Datasession"))
 ENDIF
 DIMENSION laVenadr[5]
 laVenadr = ''
 lcVenname= ''
 IF !EMPTY(ALLTRIM(cVendCode))
   IF loApVendor.seek(ALLTRIM(cVendCode))
      lcVenname = ApVendor.cVenComp
      laVenadr[1] = gfgetadr('ApVendor', '', '', '', 1)
      laVenadr[2] = gfgetadr('ApVendor', '', '', '', 2)
      laVenadr[3] = gfgetadr('ApVendor', '', '', '', 3)
      laVenadr[4] = gfgetadr('ApVendor', '', '', '', 4)
      laVenadr[5] = gfgetadr('ApVendor', '', '', '', 5)
      DO lfshiftarr WITH laVenadr
   ELSE
      STORE SPACE(0) TO lcVenName, laVenadr
   ENDIF
 ENDIF
RETURN ''
