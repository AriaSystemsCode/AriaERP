*:***************************************************************************
*: Program file  : MFCop30
*: Program desc. : Convert order & account fields of CUTTKTH From 26 to 27
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : Mohamed Shokry
*: Reference     : *C101917
*: Notes         : 1- This Program Made according to nat. mail to convert Account 
*:               :    and order from 26 to 27 in CUTTKTH Table.
*:***************************************************************************
*: Calls : 
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFCOP30
*:***************************************************************************
*: Modifications:
*:***************************************************************************
lcCopPath = FULLPATH('')
lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]
LCCUTTKTH =gfTempName()
lcPath = ''
=lfGetDir()
=lfConvrt()
*!*************************************************************
*! Name      : lfGetDir
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/09/2000
*! Purpose   : To get the 'directory' 
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : =lfGetDir()
*!**************************************************************
*
FUNCTION lfGetDir
lcOldPath = lcCopPath
lcCopPath = GETDIR(lcCopPath,'Select Aria 2.6 root dir.')
lcPath = LEFT(lcCopPath,10)
*-- END OF lfGetDir.
*!*************************************************************
*! Name      : lfConvrt
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/09/2000
*! Purpose   : To Convert from 26 to 27
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : =lfConvrt()
*!**************************************************************
*
FUNCTION lfConvrt
*--return if choose cancel.
llCHK=.F.                     && Variable to Check message
IF EMPTY(lcCopPath)
  RETURN
ENDIF
*--Open needed files 

IF !FILE(ALLTRIM(lcPath)+'CONTROL.DBF')
  =gfModalGen('QRM00000B38018','F',' ',' ','Aria 2.6 path is not correct')               
  RETURN
ENDIF

=gfOpenFile(lcPath+'CONTROL',lcPath+'CONTROL','SH')
=gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')

SELECT SYCCOMP
SCAN 
  IF "MF"$MmodlSet
    SELECT CONTROL 
    LOCATE FOR (SYCCOMP.cComp_Id = syscode)
    IF FOUND()
      lcpath = lcPath+ Dd
      llCHK = .T.
      *--Open needed files 
      lcDirctor=ALLTRIM(SYCCOMP.Ccom_ddir)        && to get directory
      =gfOpenFile(lcDirctor +'CUTTKTH',lcDirctor +'CUTTKTH','SH')
      =gfOpenFile(ALLTRIM(lcPath)+'CUTTKTH',ALLTRIM(lcPath)+'CUTTKTH','SH',LCCUTTKTH, .T.)
      SELECT CUTTKTH
      IF TYPE('CUTTKTH.cOrdCop') = 'U' .OR. TYPE('CUTTKTH.cCusCop') = 'U'
        =gfModalGen('QRM00000B38018','F',' ',' ','One or more custom fields was not found. Please mantain dictonary first for company ' + SYCCOMP.cComp_Id)         
        RETURN
      ENDIF
      SELECT (LCCUTTKTH)
      SCAN 
        SELECT CUTTKTH
        SEEK &LCCUTTKTH..CUTTKT
        REPLACE cOrdCop WITH &LCCUTTKTH..Order,;
                cCusCop WITH &LCCUTTKTH..account
      ENDSCAN
      = gfCloseFile('LCCUTTKTH')
      = gfCloseFile('CUTTKTH')
    ELSE
      =gfModalGen('QRM00000B38018','F',' ',' ','Company ID '+SYCCOMP.cComp_Id +' was not found in Aria26. This company will be skipped.')
    ENDIF
    lcPath = LEFT(lcCopPath,10)
  ENDIF
ENDSCAN
IF llCHK
  =gfModalGen('QRM00000B38018','F',' ',' ','Conversion for custom fields is done successfully.')   
ELSE
  =gfModalGen('QRM00000B38018','F',' ',' ','No Companies in version 2.6 matchs any of Aria 2.7 companies.')   
ENDIF
*-- END lfConvrt.

*!*************************************************************
*! Name      : lfClosFls
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 07/09/2000
*! Purpose   : To Close Necessary Files That Open Before.
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : =lfClosFls()
*!**************************************************************
*
FUNCTION lfClosFls

*-- Close All Open Files.
= gfCloseFile('SYCCOMP')
= gfCloseFile('CONTROL')

*-- End Of lfClosFls.
