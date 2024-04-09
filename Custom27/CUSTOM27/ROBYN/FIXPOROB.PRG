*:************************************************************************
*: Program file  : Fixporob.Prg
*: Program desc. : Fix program to copy some fildes from VendCapa.dbf in 2.6
*:                 to ApVendor.dbf in 2.7
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : Wael Mohammed Anwar (WMA)
*: Date          : 07/30/2000
*: Reference     : C#101335
*:************************************************************************
*: Calls : None.
*:************************************************************************
*: Passed Parameters  : None.
*:************************************************************************

*-- Call GETDIR function to get aria 2.6 data dir. 
lcDataDir6 = GETDIR('','Select Aria 2.6 Data Folder')

*-- If user select cancel return false and exit from program.
IF EMPTY(lcDataDir6)
  RETURN(.F.)
ENDIF


*-- Call GETDIR function to get aria 2.7 data dir. 
lcDataDir7 = GETDIR('','Select Aria 2.7 Data Folder')

*-- If user select cancel return false and exit from program.
IF EMPTY(lcDataDir7)
  RETURN(.F.)
ENDIF

  
*-- check if file 'VENDCAPA.DBF' does not exist.
IF !FILE(lcDataDir6+'VENDCAPA.DBF')
  WAIT WINDOW 'File ' + lcDataDir6+'VENDCAPA.DBF' + ' does not exists... Press any key to exit'
  RETURN(.F.)
ELSE
  *-- check if file 'APVENDOR.DBF' does not exist.
  IF !FILE(lcDataDir7+'APVENDOR.DBF')
    WAIT WINDOW 'File ' + lcDataDir7+'APVENDOR.DBF' + ' does not exists... Press any key to exit'
    RETURN(.F.)
  ENDIF
ENDIF

*-- Open 'VENDCAPA' file & check if empty.
IF USED('VENDCAPA')
  SELECT VENDCAPA
  USE
ENDIF
SELECT 0
USE lcDataDir6+'VENDCAPA.DBF' ORDER TAG 'VENDCAPA' SHAR
GO TOP
IF EOF()
  USE
  WAIT WINDOW 'File VENDCAPA.DBF is empty... Press any key to exit'
  RETURN(.F.) 
ENDIF
  
*-- Open 'APVENDOR' file & check if empty.
IF USED('APVENDOR')
  SELECT APVENDOR
  USE
ENDIF
SELECT 0
USE lcDataDir7+'APVENDOR.DBF' ORDER TAG 'VENCODE' SHAR
GO TOP
IF EOF()
  USE
  WAIT WINDOW 'File APVENDOR.DBF is empty... Press any key to exit'
  RETURN(.F.) 
ENDIF


*-- Copy information from old file (in aria 2.6) to new file (in aria 2.7).
SELECT VENDCAPA
SCAN 
  IF SEEK(VENDCAPA.VENDOR,'APVENDOR')
    WAIT WINDOW 'Copying data for Vendor '+ALLTRIM(VENDOR) NOWAIT
    SELECT APVENDOR
    REPLACE Ncapacity    WITH VENDCAPA.Capacity ,;
            Ccategtype   WITH VENDCAPA.Category
  ELSE
    WAIT WINDOW 'Vendor '+ALLTRIM(VENDOR)+' not found in APVENDOR.DBF file' NOWAIT
  ENDIF
ENDSCAN


*-- display wait window when finished from copy process.
WAIT WINDOW 'Process completed successfully... Press any key to exit'


*-- Close opened files.
IF USED('VendCapa')
  SELECT VendCapa
  USE
ENDIF

IF USED('APVENDOR')
  SELECT APVENDOR
  USE
ENDIF

*-- End OF Fixporob.Prg