lcCurDir=CurDir()
SlctDataDir = GETDIR(lcCurDir)
Use (SlctDataDir +"asn_ship")
COPY  STRUCTURE  TO  ( SlctDataDir+"UccTmplt")
USE SlctDataDir+"UccTmplt" EXCL  
ALTER TABLE UccTmplt ADD COLUMN Ucc128Fnt   G
ALTER TABLE UccTmplt ADD  COLUMN  shpZipFnt   G
ALTER TABLE UccTmplt ADD  COLUMN  ShpToStFnt  G
ALTER TABLE UccTmplt ADD  COLUMN  UPCFnt      G
ALTER TABLE UccTmplt ADD  COLUMN  UPC_humn   C(60)
ALTER TABLE UccTmplt ADD  COLUMN  CustPOFnt   G

FOR  I = 1 TO  4500
  Append Blank
  APPEND GENERAL Ucc128Fnt  CLASS ("IDAuto.BarCode")
  APPEND GENERAL shpZipFnt  CLASS ("IDAuto.BarCode") 
  APPEND GENERAL ShpToStFnt CLASS ("IDAuto.BarCode")
  APPEND GENERAL UPCFnt     CLASS ("IDAuto.BarCode") 
  APPEND GENERAL CustPOFnt  CLASS ("IDAuto.BarCode")
ENDFOR  
=MESSAGEBOX('The file UccTmplt has been created',64,'File created')