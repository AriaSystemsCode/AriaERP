
set
USE r:\aria26\povar.dbf SHARED
SELECT 2
USE r:\aria26\lstpol.dbf SHARED
BROWSE LAST
SELECT 1
BROWSE LAST
go top
set
SELECT 5
USE r:\aria26\data2\posln.dbf SHARED
USE r:\aria26\data2\posln.dbf SHARED again alias POSLN26
set
SELECT 6
USE r:\aria27\dbfs\02\posln.dbf SHARED
USE r:\aria27\dbfs\02\posln.dbf SHARED again alias POSLN27
set
SELECT 5
set order to 1
select 6
set order to 1
set
SELECT 1
SET RELATION TO Povar.po INTO Posln26 ADDITIVE
SELECT 5
SET RELATION TO "P"+ Posln26.po INTO Posln27 ADDITIVE
SELECT 6
SET FILTER TO Posln27.trancd='2'
SELECT 1
