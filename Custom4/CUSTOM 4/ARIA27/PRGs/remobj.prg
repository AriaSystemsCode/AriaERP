*E038729,1 AMH Create object to connect to data company.
LOCAL lcSetClass
lcSetClass = SET('CLASSLIB')
SET CLASSLIB TO (oAriaApplication.lcAria4Class+"MAIN.VCX"),(oAriaApplication.lcAria4Class+"UTILITY.VCX")
oAriaApplication.RemoteCompanyData = NEWOBJECT("RemoteDataAccess",oAriaApplication.lcAria4Class+"MAIN.VCX")
oAriaApplication.RemoteCompanyData.Usetechnique = 2
SET CLASSLIB TO &lcSetClass.