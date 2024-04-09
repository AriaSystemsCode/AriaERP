Local loCursor,ovfp
CLEAR

ON ERROR

Set Exclusive Off
Close Databases All
Set Multilocks On
loCursor = Createobject('CA')
loCursor.CursorFill()
susp
GO top
* Display the value of the company name before update.
? "Before:",companyname
?
*!*	ovfp=Createobject("visualfoxpro.application.8")
*!*	ovfp.DoCmd("set exclusive off")
*ovfp.DoCmd("update (_samples+'\northwind\customers') set companyname='Alfreds Futterkisted' where customerid='ALFKI'")
*GO top
* Update the data in the cursor.
replace companyname WITH 'Alfreds Futterkiste'
* Update the back end.
retval=TABLEUPDATE(0,.F.,locursor.alias)
Messagebox("Tableupdate="+Transform(retval))

* If update conflict occurs, display the error.
if(retval=.F.)
 LOCAL ARRAY errors(1)
 AERROR(errors)
* Displays the errors.

 IF "Update conflict"$errors[2]
  MESSAGEBOX("Update Conflict-reverting changes")
  =TABLEREVERT(.T.,locursor.alias)
 ENDIF
endif
 * Refresh the Cursor to get the updated data.
loCursor.CursorRefresh()  && Get the data again to be sure
GO top
* Display the value of the company name after update.
?
? "After:",companyname

Define Class CA As CursorAdapter
 Alias = 'test1'
 DataSourceType = 'NATIVE'
 SelectCmd = 'select * from C:\northwind\customers")'
 Tables = 'Customers'
 KeyFieldList = "customerid"
 UpdatableFieldList = "companyname"
 UpdateNameList = "customerid customers.customerid,companyname customers.companyname"
 WhereType= 3
	* This is a custom property, that is added to handle update conflicts. It does not do
 * anything by itself. It is added below to the automatically-generated UpdateInsertCmd to
 * test whether anything was actually updated.
 

 Procedure AfterUpdate
  Lparameters cFldState, lForce, nUpdateType, UpdateInsertCmd, DeleteCmd, lResult
  * To see why it will fail on the back end, look at the UpdateInsertCmd that is used
  ? "Update Command sent="+UpdateInsertCmd
	* Swap the actual values in the command to see what occurred.
  UpdateInsertCmd=Strtran(UpdateInsertCmd,[OLDVAL('customerid','test1')],Oldval('customerid','test1'))
  UpdateInsertCmd=Strtran(UpdateInsertCmd,[OLDVAL('companyname','test1')],Oldval('companyname','test1'))
  UpdateInsertCmd=Strtran(UpdateInsertCmd,[test1.companyname],test1.companyname)
  ? "With the OLDVAL() and test1.companyname evaluated the update statement is :"+UpdateInsertCmd
  * Check tally.
  ? "Tally="+Transform(_Tally)

 Procedure BeforeUpdate
  Lparameters cFldState, lForce, nUpdateType, cUpdateInsertCmd, cDeleteCmd
  cUpdateInsertCmd=cUpdateInsertCmd+this.ConflictCheckCmd

ENDDEFINE
