using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Environment;
using System.EnterpriseServices;
using Aria.Data;
using Aria.Utilities.ParameterSubstitution;
using Aria.DataTypes;
using System.Collections;
using System.Runtime.InteropServices;


namespace Aria.EnterpriseServices.Audit
{
    
    public class AriaAuditingManager : ServicedComponent 
    {
        public void Add(AriaDbConnection connection, string ObjectName, string EventName, string auditID, AriaArgumentList argumentList)
        {
            AriaParameterSubstituter partameterSubstitution = new AriaParameterSubstituter(connection, argumentList);

            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            AriaObjectRevision objectRevision = objectDictionary.LoadAriaObjectLastRevision(connection, ObjectName);
            AriaObjectEvent objectEvent = objectDictionary.LoadAriaObjectEvent(connection, ObjectName, objectRevision.ObjectRevision, EventName);

            AriaDbCommand command = new AriaDbCommand("SELECT * FROM AriaObject27Mapping WHERE ObjectName = @ObjectName", 
                                                        connection, AriaDatabaseTypes.Aria50SystemFiles);
            
            command.Parameters.Add(new AriaDbParameter("ObjectName", ObjectName));
            DataTable table = command.GetDataTable();

            string sqlAuditCommand = "INSERT INTO audtrail (capobjnam,key,caudtralid,cevent_id,mneededinf, cAdd_User, dAdd_Date, cAdd_Time) VALUES('@mappedObject','" + table.Rows[0]["Aria27ObjectKey"].ToString() + "','@auditID','@eventName','@auditInfo', 'MAH', {^2007-09-27}, '" + DateTime.Now.ToString("T") + "')";
            command = new AriaDbCommand(sqlAuditCommand, connection, AriaDatabaseTypes.Aria27Data);

            command.Parameters.Add(new AriaDbParameter("mappedObject", table.Rows[0]["Aria27ObjectName"]));
            command.Parameters.Add(new AriaDbParameter("auditID", auditID));
            command.Parameters.Add(new AriaDbParameter("eventName", EventName));
            command.Parameters.Add(new AriaDbParameter("auditInfo", partameterSubstitution.GetSubstitutedText(objectEvent.AuditInfo)));

            for( int argumentIndex = 0 ; argumentIndex < argumentList.Count ; argumentIndex++ )
            {
                AriaArgument argument = (AriaArgument)argumentList[argumentIndex];
                ArrayList fields = ((AriaDataObjectPointer)argument.Value).KeyFields;

                for (int fieldIndex = 0; fieldIndex < fields.Count; fieldIndex++)
                {
                    command.Parameters.Add(new AriaDbParameter(((AriaDataObjectPointerKeyField)fields[fieldIndex]).FieldName, ((AriaDataObjectPointerKeyField)fields[fieldIndex]).Value));
                }
            }

            command.ExecuteNonQuery();
        }
    }
}
