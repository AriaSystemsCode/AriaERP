using System.Linq;
using Aria5SystemAdmin.Module.BusinessObjects;
using System;
using System.Collections.Generic;
using DevExpress.Xpo;
using DevExpress.Xpo.DB;
using DevExpress.Xpo.Metadata;
using DevExpress.XtraScheduler;
using DevExpress.Utils.Serializing.Helpers;



namespace Aria5SystemAdmin.Module
{
    class Aria5Globals
    {
        
        public int GetSequence(String ObjectName, String FieldName, Session session, string whereString)
        {
            int NextID = 0;
            if (string.IsNullOrEmpty(whereString) == false)
            { whereString = " and " + whereString; }
            SelectedData CanselectedData = session.ExecuteQuery(String.Format("Select Max({0}) as {0} from {1} where GCrecord is null " + whereString, FieldName, ObjectName));
            try
            {
                NextID = int.Parse(CanselectedData.ResultSet[0].Rows[0].Values[0].ToString().TrimEnd()) + 1;
            }
            catch (Exception ex)
            {
                NextID = 1;
            }
            

            return NextID;
        }

        public string generateXML(String ObjactName, String Oid, Session session)
        {

            string xmlString = "";
            string ss2 = String.Format("Select *  from '{0}' where Oid = '{1}'",ObjactName , Oid);
            SelectedData CanselectedData = session.ExecuteQuery(ss2);


            
            return xmlString;
        }
    }
}
