using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using DevExpress.Data.Filtering	;
using DevExpress.Xpo.DB;

namespace Aria5SystemAdmin.Module.Managers
{
    public class LockingManager
    {
        private DateTime startTime;
        public string getLock(Guid Oid, String UserName, string tableName, string Devciesignature)
        {
            Session session = XpoDefault.Session;
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                startTime = DateTime.Now;
             
                SelectedData data = session.ExecuteSproc("GetLock", new OperandValue(Oid), new OperandValue(UserName), new OperandValue(tableName), new OperandValue(Devciesignature), new OperandValue(startTime));   

                if (data.ResultSet!=null)
                {
                    foreach (SelectStatementResultRow row in data.ResultSet[1].Rows) 
                    {
                        string LockingUser = row.Values[1].ToString();
                        return LockingUser;
                    } 
	            }
                else
                {
                    return "";
                }
                }
                else
                {
                    return "";
                } 
             return "";    
            }

        public DateTime ReleaseLock(Guid Oid, String UserName, string tableName)
        {
            Session session = XpoDefault.Session;
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                SelectedData data = session.ExecuteSproc("ReleaseLock", new OperandValue(Oid), new OperandValue(UserName), new OperandValue(tableName));
                if (data.ResultSet != null)
                {
                    for (int i = 1; i < data.ResultSet.Length; i++)
                    {
                        foreach (SelectStatementResultRow row in data.ResultSet[i].Rows)
                        {
                            DateTime LockingTime = Convert.ToDateTime(row.Values[1]);
                            return LockingTime;
                        }
                    }
                }
                else
                {
                    return DateTime.Now;
                }
            }
            else
            {
                return DateTime.Now;
            }
            return DateTime.Now;
        }
    }
       
}