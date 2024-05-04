using DevExpress.Xpo;
using DevExpress.Xpo.DB;
using DevExpress.Xpo.Metadata;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Helpers
{
    public static class ClientSessionInfo
    {
        private static Dictionary<Guid, IDataLayer> ClientDataLayer = new Dictionary<Guid, IDataLayer>();
        private static List<XPClassInfo> ClientClassesInformationList = new List<XPClassInfo>();
        private static Dictionary<IDataLayer, List<XPClassInfo>> ClientClassInfo = new Dictionary<IDataLayer, List<XPClassInfo>>();

        public static List< XPClassInfo> SetClientDataLayer(Guid accountOid)
        {
            List<XPClassInfo> NewClassInfo = new List<XPClassInfo>();

            if (ClientDataLayer.ContainsKey(accountOid))
	        {
		       IDataLayer CurrentDataLayer= ClientDataLayer.Where(r=>r.Key==accountOid).FirstOrDefault().Value;
                if (ClientClassInfo.ContainsKey(CurrentDataLayer))
                {
                    ClientClassesInformationList = ClientClassInfo.Where(r => r.Key == CurrentDataLayer).FirstOrDefault().Value;
                    return ClientClassesInformationList;
                }
            }
            else
	        {
                Session session = new Session(XpoHelper.GetNewDataLayer());
                ClientDataLayer.Add(accountOid, session.DataLayer);
                string accountSchema = GetAccountSchema(accountOid);
                if (!string.IsNullOrWhiteSpace(accountSchema))
                {
                    NewClassInfo = SchemaHelper.SetCleintSchema(session,accountSchema);
                    ClientClassInfo.Add(session.DataLayer, NewClassInfo);
                }
                return NewClassInfo;        
	        }
            return NewClassInfo;
        }

        public static  string GetAccountSchema(Guid accountOid)
        {
            Session accountSession = new Session(XpoHelper.GetNewDataLayer());
            string accountSchema = "";

            SelectedData data = accountSession.ExecuteQuery("Select DbSchema from contact where oid = '" + accountOid.ToString() + "'");

            if (data.ResultSet != null && data.ResultSet.Count() > 0 && data.ResultSet[0].Rows.Count() > 0 &&
                data.ResultSet[0].Rows[0].Values[0] != null && data.ResultSet[0].Rows[0].Values[0] != DBNull.Value)
            {
                accountSchema = data.ResultSet[0].Rows[0].Values[0].ToString().Trim();
            }
            else
            {
                accountSchema = null;
            }

            accountSession.Dispose();

            return accountSchema;
        }
    }
}