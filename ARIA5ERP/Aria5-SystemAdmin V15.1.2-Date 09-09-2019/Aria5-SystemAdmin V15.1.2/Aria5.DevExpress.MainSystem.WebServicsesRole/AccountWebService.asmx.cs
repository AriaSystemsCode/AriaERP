using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;

namespace Aria5.DevExpress.MainSystem.WebServicsesRole
{
    /// <summary>
    /// Summary description for AccountWebService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class AccountWebService : System.Web.Services.WebService
    {
       

        [WebMethod (EnableSession=true)]
        public string AccountStatus(Guid Account)
        {
            if (Session["IsAuth"] != null && Boolean.Parse(Session["Permission"].ToString()) == true)
            {
                XpoDefault.ConnectionString = Login.ConnectionString;
                Session session = new Session();

                Contact _Account = session.FindObject<Contact>(CriteriaOperator.Parse("[OID] = '" + Account + "'"));

                // Contact _Account = new Contact(session);
                return _Account.Status.ToString();
            }

            else
                return string.Empty;
            // return "xxx";
        }

    }


}
