using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using Aria5SystemAdmin.Module.Managers;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
{
    /// <summary>
    /// Summary description for AriaLockingService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class AriaLockingService : System.Web.Services.WebService
    {

        //[WebMethod]
        //public string HelloWorld()
        //{
        //    return "Hello World";
        //}

      
        # region Methods
        [WebMethod]
        public  string GetLock(Guid Oid, string UserName, string TableName, string DeviceSignature)
        {
            Helpers.SchemaHelper.SwitchSchema();

            LockingManager LockManger = new LockingManager();
            return LockManger.getLock(Oid, UserName, TableName, DeviceSignature);
        }
        [WebMethod]
        public  DateTime ReleaseLock(Guid Oid, string UserName, string TableName)
        {
            Helpers.SchemaHelper.SwitchSchema();

            LockingManager LockManger = new LockingManager();
            return LockManger.ReleaseLock(Oid, UserName, TableName);
        }
        # endregion
    }
}
