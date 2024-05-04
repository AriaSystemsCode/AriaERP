using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using Aria5SystemAdmin.Module.Managers;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Helpers;
using DevExpress.Xpo.Metadata;
using Microsoft.Azure.NotificationHubs;
using System.Web.Configuration;


namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
{
    /// <summary>
    /// Summary description for AriaSynchronizationService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class AriaSynchronizationService : System.Web.Services.WebService
    {
        # region Methods
       // [WebMethod(EnableSession = true)]
        [WebMethod(EnableSession = true, CacheDuration = 3600)]
        public List<SyncDataObject> PullData(Guid accountOid, int fromVersion, int toVersion, string deviceSeginture)
        {
            //var cacheDl = Context.Cache[accountOid.ToString()];

            //if (cacheDl == null)
            //{
            //    cacheDl = ClientSessionInfo.SetClientDataLayer(accountOid);

            //    Context.Cache.Insert(accountOid.ToString(), cacheDl);
            //}

         //   return synchronizationManager.PullData(accountOid, fromVersion, toVersion, deviceSeginture, cacheDl as List<XPClassInfo>);

            SyncManager synchronizationManager = new SyncManager();
            return synchronizationManager.PullData(accountOid, fromVersion, toVersion, deviceSeginture);
        }

    [WebMethod(EnableSession = true,CacheDuration=3600)]
    //  [WebMethod(EnableSession = true)]

      public List<string> PushData(Guid accountOid, List<SyncDataObject> dataToUpdate, string deviceSeginture)
      {
          //var cacheDl = Context.Cache[accountOid.ToString()];

          //if (cacheDl == null)
          //  {
          //      cacheDl = ClientSessionInfo.SetClientDataLayer(accountOid);

          //      Context.Cache.Insert(accountOid.ToString(), cacheDl);
          //  }
            SyncManager synchronizationManager = new SyncManager();
        
       //   return synchronizationManager.PushData(accountOid, dataToUpdate, deviceSeginture, cacheDl as List<XPClassInfo>);

         return synchronizationManager.PushData(accountOid, dataToUpdate, deviceSeginture);
      }
      [WebMethod(EnableSession = true)]
      public int GetDBVersion(Guid accountOid)
    {
        //var cacheDl = Context.Cache[accountOid.ToString()];

        //if (cacheDl == null)
        //{
        //    cacheDl = ClientSessionInfo.SetClientDataLayer(accountOid);

        //    Context.Cache.Insert(accountOid.ToString(), cacheDl);
        //}
          SyncManager synchronizationManager = new SyncManager();
        //  return synchronizationManager.GetDbVersion(accountOid, cacheDl as List<XPClassInfo>);
          return synchronizationManager.GetDbVersion(accountOid);
      }
      //[WebMethod(EnableSession = true)]
      //public bool Register(Guid account, String deviceSeginture)
      //{
      //    Helpers.SchemaHelper.SwitchSchema();
      //    SyncManager synchronizationManager = new SyncManager();
      //    return synchronizationManager.Register(account, deviceSeginture);
      //}

      [WebMethod(EnableSession = true)]
      public bool Notify(String txt,Guid accountOid)
      {
          //Helpers.SchemaHelper.SwitchSchema();
          SyncManager synchronizationManager = new SyncManager();
          return synchronizationManager.Notify(txt, accountOid);
      }

      [WebMethod(EnableSession = true)]
      public string GetMinFrontEndVersion()
      {
         
          SyncManager synchronizationManager = new SyncManager();
          return synchronizationManager.GetMinFrontEndVersion();
      }
        # endregion

     
    }
}
