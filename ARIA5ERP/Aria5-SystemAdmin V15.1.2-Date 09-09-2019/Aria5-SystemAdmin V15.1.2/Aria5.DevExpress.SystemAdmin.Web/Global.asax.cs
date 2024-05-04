using System;
using System.Configuration;
using System.Web.Configuration;
using System.Web;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.ExpressApp.Security;
using DevExpress.ExpressApp.Web;
using DevExpress.Web;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Security.Strategy;

namespace Aria5SystemAdmin.Web
{
    public class Global : System.Web.HttpApplication
    {

        public void SetConnection()
        {
            string schemaName = "SystemAdmin";


            XPDictionary xpDictionary = DevExpress.ExpressApp.Xpo.XpoTypesInfoHelper.GetXpoTypeInfoSource().XPDictionary;


            foreach (var type in XafTypesInfo.PersistentEntityStore.RegisteredEntities)
            {
                if (type == typeof(XPObjectType) || type.IsSubclassOf(typeof(XPBaseObject)))
                {
                        var typeInfo = xpDictionary.GetClassInfo(type);




                        if (typeInfo.Table != null)
                        {
                            if (typeInfo.Table.Name.Contains("User") || typeInfo.Table.Name.Contains("Role"))
                            {
                                int i = 0;
                            }

                            if (!typeInfo.Table.Name.StartsWith(schemaName + "."))
                            {
                                typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name;
                            }

                            foreach (var fk in typeInfo.Table.ForeignKeys)
                            {
                                if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                                {
                                    fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                                }
                            }
                        }
                }
            }

            foreach (var type in xpDictionary.Classes)
            {
                try
                {
                    XPClassInfo type1 = (XPClassInfo)type;
                    //var typeInfo = xpDictionary.GetClassInfo(type);

                    if (!type1.Table.Name.StartsWith(schemaName + "."))
                    {
                        type1.Table.Name = schemaName + "." + type1.Table.Name;
                    }

                    foreach (var fk in type1.Table.ForeignKeys)
                    {
                        if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                        {
                            fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                        }
                    }
                }
                catch (Exception)
                {
                }
            }

            var x = xpDictionary.GetDataStoreSchema(typeof(Account).Assembly, typeof(SecuritySystemUser).Assembly);

            foreach (var type in x)
            {
                if (!type.Name.StartsWith(schemaName + "."))
                {
                    type.Name = schemaName + "." + type.Name;
                }

                foreach (var fk in type.ForeignKeys)
                {
                    if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                    {
                        fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                    }
                }
            }

            var x1 = xpDictionary.GetDataStoreSchema(typeof(XPObjectType).Assembly);

            foreach (var type in x)
            {
                if (!type.Name.StartsWith(schemaName + "."))
                {
                    type.Name = schemaName + "." + type.Name;
                }

                foreach (var fk in type.ForeignKeys)
                {
                    if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                    {
                        fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                    }
                }
            }
        }

        public Global()
        {
            InitializeComponent();
        }
        protected void Application_Start(Object sender, EventArgs e)
        {
            ASPxWebControl.CallbackError += new EventHandler(Application_Error);

#if EASYTEST
			DevExpress.ExpressApp.Web.TestScripts.TestScriptsManager.EasyTestEnabled = true;
#endif

        }
        protected void Session_Start(Object sender, EventArgs e)
        {
            WebApplication.SetInstance(Session, new Aria5SystemAdminAspNetApplication());
#if EASYTEST
			if(ConfigurationManager.ConnectionStrings["EasyTestConnectionString"] != null) {
				WebApplication.Instance.ConnectionString = ConfigurationManager.ConnectionStrings["EasyTestConnectionString"].ConnectionString;
			}
#endif
            if (ConfigurationManager.ConnectionStrings["ConnectionString"] != null)
            {
                WebApplication.Instance.ConnectionString = ConfigurationManager.ConnectionStrings["ConnectionString"].ConnectionString;
            }

            //SetConnection();

            WebApplication.Instance.Setup();

            WebApplication.Instance.DatabaseVersionMismatch += Instance_DatabaseVersionMismatch;
            SetConnection();

            WebApplication.Instance.Start();
        }

        void Instance_DatabaseVersionMismatch(object sender, DatabaseVersionMismatchEventArgs e)
        {
            SetConnection();
        }
        protected void Application_BeginRequest(Object sender, EventArgs e)
        {
            string filePath = HttpContext.Current.Request.PhysicalPath;
            if (!string.IsNullOrEmpty(filePath)
                && (filePath.IndexOf("Images") >= 0) && !System.IO.File.Exists(filePath))
            {
                HttpContext.Current.Response.End();
            }
        }
        protected void Application_EndRequest(Object sender, EventArgs e)
        {
        }
        protected void Application_AuthenticateRequest(Object sender, EventArgs e)
        {
        }
        protected void Application_Error(Object sender, EventArgs e)
        {
            ErrorHandling.Instance.ProcessApplicationError();
        }
        protected void Session_End(Object sender, EventArgs e)
        {
            WebApplication.LogOff(Session);
            WebApplication.DisposeInstance(Session);
        }
        protected void Application_End(Object sender, EventArgs e)
        {
        }
        #region Web Form Designer generated code
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
        }
        #endregion
    }
}
