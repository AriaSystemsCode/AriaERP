using System;
using System.Collections.Generic;
using System.ComponentModel;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Xpo;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Security.Strategy;

namespace Aria5SystemAdmin.Web
{
    public partial class Aria5SystemAdminAspNetApplication : WebApplication
    {
        private DevExpress.ExpressApp.SystemModule.SystemModule module1;
        private DevExpress.ExpressApp.Web.SystemModule.SystemAspNetModule module2;
        private Aria5SystemAdmin.Module.Web.Aria5SystemAdminAspNetModule module4;
        private DevExpress.ExpressApp.ConditionalAppearance.ConditionalAppearanceModule conditionalAppearanceModule1;
        private DevExpress.ExpressApp.Validation.ValidationModule validationModule1;
        private DevExpress.ExpressApp.ViewVariantsModule.ViewVariantsModule viewVariantsModule1;
        private DevExpress.ExpressApp.Security.SecurityModule securityModule1;
        private Module.Aria5SystemAdminModule aria5SystemAdminModule1;
        private System.Data.SqlClient.SqlConnection sqlConnection1;

        public Aria5SystemAdminAspNetApplication()
        {
            InitializeComponent();
        }

        protected override void CreateDefaultObjectSpaceProvider(CreateCustomObjectSpaceProviderEventArgs args)
        {
            args.ObjectSpaceProvider = new XPObjectSpaceProviderThreadSafe(args.ConnectionString, args.Connection);
        }




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


        private void Aria5SystemAdminAspNetApplication_DatabaseVersionMismatch(object sender, DevExpress.ExpressApp.DatabaseVersionMismatchEventArgs e)
        {
#if EASYTEST
			e.Updater.Update();
			e.Handled = true;
#else
            if (System.Diagnostics.Debugger.IsAttached)
            {
                SetConnection();
                e.Updater.Confirmation += Updater_Confirmation;
                //DevExpress.Xpo.DB.ConnectionProviderSql.Factories[0].MeanSchemaGeneration = true;

                e.Updater.Update();
                e.Handled = true;
            }
            else
            {
                string message = "The application cannot connect to the specified database, because the latter doesn't exist or its version is older than that of the application.\r\n" +
                    "This error occurred  because the automatic database update was disabled when the application was started without debugging.\r\n" +
                    "To avoid this error, you should either start the application under Visual Studio in debug mode, or modify the " +
                    "source code of the 'DatabaseVersionMismatch' event handler to enable automatic database update, " +
                    "or manually create a database using the 'DBUpdater' tool.\r\n" +
                    "Anyway, refer to the following help topics for more detailed information:\r\n" +
                    "'Update Application and Database Versions' at http://www.devexpress.com/Help/?document=ExpressApp/CustomDocument2795.htm\r\n" +
                    "'Database Security References' at http://www.devexpress.com/Help/?document=ExpressApp/CustomDocument3237.htm\r\n" +
                    "If this doesn't help, please contact our Support Team at http://www.devexpress.com/Support/Center/";

                if (e.CompatibilityError != null && e.CompatibilityError.Exception != null)
                {
                    message += "\r\n\r\nInner exception: " + e.CompatibilityError.Exception.Message;
                }
                throw new InvalidOperationException(message);
            }
#endif
        }

        void Updater_Confirmation(object sender, CancelEventArgs e)
        {
            
        }


        private void InitializeComponent()
        {
            this.module1 = new DevExpress.ExpressApp.SystemModule.SystemModule();
            this.module2 = new DevExpress.ExpressApp.Web.SystemModule.SystemAspNetModule();
            this.module4 = new Aria5SystemAdmin.Module.Web.Aria5SystemAdminAspNetModule();
            this.sqlConnection1 = new System.Data.SqlClient.SqlConnection();
            this.conditionalAppearanceModule1 = new DevExpress.ExpressApp.ConditionalAppearance.ConditionalAppearanceModule();
            this.validationModule1 = new DevExpress.ExpressApp.Validation.ValidationModule();
            this.viewVariantsModule1 = new DevExpress.ExpressApp.ViewVariantsModule.ViewVariantsModule();
            this.securityModule1 = new DevExpress.ExpressApp.Security.SecurityModule();
            this.aria5SystemAdminModule1 = new Aria5SystemAdmin.Module.Aria5SystemAdminModule();
            ((System.ComponentModel.ISupportInitialize)(this)).BeginInit();
            // 
            // sqlConnection1
            // 
            this.sqlConnection1.ConnectionString = "Data Source=TR-MAHMOUD\\SQLEXPRESS;Initial Catalog=Aria5;User ID=Test2;Password=ar" +
    "ia_123";
            this.sqlConnection1.FireInfoMessageEventOnUserErrors = false;
            // 
            // validationModule1
            // 
            this.validationModule1.AllowValidationDetailsAccess = true;
            // 
            // viewVariantsModule1
            // 
            this.viewVariantsModule1.GenerateVariantsNode = true;
            this.viewVariantsModule1.ShowAdditionalNavigation = false;
            // 
            // Aria5SystemAdminAspNetApplication
            // 
            this.ApplicationName = "Aria5SystemAdmin";
            this.Connection = this.sqlConnection1;
            this.Modules.Add(this.module1);
            this.Modules.Add(this.module2);
            this.Modules.Add(this.conditionalAppearanceModule1);
            this.Modules.Add(this.validationModule1);
            this.Modules.Add(this.viewVariantsModule1);
            this.Modules.Add(this.securityModule1);
            this.Modules.Add(this.aria5SystemAdminModule1);
            this.Modules.Add(this.module4);
            this.DatabaseVersionMismatch += new System.EventHandler<DevExpress.ExpressApp.DatabaseVersionMismatchEventArgs>(this.Aria5SystemAdminAspNetApplication_DatabaseVersionMismatch);
            ((System.ComponentModel.ISupportInitialize)(this)).EndInit();

        }
    }
}
