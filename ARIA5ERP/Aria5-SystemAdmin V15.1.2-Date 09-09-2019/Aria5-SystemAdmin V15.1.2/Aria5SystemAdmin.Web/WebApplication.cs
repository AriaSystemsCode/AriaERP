using System;
using System.Collections.Generic;
using System.ComponentModel;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Xpo;
using DevExpress.ExpressApp.Security.Strategy;
using DevExpress.ExpressApp.Security;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.Data.SqlClient;
using DevExpress.Xpo.Metadata;
using DevExpress.Xpo.DB;
using Aria5SystemAdmin.Module;
using DevExpress.Data.Filtering;
using System.Configuration;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
//ATA 
using DevExpress.ExpressApp.Security.Adapters;

namespace Aria5SystemAdmin.Web
{
    public partial class Aria5SystemAdminAspNetApplication : WebApplication
    {
        //private SecurityStrategyComplex securityStrategyComplex1;
        private AriaSecurityStrategy securityStrategyComplex1;
        private AuthenticationStandard authenticationStandard1;

        private DevExpress.ExpressApp.SystemModule.SystemModule module1;
        private DevExpress.ExpressApp.Web.SystemModule.SystemAspNetModule module2;
        private Aria5SystemAdmin.Module.Aria5SystemAdminModule module3;
        private Aria5SystemAdmin.Module.Web.Aria5SystemAdminAspNetModule module4;
        private DevExpress.ExpressApp.ConditionalAppearance.ConditionalAppearanceModule conditionalAppearanceModule1;
        private DevExpress.ExpressApp.Validation.ValidationModule validationModule1;
        private DevExpress.ExpressApp.Reports.Web.ReportsAspNetModule reportsAspNetModule1;
        private DevExpress.ExpressApp.Reports.ReportsModule reportsModule1;
        private DevExpress.ExpressApp.ViewVariantsModule.ViewVariantsModule viewVariantsModule1;
        private DevExpress.ExpressApp.TreeListEditors.TreeListEditorsModuleBase treeListEditorsModuleBase1;
        private DevExpress.ExpressApp.TreeListEditors.Web.TreeListEditorsAspNetModule treeListEditorsAspNetModule1;
        private DevExpress.ExpressApp.Objects.BusinessClassLibraryCustomizationModule businessClassLibraryCustomizationModule1;
        private DevExpress.ExpressApp.Security.SecurityModule securityModule1;
        private AriaDevExpress.Module.AriaDevExpressModule ariaDevExpressModule1;
        private DevExpress.ExpressApp.HtmlPropertyEditor.Web.HtmlPropertyEditorAspNetModule htmlPropertyEditorAspNetModule1;
        private Module.Aria5SystemAdminModule aria5SystemAdminModule1;
        private DevExpress.ExpressApp.FileAttachments.Web.FileAttachmentsAspNetModule fileAttachmentsAspNetModule1;
        private DevExpress.ExpressApp.CloneObject.CloneObjectModule cloneObjectModule1;
        private Aria5SystemAdminModule aria5SystemAdminModule2;
        private DevExpress.ExpressApp.AuditTrail.AuditTrailModule auditTrailModule1;
        private Aria5.DevExpress.OneTouchAway.Module.ModuleModule moduleModule1;
        private Aria5SystemAdminModule aria5SystemAdminModule3;
        private AriaDevExpress.Module.AriaDevExpressModule ariaDevExpressModule2;
        private GaugePropertyEditor.Web.GaugePropertyEditorAspNetModule gaugePropertyEditorAspNetModule1;
        private DevExpress.ExpressApp.ReportsV2.ReportsModuleV2 reportsModuleV21;
        private DevExpress.ExpressApp.Notifications.NotificationsModule notificationsModule1;
        private DevExpress.ExpressApp.Kpi.KpiModule kpiModule1;
        private HyperLinkPropertyEditor.Web.HyperLinkPropertyEditorAspNetModule hyperLinkPropertyEditorAspNetModule1;
        private DevExpress.ExpressApp.Notifications.Web.NotificationsAspNetModule notificationsAspNetModule1;
        private DevExpress.ExpressApp.Validation.Web.ValidationAspNetModule validationAspNetModule1;
        private System.Data.SqlClient.SqlConnection sqlConnection1;

        public Aria5SystemAdminAspNetApplication()
        {
            InitializeComponent();
            DevExpress.ExpressApp.Security.SecurityAdapterHelper.Enable(ReloadPermissionStrategy.CacheOnFirstAccess);
        }

       // Mina.B 2015-06-21 [Begin]
        protected override void OnLoggingOn(LogonEventArgs args)
        {

            base.OnLoggingOn(args);
        //  return;
            IObjectSpace objectSpace = this.ObjectSpaceProvider.CreateObjectSpace();
            string userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
            CriteriaOperator criteria = CriteriaOperator.Parse("UserName = '" + userName + "'");
            AriaSecuritySystemUser user = objectSpace.FindObject<AriaSecuritySystemUser>(criteria);

            if (user != null && userName.ToUpper().Trim() == "ADMIN")
            {
                CriteriaOperator criteriUser = CriteriaOperator.Parse("UserName = '" + userName + "'");
                SecuritySystemUser user1 = objectSpace.FindObject<SecuritySystemUser>(criteria);
                //user = objectSpace.CreateObject<AriaSecuritySystemUser>();
                CriteriaOperator criteriAccount = CriteriaOperator.Parse("Id = 'Aria'");
                Account accountAria = objectSpace.FindObject<Account>(criteriAccount);
                user.Account = accountAria;
                user.UserName = user1.UserName;
                foreach (var role in user1.Roles)
                {
                    user.Roles.Add(role);
                }
                user.ChangePasswordOnFirstLogon = user1.ChangePasswordOnFirstLogon;
                user.IsActive = user1.IsActive;
                user1.Session.CommitTransaction();
                user.Save();
                user.Session.CommitTransaction();
            }


         
            AriaSecuritySystemUser.CurrentAccount = user.Account;

            if (user.Account != null)
            {
                
                    string schemaName = user.Account.DBSchema;
                    if (!string.IsNullOrEmpty(schemaName))
                    {

                    string strCommand = String.Format("IF (NOT EXISTS (SELECT * FROM sys.schemas WHERE name = '" + schemaName + "')) BEGIN EXEC ('CREATE SCHEMA [" + schemaName + "] AUTHORIZATION [dbo]') END", schemaName, "dbo");
                    SqlConnection myConnection = new SqlConnection();
                    //myConnection.ConnectionString = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();

#if PRODUCTION

                    myConnection.ConnectionString = ConfigurationManager.AppSettings["ProductionConnectionString"].ToString();
                    //SqlConnection myConnection = new SqlConnection(@"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin;User ID=sa;Password=aria_123");
                    // this.sqlConnection1.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
#endif

#if STAGING
                    myConnection.ConnectionString = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();
                    //this.sqlConnection1.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog= Aria5SystemAdmin ;User ID=sa;Password=aria_123";

#endif


                    try
                    {
                        myConnection.Open();

                        // Authenticate user before any further processing 

                        SqlCommand command = new SqlCommand(strCommand, myConnection);
                        SqlDataReader reader = command.ExecuteReader();

                    }
                    catch (SqlException ex)
                    { }

                    XPDictionary xpDictionary = DevExpress.ExpressApp.Xpo.XpoTypesInfoHelper.GetXpoTypeInfoSource().XPDictionary;



                    // Mahmoud Start
                    List<string> clientTables = new List<string>();
                    foreach (var classes in xpDictionary.Classes)
                    {


                        dynamic typeInfo = classes;

                        var table1 = typeInfo.Table;

                        //if (table1.Name.ToUpper() == "ClientEntityTypeSettings".ToString().ToUpper())
                        //{
                        //    string x = ";";

                        //}

                        foreach (Attribute att in typeInfo.Attributes)
                        {
                            if (att is IsClient && ((IsClient)att).ClientTable)
                            {
                                clientTables.Add(table1.Name);
                            }
                        }

                        //  // Mahmoud 
                        if (typeInfo.ClassType == typeof(AuditDataItemPersistent) ||
                             typeInfo.ClassType == typeof(AuditedObjectWeakReference) ||
                             typeInfo.ClassType == typeof(XPWeakReference))
                        {
                            clientTables.Add(table1.Name);
                        }
                        // Mahmoud 
                    }

                    foreach (var classes in xpDictionary.Classes)
                    {
                        dynamic typeInfo = classes;

                        bool isClient = false;
                        foreach (Attribute att in typeInfo.Attributes)
                        {
                            if (att is IsClient && ((IsClient)att).ClientTable)
                            {
                                isClient = true;
                            }
                        }

                        // Mahmoud 
                        if (typeInfo.ClassType == typeof(AuditDataItemPersistent) ||
                            typeInfo.ClassType == typeof(AuditedObjectWeakReference) ||
                            typeInfo.ClassType == typeof(XPWeakReference))
                        {
                            isClient = true;
                        }
                        // Mahmoud 

                        if (isClient)
                        {
                            if (typeInfo.Table != null)
                            {
                                if (!typeInfo.Table.Name.StartsWith(schemaName + "."))
                                {
                                    if (typeInfo.Table.Name.Contains("."))
                                    {
                                        typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name.Split('.')[1];
                                    }
                                    else
                                    {
                                        typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name;
                                    }
                                }

                                foreach (var fk in typeInfo.Table.ForeignKeys)
                                {
                                    if (!string.IsNullOrEmpty(fk.PrimaryKeyTable) && clientTables.Contains(fk.PrimaryKeyTable))
                                    {
                                        if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                                        {
                                            if (fk.PrimaryKeyTable.Split('.').Length > 1)
                                            {
                                                fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable.Split('.')[1];
                                            }
                                            else
                                            {
                                                fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable.Split('.')[0];
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    
                
            }
                    else
                    {
                    SqlConnection myConnection = new SqlConnection();
                    //    myConnection.ConnectionString = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();

#if PRODUCTION

                    myConnection.ConnectionString = ConfigurationManager.AppSettings["ProductionConnectionString"].ToString();
                    //SqlConnection myConnection = new SqlConnection(@"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin;User ID=sa;Password=aria_123");
                    // this.sqlConnection1.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
#endif

#if STAGING
                    myConnection.ConnectionString = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();
                    //this.sqlConnection1.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog= Aria5SystemAdmin ;User ID=sa;Password=aria_123";

#endif
                    try
                    {
                            myConnection.Open();

                        }
                        catch (SqlException ex)
                        { }
                    }

            }
        }


       

        protected override void CreateDefaultObjectSpaceProvider(CreateCustomObjectSpaceProviderEventArgs args)
        {
            // sara.N 1-12-2015 Threading Issue of website [Start]

            // args.ObjectSpaceProvider = new XPObjectSpaceProvider(args.ConnectionString, args.Connection, true);
               args.ObjectSpaceProvider = new XPObjectSpaceProvider(args.ConnectionString, args.Connection, true);

            // sara.N 1-12-2015 Threading Issue of website [End]
        }

        private void Aria5SystemAdminAspNetApplication_DatabaseVersionMismatch(object sender, DevExpress.ExpressApp.DatabaseVersionMismatchEventArgs e)
        {
#if EASYTEST
			e.Updater.Update();
			e.Handled = true;
#else
            if (System.Diagnostics.Debugger.IsAttached || true)
            {
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

        private void InitializeComponent()
        {
            this.authenticationStandard1 = new DevExpress.ExpressApp.Security.AuthenticationStandard();
            this.module1 = new DevExpress.ExpressApp.SystemModule.SystemModule();
            this.module2 = new DevExpress.ExpressApp.Web.SystemModule.SystemAspNetModule();
            this.sqlConnection1 = new System.Data.SqlClient.SqlConnection();
            this.conditionalAppearanceModule1 = new DevExpress.ExpressApp.ConditionalAppearance.ConditionalAppearanceModule();
            this.validationModule1 = new DevExpress.ExpressApp.Validation.ValidationModule();
            this.reportsAspNetModule1 = new DevExpress.ExpressApp.Reports.Web.ReportsAspNetModule();
            this.reportsModule1 = new DevExpress.ExpressApp.Reports.ReportsModule();
            this.viewVariantsModule1 = new DevExpress.ExpressApp.ViewVariantsModule.ViewVariantsModule();
            this.treeListEditorsModuleBase1 = new DevExpress.ExpressApp.TreeListEditors.TreeListEditorsModuleBase();
            this.treeListEditorsAspNetModule1 = new DevExpress.ExpressApp.TreeListEditors.Web.TreeListEditorsAspNetModule();
            this.businessClassLibraryCustomizationModule1 = new DevExpress.ExpressApp.Objects.BusinessClassLibraryCustomizationModule();
            this.htmlPropertyEditorAspNetModule1 = new DevExpress.ExpressApp.HtmlPropertyEditor.Web.HtmlPropertyEditorAspNetModule();
            this.fileAttachmentsAspNetModule1 = new DevExpress.ExpressApp.FileAttachments.Web.FileAttachmentsAspNetModule();
            this.securityModule1 = new DevExpress.ExpressApp.Security.SecurityModule();
            this.cloneObjectModule1 = new DevExpress.ExpressApp.CloneObject.CloneObjectModule();
            this.aria5SystemAdminModule2 = new Aria5SystemAdmin.Module.Aria5SystemAdminModule();
            this.securityStrategyComplex1 = new Aria5SystemAdmin.Web.AriaSecurityStrategy();
            this.auditTrailModule1 = new DevExpress.ExpressApp.AuditTrail.AuditTrailModule();
            this.ariaDevExpressModule1 = new AriaDevExpress.Module.AriaDevExpressModule();
            this.module4 = new Aria5SystemAdmin.Module.Web.Aria5SystemAdminAspNetModule();
            this.moduleModule1 = new Aria5.DevExpress.OneTouchAway.Module.ModuleModule();
            this.aria5SystemAdminModule3 = new Aria5SystemAdmin.Module.Aria5SystemAdminModule();
            this.ariaDevExpressModule2 = new AriaDevExpress.Module.AriaDevExpressModule();
            this.gaugePropertyEditorAspNetModule1 = new GaugePropertyEditor.Web.GaugePropertyEditorAspNetModule();
            this.reportsModuleV21 = new DevExpress.ExpressApp.ReportsV2.ReportsModuleV2();
            this.notificationsModule1 = new DevExpress.ExpressApp.Notifications.NotificationsModule();
            this.kpiModule1 = new DevExpress.ExpressApp.Kpi.KpiModule();
            this.hyperLinkPropertyEditorAspNetModule1 = new HyperLinkPropertyEditor.Web.HyperLinkPropertyEditorAspNetModule();
            this.notificationsAspNetModule1 = new DevExpress.ExpressApp.Notifications.Web.NotificationsAspNetModule();
            this.validationAspNetModule1 = new DevExpress.ExpressApp.Validation.Web.ValidationAspNetModule();
            ((System.ComponentModel.ISupportInitialize)(this)).BeginInit();
            // 
            // authenticationStandard1
            // 
            this.authenticationStandard1.LogonParametersType = typeof(DevExpress.ExpressApp.Security.AuthenticationStandardLogonParameters);
            // 
            // sqlConnection1
            // 
#if PRODUCTION
           // this.sqlConnection1.ConnectionString = ConfigurationManager.AppSettings["ProductionConnectionString"].ToString();
            this.sqlConnection1.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
#endif

#if STAGING
            this.sqlConnection1.ConnectionString = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();
            //this.sqlConnection1.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog= Aria5SystemAdmin ;User ID=sa;Password=aria_123";

#endif
            this.sqlConnection1. FireInfoMessageEventOnUserErrors = false;
            // 
            // validationModule1
            // 
            this.validationModule1.AllowValidationDetailsAccess = true;
            this.validationModule1.IgnoreWarningAndInformationRules = false;
            // 
            // reportsAspNetModule1
            // 
            this.reportsAspNetModule1.ShowFormatSpecificPreviewActions = true;
            // 
            // reportsModule1
            // 
            this.reportsModule1.EnableInplaceReports = true;
            this.reportsModule1.ReportDataType = typeof(DevExpress.Persistent.BaseImpl.ReportData);
            // 
            // securityStrategyComplex1
            // 
            this.securityStrategyComplex1.Authentication = this.authenticationStandard1;
            this.securityStrategyComplex1.RoleType = typeof(DevExpress.ExpressApp.Security.Strategy.SecuritySystemRole);
            this.securityStrategyComplex1.UserType = typeof(DevExpress.ExpressApp.Security.Strategy.SecuritySystemUser);
            // 
            // auditTrailModule1
            // 
            this.auditTrailModule1.AuditDataItemPersistentType = typeof(AriaDevExpress.Module.BusinessObjects.SysFiles.AriaAuditDataPersistent);
            // 
            // reportsModuleV21
            // 
            this.reportsModuleV21.EnableInplaceReports = true;
            this.reportsModuleV21.ReportDataType = typeof(DevExpress.Persistent.BaseImpl.ReportDataV2);
            // 
            // notificationsModule1
            // 
            this.notificationsModule1.CanAccessPostponedItems = false;
            this.notificationsModule1.NotificationsRefreshInterval = System.TimeSpan.Parse("00:05:00");
            this.notificationsModule1.NotificationsStartDelay = System.TimeSpan.Parse("00:00:05");
            this.notificationsModule1.ShowNotificationsWindow = true;
            // 
            // Aria5SystemAdminAspNetApplication
            // 
            this.ApplicationName = "Aria5SystemAdmin";
            this.Connection = this.sqlConnection1;
            this.Modules.Add(this.module1);
            this.Modules.Add(this.securityModule1);
            this.Modules.Add(this.module2);
            this.Modules.Add(this.conditionalAppearanceModule1);
            this.Modules.Add(this.validationModule1);
            this.Modules.Add(this.viewVariantsModule1);
            this.Modules.Add(this.treeListEditorsModuleBase1);
            this.Modules.Add(this.treeListEditorsAspNetModule1);
            this.Modules.Add(this.businessClassLibraryCustomizationModule1);
            this.Modules.Add(this.htmlPropertyEditorAspNetModule1);
            this.Modules.Add(this.reportsModule1);
            this.Modules.Add(this.reportsAspNetModule1);
            this.Modules.Add(this.fileAttachmentsAspNetModule1);
            this.Modules.Add(this.cloneObjectModule1);
            this.Modules.Add(this.reportsModuleV21);
            this.Modules.Add(this.notificationsModule1);
            this.Modules.Add(this.aria5SystemAdminModule2);
            this.Modules.Add(this.auditTrailModule1);
            this.Modules.Add(this.ariaDevExpressModule1);
            this.Modules.Add(this.module4);
            this.Modules.Add(this.moduleModule1);
            this.Modules.Add(this.gaugePropertyEditorAspNetModule1);
            this.Modules.Add(this.kpiModule1);
            this.Modules.Add(this.hyperLinkPropertyEditorAspNetModule1);
            this.Modules.Add(this.notificationsAspNetModule1);
            this.Modules.Add(this.validationAspNetModule1);
            this.Security = this.securityStrategyComplex1;
            this.DatabaseVersionMismatch += new System.EventHandler<DevExpress.ExpressApp.DatabaseVersionMismatchEventArgs>(this.Aria5SystemAdminAspNetApplication_DatabaseVersionMismatch);
            ((System.ComponentModel.ISupportInitialize)(this)).EndInit();

        }
    }
}
