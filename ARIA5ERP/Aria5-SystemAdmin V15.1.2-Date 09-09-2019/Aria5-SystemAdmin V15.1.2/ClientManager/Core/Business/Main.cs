using Core.Utilites;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.DirectoryServices;

namespace Core.Business
{
    public class Main
    {
        public delegate void progress(string RequestKey, string Message);
        public progress LogProgress { get; set; }
        public string RequestKey { get; set; }
        private bool LogEnabled { get { return LogProgress != null; } }
        string currentState = "";

        public void CreateClient(Settings settings)
        {
            try
            {
               // System.Threading.Thread.Sleep(60000);
                Core.Business.IO IO = new Core.Business.IO(settings);
                Core.Business.DB DB = new Core.Business.DB(settings);
                Core.Business.AD AD = new Core.Business.AD(settings);

                settings.NeededActions = settings.NeededActions ?? new List<Actions>();

                if (settings.NeededActions.Contains(Actions.CreateClientAriaFolder))
                {
                    currentState = "Creating Client Shared Aria Folders";
                    IO.CreateClientAriaFolder();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CreateClientSQLFolder))
                {
                    currentState = "Creating Client SQL Folders";
                    IO.CreateClientSQLFolder();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.Copy_AriaSource_ClientFolder))
                {
                    currentState = "Coping Aria Source to Client Shared Folder";
                    IO.Copy_AriaSource_ClientFolder();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CreateClientUser))
                {
                    currentState = "Create Client Sql User";
                    DB.CreateClientUser();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.Copy_AriaMaster_ClientSQLFolder))
                {
                    currentState = "Coping Aria Master DB to Client SQL Folder";
                    IO.Copy_AriaMaster_ClientSQLFolder();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.AttachClientMasterDB))
                {
                    currentState = "Attaching Client Master DB to SQL Server";
                    DB.AttachClientMasterDB();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CreateAllA4Databases))
                {
                    currentState = "Creating Client Aria4XP SQL Databases";
                    DB.CreateAllA4Databases();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.AddAllSycCompanyInfo))
                {
                    currentState = "Inserting Client Companies rows to Fox SycCompany";
                    DB.AddAllSycCompanyInfo();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CreateCompanyDbfsFiles))
                {
                    currentState = "Creating Companies dbfs Files using default 99 company";
                    IO.CreateAllCompaniesFiles();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.UpdateSycinst))
                {
                    currentState = "Update Client Fox Sycinst with Current Paths";
                    DB.UpdateSycinst();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CreateClientGroup))
                {
                    currentState = "Creating Client Active Directory Group";
                    AD.CreateClientGroup();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CreateClientUsers))
                {
                    currentState = "Creating Client Active Directory Users";
                    AD.CreateClientUsers();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.AdjustClientFolderSecurity))
                {
                    currentState = "Configuring Client Shared Folder Security";
                    AD.AdjustClientFolderSecurity();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.ShareClientSharedFolder))
                {
                    currentState = "Sharing Client Shared Folder";
                    AD.ShareClientSharedFolder();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CreateClientSettingsXML))
                {
                    currentState = "Creating Client Aria4 XML Settings";
                    IO.CreateClientSettingsXML();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.AddSystemMasterClientRow))
                {
                    currentState = "Inserting System.Master Client Row";
                    DB.AddSystemMasterClientRow();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.AddSystemMasterAriaUsersRows))
                {
                    currentState = "Inserting System.Master AriaUser Rows";
                    DB.AddSystemMasterAriaUsersRows();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.AddSystemMasterAriaUsersRoles))
                {
                    currentState = "Inserting System.Master AriaUsersRoles Rows";
                    DB.AddSystemMasterAriaUsersRoles();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.AddSystemMasterClientProductRows))
                {
                    currentState = "Inserting System.Master AriaClientProducts Rows";
                    DB.AddSystemMasterAriaClientProductRows();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.DeleteActKey))
                {
                    currentState = "Deleting Old Activiation Key";
                    IO.DeleteActKey();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.CopyActKey))
                {
                    currentState = "Copying New Activiation Key";
                    IO.CopyActKey();
                    success();
                }

                if (settings.NeededActions.Contains(Actions.UpdateSydAppl))
                {
                    currentState = "Updating Aria4 SysFiles SydAppl";
                    DB.UpdateSydAppl();
                    success();
                }

             
                currentState = "Client Created Successfully";
            }
            catch (Exception ex)
            {
                if (LogEnabled)
                {
                    string exDetails = ex.GetExecptionMessage();
                    string msg = string.Format("{0} Failed with the following error:{1}", currentState, exDetails);
                    LogProgress(RequestKey, msg);
                }
                throw;
            }
        }

        void success()
        {
            if (LogEnabled)
            {
                string msg = string.Format("{0} Completed Successfully.", currentState);
                LogProgress(RequestKey, msg);
            }
        }
    }
}
