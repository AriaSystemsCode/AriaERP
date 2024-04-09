using System;
using System.Collections.Generic;
using System.Text;
using NetFwTypeLib;
using System.Management;
using System.Security.Principal;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.Windows.Forms;

namespace Aria.Configuration.Server
{
    internal class SafeTokenHandle : SafeHandleZeroOrMinusOneIsInvalid
    {
        private SafeTokenHandle()
            : base(true)
        {
        }

        internal SafeTokenHandle(IntPtr handle)
            : base(true)
        {
            base.SetHandle(handle);
        }

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        internal static extern bool CloseHandle(IntPtr handle);

        protected override bool ReleaseHandle()
        {
            return CloseHandle(base.handle);
        }
    }
    
    class SecurityUtilities
    {
        [DllImport("advapi32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
        public static extern bool LogonUser(String lpszUsername, String lpszDomain, String lpszPassword,
            int dwLogonType, int dwLogonProvider, out SafeTokenHandle phToken);

        public static void SetThreadUser(string domain, string userID, string password)
        {
            IntPtr x = WindowsIdentity.GetCurrent().Token;

            const int LOGON32_PROVIDER_DEFAULT = 0;
            //This parameter causes LogonUser to create a primary token.
            const int LOGON32_LOGON_INTERACTIVE = 2;

            SafeTokenHandle safeTokenHandle;

            bool result = LogonUser(userID, domain, password, LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, out safeTokenHandle);

            using (WindowsImpersonationContext impersonatedUser = WindowsIdentity.Impersonate(safeTokenHandle.DangerousGetHandle()))
            {

                // Check the identity.
                MessageBox.Show("After impersonation: "+ WindowsIdentity.GetCurrent().Name);

                System.IO.File.Create(@"c:\temp\a1.txt");

            }

            WindowsIdentity.Impersonate(x);
        }

        public static void AuthorizeApplication(string applicationName, string applicationPath)
        {
            Type NetFwMgrType = Type.GetTypeFromProgID("HNetCfg.FwMgr", false);
            INetFwMgr mgr = (INetFwMgr)Activator.CreateInstance(NetFwMgrType);
            bool Firewallenabled = mgr.LocalPolicy.CurrentProfile.FirewallEnabled;

            INetFwAuthorizedApplications applications;
            Type type = Type.GetTypeFromProgID("HNetCfg.FwAuthorizedApplication");
            INetFwAuthorizedApplication application = (INetFwAuthorizedApplication)Activator.CreateInstance(type);
            application.Name = applicationName;
            application.ProcessImageFileName = applicationPath.Replace("\"", "");
            application.Enabled = true; //enable it

            applications = (INetFwAuthorizedApplications)mgr.LocalPolicy.CurrentProfile.AuthorizedApplications;
            applications.Add(application);
        }

        public static bool IsApplicationAuthorized(string applicationPath)
        {
            Type NetFwMgrType = Type.GetTypeFromProgID("HNetCfg.FwMgr", false);
            INetFwMgr mgr = (INetFwMgr)Activator.CreateInstance(NetFwMgrType);
            bool Firewallenabled = mgr.LocalPolicy.CurrentProfile.FirewallEnabled;

            if (Firewallenabled)
            {
                INetFwAuthorizedApplications applications;
                applications = (INetFwAuthorizedApplications)mgr.LocalPolicy.CurrentProfile.AuthorizedApplications;

                foreach (INetFwAuthorizedApplication application in applications)
                {
                    if (application.ProcessImageFileName.ToUpper().Trim() == applicationPath.Replace("\"", "").ToUpper().Trim())
                    {
                        return true;
                    }
                }

                return false;
            }
            else
            {
                return true;
            }
        }

        public static List<string> GetUsers(String DomainName, String GroupName)
        {
            List<string> result = new List<string>();

            #region Build WMI query using normal SQL statement
            ///<summary>
            /// Build our query
            /// Be careful in adding double quotes and single quotes
            /// Thats why I have used StringBuilder Class to avoid confusion
            ///</summary>
            //StringBuilder sBuilder = new StringBuilder("select * from Win32_GroupUser where ");
            //sBuilder.Append("GroupComponent=");
            //sBuilder.Append('"');
            //sBuilder.Append("Win32_Group.Domain='CHAKS-PC',Name='Users'");
            //sBuilder.Append('"');
            #endregion

            #region Build WMI query using SelectQuery
            ///<summary>
            /// Alternate method for building query
            /// Which I think is better approach
            ///</summary>
            StringBuilder sBuilder = new StringBuilder("GroupComponent=");
            sBuilder.Append('"');
            sBuilder.Append("Win32_Group.Domain=");
            sBuilder.Append("'");
            sBuilder.Append(DomainName);
            sBuilder.Append("'");
            sBuilder.Append(",Name=");
            sBuilder.Append("'");
            sBuilder.Append(GroupName);
            sBuilder.Append("'");
            sBuilder.Append('"');
            SelectQuery sQuery = new SelectQuery("Win32_GroupUser", sBuilder.ToString());
            #endregion

            ///<summary>
            /// Execute the query
            /// Construct a ManagementPath from the PartComponent and check for ClassName
            /// and extract the UserName
            /// Depending on which method you used to build the query,
            /// pass the String or SelectQuery object to ManagementObjectSearcher
            ///</summary>
            try
            {
                ManagementObjectSearcher mSearcher = new ManagementObjectSearcher(sQuery);

                foreach (ManagementObject mObject in mSearcher.Get())
                {
                    ManagementPath path = new ManagementPath(mObject["PartComponent"].ToString());

                    if (path.ClassName == "Win32_UserAccount")
                    {
                        String[] names = path.RelativePath.Split(',');
                        result.Add(names[0].Substring(names[0].IndexOf("=") + 1).Replace('"', ' ').Trim() + @"\" + names[1].Substring(names[1].IndexOf("=") + 1).Replace('"', ' ').Trim());
                    }
                }
            }
            catch (Exception ex)
            {
                //Console.WriteLine(ex.ToString());
            }

            return result;
        }
    }
}
