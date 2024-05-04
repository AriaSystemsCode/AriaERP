using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Aria5.DevExpress.MainSystem.CloudService.TestSara
{
    public partial class configForm : Form
    {
        public configForm()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=azuresqladmin;Password=aria_123";
            Session session = XpoDefault.Session;
            string deviceSign = "176.92|162.47|18.71|34.108";
            string applicationID = "Aria5-Windows8Xaml-1TouchAwayFrontend";
            Guid accountOid = new Guid("d002568f-f55a-46ba-b290-dac589c7e2b6");

         ConfigurationItem x =  GetInformation(session, accountOid, applicationID, deviceSign);
        }

        public  ConfigurationItem GetInformation(Session session, Guid AccountOid, String ApplicationID, String DeviceSignature)
        {
            // ConfigurationItem information_about_running_application_on_specific_device = new ConfigurationItem(XpoDefault.Session);
            //if (session.IsConnected != true) { session.Connect(); }
            //if (session.IsConnected == true)
            {
                # region documentation
                //- If Athunticate then, Calls the Device Manager entity to return the device ID corresponding to the sent device Signature.
                //- If Athunticate then, Searches the ConfigurationItem table for the account, device and application. 
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[AccountId] = '" + AccountID.ToString() + "' and [ApplicationId]='" + ApplicationID + "' and DeviceSignature='" + DeviceSignature + "'"));
                # endregion documentation
                XPCollection<AccountDevice> accountDevice = new XPCollection<AccountDevice>(session, (CriteriaOperator.Parse("DeviceSignature='" + DeviceSignature + "'")));
                if (accountDevice.Count > 0)
                {
                    try
                    {
                        Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("Oid='" + AccountOid + "'"));
                        XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + account.Id + "' and [ApplicationId]='" + ApplicationID + "' and Device='" + accountDevice[0].Oid + "'")));
                        foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                        {
                            //information_about_running_application_on_specific_device.VersionType = item.VersionType;
                            //information_about_running_application_on_specific_device.StartDate = item.StartDate;
                            //information_about_running_application_on_specific_device.EndDate = item.EndDate;
                            //information_about_running_application_on_specific_device.ActivationKeyID = item.ActivationKeyID;

                            return item;

                        }
                    }
                    //sara.m [Start]
                    catch (Exception ex)
                    {
                        Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("Oid='" + AccountOid + "'"));
                        XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + account.Id + "' and [ApplicationId]='" + ApplicationID + "'")));
                        foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                        {
                            //information_about_running_application_on_specific_device.VersionType = item.VersionType;
                            //information_about_running_application_on_specific_device.StartDate = item.StartDate;
                            //information_about_running_application_on_specific_device.EndDate = item.EndDate;
                            //information_about_running_application_on_specific_device.ActivationKeyID = item.ActivationKeyID;

                            return item;

                        }
                    }
                    //sara.m [End]
                }
                //sara.m [Start]
                else
                {
                    Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("Oid='" + AccountOid + "'"));
                    XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + account.Id + "' and [ApplicationId]='" + ApplicationID + "'")));
                    foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                    {
                        //information_about_running_application_on_specific_device.VersionType = item.VersionType;
                        //information_about_running_application_on_specific_device.StartDate = item.StartDate;
                        //information_about_running_application_on_specific_device.EndDate = item.EndDate;
                        //information_about_running_application_on_specific_device.ActivationKeyID = item.ActivationKeyID;

                        return item;

                    }
                }
                //sara.m [End]
                return null;
            }
           // else { return null; }

        }

    }
}
