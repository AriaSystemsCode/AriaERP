using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Layout;
using DevExpress.ExpressApp.Model.NodeGenerators;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Templates;
using DevExpress.ExpressApp.Utils;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.Validation;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.ServiceReferenceTfsManager;
using DevExpress.Xpo;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out https://documentation.devexpress.com/eXpressAppFramework/clsDevExpressExpressAppViewControllertopic.aspx.
    public partial class GenerateFoxBuild : ViewController
    {
        public GenerateFoxBuild()
        {
            InitializeComponent();
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.
        }
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();
            // Access and customize the target View control.
        }
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }

        private void Generatebuild_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
          
            ApplicationBuild_T applicationBuild = ((ApplicationBuild_T)this.View.CurrentObject);

            //MMT
            if (applicationBuild.Status == ApplicationBuildStatus.Complete)
            {
                throw new Exception("Build Status is 'Complete', Cannot generate.");
            }
            if (applicationBuild.Status == ApplicationBuildStatus.Open)
            {
                throw new Exception("Build Status is 'Open', Cannot generate.");
            }
            //MMT

            string PName = applicationBuild.Application_T.Id;
            if (!PName.Contains("4"))
            {
                PName = "EDI";
            }

            if (applicationBuild.ApplicationTrackingEntries.Count > 0)
            {
                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient CreateFixWebServiceClient = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
                CreateFixWebServiceClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
                IObjectSpace ospace = Application.CreateObjectSpace();
               List<string> TrackingEnt = new List<string>();
                foreach (TrackingEntry item in applicationBuild.ApplicationTrackingEntries)
                {
                    TrackingEnt.Add(item.Type.ToString().Substring(0, 1) + item.ID.ToString());
                }
                    if (CreateEnvironment(CreateFixWebServiceClient, applicationBuild.ApplicationBuildId, TrackingEnt, applicationBuild.Application_T.Name, ((ApplicationBuild_T)(this.View.CurrentObject)).Session) == false)
                    {
                        throw new Exception("Can't Create Build Environment");
                    }

                if (!PName.Contains("4"))
                {
                    PName = "A30";
                }
                else
                {
                    PName = "A40";
                }
                if (CreateFixWebServiceClient.createFOXBuild(applicationBuild.ApplicationBuildId, PName))
                {
                    string Buildname = "BD" + applicationBuild.ApplicationBuildId + "R13";
                    applicationBuild.BuildURL="ftp://192.168.1.171/tracking/Builds/"+ PName+"/"+"BD" + applicationBuild.ApplicationBuildId+ PName+ "/" + Buildname  + ".EXE";
                    applicationBuild.Save();
                    //MMT
                    //ospace.CommitChanges();
                    applicationBuild.Session.CommitTransaction();
                    //MMT
                    //Application.ShowViewStrategy.ShowMessage("Fix Number '" + ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "' is succefully created");
                    MessageBox.Show(Application, "Build# " + Buildname + " Successfuly Created ",
                            new Action(delegate
                            {
                                ObjectSpace.Refresh();
                            }));
                }
                else
                {
                    MessageBox.Show(Application, "Build# " + applicationBuild.ApplicationBuildId + "Not Created ",
                            new Action(delegate
                            {
                                ObjectSpace.Refresh();
                            }));
                }
            }
           
        }


        public bool CreateEnvironment(WebService1SoapClient createfixwebserviceclient, string BuildNumber, List<string> TrackingEntry, string ProjectName, Session session)
        {
            ArrayOfString x = new ArrayOfString();
            foreach (var item in TrackingEntry)
            {
                IObjectSpace ospace = Application.CreateObjectSpace();
                TrackingEntry TrackingEntryObj = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[ID] = '" + item.Substring(1,6) + "'"));
                string resourceName = "";
                if (TrackingEntryObj != null)
                {
                    TrackingTask TrackingTask = TrackingEntryObj.TrackingTasks.Where(t => t.Task.CheckOut == true).FirstOrDefault();
                    if (TrackingTask != null && TrackingTask.Resources != null)
                        resourceName = TrackingTask.Resources.Name;
                }
                if (resourceName.Trim().Length > 1)
                {
                    x.Add(item+"-"+ resourceName.Trim());
                }
                else
                {
                    x.Add(item);
                }


            }
             bool isCreated;

                    isCreated = createfixwebserviceclient.CreateBuildEnvironment(BuildNumber,x, ProjectName);
                    if (isCreated)
                    {
                        isCreated = CraeteAttachementfolder(createfixwebserviceclient, TrackingEntry, ProjectName, session);

                    }
            return isCreated;
        }
        public bool CraeteAttachementfolder(WebService1SoapClient createfixwebserviceclient, List<string> TrackingEntries, string PName,Session session)
        {   
            
            IList<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments> allattachments = new List<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
            List<AriaObjectShelve> Shelves = new List<AriaObjectShelve>();
            
           
            foreach (string TrackingEntryID in TrackingEntries)
            {
                
                TrackingEntry TrackingEntry = session.FindObject<TrackingEntry>(CriteriaOperator.Parse("[ID] = '" + TrackingEntryID.Substring(1) + "'"));
                Shelves.AddRange(TrackingEntry.AriaObjectShelves);
                if (TrackingEntry.HasChildren)
                {
                    foreach (TrackingEntry Child in TrackingEntry.ChildTrackingEntries)
                    {
                        Shelves.AddRange(Child.AriaObjectShelves);
                    }
                }

            }
            
            
            foreach (AriaObjectShelve objectShe in Shelves)
            {
                if (objectShe.ObjectType.Name == "AriaFile")
                {
                    allattachments.Add(getthefilefieldsysfile(createfixwebserviceclient, objectShe));
                }
                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments[] attachments = CreateAttachmentRecordforsys(objectShe, ((ApplicationBuild_T)View.CurrentObject).ApplicationBuildId, PName);
                if (attachments.Count() > 0)
                {
                    foreach (Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments sysfileattachement in attachments)
                    {
                        if (allattachments.FirstOrDefault(x => x.Name == sysfileattachement.Name && x.Type == sysfileattachement.Type) == null)
                            allattachments.Add(sysfileattachement);
                    }
                    Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray[] arrayofobjects = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray[objectShe.AriaObjectShelveSettings.Where(c => !string.IsNullOrEmpty(c.ActualColumnName)).Count()];
                    if (objectShe.AriaObjectShelveSettings.Count > 0)
                    {
                        int rows = 0;
                        foreach (AriaObjectShelveSetting setting in objectShe.AriaObjectShelveSettings)
                        {
                            if (setting.ActualColumnName != null && !string.IsNullOrEmpty(setting.ActualColumnName)&& !string.IsNullOrEmpty(setting.Value))
                            {

                                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray oneobject = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray();
                                oneobject.columnname = setting.ActualColumnName.ToLower().Trim();
                                oneobject.valuename = setting.Value.Trim();
                                arrayofobjects[rows] = oneobject;
                                rows++;
                            }
                        }
                    }
                    string modificationtype = objectShe.ModificationType.ToString().Substring(0, 1);
                    
                    PName = "A30";
                    createfixwebserviceclient.Createsysfiletable(arrayofobjects, ((ApplicationBuild_T)View.CurrentObject).ApplicationBuildId, attachments[0].Name.ToUpper(), attachments[0].Tag, "BD", PName);
                    
                }
                else
                {
                    attachments = getattachmentsnotsystem(objectShe);
                    foreach (Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments item in attachments)
                    {
                        allattachments.Add(item);
                    }
                }
            }
            return createfixwebserviceclient.CreateBuildattachmenttable(allattachments.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>(), ((ApplicationBuild_T)this.View.CurrentObject).ApplicationBuildId, ((ApplicationBuild_T)this.View.CurrentObject).ApplicationId);

        }
        public Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments[] getattachmentsnotsystem(AriaObjectShelve Obj)
        {
            IList<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments> listofattachments = new List<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments atta = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments();
            atta.Name = Obj.ObjectName;
            string subfolder = Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value;
            int lindx = subfolder.IndexOf(")");
            string source = "";

            if (Obj.TrackingEntry.Application.Id.Contains("4"))
            {
                if (subfolder.ToUpper().Contains("ARIA"))
                    source = subfolder.Remove(0, lindx + 2);
                source = @"D:\Aria4xp\ARIA4XP\Aria4xp (R13)\" + source + "\\";
            }
            else
            {
                source = Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value.Replace("$", @"D:") + "\\";
            }
            atta.Source = source;// Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value.Replace("$",@"D:")+"\\";
            atta.Dest = Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value + "\\";
            //MMT
            atta.Ticket = Obj.TrackingEntry.ReferenceNo.ToString().Trim();
            atta.TrackingNo = Obj.TrackingEntry.Type.ToString().Trim ().Substring(0, 1) + Obj.TrackingEntry.ID.ToString().Trim ();
            atta.cTrksDsc = Obj.TrackingEntry.Description.ToString().Trim();
            //MMT
            atta.Type = Obj.ObjectType.Name;
            listofattachments.Add(atta);  
            return listofattachments.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
        }
        public Module.ServiceReferenceTfsManager.Attachments getthefilefieldsysfile(WebService1SoapClient createfixwebserviceclient, AriaObjectShelve ariaObjectShelve)
        {
            if (ariaObjectShelve.AriaObjectShelveProperties.Where(p => p.IsNew == true && p.PropertyType.Name == "AriaField").Count() > 0)
            {
                foreach (AriaObjectShelveProperty shelveprop in ariaObjectShelve.AriaObjectShelveProperties.Where(p => p.IsNew == true && p.PropertyType.Name == "AriaField"))
                {
                    Module.ServiceReferenceTfsManager.Testarray[] arrayofobjects = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray[3];
                    Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray oneobject = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray();
                    oneobject.columnname = "cfile_nam";
                    oneobject.valuename = ariaObjectShelve.ObjectName;
                    arrayofobjects[0] = oneobject;
                    oneobject.columnname = "cfld_name";
                    oneobject.valuename = shelveprop.PropertyName;
                    arrayofobjects[1] = oneobject;

                    // string modificationtype = ariaObjectShelve.ModificationType.ToString().Substring(0, 1);
                    // Sara.n , Take fix Type [Start]
                    //createfixwebserviceclient.Createsysfiletable(arrayofobjects, "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), "SYDFLFLD.DBF", "CFLFLD", "A");
                    createfixwebserviceclient.Createsysfiletable(arrayofobjects, ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), "SYDFLFLD.DBF", "CFLFLD", "A","A30");

                    // Sara.n , Take fix Type [Start]
                }
                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments atta = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments();
                atta.Source = "D:\\Aria4xp\\ARIA4XP\\ARIA4XP (R13)\\sysfiles\\";
                // Sara.n , Take fix Type [Start]
                //atta.Dest = "D:\\Tracking\\Fixes\\" + "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "\\Attachments\\Sysfiles\\";
                atta.Dest = "D:\\Tracking\\Fixes\\" + ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "\\Attachments\\Sysfiles\\";
                // Sara.n , Take fix Type [End]
                atta.Type = "System";
                atta.Name = "SYDFLFLD.DBF";
                atta.Tag = "CFLFLD";
                return atta;
            }
            else
            {
                return null;
            }
        }

        public Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments[] CreateAttachmentRecordforsys(AriaObjectShelve Object, string Path,string PNAME)
        {
            IList<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments> attach = new List<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments atta = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments();
            atta.Source = "D:\\Aria4xp\\ARIA4XP\\ARIA4XP (R13)\\sysfiles\\";
            atta.Dest = "D:\\Tracking\\Builds\\BD" + Path +PNAME+ "\\Attachments\\Sysfiles\\";
            atta.Type = "System";

            switch (Object.ObjectType.ObjectTypeID.ToUpper())
            {
                case "ARIAFIELD":
                    atta.Name = "SYDFIELD.DBF";
                    atta.Tag = "CFLD_NAME";
                    break;
                case "DATA":
                    atta.Name = "SYDFIELS.DBF";
                    atta.Tag = "CFILE_NAM";
                    break;
                case "ARIAINDEX":
                    atta.Name = "SYDINDEX.DBF";
                    atta.Tag = "CFILE_NAM";
                    break;
                case "ARIAOBJECT":
                    atta.Name = "SYDOBJCT.DBF";
                    atta.Tag = "CAPP_ID";
                    break;
                case "ARIAREPORT":
                    atta.Name = "SYDREPRT.DBF";
                    atta.Tag = "CREP_ID";
                    break;
                case "OPTIONGRID":
                    atta.Name = "SYREPUVR.DBF";
                    atta.Tag = "CREP_ID";
                    break;
                case "ARIATRIGGER":
                    atta.Name = "SYCTRIGG.DBF";
                    atta.Tag = "OBJEVENT";
                    break;
                case "MENU":
                    atta.Name = "SYCMENU.DBF";
                    atta.Tag = "MENUUQ";
                    break;
                default:
                    atta = null;
                    break;
            }
            if (atta != null)
                attach.Add(atta);
            return attach.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
        }
    }
}
