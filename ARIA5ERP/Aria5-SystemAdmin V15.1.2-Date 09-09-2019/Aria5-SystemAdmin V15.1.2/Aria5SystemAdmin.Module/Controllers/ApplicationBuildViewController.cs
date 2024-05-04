using System;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp;
using DevExpress.Data.Filtering;
using System.Collections.Generic;
using DevExpress.Persistent.Base;
using DevExpress.ExpressApp.Utils;
using DevExpress.ExpressApp.Layout;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Templates;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Model.NodeGenerators;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Web.SystemModule;
using Aria5SystemAdmin.Module.Managers;
using System.Net;
using Microsoft.TeamFoundation.Client;
using Microsoft.TeamFoundation.VersionControl.Client;
using System.Web.Configuration;
using Microsoft.TeamFoundation.Build.Client;

namespace Aria5SystemAdmin.Module.Controllers
{

   
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class ApplicationBuildViewController : ViewController
    {
        public static Guid currentApplicationBuild;
        public ApplicationBuildViewController()
        {
            InitializeComponent();
            RegisterActions(components);
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
        private void ApplicationBuildViewController_ViewControlsCreated(object sender, EventArgs e)
        {
            if (this.View.CurrentObject != null && this.View.CurrentObject.GetType() == typeof(ApplicationBuild_T))
            {
                currentApplicationBuild = ((ApplicationBuild_T)this.View.CurrentObject).Oid;
            }
        }
        private void ApplicationBuildViewController_Activated(object sender, EventArgs e)
        {

            if (this.View.CurrentObject != null && this.View.CurrentObject.GetType() == typeof(ApplicationBuild_T))
            {
                currentApplicationBuild = ((ApplicationBuild_T)this.View.CurrentObject).Oid;
                //MMT
                ApplicationBuild_T currentBuild = ((ApplicationBuild_T)this.View.CurrentObject);
                if (currentBuild.Status == ApplicationBuildStatus.Approved)
                {
                    Frame.GetController<ApplicationBuildViewController>().Actions["ApproveAction"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ApplicationBuildViewController>().Actions["GenerateBuildAction"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<ApplicationBuildViewController>().Actions["CompleteBuildAction"].Enabled.SetItemValue("Disable", false);
                }
                if (currentBuild.Status == ApplicationBuildStatus.Complete)
                {
                    Frame.GetController<ApplicationBuildViewController>().Actions["ApproveAction"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ApplicationBuildViewController>().Actions["CompleteBuildAction"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ApplicationBuildViewController>().Actions["GenerateBuildAction"].Enabled.SetItemValue("Disable", false);
                   // Frame.GetController<ApplicationBuildViewController>().Actions["a5c5624f-8c22-480b-b152-4dfccee7614a"].Enabled.SetItemValue("Disable", false);
                }
                if (currentBuild.Status == ApplicationBuildStatus.ReleaseCandidate)
                {
                    Frame.GetController<ApplicationBuildViewController>().Actions["ApproveAction"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ApplicationBuildViewController>().Actions["GenerateBuildAction"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ApplicationBuildViewController>().Actions["CompleteBuildAction"].Enabled.SetItemValue("Disable", true);
                }
                if (currentBuild.Status == ApplicationBuildStatus.Open)
                {
                   // Frame.GetController<ApplicationBuildViewController>().Actions["ApproveAction"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ApplicationBuildViewController>().Actions["GenerateBuildAction"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ApplicationBuildViewController>().Actions["CompleteBuildAction"].Enabled.SetItemValue("Disable", false);
                }

                //MMT
            }



        }
        private void GenerateController_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            ApplicationBuild_T applicationBuild = ((ApplicationBuild_T)this.View.CurrentObject);
            bool flag = Generate(applicationBuild, "Testing");
            if (flag)
            {
                applicationBuild.Status = ApplicationBuildStatus.Staging;
                applicationBuild.Save();

                MessageBox.Show(Application, "Build# " + applicationBuild.ApplicationBuildId + " has been successfully generated for testing.",
                         new Action(delegate
                         {
                             ObjectSpace.Refresh();
                         }));

                applicationBuild.Session.CommitTransaction();
                Frame.View.ObjectSpace.Refresh();
            }
        }
        public bool Generate(ApplicationBuild_T applicationBuild, string state)
        {

            //MMT
            

            //applicationBuild.Application_T.Name
            if (applicationBuild.Status == ApplicationBuildStatus.Complete)
            {
                throw new Exception("Build Status is 'Complete', Cannot generate.");
            }
            if (applicationBuild.Status == ApplicationBuildStatus.Open )
            {
                throw new Exception("Build Status is 'Open', Cannot generate.");
            }
            string PName = applicationBuild.Application_T.Id;
            if (!PName.Contains("4"))
            {
                PName = "EDI";
            }
            GenerateFoxBuild  generateFoxBuild = Frame.GetController<GenerateFoxBuild>();
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
                if (generateFoxBuild.CreateEnvironment(CreateFixWebServiceClient, applicationBuild.ApplicationBuildId, TrackingEnt, applicationBuild.Application_T.Name, ((ApplicationBuild_T)(this.View.CurrentObject)).Session) == false)
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
                    applicationBuild.BuildURL = "ftp://192.168.1.171/tracking/Builds/" + PName + "/" + "BD" + applicationBuild.ApplicationBuildId + PName + "/" + Buildname + ".EXE";
                    applicationBuild.Save();
                    //MMT
                    //ospace.CommitChanges();
                    applicationBuild.Session.CommitTransaction();
                    //MMT
                    //Application.ShowViewStrategy.ShowMessage("Fix Number '" + ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "' is succefully created");
                    // MessageBox.Show(Application, "Build# " + Buildname + " Successfuly Created ",
                    //new Action(delegate
                    //{
                    //    ObjectSpace.Refresh();
                    //}));
                    return true;
                }
                else
                {
                    //MessageBox.Show(Application, "Build# " + applicationBuild.ApplicationBuildId + "Not Created ",
                    //        new Action(delegate
                    //        {
                    //            ObjectSpace.Refresh();
                    //        }));
                    return false;
                }
            }
            else
            {
                return false;
            }

            //MMT


         //   if (applicationBuild.Application_T.Name == "Aria4XP")
         //   {
         //       Aria5SystemAdmin.Module.ServiceReferenceTfsManager.ArrayOfString trackingnumbers = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.ArrayOfString();
         //       //foreach (TrackingEntry item in applicationBuild.ApplicationBuildEntries_Ts)
         //       //{
         //       //    trackingnumbers.Add(item.TrackingEntryId);
         //       //}
         //       foreach (TrackingEntry item in applicationBuild.ApplicationTrackingEntries)
         //       {
         //           trackingnumbers.Add(item.ID.ToString());
         //       }
         //       Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient newchannel = new  Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
         //       newchannel.CreateBuild(trackingnumbers,"B","A40",applicationBuild.Application_T.ReleaseNo,applicationBuild.Application_T.ServicePack,int.Parse(applicationBuild.ApplicationBuildId));

         //   }
         //   bool flag = false;
         //   IObjectSpace objectSpace = Application.CreateObjectSpace();
         //   if (applicationBuild.ApplicationTrackingEntries.Count > 0)
         //   {
         //       //    foreach (Tracking applicationBuildEntry in applicationBuild.ApplicationBuildEntries_Ts)
         //           foreach (TrackingEntry applicationBuildEntry in applicationBuild.ApplicationTrackingEntries)
         //       {
         //           //if (applicationBuildEntry.TrackingEntry.ChildTrackingEntries.Count > 0)
         //               if (applicationBuildEntry.ChildTrackingEntries.Count > 0)
         //               {
         //              // foreach (var childTrackingEntry in applicationBuildEntry.TrackingEntry.ChildTrackingEntries)
         //                   foreach (var childTrackingEntry in applicationBuildEntry.ChildTrackingEntries)
         //                   {
         //                   //var objectExist = applicationBuild.Session.FindObject<ApplicationBuildEntries_T>(CriteriaOperator.Parse("[TrackingEntry] ='" + childTrackingEntry.Oid + "' AND [ApplicationBuild_T]='" + applicationBuild.Oid + "'"));
         //                   var objectExist = applicationBuild.Session.FindObject<TrackingEntry>(CriteriaOperator.Parse("[ID] ='" + childTrackingEntry.ID + "' AND [ApplicationBuild]='" + applicationBuild.Oid + "'"));

         //                   if (objectExist == null)
         //                   {
         //                      // MessageBox.Show(Application, "Tracking entry #" + applicationBuildEntry.TrackingEntry.ID + " has a dependency on a non-attached tracking entry #" + childTrackingEntry.ID + ". Make sure that all dependent tracking entries are attached to the build.",

         //                       MessageBox.Show(Application, "Tracking entry #" + applicationBuildEntry.ID.ToString() + " has a dependency on a non-attached tracking entry #" + childTrackingEntry.ID + ". Make sure that all dependent tracking entries are attached to the build.",
         //                           new Action(delegate
         //                           {
         //                               ObjectSpace.Refresh();
         //                           }));

         //                       flag = false;
         //                       return flag;
         //                   }

         //               }
         //           }
         //       }
         //   }
         //   else
         //   {
         //       MessageBox.Show(Application, "No tracking entries are linked. Can't proceed with generating the build.",
         //            new Action(delegate
         //            {
         //                ObjectSpace.Refresh();
         //            }));
         //       flag = false;
         //       return flag;
         //   }
         //   #region  call Aria5-DevExpress-ApplicationPackageGenerator

         //   if (state.Trim().ToUpper() == "LIVE") ApplicationPackageManager.Generate(applicationBuild, ApplicationPackageManager.Buildtype.Production);
         //   if (state.Trim().ToUpper() != "LIVE") ApplicationPackageManager.Generate(applicationBuild, ApplicationPackageManager.Buildtype.Staging);

         //   TfsManager tfs = new TfsManager();
       
         //   // Sara.N 8-11-2015, Build Task [Start]
         //   //   tfs.CreateWorkSpace(applicationBuild);
         //   ApplicationSetting tfsMappingPath = applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFSMappingPath".Trim().ToUpper());
         //   ApplicationSetting InitialLabel = applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "InitialLabel".Trim().ToUpper());
         //   ApplicationSetting LastLabel = applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "LastLabel".Trim().ToUpper());
         //   ApplicationSetting TfsProjectName = applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TfsProjectName".Trim().ToUpper());
         //   ApplicationSetting TeamProject = applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TeamProject".Trim().ToUpper());

         //   ApplicationSetting CurrentBuildLabel = applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "CurrentBuildLabel".Trim().ToUpper());
         //  //ATA 21/9/2016 test create work space function work or not [start]

         //   //TfsUserName = ;
         //   //TfsPassWord =WebConfigurationManager.AppSettings["tfsUserName"].ToString(), WebConfigurationManager.AppSettings["tfsPassWord"].ToString();
         //   //TfsDomain = WebConfigurationManager.AppSettings["tfsDomain"].ToString();
         //   //TfsProjectName = TFSProject;
         //   //TfsGroupName = WebConfigurationManager.AppSettings["tfsGroupName"].ToString();
         //   //TfsCollection = TFSCollection;
         //   //BuildWorkSapce = WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString();
         //   //TfsWorkSpacePath = WebConfigurationManager.AppSettings["tfsWorkSpacePath"].ToString();
         //   //TfsCheckinWorkSpace = WebConfigurationManager.AppSettings["tfsCheckinWorkSpaceCreated"].ToString();
         //  // CreatelocalWorkSpace(tfsMappingPath.SettingValue.ToString(), InitialLabel.SettingValue.ToString(), LastLabel.SettingValue.ToString(),WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString(),WebConfigurationManager.AppSettings["tfsUserName"].ToString(), WebConfigurationManager.AppSettings["tfsPassWord"].ToString(),WebConfigurationManager.AppSettings["tfsDomain"].ToString(),WebConfigurationManager.AppSettings["TFSCollection"].ToString(),WebConfigurationManager.AppSettings["tfsWorkSpacePath"].ToString());
         //   // ATA 
                    
         //   tfs.CreateWorkSpace(tfsMappingPath.SettingValue.ToString(), InitialLabel.SettingValue.ToString(), LastLabel.SettingValue.ToString());
         //   // Sara.N 8-11-2015, Build Task [End]

         //   //foreach (var tracking in applicationBuild.ApplicationBuildEntries_Ts.ToList().OrderByDescending(r => r.ID.ToString()))
         //   //{
         //   //    if (tracking.TrackingEntry.Entity.AriaObjectRevisions.Where(r => r.TrackingNo != null && r.TrackingNo.ID == tracking.TrackingEntry.ID).Count() > 0)
         //   //    {
         //   //        var rev = tracking.TrackingEntry.Entity.AriaObjectRevisions.First(r => r.TrackingNo != null && r.TrackingNo.ID == tracking.TrackingEntry.ID);

         //   //        if (rev.AriaObjectSettings.Where(r => r.SettingType != null && r.SettingType.Name != null && r.SettingType.Name.ToUpper().Trim() == "FilePath".ToUpper().Trim()).Count() > 0)
         //   //        {
         //   //            var settingFilePath = rev.AriaObjectSettings.First(r => r.SettingType != null && r.SettingType.Name != null && r.SettingType.Name.ToUpper().Trim() == "FilePath".ToUpper().Trim()).Value.ToString();

         //   //            string metadata = tracking.TrackingEntry.Entity.SpliteMetadata(tracking.TrackingEntry.Entity.Oid, rev.Oid);


         //   //            tfs.UpdateMetadataTFS(metadata, settingFilePath, applicationBuild);
         //   //        }
         //   //    }

         //   //    foreach (var shelve in tracking.TrackingEntry.AriaObjectShelves)
         //   //    {

         //   //        if (!String.IsNullOrWhiteSpace(shelve.ChangeSet)) tfs.GetSpecificChangeset(applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim() == "TFSMappingPath").ToString(), Convert.ToInt32(shelve.ChangeSet), shelve.AriaObject.AriaObjectSettings.First(s => s.SettingType.SettingTypeId.Trim() == "StorageFileName").ToString());
         //   //    }


         //       foreach (var tracking in applicationBuild.ApplicationTrackingEntries.ToList().OrderByDescending(r => r.ID))
         //   {
         //       if (tracking.Entity.AriaObjectRevisions.Where(r => r.TrackingNo != null && r.TrackingNo.ID == tracking.ID).Count() > 0)
         //       {
         //           var rev = tracking.Entity.AriaObjectRevisions.First(r => r.TrackingNo != null && r.TrackingNo.ID == tracking.ID);

         //           if (rev.AriaObjectSettings.Where(r => r.SettingType != null && r.SettingType.Name != null && r.SettingType.Name.ToUpper().Trim() == "FilePath".ToUpper().Trim()).Count() > 0)
         //           {
         //               var settingFilePath = rev.AriaObjectSettings.First(r => r.SettingType != null && r.SettingType.Name != null && r.SettingType.Name.ToUpper().Trim() == "FilePath".ToUpper().Trim()).Value.ToString();

         //               string metadata = tracking.Entity.SpliteMetadata(tracking.Entity.Oid, rev.Oid);

                     
         //               tfs.UpdateMetadataTFS(metadata, settingFilePath, applicationBuild);
         //           }
         //       }

         //       foreach (var shelve in tracking.AriaObjectShelves)
         //       {
                   
         //          if (!String.IsNullOrWhiteSpace(shelve.ChangeSet)) tfs.GetSpecificChangeset(applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim() == "TFSMappingPath").ToString(), Convert.ToInt32(shelve.ChangeSet), shelve.AriaObject.AriaObjectSettings.First(s => s.SettingType.SettingTypeId.Trim() == "StorageFileName").ToString());
         //       }
         //   }
         //   //ATA 21/9/2016   [start]
         //   //CreatelocalWorkSpaceLabel(tfsMappingPath.SettingValue.Trim().ToString(), CurrentBuildLabel.SettingValue.Trim().ToUpper().ToString(), WebConfigurationManager.AppSettings["tfsUserName"].ToString(), WebConfigurationManager.AppSettings["tfsPassWord"].ToString(), WebConfigurationManager.AppSettings["tfsDomain"].ToString(), WebConfigurationManager.AppSettings["TFSCollection"].ToString(), WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString());
         ////ATA 21/9/2016   [End]
         //       tfs.CreateWorkSpaceLabel(tfsMappingPath.SettingValue.Trim().ToString(), CurrentBuildLabel.SettingValue.Trim().ToUpper().ToString());

         //   ApplicationSetting buildDef = applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID == "BuildDefintion");

         //   //tfs.BuildDeploy(applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim() == "CurrentBuildLabel").ToString(), applicationBuild.Application_T.ApplicationSettings.First(r => r.SettingID.Trim() == "TFS Main Project name").ToString(), buildDef.SettingValue.ToString().Trim());
         //  //ATA 21/9/2016 [start]
         //  // BuildlocalDeploy(CurrentBuildLabel.SettingValue.Trim().ToUpper().ToString(), TeamProject.SettingValue.ToString(), buildDef.SettingValue.ToString().Trim(), WebConfigurationManager.AppSettings["tfsUserName"].ToString(), WebConfigurationManager.AppSettings["tfsPassWord"].ToString(), WebConfigurationManager.AppSettings["tfsDomain"].ToString(), WebConfigurationManager.AppSettings["TFSCollection"].ToString());
         //   //ATA 21/9/2016 [end]
         //    tfs.BuildDeploy(CurrentBuildLabel.SettingValue.Trim().ToUpper().ToString(), TeamProject.SettingValue.ToString(), buildDef.SettingValue.ToString().Trim());
 
         //   LastLabel.SettingValue = CurrentBuildLabel.SettingValue;
         //   LastLabel.Save();
         //   LastLabel.Session.CommitTransaction();
         //   DevExpress.XtraEditors.XtraMessageBox.Show("Build deployement succeeded!");
         //   flag = true;
         //   #endregion

         //   if (state == "Testing")
         //   {
         //       //flag that it's a testing version
         //   }
         //   if (state == "Live")
         //   {
         //       //flag that it's a live version
         //   }
           // return flag;
        } 
        private void ApproveController_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            ApplicationBuild_T applicationBuild = (ApplicationBuild_T)e.CurrentObject;

            bool flag = true;//Generate(applicationBuild, "Live");

            if (flag)
            {
                Application_T app = applicationBuild.Session.GetObjectByKey<Application_T>(applicationBuild.Application_T.Oid);


                XPCollection collectionApp = new XPCollection(applicationBuild.Session, typeof(Application_T), CriteriaOperator.Parse("Oid = '" + applicationBuild.Application_T.Oid + "'"));
                collectionApp.Load();
                collectionApp.Sorting.Add(new SortProperty("BuildNo", DevExpress.Xpo.DB.SortingDirection.Ascending));

                //MMT
               /* if (applicationBuild.Status == ApplicationBuildStatus.Open)
                {
                    //MMT
                    int appBuild = Convert.ToInt32(((Application_T)collectionApp[0]).BuildNo);
                    int appNextBuild = appBuild + 1;
                    app.BuildNo = appNextBuild.ToString();
                    //MMT
                    //applicationBuild.ApplicationBuildId = app.BuildNo;
                }*/
                //MMT
                //applicationBuild.Status = ApplicationBuildStatus.ReleaseCandidate;
                applicationBuild.Status = ApplicationBuildStatus.Approved ;
                //MMT
                applicationBuild.ApproveDate = DateTime.Today;
                applicationBuild.ApprovedBy = SecuritySystem.CurrentUserName;





                app.Save();
                applicationBuild.Save();
                //MMT
                //MessageBox.Show(Application, "Build# " + applicationBuild.ApplicationBuildId + " has been successfully generated for production.",
                //   new Action(delegate
                //   {
                //       ObjectSpace.Refresh();
                //   }));
                MessageBox.Show(Application, "Build# " + applicationBuild.ApplicationBuildId + " has been successfully approved.",
                   new Action(delegate
                   {
                       ObjectSpace.Refresh();
                   }));
                //MMT
                applicationBuild.Session.CommitTransaction();
                //MMT
                //Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", true);
                //MMT
                flag = Generate(applicationBuild, "Live");

                Frame.View.ObjectSpace.Refresh();
            }
        }
        private void CompleteController_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            ApplicationBuild_T applicationBuild = (ApplicationBuild_T)e.CurrentObject;

            //MMT
            ApplicationBuild_T appBuild = objectSpace.GetObjectByKey<ApplicationBuild_T>(applicationBuild.Oid);
            //MMT
            Application_T app = objectSpace.GetObjectByKey<Application_T>(applicationBuild.Application_T.Oid);
            XPCollection collectionApp = new XPCollection(applicationBuild.Session, typeof(Application_T), CriteriaOperator.Parse("Oid = '" + applicationBuild.Application_T.Oid + "'"));
            collectionApp.Load();
            collectionApp.Sorting.Add(new SortProperty("BuildNo", DevExpress.Xpo.DB.SortingDirection.Ascending));
            if (applicationBuild.Status == ApplicationBuildStatus.ReleaseCandidate)
            {
                
                int appBuildNo = Convert.ToInt32(((Application_T)collectionApp[0]).BuildNo);
                int appNextBuild = appBuildNo + 1;
                app.BuildNo = appNextBuild.ToString();
                app.Save();
                //MMT
                //applicationBuild.ApplicationBuildId = app.BuildNo;
            }
            //MMT

            List<AriaObject> updateEntities = new List<AriaObject>();
            List<TrackingEntry> trackingEntries = new List<TrackingEntry>();
            TrackingEntryViewController trackingEntryViewController = Frame.GetController<TrackingEntryViewController>();
            foreach (TrackingEntry itemtr in applicationBuild.ApplicationTrackingEntries)
            {
                
                TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(itemtr.Oid);
                //*
                #region Check Child Entites
                bool notCompleteFlag = false;
                if (trackingEntry.ChildTrackingEntries.Count > 0)
                {
                    foreach (var childTrackingEntry in trackingEntry.ChildTrackingEntries)
                    {

                        if (childTrackingEntry.Status != TrackingEntry.TrackingStatus.Complete)
                        {
                            notCompleteFlag = true;

                            throw new Exception("Cannot complete tracking entry before complete children tracking entries.");
                        }
                        else
                        {
                            notCompleteFlag = false;
                        }
                    }
                }
                #endregion
                //trackingEntryViewController.
                Guid AiraObjectOid = new Guid () ;
                if (notCompleteFlag == false)
                {
                    #region TrackingEntry Entity New
                    switch (itemtr.ModificationType)
                    {
                        case TrackingEntry.ModificationTypes.Add:
                             AiraObjectOid = trackingEntryViewController.AddNewObject(trackingEntry);

                            foreach (AriaObjectShelve item in trackingEntry.AriaObjectShelves)
                            {
                                if (itemtr.ModificationType == TrackingEntry.ModificationTypes.Add &&
                                   item.ModificationType == AriaObjectShelve.ModificationTypes.Modify)
                                {

                                    // ((XPObjectSpace)this.ObjectSpace).Session.Connection.State == System.Data.ConnectionState.

                                    item.AriaObject = objectSpace.GetObjectByKey<AriaObject>(AiraObjectOid);

                                    trackingEntryViewController.AddSetSettings(item);
                                    trackingEntryViewController.AddNewProperties(item);
                                    trackingEntryViewController.AddNewMethods(item);
                                    trackingEntryViewController.AddNewEvents(item);
                                }
                            }


                            trackingEntry.Entity.AssignNewRevision(trackingEntry);

                            break;
                        case TrackingEntry.ModificationTypes.Modify:
                            //MMT
                            if (!updateEntities.Exists (r => r == trackingEntry.Entity))
                            {
                             //MMT
                                itemtr.Entity.AssignNewRevision(trackingEntry);
                                updateEntities.Add(trackingEntry.Entity);
                            //MMT
                            }
                            //MMT
                            break;
                    }
                    #endregion


                    foreach (AriaObjectShelve item in trackingEntry.AriaObjectShelves)
                    {
                        switch (itemtr.ModificationType)
                        {
                            case TrackingEntry.ModificationTypes.Add:

                                switch (item.ModificationType)
                                {
                                    case AriaObjectShelve.ModificationTypes.Add:
                                        trackingEntryViewController.AddNewAttachment(item);
                                        trackingEntryViewController.AddSetSettings(item);
                                        trackingEntryViewController.AddNewProperties(item);
                                        trackingEntryViewController.AddNewMethods(item);
                                        trackingEntryViewController.AddNewEvents(item);

                                        item.AriaObject.AssignNewRevision(trackingEntry);
                                        item.Save();
                                        item.Session.CommitTransaction();
                                        break;

                                    case AriaObjectShelve.ModificationTypes.Delete:
                                        // Doesn't occurs
                                        break;

                                    case AriaObjectShelve.ModificationTypes.Modify:
                                        item.AriaObject = item.Session.GetObjectByKey<AriaObject>(AiraObjectOid);
                                        item.Save();
                                        break;
                                }

                                break;

                            case TrackingEntry.ModificationTypes.Modify:
                                switch (item.ModificationType)
                                {
                                    case AriaObjectShelve.ModificationTypes.Add:
                                        trackingEntryViewController.AddNewAttachment(item);
                                        trackingEntryViewController.AddSetSettings(item);
                                        trackingEntryViewController.AddNewProperties(item);
                                        trackingEntryViewController.AddNewMethods(item);
                                        trackingEntryViewController.AddNewEvents(item);

                                        item.AriaObject.AssignNewRevision(trackingEntry);
                                        item.Save();
                                        item.Session.CommitTransaction();
                                        break;

                                    case AriaObjectShelve.ModificationTypes.Delete:
                                        item.Session.GetObjectByKey<AriaObject>(item.AriaObject.Oid).ParentObjectID = null;
                                        break;

                                    case AriaObjectShelve.ModificationTypes.Modify:
                                        trackingEntryViewController.AddSetSettings(item);
                                        trackingEntryViewController.AddNewProperties(item);
                                        trackingEntryViewController.AddNewMethods(item);
                                        trackingEntryViewController.AddNewEvents(item);

                                        item.AriaObject.AssignNewRevision(trackingEntry);
                                        item.Save();
                                        //MMT
                                        //item.Session.CommitTransaction();
                                        //MMT
                                        break;
                                }

                                break;
                        }
                    }

                    // CheckIn
                    string filepath = string.Empty;
                    string AssignedResource = string.Empty;

                    TfsManager tfsHandler = new TfsManager();

                    AssignedResource = "Khaled Mayhoub";

                    // loop on all shelve files to get the files which have been selected from the list

                    foreach (AriaObjectShelve selectedFile in trackingEntry.AriaObjectShelves)
                    {
                       string FilePath = selectedFile.AriaObject.AriaObjectSettings.Where(r => r.SettingType.SettingTypeId == "FilePath").Count() <= 0 ? "" :
                                        selectedFile.AriaObject.AriaObjectSettings.First(r => r.SettingType.SettingTypeId == "FilePath").Value;



                        if (!string.IsNullOrEmpty(FilePath))
                        {
                            selectedFile.State = "CheckedIn";

                            selectedFile.Save();

                            trackingEntry.Session.CommitTransaction();

                            


                        }
                    }

                   // Frame.View.ObjectSpace.Refresh();

                    //MMT
                    CommentCheckIn(trackingEntry);
                    //MMT

                    trackingEntry.Status = TrackingEntry.TrackingStatus.Complete;
                    trackingEntry.CompleteDate = DateTime.Today.Date;
                    trackingEntry.Save();
                    trackingEntries.Add(trackingEntry);
                    //mmt
                    //trackingEntry.Session.CommitTransaction();
                    //mmt


                    //Frame.View.ObjectSpace.Refresh();
                }


            }
            //MMT
            appBuild.Status = ApplicationBuildStatus.Complete;
            appBuild.CompleteDate = DateTime.Today;
            appBuild.CompleteBy = SecuritySystem.CurrentUserName;
            appBuild.Save();
            objectSpace.CommitChanges();
            //MMT
            //applicationBuild.Session.CommitTransaction();
            
            //foreach (TrackingEntry itemtr in trackingEntries)
            //{
            //    itemtr.Session.CommitTransaction(); 
            //}
                
            //MMT
            Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
            //MMT
            Frame.View.ObjectSpace.Refresh();
        }

        #region commented

        //public void CreatelocalWorkSpace(string tfsMappingPathValue, string initLabel, string lastLabel, string BuildWorkSapce, string TfsUserName, string TfsPassWord, string TfsDomain, string TfsCollection, string TfsWorkSpacePath)
        //{   //  TfsWorkSpacePath = @"D:\Dev\BuildWorkSpace\";
        //    // BuildWorkSapce = "BuildWorkSpace";
        //    NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //    TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);
        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
        //    // System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(TfsWorkSpacePath);
        //    //  dir.Attributes = FileAttributes.Normal;
        //    if (vcs.GetWorkspace(BuildWorkSapce, TfsUserName) != null)
        //    {
        //        vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
        //    }
        //    if (vcs.TryGetWorkspace(BuildWorkSapce) != null)
        //    {
        //        vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
        //    }
        //    //if (dir.Exists)
        //    //{
        //    //    //foreach (var item in dir.GetFiles())
        //    //    //{
        //    //    //    item.Attributes &= ~FileAttributes.ReadOnly;
        //    //    //   // item.IsReadOnly = false;
        //    //    //}
        //    //    dir.Attributes &= ~FileAttributes.ReadOnly;
        //    //    dir.Delete(true);
        //    //}

        //    // Creating TFS WorkSpace Khaled Chec
        //    if (vcs.TryGetWorkspace(BuildWorkSapce) == null)
        //    {
        //        vcs.CreateWorkspace(BuildWorkSapce, TfsUserName);
        //    }
        //    var workspace = vcs.GetWorkspace(BuildWorkSapce, TfsUserName);
        //    // Mapping Source Code from ProjectPath to Physical Path at TFS WorkSpace 
        //    workspace.Map(tfsMappingPathValue, TfsWorkSpacePath); //@"D:\Dev\BuildWorkSpace");  //@"D:\Dev\BuildWorkSpace"
        //    //Get Latest Vr. of the Source Code 
        //    var LatestRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), VersionSpec.Latest);
        //    var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
        //    if (lastLabel == null || lastLabel.Trim() == "")
        //    {
        //        var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(initLabel));// Mr. Mahmoud check where get the Old Label ????
        //        var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
        //    }
        //    else
        //    {
        //        var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(lastLabel));// Mr. Mahmoud check where get the Old Label ????
        //        var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
        //    }
        //}

        //public void CreatelocalWorkSpaceLabel(string tfsMappingPathValue, string currentBuildLablestring, string TfsUserName, string TfsPassWord, string TfsDomain, string TfsCollection, string BuildWorkSapce)
        //{

        //    NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
        //    //Creating Label From Prepeared WorkSpace with Specific Vr.
        //    VersionControlLabel labelToCreate = new VersionControlLabel(vcs, currentBuildLablestring, TfsUserName, "", ""); // Mr. Mahmoud check where get the New Label 
        //    LabelItemSpec[] labelItemSpecs = new LabelItemSpec[1];
        //    ItemSpec itemSpec = new ItemSpec(tfsMappingPathValue, RecursionType.Full);

        //    VersionSpec a;
        //    a = VersionSpec.ParseSingleSpec("W" + BuildWorkSapce, TfsUserName);

        //    labelItemSpecs[0] = new LabelItemSpec(itemSpec, a, false);
        //    vcs.CreateLabel(labelToCreate, labelItemSpecs, LabelChildOption.Replace);

        //    //Delete WorkSpace After Creating the Need Label.
        //    vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
        //    //    System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(TfsWorkSpacePath);

        //    //   dir.Attributes = FileAttributes.Normal;

        //    //if (dir.Exists)
        //    //{
        //    //    foreach (var item in dir.GetFiles())
        //    //    {
        //    //        item.Attributes &= ~FileAttributes.ReadOnly;
        //    //        // item.IsReadOnly = false;
        //    //    }
        //    //    dir.Attributes &= ~FileAttributes.ReadOnly;
        //    //    dir.Delete(true);
        //    //    Directory.GetAccessControl(TfsWorkSpacePath);
        //    //    Directory.Delete(TfsWorkSpacePath, true);
        //    //}
        //    //----------------
        //    //NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    //TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

        //    //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
        //    ////Creating Label From Prepeared WorkSpace with Specific Vr.
        //    //VersionControlLabel labelToCreate = new VersionControlLabel(vcs, currentBuildLablestring, TfsUserName, "", ""); // Mr. Mahmoud check where get the New Label 
        //    //LabelItemSpec[] labelItemSpecs = new LabelItemSpec[1];
        //    //ItemSpec itemSpec = new ItemSpec(tfsMappingPathValue, RecursionType.Full);

        //    //VersionSpec a;
        //    //a = VersionSpec.ParseSingleSpec("W" + BuildWorkSapce, TfsUserName);

        //    //labelItemSpecs[0] = new LabelItemSpec(itemSpec, a, false);
        //    //vcs.CreateLabel(labelToCreate, labelItemSpecs, LabelChildOption.Replace);

        //    ////Delete WorkSpace After Creating the Need Label.
        //    //vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
        //}

        //public void BuildlocalDeploy(string currentLabel, string projectname, String buildDefinitionName,string TfsUserName, string TfsPassWord, string TfsDomain, string TfsCollection)
        //{

        //    //ApplicationSetting currentLabelSetting = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "CurrentBuildLabel".Trim().ToUpper());
        //    //string currentLabel = currentLabelSetting.SettingValue;

        //    //ApplicationSetting tfsProjectName = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFS Main Project name".Trim().ToUpper());
        //    //string projectname = tfsProjectName.SettingValue;

        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //    //TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
        //    //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //    TeamFoundationServer TFS = new TeamFoundationServer(TfsCollection, cre);
        //    //TeamFoundationServer tfs = TeamFoundationServerFactory.GetServer(TfsCollection, cre);

        //    IBuildServer buildServer = (IBuildServer)TFS.GetService(typeof(IBuildServer));
        //    IBuildDefinition buildDef = buildServer.GetBuildDefinition(projectname, buildDefinitionName);
        //    var queuedBuild = buildServer.QueueBuild(buildDef);

        //    ((IBuildDetail)queuedBuild.Builds[0]).SourceGetVersion = "L" + currentLabel; // "LNewBuildLabel";//InitialLabelvalue;


        //    queuedBuild.Builds[0].Save();
        //}

        //public void SetParameters(string tfsUserName, string Password, string Domain, string projectName, string GroupName, string tfsCollection, string checkInWorkspace, string buildWorkSapce, string tfsWorkSpacePath)
        //{
        //    TfsUserName = tfsUserName;
        //    TfsPassWord = Password;
        //    TfsDomain = Domain;
        //    TfsProjectName = projectName;
        //    TfsGroupName = GroupName;
        //    TfsCollection = tfsCollection;
        //    BuildWorkSapce = buildWorkSapce;
        //    TfsWorkSpacePath = tfsWorkSpacePath;
        //    TfsCheckinWorkSpace = checkInWorkspace;
        //}
        #endregion

        private void GenerateController_Execute_1(object sender, SimpleActionExecuteEventArgs e)
        {
            // MMT change build status to Release Candidate
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            ApplicationBuild_T applicationBuild = (ApplicationBuild_T)e.CurrentObject;
            ApplicationBuild_T appBuild = objectSpace.GetObjectByKey<ApplicationBuild_T>(applicationBuild.Oid);
            appBuild.Status = ApplicationBuildStatus.ReleaseCandidate;
            appBuild.Save();
            appBuild.Session.CommitTransaction();
            Frame.View.ObjectSpace.Refresh();
            //MMT

        }
        //MMT
        public void CommentCheckIn(TrackingEntry tE)
        {
            string TFSCollection = "", TFSProjectName = "", TFSProjectPath = "";
            string fileName = "", filePath = "", fullPath = "", ShelveSet = "", CheckInComment = "";

            if (tE != null && tE is TrackingEntry)
            {
                Application_T x = tE.Application;

                if (x.ApplicationSettings.Count > 0)
                {
                    ApplicationSetting Tfscollectionsetting = x.ApplicationSettings.Where(y => y.SettingType.SettingTypeId == "Tfscollection").FirstOrDefault();
                    if (Tfscollectionsetting != null)
                    {
                        TFSCollection = Tfscollectionsetting.SettingValue;
                    }
                    ApplicationSetting Tfsprjectsetting = x.ApplicationSettings.Where(y => y.SettingType.SettingTypeId == "Tfsproject").FirstOrDefault();
                    if (Tfsprjectsetting != null)
                    {
                        TFSProjectName = Tfsprjectsetting.SettingValue;
                    }
                    TFSProjectPath = "$/" + TFSProjectName;
                }
                TfsManager tfs = new TfsManager(TFSCollection, TFSProjectName);

                TrackingTask task = tE.TrackingTasks.Where(i => i.Task.CheckOut == true && i.Status == TrackingTask.StatusTypes.Complete).FirstOrDefault();
                if (task != null)
                {
                    string assignedSource = task.Resources.Name;
                    //MMT
                    //CheckInComment = ((TrackingEntry)this.View.CurrentObject).TicketNumber + " - " + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + " - " + assignedSource;
                    CheckInComment = tE.ReferenceNo + " - " + tE.Type.ToString().Substring(0, 1) + tE.ID.ToString() + " - " + assignedSource;
                    //
                    ShelveSet = tE.Type.ToString().Substring(0, 1) + tE.ID.ToString();
                    //MMT
                    TFSProjectPath = x.Name;
                    //MMT
                    tfs.CommentCheckin(TFSProjectPath, ShelveSet, assignedSource, CheckInComment);

                    foreach (AriaObjectShelve objectShe in tE.AriaObjectShelves)
                    {
                        fileName = ""; filePath = ""; fullPath = "";
                        // objectShe.ChangeSet = changeset.ToString();

                        if (objectShe.AriaObjectShelveSettings.Count > 0)
                        {
                            AriaObjectShelveSetting Storagefilenametype = objectShe.AriaObjectShelveSettings.Where(z => z.SettingType.SettingTypeId == "StorageFileName").FirstOrDefault();
                            if (Storagefilenametype != null)
                            {
                                fileName = Storagefilenametype.Value;
                            }
                            AriaObjectShelveSetting Subfoldertype = objectShe.AriaObjectShelveSettings.Where(z => z.SettingType.SettingTypeId == "SubFolder").FirstOrDefault();
                            if (Subfoldertype != null)
                            {
                                filePath = Subfoldertype.Value;
                                //MMT
                                if (filePath.ToUpper().Contains("\\Aria4XP\\Aria4XP\\".ToUpper()))
                                {
                                    filePath = filePath.ToUpper().Replace("\\Aria4XP\\Aria4XP\\".ToUpper(), "\\Aria4XP\\".ToUpper());
                                }
                                //MMT
                            }
                            fullPath = filePath + "\\" + fileName;
                        }
                        //ATA 2/9/2017 
                        objectShe.CodeStatus = AriaObjectShelve.Status.CheckedIn;
                    }
                    //   tfs.DenyCheckoutFilePermission(fullPath, assignedSource);
                    denyCheckout(tE);
                    // ((TrackingEntry)this.View.CurrentObject).Status = TrackingEntry.TrackingStatus.Complete;

                }
                else {
                    throw new Exception("Cannot complete tracking entry#"+ tE.Type.ToString().Substring (0,1)+tE.ID.ToString () +" because 'Access Source files' Task is incomplete or missing.");
                }

            }
        }
        public void denyCheckout(TrackingEntry tE)
        {
            //ATA 
            ///TfsManager tfs = new TfsManager("http://tf_server:8080/tfs/Aria_Dev-2015", "Aria 5 System Admin");
            TfsManager tfs = new TfsManager();
            //ATA 
            if (tE != null && tE is TrackingEntry)
            {
                TrackingTask task = tE.TrackingTasks.Where(x => x.Task.CheckOut == true).FirstOrDefault();
                if (task != null)
                {
                    string assignedSource = task.Resources.Name;
                    foreach (AriaObjectShelve objectShe in tE.AriaObjectShelves)
                    {
                        string fileName = "", filePath = "", fullPath = "";

                        if (objectShe.AriaObjectShelveSettings.Count > 0)
                        {
                            AriaObjectShelveSetting Storagefilenametype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "StorageFileName").FirstOrDefault();
                            if (Storagefilenametype != null)
                            {
                                fileName = Storagefilenametype.Value;
                            }
                            AriaObjectShelveSetting Subfoldertype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "SubFolder").FirstOrDefault();
                            if (Subfoldertype != null)
                            {
                                filePath = Subfoldertype.Value;

                            }
                            fullPath = filePath + "\\" + fileName;
                        }

                        if (fullPath != "")
                        {
                            TrackingEntryViewController trackingEntryViewController = Frame.GetController<TrackingEntryViewController>();
                            //ATA new function to deny check out 
                            trackingEntryViewController.Denycheckoutfile(fullPath, assignedSource, tfs);
                        }
                        //ATA old denycheckout function 
                        //tfs.DenyCheckoutFilePermission(fullPath, assignedSource);

                    }

                }
            }
            //DevExpress.XtraEditors.XtraMessageBox.Show("Resource denied to checkout the Files!");
        }
        //MMT

    }
}
