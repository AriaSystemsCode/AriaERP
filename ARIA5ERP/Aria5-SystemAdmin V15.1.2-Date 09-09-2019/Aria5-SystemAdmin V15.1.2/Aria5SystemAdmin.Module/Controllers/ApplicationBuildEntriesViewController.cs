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

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class ApplicationBuildEntriesViewController : ViewController
    {
        public ApplicationBuildEntriesViewController()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.

            AppBuildLinkController.Enabled.SetItemValue("Disable", true);

        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.

            IObjectSpace objectSpace = Application.CreateObjectSpace();
            ApplicationBuild_T applicationBuild = objectSpace.GetObjectByKey<ApplicationBuild_T>(ApplicationBuildViewController.currentApplicationBuild);

            if (View is ListView && applicationBuild != null)
            {
                if ((applicationBuild.Status == ApplicationBuildStatus.ReleaseCandidate || applicationBuild.Status == ApplicationBuildStatus.Complete))
                {
                    AppBuildLinkController.Enabled.SetItemValue("Disable", false);
                    Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                }

                else
                {
                    AppBuildLinkController.Enabled.SetItemValue("Disable", true);
                    Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", true);
                }
            }
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


        private void AppBuildLinkController_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            ApplicationBuild_T applicationBuild = objectSpace.GetObjectByKey<ApplicationBuild_T>(ApplicationBuildViewController.currentApplicationBuild);

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(TrackingEntryBrowse));

            var trackingCollection = objectSpace.GetObjects(typeof(TrackingEntry), CriteriaOperator.Parse("[BuildNo] = " + 0 + " and [Application] ='" + applicationBuild.Application_T.Oid + "'"));// new CollectionSource(objectSpace, typeof(TrackingEntry));

            bool Exist = false;

            if (applicationBuild != null)
            {
                foreach (TrackingEntry Trackingentry in trackingCollection)
                {
                     var trackingEntryToBrowse = objectSpace.CreateObject<TrackingEntryBrowse>();

                                        trackingEntryToBrowse.ParentTrackingEntry = Trackingentry.ParentTrackingEntry;
                                        trackingEntryToBrowse.ReferenceNo = Trackingentry.ReferenceNo;
                                        trackingEntryToBrowse.Description = Trackingentry.Description;
                                        trackingEntryToBrowse.AccountName = Trackingentry.AccountName;
                                        trackingEntryToBrowse.Entity = Trackingentry.Entity;
                                        trackingEntryToBrowse.ID = Trackingentry.ID;
                                        trackingEntryToBrowse.Type = Trackingentry.Type;
                                        trackingEntryToBrowse.TrackingOid = Trackingentry.Oid.ToString();
                                        trackingEntryToBrowse.Key = Guid.NewGuid().ToString();
                                        newCollectionSource.Add(trackingEntryToBrowse);
                }

              // if (trackingCollection.List.Count > 0)
                if(1==1)
                {
                    // Current Application
                    foreach (AriaObject ariaObject in applicationBuild.Application_T.AriaObjects)
                    {   // Check Aria Object of type entity 
                        if (ariaObject.ObjectType.Name.ToUpper() == "ENTITY")
                        { // we need to add one to many relation, tracking, AriaObject

                            foreach (AriaObjectRevision ariaObjectRevision in ariaObject.AriaObjectRevisions)
                            {
                                TrackingEntry trackingEntry = ariaObjectRevision.TrackingNo;
                                if (trackingEntry != null)
                                {
                                    // validate tracking type and status
                                    if (trackingEntry.Entity == ariaObject && trackingEntry.Type != TrackingEntry.TrackingType.Custom && trackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                                    {
                                        var trackingEntryToBrowse = objectSpace.CreateObject<TrackingEntryBrowse>();

                                        trackingEntryToBrowse.ParentTrackingEntry = trackingEntry.ParentTrackingEntry;
                                        trackingEntryToBrowse.ReferenceNo = trackingEntry.ReferenceNo;
                                        trackingEntryToBrowse.Description = trackingEntry.Description;
                                        trackingEntryToBrowse.AccountName = trackingEntry.AccountName;
                                        trackingEntryToBrowse.Entity = trackingEntry.Entity;
                                        trackingEntryToBrowse.ID = trackingEntry.ID;
                                        trackingEntryToBrowse.Type = trackingEntry.Type;
                                        trackingEntryToBrowse.TrackingOid = trackingEntry.Oid.ToString();
                                        trackingEntryToBrowse.Key = Guid.NewGuid().ToString();

                                        // vlidate tracking not linked to any other application build.
                                        //foreach (ApplicationBuildEntries_T applicationBuildEntry in trackingEntry.ApplicationBuildEntries_Ts)
                                        //{
                                        //    if (applicationBuildEntry.ApplicationBuild_T != null && applicationBuildEntry.ApplicationBuild_T == applicationBuild)
                                        //    { Exist = true; break; }
                                        //}

                                        //if (Exist == false) { newCollectionSource.Add(trackingEntryToBrowse); }

                                        //Exist = false;

                                    }
                                }
                            }

                        }



                    }

                    //Sara.N , 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming. [Start]
                    #region  Sara.N[Start]
                    Application_T appli = objectSpace.GetObjectByKey<Application_T>(ApplicationBuildViewController.currentApplicationBuild);
                    // Current Prereqquiste
                  // Loop to get all Tracking entires for each Prereqquiste application fot the parent one.
                    foreach (Application_T application_T in applicationBuild.Application_T.PreRequiste_Applications)
                    {
                        foreach (AriaObject ariaObject in application_T.AriaObjects)
                        {
                            if (ariaObject.ObjectType.Name.ToUpper() == "ENTITY")
                            { // we need to add one to many relation, tracking, AriaObject

                                foreach (AriaObjectRevision ariaObjectRevision in ariaObject.AriaObjectRevisions)
                                {
                                    TrackingEntry trackingEntry = ariaObjectRevision.TrackingNo;
                                    if (trackingEntry != null)
                                    {
                                        // validate tracking type and status
                                        if (trackingEntry.Entity == ariaObject && trackingEntry.Type != TrackingEntry.TrackingType.Custom && trackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                                        {
                                            var trackingEntryToBrowse = objectSpace.CreateObject<TrackingEntryBrowse>();

                                            trackingEntryToBrowse.ParentTrackingEntry = trackingEntry.ParentTrackingEntry;
                                            trackingEntryToBrowse.ReferenceNo = trackingEntry.ReferenceNo;
                                            trackingEntryToBrowse.Description = trackingEntry.Description;
                                            trackingEntryToBrowse.AccountName = trackingEntry.AccountName;
                                            trackingEntryToBrowse.Entity = trackingEntry.Entity;
                                            trackingEntryToBrowse.ID = trackingEntry.ID;
                                            trackingEntryToBrowse.Type = trackingEntry.Type;
                                            trackingEntryToBrowse.TrackingOid = trackingEntry.Oid.ToString();
                                            trackingEntryToBrowse.Key = Guid.NewGuid().ToString();

                                            // vlidate tracking not linked to any other application build.
                                            //foreach (ApplicationBuildEntries_T applicationBuildEntry in trackingEntry.ApplicationBuildEntries_Ts)
                                            //{
                                            //    if (applicationBuildEntry.ApplicationBuild_T != null && applicationBuildEntry.ApplicationBuild_T == applicationBuild)
                                            //    { Exist = true; break; }
                                            //}

                                            //if (Exist == false) { newCollectionSource.Add(trackingEntryToBrowse); }

                                            //Exist = false;

                                        }
                                    }
                                }

                            }
                        }


                    }
                    #endregion   //, 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming.
                    //Sara.N , 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming. [END]
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(TrackingEntryBrowse)),
            newCollectionSource, false);



            e.View = vwAriaObject;
        }

        private void AppBuildLinkController_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {

            IObjectSpace objectSpace = Application.CreateObjectSpace();

            ApplicationBuild_T applicationBuild = objectSpace.GetObjectByKey<ApplicationBuild_T>(ApplicationBuildViewController.currentApplicationBuild);

            foreach (TrackingEntryBrowse selectedTrackingEntryBrowse in e.PopupWindow.View.SelectedObjects)
            {

                TrackingEntry selectedTrackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(Guid.Parse(selectedTrackingEntryBrowse.TrackingOid));

                //ApplicationBuildEntries_T newApplicationBuildEntry = new ApplicationBuildEntries_T(selectedTrackingEntry.Session);

                //newApplicationBuildEntry.ApplicationBuild_T = objectSpace.GetObjectByKey<ApplicationBuild_T>(ApplicationBuildViewController.currentApplicationBuild);
                //newApplicationBuildEntry.TrackingEntry = selectedTrackingEntry;
                //newApplicationBuildEntry.TrackingEntryId = selectedTrackingEntry.ID.ToString();
                //applicationBuild.ApplicationBuildEntries_Ts.Add(newApplicationBuildEntry);

            }

            applicationBuild.Save();
            applicationBuild.Session.CommitTransaction();

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();


        }
    }
}
