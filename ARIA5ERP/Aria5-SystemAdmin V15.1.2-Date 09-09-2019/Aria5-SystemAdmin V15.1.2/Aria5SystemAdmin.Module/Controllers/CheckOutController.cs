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
using Aria5SystemAdmin.Module.Managers;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class CheckOutController : ViewController
    {
        public CheckOutController()
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

        private void CheckedOutFiles_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            //IObjectSpace objectSpace = Application.CreateObjectSpace();

            //string filepath=string.Empty;
            //string AssignedResource = string.Empty;
            //TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryController.currentriaTrakingEntry);

            //// loop on all shelve files to get the files which have been selected from the list

            //foreach (AriaObjectShelve selectedFile in e.SelectedObjects)
            //{
            //    string FilePath = selectedFile.AriaObject.AriaObjectSettings.Where(r => r.SettingType.Name == "FilePath").Count() <= 0 ? "" :
            //                        selectedFile.AriaObject.AriaObjectSettings.First(r => r.SettingType.Name == "FilePath").Value;

            //    if (!string.IsNullOrEmpty(FilePath))
            //    {

            //        TfsManager tfsHandler = new TfsManager("http://tf_server:8080/tfs/DefaultCollection",
            //                                                "ProjectAdmin",
            //                                                "aria_123",
            //                                                "TF_Server",
            //                                                "1touchaway",
            //                                                "1touchaway Team");


            //        filepath = FilePath;

            //        //check for the resources not empty
            //        AssignedResource = "Khaled Mayhoub";

            //        tfsHandler.GrantCheckoutFilePermission(filepath, AssignedResource);

            //        selectedFile.State = "CheckedOut";

            //        selectedFile.Save();

            //        selectedFile.Session.CommitTransaction();
            //    }
            //}
        }
    }
}
