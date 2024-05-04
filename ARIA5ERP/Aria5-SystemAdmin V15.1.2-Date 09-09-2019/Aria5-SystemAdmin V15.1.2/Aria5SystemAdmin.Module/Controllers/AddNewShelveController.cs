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

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class AddNewShelveController : ViewController
    {
        // private static AriaObjectShelve currentriaObjectShelve;

        public AddNewShelveController()
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

        private void NewWindow_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            foreach (AriaObjectBrowse selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObject selectedAriaObject = objectSpace.GetObjectByKey<AriaObject>(Guid.Parse(selectedAriaObjectBrowse.ObjectOid));

                AriaObjectShelve newObjectShelve = new AriaObjectShelve(selectedAriaObject.Session);


                var objectExist = newObjectShelve.Session.FindObject<AriaObjectShelve>(CriteriaOperator.Parse("[TrackingEntry] ='" + TrackingEntryController.currentriaTrakingEntry + "' AND [AriaObject] ='" + selectedAriaObject.Oid + "'"));

                if (objectExist == null)
                {
                    newObjectShelve.AriaObject = newObjectShelve.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.Oid + "'"));
                    newObjectShelve.TrackingEntry = objectSpace.GetObjectByKey < TrackingEntry >  (TrackingEntryController.currentriaTrakingEntry);
                    newObjectShelve.ObjectDescription = selectedAriaObject.ObjectDescription;
                    newObjectShelve.ObjectID = selectedAriaObject.ObjectID;
                    newObjectShelve.ObjectName = selectedAriaObject.ObjectName;
                    newObjectShelve.ObjectType = newObjectShelve.Session.FindObject<ObjectType>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.ObjectType.Oid + "'"));


                    newObjectShelve.Save();

                    (selectedAriaObject.Session).Save(newObjectShelve);
                    (selectedAriaObject.Session).CommitTransaction();
                }

            }


        }

        private void NewWindow_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryController.currentriaTrakingEntry);

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowse));
            bool Exist = false;
            if (trackingEntry.Entity != null)
            {
                foreach (var relatedObject in objectSpace.GetObjectByKey<AriaObject>(trackingEntry.Entity.Oid).getchildren)
                {

                    var ariaObject = objectSpace.CreateObject<AriaObjectBrowse>();

                    ariaObject.ObjectName = relatedObject.ObjectName;
                    ariaObject.ObjectID = relatedObject.ObjectID;
                    ariaObject.ObjectDescription = relatedObject.ObjectDescription;
                    ariaObject.ActiveRevision = relatedObject.ActiveRevision;
                    ariaObject.Key = Guid.NewGuid().ToString();
                    ariaObject.ObjectOid = relatedObject.Oid.ToString();

                    foreach (AriaObjectShelve shelveObject in trackingEntry.AriaObjectShelves)
                    {
                        if (shelveObject.ObjectName == ariaObject.ObjectName)
                        {
                            Exist = true;
                        }

                    }

                    if (Exist == false)
                    {
                        newCollectionSource.Add(ariaObject);
                    }
                    Exist = false;
                }
            }

            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowse)),
            newCollectionSource, false);

            e.View = vwAriaObject;
        }


     
    }
}
