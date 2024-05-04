using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using Aria5SystemAdmin.Module.BusinessObjects;

namespace Aria5SystemAdmin.Module.Controllers
{
    public partial class AriaObjectPropertiesViewController : ViewController
    {
        public AriaObjectPropertiesViewController()
        {
            InitializeComponent();
            RegisterActions(components);
        }

        private void ShowNotesAction_CustomizePopupWindowParams(object sender,   CustomizePopupWindowParamsEventArgs e)
        
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();


            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObject));
            //string strName = ((AriaObject)View.CurrentObject).ObjectType.Name;

            newCollectionSource.Criteria.Add("1", DevExpress.Data.Filtering.CriteriaOperator.Parse("PropertyLevel = 1"));


            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObject)),
            newCollectionSource, true);
            e.View = vwAriaObject;

        }

        private void AriaObjectProperties_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            try
            {
                ((AriaObject)View.CurrentObject).Session.BeginTransaction();
            }
            catch (Exception ex)
            {
                ((AriaObject)View.CurrentObject).Session.CommitTransaction();
                ((AriaObject)View.CurrentObject).Session.BeginTransaction();
 
            }
            foreach (AriaObject  selectedAriaObject in e.PopupWindow.View.SelectedObjects)
            {


                AriaObjectProperties newAriaObjectProperties = new AriaObjectProperties(((AriaObject)View.CurrentObject).Session);
                //newAriaObjectProperties = View.ObjectSpace.CreateObject<AriaObjectProperties>();
                View.ObjectSpace.IsNewObject(newAriaObjectProperties);
 
                
                ((AriaObject)View.CurrentObject).IncrementRevisions();
                Boolean hasEmpty = ((AriaObject)View.CurrentObject).hasEmptyRevision;

 
                newAriaObjectProperties.AriaObject = ((AriaObject)View.CurrentObject);
                //newAriaObjectProperties.ObjectRevision = ((AriaObject)View.CurrentObject).ActiveRevision;
                newAriaObjectProperties.ObjectRevision = "";
                newAriaObjectProperties.ModificationType = AriaObjectProperties.modificationType.Add;
                newAriaObjectProperties.PropertyName = ((AriaObject)selectedAriaObject).ObjectName;
                newAriaObjectProperties.PropertyDescription = ((AriaObject)selectedAriaObject).ObjectDescription;
                newAriaObjectProperties.PropertySettings = "";
                newAriaObjectProperties.Required = false ;
                newAriaObjectProperties.GenSquFn = "";
                string strName = ((AriaObject)selectedAriaObject).ObjectType.Name;
                newAriaObjectProperties.PropertyType = ((AriaObject)View.CurrentObject).Session.FindObject<PropertyType>(DevExpress.Data.Filtering.CriteriaOperator.Parse("Name='" + strName + "'"));
                newAriaObjectProperties.AriaParentRevision = selectedAriaObject.ActiveRevision;
                newAriaObjectProperties.AriaParentObject = ((AriaObject)View.CurrentObject).Session.FindObject<AriaObject>(DevExpress.Data.Filtering.CriteriaOperator.Parse("Oid='" + selectedAriaObject.Oid.ToString()   + "'")); ;
                // Fix properties, adding errors 2013-Jan-14 [Start]
                //newAriaObjectProperties.calledFromSettings = false;
                // Fix properties, adding errors 2013-Jan-14 [End]
                ((AriaObject)View.CurrentObject).Session.Save(newAriaObjectProperties);
                ((AriaObject)View.CurrentObject).AriaObjectPropertieses.Add(newAriaObjectProperties) ;


            }
            ((AriaObject)View.CurrentObject).Session.CommitTransaction();
  
        }

    }
}

