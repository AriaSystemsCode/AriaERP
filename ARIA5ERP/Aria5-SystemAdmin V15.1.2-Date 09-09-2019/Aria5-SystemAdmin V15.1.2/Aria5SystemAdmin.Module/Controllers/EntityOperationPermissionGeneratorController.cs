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
using DevExpress.ExpressApp.Security.Strategy;
using DevExpress.ExpressApp.Web.Editors.ASPx;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class EntityOperationPermissionGeneratorController : ViewController
    {
        public EntityOperationPermissionGeneratorController()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
            TargetObjectType = typeof(EntityOperationPermissionGenerator);
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.
            ((DetailView)View).ViewEditMode = ViewEditMode.Edit;
            Frame.GetController<ModificationsController>().Actions["Save"].Active.SetItemValue("Hide", false);
            Frame.GetController<ModificationsController>().Actions["SaveAndClose"].Active.SetItemValue("Hide", false);
            Frame.GetController<RefreshController>().Actions["Refresh"].Active.SetItemValue("Hide", false);
            //Frame.GetController<EntityOperationPermissionGenerator>().Actions["UpdateEntities"].Active.SetItemValue("Hide", false);
            Application.ModelChanged += Application_ModelChanged;
            //Frame.GetController<ModificationsController>().Actions["SaveAndNew"].Enabled.SetItemValue("Disable", false);
        }

        void Application_ModelChanged(object sender, EventArgs e)
        {
            throw new NotImplementedException();
        }

        
        void View_ControlsCreated(object sender, EventArgs e)
        {
            
        }
        protected override void OnViewChanged()
        {
            base.OnViewChanged();
        }
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();


            foreach (var item in ((DetailView)this.View).Items)
            {
               
                if (item is ASPxCheckedListBoxEditor && item.Id == "Entities")
                {
                   // var x = ((ASPxCheckedListBoxEditor)item).;//.Active.SetItemValue("Hide",false);
                   // ((ASPxCheckedListBoxEditor)item).AllowEdit["A1"] = false;
                     

                    //((ASPxCheckedListBoxEditor)item).CreateControl();
                    //((ASPxCheckedListBoxEditor)item)
                   
                    //((ICheckedListBoxItemsProvider)(View.CurrentObject as EntityOperationPermissionGenerator))
                    //((ASPxCheckedListBoxEditor)item).Control
                    //((ASPxCheckedListBoxEditor)item).ControlCreated += EntityOperationPermissionGeneratorController_ControlCreated;
                    //if ((item.CurrentObject as EntityOperationPermissionGenerator).Application != null)
                    //{
                    //    foreach(var x in (item.CurrentObject as EntityOperationPermissionGenerator).Application.AriaObjects)
                    //    {
                    //        (item.CurrentObject as EntityOperationPermissionGenerator).Entities += x.ObjectName + ";";
                    //    }
                    //}
                }
            }
            

            

            // Access and customize the target View control.
        }

    

        
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }

        private void GeneratePermission_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if( (e.CurrentObject as EntityOperationPermissionGenerator).Application != null &&
                !string.IsNullOrEmpty((e.CurrentObject as EntityOperationPermissionGenerator).Entities) &&
                (e.CurrentObject as EntityOperationPermissionGenerator).Role != null )
            {
                IObjectSpace objectSpace = Application.CreateObjectSpace();
                
                string [] entityArray = (e.CurrentObject as EntityOperationPermissionGenerator).Entities.Split(';');

                CriteriaOperator criteriaRole = CriteriaOperator.Parse("Name = '" + (e.CurrentObject as EntityOperationPermissionGenerator).Role.Name + "'");
                AriaSecuritySystemRole role = objectSpace.FindObject<AriaSecuritySystemRole>(criteriaRole);

                CriteriaOperator criteriaApp = CriteriaOperator.Parse("Name = '" + (e.CurrentObject as EntityOperationPermissionGenerator).Application.Name + "'");
                Application_T app = objectSpace.FindObject<Application_T>(criteriaApp);

                foreach(string entity in entityArray)
                {
                    if (!string.IsNullOrEmpty(entity))
                    {
                        CriteriaOperator criteriaEntity = CriteriaOperator.Parse("ObjectName = '" + entity.Split(':').First() + "'");
                        AriaObject entityObject = objectSpace.FindObject<AriaObject>(criteriaEntity);

                        CriteriaOperator criteriaPermission = CriteriaOperator.Parse("AriaSecuritySystemRole = '" + role.Oid + "'" + " AND " + "AriaObject = '" + entityObject.Oid + "'" + " AND " + "Application_T = '" + (e.CurrentObject as EntityOperationPermissionGenerator).Application.Oid + "'");
                        EntityOperationPermission permission = objectSpace.FindObject<EntityOperationPermission>(criteriaPermission);

                        if (permission == null)
                        {
                            permission = new EntityOperationPermission(entityObject.Session);
                        }

                        permission.AllowAdd = (e.CurrentObject as EntityOperationPermissionGenerator).AllowAdd;
                        permission.AllowEdit = (e.CurrentObject as EntityOperationPermissionGenerator).AllowEdit;
                        permission.AllowView = (e.CurrentObject as EntityOperationPermissionGenerator).AllowView;
                        permission.AllowDelete = (e.CurrentObject as EntityOperationPermissionGenerator).AllowDelete;
                        permission.Application_T = app;
                        permission.AriaObject = entityObject;
                        permission.AriaSecuritySystemRole = role;

                        permission.Save();
                        permission.Session.CommitTransaction();
                    }



                }

                EntityOperationPermissionGenerator permissionGenerator = this.View.ObjectSpace.CreateObject<EntityOperationPermissionGenerator>();
                //var r = Application.CreateDetailView(objectSpace, permissionGenerator, true);
                //this.SetView(Application.CreateDetailView(objectSpace, permissionGenerator, true));

                this.View.CurrentObject = permissionGenerator;
                this.View.AllowEdit["CanEditPermissionGenerator"] = true;

                foreach (var item in ((DetailView)this.View).Items)
                {

                    if (item is ASPxLookupPropertyEditor && item.Id == "Application")
                    {
                        ((ASPxLookupPropertyEditor)item).AllowEdit["UpdateEntity"] = true;
                    }
                }
            }
        }

        private void UpdateEntities_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            foreach (var item in ((DetailView)this.View).Items)
            {

                if (item is ASPxLookupPropertyEditor && item.Id == "Application")
                {
                    ((ASPxLookupPropertyEditor)item).AllowEdit["UpdateEntity"] = false;
                }
            }
        }
    }
}
