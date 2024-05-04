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
using DevExpress.ExpressApp.Security.Strategy;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.DC;
using DevExpress.ExpressApp.Model;
using DevExpress.ExpressApp.Web.SystemModule;
using DevExpress.ExpressApp.Security;
using DevExpress.Xpo;
using System.Diagnostics;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppWindowControllertopic.
    public partial class HideObjectBasedOnItsRelatedEntityController : WindowController
    {
        private ShowNavigationItemController navigationController;
        private AriaSecuritySystemUser user;
        private List<SecuritySystemRole> rolesList;

        List<ChoiceActionItem> xx = new List<ChoiceActionItem>();

        public HideObjectBasedOnItsRelatedEntityController()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Windows (via the TargetXXX properties) and create their Actions.
            TargetWindowType = WindowType.Main;

            
            
        }
       
        //override set
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target Window.
            Application.DetailViewCreated += Application_DetailViewCreated;
            Application.ListViewCreated += Application_ListViewCreated;
            Application.DetailViewCreating += Application_DetailViewCreating;

            IObjectSpace objectSpace = Application.CreateObjectSpace();
            string uesrName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;

            CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + uesrName + "'");
            user = objectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
        }
        protected override void OnDeactivated()
        {

            if (navigationController != null)
            {
                //navigationController.ItemsInitialized -= navigationController_ItemsInitialized;
                //navigationController = null;
            }

            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }

        protected override void OnFrameAssigned()
        {
            base.OnFrameAssigned();
            navigationController = Frame.GetController<ShowNavigationItemController>();
            if (navigationController != null)
            {
                navigationController.SynchronizeItemsWithSecurityMode = SynchronizeItemsWithSecurityMode.AllItems;
                navigationController.ItemsInitialized += navigationController_ItemsInitialized;
            }
        }

        #region Commented code
        void Enabled_Changed(object sender, EventArgs e)
        {
            if (!((BoolList)sender)["HasRights"]) ((BoolList)sender)["HasRights"] = true;

            foreach (var i in xx)
            {
                if (!i.Active["HasRights"]) i.Active["HasRights"] = true;
                if (!i.Enabled["HasRights"]) i.Enabled["HasRights"] = true;
            }

            //throw new NotImplementedException();
        }

        void Active_Changed(object sender, EventArgs e)
        {
            if(!((BoolList)sender)["HasRights"]) ((BoolList)sender)["HasRights"] = true;
           // throw new NotImplementedException();

            foreach (var i in xx)
            {
                if (!i.Active["HasRights"]) i.Active["HasRights"] = true;
                if (!i.Enabled["HasRights"]) i.Enabled["HasRights"] = true;
            }
        }

        void Enabled_ResultValueChanged(object sender, BoolValueChangedEventArgs e)
        {
            if (!((BoolList)sender)["HasRights"]) ((BoolList)sender)["HasRights"] = true;
            //throw new NotImplementedException();
            foreach (var i in xx)
            {
                if (!i.Active["HasRights"]) i.Active["HasRights"] = true;
                if (!i.Enabled["HasRights"]) i.Enabled["HasRights"] = true;
            }
        }

        void Active_ResultValueChanged(object sender, BoolValueChangedEventArgs e)
        {
            if (!((BoolList)sender)["HasRights"]) ((BoolList)sender)["HasRights"] = true;
            //throw new NotImplementedException();
            foreach (var i in xx)
            {
                if (!i.Active["HasRights"]) i.Active["HasRights"] = true;
                if (!i.Enabled["HasRights"]) i.Enabled["HasRights"] = true;
            }
        }
        #endregion
        private void navigationController_ItemsInitialized(object sender, EventArgs e)
        {
            if (user == null) return;

            rolesList = new List<SecuritySystemRole>();
            List<SecuritySystemRole> userRole = new List<SecuritySystemRole>();
            foreach (SecuritySystemRole role in user.Roles)
            {
                userRole.Add(role);
            }
            GetRoles(userRole);
            

            HideItem(navigationController.ShowNavigationItemAction.Items);
        }

        private void HideItem(ChoiceActionItemCollection items)
        {
            bool isAdmin = false;

            foreach (var role in rolesList)
            {
                if (role.IsAdministrative)
                {
                    isAdmin = true;

                    if(user.Account.Id.ToUpper().Trim() == "ARIA") return;
                }
            }
            
            foreach (ChoiceActionItem item in items)
            {
                IModelView view = (item.Model as IModelNavigationItem).View;

                if (view == null)
                {
                    if (item.ToolTip == null || item.ToolTip != "NotLinkedToSecurity")
                    {
                        if (user.Account.Id.ToUpper().Trim() != "ARIA")
                        {
                            bool isItemEnabled = false;

                            if (user.Account.ConfigurationItems.Count > 0)
                            {
                                for (int i = 0; i < user.Account.ConfigurationItems.Count; i++)
                                {
                                    if (user.Account.ConfigurationItems[i].ApplicationId.ToUpper().Trim().Contains(item.Id.ToUpper().Trim()))
                                    {
                                        isItemEnabled = true;
                                        break;
                                    }

                                    for (int j = 0; j < user.Account.ConfigurationItems[i].PreRequesiteAppList.Count; j++)
                                    {
                                        if (user.Account.ConfigurationItems[i].PreRequesiteAppList[j].Id.Contains(item.Id) || user.Account.ConfigurationItems[i].ApplicationId.Contains(item.Id))
                                        {
                                            isItemEnabled = true;
                                        }
                                    }
                                }

                                item.Active["ActiveMenu"] = isItemEnabled;

                                foreach (var key in item.Active.GetKeys())
                                {
                                    item.Active[key] = isItemEnabled;
                                }

                                foreach (var key in item.Enabled.GetKeys())
                                {
                                    item.Enabled[key] = isItemEnabled;
                                }

                                if (!isItemEnabled)
                                {
                                    HideRelatedItems(item);
                                }

                                
                            }
                        }
                    }
                }
                else
                {
                    if (!isAdmin)
                    {
                        IModelClass classModel = null;
                        if (view.Id.Contains("ListView"))
                        {
                            IModelListView viewList = (item.Model as IModelNavigationItem).View as IModelListView;
                            classModel = viewList.ModelClass as IModelClass;
                        }
                        else if (view.Id.Contains("DetailView"))
                        {
                            IModelDetailView viewDetail = (item.Model as IModelNavigationItem).View as IModelDetailView;
                            classModel = viewDetail.ModelClass as IModelClass;

                        }

                        bool allowView = false;

                        foreach (var role in rolesList)
                        {
                            EntityOperationPermission permissionObj = GetEntityPermission(classModel.TypeInfo as TypeInfo, role);

                            if (permissionObj != null)
                            {
                                allowView = permissionObj.AllowView;
                            }

                            if (allowView) break;
                        }

                        if (!allowView)
                        {
                            item.Active["ActiveMenu"] = allowView;
                        }

                        foreach (var key in item.Active.GetKeys())
                        {
                            item.Active[key] = allowView;
                        }
                        foreach (var key in item.Enabled.GetKeys())
                        {
                            item.Enabled[key] = allowView;
                        }
                    }
                }

                HideItem(item.Items);    
            }
        }

        public void HideRelatedItems(ChoiceActionItem MainItem)
        {
            for (int i = 0; i < MainItem.Items.Count; i++)
            {
                MainItem.Items[i].Active["ActiveMenu"] = false;

                foreach (var key in MainItem.Items[i].Active.GetKeys())
                {
                    MainItem.Items[i].Active[key] = false;
                }
                foreach (var key in MainItem.Items[i].Enabled.GetKeys())
                {
                    MainItem.Items[i].Enabled[key] = false;
                }

                HideRelatedItems(MainItem.Items[i]);
            }
        }
        

        private void GetRoles(List<SecuritySystemRole> roles)
        {
            foreach(SecuritySystemRole role in roles)
            {
                rolesList.Add(role);
                if (role.ChildRoles.Count > 0)
                {
                    List<SecuritySystemRole> childRoles = new List<SecuritySystemRole>();
                    foreach (SecuritySystemRole childRole in role.ChildRoles)
                    {
                        if(!rolesList.Contains(childRole))
                        {
                            childRoles.Add(childRole);
                        }
                    }
                    if (childRoles.Count > 0)
                    {
                        GetRoles(childRoles);
                    }
                }
            }
        }


        private void Application_DetailViewCreating(object sender, DetailViewCreatingEventArgs e)
        {
            if (e.ViewID == "SecuritySystemUser_DetailView")
            {
                e.View = Application.CreateDetailView(e.ObjectSpace, "AriaSecuritySystemUser_DetailView", false, e.Obj);
            }
        }

        private void Application_ListViewCreated(object sender, ListViewCreatedEventArgs e)
        {
            bool allowNew = true;
            bool allowEdit = true;
            bool allowDelete = true;
            foreach (var role in user.Roles)
            {
                EntityOperationPermission permissionObj = GetEntityPermission(e.View.ObjectTypeInfo as TypeInfo, user.Roles[0]);
                if (permissionObj != null)
                {
                    if (!permissionObj.AllowAdd) allowNew = false;
                    if (!permissionObj.AllowEdit) allowEdit = false;
                    if (!permissionObj.AllowDelete) allowDelete = false;
                }
            }

            e.View.AllowNew["CanAdd"] = allowNew;
            e.View.AllowEdit["CanEdit"] = allowEdit;
            e.View.AllowDelete["CanDelete"] = allowDelete;
        }

        private void Application_DetailViewCreated(object sender, DetailViewCreatedEventArgs e)
        {
            bool allowNew = true;
            bool allowEdit = true;
            bool allowDelete = true;
            foreach (var role in user.Roles)
            {
                EntityOperationPermission permissionObj = GetEntityPermission(e.View.ObjectTypeInfo as TypeInfo, user.Roles[0]);
                if (permissionObj != null)
                {
                    if (!permissionObj.AllowAdd) allowNew = false;
                    if (!permissionObj.AllowEdit) allowEdit = false;
                    if (!permissionObj.AllowDelete) allowDelete = false;
                }
            }

            e.View.AllowNew["CanAdd"] = allowNew;
            e.View.AllowEdit["CanEdit"] = allowEdit;
            e.View.AllowDelete["CanDelete"] = allowDelete;          
        }

        private EntityOperationPermission GetEntityPermission(TypeInfo objectTypeInfo, SecuritySystemRole role)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            var attributes = (objectTypeInfo as DevExpress.ExpressApp.DC.TypeInfo).Attributes;
            EntityOperationPermission permissionObj = null;
            foreach (var att in attributes)
            {
                if (att is RelatedEntity)
                {
                    CriteriaOperator criteria = CriteriaOperator.Parse("AriaObject.ObjectName = '" + (att as RelatedEntity).EntityName + "' AND [AriaSecuritySystemRole] ='" + role.Oid + "'");
                    permissionObj = objectSpace.FindObject<EntityOperationPermission>(criteria);
                }
            }
            return permissionObj;
        }

    }
}
