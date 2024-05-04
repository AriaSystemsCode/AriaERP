using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using AriaDevExpress.Module.BusinessObjects.SysFiles;

namespace AriaDevExpress.Module.Controllers.SysFiles
{
    public partial class ExpressionBuilderController : ViewController
    {
        ExpressionBuilder ExpBuilder;
        public ExpressionBuilderController()
        {
            InitializeComponent();
            RegisterActions(components);
        }

        private void ExpressionBuilderShowAction_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            ((Index)View.CurrentObject).TagExpression = ExpBuilder.Expression;
        }

        private void ExpressionBuilderShowAction_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace ExpBuilderObjectSpace = Application.CreateObjectSpace();
            ExpBuilder = ExpBuilderObjectSpace.CreateObject<ExpressionBuilder>();
            ExpBuilder.File = ExpBuilderObjectSpace.GetObject<File>(((Index)this.View.CurrentObject).Filenam);
            DetailView detailView = Application.CreateDetailView(ExpBuilderObjectSpace, ExpBuilder);
            detailView.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
            e.View = detailView;
        }

        private void ExpressionBuilderController_AfterConstruction(object sender, EventArgs e)
        {
            if(View != null )
            if (((DetailView)this.View).ViewEditMode == DevExpress.ExpressApp.Editors.ViewEditMode.Edit)

            //((DevExpress.ExpressApp.DetailView)(View)).ViewEditMode =
            TargetObjectType = typeof(Index);
            ExpressionBuilderShowAction.SelectionDependencyType = SelectionDependencyType.RequireMultipleObjects;
            ExpressionBuilderShowAction.TargetObjectsCriteria = "true"; // "IsCode";
            ExpressionBuilderShowAction.TargetObjectsCriteriaMode = TargetObjectsCriteriaMode.TrueForAll;
          //  ExpressionBuilderShowAction.TargetViewNesting = Nesting.Nested;
        }
    }
}
