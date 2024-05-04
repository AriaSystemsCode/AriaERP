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
using DevExpress.ExpressApp.Security;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out https://documentation.devexpress.com/eXpressAppFramework/clsDevExpressExpressAppViewControllertopic.aspx.
    //ATA Add this controller to carry all approvals action for approval mecanism that will be apply 1/7/2017 [added ]
    public partial class ApprovalsController : ViewController
    {
        public ApprovalsController()
        {
            InitializeComponent();
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            //ATA add action for iteration project components acton and check ability to show this action for specific role user and spcefic screen 1/8/2017 [start]
            string username = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
            CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + username + "'");
          AriaSecuritySystemUser  user = ObjectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
          if ((View.ObjectTypeInfo.Type == typeof(QAUseCase) && user.Roles.Where(x => x.Name == "UseCaseApprover").Count() > 0) || (View.ObjectTypeInfo.Type == typeof(Requirement) && user.Roles.Where(x => x.Name == "RequirementApprover").Count() > 0)
                || (View.ObjectTypeInfo.Type == typeof(QAProjectEntity) && user.Roles.Where(x => x.Name == "ProjectEntityApprover").Count() > 0)
                || (View.ObjectTypeInfo.Type == typeof(TestCase) && user.Roles.Where(x => x.Name == "TestCaseApprover").Count() > 0) || (View.ObjectTypeInfo.Type == typeof(EntitySystemDesign) && user.Roles.Where(x => x.Name == "DetailDesignApprover").Count() > 0) || (View.ObjectTypeInfo.Type == typeof(QAUseCasePoints) && user.Roles.Where(x => x.Name == "UseCasePointsApprover").Count() > 0))
            {
                BaselineItemsApprovale.Active["1"] = true;
            }
            else
            {
                BaselineItemsApprovale.Active["1"] = false;
            }
            base.OnActivated();
            //ATA add action for iteration project components acton and check ability to show this action for specific role user and spcefic screen 1/8/2017 [End]
            // Perform various tasks depending on the target View.
            //ATA calcresourceshare while opening the screeen  2/19/2017[start]
            if (View.ObjectTypeInfo.Type == typeof(QAResourceShare) && View is DetailView)
            {
                QAResourceShare obj = (QAResourceShare)View.CurrentObject;
                foreach (QAWBS phase in ((QAResourceShare)View.CurrentObject).UseCasePoints.WBS)
                {
                    switch (phase.Month)
                    {
                        case 0:
                            ((QAResourceShare)View.CurrentObject).M0SharePercentage =(short) ObjectSpace.GetObjects<QAActivity>(CriteriaOperator.Parse("[QAWBS] = '"+phase.Oid+"' and [Resource] = '"+obj.ResourceName.Oid+"' and [Phase] = '0'")).Sum<QAActivity>(x=>x.AvgEstVal);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            ((QAResourceShare)View.CurrentObject).M0SharePercentage +=(short) ObjectSpace.GetObjects<TrackingTask>(CriteriaOperator.Parse("[WBSMonth] = '"+phase.Oid+"' and [Resources] = '"+obj.ResourceName.Oid+"'")).Sum<TrackingTask>(x=>x.Duration);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            break;
                        case 1:
                            ((QAResourceShare)View.CurrentObject).M1SharePercentage =(short) ObjectSpace.GetObjects<QAActivity>(CriteriaOperator.Parse("[QAWBS] = '"+phase.Oid+"' and [Resource] = '"+obj.ResourceName.Oid+"'and [Phase] = '0'")).Sum<QAActivity>(x=>x.AvgEstVal);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            ((QAResourceShare)View.CurrentObject).M1SharePercentage +=(short) ObjectSpace.GetObjects<TrackingTask>(CriteriaOperator.Parse("[WBSMonth] = '"+phase.Oid+"' and [Resources] = '"+obj.ResourceName.Oid+"'")).Sum<TrackingTask>(x=>x.Duration);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);

                            //obj.M0SharePercentage = (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            break;
                        case 2:
                            ((QAResourceShare)View.CurrentObject).M2SharePercentage =(short) ObjectSpace.GetObjects<QAActivity>(CriteriaOperator.Parse("[QAWBS] = '"+phase.Oid+"' and [Resource] = '"+obj.ResourceName.Oid+"' and [Phase] = '0'")).Sum<QAActivity>(x=>x.AvgEstVal);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            ((QAResourceShare)View.CurrentObject).M2SharePercentage +=(short) ObjectSpace.GetObjects<TrackingTask>(CriteriaOperator.Parse("[WBSMonth] = '"+phase.Oid+"' and [Resources] = '"+obj.ResourceName.Oid+"'")).Sum<TrackingTask>(x=>x.Duration);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);

                            //obj.M0SharePercentage = (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            break;
                        case 3:
                            ((QAResourceShare)View.CurrentObject).M3SharePercentage =(short) ObjectSpace.GetObjects<QAActivity>(CriteriaOperator.Parse("[QAWBS] = '"+phase.Oid+"' and [Resource] = '"+obj.ResourceName.Oid+"' and ([Phase] = '0' or [Phase] is Null)")).Sum<QAActivity>(x=>x.AvgEstVal);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            ((QAResourceShare)View.CurrentObject).M3SharePercentage +=(short) ObjectSpace.GetObjects<TrackingTask>(CriteriaOperator.Parse("[WBSMonth] = '"+phase.Oid+"' and [Resources] = '"+obj.ResourceName.Oid+"'")).Sum<TrackingTask>(x=>x.Duration);// (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);

                            //obj.M0SharePercentage = (short)phase.QAActivities.Where(x => x.Resource == obj.ResourceName).Sum(x => x.AvgEstVal);
                            break;
                        default:
                            break;
                    }
                }
                ((QAResourceShare)View.CurrentObject).TotalShareHours =(short)(((QAResourceShare)View.CurrentObject).M0SharePercentage + ((QAResourceShare)View.CurrentObject).M1SharePercentage + ((QAResourceShare)View.CurrentObject).M2SharePercentage + ((QAResourceShare)View.CurrentObject).M3SharePercentage);
                ((QAResourceShare)View.CurrentObject).Save();
                ((QAResourceShare)View.CurrentObject).Session.CommitTransaction();
                ObjectSpace.CommitChanges();
            }
            //ATA calcresourceshare while opening the screeen 2/19/2017[End]
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
        //ATA approve scope action that change the status to be approved instead of new and save approval date and approver 1/8/2017 [Start]
        private void ScopeApproval_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (View is ListView)
            {
                if (View.SelectedObjects.Count > 0)
                {
                    foreach (object Scope in View.SelectedObjects)
                    {
                        if (((ProjectScope)Scope).Status == approvalstatus.New)
                        {
                            ((ProjectScope)Scope).Status = approvalstatus.Approved;
                            ((ProjectScope)Scope).ApprovalDate = DateTime.Now;
                            ((ProjectScope)Scope).ApprovedBy = ((ProjectScope)View.CurrentObject).Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("[Oid] = '" + ((AriaSecuritySystemUser)SecuritySystem.CurrentUser).Oid + "'"));
                            ((ProjectScope)Scope).Save();
                            ((ProjectScope)Scope).Session.CommitTransaction();
                        }
                    }
                    ObjectSpace.CommitChanges();
                }
            }
        }
        //ATA approve scope action that change the status to be approved instead of new and save approval date and approver 1/8/2017 [End]
        //ATA approve Proof Of concept action that change the status to be approved instead of new and save approval date and approver 1/9/2017 [Start]


        private void ProofOfConceptApproval_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (View is DetailView)
            {

                //ProofOfConcept Scope = ((ProofOfConcept)View.CurrentObject);
                ((ProofOfConcept)View.CurrentObject).Status = approvalstatus.Approved;
                ((ProofOfConcept)View.CurrentObject).ApprovalDate = DateTime.Now;
                ((ProofOfConcept)View.CurrentObject).ApprovedBy = ((ProofOfConcept)View.CurrentObject).Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("[Oid] = '"+((AriaSecuritySystemUser)SecuritySystem.CurrentUser).Oid+"'"));
                ((ProofOfConcept)View.CurrentObject).Save();
                ((ProofOfConcept)View.CurrentObject).Session.CommitTransaction();
            }
        }
        //ATA approve Proof Of concept action that change the status to be approved instead of new and save approval date and approver 1/9/2017 [End]
        //ATA baseline items approv Action that will call setapprovedfunction for each object 1/8/2017 [start]
        private void BaselineItemsApprovale_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (View is DetailView)
            {
                setapproval(View.CurrentObject);
            }
            else
            {
                foreach (object Selectedone in View.SelectedObjects)
                {
                    setapproval(Selectedone);
                }
            }

        }
        //ATA baseline items approv Action that will call setapprovedfunction for each object 1/8/2017 [End]
/// <summary>
/// check the object type and casting it then change the status foor ready or approved based on the objedct type 1/8/2017 [Start]
/// </summary>
/// <param name="approveditem"></param>
        public void setapproval(object approveditem)
        {
            if (approveditem.GetType() == typeof(QAUseCase))
            {
                ((QAUseCase)approveditem).ApproveStatus = QAUseCase.ApproveStatu.Ready;
                ((QAUseCase)approveditem).Save();
                ((QAUseCase)approveditem).Session.CommitTransaction();
            }
            else if (approveditem.GetType() == typeof(Requirement))
            {
                ((Requirement)approveditem).ApproveStatus = Requirement.ApproveStatu.Ready; 
                ((Requirement)approveditem).Save();
                ((Requirement)approveditem).Session.CommitTransaction();
            }
            else if (approveditem.GetType() == typeof(QAProjectEntity))
            {
                ((QAProjectEntity)approveditem).ApproveStatus = QAProjectEntity.ApproveStatu.Ready;
                ((QAProjectEntity)approveditem).Save();
                ((QAProjectEntity)approveditem).Session.CommitTransaction();
            }
            else if (approveditem.GetType() == typeof(TestCase))
            {
                ((TestCase)approveditem).ApproveStatus = TestCase.ApproveStatu.Ready;
                ((TestCase)approveditem).Save();
                ((TestCase)approveditem).Session.CommitTransaction();
            }
            else if (approveditem.GetType() == typeof(EntitySystemDesign))
            {
                ((EntitySystemDesign)approveditem).Status = EntitySystemDesign.StatusItems.Approved;
                ((EntitySystemDesign)approveditem).Save();
                ((EntitySystemDesign)approveditem).Session.CommitTransaction();
            }
            else if (approveditem.GetType() == typeof(QAUseCasePoints))
            {
                if (((QAUseCasePoints)approveditem).WBS.Count >= 4)
                {
                    ((QAUseCasePoints)approveditem).ApproveStatus = QAUseCasePoints.ApproveStatu.Ready;
                    ((QAUseCasePoints)approveditem).Save();
                    ((QAUseCasePoints)approveditem).Session.CommitTransaction();
                }
                else
                {
                    throw new Exception("Use case points can't be approved without at least 4 WBS phases");
                }
               
            }
        }
        /// check the object type and casting it then change the status foor ready or approved based on the objedct type 1/8/2017 [End]
        //
    }
}
