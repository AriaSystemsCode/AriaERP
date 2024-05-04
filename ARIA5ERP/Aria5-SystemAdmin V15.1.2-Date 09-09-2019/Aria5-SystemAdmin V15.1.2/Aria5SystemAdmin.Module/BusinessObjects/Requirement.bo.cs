using System;
using System.Collections.Generic;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using Aria5SystemAdmin.Module.Managers;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Requirement
    {
        
        
        public override void AfterConstruction()
        {
            base.AfterConstruction();
            if (_account == null)
            {
               // this._account = this.Session.FindObject<Account>(CriteriaOperator.Parse("Oid = '10272A1E-CB80-4C8A-9E3A-B4CB4334814C'"));
                this.Account = this.Session.FindObject<Account>(CriteriaOperator.Parse("[Name] = 'Aria Systems INC.'"));
            }
            /*
            foreach (QAUseCase usecase in this.Usecases)
            {
                bool flag = false;
                foreach (QAProjectEntity projectentity in this.QAProjectEntities)
                    {
                    
                
                    foreach (QAProjectEntity entity in usecase.ProjectEntities)
                    {
                        if (projectentity == entity)
                        {
                            flag = true;
                            break;

                        }
                    }
                    if (!flag)
                    {
                        this.QAProjectEntities.Remove(projectentity);
                    }
                    else
                    {
                        continue;
                    }
                } }*/



            //        base.AfterConstruction();
        //        if (this.RequirementType.Name=="Needs")
        //        {
        //            this.Entity.
        //        }
        }

        protected override void OnSaving()
        {
            #region commented code
            /*
            
                foreach (QAUseCase usecase in this.Usecases)
                {
                    bool flag = false;
                    foreach (QAProjectEntity entity in usecase.ProjectEntities)
                    {
                        foreach (QAProjectEntity projectentity in this.QAProjectEntities)
                        {
                            if (projectentity == entity)
                            {
                                flag = true;
                                break;

                            }
                        }
                        if (!flag)
                        {
                            this.QAProjectEntities.Add(entity);
                            
                        }
                        else
                        {
                            continue;
                        }
                    }
                }*/
            #endregion
            if (this.Session.IsNewObject(this))
            {
                USECASE = true;
                PROJECTENTITY = true;
                TESTCASE = true;
                //ATA 17/7/2016 notification customization [start]
                if (this.Application_T != null)
                {
                    int x = Application_T.Requirements.Count + 1;
                    this.SequenceNum = this.Application_T + "-" + x.ToString("0000");

                }
            }
           
            if (this.ProjectTemplate != null)
            {
                if (this.ApproveStatus == ApproveStatu.Ready)
                {
                    if (this.Usecases.Count == 0)
                    {
                        throw new Exception("this requirement can't be ready because there is no use case linked to it");
                    }
                }
                this.Session.Save(this.ProjectTemplate);
                //ATA 1/11/2017 change the requirement statues while linking to a project [Start]
                if (this.RequirementType.Name == "Feature")
                {
                    if (this.RequirementStatus == RequirmentsStatus.New)
                    {
                        this.RequirementStatus = RequirmentsStatus.InWork;
                    }
                    this.Session.Save(this.Needs);
                }
                //BaselineNCsManager NCmanager = new BaselineNCsManager();
                //NCmanager.CheckNCssolved(this.ProjectTemplate);
               
            }
            else
            {
                if(this.Features.Count > 0)
                {
                    this.RequirementStatus = RequirmentsStatus.Passed;
                    foreach (Requirement feature in this.Features)
                    {

                        if (feature.RequirementStatus != RequirmentsStatus.Passed)
                        {
                            this.RequirementStatus = RequirmentsStatus.InWork;
                        }
                    }
                }
                //ATA 1/11/2017 change the requirement statues while linking to a project [End]
                
            }
                //ATA 17/7/2016 notification customization [END]
            
            base.OnSaving();
        }

        protected override void OnDeleting()
        {
            if (DateTime.Today > CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 2))
            {
                base.OnDeleting();
            }
            else if (DateTime.Today >= CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0)))
            {
                if (this.ProjectTemplate.Requirements.Count == 1)
                {
                    this.ProjectTemplate.AlarmTime = DateTime.Today;
                    this.Session.Save(this.ProjectTemplate);
                    base.OnDeleting();
                }
            }
        }
        
       
    }
}
