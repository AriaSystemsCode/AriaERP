using DevExpress.ExpressApp.ConditionalAppearance;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [Appearance("TrackingDisablingInTC", AppearanceItemType = "ViewItem", Context = "DetailView", TargetItems = "TrackingEntry", Criteria = ("TrackingEntry is not null"), Enabled = false)]
    [Appearance("TitleDisablingInTC", AppearanceItemType = "ViewItem", Context = "DetailView", TargetItems = "Title", Criteria = ("Title is not null"), Enabled = false)]

    public partial class TestCase 
    {
      
      

        //protected override void OnLoading()
        //{
        //    int count = 0;
        //    for (int i = 0; i < this.TestCaseSteps.Count; i++)
        //    {
        //        if (this.TestCaseSteps[i].Passed == false)
        //        {
        //            count++;
        //        }

        //    }
        //    if (count > 0)
        //    {
        //        this.Status = status.Failed;
        //    }
        //    else
        //    {
        //        this.Status = status.Passed;
        //    }
        //    count = 0;
        //    base.OnLoading();
        //}
   
      
        protected override void OnSaving()
        {
            
                //ATA 17/7/2016 notification customization [end]
            if (this.Session.IsNewObject(this))
            {
                
            }
                if (this.ProjectTemplate != null && this.IsOriginal == true)
                {
                    if (this.ApproveStatus == ApproveStatu.Ready)
                    {
                        if (this.TestCaseSteps.Count == 0)
                        {
                            throw new Exception("can't save this test case as ready without steps");
                        }
                        else
                        {
                            if (this.ProjectTemplate.AlarmTime != null && this.ProjectTemplate.Requirements.Count > 0 && this.ProjectTemplate.UseCases.Count > 0 && this.ProjectTemplate.ProjectEntities.Count > 0)
                            {
                                this.Session.Save(this.ProjectTemplate);
                            }   
                        }
                    }
                    //set alarm time to null as per no notification after m3
                   
                }
                //cancel requirement notification after add tet cases and found that usecases added and project entity added 
                //if (this._requirement != null && this.Requirement.Usecases.Count > 0)
                //{
                //    if (this._requirement.AlarmTime != null)
                //    {
                //        this._requirement.AlarmTime = null;
                //        this.Session.Save(this.Requirement);
                //    }
                //}
            //ATA 17/7/2016 notification customization [end]
            base.Save();
            //if (Session.IsNewObject(this))
            //{
            //    this.IsOriginal = true;
            //}
            //base.OnSaving();
        //    int count = 0;
        //    for (int i = 0; i < this.TestCaseSteps.Count; i++)
        //    {
        //        if (this.TestCaseSteps[i].Passed == false)
        //        {
        //            count++;
        //        }

        //    }
        //    if (count > 0)
        //    {
        //        this.Status = status.Failed;
        //    }
        //    else
        //    {
        //        this.Status = status.Passed;
        //    }
        //    count = 0;
        //    base.OnSaving();
          
        }
        protected override void OnDeleting()
        { 
            if (this.IsOriginal == true)
            {
                if (DateTime.Today > CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 3))
                {
                    base.OnDeleting();
                }
                else if (DateTime.Today >= CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 3).Subtract(new TimeSpan(10, 0, 0, 0)))
                {
                    if (this.ProjectTemplate.UseCases.Count == 1)
                    {
                        //this.ProjectTemplate.NotificationMessage = string.Format("you need to add this items before this date '{0}' for this project'{1}'", CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 2), this.ProjectTemplate.Name);
                        //this.ProjectTemplate.NotificationMessage += " Use cases";
                        //if (this.ProjectTemplate.Requirements.Count == 0)
                        //{
                        //    this.ProjectTemplate.NotificationMessage += " requirements";
                        //}
                        //if (this.ProjectTemplate.ProjectEntities.Count == 0)
                        //{
                        //    this.ProjectTemplate.NotificationMessage += " Project entities ";
                        //}

                        this.ProjectTemplate.AlarmTime = DateTime.Today;
                        this.Session.Save(this.ProjectTemplate);

                    } if (this.Requirement.Usecases.Count == 1)
                    {
                        this.Session.Save(this.Requirement);
                    }
                }
            }
                base.OnDeleting();
           
        }
        //ATA bgn 15/3/2016
        /*
        public DateTime CalcEnddate(DateTime StartDatetime, int MonthesCount)
        {
            DateTime end = DateTime.Now;
            DateTime firstDayafterNMonth = StartDatetime.AddDays(-StartDatetime.Day + 1).AddMonths(MonthesCount);
            int diff = 7 - (int)firstDayafterNMonth.DayOfWeek;
            end = firstDayafterNMonth.AddDays(diff);


            return end;
        }
        //ATA end 
        */
        //protected override void OnDeleting()
        //{
        //    if (Session.CollectReferencingObjects(this).Count > 0)
        //    {
        //        throw new Exception("Can not Delete Test Case!");
        //    }
        //    else
        //    {
        //        base.OnDeleting();
        //    } 

        //}

        public override void AfterConstruction()
        {
            // Doaa 05/01/2019 {start}
            if(this.Status == null)
             this.Status = status.New;
            // Doaa 05/01/2019 {End}

            base.AfterConstruction();

        }
    }
}
