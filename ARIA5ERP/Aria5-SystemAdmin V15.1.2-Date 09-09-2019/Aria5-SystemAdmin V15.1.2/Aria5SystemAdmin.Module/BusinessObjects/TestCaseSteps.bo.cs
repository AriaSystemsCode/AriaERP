using DevExpress.Data.Filtering;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class TestCaseSteps
    {

        public override void AfterConstruction()
        {
            //int i = 0;
            //foreach (var tcs in this.TestCase.TestCaseSteps)
            //{
            //    if (tcs.Oid != Oid && tcs.IsDeleted() == false)
            //    {
            //        i++;
            //    }
            //}

           // base.AfterConstruction();
           // Aria5SystemAdmin.Module.Aria5Globals aria5Globals = new Aria5Globals();
           // this.Number = aria5Globals.GetSequence("TestCaseSteps", "Number", Session, "TestCase = '" + this.TestCase.Oid.ToString() + "'");
           ////Number &= Convert.ToInt32(this.Session.Evaluate(TestCaseSteps)(DevExpress.Data.Filtering.CriteriaOperator.Parse("Max(Number)"), nothing)) + 1;
        }
        protected override void OnDeleting()
        {
            base.OnDeleting();
            //ATA renumber the steps if you delete any one of it 4/12/2017[start]
            foreach (TestCaseSteps step in this.TestCase.TestCaseSteps)
            {
                if (step.Number > this.Number)
                {
                    step.Number -= 1;
                    step.Save();
                }
            }
            //ATA renumber the steps if you delete any one of it 4/12/2017[End]

        }

        //protected override void OnSaving()
        //{
        //    base.OnSaving();
        // //   int x = this.TestCase.TestCaseSteps.Count;

        // //   Number = (short)(x + 1);
          
            
        //}
        //protected override void OnDeleting()
        //{
        //    base.OnDeleting();
        //    for (int i= 0; i <this.Number; i++)
        //    {

               
        //    }

        //}
    }
}
