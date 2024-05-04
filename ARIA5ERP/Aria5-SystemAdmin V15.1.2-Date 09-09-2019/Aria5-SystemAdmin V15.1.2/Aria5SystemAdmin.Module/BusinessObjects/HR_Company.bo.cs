using Aria5.DevExpress.MainSystem.Module.Managers;
using DevExpress.Data.Filtering;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HR_Company
    {
        public override void AfterConstruction()
        {
                base.AfterConstruction();
                //if (this.Session.IsNewObject(this))
                //{
                //    HR_EntityType temp = null;
                //    temp = this.Session.FindObject<HR_EntityType>(CriteriaOperator.Parse("[Id] == 'Comp'"));
                //    if (temp != null)
                //    {
                //        this.Type = temp;
                //    }
                //    if (this.Type != null && this.Type.IdentifierStructture != null)
                //    {
                //        this.Code = IdentifierStructureManager.GetNextId(this.Type.IdentifierStructture.IdentifierStructureId, new Dictionary<int, object>(),this.Session);
                //    }
                //}
        }
    }
}
