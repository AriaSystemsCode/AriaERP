using Aria5.DevExpress.MainSystem.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class TypeSettings
    {

        protected override void OnSaving()
        {
            if (Settingtype != null && Settingtype.Name == "Identifier Structure")
            {
                IdentifierStructure Idst = this.Session.FindObject<IdentifierStructure>(CriteriaOperator.Parse("[IdentifierStructureId] = '" + this.Value + "'"));
                if (Idst == null)
                {
                    throw new Exception("There is no Identifier Structure with this value please enter a valid ID in value field");
                }
            }
            base.OnSaving();
        }
    }
}
