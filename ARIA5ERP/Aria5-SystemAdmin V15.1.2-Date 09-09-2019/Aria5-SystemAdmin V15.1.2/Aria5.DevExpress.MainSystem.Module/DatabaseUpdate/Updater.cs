using System;
using System.Security.Principal;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Updating;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using Aria5.DevExpress.MainSystem.Module.BusinessObjects;

namespace Aria5.DevExpress.MainSystem.Module.DatabaseUpdate
{
    public class Updater : ModuleUpdater
    {
        public Updater(IObjectSpace objectSpace, Version currentDBVersion) : base(objectSpace, currentDBVersion) { }
        public override void UpdateDatabaseAfterUpdateSchema()
        {
            base.UpdateDatabaseAfterUpdateSchema();

            #region Sara, Adding IdentifierStructure initial data 6/7/2014
            IdentifierStructure identifierStructure = ObjectSpace.FindObject<IdentifierStructure>(CriteriaOperator.Parse("IsVisible = 1 OR IsVisible = 0"));

            if (identifierStructure == null)
            {

                identifierStructure = ObjectSpace.CreateObject<IdentifierStructure>();
                identifierStructure.IdentifierStructureId = "ACC-001";
                identifierStructure.Description = "Account";
                identifierStructure.NoOfSegments = 1;
                identifierStructure.IdentifierLabel = "Account";
                identifierStructure.IsVisible = true;
                identifierStructure.Save();

            }

            #endregion

            #region Sara, Adding IdentifierSegment initial data 6/7/2014
            IdentifierSegment identifierSegment = ObjectSpace.FindObject<IdentifierSegment>(CriteriaOperator.Parse("NextIdentity == '2'"));

            if (identifierSegment == null)
            {

                identifierSegment = ObjectSpace.CreateObject<IdentifierSegment>();
                identifierSegment.IdentifierStructure = identifierStructure;
                identifierSegment.Position = 1;
                identifierSegment.Description = "Account";
                identifierSegment.Caption = "Account";

                identifierSegment.SeparatorAfter = ' ';

                identifierSegment.Length = 10;
                identifierSegment.Dimension = 1;
                identifierSegment.ValueType = ((ValueTypes)Enum.Parse(typeof(ValueTypes), "Identity"));
                identifierSegment.InputMask = "9999999999";
                identifierSegment.Format = "9999999999";
                identifierSegment.IdentitySeed = 500000000;
                identifierSegment.IdentityIncrement = 1;
                identifierSegment.NextIdentity = 2;
                identifierSegment.Save();

            }

            #endregion




        }
    }
}
