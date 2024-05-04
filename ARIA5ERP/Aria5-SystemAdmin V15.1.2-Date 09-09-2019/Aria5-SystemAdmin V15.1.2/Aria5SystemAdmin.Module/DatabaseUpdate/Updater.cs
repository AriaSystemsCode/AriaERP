using System;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Updating;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.Persistent.BaseImpl;
using DevExpress.ExpressApp.Security;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Security.Strategy;

namespace Aria5SystemAdmin.Module.DatabaseUpdate
{
    public class Updater : ModuleUpdater
    {
        public Updater(IObjectSpace objectSpace, Version currentDBVersion) : base(objectSpace, currentDBVersion) { }
        public override void UpdateDatabaseAfterUpdateSchema()
        {

            base.UpdateDatabaseAfterUpdateSchema();

            #region HIA, Adding Entity Status initial Data 2013-11-06 [Begin]
            EntityStatus entityTypeStatus = ObjectSpace.FindObject<EntityStatus>(
            CriteriaOperator.Parse("StatusID == 'ACTIVE'"));
            if (entityTypeStatus == null)
            {

                entityTypeStatus = ObjectSpace.CreateObject<EntityStatus>();
                entityTypeStatus.StatusID = "ACTIVE".ToUpper();
                entityTypeStatus.Description = "Active";
                entityTypeStatus.Save();
            }

            #endregion HIA, Adding Entity Status initial Data 2013-11-06 [End]

            #region HIA, Adding Entity Type initial Data 2013-11-06 [Begin]
            // HIA, Adding Entity Type initial Data 2013-11-06 [Begin]
            // Business, Events, Information, News, Ads, Administration and Public Services
            EntityType entityTypeBusiness = ObjectSpace.FindObject<EntityType>(
            CriteriaOperator.Parse("TypeId == 'BUSINESS'"));
            if (entityTypeBusiness == null)
            {
                entityTypeBusiness = ObjectSpace.CreateObject<EntityType>();
                entityTypeBusiness.TypeId = "Business".ToUpper();
                entityTypeBusiness.Name = "Business";
                entityTypeBusiness.Save();
            }

            #region Adding Business sub types
            EntityType entityTypeShopping = ObjectSpace.FindObject<EntityType>(
            CriteriaOperator.Parse("TypeId == 'SHOPPING'"));

            EntityType entityTypeBusinessExist = ObjectSpace.FindObject<EntityType>(
            CriteriaOperator.Parse("TypeId == 'BUSINESS'"));
            if (entityTypeBusinessExist != null && entityTypeShopping == null)
            {
                entityTypeShopping = ObjectSpace.CreateObject<EntityType>();
                entityTypeShopping.TypeId = "SHOPPING".ToUpper();
                entityTypeShopping.Name = "Shopping";
                entityTypeShopping.ParentType = entityTypeBusinessExist;
                entityTypeShopping.Save();
            }

            EntityType entityTypeRestaurant = ObjectSpace.FindObject<EntityType>(
                 CriteriaOperator.Parse("TypeId == 'RESTAURANT'"));

            if (entityTypeBusinessExist != null && entityTypeRestaurant == null)
            {
                entityTypeRestaurant = ObjectSpace.CreateObject<EntityType>();
                entityTypeRestaurant.TypeId = "RESTAURANT".ToUpper();
                entityTypeRestaurant.Name = "Restaurant";
                entityTypeRestaurant.ParentType = entityTypeBusinessExist;
                entityTypeRestaurant.Save();
            }

            #endregion Adding Business sub types

            EntityType entityTypeEvents = ObjectSpace.FindObject<EntityType>(
                        CriteriaOperator.Parse("TypeId == 'EVENTS'"));
            if (entityTypeEvents == null)
            {
                entityTypeEvents = ObjectSpace.CreateObject<EntityType>();
                entityTypeEvents.TypeId = "Events".ToUpper();
                entityTypeEvents.Name = "Events";
                entityTypeEvents.Save();
            }


            EntityType entityTypeInformation = ObjectSpace.FindObject<EntityType>(
                                    CriteriaOperator.Parse("TypeId == 'INFORMATION'"));
            if (entityTypeInformation == null)
            {
                entityTypeInformation = ObjectSpace.CreateObject<EntityType>();
                entityTypeInformation.TypeId = "Information".ToUpper();
                entityTypeInformation.Name = "Information";
                entityTypeInformation.Save();
            }

            EntityType entityTypeNews = ObjectSpace.FindObject<EntityType>(
                                    CriteriaOperator.Parse("TypeId == 'NEWS'"));
            if (entityTypeNews == null)
            {
                entityTypeNews = ObjectSpace.CreateObject<EntityType>();
                entityTypeNews.TypeId = "News".ToUpper();
                entityTypeNews.Name = "News";
                entityTypeNews.Save();
            }

            EntityType entityTypeAds = ObjectSpace.FindObject<EntityType>(
                                    CriteriaOperator.Parse("TypeId == 'ADS'"));
            if (entityTypeAds == null)
            {
                entityTypeAds = ObjectSpace.CreateObject<EntityType>();
                entityTypeAds.TypeId = "ADS".ToUpper();
                entityTypeAds.Name = "Advertisement";
                entityTypeAds.Save();
            }

            EntityType entityTypeAdministration = ObjectSpace.FindObject<EntityType>(
                                    CriteriaOperator.Parse("TypeId == 'PERSON'"));
            if (entityTypeAdministration == null)
            {
                entityTypeAdministration = ObjectSpace.CreateObject<EntityType>();
                entityTypeAdministration.TypeId = "PERSON".ToUpper();
                entityTypeAdministration.Name = "Administration";
                entityTypeAdministration.Save();
            }

            EntityType entityTypePUBLICSERVICES = ObjectSpace.FindObject<EntityType>(
                                    CriteriaOperator.Parse("TypeId == 'PUBLICSERVICES'"));
            if (entityTypePUBLICSERVICES == null)
            {
                entityTypePUBLICSERVICES = ObjectSpace.CreateObject<EntityType>();
                entityTypePUBLICSERVICES.TypeId = "PUBLICSERVICES".ToUpper();
                entityTypePUBLICSERVICES.Name = "Public Services";
                entityTypePUBLICSERVICES.Save();
            }

            // HIA, Adding Entity Type initial Data 2013-11-06 [End]
            #endregion HIA, Adding Entity Type initial Data 2013-11-06 [Begin]

            #region HIA, Adding Entity Category initial Data 2013-11-06 [Begin]
            //The ‘Public Services’ entity type will have the following categories by default:
            // Toilets, Exits, Stairs, Escalators, Elevators, ATM, Entrance, Parking, Mosque 
            EntityType entityTypePUBLICSERVICESExist = ObjectSpace.FindObject<EntityType>(
                                    CriteriaOperator.Parse("TypeId == 'PUBLICSERVICES'"));

            EntityCategory entityCategoryToilets = ObjectSpace.FindObject<EntityCategory>(
                                    CriteriaOperator.Parse("CategoryId == 'TOILETS'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryToilets == null)
            {
                entityCategoryToilets = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryToilets.CategoryId = "TOILETS".ToUpper();
                entityCategoryToilets.Name = "Toilets";
                entityCategoryToilets.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryToilets.Save();
            }

            EntityCategory entityCategoryExits = ObjectSpace.FindObject<EntityCategory>(
                        CriteriaOperator.Parse("CategoryId == 'EXITS'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryExits == null)
            {
                entityCategoryExits = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryExits.CategoryId = "EXITS".ToUpper();
                entityCategoryExits.Name = "Exits";
                entityCategoryExits.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryExits.Save();
            }


            EntityCategory entityCategoryStairs = ObjectSpace.FindObject<EntityCategory>(
             CriteriaOperator.Parse("CategoryId == 'STAIRS'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryStairs == null)
            {
                entityCategoryStairs = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryStairs.CategoryId = "STAIRS".ToUpper();
                entityCategoryStairs.Name = "Stairs";
                entityCategoryStairs.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryStairs.Save();
            }

            EntityCategory entityCategoryEscalators = ObjectSpace.FindObject<EntityCategory>(
             CriteriaOperator.Parse("CategoryId == 'ESCALATORS'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryEscalators == null)
            {
                entityCategoryEscalators = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryEscalators.CategoryId = "ESCALATORS".ToUpper();
                entityCategoryEscalators.Name = "Escalators";
                entityCategoryEscalators.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryEscalators.Save();
            }

            EntityCategory entityCategoryElevators = ObjectSpace.FindObject<EntityCategory>(
             CriteriaOperator.Parse("CategoryId == 'ELEVATORS'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryElevators == null)
            {
                entityCategoryElevators = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryElevators.CategoryId = "ELEVATORS".ToUpper();
                entityCategoryElevators.Name = "Elevators";
                entityCategoryElevators.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryElevators.Save();
            }

            EntityCategory entityCategoryATM = ObjectSpace.FindObject<EntityCategory>(
             CriteriaOperator.Parse("CategoryId == 'ATM'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryATM == null)
            {
                entityCategoryATM = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryATM.CategoryId = "ATM".ToUpper();
                entityCategoryATM.Name = "ATM";
                entityCategoryATM.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryATM.Save();
            }

            // Parking, Mosque 
            EntityCategory entityCategoryEntrance = ObjectSpace.FindObject<EntityCategory>(
             CriteriaOperator.Parse("CategoryId == 'ENTRANCE'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryEntrance == null)
            {
                entityCategoryEntrance = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryEntrance.CategoryId = "ENTRANCE".ToUpper();
                entityCategoryEntrance.Name = "Entrance";
                entityCategoryEntrance.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryEntrance.Save();
            }


            EntityCategory entityCategoryParking = ObjectSpace.FindObject<EntityCategory>(
             CriteriaOperator.Parse("CategoryId == 'PARKING'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryParking == null)
            {
                entityCategoryParking = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryParking.CategoryId = "PARKING".ToUpper();
                entityCategoryParking.Name = "Parking";
                entityCategoryParking.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryParking.Save();
            }

            EntityCategory entityCategoryMosque = ObjectSpace.FindObject<EntityCategory>(
             CriteriaOperator.Parse("CategoryId == 'MOSQUE'"));

            if (entityTypePUBLICSERVICESExist != null && entityCategoryMosque == null)
            {
                entityCategoryMosque = ObjectSpace.CreateObject<EntityCategory>();
                entityCategoryMosque.CategoryId = "MOSQUE".ToUpper();
                entityCategoryMosque.Name = "Mosque";
                entityCategoryMosque.EntityType = entityTypePUBLICSERVICESExist;
                entityCategoryMosque.Save();
            }



            #endregion HIA, Adding Entity Category initial Data 2013-11-06 [End]

            #region HIA, Adding Location Type initial Data 2013-11-26 [Begin]

            string initialLocationType = "Building,Level,Floor,Apartment,Section,Room,Hall,Stand,Aisle,Bin,Shelf or Partition";
            string[] words = initialLocationType.Split(',');
            foreach (string word in words)
            {
                try
                {
                    AddInitialLocationType(word);
                }
                catch (Exception ex)
                { };

            }

            #endregion HIA, Adding Location Type initial Data 2013-11-26 [End]

            #region HES, Adding Default Admin user 07-03-2014 [Begin]

            SecuritySystemRole adminRole = ObjectSpace.FindObject<SecuritySystemRole>(new BinaryOperator("Name", SecurityStrategy.AdministratorRoleName));
            if (adminRole == null)
            {
                adminRole = ObjectSpace.CreateObject<SecuritySystemRole>();
                adminRole.Name = SecurityStrategy.AdministratorRoleName;
                adminRole.IsAdministrative = true;
                adminRole.CanEditModel = false;
            }
            // If a user named 'Sam' doesn't exist in the database, create this user 
            SecuritySystemUser user1 = ObjectSpace.FindObject<SecuritySystemUser>(
              new BinaryOperator("UserName", "Admin"));
            if (user1 == null)
            {
                user1 = ObjectSpace.CreateObject<SecuritySystemUser>();
                user1.UserName = "Admin";
                // Set a password if the standard authentication type is used 
                user1.SetPassword("");
                user1.Roles.Add(adminRole);
            }

            #endregion HES, Adding Default Admin user 07-03-2014 [End]

        }

        #region HIA, Adding Location Type initial Data 2013-11-26 [Begin]
        public void AddInitialLocationType(string IdString)
        {

            // LocationType LocationTypeObject = ObjectSpace.FindObject<LocationType>(
            //   CriteriaOperator.Parse("Id == '" + IdString.ToUpper() + "'"));
            //    if (LocationTypeObject == null)
            //    {

            //        LocationType LocationTypeObjectNew = ObjectSpace.CreateObject<LocationType>();
            //        LocationTypeObjectNew.Id = IdString.ToUpper();
            //        LocationTypeObjectNew.Description = IdString;
            //        LocationTypeObjectNew.Save();
            //    }
            //}
        #endregion HIA, Adding Location Type initial Data 2013-11-26 [End]

        }
    }
}