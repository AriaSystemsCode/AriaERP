﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------
using System;
using System.Collections.Generic;

using System.Linq;

using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module;

namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
    [DefaultClassOptions]
    [IsClient(true, false)]
    
    [RelatedEntity("Aria5-Windows8Xaml-Food")]
    [MapInheritance(MapInheritanceType.ParentTable)]
    [DivisionAttribute(true)]
    public partial class Food : ClientEntity
    {
        private System.String _name;
        private System.String _cookingMethod;
        private System.String _course;
        private System.String _cuisine;
        private System.String _dietary;
        private System.String _majorIngredient;
        private System.String _family;
        private System.Double _price;
        private System.String _ingredients;
        private System.Boolean _isSpecial;
        private System.Boolean _isNewItem;
        private System.DateTime _startPeriod;
        private System.DateTime _endPeriod;
        private System.Int32 _carbohydrates;
        private System.Int32 _protein;
        private System.Int32 _sodium;
        private System.Int32 _calories;
        private System.Int32 _totalFat;
        private System.Int32 _saturatedFat;
        private System.Int32 _transFat;
        private System.Int32 _allergens;
        private System.Int32 _cholesterol;
        private System.Int32 _sugar;
        public Food(Session session)
            : base(session)
        {

        }
      [NonPersistentAttribute]
        public System.String Name
        {
            get
            {
                return _name;
            }
            set
            {
                SetPropertyValue("Name", ref _name, value);
            }
        }
        [NonPersistentAttribute]
        public System.String CookingMethod
        {
            get
            {
                return _cookingMethod;
            }
            set
            {
                SetPropertyValue("CookingMethod", ref _cookingMethod, value);
            }
        }
        [NonPersistentAttribute]
        public System.String Course
        {
            get
            {
                return _course;
            }
            set
            {
                SetPropertyValue("Course", ref _course, value);
            }
        }

        [NonPersistentAttribute]
        public System.String Cuisine
        {
            get
            {
                return _cuisine;
            }
            set
            {
                SetPropertyValue("Cuisine", ref _cuisine, value);
            }
        }

        [NonPersistentAttribute]
        public System.String Dietary
        {
            get
            {
                return _dietary;
            }
            set
            {
                SetPropertyValue("Dietary", ref _dietary, value);
            }
        }

        [NonPersistentAttribute]
        public System.String MajorIngredient
        {
            get
            {
                return _majorIngredient;
            }
            set
            {
                SetPropertyValue("MajorIngredient", ref _majorIngredient, value);
            }
        }

        [NonPersistentAttribute]
        public System.String Family
        {
            get
            {
                return _family;
            }
            set
            {
                SetPropertyValue("Family", ref _family, value);
            }
        }

        [NonPersistentAttribute]
        public System.Double Price
        {
            get
            {
                return _price;
            }
            set
            {
                SetPropertyValue("Price", ref _price, value);
            }
        }

        [NonPersistentAttribute]
        public System.String Ingredients
        {
            get
            {
                return _ingredients;
            }
            set
            {
                SetPropertyValue("Ingredients", ref _ingredients, value);
            }
        }

        [NonPersistentAttribute]
        public System.Boolean IsSpecial
        {
            get
            {
                return _isSpecial;
            }
            set
            {
                SetPropertyValue("IsSpecial", ref _isSpecial, value);
            }
        }

        [NonPersistentAttribute]
        public System.Boolean IsNewItem
        {
            get
            {
                return _isNewItem;
            }
            set
            {
                SetPropertyValue("IsNewItem", ref _isNewItem, value);
            }
        }

        [NonPersistentAttribute]
        public System.DateTime StartPeriod
        {
            get
            {
                return _startPeriod;
            }
            set
            {
                SetPropertyValue("StartPeriod", ref _startPeriod, value);
            }
        }

        [NonPersistentAttribute]
        public System.DateTime EndPeriod
        {
            get
            {
                return _endPeriod;
            }
            set
            {
                SetPropertyValue("EndPeriod", ref _endPeriod, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 Carbohydrates
        {
            get
            {
                return _carbohydrates;
            }
            set
            {
                SetPropertyValue("Carbohydrates", ref _carbohydrates, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 Protein
        {
            get
            {
                return _protein;
            }
            set
            {
                SetPropertyValue("Protein", ref _protein, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 Sodium
        {
            get
            {
                return _sodium;
            }
            set
            {
                SetPropertyValue("Sodium", ref _sodium, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 Calories
        {
            get
            {
                return _calories;
            }
            set
            {
                SetPropertyValue("Calories", ref _calories, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 TotalFat
        {
            get
            {
                return _totalFat;
            }
            set
            {
                SetPropertyValue("TotalFat", ref _totalFat, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 SaturatedFat
        {
            get
            {
                return _saturatedFat;
            }
            set
            {
                SetPropertyValue("SaturatedFat", ref _saturatedFat, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 TransFat
        {
            get
            {
                return _transFat;
            }
            set
            {
                SetPropertyValue("TransFat", ref _transFat, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 Allergens
        {
            get
            {
                return _allergens;
            }
            set
            {
                SetPropertyValue("Allergens", ref _allergens, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 Cholesterol
        {
            get
            {
                return _cholesterol;
            }
            set
            {
                SetPropertyValue("Cholesterol", ref _cholesterol, value);
            }
        }

        [NonPersistentAttribute]
        public System.Int32 Sugar
        {
            get
            {
                return _sugar;
            }
            set
            {
                SetPropertyValue("Sugar", ref _sugar, value);
            }
        }
    }
}
