//------------------------------------------------------------------------------
// <auto-generated>
//    This code was generated from a template.
//
//    Manual changes to this file may cause unexpected behavior in your application.
//    Manual changes to this file will be overwritten if the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace Core.Data
{
    using System;
    using System.Collections.Generic;
    
    public partial class AriaFavorite
    {
        public int FavoriteID { get; set; }
        public string UserID { get; set; }
        public Nullable<int> Rank { get; set; }
        public Nullable<int> ParentFavoriteID { get; set; }
        public string FavoriteName { get; set; }
        public Nullable<int> URL { get; set; }
        public Nullable<bool> FolderOrNot { get; set; }
    }
}
