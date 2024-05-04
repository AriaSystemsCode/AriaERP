using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;

namespace AriaDevExpress.Module.BusinessObjects.HR
{

    public enum Gender
    {
        Select,
        Male,
        Female
    }
    public enum Employment_status
    {
        Select,
        [DisplayName("Full time")]
        Fulltime,
        [DisplayName("Part time")]
        Parttime,
        [DisplayName("Free lancing")]
        Freelancing
    }
    public enum Military_status
    {
        Select,
        Complete,
        [DisplayName("Will serve")]
        Willserve,
        Postponed,
        Exempted
    }
    public enum Marital_status
    {
        Select,
        Single,
        Engaged,
        Married
    }
    public enum Title { Mr, Mrs, Ms, Miss }

}
