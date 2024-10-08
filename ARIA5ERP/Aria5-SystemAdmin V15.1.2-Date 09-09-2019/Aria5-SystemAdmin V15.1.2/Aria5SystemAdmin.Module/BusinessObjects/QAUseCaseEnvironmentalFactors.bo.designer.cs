//------------------------------------------------------------------------------
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
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-QAUseCaseEnvironmentalFactors")]

    public partial class QAUseCaseEnvironmentalFactors :QAUseCaseTechnicalFactors// DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.QAUseCasePoints _useCasePoint;
    public QAUseCaseEnvironmentalFactors(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

      // Sara.N, Customize Clonning of usecase Points [Start]
    public QAUseCaseEnvironmentalFactors(DevExpress.Xpo.Session session, QAUseCaseEnvironmentalFactors UseCaseEnvironmentalFactors)
        : base(session)
    {
        this.Description = UseCaseEnvironmentalFactors.Description;
        this.Relivance = UseCaseEnvironmentalFactors.Relivance;
        this.Score = UseCaseEnvironmentalFactors.Score;
        this.UseCasePoints = UseCaseEnvironmentalFactors.UseCasePoints;
        this.UseCasePoint = UseCaseEnvironmentalFactors.UseCasePoint;
        this.Weight = UseCaseEnvironmentalFactors.Weight;

    }

    // Sara.N, Customize Clonning of usecase Points [End]
    [DevExpress.Xpo.AssociationAttribute("UseCaseEnvironmentalFactors-UseCasePoint")]
    public Aria5SystemAdmin.Module.BusinessObjects.QAUseCasePoints UseCasePoint
    {
      get
      {
        return _useCasePoint;
      }
      set
      {
        SetPropertyValue("UseCasePoint", ref _useCasePoint, value);
      }
    }

    //private System.Int16 _weight;
    //private System.String _description;
    //private short _score;
    //private System.Int16 _relivance;
    //private Aria5SystemAdmin.Module.BusinessObjects.QAUseCasePoints _useCasePoints;
    
  
    //[DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    //public System.Int16 Weight
    //{
    //  get
    //  {
    //    return _weight;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Weight", ref _weight, value);
    //  }
    //}
    //[DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    //public System.Int16 Relivance
    //{
    //  get
    //  {
    //    return _relivance;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Relivance", ref _relivance, value);
    //  }
    //}
    //public short Score
    //{
    //  get
    //  {
    //    _score = (short)(Relivance * Weight);
    //    return _score;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Score", ref _score, value);
    //  }
    //}
    //public System.String Description
    //{
    //  get
    //  {
    //    return _description;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Description", ref _description, value);
    //  }
    //}
  }
}
