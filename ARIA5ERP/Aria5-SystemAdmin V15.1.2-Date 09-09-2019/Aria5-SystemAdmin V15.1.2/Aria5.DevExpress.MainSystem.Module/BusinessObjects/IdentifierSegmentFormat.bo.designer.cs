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
namespace Aria5.DevExpress.MainSystem.Module.BusinessObjects
{
  //[DefaultClassOptions]
  public partial class IdentifierSegmentFormat : BaseObject
  {
      
    private System.String _applyWhen; 
    private System.String _expression;
    private System.Int64 _nextIdentity;
    private System.Int16 _identityIncrement;
    private System.Int64 _identitySeed;
    private System.String _format;
    private System.String _inputMask;
    private System.String _title;
    private Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierSegment _identifierSegment;
    public IdentifierSegmentFormat(Session session)
      : base(session)
    {
    }
    [Association("Segments-Formats")]
    public Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierSegment IdentifierSegment
    {
      get
      {
        return _identifierSegment;
      }
      set
      {
        SetPropertyValue("IdentifierSegment", ref _identifierSegment, value);
      }
    }
    public System.String Title
    {
      get
      {
        return _title;
      }
      set
      {
        SetPropertyValue("Title", ref _title, value);
      }
    }
    [SizeAttribute(30)]
    public System.String InputMask
    {
      get
      {
        return _inputMask;
      }
      set
      {
        SetPropertyValue("InputMask", ref _inputMask, value);
      }
    }
    [SizeAttribute(30)]
    public System.String Format
    {
      get
      {
        return _format;
      }
      set
      {
        SetPropertyValue("Format", ref _format, value);
      }
    }
    public System.Int64 IdentitySeed
    {
      get
      {
        return _identitySeed;
      }
      set
      {
        SetPropertyValue("IdentitySeed", ref _identitySeed, value);
      }
    }
    public System.Int16 IdentityIncrement
    {
      get
      {
        return _identityIncrement;
      }
      set
      {
        SetPropertyValue("IdentityIncrement", ref _identityIncrement, value);
      }
    }
    public System.Int64 NextIdentity
    {
      get
      {
        return _nextIdentity;
      }
      set
      {
        SetPropertyValue("NextIdentity", ref _nextIdentity, value);
      }
    }
    [SizeAttribute(-1)]
    public System.String Expression
    {
      get
      {
        return _expression;
      }
      set
      {
        SetPropertyValue("Expression", ref _expression, value);
      }
    }
    [SizeAttribute(-1)]
    public System.String ApplyWhen
    {
      get
      {
        return _applyWhen;
      }
      set
      {
        SetPropertyValue("ApplyWhen", ref _applyWhen, value);
      }
    }
  }
}