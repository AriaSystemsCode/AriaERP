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
using ns1 =DevExpress.Persistent.BaseImpl;
using System.ComponentModel;
using ns2 =DevExpress.Xpo;
using DevExpress.Xpo ; 
namespace Aria5.DevExpress.MainSystem.Module.BusinessObjects
{
  //[DefaultClassOptions]
  [DefaultProperty("Description")]
    public partial class IdentifierSegment : ns1.BaseObject
  {
    private Aria5.DevExpress.MainSystem.Module.BusinessObjects.ValueTypes _valueType;
    private Direction _matrixDirection;
    private System.Boolean _alllowMultipleValuesRow;
    private System.String _valueFilter;
    private System.String _attributeName;
    private System.String _tableName;
    private System.Int64 _nextIdentity;
    private System.Int16 _identityIncrement;
    private System.Int64 _identitySeed;
    private System.String _format;
    private System.String _inputMask;
    private System.Int16 _dimension;
    private System.Char? _separatorAfter;
    private System.Int16 _length;
    private System.String _caption;
    private System.String _description;
    private System.Int16 _position;
    private Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierStructure _identifierStructure;
    public IdentifierSegment(ns2.Session  session)
      : base(session)
    {
    }
    [Association("Identifier-Segments")]
    public Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierStructure IdentifierStructure
    {
      get
      {
        return _identifierStructure;
      }
      set
      {
        SetPropertyValue("IdentifierStructure", ref _identifierStructure, value);
      }
    }
    public System.Int16 Position
    {
      get
      {
        return _position;
      }
      set
      {
        SetPropertyValue("Position", ref _position, value);
      }
    }
    [ns2.SizeAttribute(50)]
    public System.String Description
    {
      get
      {
        return _description;
      }
      set
      {
        SetPropertyValue("Description", ref _description, value);
      }
    }
    [ns2.SizeAttribute(50)]
    public System.String Caption
    {
      get
      {
        return _caption;
      }
      set
      {
        SetPropertyValue("Caption", ref _caption, value);
      }
    }
    public System.Int16 Length
    {
      get
      {
        return _length;
      }
      set
      {
        SetPropertyValue("Length", ref _length, value);
      }
    }
    [ns2.SizeAttribute(1)]
    public System.Char? SeparatorAfter
    {
      get
      {
        return _separatorAfter;
      }
      set
      {
        SetPropertyValue("SeparatorAfter", ref _separatorAfter, value);
      }
    }
    public System.Int16 Dimension
    {
      get
      {
        return _dimension;
      }
      set
      {
        SetPropertyValue("Dimension", ref _dimension, value);
      }
    }
    [ns2.SizeAttribute(20)]
    public Aria5.DevExpress.MainSystem.Module.BusinessObjects.ValueTypes ValueType
    {
      get
      {
        return _valueType;
      }
      set
      {
        SetPropertyValue("ValueType", ref _valueType, value);
      }
    }
    [ns2.SizeAttribute(30)]
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
    [ns2.SizeAttribute(30)]
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
    public System.Int64  NextIdentity
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
    public System.String TableName
    {
      get
      {
        return _tableName;
      }
      set
      {
        SetPropertyValue("TableName", ref _tableName, value);
      }
    }
    public System.String AttributeName
    {
      get
      {
        return _attributeName;
      }
      set
      {
        SetPropertyValue("AttributeName", ref _attributeName, value);
      }
    }
    [SizeAttribute(-1)]
    public System.String ValueFilter
    {
      get
      {
        return _valueFilter;
      }
      set
      {
        SetPropertyValue("ValueFilter", ref _valueFilter, value);
      }
    }
    [SizeAttribute(1)]
    public System.Boolean AlllowMultipleValuesRow
    {
      get
      {
        return _alllowMultipleValuesRow;
      }
      set
      {
        SetPropertyValue("AlllowMultipleValuesRow", ref _alllowMultipleValuesRow, value);
      }
    }
    [SizeAttribute(10)]
    public Direction MatrixDirection
    {
      get
      {
        return _matrixDirection;
      }
      set
      {
        SetPropertyValue("MatrixDirection", ref _matrixDirection, value);
      }
    }
    [AssociationAttribute("Segments-Formats")]
    public ns2.XPCollection<IdentifierSegmentFormat> IdentifierSegmentFormat
    {
      get
      {
        return GetCollection<IdentifierSegmentFormat>("IdentifierSegmentFormat");
      }
    }
    [Association("Segments-ValidEntries")]
    public ns2.XPCollection<IdentifierSegmentValidEntry> SegmentsValidEntry
    {
      get
      {
        return GetCollection<IdentifierSegmentValidEntry>("SegmentsValidEntry");
      }
    }
  }
  public enum ValueTypes
  {
   Select = 0,
    ManuallyEntered=1,
    Identity=2,
    FixedValue=3,
    ValidEntries=4,
    EntityValue=5,
    Expression=6,
    CurrentYearValue=7
  }
  public enum Direction
  {
   Select = 0,
      Right=1,
    Left=2
  }
}