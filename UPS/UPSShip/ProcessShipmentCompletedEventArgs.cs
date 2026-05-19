// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ProcessShipmentCompletedEventArgs
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;


namespace UPS.UPSShip
{
    [DesignerCategory("code")]
    [DebuggerStepThrough]
    [GeneratedCode("System.Web.Services", "4.6.1087.0")]
    public class ProcessShipmentCompletedEventArgs : AsyncCompletedEventArgs
    {
      private object[] results;

      internal ProcessShipmentCompletedEventArgs(
        object[] results,
        Exception exception,
        bool cancelled,
        object userState)
        : base(exception, cancelled, userState)
      {
        this.results = results;
      }

      public ShipmentResponse Result
      {
        get
        {
          this.RaiseExceptionIfNecessary();
          return (ShipmentResponse) this.results[0];
        }
      }
    }
}
