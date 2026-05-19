// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ProcessShipAcceptCompletedEventArgs
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;


namespace UPS.UPSShip
{
    [GeneratedCode("System.Web.Services", "4.6.1087.0")]
    [DesignerCategory("code")]
    [DebuggerStepThrough]
    public class ProcessShipAcceptCompletedEventArgs : AsyncCompletedEventArgs
    {
      private object[] results;

      internal ProcessShipAcceptCompletedEventArgs(
        object[] results,
        Exception exception,
        bool cancelled,
        object userState)
        : base(exception, cancelled, userState)
      {
        this.results = results;
      }

      public ShipAcceptResponse Result
      {
        get
        {
          this.RaiseExceptionIfNecessary();
          return (ShipAcceptResponse) this.results[0];
        }
      }
    }
}
