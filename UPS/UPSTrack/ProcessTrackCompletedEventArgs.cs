// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.ProcessTrackCompletedEventArgs
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;


namespace UPS.UPSTrack
{
    [DesignerCategory("code")]
    [GeneratedCode("System.Web.Services", "4.6.1087.0")]
    [DebuggerStepThrough]
    public class ProcessTrackCompletedEventArgs : AsyncCompletedEventArgs
    {
      private object[] results;

      internal ProcessTrackCompletedEventArgs(
        object[] results,
        Exception exception,
        bool cancelled,
        object userState)
        : base(exception, cancelled, userState)
      {
        this.results = results;
      }

      public TrackResponse Result
      {
        get
        {
          this.RaiseExceptionIfNecessary();
          return (TrackResponse) this.results[0];
        }
      }
    }
}
