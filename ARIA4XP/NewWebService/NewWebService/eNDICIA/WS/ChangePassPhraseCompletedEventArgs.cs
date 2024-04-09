using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	public class ChangePassPhraseCompletedEventArgs : AsyncCompletedEventArgs
	{
		private object[] results;

		public ChangePassPhraseRequestResponse Result
		{
			get
			{
				base.RaiseExceptionIfNecessary();
				return (ChangePassPhraseRequestResponse)this.results[0];
			}
		}

		internal ChangePassPhraseCompletedEventArgs(object[] results, Exception exception, bool cancelled, object userState) : base(exception, cancelled, userState)
		{
			this.results = results;
		}
	}
}