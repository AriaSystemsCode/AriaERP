using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="www.envmgr.com/LabelService")]
	public class CertifiedIntermediary
	{
		private string accountIDField;

		private string passPhraseField;

		public string AccountID
		{
			get
			{
				return this.accountIDField;
			}
			set
			{
				this.accountIDField = value;
			}
		}

		public string PassPhrase
		{
			get
			{
				return this.passPhraseField;
			}
			set
			{
				this.passPhraseField = value;
			}
		}

		public CertifiedIntermediary()
		{
		}
	}
}