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
	public class GetVersionResults
	{
		private string errorMessageField;

		private string modeField;

		private string postalSystemField;

		private string serverNameField;

		private int statusField;

		private string versionField;

		public string ErrorMessage
		{
			get
			{
				return this.errorMessageField;
			}
			set
			{
				this.errorMessageField = value;
			}
		}

		public string Mode
		{
			get
			{
				return this.modeField;
			}
			set
			{
				this.modeField = value;
			}
		}

		public string PostalSystem
		{
			get
			{
				return this.postalSystemField;
			}
			set
			{
				this.postalSystemField = value;
			}
		}

		public string ServerName
		{
			get
			{
				return this.serverNameField;
			}
			set
			{
				this.serverNameField = value;
			}
		}

		public int Status
		{
			get
			{
				return this.statusField;
			}
			set
			{
				this.statusField = value;
			}
		}

		public string Version
		{
			get
			{
				return this.versionField;
			}
			set
			{
				this.versionField = value;
			}
		}

		public GetVersionResults()
		{
		}
	}
}