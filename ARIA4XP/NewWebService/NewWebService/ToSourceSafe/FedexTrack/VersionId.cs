using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public class VersionId
	{
		private string serviceIdField;

		private int majorField;

		private int intermediateField;

		private int minorField;

		public int Intermediate
		{
			get
			{
				return this.intermediateField;
			}
			set
			{
				this.intermediateField = value;
			}
		}

		public int Major
		{
			get
			{
				return this.majorField;
			}
			set
			{
				this.majorField = value;
			}
		}

		public int Minor
		{
			get
			{
				return this.minorField;
			}
			set
			{
				this.minorField = value;
			}
		}

		public string ServiceId
		{
			get
			{
				return this.serviceIdField;
			}
			set
			{
				this.serviceIdField = value;
			}
		}

		public VersionId()
		{
			this.serviceIdField = "trck";
			this.majorField = 4;
			this.intermediateField = 1;
			this.minorField = 0;
		}
	}
}