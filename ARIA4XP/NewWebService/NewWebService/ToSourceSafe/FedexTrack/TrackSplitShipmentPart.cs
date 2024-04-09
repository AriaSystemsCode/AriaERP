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
	public class TrackSplitShipmentPart
	{
		private int pieceCountField;

		private bool pieceCountFieldSpecified;

		private DateTime timestampField;

		private bool timestampFieldSpecified;

		private string statusCodeField;

		private string statusDescriptionField;

		public int PieceCount
		{
			get
			{
				return this.pieceCountField;
			}
			set
			{
				this.pieceCountField = value;
			}
		}

		[XmlIgnore]
		public bool PieceCountSpecified
		{
			get
			{
				return this.pieceCountFieldSpecified;
			}
			set
			{
				this.pieceCountFieldSpecified = value;
			}
		}

		public string StatusCode
		{
			get
			{
				return this.statusCodeField;
			}
			set
			{
				this.statusCodeField = value;
			}
		}

		public string StatusDescription
		{
			get
			{
				return this.statusDescriptionField;
			}
			set
			{
				this.statusDescriptionField = value;
			}
		}

		public DateTime Timestamp
		{
			get
			{
				return this.timestampField;
			}
			set
			{
				this.timestampField = value;
			}
		}

		[XmlIgnore]
		public bool TimestampSpecified
		{
			get
			{
				return this.timestampFieldSpecified;
			}
			set
			{
				this.timestampFieldSpecified = value;
			}
		}

		public TrackSplitShipmentPart()
		{
		}
	}
}