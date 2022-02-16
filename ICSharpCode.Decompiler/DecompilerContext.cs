using System.Collections.Generic;
using System.Threading;

using dnlib.DotNet;

using dnSpy.Contracts.Decompiler;

using ICSharpCode.Decompiler.CSharp;

namespace ICSharpCode.Decompiler
{
	public class DecompilerContext
	{
		public DecompilerContext(int settingsVersion, ModuleDef currentModule, MetadataTextColorProvider metadataTextColorProvider = null)
			: this(settingsVersion, currentModule, metadataTextColorProvider, false)
		{
		}

		public DecompilerContext(int settingsVersion, ModuleDef currentModule, MetadataTextColorProvider metadataTextColorProvider, bool calculateILSpans)
		{
			this.Settings = new DecompilerSettings(LanguageVersion.CSharp9_0);
			this.UsingNamespaces = new List<string>();
			this.SettingsVersion = settingsVersion;
			this.CurrentModule = currentModule;
			this.CalculateILSpans = calculateILSpans;
			this.MetadataTextColorProvider = metadataTextColorProvider ?? CSharpMetadataTextColorProvider.Instance;
		}

		private DecompilerContext(DecompilerContext other)
		{
			this.UsingNamespaces = new List<string>();
			this.MetadataTextColorProvider = other.MetadataTextColorProvider;
			this.CurrentModule = other.CurrentModule;
			this.CancellationToken = other.CancellationToken;
			this.CurrentType = other.CurrentType;
			this.CurrentMethod = other.CurrentMethod;
			this.Settings = other.Settings.Clone();
			this.SettingsVersion = other.SettingsVersion;
			this.CurrentMethodIsAsync = other.CurrentMethodIsAsync;
			this.CurrentMethodIsYieldReturn = other.CurrentMethodIsYieldReturn;
			this.CalculateILSpans = other.CalculateILSpans;
			this.AsyncMethodBodyDecompilation = other.AsyncMethodBodyDecompilation;
			this.UsingNamespaces.AddRange(other.UsingNamespaces);
		}

		internal DecompilerContext CloneDontUse()
		{
			DecompilerContext decompilerContext = (DecompilerContext)base.MemberwiseClone();
			return decompilerContext;
		}

		internal DecompilerContext Clone()
		{
			return new DecompilerContext(this);
		}

		public void Reset()
		{
			this.CurrentModule = null;
			this.CancellationToken = CancellationToken.None;
			this.CurrentType = null;
			this.CurrentMethod = null;
			this.Settings = new DecompilerSettings();
			this.CurrentMethodIsAsync = false;
			this.CurrentMethodIsYieldReturn = false;
			this.UsingNamespaces.Clear();
		}

		public MetadataTextColorProvider MetadataTextColorProvider;
		public ModuleDef CurrentModule;
		public CancellationToken CancellationToken;
		public TypeDef CurrentType;
		public MethodDef CurrentMethod;
		public DecompilerSettings Settings;
		public readonly int SettingsVersion;
		public bool CurrentMethodIsAsync;
		public bool CurrentMethodIsYieldReturn;
		public bool CalculateILSpans;
		public bool AsyncMethodBodyDecompilation;
		public readonly List<string> UsingNamespaces;
	}
}
