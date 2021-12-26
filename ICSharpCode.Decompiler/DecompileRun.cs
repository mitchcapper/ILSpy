using System;
using System.Collections.Generic;
using System.Threading;
using ICSharpCode.Decompiler.CSharp;
using ICSharpCode.Decompiler.CSharp.Syntax;
using ICSharpCode.Decompiler.CSharp.TypeSystem;
using ICSharpCode.Decompiler.TypeSystem;

namespace ICSharpCode.Decompiler
{
	internal class DecompileRun
	{
		public HashSet<string> DefinedSymbols { get; private set; } = new HashSet<string>();
		public HashSet<string> Namespaces { get; private set; } = new HashSet<string>();
		public CancellationToken CancellationToken { get; set; }
		public DecompilerSettings Settings { get; }
		public Dictionary<ITypeDefinition, RecordDecompiler> RecordDecompilers { get; } = new Dictionary<ITypeDefinition, RecordDecompiler>();

		private Lazy<UsingScope> usingScope => new Lazy<UsingScope>(() => CreateUsingScope(Namespaces));
		public UsingScope UsingScope => usingScope.Value;

		public DecompileRun(DecompilerSettings settings)
		{
			this.Settings = settings ?? throw new ArgumentNullException(nameof(settings));
		}

		UsingScope CreateUsingScope(HashSet<string> requiredNamespacesSuperset)
		{
			var usingScope = new UsingScope();
			foreach (var ns in requiredNamespacesSuperset) {
				string[] parts = ns.Split('.');
				AstType nsType = new SimpleType(parts[0]);
				for (int i = 1; i < parts.Length; i++) {
					nsType = new MemberType { Target = nsType, MemberName = parts[i] };
				}

				if (nsType.ToTypeReference(CSharp.Resolver.NameLookupMode.TypeInUsingDeclaration) is TypeOrNamespaceReference reference)
					usingScope.Usings.Add(reference);
			}
			return usingScope;
		}
	}
}
