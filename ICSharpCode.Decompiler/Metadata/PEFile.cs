// Copyright (c) 2018 Siegfried Pammer
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#nullable enable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;

using dnlib.DotNet;

using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.Metadata
{
	/// <summary>
	/// PEFile is the main class the decompiler uses to represent a metadata assembly/module.
	/// Every file on disk can be loaded into a standalone PEFile instance.
	///
	/// A PEFile can be combined with its referenced assemblies/modules to form a type system,
	/// in that case the <see cref="MetadataModule"/> class is used instead.
	/// </summary>
	/// <remarks>
	/// In addition to wrapping a <c>System.Reflection.Metadata.PEReader</c>, this class
	/// contains a few decompiler-specific caches to allow efficiently constructing a type
	/// system from multiple PEFiles. This allows the caches to be shared across multiple
	/// decompiled type systems.
	/// </remarks>
	public class PEFile : IDisposable, TypeSystem.IModuleReference
	{
		public ModuleDef Module { get; }

		public PEFile(string fileName)
			: this(new FileStream(fileName, FileMode.Open, FileAccess.Read))
		{
		}

		public PEFile(Stream stream)
			: this(ModuleDefMD.Load(stream))
		{
		}

		public PEFile(ModuleDef reader)
		{
			this.Module = reader ?? throw new ArgumentNullException(nameof(reader));
			this.Module.EnableTypeDefFindCache = true;
		}

		string? name;

		public string Name {
			get {
				return Module.Name;
			}
		}

		string? fullName;

		public string FullName {
			get {
				return Module.FullName;
			}
		}

		public TargetRuntime GetRuntime()
		{
			string version = Module.RuntimeVersion;
			if (version == null || version.Length <= 1)
				return TargetRuntime.Unknown;
			switch (version[1])
			{
				case '1':
					if (version.Length <= 3)
						return TargetRuntime.Unknown;
					if (version[3] == 1)
						return TargetRuntime.Net_1_0;
					else
						return TargetRuntime.Net_1_1;
				case '2':
					return TargetRuntime.Net_2_0;
				case '4':
					return TargetRuntime.Net_4_0;
				default:
					return TargetRuntime.Unknown;
			}
		}

		public void Dispose()
		{
			Module.Dispose();
		}

		/// <summary>
		/// Finds the top-level-type with the specified name.
		/// </summary>
		public TypeDef GetTypeDefinition(TopLevelTypeName typeName)
		{
			return Module.Find(typeName.ReflectionName, true);
		}

		Dictionary<FullTypeName, ExportedType> typeForwarderLookup;

		/// <summary>
		/// Finds the type forwarder with the specified name.
		/// </summary>
		public ExportedType GetTypeForwarder(FullTypeName typeName)
		{
			var lookup = LazyInit.VolatileRead(ref typeForwarderLookup);
			if (lookup == null) {
				lookup = new Dictionary<FullTypeName, ExportedType>(Module.ExportedTypes.Count);
				foreach (var handle in Module.ExportedTypes) {
					lookup[handle.GetFullTypeName()] = handle;
				}
				lookup = LazyInit.GetOrSet(ref typeForwarderLookup, lookup);
			}
			if (lookup.TryGetValue(typeName, out var resultHandle))
				return resultHandle;
			else
				return default;
		}

		public TypeSystem.IModuleReference WithOptions(TypeSystemOptions options)
		{
			return new PEFileWithOptions(this, options);
		}

		TypeSystem.IModule TypeSystem.IModuleReference.Resolve(ITypeResolveContext context)
		{
			return new MetadataModule(context.Compilation, this, TypeSystemOptions.Default);
		}

		private class PEFileWithOptions : TypeSystem.IModuleReference
		{
			readonly PEFile peFile;
			readonly TypeSystemOptions options;

			public PEFileWithOptions(PEFile peFile, TypeSystemOptions options)
			{
				this.peFile = peFile;
				this.options = options;
			}

			TypeSystem.IModule TypeSystem.IModuleReference.Resolve(ITypeResolveContext context)
			{
				return new MetadataModule(context.Compilation, peFile, options);
			}
		}
	}
}
