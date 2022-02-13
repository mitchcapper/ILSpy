// Copyright (c) 2018 Daniel Grunwald
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

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using dnlib.DotNet;
using dnSpy.Contracts.Decompiler;
using ICSharpCode.Decompiler.Util;
using CallingConvention = dnlib.DotNet.CallingConvention;

namespace ICSharpCode.Decompiler.TypeSystem.Implementation
{
	sealed class MetadataMethod : IMethod
	{
		readonly MetadataModule module;
		readonly MethodDef handle;

		// eagerly loaded fields:
		readonly MethodAttributes attributes;
		readonly SymbolKind symbolKind;
		readonly ITypeParameter[] typeParameters;
		readonly IHasSemantic accessorOwner;
		public MethodSemanticsAttributes AccessorKind { get; }
		public bool IsExtensionMethod { get; }
		bool IMethod.IsLocalFunction => false;

		// lazy-loaded fields:
		ITypeDefinition declaringType;
		string name;
		IParameter[] parameters;
		IMember accessorMember;
		IType returnType;
		volatile ThreeState returnTypeIsRefReadonly = ThreeState.Unknown;
		volatile ThreeState thisIsRefReadonly = ThreeState.Unknown;
		bool isInitOnly;

		internal MetadataMethod(MetadataModule module, MethodDef handle)
		{
			Debug.Assert(module != null);
			Debug.Assert(handle != null);
			this.module = module;
			this.handle = handle;
			this.attributes = handle.Attributes;

			this.symbolKind = SymbolKind.Method;
			const MethodAttributes finalizerAttributes = MethodAttributes.Virtual | MethodAttributes.Family | MethodAttributes.HideBySig;
			this.typeParameters = MetadataTypeParameter.Create(module, this, handle.GenericParameters);
			IHasSemantic owner = handle.SemanticsAttributes != 0 ? FindOwner(handle) : null;
			if (owner != null) {
				this.symbolKind = SymbolKind.Accessor;
				this.accessorOwner = owner;
				this.AccessorKind = handle.SemanticsAttributes;
			}
			else if ((attributes & (MethodAttributes.SpecialName | MethodAttributes.RTSpecialName)) != 0
						 && typeParameters.Length == 0)
			{
				string name = this.Name;
				if (name == ".cctor" || name == ".ctor")
				{
					this.symbolKind = SymbolKind.Constructor;
				}
				else if (name.StartsWith("op_", StringComparison.Ordinal)
						 && CSharp.Syntax.OperatorDeclaration.GetOperatorType(name) != null)
				{
					this.symbolKind = SymbolKind.Operator;
				}
			}
			else if ((attributes & finalizerAttributes) == finalizerAttributes && typeParameters.Length == 0)
			{
				if (Name == "Finalize" && Parameters.Count == 0 && ReturnType.IsKnownType(KnownTypeCode.Void)
					&& (DeclaringTypeDefinition as MetadataTypeDefinition)?.Kind == TypeKind.Class)
				{
					this.symbolKind = SymbolKind.Destructor;
				}
			}
			this.IsExtensionMethod = (attributes & MethodAttributes.Static) == MethodAttributes.Static
									 && (module.TypeSystemOptions & TypeSystemOptions.ExtensionMethods) == TypeSystemOptions.ExtensionMethods
									 && handle.CustomAttributes.HasKnownAttribute(KnownAttribute.Extension);
		}

		private static IHasSemantic FindOwner(MethodDef handle){
			IHasSemantic owner = null;
			var comparer = MethodEqualityComparer.CompareDeclaringTypes;
			if (handle.IsGetter || handle.IsSetter) {
				foreach (var property in handle.DeclaringType.Properties) {
					if (handle.IsGetter && comparer.Equals(property.GetMethod, handle)) {
						owner = property;
						break;
					}
					if (handle.IsSetter && comparer.Equals(property.SetMethod, handle)) {
						owner = property;
						break;
					}
				}
			}

			if (handle.IsAddOn || handle.IsRemoveOn || handle.IsFire) {
				foreach (var eventDef in handle.DeclaringType.Events) {
					if (handle.IsAddOn && comparer.Equals(eventDef.AddMethod, handle)) {
						owner = eventDef;
						break;
					}
					if (handle.IsRemoveOn && comparer.Equals(eventDef.RemoveMethod, handle)) {
						owner = eventDef;
						break;
					}
					if (handle.IsFire && comparer.Equals(eventDef.InvokeMethod, handle)) {
						owner = eventDef;
						break;
					}
				}
			}

			return owner;
		}

		IMDTokenProvider IEntity.MetadataToken => handle;

		public IMDTokenProvider OriginalMember => handle;

		public dnlib.DotNet.IMethod MetadataToken => handle;

		public override string ToString()
		{
			return $"{handle.MDToken.Raw:X8} {DeclaringType?.ReflectionName}.{Name}";
		}

		public string Name {
			get {
				string name = LazyInit.VolatileRead(ref this.name);
				if (name != null)
					return name;
				return LazyInit.GetOrSet(ref this.name, handle.Name.String);
			}
		}

		public IReadOnlyList<ITypeParameter> TypeParameters => typeParameters;
		IReadOnlyList<IType> IMethod.TypeArguments => typeParameters;

		public SymbolKind SymbolKind => symbolKind;
		public bool IsConstructor => symbolKind == SymbolKind.Constructor;
		public bool IsDestructor => symbolKind == SymbolKind.Destructor;
		public bool IsOperator => symbolKind == SymbolKind.Operator;
		public bool IsAccessor => symbolKind == SymbolKind.Accessor;
		public bool HasBody => handle.HasBody;

		public IMember AccessorOwner {
			get {
				if (accessorOwner is null)
					return null;

				IMember accessorMember = LazyInit.VolatileRead(ref this.accessorMember);
				if (accessorMember != null)
					return accessorMember;

				if (accessorOwner is PropertyDef propertyDef)
					accessorMember = module.GetDefinition(propertyDef);
				else if (accessorOwner is EventDef eventDef)
					accessorMember = module.GetDefinition(eventDef);

				return LazyInit.GetOrSet(ref this.accessorMember, accessorMember);
			}
		}

		#region Signature (ReturnType + Parameters)
		public IReadOnlyList<IParameter> Parameters {
			get {
				var parameters = LazyInit.VolatileRead(ref this.parameters);
				if (parameters != null)
					return parameters;

				var genericContext = new GenericContext(DeclaringType.TypeParameters, this.TypeParameters);
				List<IParameter> param = new List<IParameter>();

				var entityOptions = module.OptionsForEntity(this);

				foreach (Parameter par in handle.Parameters) {
					if (par.IsNormalMethodParameter) {
						var parameterType = module.ResolveType(par.Type, genericContext, entityOptions, par.ParamDef, NullableContext);
						param.Add(new MetadataParameter(module, this, parameterType, par));
					}
				}

				if (handle.CallingConvention == CallingConvention.VarArg) {
					param.Add(new DefaultParameter(SpecialType.ArgList, name: string.Empty, this));
				}

				return LazyInit.GetOrSet(ref this.parameters, param.ToArray());
			}
		}

		public IType ReturnType {
			get {
				var returnType = LazyInit.VolatileRead(ref this.returnType);
				if (returnType != null)
					return returnType;

				var genericContext = new GenericContext(DeclaringType.TypeParameters, this.TypeParameters);
				var sig = handle.ReturnType.DecodeSignature(module, genericContext);

				isInitOnly = sig is ModifiedType {
					Modifier: { Name: "IsExternalInit", Namespace: "System.Runtime.CompilerServices" }
				};

				var retType = ApplyAttributeTypeVisitor.ApplyAttributesToType(sig,
					module.Compilation, handle.Parameters.ReturnParameter.ParamDef, module.OptionsForEntity(this), NullableContext, isSignatureReturnType: true);

				return LazyInit.GetOrSet(ref this.returnType, retType);
			}
		}

		public bool IsInitOnly {
			get {
				_ = ReturnType;

				return this.isInitOnly;
			}
		}

		internal Nullability NullableContext {
			get {
				return handle.CustomAttributes.GetNullableContext() ?? DeclaringTypeDefinition.NullableContext;
			}
		}
		#endregion

		public bool IsExplicitInterfaceImplementation {
			get {
				if (Name.IndexOf('.') < 0)
					return false;
				return handle.HasOverrides;
			}
		}

		public IEnumerable<IMember> ExplicitlyImplementedInterfaceMembers {
			get {
				foreach (MethodOverride handleOverride in handle.Overrides) {
					yield return module.ResolveMethod(handleOverride.MethodDeclaration,
						new GenericContext(this.DeclaringType.TypeParameters));
				}
			}
		}

		IMember IMember.MemberDefinition => this;
		IMethod IMethod.ReducedFrom => null;
		TypeParameterSubstitution IMember.Substitution => TypeParameterSubstitution.Identity;

		public ITypeDefinition DeclaringTypeDefinition {
			get {
				var declType = LazyInit.VolatileRead(ref this.declaringType);
				if (declType != null) {
					return declType;
				} else {
					return LazyInit.GetOrSet(ref this.declaringType,
						module.GetDefinition(handle.DeclaringType));
				}
			}
		}

		public IType DeclaringType => DeclaringTypeDefinition;

		public IModule ParentModule => module;
		public ICompilation Compilation => module.Compilation;

		#region Attributes
		IType FindInteropType(string name)
		{
			return module.Compilation.FindType(new TopLevelTypeName(
				"System.Runtime.InteropServices", name
			));
		}

		public IEnumerable<IAttribute> GetAttributes()
		{
			var b = new AttributeListBuilder(module);

			// SpecialName
			if ((handle.Attributes & (MethodAttributes.SpecialName | MethodAttributes.RTSpecialName)) == MethodAttributes.SpecialName
				&& SymbolKind == SymbolKind.Method)
			{
				b.Add(KnownAttribute.SpecialName);
			}

			b.Add(handle.GetCustomAttributes(), symbolKind);

			return b.Build();
		}
		#endregion

		#region Return type attributes
		public IEnumerable<IAttribute> GetReturnTypeAttributes()
		{
			var b = new AttributeListBuilder(module);
			if (handle.Parameters.ReturnParameter.HasParamDef) {
				b.AddMarshalInfo(handle.Parameters.ReturnParameter.ParamDef.MarshalType);
				b.Add(handle.Parameters.ReturnParameter.ParamDef.CustomAttributes, SymbolKind.ReturnType);
			}
			return b.Build();
		}

		public bool ReturnTypeIsRefReadOnly {
			get {
				if (returnTypeIsRefReadonly != ThreeState.Unknown) {
					return returnTypeIsRefReadonly == ThreeState.True;
				}
				bool hasReadOnlyAttr = false;
				if (handle.Parameters.ReturnParameter.HasParamDef) {
					hasReadOnlyAttr = handle.Parameters.ReturnParameter.ParamDef.CustomAttributes.HasKnownAttribute(KnownAttribute.IsReadOnly);
				}
				this.returnTypeIsRefReadonly = hasReadOnlyAttr.ToThreeState();
				return hasReadOnlyAttr;
			}
		}

		public bool ThisIsRefReadOnly {
			get {
				if (thisIsRefReadonly != ThreeState.Unknown) {
					return thisIsRefReadonly == ThreeState.True;
				}
				bool hasReadOnlyAttr = DeclaringTypeDefinition?.IsReadOnly ?? false;
				if ((module.TypeSystemOptions & TypeSystemOptions.ReadOnlyMethods) != 0) {
					hasReadOnlyAttr |= handle.CustomAttributes.HasKnownAttribute(KnownAttribute.IsReadOnly);
				}
				this.thisIsRefReadonly = hasReadOnlyAttr.ToThreeState();
				return hasReadOnlyAttr;
			}
		}

		#endregion

		public Accessibility Accessibility => GetAccessibility(attributes);

		internal static Accessibility GetAccessibility(MethodAttributes attr)
		{
			switch (attr & MethodAttributes.MemberAccessMask) {
				case MethodAttributes.Public:
					return Accessibility.Public;
				case MethodAttributes.Assembly:
					return Accessibility.Internal;
				case MethodAttributes.Private:
					return Accessibility.Private;
				case MethodAttributes.Family:
					return Accessibility.Protected;
				case MethodAttributes.FamANDAssem:
					return Accessibility.ProtectedAndInternal;
				case MethodAttributes.FamORAssem:
					return Accessibility.ProtectedOrInternal;
				default:
					return Accessibility.None;
			}
		}

		public bool IsStatic => (attributes & MethodAttributes.Static) != 0;
		public bool IsAbstract => (attributes & MethodAttributes.Abstract) != 0;
		public bool IsSealed => (attributes & (MethodAttributes.Abstract | MethodAttributes.Final | MethodAttributes.NewSlot | MethodAttributes.Static)) == MethodAttributes.Final;
		public bool IsVirtual => (attributes & (MethodAttributes.Abstract | MethodAttributes.Virtual | MethodAttributes.NewSlot | MethodAttributes.Final)) == (MethodAttributes.Virtual | MethodAttributes.NewSlot);
		public bool IsOverride => (attributes & (MethodAttributes.NewSlot | MethodAttributes.Virtual)) == MethodAttributes.Virtual;
		public bool IsOverridable
			=> (attributes & (MethodAttributes.Abstract | MethodAttributes.Virtual)) != 0
			&& (attributes & MethodAttributes.Final) == 0;

		public string FullName => $"{DeclaringType?.FullName}.{Name}";
		public string ReflectionName => $"{DeclaringType?.ReflectionName}.{Name}";
		public string Namespace => DeclaringType?.Namespace ?? string.Empty;

		public override bool Equals(object obj)
		{
			if (obj is not MetadataMethod m)
				m = (obj as MetadataMethodWithOriginalMember)?.backing;
			if (m is not null)
				return handle == m.handle && module.PEFile == m.module.PEFile;
			return false;
		}

		public override int GetHashCode()
		{
			return 0x5a00d671 ^ module.PEFile.GetHashCode() ^ handle.GetHashCode();
		}

		bool IMember.Equals(IMember obj, TypeVisitor typeNormalization)
		{
			return Equals(obj);
		}

		public IMethod Specialize(TypeParameterSubstitution substitution)
		{
			return SpecializedMethod.Create(this, substitution);
		}

		IMember IMember.Specialize(TypeParameterSubstitution substitution)
		{
			return SpecializedMethod.Create(this, substitution);
		}

		internal IMethod WithOriginalMember(IMDTokenProvider originalMember)
		{
			return new MetadataMethodWithOriginalMember(this, originalMember);
		}

		private sealed class MetadataMethodWithOriginalMember : IMethod
		{
			internal readonly MetadataMethod backing;

			public MetadataMethodWithOriginalMember(MetadataMethod mdMethod, IMDTokenProvider originalMember)
			{
				this.backing = mdMethod;
				this.OriginalMember = originalMember;
			}

			public SymbolKind SymbolKind => backing.SymbolKind;

			public string FullName => backing.FullName;

			public IMethod Specialize(TypeParameterSubstitution substitution)
			{
				return SpecializedMethod.Create(this, substitution);
			}

			public dnlib.DotNet.IMethod MetadataToken => backing.MetadataToken;

			IMDTokenProvider IEntity.MetadataToken => backing.MetadataToken;

			public IMDTokenProvider OriginalMember { get; }

			public string Name => backing.Name;

			public ITypeDefinition DeclaringTypeDefinition => backing.DeclaringTypeDefinition;

			public IMember MemberDefinition => this;

			public IType ReturnType => backing.ReturnType;

			public IType DeclaringType => backing.DeclaringType;

			public IEnumerable<IMember> ExplicitlyImplementedInterfaceMembers => backing.ExplicitlyImplementedInterfaceMembers;

			public bool IsExplicitInterfaceImplementation => backing.IsExplicitInterfaceImplementation;

			public bool IsVirtual => backing.IsVirtual;

			public bool IsOverride => backing.IsOverride;

			public bool IsOverridable => backing.IsOverridable;

			public TypeParameterSubstitution Substitution => TypeParameterSubstitution.Identity;

			public IEnumerable<IAttribute> GetReturnTypeAttributes()
			{
				return backing.GetReturnTypeAttributes();
			}

			public bool ReturnTypeIsRefReadOnly => backing.ReturnTypeIsRefReadOnly;

			public bool IsInitOnly => backing.IsInitOnly;

			public bool ThisIsRefReadOnly => backing.ThisIsRefReadOnly;

			public IReadOnlyList<ITypeParameter> TypeParameters => backing.TypeParameters;

			public IReadOnlyList<IType> TypeArguments => backing.TypeParameters;

			public bool IsExtensionMethod => backing.IsExtensionMethod;

			public bool IsLocalFunction => false;

			public bool IsConstructor => backing.IsConstructor;

			public bool IsDestructor => backing.IsDestructor;

			public bool IsOperator => backing.IsOperator;

			public bool HasBody => backing.HasBody;

			public bool IsAccessor => backing.IsAccessor;

			public IMember AccessorOwner => backing.AccessorOwner;

			public MethodSemanticsAttributes AccessorKind => backing.AccessorKind;

			public IMethod ReducedFrom => null;

			IMember IMember.Specialize(TypeParameterSubstitution substitution)
			{
				return SpecializedMethod.Create(this, substitution);
			}

			public override bool Equals(object obj)
			{
				if (obj is not MetadataMethod f)
					f = (obj as MetadataMethodWithOriginalMember)?.backing;
				if (f is not null)
					return backing.handle == f.handle && backing.module.PEFile == f.module.PEFile;
				return false;
			}

			public override int GetHashCode()
			{
				return 0x11dda32b ^ backing.module.PEFile.GetHashCode() ^ backing.handle.GetHashCode();
			}

			public bool Equals(IMember obj, TypeVisitor typeNormalization)
			{
				return Equals(obj);
			}

			public IModule ParentModule => backing.ParentModule;

			public IEnumerable<IAttribute> GetAttributes()
			{
				return backing.GetAttributes();
			}

			public Accessibility Accessibility => backing.Accessibility;

			public bool IsStatic => backing.IsStatic;

			public bool IsAbstract => backing.IsAbstract;

			public bool IsSealed => backing.IsSealed;

			public string ReflectionName => backing.ReflectionName;

			public string Namespace => backing.Namespace;

			public ICompilation Compilation => backing.Compilation;

			public IReadOnlyList<IParameter> Parameters => backing.Parameters;
		}
	}
}
