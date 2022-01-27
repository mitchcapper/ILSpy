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
using System.Runtime.InteropServices;
using dnlib.DotNet;
using dnSpy.Contracts.Decompiler;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem.Implementation
{
	/// <summary>
	/// Type definition backed by System.Reflection.Metadata
	/// </summary>
	sealed class MetadataTypeDefinition : ITypeDefinition
	{
		readonly MetadataModule module;
		readonly TypeDef handle;

		// eagerly loaded:
		readonly FullTypeName fullTypeName;
		readonly TypeAttributes attributes;
		public TypeKind Kind { get; }
		public bool IsByRefLike { get; }
		public bool IsReadOnly { get; }
		public ITypeDefinition DeclaringTypeDefinition { get; }
		public IReadOnlyList<ITypeParameter> TypeParameters { get; }
		public KnownTypeCode KnownTypeCode { get; }
		public IType EnumUnderlyingType { get; }
		public bool HasExtensionMethods { get; }
		public Nullability NullableContext { get; }

		// lazy-loaded:
		IMember[] members;
		IField[] fields;
		IProperty[] properties;
		IEvent[] events;
		IMethod[] methods;
		List<IType> directBaseTypes;
		string defaultMemberName;
		private string reflectionName;

		internal MetadataTypeDefinition(MetadataModule module, TypeDef handle)
		{
			Debug.Assert(module != null);
			Debug.Assert(handle != null);
			this.module = module;
			this.handle = handle;
			this.attributes = handle.Attributes;
			this.fullTypeName = handle.GetFullTypeName();
			// Find DeclaringType + KnownTypeCode:
			if (handle.IsNested) {
				this.DeclaringTypeDefinition = module.GetDefinition(handle.DeclaringType);

				// Create type parameters:
				this.TypeParameters = MetadataTypeParameter.Create(module, this.DeclaringTypeDefinition, this, handle.GenericParameters);
				this.NullableContext = handle.CustomAttributes.GetNullableContext() ?? this.DeclaringTypeDefinition.NullableContext;
			} else {
				// Create type parameters:
				this.TypeParameters = MetadataTypeParameter.Create(module, this, handle.GenericParameters);

				this.NullableContext = handle.CustomAttributes.GetNullableContext() ?? module.NullableContext;

				var topLevelTypeName = fullTypeName.TopLevelTypeName;
				for (int i = 0; i < KnownTypeReference.KnownTypeCodeCount; i++) {
					var ktr = KnownTypeReference.Get((KnownTypeCode)i);
					if (ktr != null && ktr.TypeName == topLevelTypeName) {
						this.KnownTypeCode = (KnownTypeCode)i;
						break;
					}
				}
			}
			// Find type kind:
			if (handle.IsInterface) {
				this.Kind = TypeKind.Interface;
			} else if (handle.IsEnum) {
				this.Kind = TypeKind.Enum;
				this.EnumUnderlyingType = module.Compilation.FindType(handle.GetEnumUnderlyingType().ElementType.ToKnownTypeCode());
			} else if (handle.IsValueType) {
				if (KnownTypeCode == KnownTypeCode.Void) {
					this.Kind = TypeKind.Void;
				} else {
					this.Kind = TypeKind.Struct;
					this.IsByRefLike = (module.TypeSystemOptions & TypeSystemOptions.RefStructs) == TypeSystemOptions.RefStructs
									   && handle.CustomAttributes.HasKnownAttribute(KnownAttribute.IsByRefLike);
					this.IsReadOnly = (module.TypeSystemOptions & TypeSystemOptions.ReadOnlyStructsAndParameters) == TypeSystemOptions.ReadOnlyStructsAndParameters
									  && handle.CustomAttributes.HasKnownAttribute(KnownAttribute.IsReadOnly);
				}
			} else if (handle.IsDelegate) {
				this.Kind = TypeKind.Delegate;
			} else {
				this.Kind = TypeKind.Class;
				this.HasExtensionMethods = this.IsStatic &&
										   (module.TypeSystemOptions & TypeSystemOptions.ExtensionMethods) ==
										   TypeSystemOptions.ExtensionMethods &&
										   handle.CustomAttributes.HasKnownAttribute(KnownAttribute.Extension);
			}
		}

		public override string ToString()
		{
			return $"{handle.MDToken.Raw:X8} {fullTypeName}";
		}

		ITypeDefinition[] nestedTypes;

		public IReadOnlyList<ITypeDefinition> NestedTypes {
			get {
				var nestedTypes = LazyInit.VolatileRead(ref this.nestedTypes);
				if (nestedTypes != null)
					return nestedTypes;
				var nestedTypeCollection = handle.NestedTypes;
				var nestedTypeList = new List<ITypeDefinition>(nestedTypeCollection.Count);
				foreach (TypeDef h in nestedTypeCollection) {
					nestedTypeList.Add(module.GetDefinition(h));
				}
				return LazyInit.GetOrSet(ref this.nestedTypes, nestedTypeList.ToArray());
			}
		}

		#region Members
		public IReadOnlyList<IMember> Members {
			get {
				var members = LazyInit.VolatileRead(ref this.members);
				if (members != null)
					return members;
				members = this.Fields.Concat<IMember>(this.Methods).Concat(this.Properties).Concat(this.Events).ToArray();
				return LazyInit.GetOrSet(ref this.members, members);
			}
		}

		public IEnumerable<IField> Fields {
			get {
				var fields = LazyInit.VolatileRead(ref this.fields);
				if (fields != null)
					return fields;
				var fieldCollection = handle.Fields;
				var fieldList = new List<IField>(fieldCollection.Count);
				foreach (FieldDef field in fieldCollection) {
					var attr = field.Attributes;
					if (module.IsVisible(attr)) {
						fieldList.Add(module.GetDefinition(field));
					}
				}
				return LazyInit.GetOrSet(ref this.fields, fieldList.ToArray());
			}
		}

		public IEnumerable<IProperty> Properties {
			get {
				var properties = LazyInit.VolatileRead(ref this.properties);
				if (properties != null)
					return properties;
				var propertyCollection = handle.Properties;
				var propertyList = new List<IProperty>(propertyCollection.Count);
				foreach (PropertyDef property in propertyCollection) {
					bool getterVisible = property.GetMethod != null && module.IsVisible(property.GetMethod.Attributes);
					bool setterVisible = property.SetMethod != null && module.IsVisible(property.SetMethod.Attributes);
					if (getterVisible || setterVisible) {
						propertyList.Add(module.GetDefinition(property));
					}
				}
				return LazyInit.GetOrSet(ref this.properties, propertyList.ToArray());
			}
		}

		public IEnumerable<IEvent> Events {
			get {
				var events = LazyInit.VolatileRead(ref this.events);
				if (events != null)
					return events;
				var eventCollection = handle.Events;
				var eventList = new List<IEvent>(eventCollection.Count);
				foreach (EventDef ev in eventCollection) {
					if (ev.AddMethod == null)
						continue;
					var addMethod = ev.AddMethod;
					if (module.IsVisible(addMethod.Attributes)) {
						eventList.Add(module.GetDefinition(ev));
					}
				}
				return LazyInit.GetOrSet(ref this.events, eventList.ToArray());
			}
		}

		public IEnumerable<IMethod> Methods {
			get {
				var methods = LazyInit.VolatileRead(ref this.methods);
				if (methods != null)
					return methods;
				var methodsCollection = handle.Methods;
				var methodsList = new List<IMethod>(methodsCollection.Count);
				foreach (MethodDef md in methodsCollection) {
					if (md.SemanticsAttributes == MethodSemanticsAttributes.None && module.IsVisible(md.Attributes)) {
						methodsList.Add(module.GetDefinition(md));
					}
				}
				if (this.Kind == TypeKind.Struct || this.Kind == TypeKind.Enum) {
					methodsList.Add(FakeMethod.CreateDummyConstructor(Compilation, this, IsAbstract ? Accessibility.Protected : Accessibility.Public));
				}
				return LazyInit.GetOrSet(ref this.methods, methodsList.ToArray());
			}
		}
		#endregion

		public IType DeclaringType => DeclaringTypeDefinition;

		public bool? IsReferenceType {
			get {
				switch (Kind) {
					case TypeKind.Struct:
					case TypeKind.Enum:
					case TypeKind.Void:
						return false;
					default:
						return true;
				}
			}
		}

		public int TypeParameterCount => TypeParameters.Count;

		IReadOnlyList<IType> IType.TypeArguments => TypeParameters;

		Nullability IType.Nullability => Nullability.Oblivious;

		public IType ChangeNullability(Nullability nullability)
		{
			if (nullability == Nullability.Oblivious)
				return this;
			else
				return new NullabilityAnnotatedType(this, nullability);
		}

		public IEnumerable<IType> DirectBaseTypes {
			get {
				var baseTypes = LazyInit.VolatileRead(ref this.directBaseTypes);
				if (baseTypes != null)
					return baseTypes;
				var context = new GenericContext(TypeParameters);
				var interfaceImplCollection = handle.Interfaces;
				baseTypes = new List<IType>(1 + interfaceImplCollection.Count);
				ITypeDefOrRef baseType = handle.BaseType;
				if (baseType != null) {
					baseTypes.Add(module.ResolveType(baseType, context));
				} else if (Kind == TypeKind.Interface) {
					// td.BaseType.IsNil is always true for interfaces,
					// but the type system expects every interface to derive from System.Object as well.
					baseTypes.Add(Compilation.FindType(KnownTypeCode.Object));
				}
				foreach (var iface in interfaceImplCollection) {
					baseTypes.Add(module.ResolveType(iface.Interface, context, iface, Nullability.Oblivious));
				}
				return LazyInit.GetOrSet(ref this.directBaseTypes, baseTypes);
			}
		}

		dnlib.DotNet.IType IType.MetadataToken => handle;

		public IMDTokenProvider OriginalMember { get; internal set; }

		IMDTokenProvider IEntity.MetadataToken => handle;
		public TypeDef MetadataToken => handle;

		public FullTypeName FullTypeName => fullTypeName;
		public string Name => fullTypeName.Name;

		public IModule ParentModule => module;

		#region Type Attributes
		public IEnumerable<IAttribute> GetAttributes()
		{
			var b = new AttributeListBuilder(module);

			// SpecialName
			if ((handle.Attributes & (TypeAttributes.SpecialName | TypeAttributes.RTSpecialName)) == TypeAttributes.SpecialName)
				b.Add(KnownAttribute.SpecialName);

			b.Add(handle.GetCustomAttributes(), SymbolKind.TypeDefinition);

			return b.Build();
		}

		public string DefaultMemberName {
			get {
				string defaultMemberName = LazyInit.VolatileRead(ref this.defaultMemberName);
				if (defaultMemberName != null)
					return defaultMemberName;
				foreach (var a in handle.CustomAttributes) {
					if (!a.IsKnownAttribute(KnownAttribute.DefaultMember))
						continue;
					if (a.ConstructorArguments.Count == 1 && a.ConstructorArguments[0].Value is UTF8String name) {
						defaultMemberName = name;
						break;
					}
				}
				return LazyInit.GetOrSet(ref this.defaultMemberName, defaultMemberName ?? "Item");
			}
		}
		#endregion

		public Accessibility Accessibility {
			get {
				switch (handle.Visibility) {
					case TypeAttributes.NotPublic:
					case TypeAttributes.NestedAssembly:
						return Accessibility.Internal;
					case TypeAttributes.Public:
					case TypeAttributes.NestedPublic:
						return Accessibility.Public;
					case TypeAttributes.NestedPrivate:
						return Accessibility.Private;
					case TypeAttributes.NestedFamily:
						return Accessibility.Protected;
					case TypeAttributes.NestedFamANDAssem:
						return Accessibility.ProtectedAndInternal;
					case TypeAttributes.NestedFamORAssem:
						return Accessibility.ProtectedOrInternal;
					default:
						return Accessibility.None;
				}
			}
		}

		public bool IsStatic => handle.IsAbstract && handle.IsSealed;
		public bool IsAbstract => handle.IsAbstract;
		public bool IsSealed => handle.IsSealed;

		public SymbolKind SymbolKind => SymbolKind.TypeDefinition;

		public ICompilation Compilation => module.Compilation;

		public string FullName {
			get {
				if (DeclaringType != null)
					return DeclaringType.FullName + "." + Name;
				if (!string.IsNullOrEmpty(this.Namespace))
					return this.Namespace + "." + Name;
				return Name;
			}
		}

		public string ReflectionName {
			get {
				string reflName = LazyInit.VolatileRead(ref this.reflectionName);
				if (reflName != null)
					return reflName;

				var sb = module.Compilation.SharedStringBuilder;
				sb.Length = 0;
				reflName = FullNameFactory.FullName(handle, true, null, sb);

				return LazyInit.GetOrSet(ref this.reflectionName, reflName);
			}
		}
		public string Namespace => handle.Namespace;

		ITypeDefinition IType.GetDefinition() => this;
		TypeParameterSubstitution IType.GetSubstitution() => TypeParameterSubstitution.Identity;

		public IType AcceptVisitor(TypeVisitor visitor)
		{
			return visitor.VisitTypeDefinition(this);
		}

		IType IType.VisitChildren(TypeVisitor visitor)
		{
			return this;
		}

		public override bool Equals(object obj)
		{
			if (obj is MetadataTypeDefinition td) {
				return handle == td.handle && module.PEFile == td.module.PEFile;
			}
			return false;
		}

		public override int GetHashCode()
		{
			return 0x2e0520f2 ^ module.PEFile.GetHashCode() ^ handle.GetHashCode();
		}

		bool IEquatable<IType>.Equals(IType other)
		{
			return Equals(other);
		}

		#region GetNestedTypes
		public IEnumerable<IType> GetNestedTypes(Predicate<ITypeDefinition> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			const GetMemberOptions opt = GetMemberOptions.IgnoreInheritedMembers | GetMemberOptions.ReturnMemberDefinitions;
			if ((options & opt) == opt) {
				return GetFiltered(this.NestedTypes, filter);
			} else {
				return GetMembersHelper.GetNestedTypes(this, filter, options);
			}
		}

		public IEnumerable<IType> GetNestedTypes(IReadOnlyList<IType> typeArguments, Predicate<ITypeDefinition> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			return GetMembersHelper.GetNestedTypes(this, typeArguments, filter, options);
		}
		#endregion

		#region GetMembers()
		IEnumerable<T> GetFiltered<T>(IEnumerable<T> input, Predicate<T> filter) where T : class
		{
			if (filter == null)
				return input;
			else
				return ApplyFilter(input, filter);
		}

		IEnumerable<T> ApplyFilter<T>(IEnumerable<T> input, Predicate<T> filter) where T : class
		{
			foreach (var member in input) {
				if (filter(member))
					yield return member;
			}
		}

		public IEnumerable<IMethod> GetMethods(Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Methods, ExtensionMethods.And(m => !m.IsConstructor, filter));
			} else {
				return GetMembersHelper.GetMethods(this, filter, options);
			}
		}

		public IEnumerable<IMethod> GetMethods(IReadOnlyList<IType> typeArguments, Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			return GetMembersHelper.GetMethods(this, typeArguments, filter, options);
		}

		public IEnumerable<IMethod> GetConstructors(Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.IgnoreInheritedMembers)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if (ComHelper.IsComImport(this)) {
				IType coClass = ComHelper.GetCoClass(this);
				using (var busyLock = BusyManager.Enter(this)) {
					if (busyLock.Success) {
						return coClass.GetConstructors(filter, options)
							.Select(m => new SpecializedMethod(m, m.Substitution) { DeclaringType = this });
					}
				}
				return EmptyList<IMethod>.Instance;
			}
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Methods, ExtensionMethods.And(m => m.IsConstructor && !m.IsStatic, filter));
			} else {
				return GetMembersHelper.GetConstructors(this, filter, options);
			}
		}

		public IEnumerable<IProperty> GetProperties(Predicate<IProperty> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IProperty>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Properties, filter);
			} else {
				return GetMembersHelper.GetProperties(this, filter, options);
			}
		}

		public IEnumerable<IField> GetFields(Predicate<IField> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IField>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Fields, filter);
			} else {
				return GetMembersHelper.GetFields(this, filter, options);
			}
		}

		public IEnumerable<IEvent> GetEvents(Predicate<IEvent> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IEvent>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Events, filter);
			} else {
				return GetMembersHelper.GetEvents(this, filter, options);
			}
		}

		public IEnumerable<IMember> GetMembers(Predicate<IMember> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Members, filter);
			} else {
				return GetMembersHelper.GetMembers(this, filter, options);
			}
		}

		public IEnumerable<IMethod> GetAccessors(Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFilteredAccessors(filter);
			} else {
				return GetMembersHelper.GetAccessors(this, filter, options);
			}
		}

		IEnumerable<IMethod> GetFilteredAccessors(Predicate<IMethod> filter)
		{
			foreach (var prop in this.Properties) {
				var getter = prop.Getter;
				if (getter != null && (filter == null || filter(getter)))
					yield return getter;
				var setter = prop.Setter;
				if (setter != null && (filter == null || filter(setter)))
					yield return setter;
			}
			foreach (var ev in this.Events) {
				var adder = ev.AddAccessor;
				if (adder != null && (filter == null || filter(adder)))
					yield return adder;
				var remover = ev.RemoveAccessor;
				if (remover != null && (filter == null || filter(remover)))
					yield return remover;
				var invoker = ev.InvokeAccessor;
				if (invoker != null && (filter == null || filter(invoker)))
					yield return remover;
			}
		}
		#endregion

		#region IsRecord
		volatile ThreeState isRecord = ThreeState.Unknown;

		public bool IsRecord {
			get {
				if (isRecord == ThreeState.Unknown) {
					isRecord = ComputeIsRecord().ToThreeState();
				}
				return isRecord == ThreeState.True;
			}
		}

		private static readonly UTF8String opEqualityStr = new UTF8String("op_Equality");
		private static readonly UTF8String opInequalityStr = new UTF8String("op_Inequality");
		private static readonly UTF8String cloneStr = new UTF8String("<Clone>$");

		private bool ComputeIsRecord()
		{
			if (Kind != TypeKind.Class)
				return false;
			bool opEquality = false;
			bool opInequality = false;
			bool clone = false;
			foreach (var method in handle.Methods)
			{
				opEquality |= method.Name == opEqualityStr;
				opInequality |= method.Name == opInequalityStr;
				clone |= method.Name == cloneStr;
			}
			return opEquality & opInequality & clone;
		}
		#endregion
	}
}
