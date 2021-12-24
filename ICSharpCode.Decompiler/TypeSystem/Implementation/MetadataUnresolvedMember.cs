using System;
using System.Collections.Generic;
using dnlib.DotNet;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem.Implementation
{
	abstract class MetadataUnresolvedMember : IMember
	{
		protected readonly MetadataModule module;
		protected readonly MemberRef dnlibRef;

		protected MetadataUnresolvedMember(MetadataModule module, MemberRef dnlibRef)
		{
			this.module = module ?? throw new ArgumentNullException(nameof(module));
			this.dnlibRef = dnlibRef ?? throw new ArgumentNullException(nameof(dnlibRef));
		}

		IMember IMember.MemberDefinition => this;

		public abstract IType ReturnType { get; }

		IEnumerable<IMember> IMember.ExplicitlyImplementedInterfaceMembers => EmptyList<IMember>.Instance;

		bool IMember.IsExplicitInterfaceImplementation => false;

		bool IMember.IsVirtual => false;
		bool IMember.IsOverride => false;
		bool IMember.IsOverridable => false;

		TypeParameterSubstitution IMember.Substitution => TypeParameterSubstitution.Identity;

		IMDTokenProvider IEntity.MetadataToken => dnlibRef;

		public string Name => dnlibRef.Name;

		ITypeDefinition IEntity.DeclaringTypeDefinition => DeclaringType?.GetDefinition();

		public IType DeclaringType { get; set; }

		IModule IEntity.ParentModule => DeclaringType?.GetDefinition()?.ParentModule;

		IEnumerable<IAttribute> IEntity.GetAttributes() => EmptyList<IAttribute>.Instance;

		public Accessibility Accessibility { get; set; } = Accessibility.Public;

		public bool IsStatic { get; set; }
		bool IEntity.IsAbstract => false;
		bool IEntity.IsSealed => false;

		public abstract SymbolKind SymbolKind { get; }

		ICompilation ICompilationProvider.Compilation => module.Compilation;

		string INamedElement.FullName {
			get {
				if (DeclaringType != null)
					return DeclaringType.FullName + "." + Name;
				else
					return Name;
			}
		}

		string INamedElement.ReflectionName {
			get {
				if (DeclaringType != null)
					return DeclaringType.ReflectionName + "." + Name;
				else
					return Name;
			}
		}

		string INamedElement.Namespace => DeclaringType?.Namespace;

		bool IMember.Equals(IMember obj, TypeVisitor typeNormalization)
		{
			return Equals(obj);
		}

		public abstract IMember Specialize(TypeParameterSubstitution substitution);
	}

	class MetadataUnresolvedField : MetadataUnresolvedMember, IField
	{
		private IType retType;

		public MetadataUnresolvedField(MetadataModule compilation, MemberRef dnlibRef)
			: base(compilation, dnlibRef) { }

		bool IField.IsReadOnly => false;
		public bool IsVolatile { get; private set; }

		bool IVariable.IsConst => false;
		object IVariable.GetConstantValue(bool throwOnInvalidMetadata) => null;
		IType IVariable.Type => ReturnType;

		public override SymbolKind SymbolKind => SymbolKind.Field;

		public dnlib.DotNet.IField MetadataToken => dnlibRef;

		public override IType ReturnType {
			get {
				var t = LazyInit.VolatileRead(ref retType);
				if (t is not null)
					return t;

				t = dnlibRef.FieldSig.Type.DecodeSignature(module, new GenericContext(DeclaringType?.GetDefinition()?.TypeParameters));

				if (t is ModifiedType modifier && modifier.Modifier.Name == "IsVolatile" &&
					modifier.Modifier.Namespace == "System.Runtime.CompilerServices") {
					IsVolatile = true;
					t = modifier.ElementType;
				}

				return LazyInit.GetOrSet(ref retType, t);
			}
		}

		public override IMember Specialize(TypeParameterSubstitution substitution)
		{
			return SpecializedField.Create(this, substitution);
		}

		IField IField.Specialize(TypeParameterSubstitution substitution)
		{
			return SpecializedField.Create(this, substitution);
		}
	}

	class MetadataUnresolvedMethod : MetadataUnresolvedMember, IMethod
	{
		private IType retType;
		private readonly SymbolKind symbolKind;

		public MetadataUnresolvedMethod(MetadataModule compilation, MemberRef dnlibref, SymbolKind symbolKind) : base(compilation, dnlibref)
		{
			this.symbolKind = symbolKind;
		}

		public override SymbolKind SymbolKind => symbolKind;

		IEnumerable<IAttribute> IMethod.GetReturnTypeAttributes() => EmptyList<IAttribute>.Instance;
		bool IMethod.ReturnTypeIsRefReadOnly => false;
		bool IMethod.ThisIsRefReadOnly => false;
		public bool IsInitOnly { get; private set; }

		public IReadOnlyList<ITypeParameter> TypeParameters { get; set; } = EmptyList<ITypeParameter>.Instance;

		IReadOnlyList<IType> IMethod.TypeArguments => TypeParameters;

		bool IMethod.IsExtensionMethod => false;
		bool IMethod.IsLocalFunction => false;
		bool IMethod.IsConstructor => symbolKind == SymbolKind.Constructor;
		bool IMethod.IsDestructor => symbolKind == SymbolKind.Destructor;
		bool IMethod.IsOperator => symbolKind == SymbolKind.Operator;

		bool IMethod.HasBody => false;
		bool IMethod.IsAccessor => false;
		IMember IMethod.AccessorOwner => null;
		dnlib.DotNet.MethodSemanticsAttributes IMethod.AccessorKind => 0;

		IMethod IMethod.ReducedFrom => null;

		public IReadOnlyList<IParameter> Parameters { get; set; } = EmptyList<IParameter>.Instance;

		public dnlib.DotNet.IMethod MetadataToken => dnlibRef;

		public override IType ReturnType {
			get {
				var t = LazyInit.VolatileRead(ref retType);
				if (t is not null)
					return t;

				t = dnlibRef.MethodSig.RetType.DecodeSignature(module, new GenericContext(DeclaringType?.GetDefinition()?.TypeParameters));

				if (t is ModifiedType modifier && modifier.Modifier.Name == "IsExternalInit" &&
					modifier.Modifier.Namespace == "System.Runtime.CompilerServices") {
					IsInitOnly = true;
					t = modifier.ElementType;
				}

				return LazyInit.GetOrSet(ref retType, t);
			}
		}

		public override IMember Specialize(TypeParameterSubstitution substitution)
		{
			return SpecializedMethod.Create(this, substitution);
		}

		IMethod IMethod.Specialize(TypeParameterSubstitution substitution)
		{
			return SpecializedMethod.Create(this, substitution);
		}
	}
}
