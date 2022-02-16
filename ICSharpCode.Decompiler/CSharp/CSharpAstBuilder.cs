using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using dnlib.DotNet;
using dnlib.DotNet.Emit;

using dnSpy.Contracts.Decompiler;

using ICSharpCode.Decompiler.CSharp.OutputVisitor;
using ICSharpCode.Decompiler.CSharp.Syntax;
using ICSharpCode.Decompiler.CSharp.Syntax.PatternMatching;
using ICSharpCode.Decompiler.CSharp.Transforms;
using ICSharpCode.Decompiler.IL;
using ICSharpCode.Decompiler.IL.ControlFlow;
using ICSharpCode.Decompiler.IL.Transforms;
using ICSharpCode.Decompiler.Metadata;
using ICSharpCode.Decompiler.Semantics;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.CSharp
{
	public partial class CSharpAstBuilder
	{
		readonly DecompilerContext context;
		SyntaxTree syntaxTree;
		readonly Dictionary<string, NamespaceDeclaration> astNamespaces = new Dictionary<string, NamespaceDeclaration>();
		bool transformationsHaveRun;
		readonly StringBuilder stringBuilder;// PERF: prevent extra created strings
		readonly char[] commentBuffer;// PERF: prevent extra created strings
		readonly List<Task<AsyncMethodBodyResult>> methodBodyTasks = new List<Task<AsyncMethodBodyResult>>();
		readonly List<AsyncMethodBodyDecompilationState> asyncMethodBodyDecompilationStates = new List<AsyncMethodBodyDecompilationState>();
		readonly List<Comment> comments = new List<Comment>();
		IDecompilerTypeSystem typeSystem;
		TypeSystemAstBuilder typeSystemAstBuilder;
		ITypeResolveContext currentTypeResolveContext;
		DecompileRun currentDecompileRun;

		public DecompilerContext Context => this.context;

		public SyntaxTree SyntaxTree => this.syntaxTree;

		const int COMMENT_BUFFER_LENGTH = 2 + 8;

		public Func<CSharpAstBuilder, MethodDef, DecompiledBodyKind> GetDecompiledBodyKind { get; set; }

		public CSharpAstBuilder(DecompilerContext context)
		{
			this.context = context ?? throw new ArgumentNullException(nameof(context));
			this.stringBuilder = new StringBuilder();
			this.commentBuffer = new char[COMMENT_BUFFER_LENGTH];
			this.syntaxTree = new SyntaxTree();
			this.transformationsHaveRun = false;
			this.GetDecompiledBodyKind = null;
			this.typeSystem = null;
			this.typeSystemAstBuilder = null;
			this.currentTypeResolveContext = null;
			this.currentDecompileRun = null;
		}

		public void Reset()
		{
			this.typeSystem = null;
			this.typeSystemAstBuilder = null;
			this.currentTypeResolveContext = null;
			this.currentDecompileRun = null;
			this.GetDecompiledBodyKind = null;
			this.syntaxTree = new SyntaxTree();
			this.transformationsHaveRun = false;
			this.astNamespaces.Clear();
			this.stringBuilder.Clear();
			this.context.Reset();
			this.methodBodyTasks.Clear();
		}

		public void InitializeTypeSystem()
		{
			typeSystem = new DecompilerTypeSystem(new PEFile(context.CurrentModule), context.Settings);
			typeSystemAstBuilder = new TypeSystemAstBuilder {
				ShowAttributes = true,
				AlwaysUseShortTypeNames = true,
				AddResolveResultAnnotations = true,
				UseNullableSpecifierForValueTypes = context.Settings.LiftNullables,
				SupportInitAccessors = context.Settings.InitAccessors,
				SupportRecordClasses = context.Settings.RecordClasses
			};
			currentTypeResolveContext =
				new SimpleTypeResolveContext(typeSystem.MainModule).WithCurrentTypeDefinition(
					typeSystem.MainModule.GetDefinition(context.CurrentType));
			currentDecompileRun = new DecompileRun(context.Settings) { CancellationToken = context.CancellationToken };
		}

		public void AddAssembly(ModuleDef moduleDefinition, bool decompileAsm, bool decompileMod)
		{
			IEnumerable<IAttribute> attributes = null;
			string target = null;
			if (decompileAsm && moduleDefinition.Assembly != null)
			{
				RequiredNamespaceCollector.CollectAttributeNamespacesOnlyAssembly(typeSystem.MainModule, currentDecompileRun.Namespaces);
				attributes = typeSystem.MainModule.GetAssemblyAttributes();
				target = "assembly";
			}

			if (decompileMod)
			{
				RequiredNamespaceCollector.CollectAttributeNamespacesOnlyModule(typeSystem.MainModule, currentDecompileRun.Namespaces);
				attributes = typeSystem.MainModule.GetModuleAttributes();
				target = "module";
			}

			if (attributes is null)
				return;

			foreach (var a in attributes) {
				var attrSection = new AttributeSection(typeSystemAstBuilder.ConvertAttribute(a)) {
					AttributeTarget = target
				};
				syntaxTree.AddChild(attrSection, SyntaxTree.MemberRole);
			}
		}

		NamespaceDeclaration GetCodeNamespace(string name, IAssembly asm)
		{
			if (string.IsNullOrEmpty(name))
				return null;
			if (astNamespaces.TryGetValue(name, out var astNamespace))
				return astNamespace;

			// Create the namespace
			astNamespace = new NamespaceDeclaration(name, asm);
			syntaxTree.Members.Add(astNamespace);
			astNamespaces[name] = astNamespace;
			return astNamespace;
		}

		public void AddType(TypeDef typeDef)
		{
			var astType = CreateType(typeDef);
			NamespaceDeclaration astNS = GetCodeNamespace(typeDef.Namespace, typeDef.DefinitionAssembly);
			if (astNS is null)
				syntaxTree.Members.Add(astType);
			else
				astNS.Members.Add(astType);
		}

		public void AddMethod(MethodDef method)
		{
			syntaxTree.Members.Add(CreateMethod(method));
		}

		public void AddProperty(PropertyDef property)
		{
			syntaxTree.Members.Add(CreateProperty(property));
		}

		public void AddField(FieldDef field)
		{
			syntaxTree.Members.Add(CreateField(field));
		}

		public void AddEvent(EventDef ev)
		{
			syntaxTree.Members.Add(CreateEvent(ev));
		}

		public EntityDeclaration CreateType(TypeDef typeDef)
		{
			TypeDef oldCurrentType = context.CurrentType;
			var oldResolveContext = currentTypeResolveContext;
			context.CurrentType = typeDef;

			var tsTypeDef = typeSystem.MainModule.GetDefinition(typeDef);

			RequiredNamespaceCollector.CollectNamespacesOnlyType(tsTypeDef, currentDecompileRun.Namespaces);

			currentTypeResolveContext = currentTypeResolveContext.WithCurrentTypeDefinition(tsTypeDef);

			var entityDecl = typeSystemAstBuilder.ConvertEntity(tsTypeDef);
			if (entityDecl is not TypeDeclaration typeDecl)
			{
				if (entityDecl is DelegateDeclaration dd)
				{
					//TODO: cleaner
					AddComment(dd, (MethodDef)tsTypeDef.GetDelegateInvokeMethod().MetadataToken, "Invoke");
					AddComment(dd, typeDef);
				}

				// e.g. DelegateDeclaration
				return entityDecl;
			}

			bool isRecord = Context.Settings.RecordClasses && tsTypeDef.IsRecord;
			RecordDecompiler recordDecompiler = isRecord ? new RecordDecompiler(typeSystem, tsTypeDef, context.Settings, context.CancellationToken) : null;
			if (recordDecompiler != null)
				currentDecompileRun.RecordDecompilers.Add(tsTypeDef, recordDecompiler);

			if (recordDecompiler?.PrimaryConstructor != null)
			{
				foreach (var p in recordDecompiler.PrimaryConstructor.Parameters)
				{
					ParameterDeclaration pd = typeSystemAstBuilder.ConvertParameter(p);
					(IProperty prop, ICSharpCode.Decompiler.TypeSystem.IField field) = recordDecompiler.GetPropertyInfoByPrimaryConstructorParameter(p);
					Syntax.Attribute[] attributes = prop.GetAttributes().Select(attr => typeSystemAstBuilder.ConvertAttribute(attr)).ToArray();
					if (attributes.Length > 0)
					{
						var section = new AttributeSection {
							AttributeTarget = "property"
						};
						section.Attributes.AddRange(attributes);
						pd.Attributes.Add(section);
					}
					attributes = field.GetAttributes()
									  .Where(a => !PatternStatementTransform.attributeTypesToRemoveFromAutoProperties.Contains(a.AttributeType.FullName))
									  .Select(attr => typeSystemAstBuilder.ConvertAttribute(attr)).ToArray();
					if (attributes.Length > 0)
					{
						var section = new AttributeSection {
							AttributeTarget = "field"
						};
						section.Attributes.AddRange(attributes);
						pd.Attributes.Add(section);
					}
					typeDecl.PrimaryConstructorParameters.Add(pd);
				}
			}

			foreach (var type in GetNestedTypes(typeDef))
			{
				if (!MemberIsHidden(type, context.Settings))
				{
					var nestedType = CreateType(type);
					SetNewModifier(nestedType);
					typeDecl.Members.Add(nestedType);
				}
			}

			currentDecompileRun.EnumValueDisplayMode = tsTypeDef.Kind == TypeKind.Enum
				? DetectBestEnumValueDisplayMode(tsTypeDef)
				: null;

			// With C# 9 records, the relative order of fields and properties matters:
			IEnumerable<IMember> fieldsAndProperties = recordDecompiler?.FieldsAndProperties
				?? tsTypeDef.Fields.Concat<IMember>(tsTypeDef.Properties);
			foreach (var fieldOrProperty in fieldsAndProperties)
			{
				if (MemberIsHidden(fieldOrProperty.MetadataToken, context.Settings))
				{
					continue;
				}
				if (fieldOrProperty is ICSharpCode.Decompiler.TypeSystem.IField field)
				{
					if (tsTypeDef.Kind == TypeKind.Enum && !field.IsConst)
						continue;
					typeDecl.Members.Add(CreateField((FieldDef)field.MetadataToken));
				}
				else if (fieldOrProperty is IProperty property)
				{
					if (recordDecompiler?.PropertyIsGenerated(property) == true)
					{
						continue;
					}
					typeDecl.Members.Add(CreateProperty(property.MetadataToken));
				}
			}
			foreach (var @event in tsTypeDef.Events) {
				if (!MemberIsHidden(@event.MetadataToken,context.Settings))
				{
					typeDecl.Members.Add(CreateEvent(@event.MetadataToken));
				}
			}
			foreach (var method in tsTypeDef.Methods)
			{
				if (recordDecompiler?.MethodIsGenerated(method) == true)
				{
					continue;
				}
				// Check if this is a fake method.
				if (method.MetadataToken is null)
					continue;
				if (!MemberIsHidden(method.MetadataToken, context.Settings))
				{
					var memberDecl = CreateMethod((MethodDef)method.MetadataToken);
					typeDecl.Members.Add(memberDecl);
					typeDecl.Members.AddRange(AddInterfaceImplHelpers(memberDecl, method, typeSystemAstBuilder));
				}
			}
			if (typeDecl.Members.OfType<IndexerDeclaration>().Any(idx => idx.PrivateImplementationType.IsNull))
			{
				// Remove the [DefaultMember] attribute if the class contains indexers
				RemoveAttribute(typeDecl, KnownAttribute.DefaultMember);
			}
			if (context.Settings.IntroduceRefModifiersOnStructs)
			{
				if (FindAttribute(typeDecl, KnownAttribute.Obsolete, out var attr))
				{
					if (obsoleteAttributePattern.IsMatch(attr))
					{
						if (attr.Parent is AttributeSection section && section.Attributes.Count == 1)
							section.Remove();
						else
							attr.Remove();
					}
				}
			}
			if (typeDecl.ClassType == ClassType.Enum)
			{
				switch (currentDecompileRun.EnumValueDisplayMode)
				{
					case EnumValueDisplayMode.FirstOnly:
						foreach (var enumMember in typeDecl.Members.OfType<EnumMemberDeclaration>().Skip(1))
						{
							enumMember.Initializer = null;
						}
						break;
					case EnumValueDisplayMode.None:
						foreach (var enumMember in typeDecl.Members.OfType<EnumMemberDeclaration>())
						{
							enumMember.Initializer = null;
							if (enumMember.GetSymbol() is ICSharpCode.Decompiler.TypeSystem.IField f && f.GetConstantValue() == null)
							{
								typeDecl.InsertChildBefore(enumMember, new Comment(" error: enumerator has no value"), Roles.Comment);
							}
						}
						break;
					case EnumValueDisplayMode.All:
					case EnumValueDisplayMode.AllHex:
						// nothing needs to be changed.
						break;
					default:
						throw new ArgumentOutOfRangeException();
				}
				currentDecompileRun.EnumValueDisplayMode = null;
			}

			AddComment(typeDecl, typeDef);

			context.CurrentType = oldCurrentType;
			currentTypeResolveContext = oldResolveContext;
			return typeDecl;
		}

		EntityDeclaration CreateMethod(MethodDef methodDef)
		{
			var tsMethod = typeSystem.MainModule.GetDefinition(methodDef);

			RequiredNamespaceCollector.CollectNamespaces(tsMethod, typeSystem.MainModule, currentDecompileRun.Namespaces);

			var methodDecl = typeSystemAstBuilder.ConvertEntity(tsMethod);

			int lastDot = tsMethod.Name.LastIndexOf('.');
			if (tsMethod.IsExplicitInterfaceImplementation && lastDot >= 0)
			{
				methodDecl.NameToken.Name = tsMethod.Name.Substring(lastDot + 1);
			}
			FixParameterNames(methodDecl);

			if (!context.Settings.LocalFunctions && LocalFunctionDecompiler.LocalFunctionNeedsAccessibilityChange(null, methodDef))
			{
				// if local functions are not active and we're dealing with a local function,
				// reduce the visibility of the method to private,
				// otherwise this leads to compile errors because the display classes have lesser accessibility.
				// Note: removing and then adding the static modifier again is necessary to set the private modifier before all other modifiers.
				methodDecl.Modifiers &= ~(Modifiers.Internal | Modifiers.Static);
				methodDecl.Modifiers |= Modifiers.Private | (tsMethod.IsStatic ? Modifiers.Static : 0);
			}

			if (methodDef.HasBody)
			{
				DecompileBody(tsMethod, methodDecl, currentDecompileRun, currentTypeResolveContext.WithCurrentMember(tsMethod));
			}
			else if (!tsMethod.IsAbstract && tsMethod.DeclaringType.Kind != TypeKind.Interface)
			{
				methodDecl.Modifiers |= Modifiers.Extern;
			}
			if (tsMethod.SymbolKind == SymbolKind.Method && !tsMethod.IsExplicitInterfaceImplementation && methodDef.IsVirtual == methodDef.IsNewSlot)
			{
				SetNewModifier(methodDecl);
			}
			if (IsCovariantReturnOverride(tsMethod))
			{
				RemoveAttribute(methodDecl, KnownAttribute.PreserveBaseOverrides);
				methodDecl.Modifiers &= ~(Modifiers.New | Modifiers.Virtual);
				methodDecl.Modifiers |= Modifiers.Override;
			}

			AddComment(methodDecl, methodDef);

			return methodDecl;
		}

		IEnumerable<InterfaceImpl> GetInterfaceImpls(TypeDef type)
		{
			if (context.Settings.UseSourceCodeOrder)
				return type.Interfaces;// These are already sorted by MD token
			return type.GetInterfaceImpls(context.Settings.SortMembers);
		}

		IEnumerable<TypeDef> GetNestedTypes(TypeDef type)
		{
			if (context.Settings.UseSourceCodeOrder)
				return type.NestedTypes;// These are already sorted by MD token
			return type.GetNestedTypes(context.Settings.SortMembers);
		}

		IEnumerable<FieldDef> GetFields(TypeDef type)
		{
			if (context.Settings.UseSourceCodeOrder)
				return type.Fields;// These are already sorted by MD token
			return type.GetFields(context.Settings.SortMembers);
		}

		void AddTypeMembers(TypeDeclaration astType, TypeDef typeDef)
		{
			bool hasShownMethods = false;
			foreach (var d in this.context.Settings.DecompilationObjects) {
				switch (d) {
				case DecompilationObject.NestedTypes:
					foreach (TypeDef nestedTypeDef in GetNestedTypes(typeDef)) {
						if (MemberIsHidden(nestedTypeDef, context.Settings))
							continue;
						var nestedType = CreateType(nestedTypeDef);
						SetNewModifier(nestedType);
						astType.AddChild(nestedType, Roles.TypeMemberRole);
					}
					break;

				case DecompilationObject.Fields:
					foreach (FieldDef fieldDef in GetFields(typeDef)) {
						if (MemberIsHidden(fieldDef, context.Settings)) continue;
						astType.AddChild(CreateField(fieldDef), Roles.TypeMemberRole);
					}
					break;

				case DecompilationObject.Events:
					if (hasShownMethods)
						break;
					if (context.Settings.UseSourceCodeOrder || !typeDef.CanSortMethods()) {
						ShowAllMethods(astType, typeDef);
						hasShownMethods = true;
						break;
					}
					foreach (EventDef eventDef in typeDef.GetEvents(context.Settings.SortMembers)) {
						if (eventDef.AddMethod == null && eventDef.RemoveMethod == null)
							continue;
						astType.AddChild(CreateEvent(eventDef), Roles.TypeMemberRole);
					}
					break;

				case DecompilationObject.Properties:
					if (hasShownMethods)
						break;
					if (context.Settings.UseSourceCodeOrder || !typeDef.CanSortMethods()) {
						ShowAllMethods(astType, typeDef);
						hasShownMethods = true;
						break;
					}
					foreach (PropertyDef propDef in typeDef.GetProperties(context.Settings.SortMembers)) {
						if (propDef.GetMethod == null && propDef.SetMethod == null)
							continue;
						astType.Members.Add(CreateProperty(propDef));
					}
					break;

				case DecompilationObject.Methods:
					if (hasShownMethods)
						break;
					if (context.Settings.UseSourceCodeOrder || !typeDef.CanSortMethods()) {
						ShowAllMethods(astType, typeDef);
						hasShownMethods = true;
						break;
					}
					foreach (MethodDef methodDef in typeDef.GetMethods(context.Settings.SortMembers)) {
						if (MemberIsHidden(methodDef, context.Settings)) continue;

						astType.Members.Add(CreateMethod(methodDef));
					}
					break;

				default: throw new InvalidOperationException();
				}
			}
		}

		void ShowAllMethods(TypeDeclaration astType, TypeDef type)
		{
			foreach (var def in type.GetNonSortedMethodsPropertiesEvents()) {
				var md = def as MethodDef;
				if (md != null) {
					if (MemberIsHidden(md, context.Settings))
						continue;
					astType.Members.Add(CreateMethod(md));
					continue;
				}

				var pd = def as PropertyDef;
				if (pd != null) {
					if (pd.GetMethod == null && pd.SetMethod == null)
						continue;
					astType.Members.Add(CreateProperty(pd));
					continue;
				}

				var ed = def as EventDef;
				if (ed != null) {
					if (ed.AddMethod == null && ed.RemoveMethod == null)
						continue;
					astType.AddChild(CreateEvent(ed), Roles.TypeMemberRole);
					continue;
				}

				Debug.Fail("Shouldn't be here");
			}
		}

		EntityDeclaration CreateField(FieldDef fieldDef)
		{
			var tsField = typeSystem.MainModule.GetDefinition(fieldDef);

			RequiredNamespaceCollector.CollectNamespaces(tsField, typeSystem.MainModule, currentDecompileRun.Namespaces);

			if (currentTypeResolveContext.CurrentTypeDefinition!.Kind == TypeKind.Enum && tsField.IsConst) {
				var enumDec = new EnumMemberDeclaration();
				enumDec.WithAnnotation(fieldDef);
				enumDec.NameToken = Identifier.Create(tsField.Name).WithAnnotation(fieldDef);
				object constantValue = tsField.GetConstantValue();
				if (constantValue != null) {
					long initValue = (long)CSharpPrimitiveCast.Cast(TypeCode.Int64, constantValue, false);
					enumDec.Initializer = typeSystemAstBuilder.ConvertConstantValue(currentTypeResolveContext.CurrentTypeDefinition.EnumUnderlyingType, constantValue);
					if (enumDec.Initializer is PrimitiveExpression primitive
						&& initValue >= 10 && currentDecompileRun.EnumValueDisplayMode == EnumValueDisplayMode.AllHex)
					{
						primitive.Format = LiteralFormat.HexadecimalNumber;
					}
				}

				enumDec.Attributes.AddRange(tsField.GetAttributes().Select(a => new AttributeSection(typeSystemAstBuilder.ConvertAttribute(a))));
				enumDec.AddAnnotation(new MemberResolveResult(null, tsField));
				return enumDec;
			}

			bool oldUseSpecialConstants = typeSystemAstBuilder.UseSpecialConstants;
			bool isMathPIOrE = (tsField.Name == "PI" || tsField.Name == "E") && (tsField.DeclaringType.FullName == "System.Math" || tsField.DeclaringType.FullName == "System.MathF");
			typeSystemAstBuilder.UseSpecialConstants = !(tsField.DeclaringType.Equals(tsField.ReturnType) || isMathPIOrE);

			var fieldDecl = typeSystemAstBuilder.ConvertEntity(tsField);
			SetNewModifier(fieldDecl);

			if (context.Settings.FixedBuffers && IsFixedField(tsField, out var elementType, out var elementCount)) {
				var fixedFieldDecl = new FixedFieldDeclaration();
				fieldDecl.Attributes.MoveTo(fixedFieldDecl.Attributes);
				fixedFieldDecl.Modifiers = fieldDecl.Modifiers;
				fixedFieldDecl.ReturnType = typeSystemAstBuilder.ConvertType(elementType);
				fixedFieldDecl.Variables.Add(new FixedVariableInitializer {
					NameToken = Identifier.Create(tsField.Name).WithAnnotation(fieldDef),
					CountExpression = new PrimitiveExpression(elementCount)
				}.WithAnnotation(fieldDef));
				fixedFieldDecl.Variables.Single().CopyAnnotationsFrom(((FieldDeclaration)fieldDecl).Variables.Single());
				fixedFieldDecl.CopyAnnotationsFrom(fieldDecl);

				RemoveAttribute(fixedFieldDecl, KnownAttribute.FixedBuffer);
				AddComment(fixedFieldDecl, fieldDef);

				typeSystemAstBuilder.UseSpecialConstants = oldUseSpecialConstants;
				return fixedFieldDecl;
			}
			if (fieldDef.HasFieldRVA && fieldDef.InitialValue.Length > 0) {
				// Field data as specified in II.16.3.2 of ECMA-335 6th edition:
				// .data I_X = int32(123)
				// .field public static int32 _x at I_X
				var message = $" Not supported: data({BitConverter.ToString(fieldDef.InitialValue).Replace('-', ' ')}) ";
				((FieldDeclaration)fieldDecl).Variables.Single().AddChild(new Comment(message, CommentType.MultiLine), Roles.Comment);
			}

			AddComment(fieldDecl, fieldDef);

			typeSystemAstBuilder.UseSpecialConstants = oldUseSpecialConstants;
			return fieldDecl;
		}

		EntityDeclaration CreateProperty(PropertyDef propertyDef)
		{
			var tsProperty = typeSystem.MainModule.GetDefinition(propertyDef);

			RequiredNamespaceCollector.CollectNamespaces(tsProperty, typeSystem.MainModule, currentDecompileRun.Namespaces);

			EntityDeclaration propertyDecl = typeSystemAstBuilder.ConvertEntity(tsProperty);
			if (tsProperty.IsExplicitInterfaceImplementation && !tsProperty.IsIndexer) {
				int lastDot = tsProperty.Name.LastIndexOf('.');
				propertyDecl.NameToken.Name = tsProperty.Name.Substring(lastDot + 1);
			}

			FixParameterNames(propertyDecl);
			Accessor getter, setter;
			if (propertyDecl is PropertyDeclaration propertyDeclaration) {
				getter = propertyDeclaration.Getter;
				setter = propertyDeclaration.Setter;
			} else {
				getter = ((IndexerDeclaration)propertyDecl).Getter;
				setter = ((IndexerDeclaration)propertyDecl).Setter;
			}

			bool getterHasBody = tsProperty.CanGet && tsProperty.Getter!.HasBody;
			bool setterHasBody = tsProperty.CanSet && tsProperty.Setter!.HasBody;
			if (getterHasBody) {
				DecompileBody(tsProperty.Getter, getter, currentDecompileRun, currentTypeResolveContext.WithCurrentMember(tsProperty));
			}
			if (setterHasBody) {
				DecompileBody(tsProperty.Setter, setter, currentDecompileRun, currentTypeResolveContext.WithCurrentMember(tsProperty));
			}
			if (!getterHasBody && !setterHasBody && !tsProperty.IsAbstract && tsProperty.DeclaringType.Kind != TypeKind.Interface) {
				propertyDecl.Modifiers |= Modifiers.Extern;
			}
			var accessor = propertyDef.GetMethod ?? propertyDef.SetMethod;
			if (!accessor.HasOverrides && accessor.IsVirtual == accessor.IsNewSlot)
			{
				SetNewModifier(propertyDecl);
			}
			if (getterHasBody && IsCovariantReturnOverride(tsProperty.Getter))
			{
				RemoveAttribute(getter, KnownAttribute.PreserveBaseOverrides);
				propertyDecl.Modifiers &= ~(Modifiers.New | Modifiers.Virtual);
				propertyDecl.Modifiers |= Modifiers.Override;
			}

			if (propertyDef.SetMethod != null)
				AddComment(propertyDecl, propertyDef.SetMethod, "set");
			if (propertyDef.GetMethod != null)
				AddComment(propertyDecl, propertyDef.GetMethod, "get");
			AddComment(propertyDecl, propertyDef);

			return propertyDecl;
		}

		EntityDeclaration CreateEvent(EventDef eventDef)
		{
			var tsEvent = typeSystem.MainModule.GetDefinition(eventDef);

			RequiredNamespaceCollector.CollectNamespaces(tsEvent, typeSystem.MainModule, currentDecompileRun.Namespaces);

			bool adderHasBody = tsEvent.CanAdd && tsEvent.AddAccessor!.HasBody;
			bool removerHasBody = tsEvent.CanRemove && tsEvent.RemoveAccessor!.HasBody;
			typeSystemAstBuilder.UseCustomEvents = tsEvent.DeclaringTypeDefinition!.Kind != TypeKind.Interface
												   || tsEvent.IsExplicitInterfaceImplementation
												   || adderHasBody
												   || removerHasBody;
			var eventDecl = typeSystemAstBuilder.ConvertEntity(tsEvent);
			if (tsEvent.IsExplicitInterfaceImplementation) {
				int lastDot = tsEvent.Name.LastIndexOf('.');
				eventDecl.NameToken.Name = tsEvent.Name.Substring(lastDot + 1);
			}

			if (adderHasBody)
			{
				DecompileBody(tsEvent.AddAccessor, ((CustomEventDeclaration)eventDecl).AddAccessor, currentDecompileRun, currentTypeResolveContext.WithCurrentMember(tsEvent));
			}
			if (removerHasBody)
			{
				DecompileBody(tsEvent.RemoveAccessor, ((CustomEventDeclaration)eventDecl).RemoveAccessor, currentDecompileRun, currentTypeResolveContext.WithCurrentMember(tsEvent));
			}
			if (!adderHasBody && !removerHasBody && !tsEvent.IsAbstract && tsEvent.DeclaringType.Kind != TypeKind.Interface)
			{
				eventDecl.Modifiers |= Modifiers.Extern;
			}
			var accessor = eventDef.AddMethod ?? eventDef.RemoveMethod;
			if (accessor.IsVirtual == accessor.IsNewSlot) {
				SetNewModifier(eventDecl);
			}

			if (eventDef.RemoveMethod != null)
				AddComment(eventDecl, eventDef.RemoveMethod, "remove");
			if (eventDef.AddMethod != null)
				AddComment(eventDecl, eventDef.AddMethod, "add");
			AddComment(eventDecl, eventDef);

			return eventDecl;
		}

		void DecompileBody(Decompiler.TypeSystem.IMethod method, EntityDeclaration entityDecl, DecompileRun decompileRun, ITypeResolveContext decompilationContext)
		{
			try {
				var ilReader = new ILReader(typeSystem.MainModule) {
					UseDebugSymbols = context.Settings.UseDebugSymbols
				};
				var body = BlockStatement.Null;
				var methodDefinition = (MethodDef)method.MetadataToken;
				var function = ilReader.ReadIL(methodDefinition, cancellationToken: context.CancellationToken);
				function.CheckInvariant(ILPhase.Normal);

				AddAnnotationsToDeclaration(method, entityDecl, function);

				var localSettings = context.Settings.Clone();
				if (IsWindowsFormsInitializeComponentMethod(method))
				{
					localSettings.UseImplicitMethodGroupConversion = false;
					localSettings.UsingDeclarations = false;
					localSettings.AlwaysCastTargetsOfExplicitInterfaceImplementationCalls = true;
					localSettings.NamedArguments = false;
					localSettings.AlwaysQualifyMemberReferences = true;
				}

				var ilTransformContext = new ILTransformContext(function, typeSystem, localSettings) {
					CancellationToken = context.CancellationToken,
					DecompileRun = decompileRun
				};
				foreach (var transform in GetILTransforms())
				{
					context.CancellationToken.ThrowIfCancellationRequested();
					transform.Run(function, ilTransformContext);
					function.CheckInvariant(ILPhase.Normal);
					// When decompiling definitions only, we can cancel decompilation of all steps
					// after yield and async detection, because only those are needed to properly set
					// IsAsync/IsIterator flags on ILFunction.
					if (!localSettings.DecompileMemberBodies && transform is AsyncAwaitDecompiler)
						break;
				}

				// Generate C# AST only if bodies should be displayed.
				if (localSettings.DecompileMemberBodies) {
					AddDefinesForConditionalAttributes(function, decompileRun);
					var statementBuilder = new StatementBuilder(
						typeSystem,
						decompilationContext,
						function,
						localSettings,
						decompileRun,
						context.CancellationToken
					);
					body = statementBuilder.ConvertAsBlock(function.Body);

					AssignSourceLocals(function);

					Comment prev = null;
					foreach (string warning in function.Warnings)
					{
						body.InsertChildAfter(prev, prev = new Comment(warning), Roles.Comment);
					}

					entityDecl.AddChild(body, Roles.Body);
				}
				entityDecl.AddAnnotation(function);

				CleanUpMethodDeclaration(entityDecl, body, function, localSettings.DecompileMemberBodies);
			} catch (Exception innerException) when (!(innerException is OperationCanceledException || innerException is DecompilerException)) {
				throw new DecompilerException(method.MetadataToken, innerException);
			}
		}

		private static void AssignSourceLocals(ILFunction function)
		{
			foreach (ILFunction ilFunction in function.Descendants.OfType<ILFunction>())
			{
				var dict = new Dictionary<Local, SourceLocal>();
				foreach (ILVariable variable in ilFunction.Variables)
				{
					if (variable.OriginalVariable is null)
						continue;
					if (dict.TryGetValue(variable.OriginalVariable, out var existing))
					{
						variable.sourceParamOrLocal = existing;
					}
					else
					{
						dict[variable.OriginalVariable] = variable.GetSourceLocal();
					}
				}
			}
		}

		public void RunTransformations()
		{
			RunTransformations(null);
		}

		public void RunTransformations(Predicate<IAstTransform> transformAbortCondition)
		{
			RunTransforms();
			//WaitForBodies();
			//TransformationPipeline.RunTransformationsUntil(syntaxTree, transformAbortCondition, context);
			transformationsHaveRun = true;
		}

		public void RunTransforms()
		{
			var transformContext = new TransformContext(typeSystem, currentDecompileRun, currentTypeResolveContext, typeSystemAstBuilder);
			foreach (var transform in GetAstTransforms())
			{
				context.CancellationToken.ThrowIfCancellationRequested();
				transform.Run(syntaxTree, transformContext);
			}
			context.CancellationToken.ThrowIfCancellationRequested();
			syntaxTree.AcceptVisitor(new InsertParenthesesVisitor { InsertParenthesesForReadability = true });
			context.CancellationToken.ThrowIfCancellationRequested();
			GenericGrammarAmbiguityVisitor.ResolveAmbiguities(syntaxTree);
		}

		public void GenerateCode(IDecompilerOutput output)
		{
			if (!transformationsHaveRun)
				RunTransformations();

			var outputFormatter = new TextTokenWriter(output, context.MetadataTextColorProvider);
			var formattingPolicy = context.Settings.CSharpFormattingOptions;
			syntaxTree.AcceptVisitor(new CSharpOutputVisitor(outputFormatter, formattingPolicy, context.CancellationToken));
		}

		char ToHexChar(int val) {
			Debug.Assert(0 <= val && val <= 0x0F);
			if (0 <= val && val <= 9)
				return (char)('0' + val);
			return (char)('A' + val - 10);
		}

		string ToHex(uint value) {
			commentBuffer[0] = '0';
			commentBuffer[1] = 'x';
			int j = 2;
			for (int i = 0; i < 4; i++) {
				commentBuffer[j++] = ToHexChar((int)(value >> 28) & 0x0F);
				commentBuffer[j++] = ToHexChar((int)(value >> 24) & 0x0F);
				value <<= 8;
			}
			return new string(commentBuffer, 0, j);
		}

		void AddComment(AstNode node, IMemberDef member, string text = null)
		{
			if (!this.context.Settings.ShowTokenAndRvaComments)
				return;
			member.GetRVA(out uint rva, out long fileOffset);

			var creator = new CommentReferencesCreator(stringBuilder);
			creator.AddText(" ");
			if (text != null) {
				creator.AddText("(");
				creator.AddText(text);
				creator.AddText(") ");
			}
			creator.AddText("Token: ");
			creator.AddReference(ToHex(member.MDToken.Raw), new TokenReference(member));
			creator.AddText(" RID: ");
			creator.AddText(member.MDToken.Rid.ToString());
			if (rva != 0) {
				var mod = member.Module;
				var filename = mod?.Location;
				creator.AddText(" RVA: ");
				creator.AddReference(ToHex(rva), new AddressReference(filename, true, rva, 0));
				creator.AddText(" File Offset: ");
				creator.AddReference(ToHex((uint)fileOffset), new AddressReference(filename, false, (ulong)fileOffset, 0));
			}

			var cmt = new Comment(creator.Text) {
				References = creator.CommentReferences
			};
			node.InsertChildAfter(null, cmt, Roles.Comment);
		}
	}
}
