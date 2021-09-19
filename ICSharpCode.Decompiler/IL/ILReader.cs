// Copyright (c) 2014 Daniel Grunwald
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
using System.Linq;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Collections;
using System.Threading;
using dnlib.DotNet.Emit;
using dnSpy.Contracts.Decompiler;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using ICSharpCode.Decompiler.Util;
using Extensions = dnlib.DotNet.Extensions;

namespace ICSharpCode.Decompiler.IL
{
	/// <summary>
	/// Reads IL bytecodes and converts them into ILAst instructions.
	/// </summary>
	/// <remarks>
	/// Instances of this class are not thread-safe. Use separate instances to decompile multiple members in parallel.
	/// </remarks>
	public class ILReader
	{
		readonly ICompilation compilation;
		readonly MetadataModule module;
		readonly dnlib.DotNet.ModuleDef metadata;

		public bool UseDebugSymbols { get; set; }
		public List<string> Warnings { get; } = new List<string>();

		// List of candidate locations for sequence points. Includes empty il stack locations, any nop instructions, and the instruction following
		// a call instruction.
		public List<int> SequencePointCandidates { get; private set; }

		/// <summary>
		/// Creates a new ILReader instance.
		/// </summary>
		/// <param name="module">
		/// The module used to resolve metadata tokens in the type system.
		/// </param>
		public ILReader(MetadataModule module)
		{
			if (module == null)
				throw new ArgumentNullException(nameof(module));
			this.module = module;
			this.compilation = module.Compilation;
			this.metadata = module.metadata;
			this.SequencePointCandidates = new List<int>();
		}

		GenericContext genericContext;
		IMethod method;
		CilBody body;
		dnlib.DotNet.MethodDef methodDef;
		StackType methodReturnStackType;
		Instruction currentInstruction;
		int nextInstructionIndex;
		ImmutableStack<ILVariable> currentStack;
		ILVariable[] parameterVariables;
		ILVariable[] localVariables;
		BitArray isBranchTarget;
		BlockContainer mainContainer;
		List<ILInstruction> instructionBuilder;
		int currentInstructionStart;

		// Dictionary that stores stacks for each IL instruction
		Dictionary<int, ImmutableStack<ILVariable>> stackByOffset;
		Dictionary<ExceptionHandler, ILVariable> variableByExceptionHandler;
		UnionFind<ILVariable> unionFind;
		List<(ILVariable, ILVariable)> stackMismatchPairs;
		IEnumerable<ILVariable> stackVariables;

		void Init(dnlib.DotNet.MethodDef methodDef, GenericContext genericContext)
		{
			if (methodDef == null)
				throw new ArgumentNullException(nameof(methodDef));
			this.methodDef = methodDef;
			this.method = module.GetDefinition(methodDef);
			if (genericContext.ClassTypeParameters == null && genericContext.MethodTypeParameters == null) {
				// no generic context specified: use the method's own type parameters
				genericContext = new GenericContext(method);
			} else {
				// generic context specified, so specialize the method for it:
				this.method = this.method.Specialize(genericContext.ToSubstitution());
			}
			this.genericContext = genericContext;
			this.body = methodDef.Body;
			this.currentInstruction = null;
			this.nextInstructionIndex = 0;
			this.currentStack = ImmutableStack<ILVariable>.Empty;
			this.unionFind = new UnionFind<ILVariable>();
			this.stackMismatchPairs = new List<(ILVariable, ILVariable)>();
			this.methodReturnStackType = method.ReturnType.GetStackType();
			InitParameterVariables();
			this.localVariables = body.Variables.SelectArray(CreateILVariable);
			if (body.InitLocals) {
				foreach (var v in localVariables) {
					v.HasInitialValue = true;
				}
			}
			this.mainContainer = new BlockContainer(expectedResultType: methodReturnStackType);
			this.instructionBuilder = new List<ILInstruction>();
			this.isBranchTarget = new BitArray(body.GetCodeSize());
			this.stackByOffset = new Dictionary<int, ImmutableStack<ILVariable>>();
			this.variableByExceptionHandler = new Dictionary<ExceptionHandler, ILVariable>();
		}

		dnlib.DotNet.IMDTokenProvider ReadAndDecodeMetadataToken()
		{
			if (currentInstruction.Operand is dnlib.DotNet.IMDTokenProvider mdTokenProvider) {
				return mdTokenProvider;
			}
			throw new BadImageFormatException("Invalid metadata token");
		}

		IType ReadAndDecodeTypeReference()
		{
			var typeReference = (dnlib.DotNet.ITypeDefOrRef)ReadAndDecodeMetadataToken();
			return module.ResolveType(typeReference, genericContext);
		}

		IMethod ReadAndDecodeMethodReference()
		{
			var methodReference = (dnlib.DotNet.IMethod)ReadAndDecodeMetadataToken();
			return module.ResolveMethod(methodReference, genericContext);
		}

		IField ReadAndDecodeFieldReference()
		{
			var fieldReference = ReadAndDecodeMetadataToken();
			var f = module.ResolveEntity(fieldReference, genericContext) as IField;
			if (f == null)
				throw new BadImageFormatException("Invalid field token");
			return f;
		}

		void InitParameterVariables()
		{
			parameterVariables = new ILVariable[GetPopCount(OpCode.Call, methodDef)];
			int paramIndex = 0;
			foreach (var p in methodDef.Parameters)
				parameterVariables[paramIndex++] = CreateILVariable(p);
			Debug.Assert(paramIndex == parameterVariables.Length);
		}

		ILVariable CreateILVariable(Local v)
		{
			VariableKind kind = IsPinned(v.Type) ? VariableKind.PinnedLocal : VariableKind.Local;

			IType localType = module.ResolveType(v.Type, genericContext);
			if (kind == VariableKind.PinnedLocal && localType.SkipModifiers() is PinnedType pinnedType)
				localType = pinnedType.ElementType;

			ILVariable ilVar = new ILVariable(kind, localType, v.Index);
			if (!UseDebugSymbols || v.Name == null) {
				ilVar.Name = "V_" + v.Index;
				ilVar.HasGeneratedName = true;
			} else if (string.IsNullOrWhiteSpace(v.Name)) {
				ilVar.Name = "V_" + v.Index;
				ilVar.HasGeneratedName = true;
			} else {
				ilVar.Name = v.Name;
			}
			return ilVar;
		}

		bool IsPinned(dnlib.DotNet.TypeSig type)
		{
			while (type is dnlib.DotNet.ModifierSig) {
				type = type.Next;
			}
			return type.IsPinned;
		}

		ILVariable CreateILVariable(dnlib.DotNet.Parameter p)
		{
			IType parameterType;
			bool isRefReadOnly;
			if (p.IsHiddenThisParameter) {
				ITypeDefinition def = module.ResolveType(methodDef.DeclaringType, genericContext).GetDefinition();
				if (def != null && def.TypeParameterCount > 0) {
					parameterType = new ParameterizedType(def, def.TypeArguments);
					if (def.IsReferenceType == false) {
						parameterType = new ByReferenceType(parameterType);
					}
				} else {
					parameterType = module.ResolveType(p.Type, genericContext);
				}
				isRefReadOnly = method.ThisIsRefReadOnly;
			} else {
				var param = method.Parameters[p.MethodSigIndex];
				parameterType = param.Type;
				isRefReadOnly = param.IsIn;
			}

			Debug.Assert(!parameterType.IsUnbound());
			if (parameterType.IsUnbound()) {
				// parameter types should not be unbound, the only known cause for these is a Cecil bug:
				Debug.Assert(p.Index < 0); // cecil bug occurs only for "this"
				parameterType = new ParameterizedType(parameterType.GetDefinition(), parameterType.TypeArguments);
			}
			var ilVar = new ILVariable(VariableKind.Parameter, parameterType, p.MethodSigIndex == -2 ? -1 : p.MethodSigIndex);
			ilVar.IsRefReadOnly = isRefReadOnly;
			Debug.Assert(ilVar.StoreCount == 1); // count the initial store when the method is called with an argument
			if (p.IsHiddenThisParameter)
				ilVar.Name = "this";
			else if (string.IsNullOrEmpty(p.Name))
				ilVar.Name = "P_" + p.MethodSigIndex;
			else
				ilVar.Name = p.Name;
			return ilVar;
		}

		/// <summary>
		/// Warn when invalid IL is detected.
		/// ILSpy should be able to handle invalid IL; but this method can be helpful for debugging the ILReader,
		/// as this method should not get called when processing valid IL.
		/// </summary>
		void Warn(string message)
		{
			Warnings.Add(string.Format("IL_{0:x4}: {1}", currentInstructionStart, message));
		}

		ImmutableStack<ILVariable> MergeStacks(ImmutableStack<ILVariable> a, ImmutableStack<ILVariable> b)
		{
			if (CheckStackCompatibleWithoutAdjustments(a, b)) {
				// We only need to union the input variables, but can
				// otherwise re-use the existing stack.
				ImmutableStack<ILVariable> output = a;
				while (!a.IsEmpty && !b.IsEmpty) {
					Debug.Assert(a.Peek().StackType == b.Peek().StackType);
					unionFind.Merge(a.Peek(), b.Peek());
					a = a.Pop();
					b = b.Pop();
				}
				return output;
			} else if (a.Count() != b.Count()) {
				// Let's not try to merge mismatched stacks.
				Warn("Incompatible stack heights: " + a.Count() + " vs " + b.Count());
				return a;
			} else {
				// The more complex case where the stacks don't match exactly.
				var output = new List<ILVariable>();
				while (!a.IsEmpty && !b.IsEmpty) {
					var varA = a.Peek();
					var varB = b.Peek();
					if (varA.StackType == varB.StackType) {
						unionFind.Merge(varA, varB);
						output.Add(varA);
					} else {
						if (!IsValidTypeStackTypeMerge(varA.StackType, varB.StackType)) {
							Warn("Incompatible stack types: " + varA.StackType + " vs " + varB.StackType);
						}
						if (varA.StackType > varB.StackType) {
							output.Add(varA);
							// every store to varB should also store to varA
							stackMismatchPairs.Add((varB, varA));
						} else {
							output.Add(varB);
							// every store to varA should also store to varB
							stackMismatchPairs.Add((varA, varB));
						}
					}
					a = a.Pop();
					b = b.Pop();
				}
				// because we built up output by popping from the input stacks, we need to reverse it to get back the original order
				output.Reverse();
				return ImmutableStack.CreateRange(output);
			}
		}

		static bool CheckStackCompatibleWithoutAdjustments(ImmutableStack<ILVariable> a, ImmutableStack<ILVariable> b)
		{
			while (!a.IsEmpty && !b.IsEmpty) {
				if (a.Peek().StackType != b.Peek().StackType)
					return false;
				a = a.Pop();
				b = b.Pop();
			}
			return a.IsEmpty && b.IsEmpty;
		}

		private bool IsValidTypeStackTypeMerge(StackType stackType1, StackType stackType2)
		{
			if (stackType1 == StackType.I && stackType2 == StackType.I4)
				return true;
			if (stackType1 == StackType.I4 && stackType2 == StackType.I)
				return true;
			if (stackType1 == StackType.F4 && stackType2 == StackType.F8)
				return true;
			if (stackType1 == StackType.F8 && stackType2 == StackType.F4)
				return true;
			// allow merging unknown type with any other type
			return stackType1 == StackType.Unknown || stackType2 == StackType.Unknown;
		}

		/// <summary>
		/// Stores the given stack for a branch to `offset`.
		///
		/// The stack may be modified if stack adjustments are necessary. (e.g. implicit I4->I conversion)
		/// </summary>
		void StoreStackForOffset(int offset, ref ImmutableStack<ILVariable> stack)
		{
			if (stackByOffset.TryGetValue(offset, out var existing)) {
				stack = MergeStacks(existing, stack);
				if (stack != existing)
					stackByOffset[offset] = stack;
			} else {
				stackByOffset.Add(offset, stack);
			}
		}

		void ReadInstructions(CancellationToken cancellationToken)
		{
			// Fill isBranchTarget and branchStackDict based on exception handlers
			foreach (var eh in body.ExceptionHandlers) {
				ImmutableStack<ILVariable> ehStack = null;
				if (eh.HandlerType == ExceptionHandlerType.Catch) {
					var v = new ILVariable(VariableKind.ExceptionStackSlot, module.ResolveType(eh.CatchType, genericContext), (int)eh.HandlerStart.Offset) {
						Name = "E_" + eh.HandlerStart.Offset,
						HasGeneratedName = true
					};
					variableByExceptionHandler.Add(eh, v);
					ehStack = ImmutableStack.Create(v);
				} else if (eh.HandlerType == ExceptionHandlerType.Filter) {
					var v = new ILVariable(VariableKind.ExceptionStackSlot, compilation.FindType(KnownTypeCode.Object), (int)eh.HandlerStart.Offset) {
						Name = "E_" + eh.HandlerStart.Offset,
						HasGeneratedName = true
					};
					variableByExceptionHandler.Add(eh, v);
					ehStack = ImmutableStack.Create(v);
				} else {
					ehStack = ImmutableStack<ILVariable>.Empty;
				}
				if (eh.FilterStart != null) {
					isBranchTarget[(int)eh.FilterStart.Offset] = true;
					StoreStackForOffset((int)eh.FilterStart.Offset, ref ehStack);
				}
				if (eh.HandlerStart != null) {
					isBranchTarget[(int)eh.HandlerStart.Offset] = true;
					StoreStackForOffset((int)eh.HandlerStart.Offset, ref ehStack);
				}
			}

			while (nextInstructionIndex < body.Instructions.Count) {
				cancellationToken.ThrowIfCancellationRequested();
				int start = (int)body.Instructions[nextInstructionIndex].Offset;
				StoreStackForOffset(start, ref currentStack);
				currentInstructionStart = start;
				bool startedWithEmptyStack = currentStack.IsEmpty;
				ILInstruction decodedInstruction;
				try {
					decodedInstruction = DecodeInstruction();
				} catch (BadImageFormatException ex) {
					decodedInstruction = new InvalidBranch(ex.Message);
				}
				if (decodedInstruction.ResultType == StackType.Unknown && decodedInstruction.OpCode != OpCode.InvalidBranch && UnpackPush(decodedInstruction).OpCode != OpCode.InvalidExpression)
					Warn("Unknown result type (might be due to invalid IL or missing references)");
				decodedInstruction.CheckInvariant(ILPhase.InILReader);
				int end = currentInstruction.GetEndOffset();
				decodedInstruction.AddILRange(new Interval(start, end));
				UnpackPush(decodedInstruction).AddILRange(decodedInstruction);
				instructionBuilder.Add(decodedInstruction);
				if (decodedInstruction.HasDirectFlag(InstructionFlags.EndPointUnreachable)) {
					if (!stackByOffset.TryGetValue(end, out currentStack)) {
						currentStack = ImmutableStack<ILVariable>.Empty;
					}
				}

				if (IsSequencePointInstruction(decodedInstruction) || startedWithEmptyStack) {
					this.SequencePointCandidates.Add(decodedInstruction.StartILOffset);
				}
			}

			var visitor = new CollectStackVariablesVisitor(unionFind);
			for (int i = 0; i < instructionBuilder.Count; i++) {
				instructionBuilder[i] = instructionBuilder[i].AcceptVisitor(visitor);
			}
			stackVariables = visitor.variables;
			InsertStackAdjustments();
		}

		private bool IsSequencePointInstruction(ILInstruction instruction)
		{
			if (instruction.OpCode == OpCode.Nop ||
				(this.instructionBuilder.Count > 0 &&
				this.instructionBuilder.Last().OpCode == OpCode.Call ||
				this.instructionBuilder.Last().OpCode == OpCode.CallIndirect ||
				this.instructionBuilder.Last().OpCode == OpCode.CallVirt)) {

				return true;
			} else {
				return false;
			}
		}

		void InsertStackAdjustments()
		{
			if (stackMismatchPairs.Count == 0)
				return;
			var dict = new MultiDictionary<ILVariable, ILVariable>();
			foreach (var (origA, origB) in stackMismatchPairs) {
				var a = unionFind.Find(origA);
				var b = unionFind.Find(origB);
				Debug.Assert(a.StackType < b.StackType);
				// For every store to a, insert a converting store to b.
				if (!dict[a].Contains(b))
					dict.Add(a, b);
			}
			var newInstructions = new List<ILInstruction>();
			foreach (var inst in instructionBuilder) {
				newInstructions.Add(inst);
				if (inst is StLoc store) {
					foreach (var additionalVar in dict[store.Variable]) {
						ILInstruction value = new LdLoc(store.Variable);
						value = new Conv(value, additionalVar.StackType.ToPrimitiveType(), false, Sign.Signed);
						newInstructions.Add(new StLoc(additionalVar, value) {
							IsStackAdjustment = true,
						}.WithILRange(inst));
					}
				}
			}
			instructionBuilder = newInstructions;
		}

		/// <summary>
		/// Decodes the specified method body and returns an ILFunction.
		/// </summary>
		public ILFunction ReadIL(dnlib.DotNet.MethodDef methodDef, GenericContext genericContext = default, ILFunctionKind kind = ILFunctionKind.TopLevelFunction, CancellationToken cancellationToken = default(CancellationToken))
		{
			cancellationToken.ThrowIfCancellationRequested();
			Init(methodDef, genericContext);
			ReadInstructions(cancellationToken);
			var blockBuilder = new BlockBuilder(body, variableByExceptionHandler);
			blockBuilder.CreateBlocks(mainContainer, instructionBuilder, isBranchTarget, cancellationToken);
			var function = new ILFunction(this.method, methodDef, this.genericContext, mainContainer, kind);
			Util.CollectionExtensions.AddRange(function.Variables, parameterVariables);
			Util.CollectionExtensions.AddRange(function.Variables, localVariables);
			Util.CollectionExtensions.AddRange(function.Variables, stackVariables);
			Util.CollectionExtensions.AddRange(function.Variables, variableByExceptionHandler.Values);
			function.AddRef(); // mark the root node
			var removedBlocks = new List<Block>();
			foreach (var c in function.Descendants.OfType<BlockContainer>()) {
				var newOrder = c.TopologicalSort(deleteUnreachableBlocks: true);
				if (newOrder.Count < c.Blocks.Count) {
					removedBlocks.AddRange(c.Blocks.Except(newOrder));
				}
				c.Blocks.ReplaceList(newOrder);
			}
			if (removedBlocks.Count > 0) {
				removedBlocks.SortBy(b => b.StartILOffset);
				function.Warnings.Add("Discarded unreachable code: "
							+ string.Join(", ", removedBlocks.Select(b => $"IL_{b.StartILOffset:x4}")));
			}

			this.SequencePointCandidates.Sort();
			function.SequencePointCandidates = this.SequencePointCandidates;

			function.Warnings.AddRange(Warnings);
			return function;
		}

		static ILInstruction UnpackPush(ILInstruction inst)
		{
			ILVariable v;
			ILInstruction inner;
			if (inst.MatchStLoc(out v, out inner) && v.Kind == VariableKind.StackSlot)
				return inner;
			else
				return inst;
		}

		ILInstruction Neg()
		{
			switch (PeekStackType()) {
				case StackType.I4:
					return Push(new BinaryNumericInstruction(BinaryNumericOperator.Sub, new LdcI4(0), Pop(), checkForOverflow: false, sign: Sign.None));
				case StackType.I:
					return Push(new BinaryNumericInstruction(BinaryNumericOperator.Sub, new Conv(new LdcI4(0), PrimitiveType.I, false, Sign.None), Pop(), checkForOverflow: false, sign: Sign.None));
				case StackType.I8:
					return Push(new BinaryNumericInstruction(BinaryNumericOperator.Sub, new LdcI8(0), Pop(), checkForOverflow: false, sign: Sign.None));
				case StackType.F4:
					return Push(new BinaryNumericInstruction(BinaryNumericOperator.Sub, new LdcF4(0), Pop(), checkForOverflow: false, sign: Sign.None));
				case StackType.F8:
					return Push(new BinaryNumericInstruction(BinaryNumericOperator.Sub, new LdcF8(0), Pop(), checkForOverflow: false, sign: Sign.None));
				default:
					Warn("Unsupported input type for neg.");
					goto case StackType.I4;
			}
		}

		ILInstruction DecodeInstruction()
		{
			if (nextInstructionIndex >= body.Instructions.Count)
				return new InvalidBranch("Unexpected end of body");
			var cecilInst = body.Instructions[nextInstructionIndex++];
			currentInstruction = cecilInst;
			switch (cecilInst.OpCode.Code) {
				case Code.Constrained:
					return DecodeConstrainedCall();
				case Code.Readonly:
					return DecodeReadonly();
				case Code.Tailcall:
					return DecodeTailCall();
				case Code.Unaligned:
					return DecodeUnaligned();
				case Code.Volatile:
					return DecodeVolatile();
				case Code.Add:
					return BinaryNumeric(BinaryNumericOperator.Add);
				case Code.Add_Ovf:
					return BinaryNumeric(BinaryNumericOperator.Add, true, Sign.Signed);
				case Code.Add_Ovf_Un:
					return BinaryNumeric(BinaryNumericOperator.Add, true, Sign.Unsigned);
				case Code.And:
					return BinaryNumeric(BinaryNumericOperator.BitAnd);
				case Code.Arglist:
					return Push(new Arglist());
				case Code.Beq:
					return DecodeComparisonBranch(false, ComparisonKind.Equality);
				case Code.Beq_S:
					return DecodeComparisonBranch(true, ComparisonKind.Equality);
				case Code.Bge:
					return DecodeComparisonBranch(false, ComparisonKind.GreaterThanOrEqual);
				case Code.Bge_S:
					return DecodeComparisonBranch(true, ComparisonKind.GreaterThanOrEqual);
				case Code.Bge_Un:
					return DecodeComparisonBranch(false, ComparisonKind.GreaterThanOrEqual, un: true);
				case Code.Bge_Un_S:
					return DecodeComparisonBranch(true, ComparisonKind.GreaterThanOrEqual, un: true);
				case Code.Bgt:
					return DecodeComparisonBranch(false, ComparisonKind.GreaterThan);
				case Code.Bgt_S:
					return DecodeComparisonBranch(true, ComparisonKind.GreaterThan);
				case Code.Bgt_Un:
					return DecodeComparisonBranch(false, ComparisonKind.GreaterThan, un: true);
				case Code.Bgt_Un_S:
					return DecodeComparisonBranch(true, ComparisonKind.GreaterThan, un: true);
				case Code.Ble:
					return DecodeComparisonBranch(false, ComparisonKind.LessThanOrEqual);
				case Code.Ble_S:
					return DecodeComparisonBranch(true, ComparisonKind.LessThanOrEqual);
				case Code.Ble_Un:
					return DecodeComparisonBranch(false, ComparisonKind.LessThanOrEqual, un: true);
				case Code.Ble_Un_S:
					return DecodeComparisonBranch(true, ComparisonKind.LessThanOrEqual, un: true);
				case Code.Blt:
					return DecodeComparisonBranch(false, ComparisonKind.LessThan);
				case Code.Blt_S:
					return DecodeComparisonBranch(true, ComparisonKind.LessThan);
				case Code.Blt_Un:
					return DecodeComparisonBranch(false, ComparisonKind.LessThan, un: true);
				case Code.Blt_Un_S:
					return DecodeComparisonBranch(true, ComparisonKind.LessThan, un: true);
				case Code.Bne_Un:
					return DecodeComparisonBranch(false, ComparisonKind.Inequality, un: true);
				case Code.Bne_Un_S:
					return DecodeComparisonBranch(true, ComparisonKind.Inequality, un: true);
				case Code.Br:
					return DecodeUnconditionalBranch(false);
				case Code.Br_S:
					return DecodeUnconditionalBranch(true);
				case Code.Break:
					return new DebugBreak();
				case Code.Brfalse:
					return DecodeConditionalBranch(false, true);
				case Code.Brfalse_S:
					return DecodeConditionalBranch(true, true);
				case Code.Brtrue:
					return DecodeConditionalBranch(false, false);
				case Code.Brtrue_S:
					return DecodeConditionalBranch(true, false);
				case Code.Call:
					return DecodeCall(OpCode.Call);
				case Code.Callvirt:
					return DecodeCall(OpCode.CallVirt);
				case Code.Calli:
					return DecodeCallIndirect();
				case Code.Ceq:
					return Push(Comparison(ComparisonKind.Equality));
				case Code.Cgt:
					return Push(Comparison(ComparisonKind.GreaterThan));
				case Code.Cgt_Un:
					return Push(Comparison(ComparisonKind.GreaterThan, un: true));
				case Code.Clt:
					return Push(Comparison(ComparisonKind.LessThan));
				case Code.Clt_Un:
					return Push(Comparison(ComparisonKind.LessThan, un: true));
				case Code.Ckfinite:
					return new Ckfinite(Peek());
				case Code.Conv_I1:
					return Push(new Conv(Pop(), PrimitiveType.I1, false, Sign.None));
				case Code.Conv_I2:
					return Push(new Conv(Pop(), PrimitiveType.I2, false, Sign.None));
				case Code.Conv_I4:
					return Push(new Conv(Pop(), PrimitiveType.I4, false, Sign.None));
				case Code.Conv_I8:
					return Push(new Conv(Pop(), PrimitiveType.I8, false, Sign.None));
				case Code.Conv_R4:
					return Push(new Conv(Pop(), PrimitiveType.R4, false, Sign.Signed));
				case Code.Conv_R8:
					return Push(new Conv(Pop(), PrimitiveType.R8, false, Sign.Signed));
				case Code.Conv_U1:
					return Push(new Conv(Pop(), PrimitiveType.U1, false, Sign.None));
				case Code.Conv_U2:
					return Push(new Conv(Pop(), PrimitiveType.U2, false, Sign.None));
				case Code.Conv_U4:
					return Push(new Conv(Pop(), PrimitiveType.U4, false, Sign.None));
				case Code.Conv_U8:
					return Push(new Conv(Pop(), PrimitiveType.U8, false, Sign.None));
				case Code.Conv_I:
					return Push(new Conv(Pop(), PrimitiveType.I, false, Sign.None));
				case Code.Conv_U:
					return Push(new Conv(Pop(), PrimitiveType.U, false, Sign.None));
				case Code.Conv_R_Un:
					return Push(new Conv(Pop(), PrimitiveType.R, false, Sign.Unsigned));
				case Code.Conv_Ovf_I1:
					return Push(new Conv(Pop(), PrimitiveType.I1, true, Sign.Signed));
				case Code.Conv_Ovf_I2:
					return Push(new Conv(Pop(), PrimitiveType.I2, true, Sign.Signed));
				case Code.Conv_Ovf_I4:
					return Push(new Conv(Pop(), PrimitiveType.I4, true, Sign.Signed));
				case Code.Conv_Ovf_I8:
					return Push(new Conv(Pop(), PrimitiveType.I8, true, Sign.Signed));
				case Code.Conv_Ovf_U1:
					return Push(new Conv(Pop(), PrimitiveType.U1, true, Sign.Signed));
				case Code.Conv_Ovf_U2:
					return Push(new Conv(Pop(), PrimitiveType.U2, true, Sign.Signed));
				case Code.Conv_Ovf_U4:
					return Push(new Conv(Pop(), PrimitiveType.U4, true, Sign.Signed));
				case Code.Conv_Ovf_U8:
					return Push(new Conv(Pop(), PrimitiveType.U8, true, Sign.Signed));
				case Code.Conv_Ovf_I:
					return Push(new Conv(Pop(), PrimitiveType.I, true, Sign.Signed));
				case Code.Conv_Ovf_U:
					return Push(new Conv(Pop(), PrimitiveType.U, true, Sign.Signed));
				case Code.Conv_Ovf_I1_Un:
					return Push(new Conv(Pop(), PrimitiveType.I1, true, Sign.Unsigned));
				case Code.Conv_Ovf_I2_Un:
					return Push(new Conv(Pop(), PrimitiveType.I2, true, Sign.Unsigned));
				case Code.Conv_Ovf_I4_Un:
					return Push(new Conv(Pop(), PrimitiveType.I4, true, Sign.Unsigned));
				case Code.Conv_Ovf_I8_Un:
					return Push(new Conv(Pop(), PrimitiveType.I8, true, Sign.Unsigned));
				case Code.Conv_Ovf_U1_Un:
					return Push(new Conv(Pop(), PrimitiveType.U1, true, Sign.Unsigned));
				case Code.Conv_Ovf_U2_Un:
					return Push(new Conv(Pop(), PrimitiveType.U2, true, Sign.Unsigned));
				case Code.Conv_Ovf_U4_Un:
					return Push(new Conv(Pop(), PrimitiveType.U4, true, Sign.Unsigned));
				case Code.Conv_Ovf_U8_Un:
					return Push(new Conv(Pop(), PrimitiveType.U8, true, Sign.Unsigned));
				case Code.Conv_Ovf_I_Un:
					return Push(new Conv(Pop(), PrimitiveType.I, true, Sign.Unsigned));
				case Code.Conv_Ovf_U_Un:
					return Push(new Conv(Pop(), PrimitiveType.U, true, Sign.Unsigned));
				case Code.Cpblk:
					return new Cpblk(size: Pop(StackType.I4), sourceAddress: PopPointer(), destAddress: PopPointer());
				case Code.Div:
					return BinaryNumeric(BinaryNumericOperator.Div, false, Sign.Signed);
				case Code.Div_Un:
					return BinaryNumeric(BinaryNumericOperator.Div, false, Sign.Unsigned);
				case Code.Dup:
					return Push(Peek());
				case Code.Endfilter:
					return new Leave(null, Pop());
				case Code.Endfinally:
					return new Leave(null);
				case Code.Initblk:
					return new Initblk(size: Pop(StackType.I4), value: Pop(StackType.I4), address: PopPointer());
				case Code.Jmp:
					return DecodeJmp();
				case Code.Ldarg:
				case Code.Ldarg_S:
					return Push(Ldarg(((dnlib.DotNet.Parameter)cecilInst.Operand)?.Index ?? -1));
				case Code.Ldarg_0:
					return Push(Ldarg(0));
				case Code.Ldarg_1:
					return Push(Ldarg(1));
				case Code.Ldarg_2:
					return Push(Ldarg(2));
				case Code.Ldarg_3:
					return Push(Ldarg(3));
				case Code.Ldarga:
				case Code.Ldarga_S:
					return Push(Ldarga(((dnlib.DotNet.Parameter)cecilInst.Operand)?.Index ?? -1));
				case Code.Ldc_I4:
					return Push(new LdcI4((int)cecilInst.Operand));
				case Code.Ldc_I8:
					return Push(new LdcI8((long)cecilInst.Operand));
				case Code.Ldc_R4:
					return Push(new LdcF4((float)cecilInst.Operand));
				case Code.Ldc_R8:
					return Push(new LdcF8((double)cecilInst.Operand));
				case Code.Ldc_I4_M1:
					return Push(new LdcI4(-1));
				case Code.Ldc_I4_0:
					return Push(new LdcI4(0));
				case Code.Ldc_I4_1:
					return Push(new LdcI4(1));
				case Code.Ldc_I4_2:
					return Push(new LdcI4(2));
				case Code.Ldc_I4_3:
					return Push(new LdcI4(3));
				case Code.Ldc_I4_4:
					return Push(new LdcI4(4));
				case Code.Ldc_I4_5:
					return Push(new LdcI4(5));
				case Code.Ldc_I4_6:
					return Push(new LdcI4(6));
				case Code.Ldc_I4_7:
					return Push(new LdcI4(7));
				case Code.Ldc_I4_8:
					return Push(new LdcI4(8));
				case Code.Ldc_I4_S:
					return Push(new LdcI4((sbyte)cecilInst.Operand));
				case Code.Ldnull:
					return Push(new LdNull());
				case Code.Ldstr:
					return Push(DecodeLdstr());
				case Code.Ldftn:
					return Push(new LdFtn(ReadAndDecodeMethodReference()));
				case Code.Ldind_I1:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.SByte)));
				case Code.Ldind_I2:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.Int16)));
				case Code.Ldind_I4:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.Int32)));
				case Code.Ldind_I8:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.Int64)));
				case Code.Ldind_U1:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.Byte)));
				case Code.Ldind_U2:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.UInt16)));
				case Code.Ldind_U4:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.UInt32)));
				case Code.Ldind_R4:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.Single)));
				case Code.Ldind_R8:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.Double)));
				case Code.Ldind_I:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.IntPtr)));
				case Code.Ldind_Ref:
					return Push(new LdObj(PopPointer(), compilation.FindType(KnownTypeCode.Object)));
				case Code.Ldloc:
				case Code.Ldloc_S:
					return Push(Ldloc(((Local)cecilInst.Operand)?.Index ?? -1));
				case Code.Ldloc_0:
					return Push(Ldloc(0));
				case Code.Ldloc_1:
					return Push(Ldloc(1));
				case Code.Ldloc_2:
					return Push(Ldloc(2));
				case Code.Ldloc_3:
					return Push(Ldloc(3));
				case Code.Ldloca:
				case Code.Ldloca_S:
					return Push(Ldloca(((Local)cecilInst.Operand)?.Index ?? -1));
				case Code.Leave:
					return DecodeUnconditionalBranch(false, isLeave: true);
				case Code.Leave_S:
					return DecodeUnconditionalBranch(true, isLeave: true);
				case Code.Localloc:
					return Push(new LocAlloc(Pop()));
				case Code.Mul:
					return BinaryNumeric(BinaryNumericOperator.Mul, false, Sign.None);
				case Code.Mul_Ovf:
					return BinaryNumeric(BinaryNumericOperator.Mul, true, Sign.Signed);
				case Code.Mul_Ovf_Un:
					return BinaryNumeric(BinaryNumericOperator.Mul, true, Sign.Unsigned);
				case Code.Neg:
					return Neg();
				case Code.Newobj:
					return DecodeCall(OpCode.NewObj);
				case Code.Nop:
					return new Nop();
				case Code.Not:
					return Push(new BitNot(Pop()));
				case Code.Or:
					return BinaryNumeric(BinaryNumericOperator.BitOr);
				case Code.Pop:
					Pop();
					return new Nop() { Kind = NopKind.Pop };
				case Code.Rem:
					return BinaryNumeric(BinaryNumericOperator.Rem, false, Sign.Signed);
				case Code.Rem_Un:
					return BinaryNumeric(BinaryNumericOperator.Rem, false, Sign.Unsigned);
				case Code.Ret:
					return Return();
				case Code.Shl:
					return BinaryNumeric(BinaryNumericOperator.ShiftLeft, false, Sign.None);
				case Code.Shr:
					return BinaryNumeric(BinaryNumericOperator.ShiftRight, false, Sign.Signed);
				case Code.Shr_Un:
					return BinaryNumeric(BinaryNumericOperator.ShiftRight, false, Sign.Unsigned);
				case Code.Starg:
				case Code.Starg_S:
					return Starg(((dnlib.DotNet.Parameter)cecilInst.Operand)?.Index ?? -1);
				case Code.Stind_I1:
					return new StObj(value: Pop(StackType.I4), target: PopPointer(), type: compilation.FindType(KnownTypeCode.SByte));
				case Code.Stind_I2:
					return new StObj(value: Pop(StackType.I4), target: PopPointer(), type: compilation.FindType(KnownTypeCode.Int16));
				case Code.Stind_I4:
					return new StObj(value: Pop(StackType.I4), target: PopPointer(), type: compilation.FindType(KnownTypeCode.Int32));
				case Code.Stind_I8:
					return new StObj(value: Pop(StackType.I8), target: PopPointer(), type: compilation.FindType(KnownTypeCode.Int64));
				case Code.Stind_R4:
					return new StObj(value: Pop(StackType.F4), target: PopPointer(), type: compilation.FindType(KnownTypeCode.Single));
				case Code.Stind_R8:
					return new StObj(value: Pop(StackType.F8), target: PopPointer(), type: compilation.FindType(KnownTypeCode.Double));
				case Code.Stind_I:
					return new StObj(value: Pop(StackType.I), target: PopPointer(), type: compilation.FindType(KnownTypeCode.IntPtr));
				case Code.Stind_Ref:
					return new StObj(value: Pop(StackType.O), target: PopPointer(), type: compilation.FindType(KnownTypeCode.Object));
				case Code.Stloc:
				case Code.Stloc_S:
					return Stloc(((Local)cecilInst.Operand)?.Index ?? -1);
				case Code.Stloc_0:
					return Stloc(0);
				case Code.Stloc_1:
					return Stloc(1);
				case Code.Stloc_2:
					return Stloc(2);
				case Code.Stloc_3:
					return Stloc(3);
				case Code.Sub:
					return BinaryNumeric(BinaryNumericOperator.Sub, false, Sign.None);
				case Code.Sub_Ovf:
					return BinaryNumeric(BinaryNumericOperator.Sub, true, Sign.Signed);
				case Code.Sub_Ovf_Un:
					return BinaryNumeric(BinaryNumericOperator.Sub, true, Sign.Unsigned);
				case Code.Switch:
					return DecodeSwitch();
				case Code.Xor:
					return BinaryNumeric(BinaryNumericOperator.BitXor);
				case Code.Box:
					{
						var type = ReadAndDecodeTypeReference();
						return Push(new Box(Pop(type.GetStackType()), type));
					}
				case Code.Castclass:
					return Push(new CastClass(Pop(StackType.O), ReadAndDecodeTypeReference()));
				case Code.Cpobj:
					{
						var type = ReadAndDecodeTypeReference();
						var ld = new LdObj(PopPointer(), type);
						return new StObj(PopPointer(), ld, type);
					}
				case Code.Initobj:
					return InitObj(PopPointer(), ReadAndDecodeTypeReference());
				case Code.Isinst:
					return Push(new IsInst(Pop(StackType.O), ReadAndDecodeTypeReference()));
				case Code.Ldelem:
					return LdElem(ReadAndDecodeTypeReference());
				case Code.Ldelem_I1:
					return LdElem(compilation.FindType(KnownTypeCode.SByte));
				case Code.Ldelem_I2:
					return LdElem(compilation.FindType(KnownTypeCode.Int16));
				case Code.Ldelem_I4:
					return LdElem(compilation.FindType(KnownTypeCode.Int32));
				case Code.Ldelem_I8:
					return LdElem(compilation.FindType(KnownTypeCode.Int64));
				case Code.Ldelem_U1:
					return LdElem(compilation.FindType(KnownTypeCode.Byte));
				case Code.Ldelem_U2:
					return LdElem(compilation.FindType(KnownTypeCode.UInt16));
				case Code.Ldelem_U4:
					return LdElem(compilation.FindType(KnownTypeCode.UInt32));
				case Code.Ldelem_R4:
					return LdElem(compilation.FindType(KnownTypeCode.Single));
				case Code.Ldelem_R8:
					return LdElem(compilation.FindType(KnownTypeCode.Double));
				case Code.Ldelem_I:
					return LdElem(compilation.FindType(KnownTypeCode.IntPtr));
				case Code.Ldelem_Ref:
					return LdElem(compilation.FindType(KnownTypeCode.Object));
				case Code.Ldelema:
					return Push(new LdElema(indices: Pop(), array: Pop(), type: ReadAndDecodeTypeReference()));
				case Code.Ldfld:
					{
						var field = ReadAndDecodeFieldReference();
						return Push(new LdObj(new LdFlda(PopLdFldTarget(field), field) { DelayExceptions = true }, field.Type));
					}
				case Code.Ldflda:
					{
						var field = ReadAndDecodeFieldReference();
						return Push(new LdFlda(PopFieldTarget(field), field));
					}
				case Code.Stfld:
					{
						var field = ReadAndDecodeFieldReference();
						return new StObj(value: Pop(field.Type.GetStackType()), target: new LdFlda(PopFieldTarget(field), field) { DelayExceptions = true }, type: field.Type);
					}
				case Code.Ldlen:
					return Push(new LdLen(StackType.I, Pop(StackType.O)));
				case Code.Ldobj:
					return Push(new LdObj(PopPointer(), ReadAndDecodeTypeReference()));
				case Code.Ldsfld:
					{
						var field = ReadAndDecodeFieldReference();
						return Push(new LdObj(new LdsFlda(field), field.Type));
					}
				case Code.Ldsflda:
					return Push(new LdsFlda(ReadAndDecodeFieldReference()));
				case Code.Stsfld:
					{
						var field = ReadAndDecodeFieldReference();
						return new StObj(value: Pop(field.Type.GetStackType()), target: new LdsFlda(field), type: field.Type);
					}
				case Code.Ldtoken:
					return Push(LdToken(ReadAndDecodeMetadataToken()));
				case Code.Ldvirtftn:
					return Push(new LdVirtFtn(Pop(), ReadAndDecodeMethodReference()));
				case Code.Mkrefany:
					return Push(new MakeRefAny(PopPointer(), ReadAndDecodeTypeReference()));
				case Code.Newarr:
					return Push(new NewArr(ReadAndDecodeTypeReference(), Pop()));
				case Code.Refanytype:
					return Push(new RefAnyType(Pop()));
				case Code.Refanyval:
					return Push(new RefAnyValue(Pop(), ReadAndDecodeTypeReference()));
				case Code.Rethrow:
					return new Rethrow();
				case Code.Sizeof:
					return Push(new SizeOf(ReadAndDecodeTypeReference()));
				case Code.Stelem:
					return StElem(ReadAndDecodeTypeReference());
				case Code.Stelem_I1:
					return StElem(compilation.FindType(KnownTypeCode.SByte));
				case Code.Stelem_I2:
					return StElem(compilation.FindType(KnownTypeCode.Int16));
				case Code.Stelem_I4:
					return StElem(compilation.FindType(KnownTypeCode.Int32));
				case Code.Stelem_I8:
					return StElem(compilation.FindType(KnownTypeCode.Int64));
				case Code.Stelem_R4:
					return StElem(compilation.FindType(KnownTypeCode.Single));
				case Code.Stelem_R8:
					return StElem(compilation.FindType(KnownTypeCode.Double));
				case Code.Stelem_I:
					return StElem(compilation.FindType(KnownTypeCode.IntPtr));
				case Code.Stelem_Ref:
					return StElem(compilation.FindType(KnownTypeCode.Object));
				case Code.Stobj:
				{
					var type = ReadAndDecodeTypeReference();
					return new StObj(value: Pop(type.GetStackType()), target: PopPointer(), type: type);
				}
				case Code.Throw:
					return new Throw(Pop());
				case Code.Unbox:
					return Push(new Unbox(Pop(), ReadAndDecodeTypeReference()));
				case Code.Unbox_Any:
					return Push(new UnboxAny(Pop(), ReadAndDecodeTypeReference()));
				default:
					return new InvalidBranch("Unknown opcode: " + cecilInst.OpCode.ToString());
			}
		}

		StackType PeekStackType()
		{
			if (currentStack.IsEmpty)
				return StackType.Unknown;
			else
				return currentStack.Peek().StackType;
		}

		class CollectStackVariablesVisitor : ILVisitor<ILInstruction>
		{
			readonly UnionFind<ILVariable> unionFind;
			internal readonly HashSet<ILVariable> variables = new HashSet<ILVariable>();

			public CollectStackVariablesVisitor(UnionFind<ILVariable> unionFind)
			{
				Debug.Assert(unionFind != null);
				this.unionFind = unionFind;
			}

			protected override ILInstruction Default(ILInstruction inst)
			{
				foreach (var child in inst.Children) {
					var newChild = child.AcceptVisitor(this);
					if (newChild != child)
						child.ReplaceWith(newChild);
				}
				return inst;
			}

			protected internal override ILInstruction VisitLdLoc(LdLoc inst)
			{
				base.VisitLdLoc(inst);
				if (inst.Variable.Kind == VariableKind.StackSlot) {
					var variable = unionFind.Find(inst.Variable);
					if (variables.Add(variable))
						variable.Name = "S_" + (variables.Count - 1);
					return new LdLoc(variable).WithILRange(inst);
				}
				return inst;
			}

			protected internal override ILInstruction VisitStLoc(StLoc inst)
			{
				base.VisitStLoc(inst);
				if (inst.Variable.Kind == VariableKind.StackSlot) {
					var variable = unionFind.Find(inst.Variable);
					if (variables.Add(variable))
						variable.Name = "S_" + (variables.Count - 1);
					return new StLoc(variable, inst.Value).WithILRange(inst);
				}
				return inst;
			}
		}

		ILInstruction Push(ILInstruction inst)
		{
			Debug.Assert(inst.ResultType != StackType.Void);
			IType type = compilation.FindType(inst.ResultType.ToKnownTypeCode());
			var v = new ILVariable(VariableKind.StackSlot, type, inst.ResultType);
			v.HasGeneratedName = true;
			currentStack = currentStack.Push(v);
			return new StLoc(v, inst);
		}

		Interval GetCurrentInstructionInterval()
		{
			return new Interval((int)currentInstruction.Offset, currentInstruction.GetEndOffset());
		}

		ILInstruction Peek()
		{
			if (currentStack.IsEmpty) {
				return new InvalidExpression("Stack underflow").WithILRange(GetCurrentInstructionInterval());
			}
			return new LdLoc(currentStack.Peek());
		}

		ILInstruction Pop()
		{
			if (currentStack.IsEmpty) {
				return new InvalidExpression("Stack underflow").WithILRange(GetCurrentInstructionInterval());
			}
			ILVariable v;
			currentStack = currentStack.Pop(out v);
			return new LdLoc(v);
		}

		ILInstruction Pop(StackType expectedType)
		{
			ILInstruction inst = Pop();
			return Cast(inst, expectedType, Warnings, (int)currentInstruction.Offset);
		}

		internal static ILInstruction Cast(ILInstruction inst, StackType expectedType, List<string> warnings, int ilOffset)
		{
			if (expectedType != inst.ResultType) {
				if (inst is InvalidExpression) {
					((InvalidExpression)inst).ExpectedResultType = expectedType;
				} else if (expectedType == StackType.I && inst.ResultType == StackType.I4) {
					// IL allows implicit I4->I conversions
					inst = new Conv(inst, PrimitiveType.I, false, Sign.None);
				} else if (expectedType == StackType.I4 && inst.ResultType == StackType.I) {
					// C++/CLI also sometimes implicitly converts in the other direction:
					inst = new Conv(inst, PrimitiveType.I4, false, Sign.None);
				} else if (expectedType == StackType.Unknown) {
					inst = new Conv(inst, PrimitiveType.Unknown, false, Sign.None);
				} else if (inst.ResultType == StackType.Ref) {
					// Implicitly stop GC tracking; this occurs when passing the result of 'ldloca' or 'ldsflda'
					// to a method expecting a native pointer.
					inst = new Conv(inst, PrimitiveType.I, false, Sign.None);
					switch (expectedType) {
						case StackType.I4:
							inst = new Conv(inst, PrimitiveType.I4, false, Sign.None);
							break;
						case StackType.I:
							break;
						case StackType.I8:
							inst = new Conv(inst, PrimitiveType.I8, false, Sign.None);
							break;
						default:
							Warn($"Expected {expectedType}, but got {StackType.Ref}");
							inst = new Conv(inst, expectedType.ToPrimitiveType(), false, Sign.None);
							break;
					}
				} else if (expectedType == StackType.Ref) {
					// implicitly start GC tracking / object to interior
					if (!inst.ResultType.IsIntegerType() && inst.ResultType != StackType.O) {
						// We also handle the invalid to-ref cases here because the else case
						// below uses expectedType.ToKnownTypeCode(), which doesn't work for Ref.
						Warn($"Expected {expectedType}, but got {inst.ResultType}");
					}
					inst = new Conv(inst, PrimitiveType.Ref, false, Sign.None);
				} else if (expectedType == StackType.F8 && inst.ResultType == StackType.F4) {
					// IL allows implicit F4->F8 conversions, because in IL F4 and F8 are the same.
					inst = new Conv(inst, PrimitiveType.R8, false, Sign.Signed);
				} else if (expectedType == StackType.F4 && inst.ResultType == StackType.F8) {
					// IL allows implicit F8->F4 conversions, because in IL F4 and F8 are the same.
					inst = new Conv(inst, PrimitiveType.R4, false, Sign.Signed);
				} else {
					Warn($"Expected {expectedType}, but got {inst.ResultType}");
					inst = new Conv(inst, expectedType.ToPrimitiveType(), false, Sign.Signed);
				}
			}
			return inst;

			void Warn(string message)
			{
				if (warnings != null) {
					warnings.Add(string.Format("IL_{0:x4}: {1}", ilOffset, message));
				}
			}
		}

		ILInstruction PopPointer()
		{
			ILInstruction inst = Pop();
			switch (inst.ResultType) {
				case StackType.I4:
				case StackType.I8:
				case StackType.Unknown:
					return new Conv(inst, PrimitiveType.I, false, Sign.None);
				case StackType.I:
				case StackType.Ref:
					return inst;
				default:
					Warn("Expected native int or pointer, but got " + inst.ResultType);
					return new Conv(inst, PrimitiveType.I, false, Sign.None);
			}
		}

		ILInstruction PopFieldTarget(IField field)
		{
			switch (field.DeclaringType.IsReferenceType) {
				case true:
					return Pop(StackType.O);
				case false:
					return PopPointer();
				default:
					// field in unresolved type
					var stackType = PeekStackType();
					if (stackType == StackType.O || stackType == StackType.Unknown)
						return Pop();
					else
						return PopPointer();
			}
		}

		/// <summary>
		/// Like PopFieldTarget, but supports ldfld's special behavior for fields of temporary value types.
		/// </summary>
		ILInstruction PopLdFldTarget(IField field)
		{
			switch (field.DeclaringType.IsReferenceType) {
				case true:
					return Pop(StackType.O);
				case false:
					// field of value type: ldfld can handle temporaries
					if (PeekStackType() == StackType.O || PeekStackType() == StackType.Unknown)
						return new AddressOf(Pop(), field.DeclaringType);
					else
						return PopPointer();
				default:
					// field in unresolved type
					if (PeekStackType() == StackType.O || PeekStackType() == StackType.Unknown)
						return Pop();
					else
						return PopPointer();
			}
		}

		private ILInstruction Return()
		{
			if (methodReturnStackType == StackType.Void)
				return new IL.Leave(mainContainer);
			else
				return new IL.Leave(mainContainer, Pop(methodReturnStackType));
		}

		private ILInstruction DecodeLdstr()
		{
			return new LdStr((string)currentInstruction.Operand);
		}

		private ILInstruction Ldarg(int v)
		{
			if (v >= 0 && v < parameterVariables.Length) {
				return new LdLoc(parameterVariables[v]);
			} else {
				return new InvalidExpression($"ldarg {v} (out-of-bounds)");
			}
		}

		private ILInstruction Ldarga(int v)
		{
			if (v >= 0 && v < parameterVariables.Length) {
				return new LdLoca(parameterVariables[v]);
			} else {
				return new InvalidExpression($"ldarga {v} (out-of-bounds)");
			}
		}

		private ILInstruction Starg(int v)
		{
			if (v >= 0 && v < parameterVariables.Length) {
				return new StLoc(parameterVariables[v], Pop(parameterVariables[v].StackType));
			} else {
				Pop();
				return new InvalidExpression($"starg {v} (out-of-bounds)");
			}
		}

		private ILInstruction Ldloc(int v)
		{
			if (v >= 0 && v < localVariables.Length) {
				return new LdLoc(localVariables[v]);
			} else {
				return new InvalidExpression($"ldloc {v} (out-of-bounds)");
			}
		}

		private ILInstruction Ldloca(int v)
		{
			if (v >= 0 && v < localVariables.Length) {
				return new LdLoca(localVariables[v]);
			} else {
				return new InvalidExpression($"ldloca {v} (out-of-bounds)");
			}
		}

		private ILInstruction Stloc(int v)
		{
			if (v >= 0 && v < localVariables.Length) {
				return new StLoc(localVariables[v], Pop(localVariables[v].StackType)) {
					ILStackWasEmpty = currentStack.IsEmpty
				};
			} else {
				Pop();
				return new InvalidExpression($"stloc {v} (out-of-bounds)");
			}
		}

		private ILInstruction LdElem(IType type)
		{
			return Push(new LdObj(new LdElema(indices: Pop(), array: Pop(), type: type) { DelayExceptions = true }, type));
		}

		private ILInstruction StElem(IType type)
		{
			var value = Pop(type.GetStackType());
			var index = Pop();
			var array = Pop();
			return new StObj(new LdElema(type, array, index) { DelayExceptions = true }, value, type);
		}

		ILInstruction InitObj(ILInstruction target, IType type)
		{
			var value = new DefaultValue(type);
			value.ILStackWasEmpty = currentStack.IsEmpty;
			return new StObj(target, value, type);
		}

		IType constrainedPrefix;

		private ILInstruction DecodeConstrainedCall()
		{
			constrainedPrefix = ReadAndDecodeTypeReference();
			var inst = DecodeInstruction();
			var call = UnpackPush(inst) as CallInstruction;
			if (call != null)
				Debug.Assert(call.ConstrainedTo == constrainedPrefix);
			else
				Warn("Ignored invalid 'constrained' prefix");
			constrainedPrefix = null;
			return inst;
		}

		private ILInstruction DecodeTailCall()
		{
			var inst = DecodeInstruction();
			var call = UnpackPush(inst) as CallInstruction;
			if (call != null)
				call.IsTail = true;
			else
				Warn("Ignored invalid 'tail' prefix");
			return inst;
		}

		private ILInstruction DecodeUnaligned()
		{
			byte alignment = (byte)currentInstruction.Operand;
			var inst = DecodeInstruction();
			var sup = UnpackPush(inst) as ISupportsUnalignedPrefix;
			if (sup != null)
				sup.UnalignedPrefix = alignment;
			else
				Warn("Ignored invalid 'unaligned' prefix");
			return inst;
		}

		private ILInstruction DecodeVolatile()
		{
			var inst = DecodeInstruction();
			var svp = UnpackPush(inst) as ISupportsVolatilePrefix;
			if (svp != null)
				svp.IsVolatile = true;
			else
				Warn("Ignored invalid 'volatile' prefix");
			return inst;
		}

		private ILInstruction DecodeReadonly()
		{
			var inst = DecodeInstruction();
			var ldelema = UnpackPush(inst) as LdElema;
			if (ldelema != null)
				ldelema.IsReadOnly = true;
			else
				Warn("Ignored invalid 'readonly' prefix");
			return inst;
		}

		ILInstruction DecodeCall(OpCode opCode)
		{
			var method = ReadAndDecodeMethodReference();
			int firstArgument = (opCode != OpCode.NewObj && !method.IsStatic) ? 1 : 0;
			var arguments = new ILInstruction[firstArgument + method.Parameters.Count];
			for (int i = method.Parameters.Count - 1; i >= 0; i--) {
				arguments[firstArgument + i] = Pop(method.Parameters[i].Type.GetStackType());
			}
			if (firstArgument == 1) {
				arguments[0] = Pop(CallInstruction.ExpectedTypeForThisPointer(constrainedPrefix ?? method.DeclaringType));
			}
			switch (method.DeclaringType.Kind) {
				case TypeKind.Array:
				{
					var elementType = ((ArrayType)method.DeclaringType).ElementType;
					if (opCode == OpCode.NewObj)
						return Push(new NewArr(elementType, arguments));
					if (method.Name == "Set") {
						var target = arguments[0];
						var value = arguments.Last();
						var indices = arguments.Skip(1).Take(arguments.Length - 2).ToArray();
						return new StObj(new LdElema(elementType, target, indices) { DelayExceptions = true }, value, elementType);
					}
					if (method.Name == "Get") {
						var target = arguments[0];
						var indices = arguments.Skip(1).ToArray();
						return Push(new LdObj(new LdElema(elementType, target, indices) { DelayExceptions = true }, elementType));
					}
					if (method.Name == "Address") {
						var target = arguments[0];
						var indices = arguments.Skip(1).ToArray();
						return Push(new LdElema(elementType, target, indices));
					}
					Warn("Unknown method called on array type: " + method.Name);
					goto default;
				}
				case TypeKind.Struct when method.IsConstructor && !method.IsStatic && opCode == OpCode.Call
					&& method.ReturnType.Kind == TypeKind.Void:
				{
					// "call Struct.ctor(target, ...)" doesn't exist in C#,
					// the next best equivalent is an assignment `*target = new Struct(...);`.
					// So we represent this call as "stobj Struct(target, newobj Struct.ctor(...))".
					// This needs to happen early (not as a transform) because the StObj.TargetSlot has
					// restricted inlining (doesn't accept ldflda when exceptions aren't delayed).
					var newobj = new NewObj(method);
					newobj.ILStackWasEmpty = currentStack.IsEmpty;
					newobj.ConstrainedTo = constrainedPrefix;
					newobj.Arguments.AddRange(arguments.Skip(1));
					return new StObj(arguments[0], newobj, method.DeclaringType);
				}
				default:
					var call = CallInstruction.Create(opCode, method);
					call.ILStackWasEmpty = currentStack.IsEmpty;
					call.ConstrainedTo = constrainedPrefix;
					call.Arguments.AddRange(arguments);
					if (call.ResultType != StackType.Void)
						return Push(call);
					return call;
			}
		}

		ILInstruction DecodeCallIndirect()
		{
			var signatureHandle = (dnlib.DotNet.MethodSig)currentInstruction.Operand;
			var fpt = module.DecodeMethodSignature(signatureHandle, genericContext);
			var functionPointer = Pop(StackType.I);
			int firstArgument = signatureHandle.HasThis ? 1 : 0;
			var arguments = new ILInstruction[firstArgument + fpt.ParameterTypes.Length];
			for (int i = fpt.ParameterTypes.Length - 1; i >= 0; i--)
			{
				arguments[firstArgument + i] = Pop(fpt.ParameterTypes[i].GetStackType());
			}
			if (firstArgument == 1)
			{
				arguments[0] = Pop();
			}
			var call = new CallIndirect(
				signatureHandle.HasThis,
				signatureHandle.ExplicitThis,
				fpt,
				functionPointer,
				arguments
			);
			if (call.ResultType != StackType.Void)
				return Push(call);
			else
				return call;
		}

		static int GetPopCount(OpCode callCode, dnlib.DotNet.IMethod methodReference)
		{
			int popCount = methodReference.MethodSig.Params.Count;
			if (callCode != OpCode.NewObj && methodReference.MethodSig.HasThis)
				popCount++;
			return popCount;
		}

		ILInstruction Comparison(ComparisonKind kind, bool un = false)
		{
			var right = Pop();
			var left = Pop();

			if ((left.ResultType == StackType.O || left.ResultType == StackType.Ref) && right.ResultType.IsIntegerType())
			{
				// C++/CLI sometimes compares object references with integers.
				// Also happens with Ref==I in Unsafe.IsNullRef().
				if (right.ResultType == StackType.I4)
				{
					// ensure we compare at least native integer size
					right = new Conv(right, PrimitiveType.I, false, Sign.None);
				}
				left = new Conv(left, right.ResultType.ToPrimitiveType(), false, Sign.None);
			}
			else if ((right.ResultType == StackType.O || right.ResultType == StackType.Ref) && left.ResultType.IsIntegerType())
			{
				if (left.ResultType == StackType.I4)
				{
					left = new Conv(left, PrimitiveType.I, false, Sign.None);
				}
				right = new Conv(right, left.ResultType.ToPrimitiveType(), false, Sign.None);
			}

			// make implicit integer conversions explicit:
			MakeExplicitConversion(sourceType: StackType.I4, targetType: StackType.I, conversionType: PrimitiveType.I);
			MakeExplicitConversion(sourceType: StackType.I4, targetType: StackType.I8, conversionType: PrimitiveType.I8);
			MakeExplicitConversion(sourceType: StackType.I, targetType: StackType.I8, conversionType: PrimitiveType.I8);

			// Based on Table 4: Binary Comparison or Branch Operation
			if (left.ResultType.IsFloatType() && right.ResultType.IsFloatType()) {
				if (left.ResultType != right.ResultType) {
					// make the implicit F4->F8 conversion explicit:
					MakeExplicitConversion(StackType.F4, StackType.F8, PrimitiveType.R8);
				}
				if (un) {
					// for floats, 'un' means 'unordered'
					return Comp.LogicNot(new Comp(kind.Negate(), Sign.None, left, right));
				} else {
					return new Comp(kind, Sign.None, left, right);
				}
			} else if (left.ResultType.IsIntegerType() && right.ResultType.IsIntegerType() && !kind.IsEqualityOrInequality()) {
				// integer comparison where the sign matters
				Debug.Assert(right.ResultType.IsIntegerType());
				return new Comp(kind, un ? Sign.Unsigned : Sign.Signed, left, right);
			} else if (left.ResultType == right.ResultType) {
				// integer equality, object reference or managed reference comparison
				return new Comp(kind, Sign.None, left, right);
			} else {
				Warn($"Invalid comparison between {left.ResultType} and {right.ResultType}");
				if (left.ResultType < right.ResultType) {
					left = new Conv(left, right.ResultType.ToPrimitiveType(), false, Sign.Signed);
				} else {
					right = new Conv(right, left.ResultType.ToPrimitiveType(), false, Sign.Signed);
				}
				return new Comp(kind, Sign.None, left, right);
			}

			void MakeExplicitConversion(StackType sourceType, StackType targetType, PrimitiveType conversionType)
			{
				if (left.ResultType == sourceType && right.ResultType == targetType) {
					left = new Conv(left, conversionType, false, Sign.None);
				} else if (left.ResultType == targetType && right.ResultType == sourceType) {
					right = new Conv(right, conversionType, false, Sign.None);
				}
			}
		}

		int? DecodeBranchTarget(bool shortForm)
		{
			return (int?)((Instruction)currentInstruction.Operand)?.Offset;
		}

		ILInstruction DecodeComparisonBranch(bool shortForm, ComparisonKind kind, bool un = false)
		{
			int? target = DecodeBranchTarget(shortForm);
			var condition = Comparison(kind, un);
			condition.AddILRange(GetCurrentInstructionInterval());
			if (target != null) {
				MarkBranchTarget(target.Value);
				return new IfInstruction(condition, new Branch(target.Value));
			} else {
				return new IfInstruction(condition, new InvalidBranch("Invalid branch target"));
			}
		}

		ILInstruction DecodeConditionalBranch(bool shortForm, bool negate)
		{
			int? target = DecodeBranchTarget(shortForm);
			ILInstruction condition = Pop();
			switch (condition.ResultType) {
				case StackType.O:
					// introduce explicit comparison with null
					condition = new Comp(
						negate ? ComparisonKind.Equality : ComparisonKind.Inequality,
						Sign.None, condition, new LdNull());
					break;
				case StackType.I:
					// introduce explicit comparison with 0
					condition = new Comp(
						negate ? ComparisonKind.Equality : ComparisonKind.Inequality,
						Sign.None, condition, new Conv(new LdcI4(0), PrimitiveType.I, false, Sign.None));
					break;
				case StackType.I8:
					// introduce explicit comparison with 0
					condition = new Comp(
						negate ? ComparisonKind.Equality : ComparisonKind.Inequality,
						Sign.None, condition, new LdcI8(0));
					break;
				case StackType.Ref:
					// introduce explicit comparison with null ref
					condition = new Comp(
						negate ? ComparisonKind.Equality : ComparisonKind.Inequality,
						Sign.None, new Conv(condition, PrimitiveType.I, false, Sign.None), new Conv(new LdcI4(0), PrimitiveType.I, false, Sign.None));
					break;
				case StackType.I4:
					if (negate) {
						condition = Comp.LogicNot(condition);
					}
					break;
				default:
					condition = new Conv(condition, PrimitiveType.I4, false, Sign.None);
					if (negate) {
						condition = Comp.LogicNot(condition);
					}
					break;
			}
			if (target != null) {
				MarkBranchTarget(target.Value);
				return new IfInstruction(condition, new Branch(target.Value));
			} else {
				return new IfInstruction(condition, new InvalidBranch("Invalid branch target"));
			}
		}

		ILInstruction DecodeUnconditionalBranch(bool shortForm, bool isLeave = false)
		{
			int? target = DecodeBranchTarget(shortForm);
			if (isLeave) {
				currentStack = currentStack.Clear();
			}
			if (target != null) {
				MarkBranchTarget(target.Value);
				return new Branch(target.Value);
			} else {
				return new InvalidBranch("Invalid branch target");
			}
		}

		void MarkBranchTarget(int targetILOffset)
		{
			isBranchTarget[targetILOffset] = true;
			StoreStackForOffset(targetILOffset, ref currentStack);
		}

		ILInstruction DecodeSwitch()
		{
			var labels = (Instruction[])currentInstruction.Operand;
			var instr = new SwitchInstruction(Pop(StackType.I4));

			for (int i = 0; i < labels.Length; i++) {
				var section = new SwitchSection();
				section.Labels = new LongSet(i);
				int? target = (int?)labels[i]?.Offset;
				if (target != null) {
					MarkBranchTarget(target.Value);
					section.Body = new Branch(target.Value);
				} else {
					section.Body = new InvalidBranch("Invalid branch target");
				}
				instr.Sections.Add(section);
			}
			var defaultSection = new SwitchSection();
			defaultSection.Labels = new LongSet(new LongInterval(0, labels.Length)).Invert();
			defaultSection.Body = new Nop();
			instr.Sections.Add(defaultSection);
			return instr;
		}

		ILInstruction BinaryNumeric(BinaryNumericOperator @operator, bool checkForOverflow = false, Sign sign = Sign.None)
		{
			var right = Pop();
			var left = Pop();
			if (@operator != BinaryNumericOperator.Add && @operator != BinaryNumericOperator.Sub) {
				// we are treating all Refs as I, make the conversion explicit
				if (left.ResultType == StackType.Ref) {
					left = new Conv(left, PrimitiveType.I, false, Sign.None);
				}
				if (right.ResultType == StackType.Ref) {
					right = new Conv(right, PrimitiveType.I, false, Sign.None);
				}
			}
			if (@operator != BinaryNumericOperator.ShiftLeft && @operator != BinaryNumericOperator.ShiftRight) {
				// make the implicit I4->I conversion explicit:
				MakeExplicitConversion(sourceType: StackType.I4, targetType: StackType.I, conversionType: PrimitiveType.I);
				// I4->I8 conversion:
				MakeExplicitConversion(sourceType: StackType.I4, targetType: StackType.I8, conversionType: PrimitiveType.I8);
				// I->I8 conversion:
				MakeExplicitConversion(sourceType: StackType.I, targetType: StackType.I8, conversionType: PrimitiveType.I8);
				// F4->F8 conversion:
				MakeExplicitConversion(sourceType: StackType.F4, targetType: StackType.F8, conversionType: PrimitiveType.R8);
			}
			return Push(new BinaryNumericInstruction(@operator, left, right, checkForOverflow, sign));

			void MakeExplicitConversion(StackType sourceType, StackType targetType, PrimitiveType conversionType)
			{
				if (left.ResultType == sourceType && right.ResultType == targetType) {
					left = new Conv(left, conversionType, false, Sign.None);
				} else if (left.ResultType == targetType && right.ResultType == sourceType) {
					right = new Conv(right, conversionType, false, Sign.None);
				}
			}
		}

		ILInstruction DecodeJmp()
		{
			IMethod method = ReadAndDecodeMethodReference();
			// Translate jmp into tail call:
			Call call = new Call(method);
			call.IsTail = true;
			call.ILStackWasEmpty = true;
			if (!method.IsStatic) {
				call.Arguments.Add(Ldarg(0));
			}
			foreach (var p in method.Parameters) {
				call.Arguments.Add(Ldarg(call.Arguments.Count));
			}
			return new Leave(mainContainer, call);
		}

		ILInstruction LdToken(dnlib.DotNet.IMDTokenProvider token)
		{
			if (token is dnlib.DotNet.ITypeDefOrRef s) {
				return new LdTypeToken(module.ResolveType(s, genericContext));
			}
			return new LdMemberToken((IMember)module.ResolveEntity(token, genericContext));
		}
	}
}
