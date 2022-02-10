#nullable enable
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

using System;
using System.Linq;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using dnSpy.Contracts.Decompiler;
using dnSpy.Contracts.Text;

using ICSharpCode.Decompiler.IL.Patterns;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.IL
{
	[Flags]
	public enum CSharpArgumentInfoFlags
	{
		None = 0,
		UseCompileTimeType = 1,
		Constant = 2,
		NamedArgument = 4,
		IsRef = 8,
		IsOut = 0x10,
		IsStaticType = 0x20
	}

	[Flags]
	public enum CSharpBinderFlags
	{
		None = 0,
		CheckedContext = 1,
		InvokeSimpleName = 2,
		InvokeSpecialName = 4,
		BinaryOperationLogical = 8,
		ConvertExplicit = 0x10,
		ConvertArrayIndex = 0x20,
		ResultIndexed = 0x40,
		ValueFromCompoundAssignment = 0x80,
		ResultDiscarded = 0x100
	}

	public struct CSharpArgumentInfo
	{
		public string? Name { get; set; }
		public CSharpArgumentInfoFlags Flags { get; set; }
		public IType CompileTimeType { get; set; }

		public bool HasFlag(CSharpArgumentInfoFlags flag) => (Flags & flag) != 0;
	}

	partial class DynamicInstruction
	{
		public CSharpBinderFlags BinderFlags { get; }
		public IType? CallingContext { get; }

		protected DynamicInstruction(OpCode opCode, CSharpBinderFlags binderFlags, IType? context)
			: base(opCode)
		{
			BinderFlags = binderFlags;
			CallingContext = context;
		}

		protected void WriteBinderFlags(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteBinderFlags(BinderFlags, output, options);
		}

		internal static void WriteBinderFlags(CSharpBinderFlags flags, IDecompilerOutput output, ILAstWritingOptions options)
		{
			if ((flags & CSharpBinderFlags.BinaryOperationLogical) != 0)
				output.Write(".logic", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.CheckedContext) != 0)
				output.Write(".checked", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.ConvertArrayIndex) != 0)
				output.Write(".arrayindex", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.ConvertExplicit) != 0)
				output.Write(".explicit", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.InvokeSimpleName) != 0)
				output.Write(".invokesimple", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.InvokeSpecialName) != 0)
				output.Write(".invokespecial", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.ResultDiscarded) != 0)
				output.Write(".discard", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.ResultIndexed) != 0)
				output.Write(".resultindexed", BoxedTextColor.Text);
			if ((flags & CSharpBinderFlags.ValueFromCompoundAssignment) != 0)
				output.Write(".compound", BoxedTextColor.Text);
		}

		public abstract CSharpArgumentInfo GetArgumentInfoOfChild(int index);

		internal static void WriteArgumentList(IDecompilerOutput output, ILAstWritingOptions options, params (ILInstruction, CSharpArgumentInfo)[] arguments)
		{
			WriteArgumentList(output, options, (IEnumerable<(ILInstruction, CSharpArgumentInfo)>)arguments);
		}

		internal static void WriteArgumentList(IDecompilerOutput output, ILAstWritingOptions options, IEnumerable<(ILInstruction, CSharpArgumentInfo)> arguments)
		{
			output.Write("(", BoxedTextColor.Punctuation);
			int j = 0;
			foreach (var (arg, info) in arguments)
			{
				if (j > 0)
					output.Write(", ", BoxedTextColor.Text);
				output.Write("[flags: ", BoxedTextColor.Text);
				output.Write(info.Flags.ToString(), BoxedTextColor.Text);
				output.Write(", name: " + info.Name + "] ", BoxedTextColor.Text);
				arg.WriteTo(output, options);
				j++;
			}
			output.Write(")", BoxedTextColor.Punctuation);
		}
	}

	partial class DynamicConvertInstruction
	{
		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			type.WriteTo(output);
			output.Write("(", BoxedTextColor.Text);
			argument.WriteTo(output, options);
			output.Write(")", BoxedTextColor.Text);
		}

		public DynamicConvertInstruction(CSharpBinderFlags binderFlags, IType type, IType? context, ILInstruction argument)
			: base(OpCode.DynamicConvertInstruction, binderFlags, context)
		{
			this.type = type;
			Argument = argument;
		}

		protected internal override bool PerformMatch(ref ListMatch listMatch, ref Match match)
		{
			return base.PerformMatch(ref listMatch, ref match);
		}

		public override StackType ResultType => type.GetStackType();

		public bool IsChecked => (BinderFlags & CSharpBinderFlags.CheckedContext) != 0;

		public bool IsExplicit => (BinderFlags & CSharpBinderFlags.ConvertExplicit) != 0;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			return default(CSharpArgumentInfo);
		}
	}

	partial class DynamicInvokeMemberInstruction
	{
		public string Name { get; }
		public IReadOnlyList<IType> TypeArguments { get; }
		public IReadOnlyList<CSharpArgumentInfo> ArgumentInfo { get; }

		public DynamicInvokeMemberInstruction(CSharpBinderFlags binderFlags, string name, IType[]? typeArguments, IType? context, CSharpArgumentInfo[] argumentInfo, ILInstruction[] arguments)
			: base(OpCode.DynamicInvokeMemberInstruction, binderFlags, context)
		{
			Name = name;
			TypeArguments = typeArguments ?? Empty<IType>.Array;
			ArgumentInfo = argumentInfo;
			Arguments = new InstructionCollection<ILInstruction>(this, 0);
			Arguments.AddRange(arguments);
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write(Name, BoxedTextColor.Text);
			if (TypeArguments.Count > 0)
			{
				output.Write("<", BoxedTextColor.Text);
				int i = 0;
				foreach (var typeArg in TypeArguments)
				{
					if (i > 0)
						output.Write(", ", BoxedTextColor.Text);
					typeArg.WriteTo(output);
					i++;
				}
				output.Write(">", BoxedTextColor.Text);
			}
			WriteArgumentList(output, options, Arguments.Zip(ArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			if (index < 0 || index >= ArgumentInfo.Count)
				throw new ArgumentOutOfRangeException(nameof(index));
			return ArgumentInfo[index];
		}
	}

	partial class DynamicGetMemberInstruction
	{
		public string? Name { get; }
		public CSharpArgumentInfo TargetArgumentInfo { get; }

		public DynamicGetMemberInstruction(CSharpBinderFlags binderFlags, string? name, IType? context, CSharpArgumentInfo targetArgumentInfo, ILInstruction target)
			: base(OpCode.DynamicGetMemberInstruction, binderFlags, context)
		{
			Name = name;
			TargetArgumentInfo = targetArgumentInfo;
			Target = target;
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write(Name, BoxedTextColor.Text);
			WriteArgumentList(output, options, (Target, TargetArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			if (index != 0)
				throw new ArgumentOutOfRangeException(nameof(index));
			return TargetArgumentInfo;
		}
	}

	partial class DynamicSetMemberInstruction
	{
		public string? Name { get; }
		public CSharpArgumentInfo TargetArgumentInfo { get; }
		public CSharpArgumentInfo ValueArgumentInfo { get; }

		public DynamicSetMemberInstruction(CSharpBinderFlags binderFlags, string? name, IType? context, CSharpArgumentInfo targetArgumentInfo, ILInstruction target, CSharpArgumentInfo valueArgumentInfo, ILInstruction value)
			: base(OpCode.DynamicSetMemberInstruction, binderFlags, context)
		{
			Name = name;
			TargetArgumentInfo = targetArgumentInfo;
			Target = target;
			ValueArgumentInfo = valueArgumentInfo;
			Value = value;
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write(Name, BoxedTextColor.Text);
			WriteArgumentList(output, options, (Target, TargetArgumentInfo), (Value, ValueArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			switch (index)
			{
				case 0:
					return TargetArgumentInfo;
				case 1:
					return ValueArgumentInfo;
				default:
					throw new ArgumentOutOfRangeException(nameof(index));
			}
		}
	}

	partial class DynamicGetIndexInstruction
	{
		public IReadOnlyList<CSharpArgumentInfo> ArgumentInfo { get; }

		public DynamicGetIndexInstruction(CSharpBinderFlags binderFlags, IType? context, CSharpArgumentInfo[] argumentInfo, ILInstruction[] arguments)
			: base(OpCode.DynamicGetIndexInstruction, binderFlags, context)
		{
			ArgumentInfo = argumentInfo;
			Arguments = new InstructionCollection<ILInstruction>(this, 0);
			Arguments.AddRange(arguments);
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write("get_Item", BoxedTextColor.Text);
			WriteArgumentList(output, options, Arguments.Zip(ArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			if (index < 0 || index >= ArgumentInfo.Count)
				throw new ArgumentOutOfRangeException(nameof(index));
			return ArgumentInfo[index];
		}
	}

	partial class DynamicSetIndexInstruction
	{
		public IReadOnlyList<CSharpArgumentInfo> ArgumentInfo { get; }

		public DynamicSetIndexInstruction(CSharpBinderFlags binderFlags, IType? context, CSharpArgumentInfo[] argumentInfo, ILInstruction[] arguments)
			: base(OpCode.DynamicSetIndexInstruction, binderFlags, context)
		{
			ArgumentInfo = argumentInfo;
			Arguments = new InstructionCollection<ILInstruction>(this, 0);
			Arguments.AddRange(arguments);
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write("set_Item", BoxedTextColor.Text);
			WriteArgumentList(output, options, Arguments.Zip(ArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			if (index < 0 || index >= ArgumentInfo.Count)
				throw new ArgumentOutOfRangeException(nameof(index));
			return ArgumentInfo[index];
		}
	}

	partial class DynamicInvokeConstructorInstruction
	{
		readonly IType? resultType;

		public IReadOnlyList<CSharpArgumentInfo> ArgumentInfo { get; }

		public DynamicInvokeConstructorInstruction(CSharpBinderFlags binderFlags, IType? type, IType? context, CSharpArgumentInfo[] argumentInfo, ILInstruction[] arguments)
			: base(OpCode.DynamicInvokeConstructorInstruction, binderFlags, context)
		{
			ArgumentInfo = argumentInfo;
			Arguments = new InstructionCollection<ILInstruction>(this, 0);
			Arguments.AddRange(arguments);
			this.resultType = type;
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			resultType?.WriteTo(output);
			output.Write(".ctor", BoxedTextColor.Text);
			WriteArgumentList(output, options, Arguments.Zip(ArgumentInfo));
		}

		public override StackType ResultType => resultType?.GetStackType() ?? StackType.Unknown;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			if (index < 0 || index >= ArgumentInfo.Count)
				throw new ArgumentOutOfRangeException(nameof(index));
			return ArgumentInfo[index];
		}
	}

	partial class DynamicBinaryOperatorInstruction
	{
		public CSharpArgumentInfo LeftArgumentInfo { get; }
		public CSharpArgumentInfo RightArgumentInfo { get; }
		public ExpressionType Operation { get; }

		public DynamicBinaryOperatorInstruction(CSharpBinderFlags binderFlags, ExpressionType operation, IType? context, CSharpArgumentInfo leftArgumentInfo, ILInstruction left, CSharpArgumentInfo rightArgumentInfo, ILInstruction right)
			: base(OpCode.DynamicBinaryOperatorInstruction, binderFlags, context)
		{
			Operation = operation;
			LeftArgumentInfo = leftArgumentInfo;
			Left = left;
			RightArgumentInfo = rightArgumentInfo;
			Right = right;
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write(Operation.ToString(), BoxedTextColor.Text);
			WriteArgumentList(output, options, (Left, LeftArgumentInfo), (Right, RightArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			switch (index)
			{
				case 0:
					return LeftArgumentInfo;
				case 1:
					return RightArgumentInfo;
				default:
					throw new ArgumentOutOfRangeException(nameof(index));
			}
		}
	}

	partial class DynamicLogicOperatorInstruction
	{
		public CSharpArgumentInfo LeftArgumentInfo { get; }
		public CSharpArgumentInfo RightArgumentInfo { get; }
		public ExpressionType Operation { get; }

		public DynamicLogicOperatorInstruction(CSharpBinderFlags binderFlags, ExpressionType operation, IType? context, CSharpArgumentInfo leftArgumentInfo, ILInstruction left, CSharpArgumentInfo rightArgumentInfo, ILInstruction right)
			: base(OpCode.DynamicLogicOperatorInstruction, binderFlags, context)
		{
			Operation = operation;
			LeftArgumentInfo = leftArgumentInfo;
			Left = left;
			RightArgumentInfo = rightArgumentInfo;
			Right = right;
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write(Operation.ToString(), BoxedTextColor.Text);
			WriteArgumentList(output, options, (Left, LeftArgumentInfo), (Right, RightArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		protected override InstructionFlags ComputeFlags()
		{
			return DirectFlags | Left.Flags
				| SemanticHelper.CombineBranches(Right.Flags, InstructionFlags.None);
		}

		public override InstructionFlags DirectFlags => InstructionFlags.MayThrow | InstructionFlags.SideEffect | InstructionFlags.ControlFlow;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			switch (index)
			{
				case 0:
					return LeftArgumentInfo;
				case 1:
					return RightArgumentInfo;
				default:
					throw new ArgumentOutOfRangeException(nameof(index));
			}
		}
	}

	partial class DynamicUnaryOperatorInstruction
	{
		public CSharpArgumentInfo OperandArgumentInfo { get; }
		public ExpressionType Operation { get; }

		public DynamicUnaryOperatorInstruction(CSharpBinderFlags binderFlags, ExpressionType operation, IType? context, CSharpArgumentInfo operandArgumentInfo, ILInstruction operand)
			: base(OpCode.DynamicUnaryOperatorInstruction, binderFlags, context)
		{
			Operation = operation;
			OperandArgumentInfo = operandArgumentInfo;
			Operand = operand;
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write(Operation.ToString(), BoxedTextColor.Text);
			WriteArgumentList(output, options, (Operand, OperandArgumentInfo));
		}

		public override StackType ResultType {
			get {
				switch (Operation)
				{
					case ExpressionType.IsFalse:
					case ExpressionType.IsTrue:
						return StackType.I4; // bool
					default:
						return StackType.O;
				}
			}
		}

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			switch (index)
			{
				case 0:
					return OperandArgumentInfo;
				default:
					throw new ArgumentOutOfRangeException(nameof(index));
			}
		}
	}

	partial class DynamicInvokeInstruction
	{
		public IReadOnlyList<CSharpArgumentInfo> ArgumentInfo { get; }

		public DynamicInvokeInstruction(CSharpBinderFlags binderFlags, IType? context, CSharpArgumentInfo[] argumentInfo, ILInstruction[] arguments)
			: base(OpCode.DynamicInvokeInstruction, binderFlags, context)
		{
			ArgumentInfo = argumentInfo;
			Arguments = new InstructionCollection<ILInstruction>(this, 0);
			Arguments.AddRange(arguments);
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			WriteArgumentList(output, options, Arguments.Zip(ArgumentInfo));
		}

		public override StackType ResultType => StackType.O;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			if (index < 0 || index >= ArgumentInfo.Count)
				throw new ArgumentOutOfRangeException(nameof(index));
			return ArgumentInfo[index];
		}
	}

	partial class DynamicIsEventInstruction
	{
		public string? Name { get; }

		public DynamicIsEventInstruction(CSharpBinderFlags binderFlags, string? name, IType? context, ILInstruction argument)
			: base(OpCode.DynamicIsEventInstruction, binderFlags, context)
		{
			Name = name;
			Argument = argument;
		}

		public override void WriteTo(IDecompilerOutput output, ILAstWritingOptions options)
		{
			WriteILRange(output, options);
			output.Write(OpCode);
			WriteBinderFlags(output, options);
			output.Write(" ", BoxedTextColor.Text);
			output.Write("(", BoxedTextColor.Text);
			Argument.WriteTo(output, options);
			output.Write(")", BoxedTextColor.Text);
		}

		public override StackType ResultType => StackType.I4;

		public override CSharpArgumentInfo GetArgumentInfoOfChild(int index)
		{
			return default(CSharpArgumentInfo);
		}
	}
}
