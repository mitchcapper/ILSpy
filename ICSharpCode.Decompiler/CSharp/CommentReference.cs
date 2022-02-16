using System.Collections.Generic;
using System.Text;

namespace ICSharpCode.Decompiler.CSharp
{
	public struct CommentReference {
		public readonly int Length;
		public readonly object Reference;
		public readonly bool IsLocal;

		public CommentReference(int len, object @ref, bool isLocal = false) {
			this.Length = len;
			this.Reference = @ref;
			this.IsLocal = isLocal;
		}
	}

	public sealed class CommentReferencesCreator {
		readonly List<CommentReference> refs;
		readonly StringBuilder sb;

		public CommentReference[] CommentReferences {
			get { return refs.ToArray(); }
		}

		public string Text {
			get { return sb.ToString(); }
		}

		public CommentReferencesCreator(StringBuilder sb) {
			this.refs = new List<CommentReference>();
			this.sb = sb;
			this.sb.Clear();
		}

		public void AddText(string text) {
			Add(text, null, false);
		}

		public void AddReference(string text, object @ref, bool isLocal = false) {
			Add(text, @ref, isLocal);
		}

		void Add(string s, object @ref, bool isLocal) {
			refs.Add(new CommentReference(s.Length, @ref, isLocal));
			sb.Append(s);
		}
	}
}
