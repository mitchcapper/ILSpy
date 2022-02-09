using System.Text;

namespace ICSharpCode.Decompiler
{
	internal static class StringBuilderExtensions
	{
		/// <summary>
		/// Compares <paramref name="sb"/> with <paramref name="s"/>. No string is created.
		/// </summary>
		/// <param name="sb">This</param>
		/// <param name="s">String</param>
		/// <returns></returns>
		public static bool CheckEquals(this StringBuilder sb, string s) {
			if (s == null || sb.Length != s.Length)
				return false;
			for (int i = 0; i < s.Length; i++) {
				if (sb[i] != s[i])
					return false;
			}
			return true;
		}

		public static bool StartsWith(this string s, StringBuilder sb) {
			int sbLen = sb.Length;
			if (s.Length < sbLen)
				return false;
			for (int i = 0; i < sbLen; i++) {
				if (sb[i] != s[i])
					return false;
			}
			return true;
		}

		public static bool EndsWith(this StringBuilder sb, string s) {
			int sbLen = sb.Length;
			if (sbLen < s.Length)
				return false;
			for (int i = 0, j = sbLen - s.Length; i < s.Length; i++, j++) {
				if (sb[j] != s[i])
					return false;
			}
			return true;
		}
	}
}
