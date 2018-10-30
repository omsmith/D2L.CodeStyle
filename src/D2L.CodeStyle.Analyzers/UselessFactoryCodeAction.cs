using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;

namespace D2L.CodeStyle.Analyzers {
	internal sealed class UselessFactoryCodeAction : CodeAction {

		private readonly Func<CancellationToken, Task<Solution>> m_createChangedSolution;

		public UselessFactoryCodeAction(
			string title,
			Func<CancellationToken, Task<Solution>> createChangedSolution,
			string equivalenceKey = null
		) {
			Title = title;
			EquivalenceKey = equivalenceKey;

			m_createChangedSolution = createChangedSolution;
		}

		public sealed override string Title { get; }
		public sealed override string EquivalenceKey { get; }

		protected override Task<Solution> GetChangedSolutionAsync( CancellationToken cancellationToken ) {
			return m_createChangedSolution( cancellationToken );
		}

		internal Task<Solution> GetChangedSolutionInternalAsync( CancellationToken ct ) {
			return GetChangedSolutionAsync( ct );
		}

	}
}
