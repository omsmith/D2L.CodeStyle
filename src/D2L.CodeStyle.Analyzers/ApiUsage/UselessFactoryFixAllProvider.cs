using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;

namespace D2L.CodeStyle.Analyzers.ApiUsage {

	internal sealed class UselessFactoryFixAllProvider : FixAllProvider {

		public static readonly FixAllProvider Instance = new UselessFactoryFixAllProvider();

		private UselessFactoryFixAllProvider() { }

		public override async Task<CodeAction> GetFixAsync( FixAllContext fixAllContext ) {
			CancellationToken ct = fixAllContext.CancellationToken;

			ImmutableArray<DocumentId> documentsToRemove = await GatherRemovedDocumentsAsync( fixAllContext, ct ).ConfigureAwait( false );

			CodeAction batchFixerAction = await WellKnownFixAllProviders.BatchFixer.GetFixAsync( fixAllContext ).ConfigureAwait( false );
			Solution batchFixerSolution = await GetChangedSolutionAsync( batchFixerAction, ct ).ConfigureAwait( false );

			Solution sln = batchFixerSolution;
			foreach( DocumentId doc in documentsToRemove ) {
				sln = sln.RemoveDocument( doc );
			}

			return CodeAction.Create(
				title: "Remove useless factory",
				createChangedSolution: _ => Task.FromResult( sln )
			);
		}

		private static async Task<ImmutableArray<DocumentId>> GatherRemovedDocumentsAsync( FixAllContext fixAllContext, CancellationToken ct ) {
			ImmutableDictionary<Project, ImmutableArray<Diagnostic>> projectDiagnosticMap = await GatherDiagnsoticsAsync( fixAllContext, ct ).ConfigureAwait( false );
			ImmutableDictionary<Document, ImmutableArray<Diagnostic>> documentDiagnosticMap = await ProjectDiagnosticsMapToDocumentDiagnosticsMapAsync( projectDiagnosticMap, ct ).ConfigureAwait( false );

			return await GatherRemovedDocumentsAsync( fixAllContext, documentDiagnosticMap, ct ).ConfigureAwait( false );
		}

		private static async Task<ImmutableDictionary<Project, ImmutableArray<Diagnostic>>> GatherDiagnsoticsAsync(
			FixAllContext fixAllContext,
			CancellationToken ct
		) {
			var builder = ImmutableDictionary.CreateBuilder<Project, ImmutableArray<Diagnostic>>();

			Solution sln = fixAllContext.Solution;
			IEnumerable<Project> projects = sln.Projects;

			var gatherDiagnosticsTasks = projects
				.Where( p => p.Language == fixAllContext.Project.Language )
				.Select( async p => (
					 Project: p,
					 Diagnostics: await fixAllContext.GetAllDiagnosticsAsync( p ).ConfigureAwait( false )
				 ) )
				.ToImmutableArray();

			var gatheredDiagnostics = await Task.WhenAll( gatherDiagnosticsTasks ).ConfigureAwait( false );

			foreach( var x in gatheredDiagnostics ) {
				builder[ x.Project ] = x.Diagnostics;
			}

			return builder.ToImmutable();
		}

		private static async Task<ImmutableDictionary<Document, ImmutableArray<Diagnostic>>> ProjectDiagnosticsMapToDocumentDiagnosticsMapAsync(
			ImmutableDictionary<Project, ImmutableArray<Diagnostic>> projectDiagnosticsMap,
			CancellationToken ct
		) {
			var documentDiagTasks = projectDiagnosticsMap
				.Select( async kvp => {
					ImmutableDictionary<SyntaxTree, Document> treeToDocumentMap = await GetTreeToDocumentMapAsync( kvp.Key, ct ).ConfigureAwait( false );
					return kvp.Value
						.Where( diag => diag.Location.SourceTree != null )
						.Select( diag => (treeToDocumentMap[ diag.Location.SourceTree ], diag) );
				} )
				.ToArray();

			var documentDiags = await Task.WhenAll( documentDiagTasks ).ConfigureAwait( false );

			return documentDiags
				.SelectMany( x => x )
				.GroupBy( x => x.Item1, x => x.diag )
				.ToImmutableDictionary( x => x.Key, x => x.ToImmutableArray() );
		}

		private static async Task<ImmutableDictionary<SyntaxTree, Document>> GetTreeToDocumentMapAsync(
			Project project,
			CancellationToken ct
		) {
			ConcurrentDictionary<SyntaxTree, Document> map = new ConcurrentDictionary<SyntaxTree, Document>();

			var tasks = project
				.Documents
				.Select( async d => {
					SyntaxTree tree = await d.GetSyntaxTreeAsync( ct ).ConfigureAwait( false );
					map.TryAdd( tree, d );
				} )
				.ToArray();

			await Task.WhenAll( tasks ).ConfigureAwait( false );

			return map.ToImmutableDictionary();
		}

		private static async Task<ImmutableArray<DocumentId>> GatherRemovedDocumentsAsync(
			FixAllContext fixAllContext,
			ImmutableDictionary<Document, ImmutableArray<Diagnostic>> documentDiagnosticsMap,
			CancellationToken ct
		) {
			ConcurrentBag<(Diagnostic Diagnostic, CodeAction Action)> fixBag = new ConcurrentBag<(Diagnostic diagnostic, CodeAction action)>();
			void RegisterCodeFix( CodeAction action, ImmutableArray<Diagnostic> diagnostics ) {
				fixBag.Add( (diagnostics.First(), action) );
			}

			var gatherCodeFixesTasks = documentDiagnosticsMap
				.Select( async x => await GatherCodeFixesAsync( fixAllContext, x.Key, x.Value, RegisterCodeFix, ct ).ConfigureAwait( false ) )
				.ToImmutableArray();

			await Task.WhenAll( gatherCodeFixesTasks ).ConfigureAwait( false );

			var gatherRemovedDocumentsTasks = fixBag
				.Select( x => x.Action )
				.Select( async a => await GatherRemovedDocumentsAsync( fixAllContext, a, ct ).ConfigureAwait( false ) )
				.ToImmutableArray();

			ImmutableArray<DocumentId> removedDocuments = ( await Task.WhenAll( gatherRemovedDocumentsTasks ).ConfigureAwait( false ) ).SelectMany( x => x ).ToImmutableArray();
			return removedDocuments;
		}

		private static async Task<ImmutableArray<DocumentId>> GatherRemovedDocumentsAsync(
			FixAllContext fixAllContext,
			CodeAction codeAction,
			CancellationToken ct
		) {
			Solution changedSolution = await GetChangedSolutionAsync( codeAction, ct ).ConfigureAwait( false );
			SolutionChanges solutionChanges = changedSolution.GetChanges( fixAllContext.Solution );

			return solutionChanges
				.GetProjectChanges()
				.SelectMany( pc => pc.GetRemovedDocuments() )
				.ToImmutableArray();
		}

		private static async Task GatherCodeFixesAsync(
			FixAllContext fixAllContext,
			Document document,
			ImmutableArray<Diagnostic> diagnostics,
			Action<CodeAction, ImmutableArray<Diagnostic>> registerCodeFix,
			CancellationToken ct
		) {
			var registerCodeFixesTasks = diagnostics
				.Select( d => new CodeFixContext( document, d, registerCodeFix, ct ) )
				.Select( async c => await fixAllContext.CodeFixProvider.RegisterCodeFixesAsync( c ).ConfigureAwait( false ) )
				.ToImmutableArray();

			await Task.WhenAll( registerCodeFixesTasks ).ConfigureAwait( false );
		}

		private static Task<Solution> GetChangedSolutionAsync(
			CodeAction action,
			CancellationToken ct
		) {
			MethodInfo getChangedSolution = typeof( CodeAction ).GetRuntimeMethod(
				name: "GetChangedSolutionAsync",
				parameters: new[] { typeof( CancellationToken ) }
			);

			object result = getChangedSolution.Invoke( action, new object[] { ct } );

			Task<Solution> changedSolutionTask = ( Task<Solution> )result;

			return changedSolutionTask;
		}
	}
}