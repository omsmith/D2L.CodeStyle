using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Text;

namespace D2L.CodeStyle.Analyzers.ApiUsage {

	[ExportCodeFixProvider(
		LanguageNames.CSharp,
		Name = nameof( IDTObjectDatabaseConstructorsCodefix )
	)]
	public sealed class IDTObjectDatabaseConstructorsCodefix : CodeFixProvider {
		public override ImmutableArray<string> FixableDiagnosticIds
			=> ImmutableArray.Create(
				Diagnostics.ConstructorShouldTakeIDbT.Id
			);

		public override FixAllProvider GetFixAllProvider() {
			return WellKnownFixAllProviders.BatchFixer;
		}

		public override async Task RegisterCodeFixesAsync(
			CodeFixContext context
		) {
			var root = await context.Document
				.GetSyntaxRootAsync( context.CancellationToken )
				.ConfigureAwait( false ) as CompilationUnitSyntax;

			foreach( var diagnostic in context.Diagnostics ) {
				var parameterSpan = diagnostic.Location.SourceSpan;

				var parameterType = root.FindNode( parameterSpan ) as TypeSyntax;
				string dbTypeName = diagnostic.Properties[ IDTObjectDatabaseConstructorsAnalyzer.PROP_DB_TYPE ];

				context.RegisterCodeFix(
					CodeAction.Create(
						title: "Replace with IDb<T>",
						createChangedDocument: ct => Fix(
							context.Document,
							root,
							parameterType,
							dbTypeName,
							ct
						)
					),
					diagnostic
				);
			}
		}

		private static async Task<Document> Fix(
			Document orig,
			CompilationUnitSyntax root,
			TypeSyntax parameterType,
			string dbTypeName,
			CancellationToken ct
		) {
			SemanticModel model = await orig.GetSemanticModelAsync( ct ).ConfigureAwait( false );

			dbTypeName = dbTypeName.Replace( "D2L.LP.LayeredArch.Data.", "" );

			IdentifierNameSyntax dbTypeNameSyntax = SyntaxFactory.IdentifierName( dbTypeName ).WithTriviaFrom( parameterType );
			root = root.ReplaceNode( parameterType, dbTypeNameSyntax );

			if( !root.Usings.Any( IsLayeredArchUsing ) ) {
				root = root.AddUsings( LayeredArchUsing );
			}

			return orig.WithSyntaxRoot( root );
		}

		private static bool IsLayeredArchUsing( UsingDirectiveSyntax u ) {
			if( u.Name.ToString() != "D2L.LP.LayeredArch.Data" ) {
				return false;
			}

			return true;
		}

		private static readonly UsingDirectiveSyntax LayeredArchUsing =
			SyntaxFactory.UsingDirective(
				SyntaxFactory.ParseName( "D2L.LP.LayeredArch.Data" )
			);

	}
}
