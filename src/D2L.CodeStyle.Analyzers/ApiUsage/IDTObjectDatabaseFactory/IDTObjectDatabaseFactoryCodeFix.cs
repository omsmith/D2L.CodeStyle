using System;
using System.Collections.Generic;
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

namespace D2L.CodeStyle.Analyzers.ApiUsage.IDTObjectDatabaseFactory {
	// BUG: if there are two partial declarations, each declaring some
	// implemented interfaces that have [Immutable] we may emit two, one for
	// each decl which would make this code fix insert multiple [Immutable]
	// attributes if the user does a "Fix All" in VS. This will break because
	// [Immutable] has AllowMultiple = false.
	//
	// To fix it maybe we could make this fixer find all decl syntaxes
	// and use some method to pick one (e.g. look at the BaseList syntax,
	// ToString() it and pick the lowest lexicographically) and only suggest a
	// fix on that decl. VS will take care of de-duping these fixes in a
	// "Fix All".

	[ExportCodeFixProvider(
		LanguageNames.CSharp,
		Name = nameof( IDTObjectDatabaseFactoryCodeFix )
	)]
	public sealed class IDTObjectDatabaseFactoryCodeFix : CodeFixProvider {
		public override ImmutableArray<string> FixableDiagnosticIds
			=> ImmutableArray.Create(
				Diagnostics.UseAnIDbTInstead.Id
			);

		public override FixAllProvider GetFixAllProvider() {
			return WellKnownFixAllProviders.BatchFixer;
		}

		public override async Task RegisterCodeFixesAsync(
			CodeFixContext context
		) {
			try {
				var root = await context.Document
					.GetSyntaxRootAsync( context.CancellationToken )
					.ConfigureAwait( false ) as CompilationUnitSyntax;

				SemanticModel model = await context
					.Document
					.GetSemanticModelAsync( context.CancellationToken )
					.ConfigureAwait( false );

				foreach( var diagnostic in context.Diagnostics ) {
					var invocationSpan = diagnostic.Location.SourceSpan;

					SyntaxNode invocationNode = root.FindNode( invocationSpan );
					InvocationExpressionSyntax invocation;
					switch( invocationNode ) {
						case InvocationExpressionSyntax invocationNodeExpression:
							invocation = invocationNodeExpression;
							break;
						case ArgumentSyntax argumentSyntax:
							invocation = argumentSyntax.Expression as InvocationExpressionSyntax;
							break;
						default:
							continue;
					}

					if( !IDTObjectDatabaseFactoryAnalyzer.TryGetDbType( model, invocation, out GenericNameSyntax dbTypeReference ) ) {
						continue;
					}

					DocumentEditor editor = await DocumentEditor
						.CreateAsync( context.Document, context.CancellationToken )
						.ConfigureAwait( false );



					ISymbol dbFactoryDeclarationSymbol = model
						.GetSymbolInfo( ( invocation.Expression as MemberAccessExpressionSyntax ).Expression )
						.Symbol;
					SyntaxNode dbFactoryDeclaration = ( await dbFactoryDeclarationSymbol
						.DeclaringSyntaxReferences[ 0 ]
						.GetSyntaxAsync( context.CancellationToken )
						.ConfigureAwait( false )
					);

					switch( dbFactoryDeclaration ) {
						case VariableDeclaratorSyntax dbFactoryVariableDeclaration:
							if( !await TryFixFieldReference( editor, invocation, dbTypeReference, dbFactoryDeclarationSymbol, dbFactoryVariableDeclaration, context.CancellationToken ).ConfigureAwait( false ) ) {
								continue;
							}
							break;
						case ParameterSyntax dbFactoryArgumentSyntax:
							if( !await TryFixParameter( editor, invocation, dbTypeReference, dbFactoryDeclarationSymbol, context.CancellationToken ).ConfigureAwait( false ) ) {
								continue;
							}
							break;
						default:
							continue;
							//throw new Exception( $"{ dbFactoryDeclaration.Kind() }   -  { dbFactoryDeclaration.SyntaxTree.GetLocation( dbFactoryDeclaration.Span ).GetLineSpan() }" );
					}
					context.RegisterCodeFix(
						CodeAction.Create(
							title: "Inject IDb<T>",
							createChangedDocument: ct => Task.FromResult( editor.GetChangedDocument() )
						),
						diagnostic
					);

				}
			} catch { throw; }
		}

		private static readonly IdentifierNameSyntax DbReference = SyntaxFactory.IdentifierName( "db" );
		private static readonly IdentifierNameSyntax MDbReference = SyntaxFactory.IdentifierName( "m_db" );
		private static readonly AssignmentExpressionSyntax MDbAssignment = SyntaxFactory
			.AssignmentExpression(
				SyntaxKind.SimpleAssignmentExpression,
				MDbReference,
				DbReference
			);

		private static MemberDeclarationSyntax MDbDeclartion( GenericNameSyntax dbTypeReference ) {
			return SyntaxFactory
				.FieldDeclaration(
					SyntaxFactory
						.VariableDeclaration( dbTypeReference )
						.WithTrailingTrivia( SyntaxFactory.Space )
						.WithVariables( SyntaxFactory.SeparatedList( new[] {
							SyntaxFactory.VariableDeclarator( "m_db" )
						} ) )
				)
				.WithModifiers( SyntaxFactory.TokenList(
					SyntaxFactory.Token( SyntaxKind.PrivateKeyword ).WithTrailingTrivia( SyntaxFactory.Space ),
					SyntaxFactory.Token( SyntaxKind.ReadOnlyKeyword ).WithTrailingTrivia( SyntaxFactory.Space )
				) );
		}

		private static ParameterSyntax DbInjection( GenericNameSyntax dbTypeReference ) {
			return SyntaxFactory
				.Parameter( SyntaxFactory.Identifier( "db" ) )
				.WithType( dbTypeReference );
		}

		private static async Task<bool> TryFixFieldReference(
			DocumentEditor editor,
			InvocationExpressionSyntax invocation,
			GenericNameSyntax dbTypeReference,
			ISymbol dbFactoryDeclarationSymbol,
			VariableDeclaratorSyntax dbFactoryVariableDeclaration,
			CancellationToken cancellationToken
		) {
			SyntaxNode dbFactoryDeclaration = dbFactoryVariableDeclaration
				.Parent
				.Parent;

			editor.ReplaceNode(
				dbFactoryDeclaration,
				MDbDeclartion( dbTypeReference ).WithLeadingTrivia( dbFactoryDeclaration.GetLeadingTrivia() )
			);

			if( !await TryReplaceConstructorInjection(
				editor, dbTypeReference, dbFactoryDeclarationSymbol, cancellationToken
			).ConfigureAwait( false ) ) {
				return false;
			}

			

			if( !TryGetConstructorOfContainingType( dbFactoryDeclarationSymbol, out IMethodSymbol constructor ) ) {
				return false;
			}

			SyntaxNode constructorSyntax = await constructor
				.DeclaringSyntaxReferences[ 0 ]
				.GetSyntaxAsync( cancellationToken )
				.ConfigureAwait( false );

			IEnumerable<AssignmentExpressionSyntax> assignments = constructorSyntax
				.DescendantNodes()
				.OfType<AssignmentExpressionSyntax>();

			ImmutableArray<AssignmentExpressionSyntax> dbAssignments = assignments
				.Where( x => x.Left is IdentifierNameSyntax )
				.Where( x => x.Right is IdentifierNameSyntax )
				.Where( x => ( x.Left as IdentifierNameSyntax ).Identifier.ValueText == dbFactoryVariableDeclaration.Identifier.ValueText )
				.ToImmutableArray();

			if( dbAssignments.Length != 1 ) {
				return false;
			}

			AssignmentExpressionSyntax dbAssignment = dbAssignments[ 0 ];

			editor.ReplaceNode(
				dbAssignment,
				MDbAssignment.WithTriviaFrom( dbAssignment )
			);

			editor.ReplaceNode( invocation, MDbReference );

			return true;
		}

		private static async Task<bool> TryFixParameter(
			DocumentEditor editor,
			InvocationExpressionSyntax invocation,
			GenericNameSyntax dbTypeReference,
			ISymbol dbFactoryDeclarationSymbol,
			CancellationToken cancellationToken
		) {
			if( !await TryReplaceConstructorInjection(
				editor, dbTypeReference, dbFactoryDeclarationSymbol, cancellationToken
			).ConfigureAwait( false ) ) {
				return false;
			}

			editor.ReplaceNode( invocation, DbReference );

			return true;
		}

		private static async Task<bool> TryReplaceConstructorInjection(
			DocumentEditor editor,
			GenericNameSyntax dbTypeReference,
			ISymbol dbFactoryDeclarationSymbol,
			CancellationToken cancellationToken
		) {
			if( !TryGetConstructorOfContainingType( dbFactoryDeclarationSymbol, out IMethodSymbol constructor ) ) {
				return false;
			}

			ImmutableArray<IParameterSymbol> dbFactoryParameters = constructor
				.Parameters
				.Where( p => p.Type.ToDisplayString() == "D2L.LP.LayeredArch.Data.IDTObjectDatabaseFactory" )
				.ToImmutableArray();

			if( dbFactoryParameters.Length != 1 ) {
				return false;
			}

			IParameterSymbol dbFactoryParameter = dbFactoryParameters[ 0 ];

			SyntaxNode dbFactoryInjection = await dbFactoryParameter
				.DeclaringSyntaxReferences[ 0 ]
				.GetSyntaxAsync( cancellationToken )
				.ConfigureAwait( false );

			editor.ReplaceNode(
				dbFactoryInjection,
				DbInjection( dbTypeReference )
					.WithTriviaFrom( dbFactoryInjection )
			);

			return true;
		}

		private static bool TryGetConstructorOfContainingType( ISymbol symbol, out IMethodSymbol constructorSymbol ) {
			ImmutableArray<IMethodSymbol> classConstructors = symbol
				.ContainingType
				.Constructors
				.Where( x => x.Parameters.Length != 0 )
				.ToImmutableArray();
			if( classConstructors.Length != 1 ) {
				constructorSymbol = null;
				return false;
			}

			constructorSymbol = classConstructors[ 0 ];
			return true;
		}
	}
}
