using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace D2L.CodeStyle.Analyzers.ApiUsage {
	[DiagnosticAnalyzer( LanguageNames.CSharp )]
	internal sealed class UselessFactoryAnalyzer : DiagnosticAnalyzer {

		internal const string CONCRETE_TYPE_NAME_PROP = "ConcreteTypeName";

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
			=> ImmutableArray.Create( Diagnostics.UselessFactory );

		public override void Initialize( AnalysisContext context ) {
			context.EnableConcurrentExecution();
			context.RegisterCompilationStartAction( RegisterAnalyzer );
		}

		public static void RegisterAnalyzer( CompilationStartAnalysisContext context ) {
			// Cache some important type lookups
			var factoryType = context.Compilation.GetTypeByMetadataName( "D2L.LP.Extensibility.Activation.Domain.IFactory`1" );

			if( factoryType == null || factoryType.Kind == SymbolKind.ErrorType ) {
				return;
			}

			context.RegisterSyntaxNodeAction(
				ctx => Analyze(
					ctx,
					factoryType: factoryType
				),
				SyntaxKind.ClassDeclaration
			);
		}

		private static void Analyze(
			SyntaxNodeAnalysisContext context,
			INamedTypeSymbol factoryType
		) {
			var classDeclartion = context.Node as ClassDeclarationSyntax;

			if( classDeclartion == null ) {
				return;
			}

			if( !IsFactory( context.SemanticModel, factoryType, classDeclartion ) ) {
				return;
			}

			if( IsPublic( context.SemanticModel, classDeclartion ) ) {
				return;
			}

			if( !IsOnlyDeclaration( classDeclartion ) ) {
				return;
			}

			if( HasInvocations( classDeclartion ) ) {
				return;
			}

			if( HasMemberAccesses( classDeclartion ) ) {
				return;
			}

			if( !TryGetSingleDescendentNode( classDeclartion, out ObjectCreationExpressionSyntax creation ) ) {
				return;
			}

			ImmutableArray<ExpressionSyntax> argExpressions = creation.ArgumentList.Arguments.Select( x => x.Expression ).ToImmutableArray();
			if( HasConversions( context.SemanticModel, argExpressions ) ) {
				return;
			}

			if( argExpressions.Any() ) {
				if( !TryGetSingleDescendentNode( classDeclartion, out ConstructorDeclarationSyntax constructor ) ) {
					return;
				}

				if( constructor.ParameterList.Parameters.Count != creation.ArgumentList.Arguments.Count ) {
					return;
				}
			}

			ImmutableArray<ExpressionSyntax> rhsOfAssigments = GetAssignmentRhsExpressions( classDeclartion ).ToImmutableArray();
			if( HasConversions( context.SemanticModel, rhsOfAssigments ) ) {
				return;
			}

			Location location = classDeclartion.Identifier.GetLocation();

			ITypeSymbol createdType = context.SemanticModel.GetTypeInfo( creation ).Type;
			if( createdType == null ) {
				context.ReportDiagnostic(
					Diagnostic.Create(
						Diagnostics.UselessFactory,
						location
					)
				);
				return;
			}

			context.ReportDiagnostic(
				Diagnostic.Create(
					Diagnostics.UselessFactory,
					location,
					properties: ImmutableDictionary
						.Create<string, string>()
						.Add( CONCRETE_TYPE_NAME_PROP, createdType.ToDisplayString() )
				)
			);
		}

		private static bool IsFactory(
			SemanticModel model,
			INamedTypeSymbol factoryType,
			ClassDeclarationSyntax classDeclaration
		) {
			INamedTypeSymbol classType = model.GetDeclaredSymbol( classDeclaration );

			if( classType == null ) {
				return false;
			}

			if( !classType.AllInterfaces.Any( x => x.OriginalDefinition == factoryType ) ) {
				return false;
			}

			return true;
		}

		private static bool IsPublic( SemanticModel model, ClassDeclarationSyntax classDeclaration ) {
			INamedTypeSymbol classType = model.GetDeclaredSymbol( classDeclaration );

			if( classType.DeclaredAccessibility == Accessibility.Public ) {
				return true;
			}

			return false;
		}

		private static bool IsOnlyDeclaration( ClassDeclarationSyntax classDeclaration ) {
			SyntaxTree tree = classDeclaration.SyntaxTree;

			IEnumerable<TypeDeclarationSyntax> declarations = tree.GetRoot().DescendantNodes().OfType<TypeDeclarationSyntax>();

			if( declarations.Count() > 1 ) {
				return false;
			}

			return true;
		}

		private static bool HasInvocations( SyntaxNode node ) {
			IEnumerable<InvocationExpressionSyntax> invocations = node
				.DescendantNodes()
				.OfType<InvocationExpressionSyntax>();

			if( invocations.Any() ) {
				return true;
			}

			return false;
		}

		private static bool HasMemberAccesses( SyntaxNode node ) {
			IEnumerable<MemberAccessExpressionSyntax> memberAccesses = node
				.DescendantNodes()
				.OfType<MemberAccessExpressionSyntax>();

			if( memberAccesses.Any() ) {
				return true;
			}

			return false;
		}

		private static bool HasConversions( SemanticModel model, IEnumerable<ExpressionSyntax> expressions ) {
			foreach( ExpressionSyntax expression in expressions ) {
				TypeInfo typeInfo = model.GetTypeInfo( expression );

				if( typeInfo.Type == null ) {
					return true;
				}

				if( typeInfo.Type != typeInfo.ConvertedType ) {
					return true;
				}
			}

			return false;
		}

		private static bool TryGetSingleDescendentNode<T>( SyntaxNode root, out T descendent ) where T : SyntaxNode {
			ImmutableArray<T> descendentsOfT = root
				.DescendantNodes()
				.OfType<T>()
				.ToImmutableArray();

			if( descendentsOfT.Length != 1 ) {
				descendent = null;
				return false;
			}

			descendent = descendentsOfT[ 0 ];
			return true;
		}

		private static IEnumerable<ExpressionSyntax> GetAssignmentRhsExpressions( SyntaxNode root ) {
			return root
				.DescendantNodes()
				.OfType<AssignmentExpressionSyntax>()
				.Select( x => x.Right );
		}
	}
}