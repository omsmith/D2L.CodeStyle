using Microsoft.CodeAnalysis;

namespace D2L.CodeStyle.TestAnalyzers.Common {
	public static class Diagnostics {
		public static readonly DiagnosticDescriptor TestCaseSourceStrings = new DiagnosticDescriptor(
			id: "D2LTESTS001",
			title: "Use nameof in TestCaseSource attributes.",
			messageFormat: "String arguments in TestCaseSource not allowed. Use nameof( {0} ) instead.",
			category: "Cleanliness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true,
			description: "Using strings in TestCaseSource attributes creates false positives during dead code analysis. nameof should be used instead."
		);

		public static readonly DiagnosticDescriptor ValueSourceStrings = new DiagnosticDescriptor(
			id: "D2LTESTS002",
			title: "Use nameof in ValueSource attributes.",
			messageFormat: "String arguments in ValueSource not allowed. Use nameof( {0} ) instead.",
			category: "Cleanliness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true,
			description: "Using strings in ValueSource attributes creates false positives during dead code analysis. nameof should be used instead."
		);

		public static readonly DiagnosticDescriptor ConfigTestSetupStrings = new DiagnosticDescriptor(
			id: "D2LTESTS003",
			title: "Use nameof in ConfigTestSetup attributes.",
			messageFormat: "String arguments in ConfigTestSetup are not allowed. Use nameof({0}) instead.",
			category: "Cleanliness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true,
			description: "Using strings in ConfigTestSetup attributes creates false positives during dead code analysis. nameof should be used instead."
		);

		public static readonly DiagnosticDescriptor NUnitCategory = new DiagnosticDescriptor(
			id: "D2LTESTS004",
			title: "Test is incorrectly categorized",
			messageFormat: "Test is incorrectly categorized: {0}",
			category: "Correctness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true,
			description: "Tests need to be correctly categorized in order to be run."
		);

		public static readonly DiagnosticDescriptor CustomServiceLocator = new DiagnosticDescriptor(
			id: "D2LTESTS005",
			title: "Use the default test service locator.",
			messageFormat: "Custom service locators should not be used. Use static TestServiceLocator.Get<T>() or TestServiceLocatorFactory.Default instead.",
			category: "Cleanliness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true,
			description: "Custom service locators are expensive to instantiate and slow down tests significantly. Use the default locator instead."
		);

		public static readonly DiagnosticDescriptor UseAnIDbTInstead = new DiagnosticDescriptor(
			id: "D2LTESTXX01",
			title: "Should inject IDb<T>",
			description: "Should inject IDb<T>",
			messageFormat: "Should inject IDb<T>",
			category: "Correctness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true
		);

		public static readonly DiagnosticDescriptor UselessDbAssignment = new DiagnosticDescriptor(
			id: "D2LTESTXX02",
			title: "Useless DB Assignment",
			description: "Useless DB Assignment",
			messageFormat: "Useless DB Assignment",
			category: "Correctness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true
		);

		public static readonly DiagnosticDescriptor ConstructorShouldTakeIDbT = new DiagnosticDescriptor(
			id: "D2LTESTXX03",
			title: "Constructor should take IDb<T>",
			description: "Constructor should take IDb<T>",
			messageFormat: "Constructor should take IDb<T>",
			category: "Correctness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true
		);

		public static readonly DiagnosticDescriptor DbAssignment = new DiagnosticDescriptor(
			id: "D2LTESTXX04",
			title: "Update DB Assignment Type",
			description: "Update DB Assignment Type",
			messageFormat: "Update DB Assignment Type",
			category: "Correctness",
			defaultSeverity: DiagnosticSeverity.Error,
			isEnabledByDefault: true
	}
}
