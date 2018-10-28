// analyzer: D2L.CodeStyle.Analyzers.ApiUsage.IDTObjectDatabaseConstructorsAnalyzer

namespace D2L.LP.LayeredArch.Data {
	public interface IDTObjectDatabase { };
	public abstract class Split { public sealed class Main : Split { } }
	public interface IDb<TSplit> : IDTObjectDatabase where TSplit : Split { }
}

namespace D2L.LP.Extensibility.Activation.Domain {
	public interface IFactory<out T> {
		T Create();
	}
}

namespace D2L.CodeStyle.Analyzers.IDTObjectDatabaseConstructors.Examples {

	using D2L.LP.LayeredArch.Data;
	using D2L.LP.Extensibility.Activation.Domain;

	public interface IFoo { }
	public sealed class Foo : IFoo {
		public Foo( /* ConstructorShouldTakeIDbT */ IDTObjectDatabase /**/ db ) { }
	}

	public sealed class FooFactory : IFactory<IFoo> {

		private readonly IDb<Split.Main> m_db;

		IFoo IFactory<IFoo>.Create() {
			return new Foo( m_db );
		}

	}

}
