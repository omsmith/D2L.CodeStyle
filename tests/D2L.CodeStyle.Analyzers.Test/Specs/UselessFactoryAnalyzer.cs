// analyzer: D2L.CodeStyle.Analyzers.ApiUsage.UselessFactoryAnalyzer

namespace D2L.LP.Extensibility.Activation.Domain {
	public interface IFactory<out T> {
		T Create();
	}
}

namespace D2L.CodeStyle.Analyzers.IDTObjectDatabaseConstructors.Examples {

	using D2L.LP.Extensibility.Activation.Domain;

	public interface IFoo { }
	public sealed class Foo : IFoo {
		public Foo( IBar bar ) { }
	}

	public sealed class CachedFoo : IFoo {
		public CachedFoo( IFoo innerFoo ) { }
	}

	public interface IBar { }
	public interface IBaz : IBar { }

	public sealed class Bar : IBar {
		public static readonly IBar Instance = new Bar();
	}

	public interface IBarFactory {
		IBar Create();
	}

	public interface IQuux { }
	public sealed class Quux : IQuux { }

	// Useless because there's no type conversion and it's only creating a single object
	public sealed class /* UselessFactory */ FooFactory /**/ : IFactory<IFoo> {

		private readonly IBar m_bar;

		public FooFactory(
			IBar bar
		) {
			m_bar = bar;
		}

		IFoo IFactory<IFoo>.Create() {
			return new Foo( m_bar );
		}

	}

	// Not useless because there's a conversion from IBaz to IBar in the new Foo()
	public sealed class FooFactory2 : IFactory<IFoo> {

		private readonly IBaz m_baz;

		public FooFactory2(
			IBaz baz
		) {
			m_baz = baz;
		}

		IFoo IFactory<IFoo>.Create() {
			return new Foo( m_baz );
		}

	}

	// Not useless because there's a conversion from IBaz to IBar in the field assignment
	public sealed class FooFactory3 : IFactory<IFoo> {

		private readonly IBar m_bar;

		public FooFactory3(
			IBaz baz
		) {
			m_bar = baz;
		}

		IFoo IFactory<IFoo>.Create() {
			return new Foo( m_bar );
		}

	}

	// Not useless because it's not just passing injections through
	public sealed class FooFactory4 : IFactory<IFoo> {

		IFoo IFactory<IFoo>.Create() {
			return new Foo( Bar.Instance );
		}

	}

	// Not useless because it's not just passing injections through
	public sealed class FooFactory4 : IFactory<IFoo> {

		private readonly IBarFactory m_barFactory;

		public FooFactory4(
			IBarFactory barFactory
		) {
			m_barFactory = barFactory;
		}

		IFoo IFactory<IFoo>.Create() {
			return new Foo( m_barFactory.Create() );
		}

	}

	// Not useless because it's not just passing injections through
	public sealed class FooFactory5 : IFactory<IFoo> {

		private readonly IBar m_bar;

		public FooFactory4(
			IBarFactory barFactory
		) {
			m_bar = barFactory.Create();
		}

		IFoo IFactory<IFoo>.Create() {
			return new Foo( m_bar );
		}

	}

	// Not useless because it's not just a single concrete type
	public sealed class FooFactory5 : IFactory<IFoo> {

		private readonly IBar m_bar;

		public FooFactory4(
			IBar bar
		) {
			m_bar = bar;
		}

		IFoo IFactory<IFoo>.Create() {
			return new CachedFoo( new Foo( m_bar ) );
		}

	}

	// Useless because there's no constructor on the factor and no parameters to the concrete thing
	public sealed class /* UselessFactory */ QuuxFactory /**/ : IFactory<IQuux> {
		IQuux IFactory<IQuux>.Create() {
			return new Quux();
		}
	}

}
