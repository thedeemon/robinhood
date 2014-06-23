module main;
import std.stdio, std.traits, std.range, std.algorithm, std.conv, std.typetuple, std.random, std.exception;
import rbhash, std.array, std.datetime, core.time;
import vibe.utils.hashmap;

class DiffValues(K) : Exception {
	K key;
	this(K k) { super("different values"); key = k; }
}

bool equalHashes(H1, H2)(ref H1 h1, ref H2 h2) {
	if (h1.length != h2.length) { 
		writeln("sizes differ! ", h1.length, " vs. ", h2.length);
		return false;
	}
	foreach(k,v; h1) {
        alias K = Unqual!(typeof(k));
		auto p = cast(K)k in h2;
		if (p is null) { 
			writeln(k, " not found in h2");
			return false;
		}
		if (*p != v) { 
			writeln(k, " has different values: ", v, " vs. ", *p);
			//alias K = typeof(k);
			throw new DiffValues!K(cast(K)k);
			return false;
		}
	}
	return true;
}

enum Action { Add, Remove }

struct Command(K,V) {
	Action action;
	K key;
	V value;
}

void fillRandom(T)(ref T x) { x = gen!T; }

T gen(T)() { // generate a random value
	static if (is(T==bool))	           return uniform(0,2)==0;
	else static if (is(T==Action))	   return uniform(0,2)==0 ? Action.Add : Action.Remove;
	else static if (isFloatingPoint!T) return uniform(-10000.0, 10000.0);
	else static if (isScalarType!T)	   return uniform(T.min, T.max);
	else static if (is(T==string)) {
		auto n = uniform(1, 20);
		char[] s = new char[n];
		foreach(i; 0..n) s[i] = gen!char;
		return cast(string)s;
	} else
	static if (is(T==struct)) {
		T s;
		foreach(m; __traits(allMembers, T))
            static if (isAssignable!(typeof(__traits(getMember, s, m))))
			    fillRandom(__traits(getMember, s, m));
		return s;
	} else 
	static if (is(T==class)) {
		T x = new T;
		foreach(m; __traits(allMembers, T))
			static if (m != "Monitor" && isAssignable!(typeof(__traits(getMember, x, m))))
				fillRandom(__traits(getMember, x, m));
		return x;
	}
}

T gen2(T)() {
    static if (is(T==struct) || is(T==class)) return gen!T;
    else
    while(true) {
        T x = gen!T;
        if (x != T.init) return x;
    }
}

Command!(K,V)[] makeProgram(K, V)(size_t num) {
	auto prog = appender!(Command!(K,V)[]);
	auto ks = appender!(K[]);
	prog.reserve(num);
    ks.reserve(num / 2);
	while(prog.data.length < num) {
		auto k = uniform(1, num > 200 ? 100 : num/2);
		auto action = gen!Action;
		auto nks = ks.data.length;
        if (nks==0) action = Action.Add;
		void addSome(bool rememberKeys) {
			foreach(i; 0..k) {
				K key;
				if (/*!rememberKeys ||*/ (uniform(0, 10) < 4 && nks > 0)) // !rememberKeys to always remove existing keys, for vibe's HashMap
					key = ks.data[uniform(0, nks)];
				else
					key = gen2!K; // don't generate T.init, as vibe's HashMap cannot take those
				prog ~= Command!(K,V)(action, key, gen!V);
				if (rememberKeys && uniform(0, 10) < 4)
					ks ~= key;
			}

		}
		final switch(action) {
			case Action.Add:    addSome(true);	break;
			case Action.Remove:	addSome(false); break;
		}
	}    
	return prog.data;
}

void testHashes(K, V, H1, H2)(H1 delegate() make1, H2 delegate() make2, size_t num, string name1, string name2) {
  start:
	auto h1 = make1();
	auto h2 = make2();
	enforce(equalHashes(h1, h2));

	//generate program 
	auto prg = makeProgram!(K, V)(num);
	writeln("testing with prg len=", prg.length);
	StopWatch sw;
	sw.start();
	foreach(cmd; prg) 
		final switch(cmd.action) {
			case Action.Add: h1[cmd.key] = cmd.value; break;
			case Action.Remove: 
                static if (isVibeHM!H1) { // vibe's HashMap only removes existing keys
                    if (cmd.key in h1) h1.remove(cmd.key);
                } else
                    h1.remove(cmd.key); 
                break;
		}
	sw.stop();
	auto dur1 = sw.peek();
	writeln(name1, ": ", dur1.msecs, "ms");
	
	sw.reset();
	sw.start();
	foreach(cmd; prg) 
		final switch(cmd.action) {
			case Action.Add: h2[cmd.key] = cmd.value; break;
			case Action.Remove: h2.remove(cmd.key); break;
		}
	sw.stop();
	auto dur2 = sw.peek();
	writeln(name2, ": ", dur2.msecs, "ms");

	writeln(h1.length, " entries");
	if (h1.length==0) goto start;

	//if (h1.length < 40) h1.dump();
	//h1.showStats();
	try {
		writeln(name1, " =?= ", name2);
		enforce(equalHashes(h1, h2));
		writeln(name2, " =?= ", name1);
		enforce(equalHashes(h2, h1));
	} catch (DiffValues!K dv) {
        static if (hasMember!(H1, "dump")) {
		    prg.filter!(c => c.key == dv.key).writeln;
		    h1.dump();
        } else 
            writeln("Error: different values for ", dv.key);
	} 
    delete prg;
	/*hashQuality(h1);
    h1.showCollisionDistr();

    auto hh = new RHHash!(K,V)(h1.length);
    int cnt = 0;
    foreach(k,v; h1) { 
        hh.add(k, v);
        cnt++;
        /*if ((cnt % 1000)==0 || cnt < 20) {
            hh.showDist();
            hh.showCollisionDistr();
        }*
    }
    writeln("in a clean RHH:");
    hh.showStats();
    hh.showCollisionDistr();
    hh.clearAndFree();

	h1.clearAndFree();*/
}

void measure(string caption, scope void delegate() f) {
	StopWatch sw;
	sw.start();
	f();
	sw.stop();
	writeln(caption, ": ", sw.peek.msecs, " ms");
}

enum isVibeHM(T) = hasMember!(T, "grow");

void testHashesHisto(K, V, H1, H2)(H1 delegate() make1, H2 delegate() make2, size_t num) if (isIntegral!V) {
	static if (K.sizeof==1)
		enum nSrc = 256;
	else static if (K.sizeof==2)
		enum nSrc = 65536;
	else
		enum nSrc = 100000;
	auto srcValues = new K[nSrc];
	foreach(ref x; srcValues) 
		x = gen!K;
	auto data = new K[num];
	foreach(ref x; data) 
		x = srcValues[uniform(0, nSrc)];
    scope(exit) { 
        delete srcValues;
        delete data;
    }
	
	auto h1 = make1();
	auto h2 = make2();
	enforce(equalHashes(h1, h2));

	measure(H1.stringof ~ " histo making", (){ 
		foreach(x; data) {
            static if (isVibeHM!(H1)) {
                auto p = x in h1;
                if (p) (*p)++;
                else h1[x] = 1;
            } else
                h1[x]++;

        }
	});

	measure(H1.stringof ~ " reading", (){
		V v = 0;
		foreach(x; data)
			v ^= h1[x];
		writeln(v);
	});

	measure(H2.stringof ~ " histo making", (){ 
		foreach(x; data)
			h2[x]++;
	});

	measure(H2.stringof ~ " reading", (){
		V v = 0;
		foreach(x; data)
			v ^= h2[x];
		writeln(v);
	});
}

void testRB(K,V, bool histo)(size_t num) {
	writeln("testing ", K.stringof, " => ", V.stringof, ", num=",num);
    RHHash!(K, V) rh;
	auto make1() { rh = new RHHash!(K, V); return rh; }
	auto make2() { V[K] z; return z; };
	static if (histo)
		testHashesHisto!(K, V, RHHash!(K, V), V[K])(&make1, &make2, num);
	else
		testHashes!(K, V, RHHash!(K, V), V[K])(&make1, &make2, num, "RHHash", "AA"); 
    rh.clearAndFree();
}

void testLinProb(K,V, bool histo)(size_t num) {
	writeln("testing HashMap ", K.stringof, " => ", V.stringof, ", num=",num);
	auto make1() { HashMap!(K, V) z; return z; }
	auto make2() { V[K] z; return z; };
	static if (histo)
		testHashesHisto!(K, V, HashMap!(K, V), V[K])(&make1, &make2, num);
	else
        testHashes!(K, V, HashMap!(K, V), V[K])(&make1, &make2, num, "HashMap", "AA"); 
}

void testRobLin(K,V, bool histo)(size_t num) {
	writeln("testing HashMap vs. RHH on ", K.stringof, " => ", V.stringof, ", num=",num);
	RHHash!(K, V) rh;
	auto make1() { HashMap!(K, V) z; return z; }
	auto make2() { rh = new RHHash!(K, V); return rh; };
	static if (histo)
		testHashesHisto!(K, V, HashMap!(K, V), RHHash!(K, V))(&make1, &make2, num);
	else
        testHashes!(K, V, HashMap!(K, V), RHHash!(K, V))(&make1, &make2, num, "HashMap", "RHH"); 
	rh.clearAndFree();
}


auto hashQuality(K,V)(RHHash!(K,V) h) {
	auto hashValues = new RHHash!(hash_t, bool);
	foreach(hv; h.byKey.map!(k => h.calcHash(k)))
		hashValues[hv] = true;
	writeln("hash quality: keys=", h.length, " different hashes=", hashValues.length);
}

int clusterSize(int prob) {// prob : 0..999 / 1000
	auto hit = uniform(1, 1000);
	int n = 0;
	foreach(i; 0..1000) {
		if (uniform(0,1000) < prob) n++;
		else {
			if (hit <= 0) return n;
			n = 0;
		}
		hit--;
	}
	return n;
}

double averageCluster(int prob) {
	long sum, n = 10000;
	while(n>0) {
		auto sz = clusterSize(prob);
		if (sz > 0) {
			sum += sz; n--;
		}
	}
	return cast(double)sum / 10000;
}

int comp(T)(T x, T y) {
    if (x < y) return -1;
    if (x > y) return 1;
    return 0;
}

struct TestStruct(bool ownHash) {
	int x, y;
	string s;

    static if (ownHash) {
        const hash_t toHash() { 
            return x ^ y; 
        }
        const bool opEquals(ref const TestStruct!true o) { 
            return x == o.x && y == o.y && s == o.s; 
        }
        const int opCmp(ref const TestStruct!true o) {
            int r = comp(x, o.x);
            if (r != 0) return r;
            r = comp(y, o.y);
            if (r != 0) return r;
            return std.string.cmp(s, o.s);            
        }
    }
}

class TestClass(bool ownHash) {
	float a;
	char b;
	string c;

    static if (ownHash) {
        override hash_t toHash() { 
            return cast(int)a * b; 
        }
        override bool opEquals(Object ob) { 
            auto o = cast(TestClass!true)ob;
            return a == o.a && b == o.b && c == o.c;
        }
        override int opCmp(Object ob) {
            auto o = cast(TestClass!true)ob;
            int r = comp(a, o.a);
            if (r != 0) return r;
            r = comp(b, o.b);
            if (r != 0) return r;
            return std.string.cmp(c, o.c);            
        }
    }
    override string toString() const {
        return std.string.format("TestClass(%s, %s, %s)", a,b,c);
    }

}

void main(string[] argv)
{
	int num = argv.length > 1 ? argv[1].to!int : 300000;
	bool histo = argv.length > 2;

    //testLinProb!(int,int)(num);
    //return;
	alias types = TypeTuple!(/*bool,*/ int, /*double,*/ char, string, 
                             TestStruct!false, TestStruct!true, TestClass!false, TestClass!true);
	if (histo) {
		foreach(t1; types)
			//testRB!(t1, int, true)(num);
            //testLinProb!(t1, int, true)(num);
			testRobLin!(t1, int, true)(num);
	} else {
		foreach(t1; types)
			foreach(t2; types) 
				//testRB!(t1, t2, false)(num);
                //testLinProb!(t1, t2, false)(num);
				testRobLin!(t1, t2, false)(num);
	}
}
