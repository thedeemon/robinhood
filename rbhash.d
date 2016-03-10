module rbhash;
import std.range, std.traits, std.algorithm : swap, map;
import std.container : Array;

//version = stats;

version(stats) {
    int shrinkDist(long dist) 
    in {
        assert(dist >= 0);
    } body {
        if (dist < 16) return cast(int)dist;
        if (dist < 32) return 16;
        if (dist < 64) return 17;
        if (dist < 128) return 18;
        if (dist < 256) return 19;
        if (dist < 512) return 20;
        if (dist < 1024) return 21;
        return 22;
    }
}

void resize(T)(ref Array!T arr, size_t sz) {
    arr.reserve(sz);
    arr.length = sz;
}

struct RobinOptions {
    enum maxLoad = 8;  // *10%, i.e. 9 means 90%
    enum maxCluster = 200; // if max DIB / PSL / probe count gets higher than this, upsize
    enum maxOverhead = 4; // don't upsize if table is bigger than numEntries * maxOverhead
    enum combineData = false; // combine hash and key-value in one struct
}

class RHHash(Key, Value, Opts = RobinOptions) {
private:
    enum combine = Opts.combineData; 
    Array!Entry entries;
    //pragma(msg, "Entry size for ", Key.stringof, "=>", Value.stringof, ": ", Entry.sizeof);
    static if (!combine) Array!(hash_t) hashes;
    size_t numEntries, limit, numFilled; //filled is non-empty, i.e. live or dead
    enum useTypeInfo = !hasMember!(Key, "toHash");
    //pragma(msg, "RHHash using type info for ", Key.stringof, ": ", useTypeInfo);
    enum Deleted = 0x80000000;
    sizediff_t clusterSize; //max found so far during insert
    static if (useTypeInfo) {
        TypeInfo keyti;
    }

public:
    struct Entry {
        static if (combine) { hash_t hash; }
        Key key;
        static if (!is(Value==void)) Value value;
    }

    version(stats) {
        int nFind, nFindIter, nIns, nInsIter;
        sizediff_t nFindMax;
    }

    this(size_t expectedSize = 64) {
        size_t sz = 16;
        while(sz < expectedSize) sz *= 2;
        //entries = new Entry[sz];
		entries.resize(sz);
        static if (!combine) { hashes.resize(sz); }
        numEntries = 0; numFilled = 0;
        limit = sz * Opts.maxLoad / 10;
        static if (useTypeInfo) keyti = typeid(Key);        
    }

final:

    version(stats) {
        void dump() {
            import std.stdio;
            writeln("hashes.length=", hashes.length, " num=", numEntries, " filled=", numFilled);
            foreach(i, h; hashes)
                writeln(i, ": h=", h, " k=", entries[i].key, " v=", entries[i].value);
        }

        void showStats() {
            import std.stdio, std.algorithm : count;
            writeln("inserts: ", nIns, " iters: ", nInsIter, " avg: ", cast(double)nInsIter / nIns);
            writeln("finds: ", nFind, " iters: ", nFindIter, " avg: ", cast(double)nFindIter / nFind, " max: ", nFindMax);
            writeln("slots: ", hashes.length, " empty: ", hashes.count(0), " dead: ", hashes.count!(x => (x & Deleted)!=0));
        }

        RHHash!(int,int) getDistr() {
            auto dd = new RHHash!(int,int);
            foreach(i, h; hashes)
                if (h != 0)
                    dd[calcDist(h, i).shrinkDist]++;
            return dd;
        }

        void showCollisionDistr() {
            import std.algorithm, std.array, std.typecons, std.stdio;
            writeln("positions populatity");
            auto dd = new RHHash!(int,int); // position => number of hits
            immutable mask = hashes.length - 1;
            foreach(i, h; hashes)
                if (h != 0)
                    dd[h & mask]++;
            auto hitsDist = new RHHash!(int,int); // numHits => number of positions having it
            foreach(n; dd.byValue)
                hitsDist[n]++;
            auto app = appender!(Tuple!(int,int)[]);
            foreach(k,v; hitsDist)
                app ~= tuple(k,v);
            auto data = app.data;
            sort!"a[0] > b[0]"(data);
            foreach(t; data[].take(10))
                write(t[0], ":", t[1], " ");
            writeln("...");
            dd.clearAndFree();
        }

        void showDist() {   
            import std.stdio;
            writeln("DIB distribution: numEntr=", numEntries, " numFilled=", numFilled);
            auto dd = getDistr();
            foreach(i; 0..23) write(dd[i], " ");
            writeln();
            dd.clearAndFree();
        }
    }//stats

    @property size_t length() const { return numEntries; }

	static if (!is(Value==void))
    Value get(Key key, lazy Value default_value = Value.init) {
        auto idx = findIndex(key);
        if (idx == -1) return default_value;
        return entries[cast(size_t)idx].value;
    }

    Value* opBinaryRight(string op)(Key key)  
     if (op=="in") {
        auto idx = findIndex(key);
        if (idx == -1) return null;
        return &entries[cast(size_t)idx].value;
    }

	static if (!is(Value==void))
    ref Value opIndex(Key key) {
        auto idx = findIndex(key);
        if (idx == -1) idx = insert(key, Value.init); 
        return entries[cast(size_t)idx].value;
    }

	bool contains(Key key) {
		return findIndex(key) >= 0;
	}

	static if (is(Value==void))
	void add(Key key) {
		if (findIndex(key) < 0)
			insert(key); 
	}

    void clear() {
        numEntries = 0; numFilled = 0;
        static if (combine) {
            foreach(ref e; entries) e.hash = 0;
        } else
            hashes[] = 0;
    }

    void clearAndFree() {
        numEntries = 0; numFilled = 0;
        static if (!combine) {
            //delete hashes;
            //hashes = [];
			hashes.clear();
        }
        //delete entries;		
        //entries = [];
		entries.clear();
    }

	static if (is(Value==void)) alias ForEachDel = int delegate(Key);
	       else                 alias ForEachDel = int delegate(Key, Value); 

	int opApply(scope ForEachDel dg)  {
		size_t i = -1;
		foreach(e; entries) {
			i++;
			immutable h = hash(i);
			if (h==0 || (h & Deleted) != 0) continue;
			static if (is(Value==void)) auto r = dg(e.key);
			       else                 auto r = dg(e.key, e.value);
			if (r != 0) return r;
		}
		return 0;
	}

    void remove(Key key) {
        auto idx = findIndex(key);
        if (idx == -1) return;
        size_t pos = cast(size_t)idx;
        static if (combine)
            entries[pos].hash |= Deleted;
        else
            hashes[pos] |= Deleted;
        numEntries--;
    }

    auto range() {   return EntryRange(this).findNext(); } // => range of Entry
    auto byKey() {   return range().map!(e => e.key);	}
	static if (!is(Value==void))
    auto byValue() { return range().map!(e => e.value); }

private:
    struct EntryRange {
        @property bool empty() { return i >= h.entries.length; }
        Entry front() { return h.entries[i]; }
        void popFront() { i++; findNext(); }

        EntryRange findNext() { // after this call i either points to a live entry or = hashes.length
            while(i < h.entries.length) {
                immutable hp = h.hash(i);
                if (hp != 0 && (hp & Deleted)==0) return this;
                i++;
            } 
            return this;
        }

        RHHash!(Key, Value) h;
        size_t i; // points to a live entry
    }

    hash_t hash(size_t pos) { 
        static if (combine) return entries[pos].hash;
               else         return hashes[pos];
    }

    void resize(size_t newSize) {
        assert(newSize > numEntries);
        Array!Entry oldEntries = entries;
        
        auto oldNum = numEntries;
        //entries = new Entry[newSize];
		entries = Array!Entry();
		entries.resize(newSize);
        
        static if (!combine) {
            //hash_t[] oldHashes = hashes;
            //hashes = new hash_t[newSize];
            //scope(exit) delete oldHashes;
			Array!hash_t oldHashes = hashes;
			hashes = Array!hash_t();
			hashes.resize(newSize);
        }
        limit = newSize * Opts.maxLoad / 10;
        assert(numEntries + 1 < limit);
        numEntries = 0; numFilled = 0; clusterSize = 0;
        static if (combine) {
            foreach(e; oldEntries) 
                if (e.hash != 0 && (e.hash & Deleted)==0)	
                    doInsert(e);
        } else {
			size_t i = 0;
            foreach(h; oldHashes) { 
                if (h != 0 && (h & Deleted)==0)	
                    doInsert(oldEntries[i], h);
				i++;
			}
		}
        assert(numEntries == oldNum);
        //delete oldEntries;
    }

	static if (is(Value==void))
		size_t insert(Key key) { // => pos
			resizeIfNecessary();
			auto h = calcHash(key);
			static if (combine) auto entry = Entry(h, key);
			       else 		auto entry = Entry(key);			
			return doInsert(entry, h);
		}
	else
		size_t insert(Key key, Value value) { // => pos
			resizeIfNecessary();
			auto h = calcHash(key);
			static if (combine) auto entry = Entry(h, key, value);
			       else         auto entry = Entry(key, value);				
			return doInsert(entry, h);
		}

	void resizeIfNecessary() {
		if (numFilled >= limit || (clusterSize > Opts.maxCluster && entries.length * 2 < Opts.maxOverhead * numEntries))
			resize(entries.length * 2);
	}

    size_t doInsert(ref Entry entry, hash_t keyHash) { // => pos
        immutable mask = entries.length - 1;
        size_t pos = keyHash & mask, j;
        sizediff_t dist = 0, start = pos;
        version(stats) nIns++; 

        //find where to insert the new entry
        while(true) {
            immutable hp = hash(pos);
            if (hp==0) { //empty slot found, just fill it
                numFilled++;
                goto fillBucket;
            }
            immutable hpdist = calcDist(hp, pos);
            if (hpdist < dist) { // rich guy found, take his bucket
                if ((hp & Deleted) != 0) goto fillBucket; // he was dead, just fill this slot without moving others
                break;
            }
            if (hpdist == dist && (hp & Deleted) != 0) goto fillBucket; //dead guy of equal wealth, use this bucket
            pos = (pos + 1) & mask; // live poor ones so far, move along
            dist++;
        }
        //here pos points to live entry which is richer
        assert(hash(pos) != 0);
        assert((hash(pos) & Deleted)==0);
        assert(calcDist(hash(pos), pos) < dist);
        //all live entries starting from pos should be shifted right till first dead or empty slot
        j = (pos + 1) & mask;
        while(true) { // look for dead or empty
            immutable hp = hash(j);
            if (hp==0) { numFilled++; break; } // empty bucket found
            if ((hp & Deleted) != 0) break; // dead one found
            j = (j + 1) & mask;
        }
        dist = (j - start) & mask; // how far did we go?
        //shift
        while(j != pos) { // walk back shifting right values in table
            immutable prev = (j - 1) & mask;
            static if (!combine) hashes[j] = hashes[prev];
            entries[j] = entries[prev];
            j = prev;
        }
      fillBucket:
        static if (!combine) hashes[pos] = keyHash;
        entries[pos] = entry;
        numEntries++;
        clusterSize = dist > clusterSize ? dist : clusterSize;
        return pos;
    }

    sizediff_t findIndex(Key key) { // -1 if not found
        immutable hash_t keyHash = calcHash(key);
        immutable mask = entries.length - 1;
        size_t pos = keyHash & mask;
        sizediff_t dist = 0;
        version(stats) nFind++; 
        while(true) {
            version(stats) nFindIter++; 
            assert(pos < entries.length);
            immutable hash_t hp = hash(pos);
            if (hp==keyHash) {
                if (entries[pos].key == key) {
                    version(stats) if (dist > nFindMax) nFindMax = dist;
                    return pos;
                }
            } else {
                if (hp==0) return -1; // empty slot, key not found
                if (calcDist(hp, pos) < dist) return -1; // gone too far
            }
            pos = (pos+1) & mask;
            dist++;
        }
    }

    sizediff_t calcDist(size_t h, size_t p) const
    in { 
        assert(p <= entries.length); 
    } body {
        return (p - h) & (entries.length - 1);
    }

    public hash_t calcHash(Key key) const
    out(result) { 
        assert(result != 0);
        assert((result & Deleted) == 0);
    } body {
        hash_t h;
        static if (useTypeInfo) {
            h = keyti.getHash(&key);
        } else {
            static if (isPointer!Key || is(Unqual!Key == class)) 
                h = key is null ? 1 : key.toHash();
            else h = key.toHash();
        }
        h &= ~Deleted; 
        return h == 0 ? 1 : h; // h is never 0, because 0 means empty slot
    }
}
