import ctypes as C;
import ctypes.util as _;

import collections;
import collections.abc;

class Node(C.c_uint64):
    """
    BDD node object.
    """
    def __hash__(self):
        return self.value;

    def __eq__(self, other):
        assert(isinstance(other, Node));
        return self.value==other.value;

    @property
    def var(self):
        """
        Return the variable of this node.
        """
        return _sylvan.mtbdd_getvar(self);

    @property
    def low(self):
        """
        Return the low edge of this node.
        """
        return _sylvan.mtbdd_getlow(self);

    @property
    def high(self):
        """
        Return the high edge of this node.
        """
        return _sylvan.mtbdd_gethigh(self);

class Edge(C.c_uint64):
    """
    BDD edge object.
    """
    def __init__(self, val):
        # XXX: Python issue 29270 prevents use of super()
        C.c_uint64.__init__(self, val);
        _sylvan.mtbdd_ref(self.value);

    def __del__(self):
        _sylvan.mtbdd_deref(self.value);

    # XXX: Functions returning an Edge by restype will not call  Edge.__init__,
    # which leaves its Sylvan refcount unincremented. Functions returning an
    # edge must always be wrapped in Edge.from_param.
    @classmethod
    def from_param(cls, val):
        assert(isinstance(val, Edge));
        _sylvan.mtbdd_ref(val.value);
        return val;

    def __hash__(self):
        return self.value;

    def __eq__(self, other):
        assert(isinstance(other, Edge));
        return self.value==other.value;

    def __invert__(self):
        return Edge(self.value ^ 0x8000000000000000);

    @property
    def negated(self):
        """
        Returns whether this edge is negated.
        """
        return (self.value & 0x8000000000000000)!=0;

    @property
    def low(self):
        """
        Returns the low edge of this edge's node, taking into account this
         edge's negation.
        """
        return Edge.from_param(_sylvan.mtbdd_getlow(self));

    @property
    def high(self):
        """
        Returns the high edge of this edge's node, taking into account this
         edge's negation.
        """
        return Edge.from_param(_sylvan.mtbdd_gethigh(self));

    @property
    def node(self):
        """
        Returns the node of this edge.
        """
        return Node(self.value & 0x7FFFFFFFFFFFFFFF);

    @property
    def nodeCount(self):
        """
        Returns the number of nodes in the BDD rooted at this edge.
        """
        return _sylvan.mtbdd_nodecount_more(C.byref(self), 1);

    @property
    def pathCount(self):
        """
        Returns the number of paths in the BDD rooted at this edge.
        """
        return _sylvan._lace(_sylvan.sylvan_pathcount_CALL, self, 0);

    def __and__(self, other):
        return _sylvan._lace(_sylvan.sylvan_and_CALL, self, other, 0);

    def __or__(self, other):
        return ~_sylvan._lace(_sylvan.sylvan_and_CALL, ~self, ~other, 0);

    def __xor__(self, other):
        return _sylvan._lace(_sylvan.sylvan_xor_CALL, self, other, 0);

    def evaluations(self, vars):
        """
        Returns an iterator over all evaluations for the BDD rooted at this
         edge, over the variables [vars].
        Each evaluation is represented as a dict of a variable to its value.
        """
        assert(isinstance(vars, BDDSet));
        val = (C.c_char * len(vars))();
        valp = C.cast(C.byref(val), C.c_char_p);

        varList = [i for i in vars];
        leaf = _sylvan.mtbdd_enum_all_first(self, vars.root, valp, None);
        while(leaf.value!=0):
            ret = collections.OrderedDict();
            for i, v in enumerate(val):
                ret[varList[i]] = ord(v)!=0;

            yield ret;
            leaf = _sylvan.mtbdd_enum_all_next(self, vars.root, valp, None);

    def image(self, T, vars):
        """
        Computes the image of the BDD rooted at this edge over a relation [T],
         over the variables [vars].
        Returns an edge to the root of the new BDD.
        """
        vars = vars.root;
        return _sylvan._lace(_sylvan.sylvan_relnext_CALL, self, T, vars, 0);

    def preimage(self, T, vars):
        """
        Computes the preimage of the BDD rooted at this edge over a relation
         [T], over the variables [vars].
        Returns an edge to the root of the new BDD.
        """
        raise NotImplementedError;

class BDD(object):
    """
    BDD object.
    """
    name = "sylvan";

    def __init__(self):
        assert(not _sylvan.used);
        _sylvan.used = True;

    @property
    def true(self):
        """
        Returns an edge leading to True.
        """
        return Edge(0x8000000000000000);

    @property
    def false(self):
        """
        Returns an edge leading to False.
        """
        return Edge(0x0000000000000000);

    def var(self, var):
        """
        Returns an edge leading to a node for a variable [var], with its low
         edge leading to False and the high edge leading to True.
        """
        return Edge.from_param(_sylvan.mtbdd_ithvar(var));

    def ite(self, i, t, e):
        """
        Computes the if-then-else of given BDDs rooted at [i], [t], and [e].
        Returns an edge to the root of the new BDD.
        """
        return _sylvan._lace(_sylvan.sylvan_ite_CALL, i, t, e, 0);

class BDDSet(collections.abc.MutableSet):
    """
    BDD-based set object.
    """
    def __init__(self, bdd):
        assert(isinstance(bdd, BDD));
        self.bdd = bdd;
        self.root = bdd.true;
        self._len = None;

    def __len__(self):
        if(self._len is None):
            self._len = _sylvan.mtbdd_set_count(self.root);
        return self._len;

    def __iter__(self):
        t = (self.bdd.true, self.bdd.false);
        i = self.root;
        while(i not in t):
            yield i.node.var;
            i = i.high;

        assert(i!=self.bdd.false);

    def __contains__(self, item):
        return _sylvan.mtbdd_set_contains(self.root, item)==1;

    def add(self, item):
        """
        Add a given [item] to this set.
        """
        self.root = Edge.from_param(_sylvan.mtbdd_set_add(self.root, item));
        self._len = None;

    def discard(self, item):
        """
        Remove a given [item] from this set.
        """
        self.root = Edge.from_param(_sylvan.mtbdd_set_remove(self.root, item));
        self._len = None;

    def union(self, *others):
        """
        Return the union of this set with the [others].
        """
        s = BDDSet(self.bdd);
        s.root = self.root;
        for o in others:
            s.root = _sylvan._lace(_sylvan.sylvan_and_CALL, s.root, o.root, 0);

        return s;

def init():
    # initialize Sylvan
    try:
        path = C.util.find_library("sylvan");
        assert(path is not None);

        lib = C.CDLL(path, mode=C.RTLD_GLOBAL);
    except (OSError, AssertionError) as e:
        raise ImportError from e;

    # create wrapper with correct function types
    types = {
        "lace_get_worker": (C.c_void_p,),
        "lace_get_head": (C.c_void_p, C.c_void_p),

        "mtbdd_ref":   (None, C.c_uint64),
        "mtbdd_deref": (None, C.c_uint64),

        "mtbdd_getvar": (C.c_uint32, C.c_uint64),
        "mtbdd_getlow": (Edge, C.c_uint64),
        "mtbdd_gethigh": (Edge, C.c_uint64),
        "mtbdd_ithvar": (Edge, C.c_uint32),

        "mtbdd_nodecount_more": (C.c_size_t, C.POINTER(Edge), C.c_size_t),
        "sylvan_pathcount_CALL": (C.c_double, C.c_void_p, C.c_void_p,
                                  Edge, C.c_uint32),

        "sylvan_ite_CALL": (Edge, C.c_void_p, C.c_void_p, Edge, Edge, Edge,
                            C.c_uint32),
        "sylvan_and_CALL": (Edge, C.c_void_p, C.c_void_p, Edge, Edge,
                            C.c_uint32),
        "sylvan_xor_CALL": (Edge, C.c_void_p, C.c_void_p, Edge, Edge,
                            C.c_uint32),

        "sylvan_exists_CALL":  (Edge, C.c_void_p, C.c_void_p, Edge, Edge,
                                C.c_uint32),
        "sylvan_project_CALL": (Edge, C.c_void_p, C.c_void_p, Edge, Edge,
                                C.c_uint32),
        "sylvan_relnext_CALL": (Edge, C.c_void_p, C.c_void_p, Edge, Edge,
                                Edge, C.c_uint32),

        "mtbdd_enum_all_first": (Edge, Edge, Edge, C.c_char_p, C.c_void_p),
        "mtbdd_enum_all_next":  (Edge, Edge, Edge, C.c_char_p, C.c_void_p),

        "mtbdd_set_count":    (C.c_size_t, Edge),
        "mtbdd_set_contains": (C.c_int, Edge, C.c_uint32),
        "mtbdd_set_add":      (Edge, Edge, C.c_uint32),
        "mtbdd_set_remove":   (Edge, Edge, C.c_uint32),
    };

    class Library(object):
        def __init__(self, lib, types):
            # initialize Lace
            lib.lace_init(0, 10000000);
            lib.lace_startup(0, None, None);

            # determine sizes
            nodeSize, cacheSize = (16, 24), (16, 24);
            if(C.sizeof(C.c_voidp) > 4):
                nodeSize = (16, 28);

            lib.sylvan_set_sizes(1 << nodeSize[0],  1 << nodeSize[1],
                                 1 << cacheSize[0], 1 << cacheSize[1]);
            lib.sylvan_init_package();
            lib.sylvan_set_granularity(1);

            lib.sylvan_init_mtbdd();
            self.lib = lib;

            self.types = types;
            self.used = False;

        def __getattr__(self, name):
            fn = getattr(self.lib, name);
            if(fn.argtypes is None):
                # enforce type checking/type conversion
                types = self.types[name];
                fn.restype = types[0];
                fn.argtypes = types[1:];

            return fn;

        def _lace(self, fn, *args):
            worker = _sylvan.lace_get_worker();
            head = _sylvan.lace_get_head(worker);

            res = fn(worker, head, *args);

            if(fn.restype is Edge):
              res = Edge.from_param(res);
            return res;

    return Library(lib, types);

_sylvan = init();
