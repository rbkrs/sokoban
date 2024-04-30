import bdd;
import itertools;

import argparse;
import time;

from collections import deque

"""
NOTE: This model implements a simplified version of Sokoban, which does not
take into account the reachability of boxes by the player.
"""

class Field:
    NONE  = " ";
    FLOOR = ".";
    BOX   = "x";
    GOAL  = "o";
    MAN   = "@";

class Sokoban(object):
    """
    Sokoban object.
    """
    # initial board layout
    # Sokoban levels (c) 1982 Thinking Rabbit
    levels = [
        # Level 1
        ["    ...             ",
         "    x..             ",
         "    ..x             ",
         "  ..x..x.           ",
         "  . .   .           ",
         "... .   .       ..oo",
         ".x..x.............oo",
         "    .    . @    ..oo",
         "    ......          "],
        # Level 2
        ["oo.. .....  ",
         "oo.. .x..x..",
         "oo.. x    ..",
         "oo....@.  ..",
         "oo.. . ..x. ",
         "     .  x.x.",
         "  .x..x.x.x.",
         "  .... ....."],
        # Level 3
        ["        .....@ ",
         "        .x x.  ",
         "        .x..x  ",
         "         x.x.  ",
         "        .x. .  ",
         "oooo..  .x..x..",
         " ooo....x..x...",
         "oooo..         "],
    ];

    def __init__(self, level=1):
        dd = bdd.BDD();
        self.bdd = dd;
        self._var = 0;

        self.layout = self.levels[level - 1];

        lay = self.layout;
        self.rows, self.cols = len(lay), len(lay[0]);

        R, C = self.rows, self.cols;

        self.initial = dd.true;
        self.varsSrc = bdd.BDDSet(dd);
        self.varsDst = bdd.BDDSet(dd);

        # create variables for each field and set up initial board
        self.varMap = {};
        self.playerCoord = None;
        for r, c in itertools.product(range(R), range(C)):
            if(lay[r][c]==Field.NONE):
                continue;

            # create variable pair for this field
            src, dst = self.var();
            self.varMap[(r, c)] = src;

            # add to initial board
            v = dd.var(src);
            if(lay[r][c]!=Field.BOX):
                v = ~v;
            self.initial &= v;


        # create triples of fields
        triples = [];
        for r, c in itertools.product(range(R), range(C)):
            if(lay[r][c]==Field.NONE):
                continue;

            # horizontal triple
            if((c + 2) < C
                and (lay[r][c + 1]!=Field.NONE)
                and (lay[r][c + 2]!=Field.NONE)):
                tri = bdd.BDDSet(dd);

                tri.add(self.varMap[(r, c + 0)]);
                tri.add(self.varMap[(r, c + 1)]);
                tri.add(self.varMap[(r, c + 2)]);

                triples.append(tri);

            # vertical triple
            if((r + 2) < R
                and (lay[r + 1][c]!=Field.NONE)
                and (lay[r + 2][c]!=Field.NONE)):
                tri = bdd.BDDSet(dd);

                tri.add(self.varMap[(r + 0, c)]);
                tri.add(self.varMap[(r + 1, c)]);
                tri.add(self.varMap[(r + 2, c)]);

                triples.append(tri);

        # create winning board
        self.winning = dd.false;
        
        for r, c in itertools.product(range(R), range(C)):
            if (lay[r][c] == Field.BOX):
                box_on_goal = self.var();
                box_on_goal_bdd = dd.var(box_on_goal[0][0]);

                if (lay[r][c] == Field.GOAL):
                    self.winning &= box_on_goal_bdd;
    
        # create move relation
        parts = [];
        for tri in triples:
            vars = bdd.BDDSet(dd);
            for v in tri:
                # add both source and destination vars
                vars.add(v);
                vars.add(v + 1);

            src = [dd.var(v + 0) for v in tri];
            dst = [dd.var(v + 1) for v in tri];

            rel = dd.false;
            
            x = len(src);
            for i in range(x):
                if (lay[tri[i] // C][tri[i] % C] in {Field.FLOOR, Field.GOAL} and lay[tri[i + 1] // C][tri[i + 1] % C] in {Field.FLOOR, Field.GOAL}):
                    rel |= src[i] & dst[i];

            parts.append((rel, vars));

        self.movePartial = parts;

        self.move = dd.false;
        self.vars = self.varsSrc.union(self.varsDst);
        for rel, vars in parts:
            # keep all unaffected variables equal
            for v in self.varsSrc:
                if(v in vars):
                    continue;

                a = dd.var(v);
                b = dd.var(v + 1);
                rel &= ~(a ^ b);

            # combine partial relations
            self.move |= rel;

    def var(self):
        """
        Returns a new source/destination variable pair.
        """
        src = self._var;
        self._var += 1;
        dst = self._var;
        self._var += 1;

        self.varsSrc.add(src);
        self.varsDst.add(dst);

        return (src, dst);

    def draw(self, board):
        """
        Returns a textual representation of the given Sokoban board given
         by the evaluation [board].
        """
        R, C = self.rows, self.cols;
        lay = self.layout;

        # serialize board
        s = [[Field.NONE for _ in range(C)] for _ in range(R)];
        for r, c in itertools.product(range(R), range(C)):
            v = self.varMap.get((r, c), None);
            if(v is None):
                continue;

            if(board[v]):
                s[r][c] = Field.BOX;
            elif(lay[r][c]==Field.GOAL):
                s[r][c] = Field.GOAL;
            else:
                s[r][c] = Field.FLOOR;

        return "\n".join(["".join(i) for i in s]);

    class Stats(object):
        def __init__(self):
            self.it = 0;
            self.maxNodes = 0;

        def update(self, dd):
            n = dd.nodeCount;
            self.it += 1;
            self.maxNodes = max(self.maxNodes, n);

    def reachBFS(self):
        """
        Returns a BDD representing all reachable states,
         using the BFS algorithm.
        """
        stats = self.Stats();

        visited = set()
        queue = deque([self.initial])

        while queue:
            node = queue.popleft()
            if node not in visited:
                visited.add(node)
                for neighbor in self[node]:
                    if neighbor not in visited:
                        queue.append(neighbor)
                        stats.update()
        
        return visited

    def reachBFSPart(self):
        """
        Returns a BDD representing all reachable states,
         using the BFS algorithm.
        """
        stats = self.Stats();

        visited = set()
        visited_prev = set()
        queue = deque([self.initial])

        while visited != visited_prev:
            visited_prev = visited.copy()
            for node in queue:
                if node not in visited:
                    visited.add(node)
                    queue.extend(self[node])
                    stats.update()
        
        return visited

    def reachChaining(self):
        """
        Returns a BDD representing all reachable states,
         using the chaining algorithm.
        """
        stats = self.Stats();

        visited = set()
        relations = True
        visited.add(self.initial)

        while relations:
            relations = False
            new = set()

            for state in visited:
                for relation in self.movePartial:
                    new.update(relation[0].apply(state))
            
            if new.difference(visited):
                visited.update(new)
                relations = True

        return visited

    def reachSatLike(self):
        """
        Returns a BDD representing all reachable states,
         using a saturation-like algorithm.
        """
        stats = self.Stats()

        visited = set()
        visited_prev = set()

        while visited != visited_prev:
            visited_prev = visited.copy()
            for relation in reversed(self.movePartial):
                for node in visited_prev:
                    visited.update(relation[0].apply(node))
                stats.update()

        return visited

    def reachSat(self):
        """
        Returns a BDD representing all reachable states,
         using the saturation algorithm.
        """
        stats = self.Stats();
        
        visited = set()
        visited_prev = set()

        for relation in self.movePartial:
            while visited != visited_prev:
                visited_prev = visited.copy()
                for node in visited_prev:
                    visited.update(relation[0].apply(node))
                stats.update()

        return visited

    reach = [
        reachBFS,
        reachBFSPart,
        reachChaining,
        reachSatLike,
        reachSat,
    ];

def main():
    """
    Perform reachability analysis on Sokoban and print winnable states.
    """
    modes = {m.__name__[5:].lower(): m for m in Sokoban.reach};

    ap = argparse.ArgumentParser(description=main.__doc__);
    ap.add_argument("--mode", choices=modes, required=True);
    ap.add_argument("--level", type=int, default=1);
    args = ap.parse_args();

    # activate requested reachability function
    soko = Sokoban(level=args.level);
    print("Sokoban level %d" % args.level);

    t0 = time.time();
    reach = modes[args.mode](soko);
    t1 = time.time();
    dt = divmod(t1 - t0, 60);

    print("\n%02d:%06.3f elapsed; %d reachable states"
           % (dt[0], dt[1], reach.pathCount));

    # print boards
    reach &= soko.winning;
    print("%d winning states" % reach.pathCount);
    for i in reach.evaluations(soko.varsSrc):
        print([int(v) for v in i if i[v]]);
        print(soko.draw(i));
        print();

if(__name__=="__main__"):
    main();
