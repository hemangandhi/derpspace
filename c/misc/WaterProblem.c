/*
 * The problem:
 * Given an n x m matrix of heights for some terrain, determine the volume of water that would accumulate
 * over the terrain after heavy rain.
 *
 * Summary of the solution:
 * For each boundary point: DFS to find all inner points that would drain to the current point.
 * At the end of this, all unvisited points are accumulation points.
 * Add the volumes accumulated by tallying volumetric differences. If a change is found, subtract
 * the change in volume.
 */
#include <stdio.h>
#include <stdlib.h>

/*
 * Gets the index for the array representation
 * of the matrix.
 */
int getMatIdx(int h, int r, int c){
        return r * h + c;
}

/* The possible cell states.*/
typedef enum {SEEN, UNSEEN, TALLIED} PtState;

/*
 * Does a DFS traverse to
 * find all the nodes that would
 * flow to the node at (r, c).
 */
void traverse(int * mat, PtState * states, int h, int w, int r, int c, int * fc){
        states[getMatIdx(h, r, c)] = SEEN;
        if(r == 0 || c == 0 || r == h - 1 || c == w - 1)
                *fc = *fc - 1;

        int z = mat[getMatIdx(h, r, c)];
        for(int i = -1; i < 2; i++){
                for(int j = -1; j < 2; j++){
                        if(i == 0 && j == 0) continue;
                        int x = r + i, y = c + j;
                        if(x >= 0 && x < h && y >= 0 && y < w
                                        && mat[getMatIdx(h, x, y)] >= z
                                        && states[getMatIdx(h, x, y)] == UNSEEN){
                                traverse(mat, states, h, w, x, y, fc);
                        }
                }
        }
}


/*
 * Find the first unseen point
 * on the border.
 */
void firstUnseenFr(PtState * states, int h, int w, int * r, int * c){
        for(int i = 0; i < w; i++){
                if(states[getMatIdx(h, 0, i)] == UNSEEN){
                        *r = 0; *c = i;
                        return;
                }else if(states[getMatIdx(h, h - 1, i)] == UNSEEN){
                        *r = h - 1; *c = i;
                        return;
                }
        }
        for(int i = 1; i < h - 1; i++){
                if(states[getMatIdx(h, i, 0)] == UNSEEN){
                        *r = i; *c = 0;
                        return;
                }else if(states[getMatIdx(h, i, w - 1)] == UNSEEN){
                        *r = i; *c = w - 1;
                        return;
                }
        }
}

/*
 * Find the first unseen point
 * anywhere.
 */
int firstUnseen(PtState * pts, int h, int w, int * r, int * c){
        for(int i = 0; i < h; i++)
                for(int j = 0; j < w; j++){
                        if(pts[getMatIdx(h, i, j)] == UNSEEN){
                                *r = i; *c = j;
                                return 1;
                        }
                }
        return 0;
}

/*
 * Traverse from an unseen point (DFS)
 * to tally the total volume.
 */
void tallyTraverse(int * mat, PtState * pts, int h, int w, int r, int c, int * min, int * vol, int * tCt){
        pts[getMatIdx(h, r, c)] = TALLIED;
        if(mat[getMatIdx(h, r, c)] < *min){
                *vol += (*min - mat[getMatIdx(h, r, c)]);
                *tCt += 1;
        }
        for(int i = -1; i < 2; i++){
                for(int j = -1; j < 2; j++){
                        if(i == 0 && j == 0) continue;
                        int x = r + i, y = c + j;
                        if(x < 0 || x >= h || y < 0 || y >= w)
                                continue;
                        else if(pts[getMatIdx(h, x, y)] == SEEN && mat[getMatIdx(h, x, y)] < *min){
                                *vol -= (*min - mat[getMatIdx(h, x, y)]) * *tCt;
                                *min = mat[getMatIdx(h, x, y)];
                        }else if(pts[getMatIdx(h, x, y)] == UNSEEN)
                                tallyTraverse(mat, pts, h, w, x, y, min, vol, tCt);
                }
        }
}

/*
 * Drive the above DFS
 * to tally all the unseen points.
 */
int tallyVolume(int * mat, PtState * pts, int h, int w){
        int r, c, totalVol, minSeen, talliedCt;
        totalVol = 0; talliedCt = 0;
        int minSet = 0;
        while(firstUnseen(pts, h, w, &r, &c)){
                if(!minSet){
                        minSeen = mat[getMatIdx(h, r, c) - 1];
                        minSet = 1;
                }
                tallyTraverse(mat, pts, h, w, r, c, &minSeen, &totalVol, &talliedCt);
        }
        return totalVol;
}

/*
 * Create a point-state matrix, initializing
 * at unseens.
 */
PtState * createPts(int h, int w){
        PtState * pts = (PtState *) malloc(sizeof(PtState)*h*w);
        for(int i = 0; i < h * w; i++){
                pts[i] = UNSEEN;
        }
        return pts;
}

/*
 * Drive the above, calling the parts.
 */
int waterProblem(int * mat, int h, int w){
        int unseenFr = 2*h + 2*w - 4;
        if(unseenFr == h * w) return 0;

        PtState * pts = createPts(h, w);
        int r, c;
        while(unseenFr > 0){
                firstUnseenFr(pts, h, w, &r, &c);
                traverse(mat, pts, h, w, r, c, &unseenFr);
        }

        int v = tallyVolume(mat, pts, h, w);
        free(pts);
        return v;
}

//TESTS!!!!
int main(int argc, char ** argv){
        int temp[9] = {5, 5, 5, 5, 1, 5, 5, 5, 5};
        printf("wp(temp, 3, 3) = %d ... expecting 4.\n", waterProblem(temp, 3, 3));
        temp[0] = 0;
        printf("wp(temp, 3, 3) = %d ... expecting 0.\n", waterProblem(temp, 3, 3));

        int temp2[25] = {5, 5, 5, 5, 5, 
                         5, 3, 3, 3, 5, 
                         5, 3, 3, 3, 5, 
                         5, 3, 3, 3, 5, 
                         5, 5, 5, 5, 5};
        printf("wp(temp2, 5, 5) = %d ... expecting 18.\n", waterProblem(temp2, 5, 5));
        temp2[12] = 7;
        printf("wp(temp2, 5, 5) = %d ... expecting 16.\n", waterProblem(temp2, 5, 5));
        temp2[24] = 4;
        printf("wp(temp2, 5, 5) = %d ... expecting 8.\n", waterProblem(temp2, 5, 5));
        temp2[0] = 8;
        printf("wp(temp2, 5, 5) = %d ... expecting 8.\n", waterProblem(temp2, 5, 5));

}
