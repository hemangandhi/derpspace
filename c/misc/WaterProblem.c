#include <stdio.h>
#include <stdlib.h>

int getMatIdx(int h, int r, int c){
        return r * h + c;
}

typedef enum {SEEN, UNSEEN, TALLIED} PtState;

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

int sumOfUnseen(int * mat, PtState * pts, int h, int w){
        int acc = 0;
        for(int i = 0; i < h; i++){
                for(int j = 0; j < w; j++){
                        if(pts[getMatIdx(h, i, j)] == UNSEEN)
                                acc += mat[getMatIdx(h, i, j)];
                }
        }
        return acc;
}

PtState * createPts(int h, int w){
        PtState * pts = (PtState *) malloc(sizeof(PtState)*h*w);
        for(int i = 0; i < h * w; i++){
                pts[i] = UNSEEN;
        }
        return pts;
}

int waterProblem(int * mat, int h, int w){
        int unseenFr = 2*h + 2*w - 4;
        if(unseenFr == h * w) return 0;

        PtState * pts = createPts(h, w);
        int r, c;
        while(unseenFr > 0){
                firstUnseenFr(pts, h, w, &r, &c);
                printf("Traversing from (%d, %d). \n", r, c);
                traverse(mat, pts, h, w, r, c, &unseenFr);
                printf("Waiting on %d cells. %d.\n", unseenFr, SEEN == pts[getMatIdx(h, r, c)]);
        }

        int v = sumOfUnseen(mat, pts, h, w);
        free(pts);
        return v;
}

int main(int argc, char ** argv){
        int temp[9] = {5, 5, 5, 5, 4, 5, 5, 5, 5};
        printf("wp(temp, 3, 3) = %d ... expecting 4.\n", waterProblem(temp, 3, 3));
//        PtState test[9] = {SEEN, SEEN, SEEN, SEEN, SEEN, SEEN, SEEN, UNSEEN};
//        int r, c;
//        firstUnseenFr(test, 3, 3, &r, &c);
//        printf("( %d, %d) \n", r, c);
}
