#include <stdio.h>
#include <stdlib.h>

enum _bool {false, true};
typedef enum _bool bool;

bool inRange(int v, int min, int max){
	return min <= v && v < max;
}

int ** allocMat(int l, int w){
	int ** r = (int **) malloc(sizeof(int *)*l);
	for(int i = 0; i < l; i++)
		r[i] = (int *) malloc(sizeof(int)*w);

	return r;
}

void freeMat(int ** mat, int l){
	for(int i = 0; i < l; i++){
		free(mat[i]);
	}
	free(mat);
}

enum _Cs {BLANK, RED, BLUE, V_RED, V_BLUE};
typedef enum _Cs CellState;

char strCellState(CellState cs){
	switch(cs){
		case V_RED:
			return 'v';
		case RED:
			return 'r';
		case V_BLUE:
			return '^';
		case BLUE:
			return 'b';
		default:
			return 'B';

	}
}

void printMat(int ** mat, int l, int w){
	int i, j;
	for(i = 0; i < l; i++){
		printf("%d |", i);
		for(j = 0; j < w; j++){
			printf(" %c ", strCellState(mat[i][j]));			
		}
		puts("| ");
	}
	puts("---");
}

int promptFrom(int min, int max, const char * pr){
	int ret;
	do{
		printf("%s:\n", pr);
	}while(scanf("%d", &ret) <= 0 && !inRange(ret, min, max));
	return ret;
}

int ** getMatFromUsr(int * l, int * w, int * b){
	do{
		printf("Enter the number of rows: ");
	}while(scanf("%d",l) <= 0);

	do{
		printf("Enter the number of columns: ");
	}while(scanf("%d", w) <= 0);

	*b = *l * *w;
	int ** ret = allocMat(*l, *w); 

	char in;
	do{
		do{
			printf("Enter 'r' to add a red cell, 'b' to add a blue one and 's' to solve the matrix.\n");
		}while(scanf("%c", &in) <= 0 || (in != 's' && in != 'r' && in != 'b'));
		
		if(in != 's'){
			int x = promptFrom(0, *l, "Enter the row number");
			int y = promptFrom(0, *w, "Enter the column number");

			ret[x][y] = (in == 'r')?RED:BLUE;
			(*b)--;

			printMat(ret, *l, *w);
		}


	}while(in != 's');

	return ret;
}

bool isSame(CellState l, CellState r){
	switch(l){
		case RED:
		case V_RED:
			return r == RED || r == V_RED;
		case BLUE:
		case V_BLUE:
			return r == BLUE || r == V_BLUE;
		default:
			if (l == BLANK || r == BLANK) return false;
			return l == r;
	}
}

int fillRows(int ** mat, int l, int w, bool exp){
	int s = 0, i, j;
	for(i = 0; i < l; i++){
		int rc = 0, bc = 0, lc = 0;
		for(j = 0; j < w; j++){
			if(isSame(mat[i][j], RED))
				rc++;
			else if(isSame(mat[i][j], BLUE))
				bc++;
			else
				lc++;
		}

		if(lc > l/2)
			continue;

		if(rc > l/2 || bc > l/2)
			return -1*s;

		if(rc < l/2 && bc < l/2)
			continue;
		
		CellState cs;
		if(rc == l/2)
			cs = (exp == true)?V_BLUE: BLUE;
		else
			cs = (exp == true)?V_RED: RED;

		for(j = 0; j < w; j++){
			if(mat[i][j] == BLANK){
				mat[i][j] = cs;
				s++;
			}	
		}
	}

	return s;
}

int fillCols(int ** mat, int l, int w, bool exp){
	int s = 0, i, j;
	for(i = 0; i < w; i++){
		int rc = 0, bc = 0, lc = 0;
		for(j = 0; j < l; j++){
			if(isSame(mat[j][i], RED))
				rc++;
			else if(isSame(mat[j][i], BLUE))
				bc++;
			else
				lc++;
		}

		if(lc > l/2)
			continue;

		if(rc > w/2 || bc > w/2)
			return -1*s;

		if(rc < w/2 && bc < w/2)
			continue;
		
		CellState cs;
		if(rc == w/2)
			cs = (exp == true)?V_BLUE: BLUE;
		else
			cs = (exp == true)?V_RED: RED;

		for(j = 0; j < l; j++){
			if(mat[j][i] == BLANK){
				mat[j][i] = cs;
				s++;
			}	
		}
	}

	return s;
}

int fillAdj(int ** mat, int l, int w, bool exp){
	int adjs[4][2] = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
	int s = 0, i, j, k;

	for(i = 0; i < l; i++){
		for(j = 0; j < w; j++){
			for(k = 0; k < 4; k++){
				int x = i + adjs[k][0], y = j + adjs[k][1];
				int r = i + 2*adjs[k][0], c = j + 2*adjs[k][1];
				if(inRange(x, 0, l) && inRange(y, 0, w) && isSame(mat[i][j], mat[x][y])){
					if(inRange(r, 0, l) && inRange(c, 0, w)){
						if(isSame(mat[i][j], mat[r][c]))
							return -1*s;
						else if(mat[r][c] != BLANK)
							continue;
						else if(isSame(mat[i][j], RED)){
							mat[r][c] = (exp == true)?V_BLUE:BLUE;
						}else{
							mat[r][c] = (exp == true)?V_RED: RED;
						}
						s++;
					}
				}else if(inRange(r, 0, l) && inRange(c, 0, w) && isSame(mat[i][j], mat[r][c])){
					if(mat[x][y] != BLANK) continue;
					else if(isSame(mat[i][j], RED))
						mat[x][y] = (exp == true)?V_BLUE:BLUE;
					else
						mat[x][y] = (exp == true)?V_RED:RED;

					s++;
				}
			}
		}
	}

	return s;
}

int obviousMoves(int ** mat, int l, int w, bool exp){
	int s = 0;
	int d = 0;
	do{
		d = 0;
		int r = fillRows(mat, l, w, exp);
		if(r < 0){
			return -1*s + r;
		}	
		d += r;
		r = fillCols(mat, l, w, exp);
		if(r < 0){
			return -1*s + -1*d + r;
		}	
		d += r;
		r = fillAdj(mat, l, w, exp);
		if(r < 0){
			return -1*s + -1*d + r;
		}	
		d += r;
		s += d;

	}while(d > 0);
	return s;
}

int checkRowsAndCols(int ** mat, int l, int w){
	//check rows
	int i, j;
	for(i = 0; i < l; i++){
		for(j = i + 1; j < l; j++){
			int k;
			for(k = 0; k < w && isSame(mat[i][k], mat[j][k]); k++);
			
			if(k >= w)
				return -1;
		}
	}


	//check cols
	for(i = 0; i < w; i++){
		for(j = i + 1; j < w; j++){
			int k;
			for(k = 0; k < l && isSame(mat[i][k], mat[j][k]); k++);
			
			if(k == l)
				return -1;
		}
	}

	return 1;
}

void expToBlank(int ** mat, int l, int w){
	int i, j;
	for(i = 0; i < l; i++)
		for(j = 0; j < w; j++)
			if(mat[i][j] > BLUE)
				mat[i][j] = BLANK;
}

int * setExp(int ** mat, int l, int w){
	int i, j;
	for(i = 0; i < l; i++)
		for(j = 0; j < w; j++)
			if(mat[i][j] == BLANK){
				mat[i][j] = V_RED;
				int * r = (int *) malloc(sizeof(int) * 2);
				r[0] = i;
				r[1] = j;
				return r;
			}

	return NULL;
}

int solve(int ** mat, int l, int w, int n_blank){
	int d, rc;
	bool e;
	int el, ew;
	do{
		d = obviousMoves(mat, l, w, e), rc = checkRowsAndCols(mat, l, w);
		if(d < 0 || rc < 0){
			if(!e)
				return -1;
			else{
				e = false;
				expToBlank(mat, l, w);
				mat[el][ew] = BLUE;
			}
		}else if(d == 0){
			e = true;
			int * p = setExp(mat, l, w);

			if(p == NULL){
				if(checkRowsAndCols(mat, l, w) >= 0)
					return 0;
				else
					return -1;
			}	

			el = p[0];
			ew = p[1];
			free(p);
		}else{
			n_blank -= d;
		}	
	}while(n_blank > 0);
	return 0;
}

int main(){
	int l, w, b;
	int ** m = getMatFromUsr(&l, &w, &b);

	int v = solve(m, l, w, b);

	printMat(m, l, w);
	freeMat(m, l);
	return v;
	
	/*int ** m = (int **)malloc(sizeof(int)*3);
	for(int i = 0; i < 3; i++){
		m[i] = (int *)malloc(sizeof(int)*3);
		m[i][0] = RED;
		m[i][1] = BLUE;
		m[i][2] = BLANK;
	}
	printMat(m, 3, 3);
	freeMat(m, 3);
	return 0;*/
}
