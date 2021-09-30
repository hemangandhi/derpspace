#include <stdio.h>

#include "class.h"

DEFINE_CLASS(Point, int x; int y;);

ADD_CLASS_METHOD(int, Point, euclidDist, Point* other) {
  int d_x = (this->x < other->x) ? other->x - this->x : this->x - other->x;
  int d_y = (this->y < other->y) ? other->y - this->y : this->y - other->y;
  return d_x + d_y;
}

ADD_GETTER_METHOD(void, Point, print) {
  printf("(%d, %d)\n", this->x, this->y);
}

int main(int argc, char ** argv) {
  Point x = {1, 1};
  Point y = {0, 0};

  INVOKE_GETTER_METHOD(x, print);
  printf("%d\n", INVOKE_METHOD(x, euclidDist, &y));
  Point * px = &x;
  printf("%d\n", INVOKE_PTR_METHOD(px, euclidDist, &y));
  
  return 0;
}
