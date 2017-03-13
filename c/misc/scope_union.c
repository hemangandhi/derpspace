
int floatToInt(union {int i; float f;} test){
  return i;
}

int main(int argc, char ** argv){
  floatToInt(5.5f);
}
