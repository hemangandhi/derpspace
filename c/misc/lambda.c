
int (*v)(const void *, const void *) mkEqFn(int (*cmp)(const void * v, const void * w)){
    int rv(const void * a, const void * b){
        return cmp(a, b) == 0;
    }
    return rv;
}
