#include <stdio.h>
#include <stdarg.h>
double tong (int sum, ...) {
    double flag = 0.0;
    va_list ptr;
    va_start (ptr, sum);
    for (int i = 0; i < sum; i++)
    {
        if (*ptr != 0){ 
            flag = flag + va_arg(ptr, int);
        } else {
            flag = flag + va_arg(ptr, double);
        }
    }
    va_end(ptr);
    return flag;
}

int main(){
    printf("Tong: %.f\n", tong (5, 7, 3.4, 3, 8, 1));
    return 0;
} 