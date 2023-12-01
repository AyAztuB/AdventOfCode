#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(void)
{
    FILE* f = fopen("input", "r");
    char* line = NULL;
    size_t len = 0;
    int r = 0;
    while ((r = getline(&line, &len, f)) > 0)
    {
        continue;
        // TODO
    }
    free(line);
    fclose(f);
    return 0;
}
