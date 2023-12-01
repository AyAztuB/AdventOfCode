#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static int is_digit(char s[static 1], int flag)
{
    if (*s >= '0' && *s <= '9')
        return *s - '0';
    if (!flag)
        return -1;
    static const char* cmp[9] = {
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
    };
    for (int i = 0; i < 9; i++)
        if (!strncmp(s, cmp[i], strlen(cmp[i])))
            return i+1;
    return -1;
}

static int read_digits(char line[static 1], int flag)
{
    // No check for errors, we asserting line is always in the good format
    int res = 0;
    int i = 0;
    int cur = -1;
    for (; line[i] && (cur = is_digit(line + i, flag)) == -1; i++);
    res += cur;
    for (i = strlen(line) - 1; i >= 0 && (cur = is_digit(line + i, flag)) == -1; i--);
    res = res * 10 + cur;
    return res;
}

int main(void)
{
    FILE* f = fopen("input", "r");
    char* line = NULL;
    size_t len = 0;
    int r = 0;
    int sum = 0;
    int sum2 = 0;
    while ((r = getline(&line, &len, f)) > 0)
    {
        sum += read_digits(line, 0);
        sum2 += read_digits(line, 1);
    }
    free(line);
    fclose(f);
    printf("part1: %d\n", sum);
    printf("part2: %d\n", sum2);
    return 0;
}
