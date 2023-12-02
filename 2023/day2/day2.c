#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static int game_id(char** line)
{
    *line += strlen("Game ");
    int id = atoi(*line);
    for (; **line != ':'; *line += 1);
    *line += 1;
    return id;
}

static int get_one_value(char** line, int* res)
{
    static const char* colors[3] = { "red", "green", "blue" };
    for (; **line && **line == ' '; *line += 1);
    *res = atoi(*line);
    for (; **line && (**line == ' ' || (**line >= '0' && **line <= '9')); *line += 1);
    for (int i = 0; i < 3; i++)
        if (!strncmp(*line, colors[i], strlen(colors[i])))
            return i;
    return -1; // unreachable
}

static int get_rgb(char** line, int rgb[static 3])
{
    int is_possible = 1;
    while (**line && **line != ';' && **line != '\n')
    {
        int x = 0;
        int color = get_one_value(line, &x);
        rgb[color] = x;
        is_possible &= (x <= 12 + color);
        for (; **line && **line != ',' && **line != ';' && **line != '\n'; *line += 1);
        if (**line == ',')
            *line += 1;
    }
    if (**line == ';')
        *line += 1;
    return is_possible;
}

static int is_possible(char line[static 1], int* power)
{
    int id = game_id(&line);
    int m_rgb[3] = { 0 };
    while (*line && *line != '\n')
    {
        int rgb[3] = { 0 };
        if (!get_rgb(&line, rgb))
            id = -1;
        for (int i = 0; i < 3; i++)
            if (rgb[i] > m_rgb[i])
                m_rgb[i] = rgb[i];
    }
    *power = m_rgb[0] * m_rgb[1] * m_rgb[2];
    return id;
}

int main(void)
{
    FILE* f = fopen("input", "r");
    char* line = NULL;
    size_t len = 0;
    int r = 0;
    int sum = 0;
    int power_sum = 0;
    while ((r = getline(&line, &len, f)) > 0)
    {
        int power = 0;
        int id = is_possible(line, &power);
        if (id >= 0)
            sum += id;
        power_sum += power;
    }
    free(line);
    fclose(f);
    printf("part1: %d\n", sum);
    printf("part2: %d\n", power_sum);
    return 0;
}
