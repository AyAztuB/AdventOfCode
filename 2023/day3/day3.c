#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#define BUFFER_SIZE 1000000

static inline int is_digit(char c)
{
    return c >= '0' && c <= '9';
}

static inline int is_dot(char c)
{
    return c == '.';
}

static inline int is_sym(char c)
{
    return !is_digit(c) && !is_dot(c);
}

static int get_line_len(char buffer[static 1])
{
    int len = 0;
    for (; buffer[len] && buffer[len] != '\n'; len++);
    return len;
}

static inline int get_nb_lines(char buffer[static 1], int line_len)
{
    return strlen(buffer) / (line_len + 1);
}

struct position {
    int x;
    int y;
};

static inline char* get_index(char buffer[static 1], struct position p, struct position max)
{
    if (p.x < 0 || p.x >= max.x || p.y < 0 || p.y >= max.y)
        return NULL;
    return buffer + (p.x * (max.y + 1) + p.y);
}

static int has_sym_range(char line[static 1], int start, int end, int max_line)
{
    while (start < 0) start++;
    while (end >= max_line) end--;
    for (int i = start; i <= end; i++)
        if (is_sym(line[i]))
            return 1;
    return 0;
}

static int get_number(char buffer[static 1], struct position p, struct position max, int* start)
{
    int num = atoi(get_index(buffer, p, max));
    int last = p.y;
    char* curr;
    while ((curr = get_index(buffer, (struct position){ .x = p.x, .y = last }, max)) != NULL && is_digit(*curr))
        last++;
    *start = last-1;
    if (p.x > 0 && has_sym_range(&buffer[(p.x - 1) * (max.y + 1)], p.y-1, last, max.y))
        return num;
    if (p.x < max.x-1 && has_sym_range(&buffer[(p.x+1) * (max.y+1)], p.y-1, last, max.y))
        return num;
    if (p.y > 0 && (curr = get_index(buffer, (struct position){ .x = p.x, .y = p.y-1 }, max)) != NULL
        && is_sym(*curr))
        return num;
    if (p.y < max.y-1 && (curr = get_index(buffer, (struct position){ .x = p.x, .y = last }, max)) != NULL
        && is_sym(*curr))
        return num;
    return 0;
}

static int read_num(char line[static 1], int* start, int max_line)
{
    while (*start > 0 && is_digit(line[*start -1])) *start-=1;
    int num = atoi(line + *start);
    while (*start < max_line && is_digit(line[*start])) *start += 1;
    *start -= 1;
    return num;
}

static int check_gear_range(char line[static 1], int start, int last, int max_line, int* nb_num)
{
    while (start < 0) start++;
    while (last >= max_line) last--;
    int local_res = 1;
    for (int i = start; i <= last; i++)
    {
        if (is_digit(line[i]))
        {
            local_res *= read_num(line, &i, max_line);
            *nb_num += 1;
        }
    }
    return local_res;
}

static int get_gear(char buffer[static 1], struct position p, struct position max)
{
    int nb_num = 0;
    int local_res = 1;
    if (p.x > 0)
        local_res *= check_gear_range(&buffer[(p.x-1) * (max.y+1)], p.y-1, p.y+1, max.y, &nb_num);
    if (p.x < max.x-1)
        local_res *= check_gear_range(&buffer[(p.x+1) * (max.y+1)], p.y-1, p.y+1, max.y, &nb_num);
    local_res *= check_gear_range(&buffer[p.x * (max.y+1)], p.y-1, p.y+1, max.y, &nb_num);
    return nb_num == 2 ? local_res : 0;
}

static void solve(char buffer[static 1])
{
    int line_len = get_line_len(buffer);
    int nb_line = get_nb_lines(buffer, line_len);
    int part1 = 0, part2 = 0;
    struct position max = { .x = nb_line, .y = line_len };
    for (int i = 0; i < nb_line; i++)
    {
        for (int j = 0; j < line_len; j++)
        {
            char curr = *get_index(buffer, (struct position){ .x = i, .y = j }, max);
            if (is_digit(curr))
                part1 += get_number(buffer, (struct position){ .x = i, .y = j }, max, &j);
            else if (curr == '*')
                part2 += get_gear(buffer, (struct position){ .x = i, .y = j }, max);
        }
    }
    printf("part1: %d\npart2: %d\n", part1, part2);
}

int main(void)
{
    char buffer[BUFFER_SIZE] = { 0 };
    int fd = open("input", O_RDONLY);
    read(fd, buffer, BUFFER_SIZE);
    close(fd);
    solve(buffer);
    return 0;
}
