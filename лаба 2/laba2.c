#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#define MAX_LETTERS  26
#define MAX_TERMS    8
#define MAX_WORD_LEN 16
#define MAX_INPUT    256

typedef enum {
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV
} Operation;

typedef struct {
    int  term[MAX_TERMS][MAX_WORD_LEN];
    int  term_length[MAX_TERMS];
    Operation operator[MAX_TERMS];
    int  term_count;

    int  result[MAX_WORD_LEN];
    int  result_length;

    char letters[MAX_LETTERS];
    int  letter_count;

    int  cannot_be_zero[MAX_LETTERS];

    char original[MAX_INPUT];
} Puzzle;

int get_letter_index(Puzzle* puzzle, char letter) {
    for (int i = 0; i < puzzle->letter_count; i++) {
        if (puzzle->letters[i] == letter) return i;
    }
    if (puzzle->letter_count >= MAX_LETTERS) return -1;
    puzzle->letters[puzzle->letter_count] = letter;
    return puzzle->letter_count++;
}

int read_word(Puzzle* puzzle, const char* text, int* word, int* word_length) {
    int length = 0;
    while (text[length] && (text[length] >= 'A' && text[length] <= 'Z')) {
        if (length >= MAX_WORD_LEN) return -1;
        int index = get_letter_index(puzzle, text[length]);
        if (index < 0) return -1;
        word[length++] = index;
    }
    if (length == 0) return -1;
    *word_length = length;
    puzzle->cannot_be_zero[word[0]] = 1;
    return length;
}

int parse_rebus(const char* input, Puzzle* puzzle) {
    memset(puzzle, 0, sizeof(*puzzle));

    if (strlen(input) >= MAX_INPUT) return -1;
    strcpy(puzzle->original, input);

    char clean[MAX_INPUT];
    int n = 0;
    for (const char* ch = input; *ch; ch++) {
        if (isspace((unsigned char)*ch)) continue;
        if (n >= MAX_INPUT - 1) return -1;
        clean[n++] = *ch;
    }
    clean[n] = 0;

    char* equals = strchr(clean, '=');
    if (!equals) return -1;
    *equals = 0;
    char* right_side = equals + 1;

    const char* cursor = clean;
    while (*cursor) {
        if (puzzle->term_count >= MAX_TERMS) return -1;

        int index = puzzle->term_count;
        int read = read_word(puzzle, cursor,
            puzzle->term[index],
            &puzzle->term_length[index]);
        if (read < 0) return -1;
        puzzle->term_count++;
        cursor += read;

        if (!*cursor) break;

        switch (*cursor) {
        case '+': puzzle->operator[index] = OP_ADD; break;
        case '-': puzzle->operator[index] = OP_SUB; break;
        case '*': puzzle->operator[index] = OP_MUL; break;
        case '/': puzzle->operator[index] = OP_DIV; break;
        default:  return -1;
        }
        cursor++;
    }

    if (read_word(puzzle, right_side,
        puzzle->result,
        &puzzle->result_length) < 0) return -1;

    if (right_side[puzzle->result_length] != 0) return -1;

    return (puzzle->term_count >= 2) ? 0 : -1;
}

long long word_to_number(const int* word, int length, const int* digits) {
    long long value = 0;
    for (int i = 0; i < length; i++) {
        value = value * 10 + digits[word[i]];
    }
    return value;
}

int evaluate_expression(const Puzzle* puzzle, const int* digits, long long* output) {
    long long values[MAX_TERMS];
    for (int i = 0; i < puzzle->term_count; i++) {
        values[i] = word_to_number(puzzle->term[i],
            puzzle->term_length[i],
            digits);
    }

    long long reduced[MAX_TERMS];
    Operation reduced_op[MAX_TERMS];
    int reduced_count = 0;

    reduced[0] = values[0];
    for (int i = 0; i < puzzle->term_count - 1; i++) {
        Operation op = puzzle->operator[i];
        long long next = values[i + 1];

        if (op == OP_MUL) {
            reduced[reduced_count] *= next;
        }
        else if (op == OP_DIV) {
            if (next == 0) return 0;
            if (reduced[reduced_count] % next != 0) return 0;
            reduced[reduced_count] /= next;
        }
        else {
            reduced_count++;
            reduced[reduced_count] = next;
            reduced_op[reduced_count - 1] = op;
        }
    }

    long long result = reduced[0];
    for (int i = 0; i < reduced_count; i++) {
        if (reduced_op[i] == OP_ADD) result += reduced[i + 1];
        else                         result -= reduced[i + 1];
    }

    *output = result;
    return 1;
}

int is_solution(const Puzzle* puzzle, const int* digits) {
    for (int i = 0; i < puzzle->letter_count; i++) {
        if (puzzle->cannot_be_zero[i] && digits[i] == 0) return 0;
    }

    long long left_value;
    if (!evaluate_expression(puzzle, digits, &left_value)) return 0;

    long long right_value = word_to_number(puzzle->result,
        puzzle->result_length,
        digits);

    return left_value == right_value;
}

void build_answer(const Puzzle* puzzle, const int* digits, char* output, size_t output_size) {
    size_t out = 0;
    for (const char* ch = puzzle->original; *ch && out + 1 < output_size; ch++) {
        if (*ch >= 'A' && *ch <= 'Z') {
            for (int i = 0; i < puzzle->letter_count; i++) {
                if (puzzle->letters[i] == *ch) {
                    output[out++] = (char)('0' + digits[i]);
                    break;
                }
            }
        }
        else {
            output[out++] = *ch;
        }
    }
    output[out] = 0;
}

int search(const Puzzle* puzzle, int* digits, int* used, int depth, int* answer) {
    if (depth == puzzle->letter_count) {
        if (is_solution(puzzle, digits)) {
            memcpy(answer, digits, puzzle->letter_count * sizeof(int));
            return 1;
        }
        return 0;
    }

    for (int digit = 0; digit <= 9; digit++) {
        if (used[digit]) continue;

        used[digit] = 1;
        digits[depth] = digit;

        if (search(puzzle, digits, used, depth + 1, answer)) return 1;

        used[digit] = 0;
    }
    return 0;
}

int solve(const Puzzle* puzzle, int* answer) {
    int digits[MAX_LETTERS] = { 0 };
    int used[10] = { 0 };
    return search(puzzle, digits, used, 0, answer);
}

char* solve_rebus(const char* input) {
    Puzzle puzzle;
    if (parse_rebus(input, &puzzle) != 0) {
        return NULL;
    }

    int answer[MAX_LETTERS];
    if (!solve(&puzzle, answer)) {
        return NULL;
    }

    char* result_string = (char*)malloc(512);
    if (result_string) {
        build_answer(&puzzle, answer, result_string, 512);
    }
    return result_string;
}

int main(void) {
    char input[MAX_INPUT];

    printf("Enter a rebus:\n");
    if (!fgets(input, sizeof(input), stdin)) {
        fprintf(stderr, "input read error\n");
        return 1;
    }

    size_t len = strlen(input);
    if (len > 0 && input[len - 1] == '\n') input[len - 1] = 0;

    if (strlen(input) == 0) {
        fprintf(stderr, "empty input\n");
        return 1;
    }

    clock_t start = clock();
    char* solution = solve_rebus(input);
    clock_t finish = clock();

    if (!solution) {
        printf("No solution found\n");
        return 1;
    }

    printf("Solution: %s\n", solution);
    printf("Time: %.4f s\n", (double)(finish - start) / CLOCKS_PER_SEC);

    free(solution);
    return 0;
}