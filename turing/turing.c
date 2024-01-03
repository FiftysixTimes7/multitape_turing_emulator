#include "kvec.h"
#include <ctype.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// options
static struct option options[] = {{"verbose", no_argument, NULL, 'v'},
                                  {"help", no_argument, NULL, 'h'},
                                  {NULL, 0, NULL, 0}};
static bool verbose = false;
// TM data structures
struct TM_Delta {
  char *old, *new, *dir;
  unsigned next;
};
struct TM_State {
  char *name;
  bool final;
  kvec_t(struct TM_Delta) delta; // bundled within state
};
struct TM_Tape {
  kvec_t(char) pos, neg; // neg contains: -1 -2 -3 ...
  int head, left, right; // index with sign
  int left_p, right_p;   // printable boundaries
};
struct TM { // everything needed for emulation
  kvec_t(struct TM_State) Q;
  char *S, *G; // symbol sets stored in strings
  unsigned N;
  kvec_t(struct TM_Tape) T;
  struct TM_State *q;
  unsigned step;
  bool accept;
} tm;
#define STATE(idx) (&kv_A(tm.Q, (idx)))
#define DELTA(state, idx) (&kv_A((state)->delta, (idx)))
// tape operations
#define TAPE(tape_idx) (&kv_A(tm.T, (tape_idx)))
#define TAPE_FETCH(tape, idx)                                                  \
  ((idx) < 0 ? kv_A((tape)->neg, -1 - (idx)) : kv_A((tape)->pos, (idx)))
#define TAPE_HEAD(tape) (TAPE_FETCH((tape), (tape)->head))
static inline void tape_init(unsigned tape_idx, const char *init) {
  struct TM_Tape *tape = TAPE(tape_idx);
  tape->head = tape->left = 0;
  kv_init(tape->pos);
  kv_init(tape->neg);
  while (*init)
    kv_push(char, tape->pos, *(init++));
  tape->right = kv_size(tape->pos) - 1;
}
static inline void tape_move(unsigned tape_idx, char dir, char new) {
  struct TM_Tape *tape = TAPE(tape_idx);
  tape->head < 0 ? (kv_A(tape->neg, -1 - tape->head) = new)
                 : (kv_A(tape->pos, tape->head) = new);
  switch (dir) {
  case 'l':
    if (--tape->head < tape->left) {
      --tape->left;
      kv_push(char, tape->neg, '_');
    }
    break;
  case 'r':
    if (++tape->head > tape->right) {
      ++tape->right;
      kv_push(char, tape->pos, '_');
    }
    break;
  }
}
// print and output
static inline void refresh_tape_index(unsigned tape_idx) {
  struct TM_Tape *tape = TAPE(tape_idx);
  for (tape->left_p = tape->left; tape->left_p < tape->head; ++tape->left_p)
    if (TAPE_FETCH(tape, tape->left_p) != '_')
      break;
  for (tape->right_p = tape->right; tape->right_p > tape->head; --tape->right_p)
    if (TAPE_FETCH(tape, tape->right_p) != '_')
      break;
}
static inline unsigned digits(const unsigned n) {
  return n < 10 ? 1 : 1 + digits(n / 10);
}
static inline void print_snapshot() {
  fprintf(stdout, "%-*s: %u\n%-*s: %s\n%-*s: %s\n", digits(tm.N) + 6, "Step",
          tm.step, digits(tm.N) + 6, "State", tm.q->name, digits(tm.N) + 6,
          "Acc", tm.accept ? "Yes" : "No"); // "-" for left alignment
  for (unsigned i = 0; i < tm.N; ++i) {
    refresh_tape_index(i);
    fprintf(stdout, "Index%-*u : ", digits(tm.N), i);
    for (int j = TAPE(i)->left_p; j < TAPE(i)->right_p; ++j)
      fprintf(stdout, "%d ", abs(j));
    fprintf(stdout, "%d\nTape%-*u  : ", abs(TAPE(i)->right_p), digits(tm.N), i);
    for (int j = TAPE(i)->left_p; j < TAPE(i)->right_p; ++j)
      fprintf(stdout, "%-*c ", digits(abs(j)), TAPE_FETCH(TAPE(i), j));
    fprintf(stdout, "%-*c\nHead%-*u  : ", digits(abs(TAPE(i)->right_p)),
            TAPE_FETCH(TAPE(i), TAPE(i)->right_p), digits(tm.N), i);
    for (int j = TAPE(i)->left_p; j < TAPE(i)->head; ++j)
      fprintf(stdout, "%*c ", digits(abs(j)), ' ');
    fputs("^\n", stdout);
  }
  fputs("---------------------------------------------\n", stdout);
}
static inline void print_result() {
  refresh_tape_index(0);
  struct TM_Tape *tape0 = TAPE(0);
  int l = tape0->left_p, r = tape0->right_p; // strip space regardless of head
  while (TAPE_FETCH(tape0, l) == '_' && l <= r)
    ++l;
  while (TAPE_FETCH(tape0, r) == '_' && l <= r)
    --r;
  char *result = malloc(r - l + 2);
  size_t result_len = 0;
  for (int i = l; i <= r; ++i)
    result[result_len++] = TAPE_FETCH(tape0, i);
  result[result_len] = '\0';
  if (verbose)
    fprintf(stdout,
            "%s\n"
            "Result: %s\n"
            "==================== END ====================\n",
            tm.accept ? "ACCEPTED" : "UNACCEPTED", result);
  else
    fprintf(stdout, "(%s) %s\n", tm.accept ? "ACCEPTED" : "UNACCEPTED", result);
  free(result);
}
// parse .tm file
#define PARSE_LINE(lbuf, format, sbuf, alen)                                   \
  do {                                                                         \
    if (!sscanf((lbuf), (format), (sbuf), (alen)) || *(alen) != strlen(lbuf))  \
      goto parse_error;                                                        \
  } while (0) // no check for dup delim
#define CMP_LINE(lbuf, format, alen)                                           \
  (sscanf((lbuf), (format), (alen)) || *(alen) != strlen(lbuf))
static inline unsigned lookup_state(const char *name) {
  for (unsigned i = 0; i < kv_size(tm.Q); ++i)
    if (strcmp(STATE(i)->name, name) == 0)
      return i;
  fprintf(stderr, "syntax error: no state named %s\n", name);
  exit(EXIT_FAILURE);
}
static inline void parse_tm(const char *tm_path) {
  FILE *tm_file = fopen(tm_path, "r");
  if (!tm_file) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  unsigned bufsz = (1 << 10), offset = 0, len;
  char *lbuf = malloc(bufsz), *comment, *sbuf = NULL;
  while (fgets(lbuf + offset, bufsz - offset, tm_file) &&
         (len = strlen(lbuf))) {
    if (!feof(tm_file) && lbuf[len - 1] != '\n') {
      offset = bufsz - 1, bufsz <<= 1;
      lbuf = realloc(lbuf, bufsz);
      continue; // read until newline
    }
    offset = 0;
    comment = strchr(lbuf, ';');
    if (comment) // strip comments
      *comment = '\0', len = comment - lbuf;
    while (len && isspace(lbuf[len - 1])) // strip space on the right
      lbuf[(len--) - 1] = '\0';
    if (!len) // skip empty line
      continue;
    if (len < 2)
      goto parse_error;
    sbuf = realloc(sbuf, bufsz), len = 0; // reset len! (to check more errors)
    if (lbuf[0] == '#')
      switch (lbuf[1]) {
      case 'Q':
        PARSE_LINE(lbuf, "#Q = {%[a-zA-Z0-9_, ]}%n", sbuf, &len);
        for (char *tok = strtok(sbuf, ", "); tok; tok = strtok(NULL, ", "))
          kv_push(struct TM_State, tm.Q,
                  (struct TM_State){.name = strdup(tok)});
        break;
      case 'S':
        if (!CMP_LINE(lbuf, "#S = {}%n", &len))
          sbuf[0] = '\0';
        else
          PARSE_LINE(lbuf, "#S = {%[^;{}*_]}%n", sbuf, &len);
        tm.S = malloc(strlen(sbuf) / 2 + 2), tm.S[0] = '\0'; // note "+2"
        for (char *tok = strtok(sbuf, ", "); tok; tok = strtok(NULL, ", "))
          if (strlen(tok) != 1)
            goto parse_error;
          else
            strcat(tm.S, tok);
        break;
      case 'G':
        PARSE_LINE(lbuf, "#G = {%[^;{}*]}%n", sbuf, &len);
        tm.G = malloc(strlen(sbuf) / 2 + 2), tm.G[0] = '\0';
        for (char *tok = strtok(sbuf, ", "); tok; tok = strtok(NULL, ", "))
          if (strlen(tok) != 1)
            goto parse_error;
          else
            strcat(tm.G, tok);
        for (unsigned i = 0; i < strlen(tm.S); ++i)
          if (strchr(tm.G, tm.S[i]) == NULL)
            goto parse_error;
        if (strchr(tm.G, '_') == NULL)
          goto parse_error;
        break;
      case 'q':
        PARSE_LINE(lbuf, "#q0 = %[a-zA-Z0-9_]%n", sbuf, &len);
        tm.q = STATE(lookup_state(sbuf));
        break;
      case 'B':
        if (CMP_LINE(lbuf, "#B = _%n", &len))
          goto parse_error;
        break;
      case 'F':
        if (!CMP_LINE(lbuf, "#F = {}%n", &len))
          break;
        PARSE_LINE(lbuf, "#F = {%[a-zA-Z0-9_, ]}%n", sbuf, &len);
        for (char *tok = strtok(sbuf, ", "); tok; tok = strtok(NULL, ", "))
          STATE(lookup_state(tok))->final = true;
        break;
      case 'N':
        PARSE_LINE(lbuf, "#N = %u%n", &tm.N, &len);
        if (!tm.N || tm.N > __INT_MAX__)
          goto parse_error;
        break;
      default:
        goto parse_error;
      }
    else { // parse delta
      char *sold = strtok(lbuf, " "), *told = strtok(NULL, " "),
           *tnew = strtok(NULL, " "), *dir = strtok(NULL, " "),
           *snew = strtok(NULL, " ");
      if (sold == NULL || told == NULL || tnew == NULL || dir == NULL ||
          snew == NULL || strlen(told) != tm.N || strlen(tnew) != tm.N ||
          strlen(dir) != tm.N)
        goto parse_error;
      unsigned isold = lookup_state(sold), isnew = lookup_state(snew);
      for (unsigned i = 0; i < tm.N; ++i)
        if ((strchr(tm.G, told[i]) == NULL && told[i] != '*') ||
            (strchr(tm.G, tnew[i]) == NULL && tnew[i] != '*') ||
            (told[i] != '*' && tnew[i] == '*') || strchr("lr*", dir[i]) == NULL)
          goto parse_error;
      struct TM_Delta delta = {.old = strdup(told),
                               .new = strdup(tnew),
                               .dir = strdup(dir),
                               .next = isnew};
      kv_push(struct TM_Delta, (STATE(isold)->delta), delta);
    }
  }
  free(lbuf);
  free(sbuf);
  fclose(tm_file);
  return;
parse_error:
  fprintf(stderr, "syntax error: cannot accept %s\n", lbuf);
  // printf("%s %s %s %u\n", tm.S, tm.G, tm.q->name, tm.N);
  // for (unsigned i = 0; i < kv_size(tm.Q); ++i) {
  //   struct TM_State *q = STATE(i);
  //   printf("%s: %d\n", q->name, q->final);
  //   for (unsigned j = 0; j < kv_size(q->delta); ++j)
  //     printf("%s %s %s %s\n", DELTA(q, j)->old, DELTA(q, j)->new,
  //            DELTA(q, j)->dir, STATE(DELTA(q, j)->next)->name);
  // }
  exit(EXIT_FAILURE);
}
// check input and initialize tape
static inline void initialize_tape(const char *input) {
  const char *illegal;
  for (illegal = input; *illegal; ++illegal)
    if (strchr(tm.S, *illegal) == NULL) {
      if (verbose)
        fprintf(stderr,
                "Input: %s\n"
                "==================== ERR ====================\n"
                "error: Symbol \"%c\" in input is not defined in the "
                "set of input symbols\n"
                "Input: %s\n"
                "       %*c\n"
                "==================== END ====================\n",
                input, *illegal, input, (int)(illegal - input + 1), '^');
      else
        fputs("illegal input string\n", stderr);
      exit(EXIT_FAILURE);
    }
  if (verbose)
    fprintf(stdout,
            "Input: %s\n"
            "==================== RUN ====================\n",
            input);
  kv_push(struct TM_Tape, tm.T, (struct TM_Tape){});
  if (*input)
    tape_init(0, input);
  else // empty input!
    tape_init(0, "_");
  for (unsigned i = 1; i < tm.N; ++i) {
    kv_push(struct TM_Tape, tm.T, (struct TM_Tape){});
    tape_init(i, "_");
  }
}
// core emulation
static inline void emulate_turing() {
  bool cont;
  unsigned i, j;
  do {
    tm.accept = tm.accept || tm.q->final;
    if (verbose)
      print_snapshot();
    cont = false;
    for (i = 0; i < kv_size(tm.q->delta); ++i) { // match rules
      struct TM_Delta *delta = DELTA(tm.q, i);
      for (j = 0; j < tm.N; ++j) // check each tape
        if (delta->old[j] != TAPE_HEAD(TAPE(j)) &&
            !(delta->old[j] == '*' && TAPE_HEAD(TAPE(j)) != '_'))
          break;
      if (j == tm.N) { // step forward
        // printf("match rule %s %s %s\n", delta.old, delta.new, delta.dir);
        for (j = 0; j < tm.N; ++j)
          tape_move(j, delta->dir[j],
                    delta->new[j] == '*' ? TAPE_HEAD(TAPE(j)) : delta->new[j]);
        tm.q = STATE(delta->next), cont = true, ++tm.step;
        break;
      }
    }
  } while (cont);
}
// release resource
static inline void free_tm() {
  free(tm.S), free(tm.G);
  for (unsigned i = 0; i < kv_size(tm.Q); ++i) {
    tm.q = STATE(i);
    free(tm.q->name);
    for (unsigned j = 0; j < kv_size(tm.q->delta); ++j)
      free(DELTA(tm.q, j)->old), free(DELTA(tm.q, j)->new),
          free(DELTA(tm.q, j)->dir);
    kv_destroy(tm.q->delta);
  }
  kv_destroy(tm.Q);
  for (unsigned i = 0; i < kv_size(tm.T); ++i) {
    kv_destroy(TAPE(i)->pos);
    kv_destroy(TAPE(i)->neg);
  }
  kv_destroy(tm.T);
}

int main(int argc, char **argv) {
  int c;
  while ((c = getopt_long(argc, argv, "vh", options, NULL)) != -1)
    switch (c) {
    case 'v':
      verbose = true;
      break;
    case 'h':
      fputs("usage: turing [-v|--verbose] [-h|--help] <tm> <input>\n", stdout);
      return EXIT_SUCCESS;
    default:
      fputs("usage: turing [-v|--verbose] [-h|--help] <tm> <input>\n", stderr);
      return EXIT_FAILURE;
    }
  if (optind + 2 != argc) {
    fputs("usage: turing [-v|--verbose] [-h|--help] <tm> <input>\n", stderr);
    return EXIT_FAILURE;
  }
  const char *tm_path = argv[optind], *input = argv[++optind];

  parse_tm(tm_path);
  initialize_tape(input);
  emulate_turing();
  print_result();
  free_tm();
  return EXIT_SUCCESS;
}
