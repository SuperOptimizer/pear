#include "rv32gc.h"
#include "csr.h"
#include <stdlib.h>

int rv_init(rv_t *rv) {
  memset(rv, 0, sizeof(rv_t));
  rv->ram = malloc(MEMORY_SIZE);
  if (!rv->ram) return -1;
  rv_csr_init(rv);
  return 0;
}

int rv_free(rv_t *rv) {
  if (!rv) return -1;
  if (!rv->ram) return -1;
  free(rv->ram);
  rv->ram = NULL;
  return 0;
}