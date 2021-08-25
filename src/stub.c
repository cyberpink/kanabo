#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

value popcount(value v) {
#ifdef ARCH_SIXTYFOUR
  return Val_int(__builtin_popcountll(Long_val(v) & ~((uint64_t)1 << 63)));
#else
  return Val_int(__builtin_popcount(Int_val(v) & ~((uint32_t)1 << 31)));
#endif
}

intnat ctz_untagged(intnat v) {
#ifdef ARCH_SIXTYFOUR
  return __builtin_ctzll(v);
#else
  return __builtin_ctz(v);
#endif
}

value ctz(value v) {
  return Val_int(ctz_untagged(Int_val(v)));
}

intnat clz_untagged(value v) {
#ifdef ARCH_SIXTYFOUR
  return __builtin_clzll(v);
#else
  return __builtin_clz(v);
#endif
}

CAMLprim value clz(value v) {
  return Val_int(clz_untagged(v));
}
