#define SET_HEADER(name, type)                                                 \
  typedef struct name name;                                                    \
  struct name {                                                                \
    type key;                                                                  \
    name *l, *r;                                                               \
  };                                                                           \
  name *create_##name(type p);                                                 \
  void free_##name(name *p);                                                   \
  void insert_##name(name **root, type value);                                 \
  void erase_##name(name **root, type value);                                  \
  bool exist_##name(const name *root, const type value);                       \
  void iterate_##name(const name *root, void *context,                         \
                      void (*func)(void *, type))

#define SET_IMPL(name, type)                                                   \
  name *create_##name(type p) {                                                \
    name *res = malloc(sizeof(name));                                          \
    res->key = p;                                                              \
    res->l = NULL;                                                             \
    res->r = NULL;                                                             \
    return res;                                                                \
  }                                                                            \
  void free_##name(name *p) {                                                  \
    if (p) {                                                                   \
      free_##name(p->l);                                                       \
      free_##name(p->r);                                                       \
      free(p);                                                                 \
    }                                                                          \
  }                                                                            \
  name *merge_##name(name *l, name *r) {                                       \
    if (!l)                                                                    \
      return r;                                                                \
    if (!r)                                                                    \
      return l;                                                                \
    if (l->key > r->key) {                                                     \
      l->r = merge_##name(l->r, r);                                            \
      return l;                                                                \
    } else {                                                                   \
      r->l = merge_##name(l, r->l);                                            \
      return r;                                                                \
    }                                                                          \
  }                                                                            \
                                                                               \
  bool exist_##name(const name *p, const type x) {                             \
    if (!p)                                                                    \
      return false;                                                            \
    if (p->key == x)                                                           \
      return true;                                                             \
    if (p->key > x)                                                            \
      return exist_##name(p->l, x);                                            \
    return exist_##name(p->r, x);                                              \
  }                                                                            \
                                                                               \
  typedef struct {                                                             \
    name *first, *second;                                                      \
  } name##_pair;                                                               \
                                                                               \
  name##_pair split_##name(name *p, type x) {                                  \
    name##_pair res;                                                           \
    res.first = NULL;                                                          \
    res.second = NULL;                                                         \
    if (!p)                                                                    \
      return res;                                                              \
    if (p->key >= x) {                                                         \
      name##_pair q = split_##name(p->r, x);                                   \
      p->r = q.first;                                                          \
      res.first = p;                                                           \
      res.second = q.second;                                                   \
    } else {                                                                   \
      name##_pair q = split_##name(p->l, x);                                   \
      p->l = q.second;                                                         \
      res.first = q.first;                                                     \
      res.second = p;                                                          \
    }                                                                          \
    return res;                                                                \
  }                                                                            \
                                                                               \
  type most_right_##name(name *p) {                                            \
    if (p == NULL)                                                             \
      return NULL;                                                             \
    if (p->l)                                                                  \
      return most_right_##name(p->l);                                          \
    return p->key;                                                             \
  }                                                                            \
                                                                               \
  void insert_##name(name **root, type x) {                                    \
    if (!root) {                                                               \
      *root = create_##name(x);                                                \
      return;                                                                  \
    }                                                                          \
    name##_pair q = split_##name(*root, x);                                    \
    if (most_right_##name(q.first) == x) {                                     \
      *root = merge_##name(q.first, q.second);                                 \
      return;                                                                  \
    }                                                                          \
    name *t = create_##name(x);                                                \
    *root = merge_##name(q.first, merge_##name(t, q.second));                  \
  }                                                                            \
                                                                               \
  void erase_##name(name **root, type x) {                                     \
    if (!root)                                                                 \
      return;                                                                  \
                                                                               \
    if ((*root)->l == NULL && (*root)->r == NULL) {                            \
      if ((*root)->key == x) {                                                 \
        free_##name(*root);                                                    \
        *root = NULL;                                                          \
      }                                                                        \
      return;                                                                  \
    }                                                                          \
    name##_pair q1 = split_##name(*root, x);                                   \
    name##_pair q2 = split_##name(q1.first, x - 1);                            \
    *root = merge_##name(q2.first, q1.second);                                 \
  }                                                                            \
  void iterate_##name(const name *p, void *context,                            \
                      void (*func)(void *, type)) {                            \
    if (p == NULL)                                                             \
      return;                                                                  \
    iterate_##name(p->l, context, func);                                       \
    (*func)(context, p->key);                                                  \
    iterate_##name(p->r, context, func);                                       \
  }                                                                            \
  struct {}
