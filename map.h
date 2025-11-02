
#define MAP_HEADER(name, type, value_type)                                     \
  typedef struct name name;                                                    \
  struct name {                                                                \
    type key;                                                                  \
    value_type value;                                                          \
    name *l, *r;                                                               \
  };                                                                           \
  name *create_##name(type key, value_type value);                             \
  void free_##name(name *p);                                                   \
  void set_##name(name **root, type key, value_type value);                    \
  value_type get_##name(name *root, type key);                                 \
  void erase_##name(name **root, type key);                                    \
  void iterate_##name(name *root, void *context,                               \
                      void (*func)(void *, type, value_type *))

#define MAP_IMPL(name, type, value_type, free_value, default_value)            \
  name *create_##name(type p, value_type value) {                              \
    name *res = malloc(sizeof(name));                                          \
    res->key = p;                                                              \
    res->value = value;                                                        \
    res->l = NULL;                                                             \
    res->r = NULL;                                                             \
    return res;                                                                \
  }                                                                            \
  void free_##name(name *p) {                                                  \
    if (p) {                                                                   \
      free_##name(p->l);                                                       \
      free_##name(p->r);                                                       \
      free_value(p->value);                                                    \
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
  typedef struct {                                                             \
    name *first, *second;                                                      \
  } name##_pair;                                                               \
                                                                               \
  value_type get_##name(name *p, type key) {                                   \
    if (!p)                                                                    \
      return default_value;                                                    \
    if (p->key == key)                                                         \
      return p->value;                                                         \
    if (p->key < key)                                                          \
      return get_##name(p->r, key);                                            \
    return get_##name(p->l, key);                                              \
  }                                                                            \
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
  void set_##name(name **root, type key, value_type value) {                   \
    if (!root) {                                                               \
      *root = create_##name(key, value);                                       \
      return;                                                                  \
    }                                                                          \
    name##_pair q1 = split_##name(*root, key);                                 \
    name##_pair q2 = split_##name(q1.first, key - 1);                          \
    free_##name(q2.second);                                                    \
    name *t = create_##name(key, value);                                       \
    *root = merge_##name(q2.first, merge_##name(t, q1.second));                \
  }                                                                            \
                                                                               \
  void erase_##name(name **root, type key) {                                   \
    if (!root)                                                                 \
      return;                                                                  \
                                                                               \
    if ((*root)->l == NULL && (*root)->r == NULL) {                            \
      if ((*root)->key == key) {                                               \
        free_##name(*root);                                                    \
        *root = NULL;                                                          \
      }                                                                        \
      return;                                                                  \
    }                                                                          \
    name##_pair q1 = split_##name(*root, key);                                 \
    name##_pair q2 = split_##name(q1.first, key - 1);                          \
    free_##name(q2.second);                                                    \
    *root = merge_##name(q2.first, q1.second);                                 \
  }                                                                            \
  void iterate_##name(name *p, void *context,                                  \
                      void (*func)(void *, type, value_type *)) {              \
    if (p == NULL)                                                             \
      return;                                                                  \
    iterate_##name(p->l, context, func);                                       \
    (*func)(context, p->key, &p->value);                                       \
    iterate_##name(p->r, context, func);                                       \
  }                                                                            \
  struct {}
