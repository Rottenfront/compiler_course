#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "map.h"
#include "set.h"

SET_HEADER(set_ptr, void *);
SET_IMPL(set_ptr, void *);

void a(int a) {}
MAP_HEADER(map_ptr_int, void *, int);
MAP_IMPL(map_ptr_int, void *, int, a, 0);

MAP_HEADER(map_ptr_map_ptr_int, void *, map_ptr_int *);
MAP_IMPL(map_ptr_map_ptr_int, void *, map_ptr_int *, free_map_ptr_int, NULL);

void *create_resource(size_t size);
void increment_resource_bond(void *from, void *to);
void decrement_resource_bond(void *from, void *to);
void collect_garbage();

// internal functions

void iterate_children(void *ctx, void *child, int *count);
void deep_check_resources(void *ctx, void *current, map_ptr_int *children);

map_ptr_map_ptr_int *resources_graph = NULL;
set_ptr *current_resources = NULL;
void *returned_value = NULL;

void *create_resource(size_t size) {
  void *resource = malloc(size);
  insert_set_ptr(&current_resources, resource);
  return resource;
}

void increment_resource_bond(void *from, void *to) {
  map_ptr_int *from_resources = get_map_ptr_map_ptr_int(resources_graph, from);
  if (from_resources == NULL) {
    from_resources = create_map_ptr_int(to, 1);
  } else {
    int current_count = get_map_ptr_int(from_resources, to);
    set_map_ptr_int(&from_resources, to, current_count + 1);
  }
  set_map_ptr_map_ptr_int(&resources_graph, from, from_resources);
}

void decrement_resource_bond(void *from, void *to) {
  map_ptr_int *from_resources = get_map_ptr_map_ptr_int(resources_graph, from);
  assert(from_resources != NULL);
  int current_count = get_map_ptr_int(from_resources, to);
  assert(current_count > 0);
  if (current_count == 1) {
    erase_map_ptr_int(&from_resources, to);
  } else {
    set_map_ptr_int(&from_resources, to, current_count - 1);
  }
  if (from_resources == NULL) {
    erase_map_ptr_map_ptr_int(&resources_graph, from);
  } else {
    set_map_ptr_map_ptr_int(&resources_graph, from, from_resources);
  }
}

void iterate_children(void *ctx, void *child, int *count) {
  assert(*count > 0);
  assert(child != NULL);
  assert(ctx != NULL);
  set_ptr **used = ctx;
  if (exist_set_ptr(*used, child))
    return;
  map_ptr_int *child_resources =
      get_map_ptr_map_ptr_int(resources_graph, child);
  deep_check_resources(used, child, child_resources);
}

void deep_check_resources(void *ctx, void *current, map_ptr_int *children) {
  assert(ctx != NULL);
  set_ptr **used = ctx;
  insert_set_ptr(used, current);
  iterate_map_ptr_int(children, used, &iterate_children);
}

struct check_context {
  const set_ptr *used;
  set_ptr *to_destroy;
};

void check_current_resources(void *ctx, void *current) {
  struct check_context *context = ctx;
  if (!exist_set_ptr(context->used, current))
    insert_set_ptr(&context->to_destroy, current);
}

void destroy_unused_resource(void *ctx, void *resource) {
  assert(resource != NULL);
  erase_map_ptr_map_ptr_int(&resources_graph, resource);
  free(resource);
}

void collect_garbage() {
  set_ptr *used;
  deep_check_resources(&used, NULL,
                       get_map_ptr_map_ptr_int(resources_graph, NULL));
  struct check_context check_context = {used, .to_destroy = NULL};
  iterate_set_ptr(current_resources, &check_context, &check_current_resources);
  iterate_set_ptr(check_context.to_destroy, NULL, &destroy_unused_resource);
  free_set_ptr(check_context.to_destroy);
  free_set_ptr(current_resources);
  current_resources = used;
}
