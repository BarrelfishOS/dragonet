#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "misc.h"

#include "xht.h"

/*
 * Compares two given keys, and returns true if they match
 */
bool xht_compare_keys(flow_entry_t *k1, flow_entry_t *k2)
{
    if (memcmp(k1, k2, sizeof(flow_entry_t)) == 0) return true;
    return false;
}

void xht_print_key(flow_entry_t *key)
{
    printf("SRC_IP: %"PRIu32", DST_IP: %"PRIu32", "
            "SRC_PORT: %"PRIu16", DST_PORT: %"PRIu16".",
            key->src_ip, key->dst_ip,
            key->src_port, key->dst_port);
} // end function: xht_print_key

void xht_print_value(flow_details_t *value)
{
    printf("state: %"PRIu64", timestamp: %"PRIu64", counter: %"PRIu64".",
            value->state,
            value->timestamp,
            value->counter);
} // end function: xht_print_key



xht_t *xht_init(unsigned int size)
{
	xht_t *xht;

	xht = malloc(sizeof(xht_t));
	xht->table = calloc(size, sizeof(xht_entry_t *));
	if (!xht || !xht->table){
		perror("malloc");
		exit(1);
	}
	xht->size = size;

	return xht;
}

void xht_print(xht_t *xht)
{
	unsigned int i;
	xht_entry_t *curr;

	printf("xht:%p\n", xht);
	printf("==============================\n");
	for (i = 0; i < xht->size; i++) {
                if (xht->table[i] == NULL) continue;
		printf("Bucket %d: ", i);
		for (curr = xht->table[i]; curr != NULL; curr = curr->next) {
			//printf("[%lu] => %lu ", curr->key, curr->val);
                        xht_print_key(&curr->key);
                        printf(" => ");
                        xht_print_value(&curr->val);
                        printf("\n");
                }
		printf("\n");
	}
	printf("==============================\n");
}

void
xht_insert(xht_t *xht, flow_entry_t key, flow_details_t val)
{
	xht_entry_t *new_entry, *curr;
	unsigned int bucket;

	bucket = xht_hash_fn(xht, key);
	//Lookup
	curr = xht->table[bucket];
	while (curr) {
		/* found key */
		//if (curr->key == key){
		if (xht_compare_keys(&curr->key, &key)){
			curr->val = val;
			return;
		}
		curr = curr->next;
	}

	/* key does not exist, allocate a new entry  */
	new_entry = xmalloc(sizeof(xht_entry_t));
	new_entry->key = key;
	new_entry->val = val;

	/* put new entry, at the beggining of the bucket */
	new_entry->next = xht->table[bucket];
	xht->table[bucket] = new_entry;
}

bool
xht_lookup(xht_t *xht, flow_entry_t key, flow_details_t *val)
{
	unsigned int bucket;
	xht_entry_t *curr;

	bucket = xht_hash_fn(xht,key);
	curr = xht->table[bucket];
	for (;;){
		/* entry not found */
		if (curr == NULL)
			return false;

		/* entry found */
		//if (curr->key == key) {
		if (xht_compare_keys(&curr->key, &key)){
			break;
                }

		curr = curr->next;
	}

	*val = curr->val;
	return true;

}

bool
xht_remove(xht_t *xht, flow_entry_t key, flow_details_t *val)
{
	unsigned bucket;
	xht_entry_t *curr, *prev;

	bucket = xht_hash_fn(xht,key);
	curr = xht->table[bucket];
	prev = curr;
	for (;;){
		/* entry does not exist */
		if (curr == NULL)
			return false;

		/* found entry */
		//if (curr->key == key) {
		if (xht_compare_keys(&curr->key, &key)){
			break;
                }

		prev = curr;
		curr = curr->next;
	}

	*val = curr->val;
	/* delete entry */
	if (curr == xht->table[bucket])
		xht->table[bucket] = curr->next;
	else
		prev->next = curr->next;
	free(curr);

	return true;
}
