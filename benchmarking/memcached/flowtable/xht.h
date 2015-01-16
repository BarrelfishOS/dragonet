#ifndef __XHT_H_
#define __XHT_H_

#include <inttypes.h>
#include <stdbool.h>



/*
 * flow defination
 */

struct flow_entry_st {
    uint32_t        src_ip;
    uint32_t        dst_ip;
    uint16_t        src_port;
    uint16_t        dst_port;
} __attribute__((packed));

typedef struct flow_entry_st flow_entry_t;

/*
 * The details about the flow entry
 */
struct flow_details_st {
    uint64_t        timestamp;
    uint64_t        counter;
    uint64_t        state;
};

typedef struct flow_details_st flow_details_t;

/*
 * Simple hash table implementation
 */

/* hash entry */
typedef struct xht_entry_st {
	/* key, value for hash entry */
	flow_entry_t key;
	flow_details_t val;
	/* pointer to next entry, if NULL this is the last one */
	struct xht_entry_st *next;
} xht_entry_t;

typedef struct xht_st {
	 /* hash table: each entry is is a pointer to
	  * the head of a linked list of hash entries */
	xht_entry_t **table;
	unsigned int size;	/* number of slots */
} xht_t;

/**
 * xht_init: initialize a hash table
 *  size: size of the table
 */
xht_t *xht_init(unsigned int size);


/**
 * xht_lookup: find the value of the given key
 */
bool xht_lookup(xht_t *xht, flow_entry_t key, flow_details_t *val);

/**
 * xht_insert: insert a new value for the given key
 * If an entry for the key exists, just replace it
 */
void xht_insert(xht_t *xht, flow_entry_t key, flow_details_t val);

/**
 * xht_remove: remove the entry of the given value
 */
bool xht_remove(xht_t *xht,  flow_entry_t key, flow_details_t *val);

static inline uint64_t
xht_hash_fn(xht_t *xht, flow_entry_t key)
{
        // Creating a checksum from the sum of all the values
        uint64_t checksum = key.src_ip + key.dst_ip +
                            key.src_port + key.dst_port;
	return (checksum % xht->size);
}


/*
 * Compares two given keys, and returns true if they match
 */
bool xht_compare_keys(flow_entry_t *k1, flow_entry_t *k2);

/*
 * Printing the entry and flowtable for debugging
 */
void xht_print_key(flow_entry_t *key);
void xht_print_value(flow_details_t *value);
void xht_print(xht_t *xht);

#endif
