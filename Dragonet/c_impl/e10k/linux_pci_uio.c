/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <libgen.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/types.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <linux_pci_uio.h>

#define SYS_PCI_DEVPATH "/sys/bus/pci/devices/%s"


bool file_to_string(const char *path, char *dest, size_t len)
{
    FILE *f;
    bool success = true;

    if ((f = fopen(path, "r")) == NULL) {
        return false;
    }

    if (fgets(dest, len, f) == NULL) {
        success = false;
    }

    fclose(f);
    return success;
}

/** Remove whitespace from end of string */
void string_trim(char *str)
{
    size_t len = strlen(str);
    while (len > 0) {
        len--;
        if (isspace(str[len])) {
            str[len] = 0;
        } else {
            return;
        }
    }
}

static bool usp_uio_mapping_parse(struct usp_uio_mapping *map,
        const char *mpath)
{
    char path[PATH_MAX];
    char buf[32];

    // Parse name
    snprintf(path, sizeof(path), "%s/name", mpath);
    if (!file_to_string(path, map->name, sizeof(map->name))) {
        fprintf(stderr, "usp_uio_mapping_parse[%s]: name failed\n", mpath);
        return false;
    }
    string_trim(map->name);

    // Parse addr
    snprintf(path, sizeof(path), "%s/addr", mpath);
    if (!file_to_string(path, buf, sizeof(buf))) {
        fprintf(stderr, "usp_uio_mapping_parse[%s]: addr failed\n", mpath);
        return false;
    }
    map->addr = strtoull(buf, NULL, 0);

    // Parse offset
    snprintf(path, sizeof(path), "%s/offset", mpath);
    if (!file_to_string(path, buf, sizeof(buf))) {
        fprintf(stderr, "usp_uio_mapping_parse[%s]: offset failed\n", mpath);
        return false;
    }
    map->offset = strtoull(buf, NULL, 0);

    // Parse size
    snprintf(path, sizeof(path), "%s/size", mpath);
    if (!file_to_string(path, buf, sizeof(buf))) {
        fprintf(stderr, "usp_uio_mapping_parse[%s]: size failed\n", mpath);
        return false;
    }
    map->size = strtoull(buf, NULL, 0);
    map->mapped = NULL;
    return true;
}

static bool usp_uio_parse(struct usp_uio_desc *desc, const char *upath,
        const char *name)
{
    char path[PATH_MAX];
    bool success = true;
    DIR *dir;
    struct dirent *ent;
    size_t count;

    // Set device name
    strncpy(desc->device, name, sizeof(desc->device));
    desc->device[sizeof(desc->device) - 1] = 0;

    // Parse name
    snprintf(path, sizeof(path), "%s/name", upath);
    if (!file_to_string(path, desc->name, sizeof(desc->name))) {
        fprintf(stderr, "usp_uio_parse[%s]: name failed\n", upath);
        return false;
    }
    string_trim(desc->name);

    // Parse mappings
    snprintf(path, sizeof(path), "%s/maps", upath);
    if ((dir = opendir(path)) == NULL) {
        fprintf(stderr, "usp_uio_parse[%s]: diropen maps failed\n", upath);
        return false;
    }

    count = 0;
    while (success && (ent = readdir(dir)) != NULL && count < UIO_MAX_MAPPINGS){
        // we're only interested if the name starts with '
        if (!strncmp(ent->d_name, "map", 3)) {
            snprintf(path, sizeof(path), "%s/maps/%s", upath, ent->d_name);
            success = usp_uio_mapping_parse(desc->mappings + count, path);
            count++;
        }
    }
    desc->num_mappings = count;
    closedir(dir);

    if (!success) {
        fprintf(stderr, "usp_uio_parse[%s]: diropen maps failed\n", upath);
    }
    return success;
}

bool usp_pci_parse(struct usp_pci_desc *dev, const char *pcidev)
{
    char path[PATH_MAX];
    char buf[PATH_MAX];
    ssize_t len;
    bool success = true;
    DIR *dir;
    struct dirent *ent;
    size_t count;

    // Parse vendor id
    snprintf(path, sizeof(path), SYS_PCI_DEVPATH "/vendor", pcidev);
    if (!file_to_string(path, buf, sizeof(buf))) {
        fprintf(stderr, "usp_pci_parse[%s]: vendor failed\n", pcidev);
        return false;
    }
    dev->vendor = strtoul(buf, NULL, 0);

    // Parse device id
    snprintf(path, sizeof(path), SYS_PCI_DEVPATH "/device", pcidev);
    if (!file_to_string(path, buf, sizeof(buf))) {
        fprintf(stderr, "usp_pci_parse[%s]: device id failed\n", pcidev);
        return false;
    }
    dev->device = strtoul(buf, NULL, 0);

    // Get driver name
    snprintf(path, sizeof(path), SYS_PCI_DEVPATH "/driver", pcidev);
    if ((len = readlink(path, buf, sizeof(buf) - 1)) < 0) {
        fprintf(stderr, "usp_pci_parse[%s]: driver name failed\n", pcidev);
        return false;
    }
    buf[len] = 0;
    strncpy(dev->driver_name, basename(buf), sizeof(dev->driver_name));
    dev->driver_name[sizeof(dev->driver_name) - 1] = 0;

    // Parse UIO description
    snprintf(path, sizeof(path), SYS_PCI_DEVPATH "/uio", pcidev);
    if ((dir = opendir(path)) == NULL) {
        fprintf(stderr, "usp_pci_parse[%s]: uio diropen failed\n", pcidev);
        return false;
    }

    count = 0;
    while (success && (ent = readdir(dir)) != NULL && count < PCI_MAX_UIOS) {
        // we're only interested if the name starts with '
        if (!strncmp(ent->d_name, "uio", 3)) {
            snprintf(path, sizeof(path), SYS_PCI_DEVPATH "/uio/%s", pcidev,
                    ent->d_name);
            success = usp_uio_parse(dev->uio + count, path, ent->d_name);
            count++;
        }
    }
    dev->num_uio = count;
    closedir(dir);

    if (!success) {
        fprintf(stderr, "usp_pci_parse[%s]: parsing uio failed\n", pcidev);
    }
    return success;
}

void usp_pci_desc_dump(struct usp_pci_desc *desc)
{
    int i, j;
    struct usp_uio_desc *uio;
    struct usp_uio_mapping *map;
    printf(
        "usp_pci_desc {\n"
        "    vendor = %"PRIx16",\n"
        "    device = %"PRIx16",\n"
        "    driver_name = \"%s\",\n",
        desc->vendor, desc->device, desc->driver_name);
    for (i = 0; i < (int) desc->num_uio; i++) {
        uio = desc->uio + i;
        printf(
            "    uio[%d] = {\n"
            "        device = \"%s\",\n"
            "        name = \"%s\",\n",
            i, uio->device, uio->name);
        for (j = 0; j < (int) uio->num_mappings; j++) {
            map = uio->mappings + j;
            printf("        mappings[%d] = { name = \"%s\", addr = %"PRIx64", "
                "offset = %"PRIx64", size = %"PRIx64" }\n", j, map->name,
                map->addr, map->offset, map->size);
        }
        printf("    }\n");
    }
    printf("}\n");

}

static bool uio_unmap_mapping(struct usp_uio_mapping *map)
{
    void *m;
    m = (void *) ((uintptr_t) map->mapped - map->offset);
    if (munmap(m, map->size) == 0) {
        map->mapped = NULL;
        return true;
    } else {
        return false;
    }
}

bool usp_uio_map(struct usp_uio_desc *uio)
{
    char path[PATH_MAX];
    int fd;
    bool success = true;
    struct usp_uio_mapping *map;
    size_t i;
    off_t off;
    int prot;
    void *m;

    // Open /dev/uioX
    snprintf(path, sizeof(path), "/dev/%s", uio->device);
    if ((fd = open(path, O_RDWR)) < 0) {
        fprintf(stderr, "usp_uio_map: open failed\n");
        return false;
    }

    // Map all UIO mappings
    for (i = 0; i < uio->num_mappings; i++) {
        map = uio->mappings + i;
        off = getpagesize() * i;
        prot = PROT_READ | PROT_WRITE;
        if ((m = mmap(NULL, map->size, prot, MAP_SHARED, fd, off)) ==
                MAP_FAILED)
        {
            fprintf(stderr, "usp_uio_map: mapping %d failed\n", (int) i);
            success = false;
            break;
        }
        map->mapped = (void *) ((uintptr_t) m + map->offset);
    }

    // If we ran into a problem, unmap the successful mappings
    for (i = 0; !success && i < uio->num_mappings; i++) {
        map = uio->mappings + i;
        if (map->mapped != NULL && !uio_unmap_mapping(map)) {
            fprintf(stderr, "usp_uio_map: unmapping successful mapping failed"
                    " :-/\n");
        }
    }

    close(fd);
    return success;
}

bool usp_uio_unmap(struct usp_uio_desc *uio)
{
    struct usp_uio_mapping *map;
    size_t i;

    for (i = 0; i < uio->num_mappings; i++) {
        map = uio->mappings + i;
        if (!uio_unmap_mapping(map)) {
            fprintf(stderr, "usp_uio_map: unmapping successful mapping failed"
                    " :-/\n");
            return false;
        }
    }

    return true;
}


