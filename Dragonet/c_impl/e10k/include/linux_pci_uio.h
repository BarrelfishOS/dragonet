/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef LINUX_PCI_UIO_H_
#define LINUX_PCI_UIO_H_

#include <stdint.h>
#include <stdbool.h>

#define UIO_MAX_MAPPINGS 4
#define PCI_MAX_UIOS 1

struct usp_uio_mapping {
    char     name[16];
    uint64_t addr;
    uint64_t offset;
    uint64_t size;
    void    *mapped;
};

struct usp_uio_desc {
    /** Device name of UIO device (format: uioX) */
    char                   device[16];
    /** UIO name */
    char                   name[64];
    /** Number of mappings */
    size_t                 num_mappings;
    /** Specification of mappings */
    struct usp_uio_mapping mappings[UIO_MAX_MAPPINGS];
};

struct usp_pci_desc {
    /** PCI vendor ID */
    uint16_t            vendor;
    /** PCI device ID */
    uint16_t            device;

    /** Driver name used for device (e.g igb_uio) */
    char                driver_name[64];

    /** Number of UIO devices associated with this device */
    size_t              num_uio;
    /** UIO device specification */
    struct usp_uio_desc uio[PCI_MAX_UIOS];
};

bool usp_pci_parse(struct usp_pci_desc *dev, const char *pcidev);
void usp_pci_desc_dump(struct usp_pci_desc *desc);
bool usp_uio_map(struct usp_uio_desc *uio);
bool usp_uio_unmap(struct usp_uio_desc *uio);

#endif // ndef LINUX_PCI_UIO_H_

