/*
 * Copyright (c) 2007-2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <arpa/inet.h> // for htons/htonl

#include <e10k.h>
#include <sleep.h>

#define DEBUG(x...) printf("e10k: " x)
//#define DEBUG(x...) do {} while (0)

/** Enable RX operation for whole card. */
static void rx_enable(struct e10k_card *c)
{
    e10k_secrxctrl_rx_dis_wrf(&c->d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(&c->d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(&c->d, 1);
    e10k_secrxctrl_rx_dis_wrf(&c->d, 0);
}

/** Disable RX operation for whole card. */
static void rx_disable(struct e10k_card *c)
{
    e10k_secrxctrl_rx_dis_wrf(&c->d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(&c->d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(&c->d, 0);
    e10k_secrxctrl_rx_dis_wrf(&c->d, 0);
}

/** Enable TX operation for whole card. */
static void tx_enable(struct e10k_card *c)
{
    e10k_dmatxctl_txen_wrf(&c->d, 1);
}

/** Disable TX operation for whole card. */
static void tx_disable(struct e10k_card *c)
{
    e10k_dmatxctl_txen_wrf(&c->d, 0);
    while (e10k_dmatxctl_txen_rdf(&c->d) != 0); // TODO: timeout
}

/** Stop whole device. */
static void stop_device(struct e10k_card *c)
{
    int i = 0;

    DEBUG("Stopping device\n");

    // Disable RX and TX
    rx_disable(c);
    tx_disable(c);
    c->rxtx_enabled = false;

    // Disable interrupts
    e10k_eimc_cause_wrf(&c->d, 0x7FFFFFFF);
    e10k_eicr_rd(&c->d);

    // Disable each RX and TX queue
    for (i = 0; i < E10K_NQUEUES; i++) {
        e10k_txdctl_wr(&c->d, i, e10k_txdctl_swflsh_insert(0x0, 1));

        if (i < 64) {
            e10k_rxdctl_1_wr(&c->d, i, 0x0);
        } else {
            e10k_rxdctl_2_wr(&c->d, i - 64, 0x0);
        }

    }

    // From BSD driver (not in spec)
    milli_sleep(2);

    // Master disable procedure
    e10k_ctrl_pcie_md_wrf(&c->d, 1);
    while (e10k_status_pcie_mes_rdf(&c->d) != 0); // TODO: Timeout
    DEBUG("Stopping device done\n");
}

/** Initialize hardware queue n. */
static void queue_hw_init(struct e10k_card *c, uint8_t n)
{
    e10k_t *d = &c->d;
    bool enable_global = !c->rxtx_enabled;
    assert(n < E10K_NQUEUES);
    struct e10k_queue_state *qst = c->queues + n;

    DEBUG("tx.phys=%"PRIx64" tx.size=%"PRIu64"\n",
            qst->tx_ring_phys, qst->tx_ring_size);
    DEBUG("rx.phys=%"PRIx64" rx.size=%"PRIu64"\n",
            qst->rx_ring_phys, qst->rx_ring_size);
    DEBUG("txhwb.phys=%"PRIx64"\n", qst->tx_hwb_phys);


    // Initialize RX queue in HW
    e10k_rdbal_1_wr(d, n, qst->rx_ring_phys);
    e10k_rdbah_1_wr(d, n, qst->rx_ring_phys >> 32);
    e10k_rdlen_1_wr(d, n, qst->rx_ring_size);

    e10k_srrctl_1_bsz_pkt_wrf(d, n, qst->rxbufsz / 1024);
    e10k_srrctl_1_bsz_hdr_wrf(d, n, 128 / 64); // TODO: Do 128 bytes suffice in
                                               //       all cases?
    e10k_srrctl_1_desctype_wrf(d, n, e10k_adv_1buf);
    e10k_srrctl_1_drop_en_wrf(d, n, 1);

    // Set RSC status
    if (qst->use_rsc) {
        e10k_rscctl_1_maxdesc_wrf(d, n, 3);
        e10k_rscctl_1_rsc_en_wrf(d, n, 1);
        // TODO: (how) does this work for queues >=64?
        assert(n < 64);
        e10k_psrtype_split_tcp_wrf(d, n, 1);
    } else {
        e10k_rscctl_1_maxdesc_wrf(d, n, 0);
        e10k_rscctl_1_rsc_en_wrf(d, n, 0);
    }

    // Initialize queue pointers (empty)
    e10k_rdt_1_wr(d, n, qst->rx_head);
    e10k_rdh_1_wr(d, n, qst->rx_head);

    e10k_rxdctl_1_enable_wrf(d, n, 1);
    while (e10k_rxdctl_1_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] RX queue enabled\n", n);

    // Setup Interrupts for this queue
    assert(!qst->use_irq && "NO IRQs supported atm");
    /*if (queues[n].use_irq) {
        uint8_t rxv, txv;
        // Look for interrupt vector
        if (queues[n].msix_intvec != 0) {
            if (queues[n].msix_index == -1) {
                setup_interrupt(&queues[n].msix_index, queues[n].msix_intdest,
                                queues[n].msix_intvec);
            }
            rxv = txv = queues[n].msix_index;
        } else {
            rxv = QUEUE_INTRX;
            txv = QUEUE_INTTX;
        }
        DEBUG("rxv=%d txv=%d\n", rxv, txv);

        // Setup mapping queue Rx/Tx -> interrupt
        uint8_t i = n / 2;
        if ((n % 2) == 0) {
            e10k_ivar_i_alloc0_wrf(d, i, rxv);
            e10k_ivar_i_allocval0_wrf(d, i, 1);
            e10k_ivar_i_alloc1_wrf(d, i, txv);
            e10k_ivar_i_allocval1_wrf(d, i, 1);
        } else {
            e10k_ivar_i_alloc2_wrf(d, i, rxv);
            e10k_ivar_i_allocval2_wrf(d, i, 1);
            e10k_ivar_i_alloc3_wrf(d, i, txv);
            e10k_ivar_i_allocval3_wrf(d, i, 1);
        }
        if (queues[n].msix_intvec != 0) {
            e10k_eitr_l_wr(d, rxv, 0);

            // Enable autoclear (higher ones are always auto cleared)
            if (rxv < 16) {
                e10k_eiac_rtxq_wrf(d, e10k_eiac_rtxq_rdf(d) | (1 << rxv));
            }

            // Enable interrupt
            e10k_eimsn_wr(d, rxv / 32, (1 << (rxv % 32)));
        }
        if (rxv < 16) {
            // Make sure interrupt is cleared
            e10k_eicr_wr(d, 1 << rxv);
        }
    }*/


    // Enable RX
    if (enable_global) {
        DEBUG("[%x] Enabling RX globally...\n", n);
        rx_enable(c);
        DEBUG("[%x] RX globally enabled\n", n);
    }


    // Initialize TX queue in HW
    e10k_tdbal_wr(d, n, qst->tx_ring_phys);
    e10k_tdbah_wr(d, n, qst->tx_ring_phys >> 32);
    e10k_tdlen_wr(d, n, qst->tx_ring_size);

    // Initialize TX head index write back
    if (qst->tx_hwb_phys != -1ULL) {
        e10k_tdwbal_headwb_low_wrf(d, n, qst->tx_hwb_phys >> 2);
        e10k_tdwbah_headwb_high_wrf(d, n, qst->tx_hwb_phys >> 32);
        e10k_tdwbal_headwb_en_wrf(d, n, 1);
    }

    // Initialized by queue driver to avoid race conditions
    // Initialize queue pointers
    e10k_tdh_wr(d, n, qst->tx_head);
    e10k_tdt_wr(d, n, qst->tx_head);

    // Configure prefetch and writeback threshhold
    e10k_txdctl_pthresh_wrf(d, n, 8); // FIXME: Figure out what the right number
                                      //        is here.
    e10k_txdctl_hthresh_wrf(d, n, 0);
    e10k_txdctl_wthresh_wrf(d, n, 0);

    if (enable_global) {
        DEBUG("[%x] Enabling TX globally...\n", n);
        tx_enable(c);
        c->rxtx_enabled = true;
        DEBUG("[%x] TX globally enabled\n", n);
    }

    e10k_txdctl_enable_wrf(d, n, 1);
    while (e10k_txdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] TX queue enabled\n", n);


    // Some initialization stuff from BSD driver
    e10k_dca_txctrl_txdesc_wbro_wrf(d, n, 0);

    e10k_qcb_write_queue_tails(c, qst);

}

/** Stop queue. */
static void queue_hw_stop(struct e10k_card *c, uint8_t n)
{
    // This process is described in 4.6.7.1.2

    // Disable TX for this queue
    e10k_txdctl_enable_wrf(&c->d, n, 0);

    // TODO: Flush packet buffers
    // TODO: Remove all filters
    // TODO: With RSC we have to wait here (see spec), not used atm

    // Disable RX for this queue
    e10k_rxdctl_1_enable_wrf(&c->d, n, 0);
    while (e10k_rxdctl_1_enable_rdf(&c->d, n) != 0); // TODO: Timeout

    // A bit too much, but make sure memory is not used anymore
    milli_sleep(1);
}

/**
 * Initialize hardware registers.
 * Is also called after a reset of the device.
 */
static void device_init(struct e10k_card *c)
{
    int i;
    e10k_ctrl_t ctrl;
    e10k_pfqde_t pfqde;
    bool initialized_before = c->initialized;
    e10k_t *d = &c->d;

    c->initialized = 0;

    stop_device(c);

    c->running = true;
    if (initialized_before) {
        // Save queue heads and tails
        for (i = 0; i < E10K_NQUEUES; i++) {
            if (c->queues[i].enabled) {
                c->queues[i].tx_head = e10k_tdh_rd(d, i);
                if (i < 64) {
                    c->queues[i].rx_head = e10k_rdh_1_rd(d, i);
                } else {
                    c->queues[i].rx_head = e10k_rdh_2_rd(d, i - 64);
                }
            }
        }
    }

    // Make a double reset to be sure
    for (i = 0; i < 2; i++) {
        // Issue Global reset
        ctrl = e10k_ctrl_rd(d);
        ctrl = e10k_ctrl_lrst_insert(ctrl, 1);
        ctrl = e10k_ctrl_rst_insert(ctrl, 1);
        e10k_ctrl_wr(d, ctrl);
        while ((e10k_ctrl_rst_rdf(d) != 0) ||
               (e10k_ctrl_lrst_rdf(d) != 0)); // TODO: Timeout

        // Spec says 10, fbsd driver 50
        milli_sleep(50);
    }
    DEBUG("Global reset done\n");

    // Disable interrupts
    e10k_eimc_cause_wrf(d, 0x7FFFFFFF);
    e10k_eicr_rd(d);

    // Let firmware know that we have taken over
    e10k_ctrl_ext_drv_load_wrf(d, 1);

    // NO Snoop disable (from FBSD)
    // Without this, the driver only works on sbrinz1 if the receive buffers are
    // mapped non cacheable. If the buffers are mapped cacheable, sometimes we
    // seem to read old buffer contents, not sure exactly why, as far as
    // understood this, No snoop should only be enabled by the device if it is
    // save...
    // TODO: Also check performance implications of this on gottardo and other
    // machnies where it works without this.
    e10k_ctrl_ext_ns_dis_wrf(d, 1);

    // Initialize flow-control registers
    for (i = 0; i < 8; i++) {
        if (i < 4) e10k_fcttv_wr(d, i, 0x0);
        e10k_fcrtl_wr(d, i, 0x0);
        e10k_fcrth_wr(d, i, 0x0);
    }
    e10k_fcrtv_wr(d, 0x0);
    e10k_fccfg_wr(d, 0x0);

    // Initialize Phy
    e10k_phy_init(d);

    // Wait for EEPROM auto read
    while (e10k_eec_auto_rd_rdf(d) == 0); // TODO: Timeout
    DEBUG("EEPROM auto read done\n");


    // Wait for DMA initialization
    while (e10k_rdrxctl_dma_initok_rdf(d) == 0); // TODO: Timeout

    c->macaddr =
        e10k_ral_ral_rdf(d, 0) | ((uint64_t) e10k_rah_rah_rdf(d, 0) << 32);
    DEBUG("mac valid = %x\n", e10k_rah_av_rdf(d, 0));

    // Wait for link to come up
    while (e10k_links_lnk_up_rdf(d) == 0); // TODO: Timeout
    DEBUG("Link Up\n");
    milli_sleep(50);

    // Initialize interrupts
    // TODO: no interrupts for now
    e10k_eicr_wr(d, 0xffffffff);
    /*if (msix) {
        // Switch to MSI-X mode
        e10k_gpie_msix_wrf(d, 1);
        e10k_gpie_pba_sup_wrf(d, 1);
        e10k_gpie_ocd_wrf(d, 1);

        // Allocate msix vector for cdriver and set up handler
        if (cdriver_msix == -1) {
            err = pci_setup_inthandler(interrupt_handler_msix, NULL, &cdriver_vector);
            assert(err_is_ok(err));

            setup_interrupt(&cdriver_msix, disp_get_core_id(), cdriver_vector);
        }

        // Map management interrupts to our vector
        e10k_ivar_misc_i_alloc0_wrf(d, cdriver_msix);
        e10k_ivar_misc_i_alloc1_wrf(d, cdriver_msix);
        e10k_ivar_misc_i_allocval0_wrf(d, 1);
        e10k_ivar_misc_i_allocval1_wrf(d, 1);

        // Enable auto masking of interrupt
        e10k_gpie_eiame_wrf(d, 1);
        e10k_eiamn_wr(d, cdriver_msix / 32, (1 << (cdriver_msix % 32)));

        // Set no interrupt delay
        e10k_eitr_l_wr(d, cdriver_msix, 0);
        e10k_gpie_eimen_wrf(d, 1);

        // Enable interrupt
        e10k_eimsn_wr(d, cdriver_msix / 32, (1 << (cdriver_msix % 32)));
    } else {
        // Set no Interrupt delay
        e10k_eitr_l_wr(d, 0, 0);
        e10k_gpie_eimen_wrf(d, 1);

        // Enable all interrupts
        e10k_eimc_wr(d, e10k_eims_rd(d));
        e10k_eims_cause_wrf(d, 0x7fffffff);
    }*/

    // Just a guess for RSC delay
    e10k_gpie_rsc_delay_wrf(d, 2);


    // Initialize multiple register tables
    for (i = 1; i < 128; i++) {
        e10k_ral_wr(d, i, 0);
        e10k_rah_wr(d, i, 0);
    }
    for (i = 0; i < 128; i++)
        e10k_mta_bit_vec_wrf(d, i, 0);
    for (i = 0; i < 128; i++)
        e10k_vfta_vlan_flt_wrf(d, i, 0);
    for (i = 0; i < 64; i++) {
        e10k_pfvlvf_vi_en_wrf(d, i, 0);
        e10k_psrtype_wr(d, i, 0);
    }
    for (i = 0; i < 128; i++)
        e10k_pfuta_wr(d, i, 0);
    for (i = 0; i < 256; i++)
        e10k_mpsar_pool_ena_wrf(d, i, 0);
    for (i = 0; i < 128; i++) {
        e10k_fhft_1_wr(d, i, 0);
        if (i < 64) {
            e10k_fhft_2_wr(d, i, 0);
        }
    }

    // Allow for per-queue RSC
    e10k_rfctl_rsc_dis_wrf(d, 0);

    // Initialize RX filters
    for (i = 0; i < E10K_N5TFILTERS; i++) {
        e10k_ftqf_wr(d, i, 0);
        e10k_saqf_wr(d, i, 0);
        e10k_daqf_wr(d, i, 0);
        e10k_sdpqf_wr(d, i, 0);
    }
    for (i = 0; i < 32; i++)
        e10k_reta_wr(d, i, 0);
    e10k_mcstctrl_mfe_wrf(d, 0);

    // Accept broadcasts
    e10k_fctrl_bam_wrf(d, 1);

    // Make sure Rx CRC strip is consistently enabled in HLREG0 and RDRXCTL
    e10k_hlreg0_rxcrcstrp_wrf(d, 1);
    // Note: rscfrstsz has to be set to 0 (is mbz)
    e10k_rdrxctl_t rdrxctl = e10k_rdrxctl_rd(d);
    rdrxctl = e10k_rdrxctl_crcstrip_insert(rdrxctl, 1);
    e10k_rdrxctl_wr(d, rdrxctl);


    // Configure buffers etc. according to specification
    // Section 4.6.11.3.4 (No DCP, no virtualization, no RSS)
    // 1:1 from spec, though not sure if everything is necessary, but since
    // initialization is still buggy, I'd rather be conservative and set some
    // additional flags, even if they aren't strictly necessary.
    e10k_rxpbsize_size_wrf(d, 0, 0x200);
    e10k_txpbsize_size_wrf(d, 0, 0xA0);
    e10k_txpbthresh_thresh_wrf(d, 0, 0xA0);
    for (i = 1; i < 8; i++) {
        e10k_rxpbsize_size_wrf(d, i, 0x0);
        e10k_txpbsize_size_wrf(d, i, 0x0);
        e10k_txpbthresh_thresh_wrf(d, i, 0x0);
    }

    e10k_mrqc_mrque_wrf(d, e10k_no_rss);
    e10k_mtqc_rt_en_wrf(d, 0);
    e10k_mtqc_vt_en_wrf(d, 0);
    e10k_mtqc_num_tc_wrf(d, 0);
    e10k_pfvtctl_vt_en_wrf(d, 0);
    e10k_rtrup2tc_wr(d, 0);
    e10k_rttup2tc_wr(d, 0);

    e10k_dtxmxszrq_max_bytes_wrf(d, 0xFFF);

    for (i = 0; i < 128; i++) {
        pfqde = e10k_pfqde_queue_idx_insert(0x0, i);
        pfqde = e10k_pfqde_we_insert(pfqde, 1);
        e10k_pfqde_wr(d, pfqde);
    }

    e10k_mflcn_rpfce_wrf(d, 0);
    e10k_mflcn_rfce_wrf(d, 0);
    e10k_fccfg_tfce_wrf(d, e10k_lfc_en);

    /* Causes ECC error (could be same problem as with l34timir (see e10k.dev)
    for (i = 0; i < 128; i++) {
        e10k_rttdqsel_txdq_idx_wrf(d, i);
        e10k_rttdt1c_wr(d, 0);
    }*/
    for (i = 0; i < 8; i++) {
        e10k_rttdt2c_wr(d, i, 0);
        e10k_rttpt2c_wr(d, i, 0);
        e10k_rtrpt4c_wr(d, i, 0);
    }

    e10k_rttdcs_tdpac_wrf(d, 0);
    e10k_rttdcs_vmpac_wrf(d, 0);
    e10k_rttdcs_tdrm_wrf(d, 0);
    e10k_rttdcs_bdpm_wrf(d, 1);
    e10k_rttdcs_bpbfsm_wrf(d, 1);
    e10k_rttpcs_tppac_wrf(d, 0);
    e10k_rttpcs_tprm_wrf(d, 0);
    e10k_rttpcs_arbd_wrf(d, 0x224);
    e10k_rtrpcs_rac_wrf(d, 0);
    e10k_rtrpcs_rrm_wrf(d, 0);



    // disable relaxed ordering
    for (i = 0; i < E10K_NQUEUES; i++) {
        e10k_dca_txctrl_txdesc_wbro_wrf(d, i, 0);
        if (i < 64) {
            e10k_dca_rxctrl_1_rxhdr_ro_wrf(d, i, 0);
            e10k_dca_rxctrl_1_rxdata_wrro_wrf(d, i, 0);
        } else {
            e10k_dca_rxctrl_2_rxhdr_ro_wrf(d, i - 64, 0);
            e10k_dca_rxctrl_2_rxdata_wrro_wrf(d, i - 64, 0);
        }
    }

    // disable all queues
    for (i = 0; i < E10K_NQUEUES; i++) {
        e10k_txdctl_enable_wrf(d, i, 0);
        if (i < 64) {
            e10k_rxdctl_1_enable_wrf(d, i, 0);
        } else {
            e10k_rxdctl_2_enable_wrf(d, i - 64, 0);
        }
    }

    DEBUG("Card initialized (%d)\n", initialized_before);


    // Restore configuration
    if (initialized_before) {
        // Restoring filters
        for (i = 0; i < E10K_N5TFILTERS; i++) {
            if (c->ftfilters[i].enabled) {
                e10k_5tfilter_setup(c, i, c->ftfilters + i);
            }
        }

        // Restoring queues
        for (i = 0; i < E10K_NQUEUES; i++) {
            if (c->queues[i].enabled) {
                queue_hw_init(c, i);
            }
        }

        DEBUG("Configuration restored\n");
    }

    c->initialized = 1;
}


bool e10k_card_init(struct e10k_card *c)
{
    device_init(c);
    return true;
}

bool e10k_card_release(struct e10k_card *c)
{
    stop_device(c);
    c->running = false;
    return true;
}

bool e10k_card_reset(struct e10k_card *c)
{
    return false;
}

bool e10k_card_queue_init(struct e10k_card *c, uint8_t n,
                     struct e10k_queue_params *params)
{
    struct e10k_queue_state *qst = c->queues + n;

    if (n >= E10K_NQUEUES) {
        fprintf(stderr, "e10k_queue_init: Invalid queue index\n");
        return false;
    }

    if (qst->enabled) {
        fprintf(stderr, "e10k_queue_init: Queue already initialized\n");
        return false;
    }

    if (params->use_irq) {
        fprintf(stderr, "e10k_queue_init: IRQs are currently not supported\n");
        return false;
    }

    /*if (c->use_irq && params->msix_intvec != 0 && !c->msix) {
        printf("e10k: Queue %d requests MSI-X, but MSI-X is not enabled "
                " card driver. Ignoring queue\n", n);
        return;
    }*/

    // Save state so we can restore the configuration in case we need to do a
    // reset
    qst->enabled = true;
    qst->tx_ring_phys = params->tx_ring_phys;
    qst->tx_ring_size = params->tx_ring_size;
    qst->tx_hwb_phys  = params->tx_hwb_phys;
    qst->rx_ring_phys = params->rx_ring_phys;
    qst->rx_ring_size = params->rx_ring_size;
    qst->rxbufsz      = params->rxbufsz;
    qst->use_irq      = params->use_irq;
    qst->use_rsc      = params->use_rsc;
    qst->opaque       = params->opaque;
    qst->tx_head      = 0;
    qst->rx_head      = 0;
    /*qst->msix_index = -1;
    qst->msix_intvec = msix_intvec;
    qst->msix_intdest = msix_intdest;*/

    queue_hw_init(c, n);
    return true;
}

bool e10k_card_queue_stop(struct e10k_card *c, uint8_t n)
{
    struct e10k_queue_state *qst = c->queues + n;

    if (n >= E10K_NQUEUES) {
        fprintf(stderr, "e10k_queue_stop: Invalid queue index\n");
        return false;
    }

    if (!qst->enabled) {
        fprintf(stderr, "e10k_queue_stop: Queue not initialized\n");
        return false;
    }

    queue_hw_stop(c, n);
    return true;
}

/** Setup a particular 5-tuple filter */
bool e10k_5tfilter_setup(struct e10k_card *c, uint8_t idx,
                         struct e10k_5tfilter *filter)
{
    uint16_t m = filter->mask;
    e10k_l4_proto_t p;
    e10k_ftqf_t ftqf = 0;
    e10k_l34timir_t timir = 0;
    e10k_sdpqf_t sdpqf = 0;

    if (idx >= E10K_N5TFILTERS) {
        fprintf(stderr, "e10k_5tfilter_setup: Invalid filter index\n");
        return false;
    }

    // Prepare filter data
    if (!(m & MASK_SRCPORT))
        sdpqf = e10k_sdpqf_src_port_insert(sdpqf, htons(filter->src_port));
    if (!(m & MASK_DSTPORT))
        sdpqf = e10k_sdpqf_dst_port_insert(sdpqf, htons(filter->dst_port));

    if (!(m & MASK_L4PROTO)) {
        switch (filter->l4_type) {
            case L4_OTHER:  p = e10k_l4other; break;
            case L4_UDP:    p = e10k_l4udp; break;
            case L4_TCP:    p = e10k_l4tcp; break;
            case L4_SCTP:   p = e10k_l4sctp; break;
            default:
                fprintf(stderr, "e10k_5tfilter_setup: Invalid L4 type\n");
                return false;
        }
        ftqf = e10k_ftqf_protocol_insert(ftqf, p);
    }

    // Write filter data
    if (!(m & MASK_SRCIP))
        e10k_saqf_wr(&c->d, idx, htonl(filter->src_ip));
    if (!(m & MASK_DSTIP))
        e10k_daqf_wr(&c->d, idx, htonl(filter->dst_ip));
    e10k_sdpqf_wr(&c->d, idx, sdpqf);

    // Write mask bits
    ftqf = e10k_ftqf_m_srcaddr_insert(ftqf, !!(m & MASK_SRCIP));
    ftqf = e10k_ftqf_m_dstaddr_insert(ftqf, !!(m & MASK_DSTIP));
    ftqf = e10k_ftqf_m_srcport_insert(ftqf, !!(m & MASK_SRCPORT));
    ftqf = e10k_ftqf_m_dstport_insert(ftqf, !!(m & MASK_DSTPORT));
    ftqf = e10k_ftqf_m_protocol_insert(ftqf, !!(m & MASK_L4PROTO));


    // Configure destination queue and enable filter
    timir = e10k_l34timir_rx_queue_insert(timir, filter->queue);
    e10k_l34timir_wr(&c->d, idx, timir);

    ftqf = e10k_ftqf_priority_insert(ftqf, filter->priority);
    ftqf = e10k_ftqf_pool_mask_insert(ftqf, 1);
    ftqf = e10k_ftqf_queue_en_insert(ftqf, 1);
    e10k_ftqf_wr(&c->d, idx, ftqf);

    c->ftfilters[idx] = *filter;
    c->ftfilters[idx].enabled = true;

    return true;
}



