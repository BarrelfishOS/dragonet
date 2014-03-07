/*
** Copyright 2005-2014  Solarflare Communications Inc.
**                      7505 Irvine Center Drive, Irvine, CA 92618, USA
** Copyright 2002-2005  Level 5 Networks Inc.
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of version 2 of the GNU General Public License as
** published by the Free Software Foundation.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
*/

/****************************************************************************
 * Driver for Solarflare network controllers and boards
 * Copyright 2008-2013 Solarflare Communications Inc.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation, incorporated herein by reference.
 */

#ifndef EFX_MCDI_H
#define EFX_MCDI_H

/**
 * enum efx_mcdi_state - MCDI request handling state
 * @MCDI_STATE_QUIESCENT: No pending MCDI requests. If the caller holds the
 *	mcdi @iface_lock then they are able to move to %MCDI_STATE_RUNNING
 * @MCDI_STATE_RUNNING_SYNC: There is a synchronous MCDI request pending.
 *	Only the thread that moved into this state is allowed to move out of it.
 * @MCDI_STATE_RUNNING_ASYNC: There is an asynchronous MCDI request pending.
 * @MCDI_STATE_COMPLETED: An MCDI request has completed, but the owning thread
 *	has not yet consumed the result. For all other threads, equivalent to
 *	%MCDI_STATE_RUNNING.
 */
enum efx_mcdi_state {
	MCDI_STATE_QUIESCENT,
	MCDI_STATE_RUNNING_SYNC,
	MCDI_STATE_RUNNING_ASYNC,
	MCDI_STATE_COMPLETED,
};

enum efx_mcdi_mode {
	MCDI_MODE_POLL,
	MCDI_MODE_EVENTS,
};

/**
 * struct efx_mcdi_iface - MCDI protocol context
 * @efx: The associated NIC.
 * @state: Request handling state. Waited for by @wq.
 * @mode: Poll for mcdi completion, or wait for an mcdi_event.
 * @wq: Wait queue for threads waiting for @state != %MCDI_STATE_RUNNING
 * @new_epoch: Indicates start of day or start of MC reboot recovery
 * @iface_lock: Serialises access to @seqno, @credits and response metadata
 * @seqno: The next sequence number to use for mcdi requests.
 * @credits: Number of spurious MCDI completion events allowed before we
 *     trigger a fatal error
 * @resprc: Response error/success code (Linux numbering)
 * @resp_hdr_len: Response header length
 * @resp_data_len: Response data (SDU or error) length
 * @async_lock: Serialises access to @async_list while event processing is
 *	enabled
 * @async_list: Queue of asynchronous requests
 * @async_timer: Timer for asynchronous request timeout
 */
struct efx_mcdi_iface {
	struct efx_nic *efx;
	atomic_t state;
	enum efx_mcdi_mode mode;
	wait_queue_head_t wq;
	spinlock_t iface_lock;
	bool new_epoch;
	unsigned int credits;
	unsigned int seqno;
	int resprc;
	size_t resp_hdr_len;
	size_t resp_data_len;
	spinlock_t async_lock;
	struct list_head async_list;
	struct timer_list async_timer;
};

struct efx_mcdi_mon {
	struct efx_buffer dma_buf;
	struct mutex update_lock;
	unsigned long last_update;
#if defined(EFX_USE_KCOMPAT) && defined(EFX_HAVE_HWMON_CLASS_DEVICE)
	struct class_device *device;
#else
	struct device *device;
#endif
	struct efx_mcdi_mon_attribute *attrs;
	unsigned int n_attrs;
};

#ifdef CONFIG_SFC_MTD
struct efx_mcdi_mtd_partition {
	struct efx_mtd_partition common;
	bool updating;
	u16 nvram_type;
	u16 fw_subtype;
};
#endif

#define to_efx_mcdi_mtd_partition(mtd)				\
	container_of(mtd, struct efx_mcdi_mtd_partition, common.mtd)

/**
 * struct efx_mcdi_data - extra state for NICs that implement MCDI
 * @iface: Interface/protocol state
 * @hwmon: Hardware monitor state
 * @fn_flags: Flags for this function, as returned by %MC_CMD_DRV_ATTACH.
 */
struct efx_mcdi_data {
	struct efx_mcdi_iface iface;
#ifdef CONFIG_SFC_MCDI_MON
	struct efx_mcdi_mon hwmon;
#endif
	u32 fn_flags;
};

#ifdef CONFIG_SFC_MCDI_MON
static inline struct efx_mcdi_mon *efx_mcdi_mon(struct efx_nic *efx)
{
	EFX_BUG_ON_PARANOID(!efx->mcdi);
	return &efx->mcdi->hwmon;
}
#endif

extern int efx_mcdi_init(struct efx_nic *efx);
extern void efx_mcdi_fini(struct efx_nic *efx);

extern int efx_mcdi_rpc(struct efx_nic *efx, unsigned cmd,
			const efx_dword_t *inbuf, size_t inlen,
			efx_dword_t *outbuf, size_t outlen,
			size_t *outlen_actual);
extern int efx_mcdi_rpc_quiet(struct efx_nic *efx, unsigned cmd,
			      const efx_dword_t *inbuf, size_t inlen,
			      efx_dword_t *outbuf, size_t outlen,
			      size_t *outlen_actual);

extern int efx_mcdi_rpc_start(struct efx_nic *efx, unsigned cmd,
			      const efx_dword_t *inbuf, size_t inlen);
extern int efx_mcdi_rpc_finish(struct efx_nic *efx, unsigned cmd, size_t inlen,
			       efx_dword_t *outbuf, size_t outlen,
			       size_t *outlen_actual);
extern int efx_mcdi_rpc_finish_quiet(struct efx_nic *efx, unsigned cmd,
				     size_t inlen, efx_dword_t *outbuf,
				     size_t outlen, size_t *outlen_actual);

typedef void efx_mcdi_async_completer(struct efx_nic *efx,
				      unsigned long cookie, int rc,
				      efx_dword_t *outbuf,
				      size_t outlen_actual);
extern int efx_mcdi_rpc_async(struct efx_nic *efx, unsigned int cmd,
			      const efx_dword_t *inbuf, size_t inlen,
			      size_t outlen,
			      efx_mcdi_async_completer *complete,
			      unsigned long cookie);
extern int efx_mcdi_rpc_async_quiet(struct efx_nic *efx, unsigned int cmd,
				    const efx_dword_t *inbuf, size_t inlen,
				    size_t outlen,
				    efx_mcdi_async_completer *complete,
				    unsigned long cookie);

extern void efx_mcdi_display_error(struct efx_nic *efx, unsigned cmd,
				   size_t inlen, efx_dword_t *outbuf,
				   size_t outlen, int rc);

extern int efx_mcdi_poll_reboot(struct efx_nic *efx);
extern void efx_mcdi_mode_poll(struct efx_nic *efx);
extern void efx_mcdi_mode_event(struct efx_nic *efx);
extern void efx_mcdi_flush_async(struct efx_nic *efx);

extern void efx_mcdi_process_event(struct efx_channel *channel,
				   efx_qword_t *event);
extern void efx_mcdi_sensor_event(struct efx_nic *efx, efx_qword_t *ev);

/* We expect that 16- and 32-bit fields in MCDI requests and responses
 * are appropriately aligned, but 64-bit fields are only
 * 32-bit-aligned.  Also, on Siena we must copy to the MC shared
 * memory strictly 32 bits at a time, so add any necessary padding.
 */
#define MCDI_DECLARE_BUF(_name, _len)					\
	efx_dword_t _name[DIV_ROUND_UP(_len, 4)]
#define MCDI_DECLARE_BUF_OUT_OR_ERR(_name, _len)			\
	MCDI_DECLARE_BUF(_name, max_t(size_t, _len, 8))
#define _MCDI_PTR(_buf, _offset)					\
	((u8 *)(_buf) + (_offset))
#define MCDI_PTR(_buf, _field)						\
	_MCDI_PTR(_buf, MC_CMD_ ## _field ## _OFST)
#define _MCDI_CHECK_ALIGN(_ofst, _align)				\
	((_ofst) + BUILD_BUG_ON_ZERO((_ofst) & (_align - 1)))
#define _MCDI_DWORD(_buf, _field)					\
	((_buf) + (_MCDI_CHECK_ALIGN(MC_CMD_ ## _field ## _OFST, 4) >> 2))

#define MCDI_WORD(_buf, _field)						\
	((u16)BUILD_BUG_ON_ZERO(MC_CMD_ ## _field ## _LEN != 2) +	\
	 le16_to_cpu(*(__force const __le16 *)MCDI_PTR(_buf, _field)))
#define MCDI_SET_DWORD(_buf, _field, _value)				\
	EFX_POPULATE_DWORD_1(*_MCDI_DWORD(_buf, _field), EFX_DWORD_0, _value)
#define MCDI_DWORD(_buf, _field)					\
	EFX_DWORD_FIELD(*_MCDI_DWORD(_buf, _field), EFX_DWORD_0)
#define MCDI_POPULATE_DWORD_1(_buf, _field, _name1, _value1)		\
	EFX_POPULATE_DWORD_1(*_MCDI_DWORD(_buf, _field),		\
			     MC_CMD_ ## _name1, _value1)
#define MCDI_POPULATE_DWORD_2(_buf, _field, _name1, _value1,		\
			      _name2, _value2)				\
	EFX_POPULATE_DWORD_2(*_MCDI_DWORD(_buf, _field),		\
			     MC_CMD_ ## _name1, _value1,		\
			     MC_CMD_ ## _name2, _value2)
#define MCDI_POPULATE_DWORD_3(_buf, _field, _name1, _value1,		\
			      _name2, _value2, _name3, _value3)		\
	EFX_POPULATE_DWORD_3(*_MCDI_DWORD(_buf, _field),		\
			     MC_CMD_ ## _name1, _value1,		\
			     MC_CMD_ ## _name2, _value2,		\
			     MC_CMD_ ## _name3, _value3)
#define MCDI_POPULATE_DWORD_4(_buf, _field, _name1, _value1,		\
			      _name2, _value2, _name3, _value3,		\
			      _name4, _value4)				\
	EFX_POPULATE_DWORD_4(*_MCDI_DWORD(_buf, _field),		\
			     MC_CMD_ ## _name1, _value1,		\
			     MC_CMD_ ## _name2, _value2,		\
			     MC_CMD_ ## _name3, _value3,		\
			     MC_CMD_ ## _name4, _value4)
#define MCDI_POPULATE_DWORD_5(_buf, _field, _name1, _value1,		\
			      _name2, _value2, _name3, _value3,		\
			      _name4, _value4, _name5, _value5)		\
	EFX_POPULATE_DWORD_5(*_MCDI_DWORD(_buf, _field),		\
			     MC_CMD_ ## _name1, _value1,		\
			     MC_CMD_ ## _name2, _value2,		\
			     MC_CMD_ ## _name3, _value3,		\
			     MC_CMD_ ## _name4, _value4,		\
			     MC_CMD_ ## _name5, _value5)
#define MCDI_POPULATE_DWORD_6(_buf, _field, _name1, _value1,		\
			      _name2, _value2, _name3, _value3,		\
			      _name4, _value4, _name5, _value5,		\
			      _name6, _value6)				\
	EFX_POPULATE_DWORD_6(*_MCDI_DWORD(_buf, _field),		\
			     MC_CMD_ ## _name1, _value1,		\
			     MC_CMD_ ## _name2, _value2,		\
			     MC_CMD_ ## _name3, _value3,		\
			     MC_CMD_ ## _name4, _value4,		\
			     MC_CMD_ ## _name5, _value5,		\
			     MC_CMD_ ## _name6, _value6)
#define MCDI_POPULATE_DWORD_7(_buf, _field, _name1, _value1,		\
			      _name2, _value2, _name3, _value3,		\
			      _name4, _value4, _name5, _value5,		\
			      _name6, _value6, _name7, _value7)		\
	EFX_POPULATE_DWORD_7(*_MCDI_DWORD(_buf, _field),		\
			     MC_CMD_ ## _name1, _value1,		\
			     MC_CMD_ ## _name2, _value2,		\
			     MC_CMD_ ## _name3, _value3,		\
			     MC_CMD_ ## _name4, _value4,		\
			     MC_CMD_ ## _name5, _value5,		\
			     MC_CMD_ ## _name6, _value6,		\
			     MC_CMD_ ## _name7, _value7)
#define MCDI_SET_QWORD(_buf, _field, _value)				\
	do {								\
		EFX_POPULATE_DWORD_1(_MCDI_DWORD(_buf, _field)[0],	\
				     EFX_DWORD_0, (u32)(_value));	\
		EFX_POPULATE_DWORD_1(_MCDI_DWORD(_buf, _field)[1],	\
				     EFX_DWORD_0, (u64)(_value) >> 32);	\
	} while (0)
#define MCDI_QWORD(_buf, _field)					\
	(EFX_DWORD_FIELD(_MCDI_DWORD(_buf, _field)[0], EFX_DWORD_0) |	\
	(u64)EFX_DWORD_FIELD(_MCDI_DWORD(_buf, _field)[1], EFX_DWORD_0) << 32)
#define MCDI_FIELD(_ptr, _type, _field)					\
	EFX_EXTRACT_DWORD(						\
		*(efx_dword_t *)					\
		_MCDI_PTR(_ptr, MC_CMD_ ## _type ## _ ## _field ## _OFST & ~3),\
		MC_CMD_ ## _type ## _ ## _field ## _LBN & 0x1f,	\
		(MC_CMD_ ## _type ## _ ## _field ## _LBN & 0x1f) +	\
		MC_CMD_ ## _type ## _ ## _field ## _WIDTH - 1)

#define _MCDI_ARRAY_PTR(_buf, _field, _index, _align)			\
	(_MCDI_PTR(_buf, _MCDI_CHECK_ALIGN(MC_CMD_ ## _field ## _OFST, _align))\
	 + (_index) * _MCDI_CHECK_ALIGN(MC_CMD_ ## _field ## _LEN, _align))
#define MCDI_DECLARE_STRUCT_PTR(_name)					\
	efx_dword_t *_name
#define MCDI_ARRAY_STRUCT_PTR(_buf, _field, _index)			\
	((efx_dword_t *)_MCDI_ARRAY_PTR(_buf, _field, _index, 4))
#define MCDI_VAR_ARRAY_LEN(_len, _field)				\
	min_t(size_t, MC_CMD_ ## _field ## _MAXNUM,			\
	      ((_len) - MC_CMD_ ## _field ## _OFST) / MC_CMD_ ## _field ## _LEN)
#define MCDI_ARRAY_WORD(_buf, _field, _index)				\
	(BUILD_BUG_ON_ZERO(MC_CMD_ ## _field ## _LEN != 2) +		\
	 le16_to_cpu(*(__force const __le16 *)				\
		     _MCDI_ARRAY_PTR(_buf, _field, _index, 2)))
#define _MCDI_ARRAY_DWORD(_buf, _field, _index)				\
	(BUILD_BUG_ON_ZERO(MC_CMD_ ## _field ## _LEN != 4) +		\
	 (efx_dword_t *)_MCDI_ARRAY_PTR(_buf, _field, _index, 4))
#define MCDI_SET_ARRAY_DWORD(_buf, _field, _index, _value)		\
	EFX_SET_DWORD_FIELD(*_MCDI_ARRAY_DWORD(_buf, _field, _index),	\
			    EFX_DWORD_0, _value)
#define MCDI_ARRAY_DWORD(_buf, _field, _index)				\
	EFX_DWORD_FIELD(*_MCDI_ARRAY_DWORD(_buf, _field, _index), EFX_DWORD_0)
#define _MCDI_ARRAY_QWORD(_buf, _field, _index)				\
	(BUILD_BUG_ON_ZERO(MC_CMD_ ## _field ## _LEN != 8) +		\
	 (efx_dword_t *)_MCDI_ARRAY_PTR(_buf, _field, _index, 4))
#define MCDI_SET_ARRAY_QWORD(_buf, _field, _index, _value)		\
	do {								\
		EFX_SET_DWORD_FIELD(_MCDI_ARRAY_QWORD(_buf, _field, _index)[0],\
				    EFX_DWORD_0, (u32)(_value));	\
		EFX_SET_DWORD_FIELD(_MCDI_ARRAY_QWORD(_buf, _field, _index)[1],\
				    EFX_DWORD_0, (u64)(_value) >> 32);	\
	} while (0)
#define MCDI_ARRAY_FIELD(_buf, _field1, _type, _index, _field2)		\
	MCDI_FIELD(MCDI_ARRAY_STRUCT_PTR(_buf, _field1, _index),	\
		   _type ## _TYPEDEF, _field2)

#define MCDI_EVENT_FIELD(_ev, _field)			\
	EFX_QWORD_FIELD(_ev, MCDI_EVENT_ ## _field)

extern void efx_mcdi_print_fwver(struct efx_nic *efx, char *buf, size_t len);
extern int efx_mcdi_get_board_cfg(struct efx_nic *efx, u8 *mac_address,
				  u16 *fw_subtype_list, u32 *capabilities);
extern int efx_mcdi_log_ctrl(struct efx_nic *efx, bool evq, bool uart,
			     u32 dest_evq);
extern int efx_mcdi_nvram_types(struct efx_nic *efx, u32 *nvram_types_out);
extern int efx_mcdi_nvram_info(struct efx_nic *efx, unsigned int type,
			       size_t *size_out, size_t *erase_size_out,
			       bool *protected_out);
extern int efx_mcdi_nvram_test_all(struct efx_nic *efx);
extern int efx_mcdi_handle_assertion(struct efx_nic *efx);
extern void efx_mcdi_set_id_led(struct efx_nic *efx, enum efx_led_mode mode);
extern int efx_mcdi_wol_filter_set_magic(struct efx_nic *efx,
					 const u8 *mac, int *id_out);
extern int efx_mcdi_wol_filter_get_magic(struct efx_nic *efx, int *id_out);
extern int efx_mcdi_wol_filter_remove(struct efx_nic *efx, int id);
extern int efx_mcdi_wol_filter_reset(struct efx_nic *efx);
extern int efx_mcdi_flush_rxqs(struct efx_nic *efx);
extern int efx_mcdi_port_probe(struct efx_nic *efx);
extern void efx_mcdi_port_remove(struct efx_nic *efx);
extern int efx_mcdi_port_reconfigure(struct efx_nic *efx);
extern int efx_mcdi_port_get_number(struct efx_nic *efx);
extern u32 efx_mcdi_phy_get_caps(struct efx_nic *efx);
extern void efx_mcdi_process_link_change(struct efx_nic *efx, efx_qword_t *ev);
extern int efx_mcdi_set_mac(struct efx_nic *efx);
#define EFX_MC_STATS_GENERATION_INVALID ((__force __le64)(-1))
extern void efx_mcdi_mac_start_stats(struct efx_nic *efx);
extern void efx_mcdi_mac_stop_stats(struct efx_nic *efx);
extern void efx_mcdi_mac_pull_stats(struct efx_nic *efx);
extern bool efx_mcdi_mac_check_fault(struct efx_nic *efx);
extern enum reset_type efx_mcdi_map_reset_reason(enum reset_type reason);
extern int efx_mcdi_power_monitor(struct efx_nic *efx, bool enable);
extern int efx_mcdi_reset(struct efx_nic *efx, enum reset_type method);
extern int efx_mcdi_set_workaround(struct efx_nic *efx, u32 type, bool enabled);

#ifdef CONFIG_SFC_MCDI_MON
extern int efx_mcdi_mon_probe(struct efx_nic *efx);
extern void efx_mcdi_mon_remove(struct efx_nic *efx);
#else
static inline int efx_mcdi_mon_probe(struct efx_nic *efx) { return 0; }
static inline void efx_mcdi_mon_remove(struct efx_nic *efx) {}
#endif

#ifdef CONFIG_SFC_MTD
extern int efx_mcdi_mtd_read(struct mtd_info *mtd, loff_t start,
			     size_t len, size_t *retlen, u8 *buffer);
extern int efx_mcdi_mtd_erase(struct mtd_info *mtd, loff_t start, size_t len);
extern int efx_mcdi_mtd_write(struct mtd_info *mtd, loff_t start,
			      size_t len, size_t *retlen, const u8 *buffer);
extern int efx_mcdi_mtd_sync(struct mtd_info *mtd);
extern void efx_mcdi_mtd_rename(struct efx_mtd_partition *part);
#endif

#endif /* EFX_MCDI_H */