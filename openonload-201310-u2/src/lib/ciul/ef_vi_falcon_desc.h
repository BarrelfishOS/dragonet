/*
** Copyright 2005-2014  Solarflare Communications Inc.
**                      7505 Irvine Center Drive, Irvine, CA 92618, USA
** Copyright 2002-2005  Level 5 Networks Inc.
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of version 2.1 of the GNU Lesser General Public
** License as published by the Free Software Foundation.
**
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Lesser General Public License for more details.
*/

//////////////---- Descriptors C Headers ----//////////////
// Receive Kernel IP Descriptor
  #define RX_KER_BUF_SIZE_LBN 48
  #define RX_KER_BUF_SIZE_WIDTH 14
  #define RX_KER_BUF_REGION_LBN 46
  #define RX_KER_BUF_REGION_WIDTH 2
      #define RX_KER_BUF_REGION0_DECODE 0
      #define RX_KER_BUF_REGION1_DECODE 1
      #define RX_KER_BUF_REGION2_DECODE 2
      #define RX_KER_BUF_REGION3_DECODE 3
  #define RX_KER_BUF_ADR_LBN 0
  #define RX_KER_BUF_ADR_WIDTH 46
// Receive User IP Descriptor
  #define RX_USR_2BYTE_OFS_LBN 20
  #define RX_USR_2BYTE_OFS_WIDTH 12
  #define RX_USR_BUF_ID_LBN 0
  #define RX_USR_BUF_ID_WIDTH 20
// Transmit Kernel IP Descriptor
  #define TX_KER_PORT_LBN 63
  #define TX_KER_PORT_WIDTH 1
  #define TX_KER_CONT_LBN 62
  #define TX_KER_CONT_WIDTH 1
  #define TX_KER_BYTE_CNT_LBN 48
  #define TX_KER_BYTE_CNT_WIDTH 14
  #define TX_KER_BUF_REGION_LBN 46
  #define TX_KER_BUF_REGION_WIDTH 2
      #define TX_KER_BUF_REGION0_DECODE 0
      #define TX_KER_BUF_REGION1_DECODE 1
      #define TX_KER_BUF_REGION2_DECODE 2
      #define TX_KER_BUF_REGION3_DECODE 3
  #define TX_KER_BUF_ADR_LBN 0
  #define TX_KER_BUF_ADR_WIDTH 46
// Transmit User IP Descriptor
  #define TX_USR_PORT_LBN 47
  #define TX_USR_PORT_WIDTH 1
  #define TX_USR_CONT_LBN 46
  #define TX_USR_CONT_WIDTH 1
  #define TX_USR_BYTE_CNT_LBN 33
  #define TX_USR_BYTE_CNT_WIDTH 13
  #define TX_USR_BUF_ID_LBN 13
  #define TX_USR_BUF_ID_WIDTH 20
  #define TX_USR_BYTE_OFS_LBN 0
  #define TX_USR_BYTE_OFS_WIDTH 13