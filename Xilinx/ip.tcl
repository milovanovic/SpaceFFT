# SelectIO
create_ip -name selectio_wiz -vendor xilinx.com -library ip -version 5.1 -module_name SelectIO_valid
set_property -dict [list \
CONFIG.BUS_SIG_TYPE {DIFF} \
CONFIG.BUS_IO_STD {LVDS_25} \
CONFIG.SELIO_ACTIVE_EDGE {DDR} \
CONFIG.USE_SERIALIZATION {true} \
CONFIG.SERIALIZATION_FACTOR {8} \
CONFIG.SELIO_CLK_BUF {MMCM} \
CONFIG.SELIO_CLK_SIG_TYPE {SINGLE} \
CONFIG.SYSTEM_DATA_WIDTH {1} \
CONFIG.SELIO_INTERFACE_TYPE {NETWORKING} \
CONFIG.SELIO_CLK_IO_STD {HSTL_I} \
CONFIG.CLK_FWD_SIG_TYPE {DIFF} \
CONFIG.CLK_FWD_IO_STD {LVDS_25}] [get_ips SelectIO_valid]


# SYS RESET
create_ip -name proc_sys_reset -vendor xilinx.com -library ip -version 5.0 -module_name RESET_SYS
set_property -dict [list CONFIG.C_EXT_RESET_HIGH {1} CONFIG.C_AUX_RESET_HIGH {1}] [get_ips RESET_SYS]

# PLL_LVDS
create_ip -name clk_wiz -vendor xilinx.com -library ip -version 6.0 -module_name PLL_LVDS
set_property -dict [list \
CONFIG.Component_Name {PLL_LVDS} \
CONFIG.PRIMITIVE {PLL} \
CONFIG.PRIM_SOURCE {Differential_clock_capable_pin} \
CONFIG.CLKOUT2_USED {true} \
CONFIG.CLKOUT1_REQUESTED_OUT_FREQ {300} \
CONFIG.CLKOUT2_REQUESTED_OUT_FREQ {75} \
CONFIG.CLKOUT1_DRIVES {BUFG} \
CONFIG.CLKOUT2_DRIVES {BUFG} \
CONFIG.CLKOUT3_DRIVES {BUFG} \
CONFIG.CLKOUT4_DRIVES {BUFG} \
CONFIG.CLKOUT5_DRIVES {BUFG} \
CONFIG.CLKOUT6_DRIVES {BUFG} \
CONFIG.CLKOUT7_DRIVES {BUFG} \
CONFIG.MMCM_BANDWIDTH {OPTIMIZED} \
CONFIG.MMCM_CLKFBOUT_MULT_F {9} \
CONFIG.MMCM_COMPENSATION {ZHOLD} \
CONFIG.MMCM_CLKOUT0_DIVIDE_F {3} \
CONFIG.MMCM_CLKOUT1_DIVIDE {12} \
CONFIG.NUM_OUT_CLKS {2} \
CONFIG.CLKOUT1_JITTER {111.879} \
CONFIG.CLKOUT1_PHASE_ERROR {105.461} \
CONFIG.CLKOUT2_JITTER {146.170} \
CONFIG.CLKOUT2_PHASE_ERROR {105.461}] [get_ips PLL_LVDS]
