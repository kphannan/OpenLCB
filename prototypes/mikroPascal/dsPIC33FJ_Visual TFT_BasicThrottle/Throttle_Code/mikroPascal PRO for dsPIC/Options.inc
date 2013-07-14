

{.$DEFINE SIMULATE_SPI}                 // Skips over hardware loops in the SPI Code

{.$DEFINE UART_ENABLE}                  // Adds/Removes all UART Code from the build

{.$DEFINE PINCHANGE_INTERRUPT_ENABLE}   // Enables the Pin Change Interrupt

{.$DEFINE SPI_ENABLE}                   // Enables the SPI Module

{$DEFINE dsPIC33}			// Defines the target as a dsPIC33 instead of a dsPIC30		

{$DEFINE SUPPORT_AT_LEAST_ONE_CONSUMED_EVENT}
{$DEFINE SUPPORT_AT_LEAST_ONE_PRODUCED_EVENT}

{.$DEFINE SUPPORT_AT_LEAST_ONE_VNODE_CONSUMED_EVENT}
{.$DEFINE SUPPORT_AT_LEAST_ONE_VNODE_PRODUCED_EVENT}

{.$DEFINE SUPPORT_VIRTUAL_NODES}
{$DEFINE CAN_BUS}

{.$DEFINE DISABLE_DATAGRAM_ABANDON_TIMEOUT}
