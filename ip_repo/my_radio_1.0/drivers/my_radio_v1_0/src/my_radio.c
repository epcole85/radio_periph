

/***************************** Include Files *******************************/
#include "my_radio.h"
#include "xparameters.h"
#include "stdio.h"
#include "xil_io.h"

/************************** Function Definitions ***************************/


void radioTuner_tuneRadio(u32 Addr, float freq)
{
	float pha = (float)(1<<27)/125000000.*freq;
	MY_RADIO_mWriteReg(XPAR_MY_RADIO_0_S00_AXI_BASEADDR, MY_RADIO_S00_AXI_SLV_REG1_OFFSET, pha);
}


void radioTuner_setAdcFreq(unsigned int Addr, float freq)
{
	float pha = (float)(1<<27)/125000000.*freq;
	MY_RADIO_mWriteReg(XPAR_MY_RADIO_0_S00_AXI_BASEADDR, MY_RADIO_S00_AXI_SLV_REG0_OFFSET, pha);
}

void radioTuner_controlReset(u32 BaseAddress, u8 resetval)
{
	MY_RADIO_mWriteReg(XPAR_MY_RADIO_0_S00_AXI_BASEADDR, MY_RADIO_S00_AXI_SLV_REG3_OFFSET, 0x01);
	MY_RADIO_mWriteReg(XPAR_MY_RADIO_0_S00_AXI_BASEADDR, MY_RADIO_S00_AXI_SLV_REG3_OFFSET, 0x00);
}