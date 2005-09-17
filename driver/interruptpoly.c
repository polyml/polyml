/*
    Title:      Windows application to send an interrupt to Poly
	Copyright (c) 2003
		David C.J. Matthews

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/
#include <windows.h>
#include <ddeml.h>

// DDE Commands.
#define INTERRUPT_POLY	"[Interrupt]"
#define TERMINATE_POLY	"[Terminate]"

DWORD dwDDEInstance;


// DDE Callback function.  We're not interested in anything apart from
// name registrations at the moment.
HDDEDATA CALLBACK DdeCallBack(UINT uType, UINT uFmt, HCONV hconv,
							  HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
							  DWORD dwData1, DWORD dwData2)
{
	switch(uType)
	{
	case XTYP_REGISTER:
		{
			CHAR	chBuff[256];
			chBuff[0] = 0; // Just in case of an error.
			DdeQueryString(dwDDEInstance, hsz1,
				chBuff, sizeof(chBuff), CP_WINANSI);

			return (HDDEDATA) NULL;
		}

	case XTYP_UNREGISTER:
		return (HDDEDATA) NULL;

	default:
		return (HDDEDATA) NULL; 
	}
}
 


void SendDDEMessage(LPCTSTR lpszMessage)
{
	HCONV hcDDEConv;
	HDDEDATA res;
	// Send a DDE message to the process.
	DWORD dwInst = dwDDEInstance;
	HSZ hszServiceName =
		DdeCreateStringHandle(dwInst, m_csSemaName, CP_WINANSI);

	hcDDEConv =
		DdeConnect(dwInst, hszServiceName, hszServiceName, NULL); 
	DdeFreeStringHandle(dwInst, hszServiceName);
	res =
		DdeClientTransaction((LPBYTE)lpszMessage, sizeof(INTERRUPT_POLY),
			hcDDEConv, 0L, 0, XTYP_EXECUTE, TIMEOUT_ASYNC, NULL);
	if (res) DdeFreeDataHandle(res);
}

// Interrupt the ML process as though control-C had been pressed.
void RunInterrupt() 
{
	SendDDEMessage(INTERRUPT_POLY);
}


int WINAPI WinMain(
  HINSTANCE hInstance,  // handle to current instance
  HINSTANCE hPrevInstance,  // handle to previous instance
  LPSTR lpCmdLine,      // pointer to command line
  int nCmdShow          // show state of window
)
{
	// Initialise DDE.  We only want to be a client.
	DdeInitialize(&dwDDEInstance, DdeCallBack,
		APPCMD_CLIENTONLY | CBF_SKIP_CONNECT_CONFIRMS | CBF_SKIP_DISCONNECTS, 0);


	// Run event loop.
}
