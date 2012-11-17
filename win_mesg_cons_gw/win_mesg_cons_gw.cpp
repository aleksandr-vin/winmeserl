#include <stdio.h>
#include <tchar.h>
#include <windows.h>
#include <wincon.h>

#define LOG_DEBUG(M) \
  fprintf(stderr, "%s", M)

#define LOG_DEBUG1(F,A1) \
  fprintf(stderr, F, A1)

#define LOG_DEBUG4(F,A1,A2,A3,A4) \
  fprintf(stderr, F, A1, A2, A3, A4)

#define LOG_ERROR1(F,A1) \
  fprintf(stderr, F, A1)

#define MYCLASSNAME _T("win_mesg_cons_gw")

ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);

int _tmain(int argc, _TCHAR* argv[])
{
  HWND hWnd = GetConsoleWindow();
  LOG_DEBUG1("GetConsoleWindow: %d\n", hWnd);
  LONG_PTR v;
  LOG_DEBUG1("FYI, GetLastError: %d\n", GetLastError());
  HINSTANCE hInstance = (HINSTANCE)(v = GetWindowLongPtr(hWnd, GWLP_HINSTANCE));
  LOG_DEBUG1("GetWindowLongPtr(hWnd, GWLP_HINSTANCE): %d\n", v);

  return _tWinMain(hInstance, NULL, NULL, 0);
}

//////////////----------------

#define MAX_LOADSTRING 100

// Global Variables:
HINSTANCE hInst;								// current instance

// Forward declarations of functions included in this code module:
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

int APIENTRY _tWinMain(HINSTANCE hInstance,
                       HINSTANCE hPrevInstance,
                       LPTSTR    lpCmdLine,
                       int       nCmdShow)
{
  UNREFERENCED_PARAMETER(hPrevInstance);
  UNREFERENCED_PARAMETER(lpCmdLine);

  // TODO: Place code here.
  MSG msg;
  HACCEL hAccelTable = NULL;

  // Initialize global strings
  MyRegisterClass(hInstance);

  // Perform application initialization:
  if (!InitInstance (hInstance, nCmdShow))
  {
    return FALSE;
  }


  // Main message loop:
  while (GetMessage(&msg, NULL, 0, 0))
  {
    LOG_DEBUG("GetMessage returned\n");
    if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
    {
      LOG_DEBUG("TranslateAccelerator failed\n");
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }

  return (int) msg.wParam;
}



//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
//  COMMENTS:
//
//    This function and its usage are only necessary if you want this code
//    to be compatible with Win32 systems prior to the 'RegisterClassEx'
//    function that was added to Windows 95. It is important to call this function
//    so that the application will get 'well formed' small icons associated
//    with it.
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
  WNDCLASSEX wcex;

  wcex.cbSize = sizeof(WNDCLASSEX);

  wcex.style			= CS_HREDRAW | CS_VREDRAW;
  wcex.lpfnWndProc	= WndProc;
  wcex.cbClsExtra		= 0;
  wcex.cbWndExtra		= 0;
  wcex.hInstance		= hInstance;
  wcex.hIcon			= NULL;
  wcex.hCursor		= NULL;
  wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
  wcex.lpszMenuName	= NULL;
  wcex.lpszClassName	= MYCLASSNAME;
  wcex.hIconSm		= NULL;

  return RegisterClassEx(&wcex);
}

//
//   FUNCTION: InitInstance(HINSTANCE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
  HWND hWnd;

  hInst = hInstance; // Store instance handle in our global variable

  hWnd = CreateWindow(MYCLASSNAME, MYCLASSNAME, WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

  if (!hWnd)
  {
    LOG_ERROR1("CreateWindow failed, GetLastError: %d\n", GetLastError());
    return FALSE;
  }

  LOG_DEBUG1("CreateWindow succeded, hWnd: %d\n", hWnd);

  ShowWindow(hWnd, nCmdShow);
  UpdateWindow(hWnd);

  return TRUE;
}

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_COMMAND	- process the application menu
//  WM_PAINT	- Paint the main window
//  WM_DESTROY	- post a quit message and return
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  int wmId, wmEvent;
  PAINTSTRUCT ps;
  HDC hdc;

  LOG_DEBUG4("message, hwnd: %d, message: %d, wParam: %d, lParam: %d\n", hWnd, message, wParam, lParam);

  switch (message)
  {
  case WM_COMMAND:
    wmId    = LOWORD(wParam);
    wmEvent = HIWORD(wParam);
    // Parse the menu selections:
    switch (wmId)
    {
    default:
      return DefWindowProc(hWnd, message, wParam, lParam);
    }
    break;
  case WM_PAINT:
    hdc = BeginPaint(hWnd, &ps);
    // TODO: Add any drawing code here...
    EndPaint(hWnd, &ps);
    break;
  case WM_DESTROY:
    PostQuitMessage(0);
    break;
  default:
    return DefWindowProc(hWnd, message, wParam, lParam);
  }
  return 0;
}
