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

typedef __int8 uint8_t;
typedef __int32 uint32_t;

#define ERR_READ        10
#define ERR_READ_HEADER 11
#define ERR_PACKET_SIZE 12
#define ERR_SIZE_UNSUPP 100

static void write_packet(char *buf, int sz, FILE *fd)
{
  uint8_t hd[4];
  hd[0] = (sz >> 24) & 0xff;
  hd[1] = (sz >> 16) & 0xff;
  hd[2] = (sz >> 8) & 0xff;
  hd[3] = sz & 0xff;
  fwrite(hd, 1, 4, fd);

  fwrite(buf, 1, sz, fd);
  fflush(fd);
}

static size_t read_bytes(uint8_t *buf, size_t max, FILE *fd)
{
  size_t n;
  n = fread(buf, 1, max, fd);
  if ((n == 0) && !feof(fd))
    {
      exit(ERR_READ);
    }
  return n;
}

static void read_packet(uint8_t *buf, size_t max, FILE *fd)
{
  size_t n, sz;
  uint8_t hd[4];

  n = read_bytes(hd, 4, fd);
  if (n == 0 && feof(fd))
    {
      exit(EXIT_SUCCESS);
    }
  if (n != 4)
    {
      exit(ERR_READ_HEADER);
    }
  sz = (hd[0] << 24) + (hd[1] << 16) + (hd[2] << 8) + hd[3];
  if (sz > max)
    {
      exit(ERR_PACKET_SIZE);
    }
  n = read_bytes(buf, sz, fd);
  if (n != sz)
    {
      exit(ERR_READ);
    }
}

ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);

int _tmain(int argc, _TCHAR* argv[])
{
  LOG_DEBUG1("HWND size is: %d\n", sizeof(HWND));
  LOG_DEBUG1("UINT size is: %d\n", sizeof(UINT));
  LOG_DEBUG1("WPARAM size is: %d\n", sizeof(WPARAM));
  LOG_DEBUG1("LPARAM size is: %d\n", sizeof(LPARAM));

  if (sizeof(LPARAM) != 4 ||
      sizeof(WPARAM) != 4 ||
      sizeof(UINT) != 4 ||
      sizeof(HWND) != 4)
    {
      exit(ERR_SIZE_UNSUPP);
    }

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

static uint8_t send_buffer[1024];

inline
void encode_uint32(uint8_t *p, size_t o, uint32_t v)
{
  p[0 + o] = (v >> 24) & 0xff;
  p[1 + o] = (v >> 16) & 0xff;
  p[2 + o] = (v >> 8) & 0xff;
  p[3 + o] = v & 0xff;
}

void send_enode(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  uint8_t *p = send_buffer;

  /* All sizes must be 4 bytes */
  encode_uint32(p, 0, (uint32_t)hWnd);
  encode_uint32(p, sizeof(HWND), message);
  encode_uint32(p, sizeof(HWND) + sizeof(UINT), wParam);
  encode_uint32(p, sizeof(HWND) + sizeof(UINT) + sizeof(WPARAM), lParam);

  write_packet(send_buffer,
               sizeof(HWND) + sizeof(UINT) + sizeof(WPARAM) + sizeof(LPARAM),
               stdout);
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

  LOG_DEBUG4("message, hwnd: \\x%08x, message: \\x%08x, wParam: \\x%08x, lParam: \\x%08x\n", hWnd, message, wParam, lParam);

  send_enode(hWnd, message, wParam, lParam);

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
