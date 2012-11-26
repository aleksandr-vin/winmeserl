#include <stdio.h>
#include <tchar.h>
#include <windows.h>
#include <wincon.h>

FILE *log;
char log_buffer[4096];
#define LOG_BUFF_HEAD_SIZE    (6)
#define LOG_BUFF_SIZE         (4096 - LOG_BUFF_HEAD_SIZE)
#define LOG_BUFF              (log_buffer + LOG_BUFF_HEAD_SIZE)
#define LOG_BUFF_HEAD         (log_buffer)

#define LOG_LEVEL_BUFFER(L) \
  { char *_lbh = LOG_BUFF_HEAD; \
    char *_ll = #L; \
    _lbh[0] = _ll[0]; \
    _lbh[1] = _ll[1]; \
    _lbh[2] = _ll[2]; \
    _lbh[3] = _ll[3]; \
    _lbh[4] = _ll[4]; \
    _lbh[5] = ':'; }

#define LOG_AS_PACKETS(L) \
  { LOG_LEVEL_BUFFER(L); \
    write_packet(LOG_BUFF_HEAD, strlen(LOG_BUFF_HEAD), stdout); }

#define LOG_AS_TEXT(L) \
  { fprintf(log, "%s\n", LOG_BUFF); fflush(log); }

#define LOG_MSG_UNCOND(L) \
  { if (log_as_packets) { LOG_AS_PACKETS(L); } \
    else { LOG_AS_TEXT(L); } }

#define LOG_MSG(L) \
  if (!quiet_mode) { LOG_MSG_UNCOND(L); } \

#define LOG_DEBUG(M) \
  { sprintf_s(LOG_BUFF , LOG_BUFF_SIZE, "%s", M); LOG_MSG(debug); }

#define LOG_DEBUG1(F,A1) \
  { sprintf_s(LOG_BUFF, LOG_BUFF_SIZE, F, A1); LOG_MSG(debug); }

#define LOG_DEBUG4(F,A1,A2,A3,A4) \
  { sprintf_s(LOG_BUFF, LOG_BUFF_SIZE, F, A1, A2, A3, A4); LOG_MSG(debug); }

#define LOG_ERROR(M) \
  { sprintf_s(LOG_BUFF, LOG_BUFF_SIZE, "%s", M); LOG_MSG(error); }

#define LOG_ERROR1(F,A1) \
  { sprintf_s(LOG_BUFF, LOG_BUFF_SIZE, F, A1); LOG_MSG(error); }

#define MYCLASSNAME _T("win_mesg_cons_gw")

typedef __int8 uint8_t;
typedef __int32 uint32_t;

#define ERR_BADARGS       1
#define ERR_BAD_REPLY     2
#define ERR_READ          10
#define ERR_READ_HEADER   11
#define ERR_PACKET_SIZE   12
#define ERR_WRITE         13
#define ERR_READ_TIMEDOUT 14
#define ERR_SIZE_UNSUPP   100
#define ERR_SETTIMER      101


void show_usage(int argc, _TCHAR* argv[], int i)
{
  if (i > 0)
  {
    // nothing for now
  }

  fprintf(stderr,
    "Usage: %s [options]\n"
    "Options:\n"
    "  -q        - quiet mode\n"
    "  -b <time> - heartbeat every <time> seconds (default is 0)\n"
    "  -s        - sync heartbeat\n"
    "  -l        - send log messages as packets\n"
    "  -d        - dump log messages to win_mesg_cons_gw.log\n"
    "  -h        - this usage page\n"
    , "win_mesg_cons_gw");
}

BOOL quiet_mode = false;
UINT heartbeat_time = 0;
BOOL sync_heartbeat_mode = false;
BOOL log_as_packets = false;
BOOL log_to_file = false;

int get_opts(int argc, _TCHAR* argv[])
{
  int i;
  for (i = 1; i < argc; ++i)
  {
    if ('-' != (argv[i])[0])
    {
      show_usage(argc, argv, i);
      exit(ERR_BADARGS);
    }

    _TCHAR c = (argv[i])[1];
    switch (c)
    {
    case 'q':
      quiet_mode = true;
      break;
    case 's':
      sync_heartbeat_mode = true;
      break;
    case 'l':
      log_as_packets = true;
      break;
    case 'd':
      log_to_file = true;
      break;
    case 'b':
      ++i;
      if (i < argc)
      {
        if (1 != swscanf((argv[i]), _T("%u"), &heartbeat_time))
        {
          show_usage(argc, argv, i);
          exit(ERR_BADARGS);
        }
      }
      else
      {
        show_usage(argc, argv, i);
        exit(ERR_BADARGS);
      }
      break;
    case 'h':
      show_usage(argc, argv, 0);
      exit(ERR_BADARGS);
      break;
    default:
      show_usage(argc, argv, i);
      exit(ERR_BADARGS);
    }
  }

  return 0;
}


static void write_packet(char *buf, int sz, FILE *fd)
{
  uint8_t hd[4];
  hd[0] = (sz >> 24) & 0xff;
  hd[1] = (sz >> 16) & 0xff;
  hd[2] = (sz >> 8) & 0xff;
  hd[3] = sz & 0xff;
  if (4 != fwrite(hd, 1, 4, fd))
  {
    LOG_ERROR1("write failed: %d", GetLastError());
    exit(ERR_WRITE);
  }

  if (sz != fwrite(buf, 1, sz, fd))
  {
    LOG_ERROR1("write failed: %d", GetLastError());
    exit(ERR_WRITE);
  }

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


static void read_packet(uint8_t *buf, size_t max, FILE *fd, int timeout = 0)
{
  if (0 < timeout)
  {
    DWORD ret = WaitForSingleObject(GetStdHandle(STD_INPUT_HANDLE), timeout);
    switch (ret)
    {
    case WAIT_TIMEOUT:
      LOG_ERROR("WaitForSingleObject for stdin timed out");
      exit(ERR_READ_TIMEDOUT);
      break;
    case WAIT_FAILED:
      LOG_ERROR("WaitForSingleObject for stdin failed");
      break;
    case WAIT_ABANDONED:
      LOG_ERROR("WaitForSingleObject for stdin abandoned");
      break;
    case WAIT_OBJECT_0:
      break;
    }
  }

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
  get_opts(argc, argv);

  log = stderr;

  if (log_to_file)
  {
    log = fopen("win_mesg_cons_gw.log", "a");
  }

  LOG_DEBUG1("Starting %s", "window messages console gateway");

  LOG_DEBUG1("HWND size is: %d", sizeof(HWND));
  LOG_DEBUG1("UINT size is: %d", sizeof(UINT));
  LOG_DEBUG1("WPARAM size is: %d", sizeof(WPARAM));
  LOG_DEBUG1("LPARAM size is: %d", sizeof(LPARAM));

  if (sizeof(LPARAM) != 4 ||
      sizeof(WPARAM) != 4 ||
      sizeof(UINT) != 4 ||
      sizeof(HWND) != 4)
    {
      LOG_ERROR("Unsupported size of types");
      exit(ERR_SIZE_UNSUPP);
    }

  HWND hWnd = GetConsoleWindow();
  LOG_DEBUG1("GetConsoleWindow: %d", hWnd);
  LONG_PTR v;
  LOG_DEBUG1("FYI, GetLastError: %d", GetLastError());
  HINSTANCE hInstance = (HINSTANCE)(v = GetWindowLongPtr(hWnd, GWLP_HINSTANCE));
  LOG_DEBUG1("GetWindowLongPtr(hWnd, GWLP_HINSTANCE): %d", v);

  return _tWinMain(hInstance, NULL, NULL, 0);
}

//////////////----------------

void start_heartbeat()
{
  if (heartbeat_time > 0)
  {
    HWND hWnd = NULL;
    UINT_PTR nIDEvent = NULL;
    UINT uElapse = heartbeat_time * 1000;
    TIMERPROC lpTimerFunc = NULL;

    if (0 == SetTimer(hWnd, nIDEvent, uElapse, lpTimerFunc))
    {
      LOG_ERROR("Failed to set timer");
      exit(ERR_SETTIMER);
    }

    LOG_DEBUG1("Heartbeat started with %d sec interval", heartbeat_time);
  }
}

int get_ok_reply()
{
  static char rb[10];
  read_packet(rb, 2, stdin, 3000);
  if ('o' == rb[0] && 'k' == rb[1])
  {
    return 1;
  }
  return 0;
}

char *sb = "heartbeat";

void heartbeat()
{
  LOG_DEBUG("Heartbeat!");
  write_packet(sb, 9, stdout);

  if (sync_heartbeat_mode)
  {
    if (0 == get_ok_reply())
    {
      LOG_ERROR("Bad comm reply");
      exit(ERR_BAD_REPLY);
    }
    else
    {
      LOG_DEBUG("Heartbeat synced!");
    }
  }
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

  start_heartbeat();

  // Main message loop:
  while (GetMessage(&msg, NULL, 0, 0))
  {
    LOG_DEBUG("GetMessage returned");
    LOG_DEBUG4("message, hwnd: \\x%08x, message: \\x%08x, "
      "wParam: \\x%08x, lParam: \\x%08x",
      msg.hwnd, msg.message, msg.wParam, msg.lParam);

    send_enode(msg.hwnd, msg.message, msg.wParam, msg.lParam);

    switch (msg.message)
    {
    case WM_TIMER:
      heartbeat();
      break;
    }

    //TranslateMessage(&msg);
    //DispatchMessage(&msg);
    LOG_DEBUG("Calling GetMessage...");
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
    LOG_ERROR1("CreateWindow failed, GetLastError: %d", GetLastError());
    return FALSE;
  }

  LOG_DEBUG1("CreateWindow succeded, hWnd: %d", hWnd);

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

  LOG_DEBUG4("window proc call, hwnd: \\x%08x, message: \\x%08x, "
    "wParam: \\x%08x, lParam: \\x%08x",
    hWnd, message, wParam, lParam);

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
