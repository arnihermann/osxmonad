#include <ApplicationServices/ApplicationServices.h>

#define WINDOW_NAME_LENGTH 255
#define WINDOWS_ELEMENTS_LENGTH 255

#define SPACES_TRANSITIONING_ID 65538

typedef struct {
    CGWindowID wid;
    AXUIElementRef uiElement;
    char *name;
    CGPoint pos;
    CGSize size;
} Window;

typedef struct {
    Window **elements;
} Windows;

typedef struct {
    int keyCode;
    int altKey;
    int commandKey;
    int controlKey;
    int shiftKey;
} Event;

Event globalEvent;

int getWindows(Windows *);
void freeWindows(Windows *);
void setWindow(Window *);
void setWindowFocused(Window *);
void getFrame(CGPoint *, CGSize *);
void setupEventCallback();
void collectEvent();
bool isSpaceTransitioning();
