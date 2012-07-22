#include <ApplicationServices/ApplicationServices.h>

#define WINDOW_NAME_LENGTH 255
#define WINDOWS_ELEMENTS_LENGTH 255

typedef struct {
    AXUIElementRef uiElement;
    char *name;
    CGPoint pos;
    CGSize size;
} Window;

typedef struct {
    Window **elements;
} Windows;

void printWindow(Window *);
int getWindows(Windows *);
void freeWindows(Windows *);
void setWindow(Window *);
void getScreenSize(CGSize *);
