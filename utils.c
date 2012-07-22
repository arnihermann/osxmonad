#include "utils.h"

#include <Carbon/Carbon.h>

void printWindow(Window *win) {
    printf(
        "-- %s (x: %f y: %f) (%f x %f)\n",
        win->name,
        win->pos.x, win->pos.y,
        win->size.width, win->size.height
        );
}

void getProcessWindows(ProcessSerialNumber *psn, CFArrayRef *windows) {
    pid_t pid;
    GetProcessPID(psn, &pid);
    AXUIElementRef app = AXUIElementCreateApplication(pid);

    CFBooleanRef boolRef;
    AXUIElementCopyAttributeValue(app, kAXHiddenAttribute, &boolRef);
    if(boolRef == NULL || CFBooleanGetValue(boolRef)) {
        *windows = NULL;
    } else {
        AXUIElementCopyAttributeValue(app, kAXWindowsAttribute, windows);
    }

    CFRelease(app);
}

void getWindowTitle(CFStringRef *windowTitle, AXUIElementRef window) {
    AXUIElementCopyAttributeValue(window, kAXTitleAttribute, windowTitle);
}

CGPoint getWindowPosition(AXUIElementRef window) {
    AXValueRef valueRef;
    CGPoint pos;

    AXUIElementCopyAttributeValue(window, kAXPositionAttribute, &valueRef);
    AXValueGetValue(valueRef, kAXValueCGPointType, &pos);
    CFRelease(valueRef);

    return pos;
}

void setWindowPosition(CGPoint point, AXUIElementRef window) {
    AXValueRef valueRef = AXValueCreate(kAXValueCGPointType, &point);
    AXUIElementSetAttributeValue(window, kAXPositionAttribute, valueRef);
    CFRelease(valueRef);
}

CGSize getWindowSize(AXUIElementRef window) {
    AXValueRef valueRef;
    CGSize size;

    AXUIElementCopyAttributeValue(window, kAXSizeAttribute, &valueRef);
    AXValueGetValue(valueRef, kAXValueCGSizeType, &size);
    CFRelease(valueRef);

    return size;
}

void setWindowSize(CGSize size, AXUIElementRef window) {
    AXValueRef valueRef = AXValueCreate(kAXValueCGSizeType, &size);
    AXUIElementSetAttributeValue(window, kAXSizeAttribute, valueRef);
    CFRelease(valueRef);
}

void setWindow(Window *window) {
    setWindowPosition(window->pos, window->uiElement);
    setWindowSize(window->size, window->uiElement);
}

void addWindows(CFArrayRef windows, Windows *context, int *count) {
    int j;
    for(j = 0; j < CFArrayGetCount(windows) && *count < WINDOWS_ELEMENTS_LENGTH; j++) {
        AXUIElementRef window = CFArrayGetValueAtIndex(windows, j);

        CFBooleanRef boolRef;
        AXUIElementCopyAttributeValue(window, kAXMinimizedAttribute, &boolRef);
        if(boolRef == NULL || CFBooleanGetValue(boolRef)) {
            continue;
        }

        CFStringRef windowTitle;
        getWindowTitle(&windowTitle, window);
        if(windowTitle == NULL) continue;

        char *buffer = malloc(sizeof(char) * WINDOW_NAME_LENGTH);
        CFStringGetCString(windowTitle, buffer, WINDOW_NAME_LENGTH, kCFStringEncodingUTF8);

        context->elements[*count] = malloc(sizeof(Window));
        context->elements[*count]->uiElement = window;
        context->elements[*count]->name = buffer;
        context->elements[*count]->size = getWindowSize(window);
        context->elements[*count]->pos = getWindowPosition(window);

        (*count)++;
    }
}

int getWindows(Windows *context) {
    int count = 0;

    context->elements = malloc(sizeof(Window*) * WINDOWS_ELEMENTS_LENGTH);
    memset(context->elements, 0, sizeof(Window*) * WINDOWS_ELEMENTS_LENGTH);

    ProcessSerialNumber psn = {0, kNoProcess};
    while(!GetNextProcess(&psn)) {
        CFArrayRef windows;
        getProcessWindows(&psn, &windows);
        if(windows == NULL) continue;

        addWindows(windows, context, &count);
    }

    return count;
}

void freeWindows(Windows *context) {
    int i;
    for(i = 0; context->elements[i] != NULL; i++) {
        free(context->elements[i]->name);
        context->elements[i]->name = NULL;

        free(context->elements[i]);
        context->elements[i] = NULL;
    }
    free(context->elements);
    context->elements = NULL;
}

void getScreenSize(CGSize *size) {
    size->width = CGDisplayPixelsWide(CGMainDisplayID());
    size->height = CGDisplayPixelsHigh(CGMainDisplayID());
}
