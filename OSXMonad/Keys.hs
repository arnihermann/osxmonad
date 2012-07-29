module OSXMonad.Keys where

import XMonad

-- From HIToolbox/Events.h

osxKeyToX11 :: Int -> KeySym
osxKeyToX11 0x00 = xK_a
osxKeyToX11 0x01 = xK_s
osxKeyToX11 0x02 = xK_d
osxKeyToX11 0x03 = xK_f
osxKeyToX11 0x04 = xK_h
osxKeyToX11 0x05 = xK_g
osxKeyToX11 0x06 = xK_z
osxKeyToX11 0x07 = xK_x
osxKeyToX11 0x08 = xK_c
osxKeyToX11 0x09 = xK_v
osxKeyToX11 0x0B = xK_b
osxKeyToX11 0x0C = xK_q
osxKeyToX11 0x0D = xK_w
osxKeyToX11 0x0E = xK_e
osxKeyToX11 0x0F = xK_r
osxKeyToX11 0x10 = xK_y
osxKeyToX11 0x11 = xK_t
osxKeyToX11 0x12 = xK_1
osxKeyToX11 0x13 = xK_2
osxKeyToX11 0x14 = xK_3
osxKeyToX11 0x15 = xK_4
osxKeyToX11 0x16 = xK_6
osxKeyToX11 0x17 = xK_5
osxKeyToX11 0x18 = xK_equal
osxKeyToX11 0x19 = xK_9
osxKeyToX11 0x1A = xK_7
osxKeyToX11 0x1B = xK_minus
osxKeyToX11 0x1C = xK_8
osxKeyToX11 0x1D = xK_0
osxKeyToX11 0x1E = xK_bracketright
osxKeyToX11 0x1F = xK_o
osxKeyToX11 0x20 = xK_u
osxKeyToX11 0x21 = xK_bracketleft
osxKeyToX11 0x22 = xK_i
osxKeyToX11 0x23 = xK_p
osxKeyToX11 0x25 = xK_l
osxKeyToX11 0x26 = xK_j
-- TODO: 0x27
osxKeyToX11 0x28 = xK_k
osxKeyToX11 0x29 = xK_semicolon
osxKeyToX11 0x2A = xK_backslash
osxKeyToX11 0x2B = xK_comma
osxKeyToX11 0x2C = xK_slash
osxKeyToX11 0x2D = xK_n
osxKeyToX11 0x2E = xK_m
osxKeyToX11 0x2F = xK_period
osxKeyToX11 0x32 = xK_grave
osxKeyToX11 0x41 = xK_KP_Decimal
osxKeyToX11 0x43 = xK_KP_Multiply
osxKeyToX11 0x45 = xK_KP_Add
-- TODO: 0x47
osxKeyToX11 0x48 = xK_KP_Divide
osxKeyToX11 0x4C = xK_KP_Enter
osxKeyToX11 0x4E = xK_KP_Subtract
osxKeyToX11 0x51 = xK_KP_Equal
osxKeyToX11 0x52 = xK_KP_0
osxKeyToX11 0x53 = xK_KP_1
osxKeyToX11 0x54 = xK_KP_2
osxKeyToX11 0x55 = xK_KP_3
osxKeyToX11 0x56 = xK_KP_4
osxKeyToX11 0x57 = xK_KP_5
osxKeyToX11 0x58 = xK_KP_6
osxKeyToX11 0x59 = xK_KP_7
osxKeyToX11 0x5B = xK_KP_8
osxKeyToX11 0x5C = xK_KP_9

osxKeyToX11 0x24 = xK_Return
osxKeyToX11 0x30 = xK_Tab
osxKeyToX11 0x31 = xK_space
osxKeyToX11 0x33 = xK_Delete
osxKeyToX11 0x35 = xK_Escape
osxKeyToX11 0x37 = xK_Super_L
osxKeyToX11 0x38 = xK_Shift_L
osxKeyToX11 0x39 = xK_Caps_Lock
osxKeyToX11 0x3A = xK_Alt_L
osxKeyToX11 0x3B = xK_Control_L
osxKeyToX11 0x3C = xK_Shift_R
osxKeyToX11 0x3D = xK_Alt_R
osxKeyToX11 0x3E = xK_Control_R
-- TODO: 0x3F
osxKeyToX11 0x40 = xK_F17
-- TODO: 0x48
-- TODO: 0x49
-- TODO: 0x4A
osxKeyToX11 0x4F = xK_F18
osxKeyToX11 0x50 = xK_F19
osxKeyToX11 0x5A = xK_F20
osxKeyToX11 0x60 = xK_F5
osxKeyToX11 0x61 = xK_F6
osxKeyToX11 0x62 = xK_F7
osxKeyToX11 0x63 = xK_F3
osxKeyToX11 0x64 = xK_F8
osxKeyToX11 0x65 = xK_F9
osxKeyToX11 0x67 = xK_F11
osxKeyToX11 0x69 = xK_F13
osxKeyToX11 0x6A = xK_F16
osxKeyToX11 0x6B = xK_F14
osxKeyToX11 0x6D = xK_F10
osxKeyToX11 0x6F = xK_F12
osxKeyToX11 0x71 = xK_F15
osxKeyToX11 0x72 = xK_Help
osxKeyToX11 0x73 = xK_Home
osxKeyToX11 0x74 = xK_Page_Up
osxKeyToX11 0x75 = xK_Delete
osxKeyToX11 0x76 = xK_F4
osxKeyToX11 0x77 = xK_End
osxKeyToX11 0x78 = xK_F2
osxKeyToX11 0x79 = xK_Page_Down
osxKeyToX11 0x7A = xK_F1
osxKeyToX11 0x7B = xK_Left
osxKeyToX11 0x7C = xK_Right
osxKeyToX11 0x7D = xK_Down
osxKeyToX11 0x7E = xK_Up

osxKeyToX11 _ = 0
