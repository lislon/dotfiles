/* Copyright (c) 2016 All Rights Reserved
 * Igor Avdeev <lislon@mail.ru>
 *
 * Autohotkey utility to make emacs switch input methods with Ctrl+Shift.
 */
#SingleInstance force
#NoTrayIcon
#IfWinActive, ahk_class Emacs

en := DllCall("LoadKeyboardLayout", "Str", "00000409", "Int", 1)


; Any combination of Ctrl And Shift
Ctrl & Shift::

; Wait till Ctrl is released (Sometimes we hold shift for uppercase letter)
KeyWait, Ctrl


; Check if no other then Shift/Ctrl keys was pressed during C+S combination
if (A_PriorKey = "LShift" or A_PriorKey = "LControl" or A_PriorKey = "RShift" or A_PriorKey = "RControl")
{
  ;; Send emacs toggle-input-method
  Send ^\

  ;; change language to en
  PostMessage 0x50, 0, %en%,, A
}
return