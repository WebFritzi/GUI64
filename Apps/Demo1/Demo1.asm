; This app just shows a window with all the controls
; that are available in GUI64:
; * Listbox
; * RadioButtonGroup
; * Label
; * ColorPicker
; * Single line edit
; * Checkbox
; * Frame
; * Multiline label
; * UpDown
; * Button

!to "gui64app.d64",d64,"gui64app.gui","gui64 app disk"

!source "gui64.inc.asm"

; Constants
WT_TESTAPP = 10
DBLFRAME_RGTBTM = BIT_CTRL_DBLFRAME_RGT + BIT_CTRL_DBLFRAME_BTM

*=$b000
                ldx #<Wnd_TestApp          ; X = lobyte of Wnd_TestApp
                ldy #>Wnd_TestApp          ; Y = hibyte of Wnd_TestApp
                jsr GUI_CreateWindowEx     ; create window from data in Wnd_TestApp
                ; Adjust controls
                ; Listbox
                jsr GUI_SelectControl0     ; copy control struct of control #0 to $20 - $2E
                lda #0                     ; hilights first
                sta ControlHilIndex        ; Listbox entry
                ldx #<Str_LB               ; X = lobyte of Str_LB
                ldy #>Str_LB               ; Y = hibyte of Str_LB
                lda #3                     ; number of strings in string list
                jsr GUI_SetCtrlStringList  ; associate string list to listbox
                ; Radio button group
                jsr GUI_SelectControl1     ; copy control struct of control #1 to $20 - $2E
                lda #0                     ; Selects the first of
                sta ControlHilIndex        ; the two radio buttons
                ldx #<Str_RBG              ; X = lobyte of Str_RBG
                ldy #>Str_RBG              ; Y = hibyte of Str_RBG
                lda #2                     ; number of strings in string list
                jsr GUI_SetCtrlStringList  ; associate string list to Radio Button Group
                ; UpDown
                lda #11                    ; Copy control struct of control #11
                jsr GUI_SelectControl      ; to $20 - $2E
                lda #0                             ; Set
                sta ControlIndex+UPDOWN_DIGIT_LO   ; UpDown value
                sta ControlIndex+UPDOWN_DIGIT_HI   ; to "00",
                sta ControlIndex+UPDOWN_LOWERLIMIT ; the lower limit to zero,
                lda #$10                           ; and the
                sta ControlIndex+UPDOWN_UPPERLIMIT ; upper limit to 10
                jsr GUI_UpdateControl              ; confirm control changes
                ; Edit_SL
                lda #5                     ; Copy control struct of control #5
                jsr GUI_SelectControl      ; to $20 - $2E
                ldx #<Str_EDITSL           ; X = lobyte of Str_EDITSL
                ldy #>Str_EDITSL           ; Y = hibyte of Str_EDITSL
                jsr GUI_SetCtrlString      ; associate string buffer to edit control
                lda #0                     ; set caret position to 0
                ldx #11                    ; and the maximial position
                jsr GUI_SetEditSLInfo      ; to 11
                ;
                jsr GUI_GetDesign          ; Z=1: WIN, Z=0: MAC
                beq +                      ; branches to WIN
                ; MAC
                inc WindowPosY             ; increment Y coord of app window
                dec WindowHeight           ; decrement height of app window
                jsr GUI_UpdateWindow       ; confirm window changes
                ldy #12                    ; set ID and
                lda #DBLFRAME_RGTBTM       ; add bits to
                jsr GUI_SelectCtrl_AddBits ; control #12
                inc ControlPosX            ; increment X coord of button
                jmp GUI_UpdateControl      ; confirm control changes
+               rts

; Window proc
; handles mouse/joystick/keyboard events (see gui64.inc)
TestAppWndProc  jsr GUI_StdWndProc    ; must always be called in a win proc
                jsr GUI_IsInCurControl; if mouse isn't in a control,
                bcc +                 ; return
                lda wndParam0         ; contains event code
                cmp #EC_LBTNRELEASE   ; left mouse button released?
                bne +                 ; if not, return
                lda ControlIndex      ; index of control pressed
                cmp #12               ; is it the button?
                bne +                 ; if not, return
                jsr GUI_KillCurWindow ; Kill app window
                jmp GUI_Repaint
+               rts

; Window:
Str_Title_App   !pet "Demo App",0
Wnd_TestApp     !byte WT_TESTAPP, %00100000, 6, 0, 28, 22, <Str_Title_App, >Str_Title_App
                !byte <TestAppWndProc, >TestAppWndProc
; Controls in window:
                ;0
                !byte CT_LISTBOX, 1, 1, 9, 5
                !pet 0
                ;1
                !byte CT_RADIOBUTTONGROUP, 11, 1, 16, 2
                !pet 0
                ;2
                !byte CT_LABEL, 11, 4, 16, 1
                !pet "Color Picker:",0
                ;3
                !byte CT_COLORPICKER, 25, 4, 2, 1
                !pet 0
                ;4
                !byte CT_LABEL, 1, 7, 8, 1
                !pet "Edit SL:",0
                ;5
                !byte CT_EDIT_SL, 1, 8, 14, 3
                !pet 0
                ;6
                !byte CT_CHECKBOX, 15, 7, 12, 1
                !pet "Checkbox 1",0
                ;7
                !byte CT_CHECKBOX, 15, 9, 12, 1
                !pet "Checkbox 2",0
                ;8
                !byte CT_FRAME, 1, 11, 26, 6
                !pet "Frame",0
                ;9
                !byte CT_LABEL_ML, 2, 13, 9, 2
                !pet "Multiline\Label",0
                ;10
                !byte CT_LABEL, 14, 13, 8, 1
                !pet "UpDown:",0
                ;11
                !byte CT_UPDOWN, 21, 12, 5, 3
                !pet 0
                ;12
                !byte CT_BUTTON, 21, 17, 6, 3
                !pet " OK ",0
                !byte 0

; Strings
Str_RBG         !pet "Radio Button 1",0,"Radio Button 2",0
Str_LB          !pet "Listbox",0,"Entry",0,"Entry",0
Str_EDITSL      !pet "           ",0