﻿#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases. /;
#Warn  ; Enable warnings to assist with detecting common errors. /;
SendMode Event  ; Recommended for new scripts due to its superior speed and reliability./;
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory./;

SendEvent {Alt Down}
SendEvent {Tab}
SendEvent {Alt Up}

Sleep, 250

SendEvent {Ctrl down}
SendEvent {v}
SendEvent {Ctrl Up}