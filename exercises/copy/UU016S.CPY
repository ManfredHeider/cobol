      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU016S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU016S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:46:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *************************************************
      **   EINTRITTS-TABELLE                         **
      **                                             **
      **   AENDERUNG - 19.03.91 - NAUMANN            **
      **   EINFUEHRUNG DER STUFE RANGFREI, D.H.:     **
      **   STUFE RF-T M. STUF-NR. = 888,             **
      **   STUFE RF-R M. STUF-NR. = 889,             **
      **   STUFE RF-A M. STUF-NR. = 890,             **
      **   HERAUSNAHME VON VM09.                     **
      **   VON OCC. 15 AUF OCC. 12 REDUZIERT.        **
      *************************************************
       01  T3-EINTRITTS-TABELLE.
           02  FILLER              PIC X(4) VALUE 'VM01'.
           02  FILLER              PIC X(4) VALUE 'VM02'.
           02  FILLER              PIC X(4) VALUE 'VM03'.
           02  FILLER              PIC X(4) VALUE 'VM04'.
           02  FILLER              PIC X(4) VALUE 'VBA1'.
           02  FILLER              PIC X(4) VALUE 'VBA2'.
           02  FILLER              PIC X(4) VALUE 'VBA3'.
           02  FILLER              PIC X(4) VALUE 'AL  '.
           02  FILLER              PIC X(4) VALUE 'RF-T'.
           02  FILLER              PIC X(4) VALUE 'RF-R'.
           02  FILLER              PIC X(4) VALUE 'RF-A'.
           02  FILLER              PIC X(4) VALUE 'ENDE'.
       01  T3-EINTRITTS-TABR  REDEFINES T3-EINTRITTS-TABELLE.
           02  T3-EINTR-TAB1     OCCURS 12.
               05  T3-EINT        PIC XXXX.

