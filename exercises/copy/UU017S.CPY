      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU017S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU017S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:46:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *************************************************
      **   AP-PROVISIONS-TABELLE                     **
      ****************************************************************
      *    L O G B U C H                                             *
      *--------------------------------------------------------------*
      * 02/03/89 - EINFUEGEN RD2-STUFE,                 WIECHERS     *
      *            AENDERN KLARTEXT                                  *
      *               GS1 -> GS                                      *
      *               GS2 -> HGS                                     *
      ****************************************************************
       01  T4-PROVISIONS-TABELLE.
           02  FILLER              PIC X(4) VALUE 'AL  '.
           02  FILLER              PIC 999 COMP-3 VALUE 161.
           02  FILLER              PIC X(4) VALUE 'GA  '.
           02  FILLER              PIC 999 COMP-3 VALUE 262.
           02  FILLER              PIC X(4) VALUE 'RGS '.
           02  FILLER              PIC 999 COMP-3 VALUE 272.
           02  FILLER              PIC X(4) VALUE 'GS  '.
           02  FILLER              PIC 999 COMP-3 VALUE 363.
           02  FILLER              PIC X(4) VALUE 'HGS '.
           02  FILLER              PIC 999 COMP-3 VALUE 464.
           02  FILLER              PIC X(4) VALUE 'RD1 '.
           02  FILLER              PIC 999 COMP-3 VALUE 465.
           02  FILLER              PIC X(4) VALUE 'RD2 '.
           02  FILLER              PIC 999 COMP-3 VALUE 566.
           02  FILLER              PIC X(4) VALUE 'ENDE'.
           02  FILLER              PIC 999 COMP-3 VALUE 000.
           02  FILLER              PIC X(4) VALUE '    '.
           02  FILLER              PIC 999 COMP-3 VALUE 000.
           02  FILLER              PIC X(4) VALUE '    '.
           02  FILLER              PIC 999 COMP-3 VALUE 000.
       01  T4-PROVISIONS-TABR  REDEFINES T4-PROVISIONS-TABELLE.
           02  T4-PROV-TAB1     OCCURS 10.
               05  T4-STUFE       PIC XXXX.
               05  T4-NR          PIC 999 COMP-3.

