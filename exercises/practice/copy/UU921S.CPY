      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU921S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU921S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:07:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    --------------------------------------------------------
      *    COPY-MEMBER: TEXT-TABELLE 1  FUER ALLE TRANSAKTIONEN
      *
      *        AUFRUF:  TEXTTAB1  COPY  UU921S
      *
      *                                    ERSTELLT: H.SCHORN (AGS)
      *                                       DATUM: 10.01.80
      *    --------------------------------------------------------
CCC009*    SKIP1
       01  UU921S.
           02  FILLER      PIC X(79)       VALUE '***  BITTE MELDEN SIE
      -                                          'SICH UEBER DAS KONTROL
      -                                          'L-SYSTEM AN  ***
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  EINGEGEBENE A
      -                                          'NWENDUNG UNGUELTIG
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  KEINE ZUGRIF
      -                                          'FSBERECHTIGUNG FUER EI
      -                                          'NGEGEBENEN ORDNUNGSBEG
      -                                          'RIFF         '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  UNGUELTIGE EI
      -                                          'NGABE
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
       01  FILLER  REDEFINES  TEXTTAB1.
           02  TEXT1                   PIC X(79)    OCCURS 4.

