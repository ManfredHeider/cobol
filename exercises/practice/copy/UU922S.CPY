      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU922S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU922S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:07:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    --------------------------------------------------------
      *    COPY-MEMBER: TEXT-TABELLE 2  FUER K & S - TRANSAKTIONEN
      *
      *        AUFRUF:  TEXTTAB2  COPY  UU922S
      *
      *                                    ERSTELLT: H.SCHORN (AGS)
      *                                       DATUM: 10.01.80
      *                     LETZTES AENDERUNGSDATUM: 25.08.83
      *    --------------------------------------------------------
CCC009*    SKIP1
       01  UU922S.
           02  FILLER      PIC X(79)       VALUE '**** SIE SIND FUER DEN
      -                                          ' BILDSCHIRMBETRIEB NIC
      -                                          'HT ZUGELASSEN ****
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** FEHLER **  PERSONAL
      -                                          '-NR. UNGUELTIG
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** FEHLER **  KENNWORT
      -                                          ' UNGUELTIG
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** FEHLER **  ANWENDUN
      -                                          'G UNGUELTIG
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** HINWEIS **  ANWENDU
      -                                          'NG NICHT VORHANDEN
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** HINWEIS **  ANWENDU
      -                                          'NG GESPERRT
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** HINWEIS **  SIE SIN
      -                                          'D FUER DIESE ANWENDUNG
      -                                          ' NICHT ZUGELASSEN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** HINWEIS **  WEITERE
      -                                          ' ANWENDUNGEN UEBER DAT
      -                                          'ENFREIGABE-TASTE
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '** HINWEIS **  DIE ANW
      -                                          'ENDUNG IST FUER DIESEN
      -                                          ' BILDSCHIRM NICHT ZUGE
      -                                          'LASSEN       '.
CCC009*    SKIP2
       01  FILLER  REDEFINES  TEXTTAB2.
           02  TEXT2                   PIC X(79)    OCCURS 9.
CCC009*    SKIP1
      ****************** ENDE COPY-MEMBER UU922S ***********************

