      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU923S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU923S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:07:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    --------------------------------------------------------
      *    COPY-MEMBER: TEXT-TABELLE 3  FUER ANTRAGS-TRANSAKTIONEN
      *
      *        AUFRUF:  TEXTTAB3  COPY  UU923S
      *
      *                                    ERSTELLT: H.SCHORN (AGS)
      *                                       DATUM: 10.01.80
      *    AENDERUNG: 10.10.85 - GRUENING
      *               TEXTTAB ERWEITERT (HINWEIS WENN §34C NICHT ER-
      *               ERLAUBT (UU50 - GES 40 U. 41)
      *    --------------------------------------------------------
CCC009*    SKIP1
       01  UU923S.
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  VERMOEGENSBER
      -                                          'ATER-NR. UNGUELTIG
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  GESELLSCHAFTS
      -                                          '-NR. UNGUELTIG
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  VERTRAGS-NR.
      -                                          'UNGUELTIG
      -                                           '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  ORDNUNGSBEGR
      -                                          'IFF NICHT IN ANTRAGS-D
      -                                          'ATEI VORHANDEN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  ORDNUNGSBEGR
      -                                          'IFF NICHT IN VERMOEGEN
      -                                          'SBERATER-DATEI VORHAND
      -                                          'EN           '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PRUEFZIFFER Z
      -                                          'U VERMOEGENSBERATER-NR
      -                                          ' UNGUELTIG
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PRUEFZIFFER Z
      -                                          'U VERTRAGS-NR. UNGUELT
      -                                          'IG
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  ORDNUNGSBEGRI
      -                                          'FF BEREITS IN DER ANTR
      -                                          'AGS-DATEI VORHANDEN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  KEIN DATENSA
      -                                          'TZ ZU MATCHCODE VORHAN
      -                                          'DEN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  DIES IST DER
      -                                          ' LETZTE DATENSATZ ZU D
      -                                          'EM EINGEGEBENEN MATCHC
      -                                          'ODE          '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  WEITERE DATE
      -                                          'NSAETZE ZU MATCHCODE U
      -                                          'EBER PF6 - TASTE ABRUF
      -                                          'EN           '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PF6 - TASTE (
      -                                          'BLAETTERN VORWAERTS) U
      -                                          'NGUELTIG
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PF3 - TASTE (
      -                                          'BLAETTERN RUECKWAERTS)
      -                                          ' UNGUELTIG
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PF10 - TASTE
      -                                          '(ENDGUELTIGE ABMELDUNG
      -                                          ') UNGUELTIG
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PF8 - TASTE (
      -                                          'ANWENDUNGS-VERZEICHNIS
      -                                          ') UNGUELTIG.
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  DER MATCHCODE
      -                                          ' MUSS MINDESTENS 2 ZEI
      -                                          'CHEN LANG SEIN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  KEINE ZUGRIF
      -                                          'FSBERECHTIGUNG FUER EI
      -                                          'NGEGEBENEN ORDNUNGSBEG
      -                                          'RIFF         '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE '***  BITTE MELDEN SIE
      -                                          'SICH UEBER DAS KONTROL
      -                                          'L-SYSTEM AN  ***
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  DER ANTRAG W
      -                                          'URDE NICHT VOLLSTAENDI
      -                                          'G EINGEGEBEN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'BESTAETIGUNG:  DER EIN
      -                                          'GEGEBENE ANTRAG WURDE
      -                                          'ERFASST
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PF1 - TASTE (
      -                                          'AUSKUNFT ANTRAGS-DATEN
      -                                          ') UNGUELTIG
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  PF7 - TASTE (
      -                                          'AUSKUNFT AG8-DATEN) UN
      -                                          'GUELTIG
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  WEDER VERTRAG
      -                                          'S-NR. NOCH MATCH-CODE
      -                                          'EINGEGEBEN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  VERTRAGS-NR.
      -                                          'UND MATCH-CODE EINGEGE
      -                                          'BEN
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'FEHLER:  UNGUELTIGE EI
      -                                          'NGABE
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  DER VB IST A
      -                                          'USGESCHIEDEN, KEINE ER
      -                                          'FASSUNG MOEGLICH
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  DER VM IST Z
      -                                          'UR ZEIT NICHT TAETIG
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'BESTAETIGUNG: DIE ANTR
      -                                          'AGSAENDERUNG WURDE DUR
      -                                          'CHGEFUEHRT
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  DER VM IST Z
      -                                          'UR ZEIT NICHT AKTIV
      -                                          '
      -                                          '             '.
CCC009*    SKIP1
           02  FILLER      PIC X(79)       VALUE 'HINWEIS:  DEM VERMITTL
      -                                          'ER LIEGT KEIN 34C VOR
      -                                          '
      -                                          '             '.
CCC009*    SKIP2
       01  FILLER  REDEFINES  TEXTTAB3.
           02  TEXT3                   PIC X(79)    OCCURS 30.

