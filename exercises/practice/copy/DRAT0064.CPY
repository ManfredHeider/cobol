      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0064.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAT0064.CPv  $
      *
      *   Rev 2.9   Nov 03 2020 14:54:34   t13ae34
      *FAN00927: statt AAT00084 - Profilgruppen
      *
      *   Rev 2.8   Sep 08 2020 09:12:44   t13ae34
      *AAT00084: Profilgruppen für Hostberechtigungen
      *
      *   Rev 2.7   Jul 27 2020 15:28:14   t13ae34
      *AAT00084: Profilgruppen für Hostberechtigungen
      *
      *   Rev 2.6   Jun 29 2020 15:32:32   t13ae34
      *AAT00084: Profilgruppen für Hostberechtigungen
      *
      *   Rev 2.5   May 15 2020 16:15:38   t13ae34
      *AAT00084: Profilgruppen für Hostberechtigungen
      *
      *   Rev 2.4   May 14 2020 09:03:44   t13ae34
      *AAT00084: Profilgruppen für Hostberechtigungen
      *
      *   Rev 2.3   Oct 25 2002 09:26:42   t13ae34
      *AAT00023-KS10 - Doku und Vergabe Host-Kompetenzen
      *verbessern
      *
      *   Rev 2.2   24 Apr 2001 16:29:32   t13ae34
      *AAT00020-KS10 neue Profile (D,E) für
      *Status
      *
      *   Rev 2.1   Aug 02 1999 10:43:48   T13AE34
      *AAT00010-Neukonzeption KS10
      *
      *   Rev 2.0   Feb 27 1999 11:57:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Jul 23 1996 15:54:22   P13SP99
      *AFRZ0028 I13-User dürfen nur V13/W13-User im K+S bearbeiten
      *
      *   Rev 1.0   Nov 04 1995 08:46:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00084000
      *    COPY-MEMBER: DRAT0064                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0064                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S.O.                                             *
      *    DATUM:    S.O.                                             *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00010!16.07.99!2.1 ! BENUTZERPROFILE           ! HEIDER   *
      * AAT00020!23.04.01!2.2 ! BENUTZERPROFILE 'D', 'E'  ! HEIDER   *
      * AAT00023!28.08.02!2.3 ! KOMPETENZEN               ! HEIDER   *
      * AAT00084!11.05.20!2.4 ! GRUPPENPROFILE            ! HEIDER   *
      * AAT00084!20.05.20!2.5 ! ZUORDNUNG PROFIL LÖSCHEN  ! HEIDER   *
      * AAT00084!17.06.20!2.6 ! ZUORDNUNG PROFIL ÄNDERN   ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                                 00085000
       01  FEHLERTABELLE.
           02  FEHLER-000.
               03  FILLER          PIC 999     VALUE 000.
               03  FILLER          PIC X(70)   VALUE
                   '000 - KEINE FEHLERMELDUNG          '.
           02  FEHLER-001.
               03  FILLER          PIC 999     VALUE 001.
               03  FILLER          PIC X(70)   VALUE
           '** BITTE MELDEN SIE SICH UEBER DAS KONTROLL-SYSTEM AN **'.
           02  FEHLER-200.
               03  FILLER          PIC 999     VALUE 200.
               03  FILLER          PIC X(70)   VALUE
                   '200 - UNGUELTIGE FUNKTION (PROGRAMMFEHLER)'.
           02  FEHLER-201.
               03  FILLER          PIC 999     VALUE 201.
               03  FILLER          PIC X(70)   VALUE
                   '201 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-202.
               03  FILLER          PIC 999     VALUE 202.
               03  FILLER          PIC X(70)   VALUE
                   '202 - BITTE USERID EINGEBEN'.
           02  FEHLER-203.
               03  FILLER          PIC 999     VALUE 203.
               03  FILLER          PIC X(70)   VALUE
                   '203 - BITTE FUNKTION EINGEBEN'.
           02  FEHLER-204.
               03  FILLER          PIC 999     VALUE 204.
               03  FILLER          PIC X(70)   VALUE
                   '204 - WERT IM FELD FUNKTION UNGUELTIG'.
           02  FEHLER-205.
               03  FILLER          PIC 999     VALUE 205.
               03  FILLER          PIC X(70)   VALUE
                   '205 - USERID IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-206.
               03  FILLER          PIC 999     VALUE 206.
               03  FILLER          PIC X(70)   VALUE
                   '206 - PROFIL IN BENUTZERDATEI NICHT VORHANDEN'.
251194     02  FEHLER-207.
251194         03  FILLER          PIC 999     VALUE 207.
251194         03  FILLER          PIC X(70)   VALUE
251194             '207 - USERID BEREITS VORHANDEN               '.
110795     02  FEHLER-208.
110795         03  FILLER          PIC 999     VALUE 208.
110795         03  FILLER          PIC X(70)   VALUE
110795             '208 - BENUTZER DARF EIGENE DATEN NICHT ÄNDERN'.
960723     02  FEHLER-209.
960723         03  FILLER          PIC 999     VALUE 209.
960723         03  FILLER          PIC X(70)   VALUE
960723           '209 - BENUTZER DARF NUR V13-/W13-KEYS BEARBEITEN  '.
160799     02  FEHLER-210.
160799         03  FILLER          PIC 999     VALUE 210.
160799         03  FILLER          PIC X(70)   VALUE
160799           '210 - FÜR DIESE FUNKTION HABEN SIE KEINE BERECHTIGUNG
160799-    ' '.
           02  FEHLER-300.
               03  FILLER          PIC 999     VALUE 300.
               03  FILLER          PIC X(70)   VALUE
                   '300 - PROGRAMMFEHLER: TS-BILDKZ UNGÜLTIG'.
           02  FEHLER-301.
               03  FILLER          PIC 999     VALUE 301.
               03  FILLER          PIC X(70)   VALUE
                   '301 - UNGUELTIGE PF-TASTE'.
           02  FEHLER-302.
               03  FILLER          PIC 999     VALUE 302.
               03  FILLER          PIC X(70)   VALUE
                   '302 - UNGUELTIGE PF-TASTE'.
           02  FEHLER-303.
               03  FILLER          PIC 999     VALUE 303.
               03  FILLER          PIC X(70)   VALUE
                   '303 - UNGUELTIGE PF-TASTE'.
           02  FEHLER-304.
               03  FILLER          PIC 999     VALUE 304.
               03  FILLER          PIC X(70)   VALUE
                   '304 - USERID EINGEBEN'.
           02  FEHLER-305.
               03  FILLER          PIC 999     VALUE 305.
               03  FILLER          PIC X(70)   VALUE
                   '305 - USERID IN DATEI NICHT VORHANDEN'.
           02  FEHLER-306.
               03  FILLER          PIC 999     VALUE 306.
               03  FILLER          PIC X(70)   VALUE
                   '306 - USERID IN DATEI NICHT VORHANDEN'.
           02  FEHLER-310.
               03  FILLER          PIC 999     VALUE 310.
               03  FILLER          PIC X(70)   VALUE
                   '310 - UNGÜLTIGE EINGABE / PF-TASTE'.
           02  FEHLER-311.
               03  FILLER          PIC 999     VALUE 311.
               03  FILLER          PIC X(70)   VALUE
                   '311 - UNGÜLTIGE EINGABE / PF-TASTE'.
           02  FEHLER-312.
               03  FILLER          PIC 999     VALUE 312.
               03  FILLER          PIC X(70)   VALUE
                   '312 - UNGÜLTIGE EINGABE / PF-TASTE'.
           02  FEHLER-313.
               03  FILLER          PIC 999     VALUE 313.
               03  FILLER          PIC X(70)   VALUE
                   '313 - ANWENDUNG EINGEBEN'.
           02  FEHLER-314.
               03  FILLER          PIC 999     VALUE 314.
               03  FILLER          PIC X(70)   VALUE
                   '314 - ANWENDUNG IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-315.
               03  FILLER          PIC 999     VALUE 315.
               03  FILLER          PIC X(70)   VALUE
                   '315 - USERID IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-316.
               03  FILLER          PIC 999     VALUE 316.
               03  FILLER          PIC X(70)   VALUE
                   '316 - ANWENDUNG IN TRANSAKT-DATEI NICHT VORHANDEN'.
           02  FEHLER-320.
               03  FILLER          PIC 999     VALUE 320.
               03  FILLER          PIC X(70)   VALUE
                   '320 - USERID IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-321.
               03  FILLER          PIC 999     VALUE 321.
               03  FILLER          PIC X(70)   VALUE
                   '321 - ANWENDUNG IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-330.
               03  FILLER          PIC 999     VALUE 330.
               03  FILLER          PIC X(70)   VALUE
                   '330 - UNGÜLTIGE EINGABE / PF-TASTE'.
           02  FEHLER-331.
               03  FILLER          PIC 999     VALUE 331.
               03  FILLER          PIC X(70)   VALUE
                   '331 - UNGÜLTIGE EINGABE / PF-TASTE'.
           02  FEHLER-332.
               03  FILLER          PIC 999     VALUE 332.
               03  FILLER          PIC X(70)   VALUE
                   '332 - UNGÜLTIGE EINGABE / PF-TASTE'.
           02  FEHLER-333.
               03  FILLER          PIC 999     VALUE 333.
               03  FILLER          PIC X(70)   VALUE
                   '333 - ANWENDUNG IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-400.
               03  FILLER          PIC 999     VALUE 400.
               03  FILLER          PIC X(70)   VALUE
                   '400 - PROGRAMMFEHLER: TS-BILDKZ'.
           02  FEHLER-401.
               03  FILLER          PIC 999     VALUE 401.
               03  FILLER          PIC X(70)   VALUE
                   '401 - J ODER N EINGEBEN'.
           02  FEHLER-402.
               03  FILLER          PIC 999     VALUE 402.
               03  FILLER          PIC X(70)   VALUE
                   '402 - J OODER N EINGEBEN'.
           02  FEHLER-403.
               03  FILLER          PIC 999     VALUE 403.
               03  FILLER          PIC X(70)   VALUE
                   '403 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-404.
               03  FILLER          PIC 999     VALUE 404.
               03  FILLER          PIC X(70)   VALUE
                   '404 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-405.
               03  FILLER          PIC 999     VALUE 405.
               03  FILLER          PIC X(70)   VALUE
                   '405 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-406.
               03  FILLER          PIC 999     VALUE 406.
               03  FILLER          PIC X(70)   VALUE
                   '406 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-407.
               03  FILLER          PIC 999     VALUE 407.
               03  FILLER          PIC X(70)   VALUE
                   '407 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-408.
               03  FILLER          PIC 999     VALUE 408.
               03  FILLER          PIC X(70)   VALUE
                   '408 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-409.
               03  FILLER          PIC 999     VALUE 409.
               03  FILLER          PIC X(70)   VALUE
                   '409 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-410.
               03  FILLER          PIC 999     VALUE 410.
               03  FILLER          PIC X(70)   VALUE
                   '410 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-411.
               03  FILLER          PIC 999     VALUE 411.
               03  FILLER          PIC X(70)   VALUE
                   '411 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-412.
               03  FILLER          PIC 999     VALUE 412.
               03  FILLER          PIC X(70)   VALUE
                   '412 - USERID EINGEBEN'.
           02  FEHLER-413.
               03  FILLER          PIC 999     VALUE 413.
               03  FILLER          PIC X(70)   VALUE
                   '413 - ABTEILUNG EINGEBEN'.
           02  FEHLER-414.
               03  FILLER          PIC 999     VALUE 414.
               03  FILLER          PIC X(70)   VALUE
                   '414 - VBNR UNGUELTIG'.
           02  FEHLER-415.
               03  FILLER          PIC 999     VALUE 415.
               03  FILLER          PIC X(70)   VALUE
                   '415 - STATUS KANN NICHT VERAENDERT WERDEN'.
           02  FEHLER-416.
               03  FILLER          PIC 999     VALUE 416.
               03  FILLER          PIC X(70)   VALUE
                   '416 - STATUS UNGUELTIG'.
           02  FEHLER-417.
               03  FILLER          PIC 999     VALUE 417.
               03  FILLER          PIC X(70)   VALUE
160799*            '417 - _ ODER X EINGEBEN'.
160799             '417 - DIESES BENUTZERPROFIL IST NICHT ANGELEGT'.
           02  FEHLER-418.
               03  FILLER          PIC 999     VALUE 418.
               03  FILLER          PIC X(70)   VALUE
                   '418 - ANWENDUNG EINGEBEN'.
           02  FEHLER-419.
               03  FILLER          PIC 999     VALUE 419.
               03  FILLER          PIC X(70)   VALUE
                   '419 - ANWENDUNG IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-420.
               03  FILLER          PIC 999     VALUE 420.
               03  FILLER          PIC X(70)   VALUE
                   '420 - ANWENDUNG IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-421.
               03  FILLER          PIC 999     VALUE 421.
               03  FILLER          PIC X(70)   VALUE
                   '421 - USERID IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-422.
               03  FILLER          PIC 999     VALUE 422.
               03  FILLER          PIC X(70)   VALUE
                   '422 - USERID IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-423.
               03  FILLER          PIC 999     VALUE 423.
               03  FILLER          PIC X(70)   VALUE
               '423 - ANWENDUNG IN TRANSAKT-DATEI NICHT VORHANDEN'.
           02  FEHLER-424.
               03  FILLER          PIC 999     VALUE 424.
               03  FILLER          PIC X(70)   VALUE
                   '424 - USERID IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-425.
               03  FILLER          PIC 999     VALUE 425.
               03  FILLER          PIC X(70)   VALUE
                   '425 - ANWENDUNG IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-451.
               03  FILLER          PIC 999     VALUE 451.
               03  FILLER          PIC X(70)   VALUE
                   '451 - ANWENDUNG EINGEBEN'.
           02  FEHLER-452.
               03  FILLER          PIC 999     VALUE 452.
               03  FILLER          PIC X(70)   VALUE
                   '452 - ANWENDUNG IN BENUTZERDATEI NICHT VORHANDEN'.
           02  FEHLER-453.
               03  FILLER          PIC 999     VALUE 453.
               03  FILLER          PIC X(70)   VALUE
                   '453 - VP1-DATEN UNGUELTIG'.
           02  FEHLER-454.
               03  FILLER          PIC 999     VALUE 454.
               03  FILLER          PIC X(70)   VALUE
                   '454 - DIREKTION UNGUELTIG'.
           02  FEHLER-455.
               03  FILLER          PIC 999     VALUE 455.
               03  FILLER          PIC X(70)   VALUE
                   '455 - DIREKTION UNGUELTIG'.
           02  FEHLER-456.
               03  FILLER          PIC 999     VALUE 456.
               03  FILLER          PIC X(70)   VALUE
                   '456 - DIREKTION UNGUELTIG'.
           02  FEHLER-500.
               03  FILLER          PIC 999     VALUE 500.
               03  FILLER          PIC X(70)   VALUE
                   '500 - J ODER N EINGEBEN'.
           02  FEHLER-501.
               03  FILLER          PIC 999     VALUE 501.
               03  FILLER          PIC X(70)   VALUE
                   '501 - UNGUELTIGE EINGABE / PF-TASTE'.
           02  FEHLER-502.
               03  FILLER          PIC 999     VALUE 502.
               03  FILLER          PIC X(70)   VALUE
                   '502 - USERID IST GELOESCHT       '.
           02  FEHLER-503.
               03  FILLER          PIC 999     VALUE 503.
               03  FILLER          PIC X(70)   VALUE
                   '503 - USERID IST EINGERICHTET    '.
           02  FEHLER-504.
               03  FILLER          PIC 999     VALUE 504.
               03  FILLER          PIC X(70)   VALUE
                   '504 - SOLL USERID GELOESCHT WERDEN? (J/N)'.
           02  FEHLER-505.
               03  FILLER          PIC 999     VALUE 505.
               03  FILLER          PIC X(70)   VALUE
                   '505 - SOLL USERID ERFASST WERDEN? (J/N)'.
230401     02  FEHLER-506.
               03  FILLER          PIC 999     VALUE 506.
               03  FILLER          PIC X(70)   VALUE
                   '506 - STATUS WURDE GEÄNDERT            '.
280802     02  FEHLER-507.
               03  FILLER          PIC 999     VALUE 507.
               03  FILLER          PIC X(70)   VALUE
                   '507 - WERT NICHT IN KTY=206 (SF14) ERFASST'.
280802     02  FEHLER-508.
               03  FILLER          PIC 999     VALUE 508.
               03  FILLER          PIC X(70)   VALUE
                   '508 - WERT UNGÜLTIG                        '.
280802     02  FEHLER-509.
               03  FILLER          PIC 999     VALUE 509.
               03  FILLER          PIC X(70)   VALUE
                   '509 - GRUPPENPROFILE (PROF...) BITTE ÜBER FUNKTION 0
      -    '2 ERFASSEN'.
280802     02  FEHLER-510.
               03  FILLER          PIC 999     VALUE 510.
               03  FILLER          PIC X(70)   VALUE
                   '510 - BITTE J ODER N! -> INFO: ÄNDERUNG ZU GRUPPENPR
      -    'OFIL!          '.
200520     02  FEHLER-511.
               03  FILLER          PIC 999     VALUE 511.
               03  FILLER          PIC X(70)   VALUE
                   '511 - LÖSCHEN ZUORDNUNG ZU GRUPPENPROFIL ? (J/N)
      -    '               '.
200520     02  FEHLER-512.
               03  FILLER          PIC 999     VALUE 512.
               03  FILLER          PIC X(70)   VALUE
                   '512 - ZUORDNUNG ZU GRUPPENPROFIL WURDE GELÖSCHT !
      -    '               '.
170620     02  FEHLER-513.
               03  FILLER          PIC 999     VALUE 513.
               03  FILLER          PIC X(70)   VALUE
                   '513 - GRUPPENPROFIL (PROF...) NICHT ERFASST!
      -    '               '.
200520     02  FEHLER-514.
               03  FILLER          PIC 999     VALUE 514.
               03  FILLER          PIC X(70)   VALUE
                   '514 - ZUORDNUNG ERST LÖSCHEN, DANN ERFASSEN!
      -    '               '.
200520     02  FEHLER-515.
               03  FILLER          PIC 999     VALUE 515.
               03  FILLER          PIC X(70)   VALUE
                   '515 - ZUORDNUNG ZU GRUPPENPROFIL WURDE GEÄNDERT !
      -    '               '.
200520     02  FEHLER-516.
               03  FILLER          PIC 999     VALUE 516.
               03  FILLER          PIC X(70)   VALUE
                   '516 - ERFASSEN ZUORDNUNG ZU GRUPPENPROFIL ? (J/N)
      -    '               '.
200520     02  FEHLER-517.
               03  FILLER          PIC 999     VALUE 517.
               03  FILLER          PIC X(70)   VALUE
                   '517 - ZUORDNUNG ZU GRUPPENPROFIL WURDE ERFASST !
      -    '               '.
200520     02  FEHLER-518.
               03  FILLER          PIC 999     VALUE 518.
               03  FILLER          PIC X(70)   VALUE
                   '518 - KEINE ZUORDNUNG ZU GRUPPENPROFIL VORHANDEN !
      -    '               '.
170620     02  FEHLER-519.
               03  FILLER          PIC 999     VALUE 519.
               03  FILLER          PIC X(70)   VALUE
                   '519 - NEUES GLEICH ALTES GRUPPENPROFIL !
      -    '               '.
           02  FEHLER-999.
               03  FILLER          PIC 999     VALUE 999.
               03  FILLER          PIC X(70)   VALUE
                   '999'.
       01  FILLER REDEFINES FEHLERTABELLE.
150520*    02  FILLER              OCCURS 77.
170620*    02  FILLER              OCCURS 79.
170620     02  FILLER              OCCURS 86.
               03 FEHLNR           PIC 999.
               03 FEHLER           PIC X(70).






