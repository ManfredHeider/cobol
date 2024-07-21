      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0020.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drat0020.cpv  $
      *
      *   Rev 2.2   Apr 12 1999 12:02:10   T13AE34
      *AAT00005-Euro-Switch (hier: PVCS-Kopf)
      *
      *   Rev 2.0   MAR 01 1999 14:44:40   T13AE34
      *NT-UMSTELLUNG
      *
      *   Rev 1.0   NOV 04 1995 08:37:28   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *    COPY-MEMBER: AUFRUFPARAMETER UPAT0003  (DRAT0020)          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  *
      *    DATUM:    17.02.1993                                       *
      *****************************************************************
      *    VERSION   1    02.05.95 16:35:44
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.04.93! 01 ! NEU                       ! SCHÖLLH. *
      *         !17.05.93! 02 ! PO-...-HINWEIS + 9 BYTE   ! SCHÖLLH. *
      *         !        !    ! NEU: PI-...-S-ABSCHLUSS   !          *
      * AFVB0182!15.03.95!    ! NEU: PI-UPAT0003-S-CTV    ! SCHÖLLH. *
      *         !28.04.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AAT00005!17.02.99!1.1 ! EURO-SWITCH               ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    AUFRUF-PARAMETER FUER UPRO UPAT0003 (AUSSER E-ERROR-BEREICH).
      *
      *    ABHAENGIG VON PI-UPAT0003-OP SIND FOLGENDE PARAMS ZU FUELLEN
      *       UND WERDEN FORGENDE WERTE ZURÜCKGEGEBEN (AUSSER ERROR):
      *
      *    OP                          !  M  !  H  !  F  !
      *    ----------------------------+-----+-----+-----+
      *    PI-UPAT0003-MELDUNG-KEY     !  X  !     !     !
      *    PI-UPAT0003-HELP-KEY        !     !  X  !     !
      *    PI-UPAT0003-FTASTEN-KEY     !     !     !  X  !
      *    PI-UPAT0003-FTASTEN-USITZG  !     !     !  X  !
      *    PI-UPAT0003-S-ABSCHLUSS     !     !     !  X  !
      *    ----------------------------+-----+-----+-----+
      *    PO-UPAT0003-HINWEIS         !  X  !  X  !     !
      *    PO-UPAT0003-FTASTEN         !     !     !  X  !
      *    ----------------------------+-----+-----+-----+
      *
       01  PI-UPAT0003-EINGABE.
           05  PI-UPAT0003-OP                  PIC  X(001).
               88  PI-UPAT0003-OP-MELDUNG                  VALUE 'M'.
               88  PI-UPAT0003-OP-HELP                     VALUE 'H'.
               88  PI-UPAT0003-OP-FTASTEN                  VALUE 'F'.

           05  PI-UPAT0003-MELDUNG-KEY.
               10  PI-UPAT0003-MELDUNG-PGM     PIC  X(008).
               10  PI-UPAT0003-MELDUNG-TYP     PIC  X(001).
               10  PI-UPAT0003-MELDUNG-NR      PIC S9(004) BINARY.
           05  PI-UPAT0003-HELP-KEY.
               10  PI-UPAT0003-HELP-MAP-ID     PIC  X(007).
               10  PI-UPAT0003-HELP-FELD-NAME  PIC  X(007).
           05  PI-UPAT0003-FTASTEN-KEY.
               10  PI-UPAT0003-FTASTEN-PGM     PIC  X(008).
               10  PI-UPAT0003-FTASTEN-ZUSTAND PIC  X(002).
               10  PI-UPAT0003-FTASTEN-NR      PIC S9(004) BINARY.
           05  PI-UPAT0003-FTASTEN-USITZG.
               10  PI-UPAT0003-USITZG-ANZ      PIC  9(001).
               10  PI-UPAT0003-USITZG-ANZ-MAX  PIC  9(001).
           05  PI-UPAT0003-S-ABSCHLUSS         PIC  9(001).
               88  S-ABSCHL-ANZEIGE-N                      VALUE 1.
               88  S-ABSCHL-ANZEIGE-J                      VALUE 2.
150395     05  PI-UPAT0003-S-CTV               PIC  9(001).
150395         88  AT03-S-CTV-NICHT-ANZEIGE                VALUE 0.
150395         88  AT03-S-CTV-ANZEIGE                      VALUE 1.
170299     05  PI-UPAT0003-S-EURO-SWITCH       PIC  9(001).
170299         88  S-EURO-SWITCH-JA                  VALUE 1 THRU 4.
170299         88  S-EURO-SWITCH-URSPRUNG            VALUE 1.
170299         88  S-EURO-SWITCH-UMRECHNEN           VALUE 2.
170299         88  S-EURO-SWITCH-UMGERECHNET         VALUE 3.
170299         88  S-EURO-SWITCH-ZURUECK             VALUE 4.
170299     05  PI-UPAT0003-EURO-SWITCH-WAE     PIC S9(004) BINARY.
170299         88 WAEHRUNG-DEFINIERT           VALUE 1 THRU 9999.
170299     05  FILLER                          PIC  X(026).
170299*    05  FILLER                          PIC  X(029).
150395*     05  FILLER                          PIC  X(030).

       01  PO-UPAT0003-AUSGABE.
           05  PO-UPAT0003-HINWEIS             PIC  X(079).
           05  PO-UPAT0003-FTASTEN             PIC  X(079).
      *
      ******** ENDE COPY-MEMBER AUFRUF UPAT0003    (DRAT0020) *********
