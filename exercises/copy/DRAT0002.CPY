      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0002.CPv  $
      *
      *   Rev 2.1   Apr 23 2003 16:04:20   t13ex22
      *ASL00156: Neuer Dialog SL01
      *
      *   Rev 2.0   Apr 29 1999 15:04:16   T13AE35
      *ACV00002 - neuer TRAN-SYSCODE CV
      *
      *   Rev 1.4   23 Dec 1998 13:14:44   T13AE34
      *APA00035 PROGRAMMVERWALTUNG
      *
      *   Rev 1.3   25 Jul 1997 09:46:38   T13AE34
      *afan0146 - Driver-Anpassung Alt-Neudialoge
      *
      *   Rev 1.2   Mar 20 1997 16:59:54   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.1   Jun 24 1996 12:54:46   T13EX16
      *AFPA0001 Kürzel 'PA' für Daisy eingetragen
      *
      *   Rev 1.0   Nov 04 1995 08:33:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: SYSTEMCODE-KURZFORMEN (DRAT0002)              *
      *    --------------------------------------------------------   *
      *    VERSION   2    02.05.95 16:34:49
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  *
      *    DATUM:    29.10.1992                                       *
      ****************************************************************
      *    VERSION   1    25.11.94 16:13:49
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !29.10.92! 01 ! NEU                       ! SCHOELLH.*
      * AFOP0030!05.01.95!    ! ERWEITERUNG TABELLE       ! BOHLINGER*
      *         !10.01.95! 02 ! PROD.ÜBERGABE             ! GIESE    *
      * AFVB0125!24.04.95!    ! ERWEITERUNG TABELLE       ! SCHÖLLH. *
      *         !28.04.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFPA0001!24.06.96! 04 ! Daisy-TRAN 'PA' eingefügt ! Auerbach *
      * AFPA0002!20.01.97! 05 ! KURZFORMEN JETZT NUMERISCH! SCHÖLLH. *
      * APA00035!30.05.97! 06 ! NEUE TRAN-SYSCODE 'PV'    ! BOHLINGER*
      * ACV00002!09.02.99! 07 ! NEUE TRAN-SYSCODE 'CV'    !          *
      *         !        !    ! FUER CTV-DIALOG AF01      ! HERING   *
      * ASL     !24.03.03! 08 ! NEUER TRAN-SYSCODE 'SL'   ! MUND     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    ZU JEDEM SYSTEMCODE IST HIER EINE EINDEUTIGE EINSTELLIGE
      *    KURZFORM DEFINIERT
      *
      *    ACHTUNG! BEI ERGAENZUNGEN ANPASSUNGEN NOTWENDIG:
      *             - OCCURS-WERT
      *             - I-SYSCODE-MAX  (IDENTISCH MIT OCCURS-WERT!)
      *
       01  T-SYSTEMCODE-TAB.
      *        INHALT JEWEILS:
      *            SYSTEMCODE - NORMALFORM (LANG)
      *            SYSTEMCODE - KURZFORM
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'AT'.
200197*         10  FILLER                PIC  X(001) VALUE 'A'.
200197         10  FILLER                PIC  9(002) VALUE 1.
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'VB'.
200197*         10  FILLER                PIC  X(001) VALUE 'V'.
200197         10  FILLER                PIC  9(002) VALUE 2.
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'OP'.
200197*         10  FILLER                PIC  X(001) VALUE 'O'.
200197         10  FILLER                PIC  9(002) VALUE 3.
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'AN'.
200197*         10  FILLER                PIC  X(001) VALUE 'N'.
200197         10  FILLER                PIC  9(002) VALUE 4.
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'PA'.
200197*         10  FILLER                PIC  X(001) VALUE 'P'.
200197         10  FILLER                PIC  9(002) VALUE 5.
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'UU'.
               10  FILLER                PIC  9(002) VALUE 6.
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'PV'.
               10  FILLER                PIC  9(002) VALUE 7.
           05  FILLER.
               10  FILLER                PIC  X(002) VALUE 'CV'.
               10  FILLER                PIC  9(002) VALUE 8.
240303     05  FILLER.
240303         10  FILLER                PIC  X(002) VALUE 'SL'.
240303         10  FILLER                PIC  9(002) VALUE 9.

       01  T-SYSTEMCODE-TAB-RED REDEFINES T-SYSTEMCODE-TAB.
240303*    05  T-SYSCODE-ZEILE OCCURS  8
           05  T-SYSCODE-ZEILE OCCURS  9
                      INDEXED BY I-SYSCODE.
               10  T-SYSCODE-LANG        PIC  X(002).
200197*         10  T-SYSCODE-KURZ        PIC  X(001).
200197         10  T-SYSCODE-KURZ        PIC  9(002).

240303*01  I-SYSCODE-MAX                 PIC S9(004) COMP VALUE 8.
240303 01  I-SYSCODE-MAX                 PIC S9(004) COMP VALUE 9.

      *
      ******** ENDE COPY-MEMBER SYSTEMCODE-KURZFORMEN (DRAT0002) ******
