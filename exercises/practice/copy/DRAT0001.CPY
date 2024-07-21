      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0001.CPY  $
      *    $Revision:   2.16  $
      *
      *    $Log:   L:/mf/CPY/DRAT0001.CPv  $
      *
      *   Rev 2.16   Dec 09 2011 10:14:14   t13ae34
      *AVB01200: Erweiterung Dialog-Schlüsselhilfe <F2>
      *
      *   Rev 2.15   Aug 30 2007 09:20:04   t13ae34
      *AAN00753: Funktionen und Dialoge: Erweiterung der PIN auf 9 
      *Stellen
      *
      *   Rev 2.14   22 Aug 2007 13:25:04   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.13   Feb 21 2007 12:28:00   t13ae34
      *ACV00054: Change Request CTV-Service-Funktion
      *
      *   Rev 2.12   Feb 15 2007 10:04:14   t13ae34
      *ACV00054: Anpassung Driver-Rahmen -> über AAN00655
      *
      *   Rev 2.11   Oct 06 2006 10:09:36   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.10   Feb 20 2006 11:55:44   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.9   Jan 27 2006 11:37:38   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.8   Feb 27 2004 12:04:34   t13ae34
      *ACV00054 - neue CTV-Servicefunktion
      *
      *   Rev 2.7   20 Jun 2001 10:59:58   t13ae34
      *ACV00015-CTV-Druck; hier: CTV-Meldungen
      *
      *   Rev 2.6   10 May 2001 13:06:42   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.5   23 May 2000 13:23:56   T13AE18
      *aat00016
      *
      *
      *   Rev 2.4   Feb 01 2000 08:36:00   T13EX21
      *APA00081 - Erweiterung um STAT-PGEB-GES
      *
      *   Rev 2.3   Jan 26 2000 11:06:08   T13AE34
      *AAT00016-flexible Verzweigung PG17
      *
      *   Rev 2.2   Apr 12 1999 12:02:08   T13AE34
      *AAT00005-Euro-Switch (hier: PVCS-Kopf)
      *
      *   Rev 2.0   MAR 01 1999 14:44:16   T13AE34
      *NT-UMSTELLUNG
      *
      *   Rev 1.8   13 NOV 1998 16:53:14   T13EX26
      *PEG-PRODUKTIONSÜBERGABE
      *
      *   Rev 1.7   27 AUG 1997 17:52:42   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.6   25 JUL 1997 09:45:50   T13AE34
      *AFAN0146 - DRIVER-ANPASSUNG ALT-NEUDIALOGE
      *
      *   Rev 1.5   10 JUL 1997 12:34:28   T13AE34
      *AFPA0021 - S. LETZTER PUT
      *
      *   Rev 1.4   08 JUL 1997 08:49:46   T13AE34
      *AFPA0021 - CICS-NAME / VERZWEIGUNG VORSCHALT-PGM
      *
      *   Rev 1.3   MAR 20 1997 16:56:34   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.2   MAR 26 1996 16:53:54   T13EX11
      *AFVB0226  NEUER AUFRUF-MODUS:
      *          DIALOG-DWL-AUFRUF
      *
      *   Rev 1.1   NOV 15 1995 16:21:16   T13EX11
      *AFVB0226 NEU: CADI-MELDUNG-VAR-FELD
      *
      *   Rev 1.0   NOV 04 1995 08:33:24   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *    COPY-MEMBER: COMMAREA (DRAT0001)                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  *
      *    DATUM:    23.10.1992                                       *
      *****************************************************************
      *    VERSION   3    17.11.95 14:22:05
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.04.93! 01 ! NEU                       ! SCHÖLLH. *
      *         !17.05.93! 02 ! TEMP. AEND: REDEF. AUF    ! SCHÖLLH. *
      *         !        !    !    CADI-SICHERUNG         !          *
      *         !15.09.93!    ! RÜCKNAME TEMP ÄND (170593)! SCHÖLLH. *
      *         !        !    ! VERLÄNGERUNG UM 1K AUF 5K !          *
      *         !        !    ! VERGRÖSSERN DER FILLER    !          *
      *         !15.10.93!    ! ABSCHL-ANZEIGE-UNVERAEND. ! SCHÖLLH. *
      *         !        !    !  JETZT ALS UNABH. FELD    !          *
      *         !15.10.93! 03 ! PROD.ÜBERGABE             ! HECKER   *
      * FEVB0218!03.03.95!    ! INFO-DISPLAY-SCHALTER NEU ! SCHÖLLH. *
      *         !10.03.95! 04 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0182!15.03.95!    ! NEU: CARD-CTV-BRIEF-MENUE,! SCHÖLLH. *
      *         !        !    !  CARD-S-CTV               !          *
      *         !28.04.95! 05 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0226!27.09.95!    ! NEU: CADI-MELDUNG-VAR-FELD! SCHÖLLH. *
      *         !07.11.95! 06 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0226!02.02.96!    ! EINBAU 'DIALOG-DWL-AUFRUF'! SCHÖLLH. *
      * AFPA0002!14.08.96! 07 ! INT. U. EXT. VERTRAGSNR.  ! AUERBACH *
      *         !        !    ! GESELLSCHAFT IN CADU      !          *
      *         !        !    ! OB-VB-NR-UNTERB GELÖSCHT  !          *
      * AFPA0002!16.12.96! 08 ! KOMPETENZ-KZ UND BENUTZER-! AUERBACH *
      *         !        !    ! PROFILE IN CARD ABGELEGT; !          *
      *    "    !20.01.97!    ! CARD-TSQUS-NAME NEU,      ! SCHÖLLH. *
      *         !        !    ! CARU-TSQ-INFOS-TAB NEU,   !          *
      *         !        !    ! CARD-DIALOG-STATUS VON    !          *
      *         !        !    !  CADD NACH CARD VERLEGT,  !          *
      *         !        !    ! CARU-STANDARD-STATUS-     !          *
      *         !        !    !  BEREICH NEU              !          *
      *    "    !03.02.97!    ! CADI-MELDUNG-VSTAT NEU    ! AUERBACH *
      * AFPA0021!02.07.97! 09 ! CARU-CICS-NAME            ! HEIDER   *
      *         !        !    ! CARU-VORSCHALT-PGM        ! HEIDER   *
      *         !        !    ! -> V = VERZWEIGUNG        ! HEIDER   *
      *         !        !    !                           !          *
      * AFAN0146!22.07.97! 10 ! CARD-ALT-NEU              ! HEIDER   *
      * AFPA0022!21.08.97! 10 ! CADI-MELDUNG-KTY    NEU   ! OPRITZ   *
      *         !        !    ! CADI-MELDUNG-KNR     ""   !          *
      * AFPA0022!27.07.98! 11 ! STAT-PTR-DFHEIB UND       ! AUERBACH *
      *         !        !    ! STAT-PTR-DFHCOMMAREA HIN- !          *
      *         !        !    ! ZUGEFüGT                  !          *
      * AAT00005!17.02.99!1.9 ! EURO-SWITCH IN NEUDIALOGEN! HEIDER   *
      * AAT00016!19.01.00!2.3 ! FLEXIBLE VERZWEIGUNG PG17 ! HEIDER   *
      * APA00080!26.01.00!2.4 ! STANDARD-STATUS-BEREICH   ! STORCH   *
      *         !        !    ! ERWEITERT                 !          *
      * AAT00016!23.05.00!2.4 ! FLEXIBLE VERZWEIGUNG PG17 ! BOHLINGER*
      *         !        !    ! KTY-/KNR FÜR FLEX. VERZW. !          *
      *         !        !    ! IN CARU-BEREICH           !          *
      * ACV00015!08.05.01!2.6 ! CTV-BRIEFDRUCK AUS NEUDIAL! HEIDER   *
      * ACV00015!20.06.01!2.7 ! CARD-CTV-MELDUNG          ! HEIDER   *
      * ACV00054!27.02.04!2.8 ! NEUE CTV-SERVICE-FUNKTION ! HEIDER   *
      * ACV00054!31.01.06!2.10! CADD-CTV-MELDUNG-NR       ! HEIDER   *
      * ACV00054!06.03.06!2.11! CTV-MELDUNG 8-STELLIG     ! HEIDER   *
      * APA00639!26.06.07!2.14! ERW. DER PIN AUF 9 STELLEN! WANG     *
      * AVB01200!28.11.11!2.16! CADD-SCHLUEHILFE-BEZ-KURZ ! HEIDER   *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    ENTHAELT: - K-CA-LEN   (FUER NICHT-LINK-PROGRAMME)
      *              - C-COMMAREA (FUER NICHT-LINK-PROGRAMME)
      *
      *    ACHTUNG: - BEI AENDERUNG DER LAENGE DER CA MUSS
      *    !!!        - DIE C-COMMAREA ERWEITERT
      *               - DER WERT IN K-CA-LEN ANGEPASST
      *               - DIE LAENGE DER DFHCOMMAREA (DRAT0022)
      *                 ANGEPASST
      *               - DER WERT IN K-CA-LEN ANGEPASST
      *               - ALLE PGMS, DIE MIT DER CA ARBEITEN, NEU
      *                 ÜBERSETZT, GELINKT UND GEBUNDEN WERDEN
      *
      *             - DIE GESAMTLÄNGE DER BEREICHE DARF NICHT VERÄNDERT
      *               WERDEN, OHNE ZUGLEICH ALLE PROGRAMME ZU ÜBER-
      *               SETZEN, DIE MIT DER C-COMMAREA ARBEITEN
      *
      *
      *  UEBERBLICK UEBER DIE CA-BEREICHE:
      *  ---------------------------------
      *
      *    BEREICH 1: RAHMEN/DRIVER - DATEN UNABHÄNGIG VON LOG. SITZUNG
      *     CARU      (ZUGRIFF DURCH RAHMEN UND DRIVER)
      *
      *    BEREICH 2: DRIVER-INTERN ODER RAHMEN-DIALOGUEBERGREIFEND
      *     CARR      (ZUGRIFF NUR DURCH DRIVER ODER RAHMEN)
      *
      *    BEREICH 3: RAHMENFELDER - DIALOGBEZOGEN
      *     CARD      (ZUGRIFF NUR DURCH RAHMEN IM AKTUELLER DIALOG)
      *
      *    BEREICH 4: DRIVER-AUFRUF - EINGABE- UND AUSGABE-FELDER
      *     CADR      (ZUGRIFF DURCH RAHMEN, FACHFKT. UND DRIVER)
      *
      *    BEREICH 5: FACHDATEN - UNABHÄNGIG VON LOG. SITZUNG
      *     CADU      (ZUGRIFF DURCH FACHFKT. VERSCHIEDENER DIALOGE)
      *
      *    BEREICH 6: KOMMUNIKATIONSBEREICH ZWISCHEN DIALOGEN
      *     CADD      (ZUGRIFF DURCH FACHFKT. VERSCHIEDENER DIALOGE)
      *
      *    BEREICH 7: DIALOG-INTERN - FACHFUNKTIONEN-FELDER
      *     CADI      (ZUGRIFF (AUCH) DURCH FACHFKT. IM AKT. DIALOG)
      *
      *    BEREICH 8: SICHERUNG FUER FACHSPEZIFISCHE INHALTE
      *     CADI
      *
      *  ZUGRIFFSBERECHTIGUNG; SICHERUNG BEI DIALOGUNTERBRECHUNG:
      *
      *          !     ZUGRIFF     !SICHERUNG UND INIT BEI! INIT BEI!
      *          !-----------------!----------------------!----------!
      *          ! RAHMEN !FACHFKT ! WECHSEL  ! F23 / F24 ! WECHSEL  !
      *          !        !        !  UANW    !           ! DIALOG   !
      *    ------+-----------------+----------+-----------+----------!
      *    CARU  !    X   !        !          !           !          !
      *    CARR  !    X   !        !          !     X     !          !
      *    CARD  !    X   !        !     X    !     X     !    X     !
      *    CADR  !    X   !   X    !          !           !          !
      *    CADU  !   (X)  !   X    !          !           !          !
      *    CADD  !   (X)  !   X    !          !     X     !          !
      *    CADI  !   (X)  !   X    !     X    !     X     !    X     !
      *    ----------------------------------------------------------!


      *   LÄNGE DER COMMAREA FUER NICHT-LINK-PROGRAMME
       01  K-CA-LEN                  PIC S9(004) COMP VALUE 5120.


       01  C-COMMAREA.
      *
      *    BEREICH 1: RAHMEN/DRIVER - DATEN UNABHÄNGIG VON LOG. SITZUNG
      *               (ZUGRIFF DURCH RAHMEN UND DRIVER)
           05  CARU-BEREICH.
      *           LETZTE/AKTUELLE MAP
               10  CARU-MAP-LAST            PIC  X(007).
      *           SCHALTER FUER SYSTEM-BEZEICHNUNG (TEST ODER PROD)
               10  CARU-S-SYSTEM            PIC  9(001).
                   88  SYSTEM-PROD                    VALUE 1.
                   88  SYSTEM-TEST                    VALUE 2.

      *           TRACE-DISPLAY-SCHALTER JA/NEIN
      *           (TRACE-AUSGABE AB ZEILE 25 IM LAUFENDEN BETRIEB)
               10  CARU-S-TRACE-DISPLAY     PIC  9(001).
                   88  TRACE-DISPLAY-NEIN             VALUE 0.
                   88  TRACE-DISPLAY-JA               VALUE 1.
      *           TRACE-LAENGE-SCHALTER: MIT/OHNE RAHMEN
               10  CARU-S-TRACELEN          PIC  9(001).
                   88  TRACELEN-LANG                  VALUE 0.
                   88  TRACELEN-KURZ                  VALUE 1.

      *           SICHERUNG SECTION-TRACE
               10  CARU-TRACE               PIC  X(230).

      *           NAME FUER TS-QUEUE (LINK/ERR-UEBERGABE)
               10  CARU-TSQLKE-NAME         PIC  X(008).

030395*           INFO-DISPLAY-SCHALTER JA/NEIN
030395*           (AUSGABE VON INFOS IN ZEILE 21 (STRICH-ZEISE IM FUSS))
030395         10  CARU-S-INFO-DISPLAY      PIC  9(001).
030395             88  INFO-DISPLAY-NEIN              VALUE 0.
030395             88  INFO-DISPLAY-JA                VALUE 1.

200197*           TABELLE ZUM SPEICHERN DER VERWENDETEN TS-QUEUES FÜR
200197*           DIE CA PLUS DER UNVERSCHLÜSSELTEN INFORMATIONEN
200197*           ZU USITZG, DIALOG-FOLGE, UANW, PAGE
200197         10  CARU-TSQ-INFOS-TAB.
200197             15  FILLER OCCURS 40.
200197                 20 CARU-TSQI-TSQNAME-REST        PIC  X(004).
200197                 20 CARU-TSQI-USITZG-ANZ          PIC  9(001).
200197*                * IN CARU-TSQI-DIALOG-FOLGE-NR-BIN STEHT DAS
200197*                * RECHTE BYTE EINES BINARY-FELDES
200197                 20 CARU-TSQI-DIALOG-FOLGE-NR-BIN PIC  X(001).
200197                 20 CARU-TSQI-UANW                PIC  9(001).
200197                 20 CARU-TSQI-PAGE                PIC  9(001).
200197*           INDEX DER ZULETZT GESICHERTEN TSQ-INFOS
200197         10  CARU-I-TSQI              PIC S9(004) BINARY.
200197*           INDEX DER JETZT ZU ENTSICHERNDEN TSQ-INFOS
200197         10  CARU-I-TSQI-E            PIC S9(004) BINARY.
      *
200197***************************************************************
200197*     STANDARD-STATUS-BEREICH:
200197*     - STANDARDÜBERGABEBEREICH AN UNTERPROGRAMME
200197*     - ERSTMALS IM DAISY PROJEKT VERWENDET
200197*     - STRUKTUR UND FELDBEZEICHNUNGEN (AUSSER 10ER-STUFE)
200197*       IDENTISCH MIT COPY DRAT0109
200197***************************************************************
200197         10  CARU-STANDARD-STATUS-BEREICH.
200197             15  STANDARD-STATUS-BEREICH.
200197                 20  STAT-TERMINAL-ID     PIC X(004).
200197*                    * KENNUNG: DIALOG=USERID; BATCH=HPRO-NAME
200197                 20  STAT-KENNUNG         PIC X(008).
200197                 20  STAT-DIALOG-ID       PIC X(004).
200197                 20  STAT-S-SYSTEM        PIC 9(001).
200197                     88 STAT-SYSTEM-PROD            VALUE 1.
200197                     88 STAT-SYSTEM-TEST            VALUE 2.
200197                 20  STAT-S-SESSION       PIC 9(001).
200197                     88 STAT-DIALOG                 VALUE 1.
200197                     88 STAT-BATCH                  VALUE 2.
200197                 20  STAT-S-TRCE          PIC 9(001).
200197                     88 STAT-TRCE-EIN               VALUE 1.
200197                     88 STAT-TRCE-AUS               VALUE 2.
200197                 20  STAT-USITZG-ANZ      PIC 9(001).

270798***              POINTER AUF DFHEIBLK UND DFHCOMMAREA FÜR DEN
270798***              AUFRUF VON CICS-UPRO UNTERHALB VON COBOL-UPRO
270798                 20  STAT-PTR-DFHEIBLK         POINTER.
270798                 20  STAT-PTR-DFHCOMMAREA      POINTER.
260100                 20  STAT-PGEB-GES        PIC 9(004) USAGE COMP.
      *
      *ACHTUNG! NEUE FELDER BITTE VON UNTEN NACH OBEN
220500         10  FILLER                   PIC  X(028).
220500*        10  FILLER                   PIC  X(032).
270798*        10  FILLER                   PIC  X(034).
020797*        10  FILLER                   PIC  X(042).

220500*           KTY FÜR FLEXIBLE VERZWEIGUNG (S. KONZEPT KPAT0107)
220500         10  CARU-KTY-DIA-WECHSEL     PIC S9(004) COMP.
220500*           KNR ENTSPRICHT DER PRODUKTGEBERGESELLCHAFT
220500         10  CARU-KNR-DIA-WECHSEL     PIC S9(004) COMP.

020797*CICS-NAME ZUM ANZEIGEN IM RAHMEN
020797         10  CARU-CICS-NAME           PIC  X(004).
020797*KENNZEICHEN, OB ZU VORSCHALT-PGM VERZWEIGT WERDEN SOLL
020797         10  CARU-VORSCHALT-PGM       PIC  9(001).
020797             88 VORSCHALT-PGM-JA            VALUE 1.
020797             88 VORSCHALT-PGM-NEIN          VALUE ZERO.
020797*        10  FILLER                   PIC  X(047).
200197*         10  FILLER                   PIC  X(391).
030395*         10  FILLER                   PIC  X(392).
      *    LAENGE BEREICH 1: 640 BYTES
      *    -----------------------------------------------------------
      *
      *    BEREICH 2: DRIVER-INTERN ODER RAHMEN-DIALOGUEBERGREIFEND
      *               (ZUGRIFF NUR DURCH DRIVER ODER RAHMEN)
           05  CARR-BEREICH.
      *           ANZAHL UNTERBROCHENER DIALOGSITZUNGEN
               10  CARR-USITZG-ANZ          PIC  9(001).
      *           ANZAHL UNTERBRECHUNGEN DER ANWENDUNG
      *           (ENTSPR. ANZAHL GEKELLERTE ANWENDUNGEN INNERHALB
      *            EINER LOGISCHEN SITZUNG)
               10  CARR-UANW                PIC  9(001).
200197*           ANZAHL VERKETTETE DIALOGE (1. DIALOG = '0')
200197         10  CARR-DIALOG-FOLGE-NR         PIC S9(004) BINARY.
200197         10  FILLER REDEFINES CARR-DIALOG-FOLGE-NR.
200197             15  FILLER                   PIC  X(001).
200197             15  CARR-DIALOG-FOLGE-NR-BIN PIC  X(001).
      *
200197**                    GESICHERTER STAND VON C-DRI-UANW DER LETZTEN
200197**                    LOGISCHEN SITZUNG (NACH F23)
200197**                 10  CARR-UANW-SAVE           PIC  9(001).
200197**                    DRIVER-STATUS (DRIVER-INTERN)
200197*                  10  CARR-STATUS              PIC  9(001).
      *
      *           PROGRAMMNAME DES DRIVERS (VOM DRIVER GEFUELLT)
               10  CARR-DRIVER-PGM          PIC  X(008).
      *           TAC DES AKTUELLEN (ODER LETZTEN) DIALOGS
               10  CARR-DIALOG-AKT          PIC  X(004).

200197*           ANZAHL DIALOG-SEITEN MIT JEWEILS EIGENSTÄNDIGEM PGM
200197*           (1. SEITE = '1')
200197         10  CARR-PAGE                PIC  9(001).

200197         10  FILLER                   PIC  X(047).
200197*         10  FILLER                   PIC  X(048).
      *    LAENGE BEREICH 2: 64 BYTES
      *    -----------------------------------------------------------
      *
      *    BEREICH 3: DIALOG-INTERN - RAHMENFELDER
      *               (ZUGRIFF NUR DURCH RAHMEN IM AKTUELLEN DIALOG)
           05  CARD-BEREICH.
      *           OPERATIONSCODE-TABELLE (FUER DIALOG-OPERATIONEN)
      *           ACHTUNG: OCCURS UND K-DI-OP-MAX MUESSEN GLEICH SEIN
               10  CARD-OP-TAB.
                   15  CARD-OP-ZEILE OCCURS 10
                                     INDEXED BY I-OP-FUELL.
                       20  CARD-OP          PIC  X(003).

      *           INDEX DER AKTUELL IN BEARB. BEFINDLICHEN OPERATION
               10  CARD-OP-IND              PIC S9(004) COMP.
      *           MAX. INDEX-WERT ZUR OP-TABELLE
               10  CARD-OP-MAX              PIC S9(004) COMP.

      *           TAC DES AKTUELLEN (ODER LETZTEN) DIALOG-SCHRITTS
               10  CARD-DIALOG-SCHRITT-AKT  PIC  X(004).

      *           DIALOGSCHRITT-STATUS (FUER STEUERUNG DIALOGPROG.)
               10  CARD-DS-STATUS           PIC  9(001).
                   88  DS-GUELTIG                     VALUE 1 THRU 8.
                   88  DS-BILDEINGABE                 VALUE 1.
                   88  DS-FTASTE-BEHANDELN            VALUE 2.
                   88  DS-DIALOG-INIT                 VALUE 3.
                   88  DS-BEARBEITUNG                 VALUE 4.
                   88  DS-FUSS-VERSORGEN              VALUE 5.
                   88  DS-BILDAUSGABE                 VALUE 6.
                   88  DS-RETURN                      VALUE 7.
                   88  DS-VERZWEIGEN                  VALUE 8.

      *           GESICHERTER DIALOGZUSTAND
               10  CARD-DIALOG-ZUSTAND.
                   15  CARD-BILDNR          PIC  9(002).
                   15  CARD-BILD-ZUSTAND    PIC  9(001).
      *           NAME DES LETZTEN/AKTUELLEN PROGRAMMS
               10  CARD-PROG-AKT            PIC  X(008).

      *           ABSOLUTE CURSOR-POSITION
               10  CARD-CURABS              PIC S9(004) COMP.
      *           CURSOR AUF FELD IM FUSS POSITIONIEREN ODER CURABS?
      *                DEFAULT: KEIN CURSOR BEI SEND MAP-FUSS
               10  CARD-CURSOR-FUSS         PIC  9(001).
                   88  CURSOR-FUSS-NEIN                  VALUE ZERO.
                   88  CURSOR-FUSS-JA                    VALUE 1.
                   88  CURSOR-FUSS-CURABS                VALUE 2.

      *           NAME FUER TS-QUEUE (BILDER/MAPS)
               10  CARD-TSQMA-NAME          PIC  X(008).
      *           NAME FUER TS-QUEUE (BLAETTERN/TAB-SEITEN-START)
               10  CARD-TSQBL-NAME          PIC  X(008).
      *           NAME FUER TS-QUEUE (DIALOG-DIALOG-KOMMUNIKATION)
               10  CARD-TSQDD-NAME          PIC  X(008).

      *           LETZTES BILD (OHNE FUSS-VERSORG, MIT CURABS) AUSGEBEN?
               10  CARD-BILDLAST            PIC  9(001).
                   88  BILDLAST-NEIN                     VALUE ZERO.
                   88  BILDLAST-JA                       VALUE 1.
                   88  BILDLAST-NUR-BEI-FEHLER           VALUE 2.

      *           HINWEIS-/FEHLERZEILE - SCHLUESSEL (FUER ANZEIGE)
      *           - GEFUELLT VOM DRIVER / RAHMEN -
               10  CARD-MELDRAHM-KEY.
      *               DUMMY-PROGRAMMNAME FUER RAHMEN-MELDUNGEN
                   15  CARD-MELDRAHM-PGM    PIC  X(008).
      *               TYP DER MELDUNG
                   15  CARD-MELDRAHM-TYP    PIC  X(001).
      *               LFD-NR. DER MELDUNG
                   15  CARD-MELDRAHM-NR     PIC  9(004) COMP.

151093*           SCHALTER FÜR INHALT OK-FELD IM MASKENFUSS
151093         10  CARD-S-ABSCHL-WERT       PIC  9(001).
151093             88  ABSCHL-ANZEIGE-INI                VALUE ZERO.
151093             88  ABSCHL-ANZEIGE-UNVERAENDERT       VALUE 1.

150395*           SCHALTER FÜR ANZEIGE CTV-FELD UND CTV-FTASTE
150395         10  CARD-S-CTV               PIC  9(001).
150395             88  CTV-NICHT-ANZEIGE              VALUE 0.
150395             88  CTV-ANZEIGE                    VALUE 1.

150395*           CTV-BRIEFMENUE-SCHLÜSSEL ZUM DIALOG-SCHRITT
150395         10  CARD-CTV-BRIEFMENUE      PIC  X(016).

200601*>>>>>
080501*>>>>>
      *           HINWEIS-/FEHLERZEILE - SCHLUESSEL (FUER ANZEIGE)
      *           - GEFUELLT VON FREMDSYSTEM (Z.ZT.: CTV-SYSTEM)
      *        10  CARD-FREMD-MELDUNG-KEY.
      *               PROGRAMMNAME (FIX)
      *            15  CARD-FREMD-MELDUNG-PGM     PIC  X(008).
      *               TYP DER MELDUNG
      *            15  CARD-FREMD-MELDUNG-TYP     PIC  X(001).
      *               LFD-NR. DER MELDUNG
      *            15  CARD-FREMD-MELDUNG-NR      PIC  9(004) COMP.

080501*<<<<<

080501*        10  FILLER                          PIC  X(027).
200601*<<<<<
200601*        10  FILLER                          PIC  X(016).
200601         10  CARD-CTV-MELDUNG                PIC  X(007).

200601         10  FILLER                          PIC  X(020).

200197*           NAME FUER TS-QUEUE (SICHERUNG VON USER-DATEN)
200197         10  CARD-TSQUS-NAME          PIC  X(008).

200197* >> AUS CADD-BEREICH HIERHIN VERSCHOBEN (FALSCH IM CADD-BEREICH!)
200197*           DIALOG-STATUS (FUER STEUERUNG DIALOGPROG.)
200197*           - GESETZT: VON FACHFUNKTION BEI 'INI' ODER 'ABGESCHL.'
200197*           - AUSGEWERTET: VOM RAHMEN AUF OBERSTER(!) DIALOG-EBENE
200197*                          BZGL. AKTIVITÄTEN BEI
200197*                          - INI           (TRANS ...)
200197*                          - ABGESCHLOSSEN (TRANS, CTV, ...)
200197*           - INITIAL.: VOM RAHMEN BEI
200197*                       - 'LESEN MAP' AUF OBERSTER(!) DIALOG-EBENE
200197*                       VOM DRIVER BEI
200197*                       - BEI DIALOG-ERSTEINSTIEG AUF OBERSTER(!)
200197*                         DIALOG-EBENE
200197         10  CARD-DIALOG-STATUS       PIC  9(001).
200197             88  DIALOG-STATUS-INARBEIT         VALUE ZERO.
200197             88  DIALOG-STATUS-INI              VALUE 1.
200197             88  DIALOG-STATUS-ABGESCHLOSSEN    VALUE 2.

250797         10 CARD-ALT-NEU              PIC  X(001).
250797             88  CARD-ALTANWENDUNG              VALUE 'A'.
250797             88  CARD-NEUANWENDUNG              VALUE 'N'.

170299*ARBEITET DIALOG MIT EURO-SWITCH ?
170299         10  CARD-EURO-SWITCH                PIC  9(001).
170299             88 CARD-EURO-SWITCH-NEIN        VALUE ZERO.
170299             88 CARD-EURO-SWITCH-JA          VALUE 1 THRU 4.
170299                88 CARD-EURO-SWITCH-URSPRUNG    VALUE 1.
170299                88 CARD-EURO-SWITCH-UMRECHNEN   VALUE 2.
170299                88 CARD-EURO-SWITCH-UMGERECHNET VALUE 3.
170299                88 CARD-EURO-SWITCH-ZURUECK     VALUE 4.
170299*FEHLERHAFTE EINGABE BEI EURO-SWITCH ?
170299         10  CARD-EURO-SWITCH-EINGABE        PIC  9(001).
170299             88 CARD-EURO-SWITCH-OHNE-FEHLER VALUE ZERO.
170299             88 CARD-EURO-SWITCH-MIT-FEHLER  VALUE 1.
170299*STANDARDWAEHRUNG
170299         10  CARD-EURO-SWITCH-STD-WAE        PIC  9(001).
170299*ALTERNATIVWAEHRUNG
170299         10  CARD-EURO-SWITCH-ALT-WAE        PIC  9(001).

170299         10  FILLER                   PIC  X(012).
170299*        10  FILLER                   PIC  X(016).
200197*         10  FILLER                   PIC  X(026).
150395*         10  FILLER                   PIC  X(053).
150395*         10  FILLER                   PIC  X(070).
151093*         10  FILLER                   PIC  X(071).
      *    LAENGE BEREICH 3: 160 BYTES
      *    -----------------------------------------------------------
      *
      *    BEREICH 4: DRIVER-AUFRUF - EINGABE- UND AUSGABE-FELDER
      *               (ZUGRIFF DURCH RAHMEN, FACHFKT. UND DRIVER)
           05  CADR-BEREICH.
      *           DRIVER-EINGABE: DRIVER-OPERATION
               10  CADR-E-OP                PIC  X(003).
      *           TAC DER NÄCHSTEN (GEWUENSCHTEN) TRANSAKTION
               10  CADR-E-TRANS-NEXT        PIC  X(004).

      *           AUFRUF-MODUS DES DIALOGS
               10  CADR-A-AUFRUF-MODUS      PIC  9(001).
020296*             88  AUFRUF-MODUS-GUELTIG          VALUE 1 THRU 3.
020296             88  AUFRUF-MODUS-GUELTIG          VALUE 1 THRU 4.
                   88  KEIN-DRIVER-AUFRUF            VALUE 1.
                   88  DIALOG-ERSTAUFRUF             VALUE 2.
                   88  DIALOG-WIEDERAUFRUF           VALUE 3.
020296             88  DIALOG-DWL-AUFRUF             VALUE 4.
      *           DRIVER-RC BEI DIALOG-WIEDEREINSTIEG
               10  CADR-A-RC                PIC  9(001).
                   88  DR-A-RC-GUELTIG                VALUE 1, 2, 9.
                   88  DR-A-RC-OK                     VALUE 1.
                   88  DR-A-RC-ABBRUCH                VALUE 2.
                   88  DR-A-RC-NOK                    VALUE 9.

220500*           KTY FÜR FLEXIBLE VERZWEIGUNG (S. KONZEPT KPAT0107)
220500         10  CADR-KTY-DIA-WECHSEL     PIC S9(004) COMP.
220500*           KNR ENTSPRICHT DER PRODUKTGEBERGESELLCHAFT
220500         10  CADR-KNR-DIA-WECHSEL     PIC S9(004) COMP.
220500*        10  FILLER                   PIC  X(023).
220500         10  FILLER                   PIC  X(019).
      *    LAENGE BEREICH 4: 32 BYTES
      *    -----------------------------------------------------------
      *
      *    BEREICH 5: FACHDATEN - UNABHÄNGIG VON LOG. SITZUNG
      *               (ZUGRIFF DURCH FACHFKT VERSCHIEDENER DIALOGE)
           05  CADU-BEREICH.
      *           CICS-DATUM (EVTL. ABWEICHEND VON TAGESDATUM,
      *                       FORMAT: JJJJMMTT)
               10  CADU-CICS-DATUM-AN.
                   15  CADU-CICS-JAHR       PIC  9(004).
                   15  CADU-CICS-JAHR-AN REDEFINES CADU-CICS-JAHR.
                       20 CADU-CICS-JH      PIC  9(002).
                       20 CADU-CICS-JJ      PIC  9(002).
                   15  CADU-CICS-MONAT      PIC  9(002).
                   15  CADU-CICS-TAG        PIC  9(002).
               10  CADU-CICS-DATUM REDEFINES CADU-CICS-DATUM-AN
                                            PIC  9(008).
      *
      *           ORDNUNGSBEGRIFF AUS FUSS-ZEILE
               10  CADU-OB                  PIC  X(032).
      *
140896***         GELÖSCHT, DA NICHT VERWENDET
140896*           AKTUELLE VB-NR AUS UNTERBROCHENER SITZUNG
140896*        10  CADU-OB-VB-NR-UNTBR      PIC  9(007).
140896*        10  CADU-OB-VB-NR-P-UNTBR    PIC  X(001).

140896*           VNR (EXT.VERTRAGSNUMMER) UND PIN (INT.VERTRAGSNUMMER)
140896         10  CADU-OB-VNR              PIC  X(015) VALUE SPACE.
260607*        10  CADU-OB-PIN              PIC  9(007) VALUE ZERO.
260607         10  CADU-OB-PIN-X.
260607             15 CADU-OB-PIN           PIC  9(009) COMP-3
260607                                                  VALUE ZERO.
260607             15 FILLER                PIC  X(002).
140896         10  CADU-OB-PIN-P            PIC  9(002) VALUE ZERO.
140896         10  CADU-OB-GES              PIC  9(002) VALUE ZERO.

140896         10  FILLER                   PIC  X(062).
140896*        10  FILLER                   PIC  X(080).
      *    LAENGE BEREICH 5: 128 BYTES
      *    -----------------------------------------------------------
      *
      *    BEREICH 6: KOMMUNIKATIONSBEREICH ZWISCHEN DIALOGEN
      *               (ZUGRIFF DURCH FACHFKT. VERSCHIEDENER DIALOGE)
           05  CADD-BEREICH.

      *           VARIABLER KOMMUNIKATIONSBEREICH ZWISCHEN DIALOGEN
      *           - INPUT (VON AUFRUFENDEM DIALOG)
      *             ACHTUNG: AUF CADD-I-KOMM-VAR DARF NUR BEI
      *             !!!      DIALOG-INIT ZUGEGRIFFEN WERDEN; FALLS
      *             !!!      DIE INFOS SPAETER NOCH WICHTIG SIND,
      *             !!!      MUESSEN SIE IN CADI-SICHERUNG GESICHERT
      *             !!!      WERDEN.
      *             !!!      GRUND: DAS FELD WIRD EVTL. IN AUFGERUFENEN
      *             !!!      UNTERANWENDUNGEN UEBERSCHRIEBEN
               10  CADD-I-KOMM-VAR          PIC  X(128).
      *           - OUTPUT (VON AUFGERUFENEM DIALOG)
               10  CADD-O-KOMM-VAR          PIC  X(128).

      *           LETZTE VB-NR, SOWEIT AKTUELL
               10  CADD-OB-VB-NR            PIC  9(007).
270204         10  CADD-OB-VB-NR-X REDEFINES CADD-OB-VB-NR
270204                                      PIC  X(007).
      *           PRUEFZIFFER ZUR LETZTEN VB-NR
               10  CADD-OB-VB-NR-P          PIC  X(001).
      *           CTV-BRIEFNUMMER
               10  CADD-CTV                 PIC  X(007).
      *           EINDEUTIGE FELDNR PRO MASKE
               10  CADD-FELDNAME            PIC  X(007).

      *           SCHLUESSELHILFE-INPUT: KTY
               10  CADD-SCHLUEHILFE-KTY     PIC S9(004) COMP.
      *           SCHLUESSELHILFE-RETURN: KNR
               10  CADD-SCHLUEHILFE-KNR     PIC S9(004) COMP.
      *           SCHLUESSELHILFE-RETURN: RETURNCODE
               10  CADD-SCHLUEHILFE-RC      PIC  9(001).
                   88  SCHLUEHILFE-AUSWAHL            VALUE 1.
                   88  SCHLUEHILFE-AUSWAHL-NICHT      VALUE 2.

200197* >> ZUM CARD-BEREICH VERSCHOBEN (WAR FALSCH IM CADD-BEREICH!)
200197*           DIALOG-STATUS (FUER STEUERUNG DIALOGPROG.)
200197*           - GESETZT: VON FACHFUNKTION BEI 'INI' ODER 'ABGESCHL.'
200197*           - AUSGEWERTET: VOM RAHMEN AUF OBERSTER(!) DIALOG-EBENE
200197*                          BZGL. AKTIVITÄTEN BEI
200197*                          - INI           (TRANS ...)
200197*                          - ABGESCHLOSSEN (TRANS, CTV, ...)
200197*           - INITIAL.: VOM RAHMEN BEI
200197*                       - 'LESEN MAP' AUF OBERSTER(!) DIALOG-EBENE
200197*                       VOM DRIVER BEI
200197*                       - BEI DIALOG-ERSTEINSTIEG AUF OBERSTER(!)
200197*                         DIALOG-EBENE
200197*        10  CARD-DIALOG-STATUS       PIC  9(001).
200197*            88  DIALOG-STATUS-INARBEIT         VALUE ZERO.
200197*            88  DIALOG-STATUS-INI              VALUE 1.
200197*            88  DIALOG-STATUS-ABGESCHLOSSEN    VALUE 2.
200197* >> DEN FILLER HIER SO STEHEN LASSEN, BIS ALLE PG NEU KOMPILIERT
               10  FILLER                   PIC  9(001).

270204*           CTV-BRIEFNUMMER
270204         10  CADD-CTV-BRIEF           PIC  X(016).
270204*           CTV-DRUCKSCHALTER
270204         10  CADD-CTV-DRUCKSCHALTER   PIC  X(001).
270204         88  CTV-SOFORTDRUCK          VALUE 'J'.
270204         88  CTV-PANELPROMPT          VALUE 'P'.
270204         88  CTV-KEINDRUCK            VALUE 'N'.
270204*        10  FILLER                   PIC  X(356).
060306         10  CADD-CTV-MELDUNG         PIC  X(008).
281111*           SCHLUESSELHILFE-INPUT: BEZ-KURZ
281111         10  CADD-SCHLUEHILFE-BEZ-KURZ PIC X(004).
281111*        10  FILLER                   PIC  X(331).
281111         10  FILLER                   PIC  X(327).
      *    LAENGE BEREICH 6: 640 BYTES
      *    -----------------------------------------------------------
      *
      *    BEREICH 7: DIALOG-INTERN - FACHFUNKTIONEN-FELDER
      *               (ZUGRIFF (AUCH) DURCH FACHFKT. IM AKT. DIALOG)
           05  CADI-BEREICH.
      *           OPERATIONSCODE DER AKTUELLEN OPERATION
               10  CADI-OP-AKT              PIC  X(003).

      *           SCHALTER FÜR ANZEIGE OK-FELD IM MASKENFUSS
               10  CADI-S-ABSCHLUSS         PIC  9(001).
                   88  ABSCHL-GUELTIG                 VALUE 0 THRU 3.
                   88  ABSCHL-NICHT-ANZEIGE           VALUE 0.
                   88  ABSCHL-ANZEIGE-N               VALUE 1.
                   88  ABSCHL-ANZEIGE-J               VALUE 2.
151093*             88  ABSCHL-ANZEIGE-UNVERAENDERT    VALUE 3.

      *           HINWEIS-/FEHLERZEILE - SCHLUESSEL (FUER ANZEIGE)
      *           - GEFUELLT VON FACHFUNKTION -
               10  CADI-MELDUNG-KEY.
      *               PROGRAMMNAME (FIX)
                   15  CADI-MELDUNG-PGM     PIC  X(008).
      *               TYP DER MELDUNG
                   15  CADI-MELDUNG-TYP     PIC  X(001).
      *               LFD-NR. DER MELDUNG
                   15  CADI-MELDUNG-NR      PIC  9(004) COMP.

      *           FTASTEN-ZEILE - SCHLUESSEL (FUER ANZEIGE)
               10  CADI-FTASTEN-KEY.
      *               PROGRAMMNAME (FIX)
                   15  CADI-FTASTEN-PGM     PIC  X(008).
      *               TYP DER ZEILE
                   15  CADI-FTASTEN-ZUSTAND PIC  X(002).
      *               LFD-NR. DER ZEILE
                   15  CADI-FTASTEN-NR      PIC  9(004) COMP.

      *           FELDNAME (AKTUELL)
               10  CADI-FELDNAME            PIC  X(007).

      *           TABELLE ZUR SPEICHERUNG VON H-MELDUNGEN
               10  CADI-HMELD-INFO-TAB.
                   15  CADI-HMELD-INFO-ZEILE OCCURS 30
                                             INDEXED BY I-HM.
                       20  CADI-HMELD-NR           PIC  9(004) BINARY.
                       20  CADI-HMELD-ANZUZEIG     PIC  X(001).
                       20  CADI-HMELD-ANGEZEIGT    PIC  X(001).

270995*           VARIABLE TEXTE IN HINWEIS- ODER FEHLERMELDUNGEN
270995         10  CADI-MELDUNG-VT-FELDER.
270995             15  CADI-MELDUNG-VT1     PIC  X(040).
270995             15  CADI-MELDUNG-VT2     PIC  X(020).
270995             15  CADI-MELDUNG-VT3     PIC  X(020).

030297*           WIRD FÜR MELDUNGSTYP = 'V' VERWENDET
030297         10  CADI-MELDUNG-VSTAT       PIC  X(004).

161296*            STRUKTURABHÄNGIGE KOMPETENZ VORHANDEN
161296         10  CADI-STRUKTURABH-KOMP           PIC  X(001).
161296*                * VOLL: '999' OHNE AUSNAHME
161296             88  CADI-STRUKTURABH-KOMP-VOLL     VALUE '1'.
161296*                * KEINE: WENN KEIN EINTRAG IN AT0104 ODER
161296*                *        '000' OHNE AUSNAHME
161296             88  CADI-STRUKTURABH-KOMP-KEINE    VALUE '2'.
161296*                * TEILW: '999' MIT AUSNAHME ODER
161296*                *        '000' MIT AUSNAHME
161296             88  CADI-STRUKTURABH-KOMP-TEILW    VALUE '3'.

161296*            'VARIABLER PRÜFBEREICH 2' AUS DEM K+S-SYSTEM
161296         10  CADI-VP2                        PIC  X(026).

161296         10  FILLER REDEFINES CADI-VP2.
161296*            BENUTZERPROFIL FÜR DIALOG
161296             15 CADI-PROFIL.
161296                20 CADI-PROFIL-FELD-SPERRE   PIC  X(001).
161296                20 CADI-PROFIL-FELD-ANZEIGE  PIC  X(001).
161296                20 CADI-PROFIL-F-TASTEN      PIC  X(001).
161296                20 CADI-PROFIL-FC-CODE       PIC  9(001).
161296                20 CADI-PROFIL-FC-SEITE      PIC  9(001).
161296                20 CADI-PROFIL-FACHLICH      PIC  9(001).

161296*            KOMPETENZ FÜR VSTATUS AUS DEM 'DAISY-SYSTEM'
161296             15 CADI-KOMPETENZ-VSTATUS       PIC  9(001).
161296             15 FILLER                       PIC  X(019).

210897         10  FILLER                          PIC  X(115).
210897         10  CADI-MELDUNG-KTY                PIC S9(004) comp.
210897         10  CADI-MELDUNG-KNR                PIC S9(004) comp.
161296*         10  FILLER                   PIC  X(119).
161296*         10  FILLER                   PIC  X(146).
030297*         10  FILLER                   PIC  X(150).
270995*         10  FILLER                   PIC  X(230).
      *    LAENGE BEREICH 7: 384 BYTES
      *    -----------------------------------------------------------
      *
      *    BEREICH 8: SICHERUNG FUER FACHSPEZIFISCHE INHALTE
           05  CADI-SICHERUNG               PIC  X(3072).
      *
      *    LAENGE BEREICH 8: 3072 BYTES
      *    -----------------------------------------------------------
      *    LAENGE BEREICHE 1-8: 5120 BYTES
      *    -----------------------------------------------------------
      *    ACHTUNG: BEI AENDERUNG DER LAENGE DER CA IST DIE LAENGE
      *    !!!      DER DFHCOMMAREA (DRAT0022) ENTSPR. MIT ANZUPASSEN
      *    -----------------------------------------------------------
      *
      ******** ENDE COPY-MEMBER COMMAREA (DRAT0001) *******************

