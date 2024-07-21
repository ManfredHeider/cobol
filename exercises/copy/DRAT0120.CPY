      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0120.CPY  $
      *    $Revision:   1.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0120.CPv  $
      *
      *   Rev 1.2   Sep 02 2019 09:24:42   t13ex67
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 1.1   Nov 27 2018 07:22:46   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.10   Mar 10 2014 08:50:52   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.9   Jan 13 2014 09:45:52   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.8   Jun 19 2012 09:21:48   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.7   May 21 2012 08:46:42   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.6   11 Dec 2007 08:55:24   t13ae46
      *APA00622: Einführung Allfinanz (JWA)
      *
      *   Rev 2.5   Aug 30 2007 09:21:48   t13ae34
      *AAN00753: Funktionen und Dialoge: Erweiterung der PIN auf 9
      *Stellen
      *
      *   Rev 2.4   23 Aug 2007 09:49:50   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.3   03 Nov 2006 09:55:34   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Jun 27 2003 09:25:28   t13ae46
      *FPA00601: PF14-PG17, PF15-PG17 hinzu.
      *
      *   Rev 2.1   Aug 05 1999 08:30:40   T13EX21
      *APA00046 - Korrektur PMPG-Version
      *
      *   Rev 2.0   Jun 19 1999 12:28:22   T13EX21
      *APA00012 - PVCS
      *
      *   Rev 1.3   11 Nov 1998 16:16:02   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.2   06 Aug 1998 12:40:34   T01JM2Y
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Apr 09 1997 16:46:10   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:19:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0120                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN DIALOGEN (CADD-I-KOMM-VAR, CADD-O-KOMM-VAR)       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    01.11.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!01.11.96! 1.0! NEU                       ! SCHÄFER  *
      *         !13.06.97! 1.1! SCHLÜSSELFELDER DTA       ! SCHRIEBER*
      *         !        !    ! HINZUGEFÜGT               !          *
      *         !24.07.97! 1.2! PGV HINZUGEFÜGT           ! D.WILHELM*
      *         !11.08.97! 1.3! PMPG-ID U. VERSION        !          *
      *         !        !    ! HINZUGEFÜGT               ! D.WILHELM*
      *         !02.09.97! 1.4! ÄNDERUNG GESELL IN GES    ! D.WILHELM*
      *         !31.10.97! 1.5! DEF. DTA-ID U. PMPG-ID    ! D.WILHELM*
      *         !        !    ! KORRIGIERT                !          *
      *         !02.04.98! 1.6! TSQDD-NAME                ! SCHRIEBER*
      *         !28.04.98! 1.7! ORDNR, FOLGENR HINZUGEFÜGT! STORCH   *
      *         !11.05.98! 1.8! PO25/PO24 HINZUGEFÜGT     ! FOLTZ    *
      *         !27.05.98! 1.9! PO11-PO25 HINZUGEFÜGT     ! BOHLINGER*
      *         !03.07.98! 2.0! PO11-PO14, PO14-PO11      ! D.WILHELM*
      *         !06.07.98!1.10! HISTNR HINZUGEFÜGT        ! BOHLINGER*
      *         !        !    ! PO25-PO24 PO24-PO25 AUSGESTERNT      *
      *         !10.07.98! 2.0! PO11-PO16, PO11-PO21      ! D.WILHELM*
      *         !01.09.98!2.01! PDPT-ID HINZUGEFÜGT       ! BOHLINGER*
      *         !10.09.98!2.02! PO24-PO25 WIEDER AKTIVIERT! BOHLINGER*
      *         !13.09.98!2.03! STEUER-INFO UM PF-DIALOGE !SCHRIEBER *
      *         !        !    ! ERWEITERT                 !          *
      *         !29.09.98!2.03! PO23-UU44 HINZUGEFÜGT     ! BOHLINGER*
      *         !22.10.98!2.03! PNAP-ID,PRINTE HINZUGEFÜGT! SCHRIEBER*
      *         !22.10.98!2.??! PROT-ID HINZUGEFÜGT       ! WOLFF    *
      *         !06.07.99!    !- PMPG-VERSION KORRIGIERT  ! STORCH   *
      *         !        !    !- AUS PLATZGRÜNDEN REDEFI- !          *
      *         !        !    !  NITION                   !          *
      * FPA00601!26.06.03!2.2 ! PF14-PG17, PF15-PG17 HINZU! WANG     *
      * APA00452!01.11.06!2.3 ! PS ERWEITERT              ! WANG     *
      * APA00639!21.06.07!2.4 ! ERW. DER PIN AUF 9 STELLEN! WANG     *
      * APA00622!24.05.07!2.6 ! VGES HINZU                ! WANG     *
      * APA01101!06.03.12!2.7 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-ZEILEN ENTFERNT         !          *
      * APA01526!08.05.13!2.8 ! ERWEITERUNG UM UNTERNUMMER! STORCH   *
      *         !        !    ! UND SCHICHTNUMMER         !          *
      *         !06.03.14!2.9 ! FOLGENR. DREISTELLIG      ! STORCH   *
      * APA02721!21.11.18!2.10! GEL-KENNUNG HINZU         ! STORCH   *
      * APA02796!12.07.19!2.11! NEUE 88-ER STUFE PO27     ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DEN VARIABLEN KOMMUNIKATIONSBEREICH
      *    ZWISCHEN DIALOGEN
      *    - INPUT (VON AUFRUFENDEM DIALOG)
      *      ACHTUNG: AUF CADD-I-KOMM-VAR DARF NUR BEI
      *      !!!      DIALOG-INIT ZUGEGRIFFEN WERDEN; FALLS
      *      !!!      DIE INFOS SPAETER NOCH WICHTIG SIND,
      *      !!!      MUESSEN SIE IN CADI-SICHERUNG GESICHERT
      *      !!!      WERDEN.
      *      !!!      GRUND: DAS FELD WIRD EVTL. IN AUFGERUFENEN
      *      !!!      UNTERANWENDUNGEN UEBERSCHRIEBEN
      *
      ****************************************************************
      *
       01  PI-DRAT0120.
           05  PI-DRAT0120-CADD-I-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0120-CADD-I-KOMM-VAR.
      *
               10  PI-DRAT0120-COPY-ID                PIC  X(008).
                   88 PI-DRAT0120-COPY-ID-DAISY       VALUE 'DRAT0120'.
      *
      ***      STEUERINFOS FÜR SONDERFÄLLE BEIM DIALOGAUFRUF
               10  PI-DRAT0120-CADD-STEUER-INFO       PIC  X(008).
      ***          AUFRUF PA12, DER SOFORT ZUM PA11 DURCHSTARTET
                   88 PI-DRAT0120-CADD-PA12-PA11      VALUE 'PA12PA11'.
                   88 PI-DRAT0120-CADD-PG11-PG12      VALUE 'PG11PG12'.
                   88 PI-DRAT0120-CADD-PG11-PG13      VALUE 'PG11PG13'.
                   88 PI-DRAT0120-CADD-PG11-PG18      VALUE 'PG11PG18'.
                   88 PI-DRAT0120-CADD-PG12-PG14      VALUE 'PG12PG14'.
                   88 PI-DRAT0120-CADD-PG12-PG18      VALUE 'PG12PG18'.
                   88 PI-DRAT0120-CADD-PG13-PG14      VALUE 'PG13PG14'.
                   88 PI-DRAT0120-CADD-PG13-PG18      VALUE 'PG13PG18'.
                   88 PI-DRAT0120-CADD-PG19-PG17      VALUE 'PG19PG17'.
                   88 PI-DRAT0120-CADD-PO24-PO25      VALUE 'PO24PO25'.
      ***          AUFRUF INNERHALB DER BESTANDSDIALOGE (PO11-PO25)
                   88 PI-DRAT0120-CADD-PO11-PO25      VALUE 'PO11PO25'.
                   88 PI-DRAT0120-CADD-PO11-PO14      VALUE 'PO11PO14'.
                   88 PI-DRAT0120-CADD-PO14-PO11      VALUE 'PO14PO11'.
                   88 PI-DRAT0120-CADD-PO11-PO16      VALUE 'PO11PO16'.
                   88 PI-DRAT0120-CADD-PO16-PO11      VALUE 'PO16PO11'.
                   88 PI-DRAT0120-CADD-PO16-PO21      VALUE 'PO16PO21'.
                   88 PI-DRAT0120-CADD-PO11-PO21      VALUE 'PO11PO21'.
                   88 PI-DRAT0120-CADD-PO21-PO11      VALUE 'PO21PO11'.
                   88 PI-DRAT0120-CADD-PO23-UU44      VALUE 'PO23UU44'.
120719             88 PI-DRAT0120-CADD-PO23-PO27      VALUE 'PO23PO27'.
      ***          AUFRUF INNERHALB DER PF-DIALOGE      (PF11-PF15)
                   88 PI-DRAT0120-CADD-PF11-PF12      VALUE 'PF11PF12'.
                   88 PI-DRAT0120-CADD-PF12-PF13      VALUE 'PF12PF13'.
                   88 PI-DRAT0120-CADD-PF12-PF14      VALUE 'PF12PF14'.
                   88 PI-DRAT0120-CADD-PF13-PF14      VALUE 'PF13PF14'.
                   88 PI-DRAT0120-CADD-PF14-PF15      VALUE 'PF14PF15'.
                   88 PI-DRAT0120-CADD-PF14-PG17      VALUE 'PF14PG17'.
                   88 PI-DRAT0120-CADD-PF15-PG17      VALUE 'PF15PG17'.
      *
      *
               10  PI-DRAT0120-CADD-GES              PIC  9(002).
               10  PI-DRAT0120-CADD-S-GES            PIC  X(001).
      *
               10  PI-DRAT0120-CADD-VNR              PIC  X(015).
               10  PI-DRAT0120-CADD-S-VNR            PIC  X(001).
      *
               10  PI-DRAT0120-CADD-PIN              PIC  9(009) COMP-3.
060314*        10  FILLER                            PIC  X(002).
               10  PI-DRAT0120-CADD-S-PIN            PIC  X(001).
      *
               10  PI-DRAT0120-CADD-VB-NR            PIC  9(007).
               10  PI-DRAT0120-CADD-S-VB-NR          PIC  X(001).
      *
               10  PI-DRAT0120-CADD-PROD-SCHL        PIC S9(004) BINARY.
               10  PI-DRAT0120-CADD-S-PRODSCHL       PIC  X(001).
      *
               10  PI-DRAT0120-CADD-DTA-ID            PIC  S9(015)
                                                               COMP-3.
               10  FILLER REDEFINES      PI-DRAT0120-CADD-DTA-ID.
                   15  PI-DRAT0120-CADD-PDPT-ID      PIC S9(015) COMP-3.
               10  FILLER REDEFINES      PI-DRAT0120-CADD-DTA-ID.
                   15  PI-DRAT0120-CADD-PNAP-ID      PIC S9(015) COMP-3.
               10  FILLER REDEFINES      PI-DRAT0120-CADD-DTA-ID.
                   15  PI-DRAT0120-CADD-PROT-ID      PIC S9(015) COMP-3.
               10  PI-DRAT0120-CADD-S-DTA-ID          PIC  X(001).
               10  FILLER REDEFINES      PI-DRAT0120-CADD-S-DTA-ID.
                   15  PI-DRAT0120-CADD-S-PDPT-ID     PIC  X(001).
               10  FILLER REDEFINES      PI-DRAT0120-CADD-S-DTA-ID.
                   15  PI-DRAT0120-CADD-S-PNAP-ID     PIC  X(001).
               10  FILLER REDEFINES      PI-DRAT0120-CADD-S-DTA-ID.
                   15  PI-DRAT0120-CADD-S-PROT-ID     PIC  X(001).
      *
               10  PI-DRAT0120-CADD-DTA-ART           PIC  X(003).
080513         10  FILLER REDEFINES PI-DRAT0120-CADD-DTA-ART.
080513             15  PI-DRAT0120-CADD-UNTERNR       PIC  X(003).
               10  PI-DRAT0120-CADD-S-DTA-ART         PIC  X(001).
080513         10  FILLER REDEFINES PI-DRAT0120-CADD-S-DTA-ART.
080513             15  PI-DRAT0120-CADD-S-UNTERNR     PIC  X(001).
      *
               10  PI-DRAT0120-CADD-VSTAT             PIC  X(004).
080513         10  FILLER REDEFINES PI-DRAT0120-CADD-VSTAT.
080513             15  PI-DRAT0120-CADD-SCHICHTNR     PIC  X(003).
080513             15  FILLER                         PIC  X(001).
               10  PI-DRAT0120-CADD-S-VSTAT           PIC  X(001).
080513         10  FILLER REDEFINES PI-DRAT0120-CADD-S-VSTAT.
080513             15  PI-DRAT0120-CADD-S-SCHICHTNR   PIC  X(001).
      *
               10  PI-DRAT0120-CADD-TAG-ANF           PIC  X(002).
               10  PI-DRAT0120-CADD-S-TAG-ANF         PIC  X(001).
               10  PI-DRAT0120-CADD-ZEITRAUM-ANF.
                   15  PI-DRAT0120-CADD-MONAT-ANF     PIC  X(002).
                   15  PI-DRAT0120-CADD-S-MONAT-ANF   PIC  X(001).
                   15  PI-DRAT0120-CADD-JAHR-ANF      PIC  X(004).
                   15  PI-DRAT0120-CADD-S-JAHR-ANF    PIC  X(001).
      *
               10  PI-DRAT0120-CADD-TAG-ENDE          PIC  X(002).
               10  PI-DRAT0120-CADD-S-TAG-ENDE        PIC  X(001).
               10  PI-DRAT0120-CADD-ZEITRAUM-ENDE.
                   15  PI-DRAT0120-CADD-MONAT-ENDE    PIC  X(002).
                   15  PI-DRAT0120-CADD-S-MONAT-ENDE  PIC  X(001).
                   15  PI-DRAT0120-CADD-JAHR-ENDE     PIC  X(004).
                   15  PI-DRAT0120-CADD-S-JAHR-ENDE   PIC  X(001).
      *
               10  PI-DRAT0120-CADD-VSTAT-KLASSE      PIC  X(001).
               10  PI-DRAT0120-CADD-S-VSTAT-KLASS     PIC  X(001).
      *
               10  PI-DRAT0120-CADD-PGV               PIC  9(004).
               10  FILLER REDEFINES      PI-DRAT0120-CADD-PGV.
                   15  PI-DRAT0120-CADD-PMPG-VERSION      PIC  9(004).
               10  PI-DRAT0120-CADD-S-PGV             PIC  X(001).
      *
               10  PI-DRAT0120-CADD-PMPG-ID           PIC  S9(015)
                                                               COMP-3.
               10  FILLER REDEFINES      PI-DRAT0120-CADD-PMPG-ID.
                   15  PI-DRAT0120-CADD-PRINTE       PIC S9(015) COMP-3.
               10  PI-DRAT0120-CADD-S-PMPG-ID         PIC  X(001).
               10  FILLER REDEFINES      PI-DRAT0120-CADD-S-PMPG-ID.
                   15  PI-DRAT0120-CADD-S-PRINTE     PIC   X(001).
      *
      ***      AUS PLATZGRÜNDEN PMPG-VERSION BEI PGV REDEFINIERT.
               10  PI-DRAT0120-CADD-PMPG-VERS-ALT     PIC  9(003).

      ***          AUS PLATZNOT WURDE EIN REDEFINE DURCHGEFüHRT
               10  FILLER REDEFINES      PI-DRAT0120-CADD-PMPG-VERS-ALT.
                   15  PI-DRAT0120-CADD-HISTNR       PIC S9(004) BINARY.
                   15  PI-DRAT0120-CADD-S-HISTNR     PIC  X(001).

               10  PI-DRAT0120-CADD-S-PMPG-VERS       PIC  X(001).
      *
      ***      TS-QUEUE NAME FÜR DATENÜBERGABE BEI ÜBERGANG
      ***      VON PG19  ZU  PG17
               10  PI-DRAT0120-CADD-TSQDD-NAME        PIC  X(008).
201118         10  FILLER REDEFINES PI-DRAT0120-CADD-TSQDD-NAME.
201118             15  PI-DRAT0120-CADD-GEL-KENNUNG   PIC  X(006).
201118             15  PI-DRAT0120-CADD-S-GEL-KENNUNG PIC  X(001).
201118             15  FILLER                         PIC  X(001).
      *
               10  PI-DRAT0120-CADD-ORDNR             PIC  9(002).
               10  FILLER REDEFINES      PI-DRAT0120-CADD-ORDNR.
                   15  PI-DRAT0120-CADD-MANDANT       PIC  9(002).
               10  PI-DRAT0120-CADD-S-ORDNR           PIC  X(001).
               10  FILLER REDEFINES      PI-DRAT0120-CADD-S-ORDNR.
                   15  PI-DRAT0120-CADD-S-MANDANT     PIC  X(001).
      *
060314*        10  PI-DRAT0120-CADD-FOLGENR           PIC  9(002).
060314         10  PI-DRAT0120-CADD-FOLGENR           PIC  9(003).
               10  PI-DRAT0120-CADD-S-FOLGENR         PIC  X(001).
060314         10  FILLER                             PIC  X(001).
      *
       01  PO-DRAT0120.
           05  PO-DRAT0120-CADD-O-KOMM-VAR            PIC  X(128).
120719     05  FILLER REDEFINES   PO-DRAT0120-CADD-O-KOMM-VAR.

120719***  HIER WIRD DER RETURNCODE ÜBERGEBEN
               10  PO-DRAT0120-CADD-ZST-PRO-RATA    PIC  X(001).
                   88 PO-DRAT0120-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0120-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0120-CADD-ABBR-PF3         VALUE '3'.
               10  PO-DRAT0120-CADD-PO27            PIC  X(001).
      ******* ENDE COPY-MEMBER DRAT0120  ****************************


