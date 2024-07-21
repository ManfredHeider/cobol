      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0899.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRPA0899.CPv  $
      *
      *   Rev 2.10   Nov 27 2018 06:54:12   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.9   Mar 28 2017 09:03:48   t13ex67
      *APA02353: Neuer Produktpartner TeleSon - Dialoge
      *
      *   Rev 2.8   Sep 28 2016 10:42:22   t13ex67
      *APA02280: Dialoganpassungen PO23
      *
      *   Rev 2.7   Mar 10 2014 08:51:00   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.6   Jan 28 2014 08:19:00   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.5   Dec 19 2013 11:20:38   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.4   Jun 19 2012 09:26:08   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:51:00   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 11:06:30   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   03 Nov 2006 09:57:30   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:29:00   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:49:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0899 FUNKTION PRUEFEN-KEYFELDER          *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.11.97! 01 !NEU                        !BOHLINGER *
      *         !        !01.01! NEUE K-VARIANTE-UGPA0001 !BOHLINGER *
      *         !14.12.98!01.02! PO-BEREICH ERWEITERT     !BOHLINGER *
      * APA00452!02.11.06! 2.1 !ÄND. WG. 3-STELL. PRODUKTE!WANG      *
      * APA01101!05.03.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !     !*-ZEILEN ENTFERNT         !          *
      * APA01526!29.04.13!2.05 !WEITERE AUSWAHLKRITERIEN  ! STORCH   *
      *         !06.03.14!2.06 !FOLGENR. DREISTELLIG      ! STORCH   *
      * APA02353!28.02.17!2.09 ! NEU: KNR-TLSN-PARTNER    ! STORCH   *
      * APA02721!24.09.18!2.10 !WEITERES AUSWAHLKRITERIUM ! STORCH   *
      *         !        !     !GEL-KENNUNG BEI GES. 21   !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0899                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0899.
           05  PI-DRPA0899-UFKT              PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=K-VARIANTE-1, 2=K-VARIANTE-2,
      *                          3=K-VARIANTE-3, 4=K-VARIANTE-4
      *   FÜR DIALOGMODULE      10=K-VARIANTE-UGPA0001
          05 PI-DRPA0899-AUSWAHL-KRITERIEN.
             10 PI-DRPA0899-PGEB-GES         PIC X(002).
             10 PI-DRPA0899-VNR              PIC X(015).
             10 PI-DRPA0899-FAELLIG-VON      PIC X(010).
             10 PI-DRPA0899-FAELLIG-BIS      PIC X(010).
             10 PI-DRPA0899-S-FAELLIG        PIC X(001).
060314*      10 PI-DRPA0899-FOLGE-NR         PIC X(002).
060314       10 PI-DRPA0899-FOLGE-NR         PIC X(003).
             10 PI-DRPA0899-S-FOLGE-NR       PIC X(001).
             10 PI-DRPA0899-PROD-PROD-SCHL   PIC X(003).
             10 PI-DRPA0899-S-PROD-SCHL      PIC X(001).
             10 PI-DRPA0899-ORDN-NR          PIC X(002).
             10 PI-DRPA0899-S-ORDN-NR        PIC X(001).
             10 PI-DRPA0899-HIST-NR          PIC X(003).
             10 PI-DRPA0899-S-HIST-NR        PIC X(001).
290413       10 PI-DRPA0899-UNTER-NR         PIC X(003).
290413       10 PI-DRPA0899-S-UNTER-NR       PIC X(001).
290413       10 PI-DRPA0899-SCHICHT-NR       PIC X(003).
290413       10 PI-DRPA0899-S-SCHICHT-NR     PIC X(001).
050916       10 PI-DRPA0899-ABRDAT-VON       PIC X(010).
050916       10 PI-DRPA0899-ABRDAT-BIS       PIC X(010).
050916       10 PI-DRPA0899-S-ABRDAT         PIC X(001).
240918       10 PI-DRPA0899-GEL-KENNUNG      PIC X(006).
240918       10 PI-DRPA0899-S-GEL-KENNUNG    PIC X(001).
          05 PI-DRPA0899-VERTRAGS-DATEN.
             10 PI-DRPA0899-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0899-PIN              PIC S9(009) BINARY.
             10 PI-DRPA0899-HISTORIE         PIC S9(004) BINARY.
             10 PI-DRPA0899-PVER-KUNDE-NAME  PIC  X(025).
             10 PI-DRPA0899-KNR-PVER-ART     PIC S9(004) BINARY.
             10 PI-DRPA0899-PVER-PG-VBNR     PIC S9(009) BINARY.
             10 PI-DRPA0899-KNR-MANDANT      PIC S9(004) BINARY.
290413       10 PI-DRPA0899-URSPR-POL-DAT    PIC X(10).
290413       10 PI-DRPA0899-KNR-VERM-GES     PIC S9(004) BINARY.
      *
       01 PO-DRPA0899.
          05 PO-DRPA0899-RC                   PIC  X(003).
             88 PO-DRPA0899-RC-OK                  VALUE 'OK '.
             88 PO-DRPA0899-RC-NOK                 VALUE 'NOK'.

          05 PO-DRPA0899-MELDUNG.
             10 PO-DRPA0899-MELDUNG-TYP       PIC  X(001).
             10 PO-DRPA0899-MELDUNG-NR        PIC S9(004) BINARY.
             10 PO-DRPA0899-S-PMBE-VORHANDEN  PIC  X(001).

          05 PO-DRPA0899-MAP-STEUERUNG.
             10 PO-DRPA0899-MAP-ST-GES.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0899-MAP-ST-VNR.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0899-MAP-ST-FAELLIG.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0899-MAP-ST-FOLGE-NR.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0899-MAP-ST-PROD-SCHL.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0899-MAP-ST-ORDN-NR.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA0899-MAP-ST-HIST-NR.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

290413       10 PO-DRPA0899-MAP-ST-UNTER-NR.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

290413       10 PO-DRPA0899-MAP-ST-SCHICHT-NR.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

050916       10 PO-DRPA0899-MAP-ST-ABRDATUM.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

240918       10 PO-DRPA0899-MAP-GEL-KENNUNG.
                20 PO-DRPA0899-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA0899-FELD-OFFEN     PIC X(001).
                20 PO-DRPA0899-FELD-CURSOR-KZ PIC X(001).

          05 PO-DRPA0899-DATEN.
             10 PO-DRPA0899-PGEB-GES         PIC S9(004) BINARY.
             10 PO-DRPA0899-PGEB-GES-BM      PIC  X(010).
             10 PO-DRPA0899-PVER-VNR         PIC  X(015).
             10 PO-DRPA0899-PROD-PROD-SCHL   PIC S9(004) BINARY.
             10 PO-DRPA0899-PROD-SCHL-BM     PIC  X(010).
             10 PO-DRPA0899-ORDN-NR          PIC S9(004) BINARY.
             10 PO-DRPA0899-FOLGE-NR         PIC S9(004) BINARY.
290413       10 PO-DRPA0899-UNTER-NR         PIC S9(004) BINARY.
290413       10 PO-DRPA0899-SCHICHT-NR       PIC S9(004) BINARY.
             10 PO-DRPA0899-HIST-NR          PIC S9(004) BINARY.
             10 PO-DRPA0899-PVER-KUNDE-NAME  PIC  X(020).
             10 PO-DRPA0899-PVER-AKT-PIN     PIC S9(009) BINARY.
             10 PO-DRPA0899-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0899-KNR-PVER-ART     PIC S9(004) BINARY.
             10 PO-DRPA0899-PVER-PG-VBNR     PIC S9(009) BINARY.
             10 PO-DRPA0899-KNR-MANDANT      PIC S9(004) BINARY.
290413       10 PO-DRPA0899-URSPR-POL-DAT    PIC X(10).
280217       10 PO-DRPA0899-KNR-TLSN-PARTNER PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************



