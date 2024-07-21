      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1024.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1024.CPv  $
      *
      *   Rev 2.5   May 22 2018 07:42:22   t13ex67
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSS
      *
      *   Rev 2.4   Mar 28 2017 08:20:08   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.3   Jul 16 2015 15:01:40   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.2   Dec 17 2013 13:15:34   t13ae39
      *APA01539: Kalypso TP5: Anpassungen Datenmodell WEPA0008
      *
      *   Rev 2.1   Aug 23 1999 11:44:36   T13EX21
      *APA00049 - PO-Bereich erweitert
      *
      *   Rev 2.0   May 26 1999 13:42:22   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:47:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1024                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE üBER PGEB UND VNR>>         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B. BOHLINGER                                     *
      *    DATUM:    22.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.07.98!01.01! NEU                      ! BOHLINGER*
      * APA00049!02.07.99!01.08! PO-BEREICH ERWEITERT     ! STORCH   *
      * APA01539!30.07.13!02.02! ERGÄNZUNGEN FÜR KALYPSO  ! STRAUSS  *
      * APA01932!27.03.15!02.03! NEUE ATTRIBUTE HINZU     ! STORCH   *
      * APA02353!08.03.17!02.04! NEUES ATTRIBUT HINZU     ! STORCH   *
      * APA02579!08.05.18!02.05! NEUES ATTRIBUT HINZU     ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1024                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1024.
          05 PI-DRPA1024-PROGRAMM            PIC  X(008).
          05 PI-DRPA1024-UFKT                PIC  9(002).
          05 PI-DRPA1024-DATEN.
             10 PI-DRPA1024-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1024-VNR              PIC  X(015).
      *
       01 PO-DRPA1024.
          05 PO-DRPA1024-RC                  PIC  X(003).
             88 PO-DRPA1024-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1024-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1024-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1024-DATEN.
             10 PMPG-ID                      PIC S9(015)
                                             PACKED-DECIMAL.
             10 PMPG-VERSION                 PIC S9(004) BINARY.
             10 PMPG-LFD-NR-VNR              PIC S9(009) BINARY.
             10 PDTA-ID                      PIC S9(015)
                                             PACKED-DECIMAL.
             10 PGEB-GES                     PIC S9(4) USAGE COMP.
             10 PMPE-VNR                     PIC X(15).
             10 PMPE-VERTR-BEGINN            PIC X(10).
             10 PMPE-VERTR-ABLAUF            PIC X(10).
             10 PMPE-FAELLIG-VON             PIC  X(010).
             10 PMPE-FAELLIG-BIS             PIC  X(010).
             10 PMPE-PG-VBNR                 PIC S9(9) USAGE COMP.
             10 PMPE-KUNDE-NAME              PIC X(25).
             10 PMPE-KUNDE-VNAME             PIC X(20).
             10 PMPE-GSMT-PROV-DAT           PIC X(10).
             10 PMPE-PG-ABR-DAT              PIC X(10).
             10 PMPE-PG-EINL-DAT             PIC X(10).
             10 PMPE-PIN                     PIC S9(9) USAGE COMP.
             10 KNR-ZAHLWEISE                PIC S9(4) USAGE COMP.
             10 KNR-WAEHRUNG                 PIC S9(004) BINARY.
             10 PMPE-VSTAT                   PIC  X(004).
             10 PMPE-LAST-UPDATE             PIC X(26).
             10 PMPE-KENNUNG                 PIC X(8).
             10 PMPE-PG-PM-DATEN.
                49 PMPE-PG-PM-DATEN-LEN      PIC S9(4) USAGE COMP.
                49 PMPE-PG-PM-DATEN-TEXT     PIC X(300).
290713       10 PMPE-BZ-NAME                 PIC  X(025).
290713       10 PMPE-BZ-VNAME                PIC  X(020).
290713       10 PMPE-VP-NAME                 PIC  X(025).
290713       10 PMPE-VP-VNAME                PIC  X(020).
290713       10 PMPE-KZ-EIGENV               PIC  X(1).
290713       10 KNR-FEIV-KZ                  PIC S9(004) BINARY.
290713       10 KNR-VERM-GES                 PIC S9(004) BINARY.
290713       10 PMPE-UMW-NR                  PIC S9(015)V
290713                                       PACKED-DECIMAL.
290713       10 PMPE-UNR                     PIC S9(004) BINARY.
290713       10 PMPE-SCHICHT-NR              PIC S9(004) BINARY.
290713       10 PMPE-INKST-DAT               PIC  X(010).
290713       10 KNR-INK-ART                  PIC S9(004) BINARY.
290713       10 PMPE-SAM-NR                  PIC S9(009) BINARY.
290713       10 KNR-RAB-ST                   PIC S9(004) BINARY.
290713       10 PMPE-VVG-KZ                  PIC  X(001).
290713       10 KNR-DEPOT-KZ                 PIC S9(004) BINARY.
290713       10 PMPE-PROV-SCHL               PIC  X(005).
290713       10 PMPE-BEW-DAT                 PIC  X(010).
290713       10 PMPE-URSPR-POL-DAT           PIC  X(010).
290713       10 PMPE-POL-DAT                 PIC  X(010).
290713       10 PMPE-BESVV                   PIC S9(001)V
290713                                       PACKED-DECIMAL.
290713       10 PMPE-VN-STRASSE              PIC X(035).
290713       10 PMPE-VN-PLZ                  PIC X(005).
290713       10 PMPE-VN-ORT                  PIC X(035).
290713       10 PMPE-RATENBEITRAG            PIC S9(009)V9(2)
290713                                       PACKED-DECIMAL.
270315*      10 PMPE-PG-RECH-NR              PIC X(12).
080317       10 PMPE-PG-RECH-NR              PIC X(30).
270315       10 PMPE-HIST-UNR                PIC S9(04).
080317       10 KNR-TLSN-PARTNER             PIC S9(004) BINARY.
080518       10 PMPE-GEL-KENNUNG             PIC X(06).


