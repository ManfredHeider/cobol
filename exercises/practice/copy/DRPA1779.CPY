      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1779.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1779.CPv  $
      *
      *   Rev 2.4   May 22 2018 07:42:48   t13ex67
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSS
      *
      *   Rev 2.3   Mar 28 2017 08:21:24   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.2   Jul 16 2015 15:03:04   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.1   Jan 13 2014 09:48:34   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.0   Dec 09 2013 14:48:02   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1779                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE ÜBER PGEB, VNR UND UNTERNR> *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH-SCHÄFER                                *
      *    DATUM:    09.12.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!09.12.13! 2.1 ! NEU                      ! STORCH   *
      * APA01932!22.06.15! 2.2 ! NEUE ATTTRIBUTE HINZU    ! STORCH   *
      * APA02353!08.03.17! 2.3 ! NEUES ATTRIBUT HINZU     ! STORCH   *
      * APA02579!08.05.18! 2.4 ! NEUES ATTRIBUT HINZU     ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1779                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1779.
          05 PI-DRPA1779-PROGRAMM            PIC  X(008).
          05 PI-DRPA1779-UFKT                PIC  9(002).
          05 PI-DRPA1779-DATEN.
             10 PI-DRPA1779-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1779-VNR              PIC  X(015).
             10 PI-DRPA1779-PMPE-UNR         PIC S9(004) BINARY.
      *
       01 PO-DRPA1779.
          05 PO-DRPA1779-RC                  PIC  X(003).
             88 PO-DRPA1779-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1779-RC-EOF                VALUE 'EOF'.
             88 PO-DRPA1779-RC-LEER               VALUE 'LER'.
 
          05 PO-DRPA1779-DATEN.
             10 PMPG-ID                      PIC S9(015)
                                             PACKED-DECIMAL.
             10 PMPG-VERSION                 PIC S9(004) BINARY.
             10 PMPG-LFD-NR-VNR              PIC S9(009) BINARY.
             10 PDTA-ID                      PIC S9(015)
                                             PACKED-DECIMAL.
             10 PGEB-GES                     PIC S9(004) BINARY.
             10 PMPE-VNR                     PIC  X(015).
             10 PMPE-VERTR-BEGINN            PIC  X(010).
             10 PMPE-VERTR-ABLAUF            PIC  X(010).
             10 PMPE-FAELLIG-VON             PIC  X(010).
             10 PMPE-FAELLIG-BIS             PIC  X(010).
             10 PMPE-PG-VBNR                 PIC S9(009) BINARY.
             10 PMPE-KUNDE-NAME              PIC  X(025).
             10 PMPE-KUNDE-VNAME             PIC  X(020).
             10 PMPE-GSMT-PROV-DAT           PIC  X(010).
             10 PMPE-PG-ABR-DAT              PIC  X(010).
             10 PMPE-PG-EINL-DAT             PIC  X(010).
             10 PMPE-PIN                     PIC S9(009) BINARY.
             10 KNR-ZAHLWEISE                PIC S9(004) BINARY.
             10 KNR-WAEHRUNG                 PIC S9(004) BINARY.
             10 PMPE-VSTAT                   PIC  X(004).
             10 PMPE-LAST-UPDATE             PIC  X(026).
             10 PMPE-KENNUNG                 PIC  X(008).
             10 PMPE-PG-PM-DATEN.
                49 PMPE-PG-PM-DATEN-LEN      PIC S9(004) BINARY.
                49 PMPE-PG-PM-DATEN-TEXT     PIC  X(300).
             10 PMPE-BZ-NAME                 PIC  X(025).
             10 PMPE-BZ-VNAME                PIC  X(020).
             10 PMPE-VP-NAME                 PIC  X(025).
             10 PMPE-VP-VNAME                PIC  X(020).
             10 PMPE-KZ-EIGENV               PIC  X(001).
             10 KNR-FEIV-KZ                  PIC S9(004) BINARY.
             10 KNR-VERM-GES                 PIC S9(004) BINARY.
             10 PMPE-UMW-NR                  PIC S9(015)V
                                             PACKED-DECIMAL.
             10 PMPE-UNR                     PIC S9(004) BINARY.
             10 PMPE-SCHICHT-NR              PIC S9(004) BINARY.
             10 PMPE-INKST-DAT               PIC  X(010).
             10 KNR-INK-ART                  PIC S9(004) BINARY.
             10 PMPE-SAM-NR                  PIC S9(009) BINARY.
             10 KNR-RAB-ST                   PIC S9(004) BINARY.
             10 PMPE-VVG-KZ                  PIC  X(001).
             10 KNR-DEPOT-KZ                 PIC S9(004) BINARY.
             10 PMPE-PROV-SCHL               PIC  X(005).
             10 PMPE-BEW-DAT                 PIC  X(010).
             10 PMPE-URSPR-POL-DAT           PIC  X(010).
             10 PMPE-POL-DAT                 PIC  X(010).
             10 PMPE-BESVV                   PIC S9(001)V
                                             PACKED-DECIMAL.
             10 PMPE-VN-STRASSE              PIC  X(035).
             10 PMPE-VN-PLZ                  PIC  X(005).
             10 PMPE-VN-ORT                  PIC  X(035).
             10 PMPE-RATENBEITRAG            PIC S9(009)V9(2)
                                             PACKED-DECIMAL.
220615       10 PMPE-PG-RECH-NR              PIC X(12).
220615       10 PMPE-HIST-UNR                PIC S9(04).
080317       10 KNR-TLSN-PARTNER             PIC S9(004) BINARY.
080518       10 PMPE-GEL-KENNUNG             PIC X(06).


