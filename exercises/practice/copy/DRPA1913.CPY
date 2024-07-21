      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1913.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1913.CPv  $
      *
      *   Rev 2.1   Nov 27 2018 07:22:32   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.0   Nov 20 2018 10:17:54   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1913                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE ÜBER PGEB, VNR UND KENNUNG>>*
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH-SCHÄFER                                *
      *    DATUM:    20.11.2018                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA01721!20.11.18! 2.1 ! NEU                      ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1913                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1913.
          05 PI-DRPA1913-PROGRAMM            PIC  X(008).
          05 PI-DRPA1913-UFKT                PIC  9(002).
          05 PI-DRPA1913-DATEN.
             10 PI-DRPA1913-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1913-VNR              PIC  X(015).
             10 PI-DRPA1913-GEL-KENNUNG      PIC  X(006).
      *
       01 PO-DRPA1913.
          05 PO-DRPA1913-RC                  PIC  X(003).
             88 PO-DRPA1913-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1913-RC-EOF                VALUE 'EOF'.
             88 PO-DRPA1913-RC-LEER               VALUE 'LER'.

          05 PO-DRPA1913-DATEN.
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
             10 PMPE-PG-RECH-NR              PIC X(12).
             10 PMPE-HIST-UNR                PIC S9(004) BINARY.
             10 KNR-TLSN-PARTNER             PIC S9(004) BINARY.
             10 PMPE-GEL-KENNUNG             PIC X(06).
      *

