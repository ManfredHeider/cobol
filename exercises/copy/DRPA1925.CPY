      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1925.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1925.CPv  $
      *
      *   Rev 2.1   Aug 26 2019 12:10:26   t13ex67
      *APA02843: WEPA0008
      *
      *   Rev 2.0   Aug 19 2019 11:22:18   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1925  FUNKTION LIST PDPE-MELDUNGEN       *
      *                           SORTIERT NACH PDPT-PDPE-NR         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    19.08.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02843!19.08.19!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1925.
          05 PI-DRPA1925-PROGRAMM            PIC  X(008).
          05 PI-DRPA1925-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1925-DATEN.
             10 PI-DRPA1925-PDPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1925.
          05 PO-DRPA1925-RC                         PIC  X(003).
             88 PO-DRPA1925-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1925-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1925-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1925-PDPE.
              10 PGEB-GES                    PIC S9(4)   COMP.
              10 PDPT-ID                     PIC S9(15)V COMP-3.
              10 PDPT-PDPE-NR                PIC S9(4)   COMP.
              10 PDPE-ST-NR                  PIC S9(9)   COMP.
              10 PDPE-NT-PBTRG-BRWG          PIC S9(9)V9(2) COMP-3.
              10 PDPE-PROV-RCK-BRWG          PIC S9(9)V9(2) COMP-3.
              10 PDPE-EINH                   PIC S9(6)V9(2) COMP-3.
              10 PDPE-NT-PBTRG-AZWG          PIC S9(9)V9(2) COMP-3.
              10 PDPE-PROV-RCK-AZWG          PIC S9(9)V9(2) COMP-3.
              10 KNR-AUSZ-WAEHRUNG           PIC S9(4)   COMP.
              10 PDPE-BUCH-RELEVANT          PIC X(1).
              10 PDPE-UST-PBTRG-BWG          PIC S9(9)V9(2) COMP-3.
              10 PDPE-UST-RCK-BRWG           PIC S9(9)V9(2) COMP-3.
              10 PDPE-UST-PBTRG-AWG          PIC S9(9)V9(2) COMP-3.
              10 PDPE-UST-RCK-AZWG           PIC S9(9)V9(2) COMP-3.
              10 PDPE-KZ-UST-BER             PIC X(1).
              10 PDPE-LAST-UPDATE            PIC X(26).
      *
      ******* ENDE COPY-MEMBER **************************************

