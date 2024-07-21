      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  10.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0047.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0047.CPv  $
      *
      *   Rev 2.1   Jan 17 2018 14:14:52   t13ae30
      *AAS01225: Programme - TWI
      *
      *   Rev 2.0   Jan 10 2018 13:58:24   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01225!10.01.18!2.1 ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAS0047                                 *
      *    EH ZUSTRUKTURIERTER PARTNER                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAS0047.
          05 PI-DRAS0047-DATEN.
             10 PI-DRAS0047-PS-BETR          PIC S9(009) COMP.
             10 PI-DRAS0047-JAHR             PIC S9(04) BINARY.
             10 PI-DRAS0047-PS-VON           PIC  X(010).
             10 PI-DRAS0047-PS-BIS           PIC  X(010).
             10 PI-DRAS0047-KTY-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0047-KNR-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0047-KNR-ZUSTRK-TYP   PIC S9(04) BINARY.
      *
       01 PO-DRAS0047.
          05 PO-DRAS0047-RC                  PIC  X(003).
             88 PO-DRAS0047-RC-OK            VALUE 'OK '.
             88 PO-DRAS0047-RC-NFD           VALUE 'NFD'.

          05 PO-DRAS0047-DATEN.
             10 PO-DRAS0047-BETR-ST-NR       PIC S9(009)   COMP.
             10 PO-DRAS0047-EH-ZUSTRUK-PART  PIC S9(009)V9 COMP-3.
      ******** ENDE COPY-MEMBER **************************************


