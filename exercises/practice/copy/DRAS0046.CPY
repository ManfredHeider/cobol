      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0046.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0046.CPv  $
      *
      *   Rev 2.1   Jan 17 2018 14:14:40   t13ae30
      *AAS01225: Programme - TWI
      *
      *   Rev 2.0   Jan 05 2018 15:03:32   t13ae30
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
      *    FÜR DIE FUNKTION DRAS0046                                 *
      *    EH ZUSTRUKTURIERTER PARTNER                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAS0046.
          05 PI-DRAS0046-DATEN.
             10 PI-DRAS0046-PS-BETR          PIC S9(009) COMP.
             10 PI-DRAS0046-JAHR             PIC S9(04) BINARY.
             10 PI-DRAS0046-PS-VON           PIC  X(010).
             10 PI-DRAS0046-PS-BIS           PIC  X(010).
             10 PI-DRAS0046-KTY-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0046-KNR-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0046-KNR-ZUSTRK-TYP   PIC S9(04) BINARY.
      *
       01 PO-DRAS0046.
          05 PO-DRAS0046-RC                  PIC  X(003).
             88 PO-DRAS0046-RC-OK            VALUE 'OK '.
             88 PO-DRAS0046-RC-NFD           VALUE 'NFD'.

          05 PO-DRAS0046-DATEN.
             10 PO-DRAS0046-BETR-ST-NR       PIC S9(009)   COMP.
             10 PO-DRAS0046-EH-ZUSTRUK-PART  PIC S9(009)V9 COMP-3.
      ******** ENDE COPY-MEMBER **************************************


