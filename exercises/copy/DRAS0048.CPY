      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0048.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0048.CPv  $
      *
      *   Rev 2.1   Feb 12 2018 12:32:30   t13ae30
      *FAS00330: Siegeradler DVAG, Wien EH Korrektur 
      *
      *   Rev 2.0   Feb 06 2018 18:04:54   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAS00333006.02.18! 01 ! NEUE FUNKTION DRAS0048    ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAS0048                                 *
      *    EH ZUSTRUKTURIERTER PARTNER                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAS0048.
          05 PI-DRAS0048-DATEN.
             10 PI-DRAS0048-PARTDW-BETR      PIC S9(009) COMP.
             10 PI-DRAS0048-JAHR             PIC S9(04) BINARY.
             10 PI-DRAS0048-PARTDW-VON       PIC  X(010).
             10 PI-DRAS0048-PARTDW-BIS       PIC  X(010).
             10 PI-DRAS0048-KTY-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0048-KNR-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0048-KNR-ZUSTRK-TYP   PIC S9(04) BINARY.
      *
       01 PO-DRAS0048.
          05 PO-DRAS0048-RC                  PIC  X(003).
             88 PO-DRAS0048-RC-OK            VALUE 'OK '.
             88 PO-DRAS0048-RC-NFD           VALUE 'NFD'.

          05 PO-DRAS0048-DATEN.
             10 PO-DRAS0048-BETR-ST-NR       PIC S9(009)   COMP.
             10 PO-DRAS0048-EH-ZUSTRUK-PART  PIC S9(009)V9 COMP-3.
      ******** ENDE COPY-MEMBER **************************************


