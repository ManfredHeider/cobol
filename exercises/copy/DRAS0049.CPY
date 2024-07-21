      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0049.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0049.CPv  $
      *
      *   Rev 2.1   Feb 12 2018 12:32:40   t13ae30
      *FAS00330: Siegeradler DVAG, Wien EH Korrektur 
      *
      *   Rev 2.0   Feb 06 2018 18:05:04   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAS00330!06.02.18!2.1 ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAS0049                                 *
      *    EH ZUSTRUKTURIERTER PARTNER                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAS0049.
          05 PI-DRAS0049-DATEN.
             10 PI-DRAS0049-PS-BETR          PIC S9(009) COMP.
             10 PI-DRAS0049-JAHR             PIC S9(04) BINARY.
             10 PI-DRAS0049-PS-VON           PIC  X(010).
             10 PI-DRAS0049-PS-BIS           PIC  X(010).
             10 PI-DRAS0049-KTY-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0049-KNR-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0049-KNR-ZUSTRK-TYP   PIC S9(04) BINARY.
      *
       01 PO-DRAS0049.
          05 PO-DRAS0049-RC                  PIC  X(003).
             88 PO-DRAS0049-RC-OK            VALUE 'OK '.
             88 PO-DRAS0049-RC-NFD           VALUE 'NFD'.

          05 PO-DRAS0049-DATEN.
             10 PO-DRAS0049-BETR-ST-NR       PIC S9(009)   COMP.
             10 PO-DRAS0049-EH-ZUSTRUK-PART  PIC S9(009)V9 COMP-3.

      ******** ENDE COPY-MEMBER **************************************

