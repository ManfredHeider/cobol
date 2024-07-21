      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0045.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0045.CPv  $
      *
      *   Rev 2.1   Sep 20 2017 14:02:08   T13AE30
      *FAS00328: Siegeradler - Korr. UPAS0009
      *
      *   Rev 2.0   Sep 20 2017 10:57:14   T13AE30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAS00328!20.09.17! 01 ! NEUE FUNKTION DRAS0045    ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAS0045                                 *
      *    EH ZUSTRUKTURIERTER PARTNER                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAS0045.
          05 PI-DRAS0045-DATEN.
             10 PI-DRAS0045-PARTDW-BETR      PIC S9(009) COMP.
             10 PI-DRAS0045-JAHR             PIC S9(04) BINARY.
             10 PI-DRAS0045-PARTDW-VON       PIC  X(010).
             10 PI-DRAS0045-PARTDW-BIS       PIC  X(010).
             10 PI-DRAS0045-KTY-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0045-KNR-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0045-KNR-ZUSTRK-TYP   PIC S9(04) BINARY.
      *
       01 PO-DRAS0045.
          05 PO-DRAS0045-RC                  PIC  X(003).
             88 PO-DRAS0045-RC-OK            VALUE 'OK '.
             88 PO-DRAS0045-RC-NFD           VALUE 'NFD'.

          05 PO-DRAS0045-DATEN.
             10 PO-DRAS0045-BETR-ST-NR       PIC S9(009)   COMP.
             10 PO-DRAS0045-EH-ZUSTRUK-PART  PIC S9(009)V9 COMP-3.
      ******** ENDE COPY-MEMBER **************************************

