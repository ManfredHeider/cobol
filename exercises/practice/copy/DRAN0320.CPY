      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0320.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0320.CPv  $
      *
      *   Rev 2.3   Jan 22 2018 19:34:00   T13AE34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.2   Jan 22 2018 19:14:46   T13AE34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.1   Jan 22 2018 11:24:20   t13ae34
      *AAN01355: Addieren Werte auf Hauptrodukt AML (20) aufgrund T
      *BAN0011
      *
      *   Rev 2.0   Jan 22 2018 08:46:24   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0320                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ANTQ - AKTUELLE EINTRÄGE PLAUSI>>   *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01355!22.01.18!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0320                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRAN0320.
          05 PI-DRAN0320-PROGRAMM               PIC  X(008).
          05 PI-DRAN0320-UFKT                   PIC  9(002).
          05 PI-DRAN0320-ANTQ-PLAUSI            PIC S9(4) COMP.
      *
       01 PO-DRAN0320.
          05 PO-DRAN0320-RC                     PIC  X(003).
             88 PO-DRAN0320-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0320-RC-EOF                   VALUE 'EOF'.
             88 PO-DRAN0320-RC-LEER                  VALUE 'LER'.
          05 PO-DRAN0320-DATEN.
             10 PO-DRAN0320-PGEB-GES            PIC S9(4) COMP.
             10 PO-DRAN0320-PROD-PROD-SCHL      PIC S9(4) COMP.
             10 PO-DRAN0320-ANTQ-GUELTIG-AB     PIC X(10).
             10 PO-DRAN0320-ANTQ-GUELTIG-BIS    PIC X(10).
             10 PO-DRAN0320-ANTQ-WERT-NUM       PIC S9(9)V9(3) COMP-3.
             10 PO-DRAN0320-ANTQ-WERT-ALPHANUM  PIC X(30).
             10 PO-DRAN0320-ANTQ-LAST-UPDATE    PIC X(26).
             10 PO-DRAN0320-ANTQ-KENNUNG        PIC X(8).
      *
      ******* ENDE COPY-MEMBER **************************************



