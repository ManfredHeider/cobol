      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0332.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0332.CPv  $
      *
      *   Rev 2.3   May 17 2019 10:47:12   t13ae23
      *AAN01419: Optimierung Auftrag AN205100 - capping killer
      *
      *   Rev 2.2   May 07 2019 11:31:22   t13ae23
      *AAN01419: Optimierung Auftrag AN205100 - capping killer
      *
      *   Rev 2.1   Mar 13 2019 08:33:48   p13sp16
      *Owner = T13AE34, Archivreparatur by P13SP16
      *
      *   Rev 2.0   Mar 13 2019 08:31:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0332                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ANTQ - AKTUELLE EINTRÄGE PLAUSI>>   *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01419!26.04.18!2.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0332                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRAN0332.
          05 PI-DRAN0332-PROGRAMM               PIC  X(008).
          05 PI-DRAN0332-UFKT                   PIC  9(002).
      *
       01 PO-DRAN0332.
          05 PO-DRAN0332-RC                     PIC  X(003).
             88 PO-DRAN0332-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0332-RC-EOF                   VALUE 'EOF'.
             88 PO-DRAN0332-RC-LEER                  VALUE 'LER'.
          05 PO-DRAN0332-DATEN.
             10 PO-DRAN0332-PGEB-GES            PIC S9(4) COMP.
             10 PO-DRAN0332-PROD-PROD-SCHL      PIC S9(4) COMP.
             10 PO-DRAN0332-ANTQ-PLAUSI         PIC S9(4) COMP.
             10 PO-DRAN0332-ANTQ-GUELTIG-AB     PIC X(10).
             10 PO-DRAN0332-ANTQ-GUELTIG-BIS    PIC X(10).
             10 PO-DRAN0332-ANTQ-WERT-NUM       PIC S9(9)V9(3) COMP-3.
             10 PO-DRAN0332-ANTQ-WERT-ALPHANUM  PIC X(30).
             10 PO-DRAN0332-ANTQ-LAST-UPDATE    PIC X(26).
             10 PO-DRAN0332-ANTQ-KENNUNG        PIC X(8).
      *
      ******* ENDE COPY-MEMBER **************************************


