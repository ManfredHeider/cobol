      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1215.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1215.CPv  $
      *
      *   Rev 2.1   Jun 24 2019 12:53:12   t13ae30
      *AVB01556: Allfinanz 1 & 2: Anpassung Beförderungsprüfung Str
      *uktur (VB-System)
      *
      *   Rev 2.0   Jun 03 2019 14:08:48   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1215                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR ZJVB0037                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    03.06.2019                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB01556!03.06.19! 2.1! NEU                       !TH.WILLIG *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1215                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB1215.
          05 PI-DRVB1215-PROGRAMM           PIC  X(008).
          05 PI-DRVB1215-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1215-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRVB1215.

          05 PO-DRVB1215-RC                 PIC  X(003).
             88 PO-DRVB1215-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1215-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1215-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1215-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1215-DATEN.
             10 PO-DRVB1215-UNTST-NR        PIC S9(009) BINARY.
             10 PO-DRVB1215-BETR-SEIT       PIC  X(010) .
             10 PO-DRVB1215-GEEV-AB         PIC  X(010) .
             10 PO-DRVB1215-STUFE           PIC S9(004) BINARY.

      *
      ***** ENDE COPY-MEMBER DRVB1215 *****************************


