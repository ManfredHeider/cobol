      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  16.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3806.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3806.CPv  $
      *
      *   Rev 2.1   Jul 06 2020 13:47:24   u130176
      *AKI01789: Anpassungen Kundenbestandsvolumen für AS-Club (Ums
      *etzung)
      *
      *   Rev 2.0   Jun 16 2020 16:33:58   u130176
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3806                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJKI0041                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: LORSON                                           *
      *    DATUM:    17.06.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI01789!17.06.20!2.1 ! NEU                       ! U130176   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI3806.
           05  PI-DRKI3806-PROGRAMM              PIC  X(008).
           05  PI-DRKI3806-UFKT                  PIC  9(002).
      *
       01  PO-DRKI3806.
           05  PO-DRKI3806-RC                    PIC  X(003).
               88  PO-DRKI3806-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3806-RC-LEER                VALUE 'LER'.
               88  PO-DRKI3806-RC-EOF                 VALUE 'EOF'.
           05  PO-DRKI3806-DATEN.
             10 PO-DRKI3806-VERPARTGESELLSCHID   PIC S9(4) USAGE COMP.
             10 PO-DRKI3806-VERVERTRAGSNUMMER    PIC X(15).
             10 PO-DRKI3806-VERMANDANTNR         PIC S9(9) USAGE COMP.
             10 PO-DRKI3806-VBNEUID              PIC X(13).
             10 PO-DRKI3806-KDUGUELTIGABDAT      PIC X(10).


