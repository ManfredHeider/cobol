      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1089.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1089.CPv  $
      *
      *   Rev 2.1   Jun 10 2015 14:26:08   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Mar 27 2015 11:08:32   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1089                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0125                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: V.RAHNER                                         *
      *    DATUM:    17.03.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAN01198!        !2.1 ! NEU                       ! RAHNER    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1089 - ERMITTELN SATZ AUS TABELLE USTAT       *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1089.
          05 PI-DRVB1089-DATEN.
             10 PI-DRVB1089-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1089-USTAT-DAT-BIS      PIC X(10).

       01 PO-DRVB1089.
         03 PO-DRVB1089-RC                     PIC  X(03).
             88 PO-DRVB1089-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1089-RC-NOT-FOUND            VALUE 'NFD'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1089   --------------------------*
      *---------------------------------------------------------------*

