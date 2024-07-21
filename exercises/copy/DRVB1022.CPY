      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1022.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1022.CPv  $
      *
      *   Rev 2.1   Jul 07 2011 15:50:00   t13ae30
      *AAS00964: Zustrukturierung von Partnern
      *
      *   Rev 2.0   Jun 01 2011 14:28:44   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1022                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0312                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    01.06.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00964!01.06.11!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1019 - ERMITTELN SATZ AUS TABELLE DIRW        *
      *                        DAT_BIS >= CURRENT DATE                *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1022.
          05 PI-DRVB1022-DATEN.
             10 PI-DRVB1022-MA-ST-NR           PIC S9(09)       BINARY.

       01 PO-DRVB1022.
         03 PO-DRVB1022-RC                     PIC  X(03).
             88 PO-DRVB1022-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1022-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1022-DATEN.
          05 PO-DRVB1022-NI-VIVB0312.
             10 NI-DRVB1022-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1022-DIR-ALT            PIC S9(04)       BINARY.
             10 NI-DRVB1022-DIR-NEU            PIC S9(04)       BINARY.
             10 NI-DRVB1022-DIRW-DAT-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1022-DIRW-DAT-BIS       PIC S9(04)       BINARY.
             10 NI-DRVB1022-DIRW-LAST-UPDATE   PIC S9(04)       BINARY.
          05 PO-DRVB1022-DCL-VIVB0312.
             10 PO-DRVB1022-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1022-DIR-ALT            PIC S9(04)       BINARY.
             10 PO-DRVB1022-DIR-NEU            PIC S9(04)       BINARY.
             10 PO-DRVB1022-DIRW-DAT-VON       PIC  X(26).
             10 PO-DRVB1022-DIRW-DAT-BIS       PIC  X(10).
             10 PO-DRVB1022-DIRW-LAST-UPDATE   PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1019   --------------------------*
      *---------------------------------------------------------------*
