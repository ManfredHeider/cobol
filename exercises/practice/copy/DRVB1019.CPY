      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1019.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1019.CPv  $
      *
      *   Rev 2.1   Jul 07 2011 15:49:44   t13ae30
      *AAS00964: Zustrukturierung von Partnern
      *
      *   Rev 2.0   May 11 2011 18:35:34   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1019                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0312                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    11.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00964!11.05.11!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1019 - ERMITTELN SATZ AUS TABELLE DIRW        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1019.
          05 PI-DRVB1019-DATEN.
             10 PI-DRVB1019-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1019-DIRW-DAT-BIS       PIC  X(10).

       01 PO-DRVB1019.
         03 PO-DRVB1019-RC                     PIC  X(03).
             88 PO-DRVB1019-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1019-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1019-DATEN.
          05 PO-DRVB1019-NI-VIVB0312.
             10 NI-DRVB1019-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1019-DIR-ALT            PIC S9(04)       BINARY.
             10 NI-DRVB1019-DIR-NEU            PIC S9(04)       BINARY.
             10 NI-DRVB1019-DIRW-DAT-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1019-DIRW-DAT-BIS       PIC S9(04)       BINARY.
             10 NI-DRVB1019-DIRW-LAST-UPDATE   PIC S9(04)       BINARY.
          05 PO-DRVB1019-DCL-VIVB0312.
             10 PO-DRVB1019-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1019-DIR-ALT            PIC S9(04)       BINARY.
             10 PO-DRVB1019-DIR-NEU            PIC S9(04)       BINARY.
             10 PO-DRVB1019-DIRW-DAT-VON       PIC  X(26).
             10 PO-DRVB1019-DIRW-DAT-BIS       PIC  X(10).
             10 PO-DRVB1019-DIRW-LAST-UPDATE   PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1019   --------------------------*
      *---------------------------------------------------------------*
