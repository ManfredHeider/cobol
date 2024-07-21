      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1045.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1045.CPv  $
      *
      *   Rev 2.1   Feb 06 2012 16:52:10   t13ex43
      *AVB01195: PGM - Prüfung in VB45 'Orgabetreuer für Einzeldire
      *ktion'
      *
      *   Rev 2.0   Feb 06 2012 14:52:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1045                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVBB099                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    06.02.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01195!06.02.12!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1045 - ERMITTELN ORGABETREUER PRO DIREKTION   *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1045.
          05 PI-DRVB1045-DATEN.
             10 PI-DRVB1045-NAMZ-LFD           PIC S9(04)       BINARY.
             10 PI-DRVB1045-KTY-NAMZ-FUNK      PIC S9(04)       BINARY.
             10 PI-DRVB1045-KNR-NAMZ-FUNK      PIC S9(04)       BINARY.
             10 PI-DRVB1045-NAMZ-DAT-BIS       PIC  X(10).

       01 PO-DRVB1045.
         03 PO-DRVB1045-RC                     PIC  X(03).
             88 PO-DRVB1045-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1045-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1045-DATEN.
          05 PO-DRVB1045-NI-VIVBB099.
             10 NI-DRVB1045-KTY-NAMZ-TYP       PIC S9(04)       BINARY.
             10 NI-DRVB1045-KNR-NAMZ-TYP       PIC S9(04)       BINARY.
             10 NI-DRVB1045-NAMZ-LFD           PIC S9(04)       BINARY.
             10 NI-DRVB1045-KTY-ANREDE         PIC S9(04)       BINARY.
             10 NI-DRVB1045-KNR-ANREDE         PIC S9(04)       BINARY.
             10 NI-DRVB1045-NAMZ-TITEL-AKAD    PIC S9(04)       BINARY.
             10 NI-DRVB1045-NAMZ-NAME          PIC S9(04)       BINARY.
             10 NI-DRVB1045-NAMZ-VORNAME       PIC S9(04)       BINARY.
             10 NI-DRVB1045-KTY-NAMZ-FUNK      PIC S9(04)       BINARY.
             10 NI-DRVB1045-KNR-NAMZ-FUNK      PIC S9(04)       BINARY.
             10 NI-DRVB1045-NAMZ-DAT-VON       PIC S9(04)       BINARY.
             10 NI-DRVB1045-NAMZ-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1045-NAMZ-DAT-BIS       PIC S9(04)       BINARY.
          05 PO-DRVB1045-DCL-VIVBB099.
             10 PO-DRVB1045-KTY-NAMZ-TYP       PIC S9(04)       BINARY.
             10 PO-DRVB1045-KNR-NAMZ-TYP       PIC S9(04)       BINARY.
             10 PO-DRVB1045-NAMZ-LFD           PIC S9(04)       BINARY.
             10 PO-DRVB1045-KTY-ANREDE         PIC S9(04)       BINARY.
             10 PO-DRVB1045-KNR-ANREDE         PIC S9(04)       BINARY.
             10 PO-DRVB1045-NAMZ-TITEL-AKAD    PIC  X(10).
             10 PO-DRVB1045-NAMZ-NAME          PIC  X(40).
             10 PO-DRVB1045-NAMZ-VORNAME       PIC  X(20).
             10 PO-DRVB1045-KTY-NAMZ-FUNK      PIC S9(04)       BINARY.
             10 PO-DRVB1045-KNR-NAMZ-FUNK      PIC S9(04)       BINARY.
             10 PO-DRVB1045-NAMZ-DAT-VON       PIC  X(26).
             10 PO-DRVB1045-NAMZ-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1045-NAMZ-DAT-BIS       PIC  X(10).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1045   --------------------------*
      *---------------------------------------------------------------*
