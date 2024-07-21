      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0993.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0993.CPv  $
      *
      *   Rev 2.1   Aug 26 2010 11:30:28   t13ex13
      *ASL00354: Columbus-Bonus für FVD
      *
      *   Rev 2.0   Aug 19 2010 13:48:54   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0993                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0201                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    19.08.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00354!19.08.10!2.1 ! NEU                       ! GUTFLEISCH*
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB0993 - ERMITTELN SATZ AUS TABELLE CBAN        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0993.
          05 PI-DRVB0993-DATEN.
             10 PI-DRVB0993-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB0993-CBAN-ANSPR-VOM     PIC  X(10).
             10 PI-DRVB0993-CBAN-PART-VBNR     PIC S9(09)       BINARY.
             10 PI-DRVB0993-KNR-NERF-GRUND     PIC S9(04)       BINARY.

       01 PO-DRVB0993.
         03 PO-DRVB0993-RC                     PIC  X(03).
             88 PO-DRVB0993-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0993-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB0993-DATEN.
          05 PO-DRVB0993-NI-VIVB0201.
             10 NI-DRVB0993-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB0993-CBAN-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB0993-CBAN-PART-VBNR     PIC S9(04)       BINARY.
             10 NI-DRVB0993-KNR-NERF-GRUND     PIC S9(04)       BINARY.
             10 NI-DRVB0993-KTY-NERF-GRUND     PIC S9(04)       BINARY.
             10 NI-DRVB0993-CBAN-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB0993-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0993-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0993-CBAN-PART-BETRAG   PIC S9(04)       BINARY.
             10 NI-DRVB0993-CBAN-PART-EH       PIC S9(04)       BINARY.
             10 NI-DRVB0993-CBAN-PART-SEIT     PIC S9(04)       BINARY.
             10 NI-DRVB0993-KTY-NERF-STATUS    PIC S9(04)       BINARY.
             10 NI-DRVB0993-KNR-NERF-STATUS    PIC S9(04)       BINARY.
          05 PO-DRVB0993-DCL-VIVB0201.
             10 PO-DRVB0993-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB0993-CBAN-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB0993-CBAN-PART-VBNR     PIC S9(09)       BINARY.
             10 PO-DRVB0993-KNR-NERF-GRUND     PIC S9(04)       BINARY.
             10 PO-DRVB0993-KTY-NERF-GRUND     PIC S9(04)       BINARY.
             10 PO-DRVB0993-CBAN-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB0993-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0993-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0993-CBAN-PART-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0993-CBAN-PART-EH       PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0993-CBAN-PART-SEIT     PIC  X(10).
             10 PO-DRVB0993-KTY-NERF-STATUS    PIC S9(04)       BINARY.
             10 PO-DRVB0993-KNR-NERF-STATUS    PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0993   --------------------------*
      *---------------------------------------------------------------*
