      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0994.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0994.CPv  $
      *
      *   Rev 2.1   Aug 26 2010 11:30:38   t13ex13
      *ASL00354: Columbus-Bonus für FVD
      *
      *   Rev 2.0   Aug 19 2010 16:30:36   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM
      *            DATUM      :  11.03.2010
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2010 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRVB0994.CPY  $
      *    $REVISION:   2.1  $
      *
      *    $LOG:   L:/MF/CPY/DRVB0994.CPV  $
      *
      *   REV 2.1   APR 06 2010 13:31:32   T13EX13
      *ASL00344: RGT: SL08
      *
      *   REV 2.0   MAR 11 2010 10:39:14   T13EX13
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0994                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0200                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    11.03.2010                                       *
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
      *    FUNKTION DRVB0994 - ERMITTELN SATZ AUS TABELLE CBA         *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0994.
          05 PI-DRVB0994-DATEN.
             10 PI-DRVB0994-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB0994-CBA-ANSPR-VOM      PIC  X(10).
             10 PI-DRVB0994-CBA-PART-VBNR      PIC S9(09)       BINARY.

       01 PO-DRVB0994.
         03 PO-DRVB0994-RC                     PIC  X(03).
             88 PO-DRVB0994-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0994-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB0994-DATEN.
          05 PO-DRVB0994-NI-VIVB0200.
             10 NI-DRVB0994-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB0994-CBA-ANSPR-VOM      PIC S9(04)       BINARY.
             10 NI-DRVB0994-CBA-PART-VBNR      PIC S9(04)       BINARY.
             10 NI-DRVB0994-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB0994-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB0994-CBA-LAST-UPDATE    PIC S9(04)       BINARY.
             10 NI-DRVB0994-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0994-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0994-CBA-PART-BETRAG    PIC S9(04)       BINARY.
             10 NI-DRVB0994-CBA-PART-EH        PIC S9(04)       BINARY.
             10 NI-DRVB0994-CBA-PART-SEIT      PIC S9(04)       BINARY.
             10 NI-DRVB0994-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB0994-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB0994-DCL-VIVB0200.
             10 PO-DRVB0994-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB0994-CBA-ANSPR-VOM      PIC  X(10).
             10 PO-DRVB0994-CBA-PART-VBNR      PIC S9(09)       BINARY.
             10 PO-DRVB0994-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB0994-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB0994-CBA-LAST-UPDATE    PIC  X(26).
             10 PO-DRVB0994-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0994-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0994-CBA-PART-BETRAG    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0994-CBA-PART-EH        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0994-CBA-PART-SEIT      PIC  X(10).
             10 PO-DRVB0994-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB0994-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0994   --------------------------*
      *---------------------------------------------------------------*
