      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0972.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0972.CPv  $
      *
      *   Rev 2.1   Apr 06 2010 13:31:32   t13ex13
      *ASL00344: RGT: SL08
      *
      *   Rev 2.0   Mar 11 2010 10:39:14   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0972                                      *
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
      * ASL00344!11.03.10!2.1 ! NEU                       ! GUTFLEISCH*
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB0972 - ERMITTELN SATZ AUS TABELLE CBA         *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0972.
          05 PI-DRVB0972-DATEN.
             10 PI-DRVB0972-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB0972-CBA-ANSPR-VOM      PIC  X(10).
             10 PI-DRVB0972-CBA-PART-VBNR      PIC S9(09)       BINARY.
             10 PI-DRVB0972-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.

       01 PO-DRVB0972.
         03 PO-DRVB0972-RC                     PIC  X(03).
             88 PO-DRVB0972-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0972-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB0972-DATEN.
          05 PO-DRVB0972-NI-VIVB0200.
             10 NI-DRVB0972-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB0972-CBA-ANSPR-VOM      PIC S9(04)       BINARY.
             10 NI-DRVB0972-CBA-PART-VBNR      PIC S9(04)       BINARY.
             10 NI-DRVB0972-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB0972-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB0972-CBA-LAST-UPDATE    PIC S9(04)       BINARY.
             10 NI-DRVB0972-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0972-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB0972-CBA-PART-BETRAG    PIC S9(04)       BINARY.
             10 NI-DRVB0972-CBA-PART-EH        PIC S9(04)       BINARY.
             10 NI-DRVB0972-CBA-PART-SEIT      PIC S9(04)       BINARY.
             10 NI-DRVB0972-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB0972-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB0972-DCL-VIVB0200.
             10 PO-DRVB0972-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB0972-CBA-ANSPR-VOM      PIC  X(10).
             10 PO-DRVB0972-CBA-PART-VBNR      PIC S9(09)       BINARY.
             10 PO-DRVB0972-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB0972-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB0972-CBA-LAST-UPDATE    PIC  X(26).
             10 PO-DRVB0972-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0972-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0972-CBA-PART-BETRAG    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0972-CBA-PART-EH        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB0972-CBA-PART-SEIT      PIC  X(10).
             10 PO-DRVB0972-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB0972-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0972   --------------------------*
      *---------------------------------------------------------------*
