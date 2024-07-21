      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1079.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1079.CPv  $
      *
      *   Rev 2.1   Mar 02 2015 12:42:36   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda 
      *Zugriffsmodule Tabelle AZA (TBVB0222)
      *
      *   Rev 2.0   Mar 02 2015 10:43:16   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1079                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0222                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    27.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00530!27.02.15!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1079 - ERMITTELN SATZ AUS TABELLE AZA         *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1079.
          05 PI-DRVB1079-DATEN.
             10 PI-DRVB1079-AZA-BETR-VBNR      PIC S9(09)       BINARY.
             10 PI-DRVB1079-AZA-ANSPR-MONAT    PIC  X(10).
             10 PI-DRVB1079-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
 
       01 PO-DRVB1079.
         03 PO-DRVB1079-RC                     PIC  X(03).
             88 PO-DRVB1079-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1079-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1079-DATEN.
          05 PO-DRVB1079-NI-VIVB0222.
             10 NI-DRVB1079-AZA-BETR-VBNR      PIC S9(04)       BINARY.
             10 NI-DRVB1079-AZA-ANSPR-MONAT    PIC S9(04)       BINARY.
             10 NI-DRVB1079-AZA-BETRAG         PIC S9(04)       BINARY.
             10 NI-DRVB1079-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1079-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1079-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1079-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1079-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1079-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1079-AZA-LAST-UPDATE    PIC S9(04)       BINARY.
          05 PO-DRVB1079-DCL-VIVB0222.
             10 PO-DRVB1079-AZA-BETR-VBNR      PIC S9(09)       BINARY.
             10 PO-DRVB1079-AZA-ANSPR-MONAT    PIC  X(10).
             10 PO-DRVB1079-AZA-BETRAG         PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1079-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1079-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1079-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1079-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1079-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1079-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1079-AZA-LAST-UPDATE    PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1079   --------------------------*
      *---------------------------------------------------------------*

