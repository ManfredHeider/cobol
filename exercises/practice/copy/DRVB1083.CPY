      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1083.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1083.CPv  $
      *
      *   Rev 2.1   Mar 04 2015 13:23:46   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda 
      *Zugriffsmodule Tabelle ECBA (TBVB0223)
      *
      *   Rev 2.0   Mar 03 2015 18:15:40   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1083                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0223                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    03.03.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00530!03.03.15!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1083 - ERMITTELN SATZ AUS TABELLE ECBA        *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1083.
          05 PI-DRVB1083-DATEN.
             10 PI-DRVB1083-ECBA-ST-NR         PIC S9(09)       BINARY.
             10 PI-DRVB1083-ECBA-ANSPR-VOM     PIC  X(10).
             10 PI-DRVB1083-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
 
       01 PO-DRVB1083.
         03 PO-DRVB1083-RC                     PIC  X(03).
             88 PO-DRVB1083-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1083-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1083-DATEN.
          05 PO-DRVB1083-NI-VIVB0223.
             10 NI-DRVB1083-ECBA-ST-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1083-ECBA-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB1083-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1083-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1083-ECBA-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1083-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1083-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1083-ECBA-PART-EH       PIC S9(04)       BINARY.
             10 NI-DRVB1083-ECBA-FAKTOR        PIC S9(04)       BINARY.
             10 NI-DRVB1083-ECBA-ANSPR-BETRAG  PIC S9(04)       BINARY.
             10 NI-DRVB1083-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1083-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB1083-DCL-VIVB0223.
             10 PO-DRVB1083-ECBA-ST-NR         PIC S9(09)       BINARY.
             10 PO-DRVB1083-ECBA-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB1083-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1083-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1083-ECBA-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1083-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1083-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1083-ECBA-PART-EH       PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1083-ECBA-FAKTOR        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1083-ECBA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1083-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1083-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1083   --------------------------*
      *---------------------------------------------------------------*

