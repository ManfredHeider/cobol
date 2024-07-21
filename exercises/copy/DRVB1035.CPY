      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1035.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRVB1035.CPv  $
      *
      *   Rev 2.6   Feb 01 2012 15:42:40   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.5   Jan 16 2012 14:51:30   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.4   Jan 16 2012 14:38:12   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.3   Jan 16 2012 14:29:28   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.2   Jan 16 2012 14:14:44   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.1   Jan 16 2012 14:05:06   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.0   Dec 13 2011 13:14:28   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB1035                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0214                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    13.12.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00420!13.12.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1035 - ERMITTELN SATZ AUS TABELLE PABOA       *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1035.
          05 PI-DRVB1035-DATEN.
             10 PI-DRVB1035-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1035-PABOA-UNTST        PIC S9(09)       BINARY.
             10 PI-DRVB1035-PABOA-ANSPR-VOM    PIC  X(10).
             10 PI-DRVB1035-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.

       01 PO-DRVB1035.
         03 PO-DRVB1035-RC                     PIC  X(03).
             88 PO-DRVB1035-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1035-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1035-DATEN.
          05 PO-DRVB1035-NI-VIVB0214.
             10 NI-DRVB1035-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1035-PABOA-UNTST        PIC S9(04)       BINARY.
             10 NI-DRVB1035-PABOA-ANSPR-VOM    PIC S9(04)       BINARY.
             10 NI-DRVB1035-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1035-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1035-PABA-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1035-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1035-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1035-PABA-ANSP-BETRAG   PIC S9(04)       BINARY.
             10 NI-DRVB1035-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1035-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB1035-DCL-VIVB0214.
             10 PO-DRVB1035-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1035-PABOA-UNTST        PIC S9(09)       BINARY.
             10 PO-DRVB1035-PABOA-ANSPR-VOM    PIC  X(10).
             10 PO-DRVB1035-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1035-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1035-PABA-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1035-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1035-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1035-PABA-ANSP-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1035-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1035-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1035   --------------------------*
      *---------------------------------------------------------------*
