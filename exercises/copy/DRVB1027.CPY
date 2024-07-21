      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1027.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1027.CPv  $
      *
      *   Rev 2.2   Sep 01 2011 11:48:56   t13ae14
      *ASL00390: Videosonderbonus Anpassung PMRW0018
      *
      *   Rev 2.1   Aug 19 2011 14:12:46   t13ae14
      *ASL00390: Videosonderbonus FTP und Load in die Tabelle VIDEO
      *A, Anpassung PMRW0018
      *
      *   Rev 2.0   Aug 12 2011 11:16:40   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB1027                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0210                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    12.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00390!12.08.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1027 - ERMITTELN SATZ AUS TABELLE VIDEOA      *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1027.
          05 PI-DRVB1027-DATEN.
             10 PI-DRVB1027-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1027-VIDEOA-ANSPR-VOM   PIC  X(10).
             10 PI-DRVB1027-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.

       01 PO-DRVB1027.
         03 PO-DRVB1027-RC                     PIC  X(03).
             88 PO-DRVB1027-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1027-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1027-DATEN.
          05 PO-DRVB1027-NI-VIVB0210.
             10 NI-DRVB1027-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1027-VIDEOA-ANSPR-VOM   PIC S9(04)       BINARY.
             10 NI-DRVB1027-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1027-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1027-VIDEOA-LAST-UPDATE PIC S9(04)       BINARY.
             10 NI-DRVB1027-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1027-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1027-VIDEOA-ANSPR-BETRA PIC S9(04)       BINARY.
             10 NI-DRVB1027-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1027-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB1027-DCL-VIVB0210.
             10 PO-DRVB1027-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1027-VIDEOA-ANSPR-VOM   PIC  X(10).
             10 PO-DRVB1027-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1027-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1027-VIDEOA-LAST-UPDATE PIC  X(26).
             10 PO-DRVB1027-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1027-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1027-VIDEOA-ANSPR-BETRA PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1027-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1027-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1027   --------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
