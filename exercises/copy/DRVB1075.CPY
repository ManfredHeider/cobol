      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1075.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1075.CPv  $
      *
      *   Rev 2.1   Feb 26 2015 15:40:10   t13ae35
      *ARW00102: Ausbilderzuschuß monatlich automatisch verbuchen -
      * Zugriffsmodule
      *
      *   Rev 2.0   Jan 29 2015 16:11:30   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1075                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0070                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    26.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ARW00102!26.02.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1075 - ERMITTELN SATZ AUS TABELLE AUSBIF      *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1075.
          05 PI-DRVB1075-DATEN.
             10 PI-DRVB1075-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1075-AUSBIF-AZUBI       PIC S9(09)       BINARY.
             10 PI-DRVB1075-KNR-AUSBIF-TYP     PIC S9(04)       BINARY.
             10 PI-DRVB1075-AUSBIF-DAT-ANSPRU  PIC  X(10).
 
       01 PO-DRVB1075.
         03 PO-DRVB1075-RC                     PIC  X(03).
             88 PO-DRVB1075-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1075-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1075-DATEN.
          05 PO-DRVB1075-NI-VIVB0070.
             10 NI-DRVB1075-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1075-AUSBIF-AZUBI       PIC S9(04)       BINARY.
             10 NI-DRVB1075-KTY-AUSBIF-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1075-KNR-AUSBIF-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1075-AUSBIF-DAT-ANSPRU  PIC S9(04)       BINARY.
             10 NI-DRVB1075-AUSBIF-DAT-VON     PIC S9(04)       BINARY.
             10 NI-DRVB1075-AUSBIF-DAT-BIS     PIC S9(04)       BINARY.
             10 NI-DRVB1075-AUSBIF-LAST-UPDATE PIC S9(04)       BINARY.
             10 NI-DRVB1075-AUSBIF-ANSPR-BETR  PIC S9(04)       BINARY.
             10 NI-DRVB1075-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1075-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1075-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1075-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB1075-DCL-VIVB0070.
             10 PO-DRVB1075-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1075-AUSBIF-AZUBI       PIC S9(09)       BINARY.
             10 PO-DRVB1075-KTY-AUSBIF-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1075-KNR-AUSBIF-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1075-AUSBIF-DAT-ANSPRU  PIC  X(10).
             10 PO-DRVB1075-AUSBIF-DAT-VON     PIC  X(10).
             10 PO-DRVB1075-AUSBIF-DAT-BIS     PIC  X(10).
             10 PO-DRVB1075-AUSBIF-LAST-UPDATE PIC  X(26).
             10 PO-DRVB1075-AUSBIF-ANSPR-BETR  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1075-KTY-WAEHRUNG       PIC S9(09)       BINARY.
             10 PO-DRVB1075-KNR-WAEHRUNG       PIC S9(09)       BINARY.
             10 PO-DRVB1075-KTY-ANSPR-STATUS   PIC S9(09)       BINARY.
             10 PO-DRVB1075-KNR-ANSPR-STATUS   PIC S9(09)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1075   --------------------------*
      *---------------------------------------------------------------*

