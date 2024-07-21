      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0966.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0966.CPv  $
      *
      *   Rev 2.1   Mar 18 2010 15:22:40   t13ex43
      *ASL00344: PGM - Ermittlung Columbus-Bonus
      *
      *   Rev 2.0   Feb 19 2010 09:42:02   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0966                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0078                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.02.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00344!19.02.10!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB0966 - ERMITTELN SATZ AUS TABELLE KAND        *
      *                                                               *
      *****************************************************************

       01 PI-DRVB0966.
          05 PI-DRVB0966-DATEN.
             10 PI-DRVB0966-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB0966-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 PI-DRVB0966-KNR-WERTUNGS-TYP   PIC S9(04)       BINARY.
             10 PI-DRVB0966-KAND-NEU-AB        PIC  X(10).

       01 PO-DRVB0966.
         03 PO-DRVB0966-RC                     PIC  X(03).
             88 PO-DRVB0966-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0966-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB0966-DATEN.
          05 PO-DRVB0966-NI-VIVBC078.
             10 NI-DRVB0966-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB0966-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB0966-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 NI-DRVB0966-KTY-WERTUNGS-TYP   PIC S9(04)       BINARY.
             10 NI-DRVB0966-KNR-WERTUNGS-TYP   PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-WERTUNGSSTUFE PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-TAETIG-BIS    PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-DRUCK-KZ      PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-NEU-AB        PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-RUECKERM      PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-INFO          PIC S9(04)       BINARY.
             10 NI-DRVB0966-KAND-START-DATUM   PIC S9(04)       BINARY.
          05 PO-DRVB0966-DCL-VIVBC078.
             10 PO-DRVB0966-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB0966-KTY-WB-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB0966-KNR-WB-TYP         PIC S9(04)       BINARY.
             10 PO-DRVB0966-KTY-WERTUNGS-TYP   PIC S9(04)       BINARY.
             10 PO-DRVB0966-KNR-WERTUNGS-TYP   PIC S9(04)       BINARY.
             10 PO-DRVB0966-KAND-WERTUNGSSTUFE PIC S9(04)       BINARY.
             10 PO-DRVB0966-KAND-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB0966-KAND-TAETIG-BIS    PIC  X(10).
             10 PO-DRVB0966-KAND-DRUCK-KZ      PIC S9(04)       BINARY.
             10 PO-DRVB0966-KAND-NEU-AB        PIC  X(10).
             10 PO-DRVB0966-KAND-RUECKERM      PIC  X(01).
             10 PO-DRVB0966-KAND-INFO          PIC  X(20).
             10 PO-DRVB0966-KAND-START-DATUM   PIC  X(10).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB0966   --------------------------*
      *---------------------------------------------------------------*
