      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.04.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1190.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1190.CPv  $
      *
      *   Rev 2.1   Jun 19 2018 13:46:20   T13EX18
      *ASL00583: Einührung Allfinanz AG in VB-System / Geldleistung
      *en Zugriffsmodul FPRC (GBU)
      *
      *   Rev 2.0   Apr 13 2018 17:43:22   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1190                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0230                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    13.04.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00583!13.04.18!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1190 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1190.
          05 PI-DRVB1190-DATEN.
             10 PI-DRVB1190-FPRC-ST-NR         PIC S9(09)       BINARY.
             10 PI-DRVB1190-FPRC-STICHTAG      PIC  X(10).
 
       01 PO-DRVB1190.
         03 PO-DRVB1190-RC                     PIC  X(03).
             88 PO-DRVB1190-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1190-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1190-DATEN.
          05 PO-DRVB1190-NI-VIVB0230.
             10 NI-DRVB1190-FPRC-ST-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-STICHTAG      PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-MS-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-12M-UMS-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-12M-EQF       PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-03M-UMS-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-EQF-ABZ-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-FAKTOR        PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-ANZ-MONATE    PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-12M-UMS-UPFL  PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-03M-UMS-UPFL  PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-EQF-ABZ-UPFL  PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-EH-ABZ-NEG    PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-03M-KORR-FREI PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-03M-KORR-UPFL PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-EH-ANT-FREI   PIC S9(04)       BINARY.
             10 NI-DRVB1190-FPRC-EH-ANT-UPFL   PIC S9(04)       BINARY.
          05 PO-DRVB1190-DCL-VIVB0230.
             10 PO-DRVB1190-FPRC-ST-NR         PIC S9(09)       BINARY.
             10 PO-DRVB1190-FPRC-STICHTAG      PIC  X(10).
             10 PO-DRVB1190-FPRC-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB1190-FPRC-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1190-FPRC-MS-NR         PIC S9(04)       BINARY.
             10 PO-DRVB1190-FPRC-12M-UMS-FREI  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1190-FPRC-12M-EQF       PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1190-FPRC-03M-UMS-FREI  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1190-FPRC-EQF-ABZ-FREI  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1190-FPRC-FAKTOR        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1190-FPRC-ANZ-MONATE    PIC S9(04)       BINARY.
             10 PO-DRVB1190-FPRC-12M-UMS-UPFL  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1190-FPRC-03M-UMS-UPFL  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1190-FPRC-EQF-ABZ-UPFL  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1190-FPRC-EH-ABZ-NEG    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1190-FPRC-03M-KORR-FREI PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1190-FPRC-03M-KORR-UPFL PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1190-FPRC-EH-ANT-FREI   PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1190-FPRC-EH-ANT-UPFL   PIC S9(03)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1190   --------------------------*
      *---------------------------------------------------------------*

