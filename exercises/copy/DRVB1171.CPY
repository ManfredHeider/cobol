      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1171.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1171.CPv  $
      *
      *   Rev 2.1   May 02 2017 16:33:48   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0229
      *
      *   Rev 2.0   May 02 2017 10:58:54   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1171                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0229                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!02.05.17!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1171 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1171.
          05 PI-DRVB1171-DATEN.
             10 PI-DRVB1171-FPRB-ST-NR         PIC S9(09)       BINARY.
             10 PI-DRVB1171-FPRB-STICHTAG      PIC  X(10).
 
       01 PO-DRVB1171.
         03 PO-DRVB1171-RC                     PIC  X(03).
             88 PO-DRVB1171-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1171-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1171-DATEN.
          05 PO-DRVB1171-NI-VIVB0229.
             10 NI-DRVB1171-FPRB-ST-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-STICHTAG      PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-MS-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-12M-UMS-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-12M-EQF       PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-03M-UMS-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-EQF-ABZ-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-FAKTOR        PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-ANZ-MONATE    PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-12M-UMS-UPFL  PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-03M-UMS-UPFL  PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-EQF-ABZ-UPFL  PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-EH-ABZ-NEG    PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-03M-KORR-FREI PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-03M-KORR-UPFL PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-EH-ANT-FREI   PIC S9(04)       BINARY.
             10 NI-DRVB1171-FPRB-EH-ANT-UPFL   PIC S9(04)       BINARY.
          05 PO-DRVB1171-DCL-VIVB0229.
             10 PO-DRVB1171-FPRB-ST-NR         PIC S9(09)       BINARY.
             10 PO-DRVB1171-FPRB-STICHTAG      PIC  X(10).
             10 PO-DRVB1171-FPRB-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB1171-FPRB-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1171-FPRB-MS-NR         PIC S9(04)       BINARY.
             10 PO-DRVB1171-FPRB-12M-UMS-FREI  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1171-FPRB-12M-EQF       PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1171-FPRB-03M-UMS-FREI  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1171-FPRB-EQF-ABZ-FREI  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1171-FPRB-FAKTOR        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1171-FPRB-ANZ-MONATE    PIC S9(04)       BINARY.
             10 PO-DRVB1171-FPRB-12M-UMS-UPFL  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1171-FPRB-03M-UMS-UPFL  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1171-FPRB-EQF-ABZ-UPFL  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1171-FPRB-EH-ABZ-NEG    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1171-FPRB-03M-KORR-FREI PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1171-FPRB-03M-KORR-UPFL PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1171-FPRB-EH-ANT-FREI   PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1171-FPRB-EH-ANT-UPFL   PIC S9(03)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1171   --------------------------*
      *---------------------------------------------------------------*

