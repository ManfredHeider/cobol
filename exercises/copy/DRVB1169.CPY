      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1169.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1169.CPv  $
      *
      *   Rev 2.1   May 02 2017 16:28:58   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0228
      *
      *   Rev 2.0   May 02 2017 11:00:20   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1169                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0228                    *
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
      *    FUNKTION DRVB1169 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1169.
          05 PI-DRVB1169-DATEN.
             10 PI-DRVB1169-FPRDD-ST-NR        PIC S9(09)       BINARY.
             10 PI-DRVB1169-FPRDD-STICHTAG     PIC  X(10).
             10 PI-DRVB1169-FPRDD-ANSPR-MONAT  PIC  X(10).
 
       01 PO-DRVB1169.
         03 PO-DRVB1169-RC                     PIC  X(03).
             88 PO-DRVB1169-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1169-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1169-DATEN.
          05 PO-DRVB1169-NI-VIVB0228.
             10 NI-DRVB1169-FPRDD-ST-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-STICHTAG     PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ANSPR-MONAT  PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ANSPR-VOM    PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-LAST-UPDATE  PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-MS-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-12M-UMS-FREI PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-12M-EQF      PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-03M-UMS-FREI PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-PRO-ABZUG-FR PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-N-PRO-ABZ-FR PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ZW-BETRAG    PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-EQF-ABZUG-FR PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-BETRAG-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-FAKTOR       PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ANZ-MONATE   PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-UMS-BED-ERF  PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-S-AUSZAHLUNG PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-S-NACH-DIR   PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-12M-UMS-UPFL PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-03M-UMS-UPFL PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-PRO-ABZUG-UP PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-N-PRO-ABZ-UP PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ZW-BETRAG-UP PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-EQF-ABZUG-UP PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-BETRAG-UPFL  PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ABZUG-NEG    PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-03M-KORR-FRE PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-03M-KORR-UPF PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ANTEIL-FREI  PIC S9(04)       BINARY.
             10 NI-DRVB1169-FPRDD-ANTEIL-UPFL  PIC S9(04)       BINARY.
          05 PO-DRVB1169-DCL-VIVB0228.
             10 PO-DRVB1169-FPRDD-ST-NR        PIC S9(09)       BINARY.
             10 PO-DRVB1169-FPRDD-STICHTAG     PIC  X(10).
             10 PO-DRVB1169-FPRDD-ANSPR-MONAT  PIC  X(10).
             10 PO-DRVB1169-FPRDD-ANSPR-VOM    PIC  X(10).
             10 PO-DRVB1169-FPRDD-LAST-UPDATE  PIC  X(26).
             10 PO-DRVB1169-FPRDD-MS-NR        PIC S9(04)       BINARY.
             10 PO-DRVB1169-FPRDD-12M-UMS-FREI PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-12M-EQF      PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-03M-UMS-FREI PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-PRO-ABZUG-FR PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-N-PRO-ABZ-FR PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-ZW-BETRAG    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-EQF-ABZUG-FR PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-BETRAG-FREI  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-FAKTOR       PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-ANZ-MONATE   PIC S9(04)       BINARY.
             10 PO-DRVB1169-FPRDD-UMS-BED-ERF  PIC  X(01).
             10 PO-DRVB1169-FPRDD-S-AUSZAHLUNG PIC  X(01).
             10 PO-DRVB1169-FPRDD-S-NACH-DIR   PIC  X(01).
             10 PO-DRVB1169-FPRDD-12M-UMS-UPFL PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-03M-UMS-UPFL PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-PRO-ABZUG-UP PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-N-PRO-ABZ-UP PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-ZW-BETRAG-UP PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-EQF-ABZUG-UP PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-BETRAG-UPFL  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-ABZUG-NEG    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-03M-KORR-FRE PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-03M-KORR-UPF PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1169-FPRDD-ANTEIL-FREI  PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1169-FPRDD-ANTEIL-UPFL  PIC S9(03)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1169   --------------------------*
      *---------------------------------------------------------------*

