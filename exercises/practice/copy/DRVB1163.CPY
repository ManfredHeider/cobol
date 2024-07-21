      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1163.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1163.CPv  $
      *
      *   Rev 2.2   May 02 2017 15:55:22   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0225
      *
      *   Rev 2.1   May 02 2017 13:55:26   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0225
      *
      *   Rev 2.0   Apr 28 2017 10:56:30   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1163                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0225                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    28.04.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!28.04.17!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1163 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1163.
          05 PI-DRVB1163-DATEN.
             10 PI-DRVB1163-FPRA-ST-NR         PIC S9(09)       BINARY.
             10 PI-DRVB1163-FPRA-ANSPR-VOM     PIC  X(10).
             10 PI-DRVB1163-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
 
       01 PO-DRVB1163.
         03 PO-DRVB1163-RC                     PIC  X(03).
             88 PO-DRVB1163-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1163-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1163-DATEN.
          05 PO-DRVB1163-NI-VIVB0225.
             10 NI-DRVB1163-FPRA-ST-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1163-FPRA-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB1163-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1163-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1163-FPRA-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1163-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1163-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1163-FPRA-ANSP-BET-FREI PIC S9(04)       BINARY.
             10 NI-DRVB1163-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1163-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1163-FPRA-ANSP-BET-UPFL PIC S9(04)       BINARY.
          05 PO-DRVB1163-DCL-VIVB0225.
             10 PO-DRVB1163-FPRA-ST-NR         PIC S9(09)       BINARY.
             10 PO-DRVB1163-FPRA-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB1163-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1163-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1163-FPRA-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1163-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1163-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1163-FPRA-ANSP-BET-FREI PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1163-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1163-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1163-FPRA-ANSP-BET-UPFL PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1163   --------------------------*
      *---------------------------------------------------------------*
 

