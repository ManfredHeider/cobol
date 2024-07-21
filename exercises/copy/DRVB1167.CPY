      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1167.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1167.CPv  $
      *
      *   Rev 2.2   May 11 2017 15:06:00   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0227
      *
      *   Rev 2.1   May 02 2017 16:03:44   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0227
      *
      *   Rev 2.0   May 02 2017 10:25:14   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1167                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0227                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!02.05.17!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1167 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1167.
          05 PI-DRVB1167-DATEN.
             10 PI-DRVB1167-FPRM-BETR-ST-NR    PIC S9(09)       BINARY.
             10 PI-DRVB1167-FPRM-PART-ST-NR    PIC S9(09)       BINARY.
             10 PI-DRVB1167-FPRM-STICHTAG      PIC  X(10).
             10 PI-DRVB1167-KNR-ABZUG          PIC S9(04)       BINARY.
 
       01 PO-DRVB1167.
         03 PO-DRVB1167-RC                     PIC  X(03).
             88 PO-DRVB1167-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1167-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1167-DATEN.
          05 PO-DRVB1167-NI-VIVB0227.
             10 NI-DRVB1167-FPRM-BETR-ST-NR    PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-PART-ST-NR    PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-STICHTAG      PIC S9(04)       BINARY.
             10 NI-DRVB1167-KNR-ABZUG          PIC S9(04)       BINARY.
             10 NI-DRVB1167-KTY-ABZUG          PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-ANSPR-VOM     PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-BETR-UMSATZ   PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-BETR-UMS-MAX  PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-PART-UMSATZ   PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-PART-ABZUG    PIC S9(04)       BINARY.
             10 NI-DRVB1167-FPRM-LAST-UPDATE   PIC S9(04)       BINARY.
          05 PO-DRVB1167-DCL-VIVB0227.
             10 PO-DRVB1167-FPRM-BETR-ST-NR    PIC S9(09)       BINARY.
             10 PO-DRVB1167-FPRM-PART-ST-NR    PIC S9(09)       BINARY.
             10 PO-DRVB1167-FPRM-STICHTAG      PIC  X(10).
             10 PO-DRVB1167-KNR-ABZUG          PIC S9(04)       BINARY.
             10 PO-DRVB1167-KTY-ABZUG          PIC S9(04)       BINARY.
             10 PO-DRVB1167-FPRM-ANSPR-VOM     PIC  X(10).
             10 PO-DRVB1167-FPRM-BETR-UMSATZ   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1167-FPRM-BETR-UMS-MAX
                                               PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1167-FPRM-PART-UMSATZ   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1167-FPRM-PART-ABZUG    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1167-FPRM-LAST-UPDATE   PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1167   --------------------------*
      *---------------------------------------------------------------*
 

