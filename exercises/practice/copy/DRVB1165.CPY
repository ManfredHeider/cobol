      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1165.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1165.CPv  $
      *
      *   Rev 2.1   May 02 2017 15:52:26   t13ex18
      *ASL00569: Förderprovision Zugriffsmodul für TBVB0226
      *
      *   Rev 2.0   Apr 28 2017 17:26:08   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1165                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0226                    *
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
      *    FUNKTION DRVB1165 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1165.
          05 PI-DRVB1165-DATEN.
             10 PI-DRVB1165-FPRNERF-ST-NR      PIC S9(09)       BINARY.
             10 PI-DRVB1165-FPRNERF-STICHTAG   PIC  X(10).
 
       01 PO-DRVB1165.
         03 PO-DRVB1165-RC                     PIC  X(03).
             88 PO-DRVB1165-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1165-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1165-DATEN.
          05 PO-DRVB1165-NI-VIVB0226.
             10 NI-DRVB1165-FPRNERF-ST-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1165-FPRNERF-STICHTAG   PIC S9(04)       BINARY.
             10 NI-DRVB1165-FPRNERF-LAST-UPD   PIC S9(04)       BINARY.
             10 NI-DRVB1165-KNR-NERF-GRUND     PIC S9(04)       BINARY.
             10 NI-DRVB1165-KTY-NERF-GRUND     PIC S9(04)       BINARY.
          05 PO-DRVB1165-DCL-VIVB0226.
             10 PO-DRVB1165-FPRNERF-ST-NR      PIC S9(09)       BINARY.
             10 PO-DRVB1165-FPRNERF-STICHTAG   PIC  X(10).
             10 PO-DRVB1165-FPRNERF-LAST-UPD   PIC  X(26).
             10 PO-DRVB1165-KNR-NERF-GRUND     PIC S9(04)       BINARY.
             10 PO-DRVB1165-KTY-NERF-GRUND     PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1165   --------------------------*
      *---------------------------------------------------------------*

