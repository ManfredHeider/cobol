      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0765.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0765.CPv  $
      *
      *   Rev 2.1   Mar 17 2005 11:18:44   t13ex43
      *AKI00479: VB-Programme - Div. Auswert. zu Kd.-bewegungen in 
      *2004 
      *
      *   Rev 2.0   Mar 16 2005 10:29:40   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0765                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0023                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    16.03.2005                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00479!16.03.05!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0765 - ERMITTELN DIFFPROV-SATZ AUS MSBZ/MS    *
      *                        PER STICHTAG                           *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0765.
           05  PI-DRVB0765-DATEN.
               10  PI-DRVB0765-MA-ST-NR          PIC S9(09) COMP.
               10  PI-DRVB0765-STICHTAG-ISO      PIC  X(10).
      *
       01  PO-DRVB0765.
           05  PO-DRVB0765-RC                    PIC  X(03).
               88  PO-DRVB0765-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0765-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0765-DATEN.
               10  PO-DRVB0765-DIFF-PROV-SATZ    PIC S9(03)V99 COMP-3.
      ****
      **** ENDE COPY DRVB0765 ****************************************
      ****
