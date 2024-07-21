      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0762.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0762.CPv  $
      *
      *   Rev 2.1   Mar 17 2005 11:18:02   t13ex43
      *AKI00479: VB-Programme - Div. Auswert. zu Kd.-bewegungen in 
      *2004 
      *
      *   Rev 2.0   Mar 15 2005 09:03:32   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0762                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0023                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    15.03.2005                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00479!15.03.05!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0762 - ERMITTELN DIFFPROV-SATZ AUS MSBZ/MS    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0762.
           05  PI-DRVB0762-DATEN.
               10  PI-DRVB0762-MA-ST-NR          PIC S9(09) COMP.
      *
       01  PO-DRVB0762.
           05  PO-DRVB0762-RC                    PIC  X(03).
               88  PO-DRVB0762-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0762-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0762-DATEN.
               10  PO-DRVB0762-DIFF-PROV-SATZ    PIC S9(03)V99 COMP-3.
      ****
      **** ENDE COPY DRVB0762 ****************************************
      ****
