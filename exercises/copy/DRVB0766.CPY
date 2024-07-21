      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0766.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0766.CPv  $
      *
      *   Rev 2.1   Mar 17 2005 11:18:58   t13ex43
      *AKI00479: VB-Programme - Div. Auswert. zu Kd.-bewegungen in 
      *2004 
      *
      *   Rev 2.0   Mar 16 2005 10:30:02   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0766                                      *
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
      *    FUNKTION DRVB0766 - ERMITTELN DIFFPROV-SATZ AUS VBPZ/VBP   *
      *                        PER STICHTAG                           *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0766.
           05  PI-DRVB0766-DATEN.
               10  PI-DRVB0766-MA-ST-NR          PIC S9(09) COMP.
               10  PI-DRVB0766-STICHTAG-ISO      PIC  X(10).
      *
       01  PO-DRVB0766.
           05  PO-DRVB0766-RC                    PIC  X(03).
               88  PO-DRVB0766-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0766-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0766-DATEN.
               10  PO-DRVB0766-DIFF-PROV-SATZ    PIC S9(03)V99 COMP-3.
      ****
      **** ENDE COPY DRVB0766 ****************************************
      ****
