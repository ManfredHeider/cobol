      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0684.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0684.CPv  $
      *
      *   Rev 2.1   Mar 17 2005 11:17:34   t13ex43
      *AKI00479: VB-Programme - Div. Auswert. zu Kd.-bewegungen in 
      *2004 
      *
      *   Rev 2.0   Mar 15 2005 09:02:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0684                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0133                        *
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
      *    FUNKTION DRVB0684 - ERMITTELN DIFFERENZ-PROVISIONSSATZ     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0684.
           05  PI-DRVB0684-DATEN.
               10  PI-DRVB0684-MA-ST-NR          PIC S9(09) COMP.
      *
       01  PO-DRVB0684.
           05  PO-DRVB0684-RC                    PIC  X(03).
               88  PO-DRVB0684-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0684-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0684-DATEN.
               10  PO-DRVB0684-DIFF-PROV-SATZ    PIC S9(03)V99 COMP-3.
      ****
      **** ENDE COPY DRVB0684 ****************************************
      ****
