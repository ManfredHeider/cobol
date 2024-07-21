      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2024.CPv  $
      *
      *   Rev 2.1   27 Mar 2007 10:37:08   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - Programme
      *
      *   Rev 2.0   Feb 12 2007 17:07:14   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2024                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0196                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    13.02.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00556!13.02.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI2024 - ABSCHLUSSVERMITTLER VERTRAG            *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI2024.
           05  PI-DRKI2024-DATEN.
               10  PI-DRKI2024-OTTICKETID        PIC  9(018).
               10  PI-DRKI2024-OTHOSTVERTRAGSID  PIC  X(013).
      *
       01  PO-DRKI2024.
           05  PO-DRKI2024-RC                    PIC  X(003).
               88  PO-DRKI2024-RC-OK                  VALUE 'OK '.
               88  PO-DRKI2024-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI2024-DATEN.
               10  PO-DRKI2024-OTFEHLER-KNR      PIC S9(004) COMP.
      ****
      **** ENDE COPY DRKI2024 ****************************************
      ****
