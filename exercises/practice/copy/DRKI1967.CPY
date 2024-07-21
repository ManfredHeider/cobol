      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1967.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1967.CPv  $
      *
      *   Rev 2.4   09 May 2007 15:30:14   t13ex43
      *AKI00556: Steuersätze KD-Zusammenf. erweitern
      *
      *   Rev 2.3   20 Apr 2007 11:30:40   t13ex43
      *AKI00556: Steuersätze KD-Zusammenf. erweitern
      *
      *   Rev 2.2   06 Feb 2007 09:23:30   t13ex43
      *AKI00556: Erweiterung PO-Bereich um NFD-Daten
      *
      *   Rev 2.1   19 Jan 2007 09:48:52   t13ex43
      *AKI00556: Steuersätze für Kundenzusammenführung
      *
      *   Rev 2.0   Oct 09 2006 13:52:54   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1967                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0199                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.12.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00556!08.12.06!2.1 ! NEU                       ! KLEEMEIER *
050207* AKI00556!05.02.07!2.2 ! NFD-DATEN HINZU           ! KLEEMEIER *
200407* AKI00556!20.04.07!2.3 ! TICKET-ID IN STINFO HINZU ! KLEEMEIER *
090507* AKI00556!09.05.07!2.4 ! NEUE LÄNGE VON STINFO     ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI1967 - STEUERSÄTZE FÜR KUNDENZUSAMMENFÜHRUNG  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI1967.
           05  PI-DRKI1967-DATEN.
               10  PI-DRKI1967-OTTICKETID        PIC S9(018)V COMP-3.
      *
       01  PO-DRKI1967.
           05  PO-DRKI1967-RC                    PIC  X(003).
               88  PO-DRKI1967-RC-OK                  VALUE 'OK '.
               88  PO-DRKI1967-RC-NOT-FOUND           VALUE 'NFD'.
               88  PO-DRKI1967-RC-LEER                VALUE 'LER'.
               88  PO-DRKI1967-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI1967-DATEN.
200407****     10  PO-DRKI1967-STINFO-KDZUF      PIC  X(089).
090507****     10  PO-DRKI1967-STINFO-KDZUF      PIC  X(108).
090507         10  PO-DRKI1967-STINFO-KDZUF      PIC  X(105).
050207     05  PO-DRKI1967-NFD-DATEN.
050207         10  PO-DRKI1967-TABELLE           PIC  X(030).
050207         10  PO-DRKI1967-TABELLE-ID        PIC  X(013).
      ****
      **** ENDE COPY DRKI1967 ****************************************
      ****
