      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2000.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2000.CPv  $
      *
      *   Rev 2.6   02 Feb 2009 11:05:48   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-VER
      *SICHERUNG_SUMME_BKZ
      *
      *   Rev 2.5   28 Feb 2007 14:50:32   t13ex43
      *AKI00556: UPKI0194 für FA wieder auf V 2.5
      *
      *   Rev 2.4   28 Feb 2007 11:22:54   t13ex43
      *AKI00556: UPKI0194 für Prod zurück auf V 2.1
      *
      *   Rev 2.3   31 Jan 2007 11:43:52   t13ex43
      *AKI00556: Erweiterung PO-Bereich um NFD-Daten
      *
      *   Rev 2.2   26 Jan 2007 14:27:44   t13ex43
      *AKI00556: Erweiterung UPKI0194.DRKI2000
      *
      *   Rev 2.1   25 Jan 2007 11:53:16   t13ex43
      *AKI00556: CPY - Datenabgleich HOST-DB <-> Online-DB - Ermitt
      *eln Produkt-ID
      *
      *   Rev 2.0   Nov 23 2006 08:39:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2000                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0194                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.11.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00556!23.11.06!2.1 ! NEU                       ! KLEEMEIER *
260107* AKI00556!26.01.07!2.2 ! TBNAME AUF 30 ERWEITERT   ! KLEEMEIER *
310107* AKI00556!31.01.07!2.3 ! NFD-DATEN HINZU           ! KLEEMEIER *
280207* AKI00556!28.02.07!2.4 ! VERSION 2.1 FÜR PRODUKTION! KLEEMEIER *
280207*         !        !    ! WIEDER HERGESTELLT        !           *
280207* AKI00556!28.02.07!2.5 ! VERSION 2.3 FÜR FA        ! KLEEMEIER *
      * AKI00768!29.12.08!2.6 ! Berechnungskennzeichen    ! I. Frenzel*
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI2000 - ERMITTELN KI-TABELLE                   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI2000.
           05  PI-DRKI2000-DATEN.
               10  PI-DRKI2000-VERTRAGSID        PIC  X(013).
               10  PI-DRKI2000-LFDNR             PIC S9(004) COMP.
      *
       01  PO-DRKI2000.
           05  PO-DRKI2000-RC                    PIC  X(003).
               88  PO-DRKI2000-RC-OK                  VALUE 'OK '.
               88  PO-DRKI2000-RC-NOT-FOUND           VALUE 'NFD'.
               88  PO-DRKI2000-RC-LEER                VALUE 'LER'.
           05  PO-DRKI2000-DATEN.
260107***      10  PO-DRKI2000-TBNAME            PIC  X(018).
260107         10  PO-DRKI2000-TBNAME            PIC  X(030).
               10  PO-DRKI2000-VETPRODUKTID      PIC S9(004) COMP.
               10  PO-DRKI2000-VETVERSSUMME      PIC S9(009)V99 COMP-3.
               10  PO-DRKI2000-VETSUBTYPE        PIC  X(001).
               10  PO-DRKI2000-VETVERSSUMMEWKZ   PIC  X(001).
291208         10  PO-DRKI2000-VETVERSSUMMEBKZ   PIC  X(001).
310107     05  PO-DRKI2000-NFD-DATEN.
310107         10  PO-DRKI2000-TABELLE           PIC  X(030).
310107         10  PO-DRKI2000-TABELLE-ID        PIC  X(013).
      ****
      **** ENDE COPY DRKI2000 ****************************************
      ****
