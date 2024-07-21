      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1989.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1989.CPv  $
      *
      *   Rev 2.5   Dec 02 2013 10:51:50   t13ex27
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen-Abgleich
      *
      *   Rev 2.4   28 Feb 2007 14:50:14   t13ex43
      *AKI00556: UPKI0194 für FA wieder auf V 2.5
      *
      *   Rev 2.3   28 Feb 2007 11:22:38   t13ex43
      *AKI00556: UPKI0194 für Prod zurück auf V 2.1
      *
      *   Rev 2.2   31 Jan 2007 11:43:30   t13ex43
      *AKI00556: Erweiterung PO-Bereich um NFD-Daten
      *
      *   Rev 2.1   Jan 08 2007 14:25:16   t13ex43
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Ermitteln Pr
      *odukt-ID
      *
      *   Rev 2.0   Nov 08 2006 10:58:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1989                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0194                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    09.11.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00556!09.11.06!2.1 ! NEU                       ! KLEEMEIER *
310107* AKI00556!31.01.07!2.2 ! NFD-DATEN HINZU, TBNAME 30! KLEEMEIER *
280207* AKI00556!28.02.07!2.3 ! VERSION 2.1 FÜR PRODUKTION! KLEEMEIER *
280207*         !        !    ! WIEDER HERGESTELLT        !           *
280207* AKI00556!28.02.07!2.4 ! VERSION 2.2 FÜR FA        ! KLEEMEIER *
      * FKI01434!18.10.13!2.5 ! Obejektid entfernt        ! Frenzel   *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI1989 - ERMITTELN PRODUKT-ID                   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI1989.
           05  PI-DRKI1989-DATEN.
310107****     10  PI-DRKI1989-TBNAME            PIC  X(018).
310107         10  PI-DRKI1989-TBNAME            PIC  X(030).
               10  PI-DRKI1989-VERTRAGSID        PIC  X(013).
               10  PI-DRKI1989-LFDNR             PIC S9(004) COMP.
      *
       01  PO-DRKI1989.
           05  PO-DRKI1989-RC                    PIC  X(003).
               88  PO-DRKI1989-RC-OK                  VALUE 'OK '.
               88  PO-DRKI1989-RC-NOT-FOUND           VALUE 'NFD'.
               88  PO-DRKI1989-RC-LEER                VALUE 'LER'.
           05  PO-DRKI1989-DATEN.
               10  PO-DRKI1989-VETPRODUKTID      PIC S9(004) COMP.
               10  PO-DRKI1989-VETVERSSUMME      PIC S9(009)V99 COMP-3.
               10  PO-DRKI1989-VETVERTRAGSID     PIC  X(013).
               10  PO-DRKI1989-VETVERSTARLFDNR   PIC S9(004) COMP.
310107     05  PO-DRKI1989-NFD-DATEN.
310107         10  PO-DRKI1989-TABELLE           PIC  X(030).
310107         10  PO-DRKI1989-TABELLE-ID        PIC  X(013).
      ****
      **** ENDE COPY DRKI1989 ****************************************
      ****
