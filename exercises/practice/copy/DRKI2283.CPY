      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2283.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2283.CPv  $
      *
      *   Rev 2.1   21 Apr 2009 12:15:50   t13ex27
      *FKI00911: Verträge löschen-DB2
      *
      *   Rev 2.0   20 Apr 2009 17:16:54   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2283                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das List-OZP ZXKI0330                    *
      *    Lebenteilschicht                                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Pohl                                          *
      *    DATUM:    04.02.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00911!17.04.09!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI2283                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI2283.
          05 PI-DRKI2283-PROGRAMM               PIC X(008).
          05 PI-DRKI2283-UFKT-CODE              PIC 9(002).
          05 PI-DRKI2283-DATEN.
             10 PI-DRKI2283-LTSVERTRAGSID       PIC X(013).
      *
       01 PO-DRKI2283.

          05 PO-DRKI2283-RC                     PIC  X(003).
             88 PO-DRKI2283-RC-OK                    VALUE 'OK '.
             88 PO-DRKI2283-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI2283-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI2283-DATEN.
            10 PO-DRKI2283-LTSVERSTARLFDNR      PIC S9(4) COMP.
            10 PO-DRKI2283-LTSZAHLDYNERHOEH     PIC S9(4) COMP.
            10 PO-DRKI2283-LTSTTNR              PIC S9(4) COMP.
      *
      ***** Ende Copy-Member DRKI2283 *****************************
