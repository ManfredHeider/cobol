      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3076.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3076.CPv  $
      *
      *   Rev 2.1   Jun 10 2016 12:56:58   t13ex73
      *FKI01518: Einpflegen von Beziehungen im UPKI0224 prüfen
      *
      *   Rev 2.0   Apr 29 2016 12:27:14   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3076                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0256                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: DDY                                              *
      *    DATUM:    02.05.2016                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * FKI01518!02.05.16!2.0 ! NEU                       ! DDY       *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI3076 - Kundenbeziehungen Pflegen              *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI3076.
           05  PI-DRKI3076-DATEN.
               10  PI-DRKI3076-PERSONENID        PIC  X(013).
               10  PI-DRKI3076-PARTGESELLSCHID   PIC S9(004) COMP.
               10  PI-DRKI3076-HOSTVERTRAGID     PIC  X(013).
               10  PI-DRKI3076-PGMNAME           PIC  X(008).
      *
       01  PO-DRKI3076.
           05  PO-DRKI3076-RC                    PIC  X(003).
               88  PO-DRKI3076-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3076-RC-NOK                 VALUE 'NOK'.
               88  PO-DRKI3076-RC-KND-UNBETREUT       VALUE 'KUB'.
               88  PO-DRKI3076-RC-QUELL-VB-NFD        VALUE 'QVB'.
               88  PO-DRKI3076-RC-ZIEL-VB-NFD         VALUE 'ZVB'.
               88  PO-DRKI3076-RC-KND-KEINE-VER       VALUE 'KVE'.
               88  PO-DRKI3076-RC-KND-NFD             VALUE 'KNF'.
               88  PO-DRKI3076-RC-KND-KEINE-KVB       VALUE 'KVB'.
           05  PO-DRKI3076-DATEN.
               10  PO-DRKI3076-ANZ-KVB-DEL       PIC S9(004) COMP-3.
               10  PO-DRKI3076-ANZ-KVB-INS       PIC S9(004) COMP-3.
      ****
      **** ENDE COPY DRKI3076 ****************************************
      ****

