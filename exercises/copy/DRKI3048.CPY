      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.11.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3048.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI3048.CPv  $
      *
      *   Rev 2.8   Jan 27 2017 11:18:06   t13ex78
      *AKI01481: Änderungen UPKI0255 und andere
      *
      *   Rev 2.7   Jan 06 2017 09:55:06   t13ex78
      *AKI01481: UPKI0199
      *
      *   Rev 2.6   Jan 06 2017 08:27:52   t13ex78
      *AKI01481: Änderungen UPKI0255 und andere
      *
      *   Rev 2.5   Dec 22 2016 12:27:08   t13ex78
      *AKI01481: Änderungen UPKI0255 und andere
      *
      *   Rev 2.4   Dec 20 2016 10:59:00   t13ex78
      *AKI01481: Änderungen UPKI0255 und andere
      *
      *   Rev 2.3   Dec 19 2016 16:39:50   t13ex78
      *AKI01481: Änderungen UPKI0255 und andere
      *
      *   Rev 2.2   Dec 19 2016 16:25:36   t13ex78
      *AKI01481: Änderungen UPKI0255 und andere
      *
      *   Rev 2.1   Apr 28 2016 11:48:00   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)-UPKI0255
      *
      *   Rev 2.0   Nov 25 2015 11:59:14   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3048                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0255                        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01400!10.01.16!2.1 ! NEU                       ! IFR       *
      * --------------------------------------------------------------*
151216* AKI01481!15.12.16!    !Wartezeit-KZ als neuer     ! tfl       *
      *         !        !    !Ticketwert und Attribut    !           *
      *         !        !    !in KUNDENUEBERTRAGUNG      !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI3048 - KUNDENÜBERTRAGUNG MIT ÄNDERUNG EVM-KZ  *
      *    MIT TICKET-ID                                              *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI3048.
           05  PI-DRKI3048-DATEN.
               10  PI-DRKI3048-HOSTVERTRAGID     PIC  X(013).
               10  PI-DRKI3048-VBPERSONENID-ALT  PIC  X(013).
               10  PI-DRKI3048-VBPERSONENID-NEU  PIC  X(013).
               10  PI-DRKI3048-GUELTIG-AB-DAT    PIC  X(010).
               10  PI-DRKI3048-EIGEN-VERM-KZ     PIC  X(001).
               10  PI-DRKI3048-ERSTELLTVON       PIC  X(030).
               10  PI-DRKI3048-AENDGRUND-KNR     PIC S9(004) COMP.
               10  PI-DRKI3048-KKVLID            PIC  X(013).
               10  PI-DRKI3048-EIGEN-VERM-ALT    PIC  X(001).
               10  PI-DRKI3048-OTTICKETID        PIC 9(018) COMP-3.
151216         10  PI-DRKI3048-WARTEZEITKZ       PIC  X(001).
 
      *
       01  PO-DRKI3048.
           05  PO-DRKI3048-RC                    PIC  X(003).
               88  PO-DRKI3048-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3048-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI3048-DATEN.
               10  PO-DRKI3048-FEHLER-KNR        PIC S9(004) COMP.
 
      **** ENDE COPY DRKI3048 ****************************************
 
 
 
 
 
 
 

