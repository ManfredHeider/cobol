      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2593.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2593.CPv  $
      *
      *   Rev 2.3   Jun 19 2017 11:00:02   t13ex78
      *FKI01546: KÜ/HÜ V2 - EV-KZ pro  Vertrag
      *
      *   Rev 2.2   Jun 06 2017 14:26:38   t13ex78
      *FKI01546: KÜ/HÜ V2 - EV-KZ pro  Vertrag
      *
      *   Rev 2.1   Apr 19 2012 14:46:26   t13ex27
      *AKI01083: Haushalte bei Kundenübertragung mit übertragen
      *
      *   Rev 2.0   Apr 19 2012 14:27:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2593                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0203                        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01083!19.04.12!2.1 ! NEU                       !I. Frenzel *
      * --------------------------------------------------------------*
      * FKI01546!06.06.17!    ! Wartezeit-KZ als neuer    !tfl        *
      *         !        !    ! Ticketwert und Attribut   !           *
      *         !        !    ! in KUNDENUEBERTRAGUNG     !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI2593 - KUNDENÜBERTRAGUNG MIT TICKET-ID        *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI2593.
           05  PI-DRKI2593-DATEN.
               10  PI-DRKI2593-HOSTVERTRAGID     PIC  X(013).
               10  PI-DRKI2593-VBPERSONENID-ALT  PIC  X(013).
               10  PI-DRKI2593-VBPERSONENID-NEU  PIC  X(013).
               10  PI-DRKI2593-GUELTIG-AB-DAT    PIC  X(010).
               10  PI-DRKI2593-EIGEN-VERM-KZ     PIC  X(001).
               10  PI-DRKI2593-ERSTELLTVON       PIC  X(030).
               10  PI-DRKI2593-AENDGRUND-KNR     PIC S9(004) COMP.
               10  PI-DRKI2593-KKVLID            PIC  X(013).
               10  PI-DRKI2593-EIGEN-VERM-ALT    PIC  X(001).
               10  PI-DRKI2593-OTTICKETID        PIC 9(018) COMP-3.
060617         10  PI-DRKI2593-WARTEZEITKZ       PIC X(001).
 
      *
       01  PO-DRKI2593.
           05  PO-DRKI2593-RC                    PIC  X(003).
               88  PO-DRKI2593-RC-OK                  VALUE 'OK '.
               88  PO-DRKI2593-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI2593-DATEN.
               10  PO-DRKI2593-FEHLER-KNR        PIC S9(004) COMP.
      ****
      **** ENDE COPY DRKI2593 ****************************************
      ****
 

