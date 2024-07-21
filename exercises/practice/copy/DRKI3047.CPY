      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.11.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3047.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3047.CPv  $
      *
      *   Rev 2.1   Apr 28 2016 11:47:50   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)-UPKI0255
      *
      *   Rev 2.0   Nov 25 2015 11:59:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3047                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0203                        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01400!10.01.16!2.1 ! NEU                       ! IFR       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI3047 - KUNDENÜBERTRAGUNG MIT ÄNDERUNG EVM-KZ  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI3047.
           05  PI-DRKI3047-DATEN.
               10  PI-DRKI3047-HOSTVERTRAGID     PIC  X(013).
               10  PI-DRKI3047-VBPERSONENID-ALT  PIC  X(013).
               10  PI-DRKI3047-VBPERSONENID-NEU  PIC  X(013).
               10  PI-DRKI3047-GUELTIG-AB-DAT    PIC  X(010).
               10  PI-DRKI3047-EIGEN-VERM-KZ     PIC  X(001).
               10  PI-DRKI3047-ERSTELLTVON       PIC  X(030).
               10  PI-DRKI3047-AENDGRUND-KNR     PIC S9(004) COMP.
               10  PI-DRKI3047-KKVLID            PIC  X(013).
               10  PI-DRKI3047-EIGEN-VERM-ALT    PIC  X(001).
      *
       01  PO-DRKI3047.
           05  PO-DRKI3047-RC                    PIC  X(003).
               88  PO-DRKI3047-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3047-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI3047-DATEN.
               10  PO-DRKI3047-FEHLER-KNR        PIC S9(004) COMP.

      **** ENDE COPY DRKI3047 ****************************************

