      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3380.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3380.CPv  $
      *
      *   Rev 2.2   Aug 27 2018 10:17:32   t13ex78
      *AKI01598: Mandantenprüfung und erw. Protokoll
      *
      *   Rev 2.1   Aug 24 2018 13:51:14   t13ex78
      *AKI01598: Mandantenprüfung und erw. Protokoll
      *
      *   Rev 2.0   Aug 21 2018 09:43:36   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3380                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0255                        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01598!21.08.18!2.1 ! Neuerstellung             ! tfl       *
      * --------+--------+----+---------------------------+-----------*
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI3380 - KUNDENÜBERTRAGUNG                      *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI3380.
           05  PI-DRKI3380-DATEN.
               10  PI-DRKI3380-HOSTVERTRAGID     PIC  X(013).
               10  PI-DRKI3380-VBPERSONENID-ALT  PIC  X(013).
               10  PI-DRKI3380-VBPERSONENID-NEU  PIC  X(013).
               10  PI-DRKI3380-GUELTIG-AB-DAT    PIC  X(010).
               10  PI-DRKI3380-EIGEN-VERM-KZ     PIC  X(001).
               10  PI-DRKI3380-ERSTELLTVON       PIC  X(030).
               10  PI-DRKI3380-AENDGRUND-KNR     PIC S9(004) COMP.
               10  PI-DRKI3380-KKVLID            PIC  X(013).
               10  PI-DRKI3380-EIGEN-VERM-ALT    PIC  X(001).
               10  PI-DRKI3380-DTAERLEDIGTKZ     PIC  X(001).
      *
       01  PO-DRKI3380.
           05  PO-DRKI3380-RC                    PIC  X(003).
               88  PO-DRKI3380-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3380-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI3380-DATEN.
               10  PO-DRKI3380-FEHLER-KNR        PIC S9(004) COMP.
      ****
      **** ENDE COPY DRKI3380 ****************************************
      ****
 

