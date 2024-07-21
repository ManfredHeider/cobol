      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0943.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRPA0943.CPv  $
      *
      *   Rev 2.3   Jun 25 2003 09:56:16   t13ex25
      *FPA00599; Buchungsart neu
      *
      *   Rev 2.2   25 Apr 2000 10:17:40   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.1   May 27 1999 10:02:26   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:16:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0943                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<EINHEITENMAXIMIERUNG>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILHELM                                          *
      *    DATUM:    27.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !27.05.99!2.1 ! NEU                       ! WILHELM  *
      *         !        !    !                           !          *
      * APA00084!14.03.00!2.2 ! COPY DRPA0942 DURCH       ! KRUFF    *
      *         !        !    ! DRPA0989 ERSETZT          !          *
      *         !        !    !                           !          *
      * FPA00599!25.06.03!2.3 ! Buchungsart neu           ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0943                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0943.
140300*   COPY DRPA0942.
140300    20 PI-DRPA0943-PAMS-DATEN.
140300       COPY DRPA0989.
          20 KNR-ZUSTAND-EH-MAX          PIC S9(4)       BINARY SYNC.
250603    20 KNR-BUCHUNGSART             PIC S9(4)       BINARY SYNC.
          20 PVER-AKT-EINH-ALT           PIC S9(6)V9(2)  USAGE COMP-3.
          20 MAX-IND                     PIC S9(4)       BINARY SYNC.
      *
       01 PO-DRPA0943.
140300*   COPY DRPA0942.
140300    20 PO-DRPA0943-PAMS-DATEN.
140300       COPY DRPA0989.
          20 KNR-ZUSTAND-EH-MAX          PIC S9(4)       BINARY SYNC.
          20 PVER-AKT-EINH-NEU           PIC S9(6)V9(2)  USAGE COMP-3.
          20 ZW-EINH                     PIC S9(6)V9(2)  USAGE COMP-3.
          20 PO-DRPA0943-RC              PIC  X(3).
             88 PO-DRPA0943-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0943-RC-NOK                       VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************
