      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0777.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0777.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:08:36   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:46:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0777                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  PE-STRUKTUR EINFüGEN                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    09.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.01.98! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * apa00012!13.08.98! 1.1! neu: Strukturerfassungs   ! Parent   *
      *         !        !    ! und -änderungsdatum       !          *
      *         !        !    !                           !          *
      * apa00012!18.08.98! 02 ! Positionsnummer entfernt  ! Parent   *
      *         !        !    ! ( im Rahmen Denormalisier)!          *
      *         !        !    ! PEBD-Daten jetzt aus      !          *
      *         !        !    ! DRPA0433 (mit MAX-USED)   !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0777                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0777.
         03 PI-DRPA0777-KOPF.
            05 PI-DRPA0777-PROGRAMM              PIC  X(008).
            05 PI-DRPA0777-UFKT                  PIC  9(002).
         03 PI-DRPA0777-PEST-PEBD-DATEN.
      *
          COPY DRPA0433.
      *
       01 PO-DRPA0777.
          05 PO-DRPA0777-RC                         PIC  X(003).
             88 PO-DRPA0777-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0777-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
