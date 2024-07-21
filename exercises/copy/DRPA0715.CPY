      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0715.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0715.CPv  $
      *
      *   Rev 2.3   Oct 19 2005 12:00:56   t13ex44
      *APA00388: Erweiterung SA815 (Satzlänge/3-stellige Prod-Schlü
      *ssel)
      *
      *   Rev 2.2   Nov 26 2001 16:34:50   t13ex21
      *FPA00293 - Neue Funktion
      *
      *   Rev 2.1   Nov 05 1999 12:01:44   T13EX21
      *FPA00107 - Neue Funktion DRPA0746
      *
      *   Rev 2.0   May 26 1999 15:15:30   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0715  FUNKTIONSAUFRUFE FÜR ZTPA0122      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      *         !08.02.99!    ! NEUE FUNKTION DRPA1165    ! LÜTTGES  *
      *         !05.03.99!    ! NEUE FUNKTION DRPA1190    ! D.WILHELM*
      * FPA00107!05.11.99!    ! NEUE FUNKTION DRPA0746    ! STORCH   *
      * FPA00293!30.10.01!2.02! NEUE FUNKTION DRPA1191    ! STORCH   *
      * APA00388!04.10.05!2.03! NEUE FUNKTION DRPA1655    ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0122.
           05  PI-ZTPA0122-FUNKTION            PIC  X(008).
               88 PI-ZTPA0122-DRPA0716-INS          VALUE 'DRPA0716'.
               88 PI-ZTPA0122-DRPA0717-UPD          VALUE 'DRPA0717'.
               88 PI-ZTPA0122-DRPA0718-DEL          VALUE 'DRPA0718'.
               88 PI-ZTPA0122-DRPA0719-SEL-ALL      VALUE 'DRPA0719'.
100298         88 PI-ZTPA0122-DRAT0228-COM          VALUE 'DRAT0228'.
080299         88 PI-ZTPA0122-DRPA1165-UPD          VALUE 'DRPA1165'.
050399         88 PI-ZTPA0122-DRPA1190-UPD          VALUE 'DRPA1190'.
051199         88 PI-ZTPA0122-DRPA0746-INS2         VALUE 'DRPA0746'.
301001         88 PI-ZTPA0122-DRPA1191-UPD          VALUE 'DRPA1191'.
041005         88 PI-ZTPA0122-DRPA1655-SEL-F-I      VALUE 'DRPA1655'.
