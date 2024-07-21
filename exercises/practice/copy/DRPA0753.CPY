      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0753.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0753.CPv  $
      *
      *   Rev 2.1   07 Sep 2001 16:29:02   t13ex21
      *FPA00262 - Neue Funktion
      *
      *   Rev 2.0   May 26 1999 15:25:42   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0753  FUNKTIONEN STANDARD-OZP            *
      *                           PVOR TBPA0108                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. OPRITZ                                       *
      *    DATUM:    23.12.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.12.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      * FPA00262!14.08.01!2.01! NEU: COMMIT-FUNKTION      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0108.
           05 PI-ZTPA0108-FUNKTION            PIC X(008).
              88 PI-ZTPA0108-DRPA0754-INS         VALUE 'DRPA0754'.
              88 PI-ZTPA0108-DRPA0755-UPD         VALUE 'DRPA0755'.
              88 PI-ZTPA0108-DRPA0756-DEL         VALUE 'DRPA0756'.
              88 PI-ZTPA0108-DRPA0757-SEL-ALL     VALUE 'DRPA0757'.
140801        88 PI-ZTPA0108-DRAT0228-COM         VALUE 'DRAT0228'.
