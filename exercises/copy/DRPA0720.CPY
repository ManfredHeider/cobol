      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0720.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0720.CPv  $
      *
      *   Rev 2.1   07 Sep 2001 16:29:32   t13ex21
      *FPA00262 - Neue Funktion
      *
      *   Rev 2.0   May 26 1999 15:15:38   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0720  FUNKTIONSAUFRUFE FÜR ZTPA0123      *
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
      * FPA00262!14.08.01!2.01! NEU: COMMIT-FUNKTION      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0123.
           05  PI-ZTPA0123-FUNKTION            PIC  X(008).
               88 PI-ZTPA0123-DRPA0721-INS          VALUE 'DRPA0721'.
               88 PI-ZTPA0123-DRPA0722-UPD          VALUE 'DRPA0722'.
               88 PI-ZTPA0123-DRPA0723-DEL          VALUE 'DRPA0723'.
               88 PI-ZTPA0123-DRPA0724-SEL-ALL      VALUE 'DRPA0724'.
140801         88 PI-ZTPA0123-DRAT0228-COM          VALUE 'DRAT0228'.
