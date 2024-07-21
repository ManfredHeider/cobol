      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0710.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0710.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:15:20   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0710  FUNKTIONSAUFRUFE FÜR ZTPA0121      *
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0121.
           05  PI-ZTPA0121-FUNKTION            PIC  X(008).
               88 PI-ZTPA0121-DRPA0711-INS          VALUE 'DRPA0711'.
               88 PI-ZTPA0121-DRPA0712-UPD          VALUE 'DRPA0712'.
               88 PI-ZTPA0121-DRPA0713-DEL          VALUE 'DRPA0713'.
               88 PI-ZTPA0121-DRPA0714-SEL-ALL      VALUE 'DRPA0714'.
100298         88 PI-ZTPA0121-DRAT0228-COM          VALUE 'DRAT0228'.
