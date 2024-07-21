      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0673.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0673.CPv  $
      *
      *   Rev 2.1   Jul 05 1999 10:37:30   T13EX21
      *APA00012 - Neue Funktion
      *
      *   Rev 2.0   Mar 10 1999 12:11:54   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:59:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0673  FUNKTIONEN STANDARD-OZP            *
      *                           PEST TBPA0119                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    05.12.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!05.12.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      *         !02.07.99!    ! NEUE FUNKTION DRPA1280    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0119.
           05 PI-ZTPA0119-FUNKTION            PIC X(008).
              88 PI-ZTPA0119-DRPA0664-INS         VALUE 'DRPA0664'.
              88 PI-ZTPA0119-DRPA0665-UPD         VALUE 'DRPA0665'.
              88 PI-ZTPA0119-DRPA0666-DEL         VALUE 'DRPA0666'.
              88 PI-ZTPA0119-DRPA0667-SEL-ALL     VALUE 'DRPA0667'.
100298        88 PI-ZTPA0119-DRAT0228-COM         VALUE 'DRAT0228'.
020799        88 PI-ZTPA0119-DRPA1280-UPD2        VALUE 'DRPA1280'.
