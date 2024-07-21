      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0318.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0318.CPv  $
      *
      *   Rev 2.1   Jan 19 2000 08:47:50   T13EX21
      *APA00063 - DTA-Sachbearbeiter
      *
      *   Rev 2.0   Feb 27 1999 14:28:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:47:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:35:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0318  Funktionen Standard-OZP            *
      *                           PMPE TBPA0102                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. OPRITZ                                       *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      * APA00012 20.07.98!    ! NEUE FUNKTION DRAT0276    ! GRÜNER   *
      * APA00063!26.10.99!    ! NEUE FUNKTION DRPA0737    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0102.
           05 PI-ZTPA0102-FUNKTION            PIC X(008).
              88 PI-ZTPA0102-DRPA0319-INS         VALUE 'DRPA0319'.
              88 PI-ZTPA0102-DRPA0320-UPD         VALUE 'DRPA0320'.
              88 PI-ZTPA0102-DRPA0321-DEL         VALUE 'DRPA0321'.
              88 PI-ZTPA0102-DRPA0322-SEL-ALL     VALUE 'DRPA0322'.
              88 PI-ZTPA0102-DRPA0695-MASS-UPD    VALUE 'DRPA0695'.
100298        88 PI-ZTPA0102-DRAT0228-COM         VALUE 'DRAT0228'.
200798        88 PI-ZTPA0102-DRAT0276-COM-DEL     VALUE 'DRAT0276'.
261099        88 PI-ZTPA0102-DRPA0737-UPD-VSTAT   VALUE 'DRPA0737'.
