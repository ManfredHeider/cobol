      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0308.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0308.CPv  $
      *
      *   Rev 2.1   Jan 19 2000 08:47:36   T13EX21
      *APA00063 - DTA-Sachbearbeiter
      *
      *   Rev 2.0   Feb 27 1999 14:28:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:41:40   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:22:46   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0308  Funktionen Standard-OZP            *
      *                           PMPG TBPA0100                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Opritz                                       *
      *    DATUM:    05.06.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !10.12.97!    ! FKT. DRPA0694             ! D.WILHELM*
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      * APA00063!26.10.99!    ! NEUE FUNKTION DRPA0736    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0100.
           05 PI-ZTPA0100-FUNKTION            PIC X(008).
              88 PI-ZTPA0100-DRPA0309-INS         VALUE 'DRPA0309'.
              88 PI-ZTPA0100-DRPA0310-UPD         VALUE 'DRPA0310'.
              88 PI-ZTPA0100-DRPA0311-DEL         VALUE 'DRPA0311'.
              88 PI-ZTPA0100-DRPA0312-SEL-ALL     VALUE 'DRPA0312'.
              88 PI-ZTPA0100-DRPA0694-MASS-UPD    VALUE 'DRPA0694'.
100298        88 PI-ZTPA0100-DRAT0228-COM         VALUE 'DRAT0228'.
261099        88 PI-ZTPA0100-DRPA0736-UPD-VSTAT   VALUE 'DRPA0736'.
