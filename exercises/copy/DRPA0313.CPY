      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0313.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0313.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:44:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:34:44   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0313  Funktionen Standard-OZP            *
      *                           PMPG TBPA0101                      *
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
      * AFPA0012!06.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0101.
           05 PI-ZTPA0101-FUNKTION            PIC X(008).
              88 PI-ZTPA0101-DRPA0314-INS         VALUE 'DRPA0314'.
              88 PI-ZTPA0101-DRPA0315-UPD         VALUE 'DRPA0315'.
              88 PI-ZTPA0101-DRPA0316-DEL         VALUE 'DRPA0316'.
              88 PI-ZTPA0101-DRPA0317-SEL-ALL     VALUE 'DRPA0317'.
100298        88 PI-ZTPA0101-DRAT0228-COM         VALUE 'DRAT0228'.
