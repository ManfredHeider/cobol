      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0323.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0323.CPv  $
      *
      *   Rev 2.2   Sep 17 2019 07:33:20   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen 
      *
      *   Rev 2.1   Aug 27 2019 14:27:00   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen
      *
      *   Rev 2.0   Feb 27 1999 14:28:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:50:36   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:36:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0323  FUNKTIONEN STANDARD-OZP            *
      *                           PMPP TBPA0103                      *
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
      * AFPA0012!10.12.97! 01 ! MASSENDELETE              ! SCHRIEBER*
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      *         !10.02.98!    ! NEUE FUNKTION DRAT0228    ! STORCH   *
      * APA02838!26.08.19! 02 ! NEUE FUNKTION DRPA1933    ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0103.
           05 PI-ZTPA0103-FUNKTION            PIC X(008).
              88 PI-ZTPA0103-DRPA0324-INS         VALUE 'DRPA0324'.
              88 PI-ZTPA0103-DRPA0325-UPD         VALUE 'DRPA0325'.
              88 PI-ZTPA0103-DRPA0326-DEL         VALUE 'DRPA0326'.
              88 PI-ZTPA0103-DRPA0327-SEL-ALL     VALUE 'DRPA0327'.
              88 PI-ZTPA0103-DRPA0701-MDEL        VALUE 'DRPA0701'.
100298        88 PI-ZTPA0103-DRAT0228-COM         VALUE 'DRAT0228'.
260819        88 PI-ZTPA0103-DRPA1933-UPD-PGV     VALUE 'DRPA1933'.


