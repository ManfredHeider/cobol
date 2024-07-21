      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0633.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0633.CPv  $
      *
      *   Rev 2.1   Jun 07 1999 12:18:56   T13EX21
      *APA00012 - Neue Funktion DRPA1256
      *
      *   Rev 2.0   May 27 1999 13:14:00   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:56:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0633  FUNKTIONEN STANDARD-OZP  ZTPA0105  *
      *                           PNAP TBPA0105                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. JäGER-SOKOLUK                                *
      *    DATUM:    20.11.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!08.12.97!01.01! NEUERSTELLUNG             ! JäGER   *
      *         !10.02.98!     ! NEUE FUNKTION DRAT0228    ! STORCH  *
      *         !04.06.99!     ! NEUE FUNKTION DRPA1256    ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0105.
           05 PI-ZTPA0105-FUNKTION            PIC X(008).
              88 PI-ZTPA0105-DRPA0634-INS         VALUE 'DRPA0634'.
              88 PI-ZTPA0105-DRPA0636-DEL         VALUE 'DRPA0636'.
              88 PI-ZTPA0105-DRPA0637-SEL-ALL     VALUE 'DRPA0637'.
100298        88 PI-ZTPA0105-DRAT0228-COM         VALUE 'DRAT0228'.
040699        88 PI-ZTPA0105-DRPA1256-SEL         VALUE 'DRPA1256'.
