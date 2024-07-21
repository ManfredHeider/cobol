      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0540.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0540.CPv  $
      *
      *   Rev 2.2   Jan 19 2000 08:48:04   T13EX21
      *APA00063 - DTA-Sachbearbeiter
      *
      *   Rev 2.1   Jun 09 1999 13:30:46   T01JM2S
      *APA00012 - UMSETZUNG FACHL. ANFORDERUNG FKPAG026
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:29:58   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Sep 1997 17:00:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0540  FUNKTIONEN ZU UWPA0037              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    23.10.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.10.97!01.01! NEU                      ! SCHRIEBER*
      *         !        !     !                          !          *
      * APA00012!07.06.99!2.1  ! NEUE FUNKTION DRPA1257   ! WOLFF    *
      *         !        !     !                          !          *
      * APA00063!26.10.99!2.2  ! NEUE FUNKTION DRPA0735   ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE FUNKTIONEN ZU UWPA0037            *
      *                                                              *
      ****************************************************************
      *
      *
       01  PI-UWPA0037.
           05  PI-UWPA0037-FUNKTION            PIC  X(008).
               88 PI-UWPA0037-DRPA0541              VALUE 'DRPA0541'.
070699         88 PI-UWPA0037-DRPA1257              VALUE 'DRPA1257'.
261099         88 PI-UWPA0037-DRPA0735              VALUE 'DRPA0735'.
      *
      ******* ENDE COPY-MEMBER **************************************
