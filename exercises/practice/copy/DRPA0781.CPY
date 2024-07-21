      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0781.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0781.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:15:50   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:47:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0781                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTPA0111                        *
      *    Standard-OZP                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    13.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.01.98!01.01! NEU                      !  STORCH  *
      *         !10.02.98!     ! NEUE FUNKTION DRAT0228   !  STORCH  *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZTPA0111.
           05  PI-ZTPA0111-FUNKTION            PIC  X(008).
               88 PI-ZTPA0111-DRPA0792-INS          VALUE 'DRPA0792'.
               88 PI-ZTPA0111-DRPA0793-UPD          VALUE 'DRPA0793'.
               88 PI-ZTPA0111-DRPA0794-DEL          VALUE 'DRPA0794'.
               88 PI-ZTPA0111-DRPA0795-SEL-ALL      VALUE 'DRPA0795'.
100298         88 PI-ZTPA0111-DRAT0228-COM          VALUE 'DRAT0228'.
