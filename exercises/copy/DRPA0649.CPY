      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0649.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0649.CPv  $
      *
      *   Rev 2.1   Jul 05 1999 10:37:08   T13EX21
      *APA00012 - Neue Funktion 
      *
      *   Rev 2.0   May 27 1999 11:31:22   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:57:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0649                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZFPA0001                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    27.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.11.97! 01 ! NEU                       ! PRIGGEN  *
      *         !07.01.98!    ! NEUE FUNKTION             ! STORCH   *
      *         !        !    !                           !          *
      * APA00012!15.09.98!    ! NEUE FUNKTION COMMIT      ! PARENT   *
      *         !        !    !                           !          *
      * APA00012!02.07.99!    ! NEUE FUNKTION DRPA1279    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
       01  PI-ZFPA0001.
           05  PI-ZFPA0001-FUNKTION            PIC  X(008).
               88 PI-ZFPA0001-DRPA0650              VALUE 'DRPA0650'.
070198         88 PI-ZFPA0001-DRPA0777              VALUE 'DRPA0777'.
150998         88 PI-ZFPA0001-DRAT0228-COM          VALUE 'DRAT0228'.
020799         88 PI-ZFPA0001-DRPA1279              VALUE 'DRPA1279'.
      *
      ******* ENDE COPY-MEMBER **************************************
