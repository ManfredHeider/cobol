      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0517.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0517.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:24:54   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Sep 1997 16:59:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0517                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: T. MUND                                         *
      *    DATUM:    25.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA....!25.09.97! 01 ! Neuerstellung             ! T. MUND  *
      *         !        !    !                           !          *
      * APA00012!20.07.98! 02 ! Neue Funktion Delete      ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE FUNKTIONEN FÜR DAS INSERT-LIST-   *
      *    OZP ZIPA0102                                              *
      *                                                              *
      *    >>> INSERT <<<                                            *
      *                                                              *
      ****************************************************************
      *
       01  PI-ZIPA0102.
           05 PI-ZIPA0102-FUNKTION            PIC X(008).
              88 PI-ZIPA0102-DRPA0518-INSERT      VALUE 'DRPA0518'.
200798        88 PI-ZIPA0102-DRPA1017-DELETE      VALUE 'DRPA1017'.
      ***** Ende-Copy-Member *****************************************
