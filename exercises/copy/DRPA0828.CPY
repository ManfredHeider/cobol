      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0828.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0828.cpv  $
      *
      *   Rev 2.1   May 14 1999 09:01:52   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1229
      *
      *   Rev 2.0   Feb 27 1999 14:29:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:58:00   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:44:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0828                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0089                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    20.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!20.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!10.05.99!2.1 ! NEUE FUNKTION DRPA1229    ! WOLFF    *
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
       01  PI-UPPA0089.
           05  PI-UPPA0089-FUNKTION            PIC  X(008).
               88 PI-UPPA0089-DRPA0848              VALUE 'DRPA0848'.
               88 PI-UPPA0089-DRAT0228              VALUE 'DRAT0228'.
               88 PI-UPPA0089-DRPA0829              VALUE 'DRPA0829'.
               88 PI-UPPA0089-DRPA0830              VALUE 'DRPA0830'.
               88 PI-UPPA0089-DRPA0831              VALUE 'DRPA0831'.
               88 PI-UPPA0089-DRPA0849              VALUE 'DRPA0849'.
               88 PI-UPPA0089-DRPA0878              VALUE 'DRPA0878'.
               88 PI-UPPA0089-DRPA1229              VALUE 'DRPA1229'.
      *
      ******* ENDE COPY-MEMBER **************************************
