      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0835.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0835.CPv  $
      *
      *   Rev 2.2   May 17 1999 15:46:08   T01JM35
      *Test Check In
      *
      *   Rev 2.1   May 17 1999 15:30:34   T01JM2S
      *Test Check In
      *
      *   Rev 2.0   Feb 27 1999 14:29:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 10:01:12   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:45:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0835                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTPA0125                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    10.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!17.05.99!2.1 ! TEST CHECK IN             ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!17.05.99!2.2 ! TEST CHECK IN             !PROEMPELER*
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
       01  PI-ZTPA0125.
           05  PI-ZTPA0125-FUNKTION            PIC  X(008).
               88 PI-ZTPA0125-DRPA0836-INS          VALUE 'DRPA0836'.
               88 PI-ZTPA0125-DRPA0837-UPD          VALUE 'DRPA0837'.
               88 PI-ZTPA0125-DRPA0838-SEL-ALL      VALUE 'DRPA0838'.
      *
      ******* ENDE COPY-MEMBER **************************************
