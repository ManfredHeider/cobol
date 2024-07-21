      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0631.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/MF/CPY/DRPA0631.CPv  $
      *
      *   Rev 2.4   12 Sep 2001 14:15:50   t13ae46
      *APA00144: Neue Funktion DRPA1008
      *
      *   Rev 2.3   12 Jul 2000 12:03:50   T13EX21
      *APA00096 - Neue Funktionen
      *
      *   Rev 2.2   Apr 04 2000 08:46:24   T13EX21
      *APA00088 - Neue Funktion DRPA1350
      *
      *   Rev 2.1   Nov 25 1999 09:16:54   T13EX21
      *APA00063 - Neue Funktionen
      *
      *   Rev 2.0   Feb 27 1999 14:29:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:19:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:56:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0631                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0083                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    19.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!19.11.97! 01 ! NEU                       ! D.WILHELM*
      * APA00063!06.10.99! 02 ! NEUE FUNKTIONEN IM DTA-   ! STORCH   *
      *         !        !    ! SACHBEARBEITER            !          *
      * APA00088!15.03.00!2.2 ! NEUE FUNKTION DRPA1350    ! STORCH   *
      * APA00096!15.06.00!2.3 ! NEUE FUNKTION DRPA1394    ! STORCH   *
      * APA00144!28.02.01!2.4 ! NEUE FUNKTION DRPA1008    ! WANG     *
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
       01  PI-UPPA0083.
           05  PI-UPPA0083-FUNKTION            PIC  X(008).
               88 PI-UPPA0083-DRPA0632              VALUE 'DRPA0632'.
061099         88 PI-UPPA0083-DRPA0160              VALUE 'DRPA0160'.
061099         88 PI-UPPA0083-DRPA0161              VALUE 'DRPA0161'.
061099         88 PI-UPPA0083-DRPA0230              VALUE 'DRPA0230'.
061099         88 PI-UPPA0083-DRPA0231              VALUE 'DRPA0231'.
061099         88 PI-UPPA0083-DRPA0401              VALUE 'DRPA0401'.
061099         88 PI-UPPA0083-DRPA0599              VALUE 'DRPA0599'.
150300         88 PI-UPPA0083-DRPA1350              VALUE 'DRPA1350'.
150600         88 PI-UPPA0083-DRPA1394              VALUE 'DRPA1394'.
280801         88 PI-UPPA0083-DRPA1008              VALUE 'DRPA1008'.
      *
      ******* ENDE COPY-MEMBER **************************************
