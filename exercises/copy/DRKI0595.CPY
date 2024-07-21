      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0595.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0595.CPv  $
      *
      *   Rev 2.2   Jan 08 2007 14:31:38   t13ex43
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Ermitteln Pr
      *odukt-ID
      *
      *   Rev 2.1   Mar 10 1999 12:06:34   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:25:46   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   07 Aug 1998 14:50:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: ZTKI0613                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0613                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HORNUNG BÖHM                                     *
      *    DATUM:    28.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
151106* AKI00556!15.11.06!2.2 ! DRKI1996 HINZU            ! KLEEMEIER*
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
       01  PI-ZWKI0355.
           05  PI-ZWKI0355-FUNKTION            PIC  X(008).
             88   PI-ZWKI0355-DRKI0394     VALUE 'DRKI0394'.
151106       88   PI-ZWKI0355-DRKI1996     VALUE 'DRKI1996'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
