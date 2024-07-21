      * I13PC18   Stand: 27/08/98    13:06:50
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0739.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0739.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:28:42   T13EX39
      *FUNKTION UPDATE ZTKI0357
      *
      *   Rev 1.0   28 Jul 1998 09:56:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0739                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0739                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  02.12.98/17.11.30
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0739
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0739.
           05  PI-DRKI0739-KEY.


             10 PI-DRKI0739-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0739-DATEN.


             10 PI-DRKI0739-VERVERTRAGSNUMMER   PIC X(15).


      *
       01  PO-DRKI0739.
           05  PO-DRKI0739-RC                 PIC  X(003).
               88 PO-DRKI0739-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0739-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
