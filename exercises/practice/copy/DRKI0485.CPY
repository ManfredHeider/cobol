      * T13EX47   Stand: 17/11/98    16:23:24
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0485.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0485.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:53:40   T13EX39
      *FUNKTION DELETE FUER ZTKI0359
      *
      *   Rev 1.0   28 Jul 1998 09:57:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0485                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0485                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  12.08.98/18.53.53
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001! 17.8.98!1.0 ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0485
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0485.
           05  PI-DRKI0485-KEY.


             10 PI-DRKI0485-VRBPERSONENID   PIC X(13).
             10 PI-DRKI0485-VRBPERSONENROLLEKZ   PIC X(01).
             10 PI-DRKI0485-VRBVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0485.
           05  PO-DRKI0485-RC                 PIC  X(003).
               88 PO-DRKI0485-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0485-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
