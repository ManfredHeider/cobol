      * I13PC18   Stand: 31/08/98    08:53:35
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0486.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0486.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:55:06   T13EX39
      *FUNKTION SELECT FUER ZTKI0359
      *
      *   Rev 1.0   28 Jul 1998 09:57:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0486                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0486                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  31.08.98/08.47.23
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
      *    FÜR DIE FUNKTION DRKI0486
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0486.
           05  PI-DRKI0486-KEY.


             10 PI-DRKI0486-VRBPERSONENID   PIC X(13).
             10 PI-DRKI0486-VRBPERSONENROLLEKZ   PIC X(01).
             10 PI-DRKI0486-VRBVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0486.
           05  PO-DRKI0486-RC                 PIC  X(003).
               88 PO-DRKI0486-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0486-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0486-DATEN.


             10 PO-DRKI0486-VRBABLAUFDATUM   PIC X(10).
             10 PO-DRKI0486-VRBAVKENNZEICHEN   PIC X(01).
             10 PO-DRKI0486-VRBGUELTIGKEITSDAT   PIC X(10).
             10 PO-DRKI0486-VRBPABADRESSEID   PIC X(13).
             10 PO-DRKI0486-VRBPABPERSONENID   PIC X(13).


      *
      ******* ENDE COPY-MEMBER **************************************
