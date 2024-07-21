      * T13EX39   Stand: 28/07/98    17:04:38
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0311.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0311.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:19:34   T13EX39
      *FUNKTION UPDATE FUER ZTKI0357
      *
      *   Rev 1.0   07 Jul 1998 11:50:54   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0311                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0311                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.08.98/12.59.21
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
      *    FÜR DIE FUNKTION DRKI0311
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0311.
           05  PI-DRKI0311-KEY.


             10 PI-DRKI0311-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0311-DATEN.


             10 PI-DRKI0311-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI0311-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI0311-VERFREISTAUFTRAG   PIC X(01).


      *
       01  PO-DRKI0311.
           05  PO-DRKI0311-RC                 PIC  X(003).
               88 PO-DRKI0311-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0311-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
