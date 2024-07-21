      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0741.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0741.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 11:16:48   T13EX47
      *INSERT fuer ZTKI0067
      *
      *   Rev 1.0   06 Oct 1998 15:34:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0741                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0741                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.10.98/15.36.40
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
      *    FÜR DIE FUNKTION DRKI0741
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0741.
           05  PI-DRKI0741-KEY.


             10 PI-DRKI0741-KVBKIPERSONENID   PIC X(13).
             10 PI-DRKI0741-KVBKIPERSONENROLLE   PIC X(01).
             10 PI-DRKI0741-KVBVBPERSONENID   PIC X(13).
             10 PI-DRKI0741-KVBVBPERSONENROLLE   PIC X(01).


           05  PI-DRKI0741-DATEN.


             10 PI-DRKI0741-KVBVERMITTLERID   PIC X(13).
             10 PI-DRKI0741-KVBVERMITTLERROLLE   PIC X(01).

      *
       01  PO-DRKI0741.
           05  PO-DRKI0741-RC                 PIC  X(003).
               88 PO-DRKI0741-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0741-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
