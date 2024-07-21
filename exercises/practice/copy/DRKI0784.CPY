      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0784.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0784.CPv  $
      *
      *   Rev 2.2   Sep 15 1999 13:09:46   T13EX39
      *AKI00049 WKZ eingebaut
      *
      *   Rev 2.1   Mar 10 1999 12:06:54   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:26:00   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   06 Oct 1998 15:39:58   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0784                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0784                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.09.99/08.16.46
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
      *    FÜR DIE FUNKTION DRKI0784
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0784.
           05  PI-DRKI0784-KEY.


             10 PI-DRKI0784-KFMVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0784-KFMVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0784.
           05  PO-DRKI0784-RC                 PIC  X(003).
               88 PO-DRKI0784-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0784-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
