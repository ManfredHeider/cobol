      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0421.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0421.CPv  $
      *
      *   Rev 2.3   04 Oct 2006 15:07:00   t13ex39
      *AKI00615: Einführung Pflegerentenversicherung
      *
      *   Rev 2.2   26 Sep 2006 12:44:00   t13ex39
      *AKI00619: neuer Unfall Plus Tarif "50+"
      *
      *   Rev 1.0   28 Jul 1998 09:52:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0421                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0421                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  26.09.2006/12:29:49
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
      *    FÜR DIE FUNKTION DRKI0421
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0421.
           05  PI-DRKI0421-KEY.


             10 PI-DRKI0421-LRWVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0421-LRWVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI0421.
           05  PO-DRKI0421-RC                 PIC  X(003).
               88 PO-DRKI0421-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0421-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
