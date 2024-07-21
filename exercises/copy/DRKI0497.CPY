      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0497.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0497.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 Feb 1999 10:22:22   I13PC48
      *SELECT AUF TAB. 353
      *
      *   Rev 1.0   28 Jul 1998 09:58:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0403                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0403                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  03.08.98/11.36.15
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
      *    FÜR DIE FUNKTION DRKI0497 / manuell erstellt!!!
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>> select max(lfd_nr) auf verssachtarif  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0497.
           05  PI-DRKI0497-KEY.
             10 PI-DRKI0497-SATVERTRAGSID   PIC X(13).

           05  PI-DRKI0497-DATEN.
             10 PI-DRKI0497-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0497-SATZWEIGSCHLUESSEL   PIC X(2).
             10 PI-DRKI0497-SATZWEIGTEXT   PIC X(3).

      *
       01  PO-DRKI0497.
           05  PO-DRKI0497-RC                 PIC  X(003).
               88 PO-DRKI0497-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0497-RC-NOT-FOUND                 VALUE 'NFD'.

           05  PO-DRKI0497-DATEN.
             10 PO-DRKI0497-SATVERSTARLFDNR   PIC S9(4) COMP.

      *
      ******* ENDE COPY-MEMBER **************************************
