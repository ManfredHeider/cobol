      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0443.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0443.CPv  $
      *
      *   Rev 2.3   Jan 25 2012 11:43:14   t13ex14
      *FKI01362: Korrektur KFZKASKOART SST-Generierung zu ZTKI0322
      *
      *   Rev 2.2   Sep 15 1999 13:09:36   T13EX39
      *AKI00049 WKZ eingebaut
      *
      *   Rev 2.1   Mar 10 1999 11:55:42   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:23:52   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 09:54:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0443                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0443                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.01.2012 / 12:08:37 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI0443
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0443.
           05  PI-DRKI0443-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0443-KFKVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0443-KFKVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI0443-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI0443-KFKBEITRSATZFZINPR   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0443-KFKKASKOART   PIC  X(0002).
             10 PI-DRKI0443-KFKKASKOBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0443-KFKKASKOBEITRWKZ   PIC  X(0001).
             10 PI-DRKI0443-KFKREGIONALKLASSE   PIC  X(0003).
             10 PI-DRKI0443-KFKSBTEILKASKOWKZ   PIC  X(0001).
             10 PI-DRKI0443-KFKSBVOLLKASKOWKZ   PIC  X(0001).
             10 PI-DRKI0443-KFKSCHADFREIHEITID   PIC S9(4) COMP.
             10 PI-DRKI0443-N-KFKSCHDFRHTD     PIC S9(4) COMP.
             10 PI-DRKI0443-KFKSEBLSTBETEILTK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0443-KFKSELBSTBETEILKZ   PIC  X(0001).
             10 PI-DRKI0443-KFKSELBSTBETEILVK   PIC S9(9)V9(2) COMP-3.
END   *


      *
       01  PO-DRKI0443.
           05  PO-DRKI0443-RC                     PIC  X(003).
               88 PO-DRKI0443-RC-OK               VALUE 'OK '.
               88 PO-DRKI0443-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
