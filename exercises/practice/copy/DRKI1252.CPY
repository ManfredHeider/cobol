      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1252.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1252.CPv  $
      *
      *   Rev 2.2   Jan 25 2012 11:43:00   t13ex14
      *FKI01362: Korrektur KFZKASKOART SST-Generierung zu ZTKI0322
      *
      *   Rev 2.1   May 02 2006 10:49:12   t13ex39
      *AKI00215: Umstellung aud EURO  KFZKASKO
      *
      *   Rev 2.0   05 Jul 2001 17:06:50   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1252                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1252                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.01.2012 / 12:08:53 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI1252
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1252.
           05  PI-DRKI1252-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1252-KFKVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1252-KFKVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI1252-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI1252-KFKBEITRSATZFZINPR   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1252-N-KFKBTRSTZFZNPR   PIC S9(4) COMP.
             10 PI-DRKI1252-KFKKASKOART   PIC  X(0002).
             10 PI-DRKI1252-KFKKASKOBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1252-NI-KFKKASKOBEITRAG PIC S9(4) COMP.
             10 PI-DRKI1252-KFKKASKOBEITRWKZ   PIC  X(0001).
             10 PI-DRKI1252-KFKREGIONALKLASSE   PIC  X(0003).
             10 PI-DRKI1252-KFKSBTEILKASKOWKZ   PIC  X(0001).
             10 PI-DRKI1252-KFKSBVOLLKASKOWKZ   PIC  X(0001).
             10 PI-DRKI1252-KFKSCHADFREIHEITID   PIC S9(4) COMP.
             10 PI-DRKI1252-N-KFKSCHDFRHTD     PIC S9(4) COMP.
             10 PI-DRKI1252-KFKSEBLSTBETEILTK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1252-N-KFKSBLSTBTLTK    PIC S9(4) COMP.
             10 PI-DRKI1252-KFKSELBSTBETEILKZ   PIC  X(0001).
             10 PI-DRKI1252-KFKSELBSTBETEILVK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1252-N-KFKSLBSTBTLVK    PIC S9(4) COMP.
END   *


      *
       01  PO-DRKI1252.
           05  PO-DRKI1252-RC                     PIC  X(003).
               88 PO-DRKI1252-RC-OK               VALUE 'OK '.
               88 PO-DRKI1252-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
