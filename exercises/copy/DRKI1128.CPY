      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1128.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRKI1128.CPv  $
      *
      *   Rev 2.3   Jan 27 2003 12:04:10   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 1.0   28 Jul 1998 09:58:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1128                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1128                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  03.12.2002/12:59:02
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
      *    FÜR DIE FUNKTION DRKI1128
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1128.
           05  PI-DRKI1128-KEY.


             10 PI-DRKI1128-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1128-GHPVERTRAGSID   PIC X(13).


           05  PI-DRKI1128-DATEN.


             10 PI-DRKI1128-GHPANLAGEZIEL   PIC X(01).
             10 PI-DRKI1128-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI1128-NI-GHPRISIKOKLASSE   PIC S9(4) COMP.


      *
       01  PO-DRKI1128.
           05  PO-DRKI1128-RC                 PIC  X(003).
               88 PO-DRKI1128-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1128-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
