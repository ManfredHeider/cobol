      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1550.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1550.CPv  $
      *
      *   Rev 2.1   Jul 10 2003 13:33:08   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.0   Feb 27 1999 12:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 Feb 1999 10:22:54   I13PC48
      *SELECT COUNT AUF TAB. 348
      *
      *   Rev 1.0   14 Jul 1998 17:04:50   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1550                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0386                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HORNUNG/BÖHM                                     *
      *    DATUM:    29-May-1998                                      *
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
      *    FÜR DIE FUNKTION DRKI0386
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *    MANUELL BEARBEITET !!!!!!!!!!!!
      ****************************************************************
      *
       01  PI-DRKI1550.
           05  PI-DRKI1550-KEY.
               10   PI-DRKI1550-GHPVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1550.
           05  PO-DRKI1550-RC                 PIC  X(003).
               88 PO-DRKI1550-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1550-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1550-DATEN.
               10   PO-DRKI1550-COUNTER       PIC S9(4) COMP.



      *
      ******* ENDE COPY-MEMBER **************************************
