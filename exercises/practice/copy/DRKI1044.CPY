      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1044.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1044.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 11:06:24   T13EX39
      *AKI00098 OZPs
      *
      *   Rev 2.0   Feb 09 2000 14:17:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1044                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1044                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  13.03.00/16.38.58
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
      *    FÜR DIE FUNKTION DRKI1044
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1044.
           05  PI-DRKI1044-KEY.


             10 PI-DRKI1044-PERSROLLENID   PIC X(13).
             10 PI-DRKI1044-PERSROLLENKZ   PIC X(01).

      *
       01  PO-DRKI1044.
           05  PO-DRKI1044-RC                 PIC  X(003).
               88 PO-DRKI1044-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1044-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1044-DATEN.


             10 PO-DRKI1044-LETZTEAENDERUNG   PIC X(26).


      *
      ******* ENDE COPY-MEMBER **************************************
