      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1135.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1135.CPv  $
      *
      *   Rev 2.1   31 Jan 2001 14:25:38   T13EX39
      *AKI00068 KBI
      *
      *   Rev 2.0   07 Jun 2000 17:08:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1135                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1135                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.10.00/11.45.45
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
      *    FÜR DIE FUNKTION DRKI1135
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1135.
           05  PI-DRKI1135-KEY.


             10 PI-DRKI1135-GEFGEFAHRCODE   PIC S9(4) COMP.

      *
       01  PO-DRKI1135.
           05  PO-DRKI1135-RC                 PIC  X(003).
               88 PO-DRKI1135-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1135-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1135-DATEN.


             10 PO-DRKI1135-GEFGEFAHRKTEXT   PIC X(3).
             10 PO-DRKI1135-GEFGEFAHRTEXT   PIC X(30).


      *
      ******* ENDE COPY-MEMBER **************************************
