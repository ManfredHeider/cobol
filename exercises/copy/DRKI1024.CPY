      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1024.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 11:06:00   T13EX39
      *AKI00098 OZPs
      *
      *   Rev 2.0   Feb 09 2000 14:16:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1024                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1024                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  13.03.00/12.51.29
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00098!13.03.00!1.0 ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1024
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1024.
           05  PI-DRKI1024-KEY.


             10 PI-DRKI1024-VEREINLFDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1024-VEREINPERSONENID   PIC X(13).

      *
       01  PO-DRKI1024.
           05  PO-DRKI1024-RC                 PIC  X(003).
               88 PO-DRKI1024-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1024-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1024-DATEN.


             10 PO-DRKI1024-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI1024-VEREINFUNKTION   PIC X(30).
             10 PO-DRKI1024-VEREINTEXT   PIC X(30).


      *
      ******* ENDE COPY-MEMBER **************************************
