      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1384.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1384.CPv  $
      *
      *   Rev 2.2   Mar 03 2004 14:09:32   T13EX39
      *AKI00403.UE002 DB-Änderung
      *
      *   Rev 2.1   Jul 30 2002 13:39:34   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Apr 09 2002 08:32:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1384                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1384                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.04.2002/12:30:36
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1384
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1384.
           05  PI-DRKI1384-KEY.


             10 PI-DRKI1384-EVP-ID   PIC X(12).
             10 PI-DRKI1384-EVP-ORDER-NUMBER   PIC S9(9) COMP.


           05  PI-DRKI1384-DATEN.


             10 PI-DRKI1384-EVP-CC-ID   PIC S9(9) COMP.
             10 PI-DRKI1384-EVP-STRING-DATA     PIC X(13).

             10 PI-DRKI1384-EVP-SUBTYPE   PIC S9(4) COMP.

      *
       01  PO-DRKI1384.
           05  PO-DRKI1384-RC                 PIC  X(003).
               88 PO-DRKI1384-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1384-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
