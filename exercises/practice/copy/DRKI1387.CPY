      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1387.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1387.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 13:39:38   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Apr 09 2002 08:32:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1387                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1387                    *
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
      *    FšR DIE FUNKTION DRKI1387
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1387.
           05  PI-DRKI1387-KEY.


             10 PI-DRKI1387-EVP-ID   PIC X(12).
             10 PI-DRKI1387-EVP-ORDER-NUMBER   PIC S9(9) COMP.

      *
       01  PO-DRKI1387.
           05  PO-DRKI1387-RC                 PIC  X(003).
               88 PO-DRKI1387-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1387-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
